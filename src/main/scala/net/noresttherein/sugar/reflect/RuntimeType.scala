package net.noresttherein.sugar.reflect

import java.{lang => j}

import scala.annotation.{implicitNotFound, unspecialized}
import scala.collection.concurrent.TrieMap
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{Type, TypeTag, runtimeMirror, typeOf}
import scala.runtime.BoxedUnit

import net.noresttherein.sugar.extensions.{castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.noSuch_!
import net.noresttherein.sugar.reflect.RuntimeType.{ExactRuntimeType, OfAnyRef, OfBoolean, OfByte, OfChar, OfDouble, OfFloat, OfInt, OfLong, OfShort, OfUnit}
import net.noresttherein.sugar.reflect.Specialized.{Enforce, Fun1Arg, Fun1Vals, Primitives, SpecializedExact}
import net.noresttherein.sugar.reflect.Specialize.Specifically






/** A type class describing the representation of type `T` at some code point in the compiled bytecode.
  * An implicit instance is always available - constants for built-in value types (and some others),
  * derived from an implicit `TypeTag`, `ClassTag` or being summoned from a `@specialized` class/method -
  * in that order of precedence. It serves as an umbrella type combining information coming from the latter
  * three sources and reflects the most specific information about a type that can be obtained
  * at the point of summoning, ''but not necessarily the compiled type a variable of this type declared at that point''.
  * The latter is, in general, impossible to define, as there is no means of differentiating between a `ClassTag`
  * for a concrete (statically known) type, and an implicit argument inside a generic method.
  *
  * Instances can be also obtained explicitly from the companion object based either on a runtime
  * class/`ClassTag`/`TypeTag`, or existing specialization context, or predefined values for all Scala value types
  * as well as some special ones. The consequence is that [[net.noresttherein.sugar.reflect.RuntimeType.runType runType]]
  * of any `RuntimeType[X]` can be any of a wide selection of different classes, without real guarantees about
  * their relation to a `runType` of an instance summoned locally. In cases, where it is undesirable,
  * a [[net.noresttherein.sugar.reflect.Specialized Specialized]]`[X]` subtype may be requested instead,
  * whose `runType` can be only classes which are valid arguments to `@specialized` annotation.
  *
  * In general, a `RuntimeType[T]` may represent any type assignable from `T` ''by the runtime'': its supertype,
  * or a type to which it is automatically boxed/unboxed by the Scala runtime. For example, `RuntimeType[Int]`
  * may represent any possible runtime type of an expression evaluating to `scala.Int`: `int`, `java.lang.Integer`,
  * (erased) `java.lang.Object`, or even `java.lang.Number`. Those instances are not equal, and while in many situations
  * these cases could be handled by the same code, it is still possible to cause a `ClassCastException` -
  * especially when working with arrays. Thus, in order to achieve correctness, programs must be mindful of the context,
  * and combine this information with one coming from program invariants.
  *
  * This class cannot be used as an evidence of general type compatibility of type parameters of two instances,
  * for the same reason that a pear is not an apple, despite both being fruits. The situation is however further
  * complicated by the muddy relationship between static and dynamic types in Scala:
  *   - loss of type parameter information through erasure,
  *     as in the case of `classTag[List[Int]] == classTag[List[Long]]`;
  *   - the duality of Scala's inbuilt 'AnyVal's, represented in runtime by both java primitives and their wrappers,
  *     and thus two different `Class[_]` instances, which forces any API to either become a leaky abstraction,
  *     reflecting that duality, 'lie', committing to potentially costly conversions, or risk casting errors -
  *     especially when dealing with arrays;
  *   - even worse, value classes may be represented both directly by the backing type,
  *     and as a Java object of the class, but ''they are not automatically boxed/unboxed'',
  *     leading to runtime exceptions;
  *   - `RuntimeType[Unit]` can represent either `java.lang.Void.TYPE` when denoting a (`void`) return type -
  *     a synthetic, ''primitive'' class with no values and an illegal argument for most operations,
  *     `scala.runtime.BoxedUnit` - a normal class extending `java.lang.Object`, or even `java.lang.Object` itself;
  *   - `RuntimeType[Nothing].runType` equals synthetic `classOf[Nothing$]`, but an instance of `Nothing$`
  *     is not assignable to anything other than its class and `Object`;
  *   - the same goes for `RuntimeType[Null]`, which can potentially represent `Null$`, and while `Null <: T` for any
  *     `T <: AnyRef`, `Null$ <: T` only for `T =:= Null$` and `T =:= AnyRef`;
  *   - the information about upper or lower bounds on type arguments is lost,
  *     but may affect the actual byte code signature of the method;
  *   - an implementation of a method declared in a more generic super type may have more specialized erasure
  *     than the signature of the overridden method.
  *
  * This evidence is, however, useful when used in conjunction with generic classes and methods to reflect ''their''
  * runtime information about their type parameters. Its primary function is to provide the ability to distinguish
  * between specialized synthetic versions of a generic class or method marked as `@specialized T` (with optional
  * parameters to the annotation). Unlike `ClassTag`s and `TypeTag`s, an implicit value of this class is available
  * even in contexts where `T` is abstract, representing the type after erasure and specialization.
  *
  * In particular, it makes it possible for a generic class/trait/method to discover if it was instantiated/called
  * with one of the built-in value types as the argument, and to which primitive Java type it corresponds.
  * In a generic, erased context, `implicitly[RuntimeType[T]]` (or just `RuntimeType[T]`) denotes the representation
  * of `T` as `java.lang.Object`, possibly after lifting from a Java primitive type, and downcast/unboxed
  * at codepoints where more specific information about the type becomes available. If, on the other hand,
  * `T` can be proven to be an inbuilt value type, the implicit value will always be a constant
  * from the companion object specific to that type. This permits to pick a specific implementation of a generic
  * trait/class, optimized for the given value type, or to verify if two collections share the element type.
  *
  * The lack of reliance on class tags makes it additionally possible to integrate specialized collections
  * with the generic standard library which would be otherwise impossible due to established method signatures.
  * For example, `ValSet[Byte]()`, `ValSet[Int]()`, `ValSet[Double]()` may all yield different implementations,
  * while retaining the flexibility and uniformity of the generic interface of `IterableFactory`
  * (without a `ClassTag` parameter).
  *
  * While the focus is on value types and their use with erased and specialized generic classes, an instance can,
  * as stated, denote an representation as any reference type, generalizing the concept to provide a uniform way
  * for specifying the degree of available dynamic information about a given type. In particular, in contexts where `T`
  * is fully instantiated (i.e., the instance was for example obtained from `RuntimeType[String]`), its full
  * class information is available to represent lack of any type abstraction. Other use cases include:
  *   - a generic class being able to provide the the information known by the implementation of its type parameter
  *     together with the ''degree'' of that information, regardless of whether its dynamic type
  *     is truly generic and erased, specialized, or a subclass manually specialized for a specific type
  *     (as in `class StringSet extends Set[String]`),
  *   - a single, unspecialized factory method for an array or a specialized class, which will always create an instance
  *     of the most specific type, regardless if called from specialized code, or just in presence
  *     of an implicit `ClassTag`;
  *   - an ability to call `@specialized` code from non-specialized one, limiting the number of actually `@specialized`
  *     methods to those, where the information is truly important for efficiency - for example, where array elements
  *     are actually accessed - for example, the majority of collection methods accepting a function `f :A => Boolean`
  *     could be implemented in the generic form, extracting the application of `f` to a single specialized method,
  *     considerably reducing the class size when compared to specializing them all,
  *   - an ability to call ''manually specialized'' (unrelated) methods from generic ones
  *     (see [[net.noresttherein.sugar.reflect.Specialize Specialize]]).
  *
  * The situation is considerably simpler for [[net.noresttherein.sugar.reflect.Specialized Specialized]] subclass,
  * as it has only instances representing each Java primitive type and the erased context;
  * the former gives full information by providing equal lower and upper bounds on the type parameter
  * (if `spec :Specialized[T]`, and `spec.runType == classOf[Int]`, then `T =:= Int`),
  * while the latter essentially tells essentially nothing, and all possible instances are declared as constants
  * in its companion object. Still, the fact that the specialization context information for a type can be derived
  * from different sources, means that it is very easy to obtain different instance depends on how it was obtained.
  * Consider:
  * {{{
  *     def ofClass[T :ClassTag] = RuntimeType[T] //Full runtime class information
  *     def ofTypeClass[T :RuntimeType] = RuntimeType[T] //Whatever was passed implicitly
  *     def ofSpec[@specialized T] = RuntimeType[T] //Retrieve an instance based on method specialization
  *
  *     def erased[T] = (ofClass[T].classTag, ofTypeClass[T].classTag, ofSpec[T].classTag)
  *     def spec[@specialized T] = (ofClass[T].classTag, ofTypeClass[T].classTag, ofSpec[T].classTag)
  *     def typeClass[T :RuntimeType] = (ofClass[T].classTag, ofTypeClass[T].classTag, ofSpec[T].classTag)
  *     def both[@specialized T :RuntimeType] = (ofClass[T].classTag, ofTypeClass[T].classTag, ofSpec[T].classTag)
  *
  *     println(erased[Int])             //"(Object, Object, Object)"
  *     println(spec[Int])               //"(Object, Int, Int)"
  *     println(typeClass[Int])          //"(Object, Int, Object)"
  *     println(both[Int])               //"(Object, Int, Int)"
  *     println(both[Int](ofClass[Int])) //"(Int, Int, Int)"
  * }}}
  *
  * @tparam T any Scala type, usually itself a type parameter of a generic method/class;
  *           this is not the final erased/unboxed type.
  * @see [[net.noresttherein.sugar.reflect.Specialize]] for calling specialized code from non-specialized context.
  * @see [[net.noresttherein.sugar.reflect.Specialized]]
  * @see [[net.noresttherein.sugar.reflect.RuntimeType.ValueClass]]
  * @author Marcin Mościcki
  */
@implicitNotFound("Cannot determine the runtime type of type ${T}. " +
                  "This is most likely a result of introduction of a conflicting implicit value for RuntimeType[${T}].")
sealed trait RuntimeType[@specialized T] extends Serializable {

	/** Type to which generic methods for `T` are specialized, i.e. representation of `T` inside a method `m[T]`,
	  * after erasure and any specializations. It is always a type alias for either `java.lang.Object`
	  * or one of Java primitive types.
	  */
	type GenericType >: RunType

	/** Closest information about type `T` as represented in byte code. This is different from
	  * [[net.noresttherein.sugar.reflect.RuntimeType.GenericType GenericType]]
	  * in that it doesn't necessarily represent erasure, but can be also any reference type.
	  *
	  * While in general values of `RunType` do not always conform to the type `T` denoted by this instance,
	  * the semantics of Java generics and Scala specialization as well as their runtimes with regard to autoboxing
	  * mean that the cast `(_ :RunType).asInstanceOf[T]` is safe as long as this instance was statically obtained in
	  * the context of casting. This is because either `RunType` and `T` are the same specialized value type and
	  * the cast is removed at compilation, or `T` is erased.
	  */
	type RunType >: T

	/** Type to which values of `T` are boxed whenever a reference type is expected. For primitive types it is declared
	  * as their corresponding wrappers, for a reference type `T <: AnyRef` it is simply `T`. The tricky case are
	  * custom value classes which are promoted to `AnyRef` without nominally being its subtypes.
	  */
	type BoxType


	/** The default value for type `T` as defined by the java spec. For primitive types,
	  * it would be some version of zero/`false`, for reference type the `null` value.
	  * `Unit` is represented by its scala singleton value.
	  */
	def default :T

	/** Performs a cast from type `T` to its runtime representation in the context where this type class was  obtained.
	  * This cast is always safe as it amounts to either an identity upcast, or boxing a java primitive to its reference
	  * counterpart.
	  */
	def toRunType(x :T) :RunType = x.asInstanceOf[RunType]

	/** Performs a cast from an array with element type `T` to its erased/specialized form as defined by this instance.
	  * This cast is always safe as it amounts to either an identity upcast, or boxing a java primitive to its reference
	  * counterpart.
	  */
	def toRunTypeArray(x :Array[T]) :Array[RunType] = x.asInstanceOf[Array[RunType]]

	/** Performs a cast from type `T` to its runtime representation in the context where this type class was  obtained.
	 * This cast is always safe as it amounts to either an identity upcast, or boxing a java primitive to its reference
	 * counterpart.
	 */
	def toGenericType(x :T) :GenericType = x.asInstanceOf[GenericType]

	/** Performs a cast from an array with element type `T` to the erased/specialized form as defined by this instance.
	  * This cast is always safe as it amounts to either an identity upcast, or boxing a java primitive to its reference
	  * counterpart.
	  */
	def toGenericTypeArray(x :Array[T]) :Array[GenericType] = x.asInstanceOf[Array[GenericType]]

	/** Performs a cast from the runtime representation type to the static type `T` as appearing in the code.
	  *  This is ''not'' a no-op, but the cast is safe, as the argument is of a path-dependent type, meaning it must
	  *  have been obtained in the same context as this instance.
	  */
	def fromRunType(x :RunType) :T = x.asInstanceOf[T]

	/** Performs a cast of an array from the runtime representation type to the static element type `T`,
	  * as appearing in the code.
	  * This is ''not'' a no-op, but the cast is safe, as the argument is of a path-dependent type, meaning it must
	  * have been obtained in the same context as this instance.
	  */
	def fromRunTypeArray(xs :Array[RunType]) :Array[T] = xs.asInstanceOf[Array[T]]

	/** Performs a cast from the erased/specialized representation type to the static type `T` as appearing in the code.
	  * This is essentially how java generics (with erasure) and scala specialization work: the runtime either
	  * casts down a `java.lang.Object` to the type `T` in the point of use, rather than in this method, deferring
	  * any `ClassCastException`s to the moment where `T` is statically known. It is therefore safe with the same
	  * caveats as generic collections: the cast value must not had been cast before.
	  */
	def fromGenericType(x :GenericType) :T = x.asInstanceOf[T]

	/** Performs a cast of an array from the erased/specialized representation type to the static type `T`,
	  * as appearing in the code. This is essentially how java generics (with erasure) and scala specialization work:
	  * the runtime either casts down a `java.lang.Object` to the type `T` in the point of use, rather than
	  * in this method, deferring any `ClassCastException`s to the moment where `T` is statically known.
	  * It is therefore safe with the same caveats as generic collections: the cast value must not had been cast before.
	  */
	def fromGenericTypeArray(xs :Array[GenericType]) :Array[T] = xs.asInstanceOf[Array[T]]



	/** An empty array which element type is the class representing `T` as par this level of specialization.
	  * This may be `T` itself, after unboxing (if `T` is specialized) or erasure (otherwise).
	  *
	  * @return `Array[Any]` (`Object[]`) if `T &lt:: AnyRef` or is an erased and boxed scala `AnyVal`,
	  *         or a java array of the primitive type corresponding to `T &lt:: AnyVal`, if `T` is known
	  *         (either fully instantiated, or by being a specialized type parameter).
	  */
	def emptyArray :Array[RunType] = classTag.newArray(0)

	/** An empty array which element type is the class representing `T` as par this level of specialization.
	  * This is a variant of [[net.noresttherein.sugar.reflect.RuntimeType.emptyArray emptyArray]] casting the array
	  * to `Array[T]`. This cast is safe if this instance was obtained in the context of the casting, as in that case
	  * both `RunType` and `T` refer to the same type/class used to represent `T` in that context; this cast actually
	  * compiles to no-op in this method. In fact, it will be still safe as long as this instance represents
	  * a higher level of specialization than available in the context of the caller, meaning either the calling code
	  * is not specialized for `T` at all, or `T` is a statically known super class of the type represented by this
	  * type class.
	  */
	@inline final def castArray :Array[T] = emptyArray.asInstanceOf[Array[T]]

	/** An array storing `T` in its erased/specialized form.
	  * @return `Array[AnyRef]` or one of java's primitive arrays.
	  */
	def emptyGenericArray :Array[GenericType] = erasedClassTag.newArray(0)

	/** An array which can store any value of `T` after autoboxing.
	  * @return an array of some reference element type.
	  */
	def emptyBoxArray :Array[BoxType] = boxClassTag.newArray(0)

	/** Create an array of the given length which can be used to store values of type `T`.
	  * In a context where more type information is present (`T` is not fully erased), it may create an array
	  * for a specific, non-erased superclass of `T`.
	  */
	@inline final def newArray(size :Int) :Array[RunType] = Array.ofDim[RunType](size)(classTag)

	/** Create an array of the given length which can be used to store values of type `T`.
	  * This is a variant of [[net.noresttherein.sugar.reflect.RuntimeType.emptyArray emptyArray]] casting the array
	  * to `Array[T]`. This cast is safe if this instance was obtained in the context of the casting, as in that case
	  * both `RunType` and `T` refer to the same type/class used to represent `T` in that context; this cast actually
	  * compiles to no-op in this method. In fact, it will be will be still safe as long as this instance represents
	  * a higher level of specialization than available in the context of the caller, meaning either the calling code
	  * is not specialized for `T` at all, or `T` is a statically known super class of the type represented by this
	  * type class.
	  */
	@inline final def castArray(size :Int) :Array[T] = Array.ofDim[T](size)(classTag.asInstanceOf[ClassTag[T]])

	/** The most generic array which can store elements of `T` without boxing in the context of this instance.
	  * @param size requested array length
	  * @return either one of java primitive arrays or `Array[AnyRef]`, based on specialization context for `T`.
	  */
	@inline final def newGenericArray(size :Int) :Array[GenericType] = Array.ofDim[GenericType](size)(erasedClassTag)

	/** A reference array (i.e. a subclass of `java.lang.Object[]`) most appropriate for storing reference version
	  * of `T`. The element type will be either the boxed value type (like `java.lang.Integer` or `java.lang.Boolean`),
	  * a specific class for instances obtained based on class tags, or `java.lang.Object` in fully erased contexts.
	  * @param size requested array length
	  */
	@inline final def newBoxArray(size :Int) :Array[BoxType] = Array.ofDim[BoxType](size)(boxClassTag)


	/** Class representing most specific information about type `T` at the point of obtaining this value. */
	def runType :Class[RunType]

	/** Class representing dynamic type for parameter `T` in a generic context after erasure and specialization.
	  * The only possible values are synthetic Java classes for Java primitives
	  * (i.e. `Integer.TYPE`) and `Class[java.lang.Object]`.
	  */
	def genericType :Class[GenericType]

	/** For value types, the Java class boxing this value. For reference types, equal to `runType`. */
	def boxType :Class[BoxType]


	/** Shorthand for `runType.getName`. */
	def className :String = runType.getName

	/** Shorthand for `genericType.getName`. */
	def genericClassName :String = genericType.getName

	/** Shorthand for `boxType.getName`. */
	def boxClassName :String = boxType.getName

	/** Scala name of the runtime type of `T` as returned by the `ClassTag` corresponding to the `runType` class. */
	def scalaName :String = classTag.toString

	/** Scala name of the type specialized for `T`, as returned by the `ClassTag`
	  * corresponding to the `genericType` class.
	  */
	def genericScalaName :String = erasedClassTag.toString

	/** Scala name of the runtime type of the reference type for `T`, as returned by the `ClassTag`
	  * corresponding to the `boxType` class.
	  */
	def boxScalaName :String = boxClassTag.toString


	/** `ClassTag` representing the type used in the bytecode to represent values of `T`
	  * in place of obtaining this instance.
	  */
	implicit def classTag :ClassTag[RunType] = ClassTag(runType)

	/** `ClassTag` representing the type used in the bytecode to represent values of `T` in contexts where `T`
	  * is an unbound, but possibly specialized, type parameter of a generic type.
	  */
	def erasedClassTag :ClassTag[GenericType] = ClassTag(genericType)

	/** `ClassTag` representing the type to which `T` is boxed wherever a reference type is required
	  * (such as type parameters for generic, non-specialized types).
	  */
	def boxClassTag :ClassTag[BoxType] = ClassTag(boxType)



	/** Is this a specialization of `scala.Unit`, represented as java 'void' pseudo type? */
	@inline final def isUnit :Boolean = runType == classOf[Unit]

	/** Is this a specialization of inbuilt scala 'AnyVal' type (corresponding to a java primitive)? */
	@inline final def isValueType :Boolean = runType.isPrimitive

	/** Is `T` type  `AnyRef` itself (and not its subclass), i.e. is `T` assignable *from* `AnyRef`? */
	@inline final def isAnyRef :Boolean = runType eq RuntimeType.AnyRefClass

	/** Is `T` represented in this context by a reference type (subtype of `AnyRef`),
	  * either due to being a reference type itself, or through autoboxing?
	  */
	@inline final def isRef :Boolean = RuntimeType.AnyRefClass.isAssignableFrom(runType)

	/** Is all type information about `T` erased and its values are represented as instances of `java.lang.Object`? */
	def isErased :Boolean = true

	/** Does this instance represent a type scala `Function1`'s arguments are specialized for?
	  * This information is important as all traversals of a collection rely on calling a passed function
	  * for the elements, which will result in boxing. Wherever possible it might be more efficient to use
	  * the iterator instead, which is specialized for all element types collections are specialized for.
	  */
	@inline final def isFun1Arg :Boolean =
		(this eq OfInt) || (this eq OfLong) || (this eq OfFloat) || (this eq OfDouble)

	/** Does this instance represent a type scala `Function1`'s return types are specialized for? */
	@inline final def isFun1Res :Boolean =
		(this eq OfInt) || (this eq OfLong) || (this eq OfFloat) ||
			(this eq OfDouble) || (this eq OfBoolean) || (this eq OfUnit)

	//todo: analyze with a fresh mind if this is sound (overridden in Specialized).
	def asSubtype[S <: T] :RuntimeType[S] = this.asInstanceOf[RuntimeType[S]]


	protected[reflect] final def call[R[_]](callback: Specialize[R])(implicit force :Enforce[T]) :R[T] =
		callback.specialized(this)

	protected[reflect] def call[R[_]](callback :Specifically[R])(implicit force :Enforce[T]) :R[T]

	protected[reflect] final def call[P[_], R[_]](callback: Specialize.WithArg[P, R])(param :P[T])
	                                             (implicit force :Enforce[T]) :R[T] =
		callback.specialized(param)(this)

	protected[reflect] def call[P[_], R[_]](callback :Specifically.WithArg[P, R])
	                                       (param :P[T])(implicit force :Enforce[T]) :R[T]

	//A variant of the above @specialized for the type parameter.
	protected[reflect] def call[R[_]](callback :Specifically.Lift[R])(param :T) :R[T] //=
//		call(callback :Specifically.WithArg[Self, R])(param)

	/** Invokes the proper variant of `@specialized` method `callback.specialized`,
	  * even when called from non-specialized context. This works because this method is `@specialized`
	  * for the argument type of this type class, ergo the erased variant of this method delegates
	  * to the variant specific to `T`.
	  */
	protected[reflect] final def call[P1[_], P2[_], R[_]]
	                                 (callback :Specialize.With2Args[P1, P2, R])
	                                 (param1 :P1[T], param2 :P2[T])(implicit force :Enforce[T]) :R[T] =
		callback.specialized(param1, param2)(this)

	protected[reflect] def call[P1[_], P2[_], R[_]]
	                           (callback :Specifically.With2Args[P1, P2, R])
	                           (param1 :P1[T], param2 :P2[T])(implicit force :Enforce[T]) :R[T]

	protected[reflect] def call[R[_]](callback :Specifically.Lift2[R])(param1 :T, param2 :T) :R[T] //=
//		call[Self, Self, R](callback :Specifically.With2Args[Self, Self, R])(param1, param2)



	/** An identifier which is different for every specializable type and the (erased) reference type. */
	@unspecialized protected[reflect] def discriminator :Enforce[T]


	/** Two instances are guaranteed to be fully compatible if they both represent specializations of the same
	  * scala `AnyVal` type represented by the same underlying java primitive.
	  * As a result, if `RuntimeType[A] sameAs RuntimeType[B]`,
	  * than it is safe to cast values of `A` and `B` in both directions. Additionally, in a context where both
	  * `A` and `B` are directly specialized, such a cast shouldn't cause any autoboxing as both values should already
	  * be represented by the same java primitive. Note that no two instances representing usages of reference types
	  * are the same in this meaning, even a `RuntimeType[AnyRef]` with itself!
	  * This relation is strictly stronger than equality on `RuntimeType[_]`.
	  * @see [[net.noresttherein.sugar.reflect.RuntimeType.equals]]
	  */
	@inline final def sameAs(other :RuntimeType[_]) :Boolean = runType == other.runType

	/** Defines classes of abstraction of types which, at code points associated with the compared instances,
	  * are represented as the same Java type in byte code.
	  * This is a weaker relation than equality of corresponding type arguments T. For one, erasure abstracts over
	  * type parameters, so [[net.noresttherein.sugar.reflect.Specialized Specialized]]`[X] =:= Specialized[Y]`
	  * for any `X, Y <: AnyRef`, even if types are unrelated. On the other hand, also
	  * `Specialized[String] =:= RuntimeType.`[[net.noresttherein.sugar.reflect.RuntimeType.of of]]`[AnyRef]`,
	  * despite the latter unequivocally representing `AnyRef` itself. In general, a `RuntimeType[T]` may represent
	  * any supertype of `T`, in particular `Any` / `AnyRef`, which can possibly result in equating two unrelated types.
	  * On the other
	  * hand, representations of a value type and a reference type will never be equal, even if the latter is
	  * the runtime box of the former. Likewise, custom value class and its backing value type will also compare as
	  * different, even though scala/java runtime would afford cross-compatibility.
	  * It is primarily useful in the context of specialization, as `runType` being a primitive token class guarantees
	  * @return `this.runType == that.runType`.
	  * @see [[net.noresttherein.sugar.reflect.RuntimeType.sameAs]]
	  */
	@inline final def =:=(other :RuntimeType[_]) :Boolean = runType eq other.runType

	/** True if all values represented as this runtime type are directly assignable to variables defined
	  * as the runtime type `other`. This is weaker than the `<:<` relation on scala `Type` instances,
	  * and in general says nothing about subtype relation between the type parameters of compared instances.
	  * It occupies itself only with the runtime class/type at a given point; not only any type parameters
	  * of type `T` itself are always erased, but `other` can potentially represent complete type erasure
	  * to `java.lang.Object`. In fact, it is quite possible that `(x :RuntimeType[X]) <:< (y :RuntimeType[Y])`
	  * and `Y <:< X` for different types `X` and `Y`: it suffices that `Y` is stored as a strict super type of `X`
	  * (as in the case of complete type erasure) and downcast by the ''VM'' when non-abstract reference
	  * to a collection element is encountered. Therefore, this check is a very poor substitute for type safety
	  * and is useful primarily in conjunction with additional constraints: either static type bounds, compared types
	  * being inbuilt value types, class-specific invariants.
	  * @return `other.runType isAssignableFrom this.runType`.
	  */
	def <:<(other :RuntimeType[_]) :Boolean = other.runType.isAssignableFrom(runType)

	/** Returns `other <:< this`. */
	@inline final def >:>(other :RuntimeType[_]) :Boolean = other <:< this

	/** Equates specialization of primitive types with their representations as auto boxed objects.
	  * For reference type this corresponds simply to equality on
	  * [[net.noresttherein.sugar.reflect.RuntimeType.runType runType]] and is always true .
	  * if `this `[[net.noresttherein.sugar.reflect.RuntimeType.=:= =:=]]` other`.
	  * @return `this.boxType == other.boxType`.
	  */
	def =%=(other :RuntimeType[_]) :Boolean = boxType eq other.boxType

	/** True if all values represented as this runtime type can be safely assigned to a variable represented by `other`,
	  * perhaps including boxing by scala run time, at the codepoint this instance is referring to.
	  * Note that it compares the compiled type representations at possibly arbitrary points; in particular,
	  * if `other` represents `Any`/`AnyRef`/erasure, the relation will hold regardless
	  * of this instance. It is useful mainly with additional invariants. For example, `RelayArray[T]` can be potentially
	  * backed by any array `Array[U] forSome { type U >: T }` ''as well as'' `Array[S] forSome { type S <: T }`:
	  * the former due to full or partial erasure and the latter as the effect of its covariance. This relation can
	  * answer the question if elements of another collection can be safely written to the array, while the element type
	  * of the union of the collections is determined statically as their ''LUB'' type. If, in addition,
	  * the [[net.noresttherein.sugar.reflect.RuntimeType.<:< <:<]] relation also holds,
	  * two backing arrays can be copied using `System.arraycopy`.
	  * @return `other.boxType isAssignableFrom this.boxType`.
	  */
	def <%<(other :RuntimeType[_]) :Boolean = other.boxType.isAssignableFrom(boxType)

	/** Returns `other <%< this`. */
	@inline final def >%>(other :RuntimeType[_]) :Boolean = other <%< this

	/** Two instances are equal if and only if they denote the same bytecode representation of the same type.
	  * In other words, for `a :RuntimeType[A], b :RuntimeType[B]`, `a == b` not only implies
	  * `this `[[net.noresttherein.sugar.reflect.RuntimeType.=:= =:=]]` that` (variables of their respective types
	  * declared at their respective code points have the same type in bytecode), but also that `typeOf[A] =:= typeOf[B]`.
	  * This can happen only if both `A` and `B` are builtin value types, or `a` and `b` are the same object.
	  * @return `(this eq that) || this.runType.isPrimitive && this.runType == that.runType`.
	  */
	final override def equals(that :Any) :Boolean = that match {
		case r :RuntimeType[_] => (this eq r) || runType.isPrimitive && (r.runType eq runType)
		case _ => false
	}
	final override def hashCode :Int = runType.hashCode

	override def toString :String = "[" + classTag + "]"
}




@SerialVersionUID(Ver)
object RuntimeType extends Rank1RuntimeTypes {

	/** Retrieves implicit information about runtime representation of type `T` at the point of calling.
	  * This is just a shortcut for `implicitly[RuntimeType[T]]`. Note that there should always be an implicit value
	  * for this parameter: if none is available in the local context, this factory is searched for a matching instance,
	  * picking a declared constant if `T` is a known specializable type, or defaulting to a lookup based
	  * on `@specialized` context. If `T` is abstract or not specializable, returned instance
	  * will equal `RuntimeType.erased[T]`.
	  * @tparam T possibly specialized type parameter of a generic method/class
	  * @return implicit value for `RuntimeType[T]` (which should always be available),
	  *         falling back to a lookup verifying real time type of `T` in the place of invocation.
	  */
	@inline def apply[T](implicit specialization :RuntimeType[T]) :RuntimeType[T] = specialization


	/** Return specialization type class instance which uses the given class as the runtime class.
	  * This represents the case where no static information is lost except for potential type parameters of `T`,
	  * if it is a generic type itself. For classes representing java primitives (including `Unit/void`)
	  * the corresponding specialization constant is returned. For reference types
	  * a [[net.noresttherein.sugar.reflect.RuntimeType.RefRuntimeType RefRuntimeType]] instance wrapping
	  * the given class is returned. Custom value classes are likewise represented by their lifted reference type.
	  * @return a `RuntimeType` instance which `runType` equals the given class.
	  */
	def ofClass[T](tpe :Class[T]) :RuntimeType[T] = (
		if (tpe.isPrimitive) tpe match {
			case j.Integer.TYPE   => OfInt
			case j.Double.TYPE    => OfDouble
			case j.Long.TYPE      => OfLong
			case j.Boolean.TYPE   => OfBoolean
			case j.Byte.TYPE      => OfByte
			case j.Character.TYPE => OfChar
			case j.Float.TYPE     => OfFloat
			case j.Short.TYPE     => OfShort
			case j.Void.TYPE      => OfUnit
			case _ => new RefRuntimeType[AnyRef](tpe.asInstanceOf[Class[AnyRef]]) //this is an impossible case ...
		} else if (tpe == classOf[AnyRef])
			OfAnyRef
		else
			new RefRuntimeType[AnyRef](tpe.asInstanceOf[Class[AnyRef]])
	).asInstanceOf[RuntimeType[T]]

	/** Return specialization type class instance specific to the given class, based on an implicit `ClassTag`.
	  * Equal to [[net.noresttherein.sugar.reflect.RuntimeType.ofClass ofClass]](classTag[T].runtimeClass).
	  * Note that, in context where `ClassTag[T]` is available implicitly, but `T` is an erased abstract type,
	  * returned instance will be based on that class tag and equal to the appropriate value class specialization
	  * for java primitives, despite values of `T` being auto boxed in that context.
	  *
	  * @tparam T type for which specialization should be resolved.
	  * @return an instance representing either one of java primitives or `java.lang.Object`.
	  */
	@inline def of[T](implicit tpe :ClassTag[T]) :RuntimeType[T] =
		ofClass(tpe.runtimeClass).asInstanceOf[RuntimeType[T]]

	/** The best representation of static type `T` based on implicit type information
	  * once erasure is performed for reference types.
	  * @return an instance representing either a java primitive (including `void`), synthetic `Null`
	  *         or erasure/boxing (for custom value types) to `AnyRef`.
	  */
	override def ofType[T](implicit tag :TypeTag[T]) :RuntimeType[T] = RuntimeTypes.ofType(tag.tpe)

	/** A shorthand for [[net.noresttherein.sugar.reflect.RuntimeType.ofClass ofClass]]`(array.getClass.getComponentType)` */
	@inline def ofElements[T](array :Array[T]) :RuntimeType[T] = ofClass(array.getClass.getComponentType.castParam[T])

	/** Most specific specialization for the given value. If `value` is a boxed java primitive, this will be the
	  * specialization for the appropriate value type. In all other cases, it will be an instance representing
	  * `value.getClass`.
	  */
	def ofValue[T](value :T) :RuntimeType[T] = //ofClass(UnboxedClass(value.getClass).asInstanceOf[Class[E]])
		(value match {
			case _ :j.Number => value match {
				case _ :j.Integer => OfInt
				case _ :j.Long    => OfLong
				case _ :j.Double  => OfDouble
				case _ :j.Byte    => OfByte
				case _ :j.Float   => OfFloat
				case _ :j.Short   => OfShort
				case _ => new RefRuntimeType[AnyRef](value.getClass.asInstanceOf[Class[AnyRef]])
			}
			case _ :j.Character => OfChar
			case _ :j.Boolean   => OfBoolean
			case _ :Unit        => OfUnit
			case _ if value.getClass eq classOf[AnyRef] => OfAnyRef
			case _ => new RefRuntimeType[AnyRef](value.getClass.asInstanceOf[Class[AnyRef]])
		}).asInstanceOf[RuntimeType[T]]


	/** Usage of type `T` as an unbound generic parameter in a fully specialized context.
	  * If `tpe` is the token class for one of the java primitives, the corresponding constant is used to represent
	  * the appropriate specialization. All reference types as well as custom value types are represented by an instance
	  * whose `runType` equals `AnyRef`; in that case, the actual information about the class of `T` is discarded.
	  * @return one of primitive specializations or an instance representing erasure to `AnyRef`.
	  */
	def genericClass[T](tpe :Class[T]) :Specialized[T] = (
		if (tpe.isPrimitive) tpe match {
			case j.Integer.TYPE   => OfInt
			case j.Long.TYPE      => OfLong
			case j.Double.TYPE    => OfDouble
			case j.Boolean.TYPE   => OfBoolean
			case j.Byte.TYPE      => OfByte
			case j.Character.TYPE => OfChar
			case j.Float.TYPE     => OfFloat
			case j.Short.TYPE     => OfShort
			case j.Void.TYPE      => OfUnit
			case _                => Erased
		} else
			Erased
	).asInstanceOf[Specialized[T]]

	/** Equals to [[net.noresttherein.sugar.reflect.RuntimeType.genericClass genericClass]]`(clazz)`
	  * for the runtime class as defined by an implicit `ClassTag`.
	  */
	@inline def generic[T](implicit tpe :ClassTag[T]) :Specialized[T] =
		genericClass(tpe.runtimeClass).asInstanceOf[Specialized[T]]

	/** Representation of any type as its auto boxed, erased form without any specialization or upper type bounds.
	  * @return a singleton instance, with all type members are defined as `AnyRef`,
	  *         and [[net.noresttherein.sugar.reflect.RuntimeType.runType runType]],
	  *         [[net.noresttherein.sugar.reflect.RuntimeType.genericType genericType]],
	  *         [[net.noresttherein.sugar.reflect.RuntimeType.boxType boxType]] all equal `classOf[AnyRef]`.
	  */
	@inline def erased[T] :Specialized[T] = Erased.asInstanceOf[Specialized[T]]

	/** Yields the representation of type `T` in the caller's context after erasure and specialization. */
	def specialized[@specialized T] :Specialized[T] = {
		new Enforce[T] match {
			case ErasedKey  => Erased
			case IntKey     => OfInt
			case LongKey    => OfLong
			case DoubleKey  => OfDouble
			case BooleanKey => OfBoolean
			case ByteKey    => OfByte
			case CharKey    => OfChar
			case FloatKey   => OfFloat
			case ShortKey   => OfShort
			case UnitKey    => OfUnit
			case _          => Erased
		}
	}.asInstanceOf[Specialized[T]]




	/** An empty array guaranteed to hold values of `T`, with most specific element type based on the information
	  * about `E` in the caller's context.
	  * @return an array which component type is some super class of `T`.
	  */
	@inline def arrayOf[T](implicit specialized :RuntimeType[T]) :Array[T] =
		specialized.emptyArray.asInstanceOf[Array[T]]

	/** A new array of the given size, guaranteed to hold values of `T`, with most specific element type based on the
	  * information about `E` in the caller's context. Note that this method can cause a breach in type safety,
	  * as the returned object is actually of type `Array[S forSome { type S >: T }]`. Thus it is possible,
	  * by additional casting, to store in the array an element which is not of class `E` without throwing an exception,
	  * likely resulting in `ClassCastException` at some later time when the element is accessed.
	  * @return an array which component type is some super class of `T` as defined by the implicit `RuntimeType`
	  *         type class.
	  */
	@inline def arrayOf[T](capacity :Int)(implicit specialized :RuntimeType[T]) :Array[T] =
		specialized.newArray(capacity).asInstanceOf[Array[T]]


	/** Creates an empty array guaranteed to be able to hold values of type `T`, as it would appear in erased
	  * and specialized byte code. For inbuilt, specialized (by the implicit parameter) value classes a corresponding
	  * java primitive array is returned. For `AnyRef` subtypes, the actual class of the created array
	  * will be `Object[]`. The downcast required to present it as `Array[T]` is erased, so any `ClassCastException`s
	  * will be delayed until the client code attempts to enforce its type to an actual concrete class.
	  * Note that it is still perfectly safe to call it if the array doesn't escape the context
	  * in which `T` is an erased type, or if `T` is a primitive.
	  * @param specialized specialization information about type `T`.
	  */
	@inline def genericArrayOf[T](implicit specialized :RuntimeType[T]) :Array[T] =
		specialized.emptyGenericArray.asInstanceOf[Array[T]]


	/** Creates an array of the given size, guaranteed to be able to hold values of type `T`, as it would appear
	  * in erased and specialized byte code. For inbuilt, specialized (by the implicit parameter) value classes
	  * a corresponding java primitive array is returned. For `AnyRef` subtypes, the actual class of the created array
	  * will be `[Object`. The downcast required to present it as `Array[T]` is erased, so any `ClassCastException`s
	  * will be delayed until the client code attempts to enforce its type to an actual concrete class.
	  * Note that it is still perfectly safe to call it if the array doesn't escape the context
	  * in which `E` is an erased type, or if `T` is a primitive.
	  * @param specialized specialization information about type `T`
	  */
	@inline def genericArrayOf[T](capacity :Int)(implicit specialized :RuntimeType[T]) :Array[T] =
		specialized.newGenericArray(capacity).asInstanceOf[Array[T]]




	private final val AnyRefClass = classOf[AnyRef]
	private final val NothingClass = classOf[Nothing]
	private final val NullClass = classOf[Null]



	/** Implicit casting from the runtime representation of type `T` with a `RuntimeType` type class to type `T` itself.
	  */
	@SerialVersionUID(Ver)
	object conversions {
		implicit def castFromRuntimeType[T](implicit tpe :RuntimeType[T]) :tpe.RunType => T =
			tpe.fromRunType

		implicit def castFromRuntimeTypeArray[T](implicit tpe :RuntimeType[T]) :Array[tpe.RunType] => Array[T] =
			tpe.fromRunTypeArray

		implicit def castFromGenericType[T](implicit tpe :RuntimeType[T]) :tpe.GenericType => T =
			tpe.fromGenericType

		implicit def castFromGenericTypeArray[T](implicit tpe :RuntimeType[T]) :Array[tpe.GenericType] => Array[T] =
			tpe.fromGenericTypeArray


		implicit def castToRuntimeType[R, T](implicit tpe :RuntimeType[T] { type RunType = R }) :T => R =
			tpe.toRunType

		implicit def castToRuntimeTypeArray[R, T](implicit tpe :RuntimeType[T] { type RunType = R })
				:Array[T] => Array[R] =
			tpe.toRunTypeArray

		implicit def castToGenericType[G, T](implicit tpe :RuntimeType[T] { type GenericType = G }) :T => G =
			tpe.toGenericType

		implicit def castToGenericTypeArray[G, T](implicit tpe :RuntimeType[T] { type GenericType = G })
				:Array[T] => Array[G] =
			tpe.toGenericTypeArray
	}



	/** Implicit specialization determined from an implicitly available `ClassTag[E]`. Same as `RuntimeType.of[E]`. */
	@inline implicit def runtimeClass[E](implicit tpe :ClassTag[E]) :RuntimeType[E] = of[E]

	/** Specialization for `Byte`. */
	implicit final val OfByte :SpecializedExact[Byte] = Specialized.ForByte

	/** Specialization for `Short`. */
	implicit final val OfShort :SpecializedExact[Short] = Specialized.ForShort

	/** Runtime type of `Int` as the java primitive. */
	implicit final val OfInt :SpecializedExact[Int] = Specialized.ForInt

	/** Runtime type of `Long` as the java primitive. */
	implicit final val OfLong :SpecializedExact[Long] = Specialized.ForLong

	/** Runtime type of `Char` as the java primitive. */
	implicit final val OfChar :SpecializedExact[Char] = Specialized.ForChar

	/** Runtime type of `Float` as the java primitive. */
	implicit final val OfFloat :SpecializedExact[Float] = Specialized.ForFloat

	/** Runtime type of `Double` as the java primitive. */
	implicit final val OfDouble :SpecializedExact[Double] = Specialized.ForDouble

	/** Runtime type of `Boolean` as the java primitive. */
	implicit final val OfBoolean :SpecializedExact[Boolean] = Specialized.ForBoolean

	/** Runtime type of `Unit` as java `void`. This is different from other specialized value types in that
	  * there is no unboxed value for `void` - the boxed constant `scala.runtime.BoxedUnit` is used instead.
	  * Similarly, created arrays also use the boxed type as the component type rather than `void`
	  * for the obvious reason. While this is an exception to other primitives, it is of no practical importance
	  * for Scala code as scala performs the exact same promotion when using the `Unit` type directly and runtime
	  * autoboxing guarantees compatibility.
	  */
	implicit final val OfUnit :SpecializedExact[Unit] = Specialized.ForUnit

	/** Specialization for `AnyRef` (and indirectly also `Any` by promotion). */
	implicit final val OfAnyRef :ExactRuntimeType[AnyRef] = new RefRuntimeType[AnyRef] with ExactRuntimeType[AnyRef] {
		override final val runType :Class[AnyRef] = AnyRefClass
		override final val emptyArray :Array[AnyRef] = new Array[AnyRef](0)

		protected[reflect] override final val discriminator = new Enforce[AnyRef]
		private def readResolve :AnyRef = RuntimeType.OfAnyRef
		override def toString = "[AnyRef]"
	}

	/** Explicit specialization for `Any` - this is equivalent to `OfAnyRef`. While both these values and
	  * [[net.noresttherein.sugar.reflect.RuntimeType.erased erased]]`[T]` describe erased context, the latter
	  * does not equal the former. This is because for this value (and `OfAnyRef`) the type parameter is well defined,
	  * while for the erased instance all information is lost.
	  */
	implicit final val OfAny :RuntimeType[Any] = OfAnyRef.asInstanceOf[RuntimeType[Any]]

	/** Runtime type of 'Nothing'. This class declares the `RunType` as `Nothing` and throws an exception
	  * from its [[net.noresttherein.sugar.reflect.RuntimeType.default default]] method. The box type - and hence the
	  * component type of the boxed array - is however defined as `Nothing$`, which is scala runtime representation
	  * of the type use in the bytecode whenever the type is required.
	  * This special value is mainly used by collection factory methods (and similar) to check if an instance must
	  * be empty, possibly returning a dedicated empty object. In particular this is useful in cases where the
	  * type parameter is required to be a subtype of `AnyVal` as a (weaker) declaration that a method is applicable
	  * only to Java primitive types. While the default `RuntimeType` for a custom value class would be considered
	  * an illegal argument for such a method, often the `Nothing` case can still be handled sensibly.
	  */
	implicit final val OfNothing :RuntimeType[Nothing] = new RuntimeType[Nothing] {
		override type RunType = Nothing
		override type BoxType = scala.runtime.Nothing$
		override type GenericType = Any

		override def default = noSuch_!("Specialized[Nothing].default")

		override val runType = classOf[Nothing]
		override val classTag = ClassTag(classOf[Nothing])
		override val emptyArray :Array[Nothing] = new Array[Nothing](0)
		override val boxType = classOf[scala.runtime.Nothing$]
		override val genericType = classOf[Any]
		protected[reflect] override val discriminator = Enforce.forceSpecialization

		protected[reflect] override def call[R[_]](callback :Specifically[R])(implicit force :Enforce[Nothing]) =
			callback.forNothing

		protected[reflect] override def call[P[_], R[_]](callback :Specifically.WithArg[P, R])
		                                                (param :P[Nothing])(implicit force :Enforce[Nothing]) =
			callback.forNothing(param)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift[R])(param :Nothing) :R[Nothing] =
			callback.forNothing(param)

		protected[reflect] override def call[P1[_], P2[_], R[_]](callback :Specifically.With2Args[P1, P2, R])
		                                                        (param1 :P1[Nothing], param2 :P2[Nothing])
		                                                        (implicit force :Enforce[Nothing]) :R[Nothing] =
			callback.forNothing(param1, param2)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift2[R])
		                                          (param1 :Nothing, param2 :Nothing) :R[Nothing] =
			callback.forNothing(param1, param2)

		private def readResolve :AnyRef = RuntimeType.OfNothing
		override def toString = "[Nothing]"
	}

	/** Represents an erased generic type argument referenced to as `java.lang.Object` and downcast in the point of use. */
	private[this] final val Erased :Specialized[Any] = Specialized.Erasure



	/** `Specialized` instances representing all possible method/class specialization (all primitives and erasure).
	  * `Specializations(Specialized[T])` is true for all concrete and abstract types `T`.
 	  */
	final val Specializations = Set[Specialized[_]](
		OfByte, OfShort, OfInt, OfLong, OfChar, OfFloat, OfDouble, OfBoolean, OfUnit, Erased
	)



	/** A trait extended by `RuntimeType` instances retaining full type information about `T`, i.e.
	 * representing values of type `T` as class `classOf[T]` (including inbuilt value classes) in the bytecode.
	 * Note that this trait is ''not'' specialized itself, so accepting it as a parameter will not result in
	 * the specialization of the method.
	 */
	sealed trait ExactRuntimeType[T] extends RuntimeType[T] {
		type RunType = T
	}

	/** Any representation of a reference type by a java/scala class specified by implicit `ClassTag[T]`.
	  * This is different from erasure in that `T` is not a value type and this instance may represent
	  * any super type of `T`. Values of the same type may be represented by many different instances of
	  * [[net.noresttherein.sugar.reflect.RuntimeType.RefRuntimeType RefRuntimeType]], representing different
	  * levels of generalisation after taking type bounds into the equation, from full type information to `AnyRef`.
	  * @param classTag the class representing the static type assignable from `T`.
	  * @tparam T any scala type, usually itself a type parameter of a generic method/class;
	  *           this is not the final erased/unboxed type.
	  */
	@SerialVersionUID(Ver)
	sealed class RefRuntimeType[T >: Null <: AnyRef] private[RuntimeType]
	                           ()(implicit final override val classTag :ClassTag[T])
		extends RuntimeType[T] with ExactRuntimeType[T]
	{
		private[RuntimeType] def this(runClass :Class[T]) =
			this()(new ClassTag[T] { override def runtimeClass: Class[_] = runClass })

		override type GenericType = AnyRef
		override type BoxType = T

		override def default :Null = null

		override def erasedClassTag :ClassTag[AnyRef] = implicitly[ClassTag[AnyRef]]
		override def boxClassTag    :ClassTag[T] = classTag
		override def runType        :Class[T] = classTag.runtimeClass.asInstanceOf[Class[T]]
		override def genericType    :Class[AnyRef] = AnyRefClass
		override def boxType        :Class[T] = runType

		override def emptyArray: Array[T] = Array.empty[T]
		override def emptyGenericArray: Array[AnyRef] = Array.empty[AnyRef]
		override def emptyBoxArray: Array[T] = Array.empty[T]

		protected[reflect] override def discriminator :Enforce[T] = ErasedKey.asInstanceOf[Enforce[T]]

		protected[reflect] override def call[R[_]](callback: Specifically[R])(implicit force: Enforce[T]) :R[T] =
			callback.forRef[T](this)

		protected[reflect] override def call[P[_], R[_]](callback :Specifically.WithArg[P, R])
		                                                (param :P[T])(implicit force :Enforce[T]) :R[T] =
			callback.forRef[T](param)(this)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift[R])(param :T) :R[T] =
			callback.forRef[T](param)(this)

		protected[reflect] override def call[P1[_], P2[_], R[_]](callback :Specifically.With2Args[P1, P2, R])
		                                                        (param1 :P1[T], param2 :P2[T])
		                                                        (implicit force :Enforce[T]) :R[T] =
			callback.forRef[T](param1, param2)(this)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift2[R])(param1 :T, param2 :T) :R[T] =
			callback.forRef[T](param1, param2)(this)
	}



	/** A representation of custom value type `T` wrapping a value of type `V` (possibly a standard scala value type).
	  * While in any generic context, specialized or not, value class instances are promoted to `AnyRef` and thus
	  * this type class is functionally equivalent to a `RuntimeType` instance representing full erasure,
	  * there are applications in which such information is desirable. In particular, it allows specialized collection
	  * implementations to store value classes as their wrapped primitives instead and box them only on access.
	  * By providing an implicit value of this type for your custom value class you can instruct supporting collections
	  * to store the values as their member field instead, as per its specialization information. Note that functions
	  * accepting or returning custom value classes wrapping a built-in value class will always instantiate their class,
	  * meaning collection methods would result in boxing of each element at every access: this is a trade off
	  * between speed and space.
	  */
	trait ValueClass[@specialized(Primitives) V, T] extends RuntimeType[T] {
		override type GenericType = Any
		override type BoxType = RunType

		/** The standard scala value type which consists the runtime representation of `T` in non-generic contexts. */
		type BaseType = V
		/** Specialization for the base primitive value of the custom value class `T`. */
		def baseType :Specialized[V]

		/** Convert the primitive representation of the value to the custom type `T`. */
		def apply(value :V) :T

		/** Convert the custom value type to the primitive value forming its runtime representation. */
		def unapply(box :T) :V

		override def default :T = apply(baseType.default)

		override def genericType    :Class[Any] = classOf[Any]
		override def erasedClassTag :ClassTag[Any] = implicitly[ClassTag[Any]]
		override def boxType        :Class[BoxType] = runType
		override def boxClassTag    :ClassTag[BoxType] = classTag

		override def emptyGenericArray :Array[Any] = ValueClass.EmptyAnyArray
		override def emptyBoxArray     :Array[BoxType] = emptyArray

		protected[reflect] override def call[R[_]](callback :Specifically[R])
		                                           (implicit force :Enforce[T]) :R[T] =
			callback.forOthers(this)

		protected[reflect] override def call[P[_], R[_]](callback :Specifically.WithArg[P, R])
		                                                (param :P[T])(implicit force :Enforce[T]) :R[T] =
			callback.forOthers(param)(this)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift[R])(param :T) :R[T] =
			callback.forOthers(param)(this)

		protected[reflect] override def call[P1[_], P2[_], R[_]](callback :Specifically.With2Args[P1, P2, R])
		                                                        (param1 :P1[T], param2 :P2[T])
		                                                        (implicit force :Enforce[T]) :R[T] =
			callback.forOthers(param1, param2)(this)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift2[R])(param1 :T, param2 :T) :R[T] =
			callback.forOthers[T](param1, param2)(this)

		protected[reflect] override def discriminator :Enforce[T] = Enforce.forceSpecialization
	}


	@SerialVersionUID(Ver)
	object ValueClass {
		/** Creates a `RuntimeType` instance for a value class `T` wrapping a standard value type `V`.
		  * Relies on implicit specialization information for `V` and the class tag for `T`.
		  * @param wrap the constructor function creating a value class instance boxing the given value
		  * @param unwrap the getter function accessing the field of the value class `T`.
		  * @tparam T a custom box class, typically, but not necessarily, a value class.
		  * @tparam V the type of the single member field of `T`.
		  */
		def apply[@specialized(Primitives) V :Specialized, T <: AnyVal :ClassTag]
		         (wrap :V => T)(unwrap :T=>V) :ValueClass[V, T] =
			new ValueClassType[V, T] {
				override def apply(value :V) :T = wrap(value)
				override def unapply(box :T) :V = unwrap(box)
			}

		/** Convenience base class for custom [[net.noresttherein.sugar.reflect.RuntimeType.ValueClass]] implementations
		  * relying on implicit runtime type information for the representation for the wrapped type `V` and class tag
		  * for the described value class `T`.
		  */
		abstract class ValueClassType[@specialized(Primitives) V, T]
		                             (implicit val baseType :Specialized[V], override val classTag :ClassTag[T])
			extends ValueClass[V, T]
		{
			override type RunType = T
			override def runType :Class[T] = classTag.runtimeClass.asInstanceOf[Class[T]]
		}

		/** Convenience base class for custom [[net.noresttherein.sugar.reflect.RuntimeType.ValueClass]] implementations.
		  * Unlike [[net.noresttherein.sugar.reflect.RuntimeType.ValueClass.ValueClassType]], which retains full
		  * information about the class, this instance does not rely on any implicit type information and represents
		  * the erasure of type `T`. `RunType` (and `BoxType`, `GenericType`) are declared as `Any` and the specialization
		  * type class instance for the backing value type is retrieved based on this class's specialization.
		  * This makes it slightly lighter and cheaper to construct.
		  */
		abstract class ErasedValueClass[@specialized(Primitives) V <: AnyVal, T]
			extends ValueClass[V, T]
		{
			override type RunType = Any
			override def runType :Class[Any] = classOf[Any]

			override def baseType :Specialized[V] = Specialized[V]
		}

		/** Implicit conversions performing the boxing and unboxing based on an implicit `ValueClass` instance. */
		@SerialVersionUID(Ver)
		object conversions {
			implicit def implicitBoxing[@specialized(Fun1Arg) V <: AnyVal, T]
			                           (implicit valueClass :ValueClass[V, T]) :V => T =
				valueClass.apply

			implicit def implicitUnboxing[@specialized(Fun1Vals) V <: AnyVal, T]
			                             (implicit valueClass :ValueClass[V, T]) :T => V =
				valueClass.unapply
		}

		private val EmptyAnyArray = new Array[Any](0)
	}


	/** We have the discriminators redeclare here to avoid (two!) virtual calls to accessor methods in RuntimeType
	  * during specialization discovery.
	  */
	private[this] final val IntKey     = OfInt.discriminator
	private[this] final val LongKey    = OfLong.discriminator
	private[this] final val ShortKey   = OfShort.discriminator
	private[this] final val ByteKey    = OfByte.discriminator
	private[this] final val DoubleKey  = OfDouble.discriminator
	private[this] final val FloatKey   = OfFloat.discriminator
	private[this] final val CharKey    = OfChar.discriminator
	private[this] final val BooleanKey = OfBoolean.discriminator
	private[this] final val UnitKey    = OfUnit.discriminator
	private[this] final val ErasedKey  = Erased.discriminator
}




/** Implicit `RuntimeType` value of third order in precedence, verifying specialization context of the caller.
  * As it will always yield a value, but possibly representing an erased one despite programmer's intentions, it's worse
  * than any sure source of type information, but still better than nothing.
  */
protected[reflect] sealed abstract class Rank2RuntimeTypes {
	/** Runtime type resolution in the context of the caller.
	  * Implemented by [[net.noresttherein.sugar.reflect.Specialized specialized]] in the subclass.
	  * Returned instance reflects recognized runtime type of `T` in the reference point. If the call happens
	  * from within code specialized for type argument `T` (or `T` is statically known to be a scala value type),
	  * returned instance will carry information about the corresponding primitive. If `T` is erased, or known to be
	  * a reference type, returned instance represents scala `AnyRef`.
	  */
	@inline final implicit def specializedRuntimeType[@specialized T] :RuntimeType[T] = specialized[T]

	/** Resolve local specialization information for type `T`. When called from code specialized for type `T`,
	  * either explicitly by the `@specialized` annotation, or one where `T` is fully instantiated,
	  * it will return an instance associated with the specialized type.
	  * Otherwise (including all `AnyRef` subtypes), a generic instance equal to `Specialized[Any]` is returned.
	  */
	def specialized[@specialized T] :Specialized[T]
}


/** Implicit values for `RuntimeType` type class of secondary precedence, with lower reliability or efficiency
  * than dedicated values declared in [[net.noresttherein.sugar.reflect.RuntimeType$]].
  */
protected[reflect] sealed abstract class Rank1RuntimeTypes extends Rank2RuntimeTypes {
	/** Retrieve specialization information about type `T` from an implicitly available `TypeTag`.
	  * Implemented in subclass by [[net.noresttherein.sugar.reflect.RuntimeType.ofType ofType]].
	  */
	@inline final implicit def runtimeType[T](implicit tpe :TypeTag[T]) :RuntimeType[T] = ofType[T]

	/** Retrieve specialization information for type `T` from an implicitly available `TypeTag`.
	  * 'TypeTag's are more annoying than `ClassTag`s, hence the latter has precedence, but we'll make do
	  * with what we have. Returned instance reflects the best runtime type for type `T`, but not necessarily
	  * the one applicable to current context. If type `T` is abstract and not specialized, but a `TypeTag[T]` instance
	  * identifies it as a java primitive type, an instance for that primitive will be returned despite the fact
	  * that all values of `E` in that context might be erased and boxed in runtime.
	  */
	def ofType[T :TypeTag] :RuntimeType[T]
}



private object RuntimeTypes {
	def specializedType[T](implicit tag :TypeTag[T]) :Specialized[T] = {
		val tpe = tag.tpe
		(
			if (tpe <:< AnyRefType)       Specialized.Erasure
			else if (tpe <:< IntType)     Specialized.ForInt
			else if (tpe <:< DoubleType)  Specialized.ForDouble
			else if (tpe <:< LongType)    Specialized.ForLong
			else if (tpe <:< ByteType)    Specialized.ForByte
			else if (tpe <:< CharType)    Specialized.ForChar
			else if (tpe <:< BooleanType) Specialized.ForBoolean
			else if (tpe <:< FloatType)   Specialized.ForFloat
			else if (tpe <:< ShortType)   Specialized.ForShort
			else if (tpe <:< UnitType)    Specialized.ForUnit
			else Specialized.Erasure
		).asInstanceOf[Specialized[T]]
	}

	def ofType[T](tpe :Type) :RuntimeType[T] = {
		if (tpe =:= typeOf[Any])
			RuntimeType.OfAny.asInstanceOf[RuntimeType[T]] //else clause throws ClassNotFound for Any
		else if (tpe <:< AnyValType)           //use <:< because the type can be narrowed to Singleton
			(if (tpe <:< IntType)         RuntimeType.OfInt
			else if (tpe <:< LongType)    RuntimeType.OfLong
			else if (tpe <:< DoubleType)  RuntimeType.OfDouble
			else if (tpe <:< BooleanType) RuntimeType.OfBoolean
			else if (tpe <:< ByteType)    RuntimeType.OfByte
			else if (tpe <:< CharType)    RuntimeType.OfChar
			else if (tpe <:< FloatType)   RuntimeType.OfFloat
			else if (tpe <:< ShortType)   RuntimeType.OfShort
			else if (tpe <:< UnitType)    RuntimeType.OfUnit
			else ofValueClass[T](tpe)).castFrom[RuntimeType[_], RuntimeType[T]]
		else
			RuntimeType.ofClass(runtimeMirror(getClass.getClassLoader).runtimeClass(tpe).castParam[T])
	}

	//todo: verify this actually works
	private def ofValueClass[T](tpe :Type) :RuntimeType[T] = ValueTypes.getOrElse(tpe, null) match {
		case null =>
			tpe.decls.flatMap { field =>
				if (!field.isTerm && field.asTerm.isVal)
					None
				else
					tpe.member(field.name.decodedName).alternatives.find {
						s => s.isMethod && s.asMethod.paramLists.isEmpty
					}.map { getter => ofType[T](getter.asMethod.returnType) }
			} match {
				case Seq(memberType) =>
					ValueTypes.put(tpe, memberType)
					memberType
				case _ =>
					RuntimeType.ofClass(runtimeMirror(getClass.getClassLoader).runtimeClass(tpe).asInstanceOf[Class[T]])
			}
		case tpe => tpe.castParam[T]
	}

	private[this] final val AnyRefType  = implicitly[TypeTag[AnyRef]].tpe
	private[this] final val AnyValType  = implicitly[TypeTag[AnyVal]].tpe
	private[this] final val IntType     = implicitly[TypeTag[Int]].tpe
	private[this] final val DoubleType  = implicitly[TypeTag[Double]].tpe
	private[this] final val LongType    = implicitly[TypeTag[Long]].tpe
	private[this] final val ByteType    = implicitly[TypeTag[Byte]].tpe
	private[this] final val CharType    = implicitly[TypeTag[Char]].tpe
	private[this] final val BooleanType = implicitly[TypeTag[Boolean]].tpe
	private[this] final val FloatType   = implicitly[TypeTag[Float]].tpe
	private[this] final val ShortType   = implicitly[TypeTag[Short]].tpe
	private[this] final val UnitType    = implicitly[TypeTag[Unit]].tpe

	private[this] val ValueTypes :collection.mutable.Map[Type, RuntimeType[_]] = TrieMap[Type, RuntimeType[_]]()

}






/** Base trait describing context where type `T` is used as a generic type argument subject to possible erasure and
  * Scala 2 specialization. There is always an implicit value of `Specialized[T]` for any type `T`, representing
  * that upper bound of `java.lang.Object` or a Java primitive type, even if `T` is abstract.
  * When summoned from a method `@specialized` for the type (including methods of classes specialized for the type
  * which satisfy the specialization conditions), the instance will always be the same constant declared
  * in the companion object. However, exactly as with `RuntimeType`, an instance of `Specialized` can be also
  * derived from an implicit `TypeTag` or `ClassTag` (in that order of preference), even in a non-specialized context.
  * if no more specific type information is available. Unlike in the case of the supertype, though,
  * [[net.noresttherein.sugar.reflect.RuntimeType.runType runType]] of an instance can only be one of the ten
  * predefined constants in object [[net.noresttherein.sugar.reflect.Specialized$ Specialized]] which form the set
  * of legal arguments to `@specialized` annotation: the nine Java primitive types (including `void`,
  * one of the representations of Scala `Unit`), and an erased boxed value `Specialized[Any]/Specialized[AnyRef]`.
  * Which of those ten instances is obtained when summoning depends on the type and amount of information about
  * the type at the summoning point.
  *
  * In other words, the runtime type and [[net.noresttherein.sugar.reflect.RuntimeType.GenericType generic]] type
  * are the same for all instances of `Specialized`. It thus allows to retain the same information about type `T`
  * as Scala's `@specialized` annotation, and the information can be passed as a type class instead of a stack
  * of calls of specialized methods, with the full ability to call back the appropriate specific variant
  * of a specialized method at any point (by using a [[net.noresttherein.sugar.reflect.Specialize Specialize]] callback
  * or one of its parameterized versions). Consider:
  * {{{
  *     def typeName[@specialized T](t :T) = s"\$t is an \${RuntimeType[T].scalaName}"
  *
  *     println(typeName(1)) // "1 is an Int"
  *     println(typeName(true)) //"true is an Boolean"
  *     println(typeName("hamster")) //"hamster is an Object"
  * }}}
  * but
  * {{{
  *     def intIsSpecial[@specialized(Int) T](t :T) =
  *         if (RuntimeType[T] == RuntimeType.OfInt) s"hey, we got an Int: \${RuntimeType[T].classTag}!"
  *         else s"something else: \${RuntimeType[T].classTag} :("
  *
  *     def any[T](t :T) = intIsSpecial(t)
  *     println(intIsSpecial(1)) // "hey, we got an Int: Int"
  *     println(intIsSpecial(1.0))  //"something else: Object :("
  *     println(any(1)) //"something else: Object :("
  * }}}
  * Note that in the last case we lost specialization information because erased method `any` called the generic
  * version of `intIsSpecial`.
  *
  * A secondary use case is the ability to invoke a specialized variant of a method even from the context where
  * actual type argument is erased in the byte code, by passing the type class along the call stack:
  * {{{
  *     def newArray[@specialized T] = RuntimeType[T].newArray(42).asInstanceOf[Array[T]]
  *
  *     object SpecArray extends Specialize[Array] {
  *         def specialized[@specialized T :RuntimeType] = newArray[T]
  *     }
  *
  *     //erased method without any specialization
  *     def unspecialized[T :RuntimeType](t :T) = SpecArray[T]()
  *
  *     println(unspecialized(1).getClass.getSimpleName) //int[]
  *     println(unspecialized(1.0).getClass.getSimpleName) //double[]
  *     println(unspecialized("hello").getClass.getSimpleName) //Object[]
  *     val strings = unspecialized("hello") //java.lang.ClassCastException: [Ljava.lang.Object; cannot be cast to [Ljava.lang.String;
  * }}}
  * Note that:
  *   1. method `unspecialized` is simply erased, no specialized variants are generated by the scala compiler
  *   2. method `newArray` doesn't require any implicit or explicit parameters, retrieving specialization information
  *      by `RuntimeType.apply[T]`
  *   3. `ClassCastException` is the consequence of erasure of the type parameter and cast in the `newArray` method
  *      which performs a (purely syntactic in this case) cast down from an erased array to array
  *      of the given type parameter.
  */ //Specialized to enforce specialization of the factory method.
sealed trait Specialized[@specialized T] extends RuntimeType[T] {
	override type GenericType = RunType

	override def genericType :Class[GenericType] = runType
	override def emptyGenericArray :Array[GenericType] = emptyArray
	override def erasedClassTag :ClassTag[GenericType] = classTag

	override def isErased = false

	override def asSubtype[S <: T] :Specialized[S] = this.asInstanceOf[Specialized[S]]

	protected[reflect] override val discriminator :Enforce[T]

	override def toString :String = "[@specialized(" + classTag + ")]"
}




/** Provides access to representations of types after erasure and specialization. */
@SerialVersionUID(Ver)
object Specialized extends SpecializedFromType {

	/** An argument for `scala.specialized` annotation specializing for all primitives, including `Unit/void`.
	  * This is equivalent to parameterless `@specialized`, but may be useful as a switch value.
	  */
	final val All :Specializable.Group[(Byte, Short, Int, Long, Char, Float, Double, Boolean, Unit, AnyRef)] = null

	/** All possible types with the exception of `Unit` */
	final val NotUnit :Specializable.Group[(Byte, Short, Int, Long, Char, Float, Double, Boolean, AnyRef)] = null

	/** An argument for `scala.specialized` annotation specializing for all java primitives, including `Unit/void`. */
	final val Primitives :Specializable.Group[(Byte, Short, Int, Long, Char, Float, Double, Boolean, Unit)] = null

	/** An argument for `scala.specialized` annotation specializing for all java primitives, excluding `Unit/void`. */
	final val Vals :Specializable.Group[(Byte, Short, Int, Long, Char, Float, Double, Boolean)] = null

	/** An argument for the `@specialized` annotation specializing for `AnyRef` and all primitives,
	  * except for `Boolean` and `Unit`.
	  */
	final val MultiValue :Specializable.Group[(Byte, Short, Int, Long, Char, Float, Double, AnyRef)] = null

	/** An argument for `@specialized` annotation specializing for all value types which are not easily cached (or `Short`). */
	final val NotCached :Specializable.Group[(Char, Int, Long, Float, Double)] = null

	/** Most commonly used value types. */ //consider: adding Float
	final val Common :Specializable.Group[(Byte, Char, Int, Long, Double)] = null

	/** Most commonly used value types as return values. */
	final val CommonRes :Specializable.Group[(Byte, Char, Int, Long, Double, Boolean, Unit)] = null

	/** An argument for `scala.specialized` annotation specializing for all numeric value classes. */
	final val Numbers :Specializable.Group[(Byte, Short, Int, Long, Float, Double)] = null

	/** Types for which `scala.Function0` (that is, lazy expressions) is specialized.
	  * This includes every primitive type.
	  */
	final val Fun0 = Primitives

	/** Types `scala.Function1`'s argument is specialized for. */
	final val Fun1Arg :Specializable.Group[(Int, Long, Float, Double)] = null

	/** Types `scala.Function1`'s result type is specialized for. */
	final val Fun1 :Specializable.Group[(Int, Long, Float, Double, Boolean, Unit)] = Specializable.Return
	//new Specializable.Group(Unit, Boolean, Int, Float, Long, Double)

	/** Result types `scala.Function1` is specialized for except `Unit`. */
	final val Fun1Vals :Specializable.Group[(Int, Long, Float, Double, Boolean)] = null

	/** Types `scala.Function2`s arguments are specialized for. */
	final val Fun2Arg :Specializable.Group[(Int, Long, Double)] = null

	/** Types `scala.Function2` result type is specialized for - same as `Fun1Res`. */
	final val Fun2 :Specializable.Group[(Int, Long, Float, Double, Boolean, Unit)] = null

	/** Result types `scala.Function2` is specialized for, except for `Unit` - same as `Fun1Vals`. */
	final val Fun2Vals :Specializable.Group[(Int, Long, Float, Double, Boolean)] = null

	/** Element types `scala.Tuple2` is specialized for. */
	final val Tuple2Elem :Specializable.Group[(Int, Long, Char, Double, Boolean)] = null

	/** The types for which [[scala.collection.Stepper Stepper]] and Java [[java.util.Iterator Iterator]] are specialized. */
	final val Steppers :Specializable.Group[(Int, Long, Double, AnyRef)] = null


	/** Summons an implicit value for [[net.noresttherein.sugar.reflect.Specialized Specialized]]`[T]`,
	  * representing the way it would be used in context of a generic call, after erasure or specialization.
	  * There will always be a value for every type, in the most generic scenario representing the complete erasure
	  * and boxing (for value types).
	  */
	@inline def apply[T](implicit manifest :Specialized[T]) :Specialized[T] = manifest

	/** Determines the local specialization context at the point of calling.
	  * Same as [[net.noresttherein.sugar.reflect.Specialized RuntimeType.specialized]].
	  */
	@inline def locally[@specialized T] :Specialized[T] = RuntimeType.specialized[T]


	/** Return specialization type class instance which uses the given class as the runtime class.
	  * This represents the case where no static information is lost except for potential type parameters of `T`,
	  * if it is a generic type itself. For classes representing java primitives (including `Unit/void`)
	  * the corresponding specialization constant is returned. For reference types
	  * a [[net.noresttherein.sugar.reflect.RuntimeType.RefRuntimeType RefRuntimeType]] instance wrapping
	  * the given class is returned. Custom value classes are likewise represented by their lifted reference type.
	  * @return a `RuntimeType` instance which `runType` equals the given class.
	  */
	@inline def ofClass[T](tpe :Class[T]) :Specialized[T] = RuntimeType.genericClass(tpe)

	/** Return specialization type class instance specific to the given class, based on an implicit `ClassTag`.
	  * Equal to [[net.noresttherein.sugar.reflect.Specialized.ofClass ofClass]](classTag[T].runtimeClass).
	  * Note that, in context where `ClassTag[T]` is available implicitly, but `T` is an erased abstract type,
	  * returned instance will be based on that class tag and equal to the appropriate value class specialization
	  * for java primitives, despite values of `T` being auto boxed in that context.
	  *
	  * @tparam T type for which specialization should be resolved.
	  * @return an instance representing either one of java primitives or `java.lang.Object`.
	  */
	@inline def of[T :ClassTag] :Specialized[T] = RuntimeType.generic[T]

	/** The best representation of static type `T` based on implicit type information
	  * once erasure is performed for reference types.
	  * @return an instance representing either a java primitive (including `void`), synthetic `Null`
	  *         or erasure/boxing (for custom value types) to `AnyRef`.
	  */
	def ofType[T :TypeTag] :Specialized[T] = RuntimeTypes.specializedType[T]

	/** A shorthand for [[net.noresttherein.sugar.reflect.RuntimeType.ofClass ofClass]]`(array.getClass.getComponentType)` */
	@inline def ofElements[T](array :Array[T]) :RuntimeType[T] = ofClass(array.getClass.getComponentType.castParam[T])

	/** Most specific specialization for the given value. If `value` is a boxed java primitive, this will be the
	  * specialization for the appropriate value type. In all other cases, it will be an instance representing
	  * `value.getClass`.
	  */
	def ofValue[T](value :T) :Specialized[T] =
		(value match {
			case _ :j.Number => value match {
				case _ :j.Integer => OfInt
				case _ :j.Long    => OfLong
				case _ :j.Double  => OfDouble
				case _ :j.Byte    => OfByte
				case _ :j.Float   => OfFloat
				case _ :j.Short   => OfShort
				case _            => Erasure
			}
			case _ :j.Character => OfChar
			case _ :j.Boolean   => OfBoolean
			case _ :Unit        => OfUnit
			case _ if value.getClass eq classOf[AnyRef] => OfAnyRef
			case _ => Erasure
		}).asInstanceOf[Specialized[T]]

	/** Representation of any type as its auto boxed, erased form without any specialization or upper type bounds.
	  * @return a singleton instance, with all type members are defined as `AnyRef`,
	  *         and [[net.noresttherein.sugar.reflect.RuntimeType.runType runType]],
	  *         [[net.noresttherein.sugar.reflect.RuntimeType.genericType genericType]],
	  *         [[net.noresttherein.sugar.reflect.RuntimeType.boxType boxType]] all equal `classOf[AnyRef]`.
	  */
	@inline def erased[T] :Specialized[T] = Erasure.asInstanceOf[Specialized[T]]


	/** Default implicit value used for abstract types `T` based on passed `ClassTag`. Unlike the corresponding
	  * `RuntimeType` implicit, all reference types and value types without java primitive representation are
	  * collated to the same 'erased' instance.
	  */
	@inline implicit def specializedClassTag[T :ClassTag] :Specialized[T] = RuntimeType.generic[T]

	/** Default implicit value for all reference types representing erasure. All returned values are equal regardless
	  * of type parameter `T`.
	  */
	@inline implicit def specializedRef[T <: AnyRef] :Specialized[T] = Erasure.asInstanceOf[Specialized[T]]


	/** Specialization for `Byte`. */
	@SerialVersionUID(Ver)
	implicit object ForByte extends SpecializedPrimitive[Byte, j.Byte](new Enforce[Byte], 0) {
		protected[reflect] override def call[R[_]](callback: Specifically[R])
		                                          (implicit force: Enforce[Byte]) :R[Byte] =
			callback.forByte

		protected[reflect] override def call[P[_], R[_]](callback :Specifically.WithArg[P, R])(param :P[Byte])
		                                                (implicit force :Enforce[Byte]) :R[Byte] =
			callback.forByte(param)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift[R])(param :Byte) :R[Byte] =
			callback.forByte(param)

		protected[reflect] override def call[P1[_], P2[_], R[_]](callback :Specifically.With2Args[P1, P2, R])
		                                                        (param1 :P1[Byte], param2 :P2[Byte])
		                                                        (implicit force :Enforce[Byte]) :R[Byte] =
			callback.forByte(param1, param2)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift2[R])
		                                          (param1 :Byte, param2 :Byte) :R[Byte] =
			callback.forByte(param1, param2)
	}


	/** Specialization for `Short`. */
	@SerialVersionUID(Ver)
	implicit object ForShort extends SpecializedPrimitive[Short, j.Short](new Enforce[Short], 0) {
		protected[reflect] override def call[R[_]](callback: Specifically[R])
		                                          (implicit force: Enforce[Short]) :R[Short] =
			callback.forShort

		protected[reflect] override def call[P[_], R[_]](callback :Specifically.WithArg[P, R])
		                                                (param :P[Short])(implicit force :Enforce[Short]) :R[Short] =
			callback.forShort(param)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift[R])(param :Short) :R[Short] =
			callback.forShort(param)

		protected[reflect] override def call[P1[_], P2[_], R[_]](callback :Specifically.With2Args[P1, P2, R])
		                                                        (param1 :P1[Short], param2 :P2[Short])
		                                                        (implicit force :Enforce[Short]) :R[Short] =
			callback.forShort(param1, param2)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift2[R])
		                                          (param1 :Short, param2 :Short) :R[Short] =
			callback.forShort(param1, param2)
	}


	/** Specialization for `Int`. */
	@SerialVersionUID(Ver)
	implicit object ForInt extends SpecializedPrimitive[Int, j.Integer](new Enforce[Int], 0) {
		protected[reflect] override def call[R[_]](callback: Specifically[R])
		                                          (implicit force: Enforce[Int]) :R[Int] =
			callback.forInt

		protected[reflect] override def call[P[_], R[_]](callback :Specifically.WithArg[P, R])
		                                                (param :P[Int])(implicit force :Enforce[Int]) :R[Int] =
			callback.forInt(param)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift[R])(param :Int) :R[Int] =
			callback.forInt(param)

		protected[reflect] override def call[P1[_], P2[_], R[_]](callback :Specifically.With2Args[P1, P2, R])
		                                                        (param1 :P1[Int], param2 :P2[Int])
		                                                        (implicit force :Enforce[Int]) :R[Int] =
			callback.forInt(param1, param2)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift2[R])(param1 :Int, param2 :Int) :R[Int] =
			callback.forInt(param1, param2)
	}


	/** Specialization for `Long`. */
	@SerialVersionUID(Ver)
	implicit object ForLong extends SpecializedPrimitive[Long, j.Long](new Enforce[Long], 0) {
		protected[reflect] override def call[R[_]](callback: Specifically[R])
		                                          (implicit force: Enforce[Long]) :R[Long] =
			callback.forLong

		protected[reflect] override def call[P[_], R[_]](callback :Specifically.WithArg[P, R])
		                                                (param :P[Long])(implicit force :Enforce[Long]) :R[Long] =
			callback.forLong(param)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift[R])(param :Long) :R[Long] =
			callback.forLong(param)

		protected[reflect] override def call[P1[_], P2[_], R[_]](callback :Specifically.With2Args[P1, P2, R])
		                                                        (param1 :P1[Long], param2 :P2[Long])
		                                                        (implicit force :Enforce[Long]) :R[Long] =
			callback.forLong(param1, param2)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift2[R])
		                                          (param1 :Long, param2 :Long) :R[Long] =
			callback.forLong(param1, param2)
	}


	/** Specialization for `Char`. */
	@SerialVersionUID(Ver)
	implicit object ForChar extends SpecializedPrimitive[Char, j.Character](new Enforce[Char], 0) {
		protected[reflect] override def call[R[_]](callback: Specifically[R])
		                                          (implicit force: Enforce[Char]) :R[Char] =
			callback.forChar

		protected[reflect] override def call[P[_], R[_]](callback :Specifically.WithArg[P, R])
		                                                (param :P[Char])(implicit force :Enforce[Char]) :R[Char] =
			callback.forChar(param)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift[R])(param :Char) :R[Char] =
			callback.forChar(param)

		protected[reflect] override def call[P1[_], P2[_], R[_]](callback :Specifically.With2Args[P1, P2, R])
		                                                        (param1 :P1[Char], param2 :P2[Char])
		                                                        (implicit force :Enforce[Char]) :R[Char] =
			callback.forChar(param1, param2)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift2[R])
		                                          (param1 :Char, param2 :Char) :R[Char] =
			callback.forChar(param1, param2)
	}


	/** Specialization for `Float`. */
	@SerialVersionUID(Ver)
	implicit object ForFloat extends SpecializedPrimitive[Float, j.Float](new Enforce[Float], 0) {
		protected[reflect] override def call[R[_]](callback: Specifically[R])
		                                          (implicit force: Enforce[Float]) :R[Float] =
			callback.forFloat

		protected[reflect] override def call[P[_], R[_]](callback :Specifically.WithArg[P, R])
		                                                (param :P[Float])(implicit force :Enforce[Float]) :R[Float] =
			callback.forFloat(param)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift[R])(param :Float) :R[Float] =
			callback.forFloat(param)

		protected[reflect] override def call[P1[_], P2[_], R[_]](callback :Specifically.With2Args[P1, P2, R])
		                                                        (param1 :P1[Float], param2 :P2[Float])
		                                                        (implicit force :Enforce[Float]) :R[Float] =
			callback.forFloat(param1, param2)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift2[R])
		                                          (param1 :Float, param2 :Float) :R[Float] =
			callback.forFloat(param1, param2)
	}


	/** Specialization for `Double`. */
	@SerialVersionUID(Ver)
	implicit object ForDouble extends SpecializedPrimitive[Double, j.Double](new Enforce[Double], 0) {
		protected[reflect] override def call[R[_]](callback: Specifically[R])
		                                          (implicit force: Enforce[Double]) :R[Double] =
			callback.forDouble

		protected[reflect] override def call[P[_], R[_]](callback :Specifically.WithArg[P, R])
		                                                (param :P[Double])(implicit force :Enforce[Double]) :R[Double] =
			callback.forDouble(param)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift[R])(param :Double) :R[Double] =
			callback.forDouble(param)

		protected[reflect] override def call[P1[_], P2[_], R[_]](callback :Specifically.With2Args[P1, P2, R])
		                                                        (param1 :P1[Double], param2 :P2[Double])
		                                                        (implicit force :Enforce[Double]) :R[Double] =
			callback.forDouble(param1, param2)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift2[R])
		                                          (param1 :Double, param2 :Double) :R[Double] =
			callback.forDouble(param1, param2)
	}


	/** Specialization for `Boolean`. */
	@SerialVersionUID(Ver)
	implicit object ForBoolean extends SpecializedPrimitive[Boolean, j.Boolean](new Enforce[Boolean], false) {
		protected[reflect] override def call[R[_]](callback: Specifically[R])
		                                          (implicit force: Enforce[Boolean]) :R[Boolean] =
			callback.forBoolean

		protected[reflect] override def call[P[_], R[_]](callback :Specifically.WithArg[P, R])(param :P[Boolean])
		                                                (implicit force :Enforce[Boolean]) :R[Boolean] =
			callback.forBoolean(param)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift[R])(param :Boolean) :R[Boolean] =
			callback.forBoolean(param)

		protected[reflect] override def call[P1[_], P2[_], R[_]](callback :Specifically.With2Args[P1, P2, R])
		                                                        (param1 :P1[Boolean], param2 :P2[Boolean])
		                                                        (implicit force :Enforce[Boolean]) :R[Boolean] =
			callback.forBoolean(param1, param2)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift2[R])
		                                          (param1 :Boolean, param2 :Boolean) :R[Boolean] =
			callback.forBoolean(param1, param2)
	}


	/** Specialization for `Unit` as java `void`. */
	@SerialVersionUID(Ver)
	implicit object ForUnit extends SpecializedPrimitive[Unit, BoxedUnit](new Enforce[Unit], ()) { //todo - this is not really a primitive:
		protected[reflect] override def call[R[_]](callback: Specifically[R])
		                                          (implicit force: Enforce[Unit]) :R[Unit] =
			callback.forUnit

		protected[reflect] override def call[P[_], R[_]](callback :Specifically.WithArg[P, R])
		                                                (param :P[Unit])(implicit force :Enforce[Unit]) :R[Unit] =
			callback.forUnit(param)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift[R])(param :Unit) :R[Unit] =
			callback.forUnit(param)

		protected[reflect] override def call[P1[_], P2[_], R[_]](callback :Specifically.With2Args[P1, P2, R])
		                                                        (param1 :P1[Unit], param2 :P2[Unit])
		                                                        (implicit force :Enforce[Unit]) :R[Unit] =
			callback.forUnit(param1, param2)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift2[R])
		                                          (param1 :Unit, param2 :Unit) :R[Unit] =
			callback.forUnit(param1, param2)
	}


	/** Represents the erased context, in which the nominal type is referenced as `AnyRef` (`java.lang.Object`)
	  * in the byte code and cast down when leaving the generic context. It is used (after casting)
	  * as the `Specialized` instance, regardless of the type parameter, in all not specialized code.
	  * It is public primarily to allow exhaustive pattern matching, and in order to manually obtain
	  * an erased instance for `T` prefer using [[net.noresttherein.sugar.reflect.RuntimeType.erased erased]].
	  */
	@SerialVersionUID(Ver)
	object Erasure extends Specialized[Any] {
		override type RunType = Any
		override type BoxType = AnyRef

		override val runType = classOf[Any]
		override val boxType = classOf[AnyRef]

		override val classTag = ClassTag[Any](runType)
		override val boxClassTag = ClassTag[AnyRef](boxType)

		override def isErased = true

		override def default :Any = null

		override val emptyArray :Array[Any] = new Array[Any](0)
		override val emptyBoxArray :Array[AnyRef] = new Array[AnyRef](0)

		protected[reflect] override def call[R[_]](callback :Specifically[R])(implicit force :Enforce[Any]) :R[Any] =
			callback.forOthers(this)

		protected[reflect] override def call[P[_], R[_]](callback :Specifically.WithArg[P, R])
		                                                (param :P[Any])(implicit force :Enforce[Any]) :R[Any] =
			callback.forOthers(param)(this)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift[R])(param :Any) :R[Any] =
			callback.forOthers(param)(this) //consider: delegating directly to generic instead

		protected[reflect] override def call[P1[_], P2[_], R[_]](callback :Specifically.With2Args[P1, P2, R])
		                                                        (param1 :P1[Any], param2 :P2[Any])
		                                                        (implicit force :Enforce[Any]) :R[Any] =
			callback.forOthers(param1, param2)(this)

		protected[reflect] override def call[R[_]](callback :Specifically.Lift2[R])
		                                          (param1 :Any, param2 :Any) :R[Any] =
			callback.forOthers(param1, param2)(this)

		protected[reflect] override val discriminator = new Enforce[Any]

		override def toString = "[_]"
	}

	/** A trait extended by `Specialized` instances retaining full type information about `T`, i.e.
	  * representing one of the inbuilt value classes or `AnyRef` itself (but not other reference types).
	  */
	sealed trait SpecializedExact[T] extends Specialized[T] with ExactRuntimeType[T]


	/** Introduced because of scalac bug which caused the class initializer to reassign final value
	  * from the generic superclass.
	  */ //todo: verify if this is still relevant for contemporary compilers
	private[Specialized] sealed abstract class PrimitiveBugWorkaround[T, B <: AnyRef]
	                                                                 (implicit final override val classTag :ClassTag[T])
		extends Specialized[T] with SpecializedExact[T]
	{
		override type BoxType = B

		override final val runType = classTag.runtimeClass.asInstanceOf[Class[T]]
		override final val genericType = runType
		override final val boxType = BoxClass(runType).asInstanceOf[Class[B]]

		override final val erasedClassTag :ClassTag[T] = classTag
		override final val boxClassTag    :ClassTag[B] = ClassTag(boxType)

		override final val emptyArray        :Array[T] = Array.empty[T]
		override final val emptyGenericArray :Array[T] = emptyArray
		override final val emptyBoxArray     :Array[B] = Array.empty[B](boxClassTag)
	}

	/** Base class for all instances representing a ''jvm'' primitive type, including `void`. */
	sealed abstract class SpecializedPrimitive[@specialized V, B <: AnyRef]
	                      (protected[reflect] override final val discriminator :Enforce[V], override final val default :V)
	                      (implicit classTag :ClassTag[V])
		extends PrimitiveBugWorkaround[V, B] with Specialized[V]
	{
		override def toString :String = string
		private[this] val string = super.toString
	}




	/** A token generic class specialized on its type parameter used to enforce specialization of a method by adding
	  * it as an implicit parameter. Implicit value is available for any type argument,
	  * but ''is not specialized itself'', which makes it faster to obtain and easier to inline by the JVM
	  * than a `RuntimeType`. It therefore cannot provide any information about specialization and the latter class
	  * (or [[net.noresttherein.sugar.reflect.Specialized Specialized]]) should be used for that purpose.
	  *
	  * This class defines equality in terms of its runtime class, with two instances being equal '''iff''' `getClass`
	  * returns the same object for both of them. As scala specialization is done by introducing separate synthetic
	  * subclasses for all specialized type parameters, comparing a locally created instance with predefined constants
	  * for all java primitives lets one discover if executed code is specialized and for which value class.
	  */
	@SerialVersionUID(Ver)
	sealed class Enforce[@specialized X] private[reflect] {
		override def equals(that :Any) :Boolean = that.getClass eq getClass
		override def hashCode :Int = getClass.hashCode

		def className :String = getClass.getName

		override def toString :String = className.substring(className.indexOf("$")+1)
	}

	@SerialVersionUID(Ver)
	object Enforce {
		private[this] final val instance = new Enforce[Any]

		@inline implicit def forceSpecialization[X] :Enforce[X] = instance.asInstanceOf[Enforce[X]]
	}
}


/* Unfortunately all implicit definitions must be repeated for `Specialized` again with the same precedence order,
 * as implicit values of RuntimeType based on ClassTag and TypeTag are not Specialized instances (they retain
 * the type information), and even declaring the implicit based on specialization as Specialized causes a resolution
 * conflict.
 */
private[reflect] sealed abstract class SpecializedFromAnnotation {
	/** Fallback implicit `Specialized[T]` for any concrete and abstract type `T` discovering `T` based on
	  * (potential) specialization of the calling code.
	  */
	@inline implicit final def specializedAnnotation[@specialized T] :Specialized[T] = RuntimeType.specialized[T]
}


private[reflect] sealed abstract class SpecializedFromType extends SpecializedFromAnnotation {
	/** Implicit value discovering information about type `T` based on implicit `TypeTag`. It is of second precedence
	  * to `ClassTag` as the latter provides all required information directly and using is faster.
	  */
	implicit final def specializedType[T](implicit tag :TypeTag[T]) :Specialized[T] =
		RuntimeTypes.specializedType[T]

}
