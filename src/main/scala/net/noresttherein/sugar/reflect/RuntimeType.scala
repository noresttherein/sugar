package net.noresttherein.sugar.reflect

import scala.annotation.{implicitNotFound, unspecialized}
import scala.collection.concurrent.TrieMap
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{Type, TypeTag, runtimeMirror, typeOf}
import scala.runtime.BoxedUnit

import net.noresttherein.sugar.extensions.{castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.reflect.RuntimeType.{Enforce, OfBoolean, OfDouble, OfFloat, OfInt, OfLong, OfUnit, Specialized}
import net.noresttherein.sugar.reflect.RuntimeType.Specialized.{Fun1, Fun1Vals, Primitives}
import net.noresttherein.sugar.reflect.Specialize.SpecializeIndividually






/** Type class describing the representation of type `T` in compiled byte code as known at the point of its implicit
  * summoning or inferred from an implicit `TypeTag` or `ClassTag`. It  is used in conjunction with generic classes and
  * methods to reflect runtime information about their type parameters. Its primary function is providing the ability to
  * distinguish between specialized synthetic versions of a generic class or method marked as `@specialized T`
  * (with optional parameters to the annotation). Unlike `ClassTag`s and `TypeTag`s, an implicit value of this
  * class is available even in contexts where `T` is abstract, representing the type after erasure and specialization.
  * In particular, it makes it possible for a generic class/trait/method to discover if it was instantiated/called
  * with one of the value types as the parameter and to which primitive java type it corresponds. In a generic, erased
  * context, `implicitly[RuntimeType[T]]` will denote an erased (and possibly boxed) type represented in the byte code 
  * as `java.lang.Object` instances (and downcast at point of calling). If, on the other hand, `T` can be proven to be 
  * an inbuilt value type, the implicit value will be specific to that type. This permits to pick a specific 
  * implementation, optimized for the given value type, or to verify if two collections share the element type.
  * The lack of reliance on class tags makes it additionally possible to integrate specialized collections 
  * with the generic standard library which would be otherwise impossible due to established method signatures.
  * For example, `ValSet[Byte]()`, `ValSet[Int]()`, `ValSet[Double]()` may all yield different implementations, 
  * while retaining the flexibility and uniformity of the generic interface.
  *
  * While the focus is on value types and their use with erased and specialized collections, an instance can,
  * potentially, represent any reference type, generalizing the concept to provide a uniform way for specifying the
  * degree of available static information about a given type. In particular, in contexts where `T` 
  * is fully instantiated (i.e., the instance was for example obtained from `RuntimeType[String]`), its full 
  * class information is available to represent lack of any type abstraction. Other reasons are the need for subclasses 
  * of generic, specialized types, which are dedicated to a concrete reference type 
  * (as in `class StringSet extends ValSet[String]`), to be able to correctly inform about their actual type argument, 
  * and a way to manually request a specific array type via the same interface.
  *
  * It thus serves as a common umbrella for information available via scala reflection, class tags, 
  * and local specialization. This makes it more powerful, for example allowing to call specialized code 
  * from non-specialized based on this ype class, which in turn can make generated specialized classes smaller 
  * by extracting the code actually referencing values of this type from methods which can be implemented 
  * without this information. For example, the majority of collection methods accepting a function working
  * with their element type can be implemented in the generic form, deferring the application point to a single,
  * reusable specialized method.
  *
  * In general, however, relying on this class for type safety is a very tricky ground, due to the muddy relationship
  * between static and dynamic types of scala code:
  *   - the duality of scala inbuilt 'AnyVal's, represented in runtime by both java primitives and their wrappers,
  *     and thus two different `Class[_]` instances, which forces any API to either become a leaky abstraction,
  *     reflecting that duality, 'lie', committing to potentially costly conversions, or risk casting errors -
  *     especially when dealing with arrays.
  *   - upper or lower bounds on type arguments which may affect the actual byte code signature of the method 
  *     are not available;
  *   - implementations of methods declared in more generic super types, which may have a more specialized erasure
  *     than the signature of the overridden method.
  *   - information lost during erasure, which still may cause conflicts such as with arrays (or other specialized code)
  *     with element type which is generic itself.
  *
  * While scala compiler and runtime make their best attempt to hide this from user code by auto-boxing and un-boxing,
  * without being careful it is still possible to run into `ClassCastException`s, especially when working with arrays.
  *
  * More formally, an instance of [[net.noresttherein.sugar.reflect.RuntimeType RuntimeType]]`[T]` may represent 
  * any runtime type assignable from `T`, potentially using autoboxing. For example, `RuntimeType[Int]` may represent
  * any possible runtime type of an expression evaluating to `scala.Int`: `int`, `java.lang.Integer`, 
  * or (erased) `java.lang.Object`. Instances representing each of these types are not considered equal,
  * despite their corresponding types being almost functionally equivalent in the bytecode and `Int` 
  * lacking polymorphism. For primitives, this is largely transparent - the code operating on any of these 
  * will be generally interchangeable due to autoboxing. The situation is more complex for reference types,
  * as the type hierarchy is unlimited and there is no auto-conversion involved (other than 
  * identity conversion / upcasting). This means there is no homomorphism between type `T` and `RuntimeType[T]`
  * regarding equality: both different instances of `RuntimeType[T]` may be unequal, being incompatible, and equality
  * between `RuntimeType[T]` and `RuntimeType[F]` confirms only that types `T` and `F` have identical representation 
  * in referenced contexts, but potentially little about their actual relationship - in particular, 
  * both specializations may refer to `AnyRef`.
  *
  * Instances of `RuntimeType[T]` can be obtained from the companion object based either on a runtime
  * class/`ClassTag`/`TypeTag`, or existing specialization context. Implicit values are provided directly for all value
  * types (and several other common types), or created based on any of the aforementioned sources implicitly available
  * (in exactly that order). The consequence of that fact is that the default value will always reflect the most
  * specific representation that can be determined and, in case of `AnyRef` subtypes, whenever actual information
  * about the class is known (in particular always when `T` is statically known), it will be used rather than the erased
  * `AnyRef`. It may not always be the desirable behaviour and, in cases where only root level specialization is of
  * interest (that is, if the type in question is a jvm primitive or `java.lang.Object`),
  * [[net.noresttherein.sugar.reflect.RuntimeType$.Specialized Specialized]]
  * instance may be requested instead.
  *
  * Implicitly provided `Specialized[T]` for all built-in value types will either represent a corresponding java
  * primitive or an erased and boxed value `Specialized[AnyRef]`, depending on whether the actual type was known
  * or erased in the context in which it was created. Consider:
  * {{{
  *     def typeName[@specialized T](t :T) = s"$t is a ${RuntimeType[T].typeName}"
  *
  *     println(typeName(1)) // "1 is a int"
  *     println(typeName(true)) //"true is a boolean"
  *     println(typeName("hamster")) //"hamster is a java.lang.Object"
  * }}}
  * or
  * {{{
  *     def intIsSpecial[@specialized(Int) T](t :T) =
  *         if (RuntimeType[T] == RuntimeType.OfInt) s"hey, we got an int: ${RuntimeType[T].classTag}!"
  *         else s"something else: ${RuntimeType[T].classTag} :("
  *
  *     def any[T](t :T) = intIsSpecial(t)
  *     println(intIsSpecial(1)) // "hey, we got an int: Int"
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
  *
  * An implicit value `RuntimeType[T]` is always present for any concrete or abstract type `T`. If `T` is known to be
  * one of the specializable scala types in the point of resolution, a constant singleton for that primitive is returned.
  * If `T` is an abstract type or proper subclass of `AnyRef`, implicit resolution defaults to a lookup mechanism
  * which attempts to recognize if it is requested from a context where `T` is a specialized type parameter. Therefore
  * specialization context can be passed from method to method in two ways: either as a `RuntimeType` context bound
  * (type class-like implicit parameter), or by preserving `@specialized` annotation on the type.
  * As the implicit resolution algorithm searches local context before companion objects, any implicit parameter 
  * will override
  * default implicit values declared in [[RuntimeType$]]. This can lead to situations, where the two values aren't equal,
  * usually due to execution of non-specialized generic code on the call stack, while a `RuntimeType` instance is passed
  * as an implicit parameter. Consider:
  * {{{
  *     def whoIs[T :RuntimeType] = RuntimeType[T] //retrieve an instance from the implicit parameter
  *     def whoIs2[@specialized T] = RuntimeType[T] //retrieve an instance based on method specialization
  *     def erase[T] = RuntimeType[T]
  *
  *     def erased[T] = whoIs[T] -> whoIs2[T]
  *     def spec[@specialized T] = whoIs[T] -> whoIs2[T]
  *     def typeClass[T :RuntimeType] = whoIs[T] -> whoIs2[T]
  *     def both[@specialized T :RuntimeType] = whoIs[T] -> whoIs2[T]
  *
  *     println(erased[Int])           //"(Object, Object)"
  *     println(spec[Int])             //"(int, int)"
  *     println(typeClass[Int])        //"(int, Object)"
  *     println(both[Int])             //"(int, int)"
  *     println(both[Int](erase[Int])) //"(Object, int)"
  * }}}
  *
  * @tparam T any scala type, usually itself a type parameter of a generic method/class; 
  *           this is not the final erased/unboxed type.
  * @see [[net.noresttherein.sugar.reflect.Specialize]] for calling specialized code from non-specialized context.
  * @see [[net.noresttherein.sugar.reflect.RuntimeType.Specialized]]
  * @see [[net.noresttherein.sugar.reflect.RuntimeType.ValueClass]]
  * @author Marcin Mościcki
  */
@implicitNotFound("Cannot determine the runtime type of type ${T}. " +
                  "This is most likely a result of introduction of a conflicting implicit value for RuntimeType[${T}].")
sealed trait RuntimeType[@specialized T] extends Serializable {

	/** Type to which generic methods for `T` are specialized, i.e. representation of `T` inside a method `m[T]`,
	  * after erasure and any specializations. It is the scala alias for either `java.lang.Object` or one of java primitive types.
	  */
	type GenericType >: RunType

	/** Closest information about type `T` as represented in byte code. This is different from
	  * [[net.noresttherein.sugar.reflect.RuntimeType.GenericType GenericType]]
	  * in that it doesn't necessarily represent erasure, but can be in particular any reference type.
	  *
	  * While in general values of `RunType` do not always conform to the type `T` denoted by this instance,
	  * the semantics of java generics and scala specialization as well as their runtimes with regard to autoboxing
	  * mean that the cast `(_ :RunType).asInstanceOf[T]` is safe as long as this instance was obtained in
	  * the context of casting. This is because either `RunType` and `T` are the same specialized value type and
	  * the cast is removed at compilation, or `T` is erased.
	  */
	type RunType >: T

	/** Type to which values of `T` are boxed whenever a reference type is expected. For primitive types it is declared
	  * as their corresponding wrappers, for a reference type `T <: AnyRef` it is simply `T`. The tricky case are
	  * custom value classes which are promoted to `AnyRef` without nominally extending it.
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
	  * compiles to no-op in this method. In fact, it will be will be still safe as long as this instance represents
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
	  * Only possible values are synthetic java classes for java primitives
	  * (i.e. `Integer.TYPE` and `Class[java.lang.Object]`.
	  */
	def genericType :Class[GenericType]

	/** For value types, the java class boxing this value. For reference types, equal to `runType`. */
	def boxType :Class[BoxType]


	/** Shorthand for `runType.getName`. */
	def className :String = runType.getName

	/** Shorthand for `genericType.getName`. */
	def genericClassName :String = genericType.getName

	/** Shorthand for `boxType.getName`. */
	def boxClassName :String = boxType.getName

	/** Scala name of the runtime type of `T` as returned by the `ClassTag` corresponding to the `runType` class. */
	def typeName :String = classTag.toString

	/** Scala name of the type specialized for `T`, as returned by the `ClassTag`
	  * corresponding to the `genericType` class.
	  */
	def genericTypeName :String = erasedClassTag.toString

	/** Scala name of the runtime type of the reference type for `T`, as returned by the `ClassTag`
	  * corresponding to the `boxType` class.
	  */
	def boxTypeName :String = boxClassTag.toString


	/** `ClassTag` representing the type used in the bytecode to represent values of `T`
	  * in place of obtaining this instance.
	  */
	implicit def classTag :ClassTag[RunType] = ClassTag(runType)

	/** `ClassTag` representing the type used in the bytecode to represent values of `T` in contexts where `T`
	  * is an unbound, but possibly specialized, type parameter of a generic type.
	  */
	def erasedClassTag :ClassTag[GenericType] = ClassTag(genericType)

	/** `ClassTag` representing the type to which `T` is boxed wherever a reference type is required
	  * (such as type parameters for generic, non specialized types).
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



	protected[reflect] def call[R[_]](callback: Specialize[R])(implicit force :Enforce[T]) :R[T] =
		callback.specialized(this)

	protected[reflect] final def call[P[_], R[_]](callback: Specialize.With[P, R])(param :P[T])
	                                           (implicit force :Enforce[T]) :R[T] =
		callback.specialized(param)(this)

	protected[reflect] final def call[P1[_], P2[_], R[_]]
	                               (callback :Specialize.With2[P1, P2, R])
	                               (param1 :P1[T], param2 :P2[T])(implicit force :Enforce[T]) :R[T] =
		callback.specialized(param1, param2)(this)

	protected[reflect] def call[R[X]](callback :SpecializeIndividually[R])(implicit force :Enforce[T]) :R[T]


	/** An identifier which is different for every specializable type and a (erased) reference type. */
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
	@inline final def sameAs(other :RuntimeType[_]) :Boolean = runType.isPrimitive && runType == other.runType

	/** Same as [[net.noresttherein.sugar.reflect.RuntimeType.sameAs]]. */
	@inline final def =:=(other :RuntimeType[_]) :Boolean = runType.isPrimitive && runType == other.runType

	/** Equates specialization of primitive types with their representations as auto boxed objects.
	  * For reference type this corresponds simply to equality on `runType`.
	  * @return `true` if `other` boxes to the same type this instance boxes to.
	  */
	def =%=(other :RuntimeType[_]) :Boolean = boxType eq other.boxType

	/** True if all values represented as this runtime type can be safely assigned to a variable represented by `other`,
	  * perhaps including boxing by scala run time. Note that it compares the compiled type representations at possibly
	  * arbitrary points; in particular, if `other` represents `Any`/`AnyRef`/erasure, the relation will hold regardless
	  * of this instance. It is useful with additional invariants. For example, `ArrayPlus[T]` can be potentially
	  * backed by any array `Array[U] forSome { type U >: T }` ''as well as'' `Array[S] forSome { type S &lt;: T }`:
	  * the former due to full or partial erasure and the latter as the effect of its covariance. This relation can
	  * answer the question if elements of another collection can be safely written to the array, while the element type
	  * of the union of the collections is determined statically as their ''LUB'' type. If, in addition, the `&lt;:&lt;`
	  * relation also holds, two backing arrays can be copied using `System.arraycopy`.
	  * @return `other.boxType isAssignableFrom this.boxType`.
	  * @see [[net.noresttherein.sugar.reflect.RuntimeType.<:<]] for additional information.
	  */
	def <%<(other :RuntimeType[_]) :Boolean = other.boxType.isAssignableFrom(boxType)

	/** True if all values represented as this runtime type are directly assignable to variables defined by `other`.
	  * This is weaker than the `&lt;:&lt;` relation on scala `Type` instances and in general says nothing about
	  * subtype relation between the type parameters of compared instances. It occupies itself only with the runtime
	  * class/type at a given point; not only any type parameters of type `T` itself are always erased, but `other`
	  * can potentially represent complete type erasure to `java.lang.Object`. In fact, it is quite possible
	  * that `(x :RuntimeType[X]) &lt;:&lt; (y :RuntimeType[Y])` and `Y &lt;:&lt; X` for different types `X` and `Y`:
	  * it suffices that `Y` is stored as a strict super type of `X` (as in the case of complete type erasure)
	  * and downcast by the ''VM'' when non-abstract reference to a collection element is encountered.
	  * Therefore this check is a very poor substitute for type safety and is useful primarily in conjunction with
	  * additional constraints: either static type bounds, compared types being inbuilt value types, class-specific
	  * invariants.
	  * @return `other.runType isAssignableFrom this.runType`.
	  */
	def <:<(other :RuntimeType[_]) :Boolean = other.runType.isAssignableFrom(runType)

	/** Returns `other &lt;%&lt; this`. */
	def >%>(other :RuntimeType[_]) :Boolean = other <%< this

	/** Returns `other &lt;:&lt; this`. */
	def >:>(other :RuntimeType[_]) :Boolean = other <:< this

	/** Two instances are equal if and only if they represent the same runtime class/type at their respective origin.
	  * This is a weaker relation than equality of corresponding type arguments T; for one, erasure abstracts over
	  * type parameters. More importantly however, a `RuntimeType[T]` may represent any supertype of `T`,
	  * in particular `Any` / `AnyRef`, which can possibly result in equating any two unrelated types. On the other
	  * hand, representations of a value type and a reference type will never be equal, even if the latter is
	  * the runtime box of the former. Likewise, custom value class and its backing value type will also compare as
	  * different, even though scala/java runtime would afford cross-compatibility.
	  * It is primarily useful in the context of specialization, as `runType` being a primitive token class guarantees
	  * @return `this.runType == that.runType` if `that` is a `RuntimeType` and false otherwise.
	  * @see [[net.noresttherein.sugar.reflect.RuntimeType.sameAs]]
	  */
	final override def equals(that :Any) :Boolean = that match {
		case r :RuntimeType[_] => r.runType eq runType
		case _ => false
	}
	final override def hashCode :Int = runType.hashCode

	override def toString :String = "[" + classTag + "]"
}






/** Implicit `RuntimeType` value of third order in precedence, verifying specialization context of the caller.
  * As it will always yield a value, but possibly representing an erased one despite programmer's intentions, it's worse
  * than any sure source of type information, but still better than nothing.
  */
protected[reflect] sealed abstract class FallbackRuntimeTypeImplicit {

	/** Runtime type resolution in the context of the caller.
	  * Implemented by [[net.noresttherein.sugar.reflect.RuntimeType.specialized specialized]] in the subclass.
	  * Returned instance reflects recognized runtime type of `E` in the reference point. If the call happens
	  * from within code specialized for type argument `E` (or `E` is statically known to be a scala value type),
	  * returned instance will carry information about the corresponding primitive. If `E` is erased, or known to be
	  * a reference type, returned instance represents scala `AnyRef`.
	  */
	@inline final implicit def specializedRuntimeType[@specialized E] :RuntimeType[E] = specialized[E]

	/** Resolve local specialization information for type `E`. When called from code specialized for type `E`,
	  * either explicitly by the `@specialized` annotation, or one where `E` is fully instantiated,
	  * it will return an instance associated with the specialized type.
	  * Otherwise (including all `AnyRef` subtypes), a generic instance equal to `Specialized[Any]` is returned.
	  */
	def specialized[@specialized E] :Specialized[E]
}



/** Implicit values for `RuntimeType` type class of secondary precedence, with lower reliability or efficiency
  * than dedicated values declared in [[net.noresttherein.sugar.reflect.RuntimeType$]].
  */
protected[reflect] sealed abstract class SecondaryRuntimeTypeImplicits extends FallbackRuntimeTypeImplicit {

	/** Retrieve specialization information about type `T` from an implicitly available `TypeTag`.
	  * Implemented in subclass by [[net.noresttherein.sugar.reflect.RuntimeType.ofType ofType]].
	  */
	@inline final implicit def runtimeType[T](implicit tpe :TypeTag[T]) :RuntimeType[T] = ofType[T]

	/** Retrieve specialization information for type `T` from an implicitly available `TypeTag`.
	  * 'TypeTag's are more annoying than `ClassTag`s, hence the latter has precedence, but we'll make do
	  * with what we have. Returned instance reflects best runtime type for type `T`, but not necessarily
	  * the one applicable to current context. If type `T` is abstract and not specialized, but a `TypeTag[T]` instance
	  * identifies it as a java primitive type, an instance for that primitive will be returned despite the fact
	  * that all values of `E` in that context might be erased and boxed in runtime.
	  */
	def ofType[T :TypeTag] :RuntimeType[T]
}



@SerialVersionUID(Ver)
object RuntimeType extends SecondaryRuntimeTypeImplicits {
	import java.{lang=>j}

	/** Retrieves implicit information about runtime representation of type 'E' at the point of calling.
	  * This is just a shortcut for `implicitly[RuntimeType[E]]`. Note that there should always be an implicit value
	  * for this parameter: if none is available in the local context, this factory is searched for a matching instance,
	  * picking a declared constant if `E` is a known specializable type, or defaulting to a lookup based
	  * on `@specialized` context. If `E` is abstract or not specializable, returned instance
	  * will equal `RuntimeType.erased[E]`.
	  * @tparam E possibly specialized type parameter of a generic method/class
	  * @return implicit value for `RuntimeType[E]` (which should always be available),
	  *         falling back to a lookup verifying real time type of 'E' in the place of invocation.
	  */
	@inline final def apply[E](implicit specialization :RuntimeType[E]) :RuntimeType[E] = specialization


	/** Return specialization type class instance which uses the given class as the runtime class.
	  * This represents the case where no static information is lost except for potential type parameters of `E`,
	  * if it is a generic type itself. For classes representing java primitives (including `Unit/void`)
	  * the corresponding specialization constant is returned. For reference types
	  * a [[net.noresttherein.sugar.reflect.RuntimeType.RefRuntimeType RefRuntimeType]] instance wrapping
	  * the given class is returned. Custom value classes are likewise represented by their lifted reference type.
	  * @return a `RuntimeType` instance which `runType` equals the given class.
	  */
	final def ofClass[E](tpe :Class[E]) :RuntimeType[E] = (
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
	).asInstanceOf[RuntimeType[E]]

	/** Return specialization type class instance specific to the given class, based on an implicit `ClassTag`.
	  * Equal to [[ofClass]](classTag[E].runtimeClass).
	  * Note that, in context where `ClassTag[E]` is available implicitly, but `E` is an erased abstract type,
	  * returned instance will be based on that class tag and equal to the appropriate value class specialization
	  * for java primitives, despite values of `E` being auto boxed in that context.
	  *
	  * @tparam E type for which specialization should be resolved.
	  * @return an instance representing either one of java primitives or `java.lang.Object`.
	  */
	@inline final def of[E](implicit tpe :ClassTag[E]) :RuntimeType[E] =
		ofClass(tpe.runtimeClass).asInstanceOf[RuntimeType[E]]

	/** The best representation of static type `E` based on implicit type information
	  * once erasure is performed for reference types.
	  * @return an instance representing either a java primitive (including `void`), synthetic `Null`
	  *         or erasure/boxing (for custom value types) to `AnyRef`.
	  */
	override final def ofType[T](implicit tag :TypeTag[T]) :RuntimeType[T] = ofType(tag.tpe)

	private def ofType[T](tpe :Type) :RuntimeType[T] = {
		if (tpe =:= typeOf[Any])
			OfAny.asInstanceOf[RuntimeType[T]] //else clause throws ClassNotFound for Any
		else if (tpe <:< AnyValType)           //use <:< because the type can be narrowed to Singleton
			(if (tpe <:< IntType)         OfInt
			else if (tpe <:< LongType)    OfLong
			else if (tpe <:< DoubleType)  OfDouble
			else if (tpe <:< BooleanType) OfBoolean
			else if (tpe <:< ByteType)    OfByte
			else if (tpe <:< CharType)    OfChar
			else if (tpe <:< FloatType)   OfFloat
			else if (tpe <:< ShortType)   OfShort
			else if (tpe <:< UnitType)    OfUnit
			else ofValueClass[T](tpe)).castFrom[RuntimeType[_], RuntimeType[T]]
		else
			ofClass(runtimeMirror(getClass.getClassLoader).runtimeClass(tpe).castParam[T])
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
					ofClass(runtimeMirror(getClass.getClassLoader).runtimeClass(tpe).asInstanceOf[Class[T]])
			}
		case tpe => tpe.castParam[T]
	}

	/** Most specific specialization for the given value. If `value` is a boxed java primitive, this will be the
	  * specialization for the appropriate value type. In all other cases, it will be an instance representing
	  * `value.getClass`.
	  */
	final def ofValue[E](value :E) :RuntimeType[E] = //ofClass(UnboxedClass(value.getClass).asInstanceOf[Class[E]])
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
		}).asInstanceOf[RuntimeType[E]]


	/** Usage of type `E` as an unbound generic parameter in a fully specialized context.
	  * If `tpe` is the token class for one of the java primitives, the corresponding constant is used to represent
	  * the appropriate specialization. All reference types as well as custom value types are represented by an instance
	  * whose `runType` equals `AnyRef`; in that case, the actual information about the class of `E` is discarded.
	  * @return one of primitive specializations or an instance representing erasure to `AnyRef`.
	  */
	final def genericClass[E](tpe :Class[E]) :Specialized[E] = (
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
	).asInstanceOf[Specialized[E]]

	/** Equals to [[net.noresttherein.sugar.reflect.RuntimeType.genericClass genericClass]]`(clazz)`
	  * for the runtime class as defined by an implicit `ClassTag`.
	  */
	@inline final def generic[E](implicit tpe :ClassTag[E]) :Specialized[E] =
		genericClass(tpe.runtimeClass).asInstanceOf[Specialized[E]]

	/** Representation of any type as its auto boxed, erased form without any specialization or upper type bounds.
	  * @return a singleton instance, with all type members are defined as `AnyRef`,
	  *         and [[net.noresttherein.sugar.reflect.RuntimeType.runType runType]],
	  *         [[net.noresttherein.sugar.reflect.RuntimeType.genericType genericType]],
	  *         [[net.noresttherein.sugar.reflect.RuntimeType.boxType boxType]] all equal `classOf[AnyRef]`.
	  */
	@inline final def erased[E] :Specialized[E] = Erased.asInstanceOf[Specialized[E]]

	/** Yields the representation of type `E` in the caller's context after erasure and specialization. */
	final def specialized[@specialized E] :Specialized[E] = {
		new Enforce[E] match {
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
			case _ => Erased
		}
	}.asInstanceOf[Specialized[E]]




	/** An empty array guaranteed to hold values of `E`, with most specific element type based on the information
	  * about `E` in the caller's context.
	  * @return an array which component type is some super class of `E`.
	  */
	@inline final def arrayOf[E](implicit specialized :RuntimeType[E]) :Array[E] =
		specialized.emptyArray.asInstanceOf[Array[E]]

	/** A new array of the given size, guaranteed to hold values of `E`, with most specific element type based on the
	  * information about `E` in the caller's context. Note that this method can cause a breach in type safety,
	  * as the returned object is actually of type `Array[S forSome { type S >: E }]`. Thus it is possible,
	  * by additional casting, to store in the array an element which is not of class `E` without throwing an exception,
	  * likely resulting in `ClassCastException` at some later time when the element is accessed.
	  * @return an array which component type is some super class of `E` as defined by the implicit `RuntimeType`
	  *         type class.
	  */
	@inline final def arrayOf[E](capacity :Int)(implicit specialized :RuntimeType[E]) :Array[E] =
		specialized.newArray(capacity).asInstanceOf[Array[E]]


	/** Creates an empty array guaranteed to be able to hold values of type `E`, as it would appear in erased
	  * and specialized byte code. For inbuilt, specialized (by the implicit parameter) value classes a corresponding
	  * java primitive array is returned. For `AnyRef` subtypes, the actual class of the created array
	  * will be `Object[]`. The downcast required to present it as `Array[E]` is erased, so any `ClassCastException`s
	  * will be delayed until the client code attempts to enforce its type to an actual concrete class.
	  * Note that it is still perfectly safe to call it if the array doesn't escape the context
	  * in which `E` is an erased type, or if `E` is a primitive.
	  * @param specialized specialization information about type `E`.
	  */
	@inline final def genericArrayOf[E](implicit specialized :RuntimeType[E]) :Array[E] =
		specialized.emptyGenericArray.asInstanceOf[Array[E]]


	/** Creates an array of the given size, guaranteed to be able to hold values of type `E`, as it would appear
	  * in erased and specialized byte code. For inbuilt, specialized (by the implicit parameter) value classes
	  * a corresponding java primitive array is returned. For `AnyRef` subtypes, the actual class of the created array
	  * will be `[Object`. The downcast required to present it as `Array[E]` is erased, so any `ClassCastException`s
	  * will be delayed until the client code attempts to enforce its type to an actual concrete class.
	  * Note that it is still perfectly safe to call it if the array doesn't escape the context
	  * in which `E` is an erased type, or if `E` is a primitive.
	  * @param specialized specialization information about type `E`
	  */
	@inline final def genericArrayOf[E](capacity :Int)(implicit specialized :RuntimeType[E]) :Array[E] =
		specialized.newGenericArray(capacity).asInstanceOf[Array[E]]




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
	@inline implicit final def runtimeClass[E](implicit tpe :ClassTag[E]) :RuntimeType[E] = of[E]

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
		private def readResolve = RuntimeType.OfAnyRef
		override def toString = "[AnyRef]"
	}

	/** Explicit specialization for `Any` - this is equivalent to `OfAnyRef`. While both these values and
	  * [[net.noresttherein.sugar.reflect.RuntimeType.erased erased]]`[T]` describe erased context, the latter
	  * does not equal the former. This is because for this value (and `OfAnyRef`) the type parameter is well defined,
	  * while for the erased instance all information is lost.
	  */
	implicit val OfAny :RuntimeType[Any] = OfAnyRef.asInstanceOf[RuntimeType[Any]]

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

		override def default = throw new NoSuchElementException("Specialized[Nothing].default")

		override val runType = classOf[Nothing]
		override val classTag = ClassTag(classOf[Nothing])
		override val emptyArray :Array[Nothing] = new Array[Nothing](0)
		override val boxType = classOf[scala.runtime.Nothing$]
		override val genericType = classOf[Any]
		protected[reflect] override val discriminator = new Enforce[Nothing]

		protected[reflect] override def call[R[X]](callback :SpecializeIndividually[R])(implicit force :Enforce[Nothing]) =
			callback.forNothing

		private def readResolve = RuntimeType.OfNothing
		override def toString = "[Nothing]"
	}

	/** Represents an erased generic type argument referenced to as `java.lang.Object` and downcast in the point of use. */
	private[this] final val Erased :Specialized[Any] = Specialized.Erasure

	private val ValueTypes :collection.mutable.Map[Type, RuntimeType[_]] = TrieMap[Type, RuntimeType[_]]()



	/** `Specialized` instances representing all possible method/class specialization (all primitives and erasure).
	  * `Specializations(Specialized[T])` is true for all concrete and abstract types `T`.
 	  */
	final val Specializations = Set[Specialized[_]](
		OfByte, OfShort, OfInt, OfLong, OfChar, OfFloat, OfDouble, OfBoolean, OfUnit, Erased
	)

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



	/** Base trait describing context where type `T` is used as a generic type argument subject to possible erasure and
	  * specialization. There will be an implicit value of `Specialized[T]` for any type `T`, representing
	  * that upper bound, if no more specific type information is available. This in particular includes all built in
	  * value types which erase/specialize to themselves, but also for example an `Specialized[String]`
	  * denoting erased usage of `String`, while `String` is not a 'root type'.
	  *
	  * In other words, the runtime type and erased type are the same for all instances of `Specialized`.
	  * It thus allows to retain the same information about type `T` as scala `@specialized` annotation and can be used
	  * as a type class instead of specializing code which doesn't reference values of `T` directly but needs to call
	  * specialized code nevertheless (which is possible via [[net.noresttherein.sugar.reflect.Specialize Specialize]]).
	  *
	  * There are only 10 distinct values of this class:
	  * one for every inbuilt value type, `Unit`, and one denoting erased usage of a reference type.
	  * They are defined as singleton objects in the companion object to this trait and can be used in
	  * exhaustive pattern matching.
	  */ //specialized to enforce specialization of the factory method
	sealed trait Specialized[@specialized T] extends RuntimeType[T] {
		override type GenericType = RunType

		override def genericType :Class[GenericType] = runType
		override def emptyGenericArray :Array[GenericType] = emptyArray
		override def erasedClassTag :ClassTag[GenericType] = classTag

		override def isErased = false

		protected[reflect] override val discriminator :Enforce[T]

		override def toString :String = "[@specialized(" + classTag + ")]"
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
		@inline implicit final def specializedAnnotation[@specialized T] :Specialized[T] = specialized[T]
	}


	private[reflect] sealed abstract class SpecializedFromType extends SpecializedFromAnnotation {
		/** Implicit value discovering information about type `T` based on implicit `TypeTag`. It is of second precedence
		  * to `ClassTag` as the latter provides all required information directly and using is faster.
		  */
		implicit final def specializedType[T](implicit tag :TypeTag[T]) :Specialized[T] = {
			val tpe = tag.tpe
			(
				if (tpe <:< AnyRefType) Specialized.Erasure
				else if (tpe <:< IntType) Specialized.ForInt
				else if (tpe <:< DoubleType) Specialized.ForDouble
				else if (tpe <:< LongType) Specialized.ForLong
				else if (tpe <:< ByteType) Specialized.ForByte
				else if (tpe <:< CharType) Specialized.ForChar
				else if (tpe <:< BooleanType) Specialized.ForBoolean
				else if (tpe <:< FloatType) Specialized.ForFloat
				else if (tpe <:< ShortType) Specialized.ForShort
				else if (tpe <:< UnitType) Specialized.ForUnit
				else Specialized.Erasure
			).asInstanceOf[Specialized[T]]
		}
	}


	/** Provides access to representations of types after erasure and specialization. */
	@SerialVersionUID(Ver)
	object Specialized extends SpecializedFromType {

		/** An argument for `scala.specialized` annotation specializing for all primitives, including `Unit/void`.
		  * This is equivalent to parameterless `@specialized`, but may be useful as a switch value.
		  */
		final val All :Specializable.Group[(Byte, Short, Int, Long, Char, Float, Double, Boolean, Unit)] = null
			//new Specializable.Group((Byte, Short, Int, Long, Char, Float, Double, Boolean, Unit))

		/** An argument for `scala.specialized` annotation specializing for all java primitives, excluding `Unit/void`. */
		final val Primitives :Specializable.Group[(Byte, Short, Int, Long, Char, Float, Double, Boolean)] = null

		/** An argument for the `@specialized` annotation specializing for all primitives except `Boolean` (and `Unit`). */
		final val MultiValue :Specializable.Group[(Byte, Short, Int, Long, Char, Float, Double)] = null

		/** An argument for `scala.specialized` annotation specializing for all numeric value classes. */
		final val Numbers :Specializable.Group[(Byte, Short, Int, Long, Float, Double)] = null

		/** Types for which `scala.Function0` (that is, lazy expressions) is specialized.
		  * This includes every primitive type.
		  */
		final val Fun0 = All

		/** Types `scala.Function1`s argument is specialized for. */
		final val Fun1 :Specializable.Group[(Int, Long, Float, Double)] = null

		/** Types `scala.Function1` result type is specialized for. */
		final val Fun1Res :Specializable.Group[(Int, Long, Float, Double, Boolean, Unit)] = Specializable.Return
		//new Specializable.Group(Unit, Boolean, Int, Float, Long, Double)

		/** Result types `scala.Function1` is specialized for with the exception of `Unit`. */
		final val Fun1Vals :Specializable.Group[(Boolean, Int, Float, Long, Double)] = null

		/** Types `scala.Function2`s arguments are specialized for. */
		final val Fun2 :Specializable.Group[(Int, Long, Double)] = null

		/** Types `scala.Function2` result type is specialized for - same as `Fun1Res`. */
		final val Fun2Res :Specializable.Group[(Unit, Boolean, Int, Float, Long, Double)] = null

		/** Result types `scala.Function2` is specialized for, except for `Unit` - same as `Fun1Vals`. */
		final val Fun2Vals :Specializable.Group[(Boolean, Int, Float, Long, Double)] = null

		/** Element types `scala.Tuple2` is specialized for. */
		final val Tuple2Elem :Specializable.Group[(Int, Long, Double, Char, Boolean)] = null


		/** Summons an implicit value for [[net.noresttherein.sugar.reflect.RuntimeType.Specialized Specialized]]`[T]`,
		  * representing the way it would be used in context of a generic call, after erasure or specialization.
		  * There will always be a value for every type, in the most generic scenario representing the complete erasure
		  * and boxing (for value types).
		  */
		@inline final def apply[T](implicit manifest :Specialized[T]) :Specialized[T] = manifest

		/** Determines the local specialization context at the point of calling.
		  * Same as [[net.noresttherein.sugar.reflect.RuntimeType.specialized RuntimeType.specialized]].
		  */
		@inline final def locally[@specialized T] :Specialized[T] = specialized[T]


		/** Default implicit value used for abstract types `T` based on passed `ClassTag`. Unlike the corresponding
		  * `RuntimeType` implicit, all reference types and value types without java primitive representation are
		  * collated to the same 'erased' instance.
		  */
		@inline implicit final def specializedClassTag[T :ClassTag] :Specialized[T] = RuntimeType.generic[T]

		/** Default implicit value for all reference types representing erasure. All returned values are equal regardless
		  * of type parameter `T`.
		  */
		@inline implicit final def specializedRef[T <: AnyRef] :Specialized[T] = Erasure.asInstanceOf[Specialized[T]]


		/** Specialization for `Byte`. */
		@SerialVersionUID(Ver)
		implicit object ForByte extends SpecializedPrimitive[Byte, j.Byte](new Enforce[Byte], 0) {
			protected[reflect] override def call[R[_]](callback: SpecializeIndividually[R])
			                                        (implicit force: Enforce[Byte]) =
				callback.forByte
		}

		/** Specialization for `Short`. */
		@SerialVersionUID(Ver)
		implicit object ForShort extends SpecializedPrimitive[Short, j.Short](new Enforce[Short], 0) {
			protected[reflect] override def call[R[_]](callback: SpecializeIndividually[R])
			                                        (implicit force: Enforce[Short]) =
				callback.forShort
		}

		/** Specialization for `Int`. */
		@SerialVersionUID(Ver)
		implicit object ForInt extends SpecializedPrimitive[Int, j.Integer](new Enforce[Int], 0) {
			protected[reflect] override def call[R[_]](callback: SpecializeIndividually[R])
			                                        (implicit force: Enforce[Int]) =
				callback.forInt
		}

		/** Specialization for `Long`. */
		@SerialVersionUID(Ver)
		implicit object ForLong extends SpecializedPrimitive[Long, j.Long](new Enforce[Long], 0) {
			protected[reflect] override def call[R[_]](callback: SpecializeIndividually[R])
			                                        (implicit force: Enforce[Long]) =
				callback.forLong
		}

		/** Specialization for `Char`. */
		@SerialVersionUID(Ver)
		implicit object ForChar extends SpecializedPrimitive[Char, j.Character](new Enforce[Char], 0) {
			protected[reflect] override def call[R[_]](callback: SpecializeIndividually[R])
			                                        (implicit force: Enforce[Char]) =
				callback.forChar
		}

		/** Specialization for `Float`. */
		@SerialVersionUID(Ver)
		implicit object ForFloat extends SpecializedPrimitive[Float, j.Float](new Enforce[Float], 0) {
			protected[reflect] override def call[R[_]](callback: SpecializeIndividually[R])
			                                        (implicit force: Enforce[Float]) =
				callback.forFloat
		}

		/** Specialization for `Double`. */
		@SerialVersionUID(Ver)
		implicit object ForDouble extends SpecializedPrimitive[Double, j.Double](new Enforce[Double], 0) {
			protected[reflect] override def call[R[_]](callback: SpecializeIndividually[R])
			                                        (implicit force: Enforce[Double]) =
				callback.forDouble
		}

		/** Specialization for `Boolean`. */
		@SerialVersionUID(Ver)
		implicit object ForBoolean extends SpecializedPrimitive[Boolean, j.Boolean](new Enforce[Boolean], false) {
			protected[reflect] override def call[R[_]](callback: SpecializeIndividually[R])
			                                        (implicit force: Enforce[Boolean]) =
				callback.forBoolean
		}

		/** Specialization for `Unit` as java `void`. */
		@SerialVersionUID(Ver)
		implicit object ForUnit extends SpecializedPrimitive[Unit, BoxedUnit](new Enforce[Unit], ()) { //todo - this is not really a primitive:
			protected[reflect] override def call[R[_]](callback: SpecializeIndividually[R])
			                                        (implicit force: Enforce[Unit]) =
				callback.forUnit
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

			protected[reflect] override def call[R[X]](callback :SpecializeIndividually[R])
			                                           (implicit force :Enforce[Any]) :R[Any] =
				callback.forRef(this)

			protected[reflect] override val discriminator = new Enforce[Any]

			override def toString = "[_]"
		}

	}




	/** Introduced because of scalac bug which caused the class initializer to reassign final value
	  * from the generic superclass.
	  */
	private[RuntimeType] sealed abstract class PrimitiveBugWorkaround[T, B <: AnyRef]
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
	sealed abstract class SpecializedPrimitive[@specialized T, B <: AnyRef]
			(protected[reflect] override final val discriminator :Enforce[T], override final val default :T)
			(implicit classTag :ClassTag[T])
		extends PrimitiveBugWorkaround[T, B] with Specialized[T]
	{
		override def toString :String = string
		private[this] val string = super.toString
	}



	/** A trait extended by `RuntimeType` instances retaining full type information about `T`, i.e.
	 * representing values of type `T` as class `classOf[T]` (including inbuilt value classes) in the bytecode.
	 * Note that this trait is ''not'' specialized itself, so accepting it as a parameter will not result in
	 * the specialization of the method.
	 */
	sealed trait ExactRuntimeType[T] extends RuntimeType[T] {
		type RunType = T
	}

	/** A trait extended by `Specialized` instances retaining full type information about `T`, i.e.
	 * representing one of the inbuilt value classes or `AnyRef` itself (but not other reference types).
	 */
	sealed trait SpecializedExact[T] extends Specialized[T] with ExactRuntimeType[T]

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

		protected[reflect] override def call[R[X]](callback: SpecializeIndividually[R])(implicit force: Enforce[T]) =
			callback.forRef[T](this)
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

		protected[reflect] override def call[R[X]](callback :SpecializeIndividually[R])
		                                           (implicit force :Enforce[T]) :R[T] =
			callback.forRef(this)

		protected[reflect] override def discriminator :Enforce[T] = new Enforce[T]
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
			implicit def implicitBoxing[@specialized(Fun1) V <: AnyVal, T]
			                           (implicit valueClass :ValueClass[V, T]) :V => T =
				valueClass.apply

			implicit def implicitUnboxing[@specialized(Fun1Vals) V <: AnyVal, T]
			                             (implicit valueClass :ValueClass[V, T]) :T => V =
				valueClass.unapply
		}

		private val EmptyAnyArray = new Array[Any](0)
	}




	/** A token generic class specialized on its type parameter used to enforce specialization of a method by adding
	  * it as an implicit parameter. Implicit value is available for any type argument,
	  * but ''is not specialized itself'', which makes it faster to obtain and easier to inline by the JVM
	  * than a `RuntimeType`. It therefore carries no actual information about specialization and the latter class
	  * (or [[net.noresttherein.sugar.reflect.RuntimeType.Specialized Specialized]]) should be used for that purpose.
	  *
	  * This class defines equality in terms of its runtime class, with two instances being equal '''iff''' `getClass`
	  * returns the same object for both of them. As scala specialization is done by introducing separate synthetic
	  * subclasses for all specialized type parameters, comparing a locally created instance with predefined constants
	  * for all java primitives lets one discover if executed code is specialized and for which value class.
	  */
	sealed class Enforce[@specialized X] private[RuntimeType]() {
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


	/** We have the discriminators redeclare here to avoid (two!) virtual calls to accessor methods in RuntimeType
	  * during specialization discovery.
	  */
	private[this] final val IntKey     = OfInt.discriminator //new Enforce[Int]
	private[this] final val LongKey    = OfLong.discriminator //new Enforce[Long]
	private[this] final val ShortKey   = OfShort.discriminator //new Enforce[Short]
	private[this] final val ByteKey    = OfByte.discriminator //new Enforce[Byte]
	private[this] final val DoubleKey  = OfDouble.discriminator //new Enforce[Double]
	private[this] final val FloatKey   = OfFloat.discriminator //new Enforce[Float]
	private[this] final val CharKey    = OfChar.discriminator //new Enforce[Char]
	private[this] final val BooleanKey = OfBoolean.discriminator //new Enforce[Boolean]
	private[this] final val UnitKey    = OfUnit.discriminator //new Enforce[Unit]
	private[this] final val ErasedKey  = Erased.discriminator //new Enforce[Any]
}
