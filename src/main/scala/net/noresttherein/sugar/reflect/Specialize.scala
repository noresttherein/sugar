package net.noresttherein.sugar.reflect

import net.noresttherein.sugar.exceptions.unsupported_!
import net.noresttherein.sugar.typist.kinds.{Fixed, Self}




/** Generic specialized callback, allowing to call a specialized method from non-specialized code based on passed
  * implicit specialization information. This is the parameterless version -
  * [[net.noresttherein.sugar.reflect.Specialize.WithArg WithArg]] is a similar dispatcher accepting a single parameter
  * of a type related to type argument of the created object.
  *
  * An unspecialized call to [[net.noresttherein.sugar.reflect.Specialize.apply apply]]`[X]()`
  * when an implicit `Specialized[X]` is present will be delegated to the proper `@specialized` variant
  * of method [[net.noresttherein.sugar.reflect.Specialize.specialized specialized]]`[X]`.
  *
  * This is a parameterless callback - if you need to pass an argument,
  * see [[net.noresttherein.sugar.reflect.Specialize.WithArg WithArg]] and
  * [[net.noresttherein.sugar.reflect.Specialize.With2Args With2Args]],
  * which offer parallel type hierarchies to this trait.
  * @tparam R a type constructor for generic types, either specialized themselves, or requiring a specialized constructor.
  * @see [[net.noresttherein.sugar.reflect.Specialize.Specifically]]
  * @author Marcin Mościcki
  */
trait Specialize[+R[_]] {
	/** Double dispatch execution of [[net.noresttherein.sugar.reflect.Specialize.specialized specialized]] on `this`.
	  * Retrieves implicit specialization information, whatever is available, and invokes the appropriate
	  * specialized variant of [[net.noresttherein.sugar.reflect.Specialize.specialized specialized]].
	  * @param specialization information about runtime specialization requested for this call.
	  * @return result of calling the most appropriately specialized variant of `this.specialized[E]`.
	  */
	def apply[E]()(implicit specialization :RuntimeType[E]) :R[E] =
		specialization.call(this)

	/** Callback specialized method to be implemented by subclasses.
	  * Invoked as a result of calling `this()`, and - as long as any specialization information was present
	  * at that point, either explicitly (specialized code) or implicitly (for example `ClassTag[E]`), an appropriate
	  * specialized variant of this method will be invoked.
	  * @tparam E original type parameter as defined in runtime by the call to
	  *           [[net.noresttherein.sugar.reflect.Specialize#apply]].
	  */ //It is public to eliminate the need of an additional delegate call to a package private forwarder method.
	def specialized[@specialized E :RuntimeType] :R[E]
}




/** A companion to `Specialize[R[_]]` callback containing various base traits for its implementations,
  * special cases, and their parameterized variants.
  * @see [[net.noresttherein.sugar.reflect.Specialize$.WithArg WithArg]]
  * @see [[net.noresttherein.sugar.reflect.Specialize$.With2Args With2Args]]
  */
@SerialVersionUID(Ver)
object Specialize {

	/** Generic callback invoking specialized single parameter method from non-specialized one based on passed implicit
	  * specialization information. For the common cases where `P[X] = X` (the parameter type is the type parameter
	  * of the result), or `P[X] = Y` (an unrelated parameter type), see aliases
	  * [[net.noresttherein.sugar.reflect.Specialize$.Lift Lift]]`[R[_]]` and
	  * [[net.noresttherein.sugar.reflect.Specialize$.WithValue WithValue]]`[P, R[_]]`.
	  * @note Every sub-trait and subclass ''S'' of parameterless [[net.noresttherein.sugar.reflect.Specialize Specialize]]
	  *       defined here has an analogue trait/class of this trait in its companion object named ''S''`.WithArg`.
	  * @see [[net.noresttherein.sugar.reflect.Specialize.Specifically.WithArg]]
	  * @tparam P type constructor for the parameter given to the callback.
	  * @tparam R type constructor for the returned value, needing code specialized for some parameter type `E` to compute.
	  */ //Consider: reversing the parameter order would make infix syntax more intuitive: R Specialize.WithArg P.
	trait WithArg[-P[_], +R[_]] {
		/** Retrieves implicit specialization information available at the calling point and invokes the appropriate
		  * specialized version of this instance's `specialized` method with the same argument.
		  * @param param          the parameter given
		  *                       to [[net.noresttherein.sugar.reflect.Specialize.WithArg.specialized specialized]].
		  * @param specialization the requested specialization.
		  * @tparam E the type on which the call is specialized.
		  * @return the result of calling
		  *         [[net.noresttherein.sugar.reflect.Specialize.WithArg.specialized specialized]]`(param)`.
		  */
		def apply[E](param :P[E])(implicit specialization :RuntimeType[E]) :R[E] =
			specialization.call(this)(param)

		/** A specialized callback invoked from [[WithArg#apply]] based on the requested specialization type. */
		def specialized[@specialized E :RuntimeType](param :P[E]) :R[E]
	}

	/** A special case of `Specialize.WithArg` for situations where the argument is the specialized type itself.
	  * Note that the argument is boxed, as the whole purpose of this trait is to pass an argument
	  * from unspecialized context. If you want to just delegate `apply` to manually specialized methods
	  * use [[net.noresttherein.sugar.reflect.Specialize.Specifically.Lift Specifically.Lift]].
	  */
	type Lift[+R[_]] = WithArg[Self, R]

	/** Generic callback invoking a specialized method accepting parameter of type `P` and returning a value of `R[X]`
	  * specialized for type `X` specified at call site. This is simply a syntactic wrapper
	  * over [[net.noresttherein.sugar.reflect.Specialize.WithArg WithArg]].
	  * @tparam R result type constructor
	  * @tparam P parameter type
	  */
	type WithValue[-P, +R[_]] = WithArg[Fixed[P]#T, R]

	/** Generic callback invoking specialized two-parameter method from non-specialized one based on passed implicit
	  * specialization information.
	  * @tparam P1 type constructor for the first parameter given to the callback.
	  * @tparam P2 type constructor for the second parameter given to the callback
	  * @tparam R  type constructor for the returned value, needing code specialized for some parameter type `E` to compute.
	  */
	trait With2Args[-P1[_], -P2[_], +R[_]] {
		/** Retrieves implicit specialization information available at the calling point and invokes the appropriate
		  * specialized version of this instance's `specialized` method with the same argument.
		  * @param param1         the first parameter given
		  *                       to [[net.noresttherein.sugar.reflect.Specialize.With2Args.specialized specialized]].
		  * @param param2         the second parameter given
		  *                       to [[net.noresttherein.sugar.reflect.Specialize.With2Args.specialized specialized]].
		  * @param specialization requested specialization
		  * @tparam E the type on which the call is specialized
		  * @return the result of calling
		  *         [[net.noresttherein.sugar.reflect.Specialize.WithArg.specialized specialized]]`(param1, param2)`.
		  */
		def apply[E](param1 :P1[E], param2 :P2[E])(implicit specialization :RuntimeType[E]) :R[E] =
			specialization.call(this)(param1, param2)

		/** Specialized callback invoked from this instance's `apply` method based on the requested specialization type. */
		def specialized[@specialized E :RuntimeType](param1 :P1[E], param2 :P2[E]) :R[E]
	}



	/** A double-dispatch callback invoking different, manually specialized methods (not just synthetic specialized
	  * variant of the same method) based on specialization information for some type `E` given at call site.
	  * Similarly to [[net.noresttherein.sugar.reflect.Specialize Specialize]], it allows to call a specialized method
	  * from non-specialized one, but delegates to the new ''forE'' method for type `E` specified at call site.
	  * If you wish to specialize only for a few types, consider extending
	  * [[net.noresttherein.sugar.reflect.Specialize.SpecializeSome SpecializeSome]] instead, which serves
	  * as the default abstract base class, delegating all individual calls back to the single default abstract method.
	  * @tparam R type constructor for the returned value.
	  * @see [[net.noresttherein.sugar.reflect.Specialize.ForVals]]
	  * @see [[net.noresttherein.sugar.reflect.Specialize.SpecializeSome SpecializeSome]]
	  */
	trait Specifically[+R[_]] extends Specialize[R] {
		/** Call the appropriate, manually specialized ''forE'' method for type `E`, and return its result.
		  * @param specialization implicit specialization information for type `E`
		  * @tparam E the type specialized for.
		  */
		override def apply[E]()(implicit specialization :RuntimeType[E]) :R[E] =
			specialization.call(this)

		/** Invoked from `this[E]()` if `E` is specified to be `Byte` by the implicit argument
		  * to [[net.noresttherein.sugar.reflect.Specialize.Specifically.apply apply]].
		  */
		def forByte :R[Byte]

		/** Invoked from `this[E]()` if `E` is specified to be `Short` by the implicit argument
		  * to [[net.noresttherein.sugar.reflect.Specialize.Specifically.apply apply]].
		  */
		def forShort :R[Short]

		/** Invoked from `this[E]()` if `E` is specified to be `Char` by the implicit argument
		  * to [[net.noresttherein.sugar.reflect.Specialize.Specifically.apply apply]].
		  */
		def forChar :R[Char]

		/** Invoked from `this[E]()` if `E` is specified to be `Int` by the implicit argument
		  * to [[net.noresttherein.sugar.reflect.Specialize.Specifically.apply apply]].
		  */
		def forInt :R[Int]

		/** Invoked from `this[E]()` if `E` is specified to be `Long` by the implicit argument
		  * to [[net.noresttherein.sugar.reflect.Specialize.Specifically.apply apply]].
		  */
		def forLong :R[Long]

		/** Invoked from `this[E]()` if `E` is specified to be `Float` by the implicit argument
		  * to [[net.noresttherein.sugar.reflect.Specialize.Specifically.apply apply]].
		  */
		def forFloat :R[Float]

		/** Invoked from `this[E]()` if `E` is specified to be `Double` by the implicit argument
		  * to [[net.noresttherein.sugar.reflect.Specialize.Specifically.apply apply]]. */
		def forDouble :R[Double]

		/** Invoked from `this[E]()` if `E` is specified to be `Boolean` by the implicit argument
		  * to [[net.noresttherein.sugar.reflect.Specialize.Specifically.apply apply]]. */
		def forBoolean :R[Boolean]

		/** Invoked from `this[E]()` if `E` is specified to be `Unit` by the implicit argument
		  * to [[net.noresttherein.sugar.reflect.Specialize.Specifically.apply apply]]. */
		def forUnit :R[Unit]

		/** Invoked from `this[E]()` if `E` is specified to be `Nothing` by the implicit argument
		  * to [[net.noresttherein.sugar.reflect.Specialize.Specifically.apply apply]]. */
		def forNothing :R[Nothing]

		/** Invoked from `this[E]()` if `E` is either a reference type or is erased and boxed at the point of calling.
		  * Implicit argument gives all available information about type `E`.
		  * In some cases, this method may be also called if `E` was only promoted to `AnyRef` by the Scala runtime
		  * in a manner transparent to `RuntimeType`.
		  */
		def forRef[E <: AnyRef :RuntimeType] :R[E]

		/** Invoked from `this[E]()` in case `!RuntimeType[T].runType.isPrimitive`.
		  * Typically [[net.noresttherein.sugar.reflect.Specialize.Specifically.forRef forRef]],
		  * [[net.noresttherein.sugar.reflect.Specialize.Specifically.forUnit forUnit]],
		  * and [[net.noresttherein.sugar.reflect.Specialize.Specifically.forNothing forNothing]] all delegate here,
		  * but it may also be called directly if `E` is erased, is a value class, and in some other corner cases
		  * when a type is not formally `AnyRef`, but is represented by a Java object.
		  */ //Not named forAny as we might want to declare a constant val forAny :R[Any].
		def forOthers[E :RuntimeType] :R[E]

		/** This method is no longer invoked as a specialized callback; instead, the appropriate method for the
		  * specialized type is called directly. However, subclasses may choose to use it as the default, generic
		  * implementation for the manually specialized callback methods. In that case, prefer extending
		  * [[net.noresttherein.sugar.reflect.Specialize.SpecializeSome SpecializeSome]] instead.
		  * @throws UnsupportedOperationException unless overridden.
		  */
		override def specialized[@specialized E :RuntimeType] :R[E] =
			unsupported_!("(" + this + " :Specifically).specialized" + RuntimeType[E])
	}


	@SerialVersionUID(Ver)
	object Specifically {
		/** An analogue of [[net.noresttherein.sugar.reflect.Specialize.Specifically! Specifically]]
		  * to the parameterized variant [[net.noresttherein.sugar.reflect.Specialize.WithArg Specialize.WithArg]]
		  * of [[net.noresttherein.sugar.reflect.Specialize Specialize]].
		  * Differs from `Specialize.WithArg` in that it is manually specialized, instead of relying on the Scala 2
		  * specialization. Method `apply[E](param)` no longer delegates directly to a single
		  * `@specialized` [[net.noresttherein.sugar.reflect.Specialize.WithArg.specialized method]],
		  * but to method ''forE'', specific to the type argument of the return type, as defined by the given parameter.
		  * @see [[net.noresttherein.sugar.reflect.Specialize.SpecializeSome.WithArg SpecializeSome.WithArg]]
		  * @tparam P type constructor of the parameter given to `apply`.
		  * @tparam R type constructor of the return type, parameterized with the same type as parameter type `P`.
		  */
		trait WithArg[-P[_], +R[_]] extends Specialize.WithArg[P, R] {
			/** Call the appropriate, manually specialized ''forE''`(param)` method for type `E`, and return its result.
			  * @param specialization implicit specialization information for type `E`
			  * @tparam E the type specialized for.
			  */
			override def apply[E](param :P[E])(implicit specialization :RuntimeType[E]) :R[E] =
				specialization.call(this)(param) //Calls a more specific overloaded variant of inherited one.

			/** Invoked from `this[E](param)` if `E` is specified to be `Byte` by the implicit argument
			  * to [[net.noresttherein.sugar.reflect.Specialize.WithArg.apply apply]].
			  */
			def forByte(param :P[Byte]) :R[Byte]

			/** Invoked from `this[E](param)` if `E` is specified to be `Short` by the implicit argument
			  * to [[net.noresttherein.sugar.reflect.Specialize.WithArg.apply apply]].
			  */
			def forShort(param :P[Short]) :R[Short]

			/** Invoked from `this[E](param)` if `E` is specified to be `Char` by the implicit argument
			  * to [[net.noresttherein.sugar.reflect.Specialize.WithArg.apply apply]].
			  */
			def forChar(param :P[Char]) :R[Char]

			/** Invoked from `this[E](param)` if `E` is specified to be `Int` by the implicit argument
			  * to [[net.noresttherein.sugar.reflect.Specialize.WithArg.apply apply]].
			  */
			def forInt(param :P[Int]) :R[Int]

			/** Invoked from `this[E](param)` if `E` is specified to be `Long` by the implicit argument
			  * to [[net.noresttherein.sugar.reflect.Specialize.WithArg.apply apply]].
			  */
			def forLong(param :P[Long]) :R[Long]

			/** Invoked from `this[E](param)` if `E` is specified to be `Float` by the implicit argument
			  * to [[net.noresttherein.sugar.reflect.Specialize.WithArg.apply apply]].
			  */
			def forFloat(param :P[Float]) :R[Float]

			/** Invoked from `this[E](param)` if `E` is specified to be `Double` by the implicit argument
			  * to [[net.noresttherein.sugar.reflect.Specialize.WithArg.apply apply]].
			  */
			def forDouble(param :P[Double]) :R[Double]

			/** Invoked from `this[E](param)` if `E` is specified to be `Boolean` by the implicit argument
			  * to [[net.noresttherein.sugar.reflect.Specialize.WithArg.apply apply]].
			  */
			def forBoolean(param :P[Boolean]) :R[Boolean]

			/** Invoked from `this[E](param)` if `E` is specified to be `Unit` by the implicit argument
			  * to [[net.noresttherein.sugar.reflect.Specialize.WithArg.apply apply]].
			  */
			def forUnit(param :P[Unit]) :R[Unit]
			
			/** Invoked from `this[E](param)` if `E` is specified to be `Nothing` by the implicit argument
			  * to [[net.noresttherein.sugar.reflect.Specialize.WithArg.apply apply]].
			  */
			def forNothing(param :P[Nothing]) :R[Nothing]
			
			/** Invoked from `this[E](param)` if `E` is specified to be an `AnyRef` subtype by the implicit argument
			  * to [[net.noresttherein.sugar.reflect.Specialize.WithArg.apply apply]].
			  * In some cases, this method may be also called if `E` was only promoted to `AnyRef` by the Scala runtime
			  * in a manner transparent to `RuntimeType`.
			  */
			def forRef[E <: AnyRef :RuntimeType](param :P[E]) :R[E]

			/** Invoked from `this[E](param)` if `E` is represented by a Java object, but is not necessarily
			  * a subtype of `AnyRef` (for example, in an erased context).
			  */ //Not forAny for consistency with Specifically[R], see the comment there.
			def forOthers[E :RuntimeType](param :P[E]) :R[E]

			/** This method is no longer invoked as a specialized callback; instead, the appropriate method for the
			  * specialized type is called directly. However, subclasses may choose to use it as the default, generic
			  * implementation for the manually specialized callback methods. In that case, prefer extending
			  * [[net.noresttherein.sugar.reflect.Specialize.SpecializeSome.Lift SpecializeSome.Lift]] instead.
			  * @throws UnsupportedOperationException unless overridden.
			  */
			override def specialized[@specialized E :RuntimeType](param :P[E]) :R[E] =
				unsupported_!("(" + this + " :Specifically.WithArg).specialized" + RuntimeType[E])
		}

		/** A base trait for `Specifically.WithArg` (and, by extension, `Specialized.WithArg`) accepting
		  * an argument of the actual type parameter to result type `R`. Boxing of the argument may be avoided,
		  * but only if [[net.noresttherein.sugar.reflect.Specialize.Specifically.Lift.apply apply]]`(param :E)`
		  * is called in a `@specialized` context, as calling the inherited generic `apply(param :P[E])` will box
		  * the argument straight away.
		  * @see [[net.noresttherein.sugar.reflect.Specialize.SpecializeSome.Lift SpecializeSome.Lift]]
		  * @tparam R type constructor of the return type, parameterized with the same type as parameter type `P`.
		  */
		trait Lift[+R[_]] extends WithArg[Self, R] {
			//Overrides to introduce specialization for the argument, i.e. variants specialized(param :Int) :R[Int], etc.
			/** Retrieves implicit specialization information available at the calling point and invokes the appropriate
			  * specialized version of this instance's `specialized` method with the same argument.
			  * @param param          the parameter given
			  *                       to [[net.noresttherein.sugar.reflect.Specialize.WithArg.specialized specialized]].
			  * @param specialization the requested specialization.
			  * @tparam E the type on which the call is specialized.
			  * @return the result of calling
			  *         [[net.noresttherein.sugar.reflect.Specialize.WithArg.specialized specialized]]`(param)`.
			  */
			override def apply[@specialized E](param :E)(implicit specialization :RuntimeType[E]) :R[E] =
				specialization.call(this)(param)

			override def forByte(param :Byte)       :R[Byte]
			override def forShort(param :Short)     :R[Short]
			override def forChar(param :Char)       :R[Char]
			override def forInt(param :Int)         :R[Int]
			override def forLong(param :Long)       :R[Long]
			override def forFloat(param :Float)     :R[Float]
			override def forDouble(param :Double)   :R[Double]
			override def forBoolean(param :Boolean) :R[Boolean]

			override def specialized[@specialized E :RuntimeType](param :E) :R[E] = super[WithArg].specialized[E](param)
		}


		/** An analogue of [[net.noresttherein.sugar.reflect.Specialize.Specifically! Specifically]]
		  * to the two parameter variant [[net.noresttherein.sugar.reflect.Specialize.With2Args With2Args]]
		  * of [[net.noresttherein.sugar.reflect.Specialize Specialize]].
		  * Differs from `Specialize.WithArg` in that it is manually specialized, instead of relying on the Scala 2
		  * specialization. Method `apply[E](param1, param2)` no longer delegates directly to a single
		  * `@specialized` [[net.noresttherein.sugar.reflect.Specialize.With2Args.specialized method]],
		  * but to method ''forE(param1, param2)'', specific to the type argument of the return type,
		  * as defined by the given parameters.
		  * @see [[net.noresttherein.sugar.reflect.Specialize.SpecializeSome.With2Args SpecializeSome.With2Args]]
		  * @tparam P1 type constructor of the first parameter given to `apply`.
		  * @tparam P2 type constructor of the second parameter given to `apply`.
		  * @tparam R  type constructor of the return type, parameterized with the same type as parameter type `P`.
		  */
		trait With2Args[-P1[_], -P2[_], +R[_]] extends Specialize.With2Args[P1, P2, R] {
			/** Call the appropriate, manually specialized ''forE''`(param1, param2)` method for type `E`,
			  * and return its result. By default, all these methods simply forward
			  * to [[net.noresttherein.sugar.reflect.Specialize.Specifically.specialized specialized]].
			  * If `E` is not a primitive type or no specialization information is available/a generic implicit value
			  * is provided it instead delegates directly
			  * to [[net.noresttherein.sugar.reflect.Specialize.Specifically.specialized specialized]].
			  * @param specialization implicit specialization information for type `E`
			  * @tparam E the type specialized for.
			  */
			override def apply[E](param1 :P1[E], param2 :P2[E])(implicit specialization :RuntimeType[E]) :R[E] =
				specialization.call(this)(param1, param2) //Calls a more specific overloaded variant of inherited one.
			
			/** Invoked from `this[E](param1, param2)` if `E` is specified to be `Byte` by the implicit argument
			  * to [[net.noresttherein.sugar.reflect.Specialize.With2Args.apply apply]].
			  */
			def forByte(param1 :P1[Byte], param2 :P2[Byte]) :R[Byte]
			
			/** Invoked from `this[E](param1, param2)` if `E` is specified to be `Short` by the implicit argument
			  * to [[net.noresttherein.sugar.reflect.Specialize.With2Args.apply apply]].
			  */
			def forShort(param1 :P1[Short], param2 :P2[Short]) :R[Short]
			
			/** Invoked from `this[E](param1, param2)` if `E` is specified to be `Char` by the implicit argument
			  * to [[net.noresttherein.sugar.reflect.Specialize.With2Args.apply apply]].
			  */
			def forChar(param1 :P1[Char], param2 :P2[Char]) :R[Char]
			
			/** Invoked from `this[E](param1, param2)` if `E` is specified to be `Int` by the implicit argument
			  * to [[net.noresttherein.sugar.reflect.Specialize.With2Args.apply apply]].
			  */
			def forInt(param1 :P1[Int], param2 :P2[Int]) :R[Int]
			
			/** Invoked from `this[E](param1, param2)` if `E` is specified to be `Long` by the implicit argument
			  * to [[net.noresttherein.sugar.reflect.Specialize.With2Args.apply apply]].
			  */
			def forLong(param1 :P1[Long], param2 :P2[Long]) :R[Long]
			
			/** Invoked from `this[E](param1, param2)` if `E` is specified to be `Float` by the implicit argument
			  * to [[net.noresttherein.sugar.reflect.Specialize.With2Args.apply apply]].
			  */
			def forFloat(param1 :P1[Float], param2 :P2[Float]) :R[Float]
			
			/** Invoked from `this[E](param1, param2)` if `E` is specified to be `Double` by the implicit argument
			  * to [[net.noresttherein.sugar.reflect.Specialize.With2Args.apply apply]].
			  */
			def forDouble(param1 :P1[Double], param2 :P2[Double]) :R[Double]
			
			/** Invoked from `this[E](param1, param2)` if `E` is specified to be `Boolean` by the implicit argument
			  * to [[net.noresttherein.sugar.reflect.Specialize.With2Args.apply apply]].
			  */
			def forBoolean(param1 :P1[Boolean], param2 :P2[Boolean]) :R[Boolean]
			
			/** Invoked from `this[E](param1, param2)` if `E` is specified to be `Unit` by the implicit argument
			  * to [[net.noresttherein.sugar.reflect.Specialize.With2Args.apply apply]].
			  */
			def forUnit(param1 :P1[Unit], param2 :P2[Unit]) :R[Unit]
			
			/** Invoked from `this[E](param1, param2)` if `E` is specified to be `Nothing` by the implicit argument
			  * to [[net.noresttherein.sugar.reflect.Specialize.With2Args.apply apply]].
			  */
			def forNothing(param1 :P1[Nothing], param2 :P2[Nothing]) :R[Nothing]

			/** Invoked from `this[E](param1, param2)` if `E` is a subtype of `AnyRef`.
			  * In some cases, this method may be also called if `E` was only promoted to `AnyRef` by the Scala runtime
			  * in a manner transparent to `RuntimeType`.
			  */
			def forRef[E <: AnyRef :RuntimeType](param1 :P1[E], param2 :P2[E]) :R[E]

			/** Invoked from `this[E](param1, param2)` if `E` is represented by a Java object, but is not necessarily
			  * a subtype of `AnyRef` (for example, in an erased context).
			  */ //Not forAny for consistency with Specifically[R], see the comment there.
			def forOthers[E :RuntimeType](param1 :P1[E], param2 :P2[E]) :R[E]

			override def specialized[@specialized E :RuntimeType](param1 :P1[E], param2 :P2[E]) :R[E] =
				unsupported_!("(" + this + " :Specifically.With2Args).specialized" + RuntimeType[E] + "(_, _)")
		}

		/** A base trait for `Specifically.With2Args` (and, by extension, `Specialized.With2Args`) accepting
		  * an arguments of the actual type parameter to result type `R`. Boxing of the arguments may be avoided, but
		  * only if [[net.noresttherein.sugar.reflect.Specialize.Specifically.Lift.apply apply]]`(param1 :E, param2 :E)`
		  * is called in a `@specialized` context, as calling the inherited generic `apply(param1 :P[E], param2 :P[E])`
		  * will box the argument straight away.
		  * @see [[net.noresttherein.sugar.reflect.Specialize.SpecializeSome.Lift2 SpecializeSome.Lift2]]
		  * @tparam R  type constructor of the return type, parameterized with the same type as parameter type `P`.
		  */
		trait Lift2[+R[_]] extends With2Args[Self, Self, R] {
			//Overrides to introduce specialization for the arguments, i.e. specialized(p1 :Int, p2 :Int) :R[Int], etc.
			override def apply[@specialized E](param1 :E, param2 :E)(implicit specialization :RuntimeType[E]) :R[E] =
				specialization.call(this)(param1, param2)

			override def forByte(param1 :Byte, param2 :Byte) :R[Byte]
			override def forShort(param1 :Short, param2 :Short) :R[Short]
			override def forChar(param1 :Char, param2 :Char) :R[Char]
			override def forInt(param1 :Int, param2 :Int) :R[Int]
			override def forLong(param1 :Long, param2 :Long) :R[Long]
			override def forFloat(param1 :Float, param2 :Float) :R[Float]
			override def forDouble(param1 :Double, param2 :Double) :R[Double]
			override def forBoolean(param1 :Boolean, param2 :Boolean) :R[Boolean]

			override def specialized[@specialized E :RuntimeType](param1 :E, param2 :E) :R[E] =
				super[With2Args].specialized(param1, param2)
		}
	}



	/** A `Specifically` mix-in trait delegating cases for `Unit`, `Nothing`, and `AnyRef` to `Any`. */
	trait ForVals[+R[_]] extends Specifically[R] {
		override def forUnit :R[Unit]       = forOthers
		override def forNothing :R[Nothing] = forOthers[Nothing]
		override def forRef[E <: AnyRef :RuntimeType] :R[E] = forOthers
	}


	@SerialVersionUID(Ver)
	object ForVals {
		/** A `Specifically.WithArg` mix-in trait delegating cases for `Unit`, `Nothing`, and `AnyRef` to `Any`. */
		trait WithArg[-P[_], +R[_]] extends Specifically.WithArg[P, R] {
			override def forUnit(param :P[Unit]) :R[Unit] = forOthers(param)
			override def forNothing(param :P[Nothing]) :R[Nothing] = forOthers[Nothing](param)
			override def forRef[E <: AnyRef :RuntimeType](param :P[E]) :R[E] = forOthers(param)
		}

		/** A `Specifically.With2Args` mix-in trait delegating cases for `Unit`, `Nothing`, and `AnyRef` to `Any`. */
		trait With2Args[-P1[_], -P2[_], +R[_]] extends Specifically.With2Args[P1, P2, R] {
			override def forRef[E <: AnyRef :RuntimeType](param1 :P1[E], param2 :P2[E]) :R[E] = forOthers(param1, param2)
			override def forUnit(param1 :P1[Unit], param2 :P2[Unit]) :R[Unit] = forOthers(param1, param2)
			override def forNothing(param1 :P1[Nothing], param2 :P2[Nothing]) :R[Nothing] =
				forOthers[Nothing](param1, param2)
		}
	}



	/** A convenience base class of
	  * [[net.noresttherein.sugar.reflect.Specialize.Specifically Specifically]],
	  * which delegates all methods specific to value types to single
	  * [[net.noresttherein.sugar.reflect.Specialize.SpecializeSome.specialized specialized]],
	  * allowing subclasses to provide distinct implementations only for a selected few types and a default
	  * common method for all others. In order to avoid the generation of unwanted variants
	  * of the `specialized` method in subclasses, the latter forwards in turn
	  * to non-specialized method [[net.noresttherein.sugar.reflect.Specialize.Specifically.forOthers forOthers]],
	  * as do `forNothing` and `forRef`, directly.
	  */
	abstract class SpecializeSome[+R[_]] extends Specifically[R] {
		/** Call the appropriate, manually specialized ''forE'' method for type `E`, and return its result.
		  * By default, all methods specific to value types simply forward
		  * to [[net.noresttherein.sugar.reflect.Specialize.Specifically.specialized specialized]].
		  * If `E` is not a primitive type or no specialization information is available/a generic implicit value
		  * is provided it instead delegates directly
		  * to [[net.noresttherein.sugar.reflect.Specialize.Specifically.forOthers forAny]].
		  * @param specialization implicit specialization information for type `E`
		  * @tparam E the type specialized for.
		  */ //Overridden to make final and eligible for compiler inlining
		@inline final override def apply[E]()(implicit specialization :RuntimeType[E]) :R[E] =
			specialization.call(this)

		override def forByte    :R[Byte]    = specialized
		override def forShort   :R[Short]   = specialized
		override def forChar    :R[Char]    = specialized
		override def forInt     :R[Int]     = specialized
		override def forLong    :R[Long]    = specialized
		override def forFloat   :R[Float]   = specialized
		override def forDouble  :R[Double]  = specialized
		override def forBoolean :R[Boolean] = specialized
		override def forUnit    :R[Unit]    = forOthers
		override def forNothing :R[Nothing] = forOthers[Nothing]
		override def forRef[E <: AnyRef :RuntimeType] :R[E] = forOthers[E]

		/** Default target for all `for`''T'' methods. Directly invokes non-specialized method `generic`. */
		override def specialized[@specialized E :RuntimeType] :R[E] = forOthers
	}


	@SerialVersionUID(Ver)
	object SpecializeSome {
		/** A convenience base class of
		  * [[net.noresttherein.sugar.reflect.Specialize.Specifically.WithArg Specifically.WithArg]],
		  * which delegates all methods specific to value types to single
		  * [[net.noresttherein.sugar.reflect.Specialize.SpecializeSome.WithArg.specialized specialized]],
		  * allowing subclasses to provide distinct implementations only for a selected few types and a default
		  * common method for all others. In order to avoid the generation of unwanted variants
		  * of the `specialized` method in subclasses, the latter forwards in turn to non-specialized method
		  * [[net.noresttherein.sugar.reflect.Specialize.SpecializeSome.WithArg.generic generic]],
		  * as do `forNothing` and `forRef`, directly.
		  */
		abstract class WithArg[-P[_], +R[_]] extends Specifically.WithArg[P, R] {
			override def forByte(param :P[Byte])                       :R[Byte] = specialized(param)
			override def forShort(param :P[Short])                     :R[Short] = specialized(param)
			override def forChar(param :P[Char])                       :R[Char] = specialized(param)
			override def forInt(param :P[Int])                         :R[Int] = specialized(param)
			override def forLong(param :P[Long])                       :R[Long] = specialized(param)
			override def forFloat(param :P[Float])                     :R[Float] = specialized(param)
			override def forDouble(param :P[Double])                   :R[Double] = specialized(param)
			override def forBoolean(param :P[Boolean])                 :R[Boolean] = specialized(param)
			override def forUnit(param :P[Unit])                       :R[Unit] = forOthers(param)
			override def forNothing(param :P[Nothing])                 :R[Nothing] = forOthers[Nothing](param)
			override def forRef[E <: AnyRef :RuntimeType](param :P[E]) :R[E] = forOthers(param)
		}

		/** A variant of [[net.noresttherein.sugar.reflect.Specialize.SpecializeSome.WithArg SpecializeSome.WithArg]]
		  * using the type parameter of the return type as the method argument.
		  */
		abstract class Lift[+R[_]] extends WithArg[Self, R] {
			/** Call the appropriate, manually specialized `forE` method for type `E`, and return its result.
			  * By default, all these methods simply forward
			  * to [[net.noresttherein.sugar.reflect.Specialize.Specifically.WithArg.specialized specialized]].
			  * If `E` is not a primitive type or no specialization information is available/a generic implicit value
			  * is provided it instead delegates directly
			  * to [[net.noresttherein.sugar.reflect.Specialize.Specifically.WithArg.specialized specialized]].
			  * @param specialization implicit specialization information for type `E`
			  * @tparam E the type specialized for.
			  */ //Overridden for specialization and to delegate to a different overloaded variant of call.
			@inline final override def apply[@specialized E](param :E)(implicit specialization :RuntimeType[E]) :R[E] =
				specialization.call(this)(param)

			//Overridden for specialization of the argument
			override def forByte(param :Byte) :R[Byte] = specialized(param)
			override def forShort(param :Short) :R[Short] = specialized(param)
			override def forChar(param :Char) :R[Char] = specialized(param)
			override def forInt(param :Int) :R[Int] = specialized(param)
			override def forLong(param :Long) :R[Long] = specialized(param)
			override def forFloat(param :Float) :R[Float] = specialized(param)
			override def forDouble(param :Double) :R[Double] = specialized(param)
			override def forBoolean(param :Boolean) :R[Boolean] = specialized(param)
		}


		/** A convenience base class of
		  * [[net.noresttherein.sugar.reflect.Specialize.Specifically.With2Args Specifically.With2Args]],
		  * which delegates all methods specific to value types to single
		  * [[net.noresttherein.sugar.reflect.Specialize.SpecializeSome.With2Args.specialized specialized]],
		  * allowing subclasses to provide distinct implementations only for a selected few types and a default
		  * common method for all others. In order to avoid the generation of unwanted variants
		  * of the `specialized` method in subclasses, the latter forwards in turn to non-specialized method
		  * [[net.noresttherein.sugar.reflect.Specialize.SpecializeSome.With2Args.generic generic]],
		  * as do `forNothing` and `forRef`, directly.
		  */
		abstract class With2Args[-P1[_], -P2[_], +R[_]] extends Specifically.With2Args[P1, P2, R] {
			override def forByte(param1 :P1[Byte], param2 :P2[Byte]) :R[Byte] = specialized(param1, param2)
			override def forShort(param1 :P1[Short], param2 :P2[Short]) :R[Short] = specialized(param1, param2)
			override def forChar(param1 :P1[Char], param2 :P2[Char]) :R[Char] = specialized(param1, param2)
			override def forInt(param1 :P1[Int], param2 :P2[Int]) :R[Int] = specialized(param1, param2)
			override def forLong(param1 :P1[Long], param2 :P2[Long]) :R[Long] = specialized(param1, param2)
			override def forFloat(param1 :P1[Float], param2 :P2[Float]) :R[Float] = specialized(param1, param2)
			override def forDouble(param1 :P1[Double], param2 :P2[Double]) :R[Double] = specialized(param1, param2)
			override def forBoolean(param1 :P1[Boolean], param2 :P2[Boolean]) :R[Boolean] = specialized(param1, param2)

			override def forUnit(param1 :P1[Unit], param2 :P2[Unit]) :R[Unit] = forOthers(param1, param2)
			override def forNothing(param1 :P1[Nothing], param2 :P2[Nothing]) :R[Nothing] = forOthers[Nothing](param1, param2)
			override def forRef[E <: AnyRef :RuntimeType](param1 :P1[E], param2 :P2[E]) :R[E] = forOthers(param1, param2)
		}

		/** A variant of [[net.noresttherein.sugar.reflect.Specialize.SpecializeSome.With2Args SpecializeSome.With2Args]]
		  * using the type parameter of the return type as the method arguments.
		  */
		abstract class Lift2[+R[_]] extends With2Args[Self, Self, R] {
			/** Call the appropriate, manually specialized `forE` method for type `E`, and return its result.
			  * By default, all these methods simply forward
			  * to [[net.noresttherein.sugar.reflect.Specialize.Specifically.With2Args.specialized specialized]].
			  * If `E` is not a primitive type or no specialization information is available/a generic implicit value
			  * is provided it instead delegates directly
			  * to [[net.noresttherein.sugar.reflect.Specialize.Specifically.With2Args.specialized specialized]].
			  * @param specialization implicit specialization information for type `E`
			  * @tparam E the type specialized for.
			  */ //Overridden for specialization and to delegate to a different overloaded variant of call.
			@inline final override def apply[E](param1 :E, param2 :E)(implicit specialization :RuntimeType[E]) :R[E] =
				specialization.call[Self, Self, R](this)(param1, param2)

			override def forByte(param1 :Byte, param2 :Byte) :R[Byte] = specialized(param1, param2)
			override def forShort(param1 :Short, param2 :Short) :R[Short] = specialized(param1, param2)
			override def forChar(param1 :Char, param2 :Char) :R[Char] = specialized(param1, param2)
			override def forInt(param1 :Int, param2 :Int) :R[Int] = specialized(param1, param2)
			override def forLong(param1 :Long, param2 :Long) :R[Long] = specialized(param1, param2)
			override def forFloat(param1 :Float, param2 :Float) :R[Float] = specialized(param1, param2)
			override def forDouble(param1 :Double, param2 :Double) :R[Double] = specialized(param1, param2)
			override def forBoolean(param1 :Boolean, param2 :Boolean) :R[Boolean] = specialized(param1, param2)
		}
	}


	/** A shorter type alias for `SpecializeSome` for those who prefer to use a longer, qualified name
	  * in order to better convey that a type is a subtype of [[net.noresttherein.sugar.reflect.Specialize Specialize]]:
	  * `Specialize.ForSome[R]`.
	  */
	type ForSome[+R[_]] = SpecializeSome[R]


	/** This is class is similar to [[net.noresttherein.sugar.reflect.Specialize.SpecializeSome SpecializeSome]]
	  * in behavior, but overrides all type-specific methods with constants initialized by a call to `specialized`.
	  */
	abstract class SpecializedVals[+R[_]] extends Specifically[R] {
		override final val forByte    = specialized[Byte]
		override final val forShort   = specialized[Short]
		override final val forInt     = specialized[Int]
		override final val forLong    = specialized[Long]
		override final val forFloat   = specialized[Float]
		override final val forDouble  = specialized[Double]
		override final val forChar    = specialized[Char]
		override final val forBoolean = specialized[Boolean]
		override final val forUnit    = forOthers[Unit]
		override final val forNothing = forOthers[Nothing]
		override def forRef[E <: AnyRef :RuntimeType] :R[E] = forOthers[E]
	}
//
//	/** A shorter type alias for `SpecializedVals` for those who prefer to use a longer, qualified name
//	  * in order to better convey that a type is a subtype of [[net.noresttherein.sugar.reflect.Specialize Specialize]]:
//	  * `Specialize.Vals[R]`.
//	  */
//	type ForVals[R[_]] = SpecializedVals[R]


//	private[reflect] def specialize[@specialized X :RuntimeType, R[_]](callback :Specialize[R]) :R[X] =
//		callback.specialized
}
