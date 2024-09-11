package net.noresttherein.sugar.reflect

import net.noresttherein.sugar.funny.Curry2
import net.noresttherein.sugar.reflect.Specialize.Specifically
import net.noresttherein.sugar.reflect.CrossSpecialize.SpecializeFirst




/** Similarly to how [[net.noresttherein.sugar.reflect.Specialize]] performs a callback to a specialized method
  * based on implicitly available `RuntimeType[T]`, this class executes a double callback to invoke a method
  * specialized for two types.
  * @tparam R a type constructor for the result type, accepting two specialized type parameters.
  * @see [[net.noresttherein.sugar.reflect.CrossSpecialize.StagedSpecialize]]
  * @author Marcin Mościcki marcin@moscicki.net
  */
abstract class CrossSpecialize[+R[X, Y]] extends Specialize[SpecializeFirst[R]#T] { pair =>

	/** Initiates the double callback using the implicitly passed runtime type information to pick the appropriate
	  * specialized variant of [[net.noresttherein.sugar.reflect.CrossSpecialize#specialize[X, Y]].
	  */
	def apply[X, Y]()(implicit xType :RuntimeType[X], yType :RuntimeType[Y]) :R[X, Y] = apply[X]().apply[Y]()

	/** Intermediate callback specialized for the first type parameter, which returns the callback
	  * for the specialization of the second parameter.
	  */
	override def specialized[@specialized X :RuntimeType] :Specialize[Curry2[R]#A1[X]#A2] =
		new SpecializeSecond[X]

	/** Specialized callback method invoked as the result of the call to `apply()`,
	  * responsible for producing the final result value.
	  */
	def specialized[@specialized X :RuntimeType, @specialized Y :RuntimeType] :R[X, Y]

	private[CrossSpecialize] class SpecializeSecond[@specialized T :RuntimeType]
		extends Specialize[Curry2[R]#A1[T]#A2]
	{
		override def specialized[@specialized Y :RuntimeType] :R[T, Y] = pair.specialized[T, Y]
	}

}




@SerialVersionUID(Ver)
object CrossSpecialize {

	type SpecializeFirst[R[X, Y]] = { type T[X] = Specialize[Curry2[R]#A1[X]#A2] }



	/** Default base class for `CrossSpecialize` which enumerates the cases where the first type is a value type,
	  * `Unit` and `Nothing` as [[net.noresttherein.sugar.reflect.Specialize Specialize]] constant fields
	  * specializing for the second type parameter. Performs a similar function
	  * as [[net.noresttherein.sugar.reflect.Specialize.SpecializeSome]], allowing extending classes
	  * to provide individual, dedicated results for selected type parameters, while defaulting to `forAny[X]` method
	  * for all others. As full cartesian specialization on all primitives will result in generation of 100 methods,
	  * it is advisable to not override the `specialized` method at all, which will then delegate to the non-specialized
	  * method `forAny[X, Y]` as the default fallback implementation.
	  * @tparam R a type constructor for the result type, accepting two specialized type parameters.
	  */
	abstract class CrossSpecializeSome[R[X, Y]]
		extends CrossSpecialize[R] with Specifically[SpecializeFirst[R]#T]
	{
		type First[X] = Specialize[Curry2[R]#A1[X]#A2]

		override val forByte    :First[Byte]    = first
		override val forShort   :First[Short]   = first
		override val forChar    :First[Char]    = first
		override val forInt     :First[Int]     = first
		override val forLong    :First[Long]    = first
		override val forFloat   :First[Float]   = first
		override val forDouble  :First[Double]  = first
		override val forBoolean :First[Boolean] = first
		override val forUnit    :First[Unit]    = first[Unit]
		override val forNothing :First[Nothing] = first[Nothing]

		override def forRef[T <: AnyRef :RuntimeType] :First[T] = forOthers[T]
		override def forOthers[T :RuntimeType] :First[T] = first[T]

		/** Method invoked to initialize individual `forT` fields, having the specialization of the first type parameter.
		  * Returns the callback that should be invoked once the second type parameter specialization is determined.
		  * Default callback results in eventual invocation of this instance's `specialized` method.
		  */
		protected def first[@specialized X :RuntimeType] :First[X] = new SpecializeSecond[X]

		/** The final target of the triple dispatch invoked when the specialization for both types has been determined.
		  * Note that overriding this method will create `100` synthetic variants! consider overriding individual
		  * `for`''T'' methods and [[net.noresttherein.sugar.reflect.CrossSpecialize.CrossSpecializeSome.forAny2 forAny2]]
		  * instead.
		  * @return result of invoking `forAny2`.
		  */
		override def specialized[@specialized X :RuntimeType, @specialized Y :RuntimeType] :R[X, Y] =
			forAny2[X, Y]

		protected def forAny2[X :RuntimeType, Y :RuntimeType] :R[X, Y]
//		/** This is the final, non-specialized defender method to which all individual methods delegate by default. */
//		protected def generic[X :RuntimeType, Y :RuntimeType] :R[X, Y]
	}


//	private[reflect] def specialize[@specialized X :RuntimeType, @specialized Y :RuntimeType, R[_, _]]
//	                               (callback :CrossSpecialize[R]) :R[X, Y] =
//		callback.specialized[X, Y]
}
