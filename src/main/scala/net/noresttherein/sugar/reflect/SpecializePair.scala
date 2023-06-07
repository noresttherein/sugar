package net.noresttherein.sugar.reflect

import net.noresttherein.sugar.reflect.Specialize.SpecializeIndividually
import net.noresttherein.sugar.reflect.SpecializePair.{Curry, SpecializeFirst}




/** Similarly to how [[net.noresttherein.sugar.reflect.Specialize]] performs a callback to a specialized method
  * based on implicitly available `RuntimeType[T]`, this class executes a double callback to invoke a method
  * specialized for two types.
  * @tparam R a type constructor for the result type, accepting two specialized type parameters.
  * @author Marcin Mościcki marcin@moscicki.net
  */
abstract class SpecializePair[R[X, Y]] extends Specialize[SpecializeFirst[R]#T] { pair =>

	/** Initiates the double callback using the implicitly passed runtime type information to pick the appropriate
	  * specialized variant of [[net.noresttherein.sugar.reflect.SpecializePair#specialize[X, Y]].
	  */
	def apply[X, Y]()(implicit xType :RuntimeType[X], yType :RuntimeType[Y]) :R[X, Y] = apply[X]().apply[Y]()

	/** Intermediate callback specialized for the first type parameter, which returns the callback
	  * for the specialization of the second parameter.
	  */
	override def specialized[@specialized X :RuntimeType] :Specialize[Curry[R]#T1[X]#T2] =
		new SpecializeSecond[X]

	/** Specialized callback method invoked as the result of the call to `apply()`,
	  * responsible for producing the final result value.
	  */
	def specialized[@specialized X :RuntimeType, @specialized Y :RuntimeType] :R[X, Y]

	private[SpecializePair] class SpecializeSecond[@specialized T :RuntimeType]
		extends Specialize[Curry[R]#T1[T]#T2]
	{
		override def specialized[@specialized Y :RuntimeType] :R[T, Y] = pair.specialized[T, Y]
	}

}




@SerialVersionUID(Ver)
object SpecializePair {

	type Curry[R[X, Y]] = { type T1[X] = { type T2[Y] = R[X, Y] } }

	type SpecializeFirst[R[X, Y]] = { type T[X] = Specialize[Curry[R]#T1[X]#T2] }



	/** An equivalent of [[net.noresttherein.sugar.reflect.Specialize.SpecializeSome]], it allows extending classes
	  * to provide individual, dedicated results for selected type parameters, while defaulting to the `generic` method
	  * for all others. As full cartesian specialization on all primitives will result in generation of 100 methods,
	  * it is advisable to not override the `specialized` method at all, which will delegate to the non-specialized
	  * method `generic[X, Y]` as the default fallback implementation. Instead,
	  * @tparam R a type constructor for the result type, accepting two specialized type parameters.
	  */
	abstract class SpecializeSomePairs[R[X, Y]]
		extends SpecializePair[R] with SpecializeIndividually[SpecializeFirst[R]#T]
	{
		type First[X] = Specialize[Curry[R]#T1[X]#T2]

		override val forByte    :First[Byte]    = second
		override val forShort   :First[Short]   = second
		override val forChar    :First[Char]    = second
		override val forInt     :First[Int]     = second
		override val forLong    :First[Long]    = second
		override val forFloat   :First[Float]   = second
		override val forDouble  :First[Double]  = second
		override val forBoolean :First[Boolean] = second
		override val forUnit    :First[Unit]    = second[Unit]
		override val forNothing :First[Nothing] = second[Nothing]

		override def forRef[T :RuntimeType] :First[T] = second[T]

		/** Method invoked to initialize individual `forT` fields, having the specialization of the first type parameter.
		  * Returns the callback that should be invoked once the second type parameter specialization is determined.
		  * Default callback results in eventual invocation of this instance's `specialized` method.
		  */
		protected def second[@specialized X :RuntimeType] :First[X] = new SpecializeSecond[X]

		/** The final target of the triple dispatch invoked when the specialization for both types has been determined.
		  * Note that overriding this method will create `100` synthetic variants! consider overriding individual
		  * `for`''T'' methods and [[net.noresttherein.sugar.reflect.SpecializePair.SpecializeSomePairs]] instead.
		  * @return result of invoking `generic`
		  */
		override def specialized[@specialized X :RuntimeType, @specialized Y :RuntimeType] :R[X, Y] = generic[X, Y]

		/** This is the final, non-specialized defender method to which all individual methods delegate by default.
		  */
		protected[this] def generic[X :RuntimeType, Y :RuntimeType] :R[X, Y]
	}

}
