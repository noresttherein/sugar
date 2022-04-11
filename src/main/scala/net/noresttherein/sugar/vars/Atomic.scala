package net.noresttherein.sugar.vars

import java.lang.invoke.VarHandle.{acquireFence, releaseFence}

import net.noresttherein.sugar.vars.InOut.{InOutOrdering, SpecializedVars}
import net.noresttherein.sugar.vars.VolatileLike.{BoolVolatileLike, RefVolatileLike}




/** Atomic variables providing several 'test-and-set' operations. They are very similar
  * to [[net.noresttherein.sugar.vars.Volatile Volatile]], but unlike them, and unlike their Java counterparts,
  * value read happens only under [[java.lang.invoke.VarHandle.getAcquire getAcquire]]
  * and value write under [[java.lang.invoke.VarHandle.setRelease setRelease]] memory ordering guarantees,
  * unless done as a part of one of the atomic operations defined by [[net.noresttherein.sugar.vars.InOut InOut]]
  * interface.
  *
  * The effect roughly means that the value returned by this instance's getter will be at least as old as any value
  * read after that call in program order, while assignments to it will happen after any reads and writes preceding
  * them in program order - see the documentation of [[java.lang.invoke.VarHandle VarHandle]] for more specific
  * information.
  *
  * This makes this class slightly more efficient in highly contentious environment at the cost of lesser applicability.
  * It is best used as a flag controlling access to some other resource.
  *
  * @author Marcin Mościcki marcin@moscicki.net
  */
@SerialVersionUID(1L)
sealed class Atomic[@specialized(SpecializedVars) T] private[vars] (private[this] var x :T)
	extends VolatileLike[T] with Mutable[T] with Serializable
{
	protected override def factory :Atomic.type = Atomic

	override def value :T = {
		val res = x
		acquireFence()
		res
	}

	override def value_=(newValue :T) :Unit = {
		releaseFence()
		x = value
	}

//	override def toString :String =
}




/** Factory of [[net.noresttherein.sugar.vars.Atomic atomic]] variables.
  * @define variable atomic variable
  */
object Atomic extends VolatileLikeFactory[Atomic] {

	implicit def AtomicOrdering[T :Ordering] :Ordering[Atomic[T]] = new InOutOrdering[Atomic, T]


	protected override def newInstance[@specialized(SpecializedVars) T](init :T) :Atomic[T] = new Atomic(init)
	protected override def newRefInstance[T](init :T) :Atomic[T] = new AtomicRef(init)
	protected override def newBoolInstance(init :Boolean) :Atomic[Boolean] = new AtomicBool(init)


	/** An unspecialized `Atomic` implementation overriding atomic mutator methods to compare the value
	  * using `eq`/`ne`, rather than `==`/`!=` as in `Atomic` (which would call `equals` on reference types,
	  * which we do not want).
	  */
	@SerialVersionUID(1L)
	private class AtomicRef[T](init :T) extends Atomic[T](init) with RefVolatileLike[T]

	/** Optimised implementation of `Atomic[Bool]` which enumerates all two possible results
	  * in accumulate/mutate methods.
	  */
	@SerialVersionUID(1L)
	private class AtomicBool(init :Boolean) extends Atomic[Boolean](init) with BoolVolatileLike

}
