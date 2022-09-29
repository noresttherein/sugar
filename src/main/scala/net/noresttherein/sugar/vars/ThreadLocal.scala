package net.noresttherein.sugar.vars

import net.noresttherein.sugar.vars.InOut.InOutOrdering
import net.noresttherein.sugar.witness.DefaultValue




/** A very light adapter of [[java.lang.ThreadLocal]] to [[net.noresttherein.sugar.vars.InOut InOut]]`[T]`,
  * which by implicit conversions provides arithmetic assignment operations.
  * @define Ref `ThreadLocal`
  * @author Marcin Mościcki
  */
@SerialVersionUID(ver)
sealed class ThreadLocal[T] private (init :T) extends Mutable[T] {
	private[this] val local = java.lang.ThreadLocal.withInitial[T](() => init)

	override def value :T = local.get
	override def value_=(newValue :T) :Unit = local.set(newValue)

	private[vars] override def isSpecialized = false
}





/** A factory of `InOut[T]` variables backed by thread local storage. */
@SerialVersionUID(ver)
object ThreadLocal {
	@inline def apply[T](init :T) :ThreadLocal[T] = new ThreadLocal[T](init)
	@inline def apply[T](implicit default :DefaultValue[T]) :ThreadLocal[T] = new ThreadLocal(default.get)


	implicit def ThreadLocalOrdering[T :Ordering] :Ordering[ThreadLocal[T]] = new InOutOrdering[ThreadLocal, T]
}
