package net.noresttherein.slang.vars

import net.noresttherein.slang.vars.InOut.DefaultValue


/** A very light adapter of `java.lang.ThreadLocal` to `InOut[T]`, which by implicit conversions provides
  * arithmetic assignment operations. Instances of these class.
  * @author Marcin Mościcki
  */
class ThreadLocal[T](init :T) extends InOut[T] {
	private[this] val local = java.lang.ThreadLocal.withInitial[T](() => init)

	override def get :T = local.get

	override def value_=(value :T) :Unit = local.set(value)

}





/** A factory of `InOut[T]` variables backed by thread local storage. */
object ThreadLocal {
	@inline def apply[T](init :T) :ThreadLocal[T] = new ThreadLocal[T](init)

	@inline def apply[T](implicit default :DefaultValue[T]) :ThreadLocal[T] =
		new ThreadLocal(default.value)
}
