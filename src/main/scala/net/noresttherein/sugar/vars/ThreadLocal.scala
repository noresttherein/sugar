package net.noresttherein.sugar.vars

import java.util.function.Supplier

import net.noresttherein.sugar.vars.InOut.InOutOrdering
import net.noresttherein.sugar.vars.ThreadLocal.CustomThreadLocal
import net.noresttherein.sugar.witness.DefaultValue




/** A very light adapter of [[java.lang.ThreadLocal]] to [[net.noresttherein.sugar.vars.InOut InOut]]`[T]`,
  * which by implicit conversions provides arithmetic assignment operations.
  * @define Ref `ThreadLocal`
  * @define ref thread local variable
  * @author Marcin Mościcki
  */ //Not Serializable - should it be?
sealed class ThreadLocal[T] protected (local :java.lang.ThreadLocal[T]) extends Mutable[T] {
	def this(init :() => T) = this(new CustomThreadLocal(init))

	override def value :T = local.get
	override def value_=(newValue :T) :Unit = local.set(newValue)

	private[vars] override def isSpecialized = false
}




/** A factory of `InOut[T]` variables backed by thread local storage. */
object ThreadLocal {
	def apply[T](init: => T) :ThreadLocal[T] = new ThreadLocal[T](() => init)

	@inline def apply[T](implicit default :DefaultValue[T]) :ThreadLocal[T] = new ThreadLocal(default.function0)

	def inheritable[T](init: => T, inherit :T => T) :ThreadLocal[T] = new ThreadLocal[T](
		new java.lang.InheritableThreadLocal[T] {
			override def initialValue() = init
			override def childValue(parentValue :T) = inherit(parentValue)
		}
	)

	@inline def inheritable[T](init: => T) :ThreadLocal[T] = inheritable(init, identity)

	def inheritable[T](inherit :T => T)(implicit default :DefaultValue[T]) :ThreadLocal[T] =
		new ThreadLocal[T](
			new java.lang.InheritableThreadLocal[T] {
				override def initialValue() = default.get
				override def childValue(parentValue :T) :T = inherit(parentValue)
			}
		)

	@inline def inheritable[T](implicit default :DefaultValue[T]) :ThreadLocal[T] =
		inheritable(identity[T] _)(default)

	implicit def ThreadLocalOrdering[T :Ordering] :Ordering[ThreadLocal[T]] = new InOutOrdering[ThreadLocal, T]

	private class CustomThreadLocal[T](init :() => T) extends java.lang.ThreadLocal[T] {
		override def initialValue() = init()
	}
}
