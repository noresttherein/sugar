package net.turambar.slang

import Specializable.Primitives

/** An alternative to inbuilt `lazy val`s, this implementation provides callbacks allowing to check it's initialization state.
  *
  * @author Marcin Mościcki
  */
trait Lazy[@specialized(Primitives) +T] extends Function0[T] {
	@inline final def apply() :T = value
	def value :T

	def isInitialized :Boolean

//	override def canEqual(that :Any) =

	override def equals(that :Any) :Boolean = that match {
		case lzy :Lazy[_] => (lzy eq this) || lzy.value == value
		case _ => false
	}

	override def hashCode = value.hashCode
}


object Lazy {

	@inline final def apply[@specialized(Primitives) T](init : =>T) :Lazy[T] = new SyncLazy(init)


	/** Unlike default `Lazy(_)` and inbuilt `lazy val`s, this instance doesn't synchronize,
	  * yielding possibly minor performance benefit while still remaining thread safe. It happens
	  * at the cost of possibly evaluating the initialization callback more than once, and in the case
	  * of concurrent access of unitialized value the result isn't determined; it is therefore strongly encouraged
	  * to use only relatively lightweight and '''idempotent''' functions which won't return null as initializers.
	  * All testing functions become even less helpful, but at least it is guaranteed that if `init` never returns
	  * `null` values, once `isInitialized` returns `true`, it will always happen so
	  * (in the sense of java memory 'happens before' relation).
	  */
	@inline final def lzy[@specialized(Primitives) T](idempotent : =>T) :Lazy[T] = new VolatileLazy(idempotent)

	@inline final def eager[@specialized(Primitives) T](value :T) :Lazy[T] = new EagerLazy(value)

	@inline final def isEager[T](lzy :()=>T) = lzy.isInstanceOf[EagerLazy[_]]


	implicit def delazify[@specialized(Primitives) T](l :Lazy[T]) :T = l.value

	implicit def lazify[@specialized(Primitives) T](value : =>T) :Lazy[T] = new VolatileLazy[T](value)


	private final class EagerLazy[@specialized(Primitives) T](val value :T) extends Lazy[T] {
		override def isInitialized = true
		override def toString = value.toString
	}


	private final class VolatileLazy[@specialized(Primitives) T](idempotent : =>T) extends Lazy[T] {
		@volatile private[this] var evaluated :T = _

		override def value: T = {
			if (evaluated==null)
				evaluated = idempotent
			evaluated
		}

		override def isInitialized: Boolean = evaluated!=null

		override def toString =
			if (evaluated!=null) evaluated.toString
			else "lzy(?)"

	}

	private final class SyncLazy[@specialized(Primitives) T](init : =>T) extends Lazy[T] {
		private[this] var evaluated :T = _

		def value :T = synchronized {
			if (evaluated==null)
				evaluated = init
			evaluated
		}

		def isInitialized = synchronized { evaluated!=null }


		override def toString = synchronized {
			if (evaluated!=null) evaluated.toString
			else "Lazy(?)"
		}
	}
}
