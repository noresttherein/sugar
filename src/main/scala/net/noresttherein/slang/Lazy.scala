package net.noresttherein.slang

import scala.Specializable.Primitives

/** An alternative to inbuilt `lazy val`s, this implementation provides callbacks allowing to check it's initialization state.
  * Also, once the value is confirmed to be initialized, the initializer expression is freed for garbage collection.
  * @author Marcin Mościcki
  */
trait Lazy[@specialized(Primitives) +T] extends (()=>T) {
	@inline final def apply() :T = value
	def value :T

	def isEvaluated :Boolean
	def isUndefined = false

	def map[@specialized(Primitives) O](f :T=>O) :Lazy[O]
	def flatMap[@specialized(Primitives) O](f :T=>Lazy[O]) :Lazy[O]

	override def equals(that :Any) :Boolean = that match {
		case lzy :Lazy[_] => (lzy eq this) || lzy.value == value
		case _ => false
	}

	override def hashCode :Int = value.hashCode
}



object Lazy {

	@inline final def apply[@specialized(Primitives) T](init : =>T) :Lazy[T] = new SyncLazy(() => init)


	/** Unlike default `Lazy(_)` and inbuilt `lazy val`s, this instance doesn't synchronize,
	  * yielding possibly minor performance benefit while still remaining thread safe. It happens
	  * at the cost of possibly evaluating the initialization callback more than once, and concurrent access
	  * of a uninitialized value may return results from different calls. For this reason the initializer should
	  * be a relatively lightweight and '''idempotent''' function which never returns a null value.
	  * All testing functions become even less helpful, but at least it is guaranteed that if `idempotent` never returns
	  * `null` values, once `isInitialized` returns `true`, it will always be so
	  * (in the sense of the java memory 'happens before' relation).
	  */
	@inline final implicit def lazyval[@specialized(Primitives) T](idempotent : =>T) :Lazy[T] = new VolatileLazy(() => idempotent)

	/** A wrapper over a computed value adapting it to the `Lazy` type. */
	@inline final def eager[@specialized(Primitives) T](value :T) :Lazy[T] = new EagerLazy(value)

	@inline final def isEager[T](lzy :()=>T) :Boolean = lzy.isInstanceOf[EagerLazy[_]]


	implicit def delazify[@specialized(Primitives) T](l :Lazy[T]) :T = l.value


	private final object Undefined extends Lazy[Nothing] {
		def value = throw new NoSuchElementException("Lazy.Undefined.value")
		override def isEvaluated = false
		override def isUndefined = true

		override def map[@specialized(Primitives) O](f: Nothing => O): this.type = this

		override def flatMap[@specialized(Primitives) O](f: Nothing => Lazy[O]): this.type = this
	}

	/** An already computed (initialized value) */
	private final class EagerLazy[@specialized(Primitives) T](eager :T) extends Lazy[T] {
		def value :T = eager
		override def isEvaluated = true
		override def toString :String = value.toString

		override def map[@specialized(Primitives) O](f: T => O): EagerLazy[O] = new EagerLazy(f(eager))

		override def flatMap[@specialized(Primitives) O](f: T => Lazy[O]): Lazy[O] = f(eager)
	}


	private final class VolatileLazy[@specialized(Primitives) +T](idempotent : ()=>T) extends Lazy[T] {
		def this(value :T) = { this(null); evaluated = value; fast = value }

		@volatile private[this] var init = idempotent
		@volatile private[this] var evaluated :T = _
		private[this] var fast :T = _
		private[this] var fastInit = false

		override def value: T = {
			if (fastInit)
				fast
			else if (init != null) {
				fast = init()
				fastInit = true
				evaluated = fast
				init = null //clear the specialized field
				clear()     //clear the erased field
				fast
			}else {
				fast = evaluated
				fastInit = true
				fast
			}
		}

		private def clear() :Unit = init = null

		override def isEvaluated: Boolean = fastInit || init == null
		override def isUndefined = false


		override def map[@specialized(Primitives) O](f: T => O): VolatileLazy[O] =
			if (fastInit)
				new VolatileLazy(f(fast))
			else {
				val init = this.init
				if (init == null) new VolatileLazy(f(evaluated))
				else new VolatileLazy(() => f(init()))
			}

		override def flatMap[@specialized(Primitives) O](f: T => Lazy[O]): Lazy[O] =
			if (fastInit)
				f(fast)
			else {
				val init = this.init
				if (init == null) f(evaluated)
				else new VolatileLazy(() => f(init()).value)
			}

		override def toString :String =
			if (init==null) evaluated.toString
			else "volatile(?)"

	}



	private final class SyncLazy[@specialized(Primitives) T](evaluate : () =>T) extends Lazy[T] {

		def this(value :T) = { this(null); evaluated = value }

		private[this] var init = evaluate
		private[this] var evaluated :T = _

		def value :T = synchronized {
			if (init!=null) {
				evaluated = init()
				init = null //clears the specialized field
				clear()     //clears the erased field
				isEvaluated
			}
			evaluated
		}

		/** A specialized class has two `init` fields (one in the generic base class and one specialized).
		  * Snippet `init = null` in specialized `value` method clears only the specialized field, while
		  * the `isEvaluated` method checks only the generic (base) field. Hence a non-specialized method
		  * to explicitly clear the reference field `init`.
		  */
		private[this] def clear() :Unit = init = null

		def isEvaluated :Boolean = synchronized { init == null }
		override def isUndefined = false


		override def map[@specialized(Primitives) O](f: T => O): SyncLazy[O] = synchronized {
			if (init==null) new SyncLazy(f(evaluated))
			else new SyncLazy(() => f(init()))
		}

		override def flatMap[@specialized(Primitives) O](f: T => Lazy[O]): Lazy[O] = synchronized {
			if (init==null) f(evaluated)
			else new SyncLazy(() => f(init()).value)
		}

		override def toString :String = synchronized {
			if (init==null) evaluated.toString
			else "lazy(?)"
		}
	}
}
