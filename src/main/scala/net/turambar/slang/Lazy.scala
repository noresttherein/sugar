package net.turambar.slang

import Specializable.Primitives

/** An alternative to inbuilt `lazy val`s, this implementation provides callbacks allowing to check it's initialization state.
  * Also, once the value is confirmed to be initialized, the initializer expression is freed for garbage collection.
  * @author Marcin Mościcki
  */
trait Lazy[@specialized(Primitives) +T] extends (()=>T) {
	@inline final def apply() :T = value
	def value :T

	def isInitialized :Boolean
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

	@inline final def apply[@specialized(Primitives) T](init : =>T) :SyncLazy[T] = new SyncLazy(() => init)

	type lazyval[@specialized(Primitives) +T] = VolatileLazy[T]

	/** Unlike default `Lazy(_)` and inbuilt `lazy val`s, this instance doesn't synchronize,
	  * yielding possibly minor performance benefit while still remaining thread safe. It happens
	  * at the cost of possibly evaluating the initialization callback more than once, and in the case
	  * of concurrent access of uninitialized value the result isn't specified. It is therefore strongly encouraged
	  * to use only relatively lightweight and '''idempotent''' functions which won't return null as initializers.
	  * All testing functions become even less helpful, but at least it is guaranteed that if `init` never returns
	  * `null` values, once `isInitialized` returns `true`, it will always be so
	  * (in the sense of java memory 'happens before' relation).
	  */
	@inline final implicit def lazyval[@specialized(Primitives) T](idempotent : =>T) :VolatileLazy[T] = new VolatileLazy(() => idempotent)

	@inline final def eager[@specialized(Primitives) T](value :T) :Lazy[T] = new EagerLazy(value)

	@inline final def isEager[T](lzy :()=>T) :Boolean = lzy.isInstanceOf[EagerLazy[_]]


	implicit def delazify[@specialized(Primitives) T](l :Lazy[T]) :T = l.value


	private final object Undefined extends Lazy[Nothing] {
		@inline final def value = throw new NoSuchElementException("Lazy.Undefined.value")
		@inline override def isInitialized = false
		@inline override def isUndefined = true

//		@inline override def filter(p: (Nothing) => Boolean): this.type = this
//		@inline override def withFilter(p: (Nothing) => Boolean): this.type = this

		@inline override def map[@specialized(Primitives) O](f: Nothing => O): this.type = this

		@inline override def flatMap[@specialized(Primitives) O](f: Nothing => Lazy[O]): this.type = this
	}

	/** An already computed (initialized value) */
	private final class EagerLazy[@specialized(Primitives) T](eager :T) extends Lazy[T] {
		@inline def value :T = eager
		@inline override def isInitialized = true
		@inline override def toString :String = value.toString

		@inline override def map[@specialized(Primitives) O](f: T => O): EagerLazy[O] = new EagerLazy(f(eager))

		@inline override def flatMap[@specialized(Primitives) O](f: T => Lazy[O]): Lazy[O] = f(eager)
	}


	final class VolatileLazy[@specialized(Primitives) +T](idempotent : ()=>T) extends Lazy[T] {
		def this(value :T) = { this(null); evaluated = value; }

		@volatile private[this] var init = idempotent
		@volatile private[this] var evaluated :T = _

		@inline override def value: T = {
			if (init != null) {
				evaluated = init()
				init = null
			}
			evaluated
		}

		@inline override def isInitialized: Boolean = init == null
		@inline override def isUndefined = false


		@inline override def map[@specialized(Primitives) O](f: T => O): VolatileLazy[O] = {
			val init = this.init
			if (init == null) new VolatileLazy(f(evaluated))
			else new VolatileLazy(() => f(init()))
		}

		@inline override def flatMap[@specialized(Primitives) O](f: T => Lazy[O]): Lazy[O] = {
			val init = this.init
			if (init == null) f(evaluated)
			else new VolatileLazy(() => f(init()).value)
		}

		override def toString :String =
			if (init==null) evaluated.toString
			else "volatile(?)"

	}



	final class SyncLazy[@specialized(Primitives) T](private[this] var init : () =>T) extends Lazy[T] {

		def this(value :T) = { this(null); evaluated = value }

		private[this] var evaluated :T = _

		@inline def value :T = synchronized {
			if (init!=null) {
				evaluated = init()
				init = null
			}
			evaluated
		}

		@inline def isInitialized :Boolean = synchronized { init == null }
		@inline override def isUndefined = false


		@inline override def map[@specialized(Primitives) O](f: T => O): SyncLazy[O] = synchronized {
			if (init==null) new SyncLazy(f(evaluated))
			else new SyncLazy(() => f(init()))
		}

		@inline override def flatMap[@specialized(Primitives) O](f: T => Lazy[O]): Lazy[O] = synchronized {
			if (init==null) f(evaluated)
			else new SyncLazy(() => f(init()).value)
		}

		override def toString :String = synchronized {
			if (init==null) evaluated.toString
			else "lazy(?)"
		}
	}
}
