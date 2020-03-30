package net.noresttherein.slang




/** A monadic lazy value. Unlike scala's `lazy val`s, this implementation doesn't incur any synchronization penalty
  * once the value is initialized. Additionally, it provides callbacks allowing to check it's initialization state and,
  * once the value is initialized, the initializer expression is freed for garbage collection.
  * Two implementations exist: one which uses a synchronized block to ensure it is initialized at most once,
  * and one which uses only a `@volatile` variable, at the cost of possibly executing the initializer block multiple
  * times in case of concurrent initial access.
  * @author Marcin Mościcki
  */
trait Lazy[+T] extends (()=>T) {
	@inline final def apply() :T = get
	def get :T

	def isEvaluated :Boolean
	def isUndefined = false

	/** Creates a new `Lazy[O]` instance with the same characteristics as this instance, evaluated to the application
	  * of `f` to the value of this instance. If the value has already been evaluated, created instance will be
	  * eagerly evaluated - use `Lazy(f(this.get))` if you wish for `f` to not be executed before
	  * the method returns under any circumstances. If this instance initializes under a `synchronized` block,
	  * it is guaranteed that it will be evaluated at most once, regardless of which of the two values are
	  * accessed. Created `Lazy[O]` will likewise use the `synchronized` block in that case and `f` will be evaluated
	  * at most once.
	  */
	def map[O](f :T => O) :Lazy[O]

	/** Creates a new `Lazy[O]` initialized with the expression `f(this.get).get`. If this instance is already
	  * evaluated, the function will be applied immediately and its result returned directly. Otherwise a new
	  * `Lazy[O]` with the same synchronization characteristics as this instance will be created, initialized
	  * with the expression `f(this.get).get`. If you wish for `f` to not be executed before the method returns
	  * and the returned instance is accessed, use `Lazy(f(this.get).get)`.
	  */
	def flatMap[O](f :T => Lazy[O]) :Lazy[O]

	override def equals(that :Any) :Boolean = that match {
		case lzy :Lazy[_] => (lzy eq this) || lzy.get == get
		case _ => false
	}

	override def hashCode :Int = get.hashCode
}



/** A factory of lazy values. */
object Lazy {

	/** Creates a wrapper over a lazily initialized value. The difference from the scala's `lazy val`s are:
	  *   - no synchronization penalty once the value has been initialized while remaining fully thread safe;
	  *   - monadic `map` and `flatMap` operations;
	  *   - the initializer expression is freed for garbage collection;
	  *   - `isEvaluated` test method.
	  *  The implementation returned by this method uses its synchronized lock to ensure the `init` block
	  *  is called at most once.
	  *  @see
	  */
	@inline final def apply[T](init : =>T) :Lazy[T] = new SyncLazy(() => init)


	/** Unlike default `Lazy(_)` and inbuilt `lazy val`s, never uses a `synchronized` block,
	  * yielding possibly minor performance benefit while still remaining thread safe. It happens
	  * at the cost of possibly evaluating the initialization callback more than once, and concurrent access
	  * of an uninitialized value may return results from different calls. For this reason the initializer should
	  * be a relatively lightweight and '''idempotent''' function.
	  * All testing functions become even less helpful, but at least it is guaranteed that once `isInitialized`
	  * returns `true`, it will always be so (in the sense of the java memory 'happens before' relation).
	  */
	@inline final implicit def later[T](idempotent : =>T) :Lazy[T] = new VolatileLazy(() => idempotent)

	/** A wrapper over a computed value adapting it to the `Lazy` type. */
	@inline final def eager[T](value :T) :Lazy[T] = new EagerLazy(value)

	@inline final def isEager[T](lzy :()=>T) :Boolean = lzy.isInstanceOf[EagerLazy[_]]


	implicit def delazify[T](l :Lazy[T]) :T = l.get



	private final object Undefined extends Lazy[Nothing] {
		def get = throw new NoSuchElementException("Lazy.Undefined.get")
		override def isEvaluated = false
		override def isUndefined = true

		override def map[O](f: Nothing => O): this.type = this

		override def flatMap[O](f: Nothing => Lazy[O]): this.type = this
	}



	/** An already computed (initialized value) */
	private final class EagerLazy[T](eager :T) extends Lazy[T] {
		def get :T = eager
		override def isEvaluated = true
		override def toString :String = get.toString

		override def map[O](f: T => O): EagerLazy[O] = new EagerLazy(f(eager))

		override def flatMap[O](f: T => Lazy[O]): Lazy[O] = f(eager)
	}



	private final class VolatileLazy[+T](idempotent : ()=>T) extends Lazy[T] {
		def this(value :T) = {
			this(null); evaluated = value; cached = value
		}

		@volatile private[this] var initializer = idempotent
		@volatile private[this] var evaluated :T = _
		private[this] var cached :T = _

		override def get: T = {
			if (cached == null) {
				val init = initializer //check initializer and not evaluated, as initializer might have returned `null`.
				if (init == null) { //both initializer and evaluated are volatile, so if init==null then evaluated is set.
					cached = evaluated
				} else {
					cached = init()
					evaluated = cached
					initializer = null
				}
			}
			cached
		}

		override def isEvaluated: Boolean = cached != null || initializer == null
		override def isUndefined = false


		override def map[O](f: T => O): VolatileLazy[O] =
			if (cached != null)
				new VolatileLazy(f(cached))
			else {
				val init = initializer
				if (init == null) {
					cached = evaluated
					new VolatileLazy(f(cached))
				} else
					new VolatileLazy(() => f(get).get)
			}

		override def flatMap[O](f: T => Lazy[O]): Lazy[O] =
			if (cached != null)
				f(cached)
			else {
				val init = this.initializer
				if (init == null) {
					cached = evaluated
					f(cached)
				} else
					new VolatileLazy(() => f(get).get)
			}

		override def toString :String =
			if (cached != null) cached.toString
			else if (initializer == null) evaluated.toString
			else "lazy(?)"

	}



	private final class SyncLazy[+T](evaluate : () =>T) extends Lazy[T] {

		def this(value :T) = {
			this(null); evaluated = value; cached = value
		}

		@volatile private[this] var initializer = evaluate
		@volatile private[this] var evaluated :T = _
		private[this] var cached :T = _

		def get :T = {
			if (cached == null) {
				var init = initializer
				if (init == null) {
					cached = evaluated
				} else synchronized {
					init = initializer
					if (init == null)
						cached = evaluated
					else {
						cached = init()
						evaluated = cached
						initializer = null
					}
				}
			}
			cached
		}


		def isEvaluated :Boolean = initializer == null
		override def isUndefined = false


		override def map[O](f: T => O): SyncLazy[O] =
			if (cached != null)
				new SyncLazy(f(cached))
			else {
				var init = initializer
				if (init == null) {
					cached = evaluated
					new SyncLazy(f(cached))
				} else synchronized {
					init = initializer
					if (init == null) {
						cached = evaluated
						new SyncLazy(f(cached))
					} else
						new SyncLazy(() => f(get).get)
				}
			}

		override def flatMap[O](f: T => Lazy[O]): Lazy[O] =
			if (cached != null)
				new SyncLazy(f(cached))
			else {
				var init = initializer
				if (init == null) {
					cached = evaluated
					f(cached)
				} else synchronized {
					init = initializer
					if (init == null) {
						cached = evaluated
						f(cached)
					} else
						new SyncLazy(() => f(get).get)
				}
			}

		override def toString :String = synchronized {
			if (cached != null) cached.toString
			else if (initializer == null) evaluated.toString
			else "lazy(?)"
		}

	}


}
