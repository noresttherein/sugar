package net.noresttherein.slang.vars

import net.noresttherein.slang.funny.Initializer
import net.noresttherein.slang.optional.Opt
import net.noresttherein.slang.optional.Opt.Got




/** A monadic lazy value. Unlike scala's `lazy val`s, this implementation doesn't incur any synchronization penalty
  * once the value is initialized. Additionally, it provides callbacks allowing to check it's initialization state and,
  * once the value is initialized, the initializer expression is freed for garbage collection.
  * Three implementations exist: one which uses a synchronized block to ensure it is initialized at most once,
  * and one which uses only a `@volatile` variable, at the cost of possibly executing the initializer block multiple
  * times in case of concurrent initial access, and a variant of the latter which does not preserve the evaluated
  * value during serialization.
  * @author Marcin Mościcki
  */ //not specialized to avoid boxing during generic access; boxing at initialization will be overshadowed by reads.
@SerialVersionUID(1L)
trait Lazy[+T] extends (()=>T) with Val[T] with Serializable {

	/** The evaluated value of this instance. */
	@inline final def apply() :T = value

	/** The evaluated value of this instance. */
	def get :T

	/** Always returns [[net.noresttherein.slang.optional.Opt.Got Got]]`(value)`. */
	override def toOpt :Opt[T] = Got(value)

	/** Checks if the value has been previously evaluated. Note that `false` values can be stale the moment
	  * it is returned to the caller; the method is however still useful as once `true` is returned, all subsequent
	  * calls ''on this instance'' will also return `true` and the value can be safely accessed without an overhead
	  * or risk of throwing an exception.
	  */
	def isInitialized :Boolean

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

	override def toString :String = if (isInitialized) get.toString else "Lazy(?)"
}





/** A factory of lazy values. */
object Lazy {

	/** Creates a wrapper over a lazily initialized value. The difference from the scala's `lazy val`s are:
	  *   - no synchronization penalty once the value has been initialized while remaining fully thread safe;
	  *   - monadic `map` and `flatMap` operations;
	  *   - the initializer expression is freed for garbage collection;
	  *   - `isInitialized` test method.
	  *  The implementation returned by this method uses its synchronized lock to ensure the `init` block
	  *  is called at most once.
	  *  @see
	  */
	def apply[T](init : =>T) :Lazy[T] = new SyncLazy(() => init)

	/** Unlike default `Lazy(_)` and inbuilt `lazy val`s, never uses a `synchronized` block,
	  * yielding possibly minor performance benefit while still remaining thread safe. It happens
	  * at the cost of possibly evaluating the initialization callback more than once, and concurrent access
	  * of an uninitialized value may return results from different calls. For this reason the initializer should
	  * be a relatively lightweight and '''idempotent''' function.
	  * All testing functions become even less helpful, but at least it is guaranteed that once `isInitialized`
	  * returns `true`, it will always be so (in the sense of the java memory 'happens before' relation).
	  */
	def idempotent[T](idempotent : =>T) :Lazy[T] = new VolatileLazy(() => idempotent)

	/** Similar to [[net.noresttherein.slang.vars.Lazy.idempotent idempotent]] lazy value, but on serialization,
	  * instead of evaluating the value and freeing the initializer expression for garbage collection,
	  * it does the opposite: the evaluated value is `@transient` and the initializer is never dereferenced,
	  * meaning it will be evaluated again on deserialization if the value is required. This is useful
	  * if the value is large, not serializable, or if it references singleton values (which do not implement
	  * aliasing after deserialization to ensure that only one instance exists as Scala's singleton objects do).
	  * @param idempotent an initializer expression of a SAM type extending `() => T`, allowing use of literal
	  *                   expressions of the latter form as argument.
	  */
	@inline def transient[T](idempotent :Initializer[T]) :Lazy[T] = Transient[T](idempotent)


	/** A wrapper over a computed value adapting it to the `Lazy` type. */
	def eager[T](value :T) :Lazy[T] = new EagerLazy(value)

	def isEager[T](lzy :()=>T) :Boolean = lzy.isInstanceOf[EagerLazy[_]]


	implicit def delazify[T](l :Lazy[T]) :T = l.get



	@SerialVersionUID(1L)
	private final object Undefined extends Lazy[Nothing] {
		override def value = throw new NoSuchElementException("Lazy.Undefined.get")
		override def isInitialized = false
		override def isUndefined = true

		override def map[O](f: Nothing => O): this.type = this

		override def flatMap[O](f: Nothing => Lazy[O]): this.type = this
	}



	/** An already computed (initialized value) */
	@SerialVersionUID(1L)
	private final class EagerLazy[T](eager :T) extends Lazy[T] {
		override def value :T = eager
		override def toOpt :Opt[T] = Got(eager)
		override def isInitialized = true
		override def toString :String = get.toString

		override def map[O](f: T => O): EagerLazy[O] = new EagerLazy(f(eager))

		override def flatMap[O](f: T => Lazy[O]): Lazy[O] = f(eager)
	}



	@SerialVersionUID(1L)
	private final class VolatileLazy[+T](idempotent : ()=>T) extends Lazy[T] {

		def this(value :T) = {
			this(null); evaluated = value; cached = value
		}

		@scala.volatile @transient private[this] var initializer = idempotent
		@scala.volatile private[this] var evaluated :T = _
		private[this] var cached :T = _

		override def value: T = {
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

		override def isInitialized: Boolean = cached != null || initializer == null


		override def map[O](f: T => O): VolatileLazy[O] =
			if (cached != null)
				new VolatileLazy(f(cached))
			else {
				val init = initializer
				if (init == null) {
					cached = evaluated
					new VolatileLazy(f(cached))
				} else
					new VolatileLazy(() => f(get))
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


		private def writeReplace = Lazy.eager(get)
	}



	@SerialVersionUID(1L)
	private final class SyncLazy[+T](evaluate : () =>T) extends Lazy[T] {

		def this(value :T) = {
			this(null); evaluated = value; cached = value
		}

		@volatile @transient private[this] var initializer = evaluate
		@volatile private[this] var evaluated :T = _
		private[this] var cached :T = _

		override def value :T = {
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

		override def isInitialized :Boolean = initializer == null

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
						new SyncLazy(() => f(get))
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

		private def writeReplace = Lazy.eager(get)
	}


}

