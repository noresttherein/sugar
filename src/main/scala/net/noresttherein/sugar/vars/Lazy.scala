package net.noresttherein.sugar.vars

import scala.annotation.unspecialized

import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.vars.Ref.{undefined, RefFractional, RefIntegral, RefNumeric, RefOrdering}




/** A monadic lazy value. Unlike scala's `lazy val`s, this implementation doesn't incur any synchronization penalty
  * once the value is initialized for built in value types. Additionally, it provides methods allowing to check
  * its initialization state and provides monadic operations for constructing other `Lazy` instances.
  *
  * Three implementations exist:
  *   1. The default one which uses a synchronized block to ensure it is initialized at most once and has
  *      semantics equivalent to a standard Scala `lazy val`;
  *   1. An [[net.noresttherein.sugar.vars.Idempotent idempotent]] one which uses only memory fences
  *      for synchronization, at the cost of executing the initialization block possibly many times;
  *   1. A [[net.noresttherein.sugar.vars.Transient transient]] one with an opposite behaviour on serialization:
  *      the cached value is `@transient` and becomes discarded when the `Lazy` instance is serialized,
  *      but the initializing block is always retained.
  *
  * @define Ref `Lazy`
  * @author Marcin Mościcki
  */
trait Lazy[@specialized(SpecializedVars) +T] extends (() => T) with Val[T] with Serializable {
	/** Returns `!`[[net.noresttherein.sugar.vars.Val.isEmpty isEmpty]].
	  * Same as [[net.noresttherein.sugar.vars.Lazy.isDefinite isDefinite]].
	  */
	@inline final override def isFinal :Boolean = !isEmpty

	/** Checks if the value has been previously evaluated. Note that `false` values can be stale the moment
	  * they are returned to the caller; the method is however still useful as once `true` is returned, all subsequent
	  * calls on this instance will also return `true` and the value can be safely accessed without an overhead
	  * or risk of throwing an exception. Code
	  * {{{
	  *     if (v.nonEmpty) v.value else someDefaultValue
	  * }}}
	  * is thread safe for all implementations.
	  */
	override def isEmpty :Boolean = opt.isEmpty //overriden for docs only

	/** Returns `true`: all `Lazy` values can have only a single value and, once initialized,
	  * [[net.noresttherein.sugar.vars.Ref.value value]] always returns it.
	  */
	final override def isFinalizable :Boolean = true

	/** Returns `!`[[net.noresttherein.sugar.vars.Val.isEmpty isEmpty]].
	  * Same as [[net.noresttherein.sugar.vars.Lazy.isDefinite isDefinite]].
	  */
	@inline final override def isConst :Boolean = !isEmpty

	/** Always returns `true`.
	  * @see [[net.noresttherein.sugar.vars.Lazy.isDefinite]]
	  */
	@inline final override def isDefined :Boolean = true

	/** Returns ![[net.noresttherein.sugar.vars.Lazy.isEmpty isEmpty]]. */
	@inline final override def isDefinite :Boolean = !isEmpty

	/** The value of this `Lazy`. This method always returns the same value and throws no exception,
	  * but may block in order to avoid initialization. Same as [[net.noresttherein.sugar.vars.Val.const const]].
	  */
	@inline final override def get :T = const

	/** Returns `Some(`[[net.noresttherein.sugar.vars.Lazy.get get]]`)`. */
	override def toOption :Option[T] = Some(get)

	/** Returns `Some(`[[net.noresttherein.sugar.vars.Lazy.get get]]`)`,
	  * same as [[net.noresttherein.sugar.vars.Lazy.toOption toOption]].
	  */
	@inline final override def constOption :Option[T] = toOption

	override def opt :Opt[T] = if (!isEmpty) Got(value) else Lack

	/** Returns [[net.noresttherein.sugar.vars.Opt.Got Got]]`(`[[net.noresttherein.sugar.vars.Lazy.get get]]`)`. */
	@inline final override def toOpt :Opt[T] = opt

	/** Returns [[net.noresttherein.sugar.vars.Opt.Got Got]]`(`[[net.noresttherein.sugar.vars.Lazy.get get]]`)`,
	  * same as [[net.noresttherein.sugar.vars.Lazy.toOpt toOpt]].
	  */
	@inline final override def constOpt :Opt[T] = toOpt

	/** Returns [[net.noresttherein.sugar.vars.Sure Sure]]`(`[[net.noresttherein.sugar.vars.Lazy.get get]]`)`. */
	override def toUnsure :Unsure[T] = Sure(get)

	/** Returns [[net.noresttherein.sugar.vars.Sure Sure]]`(`[[net.noresttherein.sugar.vars.Lazy.get get]]`)`. */
	@inline final override def constUnsure :Unsure[T] = toUnsure

	/** Creates a new `Lazy[O]` instance with the same characteristics as this instance, evaluated to the application
	  * of `f` to the value of this instance. If the value has already been evaluated, created instance will be
	  * eagerly evaluated - use `Lazy(f(this.value))` if you wish for `f` to not be executed before
	  * the method returns under any circumstances. If this instance initializes under a `synchronized` block,
	  * it is guaranteed that it will be evaluated at most once, regardless of which of the two values are
	  * accessed. Created `Lazy[O]` will likewise use the `synchronized` block in that case and `f` will be evaluated
	  * at most once.
	  */
	override def map[O](f :T => O) :Lazy[O] = //implementation only needed because super method is not abstract
		new MappedVal[T, O](this, f) with Lazy[O] {
			override def flatMap[A](ff: O => Lazy[A]): Lazy[A] = Lazy.this.map(f andThen ff andThen (_.get))
		}

	/** Creates a new `Lazy[O]` initialized with the expression `f(this.value)).value`. If this instance is already
	  * evaluated, the function will be applied immediately and its result returned directly. Otherwise a new
	  * `Lazy[O]` with the same synchronization characteristics as this instance will be created, with
	  * `f(this.value)).value` as the initializing expression. If you wish for `f` to not be executed
	  * before the method returns and the returned instance is accessed, use `Lazy(f(this.value).value))`.
	  */
	def flatMap[O](f :T => Lazy[O]) :Lazy[O]

	override def mkString :String = mkString("Lazy")
}




/** A factory of lazy values. */
@SerialVersionUID(ver)
object Lazy {

	/** Creates a wrapper over a lazily initialized value roughly equivalent to a built in `lazy val`.
	  * The differences are:
	  *   - no synchronization penalty for value types, once the value has been initialized;
	  *   - monadic [[net.noresttherein.sugar.vars.Lazy.map map]]
	  *     and [[net.noresttherein.sugar.vars.Lazy.flatMap flatMap]] operations;
	  *   - the initializer expression is freed for garbage collection;
	  *   - [[net.noresttherein.sugar.vars.Lazy.isEmpty isEmpty]] test method.
	  *  The implementation returned by this method uses its synchronized lock to ensure the `init` block
	  *  is called at most once.
	  */
	def apply[@specialized(SpecializedVars) T](init : => T) :Lazy[T] = {
		val initializer = () => init
		new SyncLazyVal(initializer) match {
			case ref if ref.getClass == classOf[SyncLazyVal[Any]] => new SyncLazyRef(initializer)
			case spec => spec
		}
	}

	/** Unlike the default `Lazy(_)` and built in `lazy val`s, this lazy value does not use a `synchronized` block,
	  * yielding a minor performance benefit while still remaining thread safe. It happens
	  * at the cost of possibly evaluating the initialization callback more than once, and concurrent access
	  * of an uninitialized value may return results from different calls. For this reason the initializer should
	  * be a relatively lightweight and '''idempotent''' function. For standard value types, access to an initialized
	  * value incurs no synchronization overhead, although this might be partially offset by unnecessary repeated
	  * execution of the initializer after stale reads.
	  * All testing functions become even less helpful, but at least it is guaranteed that once `isDefined`
	  * returns `true`, it will always be so (in the sense of the java memory 'happens before' relation).
	  */
	@inline def idempotent[@specialized(SpecializedVars) T](idempotent : => T) :Lazy[T] = Idempotent(idempotent)

	/** Similar to [[net.noresttherein.sugar.vars.Lazy.idempotent idempotent]] lazy value, but on serialization,
	  * instead of evaluating the value and freeing the initializer expression for garbage collection,
	  * it does the opposite: the evaluated value is `@transient` and the initializer is never unreferenced,
	  * meaning it will be evaluated again on deserialization if the value is required. This is useful
	  * if the value is large, not serializable, or if it references singleton values (which do not implement
	  * aliasing after deserialization to ensure that only one instance exists as Scala's singleton objects do).
	  * @param idempotent a serializable initializer expression of a SAM type extending `() => T`,
	  *                   allowing the use of literal expressions of the latter form as argument.
	  */
	@inline def transient[@specialized(SpecializedVars) T](idempotent :Eval[T]) :Lazy[T] = Transient[T](idempotent)


	/** A wrapper over a computed value adapting it to the `Lazy` type. */
	def eager[@specialized(SpecializedVars) T](value :T) :Lazy[T] = new EagerLazy(value)


	implicit def unboxLazy[T](l :Lazy[T]) :T = l.const

	implicit def lazyOrdering[V[X] <: Lazy[X], T :Ordering] :Ordering[V[T]] = new RefOrdering[V, T]

	implicit def lazyNumeric[T :Numeric]       :Numeric[Lazy[T]]    = new RefNumeric[Lazy, T] with LazyTypeClass[T]
	implicit def lazyIntegral[T :Integral]     :Integral[Lazy[T]]   = new RefIntegral[Lazy, T] with LazyTypeClass[T]
	implicit def lazyFractional[T :Fractional] :Fractional[Lazy[T]] = new RefFractional[Lazy, T] with LazyTypeClass[T]
	
	private trait LazyTypeClass[T] extends RefNumeric[Lazy, T] {
		override def fromInt(x :Int) :Lazy[T] = Lazy(value.fromInt(x))
		protected override def fmap(x :Lazy[T], y :Lazy[T])(op :(T, T) => T) :Lazy[T] =
			for (xv <- x; yv <- y) yield op(xv, yv)

		protected override def map(x :Lazy[T])(f :T => T) :Lazy[T] = x.map(f)
		protected override def apply(x :T) :Lazy[T] = Lazy.eager(x)
	}



	/** An already computed (initialized) value. */
	@SerialVersionUID(ver) //todo: make it specialized
	//Not specialized to avoid boxing of T to ref-wrapper-of-T, especially that we likely already have the wrapper
	private final class EagerLazy[+T](eager :T) extends Lazy[T] {
		override def isEmpty = false
		override def value :T = eager
		override def const :T = eager

		override def map[O](f :T => O) :EagerLazy[O] = new EagerLazy(f(eager))
		override def flatMap[O](f :T => Lazy[O]) :Lazy[O] = f(eager)

		override def isSpecialized :Boolean = false //getClass == classOf[EagerLazy[_]]
	}


	/** `Lazy` implementation equivalent in semantics to Scala's `lazy val`.
	  * The implementation is not really specialized to avoid boxing during generic access; boxing at initialization
	  * will be likely overshadowed by reads. The implementation assumes that `T` is a value type,
	  * and its runtime reference wrapper is an immutable class
	  */
	@SerialVersionUID(ver) //todo: make it specialized
	private final class SyncLazyVal[@specialized(SpecializedVars) +T](private[this] var initializer : () => T)
		extends Lazy[T]
	{
		private[this] var evaluated :Any = undefined

		override def isEmpty :Boolean = evaluated == undefined

		@unspecialized override def value :T = {
			val res = evaluated
			if (res != undefined) res.asInstanceOf[T] else throw new NoSuchElementException("Uninitialized Lazy")
		}
		@unspecialized override def const :T = {
			var res = evaluated
			if (res == undefined)
				synchronized {
					val init = initializer
					if (init == null)
						res = evaluated
					else {
						res = init()
						evaluated = res
						initializer = null
					}
				}
			res.asInstanceOf[T]
		}

		override def option :Option[T] = {
			val res = evaluated
			if (res == undefined) None else Some(res.asInstanceOf[T])
		}
		override def opt :Opt[T] = {
			val res = evaluated
			if (res == undefined) Lack else Got(res.asInstanceOf[T])
		}
		@unspecialized override def unsure :Unsure[T] = {
			val res = evaluated
			if (res == undefined) Missing else Sure(res.asInstanceOf[T])
		}

		@unspecialized override def map[O](f :T => O) :Lazy[O] = {
			val v = evaluated
			if (v != undefined)
				eager(f(v.asInstanceOf[T]))
			else synchronized {
				val init = initializer
				if (init == null) {
					val t = evaluated.asInstanceOf[T]
					eager(f(t))
				} else
					new SyncLazyRef(() => f(apply()))
			}
		}
		@unspecialized override def flatMap[O](f :T => Lazy[O]) :Lazy[O] = {
			val v = evaluated
			if (v != undefined)
				f(v.asInstanceOf[T])
			else synchronized {
				val init = initializer
				if (init == null)
					f(evaluated.asInstanceOf[T])
				else
					new SyncLazyRef(f(apply()))
			}
		}

		override def isSpecialized = true

		override def mkString(prefix :String) :String = evaluated match {
			case `undefined` => prefix + "()"
			case v => prefix + "(" + v + ")"
		}
		override def mkString :String = evaluated match {
			case `undefined` => "SyncLazy()"
			case v => "SyncLazy(" + v + ")"
		}
		override def toString :String = String.valueOf(evaluated) //can print NoContent.toString, but it's ok

		private def writeReplace = Lazy.eager(apply())
	}



	@SerialVersionUID(ver)
	private final class SyncLazyRef[+T](private[this] var initializer :() => T) extends Lazy[T] {
		@volatile private[this] var evaluated :Any = undefined

		override def isEmpty :Boolean = evaluated == undefined

		override def value :T = evaluated match {
			case `undefined` => throw new NoSuchElementException("Uninitialized Lazy")
			case v => v.asInstanceOf[T]
		}
		override def const :T = {
			var res = evaluated
			if (res == undefined) synchronized {
				res = evaluated
				if (res == undefined) {
					res = initializer()
					evaluated = res
					initializer = null
				}
			}
			res.asInstanceOf[T]
		}

		override def opt :Opt[T] = synchronized {
			val res = evaluated
			if (res == undefined) Lack else Got(res.asInstanceOf[T])
		}

		override def map[O](f :T => O) :Lazy[O] = {
			var v = evaluated
			if (v != undefined)
				eager(f(v.asInstanceOf[T]))
			else synchronized {
				v = evaluated
				if (v != undefined)
					eager(f(v.asInstanceOf[T]))
				else
					new SyncLazyRef(() => f(apply()))
			}
		}
		override def flatMap[O](f :T => Lazy[O]) :Lazy[O] = {
			var v = evaluated
			if (v != undefined) {
				f(v.asInstanceOf[T])
			} else synchronized {
				v = evaluated
				if (v != undefined)
					f(v.asInstanceOf[T])
				else
					new SyncLazyRef(f(apply()))
			}
		}

		override def isSpecialized = false

		override def mkString :String = evaluated match {
			case `undefined` => "SyncLazy()"
			case v => "SyncLazy(" + v + ")"
		}
		override def toString :String = String.valueOf(evaluated) //can print Undefined.toString, but it's ok

		private def writeReplace = Lazy.eager(apply())
	}

}

