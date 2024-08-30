package net.noresttherein.sugar.vars

import java.io.ObjectOutputStream

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.unspecialized

import net.noresttherein.sugar.concurrent.Fences.{acquireFence, releaseFence}
import net.noresttherein.sugar.noSuch_!
import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Maybe.{No, Yes}
import net.noresttherein.sugar.vars.Opt.One
import net.noresttherein.sugar.vars.Ref.{RefFractional, RefIntegral, RefNumeric, RefOrdering, undefined}




/** A monadic lazy value. While Scala `lazy val` is a better choice in general, and has a lesser memory footprint,
  * this class, and especially its subclasses, has some advantages over it:
  *   1. They don't incur any synchronization penalty once the value is initialized for built in value types.
  *   1. They allow to check its initialization state (which in turn allows for example to discard `Lazy` and use
  *      the value directly, or choose a different method of initializing some other value).
  *   1. They provide monadic operations for constructing other `Lazy` instances.
  *   1. Can be stored in collections and passed as arguments without automatically including the whole outer object
  *      in the closure (for class fields).
  *   1. [[net.noresttherein.sugar.vars.Pure pure]] offers better performance in case of many contentious
  *      reads.
  *   1. [[net.noresttherein.sugar.vars.Transient transient]] discards the computed value on serialization.
  *
  * Three implementations exist:
  *   1. The default one which uses a synchronized block to ensure it is initialized at most once and has
  *      semantics equivalent to a standard Scala `lazy val`;
  *   1. A [[net.noresttherein.sugar.vars.Pure pure]] one which uses only memory fences
  *      for synchronization, at the cost of executing the initialization block possibly many times;
  *   1. A [[net.noresttherein.sugar.vars.Transient transient]] one with an opposite behaviour on serialization:
  *      the cached value is `@transient` and becomes discarded when the `Lazy` instance is serialized,
  *      but the initializing block is always retained.
  *
  * @define Ref `Lazy`
  * @define ref lazy value
  * @author Marcin Mościcki
  */
trait Lazy[@specialized(SpecializedVars) +T] extends (() => T) with Val[T] with Serializable {
	/** An alias for [[net.noresttherein.sugar.vars.Lazy.isFinal isFinal]]/[[net.noresttherein.sugar.vars.Lazy.isDefinite isDefinite]]
	  * with a more intuitive name.
	  */
	@inline final def isEvaluated :Boolean = isDefinite

	/** True, if this value is already evaluated.
	  * @return [[net.noresttherein.sugar.vars.Lazy.isDefinite isDefinite]].
	  */
	@inline final override def isFinal :Boolean = isDefinite

	/** True if the value is not evaluated yet.
	  * @return `!`[[net.noresttherein.sugar.vars.Lazy.isDefinite isDefinite]].
	  */
	@inline final override def isEmpty :Boolean = !isDefinite //overridden for docs only

	/** Returns `true`: all `Lazy` values can have only a single value and, once initialized,
	  * [[net.noresttherein.sugar.vars.Ref.value value]] always returns it.
	  */
	@inline final override def isFinalizable :Boolean = true

	/** Returns [[net.noresttherein.sugar.vars.Val.isDefinite isDefinite]]. */
	@inline final override def isConst :Boolean = isDefinite

	/** Always returns `true`.
	  * @see [[net.noresttherein.sugar.vars.Lazy.isDefinite isDefinite]]
	  */
	@inline final override def isDefined :Boolean = true

	/** Checks if the value has been previously evaluated. Note that `false` values can be stale the moment
	  * they are returned to the caller; the method is however still useful as once `true` is returned, all subsequent
	  * calls on this instance will also return `true` and the value can be safely accessed without an overhead
	  * or risk of throwing an exception. Code
	  * {{{
	  *     if (v.isDefinite) v.value else someDefaultValue
	  * }}}
	  * is thread safe for all implementations.
	  */
	override def isDefinite :Boolean //= opt.nonEmpty

	/** The value of this $Ref. This method always returns the same value and throws no exception,
	  * but may block in order to avoid initialization. Same as [[net.noresttherein.sugar.vars.Val.const const]].
	  */
	override def get :T

	/** The value of this $Ref. This method always returns the same value and throws no exception,
	  * but may block in order to avoid initialization. Same as [[net.noresttherein.sugar.vars.Val.get get]].
	  */
	@inline final override def const :T = get

	/** Returns `Some(`[[net.noresttherein.sugar.vars.Lazy.get get]]`)`. */
	override def toOption :Option[T] = Some(get)

	/** Returns `Some(`[[net.noresttherein.sugar.vars.Lazy.get get]]`)`,
	  * same as [[net.noresttherein.sugar.vars.Lazy.toOption toOption]].
	  */
	@inline final override def constOption :Option[T] = toOption

	override def opt :Opt[T] = if (!isEmpty) One(value) else None

	/** Returns [[net.noresttherein.sugar.vars.Opt.One One]]`(`[[net.noresttherein.sugar.vars.Lazy.get get]]`)`. */
	@inline final override def toOpt :Opt[T] = One(get)

	/** Returns [[net.noresttherein.sugar.vars.Opt.One One]]`(`[[net.noresttherein.sugar.vars.Lazy.get get]]`)`,
	  * same as [[net.noresttherein.sugar.vars.Lazy.toOpt toOpt]].
	  */
	@inline final override def constOpt :Opt[T] = One(get)

	/** Returns [[net.noresttherein.sugar.vars.Sure Sure]]`(`[[net.noresttherein.sugar.vars.Lazy.get get]]`)`. */
	override def toUnsure :Unsure[T] = Sure(get)

	/** Returns [[net.noresttherein.sugar.vars.Sure Sure]]`(`[[net.noresttherein.sugar.vars.Lazy.get get]]`)`. */
	@inline final override def unsureConst :Unsure[T] = toUnsure

	/** Creates a new $Ref`[O]` instance with the same characteristics as this instance, evaluated to the application
	  * of `f` to the value of this instance. If the value has already been evaluated, created instance will be
	  * eagerly evaluated - use $Ref`(f(this.value))` if you wish for `f` to not be executed before
	  * the method returns under any circumstances. If this instance initializes under a `synchronized` block,
	  * it is guaranteed that it will be evaluated at most once, regardless of which of the two values are
	  * accessed. Created `Lazy[O]` will likewise use the `synchronized` block in that case and `f` will be evaluated
	  * at most once.
	  */
	override def map[O](f :T => O) :Lazy[O] =
		new MappedVal[T, O](this, f) with Lazy[O]

	/** Creates a new $Ref`[O]` initialized with the expression `f(this.value)).value`. If this instance is already
	  * evaluated, the function will be applied immediately and its result returned directly. Otherwise a new
	  * $Ref`[O]` with the same synchronization characteristics as this instance will be created, with
	  * `f(this.value)).value` as the initializing expression. If you wish for `f` to not be executed
	  * before the method returns and the returned instance is accessed, use $Ref`(f(this.value).value))`.
	  */
	def flatMap[O](f :T => Lazy[O]) :Lazy[O] =
		new FlatMappedVal[T, O](this, f) with Lazy[O]

	override def mkString :String = mkString("Lazy")

	override def toString :String = maybe match {
		case Yes(v) => String.valueOf(v)
		case _      => "<uninitialized>"
	}
}




/** A factory of lazy values. */
@SerialVersionUID(Ver)
case object Lazy {

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
	@inline def apply[@specialized(SpecializedVars) T](init : => T) :Lazy[T] = from(() => init)

	/** Same as [[net.noresttherein.sugar.vars.Lazy$.apply apply]], but accepts a `Function0`,
	  * rather a by-name parameter.
	  */
	def from[@specialized(SpecializedVars) T](init: () => T) :Lazy[T] =
		new SyncLazyVal[T] match {
			case ref if ref.getClass == classOf[SyncLazyVal[Any]] => new SyncLazyRef(init)
			case spec => spec.init(init); spec
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
	  * @see [[net.noresttherein.sugar.vars.Pure! Pure]]
	  * @see [[net.noresttherein.sugar.vars.Pure$.apply Pure]]`(idempotent)`
	  */
	@inline def idempotent[@specialized(SpecializedVars) T](idempotent : => T) :Lazy[T] = Pure(idempotent)

	/** Similar to [[net.noresttherein.sugar.vars.Lazy.idempotent idempotent]] lazy value, but on serialization,
	  * instead of evaluating the value and freeing the initializer expression for garbage collection,
	  * it does the opposite: the evaluated value is `@transient` and the initializer is never unreferenced,
	  * meaning it will be evaluated again on deserialization if the value is required. This is useful
	  * if the value is large, not serializable, or if it references singleton values (which do not implement
	  * aliasing after deserialization to ensure that only one instance exists as Scala's singleton objects do).
	  * @param idempotent a serializable initializer expression of a SAM type extending `() => T`,
	  *                   allowing the use of literal expressions of the latter form as argument.
	  * @see [[net.noresttherein.sugar.vars.Transient! Transient]]
	  * @see [[net.noresttherein.sugar.vars.Transient$.apply Transient]]`(idempotent)`
	  */
	@inline def transient[@specialized(SpecializedVars) T](idempotent :Eval[T]) :Lazy[T] = Transient[T](idempotent)


	/** A wrapper over a computed value adapting it to the `Lazy` type. */
	def eager[@specialized(SpecializedVars) T](value :T) :Lazy[T] = new Eager(value)


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
	@SerialVersionUID(Ver) //todo: make it specialized
	//Not specialized to avoid boxing of T to ref-wrapper-of-T, especially that we likely already have the wrapper
	private final class Eager[+T](override val get :T) extends Const[T] with Lazy[T] {
		override def map[O](f :T => O) :Eager[O] = new Eager(f(get))
		override def flatMap[O](f :T => Lazy[O]) :Lazy[O] = f(get)
		override def isSpecialized :Boolean = false //getClass == classOf[Eager[_]]
	}

}






/** `Lazy` implementation equivalent in semantics to Scala's `lazy val`.
  * The implementation is not really specialized to avoid boxing during generic access; boxing at initialization
  * will be likely overshadowed by reads. The implementation assumes that `T` is a value type,
  * and its runtime reference wrapper is an immutable class.
  */
@SerialVersionUID(Ver)
private final class SyncLazyVal[@specialized(SpecializedVars) +T] extends Lazy[T] {
	private[this] var initializer :() => T = _
	private[this] var evaluated :Any = undefined

	def init(init :() => T @uncheckedVariance) :Unit = initializer = init

	override def isDefinite :Boolean = evaluated != undefined

	@unspecialized override def value :T = {
		val res = evaluated
		if (res != undefined) res.asInstanceOf[T]
		else noSuch_!("Uninitialized Lazy")
	}
	@unspecialized override def get :T = {
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
		if (res == undefined) None else One(res.asInstanceOf[T])
	}
	@unspecialized override def unsure :Unsure[T] = {
		val res = evaluated
		if (res == undefined) Missing else Sure(res.asInstanceOf[T])
	}

	@unspecialized override def map[O](f :T => O) :Lazy[O] = {
		val v = evaluated
		if (v != undefined)
			Lazy.eager(f(v.asInstanceOf[T]))
		else synchronized {
			val init = initializer
			if (init == null) {
				val t = evaluated.asInstanceOf[T]
				Lazy.eager(f(t))
			} else
				new SyncLazyRef(() => f(get))
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
				new SyncLazyRef(f(get))
		}
	}

	override def isSpecialized = true

	private def writeReplace :AnyRef = Lazy.eager(get)
}



@SerialVersionUID(Ver)
private[sugar] class SyncLazyRef[+T](private[this] var initializer :() => T) extends Lazy[T] {
	@volatile private[this] var evaluated :Any = undefined

	final override def isDefinite :Boolean = evaluated != undefined

	final override def value :T = evaluated match {
		case `undefined` => noSuch_!("Uninitialized Lazy")
		case v         => v.asInstanceOf[T]
	}
	final override def get :T = {
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

	final override def opt :Opt[T] = synchronized {
		val res = evaluated
		if (res == undefined) None else One(res.asInstanceOf[T])
	}

	override def map[O](f :T => O) :Lazy[O] = {
		var v = evaluated
		if (v != undefined)
			Lazy.eager(f(v.asInstanceOf[T]))
		else synchronized {
			v = evaluated
			if (v != undefined)
				Lazy.eager(f(v.asInstanceOf[T]))
			else
				new SyncLazyRef(() => f(get))
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
				new SyncLazyRef(f(get))
		}
	}

	override def isSpecialized = false

	private def writeReplace :AnyRef = Lazy.eager(get)
}






/** A mix-in trait for application classes with a full lazy field implementation, in particular for various lazy proxies.
  * It closely resembles [[net.noresttherein.sugar.vars.Lazy Lazy]], but does not extend any trait
  * or expose public methods, in order to not burden extending classes with unnecessary, potentially conflicting, API.
  * For this reason all methods are protected, with subclasses having full control over what API they want to expose.
  * @see [[net.noresttherein.sugar.vars.AbstractLazy AbstractLazy]]
  * @see [[net.noresttherein.sugar.vars.AbstractPure AbstractPure]]
  */ //consider: dropping specialization.
sealed trait LazyOps[@specialized(SpecializedVars) +T] {
	/** The lazy expression providing the value for the lazy field. */
	//todo: migrate to using a single Term field
	@transient protected[this] var initializer :() => T
	private[this] var evaluated :T = _

	/** True if the lazy field is initialized. */
	@inline protected final def isDefinite: Boolean = { val init = initializer; acquireFence(); init == null }

	/** Returns `Yes(value)` if the expression has already been evaluated, or `No` otherwise. */
	protected def ? :Maybe[T] = //Not opt, because of risk of conflicts.
		if (initializer != null)
			No
		else {
			acquireFence()
			Yes(evaluated)
		}
	/** Returns `Sure(value)` if the expression has already been evaluated, or `Missing` otherwise. */
	protected def unsure :Unsure[T] =
		if (initializer != null)
			Missing
		else {
			acquireFence()
			Sure(evaluated)
		}

	/** Returns the value of this expression, or throws `NoSuchElementException`, if it has not yet been evaluated. */
	protected def indefinite :T =
		if (initializer == null) {
			acquireFence()
			evaluated
		} else
			noSuch_!("Uninitialized " + this)

	/** Returns the value of this expression, evaluating it, if needed. */
	protected def definite :T = {
		val init = initializer
		acquireFence()
		if (init == null)
			evaluated
		else {
			val res = init()
			evaluated = res
			releaseFence()
			initializer = null
			res
		}
	}

	protected def init(v :T @uncheckedVariance) :Unit = {
		evaluated = v
		initializer = null
	}

	private def writeObject(out :ObjectOutputStream) :Unit = {
		definite
		out.defaultWriteObject()
	}
}



/** A trait with a lazy field, initialized under this object's monitor.
  * It is very similar to simply declaring a `private lazy val` in a class, but it additionally provides protected API
  * allowing to check initialization state. Extending classes must implement `protected[this] var initializer :() => T`.
  * The initialized field value can be accessed using [[net.noresttherein.sugar.vars.LazyOps.definite definite]]
  * property, and [[net.noresttherein.sugar.vars.LazyOps.isDefinite isDefinite]] returning `false` means the field
  * is already initialized, and `definite` will simply return its value, without invoking `initializer`
  * (which is set to `null` after initialization). The constructor is guaranteed to be invoked at most once.
  *
  * This utility class is useful for implementing lazy proxies implementing interface `T`, whose methods should unwrap
  * it to the underlying lazy property if it is already initialized.
  */
trait AbstractLazy[@specialized(SpecializedVars) +T] extends LazyOps[T] {
	protected override def definite :T = {
		if (isDefinite)
			indefinite
		else synchronized {
			super.definite
		}
	}
}



/** A full [[net.noresttherein.sugar.vars.Pure Pure]]-like lazy value implementation as a mix-in trait
  * for application classes, in particular various lazy proxies. Does not implement any interface
  * in order to not burden extending classes with unnecessary, and potentially conflicting, API.
  * For this reason all methods are protected, with subclasses having full control over what API they want to expose.
  */
trait AbstractPure[@specialized(SpecializedVars) +T] extends LazyOps[T]

