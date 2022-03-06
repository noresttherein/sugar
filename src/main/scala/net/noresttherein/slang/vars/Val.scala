package net.noresttherein.slang.vars

import Opt.{some_?, Got, Lack}
import net.noresttherein.slang.vars.InOut.SpecializedVars




/** The root type of [[net.noresttherein.slang.vars.InOut InOut]] and [[net.noresttherein.slang.vars.Val Val]]
  * class hierarchy of value wrappers with various implementations of referential variables, lazy values and others.
  */
trait Ref[@specialized(SpecializedVars) +T] extends Any with Equals {

	/** Checks if this object currently contains a value. In a non concurrent (or synchronized) context,
	  * if `true`, then [[net.noresttherein.slang.vars.Ref.get get]] will not block and return a value
	  * without throwing an exception. Depending on the implementation, this information
	  * may be already incorrect when returned to the application. Unless the specific type of this `Ref` is known
	  * and its contract guarantees that any particular answer is guaranteed to remain true, prefer polling
	  * with [[net.noresttherein.slang.vars.Ref.asOption asOption]]/[[net.noresttherein.slang.vars.Ref.opt opt]].
	  */
	def isDefined :Boolean// = opt.isDefined

	/** The wrapped value, if available. Depending on the actual implementation,
	  * it can throw a [[NoSuchElementException]]. This is an alias for [[net.noresttherein.slang.vars.Ref.get get]].
	  */
	@inline final def apply() :T = get

	/** The wrapped value, if available. Depending on the actual implementation,
	  * it can throw a [[NoSuchElementException]]. It is equivalent
	  * to [[net.noresttherein.slang.vars.Ref!.apply apply]]`()`, but can be more readable if the returned value
	  * is a function (or other object defining an `apply` method) which is being immediately applied.
	  * Some subclasses define also a `value` property which also accesses the value of this instance,
	  * but its semantics may differ, depending on the exact implementation.
	  */
	def get :T

	/** The value of this instance, if it is available. This is an alias for [[net.noresttherein.slang.vars.Ref.? ?]]. */
	def asOption :Option[T] = opt.asOption //todo: switch delegation order with ?

	/** The value of this instance, if it is available. Lazily initialized objects (containing their initializers)
	  * will proceed with the initialization if necessary, but subclasses which require external setting of the value
	  * will return [[None]].
	  * This method can block to wait for another thread only if initialization is currently in progress/the value
	  * is currently mutated, but not if no other thread currently accesses this variable.
	  */
	@inline final def ? :Option[T] = asOption

	/** The value of this instance, if it is available. Lazily initialized objects (containing their initializers)
	  * will proceed with the initialization if necessary, but subclasses which require external setting of the value
	  * will return [[net.noresttherein.slang.vars.Opt.Lack Lack]].
	  * This method can block to wait for another thread only if initialization is currently in progress/the value
	  * is currently mutated, but not if no other thread currently accesses this variable.
	  */
	def opt :Opt[T]

	/** The value of this instance, if it is available, as an instance of a specialized, option-like `Shot`.
	  * Lazily initialized objects (containing their initializers) will proceed with the initialization if necessary,
	  * but subclasses which require external setting of the value will return [[net.noresttherein.slang.vars.Miss Miss]].
	  * This method can block to wait for another thread only if initialization is currently in progress/the value
	  * is currently mutated, but not if no other thread currently accesses this variable.
	  */
	def asShot :Shot[T] = opt.asShot

	/** True if the content type is known to be a value type and the class is specialized for it.
	  * Signifies that usage of [[net.noresttherein.slang.vars.Shot Shot]] is preferable
	  * over [[net.noresttherein.slang.vars.Opt Opt]]. Should not be relied upon for anything critical
	  * and code should work correctly if this method returns a false result, in particular
	  * a false negative (the default return value being `false`).
	  */
	private[vars] def isSpecialized :Boolean = false

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Ref[_]]
}



object Ref {
	@inline def unapply[T](ref :Ref[T]) :Opt[T] = ref.opt

//	class RefOrdering[T](implicit content :Ordering[T]) extends Ordering[Ref[T]] {
//		override def compare(x :Ref[T], y :Ref[T]) :Int = content.compare(x.value, y.value)
//	}
//
//	implicit def RefOrdering[T :Ordering] = new RefOrdering[T]

	/** A marker object used by some implementations to signify that a reference has no actual value. */
	private[vars] object Undefined {
		override def toString = "<undefined>"
	}

}






/** A supertype of wrappers of effectively immutable values with deferred initialization which do not change once set,
  * with all ''successful'' reads returning the same value. The chief implementation class is
  * [[net.noresttherein.slang.vars.Lazy Lazy]], but the value can be also set explicitly at a later time as in
  * [[net.noresttherein.slang.vars.Out Out]].
  *
  * One thing in common among all subclasses which differentiate them
  * from the contract of [[net.noresttherein.slang.vars.Ref Ref]] is that
  * [[net.noresttherein.slang.vars.Ref.get get]] (and [[net.noresttherein.slang.vars.Val.apply apply]]`()`
  * will always return a value, and always the same value for the same instance of `Val`.
  * How and when that value is computed is unspecified, and optional methods such as
  * [[net.noresttherein.slang.vars.Ref.opt opt]], [[net.noresttherein.slang.vars.Ref.? ?]]
  * and [[net.noresttherein.slang.vars.Ref.asShot asShot]] are allowed to return empty instances
  * if the value is not available at the moment of calling.
  *
  * @author Marcin Mościcki
  */ //consider: caching of the T wrapper, Opt, Option, Shot
trait Val[@specialized(SpecializedVars) +T] extends Ref[T] with (() => T) {
	/** Checks if this object currently contains a value. This can happen either through lazy initialization or explicit
	  * assignment, depending on the implementation. If `true`, then `get` will return the value
	  * without blocking or throwing a [[NoSuchElementException]]. Note however that `false` values can become stale
	  * the moment it is returned to the caller; the method is however still useful, as once `true` is returned,
	  * all subsequent calls will also return `true`, and the value can be safely accessed without an overhead
	  * or risk of throwing an exception. Code
	  * {{{
	  *     if (v.isDefined) v.get else someDefaultValue
	  * }}}
	  * is thread safe for all implementations.
	  */
	override def isDefined :Boolean// = opt.isDefined

	/** The wrapped value, if available. Depending on the actual implementation,
	  * it can throw a [[NoSuchElementException]]. This method will always return the same value:
	  * once it returns without throwing an exception, subsequent calls will yield the same result.
	  * Note that some `Val` implementations extend also [[net.noresttherein.slang.vars.InOut InOut]],
	  * splitting their life cycle into two phases: mutable initialization and immutable final phase.
	  * In that case `this.`[[net.noresttherein.slang.vars.InOut.value value]] has different semantics
	  * from this method, always returning the current value. Specifics depend on implementation.
	  */
	def apply() :T

	override def opt :Opt[T] = if (isDefined) Got(get) else Lack

	/** Compares the evaluated values for equality, blocking if either of the instances is not yet initialized. */
	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :Val[_] if other canEqual this => get == other.get
//		case other :Hit[_] => get == other.get
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Val[_]]
	override def hashCode :Int = cachedHash
	private lazy val cachedHash = apply().hashCode
}



object Val {
	/** A wrapper over truly immutable, eagerly initialized value. Usually not very useful in the application code,
	  * but methods which work on lazy values can in some cases return such an instance if the argument
	  * is already initialized.
	  */
	def apply[T](value :T) :Val[T] = Lazy.eager(value)

	/** Matches any `Val` subclass, returning the wrapped value. */
	def unapply[T](value :Val[T]) :Val[T] = value

	/** Matches any `Val` subclass, returning the wrapped value. */
	def unapply[T](value :Ref[T]) :Opt[T] = value.opt //is opt thread safe in all implementations?

}
