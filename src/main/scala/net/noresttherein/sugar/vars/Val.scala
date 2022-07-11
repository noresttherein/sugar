package net.noresttherein.sugar.vars

import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/** A supertype of wrappers of effectively immutable values with deferred initialization which do not change once set,
  * with all ''successful'' reads returning the same value. The chief implementation class is
  * [[net.noresttherein.sugar.vars.Lazy Lazy]], but the value can be also set explicitly at a later time as in
  * [[net.noresttherein.sugar.vars.Out Out]].
  *
  * One thing in common among all subclasses which differentiate them
  * from the contract of [[net.noresttherein.sugar.vars.Ref Ref]] is that
  * [[net.noresttherein.sugar.vars.Ref.get get]] (and [[net.noresttherein.sugar.vars.Val.apply apply]]`()`
  * will always return a value, and always the same value for the same instance of `Val`.
  * How and when that value is computed is unspecified, and optional methods such as
  * [[net.noresttherein.sugar.vars.Ref.opt opt]], [[net.noresttherein.sugar.vars.Ref.? ?]]
  * and [[net.noresttherein.sugar.vars.Ref.unsure asUnsure]] are allowed to return empty instances
  * if the value is not available at the moment of calling.
  *
  * @author Marcin Mościcki
  */ //consider: caching of the T wrapper, Opt, Option, Unsure
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
	  * Note that some `Val` implementations extend also [[net.noresttherein.sugar.vars.InOut InOut]],
	  * splitting their life cycle into two phases: mutable initialization and immutable final phase.
	  * In that case `this.`[[net.noresttherein.sugar.vars.InOut.value value]] has different semantics
	  * from this method, always returning the current value. Specifics depend on implementation.
	  */ //redeclaration from Ref solely for a documentation change.
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
