package net.noresttherein.slang.vars

import net.noresttherein.slang.optional.Opt
import net.noresttherein.slang.optional.Opt.Got
import net.noresttherein.slang.vars.InOut.SpecializedVars




/** A super type of both [[net.noresttherein.slang.vars.InOut InOut]] and [[net.noresttherein.slang.vars.Val Val]]. */
trait Ref[@specialized(SpecializedVars) +T] extends (() => T) {
	/** The wrapped value, if available. Depending on the actual implementation,
	  * it can throw a [[NoSuchElementException]].
	  */
	def apply() :T

	/** The value of this instance, if it is available. Lazily initialized objects (containing their initializers)
	  * will proceed with the initialization if necessary, but subclasses which require an eternal setting of the value
	  * will return [[None]].
	  */
	def get :Option[T] = opt

	/** The value of this instance, if it is available. Lazily initialized objects (containing their initializers)
	  * will proceed with the initialization if necessary, but subclasses which require an eternal setting of the value
	  * will return [[net.noresttherein.slang.optional.Opt.Lack Lack]].
	  */
	def opt :Opt[T]

	/** `true` if there is a value associated with this instance. Depending on the implementation,
	  * this information may be already incorrect when returned to the application. Unless the specific type of this
	  * `ValRef` is known and its contract guarantees that any particular answer is guaranteed to remain true,
	  * prefer polling with [[net.noresttherein.slang.vars.Ref.get get]]/[[net.noresttherein.slang.vars.Ref.opt opt]].
	  */
	def isDefined :Boolean = opt.isDefined

	def canEqual(that :Any) :Boolean = that.isInstanceOf[Ref[_]]
}



object Ref {
	@inline def unapply[T](ref :Ref[T]) :Opt[T] = ref.opt


	private[vars] object Undefined {
		override def toString = "<undefined>"
	}
}



/** A supertype of wrappers of effectively immutable values with deferred initialization which do not change once set,
  * with all ''successful'' reads returning the same value. The chief implementation class is
  * [[net.noresttherein.slang.vars.Lazy Lazy]], but the value can be also set explicitly at a later time as in
  * [[net.noresttherein.slang.vars.Out Out]].
  * @author Marcin Mościcki
  */
trait Val[@specialized(SpecializedVars) +T] extends Ref[T] {
	/** The wrapped value, if available. Depending on the actual implementation,
	  * it can throw a [[NoSuchElementException]]. This method will always return the same value:
	  * once it returns without throwing an exception, subsequent calls will yield the same result.
	  * Note that some `Val` implementations extend also [[net.noresttherein.slang.vars.InOut InOut]],
	  * splitting their life cycle into two phases: mutable initialization and immutable final phase.
	  * In that case `this.`[[net.noresttherein.slang.vars.InOut.value value]] has different semantics
	  * from this method, always returning the current value. Specifics depend on implementation.
	  */
	def apply() :T

	/** Checks if this object contains a value. This can happen either through lazy initialization or explicit
	  * setting, depending on the implementation. Note that `false` values can be stale the moment
	  * it is returned to the caller; the method is however still useful as once `true` is returned, all subsequent
	  * calls ''on this instance'' will also return `true` and the value can be safely accessed without an overhead
	  * or risk of throwing an exception.
	  */
	override def isDefined :Boolean = opt.isDefined

//	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Val[_]]
	override def hashCode :Int = if (!isDefined) 0 else cachedHash
	private lazy val cachedHash = apply().hashCode
}




object Val {
	/** A wrapper over truly immutable, eagerly initialized value. Usually not very useful in the application code,
	  * but methods which work on lazy values can in some cases return such an instance if the argument
	  * is already initialized.
	  */
	def apply[T](value :T) :Val[T] = Lazy.eager(value)

	/** Matches any `Val` subclass, returning the wrapped value.
	  */
	def unapply[T](value :Val[T]) :Opt[T] = Got(value())
}
