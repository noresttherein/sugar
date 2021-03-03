package net.noresttherein.slang.vars

import net.noresttherein.slang.optional.Opt
import net.noresttherein.slang.optional.Opt.Got
import net.noresttherein.slang.vars.InOut.SpecializedVars




/** A super type of both [[net.noresttherein.slang.vars.InOut InOut]] and [[net.noresttherein.slang.vars.Val Val]]. */
trait ValueRef[@specialized(SpecializedVars) +T] {
	/** The evaluated value of this instance. */
	@inline final def get :T = value

	/** The value of this instance. */
	def value :T

}



/** A supertype of wrappers of effectively immutable values with deferred initialization which do not change once set,
  * with all ''successful'' reads returning the same value. The chief implementation class is
  * [[net.noresttherein.slang.vars.Lazy Lazy]], but the value can be also set explicitly at a later time as in
  * [[net.noresttherein.slang.vars.Out Out]].
  * @author Marcin Mościcki
  */
trait Val[@specialized(SpecializedVars) +T] extends ValueRef[T] {
	/** The evaluated value of this instance. */
	def value :T

	/** The value of this instance, if it is available. Lazily initialized objects (containing their initializers)
	  * will proceed with the initialization if necessary, but subclasses which require an eternal setting of the value
	  * will return [[net.noresttherein.slang.optional.Opt.Lack Lack]].
	  */
	def toOpt :Opt[T]

	/** The value of this instance, if it is available. Lazily initialized objects (containing their initializers)
	  * will proceed with the initialization if necessary, but subclasses which require an eternal setting of the value
	  * will return [[None]].
	  */
	def toOption :Option[T] = toOpt

	/** Checks if this object contains a value. This can happen either through lazy initialization or explicit
	  * setting, depending on the implementation. Note that `false` values can be stale the moment
	  * it is returned to the caller; the method is however still useful as once `true` is returned, all subsequent
	  * calls ''on this instance'' will also return `true` and the value can be safely accessed without an overhead
	  * or risk of throwing an exception.
	  */
	def isInitialized :Boolean
}




object Val {
	/** A wrapper over truly immutable, eagerly initialized value. Usually not very useful in the application code,
	  * but methods which work on lazy values can in some cases return such an instance if the argument
	  * is already initialized.
	  */
	def apply[T](value :T) :Val[T] = Lazy.eager(value)

	/** Matches any `Val` subclass, returning the wrapped value.
	  */
	def unapply[T](value :Val[T]) :Opt[T] = Got(value.get)
}
