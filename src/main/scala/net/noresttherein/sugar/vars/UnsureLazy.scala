package net.noresttherein.sugar.vars

import net.noresttherein.sugar.vars.InOut.SpecializedVars


/**
  * @author Marcin Mościcki
  */
//consider: do we need it? Can be modeled by Unsure[Opt[T]]. We'd need a complete new set of 'optional' methods
//  returning the evaluated value, although that would be already useful for Lazy.
trait UnsureLazy[@specialized(SpecializedVars) +T] extends Ref[T] {

	/** Creates a new `UnsureLazy[O]` instance with the same characteristics as this instance, evaluated
	  * to the application of `f` to the value of this instance. If the value has already been evaluated,
	  * the created instance will be eagerly evaluated - use `UnsureLazy(f(this.value))` if you wish for `f`
	  * to not be executed before the method returns under any circumstances. If this instance initializes
	  * under a `synchronized` block, it is guaranteed that it will be evaluated at most once, regardless of
	  * which of the two values are accessed. Created `UnsureLazy[O]` will likewise use the `synchronized` block
	  * in that case and `f` will be evaluated at most once.
	  */
	def map[O](f :T => O) :UnsureLazy[O]

	/** Creates a new `UnsureLazy[O]` initialized with the expression `f(this.value)).value`. If this instance is already
	  * evaluated, the function will be applied immediately and its result returned directly. Otherwise a new
	  * `UnsureLazy[O]` with the same synchronization characteristics as this instance will be created, with
	  * `f(this.value)).value` as the initializing expression. If you wish for `f` to not be executed
	  * before the method returns and the returned instance is accessed, use `UnsureLazy(f(this.value).value))`.
	  */
	def flatMap[O](f :T => UnsureLazy[O]) :UnsureLazy[O]

}




object UnsureLazy {

}
