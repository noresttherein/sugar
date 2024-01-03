package net.noresttherein.sugar.vars

import net.noresttherein.sugar.vars.InOut.SpecializedVars


/**
  * @define Ref `LazyUnsure`
  * @define ref unsure lazy value
  * @define coll unsure lazy value
  * @author Marcin Mościcki
  */ //todo:
trait LazyUnsure[@specialized(SpecializedVars) +T] extends Ref[T] {

	/** Creates a new `LazyUnsure[O]` instance with the same characteristics as this instance, evaluated
	  * to the application of `f` to the value of this instance. If the value has already been evaluated,
	  * the created instance will be eagerly evaluated - use `LazyUnsure(f(this.value))` if you wish for `f`
	  * to not be executed before the method returns under any circumstances. If this instance initializes
	  * under a `synchronized` block, it is guaranteed that it will be evaluated at most once, regardless of
	  * which of the two values are accessed. Created `LazyUnsure[O]` will likewise use the `synchronized` block
	  * in that case and `f` will be evaluated at most once.
	  */
	def map[O](f :T => O) :LazyUnsure[O]

	/** Creates a new `LazyUnsure[O]` initialized with the expression `f(this.value)).value`. If this instance is already
	  * evaluated, the function will be applied immediately and its result returned directly. Otherwise a new
	  * `LazyUnsure[O]` with the same synchronization characteristics as this instance will be created, with
	  * `f(this.value)).value` as the initializing expression. If you wish for `f` to not be executed
	  * before the method returns and the returned instance is accessed, use `LazyUnsure(f(this.value).value))`.
	  */
	def flatMap[O](f :T => LazyUnsure[O]) :LazyUnsure[O]

}




@SerialVersionUID(Ver)
object LazyUnsure {

}
