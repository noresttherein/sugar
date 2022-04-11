package net.noresttherein.sugar.repeat

import scala.annotation.tailrec

import net.noresttherein.sugar.typist.Rank




/** Extension methods providing syntax for repeating certain blocks of code or recursively composing a function.
  */
trait extensions extends Any {
	/** Adds a `repeat` method to any value, applying the function given as the argument repeatedly to its own
	  * results until the condition passed to a subsequent `until` method evaluates to true.
	  */
	@inline implicit final def repeatMethod[T](self :T) = new RepeatMethod[T](self)

	/** Adds a `times` method to any `Int` for executing a block the given number of times. */
	@inline implicit final def timesMethods[T](self :Int) = new TimesMethods(self)
}




/** Extends any value with method [[net.noresttherein.sugar.repeat.repeatMethod.repeat repeat]], creating
  * a pure functional equivalent of a `repeat ... until` loops:
  * {{{
  *     val kilo = 2 repeat { _ * 2 } until (_ > 1000)
  * }}}
  */
class RepeatMethod[T](private[this] var start :T) {
	/*** Executes the given function repeatedly applying it to the values it returns until the condition
	  * passed to the `until` method of the returned object becomes true.
	  * This instance is used as the initial argument.
	  * {{{
	  *     val kilo = 2 repeat { _ >> 2 } until (_ > 1000)
	  *     println(s"There are $kilo bytes in a kilobyte")
	  * }}}
	  */
	@inline def repeat(expr :T => T) :RepeatUntil[T] =
		new RepeatUntil(() => { start = expr(start); start })
}



/** Represents a range `[0, this)` which defines a fixed number of iterations. */
class TimesMethods(private val count :Int) extends AnyVal {

	/** Execute `f` (code block passed by name) `this` number of times. */
	@inline def times[T](f : =>T) :Unit = for (_ <- 0 until count) f

	/** Apply `f` recursively to its own result `this` number of times, starting with value `start`. */
	@tailrec def times[T](f :T => T)(start :T) :T =
		if (count <= 0) start
		else (count - 1).times(f)(f(start))

	/** Apply `f` recursively to its own result `this` number of times, starting with value `start`. */
	@inline def timesFrom[T](start :T)(f :T => T) :T = count.times(f)(start)

	/** Executes `f` `this` number of times, passing the ordinal number of the `execution` in the sequence
	  * as the argument.
	  * @return a sequence of length `this`, containing the results of each execution of `f`.
	  */
	def enumerate[T](f :Int => T) :Seq[T] = {
		@tailrec def rec(i :Int, acc :List[T]) :List[T] =
			if (i < 0) acc
			else rec(i, f(i) :: acc)
		rec(count - 1, Nil)
	}

	/** Apply `f` to its own result `this` number of times, starting with value `start`.
	  * Equivalent to `this.timesFrom(start)(f)` but helps with type inference.
	  * The name is in analogy to equivalent fold left over a range:
	  * `def pow(x :Int, n:Int) = (1 /: (0 until n)){ (acc, _) => acc * x }` (in pseudo code)
	  * @param start start value for the recursion.
	  * @param f function to recursively apply to itself.
	  * @return `start` if `this<=0` or `f(f(...f(start)))` (composed `this` number of times).
	  * @usecase `(new StringBuilder /: n)(_ += ":)"`
	  */
	@inline def /:[T](start :T)(f :T => T) :T = times(f)(start)
}

