package net.noresttherein.sugar.repeat

import scala.annotation.tailrec

import net.noresttherein.sugar.illegalState_!
import net.noresttherein.sugar.repeat.extensions.{repeatMethods, timesMethods}
import net.noresttherein.sugar.typist.Rank




/** Extension methods providing syntax for repeating certain blocks of code or recursively composing a function.
  */
trait extensions extends Any {
	/** Adds a `repeat` method to any value, applying the function given as the argument repeatedly to its own
	  * results until the condition passed to a subsequent `until` method evaluates to true.
	  */
	@inline implicit final def repeatMethods[T](self :T) :repeatMethods[T] = new repeatMethods[T](self)

	/** Adds a `times` method to any `Int` for executing a block the given number of times. */
	@inline implicit final def timesMethods[T](self :Int) :timesMethods = new timesMethods(self)
}




@SerialVersionUID(Ver)
object extensions extends extensions {
	/** Extends any value with method [[net.noresttherein.sugar.repeat.extensions.repeatMethods.repeat repeat]],
	  * creating a pure functional equivalent of a `repeat ... until` loops:
	  * {{{
	  *     val kilo = 2 repeat { _ * 2 } until (_ > 1000)
	  * }}}
	  */
	class repeatMethods[T](private val start :T) extends AnyVal {
		/** Executes the given function repeatedly applying it to the values it returns until the condition
		  * passed to [[net.noresttherein.sugar.repeat.RepeatBlock.until until]] method of the returned object
		  * becomes true. The function will be executed at least once and this instance is used as the initial argument.
		  * {{{
		  *     val kilo = 1 repeat { _ * 2 } until (_ > 1000)
		  *     println(s"There are $kilo bytes in a kilobyte")
		  * }}}
		  */
		@inline def repeat(f :T => T) :RepeatFunction[T] =
			new RepeatFunction(start, f)

		/** Executes the given function repeatedly applying it to previously returned values, returning the number
		  * of iterations needed to satisfy the condition passed to
		  * passed to [[net.noresttherein.sugar.repeat.CountingBlock.until until]] becomes true.
		  * The function will be executed at least once and this instance is used as the initial argument.
		  * {{{
		  *     val exponent = 1 repeat { _ * 2 } until (_ > 1000)
		  *     println(s"There are 2^$kilo bytes in a kilobyte")
		  * }}}
		  */
		@inline def count(f :T => T) :CountingBlock[T] =
			new CountingBlock(new RecursionBlock(start, f))

		/** Reapplies the given function repeatedly, to `this` at the beginning,
		  * and then to the previously returned result, until it returns a value satisfying the predicate given
		  * as the first argument. If `pred(this)`, then `this` is returned immediately.
		  * Note that this is different than
		  * {{{
		  *     this repeat f until pred
		  * }}}
		  * because this method will not execute the function at all if this value satisfies the predicate,
		  * while in the example above the function will be executed at least once.
		  */
		@inline def repeatUntil(pred :T => Boolean)(f :T => T) :T = {
			var res = start
			while (!pred(res))
				res = f(res)
			res
		}

		/** Reapplies the given function repeatedly to its own returned values, starting with `this`, for as long
		  * as the predicate is not satisfied. If the predicate is not satisfied for `this`, an [[IllegalStateException]]
		  * is thrown.
		  * @return the last returned value, for which `pred` was satisfied (the argument to the last application of `f`).
		  */
		def repeatWhile(pred :T => Boolean)(f :T => T) :T = {
			if (!pred(start))
				illegalState_!("Predicate " + pred + " not satisfied for " + start + ".")
			var last = start
			var next = f(last)
			while (!pred(next)) {
				last = next
				next = f(last)
			}
			last
		}

		/** Returns the number of times that
		  * `this.`[[net.noresttherein.sugar.repeat.repeatMethod.repeatUntil repeatUntil]]`(pred)(f)` would execute
		  * function `f`.
		  */
		@inline def countUntil(pred :T => Boolean)(f :T => T) :Int = {
			var arg = start
			var count = 0
			while (!pred(arg)) {
				arg = f(arg)
				count += 1
			}
			count
		}

		/** Returns the number of times that function `f` can be recursively applied to its own return values,
		  * starting with `this`, before it returns a value not satisfying the predicate. If `!pred(this)`,
		  * the method returns `0`. Otherwise, it is the maximal `n` such that `pred(f`^`n`^`(this))`.
		  */
		@inline def countWhile(pred :T => Boolean)(f :T => T) :Int = {
			var arg = start
			var count = 0
			while (!pred(arg)) {
				arg = f(arg)
				count += 1
			}
			count
		}

		/** Applies recursively the argument partial function to `this` value, and then to the values returned
		  * by the previous iterations, for as long as it is defined for the given argument.
		  */
		@inline def recurse(f :PartialFunction[T, T]) :T = reapply(start)(f)
	}



	/** Represents a range `[0, this)` which defines a fixed number of iterations. */
	class timesMethods(private val count :Int) extends AnyVal {

		/** Execute `f` (code block passed by name) `this` number of times. */
		@inline def times[T](f : => T) :Unit = for (_ <- 0 until count) f

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
		  * @example `(new StringBuilder /: n)(_ += ":)"`
		  */
		@inline def /:[T](start :T)(f :T => T) :T = times(f)(start)
	}
}

