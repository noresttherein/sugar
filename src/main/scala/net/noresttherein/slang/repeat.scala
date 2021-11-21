package net.noresttherein.slang

import scala.annotation.tailrec
import scala.collection.IterableFactory



/** A group of higher order functions and syntax extensions repeatedly or recursively applying other functions.
  *
  * The object itself provides an `apply` method which emulates the `repeat ... until ...` loops:
  * {{{
  *     repeat {
  *         sayBeep()
  *     } until (youAreMad())
  * }}}
  * The result of the whole expression is the value returned by the last invocation of the lazily evaluated expression
  * between `repeat` and [[net.noresttherein.slang.repeat.RepeatUntil.until until]]. You can also pass as the condition
  * a function accepting as an argument the result of the preceding execution:
  * {{{
  *     var genius = "bwa"
  *     val bwahaha = repeat {
  *         genius += "ha"; genius
  *     } until (_ == "bwahaha")
  * }}}
  *
  * Importing [[net.noresttherein.slang.repeat.repeatMethod repeatMethod]] extends values of any type with
  * a similar [[net.noresttherein.slang.repeat.repeatMethod.repeat repeat]] method, accepting as the body
  * of the loop a function applied first to `this`, and subsequently to the values returned by the previous applications:
  * {{{
  *     val kilo = 2 repeat (_ * 2) until (_ > 1000)
  * }}}
  * @author Marcin Mościcki
  */
object repeat {
	/** Emulates the `repeat { ... } until ( ... )` loop, repeating the execution of the passed block
	  * until the condition passed to the [[net.noresttherein.slang.repeat.RepeatUntil.until until]] method
	  * of the returned object becomes true. The block is executed at least once and the whole expression
	  * returns the result of the last evaluation.
	  * @param block the body of the ''repeat'' loop.
	  */
	@inline def apply[T](block: => T): RepeatUntil[T] = new RepeatUntil(() => block)

	/** Extends the preceding block with [[net.noresttherein.slang.repeat.RepeatUntil.until until]] methods
	  * specifying the stop condition for its execution.
	  */
	class RepeatUntil[T](private val block: () => T) extends AnyVal {
		/** Repeats `this` lazy expression until the lazy expression given as the argument evaluates to true.
		  * @param condition the termination condition; when it evaluates to true, the loop ends.
		  * @return the result of the last execution of `this` expression (the body of the loop).
		  */
		@inline def until(condition: => Boolean) :T = {
			var last = block()
			while(!condition)
				last = block()
			last
		}

		/** Repeats `this` lazy expression until the function given as the argument evaluates to true
		  * for the most recently computed value.
		  * @param condition the termination condition as the function off the value returned by the preceding block.
		  *                  When it evaluates to true, the loop ends.
		  * @return the result of the last execution of `this` expression (the body of the loop).
		  */
		@inline def until(condition: T => Boolean) :T = {
			var last = block()
			while(!condition(last))
				last = block()
			last
		}
	}

	//lets see if it doesn't cause an overload conflict in Scala 3
//	@inline def apply[T](arg :T)(f :T => T) :RecurseUntil[T] = new RecurseUntil(arg, f)
//
//	class RecurseUntil[T](acc :T, expr :T => T) {
//		@inline def until(condition :T => Boolean) :T = {
//			var last = expr(acc)
//			while (!condition(last))
//				last = expr(last)
//			last
//		}
//	}


	/** Extends any value with method [[net.noresttherein.slang.repeat.repeatMethod.repeat repeat]], creating
	  * a pure functional equivalent of a `repeat ... until` loops:
	  * {{{
	  *     val kilo = 2 repeat { _ * 2 } until (_ > 1000)
	  * }}}
	  */
	implicit class repeatMethod[T](private var start :T) {
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
	implicit class timesMethods(private val count :Int) extends AnyVal {

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




	/** Apply the given function recursively to its own result, starting with value `start`, for as long
	  * as it is applicable.
	  * @param start initial value for the recursion.
	  * @param next a function to be iterated over.
	  * @return the last value returned by `next`, to which it no longer is applicable.
	  */
	def reapply[X](start :X)(next :PartialFunction[X, X]) :X = reapplySome(start)(next.lift)

	/** Apply the given function recursively to its own result, starting with value `start`, for as long
	  * as it returns a non-empty value.
	  * @param start initial value for the recursion.
	  * @param next a function to be iterated over.
	  * @return the first computed value for which `next` returns `None`.
	  */
	@tailrec def reapplySome[X](start :X)(next :X => Option[X]) :X = next(start) match {
		case Some(x) => reapplySome(x)(next)
		case None => start
	}


	/** Applies the given function repeatedly to its results, starting with the given argument
	  * and until the returned value equals the argument.
	  * May not terminate if the function does not converge on a fixed point.
	  */
	@tailrec def fixedPoint[X](start :X)(f :X => X) :X = f(start) match {
		case found if found == start => start
		case other => fixedPoint(other)(f)
	}

	/** Applies the given function repeatedly to its results, starting with the given argument
	  * and until the returned value equals the argument, or the predefined maximum number of iterations is reached.
	  */
	@tailrec def fixedPoint[X](start :X, maxIterations :Int)(f :X => X) :Option[X] = f(start) match {
		case _ if maxIterations <= 0 => None
		case found if found == start => Some(start)
		case other => fixedPoint(other, maxIterations - 1)(f)
	}

}
