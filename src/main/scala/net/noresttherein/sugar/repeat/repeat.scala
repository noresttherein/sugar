package net.noresttherein.sugar

import scala.annotation.tailrec

import net.noresttherein.sugar.typist.Rank.Rank0


/** A group of higher order functions and syntax extensions repeatedly or recursively applying other functions.
  *
  * The object itself provides an `apply` method which emulates the `repeat ... until ...` loops:
  * {{{
  *     repeat {
  *         sayBeep()
  *     } until (youAreMad())
  * }}}
  * The result of the whole expression is the value returned by the last invocation of the lazily evaluated expression
  * between `repeat` and [[net.noresttherein.sugar.repeat.RepeatUntil.until until]]. You can also pass as the condition
  * a function accepting as an argument the result of the preceding execution:
  * {{{
  *     var genius = "bwa"
  *     val bwahaha = repeat {
  *         genius += "ha"; genius
  *     } until (_ == "bwahaha")
  * }}}
  *
  * Importing [[net.noresttherein.sugar.repeat.repeatMethod repeatMethod]] extends values of any type with
  * a similar [[net.noresttherein.sugar.repeat.repeatMethod.repeat repeat]] method, accepting as the body
  * of the loop a function applied first to `this`, and subsequently to the values returned by the previous applications:
  * {{{
  *     val kilo = 2 repeat (_ * 2) until (_ > 1000)
  * }}}
  * @author Marcin Mościcki
  */
package object repeat extends repeat.extensions {
	/** Emulates the `repeat { ... } until ( ... )` loop, repeating the execution of the passed block
	  * until the condition passed to the [[net.noresttherein.sugar.repeat.RepeatUntil.until until]] method
	  * of the returned object becomes true. The block is executed at least once and the whole expression
	  * returns the result of the last evaluation.
	  * @param block the body of the ''repeat'' loop.
	  */
	@inline def apply[T](block: => T): RepeatUntil[T] = new RepeatUntil(() => block)

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


	/** Extends the preceding block with [[net.noresttherein.sugar.repeat.RepeatUntil.until until]] methods
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
