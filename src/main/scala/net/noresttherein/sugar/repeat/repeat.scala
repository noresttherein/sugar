package net.noresttherein.sugar

import scala.annotation.tailrec

import net.noresttherein.sugar.typist.Rank.Rank0
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.One


/** A group of higher order functions and syntax extensions repeatedly or recursively applying other functions.
  *
  * The object itself provides an `apply` method which emulates the `repeat ... until ...` loops:
  * {{{
  *     repeat {
  *         sayBeep()
  *     } until (youAreMad())
  * }}}
  * The result of the whole expression is the value returned by the last invocation of the lazily evaluated expression
  * between `repeat` and [[net.noresttherein.sugar.repeat.RepeatBlock.until(condition:=>Boolean)* until]].
  * You can also pass as the condition a function accepting as an argument the result of the preceding execution:
  * {{{
  *     var genius = "bwa"
  *     val bwahaha = repeat {
  *         genius += "ha"; genius
  *     } until (_ == "bwahaha")
  * }}}
  *
  * Importing [[net.noresttherein.sugar.repeat.extensions.repeatMethods repeatMethods]] extends values of any type with
  * a similar [[net.noresttherein.sugar.repeat.extensions.repeatMethods.repeat repeat]] method, accepting as the body
  * of the loop a function applied first to `this`, and subsequently to the values returned by the previous applications:
  * {{{
  *     val kilo = 2 repeat (_ * 2) until (_ > 1000)
  * }}}
  * @author Marcin Mościcki
  */
package object repeat {
	private[repeat] final val Ver = 1L

	/** Emulates the `repeat { ... } until ( ... )` loop, repeating the execution of the passed block
	  * until the condition passed to the [[net.noresttherein.sugar.repeat.RepeatBlock.until until]] method
	  * of the returned object becomes true. The block is executed at least once and the whole expression
	  * returns the result of the last evaluation.
	  * @param block the body of the ''repeat'' loop.
	  */
	@inline def apply[T](block: => T): RepeatBlock[T] = new RepeatBlock(() => block)

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

	/** Apply the given function recursively to its own result, starting with value `start`, for as long
	  * as it is applicable. This method is available also as an extension method of `PartialFunction`,
	  * and be imported via [[net.noresttherein.sugar.funny.extensions.PartialFunctionExtension]].
	  * @param start initial value for the recursion.
	  * @param next a function to be iterated over.
	  * @return the last value returned by `next`, to which it no longer is applicable.
	  */
	def reapply[X](start :X)(next :PartialFunction[X, X]) :X = reapplySome(start)(next.lift)

	/** Apply the given function recursively to its own result, starting with value `start`, for as long
	  * as it returns a non-empty value. This method is available also as an extension method `reapply` of function
	  * `X => Option[X]`, and be imported via [[net.noresttherein.sugar.funny.extensions.homoOptionFunctionExtension]].
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
	  * This method is also available as an extension method of any `X => X` function,
	  * which can be imported via [[net.noresttherein.sugar.funny.extensions.homoFunctionExtension]]
	  */
	@tailrec def fixedPoint[X](start :X)(f :X => X) :X = f(start) match {
		case found if found == start => start
		case other => fixedPoint(other)(f)
	}

	/** Applies the given function repeatedly to its results, starting with the given argument
	  * and until the returned value equals the argument, or the predefined maximum number of iterations is reached.
	  */
	@tailrec def fixedPoint[X](start :X, maxIterations :Int)(f :X => X) :Opt[X] = f(start) match {
		case _ if maxIterations <= 0 => None
		case found if found == start => One(start)
		case other => fixedPoint(other, maxIterations - 1)(f)
	}



	object iterations {
		/** Executes the argument expression repeatedly, until the condition passed to
		  * [[net.noresttherein.sugar.repeat.CountingBlock.until until]] method of the returned object is satisfied,
		  * and returns the number of performed iterations.
		  */
		@inline def apply[T](block: => T) :CountingBlock[T] = new CountingBlock(() => block)
	}

	/** Extends the preceding block with [[net.noresttherein.sugar.repeat.RepeatBlock.until(condition:=>Boolean)* until]]
	  * methods specifying the stop condition for its execution.
	  */
	class CountingBlock[T] private[repeat] (private val block :() => T) extends AnyVal {
		/** Repeats `this` lazy expression until the lazy expression given as the argument evaluates to true
		  * and returns the number of executed iterations. The expression is always executed at least once.
		  * @param condition the termination condition; when it evaluates to true, the loop ends.
		  */
		@inline def until(condition : => Boolean) :Int = {
			var count = 0
			while (!condition) {
				block()
				count += 1
			}
			count
		}

		/** Repeats `this` lazy expression until the function given as the argument evaluates to true
		  * for the most recently computed value, returning the number of executed iterations. The expression
		  * is always executed at least once.
		  * @param condition the termination condition as the function off the value returned by the preceding block.
		  *                  When it evaluates to true, the loop ends.
		  */
		@inline def until(condition :T => Boolean) :Int = {
			var count = 0
			var last  = block()
			while (!condition(last)) {
				last = block()
				count += 1
			}
			count
		}
	}

}




package repeat {

	/** Extends the preceding block with [[net.noresttherein.sugar.repeat.RepeatBlock.until(condition:=>Boolean)* until]]
	  * [[net.noresttherein.sugar.repeat.RepeatBlock.until(condition:T=>Boolean)* methods]]
	  * specifying the stop condition for its execution.
	  */
	class RepeatBlock[T] private[repeat] (private val block: () => T) extends AnyVal {
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
		  * @return the result of the last execution of `this` expression (the body of the loop),
		  *         that is the one for whose result the condition became true.
		  */
		@inline def until(condition: T => Boolean) :T = {
			var last = block()
			while(!condition(last))
				last = block()
			last
		}

		/** Repeats `this` lazy expression for side effects until it returns the specified value. */
		@inline def until(result :T) :Unit = {
			var last = block()
			while (last != result)
				last = block()
		}

		/** Executes the preceding block `n` times for side effects. */
		@inline def times(n :Int) :Unit = { //I'd love to return T, but we'd have to throw an exception for n <= 0.
			var i = n
			while (i > 0) {
				block()
				i -= 1
			}
		}
	}

	/** Extends the preceding block with [[net.noresttherein.sugar.repeat.RepeatBlock.until(condition:T=>Boolean)* until]]
	  * methods specifying the stop condition for its execution.
	  */
	class RepeatFunction[T] private[repeat] (private[this] var arg :T, f: T => T) {
		/** Executes `this` function for the result of the preceding loop until the predicate given as the argument
		  * becomes true for the most recently computed value.
		  * @param condition the termination condition as the function off the value returned by the preceding block.
		  *                  When it evaluates to true, the loop ends.
		  * @return the result of the last execution of `this` expression (the body of the loop),
		  *         that is the one for whose result the condition became true.
		  *         If `condition(arg)`, where `arg repeat f until condition`, then it is returned immediately.
		  * @see [[net.noresttherein.sugar.funny.extensions.homoFunctionExtension homoFunctionExtension.applyWhile]]
		  */
		@inline def until(condition: T => Boolean) :T = {
			while (!condition(arg))
				arg = f(arg)
			arg
		}

		/** Applies `this` function to the result of the preceding loop for side effects,
		  * until it returns the specified value. If the initial argument `arg` in `arg repeat f until result`
		  * equals `result`, the function is not executed at all.
		  */
		@inline def until(result :T) :T = {
			while (arg != result)
				arg = f(arg)
			arg
		}

		/** Applies `this` function to the result of the preceding loop for as long as it returns values
		  * satisfying the given predicate, and returns the last value for which it held.
		  * @note this is not equivalent to `until(!condition(_))`, as `until` would in this case
		  *       return the first value returned by `f` for which the predicate is not satisfied.
		  */
		@inline def whileTrue(condition: T => Boolean) :T = {
			var last = arg
			while ({ last = arg; arg = f(arg); !condition(arg) })
				()
			last
		}

		/** Reapplies the preceding function to its own return value `n` times
		  * @return `f`^`n`^`(arg)`, where `arg repeat f times n`.
		  */
		@inline def times(n :Int) :T = {
			var i = n
			while (i > 0) {
				arg = f(arg)
				i -= 1
			}
			arg
		}
	}

	private final class RecursionBlock[T](private[this] var arg :T, f :T => T) extends (() => T) {
		override def apply() :T = { arg = f(arg); arg }
	}
}
