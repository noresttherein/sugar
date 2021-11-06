package net.noresttherein.slang

import scala.annotation.tailrec
import scala.collection.IterableFactory



/** A group of functions and injected methods for `fold`-like operations and generating
  * streams/collections of values from a starting point and a function.
  * @author Marcin Mościcki
  */
object repeatedly {

	/** Represents a range `[0, this)` which defines a fixed number of iterations. */
	implicit final class timesMethods(private val count :Int) extends AnyVal {

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

	/** A complement of `C.iterate` and `C.unfold` provided by collection companion objects, which creates
	  * a collection `C` by recursively applying a partial function while defined to its own results and collecting
	  * all returned values. Instead of listing a fixed number of elements, this method uses
	  * the generator function `next` as the termination condition and returns once it is no longer applicable
	  * to the last returned element.
	  * @param start first element added to the collection.
	  * @param next generator function returning subsequent elements for the collection based on the previous one,
	  *             serving as the termination condition by indicating that it can no longer be applied
	  *             to the given argument.
	  * @param collection builder factory determining the type of the returned collection.
	  * @tparam X element type of the generated collection.
	  * @return a collection containing the sequence starting with `start` and resulting from recursively applying
	  *         `next` to itself..
	  */
	@inline final def reapply[X, C[_]](collection :IterableFactory[C])(start :X)(next :PartialFunction[X, X]) :C[X] =
		reapplySome(collection)(start)(next.lift)

	/** Builds the collection `C[X]` by recursively reapplying the given partial function to the initial element.
	  * Instead of listing a fixed number of elements, this method uses the generator function `next` as the termination
	  * condition and returns once it returns `None`.
	  * @param start first element added to the collection
	  * @param next generator function returning subsequent elements for the collection based on the previous one,
	  *             or `None` to indicate the end of recursion
	  * @param collection builder factory determining the type of the returned collection
	  * @tparam X element type of the generated collection
	  * @return a collection containing the sequence starting with `start` and resulting from recursively applying `next`
	  *         to itself.
	  */
	@inline final def reapplySome[X, C[_]](collection :IterableFactory[C])(start :X)(next :X => Option[X]) :C[X] =
		collection match {
			case LazyList =>
				(start #:: (next(start).map(reapplySome(LazyList)(_)(next)) getOrElse LazyList.empty)).asInstanceOf[C[X]]
			case _ =>
				val builder = collection.newBuilder[X]
				builder += start
				@tailrec def rec(x :X=start) :C[X] = next(x) match {
					case Some(y) => builder += y; rec(y)
					case None => builder.result()
				}
				rec()
		}


	/** Recursively applies function `next` to its result, starting with argument `start` and returning
	  * an eager sequence containing `start` followed by the values returned by `next`.
	  * Recursion stops when the generator function is no longer applicable to the previously computed value.
	  * This is the same as [[net.noresttherein.slang.repeatedly.reapply reapply]]`(Seq)(start)(next)`
	  * @param start first element of returned list
	  * @param next generator function returning subsequent elements for the collection based on the previous one,
	  *             serving as the termination condition by indicating that it can no longer be applied
	  *             to the given argument.
	  * @tparam X element type of the generated list
	  * @return a list containing the sequence starting with `start` and resulting from recursively applying `next`
	  *         to itself.
	  */
	@inline final def listAll[X](start :X)(next :PartialFunction[X, X]) :Seq[X] = reapply(List)(start)(next)

	/** Recursively applies function `next` to its result, starting with argument `start` and returning
	  * an eager sequence containing `start` followed by the values returned by `next`.
	  * Recursion stops once the generator function returns `None`.
	  * This is the same as [[net.noresttherein.slang.repeatedly.reapply iterateSome]]`(Seq)(start)(next)`.
	  * @param start first element of returned list
	  * @param next generator function returning subsequent elements for the collection based on the previous one,
	  *             or `None` to indicate the end of recursion
	  * @tparam X element type of the generated list
	  * @return a list containing the sequence starting with `start` and resulting from recursively applying `next`
	  *         to itself.
	  */
	@inline final def listSome[X](start :X)(next :X => Option[X]) :Seq[X] = reapplySome(List)(start)(next)

}
