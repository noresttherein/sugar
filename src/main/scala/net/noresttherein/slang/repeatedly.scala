package net.noresttherein.slang

import scala.annotation.tailrec
import scala.collection.IterableFactory



/** A group of functions and injected methods for `fold`-like operations and generating
  * streams/collections of values from a starting point and a function.
  * @author Marcin Mościcki
  */
object repeatedly {

	/** A syntactic wrapper for collections, injecting methods implementing
	  * 'breakable' `foldLeft`, that is folding only some prefix of the collection
	  * based on some predicate or other termination condition.
	  *
	  * @param items any collection to fold.
	  * @tparam T element type of this collection.
	  */
	implicit class foldWhileMethods[T](private val items :Iterable[T]) extends AnyVal {

		/** Applies the given folding function `op` to the elements of this collection starting with the given
		  * initial value `start` while the predicate `until` is false for the most recently computed value.
		  * Note that this is not equivalent to `foldWhile(start)(!until(_))(op)` as the recursion goes
		  * one step further, returning the first element which doesn't satisfy the predicate, rather than
		  * the last satisfying it as in the latter case.
		  * @param start initial value.
		  * @param until predicate which needs to be satisfied for the folding to stop.
		  * @param op function generating subsequent values based on the previously computed value
		  *           and the next element of the collection.
		  * @tparam A type of generated and tested values.
		  * @return first generated value which satisfies predicate `until` or result of folding the whole collection
		  *         if no such element was computed.
		  */
		def foldUntil[A](start :A)(until :A => Boolean)(op :(A, T) => A) :A = {
			var acc = start; val it = items.iterator
			while (it.hasNext && !until(acc))
				acc = op(acc, it.next())
			acc
		}


		/** Applies the given folding function `op` to the elements of this collection starting with the given
		  * initial value `start` while the predicate `pred` is true for the most recently computed value.
		  * Note that this is not equivalent to `foldUntil(start)(!pred(_))(op)`, as the latter would apply `op`
		  * one more time unless the end of collection is reached without falsifying the predicate.
		  * @param start initial value.
		  * @param pred predicate which needs to be satisfied for folding to continue.
		  * @param op function generating subsequent values based on the previously computed value
		  *           and the next element of the collection.
		  * @tparam A type of generated and tested values.
		  * @return last generated value which satisfies predicate `pred` or result of folding the whole collection
		  *         if all computed elements satisfy the predicate.
		  */
		def foldWhile[A](start :A)(pred :A =>Boolean)(op :(A, T) => A) :A =
			items.to(LazyList).scanLeft(start)(op).takeWhile(pred).last


		/** Applies the given folding function `op` to the elements of this collection starting with the given initial
		  * value `start` for as long as `op` is defined for the previously computed value.
		  * @param start accumulator given as the first argument to first invocation of `op`.
		  * @param op a partial function combining the accumulator with the elements of the collection working
		  *           as the breaking condition when non-defined.
		  * @tparam A type of generated values.
		  * @return result of `this.foldLeft(a)(op)` or first value `a :A` such that `op` is not defined
		  *         for `(a, e)` where `e` is the first non-folded element of this collection.
		  */
		def foldFront[A](start :A)(op :PartialFunction[(A, T), A]) :A = {
			val lift = op.lift
			foldSome(start) { (acc, elem) => lift(acc->elem) }
		}


		/** Applies the given folding function to the elements of this collection and current accumulator value
		  * for as long as it returns non-empty results.
		  * @param start initial accumulator value passed to the first call of `op` together with the first element
		  *              of this collection.
		  * @param op a function generating consecutive values of `A` from the previous value and subsequent element
		  *           of this collection, yielding `None` to signal the break condition for folding.
		  * @tparam A type of generated values.
		  */
		def foldSome[A](start :A)(op :(A, T) => Option[A]) :A =
			items.to(LazyList).scanLeft(Option(start)) {
				(acc, elem) => acc.flatMap(op(_, elem))
			}.takeWhile(_.isDefined).last.get

	}


	/** Represents a range `[0, this)` which defines a fixed number of iterations. */
	implicit class timesMethods(private val times :Int) extends AnyVal {

		/** Execute `f` (code block passed by name) `this` number of times. */
		@inline def times[T](f : =>T) :Unit = for (_ <- 0 until times) f

		/** Apply `f` recursively to its own result `this` number of times, starting with value `start`. */
		@tailrec final def times[T](f :T=>T)(start :T) :T =
			if (times<=0) start
			else (times-1).times(f)(f(start))

		/** Apply `f` recursively to its own result `this` number of times, starting with value `start`. */
		@tailrec final def timesFrom[T](start :T)(f :T=>T) :T =
			if (times<=0) start
			else (times-1).timesFrom(f(start))(f)



		/** Apply `f` to its own result `this` number of times, starting with value `start`.
		  * Equivalent to `this.timesFrom(start)(f)` but helps with type inference.
		  * The name is in analogy to equivalent fold left over a range:
		  * `def pow(x :Int, n:Int) = (1 /: (0 until n)){ (acc, _) => acc * x }` (in pseudo code)
		  * @param start start value for the recursion.
		  * @param f function to recursively apply to itself.
		  * @return `start` if `this<=0` or `f(f(...f(start)))` (composed `this` number of times).
		  * @usecase `(new StringBuilder /: n)(_ += ":)"`
		  */
		@inline def /:[T](start :T)(f :T=>T) :T = times(f)(start)

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
	@inline final def iterate[X, C[_]](collection :IterableFactory[C])(start :X)(next :PartialFunction[X, X]) :C[X] =
		iterateSome(collection)(start)(next.lift)

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
	@inline final def iterateSome[X, C[_]](collection :IterableFactory[C])(start :X)(next :X => Option[X]) :C[X] =
		collection match {
			case LazyList =>
				(start #:: (next(start).map(iterateSome(LazyList)(_)(next)) getOrElse LazyList.empty)).asInstanceOf[C[X]]
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
	  * This is the same as [[net.noresttherein.slang.repeatedly.iterate iterate[X, List[X] ](start)(next)]]
	  * @param start first element of returned list
	  * @param next generator function returning subsequent elements for the collection based on the previous one,
	  *             serving as the termination condition by indicating that it can no longer be applied
	  *             to the given argument.
	  * @tparam X element type of the generated list
	  * @return a list containing the sequence starting with `start` and resulting from recursively applying `next`
	  *         to itself.
	  */
	@inline def listResults[X](start :X)(next :PartialFunction[X, X]) :Seq[X] = iterate(List)(start)(next)

	/** Recursively applies function `next` to its result, starting with argument `start` and returning
	  * an eager sequence containing `start` followed by the values returned by `next`.
	  * Recursion stops once the generator function returns `None`.
	  * This is the same as [[net.noresttherein.slang.repeatedly.iterateSome iterateSome[X, List[X] ](start)(next)]].
	  * @param start first element of returned list
	  * @param next generator function returning subsequent elements for the collection based on the previous one,
	  *             or `None` to indicate the end of recursion
	  * @tparam X element type of the generated list
	  * @return a list containing the sequence starting with `start` and resulting from recursively applying `next`
	  *         to itself.
	  */
	@inline def listSome[X](start :X)(next :X => Option[X]) :Seq[X] = iterateSome(List)(start)(next)



}
