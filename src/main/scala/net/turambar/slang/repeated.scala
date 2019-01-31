package net.turambar.slang

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

/** A group of functions and injected methods for `fold`-like operations and generating
  * streams/collections of values from a starting point and a function.
  * @author Marcin Mościcki
  */
object repeated {

	/** A synthactic wrapper for collections injecting methods implementing
	  * 'breakable' `foldLeft`, that is folding only some prefix of the collection
	  * based on some predicate or other termination condition.
	  *
	  * @param items any collection to fold
	  * @tparam T element type of this collection.
	  */
	implicit class foldWhile[T](private val items :Iterable[T]) extends AnyVal {

		/** Applies the given folding function `op` to the elements of this collection starting with the given
		  * initial value `start` while the predicate `until` is false for the most recently computed value.
		  * @param start initial value
		  * @param until predicate which needs to be satisfied
		  * @param op function generating subsequent values based on the previously computed value
		  *           and the next element of the collection.
		  * @tparam A type of generated and tested values
		  * @return first generated value which satisfies predicate `until` or result of folding the whole collection
		  *         if no such element was computed.
		  */
		def foldUntil[A](start :A)(until :A=>Boolean)(op :(A, T)=>A) :A =
			items.toStream.scanLeft(start)(op).takeWhile(!until(_)).last

		/** Applies the given folding function `op` to the elements of this collection starting with the given initial value `start`
		  * for as long as `op` is defined for the previously computed value.
		  * @param start accumulator given as the first argument to first invocation of `op`
		  * @param op a partial function combining the accumulator with the elements of the collection working
		  *           as the breaking condition when non-defined.
		  * @tparam A type of generated values
		  * @return result of `this.foldLeft(a)(op)` or first value `a :A` such that `op` is not defined
		  *         for `(a, e)` where `e` is the first non-folded element of this collection.
		  */
		def foldWhile[A](start :A)(op :PartialFunction[(A, T), A]) :A = {
			val lift = op.lift
			foldSome(start) { (acc, elem) => lift(acc->elem) }
		}

		/** Applies the given folding function to the elements of this collection and current accumulator value
		  * for as long as it returns non-empty results.
		  * @param start initial accumulator value passed to the first call of `op` together with the first element of this collection.
		  * @param op a function generating consecutive values of `A` from the previous value and subsequent element of this collection,
		  *           yielding `None` to signal the break condition for folding
		  * @tparam A type of generated values
		  */
		def foldSome[A](start :A)(op :(A, T) => Option[A]) :A =
			items.toStream.scanLeft(Option(start)) {
				(acc, elem) => acc.flatMap(op(_, elem))
			}.takeWhile(_.isDefined).last.get
		
	}
	
	/** Represents a range `[0, this)` which defines number of iterations. */
	implicit class repeatTimes(private val times :Int) extends AnyVal {
		/** Execute `f` (code block passed by name) `this` number of times. */
		@inline def times[T](f : =>T) :Unit = for (i <- 0 until times) f
		
		/** Apply `f` recursively to its own result `this` number of times, starting with value `start`. */
		@tailrec final def times[T](f :T=>T)(start :T) :T =
			if (times<=0) start
			else (times-1).times(f)(f(start))
		
		/** Apply `f` to its own result `this` number of times, starting with value `start`.
		  * Equivalent to `this.times(f)(start)` but helps with type inference.
		  * The name is in analogy to equivalent fold left over a range:
		  * `def pow(x :Int, n:Int) = (1 /: (0 until n)){ (acc, _) => acc * x }` (in pseude code)
		  * @param start start value for the recursion
		  * @param f function to recursively apply to itself
		  * @return `start` if `this<=0` or `f(f(...f(start)))` (composed `this` number of times).
		  * @usecase `(new StringBuilder /: n)(_ += ":)"`
		  */
		@inline def /:[T](start :T)(f :T=>T) :T = times(f)(start)
		

		
	}

	/** Given an accumulator value of type `A`, generate a sequence of values by recursively applying
	  * function `next`, and combine the generated value with last accumulator value.
	  * This function is equivalent to generating a virtual input stream of type `T` from a recursive partial function
	  * and folding that stream at the same time, with generator and map functions combined into a single step.
	  * The `next` function is recursively applied to previously returned accumulator and stream element (starting
	  * with accumulator `acc` and element `start`) for as long as possible, returning the most recently
	  * computed accumulator once it can no longer be applied to it's previous result.
	  * @param acc initial value of accumulator for the fold step.
	  * @param start first element of a stream to be folded
	  * @param next a partial function that, given the current accumulator for 'fold' and previous element of
	  *             the virtual stream, possibly returns the next element of the stream, and the result
	  *             of combining it with the input accumulator.
	  * @tparam A accumulator type of the fold step and returned value type.
	  * @tparam T element type of the virtual stream to be folded
	  * @return result of folding initial accumulator `acc` with the virtual stream starting with `start`.
	  */
	def accumulate[A, T](acc :A)(start :T)(next :PartialFunction[(A, T), (A, T)]) :A =
		reapplySome((acc, start))(next.lift)._1
//		(next andThen { case (a, t) => accumulate(a)(t)(next) })(acc, start)


	/** Given an acumulator value of type `A`, generate a sequence of values by recursively applying
	  * function `next`, and combine the generated value with last accumulator value.
	  * This function is equivalent to generating a virtual input stream of type `T` from a recursive partial function
	  * and folding that stream at the same time, with generator and map functions combined into a single step.
	  * The `next` function is recursively applied to previously returned accumulator and stream element (starting
	  * with accumulator `acc` and element `start`) for as long as it returns some value, returning the most recently
	  * computed accumulator once it eventually returns `None`.
	  * @param acc initial value of accumulator for the fold step.
	  * @param start first element of a stream to be folded
	  * @param next a function that, given the current accumulator for 'fold' and previous element of
	  *             the virtual stream, returns the next element of the stream, and the result
	  *             of combining it with the input accumulator, or `None` to indiciate the end of folding.
	  * @tparam A accumulator type of the fold step and returned value type.
	  * @tparam T element type of the virtual stream to be folded
	  * @return result of folding initial accumulator `acc` with the virtual stream starting with `start`.
	  */
	@tailrec def accumulateSome[A, T](acc :A)(start :T)(next :(A, T) => Option[(A, T)]) :A = next(acc, start) match {
		case Some((sum, rest)) => accumulateSome(sum)(rest)(next)
		case None => acc
	}


	/** Apply the given function recursively to its own result, starting with value `start`, for as long
	  * as it is applicable.
	  * @param start initial value for the recursion
	  * @param next a function to be recursed over
	  * @return the last value returned by `next`, to which it no longer is applicable.
	  */
	def reapply[X](start :X)(next :PartialFunction[X, X]) :X = reapplySome(start)(next.lift)

	/** Apply the given function recursively to its own result, starting with value `start`, for as long
	  * as it returns a value.
	  * @param start initial value for the recursion
	  * @param next a function to be recursed over
	  * @return the first computed value for which `next` returns `None`.
	  */
	@tailrec def reapplySome[X](start :X)(next :X => Option[X]) :X = next(start) match {
		case Some(x) => reapplySome(x)(next)
		case None => start
	}


	/** A complement of `C.iterate` provided by collection companion objects, which creates
	  * a collection `Ć` by recursively applying `next` and collecting all returned values
	  * using the builder from the implicit builder factory. This function returns
	  * once `next` is no longer applicable to the last returned element, meaning that
	  * it must have an inbuilt termination condition.
	  * @param start first element of returned collection
	  * @param next generator function returning subsequent elements for the collection based on the previous one,
	  *             serving as the termination condition by indicating that it can no longer be applied to the given argument.
	  * @param cbf implicit builder factory determining the type of the returned collection
	  * @tparam X element type of the generated collection
	  * @tparam C return collection type
	  * @return a collection containing the sequence starting with `start` and resulting from recursively applying `next` to itself.
	  */
	def iterate[X, C](start :X)(next :PartialFunction[X, X])(implicit cbf :CanBuildFrom[_, X, C]) :C =
		iterateSome(start)(next.lift)

	/** A complement of `C.iterate` provided by collection companion objects, which creates
	  * a collection `Ć` by recursively applying `next` and collecting all returned values
	  * using the builder from the implicit builder factory. This function returns
	  * once `next` returns `None`, meaning that it must have an inbuilt termination condition.
	  * @param start first element of returned collection
	  * @param next generator function returning subsequent elements for the collection based on the previous one,
	  *             or `None` to indicate the end of recursion
	  * @param cbf implicit builder factory determining the type of the returned collection
	  * @tparam X element type of the generated collection
	  * @tparam C return collection type
	  * @return a collection containing the sequence starting with `start` and resulting from recursively applying `next` to itself.
	  */
	def iterateSome[X, C](start :X)(next :X=>Option[X])(implicit cbf :CanBuildFrom[_, X, C]) :C =
		if (cbf==Stream.ReusableCBF)
			(start #:: (next(start).map(iterateSome(_)(next) :Stream[X]) getOrElse Stream.Empty)).asInstanceOf[C]
		else {
			val builder = cbf()
			builder += start
			@tailrec def rec(x :X=start) :C = next(x) match {
				case Some(y) => builder +=y; rec(y)
				case None => builder.result()
			}
			rec()
		}

	/** Recursively applies function `next` to its result, starting with argument `start` and returning
	  * an eager sequence containing `start` followed by the values returned by `next`.
	  * Recursion stops when the generator function is no longer applicable to the previously compouted value.
	  */
	@inline def list[X, Y](start :X)(next :PartialFunction[X, X]) :Seq[X] = iterate(start)(next)

	/** Recursively applies function `next` to its result, starting with argument `start` and returning
	  * an eager sequence containing `start` followed by the values returned by `next`.
	  * Recursion stops once the generator function returns `None`.
	  */
	@inline def listSome[X, Y](start :X)(next :X=>Option[X]) :Seq[X] = iterateSome(start)(next)


	/** Apply the given partial function recursively to the returned successor value, starting with the given value,
	  * and return a sequence of all intermediate results. Recursion stops once the function can no longer be applied to the
	  * argument it returned, and all first elements of previously returned pairs are returned in a sequence.
	  * In essence, this is a function combining generation of a stream from a recursive partial function and mapping
	  * over that stream to a single step.
	  * @param start initial argument for the function
	  * @param next a partial function, returning a pair consisting of an element of the result, and the next argument to itself.
	  * @tparam X type of the initial argument of the function
	  * @tparam Y element type of the returned sequence
	  * @return a sequence containing first elements of all invocations of `next` in the order of execution.
	  */
	def itemize[X, Y](start :X)(next :PartialFunction[X, (Y, X)]) :Seq[Y] = itemizeSome(start)(next.lift)

	/** Apply the given function recursively to the returned successor value, starting with the given value,
	  * and a return a sequence of all intermediate results. Recursion stops once the function returns `None`,
	  * and all first elements of previously returned pairs are returned in a sequence.
	  * In essence, this is a function combining generation of a stream from a recursive partial function and mapping
	  * over that stream to a single step.
	  * @param start initial argument for the function
	  * @param next a function, returning a pair consisting of an element of the result, and the successor argument to itself,
	  *             or `None` to indicate termination condition.
	  * @tparam X type of the initial argument of the function
	  * @tparam Y element type of the returned sequence
	  * @return a sequence containing first elements of all invocations of `next` in the order of execution.
	  */
	def itemizeSome[X, Y](start :X)(next :X=>Option[(Y, X)]) :Seq[Y] = {
		@tailrec def rec(x :X=start, acc :List[Y]=Nil) :Seq[Y] = next(x) match {
			case Some((y, x)) => rec(x, y::acc)
			case None => acc.reverse
		}
		rec()
	}

	/** Apply the given partial function recursively to its own result, starting from the given value, and return
	  * a collection of all partial results. Recursion stops once the function can no longer be applied to the argument
	  * it returned, and all first alements of returned pairs a re returned in a collection built with the builder provided by
	  * the implicit factory.
	  * @param start initial argument for the function
	  * @param next a partial function, returning a pair consisting of an element of the result, and the next argument to itself.
	  * @param cbf implicit factory of builders specifying the result type.
	  * @tparam X type of the initial argument of the function
	  * @tparam Y element type of the returned sequence
	  * @tparam S return collection type built by the implicit `CanBuildFrom`
	  * @return a collection containing first elements of all invocations of `next` in the order of execution.
	  */
	@inline def iterated[X, Y, S](start :X)(next :PartialFunction[X, (Y, X)])(implicit cbf :CanBuildFrom[_, Y, S]) :S =
		iteratedSome(start)(next.lift)

	/** Apply the given function recursively to its own result, starting from the given value, and return
	  * a collection of all partial results. Recursion stops once the generator function returns `None`,
	  * and all first alements of returned pairs a re returned in a collection built with the builder provided by
	  * the implicit factory.
	  * @param start initial argument for the function
	  * @param next a function, returning a pair consisting of an element of the result and the next argument to itself,
	  *             or `None` to indicate the end of recursion.
	  * @param cbf implicit factory of builders specifying the result type.
	  * @tparam X type of the initial argument of the function
	  * @tparam Y element type of the returned sequence
	  * @tparam S return collection type built by the implicit `CanBuildFrom`
	  * @return a collection containing first elements of all invocations of `next` in the order of execution.
	  */
	def iteratedSome[X, Y, S](start :X)(next :X=>Option[(Y, X)])(implicit cbf :CanBuildFrom[_, Y, S]) :S = {
		val b = cbf()
		@tailrec def rec(arg :X=start) :S = next(arg) match {
			case Some((y, x)) => b += y; rec(x)
			case None => b.result()
		}
		rec()
	}
	
	def streamed[X, Y](start :X)(next :PartialFunction[X, (Y, X)]) :Stream[Y] =
		next.andThen(yx => yx._1 #:: streamed(yx._2)(next)).applyOrElse(start, (_:X) => Stream.empty)
		

}
