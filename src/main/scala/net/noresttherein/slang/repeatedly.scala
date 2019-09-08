package net.noresttherein.slang

import net.noresttherein.slang.repeatedly.repeatingCollections.{IteratePartialFunction, IterateSome, UnfoldPartialFunction, UnfoldSome}

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom

/** A group of functions and injected methods for `fold`-like operations and generating
  * streams/collections of values from a starting point and a function.
  * @author Marcin Mościcki
  */
object repeatedly {

	/** A syntactic wrapper for collections, injecting methods implementing
	  * 'breakable' `foldLeft`, that is folding only some prefix of the collection
	  * based on some predicate or other termination condition.
	  *
	  * @param items any collection to fold
	  * @tparam T element type of this collection.
	  */
	implicit class foldWhileMethods[T](private val items :Iterable[T]) extends AnyVal {

		/** Applies the given folding function `op` to the elements of this collection starting with the given
		  * initial value `start` while the predicate `until` is false for the most recently computed value.
		  * Note that this is not equivalent to `foldWhile(start)(!until(_))(op)` as the recursion goes
		  * one step further, returning the first element which doesn't satisfy the predicate, rather than
		  * the last satisfying it as in the latter case.
		  * @param start initial value
		  * @param until predicate which needs to be satisfied for folding to stop
		  * @param op function generating subsequent values based on the previously computed value
		  *           and the next element of the collection.
		  * @tparam A type of generated and tested values
		  * @return first generated value which satisfies predicate `until` or result of folding the whole collection
		  *         if no such element was computed.
		  */
		def foldUntil[A](start :A)(until :A=>Boolean)(op :(A, T)=>A) :A = {
			var acc = start; val it = items.iterator
			while (it.hasNext && !until(acc))
				acc = op(acc, it.next())
			acc
		}


		/** Applies the given folding function `op` to the elements of this collection starting with the given
		  * initial value `start` while the predicate `pred` is true for the most recently computed value.
		  * Note that this is not equivalent to `foldUntil(start)(!pred(_))(op)`, as the latter would apply `op`
		  * one more time unless the end of collection is reached without falsifying the predicate.
		  * @param start initial value
		  * @param pred predicate which needs to be satisfied for folding to continue
		  * @param op function generating subsequent values based on the previously computed value
		  *           and the next element of the collection.
		  * @tparam A type of generated and tested values
		  * @return last generated value which satisfies predicate `pred` or result of folding the whole collection
		  *         if all computed elements satisfy the predicate.
		  */
		def foldWhile[A](start :A)(pred :A=>Boolean)(op :(A, T)=>A) :A =
			items.toStream.scanLeft(start)(op).takeWhile(pred).last


		/** Applies the given folding function `op` to the elements of this collection starting with the given initial value `start`
		  * for as long as `op` is defined for the previously computed value.
		  * @param start accumulator given as the first argument to first invocation of `op`
		  * @param op a partial function combining the accumulator with the elements of the collection working
		  *           as the breaking condition when non-defined.
		  * @tparam A type of generated values
		  * @return result of `this.foldLeft(a)(op)` or first value `a :A` such that `op` is not defined
		  *         for `(a, e)` where `e` is the first non-folded element of this collection.
		  */
		def foldFront[A](start :A)(op :PartialFunction[(A, T), A]) :A = {
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
		  * @param start start value for the recursion
		  * @param f function to recursively apply to itself
		  * @return `start` if `this<=0` or `f(f(...f(start)))` (composed `this` number of times).
		  * @usecase `(new StringBuilder /: n)(_ += ":)"`
		  */
		@inline def /:[T](start :T)(f :T=>T) :T = times(f)(start)

	}






	/** Apply the given function recursively to its own result, starting with value `start`, for as long
	  * as it is applicable.
	  * @param start initial value for the recursion
	  * @param next a function to be iterated over
	  * @return the last value returned by `next`, to which it no longer is applicable.
	  */
	def reapply[X](start :X)(next :PartialFunction[X, X]) :X = reapplySome(start)(next.lift)

	/** Apply the given function recursively to its own result, starting with value `start`, for as long
	  * as it returns a non-empty value.
	  * @param start initial value for the recursion
	  * @param next a function to be iterated over
	  * @return the first computed value for which `next` returns `None`.
	  */
	@tailrec def reapplySome[X](start :X)(next :X => Option[X]) :X = next(start) match {
		case Some(x) => reapplySome(x)(next)
		case None => start
	}






	/** A complement of `C.iterate` provided by collection companion objects, which creates a collection `C`
	  * by recursively applying a partial function while defined to its own results and collecting all returned values.
	  * Instead of listing a fixed number of elements, this method uses the generator function `next` as the termination
	  * condition and returns once it is no longer applicable to the last returned element.
	  * @param start first element added to the collection
	  * @param next generator function returning subsequent elements for the collection based on the previous one,
	  *             serving as the termination condition by indicating that it can no longer be applied to the given argument.
	  * @param cbf implicit builder factory determining the type of the returned collection
	  * @tparam X element type of the generated collection
	  * @return a collection containing the sequence starting with `start` and resulting from recursively applying `next` to itself.
	  */
	@inline final def iterate[X, C](start :X)(next :PartialFunction[X, X])(implicit cbf :CanBuildFrom[_, X, C]) :C =
		iterateSome(start)(next.lift)

	/** Builds the collection `C[X]` by recursively reapplying the given partial function to the initial element.
	  * Instead of listing a fixed number of elements, this method uses the generator function `next` as the termination
	  * condition and returns once it returns `None`.
	  * @param start first element added to the collection
	  * @param next generator function returning subsequent elements for the collection based on the previous one,
	  *             or `None` to indicate the end of recursion
	  * @param cbf implicit builder factory determining the type of the returned collection
	  * @tparam X element type of the generated collection
	  * @return a collection containing the sequence starting with `start` and resulting from recursively applying `next` to itself.
	  */
	@inline final def iterateSome[X, C](start :X)(next :X=>Option[X])(implicit cbf :CanBuildFrom[_, X, C]) :C =
		if (cbf==Stream.ReusableCBF)
			(start #:: (next(start).map(iterateSome(_)(next) :Stream[X]) getOrElse Stream.Empty)).asInstanceOf[C]
		else {
			val builder = cbf()
			builder += start
			@tailrec def rec(x :X=start) :C = next(x) match {
				case Some(y) => builder += y; rec(y)
				case None => builder.result()
			}
			rec()
		}

	/** A complement of `C.iterate` provided by collection companion objects, which creates a collection `C`
	  * by recursively applying a partial function while defined to its own results and collecting all returned values.
	  * Only the former needs to be provided here explicitly, while the latter will be inferred from the `apply`
	  * method of the returned ephemeral object: [[IteratePartialFunction#apply]].
	  * @tparam C[T] return collection type
	  * @return a factory accepting the initial value `start`, the generator function `next` and returning a collection
	  *         containing the sequence starting with `start` and resulting from recursively applying `next` to its result.
	  * @see [[net.noresttherein.slang.repeatedly.repeatingCollections.IteratePartialFunction#apply[X](X)(PartialFunction[X, X])(CanBuildFrom[_, X, C])]]
	  * @see [[net.noresttherein.slang.repeatedly.iterate iterate]]
	  */
	@inline def iterateAs[C[T]] :IteratePartialFunction[C] = new IteratePartialFunction[C] {}

	/** A complement of `C.iterate` provided by collection companion objects, which creates a collection `C`
	  * by recursively applying an option returning function to its own results for as long as they are defined
	  * and collecting all returned values using the builder from the implicit builder factory.
	  * It is a two-step method to separate the type argument of the created collection from its element type.
	  * Only the former needs to be provided here explicitly, while the latter will be inferred from the `apply`
	  * method of the returned ephemeral object: [[IterateSome#apply]]
	  *
	  * @tparam C[T] return collection type
	  * @return a factory accepting the initial value `start`, the generator function `next` and returning a collection
	  *         containing the sequence starting with `start` and resulting from recursively applying `next` to its result.
	  * @see [[net.noresttherein.slang.repeatedly.repeatingCollections.IterateSome#apply[X](X)(X=>Optioin[X])(CanBuildFrom[_, X, C])*]]
	  * @see [[net.noresttherein.slang.repeatedly.iterateSome iterateSome]]
	  */
	def iterateSomeAs[C[T]] :IterateSome[C] = new IterateSome[C] {}



	/** Recursively applies function `next` to its result, starting with argument `start` and returning
	  * an eager sequence containing `start` followed by the values returned by `next`.
	  * Recursion stops when the generator function is no longer applicable to the previously computed value.
	  * This is the same as [[net.noresttherein.slang.repeatedly.iterate iterate[X, List[X] ](start)(next)]]
	  * @param start first element of returned list
	  * @param next generator function returning subsequent elements for the collection based on the previous one,
	  *             serving as the termination condition by indicating that it can no longer be applied to the given argument.
	  * @tparam X element type of the generated list
	  * @return a list containing the sequence starting with `start` and resulting from recursively applying `next` to itself.
	  */
	@inline def listResults[X](start :X)(next :PartialFunction[X, X]) :Seq[X] = iterate(start)(next)

	/** Recursively applies function `next` to its result, starting with argument `start` and returning
	  * an eager sequence containing `start` followed by the values returned by `next`.
	  * Recursion stops once the generator function returns `None`.
	  * This is the same as [[net.noresttherein.slang.repeatedly.iterateSome iterateSome[X, List[X] ](start)(next)]].
	  * @param start first element of returned list
	  * @param next generator function returning subsequent elements for the collection based on the previous one,
	  *             or `None` to indicate the end of recursion
	  * @tparam X element type of the generated list
	  * @return a list containing the sequence starting with `start` and resulting from recursively applying `next` to itself.
	  */
	@inline def listSome[X](start :X)(next :X=>Option[X]) :Seq[X] = iterateSome(start)(next)






	/** Apply the given partial function recursively to its own result, starting from the given value, and return
	  * a collection of all partial results. Recursion stops once the function can no longer be applied to the argument
	  * it returned, and all first elements of returned pairs are returned in a collection built with the builder
	  * provided by the implicit factory. This operation is in a manner the opposite of folding a collection and
	  * roughly equivalent to generating a sequence of states and mapping it to the desired element type.
	  * @param start initial argument for the function
	  * @param next a partial function, returning a pair consisting of an element of the result, and the next
	  *             argument to itself.
	  * @param cbf implicit factory of builders specifying the result type.
	  * @tparam S type of the initial argument of the function
	  * @tparam X element type of the returned collection
	  * @tparam C type of the built collection
	  * @return a collection containing first elements of all invocations of `next` in the order of execution.
	  */
	def unfold[S, X, C](start :S)(next :PartialFunction[S, (X, S)])(implicit cbf :CanBuildFrom[_, X, C]) :C =
		unfoldSome(start)(next.lift)

	/** Apply the given function recursively to the returned successor value, starting with the given value,
	  * and a return a collection of all intermediate results. Recursion stops once the function returns `None`,
	  * and all first elements of previously returned pairs are returned in a collection built with a builder
	  * provided by the default implicit factory `CanBuildFrom`. This operation is in a manner the opposite of folding
	  * a collection and roughly equivalent to generating a sequence of states and mapping it to the desired element type.
	  * @param start initial argument for the function
	  * @param next a function, returning a pair consisting of an element of the result, and the successor argument
	  *             to itself, or `None` to indicate termination condition.
	  * @tparam S type of the initial argument of the function
	  * @tparam X element type of the returned collection
	  * @tparam C type of the built collection
	  * @return a collection containing first elements of all invocations of `next` in the order of execution.
	  */
	def unfoldSome[S, X, C](start :S)(next :S => Option[(X, S)])(implicit cbf :CanBuildFrom[_, X, C]) :C = {
		val b = cbf()
		@tailrec def rec(arg :S=start) :C = next(arg) match {
			case Some((x, s)) => b += x; rec(s)
			case None => b.result()
		}
		rec()
	}

	/** Creates a collection `C[T]` in a manner being opposite to folding. It starts with the given initial state
	  * and repeatedly applies the given partial function for as long as it is defined to obtain the next element
	  * of the collection and updated state.
	  * This is roughly equivalent to generating a sequence of states and mapping it to the desired element type.
	  * It is a two-step method to separate the type argument of the created collection from the element and state types.
	  * Only the former needs to be provided here explicitly, while the latter will be inferred from the `apply`
	  * method of the returned ephemeral object: [[net.noresttherein.slang.repeatedly.repeatingCollections.UnfoldPartialFunction#apply]].
	  * @tparam C[T] built collection type
	  * @return an object accepting the initial  `start`, argument for the generator function, a partial function
	  *         transforming state into a collection element and reduced state and returns the collection containing
	  *         all values returned by the latter for as long as it could be applied.
	  * @see [[net.noresttherein.slang.repeatedly.repeatingCollections.UnfoldPartialFunction]]
	  * @see [[net.noresttherein.slang.repeatedly.unfold unfold]]
	  */
	def unfoldAs[C[T]] :UnfoldPartialFunction[C] = new UnfoldPartialFunction[C] {}

	/** Creates a collection `C[T]` in a manner being opposite to folding. It starts with the given initial state
	  * and repeatedly applies an option returning function for as long as its result is defined to obtain
	  * the next element of the collection and updated state.
	  * This is roughly equivalent to generating a sequence of states and mapping it to the desired element type.
	  * This method variant uses a generator function returning an `Option` containing the state and collection element.
	  * It is a two-step method to separate the type argument of the created collection from the element and state types.
	  * Only the former needs to be provided here explicitly, while the latter will be inferred from the `apply`
	  * method of the returned ephemeral object: [[net.noresttherein.slang.repeatedly.repeatingCollections.UnfoldSome#apply]].
	  * @tparam C[T] built collection type
	  * @return an object accepting the initial  `start`, argument for the generator function, a partial function
	  *         transforming state into a collection element and reduced state and returns the collection containing
	  *         all values returned by the latter for as long as it could be applied.
	  * @see [[net.noresttherein.slang.repeatedly.repeatingCollections.UnfoldPartialFunction]]
	  * @see [[net.noresttherein.slang.repeatedly.unfoldSome unfoldSome]]
	  */
	def unfoldSomeAs[C[T]] :UnfoldSome[C] = new UnfoldSome[C] {}



	/** Apply the given partial function recursively to its own result, starting from the given value, and return
	  * a collection of all partial results. Recursion stops once the function can no longer be applied to the argument
	  * it returned, and all first elements of returned pairs are returned in a collection built with the builder
	  * provided by the implicit factory. This operation is in a manner the opposite of folding a collection and
	  * roughly equivalent to generating a sequence of states and mapping it to the desired element type.
	  * @param start initial argument for the function
	  * @param next a partial function, returning a pair consisting of an element of the result, and the next
	  *             argument to itself.
	  * @tparam S type of the initial argument of the function
	  * @tparam X element type of the returned collection
	  * @return a collection containing first elements of all invocations of `next` in the order of execution.
	  */
	@inline final def inscribe[S, X](start :S)(next :PartialFunction[S, (X, S)]) :Seq[X] = unfoldSome(start)(next.lift)

	/** Apply the given function recursively to the returned successor value, starting with the given value,
	  * and a return a sequence of all intermediate results. Recursion stops once the function returns `None`,
	  * and all first elements of previously returned pairs are returned in a collection built with a builder
	  * provided by the default implicit factory `CanBuildFrom`. This operation is in a manner the opposite of folding
	  * a collection and roughly equivalent to generating a sequence of states and mapping it to the desired element type.
	  * @param start initial argument for the function
	  * @param next a function, returning a pair consisting of an element of the result, and the successor argument
	  *             to itself, or `None` to indicate termination condition.
	  * @tparam S type of the initial argument of the function
	  * @tparam X element type of the returned collection
	  * @return a collection containing first elements of all invocations of `next` in the order of execution.
	  */
	@inline final def inscribeSome[S, X](start :S)(next :S => Option[(X, S)]) :Seq[X] = unfoldSome(start)(next)



	/** Apply the given partial function recursively to the the second of its result pair, starting with the given value,
	  * and return a stream of all intermediate results. Recursion stops once the function can no longer be applied
	  * to the argument it returned, and all first elements of previously returned pairs are returned in a sequence.
	  * This is semantically equivalent to [[net.noresttherein.slang.repeatedly.unfold unfold]], but the returned stream
	  * is lazily evaluated, essentially combining the generation of a state stream and mapping over it into a single step.
	  * @param start initial argument for the function
	  * @param next a partial function, returning a pair consisting of an element of the result, and the next argument to itself.
	  * @tparam S type of the initial argument of the function
	  * @tparam X element type of the returned sequence
	  * @return a sequence containing first elements of all invocations of `next` in the order of execution.
	  * @see [[net.noresttherein.slang.repeatedly.unfold unfold]]
	  */
	def remap[S, X](start :S)(next :PartialFunction[S, (X, S)]) :Stream[X] =
		next.andThen(yx => yx._1 #:: remap(yx._2)(next)).applyOrElse(start, (_:S) => Stream.empty)








	/** Helper classes realizing the methods defined here extracted in order to prevent polluting the namespace
	  * when wildcard imports are used.
 	  */
	object repeatingCollections {

		/** Provides an `apply` method similar to `Iterable.iterate` for building collections of type `C[T]` with variable
		  * element counts by recursively applying the given partial function. Unlike the `iterate` method provided
		  * by companion objects of all scala collections, the number of elements may be unknown at the point of calling.
		  * @see [[net.noresttherein.slang.repeatedly.repeatingCollections.IteratePartialFunction#apply]]
		  * @tparam C[T] a collection type constructor
		  */
		trait IteratePartialFunction[C[T]] extends Any {
			/** Builds the collection `C[X]` by recursively reapplying the given partial function to the initial element.
			  * This method returns once `next` is no longer applicable to the last returned element, meaning that
			  * it must have an inbuilt termination condition.
			  * @param start first element added to the collection
			  * @param next generator function returning subsequent elements for the collection based on the previous one,
			  *             serving as the termination condition by indicating that it can no longer be applied to the given argument.
			  * @param cbf implicit builder factory determining the type of the returned collection
			  * @tparam X element type of the generated collection
			  * @return a collection containing the sequence starting with `start` and resulting from recursively applying `next` to itself.
			  */
			@inline final def apply[X](start :X)(next :PartialFunction[X, X])(implicit cbf :CanBuildFrom[_, X, C[X]]) :C[X] =
				iterateSome(start)(next.lift)
		}


		/** Provides an `apply` method similar to `Iterable.iterate` for building collections of type `C[T]` with variable
		  * element counts by recursively applying the given function. Unlike the `iterate` method provided
		  * by companion objects of all scala collections, the number of elements may be unknown at the point of calling
		  * and is instead determined by the generator function returning `None`.
		  * @see [[net.noresttherein.slang.repeatedly.repeatingCollections.IterateSome#apply]]
		  * @tparam C[T] a collection type constructor
		  */
		trait IterateSome[C[T]] extends Any {
			/** Builds the collection `C[X]` by recursively reapplying the given partial function to the initial element.
			  * This method returns once `next` returns `None`, meaning that it must have an inbuilt termination condition.
			  * @param start first element added to the collection
			  * @param next generator function returning subsequent elements for the collection based on the previous one,
			  *             or `None` to indicate the end of recursion
			  * @param cbf implicit builder factory determining the type of the returned collection
			  * @tparam X element type of the generated collection
			  * @return a collection containing the sequence starting with `start` and resulting from recursively applying `next` to itself.
			  */
			@inline final def apply[X](start :X)(next :X=>Option[X])(implicit cbf :CanBuildFrom[_, X, C[X]]) :C[X] =
				iterateSome(start)(next)

		}



		/** Provides an `apply` method which recursively applies a given generator function, starting with some initial
		  * state, to obtain the updated state and the next collection element.
		  * @tparam C[X] return collection type built by the implicit `CanBuildFrom`
		  * @see [[net.noresttherein.slang.repeatedly.repeatingCollections.UnfoldPartialFunction#apply apply]]
		  */
		trait UnfoldPartialFunction[C[T]] extends Any {
			/** Apply the given partial function recursively to its own result, starting from the given value, and return
			  * a collection of all partial results. Recursion stops once the function can no longer be applied to the argument
			  * it returned, and all first elements of returned pairs are returned in a collection built with the builder
			  * provided by the implicit factory.
			  * @param start initial argument for the function
			  * @param next a partial function, returning a pair consisting of an element of the result, and the next
			  *             argument to itself.
			  * @param cbf implicit factory of builders specifying the result type.
			  * @tparam S type of the initial argument of the function
			  * @tparam X element type of the returned collection
			  * @return a collection containing first elements of all invocations of `next` in the order of execution.
			  */
			@inline final def apply[S, X](start :S)(next :PartialFunction[S, (X, S)])(implicit cbf :CanBuildFrom[_, X, C[X]]) :C[X] =
				unfoldSome(start)(next.lift)
		}



		/** Provides an `apply` method which recursively applies a given generator function, starting with some initial
		  * state, to obtain the updated state and the next collection element.
		  * @tparam C[X] return collection type built by the implicit `CanBuildFrom`
		  * @see [[net.noresttherein.slang.repeatedly.repeatingCollections.UnfoldSome#apply apply]]
		  */
		trait UnfoldSome[C[T]] extends Any {
			/** Apply the given function recursively to the returned successor value, starting with the given value,
			  * and a return a sequence of all intermediate results. Recursion stops once the function returns `None`,
			  * and all first elements of previously returned pairs are returned in a collection built with a builder
			  * provided by the default implicit factory `CanBuildFrom`.
			  * In essence, this is a function combining generation of a stream from a recursive partial function and mapping
			  * over that stream to a single step.
			  * @param start initial argument for the function
			  * @param next a function, returning a pair consisting of an element of the result, and the successor argument
			  *             to itself, or `None` to indicate termination condition.
			  * @tparam S type of the initial argument of the function
			  * @tparam X element type of the returned collection
			  * @return a collection containing first elements of all invocations of `next` in the order of execution.
			  */
			@inline final def apply[S, X](start :S)(next :S => Option[(X, S)])(implicit cbf :CanBuildFrom[_, X, C[X]]) :C[X] =
				unfoldSome(start)(next)

		}

	}


}
