package net.noresttherein.slang

import scala.annotation.tailrec
import scala.collection.{IterableFactory, IterableOnce, IterableOps, LinearSeq}

import net.noresttherein.slang.optional.Opt
import net.noresttherein.slang.optional.Opt.{Got, Lack}


/**
  * @author Marcin Mościcki
  */
package object collection {

	/** A syntactic wrapper for collections, injecting methods implementing
	  * 'breakable' `foldLeft`, that is folding only some prefix of the collection
	  * based on some predicate or other termination condition.
	  *
	  * @param items any collection to fold.
	  * @tparam T element type of this collection.
	  */
	implicit class foldingMethods[T](private val items :IterableOnce[T]) extends AnyVal {
		/** Applies the given folding function `op` to the elements of this collection starting with the given
		  * initial value `start` while the predicate `until` is false for the most recently computed value.
		  * Note that this is not equivalent to `foldWhile(start)(!until(_))(op)` as the recursion goes
		  * one step further, returning the first element which satisfies the given condition,
		  * where a negated `foldWhile` would return the result of the last iteration which did not.
		  * Another difference is that `until` is applied starting with `op(start, this.head)`
		  * (assuming this collection is not empty), rather than `op(start)` as `foldWhile` would.
		  * @param start an initial value.
		  * @param until a predicate which needs to be satisfied for the folding to stop.
		  * @param op    a function generating subsequent values based on the previously computed value
		  *              and the next element of the collection.
		  * @tparam A the type of generated and tested values.
		  * @return `start` if `this.isEmpty`, the first generated value which satisfies predicate `until`,
		  *         or the result of folding the whole collection if no such element was computed.
		  */
		def foldUntil[A](start :A)(op :(A, T) => A)(until :A => Boolean) :A = {
			var last = start; val i = items.iterator
			while (i.hasNext && { last = op(last, i.next()); !until(last) })
				{}
			last
		}


		/** Applies the given folding function `op` to the elements of this collection starting with the given
		  * initial value `start` while the predicate `pred` is true for the most recently computed value.
		  * The predicate is applied for the first time to `op(start, head)` (assuming this collection is not empty),
		  * not to `start` itself. If this collection is empty, then `start` is returned immediately,
		  * regardless of whether it satisfies the condition. If all generated values satisfy `pred`,
		  * then this method is equivalent to `foldLeft(start)(op)`.
		  * Note that this is not equivalent to `foldUntil(start)(!pred(_))(op)`, as the latter would apply `op`
		  * one more time unless the end of collection is reached without falsifying the predicate.
		  * @param start an initial value.
		  * @param pred  a predicate which needs to be satisfied for folding to continue.
		  * @param op    a function generating subsequent values based on the previously computed value
		  *              and the next element of the collection.
		  * @tparam A    the type of generated and tested values.
		  * @return      `start` if this collection is empty or `op(start, this.head)` is false,
		  *              or the result of the last application of `op` which still satisfied `pred`.
		  */
		def foldWhile[A](start :A)(pred :A => Boolean)(op :(A, T) => A) :A = {
			var last = start; var next = start
			val i = items.iterator
			while (i.hasNext && pred({ next = op(last, i.next()); next }))
				last = next
			last
		}

		/** Applies the given folding function `op` to the elements of this collection starting with the given initial
		  * value `start` for as long as `op` is defined for the previously computed value.
		  * @param start accumulator given as the first argument to first invocation of `op`.
		  * @param op a partial function combining the accumulator with the elements of the collection working
		  *           as the breaking condition when non-defined.
		  * @tparam A type of generated values.
		  * @return result of `this.foldLeft(a)(op)` or first value `a :A` such that `op` is not defined
		  *         for `(a, e)` where `e` is the first non-folded element of this collection.
		  */
		def partialFold[A](start :A)(op :PartialFunction[(A, T), A]) :A = {
			val lift = op.lift
			foldSome(start) { (acc, elem) => lift((acc, elem)) }
		}

		/** Applies the given folding function to the elements of this collection and current accumulator value
		  * for as long as it returns non-empty results.
		  * @param start initial accumulator value passed to the first call of `op` together with the first element
		  *              of this collection.
		  * @param op a function generating consecutive values of `A` from the previous value and subsequent element
		  *           of this collection, yielding `None` to signal the break condition for folding.
		  * @tparam A type of generated values.
		  * @return the result of the last execution of `op` which returned `Some`,
		  *         or `start` if this collection is empty or `op(start, this.head) == None`.
		  */
		def foldSome[A](start :A)(op :(A, T) => Option[A]) :A = items match {
			case it :Iterable[_] if it.isEmpty => start
			case it :Iterator[_] if it.isEmpty => start
			case _ =>
				val it = items.iterator
				var last = start
				while (op(last, it.next()) match {
					case Some(next) => last = next; true
					case _ => false
				}) {}
				last
		}
	}



	/** Additional extension methods for collections of the standard library framework.
	  * The common theme is performing mapping with help of a passed state/accumulator value.
	  */
	implicit class mappingMethods[C[X] <: Iterable[X], E](private val self :IterableOps[E, C, C[E]])
		extends AnyVal
	{
		/** Maps this collection from left to right with an accumulating state updated by the mapping function.
		  * The state is discarded after the operation and only the mapping results (the second elements
		  * of the tuples returned by the given function) are returned in a collection of the same dynamic type
		  * as this collection.
		  */
		def mapWith[A, O](z :A)(f :(A, E) => (A, O)) :C[O] =
			self.view.scanLeft((z, null.asInstanceOf[O])) { //safe because null is never passed to f and we are in an erased context
				(acc, e) => f(acc._1, e)
			}.tail.map(_._2).to(self.iterableFactory)

		/** Flat maps this collection from left to right with an accumulating state updated by the mapping function.
		  * The state is discarded after the operation and only the mapping results (the collections returned by
		  * by the given function) are returned in a collection of the same dynamic type as this collection.
		  */
		def flatMapWith[A, O](z :A)(f :(A, E) => (A, IterableOnce[O])) :C[O] =
			self.view.scanLeft((z, Nil :IterableOnce[O])) {
				(acc, e) => f(acc._1, e)
			}.flatMap(_._2).to(self.iterableFactory)

		/** Maps this collection in order consistent with `foreach`, passing as the first argument the index
		  * of the mapped element.
		  */
		def mapWithIndex[O](f :(Int, E) => O) :C[O] = {
			var i = 0
			val b = self.iterableFactory.newBuilder[O]
			b sizeHint self
			self foreach { e => b += f(i, e); i += 1 }
			b.result()
		}

		/** Flat maps this collection in order consistent with `foreach`, passing as the first argument the index
		  * of the mapped element.
		  */
		def flatMapWithIndex[O](f :(Int, E) => IterableOnce[O]) :C[O] = {
			var i = 0
			val b = self.iterableFactory.newBuilder[O]
			self foreach { e => b ++= f(i, e); i += 1 }
			b.result()
		}

		/** Maps the elements of the collection and reverses their order. The order in which the mapping function
		  * will be applied to the elements is undefined and depends on the runtime type of this collection.
		  * Note that if this collection is unordered, the order of the elements in the mapped collection
		  * is likewise undefined and depends on the implementation of this collection's builder.
		  * This operation is faster than `this.map(f).reverse`.
		  */
		def reverseMap[O](f :E => O) :C[O] = self match {
			case _ if self.isEmpty =>
				self.iterableFactory.empty
			case list :List[E] =>
				@tailrec def mapList(unmapped :List[E], mapped :List[O]) :List[O] = unmapped match {
					case h::t => mapList(t, f(h)::mapped)
					case _ => mapped
				}
				mapList(list, Nil).asInstanceOf[C[O]]
			case list :LinearSeq[E] =>
				@tailrec def mapLinear(unmapped :LinearSeq[E], mapped :LinearSeq[O]) :LinearSeq[O] =
					if (unmapped.isEmpty) mapped
					else mapLinear(unmapped.tail, f(unmapped.head) +: mapped)
				mapLinear(list, list.iterableFactory.empty).asInstanceOf[C[O]]
			case seq :scala.collection.IndexedSeq[E] =>
				def mapIndexed() = {
					val b = self.iterableFactory.newBuilder[O]
					var i = seq.length
					while (i > 0) {
						i -= 1
						b += f(seq(i))
					}
					b.result()
				}
				mapIndexed()
			case seq :scala.collection.Seq[E] =>
				def mapSeq() = {
					val i = seq.reverseIterator
					val b = self.iterableFactory.newBuilder[O]
					b sizeHint self
					while (i.hasNext)
						b += f(i.next())
					b.result()
				}
				mapSeq()
			case _ =>
				def mapIterable() = {
					val mapped = (List.empty[O] /: self){ (acc, e) => f(e)::acc }
					val b = self.iterableFactory.newBuilder[O]
					b sizeHint self
					b ++= mapped
					b.result()
				}
				mapIterable()
		}
	}



	/** Alternative, safer implementations of [[scala.collection.SeqOps.indexOf indexOf]] for [[Seq]],
	  * which do not return a negative index when the element is not found.
	  */
	implicit class indexOfMethods[X](private val self :scala.collection.Seq[X]) extends AnyVal {
		@inline def length :Int = self.length

		/** Finds the location of the given element in this sequence, returning its index as an option.
		  * @param x    the element, whose index is to be determined.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def getIndexOf(x :X, from :Int = 0) :Opt[Int] = self.indexOf(x, from) match {
			case n if n >= 0 => Got(n)
			case _ => Lack
		}
		/** Finds the last location of the given element in this sequence, returning its index as an option.
		  * @param x   the element, whose index is to be determined.
		  * @param end the upper, inclusive bound on the returned index.
		  */
		@inline def getLastIndexOf(x :X, end :Int = length - 1) :Opt[Int] = self.lastIndexOf(x, end) match {
			case n if n >= 0 => Got(n)
			case _ => Lack
		}
		/** Finds an element of this sequence which satisfies the predicate, returning its index as an option.
		  * @param p    a function applied consecutively to all elements with indices greater or equal `from`.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def getIndexWhere(p :X => Boolean, from :Int = 0) :Opt[Int] = self.indexOf(p, from) match {
			case n if n >= 0 => Got(n)
			case _ => Lack
		}
		/** Finds the last element of this sequence which satisfies the predicate, returning its index as an option.
		  * @param p   a function applied consecutively to all elements with indices lesser or equal `end`,
		  *            in a decreasing order.
		  * @param end the upper, inclusive bound on the returned index; elements after this position will not be checked.
		  */
		@inline def getLastIndexWhere(p :X => Boolean, end :Int = length - 1) :Opt[Int] =
			self.lastIndexWhere(p, end) match {
				case n if n >= 0 => Got(n)
				case _ => Lack
			}
		/** Finds the location of the given element in this sequence, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param x    the element, whose index is to be determined.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def sureIndexOf(x :X, from :Int = 0) :Int = self.indexOf(x, from) match {
			case n if n >= 0 => n
			case _ => throw new NoSuchElementException(
				"No " + x + " in " + self + (if (from == 0) "." else " after index " + (from-1) + "-th element.")
			)
		}
		/** Finds the last location of the given element in this sequence, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param x   the element, whose index is to be determined.
		  * @param end the upper, inclusive bound on the returned index.
		  */
		@inline def sureLastIndexOf(x :X, end :Int = length - 1) :Int = self.lastIndexOf(x, end) match {
			case n if n >= 0 => n
			case _ => throw new NoSuchElementException(
				"No " + x + " in " + self + (if (end == self.length - 1) "." else " before index " + (end + 1) + ".")
			)
		}
		/** Finds an element of this sequence which satisfies the predicate, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param p    a function applied consecutively to all elements starting with the first one.
		  */
		@inline def sureIndexWhere(p :X => Boolean) :Int = self.indexWhere(p) match {
			case n if n >= 0 => n
			case _ => throw new NoSuchElementException("No element satisfying the predicate in " + self + ".")
		}
		/** Finds an element of this sequence which satisfies the predicate, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param p    a function applied consecutively to all elements with indices greater or equal `from`,
		  *             until satisfied.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def sureIndexWhere(p :X => Boolean, from :Int) :Int = self.indexWhere(p, from) match {
			case n if n >= 0 => n
			case _ => throw new NoSuchElementException(
				"No element satisfying the predicate in " + self + " after index " + (from - 1) + "."
			)
		}
		/** Finds the last element of this sequence which satisfies the predicate, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param p   a function applied consecutively to all elements, starting with the last one, until satisfied.
		  */
		@inline def sureLastIndexWhere(p :X => Boolean) :Int = self.lastIndexWhere(p) match {
			case n if n >= 0 => n
			case _ => throw new NoSuchElementException("No element satisfying the predicate in " + self + ".")
		}
		/** Finds the last element of this sequence which satisfies the predicate, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param p   a function applied consecutively to all elements, starting with the one at position `end`,
		  *            until satisfied.
		  * @param end the upper, inclusive bound on the returned index; elements after this index are not checked.
		  */
		@inline def sureLastIndexWhere(p :X => Boolean, end :Int) :Int = self.lastIndexWhere(p, end) match {
			case n if n >= 0 => n
			case _ => throw new NoSuchElementException(
				"No element satisfying the predicate in " + self + " before index " + (end + 1) + "."
			)
		}
	}



	/** Extension methods for [[scala.collection.IterableFactory IterableFactory]], the most common type of
	  * companion objects for collection types conforming to the Scala collection framework.
	  * Provides additional generator methods which construct the collection of the proper type.
	  */
	implicit class IterableFactoryExtension[C[_]](private val companion :IterableFactory[C]) extends AnyVal {

		/** A complement of `C.iterate` and `C.unfold` provided by collection companion objects, which creates
		  * a collection `C` by recursively applying a partial function while defined to its own results and collecting
		  * all returned values. It is very similar to the standard [[scala.collection.IterableFactory.iterate iterate]],
		  * but instead of a fixed number of iterations, the generator function `next` is called for its return values
		  * until it is no longer applicable, which marks the end of the collection.
		  * @param start first element added to the collection.
		  * @param next generator function returning subsequent elements for the collection based on the previous one,
		  *             serving as the termination condition by indicating that it can no longer be applied
		  *             to the given argument.
		  * @tparam X element type of the generated collection.
		  * @return a collection containing the sequence starting with `start` and resulting from recursively applying
		  *         `next` to itself.
		  */
		@inline final def generate[X](start :X)(next :PartialFunction[X, X]) :C[X] =
			expand(start)(next.lift)

		/** Builds the collection `C[X]` by recursively reapplying the given partial function to the initial element.
		  * Instead of listing a fixed number of elements, this method uses the generator function `next` as the termination
		  * condition and returns once it returns `None`. It is the opposite
		  * of [[scala.collection.IterableOnceOps.reduce reduce]] in the same way as
		  * [[scala.collection.IterableFactory.unfold unfold]] is the opposite
		  * of [[scala.collection.IterableOnceOps.fold fold]].
		  * @param start first element added to the collection.
		  * @param next generator function returning subsequent elements for the collection based on the previous one,
		  *             or `None` to indicate the end of recursion.
		  * @tparam X element type of the generated collection
		  * @return a collection containing the sequence starting with `start` and resulting from recursively applying `next`
		  *         to itself.
		  */
		@inline final def expand[X](start :X)(next :X => Option[X]) :C[X] =
			companion match {
				case LazyList =>
					(start #:: (next(start).map(LazyList.expand(_)(next)) getOrElse LazyList.empty)).asInstanceOf[C[X]]
				case _ =>
					val builder = companion.newBuilder[X]
					builder += start
					@tailrec def rec(x :X = start) :C[X] = next(x) match {
						case Some(y) => builder += y; rec(y)
						case None => builder.result()
					}
					rec()
			}

	}
}
