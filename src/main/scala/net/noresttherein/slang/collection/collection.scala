package net.noresttherein.slang

import scala.annotation.tailrec
import scala.collection.{IterableOnce, IterableOps, LinearSeq}


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
	implicit class foldingMethods[T](private val items :Iterable[T]) extends AnyVal {
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


	/** Additional extension methods for collections of the standard library framework. */
	implicit class mappingMethods[C[X] <: Iterable[X], E](private val self :IterableOps[E, C, C[E]])
		extends AnyVal
	{
		/** Maps this collection from left to right with an accumulating state updated by the mapping function.
		  * The state is discarded after the operation and only the mapping results (the second elements
		  * of the tuples returned by the given function) are returned in a collection of the same dynamic type
		  * as this collection.
		  */
		def mapWith[A, O](z :A)(f :(A, E) => (A, O)) :C[O] =
			self.view.scanLeft((z, null.asInstanceOf[O])) {
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
}
