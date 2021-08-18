package net.noresttherein.slang

import scala.annotation.tailrec
import scala.collection.{IterableOnce, IterableOps, LinearSeq}


/**
  * @author Marcin Mościcki
  */
package object collection {

	/** Additional extension methods for collections of the standard library framework. */
	implicit class IterableExtension[C[X] <: Iterable[X], E](private val self :IterableOps[E, C, C[E]]) extends AnyVal {
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

		/** Maps the elements of the collection in the reverse order. The operation is faster than
		  * `this.map(f).reverse`. */
		def reverseMap[O](f :E => O) :C[O] = self match {
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
