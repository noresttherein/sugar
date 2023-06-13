
package net.noresttherein.sugar.collections

import scala.collection.immutable.AbstractSeq





/** A view of an `IndexedSeq` reversing the order of elements. */
private final class ReversedSeq[+T](underlying :IndexedSeq[T]) extends AbstractSeq[T] with IndexedSeq[T] {
	private[this] val len = underlying.length
	override def length :Int = len

	override def apply(i :Int) :T =
		if (i < 0) throw new IndexOutOfBoundsException(i.toString + " out of " + len)
		else underlying(len - i - 1)

	override def iterator :Iterator[T] = underlying.reverseIterator
	override def reverseIterator :Iterator[T] = underlying.iterator

	override def reverse :IndexedSeq[T] = underlying
}


object ReversedSeq {
	def apply[T](seq :IndexedSeq[T]) :IndexedSeq[T] = new ReversedSeq(seq)
}
