package net.noresttherein.sugar.collections

import scala.collection.mutable

import net.noresttherein.sugar.collections.IndexedIterable.ApplyPreferred




/** A view of an `IndexedSeq` reversing the order of elements. */
private sealed class ReversedSeq[+E](underlying :collection.IndexedSeq[E])
	extends collection.AbstractSeq[E] with collection.IndexedSeq[E] with SlicingOps[E, collection.IndexedSeq[E]]
{
	private[this] val len = underlying.length
	override def length :Int = len

	override def apply(i :Int) :E =
		if (i < 0) throw new IndexOutOfBoundsException(i.toString + " out of " + len)
		else underlying(len - i - 1)

	protected override def trustedSlice(from :Int, until :Int) :collection.IndexedSeq[E] =
		if (HasFastSlice(underlying))
			new ReversedSeq(underlying.slice(len - until, len - from))
		else
			super[IndexedSeq].slice(from, until)

	//todo: test this
	override def segmentLength(p :E => Boolean, from :Int) :Int =
		if (from >= len)
			0
		else if (ApplyPreferred(underlying))
			super[IndexedSeq].segmentLength(p, from)
		else {
			val until = len - math.max(from, 0)
			val i = underlying.lastIndexWhere(!p(_), until - 1)
			until - i - 1
		}

	override def iterator :Iterator[E] = underlying.reverseIterator
	override def reverseIterator :Iterator[E] = underlying.iterator

	override def reverse :collection.IndexedSeq[E] = underlying
	protected override def reversed :Iterable[E] = underlying
}


private object ReversedSeq {
	def apply[E](seq :collection.IndexedSeq[E]) :collection.IndexedSeq[E] = seq match {
		case seq :IndexedSeq[E]         => new Immutable(seq)
		case seq :mutable.IndexedSeq[E] => new Mutable(seq)
		case _                          => new ReversedSeq(seq)
	}
	def apply[E](seq :IndexedSeq[E]) :IndexedSeq[E] = new Immutable(seq)

	private class Immutable[+E](override val reverse :IndexedSeq[E])
		extends ReversedSeq[E](reverse) with IndexedSeq[E] with SlicingOps[E, IndexedSeq[E]]
	{
		protected override def trustedSlice(from :Int, until :Int) :IndexedSeq[E] =
			if (HasFastSlice(reverse))
				new Immutable(reverse.slice(length - until, length - from))
			else
				super[IndexedSeq].slice(from, until)
	}

	private class Mutable[E](override val reverse :mutable.IndexedSeq[E])
		extends ReversedSeq[E](reverse) with mutable.IndexedSeq[E] with SlicingOps[E, mutable.IndexedSeq[E]]
	{
		override def update(idx :Int, elem :E) :Unit =
			if (idx < 0) throw new IndexOutOfBoundsException(idx.toString + " out of " + length)
			else reverse(length - idx - 1) = elem

		protected override def trustedSlice(from :Int, until :Int) :mutable.IndexedSeq[E] =
			if (HasFastSlice(reverse))
				new Mutable(reverse.slice(length - until, length - from))
			else
				super[IndexedSeq].slice(from, until)

	}
}
