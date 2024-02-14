package net.noresttherein.sugar.collections

import scala.annotation.tailrec
import scala.collection.immutable.StrictOptimizedSeqOps
import scala.collection.mutable
import scala.collection.mutable.IndexedBuffer

import net.noresttherein.sugar.collections.IndexedIterable.ApplyPreferred
import net.noresttherein.sugar.funny.generic.Any1




@SerialVersionUID(Ver)
private object ReversedSeq {
	def apply[E](seq :collection.IndexedSeq[E]) :collection.IndexedSeq[E] = seq match {
		case seq :IndexedSeq[E]                           => apply(seq)
		case seq :mutable.IndexedSeq[E]                   => apply(seq)
		case _ :collection.StrictOptimizedSeqOps[_, _, _] =>
			new ReversedSeq(seq) with collection.StrictOptimizedSeqOps[E, collection.IndexedSeq, collection.IndexedSeq[E]]
		case _                                            => new ReversedSeq(seq)
	}
	def apply[E](seq :IndexedSeq[E]) :IndexedSeq[E] =
		if (seq.isInstanceOf[StrictOptimizedSeqOps[_, Any1, _]])
			new ImmutableReversedSeq(seq) with StrictOptimizedSeqOps[E, IndexedSeq, IndexedSeq[E]]
		else
			new ImmutableReversedSeq(seq)

	def apply[E](seq :mutable.IndexedSeq[E]) :mutable.IndexedSeq[E] =
		if (seq.isInstanceOf[collection.StrictOptimizedSeqOps[_, Any1, _]])
			new MutableReversedSeq(seq) with collection.StrictOptimizedSeqOps[E, mutable.IndexedSeq, mutable.IndexedSeq[E]]
		else
			new MutableReversedSeq(seq)

	def apply[E](seq :IndexedBuffer[E]) :IndexedBuffer[E] =
		if (seq.isInstanceOf[collection.StrictOptimizedSeqOps[_, Any1, _]])
			new ReversedIndexedBuffer[E](seq) with collection.StrictOptimizedSeqOps[E, IndexedBuffer, IndexedBuffer[E]]
		else
			new ReversedIndexedBuffer(seq)
}


/** A view of an `IndexedSeq` reversing the order of elements. */
@SerialVersionUID(Ver) //consider: makng it a GenIndexedSeqRange[E]; problem: trustedSlice must return range
private sealed class ReversedSeq[+E](underlying :collection.IndexedSeq[E])
	extends collection.AbstractSeq[E] with collection.IndexedSeq[E]
	   with SugaredSlicingOps[E, collection.IndexedSeq, collection.IndexedSeq[E]]
{
//	private[this] val len = underlying.length
//	override def length :Int = len
	override def length = underlying.length

	override def apply(i :Int) :E =
		if (i < 0) throw new IndexOutOfBoundsException(i.toString + " out of " + underlying.length)
		else underlying(underlying.length - i - 1)

//	protected override def hasFastSlice = HasFastSlice(underlying)
	protected override def clippedSlice(from :Int, until :Int) :collection.IndexedSeq[E] =
		if (HasFastSlice(underlying)) {
			val len = underlying.length
			new ReversedSeq(underlying.slice(len - until, len - from))
		} else
			super[IndexedSeq].slice(from, until)

	//todo: test this
	override def segmentLength(p :E => Boolean, from :Int) :Int =
		if (from >= underlying.length)
			0
		else if (ApplyPreferred(underlying))
			super[IndexedSeq].segmentLength(p, from)
		else {
			val until = underlying.length - math.max(from, 0)
			val i = underlying.lastIndexWhere(!p(_), until - 1)
			until - i - 1
		}

	override def iterator :Iterator[E] = underlying.reverseIterator
	override def reverseIterator :Iterator[E] = underlying.iterator

	override def reverse :collection.IndexedSeq[E] = underlying
	protected override def reversed :Iterable[E] = underlying
}


@SerialVersionUID(Ver)
private class ImmutableReversedSeq[+E](override val reverse :IndexedSeq[E])
	extends ReversedSeq[E](reverse) with IndexedSeq[E] with SugaredSlicingOps[E, IndexedSeq, IndexedSeq[E]]
{
	protected override def clippedSlice(from :Int, until :Int) :IndexedSeq[E] =
		if (HasFastSlice(reverse))
			ReversedSeq(reverse.slice(length - until, length - from))
		else
			super[IndexedSeq].slice(from, until)
}


@SerialVersionUID(Ver)
private class MutableReversedSeq[E](override val reverse :mutable.IndexedSeq[E])
	extends ReversedSeq[E](reverse) with mutable.IndexedSeq[E]
	   with SugaredSlicingOps[E, mutable.IndexedSeq, mutable.IndexedSeq[E]]
{
	override def update(idx :Int, elem :E) :Unit =
		if (idx < 0) throw new IndexOutOfBoundsException(idx.toString + " out of " + length)
		else reverse(length - idx - 1) = elem

	protected override def clippedSlice(from :Int, until :Int) :mutable.IndexedSeq[E] =
		if (HasFastSlice(reverse))
			ReversedSeq(reverse.slice(length - until, length - from))
		else
			super[IndexedSeq].slice(from, until)
}




@SerialVersionUID(Ver)
private object ReversedIndexedBuffer extends BufferFactory[IndexedBuffer] {
	def reversed[E](buffer :IndexedBuffer[E]) :IndexedBuffer[E] =
		if (buffer.isInstanceOf[collection.StrictOptimizedSeqOps[_, Any1, _]])
			new ReversedIndexedBuffer(buffer) with StrictOptimizedSeqOps[E, IndexedBuffer, IndexedBuffer[E]]
		else
			new ReversedIndexedBuffer(buffer)

	override def ofCapacity[E](capacity :Int) :IndexedBuffer[E] = reversed(TemporaryBuffer.ofCapacity(capacity))

	override def empty[A] :IndexedBuffer[A] = reversed(TemporaryBuffer.empty[A])
}


@SerialVersionUID(Ver)
private class ReversedIndexedBuffer[E](override val reverse :IndexedBuffer[E])
	extends MutableReversedSeq[E](reverse) with IndexedBuffer[E]
	   with SugaredSlicingOps[E, IndexedBuffer, IndexedBuffer[E]]
{
	protected override def clippedSlice(from :Int, until :Int) :IndexedBuffer[E] =
		if (HasFastSlice(reverse)) {
			val len = reverse.length
			ReversedSeq(reverse.slice(len - until, len - from)) //so it creates a strict instance, if necessary.
		} else
			super[IndexedBuffer].slice(from, until)

	override def addOne(elem :E) :this.type = { reverse.prepend(elem); this }
	override def prepend(elem :E) :this.type = { reverse.addOne(elem); this }
	override def addAll(xs :IterableOnce[E]) :this.type = { reverse.prependAll(reverseOther(xs)); this }
	override def prependAll(elems :IterableOnce[E]) :this.type = { reverse.addAll(reverseOther(elems)); this }

	override def insert(idx :Int, elem :E) :Unit =
		if (idx < 0)
			throw new IndexOutOfBoundsException(idx.toString + " out of " + length)
		else
			reverse.insert(reverse.length - idx, elem)

	override def insertAll(idx :Int, elems :IterableOnce[E]) :Unit =
		reverse.insertAll(reverse.length - idx, reverseOther(elems))

	@tailrec final override def patchInPlace(from :Int, patch :IterableOnce[E], replaced :Int) :this.type =
		if (from <= 0)
			patchInPlace(0, patch, replaced)
		else if (from > reverse.length)
			patchInPlace(reverse.length, patch, replaced)
		else {
			reverse.patchInPlace(reverse.length - from, reverseOther(patch), replaced)
			this
		}

	override def remove(idx :Int) :E =
		if (idx < 0) throw new IndexOutOfBoundsException(idx.toString + " out of " + length)
		else reverse.remove(reverse.length - 1 - idx)

	override def remove(idx :Int, count :Int) :Unit =
		if (idx < 0) throw new IndexOutOfBoundsException(idx.toString + " out of " + length)
		else reverse.remove(reverse.length - idx, count)

	override def clear() :Unit = reverse.clear()

	private def reverseOther(elems :IterableOnce[E]) :IterableOnce[E] = elems match {
		case seq :ReversedSeq[E] => seq.reverse
		case seq :collection.IndexedSeq[E] => ReversedSeq(seq)
		case _ => util.reverse(elems)
	}
}
