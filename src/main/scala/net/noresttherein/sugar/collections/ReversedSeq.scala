package net.noresttherein.sugar.collections

import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.StrictOptimizedSeqOps
import scala.collection.{SeqFactory, mutable}
import scala.collection.mutable.{AbstractBuffer, Buffer, IndexedBuffer}

import net.noresttherein.sugar.collections.extensions.{BufferExtension, IterableOnceExtension}
import net.noresttherein.sugar.collections.util.{HasFastReverse, errorString}
import net.noresttherein.sugar.exceptions.{illegal_!, outOfBounds_!}
import net.noresttherein.sugar.typist.kinds.Any1




@SerialVersionUID(Ver)
private object ReversedSeq { //todo: make these somehow available to applications.
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

	def apply[E](seq :mutable.IndexedSeq[E]) :mutable.IndexedSeq[E] = seq match {
		case buffer :IndexedBuffer[E]                     => apply(buffer)
		case _ :collection.StrictOptimizedSeqOps[_, _, _] =>
			new MutableReversedSeq(seq) with collection.StrictOptimizedSeqOps[E, mutable.IndexedSeq, mutable.IndexedSeq[E]]
		case _                                            => new MutableReversedSeq(seq)
	}

	def apply[E](seq :IndexedBuffer[E]) :IndexedBuffer[E] = ReversedIndexedBuffer(seq)

	def apply[E](seq :Buffer[E]) :Buffer[E] = ReversedBuffer(seq)
}


//todo: rename it to ImpureReversedSeq and ImmutableReversedSeq to ReversedSeq
/** A view of an `IndexedSeq` reversing the order of elements. */
@SerialVersionUID(Ver) //consider: making it a ImpureIndexedSubseq[E]; problem: trustedSlice must return range
private sealed class ReversedSeq[+E](underlying :collection.IndexedSeq[E])
	extends collection.AbstractSeq[E] with collection.IndexedSeq[E]
	   with SugaredSlicingOps[E, collection.IndexedSeq, collection.IndexedSeq[E]] with DefaultSerializable
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

	override def segmentLength(p :E => Boolean, from :Int) :Int = super[IndexedSeq].segmentLength(p, from)

	override def iterator :Iterator[E] = underlying.reverseIterator
	override def reverseIterator :Iterator[E] = underlying.iterator

	//This means that no public method of SeqOps returns the underlying sequence. Good or bad?
	/** Returns a new $coll with the elements of this $coll in reverse order.
	  * Use [[net.noresttherein.sugar.collections.ReversedSeq.reversed reversed]] to obtain the underlying sequence.
	  * @note for mutable sequences, this will not be the underlying sequence, but its copy!
	  */
	override def reverse :collection.IndexedSeq[E] = underlying.drop(0)

	/** Returns the underlying sequence of which this instance is a reverse view. */
	override def reversed :collection.IndexedSeq[E] = underlying

	protected[this] override def className = "ReversedSeq"
}


@SerialVersionUID(Ver)
private sealed class ImmutableReversedSeq[+E](override val reverse :IndexedSeq[E])
	extends ReversedSeq[E](reverse) with IndexedSeq[E] with SugaredSlicingOps[E, IndexedSeq, IndexedSeq[E]]
{
	protected override def clippedSlice(from :Int, until :Int) :IndexedSeq[E] =
		if (HasFastSlice(reverse))
			ReversedSeq(reverse.slice(length - until, length - from))
		else
			super[IndexedSeq].slice(from, until)

	override def reversed :IndexedSeq[E] = reverse
}


@SerialVersionUID(Ver)
private sealed class MutableReversedSeq[E](override val reversed :mutable.IndexedSeq[E])
	extends ReversedSeq[E](reversed) with mutable.IndexedSeq[E]
	   with SugaredSlicingOps[E, mutable.IndexedSeq, mutable.IndexedSeq[E]]
{
	override def update(idx :Int, elem :E) :Unit =
		if (idx < 0) throw new IndexOutOfBoundsException(idx.toString + " out of " + length)
		else reversed(length - idx - 1) = elem

	//Returns IndexedBuffer so we don't have to override it in ReversedBuffer
	protected override def fullSlice :mutable.IndexedBuffer[E] = TemporaryBuffer.empty[E] ++= this
	protected override def clippedSlice(from :Int, until :Int) :mutable.IndexedSeq[E] =
		if (HasFastSlice(reverse))
			ReversedSeq(reversed.slice(length - until, length - from))
		else
			super[IndexedSeq].slice(from, until)

	override def reverse :mutable.IndexedSeq[E] = reversed.drop(0)
}




@SerialVersionUID(Ver)
private object ReversedBuffer
	extends SeqFactory.Delegate[ReversedBuffer](ReversedIndexedBuffer) with BufferFactory[ReversedBuffer]
{
	def apply[E](buffer :Buffer[E]) :ReversedBuffer[E] = buffer match {
		case indexed :IndexedBuffer[E]                       => ReversedIndexedBuffer(indexed)
		case _ :collection.StrictOptimizedSeqOps[_, Any1, _] =>
			new Impl[E](buffer) with collection.StrictOptimizedSeqOps[E, Buffer, Buffer[E]]
		case _ =>
			new Impl[E](buffer)
	}

	override def ofCapacity[E](capacity :Int) :ReversedBuffer[E] = ReversedIndexedBuffer.ofCapacity(capacity)

	private sealed class Impl[E](override val reversed :Buffer[E])
		extends AbstractBuffer[E] with ReversedBuffer[E] with DefaultSerializable
	{
		//Not implemented in ReversedBuffer because they would conflict with ReversedSeq in ReversedIndexedBuffer.
		override def knownSize :Int = reversed.knownSize
		override def length :Int = reversed.length
		override def apply(i :Int) :E =
			if (i < 0) outOfBounds_!(i, this)
			else reversed(reversed.length - i - 1)

		override def update(idx :Int, elem :E) :Unit =
			if (idx < 0) outOfBounds_!(idx, this)
			else reversed(reversed.length - idx - 1) = elem

		override def iterator :Iterator[E] = reversed.reverseIterator
		override def reverseIterator :Iterator[E] = reversed.iterator
	}
}


/** A view of the elements of another buffer in the reverse order.
  * @define Coll `Buffer`
  * @define coll buffer
  */
@SerialVersionUID(Ver)
private sealed trait ReversedBuffer[E] extends Buffer[E] {
	/** Returns a new $coll with the elements of this $coll in reverse order.
	  * Use [[net.noresttherein.sugar.collections.ReversedBuffer.reversed reversed]] to obtain the underlying sequence.
	  * @note for mutable sequences, this will not be the underlying sequence, but its copy!
	  */
	override def reverse :Buffer[E] = reversed.drop(0)

	/** Returns the underlying buffer of which this buffer is a reverse view. */
	override val reversed :Buffer[E] = null //It's concrete in Buffer

	override def prepend(elem :E) :this.type = { reverse.append(elem); this }
	override def addOne(elem :E) :this.type = { reverse.prepend(elem); this }

	override def prependAll(elems :IterableOnce[E]) :this.type = elems match {
		case HasFastReverse(reverse) =>
			reversed addAll reverse; this
		case _ =>
			reversed.trySizeHint(elems, size)
			elems.toBasicOps.foldLeft(reversed)(_ addOne _)
			this
	}
	override def addAll(elems :IterableOnce[E]) :this.type = elems match {
		case HasFastReverse(reverse) =>
			reversed prependAll reverse; this
		case _ =>
			reversed.trySizeHint(elems, size)
			elems.toBasicOps.foldLeft(reversed)(_ prepend _)
			this
	}

	override def insert(idx :Int, elem :E) :Unit = {
		val len = reversed.length
		if (idx < 0 | idx > len)
			outOfBounds_!(idx, this, "insert")
		reversed.insert(len - idx, elem)
	}

	override def insertAll(idx :Int, elems :IterableOnce[E]) :Unit = {
		val len = reversed.length
		if (idx < 0 | idx > len)
			outOfBounds_!(idx, this, "insertAll")
		reversed.insertAll(len - idx, reverseOther(elems))
	}

	override def patchInPlace(from :Int, patch :IterableOnce[E], replaced :Int) :this.type = {
		val len = reversed.length
		val from0 = math.max(0, math.min(len, from))
		val until0 = math.max(0, math.min(len - from0, replaced))
		reversed.patchInPlace(len - until0, reverseOther(patch), until0 - from0)
		this
	}

	override def remove(idx :Int) :E = {
		val len = reversed.length
		if (idx < 0 | idx >= len)
			outOfBounds_!(idx, this, "remove")
		else
			reversed.remove(len - 1 - idx)
	}

	override def remove(idx :Int, count :Int) :Unit = {
		val len = reversed.length
		if (count < 0)
			illegal_!("Negative number " + count + " of elements to remove from " + errorString(this) + " at " + idx + ".")
		else if (idx < 0 | idx >= len - count)
			outOfBounds_!(errorString(this) + ".remove(" + idx + ", " + count + ")")
		else if (count > 0)
			reversed.remove(len - idx - count)
	}

	//sliceInPlace is implemented as drop().take(), so it should be good enough.
	override def dropInPlace(n :Int) :this.type = { reversed.dropRightInPlace(n); this }
	override def dropRightInPlace(n :Int) :this.type = { reversed.dropInPlace(n); this }
	override def takeInPlace(n :Int) :this.type = { reversed.takeRightInPlace(n); this }
	override def takeRightInPlace(n :Int) :this.type = { reversed.takeInPlace(n); this }

	override def clear() :Unit = reversed.clear()

	@inline private def reverseOther(elems :IterableOnce[E]) :IterableOnce[E] = util.reverse(elems)
//	private def reverseOther(elems :IterableOnce[E]) :IterableOnce[E] = elems match {
//		//util.reverse doesn't create a ReversedSeq, but a reverse iterator.
//		case seq :ReversedSeq[E]           => seq.reversed
//		case seq :collection.IndexedSeq[E] => ReversedSeq(seq)
//		case _                             => util.reverse(elems)
//	}

	protected[this] override def className = "ReversedBuffer"
}




@SerialVersionUID(Ver)
private object ReversedIndexedBuffer extends BufferFactory[ReversedIndexedBuffer] {
	def apply[E](buffer :IndexedBuffer[E]) :ReversedIndexedBuffer[E] =
		if (buffer.isInstanceOf[collection.StrictOptimizedSeqOps[_, Any1, _]])
			new ReversedIndexedBuffer(buffer) with collection.StrictOptimizedSeqOps[E, IndexedBuffer, IndexedBuffer[E]]
		else
			new ReversedIndexedBuffer(buffer)

	override def ofCapacity[E](capacity :Int) :ReversedIndexedBuffer[E] = apply(TemporaryBuffer.ofCapacity[E](capacity))

	override def empty[A] :ReversedIndexedBuffer[A] = apply(TemporaryBuffer.empty[A])
}


@SerialVersionUID(Ver)
private sealed class ReversedIndexedBuffer[E](override val reversed :IndexedBuffer[E])
	extends MutableReversedSeq[E](reversed) with IndexedBuffer[E]
	   with ReversedBuffer[E]
	   with SugaredSlicingOps[E, IndexedBuffer, IndexedBuffer[E]] with DefaultSerializable
{
	protected override def clippedSlice(from :Int, until :Int) :IndexedBuffer[E] =
		if (HasFastSlice(reverse)) {
			val len = reverse.length
			ReversedSeq(reversed.slice(len - until, len - from)) //so it creates a strict instance, if necessary.
		} else
			super[IndexedBuffer].slice(from, until)

	override def reverse :IndexedBuffer[E] = reversed.drop(0)

	protected[this] override def className = "ReversedBuffer"
}
