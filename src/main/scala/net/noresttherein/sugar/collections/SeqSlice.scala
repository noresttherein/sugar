package net.noresttherein.sugar.collections

import java.lang.{Math => math}

import scala.collection.{IterableFactory, IterableFactoryDefaults, SeqFactory, Stepper, StepperShape, mutable}
import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.{IndexedSeqOps, SeqOps}

import net.noresttherein.sugar.collections.extensions.IterableExtension
import net.noresttherein.sugar.extensions.{IsIterableOnceExtension, castingMethods}
import net.noresttherein.sugar.outOfBounds_!




trait GenSeqRange[+E] extends collection.Seq[E] with SlicingOps[E, GenSeqRange[E]] {
	override def segmentLength(p :E => Boolean, from :Int) :Int = super.segmentLength(p, from)
}

trait SeqRange[+E] extends Seq[E] with GenSeqRange[E] with SlicingOps[E, SeqRange[E]]

trait MutSeqRange[E] extends mutable.Seq[E] with GenSeqRange[E] with SlicingOps[E, MutSeqRange[E]]

trait GenIndexedSeqRange[+E]
	extends collection.IndexedSeq[E] with GenSeqRange[E] with SlicingOps[E, GenIndexedSeqRange[E]]
{
	override def segmentLength(p :E => Boolean, from :Int) :Int = {
		var i = 0; val len = length
		while (i < len && p(apply(i)))
			i += 1
		i
	}
}

trait IndexedSeqRange[+E] extends IndexedSeq[E] with GenIndexedSeqRange[E] with SlicingOps[E, IndexedSeqRange[E]]

trait MutIndexedSeqRange[E]
	extends mutable.IndexedSeq[E] with GenIndexedSeqRange[E] with SlicingOps[E, MutIndexedSeqRange[E]]




private abstract class GenericSeqSlice[E, +CC[A] <: collection.IndexedSeq[A] with collection.IndexedSeqOps[A, CC, CC[A]]]
                                      (underlying :CC[E], offset :Int, override val length :Int)
	extends collection.AbstractSeq[E] with collection.IndexedSeq[E] with collection.IndexedSeqOps[E, CC, CC[E]]
		with SugaredIterable[E] with SugaredIterableOps[E, CC, CC[E]]
		with IterableFactoryDefaults[E, CC] with DefaultSerializable
{ this :CC[E] =>
	protected final def whole :CC[E] = underlying
	protected final def start :Int = offset

	override def apply(i :Int) :E =
		if (i >= length)
			throw new IndexOutOfBoundsException(i.toString + " out of " + length)
		else
			underlying(offset + i)

	override def foreach[U](f :E => U) :Unit = foreach(0, length)(f)

	override def foreach[U](from :Int, until :Int)(f :E => U) :Unit = {
		val from0  = math.min(length, math.max(from, 0))
		val until0 = math.min(length, math.max(until, from0))
		if (until0 - from0 > 0)
			if (until0 - from0 <= IndexedIterable.applyPreferredMaxLength(underlying)) {
				var i = offset + from0; val end = offset + until0
				while (i < end) {
					f(underlying(i))
					i += 1
				}
			} else {
				val it = underlying.iterator.drop(from0)
				var remaining = until0 - from0
				while (remaining > 0) {
					f(it.next())
					remaining -= 1
				}
			}
	}

	override def segmentLength(p :E => Boolean, from :Int) :Int = super[IndexedSeq].segmentLength(p, from)

	override def iterator :Iterator[E] = new IndexedSeqIterator(underlying, offset, offset + length)
	override def reverseIterator :Iterator[E] = new ReverseIndexedSeqIterator(underlying, offset, offset + length)
	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S with EfficientSplit =
		IndexedSeqStepper(underlying, offset, offset + length)

	override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
		underlying.copyRangeToArray(xs, start, offset, math.min(len, length))

	override def copyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int =
		underlying.copyRangeToArray(xs, start, math.min(Int.MaxValue - offset, math.max(from, 0)) + offset, len)

	override def cyclicCopyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int =
		underlying.cyclicCopyRangeToArray(xs, start, offset, math.min(len, length))

	override def cyclicCopyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int =
		underlying.cyclicCopyRangeToArray(xs, start, offset + math.min(Int.MaxValue - offset, math.max(from, 0)), len)

	protected override def className :String = "SeqSlice"

//	protected override def newSpecificBuilder :Builder[E, CC[E]] =
//		(underlying :collection.IndexedSeqOps[E, CC, CC[E]]).iterableFactory.newBuilder[E]
//
//	protected override def fromSpecific(coll :IterableOnce[E]) :CC[E] =
//		(underlying :collection.IndexedSeqOps[E, CC, CC[E]]).iterableFactory.from(coll)

	override def iterableFactory :SeqFactory[CC] =
		(underlying :collection.IndexedSeqOps[E, CC, CC[E]]).iterableFactory.castFrom[IterableFactory[CC], SeqFactory[CC]]
//	private def writeReplace =
//		fromSpecific((underlying :collection.IndexedSeqOps[E, CC, CC[E]]).slice(offset, offset + length))
}




private[collections] sealed abstract class SeqSliceFactory[C[A] <: collection.IndexedSeq[A], S[A] <: collection.Seq[A]] {
	protected def make[E](seq :C[E], from :Int, until :Int) :S[E]

	def apply[A](seq :C[A], from :Int, until :Int) :S[A] = {
		val len = seq.length
		if (from >= len) make(seq, len, len)
		else if (until <= 0) make(seq, 0, 0)
		else if (until <= from) make(seq, from, from)
		else if (from <= 0 && until >= len) make(seq, 0, len)
		else if (from <= 0) make(seq, 0, until)
		else if (until >= len) make(seq, from, len - from)
		else make(seq, from, until - from)
	}
//
//	override def from[A](source :IterableOnce[A]) :SeqSlice[A] = source match {
//		case it :Iterable[_] if it.isEmpty => empty
//		case it :SeqSlice[A] => it
//		case it :collection.IndexedSeq[A] => make(it, 0)
//		case it :Iterator[_] if it.isEmpty => empty
//		case it => (newBuilder[A] ++= it).result()
//	}
//
//	override def newBuilder[A] :Builder[A, C[A]] =
//		DefaultIndexedSeq.newBuilder mapResult { seq => make(seq, 0, seq.length) }

//	def empty[A] :C[A] = Empty.castParam[A]
//	private[this] val Empty = make(IterableFactory.empty[Nothing], 0, 0)
//
//	protected def iterableFactory :IterableFactory[C]
}




/** A window over a range of indices in an `IndexedSeq`. It is very similar in function to
  * [[collection.IndexedSeqView IndexedSeqView]], but for three important differences:
  *   1. It is a `collection.IndexedSeq`, not only `collection.IndexedSeqOps`,
  *   1. all slicing operations will return another `SeqSlice` of the underlying collection, and
  *   1. all operations other than slicing will produce a default `IndexedSeq` ([[collection.immutable.Vector Vector]]),
  *      rather than another `View`.
  * @author Marcin Mościcki
  */ //consider: renaming to Subseq
@SerialVersionUID(Ver)
private class SeqSlice[E](whole :collection.IndexedSeq[E], offset :Int, override val length :Int)
	extends GenericSeqSlice[E, collection.IndexedSeq](whole, offset, length) with GenIndexedSeqRange[E]
{
	def this(whole :collection.IndexedSeq[E]) = this (whole, 0, whole.length)

	protected override def emptySlice :GenIndexedSeqRange[E] = SeqSlice.empty
	protected override def clippedSlice(from :Int, until :Int) :GenIndexedSeqRange[E] =
		new SeqSlice(whole, start + from, until - from)
}




/** $factoryInfo
  * @define Coll `SeqSlice`
  * @define coll sequence slice
  */
@SerialVersionUID(Ver)
case object SeqSlice extends SeqSliceFactory[collection.IndexedSeq, GenIndexedSeqRange] {
	protected override def make[E](seq :collection.IndexedSeq[E], from :Int, until :Int) :GenIndexedSeqRange[E] =
		seq match {
			case empty   :IndexedSeq[E] if empty.length == 0 => Immutable.empty
			case stable  :IndexedSeq[E]                      => new Immutable(stable, from, until)
			case mut :mutable.IndexedSeq[E]                  => new Mutable(mut, from, until)
			case _ => new SeqSlice(seq, from, until)
		}

	@inline def apply[E](seq :mutable.IndexedSeq[E], from :Int, until :Int) :MutIndexedSeqRange[E] =
		Mutable(seq, from, until)

	@inline def apply[E](seq :IndexedSeq[E], from :Int, until :Int) :IndexedSeqRange[E] =
		Immutable(seq, from, until)

	val empty :GenIndexedSeqRange[Nothing] = Immutable.empty

	@SerialVersionUID(Ver)
	private class Immutable[E](underlying :IndexedSeq[E], offset :Int, override val length :Int)
		extends GenericSeqSlice[E, IndexedSeq](underlying, offset, length)
		   with IndexedSeq[E] with IndexedSeqRange[E]
	{
		protected override def emptySlice :IndexedSeqRange[E] = Immutable.empty
		protected override def clippedSlice(from :Int, until :Int) :IndexedSeqRange[E] =
			new Immutable(whole, start + from, until - from)
	}

	private object Immutable extends SeqSliceFactory[IndexedSeq, IndexedSeqRange] {
		val empty :Immutable[Nothing] = new Immutable(IndexedSeq.empty, 0, 0)
		override def make[E](seq :IndexedSeq[E], from :Int, until :Int) :IndexedSeqRange[E] =
			if (until <= from) empty else new Immutable(seq, from, until)
		override def toString = "SeqSlice.Immutable"
	}


	//Consider: making slices a view over the current sequence.
	@SerialVersionUID(Ver)
	private class Mutable[E](underlying :mutable.IndexedSeq[E], offset :Int, override val length :Int)
		extends GenericSeqSlice[E, mutable.IndexedSeq](underlying, offset, length)
		   with mutable.IndexedSeq[E] with MutIndexedSeqRange[E]
	{
		protected override def emptySlice :MutIndexedSeqRange[E] = new Mutable(underlying.empty, 0, 0)

		override def update(idx :Int, elem :E) :Unit =
			if (idx < 0 || idx >= length) outOfBounds_!(idx, this)
			else whole

		protected override def clippedSlice(from :Int, until :Int) :MutIndexedSeqRange[E] =
			new Mutable(whole, start + from, until - from)
	}

	private object Mutable extends SeqSliceFactory[mutable.IndexedSeq, MutIndexedSeqRange] {
		override def make[E](seq :mutable.IndexedSeq[E], from :Int, until :Int) :MutIndexedSeqRange[E] =
			new Mutable(seq, from, until)
		override def toString = "SeqSlice.Mutable"
	}
}
