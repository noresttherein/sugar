package net.noresttherein.sugar.collections

import java.lang.{Math => math}

import scala.collection.{IterableFactory, IterableFactoryDefaults, SeqFactory, Stepper, StepperShape, StrictOptimizedIterableOps, mutable}
import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.StrictOptimizedSeqOps

import net.noresttherein.sugar.casting.castingMethods
import net.noresttherein.sugar.collections.extensions.IterableOnceExtension
import net.noresttherein.sugar.exceptions.outOfBounds_!
import net.noresttherein.sugar.funny.generic.Any1




trait GenSeqRange[+E] extends collection.Seq[E] with SlicingOps[E, GenSeqRange[E]] {
	override def segmentLength(p :E => Boolean, from :Int) :Int = super.segmentLength(p, from)
}

trait SeqRange[+E] extends Seq[E] with GenSeqRange[E] with SlicingOps[E, SeqRange[E]]

trait MutSeqRange[E] extends mutable.Seq[E] with GenSeqRange[E] with SlicingOps[E, MutSeqRange[E]]

trait GenIndexedSeqRange[+E]
	extends collection.IndexedSeq[E] with GenSeqRange[E] with SlicingOps[E, GenIndexedSeqRange[E]]
{
	protected override def hasFastSlice = true
	override def segmentLength(p :E => Boolean, from :Int) :Int = super[IndexedSeq].segmentLength(p, from)
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
		if (i < 0 | i >= length)
			outOfBounds_!(i, length)
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
		IndexedSeqStepper.slice(underlying, offset, offset + length)

	override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
		underlying.copyRangeToArray(xs, start, offset, math.min(len, length))

	override def copyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int =
		underlying.copyRangeToArray(xs, start, math.min(Int.MaxValue - offset, math.max(from, 0)) + offset, len)

	override def cyclicCopyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int =
		underlying.cyclicCopyRangeToArray(xs, start, offset, math.min(len, length))

	override def cyclicCopyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int =
		underlying.cyclicCopyRangeToArray(xs, start, offset + math.min(Int.MaxValue - offset, math.max(from, 0)), len)

	protected override def className :String = "SeqSlice"

	override def iterableFactory :SeqFactory[CC] =
		(underlying :collection.IndexedSeqOps[E, CC, CC[E]]).iterableFactory.castFrom[IterableFactory[CC], SeqFactory[CC]]
}




private[collections] sealed abstract class SeqSliceFactory[C[A] <: collection.IndexedSeq[A], S[A] <: collection.Seq[A]] {
	protected def make[E](seq :C[E], from :Int, length :Int) :S[E]

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
	protected override def make[E](seq :collection.IndexedSeq[E], from :Int, length :Int) :GenIndexedSeqRange[E] =
		seq match {
			case empty   :IndexedSeq[E] if empty.length == 0 => Immutable.empty
			case stable  :IndexedSeq[E]                      => Immutable(stable, from, from + length)
			case mut :mutable.IndexedSeq[E]                  => Mutable(mut, from, from + length)
			case _ :StrictOptimizedIterableOps[_, Any1, _]   =>
				new SeqSlice[E](seq, from, length)
					with StrictOptimizedSeqRangeOps[E, collection.IndexedSeq, GenIndexedSeqRange[E]]
			case _                                           => new SeqSlice(seq, from, length)
		}

	@inline def apply[E](seq :mutable.IndexedSeq[E], from :Int, until :Int) :MutIndexedSeqRange[E] =
		Mutable(seq, from, until)

	@inline def apply[E](seq :IndexedSeq[E], from :Int, until :Int) :IndexedSeqRange[E] =
		Immutable(seq, from, until)

	val empty :GenIndexedSeqRange[Nothing] = Immutable.empty

	private trait StrictOptimizedSeqRangeOps[E, +CC[X] <: collection.IndexedSeq[X], +C <: CC[E]]
		extends SlicingOps[E, C] with collection.StrictOptimizedSeqOps[E, CC, CC[E]]
	{
		override def span(p :E => Boolean) :(C, C) = super[SlicingOps].span(p)
		override def takeRight(n :Int) :C = super[SlicingOps].takeRight(n)
		override def dropRight(n :Int) :C = super[SlicingOps].dropRight(n)
		override def segmentLength(p :E => Boolean, from :Int) :Int = super.segmentLength(p, from)
	}

	@SerialVersionUID(Ver)
	private class Immutable[E](underlying :IndexedSeq[E], offset :Int, override val length :Int)
		extends GenericSeqSlice[E, IndexedSeq](underlying, offset, length)
		   with IndexedSeq[E] with IndexedSeqRange[E]
	{
		protected override def emptySlice :IndexedSeqRange[E] = Immutable.empty
		protected override def clippedSlice(from :Int, until :Int) :IndexedSeqRange[E] =
			new Immutable(whole, start + from, until - from)
	}

	@SerialVersionUID(Ver)
	private object Immutable extends SeqSliceFactory[IndexedSeq, IndexedSeqRange] {
		val empty :IndexedSeqRange[Nothing] = new Immutable(IndexedSeq.empty, 0, 0)

		override def make[E](seq :IndexedSeq[E], from :Int, length :Int) :IndexedSeqRange[E] = seq match {
			case _ if length <= 0                  => empty
			case _ :StrictOptimizedSeqOps[_, _, _] =>
				new Immutable(seq, from, length)
					with StrictOptimizedSeqOps[E, IndexedSeq, IndexedSeq[E]]
					with StrictOptimizedSeqRangeOps[E, IndexedSeq, IndexedSeqRange[E]]
			case _ =>
				new Immutable(seq, from, length)
		}
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
			new Mutable(whole.clone(), start + from, until - from)
	}

	@SerialVersionUID(Ver)
	private object Mutable extends SeqSliceFactory[mutable.IndexedSeq, MutIndexedSeqRange] {
		import mutable.IndexedSeq
		override def make[E](seq :IndexedSeq[E], from :Int, length :Int) :MutIndexedSeqRange[E] = seq match {
			case _ :collection.StrictOptimizedSeqOps[_, _, _] =>
				new Mutable(seq, from, length)
					with collection.StrictOptimizedSeqOps[E, IndexedSeq, IndexedSeq[E]]
					with StrictOptimizedSeqRangeOps[E, IndexedSeq, MutIndexedSeqRange[E]]
			case _ =>
				new Mutable(seq, from, length)
		}
		override def toString = "SeqSlice.Mutable"
	}
}
