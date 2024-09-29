package net.noresttherein.sugar.collections

import java.lang.{Math => math}

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{IterableFactory, IterableFactoryDefaults, SeqFactory, Stepper, StepperShape, StrictOptimizedIterableOps, StrictOptimizedLinearSeqOps, mutable}
import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.{AbstractSeq, LinearSeq, StrictOptimizedSeqOps}

import net.noresttherein.sugar.casting.castingMethods
import net.noresttherein.sugar.collections.extensions.IterableOnceExtension
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.exceptions.{outOfBounds_!, unsupported_!}
import net.noresttherein.sugar.typist.kinds.Any1




/** A view on a range of indices in another, possibly mutable, $Coll.
  * $Docs
  * @define Self `ImpureSubseq`
  * @define Coll `collection.Seq`
  * @define Ops  `collection.SeqOps`
  * @define View [[collection.SeqView SeqView]]
  * @define Docs It differs from the standard $Coll in that an instance can be produced in `O(1)` time
  *              (which is particularly relevant for `ArraySeq` and similar collections), and any updates to the latter
  *              will be visible in this view.
  *              It is very similar in function to $View, but for three important differences:
  *                1. it is a $Coll, not only $Ops, while the mutability of $View is undefined, making a latter
  *                   an illegal argument to many methods,
  *                1. all slicing operations will return another $Self of the underlying collection, and
  *                1. all operations other than slicing will produce a default $Coll, rather than another `View`.
  *
  *              In particular, this means that methods like `map` and `filter` a $Self will produce a default,
  *              strict $Coll, rather than a lazy view. On the other hand, it retains a reference to the original $Coll,
  *              preventing its garbage collection, and guarantees `O(1)` slicing, which is important for some
  *              algorithms. It is therefore distinct enough from both $Coll and $View to warrant a special interface.
  * @author Marcin Mościcki
  */ //consider: renaming to ImpureSeqSlice
trait ImpureSubseq[+E] extends collection.Seq[E] with SlicingOps[E, ImpureSubseq[E]] {
	//override clash between SeqOps and SlicingOps
	override def segmentLength(p :E => Boolean, from :Int) :Int = super.segmentLength(p, from)
	protected[this] override def className = "Subseq"
}

/** A view on a range of indices in another immutable sequence.
  * $Docs
  * @define Self `Subseq`
  * @define Coll `Seq`
  */
trait Subseq[+E] extends Seq[E] with ImpureSubseq[E] with SlicingOps[E, Subseq[E]]

/** A view on a range of indices in a mutable sequence.
  * $Docs
  * @define Self `MutSubseq`
  * @define Coll `mutable.Seq`
  */ //consider: making slice return another view on the underlying sequence
trait MutSubseq[E] extends mutable.Seq[E] with ImpureSubseq[E] with SlicingOps[E, MutSubseq[E]]


/** A view on a range of indices in another, possibly mutable, indexed sequence.
  * $Docs
  * @define Self `ImpureIndexedSubseq`
  * @define Coll `collection.IndexedSeq`
  * @define Ops  `collection.IndexedSeqOps`
  * @define View [[collection.IndexedSeqView IndexedSeqView]]
  */
trait ImpureIndexedSubseq[+E]
	extends collection.IndexedSeq[E] with ImpureSubseq[E] with SlicingOps[E, ImpureIndexedSubseq[E]]
{
	protected override def hasFastSlice = true
	override def segmentLength(p :E => Boolean, from :Int) :Int = super[IndexedSeq].segmentLength(p, from)
	protected[this] override def className = "IndexedSubseq"
}

/** A view on a range of indices in another immutable indexed sequence.
  * $Docs
  * @define Self `IndexedSubseq`
  * @define Coll `collection.IndexedSeq`
  */
trait IndexedSubseq[+E] extends IndexedSeq[E] with ImpureIndexedSubseq[E] with SlicingOps[E, IndexedSubseq[E]]

/** A view on a range of indices in a mutable indexed sequence.
  * $Docs
  * @define Self `MutIndexedSubseq`
  * @define Coll `mutable.IndexedSeq`
  */
trait MutIndexedSubseq[E]
	extends mutable.IndexedSeq[E] with ImpureIndexedSubseq[E] with SlicingOps[E, MutIndexedSubseq[E]]




private[collections] sealed abstract class SubseqFactory[C[A] <: collection.IndexedSeq[A], S[A] <: collection.Seq[A]] {
	protected def make[E](seq :C[E], from :Int, length :Int) :S[E]

	def apply[A](seq :C[A]) :S[A] = make(seq, 0, seq.length)

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




@SerialVersionUID(Ver)
case object Subseq {
	@inline def apply[E](seq :collection.IndexedSeq[E]) :ImpureIndexedSubseq[E] = ImpureIndexedSubseq(seq)
	@inline def apply[E](seq :mutable.IndexedSeq[E]) :MutIndexedSubseq[E] = MutIndexedSubseq(seq)
	@inline def apply[E](seq :IndexedSeq[E]) :IndexedSubseq[E] = IndexedSubseq(seq)

	@inline def apply[E](seq :collection.IndexedSeq[E], from :Int, until :Int) :ImpureIndexedSubseq[E] =
		ImpureIndexedSubseq(seq, from, until)

	@inline def apply[E](seq :mutable.IndexedSeq[E], from :Int, until :Int) :MutIndexedSubseq[E] =
		MutIndexedSubseq(seq, from, until)

	@inline def apply[E](seq :IndexedSeq[E], from :Int, until :Int) :IndexedSubseq[E] =
		IndexedSubseq(seq, from, until)

	def empty[E] :ImpureIndexedSubseq[E] = IndexedSubseq.empty
}


/** $factoryInfo
  * @define Coll `ImpureSubseq`
  * @define coll sequence slice
  */
@SerialVersionUID(Ver)
case object ImpureIndexedSubseq extends SubseqFactory[collection.IndexedSeq, ImpureIndexedSubseq] {
	protected override def make[E](seq :collection.IndexedSeq[E], from :Int, length :Int) :ImpureIndexedSubseq[E] =
		seq match {
			case empty   :IndexedSeq[E] if empty.length == 0 => IndexedSubseq.empty
			case stable  :IndexedSeq[E]                      => IndexedSubseq(stable, from, from + length)
			case mut :mutable.IndexedSeq[E]                  => MutIndexedSubseq(mut, from, from + length)
			case _ :StrictOptimizedIterableOps[_, Any1, _]   =>
				new ImpureSeqSlice[E](seq, from, length)
					with StrictOptimizedSubseqOps[E, collection.IndexedSeq, ImpureIndexedSubseq[E]]
			case _                                           => new ImpureSeqSlice(seq, from, length)
		}

//	def empty :ImpureIndexedSubseq[Nothing]
	val empty :ImpureIndexedSubseq[Nothing] = IndexedSubseq.empty

}


@SerialVersionUID(Ver)
private case object IndexedSubseq extends SubseqFactory[IndexedSeq, IndexedSubseq] {
	@inline def empty[E] :IndexedSubseq[E] = Empty
	val Empty :IndexedSubseq[Nothing] = new SeqSlice(IndexedSeq.empty, 0, 0)

	override def make[E](seq :IndexedSeq[E], from :Int, length :Int) :IndexedSubseq[E] = seq match {
		case _ if length <= 0                  => empty
		case _ :StrictOptimizedSeqOps[_, _, _] =>
			new SeqSlice(seq, from, length)
				with StrictOptimizedSeqOps[E, IndexedSeq, IndexedSeq[E]]
				with StrictOptimizedSubseqOps[E, IndexedSeq, IndexedSubseq[E]]
		case _ =>
			new SeqSlice(seq, from, length)
	}
}


@SerialVersionUID(Ver)
private case object MutIndexedSubseq extends SubseqFactory[mutable.IndexedSeq, MutIndexedSubseq] {
	def empty[E] :MutIndexedSubseq[E] = Empty.asInstanceOf[MutIndexedSubseq[E]]
	private[this] val Empty = new MutSeqSlice(TemporaryBuffer.empty, 0, 0)

	import mutable.{IndexedSeq => MutSeq}
	override def make[E](seq :MutSeq[E], from :Int, length :Int) :MutIndexedSubseq[E] = seq match {
		case _ :collection.StrictOptimizedSeqOps[_, _, _] =>
			new MutSeqSlice(seq, from, length)
				with collection.StrictOptimizedSeqOps[E, MutSeq, MutSeq[E]]
				with StrictOptimizedSubseqOps[E, MutSeq, MutIndexedSubseq[E]]
		case _ =>
			new MutSeqSlice(seq, from, length)
	}
}




private trait StrictOptimizedSubseqOps[+E, +CC[X] <: collection.Seq[X], +C <: CC[E @uncheckedVariance]]
	extends SlicingOps[E, C] with collection.StrictOptimizedSeqOps[E, CC, CC[E @uncheckedVariance]]
{
	override def span(p :E => Boolean) :(C, C) = super[SlicingOps].span(p)
	override def takeRight(n :Int) :C = super[SlicingOps].takeRight(n)
	override def dropRight(n :Int) :C = super[SlicingOps].dropRight(n)
	override def segmentLength(p :E => Boolean, from :Int) :Int = super.segmentLength(p, from)
}




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
	override def reverseIterator :Iterator[E] = new ReverseIndexedSeqIterator(underlying, offset - 1, offset + length - 1)
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

//	protected override def className :String = "SeqSlice"

	override def iterableFactory :SeqFactory[CC] =
		(underlying :collection.IndexedSeqOps[E, CC, CC[E]]).iterableFactory.castFrom[IterableFactory[CC], SeqFactory[CC]]
}


@SerialVersionUID(Ver)
private class ImpureSeqSlice[E](whole :collection.IndexedSeq[E], offset :Int, override val length :Int)
	extends GenericSeqSlice[E, collection.IndexedSeq](whole, offset, length) with ImpureIndexedSubseq[E]
{
	def this(whole :collection.IndexedSeq[E]) = this(whole, 0, whole.length)

	protected override def emptySlice :ImpureIndexedSubseq[E] = ImpureIndexedSubseq.empty
	protected override def clippedSlice(from :Int, until :Int) :ImpureIndexedSubseq[E] =
		new ImpureSeqSlice(whole, start + from, until - from)
}


@SerialVersionUID(Ver)
private class SeqSlice[E](underlying :IndexedSeq[E], offset :Int, override val length :Int)
	extends GenericSeqSlice[E, IndexedSeq](underlying, offset, length)
		with IndexedSeq[E] with IndexedSubseq[E]
{
	protected override def emptySlice :IndexedSubseq[E] = IndexedSubseq.empty
	protected override def clippedSlice(from :Int, until :Int) :IndexedSubseq[E] =
		new SeqSlice(whole, start + from, until - from)
}


//Consider: making slices a view over the current sequence.
@SerialVersionUID(Ver)
private class MutSeqSlice[E](underlying :mutable.IndexedSeq[E], offset :Int, override val length :Int)
	extends GenericSeqSlice[E, mutable.IndexedSeq](underlying, offset, length)
		with mutable.IndexedSeq[E] with MutIndexedSubseq[E]
{
	protected override def emptySlice :MutIndexedSubseq[E] = new MutSeqSlice(underlying.empty, 0, 0)

	override def update(idx :Int, elem :E) :Unit =
		if (idx < 0 || idx >= length) outOfBounds_!(idx, this)
		else whole

	protected override def clippedSlice(from :Int, until :Int) :MutIndexedSubseq[E] =
		new MutSeqSlice(whole.clone(), start + from, until - from)
}

/*
@SerialVersionUID(Ver)
private class LinearSeqPrefix[+E](seq :LinearSeq[E], override val length :Int)
	extends AbstractSeq[E] with LinearSeq[E] with StrictOptimizedSubseqOps[E, LinearSeq, LinearSeqPrefix[E]]
	   with DefaultSerializable
{
	override def knownSize :Int = length

	override def head :E = seq.head

	protected override def emptySlice :LinearSeqPrefix[E] = LinearSeqPrefix.Empty

	protected override def clippedSlice(from :Int, until :Int) :LinearSeqPrefix[E] =
		new LinearSeqPrefix(seq.drop(from), until - from)

	override def tail = new LinearSeqPrefix[E](seq.tail, length - 1)
	override def init =
		if (length == 0) unsupported_!(errorString(this) + ".init")
		else new LinearSeqPrefix[E](seq, length - 1)
}

@SerialVersionUID(Ver)
private object LinearSeqPrefix {
	val Empty :LinearSeqPrefix[Nothing] = new LinearSeqPrefix(Nil, 0)
}
*/
