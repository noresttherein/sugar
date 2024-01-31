package net.noresttherein.sugar.collections

import java.lang.{Math => math}

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.generic.DefaultSerializationProxy
import scala.collection.{SeqFactory, SeqView, Stepper, StepperShape, mutable}
import scala.collection.immutable.AbstractSeq
import scala.collection.mutable.{Buffer, Builder}

import net.noresttherein.sugar.JavaTypes.JIterator
import net.noresttherein.sugar.collections.IndexedIterable.applyPreferred
import net.noresttherein.sugar.collections.ZigZag.{Appended, Concat, Prepended}
import net.noresttherein.sugar.extensions.classNameMethods
import net.noresttherein.sugar.outOfBounds_!

//implicits
import net.noresttherein.sugar.extensions.StepperExtension




/** A recursive list-like sequence to which elements can be both prepended and appended in O(1) time;
  * `length` is also an O(1) operation.
  * It offers O(n) traversal, but subpar to standard `Seq` implementations and similarly subpar O(n) random access.
  * Each time an element or collection is prepended or appended, only a single object is created, combining
  * this sequence with the new elements. This makes it suited only as an intermediate buffer structure,
  * where collections are concatenated recursively, which would lead to O(n*n) time in standard implementations.
  * After the contents are complete, it is advised to convert this sequence into some all purpose `Seq` implementation;
  * this will also happen automatically when [[scala.collection.IterableOps.map mapping]] it. Note that `ZigZag` type
  * is not 'sticky': operations other than appending/prepending and slicing return more conventional
  * `Seq` implementations.
  *
  * The name of the collection reflects its internal structure as an unbalanced tree created by appending and prepending
  * individual elements. This class is essentially the same as `Chain` from `cats` library,
  * but implemented within the standard collection framework.
  * @define Coll `ZigZag`
  * @define coll zigzag
  * @author Marcin Mościcki marcin@moscicki.net
  */ //consider: renaming to Chain
sealed abstract class ZigZag[+E]
	extends AbstractSeq[E] with SugaredIterable[E] with SlicingOps[E, ZigZag[E]] with Serializable
{
//	private[ZigZag] def depth :Int
	override def knownSize :Int = length

	//fixme: we can't override :++ and ++: :(
	override def appended[B >: E](elem :B) :ZigZag[B] = new Appended(this, elem)
	override def prepended[B >: E](elem :B) :ZigZag[B] = new Prepended(elem, this)

	override def appendedAll[B >: E](suffix :IterableOnce[B]) :ZigZag[B] = suffix match {
		case it :Iterable[_] if it.isEmpty => this
		case _ if isEmpty => ZigZag.from(suffix)
		case it :Iterable[B] if it.sizeIs == 1 => new Appended(this, it.head)
		case seq :Seq[B] => new Concat(this, seq)
		case it :Iterable[B] => new Concat(this, it to DefaultIndexedSeq)
		case _ => new Concat(this, suffix.iterator to DefaultIndexedSeq)
	}
	override def prependedAll[B >: E](prefix :IterableOnce[B]) :ZigZag[B] = prefix match {
		case it :Iterable[_] if it.isEmpty     => this
		case _ if isEmpty                      => ZigZag.from(prefix)
		case it :Iterable[B] if it.sizeIs == 1 => new Prepended(it.head, this)
		case seq :Seq[B]                       => new Concat(seq, this)
		case it :Iterable[B]                   => new Concat(it to DefaultIndexedSeq, this)
		case _                                 => new Concat(prefix.iterator to DefaultIndexedSeq, this)
	}

	protected override def emptySlice :ZigZag[E] = ZigZag.empty

	protected override def clippedSlice(from :Int, until :Int) :ZigZag[E] = {
		@tailrec def slice(in :Seq[E], from :Int, until :Int, prefix :ZigZag[E],
		                   suffix :mutable.Queue[ZigZag[E]], lastUntil :Int) :ZigZag[E] =
		{
//			@inline def newSuffix = if (suffix == null) mutable.Queue.empty[ZigZag[E]] else suffix
			var from0      = from
			var until0     = until
			var lastUntil0 = lastUntil
			var next       = null :Seq[E]
			var newPrefix  = prefix
			var newSuffix  = suffix
			in match {
				case concat :Concat[E] =>
					val prefixLen = concat._1.length
					if (from <= 0 & until == prefixLen)
						newPrefix = prefix appendedAll concat._1
					else if (until <= prefixLen)
						next = concat._1
					else if (from >= prefixLen) {
						next   = concat._2
						from0  = from - prefixLen
						until0 = until - prefixLen
					} else {
						next = concat._1
						if (newSuffix == null)
							newSuffix = mutable.Queue.empty
						if (newSuffix.length == 0)
							lastUntil0 = until - prefixLen
						concat +=: newSuffix
					}
				case appended :Appended[E] =>
					val prefixLen = appended.length - 1
					if (from == prefixLen)
						newPrefix = prefix appended appended.last
					else {
						next = appended.init
						if (until > prefixLen) {
							if (newSuffix == null)
								newSuffix = mutable.Queue.empty
							appended +=: newSuffix
						}
					}
				case prepended :Prepended[E] =>
					if (until == 1)
						newPrefix = prefix appended prepended.head
					else {
						next   = prepended.tail
						from0  = from - 1
						until0 = until - 1
						if (from <= 0)
							newPrefix = prefix appended prepended.head
					}
				case _ =>
					newPrefix = prefix appendedAll in.slice(from, until)
			}
			while (next == null && newSuffix != null && newSuffix.nonEmpty)
				newSuffix.dequeue() match {
					case appended :Appended[E] =>
						newPrefix = newPrefix appended appended.last
					case concat :Concat[E] =>
						next  = concat._2
						from0 = 0
						until0 = if (newSuffix.isEmpty) lastUntil0 else Int.MaxValue
					case seq => throw new AssertionError(
						"Illegal continuation slice \"" + seq + "\": " + seq.className + "."
					)
				}
			if (next == null)
				newPrefix
			else
				slice(next, from0, until0, newPrefix, newSuffix, lastUntil0)
		}
		slice(this, from, until, ZigZag.empty, null, 0)
	}

	override def segmentLength(p :E => Boolean, from :Int) :Int = {
		@tailrec def segment(in :Seq[E], idx :Int, prefix :Int, suffix :mutable.Queue[ZigZag[E]]) :Int = {
			var next      = null :Seq[E]
			var newIdx    = idx
			var newPrefix = prefix
			var newSuffix = suffix
			in match {
				case concat :Concat[E] =>
					val prefixSize = concat._1.length
					if (idx >= prefixSize) {
						next = concat._2
						newIdx = idx - prefixSize
					} else {
						next = concat._1
						newSuffix = if (suffix == null) mutable.Queue.empty[ZigZag[E]] else suffix
						concat +=: newSuffix
					}
				case appended :Appended[E] =>
					val prefixSize = appended.length - 1
					if (idx == prefixSize) {
						if (p(appended.last))
							newPrefix += 1
						else if (newSuffix != null)
							newSuffix.clear()
					} else {
						next      = appended.init
						newSuffix = if (suffix == null) mutable.Queue.empty[ZigZag[E]] else suffix
						appended +=: newSuffix
					}
				case prepended :Prepended[E] =>
					if (idx <= 0) {
						if (p(prepended.head)) {
							next       = prepended.tail
							newPrefix += 1
							newIdx     = 0
						} else if (newSuffix != null)
							newSuffix.clear()
					} else {
						next   = prepended.tail
						newIdx = idx - 1
					}
				case _ =>
					val len    = in.segmentLength(p, idx)
					newPrefix += len
					if (suffix != null && suffix.nonEmpty && idx + len < in.length)
						suffix.clear()
			}
			while (next == null && newSuffix != null && newSuffix.nonEmpty)
				newSuffix.dequeue() match {
					case appended :Appended[E] =>
						if (p(appended.last)) newPrefix += 1
						else newSuffix = null
					case concat :Concat[E] =>
						next   = concat._2
						newIdx = 0
					case seq => throw new AssertionError(
						"Illegal continuation slice \"" + seq + "\": " + seq.className + "."
					)
				}
			if (next != null)
				segment(next, newIdx, newPrefix, newSuffix)
			else
				newPrefix
		}

		if (from >= length) 0
		else if (from < 0) segment(this, 0, 0, null)
		else segment(this, from, 0, null)
	}

	override def apply(i :Int) :E = {
		@tailrec def get(seq :Seq[E], idx :Int) :E = seq match {
			case concat    :Concat[E]    =>
				val prefixLen = concat._1.length
				if (idx < prefixLen) get(concat._1, idx)
				else get(concat._2, idx - prefixLen)
			case appended  :Appended[E]  => if (idx == appended.init.length) appended.last else get(appended.init, idx)
			case prepended :Prepended[E] => if (idx == 0) prepended.head else get(prepended.tail, idx - 1)
			case _                       => seq(idx)
		}
		get(this, i)
	}
	//narrow the return type, overridden by subclasses
	override def updated[U >: E](index :Int, elem :U) :ZigZag[U] = //ZigZag.from(super.updated(index, elem))
		if (index < 0 || index >= length)
			outOfBounds_!(index.toString + " out of " + length)
		else if (index == 0)
			new Prepended(elem, drop(1))
		else if (index == length - 1)
			new Appended(dropRight(1), elem)
		else
			new Concat(new Appended(take(index), elem), drop(index + 1))

	//narrow the return type, overridden by subclasses
	override def patch[U >: E](from :Int, other :IterableOnce[U], replaced :Int) :ZigZag[U] =
		if (from >= length) appendedAll(other)
		else if (from <= 0) drop(replaced) prependedAll other
		else if (replaced <= 0) take(from) appendedAll other appendedAll drop(from)
		else if (replaced >= length - from) take(from) appendedAll other
		else take(from) appendedAll other appendedAll drop(from + replaced)
//		ZigZag.from(super.patch(from, other, replaced))

	override def foreach[U](f :E => U) :Unit = {
		@inline def init(suffix :mutable.Queue[ZigZag[E]]) =
			if (suffix == null) mutable.Queue.empty[ZigZag[E]] else suffix

		@tailrec def rec(in :Seq[E], suffix :mutable.Queue[ZigZag[E]]) :Unit = in match {
			case concat    :Concat[E]    =>
				rec(concat._1, concat +=: init(suffix))
			case appended  :Appended[E]  =>
				rec(appended.init, appended +=: init(suffix))
			case prepended :Prepended[E] =>
				f(prepended.head)
				rec(prepended.tail, suffix)
			case _ =>
				in.foreach(f)
				var next :Seq[E] = null
				while (suffix != null && suffix.nonEmpty && (suffix.dequeue() match {
					case appended :Appended[E] =>
						f(appended.last)
						true
					case concat   :Concat[E] =>
						next = concat._2
						false
					case seq => throw new AssertionError(
						"Illegal continuation slice \"" + seq + "\": " + seq.className + "."
					)
				})) {}
				if (next != null)
					rec(next, suffix)
		}
		rec(this, null)
	}

	protected override def reversed :Iterable[E] = new SeqView.Reverse(this)

	@inline private def cat2(i1 :Iterator[E @uncheckedVariance], i2 :Iterator[E @uncheckedVariance]) =
		if (i1.hasNext)
			if (i2.hasNext) i1 ++ i2 else i1
		else
			i2

	override def iterator :Iterator[E] = {
		@tailrec def catIterators(s :Seq[E], prefix :Iterator[E], suffix :Iterator[E]) :Iterator[E] =
			s match {
				case appended :Appended[E] =>
					catIterators(appended.init, prefix, cat2(Iterator.single(appended.last), suffix))
				case prepended :Prepended[E] =>
					catIterators(prepended.tail, cat2(prefix, Iterator.single(prepended.head)), suffix)
				case concat :Concat[E] =>
					catIterators(concat._2, prefix ++ concat._1.iterator, suffix)
				case _ =>
					cat2(cat2(prefix, s.iterator), suffix)
			}
		catIterators(this, Iterator.empty, Iterator.empty)
	}

	override def reverseIterator :Iterator[E] = {
		@tailrec def catIterators(s :Seq[E], prefix :Iterator[E], suffix :Iterator[E]) :Iterator[E] =
			s match {
				case appended :Appended[E] =>
					catIterators(appended.init, cat2(prefix, Iterator.single(appended.last)), suffix)
				case prepended :Prepended[E] =>
					catIterators(prepended.tail, prefix, cat2(Iterator.single(prepended.head), suffix))
				case concat :Concat[E] =>
					catIterators(concat._1, prefix ++ concat._2.reverseIterator, suffix)
				case _ =>
					cat2(cat2(prefix, s.reverseIterator), suffix)
			}
		catIterators(this, Iterator.empty, Iterator.empty)
	}

	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S = {
		@inline def cat2(i1 :S, i2 :S) =
			if (i1.hasStep)
				if (i2.hasStep) i1 :++ i2 else i1
			else
				i2
		@tailrec def catSteppers(s :Seq[E], prefix :S, suffix :S) :S =
			s match {
				case appended :Appended[E] =>
					catSteppers(appended.init, prefix, cat2(Stepper1(appended.last), suffix))
				case prepended :Prepended[E] =>
					catSteppers(prepended.tail, cat2(prefix, Stepper1(prepended.head)), suffix)
				case concat :Concat[E] =>
					catSteppers(concat._2, prefix ++ concat._1.stepper, suffix)
				case _ =>
					cat2(cat2(prefix, s.stepper), suffix)
			}
		catSteppers(this, Stepper0(), Stepper0())
	}

	//todo: remove these once the bugs are fixed in SeqOps
	override def startsWith[A >: E](that :IterableOnce[A], offset :Int) :Boolean =
		offset >= 0 && offset <= length && super.startsWith(that, offset)

	override def indexOfSlice[A >: E](that :collection.Seq[A], from :Int) :Int =
		if (from > length) -1
		else super.indexOfSlice(that, 0 max from)


	override def copyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int =
		drop(from).copyToArray(xs, start, len)

	private def writeReplace :AnyRef = new DefaultSerializationProxy(ZigZag, this)
	protected override def className = "ZigZag"
}




/** $factoryInfo
  * @define Coll `ZigZag`
  * @define coll zigzag
  */
@SerialVersionUID(Ver)
case object ZigZag extends SeqFactory[ZigZag] {

	override def from[A](source :IterableOnce[A]) :ZigZag[A] = source match {
		case zigzag :ZigZag[A] => zigzag
		case empty  :Iterable[_] if empty.knownSize == 0 => Empty
		case seq    :Seq[A] => new Straight(seq)
		case _ => new Straight((DefaultIndexedSeq.newBuilder[A] ++= source).result())
	}
	override def empty[A] :ZigZag[A] = Empty

	override def newBuilder[A] :Builder[A, ZigZag[A]] = DefaultIndexedSeq.newBuilder[A] mapResult (new Straight(_))

	/** Similar to [[collection.IndexedSeqView IndexedSeqView]], but a `ZigZag`, meaning non-slicing operations
	  * build strict, normal (non `ZigZag`) sequences, rather than another view. Unlike other zigzags,
	  * it will map to an `IndexedSeq`, rather than a `List`.
	  */
	def slice[A](seq :IndexedSeq[A], from :Int, until :Int) :ZigZag[A] =
		if (until <= from | until <= 0 || from > seq.length)
			Empty
		else {
			val from0 = math.max(from, 0)
			new Slice(seq, from, math.min(until, seq.length) - from0)
		}

	private def javaIteratorOver[A, I <: JIterator[_]](seq :Seq[A])(implicit shape :JavaIteratorShape[A, I]) :I =
		seq match {
			case sugared :SugaredIterable[A] => sugared.javaIterator
			case indexed :IndexedSeq[A] => IndexedSeqStepper(indexed)(shape.stepperShape).javaIterator.asInstanceOf[I]
			case _ => seq.stepper(shape.stepperShape).javaIterator.asInstanceOf[I]
		}
//	@inline private def sliceOf[A](seq :Seq[A], from :Int, until :Int) :Seq[A] = seq match {
//		case zigzag :ZigZag[A] => zigzag.trustedSlice(from, until)
//		case _ => seq.slice(from, until)
//	}



	@SerialVersionUID(Ver)
	private class EmptyZigZag extends ZigZag[Nothing] { //with EmptyIterableOps.Generic[Seq] {
		override def length :Int = 0
		override def apply(i :Int) = outOfBounds_!(i.toString + " out of 0")
		override def clippedSlice(from :Int, until :Int) :ZigZag[Nothing] = this
		override def segmentLength(p :Nothing => Boolean, from :Int) :Int = 0
		override def view :SeqView[Nothing] = EmptySeqOps.view
		override def iterator = Iterator.empty
		override def reverseIterator = Iterator.empty
		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[Nothing, S]) :S = Stepper0()

		override def foreach[U](f :Nothing => U) :Unit = ()
		private def readResolve :Seq[Nothing] = ZigZag.empty
	}

	private[this] final val Empty :ZigZag[Nothing] = new EmptyZigZag


	@SerialVersionUID(Ver)
	private class Straight[+A](elems :Seq[A]) extends ZigZag[A] {
		private[ZigZag] def elements = elems
//		override def depth :Int = 0
		override lazy val length :Int = elems.length
		override def head = elems.head
		override def last = elems.last
		override def tail = ZigZag.from(elems.tail)
		override def apply(i :Int) :A = elems(i)
		override def segmentLength(p :A => Boolean, from :Int) :Int = elems.segmentLength(p, from)
		override def reversed :Seq[A] = elems.reverse
		override def iterator :Iterator[A] = elems.iterator
		override def reverseIterator :Iterator[A] = elems.reverseIterator
		override def javaIterator[I <: JavaIterator[_]](implicit shape :JavaIteratorShape[A, I]) :I = javaIteratorOver(elems)
		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[A, S]) :S = elems.stepper

		override def clippedSlice(from :Int, until :Int) :ZigZag[A] = elems match {
			case indexed :IndexedSeq[A] => new Slice(indexed, from, until - from)
			case _ => new Straight(elems.slice(from, until))
		}

//		override def updated[B >: A](index :Int, elem :B) :ZigZag[B] = new Straight(elems.updated(index, elem))
//		override def patch[B >: A](from :Int, other :IterableOnce[B], replaced :Int) :ZigZag[B] =
//			ZigZag.from(elems.patch(from, other, replaced))

		override def foreach[U](f :A => U) :Unit = elems.foreach(f)
//		override def stackForEach[U](f :A => U) :Unit = elems.foreach(f)
	}


	@SerialVersionUID(Ver)
	private class Slice[+A](elems :IndexedSeq[A], offset :Int, override val length :Int) extends ZigZag[A] {
		private[ZigZag] def elements = elems
		private[ZigZag] def from = offset
		private[ZigZag] def until = offset + length
//		override def depth = 0
		override def apply(i :Int) :A =
			if (i < 0 || i >= length)
				outOfBounds_!(i.toString + " out of " + length)
			else
				elems(offset + i)

		override def segmentLength(p :A => Boolean, from :Int) :Int =
			if (from >= length)
				0
			else if (applyPreferred(elems)) {
				val end   = offset + length
				val start = offset + math.max(from, 0)
				var i     = start
				while (i < end && p(elems(i)))
					i += 1
				i - start
			} else {
				var count = 0
				val it    = elems.iterator.drop(offset + math.max(from, 0))
				while (it.hasNext && p(it.next()))
					count += 1
				count
			}
		override def reversed = elems.view.slice(offset, offset + length).reverse
		override def iterator = new IndexedSeqIterator(elems, offset, offset + length)
		override def reverseIterator :Iterator[A] = new ReverseIndexedSeqIterator(elems, offset, offset + length)
		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[A, S]) :S =
			IndexedSeqStepper(elems, offset, offset + length)

		override def clippedSlice(from :Int, until :Int) = new Slice(elems, offset + from, until - from)

//		override def updated[B >: A](index :Int, elem :B) :ZigZag[B] =
//			ZigZag.from(elems.view.slice(offset, offset + length).updated(index, elem))
//
//		override def patch[B >: A](from :Int, other :IterableOnce[B], replaced :Int) :ZigZag[B] =
//			ZigZag.from(elems.view.slice(offset, offset + length).patch(from, other, replaced))

		override def foreach[U](f :A => U) :Unit = {
			var i = offset; val end = offset + length
			while (i < end) {
				f(elems(i))
				i += 1
			}
		}
//		override def stackForEach[U](f :A => U) :Unit = foreach(f)

		override def iterableFactory :SeqFactory[Seq] = IndexedSeq
	}


	@SerialVersionUID(Ver)
	private class Concat[+A](prefix :Seq[A], suffix :Seq[A]) extends ZigZag[A] {
		def _1 = prefix
		def _2 = suffix
		override lazy val length = prefix.length + suffix.length
//		override def depth = math.max(prefix.depth, suffix.depth)

//		override def iterator :Iterator[A] = prefix.iterator :++ suffix.iterator
//		override def reverseIterator :Iterator[A] = suffix.reverseIterator :++ prefix.reverseIterator
//
//		override def javaIterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[A, I]) :I =
//			javaIteratorOver(prefix) ++ javaIteratorOver(suffix)
//
//		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[A, S]) :S =
//			prefix.stepper ++ suffix.stepper
//
//		override def trustedSlice(from :Int, until :Int) :ZigZag[A] =
//			if (until <= prefixLen)
//				ZigZag.from(sliceOf(prefix, from, until))
//			else if (from >= prefixLen)
//				new Straight(suffix.slice(from - prefixLen, until - prefixLen))
//			else {
//				val first = sliceOf(prefix, from, prefixLen)
//				val second = sliceOf(suffix, 0, until - prefixLen)
//				new Concat(first, second)
//			}

//		override def updated[B >: A](index :Int, elem :B) :ZigZag[B] =
//			if (index < prefixLen) new Concat(prefix.updated(index, elem), suffix)
//			else new Concat(prefix, suffix.updated(index - prefixLen, elem))
//
//		override def patch[B >: A](from :Int, other :IterableOnce[B], replaced :Int) :ZigZag[B] = {
//			val start = math.min(math.max(from, 0), length)
//			val end   = start + math.min(math.max(replaced, 0), length - start)
//			if (end == 0) prependedAll(other)
//			else if (start == length) appendedAll(other)
//			else if (end <= prefixLen) new Concat(prefix.patch(from, other, replaced), suffix)
//			else if (start >= prefixLen) new Concat(prefix, suffix.patch(start - prefixLen, other, replaced))
//			else ZigZag.from(prefix.view.take(start).concat(other.iterator).concat(suffix.view.drop(end - prefixLen)))
//		}
//
//		override def stackForEach[U](f :A => U) :Unit = {
//			prefix foreach f
//			suffix foreach f
//		}
	}


	@SerialVersionUID(Ver)
	private class Appended[+A](override val init :ZigZag[A], override val last :A) extends ZigZag[A] {
		override lazy val length :Int = init.length + 1

//		override def iterator :Iterator[A] = init.iterator :+ last
//		override def reverseIterator :Iterator[A] = last +: init.reverseIterator
//
//		override def javaIterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[A, I]) :I =
//			init.javaIterator ++ JavaIterator.one(last)
//
//		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[A, S]) :S =
//			init.stepper :++ Stepper1(last)
//
//		override def trustedSlice(from :Int, until :Int) :ZigZag[A] =
//			if (from == prefixLen)
//				new Appended(Empty, last)
//			else if (until <= prefixLen)
//				init.trustedSlice(from, until)
//			else
//				new Appended(init.trustedSlice(from, prefixLen), last)
//
//		override def updated[B >: A](index :Int, elem :B) :ZigZag[B] =
//			if (index == prefixLen) new Appended(init, elem)
//			else new Appended(init.updated(index, elem), last)
//
//		override def patch[B >: A](from :Int, other :IterableOnce[B], replaced :Int) :ZigZag[B] = {
//			val start = math.min(math.max(from, 0), length)
//			val end = start + math.min(math.max(replaced, 0), length - start)
//			if (end == 0) prependedAll(other)
//			else if (start == length) appendedAll(other)
//			else if (end == length) init.patch(start, other, end - start)
//			else new Appended(ZigZag.from(init.patch(start, other, replaced)), last)
//		}
//
//		override def stackForEach[U](f :A => U) :Unit = {
//			init foreach f
//			f(last)
//		}
	}


	@SerialVersionUID(Ver)
	private class Prepended[+A](override val head :A, override val tail :ZigZag[A]) extends ZigZag[A] {
		override lazy val length :Int = 1 + tail.length

//		override def iterator :Iterator[A] = head +: tail.iterator
//		override def reverseIterator :Iterator[A] = tail.reverseIterator :+ head
//
//		override def javaIterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[A, I]) :I =
//			JavaIterator.one(head) ++ tail.javaIterator
//
//		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[A, S]) :S =
//			(Stepper1(head) :S) ++ tail.stepper
//
//		override def trustedSlice(from :Int, until :Int) :ZigZag[A] =
//			if (from >= 1)
//				tail.trustedSlice(from - 1, until - 1)
//			else if (until == 1)
//				new Prepended(head, Empty)
//			else
//				new Prepended(head, tail.trustedSlice(0, until - 1))
//
//		override def updated[B >: A](index :Int, elem :B) :ZigZag[B] =
//			if (index == 0)
//				new Prepended(elem, tail)
//			else if (index < 0)
//				outOfBounds_!("ZigZag<" + length + ">.updated(" + index + ", " + elem + ")")
//			else
//				new Prepended(head, tail.updated(index - 1, elem))
//
//		override def patch[B >: A](from :Int, other :IterableOnce[B], replaced :Int) :ZigZag[B] = {
//			val start = math.min(math.max(from, 0), length)
//			val end = start + math.min(math.max(replaced, 0), length - start)
//			if (end == 0) prependedAll(other)
//			else if (start == length) appendedAll(other)
//			else if (start > 0) new Prepended(head, tail.patch(start - 1, other, replaced))
//			else tail.patch(start - 1, other, end - 1)
//		}
//
//		override def stackForEach[U](f :A => U) :Unit = {
//			f(head)
//			tail foreach f
//		}
	}

	override def toString = "ZigZag"
}
