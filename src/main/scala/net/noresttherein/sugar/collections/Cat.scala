package net.noresttherein.sugar.collections

import java.lang.{Math => math}

import scala.annotation.{nowarn, tailrec}
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.generic.DefaultSerializationProxy
import scala.collection.{BufferedIterator, SeqFactory, SeqView, Stepper, StepperShape, View, immutable, mutable}
import scala.collection.immutable.{AbstractSeq, StrictOptimizedSeqOps}
import scala.collection.mutable.{Buffer, Builder}

import net.noresttherein.sugar.collections.IndexedIterable.applyPreferred
import net.noresttherein.sugar.collections.Cat.{Appended, Concat, Prepended, Straight}
import net.noresttherein.sugar.collections.extensions.{IterableOnceExtension, StepperExtension}
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.exceptions.{noSuch_!, outOfBounds_!}
import net.noresttherein.sugar.reflect.extensions.classNameMethods




/** A recursive list-like sequence to which elements can be both prepended and appended in O(1) time;
  * `length` is also an O(1) operation.
  * It offers O(n) traversal, but subpar to standard `Seq` implementations and similarly subpar O(n) random access.
  * Each time an element or collection is prepended or appended, only a single object is created, combining
  * this sequence with the new elements. This makes it suited only as an intermediate buffer structure,
  * where collections are concatenated recursively, which would lead to O(n*n) time in standard implementations.
  * After the contents are complete, it is advised to convert this sequence into some all purpose `Seq` implementation;
  * this will also happen automatically when [[scala.collection.IterableOps.map mapping]] it. Note that `Cat` type
  * is not 'sticky': operations other than appending/prepending and slicing return more conventional
  * `Seq` implementations.
  *
  * This class is essentially the same as `Chain` from `cats` library,
  * but implemented within the standard collection framework.
  * @define Coll `Cat`
  * @define coll cat sequence
  * @author Marcin Mościcki marcin@moscicki.net
  */ //other names: Chain or ZigZag
sealed abstract class Cat[+E]
	extends AbstractSeq[E] with SugaredIterable[E] with SugaredSeqOps[E, Seq, Seq[E]]
	   with StrictOptimizedSeqOps[E, Seq, Seq[E]] with SlicingOps[E, Cat[E]] with Serializable
{
	//todo: make it serializable through ArraySerializationProxy after copyToArray, which will be much faster
	// than creating the iterator.
	override def knownSize :Int = length

	protected override def hasFastSlice :Boolean = true
	protected override def emptySlice   :Cat[E] = Cat.empty
	override def empty :Cat[E] = Cat.empty

	protected override def clippedSlice(from :Int, until :Int) :Cat[E] = {
		@tailrec def slice(in :Seq[E], from :Int, until :Int, prefix :Cat[E],
		                   suffix :mutable.Queue[Cat[E]], lastUntil :Int) :Cat[E] =
		{
			var from0      = from
			var until0     = until
			var lastUntil0 = lastUntil
			var next       = null :Seq[E]
			var newPrefix  = prefix
			var newSuffix  = suffix
			in match {
				case _ if from >= until => ()
				case concat :Concat[E]  =>
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
		slice(this, from, until, Cat.empty, null, 0)
	}

	override def segmentLength(p :E => Boolean, from :Int) :Int = {
		@tailrec def segment(in :Seq[E], idx :Int, prefix :Int, suffix :mutable.Queue[Cat[E]]) :Int = {
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
						newSuffix = if (suffix == null) mutable.Queue.empty[Cat[E]] else suffix
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
						newSuffix = if (suffix == null) mutable.Queue.empty[Cat[E]] else suffix
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

	//todo: foldLeft

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

	override def updated[U >: E](index :Int, elem :U) :Cat[U] = //Cat.from(super.updated(index, elem))
		if (index < 0 || index >= length)
			outOfBounds_!(index, length)
		else if (index == 0)
			new Prepended(elem, drop(1))
		else if (index == length - 1)
			new Appended(dropRight(1), elem)
		else
			new Concat(new Appended(take(index), elem), drop(index + 1))

	override def updatedAll[U >: E](index :Int, elems :IterableOnce[U]) :Cat[U] = {
		val size = elems.knownSize
		if (index < 0 || index >= length)
			outOfBounds_!(index, length)
		else if (size == 0)
			this
		else if (size < 0)
			updatedAll(index, TemporaryIndexedSeq from elems)
		else {
			if (index > length - size)
				outOfBounds_!(errorString(this) + ".updatedAll(" + index + ", " + errorString(elems) + ")")
			slice(0, index) appendedAll elems appendedAll slice(index + size, length)
		}
	}

	override def updatedAll[U >: E](index :Int, first :U, second :U, rest :U*) :Cat[U] =
		updatedAll(index, new Prepended(first, new Prepended(second, Cat.from(rest))))

	override def overwritten[U >: E](index :Int, elems :IterableOnce[U]) :Cat[U] = {
		val length = this.length
		if (index >= length)
			this
		else elems.knownSize match {
			case  0 => this
			case -1 => overwritten(index, TemporaryIndexedSeq from elems)
			case  n =>
				if (-n >= index) //not n <= -index because index may be Int.MinValue
					this
				else if (index <= 0) //important to use iterator.slice, not slice, in case elems is unordered
					if (index + n >= length) Cat.from(elems.iterator.slice(-index, -index + length))
					else drop(index + n).prependedAll(elems.iterator.drop(-index))
				else if (n >= length - index)
					take(index).appendedAll(elems.iterator.take(length - index))
				else
					slice(0, index) appendedAll elems appendedAll slice(index + n, length)
		}
	}

	//overridden because super returns Seq
	override def overwritten[U >: E](index :Int, first :U, second :U, rest :U*) :Cat[U] =
		overwritten(index, new Prepended(first, new Prepended(second, Cat.from(rest))))

	override def inserted[U >: E](index :Int, elem :U) :Cat[U] =
		if (index < 0 || index > length)
			outOfBounds_!(index, length)
		else
			slice(0, index) appended elem appendedAll slice(index, length)

	override def insertedAll[U >: E](index :Int, elems :IterableOnce[U]) :Cat[U] =
		if (index < 0 || index > length)
			outOfBounds_!(index, length)
		else
			slice(0, index) appendedAll elems appendedAll slice(index, length)

	override def insertedAll[U >: E](index :Int, first :U, second :U, rest :U*) :Cat[U] =
		insertedAll(index, new Prepended(first, new Prepended(second, Cat.from(rest))))

	//fixme: we can't override :++ and ++: :(
	override def appended[U >: E](elem :U) :Cat[U] = new Appended(this, elem)
	override def prepended[U >: E](elem :U) :Cat[U] = new Prepended(elem, this)

	override def appendedAll[U >: E](suffix :IterableOnce[U]) :Cat[U] = suffix match {
		case view :View[U]                       => appendedAll(TemporaryIndexedSeq from view)
		case it   :Iterable[_] if it.isEmpty     => this
		case _ if isEmpty                        => Cat.from(suffix)
		case it   :Iterable[U] if it.sizeIs == 1 => new Appended(this, it.head)
		case seq  :Seq[U]                        => new Concat(this, seq)
		case it   :Iterable[U]                   => new Concat(this, it to TemporaryIndexedSeq)
		case _                                   => appendedAll(TemporaryIndexedSeq from suffix)
	}
	override def prependedAll[U >: E](prefix :IterableOnce[U]) :Cat[U] = prefix match {
		case view :View[U]                       => prependedAll(TemporaryIndexedSeq from view)
		case it   :Iterable[_] if it.isEmpty     => this
		case _ if isEmpty                        => Cat.from(prefix)
		case it   :Iterable[U] if it.sizeIs == 1 => new Prepended(it.head, this)
		case seq  :Seq[U]                        => new Concat(seq, this)
		case it   :Iterable[U]                   => new Concat(it to TemporaryIndexedSeq, this)
		case _                                   => prependedAll(TemporaryIndexedSeq from prefix)
	}

	//narrow the return type, overridden by subclasses
	override def patch[U >: E](from :Int, other :IterableOnce[U], replaced :Int) :Cat[U] =
		if (from >= length) appendedAll(other)
		else if (from <= 0) drop(replaced) prependedAll other
		else if (replaced <= 0) take(from) appendedAll other appendedAll drop(from)
		else if (replaced >= length - from) take(from) appendedAll other
		else take(from) appendedAll other appendedAll drop(from + replaced)

	override def removed(index :Int) :Cat[E] = {
		val len = length
		val Last = len - 1
		val SecondLast = len - 2
		if (index < 0 || index >= len)
			outOfBounds_!(index, len)
		else if (len == 1)
			Cat.empty
		else index match {
			case 0          => clippedSlice(1, len)
			case 1          => new Prepended(head, clippedSlice(2, len))
			case SecondLast => new Appended(clippedSlice(2, SecondLast), last)
			case Last       => clippedSlice(0, Last)
			case _          => new Concat(clippedSlice(0, index), clippedSlice(index + 1, len))
		}
	}

	override def removed(from :Int, until :Int) :Cat[E] =
		if (until <= 0 | until <= from || from >= length) this
		else slice(0, from) appendedAll slice(until, length)


	override def foreach[U](f :E => U) :Unit = {
		@inline def init(suffix :mutable.Queue[Cat[E]]) =
			if (suffix == null) mutable.Queue.empty[Cat[E]] else suffix

		@tailrec def rec(in :Seq[E], suffix :mutable.Queue[Cat[E]]) :Unit = in match {
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

	//todo: better iterator, using internally a stack with the path in this seq as a tree.
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


	/*
	override def copyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int = copyRangeToArray(xs, start, 0, len)

	override def copyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int = {
		val from0 = math.max(from, 0)
		if (len <= 0 || start >= xs.length || from0 >= length)
			0
		else if (start < 0)
			outOfBounds_!(start, length)
		else {
			@inline def init(suffix :mutable.Queue[Cat[E]]) =
				if (suffix == null) mutable.Queue.empty[Cat[E]] else suffix

			@tailrec def copy(in :Seq[E], drop :Int, offset :Int, max :Int, suffix :mutable.Queue[Cat[E]]) :Unit =
				in match {
					case concat    :Concat[E]    =>
						val first  = concat._1
						val second = concat._2
						val len1   = first.length - drop
						if (len1 < 0)
							copy(second, -drop, offset, max, ???)
						else if (len1 <= max)
							copy(first, drop, offset, max, ???)
						else if (!first.isInstanceOf[Cat[_]]) {
							if (drop > 0) first.copyRangeToArray(xs, offset, drop, max)
							else first.copyToArray(xs, offset, max)
							copy(second, 0, offset + len1, max - len1, ???)
						} else if (!second.isInstanceOf[Cat[_]]) {
							second.copyToArray(xs, offset + len1, max - len1)
							copy(first, drop, offset, max, ???)
						} else
							copy(first, drop, offset, max, concat +=: init(suffix))
					case appended  :Appended[E]  =>
						val len = appended.length

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
			val copied = math.max(0, math.min(math.min(len, xs.length - start), length - from0))
			copy(this, from0, start, copied, ???)
		}
	}
*/
	//todo: there is a serious potential for improving it by recursion like in foreach.
	override def copyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int =
		if (len <= 0 || start >= xs.length || length == 0)
			0
		else if (start < 0)
			outOfBounds_!(start, length)
		else
			iterator.copyToArray(xs, start, len)

	override def copyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int =
		drop(from).copyToArray(xs, start, len)

	protected[this] def writeReplace :AnyRef = new DefaultSerializationProxy(Cat, this)
	protected override def className = "Cat"
}




/** $factoryInfo
  * @define Coll `Cat`
  * @define coll cat
  */
@SerialVersionUID(Ver)
case object Cat extends SeqFactory[Cat] {

	override def from[A](source :IterableOnce[A]) :Cat[A] = source match {
		case zigzag :Cat[A] => zigzag
		case empty  :Iterable[_] if empty.knownSize == 0 => Empty
		case seq    :Seq[A] => new Straight(seq)
		case _ => new Straight((DefaultArraySeq.newBuilder[A] ++= source).result())
	}
	override def empty[A] :Cat[A] = Empty

	override def newBuilder[A] :Builder[A, Cat[A]] = DefaultArraySeq.newBuilder[A] mapResult (new Straight(_))

	/** Similar to [[collection.IndexedSeqView IndexedSeqView]], but a `Cat`, meaning non-slicing operations
	  * build strict, normal (non `Cat`) sequences, rather than another view. Unlike other zigzags,
	  * it will map to an `IndexedSeq`, rather than a `List`.
	  */
	def slice[A](seq :IndexedSeq[A], from :Int, until :Int) :Cat[A] =
		if (until <= from | until <= 0 || from > seq.length)
			Empty
		else {
			val from0 = math.max(from, 0)
			new Slice(seq, from, math.min(until, seq.length) - from0)
		}

	def :+[E](first :E) :Cat[E] = new Appended(Empty, first)
	def :++[E](init :IterableOnce[E]) :Cat[E] = from(init)


	@SerialVersionUID(Ver)
	private class EmptyCat extends Cat[Nothing] { //with EmptyIterableOps.Generic[Seq] {
		override def length :Int = 0
		override def apply(i :Int) = outOfBounds_!(i.toString + " out of 0")
		override def clippedSlice(from :Int, until :Int) :Cat[Nothing] = this
		override def segmentLength(p :Nothing => Boolean, from :Int) :Int = 0
		override def view :SeqView[Nothing] = EmptySeqOps.view
		override def iterator = Iterator.empty
		override def reverseIterator = Iterator.empty
		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[Nothing, S]) :S = Stepper0()

		override def foreach[U](f :Nothing => U) :Unit = ()
		private def readResolve :Seq[Nothing] = Cat.empty
	}

	private[this] final val Empty :Cat[Nothing] = new EmptyCat


	@SerialVersionUID(Ver)
	private class Straight[+A](elems :Seq[A]) extends Cat[A] {
		private[Cat] def elements = elems
		override lazy val length :Int = elems.length
		override def head = elems.head
		override def last = elems.last
		override def tail = Cat.from(elems.tail)
		override def apply(i :Int) :A = elems(i)
		override def segmentLength(p :A => Boolean, from :Int) :Int = elems.segmentLength(p, from)
		override def reversed :Seq[A] = elems.reverse
		override def iterator :Iterator[A] = elems.iterator
		override def reverseIterator :Iterator[A] = elems.reverseIterator
		override def jterator[I <: Jterator[_]](implicit shape :JteratorShape[A, I]) :I = elems.jterator
		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[A, S]) :S = elems.stepper

		override def clippedSlice(from :Int, until :Int) :Cat[A] = elems match {
			case indexed :IndexedSeq[A] => new Slice(indexed, from, until - from)
			case _ => new Straight(elems.slice(from, until))
		}

		override def foreach[U](f :A => U) :Unit = elems.foreach(f)
	}


	@SerialVersionUID(Ver)
	private class Slice[+A](elems :IndexedSeq[A], offset :Int, override val length :Int) extends Cat[A] {
		private[Cat] def elements = elems
		private[Cat] def from = offset
		private[Cat] def until = offset + length
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

		override def foreach[U](f :A => U) :Unit = {
			var i = offset; val end = offset + length
			while (i < end) {
				f(elems(i))
				i += 1
			}
		}
		override def iterableFactory :SeqFactory[Seq] = IndexedSeq
	}


	//A shared superclass of Prepended and Concat useful in the implementation of iterator and some recursive methods.
	private abstract class HasTail[+A] extends Cat[A] {
		def _2 :Seq[A]
	}
	@SerialVersionUID(Ver)
	private class Concat[+A](val _1 :Seq[A], val _2 :Seq[A]) extends HasTail[A] {
		override lazy val length = _1.length + _2.length
	}

	@SerialVersionUID(Ver)
	private class Prepended[+A](override val head :A, override val tail :Cat[A]) extends HasTail[A] {
		override lazy val length :Int = 1 + tail.length
		def _2 = tail
	}

	@SerialVersionUID(Ver)
	private class Appended[+A](override val init :Cat[A], override val last :A) extends Cat[A] {
		override lazy val length :Int = init.length + 1
	}


	private final class CatIterator[+A](cat :Cat[A]) extends BufferedIterator[A] with IteratorWithDrop[A] {
		private[this] val stack = mutable.Queue.empty[Cat[A]]
		private[this] var hd :A = _
		private[this] var hasHead = false
		private[this] var iter :Iterator[A] = _
		dropFrom(cat, 0)

		override def knownSize :Int = {
			var size = iter.knownSize
			if (size <= 0)
				size
			else {
				var next = 0
				var i = 0; val end = stack.length
				while (i < end && { next = stack(i).knownSize; next >= 0}) {
					size += next
					i += 1
				}
				if (i < end) -1 else size
			}
		}

		override def head = if (hasHead) hd else noSuch_!("Cat.empty.iterator.head")
		override def hasNext = hasHead
		override def next() :A = {
			if (!hasHead)
				noSuch_!("Cat.empty.iterator.head")
			val res = hd
			advance()
			res
		}

		private def advance() :Unit =
			if (iter.hasNext)
				hd = iter.next()
			else {
				hasHead = false
				climb()
			}

		/** Drops `n` elements from `seq`, prepending the nodes on the path to non-composite `Cat` to the stack,
		  * setting the iterator in a ready state to iterate over `seq.drop(n)` (and beyond). Requires seq.length < n.
		  */
		@tailrec private[this] def dropFrom(seq :Seq[A], n :Int) :Unit =
			seq match {                //We use sizeIs because we are mostly called for n==0.
				case cat :Concat[A]    if cat._1.sizeIs > n   => cat +=: stack; dropFrom(cat._1, n)
				case cat :Concat[A]                           => dropFrom(cat._2, n - cat._1.length)
				case cat :Appended[A]  if cat.init.sizeIs > n => cat +=: stack; dropFrom(cat.init, n)
				case cat :Appended[A]                         => hd = cat.last; iter = Iterator.empty; hasHead = true
				case cat :Prepended[A] if n > 0               => dropFrom(cat.tail, n - 1)
				case cat :Prepended[A]                        => hd = cat.head; iter = cat.iterator; hasHead = true
				case _                 if seq.isEmpty         => climb() //should never happen, but better be robust.
				case _                                        =>
					iter = seq.iterator.drop(n)
					hd = iter.next()
					hasHead = true
			}

		//Called when hasHead is false.
		private def climb() :Unit =
			if (stack.nonEmpty)
				(stack.removeHead() : @nowarn) match { //The stack contains only these three types of cats.
					case cat :HasTail[A]   => dropFrom(cat._2, 0)
					case cat :Appended[A]  => hd = cat.last; hasHead = true
				}

		@tailrec override def drop(n :Int) :Iterator[A] =
			if (!hasHead | n <= 0)
				this
			else {
				var s = 0
				if (n == 1) {
					advance()
					this
				} else if ({ s = iter.knownSize; s >= 0 }) {
					//If we retained the reference to the current Seq and had an index, we would be able to avoid
					// iterator.drop. However, it is not certain that `drop` on the sequence would be faster.
					if (s > n - 1) {                             //Implicitly drop head and the rest from iter.
						iter = iter.drop(n - 1)
						hd = iter.next()
						this
					} else if (stack.isEmpty) {                  //The iterator becomes empty.
						hasHead = false
						this
					} else (stack.removeHead(): @nowarn) match { //Drop head, the whole iter, and n-s-1 from the stack.
						case cat :HasTail[A] =>
							val len = cat._2.length
							if (len > n - s - 1) {               //Head of the stack contained our next element.
								dropFrom(cat._2, n - s - 1)      //Find the leaf seq of cat._2 containing the next element.
								this
							} else                               //Drop the whole head of the stack and recurse.
								drop(n - s - len)                //hasHead is true, so this will drop one fewer element.
						case cat :Appended[A] =>
							if (n - s - 1 == 0) {
								hd      = cat.last
								hasHead = true
								iter    = Iterator.empty
								this
							} else
								drop(n - s - 1)
					}
				} else {
					var rem = n
					if (rem > 0 && iter.hasNext) {
						iter.next()
						rem -= 1
					}
					if (rem > 0)
						drop(rem + 1)  //hasHead is true, so drop erronously counts hd as the first element.
					else {
						advance()
						this
					}
				}
			}
		//todo: copyToArray; drop; take;
	}
}
