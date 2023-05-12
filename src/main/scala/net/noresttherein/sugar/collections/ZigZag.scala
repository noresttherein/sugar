package net.noresttherein.sugar.collections

import java.lang.{Math => math}

import scala.annotation.tailrec
import scala.collection.{SeqFactory, Stepper, StepperShape}
import scala.collection.immutable.AbstractSeq
import scala.collection.mutable.Builder

import net.noresttherein.sugar.JavaTypes.JIterator
import net.noresttherein.sugar.collections.ZigZag.{Appended, Concat, Prepended}

//implicits
import net.noresttherein.sugar.extensions.{JavaIteratorExtension, StepperExtension}




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
  * individual elements. This class is essentially the same as `Chain` from the `cats` library,
  * but implemented within the standard collection framework.
  */ //consider: renaming to Chain
sealed trait ZigZag[+A] extends Seq[A] with SugaredIterable[A] with Serializable {
//	private[ZigZag] def depth :Int
	override def knownSize :Int = length

	override def appended[B >: A](elem :B) :ZigZag[B] = new Appended(this, elem)
	override def prepended[B >: A](elem :B) :ZigZag[B] = new Prepended(elem, this)

	override def appendedAll[B >: A](suffix :IterableOnce[B]) :ZigZag[B] = suffix match {
		case it :Iterable[_] if it.isEmpty => this
		case _ if isEmpty => ZigZag.from(suffix)
		case it :Iterable[B] if it.sizeIs == 1 => new Appended(this, it.head)
		case seq :Seq[B] => new Concat(this, seq)
		case it :Iterable[B] => new Concat(this, it to PassedArray)
		case _ => new Concat(this, suffix.iterator to PassedArray)
	}
	override def prependedAll[B >: A](prefix :IterableOnce[B]) :ZigZag[B] = prefix match {
		case it :Iterable[_] if it.isEmpty     => this
		case _ if isEmpty                      => ZigZag.from(prefix)
		case it :Iterable[B] if it.sizeIs == 1 => new Prepended(it.head, this)
		case seq :Seq[B]                       => new Concat(seq, this)
		case it :Iterable[B]                   => new Concat(it to PassedArray, this)
		case _                                 => new Concat(prefix.iterator to PassedArray, this)
	}

	override def empty :ZigZag[A] = ZigZag.empty
	override def take(n :Int) :ZigZag[A] = slice(0, n)
	override def drop(n :Int) :ZigZag[A] = slice(n, length)
	override def takeRight(n :Int) :ZigZag[A] = if (n <= 0) empty else slice(length - n, length)
	override def dropRight(n :Int) :ZigZag[A] = if (n <= 0) this else slice(0, length - n)

	override def slice(from :Int, until :Int) :ZigZag[A] =
		if (until <= from | until <= 0 || from >= length) empty
		else if (from <= 0 && until >= length) this
		else if (from <= 0) trustedSlice(0, until)
		else if (until > length) trustedSlice(from, length)
		else trustedSlice(from, until)

	protected[collections] def trustedSlice(from :Int, until :Int) :ZigZag[A]

	//narrow the return type, overridden by subclasses
	override def updated[B >: A](index :Int, elem :B) :ZigZag[B] = ZigZag.from(super.updated(index, elem))

	//narrow the return type, overridden by subclasses
	override def patch[B >: A](from :Int, other :IterableOnce[B], replaced :Int) :ZigZag[B] =
		ZigZag.from(super.patch(from, other, replaced))

	override def foreach[U](f :A => U) :Unit = try {
		stackForEach(f)
	} catch {
		case _ :StackOverflowError =>
			//shut up tailrec error
			@inline def headForeach(seq :Seq[A], cont :() => Unit) :Unit = rec(seq, cont)
			@tailrec def rec(seq :Seq[A], cont :() => Unit) :Unit = seq match {
				case concat    :Concat[A]    => rec(concat._1, () => headForeach(concat._2, cont))
				case append    :Appended[A]  => f(append.head); rec(append.tail, cont)
				case prepended :Prepended[A] => rec(prepended.init, () => { f(prepended.last); cont() })
				case _      => seq.foreach(f)
			}
	}

	protected def stackForEach[U](f :A => U) :Unit

	//todo: remove these once the bugs are fixed in SeqOps
	override def startsWith[B >: A](that :IterableOnce[B], offset :Int) :Boolean =
		offset >= 0 && offset <= length && super.startsWith(that, offset)

	override def indexOfSlice[B >: A](that :collection.Seq[B], from :Int) :Int =
		if (from > length) -1
		else super.indexOfSlice(that, 0 max from)

	//	override def iterableFactory :SeqFactory[ZigZag] = ZigZag
	override def className = "ZigZag"
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
		case _ => new Straight((PassedArray.newBuilder[A] ++= source).result())
	}
	override def empty[A] :ZigZag[A] = Empty

	override def newBuilder[A] :Builder[A, ZigZag[A]] = PassedArray.newBuilder[A] mapResult (new Straight(_))


	private def jiteratorOver[A, I <: JIterator[_]](seq :Seq[A])(implicit shape :JavaIteratorShape[A, I]) :I =
		seq match {
			case sugared :SugaredIterable[A] => sugared.jiterator
			case indexed :IndexedSeq[A] => IndexedSeqStepper(indexed)(shape.stepperShape).javaIterator.asInstanceOf[I]
			case _ => seq.stepper(shape.stepperShape).javaIterator.asInstanceOf[I]
		}
	@inline private def sliceOf[A](seq :Seq[A], from :Int, until :Int) :Seq[A] = seq match {
		case zigzag :ZigZag[A] => zigzag.trustedSlice(from, until)
		case _ => seq.slice(from, until)
	}



	@SerialVersionUID(Ver)
	private class EmptyZigZag extends AbstractSeq[Nothing] with ZigZag[Nothing] {
//		override def depth :Int = 0
		override def length :Int = 0
		override def apply(i :Int) :Nothing = throw new IndexOutOfBoundsException("index " + i + " out of 0")
		override def iterator :Iterator[Nothing] = Iterator.empty
		override def reverseIterator :Iterator[Nothing] = Iterator.empty
		override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[Nothing, I]) :I = JavaIterator()
		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[Nothing, S]) :S = Stepper0()
		override def trustedSlice(from :Int, until :Int) :ZigZag[Nothing] = this

		override def updated[E >: Nothing](index :Int, elem :E) :Nothing =
			throw new IndexOutOfBoundsException("ZigZag.updated(" + index + ", " + elem + ")")

		override def patch[E >: Nothing](from :Int, other :IterableOnce[E], replaced :Int) :ZigZag[E] =
			ZigZag.from(other)

		override def foreach[U](f :Nothing => U) :Unit = {}
		override def stackForEach[U](f :Nothing => U) :Unit = {}
		private def writeReplace :Seq[Nothing] = ZigZag.empty
	}

	private[this] final val Empty :ZigZag[Nothing] = new EmptyZigZag


	@SerialVersionUID(Ver)
	private class Straight[+A](elems :Seq[A]) extends AbstractSeq[A] with ZigZag[A] {
		private[ZigZag] def elements = elems
//		override def depth :Int = 0
		override val length :Int = elems.length
		override def head = elems.head
		override def last = elems.last
		override def tail = ZigZag.from(elems.tail)
		override def apply(i :Int) :A = elems(i)
		override def iterator :Iterator[A] = elems.iterator
		override def reverseIterator :Iterator[A] = elems.reverseIterator
		override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[A, I]) :I = jiteratorOver(elems)
		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[A, S]) :S = elems.stepper

		override def trustedSlice(from :Int, until :Int) :ZigZag[A] = elems match {
			case indexed :IndexedSeq[A] => new Slice(indexed, from, until - from)
			case _ => new Straight(elems.slice(from, until))
		}

		override def updated[B >: A](index :Int, elem :B) :ZigZag[B] = new Straight(elems.updated(index, elem))
		override def patch[B >: A](from :Int, other :IterableOnce[B], replaced :Int) :ZigZag[B] =
			ZigZag.from(elems.patch(from, other, replaced))

		override def foreach[U](f :A => U) :Unit = elems.foreach(f)
		override def stackForEach[U](f :A => U) :Unit = elems.foreach(f)
	}


	@SerialVersionUID(Ver)
	private class Slice[+A](elems :IndexedSeq[A], offset :Int, override val length :Int)
		extends AbstractSeq[A] with ZigZag[A]
	{
		private[ZigZag] def elements = elems
		private[ZigZag] def from = offset
		private[ZigZag] def until = offset + length
//		override def depth = 0
		override def apply(i :Int) :A =
			if (i < 0 || i >= length)
				throw new IndexOutOfBoundsException(i.toString + " out of " + length)
			else
				elems(offset + i)

		override def iterator = IndexedSeqIterator(elems, offset, offset + length)
		override def reverseIterator :Iterator[A] = ReverseIndexedSeqIterator(elems, offset, offset + length)
		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[A, S]) :S =
			IndexedSeqStepper(elems, offset, offset + length)

		override def trustedSlice(from :Int, until :Int) = new Slice(elems, offset + from, until - from)

		override def updated[B >: A](index :Int, elem :B) :ZigZag[B] =
			ZigZag.from(elems.view.slice(offset, offset + length).updated(index, elem))

		override def patch[B >: A](from :Int, other :IterableOnce[B], replaced :Int) :ZigZag[B] =
			ZigZag.from(elems.view.slice(offset, offset + length).patch(from, other, replaced))

		override def foreach[U](f :A => U) :Unit = {
			var i = offset; val end = offset + length
			while (i < end) {
				f(elems(i))
				i += 1
			}
		}
		override def stackForEach[U](f :A => U) :Unit = foreach(f)
		private def writeReplace = new Straight(toIndexedSeq)
	}


	@SerialVersionUID(Ver)
	private class Concat[+A](prefix :Seq[A], suffix :Seq[A]) extends AbstractSeq[A] with ZigZag[A] {
		def _1 = prefix
		def _2 = prefix
		private[this] val prefixLen = prefix.length
		override val length = prefixLen + suffix.length
//		override def depth = math.max(prefix.depth, suffix.depth)
		override def apply(i :Int) :A =
			if (i < 0)
				throw new IndexOutOfBoundsException(i.toString + " out of " + length)
			else if (i < prefixLen)
				prefix(i)
			else
				suffix(i - prefixLen)

		override def iterator :Iterator[A] = prefix.iterator ++ suffix.iterator
		override def reverseIterator :Iterator[A] = suffix.reverseIterator ++ prefix.reverseIterator

		override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[A, I]) :I =
			jiteratorOver(prefix) ++ jiteratorOver(suffix)

		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[A, S]) :S =
			prefix.stepper ++ suffix.stepper

		override def trustedSlice(from :Int, until :Int) :ZigZag[A] =
			if (until <= prefixLen)
				ZigZag.from(sliceOf(prefix, from, until))
			else if (from >= prefixLen)
				new Straight(suffix.slice(from - prefixLen, until - prefixLen))
			else {
				val first = sliceOf(prefix, from, prefixLen)
				val second = sliceOf(suffix, 0, until - prefixLen)
				new Concat(first, second)
			}

		override def updated[B >: A](index :Int, elem :B) :ZigZag[B] =
			if (index < prefixLen) new Concat(prefix.updated(index, elem), suffix)
			else new Concat(prefix, suffix.updated(index - prefixLen, elem))

		override def patch[B >: A](from :Int, other :IterableOnce[B], replaced :Int) :ZigZag[B] = {
			val start = math.min(math.max(from, 0), length)
			val end   = start + math.min(math.max(replaced, 0), length - start)
			if (end == 0) prependedAll(other)
			else if (start == length) appendedAll(other)
			else if (end <= prefixLen) new Concat(prefix.patch(from, other, replaced), suffix)
			else if (start >= prefixLen) new Concat(prefix, suffix.patch(start - prefixLen, other, replaced))
			else ZigZag.from(prefix.view.take(start).concat(other.iterator).concat(suffix.view.drop(end - prefixLen)))
		}

		override def stackForEach[U](f :A => U) :Unit = {
			prefix foreach f
			suffix foreach f
		}
		private def writeReplace = new Straight(toIndexedSeq)
	}


	@SerialVersionUID(Ver)
	private class Appended[+A](override val init :ZigZag[A], override val last :A) extends AbstractSeq[A] with ZigZag[A] {
		private[this] val prefixLen = init.length
		override val length :Int = prefixLen + 1
		override def apply(i :Int) :A =
			if (i < 0)
				throw new IndexOutOfBoundsException(i.toString + " out of " + length)
			else if (i == prefixLen)
				last
			else
				init(i)

		override def iterator :Iterator[A] = init.iterator ++ Iterator.single(last)
		override def reverseIterator :Iterator[A] = Iterator.single(last) ++ init.reverseIterator

		override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[A, I]) :I =
			init.jiterator ++ JavaIterator(last)

		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[A, S]) :S =
			init.stepper ++ Stepper1(last)

		override def trustedSlice(from :Int, until :Int) :ZigZag[A] =
			if (from == prefixLen)
				new Appended(Empty, last)
			else if (until <= prefixLen)
				init.trustedSlice(from, until)
			else
				new Appended(init.trustedSlice(from, prefixLen), last)

		override def updated[B >: A](index :Int, elem :B) :ZigZag[B] =
			if (index == prefixLen) new Appended(init, elem)
			else new Appended(init.updated(index, elem), last)

		override def patch[B >: A](from :Int, other :IterableOnce[B], replaced :Int) :ZigZag[B] = {
			val start = math.min(math.max(from, 0), length)
			val end = start + math.min(math.max(replaced, 0), length - start)
			if (end == 0) prependedAll(other)
			else if (start == length) appendedAll(other)
			else if (end == length) init.patch(start, other, end - start)
			else new Appended(ZigZag.from(init.patch(start, other, replaced)), last)
		}

		override def stackForEach[U](f :A => U) :Unit = {
			init foreach f
			f(last)
		}
		private def writeReplace = new Straight(toIndexedSeq)
	}


	@SerialVersionUID(Ver)
	private class Prepended[+A](override val head :A, override val tail :ZigZag[A])
		extends AbstractSeq[A] with ZigZag[A]
	{
		override val length :Int = 1 + tail.length
		override def apply(i :Int) :A =
			if (i < 0)
				throw new IndexOutOfBoundsException(i.toString + " out of " + length)
			else if (i == 0)
				head
			else
				tail(i - 1)

		override def iterator :Iterator[A] = Iterator.single(head) ++ tail.iterator
		override def reverseIterator :Iterator[A] = tail.reverseIterator ++ Iterator.single(head)

		override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[A, I]) :I =
			JavaIterator(head) ++ tail.jiterator

		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[A, S]) :S =
			(Stepper1(head) :S) ++ tail.stepper

		override def trustedSlice(from :Int, until :Int) :ZigZag[A] =
			if (from >= 1)
				tail.trustedSlice(from - 1, until - 1)
			else if (until == 1)
				new Prepended(head, Empty)
			else
				new Prepended(head, tail.trustedSlice(0, until - 1))

		override def updated[B >: A](index :Int, elem :B) :ZigZag[B] =
			if (index == 0)
				new Prepended(elem, tail)
			else if (index < 0)
				throw new IndexOutOfBoundsException("ZigZag<" + length + ">.updated(" + index + ", " + elem + ")")
			else
				new Prepended(head, tail.updated(index - 1, elem))

		override def patch[B >: A](from :Int, other :IterableOnce[B], replaced :Int) :ZigZag[B] = {
			val start = math.min(math.max(from, 0), length)
			val end = start + math.min(math.max(replaced, 0), length - start)
			if (end == 0) prependedAll(other)
			else if (start == length) appendedAll(other)
			else if (start > 0) new Prepended(head, tail.patch(start - 1, other, replaced))
			else tail.patch(start - 1, other, end - 1)
		}


		override def stackForEach[U](f :A => U) :Unit = {
			f(head)
			tail foreach f
		}
		private def writeReplace = new Straight(toIndexedSeq)
	}

	override def toString = "ZigZag"
}
