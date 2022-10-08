package net.noresttherein.sugar.collections

import scala.collection.{SeqFactory, Stepper, StepperShape}
import scala.collection.immutable.AbstractSeq
import scala.collection.mutable.Builder

import net.noresttherein.sugar.JavaTypes.JIterator
import net.noresttherein.sugar.collections.ZigZag.{Appended, Concat, Prepended}

//implicits
import net.noresttherein.sugar.extensions.{javaIteratorExtension, stepperExtension}




/** A recursive list-like sequence to which elements can be both prepended and appended in O(1) time;
  * `length` is also an O(1) operation.
  * It offers O(n) traversal, but subpar to standard `Seq` implementations and relying on the thread stack
  * (so may cause `StackOverflowError` for very long collections), and likewise subpar random access.
  * Each time an element or collection is prepended or appended, only a single object is created, combining
  * this sequence with the new elements. This makes it suited only as an intermediate buffer structure,
  * where collections are concatenated recursively, which would lead to O(n*n) time in standard implementations.
  * After the contents are complete, it is advised to convert this sequence into some all purpose `Seq` implementation;
  * this will happen automatically by [[scala.collection.IterableOps.map mapping]] it. Note that `ZigZag` type
  * is not 'sticky': operations other than slicing return more conventional `Seq` implementations.
  */
sealed trait ZigZag[+A] extends Seq[A] with SugaredIterable[A] with Serializable {
//	private[ZigZag] def depth :Int

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
		if (until < from | until <= 0 || from >= length) empty
		else if (from <= 0 && until >= length) this
		else if (from <= 0) trustedSlice(0, until)
		else trustedSlice(from, length)

	protected[collections] def trustedSlice(from :Int, until :Int) :ZigZag[A]

//	override def iterableFactory :SeqFactory[ZigZag] = ZigZag
	override def className = "ZigZag"
}




/** $factoryInfo
  * @define Coll `ZigZag`
  * @define coll zigzag
  */
@SerialVersionUID(ver)
object ZigZag extends SeqFactory[ZigZag] {

	override def from[A](source :IterableOnce[A]) :ZigZag[A] = source match {
		case zigzag :ZigZag[A] => zigzag
		case empty  :Iterable[_] if empty.isEmpty => Empty
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



	@SerialVersionUID(ver)
	private class EmptyZigZag extends AbstractSeq[Nothing] with ZigZag[Nothing] {
//		override def depth :Int = 0
		override def length :Int = 0
		override def apply(i :Int) :Nothing = throw new NoSuchElementException("ZigZag()")
		override def iterator :Iterator[Nothing] = Iterator.empty
		override def reverseIterator :Iterator[Nothing] = Iterator.empty
		override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[Nothing, I]) :I = JavaIterator()
		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[Nothing, S]) :S = Stepper0()
		override def trustedSlice(from :Int, until :Int) :ZigZag[Nothing] = this

		override def foreach[U](f :Nothing => U) :Unit = {}
		private def writeReplace :Seq[Nothing] = ZigZag.empty
	}

	private[this] final val Empty :ZigZag[Nothing] = new EmptyZigZag


	@SerialVersionUID(ver)
	private class Straight[+A](elems :Seq[A]) extends AbstractSeq[A] with ZigZag[A] {
		private[ZigZag] def elements = elems
//		override def depth :Int = 0
		override val length :Int = elems.length
		override def apply(i :Int) :A = elems(i)
		override def iterator :Iterator[A] = elems.iterator
		override def reverseIterator :Iterator[A] = elems.reverseIterator
		override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[A, I]) :I = jiteratorOver(elems)
		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[A, S]) :S = elems.stepper

		override def trustedSlice(from :Int, until :Int) :ZigZag[A] = elems match {
			case indexed :IndexedSeq[A] => new Slice(indexed, from, until - from)
			case _ => new Straight(elems.slice(from, until))
		}
		override def foreach[U](f :A => U) :Unit = elems.foreach(f)
	}


	@SerialVersionUID(ver)
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

		override def foreach[U](f :A => U) :Unit = {
			var i = offset; val end = offset + length
			while (i < end) {
				f(elems(i))
				i += 1
			}
		}
		private def writeReplace = new Straight(toIndexedSeq)
	}


	@SerialVersionUID(ver)
	private class Concat[+A](prefix :Seq[A], suffix :Seq[A]) extends AbstractSeq[A] with ZigZag[A] {
		private[this] val prefixLen = prefix.length
		override val length = prefixLen + suffix.length
//		override def depth = Math.max(prefix.depth, suffix.depth)
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

		override def foreach[U](f :A => U) :Unit = {
			prefix foreach f
			suffix foreach f
		}
		private def writeReplace = new Straight(toIndexedSeq)
	}


	@SerialVersionUID(ver)
	private class Appended[+A](prefix :ZigZag[A], last :A) extends AbstractSeq[A] with ZigZag[A] {
		private[this] val prefixLen = prefix.length
		override val length :Int = prefixLen + 1
		override def apply(i :Int) :A =
			if (i < 0)
				throw new IndexOutOfBoundsException(i.toString + " out of " + length)
			else if (i == prefixLen)
				last
			else
				prefix(i)

		override def iterator :Iterator[A] = prefix.iterator ++ Iterator.single(last)
		override def reverseIterator :Iterator[A] = Iterator.single(last) ++ prefix.reverseIterator

		override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[A, I]) :I =
			prefix.jiterator ++ JavaIterator(last)

		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[A, S]) :S =
			prefix.stepper ++ Stepper1(last)

		override def trustedSlice(from :Int, until :Int) :ZigZag[A] =
			if (from == prefixLen)
				new Appended(Empty, last)
			else if (until <= prefixLen)
				prefix.trustedSlice(from, until)
			else
				new Appended(prefix.trustedSlice(from, prefixLen), last)

		override def foreach[U](f :A => U) :Unit = {
			prefix foreach f
			f(last)
		}
		private def writeReplace = new Straight(toIndexedSeq)
	}


	@SerialVersionUID(ver)
	private class Prepended[+A](first :A, suffix :ZigZag[A]) extends AbstractSeq[A] with ZigZag[A] {
		override val length :Int = 1 + suffix.length
		override def apply(i :Int) :A =
			if (i < 0)
				throw new IndexOutOfBoundsException(i.toString + " out of " + length)
			else if (i == 0)
				first
			else
				suffix(i - 1)

		override def iterator :Iterator[A] = Iterator.single(first) ++ suffix.iterator
		override def reverseIterator :Iterator[A] = suffix.reverseIterator ++ Iterator.single(first)

		override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[A, I]) :I =
			JavaIterator(first) ++ suffix.jiterator

		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[A, S]) :S =
			(Stepper1(first) :S) ++ suffix.stepper

		override def trustedSlice(from :Int, until :Int) :ZigZag[A] =
			if (from >= 1)
				suffix.trustedSlice(from - 1, until - 1)
			else if (until == 1)
				new Prepended(first, Empty)
			else
				new Prepended(first, suffix.trustedSlice(0, until - 1))

		override def foreach[U](f :A => U) :Unit = {
			f(first)
			suffix foreach f
		}
		private def writeReplace = new Straight(toIndexedSeq)
	}
}
