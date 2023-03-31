package net.noresttherein.sugar.collections

import scala.collection.immutable.AbstractSeq
import scala.collection.{SeqFactory, Stepper, StepperShape}
import scala.collection.mutable.Builder
import scala.collection.Stepper.EfficientSplit

import net.noresttherein.sugar.JavaTypes.JIterator




/**
  * @author Marcin Mościcki
  */
@SerialVersionUID(ver)
class SeqSlice[+A] protected (whole :IndexedSeq[A], offset :Int, override val length :Int)
	extends AbstractSeq[A] with IndexedSeq[A] with SugaredIterable[A] with Serializable
{
	def this(whole :IndexedSeq[A]) = this (whole, 0, whole.length)

	override def apply(i :Int) :A =
		if (i >= length)
			throw new IndexOutOfBoundsException(i.toString + " out of " + length)
		else
			whole(offset + i)

	override def iterator :Iterator[A] = new IndexedSeqIterator(whole, offset, offset + length)
	override def reverseIterator :Iterator[A] = new ReverseIndexedSeqIterator(whole, offset, offset + length)
	override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[A, I]) :I =
		JavaIterator.slice(whole, offset, offset + length)

	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[A, S]) :S with EfficientSplit =
		IndexedSeqStepper(whole, offset, offset + length)

	override def empty :SeqSlice[A] = SeqSlice.empty[A]
	override def take(n :Int) :SeqSlice[A] = slice(0, n)
	override def drop(n :Int) :SeqSlice[A] = slice(n, length)
	override def takeRight(n :Int) :SeqSlice[A] = if (n <= 0) empty else slice(length - n, length)
	override def dropRight(n :Int) :SeqSlice[A] = if (n <= 0) this else slice(0, length - n)

	override def slice(from :Int, until :Int) :SeqSlice[A] =
		if (until < from | until <= 0 || from >= length) empty
		else if (from <= 0 && until >= length) this
		else if (from <= 0) new SeqSlice(whole, offset, until)
		else new SeqSlice(whole, offset + from, length - from)

	private def writeReplace = new Serializable {
		private[this] val data = whole.slice(offset, offset + length)
		private def readResolve = new SeqSlice(data)
	}
}




/** $factoryInfo
  * @define Coll `SeqSlice`
  * @define coll sequence slice
  */
@SerialVersionUID(ver)
object SeqSlice extends SeqFactory[SeqSlice] {

	def apply[A](seq :IndexedSeq[A], from :Int, until :Int) :SeqSlice[A] = {
		val len = seq.length
		if (from >= len) new SeqSlice(seq, len, len)
		else if (until <= 0) new SeqSlice(seq, 0, 0)
		else if (until <= from) new SeqSlice(seq, from, from)
		else if (from <= 0 && until >= len) new SeqSlice(seq, 0, len)
		else if (from <= 0) new SeqSlice(seq, 0, until)
		else if (until >= len) new SeqSlice(seq, from, len - from)
		else new SeqSlice(seq, from, until - from)
	}

	override def from[A](source :IterableOnce[A]) :SeqSlice[A] = source match {
		case it :Iterable[_] if it.isEmpty => Empty
		case it :SeqSlice[A] => it
		case it :IndexedSeq[A] => new SeqSlice(it)
		case it :Iterator[_] if it.isEmpty => Empty
		case it => (newBuilder[A] ++= it).result()
	}

	override def newBuilder[A] :Builder[A, SeqSlice[A]] = PassedArray.newBuilder mapResult (new SeqSlice(_))

	override def empty[A] :SeqSlice[A] = Empty

	private[this] val Empty = new SeqSlice(PassedArray.empty[Nothing])
}