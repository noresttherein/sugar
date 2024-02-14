package net.noresttherein.sugar.collections


import scala.collection.AbstractSeq
import scala.collection.immutable.{LinearSeq, SeqOps, StrictOptimizedSeqOps}

import net.noresttherein.sugar.arrays.MutableArrayExtension
import net.noresttherein.sugar.collections.extensions.IteratorCompanionExtension
import net.noresttherein.sugar.exceptions.{illegal_!, noSuch_!, outOfBounds_!, unsupported_!}






private sealed trait ConstSeqOps[+E, CC[+_], +C] extends StrictOptimizedSeqOps[E, CC, C] { this :C =>

	final override def length :Int =
		if (knownSize >= 0) knownSize
		else unsupported_!("Seq.infinite.length")

	override def apply(i :Int) :E =
		if (i < 0 || knownSize >= 0 && i >= knownSize)
			outOfBounds_!(i)
		else head

	protected def subseq(n :Int) :C

	override def tail :C = knownSize match {
		case 0 => unsupported_!("Seq().tail")
		case infinite if infinite < 0 => this
		case n => subseq(n - 1)
	}
	override def init :C = knownSize match {
		case 0 => unsupported_!("Seq().init")
		case infinite if infinite < 0 => unsupported_!("Seq.infinite.init")
		case n => subseq(n - 1)
	}

	override def last :E =
		if (knownSize == 0) noSuch_!("Seq().last")
		else if (knownSize <= 0) unsupported_!("Seq.infinite.last")
		else head

	override def slice(from :Int, until :Int) :C = {
		val knownSize = this.knownSize
		if (until <= from | until <= 0 | knownSize >= 0 & from >= knownSize) empty
		else if (from <= 0 && knownSize >= 0 && until >= knownSize) this
		else if (from <= 0) subseq(until)
		else if (knownSize >= 0 && until >= knownSize) subseq(knownSize - from)
		else subseq(until - from)
	}

	override def take(n :Int) :C =
		if (n <= 0) empty
		else if (n <= knownSize || knownSize < 0) subseq(n)
		else this

	override def drop(n :Int) :C =
		if (n <= 0 || knownSize < 0) this
		else if (n < knownSize) subseq(knownSize - n)
		else empty

	override def empty :C = subseq(0)

	override def iterator :Iterator[E] = knownSize match {
		case 0           => Iterator.empty
		case n if n >= 0 => Iterator.const(n)(head)
		case _           => Iterator.infinite(head)
	}
}


/** A simple wrapper over a single object exposing it as a predefined number of repetitions within a [[Seq]]. */
private abstract class AbstractConstSeq[+E] protected (elem :E, override val knownSize :Int)
	extends AbstractSeq[E] with Serializable
{
	override def isEmpty = knownSize == 0
	override def head :E =
		if (knownSize == 0) noSuch_!("Seq().head")
		else elem

	override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
		if (len < 0 || knownSize == 0 || start > xs.length)
			0
		else if (start < 0)
			outOfBounds_!(start)
		else {
			val len0 = math.min(len, xs.length - start)
			val copied = if (knownSize < 0) len0 else math.min(len0, knownSize)
			xs.fill(start, start + copied)(elem)
			copied
		}
	protected override def className :String = "ConstSeq"
}




@SerialVersionUID(Ver)
private[collections] object ConstSeq {
	def apply[T](elem :T, length :Int) :Seq[T] = ConstIndexedSeq(elem, length)
	def infinite[T](elem :T) :Seq[T] = ConstIndexedSeq.infinite(elem)
	override def toString = "ConstSeq"
}




@SerialVersionUID(Ver)
private class ConstIndexedSeq[+E] private (elem :E, override val knownSize :Int)
	extends AbstractConstSeq[E](elem, knownSize) with IndexedSeq[E] with ConstSeqOps[E, IndexedSeq, IndexedSeq[E]]
{
	protected override def subseq(n :Int) =
		if (n == 0) IndexedSeq.empty else new ConstIndexedSeq(head, n)

	override def head = super[AbstractConstSeq].head

	override def map[B](f :E => B) :IndexedSeq[B] = new ConstIndexedSeq(f(elem), knownSize) //because we may be infinite

	protected override def className = "ConstIndexedSeq"
}

@SerialVersionUID(Ver)
private[collections] object ConstIndexedSeq {
	def apply[E](elem :E, length :Int) :IndexedSeq[E] =
		if (length < 0)
			illegal_!("negative size " + length)
		else if (length == 0)
			IndexedSeq.empty
		else
			new ConstIndexedSeq[E](elem, length)

	def infinite[E](elem :E) :IndexedSeq[E] = new ConstIndexedSeq[E](elem, -1)
	override def toString = "ConstIndexedSeq"
}




@SerialVersionUID(Ver)
private class ConstLinearSeq[+E] private (elem :E, override val knownSize :Int)
	extends AbstractConstSeq[E](elem, knownSize) with LinearSeq[E] with ConstSeqOps[E, LinearSeq, LinearSeq[E]]
{
	protected override def subseq(n :Int) =
		if (n == 0) LinearSeq.empty else new ConstLinearSeq(head, n)

	override def map[B](f :E => B) :LinearSeq[B] = new ConstLinearSeq(f(elem), knownSize) //because we may be infinite

	protected override def className = "ConstLinearSeq"
}

@SerialVersionUID(Ver)
private[collections] object ConstLinearSeq {
	def apply[E](elem :E, length :Int) :LinearSeq[E] =
		if (length < 0)
			illegal_!("negative size " + length)
		else if (length == 0)
			Nil
		else
			new ConstLinearSeq[E](elem, length)

	def infinite[E](elem :E) :LinearSeq[E] = new ConstLinearSeq[E](elem, -1)
	override def toString = "ConstIndexedSeq"
}


