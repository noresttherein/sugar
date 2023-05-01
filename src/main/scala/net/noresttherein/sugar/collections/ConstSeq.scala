package net.noresttherein.sugar.collections


import scala.annotation.tailrec
import scala.collection.AbstractSeq

import net.noresttherein.sugar.collections.extensions.iteratorObjectExtension






/** A simple wrapper over a single object exposing it as a predefined number of repetitions within a [[Seq]]. */
@SerialVersionUID(Ver)
class ConstSeq[T] private (elem :T, override val knownSize :Int)
	extends AbstractSeq[T] with IndexedSeq[T] with Serializable
{
	override def head :T =
		if (knownSize >= 1) elem
		else throw new NoSuchElementException("Seq().head")

	override def last :T =
		if (knownSize >= 1) elem
		else throw new NoSuchElementException("Seq().last")

	override def tail :ConstSeq[T] = knownSize match {
		case 0 => throw new UnsupportedOperationException("Seq().tail")
		case infinite if infinite < 0 => this
		case n => new ConstSeq(elem, n)
	}

	override def slice(from :Int, until :Int) :IndexedSeq[T] =
		if (from < 0 & until >= knownSize) this
		else if (until <= from | until < 0) new ConstSeq(elem, 0)
		     else new ConstSeq(elem, until - (from max 0))

	override def take(n :Int) :IndexedSeq[T] =
		if (n < 0) new ConstSeq(elem, 0)
		else if (n < knownSize | knownSize < 0) new ConstSeq(elem, n)
		     else this

	override def drop(n :Int) :IndexedSeq[T] =
		if (n < 0 | knownSize < 0) this
		else if (n < knownSize) new ConstSeq(elem, knownSize - n)
		     else new ConstSeq(elem, 0)

	override def iterator :Iterator[T] = Iterator.const(knownSize, elem)

	override def apply(i :Int) :T =
		if (i < 0 | i >= knownSize)
			throw new IndexOutOfBoundsException(i)
		else elem

	@tailrec final override def length :Int =
		if (knownSize >= 0) knownSize
		else length

}



@SerialVersionUID(Ver)
object ConstSeq {
	def apply[T](elem :T, size :Int) :ConstSeq[T] =
		if (size < 0) new ConstSeq(elem, -1)
		else new ConstSeq(elem, size)

	def infinite[T](elem :T) :ConstSeq[T] = new ConstSeq(elem, -1)

	override def toString = "ConstSeq"
}
