package net.noresttherein.sugar.collections

import scala.collection.immutable.{AbstractSeq, LinearSeq}

import net.noresttherein.sugar.collections.extensions.{IteratorExtension, IteratorObjectExtension, immutableSeqFactoryExtension}




private[noresttherein] class PrependedSeq[+E](override val head :E, override val tail :Seq[E])
	extends AbstractSeq[E]
{
	override def length :Int = tail.length + 1
	override def knownSize :Int = {
		val size = tail.knownSize
		if (size >= 0) size + 1 else -1
	}

	override def apply(i :Int) :E =
		if (i < 0)
			throw new IndexOutOfBoundsException(i)
		else if (i == 0) head
		else tail(i - 1)

	override def iterator :Iterator[E] = head +: tail.iterator
}


private[noresttherein] class PrependedIndexedSeq[+E](override val head :E, override val tail :IndexedSeq[E])
	extends PrependedSeq(head, tail) with IndexedSeq[E]
{
	override def iterator :Iterator[E] = head +: tail.iterator
}


private[noresttherein] object PrependedSeq {
	def apply[E](head :E, tail :Seq[E]) :Seq[E] = tail match {
		case list :LinearSeq[E] => head +: list
		case _ if tail.isEmpty => IndexedSeq.one(head)
		case seq  :IndexedSeq[E] => new PrependedIndexedSeq[E](head, seq)
		case _ => new PrependedSeq[E](head, tail)
	}
	def apply[E](head :E, tail :IndexedSeq[E]) :IndexedSeq[E] = new PrependedIndexedSeq[E](head, tail)
}




private[noresttherein] class Prepended2Seq[+E](override val head :E, second :E, rest :Seq[E])
	extends AbstractSeq[E]
{
	override def length :Int = tail.length + 2
	override def knownSize :Int = {
		val size = tail.knownSize
		if (size >= 0) size + 2 else -1
	}

	override def apply(i :Int) :E = i match {
		case 0 => head
		case 1 => second
		case _ if i < 0 => throw new IndexOutOfBoundsException(i)
		case _ => rest(i - 2)
	}

	override def iterator :Iterator[E] = Iterator.double(head, second) ++: tail.iterator
}


private[noresttherein] class Prepended2IndexedSeq[+E](override val head :E, second :E, rest :IndexedSeq[E])
	extends Prepended2Seq(head, second, rest) with IndexedSeq[E]
{
	override def iterator :Iterator[E] = Iterator.double(head, second) ++: tail.iterator
}


private[noresttherein] object Prepended2Seq {
	def apply[E](first :E, second :E, rest :Seq[E]) :Seq[E] = rest match {
		case list :LinearSeq[E] => first +: second +: list
		case _ if rest.isEmpty => PassedArray.two(first, second)
		case seq  :IndexedSeq[E] => new Prepended2IndexedSeq[E](first, second, seq)
		case _ => new Prepended2Seq[E](first, second, rest)
	}
	def apply[E](first :E, second :E, rest :IndexedSeq[E]) :IndexedSeq[E] =
		new Prepended2IndexedSeq[E](first, second, rest)
}

