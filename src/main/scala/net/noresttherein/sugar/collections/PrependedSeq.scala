package net.noresttherein.sugar.collections

import scala.collection.immutable.{AbstractSeq, LinearSeq, SeqOps}

import net.noresttherein.sugar.collections.extensions.{IteratorExtension, IteratorObjectExtension, SeqFactoryExtension}




private[noresttherein] class PrependedSeq[+E](override val head :E, override val tail :Seq[E])
	extends AbstractSeq[E]
{
	override def length :Int = tail.length + 1
	override def knownSize :Int = {
		val size = tail.knownSize
		if (size >= 0) size + 1 else -1
	}
	override def drop(n :Int) :Seq[E] = slice(n, Int.MaxValue)
	override def take(n :Int) :Seq[E] = slice(0, n)
	override def slice(from :Int, until :Int) :Seq[E] =
		if (until <= 0 || until <= from) empty
		else if (from <= 0) new PrependedSeq(head, tail.drop(until - 1))
		else tail.slice(from - 1, until - 1)

	override def apply(i :Int) :E =
		if (i < 0)
			throw new IndexOutOfBoundsException(i)
		else if (i == 0) head
		else tail(i - 1)

	override def iterator :Iterator[E] = head +: tail.iterator
	protected override def className :String = "Seq"
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


private[noresttherein] class PrependedIndexedSeq[+E](override val head :E, override val tail :IndexedSeq[E])
	extends PrependedSeq(head, tail) with IndexedSeq[E]
{
	override def drop(n :Int) :IndexedSeq[E] = slice(n, Int.MaxValue)
	override def take(n :Int) :IndexedSeq[E] = slice(0, n)
	override def slice(from :Int, until :Int) :IndexedSeq[E] =
		if (until <= 0 || until <= from) empty
		else if (from <= 0) new PrependedIndexedSeq(head, tail.drop(until - 1))
		else tail.slice(from - 1, until - 1)

	override def iterator :Iterator[E] = head +: tail.iterator
	protected override def className :String = "IndexedSeq"
}




private[noresttherein] class Prepended2Seq[+E](override val head :E, second :E, rest :Seq[E])
	extends AbstractSeq[E]
{
	override def length :Int = rest.length + 2
	override def knownSize :Int = {
		val size = rest.knownSize
		if (size >= 0) size + 2 else -1
	}

	override def tail :Seq[E] = new PrependedSeq(second, rest)
	override def drop(n :Int) :Seq[E] = slice(n, Int.MaxValue)
	override def take(n :Int) :Seq[E] = slice(0, n)
	override def slice(from :Int, until :Int) :Seq[E] =
		if (until <= 0 | until <= from)
			empty
		else if (from <= 0)
			if (until == 1) Seq.one(head)
			else new Prepended2Seq(head, second, rest.take(until - 2))
		else if (from == 1)
			new PrependedSeq(second, rest.take(until - 2))
		else
			rest.slice(from - 2, until - 2)

	override def apply(i :Int) :E = i match {
		case 0 => head
		case 1 => second
		case _ if i < 0 => throw new IndexOutOfBoundsException(i)
		case _ => rest(i - 2)
	}

	override def iterator :Iterator[E] = Iterator.double(head, second) ++: rest.iterator
	
	protected override def className = "Seq"
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


private[noresttherein] class Prepended2IndexedSeq[+E](override val head :E, second :E, rest :IndexedSeq[E])
	extends Prepended2Seq(head, second, rest) with IndexedSeq[E]
{
	override def iterator :Iterator[E] = super[Prepended2Seq].iterator

	override def tail :IndexedSeq[E] = new PrependedIndexedSeq(second, rest)
	override def drop(n :Int) :IndexedSeq[E] = slice(n, Int.MaxValue)
	override def take(n :Int) :IndexedSeq[E] = slice(0, n)
	override def slice(from :Int, until :Int) :IndexedSeq[E] =
		if (until <= 0 | until <= from)
			empty
		else if (from <= 0)
			if (until == 1) IndexedSeq.one(head)
			else new Prepended2IndexedSeq(head, second, rest.take(until - 2))
		else if (from == 1)
			new PrependedIndexedSeq(second, rest.take(until - 2))
		else
			rest.slice(from - 2, until - 2)

	protected override def className = "IndexedSeq"
}

