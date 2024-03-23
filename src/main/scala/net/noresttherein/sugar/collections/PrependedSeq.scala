package net.noresttherein.sugar.collections

import scala.collection.IterableFactory
import scala.collection.immutable.{AbstractSeq, IndexedSeqOps, LinearSeq, SeqOps, StrictOptimizedSeqOps}
import scala.collection.mutable.Builder

import net.noresttherein.sugar.collections.extensions.{IteratorCompanionExtension, IteratorExtension, SeqFactoryExtension}
import net.noresttherein.sugar.funny.generic.Any1
import net.noresttherein.sugar.outOfBounds_!




object PrependedSeq {
	def apply[E](head :E, tail :Seq[E]) :Seq[E] = tail match {
		case list :LinearSeq[E]                => head +: list
		case _ if tail.isEmpty                 => IndexedSeq.one(head)
		case seq  :IndexedSeq[E]               => apply(head, seq)
		case _ :StrictOptimizedSeqOps[_, _, _] =>
			new PrependedSeq[E](head, tail) with StrictOptimizedSeqOps[E, Seq, Seq[E]]
		case _ =>
			new PrependedSeq[E](head, tail)
	}
	def apply[E](head :E, tail :IndexedSeq[E]) :IndexedSeq[E] =
		if (tail.isInstanceOf[StrictOptimizedSeqOps[_, Any1, _]])
			new PrependedIndexedSeq[E](head, tail) with StrictOptimizedSeqOps[E, IndexedSeq, IndexedSeq[E]]
		else
			new PrependedIndexedSeq(head, tail)
}


/** Currently used only as a `tail` for [[net.noresttherein.sugar.collections.Prepended2Seq Prepended2Seq]]. */
private class PrependedSeq[+E](override val head :E, override val tail :Seq[E])
	extends AbstractSeq[E] with Serializable
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
		else if (from <= 0) PrependedSeq(head, tail.drop(until - 1))
		else tail.slice(from - 1, until - 1)

	override def apply(i :Int) :E =
		if (i < 0)
			outOfBounds_!(i, length)
		else if (i == 0) head
		else tail(i - 1)

	override def iterator :Iterator[E] = head +: tail.iterator
	protected override def className :String = "Seq"
}


private class PrependedIndexedSeq[+E](override val head :E, override val tail :IndexedSeq[E])
	extends PrependedSeq(head, tail) with IndexedSeq[E]
{
	override def drop(n :Int) :IndexedSeq[E] = slice(n, Int.MaxValue)
	override def take(n :Int) :IndexedSeq[E] = slice(0, n)
	override def slice(from :Int, until :Int) :IndexedSeq[E] =
		if (until <= 0 || until <= from) empty
		else if (from <= 0) PrependedSeq(head, tail.drop(until - 1))
		else tail.slice(from - 1, until - 1)

	override def iterator :Iterator[E] = head +: tail.iterator
	protected override def className :String = "IndexedSeq"
}




object Prepended2Seq {
	def apply[E](first :E, second :E, rest :Seq[E]) :Seq[E] = rest match {
		case list :LinearSeq[E]                => first +: second +: list
		case _ if rest.isEmpty                 => RelayArray.two(first, second)
		case seq  :IndexedSeq[E]               => apply(first, second, seq)
		case _ :StrictOptimizedSeqOps[_, Any1, _] =>
			new Prepended2Seq[E](first, second, rest) with StrictOptimizedSeqOps[E, Seq, Seq[E]]
		case _ =>
			new Prepended2Seq[E](first, second, rest)
	}
	def apply[E](first :E, second :E, rest :IndexedSeq[E]) :IndexedSeq[E] =
		if (rest.isInstanceOf[StrictOptimizedSeqOps[_, Any1, _]])
			new Prepended2IndexedSeq[E](first, second, rest) with StrictOptimizedSeqOps[E, IndexedSeq, IndexedSeq[E]]
		else
			new Prepended2IndexedSeq[E](first, second, rest)
}


/** A convenient way of delegating a method taking `(T, T, T*)` to one taking `(Seq[T])`
  * without worrying about the cost of concatenation.
  */
private class Prepended2Seq[+E](override val head :E, second :E, rest :Seq[E])
	extends AbstractSeq[E] with Serializable
{
	override def length :Int = rest.length + 2
	override def knownSize :Int = {
		val size = rest.knownSize
		if (size >= 0) size + 2 else -1
	}

	override def tail :Seq[E] = PrependedSeq(second, rest)
	override def drop(n :Int) :Seq[E] = slice(n, Int.MaxValue)
	override def take(n :Int) :Seq[E] = slice(0, n)
	override def slice(from :Int, until :Int) :Seq[E] =
		if (until <= 0 | until <= from)
			empty
		else if (from <= 0)
			if (until == 1) Seq.one(head)
			else Prepended2Seq(head, second, rest.take(until - 2))
		else if (from == 1)
			PrependedSeq(second, rest.take(until - 2))
		else
			rest.slice(from - 2, until - 2)

	override def apply(i :Int) :E = i match {
		case 0 => head
		case 1 => second
		case _ if i < 0 => outOfBounds_!(i, length)
		case _ => rest(i - 2)
	}

	override def iterator :Iterator[E] = Iterator.two(head, second) ++: rest.iterator
	
	protected override def className = "Seq"
}


private class Prepended2IndexedSeq[+E](override val head :E, second :E, rest :IndexedSeq[E])
	extends Prepended2Seq(head, second, rest) with IndexedSeq[E]
{
	override def iterator :Iterator[E] = super[Prepended2Seq].iterator

	override def tail :IndexedSeq[E] = PrependedSeq(second, rest)
	override def drop(n :Int) :IndexedSeq[E] = slice(n, Int.MaxValue)
	override def take(n :Int) :IndexedSeq[E] = slice(0, n)
	override def slice(from :Int, until :Int) :IndexedSeq[E] =
		if (until <= 0 | until <= from)
			empty
		else if (from <= 0)
			if (until == 1) IndexedSeq.one(head)
			else Prepended2Seq(head, second, rest.take(until - 2))
		else if (from == 1)
			PrependedSeq(second, rest.take(until - 2))
		else
			rest.slice(from - 2, until - 2)

	protected override def className = "IndexedSeq"
}






//for lack of a better place to put it
/** A non-sticky adapter of a `String` to `IterableOnce[E]` and `IndexedSeqOps[E, IndexedSeq, String]`.
  * All operations return a `String`, or a generic `IndexedSeq` if element type changes, not another instance
  * of this class. What makes it different from standard extension methods in [[scala.collection.StringOps StringOps]]
  * is that the latter is not an `IterableOnce`. On the other hand, explicitly created
  * [[scala.collection.immutable.WrappedString WrappedString]] return the same sequence type when filtering or mapping.
  * It's useful for enabling the use of strings as parameters to any method/class requiring an `IterableOps[E, CC, C]`,
  * so that the result(s) are also `String`s.
  */ //todo: add an implicit conversion somewhere
@SerialVersionUID(Ver)
private final class StringAsSeq(override val coll :String)
	extends IterableOnce[Char] with IndexedSeqOps[Char, IndexedSeq, String]
	   with StrictOptimizedSeqOps[Char, IndexedSeq, String] with Serializable
{
	def length :Int = coll.length

	override def apply(i :Int) :Char = coll.charAt(i)

	override def iterator :Iterator[Char] = new StringIterator(coll, 0, coll.length)

	override def empty :String = ""

	protected override def fromSpecific(coll :IterableOnce[Char]) :String = coll match {
		case empty :Iterable[Char] if empty.isEmpty => ""
		case _ => (new StringBuilder ++= coll).result()
	}
	protected override def newSpecificBuilder :Builder[Char, String] = new StringBuilder
	override def iterableFactory :IterableFactory[IndexedSeq] = IndexedSeq

	override def toIterable :Iterable[Char] = coll

	override def equals(that :Any) :Boolean = that match {
		case it :StringAsSeq => it.coll == coll
		case _ => false
	}
	override def hashCode :Int = coll.hashCode
	override def toString :String = coll //"StringAsSeq(" + coll + ")"
}
