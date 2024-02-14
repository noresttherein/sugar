package net.noresttherein.sugar.collections

import scala.collection.{AbstractIterator, BufferedIterator, mutable}

import net.noresttherein.sugar.exceptions.{noSuch_!, unsupported_!}
import net.noresttherein.sugar.funny.generic



/**
  * @author Marcin Mościcki
  */
trait Mutator[E] extends BufferedIterator[E] {
	def head_=(value :E) :Unit
	def remove() :Unit = unsupported_!(toString + ".remove()")
	def insert(value :E) :Unit = unsupported_!(toString + ".insert()")
}




/** An iterator advancing over a slice of an `IndexedSeq`.
  * @param first    the index in the sequence of the first/next element to return.
  * @param `last++` the index in the sequence delimiting the iterator, that is pointing after the last element
  *                 the iterator should return.
  */
private sealed class IndexedSeqMutator[@specialized(Int, Long, Double, AnyRef) T] private[collections]
	                                  (seq :mutable.IndexedSeqOps[T, generic.Any1, _ <: AnyRef],
	                                   private[this] var first :Int, private[this] var `last++` :Int)
	extends AbstractIterator[T] with IndexedIterator[T] with Mutator[T]
{
	def this(seq :mutable.IndexedSeqOps[T, generic.Any1, _ <: AnyRef], idx :Int) = this(seq, idx, seq.length)
	def this(seq :mutable.IndexedSeqOps[T, generic.Any1, _ <: AnyRef]) = this(seq, 0, seq.length)

	private def underlying = seq
	protected final override def underlyingSize :Int = seq.length
	protected final override def index :Int = first
	protected final override def index_=(i :Int) :Unit = first = i
	protected final override def limit :Int = `last++`
	protected final override def limit_=(i :Int) :Unit = `last++` = i

	final override def hasNext :Boolean = first < `last++`
	override def head :T = seq(first)
	override def head_=(value :T) :Unit =
		if (first >= `last++`)
			unsupported_!("Index " + first + " exceeds the upper bound of " + `last++` + ".")
		else
			seq(first) = value

	override def next() :T = {
		if (first >= `last++`)
			noSuch_!("Index " + first + " exceeds the upper bound of " + `last++` + ".")
		val res = seq(first)
		first += 1
		res
	}


	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :IndexedSeqMutator[_] =>
			(seq eq other.underlying) && index == other.index && limit == other.limit
		case _ => false
	}
	override def hashCode :Int = seq.slice(index, limit).hashCode
	override def clone = new IndexedSeqMutator(seq, first, `last++`)
}


@SerialVersionUID(Ver)
private object IndexedSeqMutator extends IndexedIteratorFactory[mutable.IndexedSeq, IndexedSeqMutator] {
	protected override def make[T](seq :mutable.IndexedSeq[T], from :Int, until :Int) :IndexedSeqMutator[T] =
		new IndexedSeqMutator(seq, from, until)
}

