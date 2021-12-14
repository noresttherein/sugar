package net.noresttherein.slang.collection

import scala.collection.BufferedIterator




/** Base trait for implementations of iterators over slices of some sequential collections.
  * The iterator advances over a window on the collection; it is assumed to use random indexing
  * to return the elements, but they are never handled by this class itself.
  * Provides fast implementations for `size`, `take`, `drop` and some other methods.
  * @see [[net.noresttherein.slang.collection.AbstractIndexedSeqReverseIterator]]
  */
trait AbstractIndexedSeqIterator[+T] extends BufferedIterator[T] with Cloneable {
	protected def underlyingSize :Int
	protected var index :Int
	protected var limit :Int


	if (limit > underlyingSize)
		limit = underlyingSize
	if (index > limit)
		index = limit
	else if (index < 0)
		index = 0

	override def knownSize :Int = limit - index max 0
	override def size :Int = limit - index max 0

	override def hasNext :Boolean = index < limit

	override def take(n :Int) :Iterator[T] = {
		if (n <= 0) limit = index
		else if (n < size) limit = index + n
		this
	}
	override def drop(n :Int) :Iterator[T] = {
		if (n >= size) index = limit
		else if (n > 0) index += n
		this
	}

	override def splitAt(n :Int) :(Iterator[T], Iterator[T]) = (clone.take(n), drop(n))

	override def slice(from :Int, until :Int) :Iterator[T] = {
		if (until < size)
			limit = index + until
		if (from > size)
			index = limit
		else if (from > 0)
			index += from
		this
	}

	override def clone :AbstractIndexedSeqIterator[T] = super.clone.asInstanceOf[AbstractIndexedSeqIterator[T]]

	override def toString :String = clone.mkString("Iterator(", ",", ")")

}




/** Base trait for implementations of iterators traversing in the reverse order over slices
  * of some sequential collections. The iterator advances over a window on the collection; it is assumed to use
  * random indexing to return the elements, but they are never handled by this class itself.
  * Provides fast implementations for `size`, `take`, `drop` and some other methods.
  * @see [[net.noresttherein.slang.collection.AbstractIndexedSeqReverseIterator]]
  */
trait AbstractIndexedSeqReverseIterator[+T] extends BufferedIterator[T] with Cloneable {
	protected def underlyingSize :Int
	protected var index :Int
	protected var limit :Int


	if (index >= underlyingSize)
		index = underlyingSize
	if (limit > index)
		limit = index
	else if (limit < 0)
		limit = 0

	override def knownSize :Int = index - limit max 0
	override def size :Int = index - limit max 0

	override def hasNext :Boolean = index > limit

	override def take(n :Int) :Iterator[T] = {
		if (n <= 0) limit = index
		else if (n < size) limit += size - n
		this
	}
	override def drop(n :Int) :Iterator[T] = {
		if (n >= size) index = limit
		else if (n > 0) index -= n
		this
	}

	override def splitAt(n :Int) :(Iterator[T], Iterator[T]) = (clone.take(n), drop(n))

	override def slice(from :Int, until :Int) :Iterator[T] = {
		if (until <= size)
			limit = index - until
		if (from > size)
			index = limit
		else if (from > 0)
			index -= from
		this
	}


	override def clone :AbstractIndexedSeqReverseIterator[T] =
		super.clone.asInstanceOf[AbstractIndexedSeqReverseIterator[T]]

	override def toString :String = clone.mkString("Iterator(", ",", ")")

}




/** An iterator advancing over a slice of an array.
  * @param first    the index in the array of the first/next element to return.
  * @param `last++` the index in the array delimiting the iterator, that is pointing after the last element
  *                 the iterator should return.
  * @author Marcin Mościcki
  */
class ArrayIterator[@specialized(Int, Long, Double) +T]
                   (array :Array[T], private[this] var first :Int, private[this] var `last++` :Int)
	extends AbstractIndexedSeqIterator[T]
{
	def this(array :Array[T], idx :Int) = this(array, idx, array.length)
	def this(array :Array[T]) = this(array, 0, array.length)

	protected final override def underlyingSize :Int = array.length
	protected final override def index :Int = first
	protected final override def index_=(i :Int) :Unit = first = i
	protected final override def limit :Int = `last++`
	protected final override def limit_=(i :Int) :Unit = `last++` = i

	override def hasNext :Boolean = first < `last++`
	override def head :T = array(first)
	override def next() :T = { val res = array(first); first += 1; res }

	override def clone = new ArrayIterator(array, first, `last++`)
}




/** An iterator advancing over a slice of an array in the reverse direction.
  * @param last      the index in the array `last <= `first++`` of the last element to return
  *                  (the first index of the slice).
  * @param `first++` the index in the array pointing directly after the first/next element to return
  *                  (the end index of the slice).
  * @author Marcin Mościcki
  */
class ReverseArrayIterator[@specialized(Int, Long, Double) +T]
                          (array :Array[T], private[this] var last :Int, private[this] var `first++` :Int)
	extends AbstractIndexedSeqReverseIterator[T]
{
	def this(array :Array[T], idx :Int) = this(array, 0, idx)
	def this(array :Array[T]) = this(array, 0, array.length)

	protected final override def underlyingSize :Int = array.length
	protected final override def index :Int = `first++`
	protected final override def index_=(i :Int) :Unit = `first++` = i
	protected final override def limit :Int = last
	protected final override def limit_=(i :Int) :Unit = last = i

	override def hasNext :Boolean = `first++` > last
	override def head :T = array(`first++` - 1)
	override def next() :T = { `first++` -= 1; array(`first++`) }

	override def clone = new ReverseArrayIterator(array, last, `first++`)
}

