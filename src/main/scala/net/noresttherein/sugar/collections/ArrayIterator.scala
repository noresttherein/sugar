package net.noresttherein.sugar.collections

import scala.collection.BufferedIterator




/** Base trait for implementations of iterators over slices of some sequential collections.
  * The iterator advances over a window on the collection; it is assumed to use random indexing
  * to return the elements, but they are never handled by this class itself.
  * Provides fast implementations for `size`, `take`, `drop` and some other methods.
  * @see [[net.noresttherein.sugar.collections.AbstractIndexedReverseIterator]]
  * @author Marcin Mościcki
  */
trait AbstractIndexedIterator[+T] extends BufferedIterator[T] with Cloneable {
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

	override def clone :AbstractIndexedIterator[T] = super.clone.asInstanceOf[AbstractIndexedIterator[T]]

	override def toString :String = clone.mkString("Iterator(", ",", ")")

}




/** Base trait for implementations of iterators traversing in the reverse order over slices
  * of some sequential collections. The iterator advances over a window on the collection; it is assumed to use
  * random indexing to return the elements, but they are never handled by this class itself.
  * Provides fast implementations for `size`, `take`, `drop` and some other methods.
  * @see [[net.noresttherein.sugar.collections.AbstractIndexedIterator]]
  * @author Marcin Mościcki
  */
trait AbstractIndexedReverseIterator[+T] extends BufferedIterator[T] with Cloneable {
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


	override def clone :AbstractIndexedReverseIterator[T] =
		super.clone.asInstanceOf[AbstractIndexedReverseIterator[T]]

	override def toString :String = clone.mkString("Iterator(", ",", ")")

}




/** An iterator advancing over a slice of an array.
  * @param first    the index in the array of the first/next element to return.
  * @param `last++` the index in the array delimiting the iterator, that is pointing after the last element
  *                 the iterator should return.
  */
class ArrayIterator[@specialized(Int, Long, Double) +T]
	               (array :Array[T], private[this] var first :Int, private[this] var `last++` :Int)
	extends AbstractIndexedIterator[T]
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


@SerialVersionUID(ver)
object ArrayIterator {
	def apply[@specialized(Int, Long, Double) T](array :Array[T]) :ArrayIterator[T] =
		new ArrayIterator(array, 0, array.length)

	def apply[@specialized(Int, Long, Double) T](array :Array[T], offset :Int) :ArrayIterator[T] =
		if (offset >= array.length) new ArrayIterator(array, array.length, array.length)
		else if (offset <= 0) new ArrayIterator(array, 0, array.length)
		else new ArrayIterator(array, offset, array.length)

	def apply[@specialized(Int, Long, Double) T](array :Array[T], from :Int, until :Int) :ArrayIterator[T] = {
		val len = array.length
		if (from >= len) new ArrayIterator(array, len, len)
		else if (until <= 0) new ArrayIterator(array, 0, 0)
		else if (until <= from) new ArrayIterator(array, from, from)
		else if (from <= 0 && until >= len) new ArrayIterator(array, 0, len)
		else if (from <= 0) new ArrayIterator(array, 0, until)
		else if (until >= len) new ArrayIterator(array, from, len)
		else new ArrayIterator(array, from, until)
	}
}




/** An iterator advancing over a slice of an array in the reverse direction.
  * @param last      the index in the array `last <= `first++`` of the last element to return
  *                  (the first index of the slice).
  * @param `first++` the index in the array pointing directly after the first/next element to return
  *                  (the end index of the slice).
  */
class ReverseArrayIterator[@specialized(Int, Long, Double) +T]
	                      (array :Array[T], private[this] var last :Int, private[this] var `first++` :Int)
	extends AbstractIndexedReverseIterator[T]
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


@SerialVersionUID(ver)
object ReverseArrayIterator {
	def apply[@specialized(Int, Long, Double) T](array :Array[T]) :ReverseArrayIterator[T] =
		new ReverseArrayIterator(array, 0, array.length)

	def apply[@specialized(Int, Long, Double) T](array :Array[T], from :Int, downto :Int) :ReverseArrayIterator[T] = {
		val len = array.length
		if (from <= 0) new ReverseArrayIterator(array, 0, 0)
		else if (downto >= len) new ReverseArrayIterator(array, len, len)
		else if (downto >= from) new ReverseArrayIterator(array, from, from)
		else if (downto <= 0 && from >= len) new ReverseArrayIterator(array, 0, len)
		else if (downto <= 0) new ReverseArrayIterator(array, 0, from)
		else if (from >= len) new ReverseArrayIterator(array, downto, len)
		else new ReverseArrayIterator(array, downto, from)
	}
}






/** An iterator advancing over a slice of an `IndexedSeq`.
  * @param first    the index in the sequence of the first/next element to return.
  * @param `last++` the index in the sequence delimiting the iterator, that is pointing after the last element
  *                 the iterator should return.
  */
class IndexedSeqIterator[@specialized(Int, Long, Double) +T]
	                    (seq :collection.IndexedSeq[T], private[this] var first :Int, private[this] var `last++` :Int)
	extends AbstractIndexedIterator[T]
{
	def this(seq :IndexedSeq[T], idx :Int) = this(seq, idx, seq.length)
	def this(seq :IndexedSeq[T]) = this(seq, 0, seq.length)

	protected final override def underlyingSize :Int = seq.length
	protected final override def index :Int = first
	protected final override def index_=(i :Int) :Unit = first = i
	protected final override def limit :Int = `last++`
	protected final override def limit_=(i :Int) :Unit = `last++` = i

	override def hasNext :Boolean = first < `last++`
	override def head :T = seq(first)
	override def next() :T = { val res = seq(first); first += 1; res }

	override def clone = new IndexedSeqIterator(seq, first, `last++`)
}


@SerialVersionUID(ver)
object IndexedSeqIterator {
	def apply[@specialized(Int, Long, Double) T](seq :collection.IndexedSeq[T]) :IndexedSeqIterator[T] =
		new IndexedSeqIterator(seq, 0, seq.length)

	def apply[@specialized(Int, Long, Double) T](seq :collection.IndexedSeq[T], offset :Int) :IndexedSeqIterator[T] =
		if (offset >= seq.length) new IndexedSeqIterator(seq, seq.length, seq.length)
		else if (offset <= 0) new IndexedSeqIterator(seq, 0, seq.length)
		else new IndexedSeqIterator(seq, offset, seq.length)

	def apply[@specialized(Int, Long, Double) T]
	         (seq :collection.IndexedSeq[T], from :Int, until :Int) :IndexedSeqIterator[T] =
	{
		val len = seq.length
		if (from >= len) new IndexedSeqIterator(seq, len, len)
		else if (until <= 0) new IndexedSeqIterator(seq, 0, 0)
		else if (until <= from) new IndexedSeqIterator(seq, from, from)
		else if (from <= 0 && until >= len) new IndexedSeqIterator(seq, 0, len)
		else if (from <= 0) new IndexedSeqIterator(seq, 0, until)
		else if (until >= len) new IndexedSeqIterator(seq, from, len)
		else new IndexedSeqIterator(seq, from, until)
	}
}




/** An iterator advancing over a slice of an `IndexedSeq` in the reverse direction.
  * @param last      the index in the sequence `last <= `first++`` of the last element to return
  *                  (the first index of the slice).
  * @param `first++` the index in the sequence pointing directly after the first/next element to return
  *                  (the end index of the slice).
  */
class ReverseIndexedSeqIterator[@specialized(Int, Long, Double) +T]
	  (seq :collection.IndexedSeq[T], private[this] var last :Int, private[this] var `first++` :Int)
	extends AbstractIndexedReverseIterator[T]
{
	def this(seq :IndexedSeq[T], idx :Int) = this(seq, 0, idx)
	def this(seq :IndexedSeq[T]) = this(seq, 0, seq.length)

	protected final override def underlyingSize :Int = seq.length
	protected final override def index :Int = `first++`
	protected final override def index_=(i :Int) :Unit = `first++` = i
	protected final override def limit :Int = last
	protected final override def limit_=(i :Int) :Unit = last = i

	override def hasNext :Boolean = `first++` > last
	override def head :T = seq(`first++` - 1)
	override def next() :T = { `first++` -= 1; seq(`first++`) }

	override def clone = new ReverseIndexedSeqIterator(seq, last, `first++`)
}


@SerialVersionUID(ver)
object ReverseIndexedSeqIterator {
	def apply[@specialized(Int, Long, Double) T](seq :collection.IndexedSeq[T]) :ReverseIndexedSeqIterator[T] =
		new ReverseIndexedSeqIterator(seq, 0, seq.length)

	def apply[@specialized(Int, Long, Double) T]
	         (seq :collection.IndexedSeq[T], from :Int, downto :Int) :ReverseIndexedSeqIterator[T] =
	{
		val len = seq.length
		if (from <= 0) new ReverseIndexedSeqIterator(seq, 0, 0)
		else if (downto >= len) new ReverseIndexedSeqIterator(seq, len, len)
		else if (downto >= from) new ReverseIndexedSeqIterator(seq, from, from)
		else if (downto <= 0 && from >= len) new ReverseIndexedSeqIterator(seq, 0, len)
		else if (downto <= 0) new ReverseIndexedSeqIterator(seq, 0, from)
		else if (from >= len) new ReverseIndexedSeqIterator(seq, downto, len)
		else new ReverseIndexedSeqIterator(seq, downto, from)
	}
}






/** An iterator over an arbitrary section of a `String`,
  * similar to [[net.noresttherein.sugar.collections.ArrayIterator ArrayIterator]].
  * Has O(1) `take`/`drop`/`slice` methods.
  */
final class StringIterator private[collections]
	        (string :String, private[this] var first :Int, private[this] var `last++` :Int)
	extends AbstractIndexedIterator[Char]
{
	protected override def underlyingSize :Int = string.length
	protected override def index :Int = first
	protected override def index_=(i :Int) :Unit = first = i
	protected override def limit :Int = `last++`
	protected override def limit_=(i :Int) :Unit = `last++` = i

	override def hasNext :Boolean = first < `last++`
	override def head :Char = string.charAt(first)

	override def next() :Char = {
		val res = string.charAt(first); first += 1; res
	}

	override def clone = new StringIterator(string, first, `last++`)
}


@SerialVersionUID(ver)
object StringIterator {
	def apply(string :String) :StringIterator = new StringIterator(string, 0, string.length)

	def apply(string :String, offset :Int) :StringIterator =
		if (offset >= string.length) empty
		else if (offset <= 0) new StringIterator(string, 0, string.length)
		else new StringIterator(string, offset, string.length)

	def apply(string :String, from :Int, until :Int) :StringIterator = {
		val len = string.length
		if (from >= len || until <= 0 || from >= until) empty
		else if (from <= 0 && until >= len) new StringIterator(string, 0, len)
		else if (from <= 0) new StringIterator(string, 0, until)
		else if (until >= len) new StringIterator(string, from, len)
		else new StringIterator(string, from, until)
	}

	def empty = new StringIterator("", 0, 0)
}




/** An iterator over an arbitrary section of a `String`, running in reverse
  * similar to [[net.noresttherein.sugar.collections.ReverseArrayIterator ReverseArrayIterator]].
  * Has O(1) `take`/`drop`/`slice` methods.
  */
final class ReverseStringIterator private[collections]
	        (string :String, private[this] var last :Int, private[this] var `first++` :Int)
	extends AbstractIndexedReverseIterator[Char]
{
	/* Requires 0 <= from <= until <= string.length and maintains invariant 0 <= stop <= index <= string.length.
	* The invariant can be broken only by advancing an empty iterator.
	* `string(until)` is the character immediately following (in string) the first character in this iterator,
	* while `string(from)` is the last character in this iterator, unless it is empty.
	* This scheme results in code being a mirror image of StringIterator, with + replaced with -.
	*/
	protected override def underlyingSize :Int = string.length
	protected override def index :Int = `first++`
	protected override def index_=(i :Int) :Unit = `first++` = i
	protected override def limit :Int = last
	protected override def limit_=(i :Int) :Unit = last = i

	override def hasNext :Boolean = `first++` > last
	override def head :Char = string.charAt(`first++` - 1)

	override def next() :Char = {
		`first++` -= 1; string.charAt(`first++`)
	}

	override def clone = new ReverseStringIterator(string, last, `first++`)
}


@SerialVersionUID(ver)
object ReverseStringIterator {
	def apply(string :String) :ReverseStringIterator = new ReverseStringIterator(string, 0, string.length)

	def apply(string :String, from :Int, downto :Int) :ReverseStringIterator = {
		val len = string.length
		if (downto >= len || from <= 0 || downto >= from) empty
		else if (downto <= 0 && from >= len) new ReverseStringIterator(string, 0, len)
		else if (downto <= 0) new ReverseStringIterator(string, 0, from)
		else if (from >= len) new ReverseStringIterator(string, downto, len)
		else new ReverseStringIterator(string, downto, from)
	}

	def empty = new ReverseStringIterator("", 0, 0)
}

