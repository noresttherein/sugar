package net.noresttherein.sugar.collections

import java.lang.{Math => math}

import scala.collection.immutable.WrappedString
import scala.collection.{AbstractIterator, BufferedIterator}

import net.noresttherein.sugar.JavaTypes.JIntIterator
import net.noresttherein.sugar.extensions.castTypeParamMethods




/** Base trait for implementations of iterators over slices of some sequential collections.
  * The iterator advances over a window on the collection; it is assumed to use random indexing
  * to return the elements, but they are never handled by this class itself.
  * Provides fast implementations for `size`, `take`, `drop` and some other methods.
  * @see [[net.noresttherein.sugar.collections.AbstractIndexedReverseIterator]]
  * @author Marcin Mościcki
  */ //consider: throwing exceptions with constant strings as messages for performance
trait AbstractIndexedIterator[+T] extends BufferedIterator[T] with Cloneable {
	protected def underlyingSize :Int
	protected var index :Int
	protected var limit :Int

	protected def adjustRange() :Unit =
		if (limit > underlyingSize)
			limit = underlyingSize
		if (index > limit)
			index = limit
		else if (index < 0)
			index = 0

	override def knownSize :Int = math.max(0, limit - index)
	override def size :Int = math.max(0, limit - index)

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

	protected def adjustRange() :Unit =
		if (index >= underlyingSize)
			index = underlyingSize
		if (limit > index)
			limit = index
		else if (limit < 0)
			limit = 0

	override def knownSize :Int = math.max(0, index - limit)
	override def size :Int = math.max(0, index - limit)

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




/** An iterator advancing over a slice of an array. The advantages over built in array
  * [[collection.ArrayOps.iterator iterator]] are fast, in-place `take`, and `copyToArray` delegating to `Array.copy`,
  * making it considerably faster than copying by one.
  * @param first    the index in the array of the first/next element to return.
  * @param `last++` the index in the array delimiting the iterator, that is pointing after the last element
  *                 the iterator should return.
  */ //no SerialVersionUID because we are not Serializable :)
sealed class ArrayIterator[@specialized(Int, Long, Double, AnyRef) +T] private[collections]
	                      (array :Array[T], private[this] var first :Int, private[this] var `last++` :Int)
	extends AbstractIterator[T] with AbstractIndexedIterator[T] with AbstractArraySlice[T]
{
//	def this(array :Array[T], idx :Int) = this(array, idx, array.length)
	def this(array :Array[T]) = this(array, 0, array.length)

	private[collections] override def unsafeArray :Array[_] = array
	private[collections] override def startIndex :Int = first
	private[collections] override def isImmutable :Boolean = false

	protected final override def underlyingSize :Int = array.length
	final override def index :Int = first
	protected final override def index_=(i :Int) :Unit = first = i
	final override def limit :Int = `last++`
	protected final override def limit_=(i :Int) :Unit = `last++` = i

	def reverse :ReverseArrayIterator[T] = new ReverseArrayIterator[T](array, first, `last++`)

	override def hasNext :Boolean = first < `last++`
	override def head :T =
		if (first < `last++`) array(first)
		else throw new NoSuchElementException("Index " + first + " exceeds the limit of " + `last++` + '.')

	override def next() :T = {
		if (first >= `last++`)
			throw new NoSuchElementException("Index " + first + " exceeds the limit of " + `last++` + ".")
		val res = array(first)
		first += 1
		res
	}

	override def copyToArray[B >: T](xs :Array[B], start :Int, len :Int) :Int = {
		if (len <= 0 || first >= `last++` || start >= xs.length)
			0
		else if (start < 0)
			throw new IndexOutOfBoundsException(start.toString + " out of [0, " + xs.length + ")")
		else {
			val copied = math.min(len, math.min(`last++` - first, xs.length - start))
			ArrayLike.copy(array, first, xs, start, copied)
			copied
		}
	}

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :ArrayIterator[_] => (array eq other.unsafeArray) && index == other.index && limit == other.limit
		case _ => false
	}
	override def hashCode :Int = ErasedArray.Wrapped.Slice(array, first, `last++`).hashCode
	override def clone = new ArrayIterator(array, first, `last++`)
}


/** A factory of iterators advancing over array slices. */
@SerialVersionUID(Ver)
object ArrayIterator {
	private def make[T](array :Array[T], from :Int, until :Int) :ArrayIterator[T] =
		((array :Array[_]) match {
			case a :Array[AnyRef]  => new ArrayIterator(a, from, until)
			case a :Array[Int]     => new ArrayIterator(a, from, until)
			case a :Array[Long]    => new ArrayIterator(a, from, until)
			case a :Array[Double]  => new ArrayIterator(a, from, until)
//			case a :Array[Byte]    => new ArrayIterator(a, from, until)
//			case a :Array[Char]    => new ArrayIterator(a, from, until)
//			case a :Array[Float]   => new ArrayIterator(a, from, until)
//			case a :Array[Short]   => new ArrayIterator(a, from, until)
//			case a :Array[Boolean] => new ArrayIterator(a, from, until)
			case _                 => new ArrayIterator(array, from, until)
		}).castParam[T]

	def apply[T](array :Array[T]) :ArrayIterator[T] =
		make(array, 0, array.length)

	def apply[T](array :Array[T], offset :Int) :ArrayIterator[T] =
		if (offset >= array.length) make(array, array.length, array.length)
		else if (offset <= 0) make(array, 0, array.length)
		else make(array, offset, array.length)

	def apply[T](array :Array[T], from :Int, until :Int) :ArrayIterator[T] = {
		val len = array.length
		if (from >= len) make(array, len, len)
		else if (until <= 0) make(array, 0, 0)
		else if (from <= 0 && until >= len) make(array, 0, len)
		else if (from <= 0) make(array, 0, until)
		else if (until >= len) make(array, from, len)
		else if (until <= from) make(array, from, from)
		else make(array, from, until)
	}

	/** An iterator over `[from, until)` index range of `array`, going in reverse. The first returned element
	  * will be the one at index `until - 1`, and the last one at index `from`.
	  */
	def reversed[T](array :Array[T], from :Int, until :Int) :ReverseArrayIterator[T] =
		ReverseArrayIterator.over(array, from, until)

	/** Same as `ArrayIterator(array)`, but always produces an erased, not specialized instance. */
	def generic[T](array :Array[T]) :ArrayIterator[T] = new ArrayIterator(array)

	/** Same as `ArrayIterator(array, from, until)`, but always produces an erased, not specialized instance. */
	def generic[T](array :Array[T], from :Int, until :Int) :ArrayIterator[T] = {
		val len = array.length
		if (from >= len) new ArrayIterator(array, len, len)
		else if (until <= 0) new ArrayIterator(array, 0, 0)
		else if (from <= 0 && until >= len) new ArrayIterator(array, 0, len)
		else if (from <= 0) new ArrayIterator(array, 0, until)
		else if (until >= len) new ArrayIterator(array, from, len)
		else if (until <= from) new ArrayIterator(array, from, from)
		else new ArrayIterator(array, from, until)
	}

}




/** An iterator advancing over a slice of an array in the reverse direction.
  * @param last      the index in the array `last <= `first++`` of the last element to return
  *                  (the first index of the slice).
  * @param `first++` the index in the array pointing directly after the first/next element to return
  *                  (the end index of the slice).
  */
sealed class ReverseArrayIterator[@specialized(Int, Long, Double, AnyRef) +T] private[collections]
	                             (array :Array[T], private[this] var last :Int, private[this] var `first++` :Int)
	extends AbstractIterator[T] with AbstractIndexedReverseIterator[T]
{
//	def this(array :Array[T], idx :Int) = this(array, 0, idx)
	def this(array :Array[T]) = this(array, 0, array.length)

	private def unsafeArray :Array[_] = array
	protected final override def underlyingSize :Int = array.length
	final override def index :Int = `first++`
	protected final override def index_=(i :Int) :Unit = `first++` = i
	final override def limit :Int = last
	protected final override def limit_=(i :Int) :Unit = last = i

	override def hasNext :Boolean = `first++` > last
	override def head :T =
		if (`first++` > last) array(`first++` - 1)
		else throw new NoSuchElementException("Index " + `first++` + " reached the lower bound of " + last + ".")
	override def next() :T = {
		if (`first++` <= last)
			throw new NoSuchElementException("Index " + `first++` + " reached the lower bound of " + last + ".")
		`first++` -= 1
		array(`first++`)
	}

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :ReverseArrayIterator[_] =>
			(array eq other.unsafeArray) && index == other.index && limit == other.limit
		case _ => false
	}
	override def hashCode :Int = ReversedSeq(IArray.Wrapped.Slice(array.asInstanceOf[IArray[T]], index, limit)).hashCode
	override def clone = new ReverseArrayIterator(array, last, `first++`)
}


/** A factory of iterators advancing over a slice of an array in the reverse direction. */
@SerialVersionUID(Ver)
object ReverseArrayIterator {
	private def make[T](array :Array[T], from :Int, until :Int) :ReverseArrayIterator[T] =
		((array :Array[_]) match {
			case a :Array[AnyRef]  => new ReverseArrayIterator(a, from, until)
			case a :Array[Int]     => new ReverseArrayIterator(a, from, until)
			case a :Array[Long]    => new ReverseArrayIterator(a, from, until)
			case a :Array[Double]  => new ReverseArrayIterator(a, from, until)
//			case a :Array[Byte]    => new ReverseArrayIterator(a, from, until)
//			case a :Array[Char]    => new ReverseArrayIterator(a, from, until)
//			case a :Array[Float]   => new ReverseArrayIterator(a, from, until)
//			case a :Array[Short]   => new ReverseArrayIterator(a, from, until)
//			case a :Array[Boolean] => new ReverseArrayIterator(a, from, until)
			case _                 => new ReverseArrayIterator(array, from, until)
		}).castParam[T]

	def apply[T](array :Array[T]) :ReverseArrayIterator[T] =
		make(array, 0, array.length)

	def apply[T](array :Array[T], from :Int, downTo :Int) :ReverseArrayIterator[T] =
		over(array, downTo, from)

	def over[T](array :Array[T], from :Int, until :Int) :ReverseArrayIterator[T] = {
		val len = array.length
		if (until <= 0) make(array, 0, 0)
		else if (from >= len) make(array, len, len)
		else if (from <= 0 && until >= len) make(array, 0, len)
		else if (from <= 0) make(array, 0, until)
		else if (until >= len) make(array, from, len)
		else if (from >= until) make(array, until, until)
		else make(array, from, until)
	}

	/** Same as `ReverseArrayIterator(array)`, but always produces an erased, not specialized iterator. */
	def generic[T](array :Array[T]) :ReverseArrayIterator[T] = new ReverseArrayIterator[T](array, 0, array.length)

	/** Same as `ReverseArrayIterator(array, from, downTo)`, but always produces an erased, not specialized iterator. */
	def generic[T](array :Array[T], from :Int, downTo :Int) :ReverseArrayIterator[T] = {
		val len = array.length
		if (from <= 0) new ReverseArrayIterator(array, 0, 0)
		else if (downTo >= len) new ReverseArrayIterator(array, len, len)
		else if (downTo >= from) new ReverseArrayIterator(array, from, from)
		else if (downTo <= 0 && from >= len) new ReverseArrayIterator(array, 0, len)
		else if (downTo <= 0) new ReverseArrayIterator(array, 0, from)
		else if (from >= len) new ReverseArrayIterator(array, downTo, len)
		else new ReverseArrayIterator(array, downTo, from)
	}
}






/** An iterator advancing over a slice of an `IndexedSeq`.
  * @param first    the index in the sequence of the first/next element to return.
  * @param `last++` the index in the sequence delimiting the iterator, that is pointing after the last element
  *                 the iterator should return.
  */
sealed class IndexedSeqIterator[@specialized(Int, Long, Double, AnyRef) +T] private[collections]
	         (seq :collection.IndexedSeq[T], private[this] var first :Int, private[this] var `last++` :Int)
	extends AbstractIterator[T] with AbstractIndexedIterator[T]
{
	def this(seq :IndexedSeq[T], idx :Int) = this(seq, idx, seq.length)
	def this(seq :IndexedSeq[T]) = this(seq, 0, seq.length)

	private def underlying = seq
	protected final override def underlyingSize :Int = seq.length
	protected final override def index :Int = first
	protected final override def index_=(i :Int) :Unit = first = i
	protected final override def limit :Int = `last++`
	protected final override def limit_=(i :Int) :Unit = `last++` = i

	override def hasNext :Boolean = first < `last++`
	override def head :T = seq(first)
	override def next() :T = {
		if (first >= `last++`)
			throw new NoSuchElementException("Index " + first + " exceeds the upper bound of " + `last++` + ".")
		val res = seq(first)
		first += 1
		res
	}

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :IndexedSeqIterator[_] =>
			(seq eq other.underlying) && index == other.index && limit == other.limit
		case _ => false
	}
	override def hashCode :Int = seq.slice(index, limit).hashCode
	override def clone = new IndexedSeqIterator(seq, first, `last++`)
}


@SerialVersionUID(Ver)
object IndexedSeqIterator {
	def apply[@specialized(Int, Long, Double, AnyRef) T](seq :collection.IndexedSeq[T]) :IndexedSeqIterator[T] =
		new IndexedSeqIterator(seq, 0, seq.length)

	def apply[@specialized(Int, Long, Double, AnyRef) T](seq :collection.IndexedSeq[T], offset :Int) :IndexedSeqIterator[T] =
		if (offset >= seq.length) new IndexedSeqIterator(seq, seq.length, seq.length)
		else if (offset <= 0) new IndexedSeqIterator(seq, 0, seq.length)
		else new IndexedSeqIterator(seq, offset, seq.length)

	def apply[@specialized(Int, Long, Double, AnyRef) T]
	         (seq :collection.IndexedSeq[T], from :Int, until :Int) :IndexedSeqIterator[T] =
	{
		val len = seq.length
		if (from >= len) new IndexedSeqIterator(seq, len, len)
		else if (until <= 0) new IndexedSeqIterator(seq, 0, 0)
		else if (from <= 0 && until >= len) new IndexedSeqIterator(seq, 0, len)
		else if (from <= 0) new IndexedSeqIterator(seq, 0, until)
		else if (until >= len) new IndexedSeqIterator(seq, from, len)
		else if (until <= from) new IndexedSeqIterator(seq, from, from)
		else new IndexedSeqIterator(seq, from, until)
	}

	@inline def reverse[@specialized(Int, Long, Double, AnyRef) T]
		               (seq :collection.IndexedSeq[T], from :Int, until :Int) :ReverseIndexedSeqIterator[T] =
		ReverseIndexedSeqIterator.over(seq, from, until)
}




/** An iterator advancing over a slice of an `IndexedSeq` in the reverse direction.
  * @param last      the index in the sequence `last <= `first++`` of the last element to return
  *                  (the first index of the slice).
  * @param `first++` the index in the sequence pointing directly after the first/next element to return
  *                  (the end index of the slice).
  */
sealed class ReverseIndexedSeqIterator[@specialized(Int, Long, Double, AnyRef) +T] private[collections]
	         (seq :collection.IndexedSeq[T], private[this] var last :Int, private[this] var `first++` :Int)
	extends AbstractIterator[T] with AbstractIndexedReverseIterator[T]
{
	def this(seq :IndexedSeq[T], idx :Int) = this(seq, 0, idx)
	def this(seq :IndexedSeq[T]) = this(seq, 0, seq.length)

	private def underlying = seq
	protected final override def underlyingSize :Int = seq.length
	protected final override def index :Int = `first++`
	protected final override def index_=(i :Int) :Unit = `first++` = i
	protected final override def limit :Int = last
	protected final override def limit_=(i :Int) :Unit = last = i

	override def hasNext :Boolean = `first++` > last
	override def head :T = seq(`first++` - 1)
	override def next() :T = {
		if (`first++` <= last)
			throw new NoSuchElementException("Index " + `first++` + " has reached the lower bound of " + last + ".")
		`first++` -= 1
		seq(`first++`)
	}

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :ReverseIndexedSeqIterator[_] =>
			(underlying eq other.underlying) && index == other.index && limit == other.limit
		case _ => false
	}
	override def hashCode :Int = ReversedSeq(seq.slice(last, `first++`).toIndexedSeq).hashCode
	override def clone = new ReverseIndexedSeqIterator(seq, last, `first++`)
}


@SerialVersionUID(Ver)
object ReverseIndexedSeqIterator {
	def apply[@specialized(Int, Long, Double, AnyRef) T](seq :collection.IndexedSeq[T]) :ReverseIndexedSeqIterator[T] =
		new ReverseIndexedSeqIterator(seq, 0, seq.length)

	@inline def apply[@specialized(Int, Long, Double, AnyRef) T]
	                 (seq :collection.IndexedSeq[T], from :Int, downTo :Int) :ReverseIndexedSeqIterator[T] =
		over(seq, downTo, from)

	def over[@specialized(Int, Long, Double, AnyRef) T]
	        (seq :collection.IndexedSeq[T], from :Int, until :Int) :ReverseIndexedSeqIterator[T] = 
	{
		val len = seq.length
		if (from >= len) new ReverseIndexedSeqIterator(seq, len, len)
		else if (until <= 0) new ReverseIndexedSeqIterator(seq, 0, 0)
		else if (from <= 0 && until >= len) new ReverseIndexedSeqIterator(seq, 0, len)
		else if (from <= 0) new ReverseIndexedSeqIterator(seq, 0, until)
		else if (until >= len) new ReverseIndexedSeqIterator(seq, from, len)
		else if (until <= from) new ReverseIndexedSeqIterator(seq, from, from)
		else new ReverseIndexedSeqIterator(seq, from, until)
	}
}






/** An iterator over an arbitrary section of a `String`,
  * similar to [[net.noresttherein.sugar.collections.ArrayIterator ArrayIterator]].
  * Has O(1) `take`/`drop`/`slice` methods.
  */
final class StringIterator private[collections]
	        (string :String, private[this] var first :Int, private[this] var `last++` :Int)
	extends AbstractIterator[Char] with AbstractIndexedIterator[Char]
{
	private def underlying = string
	protected override def underlyingSize :Int = string.length
	protected override def index :Int = first
	protected override def index_=(i :Int) :Unit = first = i
	protected override def limit :Int = `last++`
	protected override def limit_=(i :Int) :Unit = `last++` = i

	override def hasNext :Boolean = first < `last++`
	override def head :Char = string.charAt(first)

	override def next() :Char = {
		if (first >= `last++`)
			throw new NoSuchElementException("Index " + first + " has reached its upper bound of " + `last++` + ".")
		val res = string.charAt(first)
		first += 1
		res
	}

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :StringIterator =>
			(string eq other.underlying) && index == other.index && limit == other.limit
		case _ => false
	}
	override def hashCode :Int = new WrappedString(string).slice(first, `last++`).hashCode
	override def clone = new StringIterator(string, first, `last++`)
}


@SerialVersionUID(Ver)
object StringIterator {
	def apply(string :String) :StringIterator = new StringIterator(string, 0, string.length)

	def apply(string :String, offset :Int) :StringIterator = {
		val length = string.length
		if (offset >= length) new StringIterator(string, length, length)
		else if (offset <= 0) new StringIterator(string, 0, string.length)
		else new StringIterator(string, offset, string.length)
	}

	def apply(string :String, from :Int, until :Int) :StringIterator = {
		val len = string.length
		if (from >= len) new StringIterator(string, len, len)
		else if (until <= 0) new StringIterator(string, 0, 0)
		else if (from <= 0 & until >= len) new StringIterator(string, 0, len)
		else if (from <= 0) new StringIterator(string, 0, until)
		else if (until >= len) new StringIterator(string, from, len)
		else if (until <= from) new StringIterator(string, from, from)
		else new StringIterator(string, from, until)
	}

	@inline def reverse(string :String, from :Int, until :Int) :ReverseStringIterator =
		ReverseStringIterator.over(string, from, until)

	val empty :StringIterator = new StringIterator("", 0, 0)
}




/** An iterator over an arbitrary section of a `String`, running in reverse
  * similar to [[net.noresttherein.sugar.collections.ReverseArrayIterator ReverseArrayIterator]].
  * Has O(1) `take`/`drop`/`slice` methods.
  */
final class ReverseStringIterator private[collections]
	        (string :String, private[this] var last :Int, private[this] var `first++` :Int)
	extends AbstractIterator[Char] with AbstractIndexedReverseIterator[Char]
{
	/* Requires 0 <= from <= until <= string.length and maintains invariant 0 <= stop <= index <= string.length.
	* The invariant can be broken only by advancing an empty iterator.
	* `string(until)` is the character immediately following (in string) the first character in this iterator,
	* while `string(from)` is the last character in this iterator, unless it is empty.
	* This scheme results in code being a mirror image of StringIterator, with + replaced with -.
	*/
	private def underlying = string
	protected override def underlyingSize :Int = string.length
	protected override def index :Int = `first++`
	protected override def index_=(i :Int) :Unit = `first++` = i
	protected override def limit :Int = last
	protected override def limit_=(i :Int) :Unit = last = i

	override def hasNext :Boolean = `first++` > last
	override def head :Char = string.charAt(`first++` - 1)

	override def next() :Char = {
		if (`first++` <= last)
			throw new NoSuchElementException("Index " + `first++` + " has reached its lower bound of " + last + ".")
		`first++` -= 1
		string.charAt(`first++`)
	}

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :ReverseStringIterator =>
			(string eq other.underlying) && index == other.index && limit == other.limit
		case _ => false
	}
	override def hashCode :Int = ReversedSeq(new WrappedString(string).slice(last, `first++`)).hashCode
	override def clone = new ReverseStringIterator(string, last, `first++`)
}


@SerialVersionUID(Ver)
object ReverseStringIterator {
	def apply(string :String) :ReverseStringIterator = new ReverseStringIterator(string, 0, string.length)

	def apply(string :String, from :Int, downTo :Int) :ReverseStringIterator = over(string, downTo, from)

	def over(string :String, from :Int, until :Int) :ReverseStringIterator = {
		val len = string.length
		if (from >= len) new ReverseStringIterator(string, len, len)
		else if (until <= 0) new ReverseStringIterator(string, 0, 0)
		else if (from <= 0 & until >= len) new ReverseStringIterator(string, 0, len)
		else if (from <= 0) new ReverseStringIterator(string, 0, until)
		else if (until >= len) new ReverseStringIterator(string, from, len)
		else if (until <= from) new ReverseStringIterator(string, from, from)
		else new ReverseStringIterator(string, from, until)
	}

	val empty :ReverseStringIterator = new ReverseStringIterator("", 0, 0)
}
