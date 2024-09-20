package net.noresttherein.sugar.collections

import java.lang.{Math => math}

import scala.collection.immutable.WrappedString
import scala.collection.{AbstractIterator, BufferedIterator}

import net.noresttherein.sugar.casting.castingMethods
import net.noresttherein.sugar.collections.util.elementsToCopy
import net.noresttherein.sugar.exceptions.{illegalState_!, noSuch_!, outOfBounds_!, unsupported_!}
import net.noresttherein.sugar.reflect.prettyprint.localClassNameOf
import net.noresttherein.sugar.typist.kinds


@SerialVersionUID(Ver)
private object IndexedIterator {
	def fix[E](iterator :IndexedIterator[E]) :iterator.type = {
		iterator.adjustRange()
		iterator
	}
	def validated[E](iterator :IndexedIterator[E]) :iterator.type = {
		iterator.validateRange()
		iterator
	}
}


//todo: make them operate on index, size basis, not index, limit.
/** Base trait for implementations of iterators over slices of some sequential collections.
  * The iterator advances over a window on the collection; it is assumed to use random indexing
  * to return the elements, but they are never handled by this class itself.
  * Provides fast implementations for `size`, `take`, `drop` and some other methods.
  * @see [[net.noresttherein.sugar.collections.ReverseIndexedIterator]]
  * @author Marcin Mościcki
  */ //consider: throwing exceptions with constant strings as messages for performance
trait IndexedIterator[+T] extends BufferedIterator[T] with Cloneable {
	protected def underlyingSize :Int
	protected var index :Int
	protected var limit :Int

	/** An optional convenience method for implementors which clips the current `index` and `limit` to
	  * `[0, underlyingSize]` range.
	  */
	protected def adjustRange() :Unit = {
		val total = underlyingSize
		val curr  = index
		var end   = limit
		if (end > total) {
			limit = total
			end   = total
		}
		if (curr > end)
			index = end
		else if (curr < 0)
			index = 0
	}

	/** An optional convenience method for implementors which throws an [[IndexOutOfBoundsException]]
	  * if  `index` or `limit` are out of `[0, underlyingSize]` range.
	  */
	protected def validateRange() :Unit = {
		val total = underlyingSize
		val curr  = index
		val end   = limit
		if (end > total || end < 0)
			outOfBounds_!(end, total)
		else if (curr > total || curr < 0)
			outOfBounds_!(curr, total)
	}

	override def knownSize :Int = math.max(0, limit - index)
	final override def size :Int = knownSize

	override def hasNext :Boolean = index < limit

	/** Returns the element at `index` in the underlying collection. */
	override def head :T

	override def next() :T = {
		val idx = index
		if (idx >= limit)
			noSuch_!("Index " + index + " exceeds the limit of " + limit + ".")
		val res = head
		index = idx + 1
		res
	}
	def skip() :this.type = {
		val idx = index
		if (idx >= limit)
			unsupported_!("Index " + idx + " exceeds the limit of " + limit + ".")
		index = idx + 1
		this
	}

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
		val size = knownSize
		if (until < size)
			limit = index + until
		if (from > size)
			index = limit
		else if (from > 0)
			index += from
		this
	}
	override def reduceLeft[U >: T](op :(U, T) => U) :U =
		if (hasNext) foldLeft[U](next())(op)
		else unsupported_!("Iterator().reduceLeft")

	override def reduceLeftOption[U >: T](op :(U, T) => U) :Option[U] = if (hasNext) Some(reduceLeft(op)) else None

	override def clone :IndexedIterator[T] = super.clone.asInstanceOf[IndexedIterator[T]]

	protected def className :String = localClassNameOf(this)

	override def toString :String = className + "|" + knownSize + "|(@" + index + "/" + underlyingSize + ")"
//	override def toString :String = clone.mkString("Iterator(", ",", ")")
}


/** Base class for forward iterators over structures with random indexing.
  * Requires of he subclasses to only implement `head`. This class does not perform any validation of its arguments.
  * Subclasses who wish to check the index range may do so by calling either
  * [[net.noresttherein.sugar.collections.IndexedIterator.adjustRange adjustRange]] or
  * [[net.noresttherein.sugar.collections.IndexedIterator.validateRange validateRange]], after implementing
  * [[net.noresttherein.sugar.collections.IndexedIterator.underlyingSize underlyingSize]]
  * (whose default implementation is a stub).
  * @param idx the index of the first element of the iterator, exposed to subclasses by mutable property `index`.
  * @param end the index immediately following the last element in the iterator.
  */
abstract class AbstractIndexedIterator[+T](private[this] var idx :Int, private[this] var end :Int)
	extends AbstractIterator[T] with IndexedIterator[T]
{
	protected override def underlyingSize :Int = -1
	protected final override def index :Int = idx
	protected final override def index_=(value :Int) :Unit = idx = value
	protected final override def limit :Int = end
	protected final override def limit_=(value :Int) :Unit = end = value

	/** Returns `limit - index`. */
	final override def knownSize :Int = end - idx

	/** Returns `index < limit`. */
	final override def hasNext :Boolean = idx < end

	/** Returns `head` and increases `index`. */
	override def next() :T = {
		val res = head
		idx += 1
		res
	}
	override def skip() :this.type =
		if (idx >= end)
			unsupported_!(toString + ".skip()")
		else {
			idx += 1
			this
		}
}




/** A special case of an `IndexedIterator` which wraps at the lo of the range: when `index` is increased
  * to `rangeEnd` (which equals `rangeStart + underlyingSize`, it is set to `rangeStart` instead.
  * For this reason, the iterator will stop ''only if'' `index == limit` (rather than `index >= limit`),
  * and `size` is also counted assuming precise indexing. Subclasses ''must'' implement `rangeStart`
  * to be the first index of the range (where iteration continues after wrapping) and `underlyingSize` in such a way,
  * that `rangeStart + underlyingSize` is the index of the lo of the iterated range
  * (where wrapping of `index` happens).
  *
  * The `validateRange` and `adjustRange` methods rely on `start` and `underlyingSize` to move `index` and `limit`
  * to `start` plus their canonical positive remainder module `underlyingSize`. The implementation assumes that before
  * any public methods are called, the indices have been initialized to point to valid positions.
  *
  * @note `index` if the implementation is in terms on an index and the number of remaining elements,
  *       rather than two indices, `index` and `limit` must be overridden by a set of accessor methods,
  *       such that updating them updates also the remaining number of elements.
  *
  */
trait CyclicIndexedIterator[+T] extends IndexedIterator[T] {
	/** The first index in the underlying range, to which wrapping happens.
	  * @return defaults to zero.
	  */
	protected def rangeStart :Int = 0

	/** The index after the last element in the underlying range, at which `index` is wrapped back to `rangeStart`.
	  * @return `rangeStart + underlyingSize`.
	  */
	protected def rangeEnd :Int = rangeStart + underlyingSize

	override def knownSize :Int = {
		val res = limit - index
		if (res >= 0)
			res
		else
			underlyingSize + res
	}
	override def hasNext :Boolean = index != limit
	override def next() :T = {
		val idx = index
		val end = limit
		if (idx == end)
			noSuch_!(toString + ".next()")
		val res = head
		index = if (idx == rangeEnd - 1) rangeStart else idx + 1
		res
	}

	protected override def adjustRange() :Unit = {
		val start = rangeStart
		val end   = rangeEnd
		val idx   = index
		val until = limit
		if (end < start | start < 0 | end - start != underlyingSize)
			illegalState_!(
				toString + " cannot iterate in range [" + start + ", " + end + ") of length " + underlyingSize + "."
			)
		if (idx > end)
			index = if (start == end) start else start + (idx - start) % (end - start)
		else if (idx < start)
			index = if (start == end) start else end + (idx - start) % (end - start)
		if (until > end)
			limit = if (start == end) start else start + (until - start) % (end - start)
		else if (idx < start)
			limit = if (start == end) start else end + (until - start) % (end - start)
	}

	protected override def validateRange() :Unit = {
		val start = rangeStart
		val end   = rangeEnd
		val size  = underlyingSize
		val idx   = index
		val until = limit
		if (start < 0 | start > end | size != end - start)
			illegalState_!(
				toString + " cannot iterate in range [" + start + ", " + end + ") of length " + size + "."
			)
		if (idx < start | idx > end | idx == end & size > 0)
			outOfBounds_!(toString + " start index " + idx + " out of range [" + start + ", " + end + ").")
		if (until < start | until >= end | until == end & size > 0)
			outOfBounds_!(toString + " lo index " + until + " out of range [" + start + ", " + end + ").")
	}

	override def copyToArray[B >: T](xs :Array[B], start :Int, len :Int) :Int = {
		val copied = elementsToCopy(underlyingSize, xs, start, len)
		val suffix = rangeEnd - index
		if (copied <= suffix)
			super.copyToArray(xs, start, len)
		else {
			super.copyToArray(xs, start, suffix)
			super.copyToArray(xs, start + suffix, copied - suffix)
			copied
		}
	}
}




@SerialVersionUID(Ver)
private object ReverseIndexedIterator {
	def fix[E](iterator :ReverseIndexedIterator[E]) :iterator.type = {
		iterator.adjustRange()
		iterator
	}
	def validated[E](iterator :ReverseIndexedIterator[E]) :iterator.type = {
		iterator.validateRange()
		iterator
	}
}


/** Base trait for implementations of iterators traversing in the reverse order over slices
  * of some sequential collections. The iterator advances over a window on the collection; it is assumed to use
  * random indexing to return the elements, but they are never handled by this class itself.
  * Provides fast implementations for `size`, `take`, `drop` and some other methods.
  * @see [[net.noresttherein.sugar.collections.IndexedIterator]]
  * @author Marcin Mościcki
  */ //todo: rename to ReverseIndexedIterator (after finishing MatrixIterator and Mutator)
trait ReverseIndexedIterator[+T] extends BufferedIterator[T] with Cloneable {
	protected def underlyingSize :Int
	protected var index :Int
	protected var limit :Int

	/** An optional convenience method for implementors which clips the current `index` and `limit` to
	  * `[0, underlyingSize]` range.
	  */
	protected def adjustRange() :Unit = {
		val total = underlyingSize
		val end   = limit
		var curr  = index
		if (curr >= total) {
			index = total
			curr  = total
		}
		if (end > curr)
			limit = curr
		else if (end < 0)
			limit = 0
	}

	/** An optional convenience method for implementors which throws an [[IndexOutOfBoundsException]]
	  * if  `index` or `limit` are out of `[0, underlyingSize]` range.
	  */
	protected def validateRange() :Unit = {
		val total = underlyingSize
		val curr  = index
		val end   = limit
		if (end > total || end < 0)
			outOfBounds_!(end, total)
		else if (curr > total || curr < 0)
			outOfBounds_!(curr, total)
	}

	override def knownSize :Int = math.max(0, index - limit)
	final override def size :Int = knownSize

	override def hasNext :Boolean = index > limit

	/** Returns the element at `index - 1` in the underlying collection. */
	override def head :T

	override def next() :T = {
		if (index <= limit)
			noSuch_!("Index " + index + " reached the lower bound of " + limit + ".")
		val hd = head
		index -= 1
		hd
	}

	def skip() :this.type = {
		val idx = index
		if (idx <= limit)
			unsupported_!(toString + ".skip()")
		index = idx - 1
		this
	}
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
	override def reduceLeft[U >: T](op :(U, T) => U) :U =
		if (!hasNext) unsupported_!("Iterator().reduceLeft")
		else foldLeft[U](next())(op)

	override def reduceLeftOption[U >: T](op :(U, T) => U) :Option[U] = if (hasNext) Some(reduceLeft(op)) else None

	override def clone :ReverseIndexedIterator[T] =
		super.clone.asInstanceOf[ReverseIndexedIterator[T]]

	protected def className :String = localClassNameOf(this)

	override def toString :String = className + "|" + knownSize + "|(@" + index + "/" + underlyingSize + ")"
//	override def toString :String = clone.mkString("Iterator(", ",", ")")
}


/** Base class for forward iterators over structures with random indexing.
  * Requires of he subclasses to only implement `head`, which is the element immediately ''before''
  * [[net.noresttherein.sugar.collections.ReverseIndexedIterator.index index]]. This class does not perform
  * any validation of its arguments. Subclasses who wish to check the index range may do so by calling either
  * [[net.noresttherein.sugar.collections.ReverseIndexedIterator.adjustRange adjustRange]] or
  * [[net.noresttherein.sugar.collections.ReverseIndexedIterator.validateRange validateRange]], after implementing
  * [[net.noresttherein.sugar.collections.ReverseIndexedIterator.underlyingSize underlyingSize]]
  * (whose the default implementation is a stub).
  * @param lo the (lower) index of the last element in the iterator, exposed to subclasses by mutable property `limit`.
  * @param hi the (upper) index immediately after the first element of the iterator,
  *           exposed to subclasses by mutable property `index`.
  */
abstract class AbstractReverseIndexedIterator[+T](private[this] var lo :Int, private[this] var hi :Int)
	extends AbstractIterator[T] with ReverseIndexedIterator[T]
{
	protected override def underlyingSize :Int = -1
	protected final override def index :Int = hi
	protected final override def index_=(value :Int) :Unit = hi = value
	protected final override def limit :Int = lo
	protected final override def limit_=(value :Int) :Unit = lo = value

	/** Returns `index - limit`. */
	final override def knownSize :Int = hi - lo

	/** True if `index > limit`. */
	final override def hasNext :Boolean = hi > lo

	/** Returns `head` and decreases `index`. */
	override def next() :T = {
		val res = head
		hi -= 1
		res
	}
	override def skip() :this.type = {
		if (hi <= lo)
			unsupported_!(toString + ".skip()")
		hi -= 1
		this
	}
}






private abstract class IndexedIteratorFactory[S[X] <: collection.IterableOps[X, kinds.Any1, _], I[X]] {
	protected def make[T](seq :S[T], from :Int, until :Int) :I[T]

	def apply[T](seq :S[T]) :I[T] =
		make(seq, 0, seq.size)

	def from[T](seq :S[T], first :Int) :I[T] =
		if (first >= seq.size) make(seq, seq.size, seq.size)
		else if (first <= 0) make(seq, 0, seq.size)
		else make(seq, first, seq.size)

	/** Returns elements `seq(first), seq(first + 1), ..., seq(first + length - 1)` of the given sequence.
	  * If reading would go past the lo of the sequence, the excess index range is ignored. Negative `length`
	  * is equivalent to zero.
	  */
	@throws[IndexOutOfBoundsException]("if offset is negative or greater than the length of the sequence.")
	def apply[T](seq :S[T], first :Int, length :Int) :I[T] = {
		val len   = seq.size
		if (first < 0 | first > len)
			outOfBounds_!(first.toString + " is out of bounds [0, " + len + ")")
		val until = first + math.min(len - first, math.max(length, 0))
		make(seq, first, until)
	}

	/** Returns elements `seq(from), seq(from + 1), ..., seq(until - 1)` of the given sequence.
	  * If any of indices in the `[from, until)` range are negative or greater than the sequence's length,
	  * they are ignored.
	  */
	def slice[T](seq :S[T], from :Int, until :Int) :I[T] = {
		val len = seq.size
		if (from >= len) make(seq, len, len)
		else if (until <= 0) make(seq, 0, 0)
		else if (from <= 0 && until >= len) make(seq, 0, len)
		else if (from <= 0) make(seq, 0, until)
		else if (until >= len) make(seq, from, len)
		else if (until <= from) make(seq, from, from)
		else make(seq, from, until)
	}
}



/** An iterator advancing over a slice of an `IndexedSeq`.
  * @param first    the index in the sequence of the first/next element to return.
  * @param `last++` the index in the sequence delimiting the iterator, that is pointing after the last element
  *                 the iterator should return.
  */
private final class IndexedSeqIterator[+T] private[collections]
	                                  (seq :collection.IndexedSeqOps[T, kinds.Any1, _], first :Int, `last++` :Int)
	extends AbstractIndexedIterator[T](first, `last++`)
{
	def this(seq :collection.IndexedSeqOps[T, kinds.Any1, _], idx :Int) = this(seq, idx, seq.length)
	def this(seq :collection.IndexedSeqOps[T, kinds.Any1, _]) = this(seq, 0, seq.length)

	private def underlying = seq
	protected final override def underlyingSize :Int = seq.length
	override def head :T = seq(index)

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :IndexedSeqIterator[_] =>
			(seq.asAnyRef eq other.underlying.asAnyRef) && index == other.index && limit == other.limit
		case _ => false
	}
	override def hashCode :Int = seq.slice(index, limit).hashCode
	override def clone = new IndexedSeqIterator(seq, first, `last++`)
}


@SerialVersionUID(Ver)
private case object IndexedSeqIterator
	extends IndexedIteratorFactory[ ({ type S[+X] = collection.IndexedSeqOps[X, kinds.Any1, _] })#S, IndexedSeqIterator]
{
	protected override def make[T](seq :collection.IndexedSeqOps[T, kinds.Any1, _], from :Int, until :Int)
			:IndexedSeqIterator[T] =
		new IndexedSeqIterator(seq, 0, seq.length)
}




/** An iterator advancing over a slice of an `IndexedSeq` in the reverse direction.
  * @param last      the index in the sequence `last <= first++` of the last element to return
  *                  (the first index of the slice).
  * @param `first++` the index in the sequence pointing directly after the first/next element to return
  *                  (the lo index of the slice).
  */
private final class ReverseIndexedSeqIterator[+T] private[collections]
	                                         (seq :collection.IndexedSeqOps[T, kinds.Any1, _], last :Int, `first++` :Int)
	extends AbstractReverseIndexedIterator[T](last - 1, `first++` - 1) with ReverseIndexedIterator[T]
{
	def this(seq :collection.IndexedSeqOps[T, kinds.Any1, _], idx :Int) = this(seq, 0, idx)
	def this(seq :collection.IndexedSeqOps[T, kinds.Any1, _]) = this(seq, 0, seq.length)

	private def underlying = seq
	protected override def underlyingSize :Int = seq.length

	override def head :T = seq(index)

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :ReverseIndexedSeqIterator[_] =>
			(underlying.asAnyRef eq other.underlying.asAnyRef) && index == other.index && limit == other.limit
		case _ => false
	}
	override def hashCode :Int = ReversedSeq(seq.toIndexedSeq.slice(limit + 1, index + 1)).hashCode
	override def clone = new ReverseIndexedSeqIterator(seq, limit + 1, index + 1)
}


@SerialVersionUID(Ver)
private object ReverseIndexedSeqIterator {
	def apply[T](seq :collection.IndexedSeqOps[T, kinds.Any1, _]) :ReverseIndexedSeqIterator[T] =
		new ReverseIndexedSeqIterator(seq, 0, seq.length)

	/** An iterator returning elements `seq(first), seq(first - 1), ..., seq(first - length + 1)`.
	  * If the iterator needs to access an element at index lesser than zero, the excess elements are ignored.
	  * Negative `length` is the same as zero.
	  */
    @throws[IndexOutOfBoundsException]("if first is negative or greater or equal to the length of the sequence")
	def apply[T](seq :collection.IndexedSeqOps[T, kinds.Any1, _], first :Int, length :Int)
            :ReverseIndexedSeqIterator[T] =
    {
		val len = seq.length
		if (first < 0 | first >= len)
			outOfBounds_!(first.toString + " is out of bounds [0, " + len + ")")
		val downTo = first + 1 - math.min(first + 1, math.max(length, 0))
		new ReverseIndexedSeqIterator(seq, downTo, first + 1)
	}

	/** An iterator returning elements `seq(hi - 1), seq(hi - 2), ..., seq(lo)`.
	  * If any of the indices in the `[lo, hi)` range is out of bounds for `seq`, it is ignored.
	  */
	def slice[T](seq :collection.IndexedSeqOps[T, kinds.Any1, _], lo :Int, hi :Int)
			:ReverseIndexedSeqIterator[T] =
	{
		val len = seq.length
		if (lo >= len) new ReverseIndexedSeqIterator(seq, len, len)
		else if (hi <= 0) new ReverseIndexedSeqIterator(seq, 0, 0)
		else if (lo <= 0 && hi >= len) new ReverseIndexedSeqIterator(seq, 0, len)
		else if (lo <= 0) new ReverseIndexedSeqIterator(seq, 0, hi)
		else if (hi >= len) new ReverseIndexedSeqIterator(seq, lo, len)
		else if (hi <= lo) new ReverseIndexedSeqIterator(seq, lo, lo)
		else new ReverseIndexedSeqIterator(seq, lo, hi)
	}
}






/** An iterator over an arbitrary section of a `String`,
  * similar to [[net.noresttherein.sugar.arrays.ArrayIterator ArrayIterator]].
  * Has O(1) `take`/`drop`/`slice` methods.
  */
final class StringIterator private[collections]
	        (string :String, private[this] var first :Int, private[this] var `last++` :Int)
	extends AbstractIterator[Char] with IndexedIterator[Char]
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
			noSuch_!("Index " + first + " has reached its upper bound of " + `last++` + ".")
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

	override def toString :String = "StringIterator(\"" + string + "\"@" + index + ")"
}


@SerialVersionUID(Ver)
object StringIterator {
	def apply(string :String) :StringIterator = new StringIterator(string, 0, string.length)

	def from(string :String, first :Int) :StringIterator = {
		val length = string.length
		if (first >= length) new StringIterator(string, length, length)
		else if (first <= 0) new StringIterator(string, 0, string.length)
		else new StringIterator(string, first, string.length)
	}

	/** Returns characters `string(first), string(first + 1), ..., string(first + length - 1)` of the given string.
	  * If reading would go past the lo of `string`, the excess index range is ignored. Negative `length`
	  * is equivalent to zero.
	  */
	@throws[IndexOutOfBoundsException]("if offset is negative or greater than the length of the array.")
	def apply(string :String, first :Int, length :Int) :StringIterator = {
		val len = string.length
		if (first < 0 | first > len)
			outOfBounds_!(first.toString + " is out of bounds [0, " + len + ")")
		val until = first + math.min(len - first, math.max(length, 0))
		new StringIterator(string, first, until)
	}

	/** Returns characters `string(from), string(from + 1), ..., string(until - 1)` of the given string.
	  * If any of indices in the `[from, until)` range are negative or greater than the string's length, they are ignored.
	  */
	def slice(string :String, from :Int, until :Int) :StringIterator = {
		val len = string.length
		if (from >= len) new StringIterator(string, len, len)
		else if (until <= 0) new StringIterator(string, 0, 0)
		else if (from <= 0 & until >= len) new StringIterator(string, 0, len)
		else if (from <= 0) new StringIterator(string, 0, until)
		else if (until >= len) new StringIterator(string, from, len)
		else if (until <= from) new StringIterator(string, from, from)
		else new StringIterator(string, from, until)
	}
//
//	@inline def reverse(string :String, from :Int, until :Int) :ReverseStringIterator =
//		ReverseStringIterator.slice(string, from, until)

	val empty :StringIterator = new StringIterator("", 0, 0)
}




/** An iterator over an arbitrary section of a `String`, running in reverse
  * similar to [[net.noresttherein.sugar.arrays.ReverseArrayIterator ReverseArrayIterator]].
  * Has O(1) `take`/`drop`/`slice` methods.
  */
final class ReverseStringIterator private[collections]
	        (string :String, private[this] var last :Int, private[this] var `first++` :Int)
	extends AbstractIterator[Char] with ReverseIndexedIterator[Char]
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
			noSuch_!("Index " + `first++` + " has reached its lower bound of " + last + ".")
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

	override def toString :String = "ReverseStringIterator(\"" + string + "\"@" + index + ")"
}


@SerialVersionUID(Ver)
object ReverseStringIterator {
	def apply(string :String) :ReverseStringIterator = new ReverseStringIterator(string, 0, string.length)

	/** An iterator returning elements `seq(first), seq(first - 1), ..., seq(first - length + 1)`.
	  * If the iterator needs to access an element at index lesser than zero, the excess elements are ignored.
	  * Negative `length` is the same as zero.
	  */
	@throws[IndexOutOfBoundsException]("if first is negative or greater or equal to the string's length")
	def apply(string :String, first :Int, length :Int) :ReverseStringIterator = {
		val len = string.length
		if (first < 0 | first >= len)
			outOfBounds_!(first.toString + " is out of bounds [0, " + len + ")")
		val downTo = first + 1 - math.min(first + 1, math.max(length, 0))
		new ReverseStringIterator(string, downTo, first + 1)
	}

	/** An iterator returning characters `string(hi - 1), string(hi - 2), ..., string(lo)`.
	  * If any of the indices in the `[lo, hi)` range is out of bounds for `string`, it is ignored.
	  */
	def slice(string :String, lo :Int, hi :Int) :ReverseStringIterator = {
		val len = string.length
		if (lo >= len) new ReverseStringIterator(string, len, len)
		else if (hi <= 0) new ReverseStringIterator(string, 0, 0)
		else if (lo <= 0 & hi >= len) new ReverseStringIterator(string, 0, len)
		else if (lo <= 0) new ReverseStringIterator(string, 0, hi)
		else if (hi >= len) new ReverseStringIterator(string, lo, len)
		else if (hi <= lo) new ReverseStringIterator(string, lo, lo)
		else new ReverseStringIterator(string, lo, hi)
	}

	val empty :ReverseStringIterator = new ReverseStringIterator("", 0, 0)
}

