package net.noresttherein.sugar.collections

import java.lang.{Math => math}

import scala.collection.immutable.WrappedString
import scala.collection.{AbstractIterator, BufferedIterator}

import net.noresttherein.sugar.casting.castingMethods
import net.noresttherein.sugar.exceptions.{noSuch_!, outOfBounds_!, unsupported_!}
import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.reflect.prettyprint.localClassNameOf




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

	override def next() :T = {
		if (index >= limit)
			noSuch_!("Index " + index + " exceeds the limit of " + limit + ".")
		val res = head
		index += 1
		res
	}
	def skip() :this.type = {
		val i = index
		if (i >= limit)
			unsupported_!("Index " + i + " exceeds the limit of " + limit + ".")
		index = i + 1
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


abstract class AbstractIndexedIterator[+T](private[this] var idx :Int, private[this] var end :Int)
	extends AbstractIterator[T] with IndexedIterator[T]
{
	protected final override def index :Int = idx
	protected final override def index_=(value :Int) :Unit = idx = value
	protected final override def limit :Int = end
	protected final override def limit_=(value :Int) :Unit = end = value

	final override def hasNext :Boolean = idx < end

	override def next() :T = {
		val res = head
		idx += 1
		res
	}
	final override def skip() :this.type =
		if (idx >= end)
			unsupported_!(toString + ".skip()")
		else {
			idx += 1
			this
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
		index = idx + 1
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


abstract class AbstractReverseIndexedIterator[+T](private[this] var idx :Int, private[this] var end :Int)
	extends AbstractIterator[T] with ReverseIndexedIterator[T]
{
	final override def index :Int = idx
	final override def index_=(value :Int) :Unit = idx = value
	final override def limit :Int = end
	final override def limit_=(value :Int) :Unit = end = value

	final override def hasNext :Boolean = idx > end

	override def next() :T = {
		val res = head
		idx -= 1
		res
	}
	final override def skip() :this.type = {
		if (idx <= end)
			unsupported_!(toString + ".skip()")
		idx -= 1
		this
	}
}






private abstract class IndexedIteratorFactory[S[X] <: collection.IterableOps[X, generic.Any1, _], I[X]] {
	protected def make[T](seq :S[T], from :Int, until :Int) :I[T]

	def apply[T](seq :S[T]) :I[T] =
		make(seq, 0, seq.size)

	def from[T](seq :S[T], first :Int) :I[T] =
		if (first >= seq.size) make(seq, seq.size, seq.size)
		else if (first <= 0) make(seq, 0, seq.size)
		else make(seq, first, seq.size)

	/** Returns elements `seq(first), seq(first + 1), ..., seq(first + length - 1)` of the given sequence.
	  * If reading would go past the end of the sequence, the excess index range is ignored. Negative `length`
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
private sealed class IndexedSeqIterator[+T] private[collections]
	                                   (seq :collection.IndexedSeqOps[T, generic.Any1, _],
	                                    private[this] var first :Int, private[this] var `last++` :Int)
	extends AbstractIterator[T] with IndexedIterator[T]
{
	def this(seq :collection.IndexedSeqOps[T, generic.Any1, _], idx :Int) = this(seq, idx, seq.length)
	def this(seq :collection.IndexedSeqOps[T, generic.Any1, _]) = this(seq, 0, seq.length)

	private def underlying = seq
	protected final override def underlyingSize :Int = seq.length
	protected final override def index :Int = first
	protected final override def index_=(i :Int) :Unit = first = i
	protected final override def limit :Int = `last++`
	protected final override def limit_=(i :Int) :Unit = `last++` = i

	final override def hasNext :Boolean = first < `last++`
	override def head :T = seq(first)
	override def next() :T = {
		if (first >= `last++`)
			noSuch_!("Index " + first + " exceeds the upper bound of " + `last++` + ".")
		val res = seq(first)
		first += 1
		res
	}

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
private case object IndexedSeqIterator extends IndexedIteratorFactory[collection.IndexedSeq, IndexedSeqIterator] {
	protected override def make[T](seq :collection.IndexedSeq[T], from :Int, until :Int) :IndexedSeqIterator[T] =
		new IndexedSeqIterator(seq, 0, seq.length)
}




/** An iterator advancing over a slice of an `IndexedSeq` in the reverse direction.
  * @param last      the index in the sequence `last <= first++` of the last element to return
  *                  (the first index of the slice).
  * @param `first++` the index in the sequence pointing directly after the first/next element to return
  *                  (the end index of the slice).
  */ //consider: renaming to IndexedSeqReverseIterator
private sealed class ReverseIndexedSeqIterator[+T] private[collections]
	                                          (seq :collection.IndexedSeqOps[T, generic.Any1, _],
	                                           private[this] var last :Int, private[this] var `first++` :Int)
	extends AbstractIterator[T] with ReverseIndexedIterator[T]
{
	def this(seq :collection.IndexedSeqOps[T, generic.Any1, _], idx :Int) = this(seq, 0, idx)
	def this(seq :collection.IndexedSeqOps[T, generic.Any1, _]) = this(seq, 0, seq.length)

	private def underlying = seq
	protected final override def underlyingSize :Int = seq.length
	protected final override def index :Int = `first++`
	protected final override def index_=(i :Int) :Unit = `first++` = i
	protected final override def limit :Int = last
	protected final override def limit_=(i :Int) :Unit = last = i

	final override def hasNext :Boolean = `first++` > last
	override def head :T = seq(`first++` - 1)
	override def next() :T = {
		if (`first++` <= last)
			noSuch_!("Index " + `first++` + " has reached the lower bound of " + last + ".")
		`first++` -= 1
		seq(`first++`)
	}

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :ReverseIndexedSeqIterator[_] =>
			(underlying.asAnyRef eq other.underlying.asAnyRef) && index == other.index && limit == other.limit
		case _ => false
	}
	override def hashCode :Int = ReversedSeq(seq.toIndexedSeq.slice(last, `first++`)).hashCode
	override def clone = new ReverseIndexedSeqIterator(seq, last, `first++`)
}


@SerialVersionUID(Ver)
private object ReverseIndexedSeqIterator {
	def apply[T](seq :collection.IndexedSeq[T]) :ReverseIndexedSeqIterator[T] =
		new ReverseIndexedSeqIterator(seq, 0, seq.length)

	/** An iterator returning elements `seq(first), seq(first - 1), ..., seq(first - length + 1)`.
	  * If the iterator would need to access an element at index lesser than zero, the excess elements are ignored.
	  * Negative `length` is the same as zero.
	  */
    @throws[IndexOutOfBoundsException]("if first is negative or greater or equal to the length of the sequence")
	def apply[T](seq :collection.IndexedSeq[T], first :Int, length :Int) :ReverseIndexedSeqIterator[T] = {
		val len = seq.length
		if (first < 0 | first >= len)
			outOfBounds_!(first.toString + " is out of bounds [0, " + len + ")")
		val downTo = first + 1 - math.min(first + 1, math.max(length, 0))
		new ReverseIndexedSeqIterator(seq, downTo, first + 1)
	}

	/** An iterator returning elements `seq(hi - 1), seq(hi - 2), ..., seq(lo)`.
	  * If any of the indices in the `[lo, hi)` range is out of bounds for `seq`, it is ignored.
	  */
	def slice[T](seq :collection.IndexedSeq[T], lo :Int, hi :Int) :ReverseIndexedSeqIterator[T] = {
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
	  * If reading would go past the end of `string`, the excess index range is ignored. Negative `length`
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
	  * If the iterator would need to access an element at index lesser than zero, the excess elements are ignored.
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

