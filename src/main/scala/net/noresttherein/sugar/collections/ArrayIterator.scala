package net.noresttherein.sugar.collections

import java.lang.{Math => math}

import scala.collection.immutable.WrappedString
import scala.collection.{AbstractIterator, BufferedIterator}

import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.collections.extensions.ArrayObjectExtension
import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.typist.casting.extensions.{castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.outOfBounds_!




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
			throw new NoSuchElementException("Index " + index + " exceeds the limit of " + limit + ".")
		val res = head
		index += 1
		res
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

	override def clone :AbstractIndexedIterator[T] = super.clone.asInstanceOf[AbstractIndexedIterator[T]]

	override def toString :String = clone.mkString("Iterator(", ",", ")")
}


@SerialVersionUID(Ver)
private object AbstractIndexedIterator {
	def fix[E](iterator :AbstractIndexedIterator[E]) :iterator.type = {
		iterator.adjustRange()
		iterator
	}
	def validated[E](iterator :AbstractIndexedIterator[E]) :iterator.type = {
		iterator.validateRange()
		iterator
	}
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
			throw new NoSuchElementException("Index " + index + " reached the lower bound of " + limit + ".")
		val hd = head
		index -= 1
		hd
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


	override def clone :AbstractIndexedReverseIterator[T] =
		super.clone.asInstanceOf[AbstractIndexedReverseIterator[T]]

	override def toString :String = clone.mkString("Iterator(", ",", ")")
}


@SerialVersionUID(Ver)
private object AbstractIndexedReverseIterator {
	def fix[E](iterator :AbstractIndexedReverseIterator[E]) :iterator.type = {
		iterator.adjustRange()
		iterator
	}
	def validated[E](iterator :AbstractIndexedReverseIterator[E]) :iterator.type = {
		iterator.validateRange()
		iterator
	}
}




//todo: add mutable/immutable flag
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

	private[collections] final override def unsafeArray :Array[_] = array
	private[collections] final override def startIndex :Int = first
	private[collections] final override def isImmutable :Boolean = false

	protected final override def underlyingSize :Int = array.length
	final override def index :Int = first
	protected final override def index_=(i :Int) :Unit = first = i
	final override def limit :Int = `last++`
	protected final override def limit_=(i :Int) :Unit = `last++` = i

	def reverse :ReverseArrayIterator[T] = new ReverseArrayIterator[T](array, first, `last++`)

	final override def hasNext :Boolean = first < `last++`
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

	final override def foldLeft[A](z :A)(op :(A, T) => A) :A = {
		var res = z
		while (first < `last++`) {
			res = op(res, array(first))
			first += 1
		}
		res
	}

	final override def copyToArray[A >: T](xs :Array[A], start :Int, len :Int) :Int = {
		if (len <= 0 || first >= `last++` || start >= xs.length)
			0
		else if (start < 0)
			throw new IndexOutOfBoundsException(start.toString + " out of [0, " + xs.length + ")")
		else {
			val copied = math.min(len, math.min(`last++` - first, xs.length - start))
			ArrayLike.copy(array, first, xs, start, copied)
			first += copied
			copied
		}
	}


	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :ArrayIterator[_] => (array eq other.unsafeArray) && index == other.index && limit == other.limit
		case _ => false
	}
	override def hashCode :Int = ArrayLike.Wrapped.Slice(array, first, `last++`).hashCode
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

	/** Returns elements `array(first), array(first + 1), ..., array(first + length - 1)` of the given array.
	  * If reading would go past the end of the array, the excess index range is ignored. Negative `length`
	  * is equivalent to zero.
	  */
	@throws[IndexOutOfBoundsException]("if offset is negative or greater than the length of the array.")
	def apply[T](array :Array[T], first :Int, length :Int) :ArrayIterator[T] = {
		val len   = array.length
		if (first < 0 | first > len)
			throw new IndexOutOfBoundsException(first.toString + " is out of bounds [0, " + len + ")")
		val until = first + math.min(len - first, math.max(length, 0))
		make(array, first, until)
	}

	def from[T](array :Array[T], first :Int) :ArrayIterator[T] =
		if (first >= array.length) make(array, array.length, array.length)
		else if (first <= 0) make(array, 0, array.length)
		else make(array, first, array.length)

	/** Returns elements `array(from), array(from + 1), ..., array(until - 1)` of the given array.
	  * If any of indices in the `[from, until)` range are negative or greater than the array's length, they are ignored.
	  */
//	def over[T](array :Array[T], from :Int, until :Int) :ArrayIterator[T] =
//		AbstractIndexedIterator.fix(make(array, from, until))
	def over[T](array :Array[T], from :Int, until :Int) :ArrayIterator[T] = {
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

	/** Same as `ArrayIterator(array, first, length)`, but always produces an erased, not specialized instance. */
	def generic[T](array :Array[T], first :Int, length :Int) :ArrayIterator[T] = {
		val len = array.length
		if (first < 0 | first > len)
			throw new IndexOutOfBoundsException(first.toString + " is out of bounds [0, " + len + ")")
		val until = first + math.min(len - first, math.max(length, 0))
		new ArrayIterator[T](array, first, until)
	}

}




/** An iterator advancing over a slice of an array in the reverse direction.
  * @param last      the index in the array `last <= first++` of the last element to return
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

	final override def hasNext :Boolean = `first++` > last
	override def head :T =
		if (`first++` > last) array(`first++` - 1)
		else throw new NoSuchElementException("Index " + `first++` + " reached the lower bound of " + last + ".")

	override def next() :T = {
		if (`first++` <= last)
			throw new NoSuchElementException("Index " + `first++` + " reached the lower bound of " + last + ".")
		`first++` -= 1
		array(`first++`)
	}

	final override def foldLeft[A](z :A)(op :(A, T) => A) :A = {
		var res = z
		while (last < `first++`) {
			`first++` -= 1
			res = op(res, array(`first++`))
		}
		res
	}

	final override def copyToArray[B >: T](xs :Array[B], start :Int, len :Int) :Int =
		if (len <= 0 || `first++` <= last || start >= xs.length)
			0
		else if (start < 0)
			throw new IndexOutOfBoundsException(
				errorString(this) + ".copyToArray(" + errorString(xs) + ", " + start + ", " + len + ")"
			)
		else {
			val copied = math.min(size, math.min(len, xs.length - start))
			val end = `first++` - copied
			var i = start
			while (`first++` > end) {
				`first++` -= 1
				xs(i) = array(`first++`)
				i += 1
			}
			copied
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

	/** An iterator returning elements `seq(first), seq(first - 1), ..., seq(first - length + 1)`.
	  * If the iterator would need to access an element at index lesser than zero, the excess elements are ignored.
	  * Negative `length` is the same as zero.
	  */
	@throws[IndexOutOfBoundsException]("if offset is negative or greater or equal to the length of the array")
	def apply[@specialized(Int, Long, Double, AnyRef) T]
	         (array :Array[T], first :Int, length :Int) :ReverseArrayIterator[T] =
	{
		val len = array.length
		if (first < 0 | first >= len)
			throw new IndexOutOfBoundsException(first.toString + " is out of bounds [0, " + len + ")")
		val downTo = first + 1 - math.min(first + 1, math.max(length, 0))
		make(array, downTo, first + 1)
	}

	/** An iterator returning elements `array(hi - 1), array(hi - 2), ..., array(lo)`.
	  * If any of the indices in the `[lo, hi)` range is out of bounds for `array`, it is ignored.
	  */
	def over[T](array :Array[T], lo :Int, hi :Int) :ReverseArrayIterator[T] = {
		val len = array.length
		if (hi <= 0) make(array, 0, 0)
		else if (lo >= len) make(array, len, len)
		else if (lo <= 0 && hi >= len) make(array, 0, len)
		else if (lo <= 0) make(array, 0, hi)
		else if (hi >= len) make(array, lo, len)
		else if (lo >= hi) make(array, hi, hi)
		else make(array, lo, hi)
	}

	/** Same as `ReverseArrayIterator(array)`, but always produces an erased, not specialized iterator. */
	def generic[T](array :Array[T]) :ReverseArrayIterator[T] = new ReverseArrayIterator[T](array, 0, array.length)

	/** Same as `ReverseArrayIterator(array, first, length)`, but always produces an erased, not specialized iterator. */
	def generic[T](array :Array[T], first :Int, length :Int) :ReverseArrayIterator[T] = {
		val len = array.length
		if (first < 0 | first >= len)
			throw new IndexOutOfBoundsException(first.toString + " is out of bounds [0, " + len + ")")
		val downTo = first + 1 - math.min(first + 1, math.max(length, 0))
		new ReverseArrayIterator[T](array, downTo, first + 1)
	}
}






/** An iterator advancing over a slice of an `IndexedSeq`.
  * @param first    the index in the sequence of the first/next element to return.
  * @param `last++` the index in the sequence delimiting the iterator, that is pointing after the last element
  *                 the iterator should return.
  */
sealed class IndexedSeqIterator[@specialized(Int, Long, Double, AnyRef) +T] private[collections]
	         (seq :collection.IndexedSeqOps[T, generic.Any, _],
	          private[this] var first :Int, private[this] var `last++` :Int)
	extends AbstractIterator[T] with AbstractIndexedIterator[T]
{
	def this(seq :collection.IndexedSeqOps[T, generic.Any, _], idx :Int) = this(seq, idx, seq.length)
	def this(seq :collection.IndexedSeqOps[T, generic.Any, _]) = this(seq, 0, seq.length)

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
			throw new NoSuchElementException("Index " + first + " exceeds the upper bound of " + `last++` + ".")
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
object IndexedSeqIterator {
	def apply[@specialized(Int, Long, Double, AnyRef) T](seq :collection.IndexedSeq[T]) :IndexedSeqIterator[T] =
		new IndexedSeqIterator(seq, 0, seq.length)

	def from[@specialized(Int, Long, Double, AnyRef) T](seq :collection.IndexedSeq[T], first :Int) :IndexedSeqIterator[T] =
		if (first >= seq.length) new IndexedSeqIterator(seq, seq.length, seq.length)
		else if (first <= 0) new IndexedSeqIterator(seq, 0, seq.length)
		else new IndexedSeqIterator(seq, first, seq.length)

	/** Returns elements `seq(first), seq(first + 1), ..., seq(first + length - 1)` of the given sequence.
	  * If reading would go past the end of the sequence, the excess index range is ignored. Negative `length`
	  * is equivalent to zero.
	  */
	@throws[IndexOutOfBoundsException]("if offset is negative or greater than the length of the sequence.")
	def apply[@specialized(Int, Long, Double, AnyRef) T]
	         (seq :collection.IndexedSeq[T], first :Int, length :Int) :IndexedSeqIterator[T] =
	{
		val len   = seq.length
		if (first < 0 | first > len)
			throw new IndexOutOfBoundsException(first.toString + " is out of bounds [0, " + len + ")")
		val until = first + math.min(len - first, math.max(length, 0))
		new IndexedSeqIterator(seq, first, until)
	}

	/** Returns elements `seq(from), seq(from + 1), ..., seq(until - 1)` of the given sequence.
	  * If any of indices in the `[from, until)` range are negative or greater than the sequence's length,
	  * they are ignored.
	  */
	def over[@specialized(Int, Long, Double, AnyRef) T]
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
  * @param last      the index in the sequence `last <= first++` of the last element to return
  *                  (the first index of the slice).
  * @param `first++` the index in the sequence pointing directly after the first/next element to return
  *                  (the end index of the slice).
  */
sealed class ReverseIndexedSeqIterator[@specialized(Int, Long, Double, AnyRef) +T] private[collections]
	         (seq :collection.IndexedSeqOps[T, generic.Any, _],
	          private[this] var last :Int, private[this] var `first++` :Int)
	extends AbstractIterator[T] with AbstractIndexedReverseIterator[T]
{
	def this(seq :collection.IndexedSeqOps[T, generic.Any, _], idx :Int) = this(seq, 0, idx)
	def this(seq :collection.IndexedSeqOps[T, generic.Any, _]) = this(seq, 0, seq.length)

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
			throw new NoSuchElementException("Index " + `first++` + " has reached the lower bound of " + last + ".")
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
object ReverseIndexedSeqIterator {
	def apply[@specialized(Int, Long, Double, AnyRef) T](seq :collection.IndexedSeq[T]) :ReverseIndexedSeqIterator[T] =
		new ReverseIndexedSeqIterator(seq, 0, seq.length)

	/** An iterator returning elements `seq(first), seq(first - 1), ..., seq(first - length + 1)`.
	  * If the iterator would need to access an element at index lesser than zero, the excess elements are ignored.
	  * Negative `length` is the same as zero.
	  */
    @throws[IndexOutOfBoundsException]("if first is negative or greater or equal to the length of the sequence")
	def apply[@specialized(Int, Long, Double, AnyRef) T]
	         (seq :collection.IndexedSeq[T], first :Int, length :Int) :ReverseIndexedSeqIterator[T] =
	{
		val len = seq.length
		if (first < 0 | first >= len)
			throw new IndexOutOfBoundsException(first.toString + " is out of bounds [0, " + len + ")")
		val downTo = first + 1 - math.min(first + 1, math.max(length, 0))
		new ReverseIndexedSeqIterator(seq, downTo, first + 1)
	}

	/** An iterator returning elements `seq(hi - 1), seq(hi - 2), ..., seq(lo)`.
	  * If any of the indices in the `[lo, hi)` range is out of bounds for `seq`, it is ignored.
	  */
	def over[@specialized(Int, Long, Double, AnyRef) T]
	        (seq :collection.IndexedSeq[T], lo :Int, hi :Int) :ReverseIndexedSeqIterator[T] =
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
			throw new IndexOutOfBoundsException(first.toString + " is out of bounds [0, " + len + ")")
		val until = first + math.min(len - first, math.max(length, 0))
		new StringIterator(string, first, until)
	}

	/** Returns characters `string(from), string(from + 1), ..., string(until - 1)` of the given string.
	  * If any of indices in the `[from, until)` range are negative or greater than the string's length, they are ignored.
	  */
	def over(string :String, from :Int, until :Int) :StringIterator = {
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

	/** An iterator returning elements `seq(first), seq(first - 1), ..., seq(first - length + 1)`.
	  * If the iterator would need to access an element at index lesser than zero, the excess elements are ignored.
	  * Negative `length` is the same as zero.
	  */
	@throws[IndexOutOfBoundsException]("if first is negative or greater or equal to the string's length")
	def apply(string :String, first :Int, length :Int) :ReverseStringIterator = {
		val len = string.length
		if (first < 0 | first >= len)
			throw new IndexOutOfBoundsException(first.toString + " is out of bounds [0, " + len + ")")
		val downTo = first + 1 - math.min(first + 1, math.max(length, 0))
		new ReverseStringIterator(string, downTo, first + 1)
	}

	/** An iterator returning characters `string(hi - 1), string(hi - 2), ..., string(lo)`.
	  * If any of the indices in the `[lo, hi)` range is out of bounds for `string`, it is ignored.
	  */
	def over(string :String, lo :Int, hi :Int) :ReverseStringIterator = {
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









/** An iterator advancing over an array, potentially wrapping over the end of the array back to the beginning.
  * @param idx       the index in the array of the first/next element to return.
  * @param remaining the remaining number of elements to iterate over.
  */ //no SerialVersionUID because we are not Serializable :)
sealed class CyclicArrayIterator[@specialized(Int, Long, Double, AnyRef) +T] private[collections]
	                            (array :Array[T], private[this] var idx :Int, private[this] var remaining :Int)
	extends AbstractIterator[T] with AbstractIndexedIterator[T]
{
//	def this(array :Array[T], idx :Int) = this(array, idx, array.length)
	def this(array :Array[T]) = this(array, 0, array.length)

	private[this] val len = array.length
	private def unsafeArray :Array[_] = array

	protected final override def underlyingSize :Int = len
	final override def index :Int = idx
	protected final override def index_=(i :Int) :Unit = idx = i % len
	final override def limit :Int = idx + remaining
	protected final override def limit_=(i :Int) :Unit = remaining = i - idx

//	def reverse :ReverseCyclicArrayIterator[T] = new ReverseCyclicArrayIterator[T](array, (idx + remaining) % len, len)

	final override def knownSize :Int = remaining
	final override def hasNext :Boolean = remaining > 0
	override def head :T =
		if (remaining > 0) array(idx)
		else throw new NoSuchElementException("Index has reached the limit of " + idx + ".")

	override def next() :T = {
		if (remaining <= 0)
			throw new NoSuchElementException("Index has reached the limit of " + idx + ".")
		val res = array(idx)
		remaining -= 1
		idx = (idx + 1) % len
		res
	}

	override def foldLeft[A](z :A)(op :(A, T) => A) :A = {
		var res  = z
		while (remaining > 0) {
			res = op(res, array(idx))
			idx = (idx + 1) % len
			remaining -= 1
		}
		res
	}


	override def copyToArray[B >: T](xs :Array[B], start :Int, len :Int) :Int = {
		if (len <= 0 | remaining <= 0 || start >= xs.length)
			0
		else if (start < 0)
			throw new IndexOutOfBoundsException(start.toString + " out of [0, " + xs.length + ")")
		else {
			val copied = math.min(len, math.min(remaining, xs.length - start))
			Array.cyclicCopy(array, idx, xs, start, copied)
			idx = (idx + copied) % this.len
			remaining -= copied
			copied
		}
	}

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :CyclicArrayIterator[_] =>
			(array eq other.unsafeArray) && index == other.index && limit == other.limit
		case _ => false
	}
//	override def hashCode :Int = ArrayLike.Wrapped.Slice(array, idx, `last++`).hashCode
	override def hashCode :Int = (System.identityHashCode(array) * 31 + idx) * 31 + remaining
	override def clone = new CyclicArrayIterator(array, idx, remaining)
}


/** A factory of iterators advancing over array slices. */
@SerialVersionUID(Ver)
object CyclicArrayIterator {
	private def make[T](array :Array[T], offset :Int, length :Int) :CyclicArrayIterator[T] =
		((array :Array[_]) match {
			case a :Array[AnyRef]  => new CyclicArrayIterator(a, offset, length)
			case a :Array[Int]     => new CyclicArrayIterator(a, offset, length)
			case a :Array[Long]    => new CyclicArrayIterator(a, offset, length)
			case a :Array[Double]  => new CyclicArrayIterator(a, offset, length)
//			case a :Array[Byte]    => new CyclicArrayIterator(a, offset, length)
//			case a :Array[Char]    => new CyclicArrayIterator(a, offset, length)
//			case a :Array[Float]   => new CyclicArrayIterator(a, offset, length)
//			case a :Array[Short]   => new CyclicArrayIterator(a, offset, length)
//			case a :Array[Boolean] => new CyclicArrayIterator(a, offset, length)
			case _                 => new CyclicArrayIterator(array, offset, length)
		}).castParam[T]

	/** An iterator returning `length` elements of the array, starting with `array(offset)`. If `offset + length`
	  * is greater than the length of the array, then the iterator wraps to the beginning of the array,
	  * returning `array(0)` following `array(array.length - 1)`, and so on, until `min(length, array.length)`
	  * elements are returned. If `length` is negative, the iterator will have no elements.
	  */
	@throws[IndexOutOfBoundsException]("if offset is negative or greater than the length of the array.")
	def apply[T](array :Array[T], offset :Int, length :Int) :CyclicArrayIterator[T] = {
		val len   = array.length
		if (offset < 0 | offset > len)
			throw new IndexOutOfBoundsException(offset.toString + " out of bounds of [0, " + len + ")")
		make(array, offset, math.min(math.max(length, 0), len))
	}

	/** An iterator returning subsequent elements of an array, starting with index `from % array.length`,
	  * and increasing modulo the length of the array until index `until` is reached (exclusive).
	  */
	def over[T](array :Array[T], from :Int, until :Int) :CyclicArrayIterator[T] = {
		val len    = array.length
		val from0  = if (from < 0) len + from % len else from % len
		val until0 = if (until < 0) len + until % len else until % len
		make(array, (from + 1) % len, until0 - from0)
	}

	/** An iterator over `[offset, offset + length)` index range of `array`, going in reverse.
	  * The first returned element will be the one at index `offset + length - 1`, and the last one at index `from`.
	  * Negative length is treated the same as zero, and offset is interpreted modulo `array.length`.
	  */
//	@throws[IndexOutOfBoundsException]("if first is negative or greater or equal to the length of the array")
//	def reversed[T](array :Array[T], offset :Int, length :Int) :ReverseCyclicArrayIterator[T] =
//		ReverseCyclicArrayIterator(array, offset, length)

	/** Same as `CyclicArrayIterator(array, offset, length)`, but always produces an erased, not specialized instance. */
	@throws[IndexOutOfBoundsException]("if offset is negative or greater than the length of the array.")
	def generic[T](array :Array[T], offset :Int, length :Int) :CyclicArrayIterator[T] = {
		val len = array.length
		if (offset < 0 | offset > len)
			throw new IndexOutOfBoundsException(offset.toString + " out of bounds of [0, " + len + ")")
		new CyclicArrayIterator(array, offset, math.min(math.max(length, 0), len))
	}

}




/** An iterator advancing over an an array in the reverse direction, wrapping back at index `0`
  * back to the end of the array.
  * @param idx       the index in the array of the next element to return.
  * @param remaining the size of the iterator.
  */
private sealed class ReverseCyclicArrayIterator[@specialized(Int, Long, Double, AnyRef) +T] private[collections]
	                 (array :Array[T], private[this] var idx :Int, private[this] var remaining :Int)
	extends AbstractIterator[T] with AbstractIndexedReverseIterator[T]
{
//	def this(array :Array[T], idx :Int) = this(array, 0, idx)
	def this(array :Array[T]) = this(array, 0, array.length)

	private[this] val len = array.length
	private def unsafeArray :Array[_] = array

	protected final override def underlyingSize :Int = len
	final override def index :Int = idx
	protected final override def index_=(i :Int) :Unit = idx = if (i < 0) len + i else i
	final override def limit :Int = idx - remaining
	protected final override def limit_=(i :Int) :Unit = remaining = idx - i

	final override def knownSize :Int = remaining
	final override def hasNext :Boolean = remaining > 0
	override def head :T =
		if (remaining > 0)
			if (idx == 0) array(len - 1) else array(idx - 1)
		else
			throw new NoSuchElementException("Index has reached the lower bound of " + idx + ".")

	override def next() :T = {
		if (remaining < 0)
			throw new NoSuchElementException("Index has reached the lower bound of " + idx + ".")
		if (idx == 0) idx = len - 1 else idx -= 1
		remaining -= 1
		array(idx)
	}

	override def foldLeft[A](z :A)(op :(A, T) => A) :A = {
		var res = z
		while (remaining > 0) {
			if (idx == 0) idx = len - 1
			else idx -= 1
			res = op(res, array(idx))
			remaining -= 1
		}
		res
	}

	final override def copyToArray[B >: T](xs :Array[B], start :Int, len :Int) :Int =
		if (len <= 0 | remaining <= 0 || start >= xs.length)
			0
		else if (start < 0)
			throw new IndexOutOfBoundsException(
				errorString(this) + ".copyToArray(" + errorString(xs) + ", " + start + ", " + len + ")"
			)
		else {
			val copied = math.min(size, math.min(len, xs.length - start))
			val end = start + copied
			var i = start
			while (i < end) {
				idx -= 1
				xs(i) = array(idx)
				i += 1
			}
			remaining -= copied
			copied
		}

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :ReverseCyclicArrayIterator[_] =>
			(array eq other.unsafeArray) && index == other.index && limit == other.limit
		case _ => false
	}
	override def hashCode :Int = (System.identityHashCode(array) * 31 + idx) * 31 + remaining
	override def clone = new ReverseCyclicArrayIterator(array, idx, remaining)
}


/** A factory of iterators advancing over a slice of an array in the reverse direction. */
@SerialVersionUID(Ver)
private object ReverseCyclicArrayIterator {
	private def make[T](array :Array[T], offset :Int, length :Int) :ReverseCyclicArrayIterator[T] =
		((array :Array[_]) match {
			case a :Array[AnyRef]  => new ReverseCyclicArrayIterator(a, offset, length)
			case a :Array[Int]     => new ReverseCyclicArrayIterator(a, offset, length)
			case a :Array[Long]    => new ReverseCyclicArrayIterator(a, offset, length)
			case a :Array[Double]  => new ReverseCyclicArrayIterator(a, offset, length)
//			case a :Array[Byte]    => new ReverseCyclicArrayIterator(a, offset, length)
//			case a :Array[Char]    => new ReverseCyclicArrayIterator(a, offset, length)
//			case a :Array[Float]   => new ReverseCyclicArrayIterator(a, offset, length)
//			case a :Array[Short]   => new ReverseCyclicArrayIterator(a, offset, length)
//			case a :Array[Boolean] => new ReverseCyclicArrayIterator(a, offset, length)
			case _                 => new ReverseCyclicArrayIterator(array, offset, length)
		}).castParam[T]

	/** An iterator returning elements of an array in the decreasing index order, starting with `array(first)`.
	  * If `length > first`, then, following `array(0)`, the iterator returns `array(array.length - 1)`,
	  * `array(array.length - 2)`, and so on, until `min(length, array.length)` elements are returned.
	  * If `length` is negative, the iterator will have no elements.
	  */
	@throws[IndexOutOfBoundsException]("if first is negative or greater or equal to the length of the array")
	def apply[T](array :Array[T], first :Int, length :Int) :ReverseCyclicArrayIterator[T] = {
		val len = array.length
		if (first < 0 | first >= len)
			throw new IndexOutOfBoundsException(first.toString + " is out of bounds of [0, " + len + ")")
		make(array, first, math.min(len, math.max(length, 0)))
	}

	/** An iterator returning elements of an array at decreasing indices, starting with `(until - 1) % array.length`,
	  * and ending (inclusive) with `from % array.length`. If `from` is greater than `until` modulo
	  * the length of the array, the element at `0` is followed by elements at `array.length - 1, array.length - 2`, etc.
	  * If `from == until`, the iterator is empty.
	  */
	def over[T](array :Array[T], from :Int, until :Int) :ReverseCyclicArrayIterator[T] = {
		val len = array.length
		val from0  = if (from < 0) len + from % len else from % len
		val until0 = if (until < 0) len + until % len else until % len
		make(array, (until0 + len - 1) % len, until0 - from0)
	}

	/** Same as `ReverseCyclicArrayIterator(array, first, length)`, but always produces an erased,
	  * not specialized iterator.
	  */
	@throws[IndexOutOfBoundsException]("if first is negative or greater or equal to the length of the array")
	def generic[T](array :Array[T], first :Int, length :Int) :ReverseCyclicArrayIterator[T] = {
		val len = array.length
		if (first < 0 | first > len)
			throw new IndexOutOfBoundsException(first.toString + " is out of bounds of [0, " + len + ")")
		new ReverseCyclicArrayIterator(array, first, math.min(len, math.max(length, 0)))
	}
}

