package net.noresttherein.sugar.collections

import java.lang.{Math => math}

import scala.collection.immutable.WrappedString
import scala.collection.{AbstractIterator, BufferedIterator}

import net.noresttherein.sugar.casting.castingMethods
import net.noresttherein.sugar.collections.util.elementsToCopy
import net.noresttherein.sugar.exceptions.{??!, illegalState_!, noSuch_!, outOfBounds_!, unsupported_!}
import net.noresttherein.sugar.reflect.prettyprint.localClassNameOf
import net.noresttherein.sugar.typist.kinds
import net.noresttherein.sugar.typist.kinds.Any1




@SerialVersionUID(Ver)
private object IndexedIterator {
	def slice[S, E](input :S, from :Int, until :Int)(f :(S, Int) => E) :Iterator[E] =
		if (until <= from)
			Iterator.empty
		else
			new AbstractIndexedIterator[E](from, until) {
				override def head :E = f(input, index)
			}

	def apply[S, E](input :S, first :Int, length :Int)(f :(S, Int) => E) :Iterator[E] =
		if (length <= 0) Iterator.empty
		else slice(input, first, first + length)(f)


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
  * May be used as a base trait for circular iterators (wrapping at `rangeEnd`) by using the remainder of `index`,
  * rather than its absolute value, to identify the `head` element.
  * @note `index` and `limit` do not have to fall into range `[rangeStart, rangeEnd]`, and they can be even negative:
  *       it is the responsibility of subclasses to initialize them correctly and map to actual values
  *       in the collection. In particular, indices lesser than zero or greater than `underlyingSize`
  *       may be used to implement circular iterators which wrap at the end of the collection, by treating them modulo
  *       its size. This implementation however assumes that `index <= limit`, in particular it defines `knownSize`
  *       as `limit - index`. In extreme cases, `limit` may even overflow; the methods of this trait are resistant
  *       to it, as long as `limit - index` is non-negative.
  * @see [[net.noresttherein.sugar.collections.ReverseIndexedIterator]]
  * @author Marcin Mościcki
  */ //consider: throwing exceptions with constant strings as messages for performance
trait IndexedIterator[+E] extends BufferedIterator[E] with SugaredIterator[E] with HasFastSlice[E] with Cloneable {
	/** An optional lowest valid index in the underlying structure. Must be non-negative. Defaults to zero. */
	protected def rangeStart :Int = 0

	/** An optional upper (exclusive) limit on valid indexes in the underlying structure.
	  * Defaults to `rangeStart + underlyingSize`. Must be greater or equal to `rangeStart`.
	  */
	protected def rangeEnd :Int = rangeStart + underlyingSize

	/** An optional size of the underlying structure, used by `validateRange()` and `adjustRange()` methods,
	  * as well as some implementations.
	  */
	protected def underlyingSize :Int

	/** The index pointing at the next (`head`) element in the underlying structure. */
	protected var index :Int

	/** An index greater or equal `index` which defines the stop of iteration.
	  * The size of the iterator is defined as `limit - index` and `hasNext` condition as `index == limit`.
	  * In the latter case, only equality between the indices ends the iteration; this is to allow implementations
	  * of circular iterators.
	  */
	protected var limit :Int

	/** The [[net.noresttherein.sugar.collections.IndexedIterator.index index]] of the current element adjusted
	  * to the valid range of `[rangeStart, rangeEnd]`. This property is useful for circular iterator implementations
	  * which wrap at the end of the range back to its start.
	  * @return `rangeStart + (index - rangeStart) % underlyingSize`
	  */
	protected def indexInRange :Int = {
		val start = rangeStart
		val span  = underlyingSize
		val rem = (index - start) % span
		if (rem < 0) start + span + rem
		else start + rem
	}

	/** Moves the index forward by one. Overriding the method in a subclass eliminates the need to retrieve index value
	  * before updating it.
	  * @return the value of `index` from before the method was called.
	  */
	protected def advance() :Int = {
		val idx = index
		index = idx + 1
		idx
	}

	/** An optional convenience method for implementors which clips the current `index` and `limit` to
	  * `[rangeStart, rangeEnd]` range.
	  */
	protected def adjustRange() :Unit = {
		val min   = rangeStart
		val max   = rangeEnd
		val curr  = index
		var end   = limit
		val uend  = end + Int.MinValue //for unsigned comparison
		val ucurr = curr + Int.MinValue
		if (uend > max + Int.MinValue) {
			limit = max
			end   = max
		} else if (uend < min + Int.MinValue) {
			limit = min
			end   = min
		}
		if (ucurr > uend)
			index = end
		else if (ucurr < min + Int.MinValue)
			index = min
	}

	/** An optional convenience method for implementors which throws an [[IndexOutOfBoundsException]]
	  * if  `index` or `limit` are out of `[0, underlyingSize]` range.
	  */
	protected def validateRange() :Unit = {
		val min   = rangeStart + Int.MinValue
		val max   = rangeEnd + Int.MinValue
		val curr  = index + Int.MinValue
		val end   = limit + Int.MinValue
		if (end > max | end < min) //Compare unsigned.
			outOfBounds_!(limit, rangeStart, rangeEnd)
		if (curr > max | curr < min)
			outOfBounds_!(index, rangeStart, rangeEnd)
		if (end < curr)
			outOfBounds_!("End index " + end + " lower than start index " + curr + ".")
	}

	override def knownSize :Int = limit - index
	final override def size :Int = knownSize

	override def hasNext :Boolean = index != limit

	/** Returns the element at `index` in the underlying collection. Must implement the mandatory check
	  * if the element exists - the implementation of `next` assumes it has already been made,
	  * in order to avoid doing it twice. Failure to do so will lead to an illegal state of the iterator
	  * and undefined behaviour.
	  */
	override def head :E

	override def next() :E = {
		val res = head //head validates the index for us.
		advance()
		res
	}
	override def skip() :this.type = {
		if (!hasNext)
			unsupported_!(toString + ": index " + index + " equals the limit marking iteration end.")
		advance()
		this
	}

	override def take(n :Int) :Iterator[E] = {
		if (n <= 0) limit = index
		else if (n < knownSize) limit = index + n
		this
	}
	override def drop(n :Int) :Iterator[E] = {
		if (n >= knownSize) index = limit
		else if (n > 0) index += n
		this
	}
	override def strictDrop(n :Int) :Iterator[E] = drop(n)

	override def hasFastDrop :Boolean = true

	override def splitAt(n :Int) :(Iterator[E], Iterator[E]) = (clone.take(n), drop(n))

	override def slice(from :Int, until :Int) :Iterator[E] = take(until).drop(from)

	override def reduceLeft[U >: E](op :(U, E) => U) :U =
		if (hasNext) foldLeft[U](next())(op)
		else unsupported_!("Iterator().reduceLeft")

	override def reduceLeftOption[U >: E](op :(U, E) => U) :Option[U] = if (hasNext) Some(reduceLeft(op)) else None

	override def clone :IndexedIterator[E] = super.clone.asInstanceOf[IndexedIterator[E]]

	protected def className :String = localClassNameOf(this)

	override def toString :String = {
		val range =
			if (underlyingSize < 0) ""
			else if (rangeStart == 0) "/" + underlyingSize
			else "/[" + rangeStart + ", " + rangeEnd + "]"
		className + "|" + knownSize + "|@" + index + range
	}
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
abstract class AbstractIndexedIterator[+E](private[this] var idx :Int, private[this] var end :Int)
	extends AbstractSugaredIterator[E] with IndexedIterator[E]
{
	protected override def underlyingSize :Int = -1
	protected final override def index :Int = idx
	protected final override def index_=(value :Int) :Unit = idx = value
	protected final override def limit :Int = end
	protected final override def limit_=(value :Int) :Unit = end = value
	protected final override def advance() :Int = { val i = idx; idx = i + 1; i }

	/** Returns `limit - index`. */
	final override def knownSize :Int = end - idx

	/** Returns `index < limit`. */
	final override def hasNext :Boolean = idx < end

	/** Returns `head` and increases `index`. */
	override def next() :E = {
		val res = head
		idx += 1
		res
	}
}




/** An `IndexedIterator` which, instead of keeping an upper index bound, defines the iteration end by maintaining
  * the number of [[net.noresttherein.sugar.collections.CountdownIterator.remaining remaining]] elements.
  */
trait CountdownIterator[+E] extends IndexedIterator[E] {
	protected var remaining :Int
	protected final override def limit :Int = index + remaining
	protected final override def limit_=(value :Int) :Unit = remaining = value - index
	protected def remaining_--() :Unit = remaining -= 1
	protected override def advance() :Int = {
		val idx = index
		index   = idx
		remaining -= 1
		idx
	}
	final override def knownSize :Int = remaining
	override def hasNext :Boolean = remaining > 0

	protected override def adjustRange() :Unit = {
		val min   = rangeStart
		val max   = rangeEnd
		val span  = max - min
		var idx   = index
		val size  = remaining
		if (max < min | span != underlyingSize)
			illegalState_!(
				toString + " cannot iterate in range [" + min + ", " + max + ") of length " + underlyingSize + "."
			)
		if (size < 0)
			remaining = 0
		if (idx > max) {
			idx   = max
			index = idx
		} else if (idx < min) {
			idx   = min
			index = idx
		}
		if (size > max - idx)
			remaining = max - idx
	}

	protected override def validateRange() :Unit = {
		val min   = rangeStart
		val max   = rangeEnd
		val span  = underlyingSize
		val size  = remaining
		val idx   = index
		if (min > max | span != max - min)
			illegalState_!(
				toString + " cannot iterate in range [" + min + ", " + max + ") of length " + span + "."
			)
		if (idx < min | idx > max)
			outOfBounds_!(toString + " start index " + idx + " out of range [" + min + ", " + max + "].")
		if (remaining < 0 | remaining > max - idx)
			outOfBounds_!(toString + " iterator size " + size + " out of range [0, " + span + "].")
	}

	override def skip() :this.type = {
		val rem = remaining
		if (rem <= 0)
			noSuch_!(toString + ".skip()")
		advance()
		this
	}

	override def take(n :Int) :Iterator[E] = {
		if (n <= 0)
			remaining = 0
		else if (n < remaining)
			remaining = n
		this
	}
	override def drop(n :Int) :Iterator[E] = {
		if (n > 0) {
			val rem = remaining
			val dropped = math.min(rem, n)
			index += dropped
			remaining = rem - dropped
		}
		this
	}
}


/** Base class for iterators relying on an increasing index to return their elements from some underlying structure.
  * Maintains the [[net.noresttherein.sugar.collections.IndexedIterator.index index]] value and
  * a [[net.noresttherein.sugar.collections.CountdownIterator.remaining counter]] of remaining elements,
  * leaving only [[net.noresttherein.sugar.collections.IndexedIterator.head head]] to implement by subclasses.
  * This class does not validate its constructor arguments: subclasses must do so either before passing them here,
  * or in their own constructor through the accessors for `index` and `remaining`.
  * In order to use methods [[net.noresttherein.sugar.collections.CountdownIterator.adjustRange adjustRange]]
  * and [[net.noresttherein.sugar.collections.CountdownIterator.validateRange validateRange]] of `CountdownIterator`
  * for this purpose, the subclass must additionally define at least
  * [[net.noresttherein.sugar.collections.IndexedIterator.underlyingSize underlyingSize]]
  * (and, optionally [[net.noresttherein.sugar.collections.IndexedIterator.rangeStart rangeStart]], if the lowest
  * valid index is greater than zero).
  * @param idx       the index of the first element to return - value of property `index`.
  * @param countdown the initial size of the iterator, and the value of `remaining` property.
  */
abstract class AbstractCountdownIterator[+E](private[this] var idx :Int, private[this] var countdown :Int)
	extends AbstractSugaredIterator[E] with CountdownIterator[E]
{
//	protected override def underlyingSize :Int = -1
	protected final override def index :Int = idx
	protected final override def index_=(value :Int) :Unit = idx = value
	protected final override def advance() :Int = {
		val i = idx
		idx = i + 1
		countdown -= 1
		i
	}
	protected final override def remaining :Int = countdown
	protected final override def remaining_=(value :Int) :Unit = countdown = value
	protected final override def remaining_--() :Unit = countdown -= 1
	final override def hasNext :Boolean = countdown > 0

	override def next() :E = {
		val res = head
		countdown -= 1
		idx += 1
		res
	}
}


/** A `CountdownIterator` which wraps at the
  * [[net.noresttherein.sugar.collections.AbstractCyclicIterator.rangeEnd end of the range]] back to
  * the [[net.noresttherein.sugar.collections.IndexedIterator.rangeStart beginning]].
  * The index [[net.noresttherein.sugar.collections.AbstractCyclicIterator.limit marking the end of the iteration]]
  * may be lower than `index`. If iteration after wrapping should start at an index other than zero,
  * the subclass should override [[net.noresttherein.sugar.collections.IndexedIterator.rangeStart rangeStart]].
  * Because in a full circular buffer condition `index == limit` may define both an empty iterator and a full one,
  * the end of iteration is marked by a counter of remaining elements, rather than an end index,
  * like in a regular `IndexedIterator`. Subclasses are responsible for validating the arguments passed
  * to the constructor, or adjusting `index` and `remaining` in their constructor.
  * Methods [[net.noresttherein.sugar.collections.CountdownIterator.adjustRange adjustRange]] and
  * [[net.noresttherein.sugar.collections.CountdownIterator.validateRange validateRange]] are provided for this purpose.
  *
  * The only method remaining for a subclass to implement (assuming lower index bound is zero) is
  * [[net.noresttherein.sugar.collections.IndexedIterator.head head]].
  * @param idx        The index of the first element to return, and the value of
  *                   [[net.noresttherein.sugar.collections.IndexedIterator.index index]] property.
  *                   Must be `rangeStart <= idx < rangeEnd`, or the subclass must adjust `index` in its constructor.
  * @param countdown  The initial size of the iterator, and the variable backing property
  *                   [[net.noresttherein.sugar.collections.CountdownIterator.remaining remaining]].
  *                   Must be non-negative.
  * @param upperBound The index at which iteration will wrap back to the beginning, and the value of
  *                   [[net.noresttherein.sugar.collections.IndexedIterator.rangeEnd rangeEnd]] property
  *                   (and [[net.noresttherein.sugar.collections.IndexedIterator.underlyingSize underlyingSize]],
  *                   unless the subclass overrides `rangeStart`). Must be greater than zero.
  */
abstract class AbstractCyclicIterator[+E](private[this] var idx :Int, private[this] var countdown :Int, upperBound :Int)
	extends AbstractSugaredIterator[E] with CountdownIterator[E]
{
	protected override def underlyingSize :Int = upperBound - rangeStart
	protected final override def index :Int = idx
	protected final override def index_=(value :Int) :Unit = {
		idx = value
		if (value + Int.MinValue >= upperBound + Int.MinValue) {
			val lowerBound = rangeStart
			val range = upperBound - lowerBound
			if (range == 0)
				idx = 0
			else
				idx = lowerBound + (((value & 0xffffffffL) - lowerBound) % range).toInt
		}
	}
	protected final override def advance() :Int = {
		val i = idx
		idx = i + 1
		if (i == upperBound - 1)
			idx = rangeStart
		countdown -= 1
		i
	}
	protected final override def remaining :Int = countdown
	protected final override def remaining_=(value :Int) :Unit = countdown = value
	protected final override def remaining_--() :Unit = countdown -= 1
	protected final override def rangeEnd :Int = upperBound
//	protected final override def knownSize :Int = countdown
	final override def hasNext :Boolean = countdown > 0
	override def next() :E = {
		val res = head
		val i = idx + 1
		idx = i
		if (i == upperBound)
			idx = rangeStart
		countdown -= 1
		res
	}
}






@SerialVersionUID(Ver)
private object ReverseIndexedIterator {
	def slice[S, E](input :S, from :Int, until :Int)(f :(S, Int) => E) :Iterator[E] =
		if (until <= from)
			Iterator.empty
		else
			new AbstractReverseIndexedIterator[E](from - 1, until - 1) {
				override def head :E = f(input, index)
			}

	def apply[S, E](input :S, first :Int, length :Int)(f :(S, Int) => E) :Iterator[E] =
		if (length <= 0) Iterator.empty
		else slice(input, first - length + 1, first + 1)(f)

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
  * May be used as a base trait for circular iterators (wrapping at `rangeStart`) by using the remainder of `index`,
  * rather than its absolute value, to identify the `head` element.
  * @note `index` and `limit` do not have to fall into range `[rangeStart, rangeEnd]`, and they can be even negative:
  *       it is the responsibility of subclasses to initialize them correctly and map to actual values
  *       in the collection. In particular, indices lesser than zero or greater than `underlyingSize`
  *       may be used to implement circular iterators which wrap at the end of the collection, by treating them modulo
  *       its size. This implementation however assumes that `limit <= index`, in particular it defines `knownSize`
  *       as `index - limit`. In extreme cases, `limit` may even underflow; the methods of this trait are resistant
  *       to it, as long as `index - limit` is non-negative.
  * @see [[net.noresttherein.sugar.collections.IndexedIterator]]
  */
trait ReverseIndexedIterator[+E] extends IndexedIterator[E] {
	/** An index lesser or equal `index` which defines the stop of iteration. It points immediately ''before''
	  * the last element returned by the iterator. The size of the iterator is defined as `index - limit` and `hasNext`
	  * condition as `index == limit`. In the latter case, only equality between the indices ends the iteration.
	  */
	protected override var limit :Int

	/** Decreases the value of the `index` property. Overriding this method in a concrete subclass will let
	  * methods of this trait avoid reading its value from a getter before the update.
	  * @return the value of `index` from before the method was called.
	  */
	protected override def advance() :Int = {
		val idx = index
		index = idx - 1
		idx
	}
//
//	protected override def indexInRange :Int = {
//		val start = rangeStart
//		start + (index - start) % underlyingSize
//	}

	/** An optional convenience method for implementors which clips the current `index` and `limit` to
	  * `[rangeStart - 1, rangeEnd - 1]` range.
	  */
	protected override def adjustRange() :Unit = {
		val min   = rangeStart - 1
		val max   = rangeEnd - 1
		val end   = limit
		var curr  = index
		if (curr > max) {
			index = max
			curr  = max
		} else if (curr < min) {
			index = min
			curr  = min
		}
		if (end > curr)
			limit = curr
		else if (end < min)
			limit = min
	}

	/** An optional convenience method for implementors which throws an [[IndexOutOfBoundsException]]
	  * if  `index` or `limit` are out of `[rangeStart - 1, rangeEnd - 1]` range.
	  */
	protected override def validateRange() :Unit = {
		val min   = rangeStart - 1
		val max   = rangeEnd - 1
		val curr  = index
		val end   = limit
		if (end > max | end < min)
			outOfBounds_!(end, min, max)
		else if (curr > max | curr < min)
			outOfBounds_!(curr, min, max)
	}

	override def knownSize :Int = index - limit

	override def take(n :Int) :Iterator[E] = {
		if (n <= 0) limit = index
		else if (n < knownSize) limit += knownSize - n
		this
	}
	override def drop(n :Int) :Iterator[E] = {
		if (n >= knownSize) index = limit
		else if (n > 0) index -= n
		this
	}

	override def clone :ReverseIndexedIterator[E] =
		super.clone.asInstanceOf[ReverseIndexedIterator[E]]
}


/** Base class for iterators returning elements in the decreasing index order at some structure with random indexing.
  * Requires of the subclasses to only implement `head`, which is the element
  * at [[net.noresttherein.sugar.collections.ReverseIndexedIterator.index index]] in the underlying collection.
  * This class does not perform any validation of its arguments. Subclasses which wish to check the index range
  * may do so by calling either [[net.noresttherein.sugar.collections.ReverseIndexedIterator.adjustRange adjustRange]]
  * or [[net.noresttherein.sugar.collections.ReverseIndexedIterator.validateRange validateRange]], after implementing
  * [[net.noresttherein.sugar.collections.ReverseIndexedIterator.underlyingSize underlyingSize]]
  * (whose the default implementation is a stub).
  * @param first the index of the first returned element, exposed to subclasses as `index`.
  * @param end   the index one lesser than the index of the last returned element (may be `-1`). Must be lesser or equal
  *              `first`, or the subclass is otherwise responsible for adjusting it in its constructor.
  */
abstract class AbstractReverseIndexedIterator[+E](private[this] var end :Int, private[this] var first :Int)
	extends AbstractSugaredIterator[E] with ReverseIndexedIterator[E]
{
	protected override def underlyingSize :Int = -1
	protected final override def index :Int = first
	protected final override def index_=(value :Int) :Unit = first = value
	protected final override def advance() :Int = { val idx = first; first = idx - 1; idx }
	protected final override def limit :Int = end
	protected final override def limit_=(value :Int) :Unit = end = value

	/** Returns `index - limit`. */
	final override def knownSize :Int = first - end

	/** True if `index > limit`. */
	final override def hasNext :Boolean = first > end

	/** Returns `head` and decreases `index`. */
	override def next() :E = {
		val res = head
		first -= 1
		res
	}
	override def skip() :this.type = {
		if (first == end)
			unsupported_!(toString + ".skip()")
		first -= 1
		this
	}
}




/** A `ReverseIndexedIterator` which, instead of keeping a lower index bound, defines the iteration end by maintaining
  * the number of [[net.noresttherein.sugar.collections.CountdownIterator.remaining remaining]] elements.
  */ //consider: extending CountdownIterator. Some methods there would have to no longer be final, though.
trait ReverseCountdownIterator[+E] extends ReverseIndexedIterator[E] {
	protected var remaining :Int
	/** Decreases the value of the `remaining` property. Overriding this method in a concrete subclass will let
	  * methods of this trait avoid reading its value from a getter before the update.
	  */
	protected def remaining_--() :Unit = remaining -= 1

	/** Decreases ''both'' `index` and `remaining` properties by one. */
	protected override def advance() :Int = {
		val idx = index
		index = idx - 1
		remaining -= 1
		idx
	}
	protected final override def limit :Int = index - remaining
	protected final override def limit_=(value :Int) :Unit = remaining = index - value

	final override def knownSize :Int = remaining

	override def hasNext :Boolean = remaining > 0

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

	override def skip() :this.type = {
		val rem = remaining
		if (rem <= 0)
			noSuch_!(toString + ".next()")
		advance()
		this
	}
	override def take(n :Int) :Iterator[E] = {
		if (n <= 0)
			remaining = 0
		else if (n < remaining)
			remaining = n
		this
	}
	override def drop(n :Int) :Iterator[E] = {
		if (n > 0) {
			val rem = remaining
			val dropped = math.min(rem, n)
			index -= dropped
			remaining = rem - dropped
		}
		this
	}
}


/** Base class for iterators relying on a decreasing index to return their elements from some underlying structure.
  * Maintains the [[net.noresttherein.sugar.collections.ReverseIndexedIterator.index index]] value and
  * a [[net.noresttherein.sugar.collections.ReverseCountdownIterator.remaining counter]] of remaining elements,
  * leaving only [[net.noresttherein.sugar.collections.ReverseIndexedIterator.head head]] to implement by subclasses.
  * This class does not validate its constructor arguments: subclasses must do so either before passing them here,
  * or in their own constructor through the accessors for `index` and `remaining`.
  * In order to use methods [[net.noresttherein.sugar.collections.ReverseCountdownIterator.adjustRange adjustRange]]
  * and [[net.noresttherein.sugar.collections.ReverseCountdownIterator.validateRange validateRange]]
  * of `ReverseCountdownIterator` for this purpose, the subclass must additionally define at least
  * [[net.noresttherein.sugar.collections.IndexedIterator.underlyingSize underlyingSize]]
  * (and, optionally [[net.noresttherein.sugar.collections.IndexedIterator.rangeStart rangeStart]], if the lowest
  * valid index is greater than zero).
  * @param idx       the index of the first element to return - value of property `index`.
  * @param countdown the initial size of the iterator, and the value of `remaining` property.
  */
abstract class AbstractReverseCountdownIterator[+E](private[this] var idx :Int, private[this] var countdown :Int)
	extends AbstractSugaredIterator[E] with ReverseCountdownIterator[E]
{
//	protected override def underlyingSize :Int = -1
	protected final override def index :Int = idx
	protected final override def index_=(value :Int) :Unit = idx = value
	protected final override def advance() :Int = {
		val i = idx
		idx = i - 1
		countdown -= 1
		i
	}
	protected final override def remaining :Int = countdown
	protected final override def remaining_=(value :Int) :Unit = countdown = value
	protected final override def remaining_--() :Unit = countdown -= 1
	final override def hasNext :Boolean = countdown > 0

	override def next() :E = {
		val res = head
		idx -= 1
		countdown -= 1
		res
	}
}


/** A `ReverseCountdownIterator` which wraps at the lower end of the range back to the last element.
  * After the element at index [[net.noresttherein.sugar.collections.AbstractCyclicIterator.rangeStart rangeStart]]
  * is returned, index is set to [[net.noresttherein.sugar.collections.AbstractCyclicIterator.rangeEnd rangeEnd]]` - 1`.
  * Because in a full circular buffer condition `index == limit` may define both an empty iterator and a full one,
  * the end of iteration is marked by a counter of remaining elements, rather than an end index,
  * like in a regular `IndexedIterator`. Subclasses are responsible for validating the arguments passed
  * to the constructor, or adjusting `index` and `remaining` in their constructor.
  * Methods [[net.noresttherein.sugar.collections.CountdownIterator.adjustRange adjustRange]] and
  * [[net.noresttherein.sugar.collections.CountdownIterator.validateRange validateRange]] are provided for this purpose.
  *
  * The only method remaining for a subclass to implement (assuming lower index bound is zero) is
  * [[net.noresttherein.sugar.collections.IndexedIterator.head head]].
  * @param idx        The index of the first element to return, and the value of
  *                   [[net.noresttherein.sugar.collections.IndexedIterator.index index]] property.
  *                   Must be `rangeStart <= idx < rangeEnd`, or the subclass must adjust `index` in its constructor.
  * @param countdown  The initial size of the iterator, and the variable backing property
  *                   [[net.noresttherein.sugar.collections.CountdownIterator.remaining remaining]].
  *                   Must be non-negative.
  * @param lowerBound The index at which iteration will wrap back to the end of the range - the value of
  *                   [[net.noresttherein.sugar.collections.IndexedIterator.rangeStart rangeStart]] property.
  *                   Must be non-negative.
  * @param upperBound The index at which iteration will wrap back to the beginning, and the value of
  *                   [[net.noresttherein.sugar.collections.IndexedIterator.rangeEnd rangeEnd]] property
  *                   (and [[net.noresttherein.sugar.collections.IndexedIterator.underlyingSize underlyingSize]],
  *                   unless the subclass overrides `rangeStart`). Must be greater than zero.
  */
abstract class AbstractReverseCyclicIterator[+E](private[this] var idx :Int, private[this] var countdown :Int,
                                                 lowerBound :Int, upperBound :Int)
	extends AbstractSugaredIterator[E] with ReverseCountdownIterator[E]
{
	protected final override def rangeStart :Int = lowerBound
	protected final override def rangeEnd :Int = upperBound
	protected final override def underlyingSize :Int = upperBound - lowerBound
	protected final override def index :Int = idx
	protected final override def index_=(value :Int) :Unit = {
		idx = value
		if (value < lowerBound) {
			val range = upperBound - lowerBound
			if (range == 0)
				idx =  0
			else
				idx = lowerBound + (upperBound + value - lowerBound) % range
		}
	}
	protected final override def advance() :Int = {
		val i = idx
		idx = i - 1
		if (i == lowerBound)
			idx = upperBound - 1
		countdown -= 1
		i
	}
	protected final override def remaining :Int = countdown
	protected final override def remaining_=(value :Int) :Unit = countdown = value
	protected final override def remaining_--() :Unit = countdown -= 1
	final override def hasNext :Boolean = countdown > 0
	override def next() :E = {
		val res = head
		val i = idx
		idx = i - 1
		if (i == lowerBound)
			idx = upperBound - 1
		countdown -= 1
		res
	}
}






/** $factoryInfo
  * @tparam S The collection-like type over which the iterators produced by this factory iterate.
  * @tparam I The type of iterators produced by this factory.
  * @define input collection
  * @define coll  iterator
  * @define Coll `IndexedIterator`
  * @define factoryInfo A factory creating ${coll}s iterating over ${input}s.
  *                     The ${coll}s rely on the random indexing of the underlying ${input}s.
  */
trait IndexedIteratorFactory[-S[_], +I[_]] extends SliceFactory[S, I] {
	/** Length of the argument $input used to determine the upper index bound. */
	protected def lengthOf[T](source :S[T]) :Int

	/** Build a $coll which will return elements from range `[from, until)` in the argument $input. */
	protected def make[T](source :S[T], from :Int, until :Int) :I[T]

	/** An iterator returning the entirety of the argument $input, starting from the beginning. */
	def apply[T](source :S[T]) :I[T] =
		make(source, 0, lengthOf(source))

	/** A $coll returning elements `source(first), source(first + 1), ..., source(source.length - 1)`.
	  * Negative first index is treated like zero, and indices greater than `source.length` result in an empty iterator.
	  */
	def from[T](source :S[T], first :Int) :I[T] = {
		val len = lengthOf(source)
		if (first >= len) make(source, len, len)
		else if (first <= 0) make(source, 0, len)
		else make(source, first, len)
	}

	/** A $coll returning elements of `source`, starting with index `first`, and continuing until the end
	  * of the $input or until `length` elements are returned - whatever comes sooner.
	  * Negative `length` is equivalent to zero.
	  */
	def apply[T](source :S[T], first :Int, length :Int) :I[T] = {
		val len   = lengthOf(source)
		val from  = math.max(0, math.min(len, first))
		val until = from + math.min(len - from, math.max(length, 0))
		make(source, from, until)
	}

	/** Returns elements `seq(from), seq(from + 1), ..., seq(until - 1)` of the given sequence.
	  * If any of indices in the `[from, until)` range are negative or greater than the sequence's length,
	  * they are ignored.
	  */
	override def slice[T](source :S[T], from :Int, until :Int) :I[T] = {
		val len = lengthOf(source)
		if (from >= len)                    make(source, len, len)
		else if (until <= 0)                make(source, 0, 0)
		else if (from <= 0 && until >= len) make(source, 0, len)
		else if (from <= 0)                 make(source, 0, until)
		else if (until >= len)              make(source, from, len)
		else if (until <= from)             make(source, from, from)
		else                                make(source, from, until)
	}
}


/** An [[net.noresttherein.sugar.collections.CountdownIterator CountdownIterator]] factory semantically equivalent
  * to the regular [[net.noresttherein.sugar.collections.IndexedIteratorFactory IndexedIteratorFactory]],
  * but passing the first index and iterator size, rather than an index range as the arguments to the iterator.
  * @tparam S The collection-like type over which the iterators produced by this factory iterate.
  * @tparam I The type of iterators produced by this factory.
  * @define Coll `CountdownIterator`
  */
trait CountdownIteratorFactory[-S[_], +I[_]] extends IndexedIteratorFactory[S, I] {
	protected override def make[T](source :S[T], first :Int, size :Int) :I[T]

	override def from[T](source :S[T], first :Int) :I[T] = {
		val len = lengthOf(source)
		if (first >= len) make(source, len, 0)
		else if (first <= 0) make(source, 0, len)
		else make(source, first, len - first)
	}

	override def apply[T](source :S[T], first :Int, length :Int) :I[T] = {
		val len   = lengthOf(source)
		val from  = math.max(0, math.min(len, first))
		val size  = math.min(len - from, math.max(length, 0))
		make(source, from, size)
	}

	override def slice[T](source :S[T], from :Int, until :Int) :I[T] = {
		val len = lengthOf(source)
		if (from >= len)                    make(source, len, 0)
		else if (until <= 0)                make(source, 0, 0)
		else if (from <= 0 && until >= len) make(source, 0, len)
		else if (from <= 0)                 make(source, 0, until)
		else if (until >= len)              make(source, from, len - from)
		else if (until <= from)             make(source, from, 0)
		else                                make(source, from, until - from)
	}
}


/** $factoryInfo
  * @tparam S The collection-like type over which the iterators produced by this factory iterate.
  * @tparam I The type of iterators produced by this factory.
  * @define coll cyclic iterator
  */
trait CyclicIteratorFactory[-S[_], +I[_]] extends CountdownIteratorFactory[S, I] {
	/** A $coll returning all the elements of `source` in ascending index order,
	  * starting with index `first % source.length`. The index is increased modulo the length of the $input,
	  * wrapping back to the beginning when its end is reached.
	  */
	override def from[T](source :S[T], first :Int) :I[T] = {
		val len = lengthOf(source)
		if (len <= 1)       make(source, 0, len)
		else if (first < 0) make(source, (len + first % len) % len, len)
		else                make(source, first % len, len)
	}

	/** A $coll returning `length` elements of the $input, starting with `source(offset)`. If `offset + length`
	  * is greater than the size of the $input, then the iterator wraps to the beginning of the $input,
	  * returning `source(0)` following `source(source.length - 1)`, and so on, until `min(length, source.length)`
	  * elements are returned. If `length` is negative, the iterator will have no elements.
	  * @param source the $input with elements to iterate.
	  * @param offset the index of the first returned element, modulo the length of the array.
	  * @param length the maximum number of returned elements.
	  */ //consider: allowing returning more than source.length elements.
	override def apply[T](source :S[T], offset :Int, length :Int) :I[T] = {
		val len = lengthOf(source)
		val from =
			if (len <= 1) 0
			else if (offset < 0) (len + offset % len) % len
			else offset % len
		make(source, from, math.min(math.max(length, 0), len))
	}

	/** A $coll returning subsequent elements of a $input, starting with index `from % source.length`,
	  * and increasing modulo the length of the $input until index `until % source.length` is reached (exclusive).
	  * If `from % source.length == until % source.length`, then the $coll will return the whole $input,
	  * unless also `from == until`, in which case it will be empty.
	  * @param source the $input with the elements to iterate.
	  * @param from   the index of the first returned element, modulo the length of the $input.
	  * @param until  the index immediately following the last element of the iterator.
	  */
	override def slice[T](source :S[T], from :Int, until :Int) :I[T] = {
		val len = lengthOf(source)
		if (len == 0) //avoid division by zero
			make(source, 0, 0)
		else {
			val from0  = if (from < 0) (len + from % len) % len else from % len
			val until0 = if (until < 0) (len + until % len) % len else until % len
			val size   =
				if (until0 > from0) until0 - from0
				else if (from0 > until0) len + until0 - from0
				else if (from == until) 0
				else len
			make(source, from0, size)
		}
	}
}


/** A factory of iterators over ${input} returning the elements in the descending index order.
  * @tparam S The collection-like type over which the iterators produced by this factory iterate.
  * @tparam I The type of iterators produced by this factory.
  * @define Coll `ReverseIndexedIterator`
  * @define coll reverse iterator
  */
trait ReverseIndexedIteratorFactory[-S[_], +I[_]] extends IndexedIteratorFactory[S, I] {
	override def apply[T](source :S[T]) :I[T] = make(source, 0, lengthOf(source))

	/** A $coll returning elements `source(first), source(first - 1), ..., source(0)`.
	  * If `first >= source.length`, then the whole $input is returned; if `first < 0`, then the iterator is empty.
	  */
	override def from[T](source :S[T], first :Int) :I[T] = {
		val len = lengthOf(source)
		if (first <= -1) make(source, 0, 0)
		else if (first >= len) make(source, 0, len)
		else make(source, 0, first + 1)
	}

	/** A $coll returning elements `source(first), source(first - 1), ..., source(first - length + 1)`,
	  * or until the first element of the $input - whichever comes sooner.
	  * If `first >= source.length`, then the actual first returned element is the last element of the $input.
	  * If the iterator needs to access an element at index lesser than zero, the excess elements are ignored.
	  * Negative `length` is the same as zero.
	  */
	override def apply[T](source :S[T], first :Int, length :Int) :I[T] = {
		val len    = lengthOf(source)
		val from   = math.max(-1, math.min(len - 1, first))
		val downTo = from - math.min(from + 1, math.max(length, 0))
		make(source, downTo + 1, from + 1)
	}

	/** A $coll returning elements `source(until - 1), source(until - 2), ..., source(from)`.
	  * If any of the indices in the `[from, until)` range is out of bounds for `source`, it is ignored.
	  * Note that the first index argument is the last element of the iterator, and the actual first element
	  * of the iterator resides at an index preceding the second argument.
	  */ //override for docs.
	override def slice[T](source :S[T], from :Int, until :Int) :I[T]
}


/** An [[net.noresttherein.sugar.collections.ReverseCountdownIterator ReverseCountdownIterator]] factory
  * semantically equivalent to the regular
  * [[net.noresttherein.sugar.collections.ReverseIndexedIteratorFactory ReverseIndexedIteratorFactory]],
  * but passing the first index and iterator size, rather than an index range as the arguments to the iterator.
  * @tparam S    The collection-like type over which the iterators produced by this factory iterate.
  * @tparam I    The type of iterators produced by this factory.
  * @define Coll `ReverseCountdownIterator`
  */
trait ReverseCountdownIteratorFactory[-S[_], +I[_]] extends ReverseIndexedIteratorFactory[S, I] {
	protected override def make[T](source :S[T], first :Int, size :Int) :I[T]

	override def apply[T](source :S[T]) :I[T] = {
		val len = lengthOf(source)
		make(source, len - 1, len)
	}

	/** A $coll returning elements `source(first), source(first - 1), ..., source(0)`.
	  * If `first >= source.length`, then the whole $input is returned; if `first < 0`, then the iterator is empty.
	  */
	override def from[T](source :S[T], first :Int) :I[T] = {
		val len = lengthOf(source)
		if (first <= -1)       make(source, -1, 0)
		else if (first >= len) make(source, len - 1, len)
		else                   make(source, first - len + 1, first + 1)
	}

	/** A $coll returning elements `source(first), source(first - 1), ..., source(first - length + 1)`,
	  * or until the first element of the $input - whichever comes sooner.
	  * If `first >= source.length`, then the actual first returned element is the last element of the $input.
	  * If the iterator needs to access an element at index lesser than zero, the excess elements are ignored.
	  * Negative `length` is the same as zero.
	  */
	override def apply[T](source :S[T], first :Int, length :Int) :I[T] = {
		val len    = lengthOf(source)
		val from   = math.max(-1, math.min(len - 1, first))
		val downTo = from - math.min(from + 1, math.max(length, 0))
		make(source, downTo + 1, from + 1)
	}

	/** A $coll returning elements `source(until - 1), source(until - 2), ..., source(from)`.
	  * If any of the indices in the `[from, until)` range is out of bounds for `source`, it is ignored.
	  * Note that the first index argument is the last element of the iterator, and the actual first element
	  * of the iterator resides at an index preceding the second argument.
	  */
	override def slice[T](source :S[T], from :Int, until :Int) :I[T] = {
		val len = lengthOf(source)
		if (from >= len) make(source, len, 0)
		else if (until <= 0) make(source, -1, 0)
		else if (from <= 0 & until >= len) make(source, len - 1, len)
		else if (from <= 0) make(source, until - 1, until)
		else if (until >= len) make(source, len - 1, len)
		else if (until <= from) make(source, from, 0)
		else make(source, until - 1, until)
	}
}


/** $factoryInfo
  * @tparam S    The collection-like type over which the iterators produced by this factory iterate.
  * @tparam I    The type of iterators produced by this factory.
  * @define coll reverse cyclic iterator
  */
trait ReverseCyclicIteratorFactory[-S[_], +I[_]] extends ReverseCountdownIteratorFactory[S, I] {
	override def from[T](source :S[T], first :Int) :I[T] = {
		val len = lengthOf(source)
		if (len == 0)          make(source, 0, 0)
		else if (first >= len) make(source, first % len, len)
		else if (first < 0)    make(source, (len + first % len) % len, len)
		else                   make(source, first, len)
	}

	/** A $coll returning elements of the $input in the decreasing index order, starting with `source(first)`.
	  * If `length > first + 1`, then, following `source(0)`, the iterator returns `source(source.length - 1)`,
	  * `source(source.length - 2)`, and so on, until `min(length, source.length)` elements are returned.
	  * If `length` is negative, the iterator will have no elements. If `length` is greater than the length of the array,
	  * then all elements in the array are returned exactly once (assuming the iterator is exhausted).
	  */
	override def apply[T](source :S[T], first :Int, length :Int) :I[T] = {
		val len = lengthOf(source)
		val from =
			if (len <= 1) 0
			else if (first >= len) first % len
			else if (first < 0) (len + first % len) % len
			else first
		make(source, from, math.min(len, math.max(length, 0)))
	}

	//An alternative approach would take size = until - from and let returning some elements more than once.
	//In that case, however, we'd need to treat both as unsigned because of a real possibility of overflow.
	/** A $coll returning elements of the $input at decreasing index order, starting with `(hi - 1) % source.length`,
	  * and ending (inclusive) with `lo % source.length`. If `lo` is greater than `hi` modulo the length of the array,
	  * the element at `0` is followed by elements at `source.length - 1, source.length - 2`, etc.
	  * If `from % len == until % len`, then the iterator will return all elements in the array, unless `from == until`,
	  * in which case it will be empty.
	  */
	override def slice[T](source :S[T], from :Int, until :Int) :I[T] = {
		val len = lengthOf(source)
		if (len == 0) //avoid division by zero!
			make(source, 0, 0)
		else {
			val start = if (until > 0) (until - 1) % len else (len + until % len - 1) % len
			val end   = if (from > 0) (from - 1) % len else (len + from % len - 1) % len
			val size  =
				if  (start > end) start - end
				else if (start < end) len + start - end
				else if (from == until) 0
				else len
			make(source, start, size)
		}
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
	protected override def underlyingSize :Int = seq.length
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


/** $factoryInfo
  * @define input indexed sequence
  * @define coll  sequence iterator
  */
@SerialVersionUID(Ver)
private case object IndexedSeqIterator
	extends IndexedIteratorFactory[({ type S[+X] = collection.IndexedSeqOps[X, kinds.Any1, Any] })#S, Iterator]
{
	protected override def lengthOf[T](source :collection.IndexedSeqOps[T, Any1, Any]) :Int = source.length

	protected override def make[T](source :collection.IndexedSeqOps[T, kinds.Any1, Any], from :Int, until :Int)
			:IndexedSeqIterator[T] =
		new IndexedSeqIterator(source, 0, source.length)
}




/** An iterator advancing over a slice of an `IndexedSeq` in the reverse direction.
  * @param end   the index in the sequence `-1 <= last <= first < seq.length` before the last element to return.
  * @param first the index in the sequence of the first element to return.
  */
private final class ReverseIndexedSeqIterator[+T] private[collections]
	                                         (seq :collection.IndexedSeqOps[T, kinds.Any1, _], end :Int, first :Int)
	extends AbstractReverseIndexedIterator[T](end, first) with ReverseIndexedIterator[T]
{
	def this(seq :collection.IndexedSeqOps[T, kinds.Any1, _], idx :Int) = this(seq, -1, idx)
	def this(seq :collection.IndexedSeqOps[T, kinds.Any1, _]) = this(seq, -1, seq.length - 1)

	private def underlying = seq
	protected override def underlyingSize :Int = seq.length

	override def head :T = seq(index)

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :ReverseIndexedSeqIterator[_] =>
			(underlying.asAnyRef eq other.underlying.asAnyRef) && index == other.index && limit == other.limit
		case _ => false
	}
	private def indexedSeq :collection.IndexedSeq[T] = seq match {
		case s :collection.IndexedSeq[T] => s
		case _                           => seq.toIndexedSeq
	}
	override def hashCode :Int = ReversedSeq(indexedSeq.slice(limit + 1, index + 1)).hashCode
	override def clone = new ReverseIndexedSeqIterator(seq, limit, index)
}


/** $factoryInfo
  * @define input indexed sequence
  * @define coll  reverse sequence iterator
  */
@SerialVersionUID(Ver)
private case object ReverseIndexedSeqIterator
	extends ReverseIndexedIteratorFactory[({ type S[X] = collection.IndexedSeqOps[X, Any1, Any] })#S, Iterator]
{
	protected override def lengthOf[T](source :collection.IndexedSeqOps[T, Any1, Any]) :Int = source.length

	protected override def make[T](source :collection.IndexedSeqOps[T, Any1, Any], from :Int, until :Int) :Iterator[T] =
		new ReverseIndexedSeqIterator(source, from - 1, until - 1)
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

	override def copyToArray[B >: Char](xs :Array[B], start :Int, len :Int) :Int = {
		val copied = elementsToCopy(knownSize, xs, start, len)
		if (copied > 0) {
			xs match {
				case chars :Array[Char] =>
					string.getChars(first, first + copied, chars, start)
				case refs  :Array[Any] =>
					var i = 0
					while (i < copied) {
						refs(start + i) = string.charAt(first + i)
						i += 1
					}
				case _ => ??!
			}
			first += copied
		}
		copied
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
	def apply(string :String, first :Int, length :Int) :StringIterator = {
		val len   = string.length
		val from  = math.max(0, math.min(len, first))
		val until = from + math.min(len - from, math.max(length, 0))
		new StringIterator(string, first, until)
	}

	/** Returns characters `string(from), string(from + 1), ..., string(until - 1)` of the given string.
	  * If any of indices in the `[from, until)` range are negative or greater than the string's length, they are ignored.
	  */
	def slice(string :String, from :Int, until :Int) :StringIterator = {
		val len = string.length
		if (from >= len)                   new StringIterator(string, len, len)
		else if (until <= 0)               new StringIterator(string, 0, 0)
		else if (from <= 0 & until >= len) new StringIterator(string, 0, len)
		else if (from <= 0)                new StringIterator(string, 0, until)
		else if (until >= len)             new StringIterator(string, from, len)
		else if (until <= from)            new StringIterator(string, from, from)
		else                               new StringIterator(string, from, until)
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
	        (string :String, private[this] var end :Int, private[this] var first :Int)
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
	protected override def index :Int = first
	protected override def index_=(i :Int) :Unit = first = i
	protected override def limit :Int = end
	protected override def limit_=(i :Int) :Unit = end = i

	override def hasNext :Boolean = first != end
	override def head :Char = string.charAt(first)

	override def next() :Char = {
		if (first <= end)
			noSuch_!("Index " + first + " has reached its lower bound of " + this + ".")
		first -= 1
		string.charAt(first + 1)
	}

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :ReverseStringIterator =>
			(string eq other.underlying) && index == other.index && limit == other.limit
		case _ => false
	}
	override def hashCode :Int = ReversedSeq(new WrappedString(string).slice(end + 1, first + 1)).hashCode
	override def clone = new ReverseStringIterator(string, end, first)

	override def toString :String = "ReverseStringIterator(\"" + string + "\"@" + index + ")"
}


@SerialVersionUID(Ver)
object ReverseStringIterator {
	def apply(string :String) :ReverseStringIterator = new ReverseStringIterator(string, -1, string.length - 1)

	/** An iterator returning elements `seq(first), seq(first - 1), ..., seq(first - length + 1)`.
	  * If the iterator needs to access an element at index lesser than zero, the excess elements are ignored.
	  * Negative `length` is the same as zero.
	  */
	def apply(string :String, first :Int, length :Int) :ReverseStringIterator = {
		val len = string.length
		val from   = math.max(-1, math.min(len - 1, first))
		val downTo = from - math.min(from + 1, math.max(length, 0))
		new ReverseStringIterator(string, downTo, from)
	}

	/** An iterator returning characters `string(hi - 1), string(hi - 2), ..., string(lo)`.
	  * If any of the indices in the `[lo, hi)` range is out of bounds for `string`, it is ignored.
	  */
	def slice(string :String, lo :Int, hi :Int) :ReverseStringIterator = {
		val len = string.length
		if (lo >= len) new ReverseStringIterator(string, len - 1, len - 1)
		else if (hi <= 0) new ReverseStringIterator(string, -1, -1)
		else if (lo <= 0 & hi >= len) new ReverseStringIterator(string, -1, len - 1)
		else if (lo <= 0) new ReverseStringIterator(string, -1, hi - 1)
		else if (hi >= len) new ReverseStringIterator(string, lo - 1, len - 1)
		else if (hi <= lo) new ReverseStringIterator(string, lo - 1, lo - 1)
		else new ReverseStringIterator(string, lo - 1, hi - 1)
	}

	val empty :ReverseStringIterator = new ReverseStringIterator("", -1, -1)
}

