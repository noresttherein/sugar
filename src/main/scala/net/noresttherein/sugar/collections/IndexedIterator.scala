package net.noresttherein.sugar.collections

import java.lang.{Math => math}

import scala.collection.immutable.WrappedString
import scala.collection.{AbstractIterator, BufferedIterator}

import net.noresttherein.sugar.casting.castingMethods
import net.noresttherein.sugar.collections.util.elementsToCopy
import net.noresttherein.sugar.exceptions.{??!, illegalState_!, noSuch_!, outOfBounds_!, unsupported_!}
import net.noresttherein.sugar.extensions.hashCodeMethods
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

	/** Moves the index forward by one, or whatever is required to advance the position of the iterator.
	  * Overriding the method in a subclass eliminates the need to retrieve index value before updating it.
	  * Contains the main logic behind `next()`. This is a trusted method; it may assume that `this.knownSize > 0`.
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
		val min   = rangeStart & 0xffffffffL
		val max   = rangeEnd & 0xffffffffL
		val curr  = index & 0xffffffffL
		val end   = limit & 0xffffffffL
		if (end > max | end < min) //Compare unsigned.
			outOfBounds_!(end, min, max - 1)
		if (curr > max | curr < min)
			outOfBounds_!(curr, min, max - 1)
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

	/** Returns the next element of the iterator. Delegates to
	  * [[net.noresttherein.sugar.collections.IndexedIterator.head head]] (which is responsible for checking
	  * that the iterator is not empty) and [[net.noresttherein.sugar.collections.IndexedIterator.advance advance]].
	  */
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

	//consider: safeCopyToArray = copyToArray

	protected def className :String = localClassNameOf(this)

	override def toString :String = { //todo: change all iterator implementations to use # instead of @ for the index.
		val range =
			if (underlyingSize < 0) ""
			else if (rangeStart == 0) "/" + underlyingSize
			else "/[" + rangeStart + ", " + rangeEnd + "]"
		className + "@" +  + index + range + "|" + knownSize + "|"
	}
}


/** Base class for forward iterators over structures with random indexing.
  * Requires the subclasses to only implement `head`. This class does not perform any validation of its arguments.
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
  * @note This implementation will work even if the underlying collection has `Int.MaxValue` elements.
  */
trait CountdownIterator[+E] extends IndexedIterator[E] {
	/** The number of elements in the iterator. */
	protected var remaining :Int
	protected override def limit :Int = index + remaining
	protected override def limit_=(value :Int) :Unit = remaining = value - index
	protected def remaining_--() :Unit = remaining -= 1

	/** Increases `index` by one and decreases `remaining` by one.
	  * @return the index before increment.
	  */
	protected override def advance() :Int = {
		val idx = index
		index   = idx + 1
		remaining -= 1
		idx
	}
	final override def knownSize :Int = remaining

	override def hasNext :Boolean = remaining > 0

	protected def maxSize :Int = rangeEnd - index

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
		if (remaining > maxSize)
			remaining = maxSize
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
		if (size < 0 | size > maxSize)
			outOfBounds_!(toString + " iterator size " + size + " out of range [0, " + maxSize + "].")
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
			//Array iterators have a tendency to divide by zero when the length of the array is zero,
			// so be careful around empty iterators.
			if (rem > 0) {
				val dropped = math.min(rem, n)
				index += dropped
				remaining = rem - dropped
			}
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
	protected final override def remaining :Int = countdown
	protected final override def remaining_=(value :Int) :Unit = countdown = value
	protected final override def remaining_--() :Unit = countdown -= 1
	protected final override def advance() :Int = {
		val i = idx
		idx = i + 1
		countdown -= 1
		i
	}
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
	protected final override def rangeEnd :Int = upperBound
	protected final override def maxSize :Int = underlyingSize
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
	protected final override def remaining :Int = countdown
	protected final override def remaining_=(value :Int) :Unit = countdown = value
	protected final override def remaining_--() :Unit = countdown -= 1
	protected final override def advance() :Int = {
		val i = idx
		idx = i + 1
		if (i == upperBound - 1)
			idx = rangeStart
		countdown -= 1
		i
	}
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
		val min   = (rangeStart & 0xffffffffL) - 1
		val max   = (rangeEnd & 0xffffffffL) - 1
		val curr  = index & 0xffffffffL
		val end   = limit & 0xffffffffL
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
	protected final override def limit :Int = end
	protected final override def limit_=(value :Int) :Unit = end = value
	protected final override def advance() :Int = { val idx = first; first = idx - 1; idx }

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
  */
trait ReverseCountdownIterator[+E] extends ReverseIndexedIterator[E] with CountdownIterator[E] {
	/** Decreases ''both'' `index` and `remaining` properties by one. */
	protected override def advance() :Int = {
		val idx = index
		index = idx - 1
		remaining -= 1
		idx
	}
	protected final override def limit :Int = index - remaining
	protected final override def limit_=(value :Int) :Unit = remaining = index - value
	protected override def maxSize :Int = index - rangeStart + 1

	override def drop(n :Int) :Iterator[E] = {
		if (n > 0) {
			val rem = remaining
			if (rem > 0) {
				val dropped = math.min(rem, n)
				index -= dropped
				remaining = rem - dropped
			}
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
	protected final override def remaining :Int = countdown
	protected final override def remaining_=(value :Int) :Unit = countdown = value
	protected final override def remaining_--() :Unit = countdown -= 1
	protected final override def advance() :Int = {
		val i = idx
		idx = i - 1
		countdown -= 1
		i
	}
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
  * Methods [[net+.noresttherein.sugar.collections.CountdownIterator.adjustRange adjustRange]] and
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
	protected final override def maxSize :Int = upperBound - lowerBound
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
	protected final override def remaining :Int = countdown
	protected final override def remaining_=(value :Int) :Unit = countdown = value
	protected final override def remaining_--() :Unit = countdown -= 1
	protected final override def advance() :Int = {
		val i = idx
		idx = i - 1
		if (i == lowerBound)
			idx = upperBound - 1
		countdown -= 1
		i
	}
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




/** Implements `equals` for `IndexedIterator` in terms of `index`, `knownSize` and the underlying collection.
  * The collection object should be returned by
  * [[net.noresttherein.sugar.collections.IndexedIteratorEquals.source source]] and is compared for ''referential''
  * equality: two iterators for different collections will never be equal, even if they will return the same elements.
  * Will equal only instances of the same class as the runtime class of this object, unless `canEqual` is overridden.
  */
trait IndexedIteratorEquals[+E] extends IndexedIterator[E] with Equals {
	/** The collection object containing elements of this iterator. */
	protected def source :AnyRef

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :IndexedIteratorEquals[_] if other canEqual this =>
			(source eq other.source) && index == other.index && knownSize == other.knownSize
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.getClass == getClass
	override def hashCode :Int = ((source.identityHashCode * 31) + index) * 31 + knownSize
}





/** An iterator advancing over a slice of an `IndexedSeq`.
  * @param first    the index in the sequence of the first/next element to return.
  * @param `last++` the index in the sequence delimiting the iterator, that is pointing after the last element
  *                 the iterator should return.
  */
private final class IndexedSeqIterator[+T] private[collections]
	                                  (sq :collection.IndexedSeqOps[T, kinds.Any1, _], first :Int, `last++` :Int)
	extends AbstractIndexedIterator[T](first, `last++`) with IndexedIteratorEquals[T]
{
	def this(seq :collection.IndexedSeqOps[T, kinds.Any1, _], idx :Int) = this(seq, idx, seq.length)
	def this(seq :collection.IndexedSeqOps[T, kinds.Any1, _]) = this(seq, 0, seq.length)

	protected override def source = sq.asInstanceOf[AnyRef]
	protected override def underlyingSize :Int = sq.length
	override def head :T = sq(index)

	override def clone = new IndexedSeqIterator(sq, first, `last++`)
}


/** $factoryInfo
  * @define coll   sequence iterator
  * @define Coll  `IndexedIterator`
  * @define source indexed sequence
  * @define Source `IndexedSeqOps`
  */
@SerialVersionUID(Ver)
private case object IndexedSeqIterator
	extends ExpandedSliceFactory[({ type S[+X] = collection.IndexedSeqOps[X, kinds.Any1, Any] })#S, Iterator]
{
	protected override def totalSizeOf[T](source :collection.IndexedSeqOps[T, Any1, Any]) :Int = source.length

	protected override def make[T](source :collection.IndexedSeqOps[T, kinds.Any1, Any], from :Int, until :Int)
			:IndexedSeqIterator[T] =
		new IndexedSeqIterator(source, 0, source.length)
}




/** An iterator advancing over a slice of an `IndexedSeq` in the reverse direction.
  * @param end   the index in the sequence `-1 <= end <= first < seq.length` before the last element to return.
  * @param first the index in the sequence of the first element to return.
  */
private final class ReverseIndexedSeqIterator[+T] private[collections]
	                                         (sq :collection.IndexedSeqOps[T, kinds.Any1, _], end :Int, first :Int)
	extends AbstractReverseIndexedIterator[T](end, first) with IndexedIteratorEquals[T]
{
	def this(seq :collection.IndexedSeqOps[T, kinds.Any1, _], idx :Int) = this(seq, -1, idx)
	def this(seq :collection.IndexedSeqOps[T, kinds.Any1, _]) = this(seq, -1, seq.length - 1)

	protected override def source = sq.asInstanceOf[AnyRef]
	protected override def underlyingSize :Int = sq.length

	override def head :T = sq(index)

	override def clone = new ReverseIndexedSeqIterator(sq, limit, index)
}


/** $factoryInfo
  * @define coll   reverse sequence iterator
  * @define Coll  `ReverseIndexedIterator`
  * @define source indexed sequence
  * @define Source `IndexedSeqOps`
  */
@SerialVersionUID(Ver)
private case object ReverseIndexedSeqIterator
	extends ReverseSliceFactory[({ type S[X] = collection.IndexedSeqOps[X, Any1, Any] })#S, Iterator]
{
	protected override def totalSizeOf[T](source :collection.IndexedSeqOps[T, Any1, Any]) :Int = source.length

	protected override def make[T](source :collection.IndexedSeqOps[T, Any1, Any], from :Int, until :Int) :Iterator[T] =
		new ReverseIndexedSeqIterator(source, from - 1, until - 1)
}




/** An iterator advancing over a slice of a potentially mutable `IndexedSeq`.
  * Unlike [[net.noresttherein.sugar.collections.IndexedSeqIterator IndexedSeqIterator]] it will never throw
  * a `ConcurrentModificationException` and will include any elements added to the underlying sequence after
  * the iterator's creation, as well as cut short the iteration if the buffer shrinks.
  * @param sq    the collection with elements to iterate over.
  * @param start the index in the sequence of the first/next element to return.
  * @param end   the index lesser than `seq.size` limiting the iterated elements,
  *              or `-1` in order to iterate over the whole remainder of the sequence.
  */
private final class RobustIndexedSeqIterator[+T] private[collections]
                    (sq :collection.IndexedSeqOps[T, kinds.Any1, _],
                     private[this] var start :Int, private[this] var end :Int)
	extends AbstractSugaredIterator[T] with IndexedIteratorEquals[T]
{
	def this(seq :collection.IndexedSeqOps[T, kinds.Any1, _], idx :Int) = this(seq, idx, -1)
	def this(seq :collection.IndexedSeqOps[T, kinds.Any1, _]) = this(seq, 0, -1)

	protected override def source = sq.asInstanceOf[AnyRef]
	protected override def underlyingSize :Int = sq.length
	protected override def index :Int = start
	protected override def index_=(value :Int) :Unit = start = value
	protected override def limit :Int = if (end < 0) sq.length else math.min(sq.length, end)
	protected override def limit_=(value :Int) :Unit = end = value

	override def head :T = sq(start)
	override def clone = new RobustIndexedSeqIterator(sq, start, end)
}


/** $factoryInfo
  * @define coll   buffer iterator
  * @define Coll  `IndexedIterator`
  * @define source indexed buffer
  * @define Source `IndexedSeqOps`
  */
@SerialVersionUID(Ver)
private case object RobustIndexedSeqIterator
	extends ExpandedSliceFactory[({ type S[+X] = collection.IndexedSeqOps[X, kinds.Any1, Any] })#S, Iterator]
{
	protected override def totalSizeOf[T](source :collection.IndexedSeqOps[T, Any1, Any]) :Int = source.length

	protected override def make[T](source :collection.IndexedSeqOps[T, kinds.Any1, Any], from :Int, until :Int)
			:RobustIndexedSeqIterator[T] =
		new RobustIndexedSeqIterator(source, 0, source.length)

	override def apply[T](source :collection.IndexedSeqOps[T, Any1, Any]) :RobustIndexedSeqIterator[T] =
		new RobustIndexedSeqIterator(source, 0, -1)

	override def from[T](source :collection.IndexedSeqOps[T, Any1, Any], first :Int) :RobustIndexedSeqIterator[T] =
		new RobustIndexedSeqIterator(source, math.max(0, math.min(source.length, first)))
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
}


@SerialVersionUID(Ver)
case object StringIterator {
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
}


@SerialVersionUID(Ver)
case object ReverseStringIterator {
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





/*
trait Indexed2Iterator[+E] extends IndexedIterator[E] {
	protected var index1 :Int
	protected var index2 :Int
	protected def range1 :Int
	protected def range2 :Int
	override def underlyingSize :Int = range2 * range1

	override def index :Int = index2 * range1 + index2
	override def index_=(value :Int) :Unit = {
		val range = range1
		val idx2 = value / range
		index2 = idx2
		index1 = value - idx2 * range
	}
}
*/
