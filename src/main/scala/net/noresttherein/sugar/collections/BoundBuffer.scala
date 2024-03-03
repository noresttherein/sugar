package net.noresttherein.sugar.collections

import java.lang.{Math => math}

import scala.collection.{StrictOptimizedSeqOps, mutable}
import scala.collection.mutable.{AbstractBuffer, ArrayBuffer, Buffer, IndexedBuffer}

import net.noresttherein.sugar.arrays.{ArrayIterator, RefArray, ReverseArrayIterator}
import net.noresttherein.sugar.collections.util.knownEmpty
import net.noresttherein.sugar.exceptions.{MaxSizeReachedException, SugaredException, concurrent_!, illegal_!, outOfBounds_!, validate}
import net.noresttherein.sugar.extensions.IterableOnceExtension
import net.noresttherein.sugar.funny.generic




//todo: ReverseBuffer for IndexedSeq and another Buffer
//todo: tests!

/** A buffer with a specified maximum size. Its companion object allows creating instances backed
  * by a given `mutable.`[[scala.collection.mutable.IndexedSeq IndexedSeq]] or an `Array`,
  * allowing to writing to them at specified positions using the `Buffer` interface.
  * Other than its internals being shared and not growing, the buffer behaves like a regular `ArrayBuffer`.
  * The only difference is that elements in the underlying array or sequence are not set to `null`
  * when elements are removed from the buffer. All methods adding elements to the buffer may throw
  * a [[net.noresttherein.sugar.collections.BufferFullException BufferFullException]].
  * @see [[net.noresttherein.sugar.collections.ViewBuffer]]
  * @define Coll `BoundBuffer`
  * @define coll bound buffer
  */
trait BoundBuffer[E] extends Buffer[E] {
	/** The maximum capacity of the buffer. Attempting to add more elements results in throwing
	  * a [[net.noresttherein.sugar.collections.BufferFullException BufferFullException]].
	  */
	def maxSize :Int
}


@SerialVersionUID(Ver)
object BoundBuffer {
	/** Creates a buffer writing to a section of a given mutable sequence.
	  * The buffer initially equals `seq.slice(from, from + size)`.
	  * @param from  the index in `seq` of the first element of the buffer.
	  * @param until the upper bound in `seq` on the buffer contents.
	  * @param size  the initial size of the buffer.
	  */
	@throws[IndexOutOfBoundsException]("if from or until are outside of [0, seq.length) range, " +
	                                   "or if size is less then zero or greater than until-from.")
	def apply[E](seq :mutable.IndexedSeq[E], from :Int, until :Int, size :Int) :BoundBuffer[E] = {
		val len = seq.length
		validate.inRange(from, 0, len)
		validate.inRange(until, 0, len)
		validate.inRange(size, 0, until - from)
		new BoundSeqBuffer(seq, from, size, until - from)
	}

	/** Creates a buffer writing to a section of a given array.
	  * The buffer initially equals `array.slice(from, from + size).toSeq`.
	  * @param from  the index in `array` of the first element of the buffer.
	  * @param until the upper bound in `array` on the buffer contents.
	  * @param size  the initial size of the buffer.
	  */
	@throws[IndexOutOfBoundsException]("if from or until are outside of [0, array.length) range, " +
	                                   "or if size is less then zero or greater than until-from.")
	def apply[E](array :Array[E], from :Int, until :Int, size :Int) :BoundBuffer[E] = {
		val len = array.length
		validate.inRange(from, 0, len)
		validate.inRange(until, 0, len)
		validate.inRange(size, 0, until - from)
		new BoundArrayBuffer(array, from, size, until - from)
	}

	/** Creates a buffer writing to a given mutable sequence, starting from its first index. */
	def empty[E](seq :mutable.IndexedSeq[E]) :BoundBuffer[E] = new BoundSeqBuffer(seq, 0, 0, seq.length)

	/** Creates a buffer writing to a given mutable array, starting from its first index. */
	def empty[E](array :Array[E]) :BoundBuffer[E] = new BoundArrayBuffer(array, 0, 0, array.length)

	/** Creates a buffer writing to a given section of a mutable sequence.
	  * First element of the buffer maps to `seq(from)`, and the buffer cannot grow past `until-from` elements.
	  */
    @throws[IndexOutOfBoundsException]("if from or until are outside of [0, seq.length] range.")
	def empty[E](seq :mutable.IndexedSeq[E], from :Int, until :Int) :BoundBuffer[E] = {
		val len = seq.length
		validate.inRange(from, 0, len)
		validate.inRange(until, 0, len)
		new BoundSeqBuffer(seq, from, 0, math.max(from, until))
	}

	/** Creates a buffer writing to a given section of an array.
	  * First element of the buffer maps to `array(from)`, and the buffer cannot grow past `until-from` elements.
	  */
	@throws[IndexOutOfBoundsException]("if from or until are outside of [0, array.length] range.")
	def empty[E](array :Array[E], from :Int, until :Int) :BoundBuffer[E] = {
		val len = array.length
		validate.inRange(from, 0, len)
		validate.inRange(until, 0, len)
		new BoundArrayBuffer(array, from, 0, math.max(from, until))
	}

	/** Creates a buffer backed by a given mutable sequence, containing all its elements.
	  * Elements must be removed before adding new ones,
	  * or a [[net.noresttherein.sugar.collections.BufferFullException BufferFullException]] will be thrown.
	  */
	def full[E](seq :mutable.IndexedSeq[E]) :BoundBuffer[E] = new BoundSeqBuffer(seq, 0, seq.length, seq.length)

	/** Creates a buffer backed by a given array, containing all its elements.
	  * Elements must be removed before adding new ones,
	  * or a [[net.noresttherein.sugar.collections.BufferFullException BufferFullException]] will be thrown.
	  */
	def full[E](array :Array[E]) :BoundBuffer[E] = new BoundArrayBuffer(array, 0, array.length, array.length)

	/** Creates a buffer view of the given index range in a given mutable sequence.
	  * The buffer initially equals `seq.slice(from, until)`, and elements must be removed before new ones
	  * are added, or a [[net.noresttherein.sugar.collections.BufferFullException BufferFullException]] will be thrown.
	  */
	@throws[IndexOutOfBoundsException]("if from or until are outside of [0, seq.length) range.")
	def full[E](seq :mutable.IndexedSeq[E], from :Int, until :Int) :BoundBuffer[E] = {
		val len = seq.length
		val until0 = math.max(from, until)
		validate.inRange(from, 0, len)
		validate.inRange(until, 0, len)
		new BoundSeqBuffer(seq, from, until0 - from, until0)
	}

	/** Creates a buffer view of the given index range in a given array.
	  * The buffer initially equals `array.slice(from, until).toSeq`, and elements must be removed before new ones
	  * are added, or a [[net.noresttherein.sugar.collections.BufferFullException BufferFullException]] will be thrown.
	  */
	@throws[IndexOutOfBoundsException]("if from or until are outside of [0, seq.length) range.")
	def full[E](array :Array[E], from :Int, until :Int) :BoundBuffer[E] = {
		val len = array.length
		val until0 = math.max(from, until)
		validate.inRange(from, 0, len)
		validate.inRange(until, 0, len)
		new BoundArrayBuffer(array, from, until0 - from, until0)
	}
}



private abstract class AbstractBoundBuffer[E](private[this] var offset :Int,
                                              private[this] var len :Int, override val maxSize :Int)
	extends AbstractBuffer[E] with IndexedBuffer[E] with BoundBuffer[E]
		//We care about extending this trait because IterableOnceExtension checks for it in its knownStrict method.
		//Note that everywhere in this file this symbol refers to the trait in scala.collection, not collection.immutable.
	   with StrictOptimizedSeqOps[E, IndexedBuffer, IndexedBuffer[E]]
{
	override def knownSize :Int = len
	override def length    :Int = len
	private[collections] def startIndex :Int = offset

	protected def get(absoluteIdx :Int) :E
	protected def set(absoluteIdx :Int, elem :E) :Unit

	override def apply(i :Int) :E =
		if (i < 0 | i >= len) outOfBounds_!(i, len)
		else get(offset + i)

	override def update(idx :Int, elem :E) :Unit =
		if (idx < 0 | idx >= len) outOfBounds_!(idx, len)
		else set(offset + idx, elem)

	override def addOne(elem :E) :this.type =
		if (len == maxSize - offset)
			bufferFull()
		else {
			set(offset + len, elem)
			len += 1
			this
		}

	override def prepend(elem :E) :this.type = { insert(0, elem); this }
	override def prependAll(elems :IterableOnce[E]) :this.type = { insertAll(0, elems); this }

	override def insert(idx :Int, elem :E) :Unit =
		if (idx < 0 | idx > len)
			outOfBounds_!(idx, len)
		else if (len == maxSize - offset)
			bufferFull()
		else {
			shiftSuffixRight(offset + idx, 1)
			set(offset + idx, elem)
			len += 1
		}

	override def insertAll(idx :Int, elems :IterableOnce[E]) :Unit =
		if (idx < 0 | idx > len)
			outOfBounds_!(idx, len)
		else if (idx == len)
			addAll(elems)
		else if (!knownEmpty(elems)) {
			val elemsSize = elems.knownSize
			if (elemsSize > 0) {
				if (elemsSize > maxSize - offset - len)
					bufferFull()
				val i = offset + idx
				shiftSuffixRight(i, elemsSize)
				elems.toBasicOps.foldLeft(i) { (i, e) => set(i, e); i + 1 }
				len += elemsSize
			} else if (elemsSize < 0) {
				val suffix = drop(idx)
				len = idx
				try addAll(elems) catch {
					case e :BufferFullException => 
						len = idx
						addAll(suffix)
						throw e
				}
				addAll(suffix)
			}
		}

	override def remove(idx :Int) :E = {
		val res = apply(idx)
		shiftSuffixLeft(offset + idx + 1, 1)
		res
	}

	override def remove(idx :Int, count :Int) :Unit = {
		def msg =
			"BoundBuffer(" + errorString + ", " + offset + ".." + (offset + len) +
				").remove(" + idx + ", " + count + ")"
		if (count < 0)
			illegal_!(msg)
		else if (idx < 0 | idx > len - count)
			outOfBounds_!(msg)
		else if (idx + count < len)
			shiftSuffixLeft(offset + idx + count, count)
		len -= count
	}

	override def clear() :Unit = len = 0

	private def shiftSuffixLeft(from :Int, by :Int) :Unit = {
		var i   = from + by
		val end = offset + len
		while (i < end) {
			set(i - by, get(i))
			i += 1
		}
	}
	private def shiftSuffixRight(from :Int, by :Int) :Unit = {
		if (offset + len + by > maxSize)
			bufferFull()
		var i = offset + len
		while (i > from) {
			i -= 1
			set(i + by, get(i))
		}
	}

	private def bufferFull() =
		throw new BufferFullException("Cannot grow the buffer past indices [" + offset + ", " + maxSize + ")")

	protected def errorString :String
}


@SerialVersionUID(Ver)
private class BoundSeqBuffer[E](underlying :mutable.IndexedSeq[E], offset :Int, len :Int, max :Int)
	extends AbstractBoundBuffer[E](offset, len, max)
{	
	protected override def get(absoluteIdx :Int) :E = underlying(absoluteIdx)
	protected override def set(absoluteIdx :Int, elem :E) :Unit = underlying(absoluteIdx) = elem
	protected override def errorString :String = util.errorString(underlying)
	override def iterator :Iterator[E] = IndexedSeqIterator(underlying, startIndex, length)
	override def reverseIterator :Iterator[E] = ReverseIndexedSeqIterator(underlying, startIndex + length - 1, length)

}


@SerialVersionUID(Ver)
private class BoundArrayBuffer[E](underlying :Array[E], offset :Int, len :Int, max :Int)
	extends AbstractBoundBuffer[E](offset, len, max) with ArraySliceSeqOps[E, IndexedBuffer, IndexedBuffer[E]]
{ //todo: override methods to use elems.copyToArray
	protected override def array :Array[E] = underlying
	private[sugar] override def startIndex :Int = super.startIndex
	private[sugar] override def isMutable = true
	protected override def get(absoluteIdx :Int) :E = underlying(absoluteIdx)
	protected override def set(absoluteIdx :Int, elem :E) :Unit = underlying(absoluteIdx) = elem

	protected[this] override def newSpecific(array :Array[E], from :Int, until :Int) :IndexedBuffer[E] =
		new ArraySliceBuffer(until - from) ++= new ArraySlice(underlying, offset + from, offset + until)

	protected override def errorString :String = util.errorString(this)
}





/** A buffer backed by an [[scala.collection.mutable.IndexedSeq IndexedSeq]] or `Array`.
  * It is a view over a section of indices in the underlying collection, and cannot grow past the bounds
  * specified during its creation. The difference from [[net.noresttherein.sugar.collections.BoundBuffer BoundBuffer]]
  * is that prepending never shifts elements, only moves left the offset in the underlying array, where in the latter
  * the elements are shifted right to make space, like in an [[scala.collection.ArrayBuffer ArrayBuffer]].
  * If the [[net.noresttherein.sugar.collections.ViewBuffer.headIdx index of the first element]] equals
  * the [[net.noresttherein.sugar.collections.ViewBuffer.floor lower bound]], any attempt to prepend
  * will throw a [[net.noresttherein.sugar.collections.BufferFullException BufferFullException]], just like
  * an attempt to append when
  * [[net.noresttherein.sugar.collections.ViewBuffer.headIdx headIdx]]` + size == `[[net.noresttherein.sugar.collections.ViewBuffer.ceiling ceiling]].
  *
  * The only operations allowed to shift the elements are [[net.noresttherein.sugar.collections.ViewBuffer.insert insert]]
  * [[net.noresttherein.sugar.collections.ViewBuffer.insertAll insertAll]],
  * [[net.noresttherein.sugar.collections.ViewBuffer.insertBefore insertBefore]], and
  * [[net.noresttherein.sugar.collections.ViewBuffer.insertAllBefore insertAllBefore]].
  * The first pair shifts the elements following the index right, just as in `ArrayBuffer`, while the latter two
  * always shift the preceding elements left. If there is no space left after/before the contents,
  * a `BufferFullException` is thrown.
  */ //todo: MutSugaredSeqOps
trait ViewBuffer[E] extends IndexedBuffer[E] with SugaredSeqOps[E, IndexedBuffer, IndexedBuffer[E]] {
	/** The lower (inclusive) index bound in the underlying sequence for elements of this buffer. */
	def floor :Int

	/** The upper (exclusive) index bound in the underlying sequence for elements of this buffer. */
	def ceiling :Int

	/** The index of the first element of this buffer in the underlying sequence. */
	def headIdx :Int

	/** Same as `insert(idx, elem)`, but elements `0..idx-1` are shift left by `1`
	 *  to make room for the new element, rather than elements `idx..len-1` shift right by `1`.
	 */
	def insertBefore(idx :Int, elem :E) :Unit

	/** Same as `insertAll(idx, elems)`, but elements `0..idx-1` are shift left by `elems.size`
	 *  to make room for `elems`, rather than elements `idx..len-1` shift right by `elems.size`.
	 */
	def insertAllBefore(idx :Int, elems :IterableOnce[E]) :Unit

	/** Like `remove(idx)`, but instead of shifting the suffix left to close the gap, the prefix is shift right. */
	def removeShiftRight(idx :Int) :E

	/** Like `remove(idx)`, but instead of shifting the suffix left to close the gap, the prefix is shift right. */
	def removeShiftRight(idx :Int, count :Int) :Unit
}


@SerialVersionUID(Ver)
object ViewBuffer {
	def appending[E](seq :mutable.IndexedSeq[E], from :Int, until :Int) :ViewBuffer[E] = SeqViewBuffer.appending(seq, from, until)
	def appending[E](seq :mutable.IndexedSeq[E]) :ViewBuffer[E] = SeqViewBuffer.appending(seq)
	def appending[E](array :Array[E], from :Int, until :Int) :ViewBuffer[E] = ArrayViewBuffer.appending(array, from, until)
	def appending[E](array :Array[E]) :ViewBuffer[E] = ArrayViewBuffer.appending(array)

	def prepending[E](seq :mutable.IndexedSeq[E], from :Int, until :Int) :ViewBuffer[E] = SeqViewBuffer.prepending(seq, from, until)
	def prepending[E](seq :mutable.IndexedSeq[E]) :ViewBuffer[E] = SeqViewBuffer.prepending(seq)
	def prepending[E](array :Array[E], from :Int, until :Int) :ViewBuffer[E] = ArrayViewBuffer.prepending(array, from, until)
	def prepending[E](array :Array[E]) :ViewBuffer[E] = ArrayViewBuffer.prepending(array)

	def empty[E](seq :mutable.IndexedSeq[E], from :Int, until :Int, offset :Int) :ViewBuffer[E] = SeqViewBuffer.empty(seq, from, until, offset)
	def empty[E](seq :mutable.IndexedSeq[E], offset :Int) :ViewBuffer[E] = SeqViewBuffer.empty(seq, offset)
	def empty[E](array :Array[E], from :Int, until :Int, offset :Int) :ViewBuffer[E] = ArrayViewBuffer.empty(array, from, until, offset)
	def empty[E](array :Array[E], offset :Int) :ViewBuffer[E] = ArrayViewBuffer.empty(array, offset)

	def full[E](seq :mutable.IndexedSeq[E], from :Int, until :Int) :ViewBuffer[E] = SeqViewBuffer.full(seq, from, until)
	def full[E](seq :mutable.IndexedSeq[E]) :ViewBuffer[E] = SeqViewBuffer.full(seq)
	def full[E](array :Array[E], from :Int, until :Int) :ViewBuffer[E] = ArrayViewBuffer.full(array, from, until)
	def full[E](array :Array[E]) :ViewBuffer[E] = ArrayViewBuffer.full(array)
}


@SerialVersionUID(Ver)
private abstract class AbstractViewBuffer[E](lo :Int, hi :Int, private[this] var offset :Int, private[this] var len :Int)
	extends AbstractBuffer[E] with IndexedBuffer[E] with ViewBuffer[E]
	   with StrictOptimizedSeqOps[E, IndexedBuffer, IndexedBuffer[E]]
{
	override def knownSize :Int = len
	override def length    :Int = len
	override def floor     :Int = lo
	override def ceiling   :Int = hi
	override def headIdx   :Int = offset

	protected def get(absoluteIdx :Int) :E
	protected def set(absoluteIdx :Int, elem :E) :Unit

	override def apply(i :Int) :E =
		if (i < 0 | i >= len) outOfBounds_!(i, len)
		else get(offset + i)

	override def update(idx :Int, elem :E) :Unit =
		if (idx < 0 | idx >= len) outOfBounds_!(idx, len)
		else set(offset + idx, elem)

	override def addOne(elem :E) :this.type =
		if (len == hi - offset)
			bufferFull()
		else {
			set(offset + len, elem)
			len += 1
			this
		}

	override def prepend(elem :E) :this.type =
		if (offset == lo)
			bufferFull()
		else {
			offset -= 1
			len    += 1
			set(offset, elem)
			this
		}
	override def prependAll(elems :IterableOnce[E]) :this.type = {
		val elemsSize = elems.knownSize
		if (elemsSize > 0) {
			if (elemsSize > offset - lo)
				throw new BufferFullException(
					"Cannot prepend " + util.errorString(elems) + ": front capacity " +
						(offset - lo) + " exceeded."
				)
			offset -= elemsSize
			len    += elemsSize
			elems.toBasicOps.foldLeft(offset) { (i, e) => set(i, e); i + 1 }
		} else if (elemsSize < 0)
			prependAll(new ArrayBuffer[E] ++= elems)
		this
	}

	override def insert(idx :Int, elem :E) :Unit =
		if (idx < 0 | idx > len)
			outOfBounds_!(idx, len)
		else if (len == hi - offset)
			bufferFull()
		else {
			shiftSuffixRight(idx, 1)
			set(offset + idx, elem)
			len += 1
		}

	override def insertAll(idx :Int, elems :IterableOnce[E]) :Unit =
		if (idx < 0 | idx > len)
			outOfBounds_!(idx, len)
		else if (idx == len)
			addAll(elems)
		else if (!knownEmpty(elems)) {
			val elemsSize = elems.knownSize
			if (elemsSize > 0) {
				if (elemsSize > hi - offset - len)
					bufferFull()
				shiftSuffixRight(idx, elemsSize)
				elems.toBasicOps.foldLeft(offset + idx) { (i, e) => set(i, e); i + 1 }
				len += elemsSize
			} else if (elemsSize < 0) {
				val suffix = drop(idx)
				len = idx
				try addAll(elems) catch {
					case e :BufferFullException =>
						len = idx
						addAll(suffix)
						throw e
				}
				addAll(suffix)
			}
		}

	override def insertBefore(idx :Int, elem :E) :Unit =
		if (idx < 0 | idx > len)
			outOfBounds_!(idx, len)
		else if (len == offset - lo)
			bufferFull()
		else {
			shiftPrefixLeft(idx, 1)
			set(offset + idx, elem)
			offset -= 1
			len += 1
		}

	override def insertAllBefore(idx :Int, elems :IterableOnce[E]) :Unit =
		if (idx < 0 | idx > len)
			outOfBounds_!(idx, len)
		else if (idx == 0)
			prependAll(elems)
		else if (!knownEmpty(elems)) {
			val elemsSize = elems.knownSize
			if (elemsSize > 0) {
				if (elemsSize > offset - lo)
					bufferFull()
				shiftPrefixLeft(idx, elemsSize)
				elems.toBasicOps.foldLeft(offset + idx) { (i, e) => set(i, e); i + 1 }
				len += elemsSize
			} else if (elemsSize < 0) {
				val prefix = take(idx)
				val suffixLen = len - idx
				val absoluteIdx = offset + idx
				offset = absoluteIdx
				len = suffixLen
				try prependAll(elems) catch {
					case e :BufferFullException =>
						offset = absoluteIdx
						len = suffixLen
						prependAll(prefix)
						throw e
				}
				prependAll(prefix)
			}
		}

	override def remove(idx :Int) :E = {
		val res = apply(idx)
		shiftSuffixLeft(idx + 1, 1)
		len -= 1
		res
	}

	override def remove(idx :Int, count :Int) :Unit = {
		validateRemove(idx, count, "remove")
		shiftSuffixLeft(idx + count, count)
		len -= count
	}

	override def removeShiftRight(idx :Int) :E = {
		val res = apply(idx)
		shiftPrefixRight(idx, 1)
		offset += 1
		len    -= 1
		res
	}

	override def removeShiftRight(idx :Int, count :Int) :Unit = {
		validateRemove(idx, count, "removeShiftRight")
		shiftPrefixRight(idx, count)
		offset += count
		len    -= count
	}

	@inline private def validateRemove(idx :Int, count :Int, method :String) :Unit = {
		def msg =
			"BoundBuffer(" + errorString + ", " + offset + ".." + (offset + len) +
				")." + method + "(" + idx + ", " + count + ")"
		if (count < 0)
			illegal_!(msg)
		if (idx < 0 | idx > len - count)
			outOfBounds_!(msg)
	}

	override def clear() :Unit = len = 0

	private def shiftSuffixLeft(from :Int, by :Int) :Unit = {
		var i   = offset + from + by
		val end = lo + len
		while (i < end) {
			set(i - by, get(i))
			i += 1
		}
	}
	private def shiftSuffixRight(from :Int, by :Int) :Unit = {
		if (offset + len + by > hi)
			bufferFull()
		val end = offset + from
		var i   = offset + len
		while (i > end) {
			i -= 1
			set(i + by, get(i))
		}
	}
	private def shiftPrefixLeft(count :Int, by :Int) :Unit = {
		if (offset - len < by)
			bufferFull()
		var i = offset + count
		while (i > offset) {
			set(i - by, get(i))
			i -= 1
		}
	}
	private def shiftPrefixRight(count :Int, by :Int) :Unit = {
		var i   = offset + count
		val end = offset
		while (i > end) {
			i -= 1
			set(i + by, get(i))
		}
	}

	protected def errorString :String

	private def bufferFull() =
		throw new BufferFullException("Cannot grow the buffer past indices [" + lo + ", " + hi + ")")
}




private sealed abstract class SliceBufferFactory[S[_], C[_]] {
	protected def over[E](seq :S[E], from :Int, until :Int, loBound :Int, hiBound :Int) :C[E]
	protected def length[E](seq :S[E]) :Int

	def appending[E](seq :S[E], from :Int, until :Int) :C[E] = {
		validateRange(seq, from, until)
		over(seq, from, from, from, math.max(from, until))
	}
	def appending[E](seq :S[E]) :C[E] = {
		val len = length(seq)
		over(seq, 0, len, 0, len)
	}

	def prepending[E](seq :S[E], from :Int, until :Int) :C[E] = {
		validateRange(seq, from, until)
		val hi = math.max(from, until)
		over(seq, hi, hi, from, hi)
	}
	def prepending[E](seq :S[E]) :C[E] = {
		val len = length(seq)
		over(seq, len, len, 0, len)
	}

	def empty[E](seq :S[E], from :Int, until :Int, offset :Int) :C[E] = {
		validateRange(seq, from, until)
		if (offset < from | offset > until)
			outOfBounds_!(offset.toString + " out of [" + from + ", " + until + "] range")
		over(seq, offset, offset, from, math.max(until, from))
	}
	def empty[E](seq :S[E], offset :Int) :C[E] = {
		val len = length(seq)
		if (offset < 0 | offset > len)
			outOfBounds_!(
				offset.toString + " is not a valid index in " + seq + " (of length " + len + ")"
			)
		over(seq, offset, offset, 0, len)
	}

	def full[E](seq :S[E], from :Int, until :Int) :C[E] = {
		validateRange(seq, from, until)
		val until0 = math.max(until, from)
		over(seq, from, until0, from, until0)
	}
	def full[E](seq :S[E]) :C[E] = {
		val len = length(seq)
		over(seq, 0, len, 0, len)
	}

	private def validateRange(seq :S[_], from :Int, until :Int) :Unit = {
		val len = length(seq)
		if (from < 0 | from > len | until < 0 | until > len)
			outOfBounds_!(
				"[" + from + ", " + until + ") is not a valid index range in " + seq
			)
	}

}


@SerialVersionUID(Ver)
private object SeqViewBuffer extends SliceBufferFactory[mutable.IndexedSeq, SeqViewBuffer] {
	protected override def length[E](seq :mutable.IndexedSeq[E]) :Int = seq.length
	protected override def over[E](seq :mutable.IndexedSeq[E], from :Int, until :Int, loBound :Int, hiBound :Int)
			:SeqViewBuffer[E] =
				if (seq.isInstanceOf[StrictOptimizedSeqOps[_, generic.Any1, _]])
			new SeqViewBuffer[E](seq, loBound, hiBound, from, until - from)
				with StrictOptimizedSeqOps[E, IndexedBuffer, IndexedBuffer[E]]
		else
			new SeqViewBuffer[E](seq, loBound, hiBound, from, until - from)
}

@SerialVersionUID(Ver)
private sealed class SeqViewBuffer[E] private(underlying :mutable.IndexedSeq[E], lo :Int, hi :Int, offset :Int, len :Int)
	extends AbstractViewBuffer[E](lo, hi, offset, len)
{
	protected final override def get(absoluteIdx :Int) :E = underlying(absoluteIdx)
	protected final override def set(absoluteIdx :Int, elem :E) :Unit = underlying(absoluteIdx) = elem
	protected final override def errorString :String = util.errorString(underlying)
	override final def iterator :Iterator[E] = IndexedSeqIterator(underlying, headIdx, length)
	override final def reverseIterator :Iterator[E] = ReverseIndexedSeqIterator(underlying, headIdx + length - 1, length)
}


@SerialVersionUID(Ver)
private object ArrayViewBuffer extends SliceBufferFactory[Array, ArrayViewBuffer] {
	override protected def length[E](seq :Array[E]) :Int = seq.length
	protected override def over[E](seq :Array[E], from :Int, until :Int, loBound :Int, hiBound :Int) :ArrayViewBuffer[E] =
		new ArrayViewBuffer(seq, loBound, hiBound, from, until)
}

@SerialVersionUID(Ver)
private final class ArrayViewBuffer[E] private(underlying :Array[E], lo :Int, hi :Int, offset :Int, len :Int)
	extends AbstractViewBuffer[E](lo, hi, offset, len) with ArraySliceSeqOps[E, IndexedBuffer, IndexedBuffer[E]]
{   //todo: take advantage of copyToArray of added elements.
	protected override def array :Array[E] = underlying
	private[sugar] override def startIndex :Int = headIdx
	private[sugar] override def isMutable = true
	protected override def get(absoluteIdx :Int) :E = underlying(absoluteIdx)
	protected override def set(absoluteIdx :Int, elem :E) :Unit = underlying(absoluteIdx) = elem
	protected override def errorString :String = util.errorString(underlying)
	override def reverseIterator :Iterator[E] = ReverseArrayIterator(underlying, headIdx + length - 1, length)

	protected[this] override def newSpecific(array :Array[E], from :Int, until :Int) :IndexedBuffer[E] =
		if (array eq underlying) new ArrayViewBuffer(array.slice(from, until), 0, until - from, 0, until - from)
		else new ArrayViewBuffer(array, 0, array.length, from, until - from)
}




/** A factory of buffers appending values to another [[scala.collection.mutable.Buffer Buffer]].
  * Created buffer is a view on a suffix of the argument buffer past its size in the moment the appending buffer was created,
  * and do not allow the original to be modified before that index.
  */
@SerialVersionUID(Ver)
object AppendingBuffer {
	def apply[E](buffer :Buffer[E]) :Buffer[E] = buffer match {
		case indexed :IndexedBuffer[E]         => apply(indexed)
		case _ :StrictOptimizedSeqOps[_, _, _] =>
			new AppendingBuffer[E](buffer, buffer.length) with StrictOptimizedSeqOps[E, Buffer, Buffer[E]]
		case _ =>
			new AppendingBuffer(buffer, buffer.length)
	}

	def apply[E](buffer :IndexedBuffer[E]) :IndexedBuffer[E] =
		if (buffer.isInstanceOf[StrictOptimizedSeqOps[_, generic.Any1, _]])
			new AppendingIndexedBuffer[E](buffer, buffer.length)
				with StrictOptimizedSeqOps[E, IndexedBuffer, IndexedBuffer[E]]
		else
			new AppendingIndexedBuffer[E](buffer, buffer.length)
}


@SerialVersionUID(Ver)
private class AppendingBuffer[E] protected (underlying :Buffer[E], offset :Int) extends AbstractBuffer[E] {
	def this(underlying :Buffer[E]) = this(underlying, underlying.length)

	override def length :Int = {
		val len = underlying.length
		if (len < offset)
			concurrent_!("The underlying buffer has been shrunk below " + offset + ": " + len + ".")
		len - offset
	}
	override def knownSize :Int = length

	override def apply(idx :Int) :E = {
		validateOffset()
		validateIdx(idx)
		underlying(offset + idx)
	}
	override def update(idx :Int, elem :E) :Unit = {
		validateOffset()
		validateIdx(idx)
		underlying(offset + idx) = elem
	}

	//These may fail if elems is this, underlying, or its iterator, but so is life.
	override def addOne(elem :E) :this.type = { underlying.addOne(elem); this }
	override def addAll(elems :IterableOnce[E]) :this.type = { underlying.addAll(elems); this }
	override def prepend(elem :E) :this.type = { underlying.insert(offset, elem); this }
	override def prependAll(elems :IterableOnce[E]) :this.type = { underlying.insertAll(offset, elems); this }
	
	override def insert(idx :Int, elem :E) :Unit = {
		validateIdx(idx)
		validateOffset()
		underlying.insert(offset + idx, elem)
	}
	override def insertAll(idx :Int, elems :IterableOnce[E]) :Unit = {
		validateIdx(idx)
		validateOffset()
		underlying.insertAll(offset + idx, elems)
	}

	override def remove(idx :Int) :E = {
		validateIdx(idx)
		validateOffset()
		underlying.remove(offset + idx)
	}
	override def remove(idx :Int, count :Int) :Unit = {
		validateIdx(idx)
		validateOffset()
		underlying.remove(offset + idx, count)
	}

	override def patchInPlace(from :Int, patch :IterableOnce[E], replaced :Int) :this.type = {
		val len = underlying.length
		if (len < offset)
			concurrent_!("The underlying buffer has been shrunk below " + offset + ": " + len + ".")
		val from0 =
			if (from > Int.MaxValue - offset) Int.MaxValue
			else offset + math.max(from, 0)
		underlying.patchInPlace(from0, patch, replaced)
		this
	}

	override def clear() :Unit = {
		val len = underlying.length
		if (len < offset)
			concurrent_!("The underlying buffer has been shrunk below " + offset + ": " + len + ".")
		remove(offset, len - offset)
	}

	override def iterator :Iterator[E] = {
		validateOffset()
		underlying.iterator.drop(offset)
	}
	override def reverseIterator :Iterator[E] = {
		val len = underlying.length
		if (len < offset)
			concurrent_!("The underlying buffer has been shrunk below " + offset + ": " + len + ".")
		underlying.reverseIterator.take(len - offset)
	}

	private def validateIdx(idx :Int) :Unit = {
		if (idx < 0)
			outOfBounds_!(idx.toString + " is less than zero")
		if (idx > Int.MaxValue - offset)
			outOfBounds_!(idx)
	}
	@inline private def validateOffset() :Unit = {
		val len = underlying.length
		if (len < offset)
			concurrent_!("The underlying buffer has been shrunk below " + offset + ": " + len + ".")
	}
}


@SerialVersionUID(Ver)
private class AppendingIndexedBuffer[E](underlying :IndexedBuffer[E], offset :Int)
	extends AppendingBuffer[E](underlying, offset) with IndexedBuffer[E]
{
	//Methods with override conflicts because IndexedBuffer doesn't declare them as override :(
	override def knownSize :Int = length
	override def iterator :Iterator[E] = super[AppendingBuffer].iterator
	override def patchInPlace(from :Int, patch :IterableOnce[E], replaced :Int) :this.type =
		super[AppendingBuffer].patchInPlace(from, patch, replaced)
}




/** A factory of buffers prepending values to another [[scala.collection.mutable.Buffer Buffer]].
  * Created buffer is a view on a prefix of the argument buffer, and do not allow the original to be modified
  * past the number of elements prepended to it through the decorator buffer.
  */
@SerialVersionUID(Ver)
object PrependingBuffer {
	def apply[E](buffer :Buffer[E]) :Buffer[E] = buffer match {
		case indexed :IndexedBuffer[E] => apply(indexed)
		case _ :StrictOptimizedSeqOps[_, _, _] =>
			new PrependingBuffer[E](buffer, buffer.length) with StrictOptimizedSeqOps[E, Buffer, Buffer[E]]
		case _ =>
			new PrependingBuffer(buffer, buffer.length)
	}

	def apply[E](buffer :IndexedBuffer[E]) :IndexedBuffer[E] =
		if (buffer.isInstanceOf[StrictOptimizedSeqOps[_, generic.Any1, _]])
			new PrependingIndexedBuffer[E](buffer, buffer.length)
				with StrictOptimizedSeqOps[E, IndexedBuffer, IndexedBuffer[E]]
		else
			new PrependingIndexedBuffer(buffer, buffer.length)
}


@SerialVersionUID(Ver)
private class PrependingBuffer[E] protected (underlying :Buffer[E], suffix :Int) extends AbstractBuffer[E] {
	def this(underlying :Buffer[E]) = this(underlying, underlying.length)

	override def length :Int = {
		val len = underlying.length
		if (len < suffix)
			concurrent_!("The underlying buffer has been shrunk below " + suffix + ": " + len + ".")
		len - suffix
	}
	override def knownSize :Int = length

	override def apply(idx :Int) :E = {
		validateIdx(idx, 0)
		underlying(idx)
	}
	override def update(idx :Int, elem :E) :Unit = {
		validateIdx(idx, 0)
		underlying(idx) = elem
	}

	override def addOne(elem :E) :this.type = { 
		val len = underlying.length
		if (len < suffix)
			concurrent_!("The underlying buffer has been shrunk below " + suffix + ": " + len + ".")
		underlying.insert(len - suffix, elem)
		this 
	}
	override def addAll(elems :IterableOnce[E]) :this.type = {
		val len = underlying.length
		if (len < suffix)
			concurrent_!("The underlying buffer has been shrunk below " + suffix + ": " + len + ".")
		underlying.insertAll(len - suffix, elems)
		this
	}
	override def prepend(elem :E) :this.type = { underlying.prepend(elem); this }
	override def prependAll(elems :IterableOnce[E]) :this.type = { underlying.prependAll(elems); this }

	override def insert(idx :Int, elem :E) :Unit = {
		validateIdx(idx, 1)
		underlying.insert(idx, elem)
	}
	override def insertAll(idx :Int, elems :IterableOnce[E]) :Unit = {
		validateIdx(idx, 1)
		underlying.insertAll(idx, elems)
	}

	override def remove(idx :Int) :E = {
		validateIdx(idx, 0)
		underlying.remove(idx)
	}
	override def remove(idx :Int, count :Int) :Unit = {
		validate.nonNegative("Removed element count", count)
		validateIdx(idx, -count)
		underlying.remove(idx, count)
	}

	override def patchInPlace(from :Int, patch :IterableOnce[E], replaced :Int) :this.type = {
		val len = underlying.length
		if (len < suffix)
			concurrent_!("The underlying buffer has been shrunk below " + suffix + ": " + len + ".")
		val from0 = math.max(0, math.min(from, len - suffix))
		val replaced0 = math.min(replaced, len - suffix - from0)
		underlying.patchInPlace(from0, patch, replaced0)
		this
	}

	override def clear() :Unit = {
		val len = underlying.length
		if (len < suffix)
			concurrent_!("The underlying buffer has been shrunk below " + suffix + ": " + len + ".")
		underlying.remove(0, len - suffix)
	}

	override def iterator :Iterator[E] = {
		val len = underlying.length
		if (len < suffix)
			concurrent_!("The underlying buffer has been shrunk below " + suffix + ": " + len + ".")
		underlying.iterator.take(len - suffix)
	}
	override def reverseIterator :Iterator[E] = {
		val len = underlying.length
		if (len < suffix)
			concurrent_!("The underlying buffer has been shrunk below " + suffix + ": " + len + ".")
		underlying.reverseIterator.drop(suffix)
	}

	private def validateIdx(idx :Int, inclusive :Int) :Unit = {
		val len = underlying.length
		if (len < suffix)
			concurrent_!("The underlying buffer has been shrunk below " + suffix + ": " + len + ".")
		if (idx < 0 || idx >= len - suffix + inclusive)
			outOfBounds_!(idx.toString + " is less than zero")
	}	
}

@SerialVersionUID(Ver)
private sealed class PrependingIndexedBuffer[E](underlying :IndexedBuffer[E], suffix :Int)
	extends PrependingBuffer[E](underlying, suffix) with IndexedBuffer[E]
{
	final override def knownSize :Int = length
	final override def iterator :Iterator[E] = super[PrependingBuffer].iterator
	final override def patchInPlace(from :Int, patch :IterableOnce[E], replaced :Int) :this.type =
		super[PrependingBuffer].patchInPlace(from, patch, replaced)
}






/** An exception thrown when elements are added to a buffer which does not have enough capacity to accommodate them. */
@SerialVersionUID(Ver)
class BufferFullException(msg :String, cause :Throwable = null) extends MaxSizeReachedException(msg, null, cause) {
	def this() = this("buffer full")
}
