package net.noresttherein.sugar.collections

import java.lang.System.arraycopy

import scala.annotation.tailrec
import scala.collection.generic.DefaultSerializable
import scala.collection.{Factory, IterableFactoryDefaults, SeqFactory, StrictOptimizedSeqOps, mutable}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer.DefaultInitialSize
import scala.collection.mutable.{AbstractBuffer, IndexedBuffer}

import net.noresttherein.sugar.arrays.{ArrayCompanionExtension, ArrayIterator, ArrayLikeSpecOps, IRefArray, RefArray, ReverseArrayIterator}
import net.noresttherein.sugar.exceptions.{illegalState_!, illegal_!, outOfBounds_!}
import net.noresttherein.sugar.casting.castingMethods
import net.noresttherein.sugar.collections.ArraySliceBuffer.AcceptableFillFactor
import net.noresttherein.sugar.collections.CompanionFactory.sourceCollectionFactory
import net.noresttherein.sugar.collections.Constants.MaxArraySize
import net.noresttherein.sugar.collections.extensions.IterableOnceExtension
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.concurrent.Fences.releaseFence
import net.noresttherein.sugar.vars.Maybe.Yes




/** A buffer implementation very similar to the standard [[scala.collection.mutable.ArrayBuffer ArrayBuffer]],
  * but able to grow in both directions. The first element of the buffer is not anchored at the first position
  * of the array, but may occur at any offset, and prepending simply adjusts the offset and writes the element
  * before the current contents, or resizes the array, if needed. This makes prepend and append operations symmetrical,
  * and both offer `O(n)` performance. The only operations actually shifting the contents are `remove`,
  * `insert`, and `insertAll`; the
  *
  * The data is not wrapped around in the array, like in [[scala.collection.mutable.ArrayDeque ArrayDeque]],
  * but always forms a coherent slice; this makes it possible for this buffer to ''alias'' its contents,
  * similarly to [[scala.collection.mutable.ListBuffer ListBuffer]]: calling
  * [[net.noresttherein.sugar.collections.ArraySliceBuffer.toSeq toSeq]] or
  * [[net.noresttherein.sugar.collections.ArraySliceBuffer.toIndexedSeq toIndexedSeq]] will create a sequence backed
  * but the very same array used by this buffer, providing the contents meet a required fill factor requirement.
  *
  * Together, this makes it the most efficient way to create a sequence by prepending elements,
  * especially if the size is initially known.
  * @define Coll `ArraySliceBuffer`
  * @define coll array buffer
  * @author Marcin Mościcki
  */ //consider: using a ClassTag and creating specific and untagged factories like in MatrixBuffer
@SerialVersionUID(Ver)
final class ArraySliceBuffer[E] private (private[this] var contents :RefArray[E],
                                         private[this] var offset :Int, private[this] var len :Int)
	extends AbstractBuffer[E] with IndexedBuffer[E] with mutable.IndexedSeqOps[E, ArraySliceBuffer, ArraySliceBuffer[E]]
	   with collection.StrictOptimizedSeqOps[E, ArraySliceBuffer, ArraySliceBuffer[E]]
	   with ArraySliceSeqOps[E, ArraySliceBuffer, ArraySliceBuffer[E]]
	   with IterableFactoryDefaults[E, ArraySliceBuffer] with DefaultSerializable
{
	def this(initialCapacity :Int) =
		this(if (initialCapacity > 0) RefArray.ofDim[E](initialCapacity) else RefArray.empty[E], 0, 0)

	def this() = this(RefArray.empty, 0, 0)

	private[this] var aliased :Boolean = false

	protected override def array :Array[E] = contents.asInstanceOf[Array[E]]
	private[sugar] override def startIndex :Int = offset
	private[sugar] override def isMutable = true

	override def length    :Int = len
	override def knownSize :Int = len

	/** Ensures the capacity for the specified number of elements in the buffer, reallocating the array if necessary.
	  * It will not shrink a buffer with already higher capacity.
	  */
	def sizeHint(totalSize :Int) :Unit =
		if (totalSize > len) {
			if (totalSize < contents.length) {
				if (offset + totalSize <= contents.length) {
					contents = RefArray.copyOfRange(contents, offset, offset + len, totalSize)
					offset = 0
				} else {
					val newOffset = contents.length - totalSize
					contents = RefArray.copyOfRange(contents, offset, offset + len, newOffset, totalSize)
				}
			} else if (totalSize > contents.length) {
				contents  = RefArray.copyOfRange(contents, offset, offset + len, totalSize)
				offset = 0
			}
		}

	override def segmentLength(p :E => Boolean, from :Int) :Int = //override clash
		ArrayLikeSpecOps.segmentLength(contents.asInstanceOf[Array[E]], offset, len)(p, from)

	protected override def clippedSlice(from :Int, until :Int) :ArraySliceBuffer[E] = {
		val length = until - from
		val slice  = RefArray.copyOfRange(contents, from, until, math.max(DefaultInitialSize, length))
		new ArraySliceBuffer(slice, 0, length)
	}

	protected[this] override def newSpecific(array :Array[E], from :Int, until :Int) :ArraySliceBuffer[E] =
		new ArraySliceBuffer[E](array.asInstanceOf[RefArray[E]], from, until - from)

	@inline private def shouldShiftNotGrow(extra :Int) :Boolean = {
		val length = contents.length
		len + extra <= (length >> 1) - (length >> 3) //if current + new data doesn't exceed 3/8 (a bit more than 1/3)
	}

	private def newSizeForExtra(n :Int) :Int = {
		def illegal_!() =
			throw new BufferFullException("Cannot allocate an array of length " + (len.toLong + n) + ".")
		if (n >= MaxArraySize - len)
			illegal_!()
		val total = len + n
		val res = math.max(math.min(contents.length, MaxArraySize >> 1) << 1, math.max(total, DefaultInitialSize))
		if (res < total)
			illegal_!()
		res
	}

	override def apply(i :Int) :E =
		if (i < 0 | i >= len)
			outOfBounds_!(i, 0, len)
		else
			contents(offset + i)

	override def update(idx :Int, elem :E) :Unit =
		if (idx < 0 | idx >= len)
			outOfBounds_!(idx, 0, len)
		else {
			if (aliased) {
				contents   = RefArray.copyOf(contents)
				aliased = false
			}
			contents(offset + idx) = elem
		}

	override def addOne(elem :E) :this.type = {
		val oldArray = contents
		var length = oldArray.length
		if (aliased || offset + len == length) {
			if (shouldShiftNotGrow(1)) {
				val oldOffset = offset
				if (aliased) {
					length = math.max(length, DefaultInitialSize)
					contents = RefArray.ofDim[E](length)
				}
				if (offset + len + 1 >= length - len)
					offset = (length - len - 1 >> 1) + 1
				arraycopy(oldArray, oldOffset, contents, offset, len)
			} else if (offset == 0) {
				length = math.max((len << 1) + offset, DefaultInitialSize)
				contents = RefArray.copyOf(contents, length)
			} else {
				length = math.max((len << 1) + offset, DefaultInitialSize)
				contents  = RefArray.ofDim[E](length)
				arraycopy(oldArray, offset, contents, offset, len)
			}
			aliased = false
		}
		contents(offset + len) = elem
		len += 1
		this
	}

	override def addAll(xs :IterableOnce[E]) :this.type = { //todo: optimize addAll
		insertAll(len, xs)
		this
	}

	override def prepend(elem :E) :this.type = {
		if (offset == 0 | aliased) {
			val oldArray = contents
			var length   = oldArray.length
			if (shouldShiftNotGrow(1)) {
				val oldOffset = offset
				if (aliased) {
					length = math.max(length, DefaultInitialSize)
					contents  = RefArray.ofDim[E](length)
				}
				if (len == 0)
					offset = length
				else if (offset <= len + 1)
					offset = length - (length - len - 1 >> 1) - len + 1
				arraycopy(oldArray, oldOffset, contents, offset, len)
			} else {
				length = math.max(len + length + 1, DefaultInitialSize)
				contents  = RefArray.copyOfRange(oldArray, 0, len, len + 1, length)
				offset = len + 1
			}
			aliased = false
		}
		offset -= 1
		len    += 1
		contents(offset) = elem
		this
	}

	override def prependAll(elems :IterableOnce[E]) :this.type = { //todo: optimize prependAll
		insertAll(0, elems)
		this
	}

	override def insert(idx :Int, elem :E) :Unit = {
		shiftAside(idx, 1)
		contents(offset + idx) = elem
	}


	@tailrec override def insertAll(idx :Int, elems :IterableOnce[E]) :Unit =
		if (this eq elems.asAnyRef)
			insertAll(idx, ArrayBufferFactory.ofCapacity(len) ++= this)
		else elems.knownSize match {
			case -1 =>
				if (idx < 0 | idx > len)
					outOfBounds_!(errorString(this) + ".insertAll(" + idx + ", _)")
				var suffix       = contents
				var suffixOffset = offset + idx
				val suffixLength = len - idx
				val newLength    = if (len < suffix.length) suffix.length else newSizeForExtra(1)
				if (aliased || idx < (len >> 1) || newLength > suffix.length) {
					//Keep the current array as the suffix, and create a new one for the prefix.
					contents = RefArray.copyOfRange(suffix, offset, offset + idx, offset, newLength)
				} else {
					//Copy the suffix aside, drop it, and append to this array.
					suffixOffset = 0
					suffix = contents.slice(offset + idx, offset + len)
					contents  = RefArray.copyOfRange(contents, offset, offset + idx, newLength)
					offset = 0
				}
				len = idx
				elems.toBasicOps.foldLeft(this)((buff, elem) => buff += elem)
				if (offset + len > contents.length - suffixLength)
					shiftAside(len, suffixLength)
				else
					len += suffixLength
				arraycopy(suffix, suffixOffset, contents, offset + len - suffixLength, suffixLength)
			case  0 =>
				if (idx < 0 || idx > len)
					outOfBounds_!(
						errorString(this) + ".insertAll(" + idx + ", " + errorString(elems) + ")"
					)
			case  n =>
				shiftAside(idx, n)
				val copied = elems.toBasicOps.copyToArray(contents.asAnyArray, offset + idx)
				if (copied != n)
					illegalCopiedCount(idx, n, copied)
		}

	/** Shifts the contents in the buffer, adding `space` uninitialized entries after `idx`.
	  * This may shift the prefix left, shift the suffix right, allocate a new array and copy the contents,
	  * or any combination of the above. After the method returns the buffer size is increased by `space`,
	  * and it is safe to call `copyToArray(this.array, offset + idx, space)`.
	  */
	private def shiftAside(idx :Int, space :Int) :Unit = {
		def outOfBounds() =
			outOfBounds_!(
				"Cannot insert " + space + " elements at " + idx + " to " + errorString(this)
			)
		if (idx < 0 | idx > len)
			outOfBounds()
		val oldArray  = contents
		val length    = oldArray.length
		val oldOffset = offset
		val freeBackSpace = length - offset - len
		if (length - len >= space) { //consider: a more aggressive growth
			if (aliased) {
				val newLength = math.max(length, DefaultInitialSize)
				contents = RefArray.ofDim[E](newLength)
				offset = newLength - len - space >> 1
				arraycopy(oldArray, oldOffset, contents, offset, idx)
				arraycopy(oldArray, oldOffset + idx, contents, offset + idx + space, len - idx)
			} else if (offset >= space && (idx < (len >> 1) || space > freeBackSpace)) {
				//more space in the front than in the back
				arraycopy(contents, offset, contents, offset - space, idx)
				offset -= space
			} else if (space <= freeBackSpace)
				//more space in the back than in the front
				arraycopy(contents, offset + idx, contents, offset + idx + space, len - idx)
			else {
				arraycopy(contents, offset, contents, 0, idx)
				arraycopy(contents, offset + idx, contents, idx + space, len - idx)
				offset = 0
			}
		} else if (offset > 0 & freeBackSpace == 0) {          //preserve right alignment
			val newLength = newSizeForExtra(space)
			contents  = RefArray.ofDim[E](newLength)
			offset = newLength - len - space
			arraycopy(oldArray, oldOffset, contents, offset, idx)
			arraycopy(oldArray, oldOffset + idx, contents, newLength - (len - idx), len - idx)
		} else if (offset == 0) { //extracted to avoid div by zero and for the performance of copyOf
			val newLength = newSizeForExtra(space)
			contents = RefArray.copyOfRange[E](contents, 0, idx, newLength)
			arraycopy(oldArray, oldOffset + idx, contents, idx + space, len - idx)
		} else {
			val newLength = newSizeForExtra(space)
			val freeSpace = newLength - len - space
			val newBackSpace = (freeSpace.toLong * freeBackSpace / (length - len)).toInt  //divide the space proportionally
			offset = newLength - len - space - newBackSpace
			contents  = RefArray.ofDim[E](newLength)
			arraycopy(oldArray, oldOffset, contents, offset, idx)
			arraycopy(oldArray, oldOffset + idx, contents, offset + idx + space, len - idx)
		}
		len    += space
		aliased = false
	}

	override def remove(idx :Int) :E = {
		val res = contents(offset + idx) //even if out of range, the following remove will throw an IOOBE.
		remove(idx, 1)
		res
	}

	override def remove(idx :Int, count :Int) :Unit = {
		def illegal() = //extracted to minimize the size out of the outer method
			illegal_!(errorString(this) + ".remove(" + idx + ", " + count + ")")
		def outOfBounds() =
			outOfBounds_!(errorString(this) + ".remove(" + idx + ", " + count + ")")
		if (count < 0)
			illegal()
		if (count > 0) {
			val end = idx + count
			if (idx < 0 || idx > len - count)
				outOfBounds()
			if (idx < len - end) {
				arraycopy(contents, offset, contents, offset + count, idx)
				contents.clear(offset, offset + count)
				offset += count
			} else {
				arraycopy(contents, offset + end, contents, offset + idx, len - end)
				contents.clear(offset + len - count, offset + len)
			}
			len -= count
		}
	}

	override def padToInPlace(len: Int, elem: E): this.type = {
		if (len > this.len)
			addAll(ConstSeq(elem, len - this.len))
		this
	}

	override def patchInPlace(from :Int, patch :IterableOnce[E], replaced :Int) :this.type = {
		val from0 = clip(from)
		if (from0 == len)
			addAll(patch)
		else if (replaced <= 0) {
			insertAll(from0, patch)
			this
		} else if (replaced >= len - from0) {
			remove(from0, len - from0)
			addAll(patch)
		} else patch.knownSize match {
			case -1 =>
				super.patchInPlace(from, patch, replaced)
			case  0 =>
				remove(from0, replaced)
				this
			case  n =>
				if (replaced < n)
					shiftAside(from0, n - replaced)
				else if (from0 < len - replaced - from0)
					remove(from0, replaced - n)
				else
					remove(from0 + n, replaced - n)
				val copied = patch.toBasicOps.copyToArray(contents.asAnyArray, offset + from0)
				if (copied != n)
					illegalCopiedCount(from0, n, copied)
				this
		}
	}

	private def illegalCopiedCount(idx :Int, declared :Int, copied :Int) :Nothing =
		illegalState_!(
			"Copied " + copied + " instead of the declared " + declared + " into " + errorString(this) + " at " + idx + "."
		)

	@inline private def clip(n :Int) :Int = math.min(math.max(n, 0), len)

	override def clear() :Unit = remove(0, len)

	def clearAndShrink(size: Int = DefaultInitialSize): this.type = {
		val size0 = math.max(size, 0)
		if (size0 < contents.length) {
			contents  = RefArray.ofDim(size0)
			offset = 0
			len    = 0
		} else
			remove(0, len)
		this
	}

	override def iterator :Iterator[E] =
		if (len == 0) Iterator.empty else ArrayIterator(contents.castFrom[RefArray[E], Array[E]], offset, len)

	override def reverseIterator :Iterator[E] =
		if (len == 0) Iterator.empty
		else ReverseArrayIterator.slice(contents.castFrom[RefArray[E], Array[E]], offset, offset + len)

	/** Same as [[net.noresttherein.sugar.collections.ArraySliceBuffer.toIndexedSeq toIndexedSeq]]. */
	override def toSeq        :Seq[E] = toIndexedSeq

	/** Creates an array-backed indexed sequence. If the fill factor is above a certain percentage (a minimum of half
	  * the array size), the returned instance will wrap the array used by this buffer, and the buffer will copy it
	  * on subsequent write attempts. The exact type of the returned sequence is defined
	  * by [[net.noresttherein.sugar.collections.DefaultArraySeq DefaultArraySeq]] factory.
	  */
	override def toIndexedSeq :IndexedSeq[E] =
		if (len == 0)
			IndexedSeq.empty
		else if (canAlias) {
			markAliased()
			DefaultArraySeq.slice(contents.unsafeIRefArray, offset, offset + len)
		} else
			DefaultArraySeq.wrap(IRefArray.copyOfRange(contents, offset, offset + len))

	override def to[C1](factory :Factory[E, C1]) :C1 = sourceCollectionFactory(factory) match {
		case Yes(companion) => companion match {
			case Seq | IndexedSeq | collection.Seq | collection.IndexedSeq => toIndexedSeq.castFrom[IndexedSeq[E], C1]
			case ArrayLikeSlice | IArrayLikeSlice | IRefArraySlice if canAlias =>
				markAliased()
				IRefArraySlice.slice(contents.unsafeIRefArray, offset, offset + len).castFrom[IRefArraySlice[E], C1]
			case ArraySeq if offset == 0 && contents != null && len == contents.length =>
				markAliased()
				ArraySeq.unsafeWrapArray(contents.asAnyArray).castFrom[ArraySeq[Any], C1]
			case _ if canAlias && RelayArrayFactory.isDefined && RelayArrayFactory == companion =>
				markAliased()
				RelayArrayFactory.get.slice(contents.unsafeIRefArray, offset, offset + len).castFrom[IndexedSeq[E], C1]
			case _ => super.to(factory)
		}
		case _ => super.to(factory)
	}
	@inline private def canAlias :Boolean = len >= contents.length * 100L / AcceptableFillFactor
	@inline private def markAliased() :Unit = {
		aliased = true
		releaseFence()
	}
//			IRefArraySlice.of(array.slice(offset, offset + len).asIRefArray)

	override def iterableFactory :SeqFactory[ArraySliceBuffer] = ArraySliceBuffer
	protected override def className :String = "ArraySliceBuffer"

	override def toString :String =
		mkString("ArraySliceBuffer|" + offset + "-" + (offset + length) + "/" + contents.length + "|(", ", ", ")")
}


/** A factory of alternative array buffers, which can grow and shrink both from the beginning and the end of an array,
  * and able to [[net.noresttherein.sugar.collections.ArraySliceBuffer.toSeq share]] the underlying array
  * with created immutable sequences.
  * @define Coll `ArraySliceBuffer`
  * @define coll array buffer
  */
@SerialVersionUID(Ver)
case object ArraySliceBuffer extends BufferFactory[ArraySliceBuffer] {
	override def ofCapacity[E](capacity :Int) :ArraySliceBuffer[E] = new ArraySliceBuffer[E](capacity)

	override def empty[E] :ArraySliceBuffer[E] = new ArraySliceBuffer[E]

	private final val AcceptableFillFactor = 50L
	private final val MinGrowThreshold     = 75L
}
