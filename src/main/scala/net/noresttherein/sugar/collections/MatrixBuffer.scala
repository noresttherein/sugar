package net.noresttherein.sugar.collections

import java.lang.{Math => math}
import java.lang.System.arraycopy

import scala.Int.{MaxValue, MinValue}
import scala.annotation.tailrec
import scala.collection.Stepper.EfficientSplit
import scala.collection.{EvidenceIterableFactory, EvidenceIterableFactoryDefaults, SeqFactory, Stepper, StepperShape, StrictOptimizedClassTagSeqFactory, StrictOptimizedSeqFactory, mutable}
import scala.collection.generic.DefaultSerializable
import scala.collection.mutable.{AbstractBuffer, Builder, IndexedBuffer}
import scala.reflect.ClassTag
import scala.util.Sorting

import net.noresttherein.sugar.JavaTypes.JIterator
import net.noresttherein.sugar.arrays.{ArrayCompanionExtension, ArrayLike, ArrayLikeOps, CyclicArrayIterator, MutableArrayExtension, ReverseCyclicArrayIterator}
import net.noresttherein.sugar.arrays.extensions.{ArrayExtension, ArrayCompanionExtension}
import net.noresttherein.sugar.casting.{cast2TypeParamsMethods, castTypeParamMethods}
import net.noresttherein.sugar.collections.MatrixBuffer.{Dim1Bits, Dim1Mask, MatrixDim2BufferIterator, MaxDim2, MaxSize1, MaxSize2, MinSize1, MinSize2, NewSize1, NewSize2, ReverseDim2MatrixBufferIterator, SpacerValues, dim1, dim2}
import net.noresttherein.sugar.collections.extensions.{IterableExtension, IterableOnceExtension, IteratorExtension, StepperCompanionExtension}
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.exceptions.{??!, noSuch_!, outOfBounds_!}
import net.noresttherein.sugar.numeric.extensions.IntExtension
import net.noresttherein.sugar.reflect.extensions.ClassExtension




/** An array buffer switching to a two dimensional array if it's size exceeds a certain threshold.
  * It has similar characteristics to an [[collection.mutable.ArrayBuffer ArrayBuffer]], including very fast `O(1)`
  * random access, and amortized `O(1)` prepend/append and removal from either end, while avoiding allocation
  * of arrays of size which might pose a challenge for the JVM's heap management. Up until $MaxSize1 elements,
  * it behaves exactly like a resizable, cyclic array buffer. Once that limit is reached, instead of growing
  * the underlying array even further, it allocates an `Array[Array[E]]`, storing in it the current `Array[E]`.
  * From this point on, unless the fill factor drops again enough to switch back to a single dimensional array,
  * growing the buffer happens through an allocation of an additional `Array[E]` of size $MaxSize1.
  *
  * Unlike `ArrayBuffer`, but similarly to [[scala.collection.mutable.ArraySeq mutable.ArraySeq]], this implementation
  * can store values in arrays of arbitrary type, based on [[scala.reflect.ClassTag ClassTag]]`[E]`.
  * However, companion object [[net.noresttherein.sugar.collections.MatrixBuffer$ MatrixBuffer]] creates, by default,
  * 'untagged' instances backed by an `Array[Any]`, boxing all elements. The associated
  * [[collection.ClassTagIterableFactory ClassTagIterableFactory]] for this class is instead
  * `MatrixBuffer.`[[net.noresttherein.sugar.collections.MatrixBuffer.specific specific]].
  *
  * All methods adding elements to the buffer
  * will throw a [[net.noresttherein.sugar.collections.BufferFullException BufferFullException]] if the total number
  * of elements would exceed `Int.MaxValue`.
  *
  * The goal of the implementation is to preserve all the advantages of an array buffer, all the while making
  * it safe to use even if the size may approach the maximum of `Int.MaxValue`.
  * @param initialCapacity makes the buffer pre-reserve internal array(s) for this many elements. If the number exceeds
  *                        a single array limit, the outer array size is based on the capacity,
  *                        but only two inner arrays are actually reserved.
  *                        Passing a negative value will result in a not initialized buffer, which delays creating
  *                        the arrays until elements are actually added.
  * @param shrink          a flag specifying if the buffer should shrink the underlying storage if its size drops
  *                        below a certain fill factor threshold.
  * @define Coll     `MatrixBuffer`
  * @define coll     matrix buffer
  * @define MaxSize1 65536
  * @author Marcin Mościcki
  */ //consider: renaming to Array2Buffer; specializing
@SerialVersionUID(Ver)
sealed class MatrixBuffer[E](initialCapacity :Int, shrink :Boolean)(implicit override val iterableEvidence :ClassTag[E])
	extends AbstractBuffer[E] with IndexedBuffer[E] with mutable.IndexedSeqOps[E, MatrixBuffer, MatrixBuffer[E]]
	   with SugaredIterable[E] with SugaredIterableOps[E, MatrixBuffer, MatrixBuffer[E]]
	   with collection.StrictOptimizedSeqOps[E, MatrixBuffer, MatrixBuffer[E]]
	   with EvidenceIterableFactoryDefaults[E, MatrixBuffer, ClassTag]
	   with DefaultSerializable
{
	/* The implementation relies heavily on binary arithmetics and properties of the power of two complement system
	* of integer values. Because the lengths of all arrays are always powers of two, then length - 1 becomes a mask
	* for all positive values lesser than length (that is, valid indices). Moreover, x & length - 1 is the same
	* as x % length (but faster), and -x & length -1 is the complement of that remainder (so, if x is an array index,
	* -x & length - 1 is the size of the suffix following x). Another property we take advantage of is
	* that an Int can be treated as unsigned if one adds Int.MinValue to both sides before comparing.
	*/
	def this(initialCapacity :Int)(implicit elementType :ClassTag[E]) = this(initialCapacity, false)
	def this(shrink :Boolean)(implicit elementType :ClassTag[E]) = this(0, shrink)
	def this()(implicit elementType :ClassTag[E]) = this(0, false)

	reserve(initialCapacity)

	/** The array storing this buffer's elements. It can be either an `Array[E]` of length `this.storageSize`
	  * ''iff'' `storageSize <= MaxSize1`, or an `Array[Array[E]]`, where the outer array is no longer
	  * than `MaxDim2`, and all non-null elements are of fixed length `MaxDim1` (equal also to `MaxSize1`).
	  * Regardless, its length is always a power of two. May also be `null` if `storageSize == 0`.
	  * In this way, the dimension - and nullity - of this field is determined exactly by the `storageSize` property.
	  * A two dimensional array maps to a virtual address space of individual elements, where the absolute index
	  * of an element at `data(n)(m)` equals `n * MaxSize1 + m`. For a single dimensional array,
	  * the absolute index is simply the array index.
	  *
	  * The first element is always stored at absolute index `dataOffset`, and the buffer contents may be wrapped,
	  * continuing from absolute index `0`. All non-null arrays in a two dimensional array either form
	  * a consecutive sequence, or are split into a prefix and a suffix of `data`.
	  * All allocated arrays which do not contain any elements of the buffer (reserved space) are always located
	  * directly after the array with the last element, never before the array with the first element (in terms
	  * of logical positions, not absolute indices).
	  */
	private[this] var data :Array[_] = _

	/** The index, in a continuous address space in `data`, of the first element of this buffer.
	  * If `data` is single dimensional, it is simply an index in that array. Otherwise, it is `n * MaxSize1 + m`,
	  * where `this.head` is located in `data(n)(m)`.
	  */
	private[this] var dataOffset  :Int = 0 //treated as unsigned if dim == 2

	/** Maximum capacity of `data`: if `data` is single dimensional, it is its length. Otherwise it is the sum
	  * of lengths of all arrays in `data`, i.e. `MaxSize1` times the number of non-null elements of `data`.
	  * It determines the dimension of the `data` array:
	  *   - if `storageSize == 0`, then `data` is `null` and the buffer is empty;
	  *   - if `storageSize <= MaxSize1`, then `data` is a single dimensional array of that length
	  *   - otherwise, data is a two dimensional array containing `storageSize >>> Dim1Bits` single dimensional arrays
	  *     of length `MaxSize1` at consecutive indices (modulo `data.length`). The index of the first allocated array
	  *     is always `dataOffset >>> Dim1Bits`, i.e. if the buffer contains any fully empty arrays, they are always
	  *     at the end.
	  * @note It may overflow to `Int.MinValue` in the most extreme case.
	  */
	private[this] var storageSize :Int = 0

	/** The backing property for `this.size`. */
	private[this] var dataSize :Int = 0

	@inline private[sugar] final def data1 :Array[E]        = data.asInstanceOf[Array[E]]
	@inline private[sugar] final def data2 :Array[Array[E]] = data.asInstanceOf[Array[Array[E]]]

	/** Returns `0` if `storageSize == 0`, `1` when `0 < storageSize <= MaxSize1`, and `2` when `storageSize > MaxSize1`. */
	@inline private[sugar] final def dim   :Int =
		(storageSize + (MaxSize2 - MaxSize1 - 1) >>> 31) + 1 - (storageSize - 1 >>> 31)

	private[sugar] final def startIndex :Int = dataOffset

	/** A mask for a two dimensional index. 'Anding' with it calculates the remainder of division by the total capacity
	  * (including unallocated arrays).
	  */
	@inline private def indexMask = (data2.length << Dim1Bits) - 1

	@inline private def emptyArrays :Int = storageSize - (dim1(dataOffset) + dataSize) >>> Dim1Bits

	final override def knownSize :Int = dataSize
	final override def length    :Int = dataSize

	override def startsWith[B >: E](that :IterableOnce[B], offset :Int) :Boolean =
		offset >= 0 && offset <= dataSize && super.startsWith(that, offset)

	final override def head :E = //todo: remove when the method becomes overridden in IndexedSeq in a future Scala version.
		if (dataSize == 0) throw new NoSuchElementException(className + "().head")
		else apply(0)

	final override def last :E =
		if (dataSize == 0) throw new NoSuchElementException(className + "().last")
		else apply(dataSize - 1)


	final override def apply(i :Int) :E = dim match {
		case _ if i < 0 || i >= dataSize => outOfBounds_!(i, dataSize)
		case 1 => data1(dataOffset + i & storageSize - 1)
		case 2 =>
			val absoluteIdx = dataOffset + i & (data2.length << Dim1Bits) - 1
			data2(absoluteIdx >>> Dim1Bits)(absoluteIdx & Dim1Mask)
	}

	override def update(idx :Int, elem :E) :Unit = dim match {
		case _ if idx < 0 || idx >= dataSize => outOfBounds_!(idx, dataSize)
		case 1 => data1(dataOffset + idx & storageSize - 1) = elem
		case 2 =>
			val absoluteIdx = dataOffset + idx & (data2.length << Dim1Bits) - 1 //(storageSize - 1 << Dim1Bits | Dim1Mask)
			data2(absoluteIdx >>> Dim1Bits)(absoluteIdx & Dim1Mask) = elem
	}

	/** Updates position `idx` to `first`, and the following positions to `second` and `rest`. */
	@throws[IndexOutOfBoundsException]("if idx < 0 or idx >= this.length - 2 - rest.length")
	def updateAll(idx :Int, first :E, second :E, rest :E*) :Unit = {
		update(idx, first)
		update(idx + 1, second)
		updateAll(idx + 2, rest)
	}

	/** Updates positions `idx`, `idx + 1`, ..., `idx + elems.size - 1` to subsequent elements of `elems`. */
	@throws[IndexOutOfBoundsException]("if idx < 0 or idx >= this.length - elems.size")
	def updateAll(idx :Int, elems :IterableOnce[E]) :Unit =
		if (idx < 0 || idx > dataSize)
			outOfBounds_!(idx)
		else if (storageSize <= MaxSize1) {
			val data1 = this.data1
			val mask  = storageSize - 1
			val size  = elems.knownSize
			if (size >= 0)
				if (size > dataSize - idx)
					outOfBounds_!(errorString(this) + ".updateAll(" + idx + ", " + errorString(elems) + ")")
				else
					elems match {
						case items :Iterable[E] => items.cyclicCopyToArray(data1, dataOffset + idx & mask)
						case _                  => elems.iterator.cyclicCopyToArray(data1, dataOffset + idx & mask)
					}
			else
				elems.toBasicOps.foldLeft(dataOffset + idx & mask) { (i, e) =>
					data1(i) = e; i + 1 & mask
				}
		} else {
			val data2 = this.data2
			val size  = elems.knownSize
			val mask  = this.indexMask
			if (size >= 0)
				if (size > dataSize - idx)
					outOfBounds_!(errorString(this) + ".updateAll(" + idx + ", " + errorString(elems) + ")")
				else
					write2(dataOffset + idx & mask, elems, size)
			else
				elems.toBasicOps.foldLeft(dataOffset + idx & mask) { (i, e) =>
					data2(dim2(i))(dim1(i)) = e; i + 1 & mask
				}
		}



	override def addOne(elem :E) :this.type = {
		if (dataSize == MaxValue)
			bufferFull(1)
		if (storageSize == 0) {                                      //dim == 0, the buffer is empty
			val array = new Array[E](NewSize1)
			array(0) = elem
			data = array
			storageSize = NewSize1
		} else if (storageSize <= MaxSize1) {                        //dim == 1
			if (dataSize < storageSize)
				data1(dataOffset + dataSize & storageSize - 1) = elem
			else if (storageSize < MaxSize1) {                       //grow the single dimension array
				data =
					if (dataOffset == 0)
						Array.copyOf(data, storageSize << 1)
					else
						Array.copyOfRanges(data1, dataOffset, storageSize, data1, 0, dataOffset, storageSize << 1)
				data1(storageSize) = elem
				storageSize <<= 1
				dataOffset = 0
			} else {                                                 //grow to the second dimension
				storageSize = MaxSize1 << 1
				val array2 = new Array[Array[E]](NewSize2)
				val array1 = new Array[E](MaxSize1)
				array2(1) = array1
				array1(0) = elem
				if (dataOffset == 0)
					array2(0) = data1
				else {
					array2(0) = Array.copyOfRanges(data1, dataOffset, MaxSize1, data1, 0, dataOffset)
					dataOffset = 0
				}
				data = array2
			}
		} else {                                                     //dim == 2
			allocBack(1)
			val absoluteIdx = dataOffset + dataSize & indexMask
			data2(dim2(absoluteIdx))(dim1(absoluteIdx)) = elem
		}
		dataSize += 1
		this
	}

	override def addAll(elems :IterableOnce[E]) :this.type = genericAdd(elems)

	def addAll(elems :ArrayLike[E]) :this.type = addAll(elems, 0, elems.length)

	def addAll(elems :ArrayLike[E], idx :Int, length :Int) :this.type = {
		if (length > MaxValue - dataSize)
			bufferFull(length)
		if (length < 0) //We check length here to avoid attempting to write to a non allocated table.
			throw new IllegalArgumentException(
				"A negative length argument to " + errorString(this) + ".addAll(" +
					errorString(elems) + ", " + idx + ", " + length + ")")
		if (idx < 0 || idx > elems.length - length)
			throw new IndexOutOfBoundsException(idx.toString + " for an " + errorString(elems))
		if (storageSize == 0) {                               //dim == 0
			if (length <= MaxSize1) {                         //create a single dimensional array
				var capacity = NewSize1
				while (capacity < length)
					capacity <<= 1
				data = Array.copyOfRange(elems, idx, idx + length, capacity)
				storageSize = capacity
			} else {                                          //create a two dimensional array
				var capacity2 = NewSize2
				val unsignedLength = length + MinValue
				while ((capacity2 << Dim1Bits) + MinValue < unsignedLength)
					capacity2 <<= 1
				data = new Array[Array[E]](capacity2)
				var i = 0
				while (i + (MaxSize1 + MinValue) <= unsignedLength) { //compare unsigned
					data2(dim2(i)) = Array.copyOfRange(elems, idx + i, idx + i + MaxSize1)
					i += MaxSize1
				}
				storageSize = i
				if (i <= length) {
					data2(dim2(i)) = Array.copyOfRange(elems, idx + i, idx + i, idx + length, MaxSize1)
					storageSize = i + MaxSize1
				}
			}
		} else if (storageSize <= MaxSize1) {                 //dim == 1
			val dataEnd = dataOffset + dataSize
			if (length <= storageSize - dataSize)
				ArrayLike.cyclicCopyTo(elems, idx, data1, dataEnd & storageSize - 1, length)
			else if (length <= MaxSize1 - dataSize) {            //increase the array size
				val newSize = dataSize + length
				var capacity = storageSize << 1
				while (capacity < newSize)
					capacity <<= 1
				if (dataEnd <= storageSize) {                 //data is not wrapped
					data = Array.copyOf(data, capacity)
					ArrayLike.cyclicCopyTo(elems, idx, data1, dataEnd, length)
				} else {
					data = Array.copyOfRanges(
						data1, dataOffset, storageSize,
						data1, 0, dataEnd - storageSize,
						elems, idx, idx + length, capacity
					)
					dataOffset = 0
				}
				storageSize = capacity
			} else {                                          //must grow to the second dimension
				growToDim2(length)
				if (dim2(dim1(dataOffset) + dataSize + length - 1) + 1 > dim2(storageSize)) {
					val dataEnd  = dataOffset + dataSize
					val data2    = this.data2
					val mask2    = data2.length - 1
					val end2     = dim2(dataEnd + length - 1) + 1 & mask2
					var i        = dim2(dataOffset + storageSize) & mask2
					storageSize += (end2 - i & mask2) << Dim1Bits
					while (i != end2) {
						data2(i) = new Array[E](MaxSize1)
						i = i + 1 & mask2
					}
				}
				write2(dataEnd, elems, idx, length)
			}
		} else {                                              //dim == 2
			allocBack(length)
			write2(dataOffset + dataSize & indexMask, elems, idx, length)
		}
		dataSize += length
		this
	}

	/** Generic implementation for `add` and `addAll` methods accepting anything with a proper type class. */
	private def genericAdd[Es](elems :Es)(implicit values :CollectionLike[E, Es]) :this.type = {
		val elemsSize = values.knownSize(elems)
		if (elemsSize > 0)
			addKnownSize(elems, elemsSize)
		else if (elemsSize < 0)
			addUnknownSize(elems)
		this
	}

	/** Implementation of `addAll` covering the case when the number of elements in `elems` is known to be `elemsSize`. */
	@tailrec private def addKnownSize[Es](elems :Es, elemsSize :Int)(implicit values :CollectionLike[E, Es]) :Unit = {
		if (elemsSize > MaxValue - dataSize)
			bufferFull(elemsSize)
		if (storageSize == 0) {                       //dim == 0
			if (elemsSize <= MaxSize1) {              //create a single dimensional array
				var capacity = NewSize1
				while (capacity < elemsSize)
					capacity <<= 1
				data = new Array[E](capacity)
				values.copyToArray(elems)(data1, 0, elemsSize)
				storageSize = capacity
				dataSize += elemsSize
			} else {                                  //create a two dimensional array
				var capacity2      = NewSize2
				val unsignedLength = elemsSize + MinValue
				while ((capacity2 << Dim1Bits) + MinValue < unsignedLength)
					capacity2 <<= 1
				data = new Array[Array[E]](capacity2)
				data2(0) = new Array[E](MaxSize1)
				data2(1) = new Array[E](MaxSize1)
				storageSize = MaxSize1 << 1
				addKnownSize(elems, elemsSize)
			}
		} else if (storageSize <= MaxSize1) {         //dim == 1
			val newSize     = dataSize + elemsSize
			val dataEnd     = dataOffset + dataSize
			if (elemsSize <= storageSize - dataSize) {
				val elemsOffset = dataEnd & storageSize - 1
				if (elemsOffset + elemsSize <= storageSize)
					values.copyToArray(elems)(data1, elemsOffset, elemsSize)
				else
					values.cyclicCopyToArray(elems)(data1, elemsOffset, elemsSize)
			} else if (elemsSize <= MaxSize1 - dataSize) { //increase the array size
				var capacity = storageSize << 1
				while (capacity < newSize)
					capacity <<= 1
				if (dataEnd <= storageSize) {         //data is not wrapped
					data = Array.copyOf(data, capacity)
					if (dataEnd + elemsSize <= capacity)
						values.copyToArray(elems)(data1, dataEnd, elemsSize)
					else
						values.cyclicCopyToArray(elems)(data1, dataEnd, elemsSize)
				} else {
					val a = Array.copyOfRanges(
						data1, dataOffset, storageSize,
						data1, 0, dataEnd - storageSize, capacity
					)
					data = a
					dataOffset = 0
					values.copyToArray(elems)(a, dataSize, elemsSize)
				}
				storageSize = capacity
			} else {                                  //must grow to the second dimension
				growToDim2(elemsSize)
				allocBack(elemsSize)
				write2(dataOffset + dataSize & indexMask, elems, elemsSize)
			}
			dataSize += elemsSize
		} else {                                      //dim == 2
			allocBack(elemsSize)
			write2(dataOffset + dataSize & indexMask, elems, elemsSize)
			dataSize += elemsSize
		}
	}

	/** Fallback implementation for `addAll` for the case when the collection size is not known. */
	private def addUnknownSize[Es](elems :Es)(implicit values :CollectionLike[E, Es]) :Unit = {
		val it = values.iterator(elems)
		if (storageSize == 0) {
			storageSize = NewSize1
			data        = new Array[E](NewSize1)
		}
		while (it.hasNext) {
			if (storageSize <= MaxSize1) {
				val mask = storageSize - 1
				dataSize += it.copyToArray(data1, dataOffset + dataSize)
				dataSize += it.copyToArray(data1, dataOffset + dataSize & mask, storageSize - dataSize)
				if (it.hasNext) {
					if (storageSize < MaxSize1)
						grow1(storageSize)
					else
						growToDim2(1)
				}
			} else {
				if (dim2(dataOffset + dataSize) == dim2(dataOffset + storageSize))
					allocBack(1)
				val data2 = this.data2
				val idx   = dim2(dataOffset + dataSize) & data2.length - 1
				val start = dim1(dataOffset + dataSize)
				val max   =
					if (idx == dim2(dataOffset) && start < dataOffset) dataOffset - start
					else MaxSize1 - start
				dataSize += it.copyToArray(data2(idx), start, max)
			}
		}
	}



	override def prepend(elem :E) :this.type = {
		if (dataSize == MaxValue)
			bufferFull(1)
		if (storageSize == 0) {                                             //dim == 0, the buffer is empty
			val array = new Array[E](NewSize1)
			array(NewSize1 - 1) = elem
			data = array
			storageSize = NewSize1
			dataOffset  = NewSize1 - 1
		} else if (storageSize <= MaxSize1) {                               //dim == 1
			if (dataSize < storageSize) {                                      //buffer not full
				dataOffset = dataOffset - 1 & storageSize - 1
				data1(dataOffset) = elem
			} else if (storageSize < MaxSize1) {                            //double the size of the dim 1 array
				storageSize <<= 1
				data =
					if (dataOffset == 0)
						Array.copyOf(data, storageSize)
					else
						Array.copyOfRanges(data1, dataOffset, dataSize, data1, 0, dataOffset, storageSize)
				dataOffset = storageSize - 1
				data1(dataOffset) = elem
			} else {                                                        //grow to the second dimension
				storageSize = MaxSize1 << 1
				val array2 = new Array[Array[E]](NewSize2)
				val array1 = new Array[E](MaxSize1)
				array2(NewSize2 - 2) = array1
				array1(MaxSize1 - 1) = elem
				if (dataOffset == 0)
					array2(NewSize2 - 1) = data1
				else
					array2(NewSize2 - 1) = Array.copyOfRanges(data1, dataOffset, MaxSize1, data1, 0, dataOffset)
				dataOffset = (NewSize2 - 1 << Dim1Bits) - 1
				data = array2
			}
		} else {                                                            //dim == 2
			allocFront(1)
			dataOffset = dataOffset - 1 & indexMask
			data2(dim2(dataOffset))(dim1(dataOffset)) = elem
		}
		dataSize += 1
		this
	}

	override def prependAll(elems :IterableOnce[E]) :this.type = genericPrepend(elems)

	def prependAll(elems :ArrayLike[E]) :this.type = prependAll(elems, 0, elems.length)

	final def prependAll(elems :ArrayLike[E], idx :Int, length :Int) :this.type = {
		if (length > MaxValue - dataSize)
			bufferFull(length)
		if (length < 0) //We check length here to avoid attempting to write to a non allocated table.
			throw new IllegalArgumentException(
				"A negative length argument to " + errorString(this) + ".adAll(" +
					errorString(elems) + ", " + idx + ", " + length + ")")
		if (length == 0)
			this
		else if (storageSize == 0)                                 //dim == 0
			addAll(elems, idx, length)
		else if (storageSize <= MaxSize1) {                   //dim == 1
			val newSize = dataSize + length
			if (length <= storageSize - dataSize) {
				val newOffset = dataOffset - length & storageSize - 1
				ArrayLike.cyclicCopyTo(elems, idx, data, newOffset, length)
				dataOffset = newOffset
				dataSize  += length
			} else if (length <= MaxSize1 - dataSize) {          //increase the array size
				val dataEnd  = dataOffset + dataSize
				var capacity = storageSize << 1
				while (capacity < newSize)
					capacity <<= 1
				if (dataEnd <= storageSize) {                 //data is not wrapped
					data = Array.copyOf(data, capacity)
					dataOffset = dataOffset - length & capacity - 1
					ArrayLike.cyclicCopyTo(elems, idx, data, dataOffset, length)
				} else {
					data = Array.copyOfRanges(
						elems, idx, length,
						data1, dataOffset, storageSize,
						data1, 0, dataEnd - storageSize, capacity
					)
					dataOffset = 0
				}
				storageSize = capacity
				dataSize   += length
			} else {                                          //must grow to the second dimension
				growToDim2(length)
				prependAll(elems, idx, length)
			}
			this
		} else {                                              //dim == 2
			allocFront(length)
			dataOffset = dataOffset - length & indexMask
			write2(dataOffset, elems, idx, length)
			dataSize += length
			this
		}
	}

	/** Implementation target for `prepend` family methods accepting any kind of collection-like thing
	  * with a proper type class.
	  */
	private def genericPrepend[Es](elems :Es)(implicit values :CollectionLike[E, Es]) :this.type = {
		val size = values.knownSize(elems)
		if (size > 0)
			prependKnownSize(elems, size)
		else if (size < 0)
			prependUnknownSize(elems)
		this
	}

	/** Implementation of `prependAll` covering the case when the number of elements is known to be `elemsSize`. */
	private def prependKnownSize[Es](elems :Es, elemsSize :Int)(implicit values :CollectionLike[E, Es]) :this.type = {
		if (elemsSize > MaxValue - dataSize)
			bufferFull(elemsSize)
		if (dataSize == 0)                                       //dim == 0
			addKnownSize(elems, elemsSize)
		else if (storageSize <= MaxSize1) {                   //dim == 1
			if (elemsSize <= storageSize - dataSize) {
				dataOffset = dataOffset - elemsSize & storageSize - 1
				dataSize  += elemsSize
				values.cyclicCopyToArray(elems)(data1, dataOffset, elemsSize)
			} else if (elemsSize <= MaxSize1 - dataSize) {       //increase the array size
				val newSize  = dataSize + elemsSize
				val dataEnd  = dataOffset + dataSize
				var capacity = storageSize << 1
				while (capacity < newSize)
					capacity <<= 1
				if (dataEnd <= storageSize) {                  //data is not wrapped
					data = Array.copyOf(data, capacity)
					dataOffset = dataOffset - elemsSize & capacity - 1
				} else {
					data = Array.copyOfRanges(
						data1, dataOffset, storageSize,
						data1, 0, dataEnd - storageSize,
						capacity
					)
					dataOffset = capacity - elemsSize
				}
				values.cyclicCopyToArray(elems)(data1, dataOffset, elemsSize)
				storageSize = capacity
				dataSize   += elemsSize
			} else {                                          //must grow to the second dimension
				growToDim2(elemsSize)
				prependKnownSize(elems, elemsSize)
			}
		} else {                                              //dim == 2
			allocFront(elemsSize)
			dataOffset = dataOffset - elemsSize & indexMask
			dataSize  += elemsSize
			write2(dataOffset, elems, elemsSize)
		}
		this
	}

	/** Implementation of `prependAll` covering the case when the number of prepended elements is not known beforehand. */
	private def prependUnknownSize[Es](elems :Es)(implicit values :CollectionLike[E, Es]) :this.type =
		if (dataSize == 0) {
			addUnknownSize(elems)
			this
		} else if (storageSize <= MaxSize1)
			prependUnknownSize1(elems)
		else                            //Two dimensions. Clear the buffer and set aside the current contents.
			prependUnknownSize2(elems)

	/** Implementation of `prependAll` covering the case when the number of prepended elements is not known beforehand
	  * and the buffer has a single dimension.
	  */
	private def prependUnknownSize1[Es](elems :Es)(implicit values :CollectionLike[E, Es]) :this.type = {
		//Try temporarily copying into our array anywhere to avoid extra allocation.
		val data1   = this.data1
		val mask    = storageSize - 1
		val dataEnd = (dataOffset + dataSize - 1 & mask) + 1
		val it      = values.iterator(elems)
		var i       = dataEnd & mask
		while (i != dataOffset && it.hasNext) {
			data1(i) = it.next()
			i = i + 1 & mask
		}
		val copied = i - dataEnd & mask
		if (!it.hasNext) {              //Everything fits in our current array, now shift the contents in place.
			dataSize += copied
			if (i != dataOffset) {
				dataOffset = dataOffset - copied & mask
				Array.cyclicCopy(data, dataEnd, data, dataOffset, copied)
			} else
				dataOffset = dataEnd
		} else {                         //Need to grow. Start empty, add already copied elems and remaining elems
			val oldOffset  = dataOffset
			val oldStorage = storageSize
			val oldSize    = dataSize
			if (storageSize < MaxSize1)
				storageSize <<= 1
			data       = new Array[E](storageSize)
			dataOffset = 0
			dataSize   = 0
			try {
				addUnknownSize(it)
			} catch {
				case e :Exception =>     //Restore the buffer state from before.
					restore(data1, oldOffset, oldSize, oldStorage)
//					if (oldOffset < dataEnd) {
//						dataSize = dataEnd - oldOffset
//						data1.clearIfRef(dataEnd, oldStorage)
//						data1.clearIfRef(0, oldOffset)
//					} else {
//						dataSize = oldStorage - oldOffset + dataEnd
//						data1.clearIfRef(dataEnd, oldOffset)
//					}
					throw e
			}
			if (dataSize > MaxValue - oldSize) {
				restore(data1, oldOffset, oldSize, oldStorage)
				bufferFull(dataSize)
			}

			reserve(oldSize + copied)
			if (oldOffset < dataEnd) {
				addAll(data1, oldOffset, oldSize)
				prependAll(data1, 0, oldOffset)
				prependAll(data1, dataEnd, oldStorage - dataEnd)
			} else {
				addAll(data1, oldOffset, oldStorage - oldOffset)
				addAll(data1, 0, dataEnd)
				prependAll(data1, dataEnd, oldOffset - dataEnd)
			}
			if (oldStorage == MaxSize1 && storageSize != (data2.length << Dim1Bits)) {
				if (data1.isInstanceOf[Array[AnyRef]])
					data1.clear()
//				data1.clearIfRef()
				data2(dim2(dataOffset + storageSize) & data2.length - 1) = data1
				storageSize += MaxSize1
			}
		}
		this
	}

	/** Implementation of `prependAll` covering the case when the number of prepended elements is not known beforehand
	  * and the buffer has two dimensions.
	  */
	private def prependUnknownSize2[Es](elems :Es)(implicit values :CollectionLike[E, Es]) :this.type = {
		val oldData     = data2
		val oldCapacity = oldData.length
		val oldMask     = oldCapacity - 1
		val oldOffset   = dataOffset
		val oldSize     = dataSize
		val oldStorage  = if (emptyArrays > 0) storageSize - MaxSize1 else storageSize
		//Clear the buffer, append elems, and append the current (old) contents
		if (oldStorage == storageSize)
			data = new Array[E](MaxSize1)                          //Avoid unnecessary copying so we can reuse it.
		else {                                                     //Avoid an allocation if we already have a free array.
			val lastEmpty = dim2(oldOffset + oldStorage) & oldMask
			data = oldData(lastEmpty)
			oldData(lastEmpty) = null
		}
		storageSize = MaxSize1
		dataOffset  = 0
		dataSize    = 0
		try {
			addUnknownSize(elems)
		} catch {
			case e :Exception =>
				restore(oldData, oldOffset, oldSize, oldStorage)
				throw e
		}
		if (dataSize + oldSize > MaxValue) {
			restore(oldData, oldOffset, oldSize, oldStorage)
			bufferFull(dataSize)
		}

		if (dataSize == 0) {
			if (dim2(oldStorage) == oldCapacity) {
				storageSize = oldStorage
			} else {
				oldData(dim2(oldOffset + oldStorage) & oldMask) = data1
				storageSize = oldStorage + MaxSize1
			}
			dataSize   = oldSize
			dataOffset = oldOffset
			data       = oldData
		} else if (storageSize == MaxSize1) {
			val newData = data1
			val newSize = dataSize
			restore(oldData, oldOffset, oldSize, oldStorage)
			if (oldStorage == MaxDim2)
				prependAll(newData, 0, newSize)
			else {
				grow2(newSize)
				val data2      = this.data2
				val mask2      = data2.length - 1
				val offset2    = dim2(dataOffset) - 1 & mask2
				val gap        = MaxSize1 - newSize + dim1(dataOffset)
				data2(offset2) = newData
				dataOffset     = offset2 << Dim1Bits
				dataSize       = newSize + oldSize + gap
				if (storageSize < (data2.length << Dim1Bits))
					storageSize += MaxSize1
				closeTheGap2(newSize, gap)
			}
		} else { //storageSize > MaxSize1
			restoreSuffix2(oldData, oldOffset, oldSize, oldStorage)
		}
		this
	}



	override def insert(idx :Int, elem :E) :Unit = genericInsert(idx, elem)
	override def insertAll(idx :Int, elems :IterableOnce[E]) :Unit = genericInsert(idx, elems)

	def insertAll(idx :Int, elems :ArrayLike[E]) :Unit = genericInsert(idx, elems)

	/** Implementation of both `insert` and `insertAll`, working for any collection-like thing with a proper type class. */
	private def genericInsert[Es](idx :Int, elems :Es)(implicit values :CollectionLike[E, Es]) :Unit =
		if (idx < 0 || idx > dataSize)
			outOfBounds_!(idx, dataSize)
		else if (idx == 0)
			genericPrepend(elems)
		else if (idx == dataSize)
			genericAdd(elems)
		else if (storageSize <= MaxSize1) { //dim == 1; dim == 0 handled because idx == 0 == size
			val knownSize = values.knownSize(elems)
			if (knownSize > 0)
				insertKnownSize1(idx, elems, knownSize)
			else if (knownSize < 0 && !values.isEmpty(elems))
				insertUnknownSize1(idx, elems)
		} else {                            //dim == 2
			val elemsSize = values.knownSize(elems)
			if (elemsSize != 0)
				if (elemsSize >= 0)
					insertKnownSize2(idx, elems, elemsSize)
				else if (!values.isEmpty(elems))
					insertUnknownSize2(idx, elems)
		}

	/** Implementation of both `insert` and `insertAll` for a single dimensional buffer,
	  * covering the case when the collection is known to have `elemsSize` elements.
	  */
	private def insertKnownSize1[Es](idx :Int, elems :Es, elemsSize :Int)(implicit values :CollectionLike[E, Es]) :Unit = {
		if (elemsSize >= MaxValue - dataSize)
			bufferFull(elems)
		if (elemsSize <= storageSize - dataSize) {
			val mask1 = storageSize - 1
			if (idx <= (dataSize >> 1)) {
				shiftLeft1(dataOffset, idx, elemsSize)
				dataOffset = dataOffset - elemsSize & mask1
			} else
				shiftRight1(dataOffset + idx & mask1, dataSize - idx, elemsSize)
			val insertOffset = dataOffset + idx & mask1
			values.cyclicCopyToArray(elems)(data1, insertOffset, elemsSize)
			dataSize += elemsSize
		} else if (elemsSize <= MaxSize1 - dataSize) {
			val newSize = dataSize + elemsSize
			var capacity = data.length << 1
			while (capacity < newSize)
				capacity <<= 1
			val newData = new Array[E](capacity)
			if (idx <= storageSize - dataOffset) {
				arraycopy(data, dataOffset, newData, 0, idx)
				val slice2Size = math.min(dataSize - idx, storageSize - dataOffset - idx)
				arraycopy(data, dataOffset + idx, newData, idx + elemsSize, slice2Size)
				if (idx + slice2Size < dataSize)
					arraycopy(data, 0, newData, idx + elemsSize + slice2Size, dataSize - idx - slice2Size)
			} else {
				val slice1Size = storageSize - dataOffset
				arraycopy(data, dataOffset, newData, 0, slice1Size)
				arraycopy(data, 0, newData, slice1Size, idx - slice1Size)
				arraycopy(data, idx - slice1Size, newData, idx + elemsSize, dataSize - idx)
			}
			values.copyToArray(elems)(newData, idx, elemsSize)
			data        = newData
			dataOffset  = 0
			storageSize = capacity
			dataSize    = newSize
		} else {
			val oldData      = data1
			val oldStorage   = storageSize
			val suffixSize   = dataSize - idx
			val suffixOffset = dataOffset + idx & oldStorage - 1
			val capacity2    = growCapacity2(elemsSize)
			val array2       = new Array[Array[E]](capacity2)
			if (dataOffset + idx <= storageSize) //the data is not wrapped
				array2(0) = Array.copyOfRange(data1, dataOffset, dataOffset + idx, MaxSize1)
			else { //the data is wrapped
				val array1 = new Array[E](MaxSize1)
				arraycopy(data, dataOffset, array1, 0, storageSize - dataOffset)
				arraycopy(data, 0, array1, storageSize - dataOffset, dataOffset + idx - storageSize)
				array2(0) = Array.copyOfRanges(
					data1, dataOffset, storageSize,
					data1, 0, dataOffset + idx - storageSize, MaxSize1
				)
			}
			array2(1)   = new Array[E](MaxSize1)
			storageSize = MaxSize1 << 1
			dataOffset  = 0
			dataSize    = idx
			data        = array2
			addKnownSize(elems, elemsSize)
			if (suffixOffset + suffixSize <= oldStorage)
				addAll(oldData, suffixOffset, suffixSize)
			else {
				addAll(oldData, suffixOffset, oldStorage - suffixOffset)
				addAll(oldData, 0, suffixSize - (oldStorage - suffixOffset))
			}
		}
	}

	/** Implementation of both `insert` and `insertAll` for a single dimensional buffer,
	  * covering the case when the size of the collection is not known.
	  */
	private def insertUnknownSize1[Es](idx :Int, elems :Es)(implicit values :CollectionLike[E, Es]) :Unit = {
		val suffixSize = dataSize - idx
		if (idx < suffixSize) {
			val prefix =
				if (dataOffset + idx <= storageSize)
					data1.slice(dataOffset, dataOffset + idx)
				else
					Array.copyOfRanges(
						data1, dataOffset, storageSize,
						data1, 0, idx - (storageSize - dataOffset)
					)
			dataSize   = suffixSize
			dataOffset = dataOffset + idx & storageSize - 1
			try {
				genericPrepend(elems)
				prependAll(prefix)
			} catch {
				case e :Exception =>
					remove(0, dataSize - suffixSize)
					prependAll(prefix)
					throw e
			}
		} else {
			val suffixOffset = dataOffset + idx & storageSize - 1
			val suffix =
				if (suffixOffset + suffixSize <= storageSize)
					data1.slice(suffixOffset, suffixOffset + suffixSize)
				else
					Array.copyOfRanges(
						data1, suffixOffset, storageSize,
						data1, 0, suffixSize - (storageSize - suffixOffset)
					)
			dataSize = idx
			try {
				genericAdd(elems)
				addAll(suffix)
			} catch {
				case e :Exception =>
					remove(idx, dataSize - idx)
					addAll(suffix)
					throw e
			}
		}
	}

	/** Implementation of both `insert` and `insertAll` for a two dimensional buffer,
	  * covering the case when the collection is known to have `elemsSize` elements.
	  */
	private def insertKnownSize2[Es](idx :Int, elems :Es, elemsSize :Int)(implicit values :CollectionLike[E, Es]) :Unit = {
		if (elemsSize + MinValue > 0xffffffff)
			bufferFull(elems)
		if (idx <= (dataSize >> 1)) {
			allocFront(elemsSize)
			shiftLeft2(dataOffset, idx, elemsSize)
			dataOffset = dataOffset - elemsSize & indexMask
		} else {
			allocBack(elemsSize)
			shiftRight2(dataOffset + idx, dataSize - idx, elemsSize)
		}
		write2(dataOffset + idx & indexMask, elems, elemsSize)
		dataSize += elemsSize
	}

	/** Implementation of both `insert` and `insertAll` for a two dimensional buffer,
	  * covering the case when the collection's size is not known.
	  */
	private def insertUnknownSize2[Es](idx :Int, elems :Es)(implicit values :CollectionLike[E, Es]) :Unit =
		if (!values.isEmpty(elems)) {
			val length2     = data2.length
			val indexMask   = (length2 << Dim1Bits) - 1
			val index2Mask  = length2 - 1
			val dataOffset2 = dim2(dataOffset)
			val absoluteIdx = dataOffset + idx & indexMask
			val idx2        = dim2(absoluteIdx)
			val idx1        = dim1(absoluteIdx)
			val prefixSize2 = dim2(dataOffset + idx + MaxSize1 - 1) - dataOffset2
			val suffixSize2 = dim2(dataOffset + dataSize + MaxSize1 - 1) - dim2(dataOffset + idx)
			if (suffixSize2 == 1) {  //Drop the suffix, append elems, and add the suffix back.
				val suffixLength = dim1(dataOffset + dataSize - 1) + 1 - idx1
				val suffix =         //The data in suffix always starts at index 0
					if (idx1 != 0 | storageSize == (MaxSize1 << 1)) {
						//The array with the suffix contains also data from the prefix or is of minimal capacity for dim 2.
						if (suffixLength > (MaxSize1 >> 1))
							//If the suffix contains most of the array, copy the whole array to reuse later.
							Array.copyOfRange(data2(idx2), idx1, dim1(dataOffset + dataSize - 1) + 1, 0, MaxSize1)
						else
							Array.copyOfRange(data2(idx2), idx1, dim1(dataOffset + dataSize - 1) + 1)
					} else {
						val a        = data2(idx2)
						val last2    = dataOffset2 + dim2(storageSize) - 1 & index2Mask
						data2(idx2)  = data2(last2)    //Fill the gap with an empty array (or reassign back to itself).
						data2(last2) = null
						storageSize -= MaxSize1
						a
					}
				dataSize = idx

				val exception = try {
					genericAdd(elems)
					null
				} catch {
					case e :Exception =>
						clearIfRef(dataOffset + idx & indexMask, dataSize)
						dataSize = idx
						e
				}
				if (suffix.length == MaxSize1 & storageSize != MaxSize2) {
					grow2(emptyArrays + 1 << Dim1Bits) //because grow counts above used storage, not allocated
					val mask2         = data2.length - 1
					val end           = dim2(dataOffset + dataSize - 1) + 1 & mask2 //the first empty array or null
					val storageEnd    = dim2(dataOffset + storageSize) & mask2
					data2(storageEnd) = data2(end)
					data2(end)        = suffix
					val gap           = dim1(-(dataOffset + dataSize))
					shiftLeft2(end << Dim1Bits, suffixLength, gap)
					if (suffix.isInstanceOf[Array[AnyRef]])
						suffix.clear(math.max(0, suffixLength - gap), suffixLength)
//					suffix.clearIfRef(math.max(0, suffixLength - gap), suffixLength)
					storageSize += MaxSize1
					dataSize    += suffixLength
				} else
					addAll(suffix)
				if (exception != null)
					throw exception

			} else if (prefixSize2 == 1) { //Drop the prefix, prepend elems, and prepend the prefix.
				val prefix =               //prefix data always starts at prefix.length - idx (is aligned to the right).
					if (idx1 != 0 | storageSize == (MaxSize1 << 1))
						//The array with the prefix contains also data from the suffix or is of minimal capacity for dim 2.
						if (idx > (MaxSize1 >> 1))
							//If the prefix takes most of the array, copy the whole array to reuse later.
							Array.copyOfRange(data2(dataOffset2), dim1(dataOffset), idx1, MaxSize1 - idx, MaxSize1)
						else
							Array.copyOfRange(data2(dataOffset2), dim1(dataOffset), idx1)
					else {
						val a = data2(dataOffset2)
						data2(dataOffset2) = null
						storageSize -= MaxSize1
						a
					}
				val oldSize = dataSize
				dataOffset  = dataOffset + idx & indexMask
				dataSize   -= idx

				val exception = try {
					genericPrepend(elems)
					null
				} catch {
					case e :Exception =>
						remove(0, dataSize - oldSize)
						e
				}
				if (prefix.length == MaxSize1 & storageSize != MaxSize2) {
					grow2(emptyArrays + 1 << Dim1Bits)
					val indexMask = this.indexMask
					val offset    = MaxSize1 - idx
					val front2    = dim2(dataOffset) - 1 & data2.length - 1
					val front     = (front2 << Dim1Bits) + offset & indexMask
					data2(front2) = prefix
					shiftRight2(front, idx, dim1(dataOffset))
					if (idx > dim1(dataOffset)) {
//						prefix.clearIfRef(offset, offset + dim1(dataOffset))
						if (prefix.isInstanceOf[Array[AnyRef]])
							prefix.clear(offset, offset + dim1(dataOffset))
					} else {
//						prefix.clearIfRef(offset, MaxSize1)
						if (prefix.isInstanceOf[Array[AnyRef]])
							prefix.clear(offset, MaxSize1)
						data2(front2) = null
						data2(dim2(dataOffset + storageSize & indexMask)) = prefix
					}
					dataOffset   = dataOffset - idx & indexMask
					dataSize    += idx
					storageSize += MaxSize1
				} else
					prependAll(prefix)
				if (exception != null)
					throw exception

			} else { //Store the suffix on the side, append elems, and append the suffix.
				val suffixLength  = dataSize - idx
				val suffix = splitInPlace2(idx, suffixSize2)
				val firstInSuffix = data2(idx2)

				val exception = try {
					genericAdd(elems)

					if(dataSize + suffixLength + MinValue > MaxSize2 + MinValue) {
						//Revert insertion if together the new and the old elements exceed the limit
						val extra = dataSize - idx
						remove(idx, extra)
						new BufferFullException(
							"Cannot add " + extra + " elements to a buffer of size " + (idx + suffixLength)
						)
					} else
						null
				} catch {
					case e :Exception =>
						val after  = data2(dim2(dataOffset + idx) & data2.length - 1)
						//We haven't cleared the prefix of the suffix in the array with idx, so lets do it now if necessary.
						if (firstInSuffix.isInstanceOf[Array[AnyRef]] &&
							(firstInSuffix eq after) && idx1 > 0 && dataSize < idx - idx1 + MaxSize1
						)
							firstInSuffix.clear(size - idx, MaxSize1)
//							firstInSuffix.clearIfRef(size - idx, MaxSize1)
						remove(idx, dataSize - idx)
						e
				}
				restoreSuffix2(suffix, idx1, suffixLength, suffix.length << Dim1Bits)
				if (exception != null)
					throw exception
			}
		}


	private def splitInPlace2(idx :Int, suffixSize2 :Int) :Array[Array[E]] = {
		val offset  = dataOffset + idx
		val data2   = this.data2
		val length2 = data2.length
		val mask2   = length2 - 1
		val idx2    = dim2(offset) & mask2
		val idx1    = dim1(offset)
		val suffix  =
			if (idx2 + suffixSize2 <= length2)
				Array.copyOfRange(data2, idx2, idx2 + suffixSize2)
			else
				Array.copyOfRanges(data2, idx2, length2, data2, 0, suffixSize2 - (length2 - idx2))
		val emptyBackArrays = emptyArrays //First, shift the empty arrays left in place of the suffix
		if (emptyBackArrays > 0) {
			val copied    = math.min(emptyBackArrays, suffixSize2 - dim2(idx1 + MaxSize1 - 1))
			val moveFrom2 = dim2(dataOffset + storageSize) - copied & mask2
			val moveTo2   = dim2(offset + MaxSize1 - 1) & mask2
			Array.cyclicCopy(data2, moveFrom2, data2, moveTo2, copied)
		}
		if (idx1 == 0)
			storageSize = storageSize - (suffixSize2 << Dim1Bits)
		else { //Must copy the first array to fully separate the suffix from the buffer
			suffix(0) = Array.copyOf(suffix(0), MaxSize1)
			storageSize = storageSize - (suffixSize2 - 1 << Dim1Bits)
		}
		//consider: deallocate the suffix arrays in the buffer
		dataSize = idx
		suffix
	}

	private def restoreSuffix2(suffix :Array[Array[E]], offset :Int, suffixLength :Int, suffixStorage :Int) :Unit = {
		if (dataSize + suffixLength + MinValue > MaxSize2 + MinValue)
			bufferFull(suffixLength)
		val suffixSize2      = suffix.length
		val combinedStorage2 = dim2(storageSize + suffixStorage)
		if (combinedStorage2 >= MaxDim2)
			grow2(MaxSize2 - dataSize)
		else if (combinedStorage2 > this.data2.length)
			grow2((combinedStorage2 << Dim1Bits) - dataSize)

		//If the suffix wraps back to the first array (suffix(dim2(offset)) contains both first and last elements)
		// we need to copy the leading elements first, so that added arrays form a continuous sequence of elements.
		val suffixSuffixLength =
			if (suffixLength > MaxSize1 && (dim2(offset + suffixLength - 1) % suffixSize2) == dim2(offset)) {
				addAll(suffix(dim2(offset)), dim1(offset), MaxSize1)
				suffixLength - dim1(-offset)
			} else
				suffixLength
		val offset1    = dim1(offset + suffixLength - suffixSuffixLength)
		val data2      = this.data2
		val length2    = data2.length
		val mask       = length2 - 1
		val maxStorage = math.min(dim2(storageSize + suffixStorage), length2)
		val currOffset = dim2(dataOffset)
		var from       = dim2(offset + suffixLength - suffixSuffixLength) % suffixSize2
		var to         = dim2(dataOffset + dataSize - 1) + 1 & mask
		var storage2   = dim2(storageSize)
		while (storage2 < maxStorage) {
			data2(currOffset + storage2 & mask) = data2(to)
			data2(to) = suffix(from)
			from      = (from + 1) % suffixSize2
			to        = to + 1 & mask
			storage2 += 1
		}
		val newSize    = dataSize + suffixLength
		val copied     = math.min(suffixSuffixLength, (maxStorage << Dim1Bits) - storageSize - offset1)
		val prefixSize = dataSize
		val gap        = dim1(-(dataOffset + dataSize)) + offset1
		storageSize    = maxStorage << Dim1Bits
		dataSize      += copied + gap
		closeTheGap2(prefixSize, gap)
		while (dataSize < newSize) { //Possible if data2.length == MaxDim2 && suffixOffset + dim1(-(dataOffset + size)) >= MaxSize1.
			addAll(suffix(from), 0, math.min(MaxSize1, newSize - dataSize))
			from = (from + 1) % suffixSize2
		}
	}

	@tailrec final override def patchInPlace(from :Int, patch :IterableOnce[E], replaced :Int) :this.type =
		if (from < 0)
			patchInPlace(0, patch, replaced)
		else if (from > dataSize)
			patchInPlace(dataSize, patch, 0)
		else if (replaced < 0)
			patchInPlace(from, patch, 0)
		else if (replaced > dataSize - from)
			patchInPlace(from, patch, dataSize - from)
		else if (replaced == 0) {
			insertAll(from, patch); this
		} else {
			val size = patch.knownSize
			if (size > 0) {
				if (size < replaced)
					remove(from, replaced - size)
				else if (size > replaced)
					if (storageSize <= MaxSize1)
						insertKnownSize1(from, Iterator.empty, size - replaced)(SpacerValues.iterableOnce)
					else
						insertKnownSize2(from, Iterator.empty, size - replaced)(SpacerValues.iterableOnce)
				updateAll(from, patch)
			} else if (size < 0) {
				val it = patch.iterator
				var i = from; val end = i + replaced
				while (i < end && it.hasNext) {
					this(i) = it.next()
					i += 1
				}
				if (it.hasNext)
					insertAll(end, it)
				else if (i < end)
					remove(i, end - i)
			} else
				remove(from, replaced)
			this
		}


	/** Copies the elements at index range `[idx, idx + length)` in the given array to the underlying data array
	  * at absolute index (combined index for the first and second dimension) `offset`.
	  */
	private def write2(offset :Int, elems :ArrayLike[E], idx :Int, length :Int) :Unit =
		if (length > 0) {
			val indexMask = data2.length - 1
			val offset2   = dim2(offset)
			val offset1   = dim1(offset)
			var copied = MaxSize1 - offset1
			if (length <= copied)
				ArrayLike.copy(elems, idx, data2(offset2), offset1, length)
			else {
				ArrayLike.copy(elems, idx, data2(offset2), offset1, copied)
				while (copied <= length - MaxSize1) {
					ArrayLike.copy(elems, idx + copied, data2(dim2(offset + copied) & indexMask), 0, MaxSize1)
					copied += MaxSize1
				}
				if (copied < length)
					ArrayLike.copy(elems, idx + copied, data2(dim2(offset + copied) & indexMask), 0, length - copied)
			}
		}

	/** Writes `max` elements included in `elems` (as defined by the implicit type class) to a two-dimensional buffer,
	  * starting at absolute index (that is. a combined first and second dimension index in the underlying array,
	  * not in the buffer). Writing happens module the total capacity of the buffer (wraps at the end).
	  * Requires allocated space for the elements to exist.
	  */
	private def write2[Es](offset :Int, elems :Es, max :Int)(implicit values :CollectionLike[E, Es]) :Int = {
		val offset1 = dim1(offset)
		if (max <= MaxSize1 - offset1) {
			val data2   = this.data2
			val offset2 = dim2(offset)
			values.copyToArray(elems)(data2(offset2), offset1, max)
		} else {
			write2(offset, values.iterator(elems), max)
		}
	}

	/** Writes `max` elements of `elems` to a two-dimensional buffer,
	  * starting at absolute index (that is. a combined first and second dimension index in the underlying array,
	  * not in the buffer). Writing happens module the total capacity of the buffer (wraps at the end).
	  * Requires allocated space for the elements to exist.
	  */
	private def write2(offset :Int, elems :Iterator[E], max :Int) :Int = {
		val data2   = this.data2
		val mask2   = data2.length - 1
		val offset2 = dim2(offset)
		val offset1 = dim1(offset)
		var idx2    = offset2
		var idx1    = offset1
		var copied  = 0
		elems match {
			case ai :IndexedIterator[E] =>
				while (copied < max && ai.hasNext) {
					copied += ai.copyToArray(data2(idx2), idx1, max - copied)
					idx1 = 0
					idx2 = idx2 + 1 & mask2
				}
			case _ if elems.knownSize > 0 =>
				var (it, next) = elems.splitAt(MaxSize1 - offset1)
				copied += it.copyToArray(data2(idx2), offset1)
				while (copied < max && next.hasNext) {
					val (i1, i2) = next.splitAt(MaxSize1)
					it   = i1
					next = i2
					idx2 = idx2 + 1 & mask2
					copied += it.copyToArray(data2(idx2), 0, max - copied)
				}
			case _ =>
				while (copied < max && elems.hasNext) {
					val array = data2(idx2)
					while (idx1 < MaxSize1 & copied < max && elems.hasNext) {
						array(idx1) = elems.next()
						copied += 1
						idx1   += 1
					}
					idx1 = 0
					idx2 = idx2 + 1 & mask2
				}
		}
		copied
	}

	/** Reserves space for additional `delta` elements in order to avoid repeated copying and reallocation
	  * when adding elements one by one.
	  */
	def reserve(delta :Int) :Unit =
		if (delta > storageSize - dataSize)
			if (delta > Int.MaxValue - dataSize)
				bufferFull(delta)
			else if (storageSize == 0) {
				if (delta <= MinSize1) {
					storageSize = MinSize1
					data = new Array[E](MinSize1)
				} else if (delta <= MaxSize1) {
					storageSize = delta.highestOneBit
					if (storageSize < delta)
						storageSize <<= 1
					if (storageSize <= MinSize1)
						storageSize = MinSize1
					data = new Array[E](storageSize)
				} else {
					storageSize = delta.highestOneBit
					if (storageSize < delta)
						storageSize <<= 1
					if (storageSize <= (MinSize2 << Dim1Bits))
						storageSize = MinSize2 << Dim1Bits
					val storage2 = dim2(storageSize)
					val a = new Array[Array[E]](storage2)
					a(0)  = new Array[E](MaxSize1)
					a(1)  = new Array[E](MaxSize1)
					data  = a
					storageSize = MaxSize1 << 1
				}
			} else if (storageSize <= MaxSize1) {
				if (delta <= MaxSize1 - dataSize) {
					var newStorageSize = (dataSize + delta).highestOneBit
					if (newStorageSize < dataSize + delta)
						newStorageSize <<= 1
					if (dataOffset + dataSize <= storageSize)
						data = Array.copyOf(data1, newStorageSize)
					else {
						data = Array.copyOfRanges(
							data1, dataOffset, storageSize - dataOffset,
							data1, 0, storageSize - (dataOffset + dataSize), newStorageSize
						)
						dataOffset = 0
					}
					storageSize = newStorageSize
				} else
					growToDim2(delta)
			} else
				grow2(delta)

	/** Ensures there is enough allocated space at the front of a two-dimensional buffer to prepend `delta` elements.
	  * This can both resize the two-dimensional array, and allocate additional single-dimensional arrays.
	  */
	private def allocFront(delta :Int) :Unit = {
		grow2(delta)
		val length2      = data2.length
		val storageSize2 = dim2(storageSize)
		val dataOffset2  = dim2(dataOffset)
		val dataOffset1  = dim1(dataOffset)
		if (storageSize2 < length2 & dataOffset1 < delta) {
			val mask2          = length2 - 1
			var requiredArrays = dim2(delta - dataOffset1 + MaxSize1 - 1)
			var lastEmptyArray = dataOffset2 + storageSize2 - 1 & mask2
			var emptyArrays    = this.emptyArrays
			var front          = dataOffset2 - 1 & mask2
			//First, move to the front empty arrays from the back, if any.
			while (emptyArrays > 0 & requiredArrays > 0) {
				data2(front)          = data2(lastEmptyArray)
				data2(lastEmptyArray) = null
				front           = front - 1 & mask2
				lastEmptyArray  = lastEmptyArray - 1 & mask2
				requiredArrays -= 1
				emptyArrays    -= 1
			}
			var unallocated = length2 - storageSize2
			while (requiredArrays > 0 && unallocated > 0) {
				data2(front)    = new Array[E](MaxSize1)
				front           = front - 1 & mask2
				requiredArrays -= 1
				unallocated    -= 1
			}
			storageSize = length2 - unallocated << Dim1Bits
		}
	}

	/** Ensures there is enough allocated space at the end of a two-dimensional buffer to append `delta` elements.
	  * This can both resize the two-dimensional array, and allocate additional single-dimensional arrays.
	  */
	private def allocBack(delta :Int) :Unit = {
		grow2(delta)
		val newSize = dataSize + delta
		val length2 = data2.length
		val mask2   = length2 - 1
		if (delta > storageSize - dim1(dataOffset) - dataSize) {
			val newLast2 = dim2(dataOffset + newSize - 1) + 1 & mask2
			var i        = dim2(dataOffset + storageSize) & mask2
			storageSize  += (newLast2 - i) << Dim1Bits & indexMask
			while (i != newLast2) {
				data2(i) = new Array[E](MaxSize1)
				i = i + 1 & mask2
			}
		}
	}

	private def grow1(delta :Int) :Unit =
		if (dataSize + delta > storageSize) {
			var capacity = storageSize << 1
			while (capacity < dataSize + delta)
				capacity <<= 1
			if (dataOffset + dataSize <= storageSize)
				data = Array.copyOfRange(data, dataOffset, dataOffset + dataSize, dataOffset, capacity)
			else {
				data = Array.copyOfRanges(
					data1, dataOffset, storageSize,
					data1, 0, dataOffset + dataSize - storageSize, capacity
				)
				dataOffset = 0
			}
			storageSize = capacity
		}

	/** Grows the outer array in a two dimensional buffer to length `l` such that `l * MaxSize1 >= dataSize + delta`.
	  * Does not allocate any new single dimensional arrays.
	  */
	private def grow2(delta :Int) :Unit = { //, appending :Boolean) :Unit = {
		val length2            = data2.length
		val newSize            = dataSize + delta
		val lastOffset         = dataOffset + dataSize - 1 >> Dim1Bits // >>, not >>>, because arg may be negative
		val requiredForAppend  = dim2(dim1(dataOffset) + newSize + MaxSize1 - 1)
		val requiredForPrepend = lastOffset + 1 - (dataOffset - delta >> Dim1Bits)
		val preferredCapacity2 = math.max(requiredForAppend, requiredForPrepend)
		if (delta > MaxValue - dataSize)
			bufferFull(delta)
		//Lets try to not write the prefix in the same array as the suffix at dim2(dataOffset) -
		// easier to grow later, because we copy whole arrays, not individual elements.
		//I originally required specifying if its for append or prepend, but it makes reserve
		// impossible to implement reliably, so I resorted to accommodating for both. Doesn't change the complexity.
		if (preferredCapacity2 > length2 - 1 & length2 != MaxDim2) {
			//Must grow the outer array.
			var capacity2 = length2 << 1
			if (newSize <= (MaxSize2 >>> 1)) {
				while (capacity2 < preferredCapacity2)
					capacity2 <<= 1
			} else
				capacity2 = MaxSize2
			val oldOffset  = dataOffset
			val oldOffset2 = dim2(oldOffset)
			if (dataOffset + storageSize + MinValue <= (length2 << Dim1Bits) + MinValue) //length2 << Dim1Bits may overflow
				data = Array.copyOf(data, capacity2) //no wrapping
			else {
				val newData = Array.copyOfRanges(
					data2, oldOffset2, length2,
					data2, 0, dim2(dataOffset + storageSize) & length2 - 1, capacity2
				)
				dataOffset = dim1(dataOffset)
				data       = newData
			}
			if (dataSize > MaxSize1 && (lastOffset & length2 - 1) == oldOffset2) { //head and last are in the same array
				val suffixSize = dim1(oldOffset + dataSize)
				val a          = data2(dim2(dataOffset))
				val suffix     = Array.copyOf(a, MaxSize1)
				data2(dim2(dataOffset + dataSize - 1) & capacity2 - 1) = suffix
				if (a.isInstanceOf[Array[AnyRef]])
					a.clear(0, suffixSize)
//				a.clearIfRef(0, suffixSize)
				storageSize += MaxSize1
			}
		}
		if (delta > (data2.length << Dim1Bits) - dataSize)
			bufferFull(delta)
	}

	/** Grows the buffer from the first dimension to a second one, setting `data` to an `Array[Array[E]]` of capacity
	  * sufficient to store `dataSize + delta` elements. Exactly two single dimensional arrays are allocated, regardless
	  * of `delta`.
	  */
	private def growToDim2(delta :Int) :Unit = {
		//We use to check storageSize > MaxSize1 to test if the array is two dimensional, so we must allocate two arrays.
		val capacity2 = growCapacity2(delta)
		val array2    = new Array[Array[E]](capacity2)
		if (dataOffset + dataSize <= storageSize) {    //the data is not wrapped
			if (storageSize == MaxSize1)
				array2(0) = data1
			else
				array2(0) = Array.copyOf(data1, MaxSize1)
			array2(1) = new Array[E](MaxSize1)
		} else                                      //the data is wrapped
			if (storageSize == MaxSize1) {
				if (dataOffset < (MaxSize1 >> 1)) { //Reuse the current array for the prefix, copy the suffix to a new one.
					array2(0) = data1
					array2(1) = Array.copyOfRange(data1, 0, dataOffset + dataSize & Dim1Mask, MaxSize1)
//					data.clearIfRef(0, dataOffset)
					if (data.isInstanceOf[Array[AnyRef]])
						data.clear(0, dataOffset)
				} else {                            //Reuse the current array for the suffix, copy the prefix to a new one.
					array2(0) = Array.copyOfRange(data1, dataOffset, MaxSize1, dataOffset, MaxSize1)
					array2(1) = data1
//					data.clearIfRef(dataOffset, MaxSize1)
					if (data.isInstanceOf[Array[AnyRef]])
						data.clear(dataOffset, MaxSize1)
				}
			} else {
				array2(0) = Array.copyOfRanges(
					data1, dataOffset, storageSize,
					data1, 0, dataOffset + dataSize - storageSize, MaxSize1
				)
				array2(1) = new Array[E](MaxSize1)
				dataOffset = 0
			}
		storageSize = MaxSize1 << 1
		data        = array2
	}

	/** The size of the outer array to which a two dimensional buffer should grow to accommodate additional `delta` elements. */
	private def growCapacity2(delta :Int) :Int = {
		val newSize   = dataSize + delta
		var capacity2 = MaxDim2
		if (length <= (MaxSize2 >>> 1) - dataSize) {
			//Lets try to not wrap the suffix all the way up to data2(dim2(dataOffset)) - easier to grow later.
			val preferredCapacity =
				if (storageSize == MaxSize1) dataOffset + newSize
				else newSize
			capacity2 = NewSize2
			while ((capacity2 << Dim1Bits) < preferredCapacity)
				capacity2 <<= 1
		}
		capacity2
	}


	override def subtractAll(xs :IterableOnce[E]) :this.type = {
		val set = xs.toBasicOps.toSet
		filterInPlace(!set.contains(_))
	}

	@inline final def removeHead :E = remove(0)
	def removeLast :E = remove(dataSize - 1)

	override def remove(idx :Int) :E = {
		val res = apply(idx)
		remove(idx, 1)
		res
	}

	override def remove(idx :Int, count :Int) :Unit = {
		val oldStorage = storageSize
		val oldOffset  = dataOffset
		val oldSize    = dataSize
		if (count < 0)
			throw new IllegalArgumentException(errorString(this) + ".remove(" + idx + ", " + count + ")")
		else if (count > 0) {
			if (idx < 0 || idx > dataSize - count)
				throw new IndexOutOfBoundsException(errorString(this) + ".remove(" + idx + ", " + count + ")")
			if (storageSize <= MaxSize1)
				remove1(idx, count)
			else
				remove2(idx, count)
		}
		assert(storageSize > MaxSize1 || storageSize == Integer.highestOneBit(storageSize),
			"|" + oldSize + "/" + oldStorage + "#" + oldOffset + "|.remove(" + idx + ", " + count + ") == |" +
				dataSize + "/" + storageSize + "#" + dataOffset + "|"
		)
	}

	/** Implementation of `remove` for the case of a single dimensional buffer. */
	private def remove1(idx :Int, count :Int) :Unit = {
		val mask         = storageSize - 1
		val end          = idx + count
		val suffixSize   = dataSize - end
		val suffixOffset = dataOffset + end & mask
		val newLength    = shrunkDimension1(count)
		if (newLength < storageSize) {
			if (dataOffset + idx <= storageSize) {
				val newData = Array.copyOfRange(data, dataOffset, dataOffset + idx, newLength)
				if (suffixOffset + suffixSize <= storageSize)
					arraycopy(data, suffixOffset, newData, idx, suffixSize)
				else {
					val slice1Size = storageSize - suffixOffset
					arraycopy(data, suffixOffset, newData, idx, slice1Size)
					arraycopy(data, 0, newData, idx + slice1Size, suffixSize - slice1Size)
				}
				data = newData
			} else {
				data = Array.copyOfRanges(
					data1, dataOffset, storageSize,
					data1, 0, idx - (storageSize - dataOffset),
					data1, suffixOffset, suffixOffset + suffixSize, newLength
				)
			}
			dataOffset  = 0
			storageSize = newLength
		} else if (idx < suffixSize) {
			val newOffset = dataOffset + count & mask
			if (idx > 0)
				Array.cyclicCopy(data, dataOffset, data, newOffset, idx)
			clearIfRef(dataOffset, count)
//			if (dataOffset <= newOffset)
//				data.clearIfRef(dataOffset, newOffset)
//			else {
//				data.clearIfRef(dataOffset, storageSize)
//				data.clearIfRef(0, newOffset)
//			}
			dataOffset = newOffset
		} else {
			if (suffixSize > 0)
				Array.cyclicCopy(data, suffixOffset, data, dataOffset + idx & mask, suffixSize)
//			val end = dataOffset + dataSize & mask
//			val newEnd = dataOffset + dataSize - count & mask
//			if (newEnd <= end)
//				data.clearIfRef(newEnd, end)
//			else {
//				data.clearIfRef(newEnd, storageSize)
//				data.clearIfRef(0, end)
//			}
			clearIfRef(dataOffset + dataSize - count & mask, count)
		}
		dataSize -= count
	}

	/** Implementation of remove for the case of a two dimensional buffer. */
	private def remove2(idx :Int, count :Int) :Unit =
		if (shrink && shouldFlatten(count))  //Reduce the dimension, copy all remaining data to a single array.
			removeAndFlatten(idx, count)
		else if (!shrink || !removeAndShrink2(idx, count)) {
			val limit     = dim2(dataSize - count + MaxSize1 - 1) << 2 //must be >= 4 so we always have 2 arrays
			val allocated = dim2(storageSize)
			var reserved  = allocated
			while (reserved > limit)
				reserved >>= 1
			val deallocated     = allocated - reserved
			val length2         = data2.length
			val mask            = length2 - 1
			val end             = idx + count
			val dataOffset2     = dim2(dataOffset)
			val storageEnd2     = dataOffset2 + dim2(storageSize)
			val dataEnd2        = dim2(dataOffset + dataSize + MaxSize1 - 1)
			val emptyBackArrays = storageEnd2 - dataEnd2

			if (idx < (dataSize - end)) {                                   //Shift the prefix right.
				shiftRight2(dataOffset, idx, count)
				val freedPrefix = dim2(dataOffset + count) - dataOffset2 //The number of unused arrays before newOffset
				val indexMask   = (length2 << Dim1Bits) - 1
				val newOffset   = dataOffset + count & indexMask
				if (deallocated < freedPrefix) {                         //Clear some vacated arrays and move them back.
					val emptyOffset =
						if (deallocated == 0) dataOffset
						else (dataOffset2 + deallocated << Dim1Bits) & indexMask
					val storageOffset = storageEnd2 & mask
					val freed         = freedPrefix - deallocated
					val newStorageEnd = storageOffset + freed & mask
					clearIfRef(emptyOffset, count - (deallocated << Dim1Bits))
					Array.cyclicCopy(data, dim2(emptyOffset), data, storageOffset, freed)
					val deallocateEnd = dataOffset2 + freedPrefix & mask
					if (deallocateEnd >= dataOffset2)
						if (newStorageEnd <= deallocateEnd & newStorageEnd > dataOffset2)
							deallocate(newStorageEnd, deallocateEnd - newStorageEnd)
						else
							deallocate(dataOffset2, freedPrefix)
					else if (newStorageEnd <= deallocateEnd)
						deallocate(newStorageEnd, deallocateEnd - newStorageEnd)
					else if (newStorageEnd > dataOffset2)
						deallocate(newStorageEnd, freedPrefix - (newStorageEnd - dataOffset2))
					else
						deallocate(dataOffset2, freedPrefix)
					storageSize -= deallocated << Dim1Bits
				} else {                                                 //Deallocate empty arrays at the back.
					val deallocatedBack = math.max(0, math.min(emptyBackArrays, deallocated - freedPrefix))
					deallocate(storageEnd2 - deallocatedBack & mask, deallocatedBack)
					deallocate(dataOffset2, freedPrefix)
//					val dataOffset1  = dim1(newOffset)
//					val clearOffset1 = math.max(0, dataOffset1 - count)
//					data2(dim2(newOffset)).clearIfRef(clearOffset1, dataOffset1) //Deallocate old elements in the first array.
					val cleared = math.min(count, dim1(newOffset))
					clearIfRef(newOffset - cleared, cleared)                    //Deallocate old elements in the first array.
					storageSize -= deallocatedBack + freedPrefix << Dim1Bits
				}
				dataOffset = newOffset
			} else {                                                     //Shift the suffix left.
				shiftLeft2(dataOffset + end & indexMask, dataSize - end, count)
				val newEnd      = dataOffset + dataSize - count
				val newEnd2     = dim2(newEnd + MaxSize1 - 1)
				val freedSuffix = dataEnd2 - newEnd2                     //The number of arrays freed by shifting.
				if (data.isInstanceOf[Array[Array[AnyRef]]]) {
					val clearOffset = dim1(newEnd)
					if (clearOffset != 0) {                                   //Deallocate old elements in the last data array.
						val clearEnd = dim1(math.min(newEnd2 << Dim1Bits, dataOffset + dataSize) - 1) + 1
						data2(newEnd2 - 1 & mask).clear(clearOffset, clearEnd)
					}
					if (deallocated <= freedSuffix) {                    //Clear vacated arrays, shift left empty arrays.
						val clearedEnd = dataEnd2 - deallocated & mask   //The first of the freed arrays to deallocate
						val moved      = math.min(emptyBackArrays, deallocated)
						clear2IfRef(newEnd2 & mask, freedSuffix - deallocated)
						//Replace the remaining dirty arrays with the last of the clean ones.
						Array.cyclicCopy(data, storageEnd2 - moved & mask, data, clearedEnd, moved)
					} else if (deallocated < freedSuffix + emptyBackArrays) {
						//Shift last clear arrays down to the last data array.
						val moved = freedSuffix + emptyBackArrays - deallocated
						Array.cyclicCopy(data, storageEnd2 - moved & mask, data, newEnd2 & mask, moved)
					}
				}
				val deallocatedBack = math.min(deallocated, freedSuffix + emptyBackArrays)
				deallocate(storageEnd2 - deallocatedBack & mask, deallocatedBack)
				storageSize -= deallocatedBack << Dim1Bits
			}
			dataSize -= count
		}

	/** Judges if the size fell low enough to transition back to one dimension. */
	@inline private def shouldFlatten(delta :Int) :Boolean = dataSize - delta < (MaxSize1 >> 1)

	/** Called by [[net.noresttherein.sugar.collections.MatrixBuffer.remove remove]] when the buffer has two dimensions,
	  * handles the case when the buffer must be reduced to a single dimension. Must be called only
	  * when [[net.noresttherein.sugar.collections.MatrixBuffer.shouldFlatten shouldFlatten]] returned `true`.
	  */
	private def removeAndFlatten(idx :Int, count :Int) :Unit = {
		val data2         = this.data2
		val end           = idx + count
		val length2       = data2.length
		val mask2         = length2 - 1
		val dataOffset2   = dim2(dataOffset)
		val dataOffset1   = dim1(dataOffset)
		val dataEnd2      = dataOffset + dataSize - 1 >>> Dim1Bits
		val dataEnd1      = dim1(dataOffset + dataSize - 1) + 1
		val suffixOffset1 = dim1(dataOffset + end)
		val suffixSize    = dataSize - end
		val newLength     = shrunkDimension2(count)
		storageSize       = newLength
		if (newLength == MaxSize1) {                          //Reuse one of our existing arrays.
			if (idx == 0) {
				if (suffixOffset1 + suffixSize <= MaxSize1) { //The whole suffix is already in a single array.
					data = data2(dataEnd2 & mask2)
					if (dataOffset2 < dataEnd2) {
						clearIfRef(0, suffixOffset1)
						if (dataOffset2 + length2 == dataEnd2)
							clearIfRef(dataOffset1, MaxSize1 - dataOffset1)
					} else
						clearIfRef(dataOffset1, suffixOffset1 - dataOffset1)
//					data.clearIfRef(if (dataOffset2 < dataEnd2) 0 else dataOffset1, suffixOffset1)
//					if (dataOffset2 + length2 == dataEnd2)
//						data.clearIfRef(dataOffset1, MaxSize1)
				} else {                                      //The suffix is split into two arrays.
					val suffixInitSize = MaxSize1 - suffixOffset1
					val suffixTailSize = dim1(suffixOffset1 + suffixSize)
					val dirtyBeforeInit =
						if (dataOffset2 + 1 < dataEnd2) suffixOffset1 - suffixTailSize
						else math.max(dataOffset1, suffixTailSize) - suffixTailSize
					val dirtyAfterTail =
						if (dataOffset2 + length2 > dataEnd2) 0
						else MaxSize1 - dataOffset1 - suffixInitSize
					if (suffixInitSize + dirtyAfterTail <= suffixTailSize + dirtyBeforeInit) {
						//Copy the init part to the tail part array.
						data = data2(dataEnd2 & mask2)
						arraycopy(data2(dataEnd2 - 1 & mask2), suffixOffset1, data, suffixOffset1, suffixInitSize)
//						data1.clearIfRef(suffixTailSize, suffixTailSize + dirtyAfterTail)
						clearIfRef(suffixTailSize, dirtyAfterTail)
					} else {
						//Copy the tail part to the init part array.
						data = data2(dataEnd2 - 1 & mask2)
						arraycopy(data2(dataEnd2 & mask2), 0, data, 0, suffixTailSize)
//						data1.clearIfRef(suffixOffset1 - dirtyBeforeInit, suffixOffset1)
						clearIfRef(suffixOffset1 - dirtyBeforeInit, dirtyBeforeInit)
					}
				}
				dataOffset = suffixOffset1
			} else if (suffixSize == 0) {
				if (dataOffset1 + idx <= MaxSize1) {          //The whole prefix is already in a single array.
					data = data2(dataOffset2)
					if (dataOffset2 < dataEnd2) {
						clearIfRef(dataOffset1 + idx, MaxSize1 - dataOffset1 - idx)
						if (dataOffset2 + length2 == dataEnd2)
							clearIfRef(0, dataEnd1)
					} else
						clearIfRef(dataOffset1 + idx, dataSize - idx)
//					data.clearIfRef(dataOffset1 + idx, if (dataOffset2 < dataEnd2) MaxSize1 else dataEnd1)
//					if (dataOffset2 + length2 == dataEnd2)
//						data.clearIfRef(0, dataEnd1)
				} else {                                      //The prefix is split into two arrays.
					val prefixInitSize = MaxSize1 - dataOffset1
					val prefixTailSize = dim1(dataOffset + idx)
					val dirtyAfterTail =
						if (dataOffset2 + 1 < dataEnd2) dataOffset1 - prefixTailSize
						else math.min(dataOffset1, dataEnd1) - prefixTailSize
					val dirtyBeforeInit =
						if (dataOffset2 + length2 > dataEnd2) 0
						else math.max(0, dataEnd1 - prefixTailSize)
					if (prefixInitSize + dirtyAfterTail <= prefixTailSize + dirtyBeforeInit) {
						//Copy the tail part to the init part array.
						data = data2(dataOffset2 + 1 & mask2)
						arraycopy(data2(dataOffset2), dataOffset1, data, dataOffset1, prefixInitSize)
//						data1.clearIfRef(prefixTailSize, prefixTailSize + dirtyAfterTail)
						clearIfRef(prefixTailSize, dirtyAfterTail)
					} else {
						//Copy the init part to the tail part array.
						data = data2(dataOffset2)
						arraycopy(data2(dataOffset2 + 1 & mask2), 0, data, 0, prefixTailSize)
//						data1.clearIfRef(prefixTailSize, prefixTailSize + dirtyBeforeInit)
						clearIfRef(prefixTailSize, dirtyBeforeInit)
					}
				}
				dataOffset = dataOffset1
			} else if (dataOffset2 == dataEnd2) {               //All data is already in a single array
				data = data2(dataOffset2)
				if (idx < suffixSize) {
					arraycopy(data, dataOffset1, data, dataOffset1 + count, idx)
					clearIfRef(dataOffset1, count)
//					data.clearIfRef(dataOffset1, dataOffset1 + count)
					dataOffset = dataOffset1 + count
				} else {
					arraycopy(data, dataOffset1 + end, data, dataOffset1 + idx, suffixSize)
					clearIfRef(dataEnd1 - count, count)
//					data.clearIfRef(dataEnd1 - count, dataEnd1)
					dataOffset = dataOffset1
				}
			} else {                                          //dataOffset2 < dataEnd2
				//Our data is potentially split into 4 arrays:
				// 0..array1End, array1End..idx, idx+count..array3End, array3End..dataSize
				//Lets try to avoid copying one of the slices.
				val Slice1Size   = math.min(idx, MaxSize1 - dataOffset1)
				val Slice2Size   = idx - Slice1Size
				val Slice4Size   = math.min(suffixSize, dataEnd1)
				val Slice3Size   = suffixSize - Slice4Size
				val maxSliceSize = math.max(Slice1Size, math.max(Slice2Size, math.max(Slice3Size, Slice4Size)))
				maxSliceSize match {
					case Slice1Size =>
						data             = data2(dataOffset2)
						val slice2Offset = dim1(dataOffset1 + Slice1Size)
						val slice3Offset = dim1(slice2Offset + Slice2Size)
						val slice4Offset = dim1(slice3Offset + Slice3Size)
						Array.cyclicCopyTo(data2(dataEnd2 - 1 & mask2), suffixOffset1, data, slice3Offset, Slice3Size)
						Array.cyclicCopyTo(data2(dataEnd2 & mask2), dataEnd1 - Slice4Size, data, slice4Offset, Slice4Size)
						Array.cyclicCopyTo(data2(dataOffset2 + 1 & mask2), 0, data, slice2Offset, Slice2Size)
						dataOffset = dataOffset1
						val clearOffset = dataOffset1 + dataSize - count
						if (clearOffset <= MaxSize1)
							clearIfRef(clearOffset, math.min(MaxSize1, dataOffset1 + dataSize) - clearOffset)
						if (dataOffset2 + length2 == dataEnd2)
							clearIfRef(math.max(0, clearOffset - MaxSize1), dataEnd1)
					case Slice2Size =>
						data             = data2(dataOffset2 + 1 & mask2)
						dataOffset       = MaxSize1 - Slice1Size
						val slice4Offset = Slice2Size + Slice3Size
						ArrayLike.copy(data2(dataEnd2 - 1 & mask2), suffixOffset1, data, Slice2Size, Slice3Size)
						ArrayLike.copy(data2(dataEnd2 & mask2), dataEnd1 - Slice4Size, data, slice4Offset, Slice4Size)
						ArrayLike.copy(data2(dataOffset2), dataOffset1, data, dataOffset, Slice1Size) //last in order not overwrite slices 3 and 4
						val clearOffset = Slice2Size + suffixSize
						clearIfRef(clearOffset, math.min(dataOffset, dataSize - Slice1Size) - clearOffset)
					case Slice3Size =>
						data             = data2(dataEnd2 - 1 & mask2)
						val slice2Offset = dim1(suffixOffset1 - Slice2Size)
						val slice1Offset = dim1(slice2Offset - Slice1Size)
						val slice4Offset = dim1(suffixOffset1 + Slice3Size)
						Array.cyclicCopyTo(data2(dataOffset2 + 1 & mask2), 0, data, slice2Offset, Slice2Size)
						Array.cyclicCopyTo(data2(dataOffset2), dataOffset1, data, slice1Offset, Slice1Size)
						Array.cyclicCopyTo(data2(dataEnd2 & mask2), dataEnd1 - Slice4Size, data, slice4Offset, Slice4Size)
						dataOffset      = slice1Offset
						val clearOffset = math.max(Slice4Size, slice1Offset - count)
						clearIfRef(clearOffset, slice1Offset - clearOffset)
					case _          =>
						data             = data2(dataEnd2 & mask2)
						val slice4Offset = dim1(dataEnd1 - Slice4Size)
						val slice3Offset = dim1(slice4Offset - Slice3Size)
						val slice2Offset = dim1(slice3Offset - Slice2Size)
						dataOffset       = dim1(slice2Offset - Slice1Size)
						Array.cyclicCopyTo(data2(dataOffset2 + 1 & mask2), 0, data, slice2Offset, Slice2Size)
						Array.cyclicCopyTo(data2(dataOffset2), dataOffset1, data, dataOffset, Slice1Size)
						Array.cyclicCopyTo(data2(dataEnd2 - 1 & mask2), suffixOffset1, data, slice3Offset, Slice3Size)
						if (dataOffset < slice4Offset) {
							val clearOffset = math.max(0, dataEnd1 - dataSize)
							clearIfRef(clearOffset, dataOffset - clearOffset)
						}
						if (dataOffset2 + length2 == dataEnd2)
							clearIfRef(dataOffset1, math.min(0, dataEnd1 - dataSize + count) + MaxSize1 - dataOffset1)
				}
			}
		} else { //newLength < MaxSize1
			val data1 = new Array[E](newLength)
			dataOffset =
				if (dataOffset1 == 0) 0
				else if (dim1(dataOffset1 + dataSize) == 0) newLength - idx - suffixSize
				else newLength - idx - suffixSize >> 1
			val slice1Size = math.min(dataOffset1 + idx, MaxSize1) - dataOffset1
			val slice2Size = idx - slice1Size
			val slice4Size = math.min(suffixSize, dataEnd1)
			val slice3Size = suffixSize - slice4Size
			arraycopy(data2(dataOffset2), dataOffset1, data1, dataOffset, slice1Size)
			if (slice2Size > 0)
				arraycopy(data2(dataOffset2 + 1 & mask2), 0, data1, dataOffset + slice1Size, slice2Size)
			if (slice3Size > 0)
				arraycopy(data2(dataEnd2 - 1 & mask2), suffixOffset1, data1, dataOffset + idx, slice3Size)
			arraycopy(data2(dataEnd2 & mask2), dataEnd1 - slice4Size, data1, dataOffset + idx + slice3Size, slice4Size)
			data = data1
		}
		dataSize   -= count
//		storageSize = newLength
	}

	/** Called from [[net.noresttherein.sugar.collections.MatrixBuffer.remove remove]] if the buffer has two dimensions,
	  * handles the case when the second dimension array must be shrunk (reallocated and its contents copied).
	  * @return True if the array was shrunk, the specified range removed, and the buffer is in a valid state.
	  *         False if there was no need for shrinking the array.
	  */
	private def removeAndShrink2(idx :Int, count :Int) :Boolean = {
		//We allow a lower fill factor for the dim 2 array, because its length is dwarfed by the total length of dim1 arrays.
		val length2    = data2.length
		val maxLength  = math.max(dim2(dataSize - count + MaxSize1 - 1) << 3, (MinSize2 << 1) - 1)
		var newLength2 = length2
		while (newLength2 > maxLength)
			newLength2 >>= 1
		newLength2 < length2 && {
			val mask2          = length2 - 1
			val end            = idx + count
			val storageEnd2    = dim2(dataOffset + storageSize)
			val dataOffset2    = dim2(dataOffset)
			val dataEnd2       = dim2(dataOffset + dataSize + MaxSize1 - 1)
			val suffixOffset   = dataOffset + end
			val suffixOffset2  = dim2(suffixOffset)

			var dirtyOffset2   = dim2(dataOffset + idx + MaxSize1 - 1)
			var dirtyEnd2      = suffixOffset2
			var copyOffset2    = dataOffset2 //The offset of the first array with copied elements.
			var copyEnd2       = dataEnd2    //The offset of the last array with copied elements.
			var gapLength2     = 0           //The number of arrays without elements to copy between idx and idx + count.
			var newDataLength2 = 0           //The number of dim 1 arrays containing elements which need to be copied.
			if (idx == 0) {
				copyOffset2    = suffixOffset2
				dirtyOffset2   = dataOffset2
				newDataLength2 = dataEnd2 - suffixOffset2
			} else if (end == dataSize) {
				copyEnd2       = dirtyOffset2 //dim2(suffixOffset + MaxSize1 - 1)
				dirtyEnd2      = dataEnd2
				newDataLength2 = copyEnd2 - dataOffset2
			} else if (dirtyOffset2 < dirtyEnd2) {
				gapLength2     = dirtyEnd2 - dirtyOffset2
				newDataLength2 = dataEnd2 - dataOffset2 - gapLength2
			} else {
				dirtyEnd2      = dirtyOffset2
				newDataLength2 = dataEnd2 - dataOffset2
			}
			val emptySuffix2   = storageEnd2 - dataEnd2
			val copiedSuffix2  = math.min(newLength2 - newDataLength2, emptySuffix2)
			val newData        = //A reduced array, initially with carried over data arrays.
				if (gapLength2 == 0) //always if idx == 0 || idx + count == dataSize
					if ((copyOffset2 & mask2) <= (copyEnd2 - 1 & mask2))
						Array.copyOfRange(data2, copyOffset2 & mask2, (copyEnd2 - 1 & mask2) + 1, newLength2)
					else
						Array.copyOfRanges(
							data2, copyOffset2 & mask2, length2,
							data2, 0, (copyEnd2 - 1 & mask2) + 1, newLength2
						)
				else {
					val a2 = new Array[Array[E]](newLength2)
					Array.cyclicCopyFrom(data2, dataOffset2, a2, 0, dirtyOffset2 - dataOffset2)
					Array.cyclicCopyFrom(
						data2, suffixOffset2 & mask2,
						a2, dirtyOffset2 - dataOffset2 & mask2, dataEnd2 - suffixOffset2
					)
					a2
				}
			Array.cyclicCopyFrom(data2, dataEnd2 & mask2, newData, newDataLength2, copiedSuffix2)
			storageSize = newDataLength2 + copiedSuffix2 << Dim1Bits
			if (newDataLength2 + copiedSuffix2 < newLength2) {
				val dirtySize = math.min(newLength2 - (newDataLength2 + copiedSuffix2), dirtyEnd2 - dirtyOffset2)
				clear2IfRef(dirtyOffset2 & mask2, dirtySize)
				Array.cyclicCopyFrom(data2, dirtyOffset2 & mask2, newData, newDataLength2 + copiedSuffix2, dirtySize)
				storageSize += dirtySize << Dim1Bits
			}
			data = newData
			if (idx > 0 & end < dataSize) {
				dataOffset = dim1(dataOffset)
				dataSize  -= gapLength2 << Dim1Bits
				closeTheGap2(idx, count - (gapLength2 << Dim1Bits))
			} else {
				dataSize -= count
				if (idx == 0) {
//					data2(0).clearIfRef(math.max(0, dataOffset - (suffixOffset2 << Dim1Bits)) , dim1(suffixOffset))
					val clearOffset = math.max(0, dataOffset - (suffixOffset2 << Dim1Bits))
					clearIfRef(clearOffset, dim1(suffixOffset) - clearOffset)
					dataOffset = dim1(suffixOffset)
				} else { //end == dataSize
					dataOffset   = dim1(dataOffset)
					val newMask  = (newLength2 << Dim1Bits) - 1
//					val dataEnd1 = dim1(dataOffset + dataSize)
//					data2(dim2(dataOffset + dataSize) & newMask).clearIfRef(dataEnd1, math.min(MaxSize1, dataEnd1 + count))
					val dataEnd  = dataOffset + dataSize & newMask
					clearIfRef(dataEnd, math.min(count, MaxSize1 - dim1(dataEnd)))
				}
			}
			true
		}
	}

	/** Shifts individual elements to close the gap after removal of range `[idx, idx + count)`.
	  * This may involve either shifting right the range `[0, idx)` or shifting left `[idx + count, dataSize)`.
	  * Empty, allocated arrays preceding and/or following shift elements are also shift.
	  * Previously occupied positions are deallocated (if the element type is a reference type).
	  * All arrays before the new data offset (in case of a right shift) are moved to the end of the buffer.
	  * The buffer size is reduced by `count`, and the buffer itself is left in a consistent state.
	  */
	private def closeTheGap2(idx :Int, count :Int) :Unit = {
		val suffixSize = dataSize - idx - count
		if (idx < suffixSize) {
			shiftRight2(dataOffset, idx, count)
			clearFront(count)
		} else {
			val length2 = data2.length
			val mask    = (length2 << Dim1Bits) - 1
			val dataEnd = dataOffset + dataSize
			shiftLeft2(dataOffset + idx + count & mask, suffixSize, count)
			clear(dataEnd - count & mask, dataEnd & mask)
		}
		dataSize -= count

		def clear(from :Int, until :Int) :Unit =
			if (data2.isInstanceOf[Array[Array[AnyRef]]]) {
				val data2  = this.data2
				val mask   = data2.length - 1
				val until2 = dim2(until) & mask
				var i = dim2(from) & mask
				if (i == until2)
					data2(i).clear(dim1(from), dim1(until))
				else {
					data2(i).clear(dim1(from), MaxSize1)
					i = i + 1 & mask
					while (i != until2) {
						data2(i).clear()
						i = i + 1 & mask
					}
					data2(until2).clear(0, dim1(until))
				}
			}
	}

	/** Shifts right `length` elements following the absolute index `offset` (index in the underlying array,
	  * not the buffer) in a single dimensional buffer by `by` positions.
	  */
	@inline private def shiftRight1(offset :Int, length :Int, by :Int) :Unit =
		Array.cyclicCopy(data1, offset, data1, offset + by & storageSize - 1, length)

	/** Shifts left `length` elements following the absolute index `offset` (index in the underlying array,
	  * not the buffer) in a single dimensional buffer by `by` positions.
	  */
	@inline private def shiftLeft1(offset :Int, length :Int, by :Int) :Unit =
		Array.cyclicCopy(data1, offset, data1, offset - by & storageSize - 1, length)


	/** Shifts left `length` elements following the absolute index `offset` (that is, combined index in the underlying
	  * array, not an index in this buffer) by `by` positions. Shifting is down modulo the length of the array
	  * (rotation). This method only works if `this.dim == 2`.
	  */
	private def shiftLeft2(offset :Int, length :Int, by :Int) :Unit =
		if (by < 0)
			shiftRight2(offset, length, -by)
		else if (length > 0 & by > 0) { //consider: handling shift by a multiple of MaxSize1
			val data2  = this.data2
			val mask   = data2.length - 1
			val from2  = dim2(offset) & mask
			val from1  = dim1(offset)
			val until  = offset + length
			val until2 = dim2(until - 1) & mask
			val until1 = dim1(until - 1) + 1
			val start  = offset - by
			val start2 = dim2(start) & mask
			val start1 = dim1(start)
			if (from2 == until2) {
				val length1 = until1 - from1
				if (from1 == 0 & start1 == 0 & length1 == MaxSize1)
					data2.swap(from2, start2)
				else if (start1 + length1 <= MaxSize1)
					arraycopy(data2(from2), from1, data2(start2), start1, length1)
				else {
					val slice1 = MaxSize1 - start1
					arraycopy(data2(from2), from1, data2(start2), start1, slice1)
					arraycopy(data2(from2), from1 + slice1, data2(start2 + 1 & mask), 0, length1 - slice1)
				}
			} else {
				var i = from2 + 1 & mask
				var j = start2
				val slice1Size =
					if (from1 == 0 & start1 == 0) {
						data2.swap(from2, start2)
						MaxSize1
					} else if (start1 <= from1) {
						arraycopy(data2(from2), from1, data2(start2), start1, MaxSize1 - from1)
						from1 - start1
					} else {
						j = j + 1 & mask
						val slice0 = MaxSize1 - start1
						arraycopy(data2(from2), from1, data2(start2), start1, slice0)
						arraycopy(data2(from2), from1 + slice0, data2(j), 0, start1 - from1)
						MaxSize1 - start1 + from1
					}
				if (slice1Size == MaxSize1) {
					while (i != until2) {
						j = j + 1 & mask
						data2.swap(i, j)
						i = i + 1 & mask
					}
					if (until1 == MaxSize1)
						data2.swap(until2, j + 1 & mask)
					else
						arraycopy(data2(until2), 0, data2(j + 1 & mask), 0, until1)
				} else {
					val slice2Size = MaxSize1 - slice1Size
					var src = data2(i)
					var dst = data2(j)
					while (i != until2) {
						arraycopy(src, 0, dst, slice2Size, slice1Size)
						j = j + 1 & mask
						dst = data2(j)
						arraycopy(src, slice1Size, dst, 0, slice2Size)
						i = i + 1 & mask
						src = data2(i)
					}
					if (until1 <= slice1Size)
						arraycopy(src, 0, dst, slice2Size, until1)
					else {
						arraycopy(src, 0, dst, slice2Size, slice1Size)
						arraycopy(src, slice1Size, data2(j + 1 & mask), 0, until1 - slice1Size)
					}
				}
			}
		}

	/** Shifts right `length` elements following the absolute index `offset` (that is, combined index in the underlying
	  * array, not an index in this buffer) by `by` positions. Shifting is down modulo the length of the array
	  * (rotation). This method only works if `this.dim == 2`.
	  */
	private def shiftRight2(offset :Int, length :Int, by :Int) :Unit =
		if (by < 0)
			shiftLeft2(offset, length, -by)
		else if (length > 0 & by > 0) {
			val data2  = this.data2
			val mask   = data2.length - 1
			val from2  = dim2(offset) & mask
			val from1  = dim1(offset)
			val until  = offset + length
			val until2 = dim2(until - 1) & mask
			val until1 = dim1(until - 1) + 1
			val end    = until + by
			val end2   = dim2(end - 1) & mask
			val end1   = dim1(end - 1) + 1
			if (from2 == until2) {
				val length1 = until1 - from1
				if (until1 == MaxSize1 & end1 == MaxSize1 & length1 == MaxSize1)
					data2.swap(from2, end2)
				else if (length1 <= end1)
					arraycopy(data2(from2), from1, data2(end2), end1 - length1, length1)
				else {
					arraycopy(data2(from2), until1 - end1, data2(end2), 0, end1)
					arraycopy(data2(from2), from1, data2(end2 - 1 & mask), MaxSize1 - (length1 - end1), length1 - end1)
				}
			} else {
				var i = until2 - 1 & mask
				var j = end2
				val slice1Size =
					if (until1 == MaxSize1 & end1 == MaxSize1) {
						data2.swap(until2, j)
						0
					} else if (until1 <= end1) {
						arraycopy(data2(until2), 0, data2(end2), end1 - until1, until1)
						end1 - until1
					} else {
						j = end2 - 1 & mask
						val remainder = until1 - end1
						arraycopy(data2(until2), remainder, data2(end2), 0, end1)
						arraycopy(data2(until2), 0, data2(j), MaxSize1 - remainder, remainder)
						MaxSize1 - remainder
					}
				if (slice1Size == 0) {   //Move whole arrays, swapping them with the arrays at the destination
					while (i != from2) {
						j = j - 1 & mask
						data2.swap(i, j)
						i = i - 1 & mask
					}
					if (from1 == 0)
						data2.swap(from2, j - 1 & mask)
					else
						arraycopy(data2(from2), from1, data2(j - 1 & mask), from1, MaxSize1 - from1)
				} else {
					val slice2Size = MaxSize1 - slice1Size
					var src = data2(i)
					var dst = data2(j)
					while (i != from2) {
						arraycopy(src, slice2Size, dst, 0, slice1Size)
						j = j - 1 & mask
						dst = data2(j)
						arraycopy(src, 0, dst, slice1Size, slice2Size)
						i = i - 1  & mask
						src = data2(i)
					}
					if (from1 >= slice2Size)
						arraycopy(src, from1, dst, from1 - slice2Size, MaxSize1 - from1)
					else {
						arraycopy(src, slice2Size, dst, 0, slice1Size)
						arraycopy(src, from1, data2(j - 1 & mask), from1 + slice1Size, slice2Size - from1)
					}
				}
			}
		}

	/** The array size to which a single-dimensional buffer should shrink after removing `delta` elements.
	  * Will return `storageSize` if no shrinking is necessary.
	  */
	private def shrunkDimension1(delta :Int) :Int =
		if (shrink) {
			var capacity = storageSize
			val limit    = math.max(dataSize - delta << 2, (MinSize1 << 1) - 1)
			while (capacity > limit)
				capacity >>= 1
			capacity
		} else
			storageSize

	/** The capacity to which a two-dimensional buffer should shrink after removing `delta` elements.
	  * This is counted in individual elements: a value of `MaxSize1` or less indicates the buffer should shrink
	  * to a single dimension; otherwise `dim2(shrunkDimension(delta))` is the number of single dimensional
	  * arrays which should remain allocated after the removal. This in no way deals with shrinking of the outer,
	  * two dimensional array itself.
	  */
	private def shrunkDimension2(delta :Int) :Int = {
		var capacity = data2.length << Dim1Bits
		if (shrink) {
			val limit = math.max(dataSize - delta << 2, MaxSize1)
			while (capacity > limit)
				capacity >>= 1
			if (capacity <= MaxSize1) {
				capacity = MaxSize1
				val limit = math.max(dataSize - delta << 2, (MinSize1 << 1) - 1)
				while (capacity > limit)
					capacity >>= 1
			}
		}
		capacity
	}

	/** Sets all member properties of this buffer to the specified values. */
	@inline private def restore(data :Array[_], dataOffset :Int, size :Int, storageSize :Int) :Unit = {
		this.data        = data
		this.dataOffset  = dataOffset
		this.dataSize    = size
		this.storageSize = storageSize
	}

	private def clearFront(n :Int) :Unit =
		if (n > 0) {
			clearIfRef(dataOffset, n)
//			val emptyArrays = dim2(dataOffset + n) - dim2(dataOffset)
//			dropFront2(emptyArrays)
			val length2     = data2.length
			val mask        = length2 - 1
			val dataOffset2 = dim2(dataOffset)
			val storageEnd2 = dim2(dataOffset + storageSize) & mask
			val emptyArrays = dim2(dataOffset + n) - dataOffset2 //Move freed arrays to the back
			Array.cyclicCopy(data, dataOffset2, data, storageEnd2, emptyArrays)
			if (dataOffset2 < storageEnd2) //we now newOffset2 <= storageEnd2
				if (storageEnd2 + emptyArrays > dataOffset2 + length2)
					deallocate(storageEnd2 + emptyArrays - length2, dataOffset2 + length2 - storageEnd2)
				else
					deallocate(dataOffset2, emptyArrays)
			else if (storageEnd2 + emptyArrays > dataOffset2)
				deallocate(storageEnd2 + emptyArrays & mask, dataOffset2 - storageEnd2)
			else
				deallocate(dataOffset2, emptyArrays)
			dataOffset = dataOffset + n & indexMask
		}

	/** Deallocates whole single dimensional arrays in the two dimensional array between indices `from2`
	  * and `from2 + length` (treated modulo `data2.length`).
	  */
	private def deallocate(from2 :Int, length :Int) :Unit =
		if (length > 0)
			if (from2 + length <= data2.length)
				data.clear(from2, from2 + length)
			else {
				data.clear(from2, data2.length)
				data.clear(0, length - (data2.length - from2))
			}

	private def clearIfRef(start :Int, length :Int) :Unit =
		if (length > 0)
			if (storageSize <= MaxSize1) {
				if (data.isInstanceOf[Array[AnyRef]])
					if (start + length <= storageSize)
						data.clear(start, start + length)
					else {
						data.clear(start, storageSize)
						data.clear(0, start + length - storageSize)
					}
			} else if (data.isInstanceOf[Array[Array[AnyRef]]]) {
				val data2  = this.data2
				val mask   = data2.length - 1
				val start2 = dim2(start)
				var end1   = dim1(start + length - 1) + 1
				var count2 = dim2(start + length - 1) - start2
				while (count2 > 0) {
					data2(start2 + count2 & mask).clear(0, end1)
					end1    = MaxSize1
					count2 -= 1
				}
				data2(start2).clear(dim1(start), end1)
			}

	/** Clears (deallocates individual elements of the collection) arrays between indices `from2` and `until2`
	  * (treated modulo `data2.length`) in the two dimensional array.
	  */
	private def clear2IfRef(start2 :Int, length2 :Int) :Unit =
		if (data.isInstanceOf[Array[Array[AnyRef]]] & length2 > 0) {
			val data2 = this.data2
			val mask  = data2.length - 1
			var count = length2 - 1
			while (count >= 0) {
				data2(start2 + count & mask).clear()
				count -= 1
			}
		}

	/** Empties the buffer, deallocating ''all'' held memory.
	  * If you intend to shortly grow the buffer to a similar size, you have two alternatives
	  * for limiting this unallocation:
	  *   1. [[net.noresttherein.sugar.collections.MatrixBuffer.remove remove]](`0, this.size)`
	  *      (as well as [[collection.mutable.Buffer.takeInPlace takeInPlace]]`(0)`) will reduce the buffer capacity
	  *      to the default initial level (if the buffer is single dimensional), or a single array
	  *      if the buffer has two dimensions.
	  *   1. [[net.noresttherein.sugar.collections.MatrixBuffer.free free]]`()` will not deallocate any storage.
	  * The references to objects in all cases are always cleared, freeing them for garbage collection.
	  */
	override def clear() :Unit =
		if (storageSize > 0) {
			if (shrink) {
				data        = null
				storageSize = 0
				dataOffset  = 0
//			} else if (storageSize <= MaxSize1) {
//				if (dataOffset + dataSize <= storageSize)
//					data.clearIfRef(dataOffset, dataOffset + dataSize)
//				else {
//					data.clearIfRef(0, dataOffset + dataSize - storageSize)
//					data.clearIfRef(dataOffset, storageSize)
//				}
			} else {
				clearIfRef(dataOffset, dataSize)
				dataOffset = dim2(dataOffset) << Dim1Bits
			}
			dataSize   = 0
		}

	/** Similar to [[net.noresttherein.sugar.collections.MatrixBuffer.clear clear]]`()`,
	  * but does not deallocate current storage.
	  */
	def free() :this.type = {
		clearIfRef(dataOffset, dataSize)
		dataSize   = 0
		dataOffset = dim2(dataOffset) << Dim1Bits
		this
	}


	override def sortInPlace[B >: E]()(implicit ord :Ordering[B]) :this.type = dim match {
		case 0 => this
		case 1 => Sorting.stableSort(data1.asInstanceOf[Array[B]], dataOffset, dataOffset + dataSize); this
		case _ => super.sortInPlace[B]()
	}

	override def foreach[U](f :E => U) :Unit = foreach(0, dataSize)(f)

	override def foreach[U](from :Int, until :Int)(f :E => U) :Unit =
		if (until > 0 & until > from && from < dataSize) {
			if (storageSize <= MaxSize1) {
				val data1 = this.data1
				val mask1 = storageSize - 1
				val start = dataOffset + math.max(from, 0) & mask1
				val end   = dataOffset + math.min(dataSize, until)
				if (end <= storageSize)
					ArrayLikeOps.foreach(data1, start, end)(f)
				else {
					ArrayLikeOps.foreach(data1, start, storageSize)(f)
					ArrayLikeOps.foreach(data1, 0, end - storageSize)(f)
				}
			} else {
				val data2 = this.data2
				val mask  = (data2.length << Dim1Bits) - 1
				val mask2 = data2.length - 1
				val start = dataOffset + math.max(from, 0) & mask
				val end   = dataOffset + math.min(dataSize, until)
				val end1  = dim1(end - 1) + 1
				var idx2  = dim2(start)
				var idx1  = dim1(start)
				var count = (end - 1 >>> Dim1Bits) - idx2
				while (count > 0) {
					ArrayLikeOps.foreach(data2(idx2), idx1, MaxSize1)(f)
					idx2   = idx2 + 1 & mask2
					idx1   = 0
					count -= 1
				}
				ArrayLikeOps.foreach(data2(idx2), idx1, end1)(f)
			}
		}

	override def foldLeft[A](z :A)(op :(A, E) => A) :A =
		if (storageSize <= MaxSize1)
			if (dataOffset + dataSize <= storageSize)
				ArrayLikeOps.foldLeft(data1, dataOffset, dataOffset + dataSize)(z)(op)
			else {
				val acc = ArrayLikeOps.foldLeft(data1, dataOffset, storageSize)(z)(op)
				ArrayLikeOps.foldLeft(data1, 0, dataOffset + dataSize - storageSize)(acc)(op)
			}
		else
			MatrixBuffer.foldLeft2(data2, dataOffset, dataSize)(z)(op)

	override def foldRight[A](z :A)(op :(E, A) => A) :A =
		if (storageSize <= MaxSize1)
			if (dataOffset + dataSize <= storageSize)
				ArrayLikeOps.foldRight(data1, dataOffset, dataOffset + dataSize)(z)(op)
			else {
				val acc = ArrayLikeOps.foldRight(data1, 0, dataOffset + dataSize - storageSize)(z)(op)
				ArrayLikeOps.foldRight(data1, dataOffset, storageSize)(acc)(op)
			}
		else
			MatrixBuffer.foldRight2(data2, dataOffset, dataSize)(z)(op)

	override def reduceLeft[U >: E](op :(U, E) => U) :U =
		if (dataSize == 0) throw new UnsupportedOperationException(className + "().reduceLeft")
		else reduceLeftImpl(op)

	override def reduceLeftOption[U >: E](op :(U, E) => U) :Option[U] =
		if (dataSize == 0) None
		else Some(reduceLeftImpl(op))

	private def reduceLeftImpl[U >: E](op :(U, E) => U) :U =
		if (storageSize <= MaxSize1) {
			val dataEnd = dataOffset + dataSize
			val head    = data1(dataOffset)
			val until   = dataOffset + 1 & data.length - 1
			if (dataEnd <= storageSize)
				ArrayLikeOps.foldLeft[U, E](data1, until, dataEnd)(head)(op)
			else {
				val acc = ArrayLikeOps.foldLeft[U, E](data1, until, storageSize)(head)(op)
				ArrayLikeOps.foldLeft[U, E](data1, 0, dataEnd - storageSize)(acc)(op)
			}
		} else {
			val head = data2(dim2(dataOffset))(dim1(dataOffset))
			MatrixBuffer.foldLeft2[U, E](data2, dataOffset + 1 & indexMask, dataSize - 1)(head)(op)
		}

	override def reduceRight[U >: E](op :(E, U) => U) :U =
		if (dataSize == 0) throw new UnsupportedOperationException(className + "().reduceRight")
		else reduceRightImpl(op)

	override def reduceRightOption[U >: E](op :(E, U) => U) :Option[U] =
		if (dataSize == 0) None
		else Some(reduceRightImpl(op))

	private def reduceRightImpl[U >: E](op :(E, U) => U) :U =
		if (storageSize <= MaxSize1) {
			val dataEnd = dataOffset + dataSize
			val until   = dataEnd - 1 & data.length - 1
			val last    = data1(until)
			if (dataEnd <= storageSize)
				ArrayLikeOps.foldRight[E, U](data1, dataOffset, dataEnd - 1)(last)(op)
			else {
				val acc = ArrayLikeOps.foldRight[E, U](data1, 0, until)(last)(op)
				ArrayLikeOps.foldRight[E, U](data1, dataOffset, storageSize)(acc)(op)
			}
		} else {
			val i    = dataOffset + dataSize - 1 & indexMask
			val last = data2(dim2(i))(dim1(i))
			MatrixBuffer.foldRight2[E, U](data2, dataOffset, dataSize - 1)(last)(op)
		}


	override def javaIterator[I <: JavaIterator[_]](implicit shape :JavaIteratorShape[E, I]) :I = dim match {
		case _ if dataSize == 0 =>
			JavaIterator()
		case 1 if dataOffset + dataSize <= storageSize =>
			JavaIterator.slice(data1, dataOffset, dataOffset + dataSize)
		case 1 =>
			JavaConcatIterator(
				JavaIterator.slice(data1, dataOffset, storageSize),
				JavaIterator.slice(data1, 0, dataOffset + dataSize - storageSize)
			)
		case _ if dim2(dataOffset + dataSize - 1) == dim2(dataOffset) =>
			JavaIterator.slice(data2(dim2(dataOffset)), dim1(dataOffset), dim1(dataOffset) + dataSize)
		case _ =>
			val iters  = TemporaryIndexedSeq.newBuilder[I]
			val data2  = this.data2
			val mask   = data2.length - 1
			var i      = dim2(dataOffset)
			val end    = dim2(storageSize + dataSize - 1) + 1 & mask
			var offset = dim1(dataOffset)
			iters sizeHint dim2(storageSize + dataSize - 1) + 1 - i
			while (i != end) {
				iters += JavaIterator.slice(data2(i), offset, MaxSize1)
				i = i + 1 & mask
				offset = 0
			}
			iters += JavaIterator.slice(data2(i), 0, dim1(dataOffset + dataSize - 1) + 1)
			JavaConcatIterator(iters.result())
	}

	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S with EfficientSplit = dim match {
		case _ if dataSize == 0 =>
			Stepper.empty
		case 1 if dataOffset + dataSize <= storageSize =>
			Stepper.slice(data1, dataOffset, dataOffset + dataSize)
		case 1 =>
			ConcatStepper(
				Stepper.slice(data1, dataOffset, storageSize),
				Stepper.slice(data1, 0, dataOffset + dataSize - storageSize),
			)
		case 2 if dim2(dataOffset + dataSize - 1) == dim2(dataOffset) && dataSize <= MaxSize1 =>
			Stepper.slice(data2(dim2(dataOffset)), dim1(dataOffset), dim1(dataOffset) + dataSize)
		case 2 =>
			val iters  = TemporaryIndexedSeq.newBuilder[S with EfficientSplit]
			val data2  = this.data2
			val mask   = data2.length - 1
			var i      = dim2(dataOffset)
			val end    = dim2(dataOffset + dataSize - 1) & mask
			var offset = dim1(dataOffset)
			iters sizeHint dim2(dataOffset + dataSize - 1) + 1 - i
			while (i != end) {
				iters += Stepper.slice(data2(i), offset, MaxSize1)
				i = i + 1 & mask
				offset = 0
			}
			iters += Stepper.slice(data2(i), 0, dim1(dataOffset + dataSize - 1) + 1)
			ConcatStepper(iters.result())
	}

	override def iterator :Iterator[E] =
		if (dataSize == 0)
			Iterator.empty
		else if (storageSize <= MaxSize1)
			new CyclicArrayIterator(data1, dataOffset, dataSize)
		else
			new MatrixDim2BufferIterator(data2, dataOffset, dataSize)

	override def reverseIterator :Iterator[E] =
		if (dataSize == 0)
			Iterator.empty
		else if (storageSize <= MaxSize1)
			new ReverseCyclicArrayIterator(data1, dataOffset + dataSize & data.length - 1, dataSize)
		else
			new ReverseDim2MatrixBufferIterator(data2, dataOffset + dataSize & indexMask, dataSize)

	override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
		copyRangeToArray(xs, start, 0, len)

	override def copyRangeToArray[B >: E](xs :Array[B], start :Int, from :Int, len :Int) :Int =
		if (len <= 0 || dataSize == 0 || from >= dataSize || start >= xs.length)
			0
		else if (start < 0)
			outOfBounds_!(start, xs.length)
		else if (storageSize <= MaxSize1) {
			val from0  = math.max(from, 0)
			val copied = math.min(len, math.min(xs.length - start, dataSize - from0))
			ArrayLike.cyclicCopyFrom(data, dataOffset + from0 & storageSize - 1, xs, start, copied)
			copied
		} else {
			val from0  = math.max(from, 0)
			val copied = math.min(len, math.min(xs.length - start, dataSize - from0))
			val mask   = indexMask
			val offset = dataOffset + from0 & mask
			MatrixBuffer.copyToArray2(data2, offset, xs, start, copied)
			copied
		}

	override def cyclicCopyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int =
		cyclicCopyRangeToArray(xs, start, 0, len)

	override def cyclicCopyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int =
		if (len <= 0 || dataSize == 0 || from >= dataSize || start >= xs.length)
			0
		else if (start < 0)
			outOfBounds_!(start, xs.length)
		else if (storageSize <= MaxSize1) {
			val max    = xs.length
			val copied = math.min(len, math.min(max, dataSize - math.max(from, 0)))
			ArrayLike.cyclicCopy(data, dataOffset + from & storageSize - 1, xs, start % max, copied)
			copied
		} else {
			val from0   = math.max(from, 0)
			val cap     = xs.length
			val start0  = start % cap
			val mask    = indexMask
			val max     = math.min(len, dataSize - from0)
			val slice1  = math.min(max, cap - start0)
			val copied  = math.min(cap, max)
			val offset1 = dataOffset + from0 & mask
			val offset2 = dataOffset + from0 + slice1 & mask
			MatrixBuffer.copyToArray2(data2, offset1, xs, start0, slice1)
			if (slice1 != copied)
				MatrixBuffer.copyToArray2(data2, offset2, xs, start0 + slice1 % cap, copied - slice1)
			copied
		}


	private def bufferFull[Es](elems :Es)(implicit values :CollectionLike[E, Es]) :Nothing =
		throw new BufferFullException(
			"Cannot add " + values.infoString(elems) + " to " + errorString(this) + ": maximum capacity exceeded."
		)
	private def bufferFull(delta :Int) :Nothing =
		throw new BufferFullException(
			"Cannot add " + delta + " elements to a buffer of size " + dataSize + ": maximum capacity exceeded."
		)

	override def iterableFactory :SeqFactory[MatrixBuffer] = if (shrink) ShrinkingMatrixBuffer else MatrixBuffer
	protected override def evidenceIterableFactory :EvidenceIterableFactory[MatrixBuffer, ClassTag] =
		if (shrink) ShrinkingMatrixBuffer.specific else MatrixBuffer.specific

	protected override def className :String = "MatrixBuffer[" + iterableEvidence.runtimeClass.localName + "]"

//	override def toString :String = mkString(className + "|" + dataSize + "/" + storageSize + "|#" + dataOffset + "(", ", ", ")")
}




//consider: making it an EvidenceIterableFactory[Maybe[ClassTag[_]]. The problem is that Maybe is a value class
// and methods accepting it as an argument will clash with their generic bridges inherited from EvidenceIterableFactory.

/** A factory creating $Coll instances which never shrink their underlying storage.
  * @inheritdoc
  * @see [[net.noresttherein.sugar.collections.ShrinkingMatrixBuffer$ ShrinkingMatrixBuffer]]
  */
@SerialVersionUID(Ver)
case object MatrixBuffer extends MatrixBufferFactory(false) {
	final val shrinking = ShrinkingMatrixBuffer

	/** Projects an absolute index to the index within some single dimensional array under a two dimensional array.
	  * @return the positive remainder of division of `idx` by `MaxSize1`.
	  */
	@inline private def dim1(idx :Int) :Int = idx & Dim1Mask

	/** Projects an absolute index (or a total capacity) to the corresponding index in the outer, two dimensional array.
	  * The argument is treated as ''unsigned'', and will always produce a non negative value.
	  * Passing a negative argument will work only if the result is later 'and'ed with a mask:
	  * `dim2(x - y) & data2.length - 1`. Prefer using `dim2(x + MaxSize1 - 1)` over `dim2(x - 1) + 1`,
	  * and divide manually where sign is uncertain.
	  * @return `idx >>> MaxSize1`
	  */
	@inline private def dim2(idx :Int) :Int = idx >>> Dim1Bits

	//Todo: use of storageSize in elements, rather than 'outer array size' means we can't have 2^32-1 elements,
	// just 2^31, because 2^32 == 0 (overflow).
	//Doesn't make sense to have MinSize1 != newSize1 or MinSize2 != newSize2,
	// because any removal just after allocation will result in reallocation to a smaller array.
	private final val Dim1Bits = 15                      //The log2 of the maximum size of arrays of the first dimension.
	private final val Dim2Bits = 16                      //The log2 of the maximum size of two dimensional arrays.
	private final val MaxDim1  = 1 << Dim1Bits           //The maximum length of a single dimensional array.
	private final val MaxDim2  = 1 << Dim2Bits           //The maximum length of a two dimensional array.
	private final val MaxSize1 = 1 << Dim1Bits           //The maximum capacity of a one dimensional buffer (same as MaxDim1).
	private final val MaxSize2 = MaxDim2 << Dim1Bits     //The maximum capacity of a two dimensional buffer (unsigned).
	private final val MaxSize  = MaxSize2                //The maximum buffer size (Int.MaxValue).
	private final val MinSize1 = 8                       //The minimal length of a one dimensional array.
	private final val MinSize2 = 8                       //The minimal length of a two dimensional array.
	private final val NewSize1 = 16                      //The initial length of newly created single dimensional arrays.
	private final val NewSize2 = 16                      //The initial length of newly created two dimensional arrays.
	private final val Dim1Mask = MaxSize1 - 1            //A mask for the lower Dim1Bits.
	private final val Dim2Mask = MaxDim2 - 1 << Dim1Bits //A mask for the higher Dim2Bits corresponding to the second dimension index.

	private def foldLeft2[A, E](array :Array[Array[E]], start :Int, length :Int)(z :A)(op :(A, E) => A) :A =
		if (length == 0)
			z
		else {
			val mask2  = array.length - 1
			var idx2   = dim2(start)
			var idx1   = dim1(start)
			var count2 = dim2(start + length - 1) - idx2
			var res    = z
			while (count2 > 0) {
				res     = ArrayLikeOps.foldLeft(array(idx2), idx1, MaxSize1)(res)(op)
				idx1    = 0
				idx2    = idx2 + 1 & mask2
				count2 -= 1
			}
			val end1  = dim1(start + length - 1) + 1
			ArrayLikeOps.foldLeft(array(idx2), idx1, end1)(res)(op)
		}
	private def foldRight2[E, A](array :Array[Array[E]], start :Int, length :Int)(z :A)(op :(E, A) => A) :A =
		if (length == 0)
			z
		else {
			val mask2  = array.length - 1
			var idx2   = dim2(start + length - 1) & mask2
			var idx1   = dim1(start + length - 1) + 1
			var count2 = dim2(start + length - 1) - dim2(start)
			var res   = z
			while (count2 > 0) {
				res     = ArrayLikeOps.foldRight(array(idx2), 0, idx1)(res)(op)
				idx1    = MaxSize1
				idx2    = idx2 - 1 & mask2
				count2 -= 1
			}
			ArrayLikeOps.foldRight(array(idx2), dim1(start), idx1)(res)(op)
		}

	private def copyToArray2[E](data :Array[Array[E]], from :Int, xs :Array[_ >: E], start :Int, count :Int) :Unit = {
		val mask   = data.length - 1
		val until  = from + count
		val until2 = dim2(until - 1)
		val until1 = dim1(until - 1) + 1
		var from2  = dim2(from)
		var from1  = dim1(from)
		var count2 = until2 - from2
		var to     = start
		while (count2 > 0) {
			ArrayLike.copy(data(from2), from1, xs, to, MaxSize1 - from1)
			to    += MaxSize1 - from1
			from1  = 0
			from2  = from2 + 1 & mask
			count2 -= 1
		}
		ArrayLike.copy(data(from2), from1, xs, to, until1 - from1)
	}


	/** A dummy `Values` type class whose iteration methods do nothing, and which does not copy anything to arrays,
	  * but which still reports the correct collection size. Used to reserve space in the buffer by hijacking
	  * `insert` method.
	  */
	private class SpacerValues[+X, -Xs](implicit values :CollectionLike[X, Xs]) extends CollectionLikeProxy[X, Xs](values) {
//		override def hasFastSlice(xs :Xs) :Boolean = false
//		override def hasFastDrop(xs :Xs) :Boolean = false
//		override def isConsumable(xs :Xs) :Boolean = false
//		override def prefersDropOverIterator(xs :Xs) :Boolean = false

		override def iterator(xs :Xs) :Iterator[X] = Iterator.empty
		override def toIterableOnce(elems :Xs) :IterableOnce[X] = Nil
//		override def slice(xs :Xs, from :Int, until :Int) :IterableOnce[X] = Nil
//		override def drop(xs :Xs, n :Int) :IterableOnce[X] = Nil
//		override def consume[U](xs :Xs)(n :Int)(f :X => U) :IterableOnce[X] = values.drop(xs, n)

		override def foreach[U](xs :Xs)(f :X => U) :Unit = {}
		override def foldLeft[A](xs :Xs)(zero :A)(op :(A, X) => A) :A = zero
		override def reduceLeft[A >: X](xs :Xs)(op :(A, X) => A) :A = iterator(xs).reduceLeft(op)
		override def reduceLeftOption[A >: X](xs :Xs)(op :(A, X) => A) :Option[A] = None

		override def copyTo[A >: X](elems :Xs)(seq :mutable.Seq[A], index :Int) :Int =
			copiedCount(elems, math.min(seq.length - index, math.max(values.size(elems), 0)))

		override def copyToArray[A >: X](xs :Xs)(array :Array[A], index :Int, max :Int) :Int =
			copiedCount(xs, math.min(array.length - index, math.max(max, 0)))

		override def cyclicCopyToArray[A >: X](xs :Xs)(array :Array[A], index :Int, max :Int) :Int =
			copiedCount(xs, math.min(array.length, math.max(max, 0)))

		private def copiedCount(xs :Xs, max :Int) :Int = {
			val size = values.knownSize(xs)
			if (size >= 0)
				math.min(size, max)
			else {
				val i   = values.iterator(xs)
				var res = 0
				while (res < max && i.hasNext) {
					res += 1
					i.next()
				}
				res
			}
		}

		override def infoString(elems :Xs) :String = values.infoString(elems)
	}


	private object SpacerValues {
		def iterableOnce[X] :CollectionLike[X, IterableOnce[X]] = iterableOncePrototype.castParams[X, IterableOnce[X]]
		private[this] val iterableOncePrototype = new SpacerValues()(IterableOnceLike.forIterableOnce)
	}


	private class MatrixDim2BufferIterator[+E](data2 :Array[Array[E]],
	                                           private[this] var idx :Int, private[this] var remaining :Int)
		extends IndexedIterator[E]
	{
		private[this] val mask = (data2.length << Dim1Bits) - 1
		protected override def underlyingSize :Int = ??!
		protected override def index :Int = idx
		protected override def index_=(value :Int) :Unit = { remaining -= value - idx; idx = value & mask }
		protected override def limit :Int = idx + remaining
		protected override def limit_=(value :Int) :Unit = remaining = value - idx

		override def hasNext :Boolean = remaining > 0
		override def head :E = data2(dim2(idx))(dim1(idx))
		override def next() :E = {
			if (remaining <= 0)
				noSuch_!("Iterator.empty")
			val hd = data2(dim2(idx))(dim1(idx))
			idx = idx + 1 & mask
			remaining -= 1
			hd
		}

		override def foldLeft[A](z :A)(op :(A, E) => A) :A = {
			val res = MatrixBuffer.foldLeft2(data2, idx, remaining)(z)(op)
			remaining = 0
			res
		}

		override def copyToArray[U >: E](xs :Array[U], start :Int, len :Int) :Int = {
			val xsLength = xs.length
			if (len <= 0 | xsLength == 0 | start >= xsLength || remaining <= 0)
				0
			else if (start < 0)
				outOfBounds_!(start, xsLength)
			else {
				val copied = math.min(remaining, math.min(len, xsLength - start))
				MatrixBuffer.copyToArray2(data2, idx, xs, start, copied)
				idx       += copied
				remaining -= copied
				copied
			}
		}
	}

	private class ReverseDim2MatrixBufferIterator[+E](data2 :Array[Array[E]],
	                                                  private[this] var idx :Int, private[this] var remaining :Int)
		extends IndexedReverseIterator[E]
	{
		private[this] val mask = (data2.length << Dim1Bits) - 1
		protected override def underlyingSize :Int = ??!
		protected override def index :Int = idx
		protected override def index_=(value :Int) :Unit = { remaining -= idx - value; idx = value & mask }
		protected override def limit :Int = idx - remaining
		protected override def limit_=(value :Int) :Unit = remaining = idx - value

		override def hasNext :Boolean = remaining > 0
		override def head :E = { val i = idx - 1 & mask; data2(dim2(i))(dim1(i)) }
		override def next() :E = {
			if (remaining <= 0)
				noSuch_!("Iterator.empty")
			idx        = idx - 1 & mask
			remaining -= 1
			data2(dim2(idx))(dim1(idx))
		}
		override def foldLeft[A](z :A)(op :(A, E) => A) :A = {
			val res = MatrixBuffer.foldLeft2(data2, idx, remaining)(z)(op)
			remaining = 0
			res
		}
	}
}


/** A factory creating $Coll instances which shrink their underlying storage when their fill factor
  * drops below a certain threshold. This threshold varies based on the dimension of the storage array:
  * for single-dimensional arrays, it is four, but two dimensional arrays require a much lower factor
  * to shrink the ''outer'' array, but release the ''inner'' data arrays more readily.
  * @define coll shrinking matrix buffer
  * @see [[net.noresttherein.sugar.collections.MatrixBuffer$ MatrixBuffer]]
  */
@SerialVersionUID(Ver)
case object ShrinkingMatrixBuffer extends MatrixBufferFactory(true)




/** The element type of the backing array depends on whether the buffer was created
  * from an array-backed collection, such as [[scala.collection.mutable.ArraySeq ArraySeq]]. If such a collection
  * is passed as an argument to [[net.noresttherein.sugar.collections.MatrixBufferFactory.from from]], the buffer
  * will use the same element type. In all other cases, including builders, the created buffer is backed by `Array[Any]`
  * like a regular [[scala.collection.mutable.ArrayBuffer ArrayBuffer]].
  *
  * Alternatively, one of the two member factories can be used for finer control:
  *   - [[net.noresttherein.sugar.collections.MatrixBuffer.specific specific]]
  *     is a [[scala.collection.ClassTagSeqFactory ClassTagSeqFactory]], whose all methods require a class tag,
  *     and which builds buffers backed by arrays of specific types.
  *   - [[net.noresttherein.sugar.collections.MatrixBuffer.untagged untagged]]
  *     is a regular [[scala.collection.SeqFactory SeqFactory]] building buffers backed by `Array[Any]`,
  *     just like `ArraySeq.`[[scala.collection.mutable.ArraySeq.untagged untagged]].
  * @define Coll     `MatrixBuffer`
  * @define coll     matrix buffer
  * @define MaxSize1 65536
  */
@SerialVersionUID(Ver)
sealed class MatrixBufferFactory protected (shrink :Boolean)
	extends StrictOptimizedSeqFactory[MatrixBuffer] with BufferFactory[MatrixBuffer]
{
	override def from[E](it :IterableOnce[E]) :MatrixBuffer[E] = it match {
		case buffer :MatrixBuffer[E @unchecked]  => new MatrixBuffer[E](shrink)(buffer.iterableEvidence) ++= it
		case seq :mutable.ArraySeq[E @unchecked] => new MatrixBuffer[E](shrink)(seq.elemTag.castParam[E]) ++= it
//		case seq :ArraySeq[E@unchecked] =>
//			new MatrixBuffer[E]()(ClassTag[E](seq.unsafeArray.getClass.getComponentType)) ++= it
//		case arr :ArrayBacked[E@unchecked] =>
//			new MatrixBuffer[E]()(ClassTag[E](arr.unsafeArray.getClass.getComponentType)) ++= it
		case _ => new ErasedMatrixBuffer[E](true) ++= it
	}

	override def empty[E] :MatrixBuffer[E] = new ErasedMatrixBuffer[E](shrink)

	override def newBuilder[E] :Builder[E, MatrixBuffer[E]] = untagged.newBuilder

	/** Creates a new buffer of the specified capacity, backed by an `Array[Any]`, boxing all elements. Same as `empty`.
	  * If the capacity calls for a two dimensional buffer, not all individual data arrays will be actually allocated,
	  * but the outer `Array[Array[E]]` will be of a sufficient size to not require reallocation as elements are added.
	  */
	override def ofCapacity[E](capacity :Int) :MatrixBuffer[E] = new ErasedMatrixBuffer(capacity, shrink)


	/** A $Coll factory producing instances backed by arrays of specific types, similarly to `mutable.ArraySeq`. */
	object specific extends StrictOptimizedClassTagSeqFactory[MatrixBuffer] {
		override def from[E :ClassTag](it :IterableOnce[E]) :MatrixBuffer[E] = new MatrixBuffer[E](shrink) ++= it

		override def empty[E :ClassTag] :MatrixBuffer[E] = new MatrixBuffer[E](shrink)

		/** A new, empty buffer backed by an `Array[E]`. Same as `empty[E]`. */
		def of[E :ClassTag] :MatrixBuffer[E] = new MatrixBuffer[E](shrink)

		def ofCapacity[E :ClassTag](capacity :Int) :MatrixBuffer[E] = new MatrixBuffer[E](capacity, shrink)

		override def newBuilder[E :ClassTag] :Builder[E, MatrixBuffer[E]] =
			new MatrixBuffer[E](shrink) with Builder[E, MatrixBuffer[E]] {
				override def sizeHint(size :Int) :Unit = {
					val length = this.length
					if (size > length)
						reserve(size - length)
				}
				override def result() = this
			}

		override def toString :String = MatrixBufferFactory.this.toString + ".specific"
	}


	/** A $Coll factory producing instances backed by `Array[AnyRef]`, similarly to `mutable.ArraySeq.untagged`. */
	object untagged extends BufferFactory[MatrixBuffer] {
		override def empty[E] :MatrixBuffer[E] = new ErasedMatrixBuffer[E](shrink)

		override def newBuilder[E] :Builder[E, MatrixBuffer[E]] =
			new ErasedMatrixBuffer[E](shrink) with Builder[E, ErasedMatrixBuffer[E]] {
				override def sizeHint(size :Int) :Unit = {
					val length = this.length
					if (size > length)
						reserve(size - length)
				}
				override def result() = this
			}

		override def ofCapacity[E](capacity :Int) :MatrixBuffer[E] = new ErasedMatrixBuffer[E](capacity, shrink)

		override def toString :String = MatrixBufferFactory.this.toString + ".untagged"
	}
}






@SerialVersionUID(Ver)
private sealed class ErasedMatrixBuffer[E](initialCapacity :Int, shrink :Boolean)
	extends MatrixBuffer[E](initialCapacity, shrink :Boolean)(ClassTag.Any.castParam[E])
	   with IterableFactoryOverrides[E, MatrixBuffer]
{
	def this(initialCapacity :Int) = this(initialCapacity, false)
	def this(shrink :Boolean) = this(0, shrink)
	def this() = this(0, false)
	protected override def className :String = "MatrixBuffer"
}
