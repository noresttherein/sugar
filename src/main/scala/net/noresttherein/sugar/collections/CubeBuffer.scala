package net.noresttherein.sugar.collections

import java.lang.System.arraycopy
import java.util.ConcurrentModificationException

import scala.annotation.{switch, tailrec}
import scala.collection.mutable.Builder
import scala.collection.{EvidenceIterableFactory, EvidenceIterableFactoryDefaults, SeqFactory, StrictOptimizedClassTagSeqFactory, StrictOptimizedSeqFactory, StrictOptimizedSeqOps, mutable}
import scala.reflect.ClassTag

import net.noresttherein.sugar.!!!
import net.noresttherein.sugar.collections.CubeBuffer.{Cube, Cube1, Cube1Dimension, Cube1Ops, Cube2, Cube2Dimension, Cube2Ops, Cube3, Cube3Dimension, Cube3Ops, Cube4, Cube4Dimension, Cube4Ops, Cube5, Cube5Dimension, Cube5Ops, CubeBufferIterator, CubeDimension, CubeOps, DimBits, DimMask, DimMask1, InitialCapacity, InitialCapacityLog, Items, IterableOnceItems, MaxEdge, MaxSize1, MaxSize2, MaxSize3, MinCapacity, SingleItem, newCapacity, project, remainder, side1, side2}
import net.noresttherein.sugar.collections.util.hasFastDrop
import net.noresttherein.sugar.extensions.{AnyRefExtension, ArrayExtension, ArrayLikeExtension, ArrayObjectExtension, BooleanExtension, ClassExtension, IntExtension, IterableOnceExtension, LongExtension, castTypeParamMethods, castingMethods, classNameMethods}
import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.funny.generic.Self




/** A mutable, growing array of up to $MaxDim dimensions. It behaves like a standard growing buffer,
  * but the underlying array may contain full arrays of lower dimension(s) instead of individual elements.
  * Computational complexity of all operations is the same as for [[collection.mutable.ArrayBuffer ArrayBuffer]],
  * with only minor performance differences:
  *    1. Random access is slightly more expensive, as is append/prepend of individual elements,
  *       potentially requiring addressing $MaxDim arrays.
  *    1. Append and prepend have the same computational complexity, regardless of past operations.
  *    1. Each array is of a moderate size, playing nice with the heap memory manager.
  *    1. Growing the capacity doesn't require a reallocation of the whole buffer, and at most $MaxDim arrays
  *       can be allocated (of which at most one has its contents copied) by an append/prepend,
  *       providing the elements fit in a single dimension. Additionally, memory freed at one end of the buffer
  *       can potentially be reclaimed on the other end.
  *    1. Unlike `mutable.ArrayBuffer`, but like other buffer implementations, `CubeBuffer` will free parts
  *       of its memory for garbage collection if its fill ratio falls below a certain threshold.
  *    1. Buffers of the second and higher dimensions have generally lesser memory footprint than array buffers,
  *       as the larger the buffer, the larger percentage of its allocated memory is always used.
  *       It is always filled in at least 25%, and, if data is only either prepended or appended, and never removed,
  *       it is always above 50%, which further increases to make the unused memory space effectively `O(1)`
  *       for dimensions three and up.
  *    1. Unlike `ArrayBuffer`, but like `mutable.`[[collection.mutable.ArraySeq ArraySeq]], its companion object
  *       creates buffers based on arrays with any requested element type. Like in the latter, it is, however,
  *       possible to obtain the autoboxing behaviour of `ArrayBuffer` by using
  *       [[net.noresttherein.sugar.collections.CubeBuffer$ CubeBuffer]]`.`[[net.noresttherein.sugar.collections.CubeBuffer.untagged untagged]].
  *    1. Because of being a still relatively unsophisticated data structure, and because internal arrays
  *       are larger and fewer than in a `Vector`, both random and sequential access are still meaningfully faster
  *       than in the latter.
  *
  * Up until $MaxEdge elements, the buffer behaves just like an ordinary `ArrayBuffer`. When the lowest dimension array
  * is filled up, instead of allocating a larger array and copying the elements,
  * a new array of the same size is allocated, and stored after the preceding one in an array of a higher dimension.
  * If the latter does not exist, it is created, and the dimension of the buffer increases; if that array is also filled
  * up, it is either reallocated, or yet another, new dimension is added if the limit for the second dimension
  * has also been reached.
  *
  * This approach makes it a viable option for collections of up to `Int.MaxValue` elements, and a safer choice
  * than `ArrayBuffer` if the expected size is not known in advance. It is not, however,
  * the most universal implementation: it is optimized for being a fast random access `mutable.ArraySeq`
  * with good built int growing characteristic. Operations in the middle of the buffer still cost `O(n)` time.
  *
  * @define Coll    `CubeBuffer`
  * @define coll    cube buffer
  * @define MaxDim  4
  * @define MaxEdge 256
  * @author Marcin Mościcki
  */
sealed class CubeBuffer[E] private (implicit protected override val iterableEvidence :ClassTag[E])
	extends mutable.AbstractBuffer[E] with mutable.IndexedBuffer[E]
	   with mutable.IndexedSeqOps[E, CubeBuffer, CubeBuffer[E]] with StrictOptimizedSeqOps[E, CubeBuffer, CubeBuffer[E]]
	   with EvidenceIterableFactoryDefaults[E, CubeBuffer, ClassTag]
	   with SugaredIterable[E] with SugaredIterableOps[E, CubeBuffer, CubeBuffer[E]]
{
	/** A type 'variable' used to refer to the current type of `this.data`. Method `cube` casts `data` to this type,
	  * which in turn is the type parameter of our type class
	  * [[net.noresttherein.sugar.collections.CubeBuffer.cubeDimension cubeDimension]].
	  */
	protected type MyCube[X] <: Cube[X]

	/** An N-dimensional array (`N=`[[net.noresttherein.sugar.collections.CubeBuffer.dim dim]]) of `E`. Its length
	  * is always between [[net.noresttherein.sugar.collections.sugar.CubeBuffer.MinCapacity MinCapacity]]
	  * and [[net.noresttherein.sugar.collections.sugar.CubeBuffer.MaxEdge MaxEdge]] (exclusive).
	  * May be `null` if `dim == 0`. All array of levels below `dim` (that is, of dimensions `[1, dim-1]`)
	  * are always of full length: a three dimensional `CubeBuffer` allocates all of its new `Array[Array[E]]`
	  * at `MaxEdge` to start with. However, the actual data arrays `Array[E]` are allocated on a one-by-one basis,
	  * rather than using an exponential scheme like `ArrayBuffer`. The length of all arrays is always a power of two;
	  * for a buffer of the first dimension, it is also a multiplicity
	  * of [[net.noresttherein.sugar.collections.sugar.CubeBuffer.InitialCapacity InitialCapacity]],
	  * while the capacity of higher dimensions is always divisible by `MaxEdge`.
	  *
	  * When speaking about indexing the array, we normally mean absolute indexing of individual elements
	  * in the complete address space of the maximum `data` capacity of `MaxEdge`^`dim`^. When speaking about indices
	  * in particular arrays, we speak of ''projections'' of the full index to a particular dimension,
	  * that is the remainder of `index % MaxEdge`^`(n - 1)`^ for an `n`-dimensional array.
	  * All arrays are referred to as `cubes`, especially in the context of continuous addressing,
	  * and their lengths are also sometimes called ''edges'' to avoid confusion.
	  *
	  * Fragments in this virtual address space can be in several states:
	  *   1. ''used''/''occupied'' - ''allocated'' and contain elements of this collection.
	  *   1. ''allocated'' - all indices map to allocated `Array[E]`, and can be assigned values.
	  *      Not used, allocated space is ''free'' or ''unused''.
	  *   1. ''initialized'' and ''unallocated'': all intermediate arrays of dimensions 1+ are allocated, and,
	  *      with the potential exception of the array of the highest dimension, have maximum length `MaxEdge`.
	  *      Initialized, but not allocated space does not map to allocated data arrays (of dimension 1).
	  *   1. ''uninitialized'' - within the maximum capacity of the cube, equal to `data.length * MaxEdge`^`dim`^,
	  *      but `data(project(index, dim))` is `null`.
	  *
	  * Using multidimensional, partially allocated arrays introduces a possibility to potentially address
	  * more than `Int.MaxValue` data cells. We take advantage of that fact to avoid juggling data just to fit
	  * all indices in range: using the buffer as a LIFO, in either direction, can result in data residing
	  * past the 31 bit limit. We still cannot contain more than `Int.MaxValue` elements because `Iterable.size`
	  * is an `Int`, and we will therefore try to also keep `(storageEnd - storageOffset)` within 31 bits,
	  * but we will not break the rule of cubes of dimensions `[2, dim)` having maximum edge length.
	  */
	private[this] var data  :Cube[E] = _
	/** The number of nested arrays in `data`, where `Array[E]` is the most nested, single dimension. Zero indicates
	  * `dim == null` and all other properties being zero.
	  */
	private[this] var dim   :Int = 0          //The number of nested arrays in data; Array[E] - single dimension.
	/** The number of elements currently stored in the buffer - the value of this buffer's `size`, no surprises. */
	private[this] var _size :Int = 0
	/** The absolute offset of the first allocated slot in this buffer - prepending `dataOffset - storageOffset`
	  * will not require memory allocation (or moving unused cubes from the back of the buffer).
	  * For dimensions `0` and `1`, it is always `0`. For higher dimensions, it is always divisible by `MaxEdge`.
	  */
	private[this] var storageOffset :Long = 0
	/** The absolute offset of the first actual element in this buffer, that is one returned by `this.head`.
	  * Takes values in the `[storageOffset, storageEnd - size)` range.
	  */
	private[this] var dataOffset    :Long = 0
	/** The absolute offset immediately following the last allocated slot in the buffer. For the first dimension,
	  * it is always equal to `data.length`. For higher dimensions, it is always divisible by `MaxEdge`.
	  */
	private[this] var storageEnd    :Long = 0
	private[this] var mutationCount :Int = 0

	@inline private def cube  :MyCube[E] = data.asInstanceOf[MyCube[E]]
	@inline private def data1 :Cube1[E]  = data.asInstanceOf[Cube1[E]]
	@inline private def data2 :Cube2[E]  = data.asInstanceOf[Cube2[E]]
	@inline private def data3 :Cube3[E]  = data.asInstanceOf[Cube3[E]]
	@inline private def data4 :Cube4[E]  = data.asInstanceOf[Cube4[E]]
	@inline private def data5 :Cube5[E]  = data.asInstanceOf[Cube5[E]]

	/** The type class for the cube of this dimension, handling addressing and memory management. */
	@inline private implicit def cubeDimension :CubeDimension[MyCube] = (dim : @switch) match {
		case 1 => Cube1Dimension.asInstanceOf[CubeDimension[MyCube]]
		case 2 => Cube2Dimension.asInstanceOf[CubeDimension[MyCube]]
		case 3 => Cube3Dimension.asInstanceOf[CubeDimension[MyCube]]
		case 4 => Cube4Dimension.asInstanceOf[CubeDimension[MyCube]]
		case 5 => Cube5Dimension.asInstanceOf[CubeDimension[MyCube]]
		case _ => !!!("A CubeBuffer of an unexpected dimension " + dim)
	}

	/** Grants direct access to the underlying array for trusted classes, to speed up access.
	  * The caller ''must'' verify that `this.dimension == 1` before using it. It is an `Array[_]` for the same reason
	  * as in `ArraySeq` `CubeBuffer[Int]` may use `Array[Int]`, `Array[Integer]`, or `Array[AnyRef]` -
	  * and not to hide multiple dimensions.
	  */
	private[collections] final def unsafeArray :Array[_] = data
	/** The number of dimensions in `unsafeArray`. Do not use the latter if `dimension != 1` as information required
	  * for it is not exposed. */
	private[collections] final def dimension :Int = dim
	/** If `this.dimension == 1`, then unsafeArray */
	private[collections] final def startIndex :Int = dataOffset.toInt

	final override def length    :Int = _size
	final override def knownSize :Int = _size

	final override def iterator :Iterator[E] = dim match {
		case 0 => Iterator.empty
		case 1 => ArrayIterator(data1)
		case _ => new CubeBufferIterator(cube, dim, dataOffset, dataOffset + size, mutationCount)
	}
	final override def reverseIterator :Iterator[E] = ReverseIndexedSeqIterator(this)
//	final override def iterator :Iterator[E] = cubeDimension.iterator(cube, dataOffset, dataOffset + _size)

	final override def apply(i :Int) :E = {
		if (i < 0 | i >= _size)
			throw new IndexOutOfBoundsException(i.toString + " out of " + _size)
		val index = i + dataOffset
		(dim : @switch) match { //manually inlined
//			case 0 => throw new IndexOutOfBoundsException(i + " out of 0")
			case 1 => Cube1Ops(data1).get(index)
			case 2 => Cube2Ops(data2).get(index)
			case 3 => Cube3Ops(data3).get(index)
			case 4 => Cube4Ops(data4).get(index)
			case _ => Cube5Ops(data5).get(index)
		}
	}
	final override def update(idx :Int, elem :E) :Unit = {
		if (idx < 0 | idx > _size)
			throw new IndexOutOfBoundsException(idx.toString + " out of " + _size)
		val index = idx + dataOffset
		mutationCount += 1
		(dim : @switch) match { //manually inlined
			case 1 => Cube1Ops(data1).set(index, elem)
			case 2 => Cube2Ops(data2).set(index, elem)
			case 3 => Cube3Ops(data3).set(index, elem)
			case 4 => Cube4Ops(data4).set(index, elem)
			case 5 => Cube5Ops(data5).set(index, elem)
		}
	}


	final override def remove(idx :Int) :E = {
		val elem = apply(idx)
		remove(idx, 1)
		elem
	}

	final override def remove(idx :Int, count :Int) :Unit = {
		if (count < 0)
			throw new IllegalArgumentException("CubeBuffer<" + _size + ">.remove(" + idx + ", " + count + ")")
		if (idx < 0 | idx > _size - count)
			throw new IndexOutOfBoundsException("CubeBuffer<" + _size + ">.remove(" + idx + ", " + count + ")")
		if (idx == 0) {
			cubeDimension.erase(cube, dataOffset, dataOffset + count)
			dataOffset += count
		} else if (idx + count == _size) {
			cubeDimension.erase(cube, _size - count, _size)
		} else {
			val from = dataOffset + idx
			val until = dataOffset + idx + count
			val upperPartSize = _size - idx - count
			val watermark = dataOffset + _size
			if (idx > upperPartSize)
				cubeDimension.shiftLeftInPlace(cube, from, until, watermark)
			else {
				cubeDimension.shiftRightInPlace(cube, dataOffset, from, until)
				dataOffset += count
			}
		}
		_size -= count
		mutationCount += 1
	}


	final override def insert(idx :Int, elem :E) :Unit = genericInsert(idx, elem :generic.Self[E])(SingleItem)
/*
		if (idx < 0 | idx > _size)
			throw new IndexOutOfBoundsException(idx.toString + " out of " + _size)
		else if (idx == _size)
			addOne(elem)
		else if (idx == 0)
			prepend(elem)
		else if (dim == 1) //A fast track for one dimension; its extraction also simplifies the n-dimensional code.
			insert1(idx, elem)
		else { //We now know dim > 0, because otherwise the only valid index is 0
			val cube               = this.cube
			val edge               = data.length
			val suffixSize         = _size - idx
			val absoluteIdx        = dataOffset + idx
			val watermark          = dataOffset + _size
			val lowerDimensionBits = DimBits * (dim - 1)
			val maxCapacity        = edge.toLong << lowerDimensionBits
			if (idx < suffixSize) { //prefer shifting the prefix left
				if (storageOffset < dataOffset) { //the simplest case - just shift the prefix left
					val thisDimension = this.cubeDimension
					thisDimension.shiftLeftInPlace(cube, dataOffset - 1, dataOffset, absoluteIdx)
					dataOffset -= 1
					thisDimension.update(cube, dataOffset + idx, elem)
				} else { //dataOffset == storageOffset  =>  we need more space at the front
					if (storageOffset > 0) { //implies dim > 1
						moveCubeFromFrontToBack()
						val thisDimension = cubeDimension
						thisDimension.shiftLeftInPlace(cube, dataOffset - 1, dataOffset, absoluteIdx)
						thisDimension.update(cube, absoluteIdx - 1, elem)
						dataOffset -= 1

					//If the fill factor is large enough, and the suffix dominates the prefix,
					// just grow earlier than we would otherwise.
					} else if (_size == maxCapacity ||
					           idx < (suffixSize >> 1) & _size > (maxCapacity >> 1) + (maxCapacity >> 2)
					) { //storageOffset == 0
						growEvenly()
						val cube = this.cube //shadow the old cube val
						val thisDimension = this.cubeDimension
						thisDimension.alloc(cube, storageOffset - MaxSize1, storageOffset)
						storageOffset -= MaxSize1
						thisDimension.shiftLeftInPlace(cube, dataOffset - 1, dataOffset, dataOffset + _size)
						dataOffset -= 1
						thisDimension.update(cube, dataOffset + idx, elem)
					} else { //We have space at the end and can't grow yet, so fallback to shifting the suffix right.
						val thisDimension = this.cubeDimension
						if (watermark == storageEnd) {
							thisDimension.alloc(cube, storageEnd, storageEnd + MaxSize1)
							storageEnd += MaxSize1
						}
						thisDimension.shiftRightInPlace(cube, absoluteIdx, watermark, watermark + 1)
						thisDimension.update(cube, absoluteIdx, elem)
					}
				}
			} else { //idx >= suffixSize  =>  prefer shifting the suffix right. Symmetrical to the previous case.
				if (watermark < storageEnd) { //the simplest case - just shift the suffix right
					val thisDimension = this.cubeDimension
					thisDimension.shiftRightInPlace(cube, absoluteIdx, watermark, watermark + 1)
					thisDimension.update(cube, dataOffset + idx, elem)
				} else { //watermark == storageEnd  =>  we need more space at the back
					if (watermark < maxCapacity) {
						moveCubeFromBackToFront()
						val thisDimension   = this.cubeDimension
						thisDimension.shiftRightInPlace(cube, absoluteIdx, watermark, watermark + 1)
						thisDimension.update(cube, absoluteIdx, elem)

					//If the fill factor is large enough, and the prefix dominates the suffix,
					// just grow earlier than we would otherwise.
					} else if (_size == maxCapacity ||
					           (idx >> 1) >= suffixSize & _size > (maxCapacity >> 1) + (maxCapacity >> 2))
					{ //watermark == storageEnd
						growEvenly()
						val cube = this.cube //shadow the old cube val
						val thisDimension = this.cubeDimension
						thisDimension.alloc(cube, storageEnd, storageEnd + MaxSize1)
						storageEnd += MaxSize1
						thisDimension.shiftRightInPlace(
							cube, dataOffset + idx, dataOffset + _size, dataOffset + _size + 1L
						)
						thisDimension.update(cube, dataOffset + idx, elem)
					} else { //We have space at the front and can't grow yet, so fallback to shifting the prefix left.
						val thisDimension = this.cubeDimension
						if (storageOffset == dataOffset) {
							thisDimension.alloc(cube, storageOffset - MaxSize1, storageOffset)
							storageOffset -= MaxSize1
						}
						thisDimension.shiftLeftInPlace(cube, dataOffset - 1, dataOffset, absoluteIdx)
						dataOffset -= 1
						thisDimension.update(cube, dataOffset + idx, elem)
					}
				}
			}
			_size += 1
			mutationCount += 1
		}
*/

	final override def insertAll(idx :Int, elems :IterableOnce[E]) :Unit =
		genericInsert(idx, elems)(IterableOnceItems)

	private final def genericInsert[X[_]](idx :Int, elems :X[E])(implicit items :Items[X]) :Unit = {
		if (idx < 0 | idx > _size)
			throw new IndexOutOfBoundsException(idx.toString + " out of " + _size)
		else if (idx == _size)
			genericAppend(elems)
		else if (idx == 0)
			genericPrepend(elems)
		else if (dim == 1) //A fast track for one dimension; its extraction also simplifies the n-dimensional code.
			genericInsert1(idx, elems)
		else { //We now know dim > 0, because otherwise the only valid index is 0
			val elemsSize          = items.size(elems)
			if (elemsSize > 0)
				insertKnownSize(idx, elems, elemsSize)
			else if (elemsSize < 0) //we know elems is not a single item (we are called from insertAll, not insert)
/*
				if (idx == 1) {
					val suffixDimension = cubeDimension
					val suffixSize = _size - 1
					val suffix = cube
//					var suffixStorageOffset = storageOffset
//					var suffixDataOffset = dataOffset + 1
					if ((dataOffset + idx & DimMask1) == 0L) { //idx falls on an Array[E] boundary
						if (dataOffset - storageOffset > MaxSize1) { //can't be equal because (dataOffset + 1 & DimMask1) == 0
							val freeFrontSpace = (dataOffset & ~DimMask1) - storageOffset
							suffixDimension.move(suffix, storageOffset, suffix, storageOffset, freeFrontSpace)
							storageOffset += freeFrontSpace
							storageEnd    += freeFrontSpace
						}
						data = cubeDimension.sliceInPlace(cube, storageOffset, storageOffset + MaxSize1)
						val suffixOffset = storageOffset - MaxSize1
						val suffixStorageEnd = storageEnd - MaxSize1
						_size         = 1
						dataOffset    = MaxSize1 - 1
						storageOffset = 0
						storageEnd    = MaxSize1

						genericAppend(elems)

//						cubeDimension.
						val prefixDataFloor = dataOffset & ~DimMask1
						val prefixDataEnd   = dataOffset + _size + MaxSize1 - 1L & ~DimMask1
						val prefixData = prefixDataEnd - prefixDataFloor
						val emptyPrePrefix = prefixDataFloor - storageOffset
						val emptyPostPrefix = storageEnd - prefixDataEnd
						if (emptyPostPrefix + prefixData <= suffixOffset) {

						}
						???
					} else {
						???
					}

				} else if (idx == _size - 1)
					???
				else
*/
					insertUnknownSize(idx, elems)
		}
	}


	private def insertKnownSize[C[_]](idx :Int, elems :C[E], elemsSize :Int)(implicit items :Items[C]) :Unit = {
		val cube               = this.cube
		val edge               = data.length
		val suffixSize         = _size - idx
		val absoluteIdx        = dataOffset + idx
		val watermark          = dataOffset + _size
		val lowerDimensionBits = DimBits * (dim - 1)
		val maxCapacity        = edge.toLong << lowerDimensionBits
		if (idx < suffixSize) { //prefer shifting the prefix left
			if (storageOffset + elemsSize <= dataOffset) { //the simplest case - just shift the prefix left
				val thisDimension = this.cubeDimension
				thisDimension.shiftLeftInPlace(cube, dataOffset - elemsSize, dataOffset, absoluteIdx)
				dataOffset -= elemsSize
				items.update(cube, dataOffset + idx, elemsSize, elems)(thisDimension)
			} else { //dataOffset == storageOffset  =>  we need more space at the front
				if (storageOffset >= elemsSize) { //implies dim > 1
					allocBack(elemsSize)
					val thisDimension = cubeDimension
					thisDimension.shiftLeftInPlace(cube, dataOffset - 1, dataOffset, absoluteIdx)
					items.update(cube, absoluteIdx - 1, elemsSize, elems)(thisDimension)
					dataOffset -= elemsSize

				//If we don't have the capacity, and can't make room by shifting right,
				// or if the fill factor is large enough, and the suffix dominates the prefix,
				// just grow earlier than we would otherwise.
				} else if (elemsSize > maxCapacity - _size || storageEnd + elemsSize > maxCapacity ||
				           idx < (suffixSize >> 1) & _size > (maxCapacity >> 1) + (maxCapacity >> 2)
				) { //storageOffset == 0
					growFrontCapacity(_size + elemsSize)
					val cube          = this.cube //shadow the old cube val
					val thisDimension = this.cubeDimension
					val newOffset = dataOffset - elemsSize & ~DimMask1
					thisDimension.alloc(cube, newOffset, storageOffset)
					storageOffset = newOffset
					thisDimension.shiftLeftInPlace(cube, dataOffset - elemsSize, dataOffset, dataOffset + _size)
					dataOffset -= elemsSize
					items.update(cube, dataOffset + idx, elemsSize, elems)(thisDimension)
				} else { //We have space at the end and can't grow yet, so fallback to shifting the suffix right.
					val thisDimension = this.cubeDimension
					if (watermark + elemsSize > storageEnd) {
						val newEnd = (watermark + elemsSize + MaxSize1 - 1L) & ~DimMask1
						thisDimension.alloc(cube, storageEnd, newEnd)
						storageEnd = newEnd
					}
					thisDimension.shiftRightInPlace(cube, absoluteIdx, watermark, watermark + elemsSize)
					items.update(cube, absoluteIdx, elemsSize, elems)(thisDimension)
				}
			}
		} else { //idx >= suffixSize  =>  prefer shifting the suffix right. Symmetrical to the previous case.
			if (watermark + elemsSize <= storageEnd) { //the simplest case - just shift the suffix right
				val thisDimension = this.cubeDimension
				thisDimension.shiftRightInPlace(cube, absoluteIdx, watermark, watermark + elemsSize)
				items.update(cube, dataOffset + idx, elemsSize, elems)(thisDimension)
			} else { //watermark == storageEnd  =>  we need more space at the back
				if (watermark + elemsSize <= maxCapacity) {
					allocFront(elemsSize)
					val thisDimension = this.cubeDimension
					thisDimension.shiftRightInPlace(cube, absoluteIdx, watermark, watermark + elemsSize)
					items.update(cube, absoluteIdx, elemsSize, elems)(thisDimension)

				//If we don't have the capacity, and can't make room by shifting left,
				// or if the fill factor is large enough, and the prefix dominates the suffix,
				// just grow earlier than we would otherwise.
				} else if (_size > maxCapacity - elemsSize || storageOffset < elemsSize ||
					(idx >> 1) >= suffixSize & _size > (maxCapacity >> 1) + (maxCapacity >> 2)
				) { //watermark == storageEnd
					growBackCapacity(_size + elemsSize)
					val cube          = this.cube //shadow the old cube val
					val thisDimension = this.cubeDimension
					val newEnd = (storageEnd + elemsSize + MaxSize1 - 1L) & ~DimMask1
					thisDimension.alloc(cube, storageEnd, newEnd)
					storageEnd = newEnd
					thisDimension.shiftRightInPlace(
						cube, dataOffset + idx, dataOffset + _size, dataOffset + _size + elemsSize
					)
					items.update(cube, dataOffset + idx, elemsSize, elems)(thisDimension)
				} else { //We have space at the front and can't grow yet, so fallback to shifting the prefix left.
					val thisDimension = this.cubeDimension
					if (storageOffset + elemsSize > dataOffset) {
						val newOffset = dataOffset - elemsSize & ~DimMask1
						thisDimension.alloc(cube, newOffset, storageOffset)
						storageOffset = newOffset
					}
					thisDimension.shiftLeftInPlace(cube, dataOffset - elemsSize, dataOffset, absoluteIdx)
					dataOffset -= elemsSize
					items.update(cube, dataOffset + idx, elemsSize, elems)(thisDimension)
				}
			}
		}
		_size += elemsSize
		mutationCount += 1
	}


	private def insertUnknownSize[C[_]](idx :Int, elems :C[E])(implicit items :Items[C]) :Unit = {
		//We don't know how many new elements there are, so we need to save the suffix on the side,
		// trim the buffer to idx, append elems, and then try to be smart at how we append the suffix after the elements.
//		val cube               = this.cube
		val edge               = data.length
		val suffixSize         = _size - idx
		val absoluteIdx        = dataOffset + idx
		val watermark          = dataOffset + _size
		val lowerDimensionBits = DimBits * (dim - 1)
		val maxCapacity        = edge.toLong << lowerDimensionBits
		val lowerCubeSize      = 1L << lowerDimensionBits
		val lowerCubeMask      = lowerCubeSize - 1L
		var prefixCubeSize     = lowerCubeSize
		var prefixDim          = dim - 1
//		var prefix :Array[_]   = data
		var suffixCubeSize     = lowerCubeSize
		var suffixDim          = dim - 1
		//If the prefix with all its padding to cube borders fits in a single cube of some dimension,
		// we can 'become' that cube.
		while (prefixCubeSize > idx + (dataOffset & prefixCubeSize - 1)) {
//			prefix = prefix((absoluteIdx >> DimBits * prefixDim).toInt & DimMask).asInstanceOf[Array[_]]
			prefixDim -= 1
			prefixCubeSize >>= DimBits
		}
		prefixDim += 1
		prefixCubeSize <<= DimBits
		val PrefixCubeMask   = prefixCubeSize - 1L
		val prefixCubeOffset = dataOffset & ~PrefixCubeMask

		//Try to find the cube dimension such that the suffix, together with whatever padding it has
		// to that cube size, will fit in at most three cubes, so we don't need to allocate a new array for it.
		//cubeSize - 1 is a mask of one bits for that dimension's index
		while (suffixCubeSize * 3 > watermark - (dataOffset + idx & suffixCubeSize - 1)) {
			suffixDim -= 1
			suffixCubeSize >>= DimBits
		}
		suffixCubeSize <<= DimBits
		suffixDim += 1
		val suffixCubeOffset = dataOffset & ~(suffixCubeSize - 1L)

		var alignedSuffixStart  = absoluteIdx & ~DimMask1
		val alignedPrefixEnd    = absoluteIdx + MaxSize1 - 1L & ~DimMask1
		//If we can fit the whole prefix, without shifting, in a single cube of a lower dimension than this one,
		// and not greater than the cube size required to fit the suffix, then extract the prefix as a stand alone
		// cube, and save the remainder of this cube with the suffix on the side. Then, 'become' the prefix,
		// append elems, and join the cubes together.
		if (prefixDim < suffixDim || prefixDim == suffixDim && prefixDim < dim) {
			val suffixAlignment     = alignedSuffixStart & lowerCubeMask
//			val emptyFrontSpace  = prefixCubeOffset - storageOffset //empty, allocated slots preceding the cube with the prefix
			val suffixDimension     = cubeDimension
			var suffix              = cube
			val suffixDim           = dim
			var suffixEnd           = storageEnd
			var suffixOffset        = storageOffset
			val alignedSuffixLength = suffixEnd - alignedSuffixStart

			//set all properties to be consistent with containing the prefix cube only
			if (storageOffset >= prefixCubeOffset) {
				this.data = suffixDimension.sliceInPlace(suffix, storageOffset, alignedPrefixEnd, 0, true)
				storageOffset &= PrefixCubeMask
			} else {
				this.data = suffixDimension.sliceInPlace(suffix, prefixCubeOffset, alignedPrefixEnd, 0, true)
				storageOffset = 0L
			}
			dataOffset &= PrefixCubeMask
			storageEnd = prefixCubeSize
			_size = idx

			genericAppend(elems) //now append the elements to the prefix

			if (_size == idx) { //all this work, and we appended nothing?
				var d = dim
				var array :Array[_] = suffix
				while (d - 1 > prefixDim) {
					array = suffix(project(absoluteIdx - 1L, d)).asInstanceOf[Array[_]]
					d -= 1
				}
				val prefixCubeIdx = project(absoluteIdx - 1L, d)
				val prefixCube    = cube
				if (alignedPrefixEnd < prefixCubeOffset + prefixCubeSize) {
					//We've got the initial suffix elements in the cube left after extracting the prefix,
					// as well as possibly full data arrays in suffix preceding prefixCubeOffset + prefixCubeSize
					val prefixDimension        = cubeDimension
					val remainderAfterPrefix   = array(prefixCubeIdx).asInstanceOf[MyCube[E]]
					val remainderOffset        = remainder(absoluteIdx, d - 1)
					val alignedRemainderOffset = remainder(absoluteIdx + MaxSize1 - 1L, d - 1)
					val alignedRemainderLength = prefixCubeOffset + prefixCubeSize - alignedRemainderOffset
					val prefixBackPadding      = alignedPrefixEnd - absoluteIdx
					prefixDimension.copy(
						remainderAfterPrefix, remainderOffset, prefixCube, dataOffset + idx, prefixBackPadding
					)
					prefixDimension.move(
						remainderAfterPrefix, alignedRemainderOffset, prefixCube, alignedPrefixEnd, alignedRemainderLength
					)
				}
				array.asInstanceOf[Array[Array[_]]](prefixCubeIdx) = prefixCube
				data          = suffix
				dim           = suffixDim
				storageOffset = suffixOffset
				storageEnd    = suffixEnd
				_size         = _size + suffixSize
			} else if (dim < suffixDim) { //cheaper to move the prefix back into the suffix than vice versa
				val cubeDimension        = this.cubeDimension
				val cube                 = this.cube
//				val alignedPrefixEnd = dataOffset + _size + MaxSize1 - 1L & ~DimMask1
				val allocatedPrefixSpace = storageEnd - storageOffset
				val freeFrontSuffixSpace = math.max(0, prefixCubeOffset - suffixOffset)
				val freeFrontPrefixSpace = dataOffset & ~DimMask1 - storageOffset
//				val freeBackPrefixSpace  = storageEnd - (dataOffset + _size + MaxSize1 - 1L & ~DimMask1)
				//We don't allocate data arrays unless we need, so storageEnd is the ceiling of dataOffset + _size.
				val alignedSize          = allocatedPrefixSpace - freeFrontPrefixSpace// - freeBackPrefixSpace
				val maxSuffixCapacity    = suffix.length << lowerDimensionBits
				val maxSuffixDimCapacity = lowerCubeSize << DimBits
				val totalAllocatedSpace  = allocatedPrefixSpace + alignedSuffixLength + freeFrontSuffixSpace
				val newPrefixSpace       =
					if ((allocatedPrefixSpace & lowerCubeMask) < suffixAlignment)
						allocatedPrefixSpace & ~lowerCubeMask + suffixAlignment
					else
						(allocatedPrefixSpace & lowerCubeMask) + lowerCubeSize + suffixAlignment
				val requiredSpace        = newPrefixSpace + alignedSuffixLength
				if (requiredSpace <= maxSuffixDimCapacity) {
					if (requiredSpace > maxSuffixCapacity) {
						val minEdge           = (requiredSpace + lowerCubeSize - 1L >> lowerDimensionBits).toInt
						val edge              = newCapacity(minEdge)
						val array             = suffixDimension.make[E](edge)
						val capacity          = edge << lowerDimensionBits
						val newSuffixOffset   = newPrefixSpace + (capacity - alignedSuffixLength >> 1 & ~lowerCubeMask)
						suffixDimension.move(suffix, alignedSuffixStart, array, newSuffixOffset, alignedSuffixLength)
						suffixDimension.reuse(
							suffix, suffixOffset, array, newSuffixOffset - allocatedPrefixSpace, freeFrontSuffixSpace
						)
						alignedSuffixStart = newSuffixOffset
						suffixOffset       = newSuffixOffset - allocatedPrefixSpace
						suffixEnd          = newSuffixOffset + alignedSuffixLength
						suffix = array
					} else if (alignedSuffixStart < alignedSize) {
						val newSuffixOffset = newPrefixSpace + (maxSuffixCapacity - alignedSuffixLength >> 1 & ~lowerCubeMask)
						suffixDimension.shiftRightInPlace(
							suffix, alignedSuffixStart, suffixEnd, newPrefixSpace + alignedSuffixLength
						)
						suffixDimension.reuse(
							suffix, suffixOffset, suffix, newSuffixOffset - allocatedPrefixSpace, freeFrontSuffixSpace
						)
						alignedSuffixStart = newSuffixOffset
					}

					val currentStorageOffset = storageOffset
					if (freeFrontSuffixSpace + allocatedPrefixSpace < alignedSuffixStart) {
						storageOffset = alignedSuffixStart - freeFrontSuffixSpace - allocatedPrefixSpace
						storageEnd    = suffixEnd
						//shift right empty front space left in the suffix
						suffixDimension.reuse(suffix, suffixOffset, suffix, storageOffset, freeFrontSuffixSpace)
					} else if (allocatedPrefixSpace <= alignedSuffixStart) {
						val frontPart = alignedSuffixStart - allocatedPrefixSpace
						storageOffset = 0L
						storageEnd = suffixEnd + freeFrontSuffixSpace - frontPart
						suffixDimension.reuse(suffix, suffixOffset, suffix, 0L, frontPart)
						suffixDimension.reuse(
							suffix, suffixOffset + frontPart, suffix, suffixEnd, storageEnd - freeFrontSuffixSpace + frontPart
						)
					} else {
						val backPrefixPart = alignedSuffixStart - allocatedPrefixSpace
						storageOffset = 0L
						storageEnd    = suffixEnd + backPrefixPart + freeFrontSuffixSpace
						suffixDimension.reuse(suffix, suffixOffset, suffix, suffixEnd + backPrefixPart, freeFrontSuffixSpace)
					}

					cubeDimension.moveTo(
						cube, dataOffset & ~DimMask1, suffix, alignedSuffixStart - alignedSize, alignedSize
					)(suffixDimension)

					if (allocatedPrefixSpace <= alignedSuffixStart)
						cubeDimension.reuseIn(
							cube, currentStorageOffset, suffix, alignedSuffixStart - allocatedPrefixSpace, allocatedPrefixSpace
						)(suffixDimension)
					else {
						val backPrefixPart = allocatedPrefixSpace - alignedSuffixStart
						cubeDimension.reuseIn(cube, currentStorageOffset, suffix, suffixEnd, backPrefixPart)(
							suffixDimension
						)
						cubeDimension.reuseIn(
							cube, currentStorageOffset + backPrefixPart, suffix, 0L, freeFrontPrefixSpace - backPrefixPart
						)(suffixDimension)
					}

					val gap = alignedSize - _size + absoluteIdx - (absoluteIdx & ~DimMask1)
					if (suffixSize < size) {
						suffixDimension.shiftLeftInPlace(
							suffix, alignedSuffixStart - gap, alignedSuffixStart, alignedSuffixStart + suffixSize
						)
						dataOffset = alignedSuffixStart - gap - _size
					} else {
						suffixDimension.shiftRightInPlace(
							suffix, alignedSuffixStart - alignedSize, alignedSuffixStart - gap, alignedSuffixStart
						)
						dataOffset = alignedSuffixStart - _size
					}
					data = suffix
					_size += suffixSize
				} else {
					//grow in dimension
				}
			} else { //dim >= suffixDim

			}
		} else if (suffixDim < dim) {
			//We can fit the whole suffix, without shifting, in at most three cubes of a dimension lower than this one.
			// This allows us to save suffix on the side without having to create a new Array for this purpose.
			val prefixDimension = cubeDimension
			val suffixEnd = dataOffset + _size
			val alignedSuffixEnd = suffixEnd + MaxSize1 - 1L & ~DimMask1
//			val freeBackSuffixSpace = storageEnd - alignedSuffixEnd
			val suffixCubeEnd = math.min(suffixCubeOffset + suffixCubeSize, storageEnd)
			val suffix =
				if (suffixCubeEnd >= storageEnd) {
					val res = prefixDimension.sliceInPlace(cube, alignedSuffixStart, storageEnd)
					storageEnd = suffixCubeOffset
					res
				} else {
					val res = prefixDimension.sliceInPlace(cube, alignedSuffixStart, suffixCubeEnd)
					prefixDimension.reuse(cube, suffixCubeEnd, cube, alignedPrefixEnd, storageEnd - suffixCubeEnd)
					storageEnd -= suffixCubeEnd - alignedPrefixEnd
					res
				}
			_size = idx

			genericAppend(elems)
		} else { //prefixDim == suffixDim == dim
			//Lets split the prefix and suffix into separate cubes.
			val suffix = cubeDimension.sliceInPlace(cube, absoluteIdx, storageEnd, 0, false)
			storageEnd = alignedPrefixEnd
			_size = idx
		}

		genericAppend(elems)


		???
	}


	private def genericInsert1[C[_]](idx :Int, elems :C[E])(implicit items :Items[C]) :Unit = ???

	private def insert1(idx :Int, elem :E) :this.type = {
		val watermark = dataOffset + _size
		val suffixSize = _size - idx
		val shiftDirection =
			if (dataOffset > 0)
				if (watermark < storageEnd)
					if (idx < suffixSize) -1 else 1
				else
					-1
			else if (watermark < storageEnd)
				1
			else 0
		shiftDirection match {
			case -1 => //shift the prefix left
				val offset = dataOffset.toInt
				arraycopy(data, offset, data, offset - 1, idx)
				data.asInstanceOf[Array[E]](offset - 1 + idx) = elem
				dataOffset -= 1
			case 1 => //shift the suffix right
				val offset = dataOffset.toInt + idx
				arraycopy(data, offset, data, offset + 1, suffixSize)
				data.asInstanceOf[Array[E]](offset) = elem
				storageEnd += 1
			case _ if storageEnd < MaxSize1 => //the array is full, we need to grow
				val edge = data.length
				val grown = new Array[E](edge << 1)
				val shift = edge >> 1
				arraycopy(data, 0, grown, shift, idx)
				grown(shift + idx) = elem
				arraycopy(data, idx, grown, shift + idx + 1, suffixSize)
				dataOffset = shift
				storageEnd += shift + 1
				data = grown
			case _ if idx < suffixSize => //grow to the second dimension, shift left
				val cube2 = new Cube2[E](InitialCapacity)
				cube2(0) = new Cube1[E](MaxEdge)
				cube2(1) = data1
				dim = 2
				dataOffset = MaxSize1 - 1
				storageEnd = MaxSize1 << 1
				Cube2Dimension.shiftLeftInPlace(cube2, dataOffset, MaxSize1, storageEnd)
				data1(idx - 1) = elem
				data = cube
			case _ => //grow to the second dimension, shift right
				val cube2 = new Cube2[E](InitialCapacity)
				cube2(1) = data1
				cube2(2) = new Cube1[E](MaxEdge)
				dim = 2
				storageOffset = MaxSize1
				dataOffset = MaxSize1
				storageEnd = MaxSize1 * 3
				val idx2 = dataOffset + idx
				Cube2Dimension.shiftRightInPlace(cube2, idx2, MaxSize1 << 1, (MaxSize1 << 1) + 1)
				data1(idx) = elem
				data = cube2
		}
		_size += 1
		mutationCount += 1
		this
	}


	private def genericPrepend[C[_]](elems :C[E])(implicit items :Items[C]) :this.type = ???

	final override def prepend(elem :E) :this.type = (dim : @switch) match {
		case 1 => prepend1(elem)
		case 0 =>
			val cube = new Cube1[E](InitialCapacity)
			cube(InitialCapacity - 1) = elem
			data = cube
			_size = 1
			dataOffset = InitialCapacity - 1
			storageEnd = InitialCapacity
			mutationCount += 1
			this
		case _ =>
			if (dataOffset == storageOffset) {
				if (storageOffset > 0) {
					if (dataOffset + _size <= storageEnd - MaxSize1)
						moveCubeFromBackToFront()
					else {
						cubeDimension.alloc(cube, storageOffset - MaxSize1, storageOffset)
						storageOffset -= MaxSize1
					}
				} else { //storageOffset == dataOffset == 0
					val edge               = data.length
					val lowerDimensionBits = DimBits * (dim - 1)
					val maxCapacity        = edge.toLong << lowerDimensionBits
					if (storageEnd > (maxCapacity >> 1) + (maxCapacity >> 2)) {
						growFront()
						cubeDimension.alloc(cube, storageOffset - MaxSize1, storageOffset)
						storageOffset -= MaxSize1
					} else {
						val free  = maxCapacity - storageEnd
						val shift = (free >> 1) + (free >> 2)
						cubeDimension.shiftRightInPlace(cube, 0, _size, _size + shift)
						dataOffset += shift
					}
				}
			}
			dataOffset -= 1
			cubeDimension.update(cube, dataOffset, elem)
			_size += 1
			mutationCount += 1
			this
	}

	private def prepend1(elem :E) :this.type = {
		if (dataOffset > 0) {
			dataOffset -= 1
			data1(dataOffset.toInt) = elem
		} else {
			val edge = data.length
			if (_size > (edge >> 2) + (edge >> 1)) { //the majority of the array is occupied, increase the size early
				if (edge < MaxEdge) {
					data = Array.copyOfRange(data, 0, _size, edge, edge << 1)
					dataOffset = edge - 1
					storageEnd = edge << 1
					data1(edge - 1) = elem
				} else {
					val cube2 = new Cube2[E](InitialCapacity)
					val cube1 = new Array[E](MaxEdge)
					cube1(MaxEdge - 1) = elem
					cube2(InitialCapacity - 2) = cube1
					cube2(InitialCapacity - 1) = data1
					storageEnd    = MaxSize1 << InitialCapacityLog
					dataOffset    = (MaxSize1 << InitialCapacityLog) - MaxSize1 - 1L
					storageOffset = MaxSize1 << (InitialCapacityLog - 1L)
				}
			} else { //no space at the front and we can't grow yet, so we need to shift the contents right.
				val free = edge - _size
				val shift = (free >> 1) + (free >> 2)
				arraycopy(data, 0, data, shift, _size)
//				if (data.isInstanceOf[Array[AnyRef]]) //set unoccupied slots to null
//					data.clear(0, shift - 1)
				dataOffset = shift - 1
				data1(shift - 1) = elem
			}
		}
		_size += 1
		mutationCount += 1
		this
	}


	final override def prependAll(elems :IterableOnce[E]) :this.type = ???

	private def genericAppend[C[_]](elems :C[E])(implicit items :Items[C]) :this.type = ???

	@inline final def +=(elems :ArrayLike[E]) :this.type = addAll(elems)

	final def addAll(elems :ArrayLike[E]) :this.type = ???

	final def addAll(elems :ArrayLike[E], offset :Int, length :Int) :this.type = ???

	final override def addAll(xs :IterableOnce[E]) :this.type = ???

	final override def addOne(elem :E) :this.type = (dim : @switch) match {
		case 1 => append1(elem)
		case 0 =>
			val cube = new Array[E](InitialCapacity)
			cube(0) = elem
			_size = 1
			storageEnd = InitialCapacity
			mutationCount += 1
			this
		case _ =>
			val edge               = data.length
			val lowerDimensionBits = DimBits * (dim - 1)
			val maxCapacity        = edge.toLong << lowerDimensionBits
			val watermark          = dataOffset + _size
			if (watermark == storageEnd) {
				if (storageEnd < maxCapacity) {
					if (dataOffset - storageOffset >= MaxSize1)
						moveCubeFromBackToFront()
					else
						cubeDimension.alloc(cube, storageOffset, storageOffset + MaxSize1)
				} else if (storageOffset > (maxCapacity >> 1) + (maxCapacity >> 2)) {
					growBack()
					cubeDimension.alloc(cube, storageEnd, storageEnd + MaxSize1)
					storageEnd += MaxSize1
				} else {
					val free  = dataOffset - storageOffset
					val shift = (free >> 1) + (free >> 2)
					cubeDimension.shiftLeftInPlace(cube, dataOffset - shift, dataOffset, watermark)
					dataOffset -= shift
				}
			}
			cubeDimension.update(cube, watermark, elem)
			_size += 1
			mutationCount += 1
			this
	}

	private def append1(elem :E) :this.type = {
		val watermark = (dataOffset + _size).toInt
		if (watermark < storageEnd) {
			data1(watermark) = elem
		} else {
			val edge = data.length
			if (_size > (edge >> 2) + (edge >> 1)) { //the majority of the array is occupied, increase the size early
				if (edge < MaxEdge) {
					data = Array.copyOfRange(data, dataOffset.toInt, watermark, dataOffset.toInt, edge << 1)
					storageEnd = edge << 1
					data1(watermark) = elem
				} else {
					val cube2 = new Cube2[E](InitialCapacity)
					val cube1 = new Array[E](MaxEdge)
					cube1(0) = elem
					cube2(0) = data1
					cube2(1) = cube1
					storageEnd = MaxSize1 << 1
				}
			} else { //no space at the back and we can't grow yet, so we need to shift the contents left.
				val shift = (edge - _size) >> 2
				arraycopy(data, dataOffset.toInt, data, shift, _size)
//				if (data.isInstanceOf[Array[AnyRef]])
//					data.clear(storageEnd - shift)
				dataOffset = shift
				data1(shift + _size) = elem
			}
		}
		_size += 1
		mutationCount += 1
		this
	}




	private def grow(atLeast :Long) :Unit = ???

	private def growFrontCapacity(minCapacity :Long) :Unit = ???
	private def growBackCapacity(minCapacity :Long) :Unit = ???

	/** Grow the cube (possibly into a new dimension) adding roughly the same amount of potential capacity
	  * in the front and in the back. This method does not allocate any data (dimension 1) arrays.
	  */
	private def growEvenly() :Unit = {
		val edge = data.length
		val lowerDimensionBits = DimBits * (dim - 1)
		val lowerCubeSize      = 1L << lowerDimensionBits
		val offsetDelta =
			if (edge < MaxEdge) { //double the outer dimension's length, copy to the middle of the array
				val thisOuterOffset = (storageOffset >> lowerDimensionBits).toInt
				val thisOuterEnd    = (storageEnd - 1 >> lowerDimensionBits).toInt + 1
				val newOuterOffset  = thisOuterOffset + (edge >> 1) - 1
				val newCube         = Array.copyOfRange(data, thisOuterOffset, thisOuterEnd, newOuterOffset, edge << 1)
				data = newCube
				((edge >> 1) - 1).toLong << lowerDimensionBits
			} else { //grow to the next dimension, insert the current cube roughly in the middle
				dim += 1
				val nextDimension = this.cubeDimension
				val newCube       = nextDimension.make[E](InitialCapacity).asInstanceOf[Array[Any]]
				newCube((InitialCapacity >> 1) - 1) = data
				data = newCube //lowerCubeSize * MaxSize1 * (InitialCapacity - 1)
				(lowerCubeSize << (DimBits + InitialCapacityLog - 1)) - lowerCubeSize
			}
		storageOffset += offsetDelta
		dataOffset    += offsetDelta
		storageEnd    += offsetDelta
	}

	private def growFront() :Unit = {
		val edge               = data.length
		val lowerDimensionBits = DimBits * (dim - 1)
		val lowerCubeSize      = 1L << lowerDimensionBits
		if (edge < MaxEdge) {
			val thisOuterOffset = (storageOffset >> lowerDimensionBits).toInt
			val thisOuterEnd    = (storageEnd - 1 >> lowerDimensionBits).toInt + 1
			val newOuterOffset  = thisOuterOffset + edge
			data = Array.copyOfRange(data, thisOuterOffset, thisOuterEnd, newOuterOffset, edge << 1)
			val offsetDelta = edge.toLong << lowerDimensionBits
			storageOffset += offsetDelta
			dataOffset    += offsetDelta
			storageEnd    += offsetDelta
		} else {
			dim += 1
			val nextDimension = this.cubeDimension
			val newCube       = nextDimension.make[E](InitialCapacity).asInstanceOf[Array[Any]]
//			newCube(InitialCapacity - 2) = Array.of(data.getClass.getComponentType, MaxEdge)
			newCube(InitialCapacity - 1) = data
			data = newCube
			val offsetDelta = (lowerCubeSize << (DimBits + InitialCapacityLog)) - lowerCubeSize
//			storageOffset += offsetDelta - lowerCubeSize
			storageOffset += offsetDelta
			dataOffset    += offsetDelta
			storageEnd    += offsetDelta
		}
	}

	private def growBack() :Unit = {
		val edge               = data.length
		val lowerDimensionBits = DimBits * (dim - 1)
//		val lowerCubeSize      = 1L << lowerDimensionBits
		if (edge < MaxEdge) {
			val thisOuterOffset = (storageOffset >> lowerDimensionBits).toInt
			val thisOuterEnd    = (storageEnd - 1 >> lowerDimensionBits).toInt + 1
			data = Array.copyOfRange(data, thisOuterOffset, thisOuterEnd, thisOuterOffset, edge << 1)
		} else {
			dim += 1
			val nextDimension = this.cubeDimension
			val newCube       = nextDimension.make[E](InitialCapacity).asInstanceOf[Array[Any]]
			newCube(0) = data
//			newCube(1) = Array.of(data.getClass.getComponentType, MaxEdge)
			data = newCube
//			storageEnd += lowerCubeSize
		}
	}


	private def initialize(capacity :Int) :Unit = {
		if (capacity < MinCapacity | capacity > MaxEdge || capacity.isPowerOf2)
			throw new IllegalArgumentException("Cannot allocate a single dimensional array of capacity " + capacity)
		data = new Array[E](capacity)
		storageEnd = capacity
		dim = 1
	}


	private def allocFront(extra :Int, mayMove :Boolean = true) :Unit = ???
	private def allocBack(extra :Int, mayMove :Boolean = true) :Unit = ???

	/** Moves an allocated, fully unoccupied cube from the back to the front of the buffer. The moved cube's dimension
	  * depends on the alignment of both the front and the back of the buffer's data.
	  */
	private def moveCubeFromFrontToBack() :Unit = {
		val watermark          = dataOffset + _size
		val lowerDimensionBits = DimBits * (dim - 1)
		val thisDimension      = this.cubeDimension
		val freeSuffixSpace    = storageEnd - watermark
		if (freeSuffixSpace < MaxSize1) { //just allocate another dimension 1 array at the front
			thisDimension.alloc(cube, storageOffset - MaxSize1, storageOffset)
			storageOffset -= MaxSize1
		} else { //no need to allocate, we can move an array from the back to the front
			//allocated (used and non-used) space in the last cube
			val allocatedBackSpace    = (storageEnd - 1) & ~(DimMask << lowerDimensionBits) + 1
			//the number of not allocated cells in the last cube with unallocated cells at the front
			val unallocatedFrontSpace = (storageOffset - 1 & ~(DimMask << lowerDimensionBits)) + 1
			//We want to move a full, free cube, ideally of a size which won't require an allocation
			// of intermediate arrays. For example, if unallocatedFrontSpace = lowerCubeSize,
			// then moving anything smaller than lowerCubeSize will require a new Dim-1 array.
			var movedSpace            = MaxSize2
			while ( //movedSpace has a single 1 bit, so movedSpace - 1 is a mask for all lower bits
				movedSpace < freeSuffixSpace && (unallocatedFrontSpace & movedSpace - 1L) == 0L &&
					(allocatedBackSpace & movedSpace - 1L) == 0L
			)
				movedSpace <<= DimBits
			movedSpace >>= DimBits
			thisDimension.move(cube, storageEnd - movedSpace, cube, storageOffset - movedSpace, movedSpace)
			storageOffset -= movedSpace
			storageEnd    -= movedSpace
		}
	}

	/** Moves an allocated, fully unoccupied cube from the front to the back of the buffer. The moved cube's dimension
	  * depends on the alignment of both the front and the back of the buffer's data.
	  */
	private def moveCubeFromBackToFront() :Unit = {
		val thisDimension      = this.cubeDimension
		val lowerDimensionBits = DimBits * (dim - 1)
		val freePrefixSpace    = dataOffset - storageOffset
		if (freePrefixSpace < MaxSize1) { //just allocate another dimension 1 array at the back
			thisDimension.alloc(cube, storageEnd, storageEnd + MaxSize1)
			storageEnd += MaxSize1
		} else { //no need to allocate, we can move an array from the front to the back
			//allocated slots in the first cube with unallocated cells at the back
			val allocatedBackSpace    = storageEnd & ~(DimMask << lowerDimensionBits)
			//the number of not allocated cells in the last cube with unallocated cells at the front
			val unallocatedFrontSpace = storageOffset & ~(DimMask << lowerDimensionBits)
			//We want to move a full, free cube, ideally of a size which won't require an allocation
			// of intermediate arrays. For example, if allocatedBackSpace = 0,
			// then moving anything smaller than lowerCubeSize will require a new Dim-1 array.
			var movedSpace            = MaxSize2
			while ( //movedSpace has a single 1 bit, so movedSpace - 1 is a mask for all lower bits
				movedSpace < freePrefixSpace && (allocatedBackSpace & movedSpace - 1L) == 0L &&
					(unallocatedFrontSpace & movedSpace - 1L) == 0L
			)
				movedSpace <<= DimBits
			movedSpace >>= DimBits
			thisDimension.move(cube, storageOffset, cube, storageEnd, movedSpace)
			storageOffset += movedSpace
			storageEnd += movedSpace
		}
	}


	override def clear() :Unit = {
		dim = 0
		data = null :Array[E]
		_size = 0
		dataOffset = 0
		storageOffset = 0
		storageEnd = 0
		mutationCount += 1
	}


	override def foreach[U](f :E => U) :Unit = {
		def foreach(dim :Int, array :Array[_]) :Unit = dim match {
			case 1 => array.asInstanceOf[Array[E]].foreach(f)
			case _ => array.asInstanceOf[Array[Array[_]]].foreach(foreach(dim - 1, _))
		}
		if (dim > 0)
			foreach(dim, data)
	}

	override def iterableFactory :SeqFactory[CubeBuffer] = CubeBuffer.untagged
	protected override def evidenceIterableFactory :EvidenceIterableFactory[CubeBuffer, ClassTag] = CubeBuffer
}




/** $factoryInfo
  * @define Coll `CubeBuffer`
  * @define coll cube buffer
  * @define MaxDim  4
  * @define MaxEdge 256
  */
object CubeBuffer extends StrictOptimizedClassTagSeqFactory[CubeBuffer] {
	//Using three dimensions instead of four would reduce the number of addressed arrays at each access,
	//offer better locality and simpler implementation while still using small, 2096 arrays, and allowing to 'waste'
	//75% of maximum capacity within 32 bit indexing, which could help optimize deque-like usage.
	private final val DimBits  = 8                      //Number of bits addressing each dimension
	private final val DimMask  = (1 << DimBits + 1) - 1 //DimBits one bits
	private final val MaxEdge  = 1 << DimBits           //Maximum array size
	private final val MaxSize1 = 1L << DimBits
	private final val MaxSize2 = MaxSize1 * MaxSize1
	private final val MaxSize3 = MaxSize1 * MaxSize1 * MaxSize1
	private final val MaxSize4 = MaxSize1 * MaxSize1 * MaxSize1 * MaxSize1
	private final val MaxSize5 = MaxSize1 * MaxSize1 * MaxSize1 * MaxSize1 * MaxSize1
	private final val Dim2Bits = DimBits * 2
	private final val Dim3Bits = DimBits * 3
	private final val Dim4Bits = DimBits * 4
	private final val Dim5Bits = DimBits * 5
	private final val DimMask1 = (1L << DimBits + 1) - 1L
	private final val DimMask2 = DimMask1 << DimBits
	private final val DimMask3 = DimMask2 << DimBits
	private final val DimMask4 = DimMask3 << DimBits
	private final val DimMask5 = DimMask4 << DimBits
	private final val InitialCapacityLog = 5
	private final val InitialCapacity = 1 << InitialCapacityLog //All highest dimension arrays start with this capacity
	//Calling sizeHint on `newBuilder` allows the first dimiension to be allocated at this capacity as a minimum.
	// In several places we assume that (storageEnd-storageFront)/4 > 0.
	// Picking 8 will allow us to introduce a fill threshold of 7/8 instead of 3/4, which should be enough.
	private final val MinCapacity = 8

	@inline private final def newCapacity(min :Int) :Int =
		if (min > MaxEdge)
			throw new IllegalArgumentException("Cannot create an array greater than " + MaxEdge + ": " + min + ".")
		else if (min == MaxEdge)
			MaxEdge
		else {
			var capacity = InitialCapacity
			while (capacity < min)
				capacity <<= 1
			capacity
		}


	@inline def side1(index :Long) :Int = index.toInt & DimMask
	@inline def side2(index :Long) :Int = (index.toInt >> DimBits) & DimMask
	@inline def side3(index :Long) :Int = (index >> (DimBits << 1)).toInt & DimMask
	@inline def side4(index :Long) :Int = (index >> (DimBits * 3)).toInt & DimMask
	@inline def side5(index :Long) :Int = (index >> (DimBits << 2)).toInt & DimMask

	@inline private def project(index :Long, dim :Int) :Int = (index >> DimBits * (dim - 1)).toInt & DimMask
	@inline private def project5(index :Long) :Int = (index >> Dim4Bits).toInt & DimMask
	@inline private def project4(index :Long) :Int = (index >> Dim3Bits).toInt & DimMask
	@inline private def project3(index :Long) :Int = (index >> Dim2Bits).toInt & DimMask
	@inline private def project2(index :Long) :Int = (index >> DimBits).toInt & DimMask
	@inline private def project1(index :Long) :Int = index.toInt & DimMask
	@inline private def remainder(index :Long, dim :Int) :Long = index & 1L << (DimBits * dim) - 1L
	@inline private def remainder4(index :Long) :Long = index & Dim4Bits
	@inline private def remainder3(index :Long) :Long = index & Dim3Bits
	@inline private def remainder2(index :Long) :Long = index & Dim2Bits
	@inline private def remainder1(index :Long) :Long = index & DimBits

	override def from[E :ClassTag](it :IterableOnce[E]) :CubeBuffer[E] = new CubeBuffer[E] ++= it

	override def empty[A :ClassTag] :CubeBuffer[A] = new CubeBuffer[A]

	override def newBuilder[A :ClassTag] :Builder[A, CubeBuffer[A]] =
		new CubeBuffer[A] with Builder[A, CubeBuffer[A]] {
			override def result() = this
			override def sizeHint(size :Int) :Unit =
				if (size > 0 && dimension == 0) {
					val limit = math.min(size, MaxEdge)
					var capacity = MinCapacity
					while (capacity < limit)
						capacity <<= 1
//					initialize(capacity)
				}
		}//.initialize(???)


	/** A factory of [[net.noresttherein.sugar.collections.CubeBuffer CubeBuffers]] using `Array[AnyRef]` as the lowest
	  * dimensional arrays, always boxing value types.
	  */
	object untagged extends StrictOptimizedSeqFactory[CubeBuffer] {
		override def from[A](source :IterableOnce[A]) :CubeBuffer[A] = CubeBuffer.from(source)(ClassTag.Any.castParam[A])

		override def empty[A] :CubeBuffer[A] =
			new CubeBuffer()(ClassTag.Any.castParam[A])

		override def newBuilder[A] :Builder[A, CubeBuffer[A]] = CubeBuffer.newBuilder(ClassTag.Any.castParam[A])
	}

	private type Cube[E]  = Array[_]
	private type Cube1[E] = Array[E]
	private type Cube2[E] = Array[Array[E]]
	private type Cube3[E] = Array[Array[Array[E]]]
	private type Cube4[E] = Array[Array[Array[Array[E]]]]
	private type Cube5[E] = Array[Array[Array[Array[Array[E]]]]]

//	@inline private def Cube1[E :ClassTag](dim :Int = InitialCapacity) :Cube1[E] = new Array[E](dim)
//	@inline private def Cube2[E :ClassTag](dim :Int = InitialCapacity) :Cube2[E] = new Cube2[E](dim)
//	@inline private def Cube3[E :ClassTag](dim :Int = InitialCapacity) :Cube3[E] = new Cube3[E](dim)
//	@inline private def Cube4[E :ClassTag](dim :Int = InitialCapacity) :Cube4[E] = new Cube4[E](dim)
//	@inline private def Cube5[E :ClassTag](dim :Int = InitialCapacity) :Cube5[E] = new Cube5[E](dim)

//	@inline private def Cube[C <: Cube[_]](first :C, dim :Int = InitialCapacity)(implicit tag :ClassTag[C]) :Array[C] = {
//		val res = new Array[C](dim)
//		res(0) = first
//		res
//	}

	@inline private implicit def Cube1Ops[E](cube :Array[E]) :Cube1Ops[E] = new Cube1Ops(cube)
	@inline private implicit def Cube2Ops[E](cube :Array[Array[E]]) :Cube2Ops[E] = new Cube2Ops(cube)
	@inline private implicit def Cube3Ops[E](cube :Array[Array[Array[E]]]) :Cube3Ops[E] = new Cube3Ops(cube)
	@inline private implicit def Cube4Ops[E](cube :Array[Array[Array[Array[E]]]]) :Cube4Ops[E] = new Cube4Ops(cube)
	@inline private implicit def Cube5Ops[E](cube :Array[Array[Array[Array[Array[E]]]]]) :Cube5Ops[E] = new Cube5Ops(cube)
//	@inline private implicit def CubeOps[C[X] <: Cube[X], E](cube :C[E]) :CubeOps[C, E] = new CubeOps(cube)


	private class Cube1Ops[E](private val self :Array[E]) extends AnyVal {
		@inline def apply(index :Int) :E = self(index.toInt)
		@inline def update(index :Int, value :E) :Unit = self(index.toInt) = value
		@inline def get(index :Long) :E = self(index.toInt)
		@inline def set(index :Long, value :E) :Unit = self(index.toInt) = value

		/** Reallocate into a new array of size `dimension`, copying the contents from range
		  * `[copyFrom, copyFrom + length)` in the current array, to range `[copyTo, copyTo + length)` in the new array.
		  * Indices must be valid.
		  */
		@inline def grow(dimension :Int, copyFrom :Long = 0, copyTo :Long = 0, length :Long = self.length) :Cube1[E] = {
			val res = Array.make(self.getClass.getComponentType.castParam[E], 0)
			arraycopy(self, copyFrom.toInt, res, copyTo.toInt, length.toInt)
			res
		}

		@inline def erase() :Cube1[E] = {
			if (self.isInstanceOf[Array[AnyRef]])
				self.clear(0, self.length)
			self
		}
		@inline def erase(from :Long, until :Long) :Cube1[E] = {
			if (self.isInstanceOf[Array[AnyRef]])
				self.clear(from.toInt, until.toInt)
			self
		}

//		@inline def <<(n :Int) :Cube1[E] = {
//			arraycopy(self, n, self, 0, MaxEdge - n)
//			self
//		}
//		@inline def >>(n :Int) :Cube1[E] = {
//			arraycopy(self, 0, self, MaxEdge - n, n)
//			self
//		}
	}


	/** Bulk operations on two dimensional arrays `Cube2[E] = Array[Array[E]]`. */
	private class Cube2Ops[E](val self :Array[Array[E]]) extends AnyVal {
//		@inline def apply(index :Int) :Cube1[E] = self(index)
//		@inline def update(index :Int, value :Cube1[E]) :Unit = self(index) = value
		@inline def get(index :Long) :E = self(side2(index))(side1(index))
		@inline def set(index :Long, value :E) :Unit = self(side2(index))(side1(index)) = value
	}


	/** Bulk operations on two dimensional arrays `Cube3[E] = Array[Array[Array[E]]]`. */
	private class Cube3Ops[E](private val self :Array[Array[Array[E]]]) extends AnyVal {
//		@inline def apply(index :Int) :Cube2[E] = self(index)
//		@inline def update(index :Int, value :Cube2[E]) :Unit = self(index) = value
		@inline def get(index :Long) :E = self(side3(index))(side2(index))(side1(index))
		@inline def set(index :Long, value :E) :Unit = self(side3(index))(side2(index))(side1(index)) = value
	}

	/** Bulk operations on four dimensional arrays `Cube4[E] = Array[Array[Array[Array[E]]]]`. */
	private class Cube4Ops[E](private val self :Array[Array[Array[Array[E]]]]) extends AnyVal {
		@inline def get(index :Long) :E = self(side4(index))(side3(index))(side2(index))(side1(index))
		@inline def set(index :Long, value :E) :Unit =
			self(side4(index))(side3(index))(side2(index))(side1(index)) = value
	}

	/** Bulk operations on five dimensional arrays `Cube4[E] = Array[Array[Array[Array[E]]]]`. */
	private class Cube5Ops[E](private val self :Array[Array[Array[Array[Array[E]]]]]) extends AnyVal {
		@inline def get(index :Long) :E = self(side5(index))(side4(index))(side3(index))(side2(index))(side1(index))
		@inline def set(index :Long, value :E) :Unit =
			self(side5(index))(side4(index))(side3(index))(side2(index))(side1(index)) = value
	}

	private class CubeOps[C[_], E](private val self :C[E]) extends AnyVal {
		@inline def shiftLeftInPlace(downTo :Long, from :Long, until :Long)(implicit dim :CubeDimension[C]) :C[E] =
			dim.shiftLeftInPlace(self, downTo, from, until)

		@inline def shiftRightInPlace(from :Long, until :Long, upTo :Long)(implicit dim :CubeDimension[C]) :C[E] =
			dim.shiftRightInPlace(self, from, until, upTo)
	}



	/** A type class providing abstraction over a single collection element `E` and `IterableOnce[E]`, without
	  * wrapping the element in a singleton collection.
	  */
	private abstract class Items[X[_]] {
		def size[E](elems :X[E]) :Int
		def update[E, C[_]](cube :C[E], index :Long, size :Int, elems :X[E])(implicit dimension :CubeDimension[C]) :Unit
	}
	private implicit object SingleItem extends Items[generic.Self] {
		override def size[E](elems :E) :Int = 1
		override def update[E, C[_]](cube :C[E], index :Long, size :Int, elems :E)
		                            (implicit dimension :CubeDimension[C]) :Unit =
			dimension.update(cube, index, elems)
	}
	private implicit object IterableOnceItems extends Items[IterableOnce] {
		override def size[E](elems :IterableOnce[E]) :Int = elems.knownSize
		override def update[E, C[_]](cube :C[E], index :Long, size :Int, elems :IterableOnce[E])
		                            (implicit dimension :CubeDimension[C]) :Unit =
			dimension.update(cube, index, size, elems)
	}
	private implicit object ArrayLikeItems extends Items[ArrayLike] {
		override def size[E](elems :ArrayLike[E]) :Int = elems.length
		override def update[E, C[_]](cube :C[E], index :Long, size :Int, elems :ArrayLike[E])
		                            (implicit dimension :CubeDimension[C]) :Unit =
			dimension.update(cube, index, size, elems, 0, elems.length)
	}
	private class ArrayLikeRangeItems(from :Int, until :Int) extends Items[ArrayLike] {
		override def size[E](elems :ArrayLike[E]) :Int = until - from

		override def update[E, C[_]](cube :C[E], index :Long, size :Int, elems :ArrayLike[E])
		                            (implicit dimension :CubeDimension[C]) :Unit =
			dimension.update(cube, index, size, elems, from, until)
	}

	/** A type class providing an interface for a multidimensional array, allowing it to be treated as a continuous
	  * address space, abstracting over its actual dimension.
	  * @tparam C a type constructor wrapping its type argument in
	  *           `this.`[[net.noresttherein.sugar.collections.CubeBuffer.Dim Dim]] `Array`s.
	  */
	private abstract class CubeDimension[C[_]](
		/** The number of dimensions in the cubes of type `C[E]`, that is the number of nested arrays within the type. */
		final val Dim :Int
	) {
		/** Maximum theoretical capacity of a cube of this dimension. It is always `MaxEdge`^`Dim`^.
		  * Note that it is a `Long` value, and thus may exceed the actual limit of `Int.MaxValue`.
		  */
		final val MaxSize :Long = MaxEdge.toLong ** Dim

		/** The number of bits required to address all elements in a hypercube of this dimension. Equals
		  * [[net.noresttherein.sugar.collections.CubeBuffer.DimBits DimBits]]` * `[[net.noresttherein.sugar.collections.CubeBuffer.CubeDimension.Dim Dim]].
		  */
		final val Bits = DimBits * Dim
		private[this] val lowerBits = DimBits * (Dim - 1)

		/** The index in the top level array under which the element at absolute index `index` is located.
		  * It is the remainder of the division of `index` by the maximum capacity of a cube of `Dim-1` dimensions.
		  */
		@inline final def project(index :Long) :Int = (index >> lowerBits).toInt & DimMask

		/** Retrieves the value at the given absolute index from the cube.
		  * @param cube  the data cube.
		  * @param index a value in the range of `0..`[[net.noresttherein.sugar.collections.CubeBuffer.MaxSize MaxSize]]
		  *              representing the position of the desired value in the infix walk order of the whole cube.
		  */
		def apply[E](cube :C[E], index :Long) :E

		/** Updates (in place) the value at the given absolute index in the cube.
		  * @param cube  the updated data cube.
		  * @param index a value in the range of `0..`[[net.noresttherein.sugar.collections.CubeBuffer.MaxSize MaxSize]]
		  *              representing the position of the desired value in the infix walk order of the whole cube.
		  * @param value a new value.
		  */
		def update[E](cube :C[E], index :Long, value :E) :Unit

		@inline final def update[E](cube :C[E], index :Long, max :Int, values :IterableOnce[E]) :Int =
			values match {
				case items :Iterable[E] => update(cube, index, max, items)
				case _                  => update(cube, index, max, values.iterator)
			}
		def update[E](cube :C[E], index :Long, max :Int, values :Iterable[E]) :Int
		def update[E](cube :C[E], index :Long, max :Int, values :Iterator[E]) :Int
		def update[E](cube :C[E], index :Long, max :Int, values :ArrayLike[E], from :Int, until :Int) :Int = ??? //todo:

//		def slice[E](cube :C[E], from :Long, until :Long) :C[E]

		def copyToBuffer[E](cube :C[E], from :Long, until :Long, buffer :CubeBuffer[E]) :Unit

		/** Splits the argument cube into two parts, and returns a cube with values from the range `[from, until)`,
		  * after erasing it from the argument cube. If the index range falls exactly at the boundaries of a cube
		  * of a certain dimension, that cube is returned as a whole, setting is position in the cube of a higher
		  * dimension to `null`. Otherwise, the returned cube will be of the smallest dimension under the argument
		  * cube which contains the full range. All data from the range is included in the returned cube,
		  * and no data from outside that range is removed from the argument cube. This means that the data from
		  * cubes which contain indices both from the within, and from the outside of the range, have their contents
		  * copied recursively. If, in a certain sub-cube `C` of the given cube, `[from, until)` range is covered by
		  * cubes `c0, c1, .., cn`, and either of `c0` doesn't start exactly with index `from` (relative to cube `C`),
		  * or `cn` doesn't end exactly with index `until` (again, relative to `C`), then cubes `c0` and `cn` are sliced
		  * recursively. The method will then return an array of the smallest size which is a multiplicity
		  * of `InitialCapacity` (and a power of `2`) greater than `n`, unless `sameEdge` is `true`, in which case
		  * the array will be of the same size as in `C`. It will contain cubes `c0', c1, ..., cn'`,
		  * where `c0'` and `cn'` are prefix and suffix cubes of `c0` and `cn`, respectively, and retain
		  * both the maximum capacity (all arrays on all dimensions are of length `MaxEdge`), and alignment
		  * of cubes `c0` and `cn`. The will occupy positions depending on the argument `align`:
		  *   - if `align == -1`, cube `c0` will be at the zero index,
		  *   - if `align == 1`, cube `cn` will occupy the last position in the returned array, and
		  *   - if `align == 0`, cubes `c0..cn` occupy the same indices as they did in cube `C`.
		  *
		  * The positions occupied by cubes `c0` and `cn` in cube `C` are set to cubes `c0''` and `cn''`,
		  * complementary to the returned cubes, while positions. used by cubes `c2,...c{n-1}` are set to `null`.
		  */
		def sliceInPlace[E](cube :C[E], from :Long, until :Long, align :Int = 0, sameEdge :Boolean = false) :Array[_]

//		def split[E](cube :C[E], index :Long) :C[E] = extract(cube, )
		def extract[E](cube :C[E], from :Long, until :Long, align :Int = -1, sameEdge :Boolean = false) :C[E]

		protected def illegalAlignment_!(align :Int) :Nothing =
			throw new IllegalArgumentException(
				"Illegal alignment value " + align +
					" of the sliced cube. Must be one of -1 (to the left), 0 (the same), and 1 (to the right)."
			)

		/** Shifts in place elements `[from, until)` in `cube` down to absolute indices
		  * `[downTo, downTo + until - from)`, clearing - but not unreferencing - vacated positions.
		  */
		def shiftLeftInPlace[E](cube :C[E], downTo :Long, from :Long, until :Long) :C[E]

		/** Shifts in place elements `[from, until)` in `cube` up to absolute indices `[upTo - (until - from), upTo)`,
		  * clearing - but not unreferencing - the vacated positions.
		  */
		def shiftRightInPlace[E](cube :C[E], from :Long, until :Long, upTo :Long) :C[E]

		/** Shifts in place elements `[srcPos, srcPos + length)` in `cube` to absolute indices `[dstPos, dstPos + length)`,
		  * clearing - but not unreferencing - the vacated positions.
		  */
		@inline final def shiftInPlace[E](cube :C[E], srcPos :Long, dstPos :Long, length :Long) :C[E] =
			if (srcPos >= dstPos) shiftLeftInPlace(cube, dstPos, srcPos, length)
			else shiftRightInPlace(cube, srcPos, srcPos + length, dstPos + length)

		/** Copies in place the last `MaxSize - n` elements `E` in `left` to the first `MaxSize - n` absolute addresses,
		  * after which it copies the first `n` elements `E` from cube `right` to the last `n` indices in cube `left`.
		  * All data is copied, no array is reused, but both cubes must already be of maximum array length
		  * and fully allocated. Cube `left` is returned for the convenience of chained calls, but `right` is left
		  * unchanged.
		  */
		@inline final def shiftIntoLeft[E](n :Long, left :C[E], right :C[E]) :C[E] = {
			val elems = MaxSize - n
			copy(left, n, left, 0, elems)
			copy(right, 0, left, elems, n)
			left
		}

		/** Copies in place the first `MaxSize - n` elements `E` in `right` to the last `MaxSize - n` absolute addresses,
		  * after which it copies the last `n` elements `E` from cube `left` to the first `n` indices in cube `right`.
		  * All data is copied, no array is reused, but both cubes must already be of maximum array length
		  * and fully allocated. Cube `right` is returned for the convenience of chained calls,
		  * cube `left` is not modified.
		  */
		@inline def shiftIntoRight[E](n :Long, left :C[E], right :C[E]) :C[E] = {
			val elems = MaxSize - n
			copy(right, 0, right, elems, n)
			copy(left, n, right, 0, elems)
			right
		}

		/** Copies data from absolute index range `[srcPos, srcPos + length)` in `src` cube, to range
		  * `[dstPos, dstPos + length)` in `dst` cube. If `src` and `dst` are the same cube, this calls
		  * [[net.noresttherein.sugar.collections.CubeBuffer.CubeDimension.shiftInPlace shiftInPlace]].
		  * The source index range must be covered by allocated space, but it is not required
		  * for the destination range (`null` entries will be swapped with full arrays).
		  * If the cubes are different instances, all data is copied and no array is carried over. The destination
		  * address range must be already allocated in that case.
		  */
		def copy[E](src :C[E], srcPos :Long, dst :C[E], dstPos :Long, length :Long) :Unit

		/** Moves the elements from the range `[srcPos, srcPos + length]` of cube `src`
		  * to the range `[dstPos, dstPos + length]` of cube `dst`. The cubes are moved as a whole wherever possible.
		  * However, if `srcPos` does not equal `dstPos` modulo `this.MaxSize / MaxEdge` (the maximum size
		  * of a cube of dimension `Dim - 1`), then carried data must be realigned, which will involve copying
		  * also ranges in arrays of dimensions between `1..Dim` (exclusive). Both indices must be divisible by `MaxEdge`
		  * (the method cannot move less than full single dimensional array and will throw an exception in that case).
		  */
		def move[E](src :C[E], srcPos :Long, dst :C[E], dstPos :Long, length :Long) :Unit

		def moveTo[C1[_], E](src :C[E], srcPos :Long, dst :C1[E], dstPos :Long, length :Long)
		                    (implicit dimension :CubeDimension[C1]) :Unit = ???

		/** Moves free data arrays from range `[srcPos, srcPos + length)` in `src` to range `[dst, dstPos + length)`
		  * of cube `dst`. The source range must be fully allocated, and if the destination range contains
		  * any allocated data arrays, they will be overridden. This method is different from `move` in that
		  * the arrays are assumed to contain no actual data, and can be copied in any order. This increases
		  * the likelihood of a possibility of moving whole, larger cubes, rather than carrying over the data arrays
		  * one by one.
		  */
		def reuse[E](src :C[E], srcPos :Long, dst :C[E], dstPos :Long, length :Long) :Unit

		def reuseIn[C1[_], E](src :C[E], srcPos :Long, dst :C[E], dstPos :Long, length :Long)
		                     (implicit dimension :CubeDimension[C1]) :Unit = ???

		/** Creates a new, uninitialized cube. For dimensions higher than 1, no actual data arrays are allocated -
		  * use [[net.noresttherein.sugar.collections.CubeBuffer.CubeDimension.alloc alloc]] for that.
		  */
		final def make[E :ClassTag](edge :Int = MaxEdge) :C[E] = Dim match {
			case 1 => new Array[E](edge).asInstanceOf[C[E]]
			case 2 => new Array[Array[E]](edge).asInstanceOf[C[E]]
			case 3 => new Array[Array[Array[E]]](edge).asInstanceOf[C[E]]
			case 4 => new Array[Array[Array[Array[E]]]](edge).asInstanceOf[C[E]]
			case 5 => new Array[Array[Array[Array[Array[E]]]]](edge).asInstanceOf[C[E]]
		}

		/** Allocate (in place) lower dimensional arrays within `cube` between absolute element indices
		  * `from` and `until`. The arrays are allocated at
		  * [[net.noresttherein.sugar.collections.CubeBuffer.CubeDimension.project projections]] of indices within
		  * `[from, until)` range onto this dimension. Existing contents are not affected.
		  *
		  * This method works differently
		  * for the [[net.noresttherein.sugar.collections.CubeBuffer.Cube1Dimension first dimension]]: the argument
		  * `cube` is ignored (in practice, given as `null`, and a new `Array[E]` is created.
		  * @param cube  top level array, in which new lower level arrays are to be allocated.
		  * @param from  an absolute index in the maximal capacity of a cube of this dimension, at
		  *              which allocated space in `cube` starts. Always a multiplicity of
		  *              [[net.noresttherein.sugar.collections.CubeBuffer.MaxEdge MaxEdge]].
		  * @param until an absolute index in the maximal capacity of the cube, to which storage space
		  *              must be allocated. For cubes of dimensions higher than 1, the arrays of the first
		  *              dimension are always allocated with `MaxEdge` capacity.
		  * @return `cube` argument, after setting indices `[project(currentCapacity), project(newCapacity))`
		  *         to new `C` arrays of maximum (for its dimension) size, possibly with the exception of the last one.
		  */
		def alloc[E :ClassTag](cube :C[E], from :Long = 0, until :Long = MaxSize) :C[E]

		/** Grow the capacity of `cube` to `newCapacity`, by allocating additional arrays of lower dimensions
		  * in the required number, and possibly reallocating this whole array. For single dimensional arrays,
		  * it works like a straightforward `ArrayBuffer`, doubling `cube.length` until it is greater or equal
		  * to `newCapacity`, and copying the elements into the new array. For dimensions higher than 1,
		  * the method starts by determining the required array length of N-dimensional array
		  * (N=`this.`[[net.noresttherein.sugar.collections.CubeBuffer.CubeDimension.Dim Dim]])
		  * to cover `storageStart + newCapacity`,
		  * increasing the current length by a minimal required power of two. If the length of the array must increase,
		  * all N-1 dimensional cubes at indices `[project(storageStart), project(storageEnd))` are copied
		  * to the new cube `C[E]`. For dimensions higher than the first, no actual data is copied: only whole
		  * cubes of dimensions lower than `Dim`. Afterwards it delegates
		  * to [[net.noresttherein.sugar.collections.CubeBuffer.CubeDimension.alloc alloc]]
		  * to create enough arrays of lower dimensions.
		  * @param cube         a cube which must grow.
		  * @param storageStart the absolute index of the first allocated cell in `cube`.
		  * @param storageEnd   the absolute index of the first non-allocated index in `cube` after `storageStart`.
		  * @param newCapacity  the total required capacity, such that `newCapacity - storageEnd` covers the required
		  *                     capacity for extra elements added meant to be added to the buffer.
		  */
		def resize[E :ClassTag](cube :C[E], storageStart :Long, storageEnd :Long, newCapacity :Long) :C[E]

		/**  */ //todo: docs
		def realloc[E :ClassTag](cube :C[E], storageStart :Long, storageEnd :Long,
		                         newStorageStart :Long, newStorageEnd :Long) :C[E] = ???

		def raise[E :ClassTag](cube :C[E], offset :Int = 0, edge :Int = InitialCapacity) :Array[C[E]] = {
			val res = Dim match {
				case 1 => new Array[Array[E]](edge).asInstanceOf[Array[C[E]]]
				case 2 => new Array[Array[Array[E]]](edge).asInstanceOf[Array[C[E]]]
				case 3 => new Array[Array[Array[Array[E]]]](edge).asInstanceOf[Array[C[E]]]
				case 4 => new Array[Array[Array[Array[Array[E]]]]](edge).asInstanceOf[Array[C[E]]]
			}
			res.asInstanceOf[Array[C[E]]](offset) = cube
			res
		}

		/** If the underlying element type `E` is stored as a reference type (that is, the component type of the
		  * most nested array in the cube is a subtype of `AnyRef`), it sets all elements in the `[from, until)`
		  * absolute range (counting in individual elements `E`) to `null`. The call has no effects for value arrays.
		  * The method is intended to avoid memory leaks by allowing removed elements from the buffer,
		  * but physically still in the arrays, to be reclaimed by the garbage collector.
		  */
		@inline final def erase[E](cube :C[E], from :Long = 0, until :Long = MaxSize) :C[E] = {
			if (from < until && isRefType(cube))
				clear(cube, from, until)
			cube
		}

		/** If `please` is true, set all elements in the `[from, until)` range (counting in individual elements `E`)
		  * to `null`/`0`/`false`, depending on the underlying array's component type. If `please` is false,
		  * the call has no effect. This is a shorthand allowing larger methods invoking `erase` multiple times
		  * to cache the value of the argument, as returned
		  * by [[net.noresttherein.sugar.collections.CubeBuffer.CubeDimension.isRefType isRefType]].
		  */
		@inline final def erase[E](cube :C[E], from :Long, until :Long, please :Boolean) :C[E] = {
			if (from < until & please)
				clear(cube, from, until)
			cube
		}

		/** Recursively [[net.noresttherein.sugar.collections.extensions.ArrayExtension.clear clears]]
		  * all single dimensional arrays within this cube, without unreferencing/deallocating any array.
		  * Meant to free the contents removed from the `CubeBuffer` for garbage collection.
		  */
		def clear[E](cube :C[E], from :Long = 0, until :Long = MaxSize) :Unit

		/** Checks if `C` is an array and if the class of the most nested component is a subclass of `AnyRef`. */
		final def isRefType[E](cube :C[E]) :Boolean = {
			var cls :Class[_] = cube.getClass
			while ({
				val elemCls = cls.getComponentType
				elemCls != null && { cls = elemCls; true }
			}) {}
			cls <:< classOf[AnyRef]
		}

		def iterator[E](cube :C[E], from :Long, until :Long) :Iterator[E]
	}


	private implicit object Cube1Dimension extends CubeDimension[Cube1](1) {
		override def apply[E](cube :Cube1[E], index :Long) :E = cube(side1(index))
		override def update[E](cube :Cube1[E], index :Long, value :E) :Unit = cube(side1(index)) = value
		override def update[E](cube :Cube1[E], index :Long, max :Int, values :Iterable[E]) :Int = {
			values.copyToArray(cube, index.toInt, max)
			max
		}
		override def update[E](cube :Cube1[E], index :Long, max :Int, values :Iterator[E]) :Int =
			if (max == Int.MaxValue) //shortcut if we won't be copying to other arrays, reuse a possibly optimized method
				values.copyToArray(cube, index.toInt)
			else {
				values.foreachWithIndex((e, i) => cube(index.toInt + i) = e)
				max
			}

		override def copyToBuffer[E](cube :Cube1[E], from :Long, until :Long, buffer :CubeBuffer[E]) :Unit =
			if (from <= 0 && until >= cube.length)
				buffer.addAll(cube)
			else if (until > from)
				buffer.addAll(cube, from.toInt, (until - from).toInt)

		override def extract[E](cube :Cube1[E], from :Long, until :Long, align :Int, sameEdge :Boolean) :Cube1[E] =
			if (from <= 0 && until >= cube.length)
				cube
			else {
				val src = from.toInt
				val len = until.toInt - src
				val capacity = if (sameEdge) cube.length else newCapacity(len)
				val res = Array.like(cube, capacity)
				val pos = align match {
					case -1 => 0
					case  0 => from
					case  1 => capacity - len
					case  _ => illegalAlignment_!(align)
				}
				arraycopy(cube, src, res, 0, len)
				res
			}

		override def sliceInPlace[E](cube :Cube1[E], from :Long, until :Long, align :Int, sameEdge :Boolean)
				:Array[_] =
			extract(cube, from, until, align, sameEdge)

		override def shiftLeftInPlace[E](cube :Cube1[E], downTo :Long, from :Long, until :Long) :Cube1[E] = {
			arraycopy(cube, from.toInt, cube, downTo.toInt, (until - from).toInt)
			cube
		}
		override def shiftRightInPlace[E](cube :Cube1[E], from :Long, until :Long, upTo :Long) :Cube1[E] = {
			arraycopy(cube, from.toInt, cube, (upTo - (until - from)).toInt, (until - from).toInt)
			cube
		}
		override def copy[E](src :Cube1[E], srcPos :Long, dst :Cube1[E], dstPos :Long, length :Long) :Unit =
			arraycopy(src, srcPos.toInt, dst, dstPos.toInt, length.toInt)

		override def move[E](src :Cube1[E], srcPos :Long, dst :Cube1[E], dstPos :Long, length :Long) :Unit =
			arraycopy(src, srcPos.toInt, dst, dstPos.toInt, length.toInt)

		override def reuse[E](src :Cube1[E], srcPos :Long, dst :Cube1[E], dstPos :Long, length :Long) :Unit = ()

		override def alloc[E :ClassTag](cube :Cube1[E], from :Long, until :Long) :Cube1[E] =
			new Array[E](until.toInt)

		override def resize[E :ClassTag](cube :Cube1[E], storageStart :Long, storageEnd :Long, newCapacity :Long) :Cube1[E] =
			Array.copyOf(cube, (storageStart + newCapacity).toInt)

		override def clear[E](cube :Cube1[E], from :Long, until :Long) :Unit = cube.clear(from.toInt, until.toInt)

		override def iterator[E](cube :Cube1[E], from :Long, until :Long) :Iterator[E] =
			ArrayIterator(cube, from.toInt, until.toInt)
	}

	private implicit val Cube2Dimension :CubeNDimension[Cube1] = new CubeNDimension[Cube1]
	private implicit val Cube3Dimension :CubeNDimension[Cube2] = new CubeNDimension[Cube2]
	private implicit val Cube4Dimension :CubeNDimension[Cube3] = new CubeNDimension[Cube3]
	private implicit val Cube5Dimension :CubeNDimension[Cube4] = new CubeNDimension[Cube4]

	private final class CubeNDimension[C[_]](implicit lower :CubeDimension[C])
		extends CubeDimension[({ type A[X] = Array[C[X]] })#A](lower.Dim + 1)
	{
		private[this] val lowerBits = DimBits * (Dim - 1)
		private[this] val LowerMaxSize = lower.MaxSize
		private[this] val LowerMask = (1L << (lowerBits + 1)) - 1L  //0b1000 - 1 == 0b111

		@inline private def relativeIdx(absoluteIndex :Long) :Long = absoluteIndex & LowerMask

		override def apply[E](cube :Array[C[E]], index :Long) :E = lower(cube(project(index)), relativeIdx(index))
		override def update[E](cube :Array[C[E]], index :Long, value :E) :Unit =
			lower.update(cube(project(index)), relativeIdx(index), value)

		override def update[E](cube :Array[C[E]], index :Long, max :Int, values :Iterable[E]) :Int = {
			var idxN  = project(index)
			val last = math.min(index + max, cube.length) - 1L
			val lastN = project(last)
			if (idxN == lastN)
				lower.update(cube(idxN), relativeIdx(index), max, values)
			else if (hasFastDrop(values)) {
				val idxL = relativeIdx(index)
				lower.update(cube(idxN), idxL, (LowerMaxSize - idxL).toInt, values)
				var remainder = values.drop((LowerMaxSize - idxL).toInt)
				idxN += 1
				while (idxN < lastN) {
					lower.update(cube(idxN), 0, LowerMaxSize.toInt, remainder)
					idxN += 1
					remainder = remainder.drop(LowerMaxSize.toInt)
				}
				lower.update(cube(lastN), 0, relativeIdx(last).toInt + 1, remainder)
				(last + 1L - index).toInt
			} else
				update(cube, index, max, values.iterator)
		}

		override def update[E](cube :Array[C[E]], index :Long, max :Int, values :Iterator[E]) :Int = {
			var idxN  = project(index)
			val last  = math.min(index + max, cube.length) - 1L
			val lastN = project(last)
			if (idxN == lastN)
				lower.update(cube(idxN), relativeIdx(index), max, values)
			else {
				val idxL = relativeIdx(index)
				lower.update(cube(idxN), idxL, (LowerMaxSize - idxL).toInt, values)
				idxN += 1
				while (idxN < lastN) {
					lower.update(cube(idxN), 0, LowerMaxSize.toInt, values)
					idxN += 1
				}
				lower.update(cube(lastN), 0, relativeIdx(last).toInt + 1, values)
				(last + 1L - index).toInt
			}
		}

		override def copyToBuffer[E](cube :Array[C[E]], from :Long, until :Long, buffer :CubeBuffer[E]) :Unit =
			if (until > from) {
				val firstN = project(from)
				val firstL = relativeIdx(from)
				val lastN  = project(until - 1L)
				val lastL  = relativeIdx(until - 1L) + 1L
				if (firstN == lastN)
					lower.copyToBuffer(cube(firstN), firstL, lastL, buffer)
				else {
					lower.copyToBuffer(cube(firstN), firstL, LowerMaxSize, buffer)
					var i = firstN + 1
					while (i < lastN) {
						lower.copyToBuffer(cube(i), 0L, LowerMaxSize, buffer)
						i += 1
					}
					lower.copyToBuffer(cube(lastN), 0L, lastL, buffer)
				}
			}

		override def extract[E](cube :Cube1[C[E]], from :Long, until :Long, align :Int, sameEdge :Boolean)
				:Array[C[E]] =
			if (from <= 0L & until >= MaxSize)
				cube
			else {
				val firstN   = project(from)
				val firstL   = relativeIdx(from)
				val lastN    = project(until - 1L)
				val lastL    = relativeIdx(until - 1L) + 1L
				if (firstN == lastN) {
					val capacity = if (sameEdge) cube.length else MinCapacity
					val pos      = align match {
						case -1 => 0
						case  0 => firstN
						case  1 => capacity - 1
						case  _ => illegalAlignment_!(align)
					}
					val res = Array.like(cube, capacity)
					res(pos) = lower.extract(cube(firstN), firstL, lastL, align, sameEdge)
					if (firstL == 0L & lastL == LowerMaxSize)
						cube(firstN) = null.asInstanceOf[C[E]]
					res
				} else {
					val cubeCount      = lastN + 1 - firstN
					val fullCubesShift = project(from + MaxSize1 - 1L) - firstN
					val fullCubeCount  = project(until) - firstN - fullCubesShift
					val capacity       = if (sameEdge) cube.length else newCapacity(cubeCount)
					val pos            = align match {
						case -1 => fullCubesShift
						case 0 => firstN + fullCubesShift
						case 1 => capacity - cubeCount + fullCubesShift
						case _ => illegalAlignment_!(align)
					}
					val res = Array.like(cube, capacity)
					if (firstL > 0L)
						res(pos - 1) = lower.extract(cube(firstN), firstL, LowerMaxSize, 1, true)
					if (lastL < LowerMaxSize)
						res(pos + lastN - firstN) = lower.extract(cube(lastN), 0L, lastL, 0, true)
					arraycopy(cube, firstN + fullCubesShift, res, pos, fullCubeCount)
					cube.clear(firstN + fullCubesShift, firstN + fullCubesShift + fullCubeCount)
					res
				}
			}

		override def sliceInPlace[E](cube :Array[C[E]], from :Long, until :Long, align :Int, sameEdge :Boolean)
				:Array[_] =
		{
			val firstN = project(from)
			val firstL = relativeIdx(from)
			val lastN  = project(until - 1L)
			val lastL  = relativeIdx(until - 1L) + 1L
			if (firstN == lastN) {
				val subCube = cube(firstN)
				if (firstL == 0L & lastL == LowerMaxSize) //we still call lower.sliceInPlace for type safety, as C[E] is not an Array
					cube(firstN) = null.asInstanceOf[C[E]]
				lower.sliceInPlace(subCube, firstL, lastL, align, sameEdge)
			} else
				extract(cube, from, until, align, sameEdge)
		}

		@tailrec override def shiftLeftInPlace[E](cube :Array[C[E]], downTo :Long, from :Long, until :Long) :Array[C[E]] =
			if (from >= until) {
				erase(cube, downTo, until)
				cube
			} else {
				val fromN   = project(from)
				val fromL   = relativeIdx(from)
				val downToN = project(downTo)
				val downToL = relativeIdx(downTo)
				val untilN  = project(until)
//				val untilL  = relativeIdx(until)
				val shiftN  = fromN - downToN
				if (downToL > 0) { //fill up self(downToN) to the brim, reducing the problem to downToL == 0
					val src = cube(fromN)
					val dst = cube(downToN)
					val length  = until - from
					val padding = LowerMaxSize - downToL
					if (length <= padding) { //not enough data even to fill up the dst cube
						lower.copy(src, fromL, dst, downToL, length)
						erase(cube, downTo + length, from + length)
					} else if (fromL < downToL) { //we have enough data in cube(fromN) to fill up cube(downToN)
						val filler = LowerMaxSize - downToL
						lower.copy(src, fromL, dst, downToL, filler)
						shiftLeftInPlace(cube, downTo + filler, from + filler, until)
					} else {
						val fromNPart = LowerMaxSize - fromL
						lower.copy(src, fromL, dst, downToL, fromNPart)
						lower.copy(cube(fromN + 1), 0, dst, downToL + fromNPart, padding - fromNPart)
						shiftLeftInPlace(cube, downTo + padding, from + padding, until)
					}
				} else if (fromL == 0) { //move whole lower dimension cube arrays, modifying only this array
					erase(cube, downTo, from)
					cube.rotateLeft(downToN, untilN)(shiftN) //rotate to reclaim used lower cubes as empty
					cube
				} else if (shiftN == 0) {
					var i = downToN
					val last = project(until - 1L)
					/* If untilL < fromL, then the last iteration will shift into cube(last - 1)
					 * fromL - untilL elements from beyond length, but we assume they are erased,
					 * so it works in our favour, as it erases everything beyond the last copied element.
					 */
					while (i < last) {
						lower.shiftIntoLeft(fromL, cube(i), cube(i + 1))
						i += 1
					}
					val remaining = relativeIdx(until - 1L) + 1
					val cubeL     = cube(last)
					if (remaining > fromL)
						lower.shiftLeftInPlace(cubeL, 0L, from, LowerMaxSize)
					lower.erase(cubeL, remaining - fromL, remaining) //ArrayExtension.clear() is permissive in indices
					cube
				} else { //downToL == 0 && fromL > 0  =>  shift left by fromL
					/* Shift contents of every array left by fromL, fill in the vacated positions with lower fromL items
					 * from the following array, and move the whole array down by shiftN slots in this cube.
					 */
					val last = project(until - 1L)
					var idxN = downToN
					while (idxN < fromN) {
						/* Move arrays at idxN <- idxN + shiftN <- idxN + 2 * shiftN ...
						 * As we move an array to cover shift in the second dimension, we shift it left internally
						 * in the first dimension, and fill the upper indices with first elements of the directly
						 * following array.
						 */
						val save = cube(idxN)
						var dst  = idxN
						var src  = idxN + shiftN
						/* If 0 < untilL < fromL, then the last step of the iteration will carry over
						 * into src(last - 1) fromL - untilL uninitialized values from beyond length.
						 * This works in our favour, as they are supposed to be erased, and thus will erase everything
						 * past last copied data.
						 */
						while (src < last) {
							cube(dst) = lower.shiftIntoLeft(fromL, cube(src), cube(src + 1))
							dst = src
							src += shiftN
						}
						if (src == last) {
							val tail = cube(src)
							val remaining = relativeIdx(until - 1L) + 1
							if (remaining > fromL)
								lower.shiftLeftInPlace(tail, 0L, fromL, remaining)
							cube(dst) = tail
							lower.erase(tail, remaining - fromL, remaining) //erase accepts indices out of range :)
							dst = src
						}
						lower.erase(save)     //empty the 'erased' array and reclaim it at the end of the cube.
						cube(dst) = save
						idxN += 1
					}
					cube
				}
			}

		@tailrec override def shiftRightInPlace[E](cube :Array[C[E]], from :Long, until :Long, upTo :Long)
				:Array[C[E]] =
			if (until <= from) {
				erase(cube, from, upTo)
				cube
			} else {
				val fromN   = project(from)
				val fromL   = relativeIdx(from)
				val untilN  = project(until)
				val untilL  = relativeIdx(until)
				val upToN   = project(upTo)
				val upToL   = relativeIdx(upTo)
				if (upToL > 0) { //pad cube(upToN) from the left to reduce upToL to 0
					val src = cube(untilN)
					val dst = cube(upToN)
					val length = until - from
					if (length <= upTo) { //not enough data to fill in even a single cube
						lower.copy(src, fromL, dst, upToL - length, length)
						erase(cube, from, upTo - length)
					} else if (untilL >= upToL) { //we have enough data in the first shift cube to fill up cube(upToN)
						lower.copy(src, untilL - upToL, dst, 0L, upToL)
						shiftRightInPlace(cube, from, until - upToL, upTo - upToL)
					} else {
						val missing = upToL - untilL
						lower.copy(src, 0L, dst, missing, untilL)
						//fill up whatever is missing with cube(untilN - 1)
						lower.copy(cube(untilN - 1), LowerMaxSize - missing, dst, 0L, missing)
						shiftRightInPlace(cube, from, until - upToL, upTo - upToL)
					}
				} else if (untilL == 0) { //copy whole Cube1 arrays, modifying only this array
					erase(cube, until, upTo)
					cube.rotateRight(fromN, upToN)(upToN - untilN) //rotate to reclaim used lower cubes as empty
					cube
				} else if (untilN == upToN) {
					val shift = LowerMaxSize - untilL
					var i = upToN - 1
					/* If fromL > untilL, then the last iteration will shift into cube(untilN + 1)
					 * untilL - fromL elements from before offset, but we assume they are erased,
					 * so it works in our favour, as it erases everything preceding copied data.
					 */
					while (i > fromN) {
						lower.shiftIntoRight(shift, cube(i - 1), cube(i))
						i -= 1
					}
					val cubeL = cube(fromN)
					val remaining = untilL - fromL
					if (remaining > 0)
						lower.shiftRightInPlace(cubeL, fromL, untilL, LowerMaxSize)
					lower.erase(cubeL, fromL, LowerMaxSize - remaining)
					cube
				} else { //upToL == 0 && untilL > 0
					/* Shift contents of every array right by LowerMaxSize - untilL, fill in the vacated positions
					 * with upper items from the preceding array, and move the whole array up by upToN - untilN slots.
					 */
					val shiftN = upToN - untilN
					val shiftL = LowerMaxSize - untilL
					var idxN = upToN - 1 //the slot within the cleared gap we aim to fill.
					while (idxN >= untilN) {
						/* Move arrays at ... idxN - 2 * shiftN ->  idx2 - shiftN -> idx2
						 * As we move an array to cover shift in the second dimension, we shift it right internally
						 * in the first dimension, and fill the lower indices with last elements of the directly
						 * preceding array.
						 */
						val save = cube(idxN)
						var dst  = idxN
						var src  = idxN - shiftN
						while (src > fromN) {
							cube(dst) = lower.shiftIntoRight(shiftL, cube(src), cube(src - 1))
							dst = src
							src -= shiftN
						}
						if (src == fromN) {
							val first = cube(fromN)
							val remaining = untilL - fromL
							if (remaining > 0)
								lower.shiftRightInPlace(first, fromL, untilL, LowerMaxSize)
							lower.erase(first, fromL, LowerMaxSize - remaining)
							cube(dst) = first
							dst = src
						}
						lower.erase(save)     //empty the 'erased' array and reclaim it at the end of the cube.
						cube(dst) = save
						idxN += 1
					}
					cube
				}
			}


		@tailrec override def copy[E](src :Array[C[E]], srcPos :Long, dst :Array[C[E]], dstPos :Long, length :Long)
				:Unit =
			if ((src eq dst) && (if (srcPos <= dstPos) srcPos + length > dstPos else dstPos + length > srcPos))
				shiftInPlace(src, srcPos, dstPos, length)
			else if (length > 0) {
				val srcN  = project(srcPos)
				val srcL  = relativeIdx(srcPos)
				val dstN  = project(dstPos)
				val dstL  = relativeIdx(dstPos)
				if (dstL > 0) {
					val srcHead = src(srcN)
					val dstHead = dst(dstN)
					val dstPadding = LowerMaxSize - dstL
					if (length <= dstPadding)  //all the data is in a single cube
						lower.copy(srcHead, srcL, dstHead, dstL, length)
					else if (srcPos <= dstPos) { //we can fill up dst(dstN) with just the data from src(srcN)
						lower.copy(srcHead, srcL, dstHead, dstL, dstPadding)
						copy(src, srcPos + dstPadding, dst, dstPos + dstPadding, length - dstPadding)
					} else {
						val srcHeadPart = LowerMaxSize - srcL
						lower.copy(srcHead, srcL, dstHead, dstL, srcHeadPart)
						lower.copy(src(srcN + 1), 0, dstHead, dstL + srcHeadPart, dstPadding - srcHeadPart)
						copy(src, srcPos + dstPadding, dst, dstPos + dstPadding, length - dstPadding)
					}
				} else if (srcL == 0) { //copy the whole lower dimensional arrays
					val lengthN = project(length)
					val lengthL = relativeIdx(length)
					var i = 0
					while (i < lengthN) {
						lower.copy(src(srcN + i), 0, dst(dstN + i), 0, LowerMaxSize)
						i += 1
					}
					if (lengthL > 0)
						lower.copy(src(srcN + lengthN), 0, dst(dstN + lengthN), 0, lengthL)
				} else {
					val lengthN = project(length - 1L)
					val lengthL = relativeIdx(length - 1L) + 1
					var i = 0
					val firstPart = LowerMaxSize - srcL
					while (i < lengthN) {
						val dstI = dst(dstN + i)
						lower.copy(src(srcN + i), srcL, dstI, 0, firstPart)
						lower.copy(src(srcN + i + 1), 0, dstI, firstPart, srcL)
						i += 1
					}
					val lastDst = dst(dstN + lengthN)
					if (lengthL <= firstPart)
						lower.copy(src(srcN + lengthN), srcL, lastDst, 0L, lengthL)
					else {
						lower.copy(src(srcN + lengthN), srcL, lastDst, 0L, firstPart)
						lower.copy(src(srcN + lengthN + 1), 0L, lastDst, firstPart, lengthL - firstPart)
					}
				}
			}

		override def move[E](src :Array[C[E]], srcPos :Long, dst :Array[C[E]], dstPos :Long, length :Long) :Unit =
			if (length > 0 && ((src ne dst) || srcPos != dstPos)) {
				val srcN    = project(srcPos)
				val srcL    = relativeIdx(srcPos)
				val dstN    = project(dstPos)
				val dstL    = relativeIdx(dstPos)
				if (srcL == 0 & dstL == 0) {
					val lengthN = project(length)
					val lengthL = relativeIdx(length)
					arraycopy(src, srcN, dst, dstN, lengthN)
					if (lengthL > 0)
						lower.move(src(srcN + lengthN), 0L, dst(dstN + lengthN), 0L, lengthL)
				} else if (srcL < dstL) {
					val length1 = LowerMaxSize - dstL
					val length2 = dstL - srcL
					val offset2 = srcL + length1
					val untilN  = project(srcPos + length - 1)
					val untilL  = relativeIdx(srcPos + length - 1) + 1
					var i = srcN
					var j = dstN
					var cube2 = dst(dstN)
					while (i < untilN) {
						val cube1 = src(i)
						lower.move(cube1, srcL, cube2, dstL, length1)
						j += 1
						cube2 = dst(j)
						lower.move(cube1, offset2, cube2, 0, length2)
						i += 1
					}
					val cube1 = src(untilN)
					if (untilL <= length1)
						lower.move(cube1, srcL, cube2, dstL, untilL)
					else {
						lower.move(cube1, srcL, cube2, dstL, length1)
						lower.move(cube1, offset2, dst(j + 1), dstL, length1 - untilL)
					}
				} else {
					val length1 = LowerMaxSize - srcL
					val length2 = srcL - dstL
					val offset2 = dstL + length1
					val untilN  = project(dstPos + length - 1)
					val untilL  = relativeIdx(dstPos + length - 1) + 1
					var i = srcN
					var j = dstN
					var cube1 = src(srcN)
					while (j < untilN) {
						val cube2 = dst(j)
						lower.move(cube1, srcL, cube2, dstL, length1)
						i += 1
						cube1 = src(i)
						lower.move(cube1, 0, cube2, offset2, length2)
						j += 1
					}
					val cube2 = dst(untilN)
					if (untilL <= length1)
						lower.move(cube1, srcL, cube2, dstL, untilL)
					else {
						lower.move(cube1, srcL, cube2, dstL, length1)
						lower.move(src(i + 1), 0, cube2, offset2, length1 - untilL)
					}
				}
			}

		override def reuse[E](src :Cube1[C[E]], srcPos :Long, dst :Cube1[C[E]], dstPos :Long, length :Long) :Unit =
			if (length > 0 && ((src ne dst) || srcPos != dstPos)) {
				val firstN = project(srcPos)
				var firstL = relativeIdx(srcPos)
				val lastN  = project(srcPos + length - 1L)
				var lastL  = relativeIdx(srcPos + length - 1L) + 1L
				val dstN   = project(dstPos)
				var dstL   = relativeIdx(dstPos)
				if (firstL == 0 & dstL == 0) {
					val lengthN = project(length)
					val lengthL = relativeIdx(length)
					arraycopy(src, firstN, dst, dstN, lengthN)
					lower.reuse(src(firstN + lengthN), 0L, dst(dstN + lengthN), 0L, lengthL)
				} else if (firstN == lastN && dstL + length <= LowerMaxSize)
					lower.reuse(src(firstN), firstL, dst(dstN), dstL, length)
				else {
					val firstCube   = src(firstN)
					val lastCube    = src(lastN)
					val dstCube     = dst(dstN)
					val dstLastCube = dst(dstN +  1)
					val dstLastL    = relativeIdx(dstL + length)
					val padding = LowerMaxSize - dstL
					val submask = LowerMask >> DimBits
					val subcubeSize = LowerMaxSize >> DimBits
					var mask = DimMask2
					while ((dstL & submask) != 0L) {
						if ((dstL & submask) == (firstL & submask)) {
							lower.move(firstCube, firstL, dstCube, dstL, subcubeSize - (dstL & submask))
							dstL = (dstL & submask) + subcubeSize
							firstL = (firstL & submask) + subcubeSize
						} else if ((dstL & submask) + (lastL & submask) == subcubeSize) {
							lower.move(lastCube, lastL - (lastL & submask), dstLastCube, dstL, lastL & submask)
							dstL += lastL & submask
							lastL &= submask
						} else {
							val pad =
								if ((dstL & mask) < (firstL & mask)) (firstL & mask) - (dstL & mask)
								else mask + 1L - ((dstL & mask) - (firstL & mask))
							lower.move(lastCube, lastL - pad, dstCube, dstL, pad)
							dstL += pad
							lastL -= pad
							mask = mask << DimBits | mask
						} //here: (dstL & mask) == (firstL & mask)
					}
					if (firstL == dstL) {
						lower.move(firstCube, firstL, dstCube, dstL, LowerMaxSize - dstL)
						lower.move(lastCube, 0L, dstLastCube, 0L, dstLastL)
					} else if (firstL < dstL) {
						lower.move(firstCube, firstL, dstCube, dstL, LowerMaxSize - dstL)
						lower.move(firstCube, firstL + LowerMaxSize - dstL, dstLastCube, 0L, dstL - firstL)
						lower.move(lastCube, 0L, dstLastCube, dstL - firstL, lastL)
					} else {
						lower.move(firstCube, firstL, dstCube, dstL, LowerMaxSize - firstL)
						lower.move(lastCube, 0L, dstCube, dstL + LowerMaxSize - firstL, firstL - dstL)
						lower.move(lastCube, firstL - dstL, dstLastCube, 0L, dstLastL)
					}
					if (firstN + 1 < lastN)
						arraycopy(src, firstN + 1, dst, dstN + 1, lastN - firstN - 1)
				}
			}

		override def alloc[E :ClassTag](cube :Array[C[E]], from :Long, until :Long) :Array[C[E]] = {
			var fromN  = project(from & DimMask1)     //align to full 1-dimensional cubes
			val fromL  = relativeIdx(from & DimMask1)
			val untilN = project(until + MaxSize1 - 1 & DimMask) //aligns to the end of the nearest k * MaxSize1
			val untilL = relativeIdx(until + MaxSize1 - 1 & DimMask)
			if (fromN == untilN)
				cube(fromN) = lower.alloc(cube(fromN), fromL, untilL)
			else {
				cube(fromN) = lower.alloc(cube(fromN), fromL, LowerMaxSize)
				fromN += 1
				while (fromN < untilN) {
					cube(fromN) = lower.alloc(lower.make[E]())
					fromN += 1
				}
				if (untilL > 0)
					cube(fromN) = lower.alloc(lower.make[E](), 0L, untilL)
			}
			cube
		}

		override def resize[E :ClassTag](cube :Array[C[E]], storageStart :Long, storageEnd :Long, newCapacity :Long)
				:Array[C[E]] =
		{
			val minEdge     = project(storageStart + newCapacity - 1L) + 1
			var edge        = cube.length
			val resized     =
				if (edge >= minEdge)
					cube
				else {
					while (edge < minEdge)
						edge <<= 1
					val fromN   = project(storageStart)
					val lengthN = project(storageEnd - storageStart - 1L) + 1
					val copy = make[E](edge)
					arraycopy(cube, fromN, copy, 0, lengthN)
					copy
				}
			alloc(resized, storageEnd, newCapacity)
		}

		override def clear[E](cube :Array[C[E]], from :Long, until :Long) :Unit = {
			val fromN  = project(from)
			val fromL  = relativeIdx(from)
			val untilN = project(until)
			val untilL = relativeIdx(until)
			if (fromN == untilN)
				lower.clear(cube(fromN), fromL, untilL)
			else {
				lower.clear(cube(fromN), fromL, LowerMaxSize)
				var i = fromN + 1
				while (i < untilN) {
					lower.clear(cube(i))
					i += 1
				}
				if (untilL > 0)
					lower.clear(cube(untilN), 0L, untilL)
			}
		}


		override def iterator[E](cube :Array[C[E]], from :Long, until :Long) :Iterator[E] =
			if (until <= from || until <= 0)
				Iterator.empty
			else
				new Iterators.AbstractFlatMap[E] {
					private[this] var idx    = project(from)
					private[this] var limit  = until
					private[this] var limitN = project(until - 1)
					locally {
						val end = if (idx == limitN) relativeIdx(limit) else lower.MaxSize
						enqueue(lower.iterator(cube(idx), relativeIdx(from), end))
					}
					override def knownSize =
						if (idx == limitN)
							source.knownSize
						else
							source.knownSize + ((limitN - idx) * lower.MaxSize + relativeIdx(limit - 1) + 1).toInt
					enqueue()

					private def enqueue() :Iterator[E] = {
						idx += 1
						val lim = if (idx == limitN) relativeIdx(limit) else lower.MaxSize
						val it = lower.iterator(cube(idx), 0, lim)
						enqueue(it)
						it
					}
					override def hasNext :Boolean = super.hasNext || idx < limitN && { enqueue(); true }

					override def drop(n :Int) :Iterator[E] = {
						if (n > 0) {
							var it = source
							var rem = n
							var cubeSize = it.knownSize
							while (cubeSize <= rem && idx < limitN) {
								rem -= cubeSize
								it = enqueue()
								cubeSize = it.knownSize
							}
							if (rem > 0)
								enqueue(it.drop(rem))
						}
						this
					}
					override def take(n :Int) :Iterator[E] = {
						if (n > 0) {
							val it = source
							val lim = if (limitN == idx) relativeIdx(limit - 1) + 1 else lower.MaxSize
							val offset = lim - it.knownSize
							if (limit > idx * lower.MaxSize + offset + n) {
								limit = idx * lower.MaxSize + offset + n
								limitN = project(limit - 1)
								if (idx == limitN)
									enqueue(it.drop(n))
							}
						}
						this
					}

					override def copyToArray[B >: E](xs :Cube1[B], start :Int, len :Int) :Int = {
						val size = knownSize
						val length = xs.length
						if (len <= 0 || start >= length || size == 0)
							0
						else if (start < 0)
							throw new IndexOutOfBoundsException(
								s"$this.copyToArray(${xs.className}{${xs.length}}, $start, $len)"
							)
						else {
							val total = math.min(size, math.min(len, length - start))
							val end = start + total
							var rem = total
							var offset = start + source.copyToArray(xs, start, rem)
							rem -= offset - start
							while (offset < end) {
								enqueue()
								val copied = source.copyToArray(xs, offset, rem)
								rem -= copied
								offset += copied
							}
							total
						}
					}
					override def toString = "Iterator[" + source + "](" + idx + ")"
				}

		override def toString :String = Dim.toString + "D"
	}




	private class CubeBufferIterator[+E](cube :Cube[E], dim :Int, from :Long, until :Long, currentMutations: => Int)
		extends Iterator[E]
	{
		private[this] val mutationsOnCreation = currentMutations
		private[this] var cube5 :Cube5[E] = _ //the cube being iterated over, if dim == 5
		private[this] var cube4 :Cube4[E] = _ //the current (and only, if dim == 4) four dimensional cube
		private[this] var cube3 :Cube3[E] = _ //the current (and only, if dim == 3) three dimensional cube
		private[this] var cube2 :Cube2[E] = _ //the current (and only, if dim == 2) two dimensional cube
		private[this] var cube1 :Cube1[E] = _ //the current (and only, if dim == 1) data array
		private[this] var idx   :Long = from  //the absolute index in the whole cube, whater dimension it may be
		private[this] var last5 :Long = -1    //the absolute index of the last element of this iterator
		private[this] var last4 :Long = -1    //the absolute index of the last element in the current cube4
		private[this] var last3 :Long = -1    //the absolute index of the last element in the current cube3
		private[this] var last2 :Long = -1    //the absolute index of the last element in the current cube2
		private[this] var last1 :Long = -1    //the absolute index of the last element in the current cube1
		dim match {
			case 1 => cube1 = cube.castFrom[Cube[E], Cube1[E]]; last1 = until - 1L
			case 2 => cube2 = cube.castFrom[Cube[E], Cube2[E]]; last2 = until - 1L
			case 3 => cube3 = cube.castFrom[Cube[E], Cube3[E]]; last3 = until - 1L
			case 4 => cube4 = cube.castFrom[Cube[E], Cube4[E]]; last4 = until - 1L
			case 5 => cube5 = cube.castFrom[Cube[E], Cube5[E]]; last5 = until - 1L
			case _ =>
				throw new IllegalArgumentException(
					"Unsupported cube dimension " + dim + " for cube " + cube.className + "[" + from + ".." + until + "]"
				)
		}
		private def lastIdx :Long = dim match {
			case 1 => last1
			case 2 => last2
			case 3 => last3
			case 4 => last4
		}

		private def reposition() :Boolean = {
			if (idx >= last1) {
				if (dim == 1)
					return false
				if (idx >= last2) {
					if (dim == 2)
						return false
					if (idx >= last3) {
						if (dim == 3)
							return false
						if (idx >= last4) {
							if (dim == 4)
								return false
							if (idx >= last5)
								return false
							val idx5 = project5(idx)
							cube4 = cube5(idx5)
							last4   = if (idx5 == project5(last5)) last5 else (idx & DimMask5) + (MaxSize4 - 1L)
						}
						val idx4 = project4(idx)
						cube3 = cube4(idx4)
						last3   = if (idx4 == project4(last4)) last4 else (idx & DimMask4) + (MaxSize3 - 1L)
					}
					val idx3 = project3(idx)
					cube2 = cube3(idx3)
					last2   = if (idx3 == project3(last3)) last3 else (idx & DimMask3) + MaxSize2 - 1L
				}
				val idx2 = project2(idx)
				cube1 = cube2(idx2)
				last1   = if (idx2 == project2(last2)) last2 else (idx & DimMask2) + MaxSize1 - 1L
			}
			true
		}

		override def hasNext :Boolean = idx <= until
		override def next() :E = {
			if (idx > until)
				throw new NoSuchElementException(toString)
			if (currentMutations != mutationsOnCreation)
				throw new ConcurrentModificationException(toString)
			if (idx > last1)
				reposition()
			val res = cube1(idx.toInt & DimMask)
			idx += 1L
			res
		}

		override def drop(n :Int) :Iterator[E] = {
			if (n > 0)
				idx += n
			this
		}

		override def take(n :Int) :Iterator[E] = {
			val end = idx + n
			if (end < last5) {
				last1 = -1L //negating the limits forces the next next() call to reposition()
				last2 = -1L
				last3 = -1L
				last4 = -1L
				last5 = -1L
				dim match {
					case 1 => last1 = end
					case 2 => last2 = end
					case 3 => last3 = end
					case 4 => last4 = end
					case _ => last5 = end
				}
			}
			this
		}

		override def hashCode :Int = (cube.identityHashCode * 31 + idx.hashCode) + lastIdx.hashCode
		override def toString :String = "CubeIterator[" + cube.localClassName + "](" + idx + ".." + lastIdx + ")"
	}

}
