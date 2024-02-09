package net.noresttherein.sugar.collections

import java.lang.System.arraycopy

import scala.collection.generic.DefaultSerializable
import scala.collection.{Factory, IterableFactoryDefaults, SeqFactory, StrictOptimizedSeqFactory, StrictOptimizedSeqOps, mutable}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Builder, GrowableBuilder}
import scala.reflect.ClassTag

import net.noresttherein.sugar.arrays.{IArrayLike, IRefArray, RefArray}
import net.noresttherein.sugar.arrays.ArrayCompanionExtension
import net.noresttherein.sugar.casting.{castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.collections.Constants.MaxArraySize
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.collections.extensions.IterableOnceExtension
import net.noresttherein.sugar.concurrent.Fences.releaseFence
import net.noresttherein.sugar.exceptions.outOfBounds_!
import net.noresttherein.sugar.illegal_!




//todo: a separate @specialized version, at least using specific arrays.
/** An `ArrayBuffer` subclass implementing `toSeq` and `toIndexedSeq` by sharing the underlying array
  * (providing at least half of it is used, i.e. `this.array.length >= this.length * 2`).
  * This sets a 'read only' flag, and any subsequent buffer modifications will reallocate the array,
  * similarly to the strategy of [[scala.collection.mutable.ListBuffer ListBuffer]].
  * @define Coll `AliasingArrayBuffer`
  * @define coll aliasing array buffer
  * @author Marcin Mościcki
  */
private class AliasingArrayBuffer[E](capacity :Int)
	extends ArrayBuffer[E](capacity) with ArrayIterableOnce[E]
	   with mutable.IndexedSeqOps[E, AliasingArrayBuffer, AliasingArrayBuffer[E]]
	   with StrictOptimizedSeqOps[E, AliasingArrayBuffer, AliasingArrayBuffer[E]]
	   with IterableFactoryDefaults[E, AliasingArrayBuffer] with DefaultSerializable
{
	def this() = this(ArrayBuffer.DefaultInitialSize)

	private[this] var aliased = false

	private[sugar] override def unsafeArray :Array[_] = array
	private[sugar] override def startIndex  :Int = 0
	private[sugar] override def isMutable = true

	private def dealias(extra :Int) :Unit =
		if (extra > 0) {
			val size      = this.length
			val length    = array.length
			val newLength = if (size < (length >> 1) - extra) length else newTotalSize(size + extra)
			array   = Array.copyOf(array, newLength)
			aliased = false
		}

	@inline private def newTotalSize(total :Int) :Int = {
		val res = math.max(math.min(array.length, MaxArraySize >> 1) << 1, total)
		if (res < total)
			throw new BufferFullException("Cannot allocate an array of length " + res + ".")
		res
	}

	@inline final override def ensureSize(n :Int) :Unit = {
		val oldArray = array
		if (n > oldArray.length) {
			val newSize = newTotalSize(n)
			array   = Array.copyOf(oldArray, newSize)
			aliased = false
		}
	}
	final override def ensureAdditionalSize(n :Int) :Unit =
		if (n > 0) {
			val size = super.size
			if (n < MaxArraySize - size) ensureSize(n + size)
			else throw new BufferFullException("Cannot allocate an array of length " + (size.toLong + n) + ".")
		}

	override def trimToSize() :Unit = {
		val oldArray = array
		super.trimToSize()
		if (array ne oldArray)
			aliased = false
	}

	override def update(index :Int, elem :E) :Unit = {
		if (aliased) {
			array    = Array.copyOf(array, array.length)
			aliased  = false
		}
		super.update(index, elem)
	}

	override def clear() :Unit = {
		if (aliased && length > 0) {
			val length = array.length
			val size   = this.length
			val newLength = if (size < (length >> 1) & size >= ArrayBuffer.DefaultInitialSize) size else length
			array   = new Array[AnyRef](newLength)
			aliased = false
		}
		super.clear()
	}

	override def clearAndShrink(size :Int) :this.type =
		if (aliased) {
			array = Array.emptyObjectArray
			aliased = false
			ensureSize(size)
			this
		} else
			super.clearAndShrink(size)

	override def addOne(elem :E) :AliasingArrayBuffer.this.type = {
		if (aliased) {
			dealias(1)
		}
		super.addOne(elem)
	}
	override def addAll(elems :IterableOnce[E]) :this.type = {
		if (aliased) {
			val extraSize = elems.knownSize match {
				case -1 => 1
				case  n => n
			}
			dealias(extraSize)
		}
		super.addAll(elems)
	}

	override def prepend(elem :E) :this.type = { insert(0, elem); this }
	override def prependAll(elems :IterableOnce[E]) :this.type = { insertAll(0, elems); this }

	override def insert(index :Int, elem :E) :Unit = {
		val size = this.length
		if (index == size)
			addOne(elem)
		else if (index < 0 | index > size)
			outOfBounds_!(errorString(this) + ".insert(" + index + ", _)")
		else {
			var array     = this.array
			val last      = array(size - 1).asInstanceOf[E]
			val length    = array.length
			if (aliased | size == length) {
				val newLength = if (size < (length >> 1)) length else newTotalSize(size + 1)
				array         = Array.copyOfRange(array, 0, index, newLength)
				arraycopy(this.array, index, array, index + 1, size - index - 1)
				this.array = array
				aliased    = false
			} else if (size < length) {
				arraycopy(array, index, array, index + 1, size - index - 1)
			}
			array(index) = elem.asInstanceOf[AnyRef]
			super.addOne(last) //because super must increase the size field
		}
	}

	override def insertAll(index :Int, elems :IterableOnce[E]) :Unit =
		if (aliased) {
			var array  = this.array
			val size   = this.length
			val length = array.length
			if (index < 0 | index > size)
				outOfBounds_!(
					errorString(this) + ".insertAll(" + index + ", " + errorString(elems) + ")"
				)
			val elemsSize = elems.knownSize
			if (elemsSize > 0) {
				val newLength = if (size < (length >> 1) - elemsSize) length else newTotalSize(size + elemsSize)
				if (elemsSize <= size - index) {
					//Copy elems, and this.array[index..size - elemsSize) to the new array, and then use
					// super.addAll(this.array[size-elemsSize, size)) to increase the buffer size accordingly.
					array = Array.copyOfRange(array, 0, index, newLength)
					val copied = elems.toBasicOps.copyToArray(array.castParam[Any], index)
					if (copied != elemsSize)
						throw new IllegalStateException(
							errorString(elems) + " copied " + copied + " instead of " + elemsSize
						)
					arraycopy(this.array, index, array, index + elemsSize, size - index - elemsSize)
					super.addAll(RefArraySlice.slice(array.castFrom[Array[AnyRef], RefArray[E]], size - elemsSize, size))
					this.array = array
				} else {
					this.array = Array.copyOf(array, newLength)
					super.insertAll(index, elems)
				}
				aliased = false
			} else {
				dealias(1)
				super.insertAll(index, elems)
			}
		} else
			super.insertAll(index, elems)

	override def subtractOne(x :E) :this.type = {
		val i = array.lastIndexOf(x.asInstanceOf[AnyRef], size - 1)
		if (i >= 0)
			remove(i)
		this
	}

	override def remove(index :Int) :E =
		if (aliased) {
			val size = this.length
			if (index < 0 | index >= size)
				outOfBounds_!(index, this)
			val array  = this.array
			val res    = array(index).asInstanceOf[E]
			this.array = Array.copyOfRanges(array, 0, index, array, index + 1, size, array.length)
			super.remove(size - 1) //to reduce the size
			aliased   = false
			res
		} else
			super.remove(index)

	override def remove(index :Int, count :Int) :Unit =
		if (aliased) {
			if (count < 0)
				illegal_!(
					"Cannot remove a negative number of elements: " + errorString(this) +
						".remove(" + index + ", " + count + ")"
				)
			val size = this.length
			if (index < 0 | index > size - count)
				outOfBounds_!(errorString(this) + ".remove(" + index + ", " + count + ")")
			val array  = this.array
			val newLen = math.max(size, (size - count) << 1) //super.remove will fail if array.length < size
			this.array = Array.copyOfRanges(array, 0, index, array, index + count, size, newLen)
			aliased   = false
			super.remove(size - count, count) //to reduce the size
		} else
			super.remove(index, count)

	override def sortInPlace[U >: E]()(implicit ord :Ordering[U]) :this.type = {
		if (aliased) {
			this.array = Array.copyOf(array, array.length)
			aliased = false
		}
		super.sortInPlace[U]()
	}

	private def become(iter :Iterable[E]) :this.type = {
		super.remove(0, length)
		aliased = false
		addAll(iter)
	}

	override def flatMapInPlace(f :E => IterableOnce[E]) :this.type =
		if (aliased)
			become(ArraySlice.slice(array.asInstanceOf[Array[E]], 0, length).flatMap(f))
		else
			super.flatMapInPlace(f)

	override def filterInPlace(p :E => Boolean) :this.type =
		if (aliased)
			become(ArraySlice.slice(array.asInstanceOf[Array[E]], 0, length).filter(p))
		else
			super.filterInPlace(p)

	override def patchInPlace(from :Int, patch :IterableOnce[E], replaced :Int) :this.type =
		if (aliased)
			become(ArraySlice.slice(array.asInstanceOf[Array[E]], 0, length).patch(from, patch.iterator, replaced))
		else
			super.patchInPlace(from, patch, replaced)

	override def dropInPlace(n :Int) :this.type =
		if (aliased)
			if (n > 0) become(ArraySlice.slice(array.asInstanceOf[Array[E]], 0, length).drop(n))
			else this
		else
			super.dropInPlace(n)

	override def dropRightInPlace(n :Int) :this.type =
		if (n < 0) this
		else takeInPlace(length - n)

	override def takeInPlace(n :Int) :this.type =
		if (aliased)
			if (n < length) become(ArraySeq.unsafeWrapArray(array.asInstanceOf[Array[E]].take(n)))
			else this
		else
			super.takeInPlace(n)

	override def takeRightInPlace(n :Int) :this.type =
		if (n < 0) this
		else dropInPlace(length - n)

	override def sliceInPlace(start :Int, end :Int) :this.type =
		if (aliased)
			if (start <= 0 && end >= length) this
			else become(ArraySeq.unsafeWrapArray(array.asInstanceOf[Array[E]].slice(start, end)))
		else
			super.sliceInPlace(start, end)

	override def dropWhileInPlace(p :E => Boolean) :this.type = dropInPlace(segmentLength(p))
	override def takeWhileInPlace(p :E => Boolean) :this.type = takeInPlace(segmentLength(p))

	override def padToInPlace(len :Int, elem :E) :this.type =
		if (aliased)
			if (len <= length) this
			else become(ConstSeq(elem, len - length))
		else
			super.padToInPlace(len, elem)

	override def toSeq :Seq[E] = toIndexedSeq

	override def toIndexedSeq :IndexedSeq[E] =
		if (size == 0)
			IndexedSeq.empty
		else if (size >= (array.length >> 1)) {
			aliased = true
			releaseFence()
			IRefArraySlice.slice(array.asInstanceOf[IRefArray[E]], 0, size)
		} else
			ArraySeq.unsafeWrapArray(toArray(ClassTag.Any.castParam[E]))

	override def to[C1](factory :Factory[E, C1]) :C1 = factory match {
		case CompanionFactory.IterableFactory(Seq | IndexedSeq) =>
			toIndexedSeq.asInstanceOf[C1]
		case CompanionFactory.IterableFactory(factory :ArrayLikeSliceWrapper[Seq, IArrayLike] @unchecked)
			if size >= (array.length >> 1) && //todo: a way of determining if a factory is immutable
				factory == RelayArray || factory == IArrayLikeSlice || factory == IRefArraySlice =>
			aliased = true
			factory.slice(array.asInstanceOf[IRefArray[E]], 0, size).asInstanceOf[C1]
		case _ =>
			super.to(factory)
	}

	override def iterableFactory :SeqFactory[AliasingArrayBuffer] = AliasingArrayBuffer
}




/** $factoryInfo
  * @define Coll `AliasingArrayBuffer`
  * @define coll aliasing array buffer*
  */
private case object AliasingArrayBuffer
	extends BufferFactory[AliasingArrayBuffer] with StrictOptimizedSeqFactory[AliasingArrayBuffer]
{
	override def ofCapacity[E](capacity :Int) :AliasingArrayBuffer[E] = new AliasingArrayBuffer[E](capacity)

	override def empty[E] :AliasingArrayBuffer[E] = new AliasingArrayBuffer[E]

	override def newBuilder[E] :Builder[E, AliasingArrayBuffer[E]] =
		new GrowableBuilder[E, AliasingArrayBuffer[E]](empty) {
			override def sizeHint(size :Int) :Unit = elems.ensureSize(size)
		}
}
