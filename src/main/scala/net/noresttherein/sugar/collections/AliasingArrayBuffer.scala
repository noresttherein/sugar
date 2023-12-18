package net.noresttherein.sugar.collections

import java.lang.System.arraycopy

import scala.collection.{Factory, StrictOptimizedSeqFactory, mutable}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.reflect.ClassTag

import net.noresttherein.sugar.collections.AliasingArrayBuffer.MinGrowThreshold
import net.noresttherein.sugar.outOfBounds_!
import net.noresttherein.sugar.collections.Constants.MaxArraySize
import net.noresttherein.sugar.collections.extensions.ArrayObjectExtension
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.extensions.{IteratorObjectExtension, castTypeParamMethods}




//todo: a separate @specialized version, at least using specific arrays.
/** An `ArrayBuffer` subclass implementing `toSeq` and `toIndexedSeq` by sharing the underlying array
  * (providing at least half of it is used, i.e. `this.array.length >= this.length * 2`).
  * This sets a 'read only' flag, and any subsequent buffer modifications will reallocate the array,
  * similarly to the strategy of [[scala.collection.mutable.ListBuffer ListBuffer]].
  * @define Coll `AliasingArrayBuffer`
  * @define coll aliasing array buffer
  * @author Marcin Mościcki
  */
private class AliasingArrayBuffer[E](capacity :Int) extends ArrayBuffer[E](capacity) {
	def this() = this(ArrayBuffer.DefaultInitialSize)

	private[this] var aliased = false
	private[this] var hinted = false

	@inline private def newTotalSize(total :Int) :Int = {
		val res = math.max(math.min(array.length, MaxArraySize >> 1) << 1, total)
		if (res < total)
			throw new IllegalArgumentException("Cannot allocate an array of length " + res + ".")
		res
	}
	@inline private def newSizeFor(delta :Int) :Int = {
		val size = this.length
		if (delta > Int.MaxValue - size) newTotalSize(size + delta)
		else throw new IllegalArgumentException("Cannot allocate an array of length " + (size.toLong + delta) + ".")
	}

	@inline final override def ensureSize(n :Int) :Unit = {
		val oldArray = array
		if (n > oldArray.length) {
			val newSize = newTotalSize(n)
			if (aliased) {
				array    = Array.copyOf(oldArray, newSize)
				aliased = false
			}
		}
	}
	final override def ensureAdditionalSize(n :Int) :Unit =
		if (n > 0) {
			val size = super.size
			if (n < MaxArraySize - size) ensureSize(n + size)
			else throw new IllegalArgumentException("Cannot allocate an array of length " + (size.toLong + n) + ".")
		}

	override def sizeHint(size :Int) :Unit =
		if (size > length) {
			hinted = true
			ensureSize(size)
		}

	override def trimToSize() :Unit = {
		val oldArray = array
		super.trimToSize()
		if (array ne oldArray)
			aliased = false
	}

	override def update(index :Int, elem :E) :Unit = {
		if (aliased) {
			val len  = array.length
			val size = this.size
			array    = Array.copyOf(array, if (hinted || size * 100L < len * MinGrowThreshold) len else size << 1)
			aliased  = false
		}
		super.update(index, elem)
	}

	override def clear() :Unit =
		if (aliased) {
			val array = this.array
			super.remove(0, length)
			if (array eq this.array)
				this.array = ErasedArray.empty
			aliased = false
		} else
			super.clear()

	override def clearAndShrink(size :Int) :this.type =
		if (aliased) {
			array = ErasedArray.empty
			aliased = false
			ensureSize(size)
			this
		} else
			super.clearAndShrink(size)

	override def addOne(elem :E) :AliasingArrayBuffer.this.type = {
		if (aliased) {
			ensureAdditionalSize(1)
			aliased = false
		}
		super.addOne(elem)
	}
	override def addAll(elems :IterableOnce[E]) :this.type = {
		if (aliased) {
			val size = elems.knownSize
			if (size >= 0)
				ensureAdditionalSize(size)
			else
				ensureAdditionalSize(1)
			aliased = false
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
			throw new IndexOutOfBoundsException(errorString(this) + ".insert(" + index + ", _)")
		else {
			var array     = this.array
			val last      = array(size - 1).asInstanceOf[E]
			val length    = array.length
			val newLength = if (size >= (length >> 1)) length else newSizeFor(1)
			array         = Array.copyOfRange(array, 0, index, newLength)
			array(index)  = elem.asInstanceOf[AnyRef]
			arraycopy(this.array, index, array, index + 1, size - index - 1)
			this.array = array
			aliased   = false
			super.addOne(last) //because super must increase the size field
		}
	}

	override def insertAll(index :Int, elems :IterableOnce[E]) :Unit =
		if (aliased) {
			val size = this.length
			if (index < 0 | index > size)
				throw new IndexOutOfBoundsException(
					errorString(this) + ".insertAll(" + index + ", " + errorString(elems) + ")"
				)
			val knownSize = elems.knownSize
			if (knownSize > 0)
				ensureAdditionalSize(knownSize)
			else if (knownSize < 0)
				ensureAdditionalSize(1)
			super.insertAll(index, elems)
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
				throw new IllegalArgumentException(
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

	private def become(iter :Iterator[E]) :this.type = {
		val array = this.array
		super.remove(0, size)
		if (array eq this.array)
			this.array = ErasedArray.empty
		aliased = false
		super.addAll(iter)
	}

	override def flatMapInPlace(f :E => IterableOnce[E]) :this.type =
		if (aliased)
			become(new ArrayIterator(array.asInstanceOf[Array[E]], 0, size).flatMap(f))
		else
			super.flatMapInPlace(f)

	override def filterInPlace(p :E => Boolean) :this.type =
		if (aliased)
			become(new ArrayIterator(array.asInstanceOf[Array[E]], 0, size).filter(p))
		else
			super.filterInPlace(p)

	override def patchInPlace(from :Int, patch :IterableOnce[E], replaced :Int) :this.type =
		if (aliased)
			become(new ArrayIterator(array.asInstanceOf[Array[E]], 0, size).patch(from, patch.iterator, replaced))
		else
			super.patchInPlace(from, patch, replaced)

	override def dropInPlace(n :Int) :this.type =
		if (aliased)
			if (n > 0) become(new ArrayIterator(array.asInstanceOf[Array[E]], 0, size).drop(n))
			else this
		else
			super.dropInPlace(n)

	override def dropRightInPlace(n :Int) :this.type =
		if (aliased)
			if (n > 0) become(new ArrayIterator(array.asInstanceOf[Array[E]], 0, size).take(length - n))
			else this
		else
			super.dropRightInPlace(n)

	override def takeInPlace(n :Int) :this.type =
		if (aliased)
			if (n < length) become(new ArrayIterator(array.asInstanceOf[Array[E]], 0, size).take(n))
			else this
		else
			super.takeInPlace(n)

	override def takeRightInPlace(n :Int) :this.type =
		if (aliased)
			if (n < 0) {
				val array = this.array
				super.remove(0, size)
				if (array eq this.array)
					this.array = ErasedArray.empty
				aliased = false
				this
			} else if (n < length)
				become(new ArrayIterator(array.asInstanceOf[Array[E]], 0, size).drop(length - n))
			else
				this
		else
			super.takeRightInPlace(n)

	override def sliceInPlace(start :Int, end :Int) :this.type =
		if (aliased)
			if (start <= 0 && end >= length) this
			else become(new ArrayIterator(array.asInstanceOf[Array[E]], 0, size).slice(start, end))
		else
			super.sliceInPlace(start, end)

	override def dropWhileInPlace(p :E => Boolean) :this.type = dropInPlace(segmentLength(p))
	override def takeWhileInPlace(p :E => Boolean) :this.type = takeInPlace(segmentLength(p))

	override def padToInPlace(len :Int, elem :E) :this.type =
		if (aliased)
			if (len <= length) this
			else become(Iterator.const(len - length)(elem))
		else
			super.padToInPlace(len, elem)

	override def toSeq :Seq[E] = toIndexedSeq

	override def toIndexedSeq :IndexedSeq[E] =
		if (size == 0)
			IndexedSeq.empty
		else if (size >= (array.length >> 1)) {
			aliased = true
			IRefArraySlice.of(array.asInstanceOf[IRefArray[E]], 0, size)
		} else
			ArraySeq.unsafeWrapArray(toArray(ClassTag.Any.castParam[E]))

	override def to[C1](factory :Factory[E, C1]) :C1 = factory match {
		case CompanionFactory.IterableFactory(Seq) | CompanionFactory.IterableFactory(IndexedSeq) =>
			toIndexedSeq.asInstanceOf[C1]
		case CompanionFactory.IterableFactory(DefaultArraySeq) if size >= (array.length >> 1) =>
			aliased = true
			DefaultArraySeq.of(array.asInstanceOf[IArray[E]], 0, size).asInstanceOf[C1]
		case _ =>
			super.to(factory)
	}
}


/** $factoryInfo
  * @define Coll `AliasingArrayBuffer`
  * @define coll aliasing array buffer*
  */
private object AliasingArrayBuffer
	extends BufferFactory[AliasingArrayBuffer] with StrictOptimizedSeqFactory[AliasingArrayBuffer]
{
	override def of[E] :AliasingArrayBuffer[E] = new AliasingArrayBuffer[E]

	override def ofCapacity[E](capacity :Int) :AliasingArrayBuffer[E] = new AliasingArrayBuffer[E](capacity)

	override def from[E](source :IterableOnce[E]) :AliasingArrayBuffer[E] =
		new AliasingArrayBuffer[E] ++= source

	override def empty[E] :AliasingArrayBuffer[E] = new AliasingArrayBuffer[E]

	override def newBuilder[E] :Builder[E, AliasingArrayBuffer[E]] =
		new mutable.GrowableBuilder[E, AliasingArrayBuffer[E]](empty) {
			override def sizeHint(size :Int) :Unit = elems.ensureSize(size)
		}

	private final val MinGrowThreshold = 75L
}
