package net.noresttherein.sugar.collections

import scala.Array.emptyObjectArray
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.Stepper.EfficientSplit
import scala.collection.immutable.{AbstractSeq, IndexedSeqOps}
import scala.collection.mutable.{ArrayBuilder, Builder}
import scala.collection.{ClassTagIterableFactory, EvidenceIterableFactory, IterableFactory, IterableFactoryDefaults, SeqFactory, Stepper, StepperShape, StrictOptimizedSeqFactory, View, mutable}
import scala.reflect.ClassTag

import net.noresttherein.sugar.outOfBounds_!
import net.noresttherein.sugar.JavaTypes.JIterator
import net.noresttherein.sugar.arrays.{ArrayFactory, ArrayIterator, ArrayLike, ArrayLikeOps, IArray, IArrayLike, IRefArray, MutableArray, RefArray, RefArrayLike, ReverseArrayIterator}
import net.noresttherein.sugar.arrays.extensions.{ArrayExtension, ArrayLikeExtension, ArrayObjectExtension, RefArrayExtension}
import net.noresttherein.sugar.collections.extensions.{IterableExtension, IterableOnceExtension, IteratorExtension}
import net.noresttherein.sugar.typist.casting.extensions.{castTypeConstructorMethods, castTypeParamMethods, castingMethods}




/** A collection backed by an array slice. Extended by the 'proper' `PassedArray` and various `ArrayLike` adapters,
  * but used instead of the latter in pattern matching to decouple code from the `PassedArray` type itself.
  */
private[sugar] trait ArrayIterableOnce[+E] extends Any with IterableOnce[E] {
	private[sugar] def unsafeArray :Array[_]
	private[sugar] def startIndex :Int
	private[sugar] def isImmutable :Boolean = false //mutability.isImmutable
	private[sugar] def isMutable :Boolean = false //mutability.isMutable
//	private[sugar] def mutability :Mutability
	//todo: add ArrayIterableOnceOps
/*
	override def copyRangeToArray[A >: E](xs :Array[A], from :Int, start :Int, len :Int) :Int =
		if (len <= 0 || start >= xs.length || from >= knownSize)
			0
		else if (start < 0)
			throw new IndexOutOfBoundsException(start)
		else {
			val from0  = math.max(from, 0)
			val copied = math.min(knownSize - from0, math.min(len, xs.length - start))
			ArrayLike.copy(unsafeArray, startIndex + from0, xs, start, copied)
			copied
		}

	override def cyclicCopyRangeToArray[A >: E](xs :Array[A], from :Int, start :Int, len :Int) :Int =
		if (len <= 0 || from >= knownSize)
			0
		else if (start < 0)
			throw new IndexOutOfBoundsException(start)
		else {
			val from0  = math.max(from, 0)
			val start0 = start % xs.length
			val copied = math.min(knownSize - from0, math.min(len, xs.length - start))
			ArrayLike.cyclicCopyTo(unsafeArray, startIndex + from0, xs, start, copied)
			copied
		}
*/
}



private[sugar] trait ArraySliceOps[+E, +CC[_], +C]
	extends ArrayIterableOnce[E] with SugaredIterableOps[E, CC, C] with SlicingOps[E, C]
{
	@inline private def array :Array[E @uncheckedVariance] = unsafeArray.asInstanceOf[Array[E]]

	override def head :E =
		if (size == 0) throw new NoSuchElementException(toString + ".head")
		else array(startIndex)

	override def last :E = {
		val len = size
		if (len == 0) throw new NoSuchElementException(toString + ".last")
		else array(startIndex + len - 1)
	}
/*

	override def tail :C = {
		val len = size
		if (len == 0) throw new UnsupportedOperationException(toString + ".tail")
		else trustedSlice(1, len)
	}
	override def init :C = {
		val len = size
		if (len == 0) throw new UnsupportedOperationException(toString + ".init")
		else trustedSlice(0, len - 1)
	}

	override def take(n :Int) :C = slice(0, n)
	override def drop(n :Int) :C = slice(n, size)
	override def takeRight(n :Int) :C = if (n <= 0) empty else { val len = size; slice(len - math.max(n, 0), len) }
	override def dropRight(n :Int) :C = if (n <= 0) coll else slice(0, size - math.max(n, 0))

	override def slice(from :Int, until :Int) :C = {
		val length = size
		if (until <= from | until <= 0 || from >= length) empty
		else if (from <= 0 & until >= length) coll
		else if (from <= 0) trustedSlice(0, until)
		else if (until >= length) trustedSlice(from, length)
		else trustedSlice(from, until)
	}

	protected def trustedSlice(from :Int, until :Int) :C
*/

	override def iterator :Iterator[E] = ArrayIterator(array, startIndex, size)

//	override def reverseIterator :Iterator[E] = {
//		val start = startIndex
//		Iterator.reverse(array, start, start + size)
//	}
	override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[E, I]) :I = {
		val start = startIndex
		JavaIterator.slice(array, start, start + size)
	}
	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S with EfficientSplit =
		ArrayStepper(array, startIndex, size)

	protected override def segmentLength(p :E => Boolean, from :Int) :Int =
		ArrayLikeOps.segmentLength(array, startIndex, size)(p, from)

	override def foldLeft[A](z :A)(op :(A, E) => A) :A = {
		val start = startIndex
		ArrayLikeOps.foldLeft(array, start, start + size)(z)(op)
	}
	override def foldRight[A](z :A)(op :(E, A) => A) :A = {
		val start = startIndex
		ArrayLikeOps.foldRight(array, start, start + size)(z)(op)
	}
	override def reduceLeft[U >: E](op :(U, E) => U) :U = {
		val start = startIndex
		ArrayLikeOps.reduceLeft[U, E](array, start, start + size)(op)
	}
	override def reduceRight[U >: E](op :(E, U) => U) :U = {
		val start = startIndex
		ArrayLikeOps.reduceRight[E, U](array, start, start + size)(op)
	}

	override def foreach[U](f :E => U) :Unit = foreach(0, size)(f)
	override def foreach[U](from :Int, until :Int)(f :E => U) :Unit = {
		val length = size
		val start = startIndex
		val from0 = math.min(length, math.max(from, 0))
		val until0 = math.min(length, math.max(from0, until))
		ArrayLikeOps.foreach(array, start + from0, start + until0)(f)
	}

	override def copyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int =
		copyRangeToArray(xs, start, 0, len)

	override def copyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int =
		if (len <= 0 || start >= xs.length || from >= size)
			0
		else if (start < 0)
			outOfBounds_!(start, xs.length)
		else {
			val from0  = math.min(size, math.max(0, from))
			val copied = math.min(len, math.min(xs.length - start, size - from0))
			ArrayLike.copy(unsafeArray, startIndex + from0, xs, start, copied)
			copied
		}

	override def cyclicCopyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int =
		cyclicCopyRangeToArray(xs, start, 0, len)

	override def cyclicCopyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int =
		if (len <= 0 || from >= size)
			0
		else if (start < 0)
			outOfBounds_!(start, xs.length)
		else {
			val from0  = math.min(size, math.max(0, from))
			val copied = math.min(len, math.min(size - from0, xs.length))
			ArrayLike.cyclicCopyTo(unsafeArray, startIndex + from0, xs, start % xs.length, copied)
			copied
		}
}






private[sugar] trait ArrayLikeWrapper[+C[_], -A[E] <: ArrayLike[E]] {
	/** Wraps the given [[net.noresttherein.sugar.arrays.ArrayLike array-like]].
	  * The collection will share the contents with the array,
	  * and thus any modifications to either will be visible in the other.
	  */
	def wrap[E](array :A[E]) :C[E]

	def isImmutable :Boolean = false
	def isMutable   :Boolean = false
//	def mutability :Mutability = Mutability.Unspecified
}

private[sugar] trait ArrayLikeSliceWrapper[+C[_], -A[E] <: ArrayLike[E]] extends ArrayLikeWrapper[C, A] {
	override def wrap[E](array :A[E]) :C[E] = make(array, 0, array.length)

	/** Wraps the given [[net.noresttherein.sugar.arrays.ArrayLike array-like]] in a collection,
	  * exposing only elements `array(from), ..., array(until - 1)`. The collection will share the contents
	  * with the array, and thus any modifications to either will be visible in the other.
	  */
	def slice[E](array :A[E], from :Int, until :Int) :C[E] = {
		val length = array.length
		if (until <= 0) make(array, 0, 0)
		else if (from >= length) make(array, length, length)
		else if (from <= 0 & until >= length) make(array, 0, length)
		else if (from <= 0) make(array, 0, until)
		else if (until >= length) make(array, from, length - from)
		else if (until <= from) make(array, from, from)
		else make(array, from, until)
	}

	protected def make[E](array :A[E], from :Int, until :Int) :C[E]
}


private[sugar] trait ArrayLikeSliceFactory
                     [+C[E] <: collection.SeqOps[E, collection.Seq, collection.Seq[E]], -A[E] <: ArrayLike[E]]
	extends StrictOptimizedSeqFactory[C] with ArrayLikeSliceWrapper[C, A]
{
	override def from[E](source :IterableOnce[E]) :C[E] = wrap(source.toRefArray.asInstanceOf[A[E]])

	override def empty[E] :C[E] = wrap(Array.emptyObjectArray.asInstanceOf[A[E]])

	override def newBuilder[E] :Builder[E, C[E]] = RefArrayLike.newBuilder[E].mapResult(a => wrap(a.asInstanceOf[A[E]]))
}


private[collections] sealed abstract class ClassTagArrayLikeSliceFactory[+C[E] <: ArrayLikeSlice[E], A[E] <: ArrayLike[E]]
	extends ClassTagIterableFactory[C] with ArrayLikeSliceWrapper[C, A]
{
	override def empty[E :ClassTag] :C[E] = wrap(ArrayFactory.empty[E].asInstanceOf[A[E]])

	def empty[E](elemType :Class[E]) :C[E] = wrap(ArrayFactory.empty(elemType).asInstanceOf[A[E]])

	override def newBuilder[E :ClassTag] :Builder[E, C[E]] =
		ArrayFactory.newBuilder[E].mapResult(array => wrap(array.asInstanceOf[A[E]]))

	def newBuilder[E](elemType :Class[E]) :Builder[E, C[E]] =
		ArrayFactory.newBuilder(elemType).mapResult(array => wrap(array.asInstanceOf[A[E]]))
}


private[collections] sealed abstract class RefArrayLikeSliceFactory[+C[E] <: ArrayLikeSlice[E], A[E] <: RefArrayLike[E]]
	extends ArrayLikeSliceFactory[C, A]
{
	override def empty[E] :C[E] = emptyPrototype.asInstanceOf[C[E]]
	private[this] val emptyPrototype :C[Nothing] =
		make[Nothing](emptyObjectArray.asInstanceOf[A[Nothing]], 0, 0)
}


//private[collections] sealed trait ArrayLikeSliceFactoryDelegate[+C[E] <: ArrayLikeSlice[E], A[E] <: ArrayLike[E]]
//	extends ArrayLikeSliceFactory[C, A]
//{
//	override def of[E](array :A[E]) :C[E] = delegate.of(array)
//	override def of[E](array :A[E], from :Int, until :Int) :C[E] = delegate.of(array, from, until)
//	protected override def make[E](array :A[E], from :Int, until :Int) :C[E] = delegate.of(array, from, until)
//
//	protected val delegate :ArrayLikeSliceFactory[C, A]
//}






/** A window over a range of indices in an `Array`. There is no guarantee that the component type of the array
  * is actually `classOf[A]` - it might be boxed (for value types), a super type (or a subtype) of `A`, or both.
  * @define Coll `ArrayLikeSlice`
  * @define coll array slice
  */
trait ArrayLikeSliceOps[@specialized(ElemTypes) +E, +CC[_], +C]
	extends ArrayIterableOnce[E] with collection.IndexedSeqOps[E, CC, C] with ArraySliceOps[E, CC, C]
{ this :C =>
	private[sugar] final override def unsafeArray :Array[_] = array
//	final def underlying :ArrayLike[E] = array
	protected def array :Array[E @uncheckedVariance]

	override def reverseIterator :Iterator[E] = {
		val start = startIndex
		ReverseArrayIterator.slice(array, start, start + length)
	}

	override def indexOf[U >: E](elem :U, from :Int) :Int =
		ArrayLikeOps.indexOf[U](array, startIndex, length)(elem, from)

	override def lastIndexOf[U >: E](elem :U, end :Int) :Int =
		ArrayLikeOps.lastIndexOf[U](array, startIndex, length)(elem, end)

	override def lastIndexWhere(p :E => Boolean, end :Int) :Int =
		ArrayLikeOps.lastIndexWhere(array, startIndex, length)(p, end)

	override def indexWhere(p :E => Boolean, from :Int) :Int =
		ArrayLikeOps.indexWhere(array, startIndex, length)(p, from)

	override def segmentLength(p :E => Boolean, from :Int) :Int =
		ArrayLikeOps.segmentLength(array, startIndex, length)(p, from)

	override def removed(index :Int) :C = {
		val length = this.length
		if (index < 0 || index > length)
			throw new IndexOutOfBoundsException(index.toString + " out of " + length)
		else {
			val start = startIndex
			val array = this.array
			val res = Array.copyOfRanges(array, start, start + index, array, start + index + 1, start + length)
			makeSpecific(res, 0, length - 1)
		}
	}

	override def removed(from :Int, until :Int) :C = {
		val length = this.length
		if (until <= from | until <= 0 || from >= length)
			this
		else if (from <= 0 & until >= length)
			this
		else if (from <= 0)
			trustedSlice(0, until)
		else if (until >= length)
			trustedSlice(from, length)
		else {
			val array = this.array
			val start = this.startIndex
			val res   = Array.copyOfRanges(array, 0, start + from, array, start + until, start + length)
			makeSpecific(res, 0, length - (until - from))
		}
	}

	protected override def trustedSlice(from :Int, until :Int) :C = {
		val start = startIndex
		makeSpecific(array, start + from, start + until)
	}
	protected def makeSpecific(array :Array[E @uncheckedVariance], from :Int, until :Int) :C
//	protected def make[X](array :Array[X], from :Int, until :Int) :CC[X]
}


/** A window over a range of indices in an `Array`. There is no guarantee that the component type of the array
  * is actually `classOf[A]` - it might be boxed (for value types), a super type (or a subtype) of `A`, or both.
  * @define Coll `ArrayLikeSlice`
  * @define coll array slice
  */
sealed trait ArrayLikeSlice[@specialized(ElemTypes) +E]
	extends collection.IndexedSeq[E] with ArrayLikeSliceOps[E, ArrayLikeSlice, ArrayLikeSlice[E]]
	   with IterableFactoryDefaults[E, ArrayLikeSlice] with Serializable
{
	override def iterableFactory :SeqFactory[ArrayLikeSlice] = ArrayLikeSlice
}


/** $factoryInfo
  * @define Coll `ArrayLikeSlice`
  * @define coll array slice
  */
@SerialVersionUID(Ver)
case object ArrayLikeSlice
	extends SeqFactory.Delegate[ArrayLikeSlice](IRefArraySlice) with ArrayLikeSliceWrapper[ArrayLikeSlice, ArrayLike]
{
	override def from[E](it :IterableOnce[E]) :ArrayLikeSlice[E] = it match {
		case view  :View[E] => from(view.iterator)
		case empty :Iterable[E] if empty.isEmpty => this.empty
		case empty :Iterator[E] if !empty.hasNext => this.empty
		case ArrayLike.Wrapped.Slice(array, from, until) => make(array, from, until)
		case iter  :Iterable[E] => IRefArraySlice.wrap(iter.toIRefArray[E])
		case _ => IRefArraySlice.wrap(it.iterator.toIRefArray)
	}

	protected override def make[E](array :ArrayLike[E], from :Int, until :Int) :ArrayLikeSlice[E] =
		(array match {
			case arr :Array[AnyRef]  => new Impl(arr, from, until)
			case arr :Array[Int]     => new Impl(arr, from, until)
			case arr :Array[Long]    => new Impl(arr, from, until)
			case arr :Array[Double]  => new Impl(arr, from, until)
			case arr :Array[Byte]    => new Impl(arr, from, until)
			case arr :Array[Char]    => new Impl(arr, from, until)
			case arr :Array[Float]   => new Impl(arr, from, until)
			case arr :Array[Short]   => new Impl(arr, from, until)
			case arr :Array[Boolean] => new Impl(arr, from, until)
			case arr :Array[Unit]    => new Impl(arr, from, until)
		}).castParam[E]

	@SerialVersionUID(Ver)
	private sealed class Impl[@specialized(ElemTypes) E] private[collections]
	                         (final override val array :Array[E], offset :Int, len :Int)
		extends collection.AbstractSeq[E]
		   with collection.IndexedSeq[E] with ArrayLikeSlice[E]
	{
		@inline final override def length :Int = len
		private[sugar] final override def startIndex :Int = offset

		override def apply(i :Int) :E =
			if (i < 0 || i >= len)
				outOfBounds_!(i, len)
			else
				array(offset + i)

		protected override def makeSpecific(array :Array[E @uncheckedVariance], from :Int, until :Int) :ArrayLikeSlice[E] =
			new Impl(array, from, until - from)

		private def writeReplace = new Serializable {
			private[this] val data = if (length == array.length) array else array.slice(offset, offset + length)
			private def readResolve = new Impl(data, 0, data.length)
		}
		protected override def className :String = "ArraySlice"
	}
}




/**
  * @define Coll `MutableArrayLikeSlice`
  * @define coll mutable array slice
  */ //required to resolve the cross inheritance problem between ArrayLikeSlice and mutable.IndexedSeq
sealed trait MutableArraySlice[@specialized(ElemTypes) E]
	extends mutable.IndexedSeq[E] with mutable.IndexedSeqOps[E, MutableArraySlice, MutableArraySlice[E]]
	   with ArrayLikeSlice[E] with ArrayLikeSliceOps[E, MutableArraySlice, MutableArraySlice[E]]
	   with IterableFactoryDefaults[E, MutableArraySlice]
{
	private[sugar] override def isMutable = true
	override def iterableFactory :SeqFactory[MutableArraySlice] = MutableArraySlice
}

/** $factoryInfo
  * @define Coll `MutableArrayLikeSlice`
  * @define coll mutable array slice
  */
@SerialVersionUID(Ver)
case object MutableArraySlice
	extends SeqFactory.Delegate[MutableArraySlice](RefArraySlice)
//	   with ArrayLikeSliceFactoryDelegate[MutableArraySlice, MutableArray]
	   with ArrayLikeSliceWrapper[MutableArraySlice, MutableArray]
{
	override def slice[E](array :MutableArray[E], from :Int, until :Int) :MutableArraySlice[E] =
		ArraySlice.slice(array.asSubtype[Array[E]], from, until)

	protected override def make[E](array :MutableArray[E], from :Int, until :Int) :MutableArraySlice[E] =
		ArraySlice.slice(array.asSubtype[Array[E]], from, until)

	override def isMutable = true
}




/**
  * @define Coll `IArrayLikeSlice`
  * @define coll immutable array slice
  */ //required to resolve the cross inheritance problem between ArrayLikeSlice and immutable.IndexedSeq
sealed trait IArrayLikeSlice[@specialized(ElemTypes) +E]
	extends IndexedSeq[E] with IndexedSeqOps[E, IArrayLikeSlice, IArrayLikeSlice[E]]
	   with ArrayLikeSlice[E] with ArrayLikeSliceOps[E, IArrayLikeSlice, IArrayLikeSlice[E]]
	   with IterableFactoryDefaults[E, IArrayLikeSlice]
{
	private[sugar] override def isImmutable :Boolean = true
	override def iterableFactory :SeqFactory[IArrayLikeSlice] = IArrayLikeSlice
	protected override def applyPreferredMaxLength :Int = Int.MaxValue
}

/** $factoryInfo
  * @define Coll `IArrayLikeSlice`
  * @define coll immutable array slice
  */
@SerialVersionUID(Ver)
case object IArrayLikeSlice
	extends SeqFactory.Delegate[IArrayLikeSlice](IRefArraySlice)
	   with ArrayLikeSliceFactory[IArrayLikeSlice, IArrayLike]
{
	override def slice[E](array :IArrayLike[E], from :Int, until :Int) :IArrayLikeSlice[E] =
		IArraySlice.slice(array.asSubtype[IArray[E]], from, until)

	protected override def make[E](array :IArrayLike[E], from :Int, until :Int) :IArrayLikeSlice[E] =
		IArraySlice.slice(array.castFrom[IArrayLike[E], IArray[E]], from, until)

	override def isImmutable = true
//	override def mutability = Mutability.Immutable
}




/**
  * @define Coll `ArraySlice`
  * @define coll array slice
  */
@SerialVersionUID(Ver)
sealed class ArraySlice[@specialized(ElemTypes) E] private[collections]
                       (final override val array :Array[E], offset :Int, len :Int)
	extends mutable.AbstractSeq[E]
	   with mutable.IndexedSeq[E] with mutable.IndexedSeqOps[E, MutableArraySlice, ArraySlice[E]]
	   with MutableArraySlice[E] with ArrayLikeSliceOps[E, MutableArraySlice, ArraySlice[E]]
	   with EvidenceIterableFactoryOverrides[E, ArraySlice, ClassTag]
{
	@inline final override def length :Int = len
	private[sugar] final override def startIndex :Int = offset

	override def apply(i :Int) :E =
		if (i < 0 || i >= len)
			outOfBounds_!(i, len)
		else
			array(offset + i)

	override def update(idx :Int, elem :E) :Unit =
		if (idx < 0 || idx >= len)
			outOfBounds_!(idx, len)
		else
			array(offset + idx) = elem

	protected override def makeSpecific(array :Array[E], from :Int, until :Int) :ArraySlice[E] =
		new ArraySlice(array, from, until - from)

	protected implicit override def iterableEvidence :ClassTag[E] =
		ClassTag(array.getClass.getComponentType.castParam[E])

	protected override def evidenceIterableFactory :EvidenceIterableFactory[ArraySlice, ClassTag] = ArraySlice

	private def writeReplace :Serializable = new ArraySerializationProxy[E](ArraySlice.wrap(_), array, offset, length)

	protected override def className :String = "ArraySlice"
}


/** $factoryInfo
  * @define Coll `ArraySlice`
  * @define coll array slice
  */
@SerialVersionUID(Ver)
case object ArraySlice extends ClassTagArrayLikeSliceFactory[ArraySlice, Array] {
	override def from[E :ClassTag](it :IterableOnce[E]) :ArraySlice[E] = it match {
		case view  :View[E] => from(view.iterator)
		case empty :Iterable[E] if empty.isEmpty => this.empty
		case empty :Iterator[E] if !empty.hasNext => this.empty
		case iter  :Iterable[E] => wrap(iter.toArray)
		case _ => wrap(it.iterator.toArray)
	}

	protected override def make[E](array :Array[E], from :Int, until :Int) :ArraySlice[E] =
		((array :Array[_]) match {
			case arr :Array[AnyRef]  => new ArraySlice(arr, from, until)
			case arr :Array[Int]     => new ArraySlice(arr, from, until)
			case arr :Array[Long]    => new ArraySlice(arr, from, until)
			case arr :Array[Double]  => new ArraySlice(arr, from, until)
			case arr :Array[Byte]    => new ArraySlice(arr, from, until)
			case arr :Array[Char]    => new ArraySlice(arr, from, until)
			case arr :Array[Float]   => new ArraySlice(arr, from, until)
			case arr :Array[Short]   => new ArraySlice(arr, from, until)
			case arr :Array[Boolean] => new ArraySlice(arr, from, until)
			case arr :Array[Unit]    => new ArraySlice(arr, from, until)
		}).castParam[E]

	override def isMutable = true
//	override def mutability = Mutability.Mutable
}




/**
  * @define Coll `IArraySlice`
  * @define coll immutable array slice
  */
@SerialVersionUID(Ver)
sealed class IArraySlice[@specialized(ElemTypes) +E] private[collections]
                        (underlying :IArray[E], offset :Int, len :Int)
	extends AbstractSeq[E] with IndexedSeq[E] with IndexedSeqOps[E, IArrayLikeSlice, IArraySlice[E]]
	   with IArrayLikeSlice[E] with ArrayLikeSliceOps[E, IArrayLikeSlice, IArraySlice[E]]
	   with EvidenceIterableFactoryOverrides[E, IArraySlice, ClassTag]
{
	protected override def array :Array[E @uncheckedVariance] = underlying.asInstanceOf[Array[E]]
	@inline final override def length :Int = len
	private[sugar] final override def startIndex :Int = offset

	override def apply(i :Int) :E =
		if (i < 0 || i >= len)
			outOfBounds_!(i, len)
		else
			array(offset + i)

	override def empty :IArraySlice[E] = IArraySlice.empty[E]

	protected override def makeSpecific(array :Array[E @uncheckedVariance], from :Int, until :Int) :IArraySlice[E] =
		new IArraySlice(array.asInstanceOf[IArray[E]], from, until - from)

	protected implicit override def iterableEvidence :ClassTag[E @uncheckedVariance] =
		ClassTag(underlying.getClass.getComponentType.castParam[E])

	protected override def evidenceIterableFactory :EvidenceIterableFactory[IArraySlice, ClassTag] = IArraySlice


	private def writeReplace :Serializable = new ArraySerializationProxy[E](
		array => IArraySlice.wrap(array.asInstanceOf[IArray[E]]), array, offset, length
	)

	protected override def className :String = "IArraySlice"
}


/** $factoryInfo
  * @define Coll `IArraySlice`
  * @define coll immutable array slice
  */
@SerialVersionUID(Ver)
case object IArraySlice extends ClassTagArrayLikeSliceFactory[IArraySlice, IArray] {
	override def from[E :ClassTag](it :IterableOnce[E]) :IArraySlice[E] = it match {
		case view  :View[E] => from(view.iterator)
		case empty :Iterable[E] if empty.isEmpty => this.empty
		case empty :Iterator[E] if !empty.hasNext => this.empty
		case IArray.Wrapped.Slice(array, from, until) => make(array, from, until)
		case iter  :Iterable[E] => wrap(iter.toArray[E].castFrom[Array[E], IArray[E]])
		case _ => wrap(it.iterator.toArray[E].castFrom[Array[E], IArray[E]])
	}

	protected override def make[E](array :IArray[E], from :Int, until :Int) :IArraySlice[E] =
		(array match {
			case arr :Array[AnyRef]  => new IArraySlice(arr.castCons[IArray], from, until)
			case arr :Array[Int]     => new IArraySlice(arr.castCons[IArray], from, until)
			case arr :Array[Long]    => new IArraySlice(arr.castCons[IArray], from, until)
			case arr :Array[Double]  => new IArraySlice(arr.castCons[IArray], from, until)
			case arr :Array[Byte]    => new IArraySlice(arr.castCons[IArray], from, until)
			case arr :Array[Char]    => new IArraySlice(arr.castCons[IArray], from, until)
			case arr :Array[Float]   => new IArraySlice(arr.castCons[IArray], from, until)
			case arr :Array[Short]   => new IArraySlice(arr.castCons[IArray], from, until)
			case arr :Array[Boolean] => new IArraySlice(arr.castCons[IArray], from, until)
			case arr :Array[Unit]    => new IArraySlice(arr.castCons[IArray], from, until)
		}).castParam[E]

	override def isImmutable = true
//	override def mutability = Mutability.Immutable
}




/**
  * @define Coll `RefArraySlice`
  * @define coll reference array slice
  */
@SerialVersionUID(Ver)
sealed class RefArraySlice[E] private (underlying :RefArray[E], offset :Int, len :Int)
	extends mutable.AbstractSeq[E]
	   with mutable.IndexedSeq[E] with mutable.IndexedSeqOps[E, RefArraySlice, RefArraySlice[E]]
	   with MutableArraySlice[E] with ArrayLikeSliceOps[E, RefArraySlice, RefArraySlice[E]]
	   with IterableFactoryDefaults[E, RefArraySlice]
{
	private[sugar] final override def startIndex :Int = offset
	protected override def array :Array[E] = underlying.castFrom[RefArray[E], Array[E]]
	final override def length :Int = len

	override def apply(i :Int) :E =
		if (i < 0 || i > len)
			outOfBounds_!(i, len)
		else
			underlying(offset + i)

	override def update(idx :Int, elem :E) :Unit =
		if (idx < 0 || idx > len)
			outOfBounds_!(idx, len)
		else
			underlying(offset + idx) = elem

	protected override def makeSpecific(array :Array[E], from :Int, until :Int) :RefArraySlice[E] =
		new RefArraySlice(array.asInstanceOf[RefArray[E]], from, until - from)

	override def iterableFactory :SeqFactory[RefArraySlice] = RefArraySlice


	private def writeReplace :Serializable = new ArraySerializationProxy[E](
		array => RefArraySlice.wrap(array.asInstanceOf[RefArray[E]]), array, offset, length
	)

	protected override def className :String = "RefArraySlice"
}


/** $factoryInfo
  * @define Coll `RefArraySlice`
  * @define coll reference array slice
  */
@SerialVersionUID(Ver)
case object RefArraySlice extends RefArrayLikeSliceFactory[RefArraySlice, RefArray] {
	override def from[E](source :IterableOnce[E]) :RefArraySlice[E] = source match {
		case view  :View[E] => from(view.iterator)
		case empty :Iterable[E] if empty.isEmpty => this.empty
		case empty :Iterator[E] if !empty.hasNext => this.empty
//		case RefArray.Wrapped.Slice(array, from, until) => new RefArraySlice(array, from, until)
		case _ => wrap(source.toBasicOps.toArray[Any].castFrom[Array[Any], RefArray[E]])
	}

	protected def make[E](array :RefArray[E], from :Int, until :Int) :RefArraySlice[E] =
		new RefArraySlice(array, from, until)

	override def isMutable = true
//	override def mutability = Mutability.Mutable
}




/** A view of a slice of an immutable array. This class - or rather, its factory object - is used
  * by [[net.noresttherein.sugar.arrays.IRefArray.Wrapped.Slice IRefArray.Wrapped.Slice]] as a default wrapper.
  * @define Coll `IRefArraySlice`
  * @define coll immutable reference array slice
  */
@SerialVersionUID(Ver)
sealed class IRefArraySlice[+E] private (underlying :IRefArray[E], offset :Int, len :Int)
	extends AbstractSeq[E] with IndexedSeq[E] with IndexedSeqOps[E, IRefArraySlice, IRefArraySlice[E]]
	   with IArrayLikeSlice[E] with ArrayLikeSliceOps[E, IRefArraySlice, IRefArraySlice[E]]
	   with IterableFactoryDefaults[E, IRefArraySlice]
{
	private[sugar] final override def startIndex :Int = offset
	protected override def array :Array[E @uncheckedVariance] = underlying.castFrom[IRefArray[E], Array[E]]
	final override def length :Int = len

	override def apply(i :Int) :E =
		if (i < 0 || i > len)
			outOfBounds_!(i, len)
		else
			underlying(offset + i)

	protected override def makeSpecific(array :Array[E @uncheckedVariance], from :Int, until :Int) :IRefArraySlice[E] =
		new IRefArraySlice(array.asInstanceOf[IRefArray[E]], from, until - from)

	override def iterableFactory :SeqFactory[IRefArraySlice] = IRefArraySlice

	protected override def applyPreferredMaxLength :Int = Int.MaxValue


	private def writeReplace :Serializable = new ArraySerializationProxy[E](
		array => IRefArraySlice.wrap(array.asInstanceOf[IRefArray[E]]), array, offset, length
	)

	protected override def className :String = "IRefArraySlice"
}


/** $factoryInfo
  * @define Coll `IRefArraySlice`
  * @define coll immutable reference array slice
  */
@SerialVersionUID(Ver)
case object IRefArraySlice extends RefArrayLikeSliceFactory[IRefArraySlice, IRefArray] {
	override def from[E](source :IterableOnce[E]) :IRefArraySlice[E] = source match {
		case slice :IRefArraySlice[E]                    => slice
		case view  :View[E]                              => from(view.iterator)
		case empty :Iterable[E] if empty.isEmpty         => this.empty
		case empty :Iterator[E] if !empty.hasNext        => this.empty
		case IRefArray.Wrapped.Slice(array, from, until) => new IRefArraySlice(array, from, until)
		case _ => wrap(source.toBasicOps.toArray[Any].castFrom[Array[Any], IRefArray[E]])
	}

	protected def make[E](array :IRefArray[E], from :Int, until :Int) :IRefArraySlice[E] =
		new IRefArraySlice(array, from, until)

	override def isImmutable = true
//	override def mutability = Mutability.Immutable
}





/** A proxy object designed for being returned from private `writeReplace` method of `Serializable` classes
  * which wrap arrays. In this way, it becomes serialized instead of the proxy class,
  * together with an `Array[E]` holding the serialized container's elements, and a constructor function `constructor`.
  * On deserialization, it is replaced with `constructor(array)`, where `constructor` and `array` are the arguments
  * given on its creation. This strategy is faster than standard collection serialization, as long as
  * the deserialized object can be created in `O(1)` based on an array, because it bypasses the builder.
  */
@SerialVersionUID(Ver)
final class ArraySerializationProxy[A](constructor :Array[A] => Any, array :Array[A])
	extends Serializable
{
	def this(factory :Array[A] => Any, array :Array[A], offset :Int, length :Int) =
		this (factory, if (length == array.length) array else array.slice(offset, offset + length))

	protected[this] def readResolve() :Any = constructor(array)
}
