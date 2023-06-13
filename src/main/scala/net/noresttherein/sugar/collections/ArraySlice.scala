package net.noresttherein.sugar.collections

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.Stepper.EfficientSplit
import scala.collection.immutable.{AbstractSeq, ArraySeq, IndexedSeqOps}
import scala.collection.mutable.Builder
import scala.collection.{ClassTagIterableFactory, EvidenceIterableFactory, EvidenceIterableFactoryDefaults, IterableFactoryDefaults, SeqFactory, Stepper, StepperShape, StrictOptimizedSeqFactory, View, mutable}
import scala.reflect.ClassTag

import net.noresttherein.sugar.{??!, outOfBounds_!}
import net.noresttherein.sugar.JavaTypes.JIterator
import net.noresttherein.sugar.collections.extensions.{ArrayLikeExtension, IArrayAsArrayLikeExtension, IRefArrayAsArrayLikeExtension, IRefArrayAsRefArrayLikeExtension, IterableOnceExtension, IteratorObjectExtension, RefArrayAsArrayLikeExtension, RefArrayExtension, StepperObjectExtension}
import net.noresttherein.sugar.extensions.{IterableExtension, IteratorExtension}
import net.noresttherein.sugar.typist.casting.extensions.{castTypeConstructorMethods, castTypeParamMethods, castingMethods}




/** A collection backed by an array slice. Extended by the 'proper' `PassedArray` and various `ArrayLike` adapters,
  *  but used instead of the latter in pattern matching to decouple code from the `PassedArray` type itself.
  */
private[collections] trait AbstractArraySlice[+E] extends IterableOnce[E] {
	private[collections] def unsafeArray :Array[_]
	private[collections] def startIndex :Int
	private[collections] def isImmutable :Boolean = false
}






private[collections] trait ArrayLikeSliceFactory[+C[_], A[E] <: ArrayLike[E]] {
	def of[E](array :A[E]) :C[E] = make(array, 0, array.length)

	def of[E](array :A[E], from :Int, until :Int) :C[E] = {
		val length = array.length
		if (until <= 0) make(array, 0, 0)
		else if (from >= length) make(array, length, length)
		else if (from <= 0 & until >= length) make(array, 0, length)
		else if (from <= 0) make(array, 0, until)
		else if (until >= length) make(array, from, length)
		else if (until <= from) make(array, from, from)
		else make(array, from, until)
	}

	protected def make[E](array :A[E], from :Int, until :Int) :C[E]
}


private[collections] sealed abstract class ClassTagArrayLikeSliceFactory[+C[E] <: ArrayLikeSlice[E], A[E] <: ArrayLike[E]]
	extends ClassTagIterableFactory[C] with ArrayLikeSliceFactory[C, A]
{
	override def empty[E :ClassTag] :C[E] = of(ArrayAsSeq.empty[E].asInstanceOf[A[E]])

	def empty[E](elemType :Class[E]) :C[E] = of(ArrayAsSeq.empty(elemType).asInstanceOf[A[E]])

	override def newBuilder[E :ClassTag] :Builder[E, C[E]] =
		ArrayAsSeq.newBuilder[E].mapResult(array => of(array.asInstanceOf[A[E]]))

	def newBuilder[E](elemType :Class[E]) :Builder[E, C[E]] =
		ArrayAsSeq.newBuilder(elemType).mapResult(array => of(array.asInstanceOf[A[E]]))
}


private[collections] sealed abstract class RefArrayLikeSliceFactory[+C[E] <: ArrayLikeSlice[E], A[E] <: RefArrayLike[E]]
	extends StrictOptimizedSeqFactory[C] with ArrayLikeSliceFactory[C, A]
{
	override def empty[E] :C[E] = emptyPrototype.asInstanceOf[C[E]]
	private[this] val emptyPrototype :C[Nothing] =
		make[Nothing](ErasedArray.empty.castFrom[Array[_], A[Nothing]], 0, 0)

	override def newBuilder[E] :Builder[E, C[E]] = ErasedArray.newBuilder[E].mapResult(arr => of(arr.asInstanceOf[A[E]]))
}


private[collections] sealed trait ArrayLikeSliceFactoryDelegate[+C[E] <: ArrayLikeSlice[E], A[E] <: ArrayLike[E]]
	extends ArrayLikeSliceFactory[C, A]
{
	override def of[E](array :A[E]) :C[E] = delegate.of(array)
	override def of[E](array :A[E], from :Int, until :Int) :C[E] = delegate.of(array, from, until)
	protected override def make[E](array :A[E], from :Int, until :Int) :C[E] = delegate.of(array, from, until)

	protected val delegate :ArrayLikeSliceFactory[C, A]
}




/** A window over a range of indices in an `Array`. There is no guarantee that the component type of the array
  * is actually `classOf[A]` - it might be boxed (for value types), a super type (or a subtype) of `A`, or both.
  * @define Coll `ArrayLikeSlice`
  * @define coll array slice
  */
trait ArrayLikeSliceOps[@specialized(ElemTypes) +E, +CC[_], +C]
	extends AbstractArraySlice[E] with collection.IndexedSeqOps[E, CC, C] with SugaredIterableOps[E, CC, C]
{ this :C =>
	private[collections] final override def unsafeArray :Array[_] = array
//	final def underlying :ArrayLike[E] = array
	protected def array :Array[E @uncheckedVariance]

	override def take(n :Int) :C = slice(0, n)
	override def drop(n :Int) :C = slice(n, length)
	override def takeRight(n :Int) :C = if (n <= 0) empty else slice(length - math.max(n, 0), length)
	override def dropRight(n :Int) :C = if (n <= 0) this else slice(0, length - math.max(n, 0))

	override def slice(from :Int, until :Int) :C = {
		val length = this.length
		if (until <= from | until <= 0 || from >= length) empty
		else if (from <= 0 & until >= length) this
		else if (from <= 0) trustedSlice(0, until)
		else if (until >= length) trustedSlice(from, length)
		else trustedSlice(from, until)
	}

	protected def trustedSlice(from :Int, until :Int) :C

	override def iterator :Iterator[E] = {
		val start = startIndex
		Iterator.slice(array, start, start + length)
	}
	override def reverseIterator :Iterator[E] = {
		val start = startIndex
		Iterator.reverse(array, start, start + length)
	}
	override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[E, I]) :I = {
		val start = startIndex
		JavaIterator.slice(array, start, start + length)
	}
	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S with EfficientSplit = {
		val start = startIndex
		Stepper.over(array, start, start + length)
	}

	override def foreach[U](from :Int, until :Int)(f :E => U) :Unit = {
		val a = array
		val length = this.length
		var i = startIndex + math.min(math.max(from, 0), length)
		val end = startIndex + math.min(length, until)
		while (i < end) {
			f(a(i))
			i += 1
		}
	}

	override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
		if (len <= 0 | start >= xs.length || length == 0)
			0
		else if (start < 0)
			outOfBounds_!(start, xs.length)
		else {
			val copied = math.min(length, math.min(len, xs.length - start))
			ArrayLike.copy(array, startIndex, xs, start, copied)
			copied
		}

	override def copyRangeToArray[B >: E](xs :Array[B], start :Int, from :Int, until :Int) :Int =
		if (until <= from | until <= 0 || start >= xs.length || from >= length)
			0
		else if (start < 0)
			outOfBounds_!(start, xs.length)
		else {
			val from0  = math.min(length, math.max(0, from))
			val until0 = math.min(length, math.max(from0, until))
			val copied = math.min(xs.length - start, until0 - from0)
			ArrayLike.copy(array, startIndex, xs, start, copied)
			copied
		}
}


/** A window over a range of indices in an `Array`. There is no guarantee that the component type of the array
  * is actually `classOf[A]` - it might be boxed (for value types), a super type (or a subtype) of `A`, or both.
  * @define Coll `ArrayLikeSlice`
  * @define coll array slice
  */
sealed trait ArrayLikeSlice[@specialized(ElemTypes) +E]
	extends collection.IndexedSeq[E] with ArrayLikeSliceOps[E, ArrayLikeSlice, ArrayLikeSlice[E]]
	   with IterableFactoryDefaults[E, ArrayLikeSlice]
{
	override def iterableFactory :SeqFactory[ArrayLikeSlice] = ArrayLikeSlice
}


/** $factoryInfo
  * @define Coll `ArrayLikeSlice`
  * @define coll array slice
  */
@SerialVersionUID(Ver)
case object ArrayLikeSlice
	extends SeqFactory.Delegate[ArrayLikeSlice](IRefArraySlice) with ArrayLikeSliceFactory[ArrayLikeSlice, ArrayLike]
{
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

	override def from[E](it :IterableOnce[E]) :ArrayLikeSlice[E] = it match {
		case view  :View[E] => from(view.iterator)
		case empty :Iterable[E] if empty.isEmpty => this.empty
		case empty :Iterator[E] if !empty.hasNext => this.empty
		case ArrayLike.Wrapped.Slice(array, from, until) => make(array, from, until)
		case iter  :Iterable[E] => IRefArraySlice.of(iter.toIRefArray[E])
		case _ => IRefArraySlice.of(it.iterator.toIRefArray)
	}

	@SerialVersionUID(Ver)
	private sealed class Impl[@specialized(ElemTypes) E] private[collections]
	                         (final override val array :Array[E], offset :Int, len :Int)
		extends collection.AbstractSeq[E]
		   with collection.IndexedSeq[E] with ArrayLikeSlice[E]
	{
		@inline final override def length :Int = len
		private[collections] final override def startIndex :Int = offset

		override def apply(i :Int) :E =
			if (i < 0 || i >= len)
				outOfBounds_!(i, len)
			else
				array(offset + i)

		protected override def trustedSlice(from :Int, until :Int) :ArrayLikeSlice[E] =
			new Impl(array, offset + from, until - from)

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
	override def iterableFactory :SeqFactory[MutableArraySlice] = MutableArraySlice
}


/** $factoryInfo
  * @define Coll `MutableArrayLikeSlice`
  * @define coll mutable array slice
  */
@SerialVersionUID(Ver)
case object MutableArraySlice
	extends SeqFactory.Delegate[MutableArraySlice](RefArraySlice)
	   with ArrayLikeSliceFactoryDelegate[MutableArraySlice, Array]
{
	protected override val delegate :ArrayLikeSliceFactory[ArraySlice, Array] = ArraySlice
}




/**
  * @define Coll `ImmutableArrayLikeSlice`
  * @define coll immutable array slice
  */ //required to resolve the cross inheritance problem between ArrayLikeSlice and immutable.IndexedSeq
sealed trait ImmutableArraySlice[@specialized(ElemTypes) +E]
	extends IndexedSeq[E] with IndexedSeqOps[E, ImmutableArraySlice, ImmutableArraySlice[E]]
	   with ArrayLikeSlice[E] with ArrayLikeSliceOps[E, ImmutableArraySlice, ImmutableArraySlice[E]]
	   with IterableFactoryDefaults[E, ImmutableArraySlice]
{
	private[collections] override def isImmutable :Boolean = true
	override def iterableFactory :SeqFactory[ImmutableArraySlice] = ImmutableArraySlice
	protected override def applyPreferredMaxLength :Int = Int.MaxValue
}

/** $factoryInfo
  * @define Coll `ImmutableArrayLikeSlice`
  * @define coll immutable array slice
  */
@SerialVersionUID(Ver)
case object ImmutableArraySlice
	extends SeqFactory.Delegate[ImmutableArraySlice](IRefArraySlice)
	   with ArrayLikeSliceFactoryDelegate[ImmutableArraySlice, IArray]
{
	protected override val delegate :ArrayLikeSliceFactory[ImmutableArraySlice, IArray] = IArraySlice
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
	private[collections] final override def startIndex :Int = offset

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

	protected override def trustedSlice(from :Int, until :Int) :ArraySlice[E] =
		new ArraySlice(array, offset + from, until - from)

	protected implicit override def iterableEvidence :ClassTag[E] =
		ClassTag(array.getClass.getComponentType.castParam[E])

	protected override def evidenceIterableFactory :EvidenceIterableFactory[ArraySlice, ClassTag] = ArraySlice

	private def writeReplace = new Serializable {
		private[this] val data = if (length == array.length) array else array.slice(offset, offset + length)
		private def readResolve = new ArraySlice(data, 0, data.length)
	}
	protected override def className :String = "ArraySlice"
}


/** $factoryInfo
  * @define Coll `ArraySlice`
  * @define coll array slice
  */
@SerialVersionUID(Ver)
case object ArraySlice extends ClassTagArrayLikeSliceFactory[ArraySlice, Array] {
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

	override def from[E :ClassTag](it :IterableOnce[E]) :ArraySlice[E] = it match {
		case view  :View[E] => from(view.iterator)
		case empty :Iterable[E] if empty.isEmpty => this.empty
		case empty :Iterator[E] if !empty.hasNext => this.empty
		case iter  :Iterable[E] => of(iter.toArray)
		case _ => of(it.iterator.toArray)
	}
}


/**
  * @define Coll `IArraySlice`
  * @define coll immutable array slice
  */
@SerialVersionUID(Ver)
sealed class IArraySlice[@specialized(ElemTypes) +E] private[collections]
                        (underlying :IArray[E], offset :Int, len :Int)
	extends AbstractSeq[E] with IndexedSeq[E] with IndexedSeqOps[E, ImmutableArraySlice, IArraySlice[E]]
	   with ImmutableArraySlice[E] with ArrayLikeSliceOps[E, ImmutableArraySlice, IArraySlice[E]]
	   with EvidenceIterableFactoryOverrides[E, IArraySlice, ClassTag]
{
	protected override def array :Array[E @uncheckedVariance] = underlying.asInstanceOf[Array[E]]
	@inline final override def length :Int = len
	private[collections] final override def startIndex :Int = offset

	override def apply(i :Int) :E =
		if (i < 0 || i >= len)
			outOfBounds_!(i, len)
		else
			array(offset + i)

	override def empty :IArraySlice[E] = IArraySlice.empty[E]

	protected override def trustedSlice(from :Int, until :Int) :IArraySlice[E] =
		new IArraySlice(underlying, offset + from, until - from)

	protected implicit override def iterableEvidence :ClassTag[E @uncheckedVariance] =
		ClassTag(underlying.getClass.getComponentType.castParam[E])

	protected override def evidenceIterableFactory :EvidenceIterableFactory[IArraySlice, ClassTag] = IArraySlice

	private def writeReplace = new Serializable {
		private[this] val data =
			if (length == underlying.length) underlying
			else underlying.slice(offset, offset + length)
		private def readResolve = new IArraySlice(data, 0, data.length)
	}
	protected override def className :String = "IArraySlice"
}


/** $factoryInfo
  * @define Coll `IArraySlice`
  * @define coll immutable array slice
  */
@SerialVersionUID(Ver)
case object IArraySlice extends ClassTagArrayLikeSliceFactory[IArraySlice, IArray] {
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

	override def from[E :ClassTag](it :IterableOnce[E]) :IArraySlice[E] = it match {
		case view  :View[E] => from(view.iterator)
		case empty :Iterable[E] if empty.isEmpty => this.empty
		case empty :Iterator[E] if !empty.hasNext => this.empty
		case IArray.Wrapped.Slice(array, from, until) => make(array, from, until)
		case iter  :Iterable[E] => of(iter.toArray[E].castFrom[Array[E], IArray[E]])
		case _ => of(it.iterator.toArray[E].castFrom[Array[E], IArray[E]])
	}
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
	private[collections] final override def startIndex :Int = offset
	protected override def array :Array[E @uncheckedVariance] = underlying.castFrom[RefArray[E], Array[E]]
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

	protected override def trustedSlice(from :Int, until :Int) :RefArraySlice[E] =
		new RefArraySlice(underlying, offset + from, until - from)

	override def iterableFactory :SeqFactory[RefArraySlice] = RefArraySlice

	private def writeReplace = new Serializable {
		private[this] val data =
			if (length == underlying.length) underlying
			else underlying.slice(offset, offset + length)
		private def readResolve = new RefArraySlice(data, 0, data.length)
	}
	protected override def className :String = "RefArraySlice"
}


/** $factoryInfo
  * @define Coll `RefArraySlice`
  * @define coll reference array slice
  */
@SerialVersionUID(Ver)
case object RefArraySlice extends RefArrayLikeSliceFactory[RefArraySlice, RefArray] {
	protected def make[E](array :RefArray[E], from :Int, until :Int) :RefArraySlice[E] =
		new RefArraySlice(array, from, until)

	override def from[E](source :IterableOnce[E]) :RefArraySlice[E] = source match {
		case view  :View[E] => from(view.iterator)
		case empty :Iterable[E] if empty.isEmpty => this.empty
		case empty :Iterator[E] if !empty.hasNext => this.empty
//		case RefArray.Wrapped.Slice(array, from, until) => new RefArraySlice(array, from, until)
		case _ => of(source.toIterableOnceOps.toArray[Any].castFrom[Array[Any], RefArray[E]])
	}
}


/** A view of a slice of an immutable array. This class - or rather, its factory object - is used
  * by [[net.noresttherein.sugar.collections.IRefArray.Wrapped.Slice IRefArray.Wrapped.Slice]] as a default wrapper.
  * @define Coll `IRefArraySlice`
  * @define coll immutable reference array slice
  */
@SerialVersionUID(Ver)
sealed class IRefArraySlice[E] private (underlying :IRefArray[E], offset :Int, len :Int)
	extends AbstractSeq[E] with IndexedSeq[E] with IndexedSeqOps[E, IRefArraySlice, IRefArraySlice[E]]
	   with ImmutableArraySlice[E] with ArrayLikeSliceOps[E, IRefArraySlice, IRefArraySlice[E]]
	   with IterableFactoryDefaults[E, IRefArraySlice]
{
	private[collections] final override def startIndex :Int = offset
	protected override def array :Array[E @uncheckedVariance] = underlying.castFrom[IRefArray[E], Array[E]]
	final override def length :Int = len

	override def apply(i :Int) :E =
		if (i < 0 || i > len)
			outOfBounds_!(i, len)
		else
			underlying(offset + i)

	protected override def trustedSlice(from :Int, until :Int) :IRefArraySlice[E] =
		new IRefArraySlice(underlying, offset + from, until - from)

	override def iterableFactory :SeqFactory[IRefArraySlice] = IRefArraySlice

	protected override def applyPreferredMaxLength :Int = Int.MaxValue

	private def writeReplace = new Serializable {
		private[this] val data =
			if (length == underlying.length) underlying
			else underlying.slice(offset, offset + length)
		private def readResolve = new IRefArraySlice(data, 0, data.length)
	}
	protected override def className :String = "IRefArraySlice"
}


/** $factoryInfo
  * @define Coll `IRefArraySlice`
  * @define coll immutable reference array slice
  */
@SerialVersionUID(Ver)
case object IRefArraySlice extends RefArrayLikeSliceFactory[IRefArraySlice, IRefArray] {
	protected def make[E](array :IRefArray[E], from :Int, until :Int) :IRefArraySlice[E] =
		new IRefArraySlice(array, from, until)

	override def from[E](source :IterableOnce[E]) :IRefArraySlice[E] = source match {
		case slice :IRefArraySlice[E]                    => slice
		case view  :View[E]                              => from(view.iterator)
		case empty :Iterable[E] if empty.isEmpty         => this.empty
		case empty :Iterator[E] if !empty.hasNext        => this.empty
		case IRefArray.Wrapped.Slice(array, from, until) => new IRefArraySlice(array, from, until)
		case _ => of(source.toIterableOnceOps.toArray[Any].castFrom[Array[Any], IRefArray[E]])
	}
}
