package net.noresttherein.sugar.collections

import scala.Array.emptyObjectArray
import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.unspecialized
import scala.collection.Stepper.EfficientSplit
import scala.collection.immutable.{AbstractSeq, ArraySeq, IndexedSeqOps}
import scala.collection.mutable.{ArrayBuilder, Builder}
import scala.collection.{ClassTagIterableFactory, EvidenceIterableFactory, IterableFactory, IterableFactoryDefaults, IterableOps, SeqFactory, Stepper, StepperShape, StrictOptimizedClassTagSeqFactory, StrictOptimizedIterableOps, StrictOptimizedSeqFactory, View, immutable, mutable}
import scala.reflect.{ClassTag, classTag}

import net.noresttherein.sugar.JavaTypes.JIterator
import net.noresttherein.sugar.arrays.{ArrayCompanionExtension, ArrayFactory, ArrayIterator, ArrayLike, ArrayLikeSpecOps, IArray, IArrayLike, IRefArray, MutableArray, RefArray, RefArrayLike, ReverseArrayIterator}
import net.noresttherein.sugar.casting.{castTypeConstructorMethods, castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.collections.extensions.{IterableExtension, IterableOnceExtension, IteratorExtension}
import net.noresttherein.sugar.concurrent.Fences.releaseFence
import net.noresttherein.sugar.exceptions.outOfBounds_!
import net.noresttherein.sugar.extensions.ClassExtension
import net.noresttherein.sugar.noSuch_!
import net.noresttherein.sugar.reflect.classes




/** A collection backed by an array slice. Extended by the 'proper' `RelayArray` and various `ArrayLike` adapters,
  * but used instead of the latter in pattern matching to decouple code from the `RelayArray` type itself.
  */
private[sugar] trait ArrayIterableOnce[+E] extends Any with IterableOnce[E] {
	//todo: make it ArrayLike[E]
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



//todo: consistent naming between ArrayLikeXxx and ArrayXxx
private[sugar] trait ArraySliceOps[+E, +CC[_], +C]
	extends ArrayIterableOnce[E] with SugaredSlicingOps[E, CC, C] with StrictOptimizedIterableOps[E, CC, C]
{
	@inline private def array :Array[E @uncheckedVariance] = unsafeArray.asInstanceOf[Array[E]]

	def elementType :Class[_] = array.getClass.getComponentType

	override def head :E =
		if (size == 0) noSuch_!(toString + ".head")
		else array(startIndex)

	override def last :E = {
		val len = size
		if (len == 0) noSuch_!(toString + ".last")
		else array(startIndex + len - 1)
	}

	override def iterator :Iterator[E] = ArrayIterator(array, startIndex, size)

	override def jterator[I <: Jterator[_]](implicit shape :JteratorShape[E, I]) :I = {
		val start = startIndex
		Jterator.slice(array, start, start + size)
	}
	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S with EfficientSplit =
		ArrayStepper(array, startIndex, size)


	protected override def segmentLength(p :E => Boolean, from :Int) :Int =
		ArrayLikeSpecOps.segmentLength(array, startIndex, size)(p, from)

	override def find(p :E => Boolean) :Option[E] = {
		val a = array
		val start = startIndex
		ArrayLikeSpecOps.indexWhere(a, start, size)(p, 0) match {
			case -1 => None
			case  i => Some(a(start + i))
		}
	}

	override def foldLeft[A](z :A)(op :(A, E) => A) :A = {
		val start = startIndex
		ArrayLikeSpecOps.foldLeft(array, start, start + size)(z)(op)
	}
	override def foldRight[A](z :A)(op :(E, A) => A) :A = {
		val start = startIndex
		ArrayLikeSpecOps.foldRight(array, start, start + size)(z)(op)
	}
	override def reduceLeft[U >: E](op :(U, E) => U) :U = {
		val start = startIndex
		ArrayLikeSpecOps.reduceLeft[U, E](array, start, start + size)(op)
	}
	override def reduceRight[U >: E](op :(E, U) => U) :U = {
		val start = startIndex
		ArrayLikeSpecOps.reduceRight[E, U](array, start, start + size)(op)
	}

	override def foreach[U](f :E => U) :Unit = foreach(0, size)(f)
	override def foreach[U](from :Int, until :Int)(f :E => U) :Unit = {
		val length = size
		val start = startIndex
		val from0 = math.min(length, math.max(from, 0))
		val until0 = math.min(length, math.max(from0, until))
		ArrayLikeSpecOps.foreach(array, start + from0, start + until0)(f)
	}

	override def filter(p :E => Boolean) :C = fromSpecific(ArrayIterator(array, startIndex, size).filter(p))
	override def filterNot(p :E => Boolean) :C = fromSpecific(ArrayIterator(array, startIndex, size).filterNot(p))

	//Missing: count

	override def copyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int =
		//Implementation inconsistent with IterableOnceOps, but consistent with Vector (check from >= size)
		copyRangeToArray(xs, start, 0, len)

	override def copyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int =
		if (len <= 0 || start >= xs.length || { val s = size; s == 0 | from >= s })
			0
		else if (start < 0)
			outOfBounds_!(start, xs.length)
		else {
			val from0  = math.max(0, from)
			val copied = math.min(len, math.min(xs.length - start, size - from0))
			ArrayLike.copy(unsafeArray, startIndex + from0, xs, start, copied)
			copied
		}

	override def cyclicCopyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int =
		cyclicCopyRangeToArray(xs, start, 0, len)

	override def cyclicCopyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int =
		if (len <= 0 || xs.length == 0 || from >= size)
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
//
//
//
//trait TypedArraySliceOps[+E, +CC[_], +C] extends ArraySliceOps[E, CC, C]




/** A window over a range of indices in an `Array`. There is no guarantee that the component type of the array
  * is actually `classOf[A]` - it might be boxed (for value types), a super type (or a subtype) of `A`, or both.
  * @define Coll `ArrayLikeSlice`
  * @define coll array slice
  * @see [[net.noresttherein.sugar.collections.ArrayLikeSliceFactoryDefaults]]
  */
trait ArraySliceSeqOps[@specialized(ElemTypes) +E, +CC[_], +C]
	extends ArrayIterableOnce[E] with collection.IndexedSeqOps[E, CC, C] with ArraySliceOps[E, CC, C]
{ this :C =>
	private[sugar] final override def unsafeArray :Array[_] = array
	protected def array :Array[E @uncheckedVariance]

	override def reverseIterator :Iterator[E] = {
		val start = startIndex
		ReverseArrayIterator.slice(array, start, start + length)
	}
	//Overridden for specialization.
	override def head :E =
		if (size == 0) noSuch_!(toString + ".head")
		else array(startIndex)

	override def last :E = {
		val len = size
		if (len == 0) noSuch_!(toString + ".last")
		else array(startIndex + len - 1)
	}

	//Not overridden for performance considerations, so a subclass can 1) use direct member field access,
	// and 2) it may be the only implementation for a certain class, which facilitates inlining.
//	override def apply(i :Int) :E =
//		if (i < 0 | i >= length)
//			outOfBounds_!(i, length)
//		else
//			array(startIndex + i)

	override def indexOf[U >: E](elem :U, from :Int) :Int =
		ArrayLikeSpecOps.indexOf[U](array, startIndex, length)(elem, from)

	override def lastIndexOf[U >: E](elem :U, end :Int) :Int =
		ArrayLikeSpecOps.lastIndexOf[U](array, startIndex, length)(elem, end)

	override def lastIndexWhere(p :E => Boolean, end :Int) :Int =
		ArrayLikeSpecOps.lastIndexWhere(array, startIndex, length)(p, end)

	override def indexWhere(p :E => Boolean, from :Int) :Int =
		ArrayLikeSpecOps.indexWhere(array, startIndex, length)(p, from)

	override def segmentLength(p :E => Boolean, from :Int) :Int =
		ArrayLikeSpecOps.segmentLength(array, startIndex, length)(p, from)

	@unspecialized override def filterNot(p :E => Boolean) :C = filter(p, false)
	@unspecialized override def filter(p :E => Boolean) :C = filter(p, true)
	private def filter(p :E => Boolean, keep :Boolean) :C = {
		val res = newSpecificBuilder
		val a = array
		var i = startIndex
		val end = i + length
		while (i < end) {
			val elem = a(i)
			if (p(elem) == keep)
				res += elem
			i += 1
		}
		res.result()
	}
	//todo: implement these here after we change semantics of copyOfRanges to allow gaps.
/*	override def updated[U >: E](index :Int, elem :U) :CC[U] = {
		val elementType = array.getClass.getComponentType
		val newElemType = if (elementType accepts elem.getClass) elementType else classOf[Any].castParam[E]
		val newArray = Array.newL
	}
	override def updatedAll[U >: E](index :Int, elems :U) :CC[U] =
	override def inserted[U >: E](index :Int, elem :U) :CC[U] = {
		val start = startIndex
		val newArray = RefArray.copyOfRanges[U](array, start, start + index + 1, array, start + index, start + length)
		newArray(index) = elem
		newSpecific(newArray, 0, length)
	}
*/
	override def removed(index :Int) :C = {
		val length = this.length
		if (index < 0 || index >= length)
			outOfBounds_!(index.toString + " out of " + length)
		else {
			val start = startIndex
			val array = this.array
			val res = Array.copyOfRanges(array, start, start + index, array, start + index + 1, start + length)
			releaseFence()
			newSpecific(res, 0, length - 1)
		}
	}

	override def removed(from :Int, until :Int) :C = {
		val length = this.length
		if (until <= from | until <= 0 || from >= length)
			coll
		else if (from <= 0 & until >= length)
			empty
		else if (from <= 0)
			clippedSlice(until, length)
		else if (until >= length)
			clippedSlice(0, from)
		else {
			val array = this.array
			val start = this.startIndex
			val res   = Array.copyOfRanges(array, 0, start + from, array, start + until, start + length)
			releaseFence()
			newSpecific(res, 0, length - (until - from))
		}
	}

	protected override def clippedSlice(from :Int, until :Int) :C = {
		val start = startIndex
		newSpecific(array, start + from, start + until)
	}
	protected[this] def newSpecific(array :Array[E], from :Int, until :Int) :C
//	protected def make[X](array :Array[X], from :Int, until :Int) :CC[X]

	//Can't override here: aside from the mutability risk, Seq.toSeq and IndexedSeq.toIndexedSeq are final.
//	override def toSeq :Seq[E] = toIndexedSeq
//	override def toIndexedSeq :IndexedSeq[E] =
//		if (isImmutable) IArrayLikeSlice.slice(array.asInstanceOf[IArrayLike[E]], startIndex, startIndex + length)
//		else IndexedSeq.from(this)
}






/** A factory wrapping $Arr arrays in collection $Coll.
  * This is a limited implementation, which supports only collections backed by entire arrays,
  * such as standard `ArraySeq`. See [[net.noresttherein.sugar.collections.ArrayLikeSliceWrapper ArrayLikeSliceWrapper]]
  * for a factory of collections backed by slices of larger arrays.
  * @see [[net.noresttherein.sugar.collections.ArraySeqFactory ArraySeqFactory]]
  * @define Coll collection
  * @define coll collection
  * @define Arr `ArrayLike`
  * @define arr array-like
  */
trait ArrayLikeWrapper[-A[E] <: ArrayLike[E], +C[_]] extends Serializable {
	/** Wraps the given $Arr in a $Coll. The collection will share the contents with the array,
	  * and thus any modifications to either will be visible in the other.
	  */
	def wrap[E](array :A[E]) :C[E]

	//consider: we could instead have separate IArrayLikeWrapper and MutableArrayWrapper to enforce this relationship,
	// but it would involve tripling every descending trait.
	def isImmutable :Boolean = false
	def isMutable   :Boolean = false
}


/** A factory of $Coll sequences backed by arrays of $Arr kind.
  * Combines the wrapping interface with standard `SeqFactory` for a single type;
  * used to define default array-backed sequences used by the library.
  * @see [[net.noresttherein.sugar.collections.DefaultArraySeq]]
  * @see [[net.noresttherein.sugar.collections.ArraySeqFactory.]]
  */ //consider: replaces ArrayLike with simply Array at least in the XxxFactory traits.
trait ArrayLikeSeqFactory[-A[E] <: ArrayLike[E], +C[E] <: collection.SeqOps[E, collection.Seq, collection.Seq[E]]]
	extends StrictOptimizedSeqFactory[C] with ArrayLikeWrapper[A, C]


/** A factory of $Coll, exposing slices of arrays of $Arr kind. */
trait ArrayLikeSliceWrapper[-A[E] <: ArrayLike[E], +C[_]] extends ArrayLikeWrapper[A, C] {
	override def wrap[E](array :A[E]) :C[E] = make(array, 0, array.length)

	/** Wraps the given $arr in a $coll, exposing only elements `array(from), ..., array(until - 1)`. The $coll
	  * will share the contents with the array, and thus any modifications to either will be visible in the other.
	  * If any of indices in the `[from, until)` range are negative or greater than the array's length, they are ignored.
	  */ //consider: renaming to range
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


/** A factory of $Coll sequences backed by slices of arrays of $Arr kind.
  * Combines the wrapping methods with standard [[scala.collection.SeqFactory SeqFactory]]
  * in order to provide a single interface, which can be used to specify default
  * [[scala.collection.immutable.ArraySeq ArraySeq]]-like implementations used by the library.
  * @see [[net.noresttherein.sugar.collections.DefaultArraySeq]]
  * @see [[net.noresttherein.sugar.collections.ArraySeqFactory.untagged]]
  */
trait ArrayLikeSliceFactory[-A[E] <: ArrayLike[E], +C[E] <: collection.SeqOps[E, collection.Seq, collection.Seq[E]]]
	extends ArrayLikeSeqFactory[A, C] with ArrayLikeSliceWrapper[A, C]


private[sugar] abstract class RefArrayLikeSliceFactory
                              [-A[E] <: RefArrayLike[E], +C[E] <: collection.SeqOps[E, collection.Seq, collection.Seq[E]]]
	extends ArrayLikeSliceFactory[A, C]
{
	override def from[E](source :IterableOnce[E]) :C[E] = source match {
		case view  :View[E] => from(view.iterator)
		case empty :Iterable[E] if empty.isEmpty => this.empty
		case empty :Iterator[E] if !empty.hasNext => this.empty
		case _ => wrap(source.toRefArray.asInstanceOf[A[E]])
	}

//	override def empty[E] :C[E] = wrap(Array.emptyObjectArray.asInstanceOf[A[E]])
	override def empty[E] :C[E] = emptyPrototype.asInstanceOf[C[E]]
	private[this] val emptyPrototype :C[Nothing] = wrap[Nothing](emptyObjectArray.asInstanceOf[A[Nothing]])

	override def newBuilder[E] :Builder[E, C[E]] = RefArrayLike.newBuilder[E].mapResult(a => wrap(a.asInstanceOf[A[E]]))
}


abstract class ClassTagArrayLikeSliceFactory[A[E] <: ArrayLike[E], +C[E]]
	extends ClassTagIterableFactory[C] with ArrayLikeSliceWrapper[A, C]
{
	override def empty[E :ClassTag] :C[E] = wrap(ArrayFactory.empty[E].asInstanceOf[A[E]])

	def empty[E](elemType :Class[E]) :C[E] = wrap(ArrayFactory.empty(elemType).asInstanceOf[A[E]])

	override def newBuilder[E :ClassTag] :Builder[E, C[E]] =
		ArrayFactory.newBuilder[E].mapResult(array => wrap(array.asInstanceOf[A[E]]))

	def newBuilder[E](elemType :Class[E]) :Builder[E, C[E]] =
		ArrayFactory.newBuilder(elemType).mapResult(array => wrap(array.asInstanceOf[A[E]]))
}

abstract class ClassTagArrayLikeSliceSeqFactory
               [A[E] <: ArrayLike[E], +C[E] <: collection.SeqOps[E, collection.Seq, collection.Seq[E]]]
	extends ClassTagArrayLikeSliceFactory[A, C] with StrictOptimizedClassTagSeqFactory[C]






/** An analogue of [[scala.collection.IterableFactoryDefaults IterableFactoryDefaults]],
  * it implements several methods based on its
  * [[net.noresttherein.sugar.collections.ArrayLikeSliceFactoryDefaults.sliceFactory sliceFactory]] property
  * of [[net.noresttherein.sugar.collections.ArrayLikeSliceWrapper ArrayLikeSliceWrapper]].
  * @tparam E The element type of this collection (may or may not be the component type of the underlying array).
  * @tparam A The kind of wrapped array/array-like type.
  * @tparam C The kind of of this collection.
  */
private[sugar] trait ArrayLikeSliceFactoryDefaults
                     [@specialized(ElemTypes) +E, -A[x] <: ArrayLike[x], +C[x] <: collection.IndexedSeq[x]]
	extends ArraySliceSeqOps[E, collection.IndexedSeq, C[E @uncheckedVariance]]
{ this :C[E @uncheckedVariance] => //If ArraySliceSeqOps doesn't have the same self type, this causes a compiler error.

	override def apply(i :Int) :E =
		if (i < 0 || i >= length)
			outOfBounds_!(i, length)
		else
			array(startIndex + i)

	protected def sliceFactory :ArrayLikeSliceWrapper[A, C]

	protected[this] override def newSpecific(array :Array[E], from :Int, until :Int) :C[E @uncheckedVariance] =
		sliceFactory.slice(array.asInstanceOf[A[E]], from, until - from)

	private def writeReplace :Serializable =
		//todo: get rid of this cast by changing unsafeArray to ArrayLike, and then array to A[E]
		new ArraySerializationProxy[A, E](sliceFactory, array.asInstanceOf[A[E]])

	protected[this] override def className :String = sliceFactory.toString
}






/** A window over a range of indices in an `Array`. There is no guarantee that the component type of the array
  * is actually `classOf[A]` - it might be boxed (for value types), a super type (or a subtype) of `A`, or both.
  * @define Coll `ArrayLikeSlice`
  * @define coll array slice
  */
private[sugar] sealed trait ArrayLikeSlice[@specialized(ElemTypes) +E]
	extends collection.IndexedSeq[E] with ArraySliceSeqOps[E, ArrayLikeSlice, ArrayLikeSlice[E]]
	   with IterableFactoryDefaults[E, ArrayLikeSlice] with Serializable
{
	override def iterableFactory :SeqFactory[ArrayLikeSlice] = ArrayLikeSlice
}


/** $factoryInfo
  * @define Coll `ArrayLikeSlice`
  * @define coll array slice
  */
@SerialVersionUID(Ver)
private[sugar] case object ArrayLikeSlice
	extends SeqFactory.Delegate[ArrayLikeSlice](IRefArraySlice) with ArrayLikeSliceWrapper[ArrayLike, ArrayLikeSlice]
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
	                         (final override val array :Array[E], final override val startIndex :Int,
	                          final override val length :Int)
		extends collection.AbstractSeq[E] with collection.IndexedSeq[E]
		   with ArrayLikeSlice[E] with ArrayLikeSliceFactoryDefaults[E, ArrayLike, ArrayLikeSlice]
	{
		protected override def sliceFactory = ArrayLikeSlice
	}
}




/**
  * @define Coll `MutableArrayLikeSlice`
  * @define coll mutable array slice
  */ //required to resolve the cross inheritance problem between ArrayLikeSlice and mutable.IndexedSeq
private[sugar] sealed trait MutableArraySlice[@specialized(ElemTypes) E]
	extends mutable.IndexedSeq[E] with mutable.IndexedSeqOps[E, MutableArraySlice, MutableArraySlice[E]]
	   with ArrayLikeSlice[E] with ArraySliceSeqOps[E, MutableArraySlice, MutableArraySlice[E]]
	   with IterableFactoryDefaults[E, MutableArraySlice]
{
	private[sugar] override def isMutable = true
	override def iterableFactory :SeqFactory[MutableArraySlice] = MutableArraySlice
}

/** $factoryInfo
  * @define Coll `MutableArrayLikeSlice`
  * @define coll mutable array slice
  * @define Arr `MutableArray`
  * @define arr mutable array-like
  */
@SerialVersionUID(Ver)
private[sugar] case object MutableArraySlice
	extends SeqFactory.Delegate[MutableArraySlice](RefArraySlice)
	   with ArrayLikeSliceWrapper[MutableArray, MutableArraySlice]
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
private[sugar] sealed trait IArrayLikeSlice[@specialized(ElemTypes) +E]
	extends IndexedSeq[E] with IndexedSeqOps[E, IArrayLikeSlice, IArrayLikeSlice[E]]
	   with ArrayLikeSlice[E] with ArraySliceSeqOps[E, IArrayLikeSlice, IArrayLikeSlice[E]]
	   with IterableFactoryDefaults[E, IArrayLikeSlice]
{
	private[sugar] override def isImmutable :Boolean = true
	override def iterableFactory :SeqFactory[IArrayLikeSlice] = IArrayLikeSlice
	protected override def applyPreferredMaxLength :Int = Int.MaxValue
}

/** $factoryInfo
  * @define Coll `IArrayLikeSlice`
  * @define coll immutable array slice
  * @define Arr `IArray`
  * @define arr immutable array
  */
@SerialVersionUID(Ver)
private[sugar] case object IArrayLikeSlice
	extends SeqFactory.Delegate[IArrayLikeSlice](IRefArraySlice)
	   with ArrayLikeSliceFactory[IArrayLike, IArrayLikeSlice]
{
	override def slice[E](array :IArrayLike[E], from :Int, until :Int) :IArrayLikeSlice[E] =
		IArraySlice.slice(array.asSubtype[IArray[E]], from, until)

	protected override def make[E](array :IArrayLike[E], from :Int, until :Int) :IArrayLikeSlice[E] =
		IArraySlice.slice(array.castFrom[IArrayLike[E], IArray[E]], from, until)

	override def isImmutable = true
}




/**
  * @define Coll `ArraySlice`
  * @define coll array slice
  */
@SerialVersionUID(Ver)
private[sugar] sealed class ArraySlice[@specialized(ElemTypes) E] private[collections]
                                      (final override val array :Array[E], final override val startIndex :Int,
                                       final override val length :Int)
	extends mutable.AbstractSeq[E]
	   with mutable.IndexedSeq[E] with mutable.IndexedSeqOps[E, MutableArraySlice, ArraySlice[E]]
	   with MutableArraySlice[E] with ArraySliceSeqOps[E, MutableArraySlice, ArraySlice[E]]
//	   with TypedArraySliceOps[E, MutableArraySlice, ArraySlice[E]]
	   with ArrayLikeSliceFactoryDefaults[E, Array, ArraySlice]
	   with EvidenceIterableFactoryOverrides[E, ArraySlice, ClassTag]
{
	override def update(idx :Int, elem :E) :Unit =
		if (idx < 0 || idx >= length)
			outOfBounds_!(idx, length)
		else
			array(startIndex + idx) = elem

	protected[this] override def newSpecific(array :Array[E], from :Int, until :Int) :ArraySlice[E] =
		new ArraySlice(array, from, until - from)

	protected implicit override def iterableEvidence :ClassTag[E] =
		ClassTag(array.getClass.getComponentType.castParam[E])

	protected override def sliceFactory :ArrayLikeSliceWrapper[Array, ArraySlice] = ArraySlice
	protected override def evidenceIterableFactory :EvidenceIterableFactory[ArraySlice, ClassTag] = ArraySlice
}


/** $factoryInfo
  * @define Coll `ArraySlice`
  * @define coll array slice
  * @define Arr `Array`
  * @define arr array
  */
@SerialVersionUID(Ver)
private[sugar] case object ArraySlice extends ClassTagArrayLikeSliceFactory[Array, ArraySlice] {
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
}




/**
  * @define Coll `IArraySlice`
  * @define coll immutable array slice
  */
@SerialVersionUID(Ver)
sealed class IArraySlice[@specialized(ElemTypes) +E] private[collections]
                        (underlying :IArray[E], final override val startIndex :Int, final override val length :Int)
	extends AbstractSeq[E] with IndexedSeq[E] with IndexedSeqOps[E, IArrayLikeSlice, IArraySlice[E]]
	   with IArrayLikeSlice[E] with ArraySliceSeqOps[E, IArrayLikeSlice, IArraySlice[E]]
//	   with TypedArraySliceOps[E, IArrayLikeSlice, IArraySlice[E]]
	   with ArrayLikeSliceFactoryDefaults[E, IArray, IArraySlice]
	   with EvidenceIterableFactoryOverrides[E, IArraySlice, ClassTag]
{
	protected final override val array :Array[E @uncheckedVariance] = underlying.asInstanceOf[Array[E]]
	releaseFence()
	@unspecialized override def slice(from :Int, until :Int) :IArraySlice[E] =
		super[ArraySliceSeqOps].slice(from, until)

	protected override def sliceFactory :ArrayLikeSliceWrapper[IArray, IArraySlice] = IArraySlice


	protected implicit override def iterableEvidence :ClassTag[E @uncheckedVariance] =
		ClassTag(array.getClass.getComponentType.castParam[E])

	protected override def evidenceIterableFactory :EvidenceIterableFactory[IArraySlice, ClassTag] = IArraySlice
}


/** $factoryInfo
  * @define Coll `IArraySlice`
  * @define coll immutable array slice
  * @define Arr `IArrayLike`
  * @define arr immutable array-like
  */
@SerialVersionUID(Ver)
private[sugar] case object IArraySlice extends ClassTagArrayLikeSliceFactory[IArray, IArraySlice] {
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
}




/**
  * @define Coll `RefArraySlice`
  * @define coll reference array slice
  */
@SerialVersionUID(Ver)
private[sugar] sealed class RefArraySlice[E] private
                            (underlying :RefArray[E], final override val startIndex :Int, final override val length :Int)
	extends mutable.AbstractSeq[E]
	   with mutable.IndexedSeq[E] with mutable.IndexedSeqOps[E, RefArraySlice, RefArraySlice[E]]
	   with MutableArraySlice[E] with ArraySliceSeqOps[E, RefArraySlice, RefArraySlice[E]]
	   with ArrayLikeSliceFactoryDefaults[E, RefArray, RefArraySlice]
	   with IterableFactoryDefaults[E, RefArraySlice]
{
	protected final override val array :Array[E] = underlying.castFrom[RefArray[E], Array[E]]

	override def update(idx :Int, elem :E) :Unit =
		if (idx < 0 || idx > length)
			outOfBounds_!(idx, length)
		else
			array(startIndex + idx) = elem

	protected override def sliceFactory :ArrayLikeSliceWrapper[RefArray, RefArraySlice] = RefArraySlice
	override def iterableFactory :SeqFactory[RefArraySlice] = RefArraySlice
}


/** $factoryInfo
  * @define Coll `RefArraySlice`
  * @define coll reference array slice
  * @define Arr `RefArray`
  * @define arr boxed array
  */
@SerialVersionUID(Ver)
private[sugar] case object RefArraySlice extends RefArrayLikeSliceFactory[RefArray, RefArraySlice] {
	protected def make[E](array :RefArray[E], from :Int, until :Int) :RefArraySlice[E] =
		new RefArraySlice(array, from, until)

	override def isMutable = true
}




/** A view of a slice of an immutable array. This class - or rather, its factory object - is used
  * by [[net.noresttherein.sugar.arrays.IRefArray.Wrapped.Slice IRefArray.Wrapped.Slice]] as a default wrapper.
  * @define Coll `IRefArraySlice`
  * @define coll immutable reference array slice
  */
@SerialVersionUID(Ver)
private[sugar] sealed class IRefArraySlice[+E] private
                            (underlying :IRefArray[E], final override val startIndex :Int, final override val length :Int)
	extends AbstractSeq[E] with IndexedSeq[E] with IndexedSeqOps[E, IRefArraySlice, IRefArraySlice[E]]
	   with IArrayLikeSlice[E] with ArraySliceSeqOps[E, IRefArraySlice, IRefArraySlice[E]]
	   with IterableFactoryDefaults[E, IRefArraySlice]
	   with ArrayLikeSliceFactoryDefaults[E, IRefArray, IRefArraySlice]
{
	protected final override val array :Array[E @uncheckedVariance] = underlying.castFrom[IRefArray[E], Array[E]]
	releaseFence()

	override def slice(from :Int, until :Int) :IRefArraySlice[E] =
		super[ArraySliceSeqOps].slice(from, until)

	protected final override def sliceFactory :ArrayLikeSliceWrapper[IRefArray, IRefArraySlice] = IRefArraySlice
	final override def iterableFactory :SeqFactory[IRefArraySlice] = IRefArraySlice

	protected override def applyPreferredMaxLength :Int = Int.MaxValue
}


/** $factoryInfo
  * @define Coll `IRefArraySlice`
  * @define coll immutable reference array slice
  * @define Arr `IRefArray`
  * @define arr immutable boxed array
  */
@SerialVersionUID(Ver)
private[sugar] case object IRefArraySlice extends RefArrayLikeSliceFactory[IRefArray, IRefArraySlice] {
	override def from[E](source :IterableOnce[E]) :IRefArraySlice[E] = source match {
		case slice :IRefArraySlice[E]                    => slice
		case view  :View[E]                              => from(view.iterator)
		case empty :Iterable[E] if empty.isEmpty         => this.empty
		case empty :Iterator[E] if !empty.hasNext        => this.empty
		case IRefArray.Wrapped.Slice(array, from, until) => new IRefArraySlice(array, from, until)
		case _ => wrap(source.toIRefArray)
	}

	protected def make[E](array :IRefArray[E], from :Int, until :Int) :IRefArraySlice[E] =
		new IRefArraySlice(array, from, until - from)

	override def isImmutable = true
}





/** A proxy object designed for being returned from private `writeReplace` method of `Serializable` classes
  * which wrap arrays. In this way, it becomes serialized instead of the proxy class,
  * together with an `Array[E]` holding the serialized container's elements, and a constructor function `constructor`.
  * On deserialization, it is replaced with `constructor(array)`, where `constructor` and `array` are the arguments
  * given on its creation. This strategy is faster than standard collection serialization, as long as
  * the deserialized object can be created in `O(1)` based on an array, because it bypasses the builder.
  */
@SerialVersionUID(Ver)
final class ArraySerializationProxy[A[X] <: ArrayLike[X], +E](constructor :A[E] => Any, array :A[E])
	extends Serializable
{
	def this(factory :ArrayLikeWrapper[A, IterableOnce], array :A[E]) =
		this(factory.wrap(_), array)

	def this(factory :A[E] => Any, array :A[E], offset :Int, length :Int) =
		this(factory, if (length == array.length) array else array.slice(offset, offset + length))

	protected[this] def readResolve() :Any = constructor(array)
}






/** An adapter of [[scala.collection.immutable.ArraySeq$ ArraySeq]] factory to
  * [[net.noresttherein.sugar.collections.ArrayLikeSliceFactory ArrayLikeSliceFactory]] interface,
  * allowing to plug standard `ArraySeq` instead of classes from this package.
  * @define Coll `ArraySeq`
  * @define coll array sequence
  * @define Arr `Array`
  * @define arr array
  */
@SerialVersionUID(Ver)
private[sugar] object ArraySeqFactory extends ClassTagArrayLikeSliceSeqFactory[Array, ArraySeq] {
	override def from[E :ClassTag](it :IterableOnce[E]) :ArraySeq[E] = ArraySeq.from(it)

	protected override def make[E](array :Array[E], from :Int, until :Int) :ArraySeq[E] =
		if (from == 0 && until == array.length) ArraySeq.unsafeWrapArray(array)
		else ArraySeq.unsafeWrapArray(array.slice(from, until))

	override def empty[E :ClassTag] :ArraySeq[E] = (classTag[E].runtimeClass match {
		case ref if !ref.isPrimitive => refSeq
		case classes.Int             => intSeq
		case classes.Long            => longSeq
		case classes.Double          => doubleSeq
		case classes.Byte            => byteSeq
		case classes.Char            => charSeq
		case classes.Float           => floatSeq
		case classes.Short           => shortSeq
		case _                       => refSeq
	}).castParam[E]

	private[this] val refSeq     = ArraySeq.empty[Nothing]
	private[this] val intSeq     = ArraySeq.empty[Int]
	private[this] val longSeq    = ArraySeq.empty[Long]
	private[this] val doubleSeq  = ArraySeq.empty[Double]
	private[this] val byteSeq    = ArraySeq.empty[Double]
	private[this] val charSeq    = ArraySeq.empty[Double]
	private[this] val shortSeq   = ArraySeq.empty[Double]
	private[this] val floatSeq   = ArraySeq.empty[Double]

	override def isImmutable :Boolean = true

	//No sense in extending SeqFactory.Delegate, because ArrayLikeSliceFactory will override it anyway.

	/** Wraps any `Array` in an immutable `ArraySeq`. This implementation allows to make the library use the standard
	  * `ArraySeq` instead of one of its own implementations.
	  * @define Coll `ArraySeq`
	  * @define coll array sequence
	  * @define Arr `Array`
	  * @define arr array
	  */
	object untagged extends SeqFactory.Delegate(ArraySeq.untagged) with ArrayLikeSliceFactory[Array, ArraySeq] {
		protected override def make[E](array :Array[E], from :Int, until :Int) :ArraySeq[E] =
			if (from == 0 && until == array.length) ArraySeq.unsafeWrapArray(array)
			else ArraySeq.unsafeWrapArray(array.slice(from, until))

		override def isImmutable :Boolean = true
		override def toString = "ArraySeq.untagged"
	}

	override def toString = "ArraySeq"
}
