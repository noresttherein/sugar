package net.noresttherein.sugar.arrays

import scala.annotation.nowarn
import scala.collection.IterableFactory
import scala.collection.immutable.{ArraySeq, IndexedSeqOps, StrictOptimizedSeqOps}
import scala.collection.mutable.Builder
import scala.reflect.ClassTag

import net.noresttherein.sugar.casting.{castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.collections.ArrayIterableOnce
import net.noresttherein.sugar.concurrent.Fences.releaseFence
import net.noresttherein.sugar.extensions.IterableOnceExtension
import net.noresttherein.sugar.typist.Unknown
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




/** Companion definitions to immutable array-like types, including conversion to and from Scala collections.
  * @see [[net.noresttherein.sugar.arrays.IArrayLike! IArrayLike]]
  * @define Coll `IArrayLike`
  * @define coll immutable array-like
  * @define updatedInfo
  * It allows to introduce multiple changes to an array in a manner safer than starting with a mutable `Array`
  * and casting it to an $Coll, but without a need to chain multiple calls to (extension) methods
  * of `other` (which would require wasteful creation of intermediate results). Additionally,
  * using [[java.util.Arrays Arrays]]`.`[[java.util.Arrays.copyOfRange copyOfRange]] is faster than creating
  * an array filled with default values and copying contents to it afterwards.
  */
@SerialVersionUID(Ver)
case object IArrayLike extends IterableFactory[IArrayLike] {

	@inline private[IArrayLike] def expose[X](array :ArrayLike[X]) :IArrayLike[X] = {
		releaseFence()
		array.asInstanceOf[IArrayLike[X]]
	}

	/** Copies the contents of the given array and returns them as a new, immutable array of the same element type. */
	@inline def copyOf[E](array :ArrayLike[E]) :IArrayLike[E] =
		copyOf(array, array.length)

	/** Reallocates the given array as a $Coll`[E]` of a new size, and copies `min(newLength, array.length)`
	  * of its initial elements. The returned array will have the same underlying element type as the argument.
	  */
	@throws[NegativeArraySizeException]("if newLength is negative")
	@inline def copyOf[E](array :ArrayLike[E], newLength :Int) :IArrayLike[E] =
		expose(ArrayLike.copyOf(array, newLength))

	/** Copies the elements of `array` in the index range `[from, until)` to a new $coll.
	  * @param array The sliced array.
	  * @param from  The index of the element in `array` to be copied as the first element of the new array.
	  * @param until The index after the last copied element in `array`. If less than `from`, an empty $Coll is returned.
	  *              If `until > array.length`, then the new $coll will contain `until - array.length` `null` elements
	  *              in its suffix.
	  * @return An immutable array with the same element type as the argument.
	  */
	@inline def copyOfRange[E](array :ArrayLike[E], from :Int, until :Int) :IArrayLike[E] =
		expose(ArrayLike.copyOfRange(array, from, until))

	/** Copies slices from two array into a new array. Providing `until < from` has the same effect as `until == from`,
	  * that is copying nothing. However, `untilX > arrayX.length` is treated as if the source array
	  * were of length `untilX`, and contained zeros/nulls past its actual length.
	  * Element `array1(from1)` is copied to `result(0)`, and so on, until `array2(from2)`
	  * is copied to `result(until1 - from1)` (assuming `until1 >= from1`).
	  * @param array1 The first sliced array.
	  * @param from1  The index of the element in `array1` to be copied as the first element of the new array.
	  * @param until1 The index after the last copied element in `array1`.
	  * @param array2 The second sliced array.
	  * @param from2  The index of the element in `array2` to be copied after `array1(until - 1)` to the new array.
	  * @param until2 The index after the last copied element in `array1`.
	  * @return An immutable array with the same element type as the argument.
	  */
	@inline def copyOfRanges[E](array1 :ArrayLike[E], from1 :Int, until1 :Int,
	                            array2 :ArrayLike[E], from2 :Int, until2 :Int) :IArrayLike[E] =
		expose(ArrayLike.copyOfRanges(array1, from1, until1, array2, from2, until2))

	/** Copies slices from three array into a new array. Providing `until < from` has the same effect as `until == from`,
	  * that is copying nothing. However, `untilX > arrayX.length` is treated as if the source array
	  * were of length `untilX`, and contained zeros/nulls past its actual length.
	  * Element `array1(from1)` is copied to `result(0)`, and so on, with `array2(from2)`
	  * copied to `result(until1 - from1)`, and `array3(from3)` to `result(until2 - from2 + until1 - from1)`
	  * (assuming `until1 >= from1`).
	  * @param array1 The first sliced array.
	  * @param from1  The index of the element in `array1` to be copied as the first element of the new array.
	  * @param until1 The index after the last copied element in `array1`.
	  * @param array2 The second sliced array.
	  * @param from2  The index of the element in `array2` to be copied after `array1(until1 - 1)` into the new array.
	  * @param until2 The index after the last copied element in `array1`.
	  * @param array3 The third sliced array.
	  * @param from3  The index of the element in `array3` to be copied after `array2(until2 - 1)` into the new array.
	  * @param until3 The index after the last copied element in `array1`.
	  * @return An immutable array with the same element type as the argument.
	  */
	@inline def copyOfRanges[E](array1 :ArrayLike[E], from1 :Int, until1 :Int,
	                            array2 :ArrayLike[E], from2 :Int, until2 :Int,
	                            array3 :ArrayLike[E], from3 :Int, until3 :Int) :IArrayLike[E] =
		expose(ArrayLike.copyOfRanges(array1, from1, until1, array2, from2, until2, array3, from3, until3))


	/** Converts the argument collection to an immutable array. If the collection is a known, immutable array wrapper,
	  * then the wrapped array is returned. The element type of the result
	  * (i.e., the runtime type of the underlying array) is unspecified.
	  */
	override def from[E](source :IterableOnce[E]) :IArrayLike[E] = expose(
		source match {
			case _ if source.knownSize == 0                  => IRefArray.empty
			case Wrapped(array)                              => array
			case ArrayLike.Wrapped.Slice(array, from, until) => array.slice(from, until)
			case items :Iterable[E]                          => items.toIRefArray
			case _                                           => source.iterator.toIRefArray
		}
	)

	override def empty[E] :IArrayLike[E] = emptyPrototype

	private[this] val emptyPrototype :IRefArray[Nothing] = Array.emptyObjectArray.asInstanceOf[IRefArray[Nothing]]

	@inline override def newBuilder[E] :Builder[E, IArrayLike[E]] = IRefArray.newBuilder



	/** Wraps immutable arrays (including [[net.noresttherein.sugar.arrays.IRefArray IRefArray]])
	  * in indexed sequences and extracts arrays from any supported collection backed by an array
	  * (including 'erased' `Array[AnyRef]`) ''iff'' it contains all elements in the array.
	  */
	@SerialVersionUID(Ver)
	object Wrapped {
		def apply[E](array :IArrayLike[E]) :IndexedSeq[E] =
			if (array.getClass == classOf[Array[AnyRef]]) IRefArray.Wrapped(array.asInstanceOf[IRefArray[E]])
			else IArray.Wrapped(array.asInstanceOf[IArray[E]])

		def unapply[E](elems :IterableOnce[E]) :Maybe[IArrayLike[E]] = elems match {
			case seq :ArraySeq[_]            =>
				Yes(seq.unsafeArray.asInstanceOf[IArrayLike[E]])
			case VectorArray(array)          => Yes(array.asInstanceOf[IArrayLike[E]])
			case slice :ArrayIterableOnce[E] =>
				val array = slice.unsafeArray
				if (slice.knownSize == array.length && slice.isImmutable)
					Yes(array.asInstanceOf[IArrayLike[E]])
				else No
			case _ => No
		}

		/** Wraps immutable arrays (including [[net.noresttherein.sugar.arrays.IRefArray IRefArray]])
		  * in indexed sequences exposing a range of indices of said array,
		  * and extracts arrays from any supported collection backed by an array (including 'erased' `Array[AnyRef]`),
		  * together with the information about the index range which is a part in that collection.
		  */
		@SerialVersionUID(Ver)
		object Slice {
			def apply[E](array :IArrayLike[E], from :Int, until :Int) :IndexedSeq[E] =
				if (array.getClass == classOf[Array[AnyRef]])
					IRefArray.Wrapped.Slice(array.asInstanceOf[IRefArray[E]], from, until)
				else
					IArray.Wrapped.Slice(array.asInstanceOf[IArray[E]], from, until)

			def unapply[E](elems :IterableOnce[E]) :Maybe[(IArrayLike[E], Int, Int)] = elems match {
				case seq :ArraySeq[_]   =>
					Yes((seq.unsafeArray.castFrom[Array[_], IArrayLike[E]], 0, seq.unsafeArray.length))
				case VectorArray(array) => Yes(array.castFrom[Array[AnyRef], IArrayLike[E]], 0, array.length)
				case slice :ArrayIterableOnce[E] if elems.knownSize >= 0 && slice.isImmutable =>
					val array = slice.unsafeArray.castFrom[Array[_], IArrayLike[E]]
					val start = slice.startIndex
					Yes((array, start, start + slice.knownSize))
				case _ =>
					No

			}
		}
	}



	/** Extension methods for immutable [[net.noresttherein.sugar.arrays.IArrayLike IArrayLike]]`[E]` subtypes:
	  * [[net.noresttherein.sugar.arrays.IArray! IArray]]
	  * and [[net.noresttherein.sugar.arrays.IRefArray! IRefArray]].
	  * It expands the collection extensions methods in
	  * [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension ArrayLikeExtension]]
	  * by taking advantage of the immutability, and sharing this instance where possible.
	  * Typically imported as
	  * `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.IArrayLikeExtension IArrayLikeExtension]],
	  * but available also as `net.noresttherein.sugar.arrays.IArrayLike.extensions.IArrayLikeExtension` and
	  * `net.noresttherein.sugar.IArrayLikeExtension`).
	  * @see [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension ArrayLikeExtension]] -
	  *      `IArrayLike` extension methods 'inherited' from [[net.noresttherein.sugar.arrays.ArrayLike! ArrayLike]].
	  * @see [[net.noresttherein.sugar.arrays.IArray.IArrayExtension IArrayExtension]] -
	  *      additional extension methods for `IArray`.
	  * @see [[net.noresttherein.sugar.arrays.IRefArray.IRefArrayExtension IRefArrayExtension]] -
	  *      additional extension methods for `IArray`.
	  * @tparam E   the type of the elements in the array (not necessarily the component type of the underlying array!).
	  * @tparam Arr the type constructor of an `ArrayLike` subtype returned by methods which return
	  *             an `Array` in [[collection.ArrayOps ArrayOps]] (and the `ArrayLike` subtype being granted
	  *             these methods).
	  */
	class IArrayLikeExtension[+Arr[X] <: IArrayLike[X], E] private[arrays] (private val self :Array[Unknown])
		extends AnyVal
	{
		@inline private def exposed :Arr[E] = self.asInstanceOf[Arr[E]]

		def iterator :Iterator[E] = IArrayLikeIterator(exposed)
		def reverseIterator :Iterator[E] = ReverseIArrayLikeIterator(exposed)

		/** A view on the index range `[from, until)` of this array as a sequence.
		  * Slicing of the returned sequence will return similar views, sharing the same underlying array.
		  */
		@inline def subseq(from :Int, until :Int) :IndexedSeq[E] = Wrapped.Slice(exposed, from, until)

		@inline def toSeq :Seq[E] = IArrayLike.Wrapped(exposed)
		@inline def toIndexedSeq :IndexedSeq[E] = IArrayLike.Wrapped(exposed)
		@inline def toArraySeq :ArraySeq[E] = ArraySeq.unsafeWrapArray(self).castParam[E]
		@inline def toOps :IndexedSeqOps[E, IArrayLike, IArrayLike[E]] = new IArrayLikeAsSeq(exposed)
	}

}







private abstract class IArrayLikeIsSeqOps[E, A[X] <: IArrayLike[X]](array :A[E])
	extends ArrayLikeIsSeqOps[E, A](array.asInstanceOf[Array[E]])
	   with StrictOptimizedSeqOps[E, Seq, A[E]] with IndexedSeqOps[E, Seq, A[E]]
{
	override def isImmutable = true
	@nowarn("cat=deprecation")
	override def toIterable      = IArrayLike.Wrapped(coll)
	override def iterator        = IArrayLikeIterator(coll)
	override def iterableFactory = IndexedSeq
}
