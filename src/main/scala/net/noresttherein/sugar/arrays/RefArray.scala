package net.noresttherein.sugar.arrays

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

import net.noresttherein.sugar.casting.{castingMethods, castTypeParamMethods}
import net.noresttherein.sugar.collections.{ArrayIterableOnce, MatrixBuffer, RefArraySlice}
import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.reflect.extensions.classNameMethods
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{Yes, No}




/** A factory of `RefArray` - values with `Array` API available as extension methods, represented in runtime
  * always as `Array[AnyRef]`, regardless of their actual type parameter.
  *
  * Note: you can use the standard [[scala.collection.IterableOnceOps.copyToArray copyToArray]] method to copy
  * collections to a `RefArray` by adapting the latter
  * using its [[net.noresttherein.sugar.arrays.RefArray.RefArrayExtension.asAnyArray asAnyArray]].
  * @see [[net.noresttherein.sugar.arrays.RefArray! RefArray]]
  * @define Coll `RefArray`
  * @define coll erased array
  */
@SerialVersionUID(Ver)
case object RefArray extends RefArrayLikeFactory[RefArray] {
	/** Extension methods specific to [[net.noresttherein.sugar.arrays.RefArray RefArray]] only.
	  * These are the standard [[net.noresttherein.sugar.arrays.RefArray.RefArrayExtension.update update]],
	  * as `RefArray` is the only mutable `ArrayLike` other than `Array`, and conversions to immutable sequences,
	  * implemented by low level array copying and `Array`-backed implementations. Enabled by importing any of
	  * `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.RefArrayExtension RefArrayExtension]],
	  * `net.noresttherein.sugar.extensions.RefArrayExtension`, or
	  * `net.noresttherein.sugar.arrays.RefArray.extensions.RefArrayExtension`.
	  * @see [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension]] imported as
	  *      `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.ArrayLikeExtension ArrayLikeExtension]],
	  *      `net.noresttherein.sugar.extensions.ArrayLikeExtension`, or
	  *      `net.noresttherein.sugar.arrays.ArrayLike.extensions.ArrayLikeExtension`.
	  * @see [[net.noresttherein.sugar.arrays.RefArrayLike.RefArrayLikeExtension]] imported as
	  *      `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.RefArrayLikeExtension RefArrayLikeExtension]],
	  *      `net.noresttherein.sugar.extensions.RefArrayLikeExtension`, or
	  *      `net.noresttherein.sugar.arrays.RefArrayLike.extensions.RefArrayLikeExtension`.
	  * @see [[net.noresttherein.sugar.arrays.MutableArray.MutableArrayExtension]] imported as
	  *      `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.MutableArrayExtension MutableArrayExtension]],
	  *      `net.noresttherein.sugar.extensions.MutableArrayExtension`, or
	  *      `net.noresttherein.sugar.arrays.MutableArray.extensions.MutableArrayExtension`.
	  */
	class RefArrayExtension[E] private[arrays](private val self :Array[Any]) extends AnyVal {
		/** The underlying array. This method is particularly useful when passing a `RefArray` to methods
		  * which write to an array, such as `copyToArray`.
		  */
		@inline def asAnyArray :Array[Any] = self

		/** The underlying array. This method is particularly useful when passing a `RefArray` to methods
		  * which write to an array, such as `copyToArray`.
		  */
		@inline final def asAnyRefArray :Array[AnyRef] = self.asInstanceOf[Array[AnyRef]]

		/** Casts this `RefArray` to an immutable `IRefArray`. This method is preferable to using `asInstanceOf`,
		  * and provided here due to a common demand, but it requires the caller to ensure that this array
		  * is no longer modified, or the immutability will be compromised.
		  */
		@inline def asIRefArray :IRefArray[E] = self.asInstanceOf[IRefArray[E]]

		@inline def update(idx :Int, elem :E) :Unit = asAnyArray(idx) = elem

		/** A sequence view on the `[from, until)` index range of this array. Any modification to either this array
		  * or the returned sequence will be visible in the other. Further slicing of the sequence also return
		  * views sharing the same underlying array.
		  */
		@inline def subseq(from :Int, until :Int) :mutable.IndexedSeq[E] =
			Wrapped.Slice(self.asInstanceOf[RefArray[E]], from, until)

		@inline def toSeq        :Seq[E] = toIndexedSeq
		@inline def toIndexedSeq :IndexedSeq[E] = IRefArray.Wrapped(self.toIRefArray.castParam[E])
		@inline def toOps        :mutable.IndexedSeqOps[E, RefArray, RefArray[E]] =
			new RefArrayAsSeq(self.castFrom[Array[Any], RefArray[E]])
	}


	@inline private def expose[X](array :Array[Any]) :RefArray[X] = array.asInstanceOf[RefArray[X]]

	/** A new `Array[AnyRef]` of the specified length, cast to $Coll`[E]`. */
	@throws[NegativeArraySizeException]("if newLength is negative")
	@inline final def ofDim[E](length :Int) :RefArray[E] =
		if (length == 0) empty
		else expose(new Array[Any](length))

	/** Reallocates the given array as a $Coll`[E]` of a new size, and copies `min(newLength - offset, array.length)`
	  * of its first elements to positions starting with `offset`.
	  * If the argument is a value array, the elements will be boxed.
	  */
	@throws[IndexOutOfBoundsException]("if offset is negative")
	@throws[NegativeArraySizeException]("if newLength is negative")
	final def copyOf[E](array :ArrayLike[E], offset :Int, newLength :Int) :RefArray[E] =
		if (offset == 0)
			copyOf(array, newLength)
		else if (newLength == 0)
			empty[E]
		else if (offset < 0 | offset > newLength)
			throw new ArrayIndexOutOfBoundsException(
				toString + ".copyOf(" + array.className + "|" + array.length + "|, " + offset + ", " + newLength + ")"
			)
		else if (newLength <= array.length - offset)
			copyOfRange(array, offset, offset + newLength)
		else {
			val res = new Array[Any](newLength)
			ArrayLike.copy(array, 0, res, offset, math.min(array.length, newLength - offset))
			expose(res)
		}

	/** Creates new $coll of the specified length and copies to it the values in the index range `[from, until)`
	  * from the argument array.
	  * The elements will be copied starting with index `0` in the returned $Coll, in the number being the minimum of
	  * `(until - from, array.length-from, newLength - offset)`. If the argument is a value array, the values
	  * will be boxed.
	  * @param array     The sliced array.
	  * @param from      The index of the element in `array` to be copied as the first element of the new array.
	  *                  Must be in range `[0, array.length]`, or an `IndexOutOfBoundsException` will be thrown.
	  * @param until     The index after the last copied element in `array`. If less than `from`, an empty array is returned.
	  *                  If `until > array.length`, then the new array will contain `until - array.length` `null` elements
	  *                  in its suffix.
	  * @param newLength The length of the created array.
	  * @return An `Array[AnyRef]` of length `until - from` as a $Coll`[E]`, with the copied slice.
	  */
	@throws[NegativeArraySizeException]("if newLength is negative")
	@inline def copyOfRange[E](array :ArrayLike[E], from :Int, until :Int, newLength :Int) :RefArray[E] =
		expose(Array.copyOfRange[Any](array, from, until, newLength)(ClassTag.Any))
//		copyOfRange(array, from, until, 0, newLength)

	/** Copies the elements of `array` in the index range `[from, until)` to a new array with an erased element type.
	  * The elements will be copied to positions starting with `offset`, in the number being the minimum of
	  * `(until - from, array.length-from, newLength - offset)`.
	  * @param array     The sliced array.
	  * @param from      The index of the element in `array` to be copied as the first element of the new array.
	  *                  Must be in range `[0, array.length]`, or an `IndexOutOfBoundsException` will be thrown.
	  * @param until     The index after the last copied element in `array`. If less than `from`, an empty array is returned.
	  *                  If `until > array.length`, then the new array will contain `until - array.length` `null` elements
	  *                  in its suffix.
	  * @param offset    An index in the created array to which the element `array(from)` is copied.
	  * @param newLength The length of the created array.
	  * @return An `Array[AnyRef]` of length `until - from` as a $Coll`[E]`, with the copied slice.
	  */
	@throws[IndexOutOfBoundsException]("if offset is negative")
	@throws[NegativeArraySizeException]("if newLength is negative")
	@inline def copyOfRange[E](array :ArrayLike[E], from :Int, until :Int, offset :Int, newLength :Int) :RefArray[E] =
		expose(Array.copyOfRange[Any](array, from, until, classOf[Any], offset, newLength))

	//consider: new methods for two and three ranges copying to a given offset.

	/** Copies slices from two arrays into a new $coll of a given length. Specifying `until < from`
	  * has the same effect as `until == from`, that is copying nothing. However, `untilX > arrayX.length`
	  * is treated as if the source array were of length `untilX`, and contained zeros/nulls past its actual length.
	  * Element `array1(from1)` is copied to `result(0)`, and so on, until `array2(from2)`
	  * is copied to `result(until1 - from1)` (assuming `until1 >= from1`).
	  * @param array1    The first sliced array.
	  * @param from1     The index of the element in `array1` to be copied as the first element of the new array.
	  * @param until1    The index after the last copied element in `array1`.
	  * @param array2    The second sliced array.
	  * @param from2     The index of the element in `array2` to be copied after `array1(until - 1)` to the new array.
	  * @param until2    The index after the last copied element in `array1`.
	  * @param newLength The length of the created array.
	  * @return A $Coll of length `until1 - from1 + until2 - from2` (for `from1 <= until1 && from2 <= until2`),
	  *         with the copied slices.
	  */
	@throws[NegativeArraySizeException]("if newLength is negative")
	@inline def copyOfRanges[E](array1 :ArrayLike[E], from1 :Int, until1 :Int,
	                            array2 :ArrayLike[E], from2 :Int, until2 :Int, newLength :Int) :RefArray[E] =
		expose(Array.copyOfRanges[Any](array1, from1, until1, array2, from2, until2, newLength)(ClassTag.Any))

	/** Copies slices from three array into a new $coll. Providing `until < from` has the same effect as `until == from`,
	  * that is copying nothing. However, `untilX > arrayX.length` is treated as if the source array
	  * were of length `untilX`, and contained zeros/nulls past its actual length.
	  * Element `array1(from1)` is copied to `result(0)`, and so on, with `array2(from2)`
	  * copied to `result(until1 - from1)`, and `array3(from3)` to `result(until2 - from2 + until1 - from1)`
	  * (assuming `until1 >= from1`).
	  * @param array1    The first sliced array.
	  * @param from1     The index of the element in `array1` to be copied as the first element of the new array.
	  * @param until1    The index after the last copied element in `array1`.
	  * @param array2    The second sliced array.
	  * @param from2     The index of the element in `array2` to be copied after `array1(until1 - 1)` into the new array.
	  * @param until2    The index after the last copied element in `array1`.
	  * @param array3    The third sliced array.
	  * @param from3     The index of the element in `array3` to be copied after `array2(until2 - 1)` into the new array.
	  * @param until3    The index after the last copied element in `array1`.
	  * @param newLength The length of the created array.
	  * @return An `Array[AnyRef]` of length `until1 - from1 + until2 - from2 + until3 - from3` (for `untilN >= fromN`)
	  *         with the copied slices, as a $Coll.
	  */
	@throws[NegativeArraySizeException]("if newLength is negative")
	@inline def copyOfRanges[E](array1 :Array[E], from1 :Int, until1 :Int,
	                            array2 :Array[E], from2 :Int, until2 :Int,
	                            array3 :Array[E], from3 :Int, until3 :Int, newLength :Int) :RefArray[E] = expose(
		Array.copyOfRanges[Any](
			array1, from1, until1, array2, from2, until2, array3, from3, until3, newLength
		)(ClassTag.Any)
	)


	/** Wraps reference arrays in indexed sequences and unwraps all known mutable collections
	  * backed by arrays of `Array[AnyRef]` type in a safe manner. $warning
	  *
	  * @define warning Arrays are represented as [[net.noresttherein.sugar.arrays.RefArray RefArray]];
	  * only 'untagged' collections, that is backed by `Array[AnyRef]`, and not its subclasses, are recognized.
	  * The extractor never attempts to copy a wrapped array, and returns it only if the collection is mutable
	  * (and not just non-immutable). Modifying the returned array will propagate the changes to the wrapping collection.
	  *
	  * Note that due to the array carrying no information about the nominal type of the stored objects,
	  * This pattern is compatible only with invariant mutable sequences in order to guarantee type safety.
	  */
	@SerialVersionUID(Ver)
	object Wrapped {
		def apply[E](array :RefArray[E]) :mutable.IndexedSeq[E] =
			mutable.ArraySeq.make(array.castFrom[RefArray[E], Array[E]])

		def unapply[E](elems :mutable.SeqOps[E, generic.Any, _]) :Maybe[RefArray[E]] = {
			val array = elems match {
				case seq   :mutable.ArraySeq[_]                                     => seq.array
				case seq   :ArrayBuffer[_]                                          => CheatedAccess.array(seq)
				case slice :ArrayIterableOnce[_] if slice.isMutable                 => slice.unsafeArray
				case seq   :MatrixBuffer[_] if seq.dim == 1 && seq.startOffset == 0 => seq.data1
				case _                                                              => null
			}
			if (array != null && array.length == elems.length && array.getClass == classOf[Array[AnyRef]])
				Yes(array.castFrom[Array[_], RefArray[E]])
			else
				No
		}

		/** Wraps and unwraps sections of a `RefArray` in mutable sequences. $warning */
		@SerialVersionUID(Ver)
		object Slice {
			def apply[E](array :RefArray[E], from :Int, until :Int) :mutable.IndexedSeq[E] =
				RefArraySlice.slice(array, from, until)

			def unapply[E](elems :mutable.SeqOps[E, generic.Any, _]) :Maybe[(RefArray[E], Int, Int)] = {
				val length = elems.length
				var start  = 0
				val array  = elems match {
					case seq   :mutable.ArraySeq[_] => seq.array
					case seq   :ArrayBuffer[_]      => CheatedAccess.array(seq)
					case slice :ArrayIterableOnce[_] if slice.isMutable =>
						start = slice.startIndex
						slice.unsafeArray
					case seq   :MatrixBuffer[_] if seq.dim == 1 =>
						val a = seq.data1
						start = seq.startOffset
						if (start <= a.length - length) a else null
					case _ =>
						null
				}
				if (array != null && array.getClass == classOf[Array[AnyRef]])
					Yes((array.castFrom[Array[_], RefArray[E]], start, start + length))
				else
					No
			}
		}
	}
}
