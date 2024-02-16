package net.noresttherein.sugar.arrays

import java.lang.System.arraycopy
import java.lang.{Math => math}
import java.util.Arrays

import scala.Array.UnapplySeqWrapper
import scala.annotation.nowarn
import scala.collection.Searching.SearchResult
import scala.collection.Stepper.EfficientSplit
import scala.collection.{ArrayOps, Factory, IndexedSeqView, IterableFactory, LazyZip2, SeqFactory, Stepper, StepperShape, immutable, mutable}
import scala.collection.generic.IsSeq
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.{ArrayBuffer, Buffer, Builder}
import scala.reflect.{ClassTag, classTag}

import net.noresttherein.sugar.casting.{cast2TypeParamsMethods, cast3TypeParamsMethods, castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.collections.{ArrayIterableOnce, ArrayLikeSlice, ArrayStepper, ElementIndex, MatrixBuffer}
import net.noresttherein.sugar.collections.ElementIndex.{indexOfNotFound, indexOfSliceNotFound, indexWhereNotFound, lastIndexOfNotFound, lastIndexOfSliceNotFound, lastIndexWhereNotFound}
import net.noresttherein.sugar.collections.extensions.StepperCompanionExtension
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.concurrent.Fences.releaseFence
import net.noresttherein.sugar.exceptions.{IncompatibleArgumentTypesException, outOfBounds_!}
import net.noresttherein.sugar.extensions.ClassExtension
import net.noresttherein.sugar.reflect.prettyprint.extensions.classNameMethods
import net.noresttherein.sugar.typist.Unknown
import net.noresttherein.sugar.vars.{IntOpt, Maybe}
import net.noresttherein.sugar.vars.IntOpt.{AnInt, NoInt}
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




/** Companion object of all array-like types in the package.
  * Provides various utility methods, in particular for copying arrays, mirroring those defined
  * in [[scala.Array$ Array]] and [[java.util.Arrays]], as well as wrapping and unwrapping arrays to Scala collections.
  * @see [[net.noresttherein.sugar.arrays.ArrayLike! ArrayLike]]
  * @define Coll `ArrayLike`
  * @define coll array-like
  * @define LUBComponentTypeInfo
  * If the actual classes of the argument arrays are not the same, then the returned array
  * will use the least common superclass of their element types for its element types.
  * If the element type of either array is a value type - and they are not equal -
  * then the returned array will box it to `AnyRef`, which will be used as the common upper bound.
  * In case no single minimal upper bound exists, due to element classes extending several shared, but unrelated
  * traits, an exception will be thrown.
  */
@SerialVersionUID(Ver)
case object ArrayLike extends IterableFactory.Delegate[ArrayLike](RefArray) {

	/** Creates an exact copy of `original`. May return the same object if `original.length == 0`. */
	@inline def copyOf[A[X] <: ArrayLike[X], E](original :A[E]) :A[E] = {
		val a = original.asInstanceOf[Array[_]]
		val l = a.length
		if (l == 0) original else Array.copyOf(a, l).asInstanceOf[A[E]]
	}

	/** Creates a new array of the specified length and the same element type as `original`,
	  * and copies all elements of `original` to it (or `newLength`, whichever is lesser).
	  */
	@throws[NegativeArraySizeException]("if newLength is negative")
	def copyOf[A[X] <: ArrayLike[X], E](original :A[E], newLength :Int) :A[E] = {
		val a = original.asInstanceOf[Array[_]]
		if (a.length == 0)
			Array.emptyLike(original.castFrom[ArrayLike[E], Array[_]]).castFrom[Array[_], A[E]]
		else
			Array.copyOf(a, newLength).castFrom[Array[_], A[E]]
	}


//	def copyAs[E :ClassTag](src :ArrayLike[_], newLength :Int) :Array[E] =
//		ArrayFactory.copyAs[E](src, newLength)
//
//	@inline final def copyAs[E :ClassTag](src :ArrayLike[_]) :Array[E] =
//		copyAs[E](src, src.asInstanceOf[Array[_]].length)


	/** Creates an array of the same type as the argument, containing its elements from index range `[from, until)`. */
	def copyOfRange[A[X] <: ArrayLike[X], E](original :A[E], from :Int, until :Int) :A[E] =
		Array.copyOfRange(original.asInstanceOf[Array[E]], from, until).asInstanceOf[A[E]]

	/** Creates a copy of a slice of the specified array, truncating or padding it to the desired length. */
	def copyOfRange[A[X] <: ArrayLike[X], E](original :A[E], from :Int, until :Int, newLength :Int) :A[E] =
		Array.copyOfRange(original.asInstanceOf[Array[E]], from, until, newLength).asInstanceOf[A[E]]

	/** Creates a new array of the same type as the argument, and copies the data from range `[from, until)`
	  *  of the argument to the new array, starting at index `offset`.
	  */ //consider: maybe we should throw an exception if from, until, out of range or offset > newLength?
	@throws[IndexOutOfBoundsException]("if offset is less than zero")
	@throws[NegativeArraySizeException]("if newLength is less than zero")
	def copyOfRange[A[X] <: ArrayLike[X], E](original :A[E], from :Int, until :Int, offset :Int, newLength :Int) :A[E] =
		Array.copyOfRange(original.asInstanceOf[Array[Unknown]], from, until, offset, newLength).asInstanceOf[A[E]]

	/** Copies slices from two array into a new array. Providing `until < from` has the same effect as `until == from`,
	  * that is copying nothing. However, `untilX > arrayX.length` is treated as if the source array
	  * were of length `untilX`, and contained zeros/nulls past its actual length.
	  * Element `array1(from1)` is copied to `result(0)`, and so on, until `array2(from2)`
	  * is copied to `result(until1 - from1)` (assuming `until1 >= from1`).
	  *
	  * $LUBComponentTypeInfo
	  * @param array1 The first sliced array.
	  * @param from1  The index of the element in `array1` to be copied as the first element of the new array.
	  * @param until1 The index after the last copied element in `array1`.
	  * @param array2 The second sliced array.
	  * @param from2  The index of the element in `array2` to be copied after `array1(until - 1)` to the new array.
	  * @param until2 The index after the last copied element in `array1`.
	  * @return An array of length `until1 - from1 + until2 - from2` (for `from1 <= until1 && from2 <= until2`),
	  *         of the same type as the arguments.
	  */
	@throws[IncompatibleArgumentTypesException]("if there is no least upper bound of the arrays' component types.")
	def copyOfRanges[A[X] <: ArrayLike[X], E](array1 :A[E], from1 :Int, until1 :Int,
	                                          array2 :A[E], from2 :Int, until2 :Int) :A[E] =
	{
		val a1 = array1.asInstanceOf[Array[Unknown]]
		val a2 = array2.asInstanceOf[Array[Unknown]]
		if (array1.getClass == array2.getClass)
			Array.copyOfRanges(a1, from1, until1, a2, from2, until2).asInstanceOf[A[E]]
		else {
			val cls1 = array1.getClass.getComponentType
			val cls2 = array2.getClass.getComponentType
			val elemType = elementType[E](cls1, cls2)
			Array.copyOfRanges(array1, from1, until1, array2, from2, until2, elemType).asInstanceOf[A[E]]
		}
	}

	/** Copies slices from two array into a new array, truncating or padding it to the requested length.
	  * Providing `until < from` has the same effect as `until == from`,
	  * that is copying nothing. However, `untilX > arrayX.length` is treated as if the source array
	  * were of length `untilX`, and contained zeros/nulls past its actual length.
	  * Element `array1(from1)` is copied to `result(0)`, and so on, until `array2(from2)`
	  * is copied to `result(until1 - from1)` (assuming `until1 >= from1`).
	  *
	  * $LUBComponentTypeInfo
	  * @param array1 The first sliced array.
	  * @param from1  The index of the element in `array1` to be copied as the first element of the new array.
	  * @param until1 The index after the last copied element in `array1`.
	  * @param array2 The second sliced array.
	  * @param from2  The index of the element in `array2` to be copied after `array1(until - 1)` to the new array.
	  * @param until2 The index after the last copied element in `array1`.
	  * @return An array of length `until1 - from1 + until2 - from2` (for `from1 <= until1 && from2 <= until2`),
	  *         of the same type as the arguments.
	  */
	@throws[IncompatibleArgumentTypesException]("if there is no least upper bound of the arrays' component types.")
	@throws[NegativeArraySizeException]("if newLength is negative.")
	def copyOfRanges[A[X] <: ArrayLike[X], E](array1 :A[E], from1 :Int, until1 :Int,
	                                          array2 :A[E], from2 :Int, until2 :Int, newLength :Int) :A[E] =
	{
		val a1 = array1.asInstanceOf[Array[Unknown]]
		val a2 = array2.asInstanceOf[Array[Unknown]]
		if (array1.getClass == array2.getClass)
			Array.copyOfRanges(a1, from1, until1, a2, from2, until2, newLength).asInstanceOf[A[E]]
		else {
			val cls1 = array1.getClass.getComponentType
			val cls2 = array2.getClass.getComponentType
			val elemType = elementType[E](cls1, cls2)
			Array.copyOfRanges(array1, from1, until1, array2, from2, until2, elemType, newLength).asInstanceOf[A[E]]
		}
	}

	/** Copies slices from three array into a new array. Providing `until < from` has the same effect as `until == from`,
	  * that is copying nothing. However, `untilX > arrayX.length` is treated as if the source array
	  * where of length `untilX`, and contained zeros/nulls past its actual length.
	  * Element `array1(from1)` is copied to `result(0)`, and so on, with `array2(from2)`
	  * copied to `result(until1 - from1)`, and `array3(from3)` to `result(until2 - from2 + until1 - from1)`
	  * (assuming `until1 >= from1`).
	  *
	  * $LUBComponentTypeInfo
	  * @param array1 The first sliced array.
	  * @param from1  The index of the element in `array1` to be copied as the first element of the new array.
	  * @param until1 The index after the last copied element in `array1`.
	  * @param array2 The second sliced array.
	  * @param from2  The index of the element in `array2` to be copied after `array1(until1 - 1)` into the new array.
	  * @param until2 The index after the last copied element in `array1`.
	  * @param array3 The third sliced array.
	  * @param from3  The index of the element in `array3` to be copied after `array2(until2 - 1)` into the new array.
	  * @param until3 The index after the last copied element in `array1`.
	  * @return An array of length `until1 - from1 + until2 - from2` (for `from1 <= until1 && from2 <= until2`),
	  *         of the same type as the arguments.
	  */
	@throws[IncompatibleArgumentTypesException]("if there is no least upper bound of the arrays' component types.")
	def copyOfRanges[A[X] <: ArrayLike[X], E](array1 :A[E], from1 :Int, until1 :Int,
	                                          array2 :A[E], from2 :Int, until2 :Int,
	                                          array3 :A[E], from3 :Int, until3 :Int) :A[E] =
	{
		val a1 = array1.asInstanceOf[Array[Unknown]]
		val a2 = array2.asInstanceOf[Array[Unknown]]
		val a3 = array3.asInstanceOf[Array[Unknown]]
		if (array1.getClass == array2.getClass && array2.getClass == array3.getClass)
			Array.copyOfRanges(a1, from1, until1, a2, from2, until2, a3, from3, until3).asInstanceOf[A[E]]
		else {
			val cls1 = array1.getClass.getComponentType
			val cls2 = array2.getClass.getComponentType
			val cls3 = array3.getClass.getComponentType
			val elemType = elementType[E](cls1, cls2, cls3)
			Array.copyOfRanges(
				array1, from1, until1, array2, from2, until2, array3, from3, until3, elemType
			).asInstanceOf[A[E]]
		}
	}

	/** Copies slices from three array into a new array, truncating or padding it to the requested length.
	  * Providing `until < from` has the same effect as `until == from`,
	  * that is copying nothing. However, `untilX > arrayX.length` is treated as if the source array
	  * where of length `untilX`, and contained zeros/nulls past its actual length.
	  * Element `array1(from1)` is copied to `result(0)`, and so on, with `array2(from2)`
	  * copied to `result(until1 - from1)`, and `array3(from3)` to `result(until2 - from2 + until1 - from1)`
	  * (assuming `until1 >= from1`).
	  *
	  * $LUBComponentTypeInfo
	  * @param array1 The first sliced array.
	  * @param from1  The index of the element in `array1` to be copied as the first element of the new array.
	  * @param until1 The index after the last copied element in `array1`.
	  * @param array2 The second sliced array.
	  * @param from2  The index of the element in `array2` to be copied after `array1(until1 - 1)` into the new array.
	  * @param until2 The index after the last copied element in `array1`.
	  * @param array3 The third sliced array.
	  * @param from3  The index of the element in `array3` to be copied after `array2(until2 - 1)` into the new array.
	  * @param until3 The index after the last copied element in `array1`.
	  * @return An array of length `until1 - from1 + until2 - from2` (for `from1 <= until1 && from2 <= until2`),
	  *         of the same type as the arguments.
	  */
	@throws[IncompatibleArgumentTypesException]("if there is no least upper bound of the arrays' component types.")
	@throws[NegativeArraySizeException]("if newLength is negative.")
	def copyOfRanges[A[X] <: ArrayLike[X], E](array1 :A[E], from1 :Int, until1 :Int,
	                                          array2 :A[E], from2 :Int, until2 :Int,
	                                          array3 :A[E], from3 :Int, until3 :Int, newLength :Int) :A[E] =
	{
		val a1 = array1.asInstanceOf[Array[Unknown]]
		val a2 = array2.asInstanceOf[Array[Unknown]]
		val a3 = array3.asInstanceOf[Array[Unknown]]
		if (array1.getClass == array2.getClass && array2.getClass == array3.getClass)
			Array.copyOfRanges(a1, from1, until1, a2, from2, until2, a3, from3, until3, newLength).asInstanceOf[A[E]]
		else {
			val cls1 = array1.getClass.getComponentType
			val cls2 = array2.getClass.getComponentType
			val cls3 = array3.getClass.getComponentType
			val elemType = elementType[E](cls1, cls2, cls3)
			Array.copyOfRanges(
				array1, from1, until1, array2, from2, until2, array3, from3, until3, elemType, newLength
			).asInstanceOf[A[E]]
		}
	}


	private def elementType[E](cls1 :Class[_], cls2 :Class[_]) :Class[E] =
		(cls1 commonSuperclass cls2) match {
			case Yes(superclass :Class[_])                 => superclass.castParam[E]
			case _ if cls1.isPrimitive || cls2.isPrimitive => classOf[Any].castParam[E] //E must be `Any`
			case _ => throw IncompatibleArgumentTypesException(
				"No least common superclass of " + cls1.getName + " and " + cls2.getName + "."
			)
		}

	private def elementType[E](cls1 :Class[_], cls2 :Class[_], cls3 :Class[_]) :Class[E] = {
		val res =
			if (cls1.isPrimitive || cls2.isPrimitive || cls3.isPrimitive)
				classOf[Any].castParam[E]
			else {
				def findSuperclass(superclass :Class[_], candidate :Class[_]) :Class[_] =
					if (superclass.isAssignableFrom(cls2) && superclass.isAssignableFrom(cls3))
						if (superclass isAssignableFrom candidate) candidate
						else if (candidate isAssignableFrom superclass) superclass
						else null
					else {
						val sup    = superclass.getSuperclass
						var best   =
							if (sup == null) candidate
							else findSuperclass(sup, candidate)
						val traits = superclass.getInterfaces
						var i      = traits.length
						while (best != null & i > 0) {
							i -= 1
							best = findSuperclass(traits(i), best)
						}
						best
					}
				val lub = findSuperclass(cls1, classOf[Any])
				if (lub == null) null else lub.castParam[E]
			}
		if (res == null)
			throw IncompatibleArgumentTypesException(
				"Classes " + cls1.getName + "," + cls2.getName + ", " + cls3.getName + " do not have a least superclass."
			)
		res
	}



	//Array.copy resorts to slowcopy if not classOf[E] >:> array.getClass.getComponentType.
	// We instead always default to System.arraycopy if both element types are reference types.
	/** Copies contents of one array-like object to another, mutable, array-like.
	  * Has the same semantics as `Array.`[[scala.Array.copy copy]], but works or any `ArrayLike`,
	  * and reverts on a fast path to `System.`[[java.lang.System.arraycopy arraycopy]] always if both objects
	  * are reference (not value type) arrays (unlike `Array.copy`, which does it only if the array types
	  * statically guarantee success). It is also faster for copying between a value array and a reference array
	  * in both directions (unless `Array.copy` is heavily inlined to the calling method by the hotspot compiler).
	  */
	@throws[IndexOutOfBoundsException]("if [srcPos, srcPos+length) is not a valid index range in src," +
	                                   "or [dstPos, dstPos+length) is not a valid index range in dst.")
	@throws[ArrayStoreException]("if the dst array cannot store some element from the specified range of src," +
	                             "even after auto boxing/auto unboxing.")
	@inline def copy(src :ArrayLike[_], srcPos :Int, dst :MutableArray[_], dstPos :Int, length :Int) :Unit =
		if (length > 0)
			if (src.isInstanceOf[Array[AnyRef]])
				if (dst.isInstanceOf[Array[AnyRef]])
					arraycopy(src, srcPos, dst, dstPos, length)
				else
					ArrayLikeSpecOps.unboxingCopy(
						src.asInstanceOf[Array[_]], srcPos, dst.asInstanceOf[Array[_]], dstPos, length
					)
			else
				if (dst.isInstanceOf[Array[AnyRef]])
					ArrayLikeSpecOps.boxingCopy(
						src.asInstanceOf[Array[_]], srcPos, dst.asInstanceOf[Array[_]], dstPos, length
					)
				else if (src.getClass == dst.getClass)
					arraycopy(src, srcPos, dst, dstPos, length)
				else //This will fail, but I don't want to be the one to do it.
					Array.copy(src, srcPos, dst, dstPos, length)

	/** Copies a maximum of `length` elements from one array-like to another, wrapping at array ends.
	  * Reading starts with index `srcPos` in `src`, and writing starts with index `dstPos` in `dst`.
	  * If an end of either array is reached, reading/writing resumes from the beginning of that array.
	  * This method will never copy the same element twice, or overwrite previously written elements.
	  */
	@throws[IndexOutOfBoundsException]("if srcPos is not in the [0, src.length) range, " +
	                                   "or dstPos is not in the [0, dst.length) range, " +
	                                   "or len > min(src.length, dst.length).")
	@throws[ArrayStoreException]("if any of elements copied from src cannot be stored in dst," +
	                             "including boxing and unboxing.")
	@inline def cyclicCopy(src :ArrayLike[_], srcPos :Int, dst :MutableArray[_], dstPos :Int, length :Int) :Unit =
		Array.cyclicCopy(src.asInstanceOf[Array[_]], srcPos, dst.asInstanceOf[Array[_]], dstPos, length)

	/** Copies `min(length, src.length, dst.length - dstPos)` elements from one array to another, wrapping at the end
	  * of the source array. Reading starts with index `srcPos` in `src`,
	  * and writing starts with index `dstPos` in `dst`. If the end of array `src` is reached before
	  * reaching the end of `dst` or copying `len` elements, then copying of the remainder restarts with `src(0)`.
	  * If the end of array `dst` reached before copying `length` elements, copying stops.
	  */
	@throws[IndexOutOfBoundsException]("if srcPos is not in the [0, src.length) range, " +
	                                   "or dstPos is not in the [0, dst.Length - len) range, or len > src.length.")
	@throws[ArrayStoreException]("if any of elements copied from src cannot be stored in dst, " +
	                             "including after boxing and unboxing.")
	@inline def cyclicCopyFrom(src :ArrayLike[_], srcPos :Int, dst :MutableArray[_], dstPos :Int, length :Int) :Unit =
		Array.cyclicCopyFrom(src.asInstanceOf[Array[_]], srcPos, dst.asInstanceOf[Array[_]], dstPos, length)

	/** Copies `min(len, src.length - srcPos, dst.length)` elements from one array-like to another, wrapping at the end
	  * of the target array. Reading starts with index `srcPos` in `src`, and writing starts with index `dstPos`
	  * in `dst`. If the end of array `dst` is reached before reaching the end of `src` or copying `len` elements,
	  * then writing of the remainder restarts with `dst(0)`. If the end of array `src` is reached
	  * before copying `length` elements, the copying stops.
	  */
	@throws[IndexOutOfBoundsException]("if srcPos is not in the [0, src.length - len) range, " +
	                                   "or dstPos is not in the [0, dst.length) range, or len > dst.length.")
	@throws[ArrayStoreException]("if any of elements copied from src cannot be stored in dst, " +
	                             "including boxing and unboxing.")
	@inline def cyclicCopyTo(src :ArrayLike[_], srcPos :Int, dst :MutableArray[_], dstPos :Int, length :Int) :Unit =
		Array.cyclicCopyTo(src.asInstanceOf[Array[_]], srcPos, dst.asInstanceOf[Array[_]], dstPos, length)

	/** Brings all the benefits of [[net.noresttherein.sugar.arrays.ArrayLike.copy copy]], but clips the input indices
	  * and maximum number of elements to arrays' sizes, providing semantics of
	  * {{{
	  *     src.drop(srcPos).copyToArray(dst, dstPos, length)
	  * }}}
	  * @return The number of copied elements.
	  */
	@throws[IndexOutOfBoundsException]("if dstPos is negative.")
	@throws[ArrayStoreException]("if any of elements copied from src cannot be stored in dst," +
	                             "including boxing and unboxing.")
	def permissiveCopy(src :ArrayLike[_], srcPos :Int, dst :MutableArray[_], dstPos :Int, length :Int) :Int = {
		val srcLength = src.asInstanceOf[Array[_]].length
		val dstLength = dst.asInstanceOf[Array[_]].length
		//we could check heere if srcLength == 0 | dstLength == 0, but it would be inconsistent
		// with the implementation in IterableOnceOps
		if (srcPos >= srcLength | dstPos >= dstLength | length <= 0)
			0
		else if (dstPos < 0)
			outOfBounds_!(dstPos, dstLength)
		else {
			val srcIdx = math.max(srcPos, 0)
			val max = math.min(math.min(srcLength - srcIdx, dstLength - dstPos), length)
			copy(src, srcIdx, dst, dstPos, max)
			max
		}
	}

	/** Invokes [[net.noresttherein.sugar.arrays.ArrayLike.cyclicCopy cyclicCopy]], but uses `srcPos` and `dstPos`
	  * module `src.length` and `dst.length` (making all indices valid), and clips `length`
	  * to the maximum number of elements to copy, based on arrays' lengths. In particular, providing a negative index
	  * results in effectively counting from the end of the array. Offers semantics of
	  * [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.cyclicCopyToArray cyclicCopyToArray]]
	  * (with the exception of wrapping the copying back to the start of `src` array),
	  * consistent with the standard `copyToArray` method.
	  * @return The number of copied elements.
	  */
	@throws[ArrayStoreException]("if any of elements copied from src cannot be stored in dst," +
	                             "including boxing and unboxing.")
	def permissiveCyclicCopy(src :ArrayLike[_], srcPos :Int, dst :MutableArray[_], dstPos :Int, length :Int) :Int = {
		val srcLength = src.asInstanceOf[Array[_]].length
		val dstLength = dst.asInstanceOf[Array[_]].length
		if (srcLength == 0 | dstLength == 0 | length <= 0)
			0
		else {
			val srcIdx = if (srcPos < 0) srcLength - srcPos % srcLength else srcPos % srcLength
			val dstIdx = if (dstPos < 0) dstLength - dstPos % dstLength else dstPos % dstLength
			val max = math.min(math.min(srcLength, dstLength), length)
			cyclicCopy(src, srcIdx, dst, dstIdx, max)
			max
		}
	}

	/** Invokes [[net.noresttherein.sugar.arrays.ArrayLike.cyclicCopyFrom cyclicCopyFrom]],
	  * but treats `srcPos` as modulo `src.length`, copies nothing if `dstPos >= dst.length`, and clips `length`
	  * to the maximum number of elements to copy, based on arrays' lengths and `srcPos` and `dstPos`. Offers semantics
	  * consistent with the standard `copyToArray` method.
	  * @return The number of copied elements.
	  */ //consider: allowing
	@throws[IndexOutOfBoundsException]("if dstPos is negative.")
	@throws[ArrayStoreException]("if any of elements copied from src cannot be stored in dst," +
	                             "including boxing and unboxing.")
	def permissiveCyclicCopyFrom(src :ArrayLike[_], srcPos :Int, dst :MutableArray[_], dstPos :Int, length :Int) :Int = {
		val srcLength = src.asInstanceOf[Array[_]].length
		val dstLength = dst.asInstanceOf[Array[_]].length
		if (srcLength == 0 | dstLength == 0 | dstPos >= dstLength | length <= 0)
			0
		else {
			val srcIdx = if (srcPos < 0) srcLength - srcPos % srcLength else srcPos % srcLength
			val max = math.min(math.min(srcLength, dstLength - dstPos), length)
			cyclicCopy(src, srcIdx, dst, dstPos, max)
			max
		}
	}

	/** Invokes [[net.noresttherein.sugar.arrays.ArrayLike.cyclicCopyTo cyclicCopyTo]], but treats `dstPos`
	  * as modulo `dst.length`, copies nothing if `srcPos >= src.length`, and clips `length` to the maximum number
	  * of elements to copy, based on arrays' lengths and `srcPos`. Offers semantics consistent with
	  * [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.cyclicCopyRangeToArray cyclicCopyRangeToArray]].
	  * @return The number of copied elements.
	  */
	@throws[IndexOutOfBoundsException]("if srcPos is negative.")
	@throws[ArrayStoreException]("if any of elements copied from src cannot be stored in dst," +
	                             "including boxing and unboxing.")
	def permissiveCyclicCopyTo(src :ArrayLike[_], srcPos :Int, dst :MutableArray[_], dstPos :Int, length :Int) :Int = {
		val srcLength = src.asInstanceOf[Array[_]].length
		val dstLength = dst.asInstanceOf[Array[_]].length
		if (srcLength == 0 | dstLength == 0 | dstPos >= dstLength | length <= 0)
			0
		else {
			val dstIdx = if (dstPos < 0) dstLength - dstPos % dstLength else dstPos % srcLength
			val max = math.min(math.min(srcLength - srcPos, dstLength), length)
			cyclicCopy(src, srcPos, dst, dstIdx, max)
			max
		}
	}



	/** A stepper iterating over a range of an array. Indices out of range are skipped silently. */
	def stepper[A, S <: Stepper[_]]
	           (array :ArrayLike[A], first :Int = 0, size :Int = Int.MaxValue)
	           (implicit shape :StepperShape[A, S]) :S with EfficientSplit =
		Stepper.apply(array.asInstanceOf[Array[A]], first, size)

	/** An iterator over a range of an array. Indices out of range are skipped silently. */
	def iterator[A](array :ArrayLike[A], first :Int = 0, size :Int = Int.MaxValue) :Iterator[A] = {
		val arr = array.asInstanceOf[Array[A]]
		val len = arr.length
		if (len == 0 | size <= 0 | first >= len) Iterator.empty
		else ArrayIterator(arr, first, size)
	}

	/** An iterator over range `[from - size + 1, from]` of an array in reverse.
	  * The first returned element is `array(first)` (or `array.last` if `first >= array.length`),
	  * followed by the immediately preceding element, and so on. Indices out of range are skipped silently.
	  */
	def reverseIterator[A](array :ArrayLike[A], first :Int = 0, size :Int = Int.MaxValue) :Iterator[A] = {
		val arr = array.asInstanceOf[Array[A]]
		val len = arr.length
		if (len == 0 | size <= 0 | first >= len) Iterator.empty
		else ReverseArrayIterator(arr, first, size)
	}

	/** An iterator listing subsequent elements of the argument array, starting with `array(first % array.length)`
	  * (or `array(array.length + first % array.length)`` if `first` is negative), wrapping at the end of the array.
	  * If `size > array.length - first % array.length`, then the iterator continues from the begining of the array.
	  * If `size > array.length`, all elements of the array are returned.
	  * Neither this method nor the iterator throw an `IndexOutOfBoundsException`.
	  */
	def cyclicIterator[A](array :ArrayLike[A], first :Int = 0, size :Int = Int.MaxValue) :Iterator[A] = {
		val arr = array.asInstanceOf[Array[A]]
		val len = arr.length
		if (len == 0 | size <= 0)
			Iterator.empty
		else if (first < 0)
			CyclicArrayIterator(arr, len + first % len, math.max(size, len))
		else
			CyclicArrayIterator(arr, first % len, math.max(size, len))
	}

	/** An iterator listing elements of an array in the reverse order, starting with `array(first % array.length)`
	  * (or `array(array.length + first % array.length)` if `first` is negative).
	  * If the start of the array is reached before returning `size` elements - i.e., `size > first % length` -
	  * then the next returned element will be the last element in the array, followed by preceding elements.
	  * If `size >= array.length`, all elements of the array are returned in this way exactly once.
	  * Neither this method nor the iterator throw an `IndexOutOfBoundsException`.
	  */
	def reverseCyclicIterator[A](array :ArrayLike[A], first :Int = -1, size :Int = Int.MaxValue) :Iterator[A] = {
		val arr = array.asInstanceOf[Array[A]]
		val len = arr.length
		if (len == 0 | size <= 0)
			Iterator.empty
		else if (first < 0)
			ReverseCyclicArrayIterator(arr, len + first % len, math.max(size, len))
		else
			ReverseCyclicArrayIterator(array.asInstanceOf[Array[A]], first, size)
	}



	@inline def unapplySeq[E](items :ArrayLike[E]) :UnapplySeqWrapper[E] =
		new UnapplySeqWrapper(items.castFrom[ArrayLike[E], Array[E]])

	/** A factory and pattern for all known collections wrapping arrays, including all
	  * [[net.noresttherein.sugar.arrays.ArrayLike ArrayLike]] subtypes.
	  * It recognizes both mutable and immutable collections, and the extracted/wrapped array is shared, not copied.
	  * Note that the actual component type of the array may be a subtype of the collection's element type.
	  */
	@SerialVersionUID(Ver)
	object Wrapped {
		def apply[E](array :ArrayLike[E]) :collection.IndexedSeq[E] =
			ArrayLikeSlice.wrap(array.castFrom[ArrayLike[E], Array[E]])

		def unapply[E](elems :IterableOnce[E]) :Maybe[ArrayLike[E]] = elems match {
			case seq :ArrayIterableOnce[E] if seq.knownSize == seq.unsafeArray.length =>
				Yes(seq.unsafeArray.castFrom[Array[_], ArrayLike[E]])
			case seq :collection.IndexedSeq[_] => elems match {
				case seq :mutable.ArraySeq[E]   => Yes(seq.array.castFrom[Array[_], ArrayLike[E]])
				case seq :ArraySeq[E]           => Yes(seq.unsafeArray.castFrom[Array[_], ArrayLike[E]])
				case seq :ArrayBuffer[_] if CheatedAccess.array(seq).length == seq.length  =>
					Yes(CheatedAccess.array(seq).asInstanceOf[RefArray[E]])
				case VectorArray(array)         => Yes(array.asInstanceOf[RefArray[E]])
				case MatrixBufferArray(array) if array.length == seq.length => Yes(array.asInstanceOf[ArrayLike[E]])
				case _ => No
			}
			case _ => No
		}

		/** A factory and pattern for non-immutable collections backed by array slices.
		  * It recognizes both mutable and immutable collections, and the extracted/wrapped array is shared, not copied.
          * Note that the actual component type of the array may be a subtype of the collection's element type.
		  */
		@SerialVersionUID(Ver)
		object Slice {
			def apply[E](array :ArrayLike[E], from :Int, until :Int) :collection.IndexedSeq[E] =
				ArrayLikeSlice.slice[E](array.castFrom[ArrayLike[E], Array[E]], from, until)

			def unapply[E](elems :IterableOnce[E]) :Maybe[(ArrayLike[E], Int, Int)] = elems match {
				case seq :ArrayIterableOnce[E] =>
					Yes(seq.unsafeArray.castFrom[Array[_], ArrayLike[E]], seq.startIndex, seq.startIndex + seq.knownSize)
				case _ :collection.IndexedSeq[_] => elems match {
					case seq :ArraySeq[E]          =>
						Yes((seq.unsafeArray.castFrom[Array[_], ArrayLike[E]], 0, seq.unsafeArray.length))
					case seq :mutable.ArraySeq[E]  =>
						Yes((seq.array.castFrom[Array[_], ArrayLike[E]], 0, seq.array.length))
					case seq :ArrayBuffer[_]       =>
						Yes(CheatedAccess.array(seq).asInstanceOf[RefArray[E]], 0, seq.length)
					case VectorArray(array)        =>
						Yes(array.asInstanceOf[RefArray[E]], 0, array.length)
					case seq :MatrixBuffer[E] if seq.dim == 1 && seq.startIndex <= seq.data1.length - seq.length =>
						Yes(seq.data1, seq.startIndex, seq.startIndex + seq.knownSize)
					case _ => No
				}
				case _ => No
			}
		}
	}



	//Consider: there is a small issue in that the factory extension method is in extensions,
	// so a clash between imports is possible.

	/** Extension methods for all [[net.noresttherein.sugar.arrays.ArrayLike! ArrayLike]]`[E]` implementations.
	  * Most of these can be found in [[scala.collection.IterableOnceOps IterableOnceOps]],
	  * [[scala.collection.IterableOps IterableOps]], or [[scala.collection.SeqOps SeqOps]],
	  * and have the exact same semantics. Original methods are documented accordingly.
	  * It is the standard collection methods subset which can be implemented safely without knowing
	  * the actual element type of the underlying array.
	  * Can be enabled for any `ArrayLike` (returning also a generic `ArrayLike`) by importing
	  * `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.ArrayLikeExtension ArrayLikeExtension]]
	  * (also `net.noresttherein.sugar.arrays.ArrayLike.extensions.ArrayLikeExtension` and
	  * `net.noresttherein.sugar.extensions.ArrayLikeExtension`).
	  * Additional extensions for individual array types are also available under
	  * `ArrayLike.extensions`/`IArray.extensions`/`RefArray.extensions`/`IRefArray.extensions`
	  * (containing implicit extension methods also from different classes, forming the full API of each type),
	  * and in object `net.noresttherein.sugar.extensions`, together with all extension methods in the whole library.
	  * @see [[net.noresttherein.sugar.arrays.IArrayLike.IArrayLikeExtension]] - additional extension methods
	  *      for `IArray` and `IRefArray`.
	  * @see [[net.noresttherein.sugar.arrays.RefArrayLike.RefArrayLikeExtension]] - additional extension methods
	  *      for `RefArray` and `IRefArray`.
	  * @tparam E   the type of the elements in the array (not necessarily the component type of the underlying array!).
	  * @tparam Arr the type constructor of an `ArrayLike` subtype returned by methods which return
	  *             an `Array` in [[collection.ArrayOps ArrayOps]] (and the `ArrayLike` subtype being granted
	  *             these methods).
	  * @define coll `array-like`
	  * @define Coll `ArrayLike`
	  */
	class ArrayLikeExtension[Arr[X] <: ArrayLike[X], E] private[sugar] (private[ArrayLike] val self :Array[Unknown])
		extends AnyVal
	{   //Avoid casting array to Array[E], as it may easily cause problems if inlined.
		//'Safe' casting methods with very limited application.
		// These methods are private, which may stop other methods from inlining, if no recursive inlining happens.
		// If so, they must be made package protected.
		@inline private def expose(array :Array[Unknown]) :Arr[E] = {
			releaseFence()
			array.asInstanceOf[Arr[E]]
		}
		@inline private def asElement(value :Unknown) :E = value.asInstanceOf[E]
		@inline private def lengthOf(other :ArrayLike[_]) :Int = other.asInstanceOf[Array[_]].length

		/** Casts this array to `Array[U]` to satisfy the type checker.
		  * This should be used only in generic context, and never in `@inline` methods.
		  */
//		@inline private def as[U] :Array[U] = self.asInstanceOf[Array[U]]
//		@inline private def arrayExtension :ArrayExtension[E] =
//			new ArrayExtension(self).asInstanceOf[ArrayExtension[E]]

		@inline def knownSize :Int = self.length
		@inline def length    :Int = self.length
		@inline def size      :Int = self.length
		@inline def isEmpty   :Boolean = self.length == 0
		@inline def nonEmpty  :Boolean = self.length > 0
		@inline def sizeIs    :Int = self.length
		@inline def lengthIs  :Int = self.length
		@inline def sizeCompare(otherSize :Int) :Int = Integer.compare(self.length, otherSize)
		@inline def lengthCompare(len :Int) :Int = Integer.compare(self.length, len)

		@inline def head :E = self(0).asInstanceOf[E]
		@inline def last :E = self(self.length - 1).asInstanceOf[E]
		@inline def headOption :Option[E] = self.headOption.castParam[E]
		@inline def lastOption :Option[E] = self.lastOption.castParam[E]

		@inline def apply(i :Int) :E = self(i).asInstanceOf[E]

		@inline def count(p :E => Boolean) :Int = self.count(p.castFrom[E => Boolean, Unknown => Boolean])
		@inline def exists(p :E => Boolean) :Boolean = indexWhere(p) >= 0
		def forall(p :E => Boolean) :Boolean =
			ArrayLikeSpecOps.segmentLength(self, 0, self.length, true)(p.castParam1[Unknown]) == -1

		def find(p :E => Boolean) :Option[E] = {
			val length = self.length
			val i = ArrayLikeSpecOps.indexWhere(self, 0, length)(p.castParam1[Unknown], 0)
			if (i == length) None else Some(asElement(self(i)))
		}
		def findLast(p :E => Boolean) :Option[E] = {
			val len = self.length
			val i = ArrayLikeSpecOps.lastIndexWhere(self, 0, len)(p.castParam1[Unknown], len)
			if (i < 0) None else Some(asElement(self(i)))
		}

		def segmentLength(p :E => Boolean, from :Int = 0) :Int =
			ArrayLikeSpecOps.segmentLength(self, 0, self.length)(p.castParam1[Unknown], from)

		def indexWhere(p :E => Boolean, from :Int = 0) :Int =
			ArrayLikeSpecOps.indexWhere(self, 0, self.length)(p.castParam1[Unknown], from)

		def lastIndexWhere(p :E => Boolean, end :Int = Int.MaxValue) :Int =
			ArrayLikeSpecOps.lastIndexWhere(self, 0, self.length)(p.castParam1[Unknown], end)

		@inline def getIndexWhere(p :E => Boolean, from :Int = 0) :Option[Int] = indexWhere(p, from) match {
			case -1 => None
			case  n => Some(n)
		}
		@inline def getLastIndexWhere(p :E => Boolean, end :Int = Int.MaxValue) :Option[Int] =
			lastIndexWhere(p, end) match {
				case -1 => None
				case  n => Some(n)
			}
		@inline def findIndexWhere(p :E => Boolean, from :Int = 0) :IntOpt = indexWhere(p, from) match {
			case -1 => NoInt
			case  n => AnInt(n)
		}
		@inline def findLastIndexWhere(p :E => Boolean, end :Int = Int.MaxValue) :IntOpt =
			lastIndexWhere(p, end) match {
				case -1 => NoInt
				case  n => AnInt(n)
			}
		@inline def sureIndexWhere(p :E => Boolean, from :Int = 0) :Int = indexWhere(p, from) match {
			case -1 => indexWhereNotFound(errorString(self), from)
			case  n => n
		}
		@inline def sureLastIndexWhere(p :E => Boolean, end :Int = Int.MaxValue) :Int = lastIndexWhere(p, end) match {
			case -1 => lastIndexWhereNotFound(errorString(self), self.length, end)
			case  n => n
		}

		def indexOf[U >: E](elem :U, from :Int = 0) :Int = ArrayLikeSpecOps.indexOf[Any](self, 0, self.length)(elem, from)
		def lastIndexOf[U >: E](elem :U, end :Int = Int.MaxValue) :Int =
			ArrayLikeSpecOps.lastIndexOf[Any](self, 0, self.length)(elem, end)

		/** Returns `Some(`[[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension.indexOf indexOf]]`(elem, from)).filter(_>=0)`. */
		@inline def getIndexOf[U >: E](elem :U, from :Int = 0) :Option[Int] = indexOf(elem, from) match {
			case -1 => None
			case  n => Some(n)
		}
		/** Returns `Some(`[[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension.lastIndexOf lastIndexOf]]`(elem, end)).filter(_>=0)`. */
		@inline def getLastIndexOf[U >: E](elem :U, end :Int = Int.MaxValue) :Option[Int] =
			lastIndexOf(elem, end) match {
				case -1 => None
				case  n => Some(n)
			}
		/** Returns [[net.noresttherein.sugar.vars.IntOpt.AnInt AnInt]]`(`[[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension.indexOf indexOf]]`(elem, from))`
		  * or [[net.noresttherein.sugar.vars.IntOpt.NoInt NoInt]] if the element is not found in this array
		  * at or after position `from`.
		  */
		@inline def findIndexOf[U >: E](elem :U, from :Int = 0) :IntOpt = indexOf(elem, from) match {
			case -1 => NoInt
			case  n => AnInt(n)
		}
		/** Returns [[net.noresttherein.sugar.vars.IntOpt.AnInt AnInt]]`(`[[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension.lastIndexOf lastIndexOf]]`(elem, end))`
		  * or [[net.noresttherein.sugar.vars.IntOpt.NoInt NoInt]] if the element is not found in this array
		  * at or before position `end`.
		  */
		@inline def findLastIndexOf[U >: E](elem :U, end :Int = Int.MaxValue) :IntOpt = lastIndexOf(elem, end) match {
			case -1 => NoInt
			case  n => AnInt(n)
		}
		/** Returns [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension.indexOf indexOf]]`(elem, from)`. */
		@throws[NoSuchElementException]("if elem is not present in this array at or after position from.")
		@inline def sureIndexOf[U >: E](elem :U, from :Int = 0) :Int = indexOf(elem, from) match {
			case -1 => indexOfNotFound(errorString(self), elem, from)
			case n => n
		}
		/** Returns [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension.lastIndexOf lastIndexOf]]`(elem, end)`. */
		@throws[NoSuchElementException]("if elem is not present in this array at or before position end.")
		@inline def sureLastIndexOf[U >: E](elem :U, end :Int = Int.MaxValue) :Int = lastIndexOf(elem, end) match {
			case -1 => lastIndexOfNotFound(errorString(self), self.length, elem, end)
			case  n => n
		}


		@inline def contains[U >: E](elem :U) :Boolean = indexOf(elem, 0) >= 0

		//todo: document and test these
		@inline def startsWith[U >: E](that :ArrayLike[U]) :Boolean = startsWith(that, 0)
		def startsWith[U >: E](that :ArrayLike[U], from :Int) :Boolean = {
			val thisLength = self.length
			val thatLength = lengthOf(that)
			val from0    = math.max(from, 0)
			from0 <= thisLength - thatLength && mismatch(that, from0, from0 + thatLength, 0, thatLength) == -1
		}
		@inline def startsWith[U >: E](that :IterableOnce[U]) :Boolean =
			mutable.ArraySeq.make(self).castParam[U].startsWith(that, 0)

		@inline def startsWith[U >: E](that :IterableOnce[U], from :Int) :Boolean =
			mutable.ArraySeq.make(self).castParam[U].startsWith(that, from)

		def endsWith[U >: E](that :ArrayLike[U]) :Boolean = {
			val thisLength = self.length
			val thatLength = lengthOf(that)
			thatLength >= thisLength && mismatch(that, thisLength - thatLength, thisLength, 0, thatLength) == -1
		}
		@inline def endsWith[U >: E](that :Iterable[U]) :Boolean =
			mutable.ArraySeq.make(self).castParam[U].endsWith(that)

		def indexOfSlice[U >: E](that :collection.Seq[U], from :Int = 0) :Int =
			mutable.ArraySeq.make(self).castParam[E].indexOfSlice(that, from)

		def lastIndexOfSlice[U >: E](that :collection.Seq[U], end :Int = Int.MaxValue) :Int =
			mutable.ArraySeq.make(self).castParam[E].lastIndexOfSlice(that, end)

		@inline def getIndexOfSlice[U >: E](that :collection.Seq[U], from :Int = 0) :Option[Int] =
			indexOfSlice(that, from) match {
				case -1 => None
				case  n => Some(n)
			}
		@inline def getLastIndexOfSlice[U >: E](that :collection.Seq[U], end :Int = Int.MaxValue) :Option[Int] =
			lastIndexOfSlice(that, end) match {
				case -1 => None
				case  n => Some(n)
			}
		@inline def findIndexOfSlice[U >: E](that :collection.Seq[U], from :Int = 0) :IntOpt =
			indexOfSlice(that, from) match {
				case -1 => NoInt
				case  n => AnInt(n)
			}
		@inline def findLastIndexOfSlice[U >: E](that :collection.Seq[U], end :Int = Int.MaxValue) :IntOpt =
			lastIndexOfSlice(that, end) match {
				case -1 => NoInt
				case n  => AnInt(n)
			}
		@inline def sureIndexOfSlice[U >: E](that :collection.Seq[U], from :Int = 0) :Int =
			indexOfSlice(that, from) match {
				case -1 => indexOfSliceNotFound(errorString(self), that, from)
				case  n => n
			}
		@inline def sureLastIndexOfSlice[U >: E](that :collection.Seq[U], end :Int = Int.MaxValue) :Int =
			lastIndexOfSlice(that, end) match {
				case -1 => lastIndexOfSliceNotFound(errorString(self), self.length, that, end)
				case  n => n
			}

		@inline def indexOfSlice[A >: E](that :ArrayLike[A]) :Int = indexOfSlice(that, 0)
		def indexOfSlice[A >: E](that :ArrayLike[A], from :Int) :Int =
			mutable.ArraySeq.make(self).castParam[E].indexOfSlice(Wrapped(that), from)

		@inline def lastIndexOfSlice[A >: E](that :ArrayLike[A]) :Int = lastIndexOfSlice(that, Int.MaxValue)
		def lastIndexOfSlice[A >: E](that :ArrayLike[A], end :Int) :Int =
			mutable.ArraySeq.make(self).castParam[E].lastIndexOfSlice(Wrapped(that), end)

		@inline def getIndexOfSlice[U >: E](that :ArrayLike[U]) :Option[Int] = getIndexOfSlice(that, 0)
		@inline def getIndexOfSlice[U >: E](that :ArrayLike[U], from :Int) :Option[Int] =
			indexOfSlice(that, from) match {
				case -1 => None
				case  n => Some(n)
			}
		@inline def getLastIndexOfSlice[U >: E](that :ArrayLike[U]) :Option[Int] =
			getLastIndexOfSlice(that, Int.MaxValue)

		@inline def getLastIndexOfSlice[U >: E](that :ArrayLike[U], end :Int) :Option[Int] =
			lastIndexOfSlice(that, end) match {
				case -1 => None
				case  n => Some(n)
			}
		@inline def findIndexOfSlice[U >: E](that :ArrayLike[U]) :IntOpt = findIndexOfSlice(that, 0)
		@inline def findIndexOfSlice[U >: E](that :ArrayLike[U], from :Int) :IntOpt =
			indexOfSlice(that, from) match {
				case -1 => NoInt
				case  n => AnInt(n)
			}
		@inline def findLastIndexOfSlice[U >: E](that :ArrayLike[U]) :IntOpt = findLastIndexOfSlice(that, Int.MaxValue)
		@inline def findLastIndexOfSlice[U >: E](that :ArrayLike[U], end :Int) :IntOpt =
			lastIndexOfSlice(that, end) match {
				case -1 => NoInt
				case  n => AnInt(n)
			}

		@inline def sureIndexOfSlice[U >: E](that :ArrayLike[U]) :Int = sureIndexOfSlice(that, 0)
		@inline def sureIndexOfSlice[U >: E](that :ArrayLike[U], from :Int) :Int =
			indexOfSlice(that, from) match {
				case -1 => indexOfSliceNotFound(errorString(self), Wrapped(that), from)
				case  n => n
			}
		@inline def sureLastIndexOfSlice[U >: E](that :ArrayLike[U]) :Int = sureLastIndexOfSlice(that, Int.MaxValue)
		@inline def sureLastIndexOfSlice[U >: E](that :ArrayLike[U], end :Int) :Int =
			lastIndexOfSlice(that, end) match {
				case -1 => lastIndexOfSliceNotFound(errorString(self), self.length, Wrapped(that), end)
				case  n => n
			}

		@inline def containsSlice[A >: E](that :collection.Seq[A]) :Boolean = indexOfSlice(that) >= 0
		@inline def containsSlice[A >: E](that :ArrayLike[A]) :Boolean = indexOfSlice(that) >= 0


		@inline def corresponds[A](that :IterableOnce[A])(p :(E, A) => Boolean) :Boolean =
			ArrayIterator(self).castParam[E].corresponds(that)(p)

		def corresponds[A](that :ArrayLike[A])(p :(E, A) => Boolean) :Boolean = {
			//consider: extracting it to ArrayLikeOps and specializing
			val other = that.castFrom[ArrayLike[A], Array[Unknown]]
			val pred = p.castParams[Unknown, Unknown, Boolean]
			val thisLen = self.length
			val thatLen = other.length
			if (thisLen != thatLen)
				return false
			var idx = 0
			while (idx < thisLen) {
				if (!pred(self(idx), other(idx)))
					return false
				idx += 1
			}
			true
		}

		/** Finds and returns the relative index of the first mismatch between two arrays,
		  * or return -1 if no mismatch is found. If the arrays are of different lengths, but equal on all positions
		  * of the shorter array, `-1` is returned.
		  * @param that the second array to be tested for a mismatch
		  * @return the relative index of the first mismatch between the two arrays over the specified ranges, or `-1`.
		  */
		@inline def mismatch[U >: E](that :ArrayLike[U]) :Int =
			mismatch(that, 0, self.length, 0, lengthOf(that))

		/** Finds and returns the relative index of the first mismatch between two arrays over the specified ranges,
		  * otherwise return -1 if no mismatch is found. The index will be in the range of 0 (inclusive) up
		  * to the length (inclusive) of the smaller range. The starting indices of the two ranges must be valid indices
		  * in their respective arrays. The ending indices are treated exclusively, and clipped to fit in ranges
		  * `thisFrom..this.length` and `thatFrom..that.length`. If the ranges are of different lengths,
		  * the longer one is truncated to the length of the shorter one.
		  * @param that      the second array to be tested for a mismatch
		  * @param thisFrom  the index (inclusive) of the first element in this array to be tested
		  * @param thisUntil the index (exclusive) of the last element in the first array to be tested
		  * @param thatFrom  the index (inclusive) of the first element in the second array to be tested
		  * @param thatUntil the index (exclusive) of the last element in the second array to be tested
		  * @return the relative index of the first mismatch between the two arrays over the specified ranges, otherwise `-1`.
		  */
		def mismatch[U >: E](that :ArrayLike[U], thisFrom :Int, thisUntil :Int, thatFrom :Int, thatUntil :Int) :Int = {
			val thisLength = self.length
			val thatLength = lengthOf(that)
			val from0  = math.max(0, math.min(thisFrom, thisLength))
			val from1  = math.max(0, math.min(thatFrom, thatLength))
			val until0 = math.max(from0, math.min(thisUntil, thisLength))
			val until1 = math.max(from1, math.min(thatUntil, thatLength))
			(self :Array[_], that :ArrayLike[_]) match {
				case (a :Array[AnyRef], b :Array[AnyRef]) =>
					Arrays.mismatch(a, from0, until0, b, from1, until1)
				case (a :Array[Int], b :Array[Int]) =>
					Arrays.mismatch(a, from0, until0, b, from1, until1)
				case (a :Array[Long], b :Array[Long]) =>
					Arrays.mismatch(a, from0, until0, b, from1, until1)
				case (a :Array[Double], b :Array[Double]) =>
					Arrays.mismatch(a, from0, until0, b, from1, until1)
				case (a :Array[Byte], b :Array[Byte]) =>
					Arrays.mismatch(a, from0, until0, b, from1, until1)
				case (a :Array[Float], b :Array[Float]) =>
					Arrays.mismatch(a, from0, until0, b, from1, until1)
				case (a :Array[Char], b :Array[Char]) =>
					Arrays.mismatch(a, from0, until0, b, from1, until1)
				case (a :Array[Short], b :Array[Short]) =>
					Arrays.mismatch(a, from0, until0, b, from1, until1)
				case (a :Array[Boolean], b :Array[Boolean]) =>
					Arrays.mismatch(a, from0, until0, b, from1, until1)
				case (a :Array[E @unchecked], b :Array[U @unchecked]) =>
					var i = from0; var j = from1
					val end = from0 + math.min(until0 - from0, until1 - from1)
					while (i < end && a(i) == b(j)) {
						i += 1; j += 1
					}
					if (i < end) i - from0
					else if (until0 - from0 == until1 - from1) -1
					else end - from0
			}
		}

		def sameElements(that :IterableOnce[_]) :Boolean = mutable.ArraySeq.make(self).sameElements(that)

		/** Equality of corresponding elements in the two arrays. Array component type is ignored, and an array
		  * of a primitive type will equal an array of the appropriate box class, as long as the actual values are equal.
		  */
		def sameElements(that :ArrayLike[_]) :Boolean =
			(self eq that) || (self.length == lengthOf(that)) && {
				def slowEquals = {
					val other = that.asInstanceOf[Array[_]]
					var i = other.length - 1
					while (i > 0 && self(i) == other(i))
						i -= 1
					true
				}
				(self :Array[_], that) match {
					case (a1 :Array[AnyRef],  a2 :Array[AnyRef])  => Arrays.equals(a1, a2)
					case (a1 :Array[Int],     a2 :Array[Int])     => Arrays.equals(a1, a2)
					case (a1 :Array[Long],    a2 :Array[Long])    => Arrays.equals(a1, a2)
					case (a1 :Array[Double],  a2 :Array[Double])  => Arrays.equals(a1, a2)
					case (a1 :Array[Byte],    a2 :Array[Byte])    => Arrays.equals(a1, a2)
					case (a1 :Array[Char],    a2 :Array[Char])    => Arrays.equals(a1, a2)
					case (a1 :Array[Float],   a2 :Array[Float])   => Arrays.equals(a1, a2)
					case (a1 :Array[Short],   a2 :Array[Short])   => Arrays.equals(a1, a2)
					case (a1 :Array[Boolean], a2 :Array[Boolean]) => Arrays.equals(a1, a2)
					case _                                        => slowEquals
				}
			}

		/** Deep equality of array elements and array component (element) types.
		  * @return `this.getClass.getComponentType == that.getClass.getComponentType && sameElements(that.asInstanceOf[Array[_]])`.
		  */
		def deepEquals(that :Any) :Boolean = that match {
			case self :AnyRef if this.asAnyRef eq self => true
			case other :Array[_]
				if other.length == self.length && other.getClass.getComponentType == self.getClass.getComponentType
			=>
				this sameElements other
			case _ => false
		}

		@inline def sum[U >: E](implicit num :Numeric[U]) :U = foldLeft(num.zero)(num.plus)
		@inline def product[U >: E](implicit num :Numeric[U]) :U = foldLeft(num.one)(num.times)
		@inline def min[U >: E](implicit ord :Ordering[U]) :E = reduceLeft(ord.min)
		@inline def max[U >: E](implicit ord :Ordering[U]) :E = reduceLeft(ord.max)
		@inline def minOption[U >: E](implicit ord :Ordering[U]) :Option[E] = reduceLeftOption(ord.min)
		@inline def maxOption[U >: E](implicit ord :Ordering[U]) :Option[E] = reduceLeftOption(ord.max)

		//consider; bringing back those methods in IndexedSeqLike
		def minBy[A](f :E => A)(implicit cmp :Ordering[A]) :E = toSeq.minBy(f)
//			IndexedSeqLike.forArray[E].minBy(as[E])(f)

		def maxBy[A](f :E => A)(implicit cmp :Ordering[A]) :E = toSeq.maxBy(f)
//			IndexedSeqLike.forArray[E].maxBy(self.asInstanceOf[Array[E]])(f)

		def minByOption[A](f :E => A)(implicit cmp :Ordering[A]) :Option[E] = toSeq.minByOption(f)
//			IndexedSeqLike.forArray[E].minByOption(self.asInstanceOf[Array[E]])(f)

		def maxByOption[A](f :E => A)(implicit cmp :Ordering[A]) :Option[E] = toSeq.maxByOption(f)
//			IndexedSeqLike.forArray[E].maxByOption(self.asInstanceOf[Array[E]])(f)

		/** Search this array for a specific element.
		  * The array should be sorted with the same `Ordering` before calling; otherwise, the results are undefined.
		  * @param elem     the element to find.
		  * @return a `Found` value containing the index corresponding to the element in the
		  *         sequence, or the `InsertionPoint` where the element would be inserted if
		  *         the element is not in the sequence.
		  */
		@inline def search[U >: E :Ordering](elem :U) :SearchResult =
			 ArraySeq.unsafeWrapArray(self).search(elem, 0, self.length)(implicitly[Ordering[U]].castParam[Any]) //boxes

		/** Search the specified range of this array for a given element.
		  * The array should be sorted with the same `Ordering` before calling; otherwise, the results are undefined.
		  * @param elem     the element to find.
		  * @param from     the index of the first element in the searched range.
		  * @param until    the index following the last element in the searched range.*
		  * @return a `Found` value containing the index corresponding to the element in the
		  *         sequence, or the `InsertionPoint` where the element would be inserted if
		  *         the element is not in the sequence.
		  */
		@inline def search[U >: E :Ordering](elem :U, from :Int, until :Int) :SearchResult =
			ArraySeq.unsafeWrapArray(self).search(elem, from, until)(implicitly[Ordering[U]].castParam[Any])

		/** Performs a binary search of element `x` in a section of this $coll, sorted according
		  * to an implicit `Ordering[E]`. If the $coll is not sorted, or the `Ordering` is not consistent with `equals`,
		  * then the behaviour is unspecified.
		  * The differences from [[collection.IndexedSeqOps.search search]] from the standard library are:
		  *   1. ability to provide bounds within which to search,
		  *   1. returning always the index of the first occurrence of the value in case of duplicates
		  *      (rather than the index of any of them),
		  *   1. avoiding boxing of the $coll to `IndexedSeqOps` to make the call,
		  *   1. returning the value as an `ElementIndex`, which does not box the result,
		  *   1. switching to direct comparisons of built in value types if the `Ordering` is the default one.
		  * @return index of the search key, if it is contained in the $coll,
		  *         as `ElementIndex`.[[net.noresttherein.sugar.collections.ElementIndex.Present Present]].
		  *         Otherwise, the ''insertion point'',
		  *         as `ElementIndex.`[[net.noresttherein.sugar.collections.ElementIndex.Absent Absent]].
		  *         The `insertion point` is defined as the point at which the key would be inserted into the $coll:
		  *         the index of the first element in the array greater than the key, or `until`,
		  *         if all elements in the array are less than the specified key.
		  */
		@inline def binarySearch[U >: E :Ordering](x :U) :ElementIndex = binarySearch(0, self.length)(x)

		/** Performs a binary search of element `x` in a section of this $coll, sorted
		  * according to an implicit `Ordering[U]`. Returns the index of the first occurrence of `x`, if present
		  * in the given range, or an index `i`: `from <= i <= until`, such that
		  * `i == until || this(i) > x && (i == from || this(i) < x)`. If `until <= from`,
		  * then [[net.noresttherein.sugar.collections.ElementIndex.Absent Absent]]`(from)` is returned immediately.
		  * The differences from [[collection.IndexedSeqOps.search search]] from the standard library are:
		  *   1. ability to provide bounds within which to search,
		  *   1. returning always the index of the first occurrence of the value in case of duplicates
		  *      (rather than the index of any of them),
		  *   1. avoiding boxing of the array to `IndexedSeqOps` to make the call,
		  *   1. returning the value as an `ElementIndex`, which does not box the result,
		  *   1. switching to direct comparisons of built in value types if the `Ordering` is the default one.
		  * @tparam U    a supertype of the type of elements in this array, as evidenced by `E <:< U`.
		  * @param from  the lower bound (inclusive) on the searched index range. A negative index is equivalent to `0`,
		  *              while if `from > this.length` the effect is the same as if `from == this.length`.
		  * @param until the upper bound (exclusive) on the searched index range. A negative index is equivalent to `0`,
		  *              while if `until > this.length` the effect is the same as if `until == this.length`.
		  *              Values lesser than `from` are treated the same way as `until == from`.
		  * @return the index of the search key, if it is present in the searched range,
		  *         as `ElementIndex`.[[net.noresttherein.sugar.collections.ElementIndex.Present Present]].
		  *         Otherwise, the ''insertion point'',
		  *         as `ElementIndex.`[[net.noresttherein.sugar.collections.ElementIndex.Absent Absent]].
		  *         The `insertion point` is defined as the point at which the key would be inserted into the $coll:
		  *         the index of the first element in the range greater than the key, or `until`,
		  *         if all elements in the range are less than the specified key.
		  */ //<:< argument required because if we use [U >: E], then scalac infers too early U =:= E.
		@inline def binarySearch[U](from :Int, until :Int)(x :U)(implicit ordering :Ordering[U], sub :E <:< U)
				:ElementIndex =
		{
			val length = self.length
			val start  = math.min(length, math.max(from, 0))
			val limit  = math.min(until, length)
			val index  = (self :Array[_]) match {
				case _ if limit <= start => -start - 1
				case array :Array[AnyRef]  => //specialized for AnyRef - faster access than in Array[_]
					BinarySearch(array, start, limit - 1, x.asInstanceOf[AnyRef])(ordering.castParam[AnyRef])
				case array :Array[Int]      if ordering == Ordering.Int =>
//					Arrays.binarySearch(array, start, limit, x.asInstanceOf[Int])
					BinarySearch(array, start, limit - 1, x.asInstanceOf[Int])
				case array :Array[Long]     if ordering == Ordering.Long =>
//					Arrays.binarySearch(array, start, limit, x.asInstanceOf[Long])
					BinarySearch(array, start, limit - 1, x.asInstanceOf[Long])
				case array :Array[Double]   if ordering == Ordering.Double.TotalOrdering =>
//					Arrays.binarySearch(array, start, limit, x.asInstanceOf[Double])
					BinarySearch(array, start, limit - 1, x.asInstanceOf[Double])
				case array :Array[Byte]     if ordering == Ordering.Byte =>
//					Arrays.binarySearch(array, start, limit, x.asInstanceOf[Byte])
					BinarySearch(array, start, limit - 1, x.asInstanceOf[Byte])
				case array :Array[Char]     if ordering == Ordering.Char =>
//					Arrays.binarySearch(array, start, limit, x.asInstanceOf[Char])
					BinarySearch(array, start, limit - 1, x.asInstanceOf[Char])
				case array :Array[Float]    if ordering == Ordering.Float.TotalOrdering =>
//					Arrays.binarySearch(array, start, limit, x.asInstanceOf[Float])
					BinarySearch(array, start, limit - 1, x.asInstanceOf[Float])
				case array :Array[Short]    if ordering == Ordering.Short =>
//					Arrays.binarySearch(array, start, limit, x.asInstanceOf[Short])
					BinarySearch(array, start, limit - 1, x.asInstanceOf[Short])
				case array :Array[U @unchecked] =>
					BinarySearch[U, U](array, start, limit - 1, x)
			}
			new ElementIndex(index)
		}

		@inline def /: [A](z :A)(op :(A, E) => A) :A = new ArrayOps(self).foldLeft[A](z)(op.castParams[A, Unknown, A])
		@inline def :\ [A](z :A)(op :(E, A) => A) :A = new ArrayOps(self).foldRight[A](z)(op.castParams[Unknown, A, A])

		@inline def fold[U >: E](z :U)(op :(U, U) => U) :U = new ArrayOps(self).foldLeft(z)(op.castParams[U, Unknown, U])
		@inline def foldLeft[A](z :A)(op :(A, E) => A) :A = new ArrayOps(self).foldLeft(z)(op.castParams[A, Unknown, A])

		def foldRight[A](z :A)(op :(E, A) => A) :A =
			ArrayLikeSpecOps.foldRight(self, 0, self.length)(z)(op.castParams[Unknown, A, A])

		@inline def reduce[U >: E](op :(U, U) => U) :U = reduceLeft(op)
		@inline def reduceOption[U >: E](op :(U, U) => U) :Option[U] = reduceLeftOption[U](op)

		def reduceLeft[U >: E](op :(U, E) => U) :U =
			ArrayLikeSpecOps.reduceLeft(self, 0, self.length)(op.castParams[Unknown, Unknown, Unknown]).castFrom[Unknown, U]

		def reduceLeftOption[U >: E](op :(U, E) => U) :Option[U] =
			if (self.length == 0) None
			else Some(ArrayLikeSpecOps.reduceLeft(self, 0, length)(op.castParams[Unknown, Unknown, Unknown])).castParam[U]

		def reduceRight[U >: E](op :(E, U) => U) :U =
			ArrayLikeSpecOps.reduceRight(self, 0, length)(op.castParams[Unknown, Unknown, Unknown]).castFrom[Unknown, U]

		def reduceRightOption[U >: E](op :(E, U) => U) :Option[U] =
			if (self.length == 0) None
			else Some(ArrayLikeSpecOps.reduceRight(self, 0, length)(op.castParams[Unknown, Unknown, Unknown])).castParam[U]

		/** A copy of this array with values at indices `i` and `j` swapped. */
		@inline def swapped(i :Int, j :Int) :Arr[E] = expose {
			val arr = self
			val res = Array.copyOf(arr, self.length)
			val boo = res(i)
			res(i)  = res(j)
			res(j)  = boo
			res
		}


		/** An array, of the same kind and element type, consisting of all elements of this array
		  * preceding the element at index, followed by all elements at positions `index + 1` and greater.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index >= length")
		def removed(index :Int) :Arr[E] = expose {
			val length = self.length
			if (index < 0 | index >= length)
				outOfBounds_!(index.toString + " out of " + length)
			Array.copyOfRanges(self, 0, index, self, index + 1, length)
		}

		/** Removes a slice from this array, copying all remaining elements
		  * to a new array of the same kind and element type.
		  * @return `take(from) ++ drop(until)`, but in one step.
		  */
		def removed(from :Int, until :Int) :Arr[E] = expose {
			val length = self.length
			if (until <= from | until <= 0 || from > length)
				ArrayFactory.copyOf(self)
			else
				Array.copyOfRanges(self, 0, from, self, until, length)
		}


		//We can't have any updated/inserted/appended methods, because they either build an array based on a ClassTag
		// (wrong for RefArrayLike), or assume an elem :E can be stored in an identical array (wrong for IArray).


		/** A new array of the same kind and length, where every element pair at indices `(i, length - i - 1)`
		  * are swapped in place.
		  */ //duplicated from ArrayOps because we want to overload it.
		def reverse :Arr[E] = expose {
			var i     = self.length
			val res   = Array.of(self.getClass.getComponentType.castParam[Unknown], i)
			val array = self
			var j = 0
			while (i > 0) {
				i -= 1
				res(j) = array(i)
				j += 1
			}
			res
		}

		/** A new array of the same kind and length, with the specified segment reversed.
		  * All values at indices `[0, from)`, and `[until, length)` are the same as in this array.
		  * Both arguments are clipped to the valid range before copying.
		  */ //consider: strict indexing
		def reverse(from :Int, until :Int) :Arr[E] = expose {
			val length = self.length
			if (until <= from | until <= 0 | from >= length)
				Array.copyOf(self, length)
			else {
				val array  = self
				val from0  = math.max(from, 0)
				val until0 = math.min(until, length)
				val res = Array.of(self.getClass.getComponentType.castParam[Unknown], length)
				ArrayLike.copy(self, 0, res, 0, from0)
				ArrayLike.copy(self, until0, res, until0, length - until0)
				var i = from0; var j = until0 - 1
				while (i <= j) {
					res(i) = array(j)
					res(j) = array(i)
					i += 1; j -= 1
				}
				res
			}
		}

		/** Creates a new array of the same length, kind, and element type,
		  * consisting of `this.slice(n, length) ++ this.slice(0, n)`.
		  * If `n` is not in `[0, length)` range, its non-negative remainder from the division by `length` is used.
		  */
		@inline def rotatedLeft(n :Int) :Arr[E] = rotatedLeft(0, self.length)(n)

		/** Creates a new array of the same length, kind, and element type,
		  * with the specified range shifted cyclically right by `n`. The bounds are clipped to `[0, length)` range,
		  * and passing `until <= from + 1` simply returns a copy of this array.
		  * If `n` is not in `[0, length)` range, its non-negative remainder from the division by `length` is used.
		  * @return `this.slice(0, from0) ++ this.slice(n, until0) ++ this.slice(from0, from0 + n) ++ this.slice(until0, length)`,
		  *         where `from0` and `until0` are `from` and `until` clipped to `[0, length)`.
		  */
		def rotatedLeft(from :Int, until :Int)(n :Int) :Arr[E] = expose {
			val length = self.length
			val from0  = math.min(length, math.max(from, 0))
			val until0 = math.min(until, length)
			val range  = until0 - from0
			if (until0 <= from0)
				Array.copyOf(self, length)
			else {
				val pivot  = if (n >= 0) n % range else range + n % range
				val res    = Array.like(self, length)
				arraycopy(self, 0, res, 0, from0)
				arraycopy(self, from0 + pivot, res, from0, range - pivot)
				arraycopy(self, from0, res, until0 - pivot, pivot)
				arraycopy(self, until0, res, until0, length - until0)
				res
			}
		}

		/** Creates a new array of the same length, kind, and element type,
		  * consisting of `this.slice(length - n, length) ++ this.slice(0, length - n)`.
		  * If `n` is not in `[0, length)` range, its non-negative remainder from the division by `length` is used.
		  */
		@inline def rotatedRight(n :Int) :Arr[E] = rotatedRight(0, self.length)(n)

		/** Creates a new array of the same length, kind, and element type, with the specified range
		  * shift cyclically right by `n`. The bounds are clipped to `[0, length)` range,
		  * and passing `until <= from + 1` simply returns a copy of this array.
		  * If `n` is not in `[0, length)` range, its non-negative remainder from the division by `length` is used.
		  * @return `this.slice(0, from0) ++ this.slice(until0 - n, until0) ++ this.slice(from0, until0 - from0 - n) ++ this.slice(until0, length)`,
		  *         where `from0` and `until0` are `from` and `until` clipped to `[0, length)`.
		  */
		def rotatedRight(from :Int, until :Int)(n :Int) :Arr[E] = {
			val length = self.length
			val start  = math.min(length, math.max(from, 0))
			val end    = math.min(length, math.max(from, until))
			if (end <= start)
				expose(Array.copyOf(self, length))
			else if (n < 0)
				rotatedLeft(from, until)(start - end - n) //guards against underflow of -(n == Int.MinValue)
			else
				rotatedLeft(from, until)(end - start - n)
		}

		/** Returns a new array, containing contents of this array shift left by `n` positions.
		  * Values from the range `[n, length)` are copied to range `[0, length - n)`.
		  * Values at `[length - n, length)` will be equal the default value for the element type (zero/null).
		  * If `n` is negative, the result is equivalent to
		  * `this `[[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension.shiftedRight shiftedRight]]` - n`.
		  * If `n >= length`, the call simply creates an uninitialized array of the same length.
		  * @see [[net.noresttherein.sugar.arrays.extensions.ArrayExtension.<<]]
		  */
		def shiftedLeft(n :Int) :Arr[E] = expose {
			val length = self.length
			if (n < 0)
				if (n <= -length) Array.like(self, length)
				else Array.copyOfRange(self, 0, length + n, -n, length)
			else
				if (n >= length) Array.like(self, length)
				else Array.copyOfRange(self, n, length, 0, length)
		}

		/** Returns a new array, containing contents of this array shift left by `n` positions.
		  * Values from the range `[n, length)` are copied to range `[0, length - n)`.
		  * Values at `[length - n, length)` will be equal the default value for the element type (zero/null).
		  * If `n` is negative, the result is equivalent to
		  * `this `[[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension.shiftedLeft shiftedLeft]]` - n`.
		  * If `n >= length`, the call simply creates an uninitialized array of the same length.
		  * @see [[net.noresttherein.sugar.arrays.extensions.ArrayExtension.>>>]]
		  */
		def shiftedRight(n :Int) :Arr[E] = expose {
			val length = self.length
			if (n < 0)
				if (n <= -length) Array.like(self, length)
				else Array.copyOfRange(self, -n, length, 0, length)
			else
				if (n >= length) Array.like(self, length)
				else Array.copyOfRange(self, 0, length - n, n, length)
		}

		@inline def sortWith(lt :(E, E) => Boolean) :Arr[E] =
			expose(new ArrayOps(self).sortWith(lt.castParams[Unknown, Unknown, Boolean]))

		@inline def sortBy[A :Ordering](f :E => A) :Arr[E] =
			expose(new ArrayOps(self).sortBy(f.castParam1[Unknown]))

		@inline def sorted[U >: E :Ordering] :Arr[E] =
			expose(new ArrayOps(self).sorted(implicitly[Ordering[U]].castParam[Unknown]))

		@inline def distinct :Arr[E] = expose(new ArrayOps(self).distinct)
		@inline def distinctBy[A](f :E => A) :Arr[E] =
			expose(new ArrayOps(self).distinctBy(f.castParams[Unknown, A]))

		@inline def slice(from :Int, until :Int) :Arr[E] = expose {
			val len = self.length
			if (until <= 0 | until <= from | from >= len) Array.emptyLike(self)
			else new ArrayOps(self).slice(from, until)
		}
		@inline def take(n :Int) :Arr[E] = slice(0, n)
		@inline def drop(n :Int) :Arr[E] = slice(n, Int.MaxValue)
		@inline def takeRight(n :Int) :Arr[E] = { val l = self.length; slice(l - n, l) }
		@inline def dropRight(n :Int) :Arr[E] = slice(0, self.length - n)
		@inline def takeWhile(p :E => Boolean) :Arr[E] = slice(0, segmentLength(p))
		@inline def dropWhile(p :E => Boolean) :Arr[E] = slice(segmentLength(p), Int.MaxValue)
		@inline def splitAt(n :Int) :(Arr[E], Arr[E]) = (slice(0, n), slice(n, Int.MaxValue))
		@inline def span(p :E => Boolean) :(Arr[E], Arr[E]) = splitAt(segmentLength(p))

		@inline def tail :Arr[E] = expose(new ArrayOps(self).tail)
		@inline def init :Arr[E] = expose(new ArrayOps(self).init)

		@inline def tails :Iterator[Arr[E]] = new ArrayOps(self).tails.castParam[Arr[E]]
		@inline def inits :Iterator[Arr[E]] = new ArrayOps(self).inits.castParam[Arr[E]]
		@inline def grouped(size :Int) :Iterator[Arr[E]] =
			new ArrayOps(self).grouped(size).castParam[Arr[E]]

		@inline def sliding(size :Int, step :Int = 1) :Iterator[Arr[E]] =
			new ArrayOps(self).sliding(size, step).castParam[Arr[E]]

		@inline def combinations(n :Int) :Iterator[Arr[E]] =
			new ArrayOps(self).combinations(n).castParam[Arr[E]]

		@inline def permutations :Iterator[Arr[E]] =
			new ArrayOps(self).permutations.castParam[Arr[E]]

		def iterator :Iterator[E] = ArrayIterator(self).castParam[E]
		def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S with EfficientSplit =
			ArrayStepper(self)(shape.castFrom[StepperShape[E, S], StepperShape[Unknown, S]])

		def reverseIterator :Iterator[E] = ReverseArrayIterator(self).castParam[E]

		@inline def filter(p :E => Boolean) :Arr[E] =
			expose(new ArrayOps(self).filter(p.castFrom[E => Boolean, Unknown => Boolean]))

		@inline def filterNot(p :E => Boolean) :Arr[E] =
			expose(new ArrayOps(self).filterNot(p.castFrom[E => Boolean, Unknown => Boolean]))

		@inline def partition(p :E => Boolean) :(Arr[E], Arr[E]) =
			new ArrayOps(self).partition(p.castFrom[E => Boolean, Unknown => Boolean])
			                  .castFrom[(Array[Unknown], Array[Unknown]), (Arr[E], Arr[E])]

		@inline def collectFirst[A](pf :PartialFunction[E, A]) :Option[A] = self.collectFirst(pf.castParam1[Unknown])

		@inline def groupBy[K](f: E => K) :Map[K, Arr[E]] =
			self.groupBy(f.castParam1[Any]).castFrom[Map[K, Array[Unknown]], Map[K, Arr[E]]]

		@inline def tapEach[U](f :E => U) :Arr[E] = { foreach(f); expose(self) }

		/** Same as [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.foreach foreach]],
		  * defined here because of the presence of an overloaded variant.
		  */
		@inline def foreach[U](f :E => U) :Unit = foreach(0, self.length)(f)

		/** Executes the given function or all elements in the index range `[from, until)`. */
		def foreach[U](from :Int, until :Int)(f :E => U) :Unit =
			ArrayLikeSpecOps.foreach(self, from, until)(f.castParam1[Unknown])

		@inline def zipWithIndex :Arr[(E, Int)] =
			new ArrayOps(self).zipWithIndex.castFrom[Array[(Unknown, Int)], Arr[(E, Int)]]
		@inline def zip[A](that :IterableOnce[A]) :Arr[(E, A)] =
			new ArrayOps(self).zip(that).castFrom[Array[(Unknown, A)], Arr[(E, A)]]

		@inline def lazyZip[A](that: Iterable[A]) :LazyZip2[E, A, Arr[E]] =
			new ArrayOps(self).lazyZip(that).castParams[E, A, Arr[E]]

		@inline def zipAll[U >: E, B](that :Iterable[B], thisElem :U, thatElem :B) :Arr[(U, B)] =
			new ArrayOps(self).zipAll(that, thisElem, thatElem).castFrom[Array[(Any, B)], Arr[(U, B)]]

		@inline def diff[U >: E](that :collection.Seq[U]) :Arr[E] =
			expose(new ArrayOps(self).diff(that.castParam[Unknown]))

		@inline def intersect[U >: E](that :collection.Seq[U]) :Arr[E] =
			expose(new ArrayOps(self).intersect(that.castParam[Unknown]))

		@inline def transpose[A](implicit asArray :E => Arr[A]): Arr[Arr[A]] =
			new ArrayOps(self).transpose(asArray.castParams[Unknown, Array[A]]).castFrom[Array[Array[A]], Arr[Arr[A]]]

		@inline def view :IndexedSeqView[E] = self.view.castParam[E]

		/** A view on the index range `[from, until)` of this array as a sequence.
		  * Slicing of the returned sequence will return similar views, sharing the same underlying array.
		  */
		@inline def subseq(from :Int, until :Int) :collection.IndexedSeq[E] =
			Wrapped.Slice(self, from, until).castParam[E]

		@inline def indices :Range = Range(0, self.length)

		/** Casts this array like value to the backing array. */
		@inline def asArray :Array[_] = self

		@inline def to[C1](factory :Factory[E, C1]) :C1 = self.to(factory.castParam1[Unknown])
		@inline def toSeq :Seq[E] = self.toSeq.asInstanceOf[Seq[E]]
		@inline def toIndexedSeq :IndexedSeq[E] = self.toIndexedSeq.castParam[E]
		@inline def toList :List[E] = self.toList.castParam[E]
		@inline def toVector :Vector[E] = self.toVector.castParam[E]
		@inline def toSet[U >: E] :Set[U] = self.toSet[Any].castParam[U]
		@inline def toMap[K, V](implicit ev :E <:< (K, V)) :Map[K, V] = self.toMap(ev.castParam1[Unknown])
		@inline def toBuffer[U >: E]: Buffer[U] = Buffer.from(self).castParam[U]

		/** Copies this array to a new array with element type specified by the class tag. */
		@inline def toArray[U >: E :ClassTag] :Array[U] =
			self.toArray(classTag[U].castParam[Unknown]).castParam[U]

		/** Copies this array to a new `IArray[A]`, with the element type specified by the class tag. */
		@inline def toIArray[U >: E :ClassTag] :IArray[U] = IArray.copyOf(self.asInstanceOf[ArrayLike[U]]).castParam[U]

		/** Copies this array to a new `RefArray[A]`. */
		@inline def toRefArray[U >: E] :RefArray[U] = RefArray.copyOf(self.asInstanceOf[ArrayLike[U]])

		/** Copies this array to a new `IRefArray[A]`. */
		@inline def toIRefArray[U >: E] :IRefArray[U] = IRefArray.copyOf(self.asInstanceOf[ArrayLike[U]])

		@inline def toOps :collection.IndexedSeqOps[E, ArrayLike, ArrayLike[E]] =
			new ArrayLikeAsSeq(expose(self))

		@inline def wrap(implicit wrapper :ArrayLikeToSeqConversion[E, Arr]) :wrapper.IndexedSeq =
			wrapper(expose(self))

		@inline def copyToArray[U >: E](xs :Array[U]) :Int = copyRangeToArray(xs, 0, 0, Int.MaxValue)
		@inline def copyToArray[U >: E](xs :Array[U], start :Int) :Int = copyRangeToArray(xs, start, 0, Int.MaxValue)
		@inline def copyToArray[U >: E](xs :Array[U], start :Int, len :Int) :Int = copyRangeToArray(xs, start, 0, len)

		/** Copies values from the index range `[from, this.length)` from this array to the argument array.
		  * Copying stops when end of this or the argument array is reached.
		  * This is equivalent to `this.drop(from).copyToArray(xs, 0, len)`, but in one step.
		  * @param xs   destination array.
		  * @param from the index in this array of the first element to copy. Negative is equivalent to zero,
		  *             and, if `from >= this.length`, nothing is copied.
		  */
		@throws[IndexOutOfBoundsException]("if start is less than zero.")
		@inline def copyRangeToArray[U >: E](xs :Array[U], from :Int) :Int =
			copyRangeToArray(xs, 0, from, Int.MaxValue)

		/** Copies values from the index range `[from, from + min(length - from, xs.length - start)`
		  * from this array to the argument array. Copying stops when end of this or the argument array is reached.
		  * This is equivalent to `this.drop(from).copyToArray(xs, start, min(length - from, xs.length - start)`,
		  * but in one step.
		  * @param xs    destination array.
		  * @param start the index in `xs` at which to start writing.
		  * @param from  the index in this array of the first element to copy. Negative is equivalent to zero,
		  *              and, if `from >= this.length`, nothing is copied.
		  * @return the number of elements copied.
		  */
		@throws[IndexOutOfBoundsException]("if start is less than zero.")
		@inline def copyRangeToArray[U >: E](xs :Array[U], start :Int, from :Int) :Int =
			copyRangeToArray(xs, start, from, Int.MaxValue)

		/** Copies values from the index range `[from, from + len)` from this array to the argument array, starting
		  * writing at position `start`. Copying stops when end of this or the argument array is reached,
		  * or after copying `until - from` elements. If `[from, from + len)` range contains indices
		  * outside of `[0, this.length)`, indices out of range are ignored.
		  *
		  * This is equivalent to `this.drop(from).copyToArray(xs, start, len)`, but in one step.
		  * @param xs    destination array.
		  * @param start the index in the destination array to which to write the first of the copied elements.
		  * @param from  the index in this array of the first element to copy. Negative is equivalent to zero,
		  *              and, if `from >= this.length`, nothing is copied.
		  * @param len   the maximum number of elements to copy.
		  * @return the number of elements copied.
		  */
		@throws[IndexOutOfBoundsException]("if start is less than zero.")
		def copyRangeToArray[U >: E](xs :Array[U], start :Int, from :Int, len :Int) :Int =
			if (len <= 0 || from >= self.length || start >= xs.length)
				0
			else {
				val from0  = math.max(from, 0)
				val copied = math.min(len, math.min(xs.length - start, self.length - from0))
				ArrayLike.copy(self, from0, xs, start, copied)
				copied
			}

		/** Copies values from this array to the argument array, starting writing at position `start`.
		  * Copying ends when `this.length`, `xs.length`, or `len` elements are copied, whichever is lesser.
		  * The first element is written at index `start % xs.length`, and if the end of the target array is reached
		  * before any of the above happens, copying resumes from the beginning of the array.
		  * @param xs    the destination array.
		  * @param start the index in the target array to which the first element will be written, modulo `xs.length`.
		  * @return the number of elements copied.
		  * @see [[net.noresttherein.sugar.arrays.extensions.ArrayCompanionExtension.cyclicCopy]]
		  */
		@throws[IndexOutOfBoundsException]("if start is less than zero.")
		@inline def cyclicCopyToArray[U >: E](xs :Array[U], start :Int) :Int =
			cyclicCopyRangeToArray(xs, start, 0, Int.MaxValue)

		/** Copies values from this array to the argument array, starting writing at position `start`.
		  * Copying ends when `this.length`, `xs.length`, or `len` elements are copied, whichever is lesser.
		  * The first element is written at index `start % xs.length`, and if the end of the target array is reached
		  * before any of the above happens, copying resumes from the beginning of the array.
		  * @param xs    the destination array.
		  * @param start the index in the target array to which the first element will be written, modulo `xs.length`.
		  * @param len   the maximum number of elements to copy.
		  * @return the number of elements copied.
		  * @see [[net.noresttherein.sugar.arrays.extensions.ArrayCompanionExtension.cyclicCopy]]
		  */
		@throws[IndexOutOfBoundsException]("if start is less than zero.")
		@inline def cyclicCopyToArray[U >: E](xs :Array[U], start :Int, len :Int) :Int =
			cyclicCopyRangeToArray(xs, start, 0, Int.MaxValue)

		/** Same as `drop(from).`[[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension.cyclicCopyToArray cyclicCopyToArray]]`(xs, start)`,
		  * but in one step.
		  * @return `cyclicCopyRangeToArray(xs, start, from, Int.MaxValue)`
		  */
		@inline def cyclicCopyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int) :Int =
			cyclicCopyRangeToArray(xs, start, from, Int.MaxValue)

		/** Copies values from the index range `[from, from + len)` from this array to the argument array, starting
		  * writing at position `start`. Copying ends when `this.length - from`, `xs.length`, or `len` elements
		  * are copied, whichever is lesser. The first element is written at index `start % xs.length`,
		  * and if the end of the target array is reached before any of the above happens,
		  * copying resumes from the beginning of the array.
		  * @param xs    the destination array.
		  * @param start the index in the target array to which the first element will be written, modulo `xs.length`.
		  * @param from  the index in this array of the first element to copy. Negative is equivalent to zero,
		  *              and, if `from >= this.length`, nothing is copied.
		  * @param len   the maximum number of elements to copy.
		  * @return the number of elements copied.
		  * @see [[net.noresttherein.sugar.arrays.extensions.ArrayCompanionExtension.cyclicCopy]]
		  */
		@throws[IndexOutOfBoundsException]("if start is less than zero.")
		def cyclicCopyRangeToArray[U >: E](xs :Array[U], start :Int, from :Int, len :Int) :Int =
			if (xs.length == 0) //avoid % by zero
				0
			else {
				val copied = math.max(0, math.min(self.length - math.max(from, 0), len))
				Array.cyclicCopyTo(self, from, xs, start % xs.length, copied)
				copied
			}

		@inline def mkString :String = addString(new StringBuilder, "", "", "").result()
		@inline def mkString(separator :String) :String = addString(new StringBuilder, "", separator, "").result()
		@inline def mkString(prefix :String, separator :String, suffix :String) :String =
			addString(new StringBuilder, prefix, separator, suffix).result()

		@inline def addString(b :StringBuilder) :b.type = addString(b, "", "", "")
		@inline def addString(b :StringBuilder, sep :String) :b.type = addString(b, "", sep, "")
		def addString(b :StringBuilder, start :String, sep :String, end :String) :b.type = {
			ArrayLikeSpecOps.addString(self, b.underlying, start, sep, end); b
		}

		/** A `String` representation of this array in the format of Scala collections' `toString`. */
		def contentsString :String = mkString(self.localClassName + "(", ", ", ")")
	}



	/** A type class defining the specific `collection.IndexedSeq` subclass to which
	  * any given `A[X] <: ArrayLike[X]` converts. In particular, for `Array`, `IArray`, each standard value type
	  * has a dedicated instance converting it to the corresponding `mutable.ArraySeq`/`immutable.ArraySeq` subclass.
	  */
	/* This type exists because without it we would need separate implicit conversion methods for each type,
	 * Which would then cause conflict with non-specific conversions to extension classes.
	 * It is either this, or having ByteArrayAsArrayLikeExtension, ByteArrayAsIArrayLikeExtension, and so on.
	 * It also means there is only ArrayLikeToSeq to import, which works for all types,
	 * instead of a couple dozen of methods.
	 */
	sealed abstract class ArrayLikeToSeqConversion[E, A[X]] extends Serializable {
		type IndexedSeq <: collection.IndexedSeq[E]
		def apply(array :A[E]) :IndexedSeq
	}

	@SerialVersionUID(Ver)
	object ArrayLikeToSeqConversion {
		private case object ArrayLikeToSeq extends ArrayLikeToSeqConversion[Unknown, ArrayLike] {
			override type IndexedSeq = collection.IndexedSeq[Unknown]
			override def apply(array :ArrayLike[Unknown]) = ArrayLike.Wrapped(array)
		}
		private case object IArrayLikeToSeq extends ArrayLikeToSeqConversion[Unknown, IArrayLike] {
			override type IndexedSeq = immutable.IndexedSeq[Unknown]
			override def apply(array :IArrayLike[Unknown]) = IArrayLike.Wrapped(array)
		}
		private case object MutableArrayToSeq extends ArrayLikeToSeqConversion[Unknown, MutableArray] {
			override type IndexedSeq = mutable.IndexedSeq[Unknown]
			override def apply(array :MutableArray[Unknown]) = MutableArray.Wrapped(array)
		}
		private case object IRefArrayToSeqProto extends ArrayLikeToSeqConversion[Unknown, IRefArray] {
			override type IndexedSeq = immutable.IndexedSeq[Unknown]
			override def apply(array :IRefArray[Unknown]) = IRefArray.Wrapped(array)
			override def toString = "IRefArrayToSeq"
		}
		private case object RefArrayToSeqProto extends ArrayLikeToSeqConversion[Unknown, RefArray] {
			override type IndexedSeq = mutable.IndexedSeq[Unknown]
			override def apply(array :RefArray[Unknown]) = RefArray.Wrapped(array)
			override def toString = "RefArrayToSeq"
		}
		implicit def genericArrayLikeToSeq[E]
				:ArrayLikeToSeqConversion[E, ArrayLike] { type IndexedSeq = collection.IndexedSeq[E] } =
			ArrayLikeToSeq.asInstanceOf[
				ArrayLikeToSeqConversion[E, ArrayLike] { type IndexedSeq = collection.IndexedSeq[E] }
			]
		implicit def genericIArrayLikeToSeq[E]
				:ArrayLikeToSeqConversion[E, IArrayLike] { type IndexedSeq = immutable.IndexedSeq[E] } =
			ArrayLikeToSeq.asInstanceOf[
				ArrayLikeToSeqConversion[E, IArrayLike] { type IndexedSeq = immutable.IndexedSeq[E] }
			]
		implicit def genericMutableArrayToSeq[E]
				:ArrayLikeToSeqConversion[E, MutableArray] { type IndexedSeq = mutable.IndexedSeq[E] } =
			ArrayLikeToSeq.asInstanceOf[
				ArrayLikeToSeqConversion[E, MutableArray] { type IndexedSeq = mutable.IndexedSeq[E] }
			]
		implicit def IRefArrayToSeq[E]
				:ArrayLikeToSeqConversion[E, IRefArray] { type IndexedSeq = mutable.IndexedSeq[E] } =
			IRefArrayToSeqProto.asInstanceOf[
				ArrayLikeToSeqConversion[E, IRefArray] { type IndexedSeq = mutable.IndexedSeq[E] }
			]
		implicit def RefArrayToSeq[E]
				:ArrayLikeToSeqConversion[E, RefArray] { type IndexedSeq = mutable.IndexedSeq[E] } =
			RefArrayToSeqProto.asInstanceOf[
				ArrayLikeToSeqConversion[E, RefArray] { type IndexedSeq = mutable.IndexedSeq[E] }
			]

		private case object IArrayToSeq extends ArrayLikeToSeqConversion[Any, IArray] {
			override type IndexedSeq = ArraySeq[Any]
			override def apply(array :IArray[Any]) =
				ArraySeq.unsafeWrapArray(array.asInstanceOf[Array[_]])
		}
		private case object AnyRefIArrayToSeq extends ArrayLikeToSeqConversion[AnyRef, IArray] {
			override type IndexedSeq = ArraySeq.ofRef[AnyRef]
			override def apply(array :IArray[AnyRef]) = new ArraySeq.ofRef(array.asInstanceOf[Array[AnyRef]])
		}
		implicit def genericIArrayToSeq[E] :ArrayLikeToSeqConversion[E, IArray] { type IndexedSeq = ArraySeq[E] } =
			IArrayToSeq.asInstanceOf[ArrayLikeToSeqConversion[E, IArray] { type IndexedSeq = ArraySeq[E] }]

		implicit def refIArrayToSeq[E <: AnyRef]
				:ArrayLikeToSeqConversion[E, IArray] { type IndexedSeq = ArraySeq.ofRef[E] } =
			AnyRefIArrayToSeq.asInstanceOf[ArrayLikeToSeqConversion[E, IArray] { type IndexedSeq = ArraySeq.ofRef[E] }]

		implicit case object ByteIArrayToSeq extends ArrayLikeToSeqConversion[Byte, IArray] {
			override type IndexedSeq = ArraySeq.ofByte
			@inline override def apply(array :IArray[Byte]) :ArraySeq.ofByte =
				new ArraySeq.ofByte(array.asInstanceOf[Array[Byte]])
		}
		implicit case object ShortIArrayToSeq extends ArrayLikeToSeqConversion[Short, IArray] {
			override type IndexedSeq = ArraySeq.ofShort
			@inline override def apply(array :IArray[Short]) :ArraySeq.ofShort =
				new ArraySeq.ofShort(array.asInstanceOf[Array[Short]])
		}
		implicit case object CharIArrayToSeq extends ArrayLikeToSeqConversion[Char, IArray] {
			override type IndexedSeq = ArraySeq.ofChar
			@inline override def apply(array :IArray[Char]) :ArraySeq.ofChar =
				new ArraySeq.ofChar(array.asInstanceOf[Array[Char]])
		}
		implicit case object IntIArrayToSeq extends ArrayLikeToSeqConversion[Int, IArray] {
			override type IndexedSeq = ArraySeq.ofInt
			@inline override def apply(array :IArray[Int]) :ArraySeq.ofInt =
				new ArraySeq.ofInt(array.asInstanceOf[Array[Int]])
		}
		implicit case object LongIArrayToSeq extends ArrayLikeToSeqConversion[Long, IArray] {
			override type IndexedSeq = ArraySeq.ofLong
			@inline override def apply(array :IArray[Long]) :ArraySeq.ofLong =
				new ArraySeq.ofLong(array.asInstanceOf[Array[Long]])
		}
		implicit case object FloatIArrayToSeq extends ArrayLikeToSeqConversion[Float, IArray] {
			override type IndexedSeq = ArraySeq.ofFloat
			@inline override def apply(array :IArray[Float]) :ArraySeq.ofFloat =
				new ArraySeq.ofFloat(array.asInstanceOf[Array[Float]])
		}
		implicit case object DoubleIArrayToSeq extends ArrayLikeToSeqConversion[Double, IArray] {
			override type IndexedSeq = ArraySeq.ofDouble
			@inline override def apply(array :IArray[Double]) :ArraySeq.ofDouble =
				new ArraySeq.ofDouble(array.asInstanceOf[Array[Double]])
		}
		implicit case object BooleanIArrayToSeq extends ArrayLikeToSeqConversion[Boolean, IArray] {
			override type IndexedSeq = ArraySeq.ofBoolean
			@inline override def apply(array :IArray[Boolean]) :ArraySeq.ofBoolean =
				new ArraySeq.ofBoolean(array.asInstanceOf[Array[Boolean]])
		}

		private case object ArrayToSeq extends ArrayLikeToSeqConversion[Any, Array] {
			override type IndexedSeq = ArraySeq[Any]
			override def apply(array :Array[Any]) =
				ArraySeq.unsafeWrapArray(array.asInstanceOf[Array[_]])
		}
		private case object AnyRefArrayToSeq extends ArrayLikeToSeqConversion[AnyRef, Array] {
			override type IndexedSeq = mutable.ArraySeq.ofRef[AnyRef]
			override def apply(array :Array[AnyRef]) = new mutable.ArraySeq.ofRef(array)
		}
		implicit def genericArrayToSeq[E] :ArrayLikeToSeqConversion[E, Array] { type IndexedSeq = mutable.ArraySeq[E] } =
			ArrayToSeq.asInstanceOf[ArrayLikeToSeqConversion[E, Array] { type IndexedSeq = mutable.ArraySeq[E] }]

		implicit def refArrayToSeq[E <: AnyRef]
				:ArrayLikeToSeqConversion[E, Array] { type IndexedSeq = mutable.ArraySeq.ofRef[E] } =
			AnyRefArrayToSeq.asInstanceOf[
				ArrayLikeToSeqConversion[E, Array] { type IndexedSeq = mutable.ArraySeq.ofRef[E] }
			]

		implicit case object ByteArrayToSeq extends ArrayLikeToSeqConversion[Byte, Array] {
			override type IndexedSeq = mutable.ArraySeq.ofByte
			@inline override def apply(array :Array[Byte]) :mutable.ArraySeq.ofByte =
				new mutable.ArraySeq.ofByte(array)
		}
		implicit case object ShortArrayToSeq extends ArrayLikeToSeqConversion[Short, Array] {
			override type IndexedSeq = mutable.ArraySeq.ofShort
			@inline override def apply(array :Array[Short]) :mutable.ArraySeq.ofShort =
				new mutable.ArraySeq.ofShort(array)
		}
		implicit case object CharArrayToSeq extends ArrayLikeToSeqConversion[Char, Array] {
			override type IndexedSeq = mutable.ArraySeq.ofChar
			@inline override def apply(array :Array[Char]) :mutable.ArraySeq.ofChar =
				new mutable.ArraySeq.ofChar(array)
		}
		implicit case object IntArrayToSeq extends ArrayLikeToSeqConversion[Int, Array] {
			override type IndexedSeq = mutable.ArraySeq.ofInt
			@inline override def apply(array :Array[Int]) :mutable.ArraySeq.ofInt =
				new mutable.ArraySeq.ofInt(array)
		}
		implicit case object LongArrayToSeq extends ArrayLikeToSeqConversion[Long, Array] {
			override type IndexedSeq = mutable.ArraySeq.ofLong
			@inline override def apply(array :Array[Long]) :mutable.ArraySeq.ofLong =
				new mutable.ArraySeq.ofLong(array)
		}
		implicit case object FloatArrayToSeq extends ArrayLikeToSeqConversion[Float, Array] {
			override type IndexedSeq = mutable.ArraySeq.ofFloat
			@inline override def apply(array :Array[Float]) :mutable.ArraySeq.ofFloat =
				new mutable.ArraySeq.ofFloat(array)
		}
		implicit case object DoubleArrayToSeq extends ArrayLikeToSeqConversion[Double, Array] {
			override type IndexedSeq = mutable.ArraySeq.ofDouble
			@inline override def apply(array :Array[Double]) :mutable.ArraySeq.ofDouble =
				new mutable.ArraySeq.ofDouble(array)
		}
		implicit case object BooleanArrayToSeq extends ArrayLikeToSeqConversion[Boolean, Array] {
			override type IndexedSeq = mutable.ArraySeq.ofBoolean
			@inline override def apply(array :Array[Boolean]) :mutable.ArraySeq.ofBoolean =
				new mutable.ArraySeq.ofBoolean(array)
		}
	}
}






//consider: having only a single IsSeq type class implementation, which creates default IndexedSeq, not array wrappers.
private abstract class ArrayLikeIsSeqTemplate[E, S[X] <: collection.Seq[X], Arr[X] <: ArrayLike[X]]
	extends IsSeq[Arr[E]] with Serializable
{
	override type A = E
	override type C = Arr[E]
	override def toString = this.innerClassName
}

private abstract class ArrayLikeIsSeqOps[E, A[X] <: ArrayLike[X]](array :Array[E])
	extends collection.StrictOptimizedSeqOps[E, collection.Seq, A[E]]
	   with collection.IndexedSeqOps[E, collection.Seq, A[E]]
	   with ArrayIterableOnce[E] with Serializable
{
	private[sugar] override def unsafeArray :Array[_] = array
	private[sugar] override def startIndex :Int = 0
	override def coll = array.asInstanceOf[A[E]]
	override def length = array.length
	override def apply(i :Int) = array(i)
	@nowarn("cat=deprecation")
	override def toIterable = ArrayLike.Wrapped(array)
	override def iterator = ArrayIterator(array)
	override def iterableFactory :SeqFactory[collection.IndexedSeq] = collection.IndexedSeq
}






object VectorArray {
	def unapply[E](elems :IterableOnce[E]) :Maybe[Array[AnyRef]] = elems match {
		case seq :Vector[E] =>
			val array = CheatedAccess.array(seq)
			val length = seq.length
			if (array.length == length) Yes(array) else No
		case _ => No
	}
}

object MatrixBufferArray {
	def unapply[E](elems :IterableOnce[E]) :Maybe[Array[_]] = elems match {
		case seq :MatrixBuffer[E @unchecked] if seq.dim == 1 =>
			val array = seq.data1
			val length = seq.length
			if (length <= array.length - seq.startIndex) Yes(array) else No
		case _ => No
	}
}
