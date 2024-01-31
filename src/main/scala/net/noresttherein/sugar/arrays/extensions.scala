package net.noresttherein.sugar.arrays

import java.lang.System.arraycopy
import java.lang.reflect.Array.newInstance
import java.util.Arrays

import scala.annotation.tailrec
import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.IsSeq
import scala.collection.{ArrayOps, Stepper, StepperShape, View, mutable}
import scala.reflect.{ClassTag, classTag}
import scala.runtime.BoxedUnit

import net.noresttherein.sugar.arrays.ArrayLike.{ArrayLikeExtension, ArrayLikeToSeqConversion}
import net.noresttherein.sugar.arrays.ArrayOrdering.{ByteArrayOrdering, CharArrayOrdering, DoubleArrayIEEEOrdering, DoubleArrayOrdering, DoubleArrayTotalOrdering, FloatArrayIEEEOrdering, FloatArrayOrdering, FloatArrayTotalOrdering, IntArrayOrdering, LongArrayOrdering, ShortArrayOrdering}
import net.noresttherein.sugar.arrays.IArray.{BooleanIArrayExtension, ByteIArrayExtension, CharIArrayExtension, DoubleIArrayExtension, FloatIArrayExtension, IArrayExtension, IntIArrayExtension, LongIArrayExtension, RefIArrayExtension, ShortIArrayExtension}
import net.noresttherein.sugar.arrays.IArrayLike.IArrayLikeExtension
import net.noresttherein.sugar.arrays.IRefArray.IRefArrayExtension
import net.noresttherein.sugar.arrays.MutableArray.MutableArrayExtension
import net.noresttherein.sugar.arrays.RefArray.RefArrayExtension
import net.noresttherein.sugar.arrays.RefArrayLike.RefArrayLikeExtension
import net.noresttherein.sugar.arrays.extensions.{ArrayCompanionExtension, ArrayExtension, ArrayExtensionConversion, ArrayExtensionConversionPrototype, IArrayExtensions, IRefArrayExtensions, RefArrayExtensions}
import net.noresttherein.sugar.casting.{castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.collections.{ArraySlice, ArrayStepper}
import net.noresttherein.sugar.collections.extensions.{IterableOnceExtension, IteratorCompanionExtension, IteratorExtension, StepperCompanionExtension}
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.exceptions.{illegal_!, outOfBounds_!}
import net.noresttherein.sugar.numeric.BitLogic
import net.noresttherein.sugar.reflect.ArrayClass
import net.noresttherein.sugar.reflect.classes
import net.noresttherein.sugar.reflect.extensions.{ClassExtension, classNameMethods}
import net.noresttherein.sugar.reflect.prettyprint.fullNameOf
import net.noresttherein.sugar.typist.{PriorityConversion, Unknown}
import net.noresttherein.sugar.witness.{Ignored, Overload}




/** Collected definitions of implicit conversions providing extension methods to `Array` and all other
  * [[net.noresttherein.sugar.arrays.ArrayLike ArrayLike]] subtypes.
  */
trait extensions extends IArrayExtensions with IRefArrayExtensions with RefArrayExtensions {
	/** Extension methods for arrays. This conversion has priority over the one to [[scala.collection.ArrayOps ArrayOps]]
	  * defined in `Predef`, in order to provide overloads of standard methods, like update.
	  */
	implicit final def ArrayExtension[E] :ArrayExtensionConversion[E] =
		ArrayExtensionConversionPrototype.asInstanceOf[ArrayExtensionConversion[E]]

	@inline implicit final def ByteArrayExtension    :ArrayExtensionConversion[Byte]    = ArrayExtension
	@inline implicit final def ShortArrayExtension   :ArrayExtensionConversion[Short]   = ArrayExtension
	@inline implicit final def CharArrayExtension    :ArrayExtensionConversion[Char]    = ArrayExtension
	@inline implicit final def IntArrayExtension     :ArrayExtensionConversion[Int]     = ArrayExtension
	@inline implicit final def LongArrayExtension    :ArrayExtensionConversion[Long]    = ArrayExtension
	@inline implicit final def DoubleArrayExtension  :ArrayExtensionConversion[Double]  = ArrayExtension
	@inline implicit final def FloatArrayExtension   :ArrayExtensionConversion[Float]   = ArrayExtension
	@inline implicit final def BooleanArrayExtension :ArrayExtensionConversion[Boolean] = ArrayExtension
	@inline implicit final def UnitArrayExtension    :ArrayExtensionConversion[Unit]    = ArrayExtension

	/** Adds extra extension methods to the `Array` object. */
	@inline implicit final def ArrayCompanionExtension(self :Array.type) :ArrayCompanionExtension =
		new ArrayCompanionExtension {}
}




/** Extension classes for working with [[net.noresttherein.sugar.array.ArrayLike ArrayLike]] subtypes
  * (including `Array`s). Implicit conversions granting these extension methods are available, as an exception,
  * from package object [[net.noresttherein.sugar.arrays arrays]].
  */
@SerialVersionUID(Ver)
object extensions {

	//Consider: removing methods shared with IndexedSeqExtension - they are granted by promoting an Array to ArrayAsSeq.
	/** Adds smart proxies to [[java.util.Arrays]] methods and additional mutable and immutable updating methods,
	  * transparently handling Scala's array polymorphism.
	  */
	class ArrayExtension[E] private[arrays] (private val self :Array[E]) extends AnyVal {
		//todo: reorder it consistently with SeqExtension, BufferExtension and mutableIndexedSeqExtension
		//todo: move duplicated methods to ArrayLikeExtension
		//todo: extract all manual iteration to ArrayLikeOps and specialize them.
		//todo: carry over to IArrayExtension whatever can't be moved up to ArrayLikeExtension.
		//consider: renaming to ops
		/** Wraps this array in an adapter to the standard `IndexedSeq` API. */
		def toOps :mutable.IndexedSeqOps[E, RefArray, Array[E]] = new ArrayAsSeq(self)

		/** Casts this array to `IArray[E]`. This always succeeds, but lifting mutation protection
		  * may have undesired consequences if a reference to this array escapes and is modified.
		  */
		@inline def unsafeAsIArray :IArray[E] = self.asInstanceOf[IArray[E]]

		/** Creates an exact copy of this array and returns it as an immutable `IArray[E]`. */
		@inline def toIArray :IArray[E] = ArrayFactory.copyOf(self).asInstanceOf[IArray[E]]

		/** Copies the elements of this array to a new `Array[AnyRef]`, and returns it as an `RefArray[A]`. */
		@inline def toRefArray[A >: E] :RefArray[A] = RefArray.copyOf(self)

		/** Copies the elements of this array to a new `Array[AnyRef]`, and returns it as an `IRefArray[E]`. */
		@inline def toIRefArray :IRefArray[E] = IRefArray.copyOf(self)

		/** Returns `getClass.`[[java.lang.Class.getComponentType getComponentType]]. */
		@inline def getComponentType :Class[E] = self.getClass.getComponentType.castParam[E]
		//clashes with conversion to ArraySeq in predef

		/** Returns `getClass.`[[java.lang.Class.getComponentType getComponentType]] as a `ClassTag`. */
		@inline def getComponentClassTag :ClassTag[E] =
			if (self.getClass.getComponentType eq classOf[Any]) ClassTag.Any.castParam[E]
			else ClassTag[E](self.getClass.getComponentType)


		//Todo: move these methods to bitpicker instead.
		/** Creates a new array of length `max(this.length, other.length)`, which at index `i` contains
		  * `this(i) | that(i)`, for `i = 0, ..., min(this.length - 1, other.length - 1)`.
		  * If the arrays are of different lengths, indices past `min(this.length, other.length)` are set
		  * to the remaining elements from the longer array.
		  */
		def |(other :Array[E])(implicit logic :BitLogic[E]) :Array[E] = logic.forArray.or(self, other)

		/** Creates a new array of length `max(this.length, other.length)`, which at index `i` contains
		  * `this(i) & that(i)`, for `i = 0, ..., min(this.length - 1, other.length - 1)`.
		  * If the arrays are of different lengths, indices past `min(this.length, other.length)` are set
		  * to the remaining elements from the longer array.
		  */
		def &(other :Array[E])(implicit logic :BitLogic[E]) :Array[E] = logic.forArray.and(self, other)

		/** Creates a new array of length `max(this.length, other.length)`, which at index `i` contains
		  * `this(i) ^ that(i)`, for `i = 0, ..., min(this.length - 1, other.length - 1)`.
		  * If the arrays are of different lengths, indices past `min(this.length, other.length)` are set
		  * to the remaining elements from the longer array.
		  */
		def ^(other :Array[E])(implicit logic :BitLogic[E]) :Array[E] = logic.forArray.xor(self, other)

		/** Creates a new array of the same length, consisting of binary negation of this array's elements. */
		def unary_~(implicit logic :BitLogic[E]) :Array[E] = logic.forArray.not(self)

		/** Shifts left the contents of this array by `bits` bits. This may both copy whole elements, and change
		  * individual elements. If type `E` has a constant bit size as all inbuilt numeric types),
		  * and `bits` is its multiple, then this shifts last `this.length - bits / bitSize[E]` whole elements
		  * by `bits / bitSize[E]` positions left, as by
		  * `this `[[net.noresttherein.sugar.collections.extensions.ArrayExtension.shiftedRight shiftedRight]]`(bits / bitSize[E])`.
		  * Otherwise, again for a type of a fixed bit size, the values in the result equal:
		  * {{{
		  *     result(i) = logic.or(
		  *         logic.shiftLeft(this(i + bits / bitSize[E]), bits % bitSize[E]),
		  *         logic.shiftRight(this(i + 1 + bits / bitSize[E]), bitSize[E] - bits % bitSize[E])
		  *     )
		  * }}}
		  * If the element type has a varying bit size (for example, arrays of different lengths), then the binary size
		  * of each element is preserved.
		  * The values on the vacated positions (the rightmost `n - bits` bits of the returned  array,
		  * where `n == logic.forArray.bitLengthOf(this)`) are cleared, i.e set to zero.
		  * This array remains unchanged by the call.
		  */ //todo: all shift methods are untested
		def <<(bits :Long)(implicit logic :BitLogic[E]) :Array[E] = logic.forArray.shiftRight(self, bits)

		/** Shifts right the contents of this array by `bits` bits. This may both copy whole elements, and change
		  * individual elements. If type `E` has a constant bit size (as all inbuilt numeric types),
		  * and `bits` is its multiple, then this shifts first `this.length - bits / bitLength[E]` elements right
		  * by `bits / bitSize[E]`, as per
		  * [[net.noresttherein.sugar.collections.extensions.ArrayExtension.shiftedRight shiftedRight]]`(bits / bitSize[E])`.
		  * Otherwise, again for a type of a fixed bit size, the values in the result equal:
		  * {{{
		  *     result(i) == logic.or(
		  *         logic.shiftLeft(result(i - 1 - bits / bitSize[E]), bits % bitSize[E]),
		  *         logic.shiftRight(result(i - bits / bitSize[E]), bitSize[E] - bits % bitSize[E])
		  *     )
		  * }}}
		  * If the element type has a varying bit size (for example, arrays of different lengths), then the binary size
		  * of each element is preserved.
		  * The values on the vacated positions (the leftmost `n - bits` bits of this array,
		  * where `n == logic.forArray.bitLengthOf(this)`) are cleared, i.e set to zero.
		  * This array remains unchanged by the call.
		  */
		def >>>(bits :Long)(implicit logic :BitLogic[E]) :Array[E] = logic.forArray.shiftLeft(self, bits)

		/** Sets the elements of this array to `this(i) | other(i)` (in terms of `BinaryLogic[E]` type class),
		  * for `i = 0, ..., min(this.length, other.length)`. If this array is longer than the argument,
		  * the remaining indices remain unchanged. If the argument is longer, positions past the length of this array
		  * are ignored.
		  */ //todo: all in place bit logic is untested
		def |=(other :Array[E])(implicit logic :BitLogic[E]) :Unit = logic.orAllInPlace(self, other)

		/** Sets the elements of this array to `this(i) | other(i)` (in terms of `BinaryLogic[E]` type class),
		  * for `i = 0, ..., min(this.length, other.length)`. If this array is longer than the argument,
		  * the remaining indices remain unchanged. If the argument is longer, positions past the length of this array
		  * are ignored.
		  */
		def &=(other :Array[E])(implicit logic :BitLogic[E]) :Unit = logic.andAllInPlace(self, other)

		/** Sets the elements of this array to `this(i) | other(i)` (in terms of `BinaryLogic[E]` type class),
		  * for `i = 0, ..., min(this.length, other.length)`. If this array is longer than the argument,
		  * the remaining indices remain unchanged. If the argument is longer, positions past the length of this array
		  * are ignored.
		  */
		def ^=(other :Array[E])(implicit logic :BitLogic[E]) :Unit = logic.xorAllInPlace(self, other)

		/** Replaces all elements `this(i)` with `~this(i)`, in terms of `BinaryLogic[E]` type class. */
		def negate()(implicit logic :BitLogic[E]) :Unit = logic.notAllInPlace(self)

		/** Shifts left (in place) this array by the specified number of ''bits'', in terms of `BinaryLogic[E]` type class.
		  * This may involve both shifting the whole elements within in the array and shifting individual values.
		  * If the binary size of the element type is constant, then shifting by a multiple of it is equivalent to
		  * [[net.noresttherein.sugar.collections.extensions.ArrayExtension.shiftLeft shiftLeft]]`(bits / bitSize[E])`.
		  * Otherwise, assuming the element size equals `bitSize[E]`, `this <<= n` will set element at position `i` to
		  * {{{
		  *     this(i + bits / bitSize[E]) << bits % bitSize[E] | this(i + n / bitSize[E] + 1) >>> bitSize[E] - n % bitSize[E]
		  * }}}
		  * The upper `logic.forArray.bitSizeOf(this) - bits` bits are cleared.
		  */
		def <<=(bits :Long)(implicit logic :BitLogic[E]) :Unit = logic.forArray.shiftLeft_!(self, bits)

		/** Shifts right (in place) this array by the specified number of ''bits'', in terms of `BinaryLogic[E]` type class.
		  * This may involve both shifting the whole elements within in the array and shifting individual values.
		  * If the binary size of the element type is constant, then shifting by a multiple of it is equivalent to
		  * [[net.noresttherein.sugar.collections.extensions.ArrayExtension.shiftRight shiftRight]]`(bits / bitSize[E])`.
		  * Otherwise, assuming the element size equals `bitSize[E]`, `this >>>= n` will set element at position `i` to
		  * {{{
		  *     this(i - bits / bitSize[E] - 1) << bits % bitSize[E] | this(i - n / bitSize[E]) >>> bitSize[E] - n % bitSize[E]
		  * }}}
		  * The lower `logic.forArray.bitSizeOf(this) - bits` bits are cleared.
		  */
		def >>>=(bits :Long)(implicit logic :BitLogic[E]) :Unit = logic.forArray.shiftRight_!(self, bits)



		//todo: make these methods available for anything with SeqLike type class
		/** Replaces the first occurrence of sequence `pattern` in this array with elements from `patch`.
		  * Returns a new array of element type specified by the implicit `ClassTag[U]`.
		  */
		@inline def replaceFirst[U >: E :ClassTag](pattern :collection.Seq[U], patch :collection.Seq[U]) :Array[U] =
			replaceFirst(pattern, patch, 0)

		/** Replaces the first occurrence of sequence `pattern` in this array at or after index `from`
		  * with elements from `patch`. Returns a new array of element type specified by the implicit `ClassTag[U]`.
		  */
		def replaceFirst[U >: E :ClassTag](pattern :collection.Seq[U], patch :collection.Seq[U], from :Int) :Array[U] = {
			val idx = self.indexOfSlice(pattern, from)
			if (idx < 0)
				ArrayFactory.copyAs[U](self)
			else {
				val length   = self.length
				val replaced = pattern.length
				val patchLen = patch.length
				val res      = Array.copyOfRange[U](self, 0, idx, length - replaced + patchLen)
				patch.copyToArray(res, idx, patchLen)
				ArrayLike.copy(self, idx + replaced, res, idx + patchLen, length - idx - replaced)
				res
			}
		}

		//These two methods could be in MutableExtension because they don't need a ClassTag,
		// but splitting extension methods with the same name between different extension classes causes conflicts.
		/** Replaces the first occurrence of sequence `pattern` in this array with elements from `patch`. */
		@inline def replaceFirst(pattern :collection.Seq[E], patch :collection.Seq[E]) :Array[E] =
			replaceFirst(pattern, patch, 0)

		/** Replaces the first occurrence of sequence `pattern` in this array at or after index `from`
		  * with elements from `patch`.
		  */
		def replaceFirst(pattern :collection.Seq[E], patch :collection.Seq[E], from :Int) :Array[E] = {
			val idx = self.indexOfSlice(pattern, from)
			if (idx < 0)
				self.clone()
			else {
				val length   = self.length
				val replaced = pattern.length
				val patchLen = patch.length
				val res      = Array.copyOfRange(self, 0, idx, length - replaced + patchLen)
				patch.copyToArray(res, idx, patchLen)
				arraycopy(self, idx + replaced, res, idx + patchLen, length - idx - replaced)
				res
			}
		}

		/** Replaces the first occurrence of sequence `pattern` in this array with elements from `patch`.
		  * Returns a new array of element type specified by the implicit `ClassTag[U]`.
		  */
		@inline def replaceFirst[U >: E :ClassTag](pattern :ArrayLike[U], patch :ArrayLike[U]) :Array[U] =
			replaceFirst(pattern, patch, 0)

		/** Replaces the first occurrence of sequence `pattern` in this array at or after index `from`
		  * with elements from `patch`. Returns a new array of element type specified by the implicit `ClassTag[U]`.
		  */ //todo: instead of wrapping, create a shared implementation based on SeqLike type class
		@inline def replaceFirst[U >: E :ClassTag](pattern :ArrayLike[U], patch :ArrayLike[U], from :Int) :Array[U] =
			replaceFirst(ArrayLike.Wrapped(pattern), ArrayLike.Wrapped(patch), from)

		/** Replaces the first occurrence of sequence `pattern` in this array with elements from `patch`. */
		@inline def replaceFirst(pattern :ArrayLike[E], patch :ArrayLike[E]) :Array[E] =
			replaceFirst(pattern, patch, 0)

		/** Replaces the first occurrence of sequence `pattern` in this array at or after index `from`
		  * with elements from `patch`.
		  */
		@inline def replaceFirst(pattern :ArrayLike[E], patch :ArrayLike[E], from :Int) :Array[E] =
			replaceFirst(ArrayLike.Wrapped(pattern), ArrayLike.Wrapped(patch), from)

		/** Replaces all occurrences of sequence `pattern` in this array with `patch`. Returns a new array
		  * of element type accommodating `patch`, regardless of whether any replacement happened.
		  */
		def replaceAll[U >: E :ClassTag](pattern :collection.Seq[U], patch :collection.Seq[U]) :Array[U] = {
			var found = self.indexOfSlice(pattern)
			if (found < 0)
				ArrayFactory.copyAs[U](self)
			else {
				val replaced    = pattern.length
				val patchLength = patch.length
				val skip        = math.max(1, replaced)
				var result      = Iterator.slice(self, 0, found) :++ patch
				var occurrences = 1
				var next        = self.indexOfSlice(pattern, found + skip)
				while (next >= 0) {
					result      = result :++ Iterator.slice(self, found + replaced, next)
					found       = next
					next        = self.indexOfSlice(pattern, found + skip)
					occurrences = occurrences + 1
				}
				val length    = self.length
				val newLength = length + (patchLength - replaced) * occurrences
				if (found + replaced < length)
					result = result :++ ArrayIterator.slice(self, found + replaced, length)
				val array = new Array[U](newLength)
				result.copyToArray(array, 0, newLength)
				array
			}
		}

		/** Replaces all occurrences of sequence `pattern` in this array with `patch`, returning a new array. */
		@inline def replaceAll(pattern :collection.Seq[E], patch :collection.Seq[E]) :Array[E] =
			replaceAll[E](pattern, patch)(getComponentClassTag)

		/** Replaces all occurrences of sequence `pattern` in this array with `patch`. Returns a new array
		  * of element type accommodating `patch`, regardless of whether any replacement happened.
		  */
		@inline def replaceAll[U >: E :ClassTag](pattern :ArrayLike[U], patch :ArrayLike[U]) :Array[U] =
			replaceAll(ArrayLike.Wrapped(pattern), ArrayLike.Wrapped(patch))

		/** Replaces all occurrences of sequence `pattern` in this array with `patch`, returning a new array. */
		@inline def replaceAll(pattern :ArrayLike[E], patch :ArrayLike[E]) :Array[E] =
			replaceAll(ArrayLike.Wrapped(pattern), ArrayLike.Wrapped(patch))



		/** Same as [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.updated updated]], duplicated because
		  * this extension class has strict precedence over `ArrayOps`.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index >= length")
		def updated[U >: E :ClassTag](index :Int, elem :U) :Array[U] = {
			val res = ArrayFactory.copyAs[U](self, self.length)
			res(index) = elem
			res
		}

		/** Same as [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.updated updated]], but does not require
		  * a `ClassTag` and accepts only values of this array's component type.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index >= length")
		def updated(index :Int, elem :E) :Array[E] = {
			val res = ArrayFactory.copyOf(self, self.length)
			res(index) = elem
			res
		}

	//		  * Passing `index >= self.length` simply returns a copy of this array, while passing `index < 0`
	//		  * starts overwriting at `0`, but ignores the first `-index` values.
		/** A copy of this array, with elements starting at `index` overwritten by the given arguments.
		  * Equivalent to `this.updated(index, first +: second +: rest)`, but may be slightly faster, depending
		  * on the variable argument list given.
		  */
		//Can't be named updated, because we can't rename MutableArrayExtension.updateAll to update,
		// as it would clash with Array.update.
		@throws[IndexOutOfBoundsException]("if index < 0 o index + 2 + rest.size > this.length")
		def updatedAll[U >: E :ClassTag](index :Int, first :U, second :U, rest :U*) :Array[U] = {
			val length     = self.length
			val restSize   = rest.knownSize
			val copiedThis = restSize >= 0 && restSize < (length >> 2)
			val res =
				if (copiedThis) ArrayFactory.copyOf[U](self, length)
				else new Array[U](length)
			val copied = res.updateAll(index, first, second, rest :_*)
			if (!copiedThis) {
				ArrayLike.copy(self, 0, res, 0, index)
				ArrayLike.copy(self, index + copied, res, index + copied, length - index - copied)
			}
			res
		}

		/** A copy of this array, with elements starting at `index` overwritten by the given arguments.
		  * Equivalent to `this.updated(index, first +: second +: rest)`, but may be slightly faster, depending
		  * on the variable argument list given.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 o index + 2 + rest.size > this.length")
		def updatedAll(index :Int, first :E, second :E, rest :E*) :Array[E] =
			updatedAll[E](index, first, second, rest :_*)(getComponentClassTag)

	//		  * Passing `index >= self.length` simply returns a copy of this array, while passing `index < 0`
	//		  * starts overwriting at `0`, but ignores the first `-index` values.
		/** A copy o this array, with elements starting at `index` overwritten by subsequent elements
		  * of the argument collection. If `index >= 0` and `index + elems.size <= length`, this is equivalent to
		  * [[collection.ArrayOps.patch patch]]`(index, elems, elems.size)`, but more efficient
		  * due to a single array allocation.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.size > this.length")
		def updatedAll[U >: E :ClassTag](index :Int, elems :IterableOnce[U]) :Array[U] = {
			//strict indexing implementation
			val length     = self.length
			val size       = elems.knownSize
			val copiedThis = size >= 0 & size <= (length >> 2)
			val res =
				if (copiedThis) ArrayFactory.copyOf[U](self, length)
				else new Array[U](length)
			val copied = res.updateAll(index, elems)
			if (!copiedThis) {
				ArrayLike.copy(self, 0, res, 0, index)
				ArrayLike.copy(self, index + copied, res, index + copied, length - index - copied)
			}
			res
		}

	//		  * Passing `index >= self.length` simply returns a copy of this array, while passing `index < 0`
	//		  * starts overwriting at `0`, but ignores the first `-index` values.
		/** A copy o this array, with elements starting at `index` overwritten by subsequent elements
		  * of the argument collection. If `index >= 0` and `index + elems.size <= length`, this is equivalent to
		  * [[collection.ArrayOps.patch patch]]`(index, elems, elems.size)`, but more efficient
		  * due to a single array allocation.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.length > this.length")
		def updatedAll[U >: E :ClassTag](index :Int, elems :ArrayLike[U]) :Array[U] = {
			val length     = self.length
			val thatLength = elems.length
			if (index < 0 | index > length - thatLength)
				throw new ArrayIndexOutOfBoundsException(
					errorString(self) + ".updatedAll[" + fullNameOf[U] + "](" + index + ", " + errorString(elems) + ")"
				)
			Array.copyOfRanges(self, 0, index, elems, 0, thatLength, self, index + thatLength, length)
		}

		/** A copy o this array, with elements starting at `index` overwritten by subsequent elements
		  * of the argument array. If `index >= 0` and `index + elems.size <= length`, this is equivalent to
		  * [[collection.ArrayOps.patch patch]]`(index, elems, elems.size)`, but more efficient
		  * due to a single array allocation.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.length > this.length")
		def updatedAll[U >: E :Overload](index :Int, elems :Array[U]) :Array[U] =
			updatedAll(index, elems :ArrayLike[U])(ClassTag[U](elems.getClass.getComponentType))

		/** A copy o this array, with elements starting at `index` overwritten by subsequent elements
		  * of the argument collection. If `index >= 0` and `index + elems.size <= length`, this is equivalent to
		  * [[collection.ArrayOps.patch patch]]`(index, elems, elems.size)`, but more efficient
		  * due to a single array allocation.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.size > this.length")
		def updatedAll(index :Int, elems :IterableOnce[E])(implicit __ :Ignored) :Array[E] =
			updatedAll[E](index, elems)(getComponentClassTag)

		//erasure clash with the following method
		/** A copy o this array, with elements starting at `index` overwritten by subsequent elements
		  * of the argument array. If `index >= 0` and `index + elems.size <= length`, this is equivalent to
		  * [[collection.ArrayOps.patch patch]]`(index, elems, elems.size)`, but more efficient
		  * due to a single array allocation.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.length > this.length")
		def updatedAll(index :Int, elems :ArrayLike[E])(implicit __ :Ignored) :Array[E] =
			updatedAll[E](index, elems)(getComponentClassTag)

		/** A copy o this array, with elements starting at `index` overwritten by subsequent elements
		  * of the argument collection. If `index >= 0` and `index + elems.size <= length`, this is equivalent to
		  * [[collection.ArrayOps.patch patch]]`(index, elems, elems.size)`, but more efficient
		  * due to a single array allocation.
		  */ //Needed because otherwise there would be ambigouity between elems :ArrayLike[E] and ArrayToSeq(elems).
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.length > this.length")
		def updatedAll(index :Int, elems :Array[E]) :Array[E] = updatedAll(index, elems :ArrayLike[E])



		/** A copy of this array, with elements starting at `index` overwritten by the given arguments.
		  * For indices in range, it is equivalent to `this.updated(index, first +: second +: rest)`,
		  * but may be slightly faster, depending on the variable argument list given. However, if index is out of range,
		  * instead of throwing an exception, it treats this sequence as a view on a slice some infinite sequence,
		  * and instead 'updates' that sequence. Indices out of range are silently ignored,
		  * and only the legal range is actually updated.
		  * @example
		  * {{{
		  *     val array = Array.iterate(0, 5)(0, _ + 1)
		  *     val array.overwritten(-2, -4, -3, -2, -1) //Array(-2, -1, 2, 3, 4)
		  * }}}
		  */
		def overwritten[U >: E :ClassTag](index :Int, first :U, second :U, rest :U*) :Array[U] = {
			val res = overwritten(index + 2, rest)
			if (index >= 0 && index < res.length)
				res(index) = first
			if (index + 1 >= 0 && index + 1 <= res.length)
				res(index + 1) = second
			res
		}

		/** A copy o this array, with elements starting at `index` overwritten by subsequent elements
		  * of the argument collection.
		  * This is the same as [[net.noresttherein.sugar.arrays.extensions.ArrayExtension.updatedAll updatedAll]],
		  * except it behaves differently if `index` is out of range: instead of throwing an exception,
		  * it treats this sequence as a view on a slice some infinite sequence, and instead 'updates' that sequence.
		  * Indices out of range are silently ignored, and only the legal range is actually updated.
		  * @example
		  * {{{
		  *     val array = Array.iterate(0, 5)(0, _ + 1)
		  *     val array.overwritten(-2, Seq(-4, -3, -2, -1)) //Array(-2, -1, 2, 3, 4)
		  * }}}
		  */
		def overwritten[U >: E :ClassTag](index :Int, elems :IterableOnce[U]) :Array[U] = {
			val length     = self.length
			val size       = elems.knownSize
			val copiedThis = size >= 0 & size < (length >> 2)
			val res =
				if (copiedThis) ArrayFactory.copyAs[U](self, length)
				else new Array[U](length)
			val copied = res.overwrite(index, elems)
			if (!copiedThis) {
				ArrayLike.copy(self, 0, res, 0, index)
				ArrayLike.copy(self, index + copied, res, index + copied, length - index - copied)
			}
			res
		}

		/** A copy o this array, with elements starting at `index` overwritten by subsequent elements
		  * of the argument array.
		  * This is the same as [[net.noresttherein.sugar.arrays.extensions.ArrayExtension.updatedAll updatedAll]],
		  * except it behaves differently if `index` is out of range: instead of throwing an exception,
		  * it treats this sequence as a view on a slice some infinite sequence, and instead 'updates' that sequence.
		  * Indices out of range are silently ignored, and only the legal range is actually updated.
		  * @example
		  * {{{
		  *     val array = Array.iterate(0, 5)(0, _ + 1)
		  *     val array.overwritten(-2, Array(-4, -3, -2, -1)) //Array(-2, -1, 2, 3, 4)
		  * }}}
		  */
		def overwritten[U >: E :ClassTag](index :Int, elems :ArrayLike[U]) :Array[U] = {
			val length     = self.length
			val size       = elems.length
			val res =
				if (size <= (length >>> 2)) ArrayFactory.copyAs[U](self, length)
				else new Array[U](length)
			val copied = res.overwrite(index, elems)
			if (size > (length >>> 2)) {
				ArrayLike.copy(self, 0, res, 0, index)
				ArrayLike.copy(self, index + copied, res, index + copied, length - index - copied)
			}
			res
		}

		/** A copy o this array, with elements starting at `index` overwritten by subsequent elements
		  * of the argument array.
		  * This is the same as [[net.noresttherein.sugar.arrays.extensions.ArrayExtension.updatedAll updatedAll]],
		  * except it behaves differently if `index` is out of range: instead of throwing an exception,
		  * it treats this sequence as a view on a slice some infinite sequence, and instead 'updates' that sequence.
		  * Indices out of range are silently ignored, and only the legal range is actually updated.
		  * @example
		  * {{{
		  *     val array = Array.iterate(0, 5)(0, _ + 1)
		  *     val array.overwritten(-2, Array(-4, -3, -2, -1)) //Array(-2, -1, 2, 3, 4)
		  * }}}
		  */
		def overwritten[U >: E :Overload](index :Int, elems :Array[U]) :Array[U] =
			overwritten(index, elems :ArrayLike[U])(ClassTag[U](elems.getClass.getComponentType))

		/** A copy of this array, with elements starting at `index` overwritten by the given arguments.
		  * For indices in range, it is equivalent to `this.updated(index, first +: second +: rest)`,
		  * but may be slightly faster, depending on the variable argument list given. However, if index is out of range,
		  * instead of throwing an exception, it treats this sequence as a view on a slice some infinite sequence,
		  * and instead 'updates' that sequence. Indices out of range are silently ignored,
		  * and only the legal range is actually updated.
		  * @example
		  * {{{
		  *     val array = Array.iterate(0, 5)(0, _ + 1)
		  *     val array.overwritten(-2, -4, -3, -2, -1) //Array(-2, -1, 2, 3, 4)
		  * }}}
		  */
		def overwritten(index :Int, first :E, second :E, rest :E*) :Array[E] =
			overwritten[E](index, first, second, rest :_*)(getComponentClassTag)

		/** A copy o this array, with elements starting at `index` overwritten by subsequent elements
		  * of the argument collection.
		  * This is the same as [[net.noresttherein.sugar.arrays.extensions.ArrayExtension.updatedAll updatedAll]],
		  * except it behaves differently if `index` is out of range: instead of throwing an exception,
		  * it treats this sequence as a view on a slice some infinite sequence, and instead 'updates' that sequence.
		  * Indices out of range are silently ignored, and only the legal range is actually updated.
		  * @example
		  * {{{
		  *     val array = Array.iterate(0, 5)(0, _ + 1)
		  *     val array.overwritten(-2, Seq(-4, -3, -2, -1)) //Array(-2, -1, 2, 3, 4)
		  * }}}
		  */
		def overwritten(index :Int, elems :IterableOnce[E])(implicit __ :Ignored) :Array[E] =
			overwritten[E](index, elems)(getComponentClassTag)

		/** A copy o this array, with elements starting at `index` overwritten by subsequent elements
		  * of the argument array.
		  * This is the same as [[net.noresttherein.sugar.arrays.extensions.ArrayExtension.updatedAll updatedAll]],
		  * except it behaves differently if `index` is out of range: instead of throwing an exception,
		  * it treats this sequence as a view on a slice some infinite sequence, and instead 'updates' that sequence.
		  * Indices out of range are silently ignored, and only the legal range is actually updated.
		  * @example
		  * {{{
		  *     val array = Array.iterate(0, 5)(0, _ + 1)
		  *     val array.overwritten(-2, Array(-4, -3, -2, -1)) //Array(-2, -1, 2, 3, 4)
		  * }}}
		  */
		def overwritten(index :Int, elems :ArrayLike[E])(implicit __ :Ignored) :Array[E] =
			overwritten[E](index, elems)(getComponentClassTag)

		/** A copy o this array, with elements starting at `index` overwritten by subsequent elements
		  * of the argument array.
		  * This is the same as [[net.noresttherein.sugar.arrays.extensions.ArrayExtension.updatedAll updatedAll]],
		  * except it behaves differently if `index` is out of range: instead of throwing an exception,
		  * it treats this sequence as a view on a slice some infinite sequence, and instead 'updates' that sequence.
		  * Indices out of range are silently ignored, and only the legal range is actually updated.
		  * @example
		  * {{{
		  *     val array = Array.iterate(0, 5)(0, _ + 1)
		  *     val array.overwritten(-2, Array(-4, -3, -2, -1)) //Array(-2, -1, 2, 3, 4)
		  * }}}
		  */
		def overwritten(index :Int, elems :Array[E]) :Array[E] = overwritten(index, elems :ArrayLike[E])



		/** A copy of this array, with the element inserted at a given position in this array,
		  * and all elements at positions equal or greater than `index` by one element further.
		  * This is equivalent to [[collection.ArrayOps.patch patch]] with a singleton collection
		  * and `replaced` equal to zero, but the index must be in the valid range for this array.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  * @return `take(index) :+ elem :++ drop(index)`, but in a more efficient manner.
		  */ //todo: permissive indexing
		@throws[IndexOutOfBoundsException]("if index < 0 or index > length")
		def inserted[U >: E :ClassTag](index :Int, elem :U) :Array[U] = {
			val length = self.length
			if (index < 0 || index > length)
				throw new ArrayIndexOutOfBoundsException(
					s"${self.className}|${self.length}|.inserted[${fullNameOf[U]}]($index, $elem)"
				)
			else if (length == 0)
				Array(elem)
			else {
				val res =
					if (index < length)
						Array.copyOfRanges(self :ArrayLike[U], 0, index + 1, self, index, length)
					else
						Array.copyOfRanges(self :ArrayLike[U], 0, index, self, index - 1, length)
	//				val res = Array.copyOfRange(self :ArrayLike[A], 0, index, length + 1)
	//				ArrayLike.copy(self, index, res, index + 1, length - index)
				res(index) = elem
				res
			}
		}

		/** A copy of this array, with the element inserted at a given position in this array,
		  * and all elements at positions equal or greater than `index` by one element further.
		  * This is equivalent to [[collection.ArrayOps.patch patch]] with a singleton collection
		  * and `replaced` equal to zero, but the index must be in the valid range for this array.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  * @return `take(index) :+ elem :++ drop(index)`, but in a more efficient manner.
		  */ //todo: permissive indexing
		@throws[IndexOutOfBoundsException]("if index < 0 or index > length")
		def inserted(index :Int, elem :E) :Array[E] = inserted[E](index, elem)(getComponentClassTag)

		/** A copy with this array with `first`, `second` and all elements in `rest` inserted
		  * starting with position `index`, followed by `this(index)` and all subsequent elements.
		  * It is similar [[collection.ArrayOps.patch patch]]`(index, first +: second +: rest, 0)`,
		  * but requires the index to be in `[0, length]` range.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		def insertedAll[U >: E :ClassTag](index :Int, first :U, second :U, rest :U*) :Array[U] = {
			if (index < 0 || index > self.length)
				throw new ArrayIndexOutOfBoundsException(
					s"${self.className}|${self.length}|.inserted[${fullNameOf[U]}]($index, _, _, _:*)"
				)
			val restSize = rest.knownSize
			if (restSize >= 0) {
				val result = new Array[U](self.length + 2 + restSize)
				ArrayLike.copy(self, 0, result, 0, index)
				result(index) = first
				result(index + 1) = second
				rest.copyToArray(result, index + 2, restSize)
				ArrayLike.copy(self, index, result, index + restSize + 2, self.length - index)
				result
			} else {
				val res = Array.newBuilder[U]
	//				val res = ArrayAsSeq.ArrayBuilder.
				res sizeHint self.length + 2
				if (index == self.length)
					res addAll self
				else if (index > 0)
					res.addAll(self, 0, index)
				res += first += second ++= rest
				if (index < self.length)
					res.addAll(self, index, self.length - index)
				res.result()
			}
		}

		/** A copy with this array with `first`, `second` and all elements in `rest` inserted
		  * starting with position `index`, followed by `this(index)` and all subsequent elements.
		  * It is similar [[collection.ArrayOps.patch patch]]`(index, first +: second +: rest, 0)`,
		  * but requires the index to be in `[0, length]` range.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		def insertedAll(index :Int, first :E, second :E, rest :E*) :Array[E] =
			insertedAll[E](index, first, second, rest :_*)(getComponentClassTag)

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * It is similar [[collection.ArrayOps.patch patch]]`(index, elems, 0)`,
		  * but requires the index to be in `[0, length]` range.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		def insertedAll[U >: E :ClassTag](index :Int, elems :IterableOnce[U]) :Array[U] =
			if (index < 0 || index > self.length)
				throw new ArrayIndexOutOfBoundsException(
					s"${self.className}|${self.length}|.insertedAll[${fullNameOf[U]}]($index, ${elems.className})"
				)
			else
				self.patch(index, elems, 0)

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * It is similar [[collection.ArrayOps.patch patch]]`(index, elems, 0)`,
		  * but requires the index to be in `[0, length]` range.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		def insertedAll[U >: E :ClassTag](index :Int, elems :ArrayLike[U]) :Array[U] =
			if (index < 0 || index > self.length)
				throw new ArrayIndexOutOfBoundsException(
					s"${self.className}|${self.length}|.insertedAll($index, ${elems.className})"
				)
			else
				Array.copyOfRanges(self, 0, index, elems, 0, elems.length, self, index, self.length)

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * It is similar [[collection.ArrayOps.patch patch]]`(index, elems, 0)`,
		  * but requires the index to be in `[0, length]` range.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		def insertedAll[U >: E :Overload](index :Int, elems :Array[U]) :Array[U] =
			insertedAll(index, elems :ArrayLike[U])(ClassTag[U](elems.getClass.getComponentType))

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * It is similar [[collection.ArrayOps.patch patch]]`(index, elems, 0)`,
		  * but requires the index to be in `[0, length]` range.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		def insertedAll(index :Int, elems :IterableOnce[E])(implicit __ :Ignored) :Array[E] =
			insertedAll[E](index, elems)(ClassTag[E](self.getClass.getComponentType))

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * It is similar [[collection.ArrayOps.patch patch]]`(index, elems, 0)`,
		  * but requires the index to be in `[0, length]` range.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		def insertedAll(index :Int, elems :ArrayLike[E])(implicit __ :Ignored) :Array[E] =
			insertedAll[E](index, elems)(ClassTag[E](self.getClass.getComponentType))

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * It is similar [[collection.ArrayOps.patch patch]]`(index, elems, 0)`,
		  * but requires the index to be in `[0, length]` range.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		def insertedAll(index :Int, elems :Array[E]) :Array[E] = insertedAll(index, elems :ArrayLike[E])



		/** Same as [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.appended appended]], duplicated because
		  * this extension class has strict precedence over `ArrayOps`.
		  */
		def appended[U >: E :ClassTag](elem :U) :Array[U] = {
			val res = ArrayFactory.copyAs[U](self, self.length + 1)
			res(self.length) = elem
			res
		}

		/** Same as [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.appended appended]],
		  * but does not require a `ClassTag` and accepts only values of this array's element type.
		  */
		def appended(elem :E) :Array[E] = {
			val res = ArrayFactory.copyOf(self, self.length + 1)
			res(self.length) = elem
			res
		}

		/** An 'exploded' variant of the standard [[collection.ArrayOps.appendedAll appendedAll]].
		  * Equivalent to `appendedAll(first +: second +: rest)`, but possibly slightly more efficient,
		  * depending on the exact class of the ''vararg'' argument.
		  */
		def appendedAll[U >: E :ClassTag](first :U, second :U, rest :U*) :Array[U] = {
			val restSize = rest.knownSize
			if (restSize >= 0) {
				val result = new Array[U](self.length + restSize + 2)
				ArrayLike.copy(self, 0, result, 0, self.length)
				result(self.length) = first
				result(self.length + 1) = second
				rest.copyToArray(result, self.length + 2, restSize)
				result
			} else {
				val result = Array.newBuilder[U]
				result sizeHint self.length + 2
				result ++= mutable.ArraySeq.make(self) += first += second ++= rest
				result.result()
			}
		}

		/** An 'exploded' variant of the standard [[collection.ArrayOps.appendedAll appendedAll]].
		  * Equivalent to `appendedAll(first +: second +: rest)`, but possibly slightly more efficient,
		  * depending on the exact class of the ''vararg'' argument.
		  */
		def appendedAll(first :E, second :E, rest :E*) :Array[E] =
			appendedAll[E](first, second, rest :_*)(ClassTag[E](self.getClass.getComponentType))

		/** Same as [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.appendedAll appendedAll]],
		  * duplicated because this extension class has strict precedence over `ArrayOps`.
		  */
		def appendedAll[U >: E :ClassTag](elems :IterableOnce[U]) :Array[U] = {
			val size = elems.knownSize
			if (size >= 0) {
				val res = new Array[U](self.length + size)
				ArrayLike.copy(self, 0, res, 0, self.length)
				elems.toBasicOps.copyToArray(res, self.length, size)
				res
			} else {
				val res = Array.newBuilder[U]
				res.addAll(self) ++= elems
				res.result()
			}
		}

		/** An overloaded variant of standard
		  * [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.appendedAll appendedAll]], appending the elements
		  * of the specified array.
		  */
		def appendedAll[U >: E :ClassTag](elems :ArrayLike[U]) :Array[U] =
			Array.copyOfRanges(self, 0, self.length, elems, 0, elems.length)

		/** An overloaded variant of standard
		  * [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.appendedAll appendedAll]], appending the elements
		  * of the specified array. The returned array will be of the same type as the argument.
		  */ //Can't use ProperArray/IArray, because IArray[U] may actually be Array[A] forSome A <: E.
		def appendedAll[U >: E :Overload](elems :Array[U]) :Array[U] =
			appendedAll(elems :ArrayLike[U])(ClassTag[U](elems.getClass.getComponentType))

		/** Same as [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.appendedAll appendedAll]],
		  * but does not require a `ClassTag` and accepts only values of this array's element type.
		  */
		def appendedAll(elems :IterableOnce[E])(implicit __ :Ignored) :Array[E] =
			appendedAll[E](elems)(ClassTag[E](self.getClass.getComponentType))

		/** An overloaded, specific variant of standard
		  * [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.appendedAll appendedAll]], appending the elements
		  * of the specified array.
		  */
		def appendedAll(elems :ArrayLike[E])(implicit __ :Ignored) :Array[E] =
			Array.copyOfRanges(self, 0, self.length, elems, 0, elems.length)(
				ClassTag[E](self.getClass.getComponentType)
			)

		/** An overloaded, specific variant of standard
		  * [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.appendedAll appendedAll]], appending the elements
		  * of the specified array.
		  */
		def appendedAll(elems :Array[E]) :Array[E] = Array.copyOfRanges(self, 0, self.length, elems, 0, elems.length)



		/** Same as [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.prepended prepended]], duplicated because
		  * this extension class has strict precedence over `ArrayOps`.
		  */
		def prepended[U >: E :ClassTag](elem :U) :Array[U] = {
			val res = new Array[U](self.length + 1)
			ArrayLike.copy(self, 0, res, 1, self.length)
			res(0) = elem
			res
		}

		/** Same as [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.prepended prepended]],
		  * but does not require a `ClassTag` and accepts only values of this array's element type.
		  */
		def prepended(elem :E) :Array[E] = {
			val res = Array.like(self, self.length + 1)
			ArrayLike.copy(self, 0, res, 1, self.length)
			res(0) = elem
			res
		}

		/** An 'exploded' variant of the standard [[collection.ArrayOps.prependedAll prependedAll]].
		  * Equivalent to `prependedAll(first +: second +: rest)`, but possibly slightly more efficient,
		  * depending on the exact class of the vararg argument.
		  */
		def prependedAll[U >: E :ClassTag](first :U, second :U, rest :U*) :Array[U] = {
			val restSize = rest.knownSize
			if (restSize >= 0) {
				val result = new Array[U](self.length + restSize + 2)
				result(0) = first
				result(1) = second
				rest.copyToArray(result, 2, restSize)
				ArrayLike.copy(self, 0, result, 2 + restSize, self.length)
				result
			} else {
				val result = Array.newBuilder[U]
				result sizeHint self.length + 2
				result += first += second ++= rest ++= mutable.ArraySeq.make(self)
				result.result()
			}
		}

		/** An 'exploded' variant of the standard [[collection.ArrayOps.prependedAll prependedAll]].
		  * Equivalent to `prependedAll(first +: second +: rest)`, but possibly slightly more efficient,
		  * depending on the exact class of the vararg argument.
		  */
		def prependedAll(first :E, second :E, rest :E*) :Array[E] =
			prependedAll[E](first, second, rest :_*)(ClassTag[E](self.getClass.getComponentType))

		/** Same as [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.prependedAll prependedAll]],
		  * duplicated because this extension class has strict precedence over `ArrayOps`.
		  */
		def prependedAll[U >: E :ClassTag](elems :IterableOnce[U]) :Array[U] = {
			val size = elems.knownSize
			if (size >= 0) {
				val res = new Array[U](self.length + size)
				elems.toBasicOps.copyToArray(res, 0, size)
				ArrayLike.copy(self, 0, res, size, self.length)
				res
			} else {
				val res = Array.newBuilder[U]
				(res ++= elems) addAll self
				res.result()
			}
		}

		/** An overloaded variant of standard
		  * [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.prependedAll prependedAll]],
		  * prepending the elements of the specified array.
		  */
		def prependedAll[U >: E :ClassTag](elems :ArrayLike[U]) :Array[U] =
			Array.copyOfRanges(elems, 0, elems.length, self, 0, self.length)

		/** An overloaded variant of standard
		  * [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.prependedAll prependedAll]],
		  * prepending the elements of the specified array. The returned array will be of the same type as the argument.
		  */
		def prependedAll[U >: E :Overload](elems :Array[U]) :Array[U] =
			prependedAll(elems :ArrayLike[U])(ClassTag[U](elems.getClass.getComponentType))

		/** Same as [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.prependedAll prependedAll]],
		  * but does not requires a `ClassTag` and accepts only values of this array's element type.
		  */
		def prependedAll(elems :IterableOnce[E])(implicit __ :Ignored) :Array[E] =
			prependedAll[E](elems)(ClassTag[E](self.getClass.getComponentType))

		/** An overloaded variant of standard
		  * [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.prependedAll prependedAll]],
		  * prepending the elements of the specified array.
		  */
		def prependedAll(elems :ArrayLike[E])(implicit __ :Ignored) :Array[E] =
			Array.copyOfRanges(elems, 0, elems.length, self, 0, self.length)(
				ClassTag[E](self.getClass.getComponentType)
			)

		/** An overloaded variant of standard
		  * [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.prependedAll prependedAll]],
		  * prepending the elements of the specified array.
		  */
		def prependedAll(elems :Array[E]) :Array[E] = Array.copyOfRanges(elems, 0, elems.length, self, 0, self.length)



		@inline def concat[U >: E :ClassTag](elems :IterableOnce[U])     :Array[U] = appendedAll(elems)
		@inline def concat[U >: E :ClassTag](elems :ArrayLike[U])        :Array[U] = appendedAll(elems)
		@inline def concat[U >: E :Overload](elems :Array[U])            :Array[U] = appendedAll(elems)
		@inline def concat(elems :IterableOnce[E])(implicit __ :Ignored) :Array[E] = appendedAll(elems)
		@inline def concat(elems :ArrayLike[E])(implicit __ :Ignored)    :Array[E] = appendedAll(elems)
		//Can't be an IArray, because we rely on the fact that we can assign E to U, and IArray is covariant.
		@inline def concat(elems :Array[E])                              :Array[E] = appendedAll(elems)


		@inline def +:[U >: E :ClassTag](elem :U) :Array[U] = prepended(elem)
		@inline def +:(elem :E) :Array[E] = prepended(elem)
		@inline def :+[U >: E :ClassTag](elem :U) :Array[U] = appended(elem)
		@inline def :+(elem :E) :Array[E] = appended(elem)

		@inline def :++[U >: E :ClassTag](elems :IterableOnce[U])     :Array[U] = appendedAll(elems)
		@inline def :++[U >: E :ClassTag](elems :ArrayLike[U])        :Array[U] = appendedAll(elems)
		@inline def :++[U >: E :Overload](elems :Array[U])            :Array[U] = appendedAll(elems)
		@inline def :++(elems :IterableOnce[E])(implicit __ :Ignored) :Array[E] = appendedAll(elems)
		@inline def :++(elems :ArrayLike[E])(implicit __ :Ignored)    :Array[E] = appendedAll(elems)
		@inline def :++(elems :Array[E])                              :Array[E] = appendedAll(elems)

		@inline def ++:[U >: E :ClassTag](elems :IterableOnce[U])     :Array[U] = prependedAll(elems)
		@inline def ++:[U >: E :ClassTag](elems :ArrayLike[U])        :Array[U] = prependedAll(elems)
		@inline def ++:[U >: E :Overload](elems :Array[U])            :Array[U] = prependedAll(elems)
		@inline def ++:(elems :IterableOnce[E])(implicit __ :Ignored) :Array[E] = prependedAll(elems)
		@inline def ++:(elems :ArrayLike[E])(implicit __ :Ignored)    :Array[E] = prependedAll(elems)
		@inline def ++:(elems :Array[E])                              :Array[E] = prependedAll(elems)

		@inline def ++[U >: E :ClassTag](elems :IterableOnce[U])     :Array[U] = concat(elems)
		@inline def ++[U >: E :ClassTag](elems :ArrayLike[U])        :Array[U] = concat(elems)
		@inline def ++[U >: E :Overload](elems :Array[U])            :Array[U] = concat(elems)
		@inline def ++(elems :IterableOnce[E])(implicit __ :Ignored) :Array[E] = concat(elems)
		@inline def ++(elems :ArrayLike[E])(implicit __ :Ignored)    :Array[E] = concat(elems)
		@inline def ++(elems :Array[E])                              :Array[E] = concat(elems)

		/** Same as standard [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.patch patch]],
		  * duplicated because this extension has strict precedence over `ArrayOps`.
		  */
		@inline def patch[U >: E :ClassTag](from :Int, elems :IterableOnce[U], replaced :Int) :Array[U] =
			new ArrayOps(self).patch(from, elems, replaced)

		/** Same as standard [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.patch patch]],
		  * duplicated because this extension has strict precedence over `ArrayOps`.
		  */
		@inline def patch(from :Int, elems :IterableOnce[E], replaced :Int) :Array[E] =
			patch[E](from, elems, replaced)(ClassTag[E](self.getClass.getComponentType))

		/** A more efficient version of standard [[collection.ArrayOps.patch patch]] working with an array replacement. */
		def patch[U >: E :ClassTag](from :Int, elems :ArrayLike[U], replaced :Int) :Array[U] = {
			val thisLength = self.length
			val thatLength = elems.length
			if (from <= 0 & replaced >= 0 & from >= thisLength - replaced)
				ArrayFactory.copyOf[U](elems, thatLength)
			else {
				val clippedFrom     = math.min(math.max(from, 0), thisLength)
				val clippedReplaced = math.min(thisLength - clippedFrom, math.max(replaced, 0))
				val clippedUntil    = clippedFrom + clippedReplaced
				val newSize         = thisLength - clippedUntil + clippedFrom + thatLength
				val res             = new Array[U](newSize)
				ArrayLike.copy(self, 0, res, 0, clippedFrom)
				ArrayLike.copy(elems, 0, res, clippedFrom, thatLength)
				ArrayLike.copy(self, clippedUntil, res, clippedFrom + thatLength, thisLength - clippedUntil)
				res
			}
		}

		/** A more efficient version of standard [[collection.ArrayOps.patch patch]] working with an array replacement. */
		@inline def patch(from :Int, elems :ArrayLike[E], replaced :Int) :Array[E] =
			patch[E](from, elems, replaced)(ClassTag[E](self.getClass.getComponentType))

		/** A more efficient version of standard [[collection.ArrayOps.patch patch]] working with an array replacement.
		  * Unlike the standard method, it does requiring a `ClassTag` for the element type, but instead
		  * returns an array of the same type as `elems`.
		  */
		@inline def patch[U >: E :Overload](from :Int, elems :Array[U], replaced :Int) :Array[U] =
			patch(from, elems :ArrayLike[U], replaced)(ClassTag[U](elems.getClass.getComponentType))


		/** A sequence view on the `[from, until)` index range of this array. Any modification to either this array
		  * or the returned sequence will be visible in the other. Further slicing of the sequence also return
		  * views sharing the same underlying array.
		  */
		def subseq(from :Int, until :Int) :mutable.IndexedSeq[E] = ArraySlice.slice(self, from, until)

//		/************ methods copied from ArrayOps only because we shadowed conversion to ArrqyOps ********************/
//
//		@inline def slice(from :Int, until :Int) :Array[E] = new ArrayOps(self).slice(from, until)
	}


	private final val SimpleRangeCopyingFaster = 32

	//Not an anonymous class because we don't want it to be parameterized with Array[Any],
	// as it would cause ClassCastExceptions for primitive arrays
	//todo: Either remove it, or move it directly to extensions, so it is more likely to be imported.
	// The default implementation in IsSeq would otherwise always win.
	private class ArrayIsSeq[E] extends ArrayLikeIsSeqTemplate[E, collection.Seq, Array] {
		override def apply(coll :Array[E]) = //todo: return a specialized sequence
			new MutableArrayIsSeqOps[E, Array](coll) {
				override def newSpecificBuilder = ArrayFactory.newBuilderLike(this.coll)
				override def fromSpecific(coll :IterableOnce[E]) =
					ArrayFactory.from(coll)(ClassTag[E](this.coll.getClass.getComponentType))
	//					Array.from(coll)(ClassTag[E](this.coll.getClass.getComponentType))
			}
		private def readResolve = Array.ArrayIsSeq
	}
	private val arrayIsSeqPrototype = new ArrayIsSeq[Any]
	private val emptyUnitArray = new Array[Unit](0)

	sealed trait ArrayExtensionConversion[E] extends (Array[E] => ArrayExtension[E]) {
		/* We can't override Function1.apply to return ArrayExtension[E] here, because it leads to the error
		 * 'bridge generated for method apply clashes with definition of the member itself'.
		 * The only way to return a value class is through generic code, as in the inherited SpecificConversion.
		 * However, while we need to be a function to be picked up as an implicit conversion,
		 * its application is a purely syntactical replacement of expression `array` with `conversion.apply(array)`.
		 * By adding an overloaded `apply` method in this class, the former resolves to this method, rather than
		 * the inherited one.
		 */
		@inline final def apply(v1 :Array[E])(implicit __ :Ignored) :ArrayExtension[E] = new ArrayExtension(v1)
	}
	private val ArrayExtensionConversionPrototype :ArrayExtensionConversion[Unknown] =
		new PriorityConversion.Wrapped[Array[Unknown], ArrayExtension[Unknown]](new ArrayExtension(_))
			with ArrayExtensionConversion[Unknown]


	/** Extensions methods for object [[Array]] (the array factory). These include:
	  *   1. missing methods from [[scala.collection.SeqFactory SeqFactory]],
	  *   1. same extension methods as for `IterableFactory`:
	  *      [[net.noresttherein.sugar.collections.extensions.IterableFactoryExtension IterableFactoryExtension]]
	  *   1. some adapters of methods in [[java.util.Arrays]], and
	  *   1. Many copying methods, in particular for copying ranges from several arrays into a new array.
	  *      These generally try to look for 'low hanging fruits', perform optimizations for simple cases,
	  *      as well as pick the likely most efficient method based on the array and range lengths.
	  * @see [[net.noresttherein.sugar.arrays.ArrayFactory ArrayFactory]] - an `IterableFactory` for arrays,
	  *      defining also simpler, lower level copying methods.
	  */
	sealed trait ArrayCompanionExtension extends Any {

		/** A type class promoting arrays to sequences. */
		implicit final def ArrayIsSeq[E] :IsSeq[Array[E]] { type A = E; type C = Array[E] } =
			arrayIsSeqPrototype.asInstanceOf[IsSeq[Array[E]] { type A = E; type C = Array[E] }]

		/** Lexicographical ordering of arrays, sadly needing manual importing. If the implicit ordering
		  * is one of standard, natural orderings on a standard value type, a dedicated implementation is returned,
		  * which delegates to low level intrinsic platform code.
		  */
		implicit final def ArrayOrdering[E :Ordering] :Ordering[Array[E]] =
			(implicitly[Ordering[E]] :Ordering[_]) match {
				case Ordering.Int                      => IntArrayOrdering.castParam[E]
				case Ordering.Long                     => LongArrayOrdering.castParam[E]
				case Ordering.Double.TotalOrdering     => DoubleArrayTotalOrdering.castParam[E]
				case Ordering.DeprecatedDoubleOrdering => DoubleArrayTotalOrdering.castParam[E]
				case Ordering.Byte                     => ByteArrayOrdering.castParam[E]
				case Ordering.Char                     => CharArrayOrdering.castParam[E]
				case Ordering.Float.TotalOrdering      => FloatArrayTotalOrdering.castParam[E]
				case Ordering.DeprecatedFloatOrdering  => FloatArrayTotalOrdering.castParam[E]
				case Ordering.Short                    => ShortArrayOrdering.castParam[E]
				case Ordering.Double.IeeeOrdering      => DoubleArrayIEEEOrdering.castParam[E]
				case Ordering.Float.IeeeOrdering       => FloatArrayIEEEOrdering.castParam[E]
				case _                                 => new ArrayOrdering[E]
			}

		/** An `Array[AnyRef]` forced to `Array[Any]`. */
		final def emptyAnyArray :Array[Any] = Array.emptyObjectArray.castFrom[Array[AnyRef], Array[Any]]

		/** An empty `Array[BoxedUnit]`. */
		final def emptyUnitArray :Array[Unit] = extensions.emptyUnitArray

		/** An empty array of the same element type as `original` */
		final def emptyLike[E](original :Array[E]) :Array[E] = ((original :Array[_]) match {
			case _ if original.getClass == classOf[Array[AnyRef]] => Array.emptyObjectArray
			case _ :Array[Unit]                                   => emptyUnitArray
			case a :Array[AnyRef]                                 =>
				java.lang.reflect.Array.newInstance(a.getClass.getComponentType, 0)
			case _ :Array[Int]                                    => Array.emptyIntArray
			case _ :Array[Long]                                   => Array.emptyLongArray
			case _ :Array[Double]                                 => Array.emptyDoubleArray
			case _ :Array[Byte]                                   => Array.emptyByteArray
			case _ :Array[Char]                                   => Array.emptyCharArray
			case _ :Array[Float]                                  => Array.emptyFloatArray
			case _ :Array[Short]                                  => Array.emptyShortArray
			case _ :Array[Boolean]                                => Array.emptyBooleanArray
		}).asInstanceOf[Array[E]]

		/** Creates a new array of the specified length, with the same element type as the original. No data is copied. */
		@inline final def like[E](original :Array[E]) :Array[E] = like(original, original.length)

		/** Creates a new array of the specified length, with the same element type as the original. No data is copied. */
		@throws[NegativeArraySizeException]("if length is negative") //consider; special cases for AnyRef and value types.
		final def like[E](original :Array[E], length :Int) :Array[E] =
			if (length == 0)
				emptyLike(original)
			else if (original.getClass == classOf[Array[AnyRef]])
				new Array[AnyRef](length).castParam[E]
			else
				((original :Array[_]) match {
					case _ :Array[Unit]    => Array.copyOf(emptyUnitArray, length) //fills the array with the Unit value
					case _ :Array[AnyRef]  => of(original.getClass.getComponentType.castParam[E], length)
					case _ :Array[Int]     => new Array[Int](length)
					case _ :Array[Long]    => new Array[Long](length)
					case _ :Array[Double]  => new Array[Double](length)
					case _ :Array[Byte]    => new Array[Byte](length)
					case _ :Array[Char]    => new Array[Char](length)
					case _ :Array[Float]   => new Array[Float](length)
					case _ :Array[Short]   => new Array[Short](length)
					case _ :Array[Boolean] => new Array[Boolean](length)
				}).castParam[E]

		/** An uninitialized array with the specified element type and length. */
		@throws[NegativeArraySizeException]("if length is negative") //consider; special cases for AnyRef and value types.
		final def of[E](elemType :Class[E], length :Int) :Array[E] = elemType match {
			case classes.AnyRef => new Array[AnyRef](length).asInstanceOf[Array[E]]
			case _ if elemType.isPrimitive => (elemType match {
				case classes.Int     => new Array[Int](length)
				case classes.Long    => new Array[Long](length)
				case classes.Double  => new Array[Double](length)
				case classes.Byte    => new Array[Byte](length)
				case classes.Char    => new Array[Char](length)
				case classes.Float   => new Array[Float](length)
				case classes.Short   => new Array[Short](length)
				case classes.Boolean => new Array[Boolean](length)
				case _               => ArrayFactory.ofDim(elemType, length)
			}).asInstanceOf[Array[E]]
			case _ => ArrayFactory.ofDim(elemType, length)
		}


		//fixme: copyOf methods will be invisible because of copyOf in Array
		/** Clones the given array. */
/*
	final def copyOf[E](elems :Array[E]) :Array[E] = {
		val res = java.lang.reflect.Array.newInstance(elems.getClass.getComponentType, elems.length)
		arraycopy(elems, 0, res, 0, elems.length)
		res.asInstanceOf[Array[E]]
	}

	final def copyOf[E :ClassTag](elems :ArrayLike[E]) :Array[E] = {
		val length = elems.length
		val res = new Array[E](length)
		ArrayLike.copy(elems, 0, res, 0, length)
		res
	}

	final def copyOf[E](elems :Array[E], offset :Int, newLength :Int) :Array[E] =
		if (offset < 0)
			throw new ArrayIndexOutOfBoundsException(offset)
		else if (newLength == 0)
			ArrayAsSeq.empty(elems.getClass.getComponentType.castParam[E])
		else {
			val length = elems.length
			val res = of(elems.getClass.getComponentType.castParam[E], newLength)
			if (length > 0)
				arraycopy(elems, 0, res, offset, math.min(length, newLength - offset))
			res
		}

	final def copyOf[E :ClassTag](elems :ArrayLike[E], offset :Int, newLength :Int) :Array[E] =
		if (offset < 0)
			throw new ArrayIndexOutOfBoundsException(offset)
		else if (newLength == 0)
			ArrayAsSeq.empty(elems.getClass.getComponentType.castParam[E])
		else {
			val length = elems.length
			val res = new Array[E](newLength)
			if (length > 0)
				ArrayLike.copy(elems, 0, res, offset, math.min(length, newLength - offset))
			res
		}
*/

		/** Same as `elems.slice(from, until)`, except it reuses empty arrays. */
		final def copyOfRange[E](elems :Array[E], from :Int, until :Int) :Array[E] =
			if (until <= from)
				ArrayFactory.emptyLike(elems)
			else {
				val length = elems.length
				val from0  = math.min(length, math.max(from, 0))
				val until0 = math.min(length, math.max(until, from))
				(((elems :Array[_]): @unchecked) match {
					case a :Array[AnyRef]      => Arrays.copyOfRange(a, from0, until0) //catches also `Unit`
					case a :Array[Int]         => Arrays.copyOfRange(a, from0, until0)
					case a :Array[Long]        => Arrays.copyOfRange(a, from0, until0)
					case a :Array[Double]      => Arrays.copyOfRange(a, from0, until0)
					case a :Array[Byte]        => Arrays.copyOfRange(a, from0, until0)
					case a :Array[Char]        => Arrays.copyOfRange(a, from0, until0)
					case a :Array[Float]       => Arrays.copyOfRange(a, from0, until0)
					case a :Array[Short]       => Arrays.copyOfRange(a, from0, until0)
					case a :Array[Boolean]     => Arrays.copyOfRange(a, from0, until0)
				}).asInstanceOf[Array[E]]
			}

		/** Creates a new `Array[E]` of the specified length, and copies to it the data between indices `from`
		  * (inclusive) and `until` (exclusive), starting writing at index `0`.
		  */
		@inline final def copyOfRange[E](elems :Array[E], from :Int, until :Int, newLength :Int) :Array[E] =
			copyOfRange(elems, from, until, 0, newLength)

		/** Creates a new `Array[E]` - with the same element type as the argument - and copies the data from range
		  * `[from, until)` of the argument to the new array, starting from index `offset`.
		  */ //consider: maybe we should throw an exception if from, until, out of range or offset > newLength?
		@throws[IndexOutOfBoundsException]("if offset is less than zero")
		@throws[NegativeArraySizeException]("if newLength is less than zero")
		//consider: swap the order of offset and newLength arguments
		final def copyOfRange[E](elems :Array[E], from :Int, until :Int, offset :Int, newLength :Int) :Array[E] =
			if (offset < 0)
				throw new ArrayIndexOutOfBoundsException(offset)
			else if (newLength == 0)
				ArrayFactory.empty(elems.getClass.getComponentType.castParam[E])
			else if (until <= from | until <= 0 | offset >= newLength || from >= elems.length)
				like(elems, newLength)
			else if (elems.isInstanceOf[Array[Unit]])
				Array.copyOf(elems, newLength) //fills the array with ()
			else {
				val length = elems.length
				val from0  = math.max(from, 0)
				val until0 = math.min(until, length)
				val copied = math.min(until0 - from0, newLength - offset)
				if (offset == 0 & copied == newLength)
					elems.slice(from0, from0 + copied)
				else if (offset == 0 & from0 == 0 & until0 >= math.min(length, newLength))
					Array.copyOf(elems, newLength)
				else if (from0 >= offset & newLength <= length - from0 & copied >= (newLength >> 1)) {
					val res = elems.slice(from0 - offset, from0 + newLength - offset)
					res.clear(0, offset)
					res.clear(offset + copied, newLength)
					res
				} else if (offset == from0 & newLength <= length & (copied >= (newLength >> 1))) {
					val res = Array.copyOf(elems, newLength)
					res.clear(0, offset)
					res.clear(offset + copied, newLength)
					res
				} else {
					val res = like(elems, newLength)
					arraycopy(elems, from0, res, offset, copied)
					res
				}
			}

		/** Creates a new `Array[E]` - of the element type specified by a `ClassTag[E]` - and copies to it
		  * the values from index range `[from, until)` of the given array.
		  * This is exactly equivalent to `elems.slice(from, until).toArray[E]`, but in one step.
		  */
		@inline final def copyOfRange[E :ClassTag](elems :ArrayLike[E], from :Int, until :Int) :Array[E] =
			copyOfRange(elems, from, until, classTag[E].runtimeClass.castParam[E])

		/** Creates a new `Array[E]` - of the element type specified by `elementClass` - and copies to it
		  * the values from index range `[from, until)` of the given array. If any of the indices is out of range,
		  * it is clipped to a valid value.
		  */ //consider: changing the type of all elementClass arguments to Class[_]
		final def copyOfRange[E](elems :ArrayLike[E], from :Int, until :Int, elementClass :Class[E]) :Array[E] =
			if (until <= from | until <= 0 || from >= elems.length)
				ArrayFactory.empty(elementClass)
			else if (from <= 0)
				ArrayFactory.copyOf(elems, elementClass, until)
			else if ({
	            elementClass.isAssignableFrom(elems.getClass.getComponentType) ||
	               classOf[AnyRef].isAssignableFrom(elementClass) && elems.isInstanceOf[Array[AnyRef]]
			}) {
				val until0 = math.min(until, elems.length)
				((elems :Any) : @unchecked) match {
					case a :Array[Unit]    => ArrayFactory.copyOf[E](a, elementClass, until0 - from)
					case a :Array[AnyRef]  =>
						val arrayClass = ArrayClass(elementClass.castParam[AnyRef])
						java.util.Arrays.copyOfRange(a, from, until0, arrayClass).castFrom[Array[AnyRef], Array[E]]
					case a :Array[Int]     => Arrays.copyOfRange(a, from, until0).castFrom[Array[Int], Array[E]]
					case a :Array[Long]    => Arrays.copyOfRange(a, from, until0).castFrom[Array[Long], Array[E]]
					case a :Array[Double]  => Arrays.copyOfRange(a, from, until0).castFrom[Array[Double], Array[E]]
					case a :Array[Byte]    => Arrays.copyOfRange(a, from, until0).castFrom[Array[Byte], Array[E]]
					case a :Array[Char]    => Arrays.copyOfRange(a, from, until0).castFrom[Array[Char], Array[E]]
					case a :Array[Float]   => Arrays.copyOfRange(a, from, until0).castFrom[Array[Float], Array[E]]
					case a :Array[Short]   => Arrays.copyOfRange(a, from, until0).castFrom[Array[Short], Array[E]]
					case a :Array[Boolean] => Arrays.copyOfRange(a, from, until0).castFrom[Array[Boolean], Array[E]]
				}
			} else if (elementClass == classOf[Unit] | elementClass == classOf[BoxedUnit]) {
				val from0 = math.max(0, from)
				ArrayFactory.ofDim[E](elementClass, until - from0)
			} else {
				val until0 = math.min(until, elems.length)
				val res = newInstance(elementClass, until0 - from).asInstanceOf[Array[E]]
				ArrayLike.copy(elems, from, res, 0, until0 - from)
				res
			}

		/** Creates a new `Array[E]` of the specified length, and copies to it the data between indices `from`
		  * (inclusive) and `until` (exclusive), starting writing at index `0`.
		  */
		@throws[NegativeArraySizeException]("if newLength is negative")
		@inline final def copyOfRange[E :ClassTag](elems :ArrayLike[E], from :Int, until :Int, newLength :Int) :Array[E] =
			copyOfRange(elems, from, until, 0, newLength)

		/** Creates a new `Array[E]` - with an element type defined by the given `ClassTag` - and copies the data
		  * from range `[from, until)` of the argument to the new array, starting from index `offset`.
		  * For some combinations of arguments, it might be faster than creating a new array and copying
		  * the contents afterwords, as the method may choose to use
		  * [[java.util.Arrays java.util.Arrays]]`.`[[java.util.Arrays.copyOfRange copyOfRange]]
		  * or `Arrays.`[[java.util.Arrays.copyOf copyOf]] instead of `Array.copy`.
		  */
		@throws[IndexOutOfBoundsException]("if offset is less than zero")
		@throws[NegativeArraySizeException]("if newLength is negative")
		@inline final def copyOfRange[E :ClassTag](elems :ArrayLike[E], from :Int, until :Int,
		                                           offset :Int, newLength :Int) :Array[E] =
			copyOfRange(elems, from, until, classTag[E].runtimeClass.castParam[E], offset, newLength)

		final def copyOfRange[E](elems :ArrayLike[E], from :Int, until :Int,
		                         elementClass :Class[E], offset :Int, newLength :Int) :Array[E] =
			if (offset < 0)
				throw new ArrayIndexOutOfBoundsException(offset)
			else if (newLength == 0)
				ArrayFactory.empty(elementClass)
//			else if ({ val E = classTag[E].runtimeClass; E == classOf[Unit] || E == classOf[BoxedUnit] })
//				Array.copyOf(ArrayAsSeq.empty[E], newLength)
			else if (until <= from | until <= 0 | offset >= newLength || from >= elems.length)
				ArrayFactory.ofDim(elementClass, newLength)
			else if (elementClass == classOf[Unit] | elementClass == classOf[BoxedUnit])
				ArrayFactory.ofDim(elementClass, newLength)
			else {
				val length = elems.length
				val from0  = math.max(from, 0)
				val until0 = math.min(until, length)
				val copied = math.min(until0 - from0, newLength - offset)
				@inline def simpleCopy = {
					val res = newInstance(elementClass, newLength).asInstanceOf[Array[E]]
					ArrayLike.copy(elems, from0, res, offset, copied)
					res
				}
				if (newLength <= SimpleRangeCopyingFaster)
					simpleCopy
				else if (offset == 0 & copied == newLength)
					copyOfRange(elems, from0, from0 + copied, elementClass)
				else if (offset == 0 & from0 <= 0 & until0 >= math.min(length, newLength))
					ArrayFactory.copyOf(elems, elementClass, newLength)
				else if (from0 >= offset & newLength <= length - from0 & copied >= (newLength >> 1)) {
					val res = copyOfRange(elems, from0 - offset, from0 + newLength - offset, elementClass)
					res.clear(0, offset)
					res.clear(offset + copied, newLength)
					res
				} else if (offset == from0 & newLength <= length & (copied >= (newLength >> 1))) {
					val res = ArrayFactory.copyOf(elems, elementClass, newLength)
					res.clear(0, offset)
					res.clear(offset + copied, newLength)
					res
				} else
					simpleCopy
			}


	//docs mentioning an IndexOutOfBounds
//		/** Copies slices from two array into a new array. Providing `until < from` has the same effect as
//		  * `until == from`, that is copying nothing. However, `untilX > arrayX.length` is treated as if the source array
//		  * were of length `untilX`, and contained zeros/nulls past its actual length.
//		  * Element `array1(from1)` is copied to `result(0)`, and so on, until `array2(from2)`
//		  * is copied to `result(until1 - from1)` (assuming `until1 >= from1`).
//		  * @param array1 The first sliced array.
//		  * @param from1  The index of the element in `array1` to be copied as the first element of the new array.
//		  *               Must be in range `[0, array1.length]`, or an `IndexOutOfBoundsException` will be thrown.
//		  * @param until1 The index after the last copied element in `array1`.
//		  * @param array2 The second sliced array.
//		  * @param from2  The index of the element in `array2` to be copied after `array1(until - 1)` to the new array.
//		  *               Must be in range `[0, array2.length]`, or an `IndexOutOfBoundsException` will be thrown.
//		  * @param until2 The index after the last copied element in `array1`.
//		  * @return An `Array[E]` of length `until1 - from1 + until2 - from2` (for `from1 <= until1 && from2 <= until2`),
//		  *         with the copied slices.
//		  */
		//consider: new methods for two and three ranges copying to a given offset.
		/** Copies slices from two arrays into a new array. Specifying `until < from` has the same effect as
		  * `until == from`, that is copying nothing. However, `untilX > arrayX.length` is treated as if the source array
		  * were of length `untilX`, and contained zeros/nulls past its actual length.
		  * Element `array1(from1)` is copied to `result(0)`, and so on, until `array2(from2)`
		  * is copied to `result(until1 - from1)` (assuming `until1 >= from1`).
		  * @param array1 The first sliced array.
		  * @param from1  The index of the element in `array1` to be copied as the first element of the new array.
		  * @param until1 The index after the last copied element in `array1`.
		  * @param array2 The second sliced array.
		  * @param from2  The index of the element in `array2` to be copied after `array1(until - 1)` to the new array.
		  * @param until2 The index after the last copied element in `array1`.
		  * @return An `Array[E]` of length `until1 - from1 + until2 - from2` (for `from1 <= until1 && from2 <= until2`),
		  *         with the copied slices.
		  */
		@inline final def copyOfRanges[E](array1 :Array[E], from1 :Int, until1 :Int,
		                                  array2 :Array[E], from2 :Int, until2 :Int) :Array[E] =
			copyOfRanges(array1 :ArrayLike[E], from1, until1, array2, from2, until2,
			             array1.getClass.getComponentType.castParam[E]
			)

		/** Copies slices from two arrays into a new array of a given length. Specifying `until < from`
		  * has the same effect as `until == from`, that is copying nothing. However, `untilX > arrayX.length`
		  * is treated as if the source array were of length `untilX`, and contained zeros/nulls past its actual length.
		  * Element `array1(from1)` is copied to `result(0)`, and so on, until `array2(from2)`
		  * is copied to `result(until1 - from1)` (assuming `until1 >= from1`).
		  * @param array1       The first sliced array.
		  * @param from1        The index of the element in `array1` to be copied as the first element of the new array.
		  * @param until1       The index after the last copied element in `array1`.
		  * @param array2       The second sliced array.
		  * @param from2        The index of the element in `array2` to be copied after `array1(until - 1)` to the new array.
		  * @param until2       The index after the last copied element in `array1`.
		  * @param newLength    The length of the created array.
		  * @return An `Array[E]` of length `until1 - from1 + until2 - from2` (for `from1 <= until1 && from2 <= until2`),
		  *         with the copied slices.
		  */
		@inline final def copyOfRanges[E](array1 :Array[E], from1 :Int, until1 :Int,
		                                  array2 :Array[E], from2 :Int, until2 :Int, newLength :Int) :Array[E] =
			copyOfRanges(array1 :ArrayLike[E], from1, until1, array2, from2, until2,
			             array1.getClass.getComponentType.castParam[E], newLength
			)

		/** Copies slices from two arrays into a new array. Specifying `until < from` has the same effect as
		  * `until == from`, that is copying nothing. However, `untilX > arrayX.length` is treated as if the source array
		  * were of length `untilX`, and contained zeros/nulls past its actual length.
		  * Element `array1(from1)` is copied to `result(0)`, and so on, until `array2(from2)`
		  * is copied to `result(until1 - from1)` (assuming `until1 >= from1`).
		  * @param array1 The first sliced array.
		  * @param from1  The index of the element in `array1` to be copied as the first element of the new array.
		  *               Must be in range `[0, array1.length]`, or an `IndexOutOfBoundsException` will be thrown.
		  * @param until1 The index after the last copied element in `array1`.
		  * @param array2 The second sliced array.
		  * @param from2  The index of the element in `array2` to be copied after `array1(until - 1)` to the new array.
		  *               Must be in range `[0, array2.length]`, or an `IndexOutOfBoundsException` will be thrown.
		  * @param until2 The index after the last copied element in `array1`.
		  * @return An `Array[E]` of length `until1 - from1 + until2 - from2` (for `from1 <= until1 && from2 <= until2`),
		  *         with the copied slices.
		  */
		@inline final def copyOfRanges[E :ClassTag](array1 :ArrayLike[E], from1 :Int, until1 :Int,
		                                            array2 :ArrayLike[E], from2 :Int, until2 :Int) :Array[E] =
			copyOfRanges(array1, from1, until1, array2, from2, until2, classTag[E].runtimeClass.castParam[E])

		/** Copies slices from two arrays into a new array of the given element type. Specifying `until < from`
		  * has the same effect as `until == from`, that is copying nothing. However, `untilX > arrayX.length`
		  * is treated as if the source array were of length `untilX`, and contained zeros/nulls past its actual length.
		  * Element `array1(from1)` is copied to `result(0)`, and so on, until `array2(from2)`
		  * is copied to `result(until1 - from1)` (assuming `until1 >= from1`).
		  * @param array1       The first sliced array.
		  * @param from1        The index of the element in `array1` to be copied as the first element of the new array.
		  *                     Must be in range `[0, array1.length]`, or an `IndexOutOfBoundsException` will be thrown.
		  * @param until1       The index after the last copied element in `array1`.
		  * @param array2       The second sliced array.
		  * @param from2        The index of the element in `array2` to be copied after `array1(until - 1)` to the new array.
		  *                     Must be in range `[0, array2.length]`, or an `IndexOutOfBoundsException` will be thrown.
		  * @param until2       The index after the last copied element in `array1`.
		  * @param elementClass The class of the stored elements, determining the class of the created array.
		  * @return An `Array[E]` of length `until1 - from1 + until2 - from2` (for `from1 <= until1 && from2 <= until2`),
		  *         with the copied slices.
		  */
		final def copyOfRanges[E](array1 :ArrayLike[E], from1 :Int, until1 :Int,
		                          array2 :ArrayLike[E], from2 :Int, until2 :Int, elementClass :Class[E]) :Array[E] =
//			if (from1 < 0 | from2 < 0 || from1 > array1.length || from2 > array2.length)
//				throw new ArrayIndexOutOfBoundsException(
//					s"Array.copyOfRanges(${array1.localClassName}<${array1.length}>, $from1, $until1, " +
//						s"${array2.localClassName}<${array2.length}>, $from2, $until2)."
//				)
//			else
		{
			val from1InRange  = math.min(math.max(from1, 0), array1.length)
			val from2InRange  = math.min(math.max(from2, 0), array2.length)
			val until1InRange = math.max(from1InRange, math.min(until1, array1.length))
			val until2InRange = math.max(from2InRange, math.min(until2, array2.length))
			val length1       = until1InRange - from1InRange
			val length2       = until2InRange - from2InRange
			if (length2 > Int.MaxValue - length1)
				illegal_!(
					"Array.copyOfRanges[" + elementClass.name + "](" +
						errorString(array1) + ", " + from1 + ", " + until1 + ", " +
						errorString(array2) + ", " + from2 + ", " + until2 + "): " +
						"cannot create an array of total length " + Integer.toUnsignedString(length1 + length2) + "."
				)
			trustedCopyOfRanges(
				array1, from1InRange, length1, array2, from2InRange, length2, elementClass, length1 + length2,
			)
		}

		/** Copies slices from two arrays into a new array of a given length. Specifying `until < from`
		  * has the same effect as `until == from`, that is copying nothing. However, `untilX > arrayX.length`
		  * is treated as if the source array were of length `untilX`, and contained zeros/nulls past its actual length.
		  * Element `array1(from1)` is copied to `result(0)`, and so on, until `array2(from2)`
		  * is copied to `result(until1 - from1)` (assuming `until1 >= from1`).
		  * @param array1    The first sliced array.
		  * @param from1     The index of the element in `array1` to be copied as the first element of the new array.
		  *                  Must be in range `[0, array1.length]`, or an `IndexOutOfBoundsException` will be thrown.
		  * @param until1    The index after the last copied element in `array1`.
		  * @param array2    The second sliced array.
		  * @param from2     The index of the element in `array2` to be copied after `array1(until - 1)` to the new array.
		  *                  Must be in range `[0, array2.length]`, or an `IndexOutOfBoundsException` will be thrown.
		  * @param until2    The index after the last copied element in `array1`.
		  * @param newLength The length of the created array.
		  * @return An `Array[E]` of length `until1 - from1 + until2 - from2` (for `from1 <= until1 && from2 <= until2`),
		  *         with the copied slices.
		  */
		@inline final def copyOfRanges[E :ClassTag](array1 :ArrayLike[E], from1 :Int, until1 :Int,
		                                            array2 :ArrayLike[E], from2 :Int, until2 :Int, newLength :Int)
				:Array[E] =
			copyOfRanges(array1, from1, until1, array2, from2, until2, classTag[E].runtimeClass.castParam[E], newLength)

		/** Copies slices from two arrays into a new array of a given length and element type. Specifying `until < from`
		  * has the same effect as `until == from`, that is copying nothing. However, `untilX > arrayX.length`
		  * is treated as if the source array were of length `untilX`, and contained zeros/nulls past its actual length.
		  * Element `array1(from1)` is copied to `result(0)`, and so on, until `array2(from2)`
		  * is copied to `result(until1 - from1)` (assuming `until1 >= from1`).
		  * @param array1       The first sliced array.
		  * @param from1        The index of the element in `array1` to be copied as the first element of the new array.
		  *                     Must be in range `[0, array1.length]`, or an `IndexOutOfBoundsException` will be thrown.
		  * @param until1       The index after the last copied element in `array1`.
		  * @param array2       The second sliced array.
		  * @param from2        The index of the element in `array2` to be copied after `array1(until - 1)` to the new array.
		  *                     Must be in range `[0, array2.length]`, or an `IndexOutOfBoundsException` will be thrown.
		  * @param until2       The index after the last copied element in `array1`.
		  * @param elementClass The class of the stored elements, determining the class of the created array.
		  * @param newLength    The length of the created array.
		  * @return An `Array[E]` of length `until1 - from1 + until2 - from2` (for `from1 <= until1 && from2 <= until2`),
		  *         with the copied slices.
		  */
		final def copyOfRanges[E](array1 :ArrayLike[E], from1 :Int, until1 :Int,
		                          array2 :ArrayLike[E], from2 :Int, until2 :Int, elementClass :Class[E], newLength :Int)
				:Array[E] =
//			if (from1 < 0 | from2 < 0 || from1 > array1.length || from2 > array2.length)
//				throw new ArrayIndexOutOfBoundsException(
//					s"Array.copyOfRanges(${array1.localClassName}<${array1.length}>, $from1, $until1, " +
//						s"${array2.localClassName}<${array2.length}>, $from2, $until2)."
//				)
//			else
		{
			val from1InRange  = math.min(math.max(from1, 0), array1.length)
			val from2InRange  = math.min(math.max(from2, 0), array2.length)
			val until1InRange = math.max(from1InRange, math.min(until1, array1.length))
			val until2InRange = math.max(from2InRange, math.min(until2, array2.length))
			val length1       = math.min(until1InRange - from1InRange, math.min(newLength, Int.MaxValue - from1InRange))
			val length2       = math.min(
				until2InRange - from2InRange, math.min(newLength - length1, Int.MaxValue - from2InRange)
			)
			trustedCopyOfRanges(array1, from1InRange, length1, array2, from2InRange, length2, elementClass, newLength)
		}

		private def trustedCopyOfRanges[E](array1 :ArrayLike[E], from1 :Int, length1 :Int,
		                                   array2 :ArrayLike[E], from2 :Int, length2 :Int,
		                                   elementClass :Class[E], newLength :Int) :Array[E] =
		{
			@inline def simpleCopy =
				if (elementClass == classOf[Unit] | elementClass == classOf[BoxedUnit])
					ArrayFactory.ofDim(elementClass, newLength)
				else {
					val res = java.lang.reflect.Array.newInstance(elementClass, newLength).asInstanceOf[Array[E]]
					ArrayLike.copy(array1, from1, res, 0, length1)
					ArrayLike.copy(array2, from2, res, length1, length2)
					res
				}
			if (newLength == 0)
				ArrayFactory.empty[E](elementClass)
			else if (newLength <= SimpleRangeCopyingFaster)
				simpleCopy
			else if (length1 == 0)
				copyOfRange(array2, from2, from2 + length2, elementClass, 0, newLength)
			else if (length2 == 0)
				copyOfRange(array1, from1, from1 + length1, elementClass, 0, newLength)
			else if (from1 == 0 && math.min(newLength, array1.length) - length1 <= (newLength >> 1)) {
				val res = ArrayFactory.copyOf[E](array1, elementClass, newLength)
				ArrayLike.copy(array2, from2, res, length1, length2)
				res.clear(length1 + length2, math.min(newLength, array1.length))
				res
			} else if (length1 >= (newLength >> 1) && array1.length - from1 >= newLength) {
				val res = copyOfRange(array1, from1, from1 + newLength, elementClass)
				ArrayLike.copy(array2, from2, res, length1, length2)
				res.clear(length1 + length2, newLength)
				res
			} else if (from2 == length1 && math.min(newLength, array2.length) - length2 <= (newLength >> 1)) {
				val res = ArrayFactory.copyOf[E](array2, elementClass, newLength)
				ArrayLike.copy(array1, from1, res, 0, length1)
				res.clear(length1 + length2, math.min(newLength, array2.length))
				res
			} else if (from2 >= length1 && array2.length - from2 >= newLength - length1 && length2 >= (newLength >> 1)) {
				val res = copyOfRange(array2, from2 - length1, from2 + newLength - length1, elementClass)
				ArrayLike.copy(array1, from1, res, 0, length1)
				res.clear(length1 + length2, newLength)
				res
			} else
				simpleCopy
		}

	//docs mentioning IndexOutOfBoundsException
//		/** Copies slices from three array into a new array. Providing `until < from` has the same effect as `until == from`,
//		  * that is copying nothing. However, `untilX > arrayX.length` is treated as if the source array
//		  * were of length `untilX`, and contained zeros/nulls past its actual length.
//		  * Element `array1(from1)` is copied to `result(0)`, and so on, with `array2(from2)`
//		  * copied to `result(until1 - from1)`, and `array3(from3)` to `result(until2 - from2 + until1 - from1)`
//		  * (assuming `until1 >= from1`).
//		  * @param array1 The first sliced array.
//		  * @param from1  The index of the element in `array1` to be copied as the first element of the new array.
//		  *               Must be in range `[0, array1.length]`, or an `IndexOutOfBoundsException` will be thrown.
//		  * @param until1 The index after the last copied element in `array1`.
//		  * @param array2 The second sliced array.
//		  * @param from2  The index of the element in `array2` to be copied after `array1(until1 - 1)` into the new array.
//		  *               Must be in range `[0, array2.length]`, or an `IndexOutOfBoundsException` will be thrown.
//		  * @param until2 The index after the last copied element in `array1`.
//		  * @param array3 The third sliced array.
//		  * @param from3  The index of the element in `array3` to be copied after `array2(until2 - 1)` into the new array.
//		  *               Must be in range `[0, array3.length]`, or an `IndexOutOfBoundsException` will be thrown.
//		  * @param until3 The index after the last copied element in `array1`.
//		  * @return An `Array[E]` of length `until1 - from1 + until2 - from2 + until3 - from3` (for `untilN >= fromN`),
//		  *         with the copied slices.
//		  */

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
		  * @return An `Array[E]` of length `until1 - from1 + until2 - from2 + until3 - from3` (for `untilN >= fromN`),
		  *         with the copied slices.
		  */ //we could avoid class tag creation if we had a private mthod accepting a class
		@inline final def copyOfRanges[E](array1 :Array[E], from1 :Int, until1 :Int,
		                                  array2 :Array[E], from2 :Int, until2 :Int,
		                                  array3 :Array[E], from3 :Int, until3 :Int) :Array[E] =
			copyOfRanges(array1 :ArrayLike[E], from1, until1, array2, from2, until2, array3, from3, until3,
				array1.getClass.getComponentType.castParam[E]
			)

		/** Copies slices from three array into a new array. Providing `until < from` has the same effect as `until == from`,
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
		  * @return An `Array[E]` of length `until1 - from1 + until2 - from2 + until3 - from3` (for `untilN >= fromN`),
		  *         with the copied slices.
		  */
		@inline final def copyOfRanges[E](array1 :Array[E], from1 :Int, until1 :Int,
		                                  array2 :Array[E], from2 :Int, until2 :Int,
		                                  array3 :Array[E], from3 :Int, until3 :Int, newLength :Int) :Array[E] =
			copyOfRanges(array1 :ArrayLike[E], from1, until1, array2, from2, until2, array3, from3, until3,
				         array1.getClass.getComponentType.castParam[E], newLength
			)

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
		  * @return An `Array[E]` of length `until1 - from1 + until2 - from2 + until3 - from3` (for `untilN >= fromN`),
		  *         with the copied slices.
		  */
		@inline final def copyOfRanges[E :ClassTag](array1 :ArrayLike[E], from1 :Int, until1 :Int,
		                                            array2 :ArrayLike[E], from2 :Int, until2 :Int,
		                                            array3 :ArrayLike[E], from3 :Int, until3 :Int) :Array[E] =
			copyOfRanges[E](array1, from1, until1, array2, from2, until2, array3, from3, until3,
			                classTag[E].runtimeClass.castParam[E]
			)

		/** Copies slices from three array into a new array of the specified element type. Providing `until < from`
		  * has the same effect as `until == from`, that is copying nothing. However, `untilX > arrayX.length`
		  * is treated as if the source array were of length `untilX`, and contained zeros/nulls past its actual length.
		  * Element `array1(from1)` is copied to `result(0)`, and so on, with `array2(from2)`
		  * copied to `result(until1 - from1)`, and `array3(from3)` to `result(until2 - from2 + until1 - from1)`
		  * (assuming `until1 >= from1`).
		  * @param array1       The first sliced array.
		  * @param from1        The index of the element in `array1` to be copied as the first element of the new array.
		  * @param until1       The index after the last copied element in `array1`.
		  * @param array2       The second sliced array.
		  * @param from2        The index of the element in `array2` to be copied after `array1(until1 - 1)`
		  *                     into the new array.
		  * @param until2       The index after the last copied element in `array1`.
		  * @param array3       The third sliced array.
		  * @param from3        The index of the element in `array3` to be copied after `array2(until2 - 1)`
		  *                     into the new array.
		  * @param until3       The index after the last copied element in `array1`.
		  * @param elementClass The class of the stored elements, determining the class of the created array.
		  * @return An `Array[E]` of length `until1 - from1 + until2 - from2 + until3 - from3` (for `untilN >= fromN`),
		  *         with the copied slices.
		  */
		final def copyOfRanges[E](array1 :ArrayLike[E], from1 :Int, until1 :Int,
		                          array2 :ArrayLike[E], from2 :Int, until2 :Int,
		                          array3 :ArrayLike[E], from3 :Int, until3 :Int, elementClass :Class[E]) :Array[E] =
//			if (from1 < 0 | from2 < 0 | from3 < 0 || from1 > array1.length || from2 > array2.length || from3 > array3
//				.length)
//				throw new ArrayIndexOutOfBoundsException(
//					s"Array.copyOfRanges(${array1.localClassName}<${array1.length}>, $from1, $until1, " +
//						s"${array2.localClassName}<${array2.length}>, $from2, $until2, " +
//						s"${array3.localClassName}<${array3.length}>, $from3, $until3)."
//				)
//			else
		{
			val from1InRange  = math.min(math.max(from1, 0), array1.length)
			val from2InRange  = math.min(math.max(from2, 0), array2.length)
			val from3InRange  = math.min(math.max(from3, 0), array3.length)
			val until1InRange = math.max(from1InRange, math.min(until1, array1.length))
			val until2InRange = math.max(from2InRange, math.min(until2, array2.length))
			val until3InRange = math.max(from3InRange, math.min(until3, array3.length))
			val length1       = until1InRange - from1InRange
			val length2       = until2InRange - from2InRange
			val length3       = until3InRange - from3InRange
			if (length3 > Int.MaxValue - length1 - length2)
				illegal_!(
					"Array.copyOfRanges[" + elementClass.name + "](" +
						errorString(array1) + ", " + from1 + ", " + until1 + ", " +
						errorString(array1) + ", " + from1 + ", " + until1 + ", " +
						errorString(array3) + ", " + from3 + ", " + until3 + "): " +
						"cannot create an array of total length " + (length1.toLong + length2 + length3) + "."
				)
			trustedCopyOfRanges(
				array1, from1InRange, length1,
				array2, from2InRange, length2,
				array3, from3InRange, length3,
				elementClass, length1 + length2 + length3
			)
		}

		/** Copies slices from three array into a new array. Providing `until < from` has the same effect as `until == from`,
		  * that is copying nothing. However, `untilX > arrayX.length` is treated as if the source array
		  * were of length `untilX`, and contained zeros/nulls past its actual length.
		  * Element `array1(from1)` is copied to `result(0)`, and so on, with `array2(from2)`
		  * copied to `result(until1 - from1)`, and `array3(from3)` to `result(until2 - from2 + until1 - from1)`
		  * (assuming `until1 >= from1`).
		  * @param array1    The first sliced array.
		  * @param from1     The index of the element in `array1` to be copied as the first element of the new array.
		  * @param until1    The index after the last copied element in `array1`.
		  * @param array2    The second sliced array.
		  * @param from2     The index of the element in `array2` to be copied after `array1(until1 - 1)`
		  *                  into the new array.
		  * @param until2    The index after the last copied element in `array1`.
		  * @param array3    The third sliced array.
		  * @param from3     The index of the element in `array3` to be copied after `array2(until2 - 1)`
		  *                  into the new array.
		  * @param until3    The index after the last copied element in `array1`.
		  * @param newLength The length of the created array.
		  * @return An `Array[E]` of length `until1 - from1 + until2 - from2 + until3 - from3` (for `untilN >= fromN`),
		  *         with the copied slices.
		  */
		@inline final def copyOfRanges[E :ClassTag](array1 :ArrayLike[E], from1 :Int, until1 :Int,
		                                            array2 :ArrayLike[E], from2 :Int, until2 :Int,
		                                            array3 :ArrayLike[E], from3 :Int, until3 :Int, newLength :Int)
				:Array[E] =
			copyOfRanges[E](array1, from1, until1, array2, from2, until2, array3, from3, until3,
			                classTag[E].runtimeClass.castParam[E], newLength
			)

		/** Copies slices from three array into a new array of the specified length and element type.
		  * Providing `until < from` has the same effect as `until == from`, that is copying nothing.
		  * However, `untilX > arrayX.length` is treated as if the source array were of length `untilX`,
		  * and contained zeros/nulls past its actual length. Element `array1(from1)` is copied to `result(0)`,
		  * and so on, with `array2(from2)` copied to `result(until1 - from1)`,
		  * and `array3(from3)` to `result(until2 - from2 + until1 - from1)` (assuming `until1 >= from1`).
		  * @param array1       The first sliced array.
		  * @param from1        The index of the element in `array1` to be copied as the first element of the new array.
		  * @param until1       The index after the last copied element in `array1`.
		  * @param array2       The second sliced array.
		  * @param from2        The index of the element in `array2` to be copied after `array1(until1 - 1)`
		  *                     into the new array.
		  * @param until2       The index after the last copied element in `array1`.
		  * @param array3       The third sliced array.
		  * @param from3        The index of the element in `array3` to be copied after `array2(until2 - 1)`
		  *                     into the new array.
		  * @param until3       The index after the last copied element in `array1`.
		  * @param elementClass The class of the stored elements, determining the class of the created array.
		  * @param newLength    The length of the created array.
		  * @return An `Array[E]` of length `until1 - from1 + until2 - from2 + until3 - from3` (for `untilN >= fromN`),
		  *         with the copied slices.
		  */
		final def copyOfRanges[E](array1 :ArrayLike[E], from1 :Int, until1 :Int,
		                          array2 :ArrayLike[E], from2 :Int, until2 :Int,
		                          array3 :ArrayLike[E], from3 :Int, until3 :Int,
		                          elementClass :Class[E], newLength :Int) :Array[E] =
//			if (from1 < 0 | from2 < 0 | from3 < 0 || from1 > array1.length || from2 > array2.length || from3 > array3
//				.length)
//				throw new ArrayIndexOutOfBoundsException(
//					s"Array.copyOfRanges(${array1.localClassName}<${array1.length}>, $from1, $until1, " +
//						s"${array2.localClassName}<${array2.length}>, $from2, $until2, " +
//						s"${array3.localClassName}<${array3.length}>, $from3, $until3)."
//				)
//			else
		{
			val from1InRange  = math.min(math.max(from1, 0), array1.length)
			val from2InRange  = math.min(math.max(from2, 0), array2.length)
			val from3InRange  = math.min(math.max(from3, 0), array3.length)
			val until1InRange = math.max(from1InRange, math.min(until1, array1.length))
			val until2InRange = math.max(from2InRange, math.min(until2, array2.length))
			val until3InRange = math.max(from3InRange, math.min(until3, array3.length))
			val length1       = math.min(until1InRange - from1InRange, newLength)
			val length2       = math.min(until2InRange - from2InRange, newLength - length1)
			val length3       = math.min(until3InRange - from3InRange, newLength - length1 - length2)
			trustedCopyOfRanges(
				array1, from1InRange, length1,
				array2, from2InRange, length2,
				array3, from3InRange, length3, elementClass, newLength
			)
		}

		private def trustedCopyOfRanges[E](array1 :ArrayLike[E], from1 :Int, length1 :Int,
		                                   array2 :ArrayLike[E], from2 :Int, length2 :Int,
		                                   array3 :ArrayLike[E], from3 :Int, length3 :Int,
		                                   elementClass :Class[E], newLength :Int) :Array[E] =
		{
			@inline def simpleCopy =
				if (elementClass == classOf[Unit] | elementClass == classOf[BoxedUnit])
					ArrayFactory.ofDim(elementClass, newLength)
				else {
					val res = newInstance(elementClass, newLength).asInstanceOf[Array[E]]
					ArrayLike.copy(array1, from1, res, 0, length1)
					ArrayLike.copy(array2, from2, res, length1, length2)
					ArrayLike.copy(array3, from3, res, length1 + length2, length3)
					res
				}
			val length12 = length1 + length2
			if (newLength == 0)
				ArrayFactory.empty[E](elementClass)
			else if (newLength <= SimpleRangeCopyingFaster) //skip trying to find the optimal method if the array is very short
				simpleCopy
//			else if ({ val E = classTag[E].runtimeClass; E == classOf[Unit] || E == classOf[BoxedUnit] })
//				Array.copyOf(ArrayAsSeq.empty[E], newLength)
			else if (length1 == 0)
				trustedCopyOfRanges(array2, from2, length2, array3, from3, length3, elementClass, newLength)
			else if (length2 == 0)
				trustedCopyOfRanges(array1, from1, length1, array3, from3, length3, elementClass, newLength)
			else if (length3 == 0)
				trustedCopyOfRanges(array1, from1, length1, array2, from2, length2, elementClass, newLength)
			//Try to use copyOf for one of the arrays if the copied range is suitably long,
			//to avoid initializing a new array, and then immediately overwriting when copying.
			else if (from1 == 0 && math.min(newLength, array1.length) - length12 <= (newLength >> 1)) {
				//Use fast Array.copyOf(array1), because less than half of the new array needs to be overwritten.
				val res = ArrayFactory.copyOf[E](array1, elementClass, newLength)
				if ((array1 ne array2) | from2 != length1)
					ArrayLike.copy(array2, from2, res, length1, length2)
				if ((array1 ne array3) | from3 != length12)
					ArrayLike.copy(array3, from3, res, length12, length3)
				res.clear(length1 + length2 + length3, math.min(newLength, array1.length))
				res
			} else if (length1 >= (newLength >> 1) && array1.length - from1 >= newLength) {
				//Like above, but copyOfRange doesn't allow specifying the new length,
				// so we may have to copy more from array1.
				val res = copyOfRange(array1, from1, from1 + newLength, elementClass)
				if ((array1 ne array2) | from2 != from1 + length1)
					ArrayLike.copy(array2, from2, res, length1, length2)
				if ((array1 ne array3) | from3 != from1 + length12)
					ArrayLike.copy(array3, from3, res, length1 + length2, length3)
				res.clear(length1 + length2 + length3, newLength)
				res
			} else if (from2 == length1 && math.min(newLength, array2.length) - length2 <= (newLength >> 1)) {
				//Create a copy of array2 and overwrite the prefix with array1 and suffix with array3 - together less than half.
				val res = ArrayFactory.copyOf[E](array2, elementClass, newLength)
				if ((array2 ne array1) | from1 != 0)
					ArrayLike.copy(array1, from1, res, 0, length1)
				if ((array2 ne array3) | from3 != from2 + length2)
					ArrayLike.copy(array3, from3, res, length1 + length2, length3)
				res.clear(length1 + length2 + length3, math.min(newLength, array2.length))
				res
			} else if (length2 >= (newLength >> 1) && from2 >= length1 && array2.length - from2 >= newLength - length1) {
				//Like above, but we must copy from an index > 0 and copy full newLength elements from array2 before overwriting.
				val res = copyOfRange(array2, from2 - length1, from2 + newLength - length1, elementClass)
				ArrayLike.copy(array1, from1, res, 0, length1)
				ArrayLike.copy(array3, from3, res, length1 + length2, length3)
				res.clear(length1 + length2 + length3, newLength)
				res
			} else if (length3 >= (newLength >> 1) && from3 >= length12 && array3.length - from3 >= newLength - length12) {
				//Make a copy of array3 with enough prefix to overwrite with array1 ++ array2
				val from = from3 - length2 - length1
				val res  = copyOfRange(array3, from, from + newLength, elementClass)
				if ((array3 ne array1) | from1 != from)
					ArrayLike.copy(array1, from1, res, 0, length1)
				if ((array3 ne array2) | from3 != from2 + length2)
					ArrayLike.copy(array2, from2, res, length1, length2)
				res.clear(length1 + length2 + length3, newLength)
				res
			} else
				simpleCopy
		}

//		@inline private def fillIfUnit(array :ArrayLike[_]) :array.type = array match {
//			case units :Array[Unit] => units.fill(()); array
//			case _ => array
//		}


		/** Copies a maximum of `len` elements from one array to another, wrapping at array ends.
		  * Reading starts with index `srcPos` in `src`, and writing starts with index `dstPos` in `dst`.
		  * If an end of either array is reached, reading/writing resumes from the beginning of that array.
		  * This method will never copy the same element twice, or overwrite previously written elements.
		  */
		@throws[IndexOutOfBoundsException]("if srcPos is not in the [0, src.length) range, " +
		                                   "or dstPos is not in the [0, dst.length) range, " +
		                                   "or len > min(src.length, dst.length).")
		@throws[ArrayStoreException]("if any of elements copied from src cannot be stored in dst," +
		                             "including boxing and unboxing.")
		final def cyclicCopy(src :Array[_], srcPos :Int, dst :Array[_], dstPos :Int, len :Int) :Unit = {
			val length1 = src.length
			val length2 = dst.length
			if (srcPos < 0 | srcPos > length1 | dstPos < 0 | dstPos > length2 | len < 0 | len > length1 | len > length2)
				throw new ArrayIndexOutOfBoundsException(
					"Array.cyclicCopy(" + errorString(src) + ", " + srcPos + ", " +
						errorString(dst) + ", " + dstPos + ", " + len + ")"
				)
			var count = len
			if (srcPos != dstPos || (src ne dst))
				if (src eq dst)
					cyclicShift(src.asInstanceOf[Array[Unknown]], srcPos, dstPos, len)
				else {
					var idx1    = srcPos
					var idx2    = dstPos
					while (count > 0) {
						if (idx1 >= idx2) {
							val suffix = math.min(length1 - idx1, count)
							Array.copy(src, idx1, dst, idx2, suffix)
							idx1   = 0
							idx2  += suffix
							count -= suffix
						} else {
							val suffix = math.min(length2 - idx2, count)
							Array.copy(src, idx1, dst, idx2, suffix)
							idx2   = 0
							idx1  += suffix
							count -= suffix
						}
					}
				}
		}

		private def cyclicShift(array :Array[_], srcPos :Int, dstPos :Int, len :Int) :Unit = {
			val length = array.length
			if (srcPos >= dstPos) {
				val srcOverflow = srcPos + len - length
				if (srcOverflow <= 0) //No wrapping, regular arraycopy will work.
					ArrayLike.copy(array, srcPos, array, dstPos, len)
				else if (dstPos + len <= srcPos) { //dst does not wrap
					//Range [srcPos, length) does not overlap with dst: copy first [0, srcEnd), then [srcPos, length)
					ArrayLike.copy(array, 0, array, dstPos + len - srcOverflow, srcOverflow)
					ArrayLike.copy(array, srcPos, array, dstPos, length - srcPos)
				} else if (srcOverflow <= dstPos) {
					//It is safe to copy from srcPos, knowing it will not overwrite anything needing copying.
					val suffix = length - srcPos
					ArrayLike.copy(array, srcPos, array, dstPos, suffix)
					val dstNoWrap = math.min(len, length - dstPos) - suffix
					ArrayLike.copy(array, 0, array, dstPos + suffix, dstNoWrap)
					if (suffix + dstNoWrap < len)
						ArrayLike.copy(array, dstNoWrap, array, 0, len - suffix - dstNoWrap)
				} else {
					//We can't copy sequentially in either direction without overwriting not yet copied data.
					// However, this means that most of the array needs copying, so we'll rotate it left.
					// After we are done copying, range [dstEnd, dstPos) should remain unchanged.
					// When we rotate left the whole array, we'll overwrite it with [dstEnd + shift, dstPos + shift).
					// So if we now copy the former to the latter, after rotation, we'll have the correct result.
					val shift  = srcPos - dstPos
					val dstEnd = dstPos + len - length
					ArrayLike.copy(array, dstEnd, array, dstEnd + shift, length - len)
					array.rotateLeft(shift)
				}
			} else { //srcPos < dstPos: largely symmetrical to the above
				val dstOverflow = dstPos + len - length
				if (dstOverflow <= 0) //No wrapping
					ArrayLike.copy(array, srcPos, array, dstPos, len)
				else if (srcPos + len <= dstPos) { //src does not wrap
					//Range [dstPos, length) does not overlap with src: start copying from right to left
					ArrayLike.copy(array, srcPos, array, dstPos, length - dstPos)
					ArrayLike.copy(array, srcPos + length - dstPos, array, 0, dstOverflow)
				} else if (dstOverflow <= srcPos) {
					//Destination area doesn't overlap with [srcPos, length)
					if (srcPos + len <= length)
						ArrayLike.copy(array, srcPos + len - dstOverflow, array, 0, dstOverflow)
					else {
						val srcOverflow = srcPos + len - length
						ArrayLike.copy(array, 0, array, dstOverflow - srcOverflow, srcOverflow)
						ArrayLike.copy(array, length - (dstOverflow - srcOverflow), array, 0, dstOverflow - srcOverflow)
					}
					ArrayLike.copy(array, srcPos, array, dstPos, len - dstOverflow)
				} else {
					val shift = dstPos - srcPos
					val dstEnd = dstPos + len - length
					ArrayLike.copy(array, dstEnd, array, dstEnd - shift, length - len)
					array.rotateRight(shift)
				}
			}
		}

		/** Copies `min(len, src.length, dst.length - dstPos)` elements from one array to another, wrapping at the end
		  * of the source array. Reading starts with index `srcPos` in `src`,
		  * and writing starts with index `dstPos` in `dst`. If the end of array `src` is reached before
		  * reaching the end of `dst` or copying `len` elements, then copying of the remainder restarts with `src(0)`.
	      * If the end of array `dst` reached before copying `length` elements, copying stops.
		  */
		@throws[IndexOutOfBoundsException]("if srcPos is not in the [0, src.length) range, " +
		                                   "or dstPos is not in the [0, dst.Length - len) range, or len > src.length.")
		@throws[ArrayStoreException]("if any of elements copied from src cannot be stored in dst," +
		                             "including boxing and unboxing.")
		final def cyclicCopyFrom(src :Array[_], srcPos :Int, dst :Array[_], dstPos :Int, len :Int) :Unit =
			if (dstPos < 0 | len > dst.length - dstPos)
				throw new ArrayIndexOutOfBoundsException(
					"Array.cyclicCopyFrom(" + errorString(src) + ", " + srcPos + ", " +
						errorString(dst) + ", " + dstPos + ", " + len + ")"
				)
			else
				cyclicCopy(src, srcPos, dst, dstPos, len)

		/** Copies `min(len, src.length - srcPos, dst.length)` elements from one array to another, wrapping at the end
		  * of the target array. Reading starts with index `srcPos` in `src`, and writing starts with index `dstPos`
		  * in `dst`. If the end of array `dst` is reached before reaching the end of `src` or copying `len` elements,
		  * then writing of the remainder restarts with `dst(0)`. If the end of array `src` is reached
		  * before copying `length` elements, the copying stops.
		  */
		@throws[IndexOutOfBoundsException]("if srcPos is not in the [0, src.length - len) range, " +
		                                   "or dstPos is not in the [0, dst.length) range, or len > dst.length.")
		@throws[ArrayStoreException]("if any of elements copied from src cannot be stored in dst," +
		                             "including boxing and unboxing.")
		final def cyclicCopyTo(src :Array[_], srcPos :Int, dst :Array[_], dstPos :Int, len :Int) :Unit =
			if (srcPos < 0 | len > src.length - srcPos)
				throw new ArrayIndexOutOfBoundsException(
					"Array.cyclicCopyTo(" + errorString(src) + ", " + srcPos + ", " +
						errorString(dst) + ", " + dstPos + ", " + len + ")"
				)
			else
				cyclicCopy(src, srcPos, dst, dstPos, len)
/*

	private def l2rCopy(src :Array[Unknown], srcPos :Int, dst :Array[Unknown], dstPos :Int, len :Int) :Unit = {
		(src :Array[_], dst :Array[_]) match {
			case (a1 :Array[AnyRef], a2 :Array[AnyRef]) =>
				def copyRefs() :Unit = {
					var i = 0
					while (i < len) {
						a2(dstPos + i) = a1(srcPos + i)
						i += 1
					}
				}
				copyRefs()
			case (a1 :Array[Int], a2 :Array[Int]) =>
				def copyInts() :Unit = {
					var i = 0
					while (i < len) {
						a2(dstPos + i) = a1(srcPos + i)
						i += 1
					}
				}
				copyInts()
			case (a1 :Array[Long], a2 :Array[Long]) =>
				def copyLongs() :Unit = {
					var i = 0
					while (i < len) {
						a2(dstPos + i) = a1(srcPos + i)
						i += 1
					}
				}
				copyLongs()
			case (a1 :Array[Double], a2 :Array[Double]) =>
				def copyDoubles() :Unit = {
					var i = 0
					while (i < len) {
						a2(dstPos + i) = a1(srcPos + i)
						i += 1
					}
				}
				copyDoubles()
			case (a1 :Array[Char], a2 :Array[Char]) =>
				def copyChars() :Unit = {
					var i = 0
					while (i < len) {
						a2(dstPos + i) = a1(srcPos + i)
						i += 1
					}
				}
				copyChars()
			case (a1 :Array[Byte], a2 :Array[Byte]) =>
				def copyBytes() :Unit = {
					var i = 0
					while (i < len) {
						a2(dstPos + i) = a1(srcPos + i)
						i += 1
					}
				}
				copyBytes()
			case (a1 :Array[Float], a2 :Array[Float]) =>
				def copyFloats() :Unit = {
					var i = 0
					while (i < len) {
						a2(dstPos + i) = a1(srcPos + i)
						i += 1
					}
				}
				copyFloats()
			case (a1 :Array[Short], a2 :Array[Short]) =>
				def copyShorts() :Unit = {
					var i = 0
					while (i < len) {
						a2(dstPos + i) = a1(srcPos + i)
						i += 1
					}
				}
				copyShorts()
			case (a1 :Array[Boolean], a2 :Array[Boolean]) =>
				def copyBooleans() :Unit = {
					var i = 0
					while (i < len) {
						a2(dstPos + i) = a1(srcPos + i)
						i += 1
					}
				}
				copyBooleans()
			case _ =>
				def copyUnknown() :Unit = {
					var i = 0
					while (i < len) {
						dst(dstPos + i) = src(srcPos + i)
						i += 1
					}
				}
				copyUnknown()
		}
	}

	private def r2lCopy(src :Array[Unknown], srcPos :Int, dst :Array[Unknown], dstPos :Int, len :Int) :Unit =
		(src :Array[_], dst :Array[_]) match {
			case (a1 :Array[AnyRef], a2 :Array[AnyRef]) =>
				def copyRefs() :Unit = {
					var i = len
					while (i > 0) {
						i -= 1
						a2(dstPos + i) = a1(srcPos + i)
					}
				}
				copyRefs()
			case (a1 :Array[Int], a2 :Array[Int]) =>
				def copyInts() :Unit = {
					var i = len
					while (i > 0) {
						i -= 1
						a2(dstPos + i) = a1(srcPos + i)
					}
				}
				copyInts()
			case (a1 :Array[Long], a2 :Array[Long]) =>
				def copyLongs() :Unit = {
					var i = len
					while (i > 0) {
						i -= 1
						a2(dstPos + i) = a1(srcPos + i)
					}
				}
				copyLongs()
			case (a1 :Array[Double], a2 :Array[Double]) =>
				def copyDoubles() :Unit = {
					var i = len
					while (i > 0) {
						i -= 1
						a2(dstPos + i) = a1(srcPos + i)
					}
				}
				copyDoubles()
			case (a1 :Array[Char], a2 :Array[Char]) =>
				def copyChars() :Unit = {
					var i = len
					while (i > 0) {
						i -= 1
						a2(dstPos + i) = a1(srcPos + i)
					}
				}
				copyChars()
			case (a1 :Array[Byte], a2 :Array[Byte]) =>
				def copyBytes() :Unit = {
					var i = len
					while (i > 0) {
						i -= 1
						a2(dstPos + i) = a1(srcPos + i)
					}
				}
				copyBytes()
			case (a1 :Array[Float], a2 :Array[Float]) =>
				def copyFloats() :Unit = {
					var i = len
					while (i > 0) {
						i -= 1
						a2(dstPos + i) = a1(srcPos + i)
					}
				}
				copyFloats()
			case (a1 :Array[Short], a2 :Array[Short]) =>
				def copyShorts() :Unit = {
					var i = len
					while (i > 0) {
						i -= 1
						a2(dstPos + i) = a1(srcPos + i)
					}
				}
				copyShorts()
			case (a1 :Array[Boolean], a2 :Array[Boolean]) =>
				def copyBooleans() :Unit = {
					var i = len
					while (i > 0) {
						i -= 1
						a2(dstPos + i) = a1(srcPos + i)
					}
				}
				copyBooleans()
			case _ =>
				def copyUnknown() :Unit = {
					var i = len
					while (i > 0) {
						i -= 1
						dst(dstPos + i) = src(srcPos + i)
					}
				}
				copyUnknown()
		}
*/

		/** A single element `Array[E]`. */
		final def one[E :ClassTag](elem :E) :Array[E] = {
			val res = new Array[E](1)
			res(0) = elem
			res
		}
//		/** A single element `Array[E]`. */
//		@inline final def single[E :ClassTag](elem :E) :Array[E] = one(elem)

		/** A single element `Array[Byte]`. */
		final def one(elem :Byte) :Array[Byte] = {
			val res = new Array[Byte](1)
			res(0) = elem
			res
		}
//		/** A single element `Array[Byte]`. */
//		@inline final def single(elem :Byte) :Array[Byte] = one(elem)

		/** A single element `Array[Short]`. */
		final def one(elem :Short) :Array[Short] = {
			val res = new Array[Short](1)
			res(0) = elem
			res
		}
//		/** A single element `Array[Short]`. */
//		@inline final def single(elem :Short) :Array[Short] = one(elem)

		/** A single element `Array[Char]`. */
		final def one(elem :Char) :Array[Char] = {
			val res = new Array[Char](1)
			res(0) = elem
			res
		}
//		/** A single element `Array[Char]`. */
//		@inline final def single(elem :Char) :Array[Char] = one(elem)

		/** A single element `Array[Int]`. */
		final def one(elem :Int) :Array[Int] = {
			val res = new Array[Int](1)
			res(0) = elem
			res
		}
//		/** A single element `Array[Int]`. */
//		@inline final def single(elem :Int) :Array[Int] = one(elem)

		/** A single element `Array[Long]`. */
		final def one(elem :Long) :Array[Long] = {
			val res = new Array[Long](1)
			res(0) = elem
			res
		}
//		/** A single element `Array[Long]`. */
//		@inline final def single(elem :Long) :Array[Long] = one(elem)

		/** A single element `Array[Float]`. */
		final def one(elem :Float) :Array[Float] = {
			val res = new Array[Float](1)
			res(0) = elem
			res
		}
//		/** A single element `Array[Float]`. */
//		@inline final def single(elem :Float) :Array[Float] = one(elem)

		/** A single element `Array[Double]`. */
		final def one(elem :Double) :Array[Double] = {
			val res = new Array[Double](1)
			res(0) = elem
			res
		}
//		/** A single element `Array[Double]`. */
//		@inline final def single(elem :Double) :Array[Double] = one(elem)

		/** A single element `Array[Boolean]`. */
		final def one(elem :Boolean) :Array[Boolean] = {
			val res = new Array[Boolean](1)
			res(0) = elem
			res
		}
//		/** A single element `Array[Boolean]`. */
//		@inline final def single(elem :Boolean) :Array[Boolean] = one(elem)

		/** A two element array of element type defined by the implicit `ClassTag`. */
		final def two[E :ClassTag](first :E, second :E) :Array[E] = {
			val res = new Array[E](2)
			res(0) = first
			res(1) = second
			res
		}

		/** An `Array[Byte]` of two elements. */
		final def two(first :Byte, second :Byte) :Array[Byte] = {
			val res = new Array[Byte](2)
			res(0) = first
			res(1) = second
			res
		}

		/** An `Array[Short]` of two elements. */
		final def two(first :Short, second :Short) :Array[Short] = {
			val res = new Array[Short](2)
			res(0) = first
			res(1) = second
			res
		}

		/** An `Array[Char]` of two elements. */
		final def two(first :Char, second :Char) :Array[Char] = {
			val res = new Array[Char](2)
			res(0) = first
			res(1) = second
			res
		}

		/** An `Array[Int]` of two elements. */
		final def two(first :Int, second :Int) :Array[Int] = {
			val res = new Array[Int](2)
			res(0) = first
			res(1) = second
			res
		}

		/** An `Array[Long]` of two elements. */
		final def two(first :Long, second :Long) :Array[Long] = {
			val res = new Array[Long](2)
			res(0) = first
			res(1) = second
			res
		}

		/** An `Array[Float]` of two elements. */
		final def two(first :Float, second :Float) :Array[Float] = {
			val res = new Array[Float](2)
			res(0) = first
			res(1) = second
			res
		}

		/** An `Array[Double]` of two elements. */
		final def two(first :Double, second :Double) :Array[Double] = {
			val res = new Array[Double](2)
			res(0) = first
			res(1) = second
			res
		}

		/** An `Array[Boolean]` of two elements. */
		final def two(first :Boolean, second :Boolean) :Array[Boolean] = {
			val res = new Array[Boolean](2)
			res(0) = first
			res(1) = second
			res
		}

		/** An array filled with `n` copies of `elem`. */
		final def const[E :ClassTag](n :Int)(elem :E) :Array[E] = {
			val res = new Array[E](n)
			res.fill(elem)
			res
		}

		/** A complement of `Array.iterate` and `Array.unfold` provided by `Array` object, which creates
		  * an `Array[E]` by recursively applying a partial function while defined to its own results and collecting
		  * all returned values. It is very similar to the standard [[Array.iterate iterate]],
		  * but instead of a fixed number of iterations, the generator function `next` is called for its return values
		  * until it is no longer applicable, which marks the end of the array.
		  * @param start the first element added to the array.
		  * @param next  a generator function returning subsequent elements for the array based on the previous one,
		  *              serving as the termination condition by indicating that it can no longer be applied
		  *              to the given argument.
		  * @tparam E the element type of the generated array.
		  * @return an array containing the sequence starting with `start`, and resulting from recursively applying
		  *         `next` to itself.
		  */
		@inline final def generate[E :ClassTag](start :E)(next :PartialFunction[E, E]) :Array[E] =
			expand(start)(next.lift)

		/** Builds an `Array[E]` by recursively reapplying the given function to the initial element.
		  * Instead of listing a fixed number of elements, this method uses the generator function `next`
		  * as the termination condition and ends the recursion once it returns `None`. It is the opposite
		  * of [[scala.collection.IterableOnceOps.reduce reduce]] in the same way as
		  * [[scala.collection.IterableFactory.unfold unfold]] is the opposite
		  * of [[scala.collection.IterableOnceOps.fold fold]].
		  * @param start The first element added to the array.
		  * @param next  A generator function returning subsequent elements for the array based on the previous one,
		  *              or `None` to indicate the end of recursion.
		  * @tparam E the element type of the generated array.
		  * @return an array containing the sequence starting with `start`,
		  *         and resulting from recursively applying `next` to itself.
		  */
		final def expand[E :ClassTag](start :E)(next :E => Option[E]) :Array[E] = {
			val builder = Array.newBuilder[E]
			builder += start
			@tailrec def rec(x :E = start) :Array[E] = next(x) match {
				case Some(y) => builder += y; rec(y)
				case None => builder.result()
			}
			rec()
		}

		/** Similar to [[Array.iterate Array.iterate]],
		  * but the iterating function accepts the positional index of the next element as an additional argument.
		  * @param start The first element of the created array.
		  * @param len   The size of the created array.
		  * @param f     A function generating subsequent elements following start.
		  *              The second element of the array will be `f(start, 1)`, the third `f(f(start, 1), 2)`, and so on.
		  */
		final def iterateWithIndex[E :ClassTag](start :E, len :Int)(f :(E, Int) => E) :Array[E] =
			Array.from(Iterator.iterateWithIndex(start, len)(f))

		/** Produces an array that uses a function `f` to produce elements of type `A`
		  * and update an internal state of type `S`.
		  * @param init State initial value
		  * @param f    Computes the next element (or returns `None` to signal
		  *             the end of the collection)
		  * @tparam E Type of the elements
		  * @tparam S Type of the internal state
		  * @return an array that produces elements using `f` until `f` returns `None`
		  */
		final def unfold[E :ClassTag, S](init :S)(f :S => Option[(E, S)]) :Array[E] =
			Array.from(new View.Unfold(init)(f))

		/** An iterator iterating over range `[from, from+size)` of an array. Indices out of range are skipped silently. */
		final def iterator[E](array :Array[E], from :Int, size :Int) :Iterator[E] =
			ArrayLike.iterator(array, from, size)

		/** A stepper iterating over range `[from, from+size0` of an array. Indices out of range are skipped silently. */
		final def stepper[E, S <: Stepper[_]]
		                 (array :Array[E], from :Int = 0, size :Int = Int.MaxValue)
		                 (implicit shape :StepperShape[E, S]) :S with EfficientSplit = Stepper(array, from, size)
	}






	private[arrays] sealed trait ArrayLikeEvidence extends Any {
		@inline implicit final def ArrayLikeOrdering[E :Ordering, Arr[X] <: ArrayLike[X]] :Ordering[Arr[E]] =
			Array.ArrayOrdering[E].castFrom[Ordering[Array[E]], Ordering[Arr[E]]]
	}

	private[arrays] sealed trait ArrayLikeConversions extends Any {
		@inline implicit final def ArrayLikeToSeq[A, Arr[X] <: ArrayLike[X]]
	                               (self :Arr[A])(implicit wrap :ArrayLikeToSeqConversion[A, Arr]) :wrap.IndexedSeq =
			wrap(self)
	}

	/** Mixin trait with extension methods conversion for `ArrayLike` subtypes.
	  * @define Coll `ArrayLike`
	  * @define Extension `ArrayLikeExtension[Arr, E]`
	  * @define conversionInfo It is a specific subclass of `E => `$Extension function in order for it to have precedence
	  *                        over conversions in `Predef` `Array[E] => ArrayOps[E]` as well as extension methods
	  *                        from `sugar.collections.extensions`, available through wrapping to `IterableOnceOps` .
	  *                        While the shadowed extension methods have the same semantics as in `ArrayOps` and
	  *                        collection extensions, they also have overloaded variants,
	  *                        which would otherwise be invisible.
	  */
	private[arrays] sealed trait ArrayLikeExtensions extends Any with ArrayLikeEvidence /*with conversions*/ {
		/** Extension methods for all `ArrayLike` subtypes.
		  * $conversionInfo
		  */
		implicit final def ArrayLikeExtension[Arr[X] <: ArrayLike[X], E] :ArrayLikeExtensionConversion[Arr, E] =
			extensions.ArrayLikeExtensionConversionPrototype.asInstanceOf[ArrayLikeExtensionConversion[Arr, E]]
	}

	sealed trait ArrayLikeExtensionConversion[Arr[X] <: ArrayLike[X], E]
		extends (Arr[E] => ArrayLikeExtension[Arr, E])
	{
		@inline final def apply(v1 :Arr[E])(implicit __ :Ignored) :ArrayLikeExtension[Arr, E] =
			new ArrayLikeExtension(v1.asInstanceOf[Array[Unknown]])
	}
	private val ArrayLikeExtensionConversionPrototype :ArrayLikeExtensionConversion[ArrayLike, Unknown] =
		new PriorityConversion.Wrapped[ArrayLike[Unknown], ArrayLikeExtension[ArrayLike, Unknown]](
			(arr :ArrayLike[Unknown]) => new ArrayLikeExtension(arr.asInstanceOf[Array[Unknown]])
		) with ArrayLikeExtensionConversion[ArrayLike, Unknown]



//	private[arrays] sealed trait IArrayLikeConversions extends Any with ArrayLikeConversions {
//		//fixme: conflicts with ArrayLikeExtension because it's more specific
//		@inline implicit final def IArrayLikeToSeq[A](self :IArrayLike[A]) :IndexedSeq[A] = IArrayLike.Wrapped(self)
//	}

//	private[arrays] sealed trait IArrayLikeEvidence extends Any with ArrayLikeEvidence {
//		//fixme: precedence conflicts with ArrayLikeExtension
//		implicit final def IArrayLikeIsSeq[E] :IsSeq[IArrayLike[E]] { type A = E; type C = IArrayLike[E] } =
//			IArrayIsSeqPrototype.asInstanceOf[IsSeq[IArrayLike[E]] { type A = E; type C = IArrayLike[E] }]
//	}
//		isSeqPrototype.asInstanceOf[IsSeq[IArrayLike[E]] { type A = E; type C = IArrayLike[E] }]
//
//	We don't know what the particular element type of the array will be, so we must use Scala's polymorphic delegates.
//	private class IArrayLikeIsSeq[E] extends ArrayLikeIsSeqTemplate[E, Seq, IArrayLike] {
//		override def apply(array :IArrayLike[E]) =
//			new IArrayLikeIsSeqOps[E, IArrayLike](array) {
//				protected override def fromSpecific(coll :IterableOnce[E]) :IArrayLike[E] = IRefArray.from(coll)
//				protected override def newSpecificBuilder :Builder[E, IArrayLike[E]] = IRefArray.newBuilder
//			}
//		private def readResolve = IArrayLike.IArrayLikeIsSeq
//	}
//	private[this] val isSeqPrototype :IArrayLikeIsSeq[Any] = new IArrayLikeIsSeq[Any]


	/** Mixin trait with extension methods conversion for `IArrayLike` subtypes.
	  * @define Coll `IArrayLike`
	  * @define Extension `IArrayLikeExtension[Arr, E]`
	  */
	private[arrays] sealed trait IArrayLikeExtensions extends Any with ArrayLikeExtensions {
		/** Extension methods for all `IArrayLike[E]` subtypes.
		  * $conversionInfo
		  */
		implicit final def IArrayLikeExtension[Arr[X] <: IArrayLike[X], E] :IArrayLikeExtensionConversion[Arr, E] =
			extensions.IArrayLikeExtensionConversionPrototype.asInstanceOf[IArrayLikeExtensionConversion[Arr, E]]
	}

	sealed trait IArrayLikeExtensionConversion[Arr[X] <: IArrayLike[X], E]
		extends (Arr[E] => IArrayLikeExtension[Arr, E])
	{
		@inline final def apply(v1 :Arr[E])(implicit __ :Ignored) :IArrayLikeExtension[Arr, E] =
			new IArrayLikeExtension(v1.asInstanceOf[Array[Unknown]])
	}
	private val IArrayLikeExtensionConversionPrototype :IArrayLikeExtensionConversion[IArrayLike, Any] =
		new PriorityConversion.Wrapped[IArrayLike[Any], IArrayLikeExtension[IArrayLike, Any]](
			(arr :IArrayLike[Any]) => new IArrayLikeExtension(arr.asInstanceOf[Array[Unknown]])
		) with IArrayLikeExtensionConversion[IArrayLike, Any]



	private[arrays] sealed trait IArrayEvidence extends Any {
		@inline implicit final def IArrayClassTag[E :ClassTag] :ClassTag[IArray[E]] = classTag[Array[E]].castParam[IArray[E]]
		//todo: specialize
//		implicit final def IArrayOrdering[E :Ordering] :Ordering[IArray[E]] =
//			Array.ArrayOrdering[E].castFrom[Ordering[Array[E]], Ordering[IArray[E]]]
//
//		/** A type class promoting immutable arrays to sequences. */
//		implicit final def IArrayIsSeq[E] :IsSeq[IArray[E]] { type A = E; type C = IArray[E] } =
//			IArrayIsSeqPrototype.asInstanceOf[IsSeq[IArray[E]] { type A = E; type C = IArray[E] }]
	}
//	private val IArrayIsSeqPrototype = new ArrayLikeIsSeqTemplate[Unknown, Seq, IArray] {
//		final override def apply(coll :IArray[Unknown]) =
//			new IArrayLikeIsSeqOps[Unknown, IArray](coll) {
//				protected final override def fromSpecific(coll :IterableOnce[Unknown]) :IArray[Unknown] =
//					IArray.from(coll)(ClassTag[Unknown](this.coll.getClass.getComponentType))
//
//				protected final override def newSpecificBuilder :Builder[Unknown, IArray[Unknown]] =
//					IArray.newBuilder(this.coll.getClass.getComponentType.castParam[Unknown])
//			}
//		private def readResolve = IArrayIsSeqPrototype
//	}

//	private[arrays] sealed trait IArrayConversions extends Any {
//		implicit final def IArrayToSeq[E](self :IArray[E]) :IndexedSeq[E] =
//	}

	/** Conversions providing extension methods for `IArray`. */
	private[arrays] sealed trait IArrayExtensions extends IArrayLikeExtensions with IArrayEvidence {
		implicit final def IArrayExtension[E] :IArrayExtensionConversion[E] =
			extensions.IArrayExtensionConversionPrototype.asInstanceOf[IArrayExtensionConversion[E]]

		implicit final def RefIArrayExtension[E <: AnyRef] :RefIArrayExtensionConversion[E] =
			extensions.RefIArrayExtensionConversionPrototype.asInstanceOf[RefIArrayExtensionConversion[E]]

		implicit object ByteIArrayExtension extends PriorityConversion.Wrapped[IArray[Byte], ByteIArrayExtension](
			(arr :IArray[Byte]) => new ByteIArrayExtension(arr.asInstanceOf[Array[Byte]])
		) {
			@inline final def apply(v1 :IArray[Byte])(implicit __ :Ignored) :ByteIArrayExtension =
				new ByteIArrayExtension(v1.asInstanceOf[Array[Byte]])
		}
		implicit object ShortIArrayExtension extends PriorityConversion.Wrapped[IArray[Short], ShortIArrayExtension](
			(arr :IArray[Short]) => new ShortIArrayExtension(arr.asInstanceOf[Array[Short]])
		) {
			@inline final def apply(v1 :IArray[Short])(implicit __ :Ignored) :ShortIArrayExtension =
				new ShortIArrayExtension(v1.asInstanceOf[Array[Short]])
		}
		implicit object CharIArrayExtension extends PriorityConversion.Wrapped[IArray[Char], CharIArrayExtension](
			(arr :IArray[Char]) => new CharIArrayExtension(arr.asInstanceOf[Array[Char]])
		) {
			@inline final def apply(v1 :IArray[Char])(implicit __ :Ignored) :CharIArrayExtension =
				new CharIArrayExtension(v1.asInstanceOf[Array[Char]])
		}
		implicit object IntIArrayExtension extends PriorityConversion.Wrapped[IArray[Int], IntIArrayExtension](
			(arr :IArray[Int]) => new IntIArrayExtension(arr.asInstanceOf[Array[Int]])
		) {
			@inline final def apply(v1 :IArray[Int])(implicit __ :Ignored) :IntIArrayExtension =
				new IntIArrayExtension(v1.asInstanceOf[Array[Int]])
		}
		implicit object LongIArrayExtension extends PriorityConversion.Wrapped[IArray[Long], LongIArrayExtension](
			(arr :IArray[Long]) => new LongIArrayExtension(arr.asInstanceOf[Array[Long]])
		) {
			@inline final def apply(v1 :IArray[Long])(implicit __ :Ignored) :LongIArrayExtension =
				new LongIArrayExtension(v1.asInstanceOf[Array[Long]])
		}
		implicit object FloatIArrayExtension extends PriorityConversion.Wrapped[IArray[Float], FloatIArrayExtension](
			(arr :IArray[Float]) => new FloatIArrayExtension(arr.asInstanceOf[Array[Float]])
		) {
			@inline final def apply(v1 :IArray[Float])(implicit __ :Ignored) :FloatIArrayExtension =
				new FloatIArrayExtension(v1.asInstanceOf[Array[Float]])
		}
		implicit object DoubleIArrayExtension extends PriorityConversion.Wrapped[IArray[Double], DoubleIArrayExtension](
			(arr :IArray[Double]) => new DoubleIArrayExtension(arr.asInstanceOf[Array[Double]])
		) {
			@inline final def apply(v1 :IArray[Double])(implicit __ :Ignored) :DoubleIArrayExtension =
				new DoubleIArrayExtension(v1.asInstanceOf[Array[Double]])
		}
		implicit object BooleanIArrayExtension extends PriorityConversion.Wrapped[IArray[Boolean], BooleanIArrayExtension](
			(arr :IArray[Boolean]) => new BooleanIArrayExtension(arr.asInstanceOf[Array[Boolean]])
		) {
			@inline final def apply(v1 :IArray[Boolean])(implicit __ :Ignored) :BooleanIArrayExtension =
				new BooleanIArrayExtension(v1.asInstanceOf[Array[Boolean]])
		}
	}

	sealed trait IArrayExtensionConversion[E] extends (IArray[E] => IArrayExtension[E]) {
		@inline final def apply(v1 :IArray[E])(implicit __ :Ignored) :IArrayExtension[E] =
			new IArrayExtension(v1.asInstanceOf[Array[E]])
	}
	sealed trait RefIArrayExtensionConversion[E <: AnyRef] extends (IArray[E] => RefIArrayExtension[E]) {
		@inline final def apply(v1 :IArray[E])(implicit __ :Ignored) :RefIArrayExtension[E] =
			new RefIArrayExtension(v1.asInstanceOf[Array[E]])
	}
	private val IArrayExtensionConversionPrototype :IArrayExtensionConversion[Unknown] =
		new PriorityConversion.Wrapped[IArray[Unknown], IArrayExtension[Unknown]](
			(arr :IArray[Unknown]) => new IArrayExtension(arr.asInstanceOf[Array[Unknown]])
		) with IArrayExtensionConversion[Unknown]

	private val RefIArrayExtensionConversionPrototype :RefIArrayExtensionConversion[AnyRef] =
		new PriorityConversion.Wrapped[IArray[AnyRef], RefIArrayExtension[AnyRef]](
			(arr :IArray[AnyRef]) => new RefIArrayExtension(arr.asInstanceOf[Array[AnyRef]])
		) with RefIArrayExtensionConversion[AnyRef]



//	private[arrays] sealed trait MutableArrayConversions extends Any {
//		implicit final def MutableArrayToSeq[E](array :MutableArray[E]) :collection.IndexedSeq[E] =
//			MutableArray.Wrapped(array)
//	}
//	private[arrays] sealed trait MutableArrayEvidence extends Any {
//		//fixme: precedence conflicts with ArrayLikeExtension
//		implicit final def MutableArrayIsSeq[E] :IsSeq[MutableArray[E]] { type A = E; type C = MutableArray[E] } =
//			Array.ArrayIsSeq.asInstanceOf[IsSeq[MutableArray[E]] { type A = E; type C = MutableArray[E] }]
//	}

	/** Mixin trait with extension methods conversion for `MutableArray` subtypes.
	  * @define Coll `MutableArray`
	  * @define Extension `MutableArrayExtension[E, Arr]`
	  */
	private[arrays] sealed trait MutableArrayExtensions extends Any with ArrayLikeExtensions {
		/** Extension methods for all `MutableArray[E]` subtypes.
		  * $conversionInfo
		  */
		implicit final def MutableArrayExtension[E] :MutableArrayExtensionConversion[E] =
			extensions.MutableArrayExtensionConversionPrototype.asInstanceOf[MutableArrayExtensionConversion[E]]
	}

	sealed trait MutableArrayExtensionConversion[E] extends (MutableArray[E] => MutableArrayExtension[E]) {
		@inline final def apply(v1 :MutableArray[E])(implicit __ :Ignored) :MutableArrayExtension[E] =
			new MutableArrayExtension(v1.asInstanceOf[Array[Unknown]])
	}
	private val MutableArrayExtensionConversionPrototype :MutableArrayExtensionConversion[Unknown] =
		new PriorityConversion.Wrapped[MutableArray[Unknown], MutableArrayExtension[Unknown]](
			(arr :MutableArray[Unknown]) => new MutableArrayExtension(arr.asInstanceOf[Array[Unknown]])
		) with MutableArrayExtensionConversion[Unknown]



	private[arrays] sealed trait RefArrayLikeEvidence extends Any {
		implicit final def RefArrayLikeClassTag[X, Arr[X] <: RefArrayLike[X]] :ClassTag[Arr[X]] =
			ArrayAnyClassTag.asInstanceOf[ClassTag[Arr[X]]]
//		implicit final def RefArrayLikeIsSeq[E, Arr[X] <: RefArrayLike[X]] :IsSeq[Arr[E]] { type A = E; type C = Arr[E] } =
//			RefArrayLikeIsSeqPrototype.castFrom[IsSeq[RefArrayLike[Any]], IsSeq[Arr[E]] { type A = E; type C = Arr[E] }]
	}
	private[this] val ArrayAnyClassTag = classTag[Array[AnyRef]]
//
//	private val RefArrayLikeIsSeqPrototype :ArrayLikeIsSeqTemplate[Any, collection.Seq, RefArrayLike] =
//		new ArrayLikeIsSeqTemplate[Any, collection.Seq, RefArrayLike] {
//			final override def apply(array :RefArrayLike[Any]) =
//				new ArrayLikeIsSeqOps[Any, RefArrayLike](array.asInstanceOf[Array[Any]]) {
//					override def fromSpecific(coll :IterableOnce[Any]) :RefArrayLike[Any] = RefArrayLike.from(coll)
//					override def newSpecificBuilder :Builder[Any, RefArrayLike[Any]] = RefArrayLike.newBuilder
//				}
//			private def readResolve = extensions.RefArrayLikeIsSeqPrototype
//			override lazy val toString = "RefArrayLikeIsSeq"
//		}

	/** Mixin trait with extension methods conversion for `RefArrayLike` subtypes.
	  * @define Coll `RefArrayLike`
	  * @define Extension `RefArrayLikeExtension[E, Arr]`
	  */
	private[arrays] sealed trait RefArrayLikeExtensions extends Any with ArrayLikeExtensions {
		/** Extension methods for all `RefArrayLike[E]` subtypes.
		  * $conversionInfo
		  */
		implicit final def RefArrayLikeExtension[Arr[X] <: RefArrayLike[X], E] :RefArrayLikeExtensionConversion[Arr, E] =
			extensions.RefArrayLikeExtensionConversionPrototype.asInstanceOf[RefArrayLikeExtensionConversion[Arr, E]]
	}

	sealed trait RefArrayLikeExtensionConversion[Arr[X] <: RefArrayLike[X], E]
		extends (Arr[E] => RefArrayLikeExtension[Arr, E])
	{
		@inline final def apply(v1 :Arr[E])(implicit __ :Ignored) :RefArrayLikeExtension[Arr, E] =
			new RefArrayLikeExtension(v1.asInstanceOf[Array[Any]])
	}
	private val RefArrayLikeExtensionConversionPrototype :RefArrayLikeExtensionConversion[RefArrayLike, Unknown] =
		new PriorityConversion.Wrapped[RefArrayLike[Unknown], RefArrayLikeExtension[RefArrayLike, Unknown]](
			(arr :RefArrayLike[Unknown]) => new RefArrayLikeExtension(arr.asInstanceOf[Array[Any]])
		) with RefArrayLikeExtensionConversion[RefArrayLike, Unknown]



//	private[arrays] trait RefArrayEvidence extends Any {
//		implicit def RefArrayClassTag[A] :ClassTag[RefArray[A]] = tag.castParam[RefArray[A]]
//		implicit def RefArrayIsSeq[E] :IsSeq[RefArray[E]] { type A = E; type C = RefArray[A] } =
//			RefArrayIsSeqPrototype.asInstanceOf[IsSeq[RefArray[E]] { type A = E; type C = RefArray[A] }]
//	}
//	private val RefArrayIsSeqPrototype :ArrayLikeIsSeqTemplate[Any, mutable.Seq, RefArray] =
//		new ArrayLikeIsSeqTemplate[Any, mutable.Seq, RefArray] {
//			final override def apply(coll :RefArray[Any]) =
//				new MutableArrayIsSeqOps[Any, RefArray](coll) {
//					protected override def fromSpecific(coll :IterableOnce[Any]) :RefArray[Any] = RefArray.from(coll)
//					protected override def newSpecificBuilder :Builder[Any, RefArray[Any]] = RefArray.newBuilder
//				}
//			private def readResolve = RefArrayIsSeqPrototype
//			override def toString = "RefArrayIsSeq"
//		}

	/** Mixin trait with extension methods conversion for `RefArray` types.
	  * @define Coll `RefArray`
	  * @define Extension `RefArrayExtension[E]`
	  */
	private[arrays] sealed trait RefArrayExtensions extends Any with RefArrayLikeExtensions with MutableArrayExtensions {
		/** Extension methods for [[net.noresttherein.sugar.arrays.RefArray RefArray]]`[E]`.
		  * $conversionInfo
		  */
		implicit final def RefArrayExtension[E] :RefArrayExtensionConversion[E] =
			extensions.RefArrayExtensionConversionPrototype.asInstanceOf[RefArrayExtensionConversion[E]]
	}

	sealed trait RefArrayExtensionConversion[E] extends (RefArray[E] => RefArrayExtension[E]) {
		@inline final def apply(v1 :RefArray[E])(implicit __ :Ignored) :RefArrayExtension[E] =
			new RefArrayExtension(v1.asInstanceOf[Array[Any]])
	}
	private val RefArrayExtensionConversionPrototype :RefArrayExtensionConversion[Unknown] =
		new PriorityConversion.Wrapped[RefArray[Unknown], RefArrayExtension[Unknown]](
			(arr :RefArray[Unknown]) => new RefArrayExtension(arr.asInstanceOf[Array[Any]])
		) with RefArrayExtensionConversion[Unknown]



//	private[arrays] trait IRefArrayEvidence extends Any {
//		implicit def IRefArrayClassTag[A] :ClassTag[IRefArray[A]] = ArrayAnyClassTag.castParam[IRefArray[A]]
//		implicit def IRefArrayIsSeq[E] :IsSeq[IRefArray[E]] { type A = E; type C = IRefArray[A] } =
//			IRefArrayIsSeqPrototype.asInstanceOf[IsSeq[IRefArray[E]] { type A = E; type C = IRefArray[A] }]
//	}
//	private val IRefArrayIsSeqPrototype :ArrayLikeIsSeqTemplate[Any, Seq, IRefArray] =
//		new ArrayLikeIsSeqTemplate[Any, Seq, IRefArray] {
//			final override def apply(coll :IRefArray[Any]) =
//				new IArrayLikeIsSeqOps[Any, IRefArray](coll) {
//					protected override def fromSpecific(coll :IterableOnce[Any]) :IRefArray[Any] = IRefArray.from(coll)
//					protected override def newSpecificBuilder :Builder[Any, IRefArray[Any]] = IRefArray.newBuilder
//				}
//			private def readResolve = extensions.IRefArrayIsSeqPrototype
//			override def toString = "IRefArrayIsSeq"
//		}

	/** Mixin trait with extension methods conversion for `IRefArray` types.
	  * @define Coll `IRefArray`
	  * @define Extension `IRefArrayExtension[E]`
	  */
	private[arrays] sealed trait IRefArrayExtensions
		extends Any with RefArrayLikeExtensions with IArrayLikeExtensions //with IRefArrayEvidence
	{
		/** Extension methods for [[net.noresttherein.sugar.arrays.IRefArray IRefArray]]`[E]`.
		  * $conversionInfo
		  */
		implicit final def IRefArrayExtension[E] :IRefArrayExtensionConversion[E] =
			extensions.IRefArrayExtensionConversionPrototype.asInstanceOf[IRefArrayExtensionConversion[E]]
	}

	sealed trait IRefArrayExtensionConversion[E] extends (IRefArray[E] => IRefArrayExtension[E]) {
		@inline final def apply(v1 :IRefArray[E])(implicit __ :Ignored) :IRefArrayExtension[E] =
			new IRefArrayExtension(v1.asInstanceOf[Array[Any]])
	}
	private val IRefArrayExtensionConversionPrototype :IRefArrayExtensionConversion[Unknown] =
		new PriorityConversion.Wrapped[IRefArray[Unknown], IRefArrayExtension[Unknown]](
			(arr :IRefArray[Unknown]) => new IRefArrayExtension(arr.asInstanceOf[Array[Any]])
		) with IRefArrayExtensionConversion[Unknown]


}
