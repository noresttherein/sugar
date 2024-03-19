package net.noresttherein.sugar.arrays


import java.util.Arrays
import java.util.random.RandomGenerator

import scala.Array.UnapplySeqWrapper
import scala.annotation.nowarn
import scala.collection.generic.IsSeq
import scala.collection.mutable.ArrayBuffer
import scala.collection.{IterableFactory, mutable}
import scala.reflect.{ClassTag, classTag}
import scala.util.{Random, Sorting}

import net.noresttherein.sugar.casting.{cast2TypeParamsMethods, cast3TypeParamsMethods, castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.collections.{ArrayIterableOnce, ArraySlice, IArrayLikeSlice, MatrixBuffer, MutableArraySlice, Mutator, RefArraySlice}
import net.noresttherein.sugar.collections.extensions.IterableOnceExtension
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.reflect.extensions.ClassExtension
import net.noresttherein.sugar.typist.Unknown
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




/** Companion definitions for Scala arrays, including `Array[AnyRef]`
  * as [[net.noresttherein.sugar.arrays.RefArray! RefArray]].
  * @see [[net.noresttherein.sugar.arrays.MutableArray! MutableArray]]
  * @define Coll `MutableArray`
  * @define coll mutable array-like
  */
@SerialVersionUID(Ver)
case object MutableArray extends IterableFactory.Delegate[MutableArray](RefArray) {

	@inline def unapplySeq[E](array :MutableArray[E]) :UnapplySeqWrapper[E] =
		new UnapplySeqWrapper(array.asSubtype[Array[E]])


	/** Wraps and unwraps arrays in mutable indexed sequences.
	  * $warning
	  * @define warning Extraction relies on a `ClassTag` for the unwrapped
	  * `IterableOnce[E]` to guarantee that the underlying array type can store new elements of that type.
	  * This however goes only as far as class comparison, and is unsound if the element type is generic,
	  * or otherwise carries information which distinguishes it from simply its runtime class.
	  * The array is shared, not copied, so changes to either the array or the sequence will be mutually visible.
	  * Note, however, that extracted `MutableArray[E]` may technically be neither `RefArray[E]` (`Array[E]`)
	  * nor an `Array[E]`; it is however guaranteed that
	  *   1. it is possible to store values of `E` in it, using `MutableArray`'s extension methods, and
	  *   1. all actual elements stored in it are subclasses of the runtime class of `E`.
	  */
	@SerialVersionUID(Ver)
	object Wrapped {
		def apply[E](array :MutableArray[E]) :mutable.IndexedSeq[E] =
			MutableArraySlice.wrap(array)

		def unapply[E](elems :mutable.SeqOps[E, generic.Any1, _]) :Maybe[MutableArray[E]] = {
			val length = elems.length
			elems match {
				case seq   :mutable.ArraySeq[_] =>
					Yes(seq.array.castFrom[Array[_], MutableArray[E]])
				case seq   :ArrayBuffer[_] if CheatedAccess.array(seq).length == length =>
					Yes(CheatedAccess.array(seq).castFrom[Array[_], RefArray[E]])
				case slice :ArrayIterableOnce[_] if slice.isMutable && slice.unsafeArray.length == length =>
					Yes(slice.unsafeArray.castFrom[Array[_], MutableArray[E]])
				case seq   :MatrixBuffer[_] if seq.dim == 1 && seq.startIndex == 0 && seq.data1.length == length =>
					Yes(seq.data1.castFrom[Array[_], MutableArray[E]])
				case _ =>
					No
			}
		}

		def unapply[E :ClassTag](elems :IterableOnce[E]) :Maybe[MutableArray[E]] = elems match {
			case slice :ArrayIterableOnce[_]
				if slice.isMutable && slice.startIndex == 0 && slice.knownSize == slice.unsafeArray.length &&
					classTag[E].runtimeClass <:< slice.unsafeArray.getClass.getComponentType =>
				Yes(slice.unsafeArray.castFrom[Array[_], MutableArray[E]])
			case _ :mutable.IndexedSeqOps[_, _, _] => elems match {
				case seq   :mutable.ArraySeq[_] if classTag[E].runtimeClass <:< seq.array.getClass.getComponentType =>
					Yes(seq.array.castFrom[Array[_], MutableArray[E]])
				case seq   :ArrayBuffer[_] if CheatedAccess.array(seq).length == seq.length =>
					Yes(CheatedAccess.array(seq).castFrom[Array[_], RefArray[E]])
				case seq   :MatrixBuffer[_]
					if seq.dim == 1 && seq.startIndex == 0 && seq.length == seq.data1.length &&
						classTag[E].runtimeClass <:< seq.iterableEvidence.runtimeClass =>
					Yes(seq.data1.castFrom[Array[_], MutableArray[E]])
				case _ =>
					No
			}
			case _ => No
		}
	}

	/** Converts a section of an array to a mutable indexd sequence, and matches any collection
	  * from which an underlying array can be extracted.
	  * $warning
	  */
	@SerialVersionUID(Ver)
	object Slice {
		def apply[E](array :MutableArray[E], from :Int, until :Int) :mutable.IndexedSeq[E] =
			if (array.getClass == classOf[Array[Any]])
				RefArraySlice.slice(array.asSubtype[RefArray[E]], from, until)
			else
				ArraySlice.slice(array.asSubtype[Array[E]], from, until)

		def unapply[E](elems :mutable.SeqOps[E, generic.Any1, _]) :Maybe[(MutableArray[E], Int, Int)] = {
			val length = elems.length
			elems match {
				case seq :mutable.ArraySeq[E] =>
					Yes(seq.array.castFrom[Array[_], MutableArray[E]], 0, length)
				case seq :ArrayBuffer[E] =>
					Yes(CheatedAccess.array(seq).castFrom[Array[AnyRef], RefArray[E]], 0, length)
				case seq :ArrayIterableOnce[E] if seq.isMutable =>
					val start = seq.startIndex
					Yes(seq.unsafeArray.castFrom[Array[_], MutableArray[E]], start, start + length)
				case seq :MatrixBuffer[E] if seq.dim == 1 && seq.startIndex + length <= seq.data1.length =>
					Yes((seq.data1, seq.startIndex, seq.startIndex + length))
				case _ =>
					No
			}
		}

		def unapply[E :ClassTag](elems :IterableOnce[E]) :Maybe[(MutableArray[E], Int, Int)] = elems match {
			case _ if elems.knownSize < 0 => No
			case slice :ArrayIterableOnce[E] if slice.isMutable =>
				val array = slice.unsafeArray.castFrom[Array[_], Array[E]]
				val start = slice.startIndex
				if (classTag[E].runtimeClass <:< array.getClass.getComponentType)
					Yes((array, start, start + slice.knownSize))
				else
					No
			case _ :collection.IndexedSeqOps[_, _, _] => elems match {
				case seq :mutable.ArraySeq[_] if classTag[E].runtimeClass <:< seq.array.getClass.getComponentType =>
					Yes((seq.array.castFrom[Array[_], Array[E]], 0, seq.array.length))

				case seq :ArrayBuffer[_] =>
					Yes((CheatedAccess.array(seq).castFrom[Array[AnyRef], RefArray[E]], 0, seq.length))

				case seq :MatrixBuffer[E]
					if seq.dim == 1 && seq.startIndex + seq.length <= seq.data1.length &&
						classTag[E].runtimeClass <:< seq.iterableEvidence.runtimeClass =>
					Yes(seq.data1, seq.startIndex, seq.startIndex + seq.length)
				case _ =>
					No
			}
			case _ => No
		}

	}



	/** Mutating extension methods available for `Array` and [[net.noresttherein.sugar.arrays.RefArray! RefArray]]. */
	class MutableArrayExtension[E] private[arrays] (private val self :Array[Unknown]) extends AnyVal {
//		@inline private def as[E] :Array[E] = self.asInstanceOf[Array[E]]

		/** Fills (in place) the whole array with the given value. */
		@inline def fill(elem :E) :Unit = fill(0, self.length)(elem)

		/** Sets (in place) all values in this array within the given index range to the specified value. */
		//consider: a different name, so we can define fill accepting => E, as in IterableFactory
//		@throws[IndexOutOfBoundsException]("if either from or until are outside of range [0, this.length)")
		def fill(from :Int, until :Int)(elem :E) :Unit = {
			if (until > from && until > 0 && from < self.length) {
				val from0 = math.max(from, 0)
				val until0 = math.min(until, self.length)
//				val length = self.length
//				if (from < 0 | until < 0 | from > length | until > length)
//					throw new IndexOutOfBoundsException(
//						errorString(self) + "fill(" + from + ", " + until + ")"
//					)
				(self :Array[_]) match {
					case array :Array[AnyRef]  => Arrays.fill(array, from0, until0, elem)
					case array :Array[Int]     => Arrays.fill(array, from0, until0, elem.asInstanceOf[Int])
					case array :Array[Long]    => Arrays.fill(array, from0, until0, elem.asInstanceOf[Long])
					case array :Array[Double]  => Arrays.fill(array, from0, until0, elem.asInstanceOf[Double])
					case array :Array[Byte]    => Arrays.fill(array, from0, until0, elem.asInstanceOf[Byte])
					case array :Array[Char]    => Arrays.fill(array, from0, until0, elem.asInstanceOf[Char])
					case array :Array[Float]   => Arrays.fill(array, from0, until0, elem.asInstanceOf[Float])
					case array :Array[Short]   => Arrays.fill(array, from0, until0, elem.asInstanceOf[Short])
					case array :Array[Boolean] => Arrays.fill(array, from0, until0, elem.asInstanceOf[Boolean])
					case array :Array[E @unchecked] =>
						var i = from0
						while (i < until0) {
							array(i) = elem
							i += 1
						}
				}
			}
		}

		/** Sets (in place) all elements in the array to `null`/`0`/`false`, depending on its type. */
		@inline def clear() :Unit = clear(0, self.length)

		//consider: strict index validation, as per most mutable methods
		/** Clears the specified index range, setting all elements to `null`/`0`/`false`. Returns this modified array. */
		def clear(from :Int, until :Int) :Unit =
			if (until > from & until > 0 && from < self.length) {
				val from0 = math.max(0, from)
				val until0 = math.min(self.length, until)
				(self :Array[_]) match {
					case _     :Array[Unit]    => Arrays.fill(self.asInstanceOf[Array[AnyRef]], ())
					case array :Array[AnyRef]  => Arrays.fill(array, from0, until0, null)
					case array :Array[Int]     => Arrays.fill(array, from0, until0, 0)
					case array :Array[Long]    => Arrays.fill(array, from0, until0, 0L)
					case array :Array[Double]  => Arrays.fill(array, from0, until0, 0.0)
					case array :Array[Byte]    => Arrays.fill(array, from0, until0, 0.toByte)
					case array :Array[Char]    => Arrays.fill(array, from0, until0, 0.toChar)
					case array :Array[Float]   => Arrays.fill(array, from0, until0, 0.0f)
					case array :Array[Short]   => Arrays.fill(array, from0, until0, 0.toShort)
					case array :Array[Boolean] => Arrays.fill(array, from0, until0, false)
				}
			}

		/** Sets all values in this array to `null`, but only if this is an instance of `Array[AnyRef]` (or its subclass). */
		@inline def clearIfRef() :Unit = clearIfRef(0, self.length)

		/** Sets all values in the specified range of this array to `null`,
		  * but only if this is an instance of `Array[AnyRef]` (or its subclass).
		  * Indices out of `[0, this.length)` are permitted, and the method will not attempt to set them.
		  */
		@inline def clearIfRef(from :Int, until :Int) :Unit = (self :Array[_]) match {
			case _     :Array[Unit] => ()
			case array :Array[AnyRef] if until > from & until > 0 && from < self.length =>
				val from0 = math.max(0, from)
				val until0 = math.min(self.length, until)
				Arrays.fill(array, from0, until0, null)
			case _ =>
		}
//
//		/** Fills the whole array with the given value. */
//		def fill(value :E) :Unit = ArrayExtension(self.asInstanceOf[Array[E]]).fill(value)
//
//		/** Fills the section of this array between the specified indices with the given value. */
//		def fill(from :Int, until :Int, value :E) :Unit =
//			ArrayExtension(self.asInstanceOf[Array[E]]).fill(from, until)(value)
//
//		def clear() :Unit = ArrayExtension(self).clear()
//
//		def clear(from :Int, until :Int) :Unit = ArrayExtension(self).clear(from, until)


		/** Swaps (in place) elements at indices `i` and `j`. */
		@inline def swap(i :Int, j :Int) :Unit = {
			val arr = self
			val boo = arr(i)
			arr(i)  = arr(j)
			arr(j)  = boo
		}

		@throws[IndexOutOfBoundsException]("if idx < 0 or idx >= this.length")
		def update(idx :Int, elem :E) :Unit = self(idx) = elem.asInstanceOf[Unknown]

		/** Sets all elements in the array to values returned by the argument function applied to element's index. */
		@inline def updateAll(f :Int => E) :Unit = updateAll(0, self.length)(f)

		/** Sets all elements in the given index range in this array to values returned by the argument function
		  * applied to element's index. If `until < from` the call has no effect.
		  * @return the number of updated elements.
		  */
//		  * @return the number of actual elements set, after adjusting the indices to legal boundaries.
		@throws[IndexOutOfBoundsException]("if either from or until is outside range [0, this.length).")
		def updateAll(from :Int, until :Int)(f :Int => E) :Unit =
			ArrayLikeSpecOps.updateAll(self, from, until)(f.castParam2[Unknown])

		/** Sets the values at indices `index, index + 1, index + 2, ...` to `first, second, elems.head`
		  * and subsequent elements of `rest`.
		  * @return the number of updated elements.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + 2 + rest.length > this.length")
		@inline def updateAll(index :Int, first :E, second :E, rest :E*) :Int = {
			self(index) = first.asInstanceOf[Unknown]
			self(index + 1) = second.asInstanceOf[Unknown]
			updateAll(index + 2, rest) + 2
		}

		/** Sets the values at indices `index, index + 1, index + 2, ...` to `first, second, elems.head`
		  * and subsequent elements of `rest`.
		  * @return the number of updated elements.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.size > this.length")
		def updateAll(index :Int, elems :IterableOnce[E]) :Int = {
			val thisSize = self.length
			val thatSize = elems.knownSize
			if (index < 0 | index > thisSize | thatSize >= 0 & index > thisSize - thatSize)
				throw new ArrayIndexOutOfBoundsException(
					errorString(self) + ".updateAll(" + index + ", " + errorString(elems) + ")"
				)
			if (thatSize >= 0)
				elems.castParam[Unknown].toBasicOps.copyToArray(self, index)
			else
				elems.castParam[Unknown].toBasicOps.foldLeft(index) { (i, elem) =>
					self(i) = elem; i + 1
				} - index
		}

		/** Sets the values at indices `index, index + 1, ...` to subsequent elements of `elems`.
		  * @return the number of updated elements.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.length > this.length")
		def updateAll(index :Int, elems :ArrayLike[E]) :Int = {
			val thisSize = self.length
			val thatSize = elems.length
			if (index < 0 | index > thisSize - thatSize)
				throw new ArrayIndexOutOfBoundsException(
					errorString(self) + ".updateAll(" + index + ", " + errorString(elems) + ")"
				)
			ArrayLike.copy(elems, 0, self, index, thatSize)
			thatSize
		}



		/** Sets the values at indices `index, index + 1, index + 2, ...` to `first, second, elems.head`
		  * and subsequent elements of `rest`. If any of the indices in the range covering all provided elements
		  * is out of range, it is simply ignored. For example,
		  * {{{
		  *     > RefArray("You", "Boo", "I").updateAll(-1, "Imoen", "CHARNAME", "Miniature Giant Space Hamster")
		  *     > Array[AnyRef]("CHARNAME", "Miniature Giant Space Hamster", "I")
		  * }}}
		  * @return the number of updated elements.
		  */
		def overwrite(index :Int, first :E, second :E, rest :E*) :Int = {
			val length = self.length
			index match {
				case Int.MinValue => 0
				case -1 =>
					if (length == 0)
						0
					else {
						self(0) = second.asInstanceOf[Unknown]
						rest.castParam[Unknown].toBasicOps.copyToArray(self, 1) + 1
					}
				case _ if index >= length => 0
				case _ if index < 0 =>
					rest.castParam[Unknown].toBasicOps.copyRangeToArray(self, 0, -index, length)
				case _ =>
					self(index)     = first.asInstanceOf[Unknown]
					if (index == length - 1)
						1
					else {
						self(index + 1) = second.asInstanceOf[Unknown]
						rest.castParam[Unknown].toBasicOps.copyToArray(self, index + 2, Int.MaxValue) + 2
					}
			}
		}

		/** Sets the values at indices `index, index + 1, index + 2, ...` to `first, second, elems.head`
		  * and subsequent elements of `rest`. If any of the indices in the range covering all provided elements
		  * is out of range, it is simply ignored. For example,
		  * {{{
		  *     > RefArray("You", "Boo", "I").updateAll(-1, "Imoen", "CHARNAME", "Miniature Giant Space Hamster")
		  *     > Array[AnyRef]("CHARNAME", "Miniature Giant Space Hamster", "I")
		  * }}}
		  * @return the number of updated elements.
		  */
		def overwrite(index :Int, elems :IterableOnce[E]) :Int =
			if (index >= 0)
				elems.castParam[Unknown].toBasicOps.copyToArray(self, index)
			else
				elems.castParam[Unknown].toBasicOps.copyRangeToArray(self, -index, 0, Int.MaxValue) + index

		/** Sets the values at indices `index, index + 1, ...` to subsequent elements of `elems`.
		  * @return the number of updated elements.  If any of the indices in the range covering all provided elements
		  * is out of range, it is simply ignored. For example,
		  * {{{
		  *     > RefArray("You", "Boo", "I").updateAll(-1, "Imoen", "CHARNAME", "Miniature Giant Space Hamster")
		  *     > Array[AnyRef]("CHARNAME", "Miniature Giant Space Hamster", "I")
		  * }}}
		  * @return the number of updated elements.
		  */
		def overwrite(index :Int, elems :ArrayLike[E]) :Int =
			if (index >= 0)
				elems.castParam[Unknown].copyToArray(self, index)
			else
				elems.castParam[Unknown].copyRangeToArray(self, -index, 0, Int.MaxValue)



		//name reverse is already used by ArrayOps to produce a copy
		/** Reverses the whole array in place. */
		@inline def reverseInPlace() :Unit = reverseInPlace(0, self.length)

		/** Reverses in place the section of this array between indices `from` (inclusive) and `until` (exclusive).
		  * If `until <= from`, or `until <= 0`, or `from >= length` the call has no effect. Passing a negative `from`,
		  * or `until > length` has the same effect as passing `0` and `length`, respectively.
		  */
		def reverseInPlace(from :Int, until :Int) :Unit = {
			val a = self
			var i = math.max(from, 0)
			var j = math.min(until, self.length)
			while (i < j) {
				j -= 1
				val boo = a(i)
				a(i)    = a(j)
				a(j)    = boo
				i += 1
			}
		}

		/** Shifts in place all the elements in the array down by `n` positions, modulo the length of the array.
		  * Element at position `n` is moved to index `0`, element at position `n + 1` to index `1`, etc.
		  * Element at `0` is moved to index `n`, element at `1` to index `n + 1`, etc.
		  */
		@inline def rotateLeft(n :Int) :Unit = rotateLeft(0, self.length)(n)

		/** Shifts in place the elements in the array between indices `from` (inclusive) and `until` (exclusive)
		  * down by `n` positions, modulo the length of the index range.
		  * Element at position `from + n` is moved to index `from`, element at position `from + n + 1`
		  * to index `from + 1`, etc. Element at `from` is moved to index `from + n`, element at `from + 1`
		  * to index `from + n + 1`, etc. If `until <= from`, or `until <= 0`, or `from >= length`,
		  * the call has no effect. Passing negative `from` is the same as passing zero, passing `until > length`
		  * clips it the length of the array.
		  */ //consider: strict indexing
		def rotateLeft(from :Int, until :Int)(n :Int) :Unit = {
			val array  = self
			val length = array.length
			val from0  = math.max(from, 0)
			val until0 = math.min(until, length)
			val range  = until0 - from0
			if (until > 0 & until0 > from0) {
				var pivot = n % range
				if (pivot < 0)
					pivot = range + pivot
				pivot match { //try to use fast platform arraycopy if the shift is very short
					case 0 =>
					case 1 =>
						val saved = array(from0)
						arraycopy(array, from0 + 1, array, from0, range - 1)
						array(until0 - 1) = saved
					case 2 =>
						val saved0 = array(from0)
						val saved1 = array(from0 + 1)
						arraycopy(array, from0 + 2, array, from0, range - 2)
						array(until0 - 1) = saved1
						array(until0 - 2) = saved0
					case _ if pivot == range - 1 =>
						val saved = array(until0 - 1)
						arraycopy(array, from0, array, from0 + 1, range - 1)
						array(from0) = saved
					case _ if pivot == range - 2 =>
						val saved1 = array(until0 - 1)
						val saved0 = array(until0 - 2)
						arraycopy(array, from0, array, from0 + 2, range - 2)
						array(from0)     = saved0
						array(from0 + 1) = saved1
					case _ =>
						reverseInPlace(from0, from0 + pivot)
						reverseInPlace(from0 + pivot, until0)
						reverseInPlace(from0, until0)
				}
			}
		}

		/** Shifts in place all the elements in the array down by `n` positions, modulo the length of the array.
		  * Element at position `n` is moved to index `0`, element at position `n + 1` to index `1`, etc.
		  * Element at `0` is moved to index `n`, element at `1` to index `n + 1`, etc.
		  */
		@inline def rotateRight(n :Int) :Unit = rotateRight(0, self.length)(n)

		/** Shifts in place the elements in the array between indices `from` (inclusive) and `until` (exclusive)
		  * down by `n` positions, modulo the length of the index range.
		  * Element at position `from + n` is moved to index `from`, element at position `from + n + 1`
		  * to index `from + 1`, etc. Element at `from` is moved to index `from + n`, element at `from + 1`
		  * to index `from + n + 1`, etc. If `until <= from`, or `until <= 0`, or `from >= length`,
		  * the call has no effect. Passing negative `from` is the same as passing zero, passing `until > length`
		  * clips it the length of the array.
		  */
		def rotateRight(from :Int, until :Int)(n :Int) :Unit = {
			val length = self.length
			val start = math.min(length, math.max(from, 0))
			val end   = math.min(length, math.max(from, until))
			if (start < end)
				if (n < 0)
					rotateLeft(from, until)(start - end - n) //guards against underflow on -(n == Int.MinValue)
				else
					rotateLeft(from, until)(end - start - n)
		}

		/** Shifts the contents of this array left by `n` position. Range `[n, length)` is copied to `[0, length - n)`.
		  * Range `[length - n, length)` remains unchanged. If `n >= this.length`, the call has no effect.
		  * If `n` is negative, the result is equivalent to
		  * [[net.noresttherein.sugar.collections.extensions.ArrayExtension.shiftRight shiftRight]]`(-n)`.
		  */
		def shiftLeft(n :Int) :Unit = {
			val length = self.length
			if (n < 0) {
				if (n > Int.MinValue && -n < length)
					arraycopy(self, 0, self, -n, length + n)
			} else if (n < length)
				arraycopy(self, n, self, 0, length - n)
		}

		/** Shifts the contents of this array right by `n` position. Range `[0, length - n)` is copied to `[n, length)`.
		  * Range `[0, n)` remains unchanged. If `n >= this.length`, the call has no effect.
		  * If `n` is negative, the result is equivalent to
		  * [[net.noresttherein.sugar.arrays.MutableArray.MutableArrayExtension.shiftLeft shiftLeft]]`(-n)`.
		  */
		def shiftRight(n :Int) :Unit = {
			val length = self.length
			if (n < 0) {
				if (n > Int.MinValue && -n < length)
					arraycopy(self, -n, self, 0, length + n)
			} else if (n < length)
				arraycopy(self, 0, self, n, length - n)
		}

		/** Shifts the contents from range `[from, until)` of this array left by `n` positions.
		  * The value at `from` is moved to index `from - n`, value at `from + 1` to `from - n + 1`, and so on.
		  * Any write which would happen outside the valid index range of the array is ignored;
		  * the actual range moved is `[from max n min until, until)`. The contents from `[from max n, until)`
		  * remain unchanged. If `n >= until`, the call has no effect. If `n` is negative, the result is equivalent to
		  * [[net.noresttherein.sugar.arrays.MutableArray.MutableArrayExtension.shiftRight shiftRight]]`(from, until)(-n)`.
		  * @note this method may update elements in this array outside range `[from, until)`
		  *       (depending on the signum of `n`).
		  */ //consider: renaming to shiftLeftInPlace
//		@throws[IndexOutOfBoundsException]("if until < 0, until < from, or from > this.length.")
		def shiftLeft(from :Int, until :Int)(n :Int) :Unit =
			if (n < 0) {
				if (n > Int.MinValue)
					shiftRight(from, until)(-n)
			} else if (n > 0) {
				val length = self.length
//				if (until < 0 | until < from | from > length)
//					throw new IndexOutOfBoundsException(
//						errorString(self) + ".shiftLeft(" + from + ", " + until + ")(" + n + ")"
//					)
				val start  = math.min(length, math.max(from, 0))
				val end    = math.min(length, math.max(from, until))
				if (n < end) {
					val offset = math.max(start, n)
					arraycopy(self, offset, self, offset - n, end - offset)
				}
			}

		/** Shifts the contents from range `[from, until)` of this array right by `n` positions.
		  * The value at `from` is moved to index `from + n`, value at `from + 1` to `from + n + 1`, and so on.
		  * Any write which would happen outside the valid index range of the array is ignored;
		  * the actual range moved is `[from max length - n min until, until)`.
		  * The contents from `[from, from max until - n)` remain unchanged. If `n >= until`, the call has no effect.
		  * If `n` is negative, the result is equivalent to
		  * [[net.noresttherein.sugar.arrays.MutableArray.MutableArrayExtension.shiftLeft shiftLeft]]`(from, until)(-n)`.
		  * @note this method may update elements in this array outside range `[from, until)`
		  *       (depending on the signum of `n`).
		  */ //consider: renaming to shiftRightInPlace
//		@throws[IndexOutOfBoundsException]("if until < 0, until < from, or from > this.length.")
		def shiftRight(from :Int, until :Int)(n :Int) :Unit =
			if (n < 0) {
				if (n > Int.MinValue)
					shiftLeft(from, until)(-n)
			} else if (n > 0) {
				val length = self.length
//				if (until < 0 | until < from | from > length)
//					throw new IndexOutOfBoundsException(
//						errorString(self) + ".shiftRight(" + from + ", " + until + ")(" + n + ")"
//					)
				val start  = math.min(length, math.max(from, 0))
				val end    = math.min(length - n, math.max(start, until))
				if (n < length - start)
					arraycopy(self, start, self, start + n, end - start)
			}

		@inline final def sortInPlace[U >: E]()(implicit ordering :Ordering[U]) :Unit =
			Sorting.stableSort(self)(ordering.castParam[Unknown])

		@throws[IndexOutOfBoundsException]("if either from or until are outside of [0, this.length) range.")
		@inline final def sortInPlace[U >: E](from :Int, until :Int)(implicit ordering :Ordering[U]) :Unit =
			Sorting.stableSort(self, from, until)(ordering.castParam[Unknown])

		@inline final def sortInPlaceWith[U >: E](lt :(U, U) => Boolean) :Unit =
			Sorting.stableSort(self, lt.castParams[Unknown, Unknown, Boolean])

		@throws[IndexOutOfBoundsException]("if either from or until are outside of [0, this.length) range.")
		@inline final def sortInPlaceWith[U >: E](from :Int, until :Int)(lt :(U, U) => Boolean) :Unit =
			Sorting.stableSort(self, lt.castParams[Unknown, Unknown, Boolean], from, until)

		@inline final def sortInPlaceBy[A](f :E => A)(implicit ordering :Ordering[A]) :Unit = {
			val by = f.castParam1[Unknown]
			Sorting.stableSort(self, (a :Unknown, b :Unknown) => ordering.lt(by(a), by(b)))
		}

		@throws[IndexOutOfBoundsException]("if either from or until are outside of [0, this.length) range.")
		@inline final def sortInPlaceBy[A](from :Int, until :Int)(f :E => A)(implicit ordering :Ordering[A]) :Unit = {
			val by = f.castParam1[Unknown]
			Sorting.stableSort(self, (a :Unknown, b :Unknown) => ordering.lt(by(a), by(b)), from, until)
		}

		@inline def shuffle()(implicit random :Random) :Unit = shuffle(0, self.length, random.self)
		@inline def shuffle(from :Int, until :Int)(implicit random :Random) :Unit = shuffle(from, until, random.self)

		def shuffle(from :Int, until :Int, random :RandomGenerator) :Unit =
			ArrayLikeSpecOps.shuffle(self, from, until)(random)

		def mutator :Mutator[E] = MutableArrayMutator(self.asInstanceOf[Array[E]])

		@inline def toSeq        :Seq[E] = toIndexedSeq
		@inline def toIndexedSeq :IndexedSeq[E] =
			IArrayLikeSlice.wrap(ArrayFactory.copyOf(self).asInstanceOf[IArrayLike[E]])

		@inline def toOps        :mutable.IndexedSeqOps[E, RefArray, MutableArray[E]] =
			if (self.isInstanceOf[Array[Any]])
				new RefArrayAsSeq(self.castFrom[Array[Unknown], RefArray[E]])
			else
				new ArrayAsSeq(self.castFrom[Array[Unknown], Array[E]])
	}
}






private abstract class MutableArrayIsSeqOps[E, A[X] <: MutableArray[X]](array :A[E])
	extends ArrayLikeIsSeqOps[E, A](array.asInstanceOf[Array[E]])
		with collection.StrictOptimizedSeqOps[E, mutable.Seq, A[E]] with mutable.IndexedSeqOps[E, mutable.Seq, A[E]]
{

	override def update(idx :Int, elem :E) :Unit = coll(idx) = elem
	override def isMutable = true
	@nowarn("cat=deprecation")
	override def toIterable      = MutableArray.Wrapped(array)
	override def iterableFactory = mutable.IndexedSeq
}


