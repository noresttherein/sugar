package net.noresttherein.sugar.arrays


import java.lang.System.arraycopy
import java.util.Arrays

import scala.Array.UnapplySeqWrapper
import scala.annotation.nowarn
import scala.collection.generic.IsSeq
import scala.collection.mutable.ArrayBuffer
import scala.collection.{IterableFactory, mutable}
import scala.reflect.{ClassTag, classTag}
import scala.util.Sorting

import net.noresttherein.sugar.arrays.MutableArray.extensions.MutableArrayExtensionConversion
import net.noresttherein.sugar.arrays.extensions.{ArrayExtension, ArrayCompanionExtension, MutableArrayExtension}
import net.noresttherein.sugar.collections.{ArrayIterableOnce, ArraySlice, IArrayLikeSlice, MatrixBuffer, MutableArraySlice, RefArraySlice}
import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.reflect.extensions.ClassExtension
import net.noresttherein.sugar.typist.PriorityConversion
import net.noresttherein.sugar.typist.casting.extensions.castingMethods
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}


/** Companion definitions for Scala arrays, including `Array[AnyRef]`
  * as [[net.noresttherein.sugar.arrays.RefArray! RefArray]].
  * @see [[net.noresttherein.sugar.arrays.MutableArray! MutableArray]]
  * @define Coll `MutableArray`
  * @define coll mutable array-like
  */
@SerialVersionUID(Ver)
case object MutableArray extends IterableFactory.Delegate[MutableArray](RefArray) {

	class MutableArrayExtension[E] private[arrays] (private val self :Array[_]) extends AnyVal {
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
				self match {
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
				self match {
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
		@inline def clearIfRef(from :Int, until :Int) :Unit = self match {
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
			val arr = self.asInstanceOf[Array[E]]
			val boo = arr(i)
			arr(i)  = arr(j)
			arr(j)  = boo
		}

		def update(idx :Int, elem :E) :Unit = self.castFrom[Array[_], Array[E]].update(idx, elem)

		/** Sets the values at indices `index, index + 1, index + 2, ...` to `first, second, elems.head`
		  * and subsequent elements of `rest`. If any of the indices in the range covering all provided elements
		  * is out of range, it is simply ignored.
		  */
		def updateAll(idx :Int, first :E, second :E, rest :E*) :Unit =
			ArrayExtension(self).asInstanceOf[Array[E]].updateAll(idx, first, second, rest :_*)

		/** Sets the values at indices `index, index + 1, ...` to subsequent elements of `elems`.
		  * If any of the indices in the range covering all provided elements is out of range, it is simply ignored.
		  */
		def updateAll(idx :Int, elems :IterableOnce[E]) :Unit =
			ArrayExtension(self.asInstanceOf[Array[E]]).updateAll(idx, elems)

		//name reverse is already used by ArrayOps to produce a copy
		/** Reverses the whole array in place. */
		@inline def reverseInPlace() :Unit = reverseInPlace(0, self.length)

		/** Reverses in place the section of this array between indices `from` (inclusive) and `until` (exclusive).
		  * If `until <= from`, or `until <= 0`, or `from >= length` the call has no effect. Passing a negative `from`,
		  * or `until > length` has the same effect as passing `0` and `length`, respectively.
		  */
		def reverseInPlace(from :Int, until :Int) :Unit = {
			val a = self.asInstanceOf[Array[E]]
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
			val array  = self.asInstanceOf[Array[E]]
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
			Sorting.stableSort(self.asInstanceOf[Array[U]])

		@throws[IndexOutOfBoundsException]("if either from or until are outside of [0, this.length) range.")
		@inline final def sortInPlace[U >: E](from :Int, until :Int)(implicit ordering :Ordering[U]) :Unit =
			Sorting.stableSort(self.asInstanceOf[Array[U]], from, until)

		@inline final def sortInPlaceWith[U >: E](lt :(U, U) => Boolean) :Unit =
			Sorting.stableSort(self.asInstanceOf[Array[U]], lt)

		@throws[IndexOutOfBoundsException]("if either from or until are outside of [0, this.length) range.")
		@inline final def sortInPlaceWith[U >: E](from :Int, until :Int)(lt :(U, U) => Boolean) :Unit =
			Sorting.stableSort(self.asInstanceOf[Array[U]], lt, from, until)

		@inline final def sortInPlaceBy[A](f :E => A)(implicit ordering :Ordering[A]) :Unit =
			Sorting.stableSort(self.asInstanceOf[Array[E]], (a :E, b :E) => ordering.lt(f(a), f(b)))

		@throws[IndexOutOfBoundsException]("if either from or until are outside of [0, this.length) range.")
		@inline final def sortInPlaceBy[A](from :Int, until :Int)(f :E => A)(implicit ordering :Ordering[A]) :Unit =
			Sorting.stableSort(self.asInstanceOf[Array[E]], (a :E, b :E) => ordering.lt(f(a), f(b)), from, until)


		@inline def toSeq        :Seq[E] = toIndexedSeq
		@inline def toIndexedSeq :IndexedSeq[E] =
			IArrayLikeSlice.wrap(self.clone().asInstanceOf[IArrayLike[E]])

		@inline def toOps        :mutable.IndexedSeqOps[E, RefArray, MutableArray[E]] =
			if (self.isInstanceOf[Array[Any]])
				new RefArrayAsSeq(self.castFrom[Array[_], RefArray[E]])
			else
				new ArrayAsSeq(self.castFrom[Array[_], Array[E]])
	}


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

		def unapply[E](elems :mutable.SeqOps[E, generic.Any, _]) :Opt[MutableArray[E]] = {
			val length = elems.length
			elems match {
				case seq   :mutable.ArraySeq[_] =>
					Got(seq.array.castFrom[Array[_], MutableArray[E]])
				case seq   :ArrayBuffer[_] if CheatedAccess.array(seq).length == length =>
					Got(CheatedAccess.array(seq).castFrom[Array[_], RefArray[E]])
				case slice :ArrayIterableOnce[_] if slice.isMutable && slice.unsafeArray.length == length =>
					Got(slice.unsafeArray.castFrom[Array[_], MutableArray[E]])
				case seq   :MatrixBuffer[_] if seq.dim == 1 && seq.startIndex == 0 && seq.data1.length == length =>
					Got(seq.data1.castFrom[Array[_], MutableArray[E]])
				case _ =>
					Lack
			}
		}

		def unapply[E :ClassTag](elems :IterableOnce[E]) :Opt[MutableArray[E]] = elems match {
//			case RefArray.Wrapped(array) => Got(array)
			case seq   :mutable.ArraySeq[_] if classTag[E].runtimeClass <:< seq.array.getClass.getComponentType =>
				Got(seq.array.castFrom[Array[_], MutableArray[E]])
			case seq   :ArrayBuffer[_] if CheatedAccess.array(seq).length == seq.length =>
				Got(CheatedAccess.array(seq).castFrom[Array[_], RefArray[E]])
			case slice :ArrayIterableOnce[_]
				if slice.isMutable && slice.startIndex == 0 && slice.knownSize == slice.unsafeArray.length &&
					classTag[E].runtimeClass <:< slice.unsafeArray.getClass.getComponentType =>
				Got(slice.unsafeArray.castFrom[Array[_], MutableArray[E]])
			case seq   :MatrixBuffer[_]
				if seq.dim == 1 && seq.startIndex == 0 && seq.length == seq.data1.length &&
					classTag[E].runtimeClass <:< seq.iterableEvidence.runtimeClass =>
				Got(seq.data1.castFrom[Array[_], MutableArray[E]])
			case _ =>
				Lack
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

			def unapply[E](elems :mutable.SeqOps[E, generic.Any, _]) :Opt[(MutableArray[E], Int, Int)] = {
				val length = elems.length
				elems match {
					case seq :mutable.ArraySeq[E] =>
						Got(seq.array.castFrom[Array[_], MutableArray[E]], 0, length)
					case seq :ArrayBuffer[E] =>
						Got(CheatedAccess.array(seq).castFrom[Array[AnyRef], RefArray[E]], 0, length)
					case seq :ArrayIterableOnce[E] if seq.isMutable =>
						val start = seq.startIndex
						Got(seq.unsafeArray.castFrom[Array[_], MutableArray[E]], start, start + length)
					case seq :MatrixBuffer[E] if seq.dim == 1 && seq.startIndex + length <= seq.data1.length =>
						Got((seq.data1, seq.startIndex, seq.startIndex + length))
					case _ =>
						Lack
				}
			}

			def unapply[E :ClassTag](elems :IterableOnce[E]) :Opt[(MutableArray[E], Int, Int)] = elems match {
				case seq :mutable.ArraySeq[_] if classTag[E].runtimeClass <:< seq.array.getClass.getComponentType =>
					Got((seq.array.castFrom[Array[_], Array[E]], 0, seq.array.length))

				case seq :ArrayBuffer[_] =>
					Got((CheatedAccess.array(seq).castFrom[Array[AnyRef], RefArray[E]], 0, seq.length))

				case slice :ArrayIterableOnce[E] if slice.isMutable && slice.knownSize >= 0 =>
					val array = slice.unsafeArray.castFrom[Array[_], Array[E]]
					val start = slice.startIndex
					if (classTag[E].runtimeClass <:< array.getClass.getComponentType)
						Got((array, start, start + slice.knownSize))
					else
						Lack
				case seq :MatrixBuffer[E]
					if seq.dim == 1 && seq.startIndex + seq.length <= seq.data1.length &&
						classTag[E].runtimeClass <:< seq.iterableEvidence.runtimeClass =>
					Got(seq.data1, seq.startIndex, seq.startIndex + seq.length)
				case _ =>
					Lack
			}

		}
	}


	implicit def MutableArrayOrdering[E :Ordering] :Ordering[MutableArray[E]] =
		Array.ArrayOrdering[E].castFrom[Ordering[Array[E]], Ordering[MutableArray[E]]]

	implicit def MutableArrayToSeq[E](array :MutableArray[E]) :collection.IndexedSeq[E] = Wrapped(array)

	//fixme: precedence conflicts with ArrayLikeExtension
	implicit def MutableArrayIsSeq[E] :IsSeq[MutableArray[E]] { type A = E; type C = MutableArray[E] } =
		Array.ArrayIsSeq.asInstanceOf[IsSeq[MutableArray[E]] { type A = E; type C = MutableArray[E] }]
//		IsSeqPrototype.asInstanceOf[IsSeq[ArrayLike[E]] { type A = E; type C = MutableArray[E] }]
//	implicit def MutableArrayIsSeq[E] :IsSeq[MutableArray[E]] { type A = E; type C = MutableArray[E] } =
//		RefArray.RefArrayIsSeq.asInstanceOf[IsSeq[MutableArray[E]] { type A = E; type C = MutableArray[E] }]
//		IsSeqPrototype.asInstanceOf[IsSeq[ArrayLike[E]] { type A = E; type C = MutableArray[E] }]
//
//	//We don't know what the particular element type of the array will be, so we must use Scala's polymorphic delegates.
//	private class MutableArrayIsSeq[E] extends ArrayLikeIsSeqTemplate[E, mutable.Seq, MutableArray] {
//		override def apply(array :MutableArray[E]) = //not ArrayLikeAsSeq because it must use collection.Seq, not ArrayLike
//			new MutableArrayIsSeqOps[E, MutableArray](array.asInstanceOf[Array[E]]) {
//				protected override def fromSpecific(coll :IterableOnce[E]) :MutableArray[E] = RefArray.from(coll)
//				protected override def newSpecificBuilder :Builder[E, MutableArray[E]] = RefArray.newBuilder
//			}
//		private def readResolve = MutableArray.MutableArrayIsSeq
//	}
//	private[this] val IsSeqPrototype :MutableArrayIsSeq[Any] = new MutableArrayIsSeq[Any]

//
//	private[arrays] sealed trait conversions extends Any with ArrayLike.conversions {
//		//fixme: conflicts with ArrayLikeExtension
////		@inline implicit final def MutableArrayToSeq[E](self :MutableArray[E]) :mutable.IndexedSeq[E] = Wrapped(self)
//	}

	/** Mixin trait with extension methods conversion for `MutableArray` subtypes.
	  * @define Coll `MutableArray`
	  * @define Extension `MutableArrayExtension[E, Arr]`
	  */
	private[arrays] trait extensions extends Any with ArrayLike.extensions {
///*		@inline implicit final def MutableArrayExtension[A](self :MutableArray[A])*/ :MutableArrayExtension[A] =
//			new MutableArrayExtension(self.asInstanceOf[Array[_]])
		/** Extension methods for all `MutableArray[E]` subtypes.
		  * $conversionInfo 
		  */
		implicit final def MutableArrayExtension[E] :MutableArrayExtensionConversion[E] =
			extensions.MutableArrayExtensionConversionPrototype.asInstanceOf[MutableArrayExtensionConversion[E]]
	}
	
	@SerialVersionUID(Ver)
	object extensions extends extensions {
		sealed trait MutableArrayExtensionConversion[E] extends (MutableArray[E] => MutableArrayExtension[E]) {
			@inline final def apply(v1 :MutableArray[E])(implicit dummy :DummyImplicit) :MutableArrayExtension[E] =
				new MutableArrayExtension(v1.asInstanceOf[Array[_]])
		}
		private def newMutableArrayExtensionConversion[E] =
			new PriorityConversion.Wrapped[MutableArray[E], MutableArrayExtension[E]](
				(arr :MutableArray[E]) => new MutableArrayExtension(arr.asInstanceOf[Array[_]])
			) with MutableArrayExtensionConversion[E]
		private val MutableArrayExtensionConversionPrototype :MutableArrayExtensionConversion[Any] =
			newMutableArrayExtensionConversion
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

