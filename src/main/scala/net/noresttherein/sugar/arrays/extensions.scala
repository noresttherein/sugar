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

//import net.noresttherein.sugar.arrays.ArrayLike.ArrayCompatibility
import net.noresttherein.sugar.arrays.ArrayOrdering.{ByteArrayOrdering, CharArrayOrdering, DoubleArrayIEEEOrdering, DoubleArrayOrdering, DoubleArrayTotalOrdering, FloatArrayIEEEOrdering, FloatArrayOrdering, FloatArrayTotalOrdering, IntArrayOrdering, LongArrayOrdering, ShortArrayOrdering}
import net.noresttherein.sugar.arrays.extensions.{ArrayExtensionConversion, ArrayExtensionConversionPrototype, ArrayObjectExtension}
import net.noresttherein.sugar.collections.{ArraySlice, ArrayStepper}
import net.noresttherein.sugar.collections.extensions.{IterableOnceExtension, IteratorExtension, IteratorObjectExtension, StepperObjectExtension}
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.extensions.ClassExtension
import net.noresttherein.sugar.numeric.BitLogic
import net.noresttherein.sugar.reflect.ArrayClass
import net.noresttherein.sugar.reflect.extensions.classNameMethods
import net.noresttherein.sugar.reflect.prettyprint.fullNameOf
import net.noresttherein.sugar.typist.{PriorityConversion, Unknown}
import net.noresttherein.sugar.typist.casting.extensions.{castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.witness.Ignored




/** Collected definitions of implicit conversions providing extension methods to `Array` and all other
  * [[net.noresttherein.sugar.arrays.ArrayLike ArrayLike]] subtypes.
  */
trait extensions extends Any with IArray.extensions with RefArray.extensions with IRefArray.extensions {
	implicit final def ArrayExtension[E] :ArrayExtensionConversion[E] =
		ArrayExtensionConversionPrototype.asInstanceOf[ArrayExtensionConversion[E]]

	/** Adds extra extension methods to the `Array` object. */
	@inline implicit final def ArrayObjectExtension(self :Array.type) :ArrayObjectExtension =
		new ArrayObjectExtension {}
}



@SerialVersionUID(Ver)
object extensions extends extensions {

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
		@inline def asIArray :IArray[E] = self.asInstanceOf[IArray[E]]

		/** Casts this array to `IRefArray[E]`. This always succeeds, but lifting mutation protection
		  * may have undesired consequences if a reference to this array escapes and is modified.
		  */
		@inline def asIRefArray(implicit ofAny :AnyRef <:< E) :IRefArray[E] = self.asInstanceOf[IRefArray[E]]

		/** Casts this `Array[Any]` to a `RefArray[Any]`. */
		@inline def asRefArray(implicit ofAny :AnyRef <:< E) :RefArray[E] = self.asInstanceOf[RefArray[E]]

		/** Creates an exact copy of this array and returns it as an immutable `IArray[E]`. */
		@inline def toIArray :IArray[E] = ArrayFactory.copyOf(self).asInstanceOf[IArray[E]]

		/** Copies the elements of this array to a new `Array[AnyRef]`, and returns it as an `RefArray[A]`. */
		@inline def toRefArray[A >: E] :RefArray[A] = RefArray.copyOf(self)

		/** Copies the elements of this array to a new `Array[AnyRef]`, and returns it as an `IRefArray[E]`. */
		@inline def toIRefArray :IRefArray[E] = IRefArray.copyOf(self)
//
//		//consider: exporting it as a specialized iterator maybe.
//		def iterator :Iterator[E] = ArrayIterator(self)
//		def reverseIterator :Iterator[E] = ReverseArrayIterator(self)

		/** Returns `getClass.`[[java.lang.Class.getComponentType getComponentType]]. */
		@inline def getComponentType :Class[E] = self.getClass.getComponentType.castParam[E]
		//clashes with conversion to ArraySeq in predef

		/** Returns `getClass.`[[java.lang.Class.getComponentType getComponentType]] as a `ClassTag`. */
		@inline def getComponentClassTag :ClassTag[E] =
			if (self.getClass.getComponentType eq classOf[Any]) ClassTag.Any.castParam[E]
			else ClassTag[E](self.getClass.getComponentType)

		//moved to ArrayLikeOps
//		/** Same as [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.foreach foreach]],
//		  * defined here because of the presence of an overloaded variant.
//		  */
//		@inline def foreach[U](f :E => U) :Unit = foreach(0, self.length)(f)
//
//		/** Executes the given function or all elements in the index range `[from, until)`. */
//		def foreach[U](from :Int, until :Int)(f :E => U) :Unit = ArrayLikeOps.foreach(self, from, until)(f)
/*
		//todo: replace this with internal specialization extracted to ArrayLikeOps
		if (until > from & until > 0 && from < self.length) {
			val from0  = math.max(0, from)
			val until0 = math.min(self.length, until)
			var i      = from0
			(self :Array[_]) match {
				case arr :Array[AnyRef] =>
					def foreachRef(f :AnyRef => U) :Unit =
						while (i < until0) {
							f(arr(i)); i += 1
						}
					foreachRef(f.asInstanceOf[AnyRef => U])
				case arr :Array[Int] =>
					def foreachInt(f :Int => U) :Unit =
						while (i < until0) {
							f(arr(i)); i += 1
						}
					foreachInt(f.asInstanceOf[Int => U])
				case arr :Array[Long] =>
					def foreachLong(f :Long => U) :Unit =
						while (i < until0) {
							f (arr(i)); i += 1
						}
					foreachLong(f.asInstanceOf[Long => U])
				case arr :Array[Double] =>
					def foreachDouble(f :Double => U) :Unit =
						while (i < until0) {
							f (arr(i)); i += 1
						}
					foreachDouble(f.asInstanceOf[Double => U])
				case arr :Array[Char] =>
					def foreachChar(f :Char => U) :Unit =
						while (i < until0) {
							f (arr(i)); i += 1
						}
					foreachChar(f.asInstanceOf[Char => U])
				case arr :Array[Byte] =>
					def foreachByte(f :Byte => U) :Unit =
						while (i < until0) {
							f (arr(i)); i += 1
						}
					foreachByte(f.asInstanceOf[Byte => U])
				case arr :Array[Float] =>
					def foreachFloat(f :Float => U) :Unit =
						while (i < until0) {
							f (arr(i)); i += 1
						}
					foreachFloat(f.asInstanceOf[Float => U])
				case arr :Array[Short] =>
					def foreachShort(f :Short => U) :Unit =
						while (i < until0) {
							f (arr(i)); i += 1
						}
					foreachShort(f.asInstanceOf[Short => U])
				case arr :Array[Boolean] =>
					def foreachBoolean(f :Boolean => U) :Unit =
						while (i < until0) {
							f (arr(i)); i += 1
						}
					foreachBoolean(f.asInstanceOf[Boolean => U])
			}
		}
*/

	//todo: document and test these
	//moved to ArrayLikeExtension
//		@inline def startsWith[U >: E](that :ArrayLike[U]) :Boolean = startsWith(that, 0)
//		def startsWith[U >: E](that :ArrayLike[U], from :Int) :Boolean = {
//			val thisLength = self.length
//			val thatLength = that.length
//			val from0    = math.max(from, 0)
//			from0 <= thisLength - thatLength && mismatch(that, from0, from0 + thatLength, 0, thatLength) == -1
//		}
//		@inline def startsWith[U >: E](that :IterableOnce[U]) :Boolean =
//			mutable.ArraySeq.make(self).startsWith(that, 0)
//		@inline def startsWith[U >: E](that :IterableOnce[U], from :Int) :Boolean =
//			mutable.ArraySeq.make(self).startsWith(that, from)
//
//		def endsWith[U >: E](that :ArrayLike[U]) :Boolean = {
//			val thisLength = self.length
//			val thatLength = that.length
//			thatLength >= thisLength && mismatch(that, thisLength - thatLength, thisLength, 0, thatLength) == -1
//		}
//		@inline def endsWith[U >: E](that :Iterable[U]) :Boolean =
//			mutable.ArraySeq.make(self).endsWith(that)
//
//		/** Computes the length of the longest segment that starts from the first element
//		  * and whose elements all satisfy some predicate.
//		  */
//		def segmentLength(p :E => Boolean, from :Int = 0) :Int =
//			ArrayLikeOps.segmentLength(self, 0, self.length)(p, from)
//
//		/** The same as `ArrayOps.indexOf`, but specialized to each value type in order to run faster. */
//		def indexOf[U >: E](elem :U, from :Int = 0) :Int =
//			ArrayLikeOps.indexOf[U](self, 0, self.length)(elem, from)
//
//		/** The same as `ArrayOps.lastIndexOf`, but specialized to each value type in order to run faster. */
//		def lastIndexOf[U >: E](elem :U, end :Int = Int.MaxValue) :Int =
//			ArrayLikeOps.lastIndexOf[U](self, 0, self.length)(elem, end)
//
//		/** The same as `ArrayOps.indexWhere`, but specialized to each value type in order to run faster. */
//		def indexWhere(p :E => Boolean, from :Int = 0) :Int =
//			ArrayLikeOps.indexWhere(self, 0, self.length)(p, from)
//
//		/** The same as `ArrayOps.lastIndexWhere`, but specialized to each value type in order to run faster. */
//		def lastIndexWhere(p :E => Boolean, end :Int = 0) :Int =
//			ArrayLikeOps.lastIndexWhere(self, 0, self.length)(p, end)

	//moved to ArrayLikeExtension
//		/** Finds and returns the relative index of the first mismatch between two arrays,
//		  * or return -1 if no mismatch is found. If the arrays are of different lengths, but equal on all positions
//		  * of the shorter array, `-1` is returned.
//		  * @param that the second array to be tested for a mismatch
//		  * @return the relative index of the first mismatch between the two arrays over the specified ranges, or `-1`.
//		  */ //todo: move to ArrayLikeExtension
//		@inline def mismatch[U >: E](that :ArrayLike[U]) :Int =
//			mismatch(that, 0, self.length, 0, that.length)
//
//		/** Finds and returns the relative index of the first mismatch between two arrays over the specified ranges,
//		  * otherwise return -1 if no mismatch is found. The index will be in the range of 0 (inclusive) up
//		  * to the length (inclusive) of the smaller range. The starting indices of the two ranges must be valid indices
//		  * in their respective arrays. The ending indices are treated exclusively, and clipped to fit in ranges
//		  * `thisFrom..this.length` and `thatFrom..that.length`. If the ranges are of different lengths,
//		  * the longer one is truncated to the length of the shorter one.
//		  * @param that      the second array to be tested for a mismatch
//		  * @param thisFrom  the index (inclusive) of the first element in this array to be tested
//		  * @param thisUntil the index (exclusive) of the last element in the first array to be tested
//		  * @param thatFrom  the index (inclusive) of the first element in the second array to be tested
//		  * @param thatUntil the index (exclusive) of the last element in the second array to be tested
//		  * @return the relative index of the first mismatch between the two arrays over the specified ranges, otherwise `-1`.
//		  */
//		def mismatch[U >: E](that :ArrayLike[U], thisFrom :Int, thisUntil :Int, thatFrom :Int, thatUntil :Int) :Int = {
//			val from0 = math.max(0, math.min(thisFrom, self.length))
//			val from1 = math.max(0, math.min(thatFrom, that.length))
//			val until0 = math.max(from0, math.min(thisUntil, self.length))
//			val until1 = math.max(from1, math.min(thatUntil, that.length))
//			(self :ArrayLike[_], that :ArrayLike[_]) match {
//				case (a :Array[AnyRef], b :Array[AnyRef]) =>
//					Arrays.mismatch(a, from0, until0, b, from1, until1)
//				case (a :Array[Int], b :Array[Int]) =>
//					Arrays.mismatch(a, from0, until0, b, from1, until1)
//				case (a :Array[Long], b :Array[Long]) =>
//					Arrays.mismatch(a, from0, until0, b, from1, until1)
//				case (a :Array[Double], b :Array[Double]) =>
//					Arrays.mismatch(a, from0, until0, b, from1, until1)
//				case (a :Array[Byte], b :Array[Byte]) =>
//					Arrays.mismatch(a, from0, until0, b, from1, until1)
//				case (a :Array[Float], b :Array[Float]) =>
//					Arrays.mismatch(a, from0, until0, b, from1, until1)
//				case (a :Array[Char], b :Array[Char]) =>
//					Arrays.mismatch(a, from0, until0, b, from1, until1)
//				case (a :Array[Short], b :Array[Short]) =>
//					Arrays.mismatch(a, from0, until0, b, from1, until1)
//				case (a :Array[Boolean], b :Array[Boolean]) =>
//					Arrays.mismatch(a, from0, until0, b, from1, until1)
////				case _ if thisFrom < 0 | thatFrom < 0 || thisFrom > self.length || thatFrom > that.length =>
////					throw new IndexOutOfBoundsException(
////						s"${self.className}<${self.length}>.mismatch(${that.className}<${that.length}>, " +
////							s"$thisFrom, $thisUntil, $thatFrom, $thatUntil)"
////					)
//				case _ =>
//					var i = from0; var j = from1
//					val end = from0 + math.min(until0 - from0, until1 - from1)
//					while (i < end && self(i) == that(j)) {
//						i += 1; j += 1
//					}
//					if (i < end) i - from0
//					else if (until0 - from0 == until1 - from1) -1
//					else end - from0
//			}
//		}


//		//this duplicates the functionality available through an implicit conversion to IndexedSeq. Is there a point?
//		/** Search this array for a specific element.
//		  * The array should be sorted with the same `Ordering` before calling; otherwise, the results are undefined.
//		  * @param elem     the element to find.
//		  * @return a `Found` value containing the index corresponding to the element in the
//		  *         sequence, or the `InsertionPoint` where the element would be inserted if
//		  *         the element is not in the sequence.
//		  */
//		//consider: this is a duplication of IndexedSeqOps.search; is there a point, as we need to create an object anyway?
//		def search[A >: E :Ordering](elem :A) :SearchResult = {
//			val i = binarySearch(0, self.length, elem)
//			if (i >= 0) Found(i) else InsertionPoint(-i - 1)
//		}
//
//		/** Search the specified range of this array for a given element.
//		  * The array should be sorted with the same `Ordering` before calling; otherwise, the results are undefined.
//		  * @param from     the index of the first element in the searched range.
//		  * @param until    the index following the last element in the searched range.
//		  * @param elem     the element to find.
//		  * @return a `Found` value containing the index corresponding to the element in the
//		  *         sequence, or the `InsertionPoint` where the element would be inserted if
//		  *         the element is not in the sequence.
//		  */
//		@tailrec def search[A >: E :Ordering](elem :A, from :Int, until :Int) :SearchResult =
//			if (from <= 0) search(elem, 0, until)
//			else if (until > self.length) search(elem, from, self.length)
//			else if (from >= self.length | until <= from) InsertionPoint(from)
//			else {
//				val i = binarySearch(from, until, elem)
//				if (i >= 0) Found(i) else InsertionPoint(-i - 1)
//			}

	//moved to ArrayLikeExtension
//		/** Performs a binary search of element `x` in a section of this array, sorted according
//		  * to an implicit `Ordering[E]`. If the array is not sorted, or the `Ordering` is not consistent with `equals`,
//		  * then the behaviour
//		  * The differences from [[collection.IndexedSeqOps.search search]] from the standard library are:
//		  *   1. ability to provide bounds within which to search,
//		  *   1. returning always the index of the first occurrence of the value in case of duplicates
//		  *      (rather than the index of any of them),
//		  *   1. avoiding boxing of the array to `IndexedSeqOps` to make the call,
//		  *   1. returning the value as an `ElementIndex`, which does not box the result,
//		  *   1. switching to direct comparisons of built in value types if the `Ordering` is the default one.
//		  * @return index of the search key, if it is contained in the array,
//		  *         as `ElementIndex`.[[net.noresttherein.sugar.collections.ElementIndex.Present Present]].
//		  *         Otherwise, the ''insertion point'',
//		  *         as `ElementIndex.`[[net.noresttherein.sugar.collections.ElementIndex.Absent Absent]].
//		  *         The `insertion point` is defined as the point at which the key would be inserted into the array:
//		  *         the index of the first element in the array greater than the key, or `until`,
//		  *         if all elements in the array are less than the specified key.
//		  */
//		@inline final def binarySearch[U >: E :Ordering](x :U) :ElementIndex = binarySearch(0, self.length, x)
//
//		/** Performs a binary search of element `x` in a section of this array, sorted
//		  * according to an implicit `Ordering[U]`. Returns the index of the first occurrence of `x`, if present
//		  * in the given range, or an index `i`: `from <= i <= until`, such that
//		  * `i == until || array(i) > x && (i == from || array(i) < x)`. If `until <= from`,
//		  * then [[net.noresttherein.sugar.collections.ElementIndex.Absent Absent]]`(from)` is returned immediately.
//		  * The differences from [[collection.IndexedSeqOps.search search]] from the standard library are:
//		  *   1. ability to provide bounds within which to search,
//		  *   1. returning always the index of the first occurrence of the value in case of duplicates
//		  *      (rather than the index of any of them),
//		  *   1. avoiding boxing of the array to `IndexedSeqOps` to make the call,
//		  *   1. returning the value as an `ElementIndex`, which does not box the result,
//		  *   1. switching to direct comparisons of built in value types if the `Ordering` is the default one.
//		  * @return the index of the search key, if it is present in the searched range,
//		  *         as `ElementIndex`.[[net.noresttherein.sugar.collections.ElementIndex.Present Present]].
//		  *         Otherwise, the ''insertion point'',
//		  *         as `ElementIndex.`[[net.noresttherein.sugar.collections.ElementIndex.Absent Absent]].
//		  *         The `insertion point` is defined as the point at which the key would be inserted into the array:
//		  *         the index of the first element in the range greater than the key, or `until`,
//		  *         if all elements in the range are less than the specified key.
//		  *///binarySearch(from :Int, until :Int)(x :U) would be more elegant, but Scala 2 infers too early U =:= E
//		def binarySearch[U >: E](from :Int, until :Int, x :U)(implicit ordering :Ordering[U]) :ElementIndex = {
//			val length = self.length
//			val start  = math.min(length, math.max(from, 0))
//			val limit  = math.min(until, length)
//			val index  = (self :Array[_]) match {
//				case _ if limit <= start => -start - 1
//				case array :Array[AnyRef]  => //specialized for AnyRef - faster access than in Array[_]
//					BinarySearch(array, start, limit - 1, x.asAnyRef)(ordering.castParam[AnyRef])
//				case array :Array[Int]      if ordering == Ordering.Int =>
////					Arrays.binarySearch(array, start, limit, x.asInstanceOf[Int])
//					BinarySearch(array, start, limit - 1, x.asInstanceOf[Int])
//				case array :Array[Long]     if ordering == Ordering.Long =>
////					Arrays.binarySearch(array, start, limit, x.asInstanceOf[Long])
//					BinarySearch(array, start, limit - 1, x.asInstanceOf[Long])
//				case array :Array[Double]   if ordering == Ordering.Double.TotalOrdering =>
////					Arrays.binarySearch(array, start, limit, x.asInstanceOf[Double])
//					BinarySearch(array, start, limit - 1, x.asInstanceOf[Double])
//				case array :Array[Byte]     if ordering == Ordering.Byte =>
////					Arrays.binarySearch(array, start, limit, x.asInstanceOf[Byte])
//					BinarySearch(array, start, limit - 1, x.asInstanceOf[Byte])
//				case array :Array[Char]     if ordering == Ordering.Char =>
////					Arrays.binarySearch(array, start, limit, x.asInstanceOf[Char])
//					BinarySearch(array, start, limit - 1, x.asInstanceOf[Char])
//				case array :Array[Float]    if ordering == Ordering.Float.TotalOrdering =>
////					Arrays.binarySearch(array, start, limit, x.asInstanceOf[Float])
//					BinarySearch(array, start, limit - 1, x.asInstanceOf[Float])
//				case array :Array[Short]    if ordering == Ordering.Short =>
////					Arrays.binarySearch(array, start, limit, x.asInstanceOf[Short])
//					BinarySearch(array, start, limit - 1, x.asInstanceOf[Short])
//				case _ =>
//					BinarySearch(self, start, limit - 1, x)
//			}
//			new ElementIndex(index)
//		}

	//moved to MutableArrayExtension
//		@inline final def sortInPlace[U >: E]()(implicit ordering :Ordering[U]) :Unit =
//			Sorting.stableSort(self.asInstanceOf[Array[U]])
//
//		@throws[IndexOutOfBoundsException]("if either from or until are outside of [0, this.length) range.")
//		@inline final def sortInPlace[U >: E](from :Int, until :Int)(implicit ordering :Ordering[U]) :Unit =
//			Sorting.stableSort(self.asInstanceOf[Array[U]], from, until)
//
//		@inline final def sortInPlaceWith[U >: E](lt :(U, U) => Boolean) :Unit = Sorting.stableSort(self, lt)
//
//		@throws[IndexOutOfBoundsException]("if either from or until are outside of [0, this.length) range.")
//		@inline final def sortInPlaceWith[U >: E](from :Int, until :Int)(lt :(U, U) => Boolean) :Unit =
//			Sorting.stableSort(self, lt, from, until)
//
//		@inline final def sortInPlaceBy[A](f :E => A)(implicit ordering :Ordering[A]) :Unit =
//			Sorting.stableSort(self, (a :E, b :E) => ordering.lt(f(a), f(b)))
//
//		@throws[IndexOutOfBoundsException]("if either from or until are outside of [0, this.length) range.")
//		@inline final def sortInPlaceBy[A](from :Int, until :Int)(f :E => A)(implicit ordering :Ordering[A]) :Unit =
//			Sorting.stableSort(self, (a :E, b :E) => ordering.lt(f(a), f(b)), from, until)
//
//		//name reverse is already used by ArrayOps to produce a copy
//		/** Reverses the whole array in place. */
//		@inline def reverseInPlace() :Unit = reverseInPlace(0, self.length)
//
//		/** Reverses in place the section of this array between indices `from` (inclusive) and `until` (exclusive).
//		  * If `until <= from`, or `until <= 0`, or `from >= length` the call has no effect. Passing a negative `from`,
//		  * or `until > length` has the same effect as passing `0` and `length`, respectively.
//		  *///consider: strict indexing
//		def reverseInPlace(from :Int, until :Int) :Unit = {
//			var i = math.max(from, 0)
//			var j = math.min(until, self.length)
//			while (i < j) {
//				j -= 1
//				val boo = self(i)
//				self(i) = self(j)
//				self(j) = boo
//				i += 1
//			}
//		}
//
//		/** A new array of the same length, where every element pair at indices `(i, length - i - 1)` are swapped
//		  * in place.
//		  */
//		def reverse :Array[E] = {
//			val res = Array.of(self.getClass.getComponentType.castParam[E], self.length)
//			var i   = self.length
//			var j = 0
//			while (i > 0) {
//				i -= 1
//				res(j) = self(i)
//				j += 1
//			}
//			res
//		}

	//moved to ArrayLikeExtension
//		/** A new array of the same length, with the specified segment reversed.
//		  * All values at indices `[0, from)`, and `[until, length)` are the same as in this array.
//		  * Both arguments are clipped to the valid range before copying.
//		  */ //consider: strict indexing
//		def reverse(from :Int, until :Int) :Array[E] = {
//			val length = self.length
//			if (until <= from | until <= 0 | from >= length)
//				Array.copyOf(self, length)
//			else {
//				val from0 = math.max(from, 0)
//				val until0 = math.min(until, length)
//				val res = Array.of(self.getClass.getComponentType.castParam[E], length)
//				ArrayLike.copy(self, 0, res, 0, from0)
//				ArrayLike.copy(self, until0, res, until0, length - until0)
//				var i = from0; var j = until0 - 1
//				while (i <= j) {
//					res(i) = self(j)
//					res(j) = self(i)
//					i += 1; j -= 1
//				}
//				res
//			}
//		}

	//moved to MutableArrayExtension
//
//		/** Shifts in place all the elements in the array down by `n` positions, modulo the length of the array.
//		  * Element at position `n` is moved to index `0`, element at position `n + 1` to index `1`, etc.
//		  * Element at `0` is moved to index `n`, element at `1` to index `n + 1`, etc.
//		  */
//		@inline def rotateLeft(n :Int) :Unit = rotateLeft(0, self.length)(n)
//
//		/** Shifts in place the elements in the array between indices `from` (inclusive) and `until` (exclusive)
//		  * down by `n` positions, modulo the length of the index range.
//		  * Element at position `from + n` is moved to index `from`, element at position `from + n + 1`
//		  * to index `from + 1`, etc. Element at `from` is moved to index `from + n`, element at `from + 1`
//		  * to index `from + n + 1`, etc. If `until <= from`, or `until <= 0`, or `from >= length`,
//		  * the call has no effect. Passing negative `from` is the same as passing zero, passing `until > length`
//		  * clips it the length of the array.
//		  */ //consider: strict indexing
//		def rotateLeft(from :Int, until :Int)(n :Int) :Unit = {
//			val fullLength = self.length
//			val from0 = math.max(from, 0)
//			val until0 = math.min(until, fullLength)
//			val length = until0 - from0
//			if (until > 0 & until0 > from0) {
//				var pivot = n % length
//				if (pivot < 0)
//					pivot = length + pivot
//				pivot match { //try to use fast platform arraycopy if the shift is very short
//					case 0 =>
//					case 1 =>
//						val saved = self(from0)
//						arraycopy(self, from0 + 1, self, from0, length - 1)
//						self(until0 - 1) = saved
//					case 2 =>
//						val saved0 = self(from0)
//						val saved1 = self(from0 + 1)
//						arraycopy(self, from0 + 2, self, from0, length - 2)
//						self(until0 - 1) = saved1
//						self(until0 - 2) = saved0
//					case _ if pivot == length - 1 =>
//						val saved = self(until0 - 1)
//						arraycopy(self, from0, self, from0 + 1, length - 1)
//						self(from0) = saved
//					case _ if pivot == length - 2 =>
//						val saved1 = self(until0 - 1)
//						val saved0 = self(until0 - 2)
//						arraycopy(self, from0, self, from0 + 2, length - 2)
//						self(from0) = saved0
//						self(from0 + 1) = saved1
//					case _ =>
//						reverseInPlace(from0, from0 + pivot)
//						reverseInPlace(from0 + pivot, until0)
//						reverseInPlace(from0, until0)
//				}
//			}
//		}
//
//		/** Shifts in place all the elements in the array down by `n` positions, modulo the length of the array.
//		  * Element at position `n` is moved to index `0`, element at position `n + 1` to index `1`, etc.
//		  * Element at `0` is moved to index `n`, element at `1` to index `n + 1`, etc.
//		  */
//		@inline def rotateRight(n :Int) :Unit = rotateRight(0, self.length)(n)
//
//		/** Shifts in place the elements in the array between indices `from` (inclusive) and `until` (exclusive)
//		  * down by `n` positions, modulo the length of the index range.
//		  * Element at position `from + n` is moved to index `from`, element at position `from + n + 1`
//		  * to index `from + 1`, etc. Element at `from` is moved to index `from + n`, element at `from + 1`
//		  * to index `from + n + 1`, etc. If `until <= from`, or `until <= 0`, or `from >= length`,
//		  * the call has no effect. Passing negative `from` is the same as passing zero, passing `until > length`
//		  * clips it the length of the array.
//		  */
//		def rotateRight(from :Int, until :Int)(n :Int) :Unit = {
//			val length = self.length
//			val start = math.min(length, math.max(from, 0))
//			val end   = math.min(length, math.max(from, until))
//			if (start < end)
//				if (n < 0)
//					rotateLeft(from, until)(start - end - n) //guards against underflow on -(n == Int.MinValue)
//				else
//					rotateLeft(from, until)(end - start - n)
//		}


	//moved to ArrayLikeExtension
//		/** Creates a new array of the same length, consisting of `this.slice(n, length) ++ this.slice(0, n)`.
//		  * If `n` is not in `[0, length)` range, its non-negative remainder from the division by `length` is used.
//		  */
//		@inline def rotatedLeft(n :Int) :Array[E] = rotatedLeft(0, self.length)(n)
//
//		/** Creates a new array of the same length, with the specified range shifted cyclically right by `n`.
//		  * The bounds are clipped to `[0, length)` range, and passing `until <= from + 1` simply returns a copy
//		  * of this array.
//		  * If `n` is not in `[0, length)` range, its non-negative remainder from the division by `length` is used.
//		  * @return `this.slice(0, from0) ++ this.slice(n, until0) ++ this.slice(from0, from0 + n) ++ this.slice(until0, length)`,
//		  *         where `from0` and `until0` are `from` and `until` clipped to `[0, length)`.
//		  */
//		def rotatedLeft(from :Int, until :Int)(n :Int) :Array[E] = {
//			val length = self.length
//			val from0  = math.min(length, math.max(from, 0))
//			val until0 = math.min(until, length)
//			val range  = until0 - from0
//			if (until0 <= from0)
//				Array.copyOf(self, length)
//			else {
//				val pivot  = if (n >= 0) n % range else range + n % range
//				val res    = Array.like(self, length)
//				arraycopy(self, 0, res, 0, from0)
//				arraycopy(self, from0 + pivot, res, from0, range - pivot)
//				arraycopy(self, from0, res, until0 - pivot, pivot)
//				arraycopy(self, until0, res, until0, length - until0)
//				res
//			}
//		}
//
//		/** Creates a new array of the same length, consisting of
//		  * `this.slice(length - n, length) ++ this.slice(0, length - n)`.
//		  * If `n` is not in `[0, length)` range, its non-negative remainder from the division by `length` is used.
//		  */
//		@inline def rotatedRight(n :Int) :Array[E] = rotatedRight(0, self.length)(n)
//
//		/** Creates a new array of the same length, with the specified range shifted cyclically right by `n`.
//		  * The bounds are clipped to `[0, length)` range, and passing `until <= from + 1` simply returns a copy
//		  * of this array.
//		  * If `n` is not in `[0, length)` range, its non-negative remainder from the division by `length` is used.
//		  * @return `this.slice(0, from0) ++ this.slice(until0 - n, until0) ++ this.slice(from0, until0 - from0 - n) ++ this.slice(until0, length)`,
//		  *         where `from0` and `until0` are `from` and `until` clipped to `[0, length)`.
//		  */
//		def rotatedRight(from :Int, until :Int)(n :Int) :Array[E] = {
//			val length = self.length
//			val start  = math.min(length, math.max(from, 0))
//			val end    = math.min(length, math.max(from, until))
//			if (end <= start)
//				Array.copyOf(self, length)
//			else if (n < 0)
//				rotatedLeft(from, until)(start - end - n) //guards against underflow of -(n == Int.MinValue)
//			else
//				rotatedLeft(from, until)(end - start - n)
//		}

	//moved to MutableArrayExtension
//		/** Shifts the contents of this array left by `n` position. Range `[n, length)` is copied to `[0, length - n)`.
//		  * Range `[length - n, length)` remains unchanged. If `n >= this.length`, the call has no effect.
//		  * If `n` is negative, the result is equivalent to
//		  * [[net.noresttherein.sugar.collections.extensions.ArrayExtension.shiftRight shiftRight]]`(-n)`.
//		  */
//		def shiftLeft(n :Int) :Unit = {
//			val length = self.length
//			if (n < 0) {
//				if (n > Int.MinValue && -n < length)
//					arraycopy(self, 0, self, -n, length + n)
//			} else if (n < length)
//				arraycopy(self, n, self, 0, length - n)
//		}
//
//		/** Shifts the contents of this array right by `n` position. Range `[0, length - n)` is copied to `[n, length)`.
//		  * Range `[0, n)` remains unchanged. If `n >= this.length`, the call has no effect.
//		  * If `n` is negative, the result is equivalent to
//		  * [[net.noresttherein.sugar.collections.extensions.ArrayExtension.shiftLeft shiftLeft]]`(-n)`.
//		  */
//		def shiftRight(n :Int) :Unit = {
//			val length = self.length
//			if (n < 0) {
//				if (n > Int.MinValue && -n < length)
//					arraycopy(self, -n, self, 0, length + n)
//			} else if (n < length)
//				arraycopy(self, 0, self, n, length - n)
//		}
//
//		/** Shifts the contents from range `[from, until)` of this array left by `n` positions.
//		  * The value at `from` is moved to index `from - n`, value at `from + 1` to `from - n + 1`, and so on.
//		  * Any write which would happen outside the valid index range of the array is ignored;
//		  * the actual range moved is `[from max n min until, until)`. The contents from `[from max n, until)`
//		  * remain unchanged. If `n >= until`, the call has no effect. If `n` is negative, the result is equivalent to
//		  * [[net.noresttherein.sugar.collections.extensions.ArrayExtension.shiftRight shiftRight]]`(from, until)(-n)`.
//		  * @note this method may update elements in this array outside range `[from, until)`
//		  *       (depending on the signum of `n`).
//		  */ //consider: renaming to shiftLeftInPlace
////		@throws[IndexOutOfBoundsException]("if until < 0, until < from, or from > this.length.")
//		def shiftLeft(from :Int, until :Int)(n :Int) :Unit =
//			if (n < 0) {
//				if (n > Int.MinValue)
//					shiftRight(from, until)(-n)
//			} else if (n > 0) {
//				val length = self.length
////				if (until < 0 | until < from | from > length)
////					throw new IndexOutOfBoundsException(
////						errorString(self) + ".shiftLeft(" + from + ", " + until + ")(" + n + ")"
////					)
//				val start  = math.min(length, math.max(from, 0))
//				val end    = math.min(length, math.max(from, until))
//				if (n < end) {
//					val offset = math.max(start, n)
//					arraycopy(self, offset, self, offset - n, end - offset)
//				}
//			}
//
//		/** Shifts the contents from range `[from, until)` of this array right by `n` positions.
//		  * The value at `from` is moved to index `from + n`, value at `from + 1` to `from + n + 1`, and so on.
//		  * Any write which would happen outside the valid index range of the array is ignored;
//		  * the actual range moved is `[from max length - n min until, until)`.
//		  * The contents from `[from, from max until - n)` remain unchanged. If `n >= until`, the call has no effect.
//		  * If `n` is negative, the result is equivalent to
//		  * [[net.noresttherein.sugar.collections.extensions.ArrayExtension.shiftLeft shiftLeft]]`(from, until)(-n)`.
//		  * @note this method may update elements in this array outside range `[from, until)`
//		  *       (depending on the signum of `n`).
//		  */ //consider: renaming to shiftRightInPlace
////		@throws[IndexOutOfBoundsException]("if until < 0, until < from, or from > this.length.")
//		def shiftRight(from :Int, until :Int)(n :Int) :Unit =
//			if (n < 0) {
//				if (n > Int.MinValue)
//					shiftLeft(from, until)(-n)
//			} else if (n > 0) {
//				val length = self.length
////				if (until < 0 | until < from | from > length)
////					throw new IndexOutOfBoundsException(
////						errorString(self) + ".shiftRight(" + from + ", " + until + ")(" + n + ")"
////					)
//				val start  = math.min(length, math.max(from, 0))
//				val end    = math.min(length - n, math.max(start, until))
//				if (n < length - start) {
//					arraycopy(self, start, self, start + n, end - start)
//				}
//			}

//		/** Returns a new array, containing contents of this array shift left by `n` positions.
//		  * Values from the range `[n, length)` are copied to range `[0, length - n)`.
//		  * Values at `[length - n, length)` will be equal the default value for the element type (zero/null).
//		  * If `n` is negative, the result is equivalent to
//		  * `this `[[net.noresttherein.sugar.collections.extensions.ArrayExtension.shiftedRight shiftedRight]]` - n`.
//		  * If `n >= length`, the call simply creates an uninitialized array of the same length.
//		  * @see [[net.noresttherein.sugar.collections.extensions.ArrayExtension.<<]]
//		  */
//		def shiftedLeft(n :Int) :Array[E] = {
//			val length = self.length
//			if (n < 0)
//				if (n <= -length) Array.like(self, length)
//				else Array.copyOfRange(self, 0, length + n, -n, length)
//			else
//				if (n >= length) Array.like(self, length)
//				else Array.copyOfRange(self, n, length, 0, length)
//		}
//
//		/** Returns a new array, containing contents of this array shift left by `n` positions.
//		  * Values from the range `[n, length)` are copied to range `[0, length - n)`.
//		  * Values at `[length - n, length)` will be equal the default value for the element type (zero/null).
//		  * If `n` is negative, the result is equivalent to
//		  * `this `[[net.noresttherein.sugar.collections.extensions.ArrayExtension.shiftedLeft shiftedLeft]]` - n`.
//		  * If `n >= length`, the call simply creates an uninitialized array of the same length.
//		  * @see [[net.noresttherein.sugar.collections.extensions.ArrayExtension.>>>]]
//		  */
//		def shiftedRight(n :Int) :Array[E] = {
//			val length = self.length
//			if (n < 0)
//				if (n <= -length) Array.like(self, length)
//				else Array.copyOfRange(self, -n, length, 0, length)
//			else
//				if (n >= length) Array.like(self, length)
//				else Array.copyOfRange(self, 0, length - n, n, length)
//		}


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

	//moved to MutableArrayExtension
//		/** Fills (in place) the whole array with the given value. */
//		@inline def fill(elem :E) :Unit = fill(0, self.length)(elem)
//
//		/** Sets (in place) all values in this array within the given index range to the specified value. */
//		//consider: a different name, so we can define fill accepting => E, as in IterableFactory
////		@throws[IndexOutOfBoundsException]("if either from or until are outside of range [0, this.length)")
//		def fill(from :Int, until :Int)(elem :E) :Unit = {
//			if (until > from && until > 0 && from < self.length) {
//				val from0 = math.max(from, 0)
//				val until0 = math.min(until, self.length)
////				val length = self.length
////				if (from < 0 | until < 0 | from > length | until > length)
////					throw new IndexOutOfBoundsException(
////						errorString(self) + "fill(" + from + ", " + until + ")"
////					)
//				(self :Array[_]) match {
//					case array :Array[AnyRef]  => Arrays.fill(array, from0, until0, elem)
//					case array :Array[Int]     => Arrays.fill(array, from0, until0, elem.asInstanceOf[Int])
//					case array :Array[Long]    => Arrays.fill(array, from0, until0, elem.asInstanceOf[Long])
//					case array :Array[Double]  => Arrays.fill(array, from0, until0, elem.asInstanceOf[Double])
//					case array :Array[Byte]    => Arrays.fill(array, from0, until0, elem.asInstanceOf[Byte])
//					case array :Array[Char]    => Arrays.fill(array, from0, until0, elem.asInstanceOf[Char])
//					case array :Array[Float]   => Arrays.fill(array, from0, until0, elem.asInstanceOf[Float])
//					case array :Array[Short]   => Arrays.fill(array, from0, until0, elem.asInstanceOf[Short])
//					case array :Array[Boolean] => Arrays.fill(array, from0, until0, elem.asInstanceOf[Boolean])
//					case _ =>
//						var i = from0
//						while (i < until0) {
//							self(i) = elem
//							i += 1
//						}
//				}
//			}
//		}
//
//		/** Sets (in place) all elements in the array to `null`/`0`/`false`, depending on its type. */
//		@inline def clear() :Unit = clear(0, self.length)
//
//		//consider: strict index validation, as per most mutable methods
//		/** Clears the specified index range, setting all elements to `null`/`0`/`false`. Returns this modified array. */
//		def clear(from :Int, until :Int) :Unit =
//			if (until > from & until > 0 && from < self.length) {
//				val from0 = math.max(0, from)
//				val until0 = math.min(self.length, until)
//				(self :Array[_]) match {
//					case _     :Array[Unit]    => Arrays.fill(self.asInstanceOf[Array[AnyRef]], ())
//					case array :Array[AnyRef]  => Arrays.fill(array, from0, until0, null)
//					case array :Array[Int]     => Arrays.fill(array, from0, until0, 0)
//					case array :Array[Long]    => Arrays.fill(array, from0, until0, 0L)
//					case array :Array[Double]  => Arrays.fill(array, from0, until0, 0.0)
//					case array :Array[Byte]    => Arrays.fill(array, from0, until0, 0.toByte)
//					case array :Array[Char]    => Arrays.fill(array, from0, until0, 0.toChar)
//					case array :Array[Float]   => Arrays.fill(array, from0, until0, 0.0f)
//					case array :Array[Short]   => Arrays.fill(array, from0, until0, 0.toShort)
//					case array :Array[Boolean] => Arrays.fill(array, from0, until0, false)
//				}
//			}
//
//		/** Sets all values in this array to `null`, but only if this is an instance of `Array[AnyRef]` (or its subclass). */
//		@inline def clearIfRef() :Unit = clearIfRef(0, self.length)
//
//		/** Sets all values in the specified range of this array to `null`,
//		  * but only if this is an instance of `Array[AnyRef]` (or its subclass).
//		  * Indices out of `[0, this.length)` are permitted, and the method will not attempt to set them.
//		  */
//		@inline def clearIfRef(from :Int, until :Int) :Unit = (self :Array[_]) match {
//			case _     :Array[Unit] => ()
//			case array :Array[AnyRef] if until > from & until > 0 && from < self.length =>
//				val from0 = math.max(0, from)
//				val until0 = math.min(self.length, until)
//				Arrays.fill(array, from0, until0, null)
//			case _ =>
//		}

	//moved to MutableArrayExtension
//		/** Swaps (in place) elements at indices `i` and `j`. */
//		@inline def swap(i :Int, j :Int) :Unit = {
//			val boo = self(i)
//			self(i) = self(j)
//			self(j) = boo
//		}
	//moved to ArrayLikeExtension
//		/** A copy of this array with values at indices `i` and `j` swapped. */
//		@inline def swapped(i :Int, j :Int) :Array[E] = {
//			val res = Array.copyOf(self, self.length)
//			val boo = res(i)
//			res(i) = res(j)
//			res(j) = boo
//			res
//		}

		//todo: make these methods available for anything with SeqLike type class
		/** Replaces the first occurrence of sequence `pattern` in this array with elements from `patch`.
		  * Returns a new array of element type specified by the implicit `ClassTag[U]`.
		  */
		@inline def replace[U >: E :ClassTag](pattern :collection.Seq[U], patch :collection.Seq[U]) :Array[U] =
			replace(pattern, patch, 0)

		/** Replaces the first occurrence of sequence `pattern` in this array at or after index `from`
		  * with elements from `patch`. Returns a new array of element type specified by the implicit `ClassTag[U]`.
		  */
		def replace[U >: E :ClassTag](pattern :collection.Seq[U], patch :collection.Seq[U], from :Int) :Array[U] = {
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

		/** Replaces the first occurrence of sequence `pattern` in this array with elements from `patch`. */
		@inline def replace(pattern :collection.Seq[E], patch :collection.Seq[E]) :Array[E] =
			replace(pattern, patch, 0)

		/** Replaces the first occurrence of sequence `pattern` in this array at or after index `from`
		  * with elements from `patch`.
		  */
		def replace(pattern :collection.Seq[E], patch :collection.Seq[E], from :Int) :Array[E] = {
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
		@inline def replace[U >: E :ClassTag](pattern :ArrayLike[U], patch :ArrayLike[U]) :Array[U] =
			replace(pattern, patch, 0)

		/** Replaces the first occurrence of sequence `pattern` in this array at or after index `from`
		  * with elements from `patch`. Returns a new array of element type specified by the implicit `ClassTag[U]`.
		  */ //todo: instead of wrapping, create a shared implementation based on SeqLike type class
		@inline def replace[U >: E :ClassTag](pattern :ArrayLike[U], patch :ArrayLike[U], from :Int) :Array[U] =
			replace(ArrayLike.Wrapped(pattern), ArrayLike.Wrapped(patch), from)

		/** Replaces the first occurrence of sequence `pattern` in this array with elements from `patch`. */
		@inline def replace(pattern :ArrayLike[E], patch :ArrayLike[E]) :Array[E] =
			replace(pattern, patch, 0)

		/** Replaces the first occurrence of sequence `pattern` in this array at or after index `from`
		  * with elements from `patch`.
		  */
		@inline def replace(pattern :ArrayLike[E], patch :ArrayLike[E], from :Int) :Array[E] =
			replace(ArrayLike.Wrapped(pattern), ArrayLike.Wrapped(patch), from)

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


		/** Sets all elements in the array to values returned by the argument function applied to element's index. */
		@inline def updateAll(f :Int => E) :Unit = updateAll(0, self.length)(f)

		/** Sets all elements in the given index range in this array to values returned by the argument function
		  * applied to element's index. If `until < from` the call has no effect.
		  */
		@throws[IndexOutOfBoundsException]("if either from or until is outside range [0, this.length).")
		def updateAll(from :Int, until :Int)(f :Int => E) :Unit = ArrayLikeOps.updateAll(self, from, until)(f)
/*
	{
		if (from < until) {
			val length = self.length
			if (until < 0 | until > length | from < 0 | from > length)
				throw new IndexOutOfBoundsException(
					errorString(self) + ".updateAll(" + from + ", " + until + ")(" + f + ")"
				)
			var i = from
			(self :Array[_]) match {
				case array :Array[AnyRef]   =>
					def updateRef(f :Int => AnyRef) :Unit =
						while (i < until) {
							array(i) = f(i); i += 1
						}
					updateRef(f.asInstanceOf[Int => AnyRef])
				case array :Array[Int]      =>
					def updateInt(f :Int => Int) :Unit =
						while(i < until) {
							array(i) = f(i); i += 1
						}
					updateInt(f.asInstanceOf[Int => Int])
				case array :Array[Long]     =>
					def updateLong(f :Int => Long) :Unit =
						while(i < until) {
							array(i) = f(i); i += 1
						}
					updateLong(f.asInstanceOf[Int => Long])
				case array :Array[Double]   =>
					def updateDouble(f :Int => Double) :Unit =
						while(i < until) {
							array(i) = f(i); i += 1
						}
					updateDouble(f.asInstanceOf[Int => Double])
				case array :Array[Char]     =>
					def updateChar(f :Int => Char) :Unit =
						while(i < until) {
							array(i) = f(i); i += 1
						}
					updateChar(f.asInstanceOf[Int => Char])
				case array :Array[Byte]     =>
					def updateByte(f :Int => Byte) :Unit =
						while(i < until) {
							array(i) = f(i); i += 1
						}
					updateByte(f.asInstanceOf[Int => Byte])
				case array :Array[Float]    =>
					def updateFloat(f :Int => Float) :Unit =
						while(i < until) {
							array(i) = f(i); i += 1
						}
					updateFloat(f.asInstanceOf[Int => Float])
				case array :Array[Short]    =>
					def updateShort(f :Int => Short) :Unit =
						while(i < until) {
							array(i) = f(i); i += 1
						}
					updateShort(f.asInstanceOf[Int => Short])
				case array :Array[Boolean]  =>
					def updateBoolean(f :Int => Boolean) :Unit =
						while(i < until) {
							array(i) = f(i); i += 1
						}
					updateBoolean(f.asInstanceOf[Int => Boolean])
			}
		}
	}
*/

//		  * If any of the indices in the range covering all provided elements
//		  * is out of range, it is simply ignored. For example,
//		  * {{{
//		  *     > Array("You", "Boo", "I").updateAll(-1, "Imoen", "CHARNAME", "Miniature Giant Space Hamster")
//		  *     > Array[String]("CHARNAME", "Miniature Giant Space Hamster", "I")
//		  * }}}
		/** Sets the values at indices `index, index + 1, index + 2, ...` to `first, second, elems.head`
		  * and subsequent elements of `rest`.
		  * @return the number of updated elements.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + 2 + rest.length > this.length")
		def updateAll(index :Int, first :E, second :E, rest :E*) :Int = {
			self(index) = first
			self(index  + 1) = second
			updateAll(index + 2, rest) + 2
		}
//		{
//			var copied = 0
//			if (index < self.length) {
//				if (index >= 0) {
//					self(index) = first
//					copied = 1
//				}
//				if (index < self.length - 1) {
//					if (index >= -1) {
//						self(index + 1) = second
//						copied += 1
//					}
//					copied += updateAll(index + 2, rest)
//				}
//			}
//			copied
//		}

//		  * If any of the indices in the range covering all provided elements
//		  * is out of range, it is simply ignored. For example,
//		  * {{{
//		  *     > Array("You", "Boo", "I").updateAll(-1, Seq("Imoen", "CHARNAME", "Miniature Giant Space Hamster"))
//		  *     > Array[String]("CHARNAME", "Miniature Giant Space Hamster", "I")
//		  * }}}
		/** Sets the values at indices `index, index + 1, ...` to subsequent elements of `elems`.
		  * @return the number of updated elements.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.size > this.length")
		def updateAll(index :Int, elems :IterableOnce[E]) :Int = {
			val thisSize = self.length
			val thatSize = elems.knownSize
			if (index < 0 | index > thisSize | thatSize >= 0 & index > thisSize - thatSize)
				throw new IndexOutOfBoundsException(
					errorString(self) + ".updateAll(" + index + ", " + errorString(elems) + ")"
				)
			if (thatSize >= 0)
				elems.toBasicOps.copyToArray(self, index)
			else
				elems.toBasicOps.foldLeft(index) { (i, elem) =>
					self(i) = elem; i + 1
				} - index
		}

//		  * If any of the indices in the range covering all provided elements
//		  * is out of range, it is simply ignored. For example,
//		  * {{{
//		  *     > Array("You", "Boo", "I").updateAll(-1, IArray("Imoen", "CHARNAME", "Miniature Giant Space Hamster"))
//		  *     > Array[String]("CHARNAME", "Miniature Giant Space Hamster", "I")
//		  * }}}
		/** Sets the values at indices `index, index + 1, ...` to subsequent elements of `elems`.
		  * @return the number of updated elements.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.length > this.length")
		def updateAll(index :Int, elems :ArrayLike[E]) :Int = {
			val thisSize = self.length
			val thatSize = elems.length
			if (index < 0 | index > thisSize - thatSize)
				throw new IndexOutOfBoundsException(
					errorString(self) + ".updateAll(" + index + ", " + errorString(elems) + ")"
				)
			ArrayLike.copy(elems, 0, self, index, thatSize)
			thatSize
		}

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
		@throws[IndexOutOfBoundsException]("if index < 0 o index + 2 + rest.size > this.length")
		def updatedAll[U >: E :ClassTag](index :Int, first :U, second :U, rest :U*) :Array[U] = {
	/*
			val length = self.length
			val res = new Array[U](length)
			if (index >= 0) {
				if (index >= length)
					ArrayLike.copy(self, 0, res, 0, length)
				else {
					ArrayLike.copy(self, 0, res, 0, index)
					res(index) = first
					if (index < length - 1) {
						res(index + 1) = second
						if (index < length - 2) {
							val copied = rest.copyToArray(res, index + 2)
							val end = index + 2 + copied
							ArrayLike.copy(self, end, res, end, length - end)
						}
					}
				}
			} else {
				val drop = -(index + 1) //avoids -Int.MinValue
				if (index == -1)
					res(0) = second
				val thatSize = rest match {
					case ErasedArray.Wrapped.Slice(array, from, until) =>
						val copied = math.max(0, math.min(until - from - drop, length))
						ArrayLike.copy(array, from + drop, res, 0, copied)
						copied
					case _ if hasFastDrop(rest) =>
						rest.drop(drop).copyToArray(res, index + 1)
					case _ =>
						rest.iterator.drop(drop).copyToArray(res, index + 1)
				}
				ArrayLike.copy(self, thatSize, res, thatSize, length - thatSize)
			}
			res
	*/
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
		/** Updates consecutive elements of this array, starting with the specified index,
		  * with elements from the collection.
		  * If `index >= 0` and `index + elems.size <= length`, this is equivalent to
		  * [[collection.ArrayOps.patch patch]]`(index, elems, elems.size)`, but more efficient
		  * due to a single array allocation.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.size > this.length")
		def updatedAll[U >: E :ClassTag](index :Int, elems :IterableOnce[U]) :Array[U] = {
	/*
			val thisSize = self.length
			val res = new Array[U](thisSize)
			if (index >= thisSize || index == Int.MinValue) {
				ArrayLike.copy(self, 0, res, 0, thisSize)
			} else if (index >= 0) {
				ArrayLike.copy(self, 0, res, 0, index)
				val end = index + elems.toIterableOnceOps.copyToArray(res, index)
				ArrayLike.copy(self, end, res, end, thisSize - end)
			} else fastDrop(elems, -index) match {
				case Got(dropped) =>
					val copied = dropped.toIterableOnceOps.copyToArray(res, 0)
					ArrayLike.copy(self, copied, res, copied, thisSize - copied)
				case _ =>
					val copied = elems.iterator.drop(-index).copyToArray(res, 0)
					ArrayLike.copy(self, copied, res, copied, thisSize - copied)
			}
			res
	*/
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

		/** Updates consecutive elements of this array, starting with the specified index,
		  * with elements from the collection.
		  * If `index >= 0` and `index + elems.size <= length`, this is equivalent to
		  * [[collection.ArrayOps.patch patch]]`(index, elems, elems.size)`, but more efficient
		  * due to a single array allocation.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.size > this.length")
		def updatedAll(index :Int, elems :IterableOnce[E]) :Array[E] =
			updatedAll[E](index, elems)(getComponentClassTag)

	//		  * Passing `index >= self.length` simply returns a copy of this array, while passing `index < 0`
	//		  * starts overwriting at `0`, but ignores the first `-index` values.
		/** Updates consecutive elements of this array, starting with the specified index,
		  * with elements from the collection.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.length > this.length")
		def updatedAll[U >: E :ClassTag](index :Int, elems :ArrayLike[U]) :Array[U] = {
	/*
			val length = self.length
			val elemsLength = elems.length
			if (index >= length | index <= -elemsLength)
				Array.copyAs[U](self, length)
			else if (index <= 0) {
				val res = new Array[U](length)
				val offset = elems.copyRangeToArray(res, -index, elemsLength)
				ArrayLike.copy(self, offset, res, offset, length - offset)
				res
			} else {
				val res = new Array[U](length)
				ArrayLike.copy(self, 0, res, 0, index)
				val offset = index + elems.copyToArray(res, index)
				ArrayLike.copy(self, offset, res, offset, length - offset)
				res
			}
	*/
			val length     = self.length
			val thatLength = elems.length
			if (index < 0 | index > length - thatLength)
				throw new IndexOutOfBoundsException(
					errorString(self) + ".updatedAll[" + fullNameOf[U] + "](" + index + ", " + errorString(elems) + ")"
				)
			Array.copyOfRanges(self, 0, index, elems, 0, thatLength, self, index + thatLength, length)
	/*
			val thisElemType   = self.getClass.getComponentType
			val resElemType    = classTag[U].runtimeClass
			val copiesFastThis = resElemType.isAssignableFrom(thisElemType)
			if (copiesFastThis && thatLength <= (length >> 2)) {
				val res = ArrayAsSeq.copyOf[U](self, length)
				ArrayLike.copy(elems, 0, res, index, thatLength)
				res
			} else {
				val res = new Array[U](length)
				ArrayLike.copy(self, 0, res, 0, index)
				ArrayLike.copy(elems, 0, res, index, thatLength)
				ArrayLike.copy(self, index + thatLength, res, index + thatLength, length - index - thatLength)
				res
			}
	*/
		}

		//erasure clash with the following method
		/** Updates consecutive elements of this array, starting with the specified index,
		  * with elements from the collection.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.length > this.length")
		def updatedAll(index :Int, elems :ArrayLike[E]) :Array[E] =
			updatedAll[E](index, elems)(getComponentClassTag)
	//		def updatedAll(index :Int, elems :ArrayLike[E], from :Int = 0, until :Int = Int.MaxValue) :Array[E] =
	//			if (from <= 0 && until >= elems.length)
	//				updatedAll[E](index, elems)(ClassTag[E](self.getClass.getComponentType))
	//			else
	//				updatedAll[E](index, ArrayLike.Wrapped.Slice(elems, from, until))(ClassTag[E](elems.getClass.getComponentType))

	//		  * Passing `index >= self.length` simply returns a copy of this array, while passing `index < 0`
	//		  * starts overwriting at `0`, but ignores the first `-index` values.
		/** Updates consecutive elements of this array, starting with the specified index,
		  * with elements from the collection.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.length > this.length")
	/*
		def updatedAll[U >: E](index :Int, elems :Array[U], from :Int = 0, until :Int = Int.MaxValue) :Array[U] = {
			val length = self.length
			val from0 = math.max(from, 0)
			val until0 = math.max(from, math.min(until, elems.length))
			if (index < 0 || index + until0 - from0 > length)
				throw new IndexOutOfBoundsException(
					errorString(self) + ".updateAll(" + index + ", " + errorString(elems) + ", " + from + ", " + until + ")"
				)
			Array.copyOfRanges(self :ArrayLike[U], 0, index, elems, from, until, self, index + until0 - from0, length)(
				ClassTag[U](elems.getClass.getComponentType)
			)
		}
	*/
		def updatedAll[U >: E](index :Int, elems :Array[U])(implicit __ :Ignored) :Array[U] =
			updatedAll(index, elems :ArrayLike[U])(ClassTag[U](elems.getClass.getComponentType))

	//			if (index >= self.length) 0
	//			else if (index >= 0) elems.toIterableOnceOps.copyToArray(self, index)
	//			else elems match {
	//				case _ if index == Int.MinValue => 0
	//				case items :Iterable[E] if hasFastDrop(items) => items.drop(-index).copyToArray(self)
	//				case _ => elems.iterator.drop(-index).copyToArray(self)
	//			}
	/*
			elems match {
				case _ if index >= self.length => 0
				case _ if index >= 0 =>
					elems.toIterableOnceOps.copyToArray(self, index)
				case ErasedArray.Wrapped.Slice(array, from, until) =>
					val idx = math.max(index, 0)
					val suffixLen = until - from - (idx - index)
					val copied = math.max(0, math.min(self.length - idx, suffixLen))
					ArrayLike.copy(array, from, self, idx, copied)
					copied
				case IndexedIterable(seq) if IndexedIterable.applyPreferred(seq) =>
					val offset = math.max(index, 0)
					var i = offset - index
					val end = math.min(seq.length, self.length.toLong - index).toInt
					val copied = math.max(0, end - i)
					while (i < end) {
						self(offset + i) = seq(i)
						i += 1
					}
					copied
				case seq :collection.LinearSeq[E] =>
					val length = self.length
					@tailrec def update(i :Int, list :collection.LinearSeq[E], copied :Int) :Int =
						if (i < length && list.nonEmpty) {
							self(i) = list.head
							update(i + 1, list.tail, copied + 1)
						} else
							copied
					val idx = math.max(index, 0)
					update(idx, seq.drop(idx - index), 0)
				case _ if index >= 0 =>
					elems.toIterableOnceOps.copyToArray(self, index, self.length)
				case _ =>
					elems.iterator.copyToArray(self, 0)
			}
	*/

//	def updatedAll2[U >: E, A[X] <: ArrayLike[X]]
//	               (index :Int, elems :A[U])(implicit elementCompatibility :ArrayCompatibility[E, U, A]) :Array[U] =
//		elementCompatibility match {
//			case _ :ClassTag[U @unchecked] => ???
//			case _ if elementCompatibility == ArrayLike.sameElementTypeArrayLike => ???
//			case _ if elementCompatibility == ArrayLike.elementSuperTypeArray => ???
//			case _ => throw new IllegalArgumentException(
//				"Unknown array compatibility witness: " + elementCompatibility + ": " + elementCompatibility.className + "."
//			)
//		}

		//moved to ArrayLikeExtension
	//		/** An array consisting of all elements of this array preceding the element at index, followed
	//		  * by all elements at positions `index + 1` and greater.
	//		  */
	//		@throws[IndexOutOfBoundsException]("if index < 0 or index >= length")
	//		def removed(index :Int) :Array[E] = {
	////			if (index < 0 | index >= self.length)
	////				Array.copyOf(self, self.length)  //if we allowed indices out of range
	////			else {
	//			val length = self.length
	//			if (index < 0 | index >= length)
	//				throw new IndexOutOfBoundsException(index.toString + " out of " + length)
	//			Array.copyOfRanges(self, 0, index, self, index + 1, self.length)
	////			val res = Array.of(self.getClass.getComponentType.castParam[E], length - 1)
	////			arraycopy(self, 0, res, 0, index)
	////			arraycopy(self, index + 1, res, index, length - index - 1)
	////			res
	//		}
	//
	//		/** Removes a slice from this array, copying all remaining elements to a new array of the same element type.
	//		  * @return `take(from) ++ drop(until)`, but in one step.
	//		  */
	//		def removed(from :Int, until :Int) :Array[E] =
	//			if (until <= from | until <= 0 || from > self.length)
	//				ArrayAsSeq.copyOf(self)
	//			else
	//				Array.copyOfRanges(self, 0, from, self, until, self.length)
	/*
		{
			val length = self.length
			if (until <= 0 | until <= from || from >= length)
				Array.copyOf(self, length)
			else if (from <= 0 & until >= length)
				ArrayAsSeq.empty(self.getClass.getComponentType.castParam[E])
			else {
				val validFrom  = math.max(from, 0)
				val validUntil = math.min(until, length)
				val res = Array.of(self.getClass.getComponentType.castParam[E], length - (validUntil - validFrom))
				arraycopy(self, 0, res, 0, validFrom)
				arraycopy(self, validUntil, res, validFrom, length - validUntil)
				res
			}
		}
	*/

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
				throw new IndexOutOfBoundsException(
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
				throw new IndexOutOfBoundsException(
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
		def insertedAll(index :Int, elems :IterableOnce[E]) :Array[E] =
			insertedAll[E](index, elems)(ClassTag[E](self.getClass.getComponentType))

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * It is similar [[collection.ArrayOps.patch patch]]`(index, elems, 0)`,
		  * but requires the index to be in `[0, length]` range.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		def insertedAll[U >: E :ClassTag](index :Int, elems :ArrayLike[U]) :Array[U] =
			if (index < 0 || index > self.length)
				throw new IndexOutOfBoundsException(
					s"${self.className}|${self.length}|.insertedAll($index, ${elems.className})"
				)
			else
				Array.copyOfRanges(self, 0, index, elems, 0, elems.length, self, index, self.length)
	//				patch(index, elems, 0)

		//erasure clash with the following method
		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * It is similar [[collection.ArrayOps.patch patch]]`(index, elems, 0)`,
		  * but requires the index to be in `[0, length]` range.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
	//		def insertedAll(index :Int, elems :ArrayLike[E], from :Int = 0, until :Int = Int.MaxValue) :Array[E] =
		def insertedAll(index :Int, elems :ArrayLike[E]) :Array[E] =
			insertedAll[E](index, elems)(ClassTag[E](self.getClass.getComponentType))
	//			Array.copyOfRanges(self, 0, index, elems, 0, elems.length, self, index, self.length)(
	//				ClassTag[E](self.getClass.getComponentType)
	//			)

	//			if (from <= 0 && until >= elems.length)
	//				insertedAll[E](index, elems)(ClassTag[E](self.getClass.getComponentType))
	//			else
	//				insertedAll[E](index, ArrayLike.Wrapped.Slice(elems, from, until))(ClassTag[E](self.getClass.getComponentType))

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * It is similar [[collection.ArrayOps.patch patch]]`(index, elems, 0)`,
		  * but requires the index to be in `[0, length]` range.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		def insertedAll[U >: E](index :Int, elems :Array[U])(implicit __ :Ignored) :Array[U] =
			insertedAll(index, elems :ArrayLike[U])(ClassTag[U](elems.getClass.getComponentType))
	/*
		def insertedAll[U >: E](index :Int, elems :Array[U], from :Int = 0, until :Int = Int.MaxValue) :Array[U] = {
			val length     = self.length
	//			val thatLength = elems.length
			if (index < 0 || index > length)
				throw new IndexOutOfBoundsException(
					errorString(self) + ".insertedAll(" + index + ", " + errorString(elems) + ")"
				)
			Array.copyOfRanges(self :ArrayLike[U], 0, index, elems, from, until, self :ArrayLike[U], index, length)(
				ClassTag[U](elems.getClass.getComponentType)
			)
	//			else {
	//				val res = Array.like(elems, length + thatLength)
	//				if (index > 0)
	//					ArrayLike.copy(self, 0, res, 0, index)
	//				arraycopy(elems, 0, res, index, thatLength)
	//				if (index < length)
	//					ArrayLike.copy(self, index, res, index + thatLength, length - index)
	//				res
	//			}
		}
	*/

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

		/** Same as [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.appendedAll appendedAll]],
		  * but does not requie a `ClassTag` and accepts only values of this array's element type.
		  */
		def appendedAll(elems :IterableOnce[E]) :Array[E] =
			appendedAll[E](elems)(ClassTag[E](self.getClass.getComponentType))

		/** An overloaded variant of standard
		  * [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.appendedAll appendedAll]], appending the elements
		  * of the specified array.
		  */
		def appendedAll[U >: E :ClassTag](elems :ArrayLike[U]) :Array[U] =
			Array.copyOfRanges(self, 0, self.length, elems, 0, elems.length)
	//		{
	//			val thisLength = self.length
	//			val thatLength = elems.length
	//			val res = new Array[A](thisLength + thatLength)
	//			ArrayLike.copy(self, 0, res, 0, thisLength)
	//			ArrayLike.copy(elems, 0, res, thisLength, thatLength)
	//			res
	//		}

		/** An overloaded, specific variant of standard
		  * [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.appendedAll appendedAll]], appending the elements
		  * of the specified array.
		  */
		def appendedAll(elems :ArrayLike[E]) :Array[E] =
			Array.copyOfRanges(self, 0, self.length, elems, 0, elems.length)(
				ClassTag[E](self.getClass.getComponentType)
			)

		/** An overloaded variant of standard
		  * [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.appendedAll appendedAll]], appending the elements
		  * of the specified array. The returned array will be of the same type as the argument.
		  */
		def appendedAll[U >: E](elems :Array[U])(implicit __ :Ignored) :Array[U] =
			appendedAll(elems :ArrayLike[U])(ClassTag[U](elems.getClass.getComponentType))
	/*
		def appendedAll[U >: E](elems :Array[U], from :Int = 0, until :Int = Int.MaxValue) :Array[U] =
			Array.copyOfRanges[U](self, 0, self.length, elems, from, until)(
				ClassTag[U](elems.getClass.getComponentType)
			)
	*/
	//		{
	//			val thisLength = self.length
	//			val thatLength = elems.length
	//			val res = Array.like(elems, thisLength + thatLength)
	//			ArrayLike.copy(self, 0, res, 0, thisLength)
	//			ArrayLike.copy(elems, 0, res, self.length, thatLength)
	//			res
	//		}

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
		  * but does not requie a `ClassTag` and accepts only values of this array's element type.
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

		/** Same as [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.prependedAll prependedAll]],
		  * but does not requires a `ClassTag` and accepts only values of this array's element type.
		  */
		def prependedAll(elems :IterableOnce[E]) :Array[E] =
			prependedAll[E](elems)(ClassTag[E](self.getClass.getComponentType))

		/** An overloaded variant of standard
		  * [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.prependedAll prependedAll]],
		  * prepending the elements of the specified array.
		  */
		def prependedAll[U >: E :ClassTag](elems :ArrayLike[U]) :Array[U] =
			Array.copyOfRanges(elems, 0, elems.length, self, 0, self.length)
	//		{
	//			val res = new Array[A](self.length + elems.length)
	//			ArrayLike.copy(elems, 0, res, 0, elems.length)
	//			ArrayLike.copy(self, 0, res, elems.length, self.length)
	//			res
	//		}

		/** An overloaded variant of standard
		  * [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.prependedAll prependedAll]],
		  * prepending the elements of the specified array.
		  */
		def prependedAll(elems :ArrayLike[E]) :Array[E] =
			Array.copyOfRanges(elems, 0, elems.length, self, 0, self.length)(
				ClassTag[E](self.getClass.getComponentType)
			)

		/** An overloaded variant of standard
		  * [[collection.ArrayOps ArrayOps]]`.`[[collection.ArrayOps.prependedAll prependedAll]],
		  * prepending the elements of the specified array. The returned array will be of the same type as the argument.
		  */
		def prependedAll[U >: E](elems :Array[U])(implicit __ :Ignored) :Array[U] =
			prependedAll(elems :ArrayLike[U])(ClassTag[U](elems.getClass.getComponentType))

		/*
				def prependedAll[U >: E](elems :Array[U], from :Int = 0, until :Int = Int.MaxValue) :Array[U] =
					Array.copyOfRanges(elems :ArrayLike[U], from, until, self, 0, self.length)(
						ClassTag[U](elems.getClass.getComponentType)
					)
		*/
	//		{
	//			val res = Array.of(elems.getClass.getComponentType.castParam[A], self.length + elems.length)
	//			ArrayLike.copy(elems, 0, res, 0, elems.length)
	//			ArrayLike.copy(self, 0, res, elems.length, self.length)
	//			res
	//		}

		@inline def concat(elems :IterableOnce[E]) :Array[E] = appendedAll(elems)
		@inline def concat[U >: E :ClassTag](elems :IterableOnce[U]) :Array[U] = appendedAll(elems)
		@inline def concat[U >: E :ClassTag](elems :ArrayLike[U]) :Array[U] = appendedAll(elems)
	//		@inline def concat(elems :ArrayLike[E]) :Array[E] = appendedAll(elems)
		//Can't be an IArray, because we rely on the fact that we can assign E to A
		@inline def concat[U >: E](elems :Array[U]) :Array[U] =
			Array.copyOfRanges(self :ArrayLike[U], 0, self.length, elems, 0, elems.length)(
				ClassTag[U](elems.getClass.getComponentType)
			)


		@inline def +:[U >: E :ClassTag](elem :U) :Array[U] = prepended(elem)
		@inline def +:(elem :E) :Array[E] = prepended(elem)
		@inline def :+[U >: E :ClassTag](elem :U) :Array[U] = appended(elem)
		@inline def :+(elem :E) :Array[E] = appended(elem)

		@inline def :++[U >: E :ClassTag](elems :IterableOnce[U]) :Array[U] = appendedAll(elems)
		@inline def :++[U >: E :ClassTag](elems :ArrayLike[U]) :Array[U] = appendedAll(elems)
	//		@inline def :++[A >: E](elems :Array[A]) :Array[A] = appendedAll(elems)
		@inline def :++(elems :IterableOnce[E]) :Array[E] = appendedAll(elems)
		@inline def :++(elems :ArrayLike[E]) :Array[E] = appendedAll(elems)

		@inline def ++:[U >: E :ClassTag](elems :IterableOnce[U]) :Array[U] = prependedAll(elems)
		@inline def ++:[U >: E :ClassTag](elems :ArrayLike[U]) :Array[U] = prependedAll(elems)
	//		@inline def ++:[A >: E](elems :Array[A]) :Array[A] = prependedAll(elems)
		@inline def ++:(elems :IterableOnce[E]) :Array[E] = prependedAll(elems)
		@inline def ++:(elems :ArrayLike[E]) :Array[E] = prependedAll(elems)

		@inline def ++[U >: E :ClassTag](elems :IterableOnce[U]) :Array[U] = concat(elems)
		@inline def ++[U >: E :ClassTag](elems :ArrayLike[U]) :Array[U] = concat(elems)
		@inline def ++[U >: E](elems :Array[U]) :Array[U] = concat(elems)
		@inline def ++(elems :IterableOnce[E]) :Array[E] = concat(elems)
	//		@inline def ++(elems :ArrayLike[E]) :Array[E] = concat(elems)

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
		@inline def patch[U >: E](from :Int, elems :Array[U], replaced :Int)(implicit __ :Ignored) :Array[U] =
			patch(from, elems :ArrayLike[U], replaced)(ClassTag[U](elems.getClass.getComponentType))


		/** A sequence view on the `[from, until)` index range of this array. Any modification to either this array
		  * or the returned sequence will be visible in the other. Further slicing of the sequence also return
		  * views sharing the same underlying array.
		  */
		def subseq(from :Int, until :Int) :mutable.IndexedSeq[E] = ArraySlice.slice(self, from, until)


		//moved to ArrayLikeExtension
	//		/** Copies values from the index range `[from, from + len)` from this array to the argument array, starting
	//		  * with position `start`. Copying stops when end of this or the argument array is reached, or after copying
	//		  * `until - from` elements. If `[from, from + len)` range contains indices outside of `[0, this.length)`,
	//		  * indices out of range are ignored.
	//		  */
	//		@throws[IndexOutOfBoundsException]("if start is less than zero.")
	//		def copyRangeToArray[U >: E](xs :Array[U], from :Int, start :Int, len :Int) :Int =
	//			if (len <= 0 || from >= self.length || start >= xs.length)
	//				0
	//			else {
	//				val from0  = math.max(from, 0)
	//				val copied = math.min(len, math.min(xs.length - start, self.length - from0))
	//				ArrayLike.copy(self, from0, xs, start, copied)
	//				copied
	//			}
	//
	//		/** Copies values from the index range `[from, from + len)` from this array to the argument array, starting
	//		  * with position `start`. Copying stops when end of this or the argument array is reached, or after copying
	//		  * `until - from` elements. If `[from, from + len)` range contains indices outside of `[0, this.length)`,
	//		  * indices out of range are ignored.
	//		  */
	//		@throws[IndexOutOfBoundsException]("if start is less than zero.")
	//		@inline def copyRangeToArray[U >: E](xs :Array[U], from :Int, len :Int = Int.MaxValue) :Int =
	//			copyRangeToArray(xs, from, 0, len)

	//
	//		/** Copies values from the index range `[from, from + len)` from this array to the argument array, starting
	//		  * with position `start`. Copying stops when end of this or the argument array is reached, or after copying
	//		  * `until - from` elements. If `[from, from + len)` range contains indices outside of `[0, this.length)`,
	//		  * indices out of range are ignored.
	//		  */
	//		@throws[IndexOutOfBoundsException]("if start is less than zero.")
	//		@inline def copyRangeToRefArray[A >: E](xs :RefArray[Any], from :Int, start :Int = 0, len :Int = Int.MaxValue) :Int =
	//			copyRangeToArray(xs.asAnyArray, from, start, len)
	//
	//		/** Same as `copyToArray(xs :Array[A], start :Int, len :Int)`, but works for reference arrays. */
	//		@inline def copyToRefArray[A >: E](xs :RefArray[A], start :Int = 0, len :Int = Int.MaxValue) :Int =
	//			copyRangeToArray(xs.asAnyArray, 0, start, len)

		//moved to ArrayLikeExtension
	//		/** Copies the elements of this $coll to the given array, starting with `from`-th element.
	//		  * Copying ends when the iterator has no additional elements, or `len` or `xs.length` elements are copied,
	//		  * whichever is smaller. The first element is written at index `start % xs.length`, and if the end of the $coll
	//		  * is reached before any of the above happens, copying resumes from the beginning of the array.
	//		  * @return the number of elements copied.
	//		  * @throws IndexOutOfBoundsException if `start` is less than zero.
	//		  */
	//		def cyclicCopyRangeToArray[U >: E](xs :Array[U], from :Int, start :Int, len :Int = Int.MaxValue) :Int = {
	//			val copied = math.max(0, math.min(self.length - math.max(from, 0), len))
	//			Array.cyclicCopyTo(self, from, xs, start % xs.length, copied)
	//			copied
	//		}
	//
	//		/** Copies the elements of this $coll to the given array.
	//		  * Copying ends when the iterator has no additional elements, or `len` or `xs.length` elements are copied,
	//		  * whichever is smaller. The first element is written at index `start % xs.length`, and if the end of the $coll
	//		  * is reached before any of the above happens, copying resumes from the beginning of the array.
	//		  * @return the number of elements copied.
	//		  * @throws IndexOutOfBoundsException if `start` is less than zero.
	//		  */
	//		@inline def cyclicCopyToArray[U >: E](xs :Array[U], start :Int, len :Int = Int.MaxValue) :Int =
	//			cyclicCopyRangeToArray(xs, 0, start, len)

		//moved to ArrayLikeExtension
	//		def sameElements(that :IterableOnce[_]) :Boolean = mutable.ArraySeq.make(self).sameElements(that)
	//
	//		/** Equality of corresponding elements in the two arrays. Array component type is ignored, and an array
	//		  * of a primitive type will equal an array of the appropriate box class, as long as the actual values are equal.
	//		  */
	//		def sameElements(that :ArrayLike[_]) :Boolean =
	//			(self eq that) || (self.length == that.length) && {
	//				val elemType = self.getClass.getComponentType
	//				def slowEquals = {
	//					var i = that.length - 1
	//					while (i > 0 && self(i) == that(i))
	//						i -= 1
	//					true
	//				}
	//				if (elemType == that.getClass.getComponentType)
	//					if (classOf[AnyRef] isAssignableFrom elemType)
	//						Arrays.equals(self.asInstanceOf[Array[AnyRef]], that.asInstanceOf[Array[AnyRef]])
	//					else if (elemType == classOf[Int])
	//						Arrays.equals(self.asInstanceOf[Array[Int]], that.asInstanceOf[Array[Int]])
	//					else if (elemType == classOf[Long])
	//						Arrays.equals(self.asInstanceOf[Array[Long]], that.asInstanceOf[Array[Long]])
	//					else if (elemType == classOf[Double])
	//						Arrays.equals(self.asInstanceOf[Array[Double]], that.asInstanceOf[Array[Double]])
	//					else if (elemType == classOf[Byte])
	//						Arrays.equals(self.asInstanceOf[Array[Byte]], that.asInstanceOf[Array[Byte]])
	//					else if (elemType == classOf[Char])
	//						Arrays.equals(self.asInstanceOf[Array[Char]], that.asInstanceOf[Array[Char]])
	//					else if (elemType == classOf[Float])
	//						Arrays.equals(self.asInstanceOf[Array[Float]], that.asInstanceOf[Array[Float]])
	//					else if (elemType == classOf[Boolean])
	//						Arrays.equals(self.asInstanceOf[Array[Boolean]], that.asInstanceOf[Array[Boolean]])
	//					else
	//						slowEquals
	//				else
	//					slowEquals
	//			}
	//
	//		/** Deep equality of array elements and array component (element) types. */
	//		def sameAs(that :Any) :Boolean = that match {
	//			case self :AnyRef if this.asAnyRef eq self => true
	//			case other :Array[_]
	//				if other.length == self.length && other.getClass.getComponentType == self.getClass.getComponentType
	//			=>
	//				this sameElements other
	//			case _ => false
	//		}
	//
	//		def contentsString :String = self.mkString(self.localClassName + "(", ", ", ")")
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
		@inline final def apply(v1 :Array[E])(implicit dummy :DummyImplicit) :ArrayExtension[E] = new ArrayExtension(v1)
	}
	private def newArrayExtensionConversion[E] =
		new PriorityConversion.Wrapped[Array[E], ArrayExtension[E]](new ArrayExtension(_))
			with ArrayExtensionConversion[E]
	private val ArrayExtensionConversionPrototype :ArrayExtensionConversion[Any] = newArrayExtensionConversion




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
	  */ //todo: rename all 'Object' extensions to 'Companion'
	sealed trait ArrayObjectExtension extends Any {

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
		final def like[E](original :Array[E], length :Int) :Array[E] =
			if (length == 0)
				emptyLike(original)
			else
//				of(array.getClass.getComponentType.castParam[E], length)
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
		@throws[NegativeArraySizeException]("if newLength is negative")
		@inline final def of[E](elemType :Class[E], length :Int) :Array[E] = ArrayFactory.ofDim(elemType, length)

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
			throw new IndexOutOfBoundsException(offset)
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
			throw new IndexOutOfBoundsException(offset)
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
		final def copyOfRange[E](elems :Array[E], from :Int, until :Int, offset :Int, newLength :Int) :Array[E] =
			if (offset < 0)
				throw new IndexOutOfBoundsException(offset)
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
				ArrayFactory.copyAs(elems, elementClass, until)
			else if ({
	            elementClass.isAssignableFrom(elems.getClass.getComponentType) ||
	               classOf[AnyRef].isAssignableFrom(elementClass) && elems.isInstanceOf[Array[AnyRef]]
			}) {
				val until0 = math.min(until, elems.length)
				((elems :Any) : @unchecked) match {
					case a :Array[Unit]    => ArrayFactory.copyAs[E](a, elementClass, until0 - from)
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
				throw new IndexOutOfBoundsException(offset)
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
					ArrayFactory.copyAs(elems, elementClass, newLength)
				else if (from0 >= offset & newLength <= length - from0 & copied >= (newLength >> 1)) {
					val res = copyOfRange(elems, from0 - offset, from0 + newLength - offset, elementClass)
					res.clear(0, offset)
					res.clear(offset + copied, newLength)
					res
				} else if (offset == from0 & newLength <= length & (copied >= (newLength >> 1))) {
					val res = ArrayFactory.copyAs(elems, elementClass, newLength)
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
//				throw new IndexOutOfBoundsException(
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
				throw new IllegalArgumentException(
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
//				throw new IndexOutOfBoundsException(
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
				val res = ArrayFactory.copyAs[E](array1, elementClass, newLength)
				ArrayLike.copy(array2, from2, res, length1, length2)
				res.clear(length1 + length2, math.min(newLength, array1.length))
				res
			} else if (length1 >= (newLength >> 1) && array1.length - from1 >= newLength) {
				val res = copyOfRange(array1, from1, from1 + newLength, elementClass)
				ArrayLike.copy(array2, from2, res, length1, length2)
				res.clear(length1 + length2, newLength)
				res
			} else if (from2 == length1 && math.min(newLength, array2.length) - length2 <= (newLength >> 1)) {
				val res = ArrayFactory.copyAs[E](array2, elementClass, newLength)
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
//				throw new IndexOutOfBoundsException(
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
				throw new IllegalArgumentException(
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
//				throw new IndexOutOfBoundsException(
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
				val res = ArrayFactory.copyAs[E](array1, elementClass, newLength)
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
				val res = ArrayFactory.copyAs[E](array2, elementClass, newLength)
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
				throw new IndexOutOfBoundsException(
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
				throw new IndexOutOfBoundsException(
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

}
