package net.noresttherein.sugar.arrays



import scala.Array.UnapplySeqWrapper
import scala.annotation.tailrec
import scala.collection.{ArrayOps, IterableFactory, Stepper, StepperShape, View, mutable}
import scala.collection.Stepper.EfficientSplit
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.reflect.ClassTag

import net.noresttherein.sugar.casting.{cast2TypeParamsMethods, cast3TypeParamsMethods, castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.collections.{ArrayIterableOnce, ArrayLikeSlice, IArraySlice, IRefArraySlice, MatrixBuffer}
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{Yes, No}




@SerialVersionUID(Ver)
object RefArrayLike extends IterableFactory.Delegate[RefArrayLike](RefArray) {

	/** Wraps and unwraps 'erased' arrays in scala collections,
	  * being mindful both of type safety and mutability contracts. $warning
	  * @define warning Arrays are always shared, never copied, and represented as
	  *                 [[net.noresttherein.sugar.arrays.RefArrayLike! RefArrayLike!]]`[E]`
	  *                 (that is, internally, as `Array[AnyRef]`). Only 'untagged' collections,
	  *                 that is backed by `Array[AnyRef]`, and not any of its subclasses, are recognized.
	  *                 Wrapping sequences are neither mutable nor immutable, and the matching pattern will likewise
	  *                 extract an array from any supported collection backed by an `Array[AnyRef]`,
	  *                 be it mutable or immutable. Note however that neither is `RefArrayLike` a mutable type,
	  *                 and casting it to [[net.noresttherein.sugar.arrays.RefArray! RefArray]] or `Array`
	  *                 may both compromise immutability, and modifying it will, most likely,
	  *                 result in an exception (likely `ClassCastException`) being thrown at some later point
	  *                 by a part of application using the unwrapped collection.
	  */
	@SerialVersionUID(Ver)
	object Wrapped {
		def apply[A](array :RefArrayLike[A]) :collection.IndexedSeq[A] = ArrayLikeSlice.wrap(array)

		def unapply[A](elems :IterableOnce[A]) :Maybe[RefArrayLike[A]] =
			if (elems.knownSize < 0)
				No
			else {
				val array = elems match {
					case slice :ArrayIterableOnce[_] if slice.knownSize == slice.unsafeArray.length =>
						slice.unsafeArray
					case _ :collection.IndexedSeqOps[_, _, _] => elems match {
						case seq :ArraySeq[_]         => seq.unsafeArray
						case seq :mutable.ArraySeq[_] => seq.array
						case VectorArray(array)       => array
						case seq :ArrayBuffer[_] if seq.length == CheatedAccess.array(seq).length =>
							CheatedAccess.array(seq)
						case seq :MatrixBuffer[_] if seq.dim == 1 && seq.startIndex == 0 && seq.length == seq.data1.length =>
							seq.data1
						case _ => null
					}
					case _ => null
				}
				if (array != null && array.getClass == classOf[Array[AnyRef]])
					Yes(array.castFrom[Array[_], IRefArray[A]])
				else
					No
			}
	}

	/** Wraps and unwraps both mutable and immutable scala collections backed by consecutive sections
	  * of `Array[AnyRef]` arrays in a safe manner. $warning
	  */
	@SerialVersionUID(Ver)
	object Slice {
		def apply[A](array :RefArrayLike[A], from :Int, until :Int) :collection.IndexedSeq[A] =
			ArrayLikeSlice.slice(array, from, until)

		def unapply[A](elems :IterableOnce[A]) :Maybe[(RefArrayLike[A], Int, Int)] = elems match {
			case arr :ArrayIterableOnce[_] =>
				val array = arr.unsafeArray.castFrom[Array[_], RefArrayLike[A]]
				if (array.getClass == classOf[Array[AnyRef]])
					Yes((array, arr.startIndex, arr.startIndex + arr.knownSize))
				else
					No

			case _ :collection.IndexedSeqOps[_, _, _] => elems match {
				case seq :ArraySeq[_] if seq.unsafeArray.getClass == classOf[Array[AnyRef]] =>
					Yes((seq.unsafeArray.castFrom[Array[_], RefArrayLike[A]], 0, seq.unsafeArray.length))

				case seq :mutable.ArraySeq[_] if seq.array.getClass == classOf[Array[AnyRef]] =>
					Yes((seq.array.castFrom[Array[_], RefArrayLike[A]], 0, seq.length))

				case VectorArray(array)        =>
					Yes((array.castFrom[Array[AnyRef], RefArrayLike[A]], 0, elems.knownSize))

				case seq :MatrixBuffer[_] if seq.dim == 1 =>
					val array = seq.data1.castFrom[Array[_], RefArrayLike[A]]
					val start = seq.startIndex
					val end   = start + seq.length
					if (array.getClass == classOf[Array[AnyRef]] && end <= array.asInstanceOf[Array[AnyRef]].length)
						Yes((array, start, end))
					else
						No
				case _ =>
					No
			}
			case _ => No
		}
	}



	/** Extension methods for [[net.noresttherein.sugar.arrays.ArrayLike ArrayLike]]`[E]` implementations
	  * backed by an erased array (`Object[]`), boxing value types. These consist of
	  * [[net.noresttherein.sugar.arrays.RefArray RefArray]] and
	  * [[net.noresttherein.sugar.arrays.IRefArray IRefArray]]. Contains methods creating a new `Arr[_]`, which,
	  * unlike those in [[net.noresttherein.sugar.arrays.IArray.IArrayExtension]]
	  * and standard [[collection.ArrayOps ArrayOps]] do not require a [[scala.reflect.ClassTag ClassTag]],
	  * but can be implemented safely when the adapted array is an `Array[Any]`.
	  * Can be enabled by importing
	  * `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.RefArrayLikeExtension RefArrayLikeExtension]]
	  * These same conversion is also available under
	  * `net.noresttherein.sugar.arrays.RefArrayLike.extensions.RefArrayLikeExtension`
	  * and in object `net.noresttherein.sugar.extensions`.
	  * @see [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension]] - additional extension methods
	  * @see [[net.noresttherein.sugar.arrays.IArray.IArrayExtension]] - the same method subset for
	  *      [[net.noresttherein.sugar.arrays.IArray IArray]].
	  * @tparam E   the type of the elements in the array (but not the component type of the underlying array!).
	  * @tparam Arr the type constructor of an `ArrayLike` subtype returned by methods which return
	  *             an `Array` in [[collection.ArrayOps ArrayOps]] (and the `ArrayLike` subtype being granted
	  *             these methods).
	  */
	class RefArrayLikeExtension[Arr[X] <: RefArrayLike[X], E] private[arrays] (private val self :Array[Any])
		extends AnyVal
	{
		//This method may be hampering inlining; if so, we should make it package-protected.
//		@inline private def expose(result :Array[Any]) :Arr[E] = result.asInstanceOf[Arr[E]]
		@inline private def expose[U >: E](result :Array[Any]) :Arr[U] = result.asInstanceOf[Arr[U]]
		@inline private def exposeOther[X](result :Array[Any]) :Arr[X] = result.asInstanceOf[Arr[X]]

		@inline def length :Int = self.length
		@inline def apply(i :Int) :E = self(i).asInstanceOf[E]
		@inline def indexOf(elem :E, from :Int = 0) :Int = self.indexOf(elem, from)
		@inline def lastIndexOf(elem :E, end :Int = Int.MaxValue) :Int = self.lastIndexOf(elem, end)
		@inline def contains(elem :E) :Boolean = self.contains(elem)

		@inline def scanLeft[A](z :A)(op :(A, E) => A) :Arr[A] =
			exposeOther(self.scanLeft[Any](z)(op.castParams[Any, Any, Any]))

		@inline def scanRight[A](z :A)(op :(E, A) => A) :Arr[A] =
			exposeOther(self.scanRight[Any](z)(op.castParams[Any, Any, Any]))

		@inline def scan[U >: E](z :U)(op :(U, U) => U) :Arr[U] =
			expose(self.scan[Any](z)(op.castParams[Any, Any, Any]))

		@inline def collect[A](pf :PartialFunction[E, A]) :Arr[A] =
			exposeOther(self.collect(pf.castParams[Any, Any]))

		@inline def partitionMap[E1, E2](f: E => Either[E1, E2]) :(Arr[E1], Arr[E2]) =
			self.partitionMap(f.castParams[Any, Either[Any, Any]]).castFrom[(Array[Any], Array[Any]), (Arr[E1], Arr[E2])]

		@inline def map[A](f :E => A) :Arr[A] = exposeOther(self.map(f.castParams[Any, Any]))
		@inline def flatMap[A](f :E => IterableOnce[A]) :Arr[A] =
			exposeOther(self.flatMap(f.castParams[Any, IterableOnce[Any]]))

		@inline def flatMap[As, A](f :E => As)(implicit asIterable :As => Iterable[A]) :Arr[A] =
			exposeOther(self.flatMap(f.castParams[Any, Iterable[Any]]))

		@inline def flatten[A](implicit asIterable :E => IterableOnce[A]) :Arr[A] =
			exposeOther(self.flatten(asIterable.castParams[Any, IterableOnce[Any]], ClassTag.Any))

		@inline def groupMap[K, A](key :E => K)(f :E => A) :Map[K, Arr[A]] =
			self.groupMap[K, Any](key.castParams[Any, K])(f.castParams[Any, Any]).castParam2[Arr[A]]


		@inline def unzip[E1, E2](implicit asPair :E => (E1, E2)) :(Arr[E1], Arr[E2]) =
			self.unzip(asPair.castParams[Any, (Any, Any)], ClassTag.Any, ClassTag.Any)
			     .castFrom[(Array[Any], Array[Any]), (Arr[E1], Arr[E2])]

		@inline def unzip3[E1, E2, E3](implicit asTriple :E => (E1, E2, E3)) :(Arr[E1], Arr[E2], Arr[E3]) =
			self.unzip3(asTriple.castParams[Any, (Any, Any, Any)], ClassTag.Any, ClassTag.Any, ClassTag.Any)
			     .castFrom[(Array[Any], Array[Any], Array[Any]), (Arr[E1], Arr[E2], Arr[E3])]

		@throws[IndexOutOfBoundsException]("if index < 0 or index >= this.length")
		@inline def updated[U >: E](index :Int, elem :U) :Arr[U] =
			expose(new ArrayOps(self).updated(index, elem))

		/** An 'exploded' variant of `updatedAll`: a copy of this array with elements starting at `index` substituted
		  * by `first`, `second`, and contents of `rest`. The index must lie in range `[0..length - rest.size - 2)`.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.length > this.length")
		@inline def updatedAll[U >: E](index :Int, first :U, second :U, rest :U*) :Arr[U] =
			expose(self.updatedAll[Any](index, first, second, rest :_*))

		/** Updates consecutive elements of this array, starting with the specified index,
		  * with elements from the collection. Equivalent to
		  * [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.patch patch]]`(index, elems, elems.size)`,
		  * but more efficient due to a single array allocation.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.length > this.length")
		@inline def updatedAll[U >: E](index :Int, elems :IterableOnce[U]) :Arr[U] =
			expose(self.updatedAll[Any](index, elems))

		/** Updates consecutive elements of this array, starting with the specified index,
		  * with elements from the collection. Equivalent to
		  * [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.patch patch]]`(index, elems, elems.size)`,
		  * but more efficient due to a single array allocation.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.length > this.length")
		@inline def updatedAll[U >: E](index :Int, elems :ArrayLike[U]) :Arr[U] =
			expose(self.updatedAll[Any](index, elems))



		/** An 'exploded' variant of `overwritten`: a copy of this array with elements starting at `index` substituted
		  * by `first`, `second`, and contents of `rest`. The index must lie in range `[0..length - rest.size - 2)`.
		  */
		@inline def overwritten[U >: E](index :Int, first :U, second :U, rest :U*) :Arr[U] =
			expose(self.overwritten[Any](index, first, second, rest :_*))

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
		@inline def overwritten[U >: E](index :Int, elems :IterableOnce[U]) :Arr[U] =
			expose(self.overwritten[Any](index, elems))

		/** A copy o this array, with elements starting at `index` overwritten by subsequent elements
		  * of the argument array.
		  * This is the same as [[net.noresttherein.sugar.arrays.extensions.ArrayExtension.updatedAll updatedAll]],
		  * except it behaves differently if `index` is out of range: instead of throwing an exception,
		  * it treats this sequence as a view on a slice some infinite sequence, and instead 'updates' that sequence.
		  * Indices out of range are silently ignored, and only the legal range is actually updated.
		  * @example
		  * {{{
		  *     val array = RefArray.iterate(0, 5)(0, _ + 1)
		  *     val array.overwritten(-2, Array(-4, -3, -2, -1)) //RefArray(-2, -1, 2, 3, 4)
		  * }}}
		  */
		@inline def overwritten[U >: E](index :Int, elems :ArrayLike[U]) :Arr[U] =
			expose(self.overwritten[Any](index, elems))

		/** A copy of this array, with the element inserted at a given position in this array,
		  * and all elements at positions equal or greater than `index` by one element further.
		  * This is equivalent to [[net.noresttherein.sugar.arrays.ArrayLike.RefArrayLikeExtension.patch patch]]
		  * with a singleton collection and `replaced` equal to zero, but the index
		  * must be in the valid range for this array.
		  * @return `take(index) :+ elem :++ drop(index)`, but in a more efficient manner.
		  * @throws IndexOutOfBoundsException if `index` < 0 or `index > length`.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index > this.length")
		@inline def inserted[U >: E](index :Int, elem :U) :Arr[U] = expose(self.inserted[Any](index, elem))

		/** An 'exploded' variant of `updatedAll`: a copy of this array with elements starting at `index` substituted
		  * by `first`, `second`, and contents of `rest`. The index must lie in range `[0..length)`.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.length > this.length")
		@inline def insertedAll[U >: E](index :Int, first :U, second :U, rest :U*) :Arr[U] =
			expose(self.insertedAll[Any](index, first, second, rest :_*))

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * @return [[net.noresttherein.sugar.arrays.RefArrayLike.RefArrayLikeExtension.patch patch]]`(index, elems, 0)`.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.size > this.length")
		@inline def insertedAll[U >: E](index :Int, elems :IterableOnce[U]) :Arr[U] =
			expose(self.insertedAll[Any](index, elems))

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * @return [[net.noresttherein.sugar.arrays.RefArrayLike.RefArrayLikeExtension.patch patch]]`(index, elems, 0)`.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.length > this.length")
		@inline def insertedAll[U >: E](index :Int, elems :ArrayLike[U]) :Arr[U] =
			expose(self.insertedAll[Any](index, elems))

		@inline def :+[U >: E](x :U) :Arr[U] = expose(self.appended[Any](x))
		@inline def +:[U >: E](x :U) :Arr[U] = expose(self.prepended[Any](x))

		@inline def appended[U >: E](x :U) :Arr[U] = expose(self.appended[Any](x))
		@inline def prepended[U >: E](x :U) :Arr[U] = expose(self.prepended[Any](x))

		@inline def appendedAll[U >: E](first :U, second :U, rest :U*) :Arr[U] =
			expose(self.appendedAll[Any](first, second,  rest :_*))

		@inline def prependedAll[U >: E](first :U, second :U, rest :U*) :Arr[U] =
			expose(self.prependedAll[Any](first, second, rest :_*))

		@inline def concat[U >: E](suffix :IterableOnce[U]) :Arr[U] = appendedAll(suffix)
		@inline def concat[U >: E](suffix :ArrayLike[U]) :Arr[U] = appendedAll(suffix)

		@inline def ++[U >: E](suffix :IterableOnce[U]) :Arr[U] = appendedAll(suffix)
		@inline def ++[U >: E](suffix :ArrayLike[U]) :Arr[U] = appendedAll(suffix)

		@inline def :++[U >: E](suffix :IterableOnce[U]) :Arr[U] = appendedAll(suffix)
		@inline def :++[U >: E](suffix :ArrayLike[U]) :Arr[U] = appendedAll(suffix)

		@inline def appendedAll[U >: E](suffix :IterableOnce[U]) :Arr[U] = expose(self.appendedAll[Any](suffix))

		@inline def appendedAll[U >: E](suffix :ArrayLike[U]) :Arr[U] =
			RefArray.copyOfRanges(self, 0, self.length, suffix, 0, suffix.length).castFrom[RefArray[Any], Arr[U]]
/*
		{
			val res = new Array[Any](array.length + suffix.length)
			arraycopy(array, 0, res, 0, array.length)
			arraycopy(suffix, 0, res, array.length, suffix.length)
			res.castFrom[Array[Any], Arr[A]]
		}
*/

		@inline def ++:[U >: E](prefix :IterableOnce[U]) :Arr[U] = prependedAll(prefix)
		@inline def ++:[U >: E](prefix :ArrayLike[U]) :Arr[U] = prependedAll(prefix)

		@inline def prependedAll[U >: E](prefix :IterableOnce[U]) :Arr[U] =
			expose(self.prependedAll[Any](prefix))

		@inline def prependedAll[U >: E](prefix :ArrayLike[U]) :Arr[U] =
			RefArray.copyOfRanges(prefix, 0, prefix.length, self, 0, self.length).castFrom[RefArray[Any], Arr[U]]
/*
		{
			val prefixLength = prefix.length
			val res = new Array[Any](array.length + prefixLength)
			arraycopy(prefix, 0, res, 0, prefixLength)
			arraycopy(array, 0, res, prefixLength, array.length)
			res.castFrom[Array[Any], Arr[A]]
		}
*/

		def padTo[U >: E](len :Int, elem :U) :Arr[U] = expose {
			val res = new Array[Any](len)
			var i = self.length
			arraycopy(self, 0, res, 0, i)
			while (i < len) {
				res(i) = elem
				i += 1
			}
			res
		}

		@inline def patch[U >: E](from :Int, other :IterableOnce[U], replaced :Int) :Arr[U] =
			expose(self.patch[Any](from, other, replaced))

		@inline def patch[U >: E](from :Int, other :ArrayLike[U], replaced :Int) :Arr[U] = expose {
			val clippedFrom     = math.max(from, 0)
			val clippedReplaced = math.min(math.max(replaced, 0), self.length - clippedFrom)
			val until           = clippedFrom + clippedReplaced
			ErasedArray.copyOfRanges(
				self, 0, from, other, 0, other.length, self, until, self.length
			)
		}
	}



	/** A lazy filtered array. No filtering is applied until one of `foreach`, `map` or `flatMap` is called.
	  * Implementation adapted from [[collection.ArrayOps.WithFilter ArrayOps.WithFilter]] to return `Arr[T]`.
	  */
	class WithFilter[Arr[_], E] private[arrays] (p :E => Boolean, xs :Array[Any], factory :IterableFactory[Arr]) {
		/** Apply `f` to each element for its side effects.
		  * Note: [U] parameter needed to help scalac's type inference.
		  */
		def foreach[U](f :E => U) :Unit = {
			val pred = p.asInstanceOf[Any => Boolean]
			val fun  = f.asInstanceOf[Any => U]
			val len = xs.array.length
			var i = 0
			while (i < len) {
				val x = xs(i)
				if (pred(x)) fun(x)
				i += 1
			}
		}

		/** Builds a new array by applying a function to all elements of this array.
		  * @param f      the function to apply to each element.
		  * @tparam A     the element type of the returned array.
		  * @return a new array resulting from applying the given function
		  *         `f` to each element of this array and collecting the results.
		  */
		def map[A](f :E => A) :Arr[A] = {
			val pred = p.asInstanceOf[Any => Boolean]
			val fun  = f.asInstanceOf[Any => A]
			val b = factory.newBuilder[A]
			var i = 0
			while (i < xs.length) {
				val x = xs(i)
				if (pred(x)) b += fun(x)
				i = i + 1
			}
			b.result()
		}

		/** Builds a new array by applying a function to all elements of this array
		  * and using the elements of the resulting collections.
		  *
		  * @param f      the function to apply to each element.
		  * @tparam A     the element type of the returned array.
		  * @return a new array resulting from applying the given collection-valued function
		  *         `f` to each element of this array and concatenating the results.
		  */
		def flatMap[A](f :E => IterableOnce[A]) :Arr[A] = {
			val pred = p.asInstanceOf[Any => Boolean]
			val fun  = f.asInstanceOf[Any => IterableOnce[A]]
			val b = factory.newBuilder[A]
			var i = 0
			while (i < xs.length) {
				val x = xs(i)
				if (pred(x)) b ++= fun(xs(i))
				i += 1
			}
			b.result()
		}

		def flatMap[As, A](f :E => As)(implicit asIterable :As => Iterable[A]) :Arr[A] =
			flatMap(x => asIterable(f(x)))

		/** Creates a new non-strict filter which combines this filter with the given predicate. */
		def withFilter(q :E => Boolean) :WithFilter[Arr, E] =
			new WithFilter[Arr, E](a => p(a) && q(a), xs, factory)
	}
}






/** Shared implementation of factories of type `Arr`, represented by an `Array[AnyRef]` during execution.
  * @define Coll `RefArrayLike`
  * @define coll object array
  */
abstract class RefArrayLikeFactory[Arr[X] <: ArrayLike[X]] private[arrays] extends IterableFactory[Arr] {
	@inline private def expose[X](array :Array[Any]) :Arr[X] = array.asInstanceOf[Arr[X]]

	/** Allocates a new `Array[AnyRef]` and copies all elements from the argument, returning it as a $Coll`[E]`.
	  * If the argument is a value array, the elements will be boxed.
	  */
	@inline final def copyOf[E](array :ArrayLike[E]) :Arr[E] =
		copyOf(array, array.length)

	/** Reallocates the given array as a $Coll`[E]` of a new size, and copies `min(newLength, array.length)`
	  * of its first elements. If the argument is a value array, the elements will be boxed.
	  */
	@throws[NegativeArraySizeException]("if newLength is negative")
	final def copyOf[E](array :ArrayLike[E], newLength :Int) :Arr[E] =
		expose(ArrayFactory.copyOf[Any](array, newLength)(ClassTag.Any))

	/** Copies the elements of `array` in the index range `[from, until)` to a new $coll with an erased element type.
	  * @param array The sliced array.
	  * @param from  The index of the element in `array` to be copied as the first element of the new array.
	  *              Must be in range `[0, array.length]`, or an `IndexOutOfBoundsException` will be thrown.
	  * @param until The index after the last copied element in `array`. If less than `from`, an empty $Coll is returned.
	  *              If `until > array.length`, then the new $coll will contain `until - array.length` `null` elements
	  *              in its suffix.
	  * @return An `Array[AnyRef]` of length `until - from` as a $Coll`[E]`, with the copied slice.
	  */
	final def copyOfRange[E](array :ArrayLike[E], from :Int, until :Int) :Arr[E] =
		expose(Array.copyOfRange[Any](array, from, until)(ClassTag.Any))

	/** Copies slices from two array into a new array. Providing `until < from` has the same effect as `until == from`,
	  * that is copying nothing. However, `untilX > arrayX.length` is treated as if the source array
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
	  * @return An `Array[AnyRef]` of length `until1 - from1 + until2 - from2` (for `from1 <= until1 && from2 <= until2`),
	  *         as a $Coll`[E]`, with the copied slices.
	  */
	final def copyOfRanges[E](array1 :ArrayLike[E], from1 :Int, until1 :Int,
	                          array2 :ArrayLike[E], from2 :Int, until2 :Int) :Arr[E] =
		expose(Array.copyOfRanges[Any](array1, from1, until1, array2, from2, until2)(ClassTag.Any))

	/** Copies slices from three array into a new array. Providing `until < from` has the same effect as `until == from`,
	  * that is copying nothing. However, `untilX > arrayX.length` is treated as if the source array
	  * were of length `untilX`, and contained zeros/nulls past its actual length.
	  * Element `array1(from1)` is copied to `result(0)`, and so on, with `array2(from2)`
	  * copied to `result(until1 - from1)`, and `array3(from3)` to `result(until2 - from2 + until1 - from1)`
	  * (assuming `until1 >= from1`).
	  * @param array1 The first sliced array.
	  * @param from1  The index of the element in `array1` to be copied as the first element of the new array.
	  *               Must be in range `[0, array1.length]`, or an `IndexOutOfBoundsException` will be thrown.
	  * @param until1 The index after the last copied element in `array1`.
	  * @param array2 The second sliced array.
	  * @param from2  The index of the element in `array2` to be copied after `array1(until1 - 1)` into the new array.
	  *               Must be in range `[0, array2.length]`, or an `IndexOutOfBoundsException` will be thrown.
	  * @param until2 The index after the last copied element in `array1`.
	  * @param array3 The third sliced array.
	  * @param from3  The index of the element in `array3` to be copied after `array2(until2 - 1)` into the new array.
	  *               Must be in range `[0, array3.length]`, or an `IndexOutOfBoundsException` will be thrown.
	  * @param until3 The index after the last copied element in `array1`.
	  * @return An `Array[AnyRef]` of length `until1 - from1 + until2 - from2 + until3 - from3` (for `untilN >= fromN`),
	  *         as a $Coll`[E]`, with the copied slices.
	  */
	final def copyOfRanges[E](array1 :ArrayLike[E], from1 :Int, until1 :Int,
	                          array2 :ArrayLike[E], from2 :Int, until2 :Int,
	                          array3 :ArrayLike[E], from3 :Int, until3 :Int) :Arr[E] =
		expose(
			Array.copyOfRanges[Any](array1, from1, until1, array2, from2, until2, array3, from3, until3)(ClassTag.Any)
		)


	@inline final override def apply[E](elems :E*) :Arr[E] = expose(Array[Any](elems :_*))

	override def from[E](source :IterableOnce[E]) :Arr[E] = expose(
		source match {
			case empty if empty.knownSize == 0 => Array.emptyAnyArray
			case items :Iterable[E]            => items.toArray[Any]
			case _                             => source.iterator.toArray[Any]
		}
	)

	@inline final def from[E](array :ArrayLike[E]) :Arr[E] = copyOf(array)

	@inline final override def empty[E] :Arr[E] = Array.emptyObjectArray.castFrom[Array[AnyRef], Arr[E]]

	final override def newBuilder[E] :Builder[E, Arr[E]] = Array.newBuilder(ClassTag.Any).castParam2[Arr[E]]

	def newBuilder[E](size :Int) :Builder[E, Arr[E]] = {
		val res = newBuilder[E]
		res sizeHint size
		res
	}


	/** Boxes the element if necessary, places it in a singleton `Array[AnyRef]`, and returns it as a $Coll`[E]`. */
	final def one[E](elem :E) :Arr[E] = expose {
		val a = new Array[Any](1)
		a(0) = elem
		a
	}

	/** Boxes the elements, if necessary, places them in an `Array[AnyRef]` of length 2, and returns as a $Coll`[E]`. */
	final def two[E](first :E, second :E) :Arr[E] = expose {
		val a = new Array[Any](2)
		a(0) = first
		a(1) = second
		a
	}


	/** A $coll filled with `n` copies of `elem`. */
	final def const[X](n :Int)(elem :X) :Arr[X] = expose {
		val res = new Array[Any](n)
		res.fill(elem)
		res
	}

	/** A complement of `iterate` and `unfold` provided by `IterableFactory`, which creates
	  * a $Coll`[X]` by recursively applying a partial function while defined to its own results and collecting
	  * all returned values. It is very similar to standard [[collection.IterableFactory.iterate iterate]],
	  * but instead of a fixed number of iterations, the generator function `next` is called for its return values
	  * until it is no longer applicable, which marks the end of the array.
	  * @param start the first element added to the array.
	  * @param next  a generator function returning subsequent elements for the array based on the previous one,
	  *              serving as the termination condition by indicating that it can no longer be applied
	  *              to the given argument.
	  * @tparam X the element type of the generated array.
	  * @return an $coll containing the sequence starting with `start` and resulting from recursively applying
	  *         `next` to itself.
	  */
	@inline final def generate[X](start :X)(next :PartialFunction[X, X]) :Arr[X] =
		expose(Array.generate[Any](start)(next.castParams[Any, Any]))

	/** Builds a $Coll`[X]` by recursively reapplying the given partial function to the initial element.
	  * Instead of listing a fixed number of elements, this method uses the generator function `next`
	  * as the termination condition and ends the recursion once it returns `None`. It is the opposite
	  * of [[scala.collection.IterableOnceOps.reduce reduce]] in the same way as
	  * [[scala.collection.IterableFactory.unfold unfold]] is the opposite
	  * of [[scala.collection.IterableOnceOps.fold fold]].
	  * @param start The first element added to the array.
	  * @param next  A generator function returning subsequent elements for the array based on the previous one,
	  *              or `None` to indicate the end of recursion.
	  * @tparam X the element type of the generated array.
	  * @return a $coll containing the sequence starting with `start`
	  *         and resulting from recursively applying `next` to itself.
	  */
	@inline final def expand[X](start :X)(next :X => Option[X]) :Arr[X] =
		expose(Array.expand[Any](start)(next.castParams[Any, Option[Any]]))

	/** Similar to [[collection.IterableFactory.iterate iterate]],
	  * but the iterating function accepts the positional index of the next element as an additional argument.
	  * @param start The first element of the created array.
	  * @param len   The size of the created array.
	  * @param f     A function generating subsequent elements following start.
	  *              The second element of the array will be `f(start, 1)`, the third `f(f(start, 1), 2)`, and so on.
	  */
	@inline final def iterateWithIndex[X](start :X, len :Int)(f :(X, Int) => X) :Arr[X] =
		expose(Array.iterateWithIndex[Any](start, len)(f.castParam1[Any]))



	@inline final def unapplySeq[E](array :Arr[E]) :UnapplySeqWrapper[E] =
		new UnapplySeqWrapper(array.castFrom[Arr[E], Array[E]])

}






/** Factory of erased arrays: `Array[E]` represented in runtime by an `Array[Any]`.
  * ''This will work only in generic contexts: '' `ErasedArray.ofDim[Int](1)` will throw a [[ClassCastException]].
  * @define Coll `Array`
  * @define coll array
  */
@SerialVersionUID(Ver)
private[sugar] case object ErasedArray extends RefArrayLikeFactory[Array] with IterableFactory[Array] {

	/** A new `Array[AnyRef]` of the specified length, cast to $Coll`[E]`. */
	@inline final def ofDim[E](length :Int) :Array[E] = new Array[Any](length).castFrom[Array[Any], Array[E]]

	/** Build an array with erased element type from the iterable collection.
	  *  @param  source the iterable collection
	  *  @return an array consisting of elements of the iterable collection
	  */
	@tailrec override def from[E](source :IterableOnce[E]) :Array[E] = source match {
		case it :View[E]                   => from(it.iterator)
		case it :Iterable[E] if it.isEmpty => empty
		case it :Iterator[E] if it.isEmpty => empty
//		case it :Wrapped(array)            => array
		case it :Iterable[E]               => it.toArray(ClassTag.Any.castParam[E])
		case _                             => source.iterator.toArray(ClassTag.Any.castParam[E])
	}

	/** ClassTag`[E]` with its `runtimeClass` equal to `Any`. */
	def erasedTag[E] :ClassTag[E] = ClassTag.Any.castParam[E]


	/** An unsafe, lower level wrapper and unwrapper of known `collection.IndexedSeq` implementations backed by arrays.
	  * This ignores both the transition between immutability and mutability, as well as the danger
	  * of `ClassCastException`s brought by the common practice of using an `Array[AnyRef]` internally as
	  * an `Array[Any]`, relying on automatic boxing and unboxing of the elements. The type compatibility
	  * must be verified after the extraction by the caller themselves, and it is assumed that the unwrapped arrays
	  * will never be modified, for example only to be wrapped in another immutable class, or to immediately copy
	  * the data. Wrapping an array equals declaration that the caller is the sole owner of it and refrains from
	  * modifying it any further.
	  */
	@SerialVersionUID(Ver)
	object Wrapped {
		@inline def apply[A](array :Array[A]) :collection.IndexedSeq[A] =
			ArrayLikeSlice.wrap(array) //not mutable.ArraySeq to be neither mutable nor immutable

		@inline def unsafe[A](array :Array[A]) :IndexedSeq[A] =
			ArraySeq.unsafeWrapArray(array)

		def unapply[A](elems :IterableOnce[A]) :Maybe[Array[_]] = elems match {
			case seq :ArrayIterableOnce[_] if seq.knownSize == seq.unsafeArray.length => Yes(seq.unsafeArray)
			case _ :collection.IndexedSeq[_] => elems match {
				case seq :ArraySeq[_]         => Yes(seq.unsafeArray)
				case seq :mutable.ArraySeq[_] => Yes(seq.array)
				case seq :ArrayBuffer[_]      =>
					val array = CheatedAccess.array(seq)
					if (array.length == seq.length) Yes(array) else No
				case seq :Vector[_]           =>
					val array = CheatedAccess.array(seq)
					if (array.length == seq.length) Yes(array) else No
				case _                         => No
			}
			case _ => No
		}
	}

	@SerialVersionUID(Ver)
	object Slice {
		@inline def apply[A](array :Array[A], from :Int, until :Int) :collection.IndexedSeq[A] =
			ArrayLikeSlice.slice(array, from, until)

		@inline def unsafe[A](array :Array[A], from :Int, until :Int) :IndexedSeq[A] =
			if (array.getClass == classOf[Array[Any]])
				IRefArraySlice.slice(array.asInstanceOf[IRefArray[A]], from, until)
			else
				IArraySlice.slice(array.asInstanceOf[IArray[A]], from, until)

		def unapply[A](elems :IterableOnce[A]) :Maybe[(Array[_], Int, Int)] = elems match {
			case seq :ArrayIterableOnce[_] =>
				val size   = seq.knownSize
				val offset = seq.startIndex
				if (size >= 0)
					Yes((seq.unsafeArray, offset, offset + size))
				else
					No
			case _ :collection.IndexedSeqOps[_, _, _] => elems match {
				case seq :ArraySeq[_]         => Yes((seq.unsafeArray, 0, seq.unsafeArray.length))
				case seq :Vector[_]           =>
					val array = CheatedAccess.array(seq)
					if (array.length == seq.length) Yes((array, 0, seq.length)) else No
				case seq :mutable.ArraySeq[_] => Yes((seq.array, 0, seq.array.length))
				case seq :ArrayBuffer[_]      => Yes((CheatedAccess.array(seq), 0, seq.length))
				case seq :MatrixBuffer[_]
					if seq.dim == 1 && seq.data1.getClass == classOf[Array[Any]] &&
						seq.startIndex + seq.length <= seq.data1.length
				=>
					Yes((seq.data1, seq.startIndex, seq.startIndex + seq.length))
				case _                        => No
			}
			case _ => No
		}
	}
}
