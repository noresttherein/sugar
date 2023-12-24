package net.noresttherein.sugar.arrays



import java.lang.System.arraycopy

import scala.Array.UnapplySeqWrapper
import scala.annotation.tailrec
import scala.collection.Stepper.EfficientSplit
import scala.collection.immutable.ArraySeq
import scala.collection.{IterableFactory, Stepper, StepperShape, View, mutable}
import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.reflect.{ClassTag, classTag}

import net.noresttherein.sugar.arrays.RefArrayLike.extensions.RefArrayLikeExtensionConversion
import net.noresttherein.sugar.arrays.RefArrayLike.extensions.RefArrayLikeExtensionConversion
import net.noresttherein.sugar.arrays.extensions.{ArrayExtension, ArrayLikeExtension, ArrayObjectExtension, MutableArrayExtension}
import net.noresttherein.sugar.collections.{ArrayIterableOnce, ArrayLikeSlice, IArraySlice, IRefArraySlice, MatrixBuffer, RefArraySlice}
import net.noresttherein.sugar.typist.PriorityConversion
import net.noresttherein.sugar.typist.casting.extensions.{cast2TypeParamsMethods, cast3TypeParamsMethods, castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




@SerialVersionUID(Ver)
object RefArrayLike extends IterableFactory.Delegate[RefArrayLike](RefArray) {
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
		@inline def length :Int = self.length
		@inline def apply(i :Int) :E = self(i).asInstanceOf[E]
		@inline def indexOf(elem :E, from :Int = 0) :Int = self.indexOf(elem, from)
		@inline def lastIndexOf(elem :E, end :Int = Int.MaxValue) :Int = self.lastIndexOf(elem, end)
		@inline def contains(elem :E) :Boolean = self.contains(elem)

		@inline def scanLeft[A](z :A)(op :(A, E) => A) :Arr[A] =
			self.scanLeft[Any](z)(op.castParams[Any, Any, Any]).castFrom[Array[Any], Arr[A]]

		@inline def scanRight[A](z :A)(op :(E, A) => A) :Arr[A] =
			self.scanRight[Any](z)(op.castParams[Any, Any, Any]).castFrom[Array[Any], Arr[A]]

		@inline def scan[A >: E](z :A)(op :(A, A) => A) :Arr[A] =
			self.scan[Any](z)(op.castParams[Any, Any, Any]).castFrom[Array[Any], Arr[A]]

		@inline def collect[A](pf :PartialFunction[E, A]) :Arr[A] =
			self.collect(pf.castParams[Any, Any]).castFrom[Array[Any], Arr[A]]

		@inline def partitionMap[E1, E2](f: E => Either[E1, E2]) :(Arr[E1], Arr[E2]) =
			self.partitionMap(f.castParams[Any, Either[Any, Any]]).castFrom[(Array[Any], Array[Any]), (Arr[E1], Arr[E2])]

		@inline def map[A](f :E => A) :Arr[A] = self.map(f.castParams[Any, Any]).castFrom[Array[Any], Arr[A]]
		@inline def flatMap[A](f :E => IterableOnce[A]) :Arr[A] =
			self.flatMap(f.castParams[Any, IterableOnce[Any]]).castFrom[Array[Any], Arr[A]]

		@inline def flatMap[As, A](f :E => As)(implicit asIterable :As => Iterable[A]) :Arr[A] =
			self.flatMap(f.castParams[Any, Iterable[Any]]).castFrom[Array[Any], Arr[A]]

		@inline def flatten[A](implicit asIterable :E => IterableOnce[A]) :Arr[A] =
			self.flatten(asIterable.castParams[Any, IterableOnce[Any]], ClassTag.Any).castFrom[Array[Any], Arr[A]]

		@inline def groupMap[K, A](key :E => K)(f :E => A) :Map[K, Arr[A]] =
			self.groupMap[K, Any](key.castParams[Any, K])(f.castParams[Any, Any])
			     .castFrom[Map[K, Array[Any]], Map[K, Arr[A]]]


		@inline def unzip[E1, E2](implicit asPair :E => (E1, E2)) :(Arr[E1], Arr[E2]) =
			self.unzip(asPair.castParams[Any, (Any, Any)], ClassTag.Any, ClassTag.Any)
			     .castFrom[(Array[Any], Array[Any]), (Arr[E1], Arr[E2])]

		@inline def unzip3[E1, E2, E3](implicit asTriple :E => (E1, E2, E3)) :(Arr[E1], Arr[E2], Arr[E3]) =
			self.unzip3(asTriple.castParams[Any, (Any, Any, Any)], ClassTag.Any, ClassTag.Any, ClassTag.Any)
			     .castFrom[(Array[Any], Array[Any], Array[Any]), (Arr[E1], Arr[E2], Arr[E3])]

		@inline def updated[A >: E](index :Int, elem :A) :Arr[A] =
			genericArrayOps(self).updated(index, elem).castFrom[Array[Any], Arr[A]]

		/** An 'exploded' variant of `updatedAll`: a copy of this array with elements starting at `index` substituted
		  * by `first`, `second`, and contents of `rest`. The index must lie in range `[0..length - rest.size - 2)`.
		  */
		@inline def updatedAll[A >: E](index :Int, first :A, second :A, rest :A*) :Arr[A] =
			self.updatedAll[Any](index, first, second, rest :_*).castFrom[Array[Any], Arr[A]]

		/** Updates consecutive elements of this array, starting with the specified index,
		  * with elements from the collection. Equivalent to
		  * [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.patch patch]]`(index, elems, elems.size)`,
		  * but more efficient due to a single array allocation.
		  */
		@inline def updatedAll[A >: E](index :Int, elems :IterableOnce[A]) :Arr[A] =
			self.updatedAll[Any](index, elems).castFrom[Array[Any], Arr[A]]

		/** Updates consecutive elements of this array, starting with the specified index,
		  * with elements from the collection. Equivalent to
		  * [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.patch patch]]`(index, elems, elems.size)`,
		  * but more efficient due to a single array allocation.
		  */
		@inline def updatedAll[A >: E](index :Int, elems :ArrayLike[A]) :Arr[A] =
			self.updatedAll[Any](index, elems).castFrom[Array[Any], Arr[A]]

		/** A copy of this array, with the element inserted at a given position in this array,
		  * and all elements at positions equal or greater than `index` by one element further.
		  * This is equivalent to [[net.noresttherein.sugar.arrays.ArrayLike.RefArrayLikeExtension.patch patch]]
		  * with a singleton collection and `replaced` equal to zero, but the index
		  * must be in the valid range for this array.
		  * @return `take(index) :+ elem :++ drop(index)`, but in a more efficient manner.
		  * @throws IndexOutOfBoundsException if `index` < 0 or `index > length`.
		  */
		@inline def inserted[A >: E](index :Int, elem :A) :Arr[A] =
			self.inserted[Any](index, elem).castFrom[Array[Any], Arr[A]]

		/** An 'exploded' variant of `updatedAll`: a copy of this array with elements starting at `index` substituted
		  * by `first`, `second`, and contents of `rest`. The index must lie in range `[0..length)`.
		  */
		@inline def insertedAll[A >: E](index :Int, first :A, second :A, rest :A*) :Arr[A] =
			self.insertedAll[Any](index, first, second, rest :_*).castFrom[Array[Any], Arr[A]]

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * @return [[net.noresttherein.sugar.arrays.ArrayLike.RefArrayLikeExtension.patch patch]]`(index, elems, 0)`.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		@inline def insertedAll[A >: E](index :Int, elems :IterableOnce[A]) :Arr[A] =
			self.insertedAll[Any](index, elems).castFrom[Array[Any], Arr[A]]

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * @return [[net.noresttherein.sugar.arrays.ArrayLike.RefArrayLikeExtension.patch patch]]`(index, elems, 0)`.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		@inline def insertedAll[A >: E](index :Int, elems :ArrayLike[A]) :Arr[A] =
			self.insertedAll[Any](index, elems).castFrom[Array[Any], Arr[A]]

		@inline def :+[A >: E](x :A) :Arr[A] = self.appended[Any](x).castFrom[Array[Any], Arr[A]]
		@inline def +:[A >: E](x :A) :Arr[A] = self.prepended[Any](x).castFrom[Array[Any], Arr[A]]

		@inline def appended[A >: E](x :A) :Arr[A] = self.appended[Any](x).castFrom[Array[Any], Arr[A]]
		@inline def prepended[A >: E](x :A) :Arr[A] = self.prepended[Any](x).castFrom[Array[Any], Arr[A]]

		@inline def appendedAll[A >: E](first :A, second :A, rest :A*) :Arr[A] =
			self.appendedAll[Any](first, second,  rest :_*).castFrom[Array[Any], Arr[A]]

		@inline def prependedAll[A >: E](first :A, second :A, rest :A*) :Arr[A] =
			self.prependedAll[Any](first, second, rest :_*).castFrom[Array[Any], Arr[A]]

		@inline def concat[A >: E](suffix :IterableOnce[A]) :Arr[A] = appendedAll(suffix)
		@inline def concat[A >: E](suffix :ArrayLike[A]) :Arr[A] = appendedAll(suffix)

		@inline def ++[A >: E](suffix :IterableOnce[A]) :Arr[A] = appendedAll(suffix)
		@inline def ++[A >: E](suffix :ArrayLike[A]) :Arr[A] = appendedAll(suffix)

		@inline def :++[A >: E](suffix :IterableOnce[A]) :Arr[A] = appendedAll(suffix)
		@inline def :++[A >: E](suffix :ArrayLike[A]) :Arr[A] = appendedAll(suffix)

		@inline def appendedAll[A >: E](suffix :IterableOnce[A]) :Arr[A] =
			self.appendedAll[Any](suffix).castFrom[Array[Any], Arr[A]]

		@inline def appendedAll[A >: E](suffix :ArrayLike[A]) :Arr[A] =
			RefArray.copyOfRanges(self, 0, self.length, suffix, 0, suffix.length).asInstanceOf[Arr[A]]
/*
		{
			val res = new Array[Any](array.length + suffix.length)
			arraycopy(array, 0, res, 0, array.length)
			arraycopy(suffix, 0, res, array.length, suffix.length)
			res.castFrom[Array[Any], Arr[A]]
		}
*/

		@inline def ++:[A >: E](prefix :IterableOnce[A]) :Arr[A] = prependedAll(prefix)
		@inline def ++:[A >: E](prefix :ArrayLike[A]) :Arr[A] = prependedAll(prefix)

		@inline def prependedAll[A >: E](prefix :IterableOnce[A]) :Arr[A] =
			self.prependedAll[Any](prefix).castFrom[Array[Any], Arr[A]]

		@inline def prependedAll[A >: E](prefix :ArrayLike[A]) :Arr[A] =
			RefArray.copyOfRanges(prefix, 0, prefix.length, self, 0, self.length).asInstanceOf[Arr[A]]
/*
		{
			val prefixLength = prefix.length
			val res = new Array[Any](array.length + prefixLength)
			arraycopy(prefix, 0, res, 0, prefixLength)
			arraycopy(array, 0, res, prefixLength, array.length)
			res.castFrom[Array[Any], Arr[A]]
		}
*/

		def padTo[A >: E](len :Int, elem :A) :Arr[A] =
			if (len <= self.length)
				self.castFrom[Array[Any], Arr[A]]
			else {
				val res = new Array[Any](len)
				var i = self.length
				arraycopy(self, 0, res, 0, i)
				while (i < len) {
					res(i) = elem
					i += 1
				}
				res.castFrom[Array[Any], Arr[A]]
			}

		@inline def patch[A >: E](from :Int, other :IterableOnce[A], replaced :Int) :Arr[A] =
			self.patch[Any](from, other, replaced).castFrom[Array[Any], Arr[A]]

		@inline def patch[A >: E](from :Int, other :ArrayLike[A], replaced :Int) :Arr[A] = {
			val clippedFrom     = math.max(from, 0)
			val clippedReplaced = math.min(math.max(replaced, 0), self.length - clippedFrom)
			val until           = clippedFrom + clippedReplaced
			ErasedArray.copyOfRanges(
				self, 0, from, other, 0, other.length, self, until, self.length
			).castFrom[Array[Any], Arr[A]]
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

		def unapply[A](elems :IterableOnce[A]) :Opt[RefArrayLike[A]] = {
			val array = elems match {
				case seq :ArraySeq[_] => seq.unsafeArray
				case seq :mutable.ArraySeq[_] => seq.array
				case seq :Vector[_] if seq.length == CheatedAccess.FlatVectorSize =>
					CheatedAccess.array(seq)
				case seq :ArrayBuffer[_] if seq.length == CheatedAccess.array(seq).length =>
					CheatedAccess.array(seq)
				case slice :ArrayIterableOnce[_] if slice.knownSize == slice.unsafeArray.length =>
					slice.unsafeArray
				case seq :MatrixBuffer[_] if seq.dim == 1 && seq.startIndex == 0 => seq.data1
				case _ => null
			}
			if (array != null && array.getClass == classOf[Array[AnyRef]])
				Got(array.castFrom[Array[_], IRefArray[A]])
			else
				Lack
		}

		/** Wraps and unwraps both mutable and immutable scala collections backed by consecutive sections
		  * of `Array[AnyRef]` arrays in a safe manner. $warning
		  */
		@SerialVersionUID(Ver)
		object Slice {
			def apply[A](array :RefArrayLike[A], from :Int, until :Int) :collection.IndexedSeq[A] =
				ArrayLikeSlice.slice(array, from, until)

			def unapply[A](elems :IterableOnce[A]) :Opt[(RefArrayLike[A], Int, Int)] = elems match {
				case seq :ArraySeq[_] if seq.unsafeArray.getClass == classOf[Array[AnyRef]] =>
					Got((seq.unsafeArray.castFrom[Array[_], RefArrayLike[A]], 0, seq.unsafeArray.length))
				case seq :mutable.ArraySeq[_] if seq.array.getClass == classOf[Array[AnyRef]] =>
					Got((seq.array.castFrom[Array[_], RefArrayLike[A]], 0, seq.length))
				case seq :Vector[_] if seq.length <= CheatedAccess.FlatVectorSize =>
					Got((CheatedAccess.array(seq).castFrom[Array[AnyRef], RefArrayLike[A]], 0, seq.length))
				case arr :ArrayIterableOnce[_] =>
					val array = arr.unsafeArray.castFrom[Array[_], RefArrayLike[A]]
					if (array.getClass == classOf[Array[AnyRef]])
						Got((array, arr.startIndex, arr.startIndex + arr.knownSize))
					else
						Lack
				case seq :MatrixBuffer[_] if seq.dim == 1 =>
					val array = seq.data1.castFrom[Array[_], RefArrayLike[A]]
					val start = seq.startIndex
					val end   = start + seq.length
					if (array.getClass == classOf[Array[AnyRef]] && end <= array.asInstanceOf[Array[AnyRef]].length)
						Got((array, start, end))
					else
						Lack
				case _ =>
					Lack
			}
		}
	}


	implicit def RefArrayLikeClassTag[X, Arr[X] <: RefArrayLike[X]] :ClassTag[Arr[X]] =
		ArrayAnyClassTag.asInstanceOf[ClassTag[Arr[X]]]

	private[this] val ArrayAnyClassTag = classTag[Array[Any]]

	/** Mixin trait with extension methods conversion for `RefArrayLike` subtypes.
	  * @define Coll `RefArrayLike`
	  * @define Extension `RefArrayLikeExtension[E, Arr]`
	  */
	private[arrays] trait extensions extends Any with ArrayLike.extensions {
//		@inline implicit final def RefArrayLikeExtension[Arr[X] <: RefArrayLike[X], A](self :RefArrayLike[A])
//				:RefArrayLikeExtension[Arr, A] =
//			new RefArrayLikeExtension(self.asInstanceOf[Array[_]])
		/** Extension methods for all `RefArrayLike[E]` subtypes.
		  * $conversionInfo 
		  */
		implicit final def RefArrayLikeExtension[Arr[X] <: RefArrayLike[X], E] :RefArrayLikeExtensionConversion[Arr, E] =
			extensions.RefArrayLikeExtensionConversionPrototype.asInstanceOf[RefArrayLikeExtensionConversion[Arr, E]]
	}
	
	@SerialVersionUID(Ver)
	object extensions extends extensions {
		sealed trait RefArrayLikeExtensionConversion[Arr[X] <: RefArrayLike[X], E]
			extends (Arr[E] => RefArrayLikeExtension[Arr, E])
		{
			@inline final def apply(v1 :Arr[E])(implicit dummy :DummyImplicit) :RefArrayLikeExtension[Arr, E] =
				new RefArrayLikeExtension(v1.asInstanceOf[Array[Any]])
		}
		private def newRefArrayLikeExtensionConversion[Arr[X] <: RefArrayLike[X], E] =
			new PriorityConversion.Wrapped[Arr[E], RefArrayLikeExtension[Arr, E]](
				(arr :Arr[E]) => new RefArrayLikeExtension(arr.asInstanceOf[Array[Any]])
			) with RefArrayLikeExtensionConversion[Arr, E]
		private val RefArrayLikeExtensionConversionPrototype :RefArrayLikeExtensionConversion[RefArrayLike, Any] =
			newRefArrayLikeExtensionConversion
	}
}






/** Shared implementation of factories of type `Arr`, represented by an `Array[AnyRef]` during execution.
  * @define Coll `RefArrayLike`
  * @define coll object array
  */
private[arrays] abstract class RefArrayLikeFactory[Arr[X] <: ArrayLike[X]] extends IterableFactory[Arr] {
//
//	/** Creates a new `Array[E]` of length `size`, executes the given initialization function for it,
//	  * and returns it as an $Coll`[E]`. It is a pattern for initialization safer than manually creating
//	  * an `Array[E]`, writing to it, and casting it $Coll`[E]`. The application should ''not'' retain a reference
//	  * to the array given as the function argument.
//	  */
//	@inline final def init[E](size :Int)(f :Array[_ >: E] => Unit) :Arr[E] = {
//		val res = new Array[Any](size)
//		f(res.castFrom[Array[Any], Array[E]])
//		res.castFrom[Array[Any], Arr[E]]
//	}

	/** Allocates a new `Array[AnyRef]` and copies all elements from the argument, returning it as a $Coll`[E]`.
	  * If the argument is a value array, the elements will be boxed.
	  */
	@inline final def copyOf[E](array :ArrayLike[E]) :Arr[E] =
		copyOf(array, array.castFrom[ArrayLike[E], Array[_]].length)

	/** Reallocates the given array as a $Coll`[E]` of a new size, and copies `min(newLength, array.length)`
	  * of its first elements. If the argument is a value array, the elements will be boxed.
	  */
	@throws[NegativeArraySizeException]("if newLength is negative")
	final def copyOf[E](array :ArrayLike[E], newLength :Int) :Arr[E] =
		ArrayFactory.copyOf[Any](array, newLength)(ClassTag.Any).asInstanceOf[Arr[E]]
/*
		if (newLength == 0 || array.asInstanceOf[Array[_]].length == 0)
			empty
		else if (array.isInstanceOf[Array[AnyRef]])
			java.util.Arrays.copyOf(
				array.castFrom[ArrayLike[E], Array[AnyRef]], newLength, classOf[Array[AnyRef]]
			).castFrom[Array[AnyRef], Arr[E]]
		else {
			val res = new Array[Any](newLength)
			ArrayLike.copy(array, 0, res, 0, newLength)
			res.castFrom[Array[Any], Arr[E]]
		}
*/

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
		Array.copyOfRange[Any](array, from, until)(ClassTag.Any).castFrom[Array[Any], Arr[E]]

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
		Array.copyOfRanges[Any](array1, from1, until1, array2, from2, until2)(ClassTag.Any).asInstanceOf[Arr[E]]
/*
		if (from1 < 0 | from2 < 0 || from1 > array1.length || from2 > array2.length)
			throw new IndexOutOfBoundsException(
				s"$toString.copyOfRanges(${array1.localClassName}|${array1.length}|, $from1, $until1, " +
					s"${array2.localClassName}|${array2.length}|, $from2, $until2)."
			)
		else {
			val length1 = math.max(from1, until1) - from1
			val length2 = math.max(from2, until2) - from2
			if (length1 + length2 == 0)
				empty[E]
			else {
				val res = new Array[Any](length1 + length2).castFrom[Array[Any], Array[E]]
				ArrayLike.copy(array1, from1, res, 0, length1)
				ArrayLike.copy(array2, from2, res, length1, length2)
				res.castFrom[Array[E], Arr[E]]
			}
		}
*/

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
		Array.copyOfRanges[Any](
			array1, from1, until1, array2, from2, until2, array3, from3, until3
		)(ClassTag.Any).asInstanceOf[Arr[E]]
/*
		if (from1 < 0 | from2 < 0 | from3 < 0 || from1 > array1.length || from2 > array2.length || from3 > array3.length)
			throw new IndexOutOfBoundsException(
				s"$toString.copyOfRanges(${array1.localClassName}|${array1.length}|, $from1, $until1, " +
					s"${array2.localClassName}|${array2.length}|, $from2, $until2, " +
					s"${array3.localClassName}|${array3.length}|, $from3, $until3)."
			)
		else {
			val length1 = math.min(from1, until1) - from1
			val length2 = math.min(from2, until2) - from2
			val length3 = math.min(from3, until3) - from2
			if (length1 + length2 + length3 == 0)
				empty[E]
			else {
				val res     = new Array[Any](length1 + length2 + length3).castFrom[Array[Any], Array[E]]
				ArrayLike.copy(array1, from1, res, 0, length1)
				ArrayLike.copy(array2, from2, res, length1, length2)
				ArrayLike.copy(array3, from3, res, length1 + length2, length3)
				res.castFrom[Array[E], Arr[E]]
			}
		}
*/


	@inline final override def apply[E](elems :E*) :Arr[E] = Array[Any](elems :_*).asInstanceOf[Arr[E]]

	override def from[E](source :IterableOnce[E]) :Arr[E] = source match {
		case empty if empty.knownSize == 0 =>
			Array.emptyObjectArray.castFrom[Array[AnyRef], Arr[E]]
		case items :Iterable[E] =>
			items.toArray[Any].castFrom[Array[Any], Arr[E]]
		case _                  =>
			source.iterator.toArray[Any].castFrom[Array[Any], Arr[E]]
	}

	@inline final def from[E](array :ArrayLike[E]) :Arr[E] = copyOf(array)

	@inline final override def empty[E] :Arr[E] = Array.emptyObjectArray.castFrom[Array[AnyRef], Arr[E]]

	final override def newBuilder[E] :Builder[E, Arr[E]] = Array.newBuilder(ClassTag.Any).castParam2[Arr[E]]

	/** Boxes the element if necessary, places it in a singleton `Array[AnyRef]`, and returns it as a $Coll`[E]`. */
	final def one[E](elem :E) :Arr[E] = {
		val a = new Array[Any](1)
		a(0) = elem
		a.castFrom[Array[Any], Arr[E]]
	}

	/** Boxes the elements, if necessary, places them in an `Array[AnyRef]` of length 2, and returns as a $Coll`[E]`. */
	final def two[E](first :E, second :E) :Arr[E] = {
		val a = new Array[Any](2)
		a(0) = first
		a(1) = second
		a.castFrom[Array[Any], Arr[E]]
	}


	/** A $coll filled with `n` copies of `elem`. */
	final def const[X](n :Int)(elem :X) :Arr[X] = {
		val res = new Array[Any](n)
		res.fill(elem)
		res.castFrom[Array[Any], Arr[X]]
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
		Array.generate[Any](start)(next.castParams[Any, Any]).castFrom[Array[Any], Arr[X]]

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
		Array.expand[Any](start)(next.castParams[Any, Option[Any]]).castFrom[Array[Any], Arr[X]]

	/** Similar to [[collection.IterableFactory.iterate iterate]],
	  * but the iterating function accepts the positional index of the next element as an additional argument.
	  * @param start The first element of the created array.
	  * @param len   The size of the created array.
	  * @param f     A function generating subsequent elements following start.
	  *              The second element of the array will be `f(start, 1)`, the third `f(f(start, 1), 2)`, and so on.
	  */
	@inline final def iterateWithIndex[X](start :X, len :Int)(f :(X, Int) => X) :IArray[X] =
		Array.iterateWithIndex[Any](start, len)(f.castParam1[Any]).castFrom[Array[Any], IArray[X]]

	/** A stepper iterating over the range of an array. Indices out of range are skipped silently. */
	@inline final def stepper[A, S <: Stepper[_]]
	                         (array :IArray[A], from :Int = 0, until :Int = Int.MaxValue)
	                         (implicit shape :StepperShape[A, S]) :S with EfficientSplit =
		Array.stepper(array.castFrom[IArray[A], Array[A]], from, until)



	@inline final def unapplySeq[E](array :Arr[E]) :UnapplySeqWrapper[E] =
		new UnapplySeqWrapper(array.castFrom[Arr[E], Array[E]])
//
//	protected final def isSeq[E] :IsSeq[Arr[E]] { type A = E; type C = Arr[E] } =
//		IsSeqPrototype.castFrom[IsSeq[Arr[Any]], IsSeq[Arr[E]] { type A = E; type C = Arr[E] }]
//
//	private[this] val IsSeqPrototype =
//		new ArrayLikeIsSeqTemplate[Any, collection.Seq, Arr] {
//			override def apply(array :Arr[Any]) =
//				new ArrayLikeIsSeqOps[Any, Arr](array.asInstanceOf[Array[Any]]) {
//					override def fromSpecific(coll :IterableOnce[Any]) :Arr[Any] = RefArrayLikeFactory.this.from(coll)
//					override def newSpecificBuilder :Builder[Any, Arr[Any]] = newBuilder
//				}
//			private def readResolve = RefArrayLikeFactory.this.isSeq
//			override lazy val toString = RefArrayLikeFactory.this.toString + "IsSeq"
//		}

}






/** Factory of erased arrays: `Array[E]` represented in runtime by an `Array[Any]`.
  * ''This will work only in generic contexts: '' `ErasedArray.ofDim[Int](1)` will throw a [[ClassCastException]].
  * @define Coll `Array`
  * @define coll array
  */
@SerialVersionUID(Ver)
private[noresttherein] case object ErasedArray extends RefArrayLikeFactory[Array] with IterableFactory[Array] {

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

		def unapply[A](elems :IterableOnce[A]) :Opt[Array[_]] = elems match {
			case seq :ArrayIterableOnce[_] if seq.knownSize == seq.unsafeArray.length => Got(seq.unsafeArray)
			case _ :collection.IndexedSeq[_] => elems match {
				case seq :ArraySeq[_]                                                     => Got(seq.unsafeArray)
				case seq :mutable.ArraySeq[_]                                             => Got(seq.array)
				case seq :ArrayBuffer[_] if CheatedAccess.array(seq).length == seq.length => Got(CheatedAccess.array(seq))
				case seq :Vector[_] if seq.length == CheatedAccess.FlatVectorSize         => Got(CheatedAccess.array(seq))
				case _                                                                    => Lack
			}
			case _ => Lack
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

			def unapply[A](elems :IterableOnce[A]) :Opt[(Array[_], Int, Int)] = elems match {
				case seq :ArrayIterableOnce[_] =>
					val size   = seq.knownSize
					val offset = seq.startIndex
					if (size >= 0)
						Got((seq.unsafeArray, offset, offset + size))
					else
						Lack
				case _ :collection.IndexedSeqOps[_, _, _] => elems match {
					case seq :ArraySeq[_]         => Got((seq.unsafeArray, 0, seq.unsafeArray.length))
					case seq :Vector[_] if seq.length <= CheatedAccess.FlatVectorSize =>
						Got((CheatedAccess.array(seq), 0, seq.length))
					case seq :mutable.ArraySeq[_] => Got((seq.array, 0, seq.array.length))
					case seq :ArrayBuffer[_]      => Got((CheatedAccess.array(seq), 0, seq.length))
//					case seq :MatrixBuffer[_] if seq.dim == 1 && seq.startIndex + seq.length <= seq.data1.length =>
//						Got((seq.data1, seq.startIndex, seq.startIndex + seq.length))
					case _                        => Lack
				}
				case _ => Lack
			}
		}
	}
}
