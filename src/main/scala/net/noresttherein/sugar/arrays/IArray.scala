package net.noresttherein.sugar.arrays

import scala.Array.UnapplySeqWrapper
import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.IsSeq
import scala.collection.{ArrayOps, ClassTagIterableFactory, Stepper, StepperShape, View}
import scala.collection.immutable.{ArraySeq, IndexedSeqOps}
import scala.collection.mutable.Builder
import scala.reflect.{ClassTag, classTag}

import net.noresttherein.sugar.arrays.IArray.extensions.IArrayExtensionConversion
import net.noresttherein.sugar.arrays.extensions.{ArrayExtension, ArrayLikeExtension, ArrayCompanionExtension, MutableArrayExtension}
import net.noresttherein.sugar.collections.{ArrayIterableOnce, ArrayLikeSliceWrapper, ArrayLikeWrapper, IArrayLikeSlice, IArraySlice, RelayArrayFactory}
import net.noresttherein.sugar.reflect.extensions.ClassExtension
import net.noresttherein.sugar.typist.PriorityConversion
import net.noresttherein.sugar.typist.casting.extensions.{cast2TypeParamsMethods, castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/** Factory of immutable arrays: `IArray` backed by an `Array`, without exposing any mutating methods.
  * Aside from standard factory methods inherited from `ClassTagIterableFactory`, and those modelled after
  * factory methods in [[Array]] and [[java.util.Arrays]], there is another convenient way of initializing
  * a new instance without a need for an intermediate collection in method
  * [[net.noresttherein.sugar.arrays.IArray.init init]], which grants a temporary access to the new object
  * as a regular `Array`:
  * {{{
  *     IArray.init[String](3){ array =>
  *         array(0) = "You"
  *         array(1) = "Boo"
  *         array(2) = "I"
  *     }
  * }}}
  * @see [[net.noresttherein.sugar.arrays.IArray! IArray]]
  * @define Coll `IArray`
  * @define coll immutable array
  */ //todo: make copies of all new copyOfRange(s) methods in ArrayCompanionExtension
@SerialVersionUID(Ver)
case object IArray extends ClassTagIterableFactory[IArray] {

	/** Accessor extension methods for [[net.noresttherein.sugar.arrays.IArray IArray]]`[Byte]`.
	  * Enabled by importing any of
	  * `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.ByteIArrayExtension ByteIArrayExtension]],
	  * `net.noresttherein.sugar.arrays.IArray.extensions.ByteIArrayExtension`,
	  * or `net.noresttherein.sugar.extensions.ByteIArrayExtension`.
	  * @see [[net.noresttherein.sugar.arrays.IArray.IArrayExtension]]
	  * @see [[net.noresttherein.sugar.arrays.IArrayLike.IArrayLikeExtension]]
	  * @see [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension]]
	  */
	@inline class ByteIArrayExtension private[arrays] (private val array :Array[Byte]) extends AnyVal {
		@inline def length :Int = array.length
		@inline def head :Byte = array(0)
		@inline def last :Byte = array(array.length - 1)
		@inline def apply(n :Int) :Byte = array(n)
		@inline def toArraySeq :ArraySeq.ofByte = new ArraySeq.ofByte(array)
	}

	/** Accessor extension methods for [[net.noresttherein.sugar.arrays.IArray IArray]]`[Short]`.
	  * Enabled by importing any of
	  * `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.ShortIArrayExtension ShortIArrayExtension]],
	  * `net.noresttherein.sugar.arrays.IArray.extensions.ShortIArrayExtension`,
	  * or `net.noresttherein.sugar.extensions.ShortIArrayExtension`.
	  * @see [[net.noresttherein.sugar.arrays.IArray.IArrayExtension]]
	  * @see [[net.noresttherein.sugar.arrays.IArrayLike.IArrayLikeExtension]]
	  * @see [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension]]
	  */
	@inline class ShortIArrayExtension private[arrays] (private val array :Array[Short]) extends AnyVal {
		@inline def length :Int = array.length
		@inline def head :Short = array(0)
		@inline def last :Short = array(array.length - 1)
		@inline def apply(n :Int) :Short = array(n)
		@inline def toArraySeq :ArraySeq.ofShort = new ArraySeq.ofShort(array)
	}

	/** Accessor extension methods for [[net.noresttherein.sugar.arrays.IArray IArray]]`[Char]`.
	  * Enabled by importing any of
	  * `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.CharIArrayExtension CharIArrayExtension]],
	  * `net.noresttherein.sugar.arrays.IArray.extensions.CharIArrayExtension`,
	  * or `net.noresttherein.sugar.extensions.CharIArrayExtension`.
	  * @see [[net.noresttherein.sugar.arrays.IArray.IArrayExtension]]
	  * @see [[net.noresttherein.sugar.arrays.IArrayLike.IArrayLikeExtension]]
	  * @see [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension]]
	  */
	@inline class CharIArrayExtension private[arrays] (private val array :Array[Char]) extends AnyVal {
		@inline def length :Int = array.length
		@inline def head :Char = array(0)
		@inline def last :Char = array(array.length - 1)
		@inline def apply(n :Int) :Char = array(n)
		@inline def toArraySeq :ArraySeq.ofChar = new ArraySeq.ofChar(array)
	}

	/** Accessor extension methods for [[net.noresttherein.sugar.arrays.IArray IArray]]`[Int]`.
	  * Enabled by importing any of
	  * `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.IntIArrayExtension IntIArrayExtension]],
	  * `net.noresttherein.sugar.arrays.IArray.extensions.IntIArrayExtension`,
	  * or `net.noresttherein.sugar.extensions.IntIArrayExtension`.
	  * @see [[net.noresttherein.sugar.arrays.IArray.IArrayExtension]]
	  * @see [[net.noresttherein.sugar.arrays.IArrayLike.IArrayLikeExtension]]
	  * @see [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension]]
	  */
	@inline class IntIArrayExtension private[arrays] (private val array :Array[Int]) extends AnyVal {
		@inline def length :Int = array.length
		@inline def head :Int = array(0)
		@inline def last :Int = array(array.length - 1)
		@inline def apply(n :Int) :Int = array(n)
		@inline def toArraySeq :ArraySeq.ofInt = new ArraySeq.ofInt(array)
	}

	/** Accessor extension methods for [[net.noresttherein.sugar.arrays.IArray IArray]]`[Long]`.
	  * Enabled by importing any of
	  * `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.LongIArrayExtension LongIArrayExtension]],
	  * `net.noresttherein.sugar.arrays.IArray.extensions.LongIArrayExtension`,
	  * or `net.noresttherein.sugar.extensions.LongIArrayExtension`.
	  * @see [[net.noresttherein.sugar.arrays.IArray.IArrayExtension]]
	  * @see [[net.noresttherein.sugar.arrays.IArrayLike.IArrayLikeExtension]]
	  * @see [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension]]
	  */
	@inline class LongIArrayExtension private[arrays] (private val array :Array[Long]) extends AnyVal {
		@inline def length :Int = array.length
		@inline def head :Long = array(0)
		@inline def last :Long = array(array.length - 1)
		@inline def apply(n :Int) :Long = array(n)
		@inline def toArraySeq :ArraySeq.ofLong = new ArraySeq.ofLong(array)
	}

	/** Accessor extension methods for [[net.noresttherein.sugar.arrays.IArray IArray]]`[Float]`.
	  * Enabled by importing any of
	  * `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.FloatIArrayExtension FloatIArrayExtension]],
	  * `net.noresttherein.sugar.arrays.IArray.extensions.FloatIArrayExtension`,
	  * or `net.noresttherein.sugar.extensions.FloatIArrayExtension`.
	  * @see [[net.noresttherein.sugar.arrays.IArray.IArrayExtension]]
	  * @see [[net.noresttherein.sugar.arrays.IArrayLike.IArrayLikeExtension]]
	  * @see [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension]]
	  */
	@inline class FloatIArrayExtension private[arrays] (private val array :Array[Float]) extends AnyVal {
		@inline def length :Int = array.length
		@inline def head :Float = array(0)
		@inline def last :Float = array(array.length - 1)
		@inline def apply(n :Int) :Float = array(n)
		@inline def toArraySeq :ArraySeq.ofFloat = new ArraySeq.ofFloat(array)
	}

	/** Accessor extension methods for [[net.noresttherein.sugar.arrays.IArray IArray]]`[Double]`.
	  * Enabled by importing any of
	  * `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.DoubleIArrayExtension DoubleIArrayExtension]],
	  * `net.noresttherein.sugar.arrays.IArray.extensions.DoubleIArrayExtension`,
	  * or `net.noresttherein.sugar.extensions.DoubleIArrayExtension`.
	  * @see [[net.noresttherein.sugar.arrays.IArray.IArrayExtension]]
	  * @see [[net.noresttherein.sugar.arrays.IArrayLike.IArrayLikeExtension]]
	  * @see [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension]]
	  */
	@inline class DoubleIArrayExtension private[arrays] (private val array :Array[Double]) extends AnyVal {
		@inline def length :Int = array.length
		@inline def head :Double = array(0)
		@inline def last :Double = array(array.length - 1)
		@inline def apply(n :Int) :Double = array(n)
		@inline def toArraySeq :ArraySeq.ofDouble = new ArraySeq.ofDouble(array)
	}

	/** Accessor extension methods for [[net.noresttherein.sugar.arrays.IArray IArray]]`[Boolean]`.
	  *  Enabled by importing any of
	  * `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.BooleanIArrayExtension BooleanIArrayExtension]],
	  * `net.noresttherein.sugar.arrays.IArray.extensions.BooleanIArrayExtension`,
	  * or `net.noresttherein.sugar.extensions.BooleanIArrayExtension`.
	  * @see [[net.noresttherein.sugar.arrays.IArray.IArrayExtension]]
	  * @see [[net.noresttherein.sugar.arrays.IArrayLike.IArrayLikeExtension]]
	  * @see [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension]]
	  */
	@inline class BooleanIArrayExtension private[arrays] (private val array :Array[Boolean]) extends AnyVal {
		@inline def length :Int = array.length
		@inline def head :Boolean = array(0)
		@inline def last :Boolean = array(array.length - 1)
		@inline def apply(n :Int) :Boolean = array(n)
		@inline def toArraySeq :ArraySeq.ofBoolean = new ArraySeq.ofBoolean(array)
	}

	/** Accessor extension methods for [[net.noresttherein.sugar.arrays.IArray IArray]]`[T]`
	  * for some reference type `T <: AnyRef`. Enabled by importing any of
	  * `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.RefIArrayExtension RefIArrayExtension]],
	  * `net.noresttherein.sugar.arrays.IArray.extensions.RefIArrayExtension`,
	  * or `net.noresttherein.sugar.extensions.RefIArrayExtension`.
	  * @see [[net.noresttherein.sugar.arrays.IArray.IArrayExtension]]
	  * @see [[net.noresttherein.sugar.arrays.IArrayLike.IArrayLikeExtension]]
	  * @see [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension]]
	  */
	@inline class RefIArrayExtension[E <: AnyRef] private[arrays] (private val array :Array[E]) extends AnyVal {
		@inline def length :Int = array.length
		@inline def head :E = array(0)
		@inline def last :E = array(array.length - 1)
		@inline def apply(n :Int) :E = array(n)
		@inline def toArraySeq :ArraySeq.ofRef[E] = new ArraySeq.ofRef(array)
	}
//
//	/** Accessor extension methods for [[net.noresttherein.sugar.arrays.IArray IArray]]`[T]` for some type `T`.
//	  * Enabled by importing any of
//	  * `net.noresttherein.sugar.collections.extensions.`[[net.noresttherein.sugar.collections.extensions.GenericIArrayExtension GenericIArrayExtension]],
//	  * `net.noresttherein.sugar.arrays.IArray.extensions.GenericIArrayExtension`,
//	  * or `net.noresttherein.sugar.extensions.GenericIArrayExtension`.
//	  * @see [[net.noresttherein.sugar.arrays.IArray.IArrayExtension]]
//	  * @see [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension]]
//	  * @tparam E the component type of the elements in the underlying array.
//	  */ //consider: do we really need it? ArrayLikeExtension does not work? Is it only for toArraySeq?
//	class GenericIArrayExtension[E] private[arrays] (private val array :Array[E]) extends AnyVal {
//		@inline def head :E = array(0)
//		@inline def last :E = array(array.length - 1)
//		@inline def apply(n :Int) :E = array(n)
//		@inline def toArraySeq :ArraySeq[E] = ArraySeq.unsafeWrapArray(array)
//	}


	/** Extension methods specific to [[net.noresttherein.sugar.arrays.IArray IArray]] only.
	  * Contains the subset of [[collection.ArrayOps ArrayOps]] methods which create a new array of a different type,
	  * requiring a [[scala.reflect.ClassTag ClassTag]] as an implicit parameter, and those which refer
	  * to the underlying `Array[E]` in a manner which could cause a `ClassCastException` for a
	  * [[net.noresttherein.sugar.arrays.RefArray RefArray]]
	  * or a [[net.noresttherein.sugar.arrays.IRefArray IRefArray]].
	  * Implicit conversion can be enabled by importing one of
	  * `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.IArray.extensions.IArrayExtension IArrayExtension]],
	  * `net.noresttherein.sugar.arrays.IArray.extensions.IArrayExtension`,
	  * or `net.noresttherein.sugar.extensions.IArrayExtension`.
	  * @see [[net.noresttherein.sugar.arrays.IArrayLike.IArrayLikeExtension]]
	  * @see [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension]]
	  * @tparam E the component type of the elements in the underlying array.
	  */
	class IArrayExtension[E] private[arrays] (private[IArray] val self :Array[E])
		extends AnyVal
	{
		/** Returns `getClass.`[[java.lang.Class.getComponentType getComponentType]]. */
		@inline def getComponentType :Class[E] = self.getClass.getComponentType.castParam[E]
		//clashes with conversion to ArraySeq in predef

		/** Returns `getClass.`[[java.lang.Class.getComponentType getComponentType]] as a `ClassTag`. */
		@inline def getComponentClassTag :ClassTag[E] =
			if (self.getClass.getComponentType eq classOf[Any]) ClassTag.Any.castParam[E]
			else ClassTag[E](self.getClass.getComponentType)

		@inline def collect[A :ClassTag](pf :PartialFunction[E, A]) :IArray[A] =
			new ArrayOps(self).collect(pf).castFrom[Array[A], IArray[A]]

		@inline def partitionMap[E1: ClassTag, E2: ClassTag](f: E => Either[E1, E2]) :(IArray[E1], IArray[E2]) =
			new ArrayOps(self).partitionMap(f).castFrom[(Array[E1], Array[E2]), (IArray[E1], IArray[E2])]

		@inline def withFilter(p :E => Boolean) :WithFilter[E] = new WithFilter(p, self)

		@inline def map[A :ClassTag](f :E => A) :IArray[A] = new ArrayOps(self).map(f).castFrom[Array[A], IArray[A]]
		@inline def flatMap[A :ClassTag](f :E => IterableOnce[A]) :IArray[A] =
			new ArrayOps(self).flatMap(f).castFrom[Array[A], IArray[A]]

		@inline def flatMap[As, A](f :E => As)(implicit asIterable :As => Iterable[A], m :ClassTag[A]) :IArray[A] =
			new ArrayOps(self).flatMap(f).castFrom[Array[A], IArray[A]]

		@inline def flatten[A :ClassTag](implicit asIterable :E => IterableOnce[A]) :IArray[A] =
			new ArrayOps(self).flatten.castFrom[Array[A], IArray[A]]

		@inline def groupMap[K, A :ClassTag](key :E => K)(f :E => A) :Map[K, IArray[A]] =
			new ArrayOps(self).groupMap(key)(f).castFrom[Map[K, Array[A]], Map[K, IArray[A]]]

		@inline def unzip[E1, E2](implicit asPair :E => (E1, E2), ct1 :ClassTag[E1], ct2 :ClassTag[E2])
				:(IArray[E1], IArray[E2]) =
			new ArrayOps(self).unzip.castFrom[(Array[E1], Array[E2]), (IArray[E1], IArray[E2])]

		@inline def unzip3[E1, E2, E3]
			              (implicit asTriple :E => (E1, E2, E3), ct1 :ClassTag[E1], ct2 :ClassTag[E2], ct3 :ClassTag[E3])
		        :(IArray[E1], IArray[E2], IArray[E3]) =
			new ArrayOps(self).unzip3.castFrom[(Array[E1], Array[E2], Array[E3]), (IArray[E1], IArray[E2], IArray[E3])]
/*

		@inline def take(n :Int) :IArray[E] =
			if (n <= 0) empty(array.getClass.getComponentType.castParam[E])
			else if (n >= array.length) array.castFrom[Array[E], IArray[E]]
			else array.take(n).castFrom[Array[E], IArray[E]]

		@inline def drop(n :Int) :IArray[E] =
			if (n <= 0) array.castFrom[Array[E], IArray[E]]
			else if (n >= array.length) empty(array.getClass.getComponentType.castParam[E])
			else array.drop(n).castFrom[Array[E], IArray[E]]

		@inline def takeRight(n :Int) :IArray[E] =
			if (n <= 0) empty(array.getClass.getComponentType.castParam[E])
			else if (n >= array.length) array.castFrom[Array[E], IArray[E]]
			else array.drop(array.length - n).castFrom[Array[E], IArray[E]]

		@inline def dropRight(n :Int) :IArray[E] =
			if (n <= 0) array.castFrom[Array[E], IArray[E]]
			else if (n >= array.length) empty(array.getClass.getComponentType.castParam[E])
			else array.take(array.length - n).castFrom[Array[E], IArray[E]]

		@inline def takeWhile(p :E => Boolean) :IArray[E] = take(array.segmentLength(p.asInstanceOf[Any => Boolean]))
		@inline def dropWhile(p :E => Boolean) :IArray[E] = drop(array.segmentLength(p.asInstanceOf[Any => Boolean]))
		@inline def slice(from :Int, until :Int) :IArray[E] =
			if (until <= from | until < 0 || from >= array.length) empty(array.getClass.getComponentType.castParam[E])
			else if (from <= 0 && until >= array.length) array.castFrom[Array[E], IArray[E]]
			else array.slice(from, until).castFrom[Array[E], IArray[E]]

		@inline def splitAt(n :Int) :(IArray[E], IArray[E]) = (take(n), drop(n))

		@inline def span(p :E => Boolean) :(IArray[E], IArray[E]) =
			array.segmentLength(p.asInstanceOf[Any => Boolean]) match {
				case 0 =>
					(IArray.empty(array.getClass.getComponentType.castParam[E]), array.castFrom[Array[E], IArray[E]])
				case n if n == array.length =>
					(array.castFrom[Array[E], IArray[E]], IArray.empty(array.getClass.getComponentType.castParam[E]))
				case n =>
					(array.take(n).castFrom[Array[E], IArray[E]], array.drop(n).asInstanceOf[IArray[E]])
			}
*/

		@inline def updated[U >: E :ClassTag](index :Int, x :U) :IArray[U] =
			new ArrayExtension(self).updated(index, x).castFrom[Array[U], IArray[U]]

		//we could reuse in this method this array if patch is out of range, but that would become inconsistent
		// with the class tag, and might cause problems for what's a very fringe case.
		/** An 'exploded' variant of `updatedAll`: a copy of this array with elements starting at `index` substituted
		  * by `first`, `second`, and contents of `rest`. The index must lie in range `[0..length - rest.size - 2)`.
		  */
		@inline def updatedAll[U >: E :ClassTag](index :Int, first :U, second :U, rest :U*) :IArray[U] =
			new ArrayExtension(self).updatedAll(index, first, second, rest :_*).castFrom[Array[U], IArray[U]]

		/** Updates consecutive elements of this array, starting with the specified index,
		  * with elements from the collection. Equivalent to
		  * [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.patch patch]]`(index, elems, elems.size)`,
		  * but more efficient due to a single array allocation.
		  */
		@inline def updatedAll[U >: E :ClassTag](index :Int, elems :IterableOnce[U]) :IArray[U] =
			new ArrayExtension(self).updatedAll(index, elems).castFrom[Array[U], IArray[U]]

		/** Updates consecutive elements of this array, starting with the specified index,
		  * with elements from the collection. Equivalent to
		  * [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.patch patch]]`(index, elems, elems.size)`,
		  * but more efficient due to a single array allocation.
		  */
		@inline def updatedAll[U >: E :ClassTag](index :Int, elems :ArrayLike[U]) :IArray[U] =
			new ArrayExtension(self).updatedAll(index, elems).castFrom[Array[U], IArray[U]]

		/** Updates consecutive elements of this array, starting with the specified index,
		  * with elements from the collection. Equivalent to
		  * [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.patch patch]]`(index, elems, elems.size)`,
		  * but more efficient due to a single array allocation.
		  */ //we can't use IArray because we are covariant, so both can be of unrelated types
		@inline def updatedAll[U >: E](index :Int, elems :Array[U]) :IArray[U] =
			new ArrayExtension(self).updatedAll(index, elems).castFrom[Array[U], IArray[U]]


		/** A copy of this array, with the element inserted at a given position in this array,
		  * and all elements at positions equal or greater than `index` by one element further.
		  * This is equivalent to [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.patch patch]]
		  * with a singleton collection and `replaced` equal to zero, but the index must be in the valid range
		  * for this array.
		  * @return `take(index) :+ elem :++ drop(index)`, but in a more efficient manner.
		  * @throws IndexOutOfBoundsException if `index` < 0 or `index > length`.
		  */
		@inline def inserted[A >: E :ClassTag](index :Int, elem :A) :IArray[A] =
			new ArrayExtension(self).inserted(index, elem).castFrom[Array[A], IArray[A]]

		/** A copy of this array, with the element inserted at a given position in this array,
		  * and all elements at positions equal or greater than `index` by one element further.
		  * This is equivalent to [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.patch patch]]
		  * with a singleton collection and `replaced` equal to zero, but the index must be in the valid range
		  * for this array.
		  * @return `take(index) :+ elem :++ drop(index)`, but in a more efficient manner.
		  * @throws IndexOutOfBoundsException if `index` < 0 or `index > length`.
		  */
		@inline def inserted(index :Int, elem :E) :IArray[E] =
			new ArrayExtension(self).inserted(index, elem).castFrom[Array[E], IArray[E]]

		/** An 'exploded' variant of `updatedAll`: a copy of this array with elements starting at `index` substituted
		  * by `first`, `second`, and contents of `rest`. The index must lie in range `[0..length)`.
		  */
		@inline def insertedAll[A >: E :ClassTag](index :Int, first :A, second :A, rest :A*) :IArray[A] =
			new ArrayExtension(self).insertedAll(index, first, second, rest :_*).castFrom[Array[A], IArray[A]]

		/** An 'exploded' variant of `updatedAll`: a copy of this array with elements starting at `index` substituted
		  * by `first`, `second`, and contents of `rest`. The index must lie in range `[0..length)`.
		  */
		@inline def insertedAll(index :Int, first :E, second :E, rest :E*) :IArray[E] =
			new ArrayExtension(self).insertedAll(index, first, second, rest :_*).castFrom[Array[E], IArray[E]]

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * @return [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.patch patch]]`(index, elems, 0)`.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		@inline def insertedAll[A >: E :ClassTag](index :Int, elems :IterableOnce[A]) :IArray[A] =
			new ArrayExtension(self).insertedAll(index, elems).castFrom[Array[A], IArray[A]]
//
//		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
//		  * and all subsequent elements.
//		  * @return [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.patch patch]]`(index, elems, 0)`.
//		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
//		  */
//		@inline def insertedAll(index :Int, elems :IterableOnce[E]) :IArray[E] =
//			array.insertedAll(index, elems).castFrom[Array[E], IArray[E]]

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * @return [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.patch patch]]`(index, elems, 0)`.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		@inline def insertedAll[A >: E :ClassTag](index :Int, elems :ArrayLike[A]) :IArray[A] =
			new ArrayExtension(self).insertedAll(index, elems).castFrom[Array[A], IArray[A]]
//
//		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
//		  * and all subsequent elements.
//		  * @return [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.patch patch]]`(index, elems, 0)`.
//		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
//		  */
//		@inline def insertedAll(index :Int, elems :ArrayLike[E]) :IArray[E] =
//			array.insertedAll(index, elems).castFrom[Array[E], IArray[E]]

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * @return [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.patch patch]]`(index, elems, 0)`.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		@inline def insertedAll[A >: E](index :Int, elems :Array[A]) :IArray[A] =
			new ArrayExtension(self).insertedAll(index, elems).castFrom[Array[A], IArray[A]]


		@inline def :+[A >: E :ClassTag](x :A) :IArray[A] = appended(x)
//		@inline def :+(x :E) :IArray[E] = appended(x)
		@inline def +:[A >: E :ClassTag](x :A) :IArray[A] = prepended(x)
//		@inline def +:(x :E) :IArray[E] = prepended(x)
		@inline def appended[A >: E :ClassTag](x :A) :IArray[A] =
			new ArrayExtension(self).appended(x).castFrom[Array[A], IArray[A]]
//		@inline def appended(x :E) :IArray[E] = array.appended(x).castFrom[Array[E], IArray[E]]
		@inline def appendedAll[A >: E :ClassTag](first :A, second :A, rest :A*) :IArray[A] =
			new ArrayExtension(self).appendedAll(first, second, rest :_*).castFrom[Array[A], IArray[A]]

		@inline def prepended[A >: E :ClassTag](x :A) :IArray[A] =
			new ArrayExtension(self).prepended(x).castFrom[Array[A], IArray[A]]
//		@inline def prepended(x :E) :IArray[E] = array.prepended(x).castFrom[Array[E], IArray[E]]
		@inline def prependedAll[A >: E :ClassTag](first :A, second :A, rest :A*) :IArray[A] =
			new ArrayExtension(self).prependedAll(first, second, rest :_*).castFrom[Array[A], IArray[A]]

		@inline def ++[A >: E :ClassTag](suffix :IterableOnce[A]) :IArray[A] = concat(suffix)
		@inline def ++[A >: E :ClassTag](suffix :ArrayLike[A]) :IArray[A] = concat(suffix)
//		@inline def ++(suffix :ArrayLike[E]) :IArray[E] = concat(suffix)
		@inline def ++[A >: E](suffix :Array[A]) :IArray[A] = concat(suffix)

		@inline def concat[A >: E :ClassTag](suffix :IterableOnce[A]) :IArray[A] =
			new ArrayExtension(self).appendedAll(suffix).castFrom[Array[A], IArray[A]]

		@inline def concat[A >: E :ClassTag](suffix :ArrayLike[A]) :IArray[A] =
			new ArrayExtension(self).appendedAll(suffix).castFrom[Array[A], IArray[A]]
//		@inline def concat(suffix :ArrayLike[E]) :IArray[E] = array.appendedAll(suffix).castFrom[Array[E], IArray[E]]

		@inline def concat[A >: E](suffix :Array[A]) :IArray[A] = self.concat(suffix).castFrom[Array[A], IArray[A]]

		@inline def :++[A >: E :ClassTag](suffix :IterableOnce[A]) :IArray[A] = appendedAll(suffix)
		@inline def :++[A >: E :ClassTag](suffix :ArrayLike[A]) :IArray[A] = appendedAll(suffix)
		@inline def :++[A >: E](suffix :Array[E]) :IArray[E] = appendedAll(suffix)
//		@inline def :++(suffix :IterableOnce[E]) :IArray[E] = appendedAll(suffix)
//		@inline def :++(suffix :ArrayLike[E]) :IArray[E] = appendedAll(suffix)

		@inline def appendedAll[A >: E :ClassTag](suffix :IterableOnce[A]) :IArray[A] =
			new ArrayExtension(self).appendedAll(suffix).castFrom[Array[A], IArray[A]]
//
//		@inline def appendedAll(suffix :IterableOnce[E]) :IArray[E] =
//			array.appendedAll(suffix).castFrom[Array[E], IArray[E]]

		@inline def appendedAll[A >: E :ClassTag](suffix :ArrayLike[A]) :IArray[A] =
			new ArrayExtension(self).appendedAll(suffix).castFrom[Array[A], IArray[A]]
//
//		@inline def appendedAll(suffix :ArrayLike[E]) :IArray[E] =
//			array.appendedAll(suffix).castFrom[Array[E], IArray[E]]

		@inline def appendedAll[A >: E](suffix :Array[A]) :IArray[A] =
			new ArrayExtension(self).appendedAll(suffix).castFrom[Array[A], IArray[A]]


		@inline def ++:[A >: E :ClassTag](prefix :IterableOnce[A]) :IArray[A] = prependedAll(prefix)
		@inline def ++:[A >: E :ClassTag](prefix :ArrayLike[A]) :IArray[A] = prependedAll(prefix)
		@inline def ++:[A >: E](prefix :Array[A]) :IArray[A] = prependedAll(prefix)
//		@inline def ++:(prefix :IterableOnce[E]) :IArray[E] = prependedAll(prefix)
//		@inline def ++:(prefix :ArrayLike[E]) :IArray[E] = prependedAll(prefix)

		@inline def prependedAll[A >: E :ClassTag](prefix :IterableOnce[A]) :IArray[A] =
			new ArrayExtension(self).prependedAll(prefix).castFrom[Array[A], IArray[A]]

		@inline def prependedAll[A >: E :ClassTag](prefix :ArrayLike[A]) :IArray[A] =
			new ArrayExtension(self).prependedAll(prefix).castFrom[Array[A], IArray[A]]
//
//		@inline def prependedAll(prefix :IterableOnce[E]) :IArray[E] =
//			array.prependedAll(prefix).castFrom[Array[E], IArray[E]]
//
//		@inline def prependedAll(prefix :ArrayLike[E]) :IArray[E] =
//			array.prependedAll(prefix).castFrom[Array[E], IArray[E]]

		@inline def prependedAll[A >: E](suffix :Array[A]) :IArray[A] =
			new ArrayExtension(self).prependedAll(suffix).castFrom[Array[A], IArray[A]]

		@inline def patch[A >: E :ClassTag](from :Int, elems :IterableOnce[A], replaced :Int) :IArray[A] =
			new ArrayExtension(self).patch(from, elems, replaced).castFrom[Array[A], IArray[A]]

		@inline def patch[A >: E :ClassTag](from :Int, elems :ArrayLike[A], replaced :Int) :IArray[A] =
			new ArrayExtension(self).patch(from, elems, replaced).castFrom[Array[A], IArray[A]]

		@inline def patch[A >: E](from :Int, elems :Array[A], replaced :Int) :IArray[A] =
			new ArrayExtension(self).patch(from, elems, replaced).castFrom[Array[A], IArray[A]]

		/** A view on the index range `[from, until)` of this array as a sequence.
		  * Slicing of the returned sequence will return similar views, sharing the same underlying array.
		  */
		@inline def subseq(from :Int, until :Int) :IndexedSeq[E] =
			Wrapped.Slice(self.asInstanceOf[IArray[E]], from, until)

		/** An exact copy of this `IArray` as a standard mutable `Array`. */
		@inline def mutable :Array[E] = ArrayFactory.copyOf(self)

		def toOps :IndexedSeqOps[E, IRefArray, IArray[E]] = new IArrayAsSeq[E](self.castFrom[Array[E], IArray[E]])

		@inline def toSeq :Seq[E] = Wrapped(self.castFrom[Array[E], IArray[E]])
		@inline def toIndexedSeq :IndexedSeq[E] = Wrapped(self.castFrom[Array[E], IArray[E]])
	}


	/** A lazy filtered array. No filtering is applied until one of `foreach`, `map` or `flatMap` is called.
	  * Implementation adapted from [[collection.ArrayOps.WithFilter ArrayOps.WithFilter]] to return an `IArray`.
	  */
	class WithFilter[E] private[IArray] (p :E => Boolean, xs :Array[E]) {

		/** Apply `f` to each element for its side effects.
		  * Note: [U] parameter needed to help scalac's type inference.
		  */
		def foreach[U](f :E => U) :Unit = {
			val len = xs.array.length
			var i = 0
			while (i < len) {
				val x = xs(i)
				if (p(x)) f(x)
				i += 1
			}
		}

		/** Builds a new array by applying a function to all elements of this array.
		  * @param f      the function to apply to each element.
		  * @tparam A     the element type of the returned array.
		  * @return a new array resulting from applying the given function
		  *         `f` to each element of this array and collecting the results.
		  */
		def map[A :ClassTag](f :E => A) :IArray[A] = {
			val b = IArray.newBuilder[A]
			var i = 0
			while (i < xs.length) {
				val x = xs(i)
				if (p(x)) b += f(x)
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
		def flatMap[A :ClassTag](f :E => IterableOnce[A]) :IArray[A] = {
			val b = IArray.newBuilder[A]
			var i = 0
			while (i < xs.length) {
				val x = xs(i)
				if (p(x)) b ++= f(xs(i))
				i += 1
			}
			b.result()
		}

		def flatMap[As, A](f :E => As)(implicit asIterable :As => Iterable[A], tag :ClassTag[A]) :IArray[A] =
			flatMap[A](x => asIterable(f(x)))

		/** Creates a new non-strict filter which combines this filter with the given predicate. */
		def withFilter(q :E => Boolean) :WithFilter[E] =
			new WithFilter[E](a => p(a) && q(a), xs)
	}




	/** Creates a new `Array[E]` of the specified length, executes the given initialization function for it,
	  * and returns it as an `IArray[E]`. It is a pattern for initialization safer than manually creating
	  * an `Array[E]`, writing to it, and casting it `IArray[E]`. The application should ''not'' retain a reference
	  * to the array given as the function argument, or the immutability of the result may be voided.
	  * {{{
	  *     IArray.init[String](3) { array =>
	  *         array(0) = "You"
	  *         array(1) = "Boo"
	  *         array(2) = "I"
	  *     }
	  * }}}
	  */
	@inline def init[E :ClassTag](length :Int)(f :Array[E] => Unit) :IArray[E] = {
		val res = if (length == 0) empty[E].asInstanceOf[Array[E]] else new Array[E](length)
		res match {
			case units :Array[Unit] => units.fill(())
			case _ =>
		}
		f(res)
		res.castFrom[Array[E], IArray[E]]
	}

	/** Creates a new `Array[E]` of the specified length and element type, executes the given initialization function
	  * for it, and returns it as an `IArray[E]`. It is a pattern for initialization safer than manually creating
	  * an `Array[E]`, writing to it, and casting it `IArray[E]`. The application should ''not'' retain a reference
	  * to the array given as the function argument, or the immutability of the result may be voided.
	  * {{{
	  *     IArray.init(classOf[String], 3) { array =>
	  *         array(0) = "You"
	  *         array(1) = "Boo"
	  *         array(2) = "I"
	  *     }
	  * }}}
	  */
	@inline def init[E](elementType :Class[E], length :Int)(f :Array[_ >: E] => Unit) :IArray[E] = {
		val res = ArrayFactory.ofDim(elementType, length)
		f(res)
		res.castFrom[Array[E], IArray[E]]
	}

	/** Creates a new `Array[E]` of the specified length and the same element type as the argument array,
	  * executes the given initialization function for it, and returns it as an `IArray[E]`.
	  * The argument array's size and contents are irrelevant.
	  * @see [[net.noresttherein.sugar.arrays.IArray.init]]
	  * @see [[net.noresttherein.sugar.arrays.IArray.updated(]]
	  */
	@inline def like[E](other :Array[E], length :Int)(f :Array[_ >: E] => Unit) :IArray[E] =
		init(other.getClass.getComponentType.castParam[E], length)(f)

	/** Creates a new `IArray` by modifying another `ArrayLike`.
	  * This method combines [[net.noresttherein.sugar.arrays.IArray.copyOf copyOf]]`(other)`
	  * with [[net.noresttherein.sugar.arrays.IArray.init init]]: the latter, instead of an empty
	  * array filled with default values, receives a copy of the argument. Note that, because `ArrayLike`
	  * is covariant, the element type of the returned array might be different from that of the array underlying `other`:
	  * the new element type is specified based on an implicit `ClassTag`.
	  *
	  * It allows to introduce multiple changes to an array in a manner safer than starting with a mutable `Array`
	  * and casting it to an `IArray`, but without a need to chain multiple calls to (extension) methods
	  * of the `original` (which would require wasteful creation of intermediate results). Additionally,
	  * using [[java.util.Arrays Arrays]]`.`[[java.util.Arrays.copyOf copyOf]] is faster than creating an array
	  * filled with default values and copying contents to it afterwards.
	  * @param other A prototype array with values passed to the initialization function.
	  * @param f     An initialization function, accepting a fresh array of the element type specified
	  *              by the `ClassTag` context parameter, allowed to modify it as will.
	  *              Should not retain the reference to the argument after its completion, or immutability
	  *              of the result will be compromised.
	  */
	@inline def updated[E :ClassTag](other :ArrayLike[E])(f :Array[_ >: E] => Unit) :IArray[E] = {
		val res = ArrayFactory.copyOf(other)
		f(res)
		res.castFrom[Array[E], IArray[E]]
	}

	/** Creates a new `IArray` by copying and modifying contents of another `ArrayLike`.
	  * This method combines [[net.noresttherein.sugar.arrays.IArray.copyOf copyOf]]`(other, newLength)`
	  * with [[net.noresttherein.sugar.arrays.IArray.init init]]: the latter, instead of an empty
	  * array filled with default values, receives a copy of the argument. Note that, because `ArrayLike`
	  * is covariant, the element type of the returned array might be different from that of the array underlying `other`:
	  * the new element type is specified based on an implicit `ClassTag`.
	  *
	  * It allows to introduce multiple changes to an array in a manner safer than starting with a mutable `Array`
	  * and casting it to an `IArray`, but without a need to chain multiple calls to (extension) methods
	  * of the `original` (which would require wasteful creation of intermediate results). Additionally,
	  * using [[java.util.Arrays Arrays]]`.`[[java.util.Arrays.copyOf copyOf]] is faster than creating an array
	  * filled with default values and copying contents to it afterwards.
	  * @param other     A prototype array with values passed to the initialization function.
	  * @param newLength The length of the created array. If lesser than the length of `other`,
	  *                  then the extra elements are ignored. Otherwise the array is padded with default values
	  *                  before being passed to `f`.
	  * @param f         An initialization function, accepting a fresh array of the element type specified
	  *                  by the `ClassTag` context parameter, allowed to modify it as will.
	  *                  Should not retain the reference to the argument after its completion, or immutability
	  *                  of the result will be compromised.
	  */
	@inline def updated[E :ClassTag](other :ArrayLike[E], newLength :Int)(f :Array[_ >: E] => Unit) :IArray[E] = {
		val res = ArrayFactory.copyOf(other, newLength)
		f(res)
		res.castFrom[Array[E], IArray[E]]
	}

	/** Creates a new `IArray` by introducing changes to a slice of another `ArrayLike`.
	  * This method combines [[net.noresttherein.sugar.arrays.IArray.copyOfRange copyOfRange]]`(other, from, until)`
	  * with [[net.noresttherein.sugar.arrays.IArray.init init]]: the latter, instead of an empty
	  * array filled with default values, receives a slice of the original. Note that, because `ArrayLike`
	  * is covariant, the element type of the returned array might be different from that of the array underlying `other`:
	  * the new element type is specified based on an implicit `ClassTag`.
	  *
	  * It allows to introduce multiple changes to an array in a manner safer than starting with a mutable `Array`
	  * and casting it to an `IArray`, but without a need to chain multiple calls to (extension) methods
	  * of the `original` (which would require wasteful creation of intermediate results). Additionally,
	  * using [[java.util.Arrays Arrays]]`.`[[java.util.Arrays.copyOfRange copyOfRange]] is faster than creating
	  * an array filled with default values and copying contents to it afterwards.
	  * @param other A prototype array whose slice is passed to the initialization function.
	  * @param from  The index in the original array to which the first element of the new array is set;
	  *              if lesser than zero or greater than `other.length`, it is clipped to the valid range.
	  * @param until The exclusive end of the copied range, clipped to the valid index range before copying.
	  * @param f     An initialization function, accepting a fresh array of the element type specified
	  *              by the `ClassTag` context parameter, allowed to modify it as will.
	  *              Should not retain the reference to the argument after its completion, or immutability
	  *              of the result will be compromised.
	  */
	@inline def updated[E :ClassTag](other :ArrayLike[E], from :Int, until :Int)(f :Array[_ >: E] => Unit) :IArray[E] = {
		val res = Array.copyOfRange(other, from, until)
		f(res)
		res.castFrom[Array[E], IArray[E]]
	}

	/** Creates a new `IArray` by introducing changes to a slice of another `ArrayLike`, including, potentially
	  * appending additional elements. This method combines
	  * [[net.noresttherein.sugar.arrays.IArray.copyOfRange copyOfRange]]`(other, from, until, newLength)`
	  * with [[net.noresttherein.sugar.arrays.IArray.init init]]: the latter, instead of an empty
	  * array filled with default values, receives an array of the specified length, with its prefix already
	  * initialized to the copy of the index range `[from, until)` of the original. Note that, because `ArrayLike`
	  * is covariant, the element type of the returned array might be different from that of the array underlying `other`:
	  * the new element type is specified based on an implicit `ClassTag`.
	  *
	  * It allows to introduce multiple changes to an array in a manner safer than starting with a mutable `Array`
	  * and casting it to an `IArray`, but without a need to chain multiple calls to (extension) methods
	  * of the `original` (which would require wasteful creation of intermediate results). Additionally,
	  * using [[java.util.Arrays Arrays]]`.`[[java.util.Arrays.copyOfRange copyOfRange]] is faster than creating
	  * an array filled with default values and copying contents to it afterwards.
	  * @param other     An array whose slice is copied to the the new array before passing it
	  *                  to the initialization function.
	  * @param from      The index in the original array to which the first element of the new array is set;
	  *                  if lesser than zero or greater than `other.length`, it is clipped to the valid range.
	  * @param until     The exclusive end of the copied range, clipped to the valid index range before copying.
	  * @param newLength The length of the created array. If lesser than the length of the copied slice, the extra
	  *                  elements are ignored. Otherwise the array is padded with default values
	  *                  before being passed to `f`.
	  * @param f         An initialization function, accepting a fresh array of the element type specified
	  *                  by the `ClassTag` context parameter, allowed to modify it as will.
	  *                  Should not retain the reference to the argument after its completion, or immutability
	  *                  of the result will be compromised.
	  */
	@throws[NegativeArraySizeException]("if newLength is less than zero")
	@inline def updated[E :ClassTag](other :ArrayLike[E], from :Int, until :Int, newLength :Int)
	                                (f :Array[_ >: E] => Unit) :IArray[E] =
	{
		val res = Array.copyOfRange(other, from, until, newLength)
		f(res)
		res.castFrom[Array[E], IArray[E]]
	}

	/** Creates a new `IArray` of the specified length by modifying a slice of another `ArrayLike`. This method combines
	  * [[net.noresttherein.sugar.arrays.IArray.copyOfRange copyOfRange]]`(other, from, until, offset, newLength)`
	  * with [[net.noresttherein.sugar.arrays.IArray.init init]]: the latter, instead of an empty
	  * array filled with default values, receives an array created as with:
	  * {{{
	  *     val res = new Array[E](newLength)
	  *     val src = from max 0 min other.length
	  *     val len = (until min other.length max from) - from
	  *     Array.copy(other, src, res, offset min newLength, len)
	  *     f(res)
	  * }}}
	  * However, depending on the arguments, the method might choose an alternative implementation if it's deemed
	  * to be more efficient. Note that, because `ArrayLike`
	  * is covariant, the element type of the returned array might be different from that of the array underlying `other`:
	  * the new element type is specified based on an implicit `ClassTag`.
	  *
	  * It allows to introduce multiple changes to an array in a manner safer than starting with a mutable `Array`
	  * and casting it to an `IArray`, but without a need to chain multiple calls to (extension) methods
	  * of the `original` (which would require wasteful creation of intermediate results).
	  * @param other     An array whose slice is copied to the the new array before passing it
	  *                  to the initialization function.
	  * @param from      The index in the original array of the first copied element;
	  *                  if lesser than zero or greater than `other.length`, it is clipped to the valid range.
	  * @param until     The exclusive end of the copied range, clipped to the valid index range before copying.
	  * @param offset    The index in the new array where copying starts.
	  * @param newLength The length of the created array. If lesser than the length of the copied slice, the extra
	  *                  elements are ignored. Otherwise the array is padded with default values
	  *                  before being passed to `f`.
	  * @param f         An initialization function, accepting a fresh array of the element type specified
	  *                  by the `ClassTag` context parameter, allowed to modify it as will.
	  *                  Should not retain the reference to the argument after its completion, or immutability
	  *                  of the result will be compromised.
	  */
	@throws[IndexOutOfBoundsException]("if offset is less than zero")
	@throws[NegativeArraySizeException]("if newLength is less than zero")
	@inline def updated[E :ClassTag](other :ArrayLike[E], from :Int, until :Int, offset :Int, newLength :Int)
	                                (f :Array[_ >: E] => Unit) :IArray[E] =
	{
		val res = Array.copyOfRange(other, from, until, offset, newLength)
		f(res)
		res.castFrom[Array[E], IArray[E]]
	}

	/** Creates a new `IArray` by modifying another `ArrayLike`.
	  * The underlying type of the new array will be the same as of the argument.
	  * This method combines [[net.noresttherein.sugar.arrays.IArray.copyOf copyOf]]`(other)`
	  * with [[net.noresttherein.sugar.arrays.IArray.init init]]: the latter, instead of an empty
	  * array filled with default values, receives a copy of the argument.
	  *
	  * It allows to introduce multiple changes to an array in a manner safer than starting with a mutable `Array`
	  * and casting it to an `IArray`, but without a need to chain multiple calls to (extension) methods
	  * of the `original` (which would require wasteful creation of intermediate results). Additionally,
	  * using [[java.util.Arrays Arrays]]`.`[[java.util.Arrays.copyOf copyOf]] is faster than creating an array
	  * filled with default values and copying contents to it afterwards.
	  * @param other A prototype array with values passed to the initialization function.
	  * @param f     An initialization function, accepting a fresh array of the element type specified
	  *              by the `ClassTag` context parameter, allowed to modify it as will.
	  *              Should not retain the reference to the argument after its completion, or immutability
	  *              of the result will be compromised.
	  */
	@inline def updated[E](other :Array[E])(f :Array[_ >: E] => Unit) :IArray[E] = {
		val res = ArrayFactory.copyOf(other)
		f(res)
		res.castFrom[Array[E], IArray[E]]
	}

	/** Creates a new `IArray` by copying and modifying contents of another `ArrayLike`.
	  * The underlying type of the new array will be the same as of the argument.
	  * This method combines [[net.noresttherein.sugar.arrays.IArray.copyOf copyOf]]`(other, newLength)`
	  * with [[net.noresttherein.sugar.arrays.IArray.init init]]: the latter, instead of an empty
	  * array filled with default values, receives a copy of the argument.
	  *
	  * It allows to introduce multiple changes to an array in a manner safer than starting with a mutable `Array`
	  * and casting it to an `IArray`, but without a need to chain multiple calls to (extension) methods
	  * of the `original` (which would require wasteful creation of intermediate results). Additionally,
	  * using [[java.util.Arrays Arrays]]`.`[[java.util.Arrays.copyOf copyOf]] is faster than creating an array
	  * filled with default values and copying contents to it afterwards.
	  * @param other     A prototype array with values passed to the initialization function.
	  * @param newLength The length of the created array. If lesser than the length of `other`,
	  *                  then the extra elements are ignored. Otherwise the array is padded with default values
	  *                  before being passed to `f`.
	  * @param f         An initialization function, accepting a fresh array of the element type specified
	  *                  by the `ClassTag` context parameter, allowed to modify it as will.
	  *                  Should not retain the reference to the argument after its completion, or immutability
	  *                  of the result will be compromised.
	  */
	@inline def updated[E](other :Array[E], newLength :Int)(f :Array[_ >: E] => Unit) :IArray[E] = {
		val res = ArrayFactory.copyOf(other, newLength)
		f(res)
		res.castFrom[Array[E], IArray[E]]
	}

	/** Creates a new `IArray` by introducing changes to a slice of some regular `Array`.
	  * The underlying type of the new array will be the same as of the argument.
	  * This method combines [[net.noresttherein.sugar.arrays.IArray.slice slice]]`(other, from, until)`
	  * with [[net.noresttherein.sugar.arrays.IArray.init init]]: the latter, instead of an empty
	  * array filled with default values, receives a slice of the original.
	  *
	  * It allows to introduce multiple changes to an array in a manner safer than starting with a mutable `Array`
	  * and casting it to an `IArray`, but without a need to chain multiple calls to (extension) methods
	  * of the `original` (which would require wasteful creation of intermediate results). Additionally,
	  * using [[java.util.Arrays Arrays]]`.`[[java.util.Arrays.copyOfRange copyOfRange]] is faster than creating
	  * an array filled with default values and copying contents to it afterwards.
	  * @param other     An array whose slice is copied to the the new array before passing it
	  *                  to the initialization function.
	  * @param from      The index in the original array to which the first element of the new array is set;
	  *                  if lesser than zero or greater than `other.length`, it is clipped to the valid range.
	  * @param until     The exclusive end of the copied range, clipped to the valid index range before copying.
	  * @param f         An initialization function, accepting a fresh array of the element type specified
	  *                  by the `ClassTag` context parameter, allowed to modify it as will.
	  *                  Should not retain the reference to the argument after its completion, or immutability
	  *                  of the result will be compromised.
	  */
	@inline def updated[E](other :Array[E], from :Int, until :Int)(f :Array[_ >: E] => Unit) :IArray[E] = {
		val res = Array.copyOfRange(other, from, until)
		f(res)
		res.castFrom[Array[E], IArray[E]]
	}

	/** Creates a new `IArray` by introducing changes to a slice of another `ArrayLike`, including, potentially
	  * appending additional elements. The underlying type of the new array will be the same as of the argument.
	  * This method combines
	  * [[net.noresttherein.sugar.arrays.IArray.copyOfRange copyOfRange]]`(other, from, until, newLength)`
	  * with [[net.noresttherein.sugar.arrays.IArray.init init]]: the latter, instead of an empty
	  * array filled with default values, receives an array of the specified length, with its prefix already
	  * initialized to the copy of the index range `[from, until)` of the original.
	  *
	  * It allows to introduce multiple changes to an array in a manner safer than starting with a mutable `Array`
	  * and casting it to an `IArray`, but without a need to chain multiple calls to (extension) methods
	  * of the `original` (which would require wasteful creation of intermediate results). Additionally,
	  * using [[java.util.Arrays Arrays]]`.`[[java.util.Arrays.copyOfRange copyOfRange]] is faster than creating
	  * an array filled with default values and copying contents to it afterwards.
	  * @param other     An array whose slice is copied to the the new array before passing it
	  *                  to the initialization function.
	  * @param from      The index in the original array to which the first element of the new array is set;
	  *                  if lesser than zero or greater than `other.length`, it is clipped to the valid range.
	  * @param until     The exclusive end of the copied range, clipped to the valid index range before copying.
	  * @param newLength The length of the created array. If lesser than the length of the copied slice, the extra
	  *                  elements are ignored. Otherwise the array is padded with default values
	  *                  before being passed to `f`.
	  * @param f         An initialization function, accepting a fresh array of the element type specified
	  *                  by the `ClassTag` context parameter, allowed to modify it as will.
	  *                  Should not retain the reference to the argument after its completion, or immutability
	  *                  of the result will be compromised.
	  */
	@throws[NegativeArraySizeException]("if newLength is less than zero")
	@inline def updated[E](other :Array[E], from :Int, until :Int, newLength :Int)
	                      (f :Array[_ >: E] => Unit) :IArray[E] =
	{
		val res = Array.copyOfRange(other, from, until, newLength)
		f(res)
		res.castFrom[Array[E], IArray[E]]
	}

	/** Creates a new `IArray` of the specified length by modifying a slice of another `ArrayLike`. This method combines
	  * [[net.noresttherein.sugar.arrays.IArray.copyOfRange copyOfRange]]`(other, from, until, offset, newLength)`
	  * with [[net.noresttherein.sugar.arrays.IArray.init init]]: the latter, instead of an empty
	  * array filled with default values, receives an array created as with:
	  * {{{
	  *     val res = new Array[E](newLength)
	  *     val src = from max 0 min other.length
	  *     val len = (until min other.length max from) - from
	  *     Array.copy(other, src, res, offset min newLength, len)
	  *     f(res)
	  * }}}
	  * However, depending on the arguments, the method might choose an alternative implementation if it's deemed
	  * to be more efficient.
	  *
	  * It allows to introduce multiple changes to an array in a manner safer than starting with a mutable `Array`
	  * and casting it to an `IArray`, but without a need to chain multiple calls to (extension) methods
	  * of the `original` (which would require wasteful creation of intermediate results).
	  * @param other     An array whose slice is copied to the the new array before passing it
	  *                  to the initialization function.
	  * @param from      The index in the original array of the first copied element;
	  *                  if lesser than zero or greater than `other.length`, it is clipped to the valid range.
	  * @param until     The exclusive end of the copied range, clipped to the valid index range before copying.
	  * @param offset    The index in the new array where copying starts.
	  * @param newLength The length of the created array. If lesser than the length of the copied slice, the extra
	  *                  elements are ignored. Otherwise the array is padded with default values
	  *                  before being passed to `f`.
	  * @param f         An initialization function, accepting a fresh array of the element type specified
	  *                  by the `ClassTag` context parameter, allowed to modify it as will.
	  *                  Should not retain the reference to the argument after its completion, or immutability
	  *                  of the result will be compromised.
	  */
	@throws[IndexOutOfBoundsException]("if offset is less than zero")
	@throws[NegativeArraySizeException]("if newLength is less than zero")
	@inline def updated[E](other :Array[E], from :Int, until :Int, offset :Int, newLength :Int)
	                      (f :Array[_ >: E] => Unit) :IArray[E] =
	{
		val res = Array.copyOfRange(other, from, until, offset, newLength)
		f(res)
		res.castFrom[Array[E], IArray[E]]
	}

	//commented out because it probably is not useful to have an immutable array with uninitialized segments.
//	/** Creates an `IArray[E]` of the specified element type and length, executes the given initialization function
//	  * for it, and returns it as an `IArray[E]`.
//	  * @see [[net.noresttherein.sugar.arrays.IArray.init]]
//	  */
//	@inline final def make[E](elemType :Class[E], length :Int)(f :Array[E] => Unit) :IArray[E] = {
//		val res = Array.of(elemType, length)
//		f(res)
//	    res.castFrom[Array[E], IArray[E]]
//	}
//
//	/** Creates a new `IArray[E]` of the specified length and of the same type as the argument,
//	  * executes the given initialization function for it, and returns it as an `IArray[E]`.
//	  * @see [[net.noresttherein.sugar.arrays.IArray.init]]
//	  */
//	@inline final def like[E](array :Array[E], length :Int)(f :Array[E] => Unit) :IArray[E] =
//		make(array.getClass.getComponentType.castParam[E], length)(f)


	/** Creates a copy of the given array, possibly changing its element type. */
	@inline def copyAs[E :ClassTag](array :ArrayLike[_]) :IArray[E] = copyAs(array, array.length)

	/** Copies one array to another, truncating or padding with default values (if necessary),
	  * so that the copy has the specified length. The new array can have a different type than the original one,
	  * as long as the values are assignment-compatible. When copying between primitive and object arrays,
	  * boxing and unboxing are supported.
	  */
	@inline def copyAs[E :ClassTag](array :ArrayLike[_], newLength :Int) :IArray[E] =
		ArrayFactory.copyAs[E](array, newLength).castFrom[Array[E], IArray[E]]

//	/** Creates an immutable copy of the argument, with the same element type. */
//	def copyOf[E](array :Array[E]) :IArray[E] = ArrayAsSeq.copyOf(array).castFrom[Array[E], IArray[E]]
//
//	/** Creates an immutable copy of the argument, truncating or padding with default values (if necessary),
//	  * so that the copy has the specified length. The returned array will have the same element type.
//	  */
//	def copyOf[E](array :Array[E], newLength :Int) :IArray[E] =
//		ArrayAsSeq.copyOf(array, newLength).castFrom[Array[E], IArray[E]]

	/** Creates a new immutable array with the element type specified by `ClassTag[E]`
	  * and the same contents as the argument.
	  */
	@inline def copyOf[E :ClassTag](array :ArrayLike[E]) :IArray[E] = copyAs(array, array.length)

	/** Copies one array to another, truncating or padding with default values (if necessary), so that the copy has
	  * the specified length. The returned array will be of the element type specified by the `ClassTag[E]`.
	  * Equivalent to `Array.`[[Array.copyAs copyAs]]`(array, newLength)`, but accepts any `ArrayLike` and returns
	  * the result as an $Coll.
	  */
	@inline def copyOf[E :ClassTag](array :ArrayLike[E], newLength :Int) :IArray[E] = copyAs(array, newLength)

	//commented out because it probably is not useful to have an immutable array with uninitialized segments.
//	@inline def copyOf[E :ClassTag](array :ArrayLike[E], offset :Int, newLength :Int) :IArray[E] =
//		Array.copyOfRange(array, 0, newLength, offset, newLength).castFrom[Array[E], IArray[E]]

	/** Copies one array to another, truncating or padding with default values (if necessary) so the copy has
	  * the specified length. The returned array will be of the same type as the original.
	  * Equivalent to `Array.`[[Array.copyOf copyOf]]`(array, newLength)`, but accepts any `ArrayLike` and returns
	  * the result as an $Coll.
	  */
	@inline def copyOf[E](array :IArray[E], newLength :Int) :IArray[E] =
		ArrayFactory.copyOf(array.castFrom[ArrayLike[E], Array[E]], newLength).castFrom[Array[E], IArray[E]]

//	/** Reallocates the given array as a $Coll`[E]` of a new size, and copies `min(newLength - offset, array.length)`
//	  * of its first elements to positions starting with `offset`.
//	  */
//	def copyOf[E](array :IArray[E], offset :Int, newLength :Int) :IArray[E] =
//		if (offset == 0)
//			copyOf(array, newLength)
//		else if (newLength == 0)
//			empty
//		else if (offset < 0 | offset > newLength)
//			throw new IndexOutOfBoundsException(
//				toString + ".copyOf(" + array.className + "|" + array.length + "|, " + offset + ", " + newLength + ")"
//			)
//		else if (newLength <= array.length - offset)
//			copyOfRange(array, offset, offset + newLength)
//		else {
//			val res = Array.of(array.getClass.getComponentType.castParam[E], newLength)
//			ArrayLike.copy(array, 0, res, offset, math.min(array.length, newLength - offset))
//			res.castFrom[Array[E], IArray[E]]
//		}

	/** Copies a fragment of an array to a new array. This works similarly
	  * to `array.`[[scala.collection.ArrayOps.slice slice]]`(from, until)`, with a couple of exceptions:
	  *   1. the argument may be any `ArrayLike[E]`, and the result is an $Coll`[E]`,
	  *   1. `from` must be in `[0, array.length]` range, or an [[IndexOutOfBoundsException]] is thrown,
	  *   1. specifying `until > array.length` pads the returned array with nulls/zero values
	  *      until the length of `until - from` is reached.
	  */
	@inline def copyOfRange[E :ClassTag](array :ArrayLike[E], from :Int, until :Int) :IArray[E] =
		Array.copyOfRange(array, from, until).castFrom[Array[_], IArray[E]]
//
//	/** Copies a fragment of an array to a new array. This works similarly
//	  * to `array.`[[scala.collection.ArrayOps.slice slice]]`(from, until)`, with a couple of exceptions:
//	  *   1. the argument may be any `ArrayLike[E]`, and the result is an $Coll`[E]`,
//	  *   1. `from` must be in `[0, array.length]` range, or an [[IndexOutOfBoundsException]] is thrown,
//	  *   1. specifying `until > array.length` pads the returned array with nulls/zero values
//	  *      until the length of `until - from` is reached.
//	  */
//	def copyOfRange[E](array :IArray[E], from :Int, until :Int) :IArray[E] =
//		Array.copyOfRange(array.castFrom[ArrayLike[E], Array[_]], from, until).castFrom[Array[_], IArray[E]]

	/** Returns `array.slice(from, until)` as an `IArray[E]`. */
	@inline def copyOfRange[E](array :Array[E], from :Int, until :Int) :IArray[E] =
		array.slice(from, until).asInstanceOf[IArray[E]]

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
	  * @return An `Array[E]` of length `until1 - from1 + until2 - from2` (for `from1 <= until1 && from2 <= until2`),
	  *         as an `IArray[E]`, with the copied slices.
	  */
	@inline def copyOfRanges[E :ClassTag](array1 :ArrayLike[E], from1 :Int, until1 :Int,
	                                      array2 :ArrayLike[E], from2 :Int, until2 :Int) :IArray[E] =
		Array.copyOfRanges(array1, from1, until1, array2, from2, until2).castFrom[Array[E], IArray[E]]

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
	  * @return An `IArray[E]` of length `until1 - from1 + until2 - from2` (for `from1 <= until1 && from2 <= until2`),
	  *         with the copied slices. The element type of the `IArray` will equal the element type of `array1`.
	  */
	@inline def copyOfRanges[E](array1 :IArray[E], from1 :Int, until1 :Int, array2 :IArray[E], from2 :Int, until2 :Int)
			:IArray[E] =
		Array.copyOfRanges(
			array1.asInstanceOf[Array[E]], from1, until1, array2.asInstanceOf[Array[E]], from2, until2
		).castFrom[Array[E], IArray[E]]

	/** Copies slices from three array into a new array. Providing `until < from` has the same effect as `until == from`,
	  * that is copying nothing. However, `untilX > arrayX.length` is treated as if the source array
	  * where of length `untilX`, and contained zeros/nulls past its actual length.
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
	  * @return An `Array[E]` of length `until1 - from1 + until2 - from2 + until3 - from3` (for `untilN >= fromN`),
	  *         as an `IArray[E]`, with the copied slices.
	  */
	@inline def copyOfRanges[E :ClassTag](array1 :ArrayLike[E], from1 :Int, until1 :Int,
	                                      array2 :ArrayLike[E], from2 :Int, until2 :Int,
	                                      array3 :ArrayLike[E], from3 :Int, until3 :Int) :IArray[E] =
		Array.copyOfRanges(array1, from1, until1, array2, from2, until2, array3, from3, until3)
		     .castFrom[Array[E], IArray[E]]

	/** Copies slices from three array into a new array. Providing `until < from` has the same effect as `until == from`,
	  * that is copying nothing. However, `untilX > arrayX.length` is treated as if the source array
	  * where of length `untilX`, and contained zeros/nulls past its actual length.
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
	  * @return An `IArray[E]` of length `until1 - from1 + until2 - from2 + until3 - from3` (for `untilN >= fromN`),
	  *         with the copied slices. The element type of the array will be equal to the element type of `array1`.
	  */
	@inline def copyOfRanges[E](array1 :IArray[E], from1 :Int, until1 :Int,
	                            array2 :IArray[E], from2 :Int, until2 :Int,
	                            array3 :IArray[E], from3 :Int, until3 :Int) :IArray[E] =
		Array.copyOfRanges(
			array1.asInstanceOf[Array[E]], from1, until1, array2.asInstanceOf[Array[E]], from2, until2,
			array3.asInstanceOf[Array[E]], from3, until3
		).castFrom[Array[E], IArray[E]]

	/** Returns `array.slice(from, until)` as an `IArray[E]`. */
	@inline def slice[E](array :Array[E], from :Int, until :Int) :IArray[E] =
		array.slice(from, until).asInstanceOf[IArray[E]]

	/** Returns `Array.`[[net.noresttherein.sugar.arrays.extensions.ArrayCompanionExtension.copyOfRanges copyOfRanges]]`(array1, from1, until1, array2, from2, until2)`
	  * as an `IArray[E]`.
	  */
	@inline def slice[E](array1 :Array[E], from1 :Int, until1 :Int,
	                     array2 :Array[E], from2 :Int, until2 :Int) :IArray[E] =
		Array.copyOfRanges(array1, from1, until1, array2, from2, until2).asInstanceOf[IArray[E]]

	/** Returns `Array.`[[net.noresttherein.sugar.arrays.extensions.ArrayCompanionExtension.copyOfRanges copyOfRanges]]`(array1, from1, until1, array2, from2, until2, array3, from3, until3)`
	  * as an `IArray[E]`.
	  */
	@inline def slice[E](array1 :Array[E], from1 :Int, until1 :Int,
	                     array2 :Array[E], from2 :Int, until2 :Int,
	                     array3 :Array[E], from3 :Int, until3 :Int) :IArray[E] =
		Array.copyOfRanges(array1, from1, until1, array2, from2, until2, array3, from3, until3).asInstanceOf[IArray[E]]

	/** Creates an `IArray` with the same element type and contents as the argument array.
	  * @return `Array.copyOf(array)` as an `IArray[E]`.
	  */
	def from[E](array :Array[E]) :IArray[E] = ArrayFactory.copyOf(array, array.length).castFrom[Array[E], IArray[E]]

	@inline def from[E :ClassTag](array :ArrayLike[E]) :IArray[E] = copyOf(array)

	override def from[E :ClassTag](it :IterableOnce[E]) :IArray[E] = it match {
		case elems :View[E]                      => from(elems.iterator)
		case Wrapped(array)                      => array
		case elems :Iterable[E] if elems.isEmpty => empty[E]
		case iter  :Iterator[E] if !iter.hasNext => empty[E]
		case elems :Iterable[E]                  => elems.toArray[E].castFrom[Array[E], IArray[E]]
		case _                                   => it.iterator.toArray[E].castFrom[Array[E], IArray[E]]
	}

	@inline final override def apply[E :ClassTag](elems :E*) :IArray[E] = Array(elems :_*).asInstanceOf[IArray[E]]

	@inline def apply(first :Byte, rest :Byte*) :IArray[Byte] = Array(first, rest :_*).asInstanceOf[IArray[Byte]]
	@inline def apply(first :Short, rest :Short*) :IArray[Short] = Array(first, rest :_*).asInstanceOf[IArray[Short]]
	@inline def apply(first :Char, rest :Char*) :IArray[Char] = Array(first, rest :_*).asInstanceOf[IArray[Char]]
	@inline def apply(first :Int, rest :Int*) :IArray[Int] = Array(first, rest :_*).asInstanceOf[IArray[Int]]
	@inline def apply(first :Long, rest :Long*) :IArray[Long] = Array(first, rest :_*).asInstanceOf[IArray[Long]]
	@inline def apply(first :Float, rest :Float*) :IArray[Float] = Array(first, rest :_*).asInstanceOf[IArray[Float]]
	@inline def apply(first :Double, rest :Double*) :IArray[Double] = Array(first, rest :_*).asInstanceOf[IArray[Double]]
	@inline def apply(first :Boolean, rest :Boolean*) :IArray[Boolean] = Array(first, rest :_*).asInstanceOf[IArray[Boolean]]

	val emptyBooleanIArray :IArray[Boolean] = Array.emptyBooleanArray.castFrom[Array[Boolean], IArray[Boolean]]
	val emptyByteIArray    :IArray[Byte]    = Array.emptyByteArray.castFrom[Array[Byte], IArray[Byte]]
	val emptyCharIArray    :IArray[Char]    = Array.emptyCharArray.castFrom[Array[Char], IArray[Char]]
	val emptyDoubleIArray  :IArray[Double]  = Array.emptyDoubleArray.castFrom[Array[Double], IArray[Double]]
	val emptyFloatIArray   :IArray[Float]   = Array.emptyFloatArray.castFrom[Array[Float], IArray[Float]]
	val emptyIntIArray     :IArray[Int]     = Array.emptyIntArray.castFrom[Array[Int], IArray[Int]]
	val emptyLongIArray    :IArray[Long]    = Array.emptyLongArray.castFrom[Array[Long], IArray[Long]]
	val emptyShortIArray   :IArray[Short]   = Array.emptyShortArray.castFrom[Array[Short], IArray[Short]]
	val emptyObjectIArray  :IArray[AnyRef]  = Array.emptyObjectArray.castFrom[Array[AnyRef], IArray[AnyRef]]
	val emptyUnitIArray    :IArray[Unit]    = Array.emptyUnitArray.castFrom[Array[Unit], IArray[Unit]]
	val emptyAnyIArray     :IArray[Any]     = emptyObjectIArray.asInstanceOf[IArray[Any]]


	override def empty[E :ClassTag] :IArray[E] = empty(classTag[E].runtimeClass.castParam[E])

	/** An empty array of the specified element type. */
	def empty[E](elementType :Class[E]) :IArray[E] = (
		if (elementType == classOf[AnyRef]) emptyObjectIArray
		else if (elementType == classOf[Int]) emptyIntIArray
		else if (elementType == classOf[Long]) emptyLongIArray
		else if (elementType == classOf[Double]) emptyDoubleIArray
		else if (elementType == classOf[Byte]) emptyByteIArray
		else if (elementType == classOf[Char]) emptyCharIArray
		else if (elementType == classOf[Float]) emptyFloatIArray
		else if (elementType == classOf[Short]) emptyShortIArray
		else if (elementType == classOf[Boolean]) emptyBooleanIArray
		else Array.of(elementType, 0).castFrom[Array[E], IArray[E]]
	).castParam[E]

	override def newBuilder[E :ClassTag] :Builder[E, IArray[E]] =
		ArrayFactory.newBuilder[E].castParam2[IArray[E]]

	/** Builds an `IArray` of the specified element type. */
	def newBuilder[E](elementType :Class[E]) :Builder[E, IArray[E]] =
		ArrayFactory.newBuilder(elementType).castParam2[IArray[E]]


	/** A single element `IArray[E]`. */
	@inline final def one[E :ClassTag](elem :E) :IArray[E] = Array.one(elem).castFrom[Array[E], IArray[E]]
//
//	/** A single element `IArray[E]`. */
//	@inline final def single[E :ClassTag](elem :E) :IArray[E] = Array.one(elem).castFrom[Array[E], IArray[E]]

	/** A single element `IArray[Byte]`. */
	@inline final def one(elem :Byte) :IArray[Byte] = Array.one(elem).castFrom[Array[Byte], IArray[Byte]]
//
//	/** A single element `IArray[Byte]`. */
//	@inline final def single(elem :Byte) :IArray[Byte] = Array.one(elem).castFrom[Array[Byte], IArray[Byte]]

	/** A single element `IArray[Short]`. */
	@inline final def one(elem :Short) :IArray[Short] = Array.one(elem).castFrom[Array[Short], IArray[Short]]
//
//	/** A single element `IArray[Short]`. */
//	@inline final def single(elem :Short) :IArray[Short] = Array.one(elem).castFrom[Array[Short], IArray[Short]]

	/** A single element `IArray[Char]`. */
	@inline final def one(elem :Char) :IArray[Char] = Array.one(elem).castFrom[Array[Char], IArray[Char]]
//
//	/** A single element `IArray[Char]`. */
//	@inline final def single(elem :Char) :IArray[Char] = Array.one(elem).castFrom[Array[Char], IArray[Char]]

	/** A single element `IArray[Int]`. */
	@inline final def one(elem :Int) :IArray[Int] = Array.one(elem).castFrom[Array[Int], IArray[Int]]
//
//	/** A single element `IArray[Int]`. */
//	@inline final def single(elem :Int) :IArray[Int] = Array.one(elem).castFrom[Array[Int], IArray[Int]]

	/** A single element `IArray[Long]`. */
	@inline final def one(elem :Long) :IArray[Long] = Array.one(elem).castFrom[Array[Long], IArray[Long]]
//
//	/** A single element `IArray[Long]`. */
//	@inline final def single(elem :Long) :IArray[Long] = Array.one(elem).castFrom[Array[Long], IArray[Long]]

	/** A single element `IArray[Float]`. */
	@inline final def one(elem :Float) :IArray[Float] = Array.one(elem).castFrom[Array[Float], IArray[Float]]
//
//	/** A single element `IArray[Float]`. */
//	@inline final def single(elem :Float) :IArray[Float] = Array.one(elem).castFrom[Array[Float], IArray[Float]]

	/** A single element `IArray[Double]`. */
	@inline final def one(elem :Double) :IArray[Double] = Array.one(elem).castFrom[Array[Double], IArray[Double]]
//
//	/** A single element `IArray[Double]`. */
//	@inline final def single(elem :Double) :IArray[Double] = Array.one(elem).castFrom[Array[Double], IArray[Double]]

	/** A single element `IArray[Boolean]`. */
	@inline final def one(elem :Boolean) :IArray[Boolean] = Array.one(elem).castFrom[Array[Boolean], IArray[Boolean]]
//
//	/** A single element `IArray[Boolean]`. */
//	@inline final def single(elem :Boolean) :IArray[Boolean] = Array.one(elem).castFrom[Array[Boolean], IArray[Boolean]]


	/** A two element IArray of element type defined by the implicit `ClassTag`. */
	@inline final def two[E :ClassTag](first :E, second :E) :IArray[E] =
		Array.two(first, second).castFrom[Array[E], IArray[E]]

	/** An `IArray[Byte]` of two elements. */
	@inline final def two(first :Byte, second :Byte) :IArray[Byte] =
		Array.two(first, second).castFrom[Array[Byte], IArray[Byte]]

	/** An `IArray[Short]` of two elements. */
	@inline final def two(first :Short, second :Short) :IArray[Short] =
		Array.two(first, second).castFrom[Array[Short], IArray[Short]]

	/** An `IArray[Char]` of two elements. */
	@inline final def two(first :Char, second :Char) :IArray[Char] =
		Array.two(first, second).castFrom[Array[Char], IArray[Char]]

	/** An `IArray[Int]` of two elements. */
	@inline final def two(first :Int, second :Int) :IArray[Int] =
		Array.two(first, second).castFrom[Array[Int], IArray[Int]]

	/** An `IArray[Long]` of two elements. */
	@inline final def two(first :Long, second :Long) :IArray[Long] =
		Array.two(first, second).castFrom[Array[Long], IArray[Long]]

	/** An `IArray[Float]` of two elements. */
	@inline final def two(first :Float, second :Float) :IArray[Float] =
		Array.two(first, second).castFrom[Array[Float], IArray[Float]]

	/** An `IArray[Double]` of two elements. */
	@inline final def two(first :Double, second :Double) :IArray[Double] =
		Array.two(first, second).castFrom[Array[Double], IArray[Double]]

	/** An `IArray[Boolean]` of two elements. */
	@inline final def two(first :Boolean, second :Boolean) :IArray[Boolean] =
		Array.two(first, second).castFrom[Array[Boolean], IArray[Boolean]]



	/** An array filled with `n` copies of `elem`. */
	@inline final def const[X :ClassTag](n :Int)(elem :X) :IArray[X] = Array.const(n)(elem).castFrom[Array[X], IArray[X]]

	/** A complement of `Array.iterate` and `Array.unfold` provided by `Array` object, which creates
	  * an `IArray[X]` by recursively applying a partial function while defined to its own results and collecting
	  * all returned values. It is very similar to the standard [[IArray.iterate iterate]],
	  * but instead of a fixed number of iterations, the generator function `next` is called for its return values
	  * until it is no longer applicable, which marks the end of the collection.
	  * @param start first element added to the array.
	  * @param next  generator function returning subsequent elements for the array based on the previous one,
	  *              serving as the termination condition by indicating that it can no longer be applied
	  *              to the given argument.
	  * @tparam X element type of the generated array.
	  * @return an immutable array containing the sequence starting with `start`,
	  *         and resulting from recursively applying `next` to itself.
	  */
	@inline final def generate[X :ClassTag](start :X)(next :PartialFunction[X, X]) :IArray[X] =
		Array.generate(start)(next).castFrom[Array[X], IArray[X]]

	/** Builds an `IArray[X]` by recursively reapplying the given partial function to the initial element.
	  * Instead of listing a fixed number of elements, this method uses the generator function `next`
	  * as the termination condition and ends the recursion once it returns `None`. It is the opposite
	  * of [[scala.collection.IterableOnceOps.reduce reduce]] in the same way as
	  * [[scala.collection.IterableFactory.unfold unfold]] is the opposite
	  * of [[scala.collection.IterableOnceOps.fold fold]].
	  * @param start The first element added to the array.
	  * @param next  A generator function returning subsequent elements for the array based on the previous one,
	  *              or `None` to indicate the end of recursion.
	  * @tparam X the element type of the generated array.
	  * @return an immutable array containing the sequence starting with `start`
	  *         and resulting from recursively applying `next` to itself.
	  */
	@inline final def expand[X :ClassTag](start :X)(next :X => Option[X]) :IArray[X] =
		Array.expand(start)(next).castFrom[Array[X], IArray[X]]

	/** Similar to [[IArray.iterate Array.iterate]],
	  * but the iterating function accepts the positional index of the next element as an additional argument.
	  * @param start The first element of the created array.
	  * @param len   The size of the created array.
	  * @param f     A function generating subsequent elements following start.
	  *              The second element of the array will be `f(start, 1)`, the third `f(f(start, 1), 2)`, and so on.
	  */
	@inline def iterateWithIndex[X :ClassTag](start :X, len :Int)(f :(X, Int) => X) :IArray[X] =
		Array.iterateWithIndex(start, len)(f).castFrom[Array[X], IArray[X]]

	/** A stepper iterating over the range of an array. Indices out of range are skipped silently. */
	@inline def stepper[A, S <: Stepper[_]]
	                   (array :IArray[A], from :Int = 0, until :Int = Int.MaxValue)
	                   (implicit shape :StepperShape[A, S]) :S with EfficientSplit =
		ArrayLike.stepper(array, from, until)



	@inline def unapplySeq[E](array :IArray[E]) :UnapplySeqWrapper[E] =
		new UnapplySeqWrapper(array.castFrom[IArray[E], Array[E]])


	/** Wraps the underlying `Array[Byte]` without copying the data. */
	@inline def wrapByte(array :IArray[Byte]) :ArraySeq.ofByte =
		new ArraySeq.ofByte(array.castFrom[IArray[Byte], Array[Byte]])

	/** Wraps the underlying `Array[Short]` without copying the data. */
	@inline def wrapShort(array :IArray[Short]) :ArraySeq.ofShort =
		new ArraySeq.ofShort(array.castFrom[IArray[Short], Array[Short]])

	/** Wraps the underlying `Array[Char]` without copying the data. */
	@inline def wrapChar(array :IArray[Char]) :ArraySeq.ofChar =
		new ArraySeq.ofChar(array.castFrom[IArray[Char], Array[Char]])

	/** Wraps the underlying `Array[Int]` without copying the data. */
	@inline def wrapInt(array :IArray[Int]) :ArraySeq.ofInt =
		new ArraySeq.ofInt(array.castFrom[IArray[Int], Array[Int]])

	/** Wraps the underlying `Array[Long]` without copying the data. */
	@inline def wrapLong(array :IArray[Long]) :ArraySeq.ofLong =
		new ArraySeq.ofLong(array.castFrom[IArray[Long], Array[Long]])

	/** Wraps the underlying `Array[Float]` without copying the data. */
	@inline def wrapFloat(array :IArray[Float]) :ArraySeq.ofFloat =
		new ArraySeq.ofFloat(array.castFrom[IArray[Float], Array[Float]])

	/** Wraps the underlying `Array[Double]` without copying the data. */
	@inline def wrapDouble(array :IArray[Double]) :ArraySeq.ofDouble =
		new ArraySeq.ofDouble(array.castFrom[IArray[Double], Array[Double]])

	/** Wraps the underlying `Array[Boolean]` without copying the data. */
	@inline def wrapBoolean(array :IArray[Boolean]) :ArraySeq.ofBoolean =
		new ArraySeq.ofBoolean(array.castFrom[IArray[Boolean], Array[Boolean]])

	@inline def wrapRef[E <: AnyRef](array :IArray[E]) :ArraySeq.ofRef[E] =
		new ArraySeq.ofRef(array.castFrom[IArray[E], Array[E]])



	/** Wraps and unwraps immutable `IndexedSeq` instances and other immutable collections backed by arrays
	  * in a safe manner. Arrays are represented as [[net.noresttherein.sugar.arrays.IArray IArray]] instances
	  * to prevent accidental modification and ensure that the user is aware that an array will be represented
	  * as an immutable structure. Additionally, extractor relies on a `ClassTag` to verify
	  * that the array element type is a subtype of the nominal collection's element type (and not an `Array[Any]`,
	  * for example). Note however, that matching against an `Iterable[Any]` will still succeed,
	  * and return an `IArray[Any]`, even if the underlying array is an `Array[Int]`. This follows the general rule
	  * that an `IArray[A]` can be safely cast to an `Array[A]` only if `A` is a built in value type.
	  */
	@SerialVersionUID(Ver)
	object Wrapped {
		private[this] val wrapper :ArrayLikeWrapper[IndexedSeq, IArray] =
			RelayArrayFactory getOrElse IArraySlice

		def apply[A](array :IArray[A]) :IndexedSeq[A] = wrapper.wrap(array)

		def unapply[E :ClassTag](elems :IterableOnce[E]) :Opt[IArray[E]] = {
			val array = elems match {
				case slice :ArrayIterableOnce[_] if slice.isImmutable && slice.knownSize == slice.unsafeArray.length =>
					Got(slice.unsafeArray)
				case seq :ArraySeq[_] => Got(seq.unsafeArray)
				case _                => Lack
			}
			val tag = classTag[E]
			if (array.isDefined && (tag == ClassTag.Any || array.get.getClass.getComponentType <:< tag.runtimeClass))
				array.castFrom[Opt[Array[_]], Opt[IArray[E]]]
			else
				Lack
		}

		/** Factory of views on slices of immutable arrays as indexed sequences,
		  * and unwraps known immutable collections backed by array slices.
		  */
		@SerialVersionUID(Ver)
		object Slice {
			private[this] val wrapper :ArrayLikeSliceWrapper[IndexedSeq, IArray] =
				RelayArrayFactory getOrElse IArraySlice

			def apply[E](array :IArray[E], from :Int, until :Int) :IndexedSeq[E] = wrapper.slice(array, from, until)

			def unapply[E :ClassTag](elems :IterableOnce[E]) :Opt[(IArray[E], Int, Int)] = {
				val tag = classTag[E]
				val expectedClass = tag.runtimeClass
				elems match {
					case seq :Vector[_] if seq.length <= CheatedAccess.FlatVectorSize && tag == ClassTag.Any =>
						Got((CheatedAccess.array(seq).castFrom[Array[_], IArray[E]], 0, seq.length))
					case seq :ArraySeq[_]
						if tag == ClassTag.Any || seq.unsafeArray.getClass.getComponentType <:< expectedClass
					=>
						Got((seq.unsafeArray.castFrom[Array[_], IArray[E]], 0, seq.unsafeArray.length))

					case slice :ArrayIterableOnce[E] if elems.knownSize >= 0 && slice.isImmutable =>
						val array = slice.unsafeArray.castFrom[Array[_], IArray[E]]
						if (tag == ClassTag.Any || array.getClass.getComponentType <:< expectedClass)
							Got((array, slice.startIndex, slice.startIndex + slice.knownSize))
						else
							Lack
					case _ =>
						Lack
				}
			}
		}
	}



	//todo: specialize
	//Does having them in a 'companion object' even makes sense, or is it better to move it to extensions?
	implicit def IArrayOrdering[E :Ordering] :Ordering[IArray[E]] =
		Array.ArrayOrdering[E].castFrom[Ordering[Array[E]], Ordering[IArray[E]]]

	implicit def IArrayClassTag[E :ClassTag] :ClassTag[IArray[E]] = classTag[Array[E]].castParam[IArray[E]]
//
//	implicit def IArrayToSeq[E](array :IArray[E]) :IndexedSeq[E] = Wrapped(array)

	/** A type class promoting immutable arrays to sequences. */
	implicit def IArrayIsSeq[E] :IsSeq[IArray[E]] { type A = E; type C = IArray[E] } =
		IsSeqPrototype.asInstanceOf[IsSeq[IArray[E]] { type A = E; type C = IArray[E] }]

	private[this] val IsSeqPrototype = new IArrayIsSeq[Any]

	private class IArrayIsSeq[E] extends ArrayLikeIsSeqTemplate[E, Seq, IArray] {
		override def apply(coll :IArray[E]) =
			new IArrayLikeIsSeqOps[E, IArray](coll) {
				protected override def fromSpecific(coll :IterableOnce[E]) :IArray[E] =
					IArray.from(coll)(ClassTag[E](this.coll.getClass.getComponentType))

				protected override def newSpecificBuilder :Builder[E, IArray[E]] =
					IArray.newBuilder(this.coll.getClass.getComponentType.castParam[E])
			}
		private def readResolve = IArrayIsSeq
	}


//	/** A type class defining the specific `immutable.ArraySeq` subclass for the given element type.
//	  * Used in implicit conversion from `IArray` to `this.Wrapped`.
//	  */
//	/* This type exists because without it we would need separate implicit conversion methods for each type,
//	 * Which would then cause conflict with non-specific conversions to extension classes.
//	 * It is either this, or having ByteArrayAsArrayLikeExtension, ByteArrayAsIArrayLikeExtension, and so on.
//	 * It also means there is a single method to import instead of a couple dozen.
//	 */
//	sealed abstract class ArraySeqType[E] extends Serializable {
//		type Wrapped <: ArraySeq[E]
//		def apply(array :Array[E]) :Wrapped
//	}
//
//	private[IArray] sealed abstract class Rank1ArraySeqType {
//		@inline implicit final def GenericArraySeqType[E] :GenericArraySeqType[E] =
//			GenericArraySeqTypePrototype.asInstanceOf[GenericArraySeqType[E]]
//		private[this] final val GenericArraySeqTypePrototype = new GenericArraySeqType[Any]
//	}
//
//	@SerialVersionUID(Ver)
//	object ArraySeqType {
//		implicit case object ByteArraySeqType extends ArraySeqType[Byte] {
//			override type Wrapped = ArraySeq.ofByte
//			@inline override def apply(array :Array[Byte]) :ArraySeq.ofByte = new ArraySeq.ofByte(array)
//		}
//		implicit case object ShortArraySeqType extends ArraySeqType[Short] {
//			override type Wrapped = ArraySeq.ofShort
//			@inline override def apply(array :Array[Short]) :ArraySeq.ofShort = new ArraySeq.ofShort(array)
//		}
//		implicit case object CharArraySeqType extends ArraySeqType[Char] {
//			override type Wrapped = ArraySeq.ofChar
//			@inline override def apply(array :Array[Char]) :ArraySeq.ofChar = new ArraySeq.ofChar(array)
//		}
//		implicit case object IntArraySeqType extends ArraySeqType[Int] {
//			override type Wrapped = ArraySeq.ofInt
//			@inline override def apply(array :Array[Int]) :ArraySeq.ofInt = new ArraySeq.ofInt(array)
//		}
//		implicit case object LongArraySeqType extends ArraySeqType[Long] {
//			override type Wrapped = ArraySeq.ofLong
//			@inline override def apply(array :Array[Long]) :ArraySeq.ofLong = new ArraySeq.ofLong(array)
//		}
//		implicit case object FloatArraySeqType extends ArraySeqType[Float] {
//			override type Wrapped = ArraySeq.ofFloat
//			@inline override def apply(array :Array[Float]) :ArraySeq.ofFloat = new ArraySeq.ofFloat(array)
//		}
//		implicit case object DoubleArraySeqType extends ArraySeqType[Double] {
//			override type Wrapped = ArraySeq.ofDouble
//			@inline override def apply(array :Array[Double]) :ArraySeq.ofDouble = new ArraySeq.ofDouble(array)
//		}
//		implicit case object BooleanArraySeqType extends ArraySeqType[Boolean] {
//			override type Wrapped = ArraySeq.ofBoolean
//			@inline override def apply(array :Array[Boolean]) :ArraySeq.ofBoolean = new ArraySeq.ofBoolean(array)
//		}
//		implicit def RefArraySeqType[E <: AnyRef] :RefArraySeqType[E] =
//			RefArraySeqTypePrototype.asInstanceOf[RefArraySeqType[E]]
//
//		final class RefArraySeqType[E <: AnyRef] private[IArray] extends ArraySeqType[E] {
//			override type Wrapped = ArraySeq.ofRef[E]
//			@inline override def apply(array :Array[E]) :ArraySeq.ofRef[E] = new ArraySeq.ofRef(array)
//			override def toString = "RefArraySeqType"
//		}
//		final class GenericArraySeqType[E] private[IArray] extends ArraySeqType[E] {
//			override type Wrapped = ArraySeq[E]
//			@inline override def apply(array :Array[E]) :ArraySeq[E] = ArraySeq.unsafeWrapArray(array)
//			override def toString = "GenericArraySeqType"
//		}
//		private[this] final val RefArraySeqTypePrototype = new RefArraySeqType[AnyRef]
//	}


//	private[arrays] sealed trait IArrayRank2Conversions extends Any with IArrayLike.conversions {
//		@inline implicit final def IArrayToSeq[E](array :IArray[E]) :IndexedSeq[E] = IArray.Wrapped(array)
//	}
//
//	/** Conversions of `IArray` of various element types to their dedicated
//	  * [[collection.immutable.ArraySeq ArraySeq]].
//	  */ //todo: see if we can drop these conversions without causing conflicts with Predef
//	private[arrays] sealed trait IArrayRank1Conversions extends Any with IArrayRank2Conversions {
//		@inline implicit final def byteIArrayToSeq(array :IArray[Byte]) :ArraySeq.ofByte =
//			new ArraySeq.ofByte(array.castFrom[IArray[Byte], Array[Byte]])
//
//		@inline implicit final def shortIArrayToSeq(array :IArray[Short]) :ArraySeq.ofShort =
//			new ArraySeq.ofShort(array.castFrom[IArray[Short], Array[Short]])
//
//		@inline implicit final def charIArrayToSeq(array :IArray[Char]) :ArraySeq.ofChar =
//			new ArraySeq.ofChar(array.castFrom[IArray[Char], Array[Char]])
//
//		@inline implicit final def intIArrayToSeq(array :IArray[Int]) :ArraySeq.ofInt =
//			new ArraySeq.ofInt(array.castFrom[IArray[Int], Array[Int]])
//
//		@inline implicit final def longIArrayToSeq(array :IArray[Long]) :ArraySeq.ofLong =
//			new ArraySeq.ofLong(array.castFrom[IArray[Long], Array[Long]])
//
//		@inline implicit final def floatIArrayToSeq(array :IArray[Float]) :ArraySeq.ofFloat =
//			new ArraySeq.ofFloat(array.castFrom[IArray[Float], Array[Float]])
//
//		@inline implicit final def doubleIArrayToSeq(array :IArray[Double]) :ArraySeq.ofDouble =
//			new ArraySeq.ofDouble(array.castFrom[IArray[Double], Array[Double]])
//
//		@inline implicit final def booleanIArrayToSeq(array :IArray[Boolean]) :ArraySeq.ofBoolean =
//			new ArraySeq.ofBoolean(array.castFrom[IArray[Boolean], Array[Boolean]])
//
//		@inline implicit final def refIArrayToSeq[E <: AnyRef](array :IArray[E]) :ArraySeq.ofRef[E] =
//			new ArraySeq.ofRef(array.castFrom[IArray[E], Array[E]])
//	}

//	private[arrays] sealed trait conversions extends Any with IArrayLike.conversions {
////		@inline implicit final def IArrayToSeq[E](self :Array[E])(implicit wrapper :ArraySeqType[E]) :wrapper.Wrapped =
////			wrapper(self)
//	}

//	private[arrays] sealed trait IArrayRank2Extensions extends Any with ArrayLike.extensions {
//		//todo: drop these and use a type class in each extension class
//		/** Adds various additional folding methods with a break condition to any `IArray`. */
//		@inline implicit final def IArrayAsIterableOnceExtension[E](self :IArray[E]) :IterableOnceExtension[E] =
//			new IterableOnceExtension[E](new IArrayAsSeq(self))
//
//		/** Adds various methods for mapping/flatMapping collections to any `IArray`.
//		  * These either pass along additional state, or have a break condition. Roughly equivalent to working
//		  * with `toLazyList.scan`, but cleaner and more efficient.
//		  */
//		@inline implicit final def IArrayAsIterableExtension[E](self :IArray[E])
//				:IterableExtension[E, IRefArray, IArray[E]] =
//			new IterableExtension[E, IRefArray, IArray[E]](new IArrayAsSeq(self))
//
//		/** Alternative, safer implementations of [[scala.collection.SeqOps.indexOf indexOf]] for immutable arrays,
//		  * which do not return a negative index when the element is not found.
//		  */
//	   @inline implicit final def IArrayAsSeqExtension[E](self :IArray[E]) :SeqExtension[E, IRefArray, IArray[E]] =
//			new SeqExtension[E, IRefArray, IArray[E]](new IArrayAsSeq(self))
//
//		/** Operations on suffixes of a sequence and binary search methods on sorted immutable arrays. */
//		@inline implicit final def IArrayAsIndexedSeqExtension[E](self :IArray[E])
//				:IndexedSeqExtension[E, IRefArray, IArray[E]] =
//			new IndexedSeqExtension[E, IRefArray, IArray[E]](new IArrayAsSeq(self))
//	}
//
//	private[arrays] sealed trait IArrayRank1Extensions extends Any with IArrayRank2Extensions {
//		@inline implicit final def GenericIArrayExtension[E](array :IArray[E]) :GenericIArrayExtension[E] =
//			new GenericIArrayExtension[E](array.castFrom[IArray[E], Array[E]])
//	}

/*
	private[arrays] sealed trait IArrayRank1Extensions extends Any with IArrayRank2Extensions {
		@inline implicit final def byteIArrayAsArrayLikeExtension(array :IArray[Byte])
				:ArrayLike.ArrayLikeExtension[Byte, IArray] =
			new ArrayLike.ArrayLikeExtension(array.castFrom[IArray[Byte], Array[_]])

		@inline implicit final def shortIArrayAsArrayLikeExtension(array :IArray[Short])
				:ArrayLike.ArrayLikeExtension[Short, IArray] =
			new ArrayLike.ArrayLikeExtension(array.castFrom[IArray[Short], Array[_]])

		@inline implicit final def charIArrayAsArrayLikeExtension(array :IArray[Char])
				:ArrayLike.ArrayLikeExtension[Char, IArray] =
			new ArrayLike.ArrayLikeExtension(array.castFrom[IArray[Char], Array[_]])

		@inline implicit final def intIArrayAsArrayLikeExtension(array :IArray[Int])
				:ArrayLike.ArrayLikeExtension[Int, IArray] =
			new ArrayLike.ArrayLikeExtension(array.castFrom[IArray[Int], Array[_]])

		@inline implicit final def longIArrayAsArrayLikeExtension(array :IArray[Long])
				:ArrayLike.ArrayLikeExtension[Long, IArray] =
			new ArrayLike.ArrayLikeExtension(array.castFrom[IArray[Long], Array[_]])

		@inline implicit final def floatIArrayAsArrayLikeExtension(array :IArray[Float])
				:ArrayLike.ArrayLikeExtension[Float, IArray] =
			new ArrayLike.ArrayLikeExtension(array.castFrom[IArray[Float], Array[_]])

		@inline implicit final def doubleIArrayAsArrayLikeExtension(array :IArray[Double])
				:ArrayLike.ArrayLikeExtension[Double, IArray] =
			new ArrayLike.ArrayLikeExtension(array.castFrom[IArray[Double], Array[_]])

		@inline implicit final def booleanIArrayAsArrayLikeExtension(array :IArray[Boolean])
				:ArrayLike.ArrayLikeExtension[Boolean, IArray] =
			new ArrayLike.ArrayLikeExtension(array.castFrom[IArray[Boolean], Array[_]])

		@inline implicit final def refIArrayAsArrayLikeExtension[E <: AnyRef](array :IArray[E])
				:ArrayLike.ArrayLikeExtension[E, IArray] =
			new ArrayLike.ArrayLikeExtension(array.castFrom[IArray[E], Array[_]])
//
//		@inline implicit final def IArrayAsArrayLikeExtension[E](array :IArray[E])
//				:ArrayLike.ArrayLikeExtension[E, IArray] =
//			new ArrayLike.ArrayLikeExtension(array.castFrom[IArray[E], Array[_]])
	}
*/

	/** Conversions providing extension methods for `IArray`. */
	private[arrays] trait extensions extends Any with IArrayLike.extensions  {

		//consider: where to put ClassTag and Ordering so that it is consistent.
//		/** Adapts (casts) a `ClassTag` for an `Array[E]` to an `IArray[E]`. */
//		@inline implicit final def IArrayClassTag[E](implicit tag :ClassTag[Array[E]]) :ClassTag[IArray[E]] =
//			tag.castParam[IArray[E]]

		@inline implicit final def ByteIArrayExtension(array :IArray[Byte]) :ByteIArrayExtension =
			new ByteIArrayExtension(array.castFrom[IArray[Byte], Array[Byte]])

		@inline implicit final def ShortIArrayExtension(array :IArray[Short]) :ShortIArrayExtension =
			new ShortIArrayExtension(array.castFrom[IArray[Short], Array[Short]])

		@inline implicit final def CharIArrayExtension(array :IArray[Char]) :CharIArrayExtension =
			new CharIArrayExtension(array.castFrom[IArray[Char], Array[Char]])

		@inline implicit final def IntIArrayExtension(array :IArray[Int]) :IntIArrayExtension =
			new IntIArrayExtension(array.castFrom[IArray[Int], Array[Int]])

		@inline implicit final def LongIArrayExtension(array :IArray[Long]) :LongIArrayExtension =
			new LongIArrayExtension(array.castFrom[IArray[Long], Array[Long]])

		@inline implicit final def FloatIArrayExtension(array :IArray[Float]) :FloatIArrayExtension =
			new FloatIArrayExtension(array.castFrom[IArray[Float], Array[Float]])

		@inline implicit final def DoubleIArrayExtension(array :IArray[Double]) :DoubleIArrayExtension =
			new DoubleIArrayExtension(array.castFrom[IArray[Double], Array[Double]])

		@inline implicit final def BooleanIArrayExtension(array :IArray[Boolean]) :BooleanIArrayExtension =
			new BooleanIArrayExtension(array.castFrom[IArray[Boolean], Array[Boolean]])

		@inline implicit final def RefIArrayExtension[E <: AnyRef](array :IArray[E]) :RefIArrayExtension[E] =
			new RefIArrayExtension(array.castFrom[IArray[E], Array[E]])

/*
		//manually specialized conversions to enforce precedence over specialized conversions to ArraySeq
		@inline implicit final def byteIArrayExtension(array :IArray[Byte]) :IArrayExtension[Byte] =
			new IArrayExtension(array.castFrom[IArray[Byte], Array[Byte]])

		@inline implicit final def shortIArrayExtension(array :IArray[Short]) :IArrayExtension[Short] =
			new IArrayExtension(array.castFrom[IArray[Short], Array[Short]])

		@inline implicit final def charIArrayExtension(array :IArray[Char]) :IArrayExtension[Char] =
			new IArrayExtension(array.castFrom[IArray[Char], Array[Char]])

		@inline implicit final def intIArrayExtension(array :IArray[Int]) :IArrayExtension[Int] =
			new IArrayExtension(array.castFrom[IArray[Int], Array[Int]])

		@inline implicit final def longIArrayExtension(array :IArray[Long]) :IArrayExtension[Long] =
			new IArrayExtension(array.castFrom[IArray[Long], Array[Long]])

		@inline implicit final def floatIArrayExtension(array :IArray[Float]) :IArrayExtension[Float] =
			new IArrayExtension(array.castFrom[IArray[Float], Array[Float]])

		@inline implicit final def doubleIArrayExtension(array :IArray[Double]) :IArrayExtension[Double] =
			new IArrayExtension(array.castFrom[IArray[Double], Array[Double]])

		@inline implicit final def booleanIArrayExtension(array :IArray[Boolean]) :IArrayExtension[Boolean] =
			new IArrayExtension(array.castFrom[IArray[Boolean], Array[Boolean]])

		@inline implicit final def refIArrayExtension[E <: AnyRef](array :IArray[E]) :IArrayExtension[E] =
			new IArrayExtension(array.castFrom[IArray[E], Array[E]])
*/

//		@inline implicit final def IArrayExtension[E](array :IArray[E]) :IArrayExtension[E] =
//			new IArrayExtension(array.castFrom[IArray[E], Array[E]])
		@inline implicit final def IArrayExtension[E] :IArrayExtensionConversion[E] =
			extensions.IArrayExtensionConversionPrototype.asInstanceOf[IArrayExtensionConversion[E]]
	}


	/** Conversions providing extension methods for `IArray`. */
	@SerialVersionUID(Ver)
	object extensions extends extensions {
		sealed trait IArrayExtensionConversion[E] extends (IArray[E] => IArrayExtension[E]) {
			@inline final def apply(v1 :IArray[E])(implicit dummy :DummyImplicit) :IArrayExtension[E] =
				new IArrayExtension(v1.asInstanceOf[Array[E]])
		}
		private def newIArrayExtensionConversion[E] =
			new PriorityConversion.Wrapped[IArray[E], IArrayExtension[E]](
				(arr :IArray[E]) => new IArrayExtension(arr.asInstanceOf[Array[E]])
			) with IArrayExtensionConversion[E]
		private val IArrayExtensionConversionPrototype :IArrayExtensionConversion[Any] = newIArrayExtensionConversion
	}
}
