package net.noresttherein.sugar.arrays


import scala.Array.emptyObjectArray
import scala.annotation.tailrec
import scala.collection.generic.IsSeq
import scala.collection.{IterableFactory, View}
import scala.collection.immutable.{ArraySeq, IndexedSeqOps}
import scala.collection.mutable.Builder
import scala.reflect.{ClassTag, classTag}

import net.noresttherein.sugar.arrays.IRefArray.extensions.IRefArrayExtensionConversion
import net.noresttherein.sugar.arrays.extensions.ArrayCompanionExtension
import net.noresttherein.sugar.collections.{ArrayIterableOnce, ArrayLikeSliceFactory, IRefArraySlice, PassedArrayFactory}
import net.noresttherein.sugar.collections.extensions.{IterableExtension, IteratorExtension}
import net.noresttherein.sugar.extensions.IterableOnceExtension
import net.noresttherein.sugar.typist.casting.extensions.{castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.typist.PriorityConversion
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/** A factory of `IRefArray` - immutable values with `Array` API available as extension methods, represented in runtime
  * always as `Array[AnyRef]`, regardless of their actual type parameter.
  * Aside from standard factory methods inherited from `IterableFactory`, and those modelled after
  * factory methods in [[Array]] and [[java.util.Arrays]], there is another convenient way of initializing
  * a new instance without a need for an intermediate collection in method
  * [[net.noresttherein.sugar.arrays.IRefArray.init init]], which grants a temporary access to the new object
  * as a regular `Array`:
  * {{{
  *     IRefArray.init[String](3){ array =>
  *         array(0) = "You"
  *         array(1) = "Boo"
  *         array(2) = "I"
  *     }
  * }}}
  * @define Coll `IRefArray`
  * @define coll immutable reference object array
  */
@SerialVersionUID(Ver)
case object IRefArray extends RefArrayLikeFactory[IRefArray] with IterableFactory[IRefArray] {
	/** Extension methods specific to [[net.noresttherein.sugar.arrays.IRefArray IRefArray]] only -
	  * standard conversions to immutable sequences implemented by wrapping the underlying array.
	  * Enabled by importing any of
	  * `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.IRefArrayExtension IRefArrayExtension]],
	  * `net.noresttherein.sugar.extensions.IRefArrayExtension`, or
	  * `net.noresttherein.sugar.arrays.RefArray.extensions.IRefArrayExtension`.
	  * @see [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension]] imported as
	  *      `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.ArrayLikeExtension ArrayLikeExtension]],
	  *      `net.noresttherein.sugar.extensions.ArrayLikeExtension`, or
	  *      `net.noresttherein.sugar.arrays.ArrayLike.extensions.ArrayLikeExtension`.
	  * @see [[net.noresttherein.sugar.arrays.RefArrayLike.RefArrayLikeExtension]] imported as
	  *      `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.RefArrayLikeExtension RefArrayLikeExtension]],
	  *      `net.noresttherein.sugar.extensions.RefArrayLikeExtension`, or
	  *      `net.noresttherein.sugar.arrays.RefArrayLike.extensions.RefArrayLikeExtension`.
	  * @see [[net.noresttherein.sugar.arrays.IArrayLike.IArrayLikeExtension]] imported as
	  *      `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.IArrayLikeExtension IRefArrayLikeExtension]],
	  *      `net.noresttherein.sugar.extensions.IArrayLikeExtension`, or
	  *      `net.noresttherein.sugar.arrays.IArrayLike.extensions.IArrayLikeExtension`.
	  */
	class IRefArrayExtension[E] private[arrays](private val self :Array[Any]) extends AnyVal {
		@inline def take(n :Int) :IRefArray[E] =
			(if (n <= 0) Array.emptyAnyArray
			else if (n >= self.length) self
			else self.take(n)).castFrom[Array[Any], IRefArray[E]]

		@inline def drop(n :Int) :IRefArray[E] =
			(if (n <= 0) self
			else if (n >= self.length) Array.emptyAnyArray
			else self.drop(n)).asInstanceOf[IRefArray[E]]

		@inline def takeRight(n :Int) :IRefArray[E] =
			(if (n <= 0) Array.emptyAnyArray
			else if (n >= self.length) self
			else self.drop(self.length - n)).castFrom[Array[Any], IRefArray[E]]

		@inline def dropRight(n :Int) :IRefArray[E] =
			(if (n <= 0) self
			else if (n >= self.length) Array.emptyAnyArray
			else self.take(self.length - n)).castFrom[Array[Any], IRefArray[E]]

		@inline def takeWhile(p :E => Boolean) :IRefArray[E] = take(self.segmentLength(p.asInstanceOf[Any => Boolean]))
		@inline def dropWhile(p :E => Boolean) :IRefArray[E] = drop(self.segmentLength(p.asInstanceOf[Any => Boolean]))
		@inline def slice(from :Int, until :Int) :IRefArray[E] =
			(if (until <= from | until < 0 || from >= self.length) Array.emptyAnyArray
			else if (from <= 0 && until >= self.length) self
			else self.slice(from, until)).castFrom[Array[Any], IRefArray[E]]

		@inline def splitAt(n :Int) :(IRefArray[E], IRefArray[E]) = (take(n), drop(n))

		@inline def span(p :E => Boolean) :(IRefArray[E], IRefArray[E]) =
			self.segmentLength(p.asInstanceOf[Any => Boolean]) match {
				case 0 =>
					(IRefArray.empty[E], self.castFrom[Array[Any], IRefArray[E]])
				case n if n == self.length =>
					(self.castFrom[Array[Any], IRefArray[E]], IRefArray.empty[E])
				case n =>
					(self.take(n).castFrom[Array[Any], IRefArray[E]],
						self.drop(n).castFrom[Array[Any], IRefArray[E]])
			}

		/** A view on the index range `[from, until)` of this array as a sequence.
		  * Slicing of the returned sequence will return similar views, sharing the same underlying array.
		  */
		@inline def subseq(from :Int, until :Int) :IndexedSeq[E] =
			Wrapped.Slice(self.asInstanceOf[IRefArray[E]], from, until)

		@inline def toSeq        :Seq[E] = toIndexedSeq
		@inline def toIndexedSeq :IndexedSeq[E] = Wrapped(self.castFrom[Array[Any], IRefArray[E]])
		@inline def toOps        :IndexedSeqOps[E, IRefArray, IRefArray[E]] =
			new IRefArrayAsSeq(self.asInstanceOf[IRefArray[E]])
	}


	/** Creates a new `Array[Any]` of the specified length, executes the given initialization function for it,
	  * and returns it as an $Coll. It is a pattern for initialization safer than manually creating
	  * an `Array[Any]`, writing to it, and casting it $Coll. The application should ''not'' retain a reference
	  * to the array given as the function argument, or the immutability of the result may be voided.
	  * {{{
	  *     IRefArray.init[String](3) { array =>
	  *         array(0) = "You"
	  *         array(1) = "Boo"
	  *         array(2) = "I"
	  *     }
	  * }}}
	  */
	@throws[NegativeArraySizeException]("if length is negative")
	@inline def init[E](length :Int)(f :Array[Any] => Unit) :IRefArray[E] = {
		val res = if (length == 0) emptyObjectArray.asInstanceOf[Array[Any]] else new Array[Any](length)
		f(res)
		res.castFrom[Array[Any], IRefArray[E]]
	}

	/** Creates a new $Coll by modifying another `ArrayLike`. This method combines [[Array.copyOf copyOf]]`(other)`
	  * with [[net.noresttherein.sugar.arrays.IRefArray.init init]]: the latter, instead of an empty
	  * array filled with default values, receives an object array filled with copied contents of `other`.
	  *
	  * It allows to introduce multiple changes to an array in a manner safer than starting with a mutable `Array`
	  * and casting it to an $Coll, but without a need to chain multiple calls to (extension) methods
	  * of the `original` (which would require wasteful creation of intermediate results). Additionally,
	  * using [[java.util.Arrays Arrays]]`.`[[java.util.Arrays.copyOf copyOf]] is faster than creating an array
	  * filled with `null` values, and copying contents to it afterwards.
	  * @param other A prototype array with values passed to the initialization function.
	  * @param f     An initialization function, accepting a fresh reference array, allowed to modify it as will.
	  *              Should not retain the reference to the argument after its completion, or immutability
	  *              of the result will be compromised.
	  */
	def updated[E](other :ArrayLike[E])(f :Array[Any] => Unit) :IRefArray[E] = {
		val res = ArrayFactory.copyOf[Any](other)(ClassTag.Any)
		f(res)
		res.castFrom[Array[Any], IRefArray[E]]
	}

	/** Creates a new $Coll by copying and modifying contents of another `ArrayLike`.
	  * This method combines `Array.`[[scala.Array.copyOf copyOf]]`(other, newLength)`
	  * with [[net.noresttherein.sugar.arrays.IRefArray.init init]]: the latter, instead of an empty
	  * array filled with default values, receives an object array with copied contents of `other`.
	  *
	  * It allows to introduce multiple changes to an array in a manner safer than starting with a mutable `Array`
	  * and casting it to an $Coll, but without a need to chain multiple calls to (extension) methods
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
	@throws[NegativeArraySizeException]("if newLength is negative")
	@inline def updated[E](other :ArrayLike[E], newLength :Int)(f :Array[Any] => Unit) :IRefArray[E] = {
		val res = ArrayFactory.copyOf[Any](other, newLength)(ClassTag.Any)
		f(res)
		res.castFrom[Array[Any], IRefArray[E]]
	}

	/** Creates a new $Coll by introducing changes to a slice of another `ArrayLike`. This method combines
	  * [[net.noresttherein.sugar.collections.extensions.ArrayCompanionExtension.copyOfRange copyOfRange]]`(other, from, until)`
	  * with [[net.noresttherein.sugar.arrays.IRefArray.init init]]: the latter, instead of an empty
	  * array filled with default values, receives a slice of the original.
	  *
	  * It allows to introduce multiple changes to an array in a manner safer than starting with a mutable `Array`
	  * and casting it to an $Coll, but without a need to chain multiple calls to (extension) methods
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
	@inline def updated[E](other :ArrayLike[E], from :Int, until :Int)(f :Array[Any] => Unit) :IRefArray[E] = {
		val res = Array.copyOfRange[Any](other, from, until)(ClassTag.Any)
		f(res)
		res.castFrom[Array[Any], IRefArray[E]]
	}

	/** Creates a new $Coll by introducing changes to a slice of another `ArrayLike`, including, potentially
	  * appending additional elements. This method combines
	  * [[net.noresttherein.sugar.arrays.extensions.ArrayCompanionExtension.copyOfRange copyOfRange]]`(other, from, until, newLength)`
	  * with [[net.noresttherein.sugar.arrays.IRefArray.init init]]: the latter, instead of an empty
	  * array filled with default values, receives an object array of the specified length, with its prefix already
	  * initialized to the copy of the index range `[from, until)` of the original.
	  *
	  * It allows to introduce multiple changes to an array in a manner safer than starting with a mutable `Array`
	  * and casting it to an $Coll, but without a need to chain multiple calls to (extension) methods
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
	@inline def updated[E](other :ArrayLike[E], from :Int, until :Int, newLength :Int)
	                      (f :Array[Any] => Unit) :IRefArray[E] =
	{
		val res = Array.copyOfRange[Any](other, from, until, newLength)(ClassTag.Any)
		f(res)
		res.castFrom[Array[Any], IRefArray[E]]
	}

	/** Creates a new $Coll of the specified length by modifying a slice of another `ArrayLike`. This method combines
	  * [[net.noresttherein.sugar.arrays.extensions.ArrayCompanionExtension.copyOfRange copyOfRange]]`(other, from, until, offset, newLength)`
	  * with [[net.noresttherein.sugar.arrays.IArray.init init]]: the latter, instead of an empty
	  * array filled with default values, receives an array created as with:
	  * {{{
	  *     val res = new Array[Any](newLength)
	  *     val src = from max 0 min other.length
	  *     val len = (until min other.length max from) - from
	  *     Array.copy(other, src, res, offset min newLength, len)
	  *     f(res)
	  * }}}
	  * However, depending on the arguments, the method might choose an alternative implementation if it's deemed
	  * to be more efficient.
	  *
	  * It allows to introduce multiple changes to an array in a manner safer than starting with a mutable `Array`
	  * and casting it to an $Coll, but without a need to chain multiple calls to (extension) methods
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


	@tailrec override def from[A](source :IterableOnce[A]) :IRefArray[A] = source match {
		case _ if source.knownSize == 0           => empty
		case _ :View[A]                           => from(source.iterator)
		case Wrapped(array)                       => array
		case items :Iterable[A] if items.isEmpty  => empty
		case items :Iterator[A] if !items.hasNext => empty
		case items :Iterable[A]                   => items.toIRefArray
		case _                                    => source.iterator.toIRefArray
	}


	/** Wraps and unwraps `IndexedSeq` instances and other immutable collections
	  * backed by arrays of `Array[AnyRef]` type in a safe manner. $warning
	  * @define warning Arrays are represented as [[net.noresttherein.sugar.arrays.IRefArray IRefArray]] instances
	  *                 to prevent accidental modification and ensure that the user is aware that an array
	  *                 will be represented as an immutable structure. Note that only 'untagged' collections,
	  *                 that is backed by `Array[AnyRef]`, and not any of its subclasses, are recognized.
	  */
	@SerialVersionUID(Ver)
	object Wrapped {
		private[this] val wrapper :ArrayLikeSliceFactory[IndexedSeq, IRefArray] =
			PassedArrayFactory getOrElse IRefArraySlice

		def apply[A](array :IRefArray[A]) :IndexedSeq[A] = wrapper.wrap(array)

		def unapply[A](elems :IterableOnce[A]) :Opt[IRefArray[A]] = {
			val array = elems match {
				case seq :ArraySeq[_] => seq.unsafeArray
				case slice :ArrayIterableOnce[_] if slice.knownSize == slice.unsafeArray.length && slice.isImmutable =>
					slice.unsafeArray
				case seq :Vector[_] if seq.length == CheatedAccess.FlatVectorSize => CheatedAccess.array(seq)
				case _                => null
			}
			if (array != null && array.getClass == classOf[Array[AnyRef]])
				Got(array.castFrom[Array[_], IRefArray[A]])
			else
				Lack
		}

		/** Wraps and unwraps `IndexedSeq` instances and other immutable collections backed by consecutive sections
		  * of `Array[AnyRef]` arrays in a safe manner. $warning
		  */
		@SerialVersionUID(Ver)
		object Slice {
			private[this] val wrapper :ArrayLikeSliceFactory[IndexedSeq, IRefArray] =
				PassedArrayFactory getOrElse IRefArraySlice

			def apply[A](array :IRefArray[A], from :Int, until :Int) :IndexedSeq[A] =
				wrapper.slice(array, from, until)

			def unapply[A](elems :IterableOnce[A]) :Opt[(IRefArray[A], Int, Int)] = elems match {
				case seq :ArraySeq[_] if seq.unsafeArray.getClass == classOf[Array[AnyRef]] =>
					Got((seq.unsafeArray.castFrom[Array[_], IRefArray[A]], 0, seq.unsafeArray.length))

				case arr :ArrayIterableOnce[A] if arr.isImmutable =>
					val array = arr.unsafeArray.castFrom[Array[_], IRefArray[A]]
					if (array.getClass == classOf[Array[AnyRef]])
						Got((array, arr.startIndex, arr.startIndex + arr.knownSize))
					else
						Lack
				case seq :Vector[A] if seq.length <= CheatedAccess.FlatVectorSize =>
					Got((CheatedAccess.array(seq).castFrom[Array[_], IRefArray[A]], 0, seq.length))
				case _ =>
					Lack
			}
		}
	}



//	implicit def IRefArrayOrdering[A :Ordering] :Ordering[IRefArray[A]] =
//		Array.ArrayOrdering[A].castParam[IRefArray[A]]

	implicit def IRefArrayClassTag[A] :ClassTag[IRefArray[A]] = tag.castParam[IRefArray[A]]
	private[this] val tag = classTag[Array[AnyRef]]

//	@inline implicit def IRefArrayToSeq[A](array :IRefArray[A]) :IndexedSeq[A] = Wrapped(array)
	implicit def IRefArrayIsSeq[E] :IsSeq[IRefArray[E]] { type A = E; type C = IRefArray[A] } =
		IsSeqPrototype.asInstanceOf[IsSeq[IRefArray[E]] { type A = E; type C = IRefArray[A] }]

	private[this] val IsSeqPrototype =
		new ArrayLikeIsSeqTemplate[Any, Seq, IRefArray] {
			override def apply(coll :IRefArray[Any]) =
				new IArrayLikeIsSeqOps[Any, IRefArray](coll) {
					protected override def fromSpecific(coll :IterableOnce[Any]) :IRefArray[Any] = from(coll)
					protected override def newSpecificBuilder :Builder[Any, IRefArray[Any]] = newBuilder
				}
			private def readResolve = IRefArrayIsSeq
			override def toString = "IRefArrayIsSeq"
		}
//
//	implicit def IRefArrayIsSeq[E] :IsSeq[IRefArray[E]] { type A = E; type C = IRefArray[A] } =
//		isSeq


//	private[arrays] sealed trait conversions extends Any with IArrayLike.conversions {
//		//fixme: conflicts with ArrayLikeExtension
////		@inline implicit final def IRefArrayToSeq[E](self :IRefArray[E]) :IndexedSeq[E] = Wrapped(self)
//	}
//
//	private[arrays] sealed trait IRefArrayRank1Extensions
//		extends Any with RefArrayLike.extensions with IArrayLike.extensions
//	{
//		/** Adds various additional folding methods with a break condition to any `RefArray`. */
//		@inline implicit final def IRefArrayAsIterableOnceExtension[E](self :IRefArray[E]) :IterableOnceExtension[E] =
//			new IterableOnceExtension(new IRefArrayAsSeq(self))
//
//		/** Adds various methods for mapping/flatMapping collections to any `IRefArray`.
//		  * These either pass along additional state, or have a break condition. Roughly equivalent to working
//		  * with `toLazyList.scan`, but cleaner and more efficient.
//		  */
//		@inline implicit final def IRefArrayAsIterableExtension[E](self :IRefArray[E])
//				:IterableExtension[E, IRefArray, IRefArray[E]] =
//			new IterableExtension[E, IRefArray, IRefArray[E]](new IRefArrayAsSeq(self))
//
//		/** Alternative, safer implementations of [[scala.collection.SeqOps.indexOf indexOf]] for immutable reference arrays,
//		  * which do not return a negative index when the element is not found.
//		  */
//	   @inline implicit final def IRefArrayAsSeqExtension[E](self :IRefArray[E]) :SeqExtension[E, IRefArray, IRefArray[E]] =
//			new SeqExtension[E, IRefArray, IRefArray[E]](new IRefArrayAsSeq(self))
//
//		/** Operations on suffixes of a sequence and binary search methods on sorted immutable reference arrays. */
//		@inline implicit final def IRefArrayAsIndexedSeqExtension[E](self :IRefArray[E])
//				:IndexedSeqExtension[E, IRefArray, IRefArray[E]] =
//			new IndexedSeqExtension(new IRefArrayAsSeq(self))
//	}

//	private[arrays] sealed trait IRefArrayRank1Extensions extends Any with IRefArrayRank2Extensions {
//
//		@inline implicit final def IRefArrayAsArrayLikeExtension[A](array :IRefArray[A])
//				:ArrayLike.ArrayLikeExtension[IRefArray, A] =
//			new ArrayLike.ArrayLikeExtension(array.castFrom[IRefArray[A], Array[Any]])
//	}
//
	/** Mixin trait with extension methods conversion for `IRefArray` types.
	  * @define Coll `IRefArray`
	  * @define Extension `IRefArrayExtension[E]`
	  */
	private[arrays] trait extensions extends Any with RefArrayLike.extensions with IArrayLike.extensions {
//		@inline implicit final def IRefArrayExtension[A, Arr[X] <: IRefArray[X]](self :IRefArray[A])
//				:IRefArrayExtension[A, Arr] =
//			new IRefArrayExtension(self.asInstanceOf[Array[_]])
		/** Extension methods for [[net.noresttherein.sugar.arrays.IRefArray IRefArray]]`[E]`.
		  * $conversionInfo 
		  */
		implicit final def IRefArrayExtension[E] :IRefArrayExtensionConversion[E] =
			extensions.IRefArrayExtensionConversionPrototype.asInstanceOf[IRefArrayExtensionConversion[E]]
	}
	
	@SerialVersionUID(Ver)
	object extensions extends extensions {
		sealed trait IRefArrayExtensionConversion[E] extends (IRefArray[E] => IRefArrayExtension[E]) {
			@inline final def apply(v1 :IRefArray[E])(implicit dummy :DummyImplicit) :IRefArrayExtension[E] =
				new IRefArrayExtension(v1.asInstanceOf[Array[Any]])
		}
		private def newIRefArrayExtensionConversion[E] =
			new PriorityConversion.Wrapped[IRefArray[E], IRefArrayExtension[E]](
				(arr :IRefArray[E]) => new IRefArrayExtension(arr.asInstanceOf[Array[Any]])
			) with IRefArrayExtensionConversion[E]
		private val IRefArrayExtensionConversionPrototype :IRefArrayExtensionConversion[Any] =
			newIRefArrayExtensionConversion
	}

}
