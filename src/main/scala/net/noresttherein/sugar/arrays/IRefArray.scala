package net.noresttherein.sugar.arrays


import java.lang.System.arraycopy

import scala.Array.emptyObjectArray
import scala.annotation.tailrec
import scala.collection.{IterableFactory, View}
import scala.collection.immutable.{ArraySeq, IndexedSeqOps}
import scala.reflect.ClassTag

import net.noresttherein.sugar.casting.{castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.collections.{ArrayIterableOnce, ArrayLikeSliceFactory, IRefArraySlice}
import net.noresttherein.sugar.concurrent.Fences.releaseFence
import net.noresttherein.sugar.extensions.IterableOnceExtension
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




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
  * @define coll immutable reference array
  * @define updatedInfo
  * It allows to introduce multiple changes to an array in a manner safer than starting with a mutable `Array`
  * and casting it to an `IRefArray`, but without a need to chain multiple calls to (extension) methods
  * of `other` (which would require wasteful creation of intermediate results). Additionally,
  * using [[java.util.Arrays Arrays]]`.`[[java.util.Arrays.copyOfRange copyOfRange]] is faster than creating
  * an array filled with default values and copying contents to it afterwards.
  */
@SerialVersionUID(Ver)
case object IRefArray extends RefArrayLikeFactory[IRefArray] with IterableFactory[IRefArray] {

	@inline private[IRefArray] def expose[X](array :Array[Any]) :IRefArray[X] = {
		releaseFence()
		array.asInstanceOf[IRefArray[X]]
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
		expose(res)
	}

	/** Creates a new $Coll by modifying another `ArrayLike`. This method combines [[Array.copyOf copyOf]]`(other)`
	  * with [[net.noresttherein.sugar.arrays.IRefArray.init init]]: the latter, instead of an empty
	  * array filled with default values, receives an object array filled with copied contents of `other`.
	  *
	  * $updatedInfo
	  * @param other A prototype array with values passed to the initialization function.
	  * @param f         An initialization function, accepting a fresh universal array.
	  *                  Should not retain the reference to the argument after its completion, or immutability
	  *                  of the result will be compromised.
	  */
	def updated[E](other :ArrayLike[E])(f :Array[Any] => Unit) :IRefArray[E] = {
		val res = ArrayFactory.copyOf[Any](other)(ClassTag.Any)
		f(res)
		expose(res)
	}

	/** Creates a new $Coll by copying and modifying contents of another `ArrayLike`.
	  * This method combines `Array.`[[scala.Array.copyOf copyOf]]`(other, newLength)`
	  * with [[net.noresttherein.sugar.arrays.IRefArray.init init]]: the latter, instead of an empty
	  * array filled with default values, receives an object array with copied contents of `other`.
	  *
	  * $updatedInfo
	  * @param other     A prototype array with values passed to the initialization function.
	  * @param newLength The length of the created array. If lesser than the length of `other`,
	  *                  then the extra elements are ignored. Otherwise the array is padded with default values
	  *                  before being passed to `f`.
	  * @param f         An initialization function, accepting a fresh universal array.
	  *                  Should not retain the reference to the argument after its completion, or immutability
	  *                  of the result will be compromised.
	  */
	@throws[NegativeArraySizeException]("if newLength is negative")
	@inline def updated[E](other :ArrayLike[E], newLength :Int)(f :Array[Any] => Unit) :IRefArray[E] = {
		val res = ArrayFactory.copyOf[Any](other, newLength)(ClassTag.Any)
		f(res)
		expose(res)
	}

	/** Creates a new $Coll by introducing changes to a slice of another `ArrayLike`. This method combines
	  * [[net.noresttherein.sugar.arrays.extensions.ArrayCompanionExtension.copyOfRange copyOfRange]]`(other, from, until)`
	  * with [[net.noresttherein.sugar.arrays.IRefArray.init init]]: the latter, instead of an empty
	  * array filled with default values, receives a slice of the original.
	  *
	  * $updatedInfo
	  * @param other A prototype array whose slice is passed to the initialization function.
	  * @param from  The index in the original array to which the first element of the new array is set;
	  *              if lesser than zero or greater than `other.length`, it is clipped to the valid range.
	  * @param until The exclusive end of the copied range, clipped to the valid index range before copying.
	  * @param f         An initialization function, accepting a fresh universal array.
	  *                  Should not retain the reference to the argument after its completion, or immutability
	  *                  of the result will be compromised.
	  */
	@inline def updated[E](other :ArrayLike[E], from :Int, until :Int)(f :Array[Any] => Unit) :IRefArray[E] = {
		val res = Array.copyOfRange[Any](other, from, until)(ClassTag.Any)
		f(res)
		expose(res)
	}

	/** Creates a new $Coll by introducing changes to a slice of another `ArrayLike`, including, potentially
	  * appending additional elements. This method combines
	  * [[net.noresttherein.sugar.arrays.extensions.ArrayCompanionExtension.copyOfRange copyOfRange]]`(other, from, until, newLength)`
	  * with [[net.noresttherein.sugar.arrays.IRefArray.init init]]: the latter, instead of an empty
	  * array filled with default values, receives an object array of the specified length, with its prefix already
	  * initialized to the copy of the index range `[from, until)` of the original.
	  *
	  * $updatedInfo
	  * @param other     An array whose slice is copied to the the new array before passing it
	  *                  to the initialization function.
	  * @param from      The index in the original array to which the first element of the new array is set;
	  *                  if lesser than zero or greater than `other.length`, it is clipped to the valid range.
	  * @param until     The exclusive end of the copied range, clipped to the valid index range before copying.
	  * @param newLength The length of the created array. If lesser than the length of the copied slice, the extra
	  *                  elements are ignored. Otherwise the array is padded with default values
	  *                  before being passed to `f`.
	  * @param f         An initialization function, accepting a fresh universal array.
	  *                  Should not retain the reference to the argument after its completion, or immutability
	  *                  of the result will be compromised.
	  */
	@throws[NegativeArraySizeException]("if newLength is less than zero")
	@inline def updated[E](other :ArrayLike[E], from :Int, until :Int, newLength :Int)
	                      (f :Array[Any] => Unit) :IRefArray[E] =
	{
		val res = Array.copyOfRange[Any](other, from, until, newLength)(ClassTag.Any)
		f(res)
		expose(res)
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
	  * $updatedInfo
	  * @param other     An array whose slice is copied to the the new array before passing it
	  *                  to the initialization function.
	  * @param from      The index in the original array of the first copied element;
	  *                  if lesser than zero or greater than `other.length`, it is clipped to the valid range.
	  * @param until     The exclusive end of the copied range, clipped to the valid index range before copying.
	  * @param offset    The index in the new array where copying starts.
	  * @param newLength The length of the created array. If lesser than the length of the copied slice, the extra
	  *                  elements are ignored. Otherwise the array is padded with default values
	  *                  before being passed to `f`.
	  * @param f         An initialization function, accepting a fresh universal array.
	  *                  Should not retain the reference to the argument after its completion, or immutability
	  *                  of the result will be compromised.
	  */
	@throws[IndexOutOfBoundsException]("if offset is less than zero")
	@throws[NegativeArraySizeException]("if newLength is less than zero")
	@inline def updated[E](other :ArrayLike[E], from :Int, until :Int, offset :Int, newLength :Int)
	                      (f :Array[_ >: E] => Unit) :IRefArray[E] =
	{
		val res = Array.copyOfRange[Any](other, from, until, offset, newLength)(ClassTag.Any)
		f(res)
		expose(res)
	}


	override def from[A](source :IterableOnce[A]) :IRefArray[A] = source match {
		case _ if source.knownSize == 0           => empty
		case Wrapped(array)                       => array
		case items :Iterable[A]                   => expose(items.toArray[Any])
		case _                                    => expose(source.iterator.toArray[Any])
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
		private[this] val wrapper :ArrayLikeSliceFactory[IRefArray, IndexedSeq] =
			RelayArrayFactory getOrElse IRefArraySlice

		def apply[A](array :IRefArray[A]) :IndexedSeq[A] = wrapper.wrap(array)

		def unapply[A](elems :IterableOnce[A]) :Maybe[IRefArray[A]] = {
			val array = elems match {
				case seq :ArraySeq[_]            => seq.unsafeArray
				case slice :ArrayIterableOnce[_] =>
					val array = slice.unsafeArray
					if (slice.knownSize == array.length && slice.isImmutable) array else null
				case VectorArray(array)          => array
				case _                           => null
			}
			if (array != null && array.getClass == classOf[Array[AnyRef]])
				Yes(array.castFrom[Array[_], IRefArray[A]])
			else
				No
		}

		/** Wraps and unwraps `IndexedSeq` instances and other immutable collections backed by consecutive sections
		  * of `Array[AnyRef]` arrays in a safe manner. $warning
		  */
		@SerialVersionUID(Ver)
		object Slice {
			private[this] val wrapper :ArrayLikeSliceFactory[IRefArray, IndexedSeq] =
				RelayArrayFactory getOrElse IRefArraySlice

			def apply[A](array :IRefArray[A], from :Int, until :Int) :IndexedSeq[A] =
				wrapper.slice(array, from, until)

			def unapply[A](elems :IterableOnce[A]) :Maybe[(IRefArray[A], Int, Int)] = elems match {
				case seq :ArraySeq[_] if seq.unsafeArray.getClass == classOf[Array[AnyRef]] =>
					Yes((seq.unsafeArray.castFrom[Array[_], IRefArray[A]], 0, seq.unsafeArray.length))

				case arr :ArrayIterableOnce[A] if arr.isImmutable =>
					val array = arr.unsafeArray.castFrom[Array[_], IRefArray[A]]
					if (array.getClass == classOf[Array[AnyRef]])
						Yes((array, arr.startIndex, arr.startIndex + arr.knownSize))
					else
						No
				case VectorArray(array) => Yes((array.castFrom[Array[_], IRefArray[A]], 0, array.length))
				case _ =>
					No
			}
		}
	}



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
		@inline private def expose[U >: E](array :Array[Any]) :IRefArray[U] = {
			releaseFence()
			array.asInstanceOf[IRefArray[E]]
		}

		def padTo[A >: E](len :Int, elem :A) :IRefArray[A] = expose(
			if (len <= self.length)
				self
			else {
				val res = new Array[Any](len)
				var i = self.length
				arraycopy(self, 0, res, 0, i)
				while (i < len) {
					res(i) = elem
					i += 1
				}
				res
			}
		)
		@inline def take(n :Int) :IRefArray[E] = expose(
			if (n <= 0) Array.emptyAnyArray
			else if (n >= self.length) self
			else self.take(n)
		)
		@inline def drop(n :Int) :IRefArray[E] = expose(
			if (n <= 0) self
			else if (n >= self.length) Array.emptyAnyArray
			else self.drop(n)
		)
		@inline def takeRight(n :Int) :IRefArray[E] = expose(
			if (n <= 0) Array.emptyAnyArray
			else if (n >= self.length) self
			else self.drop(self.length - n)
		)
		@inline def dropRight(n :Int) :IRefArray[E] = expose(
			if (n <= 0) self
			else if (n >= self.length) Array.emptyAnyArray
			else self.take(self.length - n)
		)
		@inline def takeWhile(p :E => Boolean) :IRefArray[E] = take(self.segmentLength(p.asInstanceOf[Any => Boolean]))
		@inline def dropWhile(p :E => Boolean) :IRefArray[E] = drop(self.segmentLength(p.asInstanceOf[Any => Boolean]))
		@inline def slice(from :Int, until :Int) :IRefArray[E] = expose(
			if (until <= from | until < 0 || from >= self.length) Array.emptyAnyArray
			else if (from <= 0 && until >= self.length) self
			else self.slice(from, until)
		)
		@inline def splitAt(n :Int) :(IRefArray[E], IRefArray[E]) = (take(n), drop(n))

		@inline def span(p :E => Boolean) :(IRefArray[E], IRefArray[E]) =
			self.segmentLength(p.asInstanceOf[Any => Boolean]) match {
				case 0 =>
					(IRefArray.empty[E], expose(self))
				case n if n == self.length =>
					(expose(self), IRefArray.empty[E])
				case n =>
					(expose(self.take(n)), expose(self.drop(n)))
			}

		/** A view on the index range `[from, until)` of this array as a sequence.
		  * Slicing of the returned sequence will return similar views, sharing the same underlying array.
		  */
		@inline def subseq(from :Int, until :Int) :IndexedSeq[E] =
			Wrapped.Slice(expose(self), from, until)

		@inline def toSeq        :Seq[E] = toIndexedSeq
		@inline def toIndexedSeq :IndexedSeq[E] = Wrapped(expose(self))
		@inline def toOps        :IndexedSeqOps[E, IRefArray, IRefArray[E]] = new IRefArrayAsSeq(expose(self))
	}
}
