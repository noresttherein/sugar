package net.noresttherein.sugar.arrays

import scala.Array.UnapplySeqWrapper
import scala.collection.Stepper.EfficientSplit
import scala.collection.{ArrayOps, ClassTagIterableFactory, Stepper, StepperShape, View}
import scala.collection.immutable.{ArraySeq, IndexedSeqOps}
import scala.collection.mutable.Builder
import scala.reflect.{ClassTag, classTag}

import net.noresttherein.sugar.casting.{cast2TypeParamsMethods, castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.collections.{ArrayIterableOnce, ArrayLikeSliceWrapper, IArraySlice}
import net.noresttherein.sugar.concurrent.Fences.releaseFence
import net.noresttherein.sugar.exceptions.IncompatibleArgumentTypesException
import net.noresttherein.sugar.reflect.extensions.{ClassExtension, classNameMethods}
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




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
  * @define LUBComponentTypeInfo
  * If the actual classes of the argument arrays are not the same, then the returned array
  * will use the least common superclass of their element types for its element types.
  * If the element type of either array is a value type - and they are not equal -
  * then the returned array will box it to `AnyRef`, which will be used as the common upper bound.
  * In case no single minimal upper bound exists, due to element classes extending several shared, but unrelated
  * traits, an exception will be thrown.
  */ //todo: make copies of all new copyOfRange(s) methods in ArrayCompanionExtension
@SerialVersionUID(Ver)
case object IArray extends ClassTagIterableFactory[IArray] {

	//private[IArray] to enable inlining methods calling it.
	@inline private[IArray] def expose[E](array :TypedArray[E]) :IArray[E] = {
		releaseFence()
		array.asInstanceOf[IArray[E]]
	}

	//Important to inline these methods, as f may create a closure otherwise.
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
		val res = if (length == 0) ArrayFactory.empty[E] else new Array[E](length)
		res match {
			case units :Array[Unit] => units.fill(())
			case _ =>
		}
		f(res)
		expose(res)
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
		expose(res)
	}

	/** Creates a new `Array[E]` of the specified length and the same element type as the argument array,
	  * executes the given initialization function for it, and returns it as an `IArray[E]`.
	  * The argument array's size and contents are irrelevant.
	  * @see [[net.noresttherein.sugar.arrays.IArray.init]]
	  * @see [[net.noresttherein.sugar.arrays.IArray.updated(]]
	  */ //Can't use IArray argument due to covariance
	@inline def like[E](other :Array[E], length :Int)(f :Array[_ >: E] => Unit) :IArray[E] = {
		val res = ArrayFactory.ofDim(other.getClass.getComponentType.castParam[E], length)
		f(res)
		expose(res)
//		init(other.getClass.getComponentType.castParam[E], length)(f) //manually inlined for guaranteed method inlining.
	}

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
		expose(res)
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
		expose(res)
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
		expose(res)
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
		expose(res)
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
		expose(res)
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
	  */ //can't take IArray argument due to covariance
	@inline def updated[E](other :Array[E])(f :Array[_ >: E] => Unit) :IArray[E] = {
		val res = ArrayFactory.copyOf(other)
		f(res)
		expose(res)
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
		expose(res)
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
		expose(res)
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
		expose(res)
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
		expose(res)
	}


	/** Creates a copy of the given array, possibly changing its element type.
	  * @throws ArrayStoreException        If any element in array is neither an instance of `elementType`
	  *                                    nor can it be boxed/unboxed to it.
	  */
	@inline def copyAs[E :ClassTag](array :ArrayLike[_]) :IArray[E] =
		expose(ArrayFactory.copyAs[E](array, array.asInstanceOf[Array[_]].length))

	//probably is not useful to have an immutable array with uninitialized segments.
	/** Copies one array to another, truncating or padding with default values (if necessary),
	  * so that the copy has the specified length. The new array can have a different type than the original one,
	  * as long as the values are assignment-compatible. When copying between primitive and object arrays,
	  * boxing and unboxing is supported.
	  * @throws ArrayStoreException        If any element in array is neither an instance of `elementType`
	  *                                    nor can it be boxed/unboxed to it.
	  * @throws NegativeArraySizeException If `newLength` is negative.
	  */
	@inline def copyAs[E :ClassTag](array :ArrayLike[_], newLength :Int) :IArray[E] =
		expose(ArrayFactory.copyAs[E](array, newLength))

	/** Copies the contents of the specified array to an immutable array of a new element type,
	  * truncating or padding it to the specified length.
	  * @throws ArrayStoreException        If any element in array is neither an instance of `elementType`
	  *                                    nor can it be boxed/unboxed to it.
	  * @throws NegativeArraySizeException If `newLength` is negative.
	  */
	@inline def copyAs[E](array :ArrayLike[_], elementType :Class[E], newLength :Int) :IArray[E] =
		expose(ArrayFactory.copyAs(array, elementType, newLength))

	/** Copies the contents of the specified array to an immutable array of a new element type.
	  * @throws ArrayStoreException        If any element in array is neither an instance of `elementType`
	  *                                    nor can it be boxed/unboxed to it.
	  */
	@inline def copyAs[E](array :ArrayLike[_], elementType :Class[E]) :IArray[E] =
		expose(ArrayFactory.copyAs(array, elementType, array.asInstanceOf[Array[_]].length))

	/** Creates a new immutable array with the element type specified by `ClassTag[E]`
	  * and the same contents as the argument. This method is exactly equivalent to
	  * [[net.noresttherein.sugar.arrays.IArray.copyAs copyAs]]`[E](array)`, but type safe due to enforcing
	  * a compatible element type.
	  * @throws ArrayStoreException        If any element in array is neither an instance of `elementType`
	  *                                    nor can it be boxed/unboxed to it.
	  */
	@inline def copyOf[E :ClassTag](array :ArrayLike[E]) :IArray[E] =
		expose(ArrayFactory.copyAs[E](array, array.asInstanceOf[Array[_]].length))

	/** Copies one array to another, truncating or padding with default values (if necessary), so that the copy has
	  * the specified length. The returned array will be of the element type specified by the `ClassTag[E]`.
	  * Equivalent to `Array.`[[Array.copyAs copyAs]]`(array, newLength)`, but accepts any `ArrayLike` and returns
	  * the result as an $Coll. This method is exactly equivalent to
	  * [[net.noresttherein.sugar.arrays.IArray.copyAs copyAs]]`[E](array, newLength)`, but type safe due to enforcing
	  * a compatible element type.
	  * @throws NegativeArraySizeException If `newLength` is negative.
	  */
	@inline def copyOf[E :ClassTag](array :ArrayLike[E], newLength :Int) :IArray[E] =
		expose(ArrayFactory.copyAs[E](array, newLength))

	/** Creates an immutable copy of the argument, with the same element type. This method is exactly equivalent to
	  * [[net.noresttherein.sugar.arrays.IArray.copyAs copyAs]]`[E](array, newLength)`, but never throws an exception.
	  */
	def copyOf[E](array :TypedArray[E]) :IArray[E] =
		expose(ArrayFactory.copyOf(array.asInstanceOf[Array[E]]))

	/** Copies one array to another, truncating or padding with default values (if necessary) so the copy has
	  * the specified length. The returned array will be of the same type as the original.
	  * Equivalent to `Array.`[[Array.copyOf copyOf]]`(array, newLength)`, but accepts a `TypedArray` and returns
	  * the result as an $Coll.
	  * @throws NegativeArraySizeException If `newLength` is negative.
	  */
	@inline def copyOf[E](array :TypedArray[E], newLength :Int) :IArray[E] =
		expose(ArrayFactory.copyOf(array.asInstanceOf[Array[E]], newLength))

	//fixme: outdated docs, but maybe better semantics.
	/** Copies a fragment of an array to a new array. This works similarly
	  * to `array.`[[scala.collection.ArrayOps.slice slice]]`(from, until)`, with a couple of exceptions:
	  *   1. the argument may be any `ArrayLike[E]`, and the result is an $Coll`[E]`,
	  *   1. specifying `until > array.length` pads the returned array with nulls/zero values
	  *      until the length of `until - from` is reached.
	  */
	@inline def copyOfRange[E :ClassTag](array :ArrayLike[E], from :Int, until :Int) :IArray[E] =
		expose(Array.copyOfRange(array, from, until))

	/** Returns `array.slice(from, until)` as an `IArray[E]`. */
	@inline def copyOfRange[E](array :TypedArray[E], from :Int, until :Int) :IArray[E] =
		expose(array.asInstanceOf[Array[E]].slice(from, until))

	/** Copies slices from two array into a new array. Providing `until < from` has the same effect as `until == from`,
	  * that is copying nothing. However, `untilX > arrayX.length` is treated as if the source array
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
	  *         as an `IArray[E]`, with the copied slices.
	  */
	@inline def copyOfRanges[E :ClassTag](array1 :ArrayLike[E], from1 :Int, until1 :Int,
	                                      array2 :ArrayLike[E], from2 :Int, until2 :Int) :IArray[E] =
		expose(Array.copyOfRanges(array1, from1, until1, array2, from2, until2))

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
	  * @return An `IArray[E]` of length `until1 - from1 + until2 - from2` (for `from1 <= until1 && from2 <= until2`),
	  *         with the copied slices. The element type of the `IArray` will equal the element type of `array1`.
	  */
	@throws[IncompatibleArgumentTypesException]("if there is no least upper bound of the arrays' component types.")
	@inline def copyOfRanges[E](array1 :TypedArray[E], from1 :Int, until1 :Int,
	                            array2 :TypedArray[E], from2 :Int, until2 :Int) :IArray[E] =
		expose(ArrayLike.copyOfRanges(array1, from1, until1, array2, from2, until2))

	/** Copies slices from three array into a new array. Providing `until < from` has the same effect as `until == from`,
	  * that is copying nothing. However, `untilX > arrayX.length` is treated as if the source array
	  * where of length `untilX`, and contained zeros/nulls past its actual length.
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
	  *         as an `IArray[E]`, with the copied slices.
	  */
	@inline def copyOfRanges[E :ClassTag](array1 :ArrayLike[E], from1 :Int, until1 :Int,
	                                      array2 :ArrayLike[E], from2 :Int, until2 :Int,
	                                      array3 :ArrayLike[E], from3 :Int, until3 :Int) :IArray[E] =
		expose(Array.copyOfRanges(array1, from1, until1, array2, from2, until2, array3, from3, until3))

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
	  * @return An `IArray[E]` of length `until1 - from1 + until2 - from2 + until3 - from3` (for `untilN >= fromN`),
	  *         with the copied slices. The element type of the array will be equal to the element type of `array1`.
	  */
	@throws[IncompatibleArgumentTypesException]("if there is no least upper bound of the arrays' component types.")
	@inline def copyOfRanges[E](array1 :TypedArray[E], from1 :Int, until1 :Int,
	                            array2 :TypedArray[E], from2 :Int, until2 :Int,
	                            array3 :TypedArray[E], from3 :Int, until3 :Int) :IArray[E] =
		expose(ArrayLike.copyOfRanges(array1, from1, until1, array2, from2, until2, array3, from3, until3))


	/** Creates an `IArray` with the same element type and contents as the argument array.
	  * @return `Array.copyOf(array)` as an `IArray[E]`.
	  */
	@inline def from[E](array :Array[E]) :IArray[E] = expose(ArrayFactory.copyOf(array, array.length))

	@inline def from[E :ClassTag](array :ArrayLike[E]) :IArray[E] =
		expose(ArrayFactory.copyOf(array, array.asInstanceOf[Array[_]].length))

	override def from[E :ClassTag](it :IterableOnce[E]) :IArray[E] = it match {
		case _ if it.knownSize == 0              => empty[E]
		case Wrapped(array)                      => array
		case elems :Iterable[E]                  => expose(elems.toArray[E])
		case _                                   => expose(it.iterator.toArray[E])
	}

	@inline override def apply[E :ClassTag](elems :E*) :IArray[E] = expose(Array(elems :_*))

	@inline def apply(first :Byte, rest :Byte*)       :IArray[Byte]    = expose(Array(first, rest :_*))
	@inline def apply(first :Short, rest :Short*)     :IArray[Short]   = expose(Array(first, rest :_*))
	@inline def apply(first :Char, rest :Char*)       :IArray[Char]    = expose(Array(first, rest :_*))
	@inline def apply(first :Int, rest :Int*)         :IArray[Int]     = expose(Array(first, rest :_*))
	@inline def apply(first :Long, rest :Long*)       :IArray[Long]    = expose(Array(first, rest :_*))
	@inline def apply(first :Float, rest :Float*)     :IArray[Float]   = expose(Array(first, rest :_*))
	@inline def apply(first :Double, rest :Double*)   :IArray[Double]  = expose(Array(first, rest :_*))
	@inline def apply(first :Boolean, rest :Boolean*) :IArray[Boolean] = expose(Array(first, rest :_*))

	val emptyBooleanIArray :IArray[Boolean] = expose(Array.emptyBooleanArray)
	val emptyByteIArray    :IArray[Byte]    = expose(Array.emptyByteArray)
	val emptyCharIArray    :IArray[Char]    = expose(Array.emptyCharArray)
	val emptyDoubleIArray  :IArray[Double]  = expose(Array.emptyDoubleArray)
	val emptyFloatIArray   :IArray[Float]   = expose(Array.emptyFloatArray)
	val emptyIntIArray     :IArray[Int]     = expose(Array.emptyIntArray)
	val emptyLongIArray    :IArray[Long]    = expose(Array.emptyLongArray)
	val emptyShortIArray   :IArray[Short]   = expose(Array.emptyShortArray)
	val emptyObjectIArray  :IArray[AnyRef]  = expose(Array.emptyObjectArray)
	val emptyUnitIArray    :IArray[Unit]    = expose(Array.emptyUnitArray)
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
		else expose(Array.of(elementType, 0))
	).castParam[E]

	override def newBuilder[E :ClassTag] :Builder[E, IArray[E]] =
		ArrayFactory.newBuilder[E].castParam2[IArray[E]]

	/** Builds an `IArray` of the specified element type. */
	def newBuilder[E](elementType :Class[E]) :Builder[E, IArray[E]] =
		ArrayFactory.newBuilder(elementType).castParam2[IArray[E]]


	/** A single element `IArray[E]`. The array's component type will be the class of `elem`.
	  * Note that the actual class of `elem` may be a subtype of `E` - the type as seen at point of ths call,
	  * and hence the underlying array is of type `Array[_<:E]`, rather than `Array[E]`, and attempts
	  * to update the element will fail with `ArrayStoreException`. However, `IArray` is both immutable and covariant,
	  * hence `IArray[_<:E] <: Array[E]`, and these issues do not apply, if used as intended.
	  *
	  * This method does not perform auto unboxing: passing a value `E <: AnyVal` will result in its autoboxing
	  * before invoking a generic method, and thus the returned array will be of the box class,
	  * rather than the value type.
	  */
	@inline def specific[E](elem :E) :IArray[E] = expose {
		val res = ArrayFactory.ofDim[E](elem.getClass.asInstanceOf[Class[E]], 1)
		res(0) = elem
		res
	}

	/** A single element `IArray[E]`. */
	@inline def one[E :ClassTag](elem :E) :IArray[E] = expose(Array.one(elem))

	/** A single element `IArray[Byte]`. */
	@inline def one(elem :Byte) :IArray[Byte] = expose(Array.one(elem))

	/** A single element `IArray[Short]`. */
	@inline def one(elem :Short) :IArray[Short] = expose(Array.one(elem))

	/** A single element `IArray[Char]`. */
	@inline def one(elem :Char) :IArray[Char] = expose(Array.one(elem))

	/** A single element `IArray[Int]`. */
	@inline def one(elem :Int) :IArray[Int] = expose(Array.one(elem))

	/** A single element `IArray[Long]`. */
	@inline def one(elem :Long) :IArray[Long] = expose(Array.one(elem))

	/** A single element `IArray[Float]`. */
	@inline def one(elem :Float) :IArray[Float] = expose(Array.one(elem))

	/** A single element `IArray[Double]`. */
	@inline def one(elem :Double) :IArray[Double] = expose(Array.one(elem))

	/** A single element `IArray[Boolean]`. */
	@inline def one(elem :Boolean) :IArray[Boolean] = expose(Array.one(elem))


	/** A two element `IArray[E]`. The array's component type will be a superclass of both `first` and `second`.
	  * If the values are of the same class, or one is subclass of the other, then the more generic class is chosen.
	  * Otherwise, it is the least upper bound of the two classes. In other words, if there exists a  `A` such that
	  *   1. `first.getClass <:< classOf[A]` and `second.getClass <:< classOf[A]`, and
	  *   1. for all other classes `B`, if `first.getClass <:< classOf[B]` and `second.getClass <:< classOf[B]`,
	  *      then `A <: B`.
	  *
	  * If the according minimal element in the subtyping order does not exist,
	  * then an `IllegalArgumentException` is thrown. As the runtime classes of both elements may be subclasses of `E`
	  * - the type as seen at point of ths call, the underlying array will be of type `Array[_<:E]`,
	  * rather than `Array[E]`. Attempts to update the values at a later point may fail with an `ArrayStoreException`.
	  * However, because `IArray` is both immutable and covariant, and `IArray[_<:E] <: Array[E]`,
	  * these issues do not apply, as long as it is used as intended.
	  *
	  * This method does not perform auto unboxing: passing values `E <: AnyVal` will result in their autoboxing
	  * before invoking a generic method, and thus the returned array will be of the box class,
	  * rather than the value type. In other words, this method will never return an `Array[Int]`,
	  * but rather `Array[Integer]`.
	  *
	  * @note the search for a minimal superclass is a much longer process than simply creating a two element array,
	  *       which means this method will be inefficient unless the elements are of the same class,
	  *       or one is a superclass of another. Consider using [[net.noresttherein.sugar.arrays.IArray.two two]]
	  *       with an implicit `ClassTag[E]` to explicitly provide the component class, if possible.
	  */
	def specific[E](first :E, second :E) :IArray[E] = expose {
		first.getClass commonSuperclass second.getClass match {
			case Yes(cls :Class[E @unchecked]) =>
				val res = ArrayFactory.ofDim(cls, 2)
				res(0) = first
				res(1) = second
				res
			case _ =>
				throw IncompatibleArgumentTypesException(
					"Cannot create an Array of " + first.className + " and " + second.className +
						" because the classes do not have a unique least superclass."
				)
		}
	}

	/** A two element IArray of element type defined by the implicit `ClassTag`. */
	@inline final def two[E :ClassTag](first :E, second :E) :IArray[E] =
		expose(Array.two(first, second))

	/** An `IArray[Byte]` of two elements. */
	@inline final def two(first :Byte, second :Byte) :IArray[Byte] =
		expose(Array.two(first, second))

	/** An `IArray[Short]` of two elements. */
	@inline final def two(first :Short, second :Short) :IArray[Short] =
		expose(Array.two(first, second))

	/** An `IArray[Char]` of two elements. */
	@inline final def two(first :Char, second :Char) :IArray[Char] =
		expose(Array.two(first, second))

	/** An `IArray[Int]` of two elements. */
	@inline final def two(first :Int, second :Int) :IArray[Int] =
		expose(Array.two(first, second))

	/** An `IArray[Long]` of two elements. */
	@inline final def two(first :Long, second :Long) :IArray[Long] =
		expose(Array.two(first, second))

	/** An `IArray[Float]` of two elements. */
	@inline final def two(first :Float, second :Float) :IArray[Float] =
		expose(Array.two(first, second))

	/** An `IArray[Double]` of two elements. */
	@inline final def two(first :Double, second :Double) :IArray[Double] =
		expose(Array.two(first, second))

	/** An `IArray[Boolean]` of two elements. */
	@inline final def two(first :Boolean, second :Boolean) :IArray[Boolean] =
		expose(Array.two(first, second))



	/** An array filled with `n` copies of `elem`. */
	@inline final def const[X :ClassTag](n :Int)(elem :X) :IArray[X] = expose(Array.const(n)(elem))

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
		expose(Array.generate(start)(next))

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
		expose(Array.expand(start)(next))

	/** Similar to [[IArray.iterate Array.iterate]],
	  * but the iterating function accepts the positional index of the next element as an additional argument.
	  * @param start The first element of the created array.
	  * @param len   The size of the created array.
	  * @param f     A function generating subsequent elements following start.
	  *              The second element of the array will be `f(start, 1)`, the third `f(f(start, 1), 2)`, and so on.
	  */
	@inline def iterateWithIndex[X :ClassTag](start :X, len :Int)(f :(X, Int) => X) :IArray[X] =
		expose(Array.iterateWithIndex(start, len)(f))

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
		def apply[A](array :IArray[A]) :IndexedSeq[A] = Slice(array, 0, array.length)

		def unapply[E :ClassTag](elems :IterableOnce[E]) :Maybe[IArray[E]] = {
			val array = elems match {
				case VectorArray(array) => Yes(array)
				case slice :ArrayIterableOnce[_] if slice.isImmutable && slice.knownSize == slice.unsafeArray.length =>
					Yes(slice.unsafeArray)
				case seq :ArraySeq[_]   => Yes(seq.unsafeArray)
				case _                  => No
			}
			val tag = classTag[E]
			if (array.isDefined && (tag == ClassTag.Any || array.get.getClass.getComponentType <:< tag.runtimeClass))
				array.castFrom[Maybe[Array[_]], Maybe[IArray[E]]]
			else
				No
		}

		/** Factory of views on slices of immutable arrays as indexed sequences,
		  * and unwraps known immutable collections backed by array slices.
		  */
		@SerialVersionUID(Ver)
		object Slice {
			private[this] val wrapper :ArrayLikeSliceWrapper[IArray, IndexedSeq] =
				RelayArrayFactory getOrElse IArraySlice

			def apply[E](array :IArray[E], from :Int, until :Int) :IndexedSeq[E] = wrapper.slice(array, from, until)

			def unapply[E :ClassTag](elems :IterableOnce[E]) :Maybe[(IArray[E], Int, Int)] = {
				val tag = classTag[E]
				val expectedClass = tag.runtimeClass
				elems match {
					case VectorArray(array) if tag == ClassTag.Any =>
						Yes((array.castFrom[Array[_], IArray[E]], 0, elems.knownSize))
					case seq :ArraySeq[_]
						if tag == ClassTag.Any || seq.unsafeArray.getClass.getComponentType <:< expectedClass
					=>
						Yes((seq.unsafeArray.castFrom[Array[_], IArray[E]], 0, seq.unsafeArray.length))

					case slice :ArrayIterableOnce[E] if elems.knownSize >= 0 && slice.isImmutable =>
						val array = slice.unsafeArray.castFrom[Array[_], IArray[E]]
						if (tag == ClassTag.Any || array.getClass.getComponentType <:< expectedClass)
							Yes((array, slice.startIndex, slice.startIndex + slice.knownSize))
						else
							No
					case _ =>
						No
				}
			}
		}
	}



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
	  * @tparam U the component type of the elements in the underlying array.
	  */
	class IArrayExtension[E] private[arrays](private[IArray] val self :Array[E])
		extends AnyVal
	{
		@inline private def expose[X](array :Array[X]) :IArray[X] = {
			releaseFence()
			array.asInstanceOf[IArray[X]]
		}

		/** Returns `getClass.`[[java.lang.Class.getComponentType getComponentType]]. */
		@inline def getComponentType :Class[E] = self.getClass.getComponentType.castParam[E]
		//clashes with conversion to ArraySeq in predef

		/** Returns `getClass.`[[java.lang.Class.getComponentType getComponentType]] as a `ClassTag`. */
		@inline def getComponentClassTag :ClassTag[E] =
			if (self.getClass.getComponentType eq classOf[Any]) ClassTag.Any.castParam[E]
			else ClassTag[E](self.getClass.getComponentType)

		@inline def collect[A :ClassTag](pf :PartialFunction[E, A]) :IArray[A] =
			expose(new ArrayOps(self).collect(pf))

		@inline def partitionMap[E1: ClassTag, E2: ClassTag](f: E => Either[E1, E2]) :(IArray[E1], IArray[E2]) =
			new ArrayOps(self).partitionMap(f).castFrom[(Array[E1], Array[E2]), (IArray[E1], IArray[E2])]

		@inline def withFilter(p :E => Boolean) :WithFilter[E] = new WithFilter(p, self)

		@inline def map[A :ClassTag](f :E => A) :IArray[A] = expose(new ArrayOps(self).map(f))
		@inline def flatMap[A :ClassTag](f :E => IterableOnce[A]) :IArray[A] =
			expose(new ArrayOps(self).flatMap(f))

		@inline def flatMap[As, A](f :E => As)(implicit asIterable :As => Iterable[A], m :ClassTag[A]) :IArray[A] =
			expose(new ArrayOps(self).flatMap(f))

		@inline def flatten[A :ClassTag](implicit asIterable :E => IterableOnce[A]) :IArray[A] =
			expose(new ArrayOps(self).flatten)

		@inline def groupMap[K, A :ClassTag](key :E => K)(f :E => A) :Map[K, IArray[A]] =
			new ArrayOps(self).groupMap(key)(f).castParam2[IArray[A]]

		@inline def unzip[E1, E2](implicit asPair :E => (E1, E2), ct1 :ClassTag[E1], ct2 :ClassTag[E2])
				:(IArray[E1], IArray[E2]) =
			new ArrayOps(self).unzip.castFrom[(Array[E1], Array[E2]), (IArray[E1], IArray[E2])]

		@inline def unzip3[E1, E2, E3]
			              (implicit asTriple :E => (E1, E2, E3), ct1 :ClassTag[E1], ct2 :ClassTag[E2], ct3 :ClassTag[E3])
		        :(IArray[E1], IArray[E2], IArray[E3]) =
			new ArrayOps(self).unzip3.castFrom[(Array[E1], Array[E2], Array[E3]), (IArray[E1], IArray[E2], IArray[E3])]


		@inline def updated[U >: E :ClassTag](index :Int, x :U) :IArray[U] =
			expose(ArrayExtension(self).updated(index, x))

		//We could reuse in this method this array if patch is out of range, but that would become inconsistent
		// with the class tag, and might cause problems for what's a very fringe case.
		/** An 'exploded' variant of `updatedAll`: a copy of this array with elements starting at `index` substituted
		  * by `first`, `second`, and contents of `rest`. The index must lie in range `[0..length - rest.size - 2)`.
		  */
		@inline def updatedAll[U >: E :ClassTag](index :Int, first :U, second :U, rest :U*) :IArray[U] =
			expose(ArrayExtension(self).updatedAll(index, first, second, rest :_*))

		/** Updates consecutive elements of this array, starting with the specified index,
		  * with elements from the collection. Equivalent to
		  * [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.patch patch]]`(index, elems, elems.size)`,
		  * but more efficient due to a single array allocation.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.size > this.length")
		@inline def updatedAll[U >: E :ClassTag](index :Int, elems :IterableOnce[U]) :IArray[U] =
			expose(ArrayExtension(self).updatedAll(index, elems))

		/** Updates consecutive elements of this array, starting with the specified index,
		  * with elements from the collection. Equivalent to
		  * [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.patch patch]]`(index, elems, elems.length)`,
		  * but more efficient due to a single array allocation.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.length > this.length")
		@inline def updatedAll[U >: E :ClassTag](index :Int, elems :ArrayLike[U]) :IArray[U] =
			expose(ArrayExtension(self).updatedAll(index, elems))

		/** Updates consecutive elements of this array, starting with the specified index,
		  * with elements from the collection. Equivalent to
		  * [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.patch patch]]`(index, elems, elems.length)`,
		  * but more efficient due to a single array allocation.
		  */ //we can't use IArray because we are covariant, so both can be of unrelated types
		@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.length > this.length")
		@inline def updatedAll[U >: E](index :Int, elems :Array[U]) :IArray[U] =
			expose(ArrayExtension(self).updatedAll(index, elems))


		/** An 'exploded' variant of `updatedAll`: a copy of this array with elements starting at `index` substituted
		  * by `first`, `second`, and contents of `rest`.
		  */
		@inline def overwritten[U >: E :ClassTag](index :Int, first :U, second :U, rest :U*) :IArray[U] =
			expose(ArrayExtension(self).overwritten(index, first, second, rest :_*))

		/** A copy o this array, with elements starting at `index` overwritten by subsequent elements
		  * of the argument collection.
		  * This is the same as [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.updatedAll updatedAll]],
		  * except it behaves differently if `index` is out of range: instead of throwing an exception,
		  * it treats this sequence as a view on a slice some infinite sequence, and instead 'updates' that sequence.
		  * Indices out of range are silently ignored, and only the legal range is actually updated.
		  * @example
		  * {{{
		  *     val array = IArray.iterate(0, 5)(0, _ + 1)
		  *     val array.overwritten(-2, Seq(-4, -3, -2, -1)) //IArray(-2, -1, 2, 3, 4)
		  * }}}
		  */
		@inline def overwritten[U >: E :ClassTag](index :Int, elems :IterableOnce[U]) :IArray[U] =
			expose(ArrayExtension(self).overwritten(index, elems))

		/** A copy o this array, with elements starting at `index` overwritten by subsequent elements
		  * of the argument array.
		  * This is the same as [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.updatedAll updatedAll]],
		  * except it behaves differently if `index` is out of range: instead of throwing an exception,
		  * it treats this sequence as a view on a slice some infinite sequence, and instead 'updates' that sequence.
		  * Indices out of range are silently ignored, and only the legal range is actually updated.
		  * @example
		  * {{{
		  *     val array = IArray.iterate(0, 5)(0, _ + 1)
		  *     val array.overwritten(-2, Array(-4, -3, -2, -1)) //IArray(-2, -1, 2, 3, 4)
		  * }}}
		  */
		@inline def overwritten[U >: E :ClassTag](index :Int, elems :ArrayLike[U]) :IArray[U] =
			expose(ArrayExtension(self).overwritten(index, elems))

		/** A copy o this array, with elements starting at `index` overwritten by subsequent elements
		  * of the argument array.
		  * This is the same as [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.updatedAll updatedAll]],
		  * except it behaves differently if `index` is out of range: instead of throwing an exception,
		  * it treats this sequence as a view on a slice some infinite sequence, and instead 'updates' that sequence.
		  * Indices out of range are silently ignored, and only the legal range is actually updated.
		  * @example
		  * {{{
		  *     val array = IArray.iterate(0, 5)(0, _ + 1)
		  *     val array.overwritten(-2, Array(-4, -3, -2, -1)) //IArray(-2, -1, 2, 3, 4)
		  * }}}
		  */
		@inline def overwritten[U >: E](index :Int, elems :Array[U]) :IArray[U] =
			expose(ArrayExtension(self).overwritten(index, elems))


		/** A copy of this array, with the element inserted at a given position in this array,
		  * and all elements at positions equal or greater than `index` by one element further.
		  * This is equivalent to [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.patch patch]]
		  * with a singleton collection and `replaced` equal to zero, but the index must be in the valid range
		  * for this array.
		  * @return `take(index) :+ elem :++ drop(index)`, but in a more efficient manner.
		  * @throws IndexOutOfBoundsException if `index` < 0 or `index > length`.
		  */
		@inline def inserted[U >: E :ClassTag](index :Int, elem :U) :IArray[U] =
			expose(ArrayExtension(self).inserted(index, elem))

		/** An 'exploded' variant of `updatedAll`: a copy of this array with elements starting at `index` substituted
		  * by `first`, `second`, and contents of `rest`. The index must lie in range `[0..length)`.
		  */
		@inline def insertedAll[U >: E :ClassTag](index :Int, first :U, second :U, rest :U*) :IArray[U] =
			expose(ArrayExtension(self).insertedAll(index, first, second, rest :_*))

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * @return [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.patch patch]]`(index, elems, 0)`.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		@inline def insertedAll[U >: E :ClassTag](index :Int, elems :IterableOnce[U]) :IArray[U] =
			expose(ArrayExtension(self).insertedAll(index, elems))

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * @return [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.patch patch]]`(index, elems, 0)`.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		@inline def insertedAll[U >: E :ClassTag](index :Int, elems :ArrayLike[U]) :IArray[U] =
			expose(ArrayExtension(self).insertedAll(index, elems))

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * @return [[net.noresttherein.sugar.arrays.IArray.IArrayExtension.patch patch]]`(index, elems, 0)`.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		@inline def insertedAll[U >: E](index :Int, elems :Array[U]) :IArray[U] =
			expose(ArrayExtension(self).insertedAll(index, elems))


		@inline def :+[U >: E :ClassTag](x :U) :IArray[U] = appended(x)
		@inline def +:[U >: E :ClassTag](x :U) :IArray[U] = prepended(x)

		@inline def appended[U >: E :ClassTag](x :U) :IArray[U] =
			expose(ArrayExtension(self).appended(x))

		@inline def appendedAll[U >: E :ClassTag](first :U, second :U, rest :U*) :IArray[U] =
			expose(ArrayExtension(self).appendedAll(first, second, rest :_*))

		@inline def prepended[U >: E :ClassTag](x :U) :IArray[U] =
			expose(ArrayExtension(self).prepended(x))

		@inline def prependedAll[U >: E :ClassTag](first :U, second :U, rest :U*) :IArray[U] =
			expose(ArrayExtension(self).prependedAll(first, second, rest :_*))

		@inline def ++[U >: E :ClassTag](suffix :IterableOnce[U]) :IArray[U] = concat(suffix)
		@inline def ++[U >: E :ClassTag](suffix :ArrayLike[U]) :IArray[U] = concat(suffix)
		@inline def ++[U >: E](suffix :Array[U]) :IArray[U] = concat(suffix)

		@inline def concat[U >: E :ClassTag](suffix :IterableOnce[U]) :IArray[U] =
			expose(ArrayExtension(self).appendedAll(suffix))

		@inline def concat[U >: E :ClassTag](suffix :ArrayLike[U]) :IArray[U] =
			expose(ArrayExtension(self).appendedAll(suffix))

		@inline def concat[U >: E](suffix :Array[U]) :IArray[U] = expose(ArrayExtension(self).concat(suffix))

		@inline def :++[U >: E :ClassTag](suffix :IterableOnce[U]) :IArray[U] = appendedAll(suffix)
		@inline def :++[U >: E :ClassTag](suffix :ArrayLike[U]) :IArray[U] = appendedAll(suffix)
		@inline def :++[U >: E](suffix :Array[U]) :IArray[U] = appendedAll(suffix)

		@inline def appendedAll[U >: E :ClassTag](suffix :IterableOnce[U]) :IArray[U] =
			expose(ArrayExtension(self).appendedAll(suffix))

		@inline def appendedAll[U >: E :ClassTag](suffix :ArrayLike[U]) :IArray[U] =
			expose(ArrayExtension(self).appendedAll(suffix))

		@inline def appendedAll[U >: E](suffix :Array[U]) :IArray[U] =
			expose(ArrayExtension(self).appendedAll(suffix))


		@inline def ++:[U >: E :ClassTag](prefix :IterableOnce[U]) :IArray[U] = prependedAll(prefix)
		@inline def ++:[U >: E :ClassTag](prefix :ArrayLike[U]) :IArray[U] = prependedAll(prefix)
		@inline def ++:[U >: E](prefix :Array[U]) :IArray[U] = prependedAll(prefix)

		@inline def prependedAll[U >: E :ClassTag](prefix :IterableOnce[U]) :IArray[U] =
			expose(ArrayExtension(self).prependedAll(prefix))

		@inline def prependedAll[U >: E :ClassTag](prefix :ArrayLike[U]) :IArray[U] =
			expose(ArrayExtension(self).prependedAll(prefix))

		@inline def prependedAll[U >: E](suffix :Array[U]) :IArray[U] =
			expose(ArrayExtension(self).prependedAll(suffix))

		@inline def patch[U >: E :ClassTag](from :Int, elems :IterableOnce[U], replaced :Int) :IArray[U] =
			expose(ArrayExtension(self).patch(from, elems, replaced))

		@inline def patch[U >: E :ClassTag](from :Int, elems :ArrayLike[U], replaced :Int) :IArray[U] =
			expose(ArrayExtension(self).patch(from, elems, replaced))

		@inline def patch[U >: E](from :Int, elems :Array[U], replaced :Int) :IArray[U] =
			expose(ArrayExtension(self).patch(from, elems, replaced))

		/** A view on the index range `[from, until)` of this array as a sequence.
		  * Slicing of the returned sequence will return similar views, sharing the same underlying array.
		  */
		@inline def subseq(from :Int, until :Int) :IndexedSeq[E] =
			Wrapped.Slice(expose(self), from, until)

		def toOps :IndexedSeqOps[E, IRefArray, IArray[E]] = new IArrayAsSeq[E](expose(self))

		@inline def toSeq :Seq[E] = Wrapped(expose(self))
		@inline def toIndexedSeq :IndexedSeq[E] = Wrapped(expose(self))
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
		def withFilter(q :E => Boolean) :WithFilter[E] = new WithFilter[E](a => p(a) && q(a), xs)
	}
}
