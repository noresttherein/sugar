package net.noresttherein.sugar

import scala.Specializable.Everything
import scala.annotation.unchecked.uncheckedVariance

import net.noresttherein.sugar.arrays.ReverseCyclicMatrixIterator
import net.noresttherein.sugar.arrays.extensions.{IArrayExtensions, IRefArrayExtensions, RefArrayExtensions}
import net.noresttherein.sugar.collections.{ArrayLikeSliceFactory, ValIterator}
import net.noresttherein.sugar.vars.Maybe




/** Definitions of several types represented under the hood by arrays:
  *   1. [[net.noresttherein.sugar.arrays.IArray! IArray!]]`[E]` is a covariant, immutable representation of `Array[E]`.
  *   1. [[net.noresttherein.sugar.arrays.RefArray! RefArray]]`[E]` is an `Array[AnyRef]` storing only values of `E`
  *      (possibly a value type after transparent boxing).
  *   1. [[net.noresttherein.sugar.arrays.IRefArray IRefArray]]`[E]` is a combination of the above:
  *      an immutable, covariant `Array[AnyRef]` storing only values of `E` (possibly after boxing).
  *
  * Additionally, there are some supertypes of the above for added polymorphism, in particular
  * [[net.noresttherein.sugar.arrays.ArrayLike ArrayLike]]`[E]` which is a supertype also of regular `Array[E]`.
  * The arrays can be constructed and converted between one another with various factory methods
  * defined in their companion objects, and have an interface in the form of extension methods, collected
  * in package [[net.noresttherein.sugar.arrays.extensions extensions]].
  *
  * Note that the types can be often cast into one another without causing a `ClassCastException`,
  * and thus the immutability is guaranteed only under the assumption that no such casting is done by the application.
  */
/* Dependencies on collections package to drop:
 *   1. util.errorString
 *   1. ArrayIterableOnce
 *   1. ArrayLikeWrapper and ArrayLikeSliceWrapper, ArrayLikeSliceFactory
 *   1. ArrayLikeSlice and subclasses
 *   1. SugaredIterableOps
 *   1. Builders (manually specialized Builder traits) in ArrayFactory
 *   1. ViewBuffer in ArrayFactory (easy to drop, like RelayArray)
 *   1. MatrixBuffer
 *   1. RelayArrayFactory
 *   1. IndexedIterator, IndexedReverseIterator, ValIterator (ArrayIterator), Mutator
 *   1. ArrayStepper
 *   1. IterableOnceExtension, IteratorExtension, IteratorCompanionExtension, StepperCompanionExtension
 *   1. ElementIndex
 */
package object arrays extends extensions {
	private[arrays] final val Ver = 1L
	
	/** Supertype of all types in this package represented by an `Array`, as well as an `Array[E]` itself. Its extension
	  * methods are defined in [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension ArrayLikeExtension]],
	  * with the corresponding conversion available
	  * as `sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.ArrayLikeExtension ArrayLikeExtension]], and,
	  * collectively with extensions from all other packages, in `sugar.`[[net.noresttherein.sugar.extensions extensions]].
	  * @see [[net.noresttherein.sugar.arrays.ArrayLike$ ArrayLike]]
	  * @see [[net.noresttherein.sugar.arrays.IArrayLike IArrayLike]]
	  * @see [[net.noresttherein.sugar.arrays.IArray IArray]]
	  * @see [[net.noresttherein.sugar.arrays.RefArrayLike RefArrayLike]]
	  * @see [[net.noresttherein.sugar.arrays.RefArray RefArray]]
	  * @see [[net.noresttherein.sugar.arrays.IRefArray IRefArray]]
	  * @see [[net.noresttherein.sugar.arrays.MutableArray MutableArray]]
	  */
	type ArrayLike[+E] >: MutableArray[_ <: E] <: AnyRef with Serializable

	/** Common supertype of immutable [[net.noresttherein.sugar.arrays.ArrayLike ArrayLike]]
	  * subtypes: [[net.noresttherein.sugar.arrays.IArray IArray]]
	  * and [[net.noresttherein.sugar.arrays.IRefArray IRefArray]].
	  * Casting of values of this type to an `Array[E]`, or anything but `Array[_]`, is ''not'' safe,
	  * as it can be represented by an array of any supertype or subtype of `E`.
	  * @see [[net.noresttherein.sugar.arrays.IArrayLike$ IArrayLike]]
	  * @see [[net.noresttherein.sugar.arrays.ArrayLike ArrayLike]]
	  */
	type IArrayLike[+E] >: Null <: ArrayLike[E]

	/** A common supertype of the regular `Array[E]` and an erased
	  * [[net.noresttherein.sugar.arrays.RefArray! RefArray]], represented in runtime as an `Array[Any]`.
	  * Its specific mutating methods are defined in
	  * [[net.noresttherein.sugar.arrays.MutableArray.MutableArrayExtension MutableArrayExtension]], enabled by importing
	  * `sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.MutableArrayExtension MutableArrayExtension]],
	  * or, indirectly, from `sugar.extensions`. It is a subtype of [[net.noresttherein.sugar.arrays.ArrayLike ArrayLike]],
	  * and thus, it inherits also the interface from its
	  * [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension ArrayLikeExtension]], which must be separately imported
	  * from the said packages.
	  * @see [[net.noresttherein.sugar.arrays.MutableArray$ MutableArray]]
	  */ //consider: renaming to MutArrayLike or MutArray
	type MutableArray[E] >: Array[E] <: AnyRef with Serializable

	/** A supertype of both `IArray[E]` and regular `Array[E]`. These types have in common the fact
	  * that the component type is important and must be known during their creation.
	  * Moreover, their own `getClass.getComponentType` can be used as a component type for a compatible array.
	  * While it does not introduce any extra functionality in the form of extension methods,
	  * it allows to pass both `IArray[E]` and `Array[E]` as a parameter to a method, whose split into two overloaded
	  * variants would be impossible due to method signature conflict after erasure,
	  * as both these types erase to `AnyRef` in a generic context.
	  */
	type TypedArray[@specialized(Everything) +E] >: Array[E @uncheckedVariance] <: ArrayLike[E]

	/** An immutable array with elements of type `E`, represented in runtime as some `Array[_ >: E]`.
	  * Its interface is defined as extension methods in
	  * [[net.noresttherein.sugar.arrays.IArray.IArrayExtension IArrayExtension]]`[E]`,
	  * [[net.noresttherein.sugar.arrays.IArrayLike.IArrayLikeExtension IArrayLikeExtension]]`[IArray, E]`,
	  * [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension ArrayLikeExtension]]`[IArray, E]`,
	  * as well as manually specialized variants for standard value types. Conversions enabling all these methods
	  * can be imported from `sugar.arrays.`[[net.noresttherein.sugar.arrays.extensions extensions]] or
	  * `sugar.`[[net.noresttherein.sugar.extensions extensions]].
	  * They treat the array like an immutable collection, and methods from [[scala.collection.ArrayOps ArrayOps]]
	  * which would normally copy the array, like `drop(0)`, will return the array itself.
	  *
	  * Note that this type is covariant, hence an `IArray[Any]` may be in fact an array of any value type,
	  * not only reference types. In general, if `A` is a value type, it is safe to cast
	  * between `IArray[A]` and `Array[A]` in either direction (ignoring mutability concerns).
	  * However, casting for other element types (in particular `Any`) will most likely result
	  * in either a `ClassCastException` or `ArrayStoreException` being thrown at some later point.
	  * Box types, similarly to `Array`, are not abstracted over for this purpose: an `IArray[Int]`
	  * is never an `Array[Integer]` and vice versa.
	  * @see [[net.noresttherein.sugar.arrays.IArray$]]
	  * @see [[net.noresttherein.sugar.arrays.ArrayLike]]
	  * @see [[net.noresttherein.sugar.arrays.IArrayLike]]
	  */ //todo: carefully review all usage to eliminate the chance of us casting an `IArray[Any]` to `Array[Any]`
	type IArray[@specialized(Everything) +E] >: Null <: IArrayLike[E] with TypedArray[E]

	/** A common supertype of types represented in runtime by an `Array[AnyRef]`:
	  * [[net.noresttherein.sugar.arrays.RefArray! RefArray]] and [[net.noresttherein.sugar.arrays.IRefArray! IRefArray]].
	  * Due to autoboxing employed by the Scala runtime, a `RefArray` is sound also for value types, both built-in,
	  * and value classes. Its specific interface is defined
	  * in [[net.noresttherein.sugar.arrays.RefArrayLike.RefArrayLikeExtension RefArrayLikeExtension]], but,
	  * as a subtype of [[net.noresttherein.sugar.arrays.ArrayLike! ArrayLike]], and thus a part of it interface
	  * is defined in [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension ArrayLikeExtension]],
	  * both available for import under those names from
	  * `sugar.arrays.`[[net.noresttherein.sugar.arrays.extensions extensions]], or, indirectly,
	  * from `sugar.extensions`, together with all extension methods in the library.
	  */
	type RefArrayLike[+E] >: Null <: ArrayLike[E]

	/** An erased array with elements of type `E`. It is represented always as an `Array[Any]` (i.e., `Object[])`,
	  * and `RefArray`s of value types store them in their standard box classes. This approach is used widely,
	  * including by the standard `ArrayBuffer` and `Vector`. The advantage is that the API
	  * does not depend on `ClassTag[E]` being present.
	  *
	  * Its interface is defined as extension methods in
	  * [[net.noresttherein.sugar.arrays.RefArray.RefArrayExtension RefArrayExtension]]`[E]`,
	  * [[net.noresttherein.sugar.arrays.RefArrayLike.RefArrayLikeExtension RefArrayLikeExtension]]`[RefArray, E]`,
	  * [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension ArrayLikeExtension]]`[RefArray, E]` and
	  * [[net.noresttherein.sugar.arrays.MutableArray.MutableArrayExtension MutableArrayExtension]]`[E]`.
	  * Eponymously named conversions enabling those extension methods can be imported
	  * from `sugar.arrays.`[[net.noresttherein.sugar.arrays.extensions extensions]] or
	  * `sugar.`[[net.noresttherein.sugar.extensions extensions]].
	  *
	  * Despite an additional layer of extension methods, `RefArray` will be generally faster
	  * than using a generic (erased) `Array[T]`, as all access happens statically on an `Array[AnyRef]`,
	  * and there is no need for checking the array type.
	  * @note you can use almost any existing method to copy to a `RefArray` by adapting it to `Array[Any]`
	  *       with its [[net.noresttherein.sugar.arrays.RefArray$.RefArrayExtension.asAnyArray asAnyArray]] method.
	  * @see [[net.noresttherein.sugar.arrays.RefArray$ RefArray]]
	  * @see [[net.noresttherein.sugar.arrays.RefArrayLike RefArrayLike]]
	  * @see [[net.noresttherein.sugar.arrays.ArrayLike ArrayLike]]
	  * @see [[net.noresttherein.sugar.arrays.MutableArray MutableArray]]
	  */
	type RefArray[E] >: Null <: RefArrayLike[E] with MutableArray[E] //todo: rename it to BoxArray/AnyArray/ErasedArray

	/** An immutable array with elements of type `E`, represented in runtime as `Array[Any]` (that is, `Object[]`).
	  * Its interface is defined as extension methods in
	  * [[net.noresttherein.sugar.arrays.IRefArray$.IRefArrayExtension IRefArrayExtension]]`[E]`,
	  * [[net.noresttherein.sugar.arrays.IRefArray$.IRefArrayLikeExtension IRefArrayLikeExtension]]`[IRefArray, E]`,
	  * [[net.noresttherein.sugar.arrays.IArrayLike$.IArrayLikeExtension IArrayLikeExtension]]`[IRefArray, E]`,
	  * [[net.noresttherein.sugar.arrays.IArrayLike$.ArrayLikeExtension ArrayLikeExtension]]`[RefArray, E]`,
	  * which are available for importing under those names from
	  * `sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions extensions]] or
	  * `sugar.`[[net.noresttherein.sugar.extensions extensions]].
	  *
	  * Despite an additional layer of extension methods, `RefArray` will be generally faster
	  * than using a generic (erased) `Array[T]`, as all access happens statically on an `Array[AnyRef]`,
	  * and there is no need for checking the array type.
	  * @see [[net.noresttherein.sugar.arrays.IRefArray$ IRefArray]]
	  * @see [[net.noresttherein.sugar.arrays.IArrayLike IArrayLike]]
	  * @see [[net.noresttherein.sugar.arrays.RefArrayLike RefArrayLike]]
	  * @see [[net.noresttherein.sugar.arrays.ArrayLike ArrayLike]]
	  */
	type IRefArray[+E] >: Null <: RefArrayLike[E] with IArrayLike[E]


	/** A two-dimensional array. */
	type Array2[E] = Array[Array[E]]
	/** A three-dimensional array. */
	type Array3[E] = Array[Array[Array[E]]]
	/** A four dimensional array. */
	type Array4[E] = Array[Array[Array[Array[E]]]]
	/** A five dimensional array. */
	type Array5[E] = Array[Array[Array[Array[Array[E]]]]]
	/** A six dimensional array. */
	type Array6[E] = Array[Array[Array[Array[Array[Array[E]]]]]]
	/** A seven dimensional array. */
	type Array7[E] = Array[Array[Array[Array[Array[Array[Array[E]]]]]]]

	/** A two-dimensional immutable array. */
	type IArray2[E] = IArray[IArray[E]]
	/** A three-dimensional immutable array. */
	type IArray3[E] = IArray[IArray[IArray[E]]]
	/** A four dimensional immutable array. */
	type IArray4[E] = IArray[IArray[IArray[IArray[E]]]]
	/** A five dimensional immutable array. */
	type IArray5[E] = IArray[IArray[IArray[IArray[IArray[E]]]]]
	/** A six dimensional immutable array. */
	type IArray6[E] = IArray[IArray[IArray[IArray[IArray[IArray[E]]]]]]
	/** A seven dimensional immutable array. */
	type IArray7[E] = IArray[IArray[IArray[IArray[IArray[IArray[IArray[E]]]]]]]

	/** A two-dimensional box array. */
	type RefArray2[E] = RefArray[RefArray[E]]
	/** A three-dimensional box array. */
	type RefArray3[E] = RefArray[RefArray[RefArray[E]]]
	/** A four dimensional box array. */
	type RefArray4[E] = RefArray[RefArray[RefArray[RefArray[E]]]]
	/** A five dimensional box array. */
	type RefArray5[E] = RefArray[RefArray[RefArray[RefArray[RefArray[E]]]]]
	/** A six dimensional box array. */
	type RefArray6[E] = RefArray[RefArray[RefArray[RefArray[RefArray[RefArray[E]]]]]]
	/** A seven dimensional box array. */
	type RefArray7[E] = RefArray[RefArray[RefArray[RefArray[RefArray[RefArray[RefArray[E]]]]]]]

	/** A two-dimensional immutable box array. */
	type IRefArray2[E] = IRefArray[IRefArray[E]]
	/** A three-dimensional immutable box array. */
	type IRefArray3[E] = IRefArray[IRefArray[IRefArray[E]]]
	/** A four dimensional immutable box array. */
	type IRefArray4[E] = IRefArray[IRefArray[IRefArray[IRefArray[E]]]]
	/** A five dimensional immutable box array. */
	type IRefArray5[E] = IRefArray[IRefArray[IRefArray[IRefArray[IRefArray[E]]]]]
	/** A six dimensional immutable box array. */
	type IRefArray6[E] = IRefArray[IRefArray[IRefArray[IRefArray[IRefArray[IRefArray[E]]]]]]
	/** A seven dimensional immutable box array. */
	type IRefArray7[E] = IRefArray[IRefArray[IRefArray[IRefArray[IRefArray[IRefArray[IRefArray[E]]]]]]]



	/** A forwarder to `java.lang.System.arraycopy` introduced to reduce source code level dependency on the method. */
	@inline def arraycopy(src :ArrayLike[_], srcPos :Int, dst :ArrayLike[_], dstPos :Int, length :Int) :Unit =
		java.lang.System.arraycopy(src, srcPos, dst, dstPos, length)


	//Consider: if these factories should be public, and, if so, what iterator type should they return.
	// The choice is really between ValIterator, ValIterator.Buffered (ugh), BufferedIterator and Iterator.
	// So, it boils down mainly to the dilemma if we should have @specialized classes in public API.
	// Currently I have chosen ValIterator.Buffered, because we already had tests for head in ArrayIteratorSpec.
	/** A factory of iterators advancing over array slices. */
	private[sugar] val ArrayLikeIterator :ArrayLikeIteratorFactory[ArrayLike, ValIterator.Buffered] =
		ArrayLikeIteratorFactory

	private[sugar] val IArrayLikeIterator :ArrayLikeIteratorFactory[IArrayLike, ValIterator.Buffered] =
		IArrayIteratorFactory

	private[sugar] val ArrayIterator :TypedArrayIteratorFactory[Array, ValIterator.Buffered] = ArrayIteratorFactory
	private[sugar] val IArrayIterator :TypedArrayIteratorFactory[IArray, ValIterator.Buffered] = IArrayIteratorFactory

	private[sugar] val RefArrayLikeIterator :ArrayLikeIteratorFactory[RefArray, ValIterator.Buffered] =
		RefArrayLikeIteratorFactory

	private[sugar] val RefArrayIterator :ArrayLikeIteratorFactory[RefArray, ValIterator.Buffered] =
		RefArrayLikeIteratorFactory

	private[sugar] val IRefArrayIterator :ArrayLikeIteratorFactory[IRefArray, ValIterator.Buffered] = IRefArrayIteratorFactory



	private[sugar] val ReverseArrayLikeIterator :ArrayLikeIteratorFactory[ArrayLike, ValIterator.Buffered] =
		ReverseArrayIteratorFactory

	private[sugar] val ReverseIArrayLikeIterator :ArrayLikeIteratorFactory[IArrayLike, ValIterator.Buffered] =
		ReverseArrayIteratorFactory

	private[sugar] val ReverseArrayIterator  :TypedArrayIteratorFactory[Array, ValIterator.Buffered] =
		ReverseArrayIteratorFactory

	private[sugar] val ReverseIArrayIterator :TypedArrayIteratorFactory[IArray, ValIterator.Buffered] =
		ReverseArrayIteratorFactory

	private[sugar] val ReverseRefArrayLikeIterator :ArrayLikeIteratorFactory[RefArrayLike, ValIterator.Buffered] =
		ReverseRefArrayLikeIteratorFactory

	private[sugar] val ReverseRefArrayIterator :ArrayLikeIteratorFactory[RefArray, ValIterator.Buffered] =
		ReverseRefArrayLikeIteratorFactory

	private[sugar] val ReverseIRefArrayIterator :ArrayLikeIteratorFactory[IRefArray, ValIterator.Buffered] =
		ReverseRefArrayLikeIteratorFactory



	private[sugar] val CyclicArrayLikeIterator :ArrayLikeIteratorFactory[ArrayLike, ValIterator.Buffered] =
		CyclicArrayIteratorFactory

	private[sugar] val CyclicIArrayLikeIterator :ArrayLikeIteratorFactory[IArrayLike, ValIterator.Buffered] =
		CyclicArrayIteratorFactory

	private[sugar] val CyclicArrayIterator  :TypedArrayIteratorFactory[Array, ValIterator.Buffered] =
		CyclicArrayIteratorFactory

	private[sugar] val CyclicIArrayIterator :TypedArrayIteratorFactory[IArray, ValIterator.Buffered] =
		CyclicArrayIteratorFactory

	private[sugar] val CyclicRefArrayLikeIterator :ArrayLikeIteratorFactory[RefArrayLike, ValIterator.Buffered] =
		CyclicArrayLikeIterator

	private[sugar] val CyclicRefArrayIterator :ArrayLikeIteratorFactory[RefArray, ValIterator.Buffered] =
		CyclicRefArrayLikeIterator

	private[sugar] val CyclicIRefArrayIterator :ArrayLikeIteratorFactory[IRefArray, ValIterator.Buffered] =
		CyclicIArrayLikeIterator



	private[sugar] val ReverseCyclicArrayLikeIterator :ArrayLikeIteratorFactory[ArrayLike, ValIterator.Buffered] =
		ReverseCyclicArrayIteratorFactory

	private[sugar] val ReverseCyclicIArrayLikeIterator :ArrayLikeIteratorFactory[ArrayLike, ValIterator.Buffered] =
		ReverseCyclicArrayIteratorFactory

	private[sugar] val ReverseCyclicArrayIterator  :TypedArrayIteratorFactory[Array, ValIterator.Buffered] =
		ReverseCyclicArrayIteratorFactory

	private[sugar] val ReverseCyclicIArrayIterator :TypedArrayIteratorFactory[IArray, ValIterator.Buffered] =
		ReverseCyclicArrayIteratorFactory

	private[sugar] val ReverseCyclicRefArrayLikeIterator :ArrayLikeIteratorFactory[RefArrayLike, ValIterator.Buffered] =
		ReverseCyclicArrayLikeIterator

	private[sugar] val ReverseCyclicRefArrayIterator :ArrayLikeIteratorFactory[RefArray, ValIterator.Buffered] =
		ReverseCyclicArrayLikeIterator

	private[sugar] val ReverseCyclicIRefArrayIterator :ArrayLikeIteratorFactory[IRefArray, ValIterator.Buffered] =
		ReverseCyclicIArrayLikeIterator



	private[sugar] val MutableArrayMutator :ArrayLikeIteratorFactory[MutableArray, ArrayMutator] =
		ArrayMutator.asInstanceOf[ArrayLikeIteratorFactory[MutableArray, ArrayMutator]]



	private[sugar] val MatrixIterator        :GenericMatrixIteratorFactory[Array, ValIterator.Buffered] =
		MatrixIteratorFactory

	private[sugar] val ReverseMatrixIterator :GenericMatrixIteratorFactory[Array, ValIterator.Buffered] =
		ReverseMatrixIteratorFactory

	private[sugar] val CyclicMatrixIterator  :GenericMatrixIteratorFactory[Array, ValIterator.Buffered] =
		CyclicMatrixIteratorFactory

	private[sugar] val ReverseCyclicMatrixIterator :GenericMatrixIteratorFactory[Array, ValIterator.Buffered] =
		ReverseCyclicMatrixIteratorFactory



	private[arrays] final val RelayArrayFactory :Maybe[ArrayLikeSliceFactory[IArrayLike, IndexedSeq]] =
		collections.RelayArrayFactory
}
