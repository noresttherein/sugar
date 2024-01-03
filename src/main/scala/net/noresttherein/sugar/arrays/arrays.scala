package net.noresttherein.sugar




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
//Consider: extending extensions. Anyone importing one of these types will want also the extension, and,
// if we do not have any public classes, a wildcard import will cause no namespace pollution.
// The problem starts if someone imports both arrays._ (for the types), and sugar.extensions._
// (for the whole syntax sugar) and we end up with conflicting implicit values.
package object arrays {
	final val Ver = 1L

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
	type ArrayLike[+E] >: MutableArray[_ <: E] <: AnyRef
//
//	/** A supertype of both `Array[_ <: E]` and `IArray[E]`, used when the array is guaranteed to be a subclass
//	  * of Java `E[]` (and thus can safely be cast to `Array[E]`). This stands in contrast to
//	  * [[net.noresttherein.sugar.arrays.RefArrayLike RefArrayLike]] subtypes
//	  * of [[net.noresttherein.sugar.arrays.ArrayLike ArrayLike]].
//	  */
//	type TypedArray[+E] >: Array[_ <: E] <: ArrayLike[E]

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
	  * and thus, it inherits also the inteface from its
	  * [[net.noresttherein.sugar.arrays.ArrayLikeExtension ArrayLikeExtension]], which must be separately imported
	  * from the said packages.
	  * @see [[net.noresttherein.sugar.arrays.MutableArray$ MutableArray]]
	  */ //consider: renaming to MutArrayLike or MutArray
	type MutableArray[E] >: Array[E] <: AnyRef

	/** An immutable array with elements of type `E`, represented in runtime as some `Array[_ >: E]`.
	  * Its interface is defined as extension methods in
	  * [[net.noresttherein.sugar.arrays.IArray.IArrayExtension IArrayExtension]]`[E]`,
	  * [[net.noresttherein.sugar.arrays.IArrayLike.IArrayLikeExtension IArrayLikeExtension]]`[IArray, E]`,
	  * [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension ArrayLikeExtension]]`[IArray, E]`,
	  * as well as manually specialized variants for standard value types. Conversions enabling all these methods
	  * can be imported from `sugar.arrays.`[[net.noresttherein.sugar.arrays.extensions extensions]] or
	  * `sugar.`[[net.noresttherein.sugar.extensions extensions]].
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
	type IArray[+E] >: Null <: IArrayLike[E] //with TypedArray[E]// <: ArrayLike[E]

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
	  * @see [[net.noresttherein.sugar.arrays.RefArray$ RefArray]]
	  * @see [[net.noresttherein.sugar.arrays.RefArrayLike RefArrayLike]]
	  * @see [[net.noresttherein.sugar.arrays.ArrayLike ArrayLike]]
	  * @see [[net.noresttherein.sugar.arrays.MutableArray MutableArray]]
	  */
	type RefArray[E] >: Null <: RefArrayLike[E] with MutableArray[E] //todo: rename it to BoxArray

	/** An immutable array with elements of type `E`, represented in runtime as `Array[Any]` (that is, `Object[]`).
	  * Its interface is defined as extension methods in
	  * [[net.noresttherein.sugar.arrays.IRefArray.IRefArrayExtension IRefArrayExtension]]`[E]`,
	  * [[net.noresttherein.sugar.arrays.IRefArray.IRefArrayLikeExtension IRefArrayLikeExtension]]`[IRefArray, E]`,
	  * [[net.noresttherein.sugar.arrays.IArrayLike.IArrayLikeExtension IArrayLikeExtension]]`[IRefArray, E]`,
	  * [[net.noresttherein.sugar.arrays.IArrayLike.ArrayLikeExtension ArrayLikeExtension]]`[RefArray, E]`,
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

}