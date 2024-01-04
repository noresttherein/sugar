package net.noresttherein.sugar.arrays

import scala.annotation.nowarn
import scala.collection.IterableFactory
import scala.collection.generic.IsSeq
import scala.collection.immutable.{ArraySeq, IndexedSeqOps, StrictOptimizedSeqOps}

import net.noresttherein.sugar.arrays.IArrayLike.extensions.IArrayLikeExtensionConversion
import net.noresttherein.sugar.arrays.extensions.ArrayExtension
import net.noresttherein.sugar.collections.ArrayIterableOnce
import net.noresttherein.sugar.typist.PriorityConversion
import net.noresttherein.sugar.typist.casting.extensions.{castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/** Companion definitions to immutable array-like types, including conversion to and from Scala collections.
  * @see [[net.noresttherein.sugar.arrays.IArrayLike! IArrayLike]]
  * @define Coll `IArrayLike`
  * @define coll immutable array-like
  */
@SerialVersionUID(Ver)
case object IArrayLike extends IterableFactory.Delegate[IRefArray](IRefArray) {

	/** Extension methods for immutable [[net.noresttherein.sugar.arrays.IArrayLike IArrayLike]]`[E]` subtypes:
	  * [[net.noresttherein.sugar.arrays.IArray! IArray]]
	  * and [[net.noresttherein.sugar.arrays.IRefArray! IRefArray]].
	  * It expands the collection extensions methods in
	  * [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension ArrayLikeExtension]]
	  * by taking advantage of the immutability, and sharing this instance where possible.
	  * Typically imported as
	  * `net.noresttherein.sugar.arrays.extensions.`[[net.noresttherein.sugar.arrays.extensions.IArrayLikeExtension IArrayLikeExtension]],
	  * but available also as `net.noresttherein.sugar.arrays.IArrayLike.extensions.IArrayLikeExtension` and
	  * `net.noresttherein.sugar.IArrayLikeExtension`).
	  * @see [[net.noresttherein.sugar.arrays.ArrayLike.ArrayLikeExtension ArrayLikeExtension]] -
	  *      `IArrayLike` extension methods 'inherited' from [[net.noresttherein.sugar.arrays.ArrayLike! ArrayLike]].
	  * @see [[net.noresttherein.sugar.arrays.IArray.IArrayExtension IArrayExtension]] -
	  *      additional extension methods for `IArray`.
	  * @see [[net.noresttherein.sugar.arrays.IRefArray.IRefArrayExtension IRefArrayExtension]] -
	  *      additional extension methods for `IArray`.
	  * @tparam E   the type of the elements in the array (not necessarily the component type of the underlying array!).
	  * @tparam Arr the type constructor of an `ArrayLike` subtype returned by methods which return
	  *             an `Array` in [[collection.ArrayOps ArrayOps]] (and the `ArrayLike` subtype being granted
	  *             these methods).
	  */
	class IArrayLikeExtension[+Arr[X] <: IArrayLike[X], E] private[arrays] (private val self :Array[_]) extends AnyVal {
//		def slice(from :Int, until :Int) :Arr[E] = {
//			val len = array.length
//			if (until <= 0 | until <= from | from >= len) Array.emptyLike(array).asInstanceOf[Arr[E]]
//			else if (from <= 0 & until >= len) array.asInstanceOf[Arr[E]]
//			else array.slice(from, until).castFrom[Array[_], Arr[E]]
//		}
//		@inline def take(n :Int) :Arr[E] = slice(0, n)
//		@inline def drop(n :Int) :Arr[E] = slice(n, Int.MaxValue)
//		@inline def takeRight(n :Int) :Arr[E] = { val l = array.length; slice(l - n, l) }
//		@inline def dropRight(n :Int) :Arr[E] = slice(0, array.length - n)
//		def takeWhile(p :E => Boolean) :Arr[E] =
//			slice(0, ArrayLikeOps.segmentLength(array, 0, array.length)(p.castParam1[Any], 0))
//		def dropWhile(p :E => Boolean) :Arr[E] =
//			slice(ArrayLikeOps.segmentLength(array, 0, array.length)(p.castParam1[Any], 0), Int.MaxValue)
//
//		@inline def splitAt(n :Int) :(Arr[E], Arr[E]) = (slice(0, n), slice(n, Int.MaxValue))
//		def span(p :E => Boolean) :(Arr[E], Arr[E]) =
//			splitAt(ArrayLikeOps.segmentLength(array, 0, array.length)(p.castParam1[Any], 0))
//
//		@inline def removed(index :Int) :Arr[E] = ArrayExtension(array).removed(index).castFrom[Array[_], Arr[E]]
//		@inline def removed(from :Int, until :Int) :Arr[E] =
//			if (until <= 0 | until <= from || from >= array.length) array.asInstanceOf[Arr[E]]
//			else ArrayExtension(array).removed(from, until).castFrom[Array[_], Arr[E]]
//
//		@inline def reverse :Arr[E] =
//			if (array.length <= 1) array.asInstanceOf[Arr[E]]
//			else array.reverse.castFrom[Array[_], Arr[E]]
//
//		@inline def sortWith(lt :(E, E) => Boolean) :Arr[E] =
//			if (array.length <= 1) array.asInstanceOf[Arr[E]]
//			else array.sortWith(lt.castParams[Any, Any, Boolean]).castFrom[Array[_], Arr[E]]
//
//		@inline def sortBy[A :Ordering](f :E => A) :Arr[E] =
//			if (array.length <= 1) array.asInstanceOf[Arr[E]]
//			else array.sortBy(f.castParam1[Any]).castFrom[Array[_], Arr[E]]
//
//		@inline def sorted[A >: E :Ordering] :Arr[E] =
//			if (array.length <= 1) array.asInstanceOf[Arr[E]]
//			else array.sorted(implicitly[Ordering[A]].castParam[Any]).castFrom[Array[_], Arr[E]]
//
//		@inline def distinct :Arr[E] =
//			if (array.length <= 1) array.asInstanceOf[Arr[E]]
//			else array.distinct.castFrom[Array[_], Arr[E]]
//
//		@inline def distinctBy[A](f :E => A) :Arr[E] =
//			if (array.length <= 1) array.asInstanceOf[Arr[E]]
//			else array.distinctBy(f.asInstanceOf[Any => A]).castFrom[Array[_], Arr[E]]

		//we could also overload rotatedLeft et al.

		def iterator :Iterator[E] = ArrayIterator.immutable(self.asInstanceOf[IArrayLike[E]])

		/** A view on the index range `[from, until)` of this array as a sequence.
		  * Slicing of the returned sequence will return similar views, sharing the same underlying array.
		  */
		@inline def subseq(from :Int, until :Int) :IndexedSeq[E] =
			Wrapped.Slice(self.asInstanceOf[IArrayLike[E]], from, until)

		@inline def toSeq :Seq[E] = IArrayLike.Wrapped(self.asInstanceOf[IArrayLike[E]])
		@inline def toIndexedSeq :IndexedSeq[E] = IArrayLike.Wrapped(self.asInstanceOf[IArrayLike[E]])
		@inline def toArraySeq :ArraySeq[E] = ArraySeq.unsafeWrapArray(self).castParam[E]
		@inline def toOps :IndexedSeqOps[E, IArrayLike, IArrayLike[E]] =
			new IArrayLikeAsSeq(self.asInstanceOf[IArrayLike[E]])
	}


	/** Wraps immutable arrays (including [[net.noresttherein.sugar.arrays.IRefArray IRefArray]])
	  * in indexed sequences and extracts arrays from any supported collection backed by an array
	  * (including 'erased' `Array[AnyRef]`) ''iff'' it contains all elements in the array.
	  */
	@SerialVersionUID(Ver)
	object Wrapped {
		def apply[E](array :IArrayLike[E]) :IndexedSeq[E] =
			if (array.getClass == classOf[Array[AnyRef]]) IRefArray.Wrapped(array.asInstanceOf[IRefArray[E]])
			else IArray.Wrapped(array.asInstanceOf[IArray[E]])

		def unapply[E](elems :IterableOnce[E]) :Opt[IArrayLike[E]] = elems match {
			case seq :ArraySeq[_] =>
				Got(seq.unsafeArray.asInstanceOf[IArrayLike[E]])
			case seq :Vector[_] if seq.length == CheatedAccess.FlatVectorSize =>
				Got(CheatedAccess.array(seq).asInstanceOf[IArrayLike[E]])
			case slice :ArrayIterableOnce[E] if slice.knownSize == slice.unsafeArray.length && slice.isImmutable =>
				Got(slice.unsafeArray.asInstanceOf[IArrayLike[E]])
			case _ => Lack
		}

		/** Wraps immutable arrays (including [[net.noresttherein.sugar.arrays.IRefArray IRefArray]])
		  * in indexed sequences exposing a range of indices of said array,
		  * and extracts arrays from any supported collection backed by an array (including 'erased' `Array[AnyRef]`),
		  * together with the information about the index range which is actually a part in that collection.
		  */
		@SerialVersionUID(Ver)
		object Slice {
			def apply[E](array :IArrayLike[E], from :Int, until :Int) :IndexedSeq[E] =
				if (array.getClass == classOf[Array[AnyRef]])
					IRefArray.Wrapped.Slice(array.asInstanceOf[IRefArray[E]], from, until)
				else
					IArray.Wrapped.Slice(array.asInstanceOf[IArray[E]], from, until)

			def unapply[E](elems :IterableOnce[E]) :Opt[(IArrayLike[E], Int, Int)] = elems match {
				case seq :ArraySeq[_] =>
					Got((seq.unsafeArray.castFrom[Array[_], IArrayLike[E]], 0, seq.unsafeArray.length))
				case seq :Vector[_] if seq.length <= CheatedAccess.FlatVectorSize =>
					Got((CheatedAccess.array(seq).castFrom[Array[_], IArrayLike[E]], 0, seq.length))
				case slice :ArrayIterableOnce[E] if elems.knownSize >= 0 && slice.isImmutable =>
					val array = slice.unsafeArray.castFrom[Array[_], IArrayLike[E]]
					val start = slice.startIndex
					Got((array, start, start + slice.knownSize))
				case _ =>
					Lack

			}
		}
	}

	//fixme: precedence conflicts with ArrayLikeExtension
	implicit def IArrayLikeIsSeq[E] :IsSeq[IArrayLike[E]] { type A = E; type C = IArrayLike[E] } =
		IArray.IArrayIsSeq.asInstanceOf[IsSeq[IArrayLike[E]] { type A = E; type C = IArrayLike[E] }]
//		isSeqPrototype.asInstanceOf[IsSeq[IArrayLike[E]] { type A = E; type C = IArrayLike[E] }]
//
//	We don't know what the particular element type of the array will be, so we must use Scala's polymorphic delegates.
//	private class IArrayLikeIsSeq[E] extends ArrayLikeIsSeqTemplate[E, Seq, IArrayLike] {
//		override def apply(array :IArrayLike[E]) =
//			new IArrayLikeIsSeqOps[E, IArrayLike](array) {
//				protected override def fromSpecific(coll :IterableOnce[E]) :IArrayLike[E] = IRefArray.from(coll)
//				protected override def newSpecificBuilder :Builder[E, IArrayLike[E]] = IRefArray.newBuilder
//			}
//		private def readResolve = IArrayLike.IArrayLikeIsSeq
//	}
//	private[this] val isSeqPrototype :IArrayLikeIsSeq[Any] = new IArrayLikeIsSeq[Any]


//	private[arrays] sealed trait conversions extends Any with ArrayLike.conversions {
//		//fixme: conflicts with ArrayLikeExtension because it's more specific
////		@inline implicit final def IArrayLikeToSeq[A](self :IArrayLike[A]) :IndexedSeq[A] = Wrapped(self)
//	}
//
	/** Mixin trait with extension methods conversion for `IArrayLike` subtypes.
	  * @define Coll `IArrayLike`
	  * @define Extension `IArrayLikeExtension[Arr, E]`
	  */
	private[arrays] trait extensions extends Any with ArrayLike.extensions {
//		@inline implicit final def IArrayLikeExtension[Arr[X] <: IArrayLike[X], A](self :IArrayLike[A])
//				:IArrayLikeExtension[Arr, A] =
//			new IArrayLikeExtension(self.asInstanceOf[Array[_]])
		/** Extension methods for all `IArrayLike[E]` subtypes.
		  * $conversionInfo
		  */
		implicit final def IArrayLikeExtension[Arr[X] <: IArrayLike[X], E] :IArrayLikeExtensionConversion[Arr, E] =
			extensions.IArrayLikeExtensionConversionPrototype.asInstanceOf[IArrayLikeExtensionConversion[Arr, E]]
	}

	@SerialVersionUID(Ver)
	object extensions extends extensions {
		sealed trait IArrayLikeExtensionConversion[Arr[X] <: IArrayLike[X], E]
			extends (Arr[E] => IArrayLikeExtension[Arr, E])
		{
			@inline final def apply(v1 :Arr[E])(implicit dummy :DummyImplicit) :IArrayLikeExtension[Arr, E] =
				new IArrayLikeExtension(v1.asInstanceOf[Array[_]])
		}
		private def newIArrayLikeExtensionConversion[Arr[X] <: IArrayLike[X], E] =
			new PriorityConversion.Wrapped[Arr[E], IArrayLikeExtension[Arr, E]](
				(arr :Arr[E]) => new IArrayLikeExtension(arr.asInstanceOf[Array[_]])
			) with IArrayLikeExtensionConversion[Arr, E]
		private val IArrayLikeExtensionConversionPrototype :IArrayLikeExtensionConversion[IArrayLike, Any] =
			newIArrayLikeExtensionConversion
	}
}







private abstract class IArrayLikeIsSeqOps[E, A[X] <: IArrayLike[X]](array :A[E])
	extends ArrayLikeIsSeqOps[E, A](array.asInstanceOf[Array[E]])
	   with StrictOptimizedSeqOps[E, Seq, A[E]] with IndexedSeqOps[E, Seq, A[E]]
{
	override def isImmutable = true
	@nowarn("cat=deprecation")
	override def toIterable      = IArrayLike.Wrapped(coll)
	override def iterator        = ArrayIterator.immutable(coll)
	override def iterableFactory = IndexedSeq
}
