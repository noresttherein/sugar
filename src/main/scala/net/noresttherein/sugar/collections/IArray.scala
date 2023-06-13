package net.noresttherein.sugar.collections

import java.lang.System.arraycopy
import java.lang.{Math => math}

import scala.Array.{UnapplySeqWrapper, emptyObjectArray}
import scala.Specializable.Everything
import scala.annotation.{nowarn, tailrec}
import scala.collection.Searching.SearchResult
import scala.collection.Stepper.EfficientSplit
import scala.collection.{ClassTagIterableFactory, Factory, IndexedSeqView, IterableFactory, LazyZip2, Stepper, StepperShape, View, immutable, mutable}
import scala.collection.generic.IsSeq
import scala.collection.immutable.{ArraySeq, IndexedSeqOps, StrictOptimizedSeqOps}
import scala.collection.mutable.{Buffer, Builder}
import scala.reflect.{ClassTag, classTag}

import net.noresttherein.sugar.collections.RefArrayLike.RefArrayLikeExtension
import net.noresttherein.sugar.collections.extensions.{ArrayExtension, ArrayLikeExtension, ArrayObjectExtension, IndexedSeqExtension, IterableExtension, IterableOnceExtension, IteratorExtension, SeqExtension}
import net.noresttherein.sugar.extensions.{ClassExtension, cast2TypeParamsMethods, cast3TypeParamsMethods, castTypeParamMethods, castingMethods, classNameMethods, downcast2TypeParamsMethods}
import net.noresttherein.sugar.typist.Unknown
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




@SerialVersionUID(Ver)
case object ArrayLike {

	//Array.copy resorts to slowcopy if not classOf[E] >:> array.getClass.getComponentType.
	// We instead always default to System.arraycopy if both element types are reference types.
	@inline final def copy(src :ArrayLike[_], srcPos :Int, dst :ArrayLike[_], dstPos :Int, length :Int) :Unit =
		if (src.isInstanceOf[Array[AnyRef]] & dst.isInstanceOf[Array[AnyRef]])
			arraycopy(src, srcPos, dst, dstPos, length)
		else
			Array.copy(src, srcPos, dst, dstPos, length)

//	@inline final def copyAs[E :ClassTag](src :Array[_], from :Int)


	//Consider: there is a small issue in that the factory extension method is in extensions,
	// so a clash between imports is possible.

	/** Extension methods for all [[net.noresttherein.sugar.collections.ArrayLike ArrayLike]]`[E]` implementations.
	  * It is the standard collection methods subset which can be implemented safely without knowing
	  * the actual element type of the underlying array.
	  * Can be enabled for any `ArrayLike` (returning also a generic `ArrayLike`) by importing
	  * `net.noresttherein.sugar.collections.extensions.`[[net.noresttherein.sugar.collections.ArrayLike.extensions.ArrayLikeExtension ArrayLikeExtension]]
	  * (also `net.noresttherein.sugar.collections.ArrayLike.extensions.ArrayLikeExtension` and
	  * `net.noresttherein.sugar.ArrayLikeExtension`), or
	  * `net.noresttherein.sugar.collections.extensions.`[[net.noresttherein.sugar.collections.IArray.extensions.IArrayAsArrayLikeExtension IArrayAsArrayLikeExtension]]
	  * for [[net.noresttherein.sugar.collections.IArray IArray]],
	  * `net.noresttherein.sugar.collections.extensions.`[[net.noresttherein.sugar.collections.RefArray.extensions.RefArrayAsArrayLikeExtension RefArrayAsArrayLikeExtension]]
	  * for [[net.noresttherein.sugar.collections.RefArray RefArray]], and
	  * `net.noresttherein.sugar.collections.extensions.`[[net.noresttherein.sugar.collections.IRefArray.extensions.IRefArrayAsArrayLikeExtension IRefArrayAsArrayLikeExtension]]
	  * for [[net.noresttherein.sugar.collections.IRefArray IRefArray]].
	  * These same conversions for individual array types are also available under
	  * `ArrayLike.extensions`/`IArray.extensions`/`RefArray.extensions`/`IRefArrayExtensions.extensions`
	  * (containing implicit extension methods also from different classes, forming the full API of each type),
	  * and in object `net.noresttherein.sugar.extensions`, together with all extension methods in the whole library.
	  * @see [[net.noresttherein.sugar.collections.RefArrayLike.RefArrayLikeExtension]] - additional extension methods
	  *      for `RefArray` and `IRefArray`.
	  * @see [[net.noresttherein.sugar.collections.IArray.IArrayExtension]] - additional extension methods for `IArray`.
	  * @tparam E   the type of the elements in the array (not necessarily the component type of the underlying array!).
	  * @tparam Arr the type constructor of an `ArrayLike` subtype returned by methods which return
	  *             an `Array` in [[collection.ArrayOps ArrayOps]] (and the `ArrayLike` subtype being granted
	  *             these methods).
	  */ //todo: verify these delegate to ArrayOps and ArrayExtension, and not cause an infinite recursion
	class ArrayLikeExtension[E, Arr[X]] private[collections] (private[ArrayLike] val array :Array[_])
		extends AnyVal
	{   //avoid casting array to Array[E], as it may easily cause problems if inlined
		@inline def knownSize :Int = array.length
		@inline def length    :Int = array.length
		@inline def size      :Int = array.length
		@inline def isEmpty   :Boolean = array.length == 0
		@inline def nonEmpty  :Boolean = array.length > 0
		@inline def sizeIs    :Int = array.length
		@inline def lengthIs  :Int = array.length
		@inline def sizeCompare(otherSize :Int) :Int= Integer.compare(array.length, otherSize)
		@inline def lengthCompare(len :Int) :Int = Integer.compare(array.length, len)

		@inline def head :E = array(0).asInstanceOf[E]
		@inline def last :E = array(array.length - 1).asInstanceOf[E]
		@inline def headOption :Option[E] = array.headOption.castParam[E]
		@inline def lastOption :Option[E] = array.lastOption.castParam[E]

		@inline def apply(i :Int) :E = array(i).asInstanceOf[E]

		@inline def forall(p :E => Boolean) :Boolean = array.forall(p.castFrom[E => Boolean, Any => Boolean])
		@inline def exists(p :E => Boolean) :Boolean = array.exists(p.castFrom[E => Boolean, Any => Boolean])
		@inline def count(p :E => Boolean) :Int = array.count(p.castFrom[E => Boolean, Any => Boolean])
		@inline def find(p :E => Boolean) :Option[E] = array.find(p.castFrom[E => Boolean, Any => Boolean]).castParam[E]
		def findLast(p :E => Boolean) :Option[E] = {
			var i = array.length - 1
			while (i >= 0 && !p(array(i).asInstanceOf[E]))
				i -= 1
			if (i < 0) None else Some(array(i).asInstanceOf[E])
		}
		@inline def indexWhere(p :E => Boolean, from :Int = 0) :Int =
			array.indexWhere(p.castFrom[E => Boolean, Any => Boolean], from)

		@inline def lastIndexWhere(p :E => Boolean, end :Int = Int.MaxValue) :Int =
			array.lastIndexWhere(p.castFrom[E => Boolean, Any => Boolean], end)

		@inline def segmentLength(p :E => Boolean, from :Int = 0) :Int = {
			val i = array.indexWhere(p.castFrom[E => Boolean, Any => Boolean], from)
			if (i < 0) array.length - math.max(from, 0)
			else i - math.max(from, 0)
		}

		@inline def startsWith[A >: E](that :IterableOnce[A]) :Boolean =
			array.startsWith(that.castParam[Any], 0)

		@inline def startsWith[A >: E](that :IterableOnce[A], offset :Int) :Boolean =
			array.startsWith(that.castParam[Any], offset)

		@inline def startsWith[A >: E](that :ArrayLike[A], offset :Int) :Boolean =
			array.castFrom[Array[_], Array[A]].startsWith(that.castFrom[ArrayLike[A], Array[A]], offset)

		@inline def startsWith[A >: E](that :ArrayLike[A]) :Boolean =
			array.castFrom[Array[_], Array[A]].startsWith(that.castFrom[ArrayLike[A], Array[A]], 0)

		@inline def endsWith[A >: E](that :Iterable[A]) :Boolean = array.endsWith(that.castParam[Any])
		@inline def endsWith[A >: E](that :ArrayLike[A]) :Boolean =
			array.castFrom[Array[_], Array[A]].endsWith(that.castFrom[ArrayLike[A], Array[A]])

		@inline def indexOfSlice[A >: E](that :collection.Seq[A]) :Int =
			Wrapped(array).castParam[E].indexOfSlice(that)

		@inline def indexOfSlice[A >: E](that :collection.Seq[A], from :Int) :Int =
			Wrapped(array).castParam[E].indexOfSlice(that, from)

		@inline def lastIndexOfSlice[A >: E](that :collection.Seq[A]) :Int =
			Wrapped(array).castParam[E].lastIndexOfSlice(that)

		@inline def lastIndexOfSlice[A >: E](that :collection.Seq[A], end :Int) :Int =
			Wrapped(array).castParam[E].lastIndexOfSlice(that, end)

		@inline def indexOfSlice[A >: E](that :ArrayLike[A]) :Int =
			Wrapped(array).castParam[E].indexOfSlice(Wrapped(that), 0)

		@inline def indexOfSlice[A >: E](that :ArrayLike[A], from :Int) :Int =
			Wrapped(array).castParam[E].indexOfSlice(Wrapped(that), from)

		@inline def lastIndexOfSlice[A >: E](that :ArrayLike[A]) :Int =
			Wrapped(array).castParam[E].lastIndexOfSlice(Wrapped(that), 0)

		@inline def lastIndexOfSlice[A >: E](that :ArrayLike[A], end :Int) :Int =
			Wrapped(array).castParam[E].lastIndexOfSlice(Wrapped(that), end)

		@inline def containsSlice[A >: E](that :collection.Seq[A]) :Boolean =
			Wrapped(array).castParam[E].indexOfSlice(that, 0) >= 0

		@inline def containsSlice[A >: E](that :ArrayLike[A]) :Boolean =
			Wrapped(array).castParam[E].indexOfSlice(Wrapped(that), 0) >= 0


		@inline def corresponds[A](that :IterableOnce[A])(p :(E, A) => Boolean) :Boolean =
			Wrapped(array).castParam[E].corresponds(that)(p)

		@inline def corresponds[A](that :ArrayLike[A])(p :(E, A) => Boolean) :Boolean =
			Wrapped(array).castParam[E].corresponds(Wrapped(that))(p)

		@inline def sum[A >: E](implicit num :Numeric[A]) :A = Wrapped(array).castParam[E].sum[A]
		@inline def product[A >: E](implicit num :Numeric[A]) :A = Wrapped(array).castParam[E].product[A]
		@inline def min[A >: E](implicit ord :Ordering[A]) :E = Wrapped(array).castParam[E].min[A]
		@inline def max[A >: E](implicit ord :Ordering[A]) :E = Wrapped(array).castParam[E].max[A]
		@inline def minOption[A >: E](implicit ord :Ordering[A]) :Option[E] = Wrapped(array).castParam[E].minOption[A]
		@inline def maxOption[A >: E](implicit ord :Ordering[A]) :Option[E] = Wrapped(array).castParam[E].maxOption[A]
		@inline def minBy[A](f :E => A)(implicit cmp :Ordering[A]) :E = Wrapped(array).castParam[E].minBy(f)
		@inline def maxBy[A](f :E => A)(implicit cmp :Ordering[A]) :E = Wrapped(array).castParam[E].maxBy(f)
		@inline def minByOption[A](f :E => A)(implicit cmp :Ordering[A]) :Option[E] =
			Wrapped(array).castParam[E].minByOption(f)

		@inline def maxByOption[A](f :E => A)(implicit cmp :Ordering[A]) :Option[E] =
			Wrapped(array).castParam[E].maxByOption(f)

		/** Search this array for a specific element.
		  * The array should be sorted with the same `Ordering` before calling; otherwise, the results are undefined.
		  * @param elem     the element to find.
		  * @return a `Found` value containing the index corresponding to the element in the
		  *         sequence, or the `InsertionPoint` where the element would be inserted if
		  *         the element is not in the sequence.
		  */ //consider: if we delegated this to IndexedSeq.search, we could use this method also for a regular Array[E]
		@inline def search[A >: E :Ordering](elem :A) :SearchResult =
			 array.search(elem, 0, array.length)(implicitly[Ordering[A]].castParam[Any])

		/** Search the specified range of this array for a given element.
		  * The array should be sorted with the same `Ordering` before calling; otherwise, the results are undefined.
		  * @param elem     the element to find.
		  * @param from     the index of the first element in the searched range.
		  * @param until    the index following the last element in the searched range.*
		  * @return a `Found` value containing the index corresponding to the element in the
		  *         sequence, or the `InsertionPoint` where the element would be inserted if
		  *         the element is not in the sequence.
		  */
		@inline def search[A >: E :Ordering](elem :A, from :Int, until :Int) :SearchResult =
			array.search(elem, from, until)(implicitly[Ordering[A]].castParam[Any])

		/** Performs a binary search of element `x` in a section of this array, sorted according
		  * to an implicit `Ordering[E]`. If the array is not sorted, or the `Ordering` is not consistent with `equals`,
		  * then the behaviour is unspecified.
		  * @return index of the search key, if it is contained in the array;
		  *         otherwise, `(-(''insertion point'') - 1)`.  The `insertion point` is defined as the point at which
		  *         the key would be inserted into the array: the index of the first element in the range
		  *         greater than the key, or `until` if all elements in the range are less than the specified key.
		  *         Note that this guarantees that the return value will be &gt;= 0 if and only if the key is found.
		  */
		@inline final def binarySearch[A >: E :Ordering](x :A) :Int =
			array.binarySearch(0, array.length, x)(implicitly[Ordering[A]].castParam[Any])

		/** Performs a binary search of element `x` in a section of this array, sorted according
		  * to an implicit `Ordering[E]`. If `until < from`, then `from` is returned immediately.
		  * Otherwise, an index `from <= n < until` of the first element greater or equal `x`, or `this.length`
		  * if all elements in the `[from,until)` range are strictly less than `x`. If the array is not sorted,
		  * or the `Ordering` is not consistent with `equals`, then the behaviour is unspecified.
		  * @return index of the search key, if it is contained in the array within the specified range;
		  *         otherwise, `(-(''insertion point'') - 1)`.  The `insertion point` is defined as the point at which
		  *         the key would be inserted into the array: the index of the first element in the range
		  *         greater than the key, or `until` if all elements in the range are less than the specified key.
		  *         Note that this guarantees that the return value will be &gt;= 0 if and only if the key is found.
		  */ //consider: using an opaque type for the result, maybe for now a value class?
		@throws[IndexOutOfBoundsException]("if from < 0 or from > array.length or until > array.length.")
		@inline def binarySearch[A >: E :Ordering](from :Int, until :Int, x :A) :Int =
			array.binarySearch(from, until, x)(implicitly[Ordering[A]].castParam[Any])

		@inline def /: [A](z :A)(op :(A, E) => A) :A = array.foldLeft[A](z)(op.castParams[A, Any, A])
		@inline def :\ [A](z :A)(op :(E, A) => A) :A = array.foldRight[A](z)(op.castParams[Any, A, A])

		@inline def foldLeft[A](z :A)(op :(A, E) => A) :A = array.foldLeft(z)(op.castParams[A, Any, A])
		@inline def foldRight[A](z :A)(op :(E, A) => A) :A = array.foldRight(z)(op.castParams[Any, A, A])
		@inline def fold[A >: E](z :A)(op :(A, A) => A) :A = array.fold(z)(op.castParams[Any, Any, Any]).asInstanceOf[A]
		@inline def reduce[A >: E](op :(A, A) => A) :A = array.reduce(op.castParams[Any, Any, Any]).asInstanceOf[A]
		@inline def reduceOption[A >: E](op :(A, A) => A) :Option[A] =
			array.reduceOption(op.castParams[Any, Any, Any]).castParam[A]

		@inline def reduceLeft[A >: E](op :(A, E) => A) :A =
			array.reduceLeft(op.castParams[Any, Any, Any]).asInstanceOf[A]

		@inline def reduceRight[A >: E](op :(E, A) => A) :A =
			array.reduceRight(op.castParams[Any, Any, Any]).asInstanceOf[A]

		@inline def reduceLeftOption[A >: E](op :(A, E) => A) :Option[A] =
			array.reduceLeftOption(op.castParams[Any, Any, Any]).castParam[A]

		@inline def reduceRightOption[A >: E](op :(E, A) => A) :Option[A] =
			array.reduceRightOption(op.castParams[Any, Any, Any]).castParam[A]

		@inline def slice(from :Int, until :Int) :Arr[E] = array.slice(from, until).castFrom[Array[_], Arr[E]]
		@inline def take(n :Int) :Arr[E] = array.take(n).castFrom[Array[_], Arr[E]]
		@inline def drop(n :Int) :Arr[E] = array.drop(n).castFrom[Array[_], Arr[E]]
		@inline def takeRight(n :Int) :Arr[E] = array.takeRight(n).castFrom[Array[_], Arr[E]]
		@inline def dropRight(n :Int) :Arr[E] = array.dropRight(n).castFrom[Array[_], Arr[E]]
		@inline def takeWhile(p :E => Boolean) :Arr[E] =
			array.takeWhile(p.castFrom[E => Boolean, Any => Boolean]).castFrom[Array[_], Arr[E]]

		@inline def dropWhile(p :E => Boolean) :Arr[E] =
			array.dropWhile(p.castFrom[E => Boolean, Any => Boolean]).castFrom[Array[_], Arr[E]]

		@inline def splitAt(n :Int) :(Arr[E], Arr[E]) =
			array.splitAt(n).castFrom[(Array[_], Array[_]), (Arr[E], Arr[E])]

		@inline def span(p :E => Boolean) :(Arr[E], Arr[E]) =
			array.span(p.castFrom[E => Boolean, Any => Boolean]).castFrom[(Array[_], Array[_]), (Arr[E], Arr[E])]

		@inline def removed(index :Int) :Arr[E] = array.removed(index).castFrom[Array[_], Arr[E]]
		@inline def removed(from :Int, until :Int) :Arr[E] = array.removed(from, until).castFrom[Array[_], Arr[E]]

		@inline def reverse :Arr[E] = array.reverse.castFrom[Array[_], Arr[E]]

		@inline def sortWith(lt :(E, E) => Boolean) :Arr[E] = array.sortWith(lt.castParams[Any, Any, Boolean]).castFrom[Array[_], Arr[E]]
		@inline def sortBy[A :Ordering](f :E => A) :Arr[E] =
			array.sortBy(f.castParam1[Any]).castFrom[Array[_], Arr[E]]

		@inline def sorted[A >: E :Ordering] :Arr[E] =
			array.sorted(implicitly[Ordering[A]].castParam[Any]).castFrom[Array[_], Arr[E]]

		@inline def distinct :Arr[E] = array.distinct.castFrom[Array[_], Arr[E]]
		@inline def distinctBy[A](f :E => A) :Arr[E] = array.distinctBy(f.asInstanceOf[Any => A]).castFrom[Array[_], Arr[E]]


		@inline def tail :Arr[E] = array.tail.castFrom[Array[_], Arr[E]]
		@inline def init :Arr[E] = array.init.castFrom[Array[_], Arr[E]]

		@inline def tails :Iterator[Arr[E]] = array.tails.castFrom[Iterator[Array[_]], Iterator[Arr[E]]]
		@inline def inits :Iterator[Arr[E]] = array.inits.castFrom[Iterator[Array[_]], Iterator[Arr[E]]]
		@inline def grouped(size :Int) :Iterator[Arr[E]] =
			array.grouped(size).castFrom[Iterator[Array[_]], Iterator[Arr[E]]]

		@inline def sliding(size :Int, step :Int = 1) :Iterator[Arr[E]] =
			array.sliding(size, step).castFrom[Iterator[Array[_]], Iterator[Arr[E]]]

		@inline def combinations(n :Int) :Iterator[Arr[E]] =
			array.combinations(n).castFrom[Iterator[Array[_]], Iterator[Arr[E]]]

		@inline def permutations :Iterator[Arr[E]] =
			array.permutations.castFrom[Iterator[Array[_]], Iterator[Arr[E]]]

		@inline def iterator :Iterator[E] = ArrayIterator(array).castParam[E] //array.iterator.asInstanceOf[Iterator[E]]
		@inline def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S with EfficientSplit =
			array.castFrom[Array[_], Array[Unknown]].stepper(shape.castFrom[StepperShape[E, S], StepperShape[Unknown, S]])

		@inline def reverseIterator :Iterator[E] = array.reverseIterator.castParam[E]

		@inline def filter(p :E => Boolean) :Arr[E] = array.filter(p.castFrom[E => Boolean, Any => Boolean]).castFrom[Array[_], Arr[E]]
		@inline def filterNot(p :E => Boolean) :Arr[E] = array.filterNot(p.castFrom[E => Boolean, Any => Boolean]).castFrom[Array[_], Arr[E]]
		@inline def partition(p :E => Boolean) :(Arr[E], Arr[E]) =
			array.partition(p.castFrom[E => Boolean, Any => Boolean]).castFrom[(Array[_], Array[_]), (Arr[E], Arr[E])]

		@inline def collectFirst[A](pf :PartialFunction[E, A]) :Option[A] = array.collectFirst(pf.castParam1[Any])

		@inline def groupBy[K](f: E => K) :Map[K, Arr[E]] =
			array.groupBy(f.castParam1[Any]).castFrom[Map[K, Array[_]], Map[K, Arr[E]]]

		@inline def tapEach[U](f :E => U) :Arr[E] = { foreach(f); array.castFrom[Array[_], Arr[E]] }
		@inline def foreach[U](f :E => U) :Unit = array.foreach(f.castParam1[Any])

		@inline def zipWithIndex :Arr[(E, Int)] =
			array.castFrom[Array[_], Array[Unknown]].zipWithIndex.castFrom[Array[(Unknown, Int)], Arr[(E, Int)]]
		@inline def zip[A](that :IterableOnce[A]) :Arr[(E, A)] =
			array.castFrom[Array[_], Array[Unknown]].zip(that).castFrom[Array[(Unknown, A)], Arr[(E, A)]]

		@inline def lazyZip[A](that: Iterable[A]) :LazyZip2[E, A, Arr[E]] =
			array.lazyZip(that).castParams[E, A, Arr[E]]

		@inline def zipAll[A >: E, B](that :Iterable[B], thisElem :A, thatElem :B) :Arr[(A, B)] =
			array.zipAll(that, thisElem, thatElem).castFrom[Array[(Any, B)], Arr[(A, B)]]

		@inline def diff[A >: E](that :collection.Seq[A]) :Arr[E] = array.diff(that).castFrom[Array[_], Arr[E]]

		@inline def intersect[A >: E](that :collection.Seq[A]) :Arr[E] = array.intersect(that).castFrom[Array[_], Arr[E]]

		@inline def transpose[A](implicit asArray :E => Arr[A]): Arr[Arr[A]] =
			array.transpose(asArray.castFrom[E => Arr[A], Any => Array[A]]).castFrom[Array[Array[A]], Arr[Arr[A]]]

		@inline def rotatedLeft(n :Int) :Arr[E] = array.rotatedLeft(n).castFrom[Array[_], Arr[E]]
		@inline def rotatedLeft(from :Int, until :Int)(n :Int) :Arr[E] =
			array.rotatedLeft(from, until)(n).castFrom[Array[_], Arr[E]]

		@inline def rotatedRight(n :Int) :Arr[E] = array.rotatedRight(n).castFrom[Array[_], Arr[E]]
		@inline def rotatedRight(from :Int, until :Int)(n :Int) :Arr[E] =
			array.rotatedRight(from, until)(n).castFrom[Array[_], Arr[E]]

		@inline def view :IndexedSeqView[E] = array.view.castParam[E]
		@inline def indices :Range = Range(0, array.length)

		@inline def to[C1](factory :Factory[E, C1]) :C1 = array.to(factory.castParam1[Any])
		@inline def toSeq :Seq[E] = array.toSeq.asInstanceOf[Seq[E]]
		@inline def toIndexedSeq :IndexedSeq[E] = array.toIndexedSeq.castParam[E]
		@inline def toList :List[E] = array.toList.castParam[E]
		@inline def toVector :Vector[E] = array.toVector.castParam[E]
		@inline def toSet[A >: E] :Set[A] = array.toSet[Any].castParam[A]
		@inline def toMap[K, V](implicit ev :E <:< (K, V)) :Map[K, V] = array.toMap(ev.castParam1[Any])
		@inline def toBuffer[A >: E]: Buffer[A] = Buffer.from(array).castParam[A]
		@inline def toArray[A >: E :ClassTag] :Array[A] = array.toArray(classTag[A].castParam[Any]).castFrom[Array[_], Array[A]]
		@inline def toIArray[A >: E :ClassTag] :IArray[A] = array.toArray(classTag[A].castParam[Any]).castFrom[Array[_], IArray[A]]
		@inline def toRefArray[A >: E] :RefArray[A] = array.toArray(ClassTag.Any).castFrom[Array[Any], RefArray[A]]
		@inline def toIRefArray[A >: E] :IRefArray[A] = array.toArray(ClassTag.Any).castFrom[Array[Any], IRefArray[A]]

		@inline def copyToArray[A >: E](xs :Array[A]) :Int =
			array.castFrom[Array[_], Array[Unknown]].copyToArray(xs.castFrom[Array[_], Array[Unknown]], 0, xs.length)
		@inline def copyToArray[A >: E](xs :Array[A], start :Int) :Int =
			array.castFrom[Array[_], Array[Unknown]].copyToArray(xs.castFrom[Array[_], Array[Unknown]], start, xs.length)

		@inline def copyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int =
			array.castFrom[Array[_], Array[Unknown]].copyToArray(xs.castFrom[Array[_], Array[Unknown]], start, len)

		/** Equivalent to `this.slice(from, until).copyToArray(xs)`, but doesn't create an intermediate array. */
		@inline def copyRangeToArray[A >: E](xs :Array[A], from :Int, until :Int) :Int =
			copyRangeToArray(xs, 0, from, until)

		/** Equivalent to `this.slice(from, until).copyToArray(xs, start)`, but doesn't create an intermediate array. */
		def copyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, until :Int) :Int =
			if (until <= from | until < 0 || from >= array.length || start >= xs.length)
				0
			else if (start < 0)
				throw new IndexOutOfBoundsException(
					array.className + "|" + array.length + "|.copyRangeToArray(" + xs.className + "{" + xs.length + "}, "
						+ start + ", " + from + ", " + until + ")"
				)
			else {
				val cap   = math.min(until, array.length)
				val from0 = math.max(from, 0)
				val count = math.min(cap - from0, xs.length - start)
				ArrayLike.copy(array, from, xs, start, count)
				count
			}

		@inline def mkString :String = array.mkString("", "", "")
		@inline def mkString(separator :String) :String = array.mkString("", separator, "")
		@inline def mkString(prefix :String, separator :String, suffix :String) :String =
			array.mkString(prefix, separator, suffix)

		@inline def addString(b :StringBuilder) :b.type = array.addString(b, "", "", "")
		@inline def addString(b :StringBuilder, sep :String) :b.type = array.addString(b, "", sep, "")
		@inline def addString(b :StringBuilder, start :String, sep :String, end :String) :b.type =
			array.addString(b, start, sep, end)

		@inline def mismatch[A >: E](that :ArrayLike[E]) :Int = {
			val asArray = that.castFrom[ArrayLike[E], Array[_]]
			array.mismatch(asArray, 0, array.length, 0, asArray.length)
		}

		@inline def mismatch[A >: E](that :ArrayLike[E], thisFrom :Int, thisUntil :Int, thatFrom :Int, thatUntil :Int) :Int =
			array.mismatch(that.castFrom[ArrayLike[E], Array[_]], thisFrom, thisUntil, thatFrom, thatUntil)

//		@inline def sameElements[A](other :A)(implicit isArray :A <:< Array[_]) :Boolean = array sameElements isArray(other)
		@inline def sameElements(other :ArrayLike[_]) :Boolean = array sameElements other.castFrom[ArrayLike[_], Array[_]]
		@inline def sameElements(other :IterableOnce[_]) :Boolean = array sameElements other

		@inline def contentsString :String = array.contentsString
	}


//
//	/** An index of a particular [[net.noresttherein.sugar.collections.RefArrayLike RefArrayLike]] subtype `Arr[E]`,
//	  * used to modify the value at that index:
//	  * {{{
//	  *     array @# 42 := "*"
//	  * }}}
//	  */
//	final class RefArrayLikeIndex[Arr[_], E] private[collections] (self :Array[_], val index :Int) {
////		def this(array :Arr[E], index :Int) = this(array.asInstanceOf[Array[_]], index)
//
//		/** The underlying array. */
//		def array :Arr[E] = self.asInstanceOf[Arr[E]]
//
//		/** The underlying array updated at this index to `elem`. */
//		@inline def :=[U >: E](elem :U) :Arr[U] = self.updated(index, elem).asInstanceOf[Arr[U]]
//	}



	/** A factory and pattern for all known collections wrapping arrays, including all
	  * [[net.noresttherein.sugar.collections.ArrayLike ArrayLike]] subtypes.
	  */
	@SerialVersionUID(Ver)
	object Wrapped {
		def apply[A](array :ArrayLike[A]) :collection.IndexedSeq[A] =
			ArrayLikeSlice.of(array.castFrom[ArrayLike[A], Array[A]])

		def unapply[A](items :IterableOnce[A]) :Opt[ArrayLike[A]] = items match {
			case seq :mutable.ArraySeq[A]   => Got(seq.array.castFrom[Array[_], ArrayLike[A]])
			case seq :ArraySeq[A]           => Got(seq.unsafeArray.castFrom[Array[_], ArrayLike[A]])
			case seq :AbstractArraySlice[A] => Got(seq.unsafeArray.castFrom[Array[_], ArrayLike[A]])
			case _ => Lack
		}

		/** A factory and pattern for non-immutable collections backed by array slices. */
		@SerialVersionUID(Ver)
		object Slice {
			def apply[A](array :ArrayLike[A], from :Int, until :Int) :collection.IndexedSeq[A] =
				ArrayLikeSlice.of[A](array.castFrom[ArrayLike[A], Array[A]], from, until)

			def unapply[A](items :IterableOnce[A]) :Opt[(ArrayLike[A], Int, Int)] = items match {
				case seq :ArraySeq[A] =>
					Got((seq.unsafeArray.castFrom[Array[_], ArrayLike[A]], 0, seq.unsafeArray.length))
				case seq :mutable.ArraySeq[A] =>
					Got((seq.array.castFrom[Array[_], ArrayLike[A]], 0, seq.array.length))
				case seq :AbstractArraySlice[A] =>
					Got(seq.unsafeArray.castFrom[Array[_], ArrayLike[A]], seq.startIndex, seq.knownSize)
				case _ => Lack
			}

		}
	}


	
	implicit def arrayLikeToIterableOnce[A](array :ArrayLike[A]) :IterableOnce[A] =
		new ArrayAsSeq(array.asInstanceOf[Array[_]]).castParam[A]

	implicit def ArrayLikeOrdering[E :Ordering] :Ordering[ArrayLike[E]] =
		Array.ArrayOrdering[E].castFrom[Ordering[Array[E]], Ordering[ArrayLike[E]]]

	implicit def ArrayLikeToSeq[E](array :IArray[E]) :collection.IndexedSeq[E] = Wrapped(array)

	//fixme: precedence conflicts with ArrayLikeExtension
	implicit def ArrayLikeIsSeq[E] :IsSeq[ArrayLike[E]] { type A = E; type C = ArrayLike[E] } =
		isSeqPrototype.asInstanceOf[IsSeq[ArrayLike[E]] { type A = E; type C = ArrayLike[E] }]

	//We don't know what the particular element type of the array will be, so we must use Scala's polymorphic delegates.
	private class ArrayLikeIsSeq[E] extends IsSeq[ArrayLike[E]] with Serializable {
		override type C = ArrayLike[E]
		override type A = E
		override def apply(array :ArrayLike[E]) =
			new collection.StrictOptimizedSeqOps[E, collection.Seq, ArrayLike[E]]
				with collection.IndexedSeqOps[E, collection.Seq, ArrayLike[E]]
			{
				override def length = array.asInstanceOf[Array[_]].length
				override def apply(i :Int) = array.castFrom[ArrayLike[E], Array[_]].apply(i).asInstanceOf[E]
				override def fromSpecific(coll :IterableOnce[E]) = IRefArray.from(coll)
				override def newSpecificBuilder = IRefArray.newBuilder
				override def iterator = array.iterator

				override def toIterable = Wrapped(array)
				override def coll = array
				override def iterableFactory = collection.IndexedSeq
			}
		private def readResolve = ArrayLike.ArrayLikeIsSeq
	}
	private[this] val isSeqPrototype :ArrayLikeIsSeq[Any] = new ArrayLikeIsSeq[Any]


	private[collections] trait extensions extends Any {
		@inline implicit final def ArrayLikeExtension[A](self :ArrayLike[A]) :ArrayLikeExtension[A, ArrayLike] =
			new ArrayLikeExtension(self.asInstanceOf[Array[_]])
	}

	@SerialVersionUID(Ver)
	object extensions extends extensions
}






/** Factory of immutable arrays: `IArray` backed by an `Array`, without exposing any mutating methods.
  * Aside from standard factory methods inherited from `ClassTagIterableFactory`, and those modelled after
  * factory methods in [[Array]] and [[java.util.Arrays]], there is another convenient way of initializing
  * a new instance without a need for an intermediate collection in method
  * [[net.noresttherein.sugar.collections.IArray.init init]], which grants a temporary access to the new object
  * as a regular `Array`:
  * {{{
  *     IArray.init[String](3){ array =>
  *         array(0) = "You"
  *         array(1) = "Boo"
  *         array(2) = "I"
  *     }
  * }}}
  * @define Coll `IArray`
  * @define coll immutable array
  */
@SerialVersionUID(Ver)
case object IArray extends ClassTagIterableFactory[IArray] {

	/** Accessor extension methods for [[net.noresttherein.sugar.collections.IArray IArray]]`[Byte]`.
	  * Enabled by importing any of 
	  * `net.noresttherein.sugar.collections.extensions.`[[net.noresttherein.sugar.collections.extensions.ByteIArrayExtension ByteIArrayExtension]],
	  * `net.noresttherein.sugar.collections.IArray.extensions.ByteIArrayExtension`, 
	  * or `net.noresttherein.sugar.extensions.ByteIArrayExtension`.
	  * @see [[net.noresttherein.sugar.collections.IArray.GenericIArrayExtension]]
	  * @see [[net.noresttherein.sugar.collections.IArray.IArrayExtension]]
	  * @see [[net.noresttherein.sugar.collections.ArrayLike.ArrayLikeExtension]]
	  */
	@inline class ByteIArrayExtension private[collections] (private val array :Array[Byte]) extends AnyVal {
		@inline def length :Int = array.length
		@inline def head :Byte = array(0)
		@inline def last :Byte = array(array.length - 1)
		@inline def apply(n :Int) :Byte = array(n)
		@inline def toArraySeq :ArraySeq.ofByte = new ArraySeq.ofByte(array)
	}

	/** Accessor extension methods for [[net.noresttherein.sugar.collections.IArray IArray]]`[Short]`.
	  * Enabled by importing any of
	  * `net.noresttherein.sugar.collections.extensions.`[[net.noresttherein.sugar.collections.extensions.ShortIArrayExtension ShortIArrayExtension]],
	  * `net.noresttherein.sugar.collections.IArray.extensions.ShortIArrayExtension`,
	  * or `net.noresttherein.sugar.extensions.ShortIArrayExtension`.
	  * @see [[net.noresttherein.sugar.collections.IArray.GenericIArrayExtension]]
	  * @see [[net.noresttherein.sugar.collections.IArray.IArrayExtension]]
	  * @see [[net.noresttherein.sugar.collections.ArrayLike.ArrayLikeExtension]]
	  */
	@inline class ShortIArrayExtension private[collections] (private val array :Array[Short]) extends AnyVal {
		@inline def length :Int = array.length
		@inline def head :Short = array(0)
		@inline def last :Short = array(array.length - 1)
		@inline def apply(n :Int) :Short = array(n)
		@inline def toArraySeq :ArraySeq.ofShort = new ArraySeq.ofShort(array)
	}

	/** Accessor extension methods for [[net.noresttherein.sugar.collections.IArray IArray]]`[Char]`.
	  * Enabled by importing any of
	  * `net.noresttherein.sugar.collections.extensions.`[[net.noresttherein.sugar.collections.extensions.CharIArrayExtension CharIArrayExtension]],
	  * `net.noresttherein.sugar.collections.IArray.extensions.CharIArrayExtension`,
	  * or `net.noresttherein.sugar.extensions.CharIArrayExtension`.
	  * @see [[net.noresttherein.sugar.collections.IArray.GenericIArrayExtension]]
	  * @see [[net.noresttherein.sugar.collections.IArray.IArrayExtension]]
	  * @see [[net.noresttherein.sugar.collections.ArrayLike.ArrayLikeExtension]]
	  */
	@inline class CharIArrayExtension private[collections] (private val array :Array[Char]) extends AnyVal {
		@inline def length :Int = array.length
		@inline def head :Char = array(0)
		@inline def last :Char = array(array.length - 1)
		@inline def apply(n :Int) :Char = array(n)
		@inline def toArraySeq :ArraySeq.ofChar = new ArraySeq.ofChar(array)
	}

	/** Accessor extension methods for [[net.noresttherein.sugar.collections.IArray IArray]]`[Int]`.
	  * Enabled by importing any of
	  * `net.noresttherein.sugar.collections.extensions.`[[net.noresttherein.sugar.collections.extensions.IntIArrayExtension IntIArrayExtension]],
	  * `net.noresttherein.sugar.collections.IArray.extensions.IntIArrayExtension`,
	  * or `net.noresttherein.sugar.extensions.IntIArrayExtension`.
	  * @see [[net.noresttherein.sugar.collections.IArray.GenericIArrayExtension]]
	  * @see [[net.noresttherein.sugar.collections.IArray.IArrayExtension]]
	  * @see [[net.noresttherein.sugar.collections.ArrayLike.ArrayLikeExtension]]
	  */
	@inline class IntIArrayExtension private[collections] (private val array :Array[Int]) extends AnyVal {
		@inline def length :Int = array.length
		@inline def head :Int = array(0)
		@inline def last :Int = array(array.length - 1)
		@inline def apply(n :Int) :Int = array(n)
		@inline def toArraySeq :ArraySeq.ofInt = new ArraySeq.ofInt(array)
	}

	/** Accessor extension methods for [[net.noresttherein.sugar.collections.IArray IArray]]`[Long]`.
	  * Enabled by importing any of
	  * `net.noresttherein.sugar.collections.extensions.`[[net.noresttherein.sugar.collections.extensions.LongIArrayExtension LongIArrayExtension]],
	  * `net.noresttherein.sugar.collections.IArray.extensions.LongIArrayExtension`,
	  * or `net.noresttherein.sugar.extensions.LongIArrayExtension`.
	  * @see [[net.noresttherein.sugar.collections.IArray.GenericIArrayExtension]]
	  * @see [[net.noresttherein.sugar.collections.IArray.IArrayExtension]]
	  * @see [[net.noresttherein.sugar.collections.ArrayLike.ArrayLikeExtension]]
	  */
	@inline class LongIArrayExtension private[collections] (private val array :Array[Long]) extends AnyVal {
		@inline def length :Int = array.length
		@inline def head :Long = array(0)
		@inline def last :Long = array(array.length - 1)
		@inline def apply(n :Int) :Long = array(n)
		@inline def toArraySeq :ArraySeq.ofLong = new ArraySeq.ofLong(array)
	}

	/** Accessor extension methods for [[net.noresttherein.sugar.collections.IArray IArray]]`[Float]`.
	  * Enabled by importing any of
	  * `net.noresttherein.sugar.collections.extensions.`[[net.noresttherein.sugar.collections.extensions.FloatIArrayExtension FloatIArrayExtension]],
	  * `net.noresttherein.sugar.collections.IArray.extensions.FloatIArrayExtension`,
	  * or `net.noresttherein.sugar.extensions.FloatIArrayExtension`.
	  * @see [[net.noresttherein.sugar.collections.IArray.GenericIArrayExtension]]
	  * @see [[net.noresttherein.sugar.collections.IArray.IArrayExtension]]
	  * @see [[net.noresttherein.sugar.collections.ArrayLike.ArrayLikeExtension]]
	  */
	@inline class FloatIArrayExtension private[collections] (private val array :Array[Float]) extends AnyVal {
		@inline def length :Int = array.length
		@inline def head :Float = array(0)
		@inline def last :Float = array(array.length - 1)
		@inline def apply(n :Int) :Float = array(n)
		@inline def toArraySeq :ArraySeq.ofFloat = new ArraySeq.ofFloat(array)
	}

	/** Accessor extension methods for [[net.noresttherein.sugar.collections.IArray IArray]]`[Double]`.
	  * Enabled by importing any of
	  * `net.noresttherein.sugar.collections.extensions.`[[net.noresttherein.sugar.collections.extensions.DoubleIArrayExtension DoubleIArrayExtension]],
	  * `net.noresttherein.sugar.collections.IArray.extensions.DoubleIArrayExtension`,
	  * or `net.noresttherein.sugar.extensions.DoubleIArrayExtension`.
	  * @see [[net.noresttherein.sugar.collections.IArray.GenericIArrayExtension]]
	  * @see [[net.noresttherein.sugar.collections.IArray.IArrayExtension]]
	  * @see [[net.noresttherein.sugar.collections.ArrayLike.ArrayLikeExtension]]
	  */
	@inline class DoubleIArrayExtension private[collections] (private val array :Array[Double]) extends AnyVal {
		@inline def length :Int = array.length
		@inline def head :Double = array(0)
		@inline def last :Double = array(array.length - 1)
		@inline def apply(n :Int) :Double = array(n)
		@inline def toArraySeq :ArraySeq.ofDouble = new ArraySeq.ofDouble(array)
	}

	/** Accessor extension methods for [[net.noresttherein.sugar.collections.IArray IArray]]`[Boolean]`.
	  *  Enabled by importing any of
	  * `net.noresttherein.sugar.collections.extensions.`[[net.noresttherein.sugar.collections.extensions.BooleanIArrayExtension BooleanIArrayExtension]],
	  * `net.noresttherein.sugar.collections.IArray.extensions.BooleanIArrayExtension`,
	  * or `net.noresttherein.sugar.extensions.BooleanIArrayExtension`.
	  * @see [[net.noresttherein.sugar.collections.IArray.GenericIArrayExtension]]
	  * @see [[net.noresttherein.sugar.collections.IArray.IArrayExtension]]
	  * @see [[net.noresttherein.sugar.collections.ArrayLike.ArrayLikeExtension]]
	  */
	@inline class BooleanIArrayExtension private[collections] (private val array :Array[Boolean]) extends AnyVal {
		@inline def length :Int = array.length
		@inline def head :Boolean = array(0)
		@inline def last :Boolean = array(array.length - 1)
		@inline def apply(n :Int) :Boolean = array(n)
		@inline def toArraySeq :ArraySeq.ofBoolean = new ArraySeq.ofBoolean(array)
	}

	/** Accessor extension methods for [[net.noresttherein.sugar.collections.IArray IArray]]`[T]`
	  * for some reference type `T <: AnyRef`. Enabled by importing any of
	  * `net.noresttherein.sugar.collections.extensions.`[[net.noresttherein.sugar.collections.extensions.RefIArrayExtension RefIArrayExtension]],
	  * `net.noresttherein.sugar.collections.IArray.extensions.RefIArrayExtension`,
	  * or `net.noresttherein.sugar.extensions.RefIArrayExtension`.
	  * @see [[net.noresttherein.sugar.collections.IArray.GenericIArrayExtension]]
	  * @see [[net.noresttherein.sugar.collections.IArray.IArrayExtension]]
	  * @see [[net.noresttherein.sugar.collections.ArrayLike.ArrayLikeExtension]]
	  */
	@inline class RefIArrayExtension[E <: AnyRef] private[collections] (private val array :Array[E]) extends AnyVal {
		@inline def length :Int = array.length
		@inline def head :E = array(0)
		@inline def last :E = array(array.length - 1)
		@inline def apply(n :Int) :E = array(n)
		@inline def toArraySeq :ArraySeq.ofRef[E] = new ArraySeq.ofRef(array)
	}
	
	/** Accessor extension methods for [[net.noresttherein.sugar.collections.IArray IArray]]`[T]` for some type `T`.
	  * Enabled by importing any of 
	  * `net.noresttherein.sugar.collections.extensions.`[[net.noresttherein.sugar.collections.extensions.GenericIArrayExtension GenericIArrayExtension]],
	  * `net.noresttherein.sugar.collections.IArray.extensions.GenericIArrayExtension`, 
	  * or `net.noresttherein.sugar.extensions.GenericIArrayExtension`.
	  * @see [[net.noresttherein.sugar.collections.IArray.IArrayExtension]]
	  * @see [[net.noresttherein.sugar.collections.ArrayLike.ArrayLikeExtension]]
	  * @tparam E the component type of the elements in the underlying array.
	  */
	class GenericIArrayExtension[E] private[collections] (private val array :Array[E]) extends AnyVal {
		@inline def head :E = array(0)
		@inline def last :E = array(array.length - 1)
		@inline def apply(n :Int) :E = array(n)
		@inline def toArraySeq :ArraySeq[E] = ArraySeq.unsafeWrapArray(array)
	}


	/** Extension methods specific to [[net.noresttherein.sugar.collections.IArray IArray]] only.
	  * Contains the subset of [[collection.ArrayOps ArrayOps]] methods which create a new array of a different type,
	  * requiring a [[scala.reflect.ClassTag ClassTag]] as an implicit parameter, and those which refer
	  * to the underlying `Array[E]` in a manner which could cause a `ClassCastException` for a
	  * [[net.noresttherein.sugar.collections.RefArray RefArray]]
	  * or a [[net.noresttherein.sugar.collections.IRefArray IRefArray]].
	  * Implicit conversion can be enabled by importing one of
	  * `net.noresttherein.sugar.collections.extensions.`[[net.noresttherein.sugar.collections.IArray.extensions.IArrayExtension IArrayExtension]],
	  * `net.noresttherein.sugar.collections.IArray.extensions.IArrayExtension`,
	  * or `net.noresttherein.sugar.extensions.IArrayExtension`.
	  * @see [[net.noresttherein.sugar.collections.ArrayLike.ArrayLikeExtension]]
	  * @see [[net.noresttherein.sugar.collections.IArray.GenericIArrayExtension]]
	  * @tparam E the component type of the elements in the underlying array.
	  */
	class IArrayExtension[E] private[collections] (private[IArray] val array :Array[E])
		extends AnyVal
	{
		@inline def indexOf(elem :E, from :Int = 0) :Int = array.indexOf(elem, from)
		@inline def lastIndexOf(elem :E, end :Int = Int.MaxValue) :Int = array.lastIndexOf(elem, end)
		@inline def contains(elem :E) :Boolean = array.contains(elem)

		@inline def collect[A :ClassTag](pf :PartialFunction[E, A]) :IArray[A] =
			array.collect(pf).castFrom[Array[A], IArray[A]]

		@inline def partitionMap[E1: ClassTag, E2: ClassTag](f: E => Either[E1, E2]) :(IArray[E1], IArray[E2]) =
			array.partitionMap(f).castFrom[(Array[E1], Array[E2]), (IArray[E1], IArray[E2])]

		@inline def withFilter(p :E => Boolean) :WithFilter[E] = new WithFilter(p, array)

		@inline def map[A :ClassTag](f :E => A) :IArray[A] = array.map(f).castFrom[Array[A], IArray[A]]
		@inline def flatMap[A :ClassTag](f :E => IterableOnce[A]) :IArray[A] =
			array.flatMap(f).castFrom[Array[A], IArray[A]]

		@inline def flatMap[As, A](f :E => As)(implicit asIterable :As => Iterable[A], m :ClassTag[A]) :IArray[A] =
			array.flatMap(f).castFrom[Array[A], IArray[A]]

		@inline def flatten[A :ClassTag](implicit asIterable :E => IterableOnce[A]) :IArray[A] =
			array.flatten.castFrom[Array[A], IArray[A]]

		@inline def groupMap[K, A :ClassTag](key :E => K)(f :E => A) :Map[K, IArray[A]] =
			array.groupMap(key)(f).castFrom[Map[K, Array[A]], Map[K, IArray[A]]]

		@inline def unzip[E1, E2](implicit asPair :E => (E1, E2), ct1 :ClassTag[E1], ct2 :ClassTag[E2])
				:(IArray[E1], IArray[E2]) =
			array.unzip.castFrom[(Array[E1], Array[E2]), (IArray[E1], IArray[E2])]

		@inline def unzip3[E1, E2, E3]
			              (implicit asTriple :E => (E1, E2, E3), ct1 :ClassTag[E1], ct2 :ClassTag[E2], ct3 :ClassTag[E3])
		        :(IArray[E1], IArray[E2], IArray[E3]) =
			array.unzip3.castFrom[(Array[E1], Array[E2], Array[E3]), (IArray[E1], IArray[E2], IArray[E3])]

		@inline def take(n :Int) :IArray[E] =
			if (n <= 0)
				if (array.isInstanceOf[Array[AnyRef]]) emptyObjectArray.castFrom[Array[AnyRef], IArray[E]]
				else empty(array.getClass.getComponentType.castParam[E])
			else if (n >= array.length) array.castFrom[Array[E], IArray[E]]
			else array.take(n).castFrom[Array[E], IArray[E]]

		@inline def drop(n :Int) :IArray[E] =
			if (n <= 0) array.castFrom[Array[E], IArray[E]]
			else if (n >= array.length)
				if (array.isInstanceOf[Array[AnyRef]]) emptyObjectArray.castFrom[Array[AnyRef], IArray[E]]
				else empty(array.getClass.getComponentType.castParam[E])
			else array.drop(n).castFrom[Array[E], IArray[E]]

		@inline def takeRight(n :Int) :IArray[E] =
			if (n <= 0)
				if (array.isInstanceOf[Array[AnyRef]]) emptyObjectArray.castFrom[Array[AnyRef], IArray[E]]
				else empty(array.getClass.getComponentType.castParam[E])
			else if (n >= array.length) array.castFrom[Array[E], IArray[E]]
			else array.drop(array.length - n).castFrom[Array[E], IArray[E]]

		@inline def dropRight(n :Int) :IArray[E] =
			if (n <= 0) array.castFrom[Array[E], IArray[E]]
			else if (n >= array.length)
				if (array.isInstanceOf[Array[AnyRef]]) emptyObjectArray.castFrom[Array[AnyRef], IArray[E]]
				else empty(array.getClass.getComponentType.castParam[E])
			else array.take(array.length - n).castFrom[Array[E], IArray[E]]

		@inline def takeWhile(p :E => Boolean) :IArray[E] = take(array.segmentLength(p.asInstanceOf[Any => Boolean]))
		@inline def dropWhile(p :E => Boolean) :IArray[E] = drop(array.segmentLength(p.asInstanceOf[Any => Boolean]))
		@inline def slice(from :Int, until :Int) :IArray[E] =
			if (until <= from | until < 0 || from >= array.length) 
				if (array.isInstanceOf[Array[AnyRef]]) emptyObjectArray.castFrom[Array[AnyRef], IArray[E]]
				else IArray.empty(array.getClass.getComponentType.castParam[E])
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

		@inline def updated[U >: E :ClassTag](index :Int, x :U) :IArray[U] =
			array.updated(index, x).castFrom[Array[U], IArray[U]]

		//we could reuse in this method this array if patch is out of range, but that would become inconsistent
		// with the class tag, and might cause problems for what's a very fringe case.
		/** An 'exploded' variant of `updatedAll`: a copy of this array with elements starting at `index` substituted
		  * by `first`, `second`, and contents of `rest`. The index must lie in range `[0..length - rest.size - 2)`.
		  */
		@inline def updatedAll[U >: E :ClassTag](index :Int, first :U, second :U, rest :U*) :IArray[U] =
			array.updatedAll(index, first, second, rest :_*).castFrom[Array[U], IArray[U]]

		/** Updates consecutive elements of this array, starting with the specified index,
		  * with elements from the collection. Equivalent to
		  * [[net.noresttherein.sugar.collections.IArray.IArrayExtension.patch patch]]`(index, elems, elems.size)`,
		  * but more efficient due to a single array allocation.
		  */
		@inline def updatedAll[U >: E :ClassTag](index :Int, elems :IterableOnce[U]) :IArray[U] =
			array.updatedAll(index, elems).castFrom[Array[U], IArray[U]]

		/** Updates consecutive elements of this array, starting with the specified index,
		  * with elements from the collection. Equivalent to
		  * [[net.noresttherein.sugar.collections.IArray.IArrayExtension.patch patch]]`(index, elems, elems.size)`,
		  * but more efficient due to a single array allocation.
		  */
		@inline def updatedAll[U >: E :ClassTag](index :Int, elems :ArrayLike[U]) :IArray[U] =
			array.updatedAll(index, elems).castFrom[Array[U], IArray[U]]

		/** Updates consecutive elements of this array, starting with the specified index,
		  * with elements from the collection. Equivalent to
		  * [[net.noresttherein.sugar.collections.IArray.IArrayExtension.patch patch]]`(index, elems, elems.size)`,
		  * but more efficient due to a single array allocation.
		  */
		@inline def updatedAll[U >: E](index :Int, elems :IArray[U]) :IArray[U] =
			array.updatedAll(index, elems.castFrom[IArray[U], Array[U]]).castFrom[Array[U], IArray[U]]


		/** A copy of this array, with the element inserted at a given position in this array,
		  * and all elements at positions equal or greater than `index` by one element further.
		  * This is equivalent to [[net.noresttherein.sugar.collections.IArray.IArrayExtension.patch patch]]
		  * with a singleton collection and `replaced` equal to zero, but the index must be in the valid range
		  * for this array.
		  * @return `take(index) :+ elem :++ drop(index)`, but in a more efficient manner.
		  * @throws IndexOutOfBoundsException if `index` < 0 or `index > length`.
		  */
		@inline def inserted[A >: E :ClassTag](index :Int, elem :A) :IArray[A] =
			array.inserted(index, elem).castFrom[Array[A], IArray[A]]

		/** An 'exploded' variant of `updatedAll`: a copy of this array with elements starting at `index` substituted
		  * by `first`, `second`, and contents of `rest`. The index must lie in range `[0..length)`.
		  */
		@inline def insertedAll[A >: E :ClassTag](index :Int, first :A, second :A, rest :A*) :IArray[A] =
			array.insertedAll(index, first, second, rest :_*).castFrom[Array[A], IArray[A]]

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * @return [[net.noresttherein.sugar.collections.IArray.IArrayExtension.patch patch]]`(index, elems, 0)`.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		@inline def insertedAll[A >: E :ClassTag](index :Int, elems :IterableOnce[A]) :IArray[A] =
			array.insertedAll(index, elems).castFrom[Array[A], IArray[A]]

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * @return [[net.noresttherein.sugar.collections.IArray.IArrayExtension.patch patch]]`(index, elems, 0)`.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		@inline def insertedAll[A >: E :ClassTag](index :Int, elems :ArrayLike[A]) :IArray[A] =
			array.insertedAll(index, elems).castFrom[Array[A], IArray[A]]

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * @return [[net.noresttherein.sugar.collections.IArray.IArrayExtension.patch patch]]`(index, elems, 0)`.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		@inline def insertedAll[A >: E](index :Int, elems :IArray[A]) :IArray[A] =
			array.insertedAll(index, elems.castFrom[IArray[A], Array[A]]).castFrom[Array[A], IArray[A]]


		@inline def :+[A >: E :ClassTag](x :A) :IArray[A] = array.appended(x).castFrom[Array[A], IArray[A]]
		@inline def +:[A >: E :ClassTag](x :A) :IArray[A] = array.prepended(x).castFrom[Array[A], IArray[A]]
		@inline def appended[A >: E :ClassTag](x :A) :IArray[A] = array.appended(x).castFrom[Array[A], IArray[A]]
		@inline def appendedAll[A >: E :ClassTag](first :A, second :A, rest :A*) :IArray[A] =
			array.appendedAll(first, second, rest :_*).castFrom[Array[A], IArray[A]]

		@inline def prepended[A >: E :ClassTag](x :A) :IArray[A] = array.prepended(x).castFrom[Array[A], IArray[A]]

		@inline def prependedAll[A >: E :ClassTag](first :A, second :A, rest :A*) :IArray[A] =
			array.prependedAll(first, second, rest :_*).castFrom[Array[A], IArray[A]]

		@inline def ++[A >: E :ClassTag](suffix :IterableOnce[A]) :IArray[A] =
			array.appendedAll(suffix).castFrom[Array[A], IArray[A]]

		@inline def ++[A >: E :ClassTag](suffix :ArrayLike[A]) :IArray[A] =
			array.appendedAll(suffix).castFrom[Array[A], IArray[A]]

		@inline def ++[A >: E](suffix :IArray[A]) :IArray[A] =
			array.appendedAll(suffix.castFrom[IArray[A], Array[A]]).castFrom[Array[A], IArray[A]]

		@inline def concat[A >: E :ClassTag](suffix :IterableOnce[A]) :IArray[A] =
			array.appendedAll(suffix).castFrom[Array[A], IArray[A]]

		@inline def concat[A >: E :ClassTag](suffix :ArrayLike[A]) :IArray[A] =
			array.appendedAll(suffix).castFrom[Array[A], IArray[A]]

		@inline def concat[A >: E](suffix :IArray[A]) :IArray[A] =
			array.appendedAll(suffix.asInstanceOf[Array[A]]).castFrom[Array[A], IArray[A]]

		@inline def :++[A >: E :ClassTag](suffix :IterableOnce[A]) :IArray[A] =
			array.appendedAll(suffix).castFrom[Array[A], IArray[A]]

		@inline def :++[A >: E :ClassTag](suffix :ArrayLike[A]) :IArray[A] =
			array.appendedAll(suffix).castFrom[Array[A], IArray[A]]

		@inline def :++[A >: E](suffix :IArray[A]) :IArray[A] =
			array.appendedAll(suffix.castFrom[IArray[A], Array[A]]).castFrom[Array[A], IArray[A]]

		@inline def appendedAll[A >: E :ClassTag](suffix :IterableOnce[A]) :IArray[A] =
			array.appendedAll(suffix).castFrom[Array[A], IArray[A]]

		@inline def appendedAll[A >: E :ClassTag](suffix :ArrayLike[A]) :IArray[A] =
			array.appendedAll(suffix).castFrom[Array[A], IArray[A]]

		@inline def appendedAll[A >: E](suffix :IArray[A]) :IArray[A] =
			array.appendedAll(suffix.castFrom[IArray[A], Array[A]]).castFrom[Array[A], IArray[A]]


		@inline def ++:[A >: E :ClassTag](suffix :IterableOnce[A]) :IArray[A] =
			array.prependedAll(suffix).castFrom[Array[A], IArray[A]]

		@inline def ++:[A >: E :ClassTag](suffix :ArrayLike[A]) :IArray[A] =
			array.prependedAll(suffix).castFrom[Array[A], IArray[A]]

		@inline def ++:[A >: E](suffix :IArray[A]) :IArray[A] =
			array.prependedAll(suffix.castFrom[IArray[A], Array[A]]).castFrom[Array[A], IArray[A]]

		@inline def prependedAll[A >: E :ClassTag](suffix :IterableOnce[A]) :IArray[A] =
			array.prependedAll(suffix).castFrom[Array[A], IArray[A]]

		@inline def prependedAll[A >: E :ClassTag](suffix :ArrayLike[A]) :IArray[A] =
			array.prependedAll(suffix).castFrom[Array[A], IArray[A]]

		@inline def prependedAll[A >: E](suffix :IArray[A]) :IArray[A] =
			array.prependedAll(suffix.castFrom[IArray[A], Array[A]]).castFrom[Array[A], IArray[A]]

		@inline def patch[A >: E :ClassTag](from :Int, elems :IterableOnce[A], replaced :Int) :IArray[A] =
			array.patch(from, elems, replaced).castFrom[Array[A], IArray[A]]

		@inline def patch[A >: E :ClassTag](from :Int, elems :ArrayLike[A], replaced :Int) :IArray[A] =
			array.patch(from, elems, replaced).castFrom[Array[A], IArray[A]]

		@inline def patch[A >: E](from :Int, elems :IArray[A], replaced :Int) :IArray[A] =
			array.patch(from, elems.castFrom[IArray[A], Array[A]], replaced).castFrom[Array[A], IArray[A]]

		def toOps :IndexedSeqOps[E, IRefArray, IArray[E]] = new IArrayAsSeq[E](array.castFrom[Array[E], IArray[E]])

		@inline def toSeq :Seq[E] = Wrapped(array.castFrom[Array[E], IArray[E]])
		@inline def toIndexedSeq :IndexedSeq[E] = Wrapped(array.castFrom[Array[E], IArray[E]])
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




	/** Creates a new `IArray[E]` of the specified length, executes the given initialization function for it,
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
	@inline final def init[E :ClassTag](length :Int)(f :Array[E] => Unit) :IArray[E] = {
		val res = new Array[E](length)
		f(res)
		res.castFrom[Array[E], IArray[E]]
	}

	/** Creates an `IArray[E]` of the specified element type and length, executes the given initialization function
	  * for it, and returns it as an `IArray[E]`.
	  * @see [[net.noresttherein.sugar.collections.IArray.init]]
	  */
	@inline final def make[E](elemType :Class[E], length :Int)(f :Array[E] => Unit) :IArray[E] = {
		val res = Array.of(elemType, length)
		f(res)
	    res.castFrom[Array[E], IArray[E]]
	}

	/** Creates a new `IArray[E]` of the specified length and of the same type as the argument,
	  * executes the given initialization function for it, and returns it as an `IArray[E]`.
	  * @see [[net.noresttherein.sugar.collections.IArray.init]]
	  */
	@inline final def like[E](array :Array[E], length :Int)(f :Array[E] => Unit) :IArray[E] =
		make(array.getClass.getComponentType.castParam[E], length)(f)


	/** Creates a copy of the given array, possibly changing its element type. */
	@inline def copyAs[E :ClassTag](array :ArrayLike[_]) :IArray[E] = copyAs(array, array.length)

	/** Copies one array to another, truncating or padding with default values (if necessary),
	  * so the copy has the specified length. The new array can have a different type than the original one,
	  * as long as the values are assignment-compatible. When copying between primitive and object arrays,
	  * boxing and unboxing are supported.
	  */
	def copyAs[E :ClassTag](array :ArrayLike[_], newLength :Int) :IArray[E] =
		ArrayAsSeq.copyAs[E](array, newLength).castFrom[Array[E], IArray[E]]


	/** Creates a new immutable array with the element type specified by `ClassTag[E]`
	  * and the same contents as the argument.
	  */
	@inline def copyOf[E :ClassTag](array :ArrayLike[E]) :IArray[E] = copyOf(array, array.length)
//		ArrayAsSeq.copyAs[E](array, array.length).castFrom[Array[E], IArray[E]]

	/** Copies one array to another, truncating or padding with default values (if necessary) so the copy has
	  * the specified length. The returned array will be use the element type specified by the `ClassTag[E]`.
	  * Equivalent to `Array.`[[Array.copyAs copyAs]]`(array, newLength)`, but accepts any `ArrayLike` and returns
	  * the result as an $Coll.
	  */
	@inline def copyOf[E :ClassTag](array :ArrayLike[E], newLength :Int) :IArray[E] = copyAs(array, newLength)
//		ArrayAsSeq.copyOf[E](array, newLength).castFrom[Array[E], IArray[E]]

//	/** Reallocates the given array as a $Coll`[E]` of a new size, and copies `min(newLength - offset, array.length)`
//	  * of its first elements to positions starting with `offset`.
//	  */
//	def copyOf[E :ClassTag](array :ArrayLike[E], offset :Int, newLength :Int) :IArray[E] =
//		ArrayAsSeq.copyAs[E](array, offset, newLength).castFrom[Array[E], IArray[E]]

//	/** Creates a new immutable array with the same element type as the given array, and copies the contents. */
//	@inline def copyOf[E](array :IArray[E]) :IArray[E] =
//		if (array.length == 0) array
//		else Array.copyOf(array.castFrom[IArray[E], Array[E]], array.length).castFrom[Array[E], IArray[E]]

	/** Copies one array to another, truncating or padding with default values (if necessary) so the copy has
	  * the specified length. The returned array will be of the same type as the original.
	  * Equivalent to `Array.`[[Array.copyOf copyOf]]`(array, newLength)`, but accepts any `ArrayLike` and returns
	  * the result as an $Coll.
	  */
	def copyOf[E](array :IArray[E], newLength :Int) :IArray[E] =
		ArrayAsSeq.copyOf(array.castFrom[IArray[E], Array[E]], newLength).castFrom[Array[E], IArray[E]]

//	/** Reallocates the given array as a $Coll`[E]` of a new size, and copies `min(newLength - offset, array.length)`
//	  * of its first elements to positions starting with `offset`.
//	  */
//	def copyOf[E](array :IArray[E], offset :Int, newLength :Int) :IArray[E] =
//		if (newLength == 0)
//			ArrayAsSeq.empty(array.getClass.getComponentType.castParam[E]).castFrom[Array[E], IArray[E]]
//		else {
//			if (offset < 0 | offset > newLength)
//				throw new IndexOutOfBoundsException(
//					toString + ".copyOf(" + array.className + "|" + array.length + "|, " + offset + ", " + newLength + ")"
//				)
//			val res = Array.make(array.getClass.getComponentType.castParam[E], newLength)
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
	def copyOfRange[E](array :ArrayLike[E], from :Int, until :Int) :IArray[E] =
		if (until <= from | until <= 0)
			empty(array.getClass.getComponentType.castParam[E])
		else
			((array :Any) : @unchecked) match {
				case a :Array[AnyRef]   => java.util.Arrays.copyOfRange(a, from, until).castFrom[Array[AnyRef], IArray[E]]
				case a :Array[Int]      => java.util.Arrays.copyOfRange(a, from, until).castFrom[Array[Int], IArray[E]]
				case a :Array[Long]     => java.util.Arrays.copyOfRange(a, from, until).castFrom[Array[Long], IArray[E]]
				case a :Array[Double]   => java.util.Arrays.copyOfRange(a, from, until).castFrom[Array[Double], IArray[E]]
				case a :Array[Byte]     => java.util.Arrays.copyOfRange(a, from, until).castFrom[Array[Byte], IArray[E]]
				case a :Array[Char]     => java.util.Arrays.copyOfRange(a, from, until).castFrom[Array[Char], IArray[E]]
				case a :Array[Float]    => java.util.Arrays.copyOfRange(a, from, until).castFrom[Array[Float], IArray[E]]
				case a :Array[Short]    => java.util.Arrays.copyOfRange(a, from, until).castFrom[Array[Short], IArray[E]]
				case a :Array[Boolean]  => java.util.Arrays.copyOfRange(a, from, until).castFrom[Array[Boolean], IArray[E]]
				case _ :Array[Unit]     => new Array[Unit](until - from).castFrom[Array[Unit], IArray[E]]
			}

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
	  *         as a `IArray[E]`, with the copied slices.
	  */
	def copyOfRanges[E :ClassTag](array1 :ArrayLike[E], from1 :Int, until1 :Int,
	                              array2 :ArrayLike[E], from2 :Int, until2 :Int) :IArray[E] =
		if (from1 < 0 | from2 < 0 || from1 > array1.length || from2 > array2.length)
			throw new IndexOutOfBoundsException(
				s"IArray.copyOfRanges(${array1.className}|${array1.length}|, $from1, $until1, " +
					s"${array2.className}|${array2.length}|, $from2, $until2)."
			)
		else {
			val length1 = math.min(from1, until1) - from1
			val length2 = math.min(from2, until2) - from2
			if (length1 + length2 == 0)
				empty[E]
			else {
				val res     = new Array[E](length1 + length2)
				ArrayLike.copy(array1, from1, res, 0, length1)
				ArrayLike.copy(array2, from2, res, length1, length2)
				res.castFrom[Array[E], IArray[E]]
			}
		}

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
	  *         as a `IArray[E]`, with the copied slices.
	  */
	def copyOfRanges[E :ClassTag](array1 :ArrayLike[E], from1 :Int, until1 :Int,
	                              array2 :ArrayLike[E], from2 :Int, until2 :Int,
	                              array3 :ArrayLike[E], from3 :Int, until3 :Int) :IArray[E] =
		if (from1 < 0 | from2 < 0 | from3 < 0 || from1 > array1.length || from2 > array2.length || from3 > array3.length)
			throw new IndexOutOfBoundsException(
				s"IArray.copyOfRanges(${array1.localClassName}|${array1.length}|, $from1, $until1, " +
					s"${array2.localClassName}|${array2.length}|, $from2, $until2, " +
					s"${array3.localClassName}|${array3.length}|, $from3, $until3)."
			)
		else {
			val length1 = math.min(from1, until1) - from1
			val length2 = math.min(from2, until2) - from2
			val length3 = math.min(from3, until3) - from2
			if (length1 == 0 & length2 == 0 & length3 == 0)
				empty[E]
			else {
				val res     = new Array[E](length1 + length2 + length3)
				ArrayLike.copy(array1, from1, res, 0, length1)
				ArrayLike.copy(array2, from2, res, length1, length2)
				ArrayLike.copy(array3, from3, res, length1 + length2, length3)
				res.castFrom[Array[E], IArray[E]]
			}
		}


	override def from[E :ClassTag](it :IterableOnce[E]) :IArray[E] = it match {
		case elems :View[E]                      => from(elems.iterator)
		case Wrapped(array)                      => array
		case elems :Iterable[E] if elems.isEmpty => empty[E]
		case iter  :Iterator[E] if !iter.hasNext => empty[E]
		case elems :Iterable[E]                  => elems.toArray[E].castFrom[Array[E], IArray[E]]
		case _                                   => it.iterator.toArray[E].castFrom[Array[E], IArray[E]]
	}

	@inline def from[E :ClassTag](array :ArrayLike[E]) :IArray[E] = copyOf(array)

	val emptyBooleanIArray :IArray[Boolean] = new Array[Boolean](0).castFrom[Array[Boolean], IArray[Boolean]]
	val emptyByteIArray    :IArray[Byte]    = new Array[Byte](0).castFrom[Array[Byte], IArray[Byte]]
	val emptyCharIArray    :IArray[Char]    = new Array[Char](0).castFrom[Array[Char], IArray[Char]]
	val emptyDoubleIArray  :IArray[Double]  = new Array[Double](0).castFrom[Array[Double], IArray[Double]]
	val emptyFloatIArray   :IArray[Float]   = new Array[Float](0).castFrom[Array[Float], IArray[Float]]
	val emptyIntIArray     :IArray[Int]     = new Array[Int](0).castFrom[Array[Int], IArray[Int]]
	val emptyLongIArray    :IArray[Long]    = new Array[Long](0).castFrom[Array[Long], IArray[Long]]
	val emptyShortIArray   :IArray[Short]   = new Array[Short](0).castFrom[Array[Short], IArray[Short]]
	val emptyObjectIArray  :IArray[AnyRef]  = new Array[AnyRef](0).castFrom[Array[AnyRef], IArray[AnyRef]]
	val emptyUnitIArray    :IArray[Unit]    = new Array[Unit](0).castFrom[Array[Unit], IArray[Unit]]
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
		ArrayAsSeq.newBuilder[E].castParam2[IArray[E]]

	/** Builds an `IArray` of the specified element type. */
	def newBuilder[E](elementType :Class[E]) :Builder[E, IArray[E]] =
		ArrayAsSeq.newBuilder(elementType).castParam2[IArray[E]]

	
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
	@inline final def iterateWithIndex[X :ClassTag](start :X, len :Int)(f :(X, Int) => X) :IArray[X] =
		Array.iterateWithIndex(start, len)(f).castFrom[Array[X], IArray[X]]

	/** A stepper iterating over the range of an array. Indices out of range are skipped silently. */
	@inline final def stepper[A, S <: Stepper[_]]
	                         (array :IArray[A], from :Int = 0, until :Int = Int.MaxValue)
	                         (implicit shape :StepperShape[A, S]) :S with EfficientSplit =
		Array.stepper(array.castFrom[IArray[A], Array[A]], from, until)



	def unapplySeq[E](array :IArray[E]) :UnapplySeqWrapper[E] =
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



	//todo: specialize
	implicit def IArrayOrdering[E :Ordering] :Ordering[IArray[E]] =
		Array.ArrayOrdering[E].castFrom[Ordering[Array[E]], Ordering[IArray[E]]]

	implicit def IArrayClassTag[E :ClassTag] :ClassTag[IArray[E]] = classTag[Array[E]].castParam[IArray[E]]

	implicit def IArrayToSeq[E](array :IArray[E]) :IndexedSeq[E] = Wrapped(array)

	/** A type class promoting immutable arrays to sequences. */
	implicit def IArrayIsSeq[E] :IsSeq[IArray[E]] { type A = E; type C = IArray[E] } =
		Array.ArrayIsSeq[E].castFrom[IsSeq[Array[E]], IsSeq[IArray[E]] { type A = E; type C = IArray[E] }]


	/** Wraps and unwraps immutable `IndexedSeq` instances and other immutable collections backed by arrays
	  * in a safe manner. Arrays are represented as [[net.noresttherein.sugar.collections.IArray IArray]] instances
	  * to prevent accidental modification and ensure that the user is aware that an array will be represented
	  * as an immutable structure. Additionally, extractor relies on a `ClassTag` to verify
	  * that an array is of a compatible element type to avoid `ClassCastException`s on later access.
	  */
	object Wrapped {
		private[this] val wrapper = PassedArrayWrapper getOrElse IArraySlice

		def apply[A](array :IArray[A]) :IndexedSeq[A] = wrapper.of(array)

		def unapply[A :ClassTag](items :IterableOnce[A]) :Opt[IArray[A]] = {
			val array = items match {
				case slice :AbstractArraySlice[_] if slice.isImmutable && slice.knownSize == slice.unsafeArray.length =>
					Got(slice.unsafeArray)
				case seq :ArraySeq[_] => Got(seq.unsafeArray)
				case _                => Lack
			}
			if (array.isDefined && array.get.getClass.getComponentType <:< classTag[A].runtimeClass)
				array.castFrom[Opt[Array[_]], Opt[IArray[A]]]
			else
				Lack
		}

		/** Factory of views on slices of immutable arrays as indexed sequences,
		  * and unwraps known immutable collections backed by array slices.
		  */
		object Slice {
			private[this] val wrapper = PassedArrayWrapper getOrElse IArraySlice

			def apply[A](array :IArray[A], from :Int, until :Int) :IndexedSeq[A] = wrapper.of(array, from, until)

			def unapply[A :ClassTag](items :IterableOnce[A]) :Opt[(IArray[A], Int, Int)] = {
				val expectedClass = classTag[A].runtimeClass
				items match {
					case seq :ArraySeq[_] if seq.unsafeArray.getClass.getComponentType <:< expectedClass =>
						Got((seq.unsafeArray.castFrom[Array[_], IArray[A]], 0, seq.unsafeArray.length))

					case slice :AbstractArraySlice[A] if items.knownSize >= 0 && slice.isImmutable =>
						val array = slice.unsafeArray.castFrom[Array[_], IArray[A]]
						if (array.getClass.getComponentType <:< expectedClass)
							Got((array, slice.startIndex, slice.knownSize))
						else
							Lack
					case _ =>
						Lack
				}
			}
		}
	}



	private[collections] sealed trait IArrayRank2Implicits extends Any {
		@inline implicit final def immutableArrayToSeq[E](array :IArray[E]) :IndexedSeq[E] = IArray.Wrapped(array)
	}

	/** Conversions of `IArray` of various element types to their dedicated
	  * [[collection.immutable.ArraySeq ArraySeq]].
	  */
	private[collections] sealed trait IArrayRank1Implicits extends Any with IArrayRank2Implicits {
		@inline implicit final def byteIArrayToSeq(array :IArray[Byte]) :ArraySeq.ofByte =
			new ArraySeq.ofByte(array.castFrom[IArray[Byte], Array[Byte]])

		@inline implicit final def shortIArrayToSeq(array :IArray[Short]) :ArraySeq.ofShort =
			new ArraySeq.ofShort(array.castFrom[IArray[Short], Array[Short]])

		@inline implicit final def charIArrayToSeq(array :IArray[Char]) :ArraySeq.ofChar =
			new ArraySeq.ofChar(array.castFrom[IArray[Char], Array[Char]])

		@inline implicit final def intIArrayToSeq(array :IArray[Int]) :ArraySeq.ofInt =
			new ArraySeq.ofInt(array.castFrom[IArray[Int], Array[Int]])

		@inline implicit final def longIArrayToSeq(array :IArray[Long]) :ArraySeq.ofLong =
			new ArraySeq.ofLong(array.castFrom[IArray[Long], Array[Long]])

		@inline implicit final def floatIArrayToSeq(array :IArray[Float]) :ArraySeq.ofFloat =
			new ArraySeq.ofFloat(array.castFrom[IArray[Float], Array[Float]])

		@inline implicit final def doubleIArrayToSeq(array :IArray[Double]) :ArraySeq.ofDouble =
			new ArraySeq.ofDouble(array.castFrom[IArray[Double], Array[Double]])

		@inline implicit final def booleanIArrayToSeq(array :IArray[Boolean]) :ArraySeq.ofBoolean =
			new ArraySeq.ofBoolean(array.castFrom[IArray[Boolean], Array[Boolean]])

		@inline implicit final def refIArrayToSeq[E <: AnyRef](array :IArray[E]) :ArraySeq.ofRef[E] =
			new ArraySeq.ofRef(array.castFrom[IArray[E], Array[E]])
	}

	private[collections] sealed trait IArrayRank3Extensions
		extends Any with IArrayRank1Implicits with ArrayLike.extensions
	{
		/** Adds various additional folding methods with a break condition to any `IArray`. */
		@inline implicit final def IArrayAsIterableOnceExtension[E](self :IArray[E]) :IterableOnceExtension[E] =
			new IterableOnceExtension[E](new IArrayAsSeq(self))

		/** Adds various methods for mapping/flatMapping collections to any `IArray`.
		  * These either pass along additional state, or have a break condition. Roughly equivalent to working
		  * with `toLazyList.scan`, but cleaner and more efficient.
		  */
		@inline implicit final def IArrayAsIterableExtension[E](self :IArray[E])
				:IterableExtension[E, IRefArray, IArray[E]] =
			new IterableExtension[E, IRefArray, IArray[E]](new IArrayAsSeq(self))

		/** Alternative, safer implementations of [[scala.collection.SeqOps.indexOf indexOf]] for immutable arrays,
		  * which do not return a negative index when the element is not found.
		  */
	   @inline implicit final def IArrayAsSeqExtension[E](self :IArray[E]) :SeqExtension[E, IRefArray, IArray[E]] =
			new SeqExtension[E, IRefArray, IArray[E]](new IArrayAsSeq(self))

		/** Operations on suffixes of a sequence and binary search methods on sorted immutable arrays. */
		@inline implicit final def IArrayAsIndexedSeqExtension[E](self :IArray[E])
				:IndexedSeqExtension[E, IRefArray, IArray[E]] =
			new IndexedSeqExtension[E, IRefArray, IArray[E]](new IArrayAsSeq(self))
	}

	private[collections] sealed trait IArrayRank2Extensions extends Any with IArrayRank3Extensions {
		@inline implicit final def GenericIArrayExtension[E](array :IArray[E]) :GenericIArrayExtension[E] =
			new GenericIArrayExtension[E](array.castFrom[IArray[E], Array[E]])
	}

	private[collections] sealed trait IArrayRank1Extensions extends Any with IArrayRank2Extensions {
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

		@inline implicit final def IArrayAsArrayLikeExtension[E](array :IArray[E])
				:ArrayLike.ArrayLikeExtension[E, IArray] =
			new ArrayLike.ArrayLikeExtension(array.castFrom[IArray[E], Array[_]])
	}

	/** Conversions providing extension methods for `IArray`. */
	private[collections] trait extensions extends Any with IArrayRank1Extensions {

		/** Adapts (casts) a `ClassTag` for an `Array[E]` to an `IArray[E]`. */
		@inline implicit final def IArrayClassTag[E](implicit tag :ClassTag[Array[E]]) :ClassTag[IArray[E]] =
			tag.castParam[IArray[E]]

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
			
		@inline implicit final def IArrayExtension[E](array :IArray[E]) :IArrayExtension[E] =
			new IArrayExtension(array.castFrom[IArray[E], Array[E]])

	}


	/** Conversions providing extension methods for `IArray`. */
	@SerialVersionUID(Ver)
	object extensions extends extensions
}






object RefArrayLike {
	/** Extension methods for [[net.noresttherein.sugar.collections.ArrayLike ArrayLike]]`[E]` implementations
	  * backed by an erased array (`Object[]`), boxing value types. These consist of
	  * [[net.noresttherein.sugar.collections.RefArray RefArray]] and
	  * [[net.noresttherein.sugar.collections.IRefArray IRefArray]]. Contains methods creating a new `Arr[_]`, which,
	  * unlike those in [[net.noresttherein.sugar.collections.IArray.IArrayExtension]]
	  * and standard [[collection.ArrayOps ArrayOps]] do not require a [[scala.reflect.ClassTag ClassTag]],
	  * but can be implemented safely when the adapted array is an `Array[Any]`.
	  * Can be enabled by importing
	  * `net.noresttherein.sugar.collections.extensions.`[[net.noresttherein.sugar.collections.RefArray.extensions.RefArrayAsRefArrayLikeExtension RefArrayAsRefArrayLikeExtension]]
	  * (for `RefArray`), and
	  * `net.noresttherein.sugar.collections.extensions.`[[net.noresttherein.sugar.collections.IRefArray.extensions.IRefArrayAsRefArrayLikeExtension IRefArrayAsRefArrayLikeExtension]]
	  * (for `IRefArray`). These same conversions are available under
	  * `RefArray.extensions`/`IRefArrayExtensions.extensions` and in object
	  * `net.noresttherein.sugar.extensions`.
	  * @see [[net.noresttherein.sugar.collections.ArrayLike.ArrayLikeExtension]] - additional extension methods
	  * @see [[net.noresttherein.sugar.collections.IArray.IArrayExtension]] - the same method subset for
	  *      [[net.noresttherein.sugar.collections.IArray IArray]].
	  * @tparam E   the type of the elements in the array (but not the component type of the underlying array!).
	  * @tparam Arr the type constructor of an `ArrayLike` subtype returned by methods which return
	  *             an `Array` in [[collection.ArrayOps ArrayOps]] (and the `ArrayLike` subtype being granted
	  *             these methods).
	  */
	class RefArrayLikeExtension[E, Arr[_]] private[collections] (private val array :Array[Any]) extends AnyVal {
		@inline def length :Int = array.length
		@inline def apply(i :Int) :E = array(i).asInstanceOf[E]
		@inline def indexOf(elem :E, from :Int = 0) :Int = array.indexOf(elem, from)
		@inline def lastIndexOf(elem :E, end :Int = Int.MaxValue) :Int = array.lastIndexOf(elem, end)
		@inline def contains(elem :E) :Boolean = array.contains(elem)

		@inline def scanLeft[A](z :A)(op :(A, E) => A) :Arr[A] =
			array.scanLeft[Any](z)(op.castParams[Any, Any, Any]).castFrom[Array[Any], Arr[A]]

		@inline def scanRight[A](z :A)(op :(E, A) => A) :Arr[A] =
			array.scanRight[Any](z)(op.castParams[Any, Any, Any]).castFrom[Array[Any], Arr[A]]

		@inline def scan[A >: E](z :A)(op :(A, A) => A) :Arr[A] =
			array.scan[Any](z)(op.castParams[Any, Any, Any]).castFrom[Array[Any], Arr[A]]

		@inline def collect[A](pf :PartialFunction[E, A]) :Arr[A] =
			array.collect(pf.castParams[Any, Any]).castFrom[Array[Any], Arr[A]]

		@inline def partitionMap[E1, E2](f: E => Either[E1, E2]) :(Arr[E1], Arr[E2]) =
			array.partitionMap(f.castParams[Any, Either[Any, Any]]).castFrom[(Array[Any], Array[Any]), (Arr[E1], Arr[E2])]

		@inline def map[A](f :E => A) :Arr[A] = array.map(f.castParams[Any, Any]).castFrom[Array[Any], Arr[A]]
		@inline def flatMap[A](f :E => IterableOnce[A]) :Arr[A] =
			array.flatMap(f.castParams[Any, IterableOnce[Any]]).castFrom[Array[Any], Arr[A]]

		@inline def flatMap[As, A](f :E => As)(implicit asIterable :As => Iterable[A]) :Arr[A] =
			array.flatMap(f.castParams[Any, Iterable[Any]]).castFrom[Array[Any], Arr[A]]

		@inline def flatten[A](implicit asIterable :E => IterableOnce[A]) :Arr[A] =
			array.flatten(asIterable.castParams[Any, IterableOnce[Any]], ClassTag.Any).castFrom[Array[Any], Arr[A]]

		@inline def groupMap[K, A](key :E => K)(f :E => A) :Map[K, Arr[A]] =
			array.groupMap[K, Any](key.castParams[Any, K])(f.castParams[Any, Any])
			     .castFrom[Map[K, Array[Any]], Map[K, Arr[A]]]


		@inline def unzip[E1, E2](implicit asPair :E => (E1, E2)) :(Arr[E1], Arr[E2]) =
			array.unzip(asPair.castParams[Any, (Any, Any)], ClassTag.Any, ClassTag.Any)
			     .castFrom[(Array[Any], Array[Any]), (Arr[E1], Arr[E2])]

		@inline def unzip3[E1, E2, E3](implicit asTriple :E => (E1, E2, E3)) :(Arr[E1], Arr[E2], Arr[E3]) =
			array.unzip3(asTriple.castParams[Any, (Any, Any, Any)], ClassTag.Any, ClassTag.Any, ClassTag.Any)
			     .castFrom[(Array[Any], Array[Any], Array[Any]), (Arr[E1], Arr[E2], Arr[E3])]

		@inline def updated[A >: E](index :Int, elem :A) :Arr[A] = array.updated(index, elem).castFrom[Array[Any], Arr[A]]

		/** An 'exploded' variant of `updatedAll`: a copy of this array with elements starting at `index` substituted
		  * by `first`, `second`, and contents of `rest`. The index must lie in range `[0..length - rest.size - 2)`.
		  */
		@inline def updatedAll[A >: E](index :Int, first :A, second :A, rest :A*) :Arr[A] =
			array.updatedAll(index, first, second, rest :_*).castFrom[Array[Any], Arr[A]]

		/** Updates consecutive elements of this array, starting with the specified index,
		  * with elements from the collection. Equivalent to
		  * [[net.noresttherein.sugar.collections.IArray.IArrayExtension.patch patch]]`(index, elems, elems.size)`,
		  * but more efficient due to a single array allocation.
		  */
		@inline def updatedAll[A >: E](index :Int, elems :IterableOnce[A]) :Arr[A] =
			array.updatedAll(index, elems).castFrom[Array[Any], Arr[A]]

		/** Updates consecutive elements of this array, starting with the specified index,
		  * with elements from the collection. Equivalent to
		  * [[net.noresttherein.sugar.collections.IArray.IArrayExtension.patch patch]]`(index, elems, elems.size)`,
		  * but more efficient due to a single array allocation.
		  */
		@inline def updatedAll[A >: E](index :Int, elems :ArrayLike[A]) :Arr[A] =
			array.updatedAll(index, elems).castFrom[Array[Any], Arr[A]]

		/** A copy of this array, with the element inserted at a given position in this array,
		  * and all elements at positions equal or greater than `index` by one element further.
		  * This is equivalent to [[net.noresttherein.sugar.collections.ArrayLike.RefArrayLikeExtension.patch patch]]
		  * with a singleton collection and `replaced` equal to zero, but the index
		  * must be in the valid range for this array.
		  * @return `take(index) :+ elem :++ drop(index)`, but in a more efficient manner.
		  * @throws IndexOutOfBoundsException if `index` < 0 or `index > length`.
		  */
		@inline def inserted[A >: E](index :Int, elem :A) :Arr[A] =
			array.inserted(index, elem)(ClassTag.Any).castFrom[Array[Any], Arr[A]]

		/** An 'exploded' variant of `updatedAll`: a copy of this array with elements starting at `index` substituted
		  * by `first`, `second`, and contents of `rest`. The index must lie in range `[0..length)`.
		  */
		@inline def insertedAll[A >: E](index :Int, first :A, second :A, rest :A*) :Arr[A] =
			array.insertedAll(index, first, second, rest :_*).castFrom[Array[Any], Arr[A]]

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * @return [[net.noresttherein.sugar.collections.ArrayLike.RefArrayLikeExtension.patch patch]]`(index, elems, 0)`.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		@inline def insertedAll[A >: E](index :Int, elems :IterableOnce[A]) :Arr[A] =
			array.insertedAll(index, elems)(ClassTag.Any).castFrom[Array[Any], Arr[A]]

		/** A copy with this array with `elems` inserted starting with position `index`, followed by `this(index)`
		  * and all subsequent elements.
		  * @return [[net.noresttherein.sugar.collections.ArrayLike.RefArrayLikeExtension.patch patch]]`(index, elems, 0)`.
		  * @throws IndexOutOfBoundsException if `index < 0` or `index > length`.
		  */
		@inline def insertedAll[A >: E](index :Int, elems :ArrayLike[A]) :Arr[A] =
			array.insertedAll(index, elems)(ClassTag.Any).castFrom[Array[Any], Arr[A]]

		@inline def :+[A >: E](x :A) :Arr[A] = array.appended(x)(ClassTag.Any).castFrom[Array[Any], Arr[A]]
		@inline def +:[A >: E](x :A) :Arr[A] = array.prepended(x)(ClassTag.Any).castFrom[Array[Any], Arr[A]]

		@inline def appended[A >: E](x :A) :Arr[A] = array.appended(x)(ClassTag.Any).castFrom[Array[Any], Arr[A]]
		@inline def prepended[A >: E](x :A) :Arr[A] = array.prepended(x)(ClassTag.Any).castFrom[Array[Any], Arr[A]]

		@inline def appendedAll[A >: E](first :A, second :A, rest :A*) :Arr[A] =
			array.appendedAll(first, second,  rest :_*)(ClassTag.Any).castFrom[Array[Any], Arr[A]]

		@inline def prependedAll[A >: E](first :A, second :A, rest :A*) :Arr[A] =
			array.prependedAll(first, second, rest :_*)(ClassTag.Any).castFrom[Array[Any], Arr[A]]

		@inline def concat[A >: E](suffix :IterableOnce[A]) :Arr[A] = appendedAll(suffix)
		@inline def concat[A >: E](suffix :ArrayLike[A]) :Arr[A] = appendedAll(suffix)

		@inline def ++[A >: E](suffix :IterableOnce[A]) :Arr[A] = appendedAll(suffix)
		@inline def ++[A >: E](suffix :ArrayLike[A]) :Arr[A] = appendedAll(suffix)

		@inline def :++[A >: E](suffix :IterableOnce[A]) :Arr[A] = appendedAll(suffix)
		@inline def :++[A >: E](suffix :ArrayLike[A]) :Arr[A] = appendedAll(suffix)

		@inline def appendedAll[A >: E](suffix :IterableOnce[A]) :Arr[A] =
			array.appendedAll(suffix).castFrom[Array[Any], Arr[A]]

		def appendedAll[A >: E](suffix :ArrayLike[A]) :Arr[A] = {
			val res = new Array[Any](array.length + suffix.length)
			arraycopy(array, 0, res, 0, array.length)
			arraycopy(suffix, 0, res, array.length, suffix.length)
			res.castFrom[Array[Any], Arr[A]]
		}

		@inline def ++:[A >: E](prefix :IterableOnce[A]) :Arr[A] = prependedAll(prefix)
		@inline def ++:[A >: E](prefix :ArrayLike[A]) :Arr[A] = prependedAll(prefix)

		@inline def prependedAll[A >: E](prefix :IterableOnce[A]) :Arr[A] =
			array.prependedAll(prefix).castFrom[Array[Any], Arr[A]]

		def prependedAll[A >: E](prefix :ArrayLike[A]) :Arr[A] = {
			val prefixLength = prefix.length
			val res = new Array[Any](array.length + prefixLength)
			arraycopy(prefix, 0, res, 0, prefixLength)
			arraycopy(array, 0, res, prefixLength, array.length)
			res.castFrom[Array[Any], Arr[A]]
		}

		def padTo[A >: E](len :Int, elem :A) :Arr[A] =
			if (len <= array.length)
				array.castFrom[Array[Any], Arr[A]]
			else {
				val res = new Array[Any](len)
				var i = array.length
				arraycopy(array, 0, res, 0, i)
				while (i < len) {
					res(i) = elem
					i += 1
				}
				res.castFrom[Array[Any], Arr[A]]
			}

		@inline def patch[A >: E](from :Int, other :IterableOnce[A], replaced :Int) :Arr[A] =
			array.patch(from, other, replaced).castFrom[Array[Any], Arr[A]]

		@inline def patch[A >: E](from :Int, other :ArrayLike[A], replaced :Int) :Arr[A] = {
			val clippedFrom     = math.max(from, 0)
			val clippedReplaced = math.min(math.max(replaced, 0), array.length - clippedFrom)
			val until           = clippedFrom + clippedReplaced
			ErasedArray.copyOfRanges(
				array, 0, from, other, 0, other.length, array, until, array.length
			).castFrom[Array[Any], Arr[A]]
		}
	}



	/** A lazy filtered array. No filtering is applied until one of `foreach`, `map` or `flatMap` is called.
	  * Implementation adapted from [[collection.ArrayOps.WithFilter ArrayOps.WithFilter]] to return `Arr[T]`.
	  */
	class WithFilter[Arr[_], E] private[collections] (p :E => Boolean, xs :Array[Any], factory :IterableFactory[Arr]) {
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


	private[collections] trait extensions extends Any {
		@inline final def RefArrayLikeExtension[A](self :RefArrayLike[A]) :RefArrayLikeExtension[A, RefArrayLike] =
			new RefArrayLikeExtension(self.castFrom[RefArrayLike[A], Array[Any]])
	}

	@SerialVersionUID(Ver)
	object extensions extends extensions
}






/** Shared implementation of factories of type `Arr`, represented by an `Array[AnyRef]` during execution.
  * @define Coll `RefArrayLike`
  * @tparam coll object array
  */
private[collections] sealed abstract class RefArrayLikeFactory[Arr[_]] extends IterableFactory[Arr] {

	/** A new `Array[AnyRef]` of the specified length, cast to $Coll`[E]`. */
	@inline final def ofDim[E](length :Int) :Arr[E] = new Array[Any](length).castFrom[Array[Any], Arr[E]]

	/** Creates a new `Array[E]` of length `size`, executes the given initialization function for it,
	  * and returns it as an $Coll`[E]`. It is a pattern for initialization safer than manually creating
	  * an `Array[E]`, writing to it, and casting it $Coll`[E]`. The application should ''not'' retain a reference
	  * to the array given as the function argument.
	  */
	@inline final def init[E](size :Int)(f :Array[_ >: E] => Unit) :Arr[E] = {
		val res = new Array[Any](size)
		f(res.castFrom[Array[Any], Array[E]])
		res.castFrom[Array[Any], Arr[E]]
	}

	/** Allocates a new `Array[AnyRef]` and copies all elements from the argument, returning it as a $Coll`[E]`.
	  * If the argument is a value array, the elements will be boxed.
	  */
	@inline final def copyOf[E](array :ArrayLike[E]) :Arr[E] =
		copyOf(array, array.castFrom[ArrayLike[E], Array[_]].length)

	/** Reallocates the given array as a $Coll`[E]` of a new size, and copies `min(newLength, array.length)`
	  * of its first elements. If the argument is a value array, the elements will be boxed.
	  */
	@inline final def copyOf[E](array :ArrayLike[E], newLength :Int) :Arr[E] = {
		val res = new Array[Any](newLength)
		ArrayLike.copy(array, 0, res, 0, newLength)
		res.castFrom[Array[Any], Arr[E]]
	}

	/** Reallocates the given array as a $Coll`[E]` of a new size, and copies `min(newLength - offset, array.length)`
	  * of its first elements to positions starting with `offset`.
	  * If the argument is a value array, the elements will be boxed.
	  */
	final def copyOf[E](array :ArrayLike[E], offset :Int, newLength :Int) :Arr[E] =
		if (newLength == 0)
			empty[E]
		else {
			if (offset < 0 | offset > newLength)
				throw new IndexOutOfBoundsException(
					toString + ".copyOf(" + array.className + "|" + array.length + "|, " + offset + ", " + newLength + ")"
				)
			val res = new Array[Any](newLength)
			ArrayLike.copy(array, 0, res, offset, math.min(array.length, newLength - offset))
			res.castFrom[Array[Any], Arr[E]]
		}

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
		if (from < 0 || from > array.asInstanceOf[Array[Any]].length)
			throw new IndexOutOfBoundsException(
				toString + ".copyOfRange(" + array.className + "|" + array.length + "|, " + from + ", " + until + ")"
			)
		else if (until <= from)
			empty
		else {
			val res    = new Array[Any](until - from)
			val copied = math.min(until - from, array.castFrom[ArrayLike[E], Array[_]].length - from)
			ArrayLike.copy(array, from, res, 0, copied)
			res.castFrom[Array[Any], Arr[E]]
		}

	/** Creates new $coll of the specified length and copies to it the values in the index range `[from, until)`
	  * from the argument array.
	  * The elements will be copied starting with index `0` in the returned $Coll, in the number being the minimum of
	  * `(until - from, array.length-from, newLength - offset)`. If the argument is a value array, the values
	  * will be boxed.
	  * @param array     The sliced array.
	  * @param from      The index of the element in `array` to be copied as the first element of the new array.
	  *                  Must be in range `[0, array.length]`, or an `IndexOutOfBoundsException` will be thrown.
	  * @param until     The index after the last copied element in `array`. If less than `from`, an empty array is returned.
	  *                  If `until > array.length`, then the new array will contain `until - array.length` `null` elements
	  *                  in its suffix.
	  * @param newLength The length of the created array.
	  * @return An `Array[AnyRef]` of length `until - from` as a $Coll`[E]`, with the copied slice.
	  */
	@inline final def copyOfRange[E](array :ArrayLike[E], from :Int, until :Int, newLength :Int) :Arr[E] =
		copyOfRange(array, from, until, 0, newLength)

	/** Copies the elements of `array` in the index range `[from, until)` to a new array with an erased element type.
	  * The elements will be copied to positions starting with `offset`, in the number being the minimum of
	  * `(until - from, array.length-from, newLength - offset)`.
	  * @param array     The sliced array.
	  * @param from      The index of the element in `array` to be copied as the first element of the new array.
	  *                  Must be in range `[0, array.length]`, or an `IndexOutOfBoundsException` will be thrown.
	  * @param until     The index after the last copied element in `array`. If less than `from`, an empty array is returned.
	  *                  If `until > array.length`, then the new array will contain `until - array.length` `null` elements
	  *                  in its suffix.
	  * @param offset    An index in the created array to which the element `array(from)` is copied.
	  * @param newLength The length of the created array.
	  * @return An `Array[AnyRef]` of length `until - from` as a $Coll`[E]`, with the copied slice.
	  */
	final def copyOfRange[E](array :ArrayLike[E], from :Int, until :Int, offset :Int, newLength :Int) :Arr[E] =
		if (newLength == 0)
			empty
		else if (offset < 0 | offset > newLength || from < 0 || from > array.length)
			throw new IndexOutOfBoundsException(
				toString + ".copyOfRange(" + array.className + "|" + array.length + "|, " + from + ", " + until + ", "
					+ offset + ", " + newLength + ")"
			)
		else if (until <= from | offset == newLength)
			ofDim[E](newLength)
		else {
			val res = new Array[Any](newLength)
			val copied = math.min(newLength - offset, math.min(until - from, array.length - from))
			ArrayLike.copy(array, from, res, 0, copied)
			res.castFrom[Array[Any], Arr[E]]
		}

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
		if (from1 < 0 | from2 < 0 || from1 > array1.length || from2 > array2.length)
			throw new IndexOutOfBoundsException(
				s"$toString.copyOfRanges(${array1.localClassName}|${array1.length}|, $from1, $until1, " +
					s"${array2.localClassName}|${array2.length}|, $from2, $until2)."
			)
		else {
			val length1 = math.min(from1, until1) - from1
			val length2 = math.min(from2, until2) - from2
			if (length1 + length2 == 0)
				empty[E]
			else {
				val res     = new Array[Any](length1 + length2).castFrom[Array[Any], Array[E]]
				ArrayLike.copy(array1, from1, res, 0, length1)
				ArrayLike.copy(array2, from2, res, length1, length2)
				res.castFrom[Array[E], Arr[E]]
			}
		}

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


	/** An array filled with `n` copies of `elem`. */
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

	/** Similar to [[collection.ItrableFactory.iterate iterate]],
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



	final def unapplySeq[E](array :Arr[E]) :UnapplySeqWrapper[E] =
		new UnapplySeqWrapper(array.castFrom[Arr[E], Array[E]])

	protected final def isSeq[E] :IsSeq[Arr[E]] { type A = E; type C = Arr[E] } =
		refArrayIsSeqPrototype.castFrom[IsSeq[Arr[Any]], IsSeq[Arr[E]] {type A = E; type C = Arr[E] }]

	private[this] val refArrayIsSeqPrototype = new IsSeq[Arr[Any]] with Serializable {
		override type A = Any
		override type C = Arr[Any]

		override def apply(array :Arr[Any]) =
			new collection.StrictOptimizedSeqOps[Any, IndexedSeq, Arr[Any]]
				with collection.IndexedSeqOps[Any, IndexedSeq, Arr[Any]]
			{
				override def apply(i :Int) :Any = array.castFrom[Arr[Any], Array[Any]].apply(i)
				override def length :Int = array.castFrom[Arr[Any], Array[Any]].length

				override def fromSpecific(coll :IterableOnce[Any]) = RefArrayLikeFactory.this.from(coll)
				override def newSpecificBuilder = newBuilder

				@nowarn("cat=deprecation")
				override def toIterable = mutable.ArraySeq.make(array.castFrom[Arr[Any], Array[Any]]) //new IndexedSeqArrayAdapter(array.asInstanceOf[Array[Any]])
				override def coll = array
				override def iterableFactory = IndexedSeq

				override def iterator = ArrayIterator(array.castFrom[Arr[Any], Array[Any]]) //array.asInstanceOf[Array[Any]].iterator
			}
		private def readResolve = RefArrayLikeFactory.this.isSeq
	}

}




/** A factory of `RefArray` - values with `Array` API available as extension methods, represented in runtime
  * always as `Array[AnyRef]`, regardless of their actual type parameter.
  * @define Coll `RefArray`
  * @define coll reference object array
  */
@SerialVersionUID(Ver)
case object RefArray extends RefArrayLikeFactory[RefArray] {
	/** Extension methods specific to [[net.noresttherein.sugar.collections.RefArray RefArray]] only.
	  * These are the standard [[net.noresttherein.sugar.collections.RefArray.RefArrayExtension.update update]],
	  * as `RefArray` is the only mutable `ArrayLike` other than `Array`, and conversions to immutable sequences,
	  * implemented by low level array copying and `Array`-backed implementations. Enabled by importing any of
	  * `net.noresttherein.sugar.extensions.`[[net.noresttherein.sugar.collections.RefArray.extensions.RefArrayExtension RefArrayExtension]],
	  * `net.noresttherein.sugar.collections.extensions.RefArrayExtension`, or
	  * `net.noresttherein.sugar.collections.RefArray.extensions.RefArrayExtension`.
	  * @see [[net.noresttherein.sugar.collections.ArrayLike.ArrayLikeExtension]] imported as
	  *      `net.noresttherein.sugar.extensions.`[[net.noresttherein.sugar.collections.RefArray.extensions.RefArrayAsArrayLikeExtension RefArrayAsArrayLikeExtension]],
	  *      `net.noresttherein.sugar.collections.extensions.RefArrayAsArrayLikeExtension`, or
	  *      `net.noresttherein.sugar.collections.RefArray.extensions.RefArrayAsArrayLikeExtension`.
	  *
	  * @see [[net.noresttherein.sugar.collections.RefArrayLike.RefArrayLikeExtension]] imported as
	  *      `net.noresttherein.sugar.extensions.`[[net.noresttherein.sugar.collections.RefArray.extensions.RefArrayAsRefArrayLikeExtension RefArrayAsRefArrayLikeExtension]],
	  *      `net.noresttherein.sugar.collections.extensions.RefArrayAsRefArrayLikeExtension`, or
	  *      `net.noresttherein.sugar.collections.RefArray.extensions.RefArrayAsRefArrayLikeExtension`.
	  */
	class RefArrayExtension[A] private[collections] (private val self :Array[AnyRef]) extends AnyVal {
		@inline def asAnyArray :Array[Any] = self.castFrom[Array[AnyRef], Array[Any]]

		def update(idx :Int, elem :A) :Unit = asAnyArray(idx) = elem

		/** Sets the values at indices `index, index + 1, index + 2, ...` to `first, second, elems.head`
		  * and subsequent elements of `rest`. If any of the indices in the range covering all provided elements
		  * is out of range, it is simply ignored. For example,
		  * {{{
		  *     > RefArray("You", "Boo", "I").updateAll(-1, "Imoen", "CHARNAME", "Miniature Giant Space Hamster")
		  *     > Array[AnyRef]("CHARNAME", "Miniature Giant Space Hamster", "I")
		  * }}}
		  */
		@inline def updateAll(idx :Int, first :A, second :A, rest :A*) :Unit =
			asAnyArray.updateAll(idx, first, second, rest :_*)

		/** Sets the values at indices `index, index + 1, ...` to subsequent elements of `elems`.
		  * If any of the indices in the range covering all provided elements
		  * is out of range, it is simply ignored. For example,
		  * {{{
		  *     > RefArray("You", "Boo", "I").updateAll(-1, Seq("Imoen", "CHARNAME", "Miniature Giant Space Hamster"))
		  *     > Array[AnyRef]("CHARNAME", "Miniature Giant Space Hamster", "I")
		  * }}}
		  */
		@inline def updateAll(idx :Int, elems :IterableOnce[A]) :Unit = asAnyArray.updateAll(idx, elems)

		/** Fills the whole array with the given value. */
		@inline def fill(value :A) :Unit = asAnyArray.fill(value)

		/** Fills the section of this array between the specified indices with the given value. */
		@inline def fill(from :Int, until :Int, value :A) :Unit = asAnyArray.fill(from, until, value)

		@inline def toSeq        :Seq[A] = toIndexedSeq
		@inline def toIndexedSeq :IndexedSeq[A] = IRefArray.Wrapped(self.toIRefArray.castParam[A])
		@inline def toOps        :collection.IndexedSeqOps[A, RefArray, RefArray[A]] =
			new RefArrayAsSeq(self.castFrom[Array[AnyRef], RefArray[A]])
	}


	/** Wraps reference arrays in indexed sequences and unwraps all known mutable collections backed by arrays
	  * in a safe manner. Arrays are represented as [[net.noresttherein.sugar.collections.RefArray RefArray]];
	  * only 'untagged' collections, that is backed by `Array[AnyRef]`, and not some of its subclasses, are recognized.
	  * The extractor never attempts to copy a wrapped array, and returns it only if it is not deemed immutable.
	  */
	@SerialVersionUID(Ver)
	object Wrapped {
		def apply[A](array :RefArray[A]) :mutable.IndexedSeq[A] = //RefArraySlice.of(array)
			mutable.ArraySeq.make(array.castFrom[RefArray[A], Array[A]])

		def unapply[A](items :IterableOnce[A]) :Opt[RefArray[A]] = {
			val array = items match {
				case slice :AbstractArraySlice[_] if slice.knownSize == slice.unsafeArray.length && !slice.isImmutable =>
					Got(slice.unsafeArray)
				case seq :mutable.ArraySeq[_] =>
					Got(seq.array)
				case _ => Lack
			}
			if (array.isDefined && array.get.getClass == classOf[Array[AnyRef]])
				array.castFrom[Opt[Array[_]], Opt[RefArray[A]]]
			else
				Lack
		}

		@SerialVersionUID(Ver)
		object Slice {
			def apply[A](array :RefArray[A], from :Int, until :Int) :collection.IndexedSeq[A] =
				RefArraySlice.of(array, from, until)

			def unapply[A](items :IterableOnce[A]) :Opt[(RefArray[A], Int, Int)] = items match {
				case seq :mutable.ArraySeq[_] if seq.array.getClass == classOf[Array[AnyRef]] =>
					Got((seq.array.castFrom[Array[_], RefArray[A]], 0, seq.array.length))

				case slice :AbstractArraySlice[A] if slice.knownSize >= 0 && !slice.isImmutable =>
					val array = slice.unsafeArray.castFrom[Array[_], RefArray[A]]
					if (array.getClass == classOf[Array[AnyRef]])
						Got((array, slice.startIndex, slice.knownSize))
					else
						Lack
				case _ =>
					Lack
			}
		}
	}


	implicit def RefArrayToSeq[A](array :RefArray[A]) :mutable.IndexedSeq[A] =
		Wrapped(array)

	implicit def RefArrayIsSeq[E] :IsSeq[RefArray[E]] { type A = E; type C = RefArray[A] } =
		isSeq

	implicit def RefArrayOrdering[A :Ordering] :Ordering[RefArray[A]] =
		Array.ArrayOrdering[A].castParam[RefArray[A]]

	implicit def RefArrayClassTag[A] :ClassTag[RefArray[A]] = tag.castParam[RefArray[A]]
	private[this] val tag = classTag[Array[AnyRef]]


	private[collections] sealed trait RefArrayRank2Extensions extends Any with ArrayLike.extensions {
		/** Adds various additional folding methods with a break condition to any `RefArray`. */
		@inline implicit final def RefArrayAsIterableOnceExtension[E](self :RefArray[E]) :IterableOnceExtension[E] =
			new IterableOnceExtension(new RefArrayAsSeq(self))

		/** Adds various methods for mapping/flatMapping collections to any `RefArray`.
		  * These either pass along additional state, or have a break condition. Roughly equivalent to working
		  * with `toLazyList.scan`, but cleaner and more efficient.
		  */
		@inline implicit final def RefArrayAsIterableExtension[E](self :RefArray[E])
				:IterableExtension[E, RefArray, RefArray[E]] =
			new IterableExtension[E, RefArray, RefArray[E]](new RefArrayAsSeq(self))

		/** Alternative, safer implementations of [[scala.collection.SeqOps.indexOf indexOf]] for reference arrays,
		  * which do not return a negative index when the element is not found.
		  */
	   @inline implicit final def RefArrayAsSeqExtension[E](self :RefArray[E]) :SeqExtension[E, RefArray, RefArray[E]] =
			new SeqExtension[E, RefArray, RefArray[E]](new RefArrayAsSeq(self))

		/** Operations on suffixes of a sequence and binary search methods on sorted reference arrays. */
		@inline implicit final def RefArrayAsIndexedSeqExtension[E](self :RefArray[E])
				:IndexedSeqExtension[E, RefArray, RefArray[E]] =
			new IndexedSeqExtension(new RefArrayAsSeq(self))
	}

	private[collections] trait RefArrayRank1Extensions extends Any with RefArrayRank2Extensions {
		@inline implicit final def RefArrayAsArrayLikeExtension[A](array :RefArray[A])
				:ArrayLike.ArrayLikeExtension[A, RefArray] =
			new ArrayLike.ArrayLikeExtension[A, RefArray](array.castFrom[RefArray[A], Array[Any]])
	}

	/** Conversions providing extension methods for `RefArray`. */
	private[collections] trait extensions extends Any with RefArrayRank1Extensions {

		@inline implicit final def RefArrayClassTag[E] :ClassTag[RefArray[E]] =
			extensions.RefArrayClassTagPrototype.castParam[RefArray[E]]

		@inline implicit final def RefArrayExtension[A](array :RefArray[A]) :RefArrayExtension[A] =
			new RefArrayExtension(array.castFrom[RefArray[A], Array[AnyRef]])

		@inline implicit final def RefArrayAsRefArrayLikeExtension[A](array :RefArray[A])
				:RefArrayLikeExtension[A, RefArray] =
			new RefArrayLikeExtension(array.castFrom[RefArray[A], Array[Any]])
	}

	/** Conversions providing extension methods for `RefArray`. */
	@SerialVersionUID(Ver)
	object extensions extends extensions {
		private val RefArrayClassTagPrototype = classTag[Array[AnyRef]]
	}

}




/** A factory of `IRefArray` - immutable values with `Array` API available as extension methods, represented in runtime
  * always as `Array[AnyRef]`, regardless of their actual type parameter.
  * Aside from standard factory methods inherited from `IterableFactory`, and those modelled after
  * factory methods in [[Array]] and [[java.util.Arrays]], there is another convenient way of initializing
  * a new instance without a need for an intermediate collection in method
  * [[net.noresttherein.sugar.collections.IRefArray.init init]], which grants a temporary access to the new object
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
	/** Extension methods specific to [[net.noresttherein.sugar.collections.IRefArray IRefArray]] only -
	  * standard conversions to immutable sequences implemented by wrapping the underlying array.
	  * Enabled by importing any of
	  * `net.noresttherein.sugar.extensions.`[[net.noresttherein.sugar.collections.IRefArray.extensions.IRefArrayExtension IRefArrayExtension]],
	  * `net.noresttherein.sugar.collections.extensions.IRefArrayExtension`, or
	  * `net.noresttherein.sugar.collections.RefArray.extensions.IRefArrayExtension`.
	  * @see [[net.noresttherein.sugar.collections.ArrayLike.ArrayLikeExtension]] imported as
	  *      `net.noresttherein.sugar.extensions.`[[net.noresttherein.sugar.collections.IRefArray.extensions.IRefArrayAsArrayLikeExtension IRefArrayAsArrayLikeExtension]],
	  *      `net.noresttherein.sugar.collections.extensions.IRefArrayAsArrayLikeExtension`, or
	  *      `net.noresttherein.sugar.collections.RefArray.extensions.IRefArrayAsArrayLikeExtension`.
	  *
	  * @see [[net.noresttherein.sugar.collections.RefArrayLike.RefArrayLikeExtension]] imported as
	  *      `net.noresttherein.sugar.extensions.`[[net.noresttherein.sugar.collections.IRefArray.extensions.IRefArrayAsRefArrayLikeExtension IRefArrayAsRefArrayLikeExtension]],
	  *      `net.noresttherein.sugar.collections.extensionsIRefArrayAsRefArrayLikeExtension`, or
	  *      `net.noresttherein.sugar.collections.RefArray.extensions.IRefArrayAsRefArrayLikeExtension`.
	  */
	class IRefArrayExtension[A] private[collections] (private val self :Array[Any]) extends AnyVal {
		@inline def take(n :Int) :IRefArray[A] =
			(if (n <= 0) Array.emptyAnyArray
			else if (n >= self.length) self
			else self.take(n)).castFrom[Array[Any], IRefArray[A]]

		@inline def drop(n :Int) :IRefArray[A] =
			(if (n <= 0) self
			else if (n >= self.length) Array.emptyAnyArray
			else self.drop(n)).asInstanceOf[IRefArray[A]]

		@inline def takeRight(n :Int) :IRefArray[A] =
			(if (n <= 0) Array.emptyAnyArray
			else if (n >= self.length) self
			else self.drop(self.length - n)).castFrom[Array[Any], IRefArray[A]]

		@inline def dropRight(n :Int) :IRefArray[A] =
			(if (n <= 0) self
			else if (n >= self.length) Array.emptyAnyArray
			else self.take(self.length - n)).castFrom[Array[Any], IRefArray[A]]

		@inline def takeWhile(p :A => Boolean) :IRefArray[A] = take(self.segmentLength(p.asInstanceOf[Any => Boolean]))
		@inline def dropWhile(p :A => Boolean) :IRefArray[A] = drop(self.segmentLength(p.asInstanceOf[Any => Boolean]))
		@inline def slice(from :Int, until :Int) :IRefArray[A] =
			(if (until <= from | until < 0 || from >= self.length) Array.emptyAnyArray
			else if (from <= 0 && until >= self.length) self
			else self.slice(from, until)).castFrom[Array[Any], IRefArray[A]]

		@inline def splitAt(n :Int) :(IRefArray[A], IRefArray[A]) = (take(n), drop(n))

		@inline def span(p :A => Boolean) :(IRefArray[A], IRefArray[A]) = 
			self.segmentLength(p.asInstanceOf[Any => Boolean]) match {
				case 0 =>
					(IRefArray.empty[A], self.castFrom[Array[Any], IRefArray[A]])
				case n if n == self.length =>
					(self.castFrom[Array[Any], IRefArray[A]], IRefArray.empty[A])
				case n =>
					(self.take(n).castFrom[Array[Any], IRefArray[A]],
						self.drop(n).castFrom[Array[Any], IRefArray[A]])
			}

		@inline def toSeq        :Seq[A] = toIndexedSeq
		@inline def toIndexedSeq :IndexedSeq[A] = Wrapped(self.castFrom[Array[Any], IRefArray[A]])
		@inline def toOps        :IndexedSeqOps[A, IRefArray, IRefArray[A]] =
			new IRefArrayAsSeq(self.asInstanceOf[IRefArray[A]])
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


	/** Wraps and unwraps immutable `IndexedSeq` instances and other immutable collections backed by arrays
	  * in a safe manner. Arrays are represented as [[net.noresttherein.sugar.collections.IRefArray IRefArray]]
	  * instances to prevent accidental modification and ensure that the user is aware that an array will be represented
	  * as an immutable structure. Note that only 'untagged' collections, that is backed by `Array[AnyRef]`,
	  * and not some of its subclasses, are recognized.
	  */
	@SerialVersionUID(Ver)
	object Wrapped {
		private[this] val wrapper =
			PassedArrayWrapper.castParamFrom[
				ArrayLikeSliceFactory[IndexedSeq, IArray], ArrayLikeSliceFactory[IndexedSeq, IRefArray]
			] getOrElse IRefArraySlice

		def apply[A](array :IRefArray[A]) :IndexedSeq[A] = wrapper.of(array)

		def unapply[A](items :IterableOnce[A]) :Opt[IRefArray[A]] = {
			val array = items match {
				case slice :AbstractArraySlice[_] if slice.knownSize == slice.unsafeArray.length && slice.isImmutable =>
					Got(slice.unsafeArray)
				case seq :ArraySeq[_] => Got(seq.unsafeArray)
				case _                => Lack
			}
			if (array.isDefined && array.get.getClass == classOf[Array[AnyRef]])
				array.castFrom[Opt[Array[_]], Opt[IRefArray[A]]]
			else
				Lack
		}

		@SerialVersionUID(Ver)
		object Slice {
			private[this] val wrapper =
				PassedArrayWrapper.castParamFrom[
					ArrayLikeSliceFactory[IndexedSeq, IArray], ArrayLikeSliceFactory[IndexedSeq, IRefArray]
				] getOrElse IRefArraySlice

			def apply[A](array :IRefArray[A], from :Int, until :Int) :IndexedSeq[A] =
				wrapper.of(array, from, until)

			def unapply[A](items :IterableOnce[A]) :Opt[(IRefArray[A], Int, Int)] = items match {
				case seq :ArraySeq[_] if seq.unsafeArray.getClass == classOf[Array[AnyRef]] =>
					Got((seq.unsafeArray.castFrom[Array[_], IRefArray[A]], 0, seq.unsafeArray.length))

				case arr :AbstractArraySlice[A] if arr.isImmutable =>
					val array = arr.unsafeArray.castFrom[Array[_], IRefArray[A]]
					if (array.getClass == classOf[Array[AnyRef]])
						Got((array, arr.startIndex, arr.knownSize))
					else
						Lack
				case _ =>
					Lack
			}
		}
	}


	@inline implicit def IRefArrayToSeq[A](array :IRefArray[A]) :IndexedSeq[A] = Wrapped(array)

	implicit def IRefArrayIsSeq[E] :IsSeq[IRefArray[E]] { type A = E; type C = IRefArray[A] } =
		isSeq

	implicit def IRefArrayOrdering[A :Ordering] :Ordering[IRefArray[A]] =
		Array.ArrayOrdering[A].castParam[IRefArray[A]]

	implicit def IRefArrayClassTag[A] :ClassTag[IRefArray[A]] = tag.castParam[IRefArray[A]]
	private[this] val tag = classTag[Array[AnyRef]]



	private[collections] sealed trait IRefArrayRank2Extensions extends Any with ArrayLike.extensions {
		/** Adds various additional folding methods with a break condition to any `RefArray`. */
		@inline implicit final def IRefArrayAsIterableOnceExtension[E](self :IRefArray[E]) :IterableOnceExtension[E] =
			new IterableOnceExtension(new IRefArrayAsSeq(self))

		/** Adds various methods for mapping/flatMapping collections to any `IRefArray`.
		  * These either pass along additional state, or have a break condition. Roughly equivalent to working
		  * with `toLazyList.scan`, but cleaner and more efficient.
		  */
		@inline implicit final def IRefArrayAsIterableExtension[E](self :IRefArray[E])
				:IterableExtension[E, IRefArray, IRefArray[E]] =
			new IterableExtension[E, IRefArray, IRefArray[E]](new IRefArrayAsSeq(self))

		/** Alternative, safer implementations of [[scala.collection.SeqOps.indexOf indexOf]] for immutable reference arrays,
		  * which do not return a negative index when the element is not found.
		  */
	   @inline implicit final def IRefArrayAsSeqExtension[E](self :IRefArray[E]) :SeqExtension[E, IRefArray, IRefArray[E]] =
			new SeqExtension[E, IRefArray, IRefArray[E]](new IRefArrayAsSeq(self))

		/** Operations on suffixes of a sequence and binary search methods on sorted immutable reference arrays. */
		@inline implicit final def IRefArrayAsIndexedSeqExtension[E](self :IRefArray[E])
				:IndexedSeqExtension[E, IRefArray, IRefArray[E]] =
			new IndexedSeqExtension(new IRefArrayAsSeq(self))
	}

	private[collections] sealed trait IRefArrayRank1Extensions extends Any with IRefArrayRank2Extensions {

		@inline implicit final def IRefArrayAsArrayLikeExtension[A](array :IRefArray[A])
				:ArrayLike.ArrayLikeExtension[A, IRefArray] =
			new ArrayLike.ArrayLikeExtension(array.castFrom[IRefArray[A], Array[Any]])
	}

	/** Conversions providing extension methods for `IRefArray`. */
	private[collections] trait extensions extends Any with IRefArrayRank1Extensions {
		@inline implicit final def IRefArrayAsRefArrayLikeExtension[A](array :IRefArray[A])
				:RefArrayLikeExtension[A, IRefArray] =
			new RefArrayLikeExtension(array.castFrom[IRefArray[A], Array[Any]])

		@inline implicit final def IRefArrayClassTag[E] :ClassTag[IRefArray[E]] =
			extensions.IRefArrayClassTagPrototype.castParam[IRefArray[E]]

		@inline implicit final def IRefArrayExtension[A](array :IRefArray[A]) :IRefArrayExtension[A] =
			new IRefArrayExtension(array.castFrom[IRefArray[A], Array[Any]])
	}


	/** Conversions providing extension methods for `IRefArray`. */
	@SerialVersionUID(Ver)
	object extensions extends extensions {
		private final val IRefArrayClassTagPrototype = classTag[Array[AnyRef]]
	}

}






/** Factory of erased arrays: `Array[E]` represented in runtime by an `Array[Any]`.
  * ''This will work only in generic contexts: '' `ErasedArray.ofDim[Int](1)` will throw a [[ClassCastException]].
  * @define Coll `Array`
  * @define coll array
  */
@SerialVersionUID(Ver)
private[noresttherein] case object ErasedArray extends RefArrayLikeFactory[Array] with IterableFactory[Array] {

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
			ArrayLikeSlice.of(array) //not mutable.ArraySeq to be neither mutable nor immutable

		def unapply[A](elems :IterableOnce[A]) :Opt[Array[_]] = elems match {
			case seq :ArraySeq[_]                                                      => Got(seq.unsafeArray)
			case seq :mutable.ArraySeq[_]                                              => Got(seq.array)
			case seq :AbstractArraySlice[_] if seq.knownSize == seq.unsafeArray.length => Got(seq.unsafeArray)
			case seq :CubeBuffer[_] if seq.length == seq.unsafeArray.length            => Got(seq.unsafeArray)
			case _                                                                     => Lack
		}

		@SerialVersionUID(Ver)
		object Slice {
			@inline def apply[A](array :Array[A], from :Int, until :Int) :collection.IndexedSeq[A] =
				ArrayLikeSlice.of(array, from, until)

			def unapply[A](elems :IterableOnce[A]) :Opt[(Array[_], Int, Int)] = elems match {
				case seq :AbstractArraySlice[_] =>
					val size   = seq.knownSize
					val offset = seq.startIndex
					if (size >= 0)
						Got((seq.unsafeArray, offset, offset + size))
					else
						Lack
				case _ :collection.IndexedSeqOps[_, _, _] => elems match {
					case seq  :ArraySeq[_]                        => Got((seq.unsafeArray, 0, seq.unsafeArray.length))
					case seq  :mutable.ArraySeq[_]                => Got((seq.array, 0, seq.array.length))
					case seq :CubeBuffer[_] if seq.dimension == 1 =>
						val offset = seq.startIndex
						Got((seq.unsafeArray, offset, offset + seq.length))
					case _                                        => Lack
				}
				case _ => Lack
			}
		}
	}
}
