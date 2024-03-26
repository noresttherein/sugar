package net.noresttherein.sugar.collections

import java.lang.reflect.InvocationTargetException

import scala.collection.immutable.IndexedSeqDefaults.defaultApplyPreferredMaxLength
import scala.collection.immutable.Set.{Set1, Set2, Set3, Set4}
import scala.collection.immutable.{ArraySeq, HashSet, LinearSeq, SeqOps, WrappedString}
import scala.collection.{IndexedSeqView, IterableFactory, IterableOnceOps, IterableOps, View, immutable, mutable}
import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.reflect.ClassTag

import net.noresttherein.sugar.arrays.{ArrayLike, ErasedArray}
import net.noresttherein.sugar.casting.castingMethods
import net.noresttherein.sugar.collections.HasFastSlice.{hasFastDrop, preferDropOverIterator}
import net.noresttherein.sugar.collections.IndexedIterable.applyPreferred
import net.noresttherein.sugar.collections.extensions.IterableOnceExtension
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.exceptions.{illegal_!, outOfBounds_!}
import net.noresttherein.sugar.extensions.ClassExtension
import net.noresttherein.sugar.reflect.extensions.classNameMethods
import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.funny.generic.Any1
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




private[sugar] object util {
	//These two methods are problematic because, in theory, they can be overridden by a class to throw an exception
	// and simply not used by an implementation.
	def specificBuilder[E, CC[_], C](items :IterableOps[E, CC, C]) :Builder[E, C] =
		try {
			newSpecificBuilderMethod.invoke(items).asInstanceOf[Builder[E, C]]
		} catch {
			case e :InvocationTargetException =>
				e.getTargetException.addSuppressed(e)
				throw e.getTargetException
		}

	def fromSpecific[E, CC[_], C](items :IterableOps[E, CC, C])(coll :IterableOnce[E]) :C =
		try {
			fromSpecificMethod.invoke(items, coll).asInstanceOf[C]
		} catch {
			case e :InvocationTargetException =>
				e.getTargetException.addSuppressed(e)
				throw e.getTargetException
		}

	def coll[E, CC[_], C](items :IterableOps[E, CC, C]) :C =
		try {
			collMethod.invoke(items).asInstanceOf[C]
		} catch {
			case e :InvocationTargetException =>
				e.getTargetException.addSuppressed(e)
				throw e.getTargetException
		}

	def className(items :IterableOnce[_]) :String = items match {
		case _ :Iterable[_] =>
			try classNameMethod.invoke(items).asInstanceOf[String] catch {
				case _ :InvocationTargetException => items.localClassName
			}
		case _ => items.localClassName
	}


	private[this] val newSpecificBuilderMethod =
		classOf[IterableOps[Any, Iterable, Iterable[Any]]].getMethod("newSpecificBuilder")

	private[this] val fromSpecificMethod =
		classOf[IterableOps[Any, Iterable, Iterable[Any]]].getMethod("fromSpecific", classOf[IterableOnce[_]])

	private[this] val collMethod = classOf[IterableOps[Any, Iterable, Iterable[Any]]].getMethod("coll")
	private[this] val classNameMethod = classOf[Iterable[Any]].getMethod("className")

	newSpecificBuilderMethod.setAccessible(true)
	fromSpecificMethod.setAccessible(true)
	collMethod.setAccessible(true)
	classNameMethod.setAccessible(true)


	def knownEmpty(items :IterableOnce[_]) :Boolean = {
		val size = items.knownSize
		size == 0 || size < 0 && items.knownStrict && (items match {
			case ops :IterableOnceOps[_, Any1, _] => ops.isEmpty
			case _                                => false
		})
	}

	def knownUnique(items :IterableOnce[_]) :Boolean =
		items.isInstanceOf[collection.Set[_]] || items.isInstanceOf[Ranking[_]] || knownEmpty(items)

//	def knownCovariant(items :IterableOnce[_]) :Boolean = items match {
//		case _ if items.knownSize == 0 => true
//		case _ :immutable.Iterable[_] => items match {
//			case _ :Set[_] => items match {
//				case _ :HashSet[_] | _ :Set1[_] | _ :Set2[_] | _ :Set3[_] | _ :Set4[_] | _ :SeqSet[_] => true
////				case set :EqSet[_] => set.isCovariant
//				case _ => false
//			}
//			case _ :SeqOps[_, Any1, _] => true
//		}
////		case _ :Iterator[_] => true
//		case _ => false
//	}



	def prependReverse[A](initReversed :collection.LinearSeq[A], tail :collection.LinearSeq[A]) :collection.LinearSeq[A] =
		(initReversed, tail) match {
			case (list1 :List[A], list2 :List[A]) => list1 reverse_::: list2
			case _ =>
				var init = initReversed
				var res = tail
				while (init.nonEmpty) {
					res = init.head +: res
					init = init.tail
				}
				res
	}

	def reverse[A](items :IterableOnce[A]) :IterableOnce[A] = items match {
		case seq     :ReversedSeq[A]                               => seq.reverse
//		case ranking :ReversedRanking[A]                           => ranking.reverse
		case seq     :collection.IndexedSeqOps[A, generic.Any1, _] => if (seq.length <= 1) items else seq.reverseIterator
		case items   :Iterable[A] if items.sizeIs <= 1             => items
		case _                                                     => Iterators.reverse(items)
	}

	@inline def validateArraySize(length :Int) =
		if (length < 0 | length > Constants.MaxArraySize)
			illegal_!("Cannot allocate an array of size" + length + ".")


	@inline def nothingToCopy(coll :IterableOnce[_], from :Int, xs :Array[_], start :Int, len :Int) :Boolean =
		len <= 0 ||
			{ val length = xs.length; length == 0 | start >= length } ||
			{ val size = coll.knownSize; size == 0 | size > 0 & from >= size }

	@inline def nothingToCopy(size :Int, from :Int, xs :Array[_], start :Int, len :Int) :Boolean =
		len <= 0 | size == 0 | from >= size || { val len = xs.length; len == 0 | start >= len }

	@inline def nothingToCopy(coll :IterableOnce[_], xs :Array[_], start :Int, len :Int) :Boolean =
		len <= 0 || { val length = xs.length; length == 0 | start >= length } || coll.knownSize == 0

	@inline def nothingToCopy(size :Int, xs :Array[_], start :Int, len :Int) :Boolean =
		len <= 0 | size == 0 || start >= len


	def rangeCheck(coll :IterableOnce[_], from :Int, xs :Array[_], start :Int, len :Int) :Boolean =
		//Can't simply delegate because from >= size may be true if size < 0
//		rangeCheck(coll.knownSize, xs, start, from, len)
		len <= 0 || {
			val size = coll.knownSize
			size == 0 | size > 0 & from >= size || {
				val length = xs.length
				start >= length || {
					if (start < 0)
						outOfBounds_!(
							s"Negative starting write index for ${errorString(coll)}.copyRangeToArray(" +
								s"${errorString(xs)}, $start, $from, $len)."
						)
					length == 0
				}
			}
		}

	def rangeCheck(size :Int, from :Int, xs :Array[_], start :Int, len :Int) :Boolean =
		len <= 0 | size == 0 | from >= size || {
			val length = xs.length
			start >= length || {
				if (start < 0)
					outOfBounds_!(
						s"Negative starting write index for copyRangeToArray(${errorString(xs)}, $start, $from, $len)" +
						s" for a collection of size $size."
					)
				length == 0
			}
		}
	@inline def rangeCheck(coll :IterableOnce[_], xs :Array[_], start :Int, len :Int) :Boolean =
		rangeCheck(coll.knownSize, xs, start, len)

	def rangeCheck(size :Int, xs :Array[_], start :Int, len :Int) :Boolean =
		len <= 0 || size == 0 || {
			val length = xs.length
			start >= length || {
				if (start < 0)
					outOfBounds_!(
						s"Negative starting write index for copyToArray(${errorString(xs)}, $start, $len)" +
							s" for a collection of size $size."
					)
				length == 0
			}
		}


	def elementsToCopy(coll :IterableOnce[_], from :Int, xs :Array[_], start :Int, max :Int) :Int = {
		var size   = -1
		if (max <= 0 | { size = coll.knownSize; size == 0 | size > 0 & from >= size })
			0
		else if (start < 0)
			outOfBounds_!(
				s"Negative starting index: ${errorString(coll)}.copyRangeToArray(${errorString(xs)}, $start, $from, $max)."
			)
		else
			math.min(math.min(max, size - math.max(0, from)), xs.length - start)
	}
	def elementsToCopy(size :Int, from :Int, xs :Array[_], start :Int, max :Int) :Int = {
		val length = xs.length
		//We could check for length == 0, but IterableOnceOps does not, and it's useful to have compatible semantics.
		if (max <= 0 | size == 0 | from >= size | start >= length)
			0
		else if (start < 0)
			outOfBounds_!(s"|$size|.copyToArray(${errorString(xs)}, $start, $max)")
		else
			math.min(math.min(max, size - math.max(0, from)), length - start)
	}
	def elementsToCopy(coll :IterableOnce[_], xs :Array[_], start :Int, max :Int) :Int = {
		val length = xs.length
		var size   = -1
		if (max <= 0 | { size = coll.knownSize; size == 0 })
			0
		else if (start < 0)
			outOfBounds_!(s"Negative starting index: ${errorString(coll)}.copyToArray(${errorString(xs)}, $start, $max)")
		else
			math.min(math.min(max, size), length - start)
	}
	def elementsToCopy(size :Int, xs :Array[_], start :Int, max :Int) :Int = {
		val length = xs.length
		if (max <= 0 | size == 0 | start >= length)
			0
		else if (start < 0)
			outOfBounds_!(s"|$size|.copyToArray(${errorString(xs)}, $start, $max)")
		else
			math.min(math.min(max, size), length - start)
	}



	def errorString(items :IterableOnce[_]) :String = {
		val size = items.knownSize
		if (size >= 0) className(items) + '|' + size + '|' else className(items)
	}
	def errorString(items :ArrayLike[_]) :String = items.className + '|' + items.asInstanceOf[Array[_]].length + '|'

	def multiDimErrorString(items :ArrayLike[_]) :String = {
		def dimensionString(array :Array[_]) :String =
			if (!array.getClass.getComponentType.isArray)
				"|" + array.length + '|'
			else if (array.length == 0)
				"|0|"
			else {
				val len1 = array.length
				val a2 = array.asInstanceOf[Array[Array[_]]]
				val len2 = a2(0).length
				var i   = 1
				while (i < len1 && a2(i).length == len2)
					i += 1
				if (i == len1)
					"|" + len1 + "*" + len2 + "|"
				else
					a2.iterator.map(dimensionString).mkString("|" + len1, "", "|")
			}
		items.className + dimensionString(items.asInstanceOf[Array[_]])
	}


}




private object Constants {
	/** Methods will try to avoid creating arrays of a greater length than this. For example, they may resort
	  * to using a `Vector` rather than an otherwise faster `ArraySeq` as buffers.
	  */
	final val ReasonableArraySizeProperty = Constants.getClass.getPackageName + ".reasonableArraySize"
	private final val DefaultReasonableArraySize = 0xffffff //16MB

	/** Methods will try to avoid creating arrays of a greater length than this. For example, they may resort
	  * to using a `Vector` rather than an otherwise faster `ArraySeq` as buffers.
	  */
	final val ReasonableArraySize = try {
		System.getProperty(ReasonableArraySizeProperty).toInt
	} catch {
		case _ :Exception => DefaultReasonableArraySize
	}

	final val MaxArraySize = Int.MaxValue - 8 //2147483645
}




private[sugar] object IndexedIterable {
	@inline def unapply[A](items :IterableOnce[A]) :Maybe[collection.IndexedSeqOps[A, generic.Any1, _]] = items match {
		case seq     :collection.IndexedSeqOps[A, generic.Any1, _] => Yes(seq)
		case ranking :Ranking[A]                                   => Yes(ranking.toIndexedSeq)
		case set     :IndexedSet[A]                                => Yes(set.toIndexedSeq)
		case slice   :ArrayIterableOnce[A] =>
			val from  = slice.startIndex
			val until = from + slice.knownSize
			Yes(ArraySlice.slice(slice.unsafeArray.castFrom[Array[_], Array[A]], from, until))
		case _ => No
	}


	object HasFastUpdate {
		def apply[X](seq :Seq[X]) :Boolean = seq.isInstanceOf[Vector[X]] || seq.sizeIs <= FastUpdateThreshold

		@inline def apply[K, V](map :Map[K, V]) :Boolean = true

		def unapply[CC[A] <: IterableOnce[A], X](items :CC[X]) :Maybe[SeqLike[X, CC, CC[X]]] = items match {
			case seq :collection.IndexedSeqOps[X, CC, CC[X]] @unchecked => seq match {
				case _ :Vector[X] =>
					Yes(IndexedSeqLike.generic[X, IndexedSeq].asInstanceOf[IndexedSeqLike[X, CC, CC[X]]])
				case _ :Fingers[X] =>
					Yes(IndexedSeqLike.generic[X, IndexedSeq].asInstanceOf[IndexedSeqLike[X, CC, CC[X]]])
				case _ if seq.length <= FastUpdateThreshold =>
					Yes(IndexedSeqLike.generic[X, IndexedSeq].asInstanceOf[IndexedSeqLike[X, CC, CC[X]]])
				case _ => No
			}
			case seq :collection.Seq[X] if seq.sizeIs <= FastUpdateThreshold =>
				Yes(SeqLike.generic[X, Seq].asInstanceOf[SeqLike[X, CC, CC[X]]])
			case ranking :Ranking[X] =>
				if (ranking.size <= FastUpdateThreshold || HasFastUpdate(ranking.toIndexedSeq))
					Yes(IndexedSeqLike.forRanking.asInstanceOf[IndexedSeqLike[X, CC, CC[X]]])
				else
					No
			case _ => No
		}

		private final val FastUpdateThreshold = 8
	}

	def updatePreferred[X](seq :Seq[X], count :Int) :Boolean =
		HasFastUpdate(seq) && { val size = seq.size; (size >> 5) > 0 && count <= size / (size >> 5) }

	def updatePreferred[K, V](map :Map[K, V], count :Int) :Boolean =
		HasFastUpdate(map) && { val size = map.size; (size >> 5) > 0 && count <= size / (size >> 5) }

	val HasFastAppend = HasFastUpdate

	object HasFastPrepend {
		def apply[X](seq :Seq[X]) :Boolean = seq.isInstanceOf[List[_]] || HasFastUpdate(seq)

		def unapply[CC[A] <: IterableOnce[A], X](items :CC[X]) :Maybe[SeqLike[X, CC, CC[X]]] = items match {
			case _ :LinearSeq[_] => Yes(SeqLike.generic[X, Seq].asInstanceOf[SeqLike[X, CC, CC[X]]])
			case _               => HasFastUpdate.unapply(items)
		}
	}

	object ApplyPreferred {
		def unapply[A](items :IterableOnce[A]) :Maybe[collection.SeqOps[A, generic.Any1, _]] = items match {
			case items :ArrayIterableOnce[A] => items match {
				case seq :ArraySliceSeqOps[A, generic.Any1, _] => Yes(seq)
				case _                                         => Yes(ArrayLikeSlice.from(items))
			}
			case seq :collection.IndexedSeqOps[A, generic.Any1, _] => items match {
				case seq :ArraySeq[A]         => Yes(seq)
				case seq :mutable.ArraySeq[A] => Yes(seq)
				case seq :ArrayBuffer[A]      => Yes(seq)
				case seq :WrappedString       => Yes(seq)
				case seq :Substring           => Yes(seq)
				case seq :IndexedSeq[A] if applyPreferredMaxLengthProperty.isDefined =>
					if (seq.length <= applyPreferredMaxLengthProperty.get.invoke(seq).asInstanceOf[Int])
						Yes(seq)
					else
						No
				case _ if seq.length <= defaultApplyPreferredMaxLength => Yes(seq)
				case ArrayLike.Slice(array, from, until)               => Yes(ArrayLike.Slice(array, from, until))
				case _                                                 => No
			}
			case seq :collection.SeqOps[A, generic.Any1, _] =>
				if (seq.sizeIs <= applyAlwaysPreferredLength) Yes(seq) else No

			case ranking :Ranking[A] if ranking.applyPreferred =>
				if (ranking.applyPreferred) Yes(ranking.toIndexedSeq) else No

			case set     :IndexedSet[A] if set.size <= defaultApplyPreferredMaxLength =>
				if (set.size <= defaultApplyPreferredMaxLength) Yes(set.toIndexedSeq) else No

			case ArrayLike.Slice(array, from, until) =>
				Yes(ArrayLike.Slice(array, from, until))
			case _ => No
		}
		@inline def apply(items :collection.SeqOps[_, generic.Any1, _]) :Boolean = applyPreferred(items)
	}

	def applyPreferred(seq :collection.SeqOps[_, generic.Any1, _]) :Boolean = seq match {
		case _ :ArrayIterableOnce[_] | _ :ArraySeq[_] | _ :mutable.ArraySeq[_] | _ :ArrayBuffer[_] => true
		case seq :IndexedSeq[_] if applyPreferredMaxLengthProperty.isDefined =>
			seq.length <= applyPreferredMaxLengthProperty.get.invoke(seq).asInstanceOf[Int]

		case seq :collection.IndexedSeqOps[_, generic.Any1, _] => seq.length <= defaultApplyPreferredMaxLength
		case _ => seq.sizeIs <= applyAlwaysPreferredLength
	}
	def applyPreferredMaxLength(seq :collection.SeqOps[_, generic.Any1, _]) :Int = seq match {
		case _ :ArraySeq[_] | _ :mutable.ArraySeq[_] | _ :ArrayBuffer[_] | _ :ArrayIterableOnce[_] => Int.MaxValue
		case indexed :IndexedSeq[_] if applyPreferredMaxLengthProperty.isDefined =>
			applyPreferredMaxLengthProperty.get.invoke(indexed).asInstanceOf[Int]
		case _ :collection.IndexedSeqOps[_, generic.Any1, _] => defaultApplyPreferredMaxLength
		case _ => applyAlwaysPreferredLength
	}
	private[this] final val applyAlwaysPreferredLength = 4
	private[this] val applyPreferredMaxLengthProperty =
		try Maybe {
			val m = classOf[IndexedSeq[_]].getMethod("applyPreferredMaxLength")
			m.setAccessible(true)
			m
		} catch {
			case _ :Exception => No
		}
}




private[sugar] trait HasFastSlice[+E] extends IterableOnce[E] with IterableOnceOps[E, IterableOnce, IterableOnce[E]]

//todo: rename to FastSlice
private object HasFastSlice {
	private[this] val IndexedSeqViewIterator        = new IndexedSeqView.Id(Vector.empty).iterator.getClass
	private[this] val IndexedSeqViewReverseIterator = new IndexedSeqView.Id(Vector.empty).reverseIterator.getClass
	private[this] val ArrayIterator                 = new Array[Any](0).iterator.getClass
	private[this] val ArrayReverseIterator          = new Array[Any](0).reverseIterator.getClass
	private[this] val VectorIterator                = Vector().iterator.getClass

	private def isIndexedIterator(itr :Iterator[_]) = {
		val cls = itr.getClass
		cls <:< IndexedSeqViewIterator || cls <:< IndexedSeqViewReverseIterator ||
			cls <:< ArrayIterator || cls <:< ArrayReverseIterator || cls <:< VectorIterator
	}
//	private[this] val VectorReverseIterator         = Vector().reverseIterator.getClass

	def hasFastDrop[A](items :IterableOnce[A]) :Boolean = apply(items)

	def apply[A](items :IterableOnce[A]) :Boolean = items match { //don't use unapply to avoid creating wrappers.
		case _ if { val size = items.knownSize; size >= 0 & size <= fastSliceSize } => true
		case _ :collection.IndexedSeqOps[A, Iterable, Iterable[A]] @unchecked => items match {
			case _ :IndexedSeqView[_] | _ :Vector[_] => true
			case _ :RelayArrayRange[_] | _ :Fingers[_] | _ :ArrayLikeSlice[_] | _ :SubstringOps[_] => true
			case _ => false
		}
		case _ :SugaredIterable[_] => items match {
			case _ :IndexedSet[_] => true
			case _ => false
		}
		case  _ :HasFastSlice[_] | _ :IndexedIterator[_] | _ :IndexedReverseIterator[_] => true
		case it :SugaredIterator[_]                                                     => it.hasFastDrop
		case it :Iterator[_]                                                            => isIndexedIterator(it)
		case _                                                                          => false
	}

	def unapply[A](items :IterableOnce[A]) :Maybe[IterableOps[A, Iterable, Iterable[A]]] =
		items match {
			case coll :Iterable[A] if coll.sizeIs <= fastSliceSize => Yes(coll)
			case _ :collection.IndexedSeqOps[A, Iterable, Iterable[A]] @unchecked => items match {
				case view    :IndexedSeqView[A]          => Yes(view)
				case vec     :Vector[A]                  => Yes(vec)
				case fingers :Fingers[A]                 => Yes(fingers)
				case pass    :RelayArray[A]              => Yes(pass.range)
				case slice   :ArrayLikeSlice[A]          => Yes(slice)
				case seq     :collection.IndexedSeq[A]   => Yes(SeqSlice(seq, 0, seq.length))
				case items   :ArrayIterableOnce[A]       => Yes(ArrayLikeSlice.from(items))
				//todo: this requires Stepper and Iterator implementations to take IndexedSeqOps, not IndexedSeq.
//						case IndexedIterable(seq)        => Yes(seq)
				case _                                   => ArrayLikeSlice.Convert.unapply(items)
			}
			case _ :SugaredIterable[_] => items match {
				case set   :IndexedSet[A]        => Yes(set)
				case rank  :Ranking[A]           => unapply(rank.toIndexedSeq)
//				case set   :StringSet            => Yes(set.asInstanceOf[Iterable[A]])
//				case map   :StringMap[_]         => Yes(map.asInstanceOf[Iterable[A]])
				case items :ArrayIterableOnce[A] => Yes(ArrayLikeSlice.from(items))
				case _                           => No
			}
			case _ => ArrayLikeSlice.Convert.unapply(items)
		}


	//todo: use it instead of pattern matching.
	//todo: rename it to fastSlice
	def quickSlice[A](items :IterableOnce[A], from :Int, until :Int) :Maybe[IterableOnce[A]] = items match {
		case it :Iterable[A]                          => quickSlice(it, from, until)
		case it :SugaredIterator[A] if it.hasFastDrop => Yes(it.strictSlice(from, until))
		case it :Iterator[_] if isIndexedIterator(it) => Yes(slice(it, from, until))
		case _ => ArrayLikeSlice.Convert(items, from, until) match {
			case Yes(slice)                           => Yes(slice)
			case _ if items.isInstanceOf[Iterator[_]] => No //Defence against infinite recursion below.
			case _                                    => quickSlice(items.iterator, from, until)
		}
	}

	def quickSlice[A](items :IterableOps[A, Iterable, Iterable[A]], from :Int, until :Int)
			:Maybe[IterableOps[A, Iterable, Iterable[A]]] =
	{
		def sliceSlice(array :ArrayLike[A], start :Int, end :Int) =
			if (from >= end - start)
				Yes(Nil)
			else {
				val from0  = math.min(math.max(from, 0), end - start)
				val until0 = math.min(math.max(until, 0), end -  start)
				Yes(ArrayLikeSlice.slice(array, start + from0, start + until0))
			}

		items match {
			case _ if until <= 0 | until <= from                                     => Yes(Nil)
			case _ if { val s = items.knownSize; s == 0 | s >= 0 & from >= s }       => Yes(Nil)
			case coll :Iterable[A] if coll.sizeIs <= fastSliceSize                   => Yes(coll.slice(from, until))
			case seq  :collection.IndexedSeqOps[A, Iterable, Iterable[A]] @unchecked => items match {
				case _ :IndexedSeqView[_] | _ :Vector[_] | _ :ArrayLikeSlice[_] | _ :Fingers[_] =>
					Yes(seq.slice(from, until))
				//There is no guarantee that a subclass won't reallocate on slice, like RelayArray can.
//				case _ :ArraySliceSeqOps[A, _, _]       => Yes(seq.slice(from, until))
				case array :RelayArray[A]               => Yes(array.range(from, until))
				case ArrayLike.Slice(array, start, end) => sliceSlice(array, start, end)
				case _     :Substring                   => Yes(items.slice(from, until))
				case seq   :collection.IndexedSeq[A]    => Yes(SeqSlice(seq, from, until))
				case _                                  => Yes(seq.view.slice(from, until))
			}
			case ArrayLike.Slice(array, start, end) => sliceSlice(array, start, end)
			case _ :SugaredIterableOps[_, _, _]     => items match {
				case set  :IndexedSet[A] => Yes(set.slice(from, until))
				//Unsafe, no bound forcing Iterable, for example in ArrayAsSeq.
				// We also don't know if it doesn't copy on slice (like a mutable collection must).
	//			case arr  :ArraySliceOps[A, Iterable, Iterable[A]] => Yes(arr.slice(from, until))
				case rank :Ranking[A]    => quickSlice(rank.toIndexedSeq, from, until)
//				case set  :StringSet     => Yes(set.slice(from, until).asInstanceOf[Iterable[A]])
//				case map  :StringMap[_]  => Yes(map.slice(from, until).asInstanceOf[Iterable[A]])
				case _                   => No
			}
			//Not Iterable.
//			case _ :HasFastSlice[_] | _ :IndexedIterator[_] =>
			case _ => No
		}
	}

	def slice[A](items :IterableOnce[A], from :Int, until :Int)
			:IterableOnce[A] with IterableOnceOps[A, IterableOnce, IterableOnce[A]] =
		items match {
			case it :Iterable[A] => quickSlice(it, from, until) match {
				case Yes(result) =>
					result
				case _ if until >= { val s = items.knownSize; if (s == -1) Int.MaxValue else s }
					&& preferDropOverIterator(items)
				=>
					it.drop(from)
				case _ =>
					slice(items.iterator, from, until)
			}
			case _ => ArrayLikeSlice.Convert(items, from, until) getOrElse slice(items.iterator, from, until)
		}

	def slice[A](itr :Iterator[A], from :Int, until :Int) :Iterator[A] = itr match {
		case sugared :SugaredIterator[A] =>
			sugared.strictSlice(from, until)
		case _ => //Sadly, default Iterator.slice, drop, take don't check if the argument is negative.
			val size = itr.knownSize
			if (until <= 0 | until <= from)
				Iterator.empty
			else if (size >= 0)
				if (from >= size) Iterator.empty
				else if (from <= 0 & until >= size) itr
				else if (from <= 0) itr.take(until)
				else if (until >= size) drop(itr, from)
				else { val res = itr.slice(from, until); res.hasNext; res }
			else
				if (from <= 0) itr.take(until)
				else { val res = itr.slice(from, until); res.hasNext; res }
	}

	def drop[A](items :IterableOnce[A], n :Int) :IterableOnce[A] with IterableOnceOps[A, IterableOnce, IterableOnce[A]] =
		items match {
			case it   :Iterable[A] if n <= 0 | items.knownSize == 0 => it
			case list :collection.LinearSeq[A]                      => list.drop(n)
			case _    :Iterable[A]                                  => slice(items, n, Int.MaxValue)
			case _                                                  => drop(items.iterator, n)
		}
	def drop[A](itr :Iterator[A], n :Int) :Iterator[A] = itr match {
		case sugared :SugaredIterator[A] =>
			sugared.strictDrop(n)
		//We default to potentially creating a lazy iterator wrapper, because we should not prevent
		// the use of an optimized drop in some implementations. There doesn't seem to be a good solution to this.
		case _ =>
			val size = itr.knownSize
			if (size >= 0 & n >= size)
				Iterator.empty
			else {
				val res = itr.iterator.drop(n); res.hasNext; res
			}
	}


	/** Fast here means use `items.drop`, not `iterator.drop`.
	  * It still may be O(n), but likely there is no way around it then.
	  */
	@inline def preferDropOverIterator(items :IterableOnce[_]) :Boolean =
		items.isInstanceOf[collection.LinearSeq[_]] || items.knownSize == 0

	private[this] val fastSliceSize :Int = 4
}




private object Defaults {
	/** For indices in range, functionally equivalent to [[collection.SeqOps.patch patch]]`(index, elems, elems.size)`.
	  * It does ''not'' however use `size` method and may be implemented in a different manner, and the index
	  * must be in `0..this.length - elems.length` range, or an [[IndexOutOfBoundsException]] is thrown,
	  * which may make it slightly more efficient than `patch`.
	  */
	/* Consider: should index be permissive in regard to the valid range? in updated it's not; in patch it is.
	 * I don't like the semantics of patch: permissive indices should result in no effect for the indices
	 * out of range, not simply truncating them. We can't however just validate before calling patch if we don't
	 * know the sizes, but we would like to use patch in case it has a more efficient implementation.
	 */
	def updatedAll[E, CC[_], C](self :IterableOps[E, CC, C], index :Int, elems :IterableOnce[E]) :CC[E] = {
		def outOfBounds(msg :String = "") =
			outOfBounds_!(
				errorString(self) + ".updatedAll(" + index + ", " + errorString(elems) + ")" +
					(if (msg.nonEmpty) ": " + msg else msg)
			)
		val thatSize = elems.knownSize
		val thisSize = self.knownSize
		self match {
			case _ if index < 0 || thisSize >= 0 & index > thisSize - math.max(thatSize, 0) =>
				outOfBounds()
			case _ :View[_] =>
				self.iterableFactory from Views.updatedAll(self.iterator, index, elems)
			case _ if thatSize == 0 & thisSize >= 0 & index <= thisSize =>
				//Default patch implementation doesn't check if the operation results in no changes.
				self.iterableFactory from self
			case seq :collection.SeqOps[E, CC, C] if thisSize >= 0 & thatSize >= 0 =>
				seq.patch(index, elems, thatSize)
			case _ if !self.knownStrict        =>
				self.iterableFactory from Iterators.updatedAll(self.iterator, index, elems)
			case _ if elems.toBasicOps.isEmpty =>
				//Traversing the whole list to compute its length only to throw an exception is wasteful.
//				val length = self.length
//				if (index > length)
//					outOfBounds_!(index, length)
				self.iterableFactory from self
			//Not collection.LinearSeq because we want to reuse the tail.
			case seq :LinearSeq[E] @unchecked  =>
				updatedAll(seq, index, elems, true, self.iterableFactory)
			case HasFastSlice(items)           =>
				updatedAll(items, index, elems, true, self.iterableFactory)
			case _                             =>
				self.iterableFactory from Iterators.updatedAll(self.iterator, index, elems)
		}
	}

	private def updatedAll[E, CC[_], C](seq :LinearSeq[E], index :Int, elems :IterableOnce[E],
	                                    validate :Boolean, factory :IterableFactory[CC]) :CC[E] =
	{
		//We hope for fast tail, that hd +: tail reuses tail, and that iterableFactory from seq eq seq.
		val thisSize = seq.knownSize
		val thatSize = elems.knownSize
		var i = 0
		var initReversed :List[E] = Nil
		var tail :collection.LinearSeq[E] = seq
		while (i < index && tail.nonEmpty) {       //Move the seq prefix from before the patch aside, reversing it.
			initReversed = tail.head::initReversed
			tail = tail.tail
			i += 1
		}
		if (i < index)
			if (validate)
				outOfBounds_!(
					seq.className + "|" + (if (thisSize >= 0) thisSize.toString else index.toString + "+")
						+ "|.updatedAll(" + index + ", " + errorString(elems) + ")"
				)
			else
				factory from seq
		else {
			elems match {
				case list :collection.LinearSeq[E] =>
					var patch = list
					i = 0
					while (i < -index) {
						patch = patch.tail
						i += 1
					}
					while (patch.nonEmpty && tail.nonEmpty) { //Reverse patch, prepending it to our reversed prefix.
						initReversed = patch.head::initReversed
						patch = patch.tail
						tail = tail.tail
						i += 1
					}
					if (validate && tail.isEmpty && patch.nonEmpty)
						outOfBounds_!(
							seq.className + ".updatedAll(" + index + ", " + elems.className + "|" +
								(if (thatSize >= 0) thatSize else i.toString + "+") + "|): patch too large"
						)
				case IndexedIterable(seq) => //matches only collections of known size
					val dstIdx = math.max(index, 0)
					i = dstIdx
					while (i < thatSize & tail.nonEmpty) { //Determine the length of the patch we should use.
						i   += 1
						tail = tail.tail
					}
					if (validate && tail.isEmpty)
						outOfBounds_!(
							seq.className + ".updatedAll(" + index + ", " + errorString(elems) + ": patch too large"
						)
					if (!applyPreferred(seq)) {
						var itr = seq.reverseIterator
						if (hasFastDrop(itr)) {
							itr = itr.drop(thatSize - i)
							while (i > dstIdx) {
								i   -= 1
								tail = itr.next() +: tail
							}
						}
					}
					//The loop will be entered only if we haven't already prepended patch in the preceding if block.
					while (i > dstIdx) {
						i -= 1
						tail = seq(i) +: tail
					}
				case _ =>
					val itr = HasFastSlice.drop(elems.iterator, math.max(0, -index))
					while (itr.hasNext && tail.nonEmpty) {
						initReversed = itr.next()::initReversed
						tail = tail.tail
						i += 1
					}
					if (validate && tail.isEmpty && itr.hasNext)
						outOfBounds_!(
							seq.className + "|" + i + "|.updatedAll(" + index + ", " + errorString(elems) +
								"|): patch too large"
						)
			}
			factory from util.prependReverse(initReversed, tail)
		}
	}

	//Called when self has fast slicing
	private def updatedAll[E, CC[_], C](self :collection.IterableOps[E, IterableOnce, IterableOnce[E]], index :Int,
	                                    elems :IterableOnce[E], validate :Boolean, factory :IterableFactory[CC]) :CC[E] =
	{
		def outOfBounds(elemsSize :Int) =
			outOfBounds_!(
				errorString(self) + ".updatedAll(" + index + ", " + util.className(elems) + "|" + elemsSize + "|)"
			)
		def assertExhausted(thisSize :Int, thatItr :Iterator[E]) :Unit =
			if (validate && thatItr.hasNext)
				outOfBounds_!(
					errorString(self) + ".updatedAll(" + index + ", " + errorString(elems) +
						" (size >= " + (thisSize - math.max(0, index)) + ")"
				)

		//If possible, try to add collections, rather than iterators, as there is a chance they'll reuse contents.
		val thisSize = self.knownSize
		val thatSize = elems.knownSize
		val res = factory.newBuilder[E]
		res sizeHint thisSize
		res ++= self.take(index)
		if (thisSize < 0) {
			//Default Iterator.drop creates a proxy even if n <= 0, so lets check it ourselves to avoid it.
			val thatItr = HasFastSlice.drop(elems.iterator, -index)
			val thisItr = HasFastSlice.drop(self.iterator, index)
			var i = math.max(0, index)
			while (thisItr.hasNext && thatItr.hasNext) {
				res += thatItr.next()
				thisItr.next()
				i += 1
			}
			assertExhausted(i, thatItr)
			res ++= thisItr
		} else if (thatSize < 0) {
			var i       = math.max(index, 0)
			val thatItr = HasFastSlice.drop(elems.iterator, -index)
			while (i < thisSize && thatItr.hasNext) {
				res += thatItr.next()
				i += 1
			}
			assertExhausted(i, thatItr)
			if (i < thisSize)
				res ++= self.drop(i)
		} else { //thisSize >= 0 && thatSize >= 0
			if (index <= thisSize - thatSize)
				res ++= HasFastSlice.drop(elems, -index) //Smart enough to do nothing if index >= 0
			else if (validate)
				outOfBounds(thatSize)
			else
				res ++= HasFastSlice.slice(elems, -index, thisSize - index)
			if (index < thisSize - thatSize)
				res ++= self.drop(index + thatSize)
		}
		res.result()
	}


	/** For indices in range, functionally equivalent to [[collection.SeqOps.patch patch]]`(index, elems, elems.size)`.
	  * It does ''not'' however use `size` method and may be implemented in a different manner, and the index
	  * must be in `0..this.length - elems.length` range, or an [[IndexOutOfBoundsException]] is thrown,
	  * which may make it slightly more efficient than `patch`.
	  */
	/* Consider: should index be permissive in regard to the valid range? in updated it's not; in patch it is.
	 * I don't like the semantics of patch: permissive indices should result in no effect for the indices
	 * out of range, not simply truncating them. We can't however just validate before calling patch if we don't
	 * know the sizes, but we would like to use patch in case it has a more efficient implementation.
	 */
	def overwritten[E, CC[_], C](self :IterableOps[E, CC, C], index :Int, elems :IterableOnce[E]) :CC[E] = {
		val thatSize = elems.knownSize
		val thisSize = self.knownSize
		self match {
			case _ if thatSize == 0 | thisSize == 0 || index <= 0 && thatSize >= 0 && index + thatSize <= 0
				|| thisSize >= 0 && thisSize <= index || index == Int.MinValue || index == Int.MaxValue
			=>
				self.iterableFactory from self
			case _ :View[_]          =>
				self.iterableFactory from Views.overwritten(self, index, elems)
			case seq :collection.SeqOps[E, CC, C] if thatSize >= 0 & thisSize >= 0 =>
				val srcIdx   = math.max(0, -index)
				val replaced = math.min(thisSize - math.max(0, index), thatSize - srcIdx)
				val that = HasFastSlice.slice(elems, srcIdx, srcIdx + replaced)
				seq.patch(index, that, replaced)
			case _ if !self.knownStrict =>
				self.iterableFactory from Iterators.overwritten(self.iterator, index, elems)
			case _ if elems.toBasicOps.isEmpty =>
				self.iterableFactory from self
			case seq :collection.LinearSeq[E] @unchecked =>
				updatedAll(seq, index, elems, false, self.iterableFactory)
			case HasFastSlice(items) =>
				updatedAll(items, index, elems, false, self.iterableFactory)
			case _ =>
				self.iterableFactory from Iterators.overwritten(self.iterator, index, elems)
		}
	}


	/** Inserts a new element to this sequence at the specified position, pushing all elements at `index`
	  * and beyond by one position. Equivalent to
	  * [[collection.SeqOps.patch patch]]`(index, Seq(elem), 1)`.
	  */ //todo: permissive indexing
	//Consider: use patch, in case it is overridden like in a Finger tree:
	// negative indices are treated as zero, while indices greater than the length
	// of this sequence result in appending the element to the end of the sequence.
	def inserted[E, CC[_], C](self :IterableOps[E, CC, C], index :Int, elem :E) :CC[E] = {
		val size = self.knownSize
		if (index < 0 | size >= 0 & index > size)
			outOfBounds_!(errorString(self) + ".inserted(" + index + ", _)")
		self match {
			case seq :collection.SeqOps[E, CC, C] if index == 0                => seq.prepended(elem)
			case seq :collection.SeqOps[E, CC, C] if size >= 0 & index == size => seq.appended(elem)
			case _ :View[_] => self.iterableFactory from Views.inserted(self, index, elem)
			case _ => self.iterableFactory from Iterators.inserted(self.iterator, index, elem)
		}
	}

	/** Equivalent to [[collection.SeqOps.patch patch]]`(index, elems, 0)`. */
	def insertedAll[E, CC[_], C](self :IterableOps[E, CC, C], index :Int, elems :IterableOnce[E]) :CC[E] =
		self match {
			case seq :collection.SeqOps[E, CC, C] if self.knownSize >= 0 =>
				val size = self.knownSize
				if (index < 0 || index > size)
					outOfBounds_!(errorString(self) + ".insertedAll(" + index + ", " + errorString(elems) + ")")
				seq.patch(index, elems, 0)
			case _ :View[_] => self.iterableFactory from Views.insertedAll(self, index, elems)
			case _ => //Can't use patch because insertedAll validates the index.
				self.iterableFactory from Iterators.insertedAll(self.iterator, index, elems)
		}

}
