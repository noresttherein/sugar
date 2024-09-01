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
import net.noresttherein.sugar.collections.IndexedIterable.{HasFastUpdate, applyPreferred}
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



	val HasFastAppend = HasFastUpdate

	object HasFastPrepend {
		def apply[X](seq :Seq[X]) :Boolean = seq match {
			//Technically, LinearSeq does not promise fast prepend.
			case _ :LinearSeq[_] | _ :Vector[_] | _ :Fingers[_] | _ :RelayArray[_] | _ :RelaySeq[_] => true
			case _ => false
		}

		//Unsound, the result is only good for items itself and not other CC[_] objects.
		def unapply[CC[A]/* <: IterableOnce[A]*/, X](items :CC[X]) :Maybe[SeqLike[X, CC, CC[X]]] = items match {
			case _ :LinearSeq[_] =>
				Yes(SeqLike.generic[X, Seq].asInstanceOf[SeqLike[X, CC, CC[X]]])
			case _ :Vector[_] | _ :Fingers[_] | _ :RelayArray[_] | _ :RelaySeq[_] =>
				Yes(IndexedSeqLike.generic[X, IndexedSeq].asInstanceOf[SeqLike[X, CC, CC[X]]])
			case _ =>
				No
		}
	}

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

	object HasFastReverse {
		def unapply[A](items :IterableOnce[A]) :Maybe[IterableOnce[A]] = items match {
			case seq  :ReversedSeq[A]                                   => Yes(seq.reverse)
			case it   :Iterable[A] if it.sizeIs <= 1                    => Yes(items)
			case seq  :collection.IndexedSeqOps[A, Any1, _]             => Yes(seq.reverseIterator)
			case rank :Ranking[A]                                       => Yes(rank.reverseIterator)
			case IndexedIterable(seq)                                   => Yes(seq.reverseIterator)
			case it   :Iterable[A] if it.sizeIs <= FastReverseThreshold => Yes(Iterators.reverse(it))
			case _                                                      => No
		}
		final val FastReverseThreshold = 8
	}

	def reverse[A](items :IterableOnce[A]) :IterableOnce[A] = items match {
		case seq     :ReversedSeq[A]                       => seq.reverse
//		case ranking :ReversedRanking[A]                   => ranking.reverse
		case seq     :collection.IndexedSeqOps[A, Any1, _] => if (seq.length <= 1) items else seq.reverseIterator
		case items   :Iterable[A] if items.sizeIs <= 1     => items
		case _                                             => Iterators.reverse(items)
	}



	@inline def validateArraySize(length :Int) :Unit =
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
	//todo: return IndexedSeqLike instead
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
				case _ :Vector[X] | _ :Fingers[X] =>
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
				case set   :StringSet            => Yes(set.asInstanceOf[Iterable[A]])
				case map   :StringMap[_]         => Yes(map.asInstanceOf[Iterable[A]])
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
				val res = itr.drop(n); res.hasNext; res //hasNext eagerly drops the elements
			}
	}


	/** Fast here means use `items.drop`, not `iterator.drop`.
	  * It still may be O(n), but likely there is no way around it then.
	  */
	@inline def preferDropOverIterator(items :IterableOnce[_]) :Boolean =
		items.isInstanceOf[collection.LinearSeq[_]] || items.knownSize == 0

	private[this] val fastSliceSize :Int = 4
}
