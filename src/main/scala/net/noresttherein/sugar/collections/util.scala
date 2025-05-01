package net.noresttherein.sugar.collections

import java.lang.reflect.InvocationTargetException

import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeqDefaults.defaultApplyPreferredMaxLength
import scala.collection.immutable.Set.{Set1, Set2, Set3, Set4}
import scala.collection.immutable.{ArraySeq, HashSet, LinearSeq, SeqOps, WrappedString}
import scala.collection.{IndexedSeqView, IterableFactory, IterableOnceOps, IterableOps, View, immutable, mutable}
import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.reflect.ClassTag

import net.noresttherein.sugar.JavaTypes.JStringBuilder
import net.noresttherein.sugar.arrays.{ArrayLike, ErasedArray, RefArray}
import net.noresttherein.sugar.casting.castingMethods
import net.noresttherein.sugar.collections.HasFastSlice.{hasFastDrop, preferDropOverIterator}
import net.noresttherein.sugar.collections.IndexedIterable.{HasFastUpdate, applyPreferred}
import net.noresttherein.sugar.collections.extensions.IterableOnceExtension
import net.noresttherein.sugar.collections.util.{HasFastReverse, errorString}
import net.noresttherein.sugar.exceptions.{illegal_!, maxSize_!, outOfBounds_!}
import net.noresttherein.sugar.extensions.{BufferFactoryExtension, ClassExtension, IteratorExtension}
import net.noresttherein.sugar.reflect.extensions.classNameMethods
import net.noresttherein.sugar.typist.kinds
import net.noresttherein.sugar.typist.kinds.Any1
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
		items.isInstanceOf[collection.SetOps[_, Any1, _]] || items.isInstanceOf[Ranking[_]] || knownEmpty(items)

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
			case _ :LinearSeq[_] | _ :Vector[_] | _ :TreeSeq[_] | _ :RelayArray[_] => true
			case _ => false
		}

		//Fixme: works only for known, final/sealed collection classes, whose linearization we know,
		// and we are sure that CC can't be some other type.
		//What prevents us from introducing the bound of IterableOnce[A] is that Defaults.reversePrependedAll
		// wants to pass CC[X] as the argument, which is unbound, because the method is delegated to
		// by SeqExtension[X, CC, C], and we want to be able to use SeqExtension[X, RefArray, ArrayAsSeq[X]].
		// The best solution is to stop using ArrayAsSeq as an adapter to enable IterableOnce extension methods
		// for arrays, and migrate all the extensions to depend on these type classes instead.
		def unapply[CC[A]/* <: IterableOnce[A]*/, X](items :CC[X]) :Maybe[LikeSeq[X, items.type, CC, CC[X]]] = items match {
			case _ :LinearSeq[_] =>
				Yes(LikeSeq.generic[X, Seq].asInstanceOf[LikeSeq[X, items.type, CC, CC[X]]])
			case _ :Vector[_] | _ :TreeSeq[_] | _ :RelayArray[_] =>
				Yes(LikeIndexedSeq.generic[X, IndexedSeq].asInstanceOf[LikeSeq[X, items.type, CC, CC[X]]])
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

	//todo: rename to FastReverse
	object HasFastReverse {
		//todo: return IterableOnceLike instead
		def unapply[A](items :IterableOnce[A]) :Maybe[IterableOnce[A]] = attemptReverse(items)

		def attemptReverse[A](items :IterableOnce[A]) :Maybe[IterableOnce[A]] = items match {
			case seq  :ReversedSeq[A]                                   => Yes(seq.reversed)
			case seq  :ReversedBuffer[A]                                => Yes(seq.reversed)
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
		case seq     :ReversedSeq[A]                       => seq.reversed
//		case ranking :ReversedRanking[A]                   => ranking.reverse
		case seq     :collection.IndexedSeqOps[A, Any1, _] =>
			if (seq.length <= 1) items else ReverseIndexedSeqIterator(seq) //seq.view.iterator doesn't implement knownSize
		case rank    :RankingOps[A, IterableOnce, IterableOnce[A]] =>
			if (rank.length <= 1) items else rank.reverseIterator
		case view    :View[A]                              => reverse(view.iterator)
		case items   :Iterable[A] if items.sizeIs <= 1     => items
		case IndexedIterable(seq)                          => seq.reverseIterator
//		case _                                             => Iterators.reverse(items)
		case _ =>
			val size = items.knownSize
			val buffer = if (size >= 0) TemporaryBuffer.ofCapacity[A](size) else TemporaryBuffer.of[A]
			items.toBasicOps.foldLeft(buffer)(_.prepend(_))
			buffer
	}



	@inline def validateArraySize(length :Int) :Unit =
		if (length < 0 | length > Constants.MaxArraySize)
			maxSize_!("Cannot allocate an array of size" + length + ".")


	@inline def nothingToCopy(coll :IterableOnce[_], from :Int, xs :Array[_], start :Int, len :Int) :Boolean =
		len <= 0 ||
			{ val length = xs.length; length == 0 | start >= length } ||
			{ val size = coll.knownSize; size == 0 | size > 0 & from >= size }

	@inline def nothingToCopy(size :Int, from :Int, xs :Array[_], start :Int, len :Int) :Boolean =
		len <= 0 | size == 0 | from >= size || { val len = xs.length; len == 0 | start >= len }

	@inline def nothingToCopy(coll :IterableOnce[_], xs :Array[_], start :Int, len :Int) :Boolean =
		len <= 0 || { val length = xs.length; length == 0 | start >= length } || coll.knownSize == 0

	@inline def nothingToCopy(size :Int, xs :Array[_], start :Int, len :Int) :Boolean =
		len <= 0 | size == 0 || start >= xs.length

	@inline def nothingToCopy(xs :Array[_], start :Int, len :Int) :Boolean =
		len <= 0 || { val length = xs.length; length == 0 | start >= length }


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
	def errorString(items :ArrayLike[_]) :String =
		if (items == null)
			"null"
		else
			items.className + '|' + items.asInstanceOf[Array[_]].length + '|'

	def errorString(string :String) :String =
		if (string == null) "null"
		else "String|" + string.length + "|"

	def multiDimErrorString(items :ArrayLike[_]) :String =
		if (items == null)
			"null"
		else {
			@tailrec def appendDimensions(dims :JStringBuilder, array :Array[_], componentType :Class[_]) :String =
				if (!componentType.isArray) {
					val name = componentType.demangledName
					val dim = if (array == null) "_" else array.length.toString
					name + '[' + dims + '*' + dim + ']'
				} else if (array == null)
					appendDimensions(dims.append("*_"), null, componentType.getComponentType)
				else if (array.length == 0)
					appendDimensions(dims.append("*0"), null, componentType.getComponentType)
				else
					appendDimensions(
						dims.append('*').append(array.length),
						array(0).asInstanceOf[Array[_]],
						componentType.getComponentType
					)
			val componentType = items.getClass.getComponentType
			val ccType = componentType.getComponentType
			if (ccType == null)
				componentType.name + '[' + items.length + ']'
			else if (items.length == 0)
				appendDimensions(new JStringBuilder("0"), null, ccType)
			else
				appendDimensions(new JStringBuilder(items.length.toString), items(0).asInstanceOf[Array[_]], ccType)
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
	@inline def unapply[A](items :IterableOnce[A]) :Maybe[collection.IndexedSeqOps[A, kinds.Any1, _]] = items match {
		case seq     :collection.IndexedSeqOps[A, kinds.Any1, _] => Yes(seq)
		case ranking :Ranking[A]                                 => Yes(ranking.toIndexedSeq)
		case set     :IndexedSet[A]                              => Yes(set.toIndexedSeq)
		case slice   :ArrayIterableOnce[A] =>
			val from  = slice.startIndex
			val until = from + slice.knownSize
			Yes(ArraySlice.slice(slice.unsafeArray.castFrom[Array[_], Array[A]], from, until))
		case _ => No
	}


	object HasFastUpdate {
		def apply[X](seq :Seq[X]) :Boolean = seq.isInstanceOf[Vector[X]] || seq.sizeIs <= FastUpdateThreshold

		@inline def apply[K, V](map :Map[K, V]) :Boolean = true

		def unapply[CC[A] <: IterableOnce[A], X](items :CC[X]) :Maybe[LikeSeq[X, items.type, CC, CC[X]]] = items match {
			case seq :collection.IndexedSeqOps[X, CC, CC[X]] @unchecked => seq match {
				case _ :Vector[X] | _ :TreeSeq[X] =>
					Yes(LikeIndexedSeq.generic[X, IndexedSeq].asInstanceOf[LikeIndexedSeq[X, items.type, CC, CC[X]]])
				case _ if seq.length <= FastUpdateThreshold =>
					Yes(LikeIndexedSeq.generic[X, IndexedSeq].asInstanceOf[LikeIndexedSeq[X, items.type, CC, CC[X]]])
				case _ => No
			}
			case seq :collection.Seq[X] if seq.sizeIs <= FastUpdateThreshold =>
				Yes(LikeSeq.generic[X, Seq].asInstanceOf[LikeSeq[X, items.type, CC, CC[X]]])
			case ranking :Ranking[X] =>
				if (ranking.size <= FastUpdateThreshold || HasFastUpdate(ranking.toIndexedSeq))
					Yes(
						LikeIndexedSeq.likeRanking[X, Ranking[X], Ranking, Ranking[X]]
						              .asInstanceOf[LikeIndexedSeq[X, items.type, CC, CC[X]]]
					)
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
		def unapply[A](items :IterableOnce[A]) :Maybe[collection.SeqOps[A, kinds.Any1, _]] = items match {
			case items :ArrayIterableOnce[A] => items match {
				case seq :ArraySliceSeqOps[A, kinds.Any1, _] => Yes(seq)
				case _                                         => Yes(ArrayLikeSlice.from(items))
			}
			case seq :collection.IndexedSeqOps[A, kinds.Any1, _] => items match {
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
			case seq :collection.SeqOps[A, kinds.Any1, _] =>
				if (seq.sizeIs <= applyAlwaysPreferredLength) Yes(seq) else No

			case ranking :Ranking[A] if ranking.applyPreferred =>
				if (ranking.applyPreferred) Yes(ranking.toIndexedSeq) else No

			case set     :IndexedSet[A] if set.size <= defaultApplyPreferredMaxLength =>
				if (set.size <= defaultApplyPreferredMaxLength) Yes(set.toIndexedSeq) else No

			case ArrayLike.Slice(array, from, until) =>
				Yes(ArrayLike.Slice(array, from, until))
			case _ => No
		}

		@inline def apply(items :collection.SeqOps[_, kinds.Any1, _]) :Boolean = applyPreferred(items)
	}

	def applyPreferred(seq :collection.SeqOps[_, kinds.Any1, _]) :Boolean = seq match {
		case _ :ArrayIterableOnce[_] | _ :ArraySeq[_] | _ :mutable.ArraySeq[_] | _ :ArrayBuffer[_] => true
		case seq :IndexedSeq[_] if applyPreferredMaxLengthProperty.isDefined =>
			seq.length <= applyPreferredMaxLengthProperty.get.invoke(seq).asInstanceOf[Int]

		case seq :collection.IndexedSeqOps[_, kinds.Any1, _] => seq.length <= defaultApplyPreferredMaxLength
		case _ => seq.sizeIs <= applyAlwaysPreferredLength
	}
	def applyPreferredMaxLength(seq :collection.SeqOps[_, kinds.Any1, _]) :Int = seq match {
		case _ :ArraySeq[_] | _ :mutable.ArraySeq[_] | _ :ArrayBuffer[_] | _ :ArrayIterableOnce[_] => Int.MaxValue
		case indexed :IndexedSeq[_] if applyPreferredMaxLengthProperty.isDefined =>
			applyPreferredMaxLengthProperty.get.invoke(indexed).asInstanceOf[Int]
		case _ :collection.IndexedSeqOps[_, kinds.Any1, _] => defaultApplyPreferredMaxLength
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
	//ArrayBuffer uses a subclass of IndexedSeqViewIterator
//	private[this] val ArrayBufferIterator           = ArrayBuffer.empty.iterator.getClass
//	private[this] val ReverseArrayBufferIterator    = ArrayBuffer.empty.iterator.getClass

	def isIndexedIterator(itr :Iterator[_]) = {
		val cls = itr.getClass //Need a subclass test at the very least for the ArrayBuffer.iterator/reverseIterator.
		cls <:< IndexedSeqViewIterator || cls <:< IndexedSeqViewReverseIterator ||
			cls <:< ArrayIterator || cls <:< ArrayReverseIterator || cls <:< VectorIterator
	}
//	private[this] val VectorReverseIterator         = Vector().reverseIterator.getClass

	def hasFastDrop[A](items :IterableOnce[A]) :Boolean = apply(items)

	def apply[A](items :IterableOnce[A]) :Boolean = items match { //don't use unapply to avoid creating wrappers.
		case _ if { val size = items.knownSize; size >= 0 & size <= fastSliceSize } => true
		case _ :collection.IndexedSeqOps[A, Iterable, Iterable[A]] @unchecked => items match {
			case _ :IndexedSeqView[_] | _ :Vector[_] => true
			case _ :RelayArrayRange[_] | _ :TreeSeq[_] | _ :ArrayLikeSlice[_] | _ :SubstringOps[_] => true
			case _ => false
		}
		case _ :SugaredIterable[_] => items match {
			case _ :IndexedSet[_] => true
			case _ => false
		}
		case  _ :HasFastSlice[_] | _ :IndexedIterator[_] | _ :ReverseIndexedIterator[_] => true
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
				case fingers :TreeSeq[A]                 => Yes(fingers)
				case pass    :RelayArray[A]              => Yes(pass.range)
				case slice   :ArrayLikeSlice[A]          => Yes(slice)
				case seq     :collection.IndexedSeq[A]   => Yes(Subseq(seq, 0, seq.length))
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
		case it :IndexedIterator[A]                   => Yes(it.slice(from, until))
//		case it :ReverseIndexedIterator[A]            => Yes(it.slice(from, until))
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
				case _ :IndexedSeqView[_] | _ :Vector[_] | _ :ArrayLikeSlice[_] | _ :TreeSeq[_] =>
					Yes(seq.slice(from, until))
				//There is no guarantee that a subclass won't reallocate on slice, like RelayArray can.
//				case _ :ArraySliceSeqOps[A, _, _]       => Yes(seq.slice(from, until))
				case array :RelayArray[A]               => Yes(array.range(from, until))
				case ArrayLike.Slice(array, start, end) => sliceSlice(array, start, end)
				case _     :Substring                   => Yes(items.slice(from, until))
				case seq   :collection.IndexedSeq[A]    => Yes(Subseq(seq, from, until))
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




/** Linear search implementation for arbitrary collections using `LikeSeq`/`LikeCollection` type classes.
  * Intended as the implementation for `SeqOps.indexOfSlice` and `SeqOps.lastIndexOfSlice`.
  * Performs basic checks for fast paths, and defaults to the Knuth-Morris-Pratt algorithm
  */
private[sugar] object KMP {

	/** A table of length `that.length + 1`, such that `table(i)` is the length of the longest proper prefix
	  * of `that` which is also a suffix of `that.take(i)`. An empty sequence does not have a proper prefix,
	  * so the first element is always `-1`, and the only proper prefix of singleton sequence is an empty sequence,
	  * so the second element is always `0`.
	  */
	private def kmpJumpTable[U, O](pattern :O)(implicit likeSeq :LikeSeq[U, O, Any1, _]) :Array[Int] =
		if (!likeSeq.isApplyFast(pattern))
			kmpJumpTable[Any, Array[Any]](likeSeq.toArray[Any](pattern))
		else {
			val len = likeSeq.size(pattern)
			val res = new Array[Int](len + 1)
			res(0) = -1
			var prefixLen = 0
			var i = 2
			while (i < len) {
				val curr = likeSeq(pattern, i)
				while (prefixLen >= 0 && likeSeq(pattern, prefixLen) != curr)
					prefixLen = res(prefixLen)
				prefixLen += 1
				res(i) = prefixLen
				i += 1
			}
			res
		}

	private def kmp[X, T, O](text :T, pattern :O, from :Int)
	               (implicit seq1 :LikeSeq[X, T, Any1, _], seq2 :LikeSeq[X, O, Any1, _]) :Int =
	{
		val jumpTable = kmpJumpTable[X, O](pattern)
		val thisSize  = seq1.size(text)
		val thatSize  = jumpTable.length - 1
		var i1 = from
		var i2 = 0
		while (i1 < thisSize & i2 < thatSize) {
			val next = seq1(text, i1)
			while (i2 > 0 && next != seq2(pattern, i2))
				i2 = jumpTable(i2)
			i2 += 1
			i1 += 1
		}
		if (i2 == thatSize) i1 - thatSize else -1
	}

	private def kmp[X, O](text :Iterator[X], pattern :O)(implicit seq :LikeSeq[X, O, Any1, _]) :Int = {
		val jumpTable = kmpJumpTable[X, O](pattern)
		val thatSize  = jumpTable.length - 1
		var i1 = 0
		var i2 = 0
		while (i2 < thatSize && text.hasNext) {
			val next = text.next()
			while (i2 > 0 && next != seq(pattern, i2))
				i2 = jumpTable(i2)
			i2 += 1
			i1 += 1
		}
		if (i2 == thatSize) i1 - thatSize else -1
	}

	@tailrec def indexOfSlice[X, T, O](text :T, pattern :O, from :Int = 0)
	                                  (implicit coll1 :LikeCollection[X, T], coll2 :LikeCollection[X, O]) :Int =
		coll2.specific(pattern) match {
			case specific2 if from < 0 =>
				indexOfSlice[X, T, pattern.type](text, pattern :pattern.type, 0)(coll1, specific2)

			case seq2 :LikeSeq[X, pattern.type, Any1, _] if seq2.isApplyFast(pattern) =>
				coll1.specific(text) match {
					case seq1 :LikeSeq[X, text.type, Any1, _] if seq1.isApplyPreferred(text) =>
						val len2 = seq2.size(pattern)
						val len1 = seq1.size(text)
						if (from > len1 - len2)
							-1
						else if (from == len1 - len2)
							if (seq1.startsWith(text, from, pattern)) from else -1
						else
							kmp[X, text.type, pattern.type](text, pattern, from)(seq1, seq2)

					case specific1 =>
						val len2 = seq2.size(pattern)
						val offset = math.max(0, from)
						specific1.knownSize(text) match {
							case -1 =>
								val iter = specific1.iterator(text).dropInPlace(offset - 1)
								if (iter.hasNext)
									if (len2 == 0)
										offset
									else if (iter.skip().hasNext) {
										val i = kmp[X, pattern.type](iter, pattern)(seq2)
										if (i >= 0) offset + i else -1
									} else
										-1
								else //text.length < offset
									-1
							case  n if n - offset == len2 =>
								val iter1 = specific1.iterator(text).dropInPlace(offset)
								if (offset == 0)
									if (specific1.corresponds[X, pattern.type](text, pattern)(_ == _)(seq2)) 0
									else -1
								else {
									val iter2 = seq2.iterator(pattern)
									if (iter1 sameElements iter2) 0 else -1
								}
							case  n if n - offset < len2 =>
								-1
							case  _ => //We know that text.length - offset > len2, so we need KMP.
								val iter = specific1.iterator(text).dropInPlace(offset)
								if (iter.hasNext)
									offset + kmp[X, pattern.type](iter, pattern)(seq2)
								else if (len2 == 0)
									offset
								else
									-1
						}
				}
			case _ =>
				indexOfSlice(text, coll2.toIRefArray(pattern))
		}


	def lastIndexOfSlice[X, T, O](text :T, pattern :O, end :Int = Int.MaxValue)
	                             (implicit coll1 :LikeSeq[X, T, Any1, _], coll2 :LikeCollection[X, O]) :Int =
		if (end < 0)
			-1
		else {
			def fastPathCheck(len1 :Int, len2 :Int) :Int =
				if (len1 == -1 | len2 == -1)
					Int.MinValue
				else {
					if (len2 == 0)
						math.min(len1, end)
					else if (len1 < len2)
						-1
					else if (end == 0 | len1 == len2)
						if (coll1.startsWith(text, 0, pattern)) end else -1
					else
						Int.MinValue
				}
			var size1 = coll1.knownSize(text)
			var size2 = coll2.knownSize(pattern)
			val index = fastPathCheck(size1, size2)
			if (index != Int.MinValue)
				return index
			val reversedPattern = {
				val b = ReverseBuilder.of[X](RefArray)
				coll2.addTo(pattern, b)
				b.result()
			}
			if (size2 < 0) {
				size2 = reversedPattern.length
				val index = fastPathCheck(size1, size2)
				if (index != Int.MinValue)
					return index
			}
			coll1 match {
				case seq1 :LikeSeq[X, T, Any1, _] if seq1.isApplyFast(text) => //reverseIterator should be fast.
					if (size1 == -1) //Shouldn't happen, but better safe than sorry.
						size1 = coll1.size(text)
					if (size1 - end < size2)
						-1
					else {
						val iter = seq1.reverseIterator(text).dropInPlace(size1 - end - size2)
						kmp(iter, reversedPattern) match {
							case -1 => -1
							case  i => size1 - i - size2
						}
					}
				case _ => //Compute size1 and create the reverse iterator ourselves.
					//Specifically MatrixBuffer as it allows huge collections and has a fast prepend.
					val max = {
						val until = end + math.min(Int.MaxValue - end, size2)
						if (size1 >= 0) math.min(size1, until) else until
					}
					val reversedText = MatrixBuffer.ofCapacity[X](max)
//					coll1.appendTo(text, ReversedBuffer(reversedText), max)
					val itr = coll1.iterator(text) //or we could wrap reversedText in ReversedBuffer
					var counter = 0
					while (counter < max && itr.hasNext) {
						counter += 1
						itr.next() +=: reversedText
					}
					fastPathCheck(counter, size2) match {
						case Int.MinValue =>
							kmp(reversedText.iterator, reversedPattern) match {
								case -1 => -1
								case  i => counter - i - size2
							}
						case i => i
					}
			}
		}
}
