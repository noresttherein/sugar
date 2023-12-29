package net.noresttherein.sugar.collections

import java.lang.reflect.InvocationTargetException

import scala.collection.immutable.IndexedSeqDefaults.defaultApplyPreferredMaxLength
import scala.collection.immutable.{ArraySeq, IndexedSeqOps, LinearSeq, WrappedString}
import scala.collection.{IndexedSeqView, IterableFactory, IterableOnceOps, IterableOps, View, mutable}
import scala.collection.mutable.{ArrayBuffer, Builder}

import net.noresttherein.sugar.arrays.ArrayLike
import net.noresttherein.sugar.collections.extensions.IterableOnceExtension
import net.noresttherein.sugar.exceptions.??!
import net.noresttherein.sugar.extensions.{castingMethods, classNameMethods}
import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




private[noresttherein] object util {
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
		}catch {
			case e :InvocationTargetException =>
				e.getTargetException.addSuppressed(e)
				throw e.getTargetException
		}


	private[this] val newSpecificBuilderMethod = {
		val m = classOf[IterableOps[Any, Iterable, Iterable[Any]]].getMethod("newSpecificBuilder")
		m.setAccessible(true)
		m
	}
	private[this] val fromSpecificMethod = {
		val m = classOf[IterableOps[Any, Iterable, Iterable[Any]]].getMethod("fromSpecific", classOf[IterableOnce[_]])
		m.setAccessible(true)
		m
	}

	final def knownEmpty(items :IterableOnce[_]) :Boolean = {
		val size = items.knownSize
		size == 0 || size < 0 && (items match {
			case _     :View[_]     => false //because elems.isEmpty delegates to .iterator.isEmpty
			case _     :LazyList[_] => false
			case items :Iterable[_] => items.isEmpty
			case iter  :Iterator[_] => !iter.hasNext
			case _                  => false //we don't know
		})
	}

	final def knownUnique(items :IterableOnce[_]) :Boolean =
		items.isInstanceOf[collection.Set[_]] || items.isInstanceOf[Ranking[_]] || {
			val size = items.knownSize
			size <= 1 || (items match {
				case _    :View[_]     => false
				case iter :Iterable[_] => iter.isEmpty
				case iter :Iterator[_] => !iter.hasNext
				case _                 => false
			})
		}


	def prependReverse[A](initReversed :collection.LinearSeq[A], tail :LinearSeq[A]) :LinearSeq[A] = {
		var init = initReversed
		var res = tail
		try {
			while (true) {
				res = init.head +: res
				init = init.tail
			}
			??!
		} catch {
			case _ :NoSuchElementException => res
		}
	}

	def reverse[A](items :IterableOnce[A]) :IterableOnce[A] = items match {
		case seq     :ReversedSeq[A]                              => seq.reverse
//		case ranking :ReversedRanking[A]                          => ranking.reverse
		case seq     :collection.IndexedSeqOps[A, generic.Any, _] => if (seq.length <= 1) items else seq.reverseIterator
		case items   :Iterable[A] if items.sizeIs <= 1            => items
		case _                                                    => Iterators.reverse(items)
	}

	@inline def validateArraySize(length :Int) =
		if (length < 0 | length > Constants.MaxArraySize)
			throw new IllegalArgumentException("Cannot allocate an array of size" + length + ".")

	def elementsToCopy(xs :Array[_], start :Int, max :Int, length :Int) :Int =
		if (max < 0 | length == 0 || start > xs.length)
			0
		else if (start < 0)
			throw new IndexOutOfBoundsException(s"|$length|.copyToArray(${errorString(xs)}, $start, $max)")
		else
			math.min(math.min(max, length), xs.length - start)

	def elementsToCopy(xs :Array[_], start :Int, max :Int, from :Int, length :Int) :Int =
		if (max < 0 | length == 0 | from >= length || start > xs.length)
			0
		else if (start < 0)
			throw new IndexOutOfBoundsException(s"|$length|.copyToArray(${errorString(xs)}, $start, $max)")
		else
			math.min(math.min(max, length - math.max(0, from)), xs.length - start)

//	def sizeSuffix(knownSize :Int) :String = if (knownSize >= 0) "|" + knownSize + "|" else ""
	def errorString(items :IterableOnce[_]) :String = {
		val size = items.knownSize
		if (size >= 0) items.className + '|' + size + '|' else items.className
	}
	def errorString(items :ArrayLike[_]) :String = items.className + '|' + items.asInstanceOf[Array[_]].length + '|'
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

	final val MaxArraySize = 2147483645
}




private object IndexedIterable {
	@inline def unapply[A](items :IterableOnce[A]) :Opt[collection.IndexedSeqOps[A, generic.Any, _]] = items match {
		case seq     :collection.IndexedSeqOps[A, generic.Any, _] => Got(seq)
		case ranking :Ranking[A]                                  => Got(ranking.toIndexedSeq)
		case set     :IndexedSet[A]                               => Got(set.toIndexedSeq)
		case slice   :ArrayIterableOnce[A] =>
			val from  = slice.startIndex
			val until = from + slice.knownSize
			Got(ArraySlice.slice(slice.unsafeArray.castFrom[Array[_], Array[A]], from, until))
		case _ => Lack
	}


	object HasFastUpdate {
		def apply[X](seq :Seq[X]) :Boolean = seq.isInstanceOf[Vector[X]] || seq.sizeIs <= FastUpdateThreshold

		@inline def apply[K, V](map :Map[K, V]) :Boolean = true

		def unapply[CC[A] <: IterableOnce[A], X](items :CC[X]) :Opt[SeqLike[X, CC, CC[X]]] = items match {
			case seq :collection.IndexedSeqOps[X, CC, CC[X]] @unchecked => seq match {
				case _ :Vector[X] =>
					Got(IndexedSeqLike.generic[X, IndexedSeq].asInstanceOf[IndexedSeqLike[X, CC, CC[X]]])
//				case ArrayLike.Wrapped.Slice(_, from, until) =>
//					Got(IndexedSeqLike.forIndexedSeqOps.asInstanceOf[IndexedSeqLike[X, CC, CC[X]]])
				case _ if seq.length <= FastUpdateThreshold =>
					Got(IndexedSeqLike.generic[X, IndexedSeq].asInstanceOf[IndexedSeqLike[X, CC, CC[X]]])
				case _ => Lack
			}
			case seq :collection.Seq[X] if seq.sizeIs <= FastUpdateThreshold =>
				Got(SeqLike.generic[X, Seq].asInstanceOf[SeqLike[X, CC, CC[X]]])
			case ranking :Ranking[X] =>
				if (ranking.size <= FastUpdateThreshold || HasFastUpdate(ranking.toIndexedSeq))
					Got(IndexedSeqLike.forRanking.asInstanceOf[IndexedSeqLike[X, CC, CC[X]]])
				else
					Lack
			case _ => Lack
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

		def unapply[CC[A] <: IterableOnce[A], X](items :CC[X]) :Opt[SeqLike[X, CC, CC[X]]] = items match {
			case _ :List[_] => Got(SeqLike.generic[X, Seq].asInstanceOf[SeqLike[X, CC, CC[X]]])
			case _          => HasFastUpdate.unapply(items)
		}
	}

	object ApplyPreferred {
		def unapply[A](items :IterableOnce[A]) :Opt[collection.SeqOps[A, generic.Any, _]] = items match {
			case _ :ArrayIterableOnce[A] => Got(items.toBasicOps.toIndexedSeq)
			case seq :collection.IndexedSeqOps[A, generic.Any, _] => items match {
				case seq :ArraySeq[A]         => Got(seq)
				case seq :mutable.ArraySeq[A] => Got(seq)
				case seq :ArrayBuffer[A]      => Got(seq)
				case seq :WrappedString       => Got(seq)
				case seq :Substring           => Got(seq)
				case seq :IndexedSeq[A] if applyPreferredMaxLengthProperty.isDefined =>
					if (seq.length <= applyPreferredMaxLengthProperty.get.invoke(seq).asInstanceOf[Int])
						Got(seq)
					else
						Lack
				case _ if seq.length <= defaultApplyPreferredMaxLength => Got(seq)
				case _ => Lack
			}
			case seq :collection.SeqOps[A, generic.Any, _] =>
				if (seq.sizeIs <= applyAlwaysPreferredLength) Got(seq) else Lack
			case ranking :Ranking[A] if ranking.applyPreferred =>
				if (ranking.length <= defaultApplyPreferredMaxLength) Got(ranking.toIndexedSeq) else Lack
			case set     :IndexedSet[A] if set.size <= defaultApplyPreferredMaxLength =>
				if (set.size <= defaultApplyPreferredMaxLength) Got(set.toIndexedSeq) else Lack
			case _ => Lack
		}
		@inline def apply(items :collection.SeqOps[_, generic.Any, _]) :Boolean = applyPreferred(items)
	}

	def applyPreferred(seq :collection.SeqOps[_, generic.Any, _]) :Boolean = seq match {
//			case _ if { val s = seq.knownSize; s >= 0 && s <= applyAlwaysPreferredLength } => true
		case _ :ArrayIterableOnce[_] | _ :ArraySeq[_] | _ :mutable.ArraySeq[_] | _ :ArrayBuffer[_] => true
		case seq :IndexedSeq[_] if applyPreferredMaxLengthProperty.isDefined =>
			seq.length <= applyPreferredMaxLengthProperty.get.invoke(seq).asInstanceOf[Int]

		case seq :collection.IndexedSeqOps[_, generic.Any, _] => seq.length <= defaultApplyPreferredMaxLength
		case _ => seq.sizeIs <= applyAlwaysPreferredLength
	}
	def applyPreferredMaxLength(seq :collection.SeqOps[_, generic.Any, _]) :Int = seq match {
		case _ :ArraySeq[_] | _ :mutable.ArraySeq[_] | _ :ArrayBuffer[_] | _ :ArrayIterableOnce[_] => Int.MaxValue
		case indexed :IndexedSeq[_] if applyPreferredMaxLengthProperty.isDefined =>
			applyPreferredMaxLengthProperty.get.invoke(indexed).asInstanceOf[Int]
		case _ :collection.IndexedSeqOps[_, generic.Any, _] => defaultApplyPreferredMaxLength
		case _ => applyAlwaysPreferredLength
	}
	private[this] final val applyAlwaysPreferredLength = 4
	private[this] val applyPreferredMaxLengthProperty =
		try Opt {
			val m = classOf[IndexedSeq[_]].getMethod("applyPreferredMaxLength")
			m.setAccessible(true)
			m
		} catch {
			case _ :Exception => Lack
		}
}




private object HasFastSlice {
	def apply[A](items :IterableOnce[A]) :Boolean = unapply(items).isDefined

	def hasFastDrop[A](items :IterableOnce[A]) :Boolean = unapply(items).isDefined

	def unapply[A](items :IterableOnce[A]) :Opt[IterableOnceOps[A, IterableOnce, IterableOnce[A]]] =
		items match {
//				case view :View[A]       => Got(view)
//				case iter :Iterator[A]   => Got(iter)
			case _    :collection.IndexedSeqOps[A, IterableOnce, IterableOnce[A]] @unchecked => items match {
				case view  :IndexedSeqView[A]        => Got(view)
				case vec   :Vector[A]                => Got(vec)
				case pass  :PassedArray[A]           => Got(pass)
				case slice :ArrayLikeSlice[A]        => Got(slice)
				case seq   :collection.IndexedSeq[A] => Got(new SeqSlice(seq))
				case ArrayLike.Wrapped.Slice(array, from, until) => Got(ArrayLikeSlice.slice(array, from, until))
				//todo: this requires Stepper and Iterator implementations to take IndexedSeqOps, not IndexedSeq.
//						case IndexedIterable(seq)            => Got(seq)
				case _                               => Lack
			}
			case zig  :ZigZag[A]     => Got(zig)
			case set  :IndexedSet[A] => Got(set)
			case _                   => Lack
		}


	/** Fast here means use `items.drop`, not `iterator.drop`.
	  * It still may be O(n), but likely there is no way around it then.
	  */
	@inline def preferDropOverIterator(items :IterableOnce[_]) :Boolean =
		items.isInstanceOf[collection.LinearSeq[_]] || HasFastSlice(items)

}
