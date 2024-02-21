package net.noresttherein.sugar.collections

import java.lang.reflect.InvocationTargetException

import scala.collection.immutable.IndexedSeqDefaults.defaultApplyPreferredMaxLength
import scala.collection.immutable.{ArraySeq, IndexedSeqOps, LinearSeq, WrappedString}
import scala.collection.{IndexedSeqView, IterableFactory, IterableOnceOps, IterableOps, View, mutable}
import scala.collection.mutable.{ArrayBuffer, Builder}

import net.noresttherein.sugar.arrays.ArrayLike
import net.noresttherein.sugar.casting.castingMethods
import net.noresttherein.sugar.collections.HasFastSlice.preferDropOverIterator
import net.noresttherein.sugar.collections.IndexedIterable.applyPreferred
import net.noresttherein.sugar.collections.extensions.IterableOnceExtension
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.exceptions.{??!, illegal_!, outOfBounds_!}
import net.noresttherein.sugar.reflect.extensions.classNameMethods
import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




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
	private[this] val classNameMethod = {
		val m = classOf[Iterable[Any]].getMethod("className")
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


	def prependReverse[A](initReversed :collection.LinearSeq[A], tail :collection.LinearSeq[A]) :collection.LinearSeq[A] = {
		var init = initReversed
		var res = tail
		while (init.nonEmpty) {
			res = init.head +: res
			init = init.tail
		}
		res
	}

	def reverse[A](items :IterableOnce[A]) :IterableOnce[A] = items match {
		case seq     :ReversedSeq[A]                              => seq.reverse
//		case ranking :ReversedRanking[A]                          => ranking.reverse
		case seq     :collection.IndexedSeqOps[A, generic.Any1, _] => if (seq.length <= 1) items else seq.reverseIterator
		case items   :Iterable[A] if items.sizeIs <= 1            => items
		case _                                                    => Iterators.reverse(items)
	}

	@inline def validateArraySize(length :Int) =
		if (length < 0 | length > Constants.MaxArraySize)
			illegal_!("Cannot allocate an array of size" + length + ".")

	def elementsToCopy(xs :Array[_], start :Int, max :Int, length :Int) :Int =
		if (max < 0 | length == 0 || start > xs.length)
			0
		else if (start < 0)
			outOfBounds_!(s"|$length|.copyToArray(${errorString(xs)}, $start, $max)")
		else
			math.min(math.min(max, length), xs.length - start)

	def elementsToCopy(xs :Array[_], start :Int, max :Int, from :Int, length :Int) :Int =
		if (max < 0 | length == 0 | from >= length || start > xs.length)
			0
		else if (start < 0)
			outOfBounds_!(s"|$length|.copyToArray(${errorString(xs)}, $start, $max)")
		else
			math.min(math.min(max, length - math.max(0, from)), xs.length - start)

//	def sizeSuffix(knownSize :Int) :String = if (knownSize >= 0) "|" + knownSize + "|" else ""
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
		case ranking :Ranking[A]                                  => Yes(ranking.toIndexedSeq)
		case set     :IndexedSet[A]                               => Yes(set.toIndexedSeq)
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
			case _ :List[_] => Yes(SeqLike.generic[X, Seq].asInstanceOf[SeqLike[X, CC, CC[X]]])
			case _          => HasFastUpdate.unapply(items)
		}
	}

	object ApplyPreferred {
		def unapply[A](items :IterableOnce[A]) :Maybe[collection.SeqOps[A, generic.Any1, _]] = items match {
			case _ :ArrayIterableOnce[A] => Yes(items.toBasicOps.toIndexedSeq)
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
				case _ => No
			}
			case seq :collection.SeqOps[A, generic.Any1, _] =>
				if (seq.sizeIs <= applyAlwaysPreferredLength) Yes(seq) else No
			case ranking :Ranking[A] if ranking.applyPreferred =>
				if (ranking.applyPreferred) Yes(ranking.toIndexedSeq) else No
			case set     :IndexedSet[A] if set.size <= defaultApplyPreferredMaxLength =>
				if (set.size <= defaultApplyPreferredMaxLength) Yes(set.toIndexedSeq) else No
			case _ => No
		}
		@inline def apply(items :collection.SeqOps[_, generic.Any1, _]) :Boolean = applyPreferred(items)
	}

	def applyPreferred(seq :collection.SeqOps[_, generic.Any1, _]) :Boolean = seq match {
//		case _ if { val s = seq.knownSize; s >= 0 && s <= applyAlwaysPreferredLength } => true
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




private object HasFastSlice {
	def hasFastDrop[A](items :IterableOnce[A]) :Boolean = apply(items)

	def apply[A](items :IterableOnce[A]) :Boolean = items match { //don't use unapply to avoid creating wrappers.
		case _ if { val size = items.knownSize; size >= 0 & size <= fastSliceSize } => true
		case _ :collection.IndexedSeqOps[A, Iterable, Iterable[A]] @unchecked => items match {
			case _ :IndexedSeqView[_] | _ :Vector[_] | _ :RelayArrayView[_] => true
			case _ => false
		}
		case _ :Cat[_] | _ :IndexedSet[_] | _ :StringSet | _ :StringMap[_] => true
		case _ => false
	}

	def unapply[A](items :IterableOnce[A]) :Maybe[IterableOps[A, IterableOnce, IterableOnce[A]]] =
		items match {
			case coll :Iterable[A] if coll.sizeIs <= fastSliceSize           => Yes(coll)
			case _ :collection.IndexedSeqOps[A, Iterable, Iterable[A]] @unchecked => items match {
				case view  :IndexedSeqView[A]        => Yes(view)
				case vec   :Vector[A]                => Yes(vec)
				case pass  :RelayArray[A]            => Yes(pass.temp)
				case slice :ArrayLikeSlice[A]        => Yes(slice)
				case seq   :collection.IndexedSeq[A] => Yes(new SeqSlice(seq))
//					case ArrayLike.Wrapped.Slice(array, from, until) =>
//						Yes(ArrayLikeSlice.slice(array, from, until))
				//todo: this requires Stepper and Iterator implementations to take IndexedSeqOps, not IndexedSeq.
//						case IndexedIterable(seq)            => Yes(seq)
				case _                               => No
			}
			case cat  :Cat[A]        => Yes(cat)
//			case it   :IteratorSlicing[A] if it.hasFastDrop => Yes(it)
			case set  :IndexedSet[A] => Yes(set)
			case rank :Ranking[A]    => unapply(rank.toIndexedSeq)
			case set  :StringSet     => Yes(set.asInstanceOf[Iterable[A]])
			case map  :StringMap[_]  => Yes(map.asInstanceOf[Iterable[A]])
			case _                   => No
		}

	//todo: use it instead of pattern matching.
	def quickSlice[A](items :Iterable[A], from :Int, until :Int) :Maybe[IterableOps[A, Iterable, Iterable[A]]] =
		items match {
			case _ if items.knownSize == 0 => Yes(Nil)
			case coll :Iterable[A] if coll.sizeIs <= fastSliceSize => Yes(coll.slice(from, until))
			case seq  :collection.IndexedSeqOps[A, Iterable, Iterable[A]] @unchecked => items match {
				case _ :IndexedSeqView[_] | _ :Vector[_] | _ :ArrayLikeSlice[_] => Yes(seq.slice(from, until))
				//There is no guarantee that a subclass won't reallocate on slice, like RelayArray can.
//				case _ :ArraySliceSeqOps[A, _, _]    => Yes(seq.slice(from, until))
				case array :RelayArray[A]            => Yes(array.range(from, until))
				case ArrayLike.Wrapped.Slice(array, start, end) =>
					Yes(ArrayLikeSlice.slice(array, start + from, math.min(end - until, start) + until))
				case seq   :collection.IndexedSeq[A] => Yes(SeqSlice(seq, from, until))
				case _                               => Yes(seq.view.slice(from, until))
			}
			case cat  :Cat[A]        => Yes(cat.slice(from, until))
			case set  :IndexedSet[A] => Yes(set.slice(from, until))
			//Unsafe, no bound forcing Iterable, for example in ArrayAsSeq.
			// We also don't know if it doesn't copy on slice (like a mutable collection must).
//			case arr  :ArraySliceOps[A, Iterable, Iterable[A]] => Yes(arr.slice(from, until))
			case rank :Ranking[A]    => quickSlice(rank.toIndexedSeq, from, until)
			case set  :StringSet     => Yes(set.slice(from, until).asInstanceOf[Iterable[A]])
			case map  :StringMap[_]  => Yes(map.slice(from, until).asInstanceOf[Iterable[A]])
			case _                   => No
		}
	def slice[A](items :IterableOnce[A], from :Int, until :Int) :IterableOnceOps[A, IterableOnce, IterableOnce[A]] =
		items match {
			case it :Iterable[A] => quickSlice(it, from, until) match {
				case Yes(result) => result
				case _           => items.iterator.slice(from, until)
			}
			case _ => items.iterator.slice(from, until)
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
	def updatedAll[E, CC[_], C](self :collection.SeqOps[E, CC, C], index :Int, elems :IterableOnce[E]) :CC[E] = {
		def outOfBounds(msg :String = "") =
			outOfBounds_!(
				errorString(self) + ".updatedAll(" + index + ", " + errorString(elems) + ")" +
					(if (msg.nonEmpty) ": " + msg else msg)
			)
		val thatSize = elems.knownSize
		val thisSize = self.knownSize
		self match {
			case seq :SugaredSeqOps[E, CC @unchecked, C @unchecked] =>
				seq.updatedAll(index, elems)
			case _ if index < 0 || thisSize >= 0 & index > thisSize - math.max(thatSize, 0) =>
				outOfBounds()
			case _ if self.knownLazy =>
				self.iterableFactory from Iterators.updatedAll(self.iterator, index, elems)
			case _ if thatSize == 0 || elems.toBasicOps.isEmpty =>
				//consider: traversing the whole list to compute its length only to throw an exception is wasteful
				val length = self.length
				if (index > length)
					outOfBounds_!(index, length)
				self.iterableFactory from (self :collection.SeqOps[E, CC, C])
			case HasFastSlice(items) =>
				updatedAll(items, index, elems, true, self.iterableFactory)
			case seq :collection.LinearSeq[E] @unchecked =>
				updatedAll(seq, index, elems, true, self.iterableFactory)
			case _ =>
				self.iterableFactory from Iterators.updatedAll(self.iterator, index, elems)
		}
	}

	private def updatedAll[E, CC[_], C](seq :collection.LinearSeq[E], index :Int, elems :IterableOnce[E],
	                                    validate :Boolean, factory :IterableFactory[CC]) :CC[E] =
	{
		val thisSize = seq.knownSize
		val thatSize = elems.knownSize
		var i = 0
		var initReversed :List[E] = Nil
		var tail :collection.LinearSeq[E] = seq
		while (i < index && tail.nonEmpty) {
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
					var i = 0
					while (i < -index) {
						patch = patch.tail
						i += 1
					}
					while (patch.nonEmpty && tail.nonEmpty) {
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
					tail = tail.drop(thatSize - 1)
					if (tail.isEmpty) {
						if (validate)
							outOfBounds_!(
								seq.className + ".updatedAll(" + index + ", " + errorString(elems) + ": patch too large"
							)
					} else
						tail = tail.tail
					if (applyPreferred(seq)) {
						var i = thatSize
						while (i > 0) {
							i -= 1
							tail = seq(i) +: tail
						}
					} else {
						val i = seq.reverseIterator
						while (i.hasNext)
							tail = i.next() +: tail
					}
				case _ =>
					val i = elems.iterator
					while (i.hasNext && tail.nonEmpty) {
						initReversed = i.next()::initReversed
						tail = tail.tail
					}
					if (validate && tail.isEmpty && i.hasNext)
						outOfBounds_!(
							seq.className + ".updatedAll(" + index + ", " + elems.className + "|" +
								(if (thatSize >= 0) thatSize else i.toString + "+") + "|): patch too large"
						)
			}
			factory from util.prependReverse(initReversed, tail)
		}
	}

	private def updatedAll[E, CC[_], C](self :collection.IterableOps[E, IterableOnce, IterableOnce[E]], index :Int,
	                                    elems :IterableOnce[E], validate :Boolean, factory :IterableFactory[CC]) :CC[E] =
	{
		//If possible, try to add collections, rather than iterators, as there is a chance they'll reuse contents.
		val thisSize = self.size
		val thatSize = elems.knownSize
		val res = factory.newBuilder[E]
		res sizeHint thisSize
		res ++= self.take(index)
		val toDrop =
			if (thatSize >= 0) {
				if (index < 0)
					if (preferDropOverIterator(elems))
						res ++= elems.toIterableOnceOps.drop(-index)
					else
						res ++= elems.iterator.drop(-index)
				thatSize
			} else {
				var i  = 0
				val it = elems.iterator
				while (it.hasNext) {
					res += it.next()
					i += 1
				}
				def outOfBounds() =
					outOfBounds_!(
						errorString(self) + ".updatedAll(" + index + ", " + elems.className + "|" + i + "|)"
					)
				if (validate)
					if (thisSize >= 0) {
						if (index > thisSize - i)
							outOfBounds()
					} else if (i <= Int.MaxValue - index + 1) {
						if (self.drop(index - 1 + i).toBasicOps.nonEmpty)
							outOfBounds()
					} else
						if (self.iterator.drop(index).drop(i - 1).nonEmpty)
							outOfBounds()
				i
			}
		if (index <= thisSize - toDrop)
			res ++= self.drop(index + toDrop)
		else
			res ++= self.iterator.drop(index).drop(toDrop)
		res.result()
		//we hope for fast tail, that hd +: tail reuses tail, and that iterableFactory from seq eq seq
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
	def overwritten[E, CC[_], C](self :collection.SeqOps[E, CC, C], index :Int, elems :IterableOnce[E]) :CC[E] = {
		val thatSize = elems.knownSize
		val thisSize = self.knownSize
		self match {
			case seq :SugaredSeqOps[E, CC @unchecked, C @unchecked] =>
				seq.overwritten(index, elems)
			case _ if thatSize == 0 || thisSize == 0 || index <= 0 && thatSize >= 0 && index + thatSize <= 0
				|| thisSize >= 0 && thisSize >= index || index == Int.MinValue || index == Int.MaxValue
			=>
				self.iterableFactory from self
			case _ if self.knownLazy =>
				self.iterableFactory from Iterators.overwritten(self.iterator, index, elems)
			case _ if thatSize >= 0 && thisSize >= 0 =>
				val replaced =
					if (index < 0) math.min(thatSize + index, thisSize)
					else math.min(thatSize, thisSize - index)
				val that = if (index < 0) elems.iterator.drop(-index) else elems
				self.patch(index, that, replaced)
			case _ if elems.toBasicOps.isEmpty =>
				self.iterableFactory from self
			case HasFastSlice(items) =>
				updatedAll(items, index, elems, true, self.iterableFactory)
			case seq :collection.LinearSeq[E] @unchecked =>
				updatedAll(seq, index, elems, true, self.iterableFactory)
			case _ =>
				self.iterableFactory from Iterators.overwritten(self.iterator, index, elems)
		}
	}


	/** Inserts a new element to this sequence at the specified position, pushing all elements at `index`
	  * and beyond by one position. Equivalent to
	  * [[collection.SeqOps.patch patch]]`(index, Seq(elem), elems.size)`.
	  */ //todo: permissive indexing
	//Consider: use patch, in case it is overridden like in a Finger tree:
	// negative indices are treated as zero, while indices greater than the length
	// of this sequence result in appending the element to the end of the sequence.
	def inserted[E, CC[_], C](self :collection.SeqOps[E, CC, C], index :Int, elem :E) :CC[E] = {
		val size = self.knownSize
		if (index < 0 | size >= 0 & index > size)
			outOfBounds_!(self.className + "|" + size + "|.inserted(" + index + ", _)")
		else if (index == 0)
			self.prepended(elem)
		else if (size >= 0 & index == size)
			self.appended(elem)
		else self match {
			case seq :SugaredSeqOps[E, CC @unchecked, C @unchecked] => seq.inserted(index, elem)
			case _ => self.iterableFactory from Iterators.inserted(self.iterator, index, elem)
		}
	}

	/** Equivalent to [[collection.SeqOps.patch patch]]`(index, elems, 0)`. */
	def insertedAll[E, CC[_], C](self :collection.SeqOps[E, CC, C], index :Int, elems :IterableOnce[E]) :CC[E] =
		self match {
			case seq :SugaredSeqOps[E, CC @unchecked, C @unchecked] =>
				seq.insertedAll(index, elems)
			case _ if self.knownSize >= 0 =>
				val size = self.knownSize
				if (index < 0 || index > size)
					outOfBounds_!(errorString(self) + ".insertedAll(" + index + ", " + errorString(elems) + ")")
				self.patch(index, elems, 0)
			case _ => //Can't use patch because insertedAll validates the index.
				self.iterableFactory from Iterators.insertedAll(self.iterator, index, elems)
		}

}
