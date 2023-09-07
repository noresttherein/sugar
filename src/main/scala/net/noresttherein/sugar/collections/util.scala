package net.noresttherein.sugar.collections

import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeqDefaults.defaultApplyPreferredMaxLength
import scala.collection.immutable.{ArraySeq, LinearSeq}
import scala.collection.{IndexedSeqView, IterableOnceOps, IterableOps, View, mutable}
import scala.collection.mutable.{ArrayBuffer, Builder}

import net.noresttherein.sugar.collections.IndexedIterable.HasFastUpdate
import net.noresttherein.sugar.collections.extensions.IterableOnceExtension
import net.noresttherein.sugar.exceptions.??!
import net.noresttherein.sugar.extensions.classNameMethods
import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




private[noresttherein] object util {
	//These two methods are problematic because, in theory, they can be overridden by a class to throw an exception
	// and simply not used by an implementation.
	def specificBuilder[E, CC[_], C](items :IterableOps[E, CC, C]) :Builder[E, C] =
		newSpecificBuilderMethod.invoke(items).asInstanceOf[Builder[E, C]]

	def fromSpecific[E, CC[_], C](items :IterableOps[E, CC, C])(coll :IterableOnce[E]) :C =
		fromSpecificMethod.invoke(items, coll).asInstanceOf[C]

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

	@inline final def knownEmpty(items :IterableOnce[_]) :Boolean = {
		val size = items.knownSize
		size == 0 || size < 0 && (items match {
			case _     :View[_] => false //because elems.isEmpty delegates to .iterator.isEmpty
			case _     :LazyList[_] => false
			case items :Iterable[_] => items.isEmpty
			case iter  :Iterator[_] => !iter.hasNext
			case _                  => false //we don't know
		})
	}


/*
	def prependReverse[A](initReversed :IterableOnce[A], tail :LinearSeq[A]) :LinearSeq[A] =
		initReversed match {
			case _ if knownEmpty(initReversed) => tail
			case list :collection.LinearSeq[A] => prependReverse(list, tail)
			case ErasedArray.Wrapped(array) => prependReverse(array.asInstanceOf[Array[A]], 0, array.length, tail)
			case ErasedArray.Wrapped.Slice(array, from, until) =>
				prependReverse(array.asInstanceOf[Array[A]], from, until, tail)
			case seq :collection.IndexedSeqOps[A, IndexedSeq, IndexedSeq[A]] @unchecked
				if seq.length <= IndexedSeqDefaults.defaultApplyPreferredMaxLength
			=>
				prependReverse(seq, tail)
			case _ =>
				prependReverse(initReversed.iterator, tail)
		}

	def prependReverse[A](iter :Iterator[A], tail :LinearSeq[A]) :LinearSeq[A] = {
		var res = tail
		while (iter.hasNext)
			res = iter.next() +: res
		res
	}
	def prependReverse[A](seq :collection.IndexedSeqOps[A, IndexedSeq, IndexedSeq[A]], tail :LinearSeq[A])
			:LinearSeq[A] =
	{
		var i   = seq.length
		var res = tail
		while (i > 0) {
			i -= 1
			res = seq(i) +: res
		}
		res
	}
	@tailrec def prependReverse[A](initReversed :Array[A], from :Int, until :Int, tail :LinearSeq[A]) :LinearSeq[A] =
		if (until <= from)
			tail
		else {
			val i = until - 1
			prependReverse(initReversed, from, i, initReversed(i) +: tail)
		}
*/
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
/*
	@tailrec def reverseIterator[A](items :IterableOnce[A]) :Iterator[A] = items match {
		case view :View[A] => reverseIterator(view.iterator)
		case it   :Iterable[A] if it.sizeIs <= 1 => it.iterator
		case it   :Iterator[A] if !it.hasNext    => it
//		case ErasedArray.Wrapped(array :Array[A] @unchecked) => ReverseArrayIterator(array)
		case ErasedArray.Wrapped.Slice(array :Array[A @unchecked], from :Int, until :Int) =>
			ReverseArrayIterator.over(array, from, until)
		case IndexedIterable(seq) => seq.reverseIterator
		case _ =>
			val size = items.knownSize
			if (size >= 0) (DefaultBuffer.ofCapacity[A](size) ++= items).reverseIterator
			else (DefaultBuffer.of[A] ++= items).reverseIterator
	}
*/
/*

	@tailrec def reverseSeq[A](items :IterableOnce[A]) :collection.Seq[A] = items match {
		case view :View[A]                       => reverse(view.iterator)
		case it   :Iterable[A] if it.sizeIs <= 1 => it.toSeq
		case it   :Iterator[A] if !it.hasNext    => Seq.empty
		case ApplyPreferred(items) =>
		case _ =>
		val thatSize = items.knownSize
		if (thatSize == 0)
			Seq.empty
		else if (thatSize == 1)
			items.toIterableOnceOps.toSeq
		else
		val it   = suffix.iterator
		var size = 0

		val reversed = suffix.knownSize match {
			//todo: replace with MatrixBuffer
			case newElemCount if newElemCount >= 0 && newElemCount <= ReasonableArraySize => //reverse in an array
				size = newElemCount
				val res = ErasedArray.ofDim[U](newElemCount)
				var i   = newElemCount
				while (i > 0) {
					i -= 1;
					res(i) = it.next()
				}
				DefaultArraySeq.of(res.asInstanceOf[IArray[U]])
			case _ => //reverse in a list
				var reversed :List[U] = Nil
				while (it.hasNext) {
					reversed = it.next() :: reversed
					size += 1
				}
				reversed
		}

	}
*/

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
		case seq :collection.IndexedSeqOps[A, generic.Any, _] => Got(seq)
		case ranking :Ranking[A] => Got(ranking.toIndexedSeq)
		case set :IndexedSet[A] => Got(set.toIndexedSeq)
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
//		val ApplyPreferred :BooleanMatchPattern[IterableOnce[_]] =
//			MatchPattern.Bool.when[IterableOnce[_]]("ApplyPreferred") {
//				case seq :collection.SeqOps[_, generic.Any, _] => applyPreferred(seq)
//			}
	object ApplyPreferred {
		def unapply[A](items :IterableOnce[A]) :Opt[collection.SeqOps[A, generic.Any, _]] = items match {
			case _ :AbstractArraySlice[A] => Got(items.toBasicOps.toIndexedSeq)
			case seq :collection.IndexedSeqOps[A, generic.Any, _] => items match {
				case seq :ArraySeq[A]         => Got(seq)
				case seq :mutable.ArraySeq[A] => Got(seq)
				case seq :ArrayBuffer[A]      => Got(seq)
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
		case _ :AbstractArraySlice[_] | _ :ArraySeq[_] | _ :mutable.ArraySeq[_] | _ :ArrayBuffer[_] => true
		case seq :IndexedSeq[_] if applyPreferredMaxLengthProperty.isDefined =>
			seq.length <= applyPreferredMaxLengthProperty.get.invoke(seq).asInstanceOf[Int]

		case seq :collection.IndexedSeqOps[_, generic.Any, _] => seq.length <= defaultApplyPreferredMaxLength
		case _ => seq.sizeIs <= applyAlwaysPreferredLength
	}
	def applyPreferredMaxLength(seq :collection.SeqOps[_, generic.Any, _]) :Int = seq match {
		case _ :ArraySeq[_] | _ :mutable.ArraySeq[_] | _ :ArrayBuffer[_] | _ :AbstractArraySlice[_] => Int.MaxValue
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
/*
	def unapply[A, As](items :As)(implicit values :Values[A, As])
			:Opt[IterableOnceOps[A, IterableOnce, IterableOnce[A]]] =
		//fixme: this matches A =:= As =:= Iterable[X]
		items match {
			case it :Iterable[A @unchecked] if values.toIterableOnce(items).asAnyRef eq it => it match {
				case _ :collection.IndexedSeqOps[A, IterableOnce, IterableOnce[A]] @unchecked => it match {
					case view  :IndexedSeqView[A]        => Got(view)
					case vec   :Vector[A]                => Got(vec)
					case pass  :PassedArray[A]           => Got(pass.window)
					case slice :ArrayLikeSlice[A]        => Got(slice)
					case seq   :collection.IndexedSeq[A] => Got(new SeqSlice(seq))
					case ArrayLike.Wrapped.Slice(array, from, until) =>
						Got(ArrayLikeSlice.of(array, from, until))
					//todo: this requires Stepper and Iterator implementations to take IndexedSeqOps, not IndexedSeq.
//						case IndexedIterable(seq)            => Got(seq)
					case _                        => Lack
				}
//					case zig :ZigZag[A]     => Got(zig)
				case set :IndexedSet[A] => Got(set)
				case _                  => Lack
			}
			case _ => Lack
		}
*/

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
				case ArrayLike.Wrapped.Slice(array, from, until) => Got(ArrayLikeSlice.of(array, from, until))
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

/*
	def fastDrop[A](items :IterableOnce[A], n :Int) :Opt[IterableOnce[A]] = items match {
		case _ if util.knownEmpty(items)   => Got(items)
		case seq :collection.LinearSeq[A]  => Got(seq.drop(n))
		case seq :Vector[A]                => Got(seq.drop(n))
		case seq :IndexedSeqView[A]        => Got(seq.drop(n))
		case seq :PassedArray[A]           => Got(seq.window(n, seq.length))
		case seq :ArrayLikeSlice[A]        => Got(seq.drop(n))
		case seq :collection.IndexedSeq[A] => Got(SeqSlice(seq, n, seq.length))
		case seq :ZigZag[A]                => Got(seq.drop(n))
		case _ => Lack
	}
*/

}
//
//
//
//private object HasFastAppend {
//	def apply[E, CC[_], C](seq :collection.SeqOps[E, CC, C]) :Boolean =
//		seq.isInstanceOf[Vector[_]]
//
//	def unapply[E, CC[_]](items :CC[E]) :Opt[SeqLike[E, CC, CC[E]]] = items match {
//		case seq :collection.IndexedSeqOps[E, CC, CC[E]] @unchecked => items match {
//			case _ :Vector[_] =>
//				Got(IndexedSeqLike.forIndexedSeqOps.asInstanceOf[IndexedSeqLike[E, CC, CC[E]]])
//			case ArrayLike.Wrapped.Slice(a, from, until) =>
//				Got(IndexedSeqLike.forIndexedSeqOps.asInstanceOf[IndexedSeqLike[E, CC, CC[E]]])
//			case _ if HasFastUpdate(seq) =>
//				Got(IndexedSeqLike.forIndexedSeqOps.asInstanceOf[IndexedSeqLike[E, CC, CC[E]]])
//			case _ => Lack
//		}
//		case seq :List[E] if HasFastUpdate(seq) =>
//				Got(SeqLike.forSeqOps.asInstanceOf[SeqLike[E, CC, CC[E]]])
//		case ranking :Ranking[E] =>
//
//	}
//}




private object BinarySearch {

	@tailrec def apply(array :Array[Byte], from :Int, until :Int, key :Byte) :Int =
		if (until == from) {
			val candidate = array(from)
			if (key == candidate) from
			else if (key < candidate) -from - 1
			else -from - 2
		} else {
			val middle = from + (until - from >> 1)
			if (key <= array(middle)) apply(array, from, middle, key)
			else apply(array, middle + 1, until, key)
		}

	@tailrec def apply(array :Array[Short], from :Int, until :Int, key :Short) :Int =
		if (until == from) {
			val candidate = array(from)
			if (key == candidate) from
			else if (key < candidate) -from - 1
			else -from - 2
		} else {
			val middle = from + (until - from >> 1)
			if (key <= array(middle)) apply(array, from, middle, key)
			else apply(array, middle + 1, until, key)
		}

	@tailrec def apply(array :Array[Char], from :Int, until :Int, key :Char) :Int =
		if (until == from) {
			val candidate = array(from)
			if (key == candidate) from
			else if (key < candidate) -from - 1
			else -from - 2
		} else {
			val middle = from + (until - from >> 1)
			if (key <= array(middle)) apply(array, from, middle, key)
			else apply(array, middle + 1, until, key)
		}

	@tailrec def apply(array :Array[Int], from :Int, until :Int, key :Int) :Int =
		if (until == from) {
			val candidate = array(from)
			if (key == candidate) from
			else if (key < candidate) -from - 1
			else -from - 2
		} else {
			val middle = from + (until - from >> 1)
			if (key <= array(middle)) apply(array, from, middle, key)
			else apply(array, middle + 1, until, key)
		}

	@tailrec def apply(array :Array[Long], from :Int, until :Int, key :Long) :Int =
		if (until == from) {
			val candidate = array(from)
			if (key == candidate) from
			else if (key < candidate) -from - 1
			else -from - 2
		} else {
			val middle = from + (until - from >> 1)
			if (key <= array(middle)) apply(array, from, middle, key)
			else apply(array, middle + 1, until, key)
		}

	@tailrec def apply(array :Array[Float], from :Int, until :Int, key :Float) :Int =
		if (until == from) {
			val candidate = array(from)
			if (key == candidate) from
			else if (key < candidate) -from - 1
			else -from - 2
		} else {
			val middle = from + (until - from >> 1)
			if (key <= array(middle)) apply(array, from, middle, key)
			else apply(array, middle + 1, until, key)
		}

	@tailrec def apply(array :Array[Double], from :Int, until :Int, key :Double) :Int =
		if (until == from) {
			val candidate = array(from)
			if (key == candidate) from
			else if (key < candidate) -from - 1
			else -from - 2
		} else {
			val middle = from + (until - from >> 1)
			if (key <= array(middle)) apply(array, from, middle, key)
			else apply(array, middle + 1, until, key)
		}

	@tailrec def apply(array :Array[AnyRef], from :Int, until :Int, key :AnyRef)(implicit ordering :Ordering[AnyRef]) :Int =
		if (until == from)
			math.signum(ordering.compare(key, array(from))) match {
				case  0 => from
				case -1 => -from - 1
				case  1 => -from - 2
			}
		else {
			val middle = from + (until - from >> 1)
			if (ordering.lteq(key, array(middle))) apply(array, from, middle, key)
			else apply(array, middle + 1, until, key)
		}

	@tailrec def apply[X, U >: X](array :Array[X], from :Int, until :Int, key :U)(implicit ordering :Ordering[U]) :Int =
		if (until == from)
			math.signum(ordering.compare(key, array(from))) match {
				case  0 => from
				case -1 => -from - 1
				case  1 => -from - 2
			}
		else {
			val middle = from + (until - from >> 1)
			if (ordering.lteq(key, array(middle))) apply(array, from, middle, key)
			else apply(array, middle + 1, until, key)
		}

}
