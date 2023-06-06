package net.noresttherein.sugar.collections

import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeqDefaults.defaultApplyPreferredMaxLength
import scala.collection.immutable.{IndexedSeqDefaults, LinearSeq}
import scala.collection.{IndexedSeqView, IterableOnceOps, View}
import scala.collection.mutable.Builder

import net.noresttherein.sugar.??!
import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.matching.{BooleanMatchPattern, MatchPattern}
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/**
  * @author Marcin Mościcki
  */
private object util {
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

	@inline def hasFastTake(items :IterableOnce[_]) :Boolean =
		items match { //todo: do the check for our collections without requiring presence of these classes
			case _ if knownEmpty(items) => true
			//Consider: I am not sure if Iterator and View qualify as having fast take.
			// The method returns quickly, but the cost will often be hidden under iteration, which we still need to do.
			// Also, ZigZag.drop can be potentially O(n), but at least it's faster than ZigZag.iterator.drop.
			case _ :Iterator[_] | _ :IndexedSeqView[_] | _ :Vector[_] |
			     _ :PassedArray[_] | _ :ZigZag[_] | _ :AbstractArraySlice[_] => true
			case _ => false
		}
	/** Fast here means use `items.drop`, not `iterator.drop`.
	  * It still may be O(n), but likely there is no way around it then.
	  */
	@inline def hasFastDrop(items :IterableOnce[_]) :Boolean =
		items.isInstanceOf[collection.LinearSeq[_]] || hasFastTake(items)

	def fastDrop[A](items :IterableOnce[A], n :Int) :Opt[IterableOnce[A]] = items match {
		case _ if knownEmpty(items) => Got(items)
		case seq :LinearSeq[A]      => Got(seq.drop(n))
		case seq :Vector[A]         => Got(seq.drop(n))
		case seq :IndexedSeqView[A] => Got(seq.drop(n))
		case seq :PassedArray[A]    => Got(seq.window(n, seq.length))
		case seq :ZigZag[A]         => Got(seq.drop(n))
		case seq :ArraySliceSeq[A]  => Got(seq.drop(n))
		case _ => Lack
	}

	object HasFastTake {
		def unapply[A](items :IterableOnce[A]) :Opt[IterableOnceOps[A, IterableOnce, IterableOnce[A]]] =
			items match {
				case view :View[A]           => Got(view)
				case iter :Iterator[A]       => Got(iter)
				case _    :collection.IndexedSeqOps[A, IterableOnce, IterableOnce[A]] @unchecked => items match {
					case vec  :Vector[A]         => Got(vec)
					case parr :PassedArray[A]    => Got(parr)
					case slice :ArraySliceSeq[A] => Got(slice)
				}
				case zig  :ZigZag[A]         => Got(zig)
				case set  :IndexedSet[A]     => Got(set)
				case _                       => Lack
			}
	}

	object IndexedIterable {
		@inline def unapply[A](items :IterableOnce[A]) :Opt[collection.IndexedSeqOps[A, generic.Any, _]] = items match {
			case seq :collection.IndexedSeqOps[A, generic.Any, _] => Got(seq)
			case ranking :Ranking[A] => Got(ranking.toIndexedSeq)
			case set :IndexedSet[A] => Got(set.toIndexedSeq)
			case _ => Lack
		}

		val ApplyPreferred :BooleanMatchPattern[IterableOnce[_]] =
			MatchPattern.Bool.when[IterableOnce[_]]("ApplyPreferred") {
				case seq :collection.SeqOps[_, generic.Any, _] => applyPreferred(seq)
			}

		def applyPreferred(seq :collection.SeqOps[_, generic.Any, _]) :Boolean = seq match {
//			case _ if { val s = seq.knownSize; s >= 0 && s <= applyAlwaysPreferredLength } => true
			case indexed :IndexedSeq[_] if applyPreferredMaxLengthProperty.isDefined =>
				indexed.length <= applyPreferredMaxLengthProperty.get.invoke(indexed).asInstanceOf[Int]
			case indexed :collection.IndexedSeqOps[_, generic.Any, _] =>
				indexed.length <= defaultApplyPreferredMaxLength
			case _ => seq.sizeIs <= applyAlwaysPreferredLength
		}
		def applyPreferredMaxLength(seq :collection.SeqOps[_, generic.Any, _]) :Int = seq match {
			case indexed :IndexedSeq[_] if applyPreferredMaxLengthProperty.isDefined =>
				applyPreferredMaxLengthProperty.get.invoke(indexed).asInstanceOf[Int]
			case _ :collection.IndexedSeqOps[_, generic.Any, _] => defaultApplyPreferredMaxLength
//			case _ if { val s = seq.knownSize; s >= 0 && s <= applyAlwaysPreferredLength } => applyAlwaysPreferredLength
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
	def prependReverse[A](initReversed :collection.LinearSeq[A], tail :LinearSeq[A]) :LinearSeq[A] = {
		var init = initReversed
		var res = tail
		try {
			while (true) {
				res = init.head +: res
				init = init.tail
			}
		} catch {
			case _ :NoSuchElementException => res
		}
		??!
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
}






private class ComposedBuilder[-X, Y, C, +R](underlying :Builder[Y, C], mapArgs :X => Y, mapRes :C => R)
	extends Builder[X, R]
{
	override def knownSize :Int = underlying.knownSize
	override def sizeHint(size :Int) :Unit = underlying.sizeHint(size)

	override def addOne(elem :X) :this.type = { underlying addOne mapArgs(elem); this }
	override def addAll(xs :IterableOnce[X]) :this.type = {
		val size_? = knownSize
		if (size_? >= 0)
			sizeHint(xs, size_?)
		super.addAll(xs)
	}

	override def mapResult[NewTo](f :R => NewTo) :Builder[X, NewTo] =
		new ComposedBuilder(underlying, mapArgs, mapRes andThen f)

	def mapInput[Arg](f :Arg => X) :Builder[Arg, R] =
		new ComposedBuilder(underlying, f andThen mapArgs, mapRes)

	override def result() :R = mapRes(underlying.result())
	override def clear() :Unit = underlying.clear()
}
