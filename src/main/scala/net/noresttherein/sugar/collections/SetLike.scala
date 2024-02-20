package net.noresttherein.sugar.collections

import scala.collection.{AbstractIterator, IterableOps}
import scala.collection.immutable.SetOps

import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.typist.<:?<




/**
  * @author Marcin Mościcki
  */
trait SetLike[E, CC[_], C] extends IterableLike[E, CC, C] {

	def contains(elems :C)(elem :E) :Boolean

	/** Tests if some element is contained in this set.
	  *
	  * This method is equivalent to `contains`. It allows sets to be interpreted as predicates.
	  * @param elem the element to test for membership.
	  * @return `true` if `elem` is contained in this set, `false` otherwise.
	  */
	@inline final def apply(elems :C)(elem :E) :Boolean = contains(elems)(elem)

	/** Tests whether this set is a subset of another set.
	  *
	  * @param that the set to test.
	  * @return `true` if this set is a subset of `that`, i.e. if
	  *         every element of this set is also an element of `that`.
	  */
	def subsetOf(elems :C)(that :collection.Set[E]) :Boolean = forall(elems)(that)
//	def subsetOf[O](elems :C)(that :O)(implicit isSet :SetLike[E, generic.Any, O]) :Boolean =

	/** An iterator over all subsets of this set of the given size.
	  * If the requested size is impossible, an empty iterator is returned.
	  *
	  * @param len the size of the subsets.
	  * @return the iterator.
	  */
	def subsets(elems :C, len :Int) :Iterator[C] = {
		if (len < 0 || len > size(elems)) Iterator.empty
		else new SubsetsItr(elems, toIndexedSeq(elems), len)
	}

	/** An iterator over all subsets of this set.
	  *
	  * @return the iterator.
	  */
	def subsets(elems :C) :Iterator[C] = new AbstractIterator[C] {
		private[this] val elms             = SetLike.this.toIndexedSeq(elems)
		private[this] var len              = 0
		private[this] var itr :Iterator[C] = Iterator.empty

		def hasNext = len <= elms.size || itr.hasNext

		def next() = {
			if (!itr.hasNext) {
				if (len > elms.size) Iterator.empty.next()
				else {
					itr = new SubsetsItr(elems, elms, len)
					len += 1
				}
			}

			itr.next()
		}
	}

	/** An Iterator including all subsets containing exactly len elements.
	  * If the elements in 'This' type is ordered, then the subsets will also be in the same order.
	  * ListSet(1,2,3).subsets => {{1},{2},{3},{1,2},{1,3},{2,3},{1,2,3}}
	  *
	  * $willForceEvaluation
	  *
	  */
	private class SubsetsItr(superset :C, elms :IndexedSeq[E], len :Int) extends AbstractIterator[C] {
		private[this] val idxs     = Array.range(0, len + 1)
		private[this] var _hasNext = true
		idxs(len) = elms.size

		def hasNext = _hasNext

		@throws[NoSuchElementException]
		def next() :C = {
			if (!hasNext) Iterator.empty.next()

			val buf = newSpecificBuilder(superset)
			idxs.slice(0, len) foreach (idx => buf += elms(idx))
			val result = buf.result()

			var i = len - 1
			while (i >= 0 && idxs(i) == idxs(i + 1) - 1) i -= 1

			if (i < 0) _hasNext = false
			else {
				idxs(i) += 1
				for (j <- (i + 1) until len)
					idxs(j) = idxs(j - 1) + 1
			}

			result
		}
	}

	/** Computes the intersection between this set and another set.
	  *
	  * @param that the set to intersect with.
	  * @return a new set consisting of all elements that are both in this
	  *         set and in the given set `that`.
	  */
	def intersect(elems :C)(that :collection.Set[E]) :C = this.filter(elems)(that)

	/** Computes the difference of this set and another set.
	  *
	  * @param that the set of elements to exclude.
	  * @return a set containing those elements of this
	  *         set that are not also contained in the given set `that`.
	  */
	def diff(elems :C)(that :collection.Set[E]) :C = this.filterNot(elems)(that)

	/** Creates a new $coll by adding all elements contained in another collection to this $coll, omitting duplicates.
	  *
	  * This method takes a collection of elements and adds all elements, omitting duplicates, into $coll.
	  *
	  * Example:
	  * {{{
	  *    scala> val a = Set(1, 2) concat Set(2, 3)
	  *    a: scala.collection.immutable.Set[Int] = Set(1, 2, 3)
	  *   }}}
	  *
	  *  @param that     the collection containing the elements to add.
	  *  @return a new $coll with the given elements added, omitting duplicates.
	  */
	def union(elems :C)(that :IterableOnce[E]): C //=
/*
		elems match {
			case optimizedSet @ (_ : Set.Set1[E] | _: Set.Set2[E] | _: Set.Set3[E] | _: Set.Set4[E]) =>
				var result = optimizedSet.asInstanceOf[SetOps[E, Set, Set[E]]]
				val it     = that.iterator
				while (it.hasNext)
					result = result + it.next()
				result.asInstanceOf[C]
			case _ => fromSpecific(elems)(super.concat(elems)(that))
		}
*/

//	override def toOps(elems :C) :collection.SetOps[E, CC, C] = ???
}






private[collections] sealed abstract class Rank1SetLike extends IterableOnceLikeSummons[SetLike] {
	implicit def forOps[E, CC[A]/* <: Iterable[A]*/, C <: /*CC[E] with*/ collection.SetOps[E, CC, C]] //:SetLike[E, CC, C] =
	                   (implicit specific :C <:< CC[E] with collection.SetOps[E, CC, C], generic :CC <:?< Iterable)
			:SetLike[E, CC, C] =
		prototype.asInstanceOf[SetLike[E, CC, C]]

	private[this] val prototype = new SetLike.ForOps[Any, Iterable, collection.Set[Any]] {
		private def readResolve :AnyRef = SetLike.forOps[Any, Iterable, collection.Set[Any]]
		override def toString = "Rank1SetLike.forOps"
	}
}

@SerialVersionUID(Ver)
object SetLike extends Rank1SetLike {
	trait ForOps[E, CC[A] <: Iterable[A], C <: CC[E] with collection.SetOps[E, CC, C]]
		extends SetLike[E, CC, C] with IterableLike.ForOps[E, CC, C]
	{
		override def contains(elems :C)(elem :E) :Boolean = elems.contains(elem)
		override def subsetOf(elems :C)(that :collection.Set[E]) :Boolean = elems.subsetOf(that)
		override def subsets(elems :C) :Iterator[C] = elems.subsets()
		override def subsets(elems :C, len :Int) :Iterator[C] = elems.subsets(len)
		override def intersect(elems :C)(that :collection.Set[E]) :C = elems.intersect(that)
		override def diff(elems :C)(that :collection.Set[E]) :C = elems.diff(that)
		override def union(elems :C)(that :IterableOnce[E]): C = elems.concat(that)
	}
}
