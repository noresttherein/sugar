package net.noresttherein.sugar.collections

import scala.annotation.tailrec
import scala.collection.Searching.{Found, InsertionPoint, SearchResult}
import scala.collection.immutable.{IndexedSeqOps, StringView, WrappedString}
import scala.collection.mutable.Buffer
import scala.collection.{Factory, IndexedSeqView, IterableOnceOps, IterableOps, Stepper, StepperShape, WithFilter}

import net.noresttherein.sugar.JavaTypes.JStringBuilder
import net.noresttherein.sugar.collections.LikeIndexedSeq.LikeIndexedSeqBasics
import net.noresttherein.sugar.collections.LikeSet.LikeSetBasics
import net.noresttherein.sugar.extensions.IteratorExtension
import net.noresttherein.sugar.typist.<::<
import net.noresttherein.sugar.typist.kinds.Any1
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.No




/**
  * @author Marcin Mościcki
  */
trait LikeRanking[X, -Xs, +CC[_], +C] extends LikeIndexedSeq[X, Xs, CC, C] with LikeSet[X, Xs, CC, C] {
	override def contains[A >: X](elems :Xs, elem :A) :Boolean = indexOf(elems, elem) >= 0

	def replaceAll[U >: X, O](elems :Xs, index :Int, that :O)(implicit likeCollection :LikeCollection[U, O]) :CC[U]
	def insertedAll[U >: X, O](elems :Xs, index :Int, that :O)(implicit likeCollection :LikeCollection[U, O]) :CC[U]

	override def moreSpecific(elems :Xs) :Maybe[LikeRanking[X, Xs, CC, C]] = No
	override def specific(elems :Xs) :LikeRanking[X, Xs, CC, C] = moreSpecific(elems) getOrElse this
}




private[collections] sealed abstract class Rank1LikeRankings extends LikeIterableOnceSummons[LikeRanking] {
	implicit final def forOps[X, Xs <: C, CC[+A] <: IterableOnce[A], C <: CC[X]]
	                         (implicit arg :Xs <:< C, specific :C <:< CC[X] with RankingOps[X, CC, C],
	                                   generic :CC <::< Iterable) :LikeRanking[X, Xs, CC, C] =
		prototype.asInstanceOf[LikeRanking[X, Xs, CC, C]]

	private[this] val prototype = new LikeRanking.ForOps[Any, Iterable, Ranking[Any]] {
		override def toString = "LikeRanking.forOps"
		private def readResolve :AnyRef = LikeRanking.forOps[Any, Ranking[Any], Iterable, Ranking[Any]]
	}
}


@SerialVersionUID(Ver)
object LikeRanking extends Rank1LikeRankings {

/*
	def adapt[X, Xs](elems :Xs)(implicit likeRanking :LikeRanking[X, Xs, Any1, Any]) :Ranking[X] =
		new LikeCollectionAdapter[X, elems.type](elems)
//			with LikeIterableAdapter[X, elems.type, Ranking, Ranking[X]]
			with Ranking[X]
			with LikeRankingAdapter[X, elems.type, Ranking, Ranking[X]]
		{
			override val ops = likeRanking.specific(elems)
//			override def iterator :Iterator[X] = ops.iterator(elems)
		}
*/


	trait LikeRankingBasics[X, -Xs, +CC[_], +C]
		extends LikeRanking[X, Xs, CC, C] with LikeIndexedSeqBasics[X, Xs, CC, C] with LikeSetBasics[X, Xs, CC, C]



	trait FromOps[X, -Xs, +CC[+A] <: IterableOnce[A], +C <: CC[X]]
		extends LikeRanking[X, Xs, CC, C] with LikeSetBasics[X, Xs, CC, C] with LikeIterable.FromOps[X, Xs, CC, C]
	{
		override def contains[A >: X](elems :Xs, elem :A) :Boolean = toOps(elems).contains(elem)
/*
		override def containsAll[O](elems :Xs, that :O)(implicit likeCollection :LikeCollection[X, O]) :Boolean =
			likeCollection.knownSize(that) == 0 || likeCollection.forall(that)(toImpureSet(elems))

		override def subsetOf(elems :Xs, that :collection.Set[X]) :Boolean = subsetOf[collection.Set[X]](elems, that)
		override def subsetOf[O](elems :Xs, that :O)(implicit likeSet :LikeSet[X, O, Any1, _]) :Boolean =
			likeSet.containsAll(that, elems)(this)

		override def intersect(elems :Xs, that :collection.Set[X]) :C = filter(elems)(that)
		override def intersect[O](elems :Xs, that :O)(implicit likeSet :LikeSet[X, O, Any1, _]) :C =
			filter(elems)(likeSet.toImpureSet(that))

		override def diff(elems :Xs, that :collection.Set[X]) :C = filterNot(elems)(that)
		override def diff[O](elems :Xs, that :O)(implicit likeSet :LikeSet[X, O, Any1, _]) :C =
			filterNot(elems)(likeSet.toImpureSet(that))

		override def union(elems :Xs, that :IterableOnce[X]) :C = union[IterableOnce[X]](elems, that)
*/
		override def union[O](elems :Xs, that :O)(implicit likeCollection :LikeCollection[X, O]) :C =
			//todo: get rid of either union or implement it in Ranking
			util.fromSpecific(toOps(elems))(likeCollection.toIterableOnce(that))

		override def subsets(elems :Xs, len :Int) :Iterator[C] = ??? //toImpureSet(elems).subsets(len)
		override def subsets(elems :Xs) :Iterator[C] = ??? //toImpureSet(elems).subsets()


		override def apply(elems :Xs, i :Int) :X = toOps(elems)(i)

		override def findLast(elems :Xs)(p :X => Boolean) :Option[X] = toOps(elems).findLast(p)
		override def segmentLength(elems :Xs, from :Int)(p :X => Boolean) :Int = toOps(elems).segmentLength(p, from)

		override def indexOf[U >: X](elems :Xs, from :Int, elem :U) :Int = toOps(elems).indexOf(elem) match {
			case n if n >= from => n
			case _ => -1
		}
		override def indexWhere(elems :Xs, from :Int)(p :X => Boolean) :Int = toOps(elems).indexWhere(p, from)

		override def lastIndexOf[U >: X](elems :Xs, end :Int, elem :U) :Int =
			toOps(elems).indexOf(elem) match {
				case n if n <= end => n
				case _ => -1
			}
		override def lastIndexWhere(elems :Xs, end :Int)(p :X => Boolean) :Int = toOps(elems).lastIndexWhere(p, end)

		override def lastIndexOfSlice[U >: X](elems :Xs, end :Int, that :collection.Seq[U]) :Int =
			toOps(elems).indexOfSlice(that, end) match {
				case n if n <= end => n
				case _ => -1
			}
		override def indexOfSlice[U >: X](elems :Xs, from :Int, that :collection.Seq[U]) :Int =
			toOps(elems).indexOfSlice(that, from)

		override def indexOfSlice[U >: X, O](elems :Xs, from :Int, that :O)
		                                    (implicit likeSeq :LikeSeq[U, O, Any1, _]) :Int =
			indexOfSlice(elems, from, likeSeq.toImpureSeq(that))

		override def lastIndexOfSlice[U >: X, O](elems :Xs, end :Int, that :O)
		                                        (implicit likeSeq :LikeSeq[U, O, Any1, _]) :Int =
			lastIndexOfSlice(elems, end, likeSeq.toImpureSeq(that))

		override def endsWith[U >: X, O](elems :Xs, that :O)
		                                (implicit likeIterable :LikeIterable[U, O, Any1, _]) :Boolean =
			toOps(elems).endsWith(likeIterable.toIterable(that))

		override def startsWith[U >: X, O](elems :Xs, offset :Int, that :O)
		                                  (implicit likeCollection :LikeCollection[U, O]) :Boolean =
			toOps(elems).startsWith(likeCollection.toIterableOnce(that), offset)

		override def sameElements[U >: X, O](elems :Xs, that :O)
		                                    (implicit likeCollection :LikeCollection[U, O]) :Boolean =
			toOps(elems).sameElements(likeCollection.toIterableOnce(that))

		override def search[U >: X](elems :Xs, elem :U, from :Int, to :Int)(implicit ord :Ordering[U]) :SearchResult =
			toOps(elems).indexOf(elem) match {
				case n if n >= from & n < to => Found(n)
				case n if n >= 0 => InsertionPoint(n)
				case _ => toImpureSeq(elems).search(elem, from, to)
			}


		override def distinct(elems :Xs) :C = toSpecific(elems)
		override def distinctBy[A](elems :Xs)(f :X => A) :C = toOps(elems).distinctBy(f)
		override def reverse(elems :Xs) :C = toOps(elems).reverse
		override def sorted[U >: X](elems :Xs)(implicit ord :Ordering[U]) :C = toOps(elems).sorted[U]
		override def sortWith(elems :Xs)(lt :(X, X) => Boolean) :C = toOps(elems).sortWith(lt)
		override def sortBy[A](elems :Xs)(f :X => A)(implicit ord :Ordering[A]) :C = toOps(elems).sortBy(f)


		override def updated[U >: X](elems :Xs, index :Int, elem :U) :CC[U] = toOps(elems).updated(index, elem)
		override def prepended[U >: X](elems :Xs, elem :U) :CC[U] = toOps(elems).prepended(elem)
		override def appended[U >: X](elems :Xs, elem :U) :CC[U] = toOps(elems).appended(elem)

		override def updatedAll[U >: X, O](elems :Xs, index :Int, patch :O)
		                                  (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			toOps(elems).updatedAll(index, likeCollection.toIterableOnce(patch))

		override def overwritten[U >: X, O](elems :Xs, index :Int, patch :O)
		                                   (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			toOps(elems).overwritten(index, likeCollection.toIterableOnce(patch))

		override def prependedAll[U >: X, O](elems :Xs, prefix :O)
		                                    (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			toOps(elems).prependedAll(likeCollection.toIterableOnce(prefix))

//		override def concat[U >: E, O](elems :Ranking[E], suffix :O)
//		                              (implicit likeCollection :LikeCollection[U, O]) :Ranking[U] =
//			elems.concat(likeCollection.toIterableOnce(suffix))

		override def patch[U >: X, O](elems :Xs, from :Int, other :O, replaced :Int)
		                             (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			toOps(elems).patch(from, likeCollection.toIterableOnce(other), replaced)

		override def padTo[U >: X](elems :Xs, len :Int, elem :U) :CC[U] =
			if (size(elems) >= len || contains(elems, elem)) toGeneric(elems) else toOps(elems) :+ elem

		override def replaceAll[U >: X, O](elems :Xs, index :Int, that :O)
		                                  (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			toOps(elems).replaceAll(index, likeCollection.toIterableOnce(that))

		override def insertedAll[U >: X, O](elems :Xs, index :Int, that :O)
		                                   (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			toOps(elems).insertedAll(index, likeCollection.toIterableOnce(that))

		override def reverseIterator(elems :Xs) :Iterator[X] = toOps(elems).reverseIterator
		override def view(elems :Xs) :IndexedSeqView[X] = toOps(elems).view

		override def toImpureSeq(elems :Xs) :collection.Seq[X] = toOps(elems).toSeq
		override def toImpureSet(elems :Xs) :collection.Set[X] = toOps(elems).toSet
//		override def toIterable(elems :Xs) :Iterable[X] = toSpecific(elems)

		override def toOps(elems :Xs) :RankingOps[X, CC, C]
	}

	
	trait ForOps[X, CC[+A] <: Iterable[A], C <: CC[X] with RankingOps[X, CC, C]]
		extends FromOps[X, C, CC, C] with LikeIterable.ForOps[X, CC, C]
	{
		override def apply(elems :C, i :Int) :X = elems(i)
		override def toOps(elems :C) :RankingOps[X, CC, C] = elems
	}
}




private trait LikeRankingAdapter[X, Xs, CC[+A] <: IterableOnce[A], +C <: CC[X]]
	extends RankingOps[X, CC, C] with LikeIterableAdapter[X, Xs, CC, C]
{
	override val ops :LikeRanking[X, Xs, Any1, _]

	override def apply(index :Int) :X = ops(elems, index)
	override def indexOf[U >: X](elem :U) :Int = ops.indexOf(elems, elem)

//	override def updatedAll[U >: X](index :Int, elems :IterableOnce[U]) :CC[U] =
//		iterableFactory.from(iterator.updatedAll(index, elems))
//	override def replaceAll[U >: X](index :Int, elems :IterableOnce[U]) = ops.replaceAll(this.elems, index, elems)
//	override def insertedAll[U >: X](index :Int, elems :IterableOnce[U]) = ops.insertedAll(this.elems, index, elems)

	override def reverseIterator = ops.reverseIterator(elems)
	override def view :IndexedSeqView[X] = new LikeSeqAdapterView[X, Xs](elems, ops) with IndexedSeqView[X]
}