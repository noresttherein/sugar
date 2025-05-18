package net.noresttherein.sugar.collections

import net.noresttherein.sugar.collections.LikeIterable.LikeIterableBasics
import net.noresttherein.sugar.collections.LikeIterableOnce.Generic.Template
import net.noresttherein.sugar.typist.kinds
import net.noresttherein.sugar.typist.<::<
import net.noresttherein.sugar.typist.kinds.Any1
import net.noresttherein.sugar.util.SerializableSingleton
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




/**
  * @author Marcin Mościcki
  */
trait LikeSet[X, -Xs, +CC[_], +C] extends LikeIterable[X, Xs, CC, C] {

	/** Tests if some element is contained in this set.
	  *
	  * This method is equivalent to `contains`. It allows sets to be interpreted as predicates.
	  * @param elem the element to test for membership.
	  * @return `true` if `elem` is contained in this set, `false` otherwise.
	  */ //The artificial type parameter is to resolve the conflict with LikeSeq.contains in LikeRanking
	def contains[A >: X <: X](elems :Xs, elem :A) :Boolean

	def containsAll[O](elems :Xs, that :O)(implicit likeCollection :LikeCollection[X, O]) :Boolean

	/** Tests whether this set is a subset of another set.
	  *
	  * @param that the set to test.
	  * @return `true` if this set is a subset of `that`, i.e. if
	  *         every element of this set is also an element of `that`.
	  */
	def subsetOf[O](elems :Xs, that :O)(implicit likeSet :LikeSet[X, O, kinds.Any1, _]) :Boolean

	def subsetOf(elems :Xs, that :collection.Set[X]) :Boolean

	/** Computes the intersection between this set and another set.
	  *
	  * @param that the set to intersect with.
	  * @return a new set consisting of all elements that are both in this
	  *         set and in the given set `that`.
	  */
	def intersect[O](elems :Xs, that :O)(implicit likeSet :LikeSet[X, O, kinds.Any1, _]) :C

	def intersect(elems :Xs, that :collection.Set[X]) :C

	/** Computes the difference of this set and another set.
	  *
	  * @param that the set of elements to exclude.
	  * @return a set containing those elements of this
	  *         set that are not also contained in the given set `that`.
	  */
	def diff[O](elems :Xs, that :O)(implicit likeSet :LikeSet[X, O, kinds.Any1, _]) :C

	def diff(elems :Xs, that :collection.Set[X]) :C

	def union[O](elems :Xs, that :O)(implicit likeCollection :LikeCollection[X, O]) :C

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
	def union(elems :Xs, that :IterableOnce[X]) :C

	//Candidates for removal
	/** An iterator over all subsets of this set of the given size.
	  * If the requested size is impossible, an empty iterator is returned.
	  *
	  * @param len the size of the subsets.
	  * @return the iterator.
	  */
	def subsets(elems :Xs, len :Int) :Iterator[C]

	/** An iterator over all subsets of this set.
	  *
	  * @return the iterator.
	  */
	def subsets(elems :Xs) :Iterator[C]

	def toImpureSet(elems :Xs) :collection.Set[X]

//	override def toOps(elems :C) :collection.SetOps[X, CC, C] = ???

	override def moreSpecific(elems :Xs) :Maybe[LikeSet[X, elems.type, CC, C]] = No
	override def specific(elems :Xs) :LikeSet[X, elems.type, CC, C] = moreSpecific(elems) getOrElse this
}






/** @define TypeClass `LikeSet` */
private[collections] sealed abstract class Rank2LikeSets extends LikeIterableOnceSummons[LikeSet] {
	@inline implicit final def likeGeneric[X, Xs <: CC[X], CC[_], C >: CC[X]]
	                                      (implicit generic :Generic[CC]) :LikeSet[X, Xs, CC, C] =
		generic.of
}


private[collections] sealed abstract class Rank1LikeSets extends Rank2LikeSets {
	//Type parameter Xs for consistency with other LikeIterable classes.
	implicit final def forOps[X, Xs, CC[_], C <: collection.SetOps[X, CC, C]]
	                         (implicit arg :Xs <:< C, specific :C <:< CC[X] with collection.SetOps[X, CC, C],
	                                   generic :CC <::< Iterable) :LikeSet[X, Xs, CC, C] =
		prototype.asInstanceOf[LikeSet[X, Xs, CC, C]]

	private[this] val prototype = new LikeSet.ForOps[Any, Iterable, collection.Set[Any]] {
		//todo: moreSpecific handling ranking.
		override def toString = "LikeSet.forOps"
		private def readResolve :AnyRef = LikeSet.forOps[Any, collection.Set[Any], Iterable, collection.Set[Any]]
	}

/*
	implicit def forRanking[X, CC[+_], C <: RankingOps[X, CC, C]]
	                       (implicit specific :C <:< CC[X] with RankingOps[X, CC, C], generic :CC <::< Ranking)
			:LikeSet[X, C, CC, C] =
		rankingPrototype.asInstanceOf[LikeSet[X, C, CC, C]]

	private[this] val rankingPrototype = new LikeSet.ForRanking[Any, Ranking, Ranking[Any]] {
		override def toString = "LikeSet.forRanking"
		private def readResolve :AnyRef = LikeSet.forRanking[Any, Ranking, Ranking[Any]]
	}
*/
}


@SerialVersionUID(Ver)
object LikeSet extends Rank1LikeSets {

	@inline implicit def likeRanking[X, Xs, CC[_], C](implicit like :LikeRanking[X, Xs, CC, C]) :LikeSet[X, Xs, CC, C] =
		like

	def adapt[X, Xs](elems :Xs)(implicit likeSet :LikeSet[X, Xs, Any1, Any]) :collection.Set[X] =
		new LikeCollectionAdapter[X, elems.type](elems)
			with collection.Set[X] with LikeSetAdapter[X, elems.type, collection.Set, collection.Set[X]]
		{
			override val ops = likeSet.specific(elems)
			override def diff(that :collection.Set[X]) :collection.Set[X] = filterNot(that)
		}


	trait Generic[CC[_]] extends LikeIterable.Generic[CC] with Template[CC, LikeSet]

	@SerialVersionUID(Ver)
	object Generic extends Rank1Generics {
		@inline implicit def likeRanking[CC[_]](implicit generic :LikeRanking.Generic[CC]) :Generic[CC] = generic
	}

	private[LikeSet] sealed abstract class Rank1Generics {
		implicit final def forOps[CC[X] <: Iterable[X] with collection.SetOps[X, CC, CC[X]]] :Generic[CC] =
			prototype.asInstanceOf[Generic[CC]]

		private[this] val prototype =
			new SerializableSingleton("LikeSet.Generic.forOps", Generic.forOps[Set]) with Generic[Set] {
				override implicit def of[X] :LikeSet[X, Set[X], Set, Set[X]] = LikeSet.forOps[X, Set[X], Set, Set[X]]
			}
	}


	trait LikeMoreSpecific[X, -Xs, +CC[_], +C]
		extends LikeSet[X, Xs, CC, C] with LikeIterable.LikeMoreSpecific[X, Xs, CC, C]
	{
		abstract override def contains[A >: X <: X](elems :Xs, elem :A) :Boolean = moreSpecific(elems) match {
			case Yes(specific) => specific.contains(elems, elem)
			case No            => super.contains(elems, elem)
		}
		abstract override def containsAll[O](elems :Xs, that :O)
		                                    (implicit likeCollection :LikeCollection[X, O]) :Boolean =
			moreSpecific(elems) match {
				case Yes(specific) => specific.containsAll(elems, that)
				case No            => super.containsAll(elems, that)
			}
		abstract override def subsetOf[O](elems :Xs, that :O)(implicit likeSet :LikeSet[X, O, Any1, _]) :Boolean =
			moreSpecific(elems) match {
				case Yes(specific) => specific.subsetOf(elems :elems.type, that)
				case No            => super.subsetOf(elems, that)
			}
		abstract override def subsetOf(elems :Xs, that :collection.Set[X]) :Boolean = moreSpecific(elems) match {
			case Yes(specific) => specific.subsetOf(elems :elems.type, that)
			case No            => super.subsetOf(elems, that)
		}
		abstract override def intersect[O](elems :Xs, that :O)(implicit likeSet :LikeSet[X, O, Any1, _]) :C =
			moreSpecific(elems) match {
				case Yes(specific) => specific.intersect(elems :elems.type, that)
				case No            => super.intersect(elems, that)
			}
		abstract override def intersect(elems :Xs, that :collection.Set[X]) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.intersect(elems :elems.type, that)
			case No            => super.intersect(elems, that)
		}
		abstract override def diff[O](elems :Xs, that :O)(implicit likeSet :LikeSet[X, O, Any1, _]) :C =
			moreSpecific(elems) match {
				case Yes(specific) => specific.diff(elems :elems.type, that)
				case No            => super.diff(elems, that)
			}
		abstract override def diff(elems :Xs, that :collection.Set[X]) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.diff(elems :elems.type, that)
			case No            => super.diff(elems, that)
		}
		abstract override def union[O](elems :Xs, that :O)(implicit likeCollection :LikeCollection[X, O]) :C =
			moreSpecific(elems) match {
				case Yes(specific) => specific.union(elems :elems.type, that)
				case No            => super.union(elems, that)
			}
		abstract override def union(elems :Xs, that :IterableOnce[X]) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.union(elems :elems.type, that)
			case No            => super.union(elems, that)
		}
		abstract override def subsets(elems :Xs, len :Int) :Iterator[C] = moreSpecific(elems) match {
			case Yes(specific) => specific.subsets(elems, len)
			case No            => super.subsets(elems, len)
		}
		abstract override def subsets(elems :Xs) :Iterator[C] = moreSpecific(elems) match {
			case Yes(specific) => specific.subsets(elems)
			case No            => super.subsets(elems)
		}
		abstract override def toImpureSet(elems :Xs) :collection.Set[X] = moreSpecific(elems) match {
			case Yes(specific) => specific.toImpureSet(elems)
			case No            => super.toImpureSet(elems)
		}
	}



	trait LikeSetBasics[X, -Xs, +CC[_], +C] extends LikeIterableBasics[X, Xs, CC, C] with LikeSet[X, Xs, CC, C] {
		override def containsAll[O](elems :Xs, that :O)(implicit likeCollection :LikeCollection[X, O]) :Boolean =
			likeCollection.knownSize(that) == 0 || likeCollection.forall(that)(contains(elems, _))

		override def subsetOf(elems :Xs, that :collection.Set[X]) :Boolean = {
			val thisSize = knownSize(elems)
			val thatSize = that.knownSize
			(thisSize <= thatSize | thatSize == -1) && forall(elems)(that)
		}
		override def subsetOf[O](elems :Xs, that :O)(implicit likeSet :LikeSet[X, O, kinds.Any1, _]) :Boolean = {
			val thisSize = knownSize(elems)
			val thatSize = likeSet.knownSize(that)
			(thisSize <= thatSize | thatSize == -1) && likeSet.containsAll(that, elems)(this)
		}

		override def intersect(elems :Xs, that :collection.Set[X]) :C =
			if (that.knownSize == 0) empty(elems) else filter(elems)(that)

		override def intersect[O](elems :Xs, that :O)(implicit likeSet :LikeSet[X, O, Any1, _]) :C =
			if (likeSet.knownSize(that) == 0) empty(elems)
			else if (knownSize(elems) == 0) toSpecific(elems)
			else filter(elems)(likeSet.toImpureSet(that))

		override def diff(elems :Xs, that :collection.Set[X]) :C =
			if (that.knownSize == 0) toSpecific(elems) else filterNot(elems)(that)

		override def diff[O](elems :Xs, that :O)(implicit likeSet :LikeSet[X, O, Any1, _]) :C =
			if (likeSet.knownSize(that) == 0 || knownSize(elems) == 0) toSpecific(elems)
			else filterNot(elems)(likeSet.toImpureSet(that))

//		override def union[O](elems :Xs, that :O)(implicit likeSet :LikeSet[X, O, Any1, _]) :C =
		override def union(elems :Xs, that :IterableOnce[X]) :C = union[IterableOnce[X]](elems, that)
	}



	trait FromOps[X, -Xs, +CC[_], +C <: collection.SetOps[X, CC, C]]
		extends LikeSet[X, Xs, CC, C] with LikeIterable.FromOps[X, Xs, CC, C]
	{
		override def contains[A >: X <: X](elems :Xs, elem :A) :Boolean = toOps(elems).contains(elem)
		override def containsAll[O](elems :Xs, that :O)(implicit likeCollection :LikeCollection[X, O]) :Boolean =
			likeCollection.forall(that)(toOps(elems))

		override def subsetOf(elems :Xs, that :collection.Set[X]) :Boolean = toOps(elems).subsetOf(that)

		override def subsetOf[O](elems :Xs, that :O)(implicit likeSet :LikeSet[X, O, kinds.Any1, _]) :Boolean =
			if (likeSet.knownSize(that) == 0) isEmpty(elems)
			else toOps(elems).subsetOf(likeSet.toImpureSet(that))

		override def intersect(elems :Xs, that :collection.Set[X]) :C = toOps(elems).intersect(that)

		override def intersect[O](elems :Xs, that :O)(implicit likeSet :LikeSet[X, O, Any1, _]) :C =
			if (likeSet.knownSize(that) == 0) empty(elems)
			else toOps(elems).intersect(likeSet.toImpureSet(that))

		override def diff(elems :Xs, that :collection.Set[X]) :C = toOps(elems).diff(that)

		override def diff[O](elems :Xs, that :O)(implicit likeSet :LikeSet[X, O, Any1, _]) :C =
			if (likeSet.knownSize(that) == 0) empty(elems)
			else toOps(elems).diff(likeSet.toImpureSet(that))

		override def union(elems :Xs, that :IterableOnce[X]) :C = toOps(elems).concat(elems)

		override def union[O](elems :Xs, that :O)(implicit likeCollection :LikeCollection[X, O]) :C =
			if (likeCollection.knownSize(that) == 0) toSpecific(elems)
			else toOps(elems).concat(likeCollection.toIterableOnce(that))

		override def subsets(elems :Xs, len :Int) :Iterator[C] = toOps(elems).subsets(len)
		override def subsets(elems :Xs) :Iterator[C] = toOps(elems).subsets()

		override def toImpureSet(elems :Xs) :collection.Set[X] = toOps(elems) match {
			case set :collection.Set[X] => set
			case other                  => other.toSet
		}

		override def toOps(elems :Xs) :collection.SetOps[X, CC, C]
	}


	trait ForOps[X, CC[A] <: Iterable[A], C <: CC[X] with collection.SetOps[X, CC, C]]
		extends FromOps[X, C, CC, C] with LikeIterable.ForOps[X, CC, C]
	{
		override def toOps(elems :C) :collection.SetOps[X, CC, C] = elems
	}



//	trait FromIterator[X, -Xs, +CC[_], +C]
//		extends LikeSetDefaults[X, Xs, CC, C] with LikeIterable.FromIterator[X, Xs, CC, C]
//	{
//		override def union[O](elems :Xs, that :O)(implicit likeCollection :LikeCollection[X, O]) :C =
//			makeSpecific(elems)(iterator(elems) :++ likeCollection.iterator(that))
//
//		override def subsets(elems :Xs, len :Int) :Iterator[C] = ???
//		override def subsets(elems :Xs) :Iterator[C] = ???
//	}
}




private trait LikeSetAdapter[X, Xs, +CC[_], +C <: collection.SetOps[X, CC, C]]
	extends collection.SetOps[X, CC, C] with LikeIterableAdapter[X, Xs, CC, C]
{
	protected override val ops :LikeSet[X, Xs, Any1, _]

	override def contains(elem :X) :Boolean = ops.contains(elems, elem)

	override def subsetOf(that :collection.Set[X]) :Boolean = ops.subsetOf(elems, that)
//	override def intersect(that :collection.Set[X]) :C = ops.intersect(elems, that)
//	override def diff(that :collection.Set[X]) :C = ops.diff(elems, that)
//	override def concat(that :IterableOnce[X]) :C = ops.union(elems, that)
//
//	override def subsets(len :Int) :Iterator[C] = ops.subsets(elems, len)
//	override def subsets :Iterator[C] = ops.subsets(elems)
}
