package net.noresttherein.sugar.collections

import scala.collection.immutable.SortedSet
import scala.collection.Factory


object IndexedSetSpec
	extends GenericIterableProps[IndexedSet, SortedSet, Ordering]("IndexedSet")
	   with OrderedProps[IndexedSet, SortedSet, Ordering]
{
	//todo: SetProps

	override def S[T :Ordering] :Factory[T, SortedSet[T]] = SortedSet
	override def C[T :Ordering] :Factory[T, IndexedSet[T]] = IndexedSet

	implicit override def intEvidence    :Ordering[Int] = Ordering.Int
	implicit override def longEvidence   :Ordering[Long] = Ordering.Long
	implicit override def stringEvidence :Ordering[String] = Ordering.String
	implicit override def intSEvidence   :Ordering[SortedSet[Int]] = Ordering.Implicits.sortedSetOrdering
	implicit override def intCEvidence   :Ordering[IndexedSet[Int]] = Ordering.Implicits.sortedSetOrdering
	implicit override def pairEvidence[A :Ordering, B :Ordering] :Ordering[(A, B)] = Ordering.Tuple2

	override def knowsSize = true
}