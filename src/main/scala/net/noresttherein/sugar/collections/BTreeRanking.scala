package net.noresttherein.sugar.collections

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{IterableFactory, IterableFactoryDefaults}
import scala.collection.mutable.Builder

import net.noresttherein.sugar.typist.casting.extensions.{cast2TypeParams, castTypeParam}




/**
  * @author Marcin Mościcki
  */
/*
sealed trait BTreeRanking[+E]
	extends Ranking[E]
	   with SugaredIterableOps[E, BTreeRanking, BTreeRanking[E]]
	   with IterableFactoryDefaults[E, BTreeRanking]
{
	override def iterableFactory :IterableFactory[BTreeRanking] = BTreeRanking
	protected override def className = "BTreeRanking"
}




object BTreeRanking extends IterableFactory[BTreeRanking] {

	override def from[A](source :IterableOnce[A]) :BTreeRanking[A] = ???

	override def empty[A] :BTreeRanking[A] = Empty

	def single[A](elem :A) :BTreeRanking[A] = ???

	override def newBuilder[A] :Builder[A, BTreeRanking[A]] = ???

	private final val MinRank = 4
	private final val MaxRank = 7


	private trait Node[+E] extends BTreeRanking[E] {
		def rank :Int
		protected def split[U >: E](siblings :Array[Node[U]], idx :Int) :Unit
	}



	private final class ParentNode[+E](val children :Array[Node[E] @uncheckedVariance], val prefixLengths :Array[Int],
	                                   val childWith :Map[E @uncheckedVariance, Int])
		extends Node[E]
	{
		override def rank :Int = children.length
		override def knownSize = prefixLengths(prefixLengths.length - 1)

		override def indexOf[U >: E](elem :U) :Int = {
			def find(prefix :Int = 0, node :Node[E] = this) :Int = node match {
				case leaves :Leaves[E] =>
					val res = leaves.elems.indexOf(elem)
					if (res < 0) -1 else prefix + res
				case parent :ParentNode[E] =>
					val idx = parent.childWith(elem.asInstanceOf[E])
					val prefixLength = if (idx == 0) prefix else prefix + parent.prefixLengths(idx - 1)
					find(prefixLength, parent.children(idx))
			}
			childWith.getOrElse(elem.asInstanceOf[E], -1) match {
				case -1 => -1
				case n  => find(if (n == 0) 0 else prefixLengths(n - 1), children(n))
			}
		}

		override def apply(n :Int) :E = {
			@tailrec def find(idx :Int, node :Node[E]) :E = node match {
				case leaves :Leaves[E]    => leaves.elems(idx)
				case parent :ParentNode[E] =>
					val lengths = parent.prefixLengths
					var i = 0
					var last = 0
					var next = 0
					while ({ next = lengths(i); next < idx }) {
						last = next
						i += 1
					}
					find(idx - last, parent.children(i))
			}
			checkIndex(n)
			find(n, this)
		}

		override def updated[U >: E](n :Int, elem :U) :BTreeRanking[U] = {
			class RemovedChild(var newNode :Node[U], val removed :U)

			def update(idx :Int, node :Node[E]) :RemovedChild = node match {
				case leaves :Leaves[E] =>
					val elems = leaves.elems
					val newElems = new Array[AnyRef](elems.length).castParam[U]
					System.arraycopy(elems, 0, newElems, 0, elems.length)
					val old = newElems(idx)
					newElems(idx) = elem
					new RemovedChild(new Leaves(newElems), old)
				case parent :ParentNode[E] =>
					val lengths  = parent.prefixLengths
					val children = parent.children
					var i = 0
					var last = 0 //last lengths(i) < idx
					var next = 0
					while ({ next = lengths(i); next < idx }) {
						last = next
						i += 1
					}
					val res = update(idx - last, children(i))
					val newChild = res.newNode
					val newChildren = new Array[Node[U]](lengths.length)
					System.arraycopy(children, 0, newChildren, 0, lengths.length)
					newChildren(i)  = newChild
					val index = (parent.childWith - res.removed).castParam1[U].updated(elem, i)
					res.newNode = new ParentNode[U](newChildren, lengths, index)
					res
			}
			checkIndex(n)
			update(n, this).newNode
		}

		override def added[U >: E](elem :U) :Ranking[U] = indexOf(elem) match {
			case -1 =>
				def descend(node :Node[E]) :Node[U] = node match {
					case leaves :Leaves[E] =>
						val oldElems = leaves.elems
						val elems = Array.copyOf(oldElems.castParam[U], oldElems.length + 1)
						elems(oldElems.length) = elem
						new Leaves(elems)
					case parent :ParentNode[E] =>
						val children = parent.children
						val rank = children.length
						val last = children(rank - 1)
						if (last.rank < MaxRank) {
							val newChild = descend(last)
							val newChildren = Array.copyOf(children.castParam[U], rank)
							newChildren(rank - 1) = newChild
							val lengths = Array.copyOf(parent.prefixLengths, rank)
							val length = lengths(rank - 1)
							lengths(rank - 1) = length + 1
							new ParentNode(newChildren, lengths, childWith.castParam1[U].updated(elem, length - 1))
						} else {
							val newChildren = Array.copyOf(children.castParam[U], rank + 1)
							last.split(newChildren, rank)
						}

				}
			case _  => this
		}

		protected override def split[U >: E](siblings :Array[Node[U]], idx :Int) :Unit = ???

		private def checkIndex(n :Int) :Unit =
			if (n < 0 || n >= length)
				throw new IndexOutOfBoundsException(n.toString + " out of " + length)

		override def iterator :Iterator[E] = ???
		override def reverseIterator :Iterator[E] = ???

	}



	private final class Leaves[+E](val elems :Array[E @uncheckedVariance]) extends Node[E] {
		override def rank = elems.length
		override def knownSize = elems.length
		override def indexOf[U >: E](elem :U) :Int = elems.indexOf(elem)
		override def apply(n :Int) :E = elems(n)

		override def updated[U >: E](n :Int, elem :U) :BTreeRanking[U] = {
			val rank = elems.length
			val newElems = new Array[Any](rank).castParam[U]
			System.arraycopy(elems, 0, newElems, 0, rank)
			newElems(n) = elem
			new Leaves[U](newElems)
		}
		override def added[U >: E](elem :U) :Ranking[U] = elems.indexOf(elem) match {
			case -1 =>
				val rank = elems.length
				if (rank < MaxRank) {
					val newElems = Array.copyOf(elems.castParam[U], rank + 1)
					newElems(rank) = elem
					new Leaves(newElems)
				} else {
					val leftElems  = new Array[Any](MinRank).castParam[U]
					val rightElems = new Array[Any](MinRank).castParam[U]
					System.arraycopy(elems, 0, leftElems, 0, MinRank)
					System.arraycopy(elems, MinRank, rightElems, 0, MinRank - 1)
					val left  = new Leaves(leftElems)
					val right = new Leaves(rightElems)
					val siblings = new Array[Node[U]](2)
					siblings(0) = left
					siblings(1) = right

					val index = Map.empty[U, Int]
				    var i = 0
					while (i < MinRank) {
						index = index.updated(leftElems(i), 0)
						i += 1
					}
					i = 0
					while (i < MinRank) {
						index = index.updated(rightElems(i), 1)
						i += 1
					}
					val lengths = new Array[Int](2)
					lengths(0) = MinRank
					lengths(1) = MinRank

					new ParentNode(siblings, lengths, index)
				}
			case _  => this
		}

		protected override def split[U >: E](siblings :Array[Node[U]], idx :Int) :Unit = {
			val elems = this.elems
			if (elems.length < MaxRank)
				throw new AssertionError(
					"Cannot split BTree node of rank " + elems.length + " (of " + MaxRank + ")"
				)
			val left  = new Array[Any](MinRank).castParam[U]
			val right = new Array[Any](MinRank).castParam[U]
			System.arraycopy(elems, 0, left, 0, MinRank)
			System.arraycopy(elems, MinRank, right, 0, MinRank - 1)
			siblings(idx) = new Leaves(left)
			siblings(idx + 1) = new Leaves(right)
		}

		override def iterator        :Iterator[E] = ArrayIterator(elems)
		override def reverseIterator :Iterator[E] = ReverseArrayIterator(elems)
	}


	private object Empty extends BTreeRanking[Nothing] {
		override def knownSize = 0
		override def size = 0
		override def indexOf[U >: Nothing](elem :U) :Int = -1

		override def apply(idx :Int) = throw new IndexOutOfBoundsException(idx.toString + " out of 0")
		override def updated[U >: Nothing](n :Int, value :U) =
			throw new IndexOutOfBoundsException("BTreeRanking().updated(" + n + ", " + value + ")")

		override def removed[U >: Nothing](elem :U) :Ranking[Nothing] = this
		override def added[U >: Nothing](elem :U) :Ranking[U] = BTreeRanking.single(elem)
		override def prepended[U >: Nothing](elem :U) :Ranking[U] = BTreeRanking.single(elem)
		override def appended[U >: Nothing](elem :U) :Ranking[U] = BTreeRanking.single(elem)
		override def appendedAll[U >: Nothing](elems :IterableOnce[U]) :Ranking[U] = BTreeRanking.from(elems)
		override def prependedAll[U >: Nothing](elems :Iterable[U]) :Ranking[U] = BTreeRanking.from(elems)
		override def concat[B >: Nothing](suffix :IterableOnce[B]) :BTreeRanking[B] = BTreeRanking.from(suffix)

		override def removedAll[U >: Nothing](elems :IterableOnce[U]) :Ranking[Nothing] = ???

		override def iterator = Iterator.empty
		override def reverseIterator = Iterator.empty
	}

}

*/
