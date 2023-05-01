package net.noresttherein.sugar.collections

import java.lang.invoke.VarHandle.releaseFence

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{BufferedIterator, IterableFactoryDefaults, SeqFactory}
import scala.collection.immutable.{AbstractSeq, IndexedSeqOps}
import scala.collection.mutable.Builder




//Fixme: Doesn't work, because if we want to remove a node by reference, not index, we need a link to parent,
// and for that we'd have to copy the whole tree each time.
/**
  * @author Marcin Mościcki
  */
/*
sealed trait BTreeSeq[+E]
	extends AbstractSeq[E]
	   with IndexedSeqOps[E, BTreeSeq, BTreeSeq[E]]
	   with IterableFactoryDefaults[E, BTreeSeq]
	   with Serializable
{
	override def iterableFactory :SeqFactory[BTreeSeq] = BTreeSeq
	def inserted[B >: E](i :Int, elem :B) :BTreeSeq[B]

	protected override def className = "BTreeSeq"
}




object BTreeSeq extends SeqFactory[BTreeSeq] {

	override def from[A](source :IterableOnce[A]) :BTreeSeq[A] = source match {
		case btree :BTreeSeq[A] => btree
		case empty :Iterable[A] if empty.isEmpty => this.empty[A]
		case empty :Iterator[A] if !empty.hasNext => this.empty[A]
		case _ => (newBuilder[A] ++= source).result()
	}

	override def empty[A] :BTreeSeq[A] = Empty

	override def newBuilder[A] :Builder[A, BTreeSeq[A]] = ???



	private sealed trait Node[+E] extends BTreeSeq[E] { root =>
		var parent :InnerNode[E @uncheckedVariance]

		@tailrec private[this] def first(node :Node[E] = this) :Leaf[E] = node match {
			case leaf :Leaf[E] => leaf
			case inner :InnerNode[E] => first(inner.children(0))
		}
		@tailrec private[this] def find(i :Int, node :Node[E] = this) :Leaf[E] = node match {
			case leaf :Leaf[E] => leaf
			case inner :InnerNode[E] =>
				val lengths = inner.prefixLengths
				val end = lengths.length
				var j = end - 1
				while (j > 0 && i < lengths(j))
					j -= 1
				find(i - lengths(j), inner.children(j + 1))
		}
		//goes up the tree, substituting oldChild for newChild in
		@tailrec private def update[B >: E](oldChild :Node[B], newChild :Node[B]) :Node[B] = oldChild.parent match {
			case null => newChild
			case parent =>
				val siblings = parent.children
				val lengths  = parent.prefixLengths
				val siblingsCount = siblings.length
				var j = 0
				while (siblings(j) ne oldChild)
					j += 1
				val newKids = new Array[Node[B]](siblingsCount)
				System.arraycopy(siblings, 0, newKids, 0, j)
				newKids(j) = newChild
				val delta = oldChild.length - newChild.length
				val newLengths =
					if (delta == 0)
						lengths
					else {
						val a = new Array[Int](siblingsCount)
						System.arraycopy(lengths, 0, a, 0, j)
						var i = j
						while (i < siblingsCount) {
							a(i) += delta
							i += 1
						}
						a
					}
				j += 1
				System.arraycopy(newKids, j, siblings, j, siblingsCount - j)
				val newParent = new InnerNode[B](newLengths, newKids)
				update(parent, newParent)
		}


		override def head :E = first().value
		override def apply(idx :Int) :E = {
			checkIndex(idx)
			find(idx).value
		}

		override def updated[B >: E](index :Int, elem :B) :BTreeSeq[B] = {
			//goes up the tree, substituting oldChild for newChild, without changing prefix lengths
			@tailrec def update(oldChild :Node[B], newChild :Node[B]) :Node[B] = oldChild.parent match {
				case null => newChild
				case parent =>
					val siblings = parent.children
					var j = 0
					while (siblings(j) ne oldChild)
						j += 1
					val newKids = new Array[Node[B]](siblings.length)
					System.arraycopy(siblings, 0, newKids, 0, j)
					newKids(j) = newChild
					j += 1
					System.arraycopy(newKids, j, siblings, j, siblings.length - j)
					val newParent = new InnerNode[B](parent.prefixLengths, newKids)
					update(parent, newParent)
			}
			checkIndex(index)
			val leaf = find(index)
			update(leaf, new Leaf(elem))
		}
		override def inserted[B >: E](index :Int, elem :B) :BTreeSeq[B] = {
			if (index < 0 || index > length)
				throw new IndexOutOfBoundsException(index + " out of " + length)
			def update(oldChild :Node[B], extra :Node[B]) :Node[B] = oldChild.parent match {
				case null => extra
				case parent =>
					val siblings = parent.children
					val lengths = parent.prefixLengths
					val siblingCount = siblings.length
					if (siblingCount == (Rank << 1) - 1) {

					}
					var j = 0
					while (siblings(j) ne oldChild)
						j += 1
					j += 1
					val newKids = new Array[Node[B]](siblingCount + 1)
					val newLengths = new Array[Int](siblingCount + 1)
					val extraLength = extra.length
					System.arraycopy(siblings, 0, newKids, 0, j)
					System.arraycopy(lengths, 0, newLengths, 0, j)
					newKids(j) = extra
					System.arraycopy(siblings, j, newKids, j + 1, siblings.length - j)
					while (j < siblingCount)
						newLengths(j + 1) = lengths(j) + extra
						j += 1

			}
			val current = find(index)
			update(current, new Leaf(elem))
		}

		private def checkIndex(idx :Int) =
			if (idx < 0 || idx >= length)
				throw new IndexOutOfBoundsException(idx + " out of " + length)

		override def iterator :Iterator[E] = new BTreeSeqIterator[E](this)
	}

	private class InnerNode[+E](val prefixLengths :Array[Int], val children :Array[Node[E @uncheckedVariance]])
		extends Node[E]
	{
		override var parent :InnerNode[E @uncheckedVariance] = _
		override def length :Int = {
			val lengths = prefixLengths
			if (lengths.length == 0) 0 else lengths(lengths.length - 1)
		}
	}

	private class Leaf[+E](val value :E) extends Node[E] {
		override var parent :InnerNode[E @uncheckedVariance] = _
		override def length = 1

		def removed :BTreeSeq[E] = {
			def rec(node :Node[E]) :BTreeSeq[E] = {
				val parent = node.parent
				val siblings = parent.subnodes
				val prefixLengths = parent.lengths
				val nodeRank = siblings.length
				if (nodeRank > Rank) {
					val newNodes = new Array[Node[E]](nodeRank - 1)
					val newSizes = new Array[Node[E]](nodeRank - 1)
					var i = 0
					var sibling :Node[E] = null
					while ({ sibling = siblings(i); sibling ne node }) {
						newNodes(i) = sibling
						newSizes(i) = prefixLengths(i)
						i += 1
					}
					var j = i; i += 1
					while (i < nodeRank) {
						newNodes(j) = siblings(i)
						newSizes(j) = prefixLengths(i) - 1
						i += 1; j += 1
					}
				}
			}
		}
	}

	object Empty extends BTreeSeq[Nothing] {
		override def length = 0
		override def head = throw new NoSuchElementException("BTreeSeq.empty.head")
		override def apply(i :Int) :Nothing =
			throw new IndexOutOfBoundsException("BTreeSeq.empty(" + i + ")")
		override def updated[B >: Nothing](i :Int, elem :B) :Nothing =
			throw new IndexOutOfBoundsException("BTreeSeq.empty.updated(" + i + ", " + elem + ")")
		override def inserted[B >: Nothing](i :Int, elem :B) :Nothing =
			throw new IndexOutOfBoundsException("BTreeSeq.empty.inserted(" + i + ", " + elem + ")")

		override def isEmpty = true
		override def iterator :Iterator[Nothing] = Iterator.empty
	}


	private class BTreeSeqIterator[+E](root :Node[E]) extends BufferedIterator[E] {
		//the leaf in the tree with the next value
		private[this] var current :Leaf[E] = _
		//index of the inner node at the given level on the path leading to the current leaf
		private[this] val path = {
			var level = 0
			var node = root
			while (node match {
				case leaf :Leaf[E] =>
					current = leaf
					false
				case inner :InnerNode[E] =>
					node = inner.children(0);
					level += 1
					true
			}) {}
			if (level == 0) emptyIntArray
			else new Array[Int](level)
		}

		override def head :E = current.value
		override def hasNext = current != null

		override def next() = {
			val res = current.value
			var node = current.parent
			current = null
			var level = path.length - 1 //level < 0 iff node == null
			var childNo :Int = 0
			while (node != null && {    //retreat up to the first ancestor whose last child is not on the path
				val children = node.children
				childNo = path(level) + 1
				children.length == childNo
			})
				level -= 1
			if (node != null) { //go to the leftmost leaf under node
				path(level) = childNo
				while (node.children(childNo) match {
					case leaf :Leaf[E] =>
						current = leaf
						false
					case inner :InnerNode[E] =>
						level += 1
						path(level) = 0
						node = inner
						true
				}) {}
			}
			res
		}

		override def drop(n :Int) :Iterator[E] =
			if (n <= 0)
				this
			else {
				var counter = n - 1        //counter of the elements to drop
				var node = current.parent
				current = null
				var level = path.length - 1 //level < 0 iff node == null
				var childNo :Int = 0
				while (node != null && { //retreat up for as long as the whole tree is smaller than counter
					val children = node.children
					val lengths  = node.prefixLengths
					val childrenNo = children.length
					childNo = path(level)
					val past = lengths(childNo) //the number of iterated over elements, including those already dropped going up
					childNo += 1
					//find the first sibling whose prefix length exceeds the number of dropped elements + counter
					while (childNo < childrenNo && lengths(childNo) - past < counter)
						childNo += 1
					node = node.parent
					counter -= lengths(childNo - 1) - past
					counter > 0
				})
					level -= 1
				if (node != null) { //if node == null then current == null and we dropped everything
					level += 1
					while (node.children(childNo) match {
						case leaf :Leaf[E] =>
							current = leaf
							false
						case inner :InnerNode[E] =>
							node = inner
							val lengths = inner.prefixLengths
							childNo = 0
							//go down into the first sibling whose prefix exceeds the number of the elements yet to drop
							while (lengths(childNo) < counter)
								childNo += 1
							if (childNo > 0)
								counter -= lengths(childNo - 1)
							true
					})
						{}
				}
				this
			}

		override def knownSize :Int =
			if (current == null)
				0
			else {
				var total = 1
				var node = current.parent
				var level = path.length - 1
				while (node match {
					case null => false
					case _ =>
						val children = node.children
						val lengths = node.prefixLengths
						val childrenNo = children.length
						val childNo = path(level)
						level -= 1
						if (childNo != childrenNo)
							total += lengths(childrenNo - 1) - lengths(childNo)
						node = node.parent
						true
				})
					{}
				total
			}
	}


	private final val Rank = 8
	private final val MaxDepth = 5
	private[this] val emptyIntArray  = new Array[Int](0)
	private[this] val emptyNodeArray = new Array[Node[Nothing]](0)
}
*/
