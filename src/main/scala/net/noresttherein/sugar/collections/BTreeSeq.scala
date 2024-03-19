package net.noresttherein.sugar.collections

import scala.annotation.{nowarn, tailrec}
import scala.collection.{BufferedIterator, IterableFactoryDefaults, SeqFactory, StrictOptimizedSeqFactory, mutable}
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.{AbstractSeq, ArraySeq, IndexedSeqDefaults, IndexedSeqOps, StrictOptimizedSeqOps}
import scala.collection.mutable.{Builder, ReusableBuilder}
import scala.reflect.ClassTag

import net.noresttherein.sugar.arrays.{ArrayIterator, ErasedArray, IArray, arraycopy}
import net.noresttherein.sugar.collections.BTreeSeq.{CompletePrefixes, ConvertToBTreeOnConcatFactor, Empty, Leaf, MaxChildren, Node, Rank, SemiCompletePrefixes, grow, semiCompleteNode}
import net.noresttherein.sugar.exceptions.{??!, noSuch_!, outOfBounds_!, unsupported_!}
import net.noresttherein.sugar.vars.Box
import net.noresttherein.sugar.vars.Maybe.Yes

//implicits
import net.noresttherein.sugar.casting.castTypeParamMethods
import net.noresttherein.sugar.arrays.ArrayCompanionExtension
import net.noresttherein.sugar.collections.extensions.IterableOnceExtension





/**
  * @define Coll `BTreeSeq`
  * @define coll BTree sequence
  * @author Marcin Mościcki
  */
sealed trait BTreeSeq[+E]
	extends IndexedSeq[E] with IndexedSeqOps[E, BTreeSeq, BTreeSeq[E]] with IterableFactoryDefaults[E, BTreeSeq]
	   with StrictOptimizedSeqOps[E, BTreeSeq, BTreeSeq[E]] with DefaultSerializable
{
	//todo: patch, iterator, reverseIterator
	def rank :Int
	def depth :Int =
		if (this eq Empty)
			0
		else {
			@tailrec def descend(tree :BTreeSeq[E], acc :Int) :Int = tree match {
				case node :Node[E] => descend(node.subNodes(0), acc + 1)
				case _ => acc
			}
			descend(this, 1)
		}
//	def length :Int
//	def head :E = apply(0)
//	def apply(i :Int) :E
//	def updated[U >: E](index :Int, elem :U) :BTreeSeq[U]

	/** Removes the element at the given position from this sequence.
	  * @return `this.take(index) ++ this.drop(index + 1)`, but in a more efficient manner.
	  */
	def removed(index :Int) :BTreeSeq[E]

	/** Adds a new value to this sequencee at the given position. All elements and indices `[index, length)`
	  * are pushed back by one position; specifying `index = length` has the same effect as
	  * [[net.noresttherein.sugar.collections.appended appended]].
	  */
	@throws[IndexOutOfBoundsException]("if index is lesser than 0 or greater than this.length.")
	def inserted[U >: E](index :Int, elem :U) :BTreeSeq[U]

	/** A polymorphic method of `Node` and `Leaf` used in recursive implementation of `Node.inserted(index, elem)`.
	  * The additional `surplus` parameter is a holder for an optional sibling node following the returned node
	  * if this node must be split to accommodate the new value. If non empty after the call, the caller must,
	  * aside from substituting this node with the returned one in the `children` array, also create space
	  * for the node in the box.
	  */
	//implementation for Empty and Singleton
	def inserted[U >: E](index :Int, elem :U, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] = inserted(index, elem)

	/** Appends the new element to this tree as a sequence, balancing the tree on its way up.
	  * If balancing results in splitting of this node into two nodes, the first one is returned,
	  * while `surplus` is set to the second one.
	  * @param elem    The new last element of this sequence.
	  * @param surplus An 'out' parameter, used to return a new sibling of this node. It is always empty when the
	  *                method is called, and filled or cleared, as the need arises, when the method returns.
	  */ //implementation good only for Empty and Singleton!
	def appended[U >: E](elem :U, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] = appended(elem)

	/** Appends the new element to this tree as a sequence, balancing the tree on its way up.
	  * If balancing results in splitting of this node into two nodes, the first one is returned,
	  * while `surplus` is set to the second one.
	  * @param elem    The new first element of this sequence.
	  * @param surplus An 'out' parameter, used to return a new sibling of this node. It is always empty when the
	  *                method is called, and filled or cleared, as the need arises, when the method returns.
	  */ //implementation good only for Empty and Singleton!
	def prepended[U >: E](elem :U, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] = prepended(elem)


	/** Appends to this node the first `siblingCount` children of a following sibling, which must be of the same class
	  * as this instance. For inner nodes, the new `Node`s `children` will equal
	  * `this.children ++ successor.children.take(siblingCount)`.
	  * For leaves, the new `Leaf`'s `keys` will equal `this.keys ++ successor.keys.take(siblingCount)`.
	  */
	def appendedSome[U >: E](successor :BTreeSeq[U], siblingCount :Int) :BTreeSeq[U] =
		unsupported_!(toString + ".appended(" + successor + ", " + siblingCount + ")")

	/** Prepends to this node the last `siblingCount` children of a preceding sibling, which must be of the same class
	  * as this instance. For inner nodes, the new `Node`s `children` will equal
	  * `predecessor.children.takeRight(siblingCount) ++ this.children`.
	  * For leaves, the new `Leaf`'s `keys` will equal `predecessor.keys.takeRight(siblingCount) ++ this.keys`.
	  */
    def prependedSome[U >: E](predecessor :BTreeSeq[U], siblingCount :Int) :BTreeSeq[U] =
		unsupported_!(toString + ".prepended(" + predecessor + ", " + siblingCount + ")")


	/** Inserts the tree argument into this tree at the appropriate depth, by appending it to children
	  * of either this node or one of its descendants. The method requires this node to be balanced.
	  * The result is of the same depth as this node and balanced. If appending would result in an overflow,
	  * the node is split: a new node with the first half of the children is returned,
	  * while the second half (and the extra child) is assigned to `surplus`.
	  * @param tree    An internally balanced tree with elements to append, of depth `this.depth - depth`.
	  *                May have fewer children than `Rank`.
	  * @param depth   the difference in depth between this tree and the argument, specifying at what relative depth
	  *                it should be placed in this tree.
	  * @param surplus An out parameter, set to a new, following sibling of the returned node if appending overflows.
	  */ //implementation good only for Empty and Singleton!
	def appendedAll[U >: E](tree :BTreeSeq[U], depth :Int, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] = appendedAll(tree)

	/** Inserts the tree argument into this tree at the appropriate depth, by prepending it to children
	  * of either this node or one of its descendants.  The method requires this node to be balanced.
	  * The result is of the same depth as this node and balanced. If prepending would result in an overflow,
	  * the node is split: a new node with the first half of the children and the extra child is returned,
	  * while the second half is assigned to `surplus`.
	  * @param tree    An internally balanced  tree with elements to prepend, of depth `this.depth - depth`.
	  *                May have fewer children than `Rank`.
	  * @param depth   The difference in depth between this tree and the argument, specifying at what relative depth
	  *                it should be placed in this tree.
	  * @param surplus An out parameter, set to a new, following sibling of the returned node if prepending overflows.
	  */ //implementation good only for Empty and Singleton!
	def prependedAll[U >: E](tree :BTreeSeq[U], depth :Int, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] = prependedAll(tree)

	def appendedAll[U >: E](elems :BTreeSeq[U]) :BTreeSeq[U]

	def prependedAll[U >: E](elems :BTreeSeq[U]) :BTreeSeq[U]

	@inline private def appendedSlice[U >: E](array :Array[U]) :BTreeSeq[U] = appendedSlice(array, 0, array.length)
	@inline private def prependedSlice[U >: E](array :Array[U]) :BTreeSeq[U] = prependedSlice(array, 0, array.length)

	/** Appends the elements from the range `[start, start + length)` from the given array, by slicing the array
	  * into segments for individual leaves, and then adding those leaves in whole groups as `Node`s of depth 2
	  * (first level), for as long as possible. Much faster than adding individual elements.
	  */
	private def appendedSlice[U >: E](array :Array[U], from :Int, until :Int) :BTreeSeq[U] = (until - from) match {
		case 0                                       => this
		case 1                                       => appended(array(from))
		case _ if !array.isInstanceOf[Array[AnyRef]] => appendedAll(ArrayIterator.slice(array, from, until))
		case size                                    =>
			if (size <= MaxChildren)
				appendedAll(new Leaf(ErasedArray.copyOfRange(array, from, until)))
			else {
				//Build the result by creating Nodes of level 2 manually using mutable state.
				var tree     :BTreeSeq[U]        = this             //the built tree, returned at the end
				var nextRank                     = MaxChildren      //the number of children in the currently built Node
				var leaves   :Array[BTreeSeq[U]] = null             //the children array for the next built Node
				var prefixes :Array[Int]         = CompletePrefixes //the prefixes array for the next built Node
				var index                        = from             //position in the input array
				//The inner loop copies copies only Nodes, so we must stop earlier if we need to add incomplete leaves.
				// We guarantee this way that we won't end up with less than Rank values after the last built Node.
				val outerWhileStop = {
					val rem2 = size % (MaxChildren * MaxChildren)
					if (rem2 < Rank) until - Rank //don't leave a single leaf, let alone below Rank, for the main loop
					else if (rem2 <= MaxChildren) until - rem2
					else until
				}
				//we ensured it's either >= Rank, and we can add it at the end as a Leaf,
				// or that (outerWhileStop - from) / MaxChildren < MaxChildren,
				// and the main loop will deal with it within a Node.
				val rem = (outerWhileStop - from) % MaxChildren
				//Build full, balanced Nodes of level 2 in a loop and append them to tree
				while ({
					val remainingKeys       = outerWhileStop - index
					val remainingLeafCount  = remainingKeys / MaxChildren
					val innerWhileStop :Int = {
						if (remainingLeafCount < MaxChildren)
							if (rem > 0) {
								nextRank = remainingLeafCount + 1
								//initialize the prefix lengths of the last node, taking care to balance it
								prefixes = new Array[Int](nextRank - 1)
								val fullLeaves = if (rem >= Rank) nextRank - 1 else nextRank - 2
								arraycopy(CompletePrefixes, 0, prefixes, 0, fullLeaves)
								if (rem >= Rank)
									outerWhileStop - rem
								else {
									prefixes(fullLeaves) = remainingKeys - Rank
									outerWhileStop - rem - MaxChildren
								}
							} else {
								//Because nextRank < MaxChildren, our prefixes for a complete Node are now invalid.
								nextRank = remainingLeafCount
								prefixes = SemiCompletePrefixes(nextRank - 1)
								outerWhileStop
							}
						else //nextRank == MaxChildren, prefixes is already initialized for a full node and can be reused.
							index + MaxChildren * MaxChildren
					}
					var leafCount = 0
					leaves = new Array[BTreeSeq[U]](nextRank)
					//I heard you like while loops, so I put a while loop in your while loop condition
					while (index < innerWhileStop) {
						leaves(leafCount) = new Leaf(ErasedArray.copyOfRange(array, index, index + MaxChildren))
						leafCount += 1
						index += MaxChildren
					}

					if (leafCount == nextRank - 1) {
						leaves(leafCount) = new Leaf(ErasedArray.copyOfRange(array, outerWhileStop - rem, outerWhileStop))
						index = outerWhileStop
					} else if (leafCount == nextRank - 2) {
						val secondLast = ErasedArray.copyOfRange(array, index, outerWhileStop - Rank)
						val last       = ErasedArray.copyOfRange(array, outerWhileStop - Rank, outerWhileStop)
						leaves(leafCount)     = new Leaf(secondLast)
						leaves(leafCount + 1) = new Leaf(last)
						index = outerWhileStop
					}
					tree ++= new Node(leaves, prefixes)
					index < outerWhileStop
				}) {}

				if (outerWhileStop < until)
					tree ++= new Leaf(ErasedArray.copyOfRange(array, outerWhileStop, until))
				tree
			}
	}

	/** Appends the elements from the range `[start, start + length)` from the given array, by slicing the array
	  * into segments for individual leaves, and then adding those leaves in whole groups as `Node`s of depth 2
	  * (first level), for as long as possible. Much faster than adding individual elements.
	  */
	private def prependedSlice[U >: E](array :Array[U], from :Int, until :Int) :BTreeSeq[U] = (until - from) match {
		case 0                                       => this
		case 1                                       => prepended(array(from))
		case _ if !array.isInstanceOf[Array[AnyRef]] => prependedAll(ArrayIterator.slice(array, from, until))
		case size                                    =>
			if (size <= MaxChildren)
				prependedAll(new Leaf(ErasedArray.copyOfRange(array, from, until)))
			else {
				//Build the result by creating Nodes of level 2 manually using mutable state.
				var tree     :BTreeSeq[U]        = this             //the built tree, returned at the end
				var nextRank                     = MaxChildren      //the number of children in the currently built Node
				var leaves   :Array[BTreeSeq[U]] = null             //the children array for the next built Node
				var prefixes :Array[Int]         = CompletePrefixes //the prefixes array for the next built Node
				var index                        = until            //position in the input array
				//The inner loop copies copies only Nodes, so we must stop earlier if we need to add incomplete leaves.
				// We guarantee this way that we won't end up with less than Rank values after the last built Node.
				val outerWhileStop = {
					val rem2 = size % (MaxChildren * MaxChildren)
					if (rem2 < Rank) from + Rank
					else if (rem2 <= MaxChildren) from + rem2
					else from
				}
				//we ensured it's either >= Rank, and we can add it at the end as a Leaf,
				// or that outerWhileStop / MaxChildren < MaxChildren, and the main loop will deal with it within a Node.
				val rem = (until - outerWhileStop) % MaxChildren
				//Build full, balanced Nodes of level 2 in a loop and append them to tree
				while ({
					val remainingKeys       = index - outerWhileStop
					val remainingLeafCount  = remainingKeys / MaxChildren
					val innerWhileStop :Int = {
						if (remainingLeafCount < MaxChildren)
							if (rem > 0) {
								nextRank = remainingLeafCount + 1
								//initialize the prefix lengths of the last node, taking care to balance it
								var fullLeaves = remainingLeafCount
								var len = rem
								prefixes = new Array[Int](remainingLeafCount)
								if (rem >= Rank)
									prefixes(0) = rem
								else {
									fullLeaves = nextRank - 2
									len = MaxChildren + rem
									prefixes(0) = Rank
									if (nextRank > 2)
										prefixes(1) = len
								}
								var i = nextRank - fullLeaves
								while (i < nextRank - 1) {
									len += MaxChildren
									prefixes(i) = len
									i += 1
								}
								index - fullLeaves * MaxChildren
							} else {
								//Because nextRank < MaxChildren, our prefixes for a complete Node are now invalid.
								nextRank = remainingLeafCount
								prefixes = SemiCompletePrefixes(nextRank - 1)
								outerWhileStop
							}
						else //nextRank == MaxChildren, prefixes is already initialized for a full node and can be reused.
							index - MaxChildren * MaxChildren
					}
					var leafCount = nextRank
					leaves = new Array[BTreeSeq[U]](nextRank)
					//I heard you like while loops, so I put a while loop in your while loop condition
					while (index > innerWhileStop) {
						index -= MaxChildren
						leafCount -= 1
						leaves(leafCount) = new Leaf(ErasedArray.copyOfRange(array, index, index + MaxChildren))
					}

					if (leafCount == 1) {
						leaves(0) = new Leaf(ErasedArray.copyOfRange(array, outerWhileStop, index))
						index     = outerWhileStop
					} else if (leafCount == 2) {
						val first  = ErasedArray.copyOfRange(array, outerWhileStop, outerWhileStop + Rank)
						val second = ErasedArray.copyOfRange(array, outerWhileStop + Rank, index)
						leaves(0) = new Leaf(first)
						leaves(1) = new Leaf(second)
						index = outerWhileStop
					}
					tree = tree prependedAll new Node(leaves, prefixes)
					index > outerWhileStop
				}) {}

				if (outerWhileStop > from)
					tree = tree prependedAll new Leaf(ErasedArray.copyOfRange(array, from, outerWhileStop))
				tree
			}
	}


	protected def appendedAll[U >: E](elems :Iterator[U]) :BTreeSeq[U] = {
		if (!elems.hasNext)
			this
		else {
			val key = elems.next()
			if (!elems.hasNext)
				appended(key)
			else {
				//Pack all elements in a mutable, exploded array list.
				// The elements are added to the accumulator tree only as balanced Nodes.
				var tree :BTreeSeq[U] = this                 //the growing tree
				var keys = ErasedArray.ofDim[U](MaxChildren) //a mutable buffer where appended keys are stored
				keys(0) = key
				var keyCount = 1                             //the current number of keys in array keys
				var leaves :Array[BTreeSeq[U]] = null        //a second level buffer to which complete keys arrays are written.
				var leafCount = 0                            //the current number of buffered key arrays
				while (elems.hasNext) {
					if (keyCount == 0)
						keys = ErasedArray.ofDim[U](MaxChildren)
					keys(keyCount) = elems.next()
					keyCount += 1
					if (keyCount == MaxChildren) { //flush the whole element array to Leaf buffer
						if (leafCount == 0)
							leaves = new Array[BTreeSeq[U]](MaxChildren)
						leaves(leafCount) = new Leaf(keys)
						keyCount = 0
						leafCount += 1
						if (leafCount == MaxChildren) {
							tree ++= new Node(leaves, CompletePrefixes, MaxChildren * MaxChildren)
							leafCount = 0
						}
					}
				}
				if (leafCount == 0) keyCount match {
					case 0           => tree
					case 1           => tree appended keys(0)
					case MaxChildren => tree appendedAll new Leaf(keys)
					case _           => tree appendedAll new Leaf(ErasedArray.copyOf(keys, keyCount))
				} else {
					val completeUntil =
						if (keyCount == 0)
							leafCount
						else {
							if (keyCount >= Rank) {
								leaves(leafCount) = new Leaf(ErasedArray.copyOf(keys, keyCount))
								leafCount += 1
								leafCount - 1
							} else {
								val sibling      = leaves(leafCount - 1).asInstanceOf[Leaf[U]]
								val siblingSize  = MaxChildren - Rank + keyCount
								val siblingKeys  = sibling.values.asInstanceOf[Array[U]]
								leaves(leafCount - 1) = new Leaf(ErasedArray.copyOf(siblingKeys, siblingSize))
								leaves(leafCount)     = new Leaf ({
									val a = ErasedArray.ofDim[U](Rank)
									arraycopy(siblingKeys, siblingSize, a, 0, Rank - keyCount)
									arraycopy(keys, 0, a, Rank - keyCount, keyCount)
									a
								})
								leafCount += 1
								leafCount - 2
							}
						}
					if (leafCount == MaxChildren)
						tree appendedAll semiCompleteNode(leaves, completeUntil)
					else if (leafCount == 1)
						tree appendedAll leaves(0)
					else {
						val children = new Array[BTreeSeq[E]](leafCount)
						arraycopy(leaves, 0, children, 0, leafCount)
						tree appendedAll semiCompleteNode(children, completeUntil)
					}
				}
			}
		}
	}


	//we'd need a reverseIterator for this
/*
	protected def prependedAll[U >: E](elems :Iterator[U]) :BTreeSeq[U] = {
		if (!elems.hasNext)
			this
		else {
			val key = elems.next()
			if (!elems.hasNext)
				new Singleton(key)
			else {
				//Pack all elements in a mutable, exploded array list.
				// The elements are added to the accumulator tree only as balanced Nodes.
				var tree :BTreeSeq[U] = this                 //the growing tree
				var keys = ErasedArray.ofDim[U](MaxChildren) //a mutable buffer where appended keys are stored
				keys(0) = key
				var keyCount = 1                             //the current number of keys in array keys
				var leaves :Array[BTreeSeq[U]] = null        //a second level buffer to which complete keys arrays are written.
				var leafCount = 0                            //the current number of buffered key arrays
//				var total = 1
				while (elems.hasNext) {
					if (keyCount == 0)
						keys = ErasedArray.ofDim[U](MaxChildren)
					keys(keyCount) = elems.next()
					keyCount += 1
					if (keyCount == MaxChildren) { //flush the whole element array to Leaf buffer
						if (leaves == null)
							leaves = new Array[BTreeSeq[U]](MaxChildren)
						leaves(leafCount) = new Leaf(keys)
						keyCount = 0
						leafCount += 1
						if (leafCount == MaxChildren) {
							tree ++= new Node(leaves)
							leafCount = 0
						}
					}
				}
				if (leafCount == 0) keyCount match {
					case 0           => tree
					case 1           => tree appended keys(0)
					case MaxChildren => tree appendedAll new Leaf(keys)
					case _           => tree appendedAll new Leaf(ErasedArray.copyOf(keys, keyCount))
				} else {
					if (keyCount > 0) {
						if (keyCount == MaxChildren)
							leaves(leafCount) = new Leaf(keys)
						else if (keyCount >= Rank)
							leaves(leafCount) = new Leaf(ErasedArray.copyOf(keys, keyCount))
						else {
							val sibling      = leaves(leafCount - 1).asInstanceOf[Leaf[U]]
							val siblingSize  = MaxChildren - Rank + keyCount
							val siblingKeys  = sibling.values.asInstanceOf[Array[U]]
							leaves(leafCount - 1) = new Leaf(ErasedArray.copyOf(siblingKeys, siblingSize))
							leaves(leafCount)     = new Leaf ({
								val a = ErasedArray.copyOfRange(siblingKeys, siblingSize, siblingSize + Rank)
								arraycopy(keys, 0, a, Rank - keyCount, keyCount)
								a
							})
						}
						leafCount += 1
					}
					if (leafCount == MaxChildren)
						tree appendedAll new Node(leaves)
					else if (leafCount == 1)
						tree appendedAll leaves(0)
					else {
						val children = new Array[BTreeSeq[E]](leafCount)
						arraycopy(leaves, 0, children, 0, leafCount)
						tree appendedAll new Node(children)
					}
				}
			}
		}
	}
*/


	protected def appendedByOne[U >: E](elems :Iterator[U]) :BTreeSeq[U] = {
		val growth            = Box[BTreeSeq[U]]
		var tree :BTreeSeq[U] = this
		while (elems.hasNext)
			tree = grow(tree.appended(elems.next(), growth), growth)
		tree
	}

	protected def prependedByOne[U >: E](elems :Iterator[U]) :BTreeSeq[U] = {
		val growth            = Box[BTreeSeq[U]]
		var i                 = 0
		var tree :BTreeSeq[U] = this
		while (elems.hasNext) {
			tree = grow(tree.inserted(i, elems.next(), growth), growth)
			i += 1
		}
		tree
	}


	override def appendedAll[U >: E](elems :IterableOnce[U]) :BTreeSeq[U] = elems.knownSize match {
		case 0 => this
		case 1 => elems match {
			case other :BTreeSeq[U] => appendedAll(other)
			case items :Iterable[U] => appended(items.head)
			case _                  => appended(elems.iterator.next())
		}
		case size if size > 0 => elems match {
			case other :BTreeSeq[U]            => appendedAll(other)
			case seq :collection.IndexedSeqOps[U, IterableOnce, IterableOnce[U]] @unchecked => elems match {
				case arr :ArraySeq[U] if arr.unsafeArray.isInstanceOf[Array[AnyRef]]            =>
					val array = arr.unsafeArray.asInstanceOf[Array[U]]
					if (array.length <= MaxChildren) appendedAll(new Leaf(array))
					else appendedSlice(array, 0, array.length)
				case arr :ArrayIterableOnce[U] if arr.unsafeArray.isInstanceOf[Array[AnyRef]] =>
					val array = arr.unsafeArray.asInstanceOf[Array[U]]
					if (array.length == size && array.length <= MaxChildren && arr.isImmutable)
						appendedAll(new Leaf(array))
					else
						appendedSlice(array, arr.startIndex, arr.startIndex + size)
				case arr :mutable.ArraySeq[U] if arr.array.isInstanceOf[Array[AnyRef]]          =>
					appendedSlice(arr.array.asInstanceOf[Array[U]])
				case _ if size <= MaxChildren                                                   =>
					val array = ErasedArray.ofDim[U](size)
					seq.copyToArray(array, 0, size)
					appendedAll(new Leaf(array))
				case _ =>
					val mySize = length
					//avoid infinite recursion with Empty.appendedAll
					if (mySize > 0 & size > mySize * ConvertToBTreeOnConcatFactor)
						BTreeSeq.from(elems) prependedAll this
					else //todo: take advantage of the known size
						appendedAll(seq.iterator)
			}
			case items :Iterable[U] if size <= MaxChildren                                      =>
				val array = ErasedArray.ofDim[U](size)
				items.copyToArray(array, 0, size)
				appendedAll(new Leaf(array))
//			case _
//				val growth = Box[BTreeSeq[U]]
//				((this :BTreeSeq[U]) /: seq) {
//					(tree, elem) => grow(tree.appended(elem, growth), growth)
//				}
			case _  => //todo: take advantage of the known size
				appendedAll(elems.iterator)
		}
		case _ =>
			appendedAll(elems.iterator)
	}

	override def prependedAll[U >: E](elems :IterableOnce[U]) :BTreeSeq[U] = elems.knownSize match {
		case 0 => this
		case 1 => elems match {
			case other :BTreeSeq[U] => prependedAll(other)
			case items :Iterable[U] => prepended(items.head)
			case _                  => prepended(elems.iterator.next())
		}
		case size if size > 0 => elems match {
			case other :BTreeSeq[U] => prependedAll(other)
			case seq :collection.IndexedSeqOps[U, _, _] => seq match {
				case arr :ArraySeq[U] if arr.unsafeArray.isInstanceOf[Array[AnyRef]]            =>
					val array = arr.unsafeArray.asInstanceOf[Array[U]]
					if (array.length <= MaxChildren) prependedAll(new Leaf(array))
					else prependedSlice(array, 0, array.length)

				case arr :ArrayIterableOnce[U] if arr.unsafeArray.isInstanceOf[Array[AnyRef]] =>
					val array = arr.unsafeArray.asInstanceOf[Array[U]]
					if (array.length == size && array.length <= MaxChildren && arr.isImmutable)
						prependedAll(new Leaf(array))
					else
						prependedSlice(array, arr.startIndex, arr.startIndex + size)
				case arr :mutable.ArraySeq[U] if arr.array.isInstanceOf[Array[AnyRef]]          =>
					prependedSlice(arr.array.asInstanceOf[Array[U]])
				case _ if size <= MaxChildren                                                   =>
					val array = ErasedArray.ofDim[U](size)
					seq.copyToArray(array, 0, size)
					prependedAll(new Leaf(array))
				case _                                                                          =>
					val mySize = length
					//avoid infinite recursion with Empty.appendedAll
					if (size > mySize * ConvertToBTreeOnConcatFactor)
						BTreeSeq.from(elems) appendedAll this
					else { //todo: take advantage of the known size
						val growth = Box[BTreeSeq[U]]
						(seq :\ (this :BTreeSeq[U])) {
							(elem, tree) => grow(tree.prepended(elem, growth), growth)
						}
					}
			}
			case items :Iterable[U] if size <= MaxChildren =>
				val array = ErasedArray.ofDim[U](size)
				items.copyToArray(array, 0, size)
				prependedAll(new Leaf(array))
			case _ => //todo: take advantage of the known size
				prependedByOne(elems.iterator)
		}
		case _ => prependedByOne(elems.iterator)
	}



	/** Drops the first `n` children/keys from this node. The result isn't rebalanced.
	  * @param n the number of dropped leading nodes, `0 <= n < rank`.
	  */
	def dropChildren(n :Int) :BTreeSeq[E] = ??!

	/** Takes the first `n` children/keys from this node. The result isn't rebalanced.
	  * @param n the number of dropped leading nodes, `1 < n < rank`.
	  */
	def takeChildren(n :Int) :BTreeSeq[E] = ??!

	/** Implementation method for `Seq.slice` taking helper arguments. This operation does ''not'' grow the tree -
	  * `surplus` is just provided for its reuse internally. Argument `height` should be initialized as `this.depth`,
	  * to avoid its repeated re-calculation during the recursion.
	  */
	def slice[U >: E](from :Int, until :Int, height :Int, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] = ??!
//		take(until, height, surplus).drop(from, height, surplus)

	/** Implementation method for `Seq.take` taking helper arguments. This operation does ''not'' grow the tree -
	  * `surplus` is just provided for its reuse internally. Argument `height` should be initialized as `this.depth`,
	  * to avoid its repeated re-calculation during the recursion.
	  * @param n       the number of elements to take, `0 <= n < length`
	  * @param height  the precomputed height (depth) of this tree.
	  * @param surplus an out parameter holder for an optional second node created if this node is split in two.
	  */
	def take[U >: E](n :Int, height :Int, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] = ??! //never called for Empty or Singleton

	/** Implementation method for `Seq.drop` taking helper arguments. This operation does ''not'' grow the tree -
	  * `surplus` is just provided for its reuse internally. Argument `height` should be initialized as `this.depth`,
	  * to avoid its repeated re-calculation during the recursion.
	  * @param n       the number of elements to drop, `0 <= n < length`
	  * @param height  the precomputed height (depth) of this tree.
	  * @param surplus a reusable holder used to return a second node when one is split in two.
	  *                It is always empty when returning from this method.
	  */
	def drop[U >: E](n :Int, height :Int, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] = ??! //never called for Empty or Singleton

	override def iterableFactory :SeqFactory[BTreeSeq] = BTreeSeq

	def addString(b :StringBuilder, start :String, sep :String, end :String) :b.type

	def dumpString :String = dumpString(new StringBuilder ++= "BTreeSeq").result()
	def dumpString(sb :StringBuilder) :sb.type

	protected override def className :String = "BTreeSeq"


	//todo: remove these once the bug is fixed in SeqOps
	override def startsWith[B >: E](that :IterableOnce[B], offset :Int) :Boolean =
		offset >= 0 && offset <= length && super.startsWith(that, offset)

	override def indexOfSlice[B >: E](that :collection.Seq[B], from :Int) :Int =
		if (from > length) -1
		else super.indexOfSlice(that, 0 max from)

}




object BTreeSeq extends StrictOptimizedSeqFactory[BTreeSeq] {

	override def from[E](source :IterableOnce[E]) :BTreeSeq[E] = Empty appendedAll source

/*
	override def from[E](source :IterableOnce[E]) :BTreeSeq[E] = 		source match {
		case tree :BTreeSeq[E] => tree
		case seq :ArraySeq[E]  => from(seq)
		case seq :mutable.ArraySeq[E] => from(seq.array.asInstanceOf[Array[E]])
		case seq :AbstractPassedArray[E] if seq.elems.isInstanceOf[Array[AnyRef]] && seq.length == seq.elems.length =>
			from(seq.elems)
		case _ =>
			val size = source.knownSize
			if (size == 0)
				Empty
			else if (size == 1) source match {
				case items :Iterable[E] => new Singleton(items.head)
				case _                  => new Singleton(source.iterator.next())
			} else if (size > 0 & size <= MaxChildren) {
				source match {
					case seq   :ArraySeq[E] if seq.unsafeArray.isInstanceOf[Array[AnyRef]] =>
						new Leaf(seq.unsafeArray.asInstanceOf[Array[E]])
					case seq   :mutable.ArraySeq[E] if seq.array.isInstanceOf[Array[AnyRef]] =>
						new Leaf(ErasedArray.copyOf(seq.array.asInstanceOf[Array[E]]))
					case items :Iterable[E] =>
						val array = ErasedArray.ofDim[E](size)
						items.copyToArray(array, 0, size)
						new Leaf(array)
					case _ =>
						val array = ErasedArray.ofDim[E](size)
						source.iterator.copyToArray(array, 0, size)
						new Leaf(array)
				}
			} else {
				//Pack all elements in a mutable, exploded array list.
				// The elements are added to the accumulator tree only as balanced Nodes.
				var tree :BTreeSeq[E] = Empty         //growing tree
				var keys :Array[E] = null             //a mutable buffer where appended keys are stored
				var key  = 0                          //the current number of keys in array keys
				var leaves :Array[BTreeSeq[E]] = null //a second level buffer to which complete keys arrays are written.
				var leaf = 0                          //the current number of buffered key arrays
				val items = source.iterator
				while (items.hasNext) {
					if (keys == null)
						keys = ErasedArray.ofDim[E](MaxChildren)
					keys(key) = items.next()
					key += 1
					if (key == MaxChildren) { //flush the whole element array to Leaf buffer
						if (leaves == null)
							leaves = new Array[BTreeSeq[E]](MaxChildren)
						leaves(leaf) = new Leaf(keys)
						keys = null
						key = 0
						leaf += 1
						if (leaf == MaxChildren) {
							tree = tree appendedAll new Node(leaves)
							leaves = null
							leaf = 0
						}
					}
				}
				if (leaf == 0) key match {
					case 0           => tree
					case 1           => tree appended keys(0)
					case MaxChildren => tree appendedAll new Leaf(keys)
					case _           => tree appendedAll new Leaf(ErasedArray.copyOf(keys, key))
				} else {
					if (key > 0) {
						if (key == MaxChildren)
							leaves(leaf) = new Leaf(keys)
						else if (key >= Rank)
							leaves(leaf) = new Leaf(ErasedArray.copyOf(keys, key))
						else {
							val sibling      = leaves(leaf - 1).asInstanceOf[Leaf[E]]
							val siblingSize  = MaxChildren - Rank + key
							val siblingKeys  = sibling.values.asInstanceOf[Array[E]]
							leaves(leaf - 1) = new Leaf(ErasedArray.copyOf(siblingKeys, siblingSize))
							leaves(leaf)     = new Leaf ({
								val a = ErasedArray.copyOfRange(siblingKeys, siblingSize, siblingSize + Rank)
								arraycopy(keys, 0, a, Rank - key, key)
								a
							})
						}
						leaf += 1
					}
					if (leaf == MaxChildren)
						tree appendedAll new Node(leaves)
					else if (leaf == 1)
						tree appendedAll leaves(0)
					else {
						val children = new Array[BTreeSeq[E]](leaf)
						arraycopy(leaves, 0, children, 0, leaf)
						tree appendedAll new Node(children)
					}
				}
			}
	}


	private def from[E](seq :ArraySeq[E]) :BTreeSeq[E] = seq.length match {
		case 0 => Empty
		case 1 => new Singleton(seq.head)
		case _ if !seq.unsafeArray.isInstanceOf[Array[AnyRef]] => from(seq.iterator)
		case n if n <= MaxChildren =>
			new Leaf(seq.unsafeArray.asInstanceOf[Array[E]])
		case _ =>
			from(seq.unsafeArray.asInstanceOf[Array[E]])
	}

	def from[E](array :Array[E]) :BTreeSeq[E] = array.length match {
		case 0    => Empty
		case 1    => new Singleton(array(0))
		case _ if !array.isInstanceOf[Array[AnyRef]] => from(array.iterator)
		case size =>
			if (size <= MaxChildren)
				new Leaf(array)
			else {
				//Build the result by creating Nodes of level 2 manually using mutable state.
				var tree   :BTreeSeq[E] = Empty        //the built tree, returned at the end
				var nextRank = MaxChildren             //the number of children in the currently built Node
				var leaves :Array[BTreeSeq[E]] = null  //the children array for the next built Node
				var prefixes :Array[Int] = null        //the prefixes array for the next built Node
				var start = 0                          //position in the input array
				//The inner loop copies copies only Nodes, so we must stop earlier if we need to add incomplete leaves.
				// We guarantee this way that we won't end up with less than Rank values after the last built Node.
				val outerWhileStop = {
					val rem = size % MaxChildren
					val rem2 = size % (MaxChildren * MaxChildren)
					if (rem != 0 & rem == rem2)
						if (rem < Rank) size - Rank else Rank
					else size
				}
				//we ensured it's either >= Rank, and we can add it at the end as a Leaf,
				// or that outerWhileStop / MaxChildren < MaxChildren, and the main loop will deal with it within a Node.
				val rem = outerWhileStop % MaxChildren
				//Build full, balanced Nodes of level 2 in a loop and append them to tree
				while ({
					val remainingKeys = outerWhileStop - start
					val remainingLeafCount = remainingKeys / MaxChildren
					val innerWhileStop :Int = {
						if (remainingLeafCount < MaxChildren)
							if (rem > 0) {
								nextRank = remainingLeafCount + 1
								//initialize the prefix lengths of the last node, taking core to balance it
								prefixes = new Array[Int](nextRank - 1)
								val fullLeaves = if (rem >= Rank) nextRank - 1 else nextRank - 2
								var i = 0; var len = 0
								while (i < fullLeaves) {
									len += MaxChildren
									prefixes(i) = len
									i += 1
								}
								if (rem >= Rank) {
									prefixes(fullLeaves) = rem
									remainingKeys - rem - MaxChildren
								} else {
									prefixes(fullLeaves) = MaxChildren - Rank
									prefixes(fullLeaves + 2) = Rank
									remainingKeys - rem - (MaxChildren << 1)
								}
							} else {
								//Because nextRank < MaxChildren, our prefixes for a complete Node are now invalid.
								nextRank = remainingLeafCount
								prefixes = null
								start + MaxChildren * nextRank
							}
						else //nextRank == MaxChildren, prefixes is already initialized for a full node and can be reused.
							start + MaxChildren * MaxChildren
					}
					if (prefixes == null) {
						prefixes = new Array[Int](nextRank - 1)
						var i = 0; var len = 0
						while (i < nextRank - 1) {
							len += MaxChildren
							prefixes(i) = len
							i += 1
						}
					}
					var leafCount = 0
					leaves = new Array[BTreeSeq[E]](nextRank)
					//I heard you like while loops, so I put a while loop in your while loop condition
					while ({
						val leaf = ErasedArray.ofDim[E](MaxChildren)
						arraycopy(array, 0, leaf, 0, MaxChildren)
						leaves(leafCount) = new Leaf(leaf)
						leafCount += 1
						start += MaxChildren
						start < innerWhileStop
					}) {}

					if (leafCount == nextRank - 1) {
						val last = ErasedArray.ofDim[E](rem)
						arraycopy(array, 0, last, 0, rem)
						leaves(leafCount) = new Leaf(last)
						start = outerWhileStop
					} else if (leafCount == nextRank - 2) {
						val secondLast = ErasedArray.ofDim[E](MaxChildren - Rank)
						val last = ErasedArray.ofDim[E](Rank)
						arraycopy(array, start, secondLast, 0, MaxChildren - Rank)
						start += MaxChildren - Rank
						arraycopy(array, start, last, 0, Rank)
						leaves(leafCount) = new Leaf(secondLast)
						leaves(leafCount + 1) = new Leaf(last)
						start = outerWhileStop
					}
					tree = tree appendedAll new Node(leaves, prefixes)
					start < outerWhileStop
				}) {}

				if (outerWhileStop < size)
					tree ++= new Leaf(ErasedArray.copyOfRange(array, outerWhileStop, size - outerWhileStop))
				tree
			}
	}
*/


	override def empty[E] :BTreeSeq[E] = Empty
	override def newBuilder[E] :Builder[E, BTreeSeq[E]] = new BTreeSeqBuilder[E]


	/** Combines a result of some tree operation `first` with its optional sibling created during splitting.
	  * Clears `second` before returning.
	  * @return `second.opt.mapOrElse(new Node(first, _), first)`.
	  */
	private def grow[E](first :BTreeSeq[E], second :Box[BTreeSeq[E]]) :BTreeSeq[E] =
		second.maybeRemove() match {
			case Yes(node) => new Node(first, node)
			case _         => first
		}

	/** Handles the case of a tree splitting into two after growing. If `surplus` is not `null`,
	  * then it is set to `second` and `first` is returned. Otherwise a new `Node(first, second)` is returned.
	  */
	private def grow[E](first :BTreeSeq[E], second :BTreeSeq[E], surplus :Box[BTreeSeq[E]]) :BTreeSeq[E] =
		if (surplus == null)
			new Node(first, second)
		else {
			surplus := second
			first
		}



	@SerialVersionUID(Ver)
	private object Empty extends AbstractSeq[Nothing] with BTreeSeq[Nothing] with EmptyIndexedSeqOps.Generic[BTreeSeq] {
		override def rank = 0
//		override def length = 0
//		override def head :Nothing = throw new NoSuchElementException("BTreeSeq().head")
//		override def last :Nothing = throw new NoSuchElementException("BTreeSeq().last")
//		override def apply(index :Int) = outOfBounds_!(index.toString + " out of 0")
//		override def updated[U >: Nothing](i :Int, elem :U) =
//			outOfBounds_!(i.toString + " out of 0")

		override def inserted[U >: Nothing](i :Int, elem :U) =
			if (i == 0) new Singleton(elem)
			else outOfBounds_!(i.toString + " out of 0")

		override def removed(index :Int) :BTreeSeq[Nothing] =
			outOfBounds_!(index.toString + " out of 0")

		//		override def appended[U >: Nothing](elem :U) = new Singleton(elem)
		//		override def prepended[U >: Nothing](elem :U) = new Singleton(elem)
		override def appendedAll[U >: Nothing](elems :BTreeSeq[U]) :BTreeSeq[U] = elems
		override def prependedAll[U >: Nothing](elems :BTreeSeq[U]) :BTreeSeq[U] = elems

		override def appendedAll[B >: Nothing](suffix :IterableOnce[B]) =
			((this :BTreeSeq[B]) /: suffix.toBasicOps)(_ :+ _)

		override def prependedAll[B >: Nothing](suffix :IterableOnce[B]) =
			((this :BTreeSeq[B]) /: suffix.toBasicOps)(_ :+ _)

		//		override def slice(from :Int, until :Int) = this
//		override def drop(n :Int) = this
//		override def take(n :Int) = this
//
//		override def iterator :Iterator[Nothing] = Iterator.empty
//
//		override def addString(b :StringBuilder, start :String, sep :String, end :String) :b.type = b ++= start ++= end
		override def one[T](elem :T) :BTreeSeq[T] = new Singleton(elem)
		override def dumpString(sb :StringBuilder) :sb.type = sb ++= "()"
	}


	@SerialVersionUID(Ver)
	private final class Singleton[+E](override val head :E)
		extends AbstractSeq[E] with BTreeSeq[E] with SingletonIndexedSeqOps.Generic[E, BTreeSeq]
	{
		override def rank = 1
		override def length = 1
		override def apply(i :Int) =
			if (i == 0) head
			else outOfBounds_!(i.toString + " out of 1")

		override def updated[U >: E](index :Int, elem :U) =
			if (index == 0) new Singleton(elem)
			else outOfBounds_!(index.toString + " out of 1")

		override def inserted[U >: E](index :Int, elem :U) = index match {
			case 0 => new Leaf(ErasedArray.two(elem, head))
			case 1 => new Leaf(ErasedArray.two(head, elem))
			case _ => outOfBounds_!(index.toString + " out of 1")
		}
		override def removed(index :Int) :BTreeSeq[E] =
			if (index == 0) Empty
			else outOfBounds_!(index.toString + " out of 1")

		override def appended[U >: E](elem :U) = new Leaf(ErasedArray.two(head, elem))
		override def prepended[U >: E](elem :U) = new Leaf(ErasedArray.two(elem, head))
		override def appendedAll[U >: E](elems :BTreeSeq[U]) = if (elems.isEmpty) this else elems prepended head
		override def prependedAll[U >: E](elems :BTreeSeq[U]) = if (elems.isEmpty) this else elems appended head
/*
		override def appendedAll[U >: T](elems :IterableOnce[U]) :BTreeSeq[U] = elems match {
			case elems :Iterable[U] if elems.knownSize == 0 => this
			case tree :BTreeSeq[U] => head +: tree
			case it :Iterator[U] if !it.hasNext => this
			case _ =>
				val b = newBuilder[U]
				b.sizeHint(elems, 1)
				(b += head ++= elems).result()
		}
		override def prependedAll[U >: T](elems :IterableOnce[U]) :BTreeSeq[U] = elems match {
			case elems :Iterable[U] if elems.isEmpty => this
			case tree :BTreeSeq[U] => tree :+ head
			case it :Iterator[U] if !it.hasNext => this
			case _ =>
				val b = newBuilder[U]
				b.sizeHint(elems, 1)
				(b ++= elems += head).result()
		}
*/
		override def slice(from :Int, until :Int) =
			if (from <= 0 & until >= 1) this else Empty

		override def take(n :Int) = if (n >= 1) this else Empty
		override def drop(n :Int) = if (n >= 1) Empty else this

		override def iterator :Iterator[E] = Iterator.single(head)

		override def addString(b :StringBuilder, start :String, sep :String, end :String) :b.type =
			b ++= start ++= head.toString ++= end

		override def dumpString(sb :StringBuilder) :sb.type = sb ++= "(" + head + ")"
	}



//	private trait Node[+T] extends BTreeSeq[T]
	private def Leaf[U](elems :IterableOnce[U], size :Int) :Leaf[U] = elems match {
		case leaf  :Leaf[U] =>
			leaf
		//We use System.arraycopy, so we can't just use any array, because copying from int[] to Object[] will fail.
		case seq   :ArraySeq[_] if seq.unsafeArray.isInstanceOf[Array[AnyRef]] =>
			new Leaf[U](seq.unsafeArray.asInstanceOf[Array[U]])
		case items :Iterable[U] =>
			new Leaf(items.toArray[U](ClassTag.Any.castParam[U]))
		case _ if size >= 0     =>
			val values = ErasedArray.ofDim[U](size)
			elems.iterator.copyToArray(values, 0, size)
			new Leaf(values)
		case _                  =>
			new Leaf(elems.iterator.toArray[U](ClassTag.Any.castParam[U]))
	}


	@SerialVersionUID(Ver)
	private final class Leaf[+E](keys :Array[E]) extends AbstractSeq[E] with BTreeSeq[E] with ArrayIterableOnce[E] {
		assert(keys.length <= MaxChildren)

		private[sugar] override def unsafeArray :Array[_] = keys
		private[sugar] override def startIndex  :Int = 0
		private[sugar] override def isImmutable :Boolean = true

		def values :IArray[E] = keys.asInstanceOf[IArray[E]]

		override def rank = keys.length
		override def length :Int = keys.length

		override def apply(i :Int) = keys(i)

		override def updated[U >: E](i :Int, elem :U) = {
			val rank = keys.length
			val values = ErasedArray.ofDim[U](rank)
			arraycopy(keys, 0, values, 0, rank)
			values(i) = elem
			new Leaf(values)
		}

		override def inserted[U >: E](index :Int, elem :U, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] = {
			val rank = keys.length
			if (rank < MaxChildren) {
				val values = ErasedArray.ofDim[U](rank + 1)
				arraycopy(keys, 0, values, 0, index)
				values(index) = elem
				arraycopy(keys, index, values, index + 1, rank - index)
				new Leaf(values)
			} else if (index < Rank) {
				val init = ErasedArray.ofDim[U](Rank)
				arraycopy(keys, 0, init, 0, index)
				init(index) = elem
				arraycopy(keys, index, init, index + 1, Rank - index - 1)
				grow(new Leaf(init), dropChildren(Rank - 1), surplus)
			} else {
				val adjusted = index - Rank
				val tail = ErasedArray.ofDim[U](Rank)
				arraycopy(keys, Rank, tail, 0, adjusted)
				tail(adjusted) = elem
				arraycopy(keys, index, tail, adjusted + 1, rank - index)
				grow(takeChildren(Rank), new Leaf(tail), surplus)
			}
		}

		override def inserted[U >: E](index :Int, elem :U) :BTreeSeq[U] =
			if (index < 0 | index > keys.length)
				outOfBounds_!(index.toString + " out of " + keys.length)
			else
				inserted(index, elem, null)

		override def removed(index :Int) :BTreeSeq[E] = {
			val rank = keys.length
			if (rank == 2)
				index match {
					case 0 => new Singleton(keys(1))
					case 1 => new Singleton(keys(0))
					case _ => outOfBounds_!(index.toString + " out of " + rank)
				}
			else {
				val newKeys = ErasedArray.ofDim[E](rank - 1)
				arraycopy(keys, 0, newKeys, 0, index)
				arraycopy(keys, index + 1, newKeys, index, rank - index - 1)
				new Leaf(newKeys)
			}
		}

		override def appended[U >: E](value :U, surplus :Box[BTreeSeq[U]]) = {
			val rank = keys.length
			if (rank < MaxChildren) {
				val values = ErasedArray.ofDim[U](rank + 1)
				arraycopy(keys, 0, values, 0, rank)
				values(rank) = value
				new Leaf(values)
			} else {
				val tail = ErasedArray.ofDim[U](Rank)
				arraycopy(keys, Rank, tail, 0, Rank - 1)
				tail(Rank - 1) = value
				grow(takeChildren(Rank), new Leaf(tail), surplus)
			}
		}

		override def prepended[U >: E](value :U, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] = {
			val rank   = keys.length
			if (rank < MaxChildren) {
				val values = ErasedArray.ofDim[U](rank + 1)
				values(0) = value
				arraycopy(keys, 0, values, 1, rank)
				new Leaf(values)
			} else {
				val init = ErasedArray.ofDim[U](Rank)
				init(0) = value
				arraycopy(keys, 0, init, 1, Rank - 1)
				grow(new Leaf(init), dropChildren(Rank - 1), surplus)
			}
		}

		override def appended[U >: E](value :U) :BTreeSeq[U] = appended(value, null)
		override def prepended[U >: E](value :U) :BTreeSeq[U] = prepended(value, null)


		override def appendedSome[U >: E](successor :BTreeSeq[U], siblingCount :Int) :Leaf[U] = {
			val rank = keys.length
			val values = ErasedArray.ofDim[U](rank + siblingCount)
			arraycopy(keys, 0, values, 0, rank)
			arraycopy(successor.asInstanceOf[Leaf[U]].values, 0, values, rank, siblingCount)
			new Leaf(values)
		}
		override def prependedSome[U >: E](predecessor :BTreeSeq[U], siblingCount :Int) :Leaf[U] = {
			val rank = keys.length
			val values = ErasedArray.ofDim[U](rank + siblingCount)
			val predecessorValues = predecessor.asInstanceOf[Leaf[U]].values
			arraycopy(predecessorValues, predecessorValues.length - siblingCount, values, 0, siblingCount)
			arraycopy(keys, 0, values, siblingCount, rank)
			new Leaf(values)
		}

		def appendedAll[U >: E](elems :IterableOnce[U], surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] = {
//			def appendByOne(size :Int) = elems match { //good only for a total of MaxChildren * 2 because we don't use surplus
//				case it :Iterable[U] =>
//					((this :BTreeSeq[U]) /: it) {
//						(tree, elem) => tree.appended(elem, null)
//					}
//				case _               =>
//					((this :BTreeSeq[U]) /: elems.iterator) {
//						(tree, elem) => tree.appended(elem, null)
//					}
//			}
			val rank = keys.length
			val otherSize = elems.knownSize
			val total = rank + otherSize
			if (otherSize > 0) {
				if (total <= MaxChildren) {
					val values = ErasedArray.ofDim[U](total)
					arraycopy(keys, 0, values, 0, rank)
					copy(elems, values, rank, otherSize)
					new Leaf(values)
				} else if (total <= (MaxChildren << 1)) {
					val secondLeafSize =
						if (total <= MaxChildren + Rank) Rank else total - MaxChildren
					if (otherSize < Rank) {
						val shift = secondLeafSize - otherSize
						val tail = ErasedArray.ofDim[U](secondLeafSize)
						arraycopy(keys, rank - shift, tail, 0, shift)
						copy(elems, tail, shift, otherSize)
						grow(takeChildren(rank - shift), new Leaf(tail), surplus)
					} else if (rank < Rank | otherSize > MaxChildren) {
						val shift = otherSize - secondLeafSize
						val init = ErasedArray.ofDim[U](total - secondLeafSize)
						val tail = ErasedArray.ofDim[U](secondLeafSize)
						arraycopy(keys, 0, init, 0, rank)
						val array = elems match {
							case leaf :Leaf[U] => leaf.values.asInstanceOf[Array[U]]
							case seq :ArraySeq[_] if seq.unsafeArray.isInstanceOf[Array[AnyRef]] =>
								seq.unsafeArray.asInstanceOf[Array[U]]
							case it :Iterable[U] =>
								it.copyToArray(init, rank, shift)
								it.iterator.drop(shift).copyToArray(tail, 0, secondLeafSize)
								null
							case _ =>
								ErasedArray.from(elems)
						}
						if (array != null) {
							arraycopy(array, 0, init, rank, shift)
							arraycopy(array, shift, tail, 0, secondLeafSize)
						}
						grow(new Leaf(init), new Leaf(tail), surplus)
					} else
						grow(this, Leaf[U](elems, otherSize), surplus)
				} else
					super.appendedAll(elems)
//					appendByOne
			} else if (otherSize == 0)
				this
			else
				super.appendedAll(elems)
		}

		def prependedAll[U >: E](elems :IterableOnce[U], surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] = {
//			def prependByOne = elems match {
//				case seq :collection.IndexedSeq[U] =>
//					(seq :\ (this :BTreeSeq[U])) { (elem, tree) => tree.prepended(elem, null) }
//				case _ =>
//					val it = elems.iterator
//					var i = 0
//					var res :BTreeSeq[U] = this
//					while (it.hasNext) {
//						res = res.inserted(i, it.next(), null)
//						i += 1
//					}
//					res
//			}
			val rank = keys.length
			val otherSize = elems.knownSize
			if (otherSize > 0) {
				val total = rank + otherSize
				if (total <= MaxChildren) {
					val values = ErasedArray.ofDim[U](rank + otherSize)
					copy(elems, values, 0, otherSize)
					arraycopy(keys, 0, values, otherSize, rank)
					new Leaf(values)
				} else if (total <= (MaxChildren << 1)) {
					val firstLeafSize = if (total <= MaxChildren + Rank) Rank else total - MaxChildren
					if (otherSize < Rank) {
						val shift = firstLeafSize - otherSize
						val init = ErasedArray.ofDim[U](firstLeafSize)
						copy(elems, init, 0, otherSize)
						arraycopy(keys, 0, init, otherSize, shift)
						grow(new Leaf(init), dropChildren(shift), surplus)
					} else if (rank < Rank | otherSize > MaxChildren) {
						val shift = otherSize - firstLeafSize
						val init = ErasedArray.ofDim[U](firstLeafSize)
						val tail = ErasedArray.ofDim[U](total - firstLeafSize)
						arraycopy(keys, 0, tail, shift, rank)
						val array = elems match {
							case leaf :Leaf[U] => leaf.values.asInstanceOf[Array[U]]
							case seq :ArraySeq[_] if seq.unsafeArray.isInstanceOf[Array[AnyRef]] =>
								seq.unsafeArray.asInstanceOf[Array[U]]
							case it :Iterable[U] =>
								it.copyToArray(init, 0, firstLeafSize)
								//sadly we cannot assume that iterator advances by the number of elements written.
								elems.iterator.drop(firstLeafSize).copyToArray(tail, 0, shift)
								null
							case _ =>
								ErasedArray.from(elems)
						}
						if (array != null) {
							arraycopy(array, 0, init, 0, firstLeafSize)
							arraycopy(array, firstLeafSize, tail, 0, shift)
						}
						grow(new Leaf(init), new Leaf(tail), surplus)
					} else
						grow(Leaf(elems, otherSize), this, surplus)
				} else //surplus must be null, but we only provide it if elems.isInstanceOf[Leaf[U]]
//					prependByOne
					super.prependedAll(elems)
			} else
				super.prependedAll(elems)
		}

		override def appendedAll[U >: E](tree :BTreeSeq[U], depth :Int, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] = {
			assert(depth == 0,
				(dumpString(
					tree.dumpString(new StringBuilder ++= "attempted to append tree ") ++= " to ") ++=
					" at depth " ++= depth.toString
					).toString
			)
			appendedAll(tree, surplus)
		}
		override def prependedAll[U >: E](tree :BTreeSeq[U], depth :Int, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] = {
			assert(depth == 0,
				(dumpString(
					tree.dumpString(new StringBuilder ++= "attempted to prepend tree ") ++= " to ") ++=
					" at depth " ++= depth.toString
					).toString
			)
			prependedAll(tree, surplus)
		}

		override def appendedAll[U >: E](elems :BTreeSeq[U]) :BTreeSeq[U] = elems match {
			case leaf :Leaf[U] => appendedAll(leaf, null)
			case _             => elems prependedAll this
		}
		override def prependedAll[U >: E](elems :BTreeSeq[U]) :BTreeSeq[U] = elems match {
			case leaf :Leaf[U] => prependedAll(leaf, null)
			case _             => elems appendedAll this
		}

		override def appendedAll[U >: E](elems :IterableOnce[U]) :BTreeSeq[U] = elems match {
			case other :Leaf[U]     => appendedAll(other, null)
			case other :BTreeSeq[U] => other prependedAll this
			case _                  => appendedAll(elems, null)
		}

		override def prependedAll[U >: E](elems :IterableOnce[U]) :BTreeSeq[U] = elems match {
			case other :Leaf[U]     => prependedAll(other, null)
			case other :BTreeSeq[U] => other appendedAll this
			case _                  => prependedAll(elems, null)
		}


		override def dropChildren(n :Int) :BTreeSeq[E] = {
			val values = ErasedArray.ofDim[E](keys.length - n)
			arraycopy(keys, n, values, 0, keys.length - n)
			new Leaf(values)
		}
		override def takeChildren(n :Int) :BTreeSeq[E] = {
			val values = ErasedArray.ofDim[E](n)
			arraycopy(keys, 0, values, 0, n)
			new Leaf(values)
		}

		override def take(n :Int) :BTreeSeq[E] =
			if (n <= 0) Empty
			else if (n == 1) new Singleton(keys(0))
			else if (n >= keys.length) this
			else takeChildren(n)

		override def drop(n :Int) :BTreeSeq[E] =
			if (n <= 0) this
			else if (n >= keys.length) Empty
			else if (n == keys.length - 1) new Singleton(keys(keys.length - 1))
			else dropChildren(n)

		override def slice(from :Int, until :Int) = {
			val rank = keys.length
			if (until <= 0 | until <= from | from >= rank) Empty
			else if (from <= 0 & until >= rank) this
			else if (from + 1 == until) new Singleton(keys(from))
			else if (from < 0) takeChildren(until)
			else if (until > rank) dropChildren(from)
			else trustedSlice(from, until)
		}

		override def take[U >: E](n :Int, height :Int, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] =
			takeChildren(n)

		override def drop[U >: E](n :Int, height :Int, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] =
			dropChildren(n)

		private def trustedSlice(from :Int, until :Int) :BTreeSeq[E] = {
			val values = ErasedArray.ofDim[E](until - from)
			arraycopy(keys, from, values, 0, until - from)
			new Leaf(values)
		}
		override def slice[U >: E](from :Int, until :Int, height :Int, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] =
			trustedSlice(from, until)

		override def iterator :Iterator[E] = ArrayIterator(keys)

		override def addString(b :StringBuilder, start :String, sep :String, end :String) :b.type =
			keys.addString(b, start, sep, end)

		override def dumpString(sb :StringBuilder) :sb.type = keys.addString(sb, "(", ", ", ")")
	}



	private def semiCompleteNode[E](leaves :Array[BTreeSeq[E]], completeUntil :Int) :Node[E] = {
		val rank = leaves.length - 1
		if (completeUntil > rank)
			new Node(leaves, SemiCompletePrefixes(rank), (rank + 1) * MaxChildren)
		else {
			val prefixes = new Array[Int](rank)
			arraycopy(CompletePrefixes, 0, prefixes, 0, completeUntil)
			var i = completeUntil
			var len = completeUntil * MaxChildren
			while (i < rank) {
				len += leaves(i).length
				prefixes(i) = len
				i += 1
			}
			new Node(leaves, prefixes, len + leaves(rank).length)
		}
	}


	//Invariant: children.length >= 2; prefixes.length = children.length - 1; prefixes(i) == children.take(i).map(_.length).sum
	@SerialVersionUID(Ver)
	private class Node[+E](children :Array[BTreeSeq[E]], prefixes :Array[Int], override val length :Int)
		extends AbstractSeq[E] with BTreeSeq[E] with DefaultSerializable
	{
		def this(children :Array[BTreeSeq[E]], prefixes :Array[Int]) =
			this(children, prefixes, prefixes(prefixes.length - 1) + children(children.length - 1).length)

		def this(children :Array[BTreeSeq[E]]) = this(
			children, {
				var i = 0; val end = children.length - 1
				val prefixes = new Array[Int](end)
				var size = 0
				while (i < end) {
					size += children(i).length
					prefixes(i) = size
					i += 1
				}
				prefixes
			}
		)
		def this(first :BTreeSeq[E], second :BTreeSeq[E]) = this(
			{ val children = new Array[BTreeSeq[E]](2); children(0) = first; children(1) = second; children },
			Array.one(first.length), first.length + second.length
		)

		/** Create a new `Node` with `newChildren` as its children, copying prefix lengths from this instance
		  * for the children carried over from this node.
		  * @param newChildren Children of the created node; must satisfy
		  *                    `newChildren.take(unchanged).corresponds(this.children.take(unchanged))(_ eq _)`.
		  * @param unchanged   The number of leading children of this node included as a prefix in `newChildren`.
		  *                    Must be not greater than `min(this.children.length, newChildren.length)`.
		  */
		private def copy[U >: E](newChildren :Array[BTreeSeq[U]], unchanged :Int) :Node[U] = {
			val rank = newChildren.length
			if (unchanged == rank)
				if (rank == this.rank)
					this
				else {
					val newPrefixes = new Array[Int](rank - 1)
					arraycopy(prefixes, 0, newPrefixes, 0, rank - 1)
					new Node(newChildren, newPrefixes, prefixes(rank - 1))
				}
			else if (unchanged == rank - 1 && rank == this.rank)
				//a common case of appending a single element doesn't require a reallocating of the prefix lengths array
				new Node(newChildren, prefixes, prefixes(rank - 2) + newChildren(rank - 1).length)
			else {
				val newPrefixes = new Array[Int](rank - 1)
				var newLength =
					if (unchanged == 0)
						0
					else if (unchanged == children.length) {
						arraycopy(prefixes, 0, newPrefixes, 0, unchanged - 1)
						newPrefixes(unchanged - 1) = length
						length
					} else {
						arraycopy(prefixes, 0, newPrefixes, 0, unchanged)
						newPrefixes(unchanged - 1)
					}
				var i = unchanged
				while (i < rank - 1) {
					newLength += newChildren(i).length
					newPrefixes(i) = newLength
					i += 1
				}
				new Node(newChildren, newPrefixes, newLength + newChildren(rank - 1).length)
			}
		}

		final def subNodes      :IArray[BTreeSeq[E]] = children.asInstanceOf[IArray[BTreeSeq[E]]]
		@nowarn private final def prefixLengths :Array[Int] = prefixes //required by the debugger
		override def rank  :Int = children.length

		/** The length of the prefix of this sequence consisting of all its children
		  * up to and including `children(child)`. Hides the fact that `prefixes(rank - 1)` is undefined.
		  */
		@inline final def prefixLength(child :Int) =
			if (child == children.length - 1) length else prefixes(child)

		/** Locates the child which contains the value at `index`: the first child such that `index < prefixes(i)`
		  * (assuming for the purpose of the definition that `prefixes(rank - 1) == length`.
		  * If `index >= length`, then `this.rank` is returned, which isn't represented by a child in `children`.
		  */
		private def childNo(index :Int) :Int = {
			val rank = children.length
			//Check explicitly if the index falls in the last child; not only because it's a common case,
			// but because prefixes(rank-1) does not exist and we don't want to range check every access in bin search.
			if (length <= index)
				rank
			else if (prefixes(rank - 2) <= index)
				rank - 1
			else {
				var lo = 0; var hi = rank - 2
				val bump = index + 1 //we don't want to find prefixes(i) == index, but the first prefixes(i) > index
				while (lo < hi) {
					val m = (lo + hi) >> 1
					if (bump <= prefixes(m)) hi = m
					else lo = m + 1
				}
				lo
			}
		}

		override def apply(i :Int) = {
			val child = childNo(i)
			if (child == 0) children(0)(i)
			else children(child)(i - prefixes(child - 1))
		}

		/** Sw   aps the child `children(idx)` for `child` without any rebalancing. */
		private def updated[U >: E](idx :Int, child :BTreeSeq[U]) :Node[U] = {
			val newChildren = new Array[BTreeSeq[U]](children.length)
			arraycopy(children, 0, newChildren, 0, children.length)
			newChildren(idx) = child
			copy(newChildren, idx)
		}

		/** Swaps the child `children(idx)` for `child` and, if `second.nonEmpty`, inserts it at `children(idx + 1)`,
		  * pushing current children by one position farther. If the number of children exceeds `MaxChildren`,
		  * the node is split: the first half is returned, while the second is assigned to `second`.
		  * The returned node may have fewer children than `Rank` if `this.rank < Rank`
		  * (i.e., this is the root of the tree), but not more than `MaxChildren`.
		  */
		private def updated[U >: E](idx :Int, first :BTreeSeq[U], second :Box[BTreeSeq[U]]) :Node[U] = {
			val rank = children.length
			if (second == null || second.isEmpty)
				updated(idx, first)
			else if (rank < MaxChildren) {
				val newChildren = new Array[BTreeSeq[U]](rank + 1)
				arraycopy(children, 0, newChildren, 0, idx)
				newChildren(idx) = first
				newChildren(idx + 1) = second.remove()
				arraycopy(children, idx + 1, newChildren, idx + 2, rank - idx - 1)
				copy(newChildren, idx)
			} else {
				val init = new Array[BTreeSeq[U]](Rank)
				val tail = new Array[BTreeSeq[U]](Rank)
				if (idx >= Rank) {
					val i = idx - Rank
					arraycopy(children, 0, init, 0, Rank)
					arraycopy(children, Rank, tail, 0, i)
					tail(i) = first
					tail(i + 1) = second.get
					arraycopy(children, Rank + i + 1, tail, i + 2, Rank - i - 2)
					second := new Node(tail)
					copy(init, Rank)
				} else {
					arraycopy(children, 0, init, 0, idx)
					init(idx) = first
					if (idx == Rank - 1) {
						tail(0) = second.get
						arraycopy(children, Rank, tail, 1, idx)
					} else {
						init(idx + 1) = second.get
						arraycopy(children, idx + 1, init, idx + 2, Rank - idx - 2)
						arraycopy(children, Rank - 1, tail, 0, Rank)
					}
					second := new Node(tail)
					copy(init, idx)
				}
			}
		}

		override def updated[U >: E](index :Int, elem :U) = {
			val child = childNo(index)
			val newChild =
				if (child == 0) children(0).updated(index, elem)
				else children(child).updated(index - prefixes(child - 1), elem)
			val newChildren = new Array[BTreeSeq[U]](children.length)
			arraycopy(children, 0, newChildren, 0, children.length)
			newChildren(child) = newChild
			new Node(newChildren, prefixes, length)
		}

		override def inserted[U >: E](index :Int, elem :U, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] =
			if (index == length)
				appended(elem, surplus)
			else {
				val i = childNo(index)
				val relative = if (i == 0) index else index - prefixes(i - 1)
				val child = children(i).inserted(relative, elem, surplus)
				updated(i, child, surplus)
			}

		override def inserted[U >: E](index :Int, elem :U) :BTreeSeq[U] =
			if (index < 0 | index > length)
				outOfBounds_!(index.toString + " out of " + length)
			else {
				val growth = Box[BTreeSeq[U]]
				grow(inserted(index, elem, growth), growth)
			}

		override def removed(index :Int) :BTreeSeq[E] = {
			val rank = children.length
			val i = childNo(index)
			val child = children(i)
			val newChild = child.removed(if (i == 0) index else index - prefixes(i - 1))
			if (newChild.rank >= Rank)  //the tree is still balanced, no need to do anything other than update the child
				updated(i, newChild)
			else if (i == 0) { //we descended into the first child - balancing will use the following sibling
				val sibling = children(1)
				val siblingRank = sibling.rank
				if (siblingRank > Rank) { //move the first child/key from sibling to newChild
					val newChildren = new Array[BTreeSeq[E]](rank)
					newChildren(0) = newChild.appendedSome(sibling, 1)
					newChildren(1) = sibling.dropChildren(1)
					arraycopy(children, 2, newChildren, 2, rank - 2)
					new Node(newChildren)
				} else if (rank == 2) //reduce the height of the tree; possible only if we are the root node
					newChild.appendedSome(sibling, siblingRank)
				else {
					//Merge newChild with sibling. May reduce children below Rank,
					// but either we are the root and it's ok, or we will fix this when the call returns to our parent.
//					updated(0, newChild.appendedSome(sibling, siblingRank))
					val newChildren = new Array[BTreeSeq[E]](rank - 1)
					newChildren(0) = newChild.appendedSome(sibling, siblingRank)
					arraycopy(children, 2, newChildren, 1, rank - 2)
					new Node(newChildren)
				}
			} else { //balance newChild with the help of its preceding sibling
				val sibling = children(i - 1)
				val siblingRank = sibling.rank
				if (siblingRank > Rank) { //move the last child/key from sibling to newChild
					val newChildren = new Array[BTreeSeq[E]](rank)
					arraycopy(children, 0, newChildren, 0, rank)
					newChildren(i - 1) = sibling takeChildren siblingRank - 1
					newChildren(i)     = newChild.prependedSome(sibling, 1)
					copy(newChildren, i - 1)
				} else if (rank == 2) //reduce the height of the tree; possible only if we are the root node
					newChild.prependedSome(sibling, siblingRank)
				else {
					//Merge newChild with sibling. May reduce children below Rank,
					// but either we are the root and it's ok, or we will fix this when the call returns to our parent.
					val newChildren = new Array[BTreeSeq[E]](rank - 1)
					arraycopy(children, 0, newChildren, 0, i - 1)
					newChildren(i - 1) = sibling.appendedSome(newChild, newChild.rank)
					arraycopy(children, i + 1, newChildren, i, rank - i - 1)
					copy(newChildren, i - 1)
				}
			}
		}

		override def appended[U >: E](elem :U, surplus :Box[BTreeSeq[U]]) =
			updated(children.length - 1, children(children.length - 1).appended(elem, surplus), surplus)

		override def prepended[U >: E](elem :U, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] =
			updated(0, children(0).prepended(elem, surplus), surplus)

		override def appended[U >: E](elem :U) :BTreeSeq[U] = {
			val growth = Box[BTreeSeq[U]]
			grow(appended(elem, growth), growth)
		}
		override def prepended[U >: E](elem :U) :BTreeSeq[U] = {
			val growth = Box[BTreeSeq[U]]
			grow(prepended(elem, growth), growth)
		}


		override def appendedSome[U >: E](successor :BTreeSeq[U], siblingCount :Int) :Node[U] = {
			val node = successor.asInstanceOf[Node[U]]
			val siblings = node.subNodes
			val rank = children.length
			val newChildren = new Array[BTreeSeq[U]](rank + siblingCount)
			arraycopy(children, 0, newChildren, 0, rank)
			arraycopy(siblings, 0, newChildren, rank, siblingCount)
			copy(newChildren, rank)
		}
		override def prependedSome[U >: E](predecessor :BTreeSeq[U], siblingCount :Int) :Node[U] = {
			val node = predecessor.asInstanceOf[Node[U]]
			val siblings = node.subNodes
			val rank = children.length
			val newChildren = new Array[BTreeSeq[U]](rank + siblingCount)
			arraycopy(siblings, siblings.length - siblingCount, newChildren, 0, siblingCount)
			arraycopy(children, 0, newChildren, siblingCount, rank)
			new Node(newChildren)
		}

		override def appendedAll[U >: E](tree :BTreeSeq[U], depth :Int, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] =
			if (depth == 0) {
				val rank = children.length
				val treeRank = tree.rank
				val totalRank = rank + treeRank
				if (totalRank <= MaxChildren) //append all tree.children to this.children
					appendedSome(tree, treeRank)
				else if (rank >= Rank & treeRank >= Rank) { //nothing to do: both this and tree are balanced
					surplus := tree
					this
				} else if (rank >= Rank) { //carry over some of our children to treeRank to balance it
					val tail = new Array[BTreeSeq[U]](Rank)
					arraycopy(children, totalRank - Rank, tail, 0, Rank - treeRank)
					arraycopy(tree.asInstanceOf[Node[U]].subNodes, 0, tail, Rank - treeRank, treeRank)
					surplus := new Node(tail)
					takeChildren(totalRank - Rank)
				} else { //carry over some children of tree to this node to balance it
					val initSize = totalRank - Rank
					val init = new Array[BTreeSeq[U]](initSize)
					arraycopy(children, 0, init, 0, rank)
					arraycopy(tree.asInstanceOf[Node[U]].subNodes, 0, init, rank, initSize - rank)
					surplus := tree.dropChildren(initSize - rank)
					copy(init, rank)
				}
			} else {
				val rank = children.length
				val child = children(rank - 1).appendedAll(tree, depth - 1, surplus)
				updated(rank - 1, child, surplus)
			}

		override def prependedAll[U >: E](tree :BTreeSeq[U], depth :Int, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] =
			if (depth == 0) {
				val rank = children.length
				val treeRank = tree.rank
				val totalRank = rank + treeRank
				if (totalRank <= MaxChildren) //prepend all tree children to this.children
					prependedSome(tree, tree.rank)
				else if (rank >= Rank & treeRank >= Rank) { //nothing to do, because both this and tree are balanced
					surplus := this
					tree
				} else if (rank >= Rank) { //carry over some of our children to treeRank to balance it
					val node = tree.asInstanceOf[Node[U]]
					val init = new Array[BTreeSeq[U]](Rank)
					arraycopy(node.subNodes, 0, init, 0, treeRank)
					arraycopy(children, 0, init, treeRank, Rank - treeRank)
					surplus := dropChildren(Rank - treeRank)
					node.copy(init, treeRank)
				} else { //carry over some children of tree to this node to balance it
					val tail = new Array[BTreeSeq[U]](totalRank - Rank)
					arraycopy(tree.asInstanceOf[Node[U]].subNodes, Rank, tail, 0, treeRank - Rank)
					arraycopy(children, 0, tail, treeRank - Rank, rank)
					surplus := new Node(tail)
					tree.takeChildren(Rank)
				}
			} else {
				val child = children(0).prependedAll(tree, depth - 1, surplus)
				updated(0, child, surplus)
			}

		override def appendedAll[U >: E](elems :BTreeSeq[U]) :BTreeSeq[U] = elems.length match {
			case 0 => this
			case 1 => appended(elems.head)
			case _ =>
				val growth = Box[BTreeSeq[U]]
				val depthDiff = depth - elems.depth
				if (depthDiff >= 0)
					grow(appendedAll(elems, depthDiff, growth), growth)
				else
					grow(elems.prependedAll(this, -depthDiff, growth), growth)

		}
		override def prependedAll[U >: E](elems :BTreeSeq[U]) :BTreeSeq[U] = elems.length match {
			case 0 => this
			case 1 => prepended(elems.head)
			case _ =>
				val growth = Box[BTreeSeq[U]]
				val depthDiff = depth - elems.depth
				if (depthDiff >= 0)
					grow(prependedAll(elems, depthDiff, growth), growth)
				else
					grow(elems.appendedAll(this, -depthDiff, growth), growth)
		}


		override def dropChildren(n :Int) :BTreeSeq[E] = {
			val childCount = rank - n
			val newChildren = new Array[BTreeSeq[E]](childCount)
			arraycopy(children, n, newChildren, 0, childCount)
			new Node(newChildren)
		}

		override def takeChildren(n :Int) :BTreeSeq[E] = {
			val newChildren = new Array[BTreeSeq[E]](n)
			arraycopy(children, 0, newChildren, 0, n)
			copy(newChildren, n)
		}

		override def take(n :Int) :BTreeSeq[E] =
			if (n <= 0) Empty
			else if (n >= length) this
			else take[E](n, depth, Box.empty)

		override def drop(n :Int) :BTreeSeq[E] =
			if (n <= 0) this
			else if (n >= length) Empty
			else drop[E](n, depth, Box.empty)

		override def slice(from :Int, until :Int) :BTreeSeq[E] = {
			val len = length
			if (until <= from | until < 0 | from > len) Empty
			else if (from <= 0 && until >= len) this
			else if (from < 0) slice[E](0, until, depth, Box.empty)
			else if (until > len) slice[E](from, len, depth, Box.empty)
			else slice[E](from, until, depth, Box.empty)
		}

		override def take[U >: E](n :Int, height :Int, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] =
			if (n >= length)
				this
			else {
				val i = childNo(n) //i <= rank - 1, so prefixes(i - 1) is well defined
				val indexInChild = if (i == 0) n else n - prefixes(i - 1)
				if (indexInChild == 0)  //everything is dropped starting with child i; i > 0 because n > 0
					takeChildren(i)
				else {
					val child = children(i).take(indexInChild, height - 1, surplus)
					if (i == 0) //we dropped everything after the first child
						child
					else {
						val sibling  = children(i - 1)
						//consider: making depth a val; we are introducing complexity log^2(n) by repeatedly calling it here
						val newChild = sibling.appendedAll(child, height - 1 - child.depth, surplus)
						if (surplus.isEmpty)
							if (i == 1)
								newChild
							else {
								val newChildren = new Array[BTreeSeq[U]](i)
								arraycopy(children, 0, newChildren, 0, i - 1)
								newChildren(i - 1) = newChild
								copy(newChildren, i - 1)
							}
						else {
							val newChildren = new Array[BTreeSeq[U]](i + 1)
							arraycopy(children, 0, newChildren, 0, i - 1)
							newChildren(i - 1) = newChild
							newChildren(i) = surplus.remove()
							copy(newChildren, i - 1)
						}
					}
				}
			}

		override def drop[U >: E](n :Int, height :Int, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] = {
			val rank = children.length
			val i = childNo(n)
			val indexInChild = if (i == 0) n else n - prefixes(i - 1)
			if (n <= 0)
				this
			else if (indexInChild == 0)
				if (i == rank - 1)
					children(rank - 1)
				else
					dropChildren(i)  //everything up to child i not including is dropped
			else {
				val child = children(i).drop(indexInChild, height - 1, surplus)
				if (i == rank - 1)
					child
				else {
					val sibling = children(i + 1)
					//consider: making depth a val; we are introducing complexity log^2(n) by repeatedly calling it here
					val newChild = sibling.prependedAll(child, height - 1 - child.depth, surplus)
					if (surplus.isEmpty)
						if (i == rank - 2)
							newChild
						else {
							val newChildren = new Array[BTreeSeq[U]](rank - i - 1)
							newChildren(0) = newChild
							arraycopy(children, i + 2, newChildren, 1, rank - i - 2)
							new Node(newChildren)
						}
					else {
						val newChildren = new Array[BTreeSeq[U]](rank - i)
						newChildren(0) = newChild
						newChildren(1) = surplus.remove()
						arraycopy(children, i + 2, newChildren, 2, rank - i - 2)
						new Node(newChildren)
					}
				}
			}
		}

		override def slice[U >: E](from :Int, until :Int, height :Int, surplus :Box[BTreeSeq[U]]) :BTreeSeq[U] = {
			val fromIdx = childNo(from)
			val relativeFrom = if (fromIdx == 0) from else from - prefixes(fromIdx -  1)
			val fromChild = children(fromIdx)
			val prefix =
				if (relativeFrom == 0) fromChild
				else fromChild.slice(relativeFrom, fromChild.length, height - 1, surplus)
			val untilIdx = childNo(until)
			val relativeUntil =
				if (untilIdx == 0) until
				else if (untilIdx == rank) 0
				else until - prefixes(untilIdx - 1)
			if (fromIdx == untilIdx) //all subsequence belongs to a single child
				prefix.slice(0, relativeUntil - relativeFrom, height - 1, surplus)
			else if (relativeFrom == 0) { //we will start copying from newChildren(fromIdx)
				if (relativeUntil == 0) { //return a Node with a subsequence of this.children
					if (fromIdx + 1 == untilIdx)
						prefix
					else if (fromIdx == 0 && untilIdx == rank)
						this
					else {
						val newChildren = new Array[BTreeSeq[U]](untilIdx - fromIdx)
						arraycopy(children, fromIdx, newChildren, 0, untilIdx - fromIdx)
						if (fromIdx == 0)
							copy(newChildren, untilIdx)
						else
							new Node(newChildren)
					}
				} else { //we need to inject the, possibly lower, suffix tree into preceding sibling of children(untilIdx)
					val suffix      = children(untilIdx).slice(0, relativeUntil, height - 1, surplus)
					val last        = children(untilIdx - 1).appendedAll(suffix, height - 1 - suffix.depth, surplus)
					val copied      = untilIdx - fromIdx - 1
					if (copied + surplus.size == 0)
						last
					else {
						val newChildren = new Array[BTreeSeq[U]](copied + 1 + surplus.size)
						arraycopy(children, fromIdx, newChildren, 0, copied)
						newChildren(copied) = last
						if (surplus.nonEmpty)
							newChildren(copied + 1) = surplus.remove()
						if (fromIdx == 0)
							copy(newChildren, copied)
						else
							new Node(newChildren)
					}
				}
			} else if (relativeUntil == 0) {
				//we need to inject the, possibly lower, prefix tree into children(fromIdx + 1)
				val first       = children(fromIdx + 1).prependedAll(prefix, height - 1 - prefix.depth, surplus)
				val copied      = untilIdx - fromIdx - 2
				if (copied + surplus.size == 0)
					first
				else {
					val newChildren = new Array[BTreeSeq[U]](copied + 1 + surplus.size)
					newChildren(0)  = first
					arraycopy(children, fromIdx + 2, newChildren, 1 + surplus.size, copied)
					if (surplus.nonEmpty)
						newChildren(1) = surplus.remove()
					new Node(newChildren)
				}
			} else if (fromIdx + 1 == untilIdx) {
				//we need to merge prefix and suffix trees with each other, without any node in between
				val suffix      = children(untilIdx).slice(0, relativeUntil, height - 1, surplus)
				val prefixDepth = prefix.depth
				val suffixDepth = suffix.depth
				val first =
					if (prefixDepth >= suffixDepth)
						prefix.appendedAll(suffix, prefixDepth - suffixDepth, surplus)
					else
						suffix.prependedAll(prefix, suffixDepth - prefixDepth, surplus)
				surplus.maybeRemove() match {
					case Yes(second) => new Node(first, second)
					case _           => first
				}
			} else if (fromIdx + 2 == untilIdx) {
				//prefix and suffix trees must be merged to the same child at fromIdx + 1 (= untilIdx - 1)
				val suffix = children(untilIdx).slice(0, relativeUntil, height - 1, surplus)
				val first  = children(fromIdx + 1).prependedAll(prefix, height - 1 - prefix.depth, surplus)
				surplus.maybeRemove() match {
					case Yes(node) =>
						val second = node.appendedAll(suffix, height - 1 - suffix.depth, surplus)
						surplus.maybeRemove() match {
							case Yes(third) =>
								val newChildren = new Array[BTreeSeq[U]](3)
								newChildren(0) = first
								newChildren(1) = second
								newChildren(2) = third
								new Node(newChildren)
							case _ =>
								new Node(first, second)
						}
					case _ =>
						val res = first.appendedAll(suffix, height - 1 - suffix.depth, surplus)
						surplus.maybeRemove() match {
							case Yes(next) => new Node(res, next)
							case _ => res
						}
				}
			} else { //Finally, we can inject prefix and suffix trees independently into their following/preceding nodes.
				val first  = children(fromIdx + 1).prependedAll(prefix, prefix.depth, surplus)
				val second = surplus.maybeRemove()
				val suffix = children(untilIdx).take(relativeUntil, height - 1, surplus)
				val last   = children(untilIdx - 1).appendedAll(suffix, height - 1 - suffix.depth, surplus)
				val extra  = surplus.maybeRemove()
				val copied = untilIdx - fromIdx - 3
				val newChildren = new Array[BTreeSeq[U]](copied + 2 + second.size + extra.size)
				newChildren(0) = first
				if (second.isDefined)
					newChildren(1) = second.get
				arraycopy(children, fromIdx + 2, newChildren, 1 + second.size, copied)
				newChildren(copied + second.size) = last
				if (extra.isDefined)
					newChildren(copied + second.size + 1) = extra.get
				new Node(newChildren)
			}
		}


		override def iterator :Iterator[E] = new BTreeIterator(this)

		override def addString(b :StringBuilder, start :String, sep :String, end :String) :b.type = {
			b ++= start
			var i = 0; val rank = children.length
			while (i < rank - 1) {
				children(i).addString(b, "", sep, "")
				b ++= sep
				i += 1
			}
			children(rank - 1).addString(b, "", sep, "")
			b ++= end
		}

		override def dumpString(sb :StringBuilder) :sb.type = {
			sb += '('
			var i = 0; val last = children.length - 1
			while (i < last) {
				children(i).dumpString(sb) ++= ", "
				i += 1
			}
			children(last).dumpString(sb) += ')'
		}
	}




	private class BTreeSeqBuilder[E](
		private[this] var tree :BTreeSeq[E] = Empty,         //the built result
		private[this] var leaves :Array[BTreeSeq[E]] = null, //a second level buffer to which complete key arrays are written.
		private[this] var leafCount :Int = 0,                //the current number of buffered key arrays
		private[this] var keys   :Array[E] = null,           //a mutable buffer where appended keys are stored
		private[this] var keyCount :Int = 0,                 //the current number of keys in array keys
	)
		extends ReusableBuilder[E, BTreeSeq[E]]
	{ //growing tree
//		private[this] var hint = -1
//		private[this] var leaves    :Array[BTreeSeq[E]] = _ //a second level buffer to which complete keys arrays are written.
//		private[this] var leafCount :Int = 0                //the current number of buffered key arrays
//		private[this] var keys      :Array[E] = _           //a mutable buffer where appended keys are stored
//		private[this] var keyCount  :Int = 0                //the current number of keys in array keys
		private[this] var surplus :Box[BTreeSeq[E]] = _

		private def newLeaf   :Leaf[E] = new Leaf(
			if (keyCount == MaxChildren) keys else ErasedArray.copyOf(keys, keyCount)
		)
		private def newNode(complete :Int) :Node[E] =
			if (leafCount == MaxChildren && complete == MaxChildren)
				new Node(leaves, CompletePrefixes, MaxChildren * MaxChildren)
			else {
				val a = new Array[BTreeSeq[E]](leafCount)
				arraycopy(leaves, 0, a, 0, leafCount)
				semiCompleteNode(a, complete)
			}

		private def completedLeaf() :Unit = {
			if (leafCount == 0)
				leaves = new Array[BTreeSeq[E]](MaxChildren)
			assert(leafCount < leaves.length)
			leaves(leafCount) = new Leaf(keys)
			keyCount = 0
			leafCount += 1
			if (leafCount == MaxChildren) {
				addLevel2Node(new Node(leaves, CompletePrefixes, MaxChildren * MaxChildren))
				leafCount = 0
			}
		}

		private def flush() :Unit = {
			if (leafCount == 0) {
				if (keyCount > 0) tree match {
					case node :Node[E] =>
						if (surplus == null)
							surplus = Box[BTreeSeq[E]]
						grow(node.appendedAll(newLeaf, tree.depth - 1, surplus), surplus)
					case leaf :Leaf[E] =>
						tree = grow(leaf.appendedAll(newLeaf, null), null)
					case Empty =>
						tree = newLeaf
					case _ => //Singleton: shouldn't happen, but for peace of mind
						tree = newLeaf.prepended(tree.head)
				}
			} else if (keyCount >= Rank) {
				assert(leafCount < leaves.length)
				leaves(leafCount) = new Leaf(ErasedArray.copyOf(keys, keyCount))
				leafCount += 1
				addLevel2Node(newNode(leafCount - 1))
			} else {
				assert(leafCount < leaves.length)
				val secondLastRank = MaxChildren + keyCount - Rank
				val secondLast     = leaves(leafCount - 1).asInstanceOf[Leaf[E]]
				val last           = concatSlices(
					secondLast.values.asInstanceOf[Array[E]], secondLastRank, Rank - keyCount,
					keys, 0, keyCount
				)
				leaves(leafCount - 1) = secondLast.takeChildren(secondLastRank)
				leaves(leafCount)     = new Leaf(last)
				leafCount += 1
				addLevel2Node(newNode(leafCount - 2))
			}
			keyCount = 0
			leafCount = 0
			leaves = null
			keys   = null
		}

		override def clear() :Unit = {
			keys = null
			keyCount = 0
			tree = Empty
		}

		override def result() = {
			flush()
			val res = tree
			tree = Empty
			res
		}


		private def addLevel2Node(node :Node[E]) :Unit = tree match {
			case _ :Node[E] =>
				if (surplus == null)
					surplus = Box[BTreeSeq[E]]
				tree = grow(tree.appendedAll(node, tree.depth - 2, surplus), surplus)
			case _ :Leaf[E] =>
				if (surplus == null)
					surplus = Box[BTreeSeq[E]]
				tree = grow(node.prependedAll(tree, 1, surplus), surplus)
			case Empty      =>
				tree = node
			case _          => //Singleton
				if (surplus == null)
					surplus = Box[BTreeSeq[E]]
				tree = node.prepended(tree.head, surplus)
		}

		private def addSlice(elems :Array[E], from :Int, size :Int, immutable :Boolean) :this.type = {
			if (keyCount + size <= MaxChildren) {
				if (immutable & keyCount == 0 & from == 0 & size == elems.length) {
					if (leafCount == 0)
						leaves = new Array[BTreeSeq[E]](MaxChildren)
					leaves(leafCount) = new Leaf(elems)
				} else {
					if (keyCount == 0)
						keys = ErasedArray.ofDim(MaxChildren)
					arraycopy(elems, from, keys, 0, size)
					keyCount += size
					if (keyCount == MaxChildren)
						completedLeaf()
				}
			} else if (keyCount + size <= (MaxChildren << 1)) { //the 'Leaf' case
				val firstPart = MaxChildren - keyCount
				arraycopy(elems, 0, keys, keyCount, firstPart)
				completedLeaf()
				keys = ErasedArray.ofDim[E](MaxChildren)
				keyCount = size - firstPart
				arraycopy(elems, firstPart, keys, 0, keyCount)
			} else if (size <= BuilderAddByOneThreshold) {
				val until = from + size
				var i = from
				while (i < until) {
					addOne(elems(i))
					i += 1
				}
			} else {
				flush() //todo: move appendedSlice implementation to the builder
				tree = tree.appendedSlice(elems, from, from + size)
			}
			this
		}

		override def addOne(elem :E) = {
			if (keyCount == 0) {
				keys = ErasedArray.ofDim[E](MaxChildren)
				keyCount = 1
				keys(0) = elem
			} else {
				keys(keyCount) = elem
				keyCount += 1
				if (keyCount == MaxChildren)
					completedLeaf()
			}
			this
		}

		@tailrec final override def addAll(elems :IterableOnce[E]) :this.type = {
			elems.knownSize match {
				case 0 => this
				case 1 => elems match {
					case items :Iterable[E] => addOne(items.head)
					case _                  => addOne(elems.iterator.next())
				}
				case size => elems match {
					case leaf  :Leaf[E] =>
						if (keyCount == 0) {
							if (leafCount == 0)
								leaves = new Array[BTreeSeq[E]](MaxChildren)
							leaves(leafCount) = leaf
							leafCount += 1
							if (leafCount == MaxChildren) {
								addLevel2Node(new Node(leaves, CompletePrefixes, MaxChildren * MaxChildren))
								leafCount = 0
							}
							this
						} else
							addSlice(leaf.values.asInstanceOf[Array[E]], 0, leaf.values.length, true)
					case other :BTreeSeq[E] =>
						flush()
						val depthDiff = tree.depth - other.depth
						if (depthDiff >= 0)
							tree = tree.appendedAll(other, depthDiff, surplus)
						else
							tree = other.prependedAll(tree, -depthDiff, surplus)
						this
					case seq :collection.IndexedSeqOps[E, _, _] =>
						var array :Array[E] = null
						var immutable = false
						var from = 0
						seq match {
//							case leaf :Leaf[E] =>
//								array = leaf.values.asInstanceOf[Array[E]]
//								immutable = true
							case arr :ArraySeq[E] if arr.unsafeArray.isInstanceOf[Array[AnyRef]] =>
								array = arr.unsafeArray.asInstanceOf[Array[E]]
								immutable = true
							case arr :ArrayIterableOnce[E] if arr.unsafeArray.isInstanceOf[Array[AnyRef]] =>
								array = arr.unsafeArray.asInstanceOf[Array[E]]
								from = arr.startIndex
								immutable = arr.isImmutable
							case arr :mutable.ArraySeq[E] if arr.array.isInstanceOf[Array[AnyRef]] =>
								array = arr.array.asInstanceOf[Array[E]]
							case _ =>
						}
						if (array == null) {
							if (keyCount + size <= MaxChildren) {
								if (keyCount == 0)
									keys = ErasedArray.ofDim(MaxChildren)
								seq.copyToArray(keys, keyCount, size)
								this
							} else if (size <= IndexedSeqDefaults.defaultApplyPreferredMaxLength) {
								var i = 0
								while (i < size) {
									addOne(seq(i))
									i += 1
								}
								this
							} else
								addAll(seq.iterator)
						} else
							addSlice(array, from, size, immutable)
					case _ if size > 0 & keyCount + size <= MaxChildren =>
						if (keyCount == 0)
							keys = ErasedArray.ofDim(MaxChildren)
						copy(elems, keys, keyCount, size)
						keyCount += size
						if (keyCount == MaxChildren)
							completedLeaf()
						this
					case _ =>
						super.addAll(elems)
				}
			}
		}
	}


	private class BTreeIterator[+E](stack :Array[IArray[BTreeSeq[E]]], childIndex :Array[Int])
		extends BufferedIterator[E]
	{
		private def this(stack :Array[IArray[BTreeSeq[E]]]) = this(stack, new Array[Int](stack.length))

		def this(tree :Node[E]) = this(
			{ val a = new Array[IArray[BTreeSeq[E]]](tree.depth - 1); a(a.length - 1) = tree.subNodes; a }
		)

		private[this] var keyIdx :Int = 0
		private[this] var leaf   :IArray[E] = {
			var i = stack.length - 1
			var siblings = stack(i)
			while (i > 0) {
				siblings = siblings(0).asInstanceOf[Node[E]].subNodes
				i -= 1
				stack(i) = siblings
				childIndex(i) = 0
			}
			siblings(0).asInstanceOf[Leaf[E]].values
		}
		override def hasNext = keyIdx >= 0
		override def head :E = leaf(keyIdx)

		override def next() :E = {
			val res = try leaf(keyIdx) catch {
				case e :IndexOutOfBoundsException => noSuch_!(e)
			}
			keyIdx += 1
			if (keyIdx == leaf.length) {
				val height = stack.length
				var level = 0
				var ancestors = stack(0)
				var childIdx = childIndex(0) + 1
				while (childIdx == ancestors.length && { level += 1; level < height }) {
					ancestors = stack(level)
					childIdx = childIndex(level) + 1
				}
				if (level == height)
					keyIdx = -1
				else {
					childIndex(level) = childIdx
					while (level > 0) {
						ancestors = ancestors(childIdx).asInstanceOf[Node[E]].subNodes
						level -= 1
						childIdx = 0
						stack(level) = ancestors
						childIndex(level) = 0
					}
					leaf = ancestors(childIdx).asInstanceOf[Leaf[E]].values
					keyIdx = 0
				}
			}
			res
		}

		override def equals(that :Any) :Boolean = that match {
			case other :BTreeIterator[_] =>
				leafIdx == other.leafIdx && childIndex.sameElements(other.indexes) &&
					stack.corresponds(other.ancestors)(_ sameElements _)
		}
		private def leafIdx = keyIdx
		private def indexes = childIndex
		private def ancestors :Array[_ <: IArray[BTreeSeq[E]]] = stack
		override def toString :String =
			if (keyIdx >= 0) "BTreeIterator(->" + leaf(keyIdx) + ")"
			else "BTreeIterator(-)"
	}


	private def copy[E](elems :IterableOnce[E], xs :Array[E], start :Int, len :Int) :Unit = elems match {
		case leaf  :Leaf[E]     => arraycopy(leaf.values, 0, xs, start, len)
		case items :Iterable[E] => items.copyToArray(xs, start, len)
		case _                  => elems.iterator.copyToArray(xs, start, len)
	}

	private def concatSlices[E](array1 :Array[E], start1 :Int, len1 :Int, array2 :Array[E], start2 :Int, len2 :Int) :Array[E] = {
		val res = ErasedArray.ofDim[E](len1 + len2)
		arraycopy(array1, start1, res, 0, len1)
		arraycopy(array2, start2, res, len1, len2)
		res
	}
//	private def concatSlices[E](array1 :Array[BTreeSeq[E]], start1 :Int, len1 :Int,
//	                            array2 :Array[BTreeSeq[E]], start2 :Int, len2 :Int) :Array[BTreeSeq[E]] =
//	{
//		val res = new Array[BTreeSeq[E]](len1 + len2)
//		arraycopy(array1, start1, res, 0, len1)
//		arraycopy(array2, start2, res, len1, len2)
//		res
//	}

//	private final val Rank = 4 //good for testing
	private final val Rank = 32
	private final val MaxChildren = (Rank << 1) - 1
	private final val ConvertToBTreeOnConcatFactor = Rank //or MaxChildren?
	private final val BuilderAddByOneThreshold = MaxChildren << 1

	private final val SemiCompletePrefixes = Array.tabulate(MaxChildren)(rank =>
		Array.iterate(MaxChildren, rank)(_ + MaxChildren)
	)
	private final val CompletePrefixes = SemiCompletePrefixes(MaxChildren - 1)

}
