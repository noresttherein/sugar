package net.noresttherein.sugar.collections

import java.lang.System.arraycopy

import scala.Array.{emptyIntArray, emptyObjectArray}
import scala.annotation.{nowarn, tailrec}
import scala.collection.generic.DefaultSerializable
import scala.collection.{IterableFactoryDefaults, SeqFactory, StrictOptimizedSeqFactory}
import scala.collection.immutable.{AbstractSeq, IndexedSeqOps}
import scala.collection.mutable.Builder

import net.noresttherein.sugar.collections.Jack.ReversedJack

//implicits
import net.noresttherein.sugar.collections.extensions._
import net.noresttherein.sugar.typist.casting.extensions.castTypeConstructorMethods




/** A sequence backed by a B-Tree based finger tree. It is similar to [[collection.immutable.Vector Vector]], but offers
  * additional methods for inserting and removing elements from the interior of the sequence.
  * Unlike in vector, slicing, patching and concatenation with another `Jack` take O(log n) time.
  * All other operations have the same complexity as in vector - that is, O(1) for access and modification
  * close to either end of the sequence, and O(log n) for accessing elements in the middle. The only operations
  * which take O(n) time are those which apply a function to all elements of the sequence, such as `map` and `filter`,
  * and conversions to other collections.
  *
  * It is a typical, all purpose, 'jack o all trades': it does not have any serious weak points and is a good choice
  * if the creator doesn't know how the sequence will be used. However, being a safe option, which won't have
  * a serious impact on performance of any algorithm, comes at the cost of not being the best in the most common
  * usages: random access to elements near the middle of the sequence, in particular update,
  * is somewhat slower than in `Vector`, which makes the latter a better choice if the user is not interested
  * in bulk operations.
  * @author Marcin Mościcki
  */
sealed abstract class Jack[+E]
	extends AbstractSeq[E] with IndexedSeq[E] with IndexedSeqOps[E, Jack, Jack[E]]
	   with IterableFactoryDefaults[E, Jack] with DefaultSerializable
{
	def removed(index :Int) :Jack[E]

	def inserted[U >: E](index :Int, elem :U) :Jack[U]


	override def reverse :Jack[E] = new ReversedJack(this)
	override def iterableFactory :SeqFactory[Jack] = Jack

	protected override def className :String = "Fingers"
}



@SerialVersionUID(Ver)
object Jack extends StrictOptimizedSeqFactory[Jack] {
	override def from[A](source :IterableOnce[A]) :Jack[A] = ???

	override def empty[A] :Jack[A] = ???

	override def newBuilder[A] :Builder[A, Jack[A]] = ???


	/** An empty finger tree. */
	@SerialVersionUID(Ver)
	private object Jack0 extends Jack[Nothing] {
		override def length :Int = 0
		override def apply(i :Int) :Nothing = throw new IndexOutOfBoundsException(i.toString + " out of 0")

		override def removed(index :Int) :Jack[Nothing] =
			throw new IndexOutOfBoundsException(index.toString + " out of 0")

		override def inserted[U >: Nothing](index :Int, elem :U) :Jack[U] =
			if (index != 0)
				throw new IndexOutOfBoundsException(index.toString + " out of 0")
			else
				new Jack1(IRefArray.one(elem))

		override def reverse :Jack[Nothing] = this
	}


	/** A finger tree of level 1 - a non empty flat list of up to `MaxChildren` elements. */
	@SerialVersionUID(Ver)
	private class Jack1[+E](prefix :IRefArray[E]) extends Jack[E] {
		//todo: make it behave like a PassedArray
		override def length :Int = prefix.length
		override def apply(i :Int) :E = prefix(i)

		override def removed(index :Int) :Jack[E] =
			if (prefix.length == 1)
				if (index != 0)
					throw new IndexOutOfBoundsException(index.toString + " out of 1")
				else
					Jack0
			else
				new Jack1(prefix.removed(index))

		override def inserted[U >: E](index :Int, elem :U) :Jack[U] = {
			val length = prefix.length
			if (index < 0 | index > length)
				throw new IndexOutOfBoundsException(index.toString + " out of " + length)
			if (length < MaxChildren) //copy the array with the new element included
				new Jack1(prefix.inserted(index, elem))
			else { //split the array into a prefix and suffix and grow to a Fingers2
				val newPrefix = ErasedArray.ofDim[U](Rank)
				val newSuffix = ErasedArray.ofDim[U](Rank) //we could remove this by refactoring insertAndSplit from Fingers2.inserted
				if (index < Rank) { //insert into the prefix
					arraycopy(prefix, 0, newPrefix, 0, index)
					newPrefix(index) = elem
					arraycopy(prefix, index, newPrefix, index + 1, Rank - index - 1)
					arraycopy(prefix, Rank - 1, newSuffix, 0, Rank)
				} else { //insert into the suffix
					arraycopy(prefix, 0, newPrefix, 0, Rank)
					arraycopy(prefix, Rank, newSuffix, 0, index - Rank)
					newSuffix(index - Rank) = elem
					arraycopy(prefix, index, newSuffix, index + 1, Rank - index - 1)
				}
				new Jack2(
					newPrefix.asInstanceOf[IRefArray[U]], emptyLevel2Tree[U], Rank, newSuffix.asInstanceOf[IRefArray[U]]
				)
			}
		}

		override def appended[U >: E](elem :U) :Jack[U] = {
			val length = prefix.length
			if (length < MaxChildren) //copy the array with the new element included
				new Jack1(prefix appended elem)
			else { //split into a prefix and suffix, growing to Fingers2
				val newSuffix = IRefArray.copyOfRange(prefix, Rank, Rank << 1)
				newSuffix.asInstanceOf[Array[U]](Rank - 1) = elem
				new Jack2(prefix.take(Rank), emptyLevel2Tree[U], Rank, newSuffix)
			}
		}

		override def reverse :Jack[E] = new Jack1(prefix.reverse)
		override def iterator :Iterator[E] = prefix.iterator
	}


	/** A finger tree of level 2.
	  * @param prefix             the first `Rank <= n <= MaxChildren` elements in the sequence.
	  * @param infix              a sequence of `0 <= n <= MaxChildren - 2` slices, each of length `Rank <= n <= MaxChildren`.
	  * @param lengthBeforeSuffix `prefix.length + infix.map(_.length).sum`.
	  * @param suffix             the last `Rank <= n <= MaxChildren` elements in the sequence
	  */
	@SerialVersionUID(Ver)
	private class Jack2[+E](prefix :IRefArray[E], infix :IArray[IRefArray[E]], lengthBeforeSuffix :Int,
	                        suffix :IRefArray[E])
		extends Jack[E]
	{
		def this(prefix :IRefArray[E], infix :IArray[IRefArray[E]], suffix :IRefArray[E]) =
			this(prefix, infix, prefix.length + totalLengthOf(infix), suffix)

		override def length = lengthBeforeSuffix + suffix.length

//		private def infixSlice(i :Int) :Int =
//			if (i >= infix(infix.length - 2))
//				infix.length - 2
//			else {
//				var lo = 0; var hi = infix.length - 2
//				val bump = i + 1
//				while (lo < hi) {
//					val m = (lo + hi) >> 1
//					if (bump <= infixLengths(m)) hi = m
//					else lo = m + 1
//				}
//				lo
//			}
		private def infixFinger(index :Int) :FingerIndex = {
			var i = 0
			var last = 0
			var next = 0
			while (next <= index) {
				last = next
				next += infix(i).length
				i += 1
			}
			FingerIndex(i - 1, last)
		}

		override def apply(i :Int) :E =
			if (i < prefix.length)
				prefix(i)
			else if (i >= lengthBeforeSuffix)
				suffix(i - lengthBeforeSuffix)
			else {
				val index = infixFinger(i - prefix.length)
				infix(index.finger)(index.relative)
//				val childIdx = infixSlice(i)
//				val relativeIdx = if (childIdx == 0) prefix.length else infixLengths(childIdx - 1)
//				infix(childIdx)(relativeIdx)
			}

		private def updatedInfix[U >: E](finger :Int, elems :IRefArray[U]) :Jack[U] = {
			val prefixLength = lengthBeforeSuffix - infix(finger).length + elems.length
			new Jack2(prefix, infix.updated(finger, elems), prefixLength, suffix)
		}

		override def updated[U >: E](index :Int, elem :U) :Jack[U] =
			if (index < prefix.length)
				new Jack2(prefix.updated(index, elem), infix, lengthBeforeSuffix, suffix)
			else if (index >= lengthBeforeSuffix)
				new Jack2(prefix, infix, lengthBeforeSuffix, suffix.updated(index - lengthBeforeSuffix, elem))
			else {
				val finger = infixFinger(index - prefix.length)
				updatedInfix(finger.finger, infix(finger.finger).updated(finger.relative, elem))
			}


		def removed(index :Int) :Jack[E] = {
			/** Removes the element at `index` from `suffix` of length `Rank`, and prepends the given element. */
			def removeAndPrepend(suffix :IRefArray[E], index :Int, predecessor :E) :IRefArray[E] = {
				val res = ErasedArray.ofDim[E](Rank)
				arraycopy(suffix, 0, res, 1, index)
				arraycopy(suffix, index + 1, res, index + 1, Rank - index - 1)
				res(0) = predecessor
				res.asInstanceOf[IRefArray[E]]
			}
			/** Removes the element at `index` from `suffix` of length `Rank`, and prepends a whole array of length `Rank`. */
			def removeAndPrependAll(suffix :IRefArray[E], index :Int, sibling :IRefArray[E]) :IRefArray[E] = {
				val res = ErasedArray.ofDim[E](MaxChildren)
				arraycopy(sibling, 0, res, 0, Rank)
				arraycopy(suffix, 0, res, Rank, index)
				arraycopy(suffix, index + 1, res, Rank + index, Rank - index - 1)
				res.asInstanceOf[IRefArray[E]]
			}
			/** Removes the element at `index` from `prefix` of length `Rank`,
			  * appending the given element to pad it to `Rank`. */
			def removeAndAppend(prefix :IRefArray[E], index :Int, successor :E) :IRefArray[E] = {
				val res = ErasedArray.ofDim[E](Rank)
				arraycopy(prefix, 0, res, 0, index)
				arraycopy(prefix, index + 1, res, index, Rank - index - 1)
				res(Rank - 1) = successor
				res.asInstanceOf[IRefArray[E]]
			}
			/** Removes the element at `index` from `prefix` of length `Rank`, and appends a whole array of length `Rank`. */
			def removeAndAppendAll(prefix :IRefArray[E], index :Int, sibling :IRefArray[E]) :IRefArray[E] = {
				val res = ErasedArray.ofDim[E](MaxChildren)
				arraycopy(prefix, 0, res, 0, index)
				arraycopy(prefix, index + 1, res, index, Rank - index - 1)
				arraycopy(sibling, 0, res, Rank - 1, Rank)
				res.asInstanceOf[IRefArray[E]]
			}
			if (index < prefix.length) { //The removed element lies in the prefix
				if (prefix.length > Rank) //The easy case: simply remove the element from prefix.
					new Jack2(prefix.removed(index), infix, lengthBeforeSuffix - 1, suffix)
				else if (infix.length > 0) {
					val sibling = infix(0)
					if (sibling.length > Rank) { //Carry over the first element in the infix tree to prefix
						val newSibling = sibling.tail
						val newInfix   = infix.updated(0, newSibling)
						val newPrefix  = removeAndAppend(prefix, index, sibling.head)
						new Jack2(newPrefix, newInfix, lengthBeforeSuffix - 1, suffix)
					} else if (infix.length == 1) {//Append the whole single element of infix to prefix, clearing infix
						val newPrefix = removeAndAppendAll(prefix, index, sibling)
						new Jack2(newPrefix, emptyLevel2Tree[E], MaxChildren, suffix)
					} else {
						val newPrefix = removeAndAppendAll(prefix, index, sibling)
						new Jack2(newPrefix, infix.tail, lengthBeforeSuffix - 1, suffix)
					}
				} else if (suffix.length > Rank) {//infix.length == 0 -> carry over the first element from suffix
					val newPrefix = removeAndAppend(prefix, index, suffix.head)
					new Jack2(newPrefix, infix, lengthBeforeSuffix - 1, suffix.tail)
				} else //suffix.length == Rank -> reduce the level by merging prefix and suffix
					new Jack1(removeAndAppendAll(prefix, index, suffix))

			} else if (index >= lengthBeforeSuffix) { //The removed element lies in the suffix
				val indexInSuffix = lengthBeforeSuffix - index
				if (suffix.length > Rank) //The simplest case - simply remove an element from the suffix array.
					new Jack2(prefix, infix, lengthBeforeSuffix, suffix.removed(indexInSuffix))
				else if (infix.length > 0) {
					val sibling = infix(infix.length - 1)
					if (sibling.length > Rank) { //Remove the element from suffix, carry over the last element of infix.
						val newSibling = sibling.init
						val newInfix   = infix.updated(infix.length - 1, newSibling)
						val newSuffix  = removeAndPrepend(suffix, indexInSuffix, sibling.head)
						new Jack2(prefix, newInfix, lengthBeforeSuffix, newSuffix)
					} else if (infix.length == 1) {//Remove the element from suffix, merge infix and suffix.
						val newSuffix = removeAndPrependAll(suffix, indexInSuffix, sibling)
						new Jack2(prefix, emptyLevel2Tree[E], prefix.length, newSuffix)
					} else { //Remove the element from suffix, and merge it with the last slice in infix.
						val newSuffix = removeAndPrependAll(suffix, indexInSuffix, sibling)
						new Jack2(prefix, infix.init, lengthBeforeSuffix - Rank, newSuffix)
					}
				} else if (prefix.length > Rank) { //Remove the element from suffix, carry over last prefix element.
					val newSuffix = removeAndPrepend(suffix, indexInSuffix, prefix.last)
					new Jack2(prefix.init, infix, lengthBeforeSuffix - 1, newSuffix)
				} else //Remove the element from suffix, merge it with prefix, reducing the tree level.
					new Jack1(removeAndPrependAll(suffix, indexInSuffix, prefix))

			} else { //The removed element lies somewhere in infix.
				val finger   = infixFinger(index - prefix.length)
				val idx      = finger.finger
				val relative = finger.relative
				val slice    = infix(idx)
				if (slice.length > Rank) //Easy - remove the element from the slice and update infix with the shorter array.
					updatedInfix(idx, slice.removed(relative))
				else if (infix.length == 1) { //slice.length == Rank
					if (prefix.length > Rank) { //Remove the element from the slice, carry over the last prefix element.
						val newSlice  = removeAndPrepend(slice, relative, prefix(prefix.length - 1))
						new Jack2(prefix.init, infix.updated(idx, newSlice), lengthBeforeSuffix - 1, suffix)
					} else if (suffix.length > Rank) { //Remove the element, carry over the first element from suffix.
						val newSlice  = removeAndAppend(slice, relative, suffix(0))
						new Jack2(prefix, infix.updated(idx, newSlice), lengthBeforeSuffix - 1, suffix.tail)
					} else { //Remove the element and append the slice to prefix, clearing infix completely.
						val newPrefix = IRefArray.copyOfRanges(
							prefix, 0, Rank, slice, 0, relative, slice, relative + 1, slice.length
						)
						new Jack2(newPrefix, emptyLevel2Tree[E], lengthBeforeSuffix - 1, suffix)
					}
				} else if (idx == 0) { //slice.length == Rank
					val sibling = infix(1)
					if (sibling.length > Rank) { //Remove the element, carry over the first element of the next slice.
						val newSlice   = removeAndAppend(slice, relative, sibling(0))
						val newInfix   = new Array[IRefArray[E]](infix.length)
						newInfix(0)    = newSlice
						newInfix(1)    = sibling.tail
						arraycopy(infix, 2, newInfix, 2, infix.length - 2)
						new Jack2(prefix, newInfix.asInstanceOf[IArray[IRefArray[E]]], lengthBeforeSuffix - 1, suffix)
					} else { //Remove the element, merge with the next slice.
						val newSlice = removeAndAppendAll(slice, relative, sibling)
						val newInfix = infix.tail
						newInfix.asInstanceOf[Array[IRefArray[E]]](0)  = newSlice
						new Jack2(prefix, newInfix, lengthBeforeSuffix - 1, suffix)
					}
				} else { //slice.length == Rank && idx > 0
					val sibling = infix(idx - 1) //we could mirror the previous case and for inner indices pick left or right sibling
					if (sibling.length > Rank) { //Remove the element, carry over the last element of the previous slice.
						val newSlice      = removeAndPrepend(slice, relative, sibling(sibling.length - 1))
						val newInfix      = new Array[IRefArray[E]](infix.length)
						arraycopy(infix, 0, newInfix, 0, infix.length)
						newInfix(idx)     = newSlice
						newInfix(idx - 1) = sibling.init
						new Jack2(prefix, newInfix.asInstanceOf[IArray[IRefArray[E]]], lengthBeforeSuffix - 1, suffix)
					} else { //Remove the element, merge slice with the preceding slice.
						val newSlice = removeAndPrependAll(slice, relative, sibling)
						val newInfix = new Array[IRefArray[E]](infix.length - 1)
						arraycopy(infix, 0, newInfix, 0, idx - 1)
						arraycopy(infix, idx + 1, newInfix, idx, infix.length - idx - 1)
						newInfix(idx - 1) = newSlice
						new Jack2(prefix, newInfix.asInstanceOf[IArray[IRefArray[E]]], lengthBeforeSuffix -  1, suffix)
					}
				}
			}
		}

		override def inserted[U >: E](index :Int, elem :U) :Jack[U] = {
			/** Copies `prefix ++ suffix` into `newPrefix` and `newSuffix`, and inserts `elem` at `index`.
			  * Target arrays are treated for this purpose as a single, continuous virtual array.
			  * Used to make room in `prefix`, which is assumed to be of rank `MaxChildren`, for the new element.
			  * Requires:
			  * {{{
			  *     index >= 0 < MaxChildren && prefix.length == MaxChildren && suffix.length < newSuffix.length &&
			  *         prefix.length + suffix.length + 1 == newPrefix.length + newSuffix.length
			  *  }}}
			  */
			def shiftRightAndInsert(prefix :IRefArray[E], suffix :IRefArray[E], index :Int,
			                        newPrefix :IRefArray[U], newSuffix :IRefArray[U]) :Unit =
			{
				val prefixSize = newPrefix.length
				if (index < prefixSize) { //insert the element into the new prefix
					arraycopy(prefix, 0, newPrefix, 0, index)
					newPrefix.asInstanceOf[Array[U]](index) = elem
					arraycopy(prefix, index, newPrefix, index + 1, prefixSize - index - 1)
					arraycopy(prefix, prefixSize - 1, newSuffix, 0, MaxChildren - prefixSize + 1)
				} else {                  //insert the element into the suffix
					arraycopy(prefix, 0, newPrefix, 0, prefixSize)
					arraycopy(prefix, prefixSize, newSuffix, 0, index - prefixSize)
					newSuffix.asInstanceOf[Array[U]](index - prefixSize) = elem
					arraycopy(prefix, index, newSuffix, index - prefixSize + 1, MaxChildren - index)
				}
				arraycopy(suffix, 0, newSuffix, MaxChildren - prefixSize + 1, suffix.length)
			}
			/** Copies `prefix ++ suffix` into `newPrefix` and `newSuffix`, and inserts `elem` at `index`.
			  * Target arrays are treated for this purpose as a single, continuous virtual array.
			  * Used to make room in `suffix`, when it is of rank `MaxChildren`, for the new element. Requires:
			  * {{{
			  *     index >= prefix.length < prefix.length + MaxChildren &&
			  *         prefix.length < newPrefix.length && suffix.length == MaxChildren &&
			  *             prefix.length + suffix.length + 1 == newPrefix.length + newSuffix.length
			  * }}}
			  */
			def shiftLeftAndInsert(prefix :IRefArray[E], suffix :IRefArray[E], index :Int,
			                       newPrefix :IRefArray[U], newSuffix :IRefArray[U]) :Unit =
			{
				val oldPrefixSize = prefix.length
				val newPrefixSize = newPrefix.length
				val shift = newPrefixSize - oldPrefixSize
				if (index < newPrefixSize) { //insert the element into the new prefix
					arraycopy(prefix, 0, newPrefix, 0, oldPrefixSize)
					arraycopy(suffix, 0, newPrefix, oldPrefixSize, index - oldPrefixSize)
					newPrefix.asInstanceOf[Array[U]](index) = elem
					arraycopy(suffix, index - oldPrefixSize, newPrefix, index + 1, newPrefixSize - index - 1)
					arraycopy(suffix, shift - 1, newSuffix, 0, MaxChildren - shift + 1)
				} else {
					val indexInSuffix = index - newPrefixSize
					val oldIndex = index - oldPrefixSize
					arraycopy(prefix, 0, newPrefix, 0, oldPrefixSize)
					arraycopy(suffix, 0, newPrefix, oldPrefixSize, shift)
					arraycopy(suffix, shift, newSuffix, 0, indexInSuffix)
					newSuffix.asInstanceOf[Array[U]](indexInSuffix) = elem
					arraycopy(suffix, oldIndex, newSuffix, indexInSuffix + 1, MaxChildren - oldIndex)
				}
			}
			/** Splits the `slice` of rank `MaxChildren` into two parts of rank `Rank`, inserting `elem` at `index`
			  * (within the original `slice`). Requires:
			  * {{{
			  *     index >= 0 < MaxChildren && slice.length == MaxChildren &&
			  *     first.length == Rank && second.length == Rank
			  * }}}
			  */
			def insertAndSplit(slice :IRefArray[E], first :IRefArray[U], second :IRefArray[U], index :Int) :Unit = {
				if (index < Rank) {
					arraycopy(slice, 0, first, 0, index)
					first.asInstanceOf[Array[U]](index) = elem
					arraycopy(slice, index, first, index + 1, Rank - index - 1)
					arraycopy(slice, Rank - 1, second, 0, Rank)
				} else {
					val secondIndex = index - Rank
					arraycopy(slice, 0, first, 0, Rank)
					arraycopy(slice, Rank, second, 0, secondIndex)
					second.asInstanceOf[Array[U]](secondIndex) = elem
					arraycopy(slice, index, second, secondIndex + 1, MaxChildren - index)
				}
			}
			if (index < prefix.length) { //inserting within prefix
				if (prefix.length < MaxChildren) //There is room in prefix, perform the simplest insertion
					new Jack2(prefix.inserted(index, elem), infix, lengthBeforeSuffix + 1, suffix)
				else if (infix.length > 0) { //prefix.length == MaxChildren
					val sibling = infix(0)
					if (sibling.length < MaxChildren) { //carry over last elements of prefix to the first slice in infix
						val newPrefix  = new Array[Any](sibling.length + 1).asInstanceOf[IRefArray[U]]
						val newSibling = new Array[Any](MaxChildren).asInstanceOf[IRefArray[U]]
						shiftRightAndInsert(prefix, sibling, index, newPrefix, newSibling)
						new Jack2(newPrefix, infix.updated(0, newSibling), lengthBeforeSuffix + 1, suffix)
					} else { //split prefix, moving the second half to infix
						val newPrefix  = new Array[Any](Rank).asInstanceOf[IRefArray[U]]
						val newSibling = new Array[Any](Rank).asInstanceOf[IRefArray[U]]
						insertAndSplit(prefix, newPrefix, newSibling, index)
						if (infix.length < MaxChildren - 2)  //move the second half of prefix as the first slice in infix
							new Jack2(newPrefix, infix.prepended(newSibling), lengthBeforeSuffix + 1, suffix)
						else { //split infix and grow the tree
							val postPrefix = new Array[AnyRef](Rank - 1)
							postPrefix(0)  = newSibling
							arraycopy(infix, 0, postPrefix, 1, Rank - 2)
							val preSuffix = infix.slice(Rank - 2, 2 * Rank - 3)
							Jack3(newPrefix, new InnerNode(postPrefix), Node2(preSuffix), suffix, length + 1)
						}
					}
				} else if (suffix.length < MaxChildren) { //infix.length == 0; carry over last elements of prefix to suffix to make room
					val length = MaxChildren + suffix.length
					val prefixSize = length >> 1
					val newPrefix  = new Array[Any](prefixSize).asInstanceOf[IRefArray[U]]
					val newSuffix  = new Array[Any](length + 1 - prefixSize).asInstanceOf[IRefArray[U]]
					shiftRightAndInsert(prefix, suffix, index, newPrefix, newSuffix)
					new Jack2(newPrefix, emptyLevel2Tree[U], prefixSize, newSuffix)
				} else { //split prefix into two, initializing infix with the second half
					val newPrefix  = ErasedArray.ofDim[U](Rank).asInstanceOf[IRefArray[U]]
					val postPrefix = ErasedArray.ofDim[U](Rank).asInstanceOf[IRefArray[U]]
					insertAndSplit(prefix, newPrefix, postPrefix, index)
					new Jack2(newPrefix, IArray.one(postPrefix), lengthBeforeSuffix + 1, suffix)
				}
			} else if (index >= lengthBeforeSuffix) { //inserting within the suffix
				val indexInSuffix = index - lengthBeforeSuffix
				if (suffix.length < MaxChildren)      //there is room left in suffix, expand the array and return
					new Jack2(prefix, infix, lengthBeforeSuffix, suffix.inserted(indexInSuffix, elem))
				else if (infix.length > 0) {
					val sibling = infix(infix.length - 1)
					if (sibling.length < MaxChildren) { //carry over first elements of suffix to the last slice in infix
						val newSibling = new Array[Any](MaxChildren).asInstanceOf[IRefArray[U]]
						val newSuffix  = new Array[Any](sibling.length + 1).asInstanceOf[IRefArray[U]]
						shiftLeftAndInsert(sibling, suffix, sibling.length + indexInSuffix, newSibling, newSuffix)
						new Jack2(prefix, infix.updated(infix.length, newSibling), newSuffix)
					} else { //insert into the suffix and split it
						val newSibling = new Array[Any](Rank).asInstanceOf[IRefArray[U]]
						val newSuffix  = new Array[Any](Rank).asInstanceOf[IRefArray[U]]
						insertAndSplit(suffix, newSibling, newSuffix, indexInSuffix)
						if (infix.length < MaxChildren - 2) //move the first half of split suffix to the end of infix
							new Jack2(prefix, infix appended newSibling, length - Rank, suffix)
						else { //split infix after including the first half of suffix and grow the tree
							val postPrefix = infix.slice(0, Rank - 1)
							val preSuffix  = new Array[AnyRef](Rank - 1)
							arraycopy(infix, Rank - 1, preSuffix, 0, Rank - 2)
							preSuffix(Rank - 2) = newSibling
							Jack3(prefix, Node2(postPrefix), new InnerNode(preSuffix), newSuffix, length + 1)
						}
					}

				} else { //infix.length == 0
					if (prefix.length < MaxChildren) { //shift leading elements from suffix to prefix to make room
						val length = MaxChildren + prefix.length + 1
						val prefixSize = length >> 1
						val newPrefix = new Array[Any](prefixSize).asInstanceOf[IRefArray[U]]
						val newSuffix = new Array[Any](length - prefixSize).asInstanceOf[IRefArray[U]]
						shiftLeftAndInsert(prefix, suffix, index, newPrefix, newSuffix)
						new Jack2(newPrefix, emptyLevel2Tree[U], prefixSize, newSuffix)
					} else { //create an infix by splitting suffix into two parts
						val newSibling = new Array[Any](Rank).asInstanceOf[IRefArray[U]]
						val newSuffix  = new Array[Any](Rank).asInstanceOf[IRefArray[U]]
						insertAndSplit(suffix, newSibling, newSuffix, indexInSuffix)
						new Jack2(prefix, IArray.one(newSibling), length - Rank, newSuffix)
					}
				}
			} else { //insert into infix
				val finger     = infixFinger(index - prefix.length)
				val sliceIdx   = finger.finger
				val idxInSlice = finger.relative
				val slice = infix(sliceIdx)
				if (slice.length < MaxChildren) { //room in the appropriate slice, so just insert into it
					val newSlice = slice.inserted(idxInSlice, elem)
					new Jack2(prefix, infix.updated(sliceIdx, newSlice), lengthBeforeSuffix + 1, suffix)
				} else if (infix.length == 1) { //slice.length == MaxChildren: split the slice to make room
					val first = new Array[Any](Rank).asInstanceOf[IRefArray[U]]
					val second = new Array[Any](Rank).asInstanceOf[IRefArray[U]]
					insertAndSplit(slice, first, second, idxInSlice)
					new Jack2(prefix, IArray.two(first, second), lengthBeforeSuffix + 1, suffix)
				} else {
					var sibling = IRefArray.empty[E]
					val shiftToRightSibling =
						sliceIdx < infix.length - 1 && {
							sibling = infix(sliceIdx + 1)
							sibling.length < MaxChildren
						}
					if (shiftToRightSibling) { //move some elements from slice to the following slice
						val totalLength    = MaxChildren + sibling.length
						val newSliceLength = totalLength >> 1
						val newSlice       = new Array[Any](newSliceLength).asInstanceOf[IRefArray[U]]
						val newSibling     = new Array[Any](totalLength - newSliceLength).asInstanceOf[IRefArray[U]]
						shiftRightAndInsert(slice, sibling, idxInSlice, newSlice, newSibling)
						val newInfix = infix.updated(sliceIdx, newSlice, newSibling)
						new Jack2(prefix, newInfix, lengthBeforeSuffix + 1, suffix)
					} else if ({ sibling = infix(sliceIdx - 1); sibling.length < MaxChildren }) { //move elements to the left sibling
						val totalLength    = MaxChildren + sibling.length
						val newSliceLength = totalLength >> 1
						val newSibling     = new Array[Any](totalLength - newSliceLength).asInstanceOf[IRefArray[U]]
						val newSlice       = new Array[Any](newSliceLength).asInstanceOf[IRefArray[U]]
						shiftLeftAndInsert(sibling, slice, sibling.length + idxInSlice, newSibling, newSlice)
						val newInfix = infix.updated(sliceIdx - 1, newSibling, newSlice)
						new Jack2(prefix, newInfix, lengthBeforeSuffix + 1, suffix)
					} else { //split slice into two parts
						val newSlice   = new Array[Any](Rank).asInstanceOf[IRefArray[U]]
						val newSibling = new Array[Any](Rank).asInstanceOf[IRefArray[U]]
						insertAndSplit(slice, newSlice, newSibling, idxInSlice)
						if (infix.length < MaxChildren - 2) { //swap slice for newSlice and newSibling in infix
							val newInfix = new Array[IRefArray[U]](infix.length + 1)
							newInfix(0) = newSlice
							newInfix(1) = newSibling
							arraycopy(infix, 1, newInfix, 2, infix.length - 1)
							new Jack2(
								prefix, newInfix.asInstanceOf[IArray[IRefArray[U]]], lengthBeforeSuffix + 1, suffix
							)
						} else { //split slice into two parts, then split infix into two parts and grow the tree
							val left  = new Array[AnyRef](Rank - 1)
							val right = new Array[AnyRef](Rank - 1)
							if (idxInSlice == Rank - 2) { //slice is the last slice of left after split
								arraycopy(infix, 0, left, 0, Rank - 2)
								left(Rank - 2) = newSlice
								right(0)       = newSibling
								arraycopy(infix, Rank - 1, right, 0, Rank - 1)
							} else if (idxInSlice < Rank - 2) { //both newSlice and newSibling are in left
								arraycopy(infix, 0, left, 0, idxInSlice)
								left(idxInSlice)     = newSlice
								left(idxInSlice + 1) = newSibling
								arraycopy(infix, idxInSlice + 1, left, idxInSlice + 2, Rank - 3 - idxInSlice)
								arraycopy(infix, Rank - 2, right, 0, Rank - 1)
							} else { //both newSlice and newSibling are in right
								//idxInSlice <= MaxChildren - 3 == 2*Rank - 4  =>  idxInRight <= Rank - 3
								val idxInRight = idxInSlice - Rank + 1 // <= MaxChildren - 3 - Rank +1
								arraycopy(infix, 0, left, 0, Rank - 1)
								arraycopy(infix, Rank - 1, right, 0, idxInRight)
								right(idxInRight)     = newSlice
								right(idxInRight + 1) = newSibling
								arraycopy(infix, idxInSlice + 1, right, idxInRight + 2, Rank - 3 - idxInRight)
							}
							Jack3(prefix, new InnerNode(left), new InnerNode(right), suffix, length + 1)
						}
					}
				}
			}
		}

		override def appended[U >: E](elem :U) :Jack[U] =
			if (suffix.length < MaxChildren)  //There is still room in the suffix, so simply append to it.
				new Jack2(prefix, infix, lengthBeforeSuffix, suffix appended elem)
			else {
				val sibling = infix.last
				val siblingSize = sibling.length
				if (siblingSize < MaxChildren) { //Fill the preceding slice with first elements of suffix.
					val shift = MaxChildren - siblingSize
					val newSibling = IRefArray.copyOfRanges(sibling, 0, siblingSize, suffix, 0, shift)
					val newSuffix  = new Array[Any](siblingSize + 1)
					arraycopy(suffix, shift, newSuffix, 0, siblingSize)
					newSuffix(siblingSize) = elem.asInstanceOf[E]
					val newInfix = infix.updated(infix.length - 1, newSibling)
					new Jack2(prefix, newInfix, lengthBeforeSuffix + shift, newSuffix.asInstanceOf[IRefArray[U]])
				} else {
					val newSibling = suffix.take(Rank)
					val newSuffix  = ErasedArray.ofDim[E](Rank)
					arraycopy(suffix, Rank, newSuffix, 0, Rank - 1)
					newSuffix(Rank - 1) = elem.asInstanceOf[E]
					if (infix.length < MaxChildren - 2) { //Split suffix into two slices, append the first one to infix.
						val newInfix = infix appended newSibling
						new Jack2(prefix, newInfix, lengthBeforeSuffix + Rank, newSuffix.asInstanceOf[IRefArray[U]])
					} else { //Split suffix into two slices, increase the level of the tree.
						val postPrefix = infix.take(Rank - 1)
						val preSuffix  = new Array[IRefArray[U]](Rank - 1)
						arraycopy(infix, Rank - 1, preSuffix, 0, Rank - 2)
						preSuffix(Rank - 2) = newSibling
						Jack3(
							prefix, Node2(postPrefix),
							Node2(preSuffix.castCons[IArray]), newSuffix.asInstanceOf[IRefArray[U]], length + 1
						)
					}
				}
			}
	}



	@inline private def Jack3[E](first :IRefArray[E], second :InnerNode[E], third :InnerNode[E], forth :IRefArray[E],
	                             length :Int)
			:JackN[E] =
	{
		val prefixLength = first.length + second.length
		new JackN[E](
			first, IArray.one(second), emptyIntArray, prefixLength,
			emptyTree, emptyIntArray, prefixLength,
			IArray.one(third), emptyIntArray, forth, length
		)
	}
	@inline private def JackN[E](prefix :IRefArray[E], prefixes :IArray[InnerNode[E]], infix :IArray[InnerNode[E]],
	                             suffixes :IArray[InnerNode[E]], suffix :IRefArray[E]) :JackN[E] =
		JackN(prefix, prefixes, prefixLengthsOf(prefixes), infix, prefixLengthsOf(infix),
		      suffixes, prefixLengthsOf(suffixes), suffix)

	private def JackN[E](prefix :IRefArray[E], prefixes :IArray[InnerNode[E]], prefixLengths :Array[Int],
	                     infix :IArray[InnerNode[E]], infixLengths :Array[Int],
	                     suffixes :IArray[InnerNode[E]], suffixLengths :Array[Int], suffix :IRefArray[E]) :JackN[E] =
	{
		val totalPrefixLength = prefix.length + prefixLengths(prefixes.length - 1) //(if (prefixes.length == 0) 0 else prefixLengths(prefixes.length - 1))
		val totalInfixLength  = totalPrefixLength + infixLengths(infix.length - 1) //(if (infix.length == 0) 0 else infixLengths(infix.length - 1))
		val totalSuffixLength = totalInfixLength + suffixLengths(suffixes.length - 1) //(if (suffix.length == 0) 0 else suffixLengths(suffixes.length - 1))
		new JackN(
			prefix, prefixes, prefixLengths, totalPrefixLength,
			infix, infixLengths, totalInfixLength,
			suffixes, suffixLengths, suffix, totalSuffixLength + suffix.length
		)
	}

	/** A finger tree of length `N > 2` isomorphic with a B-Tree.
	  * @param prefix            The first `Rank <= n <= MaxChildren` elements in the sequence.
	  *                          It is the leftmost bottom node in the represented B-Tree.
	  * @param prefixes          An array of left finger trees of levels (depths) `2..N - 1`, representing the left-path
	  *                          from the B-Tree root to node `prefix`. The first tree is of rank
	  *                          `0 <= r <= MaxChildren - 1`; all following nodes, and all children
	  *                          (both `InnerNode`s and leaf arrays) are of rank `Rank <= r <= MaxChildren`.
	  * @param prefixLengths     Running total length of trees in `prefixes`: `prefixes.scanLeft(0)(_ + _.length).tail`
	  * @param totalPrefixLength `prefix + prefixes.foldLeft(0)(_ + _.length)`.
	  * @param infix             From zero to `MaxChildren - 2` inner children of the root of the B-Tree
	  *                          of level `N - 1` (the same as `prefixes(N - 3)` and `suffixes(0)`). All nodes under
	  *                          these children have rank `Rank <= r <= MaxChildren`.
	  * @param infixLengths      Running combined lengths of initial nodes in `infix`:
	  *                          `infix.scanLeft(0)(_ + _.length).tail`.
	  * @param totalInfixLength  The length of the prefix of this sequence consisting of `prefix`, `prefixes` and `infix`.
	  * @param suffixes          An array of right finger trees of levels (depths) `(N-1)..2`,
	  *                          representing the right-path from the B-Tree root to node `suffix`.
	  *                          The last tree is of rank `Rank - 1 <= r <= MaxChildren - 1`; all previous nodes,
	  *                          and all their children (both `InnerNode`s and leaf arrays) are of the standard rank
	  *                          `Rank <= r <= MaxChildren`.
	  * @param suffixLengths     running combined lengths of trees in `suffixes` `suffixes.scanLeft(0)(_+_.length).tail`.
	  * @param suffix            The last `Rank <= n <= MaxChildren` elements in the sequence.
	  *                          It is the rightmost bottom node in the represented B-Tree.
	  * @param length            The total length of this sequence.
	  */
	@SerialVersionUID(Ver)
	private class JackN[+E](prefix :IRefArray[E], prefixes :IArray[InnerNode[E]], prefixLengths :Array[Int],
	                        totalPrefixLength :Int,
	                        infix :IArray[InnerNode[E]], infixLengths :Array[Int], totalInfixLength :Int,
	                        suffixes :IArray[InnerNode[E]], suffixLengths :Array[Int], suffix :IRefArray[E],
	                        override val length :Int)
		extends Jack[E]
	{
		def depth :Int = prefix.length

//		private def childIndex(i :Int, prefixes :Array[Int]) :Int = ???
		private def prefixSlice(i :Int) :Int = ???
		private def suffixSlice(i :Int) :Int = ???
		private def infixSlice(i :Int) :Int = ???
		private def prefixFinger(i :Int) :FingerIndex = ???
		private def infixFinger(i :Int) :FingerIndex = ???
		private def suffixFinger(i :Int) :FingerIndex = ???

		override def apply(i :Int) :E =
			if (i < 0 | i > length)
				throw new IndexOutOfBoundsException(i.toString + " out of " + length)
			else if (i < prefix.length)
				prefix(i)
			else if (i > length - suffix.length)
				suffix(i - (length - suffix.length))
			else if (i < totalPrefixLength) {
				val finger = prefixFinger(i - prefix.length)
				prefixes(finger.finger)(finger.relative)
			} else if (i < totalInfixLength) {
				val finger = infixFinger(i - totalPrefixLength)
				infix(finger.finger)(finger.relative)
			} else {
				val finger = suffixFinger(i - totalInfixLength)
				suffixes(finger.finger)(finger.relative)
			}

		override def removed(index :Int) :Jack[E] = ???

		override def inserted[U >: E](index :Int, elem :U) :Jack[U] = ???

		override def appended[U >: E](elem :U) :Jack[U] =
			if (suffix.length < MaxChildren) //just grow the suffix only
				new JackN(
					prefix, prefixes, prefixLengths, totalPrefixLength, infix, infixLengths, totalInfixLength,
					suffixes, suffixLengths, suffix.appended(elem), length + 1
				)
			else {
				val node = suffixes.last
				var siblings = node.children//.asInstanceOf[IArray[IRefArray[E]]]
				val predecessor = siblings.last.asInstanceOf[IRefArray[E]]
				val predecessorLength = predecessor.length
				if (predecessorLength < MaxChildren) { //shift first elements from suffix to the preceding slice
					val shift = MaxChildren - predecessorLength
					val newSuffix = new Array[Any](predecessorLength + 1)
					arraycopy(suffix, shift, newSuffix, 0, predecessorLength)
					newSuffix(predecessorLength) = elem
					val newPredecessor = IRefArray.copyOfRanges(
						predecessor, 0, predecessorLength, suffix, 0, shift
					)
					val newNode = node.updatedSlice(siblings.length - 1, newPredecessor)
					val newSuffixes = suffixes.updated(suffixes.length - 1, newNode)
					new JackN(
						prefix, prefixes, prefixLengths, totalPrefixLength, infix, infixLengths, totalInfixLength,
						newSuffixes, suffixLengths, newSuffix.asInstanceOf[IRefArray[U]], length + 1
					)
				} else { //split suffix into newSibling and newSuffix, with elem appended to the latter
					val newSibling = suffix.take(Rank)
					val newSuffix  = new Array[Any](Rank).asInstanceOf[IRefArray[U]]
					arraycopy(suffix, Rank, newSuffix, 0, Rank - 1)
					(newSuffix.asInstanceOf[Array[U]])(Rank - 1) = elem
					if (siblings.length < MaxChildren - 1) { //add a new slice of level 1 to the last suffix node
						val newNode = node.appendedSlice(newSibling)
						val newSuffixes = suffixes.updated(suffixes.length - 1, newNode)
						new JackN(
							prefix, prefixes, prefixLengths, totalPrefixLength, infix, infixLengths, totalInfixLength,
							newSuffixes, suffixLengths, newSuffix.asInstanceOf[IRefArray[U]], length + 1
						)
					} else {
						var level = suffixes.length - 1
						val newSuffixes = new Array[InnerNode[U]](suffixes.length) //IArray.suffixes
						var carry :AnyRef = newSibling //first Rank nodes of suffixes(level)
						var parent = node
						//Split parent into two halves: append carry to the second,
						// and use it as the new newSuffixes(level); the first half becomes carry.
						while (level >= 0 && {
							parent = suffixes(level)
							siblings = parent.children
							siblings.length == MaxChildren - 1
						}) {
							val newSiblings = new Array[AnyRef](Rank - 1)
							arraycopy(siblings, Rank, newSiblings, 0, Rank - 2)
							newSiblings(Rank - 2) = carry
							val second = new InnerNode(newSiblings)
							newSuffixes(level) = second
							carry = parent.takeSlices(Rank)
							level -= 1
						}
						if (level >= 0) { //suffixes(level).length < MaxChildren - 1: append carry to it.
							newSuffixes(level) = parent.appendedSlice(carry)
							if (level > 0)
								arraycopy(suffixes, 0, newSuffixes, 0, level - 1)
							new JackN(
								prefix, prefixes, prefixLengths, totalPrefixLength,
								infix, infixLengths, totalInfixLength,
								newSuffixes.castCons[IArray], prefixLengthsOf(newSuffixes), newSuffix, length + 1
							)
						} else if (infix.length < MaxChildren - 2) { //updated all in newSuffix; append carry to infix
							val newInfix = infix.appended(carry.asInstanceOf[InnerNode[U]])
							val newInfixLengths = appendPrefixLength(newInfix, infixLengths, infixLengths.length)
							new JackN(
								prefix, prefixes, prefixLengths, totalPrefixLength,
								newInfix, newInfixLengths, totalInfixLength + carry.asInstanceOf[InnerNode[U]].length,
								newSuffixes.castCons[IArray], prefixLengthsOf(newSuffixes), newSuffix, length + 1
							)
						} else { //grow the tree; use carry as the first suffix, and current infix as the last prefix
							val infixAsNode = Node(infix, infixLengths, totalInfixLength - totalPrefixLength)
							val newPrefixes = prefixes appended infixAsNode
							val newPrefixLengths = appendPrefixLength(newPrefixes, prefixLengths, prefixLengths.length)
							val extendedSuffixes = (newSuffixes prepended carry).asInstanceOf[IArray[InnerNode[U]]]
							new JackN[U](
								prefix, newPrefixes, newPrefixLengths, totalInfixLength,
								emptyTree, emptyIntArray, totalInfixLength,
								extendedSuffixes, prefixLengthsOf(extendedSuffixes), newSuffix, length + 1
							)
						}
					}
				}
			}

		override def take(n :Int) :Jack[E] =
			if (n <= 0)
				Jack0
			else if (n > length)
				this
			else if (n < totalPrefixLength) {
				if (n < prefix.length)
					new Jack1(prefix.take(n))
				else
					???
			} else if (n < totalPrefixLength + totalInfixLength) {
				???
			} else {
				???
			}
	}



	final class ReversedJack[+T](underlying :Jack[T]) extends Jack[T] {
		private[this] val len = underlying.length
		override def length :Int = len

		override def apply(i :Int) :T =
			if (i < 0) throw new IndexOutOfBoundsException(i.toString + " out of " + len)
			else underlying(len - i - 1)

		override def removed(index :Int) :Jack[T] =
			if (index < 0) throw new IndexOutOfBoundsException(index.toString + " out of " + len)
			else new ReversedJack(underlying.removed(len - index - 1))

		override def inserted[U >: T](index :Int, elem :U) :Jack[U] =
			if (index < 0) throw new IndexOutOfBoundsException(index.toString + " out of " + len)
			else new ReversedJack(underlying.inserted(len - index, elem)) //not len - index - 1!

		override def iterator :Iterator[T] = underlying.reverseIterator
		override def reverseIterator :Iterator[T] = underlying.iterator

		override def reverse :Jack[T] = underlying
	}




	@inline private def Node2[E](slices :IArray[IRefArray[E]]) :InnerNode[E] =
		new InnerNode(slices.asInstanceOf[Array[AnyRef]], prefixLengthsOf(slices))

	@inline private def Node[E](slices :IArray[InnerNode[E]]) :InnerNode[E] =
		new InnerNode(slices.asInstanceOf[Array[AnyRef]], prefixLengthsOf(slices))

	@inline private def Node[E](slices :IArray[InnerNode[E]], lengths :Array[Int], length :Int) :InnerNode[E] =
		new InnerNode(slices.asInstanceOf[Array[AnyRef]], lengths, length)

	/** An inner node of level (depth) at least `2` in a finger tree `JackN`.
	  * @param slices        the children of this node. If this node is of level 2, each element is an `IRefArray[E]]`;
	  *                      otherwise, it is an `InnerNode[E]`. Nodes in `Fingers.prefixes` and
	  *                      `Fingers.suffixes` have rank `Rank - 1 <= r <= MaxChildren - 1`. Nodes in `Fingers.infix`
	  *                      have rank `Rank - 2 <= r <= MaxChildren - 2`. Child nodes of the former and the latter
	  *                      (true inner nodes) have rank `Rank <= r <= MaxChildren`.
	  * @param prefixLengths running total length of child nodes in `slices`:
	  *                      `slices.map(totalLengthOf).scanLeft(0)(_ + _).tail.init`.
	  * @param length        the total number of leaves under this node.
	  */
	@SerialVersionUID(Ver)
	private final class InnerNode[+E] (slices :Array[AnyRef], prefixLengths :Array[Int], val length :Int) {
		def this(slices :Array[AnyRef], prefixLengths :Array[Int]) =
			this(slices, prefixLengths, {
				val rank = slices.length; prefixLengths(rank - 2) + totalLengthOf(slices(rank - 1))
			})

//		def this(slices :IArray[IRefArray[E]], prefixLengths :Array[Int], length :Int) =
//			this(slices.asInstanceOf[Array[AnyRef]], prefixLengths, length)
//
//		def this(slices :IArray[IRefArray[E]]) = this(slices.asInstanceOf[Array[AnyRef]], prefixLengthsOf(slices))

//		def this(slices :IArray[InnerNode[E]], prefixLengths :Array[Int], length :Int) =
//			this(slices.asInstanceOf[Array[AnyRef]], prefixLengths, length)
//
//		def this(slices :IArray[InnerNode[E]]) = this(slices.asInstanceOf[Array[AnyRef]], prefixLengthsOf(slices))
		def this(slices :Array[AnyRef]) = this(slices, prefixLengthsOf(slices))

		def children = slices
		def lengths = prefixLengths
		def rank = slices.length

		def depth :Int = {
			@tailrec def descend(tree :Any, acc :Int) :Int = tree match {
				case node :InnerNode[_] => descend(node.children(0), acc + 1)
				case _ => acc + 1
			}
			descend(slices(0), 1)
		}

		def sliceIdx(i :Int) :Int = ???

		def apply(i :Int) :E = {
			val childIdx = sliceIdx(i)
			val relativeIdx = if (childIdx == 0) i else i - prefixLengths(i - 1)
			(slices(childIdx) : @nowarn) match {
				case node :InnerNode[E @unchecked] => node(relativeIdx)
				case array :Array[E @unchecked] => array(childIdx)
			}
		}

		def updated[U >: E](index :Int, elem :U) :InnerNode[U] = ???
		def updatedSlice[U >: E](index :Int, node :InnerNode[U]) :InnerNode[U] = ???
		def updatedSlice[U >: E](index :Int, siblings :IRefArray[U]) :InnerNode[U] = ???

//		def appendedSlice[U >: E](node :InnerNode[U]) :InnerNode[U] = ???
//		def appendedSlice[U >: E](node :IRefArray[U]) :InnerNode[U] = ???
		def appendedSlice(node :AnyRef) :InnerNode[E] = ???

		def takeSlices(n :Int) :InnerNode[E] = ???
		def dropSlices(n :Int) :InnerNode[E] = ???

		def toFingers :Jack[E] = depth match {
			case 2 => new Jack2(
				slices(0).asInstanceOf[IRefArray[E]],
				slices.slice(1, slices.length - 2).asInstanceOf[IArray[IRefArray[E]]],
//				prefixLengths.slice(1, slices.length - 2),
				prefixLengths(slices.length - 1),
				slices(slices.length - 1).asInstanceOf[IRefArray[E]]
			)
			case d =>
//			val d = depth
				val prefixFingers = new Array[InnerNode[E]](d - 2)
				val suffixFingers = new Array[InnerNode[E]](d - 2)

				@tailrec def firstSlice(tree :InnerNode[E], level :Int) :IRefArray[E] =
					if (level > 2)
						firstSlice(tree.children(0).asInstanceOf[InnerNode[E]], level - 1)
					else
						tree.children.asInstanceOf[IRefArray[E]]

				@tailrec def lastSlice(tree :InnerNode[E], level :Int) :IRefArray[E] =
					if (level > 2)
						lastSlice(tree.children(tree.children.length - 1).asInstanceOf[InnerNode[E]], level - 1)
					else
						tree.children.asInstanceOf[IRefArray[E]]

				def splitPrefixFingers(tree :InnerNode[E], level :Int) :InnerNode[E] = level match {
					case 2 =>
//						val children = tree.children
//						prefix(0) = children(0)
						new InnerNode(tree.children.tail)
					case _ =>
						val children = tree.children
						val node = splitPrefixFingers(children(0).asInstanceOf[InnerNode[E]], level - 1)
						prefixFingers(level - 3) = node
						new InnerNode(children.tail)
				}
				def splitSuffixFingers(tree :InnerNode[E], level :Int) :InnerNode[E] = level match {
					case 2 =>
						val children = tree.children
						val treeRank = children.length
//						suffix(suffix.length - 1) = children(treeRank - 1)
						new InnerNode(children.init, tree.lengths.init, tree.lengths(treeRank - 2))
					case _ =>
						val children = tree.children
						val treeRank = children.length
						val node = splitSuffixFingers(children(treeRank - 1).asInstanceOf[InnerNode[E]], level - 1)
						suffixFingers(d - level) = node
						new InnerNode(children.init)
				}
				val rank = slices.length
				val left  = slices(0).asInstanceOf[InnerNode[E]]
				val right = slices(rank - 1).asInstanceOf[InnerNode[E]]
				val prefix = firstSlice(left, d)
				val suffix = lastSlice(right, d)
				prefixFingers(prefixFingers.length - 1) = splitPrefixFingers(left, d)
				suffixFingers(0)                        = splitSuffixFingers(right, d)
				val infix =
					if (rank > 2) {
						val a = new Array[InnerNode[E]](rank - 2)
						arraycopy(slices, 1, a, 0, rank - 2)
						a.asInstanceOf[IArray[InnerNode[E]]]
					} else
						emptyTree
				JackN[E](
					prefix, prefixFingers.castCons[IArray], //prefixLengths.take()
					infix, //prefixLengths.slice(0, rank - 2), prefixLengths(rank - 2),
					suffixFingers.castCons[IArray], suffix//length
				)
		}
	}



	@inline private def FingerIndex(finger :Int, relative :Int) :FingerIndex =
		new FingerIndex(finger << 16 | relative & 0xffff)

	/** A division of an index in a sequence (finger tree) into an index of its child and index within that child. */
	private class FingerIndex(private val bits :Int) extends AnyVal {
		/** Index of the child in which this index lies. */
		@inline def finger = bits >>> 16
		/** Index of the element within child `finger`. */
		@inline def relative = bits & 0xffff
	}


	private def totalLengthOf(tree :Any) :Int = tree match {
		case array :Array[_] =>
			var len = 0
			var i   = array.length
			while (i > 0) {
				i -= 1
				array(i) match {
					case array :Array[_] => len += array.length
					case node            => len += node.asInstanceOf[InnerNode[_]].length
				}
			}
			len
		case _ => tree.asInstanceOf[InnerNode[_]].length
	}

	private def prefixLengthsOf(array :ArrayLike[_]) :Array[Int] = {
		val res = new Array[Int](array.length)
		var i   = 0
		var len = 0
		while (i < array.length) {
			array(i) match {
				case array :Array[_] => len += array.length
				case node            => len += node.asInstanceOf[InnerNode[_]].length
			}
			res(i) = len
			i += 1
		}
		res//.asInstanceOf[IArray[Int]]
	}
	private def appendPrefixLength(newSlices :ArrayLike[_], oldLengths :Array[Int], correctEntries :Int) :Array[Int] = {
		val size = newSlices.length - 1
		val newLengths = new Array[Int](size)
		var i = math.max(correctEntries, 0)
		arraycopy(oldLengths, 0, newLengths, 0, i)
		var last = if (i == 0) 0 else oldLengths(i - 1)
		while (i < size) {
			last += totalLengthOf(newSlices(i))
			newLengths(i) = last
			i += 1
		}
		newLengths
	}

	private val emptyTree = IArray.empty[InnerNode[Nothing]]
//
//	@inline private def prefixOf[E](array :Array[E], length :Int) :Array[E] = {
//		val res = new Array[AnyRef](length).asInstanceOf[Array[E]]
//		arraycopy(array, 0, res, 0, length)
//		res
//	}
//	@inline private def suffixOf[E](array :Array[E], offset :Int) :Array[E] = {
//		val length = array.length - offset
//		val res = new Array[AnyRef](length).asInstanceOf[Array[E]]
//		arraycopy(array, offset, res, 0, length)
//		res
//	}

//	@inline private def newLevel2Tree[E](source :IArray[IRefArray[E]], from :Int, until :Int) :IArray[IRefArray[E]] =
//		source.slice(from, until)
//	{
//		val res = new Array[Array[AnyRef]](until - from).asInstanceOf[IArray[IRefArray[E]]]
//		arraycopy(source, from, res, 0, until - from)
//		res
//	}
//	@inline private def newLevel2Tree[E](source :IArray[IRefArray[E]], newSize :Int) :IArray[IRefArray[E]] =
////		IArray.copyOf(source, newSize)
//	{
//		val res = new Array[Array[AnyRef]](newSize).asInstanceOf[IArray[IRefArray[E]]]
//		if (source.length < newSize)
//			arraycopy(source, 0, res, 0, source.length)
//		else
//			arraycopy(source, 0, res, 0, newSize)
//		res
//	}
//	@inline private def newLevel2Tree[E](source :IArray[IRefArray[E]]) :IArray[IRefArray[E]] = {
//		val res = new Array[Array[AnyRef]](source.length).asInstanceOf[IArray[IRefArray[E]]]
//		arraycopy(source, 0, res, 0, source.length)
//		res
//	}
//	@inline private def newLevel2Tree[E](size :Int) :Array[Array[E]] =
//		new Array[Array[AnyRef]](size).asInstanceOf[Array[Array[E]]]

	private def emptyLevel2Tree[E] :IArray[IRefArray[E]] = Level2EmptyTree.asInstanceOf[IArray[IRefArray[E]]]
//	private val emptyInfix :Array[Any] = emptyObjectArray.asInstanceOf[Array[Any]]

	private[this] val Level2EmptyTree = new Array[Array[AnyRef]](0)

	final val Rank = 4
	final val MaxChildren = (Rank << 1) - 1
}
