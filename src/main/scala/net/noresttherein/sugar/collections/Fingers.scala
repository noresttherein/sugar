package net.noresttherein.sugar.collections

import java.lang.{Math => math}
import java.lang.System.arraycopy

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.tailrec
import scala.collection.{AbstractIterator, BufferedIterator, IterableFactoryDefaults, SeqFactory, StrictOptimizedSeqFactory, View}
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.{AbstractSeq, IndexedSeqOps, StrictOptimizedSeqOps}
import scala.collection.mutable.{Builder, ReusableBuilder}

import net.noresttherein.sugar.noSuch_!
import net.noresttherein.sugar.JavaTypes.JStringBuilder
import net.noresttherein.sugar.arrays.{ArrayLike, ErasedArray, IRefArray, IRefArrayIterator, RefArray, RefArrayLike}
import net.noresttherein.sugar.collections.HasFastSlice.preferDropOverIterator
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.concurrent.Fences.releaseFence
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.One
import net.noresttherein.sugar.exceptions.outOfBounds_!

//implicits
import net.noresttherein.sugar.arrays.{ArrayCompanionExtension, ArrayLikeExtension, MutableArrayExtension}
import net.noresttherein.sugar.casting.castTypeParamMethods
import net.noresttherein.sugar.collections.extensions._




/** An indexed sequence backed by a finger tree. It is similar to [[collection.immutable.Vector Vector]],
  * but offers additional methods for inserting and removing elements from the interior of the sequence.
  * Unlike in vector, patching and concatenation with another `Fingers` take `O(log n)` time.
  * All other operations have the same complexity as in vector - that is, `O(1)` for access and modification
  * close to either end of the sequence, `O(log n)` for accessing elements in the middle, and `O(log n)` for slicing.
  * The only operations which take `O(n)` time are those which potentially need to apply a function
  * to all elements of the sequence, such as `map` and `filter`, `find`, and conversions to other collections.
  *
  * It is a typical, all purpose, 'jack of all trades': it does not have any serious weak points and is a good choice
  * if the creator doesn't know how the sequence will be used. However, the cost of being the safe option
  * which will not increase the computational complexity of an algorithm, comes at the cost of not being the best
  * in the most common usages: random access to elements near the middle of the sequence, in particular update,
  * is somewhat slower than in `Vector`, which makes the latter a better choice if the user is not interested
  * in bulk operations.
  * @define Coll `Fingers`
  * @define coll finger tree
  * @author Marcin Mościcki
  */
sealed abstract class Fingers[+E]
	extends AbstractSeq[E] with IndexedSeq[E] with IndexedSeqOps[E, Fingers, Fingers[E]]
	   with SugaredIterable[E] with SugaredSeqOps[E, Fingers, Fingers[E]]
	   with StrictOptimizedSeqOps[E, Fingers, Fingers[E]]
	   with IterableFactoryDefaults[E, Fingers] with DefaultSerializable
{
	override def iterableFactory :SeqFactory[Fingers] = Fingers

	protected override def className :String = "Fingers"
}


/** $factoryInfo
  * @define Coll `Fingers`
  * @define coll finger tree
  */
@SerialVersionUID(Ver)
case object Fingers extends StrictOptimizedSeqFactory[Fingers] {
	import internal._
	import Children._

	override def from[A](source :IterableOnce[A]) :Fingers[A] = source match {
		case fingers :Fingers[A]           => fingers
		case empty if empty.knownSize == 0 => Fingers0
		case _ if source.knownSize > 0 && source.knownSize <= MaxChildren =>
			val rank = source.knownSize
			val values = new Array[Any](rank + 1)
			source.toBasicOps.copyToArray(values, 1, Int.MaxValue)
			new Fingers1(values.asInstanceOf[Tree[A]], rank)
		case _ =>
			val res = newBuilder[A]
			res.sizeHint(source, 0)
			(res ++= source).result()
	}

	override def empty[A] :Fingers[A] = Fingers0

	/** A singleton sequence. */
	def one[A](elem :A) :Fingers[A] = {
		val a = new Array[Any](2)
		a(1) = elem
		new Fingers1(a.asInstanceOf[Tree[A]], 1)
	}

	/** A sequence of two elements. */
	def two[A](first :A, second :A) :Fingers[A] = {
		val a = new Array[Any](3)
		a(1) = first
		a(2) = second
		new Fingers1(a.asInstanceOf[Tree[A]], 1)
	}

	/** Creates a singleton sequence, providing a convenient way to start appending further elements. */
	@inline def :+[A](elem :A) :Fingers[A] = one(elem)

	/** Equivalent to [[net.noresttherein.sugar.collections.Fingers.from from]], but has the binding priority
	  * of standard append method, which makes it more convenient when creating a $coll by concatenating multiple
	  * collections.
	  */
	@inline def :++[A](elems :IterableOnce[A]) :Fingers[A] = from(elems)


	override def newBuilder[A] :Builder[A, Fingers[A]] = new FingersBuilder



	/** The base class for all actual implementations of `Fingers`. */
	private abstract class AbstractFingers[+E](tree :Tree[E], final override val length :Int)
		extends Fingers[E] with StrictOptimizedSeqOps[E, Fingers, Fingers[E]]
		   with SeqSlicingOps[E, Fingers, Fingers[E]]
	{
		private[this] val _prefix = tree.asInstanceOf[Array[Any]]
		@inline final def prefixValues :Children[E] = new Children(_prefix)
		@inline final def prefix :Tree[E] = _prefix.asInstanceOf[Tree[E]]
		def level :Int
		override def applyPreferredMaxLength = Rank << 2

		//The only implementation of Fingers.apply, so that the JVM can inline it.
		final override def apply(i :Int) :E =
			if (i < 0)
				outOfBounds_!(i, this)
			else if (i < _prefix.length - 1)
				_prefix(i + 1).asInstanceOf[E]
			else
				get(i)

		protected def get(index :Int) :E = outOfBounds_!(index, this)

		override def segmentLength(p :E => Boolean, from :Int) :Int =
			if (from >= length) 0
			else {
				val from0 = math.max(0, from)
				val i = indexWhere(p, from0, false)
				if (i < 0) length - from0 else i - from0
			}
		override def indexWhere(p :E => Boolean, from :Int) :Int =
			if (from >= length) -1 else indexWhere(p, math.max(0, from), true)

		protected def indexWhere(p :E => Boolean, from :Int, satisfies :Boolean) :Int

		override def patch[U >: E](from :Int, other :IterableOnce[U], replaced :Int) :Fingers[U] =
			if (replaced <= 0 | from >= length)
				insertedAll(math.max(0, math.min(from, length)), other)
			else if (from <= 0)
				drop(replaced).prependedAll(other)
			else if (replaced >= length - from)
				take(from).appendedAll(other)
			else {
				//todo: Check if it's faster to add tree slices or Fingers slices. Note that there will always be
				// a 'short' and a 'long' slice, so it's not simply 'short slice - slice Fingers, long slice - slice Tree).
				// Note that adding to a builder a Tree greater than the builder itself works essentially as prepend
				// to the added tree.
				val size = other.knownSize
				if (size == replaced)
					updatedAll(from, other)
				else if (other.isInstanceOf[Fingers[_]])
					take(from).appendedAll(other).appendedAll(drop(from + replaced))
				else {
					val res = new FingersBuilder[U]
					res.sizeHint(other, length - replaced)
					(res ++= slice(0, from) ++= other ++= slice(from + replaced, length)).result()
				}
			}

		override def updatedAll[U >: E](index :Int, elems :IterableOnce[U]) :Fingers[U] = {
			def outOfBounds() =
				outOfBounds_!(errorString(this) + ".updatedAll(" + index + ", " + errorString(elems) + ")")
			val size = elems.knownSize
			if (index < 0 || index > length - math.max(size, 0))
				outOfBounds()
			if (size == 0)
				this
			else if (size >= 0 & size <= level)
				elems.foldLeftWithIndex(this :Fingers[U])((res, elem, i) => res.updated(index + i, elem))
			else elems match {
				case other :Fingers1[U] =>
					overwritten(index, other.prefixValues.array, 1, other.length + 1).castParam[U]
				case ErasedArray.Slice(array :Array[U @unchecked], from, until) => //Covers Fingers1
					overwritten(index, array, from, until)
				case _ if size >= 0 & size <= level =>
					elems.foldLeftWithIndex(this :Fingers[U])((res, elem, i) => res.updated(index + i, elem))
				case other :AbstractFingers[U] =>
					val thisTree = toTree
					val thatTree = other.toTree
					val prefix = thisTree.slice(0, index)
					val suffix = thisTree.slice(index + other.length, length)
					suffix.insertedAll(0, thatTree).insertedAll(0, prefix).toSeq
				case _ if elems.knownFinite =>
					val prefix = clippedSlice(0, index).insertedAll(index, elems)
					if (prefix.length > length)
						outOfBounds()
					prefix.insertedAll(prefix.length, clippedSlice(prefix.length, length))
				case _ =>
					val (items, rem) = elems.iterator.splitAt(length - index)
					val res = toTree.overwritten(index, items).toSeq
					if (rem.hasNext)
						outOfBounds()
					res
			}
		}

		override def overwritten[U >: E](index :Int, elems :IterableOnce[U]) :Fingers[U] = {
			val size = elems.knownSize
			if (index >= length | size == 0 | size >= 0 & index + size <= 0)
				this
			else elems match {
				case other :Fingers1[U] =>
					overwritten(index, other.prefixValues.array, 1, other.length + 1).castParam[U]
				case ErasedArray.Slice(array :Array[U @unchecked], from, until) =>
					overwritten(index, array, from, until)
				case _ if index < 0 =>
					if (index == Int.MinValue)
						this
					else if (preferDropOverIterator(elems))
						overwritten(0, elems.toIterableOnceOps.drop(-index))
					else
						overwritten(0, elems.iterator.drop(-index))
				case _ if size >= 0 & size <= length - index =>
					updatedAll(index, elems)
				case _ =>
					val res = new FingersBuilder[U]
					res.sizeHint(length)
					res ++= slice(0, index)
					//We take a slice out of the iterator in vain hope that the builder will use Array.copy.
					val iter = elems.iterator.take(length - index)
					res ++= iter
					if (res.knownSize < length)
						res ++= slice(res.knownSize, length)
					res.result()
			}
		}
		protected def overwritten[U >: E](index :Int, elems :Array[U], from :Int, until :Int) :Fingers[U] =
			toTree.overwritten(math.max(0, index), elems, from + math.max(-index, 0), until).toSeq

		override def appended[U >: E](elem :U) :Fingers[U] = inserted(length, elem)
		override def prepended[U >: E](elem :U) :Fingers[U] = inserted(0, elem)

		override def insertedAll[U >: E](index :Int, elems :IterableOnce[U]) :Fingers[U] = elems match {
			case _ if index < 0 | index > length => outOfBounds_!(index, this)
			case empty if empty.knownSize == 0   => this
			case view  :View[U]                  => insertedAll(index, view.iterator)
			case other :AbstractFingers[U]       => insertedAll(index, other)
			case _                               => insertedDefault(index, elems)
		}

		def insertedAll[U >: E](index :Int, elems :AbstractFingers[U]) :Fingers[U] = elems match {
			case other :Fingers1[U]         => insertedAll(index, other.prefixValues)
			case _ if elems.length <= level => insertedDefault(index, elems)
			case _ if length <= elems.level =>
				val res = new FingersBuilder[U]
				var i = 0
				while (i < index) {
					res += apply(i)
					i += 1
				}
				res ++= elems
				while (i < length) {
					res += apply(i)
					i += 1
				}
				res.result()
			case _ =>
				toTree.insertedAll(index, elems.toTree).toSeq
		}
		def insertedAll[U >: E](index :Int, elems :Children[U]) :AbstractFingers[U]

		def insertedDefault[U >: E](index :Int, elems :IterableOnce[U]) :Fingers[U] = {
			val otherSize = elems.knownSize
			if (otherSize >= 0 && otherSize <= level)
				elems.foldLeftWithIndex(this :Fingers[U])((res, elem, i) => res.inserted(index + i, elem))
			else {
				val res = new FingersBuilder[U]
				res.sizeHint(elems, length)
				val tree = toTree
				res ++= tree.slice(0, index) ++= elems ++= tree.slice(index, length)
				res.result()
			}
		}

		override def appendedAll[U >: E](elems :IterableOnce[U]) :Fingers[U] = insertedAll(length, elems)
		override def prependedAll[U >: E](elems :IterableOnce[U]) :Fingers[U] = insertedAll(0, elems)

		override def removed(from :Int, until :Int) :Fingers[E] =
			if (until <= 0 | until <= from | from >= length)
				this
			else if (from <= 0 & until >= length)
				Fingers0
			else if (from <= 0)
				drop(until)
			else if (until >= length)
				take(from)
			else {
				var rem = math.min(until, length) - from
				if (rem <= level) {
					var res :Fingers[E] = this
					while (rem > 0) {
						res  = res.removed(from)
						rem -= 1
					}
					res
				} else
					clippedSlice(0, from).appendedAll(clippedSlice(until, length))
			}

		/** Converts this finger tree to a perfectly balanced tree. */
		def toTree :Tree[E]

		override def copyToArray[U >: E](xs :Array[U], start :Int, len :Int) :Int = copyRangeToArray(xs, start, 0, len)
		override def copyRangeToArray[U >: E](xs :Array[U], start :Int, from :Int, len :Int) :Int =
			if (len <= 0 | from >= length | length == 0 || start >= xs.length)
				0
			else if (start < 0)
				outOfBounds_!(errorString(this) +
					".copyRangeToArray(" + errorString(xs) + ", " + start + ", " + from + ", " + len + ")"
				)
			else {
				val from0 = math.max(0, from)
				val max   = math.min(length - from0, math.min(xs.length - start, len))
				clippedCopyToArray(xs, start, from0, max)
			}
		protected def clippedCopyToArray[U >: E](xs :Array[U], start :Int, from :Int, len :Int) :Int
	}



	/** An empty finger tree. */
	private object Fingers0 extends AbstractFingers[Nothing](emptyLeaf, 0) {
		override def level = 0

		protected override def indexWhere(p :Nothing => Boolean, from :Int, satisfies :Boolean) :Int = -1

		override def updated[U](i :Int, elem :U) = outOfBounds_!(i.toString + " out of bounds for Fingers.empty")

		override def removed(from :Int, until :Int) :Fingers[Nothing] = this
		override def removed(index :Int) :Fingers[Nothing] =
			outOfBounds_!(index.toString + " out of 0")

		override def insertedAll[U >: Nothing](index :Int, elems :IterableOnce[U]) :Fingers[U] =
			if (index != 0) outOfBounds_!(index, "Fingers.empty.insert")
			else from(elems)

		override def insertedAll[U >: Nothing](index :Int, elems :Children[U]) :AbstractFingers[U] =
			new Fingers1(Tree1(elems), elems.length)

		override def inserted[U >: Nothing](index :Int, elem :U) :Fingers[U] =
			if (index != 0) outOfBounds_!(index.toString + " out of 0")
			else one(elem)

		protected override def clippedSlice(from :Int, until :Int) :Fingers[Nothing] = this

		override def iterator = Iterator.empty
		override def reverse :Fingers[Nothing] = this
		override def toTree :Tree[Nothing] = emptyLeaf

		override def clippedCopyToArray[A >: Nothing](xs :Array[A], start :Int, from :Int, len :Int) :Int = 0
	}



	/** A finger tree of level 1 - a non empty flat list of up to `MaxChildren` elements. */
	private final class Fingers1[+E](tree :Tree[E], len :Int)
		extends AbstractFingers[E](tree, len) with ArraySliceSeqOps[E, Fingers, Fingers[E]]
	{
		releaseFence()
		@inline override def array :Array[E @uncheckedVariance] = prefixValues.array.asInstanceOf[Array[E]]
		@inline def values    :Children[E] = prefixValues
		@inline def toTree    :Tree[E]     = prefix
		override def iterator :Iterator[E] = IRefArrayIterator(prefix.asInstanceOf[IRefArray[E]], 1, length)

		override def startIndex = 1
		override def level :Int = 1

		protected override def indexWhere(p :E => Boolean, from :Int, satisfies :Boolean) :Int =
			new TreeExtension[E](prefixValues.array).indexWhere(p, from, satisfies)

		override def foldLeft[A](z :A)(op :(A, E) => A) :A = new TreeExtension[E](prefixValues.array).foldLeft(z)(op)
		override def foldRight[A](z :A)(op :(E, A) => A) :A = new TreeExtension[E](prefixValues.array).foldRight(z)(op)

		override def foreach[U](f :E => U) :Unit =  new TreeExtension[E](prefixValues.array).foreach(f)

		override def map[O](f :E => O) :Fingers[O] = new Fingers1(prefix.map(f), length)

		override def updated[U >: E](index :Int, elem :U) :Fingers1[U] =
			if (index < 0 || index >= length)
				outOfBounds_!(index, errorString(this) + ".updated")
			else
				new Fingers1(prefixValues.updated(index, elem).array.asInstanceOf[Tree[U]], length)

		override def updatedAll[U >: E](index :Int, elems :IterableOnce[U]) :Fingers1[U] =
			if (index < 0 || index > length)
				outOfBounds_!(index, errorString(this) + ".updatedAll")
			else if (elems.knownSize == 0)
				this
			else {
				val size = elems.knownSize
				if (size >= 0)
					if (size > length - index)
						outOfBounds_!(errorString(this) + ".updatedAll(" + index + ", " + errorString(elems) + ")")
					else
						overwritten(index, elems)
				else {
					val it = elems.iterator.safe
					val res = prefix.overwritten(index, it)
					if (it.hasNext)
						outOfBounds_!(
							errorString(this) + ".updatedAll(" + index + ", " +
								errorString(elems) + "): size > " + (length - index)
						)
					new Fingers1(res, length)
				}
			}
		override def updatedAll[U >: E](index :Int, first :U, second :U, rest :U*) :Fingers1[U] =
			if (index < 0 || index > length - 2)
				outOfBounds_!(errorString(this) + ".updatedAll(" + index + ", _, _, " + errorString(rest) + ")")
			else {
				val res = updatedAll(index + 2, rest)
				res.array(index + 1) = first
				res.array(index + 2) = second
				releaseFence()
				res
			}

		override def overwritten[U >: E](index :Int, elems :IterableOnce[U]) :Fingers1[U] = {
			val size = elems.knownSize
			if (index >= length || index == Int.MinValue || size == 0 || size >= 0 && index <= -size)
				this
			else elems match {
				case empty :Iterable[U] if empty.isEmpty  => this
				case empty :Iterator[U] if !empty.hasNext => this
				case _ =>
					val offset = math.max(0, index)
					val drop   = math.max(0, -index)
					val res = Array.copyOf(prefixValues.array, length + 1)
					elems.copyRangeToArray(res, offset + 1, drop, Int.MaxValue)
					new Fingers1(res.asInstanceOf[Tree[U]], length)
			}
		}

		override def overwritten[U >: E](index :Int, first :U, second :U, rest :U*) :Fingers[U] = {
			val size = rest.knownSize
			if (index >= length || size >= 0 && index <= -size - 2)
				this
			else {
				var res = overwritten(index + 2, rest)
				if (res ne this) {
					math.max(index, -2) match {
						case -2 =>
						case -1 =>
							res.array(1) = second
						case _ =>
							res.array(index + 1) = first
							if (index < length - 1)
								res.array(index + 2) = second
					}
				} else
					math.max(index, -2) match {
						case -2 =>
						case -1 =>
							res = updated(0, second)
						case _ =>
							res = updated(index, first)
							if (index < length - 1)
								res.array(index + 2) = second
					}
				releaseFence()
				res
			}
		}


		override def inserted[U >: E](index :Int, elem :U) :Fingers[U] = {
			val newLength = this.length + 1
			if (index < 0 | index > newLength)
				outOfBounds_!(index.toString + " out of " + newLength)
			if (newLength <= MaxChildren) //Copy the array with the new element included.
				new Fingers1(prefixValues.inserted(index, elem).array.asInstanceOf[Tree[U]], newLength)
			else {                       //Split the array into a prefix and suffix and grow to a Fingers2.
				val siblings = prefixValues.insertAndSplit(index, elem, Rank)
				Fingers2(siblings.first, siblings.second, newLength)
			}
		}

		override def insertedAll[U >: E](index :Int, elems :AbstractFingers[U]) :Fingers[U] = elems match {
			case _ :Fingers1[U] =>
				insertedAll(index, elems.prefixValues)
			case _ => index match {
				case 0           => elems.insertedAll(elems.length, prefixValues)
				case 1           =>
					elems.insertedAll(elems.length, prefixValues.slice(1, length)).inserted(0, prefixValues(0))
				case this.length => elems.insertedAll(0, prefixValues)
				case _ if index == this.length - 1 =>
					elems.insertedAll(0, prefixValues.slice(0, length - 1))
					     .inserted(elems.length + length - 1, prefixValues(length - 1))
				case _ =>
					elems.insertedAll(elems.length, prefixValues.slice(index, length))
					     .insertedAll(0, prefixValues.slice(0, index))
			}
		}

		override def insertedAll[U >: E](index :Int, elems :Children[U]) :AbstractFingers[U] = {
			val elemsLength = elems.length
			val totalLength = length + elemsLength
			val values = prefixValues
			if (totalLength <= MaxChildren) {
				val res = Children.copyOfRanges(
					values, 0, index, elems, 0, elemsLength, values, index, length
				)
				new Fingers1(Tree1(res), totalLength)
			} else
				if (index == length & length >= Rank & elemsLength >= Rank)
					new Fingers2(prefix, emptyInfix, length, Tree1(elems), totalLength)
				else if (index == 0 & length >= Rank & elemsLength >= Rank)
					new Fingers2(Tree1(elems), emptyInfix, elemsLength, prefix, totalLength)
				else {
					val siblings = prefixValues.insertAllAndSplit(index, elems, totalLength >> 1)
					new Fingers2(siblings.first, emptyInfix, totalLength >> 1, siblings.second, totalLength)
				}
		}

		override def appended[U >: E](elem :U) :Fingers[U]  = inserted(length, elem)
		override def prepended[U >: E](elem :U) :Fingers[U] = inserted(0, elem)

		override def removed(index :Int) :Fingers[E] =
			if (index < 0 | index >= length)
				outOfBounds_!(index, this, "removed")
			else if (length == 1)
				Fingers0
			else
				new Fingers1(prefixValues.array.removed(index + 1).asInstanceOf[Tree[E]], length - 1)

		override def removed(from :Int, until :Int) :Fingers[E] =
			if (until <= 0 | until <= from | from >= length)
				this
			else if (from <= 0 & until >= length)
				Fingers0
			else if (from <= 0)
				clippedSlice(until, length)
			else if (until >= length)
				clippedSlice(0, from)
			else {
				val newArray= prefixValues.array.removed(from + 1, until + 1).asInstanceOf[Tree[E]]
				new Fingers1(newArray, length - (until - from))
			}

		override def reverse :Fingers[E] = {
			val res = prefixValues.array.clone()
			res.reverseInPlace(1, length + 1)
			new Fingers1(res.asInstanceOf[Tree[E]], length)
		}

		protected[this] override def newSpecific(array :Array[E], from :Int, until :Int) :Fingers[E] =
			if (from == 1 & until == array.length)
				new Fingers1(array.asInstanceOf[Tree[E]], until - from)
			else
				new Fingers1(Tree1(Children(array.asInstanceOf[RefArray[E]], from, until - from)), until - from)

		override def clippedCopyToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int =
			prefix.copyToArray(xs, start, from, len)

	}



	private def Fingers2[E](prefix :Tree[E], infix :Tree[E], suffix :Tree[E]) :Fingers2[E] = {
		val array   = infix.asInstanceOf[Array[Any]]
		val beforeSuffix = prefix.rank + (
			if (array.length == 1) 0 else array(0).asInstanceOf[Array[Int]](array.length - 2)
		)
		new Fingers2(prefix, infix, beforeSuffix, suffix, beforeSuffix + suffix.rank)
	}

	@inline private def Fingers2[E](prefix :Tree[E], suffix :Tree[E], length :Int) :Fingers2[E] =
		new Fingers2(prefix, emptyInfix, prefix.rank, suffix, length)

	@inline private def Fingers2[E](prefix :Tree[E],  infix :Tree[E], suffix :Tree[E], length :Int) :Fingers2[E] =
		new Fingers2(prefix, infix, prefix.rank + infix.length, suffix, length)

	/** A finger tree of level 2.
	  * @param _prefix      a node of level 1, containing as its children the first `Rank <= n <= MaxChildren`
	  *                     elements in the sequence.
	  * @param infix        a node of level 2 and any rank. Its children are of rank `Rank <= n <= MaxChildren`.
	  * @param suffixOffset `prefix.length + infix.map(_.length).sum`.
	  * @param suffix       a node of level 1, containing as its children the last `Rank <= n <= MaxChildren`
	  *                     elements in the sequence.
	  */
	private final class Fingers2[+E](_prefix :Tree[E], infix :Tree[E], suffixOffset :Int, suffix :Tree[E], len :Int)
		extends AbstractFingers[E](_prefix, len)
	{
		releaseFence()
		//assert(prefix.length + infix.length == suffixOffset)
		//assert(prefix.length + infix.length + suffix.length == length)
		//assert(prefix.level == 1)
		//assert(prefix.isValid)
		//assert(suffix.level == 1)
		//assert(suffix.isValid)
		//assert(infix.rank <= MaxChildren - 2)
		//assert(infix.rank == 0 || infix.level == 2)
		//assert((0 until infix.rank).forall { i => val tree = infix.child(i); tree.level == 1 && tree.isValid })

		def firstLeaf   :Tree[E] = prefix
		def infixTree   :Tree[E] = infix
		def lastLeaf    :Tree[E] = suffix
		def suffixStart :Int = suffixOffset

		override def level = 2

		override def iterator :Iterator[E] = new Iterator2(this)

		override def toTree  :Tree[E] = {
			val infixRank = infix.array.length - 1
			val array = new Array[Any](infixRank + 3)
			array(1)  = prefix
			arraycopy(infix.array, 1, array, 2, infixRank)
			array(infixRank + 2) = suffix
			val root = new Children[Tree[E]](array)
			updateLengths(root)
			Tree(root)
		}

		override def get(i :Int) :E = {
			val prefixLen = prefix.rank
			if (i < prefixLen) prefix(i)
			else if (i >= suffixOffset) suffix(i - suffixOffset)
			else infix(i - prefixLen)
		}

		protected override def indexWhere(p :E => Boolean, from :Int, satisfies :Boolean) :Int = {
			val infixOffset = prefix.rank
			var i = from
			var res = -1
			if (from < infixOffset) {
				res = prefix.indexWhere(p, i, satisfies)
				if (res >= 0)
					return res
				i = infixOffset
			}
			if (from < suffixOffset) {
				res = infix.indexWhere(p, i - infixOffset, satisfies)
				if (res >= 0)
					return res + infixOffset
				i = suffixOffset
			}
			res = suffix.indexWhere(p, i - suffixOffset, satisfies)
			if (res >= 0) res + suffixOffset else -1
		}

		override def foldLeft[A](z :A)(op :(A, E) => A) :A =
			suffix.foldLeft(infix.foldLeft(prefix.foldLeft(z)(op))(op))(op)

		override def foldRight[A](z :A)(op :(E, A) => A) :A =
			prefix.foldRight(infix.foldRight(suffix.foldRight(z)(op))(op))(op)

		override def foreach[U](f :E => U) :Unit = {
			prefix.foreach(f)
			infix.foreach(f)
			suffix.foreach(f)
		}

		override def map[O](f :E => O) :Fingers[O] =
			new Fingers2(prefix.map(f), infix.map(f), suffixOffset, suffix.map(f), length)

		override def updated[U >: E](index :Int, elem :U) :Fingers[U] = {
			val prefixLen = prefix.rank
			if (index < 0) //index == -1 is silently accepted by both Children and TreeExtension.
				outOfBounds_!(index, errorString(this) + ".updated")
			if (index < prefixLen)
				new Fingers2(prefix.updated(index, elem), infix, suffixOffset, suffix, length)
			else if (index >= suffixOffset)
				new Fingers2(prefix, infix, suffixOffset, suffix.updated(index - suffixOffset, elem), length)
			else
				new Fingers2(prefix, infix.updated(index - prefixLen, elem), suffixOffset, suffix, length)
		}

		override def removed(index :Int) :Fingers[E] = {
			if (index < 0 | index >= length)
				outOfBounds_!(index, this, "removed")
			val prefixLen = prefix.rank
			val infixRank = infix.rank
			if (index < prefixLen) {             //The removed element lies in the prefix.
				if (prefixLen > Rank)            //The easy case: simply remove the element from prefix.
					new Fingers2(
						Tree1(prefixValues.removed(index)), infix, suffixOffset - 1, suffix, length - 1
					)
				else if (infixRank > 0) {
					val sibling = infix.child(0).values
					if (sibling.length > Rank) { //Carry over the first element in the infix tree to prefix.
						val newSibling = Tree1(sibling.tail)
						val newInfix   = Tree(infix.children.updated(0, newSibling))
						val newPrefix  = prefixValues.removeAndAppend(index, sibling(0))
						new Fingers2(Tree1(newPrefix), newInfix, suffixOffset - 1, suffix, length - 1)
					} else {                     //Carry over all elements of the first infix child to prefix.
						val newPrefix = prefixValues.removeAndAppendAll(index, sibling)
						val newInfix  = Tree(infix.children.tail)
						new Fingers2(Tree1(newPrefix), newInfix, suffixOffset - 1, suffix, length - 1)
					}
				} else if (suffix.rank > Rank) { //infix.rank == 0 -> carry over the first element from suffix.
					val newPrefix = prefixValues.removeAndAppend(index, suffix.values(0))
					val newSuffix = Tree1(suffix.values.tail)
					new Fingers2(Tree1(newPrefix), infix, suffixOffset, newSuffix, length - 1)
				} else {                         //suffix.rank == Rank -> reduce the level by merging prefix and suffix.
					val removed = Children.copyOfRanges(
						prefixValues, 0, index, prefixValues, index + 1, Rank, suffix.values, 0, Rank
					)
					new Fingers1(Tree1(removed), MaxChildren)
				}
			} else if (index >= suffixOffset) {  //The removed element lies in the suffix.
				val indexInSuffix = index - suffixOffset
				if (suffix.rank > Rank) {        //The simplest case - simply remove an element from the suffix array.
					val newSuffix = Tree1(suffix.values.removed(indexInSuffix))
					new Fingers2(prefix, infix, suffixOffset, newSuffix, length - 1)
				} else if (infix.rank > 0) {
					val sibling = infix.child(infixRank - 1).values
					if (sibling.length > Rank) { //Remove the element from suffix, carry over the last element of infix.
						val newSibling = Tree1(sibling.init)
						val newInfix   = Tree(infix.children.updated(infixRank - 1, newSibling))
						val newSuffix  = suffix.values.removeAndPrepend(indexInSuffix, sibling.last)
						new Fingers2(prefix, newInfix, suffixOffset - 1, Tree1(newSuffix), length - 1)
					} else {                     //Remove the element from suffix, merge it with the last infix slice.
						val newSuffix = suffix.values.removeAndPrependAll(indexInSuffix, sibling)
						val newInfix  = Tree(infix.children.init)
						new Fingers2(prefix, newInfix, suffixOffset - Rank, Tree1(newSuffix), length - 1)
					}
				} else if (prefixLen > Rank) {   //Remove the element from suffix, carry over the last prefix element.
					val newPrefix = Tree1(prefixValues.init)
					val newSuffix = suffix.values.removeAndPrepend(indexInSuffix, prefixValues.last)
					new Fingers2(newPrefix, infix, suffixOffset - 1, Tree1(newSuffix), length - 1)
				} else {                         //Remove the element from suffix, merge with prefix, reducing the level.
					val removed = Children.copyOfRanges(
						prefixValues, 0, Rank,
						suffix.values, 0, indexInSuffix,
						suffix.values, indexInSuffix + 1, Rank
					)
					new Fingers1(Tree1(removed), MaxChildren)
				}
			} else {                             //The removed element lies somewhere in infix.
				val finger   = infix.splitIndex(index - prefixLen)
				val childIdx = finger.child
				val relative = finger.relative
				val slice    = infix.child(childIdx).values
				if (slice.length > Rank) {
					//Easy - remove the element from the slice and update infix with the shorter array.
					val newInfix = Tree(infix.children.updated(childIdx, Tree1(slice.removed(relative))))
					new Fingers2(prefix, newInfix, suffixOffset - 1, suffix, length - 1)
				} else if (infixRank == 0) {     //slice.length == Rank
					if (prefixLen > Rank) {      //Remove the element from the slice, carry over the last prefix element.
						val newSlice = slice.removeAndPrepend(relative, prefix(prefix.length - 1))
						val newInfix = Tree(infix.children.updated(childIdx, Tree1(newSlice)))
						new Fingers2(Tree1(prefixValues.init), newInfix, suffixOffset - 1, suffix, length - 1)
					} else if (suffix.length > Rank) { //Remove the element, carry over the first element from suffix.
						val newSlice = slice.removeAndAppend(relative, suffix.values(0))
						val newInfix = Tree(infix.children.updated(childIdx, Tree1(newSlice)))
						new Fingers2(prefix, newInfix, suffixOffset - 1, Tree1(suffix.values.tail), length - 1)
					} else { //Remove the element and append the slice to prefix, clearing infix completely.
						val newPrefix = Children.copyOfRanges(
							prefixValues, 0, Rank, slice, 0, relative, slice, relative + 1, slice.length
						)
						new Fingers2(Tree1(newPrefix), emptyInfix, suffixOffset - 1, suffix, length - 1)
					}
				} else if (childIdx == 0) {      //slice.length == Rank
					if (infixRank == 1) {
						if (suffix.rank > Rank) {
							val newSlice = slice.removeAndAppend(relative, suffix(0))
							val newInfix = Tree(Children(Tree1(newSlice)))
							val newSuffix = Tree1(suffix.values.tail)
							new Fingers2(prefix, newInfix, suffixOffset, newSuffix, length - 1)
						} else if (prefixValues.length > Rank) {
							val newPrefix = Tree1(prefixValues.init)
							val newSlice  = slice.removeAndPrepend(relative, prefixValues(prefixValues.length - 1))
							val newInfix  = Tree(Children(Tree1(newSlice)))
							new Fingers2(newPrefix, newInfix, suffixOffset - 1, suffix, length - 1)
						} else {
							val newPrefix = Children.copyOfRanges(
								prefixValues, 0, prefixValues.length, slice, 0, relative
							)
							val newSuffix = Children.copyOfRanges(
								slice, relative + 1, slice.length, suffix.values, 0, suffix.rank
							)
							Fingers2(Tree1(newPrefix), emptyInfix, Tree1(newSuffix), length - 1)
						}
					} else {
						val sibling = infix.child(1).values
						if (sibling.length > Rank) {
							//Remove the element, carry over the first element of the next slice.
							val newSlice = slice.removeAndAppend(relative, sibling(0))
							val newInfix = Tree(infix.children.replaced(0, Tree1(newSlice), Tree1(sibling.tail)))
							new Fingers2(prefix, newInfix, suffixOffset - 1, suffix, length - 1)
						} else {                 //Remove the element, merge with the next slice.
							val newSlice = slice.removeAndAppendAll(relative, sibling)
							val newInfix = infix.children.sliceAndUpdate(1, infixRank, 0, Tree1(newSlice))
							new Fingers2(prefix, Tree(newInfix), suffixOffset - 1, suffix, length - 1)
						}
					}
				} else {                         //slice.length == Rank && idx > 0
					//We could mirror the previous case and for inner indices pick left or right sibling.
					val sibling    = infix.child(childIdx - 1).values
					val siblingLen = sibling.length
					if (siblingLen > Rank) {     //Remove the element, carry over the last element of the previous slice.
						val newSlice   = slice.removeAndPrepend(relative, sibling(siblingLen - 1))
						val newSibling = sibling.slice(0, siblingLen - 1)
						val newInfix   = Tree(infix.children.replaced(childIdx - 1, Tree1(newSibling), Tree1(newSlice)))
						new Fingers2(prefix, newInfix, suffixOffset - 1, suffix, length - 1)
					} else {                     //Remove the element, merge slice with the preceding slice.
						val newSlice = slice.removeAndPrependAll(relative, sibling)
						val newInfix = infix.children.removeAndUpdate(childIdx - 1, childIdx - 1, Tree1(newSlice))
						new Fingers2(prefix, Tree(newInfix), suffixOffset -  1, suffix, length - 1)
					}
				}
			}
		}


		override def appended[U >: E](elem :U) :Fingers[U] = insertIntoSuffix(length, elem)
		override def prepended[U >: E](elem :U) :Fingers[U] = insertIntoPrefix(0, elem)

		override def inserted[U >: E](index :Int, elem :U) :Fingers[U] =
			if (index < prefix.rank)
				insertIntoPrefix(index, elem)
			else if (index >= suffixOffset)
				insertIntoSuffix(index, elem)
			else
				insertIntoInfix(index, elem)

		private def insertIntoPrefix[U >: E](index :Int, elem :U) :Fingers[U] =
			if (prefix.rank < MaxChildren) {                //There is room in prefix, perform the simplest insertion.
				val newPrefix = Tree1(prefixValues.inserted(index, elem))
				new Fingers2(newPrefix, infix, suffixOffset + 1, suffix, length + 1)
			} else {                                        //prefix.length == MaxChildren
				val infixRank   = infix.rank
				val sibling     = if (infixRank == 0) suffix.values else infix.child(0).values
				val siblingRank = sibling.length
				if (siblingRank < MaxChildren) {            //Grow the sibling instead.
					if (index == MaxChildren) {             //Insert the element as the first child of sibling instead.
						val newSibling = Tree1(sibling.inserted(0, elem))
						if (infixRank == 0)
							new Fingers2(prefix, emptyInfix, suffixOffset + 1, newSibling, length + 1)
						else {
							val newInfix = Tree(infix.children.updated(0, newSibling))
							new Fingers2(prefix, newInfix, suffixOffset + 1, suffix, length + 1)
						}
					} else {                                //Carry over the last elements of prefix to sibling.
						//Reduce the size of prefix as much as possible to make future inserts into prefix simpler.
						val carryFrom  = math.max(index, siblingRank)
						val newSibling = Tree1(sibling.insertedSlice(0, prefixValues, carryFrom, MaxChildren))
						val newPrefix  = Tree1(prefixValues.sliceAndInsert(0, carryFrom, index, elem))
						if (infixRank == 0)
							new Fingers2(newPrefix, emptyInfix, carryFrom + 1, newSibling, length + 1)
						else {
							val newInfix = Tree(infix.children.updated(0, newSibling))
							new Fingers2(newPrefix, newInfix, suffixOffset + 1, suffix, length + 1)
						}
					}
				} else {                                    //Split prefix, moving the second half to infix.
					val siblings = prefixValues.insertAndSplit(index, elem, Rank)
					if (infixRank == 0)                     //suffix.rank == MaxChildren
						new Fingers2(siblings.first, Tree(Children(siblings.second)), suffixOffset + 1, suffix, length + 1)
					else if (infixRank < MaxChildren - 2) { //Move the second half of prefix as the first child in infix.
						val newPrefix = siblings.first
						val newInfix  = Tree(infix.children.prepended(siblings.second))
						new Fingers2(newPrefix, newInfix, suffixOffset + 1, suffix, length + 1)
					} else {                                //Split infix and grow the tree.
						val newPrefix  = siblings.first
						val postPrefix = Tree(infix.children.sliceAndInsert(0, Rank - 2, 0, siblings.second))
						val preSuffix  = Tree(infix.children.slice(Rank -2, MaxChildren - 2))
						Fingers3(newPrefix, postPrefix, preSuffix, suffix, length + 1)
					}
				}
			}

		private def insertIntoSuffix[U >: E](index :Int, elem :U) :Fingers[U] = {
			val indexInSuffix = index - suffixOffset
			val suffixRank    = suffix.rank
			if (suffixRank < MaxChildren) {      //There is room left in suffix, expand the array and return.
				val newSuffix = Tree1(suffix.values.inserted(indexInSuffix, elem))
				new Fingers2(prefix, infix, suffixOffset, newSuffix, length + 1)
			} else {
				val infixRank   = infix.rank
				val sibling     = if (infixRank == 0) prefixValues else infix.child(infixRank - 1).values
				val siblingRank = sibling.length
				if (siblingRank < MaxChildren) { //Carry over first elements of suffix to the last slice in infix.
					if (indexInSuffix == 0) {
						val newSibling = Tree1(sibling.inserted(siblingRank, elem))
						if (infixRank == 0)
							new Fingers2(newSibling, emptyInfix, suffixOffset + 1, suffix, length + 1)
						else {
							val newInfix = Tree(infix.children.updated(infixRank - 1, newSibling))
							new Fingers2(prefix, newInfix, suffixOffset + 1, suffix, length + 1)
						}
					} else {
						val carriedOver = math.min(indexInSuffix, MaxChildren - siblingRank)
						val newSibling  = Tree1(
							sibling.insertedSlice(siblingRank, suffix.values, 0, carriedOver)
						)
						val newSuffix   = Tree1(
							suffix.values.sliceAndInsert(carriedOver, MaxChildren, indexInSuffix - carriedOver, elem)
						)
						if (infixRank == 0)
							new Fingers2(newSibling, emptyInfix, suffixOffset + carriedOver, newSuffix, length + 1)
						else {
							val newInfix = Tree(infix.children.updated(infixRank - 1, newSibling))
							new Fingers2(prefix, newInfix, suffixOffset + carriedOver, newSuffix, length + 1)
						}
					}
				} else {                         //Insert into the suffix and split it.
					val siblings = suffix.values.insertAndSplit(indexInSuffix, elem, Rank)
					if (infixRank == 0) {
						val newInfix = Tree(Children(siblings.first))
						new Fingers2(prefix, newInfix, suffixOffset + Rank, siblings.second, length + 1)
					} else if (infixRank < MaxChildren - 2) {
						val newInfix = Tree(infix.children.appended(siblings.first))
						new Fingers2(prefix, newInfix, suffixOffset + Rank, siblings.second, length + 1)
					} else {
						val postPrefix = infix.children.slice(0, Rank - 1)
						val preSuffix  = infix.children.sliceAndInsert(
							Rank - 1, MaxChildren - 2, Rank - 2, siblings.first
						)
						Fingers3(prefix, Tree(postPrefix), Tree(preSuffix), siblings.second, length + 1)
					}
				}
			}
		}

		private def insertIntoInfix[U >: E](index :Int, elem :U) :Fingers[U] = {
			val finger      = infix.splitIndex(index - prefix.rank)
			val sliceIdx    = finger.child
			val idxInSlice  = finger.relative
			val targetSlice = infix.child(sliceIdx).values
			if (targetSlice.length < MaxChildren) { //We have room in the appropriate slice, so just insert into it.
				val newSlice = targetSlice.inserted(idxInSlice, elem)
				val newInfix = Tree(infix.children.updated(sliceIdx, Tree1(newSlice)))
				new Fingers2(prefix, newInfix, suffixOffset + 1, suffix, length + 1)
			} else if (infix.rank == 1) {           //targetSlice.length == MaxChildren: split targetSlice to make room.
				val newInfix = targetSlice.insertAndSplit(idxInSlice, elem, Rank).toTree
				new Fingers2(prefix, newInfix, suffixOffset + 1, suffix, length + 1)
			} else {                                //Try to move a child to sibling under suffix.
				var sibling = new Children[U](null)
				val shiftToLeftSibling =
					sliceIdx > 0 && {
						sibling = infix.child(sliceIdx - 1).values
						sibling.length < MaxChildren
					}
				if (shiftToLeftSibling) {
					//todo: divide the elements evenly into targetSlice and sibling
					val newSlice   = targetSlice.sliceAndInsert(1, MaxChildren, idxInSlice - 1, elem)
					val newSibling = sibling.inserted(sibling.length, targetSlice(0))
					val newInfix   = infix.children.updated(sliceIdx - 1, Tree1(newSibling), Tree1(newSlice))
					new Fingers2(prefix, Tree(newInfix), suffixOffset + 1, suffix, length + 1)
				} else if ({ sibling = infix.child(sliceIdx + 1).values; sibling.length < MaxChildren }) {
					val newSibling = sibling.inserted(0, targetSlice(MaxChildren - 1))
					val newSlice   = targetSlice.sliceAndInsert(0, MaxChildren - 1, idxInSlice, elem)
					val newInfix   = infix.children.updated(sliceIdx, Tree1(newSlice), Tree1(newSibling))
					new Fingers2(prefix, Tree(newInfix), suffixOffset + 1, suffix, length + 1)
				} else {                            //Split targetSlice into two parts.
					val siblings = targetSlice.insertAndSplit(idxInSlice, elem, Rank)
					val newInfix = infix.replace(sliceIdx, siblings, Rank - 1, MaxChildren - 2)
					if (newInfix.size == 1)
						new Fingers2(prefix, newInfix.first, suffixOffset + 1, suffix, length + 1)
					else
						Fingers3(prefix, newInfix.first, newInfix.second, suffix, length + 1)
				}
			}
		}

		override def insertedAll[U >: E](index :Int, elems :Children[U]) :AbstractFingers[U] = {
			val prefixLen   = prefix.rank
			val elemsLength = elems.length
			val newLength   = length + elemsLength
			if (index < prefixLen) {                 //Insert into the prefix.
				val combinedLength = prefixLen + elemsLength
				if (combinedLength <= MaxChildren) { //Include all the new elements in the prefix node.
					val newPrefix = Children.copyOfRanges(
						prefixValues, 0, index, elems, 0, elemsLength, prefixValues, index, prefixLen
					)
					new Fingers2(Tree1(newPrefix), infix, suffixOffset + elemsLength, suffix, newLength)
				} else { //Need to create a new successor node to prefix under infix.
					//The trick here is to calculate the lengths of the new prefix and its sibling
					// so they fall in the required range, and then determine where to split.
					val newPrefixLen = math.min(MaxChildren, math.max(Rank, combinedLength - MaxChildren))
					val siblings     =
						if (index == 0 & elemsLength >= Rank)
							Siblings(Tree1(elems), prefix)
						else if (index == prefixLen & elemsLength >= Rank)
							Siblings(prefix, Tree1(elems))
						else
							prefixValues.insertAllAndSplit(index, elems, newPrefixLen)
					swapPrefix(siblings.first, siblings.second, elemsLength)
				}
			} else if (index > suffixOffset) {       //Insert into the suffix.
				val suffixLen      = length - suffixOffset
				val indexInSuffix  = index - suffixOffset
				val combinedLength = suffixLen + elemsLength
				if (combinedLength <= MaxChildren) { //Include all the new elements in the suffix node.
					val newSuffix = Children.copyOfRanges(
						suffix.values, 0, indexInSuffix,
						elems, 0, elemsLength,
						suffix.values, indexInSuffix, suffixLen
					)
					new Fingers2(prefix, infix, suffixOffset, Tree1(newSuffix), length + elemsLength)
				} else { //Need to combine the values of elems and suffix into two legal nodes.
					val newSuffixLen  = math.min(MaxChildren, math.max(Rank, combinedLength - MaxChildren))
					val newSiblingLen = combinedLength - newSuffixLen
					val siblings =
						if (index == length & elemsLength >= Rank)
							Siblings(suffix, Tree1(elems))
						else if (index == suffixOffset & elemsLength >= Rank)
							Siblings(Tree1(elems), suffix)
						else
							suffix.values.insertAllAndSplit(indexInSuffix, elems, newSiblingLen)
					swapSuffix(siblings.first, siblings.second, elemsLength)
				}
			} else {                                 //prefixLen <= index <= lengthBeforeSuffix
				val finger     = infix.splitIndex(index - prefixLen)
				val sliceIdx   = finger.child
				val idxInSlice = finger.relative
				val slice      = infix.child(sliceIdx).values
				val combinedLength = slice.length + elemsLength
				if (combinedLength <= MaxChildren) { //The new values can be inserted into a single node.
					val newSlice = Children.copyOfRanges(
						slice, 0, idxInSlice, elems, 0, elemsLength, slice, idxInSlice, slice.length
					)
					val newInfix = Tree(infix.children.updated(sliceIdx, Tree1(newSlice)))
					new Fingers2(prefix, newInfix, suffixOffset + elemsLength, suffix, newLength)
				} else {                             //Insert into an infix child, and split it into two trees.
					val newSliceLength = math.min(MaxChildren, math.max(Rank, combinedLength - MaxChildren))
					val siblings       =
						if (idxInSlice == 0 & elems.length >= Rank)
							Siblings(Tree1(elems), Tree1(slice))
						else if (idxInSlice + elems.length == combinedLength & elems.length >= Rank)
							Siblings(Tree1(slice), Tree1(elems))
						else
							slice.insertAllAndSplit(idxInSlice, elems, newSliceLength)
					val newSlice       = siblings.first
					val successor      = siblings.second
					infix.rank match {
						case MaxInfixRank =>         //Grow to level 3 in order to incorporate the new successor node.
							val newInfix = infix.replace(sliceIdx, newSlice, successor, Rank - 1, MaxInfixRank)
							Fingers3(prefix, newInfix.first, newInfix.second, suffix, newLength)
						case _ =>                    //Just insert the new postPrefix as the first child of infix.
							val newInfix = Tree(infix.children.replaced(sliceIdx, newSlice, successor))
							new Fingers2(prefix, newInfix, suffixOffset + elemsLength, suffix, newLength)
					}
				}
			}
		}

		private def swapPrefix[U >: E](newPrefix :Tree[U], successor :Tree[U], lengthDelta :Int)
				:AbstractFingers[U] =
			infix.rank match {
				case MaxInfixRank => //Grow to level 3 in order to incorporate the new postPrefix node.
					val postPrefix = Tree(infix.children.sliceAndInsert(0, Rank - 2, 0, successor))
					val preSuffix  = Tree(infix.children.slice(Rank - 2, MaxChildren - 2))
					Fingers3(newPrefix, postPrefix, preSuffix, suffix, length + lengthDelta)
				case 0 =>            //Create a singleton infix node.
					val newInfix = Tree(Children(successor))
					new Fingers2(newPrefix, newInfix, suffixOffset + lengthDelta, suffix, length + lengthDelta)
				case _ =>            //Just insert the new postPrefix as the first child of infix.
					val newInfix = Tree(infix.children.inserted(0, successor))
					new Fingers2(newPrefix, newInfix, suffixOffset + lengthDelta, suffix, length + lengthDelta)
			}
		private def swapSuffix[U >: E](predecessor :Tree[U], newSuffix :Tree[U], lengthDelta :Int)
				:AbstractFingers[U] =
		{
			val newLength = length + lengthDelta
			infix.rank match {
				case MaxInfixRank => //Grow to level 3 in order to accommodate the new preSuffix node.
					val postPrefix = infix.children.slice(0, Rank - 1)
					val preSuffix  = infix.children.sliceAndInsert(
						Rank - 1, MaxChildren - 2, Rank - 2, predecessor
					)
					Fingers3(prefix, Tree(postPrefix), Tree(preSuffix), newSuffix, newLength)
				case 0 =>            //Create a singleton infix node with newSibling.
					val newInfix = Tree(Children(predecessor))
					new Fingers2(prefix, newInfix, newLength - newSuffix.length, newSuffix, newLength)
				case rank =>         //Simply append newSibling to children of infix.
					val newInfix = Tree(infix.children.inserted(rank, predecessor))
					new Fingers2(prefix, newInfix, newLength - newSuffix.length, newSuffix, newLength)
			}
		}

		protected override def clippedSlice(from :Int, until :Int) :Fingers[E] = {
			//todo: divide it into submethods, once we have a good idea of what is reusable.
			val sliceSize  = until - from
			val prefixRank = prefix.rank
			if (until <= prefixRank)                                 //Slice entirely within prefix.
				new Fingers1(Tree1(prefixValues.slice(from, until)), sliceSize)
			else if (from >= suffixOffset) {                         //Slice entirely within suffix.
				new Fingers1(Tree1(suffix.values.slice(from - suffixOffset, until - suffixOffset)), sliceSize)
			} else if (from >= prefixRank & until <= suffixOffset) { //Slice entirely within infix.
				val node = infix.slice(from - prefixRank, until - prefixRank)
				val rank = node.rank
				if (node.isLeaf)
					new Fingers1(node, sliceSize)
				else if (rank == 1) //Possible if from == prefixRank & until == suffixOffset & infix.rank == 1.
					new Fingers1(node.child(0), sliceSize)
				else { //rank >= 2
					val prefix = node.child(0)
					val suffix = node.child(rank - 1)
					val infix  = Tree(node.children.slice(1, rank - 1))
					Fingers2(prefix, infix, suffix, sliceSize)
				}
			} else { //The slice covers parts of at least two out of prefix, infix, and suffix.
				val prefixSliceSize = prefixRank - from
				val suffixSliceSize = until - suffixOffset
				if (prefixRank == suffixOffset) {         //Empty infix.
					//We know !(from >= suffixOffset) & !(until <= prefixRank), so from < prefixRank & until > prefixRank
					if (sliceSize <= MaxChildren) {       //Combine the prefix and suffix slices in Fingers1.
						val concatenated = Children.copyOfRanges(
							prefixValues, from, prefixRank, suffix.values, 0, suffixSliceSize
						)
						new Fingers1(Tree1(concatenated), sliceSize)
					} else if (prefixSliceSize >= Rank & suffixSliceSize >= Rank) {
						val newPrefix = prefix.slice(from, prefixRank)
						val newSuffix = suffix.slice(0, suffixSliceSize)
						new Fingers2(newPrefix, emptyInfix, prefixSliceSize, newSuffix, sliceSize)
					} else if (prefixSliceSize < Rank) {  //Append initial values of the first suffix child to prefix.
						val newPrefixSize = sliceSize >> 1
						val newPrefix = Children.copyOfRanges(
							prefixValues, from, prefixRank, suffix.values, 0, newPrefixSize - prefixSliceSize
						)
						val newSuffix = suffix.slice(newPrefixSize - prefixSliceSize, until - suffixOffset)
						new Fingers2(Tree1(newPrefix), emptyInfix, newPrefixSize, newSuffix, sliceSize)
					} else {                              //Prepend last values of prefix to the first suffix child.
						val newSuffixSize = sliceSize >> 1
						val newPrefixEnd  = prefixRank - (newSuffixSize - suffixSliceSize)
						val newPrefix = prefixValues.slice(from, newPrefixEnd)
						val newSuffix = Children.copyOfRanges(
							prefixValues, newPrefixEnd, prefixRank,
							suffix.values, 0, until - suffixOffset
						)
						new Fingers2(Tree1(newPrefix), emptyInfix, newPrefixEnd - from, Tree1(newSuffix), sliceSize)
					}
				} else if (sliceSize <= MaxChildren) {    //Reduce the slice to Fingers1
					val values =
						if (prefixSliceSize > 0 & suffixSliceSize > 0) {
							Children.copyOfRanges(        //infix.rank == 1, empty infix covered previously.
								prefixValues, from, prefixRank,
								infix.child(0).values, 0, sliceSize - prefixSliceSize - suffixSliceSize,
								suffix.values, 0, suffixSliceSize
							)
						} else if (prefixSliceSize > 0) { //The slice ends within the first or second child of infix.
							val firstInfixChild = infix.child(0).values
							val firstChildSize  = firstInfixChild.length
							if (prefixSliceSize + firstChildSize >= sliceSize)
								Children.copyOfRanges(    //Only one infix child is included.
									prefixValues, from, prefixRank,
									firstInfixChild, 0, until - prefixRank
								)
							else                          //The slice covers the prefix, and two first children of infix.
								Children.copyOfRanges(
									prefixValues, from, prefixRank,
									firstInfixChild, 0, firstChildSize,
									infix.child(1).values, 0, sliceSize - prefixRank - firstChildSize
								)
						} else {   //suffixSliceSize > 0, because !(from >= prefixRank & until <= suffixOffset).
							val lastInfixChild = infix.child(infix.rank - 1).values
							val lastChildSize  = lastInfixChild.length
							if (lastChildSize + suffixSliceSize >= sliceSize)
								Children.copyOfRanges(
									lastInfixChild, lastChildSize - (sliceSize - suffixSliceSize), lastChildSize,
									suffix.values, 0, suffixSliceSize
								)
							else { //The slice covers two last children of infix and the suffix.
								val secondLastChild   = infix.child(infix.rank - 2).values
								val secondLastSize    = secondLastChild.length
								val secondChildOffset = secondLastSize - (sliceSize - suffixSliceSize - lastChildSize)
								Children.copyOfRanges(
									secondLastChild, secondChildOffset, secondLastSize,
									lastInfixChild, 0, lastChildSize,
									suffix.values, 0, suffixSliceSize
								)
							}
						}
					new Fingers1(Tree1(values), sliceSize)
				} else { //infix.rank > 0
					val infixRank  = infix.rank
					val fromIndex  = if (from < prefixRank) SplitIndex(-1, from) else infix.splitIndex(from - prefixRank)
					val fromChild  = if (from < prefixRank) prefixValues else infix.child(fromIndex.child).values
					val fromRank   = if (from < prefixRank) prefixRank else fromChild.length
					val fromLen    = if (from < prefixRank) prefixRank - from else fromRank - fromIndex.relative
					val untilIndex =
						if (until > suffixOffset) SplitIndex(infixRank, until - suffixOffset)
						else infix.splitIndex(until - prefixRank)
					val untilChild = if (until > suffixOffset) suffix.values else infix.child(untilIndex.child).values
					val untilLen   = untilIndex.relative
					if (fromLen >= Rank && untilLen >= Rank) {           //No rebalancing necessary, just slice infix.
						val newPrefix = fromChild.drop(fromIndex.relative)
						val newInfix  = infix.children.slice(fromIndex.child + 1, untilIndex.child)
						val newSuffix = untilChild.take(untilLen)
						Fingers2(Tree1(newPrefix), Tree(newInfix), Tree1(newSuffix), sliceSize)
					} else if (fromIndex.child + 1 == untilIndex.child) {//Only two leaves involved.
						if (fromLen >= Rank) {                           //Move some values from the prefix to the suffix.
							val newPrefix = fromChild.slice(fromIndex.relative, fromIndex.relative + (sliceSize >> 1))
							val newSuffix = Children.copyOfRanges(
								fromChild, fromIndex.relative + (sliceSize >> 1), fromRank,
								untilChild, 0, untilLen
							)
							new Fingers2(Tree1(newPrefix), emptyInfix, sliceSize >> 1, Tree1(newSuffix), sliceSize)
						} else { //untilLen >= Rank because fromLen < Rank and sliceSize > MaxChildren.
							val offset    = (sliceSize >> 1) - fromLen
							val newPrefix = Children.copyOfRanges(
								fromChild, fromIndex.relative, fromRank,
								untilChild, 0, offset
							)
							val newSuffix = untilChild.slice(offset, untilLen)
							new Fingers2(Tree1(newPrefix), emptyInfix, sliceSize >> 1, Tree1(newSuffix), sliceSize)
						}
					} else if (fromIndex.child + 2 == untilIndex.child && sliceSize <= (MaxChildren << 1)) {
						//A single child between the first and last slice, and we can rearrange the three leaves into two.
						val middleChild = infix.child(fromIndex.child + 1).values
						val middleLen   = sliceSize - fromLen - untilLen
						if (fromLen < Rank & untilLen < Rank) { //Divide slice elements equally between prefix and suffix.
							val newPrefix = Children.copyOfRanges(
								fromChild, fromIndex.relative, fromRank,
								middleChild, 0, (sliceSize >> 1) - fromLen
							)
							val newSuffix = Children.copyOfRanges(
								middleChild, (sliceSize >> 1) - fromLen, middleLen,
								untilChild, 0, untilLen
							)
							new Fingers2(Tree1(newPrefix), emptyInfix, sliceSize >> 1, Tree1(newSuffix), sliceSize)
						} else if (fromLen < Rank) {            //Implies untilLen >= Rank.
							val newSuffix = untilChild.take(untilLen)
							val suffixOffset = fromLen + middleLen
							if (fromLen + middleLen <= MaxChildren) {
								val newPrefix = Children.copyOfRanges(
									fromChild, fromIndex.relative, fromRank,
									middleChild, 0, middleLen
								)
								new Fingers2(Tree1(newPrefix), emptyInfix, suffixOffset, Tree1(newSuffix), sliceSize)
							} else {
								val newPrefix = Children.copyOfRanges(
									fromChild, fromIndex.relative, fromRank,
									middleChild, 0, Rank - fromLen
								)
								val newInfix  = Children(Tree1(middleChild.drop(Rank - fromLen)))
								new Fingers2(Tree1(newPrefix), Tree(newInfix), suffixOffset, Tree1(newSuffix), sliceSize)
							}
						} else { //untilLen < Rank, because case fromLen >= Rank & untilLen >= Rank is already covered.
							val newPrefix = fromChild.drop(fromIndex.relative)
							if (middleLen + untilLen <= MaxChildren) {
								val newSuffix = Children.copyOfRanges(
									middleChild, 0, middleLen,
									untilChild, 0, untilLen
								)
								new Fingers2(Tree1(newPrefix), emptyInfix, fromLen, Tree1(newSuffix), sliceSize)
							} else {
								val infixEnd  = middleLen - (Rank - untilLen)
								val newSuffix = Children.copyOfRanges(
									middleChild, infixEnd, middleLen,
									untilChild, 0, untilLen
								)
								val newInfix = Children(Tree1(middleChild.take(infixEnd)))
								new Fingers2(
									Tree1(newPrefix), Tree(newInfix), fromLen + infixEnd, Tree1(newSuffix), sliceSize
								)
							}
						}
					} else { //At least one infix child between fromIndex and untilIndex.
						//We can consider the front and end of the slice independently,
						// as fromChild and untilChild do not compete for padding from the same sibling.
						var newPrefix      = fromChild
						var newPrefixLen   = fromLen
						var infixStartIdx  = fromIndex.child + 1
						var infixStartDrop = 0
						val prefixSibling  = infix.child(infixStartIdx).values
						if (fromLen >= Rank)
							newPrefix = fromChild.drop(fromIndex.relative)
						else if (fromLen + prefixSibling.length <= MaxChildren) {
							newPrefix = Children.copyOfRanges(
								fromChild, fromIndex.relative, fromRank,
								prefixSibling, 0, prefixSibling.length
							)
							newPrefixLen   = fromLen + prefixSibling.length
							infixStartIdx += 1
						} else {
							infixStartDrop = Rank - fromLen
							newPrefix = Children.copyOfRanges(
								fromChild, fromIndex.relative, fromRank,
								prefixSibling, 0, infixStartDrop
							)
						}
						var newSuffix     = untilChild
						var newSuffixLen  = untilLen
						var infixEndIdx   = untilIndex.child - 1
						var infixEndDrop  = 0
						val suffixSibling = infix.child(infixEndIdx).values
						val suffixSiblingLen = suffixSibling.length
						if (untilLen >= Rank)
							newSuffix = untilChild.take(untilLen)
						else if (untilLen + suffixSiblingLen <= MaxChildren) {
							newSuffix = Children.copyOfRanges(
								suffixSibling, 0, suffixSiblingLen,
								untilChild, 0, untilLen
							)
							newSuffixLen = untilLen + suffixSiblingLen
							infixEndIdx -= 1
						} else {
							infixEndDrop = Rank - untilLen
							newSuffix = Children.copyOfRanges(
								suffixSibling, suffixSiblingLen - infixEndDrop, suffixSiblingLen,
								untilChild, 0, untilLen
							)
						}
						val newInfix =
							if (infixStartIdx > infixEndIdx)
								emptyInfix
							else if (infixStartIdx == infixEndIdx) {
								val infixChild = infix.child(infixStartIdx).values
								Tree(Children(Tree1(infixChild.slice(infixStartDrop, infixChild.length - infixEndDrop))))
							} else {
								val array = infix.array.slice(infixStartIdx, infixEndIdx + 2)
								if (infixStartDrop > 0)
									array(1) = {
										val first = new Children[E](array(1).asInstanceOf[Array[Any]])
										first.drop(infixStartDrop).array
									}
								if (infixEndDrop > 0) {
									val newInfixRank = infixEndIdx + 1 - infixStartIdx
									array(newInfixRank) =
										array(newInfixRank).asInstanceOf[Array[Any]].dropRight(infixEndDrop)
								}
								updateLengths(new Children(array))
								array.asInstanceOf[Tree[E]]
							}
						Fingers2(Tree1(newPrefix), newInfix, Tree1(newSuffix))
					}
				}
			}
		}

		protected override def clippedCopyToArray[U >: E](xs :Array[U], start :Int, from :Int, len :Int) :Int = {
			var copied = 0
			val prefixRank = prefixValues.length
			if (from < prefixRank)
				copied += prefix.copyToArray(xs, start, from, len)
			if (copied < len & from < suffixOffset)
				copied += infix.copyToArray(xs, start + copied, from - prefixRank + copied, len - copied)
			if (copied < len)
				copied += suffix.copyToArray(xs, start + copied, from - suffixOffset + copied, len - copied)
			copied
		}

	}



	/** Creates a finger tree of level 3.
	  * @param first  a level 1 node with initial `Rank..MaxChildren` values of the tree.
	  * @param second a level 2 node of rank `Rank-1..MaxChildren-1`.
	  * @param third  a level 2 node of rank `Rank-1..MaxChildren-1`.
	  * @param fourth a level 1 node with the last `Rank..MaxChildren` values of the tree.
	  */
	@inline
	private def Fingers3[E](first :Tree[E], second :Tree[E], third :Tree[E], fourth :Tree[E], length :Int) :FingersN[E] = {
		val prefixLen = first.rank + second.length
		val res = new FingersN(
			first, Tree(Children(second)), prefixLen, emptyInfix, prefixLen, Tree(Children(third)), fourth, length
		)
		res
	}

	/** A finger tree of level 3 or greater.
	  * It is homomorphic with a perfectly balanced [[net.noresttherein.sugar.collections.Fingers.internal.Tree! Tree]]
	  * of level `N` (of normal rank and depth constraints), where the first and last leaf
	  * (`this.prefix` and `this.suffix`), as well as all nodes on paths to both (`this.prefixes` and `this.suffixes`)
	  * are removed from the tree, leaving `infix` with two fewer children.
	  * This provides quicker access to elements which fall into one of the prefix or suffix slices.
	  * @param prefix   A level 1 node of rank `Rank <= r <= MaxChildren`,
	  *                 the leftmost leaf in the represented tree, with the first elements of the sequence.
	  * @param prefixes A pseudo, non balanced node, containing `N-2` children, where `N` is the level of the tree.
	  *                 It forms the 'leftmost path' from the first leaf `prefix` to the root of the tree.
	  *                 The first child is of level 2, and each next sibling is a node is of a level one higher
	  *                 than the previous one, ending with level `N-1`. All children have rank
	  *                 `Rank - 1 <= r <= MaxChildren - 1`, but all grandchildren and further descendants of this node
	  *                 are of rank `Rank <= r <= MaxChildren`.
	  * @param infix    a node of level `N` - the level of the tree - containing `0 <= r <= MaxChildren - 2`
	  *                 inner children of the root of the represented B-Tree. All its descendants of level `N-1`
	  *                 and lower have rank `Rank <= r <= MaxChildren`. May be empty.
	  * @param suffixes A pseudo, non balanced node, containing `N-2` children, where `N` is the level of the tree.
	  *                 It forms the 'rightmost path' from the root to the last leaf `suffix`.
	  *                 The first child is of level `N-1`, and each next its sibling is a node is of a level one lower
	  *                 than the previous one, ending with level `2`. All children have rank
	  *                 `Rank - 1 <= r <= MaxChildren - 1`, but all grandchildren and further descendants of this node
	  *                 are of rank `Rank <= r <= MaxChildren`.
	  * @param suffix   A level 1 node of rank `Rank <= r <= MaxChildren`,
	  *                 the rightmost leaf in the represented tree, with the last elements of the sequence.
	  */
	@inline private def FingersN[E](prefix :Tree[E], prefixes :Tree[E], infix :Tree[E],
	                                suffixes :Tree[E], suffix :Tree[E]) :FingersN[E] =
	{
		val prefixLen = prefix.rank + prefixes.length
		val infixLen  = prefixLen + infix.length
		val res = new FingersN(
			prefix, prefixes, prefixLen, infix, infixLen, suffixes, suffix, infixLen + suffixes.length + suffix.length
		)
		res
	}
	@inline private def FingersN[E](prefix :Tree[E], prefixes :Tree[E], infix :Tree[E],
	                            suffixes :Tree[E], suffix :Tree[E], length :Int) :FingersN[E] =
	{
		val prefixLen = prefix.rank + prefixes.length
		val infixLen  = prefixLen + infix.length
		val res = new FingersN(prefix, prefixes, prefixLen, infix, infixLen, suffixes, suffix, length)
		res
	}


	/** A finger tree of length `N > 2` isomorphic with a B-Tree.
	  * @param _prefix       A level 1 node of rank `Rank <= r <= MaxChildren`,
	  *                      the leftmost node in the represented B-Tree, with the first elements of the sequence.
	  * @param prefixes      A pseudo, non balanced node, containing `N-2` children, where `N`
	  *                      is the level of the tree. It forms the 'leftmost path' from the root to node `prefix`.
	  *                      The first child is of level 2, and each next its sibling is a node is of a level
	  *                      one higher than the previous one, ending with level `N-1`. All children have rank
	  *                      `Rank - 1 <= r <= MaxChildren - 1`, but all grandchildren
	  *                      and further descendants of this node are of rank `Rank <= r <= MaxChildren`.
	  * @param infixOffset   `prefix.length + prefixes.foldLeft(0)(_ + _.length)`.
	  * @param infix         a node of level `N` - the level of the tree - containing `0 <= r <= MaxChildren - 2`
	  *                      inner children of the root of the represented B-Tree.
	  *                      All its descendants of level `N-1` and lower have rank `Rank <= r <= MaxChildren`.
	  * @param suffixOffset  `totalPrefixLength` plus the length of `infix`.
	  * @param suffixes      A pseudo, non balanced node, containing `N-2` children, where `N`
	  *                      is the level of the tree. It forms the 'rightmost path' from the root to node `suffix`.
	  *                      The first child is of level `N-1`, and each next its sibling is a node is of a level
	  *                      one lower than the previous one, ending with level `2`. All children have rank
	  *                      `Rank - 1 <= r <= MaxChildren - 1`, but all grandchildren
	  *                      and further descendants of this node are of rank `Rank <= r <= MaxChildren`.
	  * @param suffix        A level 1 node of rank `Rank <= r <= MaxChildren`,
	  *                      the rightmost node in the represented B-Tree, with the last elements of the sequence.
	  * @param len           The total length of this sequence.
	  */
	@SerialVersionUID(Ver)
	private final class FingersN[+E](_prefix :Tree[E], prefixes :Tree[E], infixOffset :Int, infix :Tree[E],
	                                 suffixOffset :Int, suffixes :Tree[E], suffix :Tree[E], len :Int)
		extends AbstractFingers[E](_prefix, len)
	{
		//assert(prefix.length + prefixes.length == infixOffset)
		//assert(prefix.length + prefixes.length + infix.length == suffixOffset)
		//assert(prefix.length + prefixes.length + infix.length + suffixes.length + suffix.length == length)
		//assert(prefix.rank >= Rank & prefix.rank <= MaxChildren, "prefix.rank out of range: " + prefix.rank)
		//assert(prefix.level == 1, "prefix.level == " + prefix.level + "; prefix ==" + prefix.children.toString)
		//assert(suffix.rank >= Rank & suffix.rank <= MaxChildren, "suffix.rank out of range: " + suffix.rank)
		//assert(suffix.level == 1, "suffix.level == " + suffix.level + "; suffix ==" + prefix.children.toString)
		//assert(prefixes.rank >= 1, "prefixes.rank is zero")
		//assert(suffixes.rank >= 1, "suffixes.rank is zero")
		//assert(prefixes.rank == suffixes.rank, "prefix.rank: " + prefix.rank + "; suffix.rank: " + suffix.rank)
		//assert((0 until prefixes.rank).forall { i => val t = prefixes.child(i); t.level == i + 2 && t.isValid(1) })
		//assert((0 until suffixes.rank).forall { i => val t = suffixes.child(suffixes.rank - 1 - i); t.level == i + 2 && t.isValid(1) })
		//assert(infix.rank == 0 || infix.level == suffixes.child(0).level + 1)
		//assert(infix.rank <= MaxChildren - 2)
		//assert((0 until infix.rank).forall(infix.child(_).isValid))

		releaseFence()

		override def level :Int = prefixes.rank + 2
		def firstLeaf  :Tree[E] = prefix
		def secondTree :Tree[E] = prefixes
		def thirdTree  :Tree[E] = infix
		def fourthTree :Tree[E] = suffixes
		def lastLeaf   :Tree[E] = suffix
		def initLength :Int = infixOffset
		def tailLength :Int = len - suffixOffset
		@inline def suffixValues :Children[E] = new Children(suffix.asInstanceOf[Array[Any]])
		override def iterator :Iterator[E] = new IteratorN(this)

		override def toTree  :Tree[E] = {
			val prefixesRank = prefixes.rank
			val infixRank    = infix.array.length - 1
			@tailrec def prefixTree(prefix :Tree[E], lvl :Int) :Tree[E] = {
				val tree = Tree(prefixes.child(lvl).children.prepended(prefix))
				if (lvl == prefixesRank - 1) tree
				else prefixTree(tree, lvl + 1)
			}
			@tailrec def suffixTree(suffix :Tree[E], lvl :Int) :Tree[E] = {
				val tree = Tree(suffixes.child(lvl).children.appended(suffix))
				if (lvl == 0) tree
				else suffixTree(tree, lvl - 1)
			}
			val prefix = prefixTree(this.prefix, 0)
			val suffix = suffixTree(this.suffix, prefixesRank - 1)
			val array  = new Array[Any](infixRank + 3)
			array(1) = prefix
			arraycopy(infix.array, 1, array, 2, infixRank)
			array(infixRank + 2) = suffix
			val root = new Children[Tree[E]](array)
			updateLengths(root)
			Tree(root)
		}

		override def get(i :Int) :E =
			if (i < 0 | i >= length)
				outOfBounds_!(i, className + "|" + length + "|")
			else if (i < infixOffset)
				if (i < prefix.length) prefix(i)
				else prefixes(i - prefix.length)
			else if (i >= suffixOffset)
				if (i >= length - suffix.rank) suffix(i - (length - suffix.rank))
				else suffixes(i - suffixOffset)
			else
				infix(i - infixOffset)

		protected override def indexWhere(p :E => Boolean, from :Int, satisfies :Boolean) :Int = {
			val prefixesOffset = prefix.rank
			var i = from
			var res = -1
			if (from < prefixesOffset) {
				res = prefix.indexWhere(p, i, satisfies)
				if (res >= 0)
					return res
				i = prefixesOffset
			}
			if (from < infixOffset) {
				res = prefixes.indexWhere(p, i - prefixesOffset, satisfies)
				if (res >= 0)
					return res + prefixesOffset
				i = infixOffset
			}
			if (from < suffixOffset) {
				res = infix.indexWhere(p, i - infixOffset, satisfies)
				if (res >= 0)
					return res + infixOffset
				i = suffixOffset
			}
			val lastSliceOffset = length - suffix.rank
			if (from < lastSliceOffset) {
				res = suffixes.indexWhere(p, i - suffixOffset, satisfies)
				if (res >= 0)
					return res + suffixOffset
				i = lastSliceOffset
			}
			res = suffix.indexWhere(p, i - lastSliceOffset, satisfies)
			if (res >= 0) res + lastSliceOffset else -1
		}

		override def foldLeft[A](z :A)(op :(A, E) => A) :A =
			suffix.foldLeft(
				suffixes.foldLeft(
					infix.foldLeft(
						prefixes.foldLeft(
							prefix.foldLeft(z)(op)
						)(op)
					)(op)
				)(op)
			)(op)
		override def foldRight[A](z :A)(op :(E, A) => A) :A =
			prefix.foldRight(
				prefixes.foldRight(
					infix.foldRight(
						suffixes.foldRight(
							suffix.foldRight(z)(op)
						)(op)
					)(op)
				)(op)
			)(op)

		override def foreach[U](f :E => U) :Unit = {
			prefix.foreach(f)
			prefixes.foreach(f)
			infix.foreach(f)
			suffixes.foreach(f)
			suffix.foreach(f)
		}

		override def map[O](f :E => O) :Fingers[O] =
			new FingersN(
				prefix.map(f), prefixes.map(f), infixOffset, infix.map(f),
				suffixOffset, suffixes.map(f), suffix.map(f), length
			)

		override def updated[U >: E](i :Int, elem :U) :Fingers[U] =
			if (i < 0 | i > length)
				outOfBounds_!(i, errorString(this) + ".updated")
			else if (i < infixOffset)
				if (i < prefix.length)
					new FingersN(
						prefix.updated(i, elem), prefixes, infixOffset, infix,
						suffixOffset, suffixes, suffix, length
					)
				else
					new FingersN(
						prefix, prefixes.updated(i - prefix.length, elem), infixOffset, infix,
						suffixOffset, suffixes, suffix, length
					)
			else if (i >= suffixOffset)
				if (length - i <= suffix.length)
					new FingersN(
						prefix, prefixes, infixOffset, infix,
						suffixOffset, suffixes, suffix.updated(suffix.length - (length - i), elem), length
					)
				else
					new FingersN(
						prefix, prefixes, infixOffset, infix,
						suffixOffset, suffixes.updated(i - suffixOffset, elem), suffix, length
					)
			else
				new FingersN(
					prefix, prefixes, infixOffset, infix.updated(i - infixOffset, elem),
					suffixOffset, suffixes, suffix, length
				)

		override def removed(index :Int) :Fingers[E] =
			if (index < 0 || index >= length)
				outOfBounds_!(errorString(this) + ".removed(" + index + ")")
			else if (index < infixOffset)
				removedFromPrefix(index)
			else if (index >= suffixOffset)
				removedFromSuffix(index)
			else  //totalPrefixLength <= index < lengthAfterInfix
				removedFromInfix(index)

		private def removedFromPrefix(index :Int) :Fingers[E] = {
			val prefixRank = prefix.rank
			if (index < prefixRank & prefixRank > Rank)
				new FingersN(
					Tree1(prefix.values.removed(index)), prefixes, infixOffset - 1, infix,
					suffixOffset - 1, suffixes, suffix, length - 1
				)
			else {
				/** Rebalances the whole tree when removal of an element from either `this.prefix` or `this.prefixes`
				  * caused a cascade of deletions within following prefix slices of higher level,
				  * which could not have been contained by modifying `prefixes` alone.
				  * It assumes that both `newPrefixes` and `newPrefix` are valid trees (for `prefixes` and `prefix`,
				  * accordingly), but that the slice directly following `newPrefix`, and of the same level as it,
				  * was required to bring back the balance. This slice is either the first child of `this.infix`,
				  * or, if the latter is empty, the first child of `suffixes`. If the rank of said following slice
				  * is higher than `Rank` (for the infix child) or `Rank - 1` (for the suffix slice), then the last
				  * child of that slice should be removed, as it was appended to `newPrefixes(newPrefixes.length - 1)`.
				  * Otherwise, the whole slice must be removed from the tree completely, as it was merged
				  * with the last prefix slice after the removal.
				  * @param successor   the slice of the same level as the last prefix slice
				  *                    at `newPrefixes(newPrefixes.length - 1)`, which directly precedes the latter
				  *                    in this tree. It is either the last child of infix or `prefixes(0)`,
				  *                    if the former is empty.
				  * @param newPrefixes a balanced copy of `this.prefixes` after removing the element at `index`
				  *                    from this sequence. If the first element in the array is not null,
				  *                    it must be an already valid `lengths` array.
				  * @param newPrefix   a balanced copy of `this.prefix` after removing the element at `index`
				  *                    from this sequence (if `index >= prefix.rank`, it is simply `prefix`).
				  * @return A balanced finger tree with `newPrefixes` as `prefixes` and `newPrefix` as `prefix`.
				  *         If `this.infix` is not empty, then the tree has the same level as this one,
				  *         and the first child of `infix` either removed or its first element dropped, depending
				  *         on its rank. If `this.infix` is empty, then the new tree will either have the same level
				  *         and the first child of `suffixes` modified by dropping its first element, if the child's
				  *         rank is at least `Rank`, or the tree level is reduced,
				  *         and `newPrefixes(newPrefixes.length - 1)` (result of merging
				  *         `this.prefixes.child(prefixes.rank - 1)` with `this.suffixes.child(0)` and removing
				  *         the first child of the former) is used as the infix of the new tree.
				  */
				def rebalanceRoot(newPrefix :Tree[E], newPrefixes :Children[Tree[E]], successor :Children[Tree[E]]) = {
					val infixRank = infix.rank
					val succRank  = successor.length
					if (infixRank > 0) {                //The updated child is the top (last) prefixes slice.
						if (newPrefixes.array(0) == null)
							updateLengths(newPrefixes)
						val newInfix =
							if (succRank > Rank) {      //Top prefix slice stole the first grandson of infix.
								val newSibling = Tree(successor.slice(1, succRank))
								Tree(infix.children.updated(0, newSibling))
							} else                      //Top prefix slice merged with the first son of infix.
								Tree(infix.children.slice(1, infixRank))
						FingersN(newPrefix, Tree(newPrefixes), newInfix, suffixes, suffix, length - 1)
					} else {                            //The first slice of suffixes was used in rebalancing.
						val prefixesRank = newPrefixes.length
						if (succRank > Rank - 1) {      //Top prefix slice stole the first child from the top suffix slice.
							if (newPrefixes.array(0) == null)
								updateLengths(newPrefixes)
							val newSuffixes = Tree(suffixes.children.updated(0, Tree(successor.slice(1, succRank))))
							FingersN(newPrefix, Tree(newPrefixes), emptyInfix, newSuffixes, suffix, length - 1)
						} else if (prefixesRank > 1) {  //Top prefix slice merged with top suffix, becomes the new infix.
							newPrefixes.array(0) = null //May be set to newPrefix and throw an exception when sliced.
							val reducedPrefixes = newPrefixes.slice(0, prefixesRank - 1)
							val reducedSuffixes = suffixes.children.slice(1, prefixesRank)
							updateLengths(reducedPrefixes)
							FingersN(
								newPrefix, Tree(reducedPrefixes), newPrefixes(prefixesRank - 1),
								Tree(reducedSuffixes), suffix, length - 1
							)
						} else //Top suffix slice merged with top prefix, becomes the infix of a tree of a reduced level.
							Fingers2(newPrefix, newPrefixes(0), suffix, length - 1)
					}
				}
				/** Recursively rebalances a working copy `res` of `this.prefixes` after removal.
				  * Given `slice`, equal to `res(sliceIdx)`, it removes its first child, padding it, if necessary,
				  * by either merging it completely with the following slice of the same level
				  * (the first child of `res(index + 1)`), or prepending the first child of the latter to the former.
				  * The recursion stops either when the slice is of rank higher than `Rank - 1`
				  * (and needs no rebalancing with cousins from the following sibling), or when it reaches the last
				  * prefix slice at index `res.length - 1`. In the latter case, the last prefix slice
				  * is similarly padded or merged, using either the first child of `this.infix`,
				  * or the first child of `suffixes`, if the former is empty.
				  * @param sliceIdx The index of the child of `prefixes` whose first child was merged with the preceding
				  *                 sibling in `prefixes`, after removal of an element from it, or any of preceding
				  *                 suffix slices.
				  * @param slice    The current node in `res(sliceIdx)`, which needs its whole first child removed.
				  * @param res      A copy of `prefixes` with `this.prefix` in the first (`lengths`) slot in the array.
				  * @return A properly rebalanced tree.
				  */
				@tailrec
				def rebalancePrefixes(sliceIdx :Int, slice :Children[Tree[E]], res :Children[Tree[E]]) :Fingers[E] = {
					val rank  = slice.length
					val level = res.length
					if (rank > Rank - 1) {                //The first child of slice can be safely removed.
						res(sliceIdx) = Tree(slice.slice(1, rank))
						val newPrefix = res(-1)
						updateLengths(res)
						new FingersN(
							newPrefix, Tree(res), infixOffset - 1, infix, suffixOffset - 1, suffixes, suffix, length - 1
						)
					} else if (sliceIdx < level - 1) {    //Use the first child of the following slice to rebalance slice.
						val parent     = res(sliceIdx + 1).children
						val firstChild = parent(0)
						val firstRank  = firstChild.rank
						if (firstRank > Rank) {           //Rebalance slice by appending the first child of firstChild.
							val newSlice      = slice.removeAndAppend(0, firstChild.child(0))
							val newSibling    = Tree(firstChild.children.removed(0))
							val newParent     = parent.updated(0, newSibling)
							val newPrefix     = res(-1)
							res(sliceIdx)     = Tree(newSlice)
							res(sliceIdx + 1) = Tree(newParent)
							updateLengths(res)
							new FingersN(
								newPrefix, Tree(res), infixOffset - 1, infix,
								suffixOffset - 1, suffixes, suffix, length - 1
							)
						} else {                          //Merge slice with lastChild and continue the recursion.
							val newSlice = Children.copyOfRanges(
								slice, 1, rank, firstChild.children, 0, firstRank
							)
							res(sliceIdx) = Tree(newSlice)
							rebalancePrefixes(sliceIdx + 1, parent, res)
						}
					} else {                              //We are now rebalancing slice == suffixes(0).
						val newPrefix = res(-1)
						val infixRank = infix.rank
						val sibling =
							if (infixRank > 0) infix.child(0).children
							else suffixes.child(0).children
						val minSiblingRank = if (infixRank > 0) Rank else Rank - 1
						val siblingRank = sibling.length
						val newSlice =
							if (siblingRank > minSiblingRank)
								slice.removeAndAppend(0, sibling(0))
							else
								Children.copyOfRanges(slice, 1, rank, sibling, 0, siblingRank)
						res(level - 1) = Tree(newSlice)
						res.array(0) = null               //So we can use Children methods without causing an exception.
						rebalanceRoot(newPrefix, res, sibling)
					}
				}
				var newPrefixes  = prefixes.children
				val prefixesRank = newPrefixes.length
				val infixRank    = infix.rank
				val splitIndex =
					if (index < prefixRank) SplitIndex(-1, index)
					else prefixes.nextIndex(index - prefixRank)
				val childIdx = splitIndex.child
				val child    = if (index < prefixRank) prefix else newPrefixes(childIdx)
				val successor =                            //The slice of the same level as child, directly following it.
					if (childIdx < prefixesRank - 1)
						newPrefixes(childIdx + 1).child(0)
					else if (infixRank > 0)
						infix.child(0)
					else
						suffixes.child(0)
				val minSuccessorRank = if (childIdx < prefixesRank - 1 | infixRank > 0) Rank else Rank - 1
				val minRank  = if (childIdx == -1) Rank else Rank - 1
				val newChild = child.removed(splitIndex.relative, None, One(successor), minRank, minSuccessorRank)
				val balanced = newChild.array(0) == null
				val newPrefix =
					if (childIdx == -1) {
						newChild.array(0) = null           //Avoid triggering updateLengths when newPrefixes(-1) is shanghaied.
						newChild
					} else {
						updateLengths(newChild.children)
						newPrefixes = newPrefixes.copy
						newPrefixes(childIdx) = newChild
						prefix
					}
				if (balanced) {                            //There was no need to use successor's children to rebalance.
					if (childIdx >= 0)                     //Compute new lengths for prefixes after removal.
						updateLengths(newPrefixes)
					new FingersN(
						newPrefix, Tree(newPrefixes), infixOffset - 1, infix,
						suffixOffset - 1, suffixes, suffix, length - 1
					)
				} else {                                   //The successor node was used in rebalancing.
					val succ     = successor.children
					val succRank = succ.length
					if (childIdx < prefixesRank - 1) {     //Removing an element not from the last/top prefix slice.
						if (childIdx == -1)                //newPrefixes hasn't been defensively copied yet.
							newPrefixes = newPrefixes.copy
						newPrefixes(-1) = newPrefix        //So we may access it when childIdx == -1.
						val parent = newPrefixes(childIdx + 1).children
						if (succRank > minSuccessorRank) { //The first child of succ was moved to newChild, we are done.
							val newSibling = Tree(succ.slice(1, succRank))
							newPrefixes(childIdx + 1) = Tree(parent.updated(0, newSibling))
							updateLengths(newPrefixes)
							new FingersN(
								newPrefix, Tree(newPrefixes), infixOffset - 1, infix,
								suffixOffset - 1, suffixes, suffix, length - 1
							)
						} else                             //The following prefix slice's rank was reduced below the limit.
							rebalancePrefixes(childIdx + 1, parent, newPrefixes)
					} else {                               //Removed an element from the last/top prefix slice.
						newPrefixes.array(0) = null        //So lengths are recomputed if necessary by root.
						rebalanceRoot(newPrefix, newPrefixes, successor.children)
					}
				}
			}
		}


		private def removedFromSuffix(index :Int) :Fingers[E] = {
			val suffixRank = suffix.rank
			if (index >= length - suffixRank & suffixRank > Rank)
				new FingersN(
					prefix, prefixes, infixOffset, infix,
					suffixOffset, suffixes, Tree1(suffix.values.removed(index - (length - suffixRank))), length - 1
				)
			else {
				/** Rebalances the whole tree when removal of an element from either `this.suffix` or `this.suffixes`
				  * caused a cascade of deletions within earlier/of higher level suffix slices,
				  * which could not have been contained by modifying `suffixes` alone.
				  * It assumes that both `newSuffixes` and `newSuffix` are valid trees (for `suffixes` and `suffix`,
				  * accordingly), but that the slice directly preceding `newSuffix`, and of the same level as it,
				  * was required to bring back the balance. This slice is either the last child of `this.infix`,
				  * or, if the latter is empty, the last child of `prefixes`. If the rank of said preceding slice
				  * is higher than `Rank` (for the infix child) or `Rank - 1` (for the prefix slice), then the last
				  * child of that slice should be removed, as it was prepended to `newSuffixes(0)`.
				  * Otherwise, the whole slice must be removed from the tree completely, as it was merged
				  * with the first suffix slice after the removal.
				  * @param prev        the slice of the same level as the first suffix slice at `newSuffixes(0)`,
				  *                    which directly precedes the latter in this tree. It is either
				  *                    the last child of infix or `prefixes(0)`, if the former is empty.
				  * @param newSuffixes a balanced copy of `this.suffixes` after removing the element at `index`
				  *                    from this sequence. If the first element in the array is not null,
				  *                    it must be an already precomputed, valid `lengths` table.
				  * @param newSuffix   a balanced copy of `this.suffix` after removing the element at `index`
				  *                    from this sequence (if `index < length - suffix.rank`, it is simply `suffix`).
				  * @return A balanced finger tree with `newSuffixes` as `suffixes` and `newSuffix` as `suffix`.
				  *         If `this.infix` is not empty, then the tree has the same level as this one,
				  *         and the last child of `infix` either removed or its last element dropped, depending
				  *         on its rank. If `this.infix` is empty, then the new tree will either have the same level
				  *         and the last child of `prefixes` modified by dropping its last element, if the child's
				  *         rank is at least `Rank`, or the tree level is reduced, and `newSuffixes(0)`
				  *         (result of merging `this.prefixes.child(prefixes.rank - 1)` with `this.suffixes.child(0)`
				  *         and removing the last child of the latter) is used as the infix of the new tree.
				  */
				def rebalanceRoot(prev :Children[Tree[E]], newSuffixes :Children[Tree[E]], newSuffix :Tree[E]) :Fingers[E] = {
					val infixRank = infix.rank
					val prevRank  = prev.length
					if (infixRank > 0) {                //The updated child is the top (first) suffixes slice.
						if (newSuffixes.array(0) == null)
							updateLengths(newSuffixes)
						val newInfix =
							if (prevRank > Rank) {      //Top suffix slice stole the last grandson of infix.
								val newSibling = Tree(prev.slice(0, prevRank - 1))
								Tree(infix.children.updated(infixRank - 1, newSibling))
							} else                      //Top suffix slice merged with the last son of infix.
								Tree(infix.children.slice(0, infixRank - 1))
						FingersN(prefix, prefixes, newInfix, Tree(newSuffixes), newSuffix, length - 1)
					} else {                            //The last slice of prefixes was used in rebalancing.
						val suffixesRank = newSuffixes.length
						if (prevRank > Rank - 1) {      //Top suffix slice stole the last child from the top prefix slice.
							if (newSuffixes.array(0) == null)
								updateLengths(newSuffixes)
							val newPrefixes = Tree(
								prefixes.children.updated(suffixesRank - 1, Tree(prev.slice(0, prevRank - 1)))
							)
							FingersN(prefix, newPrefixes, emptyInfix, Tree(newSuffixes), newSuffix, length - 1)
						} else if (suffixesRank > 1) {  //Top suffix slice merged with top prefix, becomes the new infix.
							newSuffixes.array(0) = null //May be set to newSuffix and throw an exception when sliced.
							val reducedPrefixes = prefixes.children.slice(0, suffixesRank - 1)
							val reducedSuffixes = newSuffixes.slice(1, suffixesRank)
							updateLengths(reducedSuffixes)
							FingersN(
								prefix, Tree(reducedPrefixes), newSuffixes(0),
								Tree(reducedSuffixes), newSuffix, length - 1
							)
						} else //Top suffix slice merged with top prefix, becomes the infix of a tree of a reduced level.
							Fingers2(prefix, newSuffixes(0), newSuffix, length - 1)
					}
				}
				/** Recursively rebalances a working copy `res` of `this.suffixes` after removal.
				  * Given `slice`, equal to `res(sliceIdx)`, it removes its last child, padding it, if necessary,
				  * by either merging it completely with the preceding slice of the same level
				  * (the last child of `res(index - 1)`), or prepending the last child of the latter to the former.
				  * The recursion stops either when the slice is of rank higher than `Rank - 1`
				  * (and needs no rebalancing with cousins from the preceding sibling), or when it reaches the first
				  * suffix slice at index `0`. In the latter case, the first suffix slice is similarly padded or merged,
				  * using either the last child of `this.infix`, or the last child of `prefixes`, if the former is empty.
				  * @param sliceIdx The index of the child of `suffixes` whose last child was merged with the following
				  *                 sibling in `suffixes`, after removal of an element from it, or any of subsequent
				  *                 suffix slices.
				  * @param slice    The current node in `res(sliceIdx)`, which needs its whole last child removed.
				  * @param res      A copy of `suffixes` with `this.suffix` in the first (`lengths`) slot in the array.
				  * @return A properly rebalanced tree.
				  */
				@tailrec
				def rebalanceSuffixes(sliceIdx :Int, slice :Children[Tree[E]], res :Children[Tree[E]]) :Fingers[E] = {
					val rank = slice.length
					if (rank > Rank - 1) {                //The last child of slice can be safely removed.
						res(sliceIdx) = Tree(slice.slice(0, rank - 1))
						val newSuffix = res(-1)
						updateLengths(res)
						new FingersN(
							prefix, prefixes, infixOffset, infix, suffixOffset, Tree(res), newSuffix, length - 1
						)
					} else if (sliceIdx > 0) {            //Use the last child of the preceding slice to rebalance slice.
						val parent     = res(sliceIdx - 1).children
						val parentRank = parent.length
						val lastChild  = parent(parentRank - 1)
						val lastRank   = lastChild.rank
						if (lastRank > Rank) {            //Rebalance slice by prepending the last child of lastChild.
							val newSlice      = slice.removeAndPrepend(rank - 1, lastChild.child(lastRank - 1))
							val newSibling    = Tree(lastChild.children.removed(lastRank - 1))
							val newParent     = parent.updated(parentRank - 1, newSibling)
							val newSuffix     = res(-1)
							res(sliceIdx)     = Tree(newSlice)
							res(sliceIdx - 1) = Tree(newParent)
							updateLengths(res)
							new FingersN(
								prefix, prefixes, infixOffset, infix, suffixOffset, Tree(res), newSuffix, length - 1
							)
						} else {                          //Merge slice with lastChild and continue the recursion.
							val newSlice = Children.copyOfRanges(
								lastChild.children, 0, lastRank, slice, 0, rank - 1
							)
							res(sliceIdx) = Tree(newSlice)
							rebalanceSuffixes(sliceIdx - 1, parent, res)
						}
					} else {                              //We are now rebalancing slice == suffixes(0).
						val newSuffix = res(-1)
						val infixRank = infix.rank
						val sibling =
							if (infixRank > 0) infix.child(infixRank - 1).children
							else prefixes.child(prefixes.rank - 1).children
						val minSiblingRank = if (infixRank > 0) Rank else Rank - 1
						val siblingRank = sibling.length
						val newSlice =
							if (siblingRank > minSiblingRank)
								slice.removeAndPrepend(rank - 1, sibling(siblingRank - 1))
							else
								Children.copyOfRanges(sibling, 0, siblingRank, slice, 0, rank - 1)
						res(0)  = Tree(newSlice)
						res.array(0) = null         //So we can use Children methods without causing an exception.
						rebalanceRoot(sibling, res, newSuffix)
					}
				}
				var newSuffixes  = suffixes.children
				val suffixesRank = newSuffixes.length
				val infixRank    = infix.rank
				val splitIndex =
					if (index >= length - suffixRank) SplitIndex(suffixesRank, index - (length - suffixRank))
					else suffixes.nextIndex(index - suffixOffset)
				val childIdx = splitIndex.child
				val child    = if (childIdx == suffixesRank) suffix else newSuffixes(childIdx)
				val predecessor =                            //The slice of the same level as child, directly before it.
					if (childIdx > 0) {
						val parent = newSuffixes(childIdx - 1).children
						parent(parent.length - 1)
					} else if (infixRank > 0)
						infix.child(infixRank - 1)
					else
						prefixes.child(suffixesRank - 1)
				val minPredecessorRank = if (childIdx > 0 | infixRank > 0) Rank else Rank - 1
				val minRank  = if (childIdx == suffixesRank) Rank else Rank - 1
				val newChild = child.removed(splitIndex.relative, One(predecessor), None, minRank, minPredecessorRank)
				val balanced = newChild.array(0) == null
				val newSuffix =
					if (childIdx == suffixesRank) {
						newChild.array(0) = null             //Avoid triggering updateLengths.
						newChild
					} else {
						updateLengths(newChild.children)
						newSuffixes = newSuffixes.copy
						newSuffixes(childIdx) = newChild
						suffix
					}
				if (balanced) {                              //There was no need to use predecessor's children.
					if (childIdx < suffixesRank)
						updateLengths(newSuffixes)
					new FingersN(
						prefix, prefixes, infixOffset, infix,
						suffixOffset, Tree(newSuffixes), newSuffix, length - 1
					)
				} else {                                     //The predecessor node was used in rebalancing.
					val prev     = predecessor.children
					val prevRank = prev.length
					if (childIdx > 0) {                      //Removing an element not from the first/top suffix slice.
						val parent = newSuffixes(childIdx - 1).children
						if (childIdx == suffixesRank)        //newSuffixes haven't been defensively copied yet.
							newSuffixes = newSuffixes.copy
						newSuffixes(-1) = newSuffix          //So we may access it when childIdx == suffixesRank.
						if (prevRank > minPredecessorRank) { //The last child of prev was moved to newChild, we arr done.
							val newSibling = Tree(prev.slice(0, prevRank - 1))
							newSuffixes(childIdx - 1) = Tree(parent.updated(parent.length - 1, newSibling))
							updateLengths(newSuffixes)
							new FingersN(
								prefix, prefixes, infixOffset, infix,
								suffixOffset, Tree(newSuffixes), newSuffix, length - 1
							)
						} else                               //The preceding slice's rank was reduced below the limit.
							rebalanceSuffixes(childIdx - 1, parent, newSuffixes)
					} else {                                 //Removed an element from the first/top suffix slice.
						newSuffixes.array(0) = null          //So it is computed if necessary by rebalanceRoot.
						rebalanceRoot(predecessor.children, newSuffixes, newSuffix)
					}
				}
			}
		}

		private def removedFromInfix(index :Int) :Fingers[E] =
			infix.rank match { //infix.rank > 0 because infixOffset <= index < suffixOffset.
				case 1 =>
					//If the infix has only a single child, rebalancing after removal
					// may need to involve prefix or suffix fingers.
					val indexInInfix = index - infixOffset
					val tree = infix.child(0) //A Tree sibling of the highest prefix and suffix fingers.
					if (tree.rank > Rank) {   //The rank of the node can be reduced, no need for outside rebalancing.
						val newInfix = Tree(infix.children.updated(0, tree.removed(indexInInfix)))
						new FingersN(
							prefix, prefixes, infixOffset, newInfix,
							suffixOffset - 1, suffixes, suffix, length - 1
						)
					} else {
						val topPrefixIdx = prefixes.rank - 1
						val topPrefix    = prefixes.child(topPrefixIdx)
						val topSuffix    = suffixes.child(0)
						if (topSuffix.rank > Rank - 1) {
							//Allow moving of the first child of the first suffixes child to the relevant infix child,
							// to make up for a removed child, if necessary.
							val newTree  = tree.removed(indexInInfix, None, One(topSuffix), Rank, Rank - 1)
							val balanced = newTree.array(0) == null
							updateLengths(newTree.children)
							val newInfix = Tree(infix.children.updated(0, newTree))
							if (balanced)    //There was no need to involve the suffixes child.
								new FingersN(
									prefix, prefixes, infixOffset, newInfix,
									suffixOffset - 1, suffixes, suffix, length - 1
								)
							else {           //Rebalancing has used topSuffix.
								val newSuffixes = Tree(suffixes.children.updated(0, Tree(topSuffix.children.tail)))
								FingersN(prefix, prefixes, newInfix, newSuffixes, suffix, length - 1)
							}
						} else if (topPrefix.rank > Rank - 1) {
							//Allow moving of the last child of the last prefixes child to the relevant infix child,
							// to make up for a removed child, if necessary.
							val newTree  = tree.removed(indexInInfix, One(topPrefix), None, Rank, Rank - 1)
							val balanced = newTree.array(0) == null
							updateLengths(newTree.children)
							val newInfix = Tree(infix.children.updated(0, newTree))
							if (balanced)
								FingersN(prefix, prefixes, newInfix, suffixes, suffix, length - 1)
							else {
								val newPrefixes = Tree(
									prefixes.children.updated(topPrefixIdx, Tree(topPrefix.children.init))
								)
								FingersN(prefix, newPrefixes, newInfix, suffixes, suffix, length - 1)
							}
						} else { //tree.rank == Rank && topPrefix.rank == Rank - 1 && topSuffix.rank == Rank - 1
							//Decide rebalancing on the level of infix grandchildren.
							val splitIndex = tree.nextIndex(indexInInfix)
							val subtreeIdx = splitIndex.child
							val subtree    = tree.child(subtreeIdx)
							val newSubtree =
								//This specific choice between successor and predecessor comes into play later.
								if (subtreeIdx < (Rank >> 1)) {
									val next = tree.child(subtreeIdx + 1)
									subtree.removed(splitIndex.relative, None, One(next), Rank, Rank)
								} else {
									val prev = tree.child(subtreeIdx - 1)
									subtree.removed(splitIndex.relative, One(prev), None, Rank, Rank)
								}
							if (newSubtree.array(0) == null) { //Rebalancing was completely contained within subtree.
								if (!subtree.isLeaf)
									updateLengths(newSubtree.children)
								val newTree  = Tree(tree.children.updated(subtreeIdx, newSubtree))
								val newInfix = Tree(infix.children.updated(0, newTree))
								new FingersN(
									prefix, prefixes, infixOffset, newInfix,
									suffixOffset - 1, suffixes, suffix, length - 1
								)
							} else { //newSubtree either stole a child from its sibling, or merged with them completely.
								if (!subtree.isLeaf)
									updateLengths(newSubtree.children)
								else
									newSubtree.array(0) = null
								val siblingIdx = if (subtreeIdx < (Rank >> 1)) subtreeIdx + 1 else subtreeIdx - 1
								val sibling = tree.child(siblingIdx)
								if (sibling.rank > Rank) {     //Drop the stolen child from the sibling.
									val newTree = Tree(
										if (subtreeIdx < (Rank >> 1))
											tree.children.updated(subtreeIdx, newSubtree, Tree(sibling.children.tail))
										else
											tree.children.updated(siblingIdx, Tree(sibling.children.init), newSubtree)
									)
									val newInfix = Tree(Children(newTree))
									new FingersN(
										prefix, prefixes, infixOffset, newInfix,
										suffixOffset - 1, suffixes, suffix, length - 1
									)
								} else {
									//subtree and sibling merged into newSubtree, which leaves tree unbalanced.
									// Divide tree children between topPrefix and topSuffix.
									var newTopPrefix = new Children[Tree[E]](null)
									var newTopSuffix = new Children[Tree[E]](null)
									if (subtreeIdx < (Rank >> 1)) {           //The subtree will fall in newTopPrefix.
										newTopPrefix = Children.copyOfRanges( //Remove the sibling following subtree.
											topPrefix.array.asInstanceOf[RefArray[Tree[E]]], 1, Rank,
											tree.array.asInstanceOf[RefArray[Tree[E]]], 1, subtreeIdx + 2,
											tree.array.asInstanceOf[RefArray[Tree[E]]], subtreeIdx + 3, (Rank >> 1) + 2
										) //As long as Rank >= 4, newTopPrefix.rank < MaxChildren
										newTopPrefix(Rank + subtreeIdx - 1) = newSubtree
										updateLengths(newTopPrefix)

										newTopSuffix = Children.copyOfRanges(
											tree.children, (Rank >> 1) + 1, Rank, topSuffix.children, 0, Rank - 1
										)
									} else {                                  //The subtree will fall in newTopSuffix.
										newTopPrefix = Children.copyOfRanges(
											topPrefix.children, 0, Rank - 1, tree.children, 0, (Rank >> 1) - 1
										) //Make sure tree.child(subtreeIdx - 1) is within newTopSuffix.
										newTopSuffix = Children.copyOfRanges( //Remove the sibling preceding subtree.
											tree.array.asInstanceOf[RefArray[Tree[E]]], Rank >> 1, subtreeIdx,
											tree.array.asInstanceOf[RefArray[Tree[E]]], subtreeIdx + 1, Rank + 1,
											topSuffix.array.asInstanceOf[RefArray[Tree[E]]], 1, Rank
										) //Put the new subtree in place of its removed sibling.
										newTopSuffix(subtreeIdx - (Rank >> 1)) = newSubtree
										updateLengths(newTopSuffix)           //Only after inserting newSubtree.
									}
									val newPrefixes = Tree(prefixes.children.updated(topPrefixIdx, Tree(newTopPrefix)))
									val newSuffixes = Tree(suffixes.children.updated(0, Tree(newTopSuffix)))
									FingersN(prefix, newPrefixes, emptyInfix, newSuffixes, suffix, length - 1)
								}
							}
						}
					}
				case _ => //infix.rank greater than zero.
					val newInfix = infix.removed(index - infixOffset)
					new FingersN(
						prefix, prefixes, infixOffset, newInfix,
						suffixOffset - 1, suffixes, suffix, length - 1
					)
			}

		override def appended[U >: E](elem :U) :Fingers[U] = insertedIntoSuffix(length, elem)
		override def prepended[U >: E](elem :U) :Fingers[U] = insertedIntoPrefix(0, elem)

		override def inserted[U >: E](index :Int, elem :U) :Fingers[U] =
			if (index < infixOffset)
				insertedIntoPrefix(index, elem)
			else if (index >= suffixOffset)
				insertedIntoSuffix(index, elem)
			else
				insertedIntoInfix(index, elem)

		//If we had a type class U=>Children[U].insert, BTree[U]=>Children[U].insertedAll, we could roll this and
		// insertedAllIntoPrefix into one method. Not sure if it would reduce the amount of code, though.
		private def insertedIntoPrefix[U >: E](index :Int, elem :U) :Fingers[U] = {
			val prefixRank = prefix.rank
			if (index < prefixRank & prefixRank < MaxChildren) {
				val newPrefix = Tree1(prefixValues.inserted(index, elem))
				new FingersN(
					newPrefix, prefixes, infixOffset + 1, infix,
					suffixOffset + 1, suffixes, suffix, length + 1
				)
			} else {
				val newPrefixes = prefixes.children.copy :Children[Tree[U]]
				newPrefixes(-1) = prefix      //Avoid triggering updateLengths on Children.update.
				val splitIndex =              //Handle the cases of inserting into prefix and prefixes together.
					if (index < prefixRank) SplitIndex(-1, index)
					else prefixes.splitIndex(index - prefixRank)
				val childIdx  = splitIndex.child
				val child     = newPrefixes(childIdx)
				val rankDelta = if (childIdx == -1) 0 else 1
				val split     = child.inserted(splitIndex.relative, elem, Rank - rankDelta, MaxChildren - rankDelta)
				rebalancePrefixes(childIdx, split, newPrefixes, 1)
			}
		}

		private def insertedIntoSuffix[U >: E](index :Int, elem :U) :Fingers[U] = {
			val suffixRank = suffix.rank
			if (index >= length - suffixRank & suffixRank < MaxChildren) {
				val newSuffix = Tree1(suffix.values.inserted(index - (length - suffixRank), elem))
				new FingersN(
					prefix, prefixes, infixOffset, infix,
					suffixOffset, suffixes, newSuffix, length + 1
				)
			} else {
				val newSuffixes  = suffixes.children.copy :Children[Tree[U]]
				val suffixesRank = newSuffixes.length
				newSuffixes(-1)  = suffix
				val splitIndex =       //Handle the cases of inserting into prefix and prefixes together.
					if (index >= length - suffixRank) SplitIndex(suffixesRank, index - (length - suffixRank))
					else suffixes.splitIndex(index - suffixOffset)
				val childIdx = (splitIndex.child + 1) % (suffixesRank + 1) - 1 //splitIndex.child or -1
				val child    = newSuffixes(childIdx)
				val maxRank  = if (childIdx == -1) MaxChildren else MaxChildren - 1
				val split    = child.inserted(splitIndex.relative, elem, Rank, maxRank)
				rebalanceSuffixes(splitIndex.child, split, newSuffixes, 1)
			}
		}

		private def insertedIntoInfix[U >: E](index :Int, elem :U) :Fingers[U] = {
			val siblings = infix.inserted(index - infixOffset, elem, Rank - 1, MaxChildren - 2)
			if (siblings.size == 1)
				new FingersN(prefix, prefixes, infixOffset, siblings.first, suffixOffset + 1, suffixes, suffix, length + 1)
			else {
				val newPrefixes = Tree(prefixes.children.appended(siblings.first))
				val newSuffixes = Tree(suffixes.children.prepended(siblings.second))
				FingersN(prefix, newPrefixes, emptyInfix, newSuffixes, suffix, length + 1)
			}
		}


		override def insertedAll[U >: E](index :Int, elems :Children[U]) :AbstractFingers[U] =
			if (index < infixOffset)
				insertedAllIntoPrefix(index, elems)
			else if (index >= suffixOffset)
				insertedAllIntoSuffix(index, elems)
			else
				insertedAllIntoInfix(index, elems)

		private def insertedAllIntoPrefix[U >: E](index :Int, elems :Children[U]) :AbstractFingers[U] = {
			val prefixRank = prefix.rank
			val elemsSize  = elems.length
			if (index < prefixRank & prefixRank + elemsSize <= MaxChildren) {
				val newPrefix = Tree1(prefixValues.insertedAll(index, elems))
				new FingersN(
					newPrefix, prefixes, infixOffset + elemsSize, infix,
					suffixOffset + elemsSize, suffixes, suffix, length + elemsSize
				)
			} else {
				val newPrefixes = prefixes.children.copy :Children[Tree[U]]
				newPrefixes(-1) = prefix
				val splitIndex =
					if (index < prefixRank) SplitIndex(-1, index)
					else prefixes.splitIndex(index - prefixRank)
				val childIdx  = splitIndex.child
				val child     = newPrefixes(childIdx)
				val rankDelta = if (childIdx == -1) 0 else 1
				val split     = child.insertedAll(splitIndex.relative, elems, Rank - rankDelta, MaxChildren - rankDelta)
				rebalancePrefixes(childIdx, split, newPrefixes, elemsSize)
			}
		}

		private def insertedAllIntoSuffix[U >: E](index :Int, elems :Children[U]) :AbstractFingers[U] = {
			val suffixRank = suffix.rank
			val elemsSize  = elems.length
			if (index >= length - suffixRank & suffixRank + elemsSize <= MaxChildren) {
				val newSuffix = Tree1(suffix.values.insertedAll(index - length + suffixRank, elems))
				new FingersN(
					prefix, prefixes, infixOffset, infix,
					suffixOffset, suffixes, newSuffix, length + elemsSize
				)
			} else {
				val newSuffixes  = suffixes.children.copy :Children[Tree[U]]
				val suffixesRank = newSuffixes.length
				newSuffixes(-1)  = suffix
				val splitIndex   =
					if (index >= length - suffixRank) SplitIndex(suffixesRank, suffixRank)
					else suffixes.splitIndex(index - suffixOffset)
				val childIdx  = (splitIndex.child + 1) % (suffixesRank + 1) - 1 //splitIndex.child or -1
				val child     = newSuffixes(childIdx)
				val rankDelta = if (childIdx == -1) 0 else 1
				val split     = child.insertedAll(splitIndex.relative, elems, Rank - rankDelta, MaxChildren - rankDelta)
				rebalanceSuffixes(splitIndex.child, split, newSuffixes, elemsSize)
			}
		}

		private def insertedAllIntoInfix[U >: E](index :Int, elems :Children[U]) :AbstractFingers[U] = {
			val extra = elems.length
			val siblings = infix.insertedAll(index, elems, Rank - 1, MaxChildren - 2)
			if (siblings.size == 1)
				new FingersN(
					prefix, prefixes, infixOffset, siblings.first,
					suffixOffset + extra, suffixes, suffix, length + extra
				)
			else { //newInfix.rank == 2, because its level grew.
				val newPrefixes = Tree(prefixes.children.appended(siblings.first))
				val newSuffixes = Tree(suffixes.children.prepended(siblings.second))
				FingersN(prefix, newPrefixes, emptyInfix, newSuffixes, suffix, length + extra)
			}
		}

		/** Rebalances the tree after inserting element or elements into either `prefix` or `prefixes`.
		  * @param child       the index of the child of `newPrefixes` into which the values were inserted,
		  *                    with `-1` used to denote insertion into `suffix`.
		  * @param split       the replacement tree, or two trees, for `newPrefixes(child)`
		  * @param newPrefixes a copy of `this.prefixes`, additionally with `this.prefix` at slot `0` (for `lengths`).
		  * @param delta       the total number of inserted elements.
		  */
		private def rebalancePrefixes[U >: E](child :Int, split :Siblings[U], newPrefixes :Children[Tree[U]], delta :Int)
		       :AbstractFingers[U] =
		{
			val prefixesRank = newPrefixes.length
			newPrefixes(child) = split.first
			val newPrefix  = newPrefixes(-1) //apply doesn't validate the index.
			val rebalanced =
				split.size == 1 ||
					child < prefixesRank - 1 && prependedToPrefixSlice(split.second, child + 1, newPrefixes)
			if (rebalanced) {                //Balancing limited to prefixes.
				updateLengths(newPrefixes)
				new FingersN(
					newPrefix, Tree(newPrefixes), infixOffset + delta, infix,
					suffixOffset + delta, suffixes, suffix, length + delta
				)
			} else {                         //We are left with a single node of the same level as the top prefix slice.
				val surplus = if (child == prefixesRank - 1) split.second else newPrefixes(-1)
				val infixRank = infix.rank
				if (infixRank == MaxInfixRank) {
					val nextPrefix  = Tree(infix.children.sliceAndInsert(0, Rank - 2, 0, surplus))
					val nextSuffix  = Tree(infix.children.slice(Rank - 2, MaxInfixRank))
					newPrefixes.array(0) = null
					val extendedSuffixes = Tree(suffixes.children.inserted(0, nextSuffix))
					val extendedPrefixes = Tree(newPrefixes.inserted(prefixesRank, nextPrefix))
					updateLengths(extendedPrefixes.children)
					FingersN(newPrefix, extendedPrefixes, emptyInfix, extendedSuffixes, suffix, length + delta)
				} else {
					val newInfix = Tree(infix.children.inserted(0, surplus))
					updateLengths(newPrefixes)
					FingersN(newPrefix, Tree(newPrefixes), newInfix, suffixes, suffix, length + delta)
				}
			}
		}
		/** Adds `elems` as the first child of `res(level)`, after inserting an element or elements
		  * into `prefix` or `prefixes`.
		  * @param elems a spillover child resulting from splitting `res(level - 1)` into two after insertion.
		  * @param level the index in `res` of a node one level higher than `elems`
		  * @param res   a working copy of `this.prefixes` for rebalancing.
		  * @return `true` if `res` is balanced when the method returns, or `false` otherwise. In the latter case
		  *         `res(-1)` contains the spillover from splitting `res(res.rank-1)` into two nodes,
		  *         which must be integrated with `infix` or `suffixes`.
		  *         In either case, `lengths` are not computed for `res`.
		  */
		@tailrec
		private def prependedToPrefixSlice[U >: E](elems :Tree[U], level :Int, res :Children[Tree[U]]) :Boolean = {
			val parent = res(level).children
			if (parent.length < MaxChildren - 1) {
				res(level) = Tree(parent.prepended(elems))
				true
			} else {
				val first  = Tree(parent.sliceAndInsert(0, Rank - 2, 0, elems))
				val second = Tree(parent.slice(Rank - 2, MaxChildren - 1))
				res(level) = first
				if (level < res.length - 1)
					prependedToPrefixSlice(second, level + 1, res)
				else {
					res(-1) = second
					false
				}
			}
		}

		/** Rebalances a copy of `this.suffixes` - given as `newSuffixes` - after inserting a new element or elements.
		  * The current `suffix` of this tree is passed underhandedly at `newSuffixes(-1)`. This is so introduce
		  * a continuity of `suffixes` and `suffix` in module arithmetics: `child` equal to `newSuffixes.rank`
		  * is used to denote insertion into `suffix`, and wraps back to the beginning of the array.
		  * @param child       the index of the child of `newSuffixes` into which the values were inserted.
		  * @param split       the replacement tree, or two trees, for `newSuffixes(child)`.
		  * @param newSuffixes a copy of `this.suffixes`, additionally with `this.suffix` at slot `0` (for `lengths`).
		  * @param delta       the total number of inserted elements.
		  */
		private def rebalanceSuffixes[U >: E](child :Int, split :Siblings[U], newSuffixes :Children[Tree[U]], delta :Int)
				:AbstractFingers[U] =
		{
			val splitSize = split.size
			val first     = split.first
			val index     = (child + 1) % newSuffixes.array.length //Maps newSuffixes.rank to 0, and adds 1 to other indices.
			newSuffixes(index - 1) = if (splitSize == 1) first else split.second
			val newSuffix  = newSuffixes(-1)                       //apply doesn't validate the index.
			val rebalanced = splitSize == 1 || child > 0 && appendedToSuffixSlice(first, child - 1, newSuffixes)
			if (rebalanced) {                                      //Balancing limited to suffixes.
				updateLengths(newSuffixes)
				new FingersN(
					prefix, prefixes, infixOffset, infix,
					suffixOffset, Tree(newSuffixes), newSuffix, length + delta
				)
			} else { //We are left with a single node of the same level as the top suffix slice.
				val surplus   = if (child == 0) first else newSuffixes(-1)
				val infixRank = infix.rank
				if (infixRank == MaxInfixRank) {
					val topPrefix  = Tree(infix.children.slice(0, Rank - 1))
					val topSuffix  = Tree(
						infix.children.sliceAndInsert(Rank - 1, MaxInfixRank, Rank - 2, surplus)
					)
					newSuffixes.array(0) = null
					val extendedPrefixes = prefixes.children.appended(topPrefix)
					val extendedSuffixes = newSuffixes.prepended(topSuffix)
					updateLengths(extendedSuffixes)
					FingersN(
						prefix, Tree(extendedPrefixes), emptyInfix, Tree(extendedSuffixes), newSuffix, length + delta
					)
				} else {
					val newInfix = Tree(infix.children.appended(surplus))
					updateLengths(newSuffixes)
					FingersN(prefix, prefixes, newInfix, Tree(newSuffixes), newSuffix, length + delta)
				}
			}
		}
		/** Adds `surplus` as the last child of `res(level)`, recursively rebalancing it if necessary.
		  * @param surplus the first half of a node split after insertion into either `suffixes(level)`
		  *                (if `level < res.rank`) or `suffix` (if `level==res.rank`).
		  * @param index   An index in `res` of a suffix slice of one higher level than `surplus`,
		  *                with `res.rank` used to denote `suffixes`.
		  * @param res     a working copy of `this.suffixes`, additionally with `this.suffix` passed in the first
		  *                array slot (`res(-1)`), which allows to treat `suffixes` and `suffix` as a single sequence
		  *                using circular rotation of `res`.
		  * @return `true` if `res` is in a valid state for `suffixes` of a finger tree of this level,
		  *         or `false` otherwise. In the latter case, `res(-1)` is set to a first half of splitting `res(0)`.
		  *         In either case, `lengths` are not computed for `res`.
		  */
		@tailrec
		private def appendedToSuffixSlice[U >: E](surplus :Tree[U], index :Int, res :Children[Tree[U]]) :Boolean = {
			val parent = res(index).children
			if (parent.length < MaxChildren - 1) {
				res(index) = Tree(parent.appended(surplus))
				true
			} else {
				val first  = Tree(parent.slice(0, Rank))
				val second = Tree(parent.sliceAndInsert(Rank, MaxChildren - 1, Rank - 2, surplus))
				res(index) = second
				if (index > 0)
					appendedToSuffixSlice(first, index - 1, res)
				else {
					res(-1) = first
					false
				}
			}
		}

		protected override def clippedSlice(from :Int, until :Int) :Fingers[E] = {
			//Consider: if a mutable implementation with a builder wouldn't be faster.
			val prefixRank = prefix.rank
			val suffixRank = suffix.rank
			var res = infix.slice(from - infixOffset, until - infixOffset)
			if (from < infixOffset) {
				if (until <= prefixRank)
					res = prefix.slice(from, until)
				else {
					var fromPrefix = 0
					var fromOffset = 0
					if (from > prefixRank) {
						val idx = prefixes.nextIndex(from - prefixRank)
						fromPrefix = idx.child
						fromOffset = idx.relative
					}
					var untilPrefix = prefixes.rank
					var untilOffset = 0
					if (until < infixOffset) {
						val idx = prefixes.nextIndex(until - prefixRank)
						untilPrefix = idx.child
						untilOffset = idx.relative
					}
					val arr = prefixes.asInstanceOf[Array[Tree[E]]]

					var prefixSlice = Tree1(prefixValues.slice(from, prefixRank))
					if (fromPrefix == untilPrefix)
						prefixSlice = arr(1 + fromPrefix).slice(fromOffset, untilOffset).insertedAll(0, prefixSlice)
					else {
						if (fromOffset > 0) {
							fromPrefix += 1
							prefixSlice = arr(fromPrefix).slice(fromOffset, Int.MaxValue)
						}
						fromPrefix += 1
						while (fromPrefix <= untilPrefix) {
							prefixSlice = arr(fromPrefix).insertedAll(0, prefixSlice)
							fromPrefix += 1
						}
						if (untilOffset > 0)
							prefixSlice = arr(fromPrefix).slice(0, untilOffset).insertedAll(0, prefixSlice)
					}
					res = res.insertedAll(0, prefixSlice)
				}
			}
			if (until > suffixOffset) {
				val suffixesEnd = length - suffixRank
				var fromSuffix  = 0
				var fromOffset  = 0
				if (from > suffixOffset) {
					val idx = suffixes.nextIndex(from - suffixOffset)
					fromSuffix = idx.child
					fromOffset = idx.relative
				}
				var untilSuffix = suffixes.rank
				var untilOffset = 0
				if (until < suffixesEnd) {
					val idx = suffixes.nextIndex(until - suffixOffset)
					untilSuffix = idx.child
					untilOffset = idx.relative
				}
				//This code could be shared with the mirroring one for prefixes, but it's more efficient
				// to concatenate trees in the increasing level order.
				val arr = suffixes.asInstanceOf[Array[Tree[E]]]
				var suffixSlice =
					if (until > suffixesEnd)
						suffix.slice(from - suffixesEnd, until - suffixesEnd)
					else if (until == suffixesEnd)
						emptyLeaf
					else if (fromSuffix == untilSuffix)
						arr(1 + untilSuffix).slice(fromOffset, untilOffset)
					else
						arr(1 + untilSuffix).slice(0, untilOffset)
				if (fromSuffix < untilSuffix) {
					fromSuffix += 1
					while (untilSuffix > fromSuffix) {
						suffixSlice = suffixSlice.insertedAll(0, arr(untilSuffix)) //Array indexing is one off.
						untilSuffix -= 1
					}
					suffixSlice = suffixSlice.insertedAll(0, arr(untilSuffix).slice(fromOffset, Int.MaxValue))
				}
				res = res.insertedAll(res.length, suffixSlice)
			}
			res.toSeq
		}


		protected override def clippedCopyToArray[U >: E](xs :Array[U], start :Int, from :Int, len :Int) :Int = {
			var copied = 0
			if (from < infixOffset) {
				val prefixRank = prefixValues.length
				if (from < prefixRank)
					copied = prefix.copyToArray(xs, start, from, len)
				if (copied < len)
					copied += prefixes.copyToArray(xs, start + copied, from + copied - prefixRank, len - copied)
			}
			if (copied < len & from < suffixOffset)
				copied += infix.copyToArray(xs, start + copied, from + copied - infixOffset, len - copied)
			val lastSliceOffset = length - suffix.rank
			if (copied < len & from < lastSliceOffset)
				copied += suffixes.copyToArray(xs, start + copied, from + copied - suffixOffset, len - copied)
			if (copied < len)
				copied += suffix.copyToArray(xs, start + copied, from + copied - lastSliceOffset, len - copied)
			copied
		}

	}



	private[this] val emptyInfix :Tree[Nothing] = Array[Any](Array.empty[Int]).asInstanceOf[Tree[Nothing]]
	private[this] val emptyLeaf :Tree[Nothing] = Array[AnyRef](null).asInstanceOf[Tree[Nothing]]


	/** Classes used by the implementation. This object exists solely because an abstract type alias `Tree`
	  * causes 'abstract member may not have private modifier' compile error.
	  */
	private object internal {
		@inline def Tree[E](nodes :Children[Tree[E]]) :Tree[E] = nodes.array.asInstanceOf[Tree[E]]
		@inline def Tree1[E](values :Children[E]) :Tree[E] = values.array.asInstanceOf[Tree[E]]

		/** A node in a finger tree, consisting of a list of children, which are either values, or nodes of equal level.
		  * Implementation largely follows B-Trees, in that a node may be split into two if it grows past the maximum
		  * number of children, and a node falling below the minimum number can be joined with another node
		  * of a minimal rank. The difference from the most common B-Tree, however, is that values are only stored
		  * in leaves, and inner nodes store only references to nodes of lower level (together with indexing information).
		  *   - A node within `prefix` and `suffix` parts of any `Fingers` has rank `Rank <= rank <= MaxChildren == 2*Rank - 1`.
		  *   - A node contained directly by `FingersN.prefixes` and `FingersN.suffixes` has rank `Rank-1 <= rank <= 2*Rank - 2`.
		  *   - `FingersN.infix` has rank `0 <= rank <= 2*Rank - 3`.
		  *   - Children and descendant nodes of the latter two (true inner nodes) have rank `Rank <= rank <= 2*Rank - 1`.
		  *   - All extension methods, unless stated explicitly to the contrary,
		  *     maintain rank between `Rank` and `MaxChildren`, with the exception of the root node, which may have
		  *     as few as two children.
		  *
		  * Additionally, `FingersN.prefixes` and `suffixes`, formally simply sequences of subtrees
		  * consisting of nodes on paths to `prefix` (the leftmost leaf), and to `suffix` (the rightmost leaf),
		  * are also stored as `Tree` instances to leverage existing implementation, despite having each child
		  * of a different level.
		  *
		  * This is a type alias for `array :Array[Any]`, where `array(n + 1)` contains `n`-th child of this node
		  * (either a value of `E`, or another `Tree[E]`). For nodes of level higher than 1 (with other nodes
		  * as children), the first element in the array is an `Array[Int]` of length `rank` with the running lengths
		  * of prefixes of this node, when treated as a `Seq`, i.e., `children.scanLeft(0)(_ + _.length).tail.toArray`.
		  * A node of level 1 (directly containing values of `E`) always has `null` as the first array element.
		  * Occasionally, this slot is also used to smuggle another value out of a method,
		  * typically another tree - its sibling.
		  *
		  * This hack allows us to circumvent a need for creating an extra object containing the children list
		  * and running prefix lengths necessary for efficient random access, reducing the number of dereferenced
		  * objects when searching the tree, and the number of objects which must be created during mutations.
		  * @see [[net.noresttherein.sugar.collections.Fingers.internal.TreeExtension TreeExtension]] -
		  *      extension methods of this type.
		  */
		type Tree[+E]// >: E @uncheckedVariance

		/** An empty, non-leaf tree. The level is undefined, but it contains an (empty) `lengths` array
		  * in its sole slot. This will make any operations which produce another tree calculate and set `lengths`
		  * for the latter.
		  */
		private[this] val emptyTree :Tree[Nothing] = Array[Any](Array.empty[Int]).asInstanceOf[Tree[Nothing]]

		/** An empty leaf node (level 1 tree), a size 1 array with a `null` value. */
		private[this] val emptyLeaf :Tree[Nothing] = Array[AnyRef](null).asInstanceOf[Tree[Nothing]]

		@inline implicit def TreeExtension[E](node :Tree[E]) :TreeExtension[E] =
			new TreeExtension(node.asInstanceOf[Array[Any]])

		/** Extension methods constituting the interface
		  * of [[net.noresttherein.sugar.collections.Fingers.internal.Tree Tree]].
		  * This API, unlike [[net.noresttherein.sugar.collections.Fingers.internal.Children Children]],
		  * treats the tree as a sequence of values it contains, with the indices, unless specified differently,
		  * referring to individual elements, rather than children of this tree node.
		  */
		class TreeExtension[E](val array :Array[Any]) extends AnyVal {
			@inline def extension :TreeExtension[E] = this
			@inline def thisTree :Tree[E] = array.asInstanceOf[Tree[E]]

			def level :Int =
				if (array.length == 1) //The fishy case is emptyInfix, which has an (empty) lengths array.
					0
				else {
					var n :Array[Any] = array
					var res = 1
					while(n(0) != null) {
						res += 1
						n = n(1).asInstanceOf[Array[Any]]
					}
					res
				}
			@inline def isLeaf  :Boolean = array(0) == null
			@inline def rank    :Int = array.length - 1
			@inline def lengths :Array[Int] = array(0).asInstanceOf[Array[Int]]
			@inline def length  :Int =
				if (array.length == 1) 0
				else array(0) match {
					case lengths :Array[Int] => lengths(array.length - 2)
					case _ => array.length - 1
				}
			@inline def children :Children[Tree[E]] = new Children[Tree[E]](array)
			@inline def values :Children[E] = new Children[E](array)

			@inline def child(childIdx :Int) :Tree[E] = array(childIdx + 1).asInstanceOf[Tree[E]]

			@inline def isValid :Boolean = isValid(Rank, MaxChildren)
			@inline def isValid(rankDelta :Int) :Boolean = isValid(Rank - rankDelta, MaxChildren - rankDelta)
			def isValid(minRank :Int, maxRank :Int) :Boolean = {
				val lvl = level
				val r = rank
				r >= minRank && r <= maxRank && ((array(0) == null) || (0 until r).forall { i =>
					val tree = child(i)
					tree.level == lvl - 1 && tree.isValid
				})
			}

			/** Does a binary search in the `lengths` table and finds the child index within which index `idx` falls.
			  * The `relative` property of the returned object will equal the relative offset of `idx` within the child
			  * containing the value at that index.
			  * The `child` property of the returned object will equal the index in `lengths` of the first value strictly
			  * greater than `idx`. In particular, if `idx == this.lengths(i)` for some `i`, then `result.child == i + 1`
			  * and `result.relative == 0`. One exception to this rule is the case of `idx >= this.length`,
			  * in which case `SplitIndex(this.rank - 1, idx - this.lengths(this.rank - 2))` is returned.
			  */
			def splitIndex(idx :Int) :SplitIndex = {
				val lengths = array(0).asInstanceOf[Array[Int]]
				val rank    = array.length - 1
				val length  = lengths(rank - 1)
				if (idx >= length)
					SplitIndex(rank - 1, length - (if (rank == 1) 0 else lengths(rank - 2)))
				else {
					val childIdx = binSearch(lengths, idx)
					SplitIndex(childIdx, if (childIdx == 0) idx else idx - lengths(childIdx - 1))
				}
			}

			/** Same as `splitIndex`, but, if `idx == this.length`, returns `SplitIndex(this.length, 0)`,
			  * rather than `SplitIndex(this.length - 1, child(this.length - 1).length)`, like the latter.
			  */
			def nextIndex(idx :Int) :SplitIndex = {
				val lengths = array(0).asInstanceOf[Array[Int]]
				val child   = binSearch(lengths, idx)
				SplitIndex(child, if (child == 0) idx else idx - lengths(child - 1))
			}

			def apply(idx :Int) :E = {
				@tailrec def find(node :Tree[E], i :Int) :E = {
					val array = node.asInstanceOf[Array[AnyRef]]
					array(0) match {
						case lengths :Array[Int] =>
							val child = binSearch(lengths, i)
							val prefixLen = if (child == 0) 0 else lengths(child - 1)
							find(array(child + 1).asInstanceOf[Tree[E]], i - prefixLen)
						case _ => array(i + 1).asInstanceOf[E]
					}
				}
				find(array.asInstanceOf[Tree[E]], idx)
			}

			def updated[U >: E](idx :Int, value :U) :Tree[U] = array(0) match {
				case lengths :Array[Int] =>
					val childIdx = binSearch(lengths, idx)
					val relativeIdx = if (childIdx == 0) idx else idx - lengths(childIdx - 1)
					val newChild = array(childIdx + 1).asInstanceOf[Tree[E]].updated(relativeIdx, value)
					val res = Array.copyOf(array, array.length)
					res(childIdx + 1) = newChild
					res.asInstanceOf[Tree[U]]
				case _ =>
					val res = Array.copyOf(array, array.length)
					res(idx + 1) = value
					res.asInstanceOf[Tree[U]]
			}

			def overwritten[U >: E](idx :Int, values :Array[U], from :Int, until :Int) :Tree[U] = {
				val res = Array.copyOf(array, array.length)
				val rank = array.length - 1
				array(0) match {
					case lengths :Array[Int] =>
						var childIdx = binSearch(lengths, idx)
						var offset   = if (childIdx == 0) idx else idx - lengths(childIdx - 1)
						val end      = idx + math.min(lengths(rank - 1) - idx, until - from)
						var index    = idx
						while (index < end) {
							val child    = array(childIdx + 1).asInstanceOf[Tree[E]]
							val newChild = child.overwritten(offset, values, from + index - idx, until)
							res(childIdx + 1) = newChild
							index     = lengths(childIdx)
							offset    = 0
							childIdx += 1
						}
					case _ =>
						val max = math.min(rank - idx, until - from)
						ArrayLike.copy(values, from, res, idx + 1, max)
				}
				res.asInstanceOf[Tree[U]]
			}

			def overwritten[U >: E](idx :Int, values :Iterator[U]) :Tree[U] = {
				val res  = Array.copyOf(array, array.length)
				val rank = array.length - 1
				array(0) match {
					case lengths :Array[Int] =>
						var childIdx = binSearch(lengths, idx)
						var offset   = if (childIdx == 0) idx else idx - lengths(childIdx)
						while (values.hasNext && childIdx < rank) {
							val child    = array(childIdx + 1).asInstanceOf[Tree[E]]
							val newChild = child.overwritten(offset, values)
							res(childIdx + 1) = newChild
							offset    = 0
							childIdx += 1
						}
					case _ =>
						var i = idx + 1
						//Don't use copyToArray, because it leaves the iterator in undefined state.
						while (i <= rank && values.hasNext) {
							res(i) = values.next()
							i += 1
						}
				}
				res.asInstanceOf[Tree[U]]
			}

			/** Requires `Rank <= this.rank <= MaxChildren`. */
			def removed(idx :Int) :Tree[E] = {
				val res = removed(idx, None, None, Rank, Rank)
				if (array(0) != null)
					updateLengths(new Children[Tree[E]](res.array))
				else
					res.array(0) = null
				res
			}

			/** Returns a tree without `idx`-th element. Requires `Rank <= this.rank <= MaxChildren`.
			  * If a child must be deleted/two child merged into one, and this would bring the rank under `minRank`,
			  * it will append the first child of `successor` or prepend the last child of `predecessor`,
			  * if either is available, and their rank is greater than `minSiblingRank`. Otherwise,
			  * if either sibling exists (and has rank `minSiblingRank`), it will merge its children
			  * with those of the sibling into the returned tree. If any of the preceding operations are performed,
			  * the first element of the array will be non-null. Note that this means that `lengths` property
			  * is ''not'' set for the returned tree, and must be done so by the caller.
			  *
			  * If no sibling is given, the method may reduce the rank of this node by one.
			  */
			def removed(idx :Int, predecessor :Opt[Tree[E @uncheckedVariance]],
			            successor :Opt[Tree[E @uncheckedVariance]], minRank :Int, minSiblingRank :Int) :Tree[E] =
			{
				def padAndRemove(idx :Int) :Array[Any] =
					if (array.length <= minRank + 1 && (successor.isDefined || predecessor.isDefined))
						//Currently we are always giving only one of successor/predecessor. If we start giving a choice
						// between the two, we should prefer here the one with its rank > minSiblingRank + 1, if able.
						if (successor.isDefined) {
							val next = successor.get.array
							if (next.length > minSiblingRank + 1) { //Steal the first child of successor.
								val res = ErasedArray.copyOf(array, minRank + 1)
								arraycopy(array, idx + 2, res, idx + 1, minRank - idx - 1)
								res(0)       = next                 //Signal to the caller we've used successor.
								res(minRank) = next(1)
								res
							} else {                                //Merge this with successor.
								val res = ErasedArray.copyOf(array, minSiblingRank + minRank)
								arraycopy(array, idx + 2, res, idx + 1, minRank - idx - 1)
								arraycopy(next, 1, res, minRank, minSiblingRank)
								res(0) = next
								res
							}
						} else {                                    //predecessor.isDefined
							val prev = predecessor.get.array
							if (prev.length > minSiblingRank + 1) { //Steal the last child of predecessor.
								val res = ErasedArray.copyOf(array, minRank + 1)
								res(0) = prev                       //Signal to the caller we've used predecessor
								res(1) = prev(prev.length - 1)
								arraycopy(array, 1, res, 2, idx)
								res
							} else {                                //Merge this with predecessor.
								val res = ErasedArray.copyOf(prev, minSiblingRank + minRank)
								arraycopy(array, 1, res, 1 + minSiblingRank, idx)
								arraycopy(array, idx + 2, res, 1 + minSiblingRank + idx, minRank - 1 - idx)
								res(0) = prev
								res
							}
						}
					else {
						val res = ErasedArray.copyOfRanges(array, 0, 1 + idx, array, idx + 2, array.length)
						res(0) = null
						res
					}

				if (array(0) == null)
					padAndRemove(idx).asInstanceOf[Tree[E]]
				else {
					val thisRank = this.rank
					val index    = nextIndex(idx)
					val childIdx = index.child
					val child    = array(1 + childIdx).asInstanceOf[Tree[E]]
					val prev     = if (childIdx == 0) None else One(this.child(childIdx - 1))
					val next     = if (childIdx == thisRank - 1) None else One(this.child(childIdx + 1))
					val donorIdx    =
						if (childIdx == 0) childIdx + 1
						else if (childIdx == thisRank - 1) thisRank - 2
						else if (next.get.rank <= Rank) childIdx - 1
						else childIdx + 1
					val newChild =
						if (donorIdx == childIdx - 1) child.removed(index.relative, prev, None, Rank, Rank)
						else                          child.removed(index.relative, None, next, Rank, Rank)
					if (newChild.array(0) == null) { //We have not used prev or next to create newChild.
						if (child.array(0) != null)
							updateLengths(newChild.children)
						val res = array.updated(childIdx + 1, newChild)
						res(0) = null                //Inform the caller we have used neither prefix nor suffix.
						res.asInstanceOf[Tree[E]]
					} else {                         //Either preceding or following sibling was involved in rebalancing.
						if (child.array(0) != null)
							updateLengths(newChild.children)
						else
							newChild.array(0) = null
						val donorRank = if (donorIdx == childIdx - 1) prev.get.rank else next.get.rank
						if (donorRank > Rank) {
							val res = ErasedArray.copyOf(array, thisRank + 1)
							res(1 + donorIdx) =
								if (donorIdx == childIdx + 1) Tree(next.get.children.slice(1, donorRank))
								else                          Tree(prev.get.children.slice(0, donorRank - 1))
							res(childIdx + 1) = newChild
							res(0) = null
							res.asInstanceOf[Tree[E]]
						} else {
							val res = padAndRemove(donorIdx)
							val thisLevelShift =                      //Position child shifted due to using predecessor.
								if (res(0).asInstanceOf[AnyRef] eq predecessor)
									if (res.length == array.length) 1 //We've prepended a child of predecessor.
									else minSiblingRank               //We've prepended the whole predecessor.
								else 0                                //Appending or removing doesn't shift the position.
							val lowerLevelShift = math.max(0, donorIdx - childIdx)
							val newChildIdx  = childIdx + thisLevelShift + lowerLevelShift
							res(newChildIdx) = newChild
							res.asInstanceOf[Tree[E]]
						}
					}
				}
			}

			@inline def inserted[U >: E](idx :Int, elem :U) :Siblings[U] = inserted(idx, elem, Rank, MaxChildren)

			/** Inserts an element into the tree, possibly splitting this instance into two nodes,
			  * in order to balance the trees. The returned object is single-use and its state changes upon
			  * calling `first` and `second`.
			  */
			def inserted[U >: E](idx :Int, elem :U, minRank :Int, maxRank :Int) :Siblings[U] =
				if (array(0) == null)
					if (array.length < maxRank + 1)
						Siblings(Tree1(values.inserted(idx, elem)))
					else
						values.insertAndSplit(idx, elem, minRank)
				else {
					val index    = splitIndex(idx)
					val childIdx = index.child
					val child    = array(1 + childIdx).asInstanceOf[Tree[E]]
					val siblings = child.inserted(index.relative, elem)
					replace(childIdx, siblings, minRank, maxRank)
				}

			@inline def insertedSlice[U >: E](idx :Int, elems :Children[U], from :Int, until :Int) :Siblings[U] =
				insertedSlice(idx, elems, from, until, Rank, MaxChildren)

			def insertedSlice[U >: E](idx :Int, elems :Children[U], from :Int, until :Int, minRank :Int, maxRank :Int)
			       :Siblings[U] =
				if (until <= from)
					Siblings(array.asInstanceOf[Tree[E]])
				else if (array(0) == null) {
					val elemsLength = until - from
					val totalLength = array.length - 1 + elemsLength
					if (totalLength <= maxRank)
						Siblings(Tree1(values.insertedSlice(idx, elems, from, until)))
					else {
						val prefixLength = math.max(minRank, totalLength >> 1)
						values.insertSliceAndSplit(idx, elems, from, until, prefixLength)
					}
				} else {
					val index    = splitIndex(idx)
					val childIdx = index.child
					val child    = array(1 + childIdx).asInstanceOf[Tree[E]]
					val siblings = child.insertedSlice(index.relative, elems, from, until)
					replace(childIdx, siblings, minRank, maxRank)
				}

			@inline def insertedAll[U >: E](idx :Int, elems :Children[U]) :Siblings[U] =
				insertedAll(idx, elems, Rank, MaxChildren)

			@inline def insertedAll[U >: E](idx :Int, elems :Children[U], minRank :Int, maxRank :Int) :Siblings[U] =
				insertedSlice(idx, elems, 0, elems.length, minRank, maxRank)

			def insertedAll[U >: E](idx :Int, elems :Tree[U]) :Tree[U] =
				if (elems.array.length == 1)
					array.asInstanceOf[Tree[E]]
				else if (array.length == 1)
					elems
				else
					insertedAll(idx, elems, level, elems.level)

			private def insertedAll[U >: E](idx :Int, elems :Tree[U], thisLevel :Int, elemsLevel :Int) :Tree[U] = {
				def joinSameLevel2(first :Children[U], second :Children[U]) :Tree[U] = {
					val firstRank  = first.length
					val secondRank = second.length
					if (firstRank + secondRank <= MaxChildren)
						Tree1(second.insertedAll(0, first))
					else if (firstRank >= Rank & secondRank >= Rank)
						Tree(Children(Tree1(first), Tree1(second))) //Avoid erasing and recreating first.lengths
					else
						second.insertedAll(0, first, Rank, MaxChildren).toTree
				}
				def joinSameLevel3(first :Children[U], second :Children[U], third :Children[U]) :Tree[U] = {
					val rank1 = first.length
					val rank2 = second.length
					val rank3 = third.length
					val totalRank = rank1 + rank3 + rank2
					if (totalRank <= MaxChildren)                           //Combine the three nodes into a single one.
						Tree1(Children.copyOfRanges(first, 0, rank1, second, 0, rank2, third, 0, rank3))
					else if (rank1 >= Rank & rank2 >= Rank & rank3 >= Rank) //Join the three nodes as siblings.
						Tree(Children(Tree1(first), Tree1(second), Tree1(third)))
					else if (rank1 >= Rank) {                               //Merge or redistribute first and second.
						val siblings = second.insertedAll(rank2, third, Rank, MaxChildren)
						if (siblings.size == 1)
							Tree(Children(Tree1(first), siblings.first))
						else
							Tree(Children(Tree1(first), siblings.first, siblings.second))
					} else if (rank3 >= Rank) {                             //Merge o redistribute second and third.
						val siblings = second.insertedAll(0, first, Rank, MaxChildren)
						if (siblings.size == 1)
							Tree(Children(siblings.first, Tree1(third)))
						else
							Tree(Children(siblings.first, siblings.second, Tree1(third)))
					} else if (totalRank <= (MaxChildren << 1)) {           //Redistribute children into two nodes.
						val firstRank = totalRank >> 1
						val _1 = Children.copyOfRanges(first, 0, firstRank, second, 0, firstRank - rank1)
						val _2 = Children.copyOfRanges(second, firstRank - rank1, rank2, third, 0, rank3)
						Tree(Children(Tree1(_1), Tree1(_2)))
					} else {                                                //Redistribute children into three nodes.
						val _1 = Children.copyOfRanges(first, 0, rank1, second, 0, Rank - rank1)
						val _2 = second.slice(Rank - rank1, rank2 - (Rank - rank3))
						val _3 = Children.copyOfRanges(second, rank2 - (Rank - rank3), rank2, third, 0, rank3)
						Tree(Children(Tree1(_1), Tree1(_2), Tree1(_3)))
					}
				}
				if (elemsLevel < thisLevel)             //Insert elems under this.
					insertedAll(idx, elems, thisLevel, elemsLevel, true, true).toTree
				else if (thisLevel < elemsLevel) {      //Insert prefix and suffix of this split at idx under elems.
					if (idx == 0)                       //Append this to elems.
						elems.insertedAll(elems.length, array.asInstanceOf[Tree[E]], elemsLevel, thisLevel)
					else if (idx == length)             //Prepend this to elems.
						elems.insertedAll(0, array.asInstanceOf[Tree[E]], elemsLevel, thisLevel)
					else {                              //Insert suffix into elems, then prefix into the result.
						val prefix = slice(0, idx)
						val suffix = slice(idx, length)
						val suffixed = elems.insertedAll(elems.length, suffix, elemsLevel, suffix.level, true, true)
						val prefixed = suffixed.first.insertedAll(0, prefix, elemsLevel, prefix.level, true, true)
						if (suffixed.size == 1)
							prefixed.toTree
						else if (prefixed.size == 1)
							Tree(Children(prefixed.first, suffixed.second))
						else
							Tree(Children(prefixed.first, prefixed.second, suffixed.second))
					}
				} else if (idx == 0)              //Levels are equal, merge the two nodes by appending elems to this.
					values.insertedAll(0, elems.values, Rank, MaxChildren).toTree
				else if (idx == length)           //Levels are equal, merge the two nodes by prepending elems to this.
					values.insertedAll(rank, elems.values, Rank, MaxChildren).toTree
				else {                            //thisLevel == thatLevel
					val prefix   = slice(0, idx, thisLevel)
					val suffix   = slice(idx, length, thisLevel)
					val prefixLevel = prefix.level
					val suffixLevel = suffix.level
					if (prefixLevel == thisLevel & suffixLevel == thisLevel)
						//Redistribute children of prefix, elems, and suffix into whatever nodes are necessary.
						joinSameLevel3(prefix.values, elems.values, suffix.values)
					else if (prefixLevel == thisLevel) {
						val suffixed = elems.insertedAll(elems.length, suffix, elemsLevel, suffixLevel, true, true)
						val first    = suffixed.first
						if (suffixed.size == 1)
							joinSameLevel2(prefix.values, suffixed.first.values)
						else
							joinSameLevel3(prefix.values, first.values, suffixed.second.values)
					} else if (suffixLevel == thisLevel) {
						val prefixed = elems.insertedAll(0, prefix, elemsLevel, prefixLevel, true, true)
						val first    = prefixed.first
						if (prefixed.size == 1)
							joinSameLevel2(prefixed.first.values, suffix.values)
						else
							joinSameLevel3(first.values, prefixed.second.values, suffix.values)
					} else {
						val suffixed = elems.insertedAll(elems.length, suffix, elemsLevel, suffixLevel, true, true)
						val prefixed = suffixed.first.insertedAll(0, prefix, elemsLevel, prefixLevel, true, true)
						if (suffixed.size == 1)
							prefixed.toTree
						else if (prefixed.size == 1)
							Tree(Children(prefixed.first, suffixed.second))
						else
							Tree(Children(prefixed.first, prefixed.second, suffixed.second))
					}
				}
			}

			/** Inserts `elems` tree of level `elemsLevel` to this tree of level `thisLevel` at `idx`,
			  * minding not to temporarily modify either tree by putting it in `Siblings` in case it is shared
			  * with an immutable instance. If `thisShared`, subtrees below this tree will be defensively copied
			  * when returned as the second sibling in `Siblings`. The same applies for `elems` and `elemsShared`.
			  *
			  * Requires `thisLevel > elemsLevel`.
			  */ //todo: don't compute lengths, so we can put the result in siblings.
			private def insertedAll[U >: E](idx :Int, elems :Tree[U], thisLevel :Int, elemsLevel :Int,
			                                thisShared :Boolean, elemsShared :Boolean) :Siblings[U] =
				if (elemsLevel == 0)
					Siblings(array.asInstanceOf[Tree[E]])
				else {
					val index    = nextIndex(idx)
					if (thisLevel > elemsLevel + 1) {
						if (index.child == rank) {
							val child = this.child(rank - 1)
							val siblings = child.insertedAll(
								child.length, elems, thisLevel - 1, elemsLevel, thisShared, elemsShared
							)
							replace(rank - 1, siblings, Rank, MaxChildren)
						} else {
							val child    = this.child(index.child)
							val siblings = child.insertedAll(
								index.relative, elems, thisLevel - 1, elemsLevel, thisShared, elemsShared
							)
							replace(index.child, siblings, Rank, MaxChildren)
						}
					} else {
						val elemsRank = elems.rank
						val rank      = this.rank
						if (index.relative == 0) {
							if (elemsRank >= Rank)
								if (rank == MaxChildren)
									children.insertAndSplit(index.child, elems, Rank).castParam[U]
								else
									Siblings(Tree(children.inserted(index.child, elems)))
							else if (index.child == rank) {
								val predecessor = child(rank - 1)
								val siblings = predecessor.appendedAll(
									elems, thisLevel - 1, elemsLevel, thisShared, elemsShared
								)
								replace(rank - 1, siblings, Rank, MaxChildren)
							} else {
								val successor = child(index.child)
								val siblings  = successor.prependedAll(
									elems, thisLevel - 1, elemsLevel, thisShared, elemsShared
								)
								replace(index.child, siblings, Rank, MaxChildren)
							}
						} else {
							def replaceImbalanced2(childIdx :Int, first :Tree[U], second :Tree[U]) :Siblings[U] = {
								val firstRank  = first.rank
								val secondRank = second.rank
								val totalRank  = firstRank + secondRank
								if (firstRank >= Rank & secondRank >= Rank)
									replace(childIdx, first, second, Rank, MaxChildren)
								else if (totalRank <= MaxChildren) {
									val newChild = Children.copyOfRanges(
										first.children, 0, firstRank, second.children, 0, secondRank
									)
									Siblings(Tree(children.updated(childIdx, Tree(newChild))))
								} else {
									val siblings = second.values.insertAllAndSplit(0, first.values, totalRank >> 1)
									replace(childIdx, siblings.first, siblings.second, Rank, MaxChildren)
								}
							}
							def replaceImbalanced3(childIdx :Int, first :Tree[U], second :Tree[U], third :Tree[U])
								:Siblings[U] =
							{
								val firstRank  = first.rank
								val secondRank = second.rank
								val thirdRank  = third.rank
								val totalRank  = firstRank + thirdRank + secondRank
								if (totalRank <= MaxChildren) {
									val newChild = Children.copyOfRanges(
										first.children, 0, firstRank,
										second.children, 0, secondRank,
										third.children, 0, thirdRank
									)
									Siblings(Tree(children.updated(childIdx, Tree(newChild))))
								} else if (firstRank >= Rank & secondRank >= Rank & thirdRank >= Rank)
									replace(childIdx, first, second, third, Rank, MaxChildren)
								else if (firstRank >= Rank) {
									val suffixed = second.values.insertedAll(secondRank, third.values, Rank, MaxChildren)
									if (suffixed.size == 1)
										replace(childIdx, first, third, Rank, MaxChildren)
									else
										replace(childIdx, first, suffixed.first, suffixed.second, Rank, MaxChildren)
								} else if (thirdRank >= Rank) {
									val prefixed = second.values.insertedAll(0, first.values, Rank, MaxChildren)
									if (prefixed.size == 1)
										replace(childIdx, prefixed.first, third, Rank, MaxChildren)
									else
										replace(childIdx, prefixed.first, prefixed.second, third, Rank, MaxChildren)
								} else if (totalRank <= (MaxChildren << 1)) {
									val rank1 = totalRank >> 1
									val _1 = Children.copyOfRanges(
										first.values, 0, firstRank, second.values, 0, rank1 - firstRank
									)
									val _2 = Children.copyOfRanges(
										second.values, rank1 - firstRank, secondRank, third.values, 0, thirdRank
									)
									replace(childIdx, Tree1(_1), Tree1(_2), Rank, MaxChildren)
								} else {
									val _1 = Children.copyOfRanges(
										first.values, 0, Rank, second.values, 0, Rank - firstRank
									)
									val secondUntil = secondRank - (Rank - thirdRank)
									val _2 = second.values.slice(Rank - firstRank, secondUntil)
									val _3 = Children.copyOfRanges(
										second.values, secondUntil, secondRank, third.values, 0, thirdRank
									)
									replace(childIdx, Tree1(_1), Tree1(_2), Tree1(_3), Rank, MaxChildren)
								}
							}
							val childIdx     = index.child
							val indexInChild = index.relative
							val child        = this.child(childIdx)
							val prefix       = child.slice(0, indexInChild)
							val suffix       = child.slice(indexInChild, child.length)
							val prefixLevel  = prefix.level
							val suffixLevel  = suffix.level
							if (prefixLevel == elemsLevel & suffixLevel == elemsLevel)
								replaceImbalanced3(childIdx, prefix, elems, suffix)
							else if (prefixLevel == elemsLevel) { //suffixLevel < elemsLevel
								val siblings = elems.insertedAll(elems.length, suffix, elemsLevel, suffixLevel, true, true)
								if (siblings.size == 1)
									replaceImbalanced2(childIdx, prefix, siblings.first)
								else
									replaceImbalanced3(childIdx, prefix, siblings.first, siblings.second)
							} else if (suffixLevel == elemsLevel) {
								val siblings = elems.insertedAll(0, prefix, elemsLevel, prefixLevel, true, true)
								if (siblings.size == 1)
									replaceImbalanced2(childIdx, siblings.first, suffix)
								else
									replaceImbalanced3(childIdx, siblings.first, siblings.second, suffix)
							} else {
								val suffixed = elems.insertedAll(elems.length, suffix, elemsLevel, suffixLevel, true, true)
								val prefixed = suffixed.first.insertedAll(0, prefix, elemsLevel, prefixLevel, true, true)
								if (suffixed.size == 1)
									if (prefixed.size == 1)
										Siblings(Tree(children.updated(childIdx, prefixed.first)))
									else
										replace(childIdx, prefixed.first, prefixed.second, Rank, MaxChildren)
								else if (prefixed.size == 1)
									replace(childIdx, prefixed.first, suffixed.second, Rank, MaxChildren)
								else
									replace(childIdx, prefixed.first, prefixed.second, suffixed.second, Rank, MaxChildren)
							}
						}
					}
				}

			//Requires elemsLevel <= thisLevel
			def appendedAll[U >: E](elems :Tree[U], thisLevel :Int, elemsLevel :Int,
			                        thisShared :Boolean, elemsShared :Boolean) :Siblings[U] =
				if (elems.array.length == 1)
					Siblings(array.asInstanceOf[Tree[U]])
				else if (thisLevel > elemsLevel)
					insertedAll(length, elems, thisLevel, elemsLevel, thisShared, elemsShared)
				else
					values.insertedAll(rank, elems.values, Rank, MaxChildren)

			def prependedAll[U >: E](elems :Tree[U], thisLevel :Int, elemsLevel :Int,
			                         thisShared :Boolean, elemsShared :Boolean) :Siblings[U] =
				if (elems.array.length == 1)
					Siblings(array.asInstanceOf[Tree[U]])
				else if (thisLevel > elemsLevel)
					insertedAll(0, elems, thisLevel, elemsLevel, thisShared, elemsShared)
				else
					values.insertedAll(0, elems.values, Rank, MaxChildren)

			def replace[U >: E](index :Int, siblings :Siblings[U], minRank :Int, maxRank :Int) :Siblings[U] =
				if (siblings.size == 1)
					Siblings(new Children[Tree[U]](array).updated(index, siblings.first).array.asInstanceOf[Tree[U]])
				else
					replace(index, siblings.first, siblings.second, minRank, maxRank)

			def replace[U >: E](index :Int, first :Tree[U], second :Tree[U], minRank :Int, maxRank :Int) :Siblings[U] = {
				val rank = array.length - 1
				if (rank < maxRank)
					Siblings(new Children[Tree[U]](array).replaced(index, first, second).array.asInstanceOf[Tree[U]])
				else {
					val prefixRank = math.max(minRank, rank + 1 >> 1)
					new Children[Tree[U]](array).replaceAndSplit(index, first, second, prefixRank).castParam[U]
				}
			}

			def replace[U >: E](index :Int, first :Tree[U], second :Tree[U], third :Tree[U], minRank :Int, maxRank :Int)
					:Siblings[U] =
			{
				val rank = array.length
				if (rank + 1 < maxRank)
					Siblings(new Children[Tree[U]](array).replaced(index, first, second, third).array.asInstanceOf[Tree[U]])
				else {
					val prefixRank = math.max(minRank, rank + 2 >> 1)
					new Children[Tree[U]](array).replaceAndSplit(index, first, second, third, prefixRank).castParam[U]
				}
			}

			def slice(from :Int, until :Int) :Tree[E] = {
				val length = this.length
				if (until <= from | until <= 0 | from >= length) emptyLeaf
				else if (from <= 0 && until >= length)
					if (array.length == 2 && array(0) != null) //infix may consist of a single child.
						array(1).asInstanceOf[Tree[E]]
					else array.asInstanceOf[Tree[E]]
				else slice(math.max(0, from), math.min(until, length), level)
			}

			private def slice(from :Int, until :Int, level :Int) :Tree[E] =
				if (from <= 0 && until >= length)
					array.asInstanceOf[Tree[E]]
				else if (until <= from)
					emptyLeaf
				else if (array(0) == null)
					new Children[E](array).slice(from, until).array.asInstanceOf[Tree[E]]
				else {
					val fromIdx       = nextIndex(from)
					val fromChildIdx  = fromIdx.child
					val untilIdx      = nextIndex(until)
					val untilChildIdx = untilIdx.child

					def prefixSlice = {          //Slice the child containing element at from.
						val fromChild = child(fromChildIdx)
						if (fromIdx.relative == 0) fromChild
						else fromChild.slice(fromIdx.relative, fromChild.length, level - 1)
					}
					def mergePrefix = {          //Merge the first, incomplete child, into the following one.
						val prefix = prefixSlice //We have no fast check if the slice did not return an existing object.
						child(fromChildIdx + 1).prependedAll(prefix, level - 1, prefix.level, true, true) //prefix.level <= level - 1
					}
					def mergeSuffix =            //Merge the last, incomplete child, into the preceding one.
						if (untilIdx.relative == 0)
							Siblings(child(untilChildIdx - 1))
						else {
							val suffix = child(untilChildIdx).slice(0, untilIdx.relative, level - 1)
							val lastChild = child(untilChildIdx - 1)
							lastChild.appendedAll(suffix, level - 1, suffix.level, true, true) //suffix.level <= level - 1
						}
					if (fromChildIdx == untilChildIdx)       //The whole subsequence belongs to a single child.
						child(fromChildIdx).slice(fromIdx.relative, untilIdx.relative, level - 1)
					else if (fromChildIdx + 1 == untilChildIdx && untilIdx.relative == 0)
						child(fromChildIdx).slice(fromIdx.relative, fromIdx.relative + (until - from), level - 1)
					else if (fromIdx.relative == 0)          //Slice starts exactly at a boundary between our children.
						if (untilIdx.relative == 0)          //Slice consists of full children: simply slice the array.
							if (fromChildIdx + 1 == untilChildIdx)
								child(fromChildIdx)
							else
								Tree(children.slice(fromChildIdx, untilChildIdx))
						else { //untilIdx.relative > 0
							val siblings    = mergeSuffix
							val first       = siblings.first
							val sliceLength = untilChildIdx - fromChildIdx
							if (siblings.size == 1)
								if (sliceLength == 1)
									siblings.first
								else
									Tree(children.sliceAndUpdate(fromChildIdx, untilChildIdx, sliceLength - 1, first))
							else {
								//Don't use sliceAndUpdate/sliceAndInsert because we'd need to recompute the lengths again.
								val res = array.slice(fromChildIdx, 2 + untilChildIdx)
								res(sliceLength)     = first
								res(sliceLength + 1) = siblings.second
								updateLengths(new Children(res))
								res.asInstanceOf[Tree[E]]
							}
						}
					else if (untilIdx.relative == 0) {       //Slice ends exactly at a boundary between our children.
						val siblings = mergePrefix
						if (siblings.size == 1)
							if (fromChildIdx + 2 == untilChildIdx)
								siblings.first
							else
								Tree(children.sliceAndUpdate(fromChildIdx + 1, untilChildIdx, 0, siblings.first))
						else {
							//Don't use sliceAndUpdate/sliceAndInsert because we'd need to recompute the lengths again.
							val res = array.slice(fromChildIdx, 1 + untilChildIdx)
							res(1)  = siblings.first
							res(2)  = siblings.second
							updateLengths(new Children(res))
							res.asInstanceOf[Tree[E]]
						}
					} else if (fromChildIdx + 1 == untilChildIdx) {
						//Prefix and suffix come from adjacent nodes: merge them directly.
						val suffix = child(untilChildIdx).slice(0, untilIdx.relative, level - 1)
						suffix.insertedAll(0, prefixSlice)
					} else if (fromChildIdx + 2 == untilChildIdx) {
						//Prefix and suffix nodes are separated by a single child: merge both into the latter.
						val prefix   = prefixSlice
						val suffix   = child(untilChildIdx).slice(0, untilIdx.relative, level - 1)
						val middle   = child(fromChildIdx + 1)
						val suffixed = middle.appendedAll(suffix, level - 1, suffix.level, true, true)
						val first = suffixed.first
						val res   = first.prependedAll(prefix, level - 1, prefix.level, false, true)
						if (suffixed.size == 1)
							res.toTree
						else {
							if (res.size == 1)
								Tree(Children(res.first, suffixed.second))
							else
								Tree(Children(res.first, res.second, suffixed.second))
						}
					} else {
						//Merge prefix with the following child, and suffix with its preceding child;
						// sandwich full children between the merged prefix and suffix.
						val prefixSiblings = mergePrefix
						val suffixSiblings = mergeSuffix
						val firstIdx = fromChildIdx + (2 - prefixSiblings.size)
						val lastIdx  = untilChildIdx - 1 + suffixSiblings.size
						val res = array.slice(firstIdx, lastIdx + 1)
						res(1) = prefixSiblings.first
						if (prefixSiblings.size == 2)
							res(2) = prefixSiblings.second
						if (suffixSiblings.size == 1)
							res(lastIdx - firstIdx) = suffixSiblings.first
						else {
							res(lastIdx - firstIdx - 1) = suffixSiblings.first
							res(lastIdx - firstIdx)     = suffixSiblings.second
						}
						updateLengths(new Children(res))
						res.asInstanceOf[Tree[E]]
					}
				}

				def map[O](f :E => O) :Tree[O] = {
					val length = array.length
					val res = new Array[Any](length)
					var i = 1
					array(0) match {
						case null =>
							while (i < length) {
								res(i) = f(array(i).asInstanceOf[E])
								i += 1
							}
						case lengths =>
							res(0) = lengths
							while (i < length) {
								res(i) = new TreeExtension[E](array(i).asInstanceOf[Array[Any]]).map(f)
								i += 1
							}
					}
					res.asInstanceOf[Tree[O]]
				}

				def indexWhere(f :E => Boolean, from :Int, satisfies :Boolean) :Int = {
					val length = array.length
					var res = -1
					array(0) match {
						case lengths :Array[Int] =>
							var i      = binSearch(lengths, from) + 1
							var prefix = if (i == 1) 0 else lengths(i - 2)
							var offset = from - prefix
							while (i < length && {
								val child = new TreeExtension[E](array(i).asInstanceOf[Array[Any]])
								res = child.indexWhere(f, offset, satisfies)
								offset = 0
								i += 1
								res == -1 && {
									prefix = lengths(i - 2)
									true
								}
							}) ()
							if (res >= 0)
								res += prefix
							res
						case _ =>
							var i = from + 1
							while (i < length && f(array(i).asInstanceOf[E]) != satisfies)
								i += 1
							if (i < length) i - 1 else -1
					}
				}

				def foreach[U](f :E => U) :Unit = {
					val length = array.length
					var i = 1
					if (array(0) == null)
						while (i < length) {
							f(array(i).asInstanceOf[E])
							i += 1
						}
					else
						while (i < length) {
							new TreeExtension[E](array(i).asInstanceOf[Array[Any]]).foreach(f)
							i += 1
						}
				}
				def foldLeft[A](z :A)(f :(A, E) => A) :A = {
					val length = array.length
					var acc = z
					var i = 1
					if (array(0) == null)
						while (i < length) {
							acc = f(acc, array(i).asInstanceOf[E])
							i += 1
						}
					else
						while (i < length) {
							acc = new TreeExtension[E](array(i).asInstanceOf[Array[Any]]).foldLeft(acc)(f)
							i += 1
						}
					acc
				}
				def foldRight[A](z :A)(f :(E, A) => A) :A = {
					var i = array.length - 1
					var acc = z
					if (array(0) == null)
						while (i >= 1) {
							acc = f(array(i).asInstanceOf[E], acc)
							i -= 1
						}
					else
						while (i >= 1) {
							acc = new TreeExtension[E](array(i).asInstanceOf[Array[Any]]).foldRight(acc)(f)
							i -= 1
						}
					acc
				}

			def toSeq :Fingers[E] = level match {
				case 0   => Fingers0
				case 1   =>
					if (array.length == 1) Fingers0
					else new Fingers1(array.asInstanceOf[Tree[E]], array.length - 1)
				case 2   =>
					val rank = array.length - 1
					val children = new Children[Tree[E]](array)
					val infix = if (rank == 2) emptyTree else Tree(children.slice(1, rank - 1))
					Fingers2(children(0), infix, children(rank - 1))
				case 3 =>
					val children   = new Children[Tree[E]](array)
					val fullPrefix = children(0)
					val prefix     = fullPrefix.child(0)
					val prefixes   = Tree(Children(Tree(fullPrefix.children.drop(1))))
					val fullSuffix = children(array.length - 2)
					val suffixRank = fullSuffix.rank
					val suffixes   = Tree(Children(Tree(fullSuffix.children.slice(0, suffixRank - 1))))
					val suffix     = fullSuffix.child(suffixRank - 1)
					val infix      = Tree(children.slice(1, array.length - 2))
					FingersN(prefix, prefixes, infix, suffixes, suffix, length)
				case lvl =>
					@tailrec def collectPrefixes(prefix :Children[Tree[E]], lvl :Int, res :Children[Tree[E]]) :Tree[E] = {
						res(lvl - 1) = Tree(prefix.drop(1))
						if (lvl == 1) {
							updateLengths(res)
							prefix(0)
						} else
							collectPrefixes(prefix(0).children, lvl - 1, res)
					}
					@tailrec
					def collectSuffixes(suffix :Children[Tree[E]], lvl :Int, res :Children[Tree[E]])
							:Tree[E] =
					{
						res(res.array.length - lvl - 1) = Tree(suffix.take(suffix.array.length - 2))
						if (lvl == 1) {
							updateLengths(res)
							suffix(suffix.array.length - 2)
						} else
							collectSuffixes(suffix(suffix.array.length - 2).children, lvl - 1, res)
					}
					val children = new Children[Tree[E]](array)
					val prefixes = new Children[Tree[E]](new Array[Any](lvl - 1))
					val suffixes = new Children[Tree[E]](new Array[Any](lvl - 1))
					val prefix   = collectPrefixes(children(0).children, lvl - 2, prefixes)
					val suffix   = collectSuffixes(children(array.length - 2).children, lvl - 2, suffixes)
					val infix    = children.slice(1, array.length - 2)
					FingersN(prefix, Tree(prefixes), Tree(infix), Tree(suffixes), suffix)
			}


			def copyToArray[U >: E](xs :Array[U], start :Int, from :Int, len :Int) :Int = {
				val lengths = array(0).asInstanceOf[Array[Int]]
				if (lengths == null)
					array.asInstanceOf[Array[U]].copyRangeToArray(xs, start, from + 1, len)
				else if (lengths.length == 0)
					0
				else {
					var copied = 0
					var i = binSearch(lengths, from)
					var offset = if (i == 0) from else from - lengths(i - 1)
					val max = math.min(len, lengths(lengths.length - 1) - from)
					while (copied < max) {
						copied += child(i).copyToArray(xs, start + copied, offset, len - copied)
						offset  = 0
						i += 1
					}
					copied
				}
			}


			override def toString :String = mkString("Tree(", ", ", ")")
			def mkString(prefix :String, separator :String, suffix :String) :String =
				addString(new JStringBuilder, prefix, separator, suffix).toString

			def addString(b :JStringBuilder, prefix :String, separator :String, suffix :String) :b.type = {
				b append prefix
				val rank = array.length - 1
				var i = 1
				if (array(0) == null) {
					while (i < rank) {
						b append array(i) append separator
						i += 1
					}
					b append array(rank)
				} else {
					while (i <= rank) {
						new TreeExtension(array(i).asInstanceOf[Array[Any]]).addString(b, "", separator, "")
						i += 1
					}
				}
				b append suffix
				b
			}
		}

		@inline def SplitIndex(child :Int, relative :Int) :SplitIndex =
			new SplitIndex(child.toLong << 32 | relative & 0xffffffffL)

		/** A division of an index in a sequence (finger tree) into an index of its child and index within that child.
		  * When accessing an individual value in a `Tree`/`Children` based on an index, one needs first
		  * to access the correct child, which contains said value, and then access the value at its relative index
		  * in that child.
		  */
		class SplitIndex(private val bits :Long) extends AnyVal {
			/** Index of the child in which this index lies. */
			@inline def child = (bits >> 32).toInt
			/** Index of the element within child `child`. */
			@inline def relative = bits.toInt

			override def toString :String = child.toString + '/' + relative
		}



		/** Represents an underlying `Array[Any]` of a `Tree[E]` as a list of its children.
		  * For nodes of level higher than `1` (which have an associated `Array[Int]` of prefix lengths for fast access),
		  * methods of this class will compute the correct prefix lengths for every returned `Children` value,
		  * and set them at position `0` in the underlying array.
		  * It is purely refactoring of the API from `Tree` to avoid confusion between `n`-th ''value''
		  * in the sequence, and the `n`-th child of the node (which may also be a value for nodes of level 1).
		  * Additionally, unlike `Tree`, it is a value class wrapping the array, meaning no implicit conversion
		  * is necessary to access its methods.
		  * @tparam T either the element type of the sequence, or `Tree[E]`, where `E` is the element type.
		  */
		class Children[+T](val array :Array[Any]) extends AnyVal {
			@inline def toIRefArray :IRefArray[T] = array.asInstanceOf[IRefArray[T]]
			@inline def length = array.length - 1
			@inline def apply(idx :Int) :T = array(idx + 1).asInstanceOf[T]
			@inline def update(idx :Int, child :T @uncheckedVariance) :Unit = array(idx + 1) = child
			@inline def head :T = array(1).asInstanceOf[T]
			@inline def last :T = array(array.length - 1).asInstanceOf[T]

			@inline def updated[U >: T](index :Int, elem :U) :Children[U] = {
				val res = new Children[U](Array.copyOf(array, array.length))
				val lengths = array(0).asInstanceOf[Array[Int]]
				if (lengths == null)
					res.array(index + 1) = elem
				else {
					val oldLength = new TreeExtension(array(index + 1).asInstanceOf[Array[Any]]).length
					val newLength = new TreeExtension(elem.asInstanceOf[Array[Any]]).length
					//Must update after the read of the replaced node but before updating lengths.
					res.array(index + 1) = elem
					if (oldLength != newLength)
						updateLengths(res, lengths, index)
				}
				res
			}

			/** Creates a copy of these children with `first` at `index` and `second` at `index + 1`. */
			def updated[U >: T](index :Int, first :U, second :U) :Children[U] = {
				val a = Array.copyOf(array, array.length)
				a(index + 1) = first
				a(index + 2) = second
				val res = new Children[U](a)
				val lengths = array(0).asInstanceOf[Array[Int]]
				if (lengths != null)
					updateLengths(res, lengths, index)
				res
			}

			/** Replaces the child at `index` with two children, growing the collection by one. */
			def replaced[U >: T](index :Int, first :U, second :U) :Children[U] = {
				val rank = array.length
				val res = new Children[U](new Array[Any](rank + 1))
				arraycopy(array, 1, res.array, 1, index)
				res.array(index + 1) = first
				res.array(index + 2) = second
				arraycopy(array, index + 2, res.array, index + 3, rank - index - 2)
				val lengths = array(0).asInstanceOf[Array[Int]]
				if (lengths != null)
					updateLengths(res, lengths, index)
				res
			}

			def replaced[U >: T](index :Int, first :U, second :U, third :U) :Children[U] = {
				val rank = array.length
				val res = new Children[U](new Array[Any](rank + 2))
				arraycopy(array, 1, res.array, 1, index)
				res.array(index + 1) = first
				res.array(index + 2) = second
				res.array(index + 3) = third
				arraycopy(array, index + 2, res.array, index + 4, rank - index - 2)
				val lengths = array(0).asInstanceOf[Array[Int]]
				if (lengths != null)
					updateLengths(res, lengths, index)
				res
			}

			/** Replaces the child at `index` with the trees/values in the argument,
			  *  growing the collection by `elems.length - 1`.
			  */
			def replacedAll[U >: T](index :Int, elems :Children[U]) :Siblings[U] = {
				val elemsRank = elems.length
				val rank = array.length - 1
				if (rank - 1 + elemsRank <= MaxChildren) {
					val res = Children.copyOfRanges(
						this, 0, index, elems, 0, elemsRank, this, index + 1, rank
					)
					Siblings(Tree1(res))
				} else {
					val prefixRank = rank + elemsRank >> 1
					val elemsPrefixLen = prefixRank - index
					val prefix = Children.copyOfRanges(
						this, 0, index, elems, 0, elemsPrefixLen
					)
					val suffix = new Array[Any](rank + elemsRank - prefixRank)
					arraycopy(elems.array, elemsPrefixLen + 1, suffix, 1, elemsRank - elemsPrefixLen)
					arraycopy(array, index + 2, suffix, 1 + elemsRank - elemsPrefixLen, rank - (index + 1))
					Siblings(Tree1(prefix), suffix.asInstanceOf[Tree[U]])
				}
			}

			def replaceAndSplit[U >: T](index :Int, first :U, second :U, prefixLength :Int) :Siblings[U] = {
				val suffixLength = array.length - prefixLength
				val prefix = new Array[Any](prefixLength + 1)
				val suffix = new Array[Any](suffixLength + 1)
				if (index < prefixLength - 1) {
					arraycopy(array, 1, prefix, 1, index)
					prefix(index + 1) = first
					prefix(index + 2) = second
					arraycopy(array, index + 2, prefix, index + 3, prefixLength - index - 2)
					arraycopy(array, prefixLength, suffix, 1, suffixLength)
				} else if (index == prefixLength - 1) {
					arraycopy(array, 1, prefix, 1, index)
					prefix(prefixLength) = first
					suffix(1) = second
					arraycopy(array, index + 2, suffix, 2, suffixLength - 1)
				} else {
					val indexInSuffix = index - prefixLength
					arraycopy(array, 1, prefix, 1, prefixLength)
					arraycopy(array, prefixLength + 1, suffix, 1, indexInSuffix)
					suffix(indexInSuffix + 1) = first
					suffix(indexInSuffix + 2) = second
					arraycopy(array, index + 2, suffix, indexInSuffix + 3, array.length - index - 2)
				}
				if (array(0) != null)
					updateLengths(new Children(prefix))
				Siblings(prefix.asInstanceOf[Tree[U]], suffix.asInstanceOf[Tree[U]])
			}

			def replaceAndSplit[U >: T](index :Int, first :U, second :U, third :U, prefixLength :Int) :Siblings[U] = {
				val suffixLength = array.length - prefixLength
				val prefix = new Array[Any](prefixLength + 1)
				val suffix = new Array[Any](suffixLength + 1)
				if (index < prefixLength - 2) {
					arraycopy(array, 1, prefix, 1, index)
					prefix(index + 1) = first
					prefix(index + 2) = second
					prefix(index + 3) = third
					arraycopy(array, index + 2, prefix, index + 4, prefixLength - index - 3)
					arraycopy(array, prefixLength - 1, suffix, 1, suffixLength)
				} else if (index == prefixLength - 2) {
					arraycopy(array, 1, prefix, 1, index)
					prefix(index + 1) = first
					prefix(index + 2) = second
					suffix(1) = third
					arraycopy(array, index + 2, suffix, 2, suffixLength - 1)
				} else if (index == prefixLength - 1) {
					arraycopy(array, 1, prefix, 1, index)
					prefix(index + 1) = first
					suffix(1) = second
					suffix(2) = third
					arraycopy(array, index + 2, suffix, 3, suffixLength - 2)
				} else {
					val indexInSuffix = index - prefixLength
					arraycopy(array, 1, prefix, 1, prefixLength)
					arraycopy(array, prefixLength + 1, suffix, 1, indexInSuffix)
					suffix(indexInSuffix + 1) = first
					suffix(indexInSuffix + 2) = second
					suffix(indexInSuffix + 3) = third
					arraycopy(array, index + 2, suffix, indexInSuffix + 4, array.length - index - 2)
				}
				if (array(0) != null)
					updateLengths(new Children(prefix))
				Siblings(prefix.asInstanceOf[Tree[U]], suffix.asInstanceOf[Tree[U]])
			}

			def replaceAllAndSplit[U >: T](index :Int, elems :Children[U], prefixLength :Int) :Siblings[U] = {
				val elemsLength  = elems.array.length - 1
				val totalLength  = array.length - 2 + elemsLength
				val suffixLength = totalLength - prefixLength
				val prefix = new Array[Any](prefixLength + 1)
				val suffix = new Array[Any](suffixLength + 1)
				if (index + elemsLength <= prefixLength) {
					val prefixSuffixLength = prefixLength - index - elemsLength
					arraycopy(array, 1, prefix, 1, index)
					arraycopy(elems.array, 1, prefix, index + 1, elemsLength)
					arraycopy(array, index + 2, prefix, index + 1 + elemsLength, prefixSuffixLength)
					arraycopy(array, prefixLength + 2 - elemsLength, suffix, 1, suffixLength)
				} else if (index < prefixLength) {
					val elemsPrefix = prefixLength - index
					arraycopy(array, 1, prefix, 1, index)
					arraycopy(elems.array, 1, prefix, index + 1, elemsPrefix)
					arraycopy(elems.array, elemsPrefix + 1, suffix, 1, elemsLength - elemsPrefix)
					arraycopy(array, index + 2, suffix, elemsLength - elemsPrefix + 1, array.length - index - 2)
				} else {
					arraycopy(array, 1, prefix, 1, prefixLength)
					arraycopy(array, prefixLength + 1, suffix, 1, index - prefixLength)
					arraycopy(elems.array, 1, suffix, index - prefixLength + 1, elemsLength)
					arraycopy(array, index + 2, suffix, index + elemsLength - prefixLength + 1, array.length - index - 2)
				}
				val lengths = array(0).asInstanceOf[Array[Int]]
				if (lengths != null)
					updateLengths(new Children(prefix))
				Siblings(prefix.asInstanceOf[Tree[U]], suffix.asInstanceOf[Tree[U]])
			}


			@inline def copy :Children[T] = new Children(Array.copyOf(array, array.length))

			def slice(from :Int, until :Int) :Children[T] = {
				val rank = until - from
				val lengths = array(0).asInstanceOf[Array[Int]]
				if (rank <= 0)
					if (lengths == null) (emptyLeaf :Tree[T]).values
					else (emptyTree :Tree[T]).values
				else if (from <= 0 & until >= array.length - 1)
					this
				else {
					val res = new Array[Any](rank + 1)
					arraycopy(array, from + 1, res, 1, until - from)
					if (lengths != null)
						updateLengths(new Children(res))
					new Children(res)
				}
			}
			@inline def take(n :Int) :Children[T] = slice(0, n)
			@inline def drop(n :Int) :Children[T] = slice(n, array.length - 1)
			@inline def tail :Children[T] = slice(1, array.length - 1)
			@inline def init :Children[T] = slice(0, array.length - 2)

			def sliceAndUpdate[U >: T](from :Int, until :Int, index :Int, elem :U) :Children[U] = {
				val len     = until - from + 1
				val lengths = array(0).asInstanceOf[Array[Int]]
				val res     = new Array[Any](len)
				arraycopy(array, from + 1, res, 1, until - from)
				res(index + 1) = elem
				if (lengths != null)
					updateLengths(new Children(res))
				new Children(res)
			}

			def sliceAndInsert[U >: T](from :Int, until :Int, index :Int, elem :U) :Children[U] = {
				val res = new Array[Any](until - from + 2)
				arraycopy(array, from + 1, res, 1, index)
				res(index + 1) = elem
				arraycopy(array, from + index + 1, res, index + 2, until - from - index)
				val lengths = array(0).asInstanceOf[Array[Int]]
				if (lengths != null)
					updateLengths(new Children(res))
				new Children(res)
			}

			@inline def prepended[U >: T](child :U) :Children[U] = inserted(0, child)
			@inline def appended[U >: T](child :U) :Children[U] = inserted(array.length - 1, child)

			/** Inserts the child into this children at index `idx`, updating the prefix length counts for non leaf nodes.
			  * If `this.length == MaxChildren`, the current last child is removed from the collection. */
			def inserted[U >: T](idx :Int, child :U) :Children[U] = {
				val len = math.min(array.length, MaxChildren)
				val res = RefArray.copyOfRanges(array, 0, idx + 1, array, idx, len).asAnyArray
				res(idx + 1) = child
				val lengths = array(0).asInstanceOf[Array[Int]]
				if (lengths != null)
					updateLengths(new Children(res), lengths, idx)
				new Children(res)
			}

			@inline def prependedAll[U >: T](siblings :Children[U]) :Children[U] =
				insertedAll(0, siblings)

			@inline def appendedAll[U >: T](siblings :Children[U]) :Children[U] =
				insertedAll(array.length - 1, siblings)

			@inline def insertedAll[U >: T](idx :Int, siblings :Children[U]) :Children[U] =
				insertedSlice(idx, siblings, 0, siblings.length)

			@inline def insertedAll[U >: T](idx :Int, siblings :Children[U], minRank :Int, maxRank :Int) :Siblings[U] =
				insertedSlice(idx, siblings, 0, siblings.length, minRank, maxRank)

			def insertedSlice[U >: T](idx :Int, siblings :Children[U], from :Int, until :Int, minRank :Int, maxRank :Int)
					:Siblings[U] =
				if (length + until - from <= maxRank)
					Siblings(Tree1(insertedSlice(idx, siblings, from, until)))
				else
					insertSliceAndSplit(idx, siblings, from, until, math.max(minRank, length + until - from >> 1))

			def insertedSlice[U >: T](idx :Int, siblings :Children[U], from :Int, until :Int) :Children[U] =
				if (array.length == 1)
					siblings
				else {
					val cutOff = math.min(array.length, 1 + MaxChildren - (until - from))
					val a =
						if (cutOff <= idx + 1)
							RefArray.copyOfRanges(
								array, 0, 1 + idx, siblings.array, 1 + from, until + cutOff - idx
							)
						else
							RefArray.copyOfRanges(
								array, 0, 1 + idx, siblings.array, 1 + from, 1 + until, array, 1 + idx, cutOff
							)
					val res = new Children[U](a.asAnyArray)
					val lengths = array(0).asInstanceOf[Array[Int]]
					if (lengths != null)
						updateLengths(res, lengths, idx)
					res
				}


			def insertAndSplit[U >: T](index :Int, elem :U, prefixLength :Int) :Siblings[U] = {
				val total = array.length
				if (prefixLength <= index) { //Insert elem into suffix.
					val indexInSuffix = index - prefixLength
					val suffix = new Array[Any](1 + total - prefixLength)
					arraycopy(array, 1 + prefixLength, suffix, 1, indexInSuffix)
					arraycopy(array, 1 + index, suffix, indexInSuffix + 2, total - index - 1)
					suffix(1 + indexInSuffix) = elem
					val prefix = slice(0, prefixLength)
					Siblings(Tree1(prefix), suffix.asInstanceOf[Tree[U]])
				} else {                     //Insert elem into the returned collection.
					val suffix = array.slice(prefixLength - 1, total)
					val prefix = new Array[Any](1 + prefixLength)
					arraycopy(array, 1, prefix, 1, index)
					arraycopy(array, 1 + index, prefix, index + 2, prefixLength - index - 1)
					prefix(1 + index) = elem
					val lengths = array(0).asInstanceOf[Array[Int]]
					if (lengths != null)
						updateLengths(new Children(prefix), lengths, index)
					Siblings(prefix.asInstanceOf[Tree[U]], suffix.asInstanceOf[Tree[U]])
				}
			}

			/** Immutably Inserts `other` values at `index` in this value list,
			  * and splits the result into two `Children` collections, of which the first has `prefixLength` elements.
			  * @note This method may only be called for collections of values, not other nodes.
			  */
			@inline def insertAllAndSplit[U >: T](index :Int, other :Children[U], prefixLength :Int) :Siblings[U] =
				insertSliceAndSplit(index, other, 0, other.length, prefixLength)

			def insertSliceAndSplit[U >: T](index :Int, other :Children[U], from :Int, until :Int, prefixLength :Int)
					:Siblings[U] =
			{
				//Don't compute lengths for suffix, as they will be overwritten when returning it as Siblings.
				val len = array.length - 1
				val otherLength = until - from
				val suffixLength = len + otherLength - prefixLength
				val suffix = new Array[Any](1 + suffixLength)
				val prefix =
					//We do not attempt to reuse other if from == 0 & until == other.length,
					// because putting it in Siblings will erase its lengths, and may corrupt an existing instance.
					if (prefixLength == len & index == prefixLength) {
						arraycopy(other.array, 1 + from, suffix, 1, otherLength)
						this
					} else if (prefixLength <= index) { //Insert all children from elems to suffix.
						val firstBit = index - prefixLength
						arraycopy(array, 1 + prefixLength, suffix, 1, firstBit)
						arraycopy(other.array, 1 + from, suffix, 1 + firstBit, otherLength)
						arraycopy(array, 1 + index, suffix, 1 + firstBit + otherLength, len - index)
						slice(0, prefixLength)
					} else if (prefixLength >= index + otherLength) { //Insert all children from other to the first half.
						arraycopy(array, 1 + len - suffixLength, suffix, 1, suffixLength)
						Children.copyOfRanges(this, 0, index, other, from, until, this, index, prefixLength - otherLength)
					} else { //Split inserted nodes between the returned collection and remainder.
						val dropOther = prefixLength - index
						arraycopy(other.array, 1 + from + dropOther, suffix, 1, until - dropOther)
						arraycopy(array, 1 + index, suffix, 1 + otherLength - dropOther, len - index)
						Children.copyOfRanges(this, 0, index, other, from, dropOther)
					}
				Siblings(Tree1(prefix), suffix.asInstanceOf[Tree[U]])
			}

			@inline def removed(idx :Int) :Children[T] = removed(idx, idx + 1)

			def removed(from :Int, until :Int) :Children[T] = {
				val rank = array.length - 1
				if (until <= 0 | until <= from | from >= rank)
					this
				else if (from <= 0 & until >= rank)
					if (array(0) == null) emptyLeaf.values else emptyTree.values
				else {
					val lengths = array(0).asInstanceOf[Array[Int]]
					val newRank = rank - (until - from)
					val res = new Children[T](array.removed(1 + from, 1 + until))
					if (lengths != null) {
						val newLengths = Array.copyOf(lengths, rank - (until - from))
						val removedLength = lengths(until - 1) - (if (from == 0) 0 else lengths(from - 1))
						var i = from
						while (i < newRank) {
							newLengths(i) = lengths(i + until - from) - removedLength
							i += 1
						}
						res.array(0) = newLengths
					}
					res
				}
			}
			def removeAndUpdate[U >: T](removeIdx :Int, updateIdx :Int, elem :U) :Children[U] = {
				val len = array.length - 1
				val res = new Array[Any](len)
				arraycopy(array, 1, res, 1, removeIdx)
				arraycopy(array, removeIdx + 2, res, removeIdx + 1, len - removeIdx - 1)
				res(updateIdx + 1) = elem
				val lengths = array(0).asInstanceOf[Array[Int]]
				if (lengths != null)
					updateLengths(new Children(res), lengths, math.min(removeIdx, updateIdx))
				new Children(res)
			}
			def removeAndPrepend[U >: T](index :Int, elem :U) :Children[U] = {
				val len = array.length
				val res = new Array[Any](len)
				res(1) = elem
				arraycopy(array, 1, res, 2, index)
				arraycopy(array, index + 2, res, index + 2, len - 2 - index)
				val lengths = array(0).asInstanceOf[Array[Int]]
				if (lengths != null)
					updateLengths(new Children(res))
				new Children(res)
			}
			def removeAndAppend[U >: T](index :Int, elem :U) :Children[U] = {
				val len = array.length
				val res = new Array[Any](len)
				res(len - 1) = elem
				arraycopy(array, 1, res, 1, index)
				arraycopy(array, index + 2, res, index + 1, len - 2 - index)
				val lengths = array(0).asInstanceOf[Array[Int]]
				if (lengths != null)
					updateLengths(new Children(res), lengths, index)
				new Children(res)
			}
			def removeAndPrependAll[U >: T](index :Int, prefix :Children[U]) :Children[U] = {
				val prefixLen = prefix.length
				val len = array.length
				val res = new Array[Any](len - 1 + prefixLen)
				arraycopy(prefix.array, 1, res, 1, prefixLen)
				arraycopy(array, 1, res, 1 + prefixLen, index)
				arraycopy(array, index + 2, res, prefixLen + 1 + index, len - index - 2)
				val lengths = array(0).asInstanceOf[Array[Int]]
				if (lengths != null)
					updateLengths(new Children(res))
				new Children(res)
			}
			def removeAndAppendAll[U >: T](index :Int, suffix :Children[U]) :Children[U] = {
				val suffixLen = suffix.length
				val len = array.length
				val res = new Array[Any](len - 1 + suffixLen)
				arraycopy(array, 1, res, 1, index)
				arraycopy(array, index + 2, res, index + 1, len - index - 2)
				arraycopy(suffix.array, 1, res, len - 1, suffixLen)
				val lengths = array(0).asInstanceOf[Array[Int]]
				if (lengths != null)
					updateLengths(new Children(res))
				new Children(res)
			}

			@inline def copyToArray(xs :Array[Any], start :Int, from :Int, len :Int) :Int =
				array.copyRangeToArray(xs, start, from + 1, len)

			def addString(b :JStringBuilder, prefix :String, separator :String, suffix :String) :b.type = {
				val rank = array.length - 1
				var i = 1
				b append prefix
				if (array(0) == null) {
					while (i < rank) {
						new TreeExtension(array(i).asInstanceOf[Array[Any]]).addString(b, "Leaf(", ", ", ")")
						b append ", "
						i += 1
					}
					new TreeExtension(array(rank).asInstanceOf[Array[Any]]).addString(b, "Leaf(", ", ", ")")
				} else {
					while (i <= rank) {
						new Children(array(i).asInstanceOf[Array[Any]]).addString(b, "Tree(", ", ", ")")
						b append ", "
						i += 1
					}
				}
				b append suffix
				b
			}
			override def toString :String = addString(new JStringBuilder, "Tree(", ", ", ")").toString
		}


		object Children {
			@inline def apply[E](size :Int) :Children[E] = new Children[E](new Array[Any](size + 1))

			def apply[E](child :Tree[E]) :Children[Tree[E]] = {
				val array = new Array[Any](2)
				val lengths = new Array[Int](1)
				lengths(0) = child.length
				array(0) = lengths
				array(1) = child
				new Children(array)
			}
			def apply[E](first :Tree[E], second :Tree[E]) :Children[Tree[E]] = {
				val array   = new Array[Any](3)
				val lengths = new Array[Int](2)
				val length1 = first.length
				lengths(0)  = length1
				lengths(1)  = length1 + second.length
				array(0)    = lengths
				array(1)    = first
				array(2)    = second
				new Children(array)
			}
			def apply[E](first :Tree[E], second :Tree[E], third :Tree[E]) :Children[Tree[E]] = {
				val array   = new Array[Any](4)
				val lengths = new Array[Int](3)
				val length1 = first.length
				val length2 = second.length
				lengths(0)  = length1
				lengths(1)  = length1 + length2
				lengths(2)  = length1 + length2 + third.length
				array(0)    = lengths
				array(1)    = first
				array(2)    = second
				array(3)    = third
				new Children(array)
			}
			def apply[E](values :RefArrayLike[E], offset :Int, rank :Int) :Children[E] = {
				val array = new Array[Any](rank + 1)
				arraycopy(values, offset, array, 1, rank)
				new Children(array)
			}

			def copyOfRanges[T](nodes1 :Children[T], from1 :Int, until1 :Int,
			                    nodes2 :Children[T], from2 :Int, until2 :Int) :Children[T] =
			{
				val res = copyOfRanges(
					nodes1.toIRefArray, from1 + 1, until1 + 1,
					nodes2.toIRefArray, from2 + 1, until2 + 1
				)
				if (nodes1.array(0) != null)
					updateLengths(res)
				res
			}
			def copyOfRanges[T](nodes1 :Children[T], from1 :Int, until1 :Int,
			                    nodes2 :Children[T], from2 :Int, until2 :Int,
			                    nodes3 :Children[T], from3 :Int, until3 :Int) :Children[T] =
			{
				val res = copyOfRanges(
					nodes1.toIRefArray, from1 + 1, until1 + 1,
					nodes2.toIRefArray, from2 + 1, until2 + 1,
					nodes3.toIRefArray, from3 + 1, until3 + 1
				)
				if (nodes1.array(0) != null)
					updateLengths(res)
				res
			}

			def copyOfRanges[E](nodes1 :RefArrayLike[E], from1 :Int, until1 :Int,
			                    nodes2 :RefArrayLike[E], from2 :Int, until2 :Int) :Children[E] =
			{
				val len1  = until1 - from1
				val len2  = until2 - from2
				val array = new Array[Any](1 + len1 + len2)
				arraycopy(nodes1, from1, array, 1, len1)
				arraycopy(nodes2, from2, array, 1 + len1, len2)
				new Children(array)
			}
			def copyOfRanges[E](nodes1 :RefArrayLike[E], from1 :Int, until1 :Int,
			                    nodes2 :RefArrayLike[E], from2 :Int, until2 :Int,
			                    nodes3 :RefArrayLike[E], from3 :Int, until3 :Int) :Children[E] =
			{
				val len1  = until1 - from1
				val len2  = until2 - from2
				val len3  = until3 - from3
				val array = new Array[Any](1 + len1 + len2 + len3)
				arraycopy(nodes1, from1, array, 1, len1)
				arraycopy(nodes2, from2, array, 1 + len1, len2)
				arraycopy(nodes3, from3, array, 1 + len1 + len2, len3)
				new Children(array)
			}

		}


		@inline implicit def TreeChildren[E](children :Children[Tree[E]]) :TreeChildren[E] =
			new TreeChildren[E](children.array)

		//todo: see if we can instead make a separate extension class for Children[U], and use Children for Tree[U] only.
		class TreeChildren[E](val array :Array[Any]) extends AnyVal {

			def patch[U >: E](index :Int, replacement :Siblings[U], minRank :Int, maxRank :Int) :Siblings[U] =
				if (replacement.size == 1)
					Siblings(Tree(new Children[Tree[U]](array).updated(index, replacement.first)))
				else
					patch(index, replacement.first, replacement.second, minRank, maxRank)

			def patch[U >: E](index :Int, first :Tree[U], second :Tree[U], minRank :Int, maxRank :Int) :Siblings[U] =
				if (array.length <= maxRank)
					Siblings(Tree(patch(index, first, second)))
				else {
					val prefix = array.slice(0, 1 + minRank)
					val suffix = array.slice(minRank - 1, array.length)
					if (index <= minRank - 2) {
						prefix(index + 1) = first
						prefix(index + 2) = second
						arraycopy(array, index + 2, prefix, index + 3, minRank - index - 2)
					} else if (index == minRank - 1) {
						prefix(minRank) = first
						suffix(1)  = second
					} else {
						arraycopy(array, 1 + minRank, suffix, 1, index - minRank)
						suffix(index - minRank + 1) = first
						suffix(index - minRank + 2) = second
					}
					if (array(0) != null)
						updateLengths(new Children(prefix))
					Siblings(prefix.asInstanceOf[Tree[U]], suffix.asInstanceOf[Tree[U]])
				}
			def patch[U >: E](index :Int, first :Tree[U], second :Tree[U]) :Children[Tree[U]] = {
				val res = new Array[Any](array.length + 1)
				arraycopy(array, 1, res, 1, index)
				res(index + 1) = first
				res(index + 2) = second
				arraycopy(array, index + 2, res, index + 3, array.length - index - 2)
				val children = new Children[Tree[U]](res)
				if (array(0) != null)
					updateLengths(children)
				children
			}
			def patch[U >: E](index :Int, first :Tree[U], next :Siblings[U], minRank :Int, maxRank :Int) :Siblings[U] = {
				val nextSize = next.size
				if (array.length <= 1 + maxRank - nextSize)
					Siblings(Tree(patch(index, first, next)))
				else { //We don't delegate to patch(index, replacement :Siblings[U], minRank, maxRank) because it would clear lengths.
					val prefix = array.slice(0, 1 + minRank)
					val suffix = array.slice(minRank - nextSize, array.length)
					if (index <= minRank - 1 - nextSize) {
						prefix(index + 1) = first
						prefix(index + 2) = next.first
						if (nextSize == 2)
							prefix(index + 3) = next.second
						arraycopy(array, index + 2, prefix, index + 2 + nextSize, minRank - index - 1 - nextSize)
						Siblings(prefix.asInstanceOf[Tree[U]], suffix.asInstanceOf[Tree[U]])
					} else if (index == minRank - 2) { //nextSize == 2
						prefix(index + 1) = first
						prefix(index + 2) = next.first
						suffix(1) = next.second
						Siblings(prefix.asInstanceOf[Tree[U]], suffix.asInstanceOf[Tree[U]])
					} else if (index == minRank - 1) {
						prefix(index + 1) = first
						suffix(1) = next.first
						if (nextSize == 2)
							suffix(2) = next.second
						Siblings(prefix.asInstanceOf[Tree[U]], suffix.asInstanceOf[Tree[U]])
					} else {
						arraycopy(array, 1 + minRank, suffix, 1, index - minRank)
						suffix(index - minRank + 1) = first
						suffix(index - minRank + 2) = next.first
						if (nextSize == 2)
							suffix(index - minRank + 3) = next.second
					}
					if (array(0) != null)
						updateLengths(new Children(prefix))
					Siblings(prefix.asInstanceOf[Tree[U]], suffix.asInstanceOf[Tree[U]])
				}
			}
			def patch[U >: E](index :Int, first :Tree[U], next :Siblings[U]) :Children[Tree[U]] = {
				val nextSize = next.size
				val res = new Array[Any](array.length + nextSize)
				arraycopy(array, 1, res, 1, index)
				res(index) = first
				res(index + 1) = next.first
				if (nextSize == 2)
					res(index + 2) = next.second
				arraycopy(array, 1 + index, res, 1 + index + nextSize, array.length - index - 1)
				val children = new Children[Tree[U]](res)
				if (array(0) != null)
					updateLengths(children)
				children
			}
		}



		/** Wraps a given tree in `Siblings` without boxing, allowing returning a single tree rather than two. */
		@inline def Siblings[E](tree :Tree[E]) :Siblings[E] =
			new Siblings(tree.array)

		/** Contains two trees (as `Children[E]`) in a single value without boxing. */
		@inline def Siblings[E](first :Tree[E], second :Tree[E]) :Siblings[E] = {
			second.array(0) = first.array
			new Siblings(second.array)
		}

		/** Contains one or two trees (as `Children[E]`) in a single value without boxing.
		  * @note `first` ''must'' be called before ''second''.
		  */
		class Siblings[+E](private val array :Array[Any]) extends AnyVal {
			@inline def size :Int =
				if (array == null) 0
				else if (array(0).isInstanceOf[Array[Any]]) 2
				else 1

			@inline def first  :Tree[E] = array(0) match {
				case null             => array.asInstanceOf[Tree[E]]
				case tree :Array[Any] => tree.asInstanceOf[Tree[E]]
				case _                => array.asInstanceOf[Tree[E]] //lengths are precomputed.
			}
			@inline def second :Tree[E] = {
				if (array(0).asInstanceOf[Array[Any]](0) == null)
					array(0) = null
				else
					updateLengths(new Children(array))
				array.asInstanceOf[Tree[E]]
			}
			def toTree :Tree[E] = array(0) match {
				case null             => array.asInstanceOf[Tree[E]]
				case tree :Array[Any] =>
					if (tree.length + array.length <= MaxChildren + 2) {
						array(0) = null
						val res = new Children[Tree[E]](array).insertedAll(0, new Children[Tree[E]](tree))
						if (tree(0) != null)
							updateLengths(res)
						Tree(res)
					} else {
						if (tree(0) != null)
							updateLengths(new Children(array))
						else
							array(0) = null
						Tree(Children(tree.asInstanceOf[Tree[E]], array.asInstanceOf[Tree[E]]))
					}
				case _  => array.asInstanceOf[Tree[E]]
			}
		}


		/** A shared interface for iterators over `Fingers2` and `FingersN` which allows to add them efficiently
		  * to a `FingersBuilder`.
		  */
		abstract class FingersIterator[+E]
			extends AbstractIterator[E] with BufferedIterator[E] with IteratorWithDrop[E] with HasFastSlice[E]
		{
			override def hasFastDrop = true
			def toTree :Tree[E]
		}

		/** An iterator over a finger tree of level 2, caching the current slice (leaf).
		  * @see [[net.noresttherein.sugar.collections.Fingers.Fingers2]]
		  */
		class Iterator2[+E](coll :Fingers2[E]) extends FingersIterator[E] {
			private[this] var from  = 0
			private[this] var until = coll.length
			private[this] var leaf  = coll.firstLeaf.array
			private[this] var idx   = 1

			override def knownSize :Int = until - from
			override def hasNext :Boolean = from < until
			override def head :E =
				if (until <= from) noSuch_!("Fingers.empty.iterator.head")
				else leaf(idx).asInstanceOf[E]

			override def next() :E = {
				if (until <= from)
					noSuch_!("Fingers.empty.iterator.next")
				val res = leaf(idx).asInstanceOf[E]
				from += 1
				idx  += 1
				if (idx == leaf.length) {
					nextSlice()
					idx = 1
				}
				res
			}

			private def nextSlice() :Unit = {
				val infixOffset  = coll.firstLeaf.rank
				val suffixOffset = coll.suffixStart
				if (from >= suffixOffset) {
					leaf = coll.lastLeaf.array
					idx = from - suffixOffset + 1
				} else if (from < infixOffset) {
					leaf = coll.firstLeaf.array
					idx = from + 1
				} else {
					val infix = coll.infixTree
					val index = infix.splitIndex(from - infixOffset)
					leaf = infix.child(index.child).array
					idx = index.relative + 1
				}
			}

			override def take(n :Int) :Iterator[E] = {
				if (n <= 0)
					until = from
				else if (n < until - from)
					until = from + n
				this
			}
			override def drop(n :Int) :Iterator[E] = {
				if (n > 0) {
					from = if (n > until - from) until else from + n
					nextSlice()
				}
				this
			}
			override def splitAt(n :Int) :(Iterator[E], Iterator[E]) =
				if (from >= until) Iterators.splitEmpty
				else if (n >= until - from) (this, Iterator.empty)
				else if (n <= 0) (Iterator.empty, this)
				else (take(n), new Iterator2(coll).drop(from + n))

			override def toTree :Tree[E] = coll.toTree.slice(from, until)

			override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
				coll.copyRangeToArray(xs, start, from, math.min(len, until - from))

			override def toString :String = "Fingers.iterator@" + from + "|" + (until - from) + "|"
		}

		/** An iterator over `FingersN`. It caches the current level 1 node (array with values),
		  * and uses binary search in the tree to find the next one.
		  */
		class IteratorN[+E](coll :FingersN[E]) extends FingersIterator[E] {
			private[this] var from    = 0               //The index of the next element in the whole tree.
			private[this] var until   = coll.length     //The index after the last element of the iterator.
			private[this] var subtree = coll.firstLeaf  //The subtree of a finger subtree (prefix, prefixes, infix, suffixes, suffix)
			private[this] var leaf    = subtree.array   //The leaf with the next element
			private[this] var idx = 1                   //The index of the next element in the current leaf
			private[this] var treeOffset = 0            //The index of the first element of subtree in the whole finger tree.
			private[this] var treeEnd = leaf.length - 1 //The index in the whole finger tree after the last element of subtree.

			override def knownSize :Int = until - from
			override def hasNext :Boolean = from < until
			override def head :E =
				if (until <= from) noSuch_!("Fingers.empty.iterator.head")
				else leaf(idx).asInstanceOf[E]

			override def next() :E = {
				if (until <= from)
					noSuch_!("Fingers.empty.iterator")
				val res = leaf(idx).asInstanceOf[E]
				idx  += 1
				from += 1
				if (idx == leaf.length) {
					if (from == treeEnd)
						nextTree()
					nextSlice(subtree, from - treeOffset)
				}
				res
			}

			private def nextTree() :Unit = {
				val infixOffset = coll.initLength
				if (from < infixOffset) {
					val prefix = coll.firstLeaf
					val prefixLen = prefix.rank
					if (from < prefixLen) {
						subtree = prefix
						treeOffset = 0
						treeEnd = prefixLen
					} else {
						subtree = coll.secondTree
						treeOffset = prefixLen
						treeEnd = infixOffset
					}
				} else {
					val length = coll.length
					val suffixOffset = length - coll.tailLength
					if (from >= suffixOffset) {
						val lastOffset = length - coll.lastLeaf.rank
						if (from >= lastOffset) {
							subtree = coll.lastLeaf
							treeOffset = lastOffset
							treeEnd = length
						} else {
							subtree = coll.fourthTree
							treeOffset = suffixOffset
							treeEnd = lastOffset
						}
					} else {
						subtree = coll.thirdTree
						treeOffset = infixOffset
						treeEnd = suffixOffset
					}
				}
			}

			@tailrec private[this] def nextSlice(tree :Tree[E], i :Int) :Unit =
				if (tree.isLeaf) {
					leaf = tree.array
					idx  = i + 1
				} else {
					val split = tree.splitIndex(i)
					nextSlice(tree.child(split.child), split.relative)
				}

			override def take(n :Int) :Iterator[E] = {
				if (n <= 0)
					until = from
				else if (n < until - from)
					until = from + n
				this
			}
			override def drop(n :Int) :Iterator[E] = {
				if (n > 0)
					if (n >= until - from)
						from = until
					else {
						from += n
						idx += n
						if (idx >= leaf.length) {
							if (from >= treeEnd)
								nextTree()
							nextSlice(subtree, from - treeOffset)
						}
					}
				this
			}
			override def splitAt(n :Int) :(Iterator[E], Iterator[E]) =
				if (from >= until) Iterators.splitEmpty
				else if (n >= until - from) (this, Iterator.empty)
				else if (n <= 0) (Iterator.empty, this)
				else (take(n), new IteratorN(coll).drop(from + n))

			override def toTree :Tree[E] = coll.toTree.slice(from, until) //todo: a builder-based implementation.

			override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
				coll.copyRangeToArray(xs, start, from, math.min(len, until - from))

			override def toString :String = "Fingers.iterator@" + from + "|" + (until - from) + "|"
		}



		class FingersBuilder[E] extends ReusableBuilder[E, Fingers[E]] {
			/** The suffix of the built sequence: an array of length `1 + MaxChildren` containing `rank0` values,
			  * starting at offset `1`. New elements added to the builder are appended to this array.
			  * If the array is already full, it is added as a child of `suffixes(0)` (creating `suffixes`,
			  * if it is not initialized). May be `null` only when `rank0 == MaxChildren`. */
			private[this] var lvl0     :Array[Any] = _

			/** The number of values in `lvl0`, `0 <= rank0 <= MaxChildren`. If equal to `MaxChildren`,
			  * `lvl0` may be `null`. */
			private[this] var rank0    :Int = MaxChildren

			/** An array of length `level`, containing suffix trees of increasing level. The last element
			  * (`suffixes(level - 1)`) is the current root of the built `Tree`.
			  * If non null, elements `0..level` are all either `null` or arrays of length `1 + MaxChildren`,
			  * with the `n`-th array containing containing `ranks(n)` children (arrays of level `n-1`),
			  * starting at offset `1`. An array may be `null` only if `ranks(lvl) == 0`.
			  * All children and deeper descendants are always of rank `Rank <= r <= MaxChildren`.
			  * When a new level `n-1` array is added, but the `n` level array is full, the `n`-level array
			  * is recursively added as a child of level `n+1` array, and the `n` level array is initialized
			  * to a new array of length `MaxChildren + 1`, containing the added `n-1` level array.
			  * If the array at level `level - 1` is full, `suffixes(level)` is initialized to a new array
			  * containing the filled `level - 1` array as its only element; the size of `suffixes` itself
			  * is increased as necessary. */
			private[this] var suffixes :Array[Array[Any]] = _

			/** The number of elements in `suffixes(i)`, `0 <= i < level`. The first element is always at offset `1`. */
			private[this] var ranks    :Array[Int] = _

			/** The number of initialized inner node arrays in `suffixes`. */
			private[this] var level = 0

			/** The total number of elements added to the builder, including those in all `suffixes` arrays
			  * (not only the root `suffixes(level - 1)`). */
			private[this] var size  = 0

			/** Makes sure `suffixes` and `ranks` have length of at least `depth`. */
			private def ensureDepth(depth :Int) :Unit =
				if (suffixes == null) {
					val length = math.max(depth, 4)
					suffixes   = new Array[Array[Any]](length)
					ranks      = new Array[Int](length)
				} else if (depth > suffixes.length) {
					val length = math.max(depth, suffixes.length << 1)
					suffixes   = Array.copyOf(suffixes, length)
					ranks      = Array.copyOf(ranks, length)
				}

			override def knownSize :Int = size

			override def clear() :Unit = {
				lvl0     = null
				suffixes = null
				ranks    = null
				rank0    = MaxChildren
				level    = 0
				size     = 0
			}
			override def result() :Fingers[E] = {
				val seq = toTree(level).toSeq
				clear()
				seq
			}

			/** Removes all suffix trees of level `maxLevel` and lower, and combines them into a single valid `Tree`.
			  * Resets ranks of `ranks(0..maxLevel - 1)` to zero, and `rank0` to either zero or `MaxChildren`
			  * (if set to `null`).
			  * @param maxLevel the maximum level of removed suffixes, where level 0 is `lvl0`,
			  *                 and level `i > 0` is `suffixes(i - 1)`.
			  */
			private def toTree(maxLevel :Int) :Tree[E] =
				if (level == 0 | maxLevel == 0) rank0 match {
					case 0           => emptyLeaf
					case MaxChildren =>
						val res = lvl0
						lvl0  = null
						if (res == null) emptyLeaf else res.asInstanceOf[Tree[E]]
					case rank        =>
						rank0 = 0
						lvl0.slice(0, 1 + rank).asInstanceOf[Tree[E]]
				} else {
					var first    = lvl0                           //The first tree node to append to the next level array.
					var second   = null :Array[Any]               //The second tree node to append to the next level array.
					var rank     = if (lvl0 == null) 0 else rank0 //The rank of `first`.
					var extra    = if (rank == 0) 0 else 1        //The number of nodes to append to the next level array.
					var extraLvl = 0
					var lvl      = 0
					val max      = math.min(level, maxLevel)
					if (rank0 < MaxChildren)
						rank0  = 0
					else
						lvl0 = null
					while (lvl < max) {
						val parentRank = ranks(lvl)
						if (extra > 0 && suffixes(lvl) == null)
							suffixes(lvl) = new Array[Any](Rank << 1)
						val parent     = suffixes(lvl)
						if (extra == 1 & rank < Rank) {                  //Balance first.
							if (parentRank > 0) {
								val sibling = parent(parentRank).asInstanceOf[Array[Any]]
								val siblingRank = sibling.length - 1
								if (siblingRank + rank <= MaxChildren) { //Merge first with the last child of parent.
									val newSibling = Array.copyOfRanges(
										sibling, 0, 1 + siblingRank, first, 1, 1 + rank
									)
									newSibling(0) = null
									parent(parentRank) = newSibling
									first = parent
									extra = 0
								} else { //Pad first to Rank with last children of the last child of parent.
									val newSiblingRank = siblingRank + rank - Rank
									val newSibling = sibling.slice(0, 1 + newSiblingRank)
									newSibling(0) = null
									parent(parentRank) = newSibling
									if (first.length < Rank + 1) //Possible for a result of tree.insert when extra = -1.
										first = Array.copyOf(first, Rank + 1)
									first(0) = null
									arraycopy(first, 1, first, 1 + Rank - rank, rank)
									arraycopy(sibling, 1 + newSiblingRank, first, 1, Rank - rank)
									rank = Rank
								}
							} else {     //The lower level tree has a too low rank, but its parent is empty.
								first = if (rank == MaxChildren) first else first.slice(0, rank + 1)
								if (lvl > 0 && first(0) == null)
									computeLengths(first.asInstanceOf[Tree[E]], lvl + 1)
								extraLvl = lvl + 1
								extra = -1
							}
						}
						if (parentRank <= MaxChildren - extra) {   //Insert first (and possibly second) to suffixes(lvl).
							extra match {
								case -1 => //first is a tree of a lower level than lvl-1 and can't be hung under parent.
									if (parentRank > 0) {       //Append first to parent immutable way.
										val tree = (
											if (parentRank == MaxChildren) parent else parent.slice(0, parentRank + 1)
										).asInstanceOf[Tree[E]]
										computeLengths(tree, lvl + 2)
										val siblings = tree.appendedAll(
											first.asInstanceOf[Tree[E]], lvl + 2, extraLvl, false, false
										)
										extra = siblings.size
										first = siblings.first.asInstanceOf[Array[Any]]
										rank  = first.length - 1
										if (extra == 2)
											second = siblings.second.asInstanceOf[Array[Any]]
									}
								case  0 =>
									first = parent
									rank  = parentRank
									extra = if (parentRank > 0) 1 else 0
								case  1 =>
									parent(parentRank + 1) =
										if (rank == first.length - 1) first
										else first.slice(0, 1 + rank)
									first = parent
									rank  = parentRank + 1
								case  2 =>
									parent(parentRank + 1) = first
									parent(parentRank + 2) = second
									first = parent
									rank  = parentRank + 2
									extra = 1
							}
							if (rank == MaxChildren)            //We have used suffixes(lvl) array.
								suffixes(lvl) = null
						} else {   //Split suffixes(lvl) into two nodes to accommodate first (and second, if necessary).
							val next = new Array[Any](1 + Rank)
							if (extra == 1)                     //extra > 0, because parentRank + extra > MaxChildren.
								next(Rank) = if (rank == MaxChildren) first else first.slice(0, 1 + rank)
							else {
								next(Rank - 1) = first
								next(Rank)     = second
							}
							arraycopy(parent, parentRank - (Rank - 1) + extra, next, 1, Rank - extra)
							first  = parent.slice(0, 1 + parentRank + extra - Rank)
							second = next
							extra  = 2
						}
						ranks(lvl) = 0
						lvl += 1
					}
					extra match {
						case -1 => first.asInstanceOf[Tree[E]]   //A ready tree of a lower level.
						case  0 => emptyLeaf                     //Ranks of everything below maxLevel were 0.
						case  1 =>                               //Return first as the tree root.
							val tree = (if (rank == MaxChildren) first else first.slice(0, 1 + rank)).asInstanceOf[Tree[E]]
							computeLengths(tree, lvl + 1)
							tree
						case  2 =>                               //Join the two carry over children under a new root.
							lvl += 1
							val parent = new Array[Any](3)
							parent(1) = first
							parent(2) = second
							computeLengths(parent.asInstanceOf[Tree[E]], lvl + 1)
							parent.asInstanceOf[Tree[E]]
					}
				}

			/** Appends the array of level `startLevel` as a child of array of level `startLevel + 1`,
			  * cascading recursively to higher level arrays when full.
			  * The array of level 0 is `lvl0`, and an array of level `n > 0` is `suffixes(n - 1)`.
			  */
			private def cascadeSuffixes(startLevel :Int) :Int = {
				var rank =
					if (startLevel == 0)
						if (lvl0 == null) 0 else rank0
					else
						ranks(startLevel - 1)
				if (startLevel > level | rank == 0)
					startLevel
				else {
					var node = null :Array[Any]
					var lvl  = startLevel
					if (startLevel == 0)
						node = rank0 match {
							case MaxChildren => lvl0
							case _           => lvl0.slice(0, 1 + rank0)
						}
					else
						node = rank match {
							case MaxChildren => suffixes(startLevel - 1)
							case _           => suffixes(startLevel - 1).slice(0, 1 + rank)
						}
					while (lvl < level && { rank = ranks(lvl); rank } == MaxChildren) {
						val parent = new Array[Any](Rank << 1)
						parent(1)  = node
						node       = suffixes(lvl)
						ranks(lvl) = 1
						suffixes(lvl) = parent
						lvl += 1
					}
					if (lvl < level) {             //rank < MaxChildren ==> append node as a child to suffixes(lvl).
						rank += 1
						ranks(lvl) = rank
						if (suffixes(lvl) == null)
							suffixes(lvl) = new Array[Any](Rank << 1)
						suffixes(lvl)(rank) = node
					} else {                       //lvl == level ==> grow the tree/array stack
						ensureDepth(lvl + 1)
						level += 1
						val parent = new Array[Any](Rank << 1)
						parent(1)  = node
						ranks(lvl) = 1
						suffixes(lvl) = parent
					}
					if (startLevel == 0) {
						if (rank0 == MaxChildren)  //Otherwise we have sliced the array and can reuse the old one.
							lvl0  = null
						else
							rank0 = 0
					} else {
						if (ranks(startLevel - 1) == MaxChildren)
							suffixes(startLevel - 1) = null
						ranks(startLevel - 1) = 0
					}
					lvl
				}
			}

			override def addOne(elem :E) :this.type = {
				if (rank0 == MaxChildren) {
					if (lvl0 != null)
						cascadeSuffixes(0)
					if (lvl0 == null) {
						lvl0  = new Array[Any](Rank << 1)
						rank0 = 0
					}
				}
				rank0 += 1
				size  += 1
				lvl0(rank0) = elem
				this
			}

			override def addAll(elems :IterableOnce[E]) :this.type = elems match {
				case _ if elems.knownSize == 0                 => this
				case view   :View[E]                           => addAll(view.iterator)
				case empty  :Iterable[E] if empty.isEmpty      => this
				case empty  :Iterator[E] if !empty.hasNext     => this
				case other  :AbstractFingers[E]                => addAll(other.toTree)
				case itr    :FingersIterator[E]                => addAll(itr.toTree)
				case ErasedArray.Slice(array, from, until)     => addAll(array, from, until - from)
				case single :Iterable[E] if single.sizeIs == 1 => addOne(single.head)
				case _ =>
					elems.toBasicOps.foldLeft[this.type](this)(_ addOne _)
			}

			@inline final def ++=(elems :Tree[E]) :this.type = addAll(elems)

			def addAll(elems :Tree[E]) :this.type =
				if (elems.isLeaf)
					if (elems.rank == 0) this
					else addAll(elems.asInstanceOf[Array[Any]], 1, elems.rank)
				else {
					var tree = elems
					if (rank0 < Rank) {
						if (rank0 > 0) {
							tree = elems.insertedSlice(0, new Children(lvl0), 0, rank0).toTree
							rank0 = 0
						}
					} else {
						if (lvl0 != null)
							cascadeSuffixes(0)
					}
					var lvl = 0
					while (lvl < math.min(level, tree.level - 2)) { //tree.level may grow.
						//Prepend arrays in suffixes of rank < Rank and level lower than tree as new subtrees to tree.
						val rank = ranks(lvl)
						if (rank < Rank) {
							if (rank > 0) {
								val suffix = toTree(lvl + 1) //Creates a Tree out of everything at or below lvl in suffixes.
								tree = tree.insertedAll(0, suffix)
							}
						} else
							cascadeSuffixes(lvl + 1)
						lvl += 1
					}
					val treeLevel = tree.level
					val treeRank  = tree.rank
					if (treeLevel - 2 < level) {                   //Implies lvl == treeLevel - 2.
						var rank = ranks(lvl)
						if (suffixes(lvl) == null)
							suffixes(lvl) = new Array[Any](Rank << 1)
						if (rank + treeRank <= MaxChildren) {      //Copy the children of the tree to suffixes(lvl).
							arraycopy(tree, 1, suffixes(lvl), 1 + rank, treeRank)
							ranks(lvl) = rank + treeRank
						} else {
							//Copy the children of tree to suffixes(lvl), cascading in the mean time.
							arraycopy(tree, 1, suffixes(lvl), 1 + rank, MaxChildren - rank)
							ranks(lvl) = MaxChildren               //For the purpose of cascadeSuffixes.
							cascadeSuffixes(lvl + 1)
							if (suffixes(lvl) == null)
								suffixes(lvl) = new Array[Any](Rank << 1)
							rank = rank + treeRank - MaxChildren
							ranks(lvl) = rank
							arraycopy(tree, 1 + treeRank - rank, suffixes(lvl), 1, rank)
						}
					} else { //level <= treeLevel - 2
						ensureDepth(treeLevel - 1)
						level = treeLevel - 1
						suffixes(treeLevel - 2) = new Array[Any](Rank << 1)
						arraycopy(tree, 1, suffixes(treeLevel - 2), 1, treeRank)
						ranks(treeLevel - 2) = treeRank
					}
					size += elems.length
					this
				}

			def addAll(array :Array[_], offset :Int, len :Int) :this.type = {
				if (lvl0 == null) {
					lvl0  = new Array[Any](Rank << 1)
					rank0 = 0
				}
				val max = math.min(len, math.max(0, array.length - offset))
				val lvl0Padding = math.min(MaxChildren - rank0, max)
				ArrayLike.copy(array, offset, lvl0, 1 + rank0, lvl0Padding)
				rank0 += lvl0Padding
				val until = offset + max - MaxChildren
				var i = offset + lvl0Padding
				while (i < until) {
					cascadeSuffixes(0)
					lvl0  = new Array[Any](Rank << 1)
					rank0 = MaxChildren
					ArrayLike.copy(array, i, lvl0, 1, MaxChildren)
					i += MaxChildren
				}
				if (i < offset + max) {
					cascadeSuffixes(0)
					lvl0  = new Array[Any](Rank << 1)
					rank0 = offset + max - i
					ArrayLike.copy(array, i, lvl0, 1, rank0)
				}
				size += max
				this
			}
		}



		private def binSearch(lengths :Array[Int], idx :Int) :Int = {
			val rank = lengths.length
			var lo = 0
			var hi = rank
			while (lo < hi) {
				val mid = lo + (hi - lo >>> 1)
				if (idx < lengths(mid)) hi = mid
				else lo = mid + 1
			}
			lo
		}

		def computeLengths(tree :Tree[_], level :Int) :Unit =
			if (level > 1) {
				val children = tree.array
				if (children(0) == null) {
					val rank = children.length
					var i = 1
					while (i < rank) {
						computeLengths(children(i).asInstanceOf[Tree[_]], level - 1)
						i += 1
					}
					updateLengths(new Children(children))
				}
			}

		def prefixLengthsOf(array :RefArrayLike[Tree[_]]) :Array[Int] = {
			val rank = array.length - 1
			val res  = new Array[Int](rank)
			var len  = 0
			var i    = 1
			while (i <= rank) {
				len += array(i).length
				res(i - 1) = len
				i += 1
			}
			res
		}

		@inline def updateLengths(newNode :Children[_]) :Unit =
			newNode.array(0) = prefixLengthsOf(newNode.array.asInstanceOf[RefArray[Tree[_]]])

		@inline def updateLengths(result :Children[_], oldLengths :Array[Int], unchangedPrefix :Int) :Unit =
			result.array(0) = updatedLengths(result.array.asInstanceOf[RefArray[Tree[_]]], oldLengths, unchangedPrefix)

		def updatedLengths(newChildren :RefArrayLike[Tree[_]], oldLengths :Array[Int], unchangedPrefix :Int) :Array[Int] = {
			val rank = newChildren.length - 1
			val res = Array.copyOf(oldLengths, rank)
			var total = if (unchangedPrefix == 0) 0 else oldLengths(unchangedPrefix - 1)
			var i = unchangedPrefix
			while (i < rank) {
				total += newChildren(i + 1).length
				res(i) = total
				i += 1
			}
			res
		}

		/** The minimum number of values in a leaf, or children in an inner node. */
		final val Rank = 16 //Our Tree works for values >= 4.

		/** The maximum number of children (or values for leaf nodes) in leaves and inner nodes.
		  * Equals. `MaxChildren - 2 == 2*Rank - 3`. */
		final val MaxChildren  = (Rank << 1) - 1

		/** The maximum number of children of an `infix` node in `Fingers2` and `FingersN`; equals `2*Rank - 1`. */
		final val MaxInfixRank = MaxChildren - 2

		/** The maximum number of children of for all children of `prefixes` and `suffixes` properties of `FingersN`.
		  * Equals `MaxChildren - 1 == 2*Rank - 2`. */
		final val MaxOuterRank = MaxChildren - 1
	}

}
