package net.noresttherein.sugar.collections

import java.lang.{Math => math}
import java.lang.System.arraycopy
import java.util.Arrays.copyOf

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.{nowarn, tailrec}
import scala.collection.generic.DefaultSerializable
import scala.collection.{AbstractIndexedSeqView, AbstractIterable, AbstractSeq, AbstractSet, Factory, IndexedSeqView, IterableFactory, IterableFactoryDefaults, LinearSeq, StrictOptimizedIterableOps, View, immutable, mutable}
import scala.collection.immutable.{ArraySeq, HashMap, HashSet, IndexedSeq, IndexedSeqDefaults, IndexedSeqOps, Set, StrictOptimizedSeqOps, StrictOptimizedSetOps}
import scala.collection.mutable.{Buffer, Builder, ReusableBuilder}
import scala.util.Random

import net.noresttherein.sugar.arrays.{IArray, IRefArray, RefArray, RefArrayLike}
import net.noresttherein.sugar.arrays.extensions.{ArrayCompanionExtension, ArrayExtension, ArrayLikeExtension, RefArrayExtension, RefArrayLikeExtension}
import net.noresttherein.sugar.collections.CompanionFactory.sourceCollectionFactory
import net.noresttherein.sugar.collections.IndexedIterable.{ApplyPreferred, HasFastAppend, HasFastPrepend, HasFastUpdate}
import net.noresttherein.sugar.collections.Ranking.RankingView
import net.noresttherein.sugar.collections.RankingImpl.{AppendingBuilder, DummyHashArray, IndexedSeqFactory, RankingBuilder, RankingSeqAdapter, RankingSerializer, RankingSetAdapter, ReverseBuilder, SmallRankingCap, UnorderedBuilder, deduplicateLarge, deduplicateSmall, hashCodeOf, smallContains}
import net.noresttherein.sugar.collections.extensions.{IterableExtension, IterableOnceExtension, IteratorExtension, SeqExtension, SeqFactoryExtension}
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.typist.casting.extensions.{castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{Yes, No}

//implicits




/** An interface of ordered collections of unique elements. Combines the features of a `Seq` with those of a `Set`,
  * but implements neither. The order of the elements is arbitrary, as in a `Seq`, and plays a role in equality
  * (which makes it different from [[collection.immutable.SeqMap SeqMap]], for example).
  *
  * Implementations of methods provided by this trait assume that the following operations are efficient
  * (at worst, `O(log n)`):
  *   1. [[net.noresttherein.sugar.collections.RankingOps.indexOf indexOf]]
  *   1. [[net.noresttherein.sugar.collections.RankingOps.apply apply]]
  *   1. [[net.noresttherein.sugar.collections.RankingOps.toIndexedSeq toIndexedSeq]] and
  *      [[net.noresttherein.sugar.collections.RankingOps.toSeq toSeq]]
  *   1. [[net.noresttherein.sugar.collections.RankingOps.size size]]
  *      (and that [[net.noresttherein.sugar.collections.Ranking.knownSize knownSize]] never returns a negative value)
  *   1. `iterator.drop` and `iterator.take`
  *
  * Furthermore, it assumes that both `newSpecificBuilder` and `iterableFactory.newBuilder` create $Coll instances
  * according to addition order, and all elements equal to an already added element are ignored
  * (that is, the builder is consistent with [[net.noresttherein.sugar.collections.RankingOps.++ ++]]).
  * The implementations make an effort to adding a $Coll as the first argument to the builder, in a hope that
  * it may be able to reuse its internal structure as a starting point.
  *
  * These are not strict requirements for all extending classes, only assumptions for the default implementation.
  * The standard [[net.noresttherein.sugar.collections.Ranking Ranking]], however, does in fact offer these guarantees.
  *
  * The users should assume that any operation which changes the order of elements in a $Coll (even swapping
  * positions of two elements) are of `O(n)` complexity.
  * @tparam E  the element type
  * @tparam CC type constructor of the collection (e.g. `List`, `Set`). Operations returning a collection
  *            with a different type of element `B` (e.g. `map`) return a `CC[B]`.
  * @tparam C  type of the collection (e.g. `List[Int]`, `String`, `BitSet`). Operations returning a collection
  *            with the same type of element (e.g. `drop`, `filter`) return a `C`.
  * @define Coll `Ranking`
  * @define coll ranking
  */
trait RankingOps[+E, +CC[+X] <: IterableOnce[X], +C <: CC[E]] extends SugaredIterableOps[E, CC, C] {
	/** Same as `size`. */
	def length :Int = size

	/** Returns sequence `[0, this.size-1]`. */
	def indices :Range = 0 until size


	/** The `n`-th element in this $coll.
	  * @param index an index in the `[0..size-1]` range.
	  */
	def apply(index :Int) :E


	/** True if `that sameElements this.slice(offset, offset + that.size)`. */
	def startsWith[B >: E](that: IterableOnce[B], offset: Int = 0): Boolean = that match {
		case _ if offset < 0 || offset > size => false
		case empty :Iterable[_] if empty.isEmpty => true
		case _ if { val size = that.knownSize; size >= 0 && offset > this.size - size } => false
		case _ if applyPreferred =>
			val it = that.iterator
			var i = math.max(offset, 0)
			val end = size
			var matches = true
			while (i < end && it.hasNext && { matches = apply(i) == it.next(); matches })
				i += 1
			!it.hasNext && matches
		case _ =>
			val thisIt = this.iterator.drop(offset)
			val thatIt = that.iterator
			while (thisIt.hasNext && thatIt.hasNext)
				if (thisIt.next() != thatIt.next())
					return false
			!thatIt.hasNext
	}

	/** True if `that sameElements this.takeRight(that.size)`. */
	def endsWith[B >: E](that :Iterable[B]) :Boolean = startsWith(that, size - that.size)

	/** Computes the length of the longest segment that starts from the first element
	  * and whose elements all satisfy some predicate.
	  * @param   p     the predicate used to test elements.
	  * @param   from  the index where the search starts.
	  * @return the length of the longest segment of this $coll that starts from the first element
	  *         such that every element of the segment satisfies the predicate `p`.
	  */
	def segmentLength(p :E => Boolean, from :Int = 0) :Int = indexWhere(p, from, true) match {
		case -1 => size
		case  n => n
	}

	/** Finds index of the first element satisfying some predicate after or at some start index.
	  * Unlike [[net.noresttherein.sugar.collections.RankingOps.indexOf indexOf]], this method works in linear time.
	  * @param   p      the predicate used to test elements.
	  * @param   from   the start index
	  * @return the index `>= from` of the first element of this $coll that satisfies the predicate `p`,
	  *         or `-1`, if none exists.
	  */
	def indexWhere(p :E => Boolean, from :Int = 0) :Int = indexWhere(p, from, false)

	protected def indexWhere(p :E => Boolean, from :Int, flipped :Boolean) :Int =
		if (applyPreferred) {
			val end = size
			var i   = math.max(from, 0)
			while (i < end && p(apply(i)) == flipped)
				i += 1
			i - from
		} else {
			val i   = iterator.drop(from)
			var res = 0
			while (i.hasNext && p(i.next()) == flipped)
				res += 1
			res
		}
	//todo: remove this method and make SeqExtension work for anything SeqLike type class
	/** Finds an element of this $coll at or after index `from` which satisfies the predicate,
	  * returning its index as an option. Unlike [[net.noresttherein.sugar.collections.RankingOps.getIndexOf getIndexOf]],
	  * this method works in linear time.
	  * @param p    a function applied consecutively to all elements with indices greater or equal `from`.
	  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
	  */
	def getIndexWhere(p :E => Boolean, from :Int = 0) :Maybe[Int] = indexWhere(p, from) match {
		case -1 => No
		case  n => Yes(n)
	}

	/** Finds an element of this $coll at or after index `from` which satisfies the predicate,
	  * throwing a [[NoSuchElementException]] if it does not exist.
	  * Unlike [[net.noresttherein.sugar.collections.RankingOps.sureIndexOf sureIndexOf]],
	  * this method works in linear time.
	  * @param p    a function applied consecutively to all elements with indices greater or equal `from`,
	  *             until satisfied.
	  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
	  */
	def sureIndexWhere(p :E => Boolean, from :Int = 0) :Int = indexWhere(p, from) match {
		case -1 =>
			throw new NoSuchElementException(
				if (from == 0) "No element satisfying the predicate " + p + " in " + this + "."
				else "No element satisfying the predicate " + p + " in " + this + " at or after " + from + "."
		)
		case  n => n
	}

	/** Finds the last element at or before index `end` satisfying the predicate.
	  * Unlike [[net.noresttherein.sugar.collections.RankingOps.indexOf indexOf]],
	  * this method works in linear time.
	  * @param p   a function applied consecutively to all elements with indices lesser or equal `end`.
	  * @param end an inclusive upper bound for the element's index.
	  */
	def lastIndexWhere(p :E => Boolean, end :Int = Int.MaxValue) :Int = {
		var i = math.min(length - 1, end)
		while (i >= 0 && !p(apply(i)))
			i -= 1
		i
	}

	/** Finds the last element at or before index `end` satisfying the predicate.
	  * Unlike [[net.noresttherein.sugar.collections.RankingOps.getIndexOf getIndexOf]],
	  * this method works in linear time.
	  * @param p   a function applied consecutively to all elements with indices lesser or equal `end`.
	  * @param end an inclusive upper bound for the element's index,
	  * @return the maximum number `n <= end` such that `p(this(n))`,
	  *         or `No` if no element in this $coll's index range `[0..end]` satisfies the predicate.
	  */
	def getLastIndexWhere(p :E => Boolean, end :Int = Int.MaxValue) :Maybe[Int] = {
		val res = lastIndexWhere(p, end)
		if (res == -1) No else Yes(res)
	}

	/** Finds the last element at or before index `end` satisfying the predicate.
	  * Unlike [[net.noresttherein.sugar.collections.RankingOps.sureIndexOf sureIndexOf]],
	  * this method works in linear time.
	  * @param p   a function applied consecutively to all elements with indices lesser or equal `end`.
	  * @param end an inclusive upper bound for the element's index.
	  * @throws NoSuchElementException if no element in this $coll's index range `[0..end]` satisfies the predicate.
	  */
	def sureLastIndexWhere(p :E => Boolean, end :Int = Int.MaxValue) :Int = lastIndexWhere(p, end) match {
		case -1 =>
			throw new NoSuchElementException(
				"No element satisfying the predicate " + p + " in " + this + " at or before " + end + "."
			)
		case res => res
	}

	/** The index of the given element in this $coll, or `-1` if it does not contain `elem`. */
	def indexOf[U >: E](elem :U) :Int

	/** Same as the standard [[net.noresttherein.sugar.collections.RankingOps.indexOf indexOf]],
	  * but returns [[net.noresttherein.sugar.vars.Maybe.No No]] instead of returning `-1` if no such element exists.
	  */
	def getIndexOf[U >: E](elem :U) :Maybe[Int] = indexOf(elem) match {
		case -1 => No
		case n => Yes(n)
	}

	/** Same as the standard [[net.noresttherein.sugar.collections.RankingOps.indexOf indexOf]],
	  * but throws a [[NoSuchElementException]] if the element is not found in this collection.
	  * The exception's message lists all the contents, so this method should not be used if this collection
	  * can be large or this might pose a security vulnerability, but the extra information is much more helpful
	  * than in an [[IndexOutOfBoundsException]] which most likely would be caused by returning `-1`.
	  */
	def sureIndexOf[U >: E](elem :U) :Int = indexOf(elem) match {
		case -1 => throw new NoSuchElementException("No " + elem + " in " + this + ".")
		case n => n
	}

	/** Finds first index after or at a start index where this $coll contains a given sequence as a slice.
	  * Thanks to the uniqueness of elements in a $coll and fast
	  * [[net.noresttherein.sugar.collections.RankingOps.indexOf indexOf]], this method runs in O(that.size).
	  * @param  that    the sequence to test
	  * @param  from    the start index
	  * @return the first index `>= from` such that the elements of this $coll starting at this index
	  *         match the elements of sequence `that`, or `-1` if no such subsequence exists.
	  */
    def indexOfSlice[U >: E](that :collection.Seq[U], from :Int = 0) :Int =
	    if (that.isEmpty)
		    if (from < 0) 0
	        else if (from > size) -1
		    else from
		else
		    indexOf(that.head) match {
			    case n if n >= from && startsWith(that, n) => n
			    case _ => -1
		    }

	/** Finds the location of the given subsequence in this $coll, returning its index as an `Maybe`.
	  * Thanks to the uniqueness of elements in a $coll and fast
	  * [[net.noresttherein.sugar.collections.RankingOps.indexOf indexOf]], this method runs in O(that.size).
	  * @param that a presumed consecutive subsequence of this sequence.
	  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
	  * @return `Maybe(this.indexOfSlice(that, from)).filter(_ >= 0)`.
	  */ //todo: use Option, consistently with SeqExtension, but add also a variant for IntOpt.
	def getIndexOfSlice[U >: E](that :collection.Seq[U], from :Int = 0) :Maybe[Int] =
		indexOfSlice(that, from) match {
			case -1 => No
			case  n => Yes(n)
		}

	/** Finds the location of the given subsequence in this $coll, throwing a [[NoSuchElementException]]
	  * if it does not exist. Because of the uniqueness of elements in a $coll and fast
	  * [[net.noresttherein.sugar.collections.RankingOps.indexOf indexOf]], this method runs in O(that.size).
	  * @param that a presumed consecutive subsequence of this sequence.
	  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
	  * @return `this.indexOfSlice(that, from)`.
	  */
	def sureIndexOfSlice[U >: E](that :collection.Seq[U], from :Int = 0) :Int =
		indexOfSlice(that, from) match {
			case -1 => throw new NoSuchElementException(
				that.toSeq.toString + " does not occur in " + this + " at or after index " + from + "."
			)
			case  n => n
		}

	/** Tests whether this $coll contains a given sequence as a slice.
	  * @param  that    the sequence to test
	  * @return `true` if this $coll contains a slice with the same elements as `that`, otherwise `false`.
	  */
	final def containsSlice[U >: E](that :Seq[U]) :Boolean = indexOfSlice(that) != -1

	/** Checks if this $coll contains the given element as defined by `equals`. */
	def contains[U >: E](elem :U) :Boolean = indexOf(elem) >= 0

	/** Checks if this $coll contains all the given elements, in an unspecified order. */
	def containsAll[U >: E](elems :IterableOnce[U]) :Boolean = elems match {
		case rank  :Ranking[U] if rank.size < size => false
		case set   :collection.Set[_] if { val n = set.knownSize; n >= 0 && n < size } => false
		case items :Iterable[U] => items.isEmpty || items.forall(contains)
		case _ => elems.iterator.forall(contains)
	}

	/** Finds the first element in this collection satisfying the predicate. */
	override def find(p :E => Boolean) :Option[E] = indexWhere(p) match {
		case n if n >= 0 => Some(apply(n))
		case _           => None
	}
	/** Finds the last element in this collection satisfying the predicate. */
	def findLast(p :E => Boolean) :Option[E] = lastIndexWhere(p) match {
		case n if n >= 0 => Some(apply(n))
		case _           => None
	}


	/** Swaps the places of elements at indices `idx1` and `idx2`. */
	@throws[IndexOutOfBoundsException]("if either idx1 or idx2 are outside the valid index range for this collection.")
	def swapped(idx1 :Int, idx2 :Int) :C =
		if (idx1 > idx2)
			swapped(idx2, idx1)
		else if (idx1 < 0 | idx2 < 0 | { val n = size; idx1 >= n | idx2 >= n })
			throw new IndexOutOfBoundsException(errorString(this) + ".swapped(" + idx1 + ", " + idx2 + ")")
		else if (idx1 == idx2)
			coll
		else {
			val res = newSpecificBuilder
			res sizeHint size
			res ++= iterator.take(idx1)
			res += this(idx2)
			res ++= iterator.slice(idx1 + 1, idx2)
			res += this(idx1)
			res ++= iterator.drop(idx2 + 1)
			res.result()
		}

	/** Swaps the places of elements at indices `[range.start, range.last]` and `[start, start + range.length)`. */
	@throws[IllegalArgumentException]("if range.step does not equal 1.")
	@throws[IndexOutOfBoundsException]("if either range.head or range.last is lesser than zero or greater or equal " +
	                                   "to size, or start is lesser than zero or greater than size - range.length.")
	final def swapped(range :Range, dst :Int) :C = {
		if (range.step != 1)
			throw new IllegalArgumentException(
				"Range " + range + " is not a valid index range for " + errorString(this) + "."
			)
		swapped(range.head, dst, range.length)
	}

	/** Swaps the places of elements at indices `[idx1, idx1 + length)` and `[idx2, idx2 + length)`. */
	@throws[IndexOutOfBoundsException]("if either src or dst is lesser than zero or greater than this.size - length.")
	@throws[IllegalArgumentException]("if ranges [idx1, idx1 + length) and [idx2, idx2 + length) overlap.")
	def swapped(idx1 :Int, idx2 :Int, length :Int) :C = {
		val size = this.size
		def swap(src :Int, dst :Int, length :Int) :C = {
			val length0 = math.max(length, 0)
			if (src < 0 | dst > size - length0)
				throw new IndexOutOfBoundsException(
					errorString(this) + ".swapped(" + src + ", " + dst + ", " + length + ")"
				)
			if (src == dst | length <= 0)
				coll
			else if (length == 1)
				swapped(src, dst)
			else if (src > dst - length0)
				throw new IllegalArgumentException(
					errorString(this) + ".swapped: swapped ranges [" + src + ", " + (src + length) + ") and [" +
						dst + ", " + (dst + length) + ") overlap."
				)
			else {
				val res = newSpecificBuilder
				res sizeHint size
				res ++= iterator.take(src)
				res ++= iterator.slice(dst, dst + length)
				if (src + length < dst)
					res ++= iterator.slice(src + length, dst)
				res ++= iterator.slice(src, src + length)
				res ++= iterator.drop(dst + length)
				res.result()
			}
		}
		if (idx1 < idx2)
			swap(idx1, idx2, length)
		else
			swap(idx2, idx1, length)
	}


	/** Moves the element at position `idx1` to position `idx2`. All other elements remain in the same relative order,
	  * shifting positions in this $coll accordingly.
	  * @param src the index of the moved element.
	  * @param dst the index of the specified element in the result.
	  * @return a $Coll equal to
	  *         {{{
	  *             (this.removed(src).take(dst) :+ this(src) :++ this.removed(src).drop(dst)
	  *         }}}
	  * @see [[net.noresttherein.sugar.collctions.RankingOps.swapped(idx1:Int,idx2:Int) swapped]]
	  */
	@throws[IndexOutOfBoundsException]("if either src or dst is lesser than zero or greater or equal to this collection's size.")
	def moved(src :Int, dst :Int) :C = moved(src, dst, 1)

	/** Moves the elements in the specified range to new positions, starting with `dst`.
	  * The element `this(range.head)` is moved to position `dst`, element `this(range.head + 1)` to `dst + 1`,
	  * and so on. All other elements remain in the same relative order, changing positions where necessary.
	  * @return a $Coll equal to
	  *         {{{
	  *             (this -- slice(range.head, range.last + 1)).take(dst) ++
	  *                 slice(range.head, range.last + 1) ++
	  *                 (this -- slice(range.head, range.last + 1)).drop(dst)
	  *         }}}
	  * @see [[net.noresttherein.sugar.collections.RankingOps.swapped(range:Range,dst:Int) swapped]]
	  */
	@throws[IllegalArgumentException]("if range.step does not equal one.")
	@throws[IndexOutOfBoundsException]("if either range.start or dst is outside index range [0..this.size - range.length).")
	final def moved(range :Range, dst :Int) :C = {
		if (range.step != 1)
			throw new IllegalArgumentException(
				"Range " + range + " is not a valid index range for " + errorString(this) + "."
			)
		moved(range.head, dst, range.length)
	}

	/** Moves `length` elements following index `src` to new positions, starting with `dst`.
	  * The element `this(src)` is moved to position `dst`, element `this(src + 1)` to `dst + 1`,
	  * and so on. All other elements remain in the same relative order, changing positions where necessary.
	  * Elements `slice(src, src + length)` will occupy indices `[dst, dst + length)` in the returned $coll.
	  * Note that this means that `dst` is ''not'' interpreted regarding to ''this'' coll: i.e., element `this(dst)`
	  * will likely not follow the moved range in the returned collection.
	  *
	  *   1. If `src == dst` or `length <= 0`, then this instance is returned.
	  *   1. If `src < dst`, thn the result is the same as for
	  *      [[net.noresttherein.sugar.collections.RankingOps.rotatedRight rotatedRight]]`(src, dst+length)(dst - src)`.
	  *   1. If `dst < src`, then the result is the same as for
	  *      [[net.noresttherein.sugar.collections.RankingOps.rotatedLeft rotatedLeft]]`(dst, src+length)(src - dst)`.
	  * @return a $Coll equal to
	  *          {{{
	  *             (this -- slice(src, src + length)).take(dst) ++
	  *                 slice(src, src + length) ++
	  *                 (this -- slice(src, src + length)).drop(dst)
	  *          }}}
	  * @see [[net.noresttherein.sugar.collctions.RankingOps.swapped(src:Int,dst:Int,length:Int) swapped]]
	  */
	@throws[IndexOutOfBoundsException]("if either src or dst is outside index range [0..this.size).")
	def moved(src :Int, dst :Int, length :Int) :C = {
		val length0 = math.max(length, 0)
		if (dst < 0 | dst > size - length0 | src < 0 | src > size - length0)
			throw new IndexOutOfBoundsException(
				"Cannot move elements of " + errorString(this) + " at positions [" + src + ".." +
					(src + length) + ") to positions [" + dst + ", " + (dst + length) + ")."
			)
		if (src == dst | length0 == 0)
			coll
		else if (src < dst)
			rotatedRight(src, dst + length0)(dst - src)
		else
			rotatedLeft(dst, src + length0)(src - dst)
	}

	private def move[U >: E](src :Int, dst :Int, elem :U) :CC[U] = {
		val res = iterableFactory.newBuilder[U]
		res sizeHint size
		if (src > dst)
			(res ++= take(dst) += elem ++= iterator.drop(dst).removed(src - dst)).result()
		else {
			res ++= take(src)
			var i  = src + 1
			val it = iterator.drop(i)
			while (i < dst) {
				res += it.next()
				i += 1
			}
			res += elem ++= it
			res.result()
		}
	}


	/** Shifts left all elements in this $coll by `n` modulo `this.size`.
	  * @return `this` if `this.size <= 1`, or a $Coll equal to `drop(r) ++ take(r)`,
	  *         where `r == if (n >= 0) n % this.size else (n + (n % this.size)) % this.size`.
	  */
	def rotatedLeft(n :Int) :C = rotatedLeft(0, size)(n)

	/** Rotates left the elements in index range `[from, until)` by `n` positions.
	  * If `from` or `until` is lesser than zero, or greater than `this.size`, it is clipped to range `0..this.size`;
	  * let the clipped values be `from0` and `until0`. If `until0 <= from0 + 1` then `this` is returned.
	  * Otherwise, let `r` be the non negative remainder of dividing `n` by `until0 - from0`.
	  * The returned $coll equals
	  * {{{
	  *     take(from) ++ slice(from + r, until) ++ slice(from, from + r) ++ drop(until)
	  * }}}
	  */
	def rotatedLeft(from :Int, until :Int)(n :Int) :C =
		rotatedLeftImpl(from, until)(n) { (n, length) => if (n >= 0) n % length else length + n % length }

	/** Shifts right all elements in this $coll by `n` modulo `this.size`.
	  * @return `this` if `this.size <= 1`, or a $Coll equal to `takeRight(r) ++ dropRight(r)`,
	  *         where `r == if (n >= 0) n % this.size else (n + (n % this.size)) % this.size`.
	  */
	def rotatedRight(n :Int) :C = rotatedRight(0, size)(n)

	/** Rotates right the elements in index range `[from, until)` by `n` positions.
	  * If `from` or `until` is lesser than zero, or greater than `this.size`, it is clipped to range `0..this.size`;
	  * let the clipped values be `from0` and `until0`. If `until0 <= from0 + 1` then `this` is returned.
	  * Otherwise, let `r` be the non negative remainder of dividing `n` by `until0 - from0`.
	  * The returned $coll equals
	  * {{{
	  *     take(from) ++ slice(until - r, until) ++ slice(from, until - r) ++ drop(until)
	  * }}}
	  */
	def rotatedRight(from :Int, until :Int)(n :Int) :C =
		rotatedLeftImpl(from, until)(n) { (n, length) => if (n >= 0) length - n % length else -(n % length) }

	private def rotatedLeftImpl(from :Int, until :Int)(n :Int)(split :(Int, Int) => Int) :C = {
		val from0  = math.max(from, 0)
		val until0 = math.min(until, size)
		val length = until0 - from0
		if (length <= 1 || n % length == 0)
			coll
		else
			trustedRotatedLeft(from0, until0)(split(n, length))
	}

	/** Implementation for both `rotatedLeft` and `rotatedRight`. Semantics as in `rotatedLeft`,
	  * but assumes all arguments are validated/truncated.
	  */
	protected def trustedRotatedLeft(from :Int, until :Int)(n :Int) :C = {
		val size = this.size
		val res = newSpecificBuilder
		res sizeHint size
		if (from > 0)
			res ++= iterator.take(from)
		res ++= iterator.slice(from + n, until) ++= iterator.slice(from, from + n)
		if (until < size)
			res ++= iterator.drop(until)
		res.result()
	}

	private def replaceWithAbsent[U >: E](index :Int, elem :U) = {
		val res = iterableFactory.newBuilder[U]
		res sizeHint size
		(res ++= take(index) += elem ++= iterator.drop(index + 1)).result()
	}


	/** Creates a $coll with the same contents and order as this instance, but with `elem` at index `index`.
	  * Note that if `elem` is already present in this $coll, it will be moved to position `index`,
	  * and no value will be removed from the collection (the set of all elements will remain unchanged).
	  * Otherwise, the current value at position `index` is replaced with the new element.
	  * @param index the index at which the element must be placed in the result.
	  * @param elem  the added element.
	  * @return a $Coll equal to `(this - elem).take(index) :+ elem :++ (this - elem).takeRight(this.size - index - 1)`.
	  * @see [[net.noresttherein.sugar.collections.RankingOps.replace replace]]
	  * @see [[net.noresttherein.sugar.collections.RankingOps.inserted inserted]]
	  */
	@throws[IndexOutOfBoundsException]("if index < 0 || index >= size")
	def updated[U >: E](index :Int, elem :U) :CC[U] = indexOf(elem) match {
		case old if old == index =>
			iterableFactory.from(this)
		case _ if index < 0 | index >= size =>
			throw new IndexOutOfBoundsException(errorString(this) + ".updated(" + index + ", _)")
		case -1 =>
			replaceWithAbsent(index, elem)
		case old =>
			move(old, index, elem)
	}

	/** Creates a $coll by inserting or moving all elements of elems to positions following `index`.
	  * If `elems` does not contain any elements of this collection, the returned ranking will be a concatenation
	  * of the first `index` elements of `this`, `elems`, and the last `size - index - elems.toSet.size` of `this`.
	  * However, if the intersection of this collection and the argument is non empty, all common elements are moved
	  * from their positions in this $coll to positions defined by their first occurrences in `elems`.
	  * If `elems` contains repetitions, all occurrences of an element after the first one are ignored.
	  * For each element in `elems` not present in `this`, one unique element of `this` is removed.
	  * The returned $Coll will be of the same size as `this`, and contain
	  * `elems.iterator.`[[net.noresttherein.sugar.collections.extensions.IteratorExtension.distinct distinct]]
	  * starting at position `index`.
	  *
	  * Note that this method is not equivalent with recursive
	  * calling of [[net.noresttherein.sugar.collections.RankingOps.updated updated]] with increasing indices,
	  * because in the latter case updating a subsequent position with an element present before `index` would cause
	  * the previous `elems` values to shift left. This method performs the calculation globally for the whole collection.
	  * @return a $Coll equal to `(this -- elems).take(index) :++ Ranking.from(elems) :++ (this -- elems).takeRight(size - index - elems.toSet.size)`
	  */
	@throws[IndexOutOfBoundsException]("if index < 0 || index + elems.toSet.size > size")
	def updatedAll[U >: E](index :Int, elems :IterableOnce[U]) :CC[U]


	/** Replaces the `index`-th element in this $coll with `elem`. Note that, if `elem` is already present
	  * in this $coll at an earlier position than `index`, it will appear in the result at `index - 1`, not `index`.
	  * In either case, the current value at position `index` is removed from the collection.
	  * @param index the index of the element to replace.
	  * @param elem  the added element.
	  * @return a $Coll equal to `take(index) - elem :+ elem :++ (drop(index + 1) - elem)`.
	  * @see [[net.noresttherein.sugar.collections.RankingOps.updated updated]]
	  * @see [[net.noresttherein.sugar.collections.RankingOps.inserted inserted]]
	  */
	@throws[IndexOutOfBoundsException]("if index is negative, greater or equal to this.size")
	def replace[U >: E](index :Int, elem :U) :CC[U] = indexOf(elem) match {
		case _ if index < 0 | index >= this.size =>
			throw new IndexOutOfBoundsException(errorString(this) + ".replaced(" + index + ", _)")
		case -1 =>
			replaceWithAbsent(index, elem)
		case old if old == index =>
			iterableFactory.from(this)
		case old if old > index =>
			val res = iterableFactory.newBuilder[U]
			res sizeHint size - 1
			(res ++= take(index) += elem ++= iterator.drop(index + 1).removed(old - index - 1)).result()
		case old =>
			val res = iterableFactory.newBuilder[U]
			res sizeHint size - 1
			res ++= take(old)
			var i  = old + 1
			val it = iterator.drop(i)
			while (i < index) {
				res += it.next()
				i += 1
			}
			it.next()
			(res += elem ++= it).result()
	}

	/** Creates a $coll by replacing elements at `[index..index+elems.toSet.size)` with `elems`.
	  * If `elems` does not contain any elements of this collection, the returned ranking will be a concatenation
	  * of the first `index` elements of `this`, `elems`, and the last `size - index - elems.toSet.size` of `this`.
	  * However, if either the prefix or the suffix coming from this collection contains any elements present in `elems`,
	  * they are removed.
	  *
	  * This means that, unlike in
	  * `this.`[[net.noresttherein.sugar.collections.RankingOps.updatedAll updatedAll]]`(index, elems)`,
	  * `elems.head` may end up in a position lower than `index`. The difference between these two methods
	  * is that the former focuses on placing the patch at the given index, while this method is defined in terms
	  * of removing/replacing a specific sequence of elements from this collection.
	  * @return a $Coll equal to `take(index) -- elems :++ Ranking.from(elems) :++ (drop(index + elems.toSet.size) -- elems)`
	  */
	@throws[IndexOutOfBoundsException]("if index < 0 || index + elems.toSet.size > size")
	def replaceAll[U >: E](index :Int, elems :IterableOnce[U]) :CC[U]


	/** A copy of this $coll containing `elem` at position `index`. If this $coll does not currently contain `value`,
	  * it is inserted between elements `[0..index)` and `[index..size)`. Otherwise, the value is moved
	  * to the specified index, and the set of elements in this collection remains the same.
	  * @param index the index at which the inserted element should be present in the result.
	  * @param elem  the added element.
	  * @return a $Coll equal to `(this - elem).take(index) :+ elem :++ (this - elem).drop(index)`.
	  */
	def inserted[U >: E](index :Int, elem :U) :CC[U] =
		if (index <= 0)
			prepended(elem)
		else
			indexOf(elem) match {
				case -1 =>
					val size = this.size
					if (index >= size)
						appended(elem)
					else {
						val res = iterableFactory.newBuilder[U]
						res sizeHint size + 1
						res ++= take(index) += elem ++= iterator.drop(index)
						res.result()
					}
				case n if n == index =>
					iterableFactory.from(this)
				case old =>
					val to = math.min(size - 1, index)
					move(old, to, elem)
			}

	/** A copy of this $coll containing `elems` at position `index`. Values are inserted according to iteration order,
	  * with duplicates being resolved in favour of the last occurrence. If `elems` contains an element `e`
	  * at position `i` (after filtering out duplicates), which is in this ranking, it is put at position `index + i`
	  * in the result. Note that this is not equivalent to recursive calling
	  * of [[net.noresttherein.sugar.collections.RankingOps.inserted inserted]], as the insertion position is calculated
	  * once, in regard to all the elements in the collection, rather than for each element in `elems` independently.
	  * @param index an index in the `[0..size-1]` range.
	  * @param elems the added elements.
	  * @return a $Coll equal to `(this -- elems).take(index) ++ elems ++ (this -- elems).drop(index)`.
	  */ //consider: there is a small inconsistency in that updatedAll picks the first occurrence, not last.
	def insertedAll[U >: E](index :Int, elems :IterableOnce[U]) :CC[U]

	/** Same as `this.toSeq.patch(index, other, replaced) to $Coll` */
	def patch[U >: E](index :Int, other :IterableOnce[U], replaced :Int) :CC[U] =
		iterator.patch(index, other.iterator, replaced) to iterableFactory


	/** Adds an element to the $coll, if it is not already present. Unlike in other methods,
	  * position of `elem` in the result is unspecified. The only guarantee is that the order of other elements
	  * in this ranking is not changed. The difference from [[net.noresttherein.sugar.collections.Ranking.+| +|]],
	  * [[net.noresttherein.sugar.collections.Ranking.:+ :+]] and [[net.noresttherein.sugar.collections.Ranking.+: +:]]
	  * is that the element can be added at the front or at the back and, and may not change its position
	  * in the collection if it is already present. These loose restrictions allow for some additional optimizations,
	  * although the worst case complexity remains the same.
	  * @return `this` ''iff'' it already contains `elem`, or a `r :`$Coll, such that `r.contains(elem)` and
	  *         `r - elem == this`.
	  */
	@inline final def +|[U >: E](elem :U) :CC[U] = incl(elem)
	def incl[U >: E](elem :U) :CC[U] = added(elem)

	/** Adds an element to the $coll, if it is not already present.
	  * The difference from [[net.noresttherein.sugar.collections.RankingOps.+: +:]] and
	  * [[net.noresttherein.sugar.collections.RankingOps.:+ :+]] is that the element does not change its position
	  * in the collection if it is already present. This makes the method more efficient in that case than both
	  * [[net.noresttherein.sugar.collections.Ranking.:+ :+]] and [[net.noresttherein.sugar.collections.Ranking.+: +:]].
	  * @see [[net.noresttherein.sugar.collections.Ranking.+| +|]]
	  * @return `this` ''iff'' it already contains `elem`, or a $Coll instance containing all elements of `this`
	  *        followed by `elem` if it does not.
	  */
	@inline final def +[U >: E](elem :U) :CC[U] = this added elem
	def added[U >: E](elem :U) :CC[U] =
		if (isEmpty)
			iterableFactory.from(elem::Nil)
		else if (contains(elem))
			coll
		else {
			val res = iterableFactory.newBuilder[U]
			res sizeHint size + 1
			(res ++= this += elem).result()
		}

	/** Prepends an element to the front of the $coll. If the element is already present, it is moved to the front
	  * of the collection, with the preceding elements shift back one place.
	  * This method will be in general slower than [[net.noresttherein.sugar.collections.RankingOps.+ +]]
	  * and [[net.noresttherein.sugar.collections.RankingOps.:+ :+]].
	  * @see [[net.noresttherein.sugar.collections.Ranking.+| +|]]
	  * @see [[net.noresttherein.sugar.collections.Ranking.+ +]]
	  * @see [[net.noresttherein.sugar.collections.Ranking.:+ :+]]
	  */
	@inline final def +:[U >: E](elem :U) :CC[U] = this prepended elem
	def prepended[U >: E](elem :U) :CC[U] =
		if (isEmpty)
			iterableFactory.from(elem::Nil)
		else if (head == elem)
			coll
		else {
			val res = iterableFactory.newBuilder[U]
			res sizeHint (if (contains(elem)) size else size + 1)
			(res += elem ++= this).result()
		}

	/** Appends an element at the end of the $coll. If the element is already present, it is moved to the back
	  * of the collection, with the following elements shift forward one place.
	  * This method will be slower than [[net.noresttherein.sugar.collections.RankingOps.+ +]] if this ranking already
	  * contains the element.
	  * @see [[net.noresttherein.sugar.collections.Ranking.+| +|]]
	  * @see [[net.noresttherein.sugar.collections.Ranking.+ +]]
	  * @see [[net.noresttherein.sugar.collections.Ranking.+: +:]]
	  */
	@inline final def :+[U >: E](elem :U) :CC[U] = this appended elem
	def appended[U >: E](elem :U) :CC[U]

	/** Adds the given elements to this $coll. The total relative order of elements,
	  * precisely which occurrences of duplicates are kept, is unspecified. The only guarantees are
	  *   1. `(this +|+ elems to Set) == this.toSet | elems.toSet`,
	  *   1. all elements of `this -- elems` will be before all elements of `elems -- this`,
	  *   1. the relative order within `this -- elems` and `elems -- this` is preserved, and
	  *   1. there is a function `f :Int => Boolean`, such that
	  *      `(0 until this.size + elems.size).filter(f).map(this.toSeq ++ elems.toSeq) == (this +|+ elems)`.
	  *
	  * In the result, for some collection pairs, this method will work faster than its alternatives
	  * with stricter guarantees.
	  * @see [[net.noresttherein.sugar.collections.Ranking.++ ++]]
	  * @see [[net.noresttherein.sugar.collections.Ranking.:++ :++]]
	  * @see [[net.noresttherein.sugar.collections.Ranking.++: ++:]]
	  */
	@inline final def +|+[U >: E](elems :IterableOnce[U]) :CC[U] = union(elems)
	def union[U >: E](elems :IterableOnce[U]) :CC[U] = concat(elems)

	/** Adds the given elements to this $coll. Any elements already present in this ranking are ignored,
	  * and so are any duplicates after the first occurrence in iteration order over `elems`.
	  * The result is equivalent to adding each element sequentially
	  * with [[net.noresttherein.sugar.collections.RankingOps.+ +]]: elements are appended at the end,
	  * those already present in this ranking are ignored, and in case of duplicates in `elems`,
	  * only the first one is taken into account.
	  * @see [[net.noresttherein.sugar.collections.Ranking.+|+ +|+]]
	  * @see [[net.noresttherein.sugar.collections.Ranking.:++ :++]]
	  * @see [[net.noresttherein.sugar.collections.Ranking.++: ++:]]
	  */ //override for documentation only, the method is final in IterableOps
	def ++[U >: E](suffix :IterableOnce[U]) :CC[U] //= concat(elems)
	override def concat[U >: E](suffix :IterableOnce[U]) :CC[U] = suffix.knownSize match {
		case 0 => coll
		case 1 => suffix match {
			case it :Iterable[U] => added(it.head)
			case _               => added(suffix.iterator.next())
		}
		case _ if isEmpty => iterableFactory.from(suffix)
		case _ => (iterableFactory.newBuilder[U] ++= this ++= suffix).result()
	}

	/** Appends the given elements to this $coll. The effect is the same as calling
	  * [[net.noresttherein.sugar.collections.Ranking.:+ :+]] for every element in `elems` in its iteration order.
	  * All elements in `elems` will appear in the result in the exact same order,
	  * following this ranking without any elements from `elems`, and, in case of duplicates,
	  * only the last occurrence is taken into account.
	  * While both this method and [[net.noresttherein.sugar.collections.RankingOps.++ ++]] have (for larger collections)
	  * complexity of `O((this.size + suffix.size) * log (this.size + suffix.size))`, this method will run slower
	  * then the latter.
	  */
	@inline final def :++[U >: E](suffix :IterableOnce[U]) :CC[U] = this appendedAll suffix
	def appendedAll[U >: E](suffix :IterableOnce[U]) :CC[U]

	/** Prepends the given elements to this $coll. The effect is the same as calling
	  * [[net.noresttherein.sugar.collections.RankingOps.+: +:]] for every element in `elems`
	  * in its ''reverse'' iteration order. All elements in `elems` will appear in the result in the exact same order,
	  * on initial positions in the ranking, and, in case of duplicates,
	  * only the first occurrence is taken into account. For two $Coll arguments, it is equivalent to
	  * `prefix `[[net.noresttherein.sugar.collections.RankingOps.++ ++]]` this`, but using `++:` allows prepending
	  * a collection of any type, without converting first to a `Ranking`.
	  * @see [[net.noresttherein.sugar.collections.RankingOps.:++ :++]].
	  */
	@nowarn("cat=deprecation")
	@inline final override def ++:[U >: E](prefix :IterableOnce[U]) :CC[U] = this prependedAll prefix
	def prependedAll[U >: E](prefix :IterableOnce[U]) :CC[U] =
		if (prefix.knownSize == 0) coll
		else if (isEmpty) iterableFactory.from(prefix)
		else (iterableFactory.newBuilder[U] ++= prefix ++= this).result()

	/** Removes the given element from this $coll, if present. */
	@inline final def -[U >: E](elem :U) :C = this removedOne elem
	def removedOne[U >: E](elem :U) :C = indexOf(elem) match {
		case -1 => coll
		case  n => removed(n)
	}

	/** Removes the given elements from this $coll. */
	@inline final def --[U >: E](elems :IterableOnce[U]) :C = this removedAll elems
	def removedAll[U >: E](elems :IterableOnce[U]) :C =
		if (isEmpty || elems.knownSize == 0) coll
		else filterNot(elems.toIterableOnceOps.toSet)

	override def removed(index :Int) :C = {
		val size = knownSize
		val res  = newSpecificBuilder
		res sizeHint size - 1
		(res ++= take(index) ++= iterator.drop(index + 1)).result()
	}

	/** Returns a $Coll contains only elements present in both rankings, in an unspecified order. */
	@inline final def &[U >: E](other :Ranking[U]) :CC[U] = intersect(other)
	def intersect[U >: E](other :Ranking[U]) :CC[U] = {
		val thisSize = size
		val thatSize = other.size
		if (thisSize == 0 | thatSize == 0)
			empty
		else if (thisSize >= thatSize)
			iterableFactory.from(other.filter(contains))
		else
			filter(other.contains)
	}

	/** Returns a $coll contains only elements present in both collections, in an unspecified order. */
	@inline final def &[U >: E](other :collection.Set[U]) :CC[U] = intersect(other)
	def intersect[U >: E](other :collection.Set[U]) :CC[U] = {
		val thisSize = size
		val thatSize = other.knownSize
		if (thisSize == 0 | thatSize == 0 || thatSize < 0 && other.isEmpty)
			empty
		else if (thatSize < thisSize)
			iterableFactory.from(other.iterator.filter(contains))
		else
			filter(other)
	}

	/** Returns a $Coll contains only elements present in both rankings,
	  * in the order in which they appear in this $coll.
	  */
	@inline final def :&[U >: E](other :Ranking[U]) :C = keep(other)
	def keep[U >: E](other :Ranking[U]) :C =
		if (isEmpty || other.isEmpty)
			empty
		else
			filter(other.toSet)

	/** Returns a $coll contains only elements present in both collections,
	  * in the order in which they appear in this $coll.
	  */
	@inline final def :&[U >: E](other :collection.Set[U]) :C = keep(other)
	def keep[U >: E](other :collection.Set[U]) :C =
		if (isEmpty || other.isEmpty)
			empty
		else
			filter(other)


	/** Returns `this.intersect(other).size`, without actually constructing the intersection set. */
	final def intersectionSize[U >: E](other :Ranking[U]) :Int = {
		val thisSize = size
		val thatSize = other.size
		if (thisSize == 0 | thatSize == 0) 0
		else if (thisSize <= thatSize) count(other.contains)
		else other.count(contains)
	}
	/** Returns `this.intersect(other).size`, without actually constructing the intersection set. */
	final def intersectionSize[U >: E](other :collection.Set[U]) :Int = {
		val thisSize = size
		val thatSize = other.knownSize
		if (thisSize == 0 | thatSize == 0) 0
		else if (thisSize <= thatSize) count(other)
		else other.count(contains)
	}

	/** True if there are no common elements between this $coll and the argument. */
	final def disjoint[U>: E](other :Ranking[U]) :Boolean = {
		val thisSize = size
		val thatSize = other.size
		thisSize == 0 | thatSize == 0 || (
			if (thisSize <= thatSize) !exists(other.contains)
			else !other.exists(contains)
		)
	}
	/** True if there are no common elements between this $coll and the argument. */
	final def disjoint[U >: E](other :collection.Set[U]) :Boolean =
		isEmpty || other.isEmpty || {
			if (size <= other.knownSize) !exists(other)
			else !other.exists(contains)
		}


	def distinctBy[A](f :E => A) :C = fromSpecific(new View.DistinctBy(this, f))

	/** Creates a new $Coll with the same elements as this one, sorted with implicit `Ordering` for the elements. */
	def sorted[U >: E :Ordering] :C = fromSpecific(toSeq.sorted[U])

	/** Creates a new $Coll with the same elements as this one, sorted by a property of elements
	  * with an implicit `Ordering`.
	  */
	def sortBy[A :Ordering](f :E => A) :C = fromSpecific(toSeq.sortBy(f))

	/** Creates a new $Coll with the same elements, sorted using the passed function. */
	def sortWith(f :(E, E) => Boolean) :C = fromSpecific(toSeq.sortWith(f))

	/** Checks if the elements in this $coll follow the implicit ordering.
	  * @return [[net.noresttherein.sugar.collections.RankingOps.isSortedWith isSortedWith]]`(implicitly[Ordering[U]].compare(_, _) <= 0)`
	  */
	def isSorted[U >: E :Ordering] :Boolean = {
		val order = implicitly[Ordering[U]]
		isSortedWith(order.compare(_, _) <= 0)
	}

	/** Checks if the elements in this $coll are sorted by the given ordered property.
	  * @return [[net.noresttherein.sugar.collections.RankingOps.isSortedWith isSortedWith]]`(implicitly[Ordering[U]].compare(by(_), by(_)) <= 0)`
	  */
	def isSortedBy[U :Ordering](by :E => U) :Boolean = {
		val order = implicitly[Ordering[U]]
		isSortedWith((x, y) => order.compare(by(x), by(y)) <= 0)
	}

	/** Checks if the given predicate holds for all consecutive element pairs in this sequence. */
	def isSortedWith[U >: E](lte :(U, U) => Boolean) :Boolean =
		length <= 1 || {
			toSeq match {
				case seq :LinearSeq[E] =>
					lte(seq.head, seq.tail.head) &&
						seq.view.zip(seq.tail).forall { case (left, right) => lte(left, right) }
				case seq :collection.IndexedSeq[E] =>
					var i = seq.length - 1
					var second = seq(i);
					i -= 1
					var first = seq(i)
					while (i >= 1 && lte(first, second)) {
						second = first
						i -= 1
						first = seq(i)
					}
					i == 0 && lte(first, second)
				case seq => (seq to ArraySeq.untagged).isSortedWith(lte)
			}
		}

	def shuffled(implicit random :Random) :C = fromSpecific(toIndexedSeq.shuffled)

	/** Verifies if the element sets of the two collections are equal.
	  * @return `this.toSet == other.toSet` (without computing the actual sets).
	  */
	def isPermutationOf[U](other :Ranking[U]) :Boolean = size == other.size && other.forall(contains)
	//todo: combinations and permutations

	/** Creates a `r :`$Coll such that `r(permutation(i)) == this(i)`. */
	def reorder(permutation :Permutation) :C =
		if (permutation.length != size)
			throw new IllegalArgumentException(
				"Cannot reorder " + this + " according to " + permutation +
					" because it is of a wrong size: expected " + size + ", but got " + permutation.length + "."

			)
		else if (permutation.isIdentity) //covers also empty and singleton rankings
			coll
		else if (sizeCompare(1) <= 0)
			coll
		else {
			val size = this.size
			val seq  = RefArray.ofDim[E](size)
			val from = iterator
			val to   = permutation.iterator
			while (to.hasNext)
				seq(to.next()) = from.next()
			fromSpecific(IRefArray.Wrapped(seq.asIRefArray))
		}

	/** The same as `toSeq.`[[scala.collection.SeqOps.sameElements sameElements]]. */
	def sameElements[U >: E](that :IterableOnce[U]) :Boolean = corresponds(that)(_ == _)

	/** Creates a $Coll with the same elements as this one, but in the reverse order.
	  * This will not copy large collections, but instead return an adapter.
	  * If you wish to force creation of a standard implementation use `reverseIterator to Ranking`.
	  */
	def reverse :C = fromSpecific(reverseIterator)

	/** Iterates this $coll in the reverse order. This will ''not'' require creation of any intermediate objects
	  * other than the iterator itself.
	  */
	def reverseIterator :Iterator[E]

	override def view :IndexedSeqView[E] =
		if (isEmpty) EmptyIndexedSeqOps.view else new RankingView[E](this)

	override def to[C1](factory :Factory[E, C1]) :C1 = sourceCollectionFactory(factory) match {
		case Yes(Seq) | Yes(IndexedSeq) => toIndexedSeq.asInstanceOf[C1]
		case Yes(Set) => toSet.asInstanceOf[C1]
		case Yes(Ranking) => this.asInstanceOf[C1]
		case _ => super.to(factory)
	}

	/** An indication that iterating over this $coll using `apply` is likely to be faster than using an iterator. */
	def applyPreferred :Boolean = size <= IndexedSeqDefaults.defaultApplyPreferredMaxLength

	//todo: implement these
	//	def updated[U >: T](index :Int, first :U, second :U, rest :U*) :Ranking[U]
	//	def inserted[U >: T](index :Int, first :U, second :U, rest :U*) :Ranking[U]
	//	def removed(from :Int, until :Int) :Ranking[T]
}




//todo: MutRanking backed by skip list
/** A collection of unique items in a specific order providing `O(1)` `indexOf(T)`, `contains(T)`, `apply(Int)`,
  * `toSet`, `toSeq`, `toIndexedSeq` and `size` implementations. It can be thought of as a mix of [[Set]] and [[Seq]]:
  * no two elements in the collection are equal, but their position in the collection is important and features
  * in equality (essentially having `Seq` semantics), unlike `Set` equality, including implementations preserving
  * insertion order. Note that this means that sequence-like append/prepend operations will lead to no change
  * in the collection if the added items already exists. When a `Ranking` is mapped (or in other bulk methods returning
  * another `Ranking`), the returned Ranking's order is defined by the order of `this`, with all duplicates
  * being removed except for their first occurrence.
  *
  * Implementations are optimised more towards fast access than efficient incremental expansion.
  * It is recommended to use [[net.noresttherein.sugar.collections.Ranking.newBuilder newBuilder]]
  * when removing or adding multiple elements, especially through methods other than `+` and `++`.
  * @tparam E element type.
  * @define Coll `Ranking`
  * @define coll ranking
  * @author Marcin Mościcki marcin@moscicki.net
  */ //todo: review all methods in SeqOps and see what still needs copying.
/* It is really tempting to inherit IndexedSeqOps (without IndexedSeq), but it enforces add == appended
 * and concat = appendedAll, while we have separate semantics. Additionally, overriding indexOf(elem) is deprecated,
 * and overriding indexOf(elem, from) is a bad fit. Unfortunate, because being IndexedSeqOps implies fast random access
 * and would include Ranking in any code optimized for the latter (assuming it doesn't check for IndexedSeq only).
 */
trait Ranking[+E]
	extends immutable.Iterable[E] with SugaredIterable[E]
	   with RankingOps[E, Ranking, Ranking[E]] with IterableFactoryDefaults[E, Ranking] with Equals with Serializable
{ ranking =>
	//Consider: moving all methods down to AbstractRanking, to make creating custom Ranking subclasses easier
	// by not being burdened with methods returning a static Ranking conflicting with a more specific RankingOps.
	// This would require moving virtually the whole implementation from the Ranking object to something
	// package protected, so that AbstractRanking can access it.
	@inline final override def length  :Int     = knownSize
	@inline final override def size    :Int     = knownSize
	@inline final override def isEmpty :Boolean = knownSize == 0

	//consider: set methods
	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :Ranking[_] => sameElements(other)
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Ranking[_]]
	override def hashCode :Int = toIndexedSeq.hashCode

	override def toIndexedSeq  :IndexedSeq[E] = new RankingSeqAdapter(this)
	override def toSeq         :Seq[E] = toIndexedSeq
	override def toSet[U >: E] :Set[U] = new RankingSetAdapter(this)

	override def iterableFactory :IterableFactory[Ranking] = Ranking

	protected override def className = "Ranking"
}




/** $factoryInfo
  * @define Coll `Ranking`
  * @define coll ranking
  */
@SerialVersionUID(Ver)
case object Ranking extends IterableFactory[Ranking] {

	/** Converts the given collection to a `Ranking`, keeping always the ''last'' value if `elems` contains duplicates.
	  * The result is the same as if built recursively with [[net.noresttherein.sugar.collections.Ranking.:+ :+]].
	  * If instead the behaviour of of [[net.noresttherein.sugar.collections.Ranking.+ +]] is desired,
	  * use $Coll`.`[[net.noresttherein.sugar.collections.Ranking.from from]]`(elems)`.
	  * @return A $coll equal to $Coll.`empty `[[net.noresttherein.sugar.collections.Ranking.:++ :++]] `elems`.
	  */
	def lastOccurrences[T](elems :IterableOnce[T]) :Ranking[T] = from(elems, false)

	/** Converts the given collection to a `Ranking`, keeping always the first value if `elems` contains duplicates.
	  * The result is the same as if built recursively with [[net.noresttherein.sugar.collections.Ranking.+ +]].
	  * If instead the behaviour of of [[net.noresttherein.sugar.collections.Ranking.:+ :+]] is desired,
	  * use $Coll.`empty `[[net.noresttherein.sugar.collections.Ranking.:++ :++]] `elems`
	  * or [[net.noresttherein.sugar.collections.Ranking.appendingBuilder appendingBuilder]].
	  * @return A $coll equal to $Coll.`empty `[[net.noresttherein.sugar.collections.Ranking.++ ++]] `elems`.
	  */
	override def from[T](elems :IterableOnce[T]) :Ranking[T] = from(elems, true)

	def from[T](elems :IterableOnce[T], keepFirst :Boolean) :Ranking[T] = elems match {
		case _       if elems.knownSize == 0          => Ranking.empty[T]
		case view    :View[T]                         => from(view.iterator, keepFirst)
		case ranking :Ranking[T]                      => ranking
		case seq     :RankingSeqAdapter[T]            => seq.toRanking
		case set     :RankingSetAdapter[T]            => set.toRanking
		case iter    :Iterable[_] if iter.isEmpty     => Ranking.empty[T]
		case iter    :Iterator[_] if iter.isEmpty     => Ranking.empty[T]
		case iter    :Iterable[T] if iter.sizeIs == 1 =>
			new SingletonRanking[T](iter.head)
		case set     :collection.Set[_] =>
			if (set.size <= SmallRankingCap)
				new SmallRanking(RefArray.from(elems))
			else
				new IndexedRanking(DefaultIndexedSeq.from(elems))
		case iter    :Iterable[T] if iter.sizeIs <= SmallRankingCap =>
			deduplicateSmall(iter, keepFirst)
		case iter    :Iterable[T] =>
			deduplicateLarge(iter, keepFirst)
		case _ if keepFirst => (Ranking.newBuilder[T] ++= elems).result()
		case _ => (Ranking.appendingBuilder[T] ++= elems).result()
	}

	/** A new builder adding elements with the semantics of [[net.noresttherein.sugar.collections.Ranking.+ +]]. */
	override def newBuilder[T] :Builder[T, Ranking[T]] = new RankingBuilder[T]

	/** Same as `newBuilder`, but gives the builder the argument value as [[collection.mutable.Builder.sizeHint sizeHint]]
	  * before returning it.
	  */
	def newBuilder[T](sizeHint :Int) :Builder[T, Ranking[T]] = {
		val res = new RankingBuilder[T]
		if (sizeHint >= 0)
			res sizeHint sizeHint
		res
	}

	/** A builder adding the elements with the semantics of [[net.noresttherein.sugar.collections.Ranking.+| +|]].
	  * It only guarantees that the order of the elements in the built `Ranking` corresponds to a (non consecutive)
	  * subsequence of the input, without specifying which of the occurrences of equal elements are kept.
	  */
	def unorderedBuilder[T] :Builder[T, Ranking[T]] = new UnorderedBuilder[T]

	/** Same as [[net.noresttherein.sugar.collections.Ranking.unorderedBuilder unorderedBuilder]],
	  * but additionally gives a [[collection.mutable.Builder.sizeHint sizeHint]] to the returned builder.
	  */
	def unorderedBuilder[T](sizeHint :Int) :Builder[T, Ranking[T]] = {
		val res = new UnorderedBuilder[T]
		if (sizeHint >= 0)
			res sizeHint sizeHint
		res
	}

	/** A `Ranking` builder adding elements with the semantics of [[net.noresttherein.sugar.collections.Ranking.:+ :+]]
	  * - in case of duplicates, the most recently added item replaces the previous instance in the sequence.
	  * This builder is less efficient than [[net.noresttherein.sugar.collections.Ranking.newBuilder newBuilder]] and
	  * [[net.noresttherein.sugar.collections.Ranking.unorderedBuilder unorderedBuilder]].
	  */
	def appendingBuilder[T] :Builder[T, Ranking[T]] = new AppendingBuilder[T]

	/** Same as [[net.noresttherein.sugar.collections.Ranking.appendingBuilder appendingBuilder]],
	  * but additionally gives a [[collection.mutable.Builder.sizeHint sizeHint]] to the returned builder.
	  */
	def appendingBuilder[T](sizeHint :Int) :Builder[T, Ranking[T]] = new AppendingBuilder(sizeHint)


	override def empty[E] :Ranking[E] = EmptyRanking //reusableEmpty

	/** A specialized light `Ranking` implementation for collections containing only one element. */
	def one[T](singleton :T) :Ranking[T] = new SingletonRanking(singleton)

	/** A specialized light `Ranking` implementation for collections containing only one element. */
	def single[T](singleton :T) :Ranking[T] = new SingletonRanking(singleton)


	def unapplySeq[T](elems :Ranking[T]) :Yes[Seq[T]] = Yes(elems.toSeq)

	@SerialVersionUID(Ver)
	object implicits {
		implicit def rankingToSeq[T](ranking :Ranking[T]) :Seq[T] = ranking.toSeq
		implicit def rankingToSet[T](ranking :Ranking[T]) :Set[T] = ranking.toSet
	}

	
	@SerialVersionUID(Ver)
	class RankingView[+E](underlying :RankingOps[E, IterableOnce, _]) extends AbstractIndexedSeqView[E] {
		override def apply(i :Int) :E = underlying(i)
		override def length :Int = underlying.length
	}

	override def toString = "Ranking"
}




private object RankingImpl {

	private[this] val Duplicate = new AnyRef
	def deduplicateSmall[T](items :Iterable[T], keepFirst :Boolean) :Ranking[T] = {
		 /* Counts the total number of duplicates in the array (i.e, `array.length - array.toSet.size`),
		  * setting all bust the last copy of each equal item pair to `Duplicate`. */
		def erasePrecedingDuplicates(array :RefArray[T]) :Int = {
			var i = array.length
			var duplicates = 0
			while (i > 0) {
				i -= 1
				var j = i
				val ati = array(i)
				if (ati.asAnyRef ne Duplicate) //skip searching if ati is already a counted duplicate
					while (j > 0) {
						j -= 1
						if (ati == array(j)) {
							duplicates += 1
							array(j) = Duplicate.asInstanceOf[T]
						}
					}
			}
			duplicates
		}
		/* Counts the total number of duplicates in the array (i.e, `array.length - array.toSet.size`),
		 * setting all bust the last copy of each equal item pair to `Duplicate`. */
		def eraseFollowingDuplicates(array :RefArray[T]) :Int = {
			val len = array.length
			var duplicates = 0
			var i = 0
			while (i < len) {
				var j = i + 1
				val ati = array(i)
				if (ati.asAnyRef ne Duplicate)
					while (j < len) {
						if (ati == array(j)) {
							duplicates += 1
							array(j) = Duplicate.asInstanceOf[T]
						}
						j += 1
					}
					i += 1
			}
			duplicates
		}
		val array = RefArray.from(items)
		val duplicates =
			if (keepFirst) eraseFollowingDuplicates(array)
			else erasePrecedingDuplicates(array)
		if (duplicates == 0)
			new SmallRanking(array)
		else if (duplicates == array.length - 1) {
			var i = array.length
			var singleton :T = Duplicate.asInstanceOf[T]
			while (singleton.asAnyRef eq Duplicate) {
				i -= 1; singleton = array(i)
			}
			new SingletonRanking(singleton)
		} else {
			val deduped = RefArray.ofDim[T](array.length - duplicates)
			var i = array.length
			var j = deduped.length
			while (i > 0) { //copy non-null elements from array to deduped
				i -= 1
				val elem = array(i)
				if (elem.asAnyRef ne Duplicate) {
					j -= 1
					deduped(j) = elem
				}
			}
			new SmallRanking(deduped)
		}
	}

	def deduplicateLarge[T](items :Iterable[T], keepFirst :Boolean) :Ranking[T] = {
		val seq = items match {
			case seq :IndexedSeq[T] => seq
			case _ => items to RelayArray
		}
		val indexed = seq.view.zipWithIndex to DefaultIndexedSeq
		val map = if (keepFirst) indexed.reverseIterator.toMap else indexed.toMap
		if (map.size == seq.length)
			new IndexedRanking(seq, map)
		else {
			//remove duplicates, leave only those lucky to be in map
			val ranking = indexed.collect { case (item, i) if map(item) == i => item }
			new IndexedRanking(ranking)
		}
	}


	@inline def hashCodeOf(x :Any) :Int = if (x == null) 0 else x.hashCode

	def smallContains[U](elems :RefArray[U], hashes :Array[Int], size :Int)(elem :U, hash :Int) :Boolean = {
		var i = size - 1
		while (i >= 0 && (hashes(i) != hash || elems(i) != elem))
			i -= 1
		i >= 0
	}


	/** `SeqFactory[IndexedSeq]` best supporting modifications.
	  * When creating new rankings from scratch, `DefaultIndexedSeq` is used instead.
	  */
	final val IndexedSeqFactory = Vector
	final val BulkUpdateFasterLog = 5

	final val SmallRankingCap = 16
	final val SmallRankingOptimismCap = SmallRankingCap + (SmallRankingCap >> 1)
//	final val OptimisticIntersectionPercentPrediction = 20
	final val SmallRankingPessimismCap = SmallRankingCap << 2
	final val SmallRankingPessimismFactor = 8
	final val DummyHashArray = new Array[Int](SmallRankingCap << 1)


	@SerialVersionUID(Ver)
	class RankingSerializer[+E](elems :RefArray[E]) extends Serializable {
		def this(elems :Ranking[E]) = this(RefArray.from(elems))

		private def readResolve =
			if (elems.length <= SmallRankingCap) new SmallRanking(elems)
			else new IndexedRanking(IRefArray.Wrapped(elems.asIRefArray))
	}


	@SerialVersionUID(Ver)
	class RankingSeqAdapter[+T](ranking :Ranking[T])
		extends AbstractSeq[T] with IndexedSeq[T] with StrictOptimizedSeqOps[T, IndexedSeq, IndexedSeq[T]]
			with IterableProxy[T] with DefaultSerializable
	{
		override def underlying = ranking
		override def length :Int = ranking.size

		override def apply(idx :Int) :T = ranking(idx)

		override def segmentLength(p :T => Boolean, from :Int) :Int = ranking.segmentLength(p, from)
		override def indexWhere(p :T => Boolean, from :Int) :Int = ranking.indexWhere(p, from)
		override def lastIndexWhere(p :T => Boolean, end :Int) :Int = ranking.lastIndexWhere(p, end)
		override def indexOf[U >: T](elem :U, start :Int) :Int = {
			val i = ranking.indexOf(elem)
			if (i < start) -1 else i
		}
		override def lastIndexOf[U >: T](elem :U, end :Int) :Int = {
			val i = ranking.indexOf(elem)
			if (i > end) -1 else i
		}
		override def contains[U >: T](elem :U) :Boolean = ranking.contains(elem)

		override def toSet[U >: T] :Set[U] = ranking.toSet

		def toRanking :Ranking[T] = ranking
	}


	@SerialVersionUID(Ver)
	class RankingSetAdapter[T](ranking :Ranking[T])
		extends AbstractSet[T] with Set[T] with StrictOptimizedSetOps[T, Set, Set[T]]
			with IterableProxy[T] with DefaultSerializable
	{
		override def underlying = ranking
		override def contains(elem :T) :Boolean = ranking.contains(elem)

		override def incl(elem :T) :Set[T] = if (contains(elem)) this else Set(toSeq:_*)
		override def excl(elem :T) :Set[T] = if (contains(elem)) Set(toSeq:_*) - elem else this

		override def intersect(that :collection.Set[T]) :Set[T] = {
			val thatSize = that.knownSize
			if (ranking.size <= thatSize)
				filter(that)
			else that match {
				case set :Set[T] => set.filter(this)
				case _ => Set.from(that.iterator.filter(this))
			}
		}

		override def toIndexedSeq :IndexedSeq[T] = ranking.toIndexedSeq
		override def toSeq :Seq[T] = ranking.toSeq
		def toRanking :Ranking[T] = ranking
	}


	/** A builder of `Ranking` instances working in two modes, depending on the constructor used
	  * and constructor parameters as well as the number of elements added.
	  * In case the parameter list contains
	  *   - no arguments,
	  *   - `SmallRanking`,
	  *   - `smallSize >= 0 && smallSize <= SmallRankingCap` (if non zero, then arrays `small` and `hashes`
	  *     should be not null and contain that many elements and their computed hash codes in their prefixes),
	  * then it tries first to build a `SmallRanking`, appending elements to to the `small` array.
	  * However, if
	  *   - the number of elements added exceeds `SmallRankingCap`.
	  *   - `smallSize < 0` is given as a constructor argument,
	  *   - an `IndexedSeq` builder and an index `Map` are given as arguments,
	  *   - or `sizeHint` is called for a number greater than `SmallRankingCap`,
	  * it switches to the `IndexedRanking` building mode (by setting `smallSize = -1`, where elements are appended
	  * to the builder `large` and inserted in the map `index` associated with the next free index.
	  *
	  * The elements in the result are in the order in which they were added to the builder,
	  * with duplicates resolved in favour of the first occurrence.
	  */
	class RankingBuilder[T] protected (
                  private[this] var large     :Builder[T, IndexedSeq[T]],
                  private[this] var index     :Map[T, Int], //<-^builds IndexedRanking
                  private[this] var small     :RefArray[T], //<- builds SmallRanking
                  private[this] var hashes    :Array[Int], //hash codes of elements in small
                  private[this] var smallSize :Int) //the number of elements in small or -1 if large is used instead
		extends ReusableBuilder[T, Ranking[T]]
	{
		def this(items :Builder[T, IndexedSeq[T]], index :Map[T, Int]) =
			this(items, index, null, null, -1)

		def this(small :RefArray[T], hashes :Array[Int], size :Int) = this(null, null, small, hashes, size)

		def this(ranking :SmallRanking[T]) =
			this(null, null,
				RefArray.copyOf(ranking.array, SmallRankingCap),
				copyOf(ranking.hashCodes, SmallRankingCap),
				ranking.size)

		def this() =
			this(null, null, null, null, 0)

		override def knownSize :Int = if (smallSize >= 0) smallSize else index.size

		protected final def seqBuilder = {
			if (large == null) {
				index = Map.empty
				large = DefaultIndexedSeq.newBuilder[T]
			}
			large
		}
		protected final def map = {
			if (index == null) {
				index = Map.empty
				large = DefaultIndexedSeq.newBuilder
			}
			index
		}
		protected final def map_=(map :Map[T, Int]) :Unit = index = map
		protected final def array      = small
		protected final def array_=(array :RefArray[T]) :Unit = small = array
		protected final def hashCodes  = hashes
		protected final def hashCodes_=(array :Array[Int]) :Unit = hashes = array
		protected final def size       = smallSize
		protected final def size_=(size :Int) :Unit = smallSize = size

		@inline protected final def ensureSmall() :Unit =
			if (small == null) {
				small  = RefArray.ofDim[T](SmallRankingCap)
				hashes = new Array[Int](SmallRankingCap)
			}

		protected final def contains(elem :T, hash :Int) :Boolean =
			smallSize != 0 && {
				if (smallSize > 0)
					smallContains(small, hashes, smallSize)(elem, hash)
				else
					index != null && index.contains(elem)
			}

		override def sizeHint(size :Int) :Unit = //todo: support small.length < SmallRankingCap
			if (size >= SmallRankingCap && smallSize >= 0)
				switchToLarge(size)

		protected final def switchToLarge(extraSpace :Int = 0) :Unit =
			if (smallSize > 0) {
				index = Map.empty
				large = DefaultIndexedSeq.newBuilder[T]
				if (extraSpace < 0)
				large sizeHint math.min(extraSpace, Int.MaxValue - smallSize) + smallSize
				var i = 0; val end = smallSize
				while (i < end) {
					val item = small(i)
					large += item
					index = index.updated(item, i)
					i += 1
				}
				smallSize = -1
			} else {
				val hint =
					if (large == null) {
						index = Map.empty
						large = DefaultIndexedSeq.newBuilder
						extraSpace
					} else {
						val size = index.size
						if (extraSpace > Int.MaxValue - size) Int.MaxValue else extraSpace + size
					}
				large sizeHint hint
				smallSize = -1
			}

		/** Called when smallSize >= 0. */
		protected def smallAddOne(elem :T, hash :Int) :this.type = {
			if (small == null) {
				small  = RefArray.ofDim[T](SmallRankingCap)
				hashes = new Array[Int](SmallRankingCap)
				small(0)  = elem
				hashes(0) = hash
				smallSize = 1
			} else if (!contains(elem, hash))
				if (smallSize < SmallRankingCap) {
					small(smallSize)  = elem
					hashes(smallSize) = hash
					smallSize += 1
				} else {
					switchToLarge(SmallRankingCap)
					large += elem
					index = index.updated(elem, SmallRankingCap)
				}
			this
		}

		protected def largeAddOne(elem :T) :this.type = {
			if (!index.contains(elem)) {
				index = index.updated(elem, index.size)
				large += elem
			}
			this
		}

		override def addOne(elem :T) :this.type =
			if (smallSize >= 0)
				smallAddOne(elem, hashCodeOf(elem))
			else
				largeAddOne(elem)


		/** Called when `smallSize <= 0` */
		protected def become(xs :Ranking[T]) :this.type = xs match {
			case ranking :SmallRanking[T] if smallSize == 0 =>
				smallSize = ranking.size
				if (small == null) {
					small  = RefArray.copyOf(ranking.array, SmallRankingCap)
					hashes = copyOf(ranking.hashCodes, SmallRankingCap)
				} else {
					arraycopy(ranking.array, 0, small, 0, smallSize)
					arraycopy(ranking.hashCodes, 0, hashes, 0, smallSize)
				}
				this
			case ranking :IndexedRanking[T] =>
				large = DefaultIndexedSeq.newBuilder
				large sizeHint ranking.size
				large ++= ranking.toIndexedSeq
				index = ranking.index.castFrom[Map[_ <: T, Int], Map[T, Int]]
				smallSize = -1
				this
			case ranking :SingletonRanking[T] if smallSize == 0 =>
				smallAddOne(ranking.head, hashCodeOf(ranking.head))
			case _ if smallSize == 0 =>
				smallAddAll(xs)
			case _ =>
				largeAddAll(xs)
		}

		protected final def smallAddAll(xs :IterableOnce[T]) :this.type = {
			var maybeSize = xs.knownSize
			if (maybeSize < 0)
				maybeSize = -1 //don't underflow
			if (maybeSize != 0) {
				if (small == null) {
					small  = RefArray.ofDim[T](SmallRankingCap)
					hashes = new Array[Int](SmallRankingCap)
				}
				val it = xs.iterator
				while (smallSize < SmallRankingCap && it.hasNext) {
					val item  = it.next()
					val hash  = hashCodeOf(item)
					if (!contains(item, hash)) {
						small(smallSize)  = item
						hashes(smallSize) = hash
						smallSize += 1
					}
					maybeSize -= 1
				}
				if (it.hasNext) {
					switchToLarge(math.max(maybeSize, 1))
					largeAddAll(it)
				}
			}
			this
		}

		//assumes smallSize < 0 and that large and index are initialized
		protected def largeAddAll(xs :IterableOnce[T]) :this.type = {
			var size = index.size
			val it   = xs.iterator
			while (it.hasNext) {
				val item = it.next()
				if (!index.contains(item)) {
					index  = index.updated(item, size)
					large += item
					size  += 1
				}
			}
			this
		}

		def addSlice(xs :SmallRanking[T], from :Int, until :Int) :this.type = {
			val xsSize = until - from
			var i = 0
			if (smallSize == 0) {
				smallSize = xsSize
				if (small == null) {
					small  = RefArray.copyOfRange(xs.array, from, xs.size, SmallRankingCap)
					hashes = Array.copyOfRange(xs.hashCodes, from, xs.size, SmallRankingCap)
				} else {
					arraycopy(xs.array, from, small, 0, smallSize)
					arraycopy(xs.hashCodes, from, hashes, 0, smallSize)
				}
				i = xsSize
			} else if (smallSize > 0) {
				val elems  = xs.array
				val hashes = xs.hashCodes
				while (smallSize < SmallRankingCap & i < xsSize) {
					val elem = elems(i)
					val hash = hashes(i)
					if (!contains(elem, hash)) {
						small(smallSize) = elem
						this.hashes(smallSize) = hash
						smallSize += 1
					}
					i += 1
				}
			}
			if (i < xsSize) {
				switchToLarge(xsSize - i)
				while (i < xsSize) {
					largeAddOne(xs(i))
					i += 1
				}
			}
			this
		}

		@inline protected final def addAll(xs :SmallRanking[T]) :this.type = addSlice(xs, 0, xs.size)

		protected def addAll(xs :Ranking[T]) :this.type = xs match {
			case small  :SmallRanking[T]     => addAll(small)
			case single :SingletonRanking[T] => addOne(single.head)
			case _ if smallSize == 0 =>
				become(xs)
			case _ if !xs.applyPreferred =>
				if (smallSize > 0)
					switchToLarge(xs.size)
				largeAddAll(xs)
			case _ =>
				val xsSize = xs.size
				var i      = 0
				if (smallSize > 0 && xsSize <= SmallRankingCap)
					while (i < xsSize && smallSize < SmallRankingCap) {
						val elem = xs(i)
						val hash = hashCodeOf(elem)
						if (!contains(elem, hash)) {
							small(smallSize) = elem
							hashes(smallSize) = hash
							smallSize += 1
						}
						i += 1
					}
				if (i < xsSize) {
					switchToLarge(xsSize - i)
					while (i < xsSize) {
						largeAddOne(xs(i))
						i += 1
					}
				}
				this
		}

		override def addAll(xs :IterableOnce[T]) :this.type = xs match {
			case empty   :Iterable[T] if empty.isEmpty =>
				this
			case ranking :Ranking[T] =>
				addAll(ranking)
			case _ if smallSize < 0 =>
				largeAddAll(xs)
			case _ =>
				smallAddAll(xs)
		}

		override def clear() :Unit = {
			smallSize = 0
			index = null
			large = null
			if (small != null)
				java.util.Arrays.fill(small.asInstanceOf[Array[AnyRef]], null)
		}
		@inline final def clear(result :Ranking[T]) :Ranking[T] = {
			clear(); result
		}

		override def result() :Ranking[T] = smallSize match {
			case 0  => clear(); Ranking.empty[T]
			case 1  => clear(new SingletonRanking(small(0)))
			case -1 => clear(new IndexedRanking(large.result(), index))
			case n if n == small.length =>
				val res = new SmallRanking(small, hashes)
				small = null; hashes = null
				res
			case n  => clear(new SmallRanking[T](small.take(n), hashes.take(n)))
		}
	}



	/** A builder offering no guarantees about the positions of added elements in the built `Ranking`,
	  * other that they are consistent with ''one'' of the occurrence in the input sequence.
	  * Employs a couple of tricks more.
	  */
	final class UnorderedBuilder[T] private(
                        private[this] var _large     :Builder[T, IndexedSeq[T]],
                        private[this] var _index     :Map[T, Int], //<-^builds IndexedRanking
                        private[this] var _small     :RefArray[T], //<- builds SmallRanking
                        private[this] var _hashes    :Array[Int], //hash codes of elements in small
                        private[this] var _smallSize :Int) //the number of elements in small or -1 if large is used instead
		extends RankingBuilder[T](_large, _index, _small, _hashes, _smallSize)
	{
		def this(items :Builder[T, IndexedSeq[T]], index :Map[T, Int]) =
			this(items, index, null, null, -1)

		def this(ranking :SmallRanking[T]) =
			this(null, null,
				RefArray.copyOf(ranking.array, SmallRankingCap),
				copyOf(ranking.hashCodes, SmallRankingCap),
				ranking.size)

		def this() =
			this(null, null, null, null, 0)

		/** Tries to fit `xs` into `small` buffer. Assumes `smallSize > 0 && xs.size > smallSize`,
		  * so it's faster to filter `small` not contained in `xs` and then add `xs` knowing the elements are unique.
		  */
		private def smallFilterAddAll(xs :Ranking[T]) :this.type = {
			val small   = array
			val hashes  = hashCodes
			val oldSize = this.size
			val xsSize  = xs.size
			var newSize = 0
			var i       = 0
			val done = xs match {
				case ranking :SmallRanking[T] =>
					//Almost the same as the next case, but we take advantage of knowing the hash when testing for membership.
					while (i < oldSize) {                      //let newElems = small.filterNot(xs.contains)
						val item = small(i)
						val hash = hashes(i)
						if (ranking.indexOf(item, hash) < 0) {
							small(newSize)  = item
							hashes(newSize) = hash
							newSize += 1
						}
						i += 1
					}
					(xsSize <= SmallRankingCap - newSize) && { //current elements and xs together fit in a SmallRanking
						arraycopy(ranking.array, 0, small, newSize, xsSize)
						arraycopy(ranking.hashCodes, 0, hashes, newSize, xsSize)
						size = newSize + xsSize
						true
					}
				case _ =>
					while (i < oldSize) {                      //let newElems = small.filterNot(xs.contains)
						val item = small(i)
						if (xs.indexOf(item) < 0) {
							small(newSize)  = item
							hashes(newSize) = hashes(i)
							newSize += 1
						}
						i += 1
					}
					(xsSize <= SmallRankingCap - newSize) && { //current elements and xs together fit in a SmallRanking
						i = 0
						while (i < xsSize) {
							val item = xs(i)
							small(newSize)  = item
							hashes(newSize) = hashCodeOf(item)
							i += 1; newSize += 1
						}
						size = newSize
						true
					}
			}
			if (!done) {
				size = newSize
				switchToLarge(xsSize)
				val builder = seqBuilder
				var index   = map
				val it      = xs.iterator
				while (it.hasNext) {
					val item = it.next()
					builder += item
					index = index.updated(item, newSize)
					newSize += 1
				}
				map = index
				size = newSize
			}
			this
		}

		override def addAll(xs :Ranking[T]) :this.type = {
			val oldSize = size
			val xsSize  = xs.size
			if (oldSize > 0)
				if (xsSize <= SmallRankingCap & xsSize > oldSize)
					smallFilterAddAll(xs)
				else
					super.addAll(xs)
			else if (oldSize == 0)
				become(xs)
			else if (xsSize > (map.size << 1)) { //faster to carry over our contents to it than vice versa
				val large = seqBuilder
				val retained = large.result().filterNot(xs.contains)
				large.clear()
				if (retained.isEmpty) {
					map = Map.empty[T, Int]
					become(xs)
				} else {
					val newElems = xs match {
						case ranking :IndexedRanking[T] => ranking.toSeq
						case _ => xs
					}
					large ++= retained ++= newElems
					map = newElems.foldLeftWithIndex(Map.empty[T, Int]) {
						(acc, elem, i) => acc.updated(elem, i + oldSize)
					} ++ retained.iterator.zipWithIndex
				}
			} else
				super.addAll(xs)
			this
		}
	}


	//Vector.slice is O(log(n)), so we can use an
	/** A builder with semantics of `:+`. */ //todo: use Jack as the IndexedSeq
	class AppendingBuilder[T](private[this] var hint :Int = -1) extends ReusableBuilder[T, Ranking[T]] {
		private[this] var buffer    :mutable.Queue[IterableOnce[T]] = _
		private[this] var singleton :IterableOnce[T] = _

		override def sizeHint(size :Int) :Unit = if (size >= 0) hint = size

		private def add(elems :IterableOnce[T]) :this.type = {
			if (buffer != null)
				buffer += elems
			else if (singleton == null)
				singleton = elems
			else {
				buffer = new mutable.Queue
				buffer += singleton += elems
			}
			this
		}
		override def addOne(elem :T) = add(Ranking.one(elem))

		override def addAll(xs :IterableOnce[T]) :this.type = xs match {
			case empty   :Iterable[_] if empty.isEmpty      => this
			case empty   :Iterator[_] if !empty.hasNext     => this
			case single  :Iterable[T] if single.sizeIs == 1 => add(single)
			case ranking :Ranking[T]                        => add(ranking.reverse)
			case _                                          => add(Iterators.reverse(xs))
		}

		override def clear() :Unit = {
			singleton = null
			if (buffer != null)
				buffer.clear()
		}

		override def result() :Ranking[T] =
			if (buffer == null)
				if (singleton == null)
					Ranking.empty
				else {
					val res = new ReverseBuilder[T]
					if (hint >= 0)
						res.sizeHint(hint)
					res ++= singleton
					singleton = null
					res.result()
				}
			else {
				val res = new ReverseBuilder[T]
				if (hint < 0) {
					@tailrec def count(idx :Int = buffer.length, acc :Int = 0) :Int =
						if (idx == 0)
							acc
						else buffer(idx - 1).knownSize match {
							case -1 => -1
							case  n => count(idx - 1, acc + n)
						}
					hint = count()
				}
				if (hint >= 0)
					res sizeHint hint
				var i = buffer.length
				while (i > 0) {
					i -= 1
					res ++= buffer(i)
				}
				buffer    = null
				singleton = null
				res.result()
			}
	}



	/** A special builder which returns the result in the reverse order of appending.
	  * Because, just like `UnorderedBuilder`, it keeps the first element added in favour of subsequent duplicates,
	  * it is used to implement `Ranking.:++`, which appends with `:+` semantics - that is, should have each element
	  * positioned according to its last occurrence. Note that, for this reason, elements should be appended
	  * in reverse iteration order, i.e. for `ranking :++ extras` it is:
	  * {{{
	  *     (new ReverseBuilder ++= extras.toList.reverse ++= ranking.reverseIterator).result()
	  * }}}
	  */
	final class ReverseBuilder[T] private (
                        private[this] var large     :Buffer[T],
                        private[this] var set       :mutable.Set[T], //<-^builds IndexedRanking
                        private[this] var small     :RefArray[T], //<- builds SmallRanking
                        private[this] var hashes    :Array[Int], //hash codes of elements in small
                        private[this] var smallSize :Int) //the number of elements in small or -1 if large is used instead
		extends ReusableBuilder[T, Ranking[T]]
	{
		def this(ranking :IndexedRanking[T]) = this(
			DefaultBuffer.ofCapacity[T](ranking.size) ++= ranking.reverseIterator,
			{ val set = mutable.Set.empty[T]; set sizeHint ranking.size; set ++= ranking },
			null, null, -1
		)

		def this(ranking :SmallRanking[T]) =
			this(null, null, ranking.array.reverse, ranking.hashCodes.reverse, ranking.size)

		def this() =
			this(null, null, null, null, 0)

		override def sizeHint(size :Int) :Unit =
			if (size >= SmallRankingCap && smallSize >= 0)
				switchToLarge(math.max(0, size - smallSize))

		def isSmall :Boolean = smallSize >= 0

		def contains(elem :T) :Boolean =
			if (smallSize < 0)
				set != null && set.contains(elem)
			else
				contains(elem, hashCodeOf(elem))

		@inline def contains(elem :T, hash :Int) :Boolean =
			smallSize > 0 && smallContains(small, hashes, smallSize)(elem, hash)

		private def switchToLarge(extra :Int = 1) :Unit = {
			if (large == null) {
				large = DefaultBuffer.ofCapacity(smallSize + extra)
				set = mutable.Set.empty
				set sizeHint smallSize + extra
			}
			var i = 0
			while (i < smallSize) {
				val elem = small(i)
				large += elem
				set   += elem
				i += 1
			}
			smallSize = -1
		}

		//todo: ReverseBuilder.addAll
//		override def addAll(xs :IterableOnce[T]) :this.type = super.addAll(xs)

		private def addFirst(elem :T) :this.type = {
			small  = RefArray.ofDim[T](SmallRankingCap)
			hashes = new Array[Int](SmallRankingCap)
			small(0)  = elem
			hashes(0) = hashCodeOf(elem)
			smallSize = 1
			this
		}
		override def addOne(elem :T) =
			if (smallSize < 0) {
				if (set.add(elem))
					large += elem
				this
			} else if (small == null)
				addFirst(elem)
			else {
				val hash = hashCodeOf(elem)
				if (!contains(elem, hash)) {
					if (smallSize == SmallRankingCap) {
						switchToLarge()
						large += elem
						set   += elem
					} else {
						small(smallSize)  = elem
						hashes(smallSize) = hash
						smallSize += 1
					}
				}
				this
			}

		def prependAll(xs :IterableOnce[T]) :this.type = xs match {
			case empty   :Iterable[_] if empty.isEmpty      => this
			case empty   :Iterator[_] if !empty.hasNext     => this
			case single  :Iterable[T] if single.sizeIs == 1 => addOne(single.head)
			case seq     :IndexedSeq[T]                     => addAll(ReversedSeq(seq))
			case seq     :collection.Seq[T]                 => addAll(seq.reverseIterator)
			case ranking :Ranking[T]                        => addAll(new ReversedRanking(ranking))
			case _ => addAll(ReversedIndexedBuffer.empty[T] ++= xs)
		}

		override def clear() :Unit = {
			smallSize = 0
			if (set != null)
				set.clear()
			if (large != null)
				large.clear()
			if (small != null)
				java.util.Arrays.fill(small.asAnyRefArray, null)
		}
		@inline private def clear(result :Ranking[T]) :Ranking[T] = {
			clear(); result
		}

		override def result() :Ranking[T] = smallSize match {
			case 0  => clear(); Ranking.empty[T]
			case 1  => clear(new SingletonRanking(small(0)))
			case n if n > 0 =>
				val reversedItems  = reverse(small.asAnyRefArray, n).asInstanceOf[RefArray[T]]
				val reversedHashes = reverse(hashes, n)
				clear()
				new SmallRanking[T](reversedItems, reversedHashes)
			case _ => clear(new IndexedRanking(DefaultIndexedSeq.from(large.reverseIterator)))
		}

		private def reverse[X](elems :Array[X], prefixLength :Int) :Array[X] = {
			val res = Array.like(elems, prefixLength)
			var i   = prefixLength
			var j = 0
			while (i > 0) {
				i -= 1
				res(j) = elems(i)
				j += 1
			}
			res
		}
	}

}




///** Base class for `Ranking` implementations. In a departure from the scheme established by the scala collection library,
//  * it actually provides implementations of some of the inherited methods (including overrides).
//  * This is because they rely specifically on [[net.noresttherein.sugar.collections.Ranking$ Ranking]] being
//  * its `iterableFactory`, and return statically a `Ranking`, unlike the generic template methods
//  * in [[net.noresttherein.sugar.collections.RankingOps RankingOps]]. Placing these methods directly
//  * in `Ranking` instead, would complicate creation of future subtypes, as methods inherited from `Ranking`
//  * would not conform to the signatures of `RankingOps` extended with more specific collection types as type parameters.
//  *
//  * At the same time, this class addresses a need to provide a base class/trait requiring minimal method overrides
//  * by ad-hoc implementations. This is even more useful because it leverages its access to protected `Ranking`
//  * information, providing some optimizations which cannot be easily duplicated by external classes.
//  */
/** Base class for deault implementations of `Ranking` trait, that is implementing
  * [[net.noresttherein.sugar.collections.RankingOps RankingOps]]`[E, Ranking, Ranking[E]]` (and not a subtype).
  */
trait AbstractRanking[+E] extends AbstractIterable[E] with Ranking[E] {
	@tailrec
	private def updatedAllImpl[U >: E](index :Int, elems :IterableOnce[U],
	                                   updatedOne :(AbstractRanking[E], Int, U) => Ranking[U],
	                                   updatedSet :(AbstractRanking[E], Int, collection.Set[U]) => Ranking[U],
	                                   updatedRanking :(AbstractRanking[E], Int, Ranking[U]) => Ranking[U]) :Ranking[U] =
		elems match {
			case _       :View[U]                           =>
				updatedAllImpl(index, Ranking.from(elems), updatedOne, updatedSet, updatedRanking)
			case _ if index < 0 || index >= knownSize       => //covers also this.size == 0
				if (index == knownSize && elems.toBasicOps.isEmpty)
					this
				else
					throw new IndexOutOfBoundsException(
						"Cannot update " + errorString(this) + " at " + index + " with " + errorString(elems)
					)
			case empty   :Iterable[U] if empty.isEmpty      => this
			case empty   :Iterator[U] if !empty.hasNext     => this
			case single  :Iterable[u] if single.sizeIs == 1 => updatedOne(this, index, single.head)
			case set     :collection.Set[U @unchecked]      => updatedSet(this, index, set)
			case _                                          => updatedRanking(this, index, Ranking.from(elems))
		}


	override def updatedAll[U >: E](index :Int, elems :IterableOnce[U]) :Ranking[U] =
		updatedAllImpl[U](index, elems, _.updated(_, _),
			_.updatedAll[U, collection.Set[U]](_, _, (set :collection.Set[U], elem :U) => set.contains(elem)),
			_.updatedAll[U, Ranking[U]](_, _, (ranking :Ranking[U], elem :U) => ranking.contains(elem))
		)

	protected def updatedAll[U >: E, C <: Iterable[U]]
	                        (index :Int, unique :C, contains :(C, U) => Boolean) :Ranking[U] =
	{
		val res     = Ranking.newBuilder[U]
		val length  = knownSize
		var newSize = 0
		var i       = 0
		while (newSize < index) {
			val elem = apply(i)
			if (!contains(unique, elem)) {
				res += elem
				newSize += 1
			}
			i += 1
		}
		val it   = unique.iterator
		var skip = 0
		while (it.hasNext) {
			val elem = it.next()
			if (indexOf(elem) < 0)
				skip += 1
			res += elem
		}
		while (i < length) {
			val elem = apply(i)
			if (!contains(unique, elem))
				if (skip > 0)
					skip -= 1
				else
					res += elem
			i += 1
		}
		val ranking = res.result()
		if (ranking.size != length)
			throw new IndexOutOfBoundsException(
				"Patch collection too large for " + errorString(this) + ".updatedAll(" + index + ", " +
					errorString(unique) + "); result: " + errorString(ranking) + ")"
			)
		ranking
	}

	override def replaceAll[U >: E](index :Int, elems :IterableOnce[U]) :Ranking[U] =
		updatedAllImpl[U](index, elems, _.replace[U](_, _),
			_.replaceAll[U, collection.Set[U]](_, _, (set :collection.Set[U], elem :U) => set.contains(elem)),
			_.replaceAll[U, Ranking[U]](_, _, (ranking :Ranking[U], elem :U) => ranking.contains(elem))
		)

	protected def replaceAll[U >: E, C <: Iterable[U]]
	                        (index :Int, unique :C, contains :(C, U) => Boolean) :Ranking[U] =
	{
		val res     = Ranking.newBuilder[U]
		val length  = knownSize
		var i       = 0
		var prefix  = 0
		while (i < index) {
			val elem = apply(i)
			if (!contains(unique, elem)) {
				res    += elem
				prefix += 1
			}
			i += 1
		}
		if (prefix + unique.size > length)
			throw new IndexOutOfBoundsException(
				"Patch collection too large for " + errorString(this) + ".replaceAll(" + index + ", " +
					errorString(unique) + ")."
			)
		res ++= unique
		i   += unique.size
		while (i < length) {
			val elem = apply(i)
			if (!contains(unique, elem))
				res += elem
			i += 1
		}
		res.result()
	}

	override def insertedAll[U >: E](index :Int, elems :IterableOnce[U]) :Ranking[U] =
		elems match {
			case _ if index >= knownSize                    => appendedAll(elems)
			case _       :View[U]                           => insertedAll(index, Ranking.lastOccurrences(elems))
			case empty   :Iterable[U] if empty.isEmpty      => this
			case empty   :Iterator[U] if !empty.hasNext     => this
			case single  :Iterable[U] if single.sizeIs == 1 => inserted(index, single.head)
			case set     :collection.Set[U @unchecked] =>
				try insertedAll[U, collection.Set[U]](index, set, _.contains(_)) catch {
					case _ :ClassCastException =>
						insertedAll[U, Ranking[U]](index, Ranking.lastOccurrences[U](elems), _.contains(_))
				}
			case _ =>
				//todo: use an InverseRanking
				val ranking = Ranking.lastOccurrences(elems)
				insertedAll[U, Ranking[U]](index, ranking, _.contains(_))
		}
	//The function argument doesn't require creating a closure, so we get polymorphism for free.
	protected def insertedAll[U >: E, C <: Iterable[U]]
	                         (index :Int, unique :C, contains: (C, U) => Boolean) :Ranking[U] =
	{
		val res = Ranking.newBuilder[U]
		val it  = iterator
		var i   = 0
		while (it.hasNext & i < index) {
			val elem = it.next()
			if (!contains(unique, elem)) {
				res addOne elem
				i += 1
			}
		}
		res ++= unique
		while (it.hasNext)
			res += it.next()
		res.result()
	}

	override def added[U >: E](elem :U) :Ranking[U] =
		if (isEmpty) Ranking.one(elem)
		else if (contains(elem)) this
		else (Ranking.newBuilder[U](size + 1) ++= this += elem).result()

	override def appended[U >: E](elem :U) :Ranking[U] =
		if (isEmpty) Ranking.one(elem)
		else if (last == elem) this
		else (Ranking.appendingBuilder[U](size) ++= this += elem).result()

	override def prepended[U >: E](elem :U) :Ranking[U] =
		if (isEmpty) Ranking.one(elem)
		else if (head == elem) this
		else (Ranking.newBuilder[U](if (contains(elem)) size + 1 else size) += elem ++= this).result()

	override def union[U >: E](elems :IterableOnce[U]) :Ranking[U] = {
		if (isEmpty) Ranking.from(elems)
		else if (elems.knownSize == 0) this
		else (Ranking.unorderedBuilder[U] ++= this ++= elems).result()
	}
	override def appendedAll[U >: E](suffix :IterableOnce[U]) :Ranking[U] = {
		val thatSize = suffix.knownSize
		if (thatSize == 0)
			this
		else if (isEmpty)
			(Ranking.appendingBuilder[U] ++= suffix).result()
		else
			(Ranking.appendingBuilder[U] ++= this ++= suffix).result()
	}

	/** Creates a `r :Ranking` such that `r(permutation(i)) == this(i)`. */
	//todo: move it up, perhaps add fromSpecific(Array) to the super type.
	override def reorder(permutation :Permutation) :Ranking[E] =
		if (permutation.length != size)
			throw new IllegalArgumentException(
				"Cannot reorder " + this + " according to " + permutation +
					" because it is of a wrong size: expected " + size + ", but got " + permutation.length + "."

			)
		else if (size <= 1)
			this
		else if (permutation.isIdentity) //covers also empty and singleton rankings
			this
		else if (ApplyPreferred(permutation.toIndexedSeq) && applyPreferred) {
			val size = this.size
			val seq  = RefArray.ofDim[E](size)
			var i    = size
			while (i > 0) {
				i -= 1
				seq(permutation(i)) = this(i)
			}
			if (size <= SmallRankingCap)
				new SmallRanking(seq)
			else
				new IndexedRanking(IRefArray.Wrapped(seq.asIRefArray))
		} else {
			val size = this.size
			val seq  = RefArray.ofDim[E](size)
			val from = iterator
			val to   = permutation.iterator
			while (to.hasNext)
				seq(to.next()) = from.next()
			new IndexedRanking(IRefArray.Wrapped(seq.asIRefArray))
		}

	override def empty :Ranking[E] = EmptyRanking //reusableEmpty
}




@SerialVersionUID(Ver)
private class EmptyRanking
	extends AbstractRanking[Nothing] with StrictOptimizedIterableOps[Nothing, Ranking, Ranking[Nothing]]
	   with EmptyIterableOps.Generic[Ranking]
{
	override def one[T](elem :T) :Ranking[T] = Ranking.one(elem)

	override def apply(index :Int) :Nothing = outOfBounds("apply", index)

	override def indexOf[U](elem :U) :Int = -1
	override def segmentLength(p :Nothing => Boolean, from :Int) :Int = 0
	override def indexWhere(p :Nothing => Boolean, from :Int) :Int = -1
	override def lastIndexWhere(p :Nothing => Boolean, end :Int) :Int = -1
	override def containsAll[U >: Nothing](elems :IterableOnce[U]) :Boolean = elems.toBasicOps.isEmpty

	override def inserted[U >: Nothing](index :Int, elem :U) :Ranking[U] = Ranking.one(elem)
	override def insertedAll[U >: Nothing](index :Int, elems :IterableOnce[U]) :Ranking[U] =
		Ranking.lastOccurrences(elems)

	override def patch[U >: Nothing](index :Int, other :IterableOnce[U], replaced :Int) :Ranking[U] =
		Ranking.from(other)

	override def prependedAll[U](prefix :IterableOnce[U]) :Ranking[U] = Ranking.from(prefix)
	//Remember we must keep the last among all duplicates!
	override def appendedAll[U](suffix :IterableOnce[U]) :Ranking[U] = suffix match {
		case view    :View[U   ]                      => appendedAll(view.iterator)
		case ranking :Ranking[U]                      => ranking
		case seq     :RankingSeqAdapter[U]            => seq.toRanking
		case set     :RankingSetAdapter[U]            => set.toRanking
		case iter    :Iterable[_] if iter.isEmpty     => this
		case iter    :Iterator[_] if iter.isEmpty     => this
		case iter    :Iterable[U] if iter.sizeIs == 1 =>
			new SingletonRanking[U](iter.head)
		case iter    :Iterable[U] if iter.sizeIs <= SmallRankingCap =>
			deduplicateSmall(iter, false)
		case iter    :Iterable[U] =>
			deduplicateLarge(iter, false)
		case _ => (Ranking.appendingBuilder[U] ++= suffix).result()
	}
	override def concat[U >: Nothing](suffix :IterableOnce[U]) :Ranking[U] = Ranking.from(suffix)

	override def reverse :Ranking[Nothing] = this
	override def reverseIterator :Iterator[Nothing] = Iterator.empty

	override def toSeq        :Seq[Nothing] = Nil
	override def toIndexedSeq :IndexedSeq[Nothing] = Vector.empty
	override def view         :IndexedSeqView[Nothing] = EmptyIndexedSeqOps.view

	private def readResolve :Ranking[Nothing] = Ranking.empty
}


private object EmptyRanking extends EmptyRanking




@SerialVersionUID(Ver)
private final class SingletonRanking[+E](override val head :E)
	extends AbstractRanking[E] with SingletonIterableOps.Generic[E, Ranking]
{
	protected override def one[X](elem :X) :Ranking[X] = new SingletonRanking(elem)

	protected override def two[X](first :X, second :X) :Ranking[X] =
		if (first == second) new SingletonRanking(first)
		else new SmallRanking(RefArray.two(first, second))

	override def indexOf[U >: E](elem :U) = if (head == elem) 0 else -1
	override def indexWhere(p :E => Boolean, from :Int) :Int = if (from <= 0 && p(head)) 0 else -1
	override def lastIndexWhere(p :E => Boolean, end :Int) = if (end >= 0 && p(head)) 0 else - 1

	override def apply(index :Int) = if (index == 0) head else outOfBounds("apply", index)

	override def updated[U >: E](index :Int, elem :U) :Ranking[U] =
		if (index == 0) new SingletonRanking(elem)
		else throw new IndexOutOfBoundsException(errorString(this) + ".updated(" + index + ", " + elem + ")")

	override def inserted[U >: E](index :Int, elem :U) :Ranking[U] =
		if (index <= 0) two(elem, head)
		else two(head, elem)

	override def replace[U >: E](index :Int, elem :U) :Ranking[U] =
		if (index == 0) new SingletonRanking(elem) else outOfBounds("replace", index)

	override def incl[U >: E](elem :U) :Ranking[U] = two(head, elem)
	override def added[U >: E](elem :U) = two(head, elem)
	override def prepended[U >: E](elem :U) = two(elem, head)
	override def appended[U >: E](elem :U) = two(head, elem)

	protected override def updatedAll[U >: E, C <: Iterable[U]]
	                                 (index :Int, unique :C, contains :(C, U) => Boolean) :Ranking[U] =
		unique.size match {
			case 0 => this //in the unlikely case original elems were neither an Iterable nor an Iterator
			case 1 if index == 0 => Ranking.from(unique)
			case _ => throw new IndexOutOfBoundsException(
				errorString(this) + ".updatedAll(" + index + ", " + errorString(unique) + "): patch collection too large."
			)
		}
	protected override def replaceAll[U >: E, C <: Iterable[U]]
	                                 (index :Int, unique :C, contains :(C, U) => Boolean) :Ranking[U] =
		updatedAll[U, C](index, unique, contains)

	protected override def insertedAll[U >: E, C <: Iterable[U]]
	                                  (index :Int, unique :C, contains :(C, U) => Boolean) :Ranking[U] =
		//index == 0, because index == 1 is covered by the public insertedAll before calling us
		unique match {
			case ranking :Ranking[U]         => ranking + head
			case _ if contains(unique, head) => Ranking.from(unique)
			case _                           =>
				val res = Ranking.newBuilder[U]
				val size = unique.knownSize
				if (size >= 0)
					res sizeHint size + 1
				(res ++= unique += head).result()
		}

	@tailrec override def prependedAll[U >: E](prefix :IterableOnce[U]) = prefix match {
		case view    :View[U]                           => prependedAll(view.iterator)
		case empty   :Iterable[_] if empty.isEmpty      => this
		case ranking :Ranking[U] => ranking + head
		case set :collection.Set[U @unchecked]
			if (try set.contains(head) catch { case _ :ClassCastException => false })
		=>
			Ranking.from(prefix)
		case _ =>
			(Ranking.newBuilder[U] ++= prefix += head).result()
	}
	override def appendedAll[U >: E](suffix :IterableOnce[U]) = addEasy(suffix) match {
		case null => suffix match {
			case rank :Ranking[U] =>
				if (rank.contains(head)) rank else head +: rank
			case set    :collection.Set[U @unchecked]
			             if (try set.contains(head) catch { case _:ClassCastException => false }) =>
				Ranking.from(suffix)
			case _                =>
				(Ranking.appendingBuilder[U] += head ++= suffix).result()
		}
		case res  => res
	}

	override def concat[U >: E](suffix :IterableOnce[U]) :Ranking[U] = addEasy(suffix) match {
		case null => suffix match {
			case rank :Ranking[U] => head +: rank
			case _                => (Ranking.newBuilder[U] += head ++= suffix).result()
		}
		case res  => res
	}
	override def union[U >: E](elems :IterableOnce[U]) :Ranking[U] = addEasy(elems) match {
		case null => elems match {
			case rank   :Ranking[U] =>
				if (rank.contains(head)) rank else head +: rank
			case empty  :Iterator[_] if !empty.hasNext =>
				this
			case set    :collection.Set[U @unchecked]
			             if (try set.contains(head) catch { case _:ClassCastException => false }) =>
				Ranking.from(set)
			case _ =>
				(Ranking.unorderedBuilder[U] ++= this ++= elems).result()
		}
		case res  => res
	}

	private def addEasy[U >: E](elems :IterableOnce[U]) :Ranking[U] = elems match {
		case _ if elems.knownSize == 0                 => this
		case _      :View[U]                           => null
		case empty  :Iterable[_] if empty.isEmpty      => this
		case single :Iterable[U] if single.sizeIs == 1 => two(head, single.head)
		case empty  :Iterator[_] if !empty.hasNext     => this
		case _                                         => null
	}


	override def patch[U >: E](index :Int, other :IterableOnce[U], replaced :Int) :Ranking[U] =
		if (index <= 0 && replaced > 0) Ranking.from(other)
		else if (index <= 0) Ranking.from(other.iterator :+ head)
		else Ranking.from(head +: other.iterator)

	override def removedOne[U >: E](elem :U) :Ranking[E] =
		if (elem == head) Ranking.empty[E] else this

	@tailrec override def removedAll[U >: E](elems :IterableOnce[U]) :Ranking[E] = elems match {
		case ranking :Ranking[U] => if (ranking.contains(head)) Ranking.empty else this
		case set :HashSet[U @unchecked] => if (set(head)) Ranking.empty else this
		case set :mutable.HashSet[U @unchecked] => if (set(head)) Ranking.empty else this
		case set :mutable.LinkedHashSet[U @unchecked] => if (set(head)) Ranking.empty else this
		case view :View[U] => removedAll(view.iterator)
		case empty :Iterable[_] if empty.isEmpty => this
		case _ =>
			val it = elems.iterator
			var unequal = false
			while (it.hasNext && { unequal = it.next() != head; unequal })
				()
			if (unequal) this else Ranking.empty
	}

	override def reverse :Ranking[E] = this
	override def reverseIterator = Iterator.single(head)

	override def toSeq :Seq[E] = head::Nil
	override def toIndexedSeq :IndexedSeq[E] = IndexedSeq.one(head)
	override def toSet[U >: E] :Set[U] = Set.empty[U] + head
	override def view :IndexedSeqView[E] = SingletonIndexedSeqOps.view(head)
	override def toString = "Ranking(" + head + ")"
}




@SerialVersionUID(Ver) //todo: migrate to IRefArray.
private final class SmallRanking[+E](elements :RefArray[E], hashes :Array[Int])
	extends AbstractRanking[E] with StrictOptimizedIterableOps[E, Ranking, Ranking[E]]
	   with ArraySliceOps[E, Ranking, Ranking[E]]
{
	def this(elements :RefArray[E]) = this(elements, elements.asAnyArray.map(hashCodeOf))

	private[sugar] override def unsafeArray :Array[_] = elements.asAnyArray
	private[sugar] override def startIndex  :Int = 0
	private[sugar] def array :RefArray[E @uncheckedVariance] = elements
	private[sugar] def hashCodes :Array[Int] = hashes

	@inline override def knownSize = hashes.length

	override def indexOf[U >: E](elem :U) :Int = indexOf(elem, hashCodeOf(elem))

	def indexOf[U >: E](elem :U, hash :Int) :Int = {
		var i = hashes.length - 1
		while (i >= 0 && (hashes(i) != hash || elements(i) != elem))
			i -= 1
		i
	}
	override def segmentLength(p :E => Boolean, from :Int) :Int = elements.segmentLength(p, from)
	override def indexWhere(p :E => Boolean, from :Int) :Int = elements.indexWhere(p, from)
	override def lastIndexWhere(p :E => Boolean, end :Int) :Int = elements.lastIndexWhere(p, end)

	protected override def clippedSlice(from :Int, until :Int) :Ranking[E] =
		until - from match {
			case 0 => EmptyRanking
			case 1 => new SingletonRanking(elements(from))
			case _ => new SmallRanking(elements.slice(from, until), hashes.slice(from, until))
		}

	override def map[B](f :E => B) :Ranking[B] = new SmallRanking(elements.map(f))

	override def filterImpl(pred :E => Boolean, flipped :Boolean) :Ranking[E] =
		filterWith(pred)(_.apply(_), flipped)

	/* A more generic implementation of filter, which, instead of a single argument predicate,
	 * takes a two argument function and the second, generic argument for that function.
	 * This allows, for example, to use Ranking.contains by passing a Ranking, and _.contains(_) - which does not
	 * require a closure.
	 */
	private def filterWith[C](arg :C)(test :(C, E) => Boolean, flipped :Boolean) :Ranking[E] = {
		val Size    = hashes.length
		val remove  = new Array[Boolean](Size)
		val newSize = markForRemoval(arg, remove)(test, flipped)
		select(remove, newSize)
	}
	private def markForRemoval[C](arg :C, remove :Array[Boolean])
	                             (test :(C, E) => Boolean, keepWhen :Boolean) :Int =
	{
		val size   = hashes.length
		var newSize = 0
		var i       = 0
		while (i < size) {
			if (test(arg, elements(i)) == keepWhen)
				remove(i) = true
			else
				newSize += 1
			i += 1
		}
		newSize
	}
	private def select(remove :Array[Boolean], newSize :Int) :Ranking[E] = {
		val Size = hashes.length
		newSize match {
			case Size => this
			case 0 => Ranking.empty
			case 1 => new SingletonRanking(elements(remove.indexOf(false)))
			case _ =>
				val newElems  = RefArray.ofDim[E](newSize)
				val newHashes = new Array[Int](newSize)
				var i = 0
				var j = 0
				while (i < Size & j < newSize) {
					if (!remove(i)) {
						newElems(j)  = elements(i)
						newHashes(j) = hashes(i)
						j += 1
					}
					i += 1
				}
				new SmallRanking(newElems, newHashes)
		}
	}

	override def apply(index :Int) = elements(index)

	override def swapped(idx1 :Int, idx2 :Int) :Ranking[E] =
		if (idx1 == idx2 && idx1 >= 0 && idx2 >= 0 && idx1 < hashes.length && idx2 < hashes.length)
			this
		else {
			val newElems    = RefArray.copyOf(elements)
			val newHashes   = copyOf(hashes, hashes.length)
			newElems(idx1)  = elements(idx2)
			newHashes(idx1) = hashes(idx2)
			newElems(idx2)  = elements(idx1)
			newHashes(idx2) = hashes(idx2)
			new SmallRanking(newElems, newHashes)
		}
	override def swapped(idx1 :Int, idx2 :Int, length :Int) :Ranking[E] = {
		val size = hashes.length
		val length0 = math.max(length, 0)
		if (if (idx1 < idx2) idx1 < 0 | idx2 > size - length0 else idx2 < 0 | idx1 > size - length0)
			throw new IndexOutOfBoundsException(
				errorString(this) + ".swapped(" + idx1 + ", " + idx2 + ", " + length + ")"
			)
		if (idx1 == idx2 | length <= 0)
			this
		else if (if (idx1 < idx2) idx1 + length0 > idx2 else idx2 + length0 > idx1)
			throw new IllegalArgumentException(
				errorString(this) + ".swapped(" + idx1 + ", " + idx2 + ", " + length + "): swapped ranges overlap."
			)
		else {
			val newElems  = RefArray.copyOf(elements)
			val newHashes = copyOf(hashes, hashes.length)
			arraycopy(elements, idx1, newElems, idx2, length)
			arraycopy(hashes, idx1, newHashes, idx2, length)
			arraycopy(elements, idx2, newElems, idx1, length)
			arraycopy(hashes, idx2, newHashes, idx1, length)
			new SmallRanking(newElems, newHashes)
		}
	}

	protected override def trustedRotatedLeft(from :Int, until :Int)(n :Int) :Ranking[E] = {
		val size      = hashes.length
		val split     = from + n
		val newElems  = RefArray.copyOf(elements, size)
		val newHashes = copyOf(hashes, size)
		arraycopy(elements, from, newElems, until - n, n)
		arraycopy(hashes, from, newHashes, until - n, n)
		arraycopy(elements, split, newElems, from, until - split)
		arraycopy(hashes, split, newHashes, from, until - split)
		new SmallRanking(newElems, newHashes)
	}

	override def updated[U >: E](index :Int, elem :U) = {
		val hash = hashCodeOf(elem)
		indexOf(elem, hash) match {
			case n if n == index =>
				this
			case -1 =>
				substitute(index, elem, hash)
			case n =>
				moved(n, index)
		}
	}
	override def replace[U >: E](index :Int, elem :U) :Ranking[U] = {
		val hash   = hashCodeOf(elem)
		val length = hashes.length
		indexOf(elem, hash) match {
			case  -1 =>
				substitute(index, elem, hash)
			case old if old == index =>
				this
			case old =>
				val newHashes = copyOf(hashes, length - 1)
				val newElems  = RefArray.copyOf[U](elements, length - 1)
				arraycopy(hashes, old + 1, newHashes, old, length - old - 1)
				arraycopy(elements, old + 1, newElems, old, length - old - 1)
				val i = if (old > index) index else index - 1
				newHashes(i) = hash
				newElems(i)  = elem
				new SmallRanking(newElems, newHashes)
		}
	}
	override def inserted[U >: E](index :Int, elem :U) :Ranking[U] = {
		val hash = hashCodeOf(elem)
		val size = hashes.length
		indexOf(elem, hash) match {
			case  -1 if index <= 0    => prependAbsent(elem, hash)
			case  -1 if index >= size => appendAbsent(elem, hash)
			case  -1 if size < SmallRankingCap =>
				val index0 = math.max(0, math.min(size, index))
				val newHashes = copyOf(hashes, size + 1)
				val newElems  = RefArray.copyOf[U](elements, size + 1)
				arraycopy(elements, index0, newElems, index + 1, size - index0)
				arraycopy(hashes, index0, newHashes, index + 1, size - index0)
				newHashes(index0) = hash
				newElems(index0)  = elem
				new SmallRanking(newElems, newHashes)
			case -1 =>
				val b = DefaultIndexedSeq.newBuilder[U]
				b sizeHint SmallRankingCap + 1
				b ++= RefArraySlice.slice(elements, 0, index)
				b += elem
				b ++= RefArraySlice.slice(elements, index, SmallRankingCap)
				new IndexedRanking(b.result())
			case old =>
				moved(old, math.max(0, math.min(size - 1, index)))
		}
	}
	private def substitute[U >: E](index :Int, elem :U, hash :Int) = {
		val length       = elements.length
		val newHashes    = copyOf(hashes, length)
		val newElems     = RefArray.copyOf[U](elements, length)
		newHashes(index) = hash
		newElems(index)  = elem
		new SmallRanking(newElems, newHashes)
	}

	override def insertedAll[U >: E, C <: Iterable[U]]
	                        (index :Int, unique :C, contains: (C, U) => Boolean) :Ranking[U] =
	{
		val length        = hashes.length
		val builderElems  = RefArray.ofDim[U](SmallRankingCap)
		val builderHashes = new Array[Int](SmallRankingCap)
		var i             = 0
		var newSize       = 0
		while (newSize < index & i < length) {
			val e = elements(i)
			if (!contains(unique, e)) {
				builderElems(newSize)  = e
				builderHashes(newSize) = hashes(i)
				newSize += 1
			}
			i += 1
		}
		val res = new RankingBuilder[U](builderElems, builderHashes, newSize)
		res ++= unique
		while (i < length) {
			res += elements(i)
			i += 1
		}
		res.result()
	}

	override def removed(index :Int) :Ranking[E] = hashes.length match {
		case length if index < 0 | index >= length =>
			throw new IndexOutOfBoundsException(errorString (this) + ".removed(" + index + ")")
		case 1      => Ranking.empty
		case 2      => Ranking.one(elements(1 - index))
		case length if length - 1 == index =>
			new SmallRanking(RefArray.copyOf(elements, index), copyOf(hashes, index))
		case length =>
			//consider: copyOfRanges probably has too high overhead for such small arrays
			val newElems  = RefArray.copyOfRanges(elements, 0, index, elements, index + 1, length)
			val newHashes = Array.copyOfRanges(hashes, 0, index, hashes, index + 1, length)
			new SmallRanking(newElems, newHashes)
	}
	override def removed(from :Int, until :Int) :Ranking[E] = {
		val length = hashes.length
		if (until <= from | from >= length | until < 0)
			this
		else {
			val from0     = math.max(from, 0)
			val until0    = math.min(until, length)
			val newLength = until0 - from0
			length - newLength match {
				case 0 => Ranking.empty
				case 1 => new SingletonRanking(elements(if (from > 0) 0 else length - 1))
				case diff =>
					if (from == diff)
						new SmallRanking(elements.dropRight(diff), hashes.dropRight(diff))
					else if (from == 0)
						new SmallRanking(elements.drop(diff), hashes.drop(diff))
					else {
						val newElems  = RefArray.copyOf(elements, newLength)
						val newHashes = copyOf(hashes, newLength)
						arraycopy(elements, until0, newElems, from0, length - until0)
						arraycopy(hashes, until0, newHashes, from0, length - until0)
						new SmallRanking(newElems, newHashes)
					}
			}
		}
	}

	override def removedAll[U >: E](elems :IterableOnce[U]) = elems match {
		case empty :Iterable[_] if empty.isEmpty =>
			this
		case _ =>
			var size   = hashes.length
			val remove = new Array[Boolean](size)
			val it     = elems.iterator
			while (size > 0 && it.hasNext)
				indexOf(it.next()) match {
					case n if n >= 0 && !remove(n) =>
						remove(n) = true
						size -= 1
					case _ =>
				}
			select(remove, size)
	}

	override def prepended[U >: E](elem :U) = {
		val hash = hashCodeOf(elem)
		indexOf(elem, hash) match {
			case -1 => prependAbsent(elem, hash)
			case  0 => this
			case  n =>
				val size = hashes.length
				val res  = RefArray.copyOf[U](elements, size)
				val hs   = copyOf(hashes, size)
				arraycopy(elements, 0, res, 1, n)
				arraycopy(hashes, 0, hs, 1, n)
				res(0) = elem
				hs(0)  = hash
				new SmallRanking(res, hs)
		}
	}

	private def prependAbsent[U >: E](elem :U, hash :Int) :Ranking[U] =
		if (hashes.length >= SmallRankingCap)
			new IndexedRanking(elem +: IRefArray.Wrapped(elements.asIRefArray))
		else {
			val length = hashes.length
			val res = RefArray.ofDim[U](length + 1)
			val hs  = new Array[Int](length + 1)
			res(0)  = elem
			hs(0)   = hash
			arraycopy(elements, 0, res, 1, length)
			arraycopy(hashes, 0, hs, 1, length)
			new SmallRanking(res, hs)
		}


	override def appended[U >: E](elem :U) = {
		val hash = hashCodeOf(elem)
		indexOf(elem, hash) match {
			case -1 => appendAbsent(elem, hash)
			case n if n == hashes.length - 1 => this
			case n =>
				val size = hashes.length
				val last = hashes.length - 1
				val res  = RefArray.copyOf[U](elements, size)
				val hs   = copyOf(hashes, size)
				arraycopy(elements, n + 1, res, n, last - n)
				arraycopy(hashes, n + 1, hs, n, last - n)
				res(last) = elem
				hs(last)  = hash
				new SmallRanking(res, hs)
		}
	}
	override def added[U >: E](elem :U) = {
		val hash = hashCodeOf(elem)
		indexOf(elem, hash) match {
			case -1 => appendAbsent(elem, hash)
			case  _ => this
		}
	}
	private def appendAbsent[U >: E](elem :U, hash :Int) :Ranking[U] =
		if (hashes.length >= SmallRankingCap)
			new IndexedRanking(elements.view.appended(elem).toIndexedSeq)
		else {
			val size  = hashes.length
			val res   = RefArray.copyOf[U](elements, size + 1)
			val hs    = copyOf(hashes, size + 1)
			res(size) = elem
			hs(size)  = hash
			new SmallRanking(res, hs)
		}


	@tailrec override def prependedAll[U >: E](prefix :IterableOnce[U]) :Ranking[U] = prefix match {
		case view    :View[U]                          => prependedAll[U](view.iterator)
		case items   :Iterable[U] if items.isEmpty     =>
			this
		case items   :Iterable[U] if items.sizeIs == 1 => items.head +: this
		case ranking :Ranking[U]                       => ranking concat this
		case set     :collection.Set[U] =>
			try prependedAll[U, collection.Set[U]](set, set, _.contains(_)) catch {
				case _ :ClassCastException => (Ranking.newBuilder[U] ++= prefix ++= this).result()
			}
		case iter    :Iterator[U] if !iter.hasNext =>
			this
		case _ => (Ranking.newBuilder[U] ++= prefix ++= this).result()
	}
	//filter out all our elements present in set and combine into a single sequence.
	private def prependedAll[U >: E, C <: Iterable[U]](prefix :IterableOnce[U],
	                                                   set :C, contains :(C, U) => Boolean) :Ranking[U] =
	{
		var prefixSize = set.knownSize
		if (prefixSize < 0)
			prefixSize = prefix.knownSize
		filterWith(set)(contains, true) match {
			case empty  if empty.isEmpty => Ranking.from(set)
			case suffix :SmallRanking[U] if prefixSize >= 0 && suffix.length + prefixSize <= SmallRankingCap =>
				val suffixLength = suffix.length
				val newArray  = RefArray.ofDim[U](prefixSize + suffixLength)
				val newHashes = new Array[Int](prefixSize + suffixLength)
				prefix.toBasicOps.copyToArray(newArray.asAnyArray, 0, prefixSize)
				computeHashes(newArray, newHashes, 0, prefixSize)
				arraycopy(suffix.array, 0, newArray, prefixSize, suffixLength)
				arraycopy(suffix.hashCodes, 0, newHashes, prefixSize, suffixLength)
				new SmallRanking(newArray, newHashes)
			case single :SingletonRanking[U] if prefixSize >= 0 && prefixSize < SmallRankingCap =>
				val newArray  = RefArray.ofDim[U](prefixSize + 1)
				prefix.toBasicOps.copyToArray(newArray.asAnyArray, 0, prefixSize)
				newArray(prefixSize) = single.head
				new SmallRanking(newArray)
			case suffix =>
				new IndexedRanking(prefix.toBasicOps.toIndexedSeq :++ suffix)
		}
	}

	@tailrec override def appendedAll[U >: E](suffix :IterableOnce[U]) :Ranking[U] =
		suffix match { //this covers in particular IndexedSeqView
			case seq :collection.IndexedSeqOps[U, generic.Any, _] => seq.length match {
				case 0 => this
				case 1 => appended(seq.head)
				case n => appendReversed(seq.reverseIterator, n)
			}
			case view    :View[U]                           => appendedAll[U](view.iterator)
			case empty   :Iterable[_] if empty.isEmpty      => this
			case empty   :Iterator[_] if !empty.hasNext     => this
			case single  :Iterable[U] if single.sizeIs == 1 => appended(single.head)
			case small   :SmallRanking[U]                   => appendedAll(small)
			case ranking :Ranking[U] =>
				appendedAll[U, Ranking[U]](ranking, ranking, _.contains(_))
			case set  :collection.Set[U] =>
				try appendedAll[U, collection.Set[U]](set, set, _.contains(_)) catch {
					case _ :ClassCastException => appendReversed(Iterators.reverse(set), set.knownSize)
				}
			case _ =>
				val size     = suffix.knownSize
				val reversed = Iterators.reverse(suffix)
				appendReversed(reversed, if (size >= 0) size else reversed.knownSize)
		}
	//filter out all our elements which are present in set and combine the results
	private def appendedAll[U >: E, C <: Iterable[U]](suffix :IterableOnce[U], set :C, contains :(C, U) => Boolean)
	       :Ranking[U] =
	{
		var suffixSize = set.knownSize
		if (suffixSize < 0)
			suffixSize = suffix.knownSize
		filterWith(set)(contains, true) match {
			case empty if empty.isEmpty => Ranking.from(set)
			case prefix :SmallRanking[U] if suffixSize >= 0 & prefix.length + suffixSize <= SmallRankingCap =>
				val prefixSize = prefix.length
				val newArray   = RefArray.copyOf(prefix.array, prefixSize + suffixSize)
				val newHashes  = copyOf(prefix.hashCodes, prefixSize + suffixSize)
				suffix.toBasicOps.copyToArray(newArray.asAnyArray, prefixSize, suffixSize)
				computeHashes(newArray, newHashes, prefixSize, prefixSize + suffixSize)
				new SmallRanking[U](newArray, newHashes)
			case single if suffixSize >= 0 & suffixSize + 1 <= SmallRankingCap && single.size == 1 =>
				val newArray = RefArray.ofDim[U](suffixSize + 1)
				newArray(0) = single.head
				suffix.toBasicOps.copyToArray(newArray.asAnyArray, 1, suffixSize)
				new SmallRanking(newArray)
			case prefix =>
				new IndexedRanking(prefix ++: set.toIndexedSeq)
		}
	}
	private def appendedAll[U >: E](suffix :SmallRanking[U]) :Ranking[U] = {
		val suffixSize = suffix.size
		val keep = new Array[Int](hashes.length)
		val kept = keepAbsent(keep, suffix)
		val resItems  = RefArray.ofDim[U](kept + suffixSize)
		if (kept + suffixSize <= SmallRankingCap) {
			val resHashes = new Array[Int](kept + suffixSize)
			copyKept(keep, resItems, resHashes, 0, kept)
			arraycopy(suffix.array, 0, resItems, kept, suffixSize)
			arraycopy(suffix.hashCodes, 0, resHashes, kept, suffixSize)
			new SmallRanking(resItems, resHashes)
		} else {
			//The way copyKept is implemented, it will overwrite keep with hashes of those kept.
			// We take advantage here that both hashes and keep are Array[Int] and use keep as a throw away sink.
			copyKept(keep, resItems, DummyHashArray, 0, kept)
			arraycopy(suffix.array, 0, resItems, kept, suffixSize)
			new IndexedRanking(IRefArray.Wrapped(resItems.asIRefArray))
		}
	}

	private def appendReversed[U >: E](reverse :Iterator[U], newElemCount :Int) = {
		if (newElemCount >= 0 & newElemCount + size <= SmallRankingCap)
			appendSmallReversed(reverse)
		else
			appendLargeReversed(reverse)
	}
	private def appendSmallReversed[U >: E](reverse :Iterator[U]) :Ranking[U] = {
		val elems   = RefArray.ofDim[U](SmallRankingCap)
		val hashes  = new Array[Int](SmallRankingCap)
		var lastPos = hashes.length
		def addIfAbsent(elem :U, hash :Int, downto :Int) = {
			var i = elems.length
			var notFound = true
			while (notFound & i > downto) {
				i -= 1
				notFound = hashes(i) != hash || elems(i) != elem
			}
			if (notFound) {
				val res     = downto - 1
				elems(res)  = elem
				hashes(res) = hash
				res
			} else
				downto
		}
		while (reverse.hasNext) {
			val next = reverse.next()
			val hash = hashCodeOf(next)
			lastPos = addIfAbsent(next, hash, lastPos)
		}
		var idx = this.hashes.length
		while (idx > 0) {
			idx -= 1
			val elem = this.elements(idx)
			val hash = this.hashes(idx)
			lastPos = addIfAbsent(elem, hash, lastPos)
		}
		if (lastPos == 0)
			new SmallRanking(elems, hashes)
		else
			new SmallRanking(elems.drop(lastPos), hashes.drop(lastPos))
	}
	private def appendLargeReversed[U >: E](reverse :Iterator[U]) :Ranking[U] = {
		val res = new ReverseBuilder[U]
		res ++= reverse
		var i = size
		while (i > 0) {
			i -= 1
			res += elements(i)
		}
		res.result()
	}


	@tailrec override def concat[U >: E](suffix :IterableOnce[U]) :Ranking[U] = suffix match {
		case view     :View[U]                           => concat(view.iterator)
		case iterable :Iterable[U] if iterable.isEmpty   => this
		case iterator :Iterator[_] if iterator.isEmpty   => this
		case single   :Iterable[U] if single.sizeIs == 1 => added(single.head)
		case ranking  :SmallRanking[U]                   => concat(ranking)
		case _ =>
			(Ranking.newBuilder[U] ++= this ++= suffix).result()
	}

	private def concat[U >: E](suffix :SmallRanking[U]) :Ranking[U] =  {
		val suffixSize = suffix.knownSize
		val size = hashes.length
		val keep = new Array[Int](size + suffixSize) //Supersized for use as a sink, see the comment further down.
		val kept = suffix.keepAbsent(keep, this)
		val resItems  = RefArray.copyOf[U](elements, kept + size)
		if (kept + size <= SmallRankingCap) {
			val resHashes = copyOf(hashes, kept + size)
			suffix.copyKept(keep, resItems, resHashes, size, kept)
			new SmallRanking(resItems, resHashes)
		} else {
			suffix.copyKept(keep, resItems, DummyHashArray, size, kept)
			new IndexedRanking(IRefArray.Wrapped(resItems.asIRefArray))
		}
	}

	@tailrec override def union[U >: E](suffix :IterableOnce[U]) :Ranking[U] = suffix match {
		case view     :View[U]                           => union(view.iterator)
		case iterable :Iterable[U] if iterable.isEmpty   => this
		case iterator :Iterator[_] if iterator.isEmpty   => this
		case ranking  :Ranking[U]                        => union(ranking)
		case single   :Iterable[U] if single.sizeIs == 1 => added(single.head)
		case set      :collection.Set[U]                 =>
			try union(set) catch {
				case _ :ClassCastException =>
					val thatSize = set.knownSize
					val res = Ranking.unorderedBuilder[U](if (thatSize >= 0) thatSize + size else  -1)
					(res ++= this ++= suffix).result()
			}
		case _ =>
			val thatSize = suffix.knownSize
			val res = Ranking.unorderedBuilder[U](if (thatSize >= 0) thatSize + size else  -1)
			(res ++= this ++= suffix).result()
	}

	private def union[U >: E](suffix :Ranking[U]) :Ranking[U] = suffix match {
		case small :SmallRanking[U]  =>
			if (hashes.length < small.size) small.prependedAll(this)
			else appendedAll(small)
		case _ if suffix.size == 1   => added(suffix.head)
		case _ if suffix.size > hashes.length =>
			new IndexedRanking[U](filterWith(suffix)(_.contains(_), true) ++: suffix.toIndexedSeq)
		case _ =>
			new IndexedRanking[U](toIndexedSeq :++ suffix.iterator.filterNot(contains))
	}
	private def union[U >: E](suffix :collection.Set[U]) :Ranking[U] = suffix.knownSize match {
		case 0 => this
		case 1 => added(suffix.head)
		case large if large > hashes.length  =>
			appendedAll[U, collection.Set[U]](suffix, suffix, _.contains(_))
		case _ => //we must use suffix.iterator.filterNot, not suffix.filterNot, because the latter may change the order
			new IndexedRanking(toIndexedSeq :++ suffix.iterator.filterNot(contains))
	}

	override def reverseIterator = toIndexedSeq.reverseIterator
	override def reverse = new SmallRanking(elements.reverse, hashes.reverse)
	override def toIndexedSeq = IRefArray.Wrapped(elements.asInstanceOf[IRefArray[E]])
	override def toSeq = toIndexedSeq
	override def to[C1](factory :Factory[E, C1]) :C1 = sourceCollectionFactory(factory) match {
		case Yes(companion) => companion match {
			case Seq | IndexedSeq | collection.Seq | collection.IndexedSeq => toIndexedSeq.castFrom[IndexedSeq[E], C1]
			case Set | collection.Set => toSet.castFrom[Set[E], C1]
			case Ranking => this.castFrom[Ranking[E], C1]
			case Yes(IRefArraySlice) | Yes(ArrayLikeSlice) | Yes(IArrayLikeSlice) =>
				IRefArraySlice.wrap(elements.asIRefArray).castFrom[IRefArraySlice[E], C1]
			//fixme: currently RelayArrayFactory equals PassedArrayInternals, not PassedArray
			case _ if RelayArrayFactory.contains(companion) =>
				RelayArrayFactory.get.wrap(elements.castFrom[RefArray[E], IArray[E]]).castFrom[IndexedSeq[E], C1]
			case _ =>
				factory.fromSpecific(this)
		}
		case _ => factory.fromSpecific(this)
	}

	/** Fills the `kept` array with indices of elements of this ranking which are not present in `other`.
	  * Returns the number of absent elements. */
	private def keepAbsent[U >: E](kept :Array[Int], other :SmallRanking[U]) :Int = {
		val size = hashes.length
		var i    = 0
		var keep = 0
		while (i < size) {
			val h = hashes(i)
			val e = elements(i)
			if (other.indexOf(e, h) < 0) {
				kept(keep) = i
				keep += 1
			}
			i += 1
		}
		keep
	}
	/** Copies elements of this ranking at indices listed in `kept`, together with their hashes,
	  * into `items` and `hashes`. Writing starts at index `offset`, and the first `size` elements of `kept`
	  * are copied. */
	private def copyKept[U >: E](kept :Array[Int], items :RefArray[U], hashes :Array[Int], offset :Int, size :Int) :Unit = {
		var i = 0
		while (i < size) {
			val idx = kept(i)
			items(offset + i)  = this.elements(idx)
			hashes(offset + i) = this.hashes(idx)
			i += 1
		}
	}

	private def computeHashes[U >: E](elems :RefArrayLike[U], hashes :Array[Int], from :Int, until :Int) :Unit = {
		var i = from
		while (i < until) {
			hashes(i) = hashCodeOf(elems(i))
			i += 1
		}
	}

	private def writeReplace = new RankingSerializer(elements)
}




//we might consider LinkedHashMap implementation for lesser footprint and faster building, but
//  1. it's mutable, hence likely problematic even if not shared,
//  2. it doesn't have any provision for iterating in the reverse order.
/** A `Ranking` with a separate (indexed) sequence of elements and reverse index map.
  * We prefer being created by a builder and initialized with an `ArraySeq`, but if any element is added
  * to this instance, the sequence is converted to something with better update and concat characteristics,
  * like a `Vector`. The map is upcast to a more general element (key) type, so it is defensively converted
  * to a hash map in these scenarios.
  */
@SerialVersionUID(Ver)
private class IndexedRanking[+T](items :IndexedSeq[T], map :Map[T, Int])
	extends AbstractIterable[T] with StrictOptimizedIterableOps[T, Ranking, Ranking[T]] with IterableProxy[T]
	   with AbstractRanking[T]
{
	def this(items :IndexedSeq[T]) =
		this(items, items.iterator.zipWithIndex.toMap)

	assert(items.length == map.size, "index " + map + " inconsistent with elements " + items)

	protected override def underlying :IndexedSeq[T] = items
	@inline private def sliceable :IndexedSeq[T] = if (HasFastSlice(items)) items else IndexedSeqFactory.from(items)
	@inline private def updatable :IndexedSeq[T] = if (HasFastUpdate(items)) items else IndexedSeqFactory.from(items)
	@inline private def appendable :IndexedSeq[T] = if (HasFastAppend(items)) items else IndexedSeqFactory.from(items)
	@inline private def prependable :IndexedSeq[T] = if (HasFastPrepend(items)) items else IndexedSeqFactory.from(items)

	override def knownSize :Int   = items.length

	override def indexOf[U >: T](elem :U) :Int =
		try map.getOrElse(elem.asInstanceOf[T], -1) catch {
			case _ :ClassCastException => -1
		}
	override def indexWhere(p :T => Boolean, from :Int) :Int = //scala 2.13.10 bug
		if (from >= items.length) -1 else items.indexWhere(p, from)

	override def apply(index :Int) :T = items(index)

	override def swapped(idx1 :Int, idx2 :Int) :Ranking[T] =
		if (idx1 == idx2 && idx1 >= 0 & idx2 >= 0 && idx1 < items.length && idx2 < items.length)
			this
		else {
			val first    = items(idx1)
			val second   = items(idx2)
			val newItems = updatable.updated(idx1, second).updated(idx2, first)
			val newIndex = map.updated(first, idx2).updated(second, idx1)
			new IndexedRanking(newItems, newIndex)
		}
	@tailrec final override def swapped(idx1 :Int, idx2 :Int, length :Int) :Ranking[T] =
		if (idx1 > idx2)
			swapped(idx2, idx1, length)
		else if (idx1 < 0 | idx2 > items.length - math.max(length, 0))
			throw new IndexOutOfBoundsException(
				errorString(this) + ".swapped(" + idx1 + ", " + idx2 + ", " + length + ")"
			)
		else if (length == 1)
			swapped(idx1, idx2)
		else if (idx1 == idx2 | length <= 0)
			this
		else if (idx1 + length > idx2)
			throw new IllegalArgumentException(
				errorString(this) + ".swapped(" + idx1 + ", " + idx2 + ", " + length + "): ranges overlap."
			)
		else {
			val end1 = idx1 + length
			val end2 = idx2 + length
			val newItems = {
				val builder = IndexedSeqFactory.newBuilder[T]
				builder sizeHint items.length
				//adding iterators, not items individually, because VectorBuilder recognizes VectorIterator
				if (idx1 > 0)
					builder ++= items.iterator.take(idx1)
				builder ++= items.iterator.slice(idx2, end2)
				if (idx2 > end1)
					builder ++= items.iterator.slice(end1, idx2)
				builder ++= items.iterator.slice(idx1, end1)
				if (end2 != items.length)
					builder ++= items.iterator.drop(end2)
				builder.result()
			}
			var newMap = map
			var i = 0
			while (i < length) {
				newMap = newMap.updated(items(idx1 + i), idx2 + i).updated(items(idx2 + i), idx1 + i)
				i += 1
			}
			new IndexedRanking(newItems, newMap)
		}

	protected override def trustedRotatedLeft(from :Int, until :Int)(n :Int) :Ranking[T] = {
		val newItems = items.rotatedLeft(from, until)(n)
		val newMap   = rotateIndexLeft(newItems, from, until)(n)
		new IndexedRanking(newItems, newMap)
	}

	private def rotateIndexLeft[U >: T](newItems :IndexedSeq[U], from :Int, until :Int)(n :Int) :HashMap[U, Int] =
		map match {
			case hm :HashMap[U, Int] @unchecked if IndexedIterable.updatePreferred(map, until - from) =>
				var newIndex = hm
				var i        = from
				while (i < until) {
					newIndex = newIndex.updated(newItems(i), i)
					i += 1
				}
				newIndex
			case _ =>
				HashMap.from(newItems.iterator.zipWithIndex)
		}

	override def updated[U >: T](index :Int, elem :U) =
		indexOf(elem) match {
			case -1 => replaceWithAbsent(index, elem)
			case n if n == index => this
			case n => moved(n, index)
		}

	override def replace[U >: T](index :Int, elem :U) :Ranking[U] =
		indexOf(elem) match {
			case -1 =>
				replaceWithAbsent(index, elem)
			case i if i == index =>
				this
			case old =>
				val b = IndexedSeqFactory.newBuilder[U]
				b sizeHint items.length
				if (old > index) {
					b ++=
						items.iterator.take(index) +=
						elem ++=
						items.iterator.slice(index + 1, old) ++=
						items.iterator.drop(old + 1)
					val newItems = b.result()
					val newMap   = rotateIndexLeft(newItems, index, old + 1)(old - index) - items(index)
					new IndexedRanking(newItems, newMap)
				} else {
					b ++=
						items.iterator.take(old) ++=
						items.iterator.slice(old + 1, index) +=
						elem ++=
						items.iterator.drop(index + 1)
					val newItems = b.result()
					val rotated  = rotateIndexLeft[U](newItems, old, index + 1)(1) - items(index)
					new IndexedRanking(newItems, rotated)
				}
		}

	private def replaceWithAbsent[U >: T](index :Int, elem :U) :Ranking[U] = {
		val newMap   = HashMap.from[U, Int](map).updated(elem, index) - items(index)
		val newItems = updatable.updated(index, elem)
		new IndexedRanking(newItems, newMap)
	}

	override def inserted[U >: T](index :Int, elem :U) :Ranking[U] =
		if (index <= 0)
			prepended(elem)
		else
			indexOf(elem) match {
				case  -1 =>
					val newItems = Vector.newBuilder[U]
					newItems sizeHint items.length + 1
					newItems ++= items.iterator.take(index) += elem
					if (index < items.length)
						newItems ++= items.iterator.drop(index)
					new IndexedRanking(newItems.result())
				case old =>
					moved(old, math.min(index, items.length - 1))
			}


	override def added[U >: T](elem :U) :Ranking[U] =
		if (contains(elem))
			this
		else
			new IndexedRanking(appendable :+ elem, HashMap.from[U, Int](map).updated(elem, size))

	override def appended[U >: T](elem :U) :Ranking[U] = indexOf(elem) match {
		case -1 =>
			new IndexedRanking(appendable :+ elem, HashMap.from[U, Int](map).updated(elem, size))
		case n =>
			rotatedLeft(n, items.length)(1)
	}

	override def prepended[U >: T](elem :U) :Ranking[U] = indexOf(elem) match {
		case -1 =>
			new IndexedRanking(elem +: prependable)
		case 0  =>
			this
		case n => rotatedRight(0, n + 1)(1)
	}

	override def appendedAll[U >: T](suffix :IterableOnce[U]) :Ranking[U] = suffix match {
		case view    :View[U]                      => appendedAll(view.iterator)
		case items   :Iterable[_] if items.isEmpty => this
		case ranking :Ranking[U]                   => appendUnique[U, Ranking[U]](ranking, _.contains(_))
		case set     :collection.Set[U] @unchecked => appendUnique[U, collection.Set[U]](set, _.contains(_))
		case it      :Iterator[U] if !it.hasNext   => this
		case _ =>
			val suffixReversed = Iterators.reverse(suffix)
			if (suffixReversed.isEmpty)
				this
			else {
				val res = new ReverseBuilder[U]
				res ++= suffixReversed ++= items.reverseIterator
				res.result()
			}
	}
	private def appendUnique[U >: T, C <: Iterable[U]](elems :C, contains :(C, U) => Boolean) :Ranking[U] = {
		val res = new RankingBuilder[U](IndexedSeqFactory.newBuilder[U], Map.empty[U, Int])
		items.foreach { e =>
			if (!contains(elems, e))
				res += e
		}
		(res ++= elems).result()
	}

	override def prependedAll[U >: T](prefix :IterableOnce[U]) :Ranking[U] = prefix match {
		case view    :View[U]                           => prependedAll(view.iterator)
		case empty   :Iterable[_] if empty.isEmpty      => this
		case single  :Iterable[U] if single.sizeIs == 1 => prepended(single.head)
		case empty   :Iterator[U] if !empty.hasNext     =>  this
		case ranking :Ranking[U]                        => ranking concat this
		case _ =>
			(new RankingBuilder[U](IndexedSeqFactory.newBuilder, HashMap.empty) ++= prefix ++= this).result()
	}

	override def concat[U >: T](that :IterableOnce[U]) :Ranking[U] = that match {
		case view    :View[U]                           => concat(view.iterator)
		case empty   :Iterable[_] if empty.isEmpty      => this
		case empty   :Iterator[_] if !empty.hasNext     => this
		case single  :Iterable[U] if single.sizeIs == 1 => added(single.head)
		case _ =>
			(Ranking.newBuilder[U] ++= this ++= that).result()
	}


	override def removedOne[U >: T](elem :U) :Ranking[T] =
		indexOf(elem) match {
			case -1 => this
			case i if i == items.length - 1 =>
				new IndexedRanking[T](sliceable.init, HashMap.from(map) - elem.asInstanceOf[T])
			case i =>
				val view = items.view
				val tail = view.drop(i + 1)
				var newIndex = try {
					HashMap.from(map) - elem.asInstanceOf[T]
				} catch {
					case _ :ClassCastException => map
				}
				tail foreach { e => newIndex = newIndex.updated(e, map(e) - 1) }
				new IndexedRanking[T]((view.take(i) ++ tail).toIndexedSeq, newIndex)
		}

	override def removedAll[U >: T](elems :IterableOnce[U]) :Ranking[T] =
		elems match {
			case _ if isEmpty =>
				this
			case empty :Iterable[_] if empty.isEmpty =>
				this
			case _ =>
				val excludes = elems.iterator.toSet
				val it = items.iterator
				val b = newSpecificBuilder
				while (it.hasNext) {
					val e = it.next()
					if (!excludes(e))
						b += e
				}
				b.result()
	}

	override def reverse  :Ranking[T] = items.length match {
		case 0 => Ranking.empty
		case n if n <= SmallRankingCap =>
			val res = Ranking.newBuilder[T]; res.sizeHint(n)
			var i = n
			while (i > 0) {
				i -= 1; res += items(i)
			}
			res.result()
		case n if n <= IndexedSeqDefaults.defaultApplyPreferredMaxLength =>
			new IndexedRanking(items.reverse)
		case _ =>
			new ReversedRanking[T](this)
	}

	override def iterator :Iterator[T] = items.iterator
	override def reverseIterator :Iterator[T] = items.reverseIterator

	override def toVector :Vector[T] = items.toVector
	override def toIndexedSeq :IndexedSeq[T] = items
	override def toSeq :Seq[T] = items
	override def toSet[U >: T] :Set[U] = HashMap.from(map).keySet.castParam[U]
	//it could use a different name, as Seq.indices returns a Range
	private[collections] def index :Map[_ <: T, Int] = map

	override def copyToArray[A >: T](xs :Array[A], start :Int, len :Int) :Int =
		items.copyToArray(xs, start, len)

	override def copyRangeToArray[A >: T](xs :Array[A], start :Int, from :Int, len :Int) :Int =
		items.copyRangeToArray(xs, start, from, len)

	override def cyclicCopyToArray[A >: T](xs :Array[A], start :Int, len :Int) :Int =
		items.cyclicCopyToArray(xs, start, len)

	override def cyclicCopyRangeToArray[A >: T](xs :Array[A], start :Int, from :Int, len :Int) :Int =
		items.cyclicCopyRangeToArray(xs, start, from, len)

	override def applyPreferred :Boolean = IndexedIterable.applyPreferred(items)
	private[this] def writeReplace = new RankingSerializer(this)
}




@SerialVersionUID(Ver)
private class ReversedRanking[T](override val reverse :Ranking[T])
	extends AbstractRanking[T] with StrictOptimizedIterableOps[T, Ranking, Ranking[T]]
{
	@inline private def sizeLog = java.lang.Integer.highestOneBit(reverse.size)
	override def knownSize       = reverse.knownSize
	override def iterator        = reverse.reverseIterator
	override def reverseIterator = reverse.iterator

	@inline private[this] def newInstance[U >: T](reverse :Ranking[U]) =
		if (reverse eq this.reverse) this
		else if (reverse.isEmpty) Ranking.empty
		else new ReversedRanking(reverse)

	override def indexOf[U >: T](elem :U) = reverse.indexOf(elem) match {
		case -1 => -1
		case n  => reverse.size - n - 1
	}
	override def indexWhere(p :T => Boolean, from :Int) :Int = {
		val size = reverse.knownSize
		if (from >= size)
			-1
		else {
			val res = reverse.lastIndexWhere(p, size - math.max(from, 0) - 1)
			if (res < 0) -1 else size - res - 1
		}
	}
	override def lastIndexWhere(p :T => Boolean, end :Int) :Int = {
		val size = reverse.knownSize
		if (end < 0)
			-1
		else {
			val res = reverse.indexWhere(p, size - math.min(end, size - 1) - 1)
			if (res < 0) -1 else size - res - 1
		}
	}

	override def apply(index :Int) =
		if (index < 0)
			throw new IndexOutOfBoundsException(index.toString + " out of " + reverse.size)
		else
			reverse(reverse.size - index - 1)

	override def updated[U >: T](index :Int, elem :U) =
		if (index < 0)
			throw new IndexOutOfBoundsException(index.toString + " out of " + reverse.size)
		else
			new ReversedRanking(reverse.updated(reverse.size - index - 1, elem))

	override def inserted[U >: T](index :Int, elem :U) :Ranking[U] =
		if (index <= 0) prepended(elem)
		else if (index >= reverse.knownSize) appended(elem)
		else newInstance(reverse.inserted(reverse.knownSize - index - 1, elem))

	override def replace[U >: T](index :Int, elem :U) :Ranking[U] =
		if (index < 0 | index > reverse.knownSize)
			throw new IndexOutOfBoundsException(index.toString + " out of " + reverse.size)
		else
			newInstance(reverse.replace(reverse.size - index - 1, elem))

	override def incl[U >: T](elem :U) :Ranking[U] = newInstance(reverse +| elem)
	override def prepended[U >: T](elem :U) = newInstance(reverse :+ elem)
	override def appended[U >: T](elem :U) = newInstance(elem +: reverse)

	override def concat[U >: T](elems :IterableOnce[U]) = elems match {
		case _  :View[U]                             => concat(elems.iterator)
		case it :Iterable[U] if it.isEmpty           => this
		case it :Iterator[U] if !it.hasNext          => this
		case it :Iterable[U] if it.sizeIs <= sizeLog => newInstance(Iterators.reverse(elems) ++: reverse)
		case it :Iterator[U] if { val n = it.knownSize; n >= 0 && n <= sizeLog } => newInstance(reverse ++ elems)
		case _ => (Ranking.newBuilder[U] ++= this ++= elems).result()
	}
	override def appendedAll[U >: T](suffix :IterableOnce[U]) = suffix match {
		case it :Iterable[U] if it.sizeIs <= sizeLog => newInstance(util.reverse(suffix) ++: reverse)
		case _  :View[U]                             => appendedAll(suffix.iterator)
		case it :Iterator[U] if !it.hasNext          => this
		case _ =>
			(Ranking.appendingBuilder[U] ++= this ++= suffix).result()
	}
	override def prependedAll[U >: T](prefix :IterableOnce[U]) = prefix match {
		case small :Iterable[U] if small.sizeIs <= sizeLog => newInstance(reverse :++ util.reverse(prefix))
		case _     :View[U]                                => prependedAll(prefix.iterator)
		case empty :Iterator[_] if !empty.hasNext          => this
		case _ =>
			(Ranking.newBuilder[U] ++= prefix ++= this).result()
	}

	override def removedOne[U >: T](elem :U) = (reverse - elem) match {
		case same  if same eq reverse => this
		case other if other.isEmpty   => Ranking.empty
		case other                    => new ReversedRanking(other)
	}
	override def removedAll[U >: T](elems :IterableOnce[U]) = elems match {
		case it :Iterable[U] if it.isEmpty           => this
		case it :Iterator[U] if !it.hasNext          => this
		case it :Iterable[U] if it.sizeIs <= sizeLog => newInstance(reverse -- elems)
		case it :Iterator[U] if { val n = it.knownSize; n >= 0 && n <= sizeLog } => newInstance(reverse -- it)
		case deleted :Set[U] =>
			(Ranking.newBuilder[T] /: this) {
				(builder, item) => if (deleted(item)) builder else builder += item
			}.result()
		case _ =>
			val deleted = elems.iterator.collect { case item if reverse.indexOf(item) >= 0 => item } to Set
			(Ranking.newBuilder[T] /: this) {
				(builder, item) => if (deleted(item)) builder else builder += item
			}.result()
	}

	private def writeReplace = new RankingSerializer(this)
}






private class RankingAsSeq[E](underlying :Ranking[E])
	extends IndexedSeqOps[E, Ranking, Ranking[E]] with IterableFactoryDefaults[E, Ranking]
{
	override def length :Int = underlying.length
	override def apply(i :Int) :E = underlying(i)
	override def updated[B >: E](index :Int, elem :B) :Ranking[B] = underlying.updated(index, elem)

	override def indexOf[B >: E](elem :B, from :Int) :Int = underlying.indexOf(elem) match {
		case n if n >= from => n
		case _              => -1
	}
	override def lastIndexOf[B >: E](elem :B, end :Int) :Int = underlying.indexOf(elem) match {
		case n if n <= end => n
		case _             => -1
	}

	override def iterableFactory :IterableFactory[Ranking] = Ranking

	override def coll :Ranking[E] = underlying
	override def iterator :Iterator[E] = underlying.iterator
	override def reverseIterator :Iterator[E] = underlying.reverseIterator
	override def view :IndexedSeqView[E] = underlying.view

	@nowarn("cat=deprecation")
	override def toIterable    :Iterable[E] = underlying
	override def toSet[U >: E] :Set[U] = underlying.toSet
	override def toSeq         :Seq[E] = underlying.toSeq
	override def toIndexedSeq  :IndexedSeq[E] = underlying.toIndexedSeq
}

