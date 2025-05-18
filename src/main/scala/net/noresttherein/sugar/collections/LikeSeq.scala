package net.noresttherein.sugar.collections

import java.util.Spliterator

import scala.annotation.tailrec
import scala.collection.{SeqView, Stepper, StepperShape, View, mutable}
import scala.collection.Searching.{Found, InsertionPoint, SearchResult}
import scala.collection.mutable.{Buffer, Builder}

import net.noresttherein.sugar.JavaTypes.{JDouble, JInt, JLong}
import net.noresttherein.sugar.arrays.ArrayIterator
import net.noresttherein.sugar.collections.IndexedIterable.ApplyPreferred
import net.noresttherein.sugar.collections.LikeIterable.LikeIterableBasics
import net.noresttherein.sugar.collections.LikeIterableOnce.Generic.Template
import net.noresttherein.sugar.collections.extensions.SeqExtension
import net.noresttherein.sugar.collections.util.{elementsToCopy, errorString}
import net.noresttherein.sugar.exceptions.{noSuch_!, outOfBounds_!, unsupported_!}
import net.noresttherein.sugar.extensions.{IteratorCompanionExtension, IteratorExtension, PartialFunctionExtension, boxeqMethod}
import net.noresttherein.sugar.typist.<::<
import net.noresttherein.sugar.typist.kinds.Any1
import net.noresttherein.sugar.util.SerializableSingleton
import net.noresttherein.sugar.vars.{Maybe, Opt}
import net.noresttherein.sugar.vars.Maybe.{No, Yes}
import net.noresttherein.sugar.vars.Opt.One




/**
  * @define Coll `Seq`
  * @define coll sequence
  */ //consider: replacing it with something like OrderedLike or PositionedLike or RankedLike
trait LikeSeq[+X, -Xs, +CC[_], +C] extends LikeIterable[X, Xs, CC, C] {
	/** Tells anyone interested that iteration over the sequence is at least comparable with `apply` to `iterator`,
	  * and may be preferred, especially in case of indexing over a slice in the middle of a sequence
	  * (which requires `iterator.drop` to be fast). Implies `isApplyFast`, meaning `reverseIterator`
	  * and `size` are relatively fast, and `knownSize >= 0`.
	  */
	def isApplyPreferred(elems :Xs) :Boolean = false

	/** Tells anyone interested that random indexing with `apply` is at least `O(log n)` or better, good enough
	  * to use repeatedly. It means, in particular, that `reverseIterator` doesn't require reversing
	  * the whole collection to return its first element. Implies also `size` is no worse than `O(log n)`,
	  * too (although `knownSize` may be `-1`).
	  */
	def isApplyFast(elems :Xs) :Boolean = false

	/** Get the element at the specified index. This operation is provided for convenience in `Seq`. It should
	  * not be assumed to be efficient unless you have an `IndexedSeq`.
	  * @param elems a $coll.
	  */
	def apply(elems :Xs, i :Int) :X

	/** Finds the last element of the $coll satisfying a predicate, f any.
	  *
	  * $willNotTerminateInf
	  * @param elems a $coll.
	  * @param p     the predicate used t test elements.
	  * @return an option value containing the last element in `elems` that satisfies `p`, or `None` if none exists.
	  */
	def findLast(elems :Xs)(p :X => Boolean) :Option[X]

	/** Computes the length of the longest segment that sta s from some index
	  * and whose elements all satisfy s e predicate.
	  *
	  * $mayNotTerminateInf
	  * @param elems a $coll.
	  * @param p     the predicate use to test the elements.
	  * @param from  the index where he search starts.
	  * @return the length of the longest segment of `elems` starting at index `from`
	  *         such that every element of the segment satisfies the predicate `p`.
	  */
	def segmentLength(elems :Xs, from :Int)(p :X => Boolean) :Int
	@inline final def segmentLength(elems :Xs)(p :X => Boolean) :Int = segmentLength(elems, 0)(p)

	/** Finds index of the first element satisfying some predicate after o at some start index.
	  *
	  * $mayNotTerminateInf
	  * @param elems a $coll.
	  * @param p     the predicate to use too test elements.
	  * @param from  the start index.
	  * @return the index `>= from` of the first element of `elems` that satisfies the predicate `p`,
	  *         or `-1`, if none exists.
	  */
	def indexWhere(elems :Xs, from :Int)(p :X => Boolean) :Int
	@inline final def indexWhere(elems :Xs)(p :X => Boolean) :Int = indexWhere(elems, 0)(p)

	/** Finds index of first occurrence of some value in `elems` after or t some art  ex.
	  * @tparam U    the t e of t elem  `elem`.
	  * @param elems a $coll.
	  * @param elem  the el ent val   rch for.
	  * @param from  the start index.
	  * @return the index `>= from` of the first element of `elems` that is equal
	  *         (as determined by `==`) to `elem`, or `-1`, if none exists.
	  */
	def indexOf[U >: X](elems :Xs, from :Int, elem: U): Int
	@inline final def indexOf[U >: X](elems :Xs, elem :U) :Int = indexOf(elems, 0, elem)

	/** Finds index of last element satisfying some predicate before or at iven end index.
	  *
	  * $willNotTerminateInf
	  *
	  * @param elems a $coll.
	  * @param p     the predicate used to test elements.
	  * @return  the index `<= end` of the last element of `elems` that satisfies the predicate `p`,
	  *          or `-1`, if none exists.
	  */
	def lastIndexWhere(elems :Xs, end :Int)(p :X => Boolean): Int
	@inline final def lastIndexWhere(elems :Xs)(p :X => Boolean) :Int = lastIndexWhere(elems, Int.MaxValue)(p)

	/** Finds index of last occurrence of some value in `elems` before or at  given end index.
	  *
	  * $WillNotTerminateInf
	  * @tparam U    the type of the element `elem`.
	  * @param elems a $coll.
	  * @param elem  the element value to search for.
	  * @param end   the end index.
	  * @return the index `<= end` of the last element of `elems` that is equal
	  *         (as determined by `==`) to `elem`, or `-1`, if none exists.
	  */
	def lastIndexOf[U >: X](elems :Xs, end :Int, elem :U): Int

	@inline final def lastIndexOf[U >: X](elems :Xs, elem :U) :Int = lastIndexOf(elems, Int.MaxValue, elem)

	def indexOfSlice[U >: X, O](elems :Xs, from :Int, that :O)(implicit likeSeq :LikeSeq[U, O, Any1, _]) :Int

	@inline final def indexOfSlice[U >: X, O](elems :Xs, that :O)(implicit likeSeq :LikeSeq[U, O, Any1, _]) :Int =
		indexOfSlice[U, O](elems, 0, that)

	/** Finds first index after or at a start index where `elems` contains a gi n sequence as a slice.
	  *
	  * $mayNotTerminateInf
	  * @param elems a $coll.
	  * @param that  the sequence to test.
	  * @param from  the start index.
	  * @return  the first index `>= from` such that the elements of `elems` starting at this index
	  *          match the elements of sequence `that`, or `-1` if no such subsequence exists.
	  */
	def indexOfSlice[U >: X](elems :Xs, from :Int, that :collection.Seq[U]) :Int
	@inline final def indexOfSlice[U >: X](elems :Xs, that :collection.Seq[U]) :Int = indexOfSlice(elems, 0, that)

	def lastIndexOfSlice[U >: X, O](elems :Xs, end :Int, that :O)(implicit likeSeq :LikeSeq[U, O, Any1, _]) :Int

	@inline final def lastIndexOfSlice[U >: X, O](elems :Xs, that :O)(implicit likeSeq :LikeSeq[U, O, Any1, _]) :Int =
		lastIndexOfSlice[U, O](elems, Int.MaxValue, that)

	/** Finds last index before or at a given end index where `elems` contains a given sequence as  slice.
	  *
	  * $willNotTerm ateInf
	  *
	  * @param elems a $coll.
	  * @param that  the sequence to test
	  * @param end   the end index.
	  * @return  the last index `<= end` such that the elements of `elems` starting at this index
	  *          match the elements of sequence `that`, or `-1` if no such subsequence exists.
	  */
	def lastIndexOfSlice[U >: X](elems :Xs, end :Int, that :collection.Seq[U]) :Int

	@inline final def lastIndexOfSlice[U >: X](elems :Xs, that :collection.Seq[U]) :Int =
		lastIndexOfSlice(elems, Int.MaxValue, that)

	/** Tests whether `elems` contains a given value as an element.
	  *
	  * $mayNotTerminateInf
	  * @param elems a $coll.
	  * @param elem  the el ent to test.
	  * @return `true` if `elems` has an element that is equal (as determined by `==`) to `elem`, `false` otherwise.
	  */
	@inline def contains[A >: X](elems :Xs, elem :A) :Boolean = indexOf[A](elems, elem) >= 0

	/** Tests whether `elems` contains a given sequence as a slice.
	  * $mayNotTerminateInf
	  * @param elems a $coll.
	  * @param that  the sequence to test
	  * @return `true` if `elems` contains a slice with the same elements as `that`, otherwise `false`.
	  */
	@inline final def containsSlice[U >: X](elems :Xs, that :collection.Seq[U]) :Boolean =
		indexOfSlice[U](elems, that) != -1

	@inline final def containsSlice[U >: X, O](elems :Xs, that :O)(implicit likeSeq :LikeSeq[U, O, Any1, _]) :Boolean =
		indexOfSlice[U, O](elems, that) != -1

	/** Tests whether `elems` contains the given sequence at a given index.
	  *
	  * '''Note''': If the both the receiver object `this` and the argument
	  * `that` are infinite sequences this method may not terminate.
	  *
	  * @param elems   a $coll.
	  * @param that    the sequence to test
	  * @param offset  the index where the sequence is searched.
	  * @return `true` if the sequence `that` is contained in `elems` at
	  *         index `offset`, otherwise `false`.
	  */
	def startsWith[U >: X, O](elems :Xs, offset :Int, that :O)
	                         (implicit likeCollection :LikeCollection[U, O]) :Boolean

	@inline final def startsWith[U >: X, O](elems :Xs, that :O)(implicit likeCollection :LikeCollection[U, O]) :Boolean =
		startsWith[U, O](elems, 0, that)

	/** Tests whether `elems` ends with the given sequence.
	  *
	  * $willNotTerminateInf
	  * @param elems a $coll.
	  * @param that  the sequence to test
	  * @return `true` if `elems` has `that` as a suffix, `false` otherwise.
	  */
	def endsWith[U >: X, O](elems :Xs, that :O)(implicit likeIterable :LikeIterable[U, O, Any1, _]) :Boolean

	/** Are the elements of this collection the same (and in the same order) as those of `that`?
	  * @param elems a $coll.
	  */
	def sameElements[U >: X, O](elems :Xs, that :O)(implicit likeCollection :LikeCollection[U, O]) :Boolean

	/** Search within an interval in this sorted sequence for a specific element. If this
	  * sequence is an `IndexedSeq`, a binary search is used. Otherwise, a linear search
	  * is used.
	  *
	  * The sequence should be sorted with the same `Ordering` before calling; otherwise,
	  * the results are undefined.
	  *
	  * @see [[scala.collection.IndexedSeq]]
	  * @see [[scala.math.Ordering]]
	  * @see [[scala.collection.SeqOps]], method `sorted`
	  * @param elems a $coll.
	  * @param elem  the element to find.
	  * @param from  the index where the search starts.
	  * @param until    the index following where the search ends.
	  * @param ord   the ordering to be used to compare elements.
	  *
	  * @return a `Found` value containing the index corresponding to the element in the sequence,
	  *         or the `InsertionPoint` where the element would be inserted if the element is not in the sequence.
	  * @note if  `to <= from`, the search space is empty, and an `InsertionPoint` at `from` is returned.
	  */
	def search[U >: X](elems :Xs, elem :U, from :Int = 0, until :Int = Int.MaxValue)
	                  (implicit ord :Ordering[U]) :SearchResult


	/** Selects all the elements of `elems` ignoring the duplicates.
	  * @param elems a $coll.
	  * @return a new $coll consisting of all the elements of `elems` without duplicates.
	  */
	def distinct(elems :Xs) :C

	/** Selects all the elements of `elems` ignoring the duplicates as determined by `==` after applying
	  * the transforming function `f`.
	  * @tparam A    the type of the elements after being transformed by `f`
	  * @param elems a $coll.
	  * @param f     The transforming function whose result is used to determine the uniqueness of each element
	  * @return a new $coll consisting of all the elements of `elems` without duplicates.
	  */
	def distinctBy[A](elems :Xs)(f :X => A) :C

	/** Returns new $coll with elements in reversed o er.
	  *
	  * $willNotTerminateInf
	  * $willForceEvaluation
	  * @return A new $coll with all elements of `elems` in reversed order.
	  */
	def reverse(elems :Xs) :C

	/** Sorts `elems` according to an Ordering. The sort is stable. That is, elements that are equal
	  * (as determined by `ord.compare`) appear in the same order in the sorted sequence as in the original.
	  *
	  * @see [[scala.math.Ordering]]
	  *
	  * $willForceEvaluation
	  * @param elems a $coll.
	  * @param ord   the ordering to be used to compare elements.
	  * @return      a $coll consisting of the elements of `elems` sorted according to the ordering `ord`.
	  */
	def sorted[U >: X](elems :Xs)(implicit ord :Ordering[U]) :C

	/** Sorts `elems` according to a comparison function.
	  * $willNotTerminateInf
	  * $willForceEvaluation
	  *
	  * The sort is stable. That is, elements tha are equal (`lt` returns false for both directions of comparison)
	  * appear in the same order in the sorted sequence as in the original.
	  * @param elems a $coll.
	  * @param lt    a predicate that is true if
	  *              its first argument strictly precedes its second argument in
	  *              the desired ordering.
	  * @return      a $coll consisting of the elements of `elems` sorted according to the comparison function `lt`.
	  * @example {{{
	  *    List("Steve", "Bobby", "Tom", "John", "Bob").sortWith((x, y) => x.take(3).compareTo(y.take(3)) < 0) =
	  *    List("Bobby", "Bob", "John", "Steve  "Tom")
	  *  }}}
	  */
	def sortWith(elems :Xs)(lt :(X, X) => Boolean) :C

	/** Sorts `elems` according to the Ordering which results from transforming
	  * an implicitly given Ordering with a transformation function.
	  * $willNotTerminateInf
	  * $willForceEvaluation
	  *
	  * The sort is stable. That is, elements that are equal (as determined by
	  * `ord.compare`) appear in the same order in the sorted sequence as in the original.
	  *
	  * @see [[scala.math.Ordering]]
	  * @tparam A    the target type of the transformation `f`, and the type where the ordering `ord` is defined.
	  * @param elems a $coll.
	  * @param f     the transformation function mapping elements to some other domain `A`.
	  * @param ord   the ordering assumed on domain `A`.
	  * @return  a $coll consisting of the elements of `elems` sorted according to the ordering
	  *          where `x < y` if `ord.lt(f(x), f(y))` .
	  * @example {{{
	  *    val words = "The quick brown fox jumped over the lazy dog".split(' ')
	  *    // this works because scala.Ordering will implicitly provide an Ordering[Tuple2[Int, Char]]
	  *    words.sortBy(x => (x.length, x.head))
	  *    res0: Array[String] = Array(The, dog, fox, the, lazy, over, brown, quick, jumped)
	  *  }}}
	  */
	def sortBy[A](elems :Xs)(f :X => A)(implicit ord :Ordering[A]) :C


	/** A cop of `elems` with one single rep ced element.
	  * @tparam U    the element type of the returned $coll.
	  * @param elems a $coll.
	  * @param index the position of the replace nt
	  * @param elem  the replacing element
	  * @return a new $coll which is a copy of `elems` with the element at position `index` replaced by `elem`.
	  * @throws IndexOutOfBoundsException if `index` does not satisfy `0 <= index < length`.
	  *                                   In case of a lazy collection this exception may be thrown at a later time
	  *                                   or not at all (if the end of the collection is never evaluated).
	  */
	def updated[U >: X](elems :Xs, index: Int, elem: U): CC[U]

	/** A copy of `elems` with an element prepended.
	  * Also, the original $coll is not modified, so you will want to capture the result.
	  * Example:
	  * {{{
	  *      scala> val x = List(1)
	  *      x: List[Int] = List(1)
	  *
	  *      scala> val y = 2 +: x
	  *      y: List[Int] = List(2, 1)
	  *
	  *      scala> println(x)
	  *      List(1)
	  * }}}
	  * @tparam U    the element type of the returned $coll.
	  * @param elems a $coll.
	  * @param elem  the prepended element
	  * @return a new $coll consisting of `value` followed by all elements of `elems`.
	  */
	def prepended[U >: X](elems :Xs, elem :U) :CC[U]

	/** A copy of `elems` with an element appended.
	  * $willNotTerminateInf
	  * Example:
	  * {{{
	  *    scala> val a = List(1)
	  *    a: List[Int] = List(1)
	  *
	  *    scala> val b = a :+ 2
	  *    b: List[Int] = List(1, 2)
	  *
	  *    scala> println(a)
	  *    List(1)
	  * }}}
	  * @tparam U    the element type of the returned $coll.
	  * @param elems a $coll.
	  * @param elem  the appended element
	  * @return a new $coll consisting of all elements of `elems` followed by `value`.
	  */
	def appended[U >: X](elems :Xs, elem :U) :CC[U]

	def updatedAll[U >: X, O](elems :Xs, index :Int, patch :O)(implicit likeCollection :LikeCollection[U, O]) :CC[U]

	def overwritten[U >: X, O](elems :Xs, index :Int, patch :O)(implicit likeCollection :LikeCollection[U, O]) :CC[U]

	/** As with `appendedAll`, returns a new collection containing the elements from the left operand
	  * followed by the elements from the right operand.
	  *
	  * @tparam U     the element type of the returned collection.
	  * @param elems  a $coll.
	  * @param prefix the collection to prepend.
	  * @return       a new $coll which contains all elements of `prefix` followed by all the elements of `elems`.
	  */
	def prependedAll[U >: X, O](elems :Xs, prefix :O)(implicit likeCollection :LikeCollection[U, O]) :CC[U]

	/** Returns a new $coll containing the elements of `self` followed by the elements of `elems`.
	  * @tparam U     the element type of the returned collection.
	  * @param elems  a $coll.
	  * @param suffix the collection to append.
	  * @return       a new collection of type `CC[B]` which contains all elements
	  *               of `elems` followed by all elements of `suffix`.
	  */
	@inline final def appendedAll[U >: X, O](elems :Xs, suffix :O)(implicit likeCollection :LikeCollection[U, O]) :CC[U] =
		concat[U, O](elems, suffix)

	//todo: inserted/insertedAll
//	override def concat[U >: X, O](elems :Xs, suffix :O)(implicit likeCollection :LikeCollection[U, O]) :CC[U]

	/** Produces a new $coll where a slice of elements in `elems` is replaced by another sequence.
	  * Patching at negative indices is the same as patching starting at 0.
	  * Patching at indices at or larger than the length of the original $coll appends the patch to the end.
	  * If more values are replaced than actually exist, the excess is ignored.
	  * @tparam U       the element type of the returned $coll.
	  * @param elems    a $coll.
	  * @param from     the index of the first replaced element.
	  * @param other    the replacement sequence.
	  * @param replaced the number of elements to drop in the original $coll.
	  * @return a new $coll consisting of all elements of `elems` except that `replaced` elements,
	  *         starting from `from`, are replaced by all the elements of `other`.
	  */
	def patch[U >: X, O](elems :Xs, from :Int, other :O, replaced :Int)
	                    (implicit likeCollection :LikeCollection[U, O]) :CC[U]

	/** A copy of `elems` with an element value appended until a given target length is reached.
	  * @tparam U    the element type of the returned $coll.
	  * @param elems a $coll.
	  * @param len   the target length
	  * @param elem  the padding value
	  * @return a new $coll consisting of all elements of `elems` followed by the minimal number of occurrences
	  *         of `elem`, so that the resulting collection has a length of at least `len`.
	  */
	def padTo[U >: X](elems :Xs, len :Int, elem :U) :CC[U]


	/** An iterator yielding elements in reversed order.
	  *
	  * $willNotTerminateInf
	  *
	  * Note: `xs.reverseIterator` is the same as `xs.reverse.iterator` but might be more efficient.
	  * @param elems a $coll.
	  * @return an iterator yielding the elements of `elems` in reversed order
	  */
	def reverseIterator(elems :Xs) :Iterator[X]

	def toImpureSeq(elems :Xs) :collection.Seq[X]

//	override def view(elems :Xs) :SeqView[X]

//	override def toOps(elems :Xs) :collection.SeqOps[X, CC, C]

	override def moreSpecific(elems :Xs) :Maybe[LikeSeq[X, elems.type, CC, C]] = No
	override def specific(elems :Xs) :LikeSeq[X, elems.type, CC, C] = moreSpecific(elems) getOrElse this
}




/** @define TypeClass `LikeSeq` */
private[collections] sealed abstract class Rank3LikeSeqs extends LikeIterableOnceSummons[LikeSeq] {
	@inline implicit final def likeGeneric[X, Xs <: CC[X], CC[_], C >: CC[X]]
	                                      (implicit generic :Generic[CC]) :LikeSeq[X, Xs, CC, C] =
		generic.of
}


private[collections] sealed abstract class Rank2LikeSeqs extends Rank3LikeSeqs {
	//Type parameter Xs is needed for this method to not be 'more specific' than likeIndexedSeq, etc..
	implicit final def forOps[X, Xs, CC[A], C]
	                         (implicit arg :Xs <:< C, specific :C <:< CC[X] with collection.SeqOps[X, CC, C],
	                          generic :CC <::< Iterable) :LikeSeq[X, Xs, CC, C] =
		prototype.asInstanceOf[LikeSeq[X, Xs, CC, C]]

	private object priv {
		//Circumvent lack of recursive type aliases. We don't want to use Seq/IndexedSeq in order to avoid accidentally
		// statically referencing it in the anonymous class, which would throw a ClassCastException
		// in forOps which claims to work for any Iterable with SeqOps.
		type Ops[X] >: collection.Iterable[X] with collection.SeqOps[X, Ops, Ops[X]]
			<: collection.Iterable[X] with collection.SeqOps[X, Ops, Ops[X]]
		type IndexedOps[X] >: collection.Iterable[X] with collection.IndexedSeqOps[X, IndexedOps, IndexedOps[X]]
			<: collection.Iterable[X] with collection.IndexedSeqOps[X, IndexedOps, IndexedOps[X]]
	}
	import priv.{Ops, IndexedOps}
	private[this] val prototype = new LikeSeq.ForOps[Any, Ops, Ops[Any]] {
		override def moreSpecific(elems :Ops[Any]) = elems match {
			case indexed :collection.IndexedSeqOps[Any, IndexedOps, IndexedOps[Any]] @unchecked =>
				Yes(
					LikeIndexedSeq.forOps[Any, IndexedOps[Any], IndexedOps, IndexedOps[Any]]
				        .specific(indexed).asInstanceOf[LikeSeq[Any, elems.type, Ops, Ops[Any]]
				])
			case list :collection.LinearSeq[Any] =>
				Yes(
					LikeSeq.likeLinearSeq[Any, collection.LinearSeq, collection.LinearSeq[Any]]
					       .specific(list).asInstanceOf[LikeSeq[Any, elems.type, Ops, Ops[Any]]]
				)
			case _ => No
		}
		override def toString :String = "LikeSeq.forOps"
		private def readResolve :AnyRef = LikeSeq.forOps[Any, collection.Seq[Any], collection.Seq, collection.Seq[Any]]
	}
}


private[collections] sealed abstract class Rank1LikeSeqs extends Rank2LikeSeqs {
	@inline implicit final def likeMutableSeq[X, Xs, CC[_], C](implicit like :LikeMutableSeq[X, Xs, CC, C])
			:LikeSeq[X, Xs, CC, C] =
		like

	@inline implicit final def likeIndexedSeq[X, Xs, CC[_], C](implicit like :LikeIndexedSeq[X, Xs, CC, C])
			:LikeSeq[X, Xs, CC, C] =
		like
}


@SerialVersionUID(Ver)
object LikeSeq extends Rank1LikeSeqs {

	@inline implicit def likeMutableIndexedSeq[X, Xs, CC[_], C](implicit like :LikeMutableIndexedSeq[X, Xs, CC, C])
			:LikeSeq[X, Xs, CC, C] =
		like

	implicit def likeLinearSeq[X, CC[A] <: collection.LinearSeq[A],
	                           C <: collection.LinearSeq[X] with collection.LinearSeqOps[X, CC, C]]
	                          (implicit specific :C <:< CC[X] with collection.LinearSeqOps[X, CC, C],
	                                    generic :CC <::< collection.LinearSeq) :LikeSeq[X, C, CC, C] =
		forLinearSeq.asInstanceOf[LikeSeq[X, C, CC, C]]

	private[this] val forLinearSeq =
		new LikeCollection.ForIterableOnce[Any, collection.LinearSeq[Any]]
			with FromTail[Any, collection.LinearSeq, collection.LinearSeq[Any]]
			with ForOps[Any, collection.LinearSeq, collection.LinearSeq[Any]]
		{
			private def readResolve :AnyRef = LikeSeq.likeLinearSeq[Any, collection.LinearSeq, collection.LinearSeq[Any]]
			override def toString = "LikeSeq.forLinearSeq"
		}

	def adapt[X, Xs](elems :Xs)(implicit likeSeq :LikeSeq[X, Xs, Any1, Any]) :collection.Seq[X] =
		new LikeCollectionAdapter[X, elems.type](elems)
			with LikeIterableAdapter[X, elems.type, collection.Seq, collection.Seq[X]] with collection.Seq[X]
			with LikeSeqAdapter[X, elems.type, collection.Seq, collection.Seq[X]]
		{
			override val ops = likeSeq.specific(elems)
		}


	trait Generic[CC[_]] extends LikeIterable.Generic[CC] with Template[CC, LikeSeq]

	@SerialVersionUID(Ver)
	object Generic extends Rank1Generics {
		@inline implicit def likeMutableIndexedSeq[CC[_]](implicit generic :LikeMutableIndexedSeq.Generic[CC])
				:Generic[CC] =
			generic

		implicit def forLinearSeq[CC[X] <: collection.LinearSeq[X]] :Generic[CC] =
			linearPrototype.asInstanceOf[Generic[CC]]

		import scala.collection.immutable.LinearSeq

		@SerialVersionUID(Ver)
		private object linearPrototype extends Generic[LinearSeq] {
			implicit override def of[X] :LikeSeq[X, LinearSeq[X], LinearSeq, LinearSeq[X]] = LikeSeq.likeLinearSeq
			override def toString = "LikeSeq.Generic.forLinearSeq"
		}
	}

	private[LikeSeq] sealed abstract class Rank1Generics extends Rank2Generics {
		@inline implicit final def likeIndexedSeq[CC[_]](implicit generic :LikeIndexedSeq.Generic[CC]) :Generic[CC] =
			generic

		@inline implicit final def likeMutableSeq[CC[_]](implicit generic :LikeMutableSeq.Generic[CC]) :Generic[CC] =
			generic
	}
	private[LikeSeq] sealed abstract class Rank2Generics {
		implicit def forOps[CC[X] <: Iterable[X] with collection.SeqOps[X, CC, CC[X]]] :Generic[CC] =
			prototype.asInstanceOf[Generic[CC]]

		private[this] val prototype =
			new SerializableSingleton("LikeSeq.Generic.forOps", Generic.forOps[Seq]) with Generic[Seq] {
				implicit override def of[X] :LikeSeq[X, Seq[X], Seq, Seq[X]] = LikeSeq.forOps
			}
	}


	trait LikeMoreSpecific[+X, -Xs, +CC[_], +C]
		extends LikeSeq[X, Xs, CC, C] with LikeIterable.LikeMoreSpecific[X, Xs, CC, C]
	{
		//A short method which is most likely already more efficient than attempting to lookup a better one
//		abstract override def apply(elems :Xs, i :Int) :X = moreSpecific(elems) match {
//			case Yes(specific) => specific.apply(elems, i)
//			case No            => super.apply(elems, i)
//		}
		abstract override def findLast(elems :Xs)(p :X => Boolean) :Option[X] = moreSpecific(elems) match {
			case Yes(specific) => specific.findLast(elems)(p)
			case No            => super.findLast(elems)(p)
		}
		abstract override def segmentLength(elems :Xs, from :Int)(p :X => Boolean) :Int = moreSpecific(elems) match {
			case Yes(specific) => specific.segmentLength(elems, from)(p)
			case No            => super.segmentLength(elems, from)(p)
		}
		abstract override def indexWhere(elems :Xs, from :Int)(p :X => Boolean) :Int = moreSpecific(elems) match {
			case Yes(specific) => specific.indexWhere(elems, from)(p)
			case No            => super.indexWhere(elems, from)(p)
		}
		abstract override def indexOf[U >: X](elems :Xs, from :Int, elem :U) :Int = moreSpecific(elems) match {
			case Yes(specific) => specific.indexOf(elems, from, elem)
			case No            => super.indexOf(elems, from, elem)
		}
		abstract override def lastIndexWhere(elems :Xs, end :Int)(p :X => Boolean) :Int = moreSpecific(elems) match {
			case Yes(specific) => specific.lastIndexWhere(elems, end)(p)
			case No            => super.lastIndexWhere(elems, end)(p)
		}
		abstract override def lastIndexOf[U >: X](elems :Xs, end :Int, elem :U) :Int = moreSpecific(elems) match {
			case Yes(specific) => specific.lastIndexOf(elems, end, elem)
			case No            => super.lastIndexOf(elems, end, elem)
		}
		abstract override def indexOfSlice[U >: X, O](elems :Xs, from :Int, that :O)
		                                             (implicit likeSeq :LikeSeq[U, O, Any1, _]) :Int =
			moreSpecific(elems) match {
				case Yes(specific) => specific.indexOfSlice[U, O](elems :elems.type, from, that)
				case No            => super.indexOfSlice[U, O](elems, from, that)
			}
		abstract override def indexOfSlice[U >: X](elems :Xs, from :Int, that :collection.Seq[U]) :Int =
			moreSpecific(elems) match {
				case Yes(specific) => specific.indexOfSlice(elems :elems.type, from, that)
				case No            => super.indexOfSlice(elems, from, that)
			}
		abstract override def lastIndexOfSlice[U >: X, O](elems :Xs, end :Int, that :O)
		                                                 (implicit likeSeq :LikeSeq[U, O, Any1, _]) :Int =
			moreSpecific(elems) match {
				case Yes(specific) => specific.lastIndexOfSlice[U, O](elems :elems.type, end, that)
				case No            => super.lastIndexOfSlice[U, O](elems, end, that)
			}
		abstract override def lastIndexOfSlice[U >: X](elems :Xs, end :Int, that :collection.Seq[U]) :Int =
			moreSpecific(elems) match {
				case Yes(specific) => specific.lastIndexOfSlice(elems :elems.type, end, that)
				case No            => super.lastIndexOfSlice(elems, end, that)
			}
		abstract override def startsWith[U >: X, O](elems :Xs, offset :Int, that :O)
		                                           (implicit likeCollection :LikeCollection[U, O]) :Boolean =
			moreSpecific(elems) match {
				case Yes(specific) => specific.startsWith[U, O](elems, offset, that)
				case No            => super.startsWith[U, O](elems, offset, that)
			}
		abstract override def endsWith[U >: X, O](elems :Xs, that :O)
		                                         (implicit likeIterable :LikeIterable[U, O, Any1, _]) :Boolean =
			moreSpecific(elems) match {
				case Yes(specific) => specific.endsWith[U, O](elems, that)
				case No            => super.endsWith[U, O](elems, that)
			}
		abstract override def sameElements[U >: X, O](elems :Xs, that :O)
		                                             (implicit likeCollection :LikeCollection[U, O]) :Boolean =
			moreSpecific(elems) match {
				case Yes(specific) => specific.sameElements[U, O](elems, that)
				case No            => super.sameElements[U, O](elems, that)
			}
		abstract override def search[U >: X](elems :Xs, elem :U, from :Int, until :Int)
		                                    (implicit ord :Ordering[U]) :SearchResult =
			moreSpecific(elems) match {
				case Yes(specific) => specific.search[U](elems, elem, from, until)
				case No            => super.search[U](elems, elem, from, until)
			}
		abstract override def distinct(elems :Xs) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.distinct(elems)
			case No            => super.distinct(elems)
		}
		abstract override def distinctBy[A](elems :Xs)(f :X => A) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.distinctBy(elems)(f)
			case No            => super.distinctBy(elems)(f)
		}
		abstract override def reverse(elems :Xs) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.reverse(elems)
			case No            => super.reverse(elems)
		}
		abstract override def sorted[U >: X](elems :Xs)(implicit ord :Ordering[U]) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.sorted[U](elems)
			case No            => super.sorted[U](elems)
		}
//		abstract override def sortWith(elems :Xs)(lt :(X, X) => Boolean) :C = moreSpecific(elems) match {
//			case Yes(specific) => specific.sortWith(elems)(lt)
//			case No            => super.sortWith(elems)(lt)
//		}
//		abstract override def sortBy[A](elems :Xs)(f :X => A)(implicit ord :Ordering[A]) :C =
//			moreSpecific(elems) match {
//				case Yes(specific) => specific.sortBy(elems)(f)
//				case No            => super.sortBy(elems)(f)
//			}
		abstract override def updated[U >: X](elems :Xs, index :Int, elem :U) :CC[U] = moreSpecific(elems) match {
			case Yes(specific) => specific.updated(elems, index, elem)
			case No            => super.updated(elems, index, elem)
		}
		abstract override def prepended[U >: X](elems :Xs, elem :U) :CC[U] = moreSpecific(elems) match {
			case Yes(specific) => specific.prepended(elems, elem)
			case No            => super.prepended(elems, elem)
		}
		abstract override def appended[U >: X](elems :Xs, elem :U) :CC[U] = moreSpecific(elems) match {
			case Yes(specific) => specific.appended(elems, elem)
			case No            => super.appended(elems, elem)
		}
		abstract override def updatedAll[U >: X, O](elems :Xs, index :Int, patch :O)
		                                           (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			moreSpecific(elems) match {
				case Yes(specific) => specific.updatedAll(elems, index, patch)
				case No            => super.updatedAll(elems, index, patch)
			}
		abstract override def overwritten[U >: X, O](elems :Xs, index :Int, patch :O)
		                                            (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			moreSpecific(elems) match {
				case Yes(specific) => specific.overwritten(elems, index, patch)
				case No            => super.overwritten(elems, index, patch)
			}
		abstract override def prependedAll[U >: X, O](elems :Xs, prefix :O)
		                                             (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			moreSpecific(elems) match {
				case Yes(specific) => specific.prependedAll(elems, prefix)
				case No            => super.prependedAll(elems, prefix)
			}
		abstract override def patch[A >: X, O](elems :Xs, from :Int, other :O, replaced :Int)
		                                      (implicit likeCollection :LikeCollection[A, O]) :CC[A] =
			moreSpecific(elems) match {
				case Yes(specific) => specific.patch(elems, from, other, replaced)
				case No            => super.patch(elems, from, other, replaced)
			}
		abstract override def padTo[U >: X](elems :Xs, len :Int, elem :U) :CC[U] = moreSpecific(elems) match {
			case Yes(specific) => specific.padTo(elems, len, elem)
			case No            => super.padTo(elems, len, elem)
		}
		abstract override def reverseIterator(elems :Xs) :Iterator[X] = moreSpecific(elems) match {
			case Yes(specific) => specific.reverseIterator(elems)
			case No            => super.reverseIterator(elems)
		}
		abstract override def toImpureSeq(elems :Xs) :collection.Seq[X] = moreSpecific(elems) match {
			case Yes(specific) => specific.toImpureSeq(elems)
			case No            => super.toImpureSeq(elems)
		}
	}
	
	
	
	/** Implements those methods of `LikeSeq` which can be expressed solely in terms of other methods of this interface,
	  * without delegating to any standard Scala interface. In particular, it does not rely on `toOps`,
	  * `toIterableOnceOps`, `toIterableOnce`, or `iterator`.
	  */
	trait LikeSeqBasics[+X, -Xs, +CC[_], +C] extends LikeIterableBasics[X, Xs, CC, C] with LikeSeq[X, Xs, CC, C] {

		override def head(elems :Xs) :X =
			try apply(elems, 0) catch {
				case _ :IndexOutOfBoundsException => noSuch_!(toString + ".head(" + infoString(elems) + ")")
			}
		override def headOption(elems :Xs) :Option[X] =
			if (isEmpty(elems)) None else Some(head(elems))

		override def lastOption(elems :Xs) :Option[X] =
			if (isEmpty(elems)) None else Some(last(elems))

		override def exists(elems :Xs)(p :X => Boolean) :Boolean =
			indexWhere(elems, 0)(p) >= 0

		override def forall(elems :Xs)(p :X => Boolean) :Boolean = !exists(elems)(!p(_))

		override def indexOf[A >: X](elems :Xs, from :Int, elem: A): Int = indexWhere(elems, from)(elem == _)
		override def lastIndexOf[A >: X](elems :Xs, end :Int, elem :A): Int =
			lastIndexWhere(elems, end)(elem == _)

		override def indexOfSlice[U >: X](elems :Xs, from :Int, that :collection.Seq[U]) :Int =
			indexOfSlice[U, that.type](elems, from, that :that.type)(
				forOps[U, collection.Seq[U], collection.Seq, collection.Seq[U]].specific(that)
			)
		override def lastIndexOfSlice[U >: X](elems :Xs, end :Int, that :collection.Seq[U]) :Int =
			lastIndexOfSlice[U, that.type](elems, end, that :that.type)(
				forOps[U, collection.Seq[U], collection.Seq, collection.Seq[U]].specific(that)
			)

		//These methods can be easily implemented here as they are good for most implementations,
		// but they obviously rely on the iterator, which we promised we wouldn't.
//		override def indexOfSlice[U >: X, O](elems :Xs, from :Int, that :O)
//		                                    (implicit likeSeq :LikeSeq[U, O, Any1, _]) :Int =
//			KMP.indexOfSlice(elems, that, from)(this, likeSeq)
//
//		override def lastIndexOfSlice[U >: X, O](elems :Xs, end :Int, that :O)
//		                                        (implicit likeSeq :LikeSeq[U, O, Any1, _]) :Int =
//			KMP.lastIndexOfSlice(elems, that, end)(this, likeSeq)

		override def sameElements[U >: X, O](elems :Xs, that :O)
		                                    (implicit likeCollection :LikeCollection[U, O]) :Boolean =
			corresponds[U, O](elems, that)(_ == _)

		/** Performs binary search within range `[from, until)` using `apply`. The indices must be valid. */
		@tailrec protected final def binarySearch[U >: X](elems :Xs, elem :U, from :Int, until :Int)
		                                                 (implicit ord :Ordering[U]) :SearchResult =
			if (until <= from)
				InsertionPoint(from)
			else {
				val mid = from + (until - from) / 2
				if (ord.compare(apply(elems, mid), elem) < 0)
					binarySearch(elems, elem, mid + 1, until)
				else
					binarySearch(elems, elem, from, mid)
			}
		@tailrec protected final def linearSearch[U >: X](elems :Iterator[U], elem :U, offset :Int, until :Int) :SearchResult =
			if (until <= 0 || !elems.hasNext) InsertionPoint(offset)
			else if (elems.next() == elem) Found(offset)
			else linearSearch(elems, elem, offset + 1, until - 1)

		override def distinct(elems :Xs) :C = distinctBy(elems)(identity)
		override def distinctBy[A](elems :Xs)(f :X => A) :C = {
			val unique = mutable.Set.empty[A]
			filter(elems) { elem => unique.add(f(elem)) }
		}


		override def sortWith(elems :Xs)(lt :(X, X) => Boolean) :C = sorted(elems)(Ordering.fromLessThan(lt))
		override def sortBy[A](elems :Xs)(f :X => A)(implicit ord :Ordering[A]) :C = sorted(elems)(ord on f)

		override def reverseIterator(elems :Xs) :Iterator[X] = util.reverse(toIterableOnce(elems)).iterator

		override def toIterable(elems :Xs) :Iterable[X] = toImpureSeq(elems)

		//Or should we expose it as SeqView?
		override def view(elems :Xs) :View[X] = new LikeSeqAdapterView(elems, this)
	}



	/** Implements */
	trait OfKnownSize[+X, -Xs, +CC[_], +C]
		extends LikeSeqBasics[X, Xs, CC, C] with LikeIterable.OfKnownSize[X, Xs, CC, C]
	{
		override def forall(elems :Xs)(p :X => Boolean) :Boolean =
			segmentLength(elems, 0)(p) == size(elems)

		override def endsWith[U >: X, O](elems :Xs, that :O)
		                                (implicit likeIterable :LikeIterable[U, O, Any1, _]) :Boolean =
		{
			val thatSize = likeIterable.size(that)
			thatSize == 0 || startsWith[U, O](elems, size(elems) - thatSize, that)
		}

		override def head(elems :Xs) :X =
			if (size(elems) == 0) noSuch_!(infoString(elems) + ".head")
			else apply(elems, 0)

		override def takeWhile(elems :Xs)(p :X => Boolean) :C = take(elems, segmentLength(elems)(p))
		override def dropWhile(elems :Xs)(p :X => Boolean) :C = drop(elems, segmentLength(elems)(p))
		//		override def splitAt(elems :Xs, n :Int) :(C, C) = (take(elems, n), drop(elems, n))
		override def span(elems :Xs)(p :X => Boolean) :(C, C) = {
			val i = segmentLength(elems)(p)
			(take(elems, i), drop(elems, i))
		}
	}



	/** Implementation of a majority of methods of `LikeSeq` in terms of `apply` and `size`.
	  * The latter is implemented as `knownSize`, and `isApplyFast` is set to `true`, but not `isApplyPreferred`.
	  */
	trait FromApply[+X, -Xs, +CC[_], +C] extends OfKnownSize[X, Xs, CC, C]/* with LikeIterableOnceFactory[X, Xs, CC, C]*/ {
		override def isApplyFast(elems :Xs) :Boolean = true

		override def count(elems :Xs)(p :X => Boolean) :Int = {
			val length = size(elems)
			var res = 0
			var i   = 0
			while (i < length) {
				if (p(apply(elems, i)))
					res += 1
				i += 1
			}
			res
		}
		override def collectFirst[A](elems :Xs)(pf :PartialFunction[X, A]) :Option[A] = {
			val length = size(elems)
			var i = 0
			var res :Option[A] = None
			while (i < length & (res eq None)) {
				i += 1
				res = pf.applyAndThenEither[Option[A]](apply(elems, i), Some.apply _, _ => None)
			}
			res
		}
		override def foldLeft[A](elems :Xs)(z :A)(op :(A, X) => A) :A = {
			val length = size(elems)
			var acc = z
			var i = 0
			while (i < length) {
				acc = op(acc, apply(elems, i))
				i  += 1
			}
			acc
		}
		override def reduceLeft[U >: X](elems :Xs)(op :(U, X) => U) :U = {
			val length = size(elems)
			if (length == 0)
				unsupported_!(toString + ".foldLeft(" + infoString(elems) + ")")
			var acc :U = apply(elems, 0)
			var i   = 1
			while (i < length) {
				acc = op(acc, apply(elems, i))
				i  += 1
			}
			acc
		}
		override def foldRight[A](elems :Xs)(z :A)(op :(X, A) => A) :A = {
			var acc = z
			var i   = size(elems)
			while (i > 0) {
				i -= 1
				val elem = apply(elems, i)
				acc = op(elem, acc)
			}
			acc
		}
		override def reduceRight[U >: X](elems :Xs)(op :(X, U) => U) :U = {
			var i = size(elems)
			if (i == 0)
				unsupported_!(toString + ".reduceRight(" + infoString(elems) + ")")
			i -= 1
			var acc :U = apply(elems, i)
			while (i > 0) {
				i -= 1
				val elem = apply(elems, i)
				acc = op(elem, acc)
			}
			acc
		}

		override def last(elems :Xs) :X =
			if (size(elems) == 0) noSuch_!(infoString(elems) + ".head")
			else apply(elems, size(elems) - 1)

		override def find(elems :Xs)(p :X => Boolean) :Option[X] = indexWhere(elems)(p) match {
			case -1 => None
			case  i => Some(apply(elems, i))
		}
		override def findLast(elems :Xs)(p :X => Boolean) :Option[X] = lastIndexWhere(elems)(p) match {
			case -1 => None
			case  i => Some(apply(elems, i))
		}
		override def segmentLength(elems :Xs, from :Int)(p :X => Boolean) :Int = {
			val len   = size(elems)
			val start = math.max(0, math.min(len, from))
			val idx   = indexWhere(elems, from, false)(p)
			if (idx == -1) len - start else idx - start
		}

		override def indexWhere(elems :Xs, from :Int)(p :X => Boolean) :Int = indexWhere(elems, from, true)(p)
		protected def indexWhere(elems :Xs, from :Int, truth :Boolean)(p :X => Boolean) :Int = {
			val len   = size(elems)
			val start = math.max(0, math.min(len, from))
			var i     = start
			while (i < len) {
				val elem = apply(elems, i)
				if (p(elem) == truth)
					return i
				i += 1
			}
			-1
		}
		override def lastIndexWhere(elems :Xs, end :Int)(p :X => Boolean) :Int = {
			val len   = size(elems)
			val start = math.max(0, math.min(len - 1, end))
			var i     = start
			while (i >= 0) {
				val elem = apply(elems, i)
				if (p(elem))
					return i
				i -= 1
			}
			-1
		}

		override def indexOfSlice[U >: X, O](elems :Xs, from :Int, that :O)
		                                    (implicit likeSeq :LikeSeq[U, O, Any1, _]) :Int =
			KMP.indexOfSlice(elems, that, from)(this, likeSeq)

		override def lastIndexOfSlice[U >: X, O](elems :Xs, end :Int, that :O)
		                                        (implicit likeSeq :LikeSeq[U, O, Any1, _]) :Int =
			KMP.lastIndexOfSlice(elems, that, end)(this, likeSeq)

		override def startsWith[U >: X, O](elems :Xs, offset :Int, that :O)
		                                  (implicit likeCollection :LikeCollection[U, O]) :Boolean =
		{
			val thisSize = size(elems)
			val thatSize = likeCollection.knownSize(that)
			var i        = math.max(0, math.min(offset, thisSize))
			likeCollection match {
				case _ if thisSize - i < thatSize => false
				case other :LikeSeq[U, O, Any1, _] if thatSize >= 0 && other.isApplyFast(that) =>
					if (thisSize - i < thatSize)
						return false
					while (i < thisSize) {
						if (apply(elems, i) != other(that, i - offset))
							return false
						i += 1
					}
					true
				case _ if thatSize == 0 => true
				case _ =>
					val other = likeCollection.iterator(that)
					while (i < thisSize && other.hasNext) {
						if (apply(elems, i) != other.next())
							return false
						i += 1
					}
					true
			}
		}

		override def search[U >: X](elems :Xs, elem :U, from :Int, until :Int)
		                           (implicit ord :Ordering[U]) :SearchResult =
		{
			val len = size(elems)
			val lo  = math.max(0, math.min(len, from))
			val hi  = math.max(0, math.min(lo, until))
			binarySearch(elems, elem, lo, hi)
		}

		override def corresponds[A, O](elems :Xs, that :O)(p :(X, A) => Boolean)
		                              (implicit likeCollection :LikeCollection[A, O]) :Boolean =
			likeCollection match {
				case seq :LikeSeq[A, O, Any1, _] if seq.isApplyPreferred(that) =>
					val length = size(elems)
					length == seq.size(that) && {
						var okSoFar = true
						var i = 0
						while (i < length && okSoFar) {
							okSoFar = p(apply(elems, i), seq(that, i))
							i += 1
						}
						okSoFar
					}
				case _ =>
					val thatSize = likeCollection.knownSize(that)
					val thisSize = size(elems)
					var i = 0
					var okSoFar = true
					if (thatSize == -1) {
						val it = likeCollection.iterator(that)
						while (i < thisSize && it.hasNext) {
							okSoFar = p(apply(elems, i), it.next())
							i += 1
						}
						okSoFar & i == thisSize && !it.hasNext
					} else
						thisSize == thatSize && {
							val it = likeCollection.iterator(that)
							while (i < thisSize) {
								okSoFar = p(apply(elems, i), it.next())
								i += 1
							}
							okSoFar
						}
			}


		override def takeWhile(elems :Xs)(p :X => Boolean) :C = take(elems, segmentLength(elems)(p))
		override def dropWhile(elems :Xs)(p :X => Boolean) :C = drop(elems, segmentLength(elems)(p))
		override def span(elems :Xs)(p :X => Boolean) :(C, C) = {
			val i = segmentLength(elems)(p)
			(take(elems, i), drop(elems,i))
		}

		override def groupMapReduce[K, A](elems :Xs)(key :X => K)(f :X => A)(reduce :(A, A) => A) :Map[K, A] = {
			val length = size(elems)
			var i = 0
			val mapped = mutable.Map.empty[K, Buffer[A]]
			while (i < length) {
				val elem = apply(elems, i)
				val k = key(elem)
				val v = f(elem)
				val buffer = mapped.getOrElse(k, null)
				if (buffer == null)
					mapped.put(k, TemporaryBuffer.empty[A] += v)
				else
					buffer += v
				i += 1
			}
			Map from mapped.iterator.map { case (key, vals) => (key, vals.reduce(reduce)) }
		}

		override def appendTo[U >: X](elems :Xs, buffer :Buffer[U]) :Unit = {
			val length = size(elems)
			var i = 0
			while (i < length) {
				buffer += apply(elems, i)
				i += 1
			}
		}
		override def prependTo[U >: X](elems :Xs, buffer :Buffer[U]) :Unit = {
			var i = size(elems)
			while (i > 0) {
				i -= 1
				apply(elems, i) +=: buffer
			}
		}

		override def copyTo[U >: X, O](elems :Xs, seq :O, index :Int)
		                              (implicit likeSeq :LikeMutableIndexedSeq[U, O, Any1, _]) :Int =
		{
			val thisSize = size(elems)
			val thatSize = likeSeq.size(seq)
			if (thisSize == 0 | index >= thatSize)
				0
			else if (index < 0)
				outOfBounds_!(
					toString + ".copyTo(" + infoString(elems) + ", " + likeSeq.infoString(seq) + ", " + index + ")"
				)
			else {
				val copied = math.min(thisSize, thatSize - index)
				var i = 0
				while (i < copied) {
					likeSeq.update(seq, index + i, apply(elems, i))
					i += 1
				}
				copied
			}
		}

		override def copyToArray[U >: X](elems :Xs, array :Array[U], start :Int, max :Int) :Int = {
			val copied = elementsToCopy(size(elems), array, start, max)
			var i = 0
			while (i < copied) {
				array(start + i) = apply(elems, i)
				i += 1
			}
			copied
		}
		override def cyclicCopyToArray[U >: X](elems :Xs, array :Array[U], index :Int, max :Int) :Int =
			if (array.length == 0)
				0
			else {
				val length = size(elems)
				val offset = index % array.length
				val suffix = elementsToCopy(length, array, offset, max)
				var i      = 0
				while (i < suffix) {
					array(offset + i) = apply(elems, i)
					i += 1
				}
				val copied = elementsToCopy(length, array, 0, max)
				var j = 0
				while (i < copied) {
					array(j) = apply(elems, i)
					i += 1
					j += 1
				}
				copied
			}

		override def addTo(elems :Xs, builder :Builder[X, Any], from :Int, until :Int) :Int = {
			val thisSize = size(elems)
			val start = math.max(0, math.min(thisSize, from))
			val end   = math.max(start, math.min(thisSize, until))
			var i = start
			while (i < end) {
				builder += apply(elems, i)
				i += 1
			}
			end - start
		}


		override def iterator(elems :Xs) :Iterator[X] =
			new AbstractIndexedIterator[X](0, size(elems)) {
				override def head :X = apply(elems, index)
				override def className :String = FromApply.this.toString + ".iterator(" + infoString(elems) + ")"
			}
		override def reverseIterator(elems :Xs) :Iterator[X] =
			new AbstractReverseIndexedIterator[X](0, size(elems)) {
				override def head :X = apply(elems, index - 1)
				override def className :String = FromApply.this.toString + ".reverseIterator(" + infoString(elems) + ")"
			}

		override def stepper[S <: Stepper[_]](elems :Xs)(implicit shape :StepperShape[X, S]) :S =
			LikeSeqIndexedStepper(elems)(this, shape)
	}



	/** Implements some methods assuming `C` is a list/`LinearSeq` with fast `tail` operation, and without random
	  * accessing.
	  */ //todo: implement the majority of methods, and probably create LikeLinearSeq so we can have forOps
	trait FromTail[X, +CC[_], C] extends LikeSeqBasics[X, C, CC, C] {
		override def drop(elems :C, n :Int) :C = {
			val thisSize = knownSize(elems)
			var rem = n
			var list = elems
			if (thisSize >= 0)
				if (n >= thisSize)
					list = empty(elems)
				else {
					while (rem > 0) {
						rem -= 1
						list = tail(list)
					}
				}
			else {
				while (rem > 0 && !isEmpty(list)) {
					rem -= 1
					list = tail(list)
				}
			}
			list
		}

		override def dropWhile(elems :C)(p :X => Boolean) :C = {
			var list = elems
			while (!isEmpty(list) && p(head(list)))
				list = tail(list)
			list
		}
/*
		override def updatedAll[U >: X, O](elems :Xs, index :Int, patch :O)
		                                  (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
		{
			val thisSize = knownSize(elems)
			val thatSize = likeCollection.knownSize(patch)
			if (index < 0 | thisSize >= 0 & (index > thisSize | thatSize >= 0 & index > thisSize - thatSize))
				outOfBounds_!(toString + ".updatedAll(" + infoString(elems) + ", " + index + "," +
					likeCollection.infoString(patch)
				)
			if (index == 0)
				if (thisSize >= 0 & thatSize >= 0 & thatSize == thisSize)
					toGeneric[U](elems)(likeCollection.toIterableOnce(patch))
				else {

				}
		}
		override def overwritten[U >: X, O](elems :Xs, index :Int, patch :O)
		                                   (implicit likeCollection :LikeCollection[U, O]) :CC[U] = ???
*/

		override def appendTo[U >: X](elems :C, buffer :Buffer[U]) :Unit = {
			var list = elems
			while (!isEmpty(list)) {
				buffer addOne head(list)
				list = tail(list)
			}
		}

		override def copyTo[U >: X, O](elems :C, seq :O, index :Int)
		                              (implicit likeSeq :LikeMutableIndexedSeq[U, O, Any1, _]) :Int =
		{
			val thatSize = likeSeq.size(seq)
			val thisSize = knownSize(elems)
			if (index >= thatSize)
				0
			else if (thisSize >= 0) {
				val copied = math.min(thisSize, thatSize - math.max(0, index))
				var i = 0
				var list = elems
				while (i < copied) {
					likeSeq.update(seq, index + i, head(list))
					list = tail(list)
					i += 1
				}
				copied
			} else {
				val max = thatSize - math.max(0, index)
				var i = 0
				var list = elems
				while (i < max && !isEmpty(list)) {
					likeSeq.update(seq, index + i, head(list))
					list = tail(list)
					i += 1
				}
				i
			}
		}

		override def addTo(elems :C, builder :Builder[X, Any], from :Int, until :Int) :Int = {
			val size = knownSize(elems)
			if (until <= 0 | until <= from | size >= 0 & from >= size)
				0
			else {
				var list = drop(elems, from)
				var i = from
				if (size >= 0 & until <= size) {
					while (i < until) {
						builder addOne head(list)
						list = tail(list)
						i += 1
					}
				} else {
					while (i < until && !isEmpty(list)) {
						builder addOne head(list)
						list = tail(list)
						i += 1
					}
				}
				i - math.max(0, from)
			}
		}

		override def copyToArray[U >: X](elems :C, array :Array[U], start :Int, max :Int) :Int = {
			val size = knownSize(elems)
			if (max <= 0 || size == 0 || array.length == 0 || start >= array.length)
				0
			else if (start < 0)
				outOfBounds_!(toString + ".copyToArray(" + infoString(elems) + ", " + errorString(array) +
					", " + start + ", " + max +  ")"
				)
			else if (size > 0) {
				val copied = elementsToCopy(size, array, start, max)
				var i = 0
				var list = elems
				while (i < copied) {
					array(start + i) = head(list)
					list = tail(list)
					i += 1
				}
				copied
			} else {
				var i = 0
				val end = math.min(array.length - start, max)
				var list = elems
				while (i < end && !isEmpty(list)) {
					array(start + i) = head(list)
					list = tail(list)
					i += 1
				}
				i
			}
		}

		override def cyclicCopyToArray[U >: X](elems :C, array :Array[U], index :Int, max :Int) :Int =
			if (max <= 0 || array.length == 0)
				0
			else {
				val length = array.length
				val start  = index % length
				var list   = elems //drop(elems, from)
				var until  = start + math.min(length - start, max)
				var copied = 0
				var i      = start
				while (i < until && !isEmpty(list)) {
					while (i < until && !isEmpty(list)) {
						array(i) = head(list)
						list = tail(list)
						i += 1
						copied += 1
					}
					if (copied < max) {
						i = 0
						until = math.min(start, max - copied)
					}
				}
				copied
			}
	}



	/** Implements most methods of `LikeSeq` in terms of `knownSize`, `iterator`,
	  * and `reverseIterator` if `knownSize >= 0`.
	  */
	trait FromIterator[X, -Xs, +CC[_], +C]
		extends LikeIterable.FromIterator[X, Xs, CC, C] with LikeSeqBasics[X, Xs, CC, C]
	{
		override def findLast(elems :Xs)(p :X => Boolean) :Option[X] = knownSize(elems) match {
			case -1 =>
				val it = iterator(elems)
				var last :Opt[X] = None
				while (it.hasNext) {
					val next = it.next()
					if (p(next))
						last = One(next)
				}
				last.toOption
			case  0 => None
			case  _ => reverseIterator(elems).find(p)
		}

		override def segmentLength(elems :Xs, from :Int)(p :X => Boolean) :Int = knownSize(elems) match {
			case -1 =>
				val it = iterator(elems).dropInPlace(from)
				var res = 0
				while (it.hasNext && p(it.next()))
					res += 1
				res
			case  n if n <= from => 0
			case  n => iterator(elems).indexWhere(!p(_), from) match {
				case -1 => n - math.max(from, 0)
				case  i => i - math.max(from, 0)
			}
		}
		override def indexWhere(elems :Xs, from :Int)(p :X => Boolean) :Int =
			if (knownSize(elems) == 0) -1
			else iterator(elems).indexWhere(p, from)

		override def indexOf[U >: X](elems :Xs, from :Int, elem :U) :Int =
			if (knownSize(elems) == 0) -1
			else iterator(elems).indexOf(elem, from)

		override def lastIndexWhere(elems :Xs, end :Int)(p :X => Boolean) :Int = knownSize(elems) match {
			case  0 => -1
			case  _ if end < 0 => -1
			case -1 =>
				val it = iterator(elems)
				var i = 0
				var res = -1
				while (i <= end && it.hasNext) {
					if (p(it.next()))
						res = i
					i += 1
				}
				res
			case  n => reverseIterator(elems).indexWhere(p, n - end - 1) match {
				case -1 => -1
				case  i => n - i - 1
			}
		}
		override def lastIndexOf[U >: X](elems :Xs, end :Int, elem :U) :Int =
			if (end < 0 || knownSize(elems) == 0) -1 else lastIndexWhere(elems, end)(_ == elem)

		override def indexOfSlice[U >: X, O](elems :Xs, from :Int, that :O)
		                                    (implicit likeSeq :LikeSeq[U, O, Any1, _]) :Int =
			KMP.indexOfSlice(elems, that, from)(this, likeSeq)

		override def lastIndexOfSlice[U >: X, O](elems :Xs, end :Int, that :O)
		                                        (implicit likeSeq :LikeSeq[U, O, Any1, _]) :Int =
			KMP.lastIndexOfSlice(elems, that, end)(this, likeSeq)

		private def startsWith[U >: X](these :Iterator[U], those :Iterator[U]) :Boolean = {
			while (these.hasNext && those.hasNext)
				if (these.next() != those.next())
					return false
			!those.hasNext
		}
		override def startsWith[U >: X, O](elems :Xs, offset :Int, that :O)
		                                  (implicit likeCollection :LikeCollection[U, O]) :Boolean =
			offset >= 0 && {
				val thisSize = knownSize(elems)
				val thatSize = likeCollection.knownSize(that)
				if (thisSize >= 0)
					offset <= thisSize & (thatSize == -1 | offset <= thisSize - thatSize) && (
						thatSize == 0 || startsWith(iterator(elems), likeCollection.iterator(that))
					)
				else {
					val these = iterator(elems).dropInPlace(offset - 1)
					these.hasNext && (thatSize == 0 || { //verify that elems has at least offset elements
						these.next(); startsWith(these, likeCollection.iterator(that))
					})
				}
			}

		override def endsWith[U >: X, O](elems :Xs, that :O)
		                                (implicit likeIterable :LikeIterable[U, O, Any1, _]) :Boolean =
		{
			val thatSize = likeIterable.knownSize(that)
			thatSize == 0 || {
				val thisSize = knownSize(elems)
				if (thatSize >= 0) //consider: just use size instead of knownSize
					if (thisSize >= 0)
						if (thisSize == thatSize)
							startsWith(iterator(elems), likeIterable.iterator(that))
						else
							thatSize < thisSize && (likeIterable match {
								case likeSeq :LikeSeq[U, O, Any1, _] =>
									startsWith(reverseIterator(elems), likeSeq.reverseIterator(that))
								case _ =>
									startsWith(
										iterator(elems).dropInPlace(thisSize - thatSize),
										likeIterable.iterator(that)
									)
							})
					else {
						val window = SpillBuffer.ofMax[U](thatSize)
						val these = iterator(elems)
						while (these.hasNext)
							window += these.next()
						window.size == thatSize && startsWith(window.iterator, likeIterable.iterator(that))
					}
				else {
					val these = iterator(elems)
					val those = likeIterable.iterator(that)
					val seq1  = mutable.ArrayDeque.empty[U] //A rolling buffer with last that.size elements of elems.
					val seq2  = DefaultBuffer.empty[U]      //A copy of that.
					var equal = true
					while (equal && these.hasNext && those.hasNext) {
						val elem1 = those.next()
						val elem2 = those.next()
						seq1 += elem1
						seq2 += elem2
						equal = elem1 == elem2
					} //Either we found an unequal element or we ran out of elements in one of the iterators.
					equal && !these.hasNext && !those.hasNext || these.hasNext && {
						while (those.hasNext)
							seq2 += those.next()
						while (seq1.length < seq2.length && these.hasNext)
							seq1 += these.next()
						//If elems.size == that.size then we know !equal from the previous condition.
						these.hasNext && {
							while (these.hasNext) {
								seq1.remove(0)
								seq1 += these.next()
							}
							seq1 == seq2
						}
					}
				}

			}
		}

//		override def sameElements[U >: X, O](elems :Xs, that :O)
//		                                    (implicit likeCollection :LikeCollection[U, O]) :Boolean =
//			corresponds(elems, that)(_ == _)

		override def search[U >: X](elems :Xs, elem :U, from :Int, until :Int)
		                           (implicit ord :Ordering[U]) :SearchResult =
			if (isApplyFast(elems)) {
				val size         = this.size(elems)
				val clippedFrom  = math.max(0, math.min(size, from))
				val clippedUntil = math.max(clippedFrom, math.min(size, until))
				binarySearch(elems, elem, clippedFrom, clippedUntil)
			} else {
				val size = knownSize(elems)
				if (size == 0 | until <= 0)
					InsertionPoint(0)
				else if (size > 0 & from >= size)
					InsertionPoint(size)
				else
					linearSearch(elems.iterator.dropInPlace(from), elem, from, until)
			}

		override def reverse(elems :Xs) :C = makeSpecific(elems)(reverseIterator(elems))

		override def sorted[U >: X](elems :Xs)(implicit ord :Ordering[U]) :C = {
			val a = toArray[Any](elems).asInstanceOf[Array[X]]
			a.sortInPlace[U]()
			makeSpecific(elems)(ArrayIterator(a))
		}
//		override def sortWith(elems :Xs)(lt :(X, X) => Boolean) :C = sorted(elems)(Ordering.fromLessThan(lt))
//		override def sortBy[A](elems :Xs)(f :X => A)(implicit ord :Ordering[A]) :C = sorted(elems)(ord on f)

		override def updated[U >: X](elems :Xs, index :Int, elem :U) :CC[U] =
			makeGeneric[U](elems)(iterator(elems).updated(index, elem))

		override def prepended[U >: X](elems :Xs, elem :U) :CC[U] =
			makeGeneric[U](elems)(elem +: iterator(elems))

		override def appended[U >: X](elems :Xs, elem :U) :CC[U] =
			makeGeneric[U](elems)(iterator(elems) :+ elem)

		override def updatedAll[U >: X, O](elems :Xs, index :Int, patch :O)
		                                  (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			makeGeneric[U](elems)(iterator(elems).updatedAll(index, likeCollection.iterator(patch)))

		override def overwritten[U >: X, O](elems :Xs, index :Int, patch :O)
		                                   (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			makeGeneric[U](elems)(iterator(elems).overwritten(index, likeCollection.iterator(patch)))

		override def prependedAll[U >: X, O](elems :Xs, prefix :O)
		                                    (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			makeGeneric[U](elems)(likeCollection.iterator(prefix) :++ iterator(elems))

		override def patch[U >: X, O](elems :Xs, from :Int, other :O, replaced :Int)
		                             (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			makeGeneric[U](elems)(iterator(elems).patch(from, likeCollection.iterator(other), replaced))

		override def padTo[U >: X](elems :Xs, len :Int, elem :U) :CC[U] = {
			val size = knownSize(elems)
			if (size >= 0)
				if (size >= len) toGeneric(elems)
				else appendedAll[U, Iterator[U]](elems, Iterator.const(len - size)(elem))//(LikeCollection.forLikeIterableOnce)
			else {
				val i = iterator(elems).counting
				makeGeneric(elems)(i ++ Iterator.const[U](math.max(0, len - i.total))(elem)) //lazy concat
			}
		}
	}



	/** Implements methods of `LikeSeq` by delegating to corresponding methods in `SeqOps` of `toOps(elems)`. */
	trait FromOps[+X, -Xs, +CC[_], +C] extends LikeSeq[X, Xs, CC, C] with LikeIterable.FromOps[X, Xs, CC, C] {
		override def apply(elems :Xs, i :Int) :X = toOps(elems)(i)

		override def contains[A >: X](elems :Xs, elem :A) :Boolean = toOps(elems).contains(elem)

		override def findLast(elems :Xs)(p :X => Boolean) :Option[X] = toOps(elems).findLast(p)

		override def segmentLength(elems :Xs, from :Int)(p :X => Boolean) :Int =
			toOps(elems).segmentLength(p, from)

		override def indexWhere(elems :Xs, from :Int)(p :X => Boolean) :Int =
			toOps(elems).indexWhere(p, from)

		override def lastIndexWhere(elems :Xs, end :Int)(p :X => Boolean) :Int =
			toOps(elems).lastIndexWhere(p, end)

		override def indexOf[U >: X](elems :Xs, from :Int, elem :U) :Int =
			toOps(elems).indexOf(elem, from)

		override def lastIndexOf[U >: X](elems :Xs, end :Int, elem :U) :Int =
			toOps(elems).lastIndexOf(elem, end)

		override def indexOfSlice[A >: X](elems :Xs, from :Int, that :collection.Seq[A]) :Int =
			toOps(elems).indexOfSlice(that, from)

		override def indexOfSlice[U >: X, O](elems :Xs, from :Int, that :O)
		                                    (implicit likeSeq :LikeSeq[U, O, Any1, _]) :Int =
			toOps(elems).indexOfSlice(likeSeq.toImpureSeq(that), from)

		override def lastIndexOfSlice[A >: X](elems :Xs, end :Int, that :collection.Seq[A]) :Int =
			toOps(elems).lastIndexOfSlice(that, end)

		override def lastIndexOfSlice[U >: X, O](elems :Xs, end :Int, that :O)
		                                        (implicit likeSeq :LikeSeq[U, O, Any1, _]) :Int =
			toOps(elems).lastIndexOfSlice(likeSeq.toImpureSeq(that), end)

		override def startsWith[U >: X, O](elems :Xs, offset :Int, that :O)
		                                  (implicit likeCollection :LikeCollection[U, O]) :Boolean =
			toOps(elems).startsWith(likeCollection.toIterableOnce(that), offset)

		override def endsWith[U >: X, O](elems :Xs, that :O)
		                                (implicit likeIterable :LikeIterable[U, O, Any1, _]) :Boolean =
			toOps(elems).endsWith(likeIterable.toIterable(that))

		override def sameElements[U >: X, O](elems :Xs, that :O)
		                                    (implicit likeCollection :LikeCollection[U, O]) :Boolean =
			toOps(elems).sameElements(likeCollection.toIterableOnce(that))

		override def search[A >: X](elems :Xs, elem :A, from :Int, until :Int)(implicit ord :Ordering[A]) :SearchResult =
			toOps(elems).search(elem, from, until)


		override def distinct(elems :Xs) :C = toOps(elems).distinct
		override def distinctBy[A](elems :Xs)(f :X => A) :C = toOps(elems).distinctBy(f)
		override def reverse(elems :Xs) :C = toOps(elems).reverse
		override def sortWith(elems :Xs)(lt :(X, X) => Boolean) :C = toOps(elems).sortWith(lt)
		override def sortBy[A](elems :Xs)(f :X => A)(implicit ord :Ordering[A]) :C = toOps(elems).sortBy(f)
		override def sorted[A >: X](elems :Xs)(implicit ord :Ordering[A]) :C = toOps(elems).sorted[A]


		override def updated[U >: X](elems :Xs, index: Int, elem: U): CC[U] = toOps(elems).updated(index, elem)
		override def prepended[U >: X](elems :Xs, elem :U) :CC[U] = toOps(elems).prepended(elem)
		override def appended[U >: X](elems :Xs, elem :U) :CC[U] = toOps(elems).appended(elem)

		override def updatedAll[U >: X, O](elems :Xs, index :Int, patch :O)
		                                  (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			if (likeCollection.knownSize(patch) == 0) toGeneric(elems)
			else toOps(elems).updatedAll(index, likeCollection.toIterableOnce(patch))

		override def overwritten[U >: X, O](elems :Xs, index :Int, patch :O)
		                                   (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
		{
			val thatSize = likeCollection.knownSize(patch)
			val thisSize = knownSize(elems)
			val nothingToCopy = thisSize == 0 | thatSize == 0 || (
				if (index < 0) index == Int.MinValue | thatSize > 0 & thatSize <= -index
				else thisSize > 0 & index >= thisSize
				)
			if (nothingToCopy) toGeneric(elems)
			else toOps(elems).overwritten(index, likeCollection.toIterableOnce(patch))
		}

		override def prependedAll[U >: X, O](elems :Xs, prefix :O)
		                                    (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			if (likeCollection.knownSize(prefix) == 0) toGeneric(elems)
			else toOps(elems).prependedAll(likeCollection.toIterableOnce(prefix))

		override def concat[U >: X, O](elems :Xs, suffix :O)
		                              (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			if (likeCollection.knownSize(suffix) == 0) toGeneric(elems)
			else toOps(elems).appendedAll(likeCollection.toIterableOnce(suffix))

		override def patch[A >: X, O](elems :Xs, from :Int, other :O, replaced :Int)
		                             (implicit likeCollection :LikeCollection[A, O]) :CC[A] =
			toOps(elems).patch(from, likeCollection.toIterableOnce(other), replaced)

		override def padTo[A >: X](elems :Xs, len :Int, elem :A) :CC[A] = toOps(elems).padTo(len, elem)

		override def view(elems :Xs) :SeqView[X] = toOps(elems).view
		override def reverseIterator(elems :Xs) :Iterator[X] = toOps(elems).reverseIterator
		override def toIterable(elems :Xs) :Iterable[X] = toImpureSeq(elems)
		override def toOps(elems :Xs) :collection.SeqOps[X, CC, C]
	}


	trait ForOps[X, CC[A] <: Iterable[A], C <: CC[X] with collection.SeqOps[X, CC, C]]
		extends FromOps[X, C, CC, C] with LikeIterable.ForOps[X, CC, C]
	{
		override def isApplyPreferred(elems :C) :Boolean = ApplyPreferred(elems)
		override def apply(elems :C, i :Int) :X = elems(i)

		override def toOps(elems :C) :collection.SeqOps[X, CC, C] = elems
		override def toImpureSeq(elems :C) :collection.Seq[X] = elems match {
			case seq :collection.Seq[X] => seq
			case _                      => LikeSeq.adapt(elems)(this)
		}
	}
}




private class LikeSeqAdapterView[+X, Xs](elems :Xs, likeSeq :LikeSeq[X, Xs, Any1, _]) extends SeqView[X] {
	override def apply(i :Int) :X = likeSeq.apply(elems, i)
	override def length :Int = likeSeq.size(elems)
	override def iterator :Iterator[X] = likeSeq.iterator(elems)
}




private object LikeSeqIndexedStepper {
	def apply[X, Xs, S <: Stepper[_]](elems :Xs)(implicit ops :LikeSeq[X, Xs, Any1, Any], shape :StepperShape[X, S]) :S = (
		shape.shape match {
			case StepperShape.IntShape    =>
				new LikeSeqIntIndexedStepper(ops.asInstanceOf[LikeSeq[Int, Xs, Any1, _]], elems)
			case StepperShape.LongShape   =>
				new LikeSeqLongIndexedStepper(ops.asInstanceOf[LikeSeq[Long, Xs, Any1, _]], elems)
			case StepperShape.DoubleShape =>
				new LikeSeqDoubleIndexedStepper(ops.asInstanceOf[LikeSeq[Double, Xs, Any1, _]], elems)
			case _                        =>
				shape.seqUnbox(new LikeSeqAnyIndexedStepper(ops, elems))
		}
	).asInstanceOf[S]

	private class LikeSeqIntIndexedStepper[C](ops :LikeSeq[Int, C, Any1, _], elems :C)
		extends LikeSeqIndexedStepper[Int, JInt, C, LikeSeqIntIndexedStepper[C]](ops, elems)
		   with BoxedAllInOneIntStepper with Spliterator.OfInt with JavaIntIterator

	private class LikeSeqLongIndexedStepper[C](ops :LikeSeq[Long, C, Any1, _], elems :C)
		extends LikeSeqIndexedStepper[Long, JLong, C, LikeSeqLongIndexedStepper[C]](ops, elems)
		   with BoxedAllInOneLongStepper with Spliterator.OfLong with JavaLongIterator

	private class LikeSeqDoubleIndexedStepper[C](ops :LikeSeq[Double, C, Any1, _], elems :C)
		extends LikeSeqIndexedStepper[Double, JDouble, C, LikeSeqDoubleIndexedStepper[C]](ops, elems)
		   with BoxedAllInOneDoubleStepper with Spliterator.OfDouble with JavaDoubleIterator

	private class LikeSeqAnyIndexedStepper[X, C](ops :LikeSeq[X, C, Any1, _], elems :C)
		extends LikeSeqIndexedStepper[X, X, C, LikeSeqAnyIndexedStepper[X, C]](ops, elems)
		   with AllInOneAnyStepper[X]
}

private abstract class LikeSeqIndexedStepper[+A, B, C, +Self >: Null <: IndexedStepper[A, B, Self]]
                                            (ops :LikeSeq[A, C, Any1, _], elems :C)
	extends AbstractIndexedStepper[A, B, Self](0, ops.size(elems))
{
	override def underlyingSize :Int = ops.size(elems)
	override def nextStep() = ops(elems, nextIdx())
	override def iterator :Iterator[A] = ops.iterator(elems).dropInPlace(index).take(limit - index)
}




private trait LikeSeqAdapter[+X, Xs, +CC[_], +C] //SeqOps last, because size is final
	extends LikeIterableAdapter[X, Xs, CC, C] with collection.SeqOps[X, CC, C]
{
//	protected override val ops :LikeSeq[X, Xs, CC, C]
	protected override val ops :LikeSeq[X, Xs, Any1, _]

	override def length :Int = ops.size(elems)
	override def apply(i :Int) :X = ops.apply(elems, i)

	override def findLast(p :X => Boolean) :Option[X] = ops.findLast(elems)(p)

	override def segmentLength(p :X => Boolean, from :Int) :Int = ops.segmentLength(elems, from)(p)
	override def indexWhere(p :X => Boolean, from :Int) :Int = ops.indexWhere(elems, from)(p)
	override def indexOf[U >: X](elem: U, from :Int): Int = ops.indexOf(elems, from, elem)
	override def lastIndexWhere(p :X => Boolean, end :Int): Int = ops.lastIndexWhere(elems, end)(p)
	override def lastIndexOf[U >: X](elem :U, end :Int): Int = ops.lastIndexOf(elems, end, elem)
	override def indexOfSlice[U >: X](that :collection.Seq[U], from :Int) :Int = ops.indexOfSlice(elems, from, that)
	override def lastIndexOfSlice[U >: X](that :collection.Seq[U], end :Int) :Int = ops.lastIndexOfSlice(elems, end, that)
	override def contains[U >: X](elem :U) :Boolean = ops.contains(elems, elem)
	override def containsSlice[U >: X](that :collection.Seq[U]) :Boolean = ops.containsSlice(elems, that)

	override def sameElements[U >: X](that :IterableOnce[U]) :Boolean = ops.sameElements[U, IterableOnce[U]](elems, that)
	override def endsWith[U >: X](that :Iterable[U]) :Boolean = ops.endsWith[U, Iterable[U]](elems, that)
	override def startsWith[U >: X](that :IterableOnce[U], offset :Int) :Boolean =
		ops.startsWith[U, IterableOnce[U]](elems, offset, that)

	override def search[U >: X](elem :U, from :Int, until :Int)(implicit ord :Ordering[U]) :SearchResult =
		ops.search[U](elems, elem, from, until)

/*
	override def distinct :C = ops.distinct(elems)
	override def distinctBy[A](f :X => A) :C = ops.distinctBy(elems)(f)
	override def reverse :C = ops.reverse(elems)
	override def sorted[A >: X](implicit ord :Ordering[A]) :C = ops.sorted[A](elems)
	override def sortWith(lt :(X, X) => Boolean) :C = ops.sortWith(elems)(lt)
	override def sortBy[A](f :X => A)(implicit ord :Ordering[A]) :C = ops.sortBy(elems)(f)


	override def updated[U >: X](index: Int, elem: U): CC[U] = ops.updated(elems)(index, elem)
	override def prepended[U >: X](elem :U) :CC[U] = ops.prepended(elems)(elem)
	override def appended[U >: X](elem :U) :CC[U] = ops.appended(elems)(elem)
	override def prependedAll[U >: X](prefix :IterableOnce[U]) :CC[U] = ops.prependedAll[U, IterableOnce[U]](elems)(prefix)
	override def appendedAll[U >: X](suffix :IterableOnce[U]) :CC[U] = ops.appendedAll[U, IterableOnce[U]](elems)(suffix)
	override def patch[U >: X](from :Int, other :IterableOnce[U], replaced :Int) :CC[U] =
		ops.patch[U, IterableOnce[U]](elems)(from, other, replaced)

	override def padTo[U >: X](len :Int, elem :U) :CC[U] = ops.padTo[U](elems)(len, elem)

	override def permutations :Iterator[C] = ops.permutations(elems)
	override def combinations(n :Int) :Iterator[C] = ops.combinations(elems)(n)
*/

	override def reverseIterator :Iterator[X] = ops.reverseIterator(elems)
}






/** @define coll mutable sequence
  */
trait LikeMutableSeq[X, -Xs, +CC[_], +C] extends LikeSeq[X, Xs, CC, C] {
	/** Replaces element at given index with a new value.
	  *
	  * @param idx  the index of the element to replace.
	  * @param elem the new value.
	  * @throws IndexOutOfBoundsException if the index is not valid.
	  */
	@throws[IndexOutOfBoundsException]
	def update(elems :Xs, idx :Int, elem :X) :Unit

	//cannot return mutable.SeqOps without C <: AnyRef
//	override def toOps(elems :Xs) :mutable.SeqOps[X, CC, C]
	override def moreSpecific(elems :Xs) :Maybe[LikeMutableSeq[X, elems.type, CC, C]] = No
	override def specific(elems :Xs) :LikeMutableSeq[X, elems.type, CC, C] = moreSpecific(elems) getOrElse this
}




/** @define TypeClass `LikeMutableSeq` */
private[collections] sealed abstract class Rank2LikeMutableSeqs extends LikeIterableOnceSummons[LikeMutableSeq] {
	@inline implicit final def likeGeneric[X, Xs <: CC[X], CC[_], C >: CC[X]]
	                                      (implicit generic :Generic[CC]) :LikeMutableSeq[X, Xs, CC, C] =
		generic.of
}


private[collections] sealed abstract class Rank1LikeMutableSeqs extends Rank2LikeMutableSeqs {
	implicit final def forOps[X, Xs, CC[_], C <: AnyRef]
	                         (implicit arg :Xs <:< C, specific :C <:< CC[X] with mutable.SeqOps[X, CC, C],
	                                   generic :CC <::< Iterable) :LikeMutableSeq[X, Xs, CC, C] =
		prototype.asInstanceOf[LikeMutableSeq[X, Xs, CC, C]]

	private object priv {
		type Ops[X] <: Iterable[X] with mutable.SeqOps[X, Ops, Ops[X]]
		type IndexedOps[X] >: Iterable[X] with mutable.IndexedSeqOps[X, IndexedOps, IndexedOps[X]]
			<: Iterable[X] with mutable.IndexedSeqOps[X, IndexedOps, IndexedOps[X]]
	}
	import priv.{Ops, IndexedOps}
	private[this] val prototype = new LikeMutableSeq.ForOps[Any, Ops, Ops[Any]] {
		override def moreSpecific(elems :Ops[Any]) = elems match {
			case indexed :mutable.IndexedSeqOps[Any, IndexedOps, IndexedOps[Any]] @unchecked =>
				Yes(LikeMutableIndexedSeq.forOps[Any, IndexedOps[Any], IndexedOps, IndexedOps[Any]].specific(indexed)
				                         .asInstanceOf[LikeMutableSeq[Any, elems.type, Ops, Ops[Any]]])
			case _ => No
		}
		private def readResolve :AnyRef = LikeMutableSeq.forOps[Any, mutable.Seq[Any], Iterable, mutable.Seq[Any]]
		override def toString :String = "LikeMutableSeq.forOps"
	}
}


@SerialVersionUID(Ver)
object LikeMutableSeq extends Rank1LikeMutableSeqs {

	@inline implicit def likeMutableIndexedSeq[X, Xs, CC[_], C](implicit like :LikeMutableIndexedSeq[X, Xs, CC, C])
			:LikeIndexedSeq[X, Xs, CC, C] =
		like


	def adapt[X, Xs](elems :Xs)(implicit likeSeq :LikeMutableSeq[X, Xs, Any1, Any]) :mutable.Seq[X] =
		new LikeCollectionAdapter[X, elems.type](elems)
			with LikeIterableAdapter[X, elems.type, mutable.Seq, mutable.Seq[X]] with mutable.Seq[X]
			with LikeSeqAdapter[X, elems.type, mutable.Seq, mutable.Seq[X]]
		{
			override val ops = likeSeq.specific(elems)
			override def update(idx :Int, elem :X) :Unit = ops.update(elems, idx, elem)
		}


	trait Generic[CC[_]] extends LikeSeq.Generic[CC] with Template[CC, LikeMutableSeq]

	@SerialVersionUID(Ver)
	object Generic extends Rank1Generics {
		@inline implicit def likeMutableIndexedSeq[CC[_]](implicit generic :LikeMutableIndexedSeq.Generic[CC])
				:Generic[CC] =
			generic
	}
	private[LikeMutableSeq] sealed abstract class Rank1Generics {
		implicit def forOps[CC[X] <: Iterable[X] with mutable.SeqOps[X, CC, CC[X]]] :Generic[CC] =
			prototype.asInstanceOf[Generic[CC]]

		private[this] val prototype =
			new SerializableSingleton("LikeMutableSeq.Generic.forOps", Generic.forOps[mutable.Seq])
				with Generic[mutable.Seq]
			{
				implicit override def of[X] :LikeMutableSeq[X, mutable.Seq[X], mutable.Seq, mutable.Seq[X]] =
					LikeMutableSeq.forOps
			}
	}


	trait FromOps[X, -Xs, +CC[_], +C <: AnyRef] extends LikeMutableSeq[X, Xs, CC, C] with LikeSeq.FromOps[X, Xs, CC, C] {
		override def update(elems :Xs, idx :Int, elem :X) :Unit = toOps(elems).update(idx, elem)
		override def toOps(elems :Xs) :mutable.SeqOps[X, CC, C]
	}

	trait ForOps[X, CC[A] <: Iterable[A], C <: CC[X] with mutable.SeqOps[X, CC, C]]
		extends FromOps[X, C, CC, C] with LikeSeq.ForOps[X, CC, C]
	{
		override def update(elems :C, idx :Int, elem :X) :Unit = elems.update(idx, elem)
		override def toOps(elems :C) :mutable.SeqOps[X, CC, C] = elems
	}

	trait FromIterator[X, -Xs, +CC[_], +C <: AnyRef]
		extends LikeSeq.FromIterator[X, Xs, CC, C] with LikeMutableSeq[X, Xs, CC, C]
}
