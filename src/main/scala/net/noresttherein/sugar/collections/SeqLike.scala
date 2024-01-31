package net.noresttherein.sugar.collections

import scala.collection.{AbstractIterator, IndexedSeqView, IterableFactory, IterableOnceOps, IterableOps, LazyZip2, SeqView, Stepper, StepperShape, View, mutable}
import scala.collection.Searching.{Found, InsertionPoint, SearchResult}
import scala.collection.immutable.{IndexedSeqOps, StringView, WrappedString}
import scala.collection.mutable.Builder
import scala.reflect.ClassTag

import net.noresttherein.sugar.JavaTypes.JStringBuilder
import net.noresttherein.sugar.arrays.{ArrayCompanionExtension, ArrayExtension, ArrayFactory, ArrayLike, ArrayLikeExtension, IArray, IArrayExtension, IArrayLike, IArrayLikeExtension, IRefArray, IRefArrayExtension, MutableArrayExtension, RefArray, RefArrayExtension, RefArrayLike, RefArrayLikeExtension}
import net.noresttherein.sugar.casting.castTypeParamMethods
import net.noresttherein.sugar.collections.extensions.{IterableExtension, IterableOnceExtension, IteratorExtension, JavaStringBuilderExtension}
import net.noresttherein.sugar.exceptions.{noSuch_!, unsupported_!}
import net.noresttherein.sugar.funny
import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.typist.{<:?<, Unknown}
import net.noresttherein.sugar.reflect.extensions.ClassExtension




/**
  * @define Coll `Seq`
  * @define coll sequence
  */ //consider: rplacing it with something like OrderedLike or PositionedLike or RankedLike
trait SeqLike[+E, +CC[_], C] extends IterableLike[E, CC, C] {

	override def view(elems :C) :SeqView[E] = new SeqView.Id[E](toOps(elems))

	/** Get the element at the specified index. This operation is provided for convenience in `Seq`. It should
	  * not be assumed to be efficient unless you have an `IndexedSeq`.
	  * @param elems a $coll.
	  */
	@throws[IndexOutOfBoundsException]
	def apply(elems :C)(i :Int) :E

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
	  * @tparam A    the element type of the returned $coll.
	  * @param elems a $coll.
	  * @param elem  the prepended element
	  * @return a new $coll consisting of `value` followed by all elements of `elems`.
	  */
	def prepended[A >: E](elems :C)(elem :A) :CC[A] = toOps(elems).prepended(elem)

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
	  * @tparam A    the element type of the returned $coll.
	  * @param elems a $coll.
	  * @param elem  the appended element
	  * @return a new $coll consisting of all elements of `elems` followed by `value`.
	  */
	def appended[A >: E](elems :C)(elem :A) :CC[A] = toOps(elems).appended(elem)

	/** As with `appendedAll`, returns a new collection containing the elements from the left operand
	  * followed by the elements from the right operand. It differs from `appendedAll` in that the right operand
	  * determines the type of the resulting collection rather than the left one.
	  * Mnemonic: the COLon is on the side of the new COLlection type.
	  *
	  * @tparam U     the element type of the returned collection.
	  * @param elems  a $coll.
	  * @param prefix the collection to prepend.
	  * @return       a new $coll which contains all elements of `prefix` followed by all the elements of `elems`.
	  */
	def prependedAll[U >: E, O](elems :C)(prefix :O)
	                           (implicit iterableOnceLike :IterableOnceLike[U, generic.Any, O]) :CC[U] =
		toOps(elems).prependedAll(iterableOnceLike.toIterableOnce(prefix))

	/** Returns a new $coll containing the elements from the left hand operand followed by the elem ts from the
	  * right hand operand. The element type of `elems` is the most specific superclass encompassing
	  * the element types of the two o rands.
	  *
	  * @tparam U     the element type of the returned collection.
	  * @param elems  a $coll.
	  * @param suffix the collection to append.
	  * @return       a new collection of type `CC[B]` which contains all elements
	  *               of `elems` followed by all elements of `suffix`.
	  */
	@inline final def appendedAll[U >: E, O](elems :C)(suffix :O)
	                                        (implicit iterableOnceLike :IterableOnceLike[U, generic.Any, O]) :CC[U] =
		concat[U, O](elems)(suffix)

	override def concat[U >: E, O](elems :C)(suffix :O)
	                              (implicit iterableOnceLike :IterableOnceLike[U, generic.Any, O]) :CC[U] =
		toOps(elems).appendedAll(iterableOnceLike.toIterableOnce(suffix))

	/** Selects all the elements of `elems` ignoring the duplicates.
	  * @param elems a $coll.
	  * @return a new $coll consisting of all the elements of `elems` without duplicates.
	  */
	def distinct(elems :C) :C = distinctBy(elems)(identity)

	/** Selects all the elements of `elems` ignoring the duplicates as determined by `==` after applying
	  * the transforming function `f`.
	  * @tparam A    the type of the elements after being transformed by `f`
	  * @param elems a $coll.
	  * @param f     The transforming function whose result is used to determine the uniqueness of each element
	  * @return a new $coll consisting of all the elements of `elems` without duplicates.
	  */
	def distinctBy[A](elems :C)(f :E => A) :C = toOps(elems).distinctBy(f)

	/** Returns new $coll with elements in reversed o er.
	  *
	  * $willNotTerminateInf
	  * $willForceEvaluation
	  * @return A new $coll with all elements of `elems` in reversed order.
	  */
	def reverse(elems :C) :C = fromSpecific(elems)(reversed(elems))

	/** An iterator yielding elements in reversed order.
	  *
	  * $willNotTerminateInf
	  *
	  * Note: `xs.reverseIterator` is the same as `xs.reverse.iterator` but might be more efficient.
	  * @param elems a $coll.
	  * @return an iterator yielding the elements of `elems` in reversed order
	  */
	override def reverseIterator(elems :C) :Iterator[E] = reversed(elems).iterator

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
	def startsWith[U >: E, O](elems :C)(that :O, offset :Int = 0)
	                         (implicit iterableOnceLike :IterableOnceLike[U, generic.Any, O]) :Boolean =
		iterableOnceLike.isEmpty(that) || {
			val i = iterator(elems) drop offset
			val j = iterableOnceLike.iterator(that)
			while (j.hasNext && i.hasNext)
				if (i.next() != j.next())
					return false
			!j.hasNext
		}

	/** Tests whether `elems` ends with the given sequence.
	  *
	  * $willNotTerminateInf
	  * @param elems a $coll.
	  * @param that  the sequence to test
	  * @return `true` if `elems` has `that` as a suffix, `false` otherwise.
	  */
	def endsWith[U >: E, O](elems :C)(that :O)(implicit iterableLike :IterableLike[U, generic.Any, O]) :Boolean = {
		iterableLike.isEmpty(that) || {
			val i = iterator(elems).drop(size(elems) - iterableLike.size(that))
			val j = iterableLike.iterator(that)
			while (i.hasNext && j.hasNext)
				if (i.next() != j.next())
					return false
			!j.hasNext
		}
	}

	/** A copy of `elems` with an element value appended until a given target length is reached.
	  * @tparam A    the element type of the returned $coll.
	  * @param elems a $coll.
	  * @param len   the target length
	  * @param elem  the padding value
	  * @return a new $coll consisting of all elements of `elems` followed by the minimal number of occurrences
	  *         of `elem`, so that the resulting collection has a length of at least `len`.
	  */
	def padTo[A >: E](elems :C)(len :Int, elem :A) :CC[A] = toOps(elems).padTo(len, elem)

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
	def segmentLength(elems :C)(p :E => Boolean, from :Int = 0) :Int =
		if (knownSize(elems) == 0)
			0
		else {
			var i = 0
			val it = iterator(elems).drop(from)
			while (it.hasNext && p(it.next()))
				i += 1
			i
		}

	/** Finds index of the first element satisfying some predicate after o at some start index.
	  *
	  * $mayNotTerminateInf
	  * @param elems a $coll.
	  * @param p     the predicate to use too test elements.
	  * @param from  the start index.
	  * @return the index `>= from` of the first element of `elems` that satisfies the predicate `p`,
	  *         or `-1`, if none exists.
	  */
	def indexWhere(elems :C)(p :E => Boolean, from :Int = 0) :Int = iterator(elems).indexWhere(p, from)

	/** Finds index of first occurrence of some value in `elems` after or t some art  ex.
	  * @tparam A    the t e of t elem  `elem`.
	  * @param elems a $coll.
	  * @param elem  the el ent val   rch for.
	  * @param from  the start index.
	  * @return the index `>= from` of the first element of `elems` that is equal
	  *         (as determined by `==`) to `elem`, or `-1`, if none exists.
	  */
	def indexOf[A >: E](elems :C)(elem: A, from :Int = 0): Int = indexWhere(elems)(elem == _, from)

	/** Finds index of last occurrence of some value in `elems` before or at  given end index.
	  *
	  * $WillNotTerminateInf
	  * @tparam A    the type of the element `elem`.
	  * @param elems a $coll.
	  * @param elem  the element value to search for.
	  * @param end   the end index.
	  * @return the index `<= end` of the last element of `elems` that is equal
	  *         (as determined by `==`) to `elem`, or `-1`, if none exists.
	  */
	def lastIndexOf[A >: E](elems :C)(elem :A, end :Int = size(elems) - 1): Int =
		lastIndexWhere(elems)(elem == _, end)

	/** Finds index of last element satisfying some predicate before or at iven end index.
	  *
	  * $willNotTerminateInf
	  *
	  * @param elems a $coll.
	  * @param p     the predicate used to test elements.
	  * @return  the index `<= end` of the last element of `elems` that satisfies the predicate `p`,
	  *          or `-1`, if none exists.
	  */
	def lastIndexWhere(elems :C)(p :E => Boolean, end :Int): Int = {
		var i  = size(elems) - 1
		val it = reverseIterator(elems)
		while (it.hasNext && { val elem = it.next(); i > end || !p(elem) })
			i -= 1
		i
	}

	/** Finds first index after or at a start index where `elems` contains a gi n sequence as a slice.
	  *
	  * $mayNotTerminateInf
	  * @param elems a $coll.
	  * @param that  the sequence to test.
	  * @param from  the start index.
	  * @return  the first index `>= from` such that the elements of `elems` starting at this index
	  *          match the elements of sequence `that`, or `-1` if no such subsequence exists.
	  */ //consider: using SeqLike type class instead. The problem is that it does not currently have to(collection.Seq)
	def indexOfSlice[A >: E](elems :C)(that :collection.Seq[A], from :Int = 0) :Int =
		toOps(elems).indexOfSlice(that, from)

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
	def lastIndexOfSlice[A >: E](elems :C)(that :collection.Seq[A], end :Int = Int.MaxValue) :Int =
		toOps(elems).lastIndexOfSlice(that, end)

	/** Finds the last element of the $coll satisfying a predicate, f any.
	  *
	  * $willNotTerminateInf
	  * @param elems a $coll.
	  * @param p     the predicate used t test elements.
	  * @return an option value containing the last element in `elems` that satisfies `p`, or `None` if none exists.
	  */
	def findLast(elems :C)(p :E => Boolean) :Option[E] = reverseIterator(elems).find(p)

	/** Tests whether `elems` contains a given sequence as a slice.
	  * $mayNotTerminateInf
	  * @param elems a $coll.
	  * @param that  the sequence to test
	  * @return `true` if `elems` contains a slice with the same elements as `that`, otherwise `false`.
	  */
	@inline final def containsSlice[A >: E](elems :C)(that :Seq[A]) :Boolean = indexOfSlice[A](elems)(that) != -1

	/** Tests whether `elems` contains a given value as an element.
	  *
	  * $mayNotTerminateInf
	  * @param elems a $coll.
	  * @param elem  the el ent to test.
	  * @return `true` if `elems` has an element that is equal (as determined by `==`) to `elem`, `false` otherwise.
	  */
	@inline final def contains[A >: E](elems :C)(elem :A) :Boolean = indexOf[A](elems)(elem) >= 0

	/** Iterates over distinct permutations of elements.
	  *
	  * $willForceEvaluation
	  *
	  * @param elems a $coll.
	  * @return      An Iterator which traverses the distinct permutations f `elems`.
	  * @example {{{
	  *    Seq('a', 'b', 'b').permutations.foreach(println)
	  *    // List(a, b, b)
	  *    // List(b, a, b)
	  *    / List(b, b, a)
	  * }}} */ //a candidate for removal
	def permutations(elems :C) :Iterator[C] =
		if (isEmpty(elems)) Iterator.single(elems) else toOps(elems).permutations

	/** Iterates over combinations of elements.
	  *
	  * A '''combination''' of length `n` is a sequence of `n` elements selected in order of their first index in this sequence.
	  *
	  * For example, `"xyx"` has two combinations of length 2. The `x` is selected first: `"xx"` , `"xy"`.
	  * The sequence `"yx"` is not returned as a combination because it is subsumed by ` "xy"`.
	  *
	  * If there is more than one way to generate the same combination, only one will be returned.
	  *
	  * For example, the result `"xy"` arbitrarily selected one of the `x`  elements.
	  *
	  * As a further illustration, `"xyxx"` has three different ways to generate `"xy"`, because there are three
	  * elements `x` to choose from. Moreover, there are three unordered pairs `"xx"` but only one is returned.
	  *
	  * It is not specified which of these equal combinations is returned. It is an implementation detail
	  * that should not be relied on. For example, the combination `"xx"` does not necessarily contain
	  * the first `x` in this sequence. This behavior is observable if the elements compare equal but are no identical.
	  *
	  * As a consequence, `"xyx".combinations(3).next()` is `"xxy"`: the combination does not reflect the order
	  * of the original sequence, but the order in which elements were selected, "first index";
	  * the order of each `x` element is als arbitrary.
	  *
	  * $willForceEvaluation
	  *
	  * @param elems a $coll.
	  * @return      An Iterator which traverses the n-element combinations  of `elems`.
	  * @example {{{
	  *    Seq('a', 'b', 'b', 'b', 'c').combinations(2).foreach(println)
	  *    // List(a, b)
	  *    // List(a, c)
	  *    // List(b, b)
	  *    // List(b, c)
	  *    Seq('b', 'a', 'b').combinations(2).foreach(println)
	  *    // List(b, b)
	  *     // List(b, a)
	  *  }}}
	  */ //a candidate for removal
	def combinations(elems :C)(n :Int) :Iterator[C] =
		if (n < 0 || n > size(elems)) Iterator.empty else toOps(elems).combinations(n)

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
	def sorted[A >: E](elems :C)(implicit ord :Ordering[A]) :C =
		if (isEmpty(elems))
			elems
		else {
			val b = newSpecificBuilder(elems)
			val len = size(elems)
			if (len == 1)
				b += head(elems)
			else if (len > 1) {
				val array = toRefArray[E](elems)
				java.util.Arrays.sort(array.asAnyRefArray, ord.castParam[AnyRef])
				b.sizeHint(len)
				b ++= array.toSeq
			}
			b.result()
		}

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
	def sortWith(elems :C)(lt :(E, E) => Boolean) :C = sorted(elems)(Ordering.fromLessThan(lt))

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
	def sortBy[A](elems :C)(f :E => A)(implicit ord :Ordering[A]) :C = sorted(elems)(ord on f)

	override def isEmpty(elems :C): Boolean = sizeCompare(elems, 0) == 0

	/** Are the elements of this collection the same (and in the same order) as those of `that`?
	  * @param elems a $coll.
	  */
	def sameElements[U >: E, O](elems :C)(that :O)
	                           (implicit iterableOnceLike :IterableOnceLike[U, generic.Any, O]) :Boolean =
	{
		val thisKnownSize = knownSize(elems)
		val knownSizeDifference = thisKnownSize != -1 && {
			val thatKnownSize = iterableOnceLike.knownSize(that)
			thatKnownSize != -1 && thisKnownSize != thatKnownSize
		}
		!knownSizeDifference && iterator(elems).sameElements(iterableOnceLike.toIterableOnce(that))
	}

	/** Tests whether every element of `elems` relates to the corresponding element of another sequence
	  * by satisfying a test predicate.
	  * @tparam A    the type of th element of `that`.
	  * @param elems a $coll.
	  * @param that  the other sequence.
	  * @param p     the test predicate, which relates elements from both sequences.
	  * @return `true` if both sequences have the same length and `p(x, y)` is `true` for all corresponding elements
	  *         `x` of `elems` and `y` of `that`, otherwise `false`.
	  */ //consider: using a type class instead. We'd need SeqLike.toNonImmutableSeq or something
	def corresponds[A](elems :C)(that :collection.Seq[A])(p :(E, A) => Boolean) :Boolean = {
		val thisSize = knownSize(elems)
		val thatSize = that.knownSize
		(thisSize == -1 || thatSize == -1 || thisSize == thatSize) && iterator(elems).corresponds(that.iterator)(p)
	}

	/** Produces a new $coll where a slice of elements in `elems` is replaced by another sequence.
	  * Patching at negative indices is the same as patching starting at 0.
	  * Patching at indices at or larger than the length of the original $coll appends the patch to the end.
	  * If more values are replaced than actually exist, the excess is ignored.
	  * @tparam A       the element type of the returned $coll.
	  * @param elems    a $coll.
	  * @param from     the index of the first replaced element.
	  * @param other    the replacement sequence.
	  * @param replaced the number of elements to drop in the original $coll.
	  * @return a new $coll consisting of all elements of `elems` except that `replaced` elements,
	  *         starting from `from`, are replaced by all the elements of `other`.
	  */ //todo: use a type class for other
	def patch[A >: E](elems :C)(from :Int, other :IterableOnce[A], replaced :Int) :CC[A] =
		toOps(elems).patch(from, other, replaced)

	/** A cop of `elems` with one single rep ced element.
	  * @tparam A    the element type of the returned $coll.
	  * @param elems a $coll.
	  * @param index the position of the replace nt
	  * @param elem  the replacing element
	  * @return a new $coll which is a copy of `elems` with the element at position `index` replaced by `elem`.
	  * @throws IndexOutOfBoundsException if `index` does not satisfy `0 <= index < length`.
	  *                                   In case of a lazy collection this exception may be thrown at a later time
	  *                                   or not at all (if the end of the collection is never evaluated).
	  */
	def updated[A >: E](elems :C)(index: Int, elem: A): CC[A] = toOps(elems).updated(index, elem)

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
	  * @param to    the index following where the search ends.
	  * @param ord   the ordering to be used to compare elements.
	  *
	  * @return a `Found` value containing the index corresponding to the element in the sequence,
	  *         or the `InsertionPoint` where the element would be inserted if the element is not in the sequence.
	  * @note if  `to <= from`, the search space is empty, and an `InsertionPoint` at `from` is returned.
	  */
	def search[A >: E](elems :C)(elem :A, from :Int = 0, to :Int = Int.MaxValue)(implicit ord :Ordering[A]) :SearchResult =
		toOps(elems).search(elem, from, to)

	override def toOps(elems :C) :collection.SeqOps[E, CC, C]
}






private[collections] sealed abstract class Rank2SeqLike extends IterableOnceLikeSummons[SeqLike] {
	implicit def forOps[E, CC[A], C/* <: CC[E] with collection.SeqOps[E, CC, C]*/] //:SeqLike[E, CC, C] =
	                   (implicit specific :C <:< CC[E] with collection.SeqOps[E, CC, C], generic :CC <:?< Iterable)
			:SeqLike[E, CC, C] =
		prototype.asInstanceOf[SeqLike[E, CC, C]]

	private[this] val prototype = new SeqLike.ForOps[Any, Iterable, collection.Seq[Any]] {
		private def readResolve = SeqLike.forOps[Any, Iterable, collection.Seq[Any]]
		override def toString :String = "SeqLike.forOps"
	}
}

private[collections] sealed abstract class Rank1SeqLike extends Rank2SeqLike {
	@inline implicit def fromMutableSeqLike[E, CC[_], C]
	                                       (implicit like :MutableSeqLike[E, CC, C]) :SeqLike[E, CC, C] =
		like
	@inline implicit def fromIndexedSeqLike[E, CC[_], C]
	                                       (implicit like :IndexedSeqLike[E, CC, C]) :SeqLike[E, CC, C] =
		like
}


@SerialVersionUID(Ver)
object SeqLike extends Rank1SeqLike {

	@inline implicit def fromMutableIndexedSeqLike[E, CC[_], C]
	                                              (implicit like :MutableIndexedSeqLike[E, CC, C]) :SeqLike[E, CC, C] =
		like

	trait ForOps[E, CC[A] <: Iterable[A], C <: CC[E] with collection.SeqOps[E, CC, C]]
		extends SeqLike[E, CC, C] with IterableLike.ForOps[E, CC, C]
	{
		override def view(elems :C) :SeqView[E] = elems.view

		override def apply(elems :C)(i :Int) :E = elems(i)

		override def distinct(elems :C) :C = elems.distinct

		override def reverse(elems :C): C = elems.reverse
		override def reverseIterator(elems :C): Iterator[E] = elems.reverseIterator

		override def startsWith[U >: E, O](elems :C)(that :O, offset :Int = 0)
		                                  (implicit iterableOnceLike :IterableOnceLike[U, funny.generic.Any, O])
				:Boolean =
			elems.startsWith(iterableOnceLike.toIterableOnce(that), offset)

		override def endsWith[U >: E, O](elems :C)(that :O)
		                                (implicit iterableLike :IterableLike[U, funny.generic.Any, O]) :Boolean =
			elems.endsWith(iterableLike.toIterable(that))

		override def segmentLength(elems :C)(p :E => Boolean, from :Int) :Int = elems.segmentLength(p, from)
		override def indexWhere(elems :C)(p :E => Boolean, from :Int) :Int = elems.indexWhere(p, from)
		override def indexOf[A >: E](elems :C)(elem: A, from :Int): Int = elems.indexOf(elem, from)
		override def lastIndexOf[A >: E](elems :C)(elem :A, end :Int): Int = elems.lastIndexOf(elem, end)
		override def lastIndexWhere(elems :C)(p :E => Boolean, end :Int): Int = elems.lastIndexWhere(p, end)
		override def findLast(elems :C)(p :E => Boolean) :Option[E] = elems.findLast(p)

		override def permutations(elems :C) :Iterator[C] = elems.permutations
		override def combinations(elems :C)(n :Int) :Iterator[C] = elems.combinations(n)

		override def sorted[A >: E](elems :C)(implicit ord :Ordering[A]) :C = elems.sorted[A]
		override def sortWith(elems :C)(lt :(E, E) => Boolean) :C = elems.sortWith(lt)
		override def sortBy[A](elems :C)(f :E => A)(implicit ord :Ordering[A]) :C = elems.sortBy(f)

		override def sizeCompare(elems :C, len: Int): Int = elems.lengthCompare(len)
		override def sizeCompare[O](elems :C, that :O)(implicit collection :IterableLike[_, funny.generic.Any, O]) :Int =
			elems.lengthCompare(collection.toIterable(that))

		override def isEmpty(elems :C): Boolean = elems.isEmpty
		
		override def sameElements[U >: E, O](elems :C)(that :O)
		                                    (implicit iterableOnceLike :IterableOnceLike[U, funny.generic.Any, O])
				:Boolean =
			elems.sameElements(iterableOnceLike.toIterableOnce(that))

		override def corresponds[A](elems :C)(that :collection.Seq[A])(p :(E, A) => Boolean) :Boolean =
			elems.corresponds(that)(p)
		
		override def toIterable(elems :C) :Iterable[E] = toSeq(elems)
		override def toOps(elems :C) :collection.SeqOps[E, CC, C] = elems
	}
}






/** @define coll mutable sequence
  */
trait MutableSeqLike[E, +CC[_], C] extends SeqLike[E, CC, C] {
	/** Replaces element at given index with a new value.
	  *
	  * @param idx  the index of the element to replace.
	  * @param elem the new value.
	  * @throws IndexOutOfBoundsException if the index is not valid.
	  */
	@throws[IndexOutOfBoundsException]
	def update(elems :C)(idx :Int, elem :E) :Unit

//	override def toOps(elems :C) :mutable.SeqOps[E, CC, C]
}


private[collections] sealed abstract class Rank1MutableSeqLike extends IterableOnceLikeSummons[MutableSeqLike] {
	implicit final def forOps[E, CC[A]/* <: Iterable[A]*/, C <: AnyRef /*CC[E] with mutable.SeqOps[E, CC, C]*/]
	                         (implicit specific :C <:< CC[E] with mutable.SeqOps[E, CC, C], generic :CC <:?< Iterable)
			:MutableSeqLike[E, CC, C] =
		prototype.asInstanceOf[MutableSeqLike[E, CC, C]]

	private[this] val prototype = new MutableSeqLike.ForOps[Any, Iterable, mutable.Seq[Any]] {
		private def readResolve = MutableSeqLike.forOps[Any, Iterable, mutable.Seq[Any]]
		override def toString :String = "MutableSeqLike.forOps"
	}
}

@SerialVersionUID(Ver)
object MutableSeqLike extends Rank1MutableSeqLike {

	@inline implicit def fromMutableIndexedSeqLike[E, CC[_], C](implicit like :MutableIndexedSeqLike[E, CC, C])
			:IndexedSeqLike[E, CC, C] =
		like

	trait ForOps[E, CC[A] <: Iterable[A], C <: CC[E] with mutable.SeqOps[E, CC, C]]
		extends MutableSeqLike[E, CC, C] with SeqLike.ForOps[E, CC, C]
	{
		override def update(elems :C)(idx :Int, elem :E) :Unit = elems.update(idx, elem)
		override def toOps(elems :C) :mutable.SeqOps[E, CC, C] = elems
	}
}


/**
  * @define coll indexed sequence
  */
trait IndexedSeqLike[+E, +CC[_], C] extends SeqLike[E, CC, C] {

	override def knownSize(elems :C) :Int = size(elems)

	override def sizeCompare(elems :C, len :Int) :Int = Integer.compare(size(elems), len)

	override def sizeCompare[O](elems :C, that :O)(implicit collection :IterableLike[_, generic.Any, O]) :Int = {
		val thatSize = collection.knownSize(that)
		if (thatSize >= 0)
			Integer.compare(size(elems), thatSize)
		else {
			val res = collection.toOps(that).sizeCompare(size(elems))
			if (res == Int.MinValue) 1 else -res
		}
	}
	override def isEmpty(elems :C) :Boolean = size(elems) == 0

	override def segmentLength(elems :C)(p :E => Boolean, from :Int) :Int = {
		var i = from; val len = size(elems)
		while (i < len && p(apply(elems)(i)))
			i += 1
		i - from
	}

	override def foldRight[A](elems :C)(z :A)(op :(E, A) => A) :A = toOps(elems).foldRight(z)(op)

	override def head(elems :C) :E =
		if (size(elems) == 0) noSuch_!(infoString(elems) + ".head")
		else apply(elems)(0)

	override def headOption(elems :C) :Option[E] = if (size(elems) == 0) None else Some(head(elems))

	override def last(elems :C) :E =
		if (size(elems) == 0) noSuch_!(infoString(elems) + ".head")
		else apply(elems)(size(elems) - 1)

	override def tail(elems :C) :C = {
		val length = size(elems)
		if (length == 0) unsupported_!(infoString(elems) + "().tail")
		else slice(elems)(1, length)
	}
	override def init(elems :C) :C = {
		val length = size(elems)
		if (length == 0) unsupported_!(infoString(elems) + "().init")
		else slice(elems)(0, length - 1)
	}
	override def drop(elems :C)(n :Int) :C = slice(elems)(n, size(elems))
	override def take(elems :C)(n :Int) :C = slice(elems)(0, n)
	override def dropRight(elems :C)(n :Int) :C = slice(elems)(0, size(elems) - n)
	override def takeRight(elems :C)(n :Int) :C = { val end = size(elems); slice(elems)(end - n, end) }
	override def takeWhile(elems :C)(p :E => Boolean) :C = take(elems)(segmentLength(elems)(p))
	override def dropWhile(elems :C)(p :E => Boolean) :C = drop(elems)(segmentLength(elems)(p))
	override def splitAt(elems :C)(n :Int) :(C, C) = (take(elems)(n), drop(elems)(n))
	override def span(elems :C)(p :E => Boolean) :(C, C) = {
		val i = segmentLength(elems)(p)
		(take(elems)(i), drop(elems)(i))
	}


	protected override def reversed(elems :C) :Iterable[E] = new IndexedSeqView.Reverse(toOps(elems))

	override def reverse(elems :C) :C = fromSpecific(elems)(reversed(elems))

	override def reverseIterator(elems :C) :Iterator[E] = toOps(elems).reverseIterator //view(elems).reverseIterator

	override def view(elems :C) :IndexedSeqView[E] = toOps(elems).view //new IndexedSeqView.Id(toOps(elems))

	override def toSeq(elems :C) :Seq[E] = toIndexedSeq(elems)
	override def toOps(elems :C) :collection.IndexedSeqOps[E, CC, C]
}




private[collections] sealed abstract class Rank1IndexedSeqLike extends IterableOnceLikeSummons[IndexedSeqLike] {
	implicit def forOps[E, CC[A]/* <: Iterable[A]*/, C/* <: CC[E] with collection.IndexedSeqOps[E, CC, C]*/]
	                   (implicit specific :C <:< CC[E] with collection.IndexedSeqOps[E, CC, C], generic :CC <:?< Iterable)
			:IndexedSeqLike[E, CC, C] =
		prototype.asInstanceOf[IndexedSeqLike[E, CC, C]]

	private[this] val prototype = new IndexedSeqLike.ForOps[Any, Iterable, collection.IndexedSeq[Any]] {
		private def readResolve = IndexedSeqLike.forOps[Any, Iterable, collection.IndexedSeq[Any]]
		override def toString = "IndexedSeqLike.forOps"
	}
}


@SerialVersionUID(Ver)
object IndexedSeqLike extends Rank1IndexedSeqLike {

	@inline implicit def fromMutableIndexedSeqLike[E, CC[_], C](implicit like :MutableIndexedSeqLike[E, CC, C])
			:IndexedSeqLike[E, CC, C] =
		like

	implicit val forString :IndexedSeqLike[Char, IndexedSeq, String] = new ForString

	implicit def forArrayLike[E] :IndexedSeqLike[E, ArrayLike, ArrayLike[E]] =
		arrayLikePrototype.asInstanceOf[IndexedSeqLike[E, ArrayLike, ArrayLike[E]]]

	implicit def forRefArrayLike[E] :IndexedSeqLike[E, RefArrayLike, RefArrayLike[E]] =
		arrayLikePrototype.asInstanceOf[IndexedSeqLike[E, RefArrayLike, RefArrayLike[E]]]

	implicit def forIArrayLike[E] :IndexedSeqLike[E, IArrayLike, IArrayLike[E]] =
		iArrayLikePrototype.asInstanceOf[IndexedSeqLike[E, IArrayLike, IArrayLike[E]]]

	implicit def forArray[E] :IndexedSeqLike[E, Array, Array[E]] =
		arrayPrototype.asInstanceOf[IndexedSeqLike[E, Array, Array[E]]]

	implicit def forIArray[E] :IndexedSeqLike[E, IRefArray, IArray[E]] =
		iArrayPrototype.asInstanceOf[IndexedSeqLike[E, IRefArray, IArray[E]]]

	implicit def forIRefArray[E] :IndexedSeqLike[E, IRefArray, IRefArray[E]] =
		iRefArrayPrototype.asInstanceOf[IndexedSeqLike[E, IRefArray, IRefArray[E]]]

	implicit def forRanking[E] :IndexedSeqLike[E, Ranking, Ranking[E]] =
		rankingPrototype.asInstanceOf[IndexedSeqLike[E, Ranking, Ranking[E]]]

	private[this] val arrayLikePrototype = new SpecificForArrayLike[Unknown, ArrayLike] {
		override def toOps(elems :ArrayLike[Unknown]) = elems.toOps
		override def toIterable(elems :ArrayLike[Unknown]) = ArrayLikeSlice.wrap(elems)
		override def toIndexedSeq(elems :ArrayLike[Unknown]) = IRefArraySlice.wrap(IRefArray.copyOf(elems))
		override def toString = "IndexedSeqLike.forArrayLike"
		private def readResolve = IndexedSeqLike.forArrayLike
	}
	private[this] val iArrayLikePrototype = new SpecificForArrayLike[Unknown, IArrayLike] {
		override def toOps(elems :IArrayLike[Unknown]) = elems.toOps
		override def toIndexedSeq(elems :IArrayLike[Unknown]) = IArrayLikeSlice.wrap(elems)
		override def toString = "IndexedSeqLike.forIArrayLike"
		private def readResolve = IndexedSeqLike.forIArrayLike
	}
	private[this] val arrayPrototype = new SpecificForArrayLike[Unknown, Array] {
		override def toOps(elems :Array[Unknown]) = elems.toOps
		override def toIterable(elems :Array[Unknown]) = ArraySlice.wrap(elems)
		override def toIndexedSeq(elems :Array[Unknown]) = IArray.Wrapped(elems.toIArray)
		override def toString = "IndexedSeqLike.forArray"
		private def readResolve = IndexedSeqLike.forArray
	}
	private[this] val iArrayPrototype = new SpecificForArrayLike[Unknown, IArray] {
		override def toOps(elems :IArray[Unknown]) = elems.toOps
		override def toIndexedSeq(elems :IArray[Unknown]) = IArray.Wrapped(elems)
		override def toString = "IndexedSeqLike.forIArray"
		private def readResolve = IndexedSeqLike.forIArray
	}
	private[this] val iRefArrayPrototype = new SpecificForArrayLike[Unknown, IRefArray] {
		override def patch[A >: Unknown](elems :IRefArray[Unknown])(from :Int, other :IterableOnce[A], replaced :Int) =
			RefArrayLikeExtension(elems).patch(from, other, replaced)

		override def toOps(elems :IRefArray[Unknown]) = elems.toOps
		override def toIndexedSeq(elems :IRefArray[Unknown]) = IRefArray.Wrapped(elems)
		override def toString = "IndexedSeqLike.forIRefArray"
		private def readResolve = IndexedSeqLike.forIRefArray
	}
	private[this] val rankingPrototype = new ForRanking[Any] {
		private def readResolve = IndexedSeqLike.forRanking
		override def toString = "IndexedSeqLike.forRanking"
	}


	trait ForOps[E, CC[A] <: Iterable[A], C <: CC[E] with collection.IndexedSeqOps[E, CC, C]]
		extends IndexedSeqLike[E, CC, C] with SeqLike.ForOps[E, CC, C]
	{
		override def knownSize(elems :C) :Int = elems.knownSize

		override def reverse(elems :C) :C = elems.reverse
		protected override def reversed(elems :C) :Iterable[E] = elems.reverse

		override def head(elems :C) :E = elems.head
		override def headOption(elems :C) :Option[E] = elems.headOption
		override def last(elems :C) :E = elems.last
		override def lastOption(elems :C) :Option[E] = elems.lastOption

		override def view(elems :C) :IndexedSeqView[E] = elems.view //new IndexedSeqView.Id(toOps(elems))

		override def toOps(elems :C) :collection.IndexedSeqOps[E, CC, C] = elems
	}


	private class ForRanking[E]
		extends IndexedSeqLike[E, Ranking, Ranking[E]] with IterableLike.ForOps[E, Ranking, Ranking[E]]
	{
		override def knownSize(elems :Ranking[E]) :Int = elems.knownSize
		override def size(elems :Ranking[E]) :Int = elems.size
		override def apply(elems :Ranking[E])(i :Int) :E = elems(i)

		override def indexOf[A >: E](elems :Ranking[E])(elem :A, from :Int) = elems.indexOf(elem) match {
			case n if n >= from => n
			case _ => -1
		}
		override def indexWhere(elems :Ranking[E])(p :E => Boolean, from :Int) = elems.indexWhere(p, from)
		override def lastIndexOf[A >: E](elems :Ranking[E])(elem :A, end :Int) = elems.indexOf(elem) match {
			case n if n <= end => n
			case _ => -1
		}
		override def lastIndexWhere(elems :Ranking[E])(p :E => Boolean, end :Int) = elems.lastIndexWhere(p, end)

		override def indexOfSlice[A >: E](elems :Ranking[E])(that :collection.Seq[A], from :Int) =
			elems.indexOfSlice(that, from)

		override def lastIndexOfSlice[A >: E](elems :Ranking[E])(that :collection.Seq[A], end :Int) =
			elems.indexOfSlice(that, end) match {
				case n if n <= end => n
				case _ => -1
			}
		override def findLast(elems :Ranking[E])(p :E => Boolean) = elems.findLast(p)

		override def search[A >: E](elems :Ranking[E])(elem :A, from :Int, to :Int)(implicit ord :Ordering[A]) =
			elems.indexOf(elem) match {
				case n if n >= from & n < to => Found(n)
				case n if n >= 0 => InsertionPoint(n)
				case _ => elems.toSeq.search(elem, from, to)
			}

		override def segmentLength(elems :Ranking[E])(p :E => Boolean, from :Int) = elems.segmentLength(p, from)
		override def endsWith[U >: E, O](elems :Ranking[E])(that :O)
		                                (implicit iterableLike :IterableLike[U, funny.generic.Any, O]) =
			elems.endsWith(iterableLike.toIterable(that))
		override def startsWith[U >: E, O](elems :Ranking[E])(that :O, offset :Int)
		                                  (implicit iterableOnceLike :IterableOnceLike[U, funny.generic.Any, O]) =
			elems.startsWith(iterableOnceLike.toIterableOnce(that), offset)

		override def prepended[A >: E](elems :Ranking[E])(elem :A) = elems.prepended(elem)
		override def appended[A >: E](elems :Ranking[E])(elem :A) = elems.appended(elem)
		override def prependedAll[A >: E, O](elems :Ranking[E])(prefix :O)
		                                    (implicit iterableOnceLike :IterableOnceLike[A, funny.generic.Any, O]) =
			elems.prependedAll(iterableOnceLike.toIterableOnce(prefix))
		override def concat[A >: E, O](elems :Ranking[E])(suffix :O)
		                              (implicit iterableOnceLike :IterableOnceLike[A, funny.generic.Any, O]) =
			elems.concat(iterableOnceLike.toIterableOnce(suffix))

		override def distinct(elems :Ranking[E]) = elems
		override def distinctBy[A](elems :Ranking[E])(f :E => A) = elems.distinctBy(f)

		override def padTo[A >: E](elems :Ranking[E])(len :Int, elem :A) =
			if (elems.length >= len || elems.contains(elem)) elems else elems :+ elem

		override def patch[A >: E](elems :Ranking[E])(from :Int, other :IterableOnce[A], replaced :Int) =
			elems.patch(from, other, replaced)

		override def updated[A >: E](elems :Ranking[E])(index :Int, elem :A) = elems.updated(index, elem)

		override def reverse(elems :Ranking[E]) :Ranking[E] = elems.reverse
		override def reverseIterator(elems :Ranking[E]) = elems.reverseIterator
		protected override def reversed(elems :Ranking[E]) :Iterable[E] = elems.reverse

		override def view(elems :Ranking[E]) :IndexedSeqView[E] = elems.view

		override def toOps(elems :Ranking[E]) :IndexedSeqOps[E, Ranking, Ranking[E]] = new RankingAsSeq(elems)
		override def toIndexedSeq(elems :Ranking[E]) :IndexedSeq[E] = elems.toIndexedSeq
		override def toSet[U >: E](elems :Ranking[E]) :Set[U] = elems.toSet
	}


	@SerialVersionUID(Ver)
	private final class ForString extends IndexedSeqLike[Char, IndexedSeq, String] {
		override def knownSize(elems :String) :Int = elems.length
		override def size(elems :String) :Int = elems.length
		override def apply(elems :String)(i :Int) :Char = elems.charAt(i)

		override def forall(elems :String)(p :Char => Boolean) :Boolean = elems.forall(p)
		override def exists(elems :String)(p :Char => Boolean) :Boolean = elems.exists(p)
		override def count(elems :String)(p :Char => Boolean) :Int = elems.count(p)
		override def find(elems :String)(p :Char => Boolean) :Option[Char] = elems.find(p)
		override def findLast(elems :String)(p :Char => Boolean) :Option[Char] = elems.findLast(p)
		override def indexOf[A >: Char](elems :String)(elem :A, from :Int) :Int = elems.indexOf(elem, from) 
		override def lastIndexOf[A >: Char](elems :String)(elem :A, end :Int) :Int = elems.lastIndexOf(elem, end)

		override def indexWhere(elems :String)(p :Char => Boolean, from :Int) :Int = elems.indexWhere(p, from)
		override def lastIndexWhere(elems :String)(p :Char => Boolean, end :Int) :Int = elems.lastIndexWhere(p, end)

		override def foldLeft[A](elems :String)(z :A)(op :(A, Char) => A) :A = elems.foldLeft(z)(op)
		override def foldRight[A](elems :String)(z :A)(op :(Char, A) => A) :A = elems.foldRight(z)(op)

		override def slice(elems :String)(from :Int, until :Int) :String = {
			val length = elems.length
			val from0 = math.min(math.max(0, from), length)
			val until0 = math.max(from, math.min(until, length))
			elems.substring(from0, until0)
		}

		protected override def fromSpecific(elems :String)(coll :IterableOnce[Char]) :String = coll match {
			case s :StringAsSeq   => s.coll
			case s :WrappedString => s.toString
			case KnownSize(size)  => (new JStringBuilder(size) ++= coll).result()
			case _                => (new JStringBuilder ++= coll).result()
		}
		protected override def newSpecificBuilder(elems :String) :Builder[Char, String] = new StringBuilder

		override def iterator(elems :String) :Iterator[Char] = StringIterator(elems)
		override def reverseIterator(elems :String) :Iterator[Char] = ReverseStringIterator(elems)
		override def stepper[S <: Stepper[_]](elems :String)(implicit shape :StepperShape[Char, S]) :S =
			new StringStepper(elems).asInstanceOf[S]

		override def copyToArray[A >: Char](elems :String)(array :Array[A], start :Int, max :Int) :Int =
			copyRangeToArray[A](elems)(array, start, 0, max)

		def copyRangeToArray[A >: Char](elems :String)(xs :Array[A], start :Int, from :Int, len :Int) :Int = {
			val copied = util.elementsToCopy(xs, start, len, from, elems.length)
			xs match {
				case chars :Array[Char] =>
					elems.getChars(start, start + copied, chars, start)
				case _ =>
					val shift = start - from
					if (copied > 16)
						xs.updateAll(start, start + copied) { i :Int => elems.charAt(i - shift) }
					else {
						val end = from + copied
						var i = from
						while (i < end) {
							xs(i + shift) = elems.charAt(i)
							i += 1
						}
					}
			}
			copied
		}

		override def view(elems :String) :StringView = elems.view
		override def toIndexedSeq(elems :String) :IndexedSeq[Char] = elems
		override def toOps(elems :String) :collection.IndexedSeqOps[Char, IndexedSeq, String] = new StringAsSeq(elems)
		override def toIterableOnceOps(elems :String) :IterableOnceOps[Char, Any, Any] = StringIterator(elems)

		private def readResolve = IndexedSeqLike.forString
		override def toString = "IndexedSeqLike.forString"
	}

}






private abstract class GenericForArrayLike[E, Arr[X] <: ArrayLike[X]] extends IndexedSeqLike[E, ArrayLike, Arr[E]] {
	//shadow the inherited Arr[E] => IterableOnce[E] conversion
	import net.noresttherein.sugar.arrays.{ArrayLikeExtension => conversion}

	final override def knownSize(elems :Arr[E]) :Int = elems.length
	final override def size(elems :Arr[E]) :Int = elems.length
	final override def apply(elems :Arr[E])(i :Int) :E = elems(i)

	final override def forall(elems :Arr[E])(p :E => Boolean) :Boolean = elems.forall(p)
	final override def exists(elems :Arr[E])(p :E => Boolean) :Boolean = elems.exists(p)
	final override def count(elems :Arr[E])(p :E => Boolean) :Int = elems.count(p)
	final override def find(elems :Arr[E])(p :E => Boolean) :Option[E] = elems.find(p)
	final override def findLast(elems :Arr[E])(p :E => Boolean) :Option[E] = elems.findLast(p)
	final override def indexOf[A >: E](elems :Arr[E])(elem :A, from :Int = 0) :Int = elems.indexOf(elem, from)
	final override def lastIndexOf[A >: E](elems :Arr[E])(elem :A, end :Int) :Int = elems.lastIndexOf(elem, end)

	final override def indexWhere(elems :Arr[E])(p :E => Boolean, from :Int = 0) :Int = elems.indexWhere(p)
	final override def lastIndexWhere(elems :Arr[E])(p :E => Boolean, end :Int) :Int =
		elems.lastIndexWhere(p, end)

	final override def segmentLength(elems :Arr[E])(p :E => Boolean, from :Int = 0) :Int = elems.segmentLength(p)
	final override def startsWith[U >: E, O](elems :Arr[E])(that :O, offset :Int = 0)
	                                        (implicit iterableOnceLike :IterableOnceLike[U, generic.Any, O]) :Boolean =
		elems.startsWith(iterableOnceLike.toIterableOnce(that), offset)

	final override def endsWith[U >: E, O](elems :Arr[E])(that :O)
	                                      (implicit iterableLike :IterableLike[U, generic.Any, O]) :Boolean =
		elems.endsWith(iterableLike.toIterable(that))

	final override def indexOfSlice[A >: E](elems :Arr[E])(that :collection.Seq[A], from :Int = 0) :Int =
		elems.indexOfSlice(that)

	final override def lastIndexOfSlice[A >: E](elems :Arr[E])(that :collection.Seq[A], end :Int) :Int =
		elems.lastIndexOfSlice(that, end)

	final override def sameElements[U >: E, O](elems :Arr[E])(that :O)
	                                          (implicit iterableOnceLike :IterableOnceLike[U, generic.Any, O]) :Boolean =
		elems.sameElements(iterableOnceLike.toIterableOnce(that))

	final override def corresponds[A](elems :Arr[E])(that :collection.Seq[A])(p :(E, A) => Boolean) :Boolean =
		elems.corresponds(that)(p)

	final override def collectFirst[A](elems :Arr[E])(pf :PartialFunction[E, A]) :Option[A] = elems.collectFirst(pf)

	final override def search[A >: E :Ordering](elems :Arr[E])(elem :A, from :Int, until :Int) :SearchResult =
		elems.search(elem, from, until)

	final override def foldLeft[A](elems :Arr[E])(z :A)(op :(A, E) => A) :A = elems.foldLeft(z)(op)
	final override def foldRight[A](elems :Arr[E])(z :A)(op :(E, A) => A) :A = elems.foldRight(z)(op)
	final override def reduceLeft[A >: E](elems :Arr[E])(op :(A, E) => A) :A = elems.reduceLeft(op)
	final override def reduceRight[A >: E](elems :Arr[E])(op :(E, A) => A) :A = elems.reduceRight(op)
	final override def reduceLeftOption[A >: E](elems :Arr[E])(op :(A, E) => A) :Option[A] =
		elems.reduceLeftOption(op)
	final override def reduceRightOption[A >: E](elems :Arr[E])(op :(E, A) => A) :Option[A] =
		elems.reduceRightOption(op)

	override def iterator(elems :Arr[E]) :Iterator[E] = elems.iterator
	override def reverseIterator(elems :Arr[E]) :Iterator[E] = elems.reverseIterator
	override def stepper[S <: Stepper[_]](elems :Arr[E])(implicit shape :StepperShape[E, S]) :S = elems.stepper

	final override def copyToArray[A >: E](elems :Arr[E])(array :Array[A], start :Int, max :Int) :Int =
		elems.copyToArray(array, start, max)

	override def view(elems :Arr[E]) :IndexedSeqView[E] = elems.view //new IndexedSeqView.Id(toOps(elems))
	override def toIterableOnce(elems :Arr[E]) :IterableOnce[E] = elems.iterator
	override def toIterableOnceOps(elems :Arr[E]) :IterableOnceOps[E, generic.Any, Any] = elems.iterator

	override def infoString(elems :Arr[E]) :String = util.errorString(elems)
}


//consider: why two classes, not one?
private abstract class SpecificForArrayLike[E, Arr[X] <: ArrayLike[X]]
	extends GenericForArrayLike[E, Arr] with IndexedSeqLike[E, ArrayLike, Arr[E]]
{
	//shadow the inherited Arr[E] => IterableOnce[E] conversion
	import net.noresttherein.sugar.arrays.{ArrayLikeExtension => conversion}

	final override def slice(elems :Arr[E])(from :Int, until :Int) :Arr[E] = elems.slice(from, until)
	final override def take(elems :Arr[E])(n :Int) :Arr[E] = elems.take(n)
	final override def drop(elems :Arr[E])(n :Int) :Arr[E] = elems.drop(n)
	final override def takeRight(elems :Arr[E])(n :Int) :Arr[E] = elems.takeRight(n)
	final override def dropRight(elems :Arr[E])(n :Int) :Arr[E] = elems.dropRight(n)
	final override def takeWhile(elems :Arr[E])(p :E => Boolean) :Arr[E] = elems.takeWhile(p)
	final override def dropWhile(elems :Arr[E])(p :E => Boolean) :Arr[E] = elems.dropWhile(p)
	final override def splitAt(elems :Arr[E])(n :Int) :(Arr[E], Arr[E]) = elems.splitAt(n)
	final override def span(elems :Arr[E])(p :E => Boolean) :(Arr[E], Arr[E]) = elems.span(p)

	final override def reverse(elems :Arr[E]) :Arr[E] = elems.reverse

	final override def sorted[A >: E :Ordering](elems :Arr[E]) :Arr[E] = elems.sorted[A]
	final override def sortWith(elems :Arr[E])(lt :(E, E) => Boolean) :Arr[E] = elems.sortWith(lt)
	final override def sortBy[A :Ordering](elems :Arr[E])(f :E => A) :Arr[E] = elems.sortBy(f)
	final override def distinct(elems :Arr[E]) :Arr[E] = elems.distinct
	final override def distinctBy[A](elems :Arr[E])(f :E => A) :Arr[E] = elems.distinctBy(f)


	final override def tail(elems :Arr[E]) :Arr[E] = elems.tail
	final override def init(elems :Arr[E]) :Arr[E] = elems.init

	final override def filter(elems :Arr[E])(p :E => Boolean) :Arr[E] = elems.filter(p)
	final override def filterNot(elems :Arr[E])(p :E => Boolean) :Arr[E] = elems.filterNot(p)
	final override def partition(elems :Arr[E])(p :E => Boolean) :(Arr[E], Arr[E]) = elems.partition(p)

	final override def groupBy[K](elems :Arr[E])(f: E => K) :Map[K, Arr[E]] = elems.groupBy(f)
	final override def tapEach[U](elems :Arr[E])(f :E => U) :Arr[E] = elems.tapEach(f)
	final override def foreach[U](elems :Arr[E])(f :E => U) :Unit = elems.foreach(f)

	final override def zipWithIndex(elems :Arr[E]) :Arr[(E, Int)] = elems.zipWithIndex
	final override def zip[A, O](elems :Arr[E])(that :O)
	                            (implicit iterableOnceLike :IterableOnceLike[A, generic.Any, O]) :Arr[(E, A)] =
		elems.zip(iterableOnceLike.toIterableOnce(that))
	final override def lazyZip[A, O](elems :Arr[E])(that: O)
	                                (implicit iterableLike :IterableLike[A, generic.Any, O]) :LazyZip2[E, A, Arr[E]] =
		elems.lazyZip(iterableLike.toIterable(that))
	final override def zipAll[U >: E, A, O](elems :Arr[E])(that :O, thisElem :U, thatElem :A)
	                                       (implicit iterableLike :IterableLike[A, generic.Any, O]) :Arr[(U, A)] =
		elems.zipAll(iterableLike.toIterable(that), thisElem, thatElem)

	override def empty(elems :Arr[E]) :Arr[E] =
		ArrayFactory.empty(elems.getClass.getComponentType).asInstanceOf[Arr[E]]

	protected override def fromSpecific(elems :Arr[E])(coll :IterableOnce[E]) =
		coll.toBasicOps.toArray(ClassTag[E](elems.getClass.getComponentType)).asInstanceOf[Arr[E]]

	protected override def newSpecificBuilder(elems :Arr[E]) =
		ArrayFactory.newBuilder(elems.getClass.getComponentType.castParam[E]).asInstanceOf[Builder[E, Arr[E]]]

	final override def toArray[A >: E :ClassTag](elems :Arr[E]) :Array[A] = elems.toArray


	override def prepended[A >: E](elems :Arr[E])(elem :A) :ArrayLike[A] = {
		val res =
			if (elem.getClass <%< elems.getClass.getComponentType)
				Array.like(elems.asInstanceOf[Array[A]])
			else
				new Array[Any](elems.length + 1).asInstanceOf[Array[A]]
		res(0) = elem
		ArrayLike.copy(elems, 0, res, 1, elems.length)
		res.asInstanceOf[ArrayLike[A]]
	}
	override def appended[A >: E](elems :Arr[E])(elem :A) :ArrayLike[A] = {
		val length = elems.length
		if (elem.getClass <%< elems.getClass.getComponentType) {
			val res = ArrayFactory.copyOf(elems.asInstanceOf[Array[A]], length + 1)
			res(length) = elem
			res
		} else {
			val res = RefArray.ofDim[A](length + 1)
			ArrayLike.copy(elems, 0, res, 0, length)
			res(length) = elem
			res
		}
	}

	override def prependedAll[A >: E, O](elems :Arr[E])(prefix :O)
	                                    (implicit iterableOnceLike :IterableOnceLike[A, generic.Any, O]) =
	{
		val size = iterableOnceLike.knownSize(prefix)
		val length = elems.length
		if (size >= 0) {
			val res = RefArray.ofDim[A](length + size)
			iterableOnceLike.toIterableOnceOps(prefix).copyToArray(res.asAnyArray)
			ArrayLike.copy(elems, 0, res, size, length)
			res
		} else {
			val array = iterableOnceLike.toRefArray(prefix)
			RefArray.copyOfRanges(array, 0, array.length, elems, 0, length)
		}
	}
	override def concat[A >: E, O](elems :Arr[E])(suffix :O)
	                              (implicit iterableOnceLike :IterableOnceLike[A, generic.Any, O]) =
	{
		val size   = iterableOnceLike.knownSize(suffix)
		val length = elems.length
		if (size >= 0) {
			val res = RefArray.ofDim[A](length + size)
			ArrayLike.copy(elems, 0, res, 0, length)
			iterableOnceLike.toIterableOnceOps(suffix).copyToArray(res.asAnyArray, length)
			res
		} else {
			val array = iterableOnceLike.toRefArray(suffix)
			RefArray.copyOfRanges(elems, 0, length, array, 0, array.length)
		}
	}

	override def padTo[A >: E](elems :Arr[E])(len :Int, elem :A) = {
		if (elems.length >= len)
			RefArray.copyOf(elems)
		else {
			val res = RefArray.copyOf[A](elems, len)
			res.fill(elems.length, len)(elem)
			res
		}
	}

	override def iterableFactory(elems :Arr[E]) :IterableFactory[ArrayLike] = IRefArray

}






/** @define coll mutable indexed sequence
  */
trait MutableIndexedSeqLike[E, +CC[_], C] extends MutableSeqLike[E, CC, C] with IndexedSeqLike[E, CC, C]


private[collections] sealed abstract class Rank1MutableIndexedSeqLike
	extends IterableOnceLikeSummons[MutableIndexedSeqLike]
{
	implicit def forOps[E, CC[X]/* <: Iterable[X]*/, C <: AnyRef/* <: CC[E] with mutable.IndexedSeqOps[E, CC, C]*/]
	                   (implicit specific :C <:< CC[E] with mutable.IndexedSeqOps[E, CC, C], generic :CC <:?< Iterable)
			:MutableIndexedSeqLike[E, CC, C] =
		forOpsPrototype.asInstanceOf[MutableIndexedSeqLike[E, CC, C]]

	private[this] val forOpsPrototype = new MutableIndexedSeqLike.ForOps[Any, Iterable, mutable.IndexedSeq[Any]] {
		private def readResolve = MutableIndexedSeqLike.forOps[Any, Iterable, mutable.IndexedSeq[Any]]
		override def toString = "MutableIndexedSeqLike.forOps"
	}
}


@SerialVersionUID(Ver)
object MutableIndexedSeqLike extends Rank1MutableIndexedSeqLike {

	implicit def forRefArray[E] :MutableIndexedSeqLike[E, RefArray, RefArray[E]] =
		refArrayPrototype.asInstanceOf[MutableIndexedSeqLike[E, RefArray, RefArray[E]]]

	implicit def forArray[E] :MutableIndexedSeqLike[E, RefArray, Array[E]] =
		arrayPrototype.asInstanceOf[MutableIndexedSeqLike[E, RefArray, Array[E]]]


	private abstract class ForArrayLike[E, Arr[X] <: ArrayLike[X]]
		extends SpecificForArrayLike[E, Arr] with MutableIndexedSeqLike[E, ArrayLike, Arr[E]]
	{
		override def iterableFactory(elems :Arr[E]) :IterableFactory[ArrayLike] = RefArray
	}

	private[this] val arrayPrototype = new ForArrayLike[Unknown, Array] {
		override def update(elems :Array[Unknown])(idx :Int, elem :Unknown) :Unit = elems(idx) = elem
		override def toOps(elems :Array[Unknown]) = elems.toOps
		override def toIterable(elems :Array[Unknown]) = ArraySlice.wrap(elems)
		override def toIndexedSeq(elems :Array[Unknown]) = IArray.Wrapped(IArray.from(elems))
		override def toString = "MutableIndexedSeqLike.forArray"
		private def readResolve = MutableIndexedSeqLike.forArray
	}
	private[this] val refArrayPrototype = new ForArrayLike[Unknown, RefArray] {
		override def update(elems :RefArray[Unknown])(idx :Int, elem :Unknown) :Unit = elems(idx) = elem
		override def patch[A >: Unknown](elems :RefArray[Unknown])(from :Int, other :IterableOnce[A], replaced :Int) =
			RefArrayLikeExtension(elems).patch(from, other, replaced)

		override def toOps(elems :RefArray[Unknown]) = elems.toOps
		override def toIterable(elems :RefArray[Unknown]) = RefArray.Wrapped(elems)
		override def toIndexedSeq(elems :RefArray[Unknown]) = IRefArray.Wrapped(IRefArray.copyOf(elems))
		override def toString = "MutableIndexedSeqLike.forRefArray"
		private def readResolve = MutableIndexedSeqLike.forRefArray
	}


	trait ForOps[E, CC[X] <: Iterable[X], C <: CC[E] with mutable.IndexedSeqOps[E, CC, C]]
		extends MutableSeqLike.ForOps[E, CC, C] with IndexedSeqLike.ForOps[E, CC, C]
	{
		override def toOps(elems :C) :mutable.IndexedSeqOps[E, CC, C] = elems
	}
}
