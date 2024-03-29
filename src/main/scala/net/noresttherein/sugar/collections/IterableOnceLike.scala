package net.noresttherein.sugar.collections

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{Factory, IterableOnceOps, Stepper, StepperShape, mutable}
import scala.collection.mutable.{ArrayBuilder, Buffer}
import scala.reflect.ClassTag

import net.noresttherein.sugar.arrays.{IArray, IRefArray, RefArray}
import net.noresttherein.sugar.collections.IterableOnceLikeSummons.{GenericSummoner, Summoner}
import net.noresttherein.sugar.exceptions.unsupported_!
import net.noresttherein.sugar.extensions.{SeqExtension, castTypeParamMethods, castingMethods, classNameMethods, mutableIndexedSeqExtension}
import net.noresttherein.sugar.funny
import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.typist.<:?<




/** A type class providing operations available in [[collection.IterableOnce IterableOnce]] for type `C`.
  * An implicit instance exists for proper `IterableOnce` and collection-like types
  * (arrays and [[net.noresttherein.sugar.arrays.ArrayLike ArrayLike]], `String`, etc.), but also a singleton
  * element. This duplication allows several benefits:
  *   1. Unbounded polymorphism without boxing overhead: for example, methods
  *      `addOne(x :X)` and `addAll(x :IterableOnce[X])` can share the same implementation
  *      {{{
  *         def add[CC[_], C](x :C)(implicit multiple :IterableOnceLike[X, CC, C])
  *      }}}
  *   2. Treating one collection type like another: for example, a [[net.noresttherein.sugar.collections.Ranking Ranking]]
  *      shares features of both a `SetOps` and an `IndexedSeqOps`, but can implement neither interface
  *      due to type constraints and method conflicts. This allows all extension methods designed for sequences
  *      from this package work also for rankings.
  *   3. This polymorphism is achieved while preserving the represented type: mapping an `Array` or a `Ranking`
  *      will also produce an `Array`/`Ranking`, which is not the case with existing adapter collections.
  *
  * @tparam E  the element type.
  * @tparam CC the type constructor for the generic version of the collection, applicable to any element type.
  * @tparam C  the collection type
  *
  * @define Coll collection of the same kind
  * @define coll single use collection
  *
  * @define orderDependent
  *
  *              Note: might return different results for different runs, unless the underlying collection type is ordered.
  * @define orderDependentFold
  *
  *              Note: might return different results for different runs, unless the
  *              underlying collection type is ordered or the operator is associative
  *              and commutative.
  * @define mayNotTerminateInf
  *
  *              Note: may not terminate for infinite-sized collections.
  * @define willNotTerminateInf
  *
  *              Note: will not terminate for infinite-sized collections.
  * @define willForceEvaluation
  *              Note: Even when applied to a view or a lazy collection it will always force the elements.
  * @define consumesIterator
  *              After calling this method, one should discard the iterator it was called
  *              on. Using it is undefined and subject to change.
  * @define undefinedorder
  *              The order in which operations are performed on elements is unspecified
  *              and may be nondeterministic.
  */
//Todo: we must streamline this collection of interfaces to the basic bare bones, leaving out everything
// that we are unlikely to a) need in the context of high abstraction, and b) override with specific implementations.
trait IterableOnceLike[+E, +CC[_], C] extends CollectionLike[E, C] {

	/** Produces a $coll containing cumulative results of applying the
	  * operator going left to right, including the initial value.
	  *
	  * $willNotTerminateInf
	  * $orderDependent
	  * @tparam A    the type of the elements in the resulting collection.
	  * @param elems a $coll.
	  * @param z     the initial value.
	  * @param op    the binary operator applied to the intermediate result and the element.
	  * @return collection with intermediate results
	  */
	def scanLeft[A](elems :C)(z :A)(op :(A, E) => A) :CC[A]

	/** Selects all elements of `elems` which satisfy a predicate.
	  * @param elems a $coll.
	  * @param p     the predicate used to test elements.
	  * @return a new $coll consisting of all elements of `elems` that satisfy the given
	  *         predicate `p`. The order of the elements is preserved.
	  */
	def filter(elems :C)(p :E => Boolean) :C

	/** Selects all elements of `elems` which do not satisfy a predicate.
	  * @param elems a $coll.
	  * @param p     the predicate used to test elements.
	  * @return a new $coll consisting of all elements of `elems` that do not satisfy the given
	  *         predicate `pred`. Their order may not be preserved.
	  */
	def filterNot(elems :C)(p :E => Boolean) :C

	/** Selects the first ''n'' elements.
	  * $orderDependent
	  * @param elems a $coll.
	  * @param n     the number of elements to take from `elems`.
	  * @return a $coll consisting only of the first `n` elements of `elems`,
	  *         or else the whole $coll, if it has less than `n` elements.
	  *         If `n` is negative, returns an empty $coll.
	  */
	def take(elems :C)(n :Int) :C

	/** Takes longest prefix of elements that satisfy a predicate.
	  * $orderDependent
	  * @param elems a $coll.
	  * @param p     The predicate used to test elements.
	  * @return the longest prefix of `elems` whose elements all satisfy
	  *         the predicate `p`.
	  */
	def takeWhile(elems :C)(p :E => Boolean) :C

	/** Selects all elements except first ''n'' ones.
	  * $orderDependent
	  * @param elems a $coll.
	  * @param n     the number of elements to drop from `elems`.
	  * @return a $coll consisting of all elements of `elems` except the first `n` ones, or else the
	  *         empty $coll, if `elems` has less than `n` elements.
	  *         If `n` is negative, don't drop any elements.
	  */
	def drop(elems :C)(n :Int) :C

	/** Drops longest prefix of elements that satisfy a predicate.
	  * $orderDependent
	  * @param elems a $coll.
	  * @param p     The predicate used to test elements.
	  * @return the longest suffix of `elems` whose first element
	  *         does not satisfy the predicate `p`.
	  */
	def dropWhile(elems :C)(p :E => Boolean) :C

	/** Selects an interval of elements.  The returned $coll is made up
	  * of all elements `x` which satisfy the invariant:
	  * {{{
	  *    from <= indexOf(x) < until
	  * }}}
	  * $orderDependent
	  * @param elems a $coll.
	  * @param from  the lowest index to include from `elems`.
	  * @param until the lowest index to EXCLUDE from `elems`.
	  * @return  a $coll containing the elements greater than or equal to
	  *          index `from` extending up to (but not including) index `until`
	  *          of `elems`.
	  */
	def slice(elems :C)(from: Int, until: Int) :C

	/** Builds a new $coll by applying a function to all elements of `elems`.
	  * @tparam A    the element type of the returned $coll.
	  * @param elems a $coll.
	  * @param f     the function to apply to each element.
	  * @return      a new $coll resulting from applying the given function
	  *              `f` to each element of `elems` and collecting the results.
	  */
	def map[A](elems :C)(f :E => A) :CC[A]

	/** Builds a new $coll by applying a function to all elements of `elems`
	  * and using the elements of the resulting collections.
	  *
	  * For example:
	  * {{{
	  *      def getWords(lines: Seq[String]): Seq[String] = lines flatMap (line => line split "\\W+")
	  * }}}
	  * The type of the resulting collection is guided by the static type of $coll. This might
	  * cause unexpected results sometimes. For example:
	  * {{{
	  *      // lettersOf will return a Seq[Char] of likely repeated letters, instead of a Set
	  *      def lettersOf(words: Seq[String]) = words flatMap (word => word.toSet)
	  *
	  *      // lettersOf will return a Set[Char], not a Seq
	  *      def lettersOf(words: Seq[String]) = words.toSet flatMap ((word: String) => word.toSeq)
	  *
	  *      // xs will be an Iterable[Int]
	  *      val xs = Map("a" -> List(11,111), "b" -> List(22,222)).flatMap(_._2)
	  *
	  *      // ys will be a Map[Int, Int]
	  *      val ys = Map("a" -> List(1 -> 11,1 -> 111), "b" -> List(2 -> 22,2 -> 222)).flatMap(_._2)
	  * }}}
	  * @tparam A     the element type of the returned collection.
	  * @param elems  a $coll.
	  * @param f      the function to apply to each element.
	  * @return       a new $coll resulting from applying the given collection-valued function
	  *               `f` to each element of `elems` and concatenating the results.
	  */
	def flatMap[O, A](elems :C)(f :E => O)(implicit asIterableOnce :IterableOnceLike[A, generic.Any1, O]) :CC[A]

	/** Builds a new $coll by applying a function to all elements of `elems`
	  * and using the elements of the resulting collections.
	  * @tparam A     the element type of the returned collection.
	  * @param elems  a $coll.
	  * @param f      the function to apply to each element.
	  * @return       a new $coll resulting from applying the given collection-valued function
	  *               `f` to each element of `elems` and concatenating the results.
	  */
	def flatMapIterableOnce[A](elems :C)(f :E => IterableOnce[A]) :CC[A]

	/** Converts `elems` of iterable collections into
	  * a $coll formed by the elements of these iterable
	  * collections.
	  *
	  * The resulting collection's type will be guided by the
	  * type of $coll. For example:
	  * {{{
	  *    val xs = List(
	  *               Set(1, 2, 3),
	  *               Set(1, 2, 3)
	  *             ).flatten
	  *    // xs == List(1, 2, 3, 1, 2, 3)
	  *
	  *    val ys = Set(
	  *               List(1, 2, 3),
	  *               List(3, 2, 1)
	  *             ).flatten
	  *    // ys == Set(1, 2, 3)
	  * }}}
	  * @tparam A         the type of the elements of each iterable collection.
	  * @param elems      a $coll.
	  * @param iterableOnceLike a type class allowing to treat `elems` like `IterableOnce`.
	  * @return a new $coll resulting from concatenating all element ${coll}s.
	  */
	def flatten[A, U >: E](elems :C)(implicit iterableOnceLike :IterableOnceLike[A, generic.Any1, U]) :CC[A]

	def flattenIterableOnce[A](elems :C)(implicit asIterableOnce :E => IterableOnce[A]) :CC[A]

	/** Builds a new $coll by applying a partial function to all elements of `elems`
	  * on which the function is defined.
	  * @tparam A     the element type of the returned  $coll.
	  * @param elems  a $coll.
	  * @param pf     the partial function which filters and maps the $ ll.
	  * @return       a new $coll resulting from applying the given partial function
	  *               `pf` to each element on which it is defined and collecting the results.
	  *               The order of the elements is preserved.
	  */
	def collect[A](elems :C)(pf :PartialFunction[E, A]) :CC[A]

	/** Zips `elems` with its indices.
	  * @param elems a $coll.
	  * @return      A new $coll containing pairs consisting of all elements of `elems` paired with their index.
	  *              Indices sta  at `0`.
	  * @example
	  *    `List("a", "b", "c").zipWithIndex == List(("a", 0), ("b", 1), ("c", 2))`
	  */
	def zipWithIndex(elems :C) :CC[(E @uncheckedVariance, Int)]

	/** Splits `elems` into a prefix/suffix pair according to a predicate.
	  * Note: `c span p`  is equivalent to (but possibly more efficient than)
	  * `(c takeWhile p, c dropWhile p)`, provided the evaluation of the
	  * predicate `p` does not cause any sid effects.
	  * $orderDependent
	  *
	  * @param elems a $coll.
	  * @param p     the test predicate.
	  * @return      a pair consisting of the longest prefix of `elems` whose
	  *              elements all satisfy `p`, and the rest of `elems`.
	  */
	def span(elems :C)(p :E=> Boolean) :(C, C)

	/** Splits `elems` into a prefix/suffix pair at a given position.
	  * Note: `c splitAt n` is equivalent to (but possibly more efficient than)
	  *         `(c take n, c dro n)`.
	  * $orderDependent
	  * @param elems a $coll.
	  * @param n     the position at which to  lit. *  @return  a pair of ${coll}s consisting of the first `n`
	  *              elements of `elems`, and the other elements.
	  */
	def splitAt(elems :C)(n :Int) :(C, C)

	/** Applies a side-effecting function to each element in this collection.
	  * Strict collections will apply `f` to their elements immediately, while lazy collections
	  * like Views and LazyLists will only apply `f` on each element if and when that element
	  * is evaluated, and each time that element is evaluated.
	  * @tparam U    the return type of f.
	  * @param elems a $coll.
	  * @param f     a function to apply to each element in `elems`.
	  * @return The same logical collection as this.
	  */
	def tapEach[U](elems :C)(f :E => U) :C



	/** Tests whether a predicate holds for all elements of `elems`.
	  *
	  * $mayNotTerminateInf
	  * @param elems a $coll.
	  * @param p     the predicate used to test elements.
	  * @return      `true` if `elems` is empty or the given predicate `p`
	  *               holds for all elements of `elems`, otherwise `false`.
	  */
	def forall(elems :C)(p :E => Boolean) :Boolean = knownSize(elems) == 0 || toIterableOnceOps(elems).forall(p)

	/** Tests whether a predicate holds for at least one element of `elems`.
	  *
	  * $mayNotTerminateInf
	  * @param elems a $coll.
	  * @param p     the predicate used to test elements.
	  * @return  `true` if the given predicate `p` is satisfied by at least one element of `elems`, otherwise `false`.
	  */
	def exists(elems :C)(p :E => Boolean) :Boolean = knownSize(elems) != 0 && toIterableOnceOps(elems).exists(p)

	/** Counts the number of elements in the $coll which satisfy a predicate.
	  *
	  * $willNotTerminateInf
	  * @param elems a $coll.
	  * @param p     the predicate  used to test elements.
	  * @return      the number of elements satisfying the predicate `p`.
	  */
	def count(elems :C)(p :E => Boolean) :Int =
		if (knownSize(elems) == 0) 0 else toIterableOnceOps(elems).count(p)

	/** Finds the first element of the $coll satisfying a predicate, if any.
	  *
	  * $mayNotTerminateInf
	  * $orderDependent
	  *
	  * @param elems a $coll.
	  * @param p     the predicate used to test elements.
	  * @return an option value containing the first element in the $coll that satisfies `p`, or `None` if none exists.
	  */
	def find(elems :C)(p :E => Boolean) :Option[E] =
		if (knownSize(elems) == 0) None else toIterableOnceOps(elems).find(p)

	/** Applies a binary operator to all elements of `elems` and a start value, going right to left.
	  *
	  * $willNotTerminateInf
	  * $orderDependentFold
	  * @tparam A    the result type of the binary operator.
	  * @param elems a $coll.
	  * @param z     the start value.
	  * @param op    the binary operator.
	  * @return the result of inserting `op` between consecutive elements of `elems`, going right to left
	  *         with the start value `z` on the right: `op(x,,1,,, op(x,,2,,, ... op(x,,n,,, z)...))`
	  *         where `x,,1,,, ..., x,,n,,` are the elements of `elems`. Returns `z` if `elems` is empty.
	  */
	def foldRight[A](elems :C)(z :A)(op :(E, A) => A) :A =
		if (knownSize(elems) == 0) z else reverseIterator(elems).foldLeft(z)((b, a) => op(a, b))

	/** Optionally applies a binary operator to all elements of `elems`, going left to right.
	  * @param elems a $coll of this type class.
	  * @param op    a binary operator.
	  * @return an option value containing the result of `reduceLeft(op)` if `elems` is nonempty, `None` otherwise.
	  */
	override def reduceLeftOption[U >: E](elems :C)(op :(U, E) => U) :Option[U] =
		knownSize(elems) match {
			case -1 => toIterableOnceOps(elems).reduceLeftOption(op)
			case 0 => None
			case _ => Some(reduceLeft[U](elems)(op))
		}

	/** Applies a binary operator to all elements of `elems`, going right to left.
	  * @tparam A    the result type of the binary operator.
	  * @param elems a $coll of this type class.
	  * @param op    the binary operator.
	  * @return the result of inserting `op` between consecutive elements of `elems`,
	  *         going right to left: `op(x,,1,,, op(x,,2,,, ..., op(x,,n-1,,, x,,n,,)...))` where `x,,1,,, ..., x,,n,,`
	  *         are the elements of `elems`.
	  * @throws UnsupportedOperationException if `elems` is empty.
	  */
	def reduceRight[A >: E](elems :C)(op :(E, A) => A) :A = this match {
		case _ if knownSize(elems) == 0 => unsupported_!("empty.reduceRight")
		case _ => reverseIterator(elems).reduceLeft[A]((x, y) => op(y, x))
	}

	/** Optionally applies a binary operator to all elements of `elems`, going right to left.
	  *
	  * $willNotTerminateInf
	  * $orderDependentFold
	  * @tparam A    the result type of the binary operator.
	  * @param elems a $coll.
	  * @param op    a binary operator.
	  * @return an option value containing the result of `reduceRight(op)` if `elems` is nonempty,
	  *         `None` otherwise.
	  */
	def reduceRightOption[A >: E](elems :C)(op :(E, A) => A) :Option[A] =
		knownSize(elems) match {
			case -1 => reverseIterator(elems).reduceLeftOption[A]((x, y) => op(y, x))
			case 0 => None
			case _ => Some(reduceRight[A](elems)(op))
		}

	/** Finds the first element of the $coll for which the given partial
	  * function is defined, and applies the partial function to it.
	  *
	  * $mayNotTerminateInf
	  * $orderDependent
	  * @tparam A    The result type of the function pf.
	  * @param elems a $coll.
	  * @param pf    the partial function
	  * @return an option value containing pf applied to the first
	  *         value for which it is defined, or `None` if none exists.
	  * @example `Seq("a", 1, 5L).collectFirst({ case x: Int => x*10 }) = Some(10)`
	  */
	def collectFirst[A](elems :C)(pf :PartialFunction[E, A]) :Option[A] =
		if (knownSize(elems) == 0) None else toIterableOnceOps(elems).collectFirst(pf)

	/** Tests whether every element of this collection's iterator relates to the
	  * corresponding element of another collection by satisfying a test predicate.
	  *
	  * $willNotTerminateInf
	  * @tparam A    the type of the elements of `that`.
	  * @tparam O    the second collection type.
	  * @param elems a $coll.
	  * @param that  the other collection
	  * @param p     the test predicate, which relates elements from both collections
	  * @return `true` if both collections have the same length and
	  *         `p(x, y)` is `true` for all corresponding elements `x` of this iterator
	  *         and `y` of `that`, otherwise `false`
	  */
	def corresponds[A, CO[_], O](elems :C)(that :O)(p :(E, A) => Boolean)
	                            (implicit iterableOnceLike :IterableOnceLike[A, CO, O]) :Boolean =
		toIterableOnceOps(elems).corresponds(iterableOnceLike.toIterableOnce(that))(p)

	override def appendedTo[A >: E, SC[_], S](elems :C)(seq :collection.SeqOps[A, SC, S]) :SC[A] =
		seq appendedAll elems

	override def prependedTo[A >: E, SC[_], S](elems :C)(seq :collection.SeqOps[A, SC, S]) :SC[A] =
		seq prependedAll elems

	override def copiedTo[A >: E, SC[_], S](elems :C)(seq :collection.SeqOps[A, SC, S], index :Int) :SC[A] =
		seq.updatedAll(index, elems)

	override def patchedOn[A >: E, SC[_], S](elems :C)(seq :collection.SeqOps[A, SC, S], index :Int, replaced :Int) :SC[A] =
		seq.patch(index, elems, replaced)

	override def appendTo[A >: E](elems :C)(buffer :Buffer[A]) :Unit = buffer ++= elems
	override def prependTo[A >: E](elems :C)(buffer :Buffer[A]) :Unit = elems ++=: buffer
	override def insertTo[A >: E](elems :C)(buffer :Buffer[A], index :Int) :Unit = buffer.insertAll(index, elems)
	override def patchOn[A >: E](elems :C)(buffer :Buffer[A], index :Int, replaced :Int) :Unit =
		buffer.patchInPlace(index, elems, replaced)

	//	def setAll[A >: X](xs :Xs)(seq :mutable.Seq[A], index :Int) :Unit
	override def copyTo[A >: E](elems :C)(seq :mutable.Seq[A], index :Int) :Int = seq match {
		case indexed :mutable.IndexedSeq[A] => indexed.updateAll(index, elems)
		case _ => foldLeft(elems)(index) { (i, e) => seq(i) = e; i + 1 }
	}

	/** Displays all elements of `elems` in a string using start, end, and separator strings.
	  * Delegates to `addString`, which can be overridden.
	  * @param elems a $coll.
	  * @param start the starting string.
	  * @param sep   the separator string.
	  * @param end   the ending string.
	  * @return a string representation of `elems`. The resulting string
	  *         begins with the string `start` and ends with the string
	  *         `end`. Inside, the string representations (w.r.t. the method
	  *         `toString`) of all elements of `elems` are separated by
	  *         the string `sep`.
	  * @example `List(1, 2, 3).mkString("(", "; ", ")") = "(1; 2; 3)"`
	  */
	final def mkString(elems :C, start :String, sep :String, end :String) :String =
		if (knownSize(elems) == 0) start + end
		else addString(elems, new StringBuilder(), start, sep, end).result()

	/** Displays all elements of `elems` in a string using a separator string.
	  * Delegates to `addString`, which can be overridden.
	  * @param elems a $coll.
	  * @param sep   the separator string.
	  * @return a string representation of `elems`. In the resulting string
	  *         the string representations (w.r.t. the method `toString`)
	  *         of all elements of `elems` are separated by the string `sep`.
	  *
	  * @example `List(1, 2, 3).mkString("|") = "1|2|3"`
	  */
	@inline final def mkString(elems :C, sep :String) :String = mkString(elems, "", sep, "")

	/** Displays all elements of `elems` in a string. Delegates to `addString`, which can be overridden.
	  * @param elems a $coll.
	  * @return a string representation of `elems`. In the resulting string
	  *         the string representations (w.r.t. the method `toString`)
	  *         of all elements of `elems` follow each other without any
	  *         separator string.
	  */
	@inline final def mkString(elems :C) :String = mkString(elems, "")

	/** Appends all elements of `elems` to a string builder using start, end, and separator strings.
	  * The written text begins with the string `start` and ends with the string `end`.
	  * Inside, the string representations (w.r.t. the method `toString`)
	  * of all elements of `elems` are separated by the string `sep`.
	  *
	  * Example:
	  * {{{
	  *      scala> val a = List(1,2,3,4)
	  *      a: List[Int] = List(1, 2, 3, 4)
	  *
	  *      scala> val b = new StringBuilder()
	  *      b: StringBuilder =
	  *
	  *      scala> a.addString(b , "List(" , ", " , ")")
	  *      res5: StringBuilder = List(1, 2, 3, 4)
	  * }}}
	  * @param elems a $coll.
	  * @param b     the string builder to which elements are appended.
	  * @param start the starting string.
	  * @param sep   the separator string.
	  * @param end   the ending string.
	  * @return      the string builder `b` to which elements were appended.
	  */
	def addString(elems :C, b :StringBuilder, start :String, sep :String, end :String) :b.type =
		if (knownSize(elems) == 0) {
			val jsb = b.underlying
			if (start.length != 0) jsb append start
			if (end.length != 0) jsb append end
			b
		} else
			toIterableOnceOps(elems).addString(b, start, sep, end)

	/** Appends all elements of `elems` to a string builder using a separator string.
	  * The written text consists of the string representations (w.r.t. the method `toString`)
	  * of all elements of `elems`, separated by the string `sep`.
	  *
	  * Example:
	  * {{{
	  *      scala> val a = List(1,2,3,4)
	  *      a: List[Int] = List(1, 2, 3, 4)
	  *
	  *      scala> val b = new StringBuilder()
	  *      b: StringBuilder =
	  *
	  *      scala> a.addString(b, ", ")
	  *      res0: StringBuilder = 1, 2, 3, 4
	  * }}}
	  * @param elems a $coll.
	  * @param b     the string builder to which elements are appended.
	  * @param sep   the separator string.
	  * @return      the string builder `b` to which elements were appended.
	  */
	@inline final def addString(elems :C, b :StringBuilder, sep :String) :b.type = addString(elems, b, "", sep, "")

	/** Appends all elements of `elems` to a string builder.
	  * The written text consists of the string representations (w.r.t. the method
	  *`toString`) of all elements of `elems` without any separator string.
	  *
	  * Example:
	  * {{{
	  *      scala> val a = List(1,2,3,4)
	  *      a: List[Int] = List(1, 2, 3, 4)
	  *
	  *      scala> val b = new StringBuilder()
	  *      b: StringBuilder =
	  *
	  *      scala> val h = a.addString(b)
	  *      h: StringBuilder = 1234
	  * }}}
	  * @param elems a $coll.
	  * @param b     the string builder to which elements are appended.
	  * @return      the string builder `b` to which elements were appended.
	  */
	@inline final def addString(elems :C, b :StringBuilder) :b.type = addString(elems, b, "")

	/** Given a collection factory `factory`, convert this collection to the appropriate
	  * representation for the current element type `A`. Example uses:
	  * {{{
	  *      xs.to(List)
	  *      xs.to(ArrayBuffer)
	  *      xs.to(BitSet) // for xs: Iterable[Int]
	  * }}}
	  * @param elems a $coll.
	  */
	def to[C1](elems :C)(factory :Factory[E ,C1]):C1 = factory.fromSpecific(toIterableOnce(elems))

	/** This collection as `IterableOnceOps`. Note that it's different from
	  * [[net.noresttherein.sugar.collections.CollectionLike.toIterableOnceOps toIterableOnceOps]] in that it guarantees
	  * that the operations relate to the described collection type `C` itself,
	  * rather than something else (for example, an iterator).
	  */
	def toOps(elems :C) :IterableOnceOps[E, CC, C]

	/** Adapts this collection type to `IterableOnce[E]`.
	  * @param elems a $coll.
	  */
	override def toIterableOnce(elems :C) :IterableOnce[E] = iterator(elems)

	/** Converts this collection type to `Iterable`.
	  * For single-use collection this will copy the elements, but collections supporting
	  * [[net.noresttherein.sugar.collections.IterableOnceLike.isTraversableAgain repeated]] iterations
	  * will return an adapter to `elems`.
	  * @param elems a $coll.
	  */
	def toIterable(elems :C) :Iterable[E] = toSeq(elems)

	/** @return `elems` collection as a `List[A]`. This is equivalent to `to(List)` but might be faster. */
	def toList(elems :C): List[E] = List.from(toIterableOnce(elems))

	/** @return `elems` collection as a `Vector[A]`. This is equivalent to `to(Vector)` but might be faster. */
	def toVector(elems :C): Vector[E] = Vector.from(toIterableOnce(elems))

	/** @return `elems` collection as a `Seq[A]`. This is equivalent to `to(Seq)` but might be faster. */
	def toSeq(elems :C): Seq[E] = Seq.from(toIterableOnce(elems))

	/** @return `elems` collection as an `IndexedSeq[A]`. This is equivalent to `to(InexedSeq)` but might be faster. */
	def toIndexedSeq(elems :C): IndexedSeq[E] = IndexedSeq.from(toIterableOnce(elems))

	@`inline` final def toBuffer[A >: E](elems :C): Buffer[A] = Buffer.from(toIterableOnce(elems))

	/** @return `elems` collection as a `Map[K, V]`. This is equivalent to `to(Map)` but might be faster. */
	def toMap[K, V](elems :C)(implicit ev: E <:< (K ,V)): Map[K, V] =
		Map.from(toIterableOnce(elems).asInstanceOf[IterableOnce[(K, V)]])

	/** @return `elems` collection as a `Set[A]`. This is equivalent to `to(Set)` but might be faster. */
	def toSet[A >: E](elems :C): Set[A] = Set.from(toIterableOnce(elems))

	/** Convert collection to array. This will always create a new array, if `elems` already is an array. */
	def toArray[A >: E: ClassTag](elems :C): Array[A] = {
		val size = knownSize(elems)
		if (size >= 0) {
			val destination = new Array[A](size)
			copyToArray[A](elems)(destination, 0)
			destination
		} else
			ArrayBuilder.make[A].addAll(toIterableOnce(elems)).result()
	}
	final def toIArray[A >: E :ClassTag](elems :C) :IArray[A] = toArray[A](elems).castFrom[Array[A], IArray[A]]
	final def toRefArray[A >: E](elems :C) :RefArray[A] = toArray[Any](elems).castFrom[Array[Any], RefArray[A]]
	final def toIRefArray[A >: E](elems :C) :IRefArray[A] = toArray[Any](elems).castFrom[Array[Any], IRefArray[A]]

	// For internal use
	protected def reverseIterator(elems :C) :Iterator[E] = reversed(elems).iterator
	protected def reversed(elems :C): Iterable[E] = {
		var xs: List[E] = Nil
		val it = iterator(elems)
		while (it.hasNext) xs = it.next() :: xs
		xs
	}

	implicit def conversion :C => IterableOnce[E] = toIterableOnce

	override def infoString(elems :C) :String = {
		val n = knownSize(elems)
		if (n >= 0) elems.localClassName + "|" + n + "|"
		else elems.localClassName
	}
}






private[collections] abstract class IterableOnceLikeSummons[T[_, CC[_], _]] {
	@inline final def apply[E, CC[_], C](implicit ops :T[E, CC, C]) :ops.type = ops

	@inline final def apply[C] :Summoner[T, C] = new Summoner[T, C] {}

	@inline final def generic[E, CC[_]](implicit ops :T[E, CC, CC[E]]) :ops.type = ops

	@inline final def generic[CC[_]] :GenericSummoner[T, CC] = new GenericSummoner[T, CC] {}
}

private[collections] object IterableOnceLikeSummons {
	private[collections] sealed trait Summoner[T[_, CC[_], _], C] extends Any {
		@inline final def apply[E, CC[_]]()(implicit ops :T[E, CC, C]) :ops.type = ops
	}
	private[collections] sealed trait GenericSummoner[T[_, CC[_], _], CC[_]] extends Any {
		@inline final def apply[E]()(implicit ops :T[E, CC, CC[E]]) :ops.type = ops
	}
}




private[collections] sealed abstract class Rank2IterableOnceLike extends IterableOnceLikeSummons[IterableOnceLike] {
	implicit def forIterableOnce[E] :IterableOnceLike[E, IterableOnce, IterableOnce[E]] =
		prototype.castParam[E]

	private[this] val prototype = new IterableOnceLike.ForIterableOnce[Any] {
		private def readResolve :AnyRef = IterableOnceLike.forIterableOnce
		override def toString = "IterableOnceLike.forIterableOnce"
	}
}

private[collections] sealed abstract class Rank1IterableOnceLike extends Rank2IterableOnceLike {
	implicit def forOps[E, CC[A]/* <: IterableOnce[A]*/, C/* <: IterableOnce[E] with IterableOnceOps[E, CC, C]*/]
	                   (implicit specific :C <:< IterableOnce[E] with IterableOnceOps[E, CC, C], generic :CC <:?< IterableOnce)
			:IterableOnceLike[E, CC, C] =
		prototype.asInstanceOf[IterableOnceLike[E, CC, C]]

	private[this] val prototype = new IterableOnceLike.ForOps[Any, IterableOnce, Iterable[Any]] {
		private def readResolve :AnyRef = IterableOnceLike.forOps[Any, IterableOnce, Iterable[Any]]
		override def toString = "IterableOnceLike.forOps"
	}
}


@SerialVersionUID(Ver)
object IterableOnceLike extends Rank1IterableOnceLike {

	@inline final implicit def fromIterableLike[E, CC[_], C]
	                                           (implicit like :IterableLike[E, CC, C]) :IterableOnceLike[E, CC, C] =
		like

	/** An implementation of `IterableOnce` methods for any `IterableOnce[E]`. */
	trait ForIterableOnce[E] extends IterableOnceLike[E, IterableOnce, IterableOnce[E]] {
		override def knownSize(elems :IterableOnce[E]) :Int = elems.knownSize
		override def iterator(elems :IterableOnce[E]) :Iterator[E] = elems.iterator
		override def stepper[S <: Stepper[_]](elems :IterableOnce[E])(implicit shape :StepperShape[E, S]) :S =
			elems.stepper

		override def take(elems :IterableOnce[E])(n :Int) :IterableOnce[E] = elems.iterator.take(n)
		override def takeWhile(elems :IterableOnce[E])(p :E => Boolean) :IterableOnce[E] = elems.iterator.takeWhile(p)
		override def drop(elems :IterableOnce[E])(n :Int) :IterableOnce[E] = elems.iterator.drop(n)
		override def dropWhile(elems :IterableOnce[E])(p :E => Boolean) :IterableOnce[E] = elems.iterator.dropWhile(p)

		override def slice(elems :IterableOnce[E])(from :Int, until :Int) :IterableOnce[E] =
			elems.iterator.slice(from, until)

		override def span(elems :IterableOnce[E])(p :E => Boolean) :(IterableOnce[E], IterableOnce[E]) =
			elems.iterator.span(p)

		override def splitAt(elems :IterableOnce[E])(n :Int) :(IterableOnce[E], IterableOnce[E]) =
			elems.iterator.splitAt(n)

		override def scanLeft[A](elems :IterableOnce[E])(z :A)(op :(A, E) => A) :IterableOnce[A] =
			elems.iterator.scanLeft(z)(op)

		override def filter(elems :IterableOnce[E])(p :E => Boolean) :IterableOnce[E] = elems.iterator.filter(p)
		override def filterNot(elems :IterableOnce[E])(p :E => Boolean) :IterableOnce[E] = elems.iterator.filterNot(p)
		override def map[A](elems :IterableOnce[E])(f :E => A) :IterableOnce[A] = elems.iterator.map(f)
		override def flatMapIterableOnce[A](elems :IterableOnce[E])(f :E => IterableOnce[A]) :IterableOnce[A] =
			elems.iterator.flatMap(f)

		override def flatMap[O, A](elems :IterableOnce[E])(f :E => O)
		                          (implicit asIterableOnce :IterableOnceLike[A, funny.generic.Any1, O]) :IterableOnce[A] =
			elems.iterator.flatMap(e => asIterableOnce.toIterableOnce(f(e)))

		override def flatten[A, U >: E](elems :IterableOnce[E])
		                               (implicit iterableOnceLike :IterableOnceLike[A, funny.generic.Any1, U])
				:IterableOnce[A] =
			elems.iterator.flatten(iterableOnceLike.toIterableOnce)

		override def flattenIterableOnce[A](elems :IterableOnce[E])
		                                   (implicit asIterableOnce :E => IterableOnce[A]) :IterableOnce[A] =
			elems.iterator.flatten

		override def collect[A](elems :IterableOnce[E])(pf :PartialFunction[E, A]) :IterableOnce[A] =
			elems.iterator.collect(pf)

		override def zipWithIndex(elems :IterableOnce[E]) :IterableOnce[(E, Int)] = toIterableOnceOps(elems).zipWithIndex


		override def tapEach[U](elems :IterableOnce[E])(f :E => U) :IterableOnce[E] = toIterableOnceOps(elems).tapEach(f)

		override def toOps(elems :IterableOnce[E]) :IterableOnceOps[E, IterableOnce, IterableOnce[E]] =
			elems.iterator

		override def toIterableOnce(elems :IterableOnce[E]) :IterableOnce[E] = elems
		override def toIterableOnceOps(elems :IterableOnce[E]) :IterableOnceOps[E, IterableOnce, IterableOnce[E]] =
			elems.iterator

		implicit override def conversion :IterableOnce[E] => IterableOnce[E] = identity
	}


	/** An implementation of `IterableOnce` methods for any `C <: IterableOnceOps[E, CC, C]`. */
	trait ForOps[E, CC[A] <: IterableOnce[A], C <: CC[E] with IterableOnceOps[E, CC, C]]
		extends IterableOnceLike[E, CC, C]
	{
		override def knownSize(elems :C) :Int = elems.knownSize
		override def size(elems :C) :Int = elems.size
		override def isEmpty(elems :C) :Boolean = elems.isEmpty
		override def isTraversableAgain(elems :C) :Boolean = elems.isTraversableAgain

		override def iterator(elems :C) :Iterator[E] = elems.iterator
		override def stepper[S <: Stepper[_]](elems :C)(implicit shape :StepperShape[E, S]) :S = elems.stepper

		override def take(elems :C)(n :Int) :C = elems.take(n)
		override def takeWhile(elems :C)(p :E => Boolean) :C = elems.takeWhile(p)
		override def drop(elems :C)(n :Int) :C = elems.drop(n)
		override def dropWhile(elems :C)(p :E => Boolean) :C = elems.dropWhile(p)
		override def slice(elems :C)(from: Int, until: Int): C = elems.slice(from, until)
		override def span(elems :C)(p :E=> Boolean) :(C, C) = elems.span(p)
		override def splitAt(elems :C)(n :Int) :(C, C) = elems.splitAt(n)

		override def scanLeft[A](elems :C)(z :A)(op :(A, E) => A) :CC[A] = elems.scanLeft(z)(op)
		override def filter(elems :C)(p :E => Boolean) :C = elems.filter(p)
		override def filterNot(elems :C)(p :E => Boolean) :C = elems.filterNot(p)
		override def map[A](elems :C)(f :E => A): CC[A] = elems.map(f)
		override def flatMapIterableOnce[A](elems :C)(f :E => IterableOnce[A]) :CC[A] = elems.flatMap(f)
		override def flatMap[O, A](elems :C)(f :E => O)
		                          (implicit asIterableOnce :IterableOnceLike[A, funny.generic.Any1, O]) :CC[A] =
			elems.flatMap(e => asIterableOnce.toIterableOnce(f(e)))

		override def flattenIterableOnce[A](elems :C)(implicit asIterableOnce :E => IterableOnce[A]) :CC[A] =
			elems.flatten

		override def flatten[A, U >: E](elems :C)(implicit iterableOnceLike :IterableOnceLike[A, funny.generic.Any1, U]) :CC[A] =
			elems.flatten(iterableOnceLike.toIterableOnce)

		override def collect[A](elems :C)(pf: PartialFunction[E, A]) :CC[A] = elems.collect(pf)

		override def zipWithIndex(elems :C) :CC[(E @uncheckedVariance, Int)] = elems.zipWithIndex

		override def tapEach[U](elems :C)(f :E => U): C = elems.tapEach(f)
		override def foreach[U](elems :C)(f :E=> U): Unit = elems.foreach(f)

		override def forall(elems :C)(p :E => Boolean) :Boolean = elems.forall(p)
		override def exists(elems :C)(p :E => Boolean) :Boolean = elems.exists(p)
		override def count(elems :C)(p :E => Boolean) :Int = elems.count(p)
		override def find(elems :C)(p :E => Boolean) :Option[E] = elems.find(p)
		override def foldLeft[B](elems :C)(z :B)(op :(B ,E) => B) :B = elems.foldLeft(z)(op)
		override def foldRight[A](elems :C)(z :A)(op :(E, A) => A) :A = elems.foldRight(z)(op)
		override def reduceLeft[A >: E](elems :C)(op :(A, E) => A) :A = elems.reduceLeft(op)
		override def reduceRight[A >: E](elems :C)(op :(E, A) => A) :A = elems.reduceRight(op)
		override def reduceLeftOption[A >: E](elems :C)(op :(A, E) => A) :Option[A] = elems.reduceLeftOption(op)
		override def reduceRightOption[A >: E](elems :C)(op :(E, A) => A) :Option[A] = elems.reduceRightOption(op)

		override def copyToArray[B >: E](elems :C)(array :Array[B], start :Int = 0, max :Int = Int.MaxValue) :Int =
			elems.copyToArray(array, start, max)

		override def collectFirst[A](elems :C)(pf :PartialFunction[E, A]) :Option[A] = elems.collectFirst(pf)
		override def corresponds[A, CO[_], O](elems :C)(that :O)(p :(E, A) => Boolean)
		                                     (implicit iterableOnceLike :IterableOnceLike[A, CO, O]) :Boolean =
			elems.corresponds(iterableOnceLike.toIterableOnce(that))(p)

		override def addString(elems :C, b :StringBuilder, start :String, sep :String, end :String) :b.type =
			elems.addString(b, start, sep, end)

		override def to[C1](elems :C)(factory :Factory[E ,C1]) :C1 = factory.fromSpecific(elems)

		override def toOps(elems :C) :IterableOnceOps[E, CC, C] = elems
		override def toIterableOnce(elems :C) :IterableOnce[E] = elems
		override def toIterableOnceOps(elems :C) :IterableOnceOps[E, CC, C] = elems
		override def toList(elems :C): List[E] = elems.toList
		override def toVector(elems :C): Vector[E] = elems.toVector
		override def toMap[K, V](elems :C)(implicit ev: E <:< (K ,V)): Map[K, V] = elems.toMap
		override def toSet[A >: E](elems :C): Set[A] = elems.toSet
		override def toSeq(elems :C): Seq[E] = elems.toSeq
		override def toIndexedSeq(elems :C): IndexedSeq[E] = elems.toIndexedSeq
		override def toArray[A >: E: ClassTag](elems :C): Array[A] = elems.toArray

		implicit override def conversion :C => C = identity
	}

	//todo:
//	class ForStepper[E] extends IterableOnceLike[E, Stepper, Stepper[E]]
}
