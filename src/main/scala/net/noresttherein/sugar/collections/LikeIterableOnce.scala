package net.noresttherein.sugar.collections

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{Factory, IterableOnceOps, Stepper, StepperShape}
import scala.collection.mutable.{Builder, Buffer}
import scala.reflect.ClassTag

import net.noresttherein.sugar.arrays.ArrayLike
import net.noresttherein.sugar.casting.{castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.collections.LikeCollection.LikeCollectionBasics
import net.noresttherein.sugar.collections.LikeIterableOnce.FromIterableOnceOps
import net.noresttherein.sugar.collections.LikeIterableOnceSummons.{GenericSummoner, SpecificSummoner}
import net.noresttherein.sugar.collections.extensions.IterableOnceExtension
import net.noresttherein.sugar.reflect.extensions.classNameMethods
import net.noresttherein.sugar.typist.{<::<, =::=, kinds}
import net.noresttherein.sugar.typist.kinds.Any1
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




/** A type class providing main operations available in [[scala.collection.IterableOnceOps IterableOnceOps]] for type `C`.
  * This duplication allows several benefits:
  *   1. Polymorphism with types not implementing `IterableOnce`, for example `Array`, `String`, `Stepper`, etc.
  *   1. Treating one collection type like another: for example, a [[net.noresttherein.sugar.collections.Ranking Ranking]]
  *      shares features of both a `SetOps` and an `IndexedSeqOps`, but can implement neither interface
  *      due to type constraints and method conflicts. This allows all extension methods designed for sequences
  *      from this package work also for rankings.
  *   1. This polymorphism is achieved while preserving the represented type: mapping an `Array` or a `Ranking`
  *      will also produce an `Array`/`Ranking`, which is not the case with existing adapter collections.
  *   1. The type class imposes no bounds on its type parameter, and thus may extend several type classes
  *      in this type hierarchy, which the collection itself cannot. For example,
  *      [[net.noresttherein.sugar.collections.LikeRanking LikeRanking]] extends both
  *      [[net.noresttherein.sugar.collections.LikeSeq LikeSeq]] and
  *      [[net.noresttherein.sugar.collections.LikeSet LikeSet]].
  *
  * $semanticsDiffer
  *
  * Implicit instances exist for
  *   1. `IterableOnce` and `Iterator`,
  *   1. everything implementing `IterableOnceOps` - more specific than the above,
  *   1. all [[net.noresttherein.sugar.arrays.ArrayLike ArrayLike]] types and `String`,
  *   1. Java `Collection`, `Set`, `List`, `ArrayList`.
  * @tparam X  the type of the elements contained in the collection `Xs`.
  * @tparam Xs the collection-like type on which this type class operates, the argument to all its methods.
  * @tparam CC the type constructor for the generic version of the collection, applicable to any element type.
  * @tparam C  the type of collections built by methods taking elements only from the argument `Xs`;
  *            typically a supertype of `Xs`.
  *
  * @define Coll collection of the same kind
  * @define coll single use collection
  *
  * @define semanticsDiffer
  *
  *             Note: the exact semantics of methods building new collections may differ from those of the emulated $Coll.
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
  * @define undefinedOrder
  *              The order in which operations are performed on elements is unspecified
  *              and may be nondeterministic.
  */
//Consider: if X was invariant, we could define append/prepend etc to return C, not CC
trait LikeIterableOnce[+X, -Xs, +CC[_], +C] extends LikeCollection[X, Xs] {

	/** Applies a binary operator to all elements of `elems` and a start value, going right to left.
	  *
	  * $willNotTerminateInf
	  * $orderDependentFold
	  * @tparam A    the result type of the binary operator.
	  * @param elems a $coll.
	  * @param z     the start value.
	  * @param op    the binary operator.
	  * @return the result of inserting `op` between consecutive elements of `elems`, going right to left
	  *         with the start value `z` on the right: `op(x`,,1,,`, op(x`,,2,,`, ... op(x`,,n,,`, z)...))`
	  *         where `x`,,1,,`, ..., x`,,n,, are the elements of `elems`. Returns `z` if `elems` is empty.
	  */
	def foldRight[A](elems :Xs)(z :A)(op :(X, A) => A) :A

	/** Applies a binary operator to all elements of `elems`, going right to left.
	  * @tparam U    the result type of the binary operator.
	  * @param elems a $coll of this type class.
	  * @param op    the binary operator.
	  * @return the result of inserting `op` between consecutive elements of `elems`,
	  *         going right to left: `op(x,,1,,, op(x,,2,,, ..., op(x,,n-1,,, x,,n,,)...))` where `x,,1,,, ..., x,,n,,`
	  *         are the elements of `elems`.
	  * @throws UnsupportedOperationException if `elems` is empty.
	  */
	def reduceRight[U >: X](elems :Xs)(op :(X, U) => U) :U

	/** Optionally applies a binary operator to all elements of `elems`, going right to left.
	  *
	  * $willNotTerminateInf
	  * $orderDependentFold
	  * @tparam U    the result type of the binary operator.
	  * @param elems a $coll.
	  * @param op    a binary operator.
	  * @return an option value containing the result of `reduceRight(op)` if `elems` is nonempty,
	  *         `None` otherwise.
	  */
	def reduceRightOption[U >: X](elems :Xs)(op :(X, U) => U) :Option[U]


	/** Applies a side-effecting function to each element in this collection.
	  * Strict collections will apply `f` to their elements immediately, while lazy collections
	  * like Views and LazyLists will only apply `f` on each element if and when that element
	  * is evaluated, and each time that element is evaluated.
	  * @tparam U    the return type of f.
	  * @param elems a $coll.
	  * @param f     a function to apply to each element in `elems`.
	  * @return The same logical collection as this.
	  */
	def tapEach[U](elems :Xs)(f :X => U) :C

	def empty(elems :Xs) :C

	/** Selects the first ''n'' elements.
	  * $orderDependent
	  * @param elems a $coll.
	  * @param n     the number of elements to take from `elems`.
	  * @return a $coll consisting only of the first `n` elements of `elems`,
	  *         or else the whole $coll, if it has less than `n` elements.
	  *         If `n` is negative, returns an empty $coll.
	  */
	def take(elems :Xs, n :Int) :C

	/** Takes longest prefix of elements that satisfy a predicate.
	  * $orderDependent
	  * @param elems a $coll.
	  * @param p     The predicate used to test elements.
	  * @return the longest prefix of `elems` whose elements all satisfy
	  *         the predicate `p`.
	  */
	def takeWhile(elems :Xs)(p :X => Boolean) :C

	/** Selects all elements except first ''n'' ones.
	  * $orderDependent
	  * @param elems a $coll.
	  * @param n     the number of elements to drop from `elems`.
	  * @return a $coll consisting of all elements of `elems` except the first `n` ones, or else the
	  *         empty $coll, if `elems` has less than `n` elements.
	  *         If `n` is negative, don't drop any elements.
	  */
	def drop(elems :Xs, n :Int) :C

	/** Drops longest prefix of elements that satisfy a predicate.
	  * $orderDependent
	  * @param elems a $coll.
	  * @param p     The predicate used to test elements.
	  * @return the longest suffix of `elems` whose first element
	  *         does not satisfy the predicate `p`.
	  */
	def dropWhile(elems :Xs)(p :X => Boolean) :C

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
	def slice(elems :Xs, from: Int, until: Int) :C

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
	def span(elems :Xs)(p :X=> Boolean) :(C, C)

	/** Splits `elems` into a prefix/suffix pair at a given position.
	  * Note: `c splitAt n` is equivalent to (but possibly more efficient than)
	  *         `(c take n, c dro n)`.
	  * $orderDependent
	  * @param elems a $coll.
	  * @param n     the position at which to split.
	  * @return  a pair of ${coll}s consisting of the first `n`
	  *              elements of `elems`, and the other elements.
	  */
	def splitAt(elems :Xs, n :Int) :(C, C)

	/** Selects all elements of `elems` which satisfy a predicate.
	  * @param elems a $coll.
	  * @param p     the predicate used to test elements.
	  * @return a new $coll consisting of all elements of `elems` that satisfy the given
	  *         predicate `p`. The order of the elements is preserved.
	  */
	def filter(elems :Xs)(p :X => Boolean) :C

	/** Selects all elements of `elems` which do not satisfy a predicate.
	  * @param elems a $coll.
	  * @param p     the predicate used to test elements.
	  * @return a new $coll consisting of all elements of `elems` that do not satisfy the given
	  *         predicate `pred`. Their order may not be preserved.
	  */
	def filterNot(elems :Xs)(p :X => Boolean) :C


	/** Builds a new $coll by applying a function to all elements of `elems`.
	  * @tparam A    the element type of the returned $coll.
	  * @param elems a $coll.
	  * @param f     the function to apply to each element.
	  * @return      a new $coll resulting from applying the given function
	  *              `f` to each element of `elems` and collecting the results.
	  */
	def map[A](elems :Xs)(f :X => A) :CC[A]

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
	def flatMap[A, O](elems :Xs)(f :X => O)(implicit likeCollection :LikeCollection[A, O]) :CC[A]

	/** Builds a new $coll by applying a function to all elements of `elems`
	  * and using the elements of the resulting collections.
	  * @tparam A     the element type of the returned collection.
	  * @param elems  a $coll.
	  * @param f      the function to apply to each element.
	  * @return       a new $coll resulting from applying the given collection-valued function
	  *               `f` to each element of `elems` and concatenating the results.
	  */
	def flatMapIterableOnce[A](elems :Xs)(f :X => IterableOnce[A]) :CC[A]

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
	  * @param likeCollection a type class allowing to treat `elems` like `IterableOnce`.
	  * @return a new $coll resulting from concatenating all element ${coll}s.
	  */
	def flatten[A, U >: X](elems :Xs)(implicit likeCollection :LikeCollection[A, U]) :CC[A]

	def flattenIterableOnce[A](elems :Xs)(implicit asIterableOnce :X => IterableOnce[A]) :CC[A]

	/** Builds a new $coll by applying a partial function to all elements of `elems`
	  * on which the function is defined.
	  * @tparam A     the element type of the returned  $coll.
	  * @param elems  a $coll.
	  * @param pf     the partial function which filters and maps the $ ll.
	  * @return       a new $coll resulting from applying the given partial function
	  *               `pf` to each element on which it is defined and collecting the results.
	  *               The order of the elements is preserved.
	  */
	def collect[A](elems :Xs)(pf :PartialFunction[X, A]) :CC[A]

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
	def scanLeft[A](elems :Xs)(z :A)(op :(A, X) => A) :CC[A]

	/** Zips `elems` with its indices.
	  * @param elems a $coll.
	  * @return      A new $coll containing pairs consisting of all elements of `elems` paired with their index.
	  *              Indices sta  at `0`.
	  * @example
	  *    `List("a", "b", "c").zipWithIndex == List(("a", 0), ("b", 1), ("c", 2))`
	  */
	def zipWithIndex[U >: X](elems :Xs) :CC[(U, Int)]


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
	final def mkString(elems :Xs, start :String, sep :String, end :String) :String =
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
	@inline final def mkString(elems :Xs, sep :String) :String = mkString(elems, "", sep, "")

	/** Displays all elements of `elems` in a string. Delegates to `addString`, which can be overridden.
	  * @param elems a $coll.
	  * @return a string representation of `elems`. In the resulting string
	  *         the string representations (w.r.t. the method `toString`)
	  *         of all elements of `elems` follow each other without any
	  *         separator string.
	  */
	@inline final def mkString(elems :Xs) :String = mkString(elems, "")

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
	def addString(elems :Xs, b :StringBuilder, start :String, sep :String, end :String) :b.type =
		if (knownSize(elems) == 0) {
			val jsb = b.underlying
			if (start.length != 0) jsb append start
			if (end.length != 0) jsb append end
			b
		} else
			iterator(elems).addString(b, start, sep, end)

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
	@inline final def addString(elems :Xs, b :StringBuilder, sep :String) :b.type = addString(elems, b, "", sep, "")

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
	@inline final def addString(elems :Xs, b :StringBuilder) :b.type = addString(elems, b, "")


	/** Given a collection factory `factory`, convert this collection to the appropriate
	  * representation for the current element type `A`. Example uses:
	  * {{{
	  *      xs.to(List)
	  *      xs.to(ArrayBuffer)
	  *      xs.to(BitSet) // for xs: Iterable[Int]
	  * }}}
	  * @param elems a $coll.
	  */
	def to[C1](elems :Xs)(factory :Factory[X ,C1]) :C1

	/** Converts the enriched collection type to the generic result type of this type class.
	  * May return the same instance, even if it is mutable or otherwise linked to the argument, like an iterator.
	  */
	def toGeneric[U >: X](elems :Xs) :CC[U]

	/** Converts the enriched collection type to the specific result type of this type class.
	  * May return the same instance, even if it is mutable or otherwise linked to the argument, like an iterator.
	  */
	def toSpecific(elems :Xs) :C
//
//	/** This collection as `IterableOnceOps`. Note that it's different from
//	  * [[net.noresttherein.sugar.collections.LikeCollection.toIterableOnceOps toIterableOnceOps]] in that it guarantees
//	  * that the operations relate to the described collection type `C` itself,
//	  * rather than something else (for example, an iterator).
//	  */ //consider: moving this method to only FromOps, or at least not narrowing the return type in subclasses.
//	def toOps(elems :Xs) :IterableOnceOps[X, CC, C]

	/** Adapts this collection type to `IterableOnce[E]`.
	  * @param elems a $coll.
	  */
	override def toIterableOnce(elems :Xs) :IterableOnce[X]

	/** Converts this collection type to `Iterable`.
	  * For single-use collection this will copy the elements, but collections supporting
	  * [[net.noresttherein.sugar.collections.LikeIterableOnce.isTraversableAgain repeated]] iterations
	  * will return an adapter to `elems`.
	  * @note This will not copy mutable collections.
	  * @param elems a $coll.
	  */
	def toIterable(elems :Xs) :Iterable[X] = toIterableOnce(elems) match {
		case items :Iterable[X]                  => items
		case ArrayLike.Slice(array, from, until) => ArrayLikeSlice.slice(array, from, until)
		case items                               => items.iterator.toSeq
	}

	/** @return `elems` collection as a `List[A]`. This is equivalent to `to(elems)(List)` but might be faster. */
	def toList(elems :Xs): List[X] = List.from(toIterableOnce(elems))

	/** @return `elems` collection as a `Vector[A]`. This is equivalent to `to(elems)(Vector)` but might be faster. */
	def toVector(elems :Xs): Vector[X] = Vector.from(toIterableOnce(elems))

	/** @return `elems` collection as a `Seq[A]`. This is equivalent to `to(elems)(Seq)` but might be faster. */
	def toSeq(elems :Xs): Seq[X] = Seq.from(toIterableOnce(elems))

	/** @return `elems` collection as an `IndexedSeq[A]`. This is equivalent to `to(elms)(IndexedSeq)` but might be faster. */
	def toIndexedSeq(elems :Xs): IndexedSeq[X] = IndexedSeq.from(toIterableOnce(elems))

	@`inline` final def toBuffer[U >: X](elems :Xs): Buffer[U] = Buffer.from(toIterableOnce(elems))

	/** @return `elems` collection as a `Map[K, V]`. This is equivalent to `to(elems)(Map)` but might be faster. */
	def toMap[K, V](elems :Xs)(implicit ev: X <:< (K ,V)): Map[K, V] =
		Map.from(toIterableOnce(elems).asInstanceOf[IterableOnce[(K, V)]])

	/** @return `elems` collection as a `Set[A]`. This is equivalent to `to(elems)(Set)` but might be faster. */
	def toSet[U >: X](elems :Xs): Set[U] = Set.from(toIterableOnce(elems))

//	/** Convert collection to array. This will always create a new array, if `elems` already is an array. */
//	override def toArray[U >: X: ClassTag](elems :Xs): Array[U] = {
//		val size = knownSize(elems)
//		if (size >= 0) {
//			val destination = new Array[U](size)
//			copyToArray[U](elems)(destination, 0)
//			destination
//		} else
//			ArrayBuilder.make[U].addAll(toIterableOnce(elems)).result()
//	}
//	def toIArray[U >: X :ClassTag](elems :Xs) :IArray[U] = toArray[U](elems).castFrom[Array[U], IArray[U]]
//	def toRefArray[U >: X](elems :Xs) :RefArray[U] = toArray[Any](elems).castFrom[Array[Any], RefArray[U]]
//	def toIRefArray[U >: X](elems :Xs) :IRefArray[U] = toArray[Any](elems).castFrom[Array[Any], IRefArray[U]]

	override def infoString(elems :Xs) :String = {
		val n = knownSize(elems)
		if (n >= 0) elems.localClassName + "|" + n + "|"
		else elems.localClassName
	}

	override def moreSpecific(elems :Xs) :Maybe[LikeIterableOnce[X, elems.type, CC, C]] = No
	override def specific(elems :Xs) :LikeIterableOnce[X, elems.type, CC, C] = moreSpecific(elems) getOrElse this
}






private[collections] object LikeIterableOnceSummons {
	private[collections] sealed trait UntypedSummoner[T[_, _, CC[_], _], C] extends Any {
		@inline final def apply[E]()(implicit ops :T[E, C, Any1, _]) :ops.type = ops
	}
	private[collections] sealed trait SpecificSummoner[T[_, _, CC[_], _], C] extends Any {
		@inline final def apply[E, CC[_]]()(implicit ops :T[E, C, CC, C]) :ops.type = ops
	}
	private[collections] sealed trait GenericSummoner[T[_, _, cc[_], _], CC[_]] extends Any {
		@inline final def apply[E]()(implicit ops :T[E, CC[E], CC, CC[E]]) :ops.type = ops
	}
}


/** Base class for companion objects to [[net.noresttherein.sugar.collections.LikeIterableOnce LikeIterableOnce]]
  * sub type classes. Provides simplified methods for summoning implicit values with bounds for typical use cases,
  * as well as type aliases which can be used as view bounds.
  * @tparam LC a subtype of `LikeIterableOnce` which is being summoned.
  * @define TypeClass `LikeIterableOnce`
  */
private[collections] abstract class LikeIterableOnceSummons[LC[_, -_, +CC[_], +_]] {
	/** A type holder for type aliases to use as type class view bound on type parameters:
	  * {{{
	  *     def addAll[C :LikeSeq.of[Int]#Specific](coll :C) = ...
	  * }}}
	  * It is also useful for the common use cases when the type of produced collection does not matter,
	  * or is inherently linked to the argument collection type.
	  * {{{
	  *     def sameElements[U >: X, O :LikeSeq.of[U]#Coll](other :O) = ...
	  * }}}
	  * @tparam X the type of the elements in the collection.
	  */
	type of[X] = {
		/** A read-only type class representing `C` as a collection of `X`, without ability to create new collections
		  * of the same type.
		  */
		type Coll[C] = LikeSeq[X, C, Any1, _]

		/** A type class representing `C` as a collection of `X`, which may be used to produce other collections
		  * of the same type from its elements.
		  */
		type Specific[C] = LikeSeq[X, C, Any1, C]

		/** A type class for type constructor `C` representing `C[X]` as a generic collection type,
		  * to which elements can be added. This differs from `Specific[C]` in that the latter can potentially
		  * create collections of a type specific to `X`, like `String`, but only for elements coming from the source
		  * collection (or at least, the same type). On the other hand, this type class can be used to map or concat
		  * `C[X]` to other `C[O]`.
		  */
		type Generic[C[_]] = LikeSeq[X, C[X], C, C[X]]
	}

	/** Summons any implicit $TypeClass`[E, C, _, _]` - regardless of produced collection types. */
	@inline final def any[E, C](implicit ops :LC[E, C, Any1, _]) :ops.type = ops
	final def from[C](implicit ops :LC[Any, C, Any1, _]) :ops.type = ops

//	/** Followed by an application to an empty parameter group `()`, summons an implicit $TypeClass`[E, C, _, _]`,
//	  * where the element type `E` is inferred during the application.
//	  */
//	@inline final def any[C] :UntypedSummoner[LC, C] = new UntypedSummoner[LC, C] {}

	/** Summons an implicit $TypeClass`[E, C, CC, C]`. */
	@inline final def apply[E, CC[_], C](implicit ops :LC[E, C, CC, C]) :ops.type = ops

	/** Followed by an application to an empty parameter group `()`, summons an implicit $TypeClass`[E, C, CC, C]`,
	  * where the element type `E` and the type constructor `CC` are inferred during the application.
	  */
	@inline final def apply[C] :SpecificSummoner[LC, C] = new SpecificSummoner[LC, C] {}

	/** Summons an implicit $TypeClass`[E, CC[E], CC, CC[E]]`. */
	@inline final def generic[E, CC[_]](implicit ops :LC[E, CC[E], CC, CC[E]]) :ops.type = ops

	/** Followed by an application to an empty parameter group `()`, summons an implicit $TypeClass`[E, CC[E], CC, CC[E]]`,
	  * where the element type `E` is inferred during the application.
	  */
	@inline final def generic[CC[_]] :GenericSummoner[LC, CC] = new GenericSummoner[LC, CC] {}
}


private[collections] sealed abstract class Rank2LikeIterableOnces extends LikeIterableOnceSummons[LikeIterableOnce] {
//	implicit final def forIterableOnce[E] :LikeIterableOnce[E, IterableOnce[E], IterableOnce, IterableOnce[E]] =
//		prototype.castParam[E]
	//A complicated way of making it not more specific than forOps and forIterator.
	implicit final def forIterableOnce[E, CC[_]](implicit generic :CC =::= IterableOnce)
			:LikeIterableOnce[E, CC[E], CC, CC[E]] =
		prototype.asInstanceOf[LikeIterableOnce[E, CC[E], CC, CC[E]]]

	private[this] val prototype =
		new LikeIterableOnceForIterableOnce[Any]
			with HasMoreSpecificLikeIterableOnce[Any, IterableOnce[Any], IterableOnce, IterableOnce[Any]]
	{
		override def toString = "LikeIterableOnce.forIterableOnce"
		private def readResolve :AnyRef = LikeIterableOnce.forIterableOnce
	}
}


private[collections] sealed abstract class Rank1LikeIterableOnces extends Rank2LikeIterableOnces {
	//Xs parameter is needed so that the definition is not 'more specific' than likeIterable
	implicit final def forOps[X, Xs, CC[_], C]
	                         (implicit arg :Xs <:< C, specific :C <:< CC[X] with IterableOnceOps[X, CC, C],
	                                   generic :CC <::< IterableOnce) :LikeIterableOnce[X, Xs, CC, C] =
		prototype.asInstanceOf[LikeIterableOnce[X, Xs, CC, C]]

	//Because 'abstract member may not have a private modifier'.
	private object priv {
		type Ops[+E] <: IterableOnce[E] with IterableOnceOps[E, Ops, Ops[E]]
	}
	import priv.Ops

	private[this] val prototype :LikeIterableOnce[Any, Ops[Any], Ops, Ops[Any]] =
		new LikeIterableOnce.ForOps[Any, Ops, Ops[Any]]
			with HasMoreSpecificLikeIterableOnce[Any, Ops[Any], Ops, Ops[Any]]
		{
			override def toString = "LikeIterableOnce.forOps"
			override def toGeneric[U >: Any](elems :Ops[Any]) :Ops[U] = elems
			private def readResolve :AnyRef = LikeIterableOnce.forOps[Any, Ops[Any], Ops, Ops[Any]]
		}
}


@SerialVersionUID(Ver)
object LikeIterableOnce extends Rank1LikeIterableOnces {

	@inline implicit def likeIterable[X, Xs, CC[_], C](implicit like :LikeIterable[X, Xs, CC, C])
			:LikeIterableOnce[X, Xs, CC, C] =
		like

	implicit def forIterator[E, CC[_], C](implicit specific :C <:< Iterator[E] with IterableOnceOps[E, CC, C],
	                                               generic :CC <::< Iterator) :LikeIterableOnce[E, C, CC, C] =
		iteratorPrototype.asInstanceOf[LikeIterableOnce[E, C, CC, C]]

	private[this] val iteratorPrototype = new ForOps[Any, Iterator, Iterator[Any]] {
		override def toString = "LikeIterableOnce.forIterator"
		override def toGeneric[U >: Any](elems :Iterator[Any]) :Iterator[U] = elems
		private def readResolve :AnyRef = forIterator[Any, Iterator, Iterator[Any]]
	}


	def adapt[X, Xs](elems :Xs)(implicit likeIterableOnce :LikeIterableOnce[X, Xs, IterableOnce, IterableOnce[X]])
			:IterableOnceWithOps[X] =
		new LikeIterableOnceOpsAdapter[X, elems.type](elems)(likeIterableOnce.specific(elems))
//		new LikeCollectionAdapter[X, Xs](elems) with LikeIterableOnceAdapter[X, Xs, IterableOnce, IterableOnce[X]] {
//			override val ops = likeIterableOnce.specific(this.elems)
//		}


	/** Introduces ''abstract overrides'' for most of `LikeIterableOnce` methods, which first check if
	  * [[net.noresttherein.sugar.collections.LikeIterableOnce.moreSpecific moreSpecific]] is defined,
	  * and delegate to it over delegating to `super`.
	  */
	trait LikeMoreSpecific[+X, -Xs, +CC[_], +C]
		extends LikeIterableOnce[X, Xs, CC, C] with LikeCollection.LikeMoreSpecific[X, Xs]
	{
		abstract override def foldRight[A](elems :Xs)(z :A)(op :(X, A) => A) :A = moreSpecific(elems) match {
			case Yes(specific) => specific.foldRight(elems)(z)(op)
			case _             => super.foldRight(elems)(z)(op)
		}
		abstract override def reduceRight[A >: X](elems :Xs)(op :(X, A) => A) :A = moreSpecific(elems) match {
			case Yes(specific) => specific.reduceRight[A](elems)(op)
			case _             => super.reduceRight[A](elems)(op)
		}
		abstract override def reduceRightOption[A >: X](elems :Xs)(op :(X, A) => A) :Option[A] = moreSpecific(elems) match {
			case Yes(specific) => specific.reduceRightOption[A](elems)(op)
			case _             => super.reduceRightOption[A](elems)(op)
		}

		abstract override def tapEach[U](elems :Xs)(f :X => U) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.tapEach(elems)(f)
			case _             => super.tapEach(elems)(f)
		}
		abstract override def empty(elems :Xs) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.empty(elems)
			case _             => super.empty(elems)
		}
		abstract override def take(elems :Xs,  n :Int) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.take(elems, n)
			case _             => super.take(elems, n)
		}
		abstract override def takeWhile(elems :Xs)(p :X => Boolean) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.takeWhile(elems)(p)
			case _             => super.takeWhile(elems)(p)
		}
		abstract override def drop(elems :Xs, n :Int) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.drop(elems, n)
			case _             => super.drop(elems, n)
		}
		abstract override def dropWhile(elems :Xs)(p :X => Boolean) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.dropWhile(elems)(p)
			case _             => super.dropWhile(elems)(p)
		}
		abstract override def slice(elems :Xs, from :Int, until :Int) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.slice(elems, from, until)
			case _             => super.slice(elems, from, until)
		}
		abstract override def span(elems :Xs)(p :X => Boolean) :(C, C) = moreSpecific(elems) match {
			case Yes(specific) => specific.span(elems)(p)
			case _             => super.span(elems)(p)
		}
		abstract override def splitAt(elems :Xs, n :Int) :(C, C) = moreSpecific(elems) match {
			case Yes(specific) => specific.splitAt(elems, n)
			case _             => super.splitAt(elems, n)
		}
		abstract override def filter(elems :Xs)(p :X => Boolean) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.filter(elems)(p)
			case _             => super.filter(elems)(p)
		}
		abstract override def filterNot(elems :Xs)(p :X => Boolean) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.filterNot(elems)(p)
			case _             => super.filterNot(elems)(p)
		}

		abstract override def map[A](elems :Xs)(f :X => A) :CC[A] = moreSpecific(elems) match {
			case Yes(specific) => specific.map(elems)(f)
			case _             => super.map(elems)(f)
		}
		abstract override def flatMap[A, O](elems :Xs)(f :X => O)(implicit likeCollection :LikeCollection[A, O]) :CC[A] =
			moreSpecific(elems) match {
				case Yes(specific) => specific.flatMap(elems)(f)
				case _             => super.flatMap(elems)(f)
			}
		abstract override def flatMapIterableOnce[A](elems :Xs)(f :X => IterableOnce[A]) :CC[A] = moreSpecific(elems) match {
			case Yes(specific) => specific.flatMapIterableOnce(elems)(f)
			case _             => super.flatMapIterableOnce(elems)(f)
		}
		abstract override def flatten[A, U >: X](elems :Xs)(implicit likeCollection :LikeCollection[A, U]) :CC[A] =
			moreSpecific(elems) match {
				case Yes(specific) => specific.flatten(elems)
				case _             => super.flatten(elems)
			}
		abstract override def flattenIterableOnce[A](elems :Xs)(implicit asIterableOnce :X => IterableOnce[A]) :CC[A] =
			moreSpecific(elems) match {
				case Yes(specific) => specific.flattenIterableOnce(elems)
				case _             => super.flattenIterableOnce(elems)
			}
		abstract override def collect[A](elems :Xs)(pf :PartialFunction[X, A]) :CC[A] = moreSpecific(elems) match {
			case Yes(specific) => specific.collect(elems)(pf)
			case _             => super.collect(elems)(pf)
		}
		abstract override def scanLeft[A](elems :Xs)(z :A)(op :(A, X) => A) :CC[A] = moreSpecific(elems) match {
			case Yes(specific) => specific.scanLeft(elems)(z)(op)
			case _             => super.scanLeft(elems)(z)(op)
		}
		abstract override def zipWithIndex[U >: X](elems :Xs) :CC[(U, Int)] = moreSpecific(elems) match {
			case Yes(specific) => specific.zipWithIndex(elems)
			case _             => super.zipWithIndex(elems)
		}

		abstract override def to[C1](elems :Xs)(factory :Factory[X, C1]) :C1 = moreSpecific(elems) match {
			case Yes(specific) => specific.to(elems)(factory)
			case _             => super.to(elems)(factory)
		}
		abstract override def toGeneric[U >: X](elems :Xs) :CC[U] = moreSpecific(elems) match {
			case Yes(specific) => specific.toGeneric(elems)
			case _             => super.toGeneric(elems)
		}
		abstract override def toSpecific(elems :Xs) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.toSpecific(elems)
			case _             => super.toSpecific(elems)
		}

//		abstract override def toOps(elems :Xs) :IterableOnceOps[X, CC, C] = ???
//		abstract override def toIterableOnce(elems :Xs) :IterableOnce[X] = ???
	}



	/** Implements those methods of `LikeIterableOnce` which possible by delegating only to other methods
	  * of this interface, and not `toOps` or `iterator`.
	  * For a default implementation using `iterator(elems)`, see
	  * [[net.noresttherein.sugar.collections.LikeIterableOnce.FromIterator FromIterator]].
	  */
	trait LikeIterableOnceBasics[+X, -Xs, +CC[_], +C]
		extends LikeCollectionBasics[X, Xs] with LikeIterableOnce[X, Xs, CC, C]
	{
		override def reduceRightOption[U >: X](elems :Xs)(op :(X, U) => U) :Option[U] = knownSize(elems) match {
			case  0                   => None
			case -1 if isEmpty(elems) => None
			case  _                   => Some(reduceRight[U](elems)(op))
		}

		override def take(elems :Xs, n :Int) :C = slice(elems, 0, n)
		override def drop(elems :Xs, n :Int) :C = slice(elems, n, Int.MaxValue)
		override def splitAt(elems :Xs, n :Int) :(C, C) = (take(elems, n), drop(elems, n))

		override def filterNot(elems :Xs)(p :X => Boolean) :C = filter(elems)(!p(_))

		override def map[A](elems :Xs)(f :X => A) :CC[A] = collect(elems) { case elems => f(elems) }

		override def flatMap[A, O](elems :Xs)(f :X => O)(implicit likeCollection :LikeCollection[A, O]) :CC[A] =
			flatMapIterableOnce(elems)(f andThen likeCollection.conversion)

		override def flatten[A, U >: X](elems :Xs)(implicit likeCollection :LikeCollection[A, U]) :CC[A] =
			flattenIterableOnce(elems)(likeCollection.conversion)

		override def flattenIterableOnce[A](elems :Xs)(implicit asIterableOnce :X => IterableOnce[A]) :CC[A] =
			flatMap(elems)(asIterableOnce)
	}



	/** Implements `LikeIterableOnce` type class for `Xs` by delegating all methods to
	  * [[net.noresttherein.sugar.collections.LikeIterableOnce.toIterableOnce toIterableOnce]] and
	  * [[net.noresttherein.sugar.collections.LikeIterableOnce.toIterableOnceOps toIterableOnceOps]].
	  */
	sealed trait FromIterableOnce[+X, -Xs, +CC[_], +C]
		extends LikeIterableOnce[X, Xs, CC, C] with LikeCollection.FromIterableOnce[X, Xs]
	{ this :LikeCollection.FromIterableOnceSeal =>
		override def foldRight[A](elems :Xs)(z :A)(op :(X, A) => A) :A =
			if (knownSize(elems) == 0) z
			else delegateOps(elems).foldRight(z)(op)

		override def reduceRight[A >: X](elems :Xs)(op :(X, A) => A) :A = delegateOps(elems).reduceRight(op)
		override def reduceRightOption[A >: X](elems :Xs)(op :(X, A) => A) :Option[A] = knownSize(elems) match {
			case 0 => None
			case _ => Some(reduceRight[A](elems)(op))
		}

		override def flatMap[A, O](elems :Xs)(f :X => O)(implicit likeCollection :LikeCollection[A, O]) :CC[A] =
			flatMapIterableOnce(elems)(elem => likeCollection.toIterableOnce(f(elem)))

		override def flatten[A, U >: X](elems :Xs)(implicit likeCollection :LikeCollection[A, U]) :CC[A] =
			flattenIterableOnce(elems)(likeCollection.conversion)

		override def to[C1](elems :Xs)(factory :Factory[X ,C1]):C1 = factory.fromSpecific(toIterableOnce(elems))
	}



	sealed trait FromIterableOnceOps[+X, -Xs, +CC[_], +C]
		extends FromIterableOnce[X, Xs, CC, C]
	{ this :LikeCollection.FromIterableOnceSeal =>
		override def tapEach[U](elems :Xs)(f :X => U) :C = toOps(elems).tapEach(f)

		override def empty(elems :Xs) :C = toOps(elems).take(0)
		override def take(elems :Xs, n :Int) :C = knownSize(elems) match {
			case  -1          => toOps(elems).take(n)
			case  k if k >= n => toSpecific(elems)
			case  _           => toOps(elems).take(n)
		}
		override def drop(elems :Xs, n :Int) :C = toOps(elems).drop(n)
		override def slice(elems :Xs, from :Int, until :Int) :C = toOps(elems).slice(from, until)
		override def splitAt(elems :Xs, n :Int) :(C, C) = toOps(elems).splitAt(n)
		override def takeWhile(elems :Xs)(p :X => Boolean) :C = toOps(elems).takeWhile(p)
		override def dropWhile(elems :Xs)(p :X => Boolean) :C = toOps(elems).dropWhile(p)
		override def span(elems :Xs)(p :X => Boolean) :(C, C) = toOps(elems).span(p)

		override def filter(elems :Xs)(p :X => Boolean) :C = toOps(elems).filter(p)
		override def filterNot(elems :Xs)(p :X => Boolean) :C = toOps(elems).filterNot(p)

		override def map[A](elems :Xs)(f :X => A) :CC[A] = toOps(elems).map(f)

		override def flatMapIterableOnce[A](elems :Xs)(f :X => IterableOnce[A]) :CC[A] =
			toOps(elems).flatMap(f)

		override def flattenIterableOnce[A](elems :Xs)(implicit asIterableOnce :X => IterableOnce[A]) :CC[A] =
			toOps(elems).flatten

		override def collect[A](elems :Xs)(pf :PartialFunction[X, A]) :CC[A] = toOps(elems).collect(pf)
		override def scanLeft[A](elems :Xs)(z :A)(op :(A, X) => A) :CC[A] = toOps(elems).scanLeft(z)(op)

		override def zipWithIndex[U >: X](elems :Xs) :CC[(U, Int)] =
			(this :FromIterableOnceOps[U, Xs, CC, C]).toOps(elems).zipWithIndex

		/** This collection as `IterableOnceOps`. Note that it's different from
		  * [[net.noresttherein.sugar.collections.LikeCollection.toIterableOnceOps toIterableOnceOps]] in that it guarantees
		  * that the operations relate to the described collection type `C` itself,
		  * rather than something else (for example, an iterator).
		  */
		def toOps(elems :Xs) :IterableOnceOps[X, CC, C]

	}


	/** An implementation of type class [[net.noresttherein.sugar.collections.LikeIterableOnce LikeIterableOnce]]
	  * for a collection type `Xs` of `X` values which delegates all calls to
	  * [[net.noresttherein.sugar.collections.LikeIterableOnce.FromOps.toOps toOps]]`(elems)`,
	  * returning the standard Scala `IterableOnceOps[X, CC, C]` interface for `elems`.
	  */
	trait FromOps[+X, -Xs, +CC[_], +C]
		extends FromIterableOnceOps[X, Xs, CC, C] with LikeCollection.FromIterableOnceOps[X, Xs]


	/** An implementation of `LikeIterableOnce` methods for any `C <: IterableOnceOps[E, CC, C]`. */
	trait ForOps[E, CC[A] <: IterableOnce[A], C <: CC[E] with IterableOnceOps[E, CC, C]]
		extends FromOps[E, C, CC, C] with LikeCollection.ForIterableOnce[E, C]
	{
		override def toSpecific(elems :C) :C = elems
		override def toOps(elems :C) :IterableOnceOps[E, CC, C] = elems
//		override def toIterableOnce(elems :C) :IterableOnce[E] = elems
		override def toIterableOnceOps(elems :C) :IterableOnceOps[E, CC, C] = elems
		override def toList(elems :C): List[E] = elems.toList
		override def toVector(elems :C): Vector[E] = elems.toVector
		override def toMap[K, V](elems :C)(implicit ev: E <:< (K ,V)): Map[K, V] = elems.toMap
		override def toSet[A >: E](elems :C): Set[A] = elems.toSet
		override def toSeq(elems :C): Seq[E] = elems.toSeq
		override def toIndexedSeq(elems :C): IndexedSeq[E] = elems.toIndexedSeq
		override def toArray[A >: E: ClassTag](elems :C): Array[A] = elems.toArray

		override def iterator(elems :C) :Iterator[E] = elems.iterator
		override def stepper[S <: Stepper[_]](elems :C)(implicit shape :StepperShape[E, S]) :S = elems.stepper

		implicit override val conversion :C => C = identity
	}



	/** Root trait of a hierarchy of direct `LikeIterableOnce` implementations, which do not delegate
	  * to [[net.noresttherein.sugar.collections.LikeIterableOnce.toIterableOnceOps toIterableOnceOps]] or
	  * to [[net.noresttherein.sugar.collections.LikeIterableOnce.FromOps.toOps toOps]].
	  * Declares methods for building the generic collection type `CC[Y]` for arbitrary types `Y`
	  * and the specific collection type `C`.
	  */
	trait LikeIterableOnceFactory[X, -Xs, +CC[_], +C] extends LikeIterableOnce[X, Xs, CC, C] {
		def makeSpecific(elems :Xs)(coll :IterableOnce[X]) :C
		def makeGeneric[A](elems :Xs)(coll :IterableOnce[A]) :CC[A]
	}

	/** A mixin `LikeIterableOnceFactory` implementing
	  * [[net.noresttherein.sugar.collections.LikeIterableOnce.GenericLikeIterableOnce.makeSpecific makeSpecific]]
	  * with [[net.noresttherein.sugar.collections.LikeIterableOnce.GenericLikeIterableOnce.makeGeneric makeGeneric]],
	  * leaving only the latter to implement by subclasses in order to be able to implement all methods of
	  * [[net.noresttherein.sugar.collections.LikeIterableOnce LikeIterableOnce]] which return another collection.
	  */
	trait GenericLikeIterableOnce[X, -Xs, +CC[_]] extends LikeIterableOnceFactory[X, Xs, CC, CC[X]] {
		override def makeSpecific(elems :Xs)(coll :IterableOnce[X]) :CC[X] = makeGeneric(elems)(coll)
	}


	/** Root trait of a hierarchy of direct `LikeIterableOnce` implementations for strict collection types,
	  * which builds the returned collection using a builder. It does not delegate
	  * to [[net.noresttherein.sugar.collections.LikeIterableOnce.toIterableOnceOps toIterableOnceOps]] or
	  * to [[net.noresttherein.sugar.collections.LikeIterableOnce.FromOps.toOps toOps]].
	  */
	trait LikeIterableOnceBuilder[X, -Xs, +CC[_], +C] extends LikeIterableOnce[X, Xs, CC, C] {
		def specificBuilder(elems :Xs) :Builder[X, C]
		def genericBuilder[A](elems :Xs) :Builder[A, CC[A]]
	}

	/** A mixin `LikeIterableOnceBuilder` implementing
	  * [[net.noresttherein.sugar.collections.LikeIterableOnce.GenericLikeIterableOnceBuilder.specificBuilder specificBuilder]]
	  * with [[net.noresttherein.sugar.collections.LikeIterableOnce.GenericLikeIterableOnceBuilder.genericBuilder genericBuilder]],
	  * leaving only the latter to implement by subclasses in order to be able to implement all methods of
	  * [[net.noresttherein.sugar.collections.LikeIterableOnce LikeIterableOnce]] which return another collection.
	  */
	trait GenericLikeIterableOnceBuilder[X, -Xs, +CC[_]] extends LikeIterableOnceBuilder[X, Xs, CC, CC[X]] {
		override def specificBuilder(elems :Xs) :Builder[X, CC[X]] = genericBuilder(elems)
	}


	/** An implementation of [[net.noresttherein.sugar.collections.LikeIterableOnce LikeIterableOnce]] type class
	  * for type `Xs` with elements of `X` implementing all the methods in terms of
	  * [[net.noresttherein.sugar.collections.LikeIterableOnce.iterator iterator]] and
	  * [[net.noresttherein.sugar.collections.LikeIterableOnce.knownSize knownSize]],
	  * building returned collections with type class specific methods
	  * [[net.noresttherein.sugar.collections.LikeIterableOnce.LikeIterableOnceFactory.makeSpecific makeSpecific]] and
	  * [[net.noresttherein.sugar.collections.LikeIterableOnce.LikeIterableOnceFactory.makeGeneric makeGeneric]].
	  */
	trait FromIterator[X, -Xs, +CC[_], +C]
		extends LikeCollection.FromIterator[X, Xs]
		   with FromIterableOnce[X, Xs, CC, C] with LikeIterableOnceFactory[X, Xs, CC, C]
	{
		override def corresponds[A, O](elems :Xs,that :O)(p :(X, A) => Boolean)
		                              (implicit likeCollection :LikeCollection[A, O]) :Boolean =
		{
			val thisSize = knownSize(elems)
			val thatSize = likeCollection.knownSize(that)
			(thisSize == -1 | thatSize == -1 | thisSize == thatSize) && (
				thisSize == 0 & thatSize == 0 || {
					val i1 = iterator(elems)
					val i2 = likeCollection.iterator(that)
					while (i1.hasNext && i2.hasNext)
						p(i1.next(), i2.next())
					!i1.hasNext && !i2.hasNext
				}
			)
		}

		override def empty(elems :Xs) :C = makeSpecific(elems)(Iterator.empty)
		override def take(elems :Xs, n :Int) :C = makeSpecific(elems)(iterator(elems).take(n))
		override def takeWhile(elems :Xs)(p :X => Boolean) :C = makeSpecific(elems)(iterator(elems).takeWhile(p))
		override def drop(elems :Xs, n :Int) :C = makeSpecific(elems)(iterator(elems).drop(n))
		override def dropWhile(elems :Xs)(p :X => Boolean) :C = makeSpecific(elems)(iterator(elems).dropWhile(p))
		override def slice(elems :Xs, from :Int, until :Int) :C = makeSpecific(elems)(iterator(elems).slice(from, until))
		override def span(elems :Xs)(p :X => Boolean) :(C, C) = {
			val (prefix, suffix) = iterator(elems).span(p)
			(makeSpecific(elems)(prefix), makeSpecific(elems)(suffix))
		}
		override def splitAt(elems :Xs, n :Int) :(C, C) = {
			val (prefix, suffix) = iterator(elems).splitAt(n)
			(makeSpecific(elems)(prefix), makeSpecific(elems)(suffix))
		}
		override def filter(elems :Xs)(p :X => Boolean) :C = makeSpecific(elems)(iterator(elems).filter(p))
		override def filterNot(elems :Xs)(p :X => Boolean) :C = makeSpecific(elems)(iterator(elems).filterNot(p))

		override def map[A](elems :Xs)(f :X => A) :CC[A] = makeGeneric(elems)(iterator(elems).map(f))
		override def flatMapIterableOnce[A](elems :Xs)(f :X => IterableOnce[A]) :CC[A] =
			makeGeneric(elems)(iterator(elems).flatMap(f))

		override def flattenIterableOnce[A](elems :Xs)(implicit asIterableOnce :X => IterableOnce[A]) :CC[A] =
			makeGeneric(elems)(iterator(elems).flatten)

		override def collect[A](elems :Xs)(pf :PartialFunction[X, A]) :CC[A] =
			makeGeneric(elems)(iterator(elems).collect(pf))

		override def scanLeft[A](elems :Xs)(z :A)(op :(A, X) => A) :CC[A] =
			makeGeneric(elems)(iterator(elems).scanLeft(z)(op))

		override def zipWithIndex[U >: X](elems :Xs) :CC[(U, Int)] = makeGeneric(elems)(iterator(elems).zipWithIndex)

		override def toSpecific(elems :Xs) :C = makeSpecific(elems)(toIterableOnce(elems))
		override def toGeneric[U >: X](elems :Xs) :CC[U] = makeGeneric[U](elems)(toIterableOnce(elems))
	}
}






private trait HasMoreSpecificLikeIterableOnce[+X, -Xs <: IterableOnce[X], +CC[_], +C]
	extends LikeIterableOnce[X, Xs, CC, C]
{
	override def moreSpecific(elems :Xs) :Maybe[LikeIterableOnce[X, elems.type, CC, C]] =
		(elems match {
			case items :Iterable[X] => Yes(LikeIterable.forOps[X, Iterable[X], Iterable, Iterable[X]].specific(items))
			case items :Iterator[X] => Yes(LikeIterableOnce.forIterator[X, Iterator, Iterator[X]].specific(items))
			case _ => No
		}).asInstanceOf[Maybe[LikeIterableOnce[X, elems.type, CC, C]]]
}




/** An implementation of `LikeIterableOnce` methods for any `IterableOnce[E]`.
  * They mostly check `knownSize` for a fast path and then delegate to
  * [[net.noresttherein.sugar.collections.LikeCollection.toIterableOnceOps toIterableOnceOps]]`(elems)`,
  * which in practice means either casting `elems` to `Iterable`, or calling `elems.iterator`.
  */
@SerialVersionUID(Ver)
private class LikeIterableOnceForIterableOnce[X]
	extends FromIterableOnceOps[X, IterableOnce[X], IterableOnce, IterableOnce[X]]
	   with LikeCollection.FromIterator[X, IterableOnce[X]]
	   with HasMoreSpecificLikeIterableOnce[X, IterableOnce[X], IterableOnce, IterableOnce[X]]
{
	override def empty(elems :IterableOnce[X]) :IterableOnce[X] = Iterator.empty


	override def toOps(elems :IterableOnce[X]) :IterableOnceOps[X, IterableOnce, IterableOnce[X]] =
		elems.toIterableOnceOps

	override def toIterableOnceOps(elems :IterableOnce[X]) :IterableOnceOps[X, kinds.Any1, Any] = elems.toBasicOps

	override def toIterableOnce(elems :IterableOnce[X]) :IterableOnce[X] = elems
	override def toGeneric[U >: X](elems :IterableOnce[X]) :IterableOnce[U] = elems
	override def toSpecific(elems :IterableOnce[X]) :IterableOnce[X] = elems
	override def iterator(elems :IterableOnce[X]) :Iterator[X] = elems.iterator
	override def stepper[S <: Stepper[_]](elems :IterableOnce[X])(implicit shape :StepperShape[X, S]) :S =
		elems.stepper

	implicit override val conversion :IterableOnce[X] => IterableOnce[X] = identity
}






private trait LikeIterableOnceAdapter[+X, Xs, +CC[_], +C] extends IterableOnce[X] with IterableOnceOps[X, CC, C] {
	protected val elems :Xs
//	protected val ops :LikeIterableOnce[X, Xs, CC, C]
	protected val ops :LikeIterableOnce[X, Xs, Any1, _]

	override def isTraversableAgain :Boolean = ops.isTraversableAgain(elems)
	override def knownSize :Int = ops.knownSize(elems)
	override def size :Int = ops.size(elems)
	override def isEmpty :Boolean = ops.isEmpty(elems)

	override def forall(p :X => Boolean) :Boolean = ops.forall(elems)(p)
	override def exists(p :X => Boolean) :Boolean = ops.exists(elems)(p)
	override def count(p :X => Boolean) :Int = ops.count(elems)(p)
	override def find(p :X => Boolean) :Option[X] = ops.find(elems)(p)
	override def collectFirst[A](pf :PartialFunction[X, A]) :Option[A] = ops.collectFirst(elems)(pf)
	override def foldLeft[A](z :A)(op :(A, X) => A) :A = ops.foldLeft(elems)(z)(op)
	override def reduceLeft[U >: X](op :(U, X) => U) :U = ops.reduceLeft[U](elems)(op)
	override def reduceLeftOption[U >: X](op :(U, X) => U) :Option[U] = ops.reduceLeftOption[U](elems)(op)
	override def foldRight[B](z :B)(op :(X, B) => B) = ops.foldRight(elems)(z)(op)

	override def corresponds[A](that :IterableOnce[A])(p :(X, A) => Boolean) :Boolean = ops.corresponds(elems, that)(p)

	override def foreach[U](f :X => U) :Unit = ops.foreach(elems)(f)

/*
	override def tapEach[U](f :X => U) :C = ops.tapEach(elems)(f)

	override def take(n :Int) :C = ops.take(elems)(n)
	override def takeWhile(p :X => Boolean) :C = ops.takeWhile(elems)(p)
	override def drop(n :Int) :C = ops.drop(elems)(n)
	override def dropWhile(p :X => Boolean) :C = ops.dropWhile(elems)(p)
	override def slice(from: Int, until: Int) :C = ops.slice(elems)(from, until)
	override def span(p :X => Boolean) :(C, C) = ops.span(elems)(p)
	override def splitAt(n :Int) :(C, C) = ops.splitAt(elems)(n)

	override def filter(p :X => Boolean) :C = ops.filter(elems)(p)
	override def filterNot(p :X => Boolean) :C = ops.filterNot(elems)(p)

	override def map[A](f :X => A) :CC[A] = ops.map(elems)(f)
	override def flatMap[A](f :X => IterableOnce[A]) :CC[A] = ops.flatMapIterableOnce(elems)(f)
	override def flatten[A](implicit asIterableOnce :X => IterableOnce[A]) :CC[A] = ops.flattenIterableOnce(elems)
	override def collect[A](pf :PartialFunction[X, A]) :CC[A] = ops.collect(elems)(pf)
	override def scanLeft[A](z :A)(op :(A, X) => A) :CC[A] = ops.scanLeft(elems)(z)(op)
	override def zipWithIndex :CC[(X @uncheckedVariance, Int)] = ops.zipWithIndex(elems)
*/

	override def copyToArray[A >: X](array :Array[A], start :Int, max :Int) :Int =
		ops.copyToArray[A](elems, array, start, max)

	override def iterator :Iterator[X] = ops.iterator(elems)
	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[X, S]) :S = ops.stepper(elems)

	override def to[O](factory :Factory[X ,O]) :O = ops.to(elems)(factory)
	override def toList: List[X] = ops.toList(elems)
	override def toVector: Vector[X] = ops.toVector(elems)
	override def toSeq: Seq[X] = ops.toSeq(elems)
	override def toIndexedSeq: IndexedSeq[X] = ops.toIndexedSeq(elems)
	override def toMap[K, V](implicit ev: X <:< (K ,V)): Map[K, V] = ops.toMap(elems)
	override def toSet[U >: X]: Set[U] = ops.toSet[U](elems)
	override def toArray[U >: X: ClassTag]: Array[U] = ops.toArray[U](elems)
}


@SerialVersionUID(Ver)
private class LikeIterableOnceOpsAdapter[+X, Xs]
              (xs :Xs)(implicit protected override val ops :LikeIterableOnce[X, Xs, IterableOnce, IterableOnce[X]])
	extends LikeCollectionAdapter[X, Xs](xs) with LikeIterableOnceAdapter[X, Xs, IterableOnce, IterableOnce[X]]
{
	override def tapEach[U](f :X => U) :IterableOnce[X] = ops.tapEach(elems)(f)

	override def take(n :Int) :IterableOnce[X] = ops.take(elems, n)
	override def takeWhile(p :X => Boolean) :IterableOnce[X] = ops.takeWhile(elems)(p)
	override def drop(n :Int) :IterableOnce[X] = ops.drop(elems, n)
	override def dropWhile(p :X => Boolean) :IterableOnce[X] = ops.dropWhile(elems)(p)
	override def slice(from: Int, until: Int) :IterableOnce[X] = ops.slice(elems, from, until)
	override def span(p :X => Boolean) :(IterableOnce[X], IterableOnce[X]) = ops.span(elems)(p)
	override def splitAt(n :Int) :(IterableOnce[X], IterableOnce[X]) = ops.splitAt(elems, n)

	override def filter(p :X => Boolean) :IterableOnce[X] = ops.filter(elems)(p)
	override def filterNot(p :X => Boolean) :IterableOnce[X] = ops.filterNot(elems)(p)

	override def map[A](f :X => A) :IterableOnce[A] = ops.map(elems)(f)
	override def flatMap[A](f :X => IterableOnce[A]) :IterableOnce[A] = ops.flatMapIterableOnce(elems)(f)
	override def flatten[A](implicit asIterableOnce :X => IterableOnce[A]) :IterableOnce[A] = ops.flattenIterableOnce(elems)
	override def collect[A](pf :PartialFunction[X, A]) :IterableOnce[A] = ops.collect(elems)(pf)
	override def scanLeft[A](z :A)(op :(A, X) => A) :IterableOnce[A] = ops.scanLeft(elems)(z)(op)
	override def zipWithIndex :IterableOnce[(X @uncheckedVariance, Int)] = ops.zipWithIndex(elems)
}
