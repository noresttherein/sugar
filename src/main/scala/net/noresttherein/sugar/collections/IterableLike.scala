package net.noresttherein.sugar.collections

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable.HashMap
import scala.collection.{IterableFactory, IterableOnceOps, IterableOps, LazyZip2, View, WithFilter, mutable}
import scala.collection.mutable.Builder

import net.noresttherein.sugar.extensions.{IterableExtension, IteratorExtension, cast3TypeParamsMethods}
import net.noresttherein.sugar.funny
import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.typist.<:?<




/** A type class providing operations available in [[collection.Iterable Iterable]] for type `C`.
  * @tparam E  the element type.
  * @tparam CC the type constructor for the generic version of the collection, applicable to any element type.
  * @tparam C  the collection type
  * @define Coll `Iterable`
  * @define coll iterable collection
  */
trait IterableLike[+E, +CC[_], C] extends IterableOnceLike[E, CC, C] {
	override def isTraversableAgain(elems :C) :Boolean = true

	/** The companion object of `elems`, providing various factory methods.
	  * @note When implementing a custom collection type and refining `CC` to the new type, this
	  *       method needs to be overridden to return a factory for the new type (the compiler will
	  *       issue an error otherwise).
	  * @param elems a $coll.
	  */
	def iterableFactory(elems :C) :IterableFactory[CC] = toOps(elems).iterableFactory

	/**
	  * Defines how to turn a given `Iterable[A]` into a collection of type `C`.
	  *
	  * This process can be done in a strict way or a non-strict way (ie. without evaluating
	  * the elements of the resulting collections). In other words, this methods defines
	  * the evaluation model of the collection.
	  *
	  * @note When implementing a custom collection type and refining `C` to the new type, this
	  *       method needs to be overridden (the compiler will issue an error otherwise). In the
	  *       common case where `C =:= CC[A]`, this can be done by mixing in the
	  *       [[scala.collection.IterableFactoryDefaults]] trait, which implements the method using
	  *       [[iterableFactory]].
	  *
	  * @note As witnessed by the `@uncheckedVariance` annotation, using this method
	  *       might be unsound. However, as long as it is called with an
	  *       `Iterable[A]` obtained from `this` collection (as it is the case in the
	  *       implementations of operations where we use a `View[A]`), it is safe.
	  * @param elems a $coll.
	  */
	protected def fromSpecific(elems :C)(coll :IterableOnce[E @uncheckedVariance]) :C =
		util.fromSpecific(toOps(elems))(coll)

	/** @return a strict builder for the same collection type.
	  *
	  *         Note that in the case of lazy collections (e.g. [[scala.collection.View]] or [[scala.collection.immutable.LazyList]]),
	  *         it is possible to implement this method but the resulting `Builder` will break laziness.
	  *         As a consequence, operations should preferably be implemented with `fromSpecific`
	  *         instead of this method.
	  *
	  * @note When implementing a custom collection type and refining `C` to the new type, this
	  *       method needs to be overridden (the compiler will issue an error otherwise). In the
	  *       common case where `C =:= CC[A]`, this can be done by mixing in the
	  *       [[scala.collection.IterableFactoryDefaults]] trait, which implements the method using
	  *       [[iterableFactory]].
	  *
	  * @note As witnessed by the `@uncheckedVariance` annotation, using this method might
	  *       be unsound. However, as long as the returned builder is only fed
	  *       with `A` values taken from `this` instance, it is safe.
	  * @param elems a $coll.
	  */
	protected def newSpecificBuilder(elems :C) :Builder[E @uncheckedVariance, C] = util.specificBuilder(toOps(elems))

	/** The empty iterable of the same type as this iterable
	  * @param elems a $coll.
	  * @return an empty iterable of type `C`.
	  */
	def empty(elems :C) :C = fromSpecific(elems)(Nil)

	/** Selects the first element of `elems`.
	  * $orderDependent
	  * @param elems a $coll.
	  * @return the first element of `elems`.
	  * @throws NoSuchElementException if the $coll is empty.
	  */
	def head(elems :C) :E = iterator(elems).next()

	/** Optionally selects the first element.
	  * $orderDependent
	  * @param elems a $coll.
	  * @return the first element of `elems` if it is nonempty, `None` if it is empty.
	  */
	def headOption(elems :C) :Option[E] = knownSize(elems) match {
		case -1 =>
			val it = iterator(elems)
			if (it.hasNext) Some(it.next()) else None
		case 0 => None
		case _ => Some(iterator(elems).next())
	}

	/** Selects the last element.
	  * $orderDependent
	  * @param elems a $coll.
	  * @return The last element of `elems`.
	  * @throws NoSuchElementException If the $coll is empty.
	  */
	def last(elems :C) :E = last(iterator(elems))

	/** Optionally selects the last element.
	  * $orderDependent
	  * @param elems a $coll.
	  * @return the last element of `elems`$ if it is nonempty, `None` if it is empty.
	  */
	def lastOption(elems :C) :Option[E] = knownSize(elems) match {
		case -1 =>
			val iter = iterator(elems)
			if (!iter.hasNext) None else Some(last(iter))
		case 0 => None
		case _ => Some(last(iterator(elems)))
	}

	private[this] def last(iter :Iterator[E]) :E = {
		var lst = iter.next()
		while (iter.hasNext) lst = iter.next()
		lst
	}

	/** Returns a value class containing operations for comparing the size of `elems` to a test value.
	  *
	  * These operations are implemented in terms of [[sizeCompare(Int) `sizeCompare(Int)`]], and
	  * allow the following more readable usages:
	  * {{{
	  *     this.sizeIs < size     // this.sizeCompare(size) < 0
	  *     this.sizeIs <= size    // this.sizeCompare(size) <= 0
	  *     this.sizeIs == size    // this.sizeCompare(size) == 0
	  *     this.sizeIs != size    // this.sizeCompare(size) != 0
	  *     this.sizeIs >= size    // this.sizeCompare(size) >= 0
	  *     this.sizeIs > size     // this.sizeCompare(size) > 0
	  * }}}
	  * @param elems a $coll.
	  */
	def sizeIs(elems :C) :IterableOps.SizeCompareOps = toOps(elems).sizeIs

	/** Compares the size of `elems` to a test value.
	  * @param elems     a $coll.
	  * @param otherSize the test value that gets compared with the size.
	  * @return A value `x` where
	  * {{{
	  *        x <  0       if this.size <  otherSize
	  *        x == 0       if this.size == otherSize
	  *        x >  0       if this.size >  otherSize
	  * }}}
	  * The method as implemented here does not call `size` directly; its running time
	  * is `O(size min otherSize)` instead of `O(size)`. The method should be overridden
	  * if computing `size` is cheap and `knownSize` returns `-1`.
	  * @see [[sizeIs]]
	  */
	def sizeCompare(elems :C, otherSize :Int) :Int = {
		val size = this.knownSize(elems)
		if (size >= 0) java.lang.Integer.compare(size, otherSize)
		else toOps(elems).sizeCompare(otherSize)
	}

	/** Compares the size of `elems` to the size of another `Iterable`.
	  * @param elems a $coll.
	  * @param that  the `Iterable` whose size is compared with `elems`'s size.
	  * @return  A value `x` where
	  *          {{{
	  *             x <  0       if this.size <  that.size
	  *             x == 0       if this.size == that.size
	  *             x >  0       if this.size >  that.size
	  *          }}}
	  *          The method as implemented here does not call `size` directly; its running time
	  *          is `O(this.size min that.size)` instead of `O(this.size + that.size)`.
	  *          The method should be overridden if computing `size` is cheap and `knownSize` returns `-1`.
	  */
	def sizeCompare[O](elems :C, that :O)(implicit collection :IterableLike[_, generic.Any, O]) :Int = {
		val thatKnownSize = collection.knownSize(that)
		if (thatKnownSize >= 0)
			this.sizeCompare(elems, thatKnownSize)
		else {
			val thisKnownSize = this.knownSize(elems)

			if (thisKnownSize >= 0) {
				val res = collection.sizeCompare(that, thisKnownSize)
				// can't just invert the result, because `-Int.MinValue == Int.MinValue`
				if (res == Int.MinValue) 1 else -res
			} else {
				val thisIt = this.iterator(elems)
				val thatIt = collection.iterator(that)
				while (thisIt.hasNext && thatIt.hasNext) {
					thisIt.next()
					thatIt.next()
				}
				java.lang.Boolean.compare(thisIt.hasNext, thatIt.hasNext)
			}
		}
	}

	/** Transposes `elems` of iterable collections into a $coll of ${coll}s.
	  *
	  * The resulting collection's type will be guided by the static type of $coll. For example:
	  * {{{
	  *    val xs = List(
	  *               Set(1, 2, 3),
	  *               Set(4, 5, 6)).transpose
	  *    // xs == List(
	  *    //         List(1, 4),
	  *    //         List(2, 5),
	  *    //         List(3, 6))
	  *
	  *    val ys = Vector(
	  *               List(1, 2, 3),
	  *               List(4, 5, 6)).transpose
	  *    // ys == Vector(
	  *    //         Vector(1, 4),
	  *    //         Vector(2, 5),
	  *    //         Vector(3, 6))
	  * }}}
	  *
	  * $willForceEvaluation
	  *
	  * @tparam A         the type of the elements of each iterable collection.
	  * @param elems      a $coll.
	  * @param asIterable an implicit conversion which asserts that the element type of `elems` is an `Iterable`.
	  * @return a two-dimensional $coll of ${coll}s which has as ''n''th row the ''n''th column of `elems`.
	  * @throws IllegalArgumentException if all collections in `elems` are not of the same size.
	  */ //consider: using a type class instead of an implicit conversion.
	def transpose[A](elems :C)(implicit asIterable :E => Iterable[A]) :CC[CC[A] @uncheckedVariance] =
		toOps(elems).transpose

	override def filter(elems :C)(p :E => Boolean) :C = toOps(elems).filter(p)
	override def filterNot(elems :C)(p :E => Boolean) :C = toOps(elems).filterNot(p)

	/** Creates a non-strict filter of `elems`.
	  *
	  * Note: the difference between `c filter p` and `c withFilter p` is that
	  *       the former creates a new collection, whereas the latter only
	  *       restricts the domain of subsequent `map`, `flatMap`, `foreach`,
	  *       and `withFilter` operations.
	  * $orderDependent
	  * @param elems a $coll.
	  * @param p     the predicate used to test elements.
	  * @return An object of class `WithFilter`, which supports `map`, `flatMap`, `foreach`, and `withFilter` operations.
	  *         All these operations apply to those elements of `elems` which satisfy the predicate `p`.
	  */
	def withFilter(elems :C)(p :E => Boolean) :WithFilter[E, CC] = toOps(elems).withFilter(p)

	/** A pair of, first, all elements that satisfy predicate `p` and, second,
	  * all elements that do not. Interesting because it splits a collection in two.
	  *
	  * The default implementation provided here needs to traverse the collection twice.
	  * Strict collections have an overridden version of `partition` in `StrictOptimizedIterableOps`,
	  * which requires only a single traversal.
	  * @param elems a $coll.
	  * @param p     a filter function selecting elements for the first of the returned ${coll}s.
	  */
	def partition(elems :C)(p :E => Boolean) :(C, C) = toOps(elems).partition(p)

	override def slice(elems :C)(from :Int, until :Int) :C = toOps(elems).slice(from, until)
	override def take(elems :C)(n :Int) :C = toOps(elems).take(n)
	override def drop(elems :C)(n :Int) :C = toOps(elems).drop(n)

	/** Selects the last ''n'' elements.
	  * $orderDependent
	  * @param elems a $coll.
	  * @param n     the number of elements to take from `elems`.
	  * @return a $coll consisting only of the last `n` elements of `elems`, or else the whole $coll,
	  *         if it has less than `n` elements. If `n` is negative, returns an empty $coll.
	  */
	def takeRight(elems :C)(n :Int) :C = toOps(elems).takeRight(n)

	/** Selects all elements except last ''n'' ones.
	  * $orderDependent
	  * @param elems a $coll.
	  * @param n     the number of elements to drop from `elems`.
	  * @return a $coll consisting of all elements of `elems` except the last `n` ones, or else the empty $coll,
	  *         if `elems` has less than `n` elements. If `n` is negative, don't drop any elements.
	  */
	def dropRight(elems :C)(n :Int) :C = toOps(elems).dropRight(n)

	override def splitAt(elems :C)(n :Int) :(C, C) = toOps(elems).splitAt(n)

	/** The rest of the collection without its first element.
	  * @param elems a $coll.
	  */
	def tail(elems :C) :C = {
		if (isEmpty(elems)) throw new UnsupportedOperationException("empty.tail")
		drop(elems)(1)
	}

	/** The initial part of the collection without its last element.
	  * $willForceEvaluation
	  * @param elems a $coll.
	  */
	def init(elems :C) :C = {
		if (isEmpty(elems)) throw new UnsupportedOperationException("empty.init")
		dropRight(elems)(1)
	}

	/** Partitions `elems` into a map of ${coll}s according to some discriminator function.
	  *
	  * $willForceEvaluation
	  * @param elems a $coll.
	  * @param f     the discriminator function.
	  * @tparam K    the type of keys returned by the discriminator function.
	  * @return      A map from keys to ${coll}s such that the following invariant holds:
	  *              {{{
	  *                 (xs groupBy f)(k) = xs filter (x => f(x) == k)
	  *              }}}
	  *              That is, every key `k` is bound to a $coll of those elements `x` for which `f(x)` equals `k`.
	  */
	def groupBy[K](elems :C)(f :E => K) :Map[K, C] = toOps(elems).groupBy(f)

	/** Partitions `elems` into a map of ${coll}s according to a discriminator function `key`.
	  * Each element in a group is transformed into a value of type `B` using the `value` function.
	  *
	  * It is equivalent to `groupBy(key).mapValues(_.map(f))`, but more efficient.
	  * {{{
	  *   case class User(name: String, age: Int)
	  *
	  *   def namesByAge(users: Seq[User]): Map[Int, Seq[String]] =
	  *     users.groupMap(_.age)(_.name)
	  * }}}
	  *
	  * $willForceEvaluation
	  * @tparam K the type of keys returned by the discriminator function
	  * @tparam A the type of values returned by the transformation function
	  * @param elems a $coll.
	  * @param key   the discriminator function.
	  * @param f     the element transformation function.
	  */
	def groupMap[K, A](elems :C)(key :E => K)(f :E => A) :Map[K, CC[A]] = toOps(elems).groupMap(key)(f)

	/** Partitions `elems` into a map according to a discriminator function `key`. All the values that
	  * have the same discriminator are then transformed by the `f` function and then reduced into a
	  * single value with the `reduce` function.
	  *
	  * It is equivalent to `groupBy(key).mapValues(_.map(f).reduce(reduce))`, but more efficient.
	  * {{{
	  *   def occurrences[A](as: Seq[A]): Map[A, Int] =
	  *     as.groupMapReduce(identity)(_ => 1)(_ + _)
	  * }}}
	  * $willForceEvaluation
	  * @tparam K the type of keys returned by the discriminator function
	  * @tparam A the type of values returned by the transformation function
	  * @param elems  a $coll.
	  * @param key    the discriminator function.
	  * @param f      the element transformation function.
	  * @param reduce an associative function.
	  */
	def groupMapReduce[K, A](elems :C)(key :E => K)(f :E => A)(reduce :(A, A) => A) :Map[K, A] =
		toOps(elems).groupMapReduce(key)(f)(reduce)

	override def tapEach[U](elems :C)(f :E => U) :C = toOps(elems).tapEach(f)

	/** Computes a prefix scan of the elements of the collection.
	  *
	  * Note: The neutral element `z` may be applied more than once.
	  * @tparam A    the element type of the resulting collection.
	  * @param elems a $coll.
	  * @param z     neutral element for the operator `op`.
	  * @param op    the associative operator for the scan.
	  * @return      a new $coll containing the prefix scan of the elements in `elems`.
	  */
	def scan[A >: E](elems :C)(z :A)(op :(A, A) => A) :CC[A] = scanLeft(elems)(z)(op)

	override def scanLeft[A](elems :C)(z :A)(op: (A, E) => A) :CC[A] = toOps(elems).scanLeft(z)(op)

	/** Produces a collection containing cumulative results of applying the operator going right to left.
	  * The head of the collection is the last cumulative result.
	  * $willNotTerminateInf
	  * $orderDependent
	  * $willForceEvaluation
	  *
	  * Example:
	  * {{{
	  *    List(1, 2, 3, 4).scanRight(0)(_ + _) == List(10, 9, 7, 4, 0)
	  * }}}
	  * @tparam A    the type of the elements in the resulting collection
	  * @param elems a $coll.
	  * @param z     the initial value
	  * @param op    the binary operator applied to the intermediate result and the element
	  * @return      collection with intermediate results
	  */
	def scanRight[A](elems :C)(z :A)(op :(E, A) => A): CC[A] = toOps(elems).scanRight(z)(op)

	override def map[A](elems :C)(f :E => A) :CC[A] = toOps(elems).map(f)

	override def flatMapIterableOnce[A](elems :C)(f :E => IterableOnce[A]) :CC[A] = toOps(elems).flatMap(f)

	override def flatMap[O, A](elems :C)(f :E => O)(implicit asIterableOnce :IterableOnceLike[A, generic.Any, O]) :CC[A] =
		toOps(elems).flatMap((e :E) => asIterableOnce.toIterableOnce(f(e)))

	override def flatten[A, U >: E](elems :C)(implicit iterableOnceLike :IterableOnceLike[A, generic.Any, U]) :CC[A] =
		flattenIterableOnce(elems)(iterableOnceLike.toIterableOnce)

	override def flattenIterableOnce[A](elems :C)(implicit asIterableOnce :E => IterableOnce[A]): CC[A] =
		flatMapIterableOnce(elems)(asIterableOnce)

	override def collect[A](elems :C)(pf :PartialFunction[E, A]) :CC[A] = toOps(elems).collect(pf)

	/** Applies a function `f` to each element of the $coll and returns a pair of ${coll}s: the first one
	  * made of those values returned by `f` that were wrapped in [[scala.util.Left]], and the second
	  * one made of those wrapped in [[scala.util.Right]].*
	  * @tparam A1  the element type of the first resulting collection
	  * @tparam A2  the element type of the second resulting collection
	  * @param elems a $coll.
	  * @param f     the 'split function' mapping the elements of `elems` to an [[scala.util.Either]]
	  *
	  * @return a pair of ${coll}s: the first one made of those values returned by `f`
	  *         that were wrapped in [[scala.util.Left]], and the second one made of those wrapped in [[scala.util.Right]].
	  */
	def partitionMap[A1, A2](elems :C)(f :E => Either[A1, A2]) :(CC[A1], CC[A2]) = toOps(elems).partitionMap(f)

	/** Returns a new $coll containing the elements from the left hand operand followed by the elements from the
	  * right hand operand. The element type of the $coll is the most specific superclass encompassing
	  * the element types of the two operands.
	  * @tparam A     the element type of the returned collection.
	  * @param suffix the iterable to append.
	  * @param elems  a $coll.
	  * @return       a new $coll which contains all elements of `elems` followed by all elements of `suffix`.
	  */
	def concat[A >: E, O](elems :C)(suffix :O)(implicit iterableOnceLike :IterableOnceLike[A, generic.Any, O]) :CC[A] =
		toOps(elems).concat(iterableOnceLike.toIterableOnce(suffix))

	/** Returns a $coll formed from `elems` and another iterable collection
	  * by combining corresponding elements in pairs.
	  * If one of the two collections is longer than the other, its remaining elements are ignored.
	  * @tparam A    the type of the second half of the returned pairs.
	  * @param elems a $coll.
	  * @param that  The iterable providing the second half of each result pair.
	  * @return      a new $coll containing pairs consisting of corresponding elements of `elems` and `that`.
	  *              The length of the returned collection is the minimum of the lengths of `elems` and `that`.
	  */
	def zip[A, O](elems :C)(that :O)
	             (implicit iterableOnceLike :IterableOnceLike[A, generic.Any, O]) :CC[(E @uncheckedVariance, A)] =
		toOps(elems).zip(iterableOnceLike.toIterableOnce(that))

	override def zipWithIndex(elems :C) :CC[(E @uncheckedVariance, Int)] = toOps(elems).zipWithIndex

	/** Returns a $coll formed from `elems` and another iterable collection by combining corresponding elements in pairs.
	  * If one of the two collections is shorter than the other,
	  * placeholder elements are used to extend the shorter collection to the length of the longer.
	  * @param elems    a $coll.
	  * @param that     the iterable providing the second half of each result pair.
	  * @param thisElem the element to be used to fill up the result if `elems` is shorter than `that`.
	  * @param thatElem the element to be used to fill up the result if `that` is shorter than `elems`.
	  * @return A new collection of type `That` containing pairs consisting of corresponding elements of `elems`
	  *         and `that`. The length of the returned collection is the maximum of the lengths of `elems` and `that`.
	  *         If `elems` is shorter than `that`, `thisElem` values are used to pad the result.
	  *         If `that` is shorter than `elems`, `thatElem` values are used to pad the result.
	  */
	def zipAll[U >: E, A, O](elems :C)(that :O, thisElem :U, thatElem :A)
	                        (implicit iterableLike :IterableLike[A, generic.Any, O]): CC[(U, A)] =
		toOps(elems).zipAll(iterableLike.toIterable(that), thisElem, thatElem)

	/** Converts `elems` of pairs into two collections of the first and second half of each pair.
	  * @tparam A1    the type of the first half of the element pairs.
	  * @tparam A2    the type of the second half of the element pairs.
	  * @param elems  a $coll.
	  * @param asPair an implicit conversion which asserts that the element type of `elems` is a pair.
	  * @return a pair of ${coll}s, containing the first, respectively second half of each element pair of `elems`.
	  */
	def unzip[A1, A2](elems :C)(implicit asPair :E => (A1, A2)) :(CC[A1], CC[A2]) = toOps(elems).unzip

	/** Converts `elems` of triples into three collections of the first, second, and third element of each triple.
	  * @tparam A1      the type of the first member of the element triples.
	  * @tparam A2      the type of the second member of the element triples.
	  * @tparam A3      the type of the third member of the element triples.
	  * @param elems    a $coll.
	  * @param asTriple an implicit conversion which asserts that the element type of `elems` is a triple.
	  * @return         a triple of ${coll}s, containing the first, second, respectively
	  *                 third member of each element triple of `elems`.
	  */
	def unzip3[A1, A2, A3](elems :C)(implicit asTriple :E => (A1, A2, A3)) :(CC[A1], CC[A2], CC[A3]) =
		toOps(elems).unzip3

	/** Analogous to `zip` except that the elements in each collection are not consumed until a strict operation is
	  * invoked on the returned `LazyZip2` decorator.
	  *
	  * Calls to `lazyZip` can be chained to support higher arities (up to 4) without incurring the expense of
	  * constructing and deconstructing intermediary tuples.
	  * {{{
	  *    val xs = List(1, 2, 3)
	  *    val res = (xs lazyZip xs lazyZip xs lazyZip xs).map((a, b, c, d) => a + b + c + d)
	  *    // res == List(4, 8, 12)
	  * }}}
	  * @tparam A the type of the second element in each eventual pair.
	  * @param elems a $coll.
	  * @param that  the iterable providing the second element of each eventual pair.
	  * @return a decorator `LazyZip2` that allows strict operations to be performed on the lazily evaluated pairs
	  *         or chained calls to `lazyZip`. Implicit conversion to `Iterable[(A, B)]` is also supported.
	  */
	def lazyZip[A, O](elems :C)(that :O)(implicit iterableLike :IterableLike[A, generic.Any, O]) :LazyZip2[E, A, C] =
		toIterable(elems).lazyZip(iterableLike.toIterable(that)).castParam3[C]

	/** Partitions elements in fixed size ${coll}s.
	  * @see [[scala.collection.Iterator]], method `grouped`.
	  * @param elems a $coll.
	  * @param size the number of elements per group.
	  * @return An iterator producing ${coll}s of size `size`, except the
	  *         last will be less than size `size` if the elements don't divide evenly.
	  */
	def grouped(elems :C)(size :Int) :Iterator[C] =
		iterator(elems).grouped(size).map(fromSpecific(elems))

	/** Groups elements in fixed size blocks by passing a "sliding window" over them (as opposed to partitioning them,
	  * as is done in `grouped`.)
	  *
	  * An empty collection returns an empty iterator, and a non-empty collection containing fewer elements
	  * than the window size returns an iterator that will produce the original collection as its only element.
	  * @see [[scala.collection.Iterator]], method `sliding`
	  * @param elems a $coll.
	  * @param size  the number of elements per group
	  * @param step  the distance between the first elements of successive groups
	  * @return An iterator producing ${coll}s of size `size`, except for a non-empty collection
	  *         with less than `size` elements, which returns an iterator that produces the source collection itself
	  *         as its only element.
	  * @example `List().sliding(2) = empty iterator`
	  * @example `List(1).sliding(2) = Iterator(List(1))`
	  * @example `List(1, 2).sliding(2) = Iterator(List(1, 2))`
	  * @example `List(1, 2, 3).sliding(2) = Iterator(List(1, 2), List(2, 3))`
	  */
	def sliding(elems :C)(size :Int, step :Int = 1) :Iterator[C] =
		iterator(elems).sliding(size, step).map(fromSpecific(elems))

	/** Iterates over the tails of `elems`. The first value will be `elems` and the final one will be an empty $coll,
	  * with the intervening values the results of successive applications of `tail`.
	  * @param elems a $coll.
	  * @return an iterator over all the tails of `elems`
	  * @example `List(1,2,3).tails = Iterator(List(1,2,3), List(2,3), List(3), Nil)`
	  */
	def tails(elems :C) :Iterator[C] = toOps(elems).tails

	/** Iterates over the inits of `elems`. The first value will be `elems` and the final one will be an empty $coll,
	  * with the intervening values the results of successive applications of `init`.
	  *
	  *  $willForceEvaluation
	  *
	  * @param elems a $coll.
	  * @return  an iterator over all the inits of `elems`
	  * @example `List(1,2,3).inits = Iterator(List(1,2,3), List(1,2), List(1), Nil)`
	  */
	def inits(elems :C) :Iterator[C] = toOps(elems).inits


	/** A view over the elements of this collection.
	  * @param elems a $coll.
	  */
	def view(elems :C) :View[E] = View.fromIteratorProvider(() => iterator(elems))

	/** A copy of `elems`, containing the same elements. Immutable collections may simply return the argument. */
	def copy(elems :C) :C = (newSpecificBuilder(elems) ++= elems).result()

	override def cyclicCopyToArray[A >: E](elems :C)(array :Array[A], index :Int, max :Int) :Int =
		toOps(elems).cyclicCopyToArray(array, index, max)

	override def toOps(elems :C) :IterableOps[E, CC, C]
}






private[collections] sealed abstract class Rank1IterableLike extends IterableOnceLikeSummons[IterableLike] {
	implicit def forOps[E, CC[X]/* <: Iterable[X]*/, C/* <: CC[E] with IterableOps[E, CC, C]*/] //:IterableLike[E, CC, C] =
	                   (implicit specific :C <:< CC[E] with IterableOps[E, CC, C], generic :CC <:?< Iterable)
			:IterableLike[E, CC, C] =
		prototype.asInstanceOf[IterableLike[E, CC, C]]

	private[this] val prototype = new IterableLike.ForOps[Any, Iterable, Iterable[Any]] {
		private def readResolve = IterableLike.forOps[Any, Iterable, Iterable[Any]]
		override def toString = "IterableLike.forOps"
	}
}

@SerialVersionUID(Ver)
object IterableLike extends Rank1IterableLike {

	@inline implicit def fromSeqLike[E, CC[X], C](implicit like :SeqLike[E, CC, C]) :IterableLike[E, CC, C] = like
	@inline implicit def fromSetLike[E, CC[X], C](implicit like :SetLike[E, CC, C]) :IterableLike[E, CC, C] = like


	/** An implementation of `IterableOnce` methods for any `C <: IterableOnceOps[E, CC, C]`. */
	trait ForOps[E, CC[X] <: Iterable[X], C <: CC[E] with IterableOps[E, CC, C]]
		extends IterableLike[E, CC, C] with IterableOnceLike.ForOps[E, CC, C]
	{
		override def iterableFactory(elems :C) :IterableFactory[CC] = (elems :IterableOps[E, CC, C]).iterableFactory

		override def empty(elems :C) :C = (elems :IterableOps[E, CC, C]).empty
		override def head(elems :C) :E = elems.head
		override def headOption(elems :C) :Option[E] = elems.headOption
		override def last(elems :C) :E = elems.last
		override def lastOption(elems :C) :Option[E] = elems.lastOption
		override def takeRight(elems :C)(n :Int) :C = elems.takeRight(n)
		override def dropRight(elems :C)(n :Int) :C = elems.dropRight(n)

		override def sizeCompare(elems :C, otherSize :Int) :Int = elems.sizeCompare(otherSize)
		override def sizeCompare[O](elems :C, that :O)(implicit collection :IterableLike[_, funny.generic.Any, O]) :Int =
			elems.sizeCompare(collection.toIterable(that))

		override def tail(elems :C) :C = elems.tail
		override def init(elems :C) :C = elems.init
		override def scan[A >: E](elems :C)(z :A)(op :(A, A) => A) :CC[A] = elems.scan(z)(op)
		override def scanRight[A](elems :C)(z :A)(op :(E, A) => A) :CC[A] = elems.scanRight(z)(op)

		override def flatten[A, U >: E]
		                    (elems :C)(implicit iterableOnceLike :IterableOnceLike[A, funny.generic.Any, U]) :CC[A] =
			elems.flatten(iterableOnceLike.toIterableOnce)

		override def flattenIterableOnce[A](elems :C)(implicit asIterableOnce :E => IterableOnce[A]): CC[A] =
			elems.flatten

		override def grouped(elems :C)(size :Int) :Iterator[C] = elems.grouped(size)
		override def sliding(elems :C)(size :Int, step :Int = 1) :Iterator[C] = elems.sliding(size, step)

		override def tails(elems :C) :Iterator[C] = elems.tails
		override def inits(elems :C) :Iterator[C] = elems.inits

		override def view(elems :C) :View[E] = elems.view

		override def toOps(elems :C) :IterableOps[E, CC, C] = elems
	}
}
