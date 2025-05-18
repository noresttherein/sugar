package net.noresttherein.sugar.collections

import scala.annotation.nowarn
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{IterableFactory, IterableOps, View, WithFilter}
import scala.collection.mutable.{Buffer, Builder}

import net.noresttherein.sugar.collections.LikeIterableOnce.Generic.Template
import net.noresttherein.sugar.collections.LikeIterableOnce.{GenericLikeIterableOnce, GenericLikeIterableOnceBuilder, LikeIterableOnceBasics, LikeIterableOnceBuilder, LikeIterableOnceFactory}
import net.noresttherein.sugar.exceptions.unsupported_!
import net.noresttherein.sugar.extensions.{BufferExtension, IteratorExtension, boxeqMethod}
import net.noresttherein.sugar.typist.kinds.Any1
import net.noresttherein.sugar.typist.{<::<, kinds}
import net.noresttherein.sugar.util.SerializableSingleton
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




/** A type class providing operations available in [[collection.Iterable Iterable]] for type `C`.
  * @tparam X  the element type.
  * @tparam CC the type constructor for the generic version of the collection, applicable to any element type.
  * @tparam C  the collection type
  * @define Coll `Iterable`
  * @define coll iterable collection
  */ //consider: removing a large portion of less used methods.
trait LikeIterable[+X, -Xs, +CC[_], +C] extends LikeIterableOnce[X, Xs, CC, C] {
	override def isTraversableAgain(elems :Xs) :Boolean = true

//	/** Returns a value class containing operations for comparing the size of `elems` to a test value. */
//	def sizeIs(elems :Xs) :IterableOps.SizeCompareOps = toOps(elems).sizeIs

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
	  */
	def sizeCompare(elems :Xs, otherSize :Int) :Int

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
	def sizeCompare[O](elems :Xs, that :O)(implicit likeIterable :LikeIterable[_, O, kinds.Any1, _]) :Int

	/** Selects the first element of `elems`.
	  * $orderDependent
	  * @param elems a $coll.
	  * @return the first element of `elems`.
	  * @throws NoSuchElementException if the $coll is empty.
	  */
	def head(elems :Xs) :X //= iterator(elems).next()

	/** Optionally selects the first element.
	  * $orderDependent
	  * @param elems a $coll.
	  * @return the first element of `elems` if it is nonempty, `None` if it is empty.
	  */
	def headOption(elems :Xs) :Option[X]

	/** Selects the last element.
	  * $orderDependent
	  * @param elems a $coll.
	  * @return The last element of `elems`.
	  * @throws NoSuchElementException If the $coll is empty.
	  */
	def last(elems :Xs) :X

	/** Optionally selects the last element.
	  * $orderDependent
	  * @param elems a $coll.
	  * @return the last element of `elems`$ if it is nonempty, `None` if it is empty.
	  */
	def lastOption(elems :Xs) :Option[X]



	/** The empty iterable of the same type as this iterable
	  * @param elems a $coll.
	  * @return an empty iterable of type `C`.
	  */
	def empty(elems :Xs) :C

	/** A copy of `elems`, containing the same elements. It is independent of the modifications to the original,
	  * but immutable collections may simply return the argument.
	  * @see [[net.noresttherein.sugar.collections.LikeIterableOnce.toSpecific toSpecific]]
	  */
	def copy(elems :Xs) :C

	/** The rest of the collection without its first element.
	  * @param elems a $coll.
	  */
	def tail(elems :Xs) :C

	/** The initial part of the collection without its last element.
	  * $willForceEvaluation
	  * @param elems a $coll.
	  */
	def init(elems :Xs) :C

	/** Selects the last ''n'' elements.
	  * $orderDependent
	  * @param elems a $coll.
	  * @param n     the number of elements to take from `elems`.
	  * @return a $coll consisting only of the last `n` elements of `elems`, or else the whole $coll,
	  *         if it has less than `n` elements. If `n` is negative, returns an empty $coll.
	  */
	def takeRight(elems :Xs, n :Int) :C

	/** Selects all elements except last ''n'' ones.
	  * $orderDependent
	  * @param elems a $coll.
	  * @param n     the number of elements to drop from `elems`.
	  * @return a $coll consisting of all elements of `elems` except the last `n` ones, or else the empty $coll,
	  *         if `elems` has less than `n` elements. If `n` is negative, don't drop any elements.
	  */
	def dropRight(elems :Xs, n :Int) :C

	/** A pair of, first, all elements that satisfy predicate `p` and, second,
	  * all elements that do not. Interesting because it splits a collection in two.
	  *
	  * The default implementation provided here needs to traverse the collection twice.
	  * Strict collections have an overridden version of `partition` in `StrictOptimizedIterableOps`,
	  * which requires only a single traversal.
	  * @param elems a $coll.
	  * @param p     a filter function selecting elements for the first of the returned ${coll}s.
	  */
	def partition(elems :Xs)(p :X => Boolean) :(C, C)


	/** Computes a prefix scan of the elements of the collection.
	  *
	  * Note: The neutral element `z` may be applied more than once.
	  * @tparam A    the element type of the resulting collection.
	  * @param elems a $coll.
	  * @param z     neutral element for the operator `op`.
	  * @param op    the associative operator for the scan.
	  * @return      a new $coll containing the prefix scan of the elements in `elems`.
	  */
	def scan[A >: X](elems :Xs)(z :A)(op :(A, A) => A) :CC[A] = scanLeft(elems)(z)(op)

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
	def scanRight[A](elems :Xs)(z :A)(op :(X, A) => A): CC[A]// = toOps(elems).scanRight(z)(op)

	/** Returns a new $coll containing the elements from the left hand operand followed by the elements from the
	  * right hand operand. The element type of the $coll is the most specific superclass encompassing
	  * the element types of the two operands.
	  * @tparam U     the element type of the returned collection.
	  * @param suffix the iterable to append.
	  * @param elems  a $coll.
	  * @return       a new $coll which contains all elements of `elems` followed by all elements of `suffix`.
	  */
	def concat[U >: X, O](elems :Xs, suffix :O)(implicit likeCollection :LikeCollection[U, O]) :CC[U]


	/** Returns a $coll formed from `elems` and another iterable collection
	  * by combining corresponding elements in pairs.
	  * If one of the two collections is longer than the other, its remaining elements are ignored.
	  * @tparam A    the type of the second half of the returned pairs.
	  * @param elems a $coll.
	  * @param that  The iterable providing the second half of each result pair.
	  * @return      a new $coll containing pairs consisting of corresponding elements of `elems` and `that`.
	  *              The length of the returned collection is the minimum of the lengths of `elems` and `that`.
	  */
	def zip[U >: X, A, O](elems :Xs, that :O)(implicit likeCollection :LikeCollection[A, O]) :CC[(U, A)]
//	override def zipWithIndex(elems :Xs) :CC[(X @uncheckedVariance, Int)] = toOps(elems).zipWithIndex

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
	def zipAll[U >: X, A, O](elems :Xs, that :O, thisElem :U, thatElem :A)
	                        (implicit likeIterable :LikeIterable[A, O, kinds.Any1, _]): CC[(U, A)]

	/** Converts `elems` of pairs into two collections of the first and second half of each pair.
	  * @tparam A1    the type of the first half of the element pairs.
	  * @tparam A2    the type of the second half of the element pairs.
	  * @param elems  a $coll.
	  * @param asPair an implicit conversion which asserts that the element type of `elems` is a pair.
	  * @return a pair of ${coll}s, containing the first, respectively second half of each element pair of `elems`.
	  */
	def unzip[A1, A2](elems :Xs)(implicit asPair :X => (A1, A2)) :(CC[A1], CC[A2])

	/** Converts `elems` of triples into three collections of the first, second, and third element of each triple.
	  * @tparam A1      the type of the first member of the element triples.
	  * @tparam A2      the type of the second member of the element triples.
	  * @tparam A3      the type of the third member of the element triples.
	  * @param elems    a $coll.
	  * @param asTriple an implicit conversion which asserts that the element type of `elems` is a triple.
	  * @return         a triple of ${coll}s, containing the first, second, respectively
	  *                 third member of each element triple of `elems`.
	  */
	def unzip3[A1, A2, A3](elems :Xs)(implicit asTriple :X => (A1, A2, A3)) :(CC[A1], CC[A2], CC[A3])
//
//	/** Transposes `elems` of iterable collections into a $coll of ${coll}s.
//	  *
//	  * The resulting collection's type will be guided by the static type of $coll. For example:
//	  * {{{
//	  *    val xs = List(
//	  *               Set(1, 2, 3),
//	  *               Set(4, 5, 6)).transpose
//	  *    // xs == List(
//	  *    //         List(1, 4),
//	  *    //         List(2, 5),
//	  *    //         List(3, 6))
//	  *
//	  *    val ys = Vector(
//	  *               List(1, 2, 3),
//	  *               List(4, 5, 6)).transpose
//	  *    // ys == Vector(
//	  *    //         Vector(1, 4),
//	  *    //         Vector(2, 5),
//	  *    //         Vector(3, 6))
//	  * }}}
//	  *
//	  * $willForceEvaluation
//	  *
//	  * @tparam A         the type of the elements of each iterable collection.
//	  * @param elems      a $coll.
//	  * @param asIterable an implicit conversion which asserts that the element type of `elems` is an `Iterable`.
//	  * @return a two-dimensional $coll of ${coll}s which has as ''n''th row the ''n''th column of `elems`.
//	  * @throws IllegalArgumentException if all collections in `elems` are not of the same size.
//	  */ //consider: using a type class instead of an implicit conversion.
//	def transpose[A](elems :Xs)(implicit asIterable :X => Iterable[A]) :CC[CC[A] @uncheckedVariance]


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
	def partitionMap[A1, A2](elems :Xs)(f :X => Either[A1, A2]) :(CC[A1], CC[A2])



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
	def groupBy[K](elems :Xs)(f :X => K) :Map[K, C]

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
	def groupMap[K, A](elems :Xs)(key :X => K)(f :X => A) :Map[K, CC[A]]

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
	def groupMapReduce[K, A](elems :Xs)(key :X => K)(f :X => A)(reduce :(A, A) => A) :Map[K, A]


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
	def withFilter(elems :Xs)(p :X => Boolean) :WithFilter[X, CC]
//	override def cyclicCopyToArray[A >: X](elems :Xs)(array :Array[A], index :Int, max :Int) :Int

	/** A view over the elements of this collection.
	  * @param elems a $coll.
	  */
	def view(elems :Xs) :View[X]

//	override def toOps(elems :Xs) :IterableOps[X, CC, C]

	override def moreSpecific(elems :Xs) :Maybe[LikeIterable[X, elems.type, CC, C]] = No
	override def specific(elems :Xs) :LikeIterable[X, elems.type, CC, C] = moreSpecific(elems) getOrElse this
}






/** @define TypeClass `LikeIterable` */
private[collections] sealed abstract class Rank2LikeIterables extends LikeIterableOnceSummons[LikeIterable] {
	//We could make this method generic and return LC[X, CC[X], CC, CC[X]], but then there would be
	// a precedence conflict with all implicits below, which are not generic. We could make them generic by accepting
	// LC type parameter by all RankNLikeX superclasses (at the cost of complete forgoing of real type safety,
	// but we can't do it with definitions in the actual object. Which would mean adding an extra superclass
	// for every Like object anyway, and increase the definition complexity and confusion even more.
	// I deemed it not worth it.
	@inline implicit final def likeGeneric[X, Xs <: CC[X], CC[_], C >: CC[X]]
	                                      (implicit generic :Generic[CC]) :LikeIterable[X, Xs, CC, C] =
		generic.of
}


private[collections] sealed abstract class Rank1LikeIterables extends Rank2LikeIterables {
	//Parameter Xs is needed so that the definition is not 'more specific' than likeSeq/likeSet
	implicit final def forOps[X, Xs, CC[_], C]
	                         (implicit arg :Xs <:< C, specific :C <:< CC[X] with IterableOps[X, CC, C],
	                                   generic :CC <::< Iterable) :LikeIterable[X, Xs, CC, C] =
		prototype.asInstanceOf[LikeIterable[X, Xs, CC, C]]

	private[this] val prototype = new LikeIterable.ForOps[Any, Iterable, Iterable[Any]] {
		import scala.collection.{Seq => GenSeq, Set => GenSet}
		override def moreSpecific(elems :Iterable[Any]) =
			(elems match {
				case seq :GenSeq[Any] => Yes(LikeSeq.forOps[Any, GenSeq[Any], GenSeq, GenSeq[Any]].specific(seq))
				case set :GenSet[Any] => Yes(LikeSet.forOps[Any, GenSet[Any], GenSet, GenSet[Any]].specific(set))
				case _                => No
			}).asInstanceOf[Maybe[LikeIterable[Any, elems.type, Iterable, Iterable[Any]]]]

		private def readResolve :AnyRef = LikeIterable.forOps[Any, Iterable[Any], Iterable, Iterable[Any]]
		override def toString = "LikeIterable.forOps"
	}
}


@SerialVersionUID(Ver)
object LikeIterable extends Rank1LikeIterables {

	@inline implicit def likeSeq[X, Xs, CC[_], C](implicit like :LikeSeq[X, Xs, CC, C]) :LikeIterable[X, Xs, CC, C] =
		like
	@inline implicit def likeSet[X, Xs, CC[_], C](implicit like :LikeSet[X, Xs, CC, C]) :LikeIterable[X, Xs, CC, C] =
		like
	@inline implicit def likeRanking[X, Xs, CC[_], C]
	                                (implicit like :LikeRanking[X, Xs, CC, C]) :LikeIterable[X, Xs, CC, C] =
		like


	def adapt[X, Xs](elems :Xs)(implicit likeIterable :LikeIterable[X, Xs, Any1, Any]) :Iterable[X] =
		new LikeCollectionAdapter[X, elems.type](elems)
			with Iterable[X] with LikeIterableAdapter[X, elems.type, Iterable, Iterable[X]]
		{
			override val ops = likeIterable.specific(this.elems)
		}


	trait Generic[CC[_]] extends LikeIterableOnce.Generic[CC] with Template[CC, LikeIterable]

	@SerialVersionUID(Ver)
	object Generic extends Rank1Generics {
		@inline implicit def likeSeq[CC[_]](implicit generic :LikeSeq.Generic[CC]) :Generic[CC] = generic
		@inline implicit def likeSet[CC[_]](implicit generic :LikeSet.Generic[CC]) :Generic[CC] = generic
		@inline implicit def likeRanking[CC[_]](implicit generic :LikeRanking.Generic[CC]) :Generic[CC] = generic
	}

	private[LikeIterable] sealed abstract class Rank1Generics {
		implicit final def forOps[CC[X] <: Iterable[X] with IterableOps[X, CC, CC[X]]] :Generic[CC] =
			prototype.asInstanceOf[Generic[CC]]

		private[this] val prototype =
			new SerializableSingleton("LikeIterable.Generic.forOps", Generic.forOps[Iterable]) with Generic[Iterable] {
				implicit override def of[X] :LikeIterable[X, Iterable[X], Iterable, Iterable[X]] = LikeIterable.forOps
			}
	}


	trait LikeMoreSpecific[+X, -Xs, +CC[_], +C]
		extends LikeIterable[X, Xs, CC, C] with LikeIterableOnce.LikeMoreSpecific[X, Xs, CC, C]
	{
		abstract override def sizeCompare(elems :Xs, otherSize :Int) :Int = moreSpecific(elems) match {
			case Yes(specific) => specific.sizeCompare(elems :elems.type, otherSize)
			case No            => super.sizeCompare(elems, otherSize)
		}
		abstract override def sizeCompare[O](elems :Xs, that :O)
		                                    (implicit likeIterable :LikeIterable[_, O, Any1, _]) :Int =
			moreSpecific(elems) match {
				case Yes(specific) => specific.sizeCompare(elems :elems.type, that)
				case No            => super.sizeCompare(elems, that)
			}
		abstract override def head(elems :Xs) :X = moreSpecific(elems) match {
			case Yes(specific) => specific.head(elems)
			case No            => super.head(elems)
		}
		abstract override def headOption(elems :Xs) :Option[X] = moreSpecific(elems) match {
			case Yes(specific) => specific.headOption(elems)
			case No            => super.headOption(elems)
		}
		abstract override def last(elems :Xs) :X = moreSpecific(elems) match {
			case Yes(specific) => specific.last(elems)
			case No            => super.last(elems)
		}
		abstract override def lastOption(elems :Xs) :Option[X] = moreSpecific(elems) match {
			case Yes(specific) => specific.lastOption(elems)
			case No            => super.lastOption(elems)
		}
		abstract override def copy(elems :Xs) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.copy(elems)
			case No            => super.copy(elems)
		}
		abstract override def tail(elems :Xs) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.tail(elems)
			case No            => super.tail(elems)
		}
		abstract override def init(elems :Xs) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.init(elems)
			case No            => super.init(elems)
		}
		abstract override def takeRight(elems :Xs, n :Int) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.takeRight(elems, n)
			case No            => super.takeRight(elems, n)
		}
		abstract override def dropRight(elems :Xs, n :Int) :C = moreSpecific(elems) match {
			case Yes(specific) => specific.dropRight(elems, n)
			case No            => super.dropRight(elems, n)
		}
		abstract override def partition(elems :Xs)(p :X => Boolean) :(C, C) = moreSpecific(elems) match {
			case Yes(specific) => specific.partition(elems)(p)
			case No            => super.partition(elems)(p)
		}
		abstract override def scanRight[A](elems :Xs)(z :A)(op :(X, A) => A) :CC[A] = moreSpecific(elems) match {
			case Yes(specific) => specific.scanRight(elems)(z)(op)
			case No            => super.scanRight(elems)(z)(op)
		}
		abstract override def concat[U >: X, O](elems :Xs, suffix :O)
		                                       (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			moreSpecific(elems) match {
				case Yes(specific) => specific.concat(elems, suffix)
				case No            => super.concat(elems, suffix)
			}
		abstract override def zip[U >: X, A, O](elems :Xs, that :O)
		                                       (implicit likeCollection :LikeCollection[A, O]) :CC[(U, A)] =
			moreSpecific(elems) match {
				case Yes(specific) => specific.zip(elems, that)
				case No            => super.zip(elems, that)
			}
		abstract override def zipAll[U >: X, A, O](elems :Xs, that :O, thisElem :U, thatElem :A)
		                                          (implicit likeIterable :LikeIterable[A, O, Any1, _]) :CC[(U, A)] =
			moreSpecific(elems) match {
				case Yes(specific) => specific.zipAll(elems, that, thisElem, thatElem)
				case No            => super.zipAll(elems, that, thisElem, thatElem)
			}
		abstract override def unzip[A1, A2](elems :Xs)(implicit asPair :X => (A1, A2)) :(CC[A1], CC[A2]) =
			moreSpecific(elems) match {
				case Yes(specific) => specific.unzip(elems)
				case No            => super.unzip(elems)
			}
		abstract override def unzip3[A1, A2, A3](elems :Xs)
		                                        (implicit asTriple :X => (A1, A2, A3)) :(CC[A1], CC[A2], CC[A3]) =
			moreSpecific(elems) match {
				case Yes(specific) => specific.unzip3(elems)
				case No            => super.unzip3(elems)
			}
		abstract override def partitionMap[A1, A2](elems :Xs)(f :X => Either[A1, A2]) :(CC[A1], CC[A2]) =
			moreSpecific(elems) match {
				case Yes(specific) => specific.partitionMap(elems)(f)
				case No            => super.partitionMap(elems)(f)
			}
		abstract override def groupBy[K](elems :Xs)(f :X => K) :Map[K, C] = moreSpecific(elems) match {
			case Yes(specific) => specific.groupBy(elems)(f)
			case No            => super.groupBy(elems)(f)
		}
		abstract override def groupMap[K, A](elems :Xs)(key :X => K)(f :X => A) :Map[K, CC[A]] =
			moreSpecific(elems) match {
				case Yes(specific) => specific.groupMap(elems)(key)(f)
				case No            => super.groupMap(elems)(key)(f)
			}
		abstract override def groupMapReduce[K, A](elems :Xs)(key :X => K)(f :X => A)(reduce :(A, A) => A) :Map[K, A] =
			moreSpecific(elems) match {
				case Yes(specific) => specific.groupMapReduce(elems)(key)(f)(reduce)
				case No            => super.groupMapReduce(elems)(key)(f)(reduce)
			}
		abstract override def withFilter(elems :Xs)(p :X => Boolean) :WithFilter[X, CC] =
			moreSpecific(elems) match {
				case Yes(specific) => specific.withFilter(elems)(p)
				case No            => super.withFilter(elems)(p)
			}
		abstract override def view(elems :Xs) :View[X] = moreSpecific(elems) match {
			case Yes(specific) => specific.view(elems)
			case No            => super.view(elems)
		}
	}



	/** Implements those methods of `LikeIterable` which can be implemented by only delegating to
	  * other methods of this interface. In particular, it does not rely on API in `IterableOps` or `Iterator`.
	  * Designed as a 'safe' mix-in for any implementation, regardless of what they are based on.
	  * For 'generic' `Iterable`s and similar,
	  * [[net.noresttherein.sugar.collections.LikeIterable.FromIterator FromIterator]] implements most method
	  * using `iterator(elems)`.
	  */
	trait LikeIterableBasics[+X, -Xs, +CC[_], +C]
		extends LikeIterableOnceBasics[X, Xs, CC, C] with LikeIterable[X, Xs, CC, C]
	{
		override def isEmpty(elems :Xs): Boolean = sizeCompare(elems, 0) == 0
/*
		override def sizeCompare(elems :Xs, otherSize :Int) :Int = {
			val size = this.knownSize(elems)
			if (size >= 0) java.lang.Integer.compare(size, otherSize)
			else toOps(elems).sizeCompare(otherSize)
		}
		override def sizeCompare[O](elems :Xs, that :O)(implicit likeIterable :LikeIterable[_, O, kinds.Any1, _]) :Int = {
			val thatKnownSize = likeIterable.knownSize(that)
			if (thatKnownSize >= 0)
				this.sizeCompare(elems, thatKnownSize)
			else {
				val thisKnownSize = this.knownSize(elems)

				if (thisKnownSize >= 0) {
					val res = likeIterable.sizeCompare(that, thisKnownSize)
					// can't just invert the result, because `-Int.MinValue == Int.MinValue`
					if (res == Int.MinValue) 1 else -res
				} else {
					val thisIt = this.iterator(elems)
					val thatIt = likeIterable.iterator(that)
					while (thisIt.hasNext && thatIt.hasNext) {
						thisIt.next()
						thatIt.next()
					}
					java.lang.Boolean.compare(thisIt.hasNext, thatIt.hasNext)
				}
			}
		}

		override def headOption(elems :Xs) :Option[X] = knownSize(elems) match {
			case -1 => if (isEmpty(elems)) None else Some(iterator(elems).next()) //This may create two iterators :(
//				val it = iterator(elems)
//				if (it.hasNext) Some(it.next()) else None
			case 0 => None
			case _ => Some(head(elems))
		}
		override def lastOption(elems :Xs) :Option[X] = knownSize(elems) match {
			case -1 => if (isEmpty(elems)) None else Some(last)
			case 0 => None
			case _ => Some(last)
		}
		override def last(elems :Xs) :X = last(iterator(elems))
		private[this] def last(iter :Iterator[X]) :X = {
			var lst = iter.next()
			while (iter.hasNext) lst = iter.next()
			lst
		}
*/

		override def empty(elems :Xs) :C = take(elems, 0)

		override def tail(elems :Xs) :C = {
			if (isEmpty(elems)) unsupported_!("empty.tail")
			drop(elems, 1)
		}
		override def init(elems :Xs) :C = {
			if (isEmpty(elems)) unsupported_!("empty.init")
			dropRight(elems, 1)
		}

		override def unzip[A1, A2](elems :Xs)(implicit asPair :X => (A1, A2)) :(CC[A1], CC[A2]) =
			(map(elems)(asPair andThen (_._1)), map(elems)(asPair andThen (_._2)))

		override def unzip3[A1, A2, A3](elems :Xs)(implicit asTriple :X => (A1, A2, A3)) :(CC[A1], CC[A2], CC[A3]) =
			(map(elems)(asTriple andThen (_._1)),
				map(elems)(asTriple andThen (_._2)),
				map(elems)(asTriple andThen (_._3)))
//
//		override def insertInto[U >: X](elems :Xs)(buffer :Buffer[U], index :Int) :Unit =
//			buffer.insertAll(index, toIterableOnce(elems))
//
//		override def patchOver[U >: X](elems :Xs)(buffer :Buffer[U], index :Int, replaced :Int) :Unit =
//			buffer.patchInPlace(index, toIterableOnce(elems), replaced)
	}



	/** Mixin `LikeIterable` type class for collections `Xs` of element type `X` which have a fast `size`
	  * operation. Implements several size-related methods without delegating to `iterator` or `toIterableOnce`.
	  */
	trait OfKnownSize[+X, -Xs, +CC[_], +C] extends LikeIterableBasics[X, Xs, CC, C] {
		override def size(elems :Xs) :Int = knownSize(elems)

		override def sizeCompare(elems :Xs, len :Int) :Int = Integer.compare(size(elems), len)

		override def sizeCompare[O](elems :Xs, that :O)(implicit likeIterable :LikeIterable[_, O, Any1, _]) :Int =
			likeIterable.knownSize(that) match {
				case -1 =>
					val res = likeIterable.sizeCompare(that, size(elems))
					if (res == Int.MinValue) 1 else -res
				case thatSize =>
					Integer.compare(size(elems), thatSize)
			}
		override def isEmpty(elems :Xs) :Boolean = size(elems) == 0

		override def dropRight(elems :Xs, n :Int) :C = slice(elems, 0, size(elems) - n)
		override def takeRight(elems :Xs, n :Int) :C = { val end = size(elems); slice(elems, end - n, end) }
	}



	/** A mixin trait for `LikeIterable` which implements optional methods
	  * [[net.noresttherein.sugar.collections.LikeIterableOnce.LikeIterableOnceFactory.makeGeneric makeGeneric]]
	  * by delegating to [[net.noresttherein.sugar.collections.LikeIterable.FromIterableFactory.iterableFactory iterableFactory]]
	  */
	trait FromIterableFactory[X, -Xs <: IterableOps[X, CC, C], +CC[_], +C]
		extends LikeIterable[X, Xs, CC, C] with LikeIterableOnceFactory[X, Xs, CC, C]
		   with LikeIterableOnceBuilder[X, Xs, CC, C]
	{

		override def copy(elems :Xs) :C = makeSpecific(elems)(elems)
		//Can't be in LikeIterableOnceFactory because Xs is unbounded there.
		override def toSpecific(elems :Xs) :C = makeSpecific(elems)(elems)
		override def toGeneric[U >: X](elems :Xs) :CC[U] = makeGeneric(elems)(elems)

		def iterableFactory(elems :Xs) :IterableFactory[CC] = elems.iterableFactory
		override def makeGeneric[A](elems :Xs)(coll :IterableOnce[A]) :CC[A] = iterableFactory(elems) from coll
		override def makeSpecific(elems :Xs)(coll :IterableOnce[X]) :C = util.fromSpecific(elems)(coll)
		override def genericBuilder[A](elems :Xs) :Builder[A, CC[A]] = iterableFactory(elems).newBuilder[A]
		override def specificBuilder(elems :Xs) :Builder[X, C] = util.specificBuilder(elems)
	}

	/** A type class for generic collection types, able to take any element type.
	  * More specifically, the type `CC[_]` of produced collections is generic, but the type `Xs` of collection
	  * with this type class may be more specific.
	  * @note Despite not implementing additional methods over `FromIterableFactory`, the use of this type class
	  *       is preferred, if possible, because it does not rely on unsafe access through reflection.
	  */
	trait GenericLikeIterable[X, -Xs <: IterableOps[X, CC, CC[X]], +CC[_]]
		extends FromIterableFactory[X, Xs, CC, CC[X]]
		   with GenericLikeIterableOnce[X, Xs, CC] with GenericLikeIterableOnceBuilder[X, Xs, CC]



	/** Implements most of `LikeIterable` methods only in terms of `IterableOnce` methods
	  * (as returned by `toIterableOnce(elems)`), and `iterator(elems)`. In particular, it does not depend
	  * on `toOps` or `toIterableOnceOps` methods, and those can be implemented by returning a delegate
	  * to this class in turn.
	  */
	trait FromIterator[X, -Xs, +CC[_], +C]
		extends LikeIterableOnce.FromIterator[X, Xs, CC, C] with LikeIterableBasics[X, Xs, CC, C]
	{ outer =>
		override def sizeCompare(elems :Xs, otherSize :Int) :Int = {
			if (otherSize == 0)
				if (isEmpty(elems)) 0 else 1
			else {
				var thisSize = knownSize(elems)
				if (thisSize >= 0)
					java.lang.Integer.compare(thisSize, otherSize)
				else {
					val i = iterator(elems)
					thisSize = 0
					while (thisSize < otherSize && i.hasNext) {
						thisSize += 1
						i.next()
					}
					if (thisSize < otherSize) -1
					else if (i.hasNext) 1
					else 0
				}
			}
		}
		override def sizeCompare[O](elems :Xs, that :O)(implicit likeIterable :LikeIterable[_, O, Any1, _]) :Int = {
			val thatSize = likeIterable.knownSize(that)
			if (thatSize >= 0)
				sizeCompare(elems, thatSize)
			else {
				val thisSize = knownSize(elems)
				if (thisSize >= 0)
					likeIterable.sizeCompare(that, thisSize)
				else {
					val these = iterator(elems)
					val those = likeIterable.iterator(that)
					var cont1, cont2 = false
					while ({
						cont1 = these.hasNext
						cont2 = those.hasNext
						cont1 & cont2
					}) {
						these.next()
						those.next()
					}
					java.lang.Boolean.compare(cont1, cont2)
				}
			}
		}

		override def head(elems :Xs) :X = iterator(elems).next()
		override def last(elems :Xs) :X = iterator(elems).last()

		override def headOption(elems :Xs) :Option[X] =
			if (knownSize(elems) == 0) None
			else iterator(elems).nextOption()

		override def lastOption(elems :Xs) :Option[X] =
			if (knownSize(elems) == 0) None
			else iterator(elems).lastOption()

		//The implementation from FromIterator is better than the overriding one in LikeIterableBasics,
		// which we mix in later, because, in turn, its isEmpty is better, and Basics mixins have in general
		// a greater potential for offering better implementations.
		override def empty(elems :Xs) :C = makeSpecific(elems)(Iterator.empty)
		override def copy(elems :Xs) :C = makeSpecific(elems)(toIterableOnce(elems))
		override def tail(elems :Xs) :C = makeSpecific(elems)(iterator(elems).skip())
		override def init(elems :Xs) :C = knownSize(elems) match {
			case 0 => unsupported_!(infoString(elems) + ".init")
			case 1 => empty(elems)
			case n =>
				val it = iterator(elems)
				if (!it.hasNext)
					unsupported_!(infoString(elems) + ".init")
				dropRight(elems, it, n, 1)
		}

		override def takeRight(elems :Xs, n :Int) :C = knownSize(elems) match {
			case   _ if n <= 0 => empty(elems)
			case  -1           => makeSpecific(elems)(iterator(elems).takeRight(n))
			case   0           => copy(elems)
			case len           => drop(elems, len - n)
		}

		override def dropRight(elems :Xs, n :Int) :C =
			if (n <= 0) toSpecific(elems) else dropRight(elems, iterator(elems), knownSize(elems), n)

		private def dropRight(elems :Xs, it :Iterator[X @uncheckedVariance], size :Int, n :Int) :C = {
			val buff = DefaultBuffer.empty[X]
			if (size >= 0) {
				var rem = size - n
				buff trySizeHint rem
				while (rem > 0 && it.hasNext) {
					buff += it.next()
					rem -= 1
				}
			} else {
				it.foldLeft(buff)(_.prepend(_))
				buff.remove(buff.length - n)
			}
			makeSpecific(elems)(buff)
		}
		override def partition(elems :Xs)(p :X => Boolean) :(C, C) = {
			val (l, r) = iterator(elems).partition(p)
			(makeSpecific(elems)(l), makeSpecific(elems)(r))
		}

		override def tapEach[U](elems :Xs)(f :X => U) :C = makeSpecific(elems)(view(elems).map { x => f(x); x })


		override def scanRight[A](elems :Xs)(z :A)(op :(X, A) => A) :CC[A] = elems match {
			case _ if isEmpty(elems) =>
				makeGeneric(elems)(Iterator.single(z))
			//It is possible that we are a weird type class of a different element type, so lets check
			// if the elements of the collection are 'our' elements. This is an approximation
			// that can still bee incorrect for some implementations, so they'll have to override scanRight themselves.
			case seq :collection.SeqOps[X, Any1, _] @unchecked if seq.head boxeq head(elems) =>
				makeGeneric(elems)(seq.reverseIterator.scanLeft(z)((a, x) => op(x, a)))
			case _ =>
				makeGeneric(elems)(TemporaryBuffer.from(elems).scanRight(z)(op))
		}

		override def concat[U >: X, O](elems :Xs, suffix :O)(implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			makeGeneric(elems)(iterator(elems) :++ likeCollection.iterator(suffix))

		override def zip[U >: X, A, O](elems :Xs, that :O)(implicit likeCollection :LikeCollection[A, O]) :CC[(U, A)] =
			makeGeneric(elems)(iterator(elems).zip(likeCollection.iterator(that)))

		override def zipAll[U >: X, A, O](elems :Xs, that :O, thisElem :U, thatElem :A)
		                                 (implicit likeIterable :LikeIterable[A, O, Any1, _]) :CC[(U, A)] =
			makeGeneric(elems)(iterator(elems).zipAll(likeIterable.iterator(that), thisElem, thatElem))

		override def partitionMap[A1, A2](elems :Xs)(f :X => Either[A1, A2]) :(CC[A1], CC[A2]) = {
			val (l, r) = iterator(elems).map(f).partition(_.isLeft)
			(makeGeneric(elems)(l.map { case Left(v) => v}) : @nowarn,
				makeGeneric(elems)(r.map { case Right(v) => v }) : @nowarn
			)
		}
		override def groupBy[K](elems :Xs)(f :X => K) :Map[K, C] =
			iterator(elems).foldLeft(Map.empty[K, Buffer[X]]) { (map, elem) =>
				val key = f(elem)
				val coll = map.getOrElse(key, DefaultBuffer.empty[X])
				coll += elem
				map.updated(key, coll)
			}.iterator.map { case (key, values) => (key, makeSpecific(elems)(values)) }.toMap

		override def groupMap[K, A](elems :Xs)(key :X => K)(f :X => A) :Map[K, CC[A]] =
			iterator(elems).foldLeft(Map.empty[K, Buffer[X]]) { (map, elem) =>
				val k = key(elem)
				val coll = map.getOrElse(k, DefaultBuffer.empty[X])
				coll += elem
				map.updated(k, coll)
			}.iterator.map { case (key, values) => (key, makeGeneric(elems)(values.iterator.map(f))) }.toMap

		override def groupMapReduce[K, A](elems :Xs)(key :X => K)(f :X => A)(reduce :(A, A) => A) :Map[K, A] =
			iterator(elems).foldLeft(Map.empty[K, Buffer[X]]) { (map, elem) =>
				val k = key(elem)
				val coll = map.getOrElse(k, DefaultBuffer.empty[X])
				coll += elem
				map.updated(k, coll)
			}.iterator.map { case (key, values) => (key, values.iterator.map(f).reduce(reduce)) }.toMap


		override def withFilter(elems :Xs)(p :X => Boolean) :WithFilter[X, CC] =
			new WithFilter[X, CC] {
				override def withFilter(q :X => Boolean) :WithFilter[X, CC] = outer.withFilter(elems)(x => p(x) && q(x))
				override def foreach[U](f :X => U) :Unit = iterator(elems).filter(p).foreach(f)
				override def map[B](f :X => B) :CC[B] = makeGeneric(elems)(iterator(elems).filter(p).map(f))
				override def flatMap[B](f :X => IterableOnce[B]) :CC[B] =
					makeGeneric(elems)(iterator(elems).filter(p).flatMap(f))
			}
		override def view(elems :Xs) :View[X] = View.fromIteratorProvider(() => iterator(elems))
	}



	/** An implementation of type class [[net.noresttherein.sugar.collections.LikeIterable LikeIterable]]
	  * for a collection type `Xs` of `X` values which delegates all calls to
	  * [[net.noresttherein.sugar.collections.LikeIterable.FromOps.toOps toOps]]`(elems)`,
	  * returning the standard Scala `IterableOps[X, CC, C]` interface for `elems`.
	  */
	trait FromOps[+X, -Xs, +CC[_], +C]
		extends LikeIterable[X, Xs, CC, C] with LikeIterableOnce.FromOps[X, Xs, CC, C]
	{
		override def sizeCompare(elems :Xs, otherSize :Int) :Int = toOps(elems).sizeCompare(otherSize)
		override def sizeCompare[O](elems :Xs, that :O)(implicit likeIterable :LikeIterable[_, O, Any1, _]) :Int = {
			val thatSize = likeIterable.knownSize(that)
			if (thatSize >= 0) toOps(elems).sizeCompare(thatSize)
			else toOps(elems).sizeCompare(likeIterable.toIterable(that))
		}

		override def head(elems :Xs) :X = toOps(elems).head
		override def last(elems :Xs) :X = toOps(elems).last
		override def headOption(elems :Xs) :Option[X] = toOps(elems).headOption
		override def lastOption(elems :Xs) :Option[X] = toOps(elems).lastOption

		override def empty(elems :Xs) :C = toOps(elems).empty
		override def tail(elems :Xs) :C = toOps(elems).tail
		override def init(elems :Xs) :C = toOps(elems).init
		override def takeRight(elems :Xs, n :Int) :C = toOps(elems).takeRight(n)
		override def dropRight(elems :Xs, n :Int) :C = toOps(elems).dropRight(n)
		override def partition(elems :Xs)(p :X => Boolean) :(C, C) = toOps(elems).partition(p)


		override def scanRight[A](elems :Xs)(z :A)(op :(X, A) => A): CC[A] = toOps(elems).scanRight(z)(op)

		override def concat[A >: X, O](elems :Xs, suffix :O)(implicit likeCollection :LikeCollection[A, O]) :CC[A] =
			toOps(elems).concat(likeCollection.toIterableOnce(suffix))

		override def zip[U >: X, A, O](elems :Xs, that :O)
		                              (implicit likeCollection :LikeCollection[A, O]) :CC[(U, A)] =
			(this :FromOps[U, Xs, CC, C]).toOps(elems).zip(likeCollection.toIterableOnce(that))

		override def zipAll[U >: X, A, O](elems :Xs, that :O, thisElem :U, thatElem :A)
		                                 (implicit likeIterable :LikeIterable[A, O, kinds.Any1, _]): CC[(U, A)] =
			toOps(elems).zipAll(likeIterable.toIterable(that), thisElem, thatElem)

		override def unzip[A1, A2](elems :Xs)(implicit asPair :X => (A1, A2)) :(CC[A1], CC[A2]) = toOps(elems).unzip

		override def unzip3[A1, A2, A3](elems :Xs)(implicit asTriple :X => (A1, A2, A3)) :(CC[A1], CC[A2], CC[A3]) =
			toOps(elems).unzip3


		override def partitionMap[A1, A2](elems :Xs)(f :X => Either[A1, A2]) :(CC[A1], CC[A2]) =
			toOps(elems).partitionMap(f)

		override def groupBy[K](elems :Xs)(f :X => K) :Map[K, C] = toOps(elems).groupBy(f)
		override def groupMap[K, A](elems :Xs)(key :X => K)(f :X => A) :Map[K, CC[A]] = toOps(elems).groupMap(key)(f)
		override def groupMapReduce[K, A](elems :Xs)(key :X => K)(f :X => A)(reduce :(A, A) => A) :Map[K, A] =
			toOps(elems).groupMapReduce(key)(f)(reduce)


		override def withFilter(elems :Xs)(p :X => Boolean) :WithFilter[X, CC] = toOps(elems).withFilter(p)

		override def copy(elems :Xs) :C = toOps(elems).filter(_ => true)
		override def view(elems :Xs) :View[X] = toOps(elems).view

		override def toGeneric[U >: X](elems :Xs) :CC[U] = {
			val ops = toOps(elems)
			ops.iterableFactory from ops
		}

		override def toOps(elems :Xs) :IterableOps[X, CC, C]
	}


	/** An implementation of `Iterable` methods for any `C <: IterableOps[E, CC, C]`. */
	trait ForOps[X, CC[x] <: Iterable[x], C <: CC[X] with IterableOps[X, CC, C]]
		extends FromOps[X, C, CC, C] with LikeIterableOnce.ForOps[X, CC, C]
	{
		override def toOps(elems :C) :IterableOps[X, CC, C] = elems
	}

}




private trait LikeIterableAdapter[+X, Xs, +CC[_], +C]
	extends IterableOps[X, CC, C] with LikeIterableOnceAdapter[X, Xs, CC, C]
{
//	protected override val ops :LikeIterable[X, Xs, CC, C]
	protected override val ops :LikeIterable[X, Xs, Any1, _]

	override def sizeCompare(otherSize :Int) :Int = ops.sizeCompare(elems, otherSize)
	override def sizeCompare(that :Iterable[_]) :Int = ops.sizeCompare(elems, that)

	override def head :X = ops.head(elems)
	override def headOption :Option[X] = ops.headOption(elems)
	override def last :X = ops.last(elems)
	override def lastOption :Option[X] = ops.lastOption(elems)

/*
	override def empty :C = ops.empty(elems)
	override def tail :C = ops.tail(elems)
	override def init :C = ops.init(elems)
	override def takeRight(n :Int) :C = ops.takeRight(elems)(n)
	override def dropRight(n :Int) :C = ops.dropRight(elems)(n)
	override def partition(p :X => Boolean) :(C, C) = ops.partition(elems)(p)

	override def scanRight[A](z :A)(op :(X, A) => A): CC[A] = ops.scanRight(elems)(z)(op)

	override def concat[A >: X](suffix :IterableOnce[A]) :CC[A] = ops.concat[A, IterableOnce[A]](elems)(suffix)
	override def zip[A](that :IterableOnce[A]) :CC[(X @uncheckedVariance, A)] = ops.zip(elems)(that)
	override def zipAll[U >: X, A](that :Iterable[A], thisElem :U, thatElem :A): CC[(U, A)] =
		ops.zipAll[U, A, Iterable[A]](elems)(that, thisElem, thatElem)

	override def unzip[A1, A2](implicit asPair :X => (A1, A2)) :(CC[A1], CC[A2]) = ops.unzip(elems)
	override def unzip3[A1, A2, A3](implicit asTriple :X => (A1, A2, A3)) :(CC[A1], CC[A2], CC[A3]) = ops.unzip3(elems)
	override def transpose[A](implicit asIterable :X => Iterable[A]) :CC[CC[A] @uncheckedVariance] = ops.transpose(elems)

	override def partitionMap[A1, A2](f :X => Either[A1, A2]) :(CC[A1], CC[A2]) = ops.partitionMap(elems)(f)
	override def groupBy[K](f :X => K) :Map[K, C] = ops.groupBy(elems)(f)
	override def groupMap[K, A](key :X => K)(f :X => A) :Map[K, CC[A]] = ops.groupMap(elems)(key)(f)
*/
	override def groupMapReduce[K, A](key :X => K)(f :X => A)(reduce :(A, A) => A) :Map[K, A] =
		ops.groupMapReduce(elems)(key)(f)(reduce)

	override def view :View[X] = ops.view(elems)
}
