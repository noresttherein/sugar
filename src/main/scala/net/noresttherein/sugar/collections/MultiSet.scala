package net.noresttherein.sugar.collections

import java.lang.{Math => math}

import scala.collection.{IterableFactory, IterableFactoryDefaults}
import scala.collection.immutable.{AbstractMap, AbstractSet, HashMap, HashSet}
import scala.collection.mutable.Builder

import net.noresttherein.sugar.extensions.{ArrayExtension, IteratorObjectExtension, PartialFunctionObjectExtension, castTypeParamMethods, classNameMethods}




/** The generic interface of [[net.noresttherein.sugar.collections.MultiSet MultiSet]].
  * @define Coll `MultiSet`
  * @define coll multi set
  */
trait MultiSetOps[X, +Multi[A] <: MultiSetOps[A, Multi]]
	extends SugaredIterableOps[X, Multi, Multi[X]] with IterableFactoryDefaults[X, Multi]
{
	/** Number of unique elements in this $coll. */
	def uniqueSize :Int = unique.size

	/** Total number of elements in this $coll, counting all repetitions.
	  * Equals [[net.noresttherein.sugar.collections.MultiSetOps.size size]] for values in the `Int` range.
	  */
	def totalSize :Long

	/** Total number of elements in this $coll, counting all repetitions. */
	override def size :Int = totalSize.toInt

	/** Returns the number of 'copies' of the argument in this $coll.
	  * If `!this.`[[net.noresttherein.sugar.collections.MultiSetOps.contains contains]]`(x)`, returns 0.
	  */
	def apply(x :X) :Int

	/** Checks if the argument is included in this $coll. */
	def contains(x :X) :Boolean = apply(x) > 0

	/** Checks if there are at least `count` copies of `x` in this $coll. */
	def contains(x :X, count :Int) :Boolean = apply(x) > count

	/** Checks if this $coll contains all the elements in the given collection.
	  * If the argument contains equal elements, this $coll must contain one copy for each among equal elements.
	  */
	def containsAll(xs :IterableOnce[X]) :Boolean = xs match {
		case empty :Iterable[X] if empty.isEmpty => true
		case multi :MultiSet[x] => multi.toSet.forall { x :x => apply(x) > multi(x) }
		case set :collection.Set[X] => set.forall(contains)
		case elems :Iterable[X] => containsAll(MultiSet.from(elems))
		case _  => containsAll(xs.iterator to MultiSet)
	}

	/** Checks if the argument contains all the elements in this $coll; for each present element, the number of copies
	  * in the argument must be not lower than in this $coll.
	  */
	def subsetOf(that :MultiSet[X]) :Boolean = forall(that(_) >= _)


	/** Same as [[net.noresttherein.sugar.collections.MultiSetOps.counts counts]]`.forall`,
	  * except it takes a two argument function, which makes possible passing lambdas in the placeholder syntax.
	  */
	def forall(pred :(X, Int) => Boolean) :Boolean   = counts forall pred.tupled

	/** Same as [[net.noresttherein.sugar.collections.MultiSetOps.counts counts]]`.exists`,
	  * except it takes a two argument function, which makes possible passing lambdas in the placeholder syntax.
	  */
	def exists(pred :(X, Int) => Boolean) :Boolean   = counts exists pred.tupled

	/** Same as [[net.noresttherein.sugar.collections.MultiSetOps.counts counts]]`.count`,
	  * except it takes a two argument function, which makes possible passing lambdas in the placeholder syntax.
	  */
	def count(pred :(X, Int) => Boolean)  :Int       = counts count pred.tupled

	/** Same as [[net.noresttherein.sugar.collections.MultiSetOps.counts counts]]`.find`,
	  * except it takes a two argument function, which makes possible passing lambdas in the placeholder syntax.
	  * The default implementation of `MultiSet` does not maintain a particular order of elements,
	  * and in case multiple elements satisfy the predicate, it is unspecified which will be returned.
	  */
	def find(pred :(X, Int) => Boolean)   :Option[X] = counts find pred.tupled map (_._1)

	/** Equivalent to [[net.noresttherein.sugar.collections.MultiSetOps.counts counts]]`.filter`,
	  * but returns a $Coll and takes a two argument function instead of one accepting a tuple,
	  * which makes it more readable and allows passing lambdas in the placeholder syntax.
	  */
	def filter(pred :(X, Int) => Boolean)    :Multi[X] = fromCounts(countsIterator filter pred.tupled)

	/** Equivalent to [[net.noresttherein.sugar.collections.MultiSetOps.counts counts]]`.filterNot`,
	  * but returns a $Coll and takes a two argument function instead of one accepting a tuple,
	  * which makes it more readable and allows passing lambdas in the placeholder syntax.
	  */
	def filterNot(pred :(X, Int) => Boolean) :Multi[X] = fromCounts(countsIterator filterNot pred.tupled)

	//Consider: I trully don't know if (X, Int) => IterableOnce[T] or (X, Int) => IterableOnce[T] is more useful.
	// The variant returning a MultiSet seems less useful, but if we call view on ourselves, allowing to return
	// a collection of any type, it suddenly gives much more possibilities.
	/** Equivalent to [[net.noresttherein.sugar.collections.MultiSetOps.counts counts]]`.map`,
	  * but returns a $Coll and takes a two argument function instead of one accepting a tuple,
	  * which makes it more readable and allows passing lambdas in the placeholder syntax.
	  * Note that the accepted function returns a single element.
	  */
	def map[T](f :(X, Int) => T) :Multi[T] = iterableFactory.from(countsIterator map f.tupled)

//	/** Equivalent to the other `map`, but accepts a two argument function instead of one accepting a `(_, Int)`,
//	  * which makes it more convenient to call passing a lambda in the placeholder syntax
//	  * and makes it slightly more readable.
//	  */
//	def map[T](f :(X, Int) => (T, Int)) :Multi[T] = map(tuplize(f) _)

	/** Equivalent to [[net.noresttherein.sugar.collections.MultiSetOps.counts counts]]`.map`,
	  * but the method creates a $Coll and the argument function must return pairs with an `Int` as the second element.
	  * Like in other methods, elements paired with negative numbers are ignored.
	  */
	def map[T](f :((X, Int)) => (T, Int)): Multi[T] = fromCounts(countsIterator.map(f))

	/** Equivalent to [[net.noresttherein.sugar.collections.MultiSetOps.counts counts]]`.flatMap`,
	  * but returns a $Coll and takes a two argument function instead of one accepting a tuple,
	  * which makes it more readable and allows passing lambdas in the placeholder syntax.
	  * Note that the accepted function returns a single element.
	  */
	def flatMap[T](f :(X, Int) => IterableOnce[T]) :Multi[T] = iterableFactory.from(countsIterator flatMap f.tupled)

//	/** Equivalent to the other `flatMap`, but accepts a two argument function instead of one accepting a `(_, Int)`,
//	  * which makes it more convenient to call passing a lambda in the placeholder syntax
//	  * and makes it slightly more readable.
//	  */
//	def flatMap[T](f :(X, Int) => IterableOnce[(T, Int)]) :Multi[T] =  flatMap(tuplize(f) _)

	/** Equivalent to [[net.noresttherein.sugar.collections.MultiSetOps.counts counts]]`.flatMap`,
	  * but the method creates a $Coll and the argument function must return pairs with an `Int` as the second element.
	  * Like in other methods, elements paired with negative numbers are ignored.
	  */
	def flatMap[T](f :((X, Int)) => IterableOnce[(T, Int)]) :Multi[T] = fromCounts(countsIterator flatMap f)

	/** Adds a single copy of the given elemnt from this $coll. */
	@inline final def +(x :X) :Multi[X] = incl(x)

	/** Removes a single occurrence of the given elemnt from this $coll. */
	@inline final def -(x :X) :Multi[X] = excl(x)

	/** If `x` is not present in this $coll, a single copy of it is added; otherwise ''all'' current copies are removed. */
	@inline final def ^(x :X) :Multi[X] = flip(x)

	/** Adds the specified number of copies of the given element to this $coll.
	  * If `xs._2 <= 0` the call has no effect.
	  */
	@inline final def |(xs :(X, Int))  :Multi[X] = union(xs)

	/** Returns either an empty $coll,
	  * if `!this.`[[net.noresttherein.sugar.collections.MultiSetOps.contains contains]]`(xs._1) || xs._2 <= 0`,
	  * or returns an instance containing a single unique element
	  * in [[net.noresttherein.sugar.collections.MultiSetOps.apply apply]]`(xs._1) min xs._2` copies.
	  */
	@inline final def &(xs :(X, Int))  :Multi[X] = intersect(xs)

	/** Removes the specified number of copies of the given element from this $coll.
	  * If `xs._2 <= 0` the call has no effect.
	  */
	@inline final def &~(xs :(X, Int)) :Multi[X] = diff(xs)

	/** A symmetric difference between this $coll and the argument, working in an analogous fashion
	  * to symmetric difference on sets. The returned $Coll contains all copies of items in either sets which are unique
	  * to one set, and shared items in `(this(x) - that(x)).abs` copies.
	  */
	@inline final def ^(that :MultiSet[X])        :Multi[X] = symDiff(that)

	/** Equivalent to `this [[net.noresttherein.sugar.collections.&~ &~]]` that | that &~ this.toSet`.
	  * The returned $Coll contains a single copy of each of the elements in the argument which is not present
	  * in this $coll, as well as all the copies of each element unique to this instance.
	  */
	@inline final def ^(that :collection.Set[X])  :Multi[X] = symDiff(that)

	/** Computes a union of two multi sets. The returned $Coll contains each of the elements present in either
	  * of the collections, in `this(x) + that(x)` copies.
	  */
	@inline final def |(that :MultiSet[X])        :Multi[X] = union(that)

	/** Computes a union of this $coll and a set, treated as a multi set with a single occurrence of each element. */
	@inline final def |(that :collection.Set[X])  :Multi[X] = union(that)

	/** Computes the intersection of this $coll and the argument multi set. The returned $Coll contains only elements
	  *  present in both collections, each in `this(x) min that(x)` copies.
	  */
	@inline final def &(that :MultiSet[X])        :Multi[X] = intersect(that)

	/** Computes the intersection of this $coll and a multi set with an infinite copies of each of the elements
	  * in the argument set. The returned $Coll contains all elements of this instance which are present
	  * in the argument, in the same number of copies.
	  */
	@inline final def &(that :collection.Set[X])  :Multi[X] = intersect(that)

	/** Computes the difference of this $coll and the argument.
	  * The returned $Coll contains elements present in this multi set in a number of copies greater than in `that`
	  * (including all those not present in `that`), each in `this(x) - that(x)` copies.
	  */
	@inline final def &~(that :MultiSet[X])       :Multi[X] = diff(that)

	/** Computes the difference of this $coll and a multi set containing an infinite number of copies of each
	  * of the elements in the argument set. The returned $Coll contains only those items from `this`
	  * which are not present in `that`, in the same number of copies as in `this`.
	  */
	@inline final def &~(that :collection.Set[X]) :Multi[X] = diff(that)

	/** Removes all of the elements of the collection from this $coll. Each item in `that` removes
	  * only one copy from this instance (providing it exists at all), with the result containing less of each
	  * element in the number of its occurrences in `that`.
	  */
	@inline final def --(that :IterableOnce[X])   :Multi[X] = removedAll(that)

	/** Sets the number of copies of `xs._1` in this $coll to `xs._2`. The returned $Coll equals
	  * `this.`[[net.noresttherein.sugar.collections.MultiSetOps.excl(x:X) excl]]`(xs._1).`[[net.noresttherein.sugar.collections.MultiSetOps.inc inc]]`(xs._2)`.
	  * If `xs._2 <= 0`, all copies of `xs._1` are removed.
	  */
	@inline final def reset(xs :(X, Int))   :Multi[X] = reset(xs._1, xs._2)

	/** Increases/decreases the number of copies of `xs._1` in this $coll by the delta `xs._2`.
	  * If `xs._2 >= this(xs._1)`, the returned $Coll will contain no copy of `xs._1`
	  * (a multi set with a 'negative number of copies' is impossible).
	  */
	@inline final def updated(xs :(X, Int)) :Multi[X] = updated(xs._1, xs._2)

	/** Increases the number of copies of `xs._1` in this $coll by the specified amount `xs._2`.
	  * If `xs._2 <= 0` the call has no effect.
	  */
	@inline final def inc(xs :(X, Int))     :Multi[X] = inc(xs._1, xs._2)

	/** Decreases the number of copies of `xs._1` in this $coll by the specified amount `xs._2`.
	  * If `xs._2 <= 0` the call has no effect.
	  */
	@inline final def dec(xs :(X, Int))     :Multi[X] = dec(xs._1, xs._2)

	/** Sets the number of copies of `x` in this $coll to `count`. The returned $Coll equals
	  * `this.`[[net.noresttherein.sugar.collections.MultiSetOps.excl(x:X) excl]]`(x).`[[net.noresttherein.sugar.collections.MultiSetOps.inc inc]]`(count)`.
	  * If `count <= 0`, all copies of `x` are removed.
	  */
	def reset(x :X, count :Int)      :Multi[X]

	/** Sets the number of copies in this $coll to the value returned by `f(this(x))`.
	  * @return [[net.noresttherein.sugar.collections.MultiSetOps.reset(x:X,count:Int)* reset]]`(x, f(this(x)))`.
	  */
	def reset(x :X)(f :Int => Int)   :Multi[X] = reset(x, f(apply(x)))

	/** Increases/decreases the number of copies of `x` in this $coll by `delta`.
	  * If `delta >= this(xs._1)`, the returned $Coll will contain no copy of `x`
	  * (a multi set with a 'negative number of copies' is impossible). This call is equivalent to
	  * either [[net.noresttherein.sugar.collections.MultiSetOps.inc inc]]
	  * or [[net.noresttherein.sugar.collections.MultiSetOps.dec dec]],
	  * depending on whether `delta` is negative or positive.
	  */
	def updated(x :X, delta :Int)    :Multi[X] = reset(x, plus(apply(x), delta))

	/** Updates the number of copies in this $coll to the value returned by `f(this(x))`.
	  * @return [[net.noresttherein.sugar.collections.MultiSetOps.updated(x:X,count:Int)* updated]]`(x, f(this(x)))`.
	  */
	def updated(x :X)(f :Int => Int) :Multi[X] = updated(x, f(apply(x)))

	/** Increases the number of copies of `x` in `this` by the specified amount. If `count <= 0` the call has no effect;
	  * if the `this(x) + count` overflows, the number of copies are set to `Int.MaxValue` instead.
	  */
	def inc(x :X, count :Int) :Multi[X] = if (count > 0) updated(x, count) else coll

	/** Decreases the number of copies of `x` in `this` by the specified amount. If `count <= 0` the call has no effect.
	  * If `count > this(x)`, the set will contain zero copies of `x`
	  */
	def dec(x :X, count :Int) :Multi[X] = if (count > 0) updated(x, -count) else coll

	/** Adds a single copy of `x` to this $coll. */
	def incl(x :X)            :Multi[X] = inc(x, 1)

	/** Removes ''all'' copies of `x` from this $coll. */
	def excl(x :X)            :Multi[X] = dec(x, Int.MaxValue)

	/** If this $coll contains `x`, all its copies are removed. Otherwise a single copy is added to the collection. */
	def flip(x :X)            :Multi[X] = if (contains(x)) dec(x, Int.MaxValue) else inc(x, 1)


	def symDiff(xs :(X, Int)) :Multi[X] =
		if (xs._2 <= 0)
			coll
		else
			apply(xs._1) match {
				case n if n >= xs._2 => dec(xs._1, n - xs._2)
				case n               => inc(xs._1, xs._2 - n)
			}
	def symDiff(that :collection.Set[X]) :Multi[X] = diff(that) | that.diff(toSet)
	def symDiff(that :MultiSet[X])       :Multi[X] = diff(that) | that.diff(toSet)
	def diff(xs :(X, Int))               :Multi[X] = dec(xs._1, xs._2)
	def diff(that :collection.Set[X])    :Multi[X] = fromCounts(counts.view.filterKeys(!that(_)))
	def diff(that :MultiSet[X])          :Multi[X] = {
		val collector = PartialFunction[(X, Int), (X, Int)] {
			(x :(X, Int), default :((X, Int)) => (X, Int)) =>
				val count = x._2 - that(x._1)
				if (count < 0) default(x)
				else (x._1, count)
		}
		fromCounts(counts.view.collect(collector))
	}
	def union(xs :(X, Int))            :Multi[X] = inc(xs._1, xs._2)
	def union(that :collection.Set[X]) :Multi[X] = union(MultiSet.from(that))
	def union(that :MultiSet[X])       :Multi[X] =
		fromCounts(
			counts.view.map { case (x, count) => (x, count + that(x)) } ++
				that.counts.view.filterKeys(apply(_) == 0)
		)

	def intersect(xs :(X, Int))            :Multi[X] //= intersect(iterableFactory fromCounts Iterator.single(xs))
	def intersect(that :collection.Set[X]) :Multi[X] = fromCounts(counts.view.filterKeys(that))
	def intersect(that :MultiSet[X])       :Multi[X] = {
		val collector = PartialFunction {
			(xs :(X, Int), default :((X, Int)) => (X, Int)) =>
				val count = plus(xs._2, -that(xs._1))
				if (count <= 0) default(xs) else (xs._1, count)
		}
		fromCounts(counts.collect(collector))
	}
	def removedAll(that :IterableOnce[X])  :Multi[X] = that match {
		case empty :Iterable[_] if empty.isEmpty => coll
		case multi :MultiSet[X @unchecked] => diff(multi)
		case other => diff(MultiSet.from(other))
	}

	def counts :Map[X, Int] = new AbstractMap[X, Int] {
		override def removed(key :X) :Map[X, Int] = dec(key, 1).counts
		override def updated[V1 >: Int](key :X, value :V1) :Map[X, V1] = value match {
			case count :Int if count <= 0 && apply(key) <= 0 => this
			case count :Int if apply(key) == count => this
			case count :Int => updated(key, count)
			case _ => (Map.newBuilder[X, V1] ++= this += ((key, value))).result()
		}
		override def apply(key :X) :Int = MultiSetOps.this.apply(key)
		override def contains(key :X) :Boolean = MultiSetOps.this.contains(key)
		override def get(key :X) :Option[Int] = Some(apply(key))

		override def iterator :Iterator[(X, Int)] = uniqueIterator.map(x => (x, apply(x)))
	}

	/** Equivalent to [[scala.collection.Iterable.toSet toSet]], but may be faster. */
	def unique :Set[X] = new AbstractSet[X] {
		override def incl(elem :X) :Set[X] =
			if (MultiSetOps.this(elem) > 0) this else inc(elem, 1).unique

		override def excl(elem :X) :Set[X] =
			if (MultiSetOps.this(elem) > 0) dec(elem, Int.MaxValue).unique else this

		override def contains(elem :X) :Boolean = MultiSetOps.this(elem) > 0
		override def iterator :Iterator[X] = uniqueIterator
	}

	def countsIterator :Iterator[(X, Int)] = counts.iterator
	def uniqueIterator :Iterator[X]

	override def iterator :Iterator[X] = new Iterator[X] {
		private[this] val i = counts.iterator
		private[this] var count = -1
		private[this] var head :X = _
		override def hasNext = count > 0 || i.hasNext
		override def next() =
			if (count > 0) {
				count -= 1; head
			} else {
				val entry = i.next()
				head = entry._1; count = entry._2 - 1
				head
			}
	}

	protected def fromCounts[T](trustedCounts :IterableOnce[(T, Int)]) :Multi[T] =
		iterableFactory fromCounts trustedCounts

	override def iterableFactory :MultiSetFactory[Multi]

	@inline private def plus(x :Int, y :Int) :Int =
		if (x < 0)
			if (y < Int.MinValue - x) Int.MinValue else x + y
		else
			if (y > Int.MaxValue - x) Int.MaxValue else x + y

}




/** A $Coll is an `Iterable` which aliases repetitions of the same elements (as defined by equals).
  * It is essentially a `Map[A, Int]`, where the value is always non-negative and represents the number of repetitions
  * of the key in the collection, but viewed as a collection of `A`. Its interface mirrors that
  * of [[scala.collection.immutable.Set Set]], where set operations usually have three overloaded variants:
  *   1. taking a $Coll argument, in which intersect, union, etc. result in a $coll with repetition
  *      numbers being the delta of the appropriate arithmetic operation;
  *   1. taking a `Set` argument, in which the set is treated as a $coll without repetitions
  *      for [[net.noresttherein.sugar.collections.MultiSetOps.union union]], but a $coll with an infinite number
  *      of repetitions of each element for [[net.noresttherein.sugar.collections.MultiSetOps.intersect intersect]],
  *      [[net.noresttherein.sugar.collections.MultiSetOps.diff diff]].
  *   1. taking an `xs :(A, Int)` argument, which is treated as a $coll consisting of a single element `xs._1`
  *      repeated `xs._2` number of times.
  *
  * Additionally, most commonly used methods of `Iterable` which accept a function `[X] A => X`,
  * have also a two argument variant `[X] (A, Int) => X` in $Coll. The default operation does not overflow
  * (or underflow, for that matter), instead saturating the number of repeats at `Int.MaxValue` if that was to happen.
  *
  * Aside from direct use as a collection, a $Coll is well suited for counting number of repetitions in another iterable:
  * `(coll to MultiSet).`[[net.noresttherein.sugar.collections.MultiSetOps.counts counts]] returns a `Map[A, Int]`.
  * It provides fast lookup, conversion to Map [[net.noresttherein.sugar.collections.MultiSetOps.counts counts]],
  * and [[scala.collection.Iterable.toSet toSet]].
  * @see [[net.noresttherein.sugar.collections.MultiSetOps.unique]]
  * @see [[net.noresttherein.sugar.collections.MultiSetOps.counts]]
  * @author Marcin Mościcki
  */
trait MultiSet[X] extends SugaredIterable[X] with MultiSetOps[X, MultiSet] {
	override def iterableFactory :MultiSetFactory[MultiSet] = MultiSet

	override def intersect(xs :(X, Int)) :MultiSet[X] =
		if (xs._2 <= 0) MultiSet.empty
		else MultiSet.single(xs._1, math.min(apply(xs._1), xs._2))

	protected override def fromCounts[T](trustedCounts :IterableOnce[(T, Int)]) :MultiSet[T] =
		MultiSet.trustedCounts(trustedCounts)


	override def equals(that :Any) :Boolean = that match {
		case self   :AnyRef if this eq self => true
		case empty  :MultiSet[_] if empty.sizeIs == 0 => isEmpty
		case single :MultiSet[x] if single.uniqueSize == 1 && (single canEqual this) =>
			uniqueSize == 1 && head == single.head && single(single.head) == apply(head)
		case multi :MultiSet[X @unchecked] if multi.canEqual(this) =>
			uniqueSize == multi.uniqueSize && multi.uniqueIterator.forall(x => apply(x) == multi(x))
		case _ => false
	}
	def canEqual(that :Any) :Boolean = that.isInstanceOf[MultiSet[_]]
	override def hashCode :Int = counts.hashCode

	protected override def className = "MultiSet"

	override def toString :String =
		uniqueIterator.map(x => x.toString + ": " + apply(x)).mkString(className + "(", ", ", ")")
}




/** Interface trait for companion objects of [[net.noresttherein.sugar.collections.MultiSet MultiSet]]
  * implementations.
  * @define Coll `MultiSet`
  * @define coll multi set
  */
trait MultiSetFactory[+M[A] <: MultiSetOps[A, M]] extends IterableFactory[M] {
	override def from[A](source :IterableOnce[A]) :M[A] = (newBuilder[A] ++= source).result()

	/** Creates a $coll containing the first elements of the pairs, each repeated the number of times
	  * equal to the total sum of the second elements in its pairs. Pairs with negative numbers are ignored.
	  */
	def fromCounts[A](counts :IterableOnce[(A, Int)]) :M[A] = counts match {
		case empty :Iterable[_] if empty.isEmpty => MultiSetFactory.this.empty
		case empty :Iterator[_] if !empty.hasNext => MultiSetFactory.this.empty
		case elems :Iterable[(A, Int)] if elems.forall(_._2 > 0) => trustedCounts(elems)
		case _ => (multiBuilder[A] ++= counts).result()
	}

	/** Creates a $coll containing the first elements of the pairs, each repeated the number of times
	  * equal to the total sum of the second elements in its pairs. Pairs with negative numbers are ignored.
	  * @return [[net.noresttherein.sugar.collections.MultiSetFactory.fromCounts fromCounts]]`(counts)`.
	  */
	def count[A](counts :(A, Int)*) :M[A] = fromCounts(counts)

	/** Fast variant of [[net.noresttherein.sugar.collections.MultiSetFactory.fromCounts fromCounts]] uses
	  * by the companion classes. It assumes that all values in the sequence are non negative and no element
	  * occurs in more than one pair.
	  */
	protected def trustedCounts[A](counts :IterableOnce[(A, Int)]) :M[A]

	/** A builder for a $Coll, to which elements are added each with a number of its repetitions.
	  * In case of duplicates, the final result equals the total of all added pairs, but pairs with negative
	  * second elements are ignored outright.
	  */
	def multiBuilder[A] :Builder[(A, Int), M[A]] = //Map.newBuilder.mapResult(fromCounts)
		new Builder[(A, Int), M[A]] {
			private[this] var counts = Map.empty[A, Int]

			override def addOne(elem :(A, Int)) = {
				if (elem._2 > 0)
					counts = counts.updated(elem._1, elem._2 + counts.getOrElse(elem._1, 0))
				this
			}
			override def clear() :Unit = counts = Map.empty[A, Int]
			override def result() = trustedCounts(counts)
		}

	override def newBuilder[A] :Builder[A, M[A]] =
		new Builder[A, M[A]] {
			private[this] var counts = Map.empty[A, Int]

			override def addOne(elem :A) = {
				counts = counts.updated(elem, counts.getOrElse(elem, 0) + 1)
				this
			}
			override def result() = if (counts.isEmpty) empty else trustedCounts(counts)
			override def clear() :Unit = counts = Map.empty[A, Int]
		}
}


/** $factoryInfo */
@SerialVersionUID(Ver)
case object MultiSet extends MultiSetFactory[MultiSet] {

	override def from[A](source :IterableOnce[A]) :MultiSet[A] = source match {
		case multi :MultiSet[A @unchecked] => multi
		case empty :Iterable[A] if empty.knownSize == 0 => this.empty[A]
		case one   :Iterable[A] if one.knownSize == 1 => single(one.head, 1)
		case set   :HashSet[A] => new Unique(set)
		case _ => (newBuilder[A] ++= source).result()
	}
	override def fromCounts[A](counts :IterableOnce[(A, Int)]) :MultiSet[A] = counts match {
		case elems :Iterable[(A, Int)] => fromMap(elems.toMap)
		case _ => fromMap(counts.iterator.toMap)
	}
	//We can either allow negative counts and be fast, or have fast uniqueSize and counts
	def fromMap[A](counts :Map[A, Int]) :MultiSet[A] =
		if (counts.isEmpty) empty[A]
		else if (counts.sizeIs == 1) single(counts.head)
		else if (counts.forall(_._2 > 0)) new MapAdapter(counts)
		else new MapAdapter(counts.filter(_._2 > 0))

	protected override def trustedCounts[A](counts :IterableOnce[(A, Int)]) :MultiSet[A] = counts match {
		case elems :Iterable[(A, Int)] if elems.isEmpty => empty
		case elems :Iterator[(A, Int)] if !elems.hasNext => empty
		case elems :Iterable[(A, Int)] if elems.sizeIs == 1 => single(elems.head)
		case elems :Iterable[(A, Int)] => new MapAdapter(elems.toMap)
		case _ => new MapAdapter(counts.iterator.toMap)
	}

	override def empty[A] :MultiSet[A] = Empty.asInstanceOf[MultiSet[A]]

	def single[A](x :A, count :Int) :MultiSet[A] =
		if (count <= 0) Empty.castParam[A] else new Singleton(x, count)

	@inline final def single[A](xs :(A, Int)) :MultiSet[A] = single(xs._1, xs._2)


	@SerialVersionUID(Ver)
	private class Empty[X] extends MultiSet[X] with EmptyIterableOps[X, MultiSet, MultiSet[X]] {
		override def totalSize = 0L
		override def uniqueSize = 0
		override def apply(x :X) = 0
		override def subsetOf(that :MultiSet[X]) = true
		override def counts = Map.empty[X, Int]
		override def unique = Set.empty[X]
		override def uniqueIterator = Iterator.empty

		override def reset(x :X, count :Int) :MultiSet[X] = single(x, count)
		override def updated(x :X, count :Int) = single(x, count)
		override def inc(x :X, count :Int) = single(x, count)
		override def dec(x :X, count :Int) = this
		override def diff(xs :(X, Int)) = this
		override def diff(xs :MultiSet[X]) = this
		override def diff(xs :collection.Set[X]) = this
		override def symDiff(xs :(X, Int)) = single(xs)
		override def symDiff(xs :MultiSet[X]) = xs
		override def symDiff(xs :collection.Set[X]) = from(xs)
		override def union(xs :(X, Int)) = single(xs)
		override def union(xs :MultiSet[X]) = xs
		override def union(xs :collection.Set[X]) = from(xs)
		override def intersect(xs :(X, Int)) = this
		override def intersect(xs :MultiSet[X]) = this
		override def intersect(that :collection.Set[X]) = this

		override def toString = "MultiSet()"
	}
	private val Empty :Empty[Any] = new Empty[Any] {
		private def readResolve = MultiSet.Empty
	}


	@SerialVersionUID(Ver)
	private class Singleton[X](override val head :X, override val knownSize :Int) extends MultiSet[X] {
		assert(knownSize > 0)
		override def totalSize = knownSize
		override def uniqueSize = 1
		override def last = head
		override def apply(x :X) = if (x == head) size else 0
		override def subsetOf(that :MultiSet[X]) :Boolean = that(head) >= size

		override def iterator = Iterator.const(knownSize, head)
		override def uniqueIterator = Iterator.single(head)
		override def toSet[B >: X] :Set[B] = Set.empty[B] + head
		override def unique :Set[X] = Set.empty[X] + head
		override def counts = new Map.Map1(head, knownSize)

		override def reset(x :X, count :Int) :MultiSet[X] =
			if (x == head)
				if (count == knownSize) this else single(x, count)
			else if (count <= 0)
				this
			else
				new MapAdapter(new Map.Map2(head, knownSize, x, count))

		override def copyRangeToArray[B >: X](xs :Array[B], start :Int, from :Int, until :Int) :Int =
			if (until <= from | until <= 0 || from >= knownSize || start >= xs.length)
				0
			else if (start < 0)
				throw new IndexOutOfBoundsException(
					s"$this.copyRangeToArray(${xs.className}<${xs.length}>, $start, $from, $until)"
				)
			else {
				val from0 = math.max(from, 0)
				val until0 = math.min(until, knownSize)
				val copied = math.min(until0 - from0, xs.length - start)
				xs.fill(from0, from0 + copied, head)
				copied
			}

		override def copyToArray[B >: X](xs :Array[B], start :Int, len :Int) :Int =
			copyRangeToArray(xs, start, 0, len)

		override def toString = "MultiSet(" + head + ": " + size + ")"
	}


	//A set pretending to be a map with all values equal 1
	@SerialVersionUID(Ver)
	private class SetMap[X](underlying :Set[X]) extends AbstractMap[X, Int] {
		override def removed(key :X) :Map[X, Int] =
			if (underlying.contains(key)) new SetMap(underlying - key) else this

		override def updated[V1 >: Int](key :X, value :V1) :Map[X, V1] = value match {
			case n :Int if n <= 0 && underlying.contains(key) => new SetMap(underlying - key)
			case n :Int if n <= 0 => this
			case 1 => new SetMap(underlying + key)
			case _ => (Map.newBuilder[X, V1] ++= this += ((key, value))).result()
		}
		override def apply(key :X) = if (underlying(key)) 1 else 0
		override def contains(key :X) = underlying(key)
		override def get(key :X) :Option[Int] = Some(apply(key))

		override def iterator :Iterator[(X, Int)] = underlying.iterator.map((_, 1))
	}


	//hash set because we treat it as covariant
	@SerialVersionUID(Ver)
	private class Unique[X](override val unique :HashSet[X]) extends MultiSet[X] { self =>
		override def knownSize  = unique.knownSize
		override def totalSize  = unique.size.toInt
		override def uniqueSize = unique.size

		override def apply(x :X) :Int = if (toSet(x)) 1 else 0
		override def subsetOf(that :MultiSet[X]) :Boolean = unique.forall(that.contains)

		override def counts :Map[X, Int] = new SetMap(unique)
		override def reset(x :X, count :Int) :MultiSet[X] =
			if (count <= 0)
				if (unique.contains(x)) new Unique(unique - x) else this
			else if (count == 1)
				if (unique.contains(x)) this else new Unique(unique + x)
			else (multiBuilder[X] ++= counts += ((x, count))).result()

		override def iterator :Iterator[X] = unique.iterator
		override def uniqueIterator = unique.iterator
		override def toSet[B >: X] :Set[B] = unique.castParam[B]
		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :Unique[_] => unique == other.unique
			case _ => super.equals(that)
		}
//		override def toString = unique.view.map(_.toString + ": 1").mkString("MultiSet(", ", ", ")")
	}


	@SerialVersionUID(Ver)
	private class MapAdapter[X](override val counts :Map[X, Int], initSize :Long = -1L) extends MultiSet[X] {
		@volatile private[this] var _totalSize = initSize

		override def knownSize :Int = if (_totalSize > Int.MaxValue) -1 else _totalSize.toInt
		override def totalSize = _totalSize match {
			case -1 => _totalSize = (0L /: counts)(_ + _._2); _totalSize
			case n => n
		}
		override def apply(x :X) :Int = counts.getOrElse(x, 0)

		override def reset(x :X, count :Int) :MultiSet[X] =
			if (count <= 0) counts.getOrElse(x, 0) match {
				case 0 => this
				case n => new MapAdapter(counts - x, if (_totalSize >= 0) _totalSize - n else -1L)
			} else counts.getOrElse(x, 0) match {
				case n if n == count => this
				case n => new MapAdapter(counts.updated(x, count), if (_totalSize >= 0) _totalSize - n + count else -1L)
			}

		override def uniqueIterator :Iterator[X] = counts.keysIterator
		override def unique = counts.keySet
		override def toSet[B >: X] = counts match {
			case _ :HashMap[_, _] => counts.keySet.castParam[B]
			case _ => counts.keySet.toSet
		}
		override def equals(that :Any) :Boolean = that match {
			case other  :MapAdapter[x] => counts == other.counts
			case _ => super.equals(that)
		}
		override def toString = counts.iterator.map(xs => xs._1.toString + ": " + xs._2).mkString("MultiSet(", ", ", ")")
	}

}
