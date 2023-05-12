package net.noresttherein.sugar.collections



import scala.annotation.tailrec
import scala.collection.{AbstractSeq, AbstractSet, Factory, IterableFactory, IterableFactoryDefaults, IterableOps, LinearSeq, immutable, mutable}
import scala.collection.immutable.{ArraySeq, HashMap, HashSet, IndexedSeq, IndexedSeqDefaults, Set}
import scala.collection.mutable.{Builder, ReusableBuilder}
import scala.reflect.ClassTag

import net.noresttherein.sugar.collections.Ranking.{IndexedRanking, RankingSeqAdapter, RankingSetAdapter, SmallRanking}
import net.noresttherein.sugar.vars.{AbstractIdempotent, Opt}
import net.noresttherein.sugar.vars.Opt.{Got, Lack}

//implicits
import net.noresttherein.sugar.extensions.{castTypeParamMethods, IterableOnceExtension, castingMethods, SeqExtension}






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
  * @tparam T element type.
  */
trait Ranking[+T]
	extends immutable.Iterable[T] with IterableOps[T, Ranking, Ranking[T]] with IterableFactoryDefaults[T, Ranking]
	   with Serializable
{ ranking =>
	@inline final def length   :Int     = knownSize
	override def size          :Int     = knownSize
	final override def isEmpty :Boolean = knownSize == 0

	override def iterableFactory :IterableFactory[Ranking] = Ranking

	override def toIndexedSeq  :IndexedSeq[T] = new RankingSeqAdapter(this)
	override def toSeq         :Seq[T] = toIndexedSeq
	override def toSet[U >: T] :Set[U] = new RankingSetAdapter(this)

	override def to[C1](factory :Factory[T, C1]) :C1 = sourceCollectionFactory(factory) match {
		case Got(Seq) | Got(IndexedSeq) => toSeq.asInstanceOf[C1]
		case Got(Set) => toSet.asInstanceOf[C1]
		case Got(Ranking) => this.asInstanceOf[C1]
		case _ => super.to(factory)
	}


	/** Creates a `Ranking` with the same elements as this one, but in the reverse order.
	  * This will not copy large collections, but instead return an adapter.
	  * If you wish to force creation of a standard implementation use `reverseIterator to Ranking`.
	  */
	def reverse :Ranking[T] = reverseIterator to Ranking

	/** Iterates this collection in the reverse order. This will ''not'' require creation of any intermediate objects
	  * other than the iterator itself.
	  */
	def reverseIterator :Iterator[T]

	/** True if `that sameElements this.slice(offset, offset + that.size)`. */
	def startsWith[B >: T](that: IterableOnce[B], offset: Int = 0): Boolean = that match {
		case empty :Iterable[_] if empty.isEmpty =>
			offset <= size
		case _ =>
			val it = that.iterator
			var i = Math.max(offset, 0)
			val end = size
			var matches = true
			while (i < end && it.hasNext && { matches = apply(i) == it.next(); matches })
				i += 1
			!it.hasNext && matches
	}

	/** True if `that sameElements this.takeRight(that.size)`. */
	def endsWith[B >: T](that :Iterable[B]) :Boolean = startsWith(that, size - that.size)

	/** Computes the length of the longest segment that starts from the first element
	  * and whose elements all satisfy some predicate.
	  * @param   p     the predicate used to test elements.
	  * @param   from  the index where the search starts.
	  * @return the length of the longest segment of this $coll that starts from the first element
	  *         such that every element of the segment satisfies the predicate `p`.
	  */
	def segmentLength(p :T => Boolean, from :Int = 0) :Int = {
		var i = from; val end = size
		while (i < end && p(apply(i)))
			i += 1
		i - from
	}

	override def foreach[U](f :T => U) :Unit = size match {
		case n if n <= IndexedSeqDefaults.defaultApplyPreferredMaxLength =>
			var i = 0
			while (i < n) {
				f(apply(i)); i += 1
			}
		case _ => super.foreach(f)
	}

	/** The `n`-th element in this collection.
	  * @param n an index in the `[0..size-1]` range.
	  */
	def apply(n :Int) :T

	/** Creates a ranking with the same contents and order as this instance, but with `value` at index `n`.
	  * @param n an index in the `[0..size-1]` range.
	  * @param value the added element.
	  */
	@throws[IndexOutOfBoundsException]("if n < 0 || n >= size")
	def updated[U >: T](n :Int, value :U) :Ranking[U]


	/** The index of the given element in this $coll, or `-1` if it does not contain `elem`. */
	def indexOf[U >: T](elem :U) :Int

	/** Same as the standard [[net.noresttherein.sugar.collections.Ranking.indexOf indexOf]],
	  * but returns [[net.noresttherein.sugar.vars.Opt.Lack Lack]] instead of returning `-1` if no such element exists.
	  */
	def getIndexOf[U >: T](elem :U) :Opt[Int] = indexOf(elem) match {
		case -1 => Lack
		case n  => Got(n)
	}

	/** Same as the standard [[net.noresttherein.sugar.collections.Ranking.indexOf indexOf]],
	  * but throws a [[NoSuchElementException]] if the element is not found in this collection.
	  * The exception's message lists all the contents, so this method should not be used
	  * if this collection can be large or this might pose a security vulnerability, but the extra information
	  * is much more helpful than in an [[IndexOutOfBoundsException]] which most likely would be caused by returning
	  * `-1`.
	  */
	def sureIndexOf[U >: T](elem :U) :Int = indexOf(elem) match {
		case -1  => throw new NoSuchElementException("No " + elem + " in " + this + ".")
		case  n => n
	}

	/** Finds index of the first element satisfying some predicate after or at some start index.
	  * Unlike [[net.noresttherein.sugar.collections.Ranking.indexOf indexOf]], this method works in linear time.
	  * @param   p      the predicate used to test elements.
	  * @param   from   the start index
	  * @return the index `>= from` of the first element of this $coll that satisfies the predicate `p`,
	  *         or `-1`, if none exists.
	  */
	def indexWhere(p :T => Boolean, from :Int = 0) :Int

	/** Finds an element of this $coll which satisfies the predicate, returning its index as an option.
	  * Unlike [[net.noresttherein.sugar.collections.Ranking.getIndexOf getIndexOf]], this method works in linear time.
	  * @param p    a function applied consecutively to all elements with indices greater or equal `from`.
	  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
	  */
	def getIndexWhere(p :T => Boolean, from :Int = 0) :Opt[Int] = indexWhere(p, from) match {
		case -1 => Lack
		case  n => Got(n)
	}

	/** Finds an element of this $coll which satisfies the predicate, throwing a [[NoSuchElementException]]
	  * if it does not exist. Unlike [[net.noresttherein.sugar.collections.Ranking.sureIndexOf sureIndexOf]],
	  * this method works in linear time.
	  * @param p    a function applied consecutively to all elements with indices greater or equal `from`,
	  *             until satisfied.
	  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
	  */
	def sureIndexWhere(p :T => Boolean, from :Int = 0) :Int = indexWhere(p, from) match {
		case -1 => throw new NoSuchElementException("No element satisfying the predicate " + p + " in " + this + ".")
		case  n => n
	}

	/** Finds first index after or at a start index where this $coll contains a given sequence as a slice.
	  * Thanks to the uniqueness of elements in a $coll and fast
	  * [[net.noresttherein.sugar.collections.Ranking.indexOf indexOf]], this method runs in O(that.size).
	  * @param  that    the sequence to test
	  * @param  from    the start index
	  * @return the first index `>= from` such that the elements of this $coll starting at this index
	  *         match the elements of sequence `that`, or `-1` if no such subsequence exists.
	  */
    def indexOfSlice[U >: T](that :collection.Seq[U], from :Int = 0) :Int =
	    if (that.isEmpty)
		    if (from < 0) 0
	        else if (from > size) -1
		    else from
		else
		    indexOf(that.head) match {
			    case n if n >= from && startsWith(that, n) => n
			    case _ => -1
		    }

	/** Finds the location of the given subsequence in this $coll, returning its index as an `Opt`.
	  * Thanks to the uniqueness of elements in a $coll and fast
	  * [[net.noresttherein.sugar.collections.Ranking.indexOf indexOf]], this method runs in O(that.size).
	  * @param that a presumed consecutive subsequence of this sequence.
	  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
	  * @return `Opt(this.indexOfSlice(that, from)).filter(_ >= 0)`.
	  */
	def getIndexOfSlice[U >: T](that :collection.Seq[U], from :Int = 0) :Opt[Int] =
		indexOfSlice(that, from) match {
			case -1 => Lack
			case  n => Got(n)
		}

	/** Finds the location of the given subsequence in this sequence, throwing a [[NoSuchElementException]]
	  * if it does not exist. Thanks to the uniqueness of elements in a $coll and fast
	  * [[net.noresttherein.sugar.collections.Ranking.indexOf indexOf]], this method runs in O(that.size).
	  * @param that a presumed consecutive subsequence of this sequence.
	  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
	  * @return `this.indexOfSlice(that, from)`.
	  */
	def sureIndexOfSlice[U >: T](that :collection.Seq[U], from :Int = 0) :Int =
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
	final def containsSlice[U >: T](that :Seq[U]) :Boolean = indexOfSlice(that) != -1

	/** Checks if this collection contains the given element as defined by `equals`. */
	def contains[U >: T](elem :U) :Boolean = indexOf(elem) >= 0

	/** Checks if this collection contains all the given elements, in an unspecified order. */
	def containsAll[U >: T](elems :IterableOnce[U]) :Boolean = elems match {
		case items :Iterable[U] => items.isEmpty || items.forall(contains)
		case _ => elems.iterator.forall(contains)
	}


	/** Adds an element to the collection, if it is not already present.
	  * The difference from [[net.noresttherein.sugar.collections.Ranking.+: +:]] and
	  * [[net.noresttherein.sugar.collections.Ranking.:+ :+]] is that the element can be added at the front
	  * or at the back and does not change its position in the collection if it is already present.
	  * @return `this` ''iff'' it already contains `elem`, or a `Ranking` instance containing all elements of `this`,
	  *        followed by `elem` if it does not.
	  */
	@inline final def +[U >: T](elem :U) :Ranking[U] = this added elem
	def added[U >: T](elem :U) :Ranking[U]

	/** Prepends an element to the front of the collection. If the element is already present, it is moved to the front
	  * of the collection, with the preceding elements shift back one place.
	  * This method will be in general slower than [[net.noresttherein.sugar.collections.Ranking.+ +]]
	  * and [[net.noresttherein.sugar.collections.Ranking.:+ :+]].
	  */
	@inline final def +:[U >: T](elem :U) :Ranking[U] = this prepended elem
	def prepended[U >: T](elem :U) :Ranking[U]

	/** Appends an element at the end of the collection. If the element is already present, it is moved to the back
	  * of the collection, with the following elements shift forward one place.
	  * This method will be slower than [[net.noresttherein.sugar.collections.Ranking.+ +]] if this ranking already
	  * contains the element.
	  */
	@inline final def :+[U >: T](elem :U) :Ranking[U] = this appended elem
	def appended[U >: T](elem :U) :Ranking[U]

	/** Appends the given elements to this collection. The effect is the same as calling
	  * [[net.noresttherein.sugar.collections.Ranking.:+ :+]] for every element in `elems` in its iteration order.
	  */
	@inline final def :++[U >: T](elems :IterableOnce[U]) :Ranking[U] = this appendedAll elems
	def appendedAll[U >: T](elems :IterableOnce[U]) :Ranking[U]

	/** Prepends the given elements to this collection. The effect is the same as calling
	  * [[net.noresttherein.sugar.collections.Ranking.+: +:]] for every element in `elems`
	  * in its ''reverse'' iteration order. This method is generally slower
	  * than [[net.noresttherein.sugar.collections.Ranking.++ ++]]
	  * and [[net.noresttherein.sugar.collections.Ranking.:++ :++]].
	  */
	@inline final def ++:[U >: T](elems :Iterable[U]) :Ranking[U] = this prependedAll elems
	def prependedAll[U >: T](elems :Iterable[U]) :Ranking[U]

	/** Adds the given elements to this collection. The exact order of the elements in the returned collection
	  * is unspecified.
	  */
	def ++[U >: T](elems :IterableOnce[U]) :Ranking[U]

	/** Removes the given element from this collection, if present. */
	@inline final def -[U >: T](elem :U) :Ranking[T] = this removed elem
	def removed[U >: T](elem :U) :Ranking[T]

	/** Removes the given elements from this collection. */
	@inline final def --[U >: T](elems :IterableOnce[U]) :Ranking[T] = this removedAll elems
	def removedAll[U >: T](elems :IterableOnce[U]) :Ranking[T]

	//consider: Set methods

	/** Creates a `r :Ranking` such that `r(permutation(i)) == this(i)`. */
	def reorder(permutation :Permutation) :Ranking[T] =
		if (permutation.length != size)
			throw new IllegalArgumentException(
				"Cannot reorder " + this + " according to " + permutation +
					" because it is of a wrong size: expected " + size + ", but got " + permutation.length + "."

			)
		else if (permutation.isIdentity) //covers also empty and singleton rankings
			this
		else if (size <= IndexedSeqDefaults.defaultApplyPreferredMaxLength) {
			val size = this.size
			val seq = new Array[Any](size).castFrom[Array[Any], Array[T]]
			var i = size
			while (i > 0) {
				i -= 1
				seq(permutation(i)) = this(i)
			}
			if (size <= Ranking.smallRankingCap)
				new SmallRanking(seq)
			else
				new IndexedRanking(PassedArrayInternals.wrap(seq))
		} else {
			val size = this.size
			val seq = new Array[Any](size).castFrom[Array[Any], Array[T]]
			val from = iterator
			val to = permutation.iterator
			while (to.hasNext)
				seq(to.next()) = from.next()
			new IndexedRanking(PassedArrayInternals.wrap(seq))
		}


	/** Creates a new `Ranking` with the same elements as this one, sorted with implicit `Ordering` for the elements. */
	def sorted[U >: T :Ordering] :Ranking[T] = toSeq.sorted[U] to Ranking

	/** Creates a new `Ranking` with the same elements as this one, sorted by a property of elements
	  * with an implicit `Ordering`.
	  */
	def sortBy[A :Ordering](f :T => A) :Ranking[T] = toSeq.sortBy(f) to Ranking

	/** Creates a new `Ranking` with the same elements, sorted using the passed function. */
	def sortWith(f :(T, T) => Boolean) :Ranking[T] = toSeq.sortWith(f) to Ranking

	/** Checks if the elements in this sequence follow the implicit ordering.
	  * @return [[net.noresttherein.sugar.collections.Ranking.isSortedWith isSortedWith]]`(implicitly[Ordering[U]].compare(_, _) <= 0)`
	  */
	def isSorted[U >: T :Ordering] :Boolean = {
		val order = implicitly[Ordering[U]]
		isSortedWith(order.compare(_, _) <= 0)
	}

	/** Checks if the elements in this sequence are sorted by the given ordered property.
	  * @return [[net.noresttherein.sugar.collections.Ranking.isSortedWith isSortedWith]]`(implicitly[Ordering[U]].compare(by(_), by(_)) <= 0)`
	  */
	def isSortedBy[U :Ordering](by :T => U) :Boolean = {
		val order = implicitly[Ordering[U]]
		isSortedWith((x, y) => order.compare(by(x), by(y)) <= 0)
	}

	/** Checks if the given predicate holds for all consecutive element pairs in this sequence. */
	def isSortedWith[U >: T](lte :(U, U) => Boolean) :Boolean =
		length <= 1 || {
			toSeq match {
				case seq :LinearSeq[T] =>
					lte(seq.head, seq.tail.head) &&
						seq.view.zip(seq.tail).forall { case (left, right) => lte(left, right) }
				case seq :collection.IndexedSeq[T] =>
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


	/** Verifies if the element sets of the two collections are equal.
	  * @return value equal to `this.toSet == other.toSet`.
	  */
	def isPermutationOf[U](other :Ranking[U]) :Boolean = size == other.size && other.forall(contains)

	/** The same as `toSeq.`[[scala.collection.SeqOps.sameElements sameElements]]. */
	def sameElements[U >: T](that :IterableOnce[U]) :Boolean = corresponds(that)((my, their) => my == their)

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :Ranking[_] => sameElements(other)
		case _ => false
	}

	protected override def className = "Ranking"
}




/**
  * $factoryInfo
  * @define Coll `Ranking`
  * @define coll ranking
  */
@SerialVersionUID(Ver)
case object Ranking extends IterableFactory[Ranking] {

	override def from[T](elems :IterableOnce[T]) :Ranking[T] = elems match {
		case ranking :Ranking[T] => ranking
		case seq :RankingSeqAdapter[T] => seq.toRanking
		case set :RankingSetAdapter[T] => set.toRanking
		case iter :Iterable[_] if iter.isEmpty => empty[T]
		case iter :Iterator[_] if iter.isEmpty => empty[T]
		case iter :Iterable[T] if iter.sizeIs == 1 =>
			new SingletonRanking[T](iter.head)
		case iter :Iterable[T] if iter.sizeIs <= SmallRankingCap =>
			deduplicateSmall(iter)
		case iter :Iterable[T] =>
			deduplicateLarge(iter)
		case _ => (newBuilder[T] ++= elems).result()
	}

	/** A new builder adding elements with the semantics of [[net.noresttherein.sugar.collections.Ranking.+ +]],
	  * with the addition that elements are always appended if absent.
	  */
	override def newBuilder[T] :Builder[T, Ranking[T]] = new RankingBuilder[T]

	/** A builder adding the elements with the semantics of [[net.noresttherein.sugar.collections.Ranking.+ +]].
	  * It only guarantees that the order of the elements in the built `Ranking` corresponds to a (non consecutive)
	  * subsequence of the input, without specifying which of the occurrences of equal elements is kept.
	  */
	def unorderedBuilder[T] :Builder[T, Ranking[T]] = new UnorderedBuilder[T]

	/** A `Ranking` builder adding elements with the semantics of [[net.noresttherein.sugar.collections.Ranking.:+ :+]]
	  * - in case of duplicates, the most recently added item replaces the previous instance in the sequence.
	  * This builder is less efficient than [[net.noresttherein.sugar.collections.Ranking.newBuilder newBuilder]] and
	  * [[net.noresttherein.sugar.collections.Ranking.unorderedBuilder unorderedBuilder]].
	  */
	def appendingBuilder[T] :Builder[T, Ranking[T]] = new AppendingBuilder[T]


	override def empty[E] :Ranking[E] = EmptyRanking //reusableEmpty

	/** A specialized light `Ranking` implementation for collections containing only one element. */
	def one[T](singleton :T) :Ranking[T] = new SingletonRanking(singleton)

	/** A specialized light `Ranking` implementation for collections containing only one element. */
	def single[T](singleton :T) :Ranking[T] = new SingletonRanking(singleton)

	/** A `Ranking[T]` with lazily evaluated contents. The initializer will be called only when any of the methods
	  * on the proxy is called. It will be executed at most once, withing a `synchronized` block for the proxy.
	  * Once computed, it remains thread safe but will incur no additional synchronization penalty.
	  */
	@inline def Lazy[T](init: => IterableOnce[T]) :Ranking[T] = delayed(from(init))

	/** A proxy to a lazily computed `Ranking[T]`. The initializer will be called when any of the methods on the proxy
	  * is called. It will be executed at most once, withing a `synchronized` block for the proxy.
	  * Once computed, it remains thread safe but will incur no additional synchronization penalty.
	  */
	def delayed[T](init: => Ranking[T]) :Ranking[T] = new LazyRanking[T](() => init)


	def unapplySeq[T](elems :Ranking[T]) :Got[Seq[T]] = Got(elems.toSeq)


	/** An implicit extension of a ''by-name'' expression evaluating to a `Ranking[T]` instance, adding a `delayed`
	  * method which creates a proxy using it to initialize its target. */
	implicit class DelayedRanking[T](initializer: => Ranking[T]) {
		/** Treats the `this` argument as a ''by-name'' expression to be evaluated only when the created `Ranking`
		  * proxy's contents are accessed.
		  * @return `Ranking.delayed(initializer)`.
		  * @see [[net.noresttherein.sugar.collections.Ranking.delayed delayed]]
		  */
		@inline def delayed :Ranking[T] = Ranking.delayed(initializer)
	}

	@SerialVersionUID(Ver)
	object implicits {
		implicit def rankingToSeq[T](ranking :Ranking[T]) :Seq[T] = ranking.toSeq
		implicit def rankingToSet[T](ranking :Ranking[T]) :Set[T] = ranking.toSet
	}

	private val EmptyRanking = new EmptyRanking


	private[this] val Duplicate = new AnyRef
	private def deduplicateSmall[T](items :Iterable[T], keepFirst :Boolean = true) :Ranking[T] = {
		 /* Counts the total number of duplicates in the array (i.e, `array.length - array.toSet.size`),
		  * setting all bust the last copy of each equal item pair to `Duplicate`. */
		def erasePrecedingDuplicates(array :Array[T]) :Int = {
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
		def eraseFollowingDuplicates(array :Array[T]) :Int = {
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
		val array = items.toArray(ClassTag[T](classOf[AnyRef]))
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
			val deduped = new Array[AnyRef](array.length - duplicates).asInstanceOf[Array[T]]
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

	private def deduplicateLarge[T](items :Iterable[T], keepFirst :Boolean = true) :Ranking[T] = {
		val seq = items match {
			case seq :IndexedSeq[T] => seq
			case _ => items to PassedArray
		}
		val indexed = seq.view.zipWithIndex to ArraySeq
		val map = if (keepFirst) indexed.reverseIterator.toMap else indexed.toMap
		if (map.size == seq.length)
			new IndexedRanking(seq, map)
		else {
			//remove duplicates, leave only those lucky to be in map
			val ranking = indexed.collect { case (item, i) if map(item) == i => item }
			new IndexedRanking(ranking)
		}
	}

	@inline private def hashCodeOf(x :Any) :Int = if (x == null) 0 else x.hashCode



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
	private class RankingBuilder[T] protected (
                  private[this] var large     :Builder[T, IndexedSeq[T]],
                  private[this] var index     :Map[T, Int], //<-^builds IndexedRanking
                  private[this] var small     :Array[T], //<- builds SmallRanking
                  private[this] var hashes    :Array[Int], //hash codes of elements in small
                  private[this] var smallSize :Int) //the number of elements in small or -1 if large is used instead
		extends ReusableBuilder[T, Ranking[T]]
	{
		def this(items :Builder[T, IndexedSeq[T]], index :Map[T, Int]) =
			this(items, index, null, null, -1)

		def this(ranking :SmallRanking[T]) =
			this(null, null,
				Array.copyOf(ranking.contents, SmallRankingCap),
				Array.copyOf(ranking.hashCodes, SmallRankingCap),
				ranking.size)

		def this() =
			this(null, null, null, null, 0)

		final protected def seqBuilder = {
			if (large == null) {
				index = Map.empty
				large = ArraySeq.untagged.newBuilder[T]
			}
			large
		}
		final protected def map = {
			if (index == null) {
				index = Map.empty
				large = ArraySeq.untagged.newBuilder
			}
			index
		}
		final protected def map_=(map :Map[T, Int]) :Unit = index = map
		final protected def array      = small
		final protected def array_=(array :Array[T]) :Unit = small = array
		final protected def hashCodes  = hashes
		final protected def hashCodes_=(array :Array[Int]) :Unit = hashes = array
		final protected def size       = smallSize
		final protected def size_=(size :Int) :Unit = smallSize = size

		override def sizeHint(size :Int) :Unit = //todo: support small.length < SmallRankingCap
			if (size >= SmallRankingCap && smallSize >= 0)
				switchToLarge(size)

		final protected def switchToLarge(extraSpace :Int) :Unit =
			if (smallSize > 0) {
				index = Map.empty
				large = ArraySeq.untagged.newBuilder[T]
				large sizeHint Math.min(extraSpace, Int.MaxValue - smallSize) + smallSize
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
						large = ArraySeq.untagged.newBuilder
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
				small  = new Array[Any](SmallRankingCap).asInstanceOf[Array[T]]
				hashes = new Array[Int](SmallRankingCap)
				small(0)  = elem
				hashes(0) = hash
				smallSize = 1
			} else {
				var i = 0
				while ({ i = hashes.indexOf(hash, i); i >= 0 && small(i) != elem })
					i += 1
				if (i < 0)
					if (smallSize < SmallRankingCap) {
						small(smallSize)  = elem
						hashes(smallSize) = hash
						smallSize += 1
					} else {
						switchToLarge(SmallRankingCap)
						large += elem
						index = index.updated(elem, SmallRankingCap)
					}
			}
			this
		}

		private def largeAddOne(elem :T) :this.type = {
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
				if (small == null) {
					small = new Array[Any](SmallRankingCap).asInstanceOf[Array[T]]
					hashes = new Array[Int](SmallRankingCap)
				}
				smallSize = ranking.size
				System.arraycopy(ranking.contents, 0, small, 0, smallSize)
				System.arraycopy(ranking.hashCodes, 0, hashes, 0, smallSize)
				this
			case ranking :IndexedRanking[T] =>
				large = IndexedSeq.newBuilder; large sizeHint ranking.size
				large ++= ranking.toIndexedSeq
				index = ranking.indices.asInstanceOf[Map[T, Int]]
				smallSize = -1
				this
			case ranking :SingletonRanking[T] =>
				smallAddOne(ranking.head, hashCodeOf(ranking.head))
			case _ =>
				super.addAll(xs)
		}

		//assumes smallSize < 0 and that large and index are initialized
		protected def largeAddAll(xs :IterableOnce[T]) :this.type = {
			xs.iterator.filter(!index.contains(_)).zipForeach(Iterator.iterate(index.size)(_ + 1)) {
				(elem, idx) => large += elem; index = index.updated(elem, idx)
			}
			this
		}

		//assumes smallSize < 0 and non-null index and large
		private def largeAddAll(xs :SmallRanking[T]) :this.type = {
			var xsIdx = 0; val  xsSize = xs.size
			val xsElems = xs.contents
			while (xsIdx < xsSize) {
				val item = xsElems(xsIdx)
				large += item
				index = index.updated(item, index.size)
				xsIdx += 1
			}
			this
		}

		protected def addAll(xs :SmallRanking[T]) :this.type = {
			val xsSize = xs.size
			if (smallSize < 0)
				super.addAll(xs)
			else if (smallSize == 0) {
				if (small == null) {
					small = Array.copyOf(xs.contents, SmallRankingCap)
					hashes = Array.copyOf(xs.hashCodes, SmallRankingCap)
				} else {
					smallSize = xsSize
					Array.copy(xs.contents, 0, small, 0, smallSize)
					Array.copy(xs.hashCodes, 0, small, 0, smallSize)
				}
				this
			} else if (xsSize > SmallRankingOptimismCap) {
				switchToLarge(xsSize)
				largeAddAll(xs)
			} else if (xs.size + smallSize <= SmallRankingOptimismCap) {
				val xsElems  = xs.contents
				val xsHashes = xs.hashCodes
				var xsIdx = 0
				var smallIdx = smallSize
				while (xsIdx < xsSize && smallIdx >= 0) {
					val item = xsElems(xsIdx)
					val hash = xsHashes(xsIdx)
					val dupIdx = hashes.lastIndexOf(hash, smallIdx - 1)
					if (dupIdx < 0 || this.small(dupIdx) != item)
						if (smallIdx == SmallRankingCap) {
							smallIdx = -1
						} else {
							this.small(smallIdx) = item
							this.hashes(smallIdx) = hash
							smallIdx += 1
						}
					xsIdx += 1
				}
				if (smallIdx >= 0) {
					smallSize = smallIdx
					this
				} else { //no luck
					switchToLarge(xsSize - xsIdx)
					largeAddAll(xs)
				}
			} else {
				switchToLarge(xsSize)
				largeAddAll(xs)
			}
		}

		protected def addAll(xs :Ranking[T]) :this.type = {
			val xsSize = xs.size
			xs match {
				case _ if smallSize < 0 =>
					largeAddAll(xs)
				case _ if smallSize == 0 =>
					become(xs)
				case _ if xsSize > SmallRankingCap =>
					switchToLarge(xsSize)
					largeAddAll(xs)
				case single  :SingletonRanking[T] =>
					addOne(single.head)
				case small   :SmallRanking[T]     =>
					addAll(small)
				case _ if smallSize + xsSize <= SmallRankingOptimismCap =>
					var i = 0
					while (i < xsSize && smallSize >= 0) {
						val elem = xs(i)
						val hash = hashCodeOf(elem)
						smallAddOne(elem, hash)
						i += 1
					}
					while (i < xsSize) {
						largeAddOne(xs(i))
						i += 1
					}
					this
				case _ if xsSize <= IndexedSeqDefaults.defaultApplyPreferredMaxLength =>
					var i = 0
					while (i < xsSize) {
						largeAddOne(xs(i))
						i += 1
					}
					this
				case _ =>
					largeAddAll(xs)
			}
		}

		override def addAll(xs :IterableOnce[T]) :this.type = xs match {
			case empty   :Iterable[T] if empty.isEmpty =>
				this
			case ranking :Ranking[T] =>
				addAll(ranking)
			case _ if smallSize < 0 =>
				largeAddAll(xs)
			case _ =>
				val knownSize = xs.knownSize
				if (knownSize > (SmallRankingCap - smallSize) * SmallRankingPessimismFactor) {
					switchToLarge(knownSize)
					largeAddAll(xs)
				} else {
					val i = xs.iterator
					while (smallSize > 0 && i.hasNext) {
						val elem = i.next()
						val hash = hashCodeOf(elem)
						smallAddOne(elem, hash)
					}
					if (i.hasNext) {
						if (smallSize == 0) {
							index = Map.empty
							large = ArraySeq.untagged.newBuilder
							smallSize = -1
						}
						largeAddAll(i)
					}
					this
				}
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
			case 0  => clear(); empty[T]
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
	private final class UnorderedBuilder[T] private(
                        private[this] var _large     :Builder[T, IndexedSeq[T]],
                        private[this] var _index     :Map[T, Int], //<-^builds IndexedRanking
                        private[this] var _small     :Array[T], //<- builds SmallRanking
                        private[this] var _hashes    :Array[Int], //hash codes of elements in small
                        private[this] var _smallSize :Int) //the number of elements in small or -1 if large is used instead
		extends RankingBuilder[T](_large, _index, _small, _hashes, _smallSize)
	{
		def this(items :Builder[T, IndexedSeq[T]], index :Map[T, Int]) =
			this(items, index, null, null, -1)

		def this(ranking :SmallRanking[T]) =
			this(null, null,
				Array.copyOf(ranking.contents, SmallRankingCap),
				Array.copyOf(ranking.hashCodes, SmallRankingCap),
				ranking.size)

		def this() =
			this(null, null, null, null, 0)

		/** Called when `smallSize >= 0` and `small.toSet intersect xs.toSet == Set()`. */
/*
		private def addUnique(xs :Ranking[T]) :this.type = {
			val otherSize = xs.size
			if (index == null) {
				index = Map.empty
				large = IndexedSeq.newBuilder; large sizeHint smallSize + otherSize
			}
			var i = 0; var j = smallSize
			while (i < j) {     //copy small (current elements not present in xs) to large
				val item = small(i)
				large += item
				index = index.updated(item, i)
				i += 1
			}
			i = 0
			while (i < otherSize) {     //now, copy xs into large
				val item = xs(i)
				large += item
				index = index.updated(item, j)
				i += 1; j += 1
			}
			small = null
			hashes = null
			smallSize = -1
			this
		}
*/

		/** Tries to fit `xs` into `small` buffer. Assumes `smallSize > 0 && xs.size > smallSize`,
		  * so it's faster to filter `small` not contained in `xs` and then add `xs` knowing the elements are unique.
		  */
		private def filterSmallAddAll(xs :Ranking[T]) :this.type = xs match {
			case ranking :SmallRanking[T] =>
				val oldElems  = array
				val oldHashes = hashCodes
				val oldSize   = this.size
				val newElems  = new Array[Any](SmallRankingCap).asInstanceOf[Array[T]]
				val newHashes = new Array[Int](SmallRankingCap)
				var i = 0; var j = 0
				while (i < oldSize) {                 //let newElems = small.filterNot(xs.contains)
					val item = oldElems(i); val hash = oldHashes(i)
					if (ranking.indexOf(item, hash) < 0) {
						newElems(j)  = item
						newHashes(j) = hash
						j += 1
					}
					i += 1
				}
				array       = newElems
				hashCodes   = newHashes
				val xsSize  = xs.size
				val newSize = j + xsSize
				if (newSize <= SmallRankingCap) { //current elements and xs together fit in a SmallRanking
					System.arraycopy(ranking.contents, 0, newElems, j, xsSize)
					System.arraycopy(ranking.hashCodes, 0, newHashes, j, xsSize)
					size = newSize
				} else {
					size = j
					switchToLarge(xsSize)
					largeAddAll(xs)
				}
				this
			case _ =>
				val newElems  = new Array[Any](SmallRankingCap).asInstanceOf[Array[T]]
				val newHashes = new Array[Int](SmallRankingCap)
				val oldElems  = array
				val oldHashes = hashCodes
				val oldSize   = this.size
				var i = 0; var j = 0
				while (i < oldSize) {                 //let newElems = small.filterNot(xs.contains)
					val item = oldElems(i)
					if (xs.indexOf(item) < 0) {
						newElems(j)  = item
						newHashes(j) = oldHashes(i)
						j += 1
					}
					i += 1
				}
				array       = newElems
				hashCodes   = newHashes
				val xsSize  = xs.size
				val newSize = oldSize + xs.size
				if (newSize <= SmallRankingCap) { //current elements and xs together fit in a SmallRanking
					i = 0
					while (i < xsSize) {
						val item = xs(i)
						newElems(j)  = item
						newHashes(j) = hashCodeOf(item)
						i += 1; j += 1
					}
					size = j
				} else {
					size = j
					switchToLarge(xsSize)
					val builder = seqBuilder
					var index = map
					xs.iterator.zipForeach(Iterator.iterate(j)(_ + 1)) {
						(elem, idx) => builder += elem; index = index.updated(elem, idx)
					}
					map = index
				}
				this
		}

		override def addAll(xs :Ranking[T]) :this.type = {
			val oldSize = size
			val xsSize  = xs.size
			if (oldSize == 0)
				become(xs)
			else if (oldSize > 0)
				if (xsSize < SmallRankingCap & xsSize > (oldSize << 1) && oldSize + xsSize <= SmallRankingOptimismCap)
					filterSmallAddAll(xs)
				else
					super.addAll(xs)
			else {
				var index = map
				val size  = index.size
				if (xsSize > (size << 2)) { //faster to carry over our contents to it than vice versa
					val builder = seqBuilder
					val retained = seqBuilder.result().filterNot(xs.contains)
					if (retained.isEmpty) {
						builder.clear()
						index = Map.empty
						become(xs)
					} else {
//						val newSize = elems.length + xsSize
						val newElems = xs match {
							case ranking :IndexedRanking[T] => ranking.toSeq
							case _ => xs
						}
						builder ++= retained ++= newElems
						index = index ++ newElems.foldLeftWithIndex(index) {
							(acc, elem, i) => acc.updated(elem, i + oldSize)
						}
					}
				}
				this
			}
		}
	}




	private class AppendingBuilder[T] extends ReusableBuilder[T, Ranking[T]] {
		private[this] var hint = -1
		private[this] var buffer = List.empty[IterableOnce[T]]

		override def sizeHint(size :Int) :Unit = hint = size

		override def addOne(elem :T) = { buffer = Ranking.one(elem)::buffer; this }

		override def addAll(xs :IterableOnce[T]) :this.type = xs match {
			case empty   :Iterable[_] if empty.isEmpty =>
				this
			case empty   :Iterator[_] if !empty.hasNext =>
				this
			case single  :Iterable[T] if single.sizeIs == 1 =>
				buffer = single::buffer; this
			case seq     :IndexedSeq[T] =>
				buffer = ReversedSeq(seq)::buffer; this
			case seq     :collection.Seq[T] =>
				buffer = seq.reverseIterator.toIndexedSeq::buffer; this
			case ranking :Ranking[T] =>
				buffer = ranking.reverse::buffer; this
			case _ =>
				buffer = ReversedSeq(IndexedSeq.from(xs))::buffer; this
		}

		override def clear() :Unit = buffer = Nil

		override def result() :Ranking[T] =
			if (buffer eq Nil)
				Ranking.empty
			else {
				val res = new ReverseBuilder[T]
				if (hint < 0) {
					@tailrec def count(list :List[IterableOnce[T]] = buffer, acc :Int = 0) :Int =
						if (list eq Nil)
							acc
						else list.head.knownSize match {
							case -1 => -1
							case  n => count(list.tail, acc + n)
						}
					hint = count()
				}
				if (hint >= 0)
					res sizeHint hint
				while (buffer ne Nil) {
					res ++= buffer.head
					buffer = buffer.tail
				}
				res.result()
			}
	}




	@inline private def reverse[T](elems :Array[T]) :Array[T] = reverse(elems, elems.length, SmallRankingCap)

	private def reverse[T](elems :Array[T], count :Int, newSize :Int) :Array[T] = {
		val res = java.lang.reflect.Array.newInstance(
			elems.getClass.getComponentType, newSize
		).asInstanceOf[Array[T]]
		var i = count; var j = 0
		while (i > 0) {
			i -= 1
			res(j) = elems(i)
			j += 1
		}
		res
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
	private final class ReverseBuilder[T] private (
                        private[this] var large     :Builder[T, IndexedSeq[T]],
                        private[this] var set       :mutable.Set[T], //<-^builds IndexedRanking
                        private[this] var small     :Array[T], //<- builds SmallRanking
                        private[this] var hashes    :Array[Int], //hash codes of elements in small
                        private[this] var smallSize :Int) //the number of elements in small or -1 if large is used instead
		extends ReusableBuilder[T, Ranking[T]]
	{
		def this(ranking :IndexedRanking[T]) = this(
			{ val large = IndexedSeq.newBuilder[T]; large sizeHint ranking.size; large ++= ranking.reverseIterator },
			{ val set = mutable.Set.empty[T]; set sizeHint ranking.size; set ++= ranking },
			null, null, -1
		)

		def this(ranking :SmallRanking[T]) =
			this(null, null, reverse(ranking.contents), reverse(ranking.hashCodes), ranking.size)

		def this() =
			this(null, null, null, null, 0)

		override def sizeHint(size :Int) :Unit =
			if (size >= SmallRankingCap && smallSize >= 0) {
				if (large == null) {
					large = IndexedSeq.newBuilder[T]
					set = mutable.Set.empty
				}
				large sizeHint size
				set sizeHint size
				var i = 0
				while (i < smallSize) {
					val elem = small(i)
					large += elem
					set   += elem
					i += 1
				}
				smallSize = -1 //add directly to the builder, skip the array
			}

		private def switchToLarge() :Unit = {
			if (large == null) {
				large = IndexedSeq.newBuilder
				set = mutable.Set.empty
				large sizeHint SmallRankingCap + 1
				set sizeHint SmallRankingCap + 1
			}
			var i = 0
			while (i < SmallRankingCap) {
				val elem = small(i)
				large += elem
				set   += elem
				i += 1
			}
			smallSize = -1
		}


		//todo:
//		override def addAll(xs :IterableOnce[T]) :this.type = super.addAll(xs)

		private def addFirst(elem :T) :this.type = {
			small = new Array[Any](SmallRankingCap).asInstanceOf[Array[T]]
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
				val i = hashes.indexOf(hash)
				if (i < 0 || small(i) != elem) {
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

		override def clear() :Unit = {
			smallSize = 0
			if (set != null)
				set.clear()
			if (large != null)
				large.clear()
			if (small != null)
				java.util.Arrays.fill(small.asInstanceOf[Array[AnyRef]], null)
		}
		@inline private def clear(result :Ranking[T]) :Ranking[T] = {
			clear(); result
		}

		override def result() :Ranking[T] = smallSize match {
			case 0  => clear(); empty[T]
			case 1  => clear(new SingletonRanking(small(0)))
			case n if n > 0 => clear(new SmallRanking[T](reverse(small, n, n), reverse(hashes, n, n)))
			case _ => clear(new IndexedRanking(large.result().reverse))
		}
	}



	private def smallRankingCap = SmallRankingCap
	private[this] final val SmallRankingCap = 16
	private[this] final val SmallRankingOptimismCap = SmallRankingCap + SmallRankingCap >> 1
	private[this] final val SmallRankingPessimismCap = SmallRankingCap << 2
	private[this] final val SmallRankingPessimismFactor = 4



	@SerialVersionUID(Ver)
	private final class SmallRanking[+T] private[Ranking](elements :Array[T], hashes :Array[Int]) extends Ranking[T] {
		def this(elements :Array[T]) = this(elements, elements.map(hashCodeOf))

		private[Ranking] def contents[U >: T]  :Array[U]   = elements.asInstanceOf[Array[U]]
		private[Ranking] def hashCodes :Array[Int] = hashes

		@inline override def knownSize = elements.length
		override def size = elements.length
		override def last = elements(elements.length - 1)
		override def head = elements(0)
		override def tail = new SmallRanking(elements.tail)
		override def init = new SmallRanking(elements.init)

		override def iterator = elements.iterator
		override def reverse = new SmallRanking(elements.reverse, hashes.reverse)
		override def reverseIterator = toIndexedSeq.reverseIterator
		override def toIndexedSeq = ArraySeq.unsafeWrapArray(elements)
		override def toSeq = ArraySeq.unsafeWrapArray(elements)

		override def foreach[U](f :T => U) :Unit = elements.foreach(f)
		override def map[B](f :T => B) :Ranking[B] = new SmallRanking(elements.map(f)(ClassTag(classOf[Any])))
		override def filterNot(pred :T => Boolean) :Ranking[T] = filterImpl(pred, false)
		override def filter(pred :T => Boolean) :Ranking[T] = filterImpl(pred, true)

		private def filterImpl(pred :T => Boolean, truth :Boolean) :Ranking[T] = {
			val filtered = Array.copyOf(elements, elements.length)
			val filteredHashes = new Array[Int](elements.length)
			var i = 0; var j = 0; val len = elements.length
			while (i < len) {
				if (pred(filtered(i)) == truth) {
					filtered(j) = filtered(i)
					filteredHashes(j) = hashes(i)
					j += 1
				}
				i += 1
			}
			if (j == 0) Ranking.empty[T]
			else if (j == 1) Ranking.single(filtered(0))
			else new SmallRanking[T](Array.copyOf(filtered, j), Array.copyOf(filteredHashes, j))
		}

		override def apply(n :Int) = elements(n)
		override def updated[U >: T](n :Int, value :U) = {
			val hash = hashCodeOf(value)
			if (hashes(n) == hash && elements(n) == value)
				this
			else {
				val len = elements.length
				val copy = new Array[Any](len).asInstanceOf[Array[U]]
				System.arraycopy(elements, 0, copy, 0, len)
				copy(n) = value
				val newHashes = new Array[Int](len)
				System.arraycopy(hashes, 0, newHashes, 0, len)
				newHashes(n) = hash
				new SmallRanking(copy, newHashes)
			}
		}

		override def indexOf[U >: T](elem :U) :Int = indexOf(elem, hashCodeOf(elem))

		def indexOf[U >: T](elem :U, hash :Int) :Int = {
			var i = 0
			while ({ i = hashes.indexOf(hash, i); i >= 0 && elements(i) != elem })
				i += 1
			i
		}

		override def indexWhere(p :T => Boolean, from :Int) = {
			var i = if (from <= 0) 0 else from
			val end = elements.length
			while (i < end && !p(elements(i)))
				i += 1
			if (i >= end) -1 else i
		}

		override def removed[U >: T](elem :U) = indexOf(elem) match {
			case n if n < 0 => this
			case n =>
				val len = elements.length
				val res = Array.ofDim[T](len - 1)(ClassTag(elements.getClass.getComponentType))
				val hs  = new Array[Int](len - 1)
				var i = 0; var j = 0
				while (i < len) {
					if (i != n) {
						res(j) = elements(i)
						hs(j) = hashes(i)
						j += 1
					}
					i += 1
				}
				new SmallRanking(res)
		}

		override def removedAll[U >: T](elems :IterableOnce[U]) = elems match {
			case empty :Iterable[_] if empty.isEmpty =>
				this
			case _ =>
				val remove = new Array[Boolean](elements.length)
				var size = elements.length
				val it = elems.iterator
				while (size > 0 && it.hasNext)
					indexOf(it.next()) match {
						case n if n >= 0 && !remove(n) =>
							remove(n) = true
							size -= 1
						case _ =>
					}
				size match {
					case 0 => Ranking.empty
					case 1 =>
						var i = elements.length - 1
						while (remove(i))
							i -= 1
						new SingletonRanking(elements(i))
					case _ =>
						val res = Array.ofDim[T](size)(ClassTag(elements.getClass.getComponentType))
						val hs = new Array[Int](size)
						var i = 0; var j = 0
						while (j < size) {
							if (!remove(i)) {
								res(j) = elements(i)
								hs(j) = hashes(i)
								j += 1
							}
							i += 1
						}
						new SmallRanking(res, hs)
				}
		}

		override def prepended[U >: T](elem :U) = {
			val hash = hashCodeOf(elem)
			indexOf(elem, hash) match {
				case -1 => prepend(elem, hash)
				case  0 => this
				case  n =>
					val res = Array.copyOf(elements.asInstanceOf[Array[U]], size)
					val hs = Array.copyOf(hashes, size)
					var i = n
					while (i > 0) {
						res(i) = res(i - 1)
						hs(i) = hs(i - 1)
						i -= 1
					}
					res(0) = elem
					hs(0) = hash
					new SmallRanking(res, hs)
			}
		}

		private def prepend[U >: T](elem :U, hash :Int) :Ranking[U] =
			if (elements.length >= SmallRankingCap)
				new IndexedRanking(elem +: PassedArrayInternals.wrap(elements))
			else {
				val res = Array.ofDim[U](elements.length + 1)(ClassTag(elements.getClass.getComponentType))
				val hs = new Array[Int](elements.length + 1)
				res(0) = elem
				hs(0) = hash
				var i = 0
				while (i < elements.length) {
					res(i + 1) = elements(i)
					hs(i + 1) = hashes(i)
					i += 1
				}
				new SmallRanking(res, hs)
			}


		override def appended[U >: T](elem :U) = {
			val hash = hashCodeOf(elem)
			indexOf(elem, hash) match {
				case n if n == elements.length => this
				case n if n >= 0 =>
					val res = Array.copyOf(elements.asInstanceOf[Array[U]], size)
					val hs = Array.copyOf(hashes, size)
					var i = n
					while (i < elements.length - 1) {
						res(i) = res(i + 1)
						hs(i) = hs(i + 1)
						i += 1
					}
					res(i) = elem
					hs(i) = hash
					new SmallRanking(res, hs)
				case _ => append(elem, hash)
			}
		}
		override def added[U >: T](elem :U) = {
			val hash = hashCodeOf(elem)
			indexOf(elem, hash) match {
				case n if n >= 0 && elements(n) == elem => this
				case _ => append(elem, hash)
			}
		}
		private def append[U >: T](elem :U, hash :Int) :Ranking[U] =
			if (elements.length >= SmallRankingCap)
				new IndexedRanking(elements.view.appended(elem).toIndexedSeq)
			else {
				val size = elements.length
				val res = Array.copyOf(elements.asInstanceOf[Array[U]], size + 1)
				val hs = Array.copyOf(hashes, size + 1)
				res(size) = elem
				hs(size) = hash
				new SmallRanking(res, hs)
			}


		override def prependedAll[U >: T](elems :Iterable[U]) = elems match {
			case _ if elems.isEmpty =>
				this
			case _ if elems.sizeIs == 1 =>
				elems.head +: this
			case ranking :Ranking[U] =>
				prependedAll(ranking.toSet)
			case set     :collection.Set[U] =>
				prependedAll(set)
			case _ => (newBuilder[U] ++= elems ++= this).result()
		}
		//filter all our elements present in set and combine into a single sequence.
		private def prependedAll[U >: T](set :collection.Set[U]) :Ranking[U] = {
			val prefixSize = set.knownSize
			filterNot(set) match {
				case empty if empty.isEmpty => Ranking.from(set)
				case suffix :SmallRanking[U] if prefixSize >= 0 && suffix.length + prefixSize <= SmallRankingCap =>
					val suffixLength = suffix.length
					val newArray = new Array[Any](prefixSize + suffixLength).asInstanceOf[Array[U]]
					val newHashes = new Array[Int](prefixSize + suffixLength)
					set.copyToArray(newArray, 0, prefixSize)
					var i = prefixSize
					while (i > 0) {
						i -= 1
						newHashes(i) = hashCodeOf(newArray(i))
					}
					Array.copy(suffix.contents, 0, newArray, prefixSize, suffixLength)
					Array.copy(suffix.hashCodes, 0, newHashes, prefixSize, suffixLength)
					new SmallRanking[U](newArray, newHashes)
				case suffix =>
					new IndexedRanking(set.toIndexedSeq :++ suffix)
			}
		}


		override def appendedAll[U >: T](elems :IterableOnce[U]) :Ranking[U] =
			elems match {
				case empty  :Iterable[_] if empty.isEmpty => this
				case empty  :Iterator[_] if !empty.hasNext => this
				case single :Iterable[U] if single.sizeIs == 1 =>
					appended(single.head)
				case ranking :Ranking[U] =>
					appendedAll(ranking.toSet)
//					appendReversed(ranking.reverseIterator, ranking.size)
				case seq :collection.IndexedSeq[U] =>
					appendReversed(seq.reverseIterator, seq.length)
				case set  :collection.Set[U] =>
					appendedAll(set)
				case _ =>
					val it = elems.iterator
					var size = 0
					val reversed = elems.knownSize match {
						case newElemCount if newElemCount >= 0 && newElemCount <= 0x10000 => //reverse in an array
							size = newElemCount
							val res = new Array[Any](newElemCount).asInstanceOf[Array[U]]
							var i = newElemCount
							while (i > 0) {
								i -= 1; res(i) = it.next()
							}
							PassedArrayInternals.wrap(res)
						case _ =>                                                         //reverse in a list
							var reversed :List[U] = Nil
							while (it.hasNext) {
								reversed = it.next() :: reversed
								size += 1
							}
							reversed
					}
					appendReversed(reversed.iterator, size)
			}
			//filter out all our elements which are present in set and combine the results
			private def appendedAll[U >: T](set :collection.Set[U]) :Ranking[U] = {
				val suffixSize = set.knownSize
				filterNot(set) match {
					case empty if empty.isEmpty => Ranking.from(set)
					case prefix :SmallRanking[U] if suffixSize >= 0 && prefix.length + suffixSize <= SmallRankingCap =>
						val prefixSize = prefix.length
						val newArray = new Array[Any](prefixSize + suffixSize).asInstanceOf[Array[U]]
						val newHashes = new Array[Int](prefixSize + suffixSize)
						Array.copy(prefix.contents, 0, newArray, 0, prefixSize)
						Array.copy(prefix.hashCodes, 0, newHashes, 0, prefixSize)
						set.copyToArray(newArray, prefixSize, suffixSize)
						var i = prefixSize + suffixSize
						while (i > prefixSize) {
							i -= 1
							newHashes(i) = hashCodeOf(newArray(i))
						}
						new SmallRanking[U](newArray, newHashes)
					case prefix =>
						new IndexedRanking(prefix ++: set.toIndexedSeq)
				}
			}

		private def appendReversed[U >: T](reverse :Iterator[U], newElemCount :Int) = {
			if (newElemCount + size <= SmallRankingCap)
				appendSmallReversed(reverse)
			else
				appendLargeReversed(reverse)
		}
		private def appendSmallReversed[U >: T](reverse :Iterator[U]) :Ranking[U] = {
			val elems = new Array[Any](SmallRankingCap).asInstanceOf[Array[U]]
			val hashes = new Array[Int](SmallRankingCap)
			var lastPos = elems.length
			def addIfAbsent(elem :U, hash :Int, downto :Int = lastPos) = {
				var i = elems.length
				var notFound = true
				while (notFound & i > downto) {
					i -= 1
					notFound = hashes(i) != hash || elems(i) != elem
				}
				if (notFound) {
					val res = downto - 1
					elems(res) = elem
					hashes(res) = hash
					res
				} else
					downto
			}
			while (reverse.hasNext) {
				val next = reverse.next()
				val hash = hashCodeOf(next)
				lastPos = addIfAbsent(next, hash)
			}
			var idx = size
			while (idx > 0) {
				idx -= 1
				val elem = this.elements(idx)
				val hash = this.hashes(idx)
				lastPos = addIfAbsent(elem, hash)
			}
			if (lastPos == 0)
				new SmallRanking(elems, hashes)
			else {
				val returnedSize = SmallRankingCap - lastPos
				val returnedElems = new Array[Any](returnedSize).asInstanceOf[Array[U]]
				val returnedHashes = new Array[Int](returnedSize)
				System.arraycopy(elems, lastPos, returnedElems, 0, returnedSize)
				System.arraycopy(hashes, lastPos, returnedHashes, 0, returnedSize)
				new SmallRanking(returnedElems, returnedHashes)
			}
		}
		private def appendLargeReversed[U >: T](reverse :Iterator[U]) :Ranking[U] = {
			val res = new ReverseBuilder[U]()
			res ++= reverse
			var i = size
			while (i > 0) {
				i -= 1
				res += elements(i)
			}
			res.result()
		}


		override def concat[U >: T](suffix :IterableOnce[U]) :Ranking[U] = suffix match {
			case iterable :Iterable[U] if iterable.isEmpty => this
			case iterator :Iterator[_] if iterator.isEmpty => this
			case ranking  :Ranking[U]  => concat(ranking)
			case single   :Iterable[U] if single.sizeIs == 1 => added(single.head)
			case set      :collection.Set[U] => concat(set)
			case _ =>
				val builder = new UnorderedBuilder[U](this)
				(builder ++= suffix).result()
		}
		private def concat[U >: T](suffix :Ranking[U]) :Ranking[U] = suffix match {
			case small :SmallRanking[U] if small.size + elements.length <= SmallRankingCap =>
				concat(small)
			case _ if suffix.size == 1 => added(suffix.head)
			case _ if suffix.size > size =>
				new IndexedRanking[U](filterNot(suffix.contains) ++: suffix.toIndexedSeq)
			case _ =>
				new IndexedRanking[U](toIndexedSeq :++ suffix.filterNot(contains))
		}
		private def concat[U >: T](suffix :SmallRanking[U]) :Ranking[U] = {
			val smallSize = suffix.size
			var size = elements.length
			if (smallSize == 1) {
				val elem = suffix.head; val hash = suffix.hashCodes(0)
				indexOf(elem, hash) match {
					case -1 => append(elem, hash)
					case _  => this
				}
			} else if (size == 1) {
				val elem = elements(0); val hash = hashes(0)
				suffix.indexOf(elem, hash) match {
					case -1 => suffix.prepend(elem, hash)
					case  _ => suffix
				}
			} else {
				val tmpItems = Array.copyOf(elements.asInstanceOf[Array[U]], smallSize + size)
				val tmpHashes = Array.copyOf(hashes, smallSize + size)
				val smallItems = suffix.contents
				val smallHashes = suffix.hashCodes
				var i = 0
				while (i < smallSize) {
					val h = smallHashes(i)
					val e = smallItems(i)
					val idx = hashes.indexOf(h)
					if (idx < 0 || elements(idx) != e) {
						tmpItems(size) = e
						tmpHashes(size) = h
						size += 1
					}
					i += 1
				}
				if (size == tmpItems.length)
					new SmallRanking(tmpItems, tmpHashes)
				else {
					val resItems = Array.copyOf(tmpItems, size)
					val resHashes = Array.copyOf(tmpHashes, size)
					new SmallRanking(resItems, resHashes)
				}
			}
		}
		private def concat[U >: T](suffix :collection.Set[U]) :Ranking[U] =
			suffix.knownSize match {
				case suffixSize if suffixSize > 0 && suffixSize + elements.length <= SmallRankingCap =>
					appendedAll(suffix)
				case suffixSize if suffixSize > 0 && suffixSize < elements.length =>
					new IndexedRanking(toIndexedSeq :++ suffix.filterNot(contains))
				case suffixSize if suffixSize > elements.length =>
					appendedAll(suffix)
				case _ =>
					(new UnorderedBuilder[U] ++= this ++= suffix).result()
			}

		private def writeReplace = new RankingSerializer(elements)
	}




	@SerialVersionUID(Ver)
	private class SingletonRanking[+T](override val head :T) extends Ranking[T] {
		override def knownSize = 1
		override def size = 1
		override def last :T = head
		override def tail = Ranking.empty[T]
		override def init = Ranking.empty[T]

		override def iterator = Iterator.single(head)
		override def reverse :Ranking[T] = this
		override def reverseIterator = Iterator.single(head)

		override def foreach[U](f :T => U) :Unit = f(head)

		override def apply(n :Int) =
			if (n == 0) head
			else throw new IndexOutOfBoundsException(n.toString + " out of 1")

		override def updated[U >: T](n :Int, value :U) :Ranking[U] =
			if (n == 0) new SingletonRanking(value)
			else throw new IndexOutOfBoundsException("updated(" + n + ", " + value + ") out of 1")

		override def indexOf[U >: T](elem :U) = if (head == elem) 0 else -1
		override def indexWhere(p :T => Boolean, from :Int) :Int = if (from <= 0 && p(head)) 0 else -1

		override def prepended[U >: T](elem :U) =
			if (elem == head)
				this
			else {
				val array = new Array[Any](2).asInstanceOf[Array[U]]
				array(0) = elem
				array(1) = head
				new SmallRanking(array)
			}

		override def appended[U >: T](elem :U) =
			if (elem == head)
				this
			else {
				val array = new Array[Any](2).asInstanceOf[Array[U]]
				array(0) = head
				array(1) = elem
				new SmallRanking(array)
			}

		override def added[U >: T](elem :U) = appended(elem)

		override def prependedAll[U >: T](elems :Iterable[U]) = elems match {
			case empty :Iterable[_] if empty.isEmpty => this
			case single :Iterable[U] if single.sizeIs == 1 => prepended(single.head)
			case ranking :Ranking[U] => if (ranking.contains(head)) ranking else ranking :+ head
			case set :collection.Set[U @unchecked]
				if (try set.contains(head) catch { case _ :ClassCastException => false })
			=>
				Ranking.from(elems)
			case _ => (newBuilder[U] ++= elems += head).result()
		}

		override def appendedAll[U >: T](elems :IterableOnce[U]) = elems match {
			case empty :Iterable[_] if empty.isEmpty => this
			case single :Iterable[U] if single.sizeIs == 1 => appended(single.head)
			case ranking :Ranking[U] => if (ranking.contains(head)) ranking else head +: ranking
			case set :collection.Set[U @unchecked]
				if (try set.contains(head) catch { case _ :ClassCastException => false })
			=>
				Ranking.from(set)
			case _ =>
				val res = appendingBuilder[U]
				res ++= this ++= elems
				res.result()
		}

		override def concat[B >: T](suffix :IterableOnce[B]) :Ranking[B] = suffix match {
			case empty :Iterable[_] if empty.isEmpty =>
				this
			case ranking :Ranking[B] => if (ranking.contains(head)) ranking else head +: ranking
			case _ => this :++ suffix
		}

		override def removed[U >: T](elem :U) :Ranking[T] =
			if (elem == head) Ranking.empty[T] else this

		override def removedAll[U >: T](elems :IterableOnce[U]) :Ranking[T] = elems match {
			case empty :Iterable[_] if empty.isEmpty => this
			case ranking :Ranking[U] => if (ranking.contains(head)) Ranking.empty else this
			case set :HashSet[U @unchecked] => if (set(head)) Ranking.empty else this
			case set :mutable.HashSet[U @unchecked] => if (set(head)) Ranking.empty else this
			case set :mutable.LinkedHashSet[U @unchecked] => if (set(head)) Ranking.empty else this
			case _ =>
				val it = elems.iterator
				var unequal = false
				while (it.hasNext && { unequal = it.next() != head; unequal })
					()
				if (unequal) this else Ranking.empty
		}

		override def toSeq :Seq[T] = head::Nil
		override def toIndexedSeq :IndexedSeq[T] = PassedArray.one(head)
		override def toSet[U >: T] :Set[U] = Set.empty[U] + head
		override def toString = "Ranking(" + head + ")"
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
	private class IndexedRanking[+T](items :IndexedSeq[T], map :Map[T, Int]) extends IterableProxy[T] with Ranking[T] {
		def this(items :IndexedSeq[T]) =
			this(items, items.view.zipWithIndex.toMap)

		assert(items.length == map.size, "index " + map + " inconsistent with elements " + items)

		protected override def underlying :IndexedSeq[T] = items
		private[this] val index = map.withDefaultValue(-1)

		override def knownSize :Int   = items.length

		override def reverseIterator :Iterator[T] = items.reverseIterator

		override def reverse  :Ranking[T]  = items.length match {
			case 0 => Ranking.empty
			case n if n <= SmallRankingCap =>
				val res = newBuilder[T]; res.sizeHint(n)
				var i = n
				while (i > 0) {
					i -= 1; res += items(i)
				}
				res.result()
			case n if n <= IndexedSeqDefaults.defaultApplyPreferredMaxLength =>
				new IndexedRanking(items.reverse)
			case _ =>
				new ReverseRankingAdapter[T](this)
		}

		override def foreach[U](f :T => U) :Unit = items foreach f

		override def apply(idx :Int) :T = items(idx)
		override def updated[U >: T](idx :Int, value :U) = {
			val old = items(idx)
			if (old == value)
				this
			else {
				val seq = items.toVector.updated(idx, value)
				val index = map.asInstanceOf[Map[U, Int]].updated(value, idx) - old
				new IndexedRanking[U](seq, index)
			}
		}

		override def indexOf[U >: T](elem :U) :Int = index(elem.asInstanceOf[T])
		override def indexWhere(p :T => Boolean, from :Int) :Int = //scala 2.13.10 bug
			if (from >= items.length) -1 else items.indexWhere(p, from)

		override def added[U >: T](elem :U) :Ranking[U] =
			if (contains(elem))
				this
			else
				new IndexedRanking(items.toVector :+ elem, HashMap.from[U, Int](index).updated(elem, size))

		override def appended[U >: T](elem :U) :Ranking[U] = indexOf(elem) match {
			case -1 =>
				new IndexedRanking(items.toVector :+ elem, HashMap.from[U, Int](index).updated(elem, size))
			case n => //todo: check performance characteristics of Vector.slice and Vector.concat
				val b = IndexedSeq.newBuilder[U]; b sizeHint size
				var i = 0
				val it = items.iterator
				//copy until the position of the elem in the sequence
				while (i < n) {
					b += it.next()
					i += 1
				}
				//skip elem and copy all following elements in items
				it.next(); i += 1; val len = items.length
				var newIndex = HashMap.from[U, Int](index)
				while (i < len) {
					val e = it.next()
					b += e
					newIndex = newIndex.updated(e, i - 1)
					i += 1
				}
				b += elem
				new IndexedRanking(b.result(), newIndex.updated(elem, len - 1))
		}

		override def prepended[U >: T](elem :U) :Ranking[U] = indexOf(elem) match {
			case -1 =>
				new IndexedRanking(elem +: items.toVector)
			case 0  =>
				this
			case  n =>
				val view = items.view
				new IndexedRanking(((elem +: view.take(n)) ++ view.drop(n + 1)).toIndexedSeq)
		}
//			if (contains(elem)) //this could be implemented analogously to :+, somewhat faster
//				this
//			else
//                new IndexedRanking(
//					elem +: items.toVector,
//                    HashMap.from(index.view.map[(U, Int)](pair => pair._1 -> (pair._2 + 1))).updated(elem, 0)
//                )

		override def appendedAll[U >: T](elems :IterableOnce[U]) :Ranking[U] = elems match {
			case it :Iterable[_] if it.isEmpty => this
			case ranking :Ranking[U] => appendUnique(ranking)
			case set :Set[U] @unchecked => appendUnique(set)
			case it  :Iterator[U] if !it.hasNext => this
			case _ =>
				val it = elems.iterator
				var unique = 0
				var suffixReversed = List.empty[U]
				while (it.hasNext) {
					val e = it.next()
					try {
						if (!map.contains(e.asInstanceOf[T]))
							unique += 1
					} catch {
						case _ :ClassCastException => unique += 1
					}
					suffixReversed = e :: suffixReversed
				}
				if (suffixReversed.isEmpty)
					this
				else {
					val res = new ReverseBuilder[U]
					res sizeHint size + unique
					res ++= suffixReversed
					if (unique > 0)
						res ++= items.reverseIterator
					res.result()
				}
		}
		private def appendUnique[U >: T](elems :Iterable[U]) :Ranking[U] = {
			var unique = 0
			val duplicates = mutable.Set.empty[U]
			val suffix = elems.iterator
			while (suffix.hasNext) {
				val elem = suffix.next()
				try {
					if (map.contains(elem.asInstanceOf[T]))
						duplicates += elem
					else
						unique += 1
				} catch {
					case _ :ClassCastException => unique += 1
				}
			}
			val res = new RankingBuilder[U](IndexedSeq.newBuilder[U], Map.empty[U, Int])
			res sizeHint this.size + unique
			val prefix = items.iterator
			while (prefix.hasNext) {
				val elem = prefix.next()
				if (!duplicates(elem))
					res += elem
			}
			(res ++= elems).result()
		}

		override def prependedAll[U >: T](elems :Iterable[U]) :Ranking[U] = elems match {
			case empty  if empty.isEmpty => this
			case single if single.sizeIs == 1 => prepended(single.head)
			case ranking :Ranking[U] =>
				val unique = !(if (ranking.size < size) ranking.exists(contains) else exists(ranking.contains))
				if (unique)
					ranking ++ this
				else if (ranking.size >= size && forall(ranking.contains))
					ranking
				else
					(new RankingBuilder[U](IndexedSeq.newBuilder, Map.empty) ++= ranking ++= this).result()
			case set :collection.Set[U @unchecked] =>
				val setSize = set.size
				val unique = !(if (setSize < size) set.exists(contains) else exists(set))
				if (unique)
					new IndexedRanking(set ++: items)
				else if (setSize >= size && forall(set))
					Ranking.from(set)
				else
					(new RankingBuilder[U](IndexedSeq.newBuilder, Map.empty) ++= set ++= this).result()
			case _ =>
				(new RankingBuilder[U](IndexedSeq.newBuilder, Map.empty) ++= elems ++= this).result()
		}

		override def removed[U >: T](elem :U) :Ranking[T] =
			index.get(elem.asInstanceOf[T]) match {
				case Some(i) if i == items.length - 1 =>
					new IndexedRanking[T](items.init, HashMap.from(index) - elem.asInstanceOf[T])
				case Some(i) =>
					val view = items.view
					val tail = view.drop(i + 1)
					var newIndex = try {
						HashMap.from(index) - elem.asInstanceOf[T]
					} catch {
						case _ :ClassCastException => index
					}
					tail foreach { e => newIndex = newIndex.updated(e, index(e) - 1) }
					new IndexedRanking[T]((view.take(i) ++ tail).toIndexedSeq, newIndex)
				case _ => this
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


		override def concat[U >: T](that :IterableOnce[U]) :Ranking[U] = {
			val i = that.iterator
			if (i.isEmpty)
				this
			else {
				//create a builder initialized with the data from this collection
                val builder = new RankingBuilder(IndexedSeq.newBuilder[U] ++= items, HashMap.from[U, Int](index))
				(builder ++= i).result()
			}
		}

		override def toIndexedSeq :IndexedSeq[T] = items
		override def toSeq :Seq[T] = items
		override def toSet[U >: T] :Set[U] = HashMap.from(index).keySet.castParam[U]
		private[Ranking] def indices :Map[_ <: T, Int] = map

		private[this] def writeReplace = new RankingSerializer(this)
	}




	@SerialVersionUID(Ver)
	private class LazyRanking[T](override protected[this] var initializer: () => Ranking[T])
		extends Ranking[T] with AbstractIdempotent[Ranking[T]]
	{
		override def knownSize = definite.size
		override def apply(idx :Int) :T = definite(idx)
		override def updated[U >: T](idx :Int, value :U) :Ranking[U] = definite.updated(idx, value)
		override def indexOf[U >: T](elem :U) :Int = definite.indexOf(elem)
		override def indexWhere(p :T => Boolean, from :Int) :Int = definite.indexWhere(p, from)

		override def prepended[U >: T](elem :U) :Ranking[U] = elem +: definite
		override def appended[U >: T](elem :U) :Ranking[U] = definite :+ elem
		override def added[U >: T](elem :U) :Ranking[U] = definite + elem

		override def appendedAll[U >: T](elems :IterableOnce[U]) :Ranking[U] =
			if (elems.iterator.isEmpty) this else definite :++ elems

		override def prependedAll[U >: T](elems :Iterable[U]) :Ranking[U] = elems ++: definite
		override def removedAll[U >: T](elems :IterableOnce[U]) :Ranking[T] = definite -- elems
		override def removed[U >: T](elem :U) :Ranking[T] = definite - elem

		override def concat[B >: T](suffix :IterableOnce[B]) :Ranking[B] = definite ++ suffix

		override def foreach[U](f :T => U) :Unit = definite foreach f
		override def iterator :Iterator[T] = definite.iterator
		override def reverse :Ranking[T] = definite.reverse
		override def reverseIterator :Iterator[T] = definite.reverseIterator

		private def writeReplace = definite
	}




	@SerialVersionUID(Ver)
	private class RankingSeqAdapter[+T](ranking :Ranking[T])
		extends AbstractSeq[T] with IndexedSeq[T] with Serializable
	{
		override def length :Int = ranking.size
		override def knownSize = length

		override def apply(idx :Int) :T = ranking(idx)

		override def indexOf[U >: T](elem :U, start :Int) :Int = {
			val i = ranking.indexOf(elem)
			if (i < start) -1 else i
		}

		override def lastIndexOf[U >: T](elem :U, end :Int) :Int = {
			val i = ranking.indexOf(elem)
			if (i > end) -1 else i
		}

		override def contains[U >: T](elem :U) :Boolean = ranking.contains(elem)

		override def iterator :Iterator[T] = ranking.iterator

		override def foreach[U](f :T => U) :Unit = ranking foreach f

		override def toSet[U >: T] :Set[U] = ranking.toSet

		def toRanking :Ranking[T] = ranking

		private def writeReplace = (ranking :Ranking[Any]) to PassedArray
	}



	@SerialVersionUID(Ver)
	private class RankingSetAdapter[T](ranking :Ranking[T])
		extends AbstractSet[T] with Set[T] with Serializable
	{
		override def knownSize :Int = ranking.knownSize
		override def size      :Int = ranking.size
		override def contains(elem :T) :Boolean = ranking.contains(elem)

		override def incl(elem :T) :Set[T] = if (contains(elem)) this else Set(toSeq:_*)
		override def excl(elem :T) :Set[T] = if (contains(elem)) Set(toSeq:_*) - elem else this

		override def iterator :Iterator[T] = ranking.iterator
		override def foreach[U](f :T => U) :Unit = ranking foreach f

		override def toIndexedSeq :IndexedSeq[T] = ranking.toIndexedSeq
		override def toSeq :Seq[T] = ranking.toSeq
		def toRanking :Ranking[T] = ranking

		private def writeReplace = ranking to HashSet
	}



	@SerialVersionUID(Ver)
	private class ReverseRankingAdapter[T](override val reverse :Ranking[T]) extends Ranking[T] {
		@inline private def sizeLog = java.lang.Integer.highestOneBit(reverse.size)
		override def size            = reverse.size
		override def knownSize       = reverse.knownSize
		override def iterator        = reverse.reverseIterator
		override def reverseIterator = reverse.iterator

		@inline private[this] def newInstance[U >: T](reverse :Ranking[U]) =
			if (reverse eq this.reverse) this
			else if (reverse.isEmpty) Ranking.empty
			else new ReverseRankingAdapter(reverse)

		override def apply(n :Int) =
			if (n < 0)
				throw new IndexOutOfBoundsException(n.toString + " out of " + reverse.size)
			else
				reverse(reverse.size - n - 1)

		override def updated[U >: T](n :Int, value :U) =
			if (n < 0)
				throw new IndexOutOfBoundsException(n.toString + " out of " + reverse.size)
			else
				new ReverseRankingAdapter(reverse.updated(reverse.size - n - 1, value))

		override def indexOf[U >: T](elem :U) = reverse.indexOf(elem) match {
			case -1 => -1
			case n  => reverse.size - n - 1
		}
		override def indexWhere(p :T => Boolean, from :Int) :Int =
			if (from >= reverse.size)
				-1
			else {
				var i = if (from <= 0) reverse.size - 1 else reverse.size - from - 1
				while (i >= 0 && !p(reverse(i)))
					i -= 1
				if (i < 0) -1 else reverse.size - i - 1
			}

		override def added[U >: T](elem :U)  = newInstance(reverse + elem)
		override def prepended[U >: T](elem :U) = newInstance(reverse :+ elem)
		override def appended[U >: T](elem :U) = newInstance(elem +: reverse)

		override def concat[U >: T](elems :IterableOnce[U])  = elems match {
			case it :Iterable[U] if it.isEmpty  => this
			case it :Iterator[U] if !it.hasNext => this
			case it :Iterable[U] if it.sizeIs <= sizeLog => newInstance(reverse ++ elems)
			case it :Iterator[U] if { val n = it.knownSize; n >= 0 && n <= sizeLog } => newInstance(reverse ++ elems)
			case _ => (newBuilder[U] ++= this ++= elems).result()
		}
		override def appendedAll[U >: T](elems :IterableOnce[U]) = elems match {
			case it :Iterable[U] if it.isEmpty           => this
			case it :Iterator[U] if !it.hasNext          => this
			case it :Iterable[U] if it.sizeIs <= sizeLog =>
				val appended = ((reverse :Ranking[U]) /: it) { (result, elem) => elem +: result }
				new ReverseRankingAdapter[U](appended)
			case it :Iterator[U] if { val n = it.knownSize; n >= 0 && n <= sizeLog } =>
				val appended = ((reverse :Ranking[U]) /: it) { (result, elem) => elem +: result }
				new ReverseRankingAdapter(appended)
			case _ =>
				(newBuilder[U] ++= this ++= elems).result()
		}
		override def prependedAll[U >: T](elems :Iterable[U]) =
			if (elems.isEmpty)
				this
			else if (elems.sizeIs <= sizeLog) {
				val appended = (elems :\ (reverse :Ranking[U])) { (elem, result) => result :+ elem }
				new ReverseRankingAdapter[U](appended)
			} else
				(newBuilder[U] ++= elems ++= this).result()

		override def removed[U >: T](elem :U) = (reverse - elem) match {
			case same  if same eq reverse => this
			case other if other.isEmpty   => Ranking.empty
			case other                    => new ReverseRankingAdapter(other)
		}
		override def removedAll[U >: T](elems :IterableOnce[U]) = elems match {
			case it :Iterable[U] if it.isEmpty           => this
			case it :Iterator[U] if !it.hasNext          => this
			case it :Iterable[U] if it.sizeIs <= sizeLog => newInstance(reverse -- elems)
			case it :Iterator[U] if { val n = it.knownSize; n >= 0 && n <= sizeLog } => newInstance(reverse -- it)
			case deleted :Set[U] =>
				(newBuilder[T] /: this) {
					(builder, item) => if (deleted(item)) builder else builder += item
				}.result()
			case _ =>
				val deleted = elems.iterator.collect { case item if reverse.indexOf(item) >= 0 => item } to Set
				(newBuilder[T] /: this) {
					(builder, item) => if (deleted(item)) builder else builder += item
				}.result()
		}

		private def writeReplace = new RankingSerializer(this)
	}



	@SerialVersionUID(Ver)
	private class RankingSerializer[+E](elems :Array[E]) extends Serializable {
		def this(elems :Ranking[E]) = this((elems :Ranking[Any]).to(Array).castParam[E])

		private def readResolve =
			if (elems.length <= SmallRankingCap) new SmallRanking(elems)
			else new IndexedRanking(PassedArrayInternals.wrap(elems))
	}

	override def toString = "Ranking"
}






//extracted to allow other classes to extend it.
@SerialVersionUID(Ver)
private[collections] class EmptyRanking extends Ranking[Nothing] {
	override def knownSize = 0
	override def size = 0
	override def apply(n :Int) :Nothing =
		throw new IndexOutOfBoundsException("Ranking()(" + n + ")")
	override def updated[U](n :Int, value :U) =
		throw new IndexOutOfBoundsException("Ranking().updated(" + n + ", " + value + ")")
	override def indexOf[U](elem :U) :Int = -1
	override def indexWhere(p :Nothing => Boolean, from :Int) :Int = -1

	override def prepended[U](elem :U) :Ranking[U] = Ranking.one(elem)
	override def appended[U](elem :U) :Ranking[U] = Ranking.one(elem)
	override def added[U](elem :U) :Ranking[U] = Ranking.one(elem)
	override def concat[U](elems :IterableOnce[U]) :Ranking[U] = Ranking.from(elems)
	override def appendedAll[U](elems :IterableOnce[U]) :Ranking[U] = Ranking.from(elems)
	override def prependedAll[U](elems :Iterable[U]) :Ranking[U] = Ranking.from(elems)
	override def removed[U](elem :U) :Ranking[Nothing] = this
	override def removedAll[U](elems :IterableOnce[U]) :Ranking[Nothing] = this

	override def iterator :Iterator[Nothing] = Iterator.empty
	override def reverse :Ranking[Nothing] = this
	override def reverseIterator :Iterator[Nothing] = Iterator.empty
	override def toSet[U >: Nothing] :Set[U] = Set.empty[U]
	override def toSeq :Seq[Nothing] = Nil
	override def toIndexedSeq :IndexedSeq[Nothing] = IndexedSeq.empty
}

