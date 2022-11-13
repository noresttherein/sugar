package net.noresttherein.sugar.collections


import scala.collection.immutable.{ArraySeq, HashSet, IndexedSeq, IndexedSeqDefaults, Iterable, Seq, Set}
import scala.collection.mutable.{Builder, ReusableBuilder}
import scala.collection.{mutable, AbstractSeq, AbstractSet, Factory, IterableFactory, IterableFactoryDefaults, IterableOps}
import scala.reflect.{classTag, ClassTag}

import net.noresttherein.sugar.collections.Ranking.{RankingSeqAdapter, RankingSetAdapter}
import net.noresttherein.sugar.vars.Idempotent.AbstractIdempotent
import net.noresttherein.sugar.vars.Opt.Got

//implicits
import net.noresttherein.sugar.extensions.{castTypeParam, foldingMethods, iterableMappingMethods}






/** A collection of unique items in a specific order providing `O(1)` `indexOf(T)`, `contains(T)`, `apply(Int)`,
  * `toSet`, `toSeq`, `toIndexedSeq` and `size` implementations. It can be thought of as a mix of [[Set]] and [[Seq]]:
  * no two elements in the collection are equal, but their position in the collection is important and features
  * in equality (essentially having `Seq` semantics), unlike `Set` equality, including implementations preserving
  * insertion order. Note that this means that sequence-like append/prepend operations will lead to no change
  * in the collection if the added items already exists.
  *
  * Implementations are optimised more towards fast access than efficient incremental expansion.
  * It is recommended to use [[net.noresttherein.sugar.collections.Ranking.newBuilder newBuilder]]
  * when removing or adding multiple elements, especially through methods other than `+` and `++`.
  *
  * This class is most prominently used for the column lists exported by mappings in order to quickly find
  * the column mapping for a given column result in a `ResultSet`.
  * @tparam T element type.
  */
trait Ranking[+T] //todo: revise .view usage for includes/excludes - probably not worth creating a view for it
	extends Iterable[T] with IterableOps[T, Ranking, Ranking[T]] with IterableFactoryDefaults[T, Ranking]
	   with Serializable
{ ranking =>
	@inline final def length :Int = size
	override def knownSize   :Int = size
	override def iterableFactory :IterableFactory[Ranking] = Ranking

	override def toIndexedSeq  :IndexedSeq[T] = new RankingSeqAdapter(this)
	override def toSeq         :Seq[T] = toIndexedSeq
	override def toSet[U >: T] :Set[U] = new RankingSetAdapter(this)

	override def to[C1](factory :Factory[T, C1]) :C1 = companionFactoryOf(factory) match {
		case Got(Seq) | Got(IndexedSeq) => toSeq.asInstanceOf[C1]
		case Got(Set) => toSet.asInstanceOf[C1]
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

	/** The index of the given element in this collection, or `-1` if it does not contain `elem`. */
	def indexOf[U >: T](elem :U) :Int


	/** Same as the standard [[net.noresttherein.sugar.collections.Ranking.indexOf indexOf]],
	  * but returns the index in an `Opt[Int]` instead of returning `-1` if no such element exists.
	  */
	def getIndexOf[U >: T](elem :U) :Option[Int] = indexOf(elem) match {
		case n if n >= 0 => Some(n)
		case _ => None
	}

	/** Same as the standard [[net.noresttherein.sugar.collections.Ranking.indexOf indexOf]],
	  * but throws a [[NoSuchElementException]] if the element is not found in this collection.
	  * The exception's message lists all the contents, so this method should not be used
	  * if this collection can be large or this might pose a security vulnerability, but the extra information
	  * is much more helpful than in a [[IndexOutOfBoundsException]] which most likely would be caused by returning
	  * `-1`.
	  */
	def sureIndexOf[U >: T](elem :U) :Int = indexOf(elem) match {
		case n if n < 0 => throw new NoSuchElementException("No " + elem + " in " + this + ".")
		case n => n
	}

	/** Checks if this collection contains the given element as defined by `equals`. */
	def contains[U >: T](elem :U) :Boolean = indexOf(elem) >= 0

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


	/** Verifies if the element sets of the two collections are equal.
	  * @return value equal to `this.toSet == other.toSet`.
	  */
	def contentsEqual[U](other :Ranking[U]) :Boolean =
		size == other.size && other.forall(contains)


	override def className = "Ranking"
}




/**
  * $factoryInfo
  * @define Coll `Ranking`
  * @define coll ranking
  */
@SerialVersionUID(ver)
object Ranking extends IterableFactory[Ranking] {

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

	/** A new builder adding elements with semantics of [[net.noresttherein.sugar.collections.Ranking.+ +]]. */
	override def newBuilder[T] :Builder[T, Ranking[T]] = new RankingBuilder[T]

//	def reverseBuilder[T] :Builder[T, Ranking[T]] = new ReverseBuilder[T]

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

	@SerialVersionUID(ver)
	object implicits {
		implicit def rankingToSeq[T](ranking :Ranking[T]) :Seq[T] = ranking.toSeq
		implicit def rankingToSet[T](ranking :Ranking[T]) :Set[T] = ranking.toSet
	}




	private def deduplicateSmall[T](items :Iterable[T]) :Ranking[T] = {
		val array = items.toArray(ClassTag[T](classOf[AnyRef]))
		var hasNulls = false //does array contain nulls?
		var duplicates = 0   //if !hasNull then array.length - duplicates == array.toSet.size; else duplicates > 0 <=> there are duplicates
		var singleton :T = null.asInstanceOf[T] //the last non-null element we've seen
		var i = array.length
		while (i > 0 & !hasNulls) {
			i -= 1
			if (array(i) == null)
				hasNulls = true
			else
				singleton = array(i)
		}
		//if there are nulls, break as soon as you find a duplicate; otherwise run through the range and count duplicates
		while (i > 0 & (duplicates == 0 | hasNulls)) {
			i -= 1
			var j = i
			val ati = array(i)
			while (j > 0 & (duplicates == 0 | hasNulls)) {
				j -= 1
				if (!hasNulls) { //lets set to null the duplicate cells so we don't count the duplicates twice
					if (ati != null && ati == array(j))
						duplicates += 1
					array(j) = null.asInstanceOf[T]
				} else if (array(i) == array(j))
					duplicates += 1
			}
		}
		if (duplicates == 0)
			new SmallRanking(array)
		else if (!hasNulls && duplicates == array.length - 1)
			new SingletonRanking(singleton)
		else if (hasNulls)
			new SmallRanking(array.toSet[T].toArray[T](classTag[AnyRef].asInstanceOf[ClassTag[T]]))
		else {
			val deduped = new Array[AnyRef](array.length - duplicates).asInstanceOf[Array[T]]
			i = array.length
			var j = deduped.length
			while (i > 0) { //copy non-null elements from array to deduped
				i -= 1
				val elem = array(i)
				if (elem != null) {
					j -= 1
					array(j) = array(i)
				}
			}
			new SmallRanking(deduped)
		}
	}

	private def deduplicateLarge[T](items :Iterable[T]) :Ranking[T] = {
		val seq = items match {
			case seq :IndexedSeq[T] => seq
			case _ => items to PassedArray
		}
		val map = seq.view.zipWithIndex.toMap
		if (map.size == seq.length)
			new IndexedRanking(seq, map)
		else {
			//remove duplicates, leave only those lucky to be in map
			val ranking = seq.flatMapWithIndex { (elem, i) => if (map(elem) == i) Some(elem) else None }
			val index  = seq.view.zipWithIndex.toMap
			new IndexedRanking(ranking, index)
		}
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
	  */
	private final class RankingBuilder[T] private(
                        private[this] var large     :Builder[T, IndexedSeq[T]],
                        private[this] var index     :Map[T, Int], //<-^builds IndexedRanking
                        private[this] var small     :Array[T], //<- builds SmallRanking
                        private[this] var hashes    :Array[Int], //hash codes of elements in small
                        private[this] var smallSize :Int) //the number of elements in small or -1 if large is used instead
		extends ReusableBuilder[T, Ranking[T]]
	{   //todo: support null element
		def this(items :Builder[T, IndexedSeq[T]], index :Map[T, Int]) =
			this(items, index, null, null, -1)

		def this(ranking :SmallRanking[T]) =
			this(null, null,
				Array.copyOf(ranking.contents, SmallRankingCap),
				Array.copyOf(ranking.hashCodes, SmallRankingCap),
				ranking.size)

		def this() =
			this(null, null, null, null, 0)

		override def sizeHint(size :Int) :Unit = //todo: support small.length < SmallRankingCap
			if (size >= SmallRankingCap && smallSize >= 0) {
				if (large == null) {
					large = IndexedSeq.newBuilder[T]
					index = Map.empty
				}
				large sizeHint size
				var i = 0
				while (i < smallSize) {
					val elem = small(i)
					large += elem
					index = index.updated(elem, i)
					i += 1
				}
				smallSize = -1 //add directly to the builder, skip the array
			}

		/** Called when smallSize >= 0. */
		private def smallAddOne(elem :T, hash :Int) :this.type = {
			if (small == null) {
				small  = new Array[Any](SmallRankingCap).asInstanceOf[Array[T]]
				hashes = new Array[Int](SmallRankingCap)
				small(0)  = elem
				hashes(0) = hash
				smallSize = 1
			} else {
				val i = hashes.indexOf(hash)
				if (i < 0 || small(i) != elem)
					if (smallSize < SmallRankingCap) {
						small(smallSize)  = elem
						hashes(smallSize) = hash
						smallSize += 1
					} else {
						if (large == null)
							large = IndexedSeq.newBuilder[T]
						large ++= small
						index = Map.empty
						mutable.ArraySeq.make(small).foreachWithIndex { (elem, i) => index = index.updated(elem, i) }
						smallSize = -1
					}
			}
			this
		}

		private def bigAddOne(elem :T) :this.type = {
			if (!index.contains(elem)) {
				index = index.updated(elem, index.size)
				large += elem
			}
			this
		}

		override def addOne(elem :T) :this.type =
			if (smallSize >= 0)
				smallAddOne(elem, elem.hashCode)
			else
				bigAddOne(elem)

		/** Called when `smallSize >= 0` and `small.toSet intersect xs.toSet == Set()`. */
		private def largeAddRanking(xs :Ranking[T]) :this.type = {
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

		/** Tries to fit `xs` into `small` buffer. Assumes `xs.size > smallSize`, so it's faster to filter `small`
		  * not contained in `xs` and then add `xs` knowing the elements are unique.
		  */
		private def smallAddAll(xs :Ranking[T]) :this.type = xs match {
			case ranking :SmallRanking[T] =>
				val newElems  = new Array[Any](SmallRankingCap).asInstanceOf[Array[T]]
				val newHashes = new Array[Int](SmallRankingCap)
				var i = 0; var j = 0
				while (i < smallSize) {                 //let newElems = small.filterNot(xs.contains)
					val item = small(i); val hash = hashes(i)
					if (ranking.indexOf(item, hash) < 0) {
						newElems(j)  = item
						newHashes(j) = hash
						j += 1
					}
					i += 1
				}
				small = newElems
				hashes = newHashes
				val otherSize = xs.size
				if (j + otherSize <= SmallRankingCap) { //current elements and xs together fit in a SmallRanking
					System.arraycopy(ranking.contents, 0, small, j, otherSize)
					System.arraycopy(ranking.hashCodes, 0, hashes, j, otherSize)
					smallSize = j + otherSize
				} else {
					smallSize = j
					largeAddRanking(xs)
				}
				this
			case _ =>
				val newElems  = new Array[Any](SmallRankingCap).asInstanceOf[Array[T]]
				val newHashes = new Array[Int](SmallRankingCap)
				var i = 0; var j = 0
				while (i < smallSize) {                 //let newElems = small.filterNot(xs.contains)
					val item = small(i)
					if (xs.indexOf(item) < 0) {
						newElems(j)  = item
						newHashes(j) = item.hashCode
						j += 1
					}
					i += 1
				}
				small  = newElems
				hashes = newHashes
				val otherSize = xs.size
				if (smallSize <= SmallRankingCap) {     //current elements and xs together fit in a SmallRanking
					i = 0
					while (i < otherSize) {
						val item = xs(i)
						newElems(j) = item; newHashes(j) = item.hashCode
						i += 1; j += 1
					}
					smallSize = j
					this
				} else {
					smallSize = j
					largeAddRanking(xs)
				}
		}

		/** Called when `smallSize <= 0` */
		private def become(xs :Ranking[T]) :this.type = xs match {
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
				smallAddOne(ranking.head, ranking.head.hashCode)
			case _ =>
				super.addAll(xs)
		}

		override def addAll(xs :IterableOnce[T]) :this.type =
			if (smallSize >= 0) xs match {
				case it :Iterable[T] if it.isEmpty => this
				case it :Ranking[T] if smallSize == 0 => become(it)
				case it :Ranking[T] if it.size > smallSize && it.size <= SmallRankingCap =>
					smallAddAll(it)
				case it :SingletonRanking[T] => addOne(it.head)
				case _ => super.addAll(xs)
			} else xs match {
				case it :Iterable[T] if it.isEmpty => this
				case it :Ranking[T]  if index == null || index.isEmpty => become(it)
				case it :Ranking[T]  if it.size > 4 * index.size => //this could be potentially duplicated for Set
					val elems    = large.result().filterNot(it.contains)
					if (elems.isEmpty) {
						large.clear()
						index = Map.empty
						become(it)
					} else {
						val newElems = it match {
							case ranking :IndexedRanking[T] => ranking.toSeq
							case _ => it
						}
						val size = elems.size
						large ++= elems ++= newElems
						val oldIndex = index
						index = newElems.foldLeftWithIndex(index) { (acc, elem, i) => index.updated(elem, i + size) }
						index = index ++ oldIndex
						this
					}
				case _ =>
					super.addAll(xs)
			}


		override def clear() :Unit = {
			smallSize = 0
			index = null
			large = null
			if (small != null)
				java.util.Arrays.fill(small.asInstanceOf[Array[AnyRef]], null)
		}
		@inline private def clear(result :Ranking[T]) :Ranking[T] = {
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
	  * Because, just like `RankingBuilder`, it keeps the first element added in favour of subsequent duplicates,
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
			small(0) = elem
			hashes(0) = elem.hashCode
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
				val hash = elem.hashCode
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



	@SerialVersionUID(ver)
	private class LazyRanking[T](override protected[this] var initializer: () => Ranking[T])
		extends Ranking[T] with AbstractIdempotent[Ranking[T]]
	{
		override def apply(idx :Int) :T = definite(idx)
		override def updated[U >: T](idx :Int, value :U) :Ranking[U] = definite.updated(idx, value)
		override def indexOf[U >: T](elem :U) :Int = definite.indexOf(elem)

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





	//we might consider LinkedHashMap implementation for lesser footprint and faster building, but
	//  1. it's mutable, hence likely problematic even if not shared,
	//  2. it doesn't have any provision for iterating in the reverse order.
	@SerialVersionUID(ver)
	private class IndexedRanking[+T](items :IndexedSeq[T], map :Map[T, Int]) extends Ranking[T] {
		def this(items :IndexedSeq[T]) = this(items, items.view.mapWithIndex { (t, i) => (t, i) }.toMap)

		private[this] val index = map.withDefaultValue(-1)
		override def size :Int = index.size
		override def isEmpty :Boolean = size == 0

		override def iterator :Iterator[T] = items.iterator
		override def reverseIterator :Iterator[T] = items.reverseIterator

		override def reverse  :Ranking[T]  = items.length match {
			case 0 => empty
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
				val seq = items.updated(idx, value)
				val index = map.asInstanceOf[Map[U, Int]].updated(value, idx) - old
				new IndexedRanking[U](seq, index)
			}
		}

		override def indexOf[U >: T](elem :U) :Int = index(elem.asInstanceOf[T])

		override def added[U >: T](elem :U) :Ranking[U] =
			if (contains(elem))
				this
			else
				new IndexedRanking(elem +: items, index.asInstanceOf[Map[U, Int]].updated(elem, size))

		override def appended[U >: T](elem :U) :Ranking[U] = indexOf(elem) match {
			case -1 =>
				new IndexedRanking(items :+ elem, index.asInstanceOf[Map[U, Int]].updated(elem, size))
			case n =>
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
				var newIndex = index
				while (i < len) {
					val e = it.next()
					b += e
					newIndex = newIndex.updated(e, i - 1)
					i += 1
				}
				b += elem
				new IndexedRanking(b.result(), newIndex.asInstanceOf[Map[U, Int]].updated(elem, len - 1))
		}

		override def prepended[U >: T](elem :U) :Ranking[U] =
			if (contains(elem)) //this could be implemented analogously to :+, somewhat faster
				this
			else
                new IndexedRanking(
					elem +: items,
                    index.asInstanceOf[Map[U, Int]].map(pair => pair._1 -> (pair._2 + 1)).updated(elem, 0)
                )

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
					if (!map.contains(e.asInstanceOf[T]))
						unique += 1
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
				if (map.contains(elem.asInstanceOf[T]))
					duplicates += elem
				else
					unique += 1
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

		override def prependedAll[U >: T](elems :Iterable[U]) :Ranking[U] =
			if (elems.isEmpty)
				this
			else
				(new RankingBuilder(IndexedSeq.newBuilder[U] ++= elems, elems.view.zipWithIndex.toMap) ++= items).result()

		override def removed[U >: T](elem :U) :Ranking[T] =
			index.get(elem.asInstanceOf[T]) match {
				case Some(i) =>
					val view = items.view
					val tail = view.drop(i + 1)
					var newIndex = index - elem.asInstanceOf[T]
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
                val builder = new RankingBuilder(IndexedSeq.newBuilder[U] ++= items, index.asInstanceOf[Map[U, Int]])
				(builder ++= i).result()
			}
		}

		override def toIndexedSeq :IndexedSeq[T] = items
		override def toSeq :Seq[T] = items
		override def toSet[U >: T] :Set[U] = index.keySet.castParam[U]
		private[Ranking] def indices :Map[_ <: T, Int] = map

		private[this] def writeReplace = new RankingSerializer(this)
	}






	@SerialVersionUID(ver)
	private final class SmallRanking[+T] private[Ranking](elements :Array[T], hashes :Array[Int]) extends Ranking[T] {
		def this(elements :Array[T]) = this(elements, elements.map(_.hashCode))

		private[Ranking] def contents[U >: T]  :Array[U]   = elements.asInstanceOf[Array[U]]
		private[Ranking] def hashCodes :Array[Int] = hashes

		@inline override def size = elements.length
		override def knownSize = size
		override def last = elements(elements.length - 1)
		override def head = elements(0)
		override def tail = new SmallRanking(elements.tail)
		override def init = new SmallRanking(elements.init)

		override def iterator = elements.iterator
		override def reverse = new SmallRanking(elements.reverse, hashes.reverse)
		override def reverseIterator = toIndexedSeq.reverseIterator
		override def toIndexedSeq = ArraySeq.unsafeWrapArray(elements)

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
			val hash = value.hashCode
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

		override def indexOf[U >: T](elem :U) :Int = indexOf(elem, elem.hashCode)
		def indexOf[U >: T](elem :U, hash :Int) :Int = hashes.indexOf(hash) match {
			case n if n >= 0 && elements(n) == elem => n
			case n => n
		}

		override def removed[U >: T](elem :U) = indexOf(elem) match {
			case n if n < 0 => this
			case n =>
				val res = Array.ofDim[T](elements.length - 1)(ClassTag(elements.getClass.getComponentType))
				val hs  = new Array[Int](elements.length - 1)
				var i = 0; var j = 0
				while (i < elements.length) {
					if (i != n) {
						res(j) = elements(i)
						hs(j) = hs(i)
						j += 1
					}
					i += 1
				}
				new SmallRanking(res)
		}

		override def removedAll[U >: T](elems :IterableOnce[U]) = elems match {
			case empty :Iterable[_] if empty.isEmpty => this
			case _ =>
				val remove = new Array[Boolean](elements.length)
				var size = elements.length
				val it = elems.iterator
				while (it.hasNext)
					indexOf(it.next()) match {
						case n if n < 0 =>
						case n =>
							remove(n) = true
							size -= 1
					}
				size match {
					case 0 => empty
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
			val hash = elem.hashCode
			indexOf(elem, hash) match {
				case n if n >= 0 =>
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
				case _ if elements.length >= SmallRankingCap =>
					val seq = elements.view.prepended(elem).toIndexedSeq
					val map = seq.view.zipWithIndex.toMap
					new IndexedRanking[U](seq, map)
				case _ =>
					val res = Array.ofDim[U](elements.length + 1)(ClassTag(elements.getClass.getComponentType))
					val hs  = new Array[Int](elements.length + 1)
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
		}

		override def appended[U >: T](elem :U) = {
			val hash = elem.hashCode
			indexOf(elem, hash) match {
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
			val hash = elem.hashCode
			indexOf(elem, hash) match {
				case n if n >= 0 => this
				case _ => append(elem, hash)
			}
		}

		private def append[U >: T](elem :U, hash :Int) :Ranking[U] =
			if (elements.length >= SmallRankingCap) {
				val seq = elements.view.appended(elem).toIndexedSeq
				val map = seq.view.zipWithIndex.toMap
				new IndexedRanking(seq, map)
			} else {
				val size = elements.length
				val res = Array.copyOf(elements.asInstanceOf[Array[U]], size + 1)
				val hs = Array.copyOf(hashes, size + 1)
				res(size) = elem
				hs(size) = hash
				new SmallRanking(res, hs)
			}

		override def prependedAll[U >: T](elems :Iterable[U]) = (newBuilder[U] ++= elems ++= this).result()

		override def appendedAll[U >: T](elems :IterableOnce[U]) :Ranking[U] = {
			val it = elems.iterator
			if (it.isEmpty)
				this
			else elems match {
				case empty :Iterable[_] if empty.isEmpty => this
				case empty :Iterator[_] if !empty.hasNext => this
				case seq :collection.IndexedSeq[U] =>
					appendReversed(seq.reverseIterator, seq.length)
				case ranking :Ranking[U] =>
					appendReversed(ranking.reverseIterator, ranking.size)
				case _ =>
					val it = elems.iterator
					var size = 0
					val reversed = elems.knownSize match {
						case newElemCount if newElemCount >= 0 && newElemCount <= 8190 => //reverse in an array
							size = newElemCount
							val res = new Array[Any](newElemCount).asInstanceOf[Array[U]]
							var i = newElemCount
							while (i > 0) {
								i -= 1; res(i) = it.next()
							}
							ArraySeq.unsafeWrapArray(res)
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
				while (i > downto && { i -= 1; hashes(i) != hash || elems(i) != elem })
					{}
				if (i > downto)
					downto
				else {
					val res = downto - 1
					elems(res) = elem
					hashes(res) = hash
					res
				}
			}
			while (reverse.hasNext) {
				val next = reverse.next()
				val hash = next.hashCode
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
			case small :SmallRanking[U] if small.size + elements.length <= SmallRankingCap =>
				def concatSmall = {
					val smallSize = small.size
					var size = elements.length
					val tmpItems = Array.copyOf(elements.asInstanceOf[Array[U]], smallSize + size)
					val tmpHashes = Array.copyOf(hashes, smallSize + size)
					val smallItems = small.contents
					val smallHashes = small.hashCodes
					var i = 0
					while (i < smallSize) {
						val h = smallHashes(i);
						val e = smallItems(i)
						val idx = hashes.indexOf(h)
						if (idx < 0 || elements(i) != e) {
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
				concatSmall
			case _ =>
				val builder = new RankingBuilder[U](this)
				(builder ++= suffix).result()
		}

		override def toSeq :Seq[T] = ArraySeq.unsafeWrapArray(elements)

		private def writeReplace = new RankingSerializer(elements)
	}


	private[this] final val SmallRankingCap = 16




	@SerialVersionUID(ver)
	private class SingletonRanking[+T](override val head :T) extends Ranking[T] {
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

		override def prepended[U >: T](elem :U) =
			if (elem == head) this else Ranking(elem, head)

		override def appended[U >: T](elem :U) =
			if (elem == head) this else Ranking(head, elem)

		override def added[U >: T](elem :U) =
			if (elem == head) this else Ranking(head, elem)

		override def prependedAll[U >: T](elems :Iterable[U]) = elems match {
			case empty :Iterable[_] if empty.isEmpty => this
			case _ => (newBuilder[U] ++= elems += head).result()
		}

		override def appendedAll[U >: T](elems :IterableOnce[U]) = elems match {
			case empty :Iterable[_] if empty.isEmpty => this
			case ranking :Ranking[U] => head +: ranking
			case _ =>
				Ranking.from(new Iterator[U] {
					private[this] val it = elems.iterator
					private[this] var i = 0
					override def hasNext = it.hasNext
					override def next() = { i += 1; if (i == 1) SingletonRanking.this.head else it.next() }
				})
		}

		override def concat[B >: T](suffix :IterableOnce[B]) :Ranking[B] = suffix match {
			case ranking :Ranking[B] => ranking + head
			case _ => this :++ suffix
		}

		override def removed[U >: T](elem :U) :Ranking[T] =
			if (elem == head) Ranking.empty[T] else this

		override def removedAll[U >: T](elems :IterableOnce[U]) :Ranking[T] = elems match {
			case empty :Iterable[_] if empty.isEmpty => this
			case ranking :Ranking[U] => if (ranking.contains(head)) empty else this
			case set :HashSet[U @unchecked] => if (set(head)) empty else this
			case set :mutable.HashSet[U @unchecked] => if (set(head)) empty else this
			case set :mutable.LinkedHashSet[U @unchecked] => if (set(head)) empty else this
			case _ =>
				val it = elems.iterator
				var unequal = false
				while (it.hasNext && { unequal = it.next() != head; unequal })
					()
				if (unequal) this else empty
		}

		override def toSeq :Seq[T] = head::Nil
		override def toString = "Ranking(" + head + ")"
	}




	@SerialVersionUID(ver)
	private class EmptyRanking extends Ranking[Nothing] {
		override def apply(n :Int) :Nothing =
			throw new IndexOutOfBoundsException("Ranking()(" + n + ")")
		override def updated[U](n :Int, value :U) =
			throw new IndexOutOfBoundsException("Ranking().updated(" + n + ", " + value + ")")
		override def indexOf[U](elem :U) :Int = -1

		override def prepended[U](elem :U) :Ranking[U] = new SingletonRanking(elem)
		override def appended[U](elem :U) :Ranking[U] = new SingletonRanking(elem)
		override def added[U](elem :U) :Ranking[U] = new SingletonRanking(elem)
		override def concat[U](elems :IterableOnce[U]) :Ranking[U] = from(elems)
		override def appendedAll[U](elems :IterableOnce[U]) :Ranking[U] = from(elems)
		override def prependedAll[U](elems :Iterable[U]) :Ranking[U] = from(elems)
		override def removed[U](elem :U) :Ranking[Nothing] = this
		override def removedAll[U](elems :IterableOnce[U]) :Ranking[Nothing] = this

		override def iterator :Iterator[Nothing] = Iterator.empty
		override def reverse :Ranking[Nothing] = this
		override def reverseIterator :Iterator[Nothing] = Iterator.empty
		override def toSeq :Seq[Nothing] = Nil
	}

	private val EmptyRanking = new EmptyRanking






	@SerialVersionUID(ver)
	private class RankingSeqAdapter[+T](ranking :Ranking[T])
		extends AbstractSeq[T] with IndexedSeq[T] with Serializable
	{
		override def length :Int = ranking.size

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



	@SerialVersionUID(ver)
	private class RankingSetAdapter[T](ranking :Ranking[T])
		extends AbstractSet[T] with Set[T] with Serializable
	{
		override def size :Int = ranking.size

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



	@SerialVersionUID(ver)
	private class ReverseRankingAdapter[T](override val reverse :Ranking[T]) extends Ranking[T] {
		@inline private def sizeLog = java.lang.Integer.highestOneBit(reverse.size)
		override def size      = reverse.size
		override def knownSize = reverse.knownSize
		override def iterator        = reverse.reverseIterator
		override def reverseIterator = reverse.iterator

		@inline private[this] def newInstance[U >: T](reverse :Ranking[U]) =
			if (reverse eq this.reverse) this
			else if (reverse.isEmpty) empty
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
			case other if other.isEmpty   => empty
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



	@SerialVersionUID(ver)
	private class RankingSerializer[+E](elems :Array[E]) extends Serializable {
		def this(elems :Ranking[E]) = this((elems :Ranking[Any]).to(Array).castParam[E])

		private def readResolve =
			if (elems.length <= SmallRankingCap) new SmallRanking(elems)
			else Ranking.from(ArraySeq.unsafeWrapArray(elems))
	}
}

