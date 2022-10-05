package net.noresttherein.sugar.collections


import scala.collection.immutable.{ArraySeq, HashSet, IndexedSeq, Iterable, Seq, Set}
import scala.collection.mutable.Builder
import scala.collection.{mutable, AbstractSeq, AbstractSet, Factory, IterableFactory, IterableFactoryDefaults, IterableOps}
import scala.reflect.ClassTag

import net.noresttherein.sugar.collections.Ranking.{RankingSeqAdapter, RankingSetAdapter}
import net.noresttherein.sugar.vars.Opt.Got

//implicits
import net.noresttherein.sugar.extensions.mappingMethods
import net.noresttherein.sugar.extensions.castTypeParam






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
{ unique =>

	override def knownSize :Int = size
	override def iterableFactory :IterableFactory[Ranking] = Ranking

	override def toIndexedSeq :IndexedSeq[T] = new RankingSeqAdapter(this)
	override def toSeq :Seq[T] = toIndexedSeq
	override def toSet[U >: T] :Set[U] = new RankingSetAdapter(this)

	override def to[C1](factory :Factory[T, C1]) :C1 = companionFactoryOf(factory) match {
		case Got(Seq) | Got(IndexedSeq) => toSeq.asInstanceOf[C1]
		case Got(Set) => toSet.asInstanceOf[C1]
		case _ => super.to(factory)
	}


	def reverse :Ranking[T] = reverseIterator to Ranking

	def reverseIterator :Iterator[T]

	/** The `n`-th element in this collection.
	  * @param n the index in the `[0..size-1]` range.
	  */
	def apply(n :Int) :T

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
	def +[U >: T](elem :U) :Ranking[U]

	/** Prepends an element to the front of the collection. If the element is already present, it is moved to the front
	  * of the collection, with the preceding elements shift back one place.
	  */
	def +:[U >: T](elem :U) :Ranking[U]

	/** Appends an element at the end of the collection. If the element is already present, it is moved to the back
	  * of the collection, with the following elements shift forward one place.
	  */
	def :+[U >: T](elem :U) :Ranking[U]

	/** Appends the given elements to this collection, with semantics equivalent to calling
	  * [[net.noresttherein.sugar.collections.Ranking.:+ :+]] for every element in `elems` in its iteration order.
	  */
	def :++[U >: T](elems :IterableOnce[U]) :Ranking[U]

	/** Prepends the given elements to this collection, with semantics equivalent to calling
	  * [[net.noresttherein.sugar.collections.Ranking.+: +:]] for every element in `elems`
	  * in its ''reverse'' iteration order.
	  */
	def ++:[U >: T](elems :Iterable[U]) :Ranking[U]

	/** Adds the given elements to this collection. The exact order of the elements in the returned collection
	  * is unspecified.
	  */
	def ++[U >: T](elems :IterableOnce[U]) :Ranking[U]

	/** Removes the given element from this collection, if present. */
	def -[U >: T](elem :U) :Ranking[T]

	/** Removes the given elements from this collection. */
	def --[U >: T](elems :IterableOnce[U]) :Ranking[T]


	/** Verifies if the element sets of the two collections are equal.
	  * @return value equal to `this.toSet == other.toSet`.
	  */
	def contentsEqual[U](other :Ranking[U]) :Boolean =
		size == other.size && other.forall(contains)


	override def className = "Unique"
}


/** Companion object serving as a factory for Unique - sequence-like collections with fast indexOf operations.
  * $factoryInfo
  * @define Coll `Ranking`
  * @define coll ranking
  */
@SerialVersionUID(ver)
object Ranking extends IterableFactory[Ranking] {

	override def from[T](elems :IterableOnce[T]) :Ranking[T] = elems match {
		case unique :Ranking[T] => unique
		case seq :RankingSeqAdapter[T] => seq.toUnique
		case set :RankingSetAdapter[T] => set.toUnique
		case iter :Iterable[_] if iter.isEmpty => empty[T]
		case iter :Iterator[_] if iter.isEmpty => empty[T]
		case iter :Iterable[T] if iter.sizeIs == 1 =>
			new SingletonRanking[T](iter.head)
		case iter :Iterable[T] if iter.sizeIs <= SmallRankingCap =>
			new SmallRanking[T](iter.toArray(ClassTag[T](classOf[AnyRef])))
		case iter :Iterable[T] =>
			val seq = iter.toIndexedSeq
			val map = seq.view.zipWithIndex.toMap
			new IndexedRanking(seq, map)
		case _ => (newBuilder[T] ++= elems).result()
	}

	override def newBuilder[T] :Builder[T, Ranking[T]] = new RankingBuilder[T]()

	override def empty[E] :Ranking[E] = EmptyRanking //reusableEmpty

	/** A specialized light `Ranking` implementation for collections containing only one element. */
	def one[T](singleton :T) :Ranking[T] = new SingletonRanking(singleton)

	/** A specialized light `Ranking` implementation for collections containing only one element. */
	def single[T](singleton :T) :Ranking[T] = new SingletonRanking(singleton)

	/** A `Unique[T]` with lazily evaluated contents. The initializer will be called only when any of the methods
	  * on the proxy is called. It will be executed at most once, withing a `synchronized` block for the proxy.
	  * Once computed, it remains thread safe but will incur no additional synchronization penalty.
	  */
	@inline def Lazy[T](init: => IterableOnce[T]) :Ranking[T] = delayed(from(init))

	/** A proxy to a lazily computed `Unique[T]`. The initializer will be called when any of the methods on the proxy
	  * is called. It will be executed at most once, withing a `synchronized` block for the proxy.
	  * Once computed, it remains thread safe but will incur no additional synchronization penalty.
	  */
	def delayed[T](init: => Ranking[T]) :Ranking[T] = new LazyRanking[T](() => init)


	def unapplySeq[T](elems :Ranking[T]) :Got[Seq[T]] = Got(elems.toSeq)


	/** An implicit extension of a ''by-name'' expression evaluating to a `Unique[T]` instance, adding a `delayed`
	  * method which creates a proxy using it to initialize its target. */
	implicit class DelayedUnique[T](initializer: => Ranking[T]) {
		/** Treats the `this` argument as a ''by-name'' expression to be evaluated only when the created `Ranking`
		  * proxy's contents are accessed.
		  * @return `Unique.delayed(initializer)`.
		  * @see [[net.noresttherein.sugar.collections.Ranking.delayed delayed]]
		  */
		@inline def delayed :Ranking[T] = Ranking.delayed(initializer)
	}

	@SerialVersionUID(ver)
	object Implicits {
		implicit def uniqueToSeq[T](unique :Ranking[T]) :Seq[T] = unique.toSeq
		implicit def uniqueToSet[T](unique :Ranking[T]) :Set[T] = unique.toSet
	}




	/** A builder of `Ranking` instances working in two modes, depending on the constructor used
	  * and constructor parameters as well as the number of elements added.
	  * In case the parameter list contains
	  *   - no arguments,
	  *   - `SmallUnique`,
	  *   - `smallSize >= 0 && smallSize <= SmallRankingCap` (if non zero, then arrays `small` and `hashes`
	  *     should be not null and contain that many elements and their computed hash codes in their prefixes),
	  * then it tries first to build a `SmallUnique`, appending elements to to the `small` array.
	  * However, if
	  *   - the number of elements added exceeds `SmallRankingCap`.
	  *   - `smallSize < 0` is given as a constructor argument,
	  *   - an `IndexedSeq` builder and an index `Map` are given as arguments,
	  *   - or `sizeHint` is called for a number greater than `SmallRankingCap`,
	  * it switches to the `IndexedUnique` building mode (by setting `smallSize = -1`, where elements are appended
	  * to the builder `large` and inserted in the map `index` associated with the next free index.
	  */
	private final class RankingBuilder[T] private(private[this] var large     :Builder[T, IndexedSeq[T]],
	                                              private[this] var index     :Map[T, Int], //<-^builds IndexedRanking
	                                              private[this] var small     :Array[T], //<- builds SmallRanking
	                                              private[this] var hashes    :Array[Int], //hash codes of elements in small
	                                              private[this] var smallSize :Int) //number of elements in small or -1 if large is used instead
		extends Builder[T, Ranking[T]]
	{
		def this(items :Builder[T, IndexedSeq[T]], index :Map[T, Int]) =
			this(items, index, null, null, -1)

		def this(unique :SmallRanking[T]) =
			this(null, null,
				Array.copyOf(unique.contents, SmallRankingCap),
				Array.copyOf(unique.hashCodes, SmallRankingCap),
				unique.size)

		def this() =
			this(null, null, null, null, 0)

		private def smallAddOne(elem :T, hash :Int) :this.type = {
			if (small == null) {
				small = new Array[Any](SmallRankingCap).asInstanceOf[Array[T]]
				hashes = new Array[Int](SmallRankingCap)
				small(0) = elem
				hashes(0) = hash
				smallSize = 1
			} else {
				val i = hashes.indexOf(hash)
				if (i < 0 || small(i) != elem)
					if (smallSize < SmallRankingCap) {
						small(smallSize) = elem
						hashes(smallSize) = hash
						smallSize += 1
					} else {
						if (large == null)
							large = IndexedSeq.newBuilder[T]
						large ++= small
						index = small.view.zipWithIndex.toMap
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

		override def addAll(xs :IterableOnce[T]) :this.type =
			if (smallSize >= 0) xs match {
				case unique :SmallRanking[T] =>
					var i = 0; val count = unique.size
					val elems = unique.contents; val hashes = unique.hashCodes
					while (i < count & smallSize >= 0) {
						smallAddOne(elems(i), hashes(i))
						i += 1
					}
					while (i < count) {
						bigAddOne(elems(i))
						i += 1
					}
					this
				case unique :SingletonRanking[T] => addOne(unique.head)
				case _ => super.addAll(xs)
			} else xs match {
				case unique :IndexedRanking[T] =>
					val elems = unique.toSeq
					val size = index.size
					large ++= elems
					index ++= Iterator.tabulate(elems.length)(i => (elems(i), i + size))
					this
				case _ => super.addAll(xs)
			}


		override def clear() :Unit = {
			smallSize = 0
			index = null
			large = null
			if (small != null)
				java.util.Arrays.fill(small.asInstanceOf[Array[AnyRef]], null)
		}

		override def sizeHint(size :Int) :Unit =
			if (size >= SmallRankingCap) {
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

		override def result() :Ranking[T] = {
			val res = smallSize match {
				case 0 => empty[T]
				case 1 => new SingletonRanking(small(0))
				case SmallRankingCap => new SmallRanking(small, hashes)
				case n if n >= 0 => new SmallRanking[T](small.take(n), hashes.take(n))
				case _ => new IndexedRanking(large.result(), index)
			}
			clear()
			res
		}
	}



	@SerialVersionUID(ver)
	private class LazyRanking[T](private[this] var initialize: () => Ranking[T]) extends Ranking[T] {
		@volatile private[this] var initialized :Ranking[T] = _
		private[this] var fastAccess :Ranking[T] = _

		protected def items :Ranking[T] = {
			if (fastAccess == null) {
				var init = initialized
				if (init != null) fastAccess = init
				else synchronized {
					init = initialized
					if (init != null) fastAccess = init
					else {
						fastAccess = initialize()
						initialized = fastAccess
						initialize = null
					}
				}
			}
			fastAccess
		}

		override def apply(idx :Int) :T = items(idx)
		override def indexOf[U >: T](elem :U) :Int = items.indexOf(elem)

		override def +:[U >: T](elem :U) :Ranking[U] = elem +: items
		override def :+[U >: T](elem :U) :Ranking[U] = items :+ elem
		override def +[U >: T](elem :U) :Ranking[U] = items + elem

		override def :++[U >: T](elems :IterableOnce[U]) :Ranking[U] =
			if (elems.iterator.isEmpty) this else items :++ elems

		override def ++:[U >: T](elems :Iterable[U]) :Ranking[U] = elems ++: items
		override def --[U >: T](elems :IterableOnce[U]) :Ranking[T] = items -- elems
		override def -[U >: T](elem :U) :Ranking[T] = items - elem

		override def concat[B >: T](suffix :IterableOnce[B]) :Ranking[B] = items ++ suffix

		override def foreach[U](f :T => U) :Unit = items foreach f
		override def iterator :Iterator[T] = items.iterator
		override def reverse :Ranking[T] = items.reverse
		override def reverseIterator :Iterator[T] = items.reverseIterator

		private def writeReplace = items
	}





	//we might consider LinkedHashMap implementation for lesser footprint and faster building, but
	//  1. it's mutable, hence likely problematic even if not shared,
	//  2. it doesn't have any provision for iterating in the reverse order.
	@SerialVersionUID(ver)
	private class IndexedRanking[+T](items :IndexedSeq[T], map :Map[T, Int]) extends Ranking[T] {
		def this(items :IndexedSeq[T]) = this(items, items.view.mapWithIndex { (t, i) => (t, i) }.toMap)

		private[this] val index = map.withDefaultValue(-1)

		override def iterator :Iterator[T] = items.iterator
		override def reverse = new IndexedRanking(items.reverse)
		override def reverseIterator :Iterator[T] = items.reverseIterator

		override def size :Int = index.size
		override def isEmpty :Boolean = size == 0

		override def foreach[U](f :T => U) :Unit = items foreach f

		override def apply(idx :Int) :T = items(idx)

		override def indexOf[U >: T](elem :U) :Int = index(elem.asInstanceOf[T])

		override def +[U >: T](elem :U) :Ranking[U] =
			if (contains(elem)) this
			else new IndexedRanking(elem +: items, index.asInstanceOf[Map[U, Int]].updated(elem, size))

		override def :+[U >: T](elem :U) :Ranking[U] = indexOf(elem) match {
			case -1 =>
				new IndexedRanking(items :+ elem, index.asInstanceOf[Map[U, Int]].updated(elem, size))
			case n =>
				val b = IndexedSeq.newBuilder[U]; b sizeHint size
				var i = 0
				val it = items.iterator
				while (i < n) {
					b += it.next()
					i += 1
				}
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

		override def +:[U >: T](elem :U) :Ranking[U] =
			if (contains(elem)) this
			else
                new IndexedRanking(
					elem +: items,
                    index.asInstanceOf[Map[U, Int]].map(pair => pair._1 -> (pair._2 + 1)).updated(elem, 0)
                )

		override def :++[U >: T](elems :IterableOnce[U]) :Ranking[U] = {
			val it = elems.iterator
			if (it.isEmpty) this
			else {
				val intersection = mutable.HashSet.empty[U]
				var size = 0
				val suffix = List.newBuilder[U]
				while (it.hasNext) {
					val e = it.next()
					if (index.contains(e.asInstanceOf[T]))
						intersection += e
					else
						size += 1
					suffix += e
				}
				val res = new RankingBuilder(IndexedSeq.newBuilder[U], Map.empty[U, Int])
				res sizeHint this.size + size
				val self = items.iterator
				while (self.hasNext) {
					val e = self.next()
					if (!intersection(e))
						res += e
				}
				(res ++= suffix.result()).result()
			}
		}

		override def ++:[U >: T](elems :Iterable[U]) :Ranking[U] =
			if (elems.isEmpty) this
			else (new RankingBuilder(IndexedSeq.newBuilder[U] ++= elems, elems.view.zipWithIndex.toMap) ++= items).result()

		override def -[U >: T](elem :U) :Ranking[T] =
			index.get(elem.asInstanceOf[T]) match {
				case Some(i) =>
					val view = items.view; val tail = view.drop(i + 1)
					var newIndex = index - elem.asInstanceOf[T]
					tail foreach { e => newIndex = newIndex.updated(e, index(e) - 1) }
					new IndexedRanking[T]((view.take(i) ++ tail).toIndexedSeq, newIndex)
				case _ => this
			}

		override def --[U >: T](elems :IterableOnce[U]) :Ranking[T] =
			elems match {
				case _ if isEmpty => this
				case empty :Iterable[_] if empty.isEmpty => this
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


		override def concat[U >: T](that :IterableOnce[U]) :Ranking[U] =
			if (that.iterator.isEmpty)
				this
			else
                (new RankingBuilder(IndexedSeq.newBuilder[U] ++= items, index.asInstanceOf[Map[U, Int]]) ++= that).result()

		override def toIndexedSeq :IndexedSeq[T] = items
		override def toSeq :Seq[T] = items
		private[Ranking] def indices :Map[_ <: T, Int] = map

		private[this] def writeReplace = new UniqueSerializer(this)
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

		override def indexOf[U >: T](elem :U) :Int = indexOf(elem, elem.hashCode)
		def indexOf[U >: T](elem :U, hash :Int) :Int = hashes.indexOf(hash) match {
			case n if n >= 0 && elements(n) == elem => n
			case n => n
		}

		override def -[U >: T](elem :U) = indexOf(elem) match {
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

		override def --[U >: T](elems :IterableOnce[U]) = elems match {
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

		override def +:[U >: T](elem :U) = {
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

		override def :+[U >: T](elem :U) = {
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

		override def +[U >: T](elem :U) = {
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

		override def ++:[U >: T](elems :Iterable[U]) = (newBuilder[U] ++= this ++= elems).result()

		override def :++[U >: T](elems :IterableOnce[U]) :Ranking[U] = {
			val it = elems.iterator
			if (it.isEmpty) this
			else {
				val intersection = mutable.HashSet.empty[U]
				var size = 0
				val suffix = List.newBuilder[U]
				while (it.hasNext) {
					val e = it.next()
					if (contains(e.asInstanceOf[T]))
						intersection += e
					else
						size += 1
					suffix += e
				}
				val res = newBuilder[U]
				res sizeHint this.size + size
				val self = iterator
				while (self.hasNext) {
					val e = self.next()
					if (!intersection(e))
						res += e
				}
				(res ++= suffix.result()).result()
			}
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
//				val suffixSize = suffix.knownSize
//				if (suffixSize >= 0)
//					builder.sizeHint(elements.length + suffixSize)
				(builder ++= suffix).result()
		}

		override def toSeq :Seq[T] = ArraySeq.unsafeWrapArray(elements)

		private def writeReplace = new UniqueSerializer(elements)
	}


	private[this] final val SmallRankingCap = 16 //low in case we use it with objects with expensive equals/hashCode




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
			else throw new IndexOutOfBoundsException(s"$n/1")

		override def indexOf[U >: T](elem :U) = if (head == elem) 0 else -1

		override def +:[U >: T](elem :U) =
			if (elem == head) this else Ranking(elem, head)

		override def :+[U >: T](elem :U) =
			if (elem == head) this else Ranking(head, elem)

		override def +[U >: T](elem :U) =
			if (elem == head) this else Ranking(head, elem)

		override def ++:[U >: T](elems :Iterable[U]) = elems match {
			case empty :Iterable[_] if empty.isEmpty => this
			case _ => (newBuilder[U] ++= elems += head).result()
		}

		override def :++[U >: T](elems :IterableOnce[U]) = elems match {
			case empty :Iterable[_] if empty.isEmpty => this
			case unique :Ranking[U] => head +: unique
			case _ =>
				Ranking.from(new Iterator[U] {
					private[this] val it = elems.iterator
					private[this] var i = 0
					override def hasNext = it.hasNext
					override def next() = { i += 1; if (i == 1) SingletonRanking.this.head else it.next() }
				})
		}

		override def concat[B >: T](suffix :IterableOnce[B]) :Ranking[B] = suffix match {
			case unique :Ranking[B] => unique + head
			case _ => this :++ suffix
		}

		override def -[U >: T](elem :U) :Ranking[T] =
			if (elem == head) Ranking.empty[T] else this

		override def --[U >: T](elems :IterableOnce[U]) :Ranking[T] = elems match {
			case empty :Iterable[_] if empty.isEmpty => this
			case unique :Ranking[U] => if (unique.contains(head)) empty else this
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

		override def toString = "Unique(" + head + ")"

		private def writeReplace = new UniqueSerializer[Any](Array[Any](head))
	}




	@SerialVersionUID(ver)
	private class EmptyRanking extends Ranking[Nothing] {
		override def apply(n :Int) :Nothing = throw new IndexOutOfBoundsException("Unique()(" + n + ")")

		override def indexOf[U >: Nothing](elem :U) :Int = -1

		override def +:[U >: Nothing](elem :U) :Ranking[U] = new SingletonRanking(elem)
		override def :+[U >: Nothing](elem :U) :Ranking[U] = new SingletonRanking(elem)
		override def +[U >: Nothing](elem :U) :Ranking[U] = new SingletonRanking(elem)
		override def concat[U >: Nothing](elems :IterableOnce[U]) :Ranking[U] = from(elems)
		override def :++[U >: Nothing](elems :IterableOnce[U]) :Ranking[U] = from(elems)
		override def ++:[U >: Nothing](elems :Iterable[U]) :Ranking[U] = from(elems)
		override def -[U >: Nothing](elem :U) :Ranking[Nothing] = this
		override def --[U >: Nothing](elems :IterableOnce[U]) :Ranking[Nothing] = this

		override def iterator :Iterator[Nothing] = Iterator.empty
		override def reverse :Ranking[Nothing] = this
		override def reverseIterator :Iterator[Nothing] = Iterator.empty
		override def toSeq :Seq[Nothing] = Nil

		private def readResolve = EmptyRanking
	}

	private val EmptyRanking = new EmptyRanking






	@SerialVersionUID(ver)
	private class RankingSeqAdapter[+T](unique :Ranking[T]) extends AbstractSeq[T] with IndexedSeq[T] {
		override def length :Int = unique.size

		override def apply(idx :Int) :T = unique(idx)

		override def indexOf[U >: T](elem :U, start :Int) :Int = {
			val i = unique.indexOf(elem)
			if (i < start) -1 else i
		}

		override def lastIndexOf[U >: T](elem :U, end :Int) :Int = {
			val i = unique.indexOf(elem)
			if (i > end) -1 else i
		}

		override def contains[U >: T](elem :U) :Boolean = unique.contains(elem)

		override def iterator :Iterator[T] = unique.iterator

		override def foreach[U](f :T => U) :Unit = unique foreach f

		override def toSet[U >: T] :Set[U] = unique.toSet

		def toUnique :Ranking[T] = unique

		private def writeReplace = (unique :Ranking[Any]) to ArraySeq
	}



	@SerialVersionUID(ver)
	private class RankingSetAdapter[T](unique :Ranking[T]) extends AbstractSet[T] with Set[T] {
		override def size :Int = unique.size

		override def contains(elem :T) :Boolean = unique.contains(elem)

		override def incl(elem :T) :Set[T] = if (contains(elem)) this else Set(toSeq:_*)

		override def excl(elem :T) :Set[T] = if (contains(elem)) Set(toSeq:_*) - elem else this

		override def iterator :Iterator[T] = unique.iterator

		override def foreach[U](f :T => U) :Unit = unique foreach f

		override def toIndexedSeq :IndexedSeq[T] = unique.toIndexedSeq

		override def toSeq :Seq[T] = unique.toSeq

		def toUnique :Ranking[T] = unique

		private def writeReplace = unique to HashSet
	}



	@SerialVersionUID(ver)
	private class UniqueSerializer[+E](elems :Array[E]) extends Serializable {
		def this(elems :Ranking[E]) = this((elems :Ranking[Any]).to(Array).castParam[E])

		private def readResolve =
			if (elems.length <= SmallRankingCap) new SmallRanking(elems)
			else Ranking.from(ArraySeq.unsafeWrapArray(elems))
	}
}

