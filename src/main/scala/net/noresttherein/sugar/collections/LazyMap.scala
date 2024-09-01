package net.noresttherein.sugar.collections

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{BufferedIterator, MapFactory, MapFactoryDefaults, SortedMapFactory, SortedMapFactoryDefaults}
import scala.collection.immutable.{AbstractMap, MapOps, SortedMap, SortedMapOps}
import scala.collection.mutable.Builder

import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.exceptions.noSuch_!
import net.noresttherein.sugar.optional.extensions.OptionExtension
import net.noresttherein.sugar.vars.{AbstractDelayed, Delayed}




/** Lazy operations on ${coll}s.
  * @tparam K     $K
  * @tparam V     $V
  * @tparam C     $C
  * @tparam LMC   $LMC
  * @tparam LC    $LC
  * @define K     The type of keys in this map.
  * @define V     The type of values associated with the keys.
  * @define LMC   The two-argument type constructor for this lazy map type.
  *               Note that it is not the type passed as the type argument for type parameter `LCC` of `LazyIterableOps`,
  *               as they have different kinds.
  * @define Coll  `LazyMap`
  * @define coll  lazy map
  * @define Eager `Map`
  */
trait LazyMapOps[K, +V, +C <: Map[K, V] with MapOps[K, V, Map, C],
                 +LMC[k, v] <: LazyMap[k, v], +LC <: LazyMapOps[K, V, C, LMC, LC]]
	extends MapOps[K, V, LazyMap, LC] with LazyIterableOps[(K, V), Iterable, C, LazyIterable, LC]
{
	protected override def lazyFactory :DelayedFactory[Iterable, LazyIterable] = LazyIterable
	protected def lazyGenericMap[X](items: => Map[K, X] @uncheckedVariance) :LMC[K, X]

	override def get(key :K) :Option[V] = definite.get(key)

	override def updated[U >: V](key :K, value :U) :LMC[K, U] = lazyGenericMap(strict.updated(key, value))
	def updated[U >: V](entry: () => (K, U)) :LMC[K, U] = added(entry())
//	def updated[V1 >: V](key :K, value: () => V1) :LMC[K, V1] = lazyUpdated(key, value())

	@inline final def +[U >: V](entry: () => (K, U)) :LMC[K, U] = updated(entry)

	def added[U >: V](key: K, value: => U) :LMC[K, U] = lazyGenericMap(strict.updated(key, value))
	def added[U >: V](entry: => (K, U)) :LMC[K, U] = lazyGenericMap(strict + entry)
	def addedAll[U >: V](suffix: => IterableOnce[(K, U)]) :LMC[K, U] = lazyGenericMap(strict concat suffix)

	@inline final def +#[U >: V](entry: => (K, U)) :LMC[K, U] = added(entry)
	@inline final def +#[U >: V](entry :(K, () => U)) :LMC[K, U] = added(entry._1, entry._2())
	@inline final def ++#[U >: V](suffix: => IterableOnce[(K, U)]) :LMC[K, U] = addedAll(suffix)

	override def removed(key :K) :LC = lazySpecific(strict.removed(key))
	override def removedAll(keys :IterableOnce[K]) :LC = lazySpecific(strict.removedAll(keys))

	def lazyRemoved(key: => K) :LC = lazySpecific(strict.removed(key))
	def lazyRemovedAll(keys: => IterableOnce[K]) :LC = lazySpecific(strict.removedAll(keys))

	@inline final def -#(key: => K) :LC = lazyRemoved(key)
	@inline final def --#(keys: => IterableOnce[K]) :LC = lazyRemovedAll(keys)

	def removed(key: () => K) :LC = lazyRemoved(key())
	def removedAll(keys: () => IterableOnce[K]) :LC = lazyRemovedAll(keys())

	@inline final def -(key: () => K) :LC = removed(key)
	@inline final def --(keys: () => IterableOnce[K]) :LC = removedAll(keys)

	override def concat[U >: V](suffix :IterableOnce[(K, U)]) :LMC[K, U] = lazyGenericMap(strict concat suffix)

	def concat[U >: V](suffix: () => IterableOnce[(K, U)]) :LMC[K, U] = addedAll(suffix())

//	@inline final def +[U >: V](entry: () => (K, U)) :LMC[K, U] = updated(entry)
	@inline final def ++[U >: V](suffix: () => IterableOnce[(K, U)]) :LMC[K, U] = concat(suffix)


	override def updatedWith[U >: V](key :K)(remappingFunction :Option[V] => Option[U]) :LMC[K, U] =
		lazyGenericMap(definite.updatedWith[U](key)(remappingFunction))

	override def transform[W](f :(K, V) => W) :LMC[K, W] = lazyGenericMap(strict.transform(f))

	override def keySet :LazySet[K] = LazySet.from(() => strict.keySet)
//	override def keys   :LazyIterable[K] = LazyIterable.factory.from(() => definite.keys)
	override def values :LazyIterable[V] = LazyIterable.from(() => strict.values)

//	@inline final def ||[V1 >: V](that: => IterableOnce[(K, V1)]) :CC[K, V] = addedAll(that)
//	@inline final def &&(that: => collection.Set[E]) :C = keep(that)

	override def foreachEntry[U](f :(K, V) => U) :Unit = definite.foreachEntry(f)

	override def empty :LC = lazySpecific((strict :MapOps[K, V, Map, C]).empty)

	override def keysIterator :Iterator[K] =
		if (initializer == null) definite.keysIterator else new LazyIterator(definite.keysIterator)

	override def valuesIterator :Iterator[V] =
		if (initializer == null) definite.valuesIterator else new LazyIterator(definite.valuesIterator)
}




/** $lazyCollectionInfo
  * @tparam K     $K
  * @tparam V     $V
  * @define Coll  `LazyMap`
  * @define coll  lazy map
  * @define Eager `Map`
  */
sealed abstract class LazyMap[K, +V] protected
	extends AbstractMap[K, V] with LazyIterable[(K, V)] with LazyMapOps[K, V, Map[K, V], LazyMap, LazyMap[K, V]]
	   with MapFactoryDefaults[K, V, LazyMap, LazyIterable]
{
	protected override def lazyGenericMap[X](items: => Map[K, X]) :LazyMap[K, X] = LazyMap.from(() => items)
	protected override def lazySpecific(items : => Map[K, V @uncheckedVariance]) :LazyMap[K, V] =
		LazyMap.from(() => items)

	override def mapFactory :MapFactory[LazyMap] = LazyMap.factory
	protected[this] override def className :String = "LazyMap"
}


/** A factory wrapping lazy expressions of `Map` type into lazy maps.
  * @define Coll  `LazyMap`
  * @define coll  lazy map
  * @define Eager `Map`
  */
@SerialVersionUID(Ver)
case object LazyMap {
	@inline def apply[K, V](map: => Map[K, V]) :LazyMap[K, V] = from(() => map)
	def from[K, V](map: () => Map[K, V]) :LazyMap[K, V] = new Delayed[K, V](map)

	/** $factoryInfo
	  * @define Coll `LazyMap`
	  * @define coll lazy map
	  */
	@SerialVersionUID(Ver)
	object factory extends MapFactory[LazyMap] {
		override def from[K, V](it :IterableOnce[(K, V)]) :LazyMap[K, V] = new Delayed(() => Map.from(it))

		override def empty[K, V] :LazyMap[K, V] = new Delayed(() => Map.empty[K, V])

		override def newBuilder[K, V] :Builder[(K, V), LazyMap[K, V]] =
			Map.newBuilder[K, V].mapResult(map => new Delayed(() => map))

		override def toString = "LazyMap.factory"
	}

	@SerialVersionUID(Ver)
	private class Delayed[K, V](protected[this] override var initializer :() => Map[K, V])
		extends LazyMap[K, V] with Serializable
	{
		override def contains(key :K) :Boolean = definite.contains(key)
		override def getOrElse[U >: V](key :K, default : => U) :U = definite.getOrElse(key, default)
		override def apply(key :K) :V = definite(key)
	}
}




/** A lazy map consisting of an evaluated part in `contents._1` and an unevaluated list of elements to add.
  * @note The list is a LIFO, so the elements in the evaluated part take precedence over those in the unevaluated part,
  *       and the earlier list elements over those following them.
  */ //consider: using LazyLinearSeq[(K, Lazy[V])) instead
@SerialVersionUID(Ver)
private class PartiallyLazyMap[K, V](@volatile private[this] var contents :(Map[K, V], LazyLinearSeq[(K, V)]))
	extends LazyMap[K, V] with Serializable
{
	def this(contents :LazyLinearSeq[(K, V)]) = this((Map.empty[K, V], contents))

	protected[this] override var initializer = () => moveAll()

	protected def copy[U >: V](prefix :Map[K, U], suffix :LazyLinearSeq[(K, U)]) :LazyMap[K, U] =
		new PartiallyLazyMap[K, U]((prefix, suffix))

	private def moveAll() :Map[K, V] = {
		var (prefix, suffix) = contents
		while (suffix.nonEmpty) {
			val (k, v) = suffix.head
			suffix = suffix.tail
			if (!prefix.contains(k))
				prefix = prefix.updated(k, v)
		}
		contents = (prefix, suffix)
		prefix
	}
	override def knownSize :Int = {
		val (prefix, suffix) = contents
		val size = prefix.knownSize
		if (size >= 0 && suffix.knownSize == 0) size else -1
	}
	override def size :Int = moveAll().size

	override def get(key :K) :Option[V] = {
		var (prefix, suffix) = contents
		prefix.get(key) match {
			case None =>
				var res :Option[V] = None
				while (suffix.nonEmpty && {
					val (k, v) = suffix.head
					suffix = suffix.tail
					if (!prefix.contains(k))
						prefix = prefix.updated(k, v)
					if (k == key)
						res = Some(v)
					res.isEmpty
				}) {}
				res
			case some => some
		}
	}

	override def contains(key :K) :Boolean = get(key).isDefined

	override def updated[U >: V](key :K, value :U) :LazyMap[K, U] = {
		val (prefix, suffix) = contents
		copy(prefix.updated(key, value), suffix)
	}
	override def +[U >: V](entry :(K, U)) :LazyMap[K, U] = {
		val (prefix, suffix) = contents
		copy(prefix + entry, suffix)
	}
	override def added[U >: V](key :K, value : => U) :LazyMap[K, U] = {
		val (prefix, suffix) = contents
		copy(prefix, (key, value) #+: suffix)
	}
	override def added[U >: V](entry: => (K, U)) :LazyMap[K, U] = {
		val (prefix, suffix) = contents
		copy(prefix, entry #+: suffix)
	}
	override def addedAll[U >: V](suffix : => IterableOnce[(K, U)]) :LazyMap[K, U] = {
		val (map, list) = contents
		copy(map, suffix #++: list)
	}

	override def concat[U >: V](that: IterableOnce[(K, U)]) :LazyMap[K, U] = {
		val (prefix, suffix) = contents
		if (suffix.isEmpty) copy(prefix, LazyLinearSeq.factory.from(util.reverse(that)))
		else copy(prefix, LazyLinearSeq.from(() => that reverse_#++: suffix))
	}

	override def removed(key :K) :LazyMap[K, V] = {
		val (prefix, suffix) = contents
		copy(prefix - key, suffix.filter(_._1 != key))
	}

	override def keySet :LazySet[K] = {
		val (prefix, suffix) = contents
		new PartiallyLazySet(prefix.keySet, suffix.map(_._1))
	}
	override def values :LazyIterable[V] = LazyIterable.factory.from(valuesIterator)

	override def foreach[U](f :((K, V)) => U) :Unit = moveAll().foreach(f)
	override def foreachEntry[U](f :(K, V) => U) :Unit = moveAll().foreachEntry(f)

	override def iterator :Iterator[(K, V)] = new AbstractIterator[(K, V)]("iterator") {
		override def head = headEntry
		override def next() = nextEntry()
	}
	override def keysIterator :Iterator[K] = new AbstractIterator[K]("keysIterator") {
		override def head = headEntry._1
		override def next() = nextEntry()._1
	}
	override def valuesIterator :Iterator[V] = new AbstractIterator[V]("valuesIterator") {
		override def head = headEntry._2
		override def next() = nextEntry()._2
	}

	private abstract class AbstractIterator[+E](name :String) extends BufferedIterator[E] {
		private[this] var (prefix, suffix) = contents
		private[this] var entry :(K, V) = null
		private[this] var prefixItr = if (prefix.isEmpty) null else prefix.iterator

		override def hasNext :Boolean = entry != null || {
			if (prefixItr != null) {
				entry = prefixItr.next()
				if (!prefixItr.hasNext)
					prefixItr = null
			} else {
				while (entry == null && suffix.nonEmpty) {
					entry = suffix.head
					if (prefix.contains(entry._1))
						entry = null
					else
						prefix = prefix + entry
				}
				if (suffix.isEmpty)
					contents = (prefix, suffix)
			}
			entry != null
		}

		protected def headEntry :(K, V) = if (hasNext) entry else noSuch_!(toString + ".next()")

		protected def nextEntry() :(K, V) =
			if (!hasNext)
				noSuch_!(toString + ".next()")
			else {
				val res = entry
				entry = null
				res
			}
		override def toString =
			errorString(PartiallyLazyMap.this) + '.' + name + (if (entry != null) "(" + entry + ", ...)" else "")
	}

	override def toString :String = {
		val (prefix, suffix) = contents
		if (suffix.knownSize == 0) prefix.mkString(className + "(", ", ", ")")
		else prefix.mkString(className + "(", ", ", ", ...)")
	}

}




@SerialVersionUID(Ver)
private class LazyValueMap[K, +V](eager :Map[K, V], lzy :Map[K, Delayed[V]]) extends LazyMap[K, V] {
	protected[this] override var initializer = () => eager ++ lzy.view.mapValues(_.get)

	private def copy[U >: V](eager :Map[K, U], lzy :Map[K, Delayed[U]]) =
		if ((eager eq this.eager) && (lzy eq this.lzy)) this else new LazyValueMap(eager, lzy)

	override def knownSize :Int = {
		val s1 = eager.knownSize
		val s2 = lzy.knownSize
		if (s1 >= 0 & s2 >= 0) s1 + s2 else -1
	}
	override def size :Int = eager.size + lzy.size

	override def contains(key :K) = eager.contains(key) || lzy.contains(key)

	override def getOrElse[U >: V](key :K, default : => U) = get(key) match {
		case Some(v) => v
		case _       => default
	}
	override def get(key :K) = eager.get(key) match {
		case None => lzy.get(key).map(_.get)
		case some => some
	}
	override def apply(key :K) = eager.get(key) match {
		case Some(v) => v
		case _       => lzy.get(key).map(_.get).orNoSuch("No key " + key + ".")
	}

	override def updated[U >: V](key :K, value :U) = new LazyValueMap(eager.updated(key, value), lzy - key)
	override def added[U >: V](key :K, value : => U) = copy(eager - key, lzy.updated(key, Delayed(value)))

	override def removed(key :K) = copy(eager.removed(key), lzy.removed(key))
	override def removedAll(keys :IterableOnce[K]) = copy(eager.removedAll(keys), lzy.removedAll(keys))
	override def concat[U >: V](suffix :IterableOnce[(K, U)]) =
		copy(eager concat suffix, lzy removedAll suffix.iterator.map(_._1))


	override def keySet = LazySet.from(() => eager.keySet | lzy.keySet)
	override def values = LazyIterable.from(() => eager.values.toSeq :++ lzy.values.iterator.map(_.get))

	override def foreachEntry[U](f :(K, V) => U) :Unit = {
		eager.foreachEntry(f)
		lzy.foreachEntry { case (k, v) => f(k, v.get) }
	}

	override def keysIterator = eager.keysIterator ++ lzy.keysIterator
	override def valuesIterator = eager.valuesIterator ++ lzy.valuesIterator.map(_.get)
	override def iterator = eager.iterator ++ lzy.iterator.map { case (k, v) => (k, v.get) }

	override def toString :String = {
		val res = new StringBuilder
		res ++= className += '('
		eager.addString(res, ", ")
		val suffix = lzy.iterator.map {
			case (key, value) => "(" + key + ", " + value.opt.mapOrElse(_.toString, "?") + ")"
		}
		if (eager.nonEmpty && lzy.nonEmpty)
			res ++= ", "
		suffix.addString(res)
		res += ')'
		res.result()
	}
}





/** $lazyCollectionInfo
  * @tparam K     $K
  * @tparam V     $V
  * @define Coll  `LazySortedMap`
  * @define coll  lazy sorted map
  * @define Eager `SortedMap`
  */
sealed abstract class LazySortedMap[K, +V]
	extends LazyMap[K, V] with SortedMap[K, V] with SortedMapOps[K, V, LazySortedMap, LazySortedMap[K, V]]
	   with AbstractDelayed[SortedMap[K, V]] with LazyMapOps[K, V, SortedMap[K, V], LazySortedMap, LazySortedMap[K, V]]
	   with LazySortedOps[K, LazySortedMap[K, V]]
	   with SortedMapFactoryDefaults[K, V, LazySortedMap, LazyIterable, LazyMap]
{
	override def sortedMapFactory :SortedMapFactory[LazySortedMap] = LazySortedMap.factory

	protected override def lazyGenericMap[X](items: => Map[K, X]) :LazySortedMap[K, X] =
		if (isOrderingStrict)
			//Use this.ordering for conversion to SortedMap because if items :SortedMap[K, X],
			// then most likely items.ordering eq this.ordering
			LazySortedMap.reorder.from(() => SortedMap.from(items)(ordering))(ordering)
		else //this.ordering may change on evaluation of initializer, so there is no sense in preserving it.
			LazySortedMap.from(() => SortedMap.from(items)(ordering))

	protected override def lazySpecific(items : => SortedMap[K, V @uncheckedVariance]) :LazySortedMap[K, V] =
		if (isOrderingStrict)
			LazySortedMap.reorder.from(() => items)(ordering)
		else
			LazySortedMap.from(() => items)

	override def rangeImpl(from :Option[K], until :Option[K]) :LazySortedMap[K, V] =
		lazySpecific(strict.rangeImpl(from, until))

	override def keySet :LazySortedSet[K] =
		if (isOrderingStrict)
			LazySortedSet.reorder.from(() => strict.keySet)(ordering)
		else
			LazySortedSet.from(() => strict.keySet)
//		LazySortedSet.factory.asInstanceOf[LazySortedSetFactory].using(() => strict.keySet)(ordering)

	override def iteratorFrom(start :K) :Iterator[(K, V)] =
		if (isDefinite) definite.iteratorFrom(start) else new LazyIterator(strict.iteratorFrom(start))

	override def keysIteratorFrom(start :K) :Iterator[K] =
		if (isDefinite) definite.keysIteratorFrom(start)
		else new LazyIterator(strict.keysIteratorFrom(start))

	protected[this] override def className :String = "LazySortedMap"
}


/** A factory wrapping lazy expressions of `SortedMap` type into lazy sorted maps.
  * @define Coll  `LazySortedMap`
  * @define coll  lazy sorted map
  * @define Eager `SortedMap`
  */
@SerialVersionUID(Ver)
case object LazySortedMap {
	@inline def apply[K, V](map: => SortedMap[K, V]) :LazySortedMap[K, V] = from(() => map)

	def from[K, V](map: () => SortedMap[K, V]) :LazySortedMap[K, V] = new Derived(map)

	def from[K :Ordering, V](elems: () => IterableOnce[(K, V)]) :LazySortedMap[K, V] = apply(elems())
	def apply[K :Ordering, V](elems: => IterableOnce[(K, V)]) :LazySortedMap[K, V] =
		new Delayed(() => SortedMap.from(elems))

	def empty[K :Ordering, V] :LazySortedMap[K, V] = new Delayed(() => SortedMap.empty)

	/** $factoryInfo
	  * @define Coll `LazySortedMap`
	  * @define coll lazy sorted map
	  */
	@SerialVersionUID(Ver)
	object factory extends SortedMapFactory[LazySortedMap] {
		override def from[K :Ordering, V](it :IterableOnce[(K, V)]) :LazySortedMap[K, V] =
			new Derived(() => SortedMap.from(it))

		override def empty[K :Ordering, V] :LazySortedMap[K, V] = new Derived(() => SortedMap.empty)

		override def newBuilder[K :Ordering, V] :Builder[(K, V), LazySortedMap[K, V]] =
			SortedMap.newBuilder[K, V].mapResult(map => from(map))

		override def toString = "LazySortedMap.factory"
	}

	/** A factory constructing lazy sorted maps by reordering other sorted maps according to an implicit ordering. */
	@SerialVersionUID(Ver)
	object reorder {
		@inline def apply[K :Ordering, V](items: => SortedMap[K, V]) :LazySortedMap[K, V] = from(() => items)

		def from[K :Ordering, V](items: () => SortedMap[K, V]) :LazySortedMap[K, V] = new Delayed(items)
	}

	@SerialVersionUID(Ver)
	private class Derived[K, V](protected[this] override var initializer :() => SortedMap[K, V])
		extends LazySortedMap[K, V] with Serializable
	{
		override def ordering = if (initializer == null) definite.ordering else new LazyOrdering(() => strictOrdering)
	}

	@SerialVersionUID(Ver)
	private class Delayed[K, V](protected[this] override var initializer :() => SortedMap[K, V])
		                       (implicit override val ordering :Ordering[K])
		extends LazySortedMap[K, V] with Serializable
	{
		override def isOrderingStrict = true
	}
}




@SerialVersionUID(Ver)
private class SortedLazyValueMap[K, +V](underlying :SortedMap[K, Delayed[V]])
	extends LazySortedMap[K, V] with Serializable
{
	protected[this] override var initializer = () => SortedMap.from(underlying.view.mapValues(_.get))

	private def copy[V1 >: V](map :SortedMap[K, Delayed[V1]]) :LazySortedMap[K, V1] =
		if (map eq underlying) this else new SortedLazyValueMap(map)

	implicit override def ordering :Ordering[K] = underlying.ordering

	override def knownSize :Int = underlying.knownSize
	override def size :Int = underlying.size

	override def contains(key :K) = underlying.contains(key)

	override def getOrElse[U >: V](key :K, default : => U) = get(key) match {
		case Some(v) => v
		case _       => default
	}
	override def get(key :K) = underlying.get(key).map(_.get)
	override def apply(key :K) = underlying(key).get

	override def added[U >: V](key :K, value : => U) = copy(underlying.updated(key, Delayed(value)))
	override def updated[U >: V](key :K, value :U) = copy(underlying.updated(key, Delayed.eager(value)))

	override def removed(key :K) = copy(underlying.removed(key))
	override def removedAll(keys :IterableOnce[K]) = copy(underlying.removedAll(keys))
	override def concat[U >: V](suffix :IterableOnce[(K, U)]) =
		copy(underlying concat suffix.iterator.map { kv => (kv._1, Delayed.eager(kv._2)) })


	override def keySet = LazySortedSet.factory.from(underlying.keySet)
	override def values = LazySeq.factory.from(underlying.values.iterator.map(_.get))

	override def foreachEntry[U](f :(K, V) => U) :Unit =
		underlying.foreachEntry { (k, v) => f(k, v.get) }

	override def keysIterator = underlying.keysIterator
	override def valuesIterator = underlying.valuesIterator.map(_.get)
	override def iterator = underlying.iterator.map { kv => (kv._1, kv._2.get) }

	override def toString :String =
		underlying.iterator.map {
			case (k, v) => "(" + k + v.maybe.mapOrElse(", " + _ + ")", ", ?")
		}.mkString(className + "(", ", ", ")")
}
