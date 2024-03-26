package net.noresttherein.sugar.collections

import java.io.ObjectOutputStream

import scala.collection.{AbstractIterator, IterableFactory, IterableFactoryDefaults, IterableOps, MapFactory, MapFactoryDefaults, SortedIterableFactory, SortedMapFactory, SortedMapFactoryDefaults, SortedSetFactoryDefaults, Stepper, StepperShape, immutable}
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable.{AbstractMap, AbstractSet, MapOps, SetOps, SortedMap, SortedMapOps, SortedSet, SortedSetOps}
import scala.collection.mutable.Builder

import net.noresttherein.sugar.extensions.SeqFactoryExtension
import net.noresttherein.sugar.vars.AbstractLazy





private final class LazyIterator[+T] private[collections] (lzy: => Iterator[T]) extends AbstractIterator[T] {
	private[this] lazy val evaluated = lzy
	override def hasNext :Boolean = evaluated.hasNext
	override def next() :T = evaluated.next()
}

object LazyIterator {
	def apply[A](iterator: => Iterator[A]) :Iterator[A] = new LazyIterator(iterator)
}



trait LazyIterableOps[+E, +CC[X] <: IterableOps[X, CC, _], +C <: IterableOps[E, CC, C],
                      +LCC[X] <: IterableOps[X, LCC, _], +LC <: IterableOps[E, LCC, LC]]
	extends AbstractLazy[C] with IterableOps[E, LCC, LC] with Serializable
{
	protected def lazySpecific(items: => C @uncheckedVariance) :LC
	protected def lazyGeneric[O](items: => CC[O] @uncheckedVariance) :LCC[O]

	override def knownSize :Int = if (initializer == null) definite.knownSize else -1
	override def size :Int = definite.size

	@inline final def isEvaluated :Boolean = isDefinite
	override def isEmpty :Boolean = definite.isEmpty

	def added[U >: E](elem: => U) :LCC[U] = lazyGeneric(definite.concat(Seq.one(elem)))
	@inline final def +~[U >: E](elem: => U) :LCC[U] = added(elem)

	def addedAll[U >: E](elems: => IterableOnce[U]) :LCC[U] = lazyGeneric(definite.concat(elems))
	@inline final def ++~[U >: E](elems: => IterableOnce[U]) :LCC[U] = addedAll(elems)

	override def concat[O >: E](suffix :IterableOnce[O]) :LCC[O] = lazyGeneric(definite.concat(suffix))

	override def headOption :Option[E] = definite.headOption
	override def lastOption :Option[E] = definite.lastOption
	override def head :E = definite.head
	override def last :E = definite.last
	override def tail :LC = lazySpecific(definite.tail)
	override def init :LC = lazySpecific(definite.init)
	//take,drop,slice, etc.
	//filter, filterNot, partition, etc

	override def transpose[O](implicit asIterable :E => Iterable[O]) :LCC[LCC[O] @uncheckedVariance] =
		lazyGeneric(definite.transpose[O].map(lazyGeneric(_)))

	override def groupBy[K](f :E => K) :LazyMap[K, LC] =
		LazyMap.factory.from(definite.groupBy(f).view.mapValues(lazySpecific(_)))

	override def groupMap[K, O](key :E => K)(f :E => O) :LazyMap[K, LCC[O]] =
		LazyMap.factory.from(definite.groupMap(key)(f).view.mapValues(lazyGeneric(_)))

	override def groupMapReduce[K, B](key :E => K)(f :E => B)(reduce :(B, B) => B) :LazyMap[K, B] =
		LazyMap.factory.from(() => definite.groupMapReduce(key)(f)(reduce))

	override def scanRight[O](z :O)(op :(E, O) => O) :LCC[O] = lazyGeneric(definite.scanRight(z)(op))

	//map, flatMap, flatten, collect, partitionMap
	//zip, zipAll, etc.

	override def empty :LC = lazySpecific(definite.empty)

	override def iterator :Iterator[E] =
		if (initializer == null) definite.iterator else new LazyIterator(definite.iterator)

	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S =
		if (initializer == null) definite.stepper else LazyStepper(definite.stepper)

	def strict :C = definite

//	protected def writeReplace :Any = definite
}



trait LazyIterable[+E]
	extends IterableProxy[E] with immutable.Iterable[E]
	   with LazyIterableOps[E, Iterable, Iterable[E], LazyIterable, LazyIterable[E]]
	   with IterableFactoryDefaults[E, LazyIterable]
{
	protected override def underlying :Iterable[E] = definite
	override def iterableFactory :IterableFactory[LazyIterable] = LazyIterable.factory
}

@SerialVersionUID(Ver)
case object LazyIterable {
	def apply[E](items: => Iterable[E]) :LazyIterable[E] = new Impl(() => items)

	@SerialVersionUID(Ver)
	object factory extends IterableFactory[LazyIterable] {
		def from[E](items: () => Iterable[E]) :LazyIterable[E] = new Impl(items)

		override def from[E](source :IterableOnce[E]) :LazyIterable[E] = new Impl(() => LazyList.from(source))

		override def empty[E] :LazyIterable[E] = new Impl(() => LazyList.empty)

		override def newBuilder[E] :Builder[E, LazyIterable[E]] = LazyList.newBuilder[E].mapResult(from(_))

		override def toString = "LazyIterable.factory"
	}
	private class Impl[E](protected[this] override var initializer :() => Iterable[E]) extends LazyIterable[E] {
		protected override def lazySpecific(items : => Iterable[E]) :LazyIterable[E] = new Impl(() => items)
		protected override def lazyGeneric[T](items : => Iterable[T]) :LazyIterable[T] = new Impl(() => items)
	}
}






sealed trait LazySetOps[E, +C <: Set[E] with SetOps[E, Set, C], +LC <: Set[E] with SetOps[E, LazySet, LC]]
	extends SetOps[E, LazySet, LC] with LazyIterableOps[E, Set, C, LazySet, LC]
{
	protected[this] override def lazyGeneric[T](items : => Set[T]) :LazySet[T] = LazySet.factory.from(() => items)
	protected override def definite :C = super.definite match {
		case lzy :LazySetOps[E, C, LC] @unchecked => lzy.definite
		case strict                               => strict
	}

	override def contains(elem :E) :Boolean = definite.contains(elem)
	override def excl(elem :E) :LC = lazySpecific(definite.excl(elem))
	override def incl(elem :E) :LC = lazySpecific(definite.incl(elem))

	override def intersect(that :collection.Set[E]) :LC = lazySpecific(definite.intersect(that))
	override def diff(that :collection.Set[E]) :LC = lazySpecific(definite.diff(that))
	override def removedAll(that :IterableOnce[E]) :LC = lazySpecific(definite.removedAll(that))

	def lazyIncl(elem: => E) :LC = lazySpecific(definite.incl(elem))
	def lazyExcl(elem: => E) :LC = lazySpecific(definite.excl(elem)) //consider: other names. plus/minus, without

	@inline final def +~(elem: => E) :LC = lazyIncl(elem)
	@inline final def -~(elem: => E) :LC = lazyExcl(elem)

	def lazyDiff(that: => collection.Set[E]) :LC = lazySpecific(definite.diff(that))
	def lazyUnion(that: => collection.Set[E]) :LC = lazySpecific(definite.concat(that))
	def lazyIntersect(that: => collection.Set[E]) :LC = lazySpecific(definite.intersect(that))

	@inline final def &&~(that: => collection.Set[E]) :LC = lazyDiff(that)
	@inline final def ||(that: => collection.Set[E]) :LC = lazyUnion(that)
	@inline final def &&(that: => collection.Set[E]) :LC = lazyIntersect(that)

	def lazyConcat(that: => IterableOnce[E]) :LC = lazySpecific(definite.concat(that))
	def lazyRemovedAll(that: => IterableOnce[E]) :LC = lazySpecific(definite.removedAll(that))

	@inline final def ++~(that: => IterableOnce[E]) :LC = lazyConcat(that)
	@inline final def --~(that: => IterableOnce[E]) :LC = lazyRemovedAll(that)
}



@SerialVersionUID(Ver)
sealed abstract class LazySet[E] protected
	extends AbstractSet[E] with LazyIterable[E] with LazySetOps[E, Set[E], LazySet[E]]
	   with IterableFactoryDefaults[E, LazySet] with Serializable
{
	protected override def lazySpecific(items : => Set[E]) :LazySet[E] = LazySet.factory.from(() => items)
	override def iterableFactory :IterableFactory[LazySet] = LazySet.factory
}

@SerialVersionUID(Ver)
case object LazySet {
	def apply[E](set: => Set[E]) :LazySet[E] = new Impl(() => set)

	def empty[A] :LazySet[A] = Empty.asInstanceOf[LazySet[A]]

	private[this] val Empty = new Impl[Nothing](() => Set.empty[Nothing])

	@SerialVersionUID(Ver)
	object factory extends IterableFactory[LazySet] {
		def from[E](set: () => Set[E]) :LazySet[E] = new Impl(set)

		override def from[E](it :IterableOnce[E]) :LazySet[E] = it match {
			case lzy :LazySet[E @unchecked] => lzy
			case _                          => new Impl(() => Set.from(it))
		}

		override def empty[E] :LazySet[E] = LazySet.empty

		override def newBuilder[A] :Builder[A, LazySet[A]] = Set.newBuilder[A].mapResult(set => new Impl(() => set))

		override def toString = "LazySet.factory"
	}

	private class Impl[E](protected[this] override var initializer :() => Set[E]) extends LazySet[E]
}



@SerialVersionUID(Ver) //todo: make it extend LazySet. The problem, of course, is overriding of a var.
final class LazySortedSet[E] private (protected override var initializer: () => SortedSet[E])
                                     (implicit override val ordering :Ordering[E])
	extends LazySet[E] with SortedSet[E] with SortedSetOps[E, LazySortedSet, LazySortedSet[E]]
	   with AbstractLazy[SortedSet[E]] with LazySetOps[E, SortedSet[E], LazySortedSet[E]]
	   with SortedSetFactoryDefaults[E, LazySortedSet, LazySet] with Serializable
{
	protected override def lazySpecific(items : => SortedSet[E]) :LazySortedSet[E] =
		new LazySortedSet(() => items)

	override def iteratorFrom(start :E) :Iterator[E] =
		if (initializer == null) definite.iteratorFrom(start) else new LazyIterator(definite.iteratorFrom(start))

	override def rangeImpl(from :Option[E], until :Option[E]) :LazySortedSet[E] =
		new LazySortedSet(() => definite.rangeImpl(from, until))

	override def sortedIterableFactory :SortedIterableFactory[LazySortedSet] = LazySortedSet.factory

}

@SerialVersionUID(Ver)
case object LazySortedSet {
	def apply[E](set: => SortedSet[E]) :LazySortedSet[E] =
		new LazySortedSet[E](() => set)(new LazyOrdering(() => set.ordering))

	def empty[E :Ordering] :LazySortedSet[E] = factory.empty

	@SerialVersionUID(Ver)
	object factory extends SortedIterableFactory[LazySortedSet] {
		def from[E](set: () => SortedSet[E]) :LazySortedSet[E] =
			new LazySortedSet[E](set)(new LazyOrdering[E](() => set().ordering))

		override def from[E :Ordering](it :IterableOnce[E]) :LazySortedSet[E] = it match {
			case ss :SortedSet[E @unchecked] if ss.ordering == Ordering[E] => new LazySortedSet(() => ss)
			case _                                                         => new LazySortedSet(() => SortedSet.from(it))
		}

		override def empty[E :Ordering] :LazySortedSet[E] = new LazySortedSet(() => SortedSet.empty)
		override def newBuilder[E :Ordering] :Builder[E, LazySortedSet[E]] =
			SortedSet.newBuilder[E].mapResult(set => new LazySortedSet(() => set))

		override def toString = "LazySortedSet.factory"
	}
}






@SerialVersionUID(Ver) //todo: an implementation backed by Map[K, Lazy[V]]
trait LazyMapOps[K, +V, +LCC[k, v] <: LazyMap[k, v] with LazyMapOps[k, v, LCC, _], +LC <: LazyMapOps[K, V, LCC, LC]]
	extends MapOps[K, V, LazyMap, LC] with LazyIterableOps[(K, V), Iterable, Map[K, V], LazyIterable, LC]
{
	protected override def lazyGeneric[T](items : => Iterable[T]) :LazyIterable[T] =
		LazyIterable.factory.from(() => items)

	protected def lazyGenericMap[X](items: => Map[K, X] @uncheckedVariance) :LCC[K, X]

	protected override def definite :Map[K, V] = super.definite match {
		case lzy :LazyMap[K, V] => lzy.definite
		case strict             => strict
	}

	override def contains(key :K) :Boolean = definite.contains(key)
	override def getOrElse[V1 >: V](key :K, default : => V1) :V1 = definite.getOrElse(key, default)
	override def get(key :K) :Option[V] = definite.get(key)
	override def apply(key :K) :V = definite(key)

	override def updated[V1 >: V](key :K, value :V1) :LCC[K, V1] = lazyGenericMap(definite.updated(key, value))

	override def removed(key :K) :LC = lazySpecific(definite.removed(key))
	override def removedAll(keys :IterableOnce[K]) :LC = lazySpecific(definite.removedAll(keys))

	def lazyRemoved(key: => K) :LC = lazySpecific(definite.removed(key))
	def lazyRemovedAll(keys: => IterableOnce[K]) :LC = lazySpecific(definite.removedAll(keys))

	@inline final def -~(key: => K) :LC = lazyRemoved(key)
	@inline final def --~(keys: => IterableOnce[K]) :LC = lazyRemovedAll(keys)

	override def concat[V2 >: V](suffix :IterableOnce[(K, V2)]) :LCC[K, V2] = lazyGenericMap(definite concat suffix)

	def added[V1 >: V](key: K, value: => V1) :LCC[K, V1] = lazyGenericMap(definite.updated(key, value))
	def added[V1 >: V](entry: => (K, V1)) :LCC[K, V1] = lazyGenericMap(definite + entry)
	def addedAll[V1 >: V](suffix: => IterableOnce[(K, V1)]) :LCC[K, V1] = lazyGenericMap(definite concat suffix)

	@inline final def +~[V1 >: V](entry: => (K, V1)) :LCC[K, V1] = added(entry)
	@inline final def ++~[V1 >: V](suffix: => IterableOnce[(K, V1)]) :LCC[K, V1] = addedAll(suffix)

	override def updatedWith[V1 >: V](key :K)(remappingFunction :Option[V] => Option[V1]) :LCC[K, V1] =
		lazyGenericMap(definite.updatedWith[V1](key)(remappingFunction))

	override def transform[W](f :(K, V) => W) :LCC[K, W] = lazyGenericMap(definite.transform(f))

	override def keySet :LazySet[K] = LazySet.factory.from(() => definite.keySet)
	override def keys   :LazyIterable[K] = LazyIterable.factory.from(() => definite.keys)
	override def values :LazyIterable[V] = LazyIterable.factory.from(() => definite.values)

//	@inline final def ||[V1 >: V](that: => IterableOnce[(K, V1)]) :CC[K, V] = addedAll(that)
//	@inline final def &&(that: => collection.Set[E]) :C = keep(that)

	override def foreachEntry[U](f :(K, V) => U) :Unit = definite.foreachEntry(f)

	override def empty :LC = lazySpecific(definite.empty)

	override def keysIterator :Iterator[K] =
		if (initializer == null) definite.keysIterator else new LazyIterator(definite.keysIterator)

	override def valuesIterator :Iterator[V] =
		if (initializer == null) definite.valuesIterator else new LazyIterator(definite.valuesIterator)
}



@SerialVersionUID(Ver)
sealed abstract class LazyMap[K, +V] protected
	extends AbstractMap[K, V] with LazyIterable[(K, V)] with LazyMapOps[K, V, LazyMap, LazyMap[K, V]]
	   with MapFactoryDefaults[K, V, LazyMap, LazyIterable]
{
	protected override def underlying :Iterable[(K, V)] = definite

	protected override def lazyGenericMap[X](items: => Map[K, X]) :LazyMap[K, X] = LazyMap.factory.from(() => items)
	protected override def lazySpecific(items : => Map[K, V @uncheckedVariance]) :LazyMap[K, V] =
		LazyMap.factory.from(() => items)

	override def mapFactory :MapFactory[LazyMap] = LazyMap.factory
}

@SerialVersionUID(Ver)
case object LazyMap {
	def apply[K, V](map: => Map[K, V]) :LazyMap[K, V] = new Impl[K, V](() => map)

	@SerialVersionUID(Ver)
	object factory extends MapFactory[LazyMap] {
		def from[K, V](map: () => Map[K, V]) :LazyMap[K, V] = new Impl(map)

		override def from[K, V](it :IterableOnce[(K, V)]) :LazyMap[K, V] = new Impl(() => Map.from(it))

		override def empty[K, V] :LazyMap[K, V] = new Impl(() => Map.empty[K, V])

		override def newBuilder[K, V] :Builder[(K, V), LazyMap[K, V]] =
			Map.newBuilder[K, V].mapResult(map => new Impl(() => map))

		override def toString = "LazyMap.factory"
	}

	private class Impl[K, V] (protected[this] override var initializer :() => Map[K, V])
		extends LazyMap[K, V] with Serializable
}



@SerialVersionUID(Ver)
final class LazySortedMap[K, +V] private (protected[this] override var initializer :() => SortedMap[K, V])
                                         (implicit override val ordering :Ordering[K])
	extends LazyMap[K, V] with SortedMap[K, V] with SortedMapOps[K, V, LazySortedMap, LazySortedMap[K, V]]
	   with AbstractLazy[SortedMap[K, V]]
	   with LazyIterable[(K, V)] with LazyMapOps[K, V, LazySortedMap, LazySortedMap[K, V]]
	   with SortedMapFactoryDefaults[K, V, LazySortedMap, LazyIterable, LazyMap]
{
	override def sortedMapFactory :SortedMapFactory[LazySortedMap] = LazySortedMap.factory

	protected override def definite :SortedMap[K, V] = super[AbstractLazy].definite match {
		case lzy :LazySortedMap[K, V] => lzy.definite
		case strict                   => strict
	}

	protected override def lazyGenericMap[X](items: => Map[K, X]) :LazySortedMap[K, X] =
		new LazySortedMap(() => SortedMap.from(items))

	protected override def lazySpecific(items : => Map[K, V @uncheckedVariance]) :LazySortedMap[K, V] =
		new LazySortedMap(() => SortedMap.from(items))

	override def keySet :LazySortedSet[K] = LazySortedSet.factory.from(() => definite.keySet)

	override def iteratorFrom(start :K) :Iterator[(K, V)] =
		if (initializer == null) definite.iteratorFrom(start) else new LazyIterator(definite.iteratorFrom(start))

	override def keysIteratorFrom(start :K) :Iterator[K] =
		if (initializer == null) definite.keysIteratorFrom(start)
		else new LazyIterator(definite.keysIteratorFrom(start))

	override def rangeImpl(from :Option[K], until :Option[K]) :LazySortedMap[K, V] =
		new LazySortedMap(() => definite.rangeImpl(from, until))
}

@SerialVersionUID(Ver)
case object LazySortedMap {
	def apply[K, V](map: => SortedMap[K, V]) :LazySortedMap[K, V] =
		new LazySortedMap(() => map)(new LazyOrdering(() => map.ordering))

	def empty[K :Ordering, V] :LazySortedMap[K, V] = new LazySortedMap(() => SortedMap.empty)

	@SerialVersionUID(Ver)
	object factory extends SortedMapFactory[LazySortedMap] {
		def from[K, V](map: () => SortedMap[K, V]) :LazySortedMap[K, V] =
			new LazySortedMap[K, V](map)(new LazyOrdering[K](() => map().ordering))

		override def from[K :Ordering, V](it :IterableOnce[(K, V)]) :LazySortedMap[K, V] =
			new LazySortedMap(() => SortedMap.from(it))

		override def empty[K :Ordering, V] :LazySortedMap[K, V] = new LazySortedMap(() => SortedMap.empty)

		override def newBuilder[K :Ordering, V] :Builder[(K, V), LazySortedMap[K, V]] =
			SortedMap.newBuilder[K, V].mapResult(map => from(() => map))

		override def toString = "LazySortedMap.factory"
	}
}




@SerialVersionUID(Ver) //consider: moving it to another package; numeric?. slang?
class LazyOrdering[X] private[collections] (protected override var initializer :() => Ordering[X])
	extends AbstractLazy[Ordering[X]] with Ordering[X] with Serializable
{
	override def compare(x :X, y :X) :Int = definite.compare(x, y)
}
