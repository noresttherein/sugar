package net.noresttherein.sugar.collections

import scala.collection.{IterableFactory, IterableFactoryDefaults, MapFactory, MapFactoryDefaults, StrictOptimizedIterableOps, immutable, mutable}
import scala.collection.immutable.Map.{Map1, Map2, Map3, Map4}
import scala.collection.immutable.Set.{Set1, Set2, Set3, Set4}
import scala.collection.immutable.{AbstractMap, AbstractSet, HashMap, HashSet, MapOps, SetOps, StrictOptimizedMapOps, StrictOptimizedSetOps}
import scala.collection.mutable.{Builder, ReusableBuilder}

import net.noresttherein.sugar.casting.{cast2TypeParamsMethods, castTypeParamMethods}
import net.noresttherein.sugar.collections.extensions.BuilderExtension
import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.vars.EqRef




/** An immutable [[Set]] implementation using referential equality and hash code (as defined by `AnyRef`)
  * rather than possibly overridden `equals` and `hashCode` methods of the element type.
  * The equality property is 'sticky' - mapping, filtering, etc. this set will also produce an `EqSet`.
  */
@SerialVersionUID(Ver)
sealed class EqSet[A] private (underlying :Set[EqRef[A]])
	extends AbstractSet[A] with SetOps[A, EqSet, EqSet[A]] with IterableFactoryDefaults[A, EqSet]
	   with Serializable
{
	final override def contains(elem :A) :Boolean = underlying.contains(EqRef(elem))
	final override def iterator :Iterator[A] = underlying.iterator.map(_.get)

	final override def incl(elem :A) :EqSet[A] =
		underlying.incl(EqRef(elem)) match {
			case same if same eq underlying => this
			case other => new EqSet(other)
		}
	final override def excl(elem :A) :EqSet[A] =
		underlying.excl(EqRef(elem)) match {
			case same if same eq underlying => this
			case other => new EqSet(other)
		}

	private def isCovariant :Boolean = underlying match {
		case _ :HashSet[_] | _ :Set1[_] | _ :Set2[_] | _ :Set3[_] | _ :Set4[_] => true
		case _ => false
	}
	override def iterableFactory :IterableFactory[EqSet] = EqSet
	override def className = "EqSet"
}


@SerialVersionUID(Ver)
case object EqSet extends IterableFactory[EqSet] {
	override def from[A](source :IterableOnce[A]) :EqSet[A] = source match {
		case set :EqSet[A] if set.isCovariant => set
		case _ => (newBuilder[A] ++= source).result()
	}

	override def empty[A] :EqSet[A] = Empty.castParam[A]

	def wrap[A](set :Set[EqRef[A]]) :EqSet[A] =
		if (set.isInstanceOf[StrictOptimizedSetOps[_, generic.Any1, _]])
			new EqSet(set) with StrictOptimizedSetOps[A, EqSet, EqSet[A]]
		else
			new EqSet(set)

	override def newBuilder[A] :Builder[A, EqSet[A]] =
		Set.newBuilder[EqRef[A]].mapInput(EqRef.apply[A]).mapResult(wrap)

	private val Empty = new EqSet(HashSet.empty[EqRef[Any]])
}




/** An immutable [[Map]] implementation using referential equality and hash code (as defined by `AnyRef`)
  * rather than possibly overriden `equals` and `hashCode` methods of the key type.
  */
@SerialVersionUID(Ver)
sealed class EqMap[K, +V] private (underlying :Map[EqRef[K], V])
	extends AbstractMap[K, V] with MapOps[K, V, EqMap, EqMap[K, V]]
	   with MapFactoryDefaults[K, V, EqMap, immutable.Iterable] with Serializable
{
	final override def apply(key :K) :V = underlying(EqRef(key))
	final override def get(key :K) :Option[V] = underlying.get(EqRef(key))
	final override def getOrElse[V1 >: V](key :K, default : => V1) :V1 =
		underlying.getOrElse(EqRef(key), default)

	final override def iterator :Iterator[(K, V)] = underlying.iterator.map { case (k, v) => (k.get, v) }

	final override def updated[V1 >: V](key :K, value :V1) :EqMap[K, V1] =
		underlying.updated(EqRef(key), value) match {
			case same if same eq underlying => this
			case other => new EqMap(other)
		}
	final override def removed(key :K) :EqMap[K, V] =
		underlying.removed(EqRef(key)) match {
			case same if same eq underlying => this
			case other => new EqMap(other)
		}
	final override def withDefault[V1 >: V](d :K => V1) :Map[K, V1] =
		new EqMap(underlying.withDefault { key => d(key.get) })

	final override def default(key :K) :V = underlying.default(EqRef(key))

	private def isCovariant :Boolean = underlying match {
		//Map.WithDefault is not covariant because it has a function K => V.
		case _ :HashMap[_, _] | _ :Map1[_, _] | _ :Map2[_, _] | _ :Map3[_, _] | _ :Map4[_, _] => true
		case _ => false
	}
	override def mapFactory :MapFactory[EqMap] = EqMap
	override def className :String = "EqMap"
}


@SerialVersionUID(Ver)
case object EqMap extends MapFactory[EqMap] {
	override def from[K, V](it :IterableOnce[(K, V)]) :EqMap[K, V] = it match {
		case map :EqMap[K, V] if map.isCovariant => map
		case other => (newBuilder ++= other).result()
	}

	override def empty[K, V] :EqMap[K, V] = Empty.castParams[K, V]

	//consider: removing it and always using a HashMap, so we can safely reuse an instance in from
	def wrap[K, V](map :Map[EqRef[K], V]) :EqMap[K, V] =
		if (map.isInstanceOf[StrictOptimizedMapOps[K, V, Map, Map[K, V]] @unchecked])
			new EqMap[K, V](map) with StrictOptimizedMapOps[K, V, EqMap, EqMap[K, V]]
		else
			new EqMap(map)

	override def newBuilder[K, V] :Builder[(K, V), EqMap[K, V]] =
		new ReusableBuilder[(K, V), EqMap[K, V]] {
			private[this] var map = Map.empty[EqRef[K], V]

			override def knownSize = map.size

			override def addOne(elem :(K, V)) :this.type = {
				map = map.updated(EqRef(elem._1), elem._2); this
			}
			override def addAll(xs :IterableOnce[(K, V)]) :this.type = {
				sizeHint(xs, map.size)
				super.addAll(xs)
			}

			override def result() = wrap(map)
			override def clear() :Unit = map = Map.empty
		}

	private val Empty = new EqMap(HashMap.empty[EqRef[Any], Any])
}




/** A mutable [[scala.collection.mutable.Set Set]] implementation using reference equality (`eq`) to determine
  * if a value belongs to it. Used to bypass overridden `equals` of the elements.
  */ //consider: renaming it to MutEqSet
@SerialVersionUID(Ver)
sealed class MutableEqSet[A] private(underlying :mutable.Set[EqRef[A]])
	extends mutable.AbstractSet[A] with mutable.SetOps[A, MutableEqSet, MutableEqSet[A]]
	   with IterableFactoryDefaults[A, MutableEqSet] with Serializable
{
	final override def contains(elem :A) :Boolean = underlying.contains(EqRef(elem))
	final override def iterator :Iterator[A] = underlying.iterator.map(_.get)

	final override def addOne(elem :A) :this.type = { underlying.addOne(EqRef(elem)); this }
	final override def subtractOne(elem :A) :this.type = { underlying.subtractOne(EqRef(elem)); this }

	final override def clear() :Unit = underlying.clear()

	final override def iterableFactory :IterableFactory[MutableEqSet] = MutableEqSet
	final override def className :String = "MutableEqSet"
}


@SerialVersionUID(Ver)
case object MutableEqSet extends IterableFactory[MutableEqSet] {
	override def from[A](source :IterableOnce[A]) :MutableEqSet[A] = empty[A] ++= source

	override def empty[A] :MutableEqSet[A] = new MutableEqSet[A](mutable.HashSet.empty)

	def wrap[A](set :mutable.Set[EqRef[A]]) :MutableEqSet[A] =
		if (set.isInstanceOf[collection.StrictOptimizedSetOps[A, collection.Set, collection.Set[A]] @unchecked])
			new MutableEqSet[A](set) with collection.StrictOptimizedSetOps[A, MutableEqSet, MutableEqSet[A]]
		else
			new MutableEqSet[A](set)

	override def newBuilder[A] :Builder[A, MutableEqSet[A]] = empty[A]
}




/** A mutable [[scala.collection.mutable.Map Map]] implementation using reference equality (`eq`) to determine
  * if a key belongs to it. Used to bypass overridden `equals` of the key type.
  */ //consider: renaming it to MutEqMap
@SerialVersionUID(Ver)
sealed class MutableEqMap[K, V] private(underlying :mutable.Map[EqRef[K], V])
	extends mutable.AbstractMap[K, V] with mutable.MapOps[K, V, MutableEqMap, MutableEqMap[K, V]]
	   with MapFactoryDefaults[K, V, MutableEqMap, mutable.Iterable] with Serializable
{
	final override def contains(key :K) :Boolean = underlying.contains(EqRef(key))
	final override def get(key :K) :Option[V] = underlying.get(EqRef(key))
	final override def getOrElse[V1 >: V](key :K, default : => V1) :V1 = underlying.getOrElse(EqRef(key), default)
	final override def getOrElseUpdate(key :K, op : => V) :V = underlying.getOrElseUpdate(EqRef(key), op)
	final override def apply(key :K) :V = underlying(EqRef(key))
	final override def default(key :K) :V = underlying.default(EqRef(key))

	final override def iterator :Iterator[(K, V)] = underlying.iterator.map { entry => (entry._1.get, entry._2) }

	final override def addOne(elem :(K, V)) :this.type = {
		underlying.addOne((EqRef(elem._1), elem._2)); this
	}
	final override def subtractOne(elem :K) :this.type = {
		underlying.subtractOne(EqRef(elem)); this
	}

	override def mapFactory :MapFactory[MutableEqMap] = MutableEqMap
	override def className = "MutableEqMap"
}


@SerialVersionUID(Ver)
case object MutableEqMap extends MapFactory[MutableEqMap] {
	override def from[K, V](it :IterableOnce[(K, V)]) :MutableEqMap[K, V] = empty[K, V] ++= it

	override def empty[K, V] :MutableEqMap[K, V] = new MutableEqMap(mutable.Map.empty)

	def wrap[K, V](map :mutable.Map[EqRef[K], V]) :MutableEqMap[K, V] =
//		new MutableEqMap(map)
		if (map.isInstanceOf[collection.StrictOptimizedMapOps[K, V, mutable.Map, mutable.Map[K, V]] @unchecked])
			new MutableEqMap[K, V](map)
				with StrictOptimizedIterableOps[(K, V), mutable.Iterable, MutableEqMap[K, V]]
				with collection.StrictOptimizedMapOps[K, V, MutableEqMap, MutableEqMap[K, V]]
		else
			new MutableEqMap(map)

	override def newBuilder[K, V] :Builder[(K, V), MutableEqMap[K, V]] = empty[K, V]
}
