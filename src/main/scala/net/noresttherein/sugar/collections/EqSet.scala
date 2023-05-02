package net.noresttherein.sugar.collections

import scala.collection.{immutable, mutable, IterableFactory, IterableFactoryDefaults, MapFactory, MapFactoryDefaults}
import scala.collection.immutable.{AbstractMap, AbstractSet, HashMap, HashSet, MapOps, SetOps}
import scala.collection.mutable.{Builder, ReusableBuilder}

import net.noresttherein.sugar.extensions.{cast2TypeParams, castTypeParam}
import net.noresttherein.sugar.vars.EqRef

//implicits
import net.noresttherein.sugar.extensions.builderExtension




/** An immutable [[Set]] implementation using referential equality and hash code (as defined by `AnyRef`)
  * rather than possibly overridden `equals` and `hashCode` methods of the element type.
  * The equality property is 'sticky' - mapping, filtering, etc. this set will also produce an `EqSet`.
  */
@SerialVersionUID(Ver)
final class EqSet[A] private (underlying :Set[EqRef[A]])
	extends AbstractSet[A] with SetOps[A, EqSet, EqSet[A]] with IterableFactoryDefaults[A, EqSet]
	   with Serializable
{
	override def contains(elem :A) :Boolean = underlying.contains(EqRef(elem))
	override def iterator :Iterator[A] = underlying.iterator.map(_.get)

	override def incl(elem :A) :EqSet[A] =
		underlying.incl(EqRef(elem)) match {
			case same if same eq underlying => this
			case other => new EqSet(other)
		}
	override def excl(elem :A) :EqSet[A] =
		underlying.excl(EqRef(elem)) match {
			case same if same eq underlying => this
			case other => new EqSet(other)
		}

	override def iterableFactory :IterableFactory[EqSet] = EqSet
	override def className = "EqSet"
}


@SerialVersionUID(Ver)
case object EqSet extends IterableFactory[EqSet] {
	override def from[A](source :IterableOnce[A]) :EqSet[A] = source match {
		case set :EqSet[A] => set
		case _ => (newBuilder[A] ++= source).result()
	}

	override def empty[A] :EqSet[A] = Empty.castParam[A]
	def wrap[A](set :Set[EqRef[A]]) :EqSet[A] = new EqSet(set)

	override def newBuilder[A] :Builder[A, EqSet[A]] =
		Set.newBuilder[EqRef[A]].mapInput(EqRef.apply[A]).mapResult(new EqSet(_))

	private val Empty = new EqSet(HashSet.empty[EqRef[Any]])
}




/** An immutable [[Map]] implementation using referential equality and hash code (as defined by `AnyRef`)
  * rather than possibly overriden `equals` and `hashCode` methods of the key type.
  */
@SerialVersionUID(Ver)
final class EqMap[K, +V] private (underlying :Map[EqRef[K], V])
	extends AbstractMap[K, V] with MapOps[K, V, EqMap, EqMap[K, V]]
	   with MapFactoryDefaults[K, V, EqMap, immutable.Iterable] with Serializable
{
	override def apply(key :K) :V = underlying(EqRef(key))
	override def get(key :K) :Option[V] = underlying.get(EqRef(key))
	override def getOrElse[V1 >: V](key :K, default : => V1) :V1 =
		underlying.getOrElse(EqRef(key), default)

	override def iterator :Iterator[(K, V)] = underlying.iterator.map { case (k, v) => (k.get, v) }

	override def updated[V1 >: V](key :K, value :V1) :EqMap[K, V1] =
		underlying.updated(EqRef(key), value) match {
			case same if same eq underlying => this
			case other => new EqMap(other)
		}
	override def removed(key :K) :EqMap[K, V] =
		underlying.removed(EqRef(key)) match {
			case same if same eq underlying => this
			case other => new EqMap(other)
		}
	override def withDefault[V1 >: V](d :K => V1) :Map[K, V1] =
		new EqMap(underlying.withDefault { key => d(key.get) })

	override def default(key :K) :V = underlying.default(EqRef(key))

	override def mapFactory :MapFactory[EqMap] = EqMap
	override def className :String = "EqMap"
}


@SerialVersionUID(Ver)
case object EqMap extends MapFactory[EqMap] {
	override def from[K, V](it :IterableOnce[(K, V)]) :EqMap[K, V] = it match {
		case map :EqMap[K, V] => map //fixme: wrong type casting
		case other => (newBuilder ++= other).result()
	}

	override def empty[K, V] :EqMap[K, V] = Empty.castParams[K, V]

	def wrap[K, V](map :Map[EqRef[K], V]) :EqMap[K, V] = new EqMap(map)

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

			override def result() = new EqMap(map)
			override def clear() :Unit = map = Map.empty
		}

	private val Empty = new EqMap(HashMap.empty[EqRef[Any], Any])
}




/** A mutable [[scala.collection.mutable.Set Set]] implementation using reference equality (`eq`) to determine
  * if a value belongs to it. Used to bypass overridden `equals` of the elements.
  */
@SerialVersionUID(Ver)
final class MutableEqSet[A] private(underlying :mutable.Set[EqRef[A]])
	extends mutable.AbstractSet[A] with mutable.SetOps[A, MutableEqSet, MutableEqSet[A]]
	   with IterableFactoryDefaults[A, MutableEqSet] with Serializable
{
	override def contains(elem :A) :Boolean = underlying.contains(EqRef(elem))
	override def iterator :Iterator[A] = underlying.iterator.map(_.get)

	override def addOne(elem :A) :this.type = { underlying.addOne(EqRef(elem)); this }
	override def subtractOne(elem :A) :this.type = { underlying.subtractOne(EqRef(elem)); this }

	override def clear() :Unit = underlying.clear()

	override def iterableFactory :IterableFactory[MutableEqSet] = MutableEqSet
	override def className :String = "MutableEqSet"
}


@SerialVersionUID(Ver)
case object MutableEqSet extends IterableFactory[MutableEqSet] {
	override def from[A](source :IterableOnce[A]) :MutableEqSet[A] = empty[A] ++= source

	override def empty[A] :MutableEqSet[A] = new MutableEqSet[A](mutable.HashSet.empty)
	def wrap[A](set :mutable.Set[EqRef[A]]) :MutableEqSet[A] = new MutableEqSet[A](set)

	override def newBuilder[A] :Builder[A, MutableEqSet[A]] = empty[A]
}




/** A mutable [[scala.collection.mutable.Map Map]] implementation using reference equality (`eq`) to determine
  * if a key belongs to it. Used to bypass overridden `equals` of the key type.
  */
@SerialVersionUID(Ver)
final class MutableEqMap[K, V] private(underlying :mutable.Map[EqRef[K], V])
	extends mutable.AbstractMap[K, V] with mutable.MapOps[K, V, MutableEqMap, MutableEqMap[K, V]]
	   with MapFactoryDefaults[K, V, MutableEqMap, mutable.Iterable] with Serializable
{
	override def contains(key :K) :Boolean = underlying.contains(EqRef(key))
	override def get(key :K) :Option[V] = underlying.get(EqRef(key))
	override def getOrElse[V1 >: V](key :K, default : => V1) :V1 = underlying.getOrElse(EqRef(key), default)
	override def getOrElseUpdate(key :K, op : => V) :V = underlying.getOrElseUpdate(EqRef(key), op)
	override def apply(key :K) :V = underlying(EqRef(key))
	override def default(key :K) :V = underlying.default(EqRef(key))

	override def iterator :Iterator[(K, V)] = underlying.iterator.map { entry => (entry._1.get, entry._2) }

	override def addOne(elem :(K, V)) :this.type = {
		underlying.addOne((EqRef(elem._1), elem._2)); this
	}
	override def subtractOne(elem :K) :this.type = {
		underlying.subtractOne(EqRef(elem)); this
	}

	override def mapFactory :MapFactory[MutableEqMap] = MutableEqMap
	override def className = "MutableEqMap"
}


@SerialVersionUID(Ver)
case object MutableEqMap extends MapFactory[MutableEqMap] {
	override def from[K, V](it :IterableOnce[(K, V)]) :MutableEqMap[K, V] = empty[K, V] ++= it

	override def empty[K, V] :MutableEqMap[K, V] = new MutableEqMap(mutable.Map.empty)
	def wrap[K, V](map :mutable.Map[EqRef[K], V]) :MutableEqMap[K, V] = new MutableEqMap(map)

	override def newBuilder[K, V] :Builder[(K, V), MutableEqMap[K, V]] = empty[K, V]
}
