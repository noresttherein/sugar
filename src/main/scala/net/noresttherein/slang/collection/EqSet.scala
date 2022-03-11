package net.noresttherein.slang.collection

import scala.collection.{immutable, mutable, IterableFactory, IterableFactoryDefaults, MapFactory, MapFactoryDefaults}
import scala.collection.immutable.{AbstractMap, AbstractSet, MapOps, SetOps}
import scala.collection.mutable.{Builder, ReusableBuilder}

import net.noresttherein.slang.extensions.builderExtension
import net.noresttherein.slang.vars.Opt
import net.noresttherein.slang.vars.Opt.Got




/** A simple wrapper over any value with equality (and hash code) defined as the ''referential'' equality
  * of the wrapped values. Used when there is a need to bypass possibly overriden `equals` (and `hashCode`)
  * by the wrapped type.
  * @see [[net.noresttherein.slang.collection.EqSet]]
  * @see [[net.noresttherein.slang.collection.EqMap]]
  * @see [[net.noresttherein.slang.collection.MutableEqSet]]
  * @see [[net.noresttherein.slang.collection.MutableEqMap]]
  */
private[collection] final class IdentityKey[T](val value :T) extends Serializable {
	override def equals(that :Any) :Boolean = that match {
		case other :IdentityKey[_] => value.asInstanceOf[AnyRef] eq other.value.asInstanceOf[AnyRef]
		case _ => false
	}
	override def hashCode :Int = System.identityHashCode(value)
	override def toString :String = value.toString
}

private[collection] object IdentityKey {
	@inline def apply[T](value :T) = new IdentityKey(value)
	@inline def unapply[T](key :IdentityKey[T]) :Opt[T] = Got(key.value)
}




/** An immutable [[Set]] implementation using referential equality and hash code (as defined by `AnyRef`)
  * rather than possibly overriden `equals` and `hashCode` methods of the element type.
  */
final class EqSet[A] private (underlying :Set[IdentityKey[A]])
	extends AbstractSet[A] with SetOps[A, EqSet, EqSet[A]] with IterableFactoryDefaults[A, EqSet]
	   with Serializable
{
	override def contains(elem :A) :Boolean = underlying.contains(new IdentityKey(elem))
	override def iterator :Iterator[A] = underlying.iterator.map(_.value)

	override def incl(elem :A) :EqSet[A] =
		underlying.incl(new IdentityKey(elem)) match {
			case same if same eq underlying => this
			case other => new EqSet(other)
		}
	override def excl(elem :A) :EqSet[A] =
		underlying.excl(new IdentityKey(elem)) match {
			case same if same eq underlying => this
			case other => new EqSet(other)
		}

	override def iterableFactory :IterableFactory[EqSet] = EqSet
	override def className = "EqSet"
}


object EqSet extends IterableFactory[EqSet] {
	override def from[A](source :IterableOnce[A]) :EqSet[A] = source match {
		case set :EqSet[A] => set
		case _ => (newBuilder[A] ++= source).result()
	}

	override def empty[A] :EqSet[A] = new EqSet(Set.empty[IdentityKey[A]])
	def wrap[A](set :Set[IdentityKey[A]]) :EqSet[A] = new EqSet(set)

	override def newBuilder[A] :Builder[A, EqSet[A]] =
		Set.newBuilder[IdentityKey[A]].mapInput(new IdentityKey[A](_)).mapResult(new EqSet(_))
}




/** An immutable [[Map]] implementation using referential equality and hash code (as defined by `AnyRef`)
  * rather than possibly overriden `equals` and `hashCode` methods of the key type.
  */
final class EqMap[K, +V] private (underlying :Map[IdentityKey[K], V])
	extends AbstractMap[K, V] with MapOps[K, V, EqMap, EqMap[K, V]]
	   with MapFactoryDefaults[K, V, EqMap, immutable.Iterable] with Serializable
{
	override def apply(key :K) :V = underlying(new IdentityKey(key))
	override def get(key :K) :Option[V] = underlying.get(new IdentityKey(key))
	override def getOrElse[V1 >: V](key :K, default : => V1) :V1 =
		underlying.getOrElse(new IdentityKey(key), default)

	override def iterator :Iterator[(K, V)] = underlying.iterator.map { case (k, v) => (k.value, v) }

	override def updated[V1 >: V](key :K, value :V1) :EqMap[K, V1] =
		underlying.updated(new IdentityKey(key), value) match {
			case same if same eq underlying => this
			case other => new EqMap(other)
		}
	override def removed(key :K) :EqMap[K, V] =
		underlying.removed(new IdentityKey(key)) match {
			case same if same eq underlying => this
			case other => new EqMap(other)
		}
	override def withDefault[V1 >: V](d :K => V1) :Map[K, V1] =
		new EqMap(underlying.withDefault { key => d(key.value) })

	override def default(key :K) :V = underlying.default(new IdentityKey(key))


	override def mapFactory :MapFactory[EqMap] = EqMap
	override def className :String = "EqMap"
}


object EqMap extends MapFactory[EqMap] {
	override def from[K, V](it :IterableOnce[(K, V)]) :EqMap[K, V] = it match {
		case map :EqMap[K, V] => map //fixme: wrong type casting
		case other => (newBuilder ++= other).result()
	}

	override def empty[K, V] :EqMap[K, V] = new EqMap(Map.empty[IdentityKey[K], V])
	def wrap[K, V](map :Map[IdentityKey[K], V]) :EqMap[K, V] = new EqMap(map)

	override def newBuilder[K, V] :Builder[(K, V), EqMap[K, V]] =
		new ReusableBuilder[(K, V), EqMap[K, V]] {
			private[this] var map = Map.empty[IdentityKey[K], V]

			override def knownSize = map.size

			override def addOne(elem :(K, V)) :this.type = {
				map = map.updated(new IdentityKey(elem._1), elem._2); this
			}
			override def addAll(xs :IterableOnce[(K, V)]) :this.type = {
				sizeHint(xs, map.size)
				super.addAll(xs)
			}

			override def result() = new EqMap(map)
			override def clear() :Unit = map = Map.empty
		}
}




/** A mutable [[scala.collection.mutable.Set Set]] implementation using reference equality (`eq`) to determine
  * if a value belongs to it. Used to bypass overriden `equals` of the elements.
  */
final class MutableEqSet[A] private(underlying :mutable.Set[IdentityKey[A]])
	extends mutable.AbstractSet[A] with mutable.SetOps[A, MutableEqSet, MutableEqSet[A]]
	   with IterableFactoryDefaults[A, MutableEqSet] with Serializable
{
	override def contains(elem :A) :Boolean = underlying.contains(new IdentityKey(elem))
	override def iterator :Iterator[A] = underlying.iterator.map(_.value)

	override def addOne(elem :A) :this.type = { underlying.addOne(new IdentityKey(elem)); this }
	override def subtractOne(elem :A) :this.type = { underlying.subtractOne(new IdentityKey(elem)); this }

	override def clear() :Unit = underlying.clear()

	override def iterableFactory :IterableFactory[MutableEqSet] = MutableEqSet
	override def className :String = "MutableEqSet"
}


object MutableEqSet extends IterableFactory[MutableEqSet] {
	override def from[A](source :IterableOnce[A]) :MutableEqSet[A] = empty[A] ++= source

	override def empty[A] :MutableEqSet[A] = new MutableEqSet[A](mutable.HashSet.empty)
	def wrap[A](set :mutable.Set[IdentityKey[A]]) :MutableEqSet[A] = new MutableEqSet[A](set)

	override def newBuilder[A] :Builder[A, MutableEqSet[A]] = empty[A]
}




/** A mutable [[scala.collection.mutable.Map Map]] implementation using reference equality (`eq`) to determine
  * if a key belongs to it. Used to bypass overriden `equals` of the key type.
  */
final class MutableEqMap[K, V] private(underlying :mutable.Map[IdentityKey[K], V])
	extends mutable.AbstractMap[K, V] with mutable.MapOps[K, V, MutableEqMap, MutableEqMap[K, V]]
	   with MapFactoryDefaults[K, V, MutableEqMap, mutable.Iterable] with Serializable
{
	override def contains(key :K) :Boolean = underlying.contains(new IdentityKey(key))
	override def get(key :K) :Option[V] = underlying.get(new IdentityKey(key))
	override def getOrElse[V1 >: V](key :K, default : => V1) :V1 = underlying.getOrElse(new IdentityKey(key), default)
	override def getOrElseUpdate(key :K, op : => V) :V = underlying.getOrElseUpdate(new IdentityKey(key), op)
	override def apply(key :K) :V = underlying(new IdentityKey(key))
	override def default(key :K) :V = underlying.default(new IdentityKey(key))

	override def iterator :Iterator[(K, V)] = underlying.iterator.map { entry => (entry._1.value, entry._2) }

	override def addOne(elem :(K, V)) :this.type = {
		underlying.addOne((new IdentityKey(elem._1), elem._2)); this
	}
	override def subtractOne(elem :K) :this.type = {
		underlying.subtractOne(new IdentityKey(elem)); this
	}

	override def mapFactory :MapFactory[MutableEqMap] = MutableEqMap
	override def className = "MutableEqMap"
}


object MutableEqMap extends MapFactory[MutableEqMap] {
	override def from[K, V](it :IterableOnce[(K, V)]) :MutableEqMap[K, V] = empty[K, V] ++= it

	override def empty[K, V] :MutableEqMap[K, V] = new MutableEqMap(mutable.Map.empty)
	def wrap[K, V](map :mutable.Map[IdentityKey[K], V]) :MutableEqMap[K, V] = new MutableEqMap(map)

	override def newBuilder[K, V] :Builder[(K, V), MutableEqMap[K, V]] = empty[K, V]
}
