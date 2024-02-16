package net.noresttherein.sugar.collections

import java.lang.System.arraycopy

import scala.annotation.nowarn
import scala.collection.{AbstractIterable, AbstractSet, ArrayOps, Factory, StrictOptimizedIterableOps}
import scala.collection.immutable.AbstractMap
import scala.collection.mutable.{Builder, ReusableBuilder}
import scala.util.hashing.MurmurHash3

import net.noresttherein.sugar.collections.ComparableFactory
import net.noresttherein.sugar.collections.NatMap.{Assoc, BaseNatMap, WhenNoKey}
import net.noresttherein.sugar.collections.NatMap.WhenNoKey.throwANoSuchElementException
import net.noresttherein.sugar.exceptions.{noSuch_!, unsupported_!}
import net.noresttherein.sugar.extensions.OptionExtension
import net.noresttherein.sugar.funny.generic.=>:
import net.noresttherein.sugar.vars.{AbstractPure, Maybe, Opt}
import net.noresttherein.sugar.vars.Maybe.{No, Yes}
import net.noresttherein.sugar.vars.Opt.One






/** A generic `Map`-like interface which implements a natural transformation between type functors `K` and `V`.
  * Each key is of type `K[X] forSome { type X }` and is mapped to a value of type `V[X]` for the same type argument `X`.
  * @define Coll `NatMap`
  * @define coll natural map
  * @author Marcin Mościcki
  */
//consider: making it invariant. Type inference sucks for this trait and we don't really take advantage of covariance
// much as we need casting on the key type anyway.
//todo: rename to GenNatMap, and make an immutable NatMap.
trait NatMap[K[X], +V[X]]
	extends Iterable[NatMap.Assoc[K, V, _]] with (K =>: V) with Equals with Serializable
{ outer =>
	def opt[X](key :K[X]) :Opt[V[X]]

	def get[X](key :K[X]) :Option[V[X]] = opt(key).toOption

	def getOrElse[U[T] >: V[T], X](key :K[X], default: => U[X]) :U[X] = get(key) getOrElse default

	def apply[X](key :K[X]) :V[X] = opt(key) match {
		case One(v) => v
		case _ => defaults(key)
	}


	def contains(key :K[_]) :Boolean = opt(key).isDefined

	def removed(key :K[_]) :NatMap[K, V]

	def updated[U[T] >: V[T], X](key :K[X], value :U[X]) :NatMap[K, U]

	def +[U[T] >: V[T], X](key :K[X], value : U[X]) :NatMap[K, U] = updated(key, value)

	def +[U[T] >: V[T], X](entry :NatMap.Assoc[K, U, X]) :NatMap[K, U] = updated(entry._1, entry._2)

	def ++[U[T] >: V[T]](entries :IterableOnce[NatMap.Assoc[K, U, _]]) :NatMap[K, U]



	def withDefault[U[T] >: V[T]](whenNoKey :K =>: U) :NatMap[K, U] = new BaseNatMap[K, U] {
		override def opt[X](key :K[X]) = outer.get(key).toOpt

		override def contains(key :K[_]) = outer.contains(key)

		override def removed(key :K[_]) = outer.removed(key).withDefault(whenNoKey)

		override def updated[W[T] >: U[T], X](key :K[X], value :W[X]) =
			outer.updated(key, value).withDefault(whenNoKey)

		override def ++[W[T] >: U[T]](entries :IterableOnce[Assoc[K, W, _]]) :NatMap[K, W] =
			(outer ++ entries).withDefault(whenNoKey)

		override def iterator = outer.iterator
		override def keySet = outer.keySet
		override def values = outer.values

		override def withDefault[S[T] >: U[T]](whenNoKey :K =>: S) =
			outer.withDefault(whenNoKey)

		protected override def default[X](key :K[X]) = whenNoKey(key)

		override def defaults :WhenNoKey[K, U] = WhenNoKey(whenNoKey)
	}


	override def iterator :Iterator[NatMap.Assoc[K, V, _]]

	def keySet :collection.Set[K[_]]

	def values :Iterable[V[_]]

	def toMap :collection.Map[K[_], V[_]]


	//todo: it's fishy that map and flatMap have the same erased signature as inherited methods from Iterable, but different in reality
	def map[A[_], B[_]](f :Assoc[K, V, _] => Assoc[A, B, _]) :NatMap[A, B] = {
		val res = NatMap.newBuilder[A, B]
		val iter = iterator
		while (iter.hasNext)
			res += f(iter.next())
		res.result()
	}

	def flatMap[A[_], B[_]](f :Assoc[K, V, _] => IterableOnce[Assoc[A, B, _]]) :NatMap[A, B] = {
		val res = NatMap.newBuilder[A, B]
		val iter = iterator
		while (iter.hasNext)
			res ++= f(iter.next())
		res.result()
	}

	override def filter(p :Assoc[K, V, _] => Boolean) :NatMap[K, V] = filterWhere(p, true)
	override def filterNot(p :Assoc[K, V, _] => Boolean) :NatMap[K, V] = filterWhere(p, false)
	private def filterWhere(p :Assoc[K, V, _] => Boolean, where :Boolean) :NatMap[K, V] = {
		val res = NatMap.newBuilder[K, V]
		val iter = iterator
		while (iter.hasNext) {
			val entry = iter.next()
			if (p(entry) == where)
				res += entry
		}
		res.result()
	}

	override def partition(p :Assoc[K, V, _] => Boolean) :(NatMap[K, V], NatMap[K, V]) = {
		val yes = NatMap.newBuilder[K, V]
		val no  = NatMap.newBuilder[K, V]
		val iter = iterator
		while (iter.hasNext) {
			val entry = iter.next()
			if (p(entry)) yes += entry
			else no += entry
		}
		(yes.result(), no.result())
	}

	private[collections] implicit def defaults :WhenNoKey[K, V]

	protected[this] override def fromSpecific(coll :IterableOnce[Assoc[K, V, _]]) :NatMap[K, V] = NatMap.from(coll)
	protected[this] override def newSpecificBuilder :Builder[Assoc[K, V, _], NatMap[K, V]] = NatMap.newBuilder
	override def empty :NatMap[K, V] = MutNatMap.empty

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[NatMap[K @unchecked, V @unchecked]]

	protected[this] override def className :String = "NatMap"

}






/** Brings into the implicit scope a conversion from the [[net.noresttherein.sugar.collections.NatMap$ NatMap]]
  * object to [[scala.collection.Factory Factory]]`[`[[net.noresttherein.sugar.collections.NatMap.Assoc Assoc]]`[K[?], V[?], ?], `[[net.noresttherein.sugar.collections.NatMap NatMap]]`[K, V]]`.
  */
private[collections] sealed abstract class ImplicitNatMapFactory

object ImplicitNatMapFactory {
	/** Converts `NatMap` companion object to a standard `Factory` from the scala collection framework, allowing
	  * its use as an argument to `to` method of any collection.
	  */
	implicit def toNatMapFactory[K[_], V[_]](companion :NatMap.type)
	                                        (implicit default :WhenNoKey[K, V] = throwANoSuchElementException[K])
			:Factory[Assoc[K, V, _], NatMap[K, V]] =
		companion.factory
}



/** A factory of maps where key and values are both types parameterized with the same type. */
@SerialVersionUID(Ver)
object NatMap extends ImplicitNatMapFactory {

	/** A single entry of a [[net.noresttherein.sugar.collections.NatMap NatMap]],
	  * associating a value of `V[X]` with a key of `K[X]`.
	  */
	sealed trait Assoc[+K[_], +V[_], X] extends Product2[K[X], V[X]] with Serializable {
		private[NatMap] def asMap[U[A] >: K[A]] :NatMap[U, V] = this.asInstanceOf[NatMap[U, V]]
		private[NatMap] def keyHashCode :Int
	}

	@SerialVersionUID(Ver)
	object Assoc {
		def apply[K[_], V[_], X](key :K[X], value :V[X]) :Assoc[K, V, X] =
			new Singleton(key, value)

		@inline def unapply[K[_], V[_], X](assoc :Assoc[K, V, X]) :Yes[(K[X], V[X])] = Yes((assoc._1, assoc._2))
	}

	implicit class ->:[K[_], X](private val key :K[X]) extends AnyVal {
		@inline def ->:[V[_]](value :V[X]) :Assoc[K, V, X] = Assoc(key, value)
	}

	implicit class NatMapExtension[K[_], V[_]](private val self :NatMap[K, V]) extends AnyVal {
		@inline def +[X](key :K[X], value :V[X]) :NatMap[K, V] = self.updated(key, value)
		@inline def +[X](entry :(K[X], V[X])) :NatMap[K, V] = self.updated(entry._1, entry._2)
		@inline def +[X](entry :Assoc[K, V, X]) :NatMap[K, V] = self.updated(entry._1, entry._2)
	}



	def apply[K[_], V[_]](entries :Assoc[K, V, _]*)
	                     (implicit default :WhenNoKey[K, V] = throwANoSuchElementException[K]) :NatMap[K, V] =
		from(entries)

	def from[K[_], V[_]](entries :IterableOnce[Assoc[K, V, _]])
	                    (implicit default :WhenNoKey[K, V] = throwANoSuchElementException[K]) :NatMap[K, V] =
		withDefault(entries)(default)

	def single[K[_], V[_], X](key :K[X], value :V[X])
	                         (implicit default :WhenNoKey[K, V] = throwANoSuchElementException[K]) :NatMap[K, V] =
		withDefault(key, value)(default)

	def empty[K[_], V[_]](implicit default :WhenNoKey[K, V] = throwANoSuchElementException[K]) :NatMap[K, V] =
		withDefault(default)


	def withDefault[K[_], V[_]](entries :Assoc[K, V, _]*)(default :WhenNoKey[K, V]) :NatMap[K, V] =
		withDefault(entries :IterableOnce[Assoc[K, V, _]])(default)

	def withDefault[K[_], V[_]](entries :IterableOnce[Assoc[K, V, _]])(default :WhenNoKey[K, V]) :NatMap[K, V] =
		entries match {
			case map :NatMap[K @unchecked, V @unchecked] if map.defaults == default => map
			case it :Iterable[Assoc[K, V, _]] if it.isEmpty => withDefault[K, V](default)
			case it :Iterable[Assoc[K, V, _]] if it.sizeIs == 1 =>
				val res = it.head.asMap
				if (res.defaults == default) res
				else {
					def entry[T](pair :Assoc[K, V, T]) = withDefault(pair._1, pair._2)(default)
					entry(it.head)
				}
			case it :Iterable[Assoc[K, V, _]] if it.sizeIs <= SmallNatMapCap =>
				new SmallNatMap[K, V](it.toArray)(default)

			case it :Iterator[Assoc[K, V, _]] if it.isEmpty => withDefault[K, V](default)
			case _ => (newBuilder[K, V](default) ++= entries).result()
		}

	def withDefault[K[_], V[_], X](key :K[X], value :V[X])(default :WhenNoKey[K, V]) :NatMap[K, V] =
		new Singleton(key, value, key.hashCode)(default)

	def withDefault[K[_], V[_]](default :WhenNoKey[K, V]) :NatMap[K, V] =
		if (default == throwANoSuchElementException[K])
			instance.asInstanceOf[NatMap[K, V]]
		else
			new EmptyMap[K, V]()(default)


	private[this] final val instance = new EmptyMap[Seq, Seq]


	def newBuilder[K[_], V[_]](implicit default :WhenNoKey[K, V] = throwANoSuchElementException[K])
			:Builder[Assoc[K, V, _], NatMap[K, V]] =
		new NatMapBuilder

	def factory[K[_], V[_]](implicit default :WhenNoKey[K, V] = throwANoSuchElementException[K])
			:Factory[Assoc[K, V, _], NatMap[K, V]] =
		new ComparableFactory[Assoc[K, V, _], NatMap[K, V]] {
			override def factory = NatMap
			override def fromSpecific(it :IterableOnce[Assoc[K, V, _]]) = NatMap.from(it)
			override def newBuilder = NatMap.newBuilder
		}


	def Lazy[K[_], V[_]](entries: => IterableOnce[Assoc[K, V, _]]) :NatMap[K, V] =
		new LazyNatMap(() => NatMap(entries.iterator.toSeq :_*))

	def delayed[K[_], V[_]](map: => NatMap[K, V]) :NatMap[K, V] = new LazyNatMap(() => map)



	/** An implicit opportunistic type class used by some `NatMap` implementations to handle the situation
	  * of a missing key. This allows 'plugging in' behaviour of throwing a desired, more informational exception,
	  * without a need of guarding every map access against thrown default `NoSuchElementException` exceptions.
	  * This class has no implicit values in the implicit scope and they must be imported explicitly,
	  * but client code typically makes providing it optional.
	  * See the companion object for common predefined implementations.
	  */ //todo: replace with an opaque type of a generic function
	trait WhenNoKey[K[_], +V[_]] extends Serializable {
		def apply[X](key :K[X]) :V[X]
	}

	@SerialVersionUID(Ver)
	object WhenNoKey {
		def apply[K[_], V[_]](f :K =>: V) :WhenNoKey[K, V] = new Wrapper[K, V](f)

		private class Wrapper[K[_], V[_]](f :K =>: V) extends WhenNoKey[K, V] {
			override def apply[X](key :K[X]) = f(key)
			def defaults = f

			override def equals(that :Any) :Boolean = that match {
				case other :Wrapper[K @unchecked, V @unchecked] => (this eq other) || f == other.defaults
				case _ => false
			}
			override def hashCode = f.hashCode
		}

		type Throw[X] = Nothing

		def throwANoSuchElementException[K[_]] :WhenNoKey[K, Throw] =
			noSuch.asInstanceOf[WhenNoKey[K, Throw]]

		@SerialVersionUID(Ver)
		private object noSuch extends WhenNoKey[({ type K[_] = Any })#K, Throw] {
			override def apply[X](key :Any) = noSuch_!(key.toString)
		}
	}



	private trait BaseNatMap[K[_], +V[_]] extends NatMap[K, V] { outer =>

		override def contains(key :K[_]) :Boolean = opt(key).isDefined

		override def removed(key :K[_]) :NatMap[K, V] =
			if (contains(key)) {
				val res = NatMap.newBuilder[K, V]
				for (entry <- this if entry._1 != key)
					res += entry
				res.result()
			} else
				  this

		override def updated[U[T] >: V[T], X](key :K[X], value :U[X]) :NatMap[K, U] =
			opt(key) match {
				case Yes(x) if x == value => this
				case _ => (NatMap.newBuilder[K, U] ++= this += Assoc(key, value)).result()
			}

		override def ++[U[T] >: V[T]](entries :IterableOnce[Assoc[K, U, _]]) :NatMap[K, U] =
			if (entries.iterator.isEmpty) this
			else (NatMap.newBuilder[K, U] ++= this ++= entries).result()

		override def keySet :collection.Set[K[_]] =
			new AbstractSet[K[_]] with Serializable {
				override def knownSize = BaseNatMap.this.knownSize
				override def iterator = BaseNatMap.this.iterator.map(_._1)
				override def contains(elem :K[_]) = BaseNatMap.this.contains(elem)
				override def diff(that :collection.Set[K[_]]) =
					(Set.newBuilder[K[_]] /: BaseNatMap.this) { (res, entry) =>
						if (!that(entry._1)) res += entry._1
						else res
					}.result()
			}

		override def values :Iterable[V[_]] =
			new AbstractIterable[V[_]] with Serializable {
				override def knownSize = BaseNatMap.this.knownSize
				override def iterator  = BaseNatMap.this.iterator.map(_._2)
				override def foreach[U](f :V[_] => U) :Unit = BaseNatMap.this.foreach(entry => f(entry._2))
			}

		override def toMap :Map[K[_], V[_]] =
			new AbstractMap[K[_], V[_]] with Serializable {
				override def knownSize = outer.knownSize
				override def iterator = outer.iterator.map(entry => (entry._1, entry._2))
				override def removed(key :K[_]) = outer.opt(key) match {
					case Yes(_) => iterator.filterNot(_._1 == key) to Map
					case _ => this
				}
				override def updated[V1 >: V[_]](key :K[_], value :V1) = opt(key) match {
					case Yes(v) if v == value => this
					case Yes(_) => outer.iterator.map {
						entry => if (entry._1 == key) (key, value) else (entry._1, entry._2)
					} to Map
					case _ => iterator ++ Iterator.single((key, value)) to Map
				}
				override def get(key :K[_]) = outer.get(key)
			}

		protected def default[X](key :K[X]) :V[X] = defaults(key)

		implicit override def defaults :WhenNoKey[K, V] = throwANoSuchElementException
	}




	@SerialVersionUID(Ver)
	private class EmptyMap[K[_], +V[_]](implicit override val defaults :WhenNoKey[K, V] = throwANoSuchElementException[K])
		extends BaseNatMap[K, V]
	{
		override def knownSize = 0
		override def size = 0
		override def isEmpty = true

		override def head = noSuch_!("NatMap().head")
		override def tail = unsupported_!("NatMap().tail")

		override def opt[X](key :K[X]) :Opt[V[X]] = None
		override def apply[X](key :K[X]) = defaults(key)
		override def get[X](key :K[X]) :Option[V[X]] = None
		override def getOrElse[U[T] >: V[T], X](key :K[X], default : => U[X]) :U[X] = default

		override def contains(key :K[_]) :Boolean = false
		override def removed(key :K[_]) :NatMap[K, V] = this
		override def updated[U[T] >: V[T], X](key :K[X], value :U[X]) :NatMap[K, U] = new Singleton(key, value)

		override def filter(p :Assoc[K, V, _] => Boolean) :NatMap[K, V] = this
		override def map[A[_], B[_]](f :Assoc[K, V, _] => Assoc[A, B, _]) :NatMap[A, B] = NatMap.empty
		override def flatMap[A[_], B[_]](f :Assoc[K, V, _] => IterableOnce[Assoc[A, B, _]]) :NatMap[A, B] = NatMap.empty

		override val iterator = Iterator.empty
		override def keySet = Set.empty
		override def values = Set.empty
		override def toMap  = Map.empty

		override def canEqual(that :Any) :Boolean = equals(that)
		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case map :NatMap[K @unchecked, V @unchecked] => map.isEmpty
			case _ => false
		}
		override val hashCode = MurmurHash3.mapHash(Map.empty)

		override def toString = "NatMap()"
	}




	/** A singleton `NatMap` and a map entry in one. */
	@SerialVersionUID(Ver)
	private class Singleton[K[_], +V[_], T](override val _1 :K[T], override val _2 :V[T], override val keyHashCode :Int)
	                                       (implicit override val defaults :WhenNoKey[K, V])
		extends Assoc[K, V, T] with BaseNatMap[K, V]
	{
		def this(_1 :K[T], _2 :V[T])(implicit defaults :WhenNoKey[K, V] = throwANoSuchElementException[K]) =
			this(_1, _2, _1.hashCode)

		override def head :Assoc[K, V, T] = this
		override def tail :NatMap[K, V] = NatMap.empty

		override def knownSize = 1
		override def size = 1
		override def isEmpty = false

		override def apply[X](key :K[X]) :V[X] =
			if (key == _1) _2.asInstanceOf[V[X]]
			else default(key)

		override def opt[X](key :K[X]) :Opt[V[X]] =
			if (key == _1) One(_2.asInstanceOf[V[X]]) else None

		override def get[X](key :K[X]) :Option[V[X]] =
			if (key == _1) Some(_2.asInstanceOf[V[X]]) else None

		override def getOrElse[U[A] >: V[A], X](key :K[X], default : => U[X]) :U[X] =
			if (key == _1) _2.asInstanceOf[V[X]] else default


		override def contains(key :K[_]) :Boolean = key == _1

		override def removed(key :K[_]) :NatMap[K, V] =
			if (key == _1) NatMap.empty[K, V] else this

		override def updated[U[A] >: V[A], X](key :K[X], value :U[X]) :NatMap[K, U] =
			if (key == _1)
				if (value == _2) this
				else new Singleton(key, value)
			else
				new SmallNatMap[K, U](Array(this, Assoc(key, value)))

		override def filter(p :Assoc[K, V, _] => Boolean) :NatMap[K, V] =
			if (p(this)) this else NatMap.empty

		override def map[A[_], B[_]](f :Assoc[K, V, _] => Assoc[A, B, _]) :NatMap[A, B] =
			f(this).asMap

		override def flatMap[A[_], B[_]](f :Assoc[K, V, _] => IterableOnce[Assoc[A, B, _]]) :NatMap[A, B] =
			NatMap.from(f(this))

		override def iterator :Iterator[Assoc[K, V, _]] = Iterator.single(this)

		override def keySet :Set[K[_]] = Set(_1)

		override def values :Iterable[V[_]] = Ranking.single(_2)

//		override def toMap[U[A] >: K[A]] :NatMap[U, V] = this.asInstanceOf[NatMap[U, V]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case pair :Assoc[K @unchecked, V @unchecked, _] =>
				_1 == pair._1 && _2 == pair._2
			case map :NatMap[K @unchecked, V @unchecked] if map canEqual this =>
				map.sizeIs == 1 && map.head == this
			case _ => false
		}
		override def hashCode = MurmurHash3.mapHash(Map(_1 -> _2))

		override def toString = "(" + _1 + ", " + _2 + ")"

		private def writeReplace :Serializable = new SerializedAssoc(_1, _2)
	}

	@SerialVersionUID(Ver)
	private class SerializedAssoc[K[_], V[_], T](key :K[T], value :V[T])(implicit defaults :WhenNoKey[K, V])
		extends Serializable
	{
		private def readResolve = new Singleton(key, value)
	}



	/** Maximum size of the array-backed `SmallNatMap`. */
	private final val SmallNatMapCap = 16

	/** A `NatMap` backed by an array, with a size limit of `SmallNatMapCap`. */
	@SerialVersionUID(Ver)
	private class SmallNatMap[K[_], +V[_]]
	                         (private[this] val entries :Array[Assoc[K, V, _]])
	                         (implicit override val defaults :WhenNoKey[K, V] = throwANoSuchElementException[K])
		extends BaseNatMap[K, V]
	{
		override def knownSize = entries.length

		private def indexOf(key :K[_]) :Int = indexOf(key, key.hashCode)

		private def indexOf(key :K[_], hash :Int) :Int = {
			var i = entries.length - 1
			while (i >= 0 && entries(i).keyHashCode != hash && entries(i)._1 != key)
				i -= 1
			i
		}

		override def apply[X](key :K[X]) :V[X] = indexOf(key) match {
			case -1 => defaults(key)
			case  n => entries(n)._2.asInstanceOf[V[X]]
		}
		override def opt[X](key :K[X]) :Opt[V[X]] = indexOf(key) match {
			case -1 => None
			case  n => One(entries(n)._2.asInstanceOf[V[X]])
		}
		override def get[X](key :K[X]) :Option[V[X]] = indexOf(key) match {
			case -1 => None
			case  n => Some(entries(n)._2.asInstanceOf[V[X]])
		}
		override def getOrElse[U[T] >: V[T], X](key :K[X], default : => U[X]) :U[X] = indexOf(key) match {
			case -1 => default
			case  n => entries(n)._2.asInstanceOf[V[X]]
		}

		override def contains(key :K[_]) :Boolean = indexOf(key) >= 0

		override def removed(key :K[_]) :NatMap[K, V] = {
			val i = indexOf(key)
			if (i < 0)
				this
			else entries.length match {
				case 1 => NatMap.empty
				case 2 => (if (i == 0) entries(1) else entries(0)).asMap
				case n =>
					val res = new Array[Assoc[K, V, _]](n - 1)
					var j = 0
					while (j < i) {
						res(j) = entries(j); j += 1
					}
					j += 1
					while (j < n) {
						res(j - 1) = entries(j); j += 1
					}
					new SmallNatMap(res)
			}
		}

		override def updated[U[T] >: V[T], X](key :K[X], value :U[X]) :NatMap[K, U] =
			updated(key, value, null)

		override def +[U[T] >: V[T], X](entry :Assoc[K, U, X]) :NatMap[K, U] =
			updated(entry._1, entry._2, entry)

		private def updated[U[T] >: V[T], X](key :K[X], value :U[X], entry :Assoc[K, U, X]) :NatMap[K, U] = {
			val size = entries.length
			val hash = if (entry == null) key.hashCode else entry.keyHashCode
			val i = indexOf(key, hash)
			if (i >= 0)
				if (entries(i)._2.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef])
					this
				else {
					val res = new Array[Assoc[K, U, _]](entries.length)
					var j = 0
					while (j < i) {
						res(j) = entries(j); j += 1
					}
					res(i) = if (entry != null) entry else new Singleton[K, U, X](key, value, hash)
					j += 1
					while (j < size) {
						res(j) = entries(j); j += 1
					}
					new SmallNatMap[K, U](res)
				}
			else if (entries.length < SmallNatMapCap) {
				val res = new Array[Assoc[K, U, _]](entries.length + 1)
				arraycopy(entries, 0, res, 0, size)
				res(size) = if (entry != null) entry else new Singleton[K, U, X](key, value, hash)
				new SmallNatMap[K, U](res)
			} else {
				var res = Map.empty[K[_], U[_]]
				var i = 0
				while (i < size) {
					val e = entries(i)
					res = res.updated(e._1, e._2)
					i += 1
				}
   				new NaturalizedMap[K, U](res.updated(key, value))
			}
		}

		override def iterator = new ArrayOps(entries).iterator

		override def foreach[U](f :Assoc[K, V, _] => U) :Unit = new ArrayOps(entries).foreach(f)

		override def map[A[_], B[_]](f :Assoc[K, V, _] => Assoc[A, B, _]) :NatMap[A, B] =
			new SmallNatMap[A, B](new ArrayOps(entries).map(f))
	}




	/** Default `NatMap` implementation backed by a regular `Map[K[_], V[_]]`. */
	@SerialVersionUID(Ver)
	private class NaturalizedMap[K[_], +V[_]]
	                            (private val entries :Map[K[_], V[_]] = Map.empty[K[_], V[_]])
	                            (implicit override val defaults :WhenNoKey[K, V] = throwANoSuchElementException[K])
		extends NatMap[K, V] with StrictOptimizedIterableOps[Assoc[K, V, _], Iterable, NatMap[K, V]]
	{
		override def size :Int = entries.size
		override def knownSize :Int = entries.knownSize

		override def contains(key :K[_]) :Boolean = entries.contains(key)

		override def apply[X](key :K[X]) :V[X] = {
			val res = entries.getOrElse(key, null.asInstanceOf[V[_]]).asInstanceOf[V[X]]
			if (res == null) defaults(key) else res
		}
		override def opt[X](key :K[X]) :Opt[V[X]] = entries.get(key).asInstanceOf[Option[V[X]]].toOpt
		override def get[X](key :K[X]) :Option[V[X]] = entries.get(key).asInstanceOf[Option[V[X]]]

		override def getOrElse[U[T] >: V[T], X](key :K[X], default: => U[X]) :U[X] =
			entries.getOrElse(key, default).asInstanceOf[U[X]]


		override def removed(key :K[_]) :NatMap[K, V] = {
			val res = entries.removed(key)
			if (res eq entries) this else new NaturalizedMap[K, V](res)
		}

		override def updated[U[T] >: V[T], X](key :K[X], value :U[X]) :NatMap[K, U] = {
			val res = entries.updated[U[_]](key, value)
			if (res eq entries) this else new NaturalizedMap[K, U](res)
		}

		override def ++[U[T] >: V[T]](entries :IterableOnce[NatMap.Assoc[K, U, _]]) :NatMap[K, U] =
			entries match {
				case _ if entries.iterator.isEmpty => this
				case map :NaturalizedMap[K @unchecked, U @unchecked] =>
					val res = this.entries ++ map.entries
					if (res eq this.entries) this
					else if (res eq map.entries) map
					else new NaturalizedMap[K, U](res)
				case _ =>
					val res = ((this.entries :Map[K[_], U[_]]) /: entries) {
						(acc, entry) => acc.updated(entry._1, entry._2)
					}
					if (res eq this.entries) this
					else new NaturalizedMap[K, U](res)
			}

		override def iterator :Iterator[NatMap.Assoc[K, V, _]] = entries.iterator.map {
			e => Assoc(e._1.asInstanceOf[K[Any]], e._2.asInstanceOf[V[Any]])
		}

		override def keySet :Set[K[_]] = entries.keySet
		override def values :Iterable[V[_]] = entries.values
		override def toMap = entries

		override def withDefault[U[T] >: V[T]](default :K =>: U) :NatMap[K, U] =
			new NaturalizedMap[K, U](entries)(WhenNoKey(default))

		override def equals(that :Any) :Boolean = that match {
			case pair :Assoc[K, V, _] @unchecked => pair == this : @nowarn
			case other :NaturalizedMap[K @unchecked, V @unchecked] => entries == other.entries
			case other :NatMap[K @unchecked, V @unchecked] if other canEqual this =>
				size == other.size && keySet == other.keySet && keySet.forall { k => this(k) == other(k) }
			case _ => false
		}
		override def hashCode :Int = entries.hashCode
	}




	@SerialVersionUID(Ver)
	private class LazyNatMap[K[_], +V[_]](override protected[this] var initializer: () => NatMap[K, V])
		extends BaseNatMap[K, V] with AbstractPure[NatMap[K, V]]
	{
		override def size :Int = definite.size

		override def opt[X](key :K[X]) :Opt[V[X]] = definite.opt(key)
		override def get[X](key :K[X]) :Option[V[X]] = definite.get(key)
		override def apply[X](key :K[X]) :V[X] = definite.apply(key)
		override def contains(key :K[_]) :Boolean = definite.contains(key)

		override def removed(key :K[_]) :NatMap[K, V] =
			? match {
				case Yes(map) => map.removed(key)
				case _ => new LazyNatMap(() => definite.removed(key))
			}
		override def updated[U[T] >: V[T], X](key :K[X], value :U[X]) :NatMap[K, U] =
			? match {
				case Yes(map) => map.updated(key, value)
				case _ => new LazyNatMap(() => definite.updated(key, value))
			}
		override def ++[U[T] >: V[T]](entries :IterableOnce[Assoc[K, U, _]]) :NatMap[K, U] =
			? match {
				case Yes(map) => map ++ entries
				case _ => new LazyNatMap(() => definite ++ entries)
			}
		override def map[A[_], B[_]](f :Assoc[K, V, _] => Assoc[A, B, _]) :NatMap[A, B] =
			? match {
				case Yes(map) => map.map(f)
				case _ => new LazyNatMap(() => definite map f)
			}
		override def flatMap[A[_], B[_]](f :Assoc[K, V, _] => IterableOnce[Assoc[A, B, _]]) :NatMap[A, B] =
			? match {
				case Yes(map) => map.flatMap(f)
				case _ => new LazyNatMap(() => definite flatMap f)
			}
		override def filter(p :Assoc[K, V, _] => Boolean) :NatMap[K, V] =
			? match {
				case Yes(map) => map.filter(p)
				case _ => new LazyNatMap(() => definite filter p)
			}
		override def partition(p :Assoc[K, V, _] => Boolean) :(NatMap[K, V], NatMap[K, V]) =
			? match {
				case Yes(map) => map.partition(p)
				case _ => (new LazyNatMap(() => definite.filter(p)), new LazyNatMap(() => definite.filterNot(p)))
			}
		override def iterator :Iterator[Assoc[K, V, _]] = view.iterator
		override def keySet :collection.Set[K[_]] = definite.keySet //todo: LazySetProxy and other Lazy collections
		override def values :Iterable[V[_]] = definite.values

		override def defaults = definite.defaults

		private def writeReplace = definite
	}




	/** Default builder for `NatMap`. Starts with filling a small array for use with a `SmallNatMap`,
	  * but if the number of elements exceeds `SmallNatMapCap`, switches to building a regular `Map[K[_], V[_]]`
	  * for wrapping in a `NaturalizedMap`.
	  */
	private class NatMapBuilder[K[_], V[_]](implicit default :WhenNoKey[K, V] = throwANoSuchElementException)
		extends ReusableBuilder[Assoc[K, V, _], NatMap[K, V]]
	{
		private[this] var small :Array[Assoc[K, V, _]] = _ //new Array[Assoc[K, V, _]](SmallNatMapCap)
		private[this] var large :Map[K[_], V[_]] = _
		private[this] var size = 0 //-1: large != null

		override def sizeHint(size :Int) :Unit =
			if (this.size == 0 & size > 0) {
				if (this.size <= SmallNatMapCap)
					small = new Array[Assoc[K, V, _]](size)
				else {
					large = Map.empty
					this.size = -1
				}
			}

		override def addOne(elem :Assoc[K, V, _]) :this.type = size match {
			case -1 =>
				large = large.updated(elem._1, elem._2)
				this
			case 0 =>
				if (small == null)
					small = new Array[Assoc[K, V, _]](SmallNatMapCap)
				small(0) = elem
				size = 1
				this
			case _ =>
				var i = size - 1
				while (i >= 0 && { val e = small(i); e.keyHashCode != elem.keyHashCode && e._1 != elem._1 })
					i -= 1
				if (i >= 0)
					small(i) = elem
				else if (size < small.length) {
					small(size) = elem
					size += 1
				} else {
					large = Map.empty
					i = 0
					while (i < size) {
						val e = small(i)
						large = large.updated(e._1, e._2)
						i += 1
					}
					large = large.updated(elem._1, elem._2)
					size = -1
					small = null
				}
				this
		}

		override def result() :NatMap[K, V] = {
			val res =
				if (large != null)
					new NaturalizedMap[K, V](large)
				else if (small != null)
					if (size == small.length)
						new SmallNatMap[K, V](small)
					else
						new SmallNatMap[K, V](Array.copyOf(small, size))
				else empty[K, V]
			clear()
			res
		}

		override def clear() :Unit = { small = null; large = null; size = 0 }
	}


	override def toString = "NatMap"
}

