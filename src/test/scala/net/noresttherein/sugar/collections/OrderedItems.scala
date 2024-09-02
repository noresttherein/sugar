package net.noresttherein.sugar.collections

import scala.collection.immutable.{AbstractSeq, AbstractSet, ArraySeq, SeqOps}
import scala.collection.{AbstractIterable, IterableFactory, IterableFactoryDefaults, IterableOps, SeqFactory, View, mutable}
import scala.collection.mutable.Builder
import scala.reflect.ClassTag

import net.noresttherein.sugar.extensions.{BufferExtension, IterableOnceExtension}
import org.scalacheck.util.Buildable
import org.scalacheck.{Arbitrary, Gen, Shrink}




/** A simplistic wrapper over `Seq` used to test how methods work for custom collections.
  * @author Marcin Mościcki
  */
class OrderedItems[+E](override val toSeq :Seq[E])
	extends AbstractIterable[E] with IterableFactoryDefaults[E, OrderedItems] with Serializable
{
	override def knownSize :Int = toSeq.knownSize
	override def iterator :Iterator[E] = toSeq.iterator
	override def iterableFactory :IterableFactory[OrderedItems] = OrderedItems
	override def equals(that :Any) :Boolean = that match {
		case other :OrderedItems[_] => canEqual(that) && other.canEqual(this) && toSeq == other.toSeq
		case _ => false
	}
	def canEqual(that :Any) :Boolean = that.isInstanceOf[OrderedItems[_]]
	override def hashCode :Int = toSeq.hashCode
	override def className = "OrderedItems"
}

object OrderedItems extends IterableFactory[OrderedItems] {
	override def from[A](source :IterableOnce[A]) :OrderedItems[A] = source match {
		case col :OrderedItems[A] => col
		case it  :Iterable[A] => new OrderedItems(it to Seq)
		case _ => new OrderedItems(source.iterator to Seq)
	}

	override def empty[A] :OrderedItems[A] = new OrderedItems(Nil)

	override def newBuilder[A] :Builder[A, OrderedItems[A]] = List.newBuilder[A].mapResult(new OrderedItems(_))


	implicit val orderedItemsGenerator :Arbitrary[OrderedItems[Int]] =
		Arbitrary(Arbitrary.arbitrary[Seq[Int]].map(_ to OrderedItems))

	implicit val orderedItemsShrink :Shrink[OrderedItems[Int]] = Shrink { col :OrderedItems[Int] =>
		implicitly[Shrink[Seq[Int]]].shrink(col.toSeq).map(_ to OrderedItems)
	}
}




class UnorderedItems[+E](override val toSeq :Seq[E])
	extends OrderedItems[E](toSeq) with IterableFactoryDefaults[E, UnorderedItems]
{
	override def iterableFactory :IterableFactory[UnorderedItems] = UnorderedItems

	def counts[U >: E] :Map[U, Int] =
		(Map.empty[U, Int].withDefaultValue(0) /: toSeq)((map, e) => map.updated(e, map(e) + 1))

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :UnorderedItems[_] => counts == other.counts
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[UnorderedItems[_]]
	override def hashCode :Int = counts.hashCode
	override def className = "UnorderedItems"
}

object UnorderedItems extends IterableFactory[UnorderedItems] {
	override def from[A](source :IterableOnce[A]) :UnorderedItems[A] = source match {
		case unordered :UnorderedItems[A] => unordered
		case items  :Iterable[A] => new UnorderedItems(items to Seq)
		case items => new UnorderedItems(items.iterator to Seq)
	}
	override def empty[A] :UnorderedItems[A] = new UnorderedItems(Nil)

	override def newBuilder[A] :Builder[A, UnorderedItems[A]] = Seq.newBuilder[A].mapResult(new UnorderedItems(_))


	implicit val unorderedItemsGenerator :Arbitrary[UnorderedItems[Int]] =
		Arbitrary(Arbitrary.arbitrary[Seq[Int]].map(_ to UnorderedItems))

	implicit val unorderedItemsShrink :Shrink[UnorderedItems[Int]] = Shrink { col :UnorderedItems[Int] =>
		implicitly[Shrink[Seq[Int]]].shrink(col.toSeq).map(_ to UnorderedItems)
	}
}




class UniqueItems[+E] private (override val toSeq :Seq[E])
	extends OrderedItems[E](toSeq) with IterableFactoryDefaults[E, UniqueItems]
{
	def this(items :IterableOnce[E]) = this(items.toBasicOps.toSeq.distinct)

	override def iterableFactory :IterableFactory[UniqueItems] = UniqueItems

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :UniqueItems[_]        => toSeq == other.toSeq
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[UniqueItems[_]]

	override def className = "UniqueItems"
}

case object UniqueItems extends IterableFactory[UniqueItems] {
	override def from[A](source :IterableOnce[A]) :UniqueItems[A] = source match {
		case unique :UniqueItems[A] => unique
		case _                      => new UniqueItems(source.toBasicOps.toSeq)
	}

	override def empty[A] :UniqueItems[A] = new UniqueItems(Nil)

	override def newBuilder[A] :Builder[A, UniqueItems[A]] =
		ArraySeq.untagged.newBuilder[A].mapResult(new UniqueItems(_))

	implicit val uniqueItemsGenerator :Arbitrary[UniqueItems[Int]] =
		Arbitrary(Arbitrary.arbitrary[Set[Int]].map(set => new UniqueItems(set.toSeq)))

	implicit val uniqueItemsShrink :Shrink[UniqueItems[Int]] = Shrink { col :UniqueItems[Int] =>
		implicitly[Shrink[Set[Int]]].shrink(col.toSet).map(set => new UniqueItems(set.toSeq))
	}
}




/** A simplistic wrapper over `Seq` used to test how methods work for custom collections.
  * @author Marcin Mościcki
  */
class AsIterableOnce[+E](val toSeq :Seq[E]) extends IterableOnce[E] {
	override def knownSize :Int = toSeq.knownSize
	override def iterator :Iterator[E] = toSeq.iterator

	override def equals(that :Any) :Boolean = that match {
		case other :AsIterableOnce[_] => canEqual(that) && other.canEqual(this) && toSeq == other.toSeq
		case _ => false
	}

	def canEqual(that :Any) :Boolean = that.isInstanceOf[OrderedItems[_]]

	override def hashCode :Int = toSeq.hashCode
	override def toString :String = toSeq.mkString("IterableOnce(", ", ", ")")
}

object AsIterableOnce extends IterableFactory[AsIterableOnce] {
	override def from[A](source :IterableOnce[A]) :AsIterableOnce[A] = source match {
		case col :AsIterableOnce[A] => col
		case it :Iterable[A] => new AsIterableOnce(it to Seq)
		case _ => new AsIterableOnce(source.iterator to Seq)
	}
	override def empty[A] :AsIterableOnce[A] = new AsIterableOnce(Nil)

	override def newBuilder[A] :Builder[A, AsIterableOnce[A]] = List.newBuilder[A].mapResult(new AsIterableOnce(_))


	implicit val asIterableOnceGenerator :Arbitrary[AsIterableOnce[Int]] =
		Arbitrary(Arbitrary.arbitrary[Seq[Int]].map(_ to AsIterableOnce))

	implicit val asIterableOnceShrink :Shrink[AsIterableOnce[Int]] = Shrink { col :AsIterableOnce[Int] =>
		implicitly[Shrink[Seq[Int]]].shrink(col.toSeq).map(_ to AsIterableOnce)
	}
}




/** A simplistic `Set` implementation preserving insertion order for iteration. */
class OrderedSet[E](underlying :Set[E], override val toSeq :Seq[E]) extends AbstractSet[E] {
	private def this(unique :Seq[E]) = this(unique.toSet, unique)
	def this(items :IterableOnce[E]) =
		this(items.iterator.zipWithIndex.toSeq.reverse.toMap.toVector.sortBy(_._2).map(_._1))

	override def iterableFactory :IterableFactory[OrderedSet] = OrderedSet
	override def incl(elem :E) :Set[E] =
		if (underlying(elem)) this else new OrderedSet(underlying incl elem, toSeq :+ elem)

	override def excl(elem :E) :Set[E] =
		if (underlying(elem)) new OrderedSet(underlying.excl(elem), toSeq.filterNot(_ == elem)) else this

	override def contains(elem :E) :Boolean = underlying.contains(elem)

	override def iterator :Iterator[E] = toSeq.iterator

	override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int = toSeq.copyToArray(xs, start, len)
	override def className = "OrderedSet"
}

object OrderedSet extends IterableFactory[OrderedSet] {
	override def from[A](source :IterableOnce[A]) :OrderedSet[A] = source match {
		case set :OrderedSet[A @unchecked] => set
		case _ if source.knownSize == 0    => empty
		case _                             => (newBuilder[A] ++= source).result()
	}

	override def empty[A] :OrderedSet[A] = new OrderedSet[A](Set.empty, IndexedSeq.empty)

	override def newBuilder[A] :Builder[A, OrderedSet[A]] = new Builder[A, OrderedSet[A]] {
		val seq = IndexedSeq.newBuilder[A]
		var set = Set.empty[A]

		override def result() = new OrderedSet[A](set, seq.result())
		override def clear() :Unit = { seq.clear(); set = Set.empty }

		override def addOne(elem :A) = {
			if (!set.contains(elem)) {
				set = set.incl(elem)
				seq += elem
			}
			this
		}
	}
}




class BoundItems[+E](underlying :collection.Seq[E], max :Int)
	extends Iterable[E] with IterableOps[E, BoundItems, BoundItems[E]]
	   with IterableFactoryDefaults[E, BoundItems]
{
	override def iterator :Iterator[E] = underlying.iterator

	override def iterableFactory :IterableFactory[BoundItems] = BoundItems.iterableFactory(max)
}

object BoundItems  {
	def empty(max :Int) = new BoundItems(Nil, max)
	def iterableFactory[E](max :Int) = new BoundItemsFactory(max)

	class BoundItemsFactory(max :Int) extends IterableFactory[BoundItems] {
		override def from[A](source :IterableOnce[A]) :BoundItems[A] =
			new BoundItems(source.toIterableOnceOps.toList.takeRight(max), max)

		override def empty[A] :BoundItems[A] = new BoundItems(Nil, max)

		override def newBuilder[A] :Builder[A, BoundItems[A]] = new Builder[A, BoundItems[A]] {
			private var queue = mutable.Queue.empty[A]
			override def clear() :Unit = queue = null
			override def result() = new BoundItems(queue, max)

			override def addOne(elem :A) = {
				if (queue.size >= max)
					queue.popHead() += elem
				else
					queue += elem
				this
			}
		}
	}
}




class StrictView[E](underlying :Seq[E]) extends View[E] {
	override def iterator :Iterator[E] = underlying.iterator
	override def toString = underlying.mkString("View(", ", ", ")")
}

object StrictView extends IterableFactory[StrictView] {
	override def from[A](source :IterableOnce[A]) :StrictView[A] = new StrictView(source.toBasicOps.toSeq)

	override def empty[A] :StrictView[A] = new StrictView(Nil)

	override def newBuilder[A] :Builder[A, StrictView[A]] = Seq.newBuilder[A].mapResult(new StrictView(_))
}




/** A `Seq` wrapper which overrides `toString` in order to print its length (in addition to contents). */
class SizedSeq[+E](val underlying :Seq[E])
	extends AbstractSeq[E] with SeqOps[E, SizedSeq, SizedSeq[E]] with IterableFactoryDefaults[E, SizedSeq]
{
	override def knownSize :Int = underlying.knownSize
	override def apply(i :Int) :E = underlying(i)
	override def length :Int = underlying.length
	override def iterator :Iterator[E] = underlying.iterator

	override def iterableFactory :SeqFactory[SizedSeq] = SizedSeq

	protected override def className :String =
		underlying.toString.replaceFirst("\\(", "|" + length + "|(")
}

object SizedSeq extends ProxyIterableFactory[SizedSeq, Seq](Vector) with SeqFactory[SizedSeq] {
	protected override def map[X](impl :Seq[X]) :SizedSeq[X] = new SizedSeq[X](impl)

	implicit def arbitrary[X :Arbitrary :ClassTag] :Arbitrary[SizedSeq[X]] = Arbitrary(
		Gen.oneOf(
		   Arbitrary.arbitrary[Vector[X]],
		   Arbitrary.arbitrary[List[X]],
		   Gen.buildableOf[ArraySeq[X], X](Arbitrary.arbitrary[X]),
		).map(new SizedSeq(_))
	)
	implicit def shrink[X :Shrink] :Shrink[SizedSeq[X]] = Shrink { col :SizedSeq[X] =>
		implicitly[Shrink[Seq[X]]].shrink(col.underlying).map(new SizedSeq(_))
	}
	implicit def buildable[X] :Buildable[X, SizedSeq[X]] = new Buildable[X, SizedSeq[X]] {
		override def builder :Builder[X, SizedSeq[X]] = SizedSeq.newBuilder
	}
	private implicit def buildableArraySeq[X :Arbitrary :ClassTag] :Buildable[X, ArraySeq[X]] =
		new Buildable[X, ArraySeq[X]] {
			override def builder :Builder[X, ArraySeq[X]] = ArraySeq.newBuilder
		}
}
