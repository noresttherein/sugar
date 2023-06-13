package net.noresttherein.sugar.collections

import scala.collection.{AbstractIterable, IterableFactory, IterableFactoryDefaults}
import scala.collection.mutable.Builder

import org.scalacheck.{Arbitrary, Shrink}




/** A simplistic wrapper over `Seq` used to test how methods work for custom collections.
  * @author Marcin Mościcki
  */
class OrderedItems[+E](override val toSeq :Seq[E])
	extends AbstractIterable[E] with IterableFactoryDefaults[E, OrderedItems]
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
	override def iterableFactory = UnorderedItems

	def counts[U >: E] :Map[U, Int] =
		(Map.empty[U, Int].withDefaultValue(0) /: toSeq)((map, e) => map.updated(e, map(e) + 1))

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :UnorderedItems[_] => counts == other.counts
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[UnorderedItems[_]]
	override def hashCode = counts.hashCode
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