package net.noresttherein.sugar.collections

import scala.collection.{EvidenceIterableFactory, Factory, IterableFactory, MapFactory, SortedMapFactory}

import net.noresttherein.sugar.extensions.classNameMethods






/** Implementations of the standard Scala collection `Factory` interface which implement `equals` in terms
  * of the underlying [[scala.collection.IterableFactory IterableFactory]] (or its equivalent).
  * It is also `Serializable`, which additionally enhances its declarative use apart from the operative application.
  */
trait ComparableFactory[-E, +C] extends Factory[E, C] with Serializable {
	def factory :Any

	def canEqual(that :Any) :Boolean = that.isInstanceOf[ComparableFactory[_, _]]

	override def equals(that :Any) :Boolean = that match {
		case other :ComparableFactory[_, _] => (other eq this) || other.factory == factory
		case _ => false
	}
	override def hashCode :Int = factory.hashCode

	override def toString :String = factory.innerClassName
}



object ComparableFactory {
	def apply[E, C[_]](factory :IterableFactory[C]) :ComparableFactory[E, C[E]] = {
		val f = factory
		new ComparableFactory[E, C[E]] {
			override val factory = f
			override def fromSpecific(it :IterableOnce[E]) = factory.from(it)
			override def newBuilder = factory.newBuilder
		}
	}

	def apply[E, C[_], Ev[_]](factory :EvidenceIterableFactory[C, Ev])(implicit ev :Ev[E])
			:ComparableFactory[E, C[E]] =
		new EvidenceFactory[E, C, Ev](factory)

	def apply[K, V, M[_, _]](factory :MapFactory[M]) :ComparableFactory[(K, V), M[K, V]] = {
		val f = factory
		new ComparableFactory[(K, V), M[K, V]] {
			override val factory = f
			override def fromSpecific(it :IterableOnce[(K, V)]) = factory.from(it)
			override def newBuilder = factory.newBuilder
		}
	}

	def apply[K :Ordering, V, M[_, _]](factory :SortedMapFactory[M]) :ComparableFactory[(K, V), M[K, V]] =
		new EvidenceMapFactory[K, V, M](factory)


	implicit def comparableIterableFactory[E, C[_]](factory :IterableFactory[C]) :ComparableFactory[E, C[E]] =
		apply(factory)

	implicit def comparableEvidenceIterableFactory[E, C[_], Ev[_]]
	                                              (factory :EvidenceIterableFactory[C, Ev])(implicit ev :Ev[E])
			:ComparableFactory[E, C[E]] =
		apply(factory)

	implicit def comparableMapFactory[K, V, M[_, _]](factory :MapFactory[M]) :ComparableFactory[(K, V), M[K, V]] =
		apply(factory)

	implicit def comparableSortedMapFactory[K :Ordering, V, M[_, _]](factory :SortedMapFactory[M])
			:ComparableFactory[(K, V), M[K, V]] =
		apply(factory)



	@SerialVersionUID(ver)
	private class EvidenceFactory[E, C[_], Ev[_]](override val factory :EvidenceIterableFactory[C, Ev])
	                                             (implicit val evidence :Ev[E])
		extends ComparableFactory[E, C[E]]
	{
		override def fromSpecific(it :IterableOnce[E]) = factory.from(it)
		override def newBuilder = factory.newBuilder

		override def equals(that :Any) :Boolean = that match {
			case other :EvidenceFactory[_, _, _] =>
				(other eq this) || other.factory == factory && other.evidence == evidence
			case _ => false
		}
		override def hashCode :Int = factory.hashCode * 31 + evidence.hashCode
	}

	@SerialVersionUID(ver)
	private class EvidenceMapFactory[K, V, M[_, _]](override val factory :SortedMapFactory[M])
	                                               (implicit val evidence :Ordering[K])
		extends ComparableFactory[(K, V), M[K, V]]
	{
		override def fromSpecific(it :IterableOnce[(K, V)]) = factory.from(it)
		override def newBuilder = factory.newBuilder

		override def equals(that :Any) :Boolean = that match {
			case other :EvidenceMapFactory[_, _, _] =>
				(other eq this) || other.factory == factory && other.evidence == evidence
			case _ => false
		}
		override def hashCode :Int = factory.hashCode() * 31 + evidence.hashCode
	}
}




