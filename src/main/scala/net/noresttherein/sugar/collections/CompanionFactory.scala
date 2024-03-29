package net.noresttherein.sugar.collections

import java.lang.reflect.Field

import scala.collection.immutable.{ArraySeq, SortedMap}
import scala.collection.{BuildFrom, EvidenceIterableFactory, Factory, IterableFactory, MapFactory, SortedMapFactory, SpecificIterableFactory}
import scala.reflect.ClassTag

import net.noresttherein.sugar.extensions.{OptionExtension, classNameMethods}
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




private object CompanionFactory {

	def unapply[E, C](factory :Factory[E, C]) :Maybe[Any] = sourceCollectionFactory(factory)

	object IterableFactory {
		def unapply[E, C[_]](factory :Factory[E, C[E]]) :Maybe[IterableFactory[C]] = sourceIterableFactory(factory)
	}
	object EvidenceIterableFactory {
		def unapply[E, C[_]](factory :Factory[E, C[E]]) :Maybe[EvidenceIterableFactory[C, E] forSome { type E[v] }] =
			sourceEvidenceIterableFactory(factory)
	}
	object MapFactory {
		def unapply[K, V, M[X, Y] <: Map[X, Y]](factory :Factory[(K, V), M[K, V]]) :Maybe[MapFactory[M]] =
			sourceMapFactory(factory)
	}
	object SortedMapFactory {
		def unapply[K, V, M[X, Y] <: Map[X, Y]](factory :Factory[(K, V), M[K, V]]) :Maybe[SortedMapFactory[M]] =
			sourceSortedMapFactory(factory)
	}

	//Methods for extracting IterableFactory and friends from various objects

	private val IterableFactoryClass               = scala.collection.Iterable.iterableFactory.getClass
	private val IterableFactoryField :Maybe[Field] =
		IterableFactoryClass.getDeclaredFields.find(
			_.getType == classOf[IterableFactory[Iterable]]
		).toMaybe.map { f => f.setAccessible(true); f }

	private val MapFactoryClass               = scala.collection.Map.mapFactory.getClass
	private val MapFactoryField :Maybe[Field] =
		MapFactoryClass.getDeclaredFields.find(
			_.getType == classOf[MapFactory[Map]]
		).toMaybe.map { f => f.setAccessible(true); f }

	private val EvidenceIterableFactoryClass               = scala.collection.mutable.ArraySeq.evidenceIterableFactory[Any].getClass
	private val EvidenceIterableFactoryField :Maybe[Field] =
		EvidenceIterableFactoryClass.getDeclaredFields.find(
			_.getType == classOf[EvidenceIterableFactory[ArraySeq, ClassTag]]
		).toMaybe.map { f => f.setAccessible(true); f }

	private val SortedMapFactoryClass               = scala.collection.immutable.SortedMap.sortedMapFactory[Int, Any].getClass
	private val SortedMapFactoryField :Maybe[Field] =
		SortedMapFactoryClass.getDeclaredFields.find(
			_.getType == classOf[SortedMapFactory[SortedMap]]
		).toMaybe.map { f => f.setAccessible(true); f }

	private val ArrayFactoryClass = scala.Array.toFactory[Any](Array).getClass

	private val BuildFromFactoryClass               = BuildFrom.buildFromString.toFactory("").getClass
	private val BuildFromFactoryField :Maybe[Field] =
		BuildFromFactoryClass.getDeclaredFields.find(
			_.getType == classOf[BuildFrom[_, _, _]]
		).toMaybe.map { f => f.setAccessible(true); f }

	def sourceIterableFactory[E, C[_]](factory :Factory[E, C[E]]) :Maybe[IterableFactory[C]] =
		factory match {
			case ComparableFactory(res :IterableFactory[C @unchecked]) => Yes(res)
			case _ if factory.getClass == IterableFactoryClass =>
				IterableFactoryField.map(_.get(factory).asInstanceOf[IterableFactory[C]])
			case _ => No
		}

	def sourceEvidenceIterableFactory[X, C[_]](factory :Factory[X, C[X]])
			:Maybe[EvidenceIterableFactory[C, E] forSome { type E[v] }] =
		factory match {
			case ComparableFactory(res :EvidenceIterableFactory[C, ClassTag] @unchecked) => Yes(res)
			case _ if factory.getClass == EvidenceIterableFactoryClass =>
				EvidenceIterableFactoryField.map(_.get(factory).asInstanceOf[EvidenceIterableFactory[C, ClassTag]])
			case _ => No
		}

	def sourceMapFactory[K, V, M[X, Y] <: Map[X, Y]](factory :Factory[(K, V), M[K, V]]) :Maybe[MapFactory[M]] =
		(factory :Factory[(K, V), Iterable[(K, V)]]) match {
			case ComparableFactory(res :MapFactory[M @unchecked]) => Yes(res)
			case _ if factory.getClass == MapFactoryClass =>
				MapFactoryField.map(_.get(factory).asInstanceOf[MapFactory[M]])
			case _ => No
		}

	def sourceSortedMapFactory[K, V, M[X, Y] <: Map[X, Y]](factory :Factory[(K, V), M[K, V]])
			:Maybe[SortedMapFactory[M]] =
		(factory :Factory[(K, V), Iterable[(K, V)]]) match {
			case ComparableFactory(res :SortedMapFactory[M @unchecked]) => Yes(res)
			case _ if factory.getClass == SortedMapFactoryClass =>
				SortedMapFactoryField.map(_.get(factory).asInstanceOf[SortedMapFactory[M]])
			case _ => No
		}

	def sourceCollectionFactory[E, T](factory :Factory[E, T]) :Maybe[Any] =
		factory match {
			case comparable :ComparableFactory[_, _] => Yes(comparable.factory)
			case _ :SpecificIterableFactory[_, _] => Yes(factory)
			case _ => factory.getClass match {
				case IterableFactoryClass =>
					IterableFactoryField.map(_.get(factory))//.asInstanceOf[IterableFactory[Iterable]])
				case MapFactoryClass =>
					MapFactoryField.map(_.get(factory))//.asInstanceOf[MapFactory[Map]])
				case EvidenceIterableFactoryClass =>
					EvidenceIterableFactoryField.map(_.get(factory))//.asInstanceOf[EvidenceIterableFactory[ArraySeq, ClassTag]])
				case SortedMapFactoryClass =>
					SortedMapFactoryField.map(_.get(factory))//.asInstanceOf[SortedMapFactory[SortedMap]])
				case ArrayFactoryClass =>
					Yes(Array)
				case BuildFromFactoryClass =>
					BuildFromFactoryField.map(_.get(factory))
				case _ => No
			}
		}

}






/** Implementations of the standard Scala collection `Factory` interface which implement `equals` in terms
  * of identity of the underlying [[scala.collection.IterableFactory IterableFactory]] (or its equivalent).
  * It is also `Serializable`, which additionally enhances its declarative use apart from the operative application.
  */
private trait ComparableFactory[-E, +C] extends Factory[E, C] with Serializable {
	def factory :Any

	override def equals(that :Any) :Boolean = that match {
		case other :ComparableFactory[_, _] => (other eq this) || other.factory == factory
		case _ => false
	}
	def canEqual(that :Any) :Boolean = that.isInstanceOf[ComparableFactory[_, _]]
	override def hashCode :Int = factory.hashCode

	override def toString :String = factory.innerClassName
}



@SerialVersionUID(Ver)
private object ComparableFactory {
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

	def unapply[X, C[A]](factory :Factory[X, C[X]]) :Maybe[Any] = factory match {
		case f :ComparableFactory[_, _] => Yes(f.factory)
		case _ => No
	}

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



	@SerialVersionUID(Ver)
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

	@SerialVersionUID(Ver)
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
