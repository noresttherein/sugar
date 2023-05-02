package net.noresttherein.sugar

import java.lang.reflect.Field
import java.util.PrimitiveIterator

import scala.annotation.nowarn
import scala.collection.{AnyStepper, ArrayOps, BuildFrom, DoubleStepper, EvidenceIterableFactory, Factory, IndexedSeqView, IntStepper, IterableFactory, IterableOnceOps, IterableOps, LazyZip2, LongStepper, MapFactory, SeqFactory, SortedMapFactory, Stepper, StepperShape, mutable}
import scala.collection.Stepper.EfficientSplit
import scala.collection.immutable.{ArraySeq, SortedMap}
import scala.collection.mutable.{Buffer, Builder}
import scala.reflect.ClassTag

import net.noresttherein.sugar.extensions.{castTypeParam, saferCasting}
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}

//implicits
import net.noresttherein.sugar.extensions.optionExtension




//consider: extending imports
package object collections {
	final val Ver = 1L

	final val ElemTypes = Specializable.AllNumeric


	type JavaIterator[E]    = java.util.Iterator[E]
	type JavaIntIterator    = PrimitiveIterator.OfInt
	type JavaLongIterator   = PrimitiveIterator.OfLong
	type JavaDoubleIterator = PrimitiveIterator.OfDouble

	type SplitStepper[+X]    = Stepper[X] with EfficientSplit
	type AnySplitStepper[+X] = AnyStepper[X] with EfficientSplit
	type IntSplitStepper     = IntStepper with EfficientSplit
	type LongSplitStepper    = LongStepper with EfficientSplit
	type DoubleSplitStepper  = DoubleStepper with EfficientSplit

	type BaseArray[+E] >: Array[_ <: E] <: AnyRef

	/** An immutable array with elements of type `E`, represented in runtime as some `Array[_ >: E]`.
	  * Its interface is defined as extension methods in
	  * [[net.noresttherein.sugar.collections.ArrayLikeExtension ArrayLikeExtension]]`[E, IArray[E]]` and
	  * [[net.noresttherein.sugar.collections.IArray.IArrayExtension IArrayExtension]]`[E]`.
	  */
	type IArray[+E] <: BaseArray[E]
//
//	/** An erased array with elements `E`. It is represented always as an `Array[Any]` (i.e., `Object[])`,
//	  * and arrays of value types store them in their standard box wrappers. The advantage is that the API
//	  * does not depend on `ClassTag[E]` being present.
//	  * Its interface is defined as extension methods in
//	  * [[net.noresttherein.sugar.collections.ArrayLikeExtension ArrayLikeExtension]]`[E, RefArray[E]]` and
//	  * [[net.noresttherein.sugar.collections.RefArray.RefArrayExtension RefArrayExtension]]`[E]`.
//	  */
//	type RefArray[E] <: Array[E]


//	private[collections] val DefaultIndexedSeq :SeqFactory[IndexedSeq] =
//		try {
//			val PassedArray = Class.forName("net.noresttherein.sugar.collections.PassedArray")
//
//		} catch {
//			case _ :Exception => IndexedSeq
//		}

	private val IterableFactoryClass = scala.collection.Iterable.iterableFactory.getClass
	private val IterableFactoryField :Opt[Field] =
		IterableFactoryClass.getDeclaredFields.find(
			_.getType == classOf[IterableFactory[Iterable]]
		).toOpt.map { f => f.setAccessible(true); f }

	private val MapFactoryClass = scala.collection.Map.mapFactory.getClass
	private val MapFactoryField :Opt[Field] =
		MapFactoryClass.getDeclaredFields.find(
			_.getType == classOf[MapFactory[Map]]
		).toOpt.map { f => f.setAccessible(true); f }

	private val EvidenceIterableFactoryClass = scala.collection.mutable.ArraySeq.evidenceIterableFactory[Any].getClass
	private val EvidenceIterableFactoryField :Opt[Field] =
		EvidenceIterableFactoryClass.getDeclaredFields.find(
			_.getType == classOf[EvidenceIterableFactory[ArraySeq, ClassTag]]
		).toOpt.map { f => f.setAccessible(true); f }

	private val SortedMapFactoryClass = scala.collection.immutable.SortedMap.sortedMapFactory[Int, Any].getClass
	private val SortedMapFactoryField :Opt[Field] =
		SortedMapFactoryClass.getDeclaredFields.find(
			_.getType == classOf[SortedMapFactory[SortedMap]]
		).toOpt.map { f => f.setAccessible(true); f }

	private val ArrayFactoryClass = scala.Array.toFactory(Array).getClass

	private val BuildFromFactoryClass = BuildFrom.buildFromString.toFactory("").getClass
	private val BuildFromFactoryField :Opt[Field] =
		BuildFromFactoryClass.getDeclaredFields.find(
			_.getType == classOf[BuildFrom[_, _, _]]
		).toOpt.map { f => f.setAccessible(true); f }

	private[sugar] def sourceIterableFactory[E, C[_]](factory :Factory[E, C[E]]) :Opt[IterableFactory[C]] =
		factory match {
			case ComparableFactory(res :IterableFactory[C @unchecked]) => Got(res)
			case _ if factory.getClass == IterableFactoryClass =>
				IterableFactoryField.map(_.get(factory).asInstanceOf[IterableFactory[C]])
			case _ => Lack
		}

	private[sugar] def sourceEvidenceIterableFactory[X, C[_]](factory :Factory[X, C[X]])
			:Opt[EvidenceIterableFactory[C, E] forSome { type E[v] }] =
		factory match {
			case ComparableFactory(res :EvidenceIterableFactory[C, ClassTag] @unchecked) => Got(res)
			case _ if factory.getClass == EvidenceIterableFactoryClass =>
				EvidenceIterableFactoryField.map(_.get(factory).asInstanceOf[EvidenceIterableFactory[C, ClassTag]])
			case _ => Lack
		}

	private[sugar] def sourceMapFactory[K, V, M[X, Y] <: Map[X, Y]](factory :Factory[(K, V), M[K, V]]) :Opt[MapFactory[M]] =
		(factory :Factory[(K, V), Iterable[(K, V)]]) match {
			case ComparableFactory(res :MapFactory[M @unchecked]) => Got(res)
			case _ if factory.getClass == MapFactoryClass =>
				MapFactoryField.map(_.get(factory).asInstanceOf[MapFactory[M]])
			case _ => Lack
		}

	private[sugar] def sourceSortedMapFactory[K, V, M[X, Y] <: Map[X, Y]](factory :Factory[(K, V), M[K, V]])
			:Opt[SortedMapFactory[M]] =
		(factory :Factory[(K, V), Iterable[(K, V)]]) match {
			case ComparableFactory(res :SortedMapFactory[M @unchecked]) => Got(res)
			case _ if factory.getClass == SortedMapFactoryClass =>
				SortedMapFactoryField.map(_.get(factory).asInstanceOf[SortedMapFactory[M]])
			case _ => Lack
		}

	private[sugar] def sourceCollectionFactory[E, T](factory :Factory[E, T]) :Opt[Any] =
		factory match {
			case comparable :ComparableFactory[_, _] => Got(comparable.factory)
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
					Got(Array)
				case BuildFromFactoryClass =>
					BuildFromFactoryField.map(_.get(factory))
				case _ => Lack
			}
		}

}



