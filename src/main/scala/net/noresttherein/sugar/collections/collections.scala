package net.noresttherein.sugar

import java.lang.reflect.Field
import java.util.PrimitiveIterator

import scala.annotation.nowarn
import scala.collection.{AnyStepper, BuildFrom, DoubleStepper, EvidenceIterableFactory, Factory, IntStepper, IterableFactory, LongStepper, MapFactory, SeqFactory, SortedMapFactory, Stepper, mutable}
import scala.collection.Stepper.EfficientSplit
import scala.collection.immutable.{ArraySeq, SortedMap}
import scala.reflect.{ClassTag, classTag}

import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}

//implicits
import net.noresttherein.sugar.extensions._




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

	/** Supertype of several types represented in runtime as arrays. This includes a Scala 2 immutable
	  * [[net.noresttherein.sugar.collections.IArray IArray]], and
	  * [[net.noresttherein.sugar.collections.RefArray RefArray]].
	  * Its interface is defined as extension methods in
	  * [[net.noresttherein.sugar.collections.ArrayLike.ArrayLikeExtension ArrayLikeExtension]] and
	  * [[net.noresttherein.sugar.collections.ArrayLike.RefArrayLikeExtension RefArrayLikeExtension]].
	  */
	type ArrayLike[+E] <: AnyRef
//	type RefArrayLike[+E] <: ArrayLike[E]

	/** An erased array with elements `E`. It is represented always as an `Array[Any]` (i.e., `Object[])`,
	  * and arrays of value types store them in their standard box wrappers. The advantage is that the API
	  * does not depend on `ClassTag[E]` being present.
	  * Its interface is defined as extension methods in
	  * [[net.noresttherein.sugar.collections.ArrayLike.ArrayLikeExtension ArrayLikeExtension]]`[RefArray, E]` and
	  * [[net.noresttherein.sugar.collections.ArrayLike.RefArrayLikeExtension RefArrayLikeExtension]]`[RefArray, E]`.
	  */ //it cannot extend or be extended by `Array[E]`, because it would throw ClassCastException in non erased contexts.
	type RefArray[E] <: ArrayLike[E]

	/** An immutable array with elements of type `E`, represented in runtime as some `Array[_ >: E]`.
	  * Its interface is defined as extension methods in
	  * [[net.noresttherein.sugar.collections.IArray.IArrayExtension IArrayExtension]]`[E]`,
	  * [[net.noresttherein.sugar.collections.IArray.GenericIArrayExtension GenericIArrayExtension]]`[E]`,
	  * and specialized variants for standard value types.
	  */
	type IArray[+E] <: ArrayLike[E] // <: ArrayLike[E]


	/** An immutable array with elements of type `E`, represented in runtime as some `Array[_ >: E]`.
	  * Its interface is defined as extension methods in
	  * [[net.noresttherein.sugar.collections.IArray.IArrayExtension IArrayExtension]]`[E]`,
	  * [[net.noresttherein.sugar.collections.IArray.GenericIArrayExtension GenericIArrayExtension]]`[E]`,
	  * and specialized variants for standard value types.
	  */
	type IRefArray[+E] <: ArrayLike[E]
//
//	/** An erased array with elements `E`. It is represented always as an `Array[Any]` (i.e., `Object[])`,
//	  * and arrays of value types store them in their standard box wrappers. The advantage is that the API
//	  * does not depend on `ClassTag[E]` being present.
//	  * Its interface is defined as extension methods in
//	  * [[net.noresttherein.sugar.collections.ArrayLikeExtension ArrayLikeExtension]]`[E, RefArray[E]]` and
//	  * [[net.noresttherein.sugar.collections.RefArray.RefArrayExtension RefArrayExtension]]`[E]`.
//	  */ //it cannot extend or be extended by `Array[E]`, because it would throw ClassCastException in non erased contexts.
//	type RefArray[E]

	private final val PassedArrayClassName = "net.noresttherein.sugar.collections.PassedArray"

	private[this] final val PassedArrayClass :Opt[Class[_]] =
		try Got(Class.forName(PassedArrayClassName + '$')) catch {
			case _ :ClassNotFoundException => Lack
		}
	private[this] final val PassedArrayFactory :Opt[SeqFactory[IndexedSeq]] =
		PassedArrayClass flatMap { cls =>
			try {
				Got(cls.getField("MODULE$").get(null).asInstanceOf[SeqFactory[IndexedSeq]])
			} catch {
				case _ :Exception => Lack
			}
		}

	final val defaultIndexedSeqProperty = "net.noresttherein.sugar.collections.IndexedSeqFactory"
	final val defaultArraySeqProperty   = "net.noresttherein.sugar.collections.ArraySeqFactory"

	private def loadSeqFactory(collectionClassName :String) :SeqFactory[IndexedSeq] = {
		val companionClass = Class.forName(collectionClassName + '$')
		val field = companionClass.getField("MODULE$")
		val res = field.get(null).asInstanceOf[SeqFactory[IndexedSeq]]
		//make sure it really makes immutable.IndexedSeq
		res.from(1::2::Nil)
		res
	}
	private def seqFactoryFromProperty(property :String) :Opt[SeqFactory[IndexedSeq]] =
		Opt(System.getProperty(property)).map { className =>
			try loadSeqFactory(className) catch {
				case e :ClassCastException => throw new ClassCastException(
					property + "=" + className +
						" does not specify the name of a SeqFactory[IndexedSeq] singleton object: " + e.getMessage
				).initCause(e)
				case e :ClassNotFoundException => throw new ClassNotFoundException(
					property + "=" + className + " not available on the class path.", e
				)
				case e :NoSuchFieldException => throw new NoSuchFieldException(
					className + "$ (specified by system property " + property + ") is not a singleton object."
				).initCause(e)
			}
		}
	/** The default `IndexedSeq` implementation used by the library. */
	private[collections] val DefaultIndexedSeq :SeqFactory[IndexedSeq] =
		seqFactoryFromProperty(defaultIndexedSeqProperty) orElse PassedArrayFactory getOrElse IndexedSeq

	/** Switches to [[net.noresttherein.sugar.collections.PassedArray PassedArray]] as the default
	  *  Array to IndexedSeq wrapper, if available on the class path.
	  */
	private[collections] val DefaultArraySeq   :SeqFactory[IndexedSeq] = PassedArrayFactory getOrElse ArraySeq.untagged
		//ArraySeq is a ClassTagBasedSeqFactory, not a SeqFactory, so our procedure won't work.
//		seqFactoryFromProperty(defaultArraySeqProperty) orElse PassedArrayFactory getOrElse ArraySeq.untagged

	/** Checks if the argument is a [[net.noresttherein.sugar.collections.PassedArray PassedArray]] in a manner safe
	  * even if it the class is not on the class path, and retrieves the underlying array and index.
	  * Returning it is an `IArray`, aside from requiring a cast to remove immutability, also guarantees that
	  * the call itself is safe, even if the underlying element type does not match the nominal collection type.
	  * Later operations on it however may fail, and it is the caller's responsibility to make sure that either
	  * the array is of the correct type, or it is used only in a generic context, taking advantage of the polymorphism
	  * illusion granted by the Scala language.
	  */
	private[collections] object PassedArrayWrapper {

		def unapply[A](elems :IterableOnce[A]) :Opt[IArray[A]] = unwrapper(elems).asInstanceOf[Opt[(IArray[A])]]

		private[this] val unwrapper :IterableOnce[_] => Opt[IArray[_]] =
			try {
				val passedArrayPlus = Class.forName("net.noresttherein.sugar.collections.AbstractPassedArray")
				val arrayGetter     = passedArrayPlus.getMethod("elems")
				arrayGetter.setAccessible(true)
				(items :IterableOnce[_]) =>
					if (passedArrayPlus isAssignableFrom items.getClass) {
						val array = arrayGetter.invoke(items).asInstanceOf[IArray[_]]
						if (array.asInstanceOf[Array[_]].length == items.knownSize)
							Got(array)
						else
							Lack
					} else
						Lack
			} catch {
				case _ :Exception => _ => Lack
			}

		/** Similarly to the enclosing object, this is a safe extractor of the underlying array from a `PassedArray`.
		  * This one however succeeds also if the collection is a view of only a section of the underlying array.
		  */
		object Slice {
			def unapply[A](elems :IterableOnce[A]) :Opt[(IArray[A], Int, Int)] =
				unwrapper(elems).asInstanceOf[Opt[(IArray[A], Int, Int)]]

			private[this] val unwrapper :IterableOnce[_] => Opt[(IArray[_], Int, Int)] =
				try {
					val passedArrayPlus = Class.forName("net.noresttherein.sugar.collections.AbstractPassedArray")
					val arrayGetter     = passedArrayPlus.getMethod("elems")
					arrayGetter.setAccessible(true)
					val offsetGetter = passedArrayPlus.getMethod("startIndex")
					offsetGetter.setAccessible(true)
					(items :IterableOnce[_]) =>
						if (passedArrayPlus isAssignableFrom items.getClass) {
							val array  = arrayGetter.invoke(items).asInstanceOf[IArray[_]]
							val offset = offsetGetter.invoke(items).asInstanceOf[Int]
							val length = (items.size : @nowarn)
							Got((array, offset, length - offset))
						} else
							Lack
				} catch {
					case _ :Exception => _ => Lack
				}
		}
	}

	/** Wraps and unwraps `IndexedSeq` instances backed by arrays in a safe manner. Arrays are represented as
	  * [[net.noresttherein.sugar.collections.IArray IArray]] instances to prevent accidental modification and
	  * ensure that the user is aware that an array will be represented as an immutable structure.
	  * Additionally, extractor relies on a `ClassTag` to verify that an array is of a compatible element type
	  * to avoid `ClassCastException`s on later access. However, because they are assumed to always be treated
	  * as immutable, if the expected element type `E` is a reference type, any subclass of `Array[AnyRef]` will be
	  * returned as `IArray[E]`. Covariance here comes from the covariance and immutability of `IArray` itself,
	  * while contravariance is safe because the argument collection's type indicates the type of elements stored,
	  * regardless of the actual element type.
	  */
	private[collections] object WrappedIArray {
		def apply[A](array :IArray[A]) :IndexedSeq[A] = wrapper(array).asInstanceOf[IndexedSeq[A]]

		//Consider: inlining a call (a :IArray[E], i :Int) => a.asInstanceOf[Array[E]](i) will fail
		// with ClassCastException. This can be avoided by implementing each such a cast
		// as a.asInstanceOf[Array[AnyRef]](i).asInstanceOf[E] incurs a minor performance hit.
		def unapply[A :ClassTag](elems :IterableOnce[A]) :Opt[IArray[A]] = {
			val array = elems match {
				case seq :ArraySeq[_]               => Got(seq.unsafeArray)
				case seq :IArrayAsSeq[A @unchecked] => Got(seq.coll)
				case _                              => PassedArrayWrapper.unapply(elems)
			}
			if (array.isDefined && (classTag[A].runtimeClass isAssignableFrom array.get.getClass.getComponentType))
				array.asInstanceOf[Opt[IArray[A]]]
			else
				Lack
		}

		object Slice {
			def unapply[A :ClassTag](elems :IterableOnce[A]) :Opt[(IArray[A], Int, Int)] = {
				val expectedClass = classTag[A].runtimeClass
				elems match {
					case seq :ArraySeq[_] if expectedClass isAssignableFrom seq.unsafeArray.getClass.getComponentType =>
						Got((seq.unsafeArray.asInstanceOf[IArray[A]], 0, seq.unsafeArray.length))
					case seq :IArrayAsSeq[A @unchecked] =>
						Got((seq.coll, 0, seq.coll.length))
					case _ =>
						val passedArray = PassedArrayWrapper.Slice.unapply(elems)
						if (passedArray.isDefined &&
							expectedClass.isAssignableFrom(passedArray.get._1.getClass.getComponentType)
						)
							passedArray
						else
							Lack
				}
			}
		}

		private[this] val wrapper :IArray[_] => IndexedSeq[Any] = {
			val arraySeq = (array :IArray[_]) => ArraySeq.unsafeWrapArray(array.asInstanceOf[Array[Any]])
			PassedArrayFactory match {
				case Got(factory) => try {
					val wrap = factory.getClass.getMethod("from", classOf[Any])
					wrap.setAccessible(true)
					val fun  = (array :IArray[_]) => wrap.invoke(factory, array).asInstanceOf[IndexedSeq[Any]]
					//test that it works
					fun(Array.ofDim[Int](1).asInstanceOf[IArray[Int]])
					fun(Array.ofDim[AnyRef](1).asInstanceOf[IArray[AnyRef]])
					fun(Array.ofDim[Any](1).asInstanceOf[IArray[Any]])
					fun
				} catch  {
					case _ :Exception => arraySeq
				}
				case _ => arraySeq
			}
		}
	}

	/** An unsafe, lower level wrapper and unwrapper of known `collection.IndexedSeq` implementations backed by arrays.
	  * This ignores both the transition between immutability and mutability, as well as the danger
	  * of `ClassCastException`s brought by the common practice of using an `Array[AnyRef]` internally as
	  * an `Array[Any]`, relying on automatic boxing and unboxing of the elements. The type compatibility
	  * must be verified after the extraction by the caller themselves, and it is assumed that the unwrapped arrays
	  * will never be modified, for example only to be wrapped in another immutable class, or to immediately copy
	  * the data. Wrapping an array equals declaration that the caller is the sole owner of it and refrains from
	  * modifying it any further.
	  */
	private[collections] object WrappedArray {
		@inline def apply[A](array :Array[A]) :IndexedSeq[A] =
			WrappedIArray(array.asInstanceOf[IArray[A]])

		def unapply[A](elems :IterableOnce[A]) :Opt[Array[_]] = elems match {
			case seq :ArraySeq[_]                                           => Got(seq.unsafeArray)
			case seq :mutable.ArraySeq[_]                                   => Got(seq.array)
			case seq :ArraySlice[_] if seq.length == seq.unsafeArray.length => Got(seq.unsafeArray)
			case seq :ArrayAsSeq[_]                                         => Got(seq.coll)
			case seq :IArrayAsSeq[_]                                        => Got(seq.coll.asInstanceOf[Array[_]])
			case _ =>
				PassedArrayWrapper.unapply(elems).asInstanceOf[Opt[Array[_]]]
		}

		object Slice {
			def unapply[A](elems :IterableOnce[A]) :Opt[(Array[_], Int, Int)] = elems match {
				case _ :collection.IndexedSeqOps[_, IndexedSeq, IndexedSeq[_]] @unchecked => elems match {
					case seq :ArraySeq[_]         => Got((seq.unsafeArray, 0, seq.unsafeArray.length))
					case seq :mutable.ArraySeq[_] => Got((seq.array, 0, seq.array.length))
					case seq :ArraySlice[_]       => Got((seq.unsafeArray, 0, seq.unsafeArray.length))
					case seq :AbstractArrayAsSeq[_, _]       => Got((seq.array, 0, seq.array.length))
//					case seq :IArrayAsSeq[_]      => Got((seq.coll.asInstanceOf[Array[_], 0, seq.coll.length]))
					case _ => PassedArrayWrapper.Slice.unapply(elems).asInstanceOf[Opt[(Array[_], Int, Int)]]
				}
				case _ => Lack
			}
		}
	}


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

	private val ArrayFactoryClass = scala.Array.toFactory[Any](Array).getClass

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



