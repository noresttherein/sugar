package net.noresttherein.sugar.collections

import java.util.Arrays

import scala.annotation.{implicitNotFound, nowarn, tailrec}
import scala.collection.{AnyStepper, BufferedIterator, DoubleStepper, EvidenceIterableFactory, Factory, IntStepper, IterableFactory, IterableOnce, IterableOps, LinearSeq, LongStepper, MapFactory, SortedMapFactory, Stepper, StepperShape, View, mutable}
import scala.collection.Stepper.EfficientSplit
import scala.collection.immutable.{ArraySeq, IndexedSeqDefaults, MapOps, SeqOps, SetOps}
import scala.collection.immutable.IndexedSeqDefaults.defaultApplyPreferredMaxLength
import scala.collection.generic.IsIterableOnce
import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.reflect.{ClassTag, classTag}
import scala.util.Random

import net.noresttherein.sugar.JavaTypes.{JIterator, JStringBuilder}
import net.noresttherein.sugar.collections.extensions.{ArrayExtension, ArrayObjectExtension, BuilderExtension, FactoryExtension, IndexedSeqExtension, IterableExtension, IterableFactoryExtension, IterableOnceExtension, IteratorObjectExtension, JavaIteratorExtension, JavaStringBuilderExtension, MapExtension, MapObjectExtension, SeqExtension, SeqFactoryExtension, SetFactoryExtension, StepperExtension, StepperObjectExtension}
import net.noresttherein.sugar.extensions.{castTypeParam, saferCasting}
import net.noresttherein.sugar.raise
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




//I don't think this introduces priority as they need to be imported by name
private[collections] sealed trait extensionsLowPriority extends Any {
	/** Adds a `foldWhile` method to any `Iterable` which implement a variant of `fold` operation with a break condition. */
	@inline implicit final def isIterableOnceExtension[C](self :C)(implicit iterable :IsIterableOnce[C])
			:IterableOnceExtension[iterable.A] =
		new IterableOnceExtension[iterable.A](iterable(self))

}


/** Extension methods for various collection types as well as collection companion objects. */
trait extensions
	extends Any with extensionsLowPriority with IArray.extensions with RefArray.extensions with IRefArray.extensions
{
	/** Adds various additional folding methods with a break condition to any `Iterable`. */
	@inline implicit final def iterableOnceExtension[E](self :IterableOnce[E]) :IterableOnceExtension[E] =
		new IterableOnceExtension[E](self)

	/** Adds various additional folding methods with a break condition to any `Array`. */
	@inline implicit final def arrayAsIterableOnceExtension[E](self :Array[E]) :IterableOnceExtension[E] =
		new IterableOnceExtension[E](new ArrayAsSeq(self))

	/** Adds various additional folding methods with a break condition to any `IArray`. */
	@inline implicit final def immutableArrayAsIterableOnceExtension[E](self :IArray[E]) :IterableOnceExtension[E] =
		new IterableOnceExtension[E](new IArrayAsSeq(self))

	/** Adds various additional folding methods with a break condition to any `RefArray`. */
	@inline implicit final def refArrayAsIterableOnceExtension[E](self :RefArray[E]) :IterableOnceExtension[E] =
		new IterableOnceExtension(new RefArrayAsSeq(self))

	/** Adds various additional folding methods with a break condition to any `RefArray`. */
	@inline implicit final def immutableRefArrayAsIterableOnceExtension[E](self :IRefArray[E]) :IterableOnceExtension[E] =
		new IterableOnceExtension(new IRefArrayAsSeq(self))

	/** Adds various additional folding methods with a break condition to any `String`. */
	@inline implicit final def stringAsIterableOnceExtension(self :String) :IterableOnceExtension[Char] =
		new IterableOnceExtension[Char](self)


	/** Adds various methods for mapping/flatMapping collections to any collection of the standard library framework.
	  * These either pass along additional state, or have a break condition. Roughly equivalent to working
	  * with `toLazyList.scan`, but cleaner and more efficient.
	  */
	@inline implicit final def iterableExtension[E, CC[X] <: Iterable[X], C]
	                                            (self :IterableOps[E, CC, C]) :IterableExtension[E, CC, C] =
		new IterableExtension[E, CC, C](self)

	/** Adds various methods for mapping/flatMapping collections to any `Array`.
	  * These either pass along additional state, or have a break condition. Roughly equivalent to working
	  * with `toLazyList.scan`, but cleaner and more efficient.
	  */
	@inline implicit final def arrayAsIterableExtension[E](self :Array[E]) :IterableExtension[E, Array, Array[E]] =
		new IterableExtension[E, Array, Array[E]](new ArrayAsSeq(self))

	/** Adds various methods for mapping/flatMapping collections to any `IArray`.
	  * These either pass along additional state, or have a break condition. Roughly equivalent to working
	  * with `toLazyList.scan`, but cleaner and more efficient.
	  */
	@inline implicit final def immutableArrayAsIterableExtension[E](self :IArray[E])
			:IterableExtension[E, IArray, IArray[E]] =
		new IterableExtension[E, IArray, IArray[E]](new IArrayAsSeq(self))

	/** Adds various methods for mapping/flatMapping collections to any `RefArray`.
	  * These either pass along additional state, or have a break condition. Roughly equivalent to working
	  * with `toLazyList.scan`, but cleaner and more efficient.
	  */
	@inline implicit final def refArrayAsIterableExtension[E](self :RefArray[E])
			:IterableExtension[E, RefArray, RefArray[E]] =
		new IterableExtension[E, RefArray, RefArray[E]](new RefArrayAsSeq(self))

	/** Adds various methods for mapping/flatMapping collections to any `IRefArray`.
	  * These either pass along additional state, or have a break condition. Roughly equivalent to working
	  * with `toLazyList.scan`, but cleaner and more efficient.
	  */
	@inline implicit final def immutableRefArrayAsIterableExtension[E](self :IRefArray[E])
			:IterableExtension[E, IRefArray, IRefArray[E]] =
		new IterableExtension[E, IRefArray, IRefArray[E]](new IRefArrayAsSeq(self))

	/** Adds various methods for mapping/flatMapping collections to any `String`.
	  * These either pass along additional state, or have a break condition. Roughly equivalent to working
	  * with `toLazyList.scan`, but cleaner and more efficient.
	  */
    @inline implicit final def stringAsIterableExtension(self :String) :IterableExtension[Char, IndexedSeq, String] =
		new IterableExtension(new StringAsSeq(self))


	/** Alternative, safer implementations of [[scala.collection.SeqOps.indexOf indexOf]] for [[Seq]],
	  * which do not return a negative index when the element is not found.
	  */
	@inline implicit final def seqExtension[E, CC[A], C]
	                                       (self :scala.collection.SeqOps[E, CC, C]) :SeqExtension[E, CC, C] =
		new SeqExtension[E, CC, C](self)

	/** Alternative, safer implementations of [[scala.collection.SeqOps.indexOf indexOf]] for [[Array]],
	  * which do not return a negative index when the element is not found.
	  */
	@inline implicit final def arrayAsSeqExtension[E](self :Array[E]) :SeqExtension[E, Array, Array[E]] =
		new SeqExtension[E, Array, Array[E]](new ArrayAsSeq(self))

	/** Alternative, safer implementations of [[scala.collection.SeqOps.indexOf indexOf]] for immutable arrays,
	  * which do not return a negative index when the element is not found.
	  */
   @inline implicit final def immutableArrayAsSeqExtension[E](self :IArray[E]) :SeqExtension[E, IArray, IArray[E]] =
		new SeqExtension[E, IArray, IArray[E]](new IArrayAsSeq(self))

	/** Alternative, safer implementations of [[scala.collection.SeqOps.indexOf indexOf]] for reference arrays,
	  * which do not return a negative index when the element is not found.
	  */
   @inline implicit final def refArrayAsSeqExtension[E](self :RefArray[E]) :SeqExtension[E, RefArray, RefArray[E]] =
		new SeqExtension[E, RefArray, RefArray[E]](new RefArrayAsSeq(self))

	/** Alternative, safer implementations of [[scala.collection.SeqOps.indexOf indexOf]] for immutable reference arrays,
	  * which do not return a negative index when the element is not found.
	  */
   @inline implicit final def immutableRefArrayAsSeqExtension[E](self :IRefArray[E])
            :SeqExtension[E, IRefArray, IRefArray[E]] =
		new SeqExtension[E, IRefArray, IRefArray[E]](new IRefArrayAsSeq(self))

	/** Alternative, safer implementations of [[scala.collection.SeqOps.indexOf indexOf]] for strings,
	  * which do not return a negative index when the element is not found.
	  */
	@inline implicit final def stringAsSeqExtension(self :String) :SeqExtension[Char, IndexedSeq, String] =
		new SeqExtension(new StringAsSeq(self))


	/** Operations on suffixes of a sequence and binary search methods on sorted sequences. */
	@inline implicit final def indexedSeqExtension[E, CC[A], C](self :collection.IndexedSeqOps[E, CC, C])
			:IndexedSeqExtension[E, CC, C] =
		new IndexedSeqExtension[E, CC, C](self)

	/** Operations on suffixes of a sequence and binary search methods on sorted arrays. */
	@inline implicit final def arrayAsIndexedSeqExtension[E](self :Array[E]) :IndexedSeqExtension[E, Array, Array[E]] =
		new IndexedSeqExtension[E, Array, Array[E]](new ArrayAsSeq(self))

	/** Operations on suffixes of a sequence and binary search methods on sorted immutable arrays. */
	@inline implicit final def immutableArrayAsIndexedSeqExtension[E](self :IArray[E])
			:IndexedSeqExtension[E, IArray, IArray[E]] =
		new IndexedSeqExtension[E, IArray, IArray[E]](new IArrayAsSeq(self))

	/** Operations on suffixes of a sequence and binary search methods on sorted reference arrays. */
	@inline implicit final def refArrayAsIndexedSeqExtension[E](self :RefArray[E])
			:IndexedSeqExtension[E, RefArray, RefArray[E]] =
		new IndexedSeqExtension(new RefArrayAsSeq(self))

	/** Operations on suffixes of a sequence and binary search methods on sorted immutable reference arrays. */
	@inline implicit final def immutableRefArrayAsIndexedSeqExtension[E](self :IRefArray[E])
			:IndexedSeqExtension[E, IRefArray, IRefArray[E]] =
		new IndexedSeqExtension(new IRefArrayAsSeq(self))

	/** Operations on suffixes of a string. */
	@inline implicit final def stringAsIndexedSeqExtension(self :String) :IndexedSeqExtension[Char, IndexedSeq, String] =
		new IndexedSeqExtension(new StringAsSeq(self))


	/** Binary search methods for sorted arrays. */
	@inline implicit final def arrayExtension[E](self :Array[E]) :ArrayExtension[E] =
		new ArrayExtension(self)

	/** Binary search methods for sorted immutable arrays. */
	@inline implicit final def immutableArrayAsArrayExtension[E](self :IArray[E]) :ArrayExtension[E] =
		new ArrayExtension(self.asInstanceOf[Array[E]])

	/** Binary search methods for sorted reference arrays. */
	@inline implicit final def refArrayAsArrayExtension[E](self :RefArray[E]) :ArrayExtension[E] =
		new ArrayExtension(self.asInstanceOf[Array[E]])

	/** Binary search methods for sorted immutable reference arrays. */
	@inline implicit final def immutableRefArrayAsArrayExtension[E](self :IRefArray[E]) :ArrayExtension[E] =
		new ArrayExtension(self.asInstanceOf[Array[E]])

//	/** Main extension methods for `IArray`, which do not require (manual) specialization. */
//	@inline implicit def IArrayExtension[E](array :IArray[E]) :IArrayExtension[E] =
//		new IArrayExtension(array.asInstanceOf[Array[E]])

	/** Adapts (casts) a `ClassTag` for an `Array[E]` to an `IArray[E]`. */
	@inline implicit def IArrayClassTag[E](implicit tag :ClassTag[Array[E]]) :ClassTag[IArray[E]] =
		tag.castParam[IArray[E]]

	@inline implicit def RefArrayClassTag[E] :ClassTag[RefArray[E]] = classTag[Array[AnyRef]].castParam[RefArray[E]]

	@inline implicit def IRefArrayClassTag[E] :ClassTag[IRefArray[E]] = classTag[Array[AnyRef]].castParam[IRefArray[E]]


	/** An [[net.noresttherein.sugar.collections.extensions.MapExtension.updatedIfAbsent updatedIfAbsent]] method
	  * for any `Map`.
	  */
	@inline implicit final def mapExtension[K, V, M[A, +B] <: MapOps[A, B, M, M[A, B]]]
	                                       (self :M[K, V]) :MapExtension[K, V, M] =
		new MapExtension(self)

	/** Adds a `++` method to any `Stepper` type. */
	@inline implicit final def stepperExtension[S <: Stepper[_]](self :S) :StepperExtension[S] =
		new StepperExtension[S](self)

//	@inline implicit final def efficientSplitStepperExtension[S <: Stepper[_]](self :S with EfficientSplit) =
//		new StepperExtension[S with EfficientSplit](self)

	/** Adds a `++` method to any standard `java.util.Iterator` subclass. */
	@inline implicit final def javaIteratorExtension[I <: JIterator[_]](self :I) :JavaIteratorExtension[I] =
		new JavaIteratorExtension[I](self)

	/** Extension factory methods for single element immutable [[collection.immutable.Seq Seq]] subtypes' companions. */
	@inline implicit final def seqFactoryExtension[C[X] <: SeqOps[X, C, C[X]]]
	                                              (self :IterableFactory[C]) :SeqFactoryExtension[C] =
		new SeqFactoryExtension(self)

	/** Extension factory methods for single element immutable [[collection.immutable.Set Set]] subtypes' companions. */
	@inline implicit final def setFactoryExtension[C[X] <: SetOps[X, C, C[X]]]
	                                              (self :IterableFactory[C]) :SetFactoryExtension[C] =
		new SetFactoryExtension(self)

	/** Additional, higher level factory methods of any [[Iterable]] type `C[_]` as extensions of its companion
	  * [[scala.collection.IterableFactory IterableFactory]]`[C]`.
	  * Adds methods [[net.noresttherein.sugar.collections.extensions.IterableFactoryExtension.expand expand]]
	  * and [[net.noresttherein.sugar.collections.extensions.IterableFactoryExtension.generate generate]]
	  * to any [[scala.collection.IterableFactory IterableFactory]].
	  */
	@inline implicit final def iterableFactoryExtension[C[_]](self :IterableFactory[C]) :IterableFactoryExtension[C] =
		new IterableFactoryExtension[C](self)

//	/** Additional extension methods for companions of mutable and immutable `IndexedSeq` implementations,
//	  * inclding binary search in sorted sequences.
//	  */
//	@inline implicit final def indexedSeqFactoryExtension[C[X] <: collection.IndexedSeqOps[X, C, C[X]]]
//	                                              (self :IterableFactory[C]) :IndexedSeqFactoryExtension[C] =
//		new IndexedSeqFactoryExtension[C](self)

	/** Extension factory methods creating single and two element [[Map Map]]s. */
	@inline implicit final def mapObjectExtension(self :Map.type) :MapObjectExtension = new MapObjectExtension {}

	/** Adds extra extension methods to the `Array` object. */
	@inline implicit final def arrayObjectExtension(self :Array.type) :ArrayObjectExtension =
		new ArrayObjectExtension {}

//	@inline implicit final def arrayToEvidenceIterableFactory(self :Array.type) :ArrayIterableFactory.type =
//		ArrayIterableFactory

	/** Adds several extensions methods to `Stepper` object for creating small steppers. */
	@inline implicit final def stepperObjectExtension(self :Stepper.type) :StepperObjectExtension =
		new StepperObjectExtension {}

	/** Adds a [[net.noresttherein.sugar.collections.extensions.IteratorObjectExtension.double double]] factory method
	  * for two element iterators to object `Iterator`.
	  */
	@inline implicit final def iteratorObjectExtension(self :Iterator.type) :IteratorObjectExtension =
		new IteratorObjectExtension {}


//	/** Extension methods for object [[scala.collection.EvidenceIterableFactory$ EvidenceIterableFactory]]. */
//	@inline implicit final def iterableFactoryObjectExtension(self :IterableFactory.type)
//			:IterableFactoryObjectExtension =
//		new IterableFactoryObjectExtension {}
//
//	/** Extension methods for object [[scala.collection.EvidenceIterableFactory$ EvidenceIterableFactory]]. */
//	@inline implicit final def evidenceIterableFactoryObjectExtension(self :EvidenceIterableFactory.type)
//			:EvidenceIterableFactoryObjectExtension =
//		new EvidenceIterableFactoryObjectExtension {}
//
//	/** Extension factory methods creating single and two element [[Map Map]]s. */
//	@inline implicit final def mapFactoryObjectExtension(self :MapFactory.type) :MapFactoryObjectExtension =
//		new MapFactoryObjectExtension {}
//
//	/** Extension factory methods creating single and two element [[Map Map]]s. */
//	@inline implicit final def sortedMapFactoryObjectExtension(self :SortedMapFactory.type)
//			:SortedMapFactoryObjectExtension =
//		new SortedMapFactoryObjectExtension {}


	/** An extension method [[net.noresttherein.sugar.collections.extensions.BuilderExtension.mapInput mapElements]]
	  * which adapts the builder to a new element type.
	  */
	@inline implicit final def builderExtension[E, C](self :Builder[E, C]) :BuilderExtension[E, C] =
		new BuilderExtension(self)

	/** Adds Scala [[scala.collection.mutable.Growable Growable]] and [[scala.collection.mutable.Builder Builder]]
	  * methods as inlined delegates to the Java [[java.lang.StringBuilder StringBuilder]].
	  * While it essentially duplicates the functionality of standard Scala
	  * [[scala.collection.mutable.StringBuilder StringBuilder]], the wrapper is not intended to be as an object
	  * or referred to directly by the application, but rather a provider of extension methods, and returns always
	  * the original `StringBuilder`. As methods are inlined, this incurs neither the penalty of creating an additional
	  * object, nor of delegating individual calls, at least with default compiler optimisations on.
	  */
	@inline implicit final def javaStringBuilderExtension(self :JStringBuilder) :JavaStringBuilderExtension =
		new JavaStringBuilderExtension(self)

	/** Extension methods for [[scala.collection.Factory Factory]] returning the collection companion object
	  * which created it.
	  */
	@inline implicit final def factoryExtension[E, C](self :Factory[E, C]) :FactoryExtension[E, C] =
		new FactoryExtension(self)
}




@SerialVersionUID(Ver)
object extensions extends extensions {

	/** A syntactic wrapper for collections, injecting methods implementing
	  * 'breakable' folding and reducing, that is folding only some prefix/suffix of the collection
	  * based on some predicate or other termination condition.
	  *
	  * @param self any collection to fold.
	  * @tparam T element type of this collection.
	  */ //todo: use Pill and Potential
	class IterableOnceExtension[T] private[collections](private val self :IterableOnce[T]) extends AnyVal {
		/** Creates a Java [[java.util.Iterator Iterator]] of a proper specialization for type `T`
		  * (`Int`, `Long`, `Double`). If the underlying collection provides a specialized (non boxing)
		  * [[scala.collection.Stepper Stepper]], then the returned iterator will not box value types.
		  */
		def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[T, I]) :I = self match {
			case sugared :SugaredIterable[T]         => sugared.jiterator
			case empty :Iterable[_] if empty.isEmpty => JavaIterator()
			case _ :ArraySeq[T] | _ :mutable.ArraySeq[T] | _ :ArrayBuffer[T] =>
				self.stepper(shape.stepperShape).javaIterator.asInstanceOf[I]
			case seq :collection.IndexedSeq[T]       => JavaIterator.over(seq)
			case it :Iterator[_] if !it.hasNext      => JavaIterator()
			case _                                   => self.stepper(shape.stepperShape).javaIterator.asInstanceOf[I]
		}

		/** Equivalent to standard `fold`, but the folding functions takes the number of already folded elements
		  * (i.e., how many times the function has been called, or the position of the element in the collection's
		  * iteration order).
		  */
		def foldWithIndex[A >: T](start :A)(op :(A, A, Int) => A) :A = foldLeftWithIndex(start)(op)

		/** Equivalent to standard `foldLeft`, but the folding functions takes the number of already folded elements
		  * (i.e., how many times the function has been called, or the position of the element in the collection's
		  * iteration order).
		  */
		def foldLeftWithIndex[A](start :A)(op :(A, T, Int) => A) :A = self match {
			case empty :Iterable[T] if empty.isEmpty => start
			case seq   :collection.IndexedSeq[T] if seq.length <= defaultApplyPreferredMaxLength =>
				var i = 0; val end = seq.length
				var acc = start
				while (i < end) {
					acc = op(acc, seq(i), i)
					i += 1
				}
				acc
			case rank  :Ranking[T] if rank.size <= defaultApplyPreferredMaxLength =>
				var i = 0; val end = rank.length
				var acc = start
				while (i < end) {
					acc = op(acc, rank(i), i)
					i += 1
				}
				acc
			case _ =>
				val it = self.iterator; var i = 0
				var acc = start
				while (it.hasNext) {
					acc = op(acc, it.next(), i)
					i += 1
				}
				acc
		}

		/** Folds this collection until the folded value satisfies the condition `pred`.
		  * Function `op` is applied recursively to the elements of this collection and its previous results,
		  * in an undetermined order, until it returns a value for which `pred` is `true`.
		  * If `pred(start)`, `start` is returned immediately.
		  * The method returns returns the first value returned by recursive application of `op`
		  * for which `pred` is `true`, or the result of folding the whole collection if no such value is generated.
		  * If `pred(start)` or this collection is empty, then `start` is returned immediately,
		  * without ever calling `op`.
		  *
		  * This method is not equivalent
		  * to `this.`[[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldWhile foldWhile]]`(start)(!pred(_))(op)`,
		  * as the latter will return the last element for which `pred` was not satisfied, while this method
		  * applies `op` once more.
		  * @param start the initial value, used as the result if the collection is empty.
		  * @param pred  a stop condition for folding, checked first for `start`, and then for every result
		  *              of function `op`. It should be an attractor state of `op`: if `pred(a)`,
		  *              then `pred(op(a, ai))` and `pred(op(ai, a))` should also be true for any `ai`
		  *              in this collection, or the result is undefined.
		  * @param op    a folding function.
		  * @return This implementation delegates
		  *         to [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldLeftUntil]]`(start)(pred)(op)`.
		  */
		def foldUntil[A >: T](start :A)(pred :A => Boolean)(op :(A, A) => A) :A =
			foldLeftUntil(start)(pred)(op)

		/** Folds this collection until the folded value satisfies the condition `pred`.
		  * Function `op` is applied recursively to the elements of this collection and its previous results,
		  * in an undetermined order, until it returns a value for which `pred` is `true`.
		  * The method returns the first value returned by recursive application of `op` for which `pred` is `true`.
		  *    1. If `pred(start)`, then return `Some(start)` immediately;
		  *    1. If this collection is empty, return `None`;
		  *    1. Otherwise, if `op` ever returns a value for which `pred` is true, return it in `Some`;
		  *    1. If, after folding the whole collection, the predicate is still not satisfied, then return `None`.
		  *
		  * It is not equivalent
		  * to `this.`[[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldWhileOption foldWhileOption]]`(start)(!pred(_))(op)`,
		  * as the latter will return the last element for which `pred` was not satisfied, while this method
		  * applies `op` once more.
		  * @param start the initial value, used as the result if the collection is empty.
		  * @param pred  a stop condition for folding, checked first for `start`, and then for every result
		  *              of function `op`. It should be an attractor state of `op`: if `pred(a)`,
		  *              then `pred(op(a, ai))` and `pred(op(ai, a))` should also be true for any `ai`
		  *              in this collection, or the result is undefined.
		  * @param op    a folding function.
		  * @return This implementation delegates to
		  *         [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldLeftUntilOption foldLeftUntilOption]]`(start)(pred)(op)`
		  */
		def foldUntilOption[A >: T](start :A)(pred :A => Boolean)(op :(A, A) => A) :Option[A] =
			foldLeftUntilOption(start)(pred)(op)

		/** Folds this collection until the folded value satisfies the condition `pred`.
		  * The method applies `op` recursively to the elements of this collection and its previous results,
		  * and returns either `Right(x)` for some intermediate result of `this.fold(start)(op)` if `pred(x)`,
		  * or `Left(this.fold(start)(op))` if `!pred(this.fold(start)(op))`.
		  * Function `op` is applied to the elements of this collection and its previous results,
		  * in an undetermined order, until it returns a value for which `pred` is `true`.
		  * The method returns the first value returned by recursive application of `op` for which `pred` is `true`.
		  *    1. If `pred(start)`, then return `Right(start)` immediately;
		  *    1. If this collection is empty, return `Left(start)`;
		  *    1. Otherwise, if `op` ever returns a value `a`, such that `pred(a)`, return `Right(a)`;
		  *    1. If, after folding the whole collection, the predicate is still not satisfied, then return `Left(a)`,
		  *       where `a` is the last value computed by `op`.
		  *
		  * It is not equivalent
		  * to `this.`[[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldWhileEither foldWhileEither]]`(start)(!pred(_))(op)`,
		  * as the latter will return the last element for which `pred` was not satisfied, while this method
		  * applies `op` once more.
		  * @param start the initial value, used as the result if the collection is empty.
		  * @param pred  a stop condition for folding, checked first for `start`, and then for every result
		  *              of function `op`. It should be an attractor state of `op`: if `pred(a)`,
		  *              then `pred(op(a, ai))` and `pred(op(ai, a))` should also be true for any `ai`
		  *              in this collection, or the result is undefined.
		  * @param op    a folding function.
		  * @return This implementation delegates
		  *         to [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldLeftUntilEither]]`(start)(pred)(op)`
		  */
		def foldUntilEither[A >: T](start :A)(pred :A => Boolean)(op :(A, A) => A) :Either[A, A] =
			foldLeftUntilEither(start)(pred)(op)


		private def foldLeftUntilAndReturn[A, X](start :A)(pred :A => Boolean)(op :(A, T) => A)
		                                        (ifNotFound :A => X, ifFound :A => X) :X =
			self match {
				case _ if pred(start) => ifFound(start)
				case it :Iterable[_] if it.isEmpty => ifNotFound(start)
				case _ =>
					var item = start; var found = false; val i = self.iterator
					while (i.hasNext && !{ item = op(item, i.next()); found = pred(item); found })
						{}
					if (found) ifFound(item) else ifNotFound(item)
			}

		/** Folds this collection from left to right by applying `op` to its elements and the previously returned value
		  * until the folded value satisfies the condition `pred`.
		  * Function `op` is applied recursively to the elements of this collection in its iteration order.
		  * If `pred(start)`, `start` is returned immediately.
		  * The method returns returns the first value returned by recursive application of `op`
		  * for which `pred` is `true`, or the result of folding the whole collection if no such value is generated.
		  * If `pred(start)` or this collection is empty, then `start` is returned immediately,
		  * without ever calling `op`.
		  *
		  * This method is a slightly more clear and efficient equivalent of
		  * {{{
		  *     toLazyList.scanLeft(start)(op).span(!pred(_)) match {
		  *         case (folded, Seq()) => folded.last
		  *         case (_, Seq(h, _*)) => h
		  *     }
		  * }}}
		  * It is not equivalent to
		  * [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldLeftWhile foldLeftWhile]]`(start)(!pred(_))(op)`,
		  * as the latter will return the last element for which `pred` was not satisfied, while this method
		  * applies `op` once more.
		  * @param start the initial value, used as the result if the collection is empty.
		  * @param pred  a stop condition for folding, checked first for `start`, and then for every result
		  *              of function `op`.
		  * @param op    a folding function.
		  * @return the first element of `scanLeft(start)(op)` for which `pred` is `true`,
		  *         or the result of folding the entire collection if no such value is generated.
		  */
		def foldLeftUntil[A](start :A)(pred :A => Boolean)(op :(A, T) => A) :A =
			foldLeftUntilAndReturn(start)(pred)(op)(identity, identity)

		/** Folds this collection from left to right by applying `op` to its elements and the previously returned value
		  * until the folded value satisfies the condition `pred`.
		  * The method applies `op` recursively to the elements of this collection in the iteration order,
		  * and returns either `Some(x)` for some intermediate result of `this.foldLeft(start)(op)` if `pred(x)`,
		  * or `None` if the predicate has not been satisfied after folding the whole collection.
		  *
		  * This method is a slightly more efficient and clear equivalent of
		  * {{{
		  *     toLazyList.scanLeft(start)(op).dropWhile(!pred(_)).headOption
		  * }}}
		  * It is not equivalent to
		  * [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldLeftWhileOption foldLeftWhileOption]]`(start)(!pred(_))(op)`,
		  * as the latter will return the last element for which `pred` was not satisfied, while this method
		  * applies `op` once more.
		  * @param start the initial value, used as the result if the collection is empty.
		  * @param pred  a stop condition for folding, checked first for `start`, and then for every result
		  *              of function `op`.
		  * @param op    a folding function.
		  * @return The first value returned by recursive application of `op` for which `pred` is `true`:
		  *    1. If `pred(start)`, then return `Some(start)` immediately;
		  *    1. If this collection is empty, return `None`;
		  *    1. Otherwise, apply `op` recursively and return the first ai, `a_0 = start; a_i+1 = op(a_i, ei)`,
		  *       where `toSeq == Seq(e0,...,en)`, such that `pred(ai)`;
		  *    1. If, after folding the whole collection, the predicate is still not satisfied, then return `None`
		  */
		def foldLeftUntilOption[A](start :A)(pred :A => Boolean)(op :(A, T) => A) :Option[A] =
			foldLeftUntilAndReturn(start)(pred)(op)(_ => None, Some.apply)


		/** Folds this collection from left to right by applying `op` to its elements and the previously returned value,
		  * until the folded value satisfies the condition `pred`.
		  * The method applies `op` recursively to the elements of this collection in the iteration order,
		  * and returns either `Right(x)` for some intermediate result of `this.foldLeft(start)(op)` if `pred(x)`,
		  * or `Left(this.foldLeft(start)(op))` if `!pred(this.foldLeft(start)(op))`.
		  *
		  * This method is a slightly more efficient and clear equivalent of
		  * {{{
		  *     toLazyList.scanLeft(start)(op).span(!pred(_)) match {
		  *         case (all, Seq()) => Left(all.last)
		  *         case (_, satisfied) => Right(satisfied.head)
		  *     }
		  * }}}
		  * It is not equivalent to
		  * [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldLeftWhileEither foldLeftWhileEither]]`(start)(!pred(_))(op)`,
		  * as the latter will return the last element for which `pred` was not satisfied, while this method
		  * applies `op` once more.
		  * @param start the initial value, used as the result if the collection is empty.
		  * @param pred  a stop condition for folding, checked first for `start`, and then for every result
		  *              of function `op`.
		  * @param op    a folding function.
		  * @return The first value returned by recursive application of `op` for which `pred` is `true`:
		  *    1. If `pred(start)`, then return `Right(start)` immediately;
		  *    1. If this collection is empty, return `Left(start)`;
		  *    1. Otherwise, apply `op` recursively and return the first `a_i`, `a_0 = start; a_i+1 = op(a_i, ei)`,
		  *       where `toSeq == Seq(e0,...,en)`, such that `pred(a_i)`;
		  *    1. If `!pred(ai)` for all `ai` then return `Left(an)`.
		  */
		def foldLeftUntilEither[A](start :A)(pred :A => Boolean)(op :(A, T) => A) :Either[A, A] =
			foldLeftUntilAndReturn(start)(pred)(op)(Left.apply, Right.apply)


		private def foldRightUntilAndReturn[A, X](start :A)(pred :A => Boolean)(op :(T, A) => A)
		                                         (ifNotFound :A => X, ifFound :A => X) :X =
			self match {
				case _ if pred(start) => ifFound(start)
				case it :Iterable[_] if it.isEmpty => ifNotFound(start)
				case seq :scala.collection.IndexedSeq[T] =>
					var i = seq.length; var last = start
					if (i <= defaultApplyPreferredMaxLength) {
						while ({ i -= 1; i >= 0 } && { last = op(seq(i), last); !pred(last) })
							{}
						if (i < 0) ifNotFound(last) else ifFound(last)
					} else {
						val it = seq.reverseIterator; var found = false
						while (it.hasNext && { last = op(it.next(), last); found = pred(last); !found })
							{}
						if (found) ifFound(last) else ifNotFound(last)
					}
				case ranking :Ranking[T] =>
					var i = ranking.size; var last = start
					if (i <= defaultApplyPreferredMaxLength) {
						while ({ i -= 1; i >= 0 } && { last = op(ranking(i), last); !pred(last) })
							{}
						if (i < 0) ifNotFound(last) else ifFound(last)
					} else {
						val it = ranking.reverseIterator; var found = false
						while (it.hasNext && { last = op(it.next(), last); found = pred(last); !found })
							{}
						if (found) ifFound(last) else ifNotFound(last)
					}
				case _ =>
					val it = self match {
						case it :Iterable[T] => it.view //though unlikely, scanRight may be lazy and more efficient
						case _ => self.iterator to LazyList
					}
					val (skipped, rest) = it.scanRight(start)(op).to(LazyList).reverse.span(!pred(_))
					if (rest.nonEmpty) ifFound(rest.head)
					else ifNotFound(skipped.last)
//			case _ =>
//				val inverse = (List.empty[T] /: self)((reversal, item) => item::reversal)
//				@tailrec def rec(item :A, rest :List[T]) :X = rest match {
//					case h::t =>
//						val next = op(h, item)
//						if (pred(next)) ifFound(next)
//						else rec(next, t)
//					case _ => ifNotFound(item)
//				}
//				rec(start, inverse)
			}


		/** Folds this collection from right to left by applying `op` to its elements and the previously returned value
		  * until the folded value satisfies the condition `pred`.
		  * Function `op` is applied recursively to the elements of this collection in the reverse iteration order.
		  * If `pred(start)`, `start` is returned immediately.
		  * The method returns returns the first value returned by recursive application of `op`
		  * for which `pred` is `true`, or the result of folding the whole collection if no such value is generated.
		  * If `pred(start)` or this collection is empty, then `start` is returned immediately,
		  * without ever calling `op`.
		  *
		  * This method is a slightly more efficient and clear equivalent of
		  * {{{
		  *     toLazyList.reverse.scanLeft(start)((a, t) => op(t, a)).span(!pred(_)) match {
		  *         case (folded, Seq()) => folded.last
		  *         case (_, Seq(h, _*)) => h
		  *     }
		  * }}}
		  * It is not equivalent to
		  * [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldRightWhile foldRightWhile]]`(start)(!pred(_))(op)`,
		  * as the latter will return the last element for which `pred` was not satisfied, while this method
		  * applies `op` once more.
		  * @param start the initial value, used as the result if the collection is empty.
		  * @param pred  a stop condition for folding, checked first for `start`, and then for every result
		  *              of function `op`.
		  * @param op    a folding function.
		  * @return the first intermediate value generated by `this.foldRight(start)(op)` for which `pred` is `true`,
		  *         or the result of folding the entire collection if no such value is generated.
		  */
		def foldRightUntil[A](start :A)(pred :A => Boolean)(op :(T, A) => A) :A =
			foldRightUntilAndReturn(start)(pred)(op)(identity, identity)

		/** Folds this collection from right to left by applying `op` to its elements and the previously returned value
		  * until the folded value satisfies the condition `pred`.
		  * The method applies `op` recursively to the elements of this collection in the iteration order,
		  * and returns either `Some(x)` for some intermediate result of `this.foldRight(start)(op)` if `pred(x)`,
		  * or `None` if the predicate has not been satisfied after folding the whole collection.
		  *
		  * This method is a slightly more efficient and clear equivalent of
		  * {{{
		  *     toLazyList.reverse.scanLeft(start)((a, t) => op(t,a)).dropWhile(!pred(_)).headOption
		  * }}}
		  * It is not equivalent to
		  * [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldRightWhileOption foldRightWhileOption]]`(start)(!pred(_))(op)`,
		  * as the latter will return the last element for which `pred` was not satisfied, while this method
		  * applies `op` once more.
		  * @param start the initial value, used as the result if the collection is empty.
		  * @param pred  a stop condition for folding, checked first for `start`, and then for every result
		  *              of function `op`.
		  * @param op    a folding function.
		  * @return The first value returned by recursive application of `op` for which `pred` is `true`:
		  *    1. If `pred(start)`, then return `Some(start)` immediately;
		  *    1. If this collection is empty, return `None`;
		  *    1. Otherwise, apply `op` recursively and return the first ai, `a_0 = start; a_i+1 = op(e_n-i, a_i)`,
		  *       where `toSeq == Seq(e1,...,en)`, such that `pred(ai)`;
		  *    1. If, after folding the whole collection, the predicate is still not satisfied, then return `None`
		  */
		def foldRightUntilOption[A](start :A)(pred :A => Boolean)(op :(T, A) => A) :Option[A] =
			foldRightUntilAndReturn(start)(pred)(op)(_ => None, Some.apply)

		/** Folds this collection from left to right by applying `op` to its elements and the previously returned value,
		  * until the folded value satisfies the condition `pred`.
		  * The method applies `op` recursively to the elements of this collection in the reverse iteration order,
		  * and returns either `Right(x)` for some intermediate result of `this.foldRight(start)(op)` if `pred(x)`,
		  * or `Left(this.foldRight(start)(op))` if `!pred(this.foldRight(start)(op))`.
		  *
		  * This method is a slightly more efficient and clear equivalent of
		  * {{{
		  *     toLazyList.reverse.scanLeft(start)((a, t) => op(t, a)).span(!pred(_)) match {
		  *         case (all, Seq()) => Left(all.last)
		  *         case (_, satisfied) => Right(satisfied.head)
		  *     }
		  * }}}
		  * It is not equivalent to
		  * [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldRightWhileEither foldRightWhileEither]]`(start)(!pred(_))(op)`,
		  * as the latter will return the last element for which `pred` was not satisfied, while this method
		  * applies `op` once more.
		  * @param start the initial value, used as the result if the collection is empty.
		  * @param pred  a stop condition for folding, checked first for `start`, and then for every result
		  *              of function `op`.
		  * @param op    a folding function.
		  * @return The first value returned by recursive application of `op` for which `pred` is `true`:
		  *    1. If `pred(start)`, then return `Right(start)` immediately;
		  *    1. If this collection is empty, return `Left(start)`;
		  *    1. Otherwise, apply `op` recursively and return the first ai, `a_0 = start; a_i+1 = op(e_n-i, a_i)`,
		  *       where `toSeq == Seq(e1,...,en)`, such that `pred(ai)`;
		  *    1. If `!pred(ai)` for all `ai` then return `Left(an)`.
		  */
		def foldRightUntilEither[A](start :A)(pred :A => Boolean)(op :(T, A) => A) :Either[A, A] =
			foldRightUntilAndReturn(start)(pred)(op)(Left.apply, Right.apply)



		/** Same as [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldLeftWhile foldLeftWhile]]. */
		def foldWhile[A >: T](start :A)(pred :A => Boolean)(op :(A, A) => A) :A =
			foldLeftWhile(start)(pred)(op)

		/** Same as [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldLeftWhileOption foldLeftWhileOption]]. */
		def foldWhileOption[A >: T](start :A)(pred :A => Boolean)(op :(A, A) => A) :Option[A] =
			foldLeftWhileOption(start)(pred)(op)

		/** Same as [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldLeftWhileEither foldLeftWhileEither]]. */
		def foldWhileEither[A >: T](start :A)(pred :A => Boolean)(op :(A, A) => A) :Either[A, A] =
			foldLeftWhileEither(start)(pred)(op)


		private def foldLeftWhileAndReturn[A, X](start :A)(pred :A => Boolean)(op :(A, T) => A)
		                                        (ifNotFound :A => X, ifFound :A => X) :X =
			self match {
				case _ if !pred(start) => ifNotFound(start)
				case it :Iterable[_] if it.isEmpty => ifFound(start)
				case _ =>
					var last = start; var next = start; var found = false
					val i = self.iterator
					while (i.hasNext && { next = op(last, i.next()); found = pred(next); found })
						last = next
					ifFound(last)
			}

		/** Folds this collection from left to right by applying `op` to its elements and the previously returned value
		  * while the folded value satisfies the condition `pred`.
		  * Function `op` is applied recursively to the elements of this collection in its iteration order
		  * as in regular [[scala.collection.IterableOps.foldLeft foldLeft]], but, additionally, predicate `pred`
		  * is tested for the current intermediate value before each iteration. If it is not satisfied,
		  * the last value satisfying it is returned. It must be satisfied for the initial value,
		  * otherwise an [[IllegalArgumentException]] is thrown. Otherwise, the result is the same as in:
		  * {{{
		  *     scanLeft(start)(op).takeWhile(pred).last
		  * }}}
		  * Note that this method is ''not'' equivalent to
		  * [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldLeftUntil foldLeftUntil]]`(start)(!pred(_))(op)`,
		  * as the latter returns the first element of `scanLeft(start)(op)` falsifying the predicate,
		  * while this method returns the last element for which it is still true.
		  * @param start the initial value, used as the result if the collection is empty.
		  * @param pred  a stop condition for folding, checked for the folded value at the beginning of every iteration.
		  * @param op    a folding function.
		  * @return the last element of the longest prefix of `scanLeft(start)(op)`
		  *         such that `pred` is `true` for all its elements.
		  */
		@throws[IllegalArgumentException]("if !pred(start).")
		def foldLeftWhile[A](start :A)(pred :A => Boolean)(op :(A, T) => A) :A =
			foldLeftWhileAndReturn(start)(pred)(op)(
				start => throw new IllegalArgumentException(
					"foldLeftWhile: starting value " + start + " does not satisfy the predicate."
				),
				identity)

		/** Folds this collection from left to right by applying `op` to its elements and the previously returned value
		  * while the folded value satisfies the condition `pred`.
		  * Function `op` is applied recursively to the elements of this collection in its iteration order
		  * as in regular [[scala.collection.IterableOps.foldLeft foldLeft]], but, additionally, predicate `pred`
		  * is tested for the current intermediate value before each iteration. If it is not satisfied,
		  * the last value satisfying it is returned, or if `!pred(start)`, then `None`. The result is the same as in
		  * {{{
		  *     scanLeft(Some(start) :Option[A])(op).takeWhile(_.exists(pred)).lastOption
		  * }}}
		  * Note that this method is ''not'' equivalent to
		  * [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldLeftUntilOption foldLeftUntilOption]]`(start)(!pred(_))(op)`,
		  * as the latter returns the first element of `scanLeft(start)(op)` falsifying the predicate,
		  * while this method returns the last element for which it is still true.
		  * @param start the initial value, used as the result if the collection is empty.
		  * @param pred  a stop condition for folding, checked for the folded value at the beginning of every iteration.
		  * @param op    a folding function.
		  * @return the last folding result for which `pred` is true in a `Some`, or `None` if it is false for `start`.
		  */
		def foldLeftWhileOption[A](start :A)(pred :A => Boolean)(op :(A, T) => A) :Option[A] =
			foldLeftWhileAndReturn(start)(pred)(op)(_ => None, Some.apply)

		/** Folds this collection from left to right by applying `op` to its elements and the previously returned value
		  * while the folded value satisfies the condition `pred`.
		  * Function `op` is applied recursively to the elements of this collection in its iteration order
		  * as in regular [[scala.collection.IterableOps.foldLeft foldLeft]], but, additionally, predicate `pred`
		  * is tested for the current intermediate value before each iteration. If it is not satisfied,
		  * the last value satisfying it is returned as a `Right`. If `!pred(start)`, then the result is `Left(start)`.
		  * It is a more concise and efficient equivalent of
		  * {{{
		  *     if (pred(start))
		  *         Right(scanLeft(start)(op).takeWhile(pred).last)
		  *     else
		  *         Left(start)
		  * }}}
		  * Note that this method is ''not'' equivalent to
		  * [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldLeftUntilEither foldLeftUntilEither]]`(start)(!pred(_))(op)`,
		  * as the latter returns the first element of `scanLeft(start)(op)` falsifying the predicate,
		  * while this method returns the last element for which it is still true.
		  * @param start the initial value, used as the result if the collection is empty.
		  * @param pred  a stop condition for folding, checked for the folded value at the beginning of every iteration.
		  * @param op    a folding function.
		  * @return the last folding result for which `pred` is true in a `Right`,
		  *         or `Left(start)` if it is false for `start`.
		  */
		def foldLeftWhileEither[A](start :A)(pred :A => Boolean)(op :(A, T) => A) :Either[A, A] =
			foldLeftWhileAndReturn(start)(pred)(op)(Left.apply, Right.apply)


		private def foldRightWhileAndReturn[A, X](start :A)(pred :A => Boolean)(op :(T, A) => A)
		                                         (ifNotFound :A => X, ifFound: A => X) :X =
			self match {
				case _ if !pred(start) => ifNotFound(start)
				case it :Iterable[_] if it.isEmpty => ifFound(start)
				case seq :scala.collection.IndexedSeq[T] =>
					var last = start; var next = start
					var i = seq.length
					if (i <= defaultApplyPreferredMaxLength) {
						while (i > 0 && { i -= 1; next = op(seq(i), last); pred(next) })
							last = next
					} else {
						val it = seq.reverseIterator
						while (it.hasNext && { next = op(it.next(), last); pred(next) })
							last = next
					}
					ifFound(last)
				case ranking :Ranking[T] =>
					var last = start; var next = start
					var i = ranking.size
					if (i <= defaultApplyPreferredMaxLength) {
						while (i > 0 && { i -= 1; next = op(ranking(i), last); pred(next) })
							last = next
					} else {
						val it = ranking.reverseIterator
						while (it.hasNext && { next = op(it.next(), last); pred(next) })
							last = next
					}
					ifFound(last)
				case _ =>
					val it = self match {
						case it :Iterable[T] => it.view //though unlikely, scanRight may be lazy and more efficient
						case _ => self.iterator to LazyList
					}
					val last = it.scanRight(start)(op).to(LazyList).reverse.takeWhile(pred).last
					ifFound(last)
	//			case _ =>
	//				var inverse = (List.empty[T] /: self)((list, item) => item::list)
	//				var last = start; var next = start; var found = false
	//				while (inverse.nonEmpty && {
	//					next = op(inverse.head, last); inverse = inverse.tail; found = pred(next); found
	//				})
	//					last = next
	//				ifFound(last)
			}


		/** Folds this collection from right to left by applying `op` to its elements and the previously returned value
		  * while the folded value satisfies the condition `pred`.
		  * Function `op` is applied recursively to the elements of this collection in the reversed iteration order
		  * as in regular [[scala.collection.IterableOps.foldRight foldRight]], but, additionally, predicate `pred`
		  * is tested for the current intermediate value before each iteration. If it is not satisfied,
		  * the last value satisfying it is returned. It must be satisfied for the initial value,
		  * otherwise an [[IllegalArgumentException]] is thrown. Otherwise, the result is the same as in both:
		  * {{{
		  *     toLazyList.reverse.foldLeftWhile(start)(pred)((a, t) => op(t, a))
		  *     toLazyList.reverse.scanLeft(start)((a, t) => op(t, a)).takeWhile(pred).last
		  * }}}
		  * Note that this method is ''not'' equivalent to
		  * [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldRightUntil foldRightUntil]]`(start)(!pred(_))(op)`,
		  * as the latter returns the first value falsifying the predicate,
		  * while this method returns the last value for which it is still true.
		  * @param start the initial value, used as the result if the collection is empty.
		  * @param pred  a stop condition for folding, checked for the folded value at the beginning of every iteration.
		  * @param op    a folding function.
		  * @return the last element of the longest prefix of `scanLeft(start)(op)`
		  *         such that `pred` is `true` for all its elements.
		  */
		@throws[IllegalArgumentException]("if !pred(start).")
		def foldRightWhile[A](start :A)(pred :A => Boolean)(op :(T, A) => A) :A =
			foldRightWhileAndReturn(start)(pred)(op)(
				start => throw new IllegalArgumentException(
					"foldLeftWhile: starting value " + start + " does not satisfy the predicate."
				),
				identity
			)

		/** Folds this collection from right to left by applying `op` to its elements and the previously returned value
		  * while the folded value satisfies the condition `pred`.
		  * Function `op` is applied recursively to the elements of this collection in the reversed iteration order
		  * as in regular [[scala.collection.IterableOps.foldRight foldRight]], but, additionally, predicate `pred`
		  * is tested for the current intermediate value before each iteration. If it is not satisfied,
		  * the last value satisfying it is returned, or, if `!pred(start)`, then `None`. The result is equal to both
		  * {{{
		  *     toLazyList.reverse.foldLeftWhileOption(start)(pred)((a, t) => op(t, a))
		  *     toLazyList.reverse.scanLeft(Some(start) :Option[A])((a, t) => op(t, a)).takeWhile(_.exists(pred)).lastOption
		  * }}}
		  * Note that this method is ''not'' equivalent to
		  * [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldRightUntilOption foldRightUntilOption]]`(start)(!pred(_))(op)`,
		  * as the latter returns the first element of `scanLeft(start)(op)` falsifying the predicate,
		  * while this method returns the last element for which it is still true.
		  * @param start the initial value, used as the result if the collection is empty.
		  * @param pred  a stop condition for folding, checked for the folded value at the beginning of every iteration.
		  * @param op    a folding function.
		  * @return the last folding result for which `pred` is true in a `Some`, or `None` if it is false for `start`.
		  */
		def foldRightWhileOption[A](start :A)(pred :A => Boolean)(op :(T, A) => A) :Option[A] =
			foldRightWhileAndReturn(start)(pred)(op)(_ => None, Some.apply)

		/** Folds this collection from right to left by applying `op` to its elements and the previously returned value
		  * while the folded value satisfies the condition `pred`.
		  * Function `op` is applied recursively to the elements of this collection in its iteration order
		  * as in regular [[scala.collection.IterableOps.foldRight foldRight]], but, additionally, predicate `pred`
		  * is tested for the current intermediate value before each iteration. If it is not satisfied,
		  * the last value satisfying it is returned as a `Right`. If `!pred(start)`, then the result is `Left(start)`.
		  * It is a more concise and efficient equivalent of
		  * {{{
		  *     if (pred(start))
		  *         Right(toList.reverse.scanLeft(start)((a, t) => op(t, a)).takeWhile(pred).last)
		  *     else
		  *         Left(start)
		  * }}}
		  * Note that this method is ''not'' equivalent to
		  * [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldRightUntilEither foldRightUntilEither]]`(start)(!pred(_))(op)`,
		  * as the latter returns the first element of `scanRight(start)(op)` falsifying the predicate,
		  * while this method returns the last element for which it is still true.
		  * @param start the initial value, used as the result if the collection is empty.
		  * @param pred  a stop condition for folding, checked for the folded value at the beginning of every iteration.
		  * @param op    a folding function.
		  * @return the last folding result for which `pred` is true in a `Right`,
		  *         or `Left(start)` if it is false for `start`.
		  */
		def foldRightWhileEither[A](start :A)(pred :A => Boolean)(op :(T, A) => A) :Either[A, A] =
			foldRightWhileAndReturn(start)(pred)(op)(Left.apply, Right.apply)



		/** Recursively applies the given partial function to the elements of this collection and a folded value,
		  * starting with `start`, for as long as it is applicable to its own return value.
		  * This method could be defined recursively as
		  * {{{
		  *     if (isEmpty || !op.isDefinedAt(start)) start
		  *     else tail.partialFold(op(start, head))(op)
		  * }}}
		  * @param start an accumulator given as the first argument to first invocation of `op`.
		  * @param op a partial function combining two elements of the collection or folded value, working
		  *           as the breaking condition when non-defined.
		  * @tparam A type of generated values.
		  * @return result of `this.fold(a)(op)` or first encountered value `a :A` such that `op` is not defined
		  *         for `(e, a)` where `e` is the first non-folded element of this collection.
		  * @return [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.partialFoldLeft partialFoldLeft]]`(start)(op)`.
		  */
		def partialFold[A >: T](start :A)(op :PartialFunction[(A, A), A]) :A = partialFoldLeft(start)(op)

		/** Applies the given folding function `op` to the elements of this collection from left to right,
		  * starting with the given initial value `start`, for as long as `op` is defined
		  * for the previously computed value.
		  * The method traverses this collection in the iteration order, maintaining the result of folding
		  * its current prefix, and stops when the function is no longer applicable to the intermediate result,
		  * returning it. It can be recursively defined as
		  * {{{
		  *     if (isEmpty || !op.isDefinedAt(start)) start
		  *     else tail.partialFoldLeft(op(start, head))(op)
		  * }}}
		  * @param start an accumulator given as the first argument to first invocation of `op`.
		  * @param op a partial function combining the accumulator with the elements of the collection working
		  *           as the breaking condition when non-defined.
		  * @tparam A type of generated values.
		  * @return result of `this.foldLeft(a)(op)` or first value `a :A` such that `op` is not defined
		  *         for `(a, e)` where `e` is the first non-folded element of this collection.
		  */
		def partialFoldLeft[A](start :A)(op :PartialFunction[(A, T), A]) :A = self match {
			case it :Iterable[_] if it.isEmpty => start
			case it :Iterator[_] if it.isEmpty => start
			case _ =>
				val it = self.iterator
				var last = start
				var continue = true
				val fallback = { input :(A, T) => continue = false; input._1 }
				while (it.hasNext && { last = op.applyOrElse((last, it.next()), fallback); continue })
					{}
				last
		}

		/** Applies the given folding function `op` to the elements of this collection from right to  left,
		  * starting with the given initial value `start`, for as long as `op` is defined
		  * for the previously computed value.
		  * The method reverses the iteration order of the collection, maintaining the result of folding
		  * its current suffix, and stops when the function is no longer applicable to the intermediate result,
		  * returning it. It can be recursively defined as
		  * {{{
		  *     if (isEmpty || !op.isDefinedAt(last)) start
		  *     else init.partialFoldRight(op(last, start))(op)
		  * }}}
		  * @param start an accumulator given as the first argument to first invocation of `op`.
		  * @param op a partial function combining the accumulator with the elements of the collection working
		  *           as the breaking condition when non-defined.
		  * @tparam A type of generated values.
		  * @return result of `this.foldRight(a)(op)` or first encountered value `a :A` such that `op` is not defined
		  *         for `(e, a)` where `e` is the first non-folded element of this collection.
		  */
		@tailrec final def partialFoldRight[A](start :A)(op :PartialFunction[(T, A), A]) :A = {
			var last = start
			var continue = true
			self match {
				case it :Iterable[_] if it.isEmpty => start
				case it :Iterator[_] if it.isEmpty => start
				case seq :scala.collection.IndexedSeq[T] if seq.length <= defaultApplyPreferredMaxLength =>
					var i = seq.length - 1
					val fallback = { input :(T, A) => continue = false; input._2 }
					while (i >= 0 && { last = op.applyOrElse((seq(i), last), fallback); continue })
						i -= 1
					last
				case ranking :Ranking[T] if ranking.length <= defaultApplyPreferredMaxLength =>
					var i = ranking.length - 1
					val fallback = { input :(T, A) => continue = false; input._2 }
					while (i >= 0 && { last = op.applyOrElse((ranking(i), last), fallback); continue })
						i -= 1
					last
				case seq :scala.collection.Seq[T] =>
					val it = seq.reverseIterator
					val fallback = { input :(T, A) => continue = false; input._2 }
					while (it.hasNext && { last = op.applyOrElse((it.next(), last), fallback); continue })
						{}
					last
				case _ =>
					val it = self match {
						case it :Iterable[T] => it to PassedArray
						case _ => self.iterator to PassedArray
					}
					it.partialFoldRight(start)(op)
			}
		}


		/** Recursively applies the given function to the elements of this collection and a folded value,
		  * starting with `start`, for as long is it returns `Some`.
		  * This method can be defined recursively as
		  * {{{
		  *     if (isEmpty) start
		  *     else op(head, start).map(tail.foldSome(_)(op)) getOrElse start
		  * }}}
		  * @param start initial accumulator value passed to the first call of `op` together with the first element
		  *              of this collection.
		  * @param op a function generating consecutive values of `A` from the previous value and subsequent element
		  *           of this collection, yielding `None` to signal the termination of folding.
		  * @return [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldLeftSome foldLeftSome]]`(start)(op)`.
		  */
		def foldSome[A >: T](start :A)(op :(A, A) => Option[A]) :A = foldLeftSome(start)(op)

		/** Applies the given folding function to the elements of this collection and a folded value,
		  * starting with `start`, for as long as it returns non empty results.
		  * The method traverses this collection in the iteration order, maintaining the result of folding
		  * its current prefix, and stops when the function is no longer applicable to the intermediate result,
		  * It can be defined recursively as
		  * {{{
		  *     if (isEmpty) start
		  *     else op(head, start).map(tail.foldLeftSome(_)(op)) getOrElse start
		  * }}}
		  * @param start initial accumulator value passed to the first call of `op` together with the first element
		  *              of this collection.
		  * @param op a function generating consecutive values of `A` from the previous value and subsequent element
		  *           of this collection, yielding `None` to signal the termination of folding.
		  * @tparam A type of generated values.
		  * @return the result of the last execution of `op` which returned `Some`,
		  *         or `start` if either this collection is empty or `op(this.head, start) == None`.
		  */
		def foldLeftSome[A](start :A)(op :(A, T) => Option[A]) :A = self match {
			case it :Iterable[_] if it.isEmpty => start
			case it :Iterator[_] if it.isEmpty => start
			case _ =>
				val it = self.iterator
				var last = start
				while (it.hasNext && (op(last, it.next()) match {
					case Some(next) => last = next; true
					case _ => false
				})) {}
				last
		}

		/** Applies the given folding function `op` to the elements of this collection from right to  left,
		  * starting with the given initial value `start`, for as long as `op` is defined
		  * for the previously computed value.
		  * The method reverses the iteration order of the collection, maintaining the result of folding
		  * its current suffix, and stops when the function is no longer applicable to the intermediate result,
		  * returning it. It can be recursively defined as
		  * {{{
		  *     if (isEmpty) start
		  *     else op(last, start).map(tail.foldRightSome(_)(op)) getOrElse start
		  * }}}
		  * @param start an accumulator given as the first argument to first invocation of `op`.
		  * @param op a function generating consecutive values of `A` from the previous value and subsequent element
		  *           of this collection, yielding `None` to signal the termination of folding.
		  * @tparam A type of generated values.
		  * @return the result of the last execution of `op` which returned `Some`,
		  *         or `start` if either this collection is empty or `op(this.last, start) == None`.
		  */
		@tailrec final def foldRightSome[A](start :A)(op :(T, A) => Option[A]) :A = self match {
			case it :Iterable[_] if it.isEmpty => start
			case it :Iterator[_] if it.isEmpty => start
			case seq :scala.collection.IndexedSeq[T] if seq.length <= defaultApplyPreferredMaxLength =>
				var i = seq.length - 1; var last = start
				while (i >= 0 && (op(seq(i), last) match {
					case Some(next) => last = next; true
					case _ => false
				})) i -= 1
				last
			case ranking :Ranking[T] if ranking.length <= defaultApplyPreferredMaxLength =>
				var i = ranking.length - 1; var last = start
				while (i >= 0 && (op(ranking(i), last) match {
					case Some(next) => last = next; true
					case _ => false
				})) i -= 1
				last
			case seq :scala.collection.Seq[T] =>
				var last = start
				var next :Option[A] = Some(start)
				val it = seq.reverseIterator
				while (it.hasNext && { next = op(it.next(), last); next.isDefined })
					last = next.get
				last
			case _ =>
				val it = self match {
					case it :Iterable[T] => it to PassedArray
					case _ => self.iterator to PassedArray
				}
				it.foldRightSome(start)(op)
		}


		/** Same as [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.reduceLeftUntil reduceLeftUntil]]. */
		def reduceUntil[A >: T](pred :A => Boolean)(op :(A, A) => A) :A = reduceLeftUntil(pred)(op)

		/** Same as [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.reduceLeftUntilOption reduceLeftUntilOption]]. */
		def reduceUntilOption[A >: T](pred :A => Boolean)(op :(A, A) => A) :Option[A] = reduceLeftUntilOption(pred)(op)


		private def reduceLeftUntilAndReturn[A >: T, X](pred :A => Boolean)(op :(A, T) => A)
		                                               (ifEmpty: => X, ifNotFound :A => X, ifFound :A => X) :X =
			self match {
				case it :Iterable[_] if it.isEmpty => ifEmpty
				case _ =>
					val i = self.iterator
					if (i.isEmpty)
						ifEmpty
					else {
						var last :A = i.next(); var found = pred(last)
						while (!found && i.hasNext) {
							last = op(last, i.next()); found = pred(last)
						}
						if (found) ifFound(last)
						else ifNotFound(last)
					}
			}

		/** Reduces this collection, going from left to right, for as long as the given predicate is not satisfied.
		  * The method returns the first value which satisfies the predicate, or the result of reducing
		  * the whole collection, if it never becomes satisfied. If `pred(this.head)`, then it is returned,
		  * without invoking the function.
		  */
		@throws[UnsupportedOperationException]("if this collection is empty.")
		def reduceLeftUntil[A >: T](pred :A => Boolean)(op :(A, T) => A) :A =
			reduceLeftUntilAndReturn(pred)(op)(
				throw new UnsupportedOperationException("empty.reduceLeftUntil"),
				identity,
				identity
			)

		/** Reduces this collection, going from left to right, for as long as the given predicate is not satisfied.
		  * The method returns in `Some` the first value which satisfies the predicate; if `pred(this.head)`,
		  * then it is returned, without invoking the function. If the collection is empty,
		  * or is completely reduced without ever satisfying the predicate, `None` is returned.
		  */
		def reduceLeftUntilOption[A >: T](pred :A => Boolean)(op :(A, T) => A) :Option[A] =
			reduceLeftUntilAndReturn(pred)(op)(None, _ => None, Some.apply)


		private def reduceRightUntilAndReturn[A >: T, X](pred :A => Boolean)(op :(T, A) => A)
		                                                (ifEmpty: => X, ifNotFound :A => X, ifFound :A => X) :X =
			self match {
				case it :Iterable[_] if it.isEmpty => ifEmpty
				case it :Iterator[_] if it.isEmpty => ifEmpty
				case seq :scala.collection.IndexedSeq[T] if seq.length <= defaultApplyPreferredMaxLength =>
					var i = seq.length - 1
					var last :A = seq(i); var found = pred(last)
					while (!found && i > 0) {
						i -= 1; last = op(seq(i), last); found = pred(last)
					}
					if (found) ifFound(last) else ifNotFound(last)
				case seq :scala.collection.Seq[T] =>
					val it = seq.reverseIterator
					var last :A = it.next(); var found = pred(last)
					while (!found && it.hasNext) {
						last = op(it.next(), last); found = pred(last)
					}
					if (found) ifFound(last) else ifNotFound(last)
				case _ =>
					var inverse = (List.empty[T] /: self)((list, item) => item::list)
					if (inverse.isEmpty)
						ifEmpty
					else {
						var last :A = inverse.head; inverse = inverse.tail; var found = pred(last)
						while (!found && inverse.nonEmpty) {
							last = op(inverse.head, last); inverse = inverse.tail; found = pred(last)
						}
						if (found) ifFound(last) else ifNotFound(last)
					}
			}

		/** Reduces this collection, going from right to left, for as long as the given predicate is not satisfied.
		  * The method returns the first value which satisfies the predicate, or the result of reducing
		  * the whole collection, if it never becomes satisfied. If `pred(this.last)`, then it is returned,
		  * without invoking the function.
		  */
		@throws[UnsupportedOperationException]("if this collection is empty.")
		def reduceRightUntil[A >: T](pred :A => Boolean)(op :(T, A) => A) :A =
			reduceRightUntilAndReturn(pred)(op)(
				throw new UnsupportedOperationException("empty.reduceRightUntil"),
				identity,
				identity
			)

		/** Reduces this collection, going from right to left, for as long as the given predicate is not satisfied.
		  * The method returns in `Some` the first value which satisfies the predicate; if `pred(this.last)`,
		  * then it is returned, without invoking the function. If the whole collection is empty,
		  * or is completely reduced without ever satisfying the predicate, `None` is returned.
		  */
		def reduceRightUntilOption[A >: T](pred :A => Boolean)(op :(T, A) => A) :Option[A] =
			reduceRightUntilAndReturn(pred)(op)(None, _ => None, Some.apply)


		/** Same as [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.partialReduceLeft partialReduceLeft]]. */
		def partialReduce[A >: T](f :PartialFunction[(A, A), A]) :A =
			partialReduceLeft(f)

		/** Reduces this collection with the given function, going from left to right,
		  * for as long as the partial function is applicable to the pair of the last value and the next item
		  * in the collection. The method returns the first value `a` - either the first element of the collection,
		  * or the last value returned by `f` - for which `!f.isDefinedAt(a, iter.next())`.
		  */
		@throws[UnsupportedOperationException]("if this collection is empty.")
		def partialReduceLeft[A >: T](f :PartialFunction[(A, T), A]) :A = {
			val lift = f.lift
			reduceLeftSome[A]((acc, elem) => lift((acc, elem)))
		}

		/** Reduces this collection with the given function, going from right to left,
		  * for as long as the partial function is applicable to the pair of the last value and the next item
		  * in the collection. The method returns the first value `a` - either the last element of the collection,
		  * or the last value returned by `f` - for which `!f.isDefinedAt(reverseIter.next(), a)`.
		  */
		@throws[UnsupportedOperationException]("if this collection is empty.")
		def partialReduceRight[A >: T](f :PartialFunction[(T, A), A]) :A = {
			val lift = f.lift
			reduceRightSome[A]((elem, acc) => lift((elem, acc)))
		}

		/** Same as [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.reduceLeftSome reduceLeftSome]]. */
		def reduceSome[A >: T](f :(A, A) => Option[A]) :A = reduceLeftSome(f)

		/** Reduces this collection with the given function, going from left to right, for as long as it returns `Some`.
		  * Once the function returns `None`, or the whole collection is reduced, the last value returned
		  * by `f` in `Some` is returned.
		  */
		@throws[UnsupportedOperationException]("if the collection is empty.")
		def reduceLeftSome[A >: T](f :(A, T) => Option[A]) :A = {
			val it = self.iterator
			if (it.isEmpty)
				throw new UnsupportedOperationException("empty.reduceLeftSome")
			var last :A = it.next()
			while (it.hasNext && (f(last, it.next()) match {
				case Some(a) => last = a; true
				case _ => false
			})) {}
			last
		}

		/** Reduces this collection with the given function, going from right to left, for as long as it returns `Some`.
		  * Once the function returns `None`, or the whole collection is reduced, the last value returned
		  * by `f` in `Some` is returned.
		  */
		@throws[UnsupportedOperationException]("if the collection is empty.")
		def reduceRightSome[A >: T](f :(T, A) => Option[A]) :A = self match {
			case it :Iterable[_] if it.isEmpty =>
				throw new UnsupportedOperationException("empty.reduceRightSome")
			case seq :scala.collection.IndexedSeq[T] if seq.length <= defaultApplyPreferredMaxLength =>
				var last :A = seq.last
				var i = seq.length - 2
				while (i >= 0 && (f(seq(i), last) match {
					case Some(next) => last = next; i -= 1; true
					case _ => false
				})) {}
				last
			case seq :scala.collection.Seq[T] =>
				val it = seq.reverseIterator
				var last :A = it.next()
				while (it.hasNext && (f(it.next(), last) match {
					case Some(next) => last = next; true
					case _ => false
				})) {}
				last
			case _ =>
				var inverse = (List.empty[T] /: self.iterator)((list, item) => item::list)
				if (inverse.isEmpty)
					throw new UnsupportedOperationException("empty.reduceRightSome")
				var last :A = inverse.head; inverse = inverse.tail
				while (inverse.nonEmpty && (f(inverse.head, last) match {
					case Some(next) => last = next; inverse = inverse.tail; true
					case _ => false
				})) {}
				last
		}

		/** Iterates over the collection, passing the index of the current element to the given function.
		  * Note that in collections with an undefined order, this index applies only to this particular iteration,
		  * rather than some absolute position.
		  */
		def foreachWithIndex[U](f :(T, Int) => U) :Unit = {
			var i = 0
			foreach { e => f(e, i); i += 1 }
		}
		private def foreach[U](f :T => U) :Unit = self match {
			case it :Iterable[T] => it foreach f
			case _ => self.iterator foreach f
		}

		/** Traverses the collection, applying a folding function `op`, checking if the condition `pred` holds
		  * for the current state and element. The folding function is applied after the predicate:
		  * if the latter is falsified, the former will not be evaluated for that element. It is almost equivalent to
		  * {{{
		  *     !foldLeftUntil(start -> false)(_._2){ (a, e) => (op(a._1, e), !pred(a._1, e))._2; }
		  * }}}
		  * except that in the latter `op` is evaluated one extra time, and this method does not create intermediate
		  * tuples, which may make it slightly faster.
		  *
		  * @param start initial state for the folding function `op`, updated by the latter at each element.
		  * @param op    a `foldLeft` kind of function, combining the current state with a next element of the collection.
		  * @param pred  a predicate applied to each element of the collection and the most recent state value;
		  *              it starts with `pred(start, this.head)` and continues with the following elements
		  *              and a value equal to `this.take(i).foldLeft(start)(op)`, where `i` is the position
		  *              of the element in the iteration order.
		  * @see [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldLeftUntil]]
		  */ //todo: test
//		def forallWith[A](start :A)(pred :(A, T) => Boolean)(op :(A, T) => A) :Boolean = {
//			var state = start
//			forall { x => val continue = pred(state, x); continue && { state = op(state, x); true }}
//		}
		/** Traverses the collection, passing state modified at each element, until the argument function returns `false`
		  * or the whole collection is folded. This is a slightly more efficient and clear  equivalent of:
		  * {{{
		  *     toLazyList.scanLeft(start -> true)(f(_._1, _)).takeWhile(_._2).last._2
		  * }}}
		  * @param start the initial state for the folding function.
		  * @param f     a function, applied to each element of the collection and the most recently computed state,
		  *              and returning the new state value as in `foldLeft` paired with the predicate value
		  *              which must hold for all elements.
		  */
		def forallWith[A](start :A)(f :(A, T) => (A, Boolean)) :Boolean = {
			var state = start
			forall { x => val (next, continue) = f(state, x); state = next; continue }
		}
		private def forall(f :T => Boolean) :Boolean = self match {
			case it :Iterable[T] => it forall f
			case _ => self.iterator forall f
		}

		/** Invokes `this.forall`, passing a function applying `f` to the next element and its position
		  * in the iteration order.
		  */
		def forallWithIndex(f :(T, Int) => Boolean) :Boolean =
			foldLeftUntil(0)(_ < 0) { (i, e) => if (f(e, i)) i + 1 else -1 } >= 0

		/** Equivalent to `this.zip(that).foreach { case (a, b) => f(a, b)` }, but does not build an intermediate
		  * collection of tuples and accepts a two argument function rather than a function of tuple,
		  * which makes it more convenient to use with the lambda placeholder syntax.
		  */
		def zipForeach[X, U](that :IterableOnce[X])(f :(T, X) => U) :Unit = {
			val l = self.iterator
			val r = that.iterator
			while (l.hasNext && r.hasNext)
				f(l.next(), r.next())
		}

		/** Equivalent to `this.zip(that).forall { case (a, b) => p(a, b) }`, but does not build an intermediate
		  * collection of tuples and accepts a two argument function rather than a function of tuple, which makes
		  * it more convenient to use with the lambda parameter placeholder syntax. The difference from
		  * `this.corresponds(that)(p)` is that if one collection is larger than the other, excess elements are simply
		  * ignored (as by `zip`).
		  */
		def zipForall[X](that :IterableOnce[X])(p :(T, X) => Boolean) :Boolean = {
			val l = self.iterator
			val r = that.iterator
			var wasTrue = true
			while (l.hasNext && r.hasNext && { wasTrue = p(l.next(), r.next()); wasTrue })
				{}
			wasTrue
		}
	}




	/** Additional extension methods for collections of the standard library framework.
	  * The common theme is performing mapping with help of a passed state/accumulator value.
	  */
	class IterableExtension[E, CC[X], C] private[extensions](private val self :IterableOps[E, CC, C]) extends AnyVal {

		/** Maps this collection from left to right with an accumulating state updated by the mapping function.
		  * The state is discarded after the operation and only the mapping results (the second elements
		  * of the tuples returned by the given function) are returned in a collection of the same dynamic type
		  * as this collection.
		  */
		def mapWith[O, A](z :A)(f :(E, A) => (O, A)) :CC[O] =
			if (self.isEmpty)
				self.iterableFactory.empty[O]
			else //safe because null is never passed to f and we are in an erased context
				self.view.scanLeft((null.asInstanceOf[O], z)) {
					(acc, e) => f(e, acc._2)
				}.tail.map(_._1).to(self.iterableFactory)

		/** Flat maps this collection from left to right with an accumulating state updated by the mapping function.
		  * The state is discarded after the operation and only the mapping results (the collections returned by
		  * by the given function) are returned in a collection of the same dynamic type as this collection.
		  */
		def flatMapWith[A, O](z :A)(f :(E, A) => (IterableOnce[O], A)) :CC[O] =
			if (self.isEmpty)
				self.iterableFactory.empty[O]
			else
				self.view.scanLeft((Nil :IterableOnce[O], z)) {
					(acc, e) => f(e, acc._2)
				}.flatMap(_._1).to(self.iterableFactory)

		/** Maps this collection in order consistent with `foreach`, passing as the first argument the index
		  * of the mapped element.
		  */
		def mapWithIndex[O](f :(E, Int) => O) :CC[O] =
			if (self.isEmpty)
				self.iterableFactory.empty[O]
			else {
				var i = 0
				val b = self.iterableFactory.newBuilder[O]
				b sizeHint self
				self foreach { e => b += f(e, i); i += 1 }
				b.result()
			}

		/** Flat maps this collection in order consistent with `foreach`, passing as the first argument the index
		  * of the mapped element in this collection (that is, the number of elements processed before it).
		  */
		def flatMapWithIndex[O](f :(E, Int) => IterableOnce[O]) :CC[O] =
			if (self.isEmpty)
				self.iterableFactory.empty[O]
			else {
				var i = 0
				val b = self.iterableFactory.newBuilder[O]
				self foreach { e => b ++= f(e, i); i += 1 }
				b.result()
			}

		/** Maps this collection from left to right with an accumulating state updated by the mapping function
		  * for as long as the state passes a given predicate.
		  * The state is discarded after the operation and only the mapping results (the second elements
		  * of the tuples returned by the given function) are returned in a collection of the same dynamic type
		  * as this collection.
		  */
		//Consider: the order of parameters to f. On one side, it works as a foldLeft, but on the other like mapWith
		def mapWhile[O, A](z :A)(pred :A => Boolean)(f :(A, E) => (A, O)) :CC[O] =
			if (self.isEmpty)
				self.iterableFactory.empty
			else //safe because null is never passed to f and we are in an erased context
				self.view.scanLeft((z, null.asInstanceOf[O])) {
					(acc, e) => f(acc._1, e)
				}.tail.takeWhile(state => pred(state._1)).map(_._2).to(self.iterableFactory)

		//commented out until Scala 3
/*
		def mapWhile[O, A](z :A)(pred :(A, E) => Boolean)(f :(A, E) => (A, O)) :C[O] =
			if (self.isEmpty)
				self.iterableFactory.empty[O]
			else {
				val b = self.iterableFactory.newBuilder[O]
				val i = self.iterator
				var acc = z
				while (i.hasNext && {
					val e = i.next()
					pred(acc, e) && {
						val (updated, out) = f(acc, e)
						b += out
						acc = updated
						true
					}
				}) {}
				b.result()
			}
*/

		/** Flat maps this collection from left to right with an accumulating state updated by the mapping function
		  * for as long as the state passes a given predicate.
		  * The state is discarded after the operation and only the mapping results (the collections returned by
		  * by the given function) are returned in a collection of the same dynamic type as this collection.
		  */
		def flatMapWhile[O, A](z :A)(pred :A => Boolean)(f :(A, E) => (A, IterableOnce[O])) :CC[O] =
			if (self.isEmpty)
				self.iterableFactory.empty
			else
				self.view.scanLeft((z, Nil :IterableOnce[O])) {
					(acc, e) => f(acc._1, e)
				}.tail.takeWhile(state => pred(state._1)).flatMap(_._2).to(self.iterableFactory)

		//Uncomment in Scala 3
/*
		def flatMapWhile[O, A](z :A)(pred :(A, E) => Boolean)(f :(A, E) => (A, IterableOnce[O])) :C[O] =
			if (self.isEmpty)
				self.iterableFactory.empty
			else {
				val b = self.iterableFactory.newBuilder[O]
				val i = self.iterator
				var acc = z
				while (i.hasNext && {
					val e = i.next()
					pred(acc, e) && {
						val (updated, out) = f(acc, e)
						b ++= out
						acc = updated
						true
					}
				}) {}
				b.result()
			}
*/
		//todo: tests and docs
		def mapUntil[A, O](z :A)(f :(A, E) => (Boolean, A, O)) :CC[O] =
			if (self.isEmpty)
				self.iterableFactory.empty
			else {
				val b = self.iterableFactory.newBuilder[O]
				val i = self.iterator
				var acc = z
				while (i.hasNext && {
					val e = i.next()
					val (stop, state, out) = f(z, e)
					acc = state
					b += out
					!stop
				}) {}
				b.result()
			}

		def flatMapUntil[A, O](z :A)(f :(A, E) => (Boolean, A, IterableOnce[O])) :CC[O] =
			if (self.isEmpty)
				self.iterableFactory.empty
			else {
				val b = self.iterableFactory.newBuilder[O]
				val i = self.iterator
				var acc = z
				while (i.hasNext && {
					val e = i.next()
					val (stop, state, out) = f(z, e)
					acc = state
					b ++= out
					!stop
				}) {}
				b.result()
			}

		/** Iterates over the collection from left to right, keeping only those elements for which `pred`
		  * returns `true` as the first pair element, all the while passing to it the latest right element as
		  * the second argument.
		  */
		def filterWith[A](z :A)(pred :(E, A) => (Boolean, A)) :CC[E] = {
			var state = z
			val res = self.iterableFactory.newBuilder[E]
			self foreach { e =>
				val entry = pred(e, state)
				if (entry._1)
					res += e
				state = entry._2
			}
			res.result()
		}


		/** Equivalent to `this.iterator.zipWithIndex.filter(x => pred(x._1, x._2)) to this.iterableFactory`.
		  * For an `IndexedSeq`, prefer `(0 until length).collect { case i if pred(this(i), i) => this(i) }`.
		  */
		def filterWithIndex(pred :(E, Int) => Boolean) :CC[E] = {
			var i = 0
			val res = self.iterableFactory.newBuilder[E]
			self.foreach { e =>
				if (pred(e, i))
					res += e
				i += 1
			}
			res.result()
		}

		/** Iterates over the collection from left to right, splitting elements into those for which `pred`
		  * returns `true` as the first pair element, and those for which it returns `false`,
		  * all the while passing to it the latest right element as the second argument.
		  */
		def partitionWith[A](z :A)(pred :(E, A) => (Boolean, A)) :(CC[E], CC[E]) = {
			var state = z
			val left  = self.iterableFactory.newBuilder[E]
			val right = self.iterableFactory.newBuilder[E]
			self foreach { e =>
				val entry = pred(e, state)
				if (entry._1)
					left += e
				else
					right += e
				state = entry._2
			}
			(left.result(), right.result())
		}

		/** Equivalent to `this.zipWithIndex.partition(x => pred(x._1, x._2))`, but possibly more efficient. */
		def partitionWithIndex(pred :(E, Int) => Boolean) :(CC[E], CC[E]) = {
			var i = 0
			val left  = self.iterableFactory.newBuilder[E]
			val right = self.iterableFactory.newBuilder[E]
			self.foreach { e =>
				if (pred(e, i))
					left += e
				else
					right += e
				i += 1
			}
			(left.result(), right.result())
		}

		/** Zips this collection with another one and maps the result in one step.
		  * No intermediate collection is created, and the mapping function accepts two arguments rather than a tuple,
		  * making it more convenient to use with placeholder parameters.
		  */
		def zipMap[X, O](that :IterableOnce[X])(f :(E, X) => O) :CC[O] = {
			val l = self.iterator
			val r = that.iterator
			val res = self.iterableFactory.newBuilder[O]
			res sizeHint self
			while (l.hasNext && r.hasNext)
				res += f(l.next(), r.next())
			res.result()
		}

		/** Equivalent to [[net.noresttherein.sugar.collections.extensions.IterableExtension.zipMap zipMap]],
		  * but throws a [[NoSuchElementException]] if the collections are of different sizes.
		  */
		def zipMapAll[X, O](that :IterableOnce[X])(f :(E, X) => O) :CC[O] = {
			val l = self.iterator
			val r = that.iterator
			val res = self.iterableFactory.newBuilder[O]
			res sizeHint self
			while (l.hasNext && r.hasNext)
				res += f(l.next(), r.next())
			if (l.hasNext)
				throw new NoSuchElementException("Cannot zipMapAll: left collection has more elements than the right one.")
			else if (r.hasNext)
				throw new NoSuchElementException("Cannot zipMapAll: right collection has more elements than the left one.")
			res.result()
		}

		/** Equivalent to `this.zipAll(that, thisElem, thatElem).map(f)`, but happens in one step and the argument function
		  * takes two arguments instead of a pair, which makes it possible to use with lambda placeholder parameters.
		  */
		def zipMapAll[X, O](that :IterableOnce[X], thisElem :E, thatElem :X)(f :(E, X) => O) :CC[O] = {
			val l = self.iterator
			val r = that.iterator
			val res = self.iterableFactory.newBuilder[O]
			val thisSize = self.knownSize
			val thatSize = that.knownSize
			if (thisSize >= 0)
				if (thatSize >= 0)
					res sizeHint (thisSize max thatSize)
				else
					res sizeHint thisSize
			else if (thatSize >= 0)
				res sizeHint thatSize
			while (l.hasNext && r.hasNext)
				res += f(l.next(), r.next())
			while (l.hasNext)
				res += f(l.next(), thatElem)
			while (r.hasNext)
				res += f(thisElem, r.next())
			res.result()
		}

		/** Equivalent to `this.zip(rights).map`, but takes a two argument function instead of a function of a pair,
		  * which makes it possible to use with placeholder lambda parameters.
		  */
		def zipFlatMap[X, O](that :IterableOnce[X])(f :(E, X) => IterableOnce[O]) :CC[O] = {
			val l = self.iterator
			val r = that.iterator
			val res = self.iterableFactory.newBuilder[O]
			while (l.hasNext && r.hasNext)
				res ++= f(l.next(), r.next())
			res.result()
		}

		/** Equivalent to [[net.noresttherein.sugar.collections.extensions.IterableExtension.zipFlatMap zipFlatMap]],
		  * but throws a [[NoSuchElementException]] if the collections are of different sizes.
		  */
		def zipFlatMapAll[X, O](that :IterableOnce[X])(f :(E, X) => IterableOnce[O]) :CC[O] = {
			val l = self.iterator
			val r = that.iterator
			val res = self.iterableFactory.newBuilder[O]
			while (l.hasNext && r.hasNext)
				res ++= f(l.next(), r.next())
			if (l.hasNext)
				throw new NoSuchElementException("Cannot zipFlatMapAll: left collection has more elements than the right one.")
			else if (r.hasNext)
				throw new NoSuchElementException("Cannot zipFlatMapAll: right collection has more elements than the left one.")
			res.result()
		}

		/** Equivalent to `this.zipAll(that, thisElem, thatElem).map(f)`, but happens in one step and the argument function
		  * takes two arguments instead of a pair, which makes it possible to use with lambda placeholder parameters.
		  */
		def zipFlatMapAll[X, O](that :IterableOnce[X], thisElem :E, thatElem :X)(f :(E, X) => IterableOnce[O]) :CC[O] = {
			val l = self.iterator
			val r = that.iterator
			val res = self.iterableFactory.newBuilder[O]
			while (l.hasNext && r.hasNext)
				res ++= f(l.next(), r.next())
			while (l.hasNext)
				res ++= f(l.next(), thatElem)
			while (r.hasNext)
				res ++= f(thisElem, r.next())
			res.result()
		}


		/** Maps the elements of the collection and reverses their order. The order in which the mapping function
		  * will be applied to the elements is undefined and depends on the runtime type of this collection.
		  * Note that if this collection is unordered, the order of the elements in the mapped collection
		  * is likewise undefined and depends on the implementation of this collection's builder.
		  * This operation is faster than `this.map(f).reverse`.
		  */
		def mapReverse[O](f :E => O) :CC[O] = self match {
			case _ if self.isEmpty =>
				self.iterableFactory.empty
			case list :List[E] =>
				@tailrec def mapList(unmapped :List[E], mapped :List[O]) :List[O] = unmapped match {
					case h::t => mapList(t, f(h)::mapped)
					case _ => mapped
				}
				mapList(list, Nil).asInstanceOf[CC[O]]
			case list :LinearSeq[E] =>
				@tailrec def mapLinear(unmapped :LinearSeq[E], mapped :LinearSeq[O]) :LinearSeq[O] =
					if (unmapped.isEmpty) mapped
					else mapLinear(unmapped.tail, f(unmapped.head) +: mapped)
				mapLinear(list, list.iterableFactory.empty).asInstanceOf[CC[O]]
			case seq :scala.collection.IndexedSeq[E] =>
				def mapIndexed() = {
					val b = self.iterableFactory.newBuilder[O]
					var i = seq.length
					while (i > 0) {
						i -= 1
						b += f(seq(i))
					}
					b.result()
				}
				mapIndexed()
			case seq :scala.collection.Seq[E] =>
				def mapSeq() = {
					val i = seq.reverseIterator
					val b = self.iterableFactory.newBuilder[O]
					b sizeHint self
					while (i.hasNext)
						b += f(i.next())
					b.result()
				}
				mapSeq()
			case _ =>
				def mapIterable() = {
					val mapped = (List.empty[O] /: self){ (acc, e) => f(e)::acc }
					val b = self.iterableFactory.newBuilder[O]
					b sizeHint self
					b ++= mapped
					b.result()
				}
				mapIterable()
		}


		/** Similar to [[scala.collection.IterableOps.zip zip]], except it zip3 three collections at once. */
		def zip3[A, B](second :IterableOnce[A], third :IterableOnce[B]) :CC[(E, A, B)] = {
			val size1 = self.knownSize
			val size2 = second.knownSize
			val size3 = third.knownSize
			if (size1 == 0 || size2 == 0 || size3 == 0)
				self.iterableFactory.empty
			else {
				val i1 = self.iterator
				val i2 = second.iterator
				val i3 = third.iterator
				val res = self.iterableFactory.newBuilder[(E, A, B)]
				if (size1 >= 0 & size2 >= 0 & size3 >= 0)
					res sizeHint Math.min(size1, Math.min(size2, size3))
				while (i1.hasNext && i2.hasNext & i3.hasNext)
					res += ((i1.next(), i2.next(), i3.next()))
				res.result()
			}
		}

		/** Similar to [[scala.collection.IterableOps.zipAll zipAll]], but zips three collections at once. */
		def zipAll3[U >: E, A, B](second :IterableOnce[A], third :IterableOnce[B],
		                          thisElem :U, secondElem :A, thirdElem :B) :CC[(U, A, B)] =
		{
			val size1 = self.knownSize
			val size2 = self.knownSize
			val size3 = self.knownSize
			if (size1 == 0 & size2 == 0 & size3 == 0)
				self.iterableFactory.empty
			else {
				val i1 = self.iterator
				val i2 = second.iterator
				val i3 = third.iterator
				val res = self.iterableFactory.newBuilder[(U, A, B)]
				if (size1 >= 0 & size2 >= 0 & size3 >= 0)
					res sizeHint Math.max(size1, Math.max(size2, size3))
				while (i1.hasNext && i2.hasNext && i3.hasNext)
					res += ((i1.next(), i2.next(), i3.next()))
				var has1 = i1.hasNext
				var has2 = i2.hasNext
				var has3 = i3.hasNext
				var e = thisElem
				var a = secondElem
				var b = thirdElem
				while (has1 | has2 | has3) {
					if (has1) {
						e = i1.next()
						has1 = i1.hasNext
					} else
						e = thisElem
					if (has2) {
						a = i2.next()
						has2 = i2.hasNext
					} else
						a = secondElem
					if (has3) {
						b = i3.next()
						has3 = i3.hasNext
					} else
						b = thirdElem
					res += ((e, a, b))
				}
				res.result()
			}
		}

		/** An immutable array with the contents of this collection. */
		def toIArray[A >: E :ClassTag] :IArray[E] = self.toArray[A].asInstanceOf[IArray[E]]

		/** Creates an `Array[AnyRef]` with elements of this collection, and passes it as an `Array[E]`. */
		def toRefArray[A >: E] :Array[E] = self.toArray[Any].asInstanceOf[Array[E]]

	}



	/** Extension methods of mutable and immutable sequences (and arrays through a wrapper):
	  *   1. alternative, safer implementations of [[scala.collection.SeqOps.indexOf indexOf]],
	  *      which do not return a negative index when the element is not found;
	  *   1. methods related to subsequences: sequences containing selected elements from another sequence,
	  *      in the same order.
	  */
	class SeqExtension[E, CC[X], C] private[extensions]
	                  (private val self :scala.collection.SeqOps[E, CC, C]) extends AnyVal
	{
		@inline private def length :Int = self.length


		/** Checks if the elements in this sequence follow the implicit ordering.
		  * @return [[net.noresttherein.sugar.collections.extensions.SeqExtension.isSortedWith isSortedWith]]`(implicitly[Ordering[U]].compare(_, _) <= 0)`
		  */
		def isSorted[U >: E :Ordering] :Boolean = {
			val order = implicitly[Ordering[U]]
			isSortedWith(order.compare(_, _) <= 0)
		}

		/** Checks if the elements in this sequence are sorted by the given ordered property.
		  * @return [[net.noresttherein.sugar.collections.extensions.SeqExtension.isSortedWith isSortedWith]]`(implicitly[Ordering[U]].compare(by(_), by(_)) <= 0)`
		  */
		def isSortedBy[U :Ordering](by :E => U) :Boolean = {
			val order = implicitly[Ordering[U]]
			isSortedWith((x, y) => order.compare(by(x), by(y)) <= 0)
		}

		/** Checks if the given predicate holds for all consecutive element pairs in this sequence.
		  * Note that that the function argument must implement `<=`,
		  * rather than `<` as in `Seq`.[[collection.Seq.sortWith sortWith]].
		  * @param lte a function imposing a full order on the elements. It must have consistent `<=` semantics.
		  */
		def isSortedWith[U >: E](lte :(U, U) => Boolean) :Boolean =
			self.sizeIs <= 1 || {
				self match {
					case _ :LinearSeq[E] =>
						lte(self.head, self(1)) && {
							val view = self.view
							view.zip(view.tail).forall { case (left, right) => lte(left, right) }
						}
					case _ :collection.IndexedSeq[E] =>
						var i = self.length - 1
						var second = self(i); i -= 1
						var first = self(i)
						while (i >= 1 && lte(first, second)) {
							second = first
							i -= 1
							first = self(i)
						}
						i == 0 && lte(first, second)
					case _ => (self to ArraySeq.untagged).isSortedWith(lte)
				}
			}

		/** Checks if each element in this sequence is strictly greater than the previos one,
		  * according to the implicit `Ordering`. Note that this is not the same as
		  * [[net.noresttherein.sugar.collections.extensions.SeqExtension.isSorted isSorted]],
		  * which accepts also equal elements.
		  */
		def isIncreasing[U >: E :Ordering] :Boolean = {
			val order = implicitly[Ordering[U]]
			isSortedWith(order.compare(_, _) < 0)
		}

		/** Checks if each element in this sequence is strictly lesser than the previous one,
		  * according to the implicit `Ordering`. */
		def isDecreasing[U >: E :Ordering] :Boolean = {
			val order = implicitly[Ordering[U]]
			isSortedWith(order.compare(_, _) > 0)
		}

		/** Reorders the elements of this sequence in such a manner that all permutations are equally probable. */
		def shuffle(implicit random :Random) :CC[E] =
			if (self.sizeIs <= 1)
				self.iterableFactory.from(self)
			else {
				val result = new Array[Any](self.length).castFrom[Array[Any], Array[E]]
				self.copyToArray(result)
				var i = result.length
				while (i > 1) {
					val j = random.nextInt(i)
					val boo = result(j)
					i -= 1
					result(j) = result(i)
					result(i) = boo
				}
				self.iterableFactory.from(ArraySeq.unsafeWrapArray(result))
			}

		/** The reverse of [[scala.collection.IterableOnceOps.slice slice]]: cuts out a segment of this collection
		  * with elements starting with element `from` and ending before `until`.
		  * For indices in range, it is equivalent to `this.take(from) ++ this.drop(until)`, but possibly faster.
		  * Specifying `until <= from` results in returning the same collection.
		  */
		//There is no good way of implementing it to return C, because we know nothing about it, and, for all we know,
		// it is possible that self is not an instance of C - ArrayAsSeq, for example. We don't even know if C is a Seq.
		def splice(from :Int, until :Int) :CC[E] =
			if (until <= 0 | until <= from)
				self.iterableFactory.from(self)
			else {
				val size = self.knownSize
				if (size >= 0 && from >= size)
					self.iterableFactory.from(self)
				else
					self.patch(from, Nil, until - Math.max(from, 0))
			}

		/** Finds the location of the given element in this sequence, returning its index as an `Opt`.
		  * @param x    the element, whose index is to be determined.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def getIndexOf(x :E, from :Int = 0) :Opt[Int] = self.indexOf(x, from) match {
			case -1 => Lack
			case  n => Got(n)
		}
		/** Finds the last location of the given element in this sequence, returning its index as an `Opt`.
		  * @param x   the element, whose index is to be determined.
		  * @param end the upper, inclusive bound on the returned index.
		  */
		@inline def getLastIndexOf(x :E, end :Int = length - 1) :Opt[Int] = self.lastIndexOf(x, end) match {
			case -1 => Lack
			case  n => Got(n)
		}
		/** Finds an element of this sequence which satisfies the predicate, returning its index as an `Opt`.
		  * @param p    a function applied consecutively to all elements with indices greater or equal `from`.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def getIndexWhere(p :E => Boolean, from :Int = 0) :Opt[Int] = self.indexWhere(p, from) match {
			case -1 => Lack
			case  n => Got(n)
		}
		/** Finds the last element of this sequence which satisfies the predicate, returning its index as an `Opt`.
		  * @param p   a function applied consecutively to all elements with indices lesser or equal `end`,
		  *            in a decreasing order.
		  * @param end the upper, inclusive bound on the returned index; elements after this position will not be checked.
		  */
		@inline def getLastIndexWhere(p :E => Boolean, end :Int = length - 1) :Opt[Int] =
			self.lastIndexWhere(p, end) match {
				case -1 => Lack
				case  n => Got(n)
			}

		/** Finds the location of the given element in this sequence, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param x    the element, whose index is to be determined.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		def sureIndexOf(x :E, from :Int = 0) :Int = self.indexOf(x, from) match {
			case -1 => throw new NoSuchElementException(indexOfErrorMessage(x, from))
			case  n => n
		}
		/** Finds the last location of the given element in this sequence, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param x   the element, whose index is to be determined.
		  * @param end the upper, inclusive bound on the returned index.
		  */
		def sureLastIndexOf(x :E, end :Int = length - 1) :Int = self.lastIndexOf(x, end) match {
			case -1 => throw new NoSuchElementException(lastIndexOfErrorMessage(x, end))
			case  n => n
		}
		/** Finds an element of this sequence which satisfies the predicate, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param p    a function applied consecutively to all elements with indices greater or equal `from`,
		  *             until satisfied.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		def sureIndexWhere(p :E => Boolean, from :Int = 0) :Int = self.indexWhere(p, from) match {
			case -1 => throw new NoSuchElementException(indexWhereErrorMessage(from))
			case  n => n
		}
		/** Finds the last element of this sequence which satisfies the predicate, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param p   a function applied consecutively to all elements, starting with the one at position `end`,
		  *            until satisfied.
		  * @param end the upper, inclusive bound on the returned index; elements after this index are not checked.
		  */
		def sureLastIndexWhere(p :E => Boolean, end :Int = length - 1) :Int =
			self.lastIndexWhere(p, end) match {
				case -1 => throw new NoSuchElementException(lastIndexWhereErrorMessage(end))
				case  n => n
			}

		/** Finds the location of the given element in this sequence, throwing an exception `E`,
		  * given as the type parameter, if it does not exist.
		  * @tparam T   the exception to be thrown; it must contain either `(String, Throwable)` or `(String)`
		  *             constructor.
		  * @param x    the element, whose index is to be determined.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		def indexOrThrow[T <: Throwable :ClassTag](x :E, from :Int = 0) :Int =
			self.indexOf(x, from) match {
				case -1 => raise[T](indexOfErrorMessage(x, from))
				case n if n >= 0 => n
			}
		/** Finds the last location of the given element in this sequence, throwing an exception `E`,
		  * given as the type parameter, if it does not exist.
		  * @tparam T  the exception to be thrown; it must contain either `(String, Throwable)` or `(String)`
		  *            constructor.
		  * @param x   the element, whose index is to be determined.
		  * @param end the upper, inclusive bound on the returned index.
		  */
		def lastIndexOrThrow[T <: Throwable :ClassTag](x :E, end :Int = length - 1) :Int =
			self.lastIndexOf(x, end) match {
				case -1 => raise[T](lastIndexOfErrorMessage(x, end))
				case  n => n
			}
		/** Finds an element of this sequence which satisfies the predicate, throwing an exception `E`,
		  * given as the type parameter, if it does not exist.
		  * @tparam T   the exception to be thrown; it must contain either `(String, Throwable)` or `(String)`
		  *             constructor.
		  * @param p    a function applied consecutively to all elements with indices greater or equal `from`,
		  *             until satisfied.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		def indexWhereOrThrow[T <: Throwable :ClassTag](p :E => Boolean, from :Int = 0) :Int =
			self.indexWhere(p, from) match {
				case -1 => raise[T](indexWhereErrorMessage(from))
				case  n => n
			}
		/** Finds the last element of this sequence which satisfies the predicate, throwing an exception `E`,
		  * given as the type parameter, if it does not exist.
		  * @tparam T  the exception to be thrown; it must contain either `(String, Throwable)` or `(String)`
		  *            constructor.
		  * @param p   a function applied consecutively to all elements, starting with the one at position `end`,
		  *            until satisfied.
		  * @param end the upper, inclusive bound on the returned index; elements after this index are not checked.
		  */
		def lastIndexWhereOrThrow[T <: Throwable :ClassTag](p :E => Boolean, end :Int = length - 1) :Int =
			self.lastIndexWhere(p, end) match {
				case -1 => raise[T](lastIndexWhereErrorMessage(end))
				case  n => n
			}

		/** Returns `this.indexOf(x, from)`, adding an assertion that the result is not negative
		  * (the element has been found).
		  * @param x    an element of this collection.
		  * @param from an inclusive lower bound on the index of the searched element.
		  */
		def assertIndexOf(x :E, from :Int = 0) :Int = assertIndexOf(x, from, indexOfErrorMessage(x, from))
		/** Returns `this.indexOf(x)`, adding an assertion that the result is not negative (the element has been found).
		  * @param x    an element of this collection.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertIndexOf(x :E, msg: => String) :Int = assertPresent(self.indexOf(x), msg)
		/** Returns `this.indexOf(x, from)`, adding an assertion that the result is not negative
		  * (the element has been found).
		  * @param x    an element of this collection.
		  * @param from an inclusive lower bound on the index of the searched element.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertIndexOf(x :E, from :Int, msg: => String) :Int = assertPresent(self.indexOf(x, from), msg)
		/** Returns `this.lastIndexOf(x, end)`, adding an assertion that the result is not negative
		  * (the element has been found).
		  * @param x    an element of this collection.
		  * @param end  an inclusive upper bound on the index of the searched element.
		  */
		def assertLastIndexOf(x :E, end :Int = length - 1) :Int =
			assertLastIndexOf(x, end, lastIndexOfErrorMessage(x, end))
		/** Returns `this.lastIndexOf(x)`, adding an assertion that the result is not negative
		  * (the element has been found).
		  * @param x    an element of this collection.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertLastIndexOf(x :E, msg: => String) :Int = assertPresent(self.lastIndexOf(x), msg)
		/** Returns `this.lastIndexOf(x, end)`, adding an assertion that the result is not negative
		  * (the element has been found).
		  * @param x    an element of this collection.
		  * @param end  an inclusive upper bound on the index of the searched element.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertLastIndexOf(x :E, end :Int, msg: => String) :Int = assertPresent(self.lastIndexOf(x, end), msg)

		/** Returns `this.indexWhere(p, from)`, adding an assertion that the result is not negative
		  * (a satisfying element has been found).
		  * @param p    a function applied consecutively to all elements, in the increasing order of indices.
		  * @param from an inclusive lower bound on the index of the searched element.
		  */
		def assertIndexWhere(p :E => Boolean, from :Int = 0) :Int =
			assertIndexWhere(p, from, indexWhereErrorMessage(from))
		/** Returns `this.indexWhere(p)`, adding an assertion that the result is not negative (a satisfying element
		  * has been found).
		  * @param p   a function applied consecutively to all elements, in the increasing order of indices.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertIndexWhere(p :E => Boolean, msg: => String) :Int = assertPresent(self.indexWhere(p), msg)
		/** Returns `this.indexWhere(p, from)`, adding an assertion that the result is not negative (a satisfying element
		  * has been found).
		  * @param p    a function applied consecutively to all elements, starting with index `from`,
		  *             until satisfied.
		  * @param from an index from which to start checking; elements if lower indices are not considered.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertIndexWhere(p :E => Boolean, from :Int, msg: => String) :Int =
			assertPresent(self.indexWhere(p, from), msg)

		/** Returns `this.lastIndexWhere(p, end)`, adding an assertion that the result is not negative
		  * (a satisfying element has been found).
		  * @param p    a function applied consecutively to all elements in a decreasing order of indices,
		  *             starting with the one at position `end`, until satisfied.
		  * @param end  the upper, inclusive bound on the returned index; elements after this index are not checked.
		  */
		def assertLastIndexWhere(p :E => Boolean, end :Int = length - 1) :Int =
			assertPresent(self.lastIndexWhere(p, end), lastIndexWhereErrorMessage(end))

		/** Returns `this.lastIndexWhere(p)`, adding an assertion that the result is not negative (a satisfying element
		  * has been found).
		  * @param p   a function applied consecutively to all elements in a decreasing order of indices,
		  *            starting with the one at position `end`, until satisfied.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertLastIndexWhere(p :E => Boolean, msg: => String) :Int =
			assertPresent(self.lastIndexWhere(p), msg)
		/** Returns `this.lastIndexWhere(p, end)`, adding an assertion that the result is not negative
		  * (a satisfying element has been found).
		  * @param p   a function applied consecutively to all elements in a decreasing order of indices,
		  *            starting with the one at position `end`, until satisfied.
		  * @param end the upper, inclusive bound on the returned index; elements after this index are not checked.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertLastIndexWhere(p :E => Boolean, end :Int, msg: => String) :Int =
			assertPresent(self.lastIndexWhere(p, end), msg)

		@inline private[collections] def assertPresent(i :Int, msg: => String) :Int = {
			assert(i >= 0, msg)
			i
		}

		/** Finds the location of the given subsequence in this sequence, returning its index as an `Opt`.
		  * @param that a presumed consecutive subsequence of this sequence.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  * @return `Opt(this.indexOfSlice(that, from)).filter(_ >= 0)`.
		  */
		@inline def getIndexOfSlice[B >: E](that :Seq[B], from :Int = 0) :Opt[Int] =
			self.indexOfSlice(that, from) match {
				case -1 => Lack
				case  n => Got(n)
			}
		/** Finds the last location of the given subsequence in this sequence, returning its index as an `Opt`.
		  * @param that a presumed consecutive subsequence of this sequence.
		  * @param end  the upper, inclusive bound on the returned index.
		  * @return `Opt(this.lastIndexOfSlice(that, end)).filter(_ >= 0)`.
		  */ //Note that Seq(1).lastIndexOf(Nil) == 1, so end must start with length, not length - 1
		@inline def getLastIndexOfSlice[B >: E](that :Seq[B], end :Int = length) :Opt[Int] =
			self.lastIndexOfSlice(that, end) match {
				case -1 => Lack
				case  n => Got(n)
			}

		/** Finds the location of the given subsequence in this sequence, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param that a presumed consecutive subsequence of this sequence.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  * @return `this.indexOfSlice(that, from)`.
		  */
		def sureIndexOfSlice[B >: E](that :Seq[B], from :Int = 0) :Int =
			self.indexOfSlice(that, from) match {
				case -1 => throw new NoSuchElementException(indexOfSliceErrorMessage(that, from))
				case  n => n
			}
		/** Finds the last location of the given subsequence in this sequence, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param that a presumed consecutive subsequence of this sequence.
		  * @param end  the upper, inclusive bound on the returned index.
		  * @return `this.lastIndexOfSlice(that, from)`.
		  */
		def sureLastIndexOfSlice[B >: E](that :Seq[B], end :Int = length) :Int =
			self.lastIndexOfSlice(that, end) match {
				case -1 => throw new NoSuchElementException(lastIndexOfSliceErrorMessage(that, end))
				case  n => n
			}

		/** Returns `this.indexOfSlice(that, from)`, adding an assertion that the result is not negative
		  * (the subsequence has been found).
		  * @param that a presumed consecutive subsequence of this sequence.
		  * @param from an inclusive lower bound on the index of the searched element.
		  */
		def assertIndexOfSlice[B >: E](that :Seq[B], from :Int = 0) :Int =
			assertIndexOfSlice(that, from, indexOfSliceErrorMessage(that, from))

		/** Returns `this.indexOfSlice(that, from)`, adding an assertion that the result is not negative
		  * (the subsequence has been found).
		  * @param that a presumed consecutive subsequence of this sequence.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertIndexOfSlice[B >: E](that :Seq[B], msg: => String) :Int =
			assertPresent(self.indexOfSlice(that), msg)

		/** Returns `this.indexOfSlice(that, from)`, adding an assertion that the result is not negative
		  * (the subsequence has been found).
		  * @param that a presumed consecutive subsequence of this sequence.
		  * @param from an inclusive lower bound on the index of the searched element.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertIndexOfSlice[B >: E](that :Seq[B], from :Int, msg: => String) :Int =
			assertPresent(self.indexOfSlice(that, from), msg)

		/** Returns `this.lastIndexOfSlice(that, end)`, adding an assertion that the result is not negative
		  * (the subsequence has been found).
		  * @param that a presumed consecutive subsequence of this sequence.
		  * @param end  the upper, inclusive bound on the returned index; elements after this index are not checked.
		  */
		def assertLastIndexOfSlice[B >: E](that :Seq[B], end :Int = length) :Int =
			assertLastIndexOfSlice(that, end, lastIndexOfSliceErrorMessage(that, end))

		/** Returns `this.lastIndexOfSlice(that, end)`, adding an assertion that the result is not negative
		  * (the subsequence has been found).
		  * @param that a presumed consecutive subsequence of this sequence.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertLastIndexOfSlice[B >: E](that :Seq[B], msg: => String) :Int =
			assertPresent(self.lastIndexOfSlice(that), msg)

		/** Returns `this.lastIndexOfSlice(that, end)`, adding an assertion that the result is not negative
		  * (the subsequence has been found).
		  * @param that a presumed consecutive subsequence of this sequence.
		  * @param end  the upper, inclusive bound on the returned index; elements after this index are not checked.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertLastIndexOfSlice[B >: E](that :Seq[B], end :Int, msg: => String) :Int =
			assertPresent(self.lastIndexOfSlice(that, end), msg)


		private[collections] def indexOfErrorMessage(x :E, from :Int) :String =
			"No " + x + " in " + self + (if (from == 0) "." else " at or after index " + from + ".")

		private[collections] def lastIndexOfErrorMessage(x :E, end :Int) :String =
			"No " + x + " in " + self + (if (end == length - 1) "." else " at or before index " + end + ".")

		private[collections] def indexWhereErrorMessage(from :Int) :String =
			"No element satisfying the predicate in " + self +
				(if (from == 0) "." else " at or after index " + from + ".")

		private[collections] def lastIndexWhereErrorMessage(end :Int) :String =
			"No element satisfying the predicate in " + self +
				(if (end == length - 1) "." else " at or before index " + end + ".")

		private[collections] def indexOfSliceErrorMessage[B >: E](that :Seq[B], from :Int) :String =
			"No " + that + " in " + self + (if (from == 0) "." else " at or after index " + from + ".")

		private[collections] def lastIndexOfSliceErrorMessage[B >: E](that :Seq[B], end :Int) :String =
			"No " + that + " in " + self + (if (end == length - 1) "." else " at or before index " + end + ".")

		/** Checks if this sequence is a subsequence of `other`, that is there is a function `f :Int => Boolean` such that
		  * `this == other.zipWithIndex.collect { case (e, i) if f(i) => e }`.
		  */
		def subseqOf(other :scala.collection.Seq[E]) :Boolean = {
			val thisLength = self.knownSize
			val thatLength = other.knownSize
			@tailrec
			def sublistOf(left :scala.collection.Seq[E], right :scala.collection.Seq[E]) :Boolean =
				left.isEmpty || right.nonEmpty && (
					if (left.head == right.head) sublistOf(left.tail, right.tail)
					else sublistOf(left, right.tail)
				)
			@tailrec def indexedSubseqOf(left :scala.collection.SeqOps[E, CC, C], leftIdx :Int,
			                             right :scala.collection.Seq[E], rightIdx :Int) :Boolean =
				leftIdx < 0 || rightIdx >= 0 && (
					if (left(leftIdx) == right(rightIdx)) indexedSubseqOf(left, leftIdx - 1, right, rightIdx - 1)
					else indexedSubseqOf(left, leftIdx, right, rightIdx - 1)
				)
			def subIteratorOf(left :Iterator[E], right :Iterator[E]) :Boolean = {
				!left.hasNext || right.hasNext && {
					var l = left.next()
					var r = right.next()
					while (left.hasNext && right.hasNext) {
						if (l == r) {
							l = left.next()
							r = right.next()
						} else
							r = right.next()
					}
					l == r && !left.hasNext
				}
			}
			(thatLength < 0 || thisLength <= thatLength && thisLength >= 0) &&
				((self, other) match {
					case (_ :scala.collection.IndexedSeq[_], _ :scala.collection.IndexedSeq[_]) =>
						indexedSubseqOf(self, thisLength - 1, other, thatLength - 1)
					case (list1 :LinearSeq[E], _ :LinearSeq[E]) =>
						sublistOf(list1, other)
					case _ =>
						subIteratorOf(self.iterator, other.iterator)
			})
		}

		/** Checks if this sequence is a subsequence of `other`, that is there is a function `f :Int => Boolean`
		  * such that `this == other.zipWithIndex.collect { case (e, i) if f(i) => e }`.
		  */
		def subseqOf(other :Array[E]) :Boolean = subseqOf(ArraySeq.unsafeWrapArray(other))

		/** A new sequence `s`, consisting of all elements of this sequence, such that `this(i) == s(permutation(i))`. */
		def reorder(permutation :Permutation) :CC[E] = permutation(self.toSeq) to self.iterableFactory
	}


	/** Extension methods for indexed sequences, adding binary search and operations working on the end of the sequence. */
	class IndexedSeqExtension[E, CC[X], C] private[extensions]
	                         (private val self :collection.IndexedSeqOps[E, CC, C])
		extends AnyVal
	{
		/** Takes the longest suffix of this sequence satisfying the predicate. */
		def takeRightWhile(f :E => Boolean) :C = {
			var i = self.length - 1
			if (i <= IndexedSeqDefaults.defaultApplyPreferredMaxLength) {
				while (i >= 0 && f(self(i)))
					i -= 1
				self.drop(i + 1)
			} else {
				val it = self.reverseIterator
				while (i >= 0 && f(it.next()))
					i -= 1
				self.drop(i + 1)
			}
		}

		/** Drops the longest suffix of this sequence satisfying the predicate. */
		def dropRightWhile(f :E => Boolean) :C = {
			var i = self.length - 1
			if (i <= IndexedSeqDefaults.defaultApplyPreferredMaxLength) {
				while (i >= 0 && f(self(i)))
					i -= 1
				self.take(i + 1)
			} else {
				val it = self.reverseIterator
				while (i >= 0 && f(it.next()))
					i -= 1
				self.take(i + 1)
			}
		}

		/** Performs binary search of element `x` in this sequence, sorted according to implicit `Ordering[E]`.
		  * Returns the index of the first element greater or equal `x`, or `this.length` if all elements
		  * are strictly less than `x`. If `elements` is not sorted, or `ordering` is not consistent with `equals`,
		  * then the behaviour is unspecified.
		  * @return `n :Int` such that `0 <= n && n <= this.length && (n == this.length || this(n) == x)`
		  *         and forall `0 <= i < n` `this(i) < x`.
		  */
		final def binarySearch(x :E)(implicit ordering :Ordering[E]) :Int =
			binarySearch(x, 0, self.length)

		/** Performs binary search of element `x` in a slice of a sequence sorted according to implicit `Ordering[E]`.
		  * If `until < from`, then `from` is returned immediately. Otherwise an index `from <= n < until`
		  * of the first element greater or equal `x`, or `this.length` if all elements in the `[from,until)` range
		  * are strictly less than `x`. If `this` is not sorted, or `ordering` is not consistent with `equals`,
		  * then the behaviour is unspecified.
		  */
		@throws[IndexOutOfBoundsException]("if from < 0 or from > elements.length or until > elements.length.")
		final def binarySearch(x :E, from :Int, until :Int)(implicit ordering :Ordering[E]) :Int = {
			if (from < 0 | from > self.length | until > self.length)
				throw new IndexOutOfBoundsException(
					"binarySearch([" + self.length + "], " + from + ", " + until + ")"
				)
			@tailrec def search(start :Int, end :Int) :Int =
				if (start >= end)
					start
				else {
					val middle = (start + end) >> 1
					ordering.compare(x, self(middle)) match {
						case 1 => search(middle + 1, end)
						case _ => search(start, middle)
					}
				}
			search(from, until)
		}
	}


	//Consider: removing it. These extensions duplicate those we get from IndexedSeqExtension
	// by promoting an Array to ArrayAsSeq
	/** Adds binary search extension methods to all arrays. */
	class ArrayExtension[E] private[collections] (private val self :Array[E]) extends AnyVal {

		/** Wraps this array in an adapter to the standard `IndexedSeq` API. */
		def toOps :mutable.IndexedSeqOps[E, Array, Array[E]] = new ArrayAsSeq(self)

		/** Performs binary search of element `x` in this array, sorted according to implicit `Ordering[E]`.
		  * Returns the index of the first element greater or equal `x`, or `array.length` if all elements
		  * are strictly less than `x`. If the array is not sorted, or the `Ordering` is not consistent with `equals`,
		  * then the behaviour is unspecified.
		  * @return `n :Int` such that `0 <= n && n <= this.length && (n == this.length || this(n) == x)`
		  *         and forall `0 <= i < n` `this(i) < x`.
		  */
		final def binarySearch(x :E)(implicit ordering :Ordering[E]) :Int = binarySearch(x, 0, self.length)

		/** Performs binary search of element `x` in a section of this array, sorted according to implicit `Ordering[E]`.
		  * If `until < from`, then `from` is returned immediately. Otherwise an index `from <= n < until`
		  * of the first element greater or equal `x`, or `this.length` if all elements in the `[from,until)` range
		  * are strictly less than `x`. If the array is not sorted, or the `Ordering` is not consistent with `equals`,
		  * then the behaviour is unspecified.
		  */
		@throws[IndexOutOfBoundsException]("if from < 0 or from > array.length or until > array.length.")
		final def binarySearch(x :E, from :Int, until :Int)(implicit ordering :Ordering[E]) :Int = {
			if (from < 0 | from > self.length | until > self.length)
				throw new IndexOutOfBoundsException(
					"[" + self.length + "].binarySearch(" + from + ", " + until + ")"
				)
			@tailrec def search(start :Int, end :Int) :Int =
				if (start >= end)
					start
				else {
					val middle = (start + end) >> 1
					ordering.compare(x, self(middle)) match {
						case 1 => search(middle + 1, end)
						case _ => search(start, middle)
					}
				}
			search(from, until)
		}


		/** Equality of corresponding elements in the two arrays. Array component type is ignored, and an array
		  * of a primitive type will equal an array of the appropriate box class, as long as the actual values are equal.
		  */
		def sameElements(that :Array[_]) :Boolean =
			(self eq that) || (self.length == that.length) && {
				val elemType = self.getClass.getComponentType
				def slowEquals = {
					var i = that.length - 1
					while (i > 0 && self(i) == that(i))
						i -= 1
					true
				}
				if (elemType == that.getClass.getComponentType)
					if (classOf[AnyRef] isAssignableFrom elemType)
						Arrays.equals(self.asInstanceOf[Array[AnyRef]], that.asInstanceOf[Array[AnyRef]])
					else if (elemType == classOf[Int])
						Arrays.equals(self.asInstanceOf[Array[Int]], that.asInstanceOf[Array[Int]])
					else if (elemType == classOf[Long])
						Arrays.equals(self.asInstanceOf[Array[Long]], that.asInstanceOf[Array[Long]])
					else if (elemType == classOf[Double])
						Arrays.equals(self.asInstanceOf[Array[Double]], that.asInstanceOf[Array[Double]])
					else if (elemType == classOf[Byte])
						Arrays.equals(self.asInstanceOf[Array[Byte]], that.asInstanceOf[Array[Byte]])
					else if (elemType == classOf[Char])
						Arrays.equals(self.asInstanceOf[Array[Char]], that.asInstanceOf[Array[Char]])
					else if (elemType == classOf[Float])
						Arrays.equals(self.asInstanceOf[Array[Float]], that.asInstanceOf[Array[Float]])
					else if (elemType == classOf[Boolean])
						Arrays.equals(self.asInstanceOf[Array[Boolean]], that.asInstanceOf[Array[Boolean]])
					else
						slowEquals
				else
					slowEquals
			}

		/** Deep equality of array elements and array component (element) types. */
		def sameAs(that :Any) :Boolean = that match {
			case self :AnyRef if this.asAnyRef eq self => true
			case other :Array[_]
				if other.length == self.length && other.getClass.getComponentType == self.getClass.getComponentType
			=>
				this sameElements other
			case _ => false
		}
	}


	/** Adds an [[net.noresttherein.sugar.collections.extensions.MapExtension.updatedIfAbsent updatedIfAbsent]]
	  *  extension method to [[scala.collection.immutable.Map immutable.Map]].
	  */
	class MapExtension[K, V, M[A, +B] <: MapOps[A, B, M, M[A, B]]] private[extensions] (private val self :M[K, V])
		extends AnyVal
	{
		@inline def updatedIfAbsent[U >: V](key :K, value :U) :M[K, U] =
			if (self.contains(key)) self else self.updated(key, value)

		/** Same as [[net.noresttherein.sugar.collections.extensions.MapExtension.updatedIfAbsent updatedIfAbsent]]. */
		@inline def ?=[U >: V](entry :(K, U)) :M[K, U] = updatedIfAbsent(entry._1, entry._2)
	}


	/** A light wrapper over [[scala.collection.StepperShape StepperShape]] evidence. It is introduced
	  * because implicit `StepperShape` values resolve well when the element type `A` is given, and the stepper type `S`
	  * needs to be provided, but not vice versa. The companion object defines implicit values providing:
	  *   - `[Int, IntStepper]`
	  *   - `[Long, LongStepper]`
	  *   - `[Double, DoubleStepper]`
	  *   - `[T, AnyStepper[T]]`
	  */
	@implicitNotFound("Cannot determine the element type ${A} of ${S}: is the latter a Stepper?")
	class StepperType[A, S <: Stepper[_]] private[extensions](val stepperShape :StepperShape[A, S])
		extends AnyVal
	{
		def shape :StepperShape.Shape = stepperShape.shape
	}

	private[extensions] sealed abstract class Rank1PreferredStepperShapes {
		implicit def compatibleStepperType[A, S <: Stepper[_]](implicit shape :StepperShape[A, S]) =
			new StepperType[A, S](shape)
	}
	object StepperType extends Rank1PreferredStepperShapes {
		implicit def anyStepperType[T, S <: AnyStepper[T]](implicit ev :S <:< AnyStepper[T]) :StepperType[T, S] =
			new StepperType[T, S](StepperShape.anyStepperShape.asInstanceOf[StepperShape[T, S]])

		implicit def intStepperType[S <: IntStepper] :StepperType[Int, S] =
			new StepperType[Int, S](StepperShape.intStepperShape.asInstanceOf[StepperShape[Int, S]])

		implicit def longStepperType[S <: LongStepper] :StepperType[Long, S] =
			new StepperType[Long, S](StepperShape.longStepperShape.asInstanceOf[StepperShape[Long, S]])

		implicit def doubleStepperType[S <: DoubleStepper] :StepperType[Double, S] =
			new StepperType[Double, S](StepperShape.doubleStepperShape.asInstanceOf[StepperShape[Double, S]])
	}

	/** Adds a `++` extension method to any `Stepper[_]`. */
	class StepperExtension[S <: Stepper[_]] private[extensions] (private val self :S) extends AnyVal {
		/** Combines this stepper with the argument, returning a stepper of the same specialization as both.
		  * This will work only for the built in Scala steppers.
		  */
		def ++[A, S2 >: S <: Stepper[_]](next :S2)(implicit itemType :StepperType[A, S2]) :S2 =
			ConcatStepper(self, next)(itemType)
	}


	/** An extension method [[net.noresttherein.sugar.collections.extensions.BuilderExtension.mapInput mapInput]]
	  * which adapts the builder to a new element type.
	  */
	class BuilderExtension[E, C] private[extensions] (private val self :Builder[E, C]) extends AnyVal {
		/** Similarly to [[scala.collection.mutable.Builder.mapResult mapResult]], this method creates
		  * a new builder which maps all given elements of new type `A` to the element type
		  * of this builder, before delegating to it. The built collection type is unchanged.
		  */
		def mapInput[A](f :A => E) :Builder[A, C] = self match {
			case composed :ComposedBuilder[E, _, _, C] => composed.mapInput(f)
			case _ => new ComposedBuilder(self, f, identity[C])
		}

		/** Same as `this.sizeHint(hint); this`, for convenience. */
		@inline def hinted(size :Int) :Builder[E, C] = { self sizeHint size; self }

		/** Same as `this.sizeHint(coll, delta); this`, for convenience. */
		@inline def hinted(coll :IterableOnce[_], delta :Int = 0) :Builder[E, C] = { self.sizeHint(coll, delta); self }
	}


	/** Adds Scala [[scala.collection.mutable.Growable Growable]] and [[scala.collection.mutable.Builder Builder]]
	  * methods as inlined delegates to the Java [[java.lang.StringBuilder StringBuilder]].
	  * While it essentially duplicates the functionality of standard Scala
	  * [[scala.collection.mutable.StringBuilder StringBuilder]], the wrapper is not intended to be as an object
	  * or referred to directly by the application, but rather a provider of extension methods, and returns always
	  * the original `StringBuilder`. As most methods are inlined, this incurs neither the penalty of creating
	  * an additional object, nor of delegating individual calls, at least with default compiler optimisations on.
	  */
	class JavaStringBuilderExtension private[extensions] (private val self :JStringBuilder)
		extends AnyVal with IterableOnce[Char]
	{
		@inline def apply(index :Int) :Char = self.charAt(index)
		@inline def last :Char = self.charAt(self.length - 1)

		@inline def length_=(newLength :Int) :Unit = self.setLength(newLength)
		@inline def clear() :JStringBuilder = { self.setLength(0); self }

		@inline def +=(char :Char) :JStringBuilder = self append char
		@inline def addOne(char :Char) :JStringBuilder = self append char

		@inline def ++=(chars :String)             :JStringBuilder = self append chars
		@inline def ++=(chars :Array[Char])        :JStringBuilder = self append chars
		@inline def ++=(chars :CharSequence)       :JStringBuilder = self append chars
		@inline def ++=(chars :IterableOnce[Char]) :JStringBuilder = addAll(chars)

		@inline def addAll(chars :String)             :JStringBuilder = self append chars
		@inline def addAll(chars :Array[Char])        :JStringBuilder = self append chars
		@inline def addAll(chars :CharSequence)       :JStringBuilder = self append chars
		@inline def addAll(chars :IterableOnce[Char]) :JStringBuilder = chars match {
			case it :Iterable[_] if it.isEmpty => self
			case it :Iterable[Char]            => it foreach self.append; self
			case _                             =>
				val i = chars.iterator
				while (i.hasNext)
					self append i.next()
				self
		}

		override def iterator :Iterator[Char] = new AbstractIndexedIterator[Char] {
			override def underlyingSize = self.length

			override var index :Int = 0
			override var limit :Int = self.length

			override def head = self.charAt(index)

			override def next() = { val res = self.charAt(index); index += 1; res }
		}

		@inline def result() :String = self.toString
		@inline override def toString :String = self.toString
	}


	/** Extension methods for [[scala.collection.Factory Factory]] which attempt to retrieve the `IterableFactory`
	  * (or other collection companion object).
	  */
	class FactoryExtension[E, C] private[extensions] (private val self :Factory[E, C]) extends AnyVal {
		/** The provider of builders used by this `Factory`. This is typically the companion object
		  * of the built collection type, but custom, specific implementations may return themselves.
		  * If `this` is a [[net.noresttherein.sugar.collections.ComparableFactory ComparableFactory]],
		  * it's [[net.noresttherein.sugar.collections.ComparableFactory.factory factory]] property is returned.
		  *
		  * This method is particularly useful when overrding [[Iterable]].[[scala.collection.IterableOnceOps.to to]],
		  * as it allows optimizations of returning `this` if `iterableFactory == factory.source`,
		  * or to use a dedicated implementation of the built collection type.
		  */
		def source :Any = sourceCollectionFactory(self) getOrElse self

		/** If this `Factory` was created by `IterableFactory.`[[scala.collection.IterableFactory.toFactory toFactory]],
		  * return the `IterableFactory` which created it.
		  */
		def iterableFactory[I[X]](implicit compat :C =:= I[E]) :Opt[IterableFactory[I]] =
			sourceIterableFactory(compat.substituteCo(self))

		/** If this `Factory` was created by
		  * `EvidenceIterableFactory.`[[scala.collection.EvidenceIterableFactory.toFactory toFactory]],
		  * return the `EvidenceIterableFactory` which created it.
		  */
		def evidenceIterableFactory[I[X]](implicit compat :C =:= I[E])
				:Opt[EvidenceIterableFactory[I, E] forSome { type E[v] }] =
			sourceEvidenceIterableFactory(compat.substituteCo(self))

		/** If this `Factory` was created by `MapFactory.`[[scala.collection.MapFactory.toFactory toFactory]],
		  * return the `MapFactory` which created it.
		  */
		def mapFactory[K, V, M[A, B] <: Map[A, B]](implicit elemType :E =:= (K, V), compat :C =:= M[K, V]) :Opt[MapFactory[M]] =
			sourceMapFactory(compat.substituteCo(elemType.substituteCo[({ type F[X] = Factory[X, C] })#F](self)))

		/** If this `Factory` was created by `SortedMapFactory.`[[scala.collection.SortedMapFactory.toFactory toFactory]],
		  * return the `IterableFactory` which created it.
		  */
		def sortedMapFactory[K, V, M[A, B] <: Map[A, B]](implicit elemType :E =:= (K, V), compat :C =:= M[K, V])
				:Opt[SortedMapFactory[M]] =
			sourceSortedMapFactory(compat.substituteCo(elemType.substituteCo[({ type F[X] = Factory[X, C] })#F](self)))
	}


	/** Adds a `++` extension method to any Java `Iterator[_]`. */
	class JavaIteratorExtension[I <: JIterator[_]] private[extensions] (private val self :I) extends AnyVal {
		/** Combines this iterator with the argument, returning an iterator of the same specialization as both.
		  * Aside from the basic `Iterator[_]`, this will work only for the standard three Java `PrimitiveIterator`s.
		  */
		def ++[A](next :I)(implicit shape :JavaIteratorShape[A, I]) :I =
			JavaConcatIterator(self, next)
	}




//	/** Extension methods for [[scala.collection.IterableFactory$ IterableFactory]] object itself. */
//	sealed trait IterableFactoryObjectExtension extends Any {
//		/** If `factory` was created by (implicit)
//		  * [[scala.collection.IterableFactory$ IterableFactory]]`.`[[scala.collection.IterableFactory.toFactory toFactory]],
//		  * returns the `IterableFactory` which created it.
//		  */
//		def unapply[X, C[A]](factory :Factory[X, C[X]]) :Opt[IterableFactory[C]] = sourceIterableFactory(factory)
//	}
//
	/** Extension methods for [[scala.collection.IterableFactory IterableFactory]], the most common type of
	  * companion objects for collection types conforming to the Scala collection framework.
	  * Provides additional generator methods which construct the collection of the proper type.
	  */
	class IterableFactoryExtension[C[_]] private[extensions] (private val companion :IterableFactory[C]) extends AnyVal {

		/** A complement of `C.iterate` and `C.unfold` provided by collection companion objects, which creates
		  * a collection `C` by recursively applying a partial function while defined to its own results and collecting
		  * all returned values. It is very similar to the standard [[scala.collection.IterableFactory.iterate iterate]],
		  * but instead of a fixed number of iterations, the generator function `next` is called for its return values
		  * until it is no longer applicable, which marks the end of the collection.
		  * @param start first element added to the collection.
		  * @param next generator function returning subsequent elements for the collection based on the previous one,
		  *             serving as the termination condition by indicating that it can no longer be applied
		  *             to the given argument.
		  * @tparam X element type of the generated collection.
		  * @return a collection containing the sequence starting with `start` and resulting from recursively applying
		  *         `next` to itself.
		  */
		@inline final def generate[X](start :X)(next :PartialFunction[X, X]) :C[X] =
			expand(start)(next.lift)

		/** Builds a collection `C[X]` by recursively reapplying the given partial function to the initial element.
		  * Instead of listing a fixed number of elements, this method uses the generator function `next`
		  * as the termination condition and ends the recursion once it returns `None`. It is the opposite
		  * of [[scala.collection.IterableOnceOps.reduce reduce]] in the same way as
		  * [[scala.collection.IterableFactory.unfold unfold]] is the opposite
		  * of [[scala.collection.IterableOnceOps.fold fold]].
		  * @param start The first element added to the collection.
		  * @param next  A generator function returning subsequent elements for the collection based on the previous one,
		  *              or `None` to indicate the end of recursion.
		  * @tparam X the element type of the generated collection.
		  * @return a collection containing the sequence starting with `start` and resulting from recursively applying `next`
		  *         to itself.
		  */
		@nowarn("cat=deprecation")
		final def expand[X](start :X)(next :X => Option[X]) :C[X] =
			companion match {
				case Stream =>
					(start #:: (next(start).map(Stream.expand(_)(next)) getOrElse Stream.empty)).asInstanceOf[C[X]]
				case LazyList =>
					(start #:: (next(start).map(LazyList.expand(_)(next)) getOrElse LazyList.empty)).asInstanceOf[C[X]]
				case _ =>
					val builder = companion.newBuilder[X]
					builder += start
					@tailrec def rec(x :X = start) :C[X] = next(x) match {
						case Some(y) => builder += y; rec(y)
						case None => builder.result()
					}
					rec()
			}

		/** Similar to [[scala.collection.IterableFactory IterableFactory]]`.`[[scala.collection.IterableFactory.iterate iterate]],
		  * but the iterating function accepts the positional index of the next element as an additional argument.
		  * @param start The first element of the created collection.
		  * @param len   The size of the created collection.
		  * @param f     A function generating subsequent elements following start.
		  *              The second element of the collection will be `f(start, 1)`, the third `f(f(start, 1), 2)`, and so on.
		  */
		final def iterateWithIndex[X](start :X, len :Int)(f :(X, Int) => X) :C[X] =
			companion.from(View.iterate((start, 0), len) { xi => (f(xi._1, xi._2 + 1), xi._2 + 1) }.map(_._1))
//
//		/** A singleton of this collection type, created through `this.empty[E] + elem`, rather than `this(elem)`,
//		  * which makes it faster for most collection implementations, as it bypasses `from` and creation
//		  * of a singleton `Seq` to contain variable arguments to `apply`.
//		  */
//		def single[E](elem :E) :C[E] = companion.empty[E] //+ elem
	}
//
//
//	/** Extension methods for [[scala.collection.EvidenceIterableFactory$ EvidenceIterableFactory]] object itself. */
//	sealed trait EvidenceIterableFactoryObjectExtension extends Any {
//		/** If `factory` was created by (implicit)
//		  * [[scala.collection.EvidenceIterableFactory$ EvidenceIterableFactory]]`.`[[scala.collection.EvidenceIterableFactory.toFactory toFactory]],
//		  * returns the `EvidenceIterableFactory` which created it.
//		  */
//		def unapply[X, C[A]](factory :Factory[X, C[X]]) :Opt[EvidenceIterableFactory[C, E] forSome { type E[v] }] =
//			sourceEvidenceIterableFactory(factory)
//	}


	/** Extension factory methods for single element immutable [[collection.immutable.Set Set]] companions. */
	class SetFactoryExtension[C[X] <: SetOps[X, C, C[X]]] private[collections]
	                         (private val self :IterableFactory[C])
		extends AnyVal
	{
		@inline def single[E](value :E) :C[E] = self.empty[E] + value
		@inline def one[E](value :E)    :C[E] = self.empty[E] + value
//		@inline def two[E](value1 :E, value2 :E) :C[E] = self.empty[E] + value1 + value2
	}

	/** Extension factory methods for single element [[collection.immutable.Seq Seq]] companions. */
	class SeqFactoryExtension[C[X] <: SeqOps[X, C, C[X]]] private[collections]
	                         (private val self :IterableFactory[C])
		extends AnyVal
	{
		@inline def single[E](value :E) :C[E] = one(value)

		@inline def one[E](value :E)    :C[E] =
			if (self eq IndexedSeq) ConstSeq(value, 1).castFrom[Seq[E], C[E]]
			else self.empty[E] :+ value
//		@inline def two[E](value1 :E, value2 :E) :C[E] = self.empty[E] :+ value1 :+ value2
	}

	sealed trait IndexedSeqObjectExtension extends Any {
		@inline def single[E](value :E) :IndexedSeq[E] = ConstSeq(value, 1)
		@inline def one[E](value :E) :IndexedSeq[E] = ConstSeq(value, 1)
	}
//
//	/** Extension methods for [[scala.collection.MapFactory$ MapFactory]] object itself. */
//	sealed trait MapFactoryObjectExtension extends Any {
//		/** If `factory` was created by (implicit)
//		  * [[scala.collection.MapFactory$ MapFactory]]`.`[[scala.collection.MapFactory.toFactory toFactory]],
//		  * returns the `MapFactory` which created it.
//		  */
//		def unapply[K, V, M[A, B] <: Map[A, B]](factory :Factory[(K, V), M[K, V]]) :Opt[MapFactory[M]] =
//			sourceMapFactory(factory)
//	}

	/** Extension factory methods for single and two element [[Map Map]]s. */
	sealed trait MapObjectExtension extends Any {
		@inline final def single[K, V](key :K, value :V) :Map[K, V] = new Map.Map1(key, value)
		@inline final def single[K, V](entry :(K, V)) :Map[K, V] = new Map.Map1(entry._1, entry._2)
		@inline final def one[K, V](key :K, value :V) :Map[K, V] = new Map.Map1(key, value)
		@inline final def one[K, V](entry :(K, V)) :Map[K, V] = new Map.Map1(entry._1, entry._2)
		@inline final def two[K, V](key1 :K, value1 :V, key2 :K, value2 :V) :Map[K, V] =
			new Map.Map2(key1, value1, key2, value2)
		@inline final def two[K, V](entry1 :(K, V), entry2 :(K, V)) :Map[K, V] =
			new Map.Map2(entry1._1, entry1._2, entry2._1, entry2._2)
	}
//
//	/** Extension methods for [[scala.collection.SortedMapFactory$ SortedMapFactory]] object itself. */
//	sealed trait SortedMapFactoryObjectExtension extends Any {
//		/** If `factory` was created by (implicit)
//		  * [[scala.collection.SortedMapFactory$ SortedMapFactory]]`.`[[scala.collection.SortedMapFactory.toFactory toFactory]],
//		  * returns the `SortedMapFactory` which created it.
//		  */
//		def unapply[K, V, M[A, B] <: Map[A, B]](factory :Factory[(K, V), M[K, V]]) :Opt[SortedMapFactory[M]] =
//			sourceSortedMapFactory(factory)
//	}


	/** Extensions methods for object [[Array]] (the array factory). Adds the same methods as
	  * [[net.noresttherein.sugar.collections.extensions.IterableFactoryExtension IterableFactoryExtension]]
	  * as well as missing methods from [[scala.collection.SeqFactory SeqFactory]]
	  * and some adapters of methods in [[java.util.Arrays]].
	  */
	sealed trait ArrayObjectExtension extends Any {
		/** An uninitialized array with the specified element type and length. */
		final def of[E](elemType :Class[E], capacity :Int) :Array[E] =
			java.lang.reflect.Array.newInstance(elemType, capacity).asInstanceOf[Array[E]]

		/** Clones the given array. */
		final def copyOf[E](elems :Array[E]) :Array[E] = {
			val res = java.lang.reflect.Array.newInstance(elems.getClass.getComponentType, elems.length)
			System.arraycopy(elems, 0, res, 0, elems.length)
			res.asInstanceOf[Array[E]]
		}

		/** A single element `Array[E]`. */
		final def one[E :ClassTag](elem :E) :Array[E] = {
			val res = new Array[E](1)
			res(0) = elem
			res
		}
		/** A single element `Array[E]`. */
		@inline final def single[E :ClassTag](elem :E) :Array[E] = one(elem)

		/** A single element `Array[Byte]`. */
		final def one(elem :Byte) :Array[Byte] = {
			val res = new Array[Byte](1)
			res(0) = elem
			res
		}
		/** A single element `Array[Byte]`. */
		@inline final def single(elem :Byte) :Array[Byte] = one(elem)

		/** A single element `Array[Short]`. */
		final def one(elem :Short) :Array[Short] = {
			val res = new Array[Short](1)
			res(0) = elem
			res
		}
		/** A single element `Array[Short]`. */
		@inline final def single(elem :Short) :Array[Short] = one(elem)

		/** A single element `Array[Char]`. */
		final def one(elem :Char) :Array[Char] = {
			val res = new Array[Char](1)
			res(0) = elem
			res
		}
		/** A single element `Array[Char]`. */
		@inline final def single(elem :Char) :Array[Char] = one(elem)

		/** A single element `Array[Int]`. */
		final def one(elem :Int) :Array[Int] = {
			val res = new Array[Int](1)
			res(0) = elem
			res
		}
		/** A single element `Array[Int]`. */
		@inline final def single(elem :Int) :Array[Int] = one(elem)

		/** A single element `Array[Long]`. */
		final def one(elem :Long) :Array[Long] = {
			val res = new Array[Long](1)
			res(0) = elem
			res
		}
		/** A single element `Array[Long]`. */
		@inline final def single(elem :Long) :Array[Long] = one(elem)

		/** A single element `Array[Float]`. */
		final def one(elem :Float) :Array[Float] = {
			val res = new Array[Float](1)
			res(0) = elem
			res
		}
		/** A single element `Array[Float]`. */
		@inline final def single(elem :Float) :Array[Float] = one(elem)

		/** A single element `Array[Double]`. */
		final def one(elem :Double) :Array[Double] = {
			val res = new Array[Double](1)
			res(0) = elem
			res
		}
		/** A single element `Array[Double]`. */
		@inline final def single(elem :Double) :Array[Double] = one(elem)

		/** A single element `Array[Boolean]`. */
		@inline final def single(elem :Boolean) :Array[Boolean] = one(elem)
		
		/** A single element `Array[Boolean]`. */
		final def one(elem :Boolean) :Array[Boolean] = {
			val res = new Array[Boolean](1)
			res(0) = elem
			res
		}

		/** A two element array of element type defined by the implicit `ClassTag`. */
		final def two[E :ClassTag](first :E, second :E) :Array[E] = {
			val res = new Array[E](2)
			res(0) = first
			res(1) = second
			res
		}

		/** An `Array[Byte]` of two elements. */
		final def two(first :Byte, second :Byte) :Array[Byte] = {
			val res = new Array[Byte](2)
			res(0) = first
			res(1) = second
			res
		}

		/** An `Array[Short]` of two elements. */
		final def two(first :Short, second :Short) :Array[Short] = {
			val res = new Array[Short](2)
			res(0) = first
			res(1) = second
			res
		}

		/** An `Array[Char]` of two elements. */
		final def two(first :Char, second :Char) :Array[Char] = {
			val res = new Array[Char](2)
			res(0) = first
			res(1) = second
			res
		}

		/** An `Array[Int]` of two elements. */
		final def two(first :Int, second :Int) :Array[Int] = {
			val res = new Array[Int](2)
			res(0) = first
			res(1) = second
			res
		}

		/** An `Array[Long]` of two elements. */
		final def two(first :Long, second :Long) :Array[Long] = {
			val res = new Array[Long](2)
			res(0) = first
			res(1) = second
			res
		}

		/** An `Array[Float]` of two elements. */
		final def two(first :Float, second :Float) :Array[Float] = {
			val res = new Array[Float](2)
			res(0) = first
			res(1) = second
			res
		}

		/** An `Array[Double]` of two elements. */
		final def two(first :Double, second :Double) :Array[Double] = {
			val res = new Array[Double](2)
			res(0) = first
			res(1) = second
			res
		}

		/** An `Array[Boolean]` of two elements. */
		final def two(first :Boolean, second :Boolean) :Array[Boolean] = {
			val res = new Array[Boolean](2)
			res(0) = first
			res(1) = second
			res
		}

		/** A complement of `Array.iterate` and `Array.unfold` provided by `Array` object, which creates
		  * an `Array[X]` by recursively applying a partial function while defined to its own results and collecting
		  * all returned values. It is very similar to the standard [[Array.iterate iterate]],
		  * but instead of a fixed number of iterations, the generator function `next` is called for its return values
		  * until it is no longer applicable, which marks the end of the collection.
		  * @param start first element added to the array.
		  * @param next  generator function returning subsequent elements for the array based on the previous one,
		  *              serving as the termination condition by indicating that it can no longer be applied
		  *              to the given argument.
		  * @tparam X element type of the generated array.
		  * @return an array containing the sequence starting with `start` and resulting from recursively applying
		  *         `next` to itself.
		  */
		@inline final def generate[X](start :X)(next :PartialFunction[X, X])(implicit elements :ClassTag[X]) :Array[X] =
			expand(start)(next.lift)

		/** Builds an `Array[X]` by recursively reapplying the given partial function to the initial element.
		  * Instead of listing a fixed number of elements, this method uses the generator function `next`
		  * as the termination condition and ends the recursion once it returns `None`. It is the opposite
		  * of [[scala.collection.IterableOnceOps.reduce reduce]] in the same way as
		  * [[scala.collection.IterableFactory.unfold unfold]] is the opposite
		  * of [[scala.collection.IterableOnceOps.fold fold]].
		  * @param start The first element added to the array.
		  * @param next  A generator function returning subsequent elements for the array based on the previous one,
		  *              or `None` to indicate the end of recursion.
		  * @tparam X the element type of the generated array.
		  * @return an array containing the sequence starting with `start`
		  *         and resulting from recursively applying `next` to itself.
		  */
		final def expand[X](start :X)(next :X => Option[X])(implicit elements :ClassTag[X]) :Array[X] = {
			val builder = Array.newBuilder[X]
			builder += start
			@tailrec def rec(x :X = start) :Array[X] = next(x) match {
				case Some(y) => builder += y; rec(y)
				case None => builder.result()
			}
			rec()
		}

		/** Similar to [[Array.iterate Array.iterate]],
		  * but the iterating function accepts the positional index of the next element as an additional argument.
		  * @param start The first element of the created collection.
		  * @param len   The size of the created collection.
		  * @param f     A function generating subsequent elements following start.
		  *              The second element of the collection will be `f(start, 1)`, the third `f(f(start, 1), 2)`, and so on.
		  */
		final def iterateWithIndex[X](start :X, len :Int)(f :(X, Int) => X)(implicit elements :ClassTag[X]) :Array[X] =
			Array.from(View.iterate((start, 0), len) { xi => (f(xi._1, xi._2 + 1), xi._2 + 1) }.map(_._1))

		/** Produces an array that uses a function `f` to produce elements of type `A`
		  * and update an internal state of type `S`.
		  * @param init State initial value
		  * @param f    Computes the next element (or returns `None` to signal
		  *             the end of the collection)
		  * @tparam A Type of the elements
		  * @tparam S Type of the internal state
		  * @return an array that produces elements using `f` until `f` returns `None`
		  */
		final def unfold[A, S](init :S)(f :S => Option[(A, S)])(implicit elements :ClassTag[A]) :Array[A] =
			Array.from(new View.Unfold(init)(f))

		/** A stepper iterating over the range of an array. Indices out of range are skipped silently. */
		final def stepper[A, S <: Stepper[_]]
		                 (array :Array[A], from :Int = 0, until :Int = Int.MaxValue)
		                 (implicit shape :StepperShape[A, S]) :S with EfficientSplit =
			ArrayStepper(array, from, until)
	}


	/** An `EvidenceIterableFactory[Array]` to which the `Array` object is implicitly converted. */
	object ArrayIterableFactory extends EvidenceIterableFactory[Array, ClassTag] {
		@inline override def from[E :ClassTag](it :IterableOnce[E]) :Array[E] = Array.from(it)
		@inline override def empty[A :ClassTag] :Array[A] = Array.empty
		@inline override def newBuilder[A :ClassTag] :Builder[A, Array[A]] = Array.newBuilder
	}


	/** Adds factory methods for array iterators
	  * and a [[net.noresttherein.sugar.collections.extensions.IteratorObjectExtension.double double]] factory method
	  * for two element iterators to object `Iterator` to complement [[scala.collection.Iterator.single single]].
	  */
	sealed trait IteratorObjectExtension extends Any {
		final def double[A](first :A, second :A) :BufferedIterator[A] = new BufferedIterator[A] {
			private[this] var idx = 0
			override def hasNext = idx < 2
			override def head = idx match {
				case 0 => first
				case 1 => second
				case _ => throw new NoSuchElementException("Iterator.empty")
			}
			override def next() = idx match {
				case 0 => idx = 1; first
				case 1 => idx = 2; second
				case _ => throw new NoSuchElementException("Iterator.empty")
			}
		}

		/** Same as `Iterator.`[[scala.collection.Iterator.fill fill]](len)(value), but returns a constant value. */
		final def const[A](len :Int, value :A) :Iterator[A] = new Iterator[A] {
			private[this] var count = len
			override def hasNext = count > 0
			override def next() = { count -= 1; value }
		}
		/** Same as `Iterator.`[[scala.collection.Iterator.continually continually]](value), but returns a constant value. */
		final def const[A](value :A) :Iterator[A] = new Iterator[A] {
			override def hasNext = true
			override def next() = value
		}

		/** An iterator over the entirety of the specified array. */
		final def over[X](array :Array[X]) :Iterator[X] = ArrayIterator(array)

		/** An iterator over a slice of an array. */
		final def slice[X](array :Array[X], from :Int, until :Int) :Iterator[X] = ArrayIterator(array, from, until)

		/** An iterator going over the elements of an array in reverse. */
		final def reverse[X](array :Array[X]) :Iterator[X] = ReverseArrayIterator(array)

		/** An iterator over a slice of an array going in reverse. The first item returned will be at `array(until-1)`,
		  * and the last `array(from)`.
		  */
		final def reverse[X](array :Array[X], from :Int, until :Int) :Iterator[X] =
			ReverseArrayIterator(array, from, until)

	}


	/** Adds factory methods to the [[scala.collection.Stepper$ Stepper]] singleton object, creating steppers
	  * of zero, one or two elements.
	  */
	sealed trait StepperObjectExtension extends Any {
		/** Creates an empty `Stepper`. This method variant requires either explicit specification of type parameters,
		  * or for the element type to be abstract, with a single implicit `StepperShape[T, S]` in scope.
		  * @see [[net.noresttherein.sugar.collections.extensions.StepperObjectExtension.apply[T] apply]]`()`.
		  */
		@inline final def empty[T, S <: Stepper[_]](implicit shape :StepperShape[T, S]) :S with EfficientSplit =
			Stepper0()

		/** Creates an empty stepper with the specified element type.
		  * The returned object has an `apply()` method accepting an implicit
		  * [[scala.collection.StepperShape StepperShape]]`[T, S]`; this allows to split type parameter groups
		  * and provide here only the element type: `Stepper[T]()`.
		  * @see [[net.noresttherein.sugar.collections.extensions.StepperObjectExtension.empty empty]]
		  */
		@inline final def apply[T] = new EmptyStepperFactory[T] {}

		/** Creates a stepper of a single element, with a shape proper for that element.
		  * While the created `Stepper` will not box value types, this method itself is generic and hence boxes them.
		  * You can invoke manually specialized methods instead - `ofInt`, `ofLong`, `ofDouble`, `ofAny` -
		  * to avoid boxing.
		  */
		@inline final def apply[T, S <: Stepper[_]](elem :T)(implicit shape :StepperShape[T, S]) :S with EfficientSplit =
			Stepper1(elem)

		/** Creates a stepper of a two elements, with a shape proper for that elements.
		  * While the created `Stepper` will not box value types, this method itself is generic and hence boxes them.
		  * You can invoke manually specialized methods instead - `ofInt`, `ofLong`, `ofDouble`, `ofAny` -
		  * to avoid boxing.
		  */
		@inline final def apply[T, S <: Stepper[_]](first :T, second :T)(implicit shape :StepperShape[T, S])
				:S with EfficientSplit =
			Stepper2(first, second)

		/** Creates a stepper iterating over the specified range of the given array. If `from < 0` or
		  * `until > array.length`, they are adjusted `0/array.length` and no exception is thrown due to indexing.
		  */
		@inline final def apply[T, S <: Stepper[_]](array :Array[T], from :Int = 0, until :Int = Int.MaxValue)
		                                           (implicit shape :StepperShape[T, S]) :S =
			ArrayStepper(array, from, until)

		/** Creates an empty stepper for reference types (`AnyStepper`). */
		@inline def ofAny[T] :AnyStepper[T] with EfficientSplit = Stepper0.ofAny

		/** Creates a single element stepper specific to reference types.
		  * When used for a value type, the value will be boxed by this call.
		  */
		@inline def ofAny[T](elem :T) :AnyStepper[T] with EfficientSplit = Stepper1.ofAny(elem)

		/** Creates a stepper of two elements specific to reference types.
		  * When used for a value type, the values will be boxed by this call.
		  */
		@inline def ofAny[T](first :T, second :T) :AnyStepper[T] with EfficientSplit = Stepper2.ofAny(first, second)


		/** Creates an empty stepper for `Int`. */
		@inline def ofInt :IntStepper with EfficientSplit = Stepper0.ofInt

		/** Creates a stepper for a single `Int`.
		  * This method can be also used for `Byte`, `Short` and `Char`, as they all use `IntStepper`.
		  */
		@inline def ofInt(elem :Int) :IntStepper with EfficientSplit = Stepper1.ofInt(elem)

		/** Creates a stepper for two `Int`s.
		  * This method can be also used for `Byte`, `Short` and `Char`, as they all use `IntStepper`.
		  */
		@inline def ofInt(first :Int, second :Int) :IntStepper with EfficientSplit = Stepper2.ofInt(first, second)

		/** Iterates over the whole `String`. */
		@inline def ofInt(string :String) :IntStepper with EfficientSplit = ofInt(string, 0, string.length)

		/** Iterates over the specified range of characters in the `string` argument. */
		@inline def ofInt(string :String, from :Int, until :Int) :IntStepper with EfficientSplit =
			new StringStepper(string, from, until)

		/** Creates an empty stepper for `Long`. */
		@inline def ofLong :LongStepper with EfficientSplit = Stepper0.ofLong

		/** Creates a stepper for a single `Long`. */
		@inline def ofLong(elem :Long) :LongStepper with EfficientSplit = Stepper1.ofLong(elem)

		/** Creates a stepper for two `Long`s. */
		@inline def ofLong(first :Long, second :Long) :LongStepper with EfficientSplit = Stepper2.ofLong(first, second)

		/** Creates an empty stepper for `Double`.
		  * This method can be also used for `Float`, as it too uses `DoubleStepper`.
		  */
		@inline def ofDouble :DoubleStepper with EfficientSplit = Stepper0.ofDouble

		/** Creates a stepper for a single `Double`.
		  * This method can be also used for `Float`, as it too uses `DoubleStepper`.
		  */
		@inline def ofDouble(elem :Double) :DoubleStepper with EfficientSplit = Stepper1.ofDouble(elem)

		/** Creates a stepper for two `Double`s.
		  * This method can be also used for `Float`, as it too uses `DoubleStepper`.
		  */
		@inline def ofDouble(first :Double, second :Double) :DoubleStepper with EfficientSplit =
			Stepper2.ofDouble(first, second)
	}

	/** An `apply()` method accepting an implicit `StepperShape[T, S]`, inferring the stepper type. It is a continuation
	  *  of [[net.noresttherein.sugar.collections.extensions.StepperObjectExtension.apply Stepper]]`[T]` call.
	  */
	sealed trait EmptyStepperFactory[T] extends Any {
		/** Creates an empty `Stepper` of shape defined by an implicit `StepperShape` for element type `T`. */
		@inline final def apply[S <: Stepper[_]]()(implicit shape :StepperShape[T, S]) :S with EfficientSplit =
			Stepper0()
	}

}






private class ComposedBuilder[-X, Y, C, +R](underlying :Builder[Y, C], mapArgs :X => Y, mapRes :C => R)
	extends Builder[X, R]
{
	override def knownSize :Int = underlying.knownSize
	override def sizeHint(size :Int) :Unit = underlying.sizeHint(size)

	override def addOne(elem :X) :this.type = { underlying addOne mapArgs(elem); this }
	override def addAll(xs :IterableOnce[X]) :this.type = {
		val size_? = knownSize
		if (size_? >= 0)
			sizeHint(xs, size_?)
		super.addAll(xs)
	}

	override def mapResult[NewTo](f :R => NewTo) :Builder[X, NewTo] =
		new ComposedBuilder(underlying, mapArgs, mapRes andThen f)

	def mapInput[Arg](f :Arg => X) :Builder[Arg, R] =
		new ComposedBuilder(underlying, f andThen mapArgs, mapRes)

	override def result() :R = mapRes(underlying.result())
	override def clear() :Unit = underlying.clear()
}
