package net.noresttherein.sugar.collections

import java.util.{Arrays, PrimitiveIterator, Spliterator}
import java.lang.{Math => math}
import java.lang.System.arraycopy

import scala.annotation.{implicitNotFound, nowarn, tailrec}
import scala.collection.{AbstractIterator, AnyStepper, ArrayOps, BufferedIterator, ClassTagIterableFactory, DoubleStepper, EvidenceIterableFactory, Factory, IntStepper, IterableFactory, IterableOnce, IterableOnceOps, IterableOps, LongStepper, MapFactory, SortedMapFactory, Stepper, StepperShape, View, mutable}
import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.{IsIterableOnce, IsSeq}
import scala.collection.immutable.{ArraySeq, LinearSeq, MapOps, SetOps}
import scala.collection.mutable.{ArrayBuffer, Buffer, Builder, IndexedBuffer, ListBuffer, ReusableBuilder}
import scala.reflect.{ClassTag, classTag}
import scala.runtime.BoxedUnit
import scala.util.{Random, Sorting}

import net.noresttherein.sugar.{outOfBounds_!, unsupported_!}
import net.noresttherein.sugar.JavaTypes.{JIterator, JStringBuilder}
import net.noresttherein.sugar.arrays.{ArrayIterator, ArrayLike, CyclicArrayIterator, ErasedArray, IArray, IRefArray, RefArray, ReverseArrayIterator}
import net.noresttherein.sugar.arrays.extensions.{ArrayCompanionExtension, ArrayExtension, ArrayLikeExtension}
import net.noresttherein.sugar.collections.Constants.ReasonableArraySize
import net.noresttherein.sugar.collections.ElementIndex.{Absent, Present, indexOfErrorMessage, indexOfNotFound, indexOfSliceErrorMessage, indexOfSliceNotFound, indexWhereErrorMessage, indexWhereNotFound, lastIndexOfErrorMessage, lastIndexOfNotFound, lastIndexOfSliceErrorMessage, lastIndexOfSliceNotFound, lastIndexWhereErrorMessage, lastIndexWhereNotFound}
import net.noresttherein.sugar.collections.HasFastSlice.preferDropOverIterator
import net.noresttherein.sugar.collections.IndexedIterable.{ApplyPreferred, applyPreferred, updatePreferred}
import net.noresttherein.sugar.collections.extensions.{ArrayBufferCompanionExtension, BooleanJteratorExtension, BufferExtension, BuilderExtension, ByteJteratorExtension, CharJteratorExtension, DoubleJteratorExtension, FactoryExtension, FloatJteratorExtension, IndexedSeqExtension, IntJteratorExtension, IterableExtension, IterableFactoryExtension, IterableOnceExtension, IteratorCompanionExtension, IteratorExtension, JavaDoubleIteratorExtension, JavaIntIteratorExtension, JavaIteratorExtension, JavaLongIteratorExtension, JavaStringBuilderExtension, JteratorExtension, LongJteratorExtension, RefJteratorExtension, SeqExtension, SeqFactoryExtension, ShortJteratorExtension, StepType, StepperCompanionExtension, StepperExtension, StepperShapeCompanionExtension, StringBuilderExtension, StringExtension, StringExtensionConversion, immutableIndexedSeqCompanionExtension, immutableMapCompanionExtension, immutableMapExtension, immutableSetFactoryExtension, mutableIndexedSeqExtension}
import net.noresttherein.sugar.collections.util.{errorString, knownEmpty}
import net.noresttherein.sugar.exceptions.raise
import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.funny.extensions.PartialFunctionExtension
import net.noresttherein.sugar.numeric.BitLogic
import net.noresttherein.sugar.reflect.ArrayClass
import net.noresttherein.sugar.reflect.extensions.ClassExtension
import net.noresttherein.sugar.reflect.prettyprint.fullNameOf
import net.noresttherein.sugar.reflect.prettyprint.extensions.classNameMethods
import net.noresttherein.sugar.repeat.extensions.timesMethods
import net.noresttherein.sugar.text.EOL
import net.noresttherein.sugar.typist.{PriorityConversion, Unknown}
import net.noresttherein.sugar.typist.casting.extensions.{castTypeConstructorMethods, castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.vars.IntOpt.{AnInt, NoInt}
import net.noresttherein.sugar.vars.{IntOpt, Opt}
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.witness.Ignored




//I don't think this introduces priority as they need to be imported by name
private[collections] sealed trait extensionsLowPriority extends Any {
	/** Adds a `foldWhile` method to any `Iterable` which implement a variant of `fold` operation with a break condition. */
	@inline implicit final def IsIterableOnceExtension[C](self :C)(implicit iterable :IsIterableOnce[C])
			:IterableOnceExtension[iterable.A] =
		new IterableOnceExtension[iterable.A](iterable(self))

	@inline implicit final def BufferExtension[E](self :Buffer[E]) :BufferExtension[E, self.type] =
		new BufferExtension[E, self.type](self)
}


//todo: global method for exception messages, which allow to switch if one wants to include collections/elems in the message
/** Extension methods for various collection types as well as collection companion objects. */
trait extensions
	extends Any with extensionsLowPriority
{
	/** Adds various additional folding methods with a break condition to any `Iterable`. */
	@inline implicit final def IterableOnceExtension[E](self :IterableOnce[E]) :IterableOnceExtension[E] =
		new IterableOnceExtension[E](self)

	/** Adds various additional folding methods with a break condition to any `Array`. */
	@inline implicit final def ArrayAsIterableOnceExtension[E](self :Array[E]) :IterableOnceExtension[E] =
		new IterableOnceExtension[E](self.toOps)

	/** Adds various additional folding methods with a break condition to any `String`. */
	@inline implicit final def StringAsIterableOnceExtension(self :String) :IterableOnceExtension[Char] =
		new IterableOnceExtension[Char](self)


	/** Extension methods for `Iterator` */
	@inline implicit final def IteratorExtension[E](self :Iterator[E]) :IteratorExtension[E] =
		new IteratorExtension(self)


	/** Adds various methods for mapping/flatMapping collections to any collection of the standard library framework.
	  * These either pass along additional state, or have a break condition. Roughly equivalent to working
	  * with `toLazyList.scan`, but cleaner and more efficient.
	  */
	@inline implicit final def IterableExtension[E, CC[X], C](self :IterableOps[E, CC, C]) :IterableExtension[E, CC, C] =
		new IterableExtension[E, CC, C](self)

	/** Adds various methods for mapping/flatMapping collections to any `Array`.
	  * These either pass along additional state, or have a break condition. Roughly equivalent to working
	  * with `toLazyList.scan`, but cleaner and more efficient.
	  */
	@inline implicit final def ArrayAsIterableExtension[E](self :Array[E]) :IterableExtension[E, RefArray, Array[E]] =
		new IterableExtension[E, RefArray, Array[E]](self.toOps)

	/** Adds various methods for mapping/flatMapping collections to any `String`.
	  * These either pass along additional state, or have a break condition. Roughly equivalent to working
	  * with `toLazyList.scan`, but cleaner and more efficient.
	  */
    @inline implicit final def StringAsIterableExtension(self :String) :IterableExtension[Char, IndexedSeq, String] =
		new IterableExtension(new StringAsSeq(self))


	/** Alternative, safer implementations of [[scala.collection.SeqOps.indexOf indexOf]] for [[Seq]],
	  * which do not return a negative index when the element is not found.
	  */
	@inline implicit final def SeqExtension[E, CC[A], C]
	                                       (self :scala.collection.SeqOps[E, CC, C]) :SeqExtension[E, CC, C] =
		new SeqExtension[E, CC, C](self)

	/** Alternative, safer implementations of [[scala.collection.SeqOps.indexOf indexOf]] for [[Array]],
	  * which do not return a negative index when the element is not found.
	  */
	@inline implicit final def ArrayAsSeqExtension[E](self :Array[E]) :SeqExtension[E, RefArray, Array[E]] =
		new SeqExtension[E, RefArray, Array[E]](self.toOps)

	/** Alternative, safer implementations of [[scala.collection.SeqOps.indexOf indexOf]] for strings,
	  * which do not return a negative index when the element is not found.
	  */
	@inline implicit final def StringAsSeqExtension(self :String) :SeqExtension[Char, IndexedSeq, String] =
		new SeqExtension(new StringAsSeq(self))


	/** Operations on suffixes of a sequence and binary search methods on sorted sequences. */
	@inline implicit final def IndexedSeqExtension[E, CC[A], C](self :collection.IndexedSeqOps[E, CC, C])
			:IndexedSeqExtension[E, CC, C] =
		new IndexedSeqExtension[E, CC, C](self)

	/** Operations on suffixes of a sequence and binary search methods on sorted arrays. */
	@inline implicit final def ArrayAsIndexedSeqExtension[E](self :Array[E]) :IndexedSeqExtension[E, RefArray, Array[E]] =
		new IndexedSeqExtension[E, RefArray, Array[E]](self.toOps)

	/** Operations on suffixes of a string. */
	@inline implicit final def StringAsIndexedSeqExtension(self :String) :IndexedSeqExtension[Char, IndexedSeq, String] =
		new IndexedSeqExtension(new StringAsSeq(self))

	@inline implicit final def mutableIndexedSeqExtension[E](self :mutable.IndexedSeq[E]) :mutableIndexedSeqExtension[E] =
		new mutableIndexedSeqExtension(self)

	/** Binary search methods for sorted arrays. */
//	@inline implicit final def ArrayExtension[E](self :Array[E]) :ArrayExtension[E] =
//		new ArrayExtension(self)
//	implicit final def ArrayExtension[E] :ArrayExtensionConversion[E] =
//		ArrayExtensionConversionPrototype.asInstanceOf[ArrayExtensionConversion[E]]

//	@inline implicit final def StringExtension(self :String) :StringExtension = new StringExtension(self)

	/** Extension methods for `String`. */ //See https://github.com/scala/bug/issues/12857 for the type parameter
	implicit def StringExtension[X] :StringExtensionConversion = extensions.StringExtensionConversionPrototype

	/** An [[net.noresttherein.sugar.collections.extensions.immutableMapExtension.updatedIfAbsent updatedIfAbsent]]
	  * method for any `Map`.
	  */
	@inline implicit final def immutableMapExtension[K, V, M[A, +B] <: MapOps[A, B, M, M[A, B]]]
	                                                (self :M[K, V]) :immutableMapExtension[K, V, M] =
		new immutableMapExtension(self)

	/** Adds a `++` method to any `Stepper` type. */
	@inline implicit final def StepperExtension[E, S <: Stepper[_]](self :S)(implicit elemType :StepType[E, S])
	       :StepperExtension[E, S] =
		new StepperExtension[E, S](self)

//	@inline implicit final def efficientSplitStepperExtension[S <: Stepper[_]](self :S with EfficientSplit) =
//		new StepperExtension[S with EfficientSplit](self)

	/** Adds a `++` method to any standard `java.util.Iterator` subclass. */
	@inline implicit final def JavaIteratorExtension[I <: JavaIterator[_]](self :I) :JavaIteratorExtension[I] =
		new JavaIteratorExtension[I](self)

	@inline implicit final def JavaIntIteratorExtension(self :JavaIntIterator) :JavaIntIteratorExtension =
		new JavaIntIteratorExtension(self)

	@inline implicit final def JavaLongIteratorExtension(self :JavaLongIterator) :JavaLongIteratorExtension =
		new JavaLongIteratorExtension(self)

	@inline implicit final def JavaDoubleIteratorExtension(self :JavaDoubleIterator) :JavaDoubleIteratorExtension =
		new JavaDoubleIteratorExtension(self)

	@inline implicit final def JteratorExtension[I <: Jterator[_]](self :I) :JteratorExtension[I] =
		new JteratorExtension(self.asInstanceOf[JavaIterator[_]])

	@inline implicit final def IntJteratorExtension(self :IntJterator) :IntJteratorExtension =
		new IntJteratorExtension(self.asInstanceOf[JavaIntIterator])

	@inline implicit final def LongJteratorExtension(self :LongJterator) :LongJteratorExtension =
		new LongJteratorExtension(self.asInstanceOf[JavaLongIterator])

	@inline implicit final def DoubleJteratorExtension(self :DoubleJterator) :DoubleJteratorExtension =
		new DoubleJteratorExtension(self.asInstanceOf[JavaDoubleIterator])

	@inline implicit final def FloatJteratorExtension(self :FloatJterator) :FloatJteratorExtension =
		new FloatJteratorExtension(self.asInstanceOf[JavaDoubleIterator])

	@inline implicit final def CharJteratorExtension(self :CharJterator) :CharJteratorExtension =
		new CharJteratorExtension(self.asInstanceOf[JavaIntIterator])

	@inline implicit final def ShortJteratorExtension(self :ShortJterator) :ShortJteratorExtension =
		new ShortJteratorExtension(self.asInstanceOf[JavaIntIterator])

	@inline implicit final def ByteJteratorExtension(self :ByteJterator) :ByteJteratorExtension =
		new ByteJteratorExtension(self.asInstanceOf[JavaIntIterator])

	@inline implicit final def BooleanJteratorExtension(self :BooleanJterator) :BooleanJteratorExtension =
		new BooleanJteratorExtension(self.asInstanceOf[JavaIntIterator])

	@inline implicit final def RefJteratorExtension[E <: AnyRef](self :RefJterator[E]) :RefJteratorExtension[E] =
		new RefJteratorExtension(self.asInstanceOf[JavaIterator[E]])


	/** Extension factory methods for single element mutable and immutable [[collection.Seq Seq]] subtypes' companions. */
	@inline implicit final def SeqFactoryExtension[C[X] <: collection.SeqOps[X, C, C[X]]]
	                                              (self :IterableFactory[C]) :SeqFactoryExtension[C] =
		new SeqFactoryExtension(self)

	@inline implicit final def immutableIndexedSeqCompanionExtension(self :IndexedSeq.type)
			:immutableIndexedSeqCompanionExtension =
		new immutableIndexedSeqCompanionExtension {}

	/** Extension factory methods for [[collection.mutable.ArrayBuffer$ ArrayBuffer]]. */
	@inline implicit final def ArrayBufferCompanionExtension(self :ArrayBuffer.type) :ArrayBufferCompanionExtension =
		new ArrayBufferCompanionExtension {}

	/** Extension factory methods for single element immutable [[collection.immutable.Set Set]] subtypes' companions. */
	@inline implicit final def immutableSetFactoryExtension[C[X] <: SetOps[X, C, C[X]]]
	                                                       (self :IterableFactory[C]) :immutableSetFactoryExtension[C] =
		new immutableSetFactoryExtension(self)

	/** Additional, higher level factory methods of any [[Iterable]] type `C[_]` as extensions of its companion
	  * [[scala.collection.IterableFactory IterableFactory]]`[C]`.
	  * Adds methods [[net.noresttherein.sugar.collections.extensions.IterableFactoryExtension.expand expand]]
	  * and [[net.noresttherein.sugar.collections.extensions.IterableFactoryExtension.generate generate]]
	  * to any [[scala.collection.IterableFactory IterableFactory]].
	  */
	@inline implicit final def IterableFactoryExtension[C[_]](self :IterableFactory[C]) :IterableFactoryExtension[C] =
		new IterableFactoryExtension[C](self)


	/** Extension factory methods creating single and two element [[Map Map]]s. */
	@inline implicit final def immutableMapCompanionExtension(self :Map.type) :immutableMapCompanionExtension =
		new immutableMapCompanionExtension {}


	/** Adds several extensions methods to `Stepper` object for creating small steppers. */
	@inline implicit final def StepperCompanionExtension(self :Stepper.type) :StepperCompanionExtension =
		new StepperCompanionExtension {}

	/** Adds an extension method for summoning a `StepperShape` for a particular element type: `StepperShape[T]()`. */
	@inline implicit final def StepperShapeCompanionExtension(self :StepperShape.type) :StepperShapeCompanionExtension =
		new StepperShapeCompanionExtension {}

	/** Adds several new extension factory methods to object `Iterator`. */
	@inline implicit final def IteratorCompanionExtension(self :Iterator.type) :IteratorCompanionExtension =
		new IteratorCompanionExtension {}


	/** An extension method [[net.noresttherein.sugar.collections.extensions.BuilderExtension.mapInput mapElements]]
	  * which adapts the builder to a new element type.
	  */
	@inline implicit final def BuilderExtension[E, C](self :Builder[E, C]) :BuilderExtension[E, C] =
		new BuilderExtension(self)

	/** Adds [[net.noresttherein.sugar.collections.extensions.StringBuilderExtension.appendln appendln]]
	  * family of methods to `StringBuilder`.
	  */
	@inline implicit final def StringBuilderExtension(self :StringBuilder) :StringBuilderExtension =
		new StringBuilderExtension(self)

	/** Adds Scala [[scala.collection.mutable.Growable Growable]] and [[scala.collection.mutable.Builder Builder]]
	  * methods as inlined delegates to the Java [[java.lang.StringBuilder StringBuilder]].
	  * While it essentially duplicates the functionality of standard Scala
	  * [[scala.collection.mutable.StringBuilder StringBuilder]], the wrapper is not intended to be as an object
	  * or referred to directly by the application, but rather a provider of extension methods, and returns always
	  * the original `StringBuilder`. As methods are inlined, this incurs neither the penalty of creating an additional
	  * object, nor of delegating individual calls, at least with default compiler optimisations on.
	  */
	@inline implicit final def JavaStringBuilderExtension(self :JStringBuilder) :JavaStringBuilderExtension =
		new JavaStringBuilderExtension(self)

	/** Extension methods for [[scala.collection.Factory Factory]] returning the collection companion object
	  * which created it.
	  */
	@inline implicit final def FactoryExtension[E, C](self :Factory[E, C]) :FactoryExtension[E, C] =
		new FactoryExtension(self)
}




//todo: add releaseFence() everywhere where necessary
@SerialVersionUID(Ver)
object extensions extends extensions {

	/** A syntactic wrapper for collections, injecting methods implementing
	  * 'breakable' folding and reducing, that is folding only some prefix/suffix of the collection
	  * based on some predicate or other termination condition.
	  *
	  * @param self any collection to fold.
	  * @tparam E element type of this collection.
	  */ //todo: use Pill and Potential
	class IterableOnceExtension[E] private[collections](private val self :IterableOnce[E]) extends AnyVal {

		/** True for collections known to be non strict: [[collection.View View]], [[collection.Iterator Iterator]],
		  * [[collection.immutable.LazyList LazyList]] and [[collection.immutable.Stream Stream]]. Returns `false`
		  * even for non standard lazy collections.
		  */
		@nowarn("cat=deprecation")
		def knownLazy :Boolean = self match {
			case _ :Iterator[_] | _ :View[_] | _ :LazyList[_] | _ :Stream[_] => true
			case _ => false
		}

		/** Returns `this` if this collection is an [[collection.Iterable Iterable]], or `this.iterator` otherwise. */
		@inline def toIterableOnceOps :IterableOnceOps[E, IterableOnce, IterableOnce[E]] = self match {
			case items :Iterable[E] => items
			case iter  :Iterator[E] => iter
			case _                  => self.iterator
		}

		/** Returns `this` if this collection is an `IterableOnceOps`,
		  * or `this.`[[collection.IterableOnce.iterator iterator]] otherwise.
		  * It is very similar to [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.toIterableOnceOps toOps]],
		  * but will return `this` not only for [[collection.Iterable Iterable]] as the former,
		  * delegating to `iterator` in fewer cases, but the result's 'self' type is unknown,
		  * meaning all methods which create collections may return arbitrary objects, not an `IterableOnce`.
		  */
		@inline def toBasicOps :IterableOnce[E] with IterableOnceOps[E, generic.Any, _] = self match {
			case ops  :IterableOnceOps[E @unchecked, _, _] => ops
			case _                                         => self.iterator
		}

		/** Creates a Java [[java.util.Iterator Iterator]] of a proper specialization for type `T`
		  * (`Int`, `Long`, `Double`). If the underlying collection provides a specialized (non boxing)
		  * [[scala.collection.Stepper Stepper]], then the returned iterator will not box value types.
		  */
		def javaIterator[I <: JavaIterator[_]](implicit shape :JavaIteratorShape[E, I]) :I = self match {
			case sugared :SugaredIterable[E]                  => sugared.javaIterator
			case empty   :Iterable[_] if empty.knownSize == 0 => JavaIterator()
//			case empty :Iterable[_] if empty.isEmpty => JavaIterator()
			case _ :ArraySeq[E] | _ :mutable.ArraySeq[E] | _ :ArrayBuffer[E] =>
				self.stepper(shape.stepperShape).javaIterator.asInstanceOf[I]
			case ErasedArray.Wrapped.Slice(array, from, until) =>
				JavaIterator.slice(array.asInstanceOf[Array[E]], from, until)
			case seq :collection.IndexedSeq[E]       => JavaIterator.over(seq)
			case it :Iterator[_] if !it.hasNext      => JavaIterator()
			case _                                   => self.stepper(shape.stepperShape).javaIterator.asInstanceOf[I]
		}

		/** Creates a manually specialized iterator-like object for this collection.
		  * @param shape implicit evidence specifying the right subtype of `Jterator` for elements of type `E`.
		  * @tparam I    a specific subtype of `Jterator` manually specialized to the element type of this collection.
		  * @return an opaque wrapper of a java iterator (possibly a `PrimitiveIterator`) providing iterator API
		  *         as extension methods.
		  */
		def jterator[I <: Jterator[_]](implicit shape :JteratorShape[E, I]) :I =
			javaIterator(shape.javaIteratorShape).asInstanceOf[I]

		//consider: making the below methods available only on IterableOnceOps
		/** Equivalent to standard `fold`, but the folding functions takes the number of already folded elements
		  * (i.e., how many times the function has been called, or the position of the element in the collection's
		  * iteration order).
		  */
		def foldWithIndex[A >: E](start :A)(op :(A, A, Int) => A) :A = foldLeftWithIndex(start)(op)

		/** Equivalent to standard `foldLeft`, but the folding functions takes the number of already folded elements
		  * (i.e., how many times the function has been called, or the position of the element in the collection's
		  * iteration order).
		  */
		def foldLeftWithIndex[A](start :A)(op :(A, E, Int) => A) :A = {
			if (knownEmpty(self))
				return start
			self match {
				case _ if knownEmpty(self) => start
				case seq   :collection.LinearSeq[E] =>
					@tailrec def foldList(i :Int, acc :A, list :collection.LinearSeq[E]) :A =
						if (list.isEmpty) acc
						else foldList(i + 1, op(acc, list.head, i), list.tail)
					foldList(0, start, seq)
				case ErasedArray.Wrapped.Slice(array, from, until) =>
					def foldArray(array :Array[E]) :A = {
						var i = 0; val end = until - from
						var acc = start
						while (i < end) {
							acc = op(acc, array(from + i), i)
							i += 1
						}
						acc
					}
					foldArray(array.castFrom[Array[_], Array[E]])
				case ApplyPreferred(seq) =>
					def foldIndexed :A = {
						var i = 0; val end = seq.length
						var acc = start
						while (i < end) {
							acc = op(acc, seq(i), i)
							i += 1
						}
						acc
					}
					foldIndexed
				case _ =>
					def foldIterator :A = {
						val it  = self.iterator
						var i   = 0;
						var acc = start
						while (it.hasNext) {
							acc = op(acc, it.next(), i)
							i += 1
						}
						acc
					}
					foldIterator
			}
		}

		/** Folds this collection until the folded value satisfies the condition `pred`.
		  * Function `op` is applied recursively to the elements of this collection and its previous results,
		  * in an undetermined order, until it returns a value for which `pred` is `true`.
		  * If `pred(start)`, `start` is returned immediately.
		  * The method returns the first value returned by recursive application of `op` for which `pred` is `true`,
		  * or the result of folding the whole collection if no such value is generated. If `pred(start)`,
		  * or this collection is empty, then `start` is returned immediately, without ever calling `op`.
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
		def foldUntil[A >: E](start :A)(pred :A => Boolean)(op :(A, A) => A) :A =
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
		def foldUntilOption[A >: E](start :A)(pred :A => Boolean)(op :(A, A) => A) :Option[A] =
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
		def foldUntilEither[A >: E](start :A)(pred :A => Boolean)(op :(A, A) => A) :Either[A, A] =
			foldLeftUntilEither(start)(pred)(op)


		private def foldLeftUntilAndReturn[A, X](start :A)(pred :A => Boolean)(op :(A, E) => A)
		                                        (ifNotFound :A => X, ifFound :A => X) :X =
			self match {
				case seq :collection.LinearSeq[E] =>
					@tailrec def foldList(acc :A, list :collection.LinearSeq[E]) :X =
						if (pred(acc)) ifFound(acc)
						else if (list.isEmpty) ifNotFound(acc)
						else foldList(op(acc, list.head), list.tail)
					foldList(start, seq)
				case _ if pred(start)      => ifFound(start)
				case _ if knownEmpty(self) => ifNotFound(start)
				case ErasedArray.Wrapped.Slice(array, from, until) =>
					def foldArray(array :Array[E]) :X = {
						var acc = start; var i = from
						while (i < until) {
							acc = op(acc, array(i))
							if (pred(acc))
								return ifFound(acc)
							i += 1
						}
						ifNotFound(acc)
					}
					foldArray(array.asInstanceOf[Array[E]])
				case ApplyPreferred(seq) =>
					def foldIndexed :X = {
						var acc = start; var i = 0; val end = seq.length
						while (i < end) {
							acc = op(acc, seq(i))
							if (pred(acc))
								return ifFound(acc)
							i += 1
						}
						ifNotFound(acc)
					}
					foldIndexed
				case _ =>
					def foldIterator :X = {
						var acc = start; val i = self.iterator
						while (i.hasNext) {
							acc = op(acc, i.next())
							if (pred(acc))
								return ifFound(acc)
						}
						ifNotFound(acc)
					}
					foldIterator
			}

		/** Folds this collection from left to right by applying `op` to its elements and the previously returned value
		  * until the folded value satisfies the condition `pred`.
		  * Function `op` is applied recursively to the elements of this collection in its iteration order.
		  * The method returns the first value returned by recursive application of `op` for which `pred` is `true`,
		  * or the result of folding the whole collection if no such value is generated. If `pred(start)`,
		  * or this collection is empty, then `start` is returned immediately, without ever calling `op`.
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
		def foldLeftUntil[A](start :A)(pred :A => Boolean)(op :(A, E) => A) :A =
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
		  *    1. Otherwise, apply `op` recursively and return the first `Right(ai)`, `a_0 = start; a_i+1 = op(a_i, ei)`,
		  *       where `toSeq == Seq(e0,...,en)`, such that `pred(ai)`;
		  *    1. If, after folding the whole collection, the predicate is still not satisfied, then return `None`
		  */
		def foldLeftUntilOption[A](start :A)(pred :A => Boolean)(op :(A, E) => A) :Option[A] =
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
		  *    1. Otherwise, apply `op` recursively and return the first `Righta_i)`, `a_0 = start; a_i+1 = op(a_i, ei)`,
		  *       where `toSeq == Seq(e0,...,en)`, such that `pred(a_i)`;
		  *    1. If `!pred(ai)` for all `ai` then return `Left(an)`.
		  */
		def foldLeftUntilEither[A](start :A)(pred :A => Boolean)(op :(A, E) => A) :Either[A, A] =
			foldLeftUntilAndReturn(start)(pred)(op)(Left.apply, Right.apply)


		@tailrec private def foldRightUntilAndReturn[A, X](start :A)(pred :A => Boolean)(op :(E, A) => A)
		                                                  (ifNotFound :A => X, ifFound :A => X) :X =
			self match {
				case _ if pred(start) => ifFound(start)
				case _ if knownEmpty(self) => ifNotFound(start)
				case ErasedArray.Wrapped.Slice(array, from, until) =>
					def foldArray(array :Array[E]) :X = {
						var acc = start; var i = until
						while (i > from) {
							i -= 1
							acc = op(array(i), acc)
							if (pred(acc))
								return ifFound(acc)
						}
						ifNotFound(acc)
					}
					foldArray(array.asInstanceOf[Array[E]])
				case ApplyPreferred(seq) =>
					def foldIndexed :X = {
						var acc = start; var i = seq.length
						while (i > 0) {
							i -= 1
							acc = op(seq(i), acc)
							if (pred(acc))
								return ifFound(acc)
						}
						ifNotFound(acc)
					}
					foldIndexed
				case seq :collection.SeqOps[E, generic.Any, _] =>
					def foldSeq(seq :collection.SeqOps[E, generic.Any, _]) :X = {
						val it  = seq.reverseIterator
						var acc = start
						while (it.hasNext) {
							acc = op(it.next(), acc)
							if (pred(acc))
								return ifFound(acc)
						}
						ifNotFound(acc)
					}
					foldSeq(seq)
				case _ =>
					IterableOnceExtension(
						TemporaryBuffer.from(self)
					).foldRightUntilAndReturn(start)(pred)(op)(ifNotFound, ifFound)
			}


		/** Folds this collection from right to left by applying `op` to its elements and the previously returned value
		  * until the folded value satisfies the condition `pred`.
		  * Function `op` is applied recursively to the elements of this collection in the reverse iteration order.
		  * If `pred(start)`, `start` is returned immediately.
		  * The method returns the first value returned by recursive application of `op` for which `pred` is `true`,
		  * or the result of folding the whole collection if no such value is generated. If `pred(start)`,
		  * or this collection is empty, then `start` is returned immediately, without ever calling `op`.
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
		def foldRightUntil[A](start :A)(pred :A => Boolean)(op :(E, A) => A) :A =
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
		  *    1. Otherwise, apply `op` recursively and return the first `Some(ai)`,
		  *       `a_0 = start; a_i+1 = op(e_n-i, a_i)`, where `toSeq == Seq(e1,...,en)`, such that `pred(ai)`;
		  *    1. If, after folding the whole collection, the predicate is still not satisfied, then return `None`
		  */
		def foldRightUntilOption[A](start :A)(pred :A => Boolean)(op :(E, A) => A) :Option[A] =
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
		  *    1. Otherwise, apply `op` recursively and return the first `Right(ai)`,
		  *       `a_0 = start; a_i+1 = op(e_n-i, a_i)`, where `toSeq == Seq(e1,...,en)`, such that `pred(ai)`;
		  *    1. If `!pred(ai)` for all `ai` then return `Left(an)`.
		  */
		def foldRightUntilEither[A](start :A)(pred :A => Boolean)(op :(E, A) => A) :Either[A, A] =
			foldRightUntilAndReturn(start)(pred)(op)(Left.apply, Right.apply)



		/** Same as [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldLeftWhile foldLeftWhile]]. */
		def foldWhile[A >: E](start :A)(pred :A => Boolean)(op :(A, A) => A) :A =
			foldLeftWhile(start)(pred)(op)

		/** Same as [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldLeftWhileOption foldLeftWhileOption]]. */
		def foldWhileOption[A >: E](start :A)(pred :A => Boolean)(op :(A, A) => A) :Option[A] =
			foldLeftWhileOption(start)(pred)(op)

		/** Same as [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.foldLeftWhileEither foldLeftWhileEither]]. */
		def foldWhileEither[A >: E](start :A)(pred :A => Boolean)(op :(A, A) => A) :Either[A, A] =
			foldLeftWhileEither(start)(pred)(op)


		private def foldLeftWhileAndReturn[A, X](start :A)(pred :A => Boolean)(op :(A, E) => A)
		                                        (ifNotFound :A => X, ifFound :A => X) :X =
			self match {
				case _ if !pred(start) => ifNotFound(start)
				case _ if knownEmpty(self) => ifFound(start)
				case seq :collection.LinearSeq[E] =>
					@tailrec def foldList(acc :A, list :collection.LinearSeq[E]) :X =
						if (list.isEmpty)
							ifFound(acc)
						else {
							val next = op(acc, list.head)
							if (pred(next))
								foldList(next, list.tail)
							else
								ifFound(acc)
						}
					foldList(start, seq)
				case ErasedArray.Wrapped.Slice(array, from, until) =>
					def foldArray(array :Array[E]) :X = {
						var acc = start; var i = from
						while (i < until) {
							val next = op(acc, array(i))
							if (!pred(next))
								return ifFound(acc)
							acc = next
							i += 1
						}
						ifFound(acc)
					}
					foldArray(array.castFrom[Array[_], Array[E]])
				case ApplyPreferred(seq) =>
					def foldIndexed :X = {
						var acc = start; var i = 0;
						val end = seq.length
						while (i < end) {
							val next = op(acc, seq(i))
							if (!pred(next))
								return ifFound(acc)
							acc = next
							i += 1
						}
						ifFound(acc)
					}
					foldIndexed
				case _ =>
					def foldIterator :X = {
						var last = start
						val i    = self.iterator
						while (i.hasNext) {
							val next = op(last, i.next())
							if (!pred(next))
								return ifFound(last)
							last = next
						}
						ifFound(last)
					}
					foldIterator
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
		def foldLeftWhile[A](start :A)(pred :A => Boolean)(op :(A, E) => A) :A =
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
		def foldLeftWhileOption[A](start :A)(pred :A => Boolean)(op :(A, E) => A) :Option[A] =
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
		def foldLeftWhileEither[A](start :A)(pred :A => Boolean)(op :(A, E) => A) :Either[A, A] =
			foldLeftWhileAndReturn(start)(pred)(op)(Left.apply, Right.apply)


		@tailrec private def foldRightWhileAndReturn[A, X](start :A)(pred :A => Boolean)(op :(E, A) => A)
		                                                  (ifNotFound :A => X, ifFound: A => X) :X =
			self match {
				case _ if !pred(start)     => ifNotFound(start)
				case _ if knownEmpty(self) => ifFound(start)
				case ErasedArray.Wrapped.Slice(array, from, until) =>
					def foldArray(array :Array[E]) :X = {
						var acc = start; var i = until
						while (i > from) {
							i -= 1
							val next = op(array(i), acc)
							if (!pred(next))
								return ifFound(acc)
							acc = next
						}
						ifFound(acc)
					}
					foldArray(array.castFrom[Array[_], Array[E]])
				case ApplyPreferred(seq) =>
					def foldIndexed :X = {
						var acc = start; var i = seq.length
						while (i > 0) {
							i -= 1
							val next = op(seq(i), acc)
							if (!pred(next))
								return ifFound(acc)
							acc = next
						}
						ifFound(acc)
					}
					foldIndexed
				case seq :collection.SeqOps[E, generic.Any, _] =>
					def foldSeq(seq :collection.SeqOps[E, generic.Any, _]) :X = {
						var last = start
						val it   = seq.reverseIterator
						while (it.hasNext) {
							val next = op(it.next(), last)
							if (!pred(next))
								return ifFound(last)
							last = next
						}
						ifFound(last)
					}
					foldSeq(seq)
				case _ =>
					IterableOnceExtension(
						TemporaryBuffer.from(self)
					).foldRightWhileAndReturn(start)(pred)(op)(ifNotFound, ifFound)
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
		def foldRightWhile[A](start :A)(pred :A => Boolean)(op :(E, A) => A) :A =
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
		def foldRightWhileOption[A](start :A)(pred :A => Boolean)(op :(E, A) => A) :Option[A] =
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
		def foldRightWhileEither[A](start :A)(pred :A => Boolean)(op :(E, A) => A) :Either[A, A] =
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
		def partialFold[A >: E](start :A)(op :PartialFunction[(A, A), A]) :A = partialFoldLeft(start)(op)

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
		def partialFoldLeft[A](start :A)(op :PartialFunction[(A, E), A]) :A = {
			if (knownEmpty(self))
				return start
			val fallback = new PartialFoldLeftFunction[A, E]
			self match {
				case seq :collection.LinearSeq[E] =>
					@tailrec def foldList(acc :A, list :collection.LinearSeq[E]) :A =
						if (list.isEmpty)
							acc
						else {
							val next =  op.applyOrElse((acc, list.head), fallback)
							if (fallback.wasCalled) acc
							else foldList(next, list.tail)
						}
					foldList(start, seq)
				case ErasedArray.Wrapped.Slice(array, from, until) =>
					def foldArray(array :Array[E]) :A = {
						var last = start; var i = from
						while (i < until) {
							val next = op.applyOrElse((last, array(i)), fallback)
							if (fallback.wasCalled)
								return last
							last = next
							i += 1
						}
						last
					}
					foldArray(array.castFrom[Array[_], Array[E]])
				case ApplyPreferred(seq) =>
					def foldIndexed :A = {
						var last = start; var i = 0; val end = seq.length
						while (i < end) {
							val next = op.applyOrElse((last, seq(i)), fallback)
							if (fallback.wasCalled)
								return last
							last = next
							i += 1
						}
						last
					}
					foldIndexed
				case _ =>
					def foldIterator :A = {
						val it   = self.iterator
						var last = start
						while (it.hasNext) {
							val next = op.applyOrElse((last, it.next()), fallback)
							if (fallback.wasCalled)
								return last
							last = next
						}
						last
					}
					foldIterator
			}
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
		@tailrec final def partialFoldRight[A](start :A)(op :PartialFunction[(E, A), A]) :A = {
			if (knownEmpty(self))
				return start
			val fallback = new PartialFoldRightFunction[A, E]
			self match {
				case ErasedArray.Wrapped.Slice(array, from, until) =>
					def foldArray(array :Array[E]) :A = {
						var i = until; var last = start
						while (i > from) {
							i -= 1
							last = op.applyOrElse((array(i), last), fallback)
							if (fallback.wasCalled)
								return last
						}
						last
					}
					foldArray(array.asInstanceOf[Array[E]])
				case ApplyPreferred(seq) =>
					def foldIndexed :A = {
						var i = seq.length; var last = start
						while (i > 0) {
							i -= 1
							last = op.applyOrElse((seq(i), last), fallback)
							if (fallback.wasCalled)
								return last
						}
						last
					}
					foldIndexed
				case seq :collection.SeqOps[E, generic.Any, _] =>
					def foldSeq(seq :collection.SeqOps[E, generic.Any, _]) :A = {
						val it   = seq.reverseIterator
						var last = start
						while (it.hasNext) {
							last = op.applyOrElse((it.next(), last), fallback)
							if (fallback.wasCalled)
								return last
						}
						last
					}
					foldSeq(seq)
				case _ =>
					TemporaryBuffer.from(self).partialFoldRight(start)(op)
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
		def foldSome[A >: E](start :A)(op :(A, A) => Option[A]) :A = foldLeftSome(start)(op)

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
		def foldLeftSome[A](start :A)(op :(A, E) => Option[A]) :A =
			self match {
				case seq :collection.LinearSeq[E] =>
					@tailrec def foldList(last :A, list :collection.LinearSeq[E]) :A =
						if (list.isEmpty) last
						else op(last, list.head) match {
							case Some(next) => foldList(next, list.tail)
							case _ => last
						}
						foldList(start, seq)
				case _ if knownEmpty(self) => start
				case ErasedArray.Wrapped.Slice(array, from :Int, until :Int) =>
					def foldArray(array :Array[E]) :A = {
						var last = start; var i = from
						while (i < until)
							op(last, array(i)) match {
								case Some(next) => last = next; i += 1
								case _ => return last
							}
						last
					}
					foldArray(array.castFrom[Array[_], Array[E]])
				case ApplyPreferred(seq)  =>
					def foldIndexed :A = {
						var last = start; var i = 0; val end = seq.length
						while (i < end)
							op(last, seq(i)) match {
								case Some(next) => last = next; i += 1
								case _ => return last
							}
						last
					}
					foldIndexed
				case _ =>
					def foldIterator :A = {
						val it   = self.iterator
						var last = start
						while (it.hasNext)
							op(last, it.next()) match {
								case Some(next) => last = next
								case _ => return last
							}
						last
					}
					foldIterator
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
		@tailrec final def foldRightSome[A](start :A)(op :(E, A) => Option[A]) :A = {
			if (knownEmpty(self))
				return start
			self match {
				case ErasedArray.Wrapped.Slice(array, from :Int, until :Int) =>
					def foldArray(array :Array[E]) :A = {
						var last = start; var i = until
						while (i > from) {
							i -= 1
							op(array(i), last) match {
								case Some(next) => last = next
								case _ => return last
							}
						}
						last
					}
					foldArray(array.castFrom[Array[_], Array[E]])
				case ApplyPreferred(seq) =>
					def foldIndexed :A = {
						var last = start; var i = seq.length
						while (i > 0) {
							i -= 1
							op(seq(i), last) match {
								case Some(next) => last = next
								case _ => return last
							}
						}
						last
					}
					foldIndexed
				case seq :collection.SeqOps[E, generic.Any, _] =>
					def foldSeq(seq :collection.SeqOps[E, generic.Any, _]) :A = {
						var last = start
						val it   = seq.reverseIterator
						while (it.hasNext)
							op(it.next(), last) match {
								case Some(next) => last = next
								case _ => return last
							}
						last
					}
					foldSeq(seq)
				case _ =>
					TemporaryBuffer.from(self).foldRightSome(start)(op)
			}
		}


		/** Same as [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.reduceLeftUntil reduceLeftUntil]]. */
		def reduceUntil[A >: E](pred :A => Boolean)(op :(A, A) => A) :A = reduceLeftUntil(pred)(op)

		/** Same as [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.reduceLeftUntilOption reduceLeftUntilOption]]. */
		def reduceUntilOption[A >: E](pred :A => Boolean)(op :(A, A) => A) :Option[A] =
			reduceLeftUntilOption(pred)(op)


		private def reduceLeftUntilAndReturn[A >: E, X](pred :A => Boolean)(op :(A, E) => A)
		                                               (ifEmpty: => X, ifNotFound :A => X, ifFound :A => X) :X =

			self match {
				case seq :collection.LinearSeq[E] =>
					@tailrec def foldList(acc :A, list :collection.LinearSeq[E]) :X =
						if (pred(acc)) ifFound(acc)
						else if (list.nonEmpty) foldList(op(acc, list.head), list.tail)
						else ifNotFound(acc)
					if (seq.isEmpty) ifEmpty
					else foldList(seq.head, seq.tail)
				case _ if knownEmpty(self) => ifEmpty
				case ErasedArray.Wrapped.Slice(array, from, until) =>
					def foldArray(array :Array[E]) :X = {
						var last :A = array(from)
						var i = from + 1
						while (i < until) {
							if (pred(last))
								return ifFound(last)
							last = op(last, array(i))
							i += 1
						}
						if (pred(last)) ifFound(last) else ifNotFound(last)
					}
					if (from == until) ifEmpty else foldArray(array.castFrom[Array[_], Array[E]])
				case ApplyPreferred(seq) =>
					def foldIndexed :X = {
						var last :A = seq.head; var i = 1; val end = seq.length
						while (i < end) {
							if (pred(last))
								return ifFound(last)
							last = op(last, seq(i))
							i += 1
						}
						ifNotFound(last)
					}
					if (seq.isEmpty) ifEmpty
					else foldIndexed
				case _ =>
					def foldIterator(i :Iterator[E]) :X = {
						var last :A = i.next()
						while (i.hasNext) {
							if (pred(last))
								return ifFound(last)
							last = op(last, i.next())
						}
						if (pred(last)) ifFound(last) else ifNotFound(last)
					}
					val i = self.iterator
					if (i.hasNext) foldIterator(i) else ifEmpty
			}

		/** Reduces this collection, going from left to right, for as long as the given predicate is not satisfied.
		  * The method returns the first value which satisfies the predicate, or the result of reducing
		  * the whole collection, if it never becomes satisfied. If `pred(this.head)`, then it is returned,
		  * without invoking the function.
		  */
		@throws[UnsupportedOperationException]("if this collection is empty.")
		def reduceLeftUntil[A >: E](pred :A => Boolean)(op :(A, E) => A) :A =
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
		def reduceLeftUntilOption[A >: E](pred :A => Boolean)(op :(A, E) => A) :Option[A] =
			reduceLeftUntilAndReturn(pred)(op)(None, _ => None, Some.apply)


		@tailrec private def reduceRightUntilAndReturn[A >: E, X](pred :A => Boolean)(op :(E, A) => A)
		                                                         (ifEmpty: => X, ifNotFound :A => X, ifFound :A => X) :X =
		{
			if (knownEmpty(self))
				return ifEmpty
			self match {
				case ErasedArray.Wrapped.Slice(array, from :Int, until :Int) =>
					def reduceArray(array :Array[E]) :X = {
						var i = until - 1
						var last :A = array(i)
						while (i > 0) {
							i -= 1
							if (pred(last))
								return ifFound(last)
							last = op(array(i), last)
						}
						if (pred(last)) ifFound(last) else ifNotFound(last)
					}
					if (from == until) ifEmpty else reduceArray(array.asInstanceOf[Array[E]])
				case ApplyPreferred(seq) =>
					def reduceIndexed: X = {
						var i = seq.length
						if (i == 0)
							return ifEmpty
						i -= 1
						var last :A = seq.last
						while (i > 0) {
							i -= 1
							if (pred(last))
								return ifFound(last)
							last = op(seq(i), last)
						}
						if (pred(last)) ifFound(last) else ifNotFound(last)
					}
					reduceIndexed
				case seq :collection.SeqOps[E, generic.Any, _] =>
					def reduceSeq(seq :collection.SeqOps[E, generic.Any, _]) :X = {
						if (seq.isEmpty)
							return ifEmpty
						val i       = seq.reverseIterator
						var last :A = i.next()
						while (i.hasNext) {
							if (pred(last))
								return ifFound(last)
							last = op(i.next(), last)
						}
						if (pred(last)) ifFound(last) else ifNotFound(last)
					}
					reduceSeq(seq)
				case _ =>
					IterableOnceExtension(
						TemporaryBuffer from self
					).reduceRightUntilAndReturn(pred)(op)(ifEmpty, ifNotFound, ifFound)
			}
		}

		/** Reduces this collection, going from right to left, for as long as the given predicate is not satisfied.
		  * The method returns the first value which satisfies the predicate, or the result of reducing
		  * the whole collection, if it never becomes satisfied. If `pred(this.last)`, then it is returned,
		  * without invoking the function.
		  */
		@throws[UnsupportedOperationException]("if this collection is empty.")
		def reduceRightUntil[A >: E](pred :A => Boolean)(op :(E, A) => A) :A =
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
		def reduceRightUntilOption[A >: E](pred :A => Boolean)(op :(E, A) => A) :Option[A] =
			reduceRightUntilAndReturn(pred)(op)(None, _ => None, Some.apply)


		/** Same as [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.partialReduceLeft partialReduceLeft]]. */
		def partialReduce[A >: E](f :PartialFunction[(A, A), A]) :A =
			partialReduceLeft(f)

		/** Reduces this collection with the given function, going from left to right,
		  * for as long as the partial function is applicable to the pair of the last value and the next item
		  * in the collection. The method returns the first value `a` - either the first element of the collection,
		  * or the last value returned by `f` - for which `!f.isDefinedAt(a, iter.next())`.
		  */
		@throws[UnsupportedOperationException]("if this collection is empty.")
		def partialReduceLeft[A >: E](f :PartialFunction[(A, E), A]) :A = {
			val fallback = new PartialFoldLeftFunction[A, E]
			self match {
				case seq :collection.LinearSeq[E] =>
					@tailrec def reduceList(last :A, list :collection.LinearSeq[E]) :A =
						if (list.isEmpty)
							last
						else {
							val next = f.applyOrElse((last, list.head), fallback)
							if (fallback.wasCalled) last
							else reduceList(next, list.tail)
						}
					if (seq.isEmpty)
						unsupported_!("partialReduceLeft on an empty " + self.className)
					reduceList(seq.head, seq.tail)
				case ErasedArray.Wrapped.Slice(array, from, until) =>
					def reduceArray(array :Array[E]) :A = {
						var i = from + 1; var last :A = array(from)
						while (i < until) {
							last = f.applyOrElse((last, array(i)), fallback)
							if (fallback.wasCalled)
								return last
							i += 1
						}
						last
					}
					if (until == from)
						unsupported_!("partialReduceLeft on an empty " + self.className)
					reduceArray(array.castFrom[Array[_], Array[E]])
				case ApplyPreferred(seq) =>
					val len = seq.length
					def reduceIndexed :A = {
						var i = len - 2; var last :A = seq.last
						while (i >= 0) {
							last = f.applyOrElse((last, seq(i)), fallback)
							if (fallback.wasCalled)
								return last
							i -= 1
						}
						last
					}
					if (len == 0)
						unsupported_!("partialReduceLeft on an empty " + self.className)
					reduceIndexed
				case _ =>
					def reduceIterator(i :Iterator[E]) :A = {
						var last :A = i.next()
						while (i.hasNext) {
							last = f.applyOrElse((last, i.next()), fallback)
							if (fallback.wasCalled)
								return last
						}
						last
					}
					val i = self.iterator
					if (!i.hasNext)
						unsupported_!("partialReduceLeft on an empty " + self.className)
					reduceIterator(i)
			}
		}

		/** Reduces this collection with the given function, going from right to left,
		  * for as long as the partial function is applicable to the pair of the last value and the next item
		  * in the collection. The method returns the first value `a` - either the last element of the collection,
		  * or the last value returned by `f` - for which `!f.isDefinedAt(reverseIter.next(), a)`.
		  */
		@throws[UnsupportedOperationException]("if this collection is empty.")
		def partialReduceRight[A >: E](f :PartialFunction[(E, A), A]) :A =
			self match {
				case ErasedArray.Wrapped.Slice(array, from, until) =>
					val fallback = new PartialFoldRightFunction[A, E]
					def reduceArray(array :Array[E]) :A = {
						var i = until - 1; var last :A = array(i)
						while (i  > 0) {
							i -= 1
							last = f.applyOrElse((array(i), last), fallback)
							if (fallback.wasCalled)
								return last
						}
						last
					}
					if (until <= from)
						unsupported_!("partialReduceRight on an empty " + self.className)
					reduceArray(array.castFrom[Array[_], Array[E]])
				case ApplyPreferred(seq) =>
					val fallback = new PartialFoldRightFunction[A, E]
					val len = seq.length
					def reduceIndexed :A = {
						var i = len - 2; var last :A = seq.last
						while (i >= 0) {
							last = f.applyOrElse((seq(i), last), fallback)
							if (fallback.wasCalled)
								return last
							i -= 1
						}
						last
					}
					if (len == 0)
						unsupported_!("partialReduceRight on an empty " + self.className)
					reduceIndexed
				case seq :collection.SeqOps[E, generic.Any, _] =>
					def reduceSeq(seq :collection.SeqOps[E, generic.Any, _]) :A = {
						val fallback = new PartialFoldRightFunction[A, E]
						val i        = seq.reverseIterator
						if (!i.hasNext)
							unsupported_!("partialReduceRight on an empty " + seq.className)
						var last :A = i.next()
						while (i.hasNext) {
							last = f.applyOrElse((i.next(), last), fallback)
							if (fallback.wasCalled)
								return last
						}
						last
					}
					reduceSeq(seq)
				case _ =>
					TemporaryBuffer.from(self).partialReduceRight(f)
			}

		/** Same as [[net.noresttherein.sugar.collections.extensions.IterableOnceExtension.reduceLeftSome reduceLeftSome]]. */
		def reduceSome[A >: E](f :(A, A) => Option[A]) :A = reduceLeftSome(f)

		/** Reduces this collection with the given function, going from left to right, for as long as it returns `Some`.
		  * Once the function returns `None`, or the whole collection is reduced, the last value returned
		  * by `f` in `Some` is returned.
		  */
		@throws[UnsupportedOperationException]("if the collection is empty.")
		def reduceLeftSome[A >: E](f :(A, E) => Option[A]) :A =
			self match {
				case seq :collection.LinearSeq[E] =>
					@tailrec def reduceList(last :A, list :collection.LinearSeq[E]) :A =
						if (list.isEmpty)
							last
						else f(last, list.head) match {
							case Some(next) => reduceList(next, list.tail)
							case _ => last
						}
					if (seq.isEmpty)
						unsupported_!("reduceLeftSome on an empty " + self.className)
					reduceList(seq.head, seq.tail)
				case ErasedArray.Wrapped.Slice(array, from, until) =>
					def foldArray(array :Array[E]) :A = {
						if (until <= from)
							unsupported_!("reduceLeftSome on an empty " + self.className)
						var i = from + 1; var last :A = array(from)
						while (i < until)
							f(last, array(i)) match {
								case Some(next) => last = next; i += 1
								case _ => return last
							}
						last
					}
					foldArray(array.asInstanceOf[Array[E]])
				case ApplyPreferred(seq) =>
					def foldIndexed :A = {
						val len = seq.length
						var last :A = seq.head; var i = 1
						while (i < len)
							f(last, seq(i)) match {
								case Some(next) => last = next; i += 1
								case _ => return last
							}
						last
					}
					if (seq.isEmpty)
						unsupported_!("reduceLeftSome on an empty " + self.className)
					foldIndexed
				case _ =>
					def foldIterator :A = {
						val it = self.iterator
						if (it.isEmpty)
							unsupported_!("reduceLeftSome on an empty " + self.className)
						var last :A = it.next()
						while (it.hasNext)
							f(last, it.next()) match {
								case Some(a) => last = a
								case _ => return last
							}
						last
					}
					foldIterator
			}

		/** Reduces this collection with the given function, going from right to left, for as long as it returns `Some`.
		  * Once the function returns `None`, or the whole collection is reduced, the last value returned
		  * by `f` in `Some` is returned.
		  */
		@throws[UnsupportedOperationException]("if the collection is empty.")
		def reduceRightSome[A >: E](f :(E, A) => Option[A]) :A =
			self match {
				case ErasedArray.Wrapped.Slice(array, from, until) =>
					def foldArray(array :Array[E]) :A = {
						var i = until - 1
						var last :A = array(i)
						i -= 1
						while (i >= from)
							f(array(i), last) match {
								case Some(next) => last = next; i -= 1
								case _ => return last
							}
						last
					}
					if (until == from)
						unsupported_!("reduceRightSome on an empty " + self.className)
					foldArray(array.castFrom[Array[_], Array[E]])
				case ApplyPreferred(seq) =>
					def foldIndexed :A = {
						var last :A = seq.last
						var i = seq.length - 2
						while (i >= 0)
							f(seq(i), last) match {
								case Some(next) => last = next; i -= 1
								case _ => return last
							}
						last
					}
					val len = seq.length
					if (len == 0)
						unsupported_!("reduceRightSome on an empty " + self.className)
					else
						foldIndexed
				case seq :collection.SeqOps[E, generic.Any, _] =>
					def foldSeq(seq :collection.SeqOps[E, generic.Any, _]) :A = {
						val it      = seq.reverseIterator
						var last :A = it.next()
						while (it.hasNext)
							f(it.next(), last) match {
								case Some(next) => last = next
								case _ => return last
							}
						last
					}
					if (seq.isEmpty)
						unsupported_!("reduceRightSome on an empty " + self.className)
					foldSeq(seq)
				case _ =>
					TemporaryBuffer.from(self).reduceRightSome(f)
			}

		/** Iterates over the collection, passing the index of the current element to the given function.
		  * Note that in collections with an undefined order, this index applies only to this particular iteration,
		  * rather than some absolute position.
		  */
		def foreachWithIndex[U](f :(E, Int) => U) :Unit =
			self.toBasicOps.foldLeft(0) { (i, e) => f(e, i); i + 1 }

		/** Equivalent to `this.slice(from, until).foreach(f)`, but may be more efficient. */
		def foreachInRange[U](from :Int, until :Int)(f :E => U) :Unit = {
			val knownSize = self.knownSize
			self match {
				case sugared :SugaredIterableOps[E, SugaredIterable, SugaredIterable[E]] @unchecked =>
					sugared.foreach(from, until)(f)
				case _ if knownSize == 0 || knownEmpty(self) => ()
				case _ if until <= 0 | until <= from =>
				case _ if from <= 0 && knownSize >= 0 & until >= knownSize =>
					self.toBasicOps.foreach(f)
				case seq :collection.LinearSeq[E] =>
					@tailrec def foreachInList(seq :collection.LinearSeq[E], until :Int) :Unit =
						if (until > 0 && seq.nonEmpty) {
							f(seq.head)
							foreachInList(seq.tail, until - 1)
						}
					foreachInList(seq.drop(from), until)
				case ErasedArray.Wrapped.Slice(array, offset, limit) =>
					def foreachInArray(array :Array[E]) :Unit = {
						var i = offset + math.max(from, 0)
						val end = offset + math.min(until, limit - offset)
						while (i < end) {
							f(array(i))
							i += 1
						}
					}
					foreachInArray(array.asInstanceOf[Array[E]])
				case ApplyPreferred(seq) =>
					def foreachInIndexed() :Unit = {
						val length = seq.length
						var i = math.max(from, 0)
						val end = math.min(until, length)
						while (i < end) {
							f(seq(i))
							i += 1
						}
					}
					foreachInIndexed()
				case _ => //we try to not use slice/take if possible to avoid creating a proxy iterator
					def foreachInIterator() :Unit = {
						if (knownSize < 0 | until < knownSize)
							self.iterator.slice(from, until).foreach(f)
						else if (from <= 0)
							self.toBasicOps.foreach(f)
						else
							self.iterator.drop(from).foreach(f)
					}
					foreachInIterator()
			}
		}

		/** Applies the given function to the elements of this collection for as long as it returns `true`.
		  * Equivalent to `dropWhile(f)`, except it is executes for side effects only, and avoids a potentially
		  * expensive `drop`. Furthermore, it is always executed eagerly, even for lazy collections (including iterators).
		  */
		def foreachWhile(f :E => Boolean) :Unit = self match {
			case _ if knownEmpty(self) =>
			case list :collection.LinearSeq[E] =>
				var seq = list
				while (seq.nonEmpty && f(seq.head))
					seq = seq.tail
			case ErasedArray.Wrapped.Slice(arr, from, until) =>
				val a = arr.asInstanceOf[Array[E]]
				var i = from
				while (i < until && f(a(i)))
					i += 1
			case ApplyPreferred(seq) =>
				var i = 0;
				val length = seq.length
				while (i < length && f(seq(i)))
					i += 1
//			case iter  :Iterator[E] =>
//				//We use dropWhile instead of a manual iteration because iter might narrow down extended IterableOnceOps.
//				self.dropWhile(f)
			case _ =>
				val it = self.iterator
				while (it.hasNext && f(it.next())) {}
		}

		//todo: forSome (a restricted keyword in Scala 2)
//		def forSome

		/** Executes the given function for the first `n` elements of this collection.
		  * Equivalent to `take(n).foreach(f)`, but doesn't create an intermediate collection.
		  */
		def forPrefix[U](n :Int)(f :E => U) :Unit = foreachInRange(0, n)(f :E => U)

		/** Executes the given function for the last `n` elements of this collection.
		  * Equivalent to `takeRight(n).foreach(f)`.
		  */
		def forSuffix[U](n :Int)(f :E => U) :Unit =
			if (n > 0) {
				val size = self.knownSize
				if (size >= 0)
					if (n >= size) self.toBasicOps.foreach(f)
					else foreachInRange(size - n, size)(f)
				else {
					val seq = TemporaryBuffer from self
					val size = seq.length
					if (size <= n) seq.foreach(f)
					else seq.foreachInRange(size - n, size)(f)
				}
			}

		/** Invokes `this.forall`, passing a function applying `f` to the next element and its position
		  * in the iteration order. Equivalent to
		  * {{{
		  *     this.iterator.zipWithIndex.forall { case (e, i) => f(e, i) }
		  * }}}
		  * but doesn't create intermediate tuples.
		  */
		def forallWithIndex(f :(E, Int) => Boolean) :Boolean =
			foldLeftUntil(0)(_ < 0) { (i, e) => if (f(e, i)) i + 1 else -1 } >= 0

		/** Traverses the collection, passing state modified at each element, until the argument function returns `false`
		  * or the whole collection is folded. This is a slightly more efficient and clear equivalent of:
		  * {{{
		  *     toLazyList.scanLeft(start -> true)(f(_._1, _)).takeWhile(_._2).last._2
		  * }}}
		  * @param start the initial state for the folding function.
		  * @param f     a function, applied to each element of the collection and the most recently computed state,
		  *              and returning the new state value as in `foldLeft` paired with the predicate value
		  *              which must hold for all elements.
		  */
		def forallWith[A](start :A)(f :(E, A) => (A, Boolean)) :Boolean =
			knownEmpty(self) || {
				var state = start
				self.toBasicOps.forall { x => val (next, continue) = f(x, state); state = next; continue }
			}

		/** Tests if all elements in the collection satisfy a certain condition, which depends on state updated
		  * when inspecting each element. The argument function is applied first to the initial state `start`
		  * and `this.head`. If it returns `Some(state)`, the condition is understood to hold for the element,
		  * and the returned state is used when testing the next element. If the function returns `None`,
		  * then the test stops. The result is equivalent to
		  * {{{
		  *     foldLeft((start, true)){ case ((state, result), elem) =>
		  *         if (result)
		  *             f(state, element).map((_._1, true)) getOrElse (state, false)
		  *         else
		  *             (state, false)
		  *     }._2
		  * }}}
		  * but does not traverse the whole collection if `result` becomes  `false`.
		  */
		def forallSome[A](start :A)(f :(E, A) => Option[A]) :Boolean =
			knownEmpty(self) || {
				var state = start
				self.toBasicOps.forall { x =>
					f(x, state) match {
						case Some(newState) => state = newState; true
						case _              => false
					}
				}
			}

		/** Verifies if a predicate holds for all consecutive pairs in this collection.
		  * @return `iterator.`[[net.noresttherein.sugar.collections.extensions.IteratorExtension.zipTail zipTail]]`.forall { case (a, b) => f(a, b) }`.
		  */
		def forallConsecutive(f :(E, E) => Boolean) :Boolean = self match {
			case _ if knownEmpty(self) => true
			case seq :collection.LinearSeq[E] =>
				@tailrec def listForall(last :E, rest :collection.LinearSeq[E]) :Boolean =
					rest.isEmpty || {
						val hd = rest.head
						f(last, hd) && listForall(hd, rest.tail)
					}
				seq.isEmpty || listForall(seq.head, seq.tail)
			case ErasedArray.Wrapped.Slice(array, from, until) =>
				@tailrec def arrayForall(a :Array[E], last :E, i :Int) :Boolean =
					i == until || {
						val hd = a(i)
						f(last, hd) && arrayForall(a, hd, i + 1)
					}
				from < until && arrayForall(array.asInstanceOf[Array[E]], array(from).asInstanceOf[E], from + 1)
			case ApplyPreferred(items) =>
				val length = items.length
				@tailrec def indexedForall(last :E, i :Int) :Boolean =
					i == length || {
						val hd = items(i)
						f(last, hd) && indexedForall(hd, i + 1)
					}
				items.nonEmpty && indexedForall(items.head, 1)
			case _ =>
				val i = self.iterator
				!i.hasNext || {
					var last = i.next()
					while (i.hasNext) {
						val old = last
						last = i.next()
						if (!f(old, last))
							return false
					}
					true
				}
		}

		/** Verifies if a predicate holds for all consecutive pairs in this collection.
		  * @return `iterator.`[[net.noresttherein.sugar.collections.extensions.IteratorExtension.zipTail zipTail]]`.forall { case (a, b) => f(a, b) }`.
		  */
		def existsConsecutive(f :(E, E) => Boolean) :Boolean = !forallConsecutive(!f(_, _))


			/** Equivalent to `this.zip(that).foreach { case (a, b) => f(a, b)` }, but does not build an intermediate
		  * collection of tuples and accepts a two argument function rather than a function of tuple,
		  * which makes it more convenient to use with the lambda placeholder syntax.
		  */
		def zipForeach[X, U](that :IterableOnce[X])(f :(E, X) => U) :Unit = {
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
		def zipForall[X](that :IterableOnce[X])(p :(E, X) => Boolean) :Boolean = {
			val l = self.iterator
			val r = that.iterator
			var wasTrue = true
			while (l.hasNext && r.hasNext && { wasTrue = p(l.next(), r.next()); wasTrue })
				{}
			wasTrue
		}

		/** Return `foldLeft(num.zero)(num.plus)`. */
		def sumBy[X](f :E => X)(implicit num: Numeric[X]) :X =
			toBasicOps.foldLeft(num.zero)((sum, e) => num.plus(sum, f(e)))

		/** Return `foldLeft(num.one)(num.times)`. */
		def productBy[X](f :E => X)(implicit num :Numeric[X]) :X =
			toBasicOps.foldLeft(num.one)((product, e) => num.times(product, f(e)))

		/** Equivalent to
		  * [[collection.IterableOnceOps.drop drop]]`(from).`[[collection.IterableOnceOps.copyToArray copyToArray]]`(xs, start, len)`,
		  * but avoids, if possible, potentially expensive `drop`.
		  * @return the number of elements copied.
		  * @throws IndexOutOfBoundsException if `start` is less than zero.
		  */ //todo: untested
		def copyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int = {
			val size = self.knownSize
			self match {
				case sugared :SugaredIterable[A] =>
					sugared.copyRangeToArray(xs, start, from, len)
				case _ if len <= 0 || size >= 0 & from >= size || start >= xs.length =>
					0
				case _ if from <= 0 =>
					toBasicOps.copyToArray(xs, start, len)
				case _ if start < 0 =>
					throw new IndexOutOfBoundsException(
						errorString(self) + ".copyRangeToArray(" +
							errorString(xs) + ", " + start + ", " + from + ", " + len + ")"
					)
				case ErasedArray.Wrapped(array :Array[A @unchecked]) =>
					array.copyRangeToArray(xs, start, from, len)
				case ApplyPreferred(seq) =>
					val from0  = math.max(from, 0)
					val copied = math.min(len, math.min(xs.length - start, seq.size - from0))
					var i = 0
					while (i < copied) {
						xs(start + i) = seq(from0 + i)
						i += 1
					}
					copied
				case it :Iterable[A] if preferDropOverIterator(self) =>
					it.drop(from).copyToArray(xs, start, len)
				case _ =>
					self.iterator.drop(from).copyToArray(xs, start, len)
			}
		}

		/** Equivalent to
		  * [[collection.IterableOnceOps.drop drop]]`(from).`[[collection.IterableOnceOps.copyToArray copyToArray]]`(xs, 0, len)`,
		  * but avoids, if possible, potentially expensive `drop`.
		  * @return the number of elements copied.
		  * @throws IndexOutOfBoundsException if `start` is less than zero.
		  */ //todo: untested
		@inline def copyRangeToArray[A >: E](xs :Array[A], from :Int, len :Int = Int.MaxValue) :Int =
			copyRangeToArray(xs, 0, from, len)
//
//		/** Equivalent to
//		  * [[collection.IterableOnceOps.drop drop]]`(from).`[[collection.IterableOnceOps.copyToArray copyToArray]]`(xs, start, len)`,
//		  * but avoids, if possible, potentially expensive `drop`, and accepts a boxing array.
//		  * @return the number of elements copied.
//		  * @throws IndexOutOfBoundsException if `start` is less than zero.
//		  *///todo: untested
//		@inline def copyRangeToRefArray[A >: E](xs :RefArray[Any], from :Int, start :Int = 0, len :Int = Int.MaxValue) :Int =
//			copyRangeToArray(xs.asAnyArray, from, 0, len)
//
//		/** Same as [[collection.Iterable.copyToArray copyToArray]]`(xs :Array[A], start :Int, len :Int)`,
//		  * but works for boxing arrays.
//		  * @return the number of elements copied.
//		  * @throws IndexOutOfBoundsException if `start` is less than zero.
//		  */
//		@inline def copyToRefArray[A >: E](xs :RefArray[A], start :Int = 0, len :Int = Int.MaxValue) :Int =
//			self.copyToArray(xs.asAnyArray, start, len)

		/** Copies the elements of this $coll to the given array, starting with `from`-th element.
		  * Copying ends when the iterator has no additional elements, or `len` or `xs.length` elements are copied,
		  * whichever is smaller. First element is written at index `start % xs.length`, and if the end of the $coll
		  * is reached before any of the above happens, copying resumes from the beginning of the array.
		  * @return the number of elements copied.
		  * @throws IndexOutOfBoundsException if `start` is less than zero.
		  */
		def cyclicCopyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int = Int.MaxValue) :Int = {
			val size   = self.knownSize
			val length = xs.length
			val start0 = start % length
			val suffixSpace = length - start0
			self match {
				case sugared :SugaredIterable[A] =>
					sugared.cyclicCopyRangeToArray(xs, start, from, len)
				case _ if len <= 0 | size >= 0 & from >= size =>
					0
				case _ if start < 0 =>
					throw new IndexOutOfBoundsException(
						errorString(self) + ".cyclicCopyRangeToArray(" +
							errorString(xs) + ", " + start + ", " + from + ", " + len + ")"
					)
				case _ if from <= 0 && (len <= suffixSpace || size >= 0 && size <= suffixSpace) =>
					toBasicOps.copyToArray(xs, start, len)
				case ErasedArray.Wrapped(array :Array[A @unchecked]) =>
					array.cyclicCopyRangeToArray(xs, start, from, len)
				case ErasedArray.Wrapped.Slice(array :Array[A @unchecked], lo, _) =>
					array.cyclicCopyRangeToArray(xs, start, lo + start + from, math.min(len, size - start - from))
				case ApplyPreferred(seq) =>
					val size   = seq.size
					val from0  = math.min(size, math.max(from, 0))
					val copied = math.min(len, math.min(size - from0, length))
					var end    = start0 + math.min(copied, suffixSpace)
					var i      = from0
					var j      = start0
					while (end > 0) {
						while (j < end) {
							xs(j) = seq(i)
							i += 1
							j += 1
						}
						j = 0
						end = math.min(start0, from0 + copied - i)
					}
					copied
				case it :Iterable[A] if preferDropOverIterator(self) && len <= suffixSpace =>
					it.drop(from).copyToArray(xs, start, len)
				case _ =>
					self.iterator.drop(from).cyclicCopyToArray(xs, start, len)
			}
		}

		/** Copies the elements of this $coll to the given array.
		  * Copying ends when the iterator has no additional elements, or `len` or `xs.length` elements are copied,
		  * whichever is smaller. The first element is written at index `start % xs.length`, and if the end of the $coll
		  * is reached before any of the above happens, copying resumes from the beginning of the array.
		  * @return the number of elements copied.
		  * @throws IndexOutOfBoundsException if `start` is less than zero or greater than `xs.length`.
		  */
		@inline def cyclicCopyToArray[A >: E](xs :Array[A], start :Int, len :Int = Int.MaxValue) :Int =
			cyclicCopyRangeToArray(xs, start, 0, len)

		/** An immutable array with the contents of this collection. */
		def toIArray[A >: E :ClassTag] :IArray[A] = toBasicOps.toArray[A].asInstanceOf[IArray[A]]

		/** Creates an `Array[AnyRef]` with elements of this collection, and passes it as an `RefArray[A]`. */
		def toRefArray[A >: E] :RefArray[A] = toBasicOps.toArray[Any].asInstanceOf[RefArray[A]]

		/** Creates an `Array[AnyRef]` with elements of this collection, and passes it as an `IRefArray[A]`. */
		def toIRefArray[A >: E] :IRefArray[A] = toBasicOps.toArray[Any].asInstanceOf[IRefArray[A]] //type param superfluous, but kept for consistency

	}

	private final class PartialFoldLeftFunction[A, T] extends (((A, T)) => A) {
		var wasCalled = false
		override def apply(v1 :(A, T)) :A = { wasCalled = true; v1._1 }
	}
	private final class PartialFoldRightFunction[A, T] extends (((T, A)) => A) {
		var wasCalled = false
		override def apply(v1 :(T, A)) :A = { wasCalled = true; v1._2 }
	}



	//todo: return C instead of CC everywhere where possible by using util.fromSpecific
	/** Additional extension methods for collections of the standard library framework.
	  * The common theme is performing mapping with help of a passed state/accumulator value.
	  * @define coll collection
	  */
	class IterableExtension[E, CC[X], C] private[collections] (private val self :IterableOps[E, CC, C]) extends AnyVal {
		@inline private def coll :C = self.drop(0)

		/** Same as `this.zip(that)`, but throws a [[NoSuchElementException]] if the collections
		  * are not of the same size. If this collection is strict, or both collections have `knownSize >= 0`,
		  * then the exception will be thrown by this method.
		  * In the other case, it will be thrown when iterating over the result.
		  */
		@throws[NoSuchElementException]("if the collections are not of the same size")
		def zipEven[X](that :IterableOnce[X]) :CC[(E, X)] =
			if (knownEmpty(self) && knownEmpty(that))
				self.iterableFactory.empty
			else
				self.iterableFactory from Iterators.zipEven(self.iterator, that.iterator)

		/** Zips this collection with another one and maps the result in one step.
		  * No intermediate collection is created, and the mapping function accepts two arguments rather than a tuple,
		  * making it more convenient to use with placeholder parameters.
		  */
		def zipMap[X, O](that :IterableOnce[X])(f :(E, X) => O) :CC[O] =
			if (knownEmpty(self))
				self.iterableFactory.empty
			else
				self.iterableFactory from Iterators.zipMap(self.iterator, that.iterator)(f)

		/** Equivalent to [[net.noresttherein.sugar.collections.extensions.IterableExtension.zipMap zipMap]],
		  * but throws a [[NoSuchElementException]] if the collections are not of the same size. If this collection
		  * is strict, or both collections have `knownSize >= 0`, then the exception will be thrown by this method.
		  * In the other case, it will be thrown when iterating over the result.
		  */
		@throws[NoSuchElementException]("if the collections are not of the same size")
		def zipMapEven[X, O](that :IterableOnce[X])(f :(E, X) => O) :CC[O] =
			if (knownEmpty(self) && knownEmpty(that))
				self.iterableFactory.empty
			else
				self.iterableFactory from Iterators.zipMapEven(self.iterator, that.iterator)(f)

		/** Equivalent to `this.zipAll(that, thisElem, thatElem).map(f)`, but happens in one step and the argument function
		  * takes two arguments instead of a pair, which makes it possible to use with lambda placeholder parameters.
		  */
		def zipMapAll[X, O](that :IterableOnce[X], thisElem :E, thatElem :X)(f :(E, X) => O) :CC[O] =
			if (knownEmpty(self) && knownEmpty(that))
				self.iterableFactory.empty
			else
				self.iterableFactory from Iterators.zipMapAll(self.iterator, that.iterator, thisElem, thatElem)(f)

		/** Equivalent to `this.zip(rights).map`, but takes a two argument function instead of a function of a pair,
		  * which makes it possible to use with placeholder lambda parameters.
		  */
		def zipFlatMap[X, O](that :IterableOnce[X])(f :(E, X) => IterableOnce[O]) :CC[O] =
			if (knownEmpty(self) && knownEmpty(that))
				self.iterableFactory.empty
			else
				self.iterableFactory from Iterators.zipFlatMap(self.iterator, that.iterator)(f)

		/** Equivalent to [[net.noresttherein.sugar.collections.extensions.IterableExtension.zipFlatMap zipFlatMap]],
		  * but throws a [[NoSuchElementException]] if the collections are not of the same size. If this collection
		  * is strict, or both collections have `knownSize >= 0`, then the exception will be thrown by this method.
		  * In the other case, it will be thrown when iterating over the result.
		  */
		@throws[NoSuchElementException]("if the collections are not of the same size")
		def zipFlatMapEven[X, O](that :IterableOnce[X])(f :(E, X) => IterableOnce[O]) :CC[O] =
			if (knownEmpty(self) && knownEmpty(that))
				self.iterableFactory.empty
			else
				self.iterableFactory from Iterators.zipFlatMapEven(self.iterator, that.iterator)(f)

		/** Equivalent to `this.zipAll(that, thisElem, thatElem).map(f)`, but happens in one step
		  * and the argument function takes two arguments instead of a pair, which makes it possible to use
		  * with lambda placeholder parameters.
		  */
		def zipFlatMapAll[X, O](that :IterableOnce[X], thisElem :E, thatElem :X)(f :(E, X) => IterableOnce[O]) :CC[O] =
			if (knownEmpty(self) && knownEmpty(that))
				self.iterableFactory.empty
			else
				self.iterableFactory from Iterators.zipFlatMapAll(self.iterator, that.iterator, thisElem, thatElem)(f)


		/** Similar to [[scala.collection.IterableOps.zip zip]], except it zips three collections at once. */
		def zip3[A, B](second :IterableOnce[A], third :IterableOnce[B]) :CC[(E, A, B)] =
			if (knownEmpty(self) && knownEmpty(second) && knownEmpty(third))
				self.iterableFactory.empty
			else
				self.iterableFactory from Iterators.zip3(self.iterator, second.iterator, third.iterator)

		/** Zips three collections, throwing a [[NoSuchElementException]] if they are of different sizes.
		  * If this collection is strict, or all collections have `knownSize >= 0`, then the exception
		  * will be thrown by this method. In the other case, it will be thrown when iterating over the result.
		  */
		@throws[NoSuchElementException]("if the collections are not of the same size")
		def zipEven3[A, B](second :IterableOnce[A], third :IterableOnce[B]) :CC[(E, A, B)] =
			if (knownEmpty(self) && knownEmpty(second) && knownEmpty(third))
				self.iterableFactory.empty
			else
				self.iterableFactory from Iterators.zipEven3(self.iterator, second.iterator, third.iterator)

		/** Similar to [[scala.collection.IterableOps.zipAll zipAll]], but zips three collections at once. */
		def zipAll3[U >: E, A, B](second :IterableOnce[A], third :IterableOnce[B],
		                          thisElem :U, secondElem :A, thirdElem :B) :CC[(U, A, B)] =
			if (knownEmpty(self) && knownEmpty(second) && knownEmpty(third))
				self.iterableFactory.empty
			else
				self.iterableFactory from Iterators.zipAll3(
					self.iterator, second.iterator, third.iterator, thisElem, secondElem, thirdElem
				)

		/** A collection of consecutive elements in this collection. This is similar to `this.sliding(2)`,
		  * but the elements are returned as tuples, and a singleton collection always returns an empty collection.
		  * @return an empty collection of the same type if this collection has fewer than two elements,
		  *         or `this.zip(this.tail)` otherwise (but possibly in a more efficient manner).
		  */
		def zipTail :CC[(E, E)] =
			if (knownEmpty(self)) self.iterableFactory.empty
			else self.iterableFactory from Iterators.zipTail(self.iterator)



		/** Maps this collection from left to right with an accumulating state updated by the mapping function.
		  * The state is discarded after the operation and only the mapping results (the second elements
		  * of the tuples returned by the given function) are returned in a collection of the same dynamic type
		  * as this collection.
		  */
		def mapWith[O, A](z :A)(f :(E, A) => (O, A)) :CC[O] =
			if (knownEmpty(self))
				self.iterableFactory.empty[O]
			else
				self.iterableFactory from Iterators.mapWith(self.iterator, z, f)

		/** Flat maps this collection from left to right with an accumulating state updated by the mapping function.
		  * The state is discarded after the operation and only the mapping results (the collections returned by
		  * by the given function) are returned in a collection of the same dynamic type as this collection.
		  */
		def flatMapWith[A, O](z :A)(f :(E, A) => (IterableOnce[O], A)) :CC[O] =
			if (knownEmpty(self))
				self.iterableFactory.empty[O]
			else
				self.iterableFactory from Iterators.flatMapWith(self.iterator, z, f)

		/** Maps this collection in order consistent with `foreach`, passing as the second argument the index
		  * of the mapped element.
		  */
		def mapWithIndex[O](f :(E, Int) => O) :CC[O] =
			if (knownEmpty(self))
				self.iterableFactory.empty[O]
			else
				self.iterableFactory from Iterators.mapWithIndex(self.iterator, f)

		/** Flat maps this collection in order consistent with `foreach`, passing as the second argument the index
		  * of the mapped element in this collection (that is, the number of elements processed before it).
		  */
		def flatMapWithIndex[O](f :(E, Int) => IterableOnce[O]) :CC[O] =
			if (knownEmpty(self))
				self.iterableFactory.empty[O]
			else
				self.iterableFactory from Iterators.flatMapWithIndex(self.iterator, f)

		/** Similar to [[collection.IterableOnceOps.collect collect]], but the collecting function takes a tuple
		  * consisting of a collection element and its position in the iteration order of this collection.
		  */
		def collectWithIndex[O](f :PartialFunction[(E, Int), O]) :CC[O] =
			if (knownEmpty(self))
				self.iterableFactory.empty
			else
				self.iterableFactory from Iterators.collectWithIndex(self.iterator, f)

		/** Maps this collection from left to right with an accumulating state updated by the mapping function
		  * for as long as the state passes a given predicate. If `!pred(z)`, an empty collection is returned.
		  * Otherwise, the last included element is the one returned by `f` together with the first state not satisfying
		  * the predicate. The state is discarded after the operation and only the mapping results (the second elements
		  * of the tuples returned by the given function) are returned in a collection of the same dynamic type
		  * as this collection.
		  */
		//Consider: the order of parameters to f. On one side, it works as a foldLeft, but on the other like mapWith
		def mapWhile[O, A](z :A)(pred :A => Boolean)(f :(A, E) => (A, O)) :CC[O] =
			if (knownEmpty(self))
				self.iterableFactory.empty
			else
				self.iterableFactory from Iterators.mapWhile(self.iterator, z, pred, f)

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
		  * for as long as the state passes a given predicate. If `!pred(z)`, an empty collection is returned.
		  * Otherwise, the last included elements are those in the collection returned by `f` together
		  * with the first state not satisfying the predicate.
		  * The state is discarded after the operation and only the mapping results (the collections returned by
		  * by the given function) are returned in a collection of the same dynamic type as this collection.
		  */
		def flatMapWhile[O, A](z :A)(pred :A => Boolean)(f :(A, E) => (A, IterableOnce[O])) :CC[O] =
			if (knownEmpty(self))
				self.iterableFactory.empty
			else
				self.iterableFactory from Iterators.flatMapWhile(self.iterator, z, pred, f)

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
		/** Maps this collection from left to right with an accumulating state updated by the mapping function
		  * for as long as the function returns `false` on the first position. If this collection is empty,
		  * an empty collection is returned. Otherwise `f` is applied first to `(z, this.head)`, and  then,
		  * recursively, to the second element of the previously returned triple and the next element of this collection.
		  * The process continues until `f` returns `(true, _, _)`, when the mapped element, and all remaining elements
		  * of this collection, are ignored.
		  * @param z initial state, passed as the first argument when calling `f` for the first time.
		  * @param f an all-in-one function, which takes the state returned when mapping the previous element,
		  *          an element of the collection, and returns, in order: answer to the question if mapping should stop,
		  *          an updated state value, and the value to which the collection element is mapped.
		  * @return  A collection of the same kind, containing the third elements of the triples returned by
		  *          the given function applied to initial elements of this collection and a previously updated state,
		  *          until it returns `false`.
		  */
		def mapUntil[A, O](z :A)(f :(A, E) => (Boolean, A, O)) :CC[O] =
			if (knownEmpty(self))
				self.iterableFactory.empty
			else
				self.iterableFactory from Iterators.mapUntil(self.iterator, z, f)

		/** Flat maps this collection from left to right with an accumulating state updated by the mapping function
		  * for as long as the function returns `false` on the first position. If this collection is empty,
		  * an empty collection is returned. Otherwise, `f` is applied first to `(z, head)`, and then, recursively,
		  * to the second value in the previously returned triple and the next element of this collection.
		  * If at any point the function returns `(true, _, _)`, all remaining elements of this collection
		  * are ignored, and previously returned collections (not including the one returned by mapping the last element)
		  * are concatenated into a collection of the same kind as this one.
		  * @param z initial state, passed as the first argument when calling `f` for the first time.
		  * @param f an all-in-one function, which takes the state returned when mapping the previous element,
		  *          an element of the collection, and returns, in order: answer to the question if mapping should stop,
		  *          an updated state value, and a collection of subsequent elements of the final result.
		  * @return A collection of the same kind, containing, in order, all elements included in the collections
		  *         returned by applying the given function to initial elements of this collection
		  *         and a previously updated state, until the function returns `true` as the first value.
		  */
		def flatMapUntil[A, O](z :A)(f :(A, E) => (Boolean, A, IterableOnce[O])) :CC[O] =
			if (knownEmpty(self))
				self.iterableFactory.empty
			else
				self.iterableFactory from Iterators.flatMapUntil(self.iterator, z, f)

		/** Maps initial elements of this collection, passing updated state between each function application.
		  * If this collection is empty, an empty collection is returned. Otherwise, `f` is applied to `(z, this.head)`,
		  * and then, recursively, to the first element of previously returned pair and the next element
		  * in this collection. Once `f` returns `None`, mapping stops, and second elements of all pairs previously
		  * returned by the mapping function are returned.
		  * @return {{{
		  *         scanLeft(Option((z, null :E))) {
		  *             case (Some(acc, _), elem) => f(acc, elem)
		  *             case _                    => None
		  *         }.tail.takeWhile(_.isDefined).flatMap(_._2)
		  *         }}}
		  */
		def mapSome[A, O](z :A)(f :(A, E) => Option[(A, O)]) :CC[O] =
			if (knownEmpty(self))
				self.iterableFactory.empty
			else
				self.iterableFactory from Iterators.mapSome(self.iterator, z, f)

		/** A collection of the same type, containing the elements of all collections returned by applying
		  * the given function to the elements of this collection, and state updated by the same function
		  * when mapping each element. If this collection is empty, an empty collection is returned.
		  * Otherwise, `f` is applied to `(z, this.head)`, and then recursively to the first element of the returned
		  * pair and the next element in the collection. When `f` returns `None`, the remaining elements
		  * of this collection. are ignored
		  */
		def flatMapSome[A, O](z :A)(f :(A, E) => Option[(A, IterableOnce[O])]) :CC[O] =
			if (knownEmpty(self))
				self.iterableFactory.empty
			else
				self.iterableFactory from Iterators.flatMapSome(self.iterator, z, f)

		/** Maps the elements of the collection and reverses their order. The order in which the mapping function
		  * will be applied to the elements is undefined and depends on the runtime type of this collection.
		  * Note that if this collection is unordered, the order of the elements in the mapped collection
		  * is likewise undefined and depends on the implementation of this collection's builder.
		  * This operation is faster than `this.map(f).reverse`.
		  */
		def mapReverse[O](f :E => O) :CC[O] = self match {
			case _ if knownEmpty(self) =>
				self.iterableFactory.empty
			case list :List[E] =>
				@tailrec def mapList(unmapped :List[E], mapped :List[O]) :List[O] = unmapped match {
					case h::t => mapList(t, f(h)::mapped)
					case _ => mapped
				}
				mapList(list, Nil).asInstanceOf[CC[O]]
			case list :LazyList[E] =>
				if (list.isEmpty)
					list.asInstanceOf[CC[O]]
				else {
					def mapLazy(unmapped :LazyList[E], mapped :LazyList[O]) :LazyList[O] =
						if (unmapped.isEmpty) mapped
						else mapLazy(unmapped.tail, f(unmapped.head) #:: mapped)
					lazy val result = mapLazy(list, LazyList.empty)
					LazyList.cons(result.head, result.tail).asInstanceOf[CC[O]]
				}
			case list :LinearSeq[E] =>
				@tailrec def mapLinear(unmapped :LinearSeq[E], mapped :LinearSeq[O]) :LinearSeq[O] =
					if (unmapped.isEmpty) mapped
					else mapLinear(unmapped.tail, f(unmapped.head) +: mapped)
				mapLinear(list, list.iterableFactory.empty).asInstanceOf[CC[O]]
			case ApplyPreferred(seq) =>
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
			case _ :View[E] =>
				self.iterableFactory.from(new Iterator[O] {
					private[this] var elems :List[O] = _
					override def hasNext = {
						if (elems == null)
							elems = (List.empty[O] /: self) { (acc, e) => f(e)::acc }
						elems ne Nil
					}
					override def next() = {
						if (elems == null)
							elems = (List.empty[O] /: self) { (acc, e) => f(e)::acc }
						val hd = elems.head
						elems = elems.tail
						hd
					}
				})
			case _ =>
				self.iterableFactory.from((List.empty[O] /: self){ (acc, e) => f(e)::acc })
		}


		/** Iterates over the collection from left to right, keeping only those elements for which `pred`
		  * returns `true` as the first pair element, all the while passing to it the latest right element as
		  * the second argument.
		  */
		def filterWith[A](z :A)(pred :(E, A) => (Boolean, A)) :C =
			if (knownEmpty(self))
				self.empty
			else
				util.fromSpecific(self)(Iterators.filterWith(self.iterator, z, pred))

		/** Equivalent to `this.iterator.zipWithIndex.filter(x => pred(x._1, x._2)) to this.iterableFactory`.
		  * For an `IndexedSeq`, prefer `(0 until length).collect { case i if pred(this(i), i) => this(i) }`.
		  */
		def filterWithIndex(pred :(E, Int) => Boolean) :C =
			if (knownEmpty(self))
				self.empty
			else
				util.fromSpecific(self)(Iterators.filterWithIndex(self.iterator, pred))

		/** Iterates over the collection from left to right, splitting elements into those for which `pred`
		  * returns `true` as the first pair element, and those for which it returns `false`,
		  * all the while passing to it the latest right element as the second argument.
		  */
		def partitionWith[A](z :A)(pred :(E, A) => (Boolean, A)) :(C, C) =
			if (knownEmpty(self))
				(self.empty, self.empty)
			else (
				util.fromSpecific(self)(Iterators.filterWith(self.iterator, z, pred)),
				util.fromSpecific(self)(Iterators.filterWith(self.iterator, z, pred, false))
			)

		/** Equivalent to `this.zipWithIndex.partition(x => pred(x._1, x._2))`, but possibly more efficient. */
		def partitionWithIndex(pred :(E, Int) => Boolean) :(C, C) =
			if (knownEmpty(self))
				(self.empty, self.empty)
			else (
				util.fromSpecific(self)(Iterators.filterWithIndex(self.iterator, pred)),
				util.fromSpecific(self)(Iterators.filterWithIndex(self.iterator, pred, false))
			)

		/** Filters elements of this collection based on their position in the iteration order.
		  * For collections with unspecified order, the result may be different for different runs.
		  * @return every element for whose index the predicate returns `true`, as a collection of the same type.
		  */
		def keep(pred :Int => Boolean) :C =
			if (knownEmpty(self) || self.knownSize == 1) coll
			else util.fromSpecific(self)(Iterators.keep(self.iterator, pred))

		/** Removes the duplicates from this collection. The order of the elements in this collection is preserved,
		  * but it is unspecified which instance, out of all duplicates, is returned in the result.
		  */
		def distinct :C =
			if (util.knownUnique(self))
				coll
			else {
				val seen = new mutable.HashSet[E]
				self.filter(elem => seen.add(elem))
			}

		/** Removes the duplicates from this collection, retaining the first occurrence of every element,
		  * and filtering out their any subsequent occurrences.
		  */ //consider: moving these two to SeqExtension
		def firstOccurrences :C =
			if (util.knownUnique(self)) coll
			else util.fromSpecific(self)(Iterators.distinct(self.iterator))

		/** Removes the duplicates from this collection, retaining the last occurrence of every element. */
		def lastOccurrences :C =
			if (util.knownUnique(self))
				coll
			else {
				lazy val result = Iterators.distinct(self.toRefArray.reverseIterator).toRefArray.reverse
				util.fromSpecific(self)(View.fromIteratorProvider(() => result.iterator))
			}

		/** A copy of this collection with the element at the specified index removed.
		  * If the index is  out of range, an [[IndexOutOfBoundsException]] will be thrown, either by this method,
		  * or when traversing the returned collection (if this collection is lazy).
		  *
		  * Note: if you'd prefer the method to silently ignore indices out of range,
		  * you can call `remove(index, index + 1)` instead.
		  */
		@throws[IndexOutOfBoundsException]("if index < 0 or index >= size")
		def removed(index :Int) :C = //:CC[E] =
			if (knownEmpty(self))
				outOfBounds_!(index, 0)
			else if (index < 0 || { val s = self.knownSize; s >= 0 & index >= s })
				outOfBounds_!(index, self.knownSize)
			else if (self.knownLazy)
				util.fromSpecific(self)(Iterators.removed(self.iterator, index))
			else self match {
				case sugared :SugaredIterable[E] =>
					util.fromSpecific(self)(sugared.removed(index))
				case seq :LinearSeq[E] =>
					//we hope that ++: reuses the right operand, and that iterableFactory.from returns the argument
					@tailrec def drop(n :Int, seq :LinearSeq[E]) :LinearSeq[E] =
						if (n <= 0) seq
						else {
							val tail = try seq.tail catch {
								case _ :UnsupportedOperationException => outOfBounds_!(index, index - n)
							}
							drop(n - 1, tail)
						}
					val tail =
						if (index < Int.MaxValue)
							drop(index + 1, seq)
						else {
							val t = seq.drop(Int.MaxValue)
							if (t.isEmpty)
								outOfBounds_!(Int.MaxValue)
							t.tail
						}
					if (tail.isEmpty)
						util.fromSpecific(self)(seq.take(index))
					else if (index == 0)
						util.fromSpecific(self)(seq.tail)
					else
						util.fromSpecific(self)(self.iterator.take(index) ++: tail)
				case _ =>
					util.fromSpecific(self)(Iterators.removed(self.iterator, index))
			}

		/** The reverse of [[scala.collection.IterableOnceOps.slice slice]]: cuts out a segment of this collection
		  * with elements starting with element `from` and ending before `until`.
		  * For indices in range, it is equivalent to `this.take(from) ++ this.drop(until)`, but possibly faster.
		  * Specifying `until <= from` results in returning the same instance (or an equal one,
		  * for most mutable collections). Note that, unlike the single argument `remove(index)`, this method
		  * will not throw an [[IndexOutOfBoundsException]], in line with other slicing methods.
		  * @return `take(from) ++ drop(until)`, but possibly more efficiently.
		  */
		//todo: use util.specificBuilder
		//todo: make the second parameter length instead, as it is inconsistent with buffer
		//The only way to implement this so it returns C is by using filter, and this will be less efficient,
		// because we don't know the target size
		def removed(from :Int, until :Int) :C =// :CC[E] =
			if (until <= 0 | until <= from || knownEmpty(self))
				coll
			else {
				val size = self.knownSize
				val nonNegFrom = math.max(from, 0)
				val nonNegUntil = math.max(until, 0)
				if (size >= 0 && from >= size)
					coll
				else
					self match {
						case sugared :SugaredIterable[E] =>
							util.fromSpecific(self)(sugared.removed(from, until))
						case _ if self.knownLazy =>
							util.fromSpecific(self)(Iterators.removed(self.iterator, nonNegFrom, nonNegUntil))
						case list :List[E] =>
							@tailrec def reversePrefix(seq :List[E], len :Int, acc :List[E]) :List[E] =
								if (len <= 0 || seq.isEmpty) acc
								else reversePrefix(seq.tail, len - 1, seq.head::acc)
							util.fromSpecific(self)(reversePrefix(list, from, Nil) reverse_::: list.drop(until))
						case seq :LinearSeq[E] =>
							val tail = seq.drop(until)
							if (tail.isEmpty)
								self.take(from) //this returns C, as we would prefer
							else  if (from <= 0)
								util.fromSpecific(self)(tail)
							else {
								util.fromSpecific(self)(self.iterator.take(from) ++: tail)
							}
						case _ =>
							util.fromSpecific(self)(Iterators.removed(self.iterator, nonNegFrom, nonNegUntil))
					}
			}

		/** Adds a single element to this collection. If this collection is of one of the standard types which
		  * provide a method for adding individual elements (`Seq`, `Set`, etc.), then that method is invoked.
		  * Otherwise, the implementation defaults to appending a singleton `Iterator` with the element.
		  */
		@inline def +(elem :E) :CC[E] = add(elem)

		/** Adds a single element to this collection. If this collection is of one of the standard types which
		  * provide a method for adding individual elements (`Seq`, `Set`, etc.), then that method is invoked.
		  * Otherwise, the implementation defaults to appending a singleton `Iterator` with the element.
		  */
		def add(elem :E) :CC[E] = self match {
			case seq :collection.SeqOps[E, CC, CC[E]] @unchecked => seq.appended(elem)
			case set :Set[E @unchecked] =>
				try set.incl(elem).castFrom[Set[E], CC[E]] catch {
					case _ :Exception => set concat Iterator.single(elem)
				}
			case rank :Ranking[E] => (rank + elem).castFrom[Ranking[E], CC[E]]
			case _ => self concat Iterator.single(elem)
		}
	}




	/** Extension methods of mutable and immutable sequences (and arrays through a wrapper):
	  *   1. alternative, safer implementations of [[scala.collection.SeqOps.indexOf indexOf]],
	  *      which do not return a negative index when the element is not found;
	  *   1. methods related to subsequences: sequences containing selected elements from another sequence,
	  *      in the same order.
	  * @define coll sequence
	  * @define Coll `Seq`
	  */ //todo: make it rely on SeqLike
	class SeqExtension[E, CC[X], C] private[collections]
	                  (private val self :scala.collection.SeqOps[E, CC, C]) extends AnyVal
	{
		@inline private def length :Int = self.length

		@inline private def genericSelf[U >: E] :CC[U] = self.iterableFactory.from[U](self)

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
					case _ :collection.LinearSeq[E] =>
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
				genericSelf
			else if (self.length <= ReasonableArraySize || ErasedArray.Wrapped.Slice.unapply(self).isDefined) {
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
				self.iterableFactory from ArraySeq.unsafeWrapArray(result)
			} else {
				val result = TemporaryBuffer.from(self)
				var i = result.length
				while (i > 1) {
					val j   = random.nextInt(i)
					val boo = result(j)
					i -= 1
					result(j) = result(i)
					result(i) = boo
				}
				self.iterableFactory from result
			}

		@inline def rotatedLeft(n :Int) :CC[E] = rotatedLeft(0, Int.MaxValue)(n)

		//Cannot return C because we use updated
		def rotatedLeft(from :Int, until :Int)(n :Int) :CC[E] =
			rotatedImpl(from, until)(n) { (length, n) => if (n >= 0) n % length else (n % length + length) % length }

		@inline def rotatedRight(n :Int) :CC[E] = rotatedRight(0, Int.MaxValue)(n)

		def rotatedRight(from :Int, until :Int)(n :Int) :CC[E] =
			rotatedImpl(from, until)(n) { (length, n) => if (n >= 0) (length - n % length) % length else -(n % length) }

		/** Implements circular left shift by `splitAt(length, n)`, where `length` is the actual length
		  * of range `[from, until)` after clipping, and `n` is the third argument of this method.
		  * A right rotation by `n` is equivalent to left rotation by `length - n`. However, if the length
		  * of this sequence is unknown, we can't validate the indices, and be sure about the real length
		  * of the rotated slice. Without the latter, we can't reduce a left shift to right shift if we don't want
		  * to enforce that arguments are all in range. So we apply `splitAt` at a later point,
		  * when the length becomes known.
		  */
		private def rotatedImpl(from :Int, until :Int)(n :Int)(splitAt :(Int, Int) => Int) :CC[E] = {
			val size   = self.knownSize
			val from0  = math.max(from, 0)
			val until0 = math.min(until, size)
			val length = until0 - from0
			if (until + Int.MaxValue <= from0 + 1 + Int.MaxValue | until <= 0 | n == 0 || //lazy to avoid division by zero
				size >= 0 & (from0 >= size - 1 | n % length == 0)
			)
				genericSelf
			else {
				val shift = splitAt(length, n)
				self match {
					case seq :Seq[E] if updatePreferred(seq, length) => //need to be a seq so that updated returns a Seq
						def rotateByUpdates(seq :Seq[E]) :CC[E] = {
							val shiftRight = until0 - from0 - shift
							val split = from0 + shift
							var res   = seq :Seq[E]
							var i     = until0
							while (i > split) {
								i  -= 1
								res = res.updated(i - shift, self(i))
							}
							while (i > from0) {
								i -= 1
								res = res.updated(i + shiftRight, self(i))
							}
							self.iterableFactory.from(res)
						}
						rotateByUpdates(seq)

					case HasFastSlice(seq) =>
						def rotateBySlices = {
							val res = self.iterableFactory.newBuilder[E]
							res sizeHint size
							(res ++= seq.take(from0)
								++= seq.slice(from0 + shift, until0)
								++= seq.slice(from0, from0 + shift)
								++= seq.drop(until0)
							).result()
						}
						rotateBySlices
					case ApplyPreferred(seq) =>
						def rotateDirectRead :CC[E] = {
							val res   = self.iterableFactory.newBuilder[E]
							val split = from0 + shift
							res sizeHint size
							var i = 0
							while (i < from0) {
								res += seq(i)
								i += 1
							}
							i = split
							while (i < until0) {
								res += seq(i)
								i += 1
							}
							i = from0
							while (i < split) {
								res += seq(i)
								i += 1
							}
							i = until0
							while (i < size) {
								res += seq(i)
								i += 1
							}
							res.result()
						}
						rotateDirectRead

					case seq :collection.LinearSeq[E] =>
						def rotateLinearSeq(seq :collection.LinearSeq[E]) :CC[E] = {
							val prefix = TemporaryBuffer.ofCapacity[E](from)
							var suffix :collection.LinearSeq[E] = seq
							var i = 0
							while (i < until && suffix.nonEmpty) {
								prefix += suffix.head
								suffix = suffix.tail
								i += 1
							}
							if (prefix.length <= from0 + 1)
								genericSelf
							else {
								val length = prefix.length - from0
								val shift = splitAt(length, n)
								i = from0 + shift
								while (i > from0) {
									i -= 1
									suffix = prefix(i) +: suffix
								}
								i = from0 + length
								while (i > shift) {
									i -= 1
									suffix = prefix(i) +: suffix
								}
								i = from0
								while (i > 0) {
									i -= 1
									suffix = prefix(i) +: suffix
								}
								self.iterableFactory.from(suffix)
							}
						}
						//If we knew that C <: Seq[E], then this cast would have been unnecessary,
						// because we would simply check if self is a linear seq, but operate solely on self.
						rotateLinearSeq(seq)
					case _ if size >= 0 =>
						def rotateKnownSize :CC[E] = {
							val length = until0 - from0
							val shift  = if (n >= 0) n % length else n % length + length
							val res    = self.iterableFactory.newBuilder[E]
							res sizeHint size
							val (front, rem)     =
								if (from0 == 0) (Iterator.empty, self.iterator)
								else self.iterator.splitAt(from0)
							val (rotated, tail)  =
								if (until0 == size) (rem, Iterator.empty)
								else rem.splitAt(length)
							//unfortunately, NewVectorIterator does not override splitAt, and neither does IndexedSeqView.iterator
							val (slice1, slice2) = rotated.splitAt(length - shift)
							res ++= front ++= slice2 ++= slice1 ++= tail
							res.result()
						}
						rotateKnownSize
					case _ =>
						def rotateWithBuilder :CC[E] = {
							val res = self.iterableFactory.newBuilder[E]
							res sizeHint self.knownSize
							val iter = self.iterator
							var i = 0
							while (i < from0 && iter.hasNext) {
								res += iter.next()
								i += 1
							}
							if (!iter.hasNext)
								genericSelf
							else {
								val buffer = TemporaryBuffer.ofCapacity[E](until - from0)
								while (i < until && iter.hasNext) {
									buffer += iter.next()
									i += 1
								}
								val until0 = i
								val length = i - from0
								val shift  = if (n > 0) n % length else (n % length + length) % length
								val split  = from0 + shift
								i = split
								while (i < until0) {
									res += buffer(i)
									i += 1
								}
								i = from0
								while (i < split) {
									res += buffer(i)
									i  += 1
								}
								res ++= iter
								res.result()
							}
						}
						rotateWithBuilder
				}
			}
		}

		//todo: swapped, maybe replace
//		def swapped(idx1 :Int, idx2 :Int) :C = ???
//		def swapped(idx1 :Int, idx2 :Int, length :Int) :C = ???

		/** Updates the element at `index` and following elements with the specified values.
		  * @return The same result as {{{
		  *         (first +: second +: rest).zip(Iterator.iterate(index)(_ + 1)).foldLeft(this) {
		  *             case (res, (e, i)) => res.updated(i, e)
		  *         }
		  *  }}}
		  */
		//clashes with the standard Seq.updated
/*
		def updated[U >: E](index :Int, first :U, second :U, rest :U*) :CC[U] = {
			def ioob() =
				throw new IndexOutOfBoundsException(
					self.localClassName + "|" + self.size + "|.updated(" + index + ", _, _, (" + rest.size + " more))"
				)
			if (index < 0)
				ioob()
			val size = self.knownSize
			val elemsSize = rest.knownSize
			if (size >= 0 & elemsSize >= 0 & index + elemsSize + 2 < size)
				ioob()

			def updated(f :Substitute[E, U]) :CC[U] = {
				val res = self.map(f)
				if (f.advanced) {
					if (!f.updateInRange)
						ioob()
					res
				} else  //self is Lazy
					self.iterableFactory from new Iterator[U] {
						val i = self.iterator.map(f)

						override def hasNext = {
							val res = i.hasNext
							if (!res && !f.updateInRange)
								ioob()
							res
						}
						override def next() = i.next()
					}
			}
			rest match {
				case _ if size >= 0 =>
					val result = self.iterableFactory.newBuilder[U]
					result sizeHint size
					result ++= self.iterator.take(index) += first += second ++= rest
					result.result()

				case seq :LinearSeq[U] =>
					updated(new Substitute[E, U] {
						private[this] var i   = -1
						private[this] var rem = first +: second +: seq
						override def advanced = i >= 0
						override def updateInRange = rem.isEmpty
						override def apply(v1 :E) = {
							i += 1
							if (i < index) v1
							else if (rem.nonEmpty) {
								val hd = rem.head
								rem = rem.tail
								hd
							} else v1
						}
					})
				case _ =>
					updated(new Substitute[E, U] {
						private[this] var i = -1
						private[this] val patch = rest.iterator
						private[this] final val Index = index
						private[this] final val IndexPlus = index + 1
						override def advanced = i >= 0
						override def updateInRange = i > IndexPlus && !patch.hasNext

						override def apply(v1 :E) :U = i match {
							case n if n < Index => i += 1; v1
							case Index => i += 1; first
							case IndexPlus => i += 1; second
							case _ if patch.hasNext => i += 1; patch.next()
							case _ => i += 1; v1
						}
					})
			}
		}
*/

		/** For indices in range, functionally equivalent to [[collection.SeqOps.patch patch]]`(index, elems, elems.size)`.
		  * It does ''not'' however use `size` method and may be implemented in a different manner, and the index
		  * must be in `0..this.length - elems.length` range, or an [[IndexOutOfBoundsException]] is thrown,
		  * which may make it slightly more efficient than `patch`.
		  */
		/* Consider: should index be permissive in regard to the valid range? in updated it's not; in patch it is.
		 * I don't like the semantics of patch: permissive indices should result in no effect for the indices
		 * out of range, not simply truncating them. We can't however just validate before calling patch if we don't
		 * know the sizes, but we would like to use patch in case it has a more efficient implementation.
		 */
		def updatedAll[U >: E](index :Int, elems :IterableOnce[U]) :CC[U] = {
			def outOfBounds(msg :String = "") =
				throw new IndexOutOfBoundsException(
					errorString(self) + "|.updatedAll(" + index + ", " + errorString(elems) + ")" +
						(if (msg.nonEmpty) ": " + msg else msg)
				)
			val thatSize = elems.knownSize
			val thisSize = self.knownSize
			//If possible, try to add collections, rather than iterators, as there is a chance they'll reuse contents.
			self match {
				case _ if index < 0 || thisSize >= 0 & index > thisSize - math.max(thatSize, 0) =>
					outOfBounds()
				case _ if self.knownLazy =>
					self.iterableFactory from Iterators.updatedAll(self.iterator, index, elems)
				case _ if thatSize == 0 || elems.toBasicOps.isEmpty =>
					//consider: traversing the whole list to compute its length only to throw an exception is wasteful
					val length = self.length
					if (index > length)
						outOfBounds_!(index, length)
					self.iterableFactory from (self :collection.SeqOps[U, CC, C])
				case HasFastSlice(items) =>
					val res = self.iterableFactory.newBuilder[U]
					res sizeHint thisSize
					res ++= items.take(index)
					val toDrop =
						if (thatSize >= 0) {
							res ++= elems
							thatSize
						} else {
							var i  = 0
							val it = elems.iterator
							while (it.hasNext) {
								res += it.next()
								i += 1
							}
							def outOfBounds() =
								throw new IndexOutOfBoundsException(
									errorString(self) + "|.updatedAll(" + index + ", " + elems.className + "|" + i + "|)"
								)
							if (thisSize >= 0) {
								if (index > thisSize - i)
									outOfBounds()
							} else if (i <= Int.MaxValue - index + 1) {
								if (items.drop(index - 1 + i).toBasicOps.nonEmpty)
									outOfBounds()
							} else
								if (self.iterator.drop(index).drop(i - 1).nonEmpty)
									outOfBounds()
							i
						}
					if (index <= thisSize - toDrop)
						res ++= items.drop(index + toDrop)
					else
						res ++= self.iterator.drop(index).drop(toDrop)
					res.result()
				//we hope for fast tail, that hd +: tail reuses tail, and that iterableFactory from seq eq seq
				case seq :LinearSeq[E] @unchecked =>
					var i = 0
					var initReversed :List[U] = Nil
					var tail :LinearSeq[U] = seq
					while (i < index && tail.nonEmpty) {
						initReversed = tail.head::initReversed
						tail = tail.tail
				        i += 1
					}
					if (i < index)
						throw new IndexOutOfBoundsException(
							self.className + "|" + (if (thisSize >= 0) thisSize.toString else index.toString + "+")
								+ "|.updatedAll(" + index + ", " + errorString(elems) + ")"
						)
					else {
						elems match {
							case list :collection.LinearSeq[U] =>
								var patch = list
								var i = 0
								while (patch.nonEmpty && tail.nonEmpty) {
									initReversed = patch.head::initReversed
									patch = patch.tail
									tail = tail.tail
									i += 1
								}
								if (tail.isEmpty && patch.nonEmpty)
									throw new IndexOutOfBoundsException(
										self.className + ".updatedAll(" + index + ", " + elems.className + "|" +
											(if (thatSize >= 0) thatSize else i.toString + "+") + "|): patch too large"
									)
							case IndexedIterable(seq) => //matches only collections of known size
								tail = tail.drop(thatSize - 1)
								if (tail.isEmpty)
									outOfBounds("patch too large")
								tail = tail.tail
								if (applyPreferred(seq)) {
									var i = thatSize
									while (i > 0) {
										i -= 1
										tail = seq(i) +: tail
									}
								} else {
									val i = seq.reverseIterator
									while (i.hasNext)
										tail = i.next() +: tail
								}
							case _ =>
								val i = elems.iterator
								while (i.hasNext && tail.nonEmpty) {
									initReversed = i.next()::initReversed
									tail = tail.tail
								}
						}
						self.iterableFactory from util.prependReverse(initReversed, tail)
					}
				case _ =>
					self.iterableFactory from Iterators.updatedAll(self.iterator, index, elems)
			}
		}

		/** Inserts a new element to this sequence at the specified position, pushing all elements at `index`
		  * and beyond by one position. Equivalent to
		  * [[collection.SeqOps.patch patch]]`(index, Seq(elem), elems.size)`.
		  */ //todo: permissive indexing
		//Consider: use patch, in case it is overridden like in a Finger tree:
		// negative indices are treated as zero, while indices greater than the length
		// of this sequence result in appending the element to the end of the sequence.
		def inserted[U >: E](index :Int, elem :U) :CC[U] = {
			val size = self.knownSize
			if (index < 0 | size >= 0 & index > size)
				throw new IndexOutOfBoundsException(self.className + "|" + size + "|.inserted(" + index + ", _)")
			else if (index == 0)
				self.prepended(elem)
			else if (size >= 0 & index == size)
				self.appended(elem)
			else //todo: SugaredSeq with method inserted
				self.iterableFactory from Iterators.inserted(self.iterator, index, elem)
		}

		//consider: if we renamed it to insertedAll, we could have an updatedAll with the same signature without a conflict
		/** Equivalent to [[net.noresttherein.sugar.collections.extensions.SeqExtension.insertedAll insertedAll]]`(first +: second +: rest)`. */
		def inserted[U >: E](index :Int, first :U, second :U, rest :U*) :CC[U] =
			insertedAll(index, Iterator.two(first, second) :++ rest.iterator)


		/** Equivalent to [[collection.SeqOps.patch patch]]`(index, elems, 0)`. */
		def insertedAll[U >: E](index :Int, elems :IterableOnce[U]) :CC[U] =
			self.iterableFactory from Iterators.insertedAll(self.iterator, index, elems)

		//clashes with standard methods in SeqOps
//		 /** Equivalent to [[net.noresttherein.sugar.collections.SeqExtension.appendedAll appendedAll]]`(first +: second +: rest)`. */
//		def appended[U >: E](first :U, second :U, rest :U*) :CC[U] =
//			rest match {
//				case seq :LinearSeq[U] => self.appendedAll(first +: second +: rest)
//				case _ if knownEmpty(rest) => self.appendedAll(RelayArray.two(first, second))
//				case _ => self appendedAll (Iterator.double(first, second) :++ rest)
//			}
//
//		/** Equivalent to [[net.noresttherein.sugar.collections.SeqExtension.prependedAll prependedAll]]`(first +: second +: rest)`. */
//		def prepended[U >: E](first :U, second :U, rest :U*) :CC[U] =
//			rest match {
//				case seq :LinearSeq[U] => self.prependedAll(first +: second +: rest)
//				case _ if knownEmpty(rest) => self.prependedAll(RelayArray.two(first, second))
//				case _ => self prependedAll (Iterator.double(first, second) ++: rest)
//			}

		/** Finds the location of the given element in this sequence, returning its index as an `Option`.
		  * @param x    the element, whose index is to be determined.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def getIndexOf(x :E, from :Int = 0) :Option[Int] = self.indexOf(x, from) match {
			case -1 => None
			case  n => Some(n)
		}
		/** Finds the last location of the given element in this sequence, returning its index as an `Option`.
		  * @param x   the element, whose index is to be determined.
		  * @param end the upper, inclusive bound on the returned index.
		  */
		@inline def getLastIndexOf(x :E, end :Int = length - 1) :Option[Int] = self.lastIndexOf(x, end) match {
			case -1 => None
			case  n => Some(n)
		}
		/** Finds an element of this sequence which satisfies the predicate, returning its index as an `Option`.
		  * @param p    a function applied consecutively to all elements with indices greater or equal `from`.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def getIndexWhere(p :E => Boolean, from :Int = 0) :Option[Int] = self.indexWhere(p, from) match {
			case -1 => None
			case  n => Some(n)
		}
		/** Finds the last element of this sequence which satisfies the predicate, returning its index as an `Option`.
		  * @param p   a function applied consecutively to all elements with indices lesser or equal `end`,
		  *            in a decreasing order.
		  * @param end the upper, inclusive bound on the returned index; elements after this position will not be checked.
		  */
		@inline def getLastIndexWhere(p :E => Boolean, end :Int = length - 1) :Option[Int] =
			self.lastIndexWhere(p, end) match {
				case -1 => None
				case  n => Some(n)
			}

		/** Finds the location of the given element in this sequence, returning its index as an `IntOpt`.
		  * @param x    the element, whose index is to be determined.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def findIndexOf(x :E, from :Int = 0) :IntOpt = self.indexOf(x, from) match {
			case -1 => NoInt
			case  n => AnInt(n)
		}
		/** Finds the last location of the given element in this sequence, returning its index as an `IntOpt`.
		  * @param x   the element, whose index is to be determined.
		  * @param end the upper, inclusive bound on the returned index.
		  */
		@inline def findLastIndexOf(x :E, end :Int = length - 1) :IntOpt = self.lastIndexOf(x, end) match {
			case -1 => NoInt
			case  n => AnInt(n)
		}
		/** Finds an element of this sequence which satisfies the predicate, returning its index as an `IntOpt`.
		  * @param p    a function applied consecutively to all elements with indices greater or equal `from`.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def findIndexWhere(p :E => Boolean, from :Int = 0) :IntOpt = self.indexWhere(p, from) match {
			case -1 => NoInt
			case  n => AnInt(n)
		}
		/** Finds the last element of this sequence which satisfies the predicate, returning its index as an `IntOpt`.
		  * @param p   a function applied consecutively to all elements with indices lesser or equal `end`,
		  *            in a decreasing order.
		  * @param end the upper, inclusive bound on the returned index; elements after this position will not be checked.
		  */
		@inline def findLastIndexWhere(p :E => Boolean, end :Int = length - 1) :IntOpt =
			self.lastIndexWhere(p, end) match {
				case -1 => NoInt
				case  n => AnInt(n)
			}

		/** Finds the location of the given element in this sequence, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param x    the element, whose index is to be determined.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		def sureIndexOf(x :E, from :Int = 0) :Int = self.indexOf(x, from) match {
			case -1 => indexOfNotFound(errorString(self), x, from)
			case  n => n
		}
		/** Finds the last location of the given element in this sequence, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param x   the element, whose index is to be determined.
		  * @param end the upper, inclusive bound on the returned index.
		  */
		def sureLastIndexOf(x :E, end :Int = length - 1) :Int = self.lastIndexOf(x, end) match {
			case -1 => lastIndexOfNotFound(errorString(self), length, x, end)
			case  n => n
		}
		/** Finds an element of this sequence which satisfies the predicate, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param p    a function applied consecutively to all elements with indices greater or equal `from`,
		  *             until satisfied.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		def sureIndexWhere(p :E => Boolean, from :Int = 0) :Int = self.indexWhere(p, from) match {
			case -1 => indexWhereNotFound(errorString(self), from)
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
				case -1 => lastIndexWhereNotFound(errorString(self), length, end)
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
				case -1 => raise[T](indexOfErrorMessage(errorString(self), x, from))
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
				case -1 => raise[T](lastIndexOfErrorMessage(errorString(self), length, x, end))
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
				case -1 => raise[T](indexWhereErrorMessage(errorString(self), from))
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
				case -1 => raise[T](lastIndexWhereErrorMessage(errorString(self), length, end))
				case  n => n
			}

		/** Returns `this.indexOf(x, from)`, adding an assertion that the result is not negative
		  * (the element has been found).
		  * @param x    an element of this collection.
		  * @param from an inclusive lower bound on the index of the searched element.
		  */
		def assertIndexOf(x :E, from :Int = 0) :Int =
			assertIndexOf(x, from, indexOfErrorMessage(errorString(self), x, from))
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
			assertLastIndexOf(x, end, lastIndexOfErrorMessage(errorString(self), length, x, end))
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
			assertIndexWhere(p, from, indexWhereErrorMessage(errorString(self), from))
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
			assertPresent(self.lastIndexWhere(p, end), lastIndexWhereErrorMessage(errorString(self), length, end))

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
		@inline def getIndexOfSlice[B >: E](that :Seq[B], from :Int = 0) :Option[Int] =
			self.indexOfSlice(that, from) match {
				case -1 => None
				case  n => Some(n)
			}
		/** Finds the last location of the given subsequence in this sequence, returning its index as an `Opt`.
		  * @param that a presumed consecutive subsequence of this sequence.
		  * @param end  the upper, inclusive bound on the returned index.
		  * @return `Opt(this.lastIndexOfSlice(that, end)).filter(_ >= 0)`.
		  */ //Note that Seq(1).lastIndexOf(Nil) == 1, so end must start with length, not length - 1
		@inline def getLastIndexOfSlice[B >: E](that :Seq[B], end :Int = length) :Option[Int] =
			self.lastIndexOfSlice(that, end) match {
				case -1 => None
				case  n => Some(n)
			}

		/** Finds the location of the given subsequence in this sequence, returning its index as an `IntOpt`.
		  * @param that a presumed consecutive subsequence of this sequence.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  * @return `IntOpt(this.indexOfSlice(that, from)).filter(_ >= 0)`.
		  */
		@inline def findIndexOfSlice[B >: E](that :Seq[B], from :Int = 0) :IntOpt =
			self.indexOfSlice(that, from) match {
				case -1 => NoInt
				case  n => AnInt(n)
			}
		/** Finds the last location of the given subsequence in this sequence, returning its index as an `IntOpt`.
		  * @param that a presumed consecutive subsequence of this sequence.
		  * @param end  the upper, inclusive bound on the returned index.
		  * @return `IntOpt(this.lastIndexOfSlice(that, end)).filter(_ >= 0)`.
		  */ //Note that Seq(1).lastIndexOf(Nil) == 1, so end must start with length, not length - 1
		@inline def findLastIndexOfSlice[B >: E](that :Seq[B], end :Int = length) :IntOpt =
			self.lastIndexOfSlice(that, end) match {
				case -1 => NoInt
				case  n => AnInt(n)
			}

		/** Finds the location of the given subsequence in this sequence, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param that a presumed consecutive subsequence of this sequence.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  * @return `this.indexOfSlice(that, from)`.
		  */
		def sureIndexOfSlice[B >: E](that :Seq[B], from :Int = 0) :Int =
			self.indexOfSlice(that, from) match {
				case -1 => indexOfSliceNotFound(errorString(self), that, from)
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
				case -1 => lastIndexOfSliceNotFound(errorString(self), self.length, that, end)
				case  n => n
			}

		/** Returns `this.indexOfSlice(that, from)`, adding an assertion that the result is not negative
		  * (the subsequence has been found).
		  * @param that a presumed consecutive subsequence of this sequence.
		  * @param from an inclusive lower bound on the index of the searched element.
		  */
		def assertIndexOfSlice[B >: E](that :Seq[B], from :Int = 0) :Int =
			assertIndexOfSlice(that, from, indexOfSliceErrorMessage(errorString(self), that, from))

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
			assertLastIndexOfSlice(that, end, lastIndexOfSliceErrorMessage(errorString(self), length, that, end))

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
	class IndexedSeqExtension[E, CC[X], C] private[collections]
	                         (private val self :collection.IndexedSeqOps[E, CC, C])
		extends AnyVal
	{
		/** Takes the longest suffix of this sequence satisfying the predicate. */
		def takeRightWhile(f :E => Boolean) :C = {
			var i = self.length - 1
			if (i <= IndexedIterable.applyPreferredMaxLength(self)) {
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
			if (i <= IndexedIterable.applyPreferredMaxLength(self)) {
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

		/** Performs a binary search of element `x` in a section of this $coll, sorted according
		  * to an implicit `Ordering[E]`. If the $coll is not sorted, or the `Ordering` is not consistent with `equals`,
		  * then the behaviour is unspecified.
		  * The differences from [[collection.IndexedSeqOps.search search]] from the standard library are:
		  *   1. ability to provide bounds within which to search,
		  *   1. returning always the index of the first occurrence of the value in case of duplicates
		  *      (rather than the index of any of them),
		  *   1. returning the value as an `ElementIndex`, which does not box the result,
		  *   1. switching to direct comparisons of built in value types if the `Ordering` is the default one.
		  * @return index of the search key, if it is contained in the $coll,
		  *         as `ElementIndex`.[[net.noresttherein.sugar.collections.ElementIndex.Present Present]].
		  *         Otherwise, the ''insertion point'',
		  *         as `ElementIndex.`[[net.noresttherein.sugar.collections.ElementIndex.Absent Absent]].
		  *         The `insertion point` is defined as the point at which the key would be inserted into the $coll:
		  *         the index of the first element in the array greater than the key, or `until`,
		  *         if all elements in the array are less than the specified key.
		  */
		@inline final def binarySearch[U >: E :Ordering](x :U) :ElementIndex = binarySearch(0, self.length, x)

		/** Performs a binary search of element `x` in a section of this $coll, sorted
		  * according to an implicit `Ordering[U]`. Returns the index of the first occurrence of `x`, if present
		  * in the given range, or an index `i`: `from <= i <= until`, such that
		  * `i == until || this(i) > x && (i == from || this(i) < x)`. If `until <= from`,
		  * then [[net.noresttherein.sugar.collections.ElementIndex.Absent Absent]]`(from)` is returned immediately.
		  * The differences from [[collection.IndexedSeqOps.search search]] from the standard library are:
		  *   1. ability to provide bounds within which to search,
		  *   1. returning always the index of the first occurrence of the value in case of duplicates,
		  *      (rather than the index of any of them),
		  *   1. returning the value as an `ElementIndex`, which does not box the result,
		  *   1. switching to direct comparisons of built in value types if the `Ordering` is the default one.
		  * @param from  the lower bound (inclusive) on the searched index range. A negative index is equivalent to `0`,
		  *              while if `from > this.length` the effect is the same as if `from == this.length`.
		  * @param until the upper bound (exclusive) on the searched index range. A negative index is equivalent to `0`,
		  *              while if `until > this.length` the effect is the same as if `until == this.length`.
		  *              Values lesser than `from` are treated the same way as `until == from`.
		  * @return the index of the search key, if it is present in the searched range,
		  *         as `ElementIndex`.[[net.noresttherein.sugar.collections.ElementIndex.Present Present]].
		  *         Otherwise, the ''insertion point'',
		  *         as `ElementIndex.`[[net.noresttherein.sugar.collections.ElementIndex.Absent Absent]].
		  *         The `insertion point` is defined as the point at which the key would be inserted into the $coll:
		  *         the index of the first element in the range greater than the key, or `until`,
		  *         if all elements in the range are less than the specified key.
		  */ //binarySearch(from :Int, until :Int)(x :U) would be more elegant, but Scala 2 infers too early U =:= E
		final def binarySearch[U >: E](from :Int, until :Int, x :U)(implicit ordering :Ordering[U]) :ElementIndex = {
			val limit = math.min(self.length, until)
			var start = math.max(from, 0)
			var end   = until
			while (start < end) {
				val middle = (start + end) >> 1
				if (ordering.lteq(x, self(middle))) end = middle
				else start = middle + 1
			}
			if (start < limit && ordering.compare(x, self(start)) == 0) Present(start)
			else Absent(start)
		}
	}


	/** Extension methods for `mutable.`[[collection.mutable.IndexedSeq IndexedSeq]].
	  * @define coll indexed sequence
	  */
	class mutableIndexedSeqExtension[E] private[collections] (private val self :mutable.IndexedSeq[E]) extends AnyVal {
		/** Swaps (in place) elements at indices `i` and `j`. */
		@inline def swap(i :Int, j :Int) :Unit = {
			val boo = self(i)
			self(i) = self(j)
			self(j) = boo
		}

		/** Fills (in place) the whole sequence with the given value. */
		@inline final def fill(elem :E) :Unit = fill(0, self.length)(elem)

		/** Sets (in place) all values in this sequence within the given index range to the specified value.
		  * Indices out of `[0, this.length)` are permitted, and the method will not attempt to set them.
		  */ //consider: this is similar to update, probably should not be permissive, as mutable methods usually are not.
		def fill(from :Int, until :Int)(elem :E) :Unit =
			if (from < until & until > 0 && from < self.length) {
				val until0 = math.min(until, self.length)
				var i      = math.max(from, 0)
				while (i < until0) {
					self(i) = elem
					i += 1
				}
			}

		/** Updates all elements in this $coll to values returned by the function applied to elements' indices. */
		@inline def updateAll(f :Int => E) :Unit = updateAll(0, self.length)(f)

		/** Updates all elements in the specified index range in this $coll to values returned by the function
		  * applied to elements' indices.
		  */
		def updateAll(from :Int, until :Int)(f :Int => E) :Unit = {
			var i = from
			while (i < until) {
				self(i) = f(i)
				i += 1
			}
		}

		/** Sets the values at indices `index, index + 1, index + 2, ...` to `first, second, elems.head`
		  * and subsequent elements of `rest`. If any of the indices in the range covering all provided elements
		  * is out of range, it is simply ignored. For example,
		  * {{{
		  *     > ArraySeq("You", "Boo", "I").updateAll(-1, "Imoen", "CHARNAME", "Miniature Giant Space Hamster")
		  *     > ArraySeq("CHARNAME", "Miniature Giant Space Hamster", "I")
		  * }}}
		  * @return the number of updated elements.
		  */
		def updateAll(index :Int, first :E, second :E, rest :E*) :Int = {
			self(index) = first
			self(index  + 1) = second
			updateAll(index + 2, rest) + 2
		}

		/** Sets the values at indices `index, index + 1, ...` to subsequent elements of `elems`.
		  * If any of the indices in the range covering all provided elements
		  * is out of range, it is simply ignored. For example,
		  * {{{
		  *     > ArraySeq("You", "Boo", "I").updateAll(-1, Seq("Imoen", "CHARNAME", "Miniature Giant Space Hamster"))
		  *     > ArraySeq("CHARNAME", "Miniature Giant Space Hamster", "I")
		  * }}}
		  * @return the number of updated elements.
		  */
		def updateAll(index :Int, elems :IterableOnce[E]) :Int = {
			val thisSize = self.length
			val thatSize = elems.knownSize
			if (index < 0 | index > thisSize | thatSize >= 0 & index > thisSize - thatSize)
				throw new IndexOutOfBoundsException(
					errorString(self) + ".updateAll(" + index + ", " + errorString(elems) + ")"
				)
			val res =
				if (thatSize < 0) -1
				else if (thatSize == 0) 0
				else (self match {
					case ErasedArray.Wrapped.Slice(array, from, _) =>
						elems.toBasicOps.copyToArray(array.asInstanceOf[Array[Any]], from + index)
					case _ => -1
				})
			if (res >= 0)
				res
			else
				elems.toBasicOps.foldLeft(index) { (i, elem) =>
					self(i) = elem; i + 1
				} - index
		}

		/** Sets the values at indices `index, index + 1, ...` to subsequent elements of `elems`.
		  * If any of the indices in the range covering all provided elements
		  * is out of range, it is simply ignored. For example,
		  * {{{
		  *     > ArraySeq("You", "Boo", "I").updateAll(-1, IArray("Imoen", "CHARNAME", "Miniature Giant Space Hamster"))
		  *     > ArraySeq("CHARNAME", "Miniature Giant Space Hamster", "I")
		  * }}}
		  * @return the number of updated elements.
		  */
		def updateAll(index :Int, elems :ArrayLike[E]) :Int = {
			val thisSize = self.length
			val thatSize = elems.length
			if (index < 0 | index > thisSize - thatSize)
				throw new IndexOutOfBoundsException(
					errorString(self) + ".updateAll(" + index + ", " + errorString(elems) + ")"
				)
			self match {
				case ErasedArray.Wrapped.Slice(array, from, _) =>
					ArrayLike.copy(elems, 0, array, from + index, thatSize)
				case _ =>
					var i = 0
					while (i < thatSize) {
						self(index + i) = elems(i)
						i += 1
					}
			}
			thatSize
		}

		/** Reverses in place the order of the elements in the whole sequence. */
		@inline def reverseInPlace() :Unit = reverseInPlace(0, self.length)

		/** Reverses in place the section of this sequence between indices `from` (inclusive) and `until` (exclusive).
		  * If `until <= from`, or `until <= 0`, or `from >= length` the call has no effect. Passing a negative `from`,
		  * or `until > length` has the same effect as passing `0` and `length`, respectively.
		  */
		def reverseInPlace(from :Int, until :Int) :Unit =
			if (until > 0 & from < until && from < self.length) {
				var i = math.max(from, 0)
				var j = math.min(until, self.length)
				while (i < j) {
					j -= 1
					val boo = self(i)
					self(i) = self(j)
					self(j) = boo
					i += 1
				}
		}

		//todo: variants of `IndexedSeqOps` existing sorting methods for ranges, which won't be seen as overloads.
//		@inline final def sortInPlace[A >: E]()(implicit ordering :Ordering[A]) :Unit = Sorting.stableSort(self)
//
//		@inline final def sortInPlace[A >: E](from :Int, until :Int)(implicit ordering :Ordering[A]) :Unit =
//			Sorting.stableSort(self, from, until)
//
//		@inline final def sortInPlaceWith[A >: E](lt :(A, A) => Boolean) :Unit = Sorting.stableSort(self, lt)
//
//		@inline final def sortInPlaceWith[A >: E](from :Int, until :Int)(lt :(A, A) => Boolean) :Unit =
//			Sorting.stableSort(self, lt, from, until)
//
//		@inline final def sortInPlaceBy[A](f :E => A)(implicit ordering :Ordering[A]) :Unit =
//			Sorting.stableSort(self, (a :E, b :E) => ordering.lt(f(a), f(b)))
//
//		@inline final def sortInPlaceBy[A](from :Int, until :Int)(f :E => A)(implicit ordering :Ordering[A]) :Unit =
//			Sorting.stableSort(self, (a :E, b :E) => ordering.lt(f(a), f(b)), from, until)

		/** Shifts in place all the elements in the sequence down by `n` positions, modulo the length of the sequence.
		  * Element at position `n` is moved to index `0`, element at position `n + 1` to index `1`, etc.
		  * Element at `0` is moved to index `n`, element at `1` to index `n + 1`, etc.
		  */
		@inline def rotateLeft(n :Int) :Unit = rotateLeft(0, self.length)(n)

		/** Shifts in place the elements in the sequence between indices `from` (inclusive) and `until` (exclusive)
		  * down by `n` positions, modulo the length of the index range.
		  * Element at position `from + n` is moved to index `from`, element at position `from + n + 1`
		  * to index `from + 1`, etc. Element at `from` is moved to index `from + n`, element at `from + 1`
		  * to index `from + n + 1`, etc. If `until <= from`, or `until <= 0`, or `from >= length`,
		  * the call has no effect. Passing negative `from` is the same as passing zero, passing `until > length`
		  * clips it the length of the sequence.
		  */
		def rotateLeft(from :Int, until :Int)(n :Int) :Unit = {
			val fullLength = self.length
			val from0 = math.max(from, 0)
			val until0 = math.min(until, fullLength)
			val length = until0 - from0
			if (until > 0 & until0 > from0) {
				var pivot = n % length
				if (pivot < 0)
					pivot = length + pivot
				pivot match { //try to use fast platform arraycopy if the shift is very short
					case 0 =>
					case 1 =>
						val saved = self(from0)
						arraycopy(self, from0 + 1, self, from0, length - 1)
						self(until0 - 1) = saved
					case 2 =>
						val saved0 = self(from0)
						val saved1 = self(from0 + 1)
						arraycopy(self, from0 + 2, self, from0, length - 2)
						self(until0 - 1) = saved1
						self(until0 - 2) = saved0
					case _ if pivot == length - 1 =>
						val saved = self(until0 - 1)
						arraycopy(self, from0, self, from0 + 1, length - 1)
						self(from0) = saved
					case _ if pivot == length - 2 =>
						val saved1 = self(until0 - 1)
						val saved0 = self(until0 - 2)
						arraycopy(self, from0, self, from0 + 2, length - 2)
						self(from0) = saved0
						self(from0 + 1) = saved1
					case _ =>
						reverseInPlace(from0, from0 + pivot)
						reverseInPlace(from0 + pivot, until0)
						reverseInPlace(from0, until0)
				}
			}
		}

		/** Shifts in place all the elements in the sequence down by `n` positions, modulo the length of the sequence.
		  * Element at position `n` is moved to index `0`, element at position `n + 1` to index `1`, etc.
		  * Element at `0` is moved to index `n`, element at `1` to index `n + 1`, etc.
		  */
		@inline def rotateRight(n :Int) :Unit = rotateRight(0, self.length)(n)

		/** Shifts in place the elements in the sequence between indices `from` (inclusive) and `until` (exclusive)
		  * down by `n` positions, modulo the length of the index range.
		  * Element at position `from + n` is moved to index `from`, element at position `from + n + 1`
		  * to index `from + 1`, etc. Element at `from` is moved to index `from + n`, element at `from + 1`
		  * to index `from + n + 1`, etc. If `until <= from`, or `until <= 0`, or `from >= length`,
		  * the call has no effect. Passing negative `from` is the same as passing zero, passing `until > length`
		  * clips it the length of the sequence.
		  */
		def rotateRight(from :Int, until :Int)(n :Int) :Unit = {
			val length = self.length
			val start = math.min(length, math.max(from, 0))
			val end   = math.min(length, math.max(from, until))
			if (start < end)
				if (n < 0)
					rotateLeft(from, until)(start - end - n) //guards against underflow on -(n == Int.MinValue)
				else
					rotateLeft(from, until)(end - start - n)
		}
	}


	/** Extension methods for `mutable.`[[collection.mutable.Buffer Buffer]].
	  * @define coll buffer
	  */
	class BufferExtension[E, This <: Buffer[E]] private[collections] (private val self :This) extends AnyVal {
		/** Removes the last element from the buffer, returning its value.
		  * @return [[collection.mutable.Buffer.remove remove]]`(this.length - 1)`.
		  */
		@inline def removeLast()  :E = self.remove(self.length - 1)

		/** Removes the first element from the buffer, returning its value.
		  * @return [[collection.mutable.Buffer.remove remove]]`(0)`.
		  */
		@inline def removeFirst() :E = self.remove(0)


		/** Fills (in place) the whole buffer with the given value. */
		@inline final def fill(elem :E) :Unit = fill(0, self.length)(elem)

		/** Sets (in place) all values in this buffer within the given index range to the specified value.
		  * Indices out of `[0, this.length)` are permitted, and the method will not attempt to set them.
		  *///consider: this is similar to update, probably should not be permissive, as mutable methods usually are not.
		def fill(from :Int, until :Int)(elem :E) :Unit =
			if (from < until & until > 0 && from < self.length) {
				val from0  = math.max(from, 0)
				val until0 = math.min(until, self.length)
				self.patchInPlace(from, IndexedSeq.const(until0 - from0)(elem), until0 - from0)
			}

		/** Updates all elements in this $coll to values returned by the function applied to elements' indices. */
		@inline def updateAll(f :Int => E) :Unit = updateAll(0, self.length)(f)

		/** Updates all elements in the specified index range in this $coll to values returned by the function
		  * applied to elements' indices.
		  */
		def updateAll(from :Int, until :Int)(f :Int => E) :Unit = {
			var i = from
			while (i < until) {
				self(i) = f(i)
				i += 1
			}
		}

		/** Sets the values at indices `index, index + 1, index + 2, ...` to `first, second, elems.head`
		  * and subsequent elements of `rest`. If any of the indices in the range covering all provided elements
		  * is out of range, it is simply ignored. For example,
		  * {{{
		  *     > ListBuffer("You", "Boo", "I").updateAll(-1, "Imoen", "CHARNAME", "Miniature Giant Space Hamster")
		  *     > ListBuffer("CHARNAME", "Miniature Giant Space Hamster", "I")
		  * }}}
		  * @return the number of updated elements.
		  */
		def updateAll(index :Int, first :E, second :E, rest :E*) :Int = {
			self(index) = first
			self(index  + 1) = second
			updateAll(index + 2, rest) + 2
		}

		/** Sets the values at indices `index, index + 1, ...` to subsequent elements of `elems`.
		  * If any of the indices in the range covering all provided elements
		  * is out of range, it is simply ignored. For example,
		  * {{{
		  *     > ListBuffer("You", "Boo", "I").updateAll(-1, Seq("Imoen", "CHARNAME", "Miniature Giant Space Hamster"))
		  *     > ListBuffer("CHARNAME", "Miniature Giant Space Hamster", "I")
		  * }}}
		  * @return the number of updated elements.
		  */
		def updateAll(index :Int, elems :IterableOnce[E]) :Int = {
			val patch = elems.toBasicOps to Iterable
			val thisSize = self.length
			val thatSize = patch.size
			if (index < 0 | index > thisSize | index > thisSize - thatSize)
				throw new IndexOutOfBoundsException(
					errorString(self) + ".updateAll(" + index + ", " + errorString(elems) + ")"
				)
			self.patchInPlace(index, patch, thatSize)
			thatSize
		}

		/** Sets the values at indices `index, index + 1, ...` to subsequent elements of `elems`.
		  * If any of the indices in the range covering all provided elements
		  * is out of range, it is simply ignored. For example,
		  * {{{
		  *     > ListBuffer("You", "Boo", "I").updateAll(-1, IArray("Imoen", "CHARNAME", "Miniature Giant Space Hamster"))
		  *     > ListBuffer("CHARNAME", "Miniature Giant Space Hamster", "I")
		  * }}}
		  * @return the number of updated elements.
		  */
		def updateAll(index :Int, elems :ArrayLike[E]) :Int = {
			val thisSize = self.length
			val thatSize = elems.length
			if (index < 0 | index > thisSize - thatSize)
				throw new IndexOutOfBoundsException(
					errorString(self) + ".updateAll(" + index + ", " + errorString(elems) + ")"
				)
			self.patch(index, ArrayLike.Wrapped(elems), thatSize)
			thatSize
		}

		/** Reverses in place the order of the elements in the whole buffer. */
		@inline def reverseInPlace() :Unit = reverseInPlace(0, self.length)

		/** Reverses in place the section of this buffer between indices `from` (inclusive) and `until` (exclusive).
		  * If `until <= from`, or `until <= 0`, or `from >= length` the call has no effect. Passing a negative `from`,
		  * or `until > length` has the same effect as passing `0` and `length`, respectively.
		  */
		def reverseInPlace(from :Int, until :Int) :Unit =
			if (until > 0 & from < until && from < self.length) {
				val patch = ReversedSeq(self.view.slice(from, until) to ArraySeq.untagged)
				self.patch(from, patch, patch.length)
			}

		/** Shifts in place all the elements in the buffer down by `n` positions, modulo the length of the buffer.
		  * Element at position `n` is moved to index `0`, element at position `n + 1` to index `1`, etc.
		  * Element at `0` is moved to index `n`, element at `1` to index `n + 1`, etc.
		  */
		@inline def rotateLeft(n :Int) :Unit = rotateLeft(0, self.length)(n)

		/** Shifts in place the elements in the buffer between indices `from` (inclusive) and `until` (exclusive)
		  * down by `n` positions, modulo the length of the index range.
		  * Element at position `from + n` is moved to index `from`, element at position `from + n + 1`
		  * to index `from + 1`, etc. Element at `from` is moved to index `from + n`, element at `from + 1`
		  * to index `from + n + 1`, etc. If `until <= from`, or `until <= 0`, or `from >= length`,
		  * the call has no effect. Passing negative `from` is the same as passing zero, passing `until > length`
		  * clips it the length of the buffer.
		  */
		def rotateLeft(from :Int, until :Int)(n :Int) :Unit = {
			val fullLength = self.length
			val from0 = math.max(from, 0)
			val until0 = math.min(until, fullLength)
			val length = until0 - from0
			if (until > 0 & until0 > from0) {
				var pivot = n % length
				if (pivot < 0)
					pivot = length + pivot
				if (pivot <= (until0 - from0 >> 1)) {
					val slice = self.view.slice(from0, from0 + pivot).toList
					self.remove(from0, from0 + pivot)
					self.insertAll(from0 + (until0 - from0 - pivot), slice)
				} else {
					val slice = self.view.slice(from0 + pivot, until).toList
					self.remove(from0 + pivot, until0)
					self.insertAll(from0, slice)
				}
			}
		}

		/** Shifts in place all the elements in the buffer down by `n` positions, modulo the length of the buffer.
		  * Element at position `n` is moved to index `0`, element at position `n + 1` to index `1`, etc.
		  * Element at `0` is moved to index `n`, element at `1` to index `n + 1`, etc.
		  */
		@inline def rotateRight(n :Int) :Unit = rotateRight(0, self.length)(n)

		/** Shifts in place the elements in the buffer between indices `from` (inclusive) and `until` (exclusive)
		  * down by `n` positions, modulo the length of the index range.
		  * Element at position `from + n` is moved to index `from`, element at position `from + n + 1`
		  * to index `from + 1`, etc. Element at `from` is moved to index `from + n`, element at `from + 1`
		  * to index `from + n + 1`, etc. If `until <= from`, or `until <= 0`, or `from >= length`,
		  * the call has no effect. Passing negative `from` is the same as passing zero, passing `until > length`
		  * clips it the length of the buffer.
		  */
		def rotateRight(from :Int, until :Int)(n :Int) :Unit = {
			val length = self.length
			val start = math.min(length, math.max(from, 0))
			val end   = math.min(length, math.max(from, until))
			if (start < end)
				if (n < 0)
					rotateLeft(from, until)(start - end - n) //guards against underflow on -(n == Int.MinValue)
				else
					rotateLeft(from, until)(end - start - n)
		}
	}


	/** Adds an [[net.noresttherein.sugar.collections.extensions.immutableMapExtension.updatedIfAbsent updatedIfAbsent]]
	  *  extension method to [[scala.collection.immutable.Map immutable.Map]].
	  */
	class immutableMapExtension[K, V, M[A, +B] <: MapOps[A, B, M, M[A, B]]] private[extensions](private val self :M[K, V])
		extends AnyVal
	{
		/** Equivalent to [[collection.MapOps.applyOrElse applyOrElse]]
		  * (and similar to [[collection.MapOps.getOrElse getOrElse]]), but does not need to create an object
		  * if `default` is a pure function.
		  * @note if the map contains a `null` value for the key, it is treated as if the key was not present,
		  *       and `default(key)` is returned.
		  */
		@inline def getOrElseApply[U >: V](key :K, default :K => U) :U = {
			val maybeNull = self.getOrElse(key, null)
			if (maybeNull != null) maybeNull.asInstanceOf[U]
			else default(key)
		}

		/** Adds the key to the map, if this map does not contain it.
		  * @return `if (contains(key)) this else updated(key, value)`.
		  */
		@inline def updatedIfAbsent[U >: V](key :K, value :U) :M[K, U] =
			if (self.contains(key)) self else self.updated(key, value)

		/** Same as [[net.noresttherein.sugar.collections.extensions.immutableMapExtension.updatedIfAbsent updatedIfAbsent]]. */
		@inline def ?=[U >: V](entry :(K, U)) :M[K, U] = updatedIfAbsent(entry._1, entry._2)

		def map2[O](f :(K, V) => O) :Iterable[O] = self.keysIterator.map { key => f(key, self(key)) }.toSeq

		def map2[O, K2, V2](f :(K, V) => O)(implicit pair :O => (K2, V2)) :Map[K2, V2] =
			self.keysIterator.map { key => pair(f(key, self(key))) }.toMap
	}



	/** Adds the same extension methods to `Iterator`
	  * as [[net.noresttherein.sugar.collections.extensions.IterableExtension IterableExtension]].
	  */ //todo: move it up in the file after IterableOnceExtension
	class IteratorExtension[E] private[collections](private val self :Iterator[E]) extends AnyVal {
		@inline def nextOpt() :Opt[E] = if (self.hasNext) Got(self.next()) else Lack

		/** Same as `this.zip(that)`, but throws a [[NoSuchElementException]] if the collections
		  * are not of the same size. If exactly one of the iterators is empty, then the exception will be thrown
		  * by this method. Otherwise, it will be thrown when one of the iterators becomes empty
		  * when iterating over the result.
		  */
		def zipEven[X](that :Iterator[X]) :Iterator[(E, X)] =
			if (!self.hasNext && !that.hasNext) Iterator.empty
			else Iterators.zipEven(self, that)

		/** Zips this iterator with another one and maps the result in one step. */
		def zipMap[X, O](that :Iterator[X])(f :(E, X) => O) :Iterator[O] =
			if (!self.hasNext || !that.hasNext) Iterator.empty
			else Iterators.zipMap(self, that)(f)

		/** Equivalent to [[net.noresttherein.sugar.collections.extensions.IteratorExtension.zipMap zipMap]],
		  * but throws an exception if the iterators are of different sizes.
		  * If exactly one iterator is empty, an `IllegalArgumentException` will be thrown by this method.
		  * Otherwise, a `NoSuchElementException` will be thrown when one of the iterators becomes empty
		  * while iterating over the result.
		  */
		def zipMapEven[X, O](that :Iterator[X])(f :(E, X) => O) :Iterator[O] =
			if (!self.hasNext && !that.hasNext) Iterator.empty
			else Iterators.zipMapEven(self, that)(f)

		/** Equivalent to `this.zipAll(that, thisElem, thatElem).map(f)`, but happens in one step and the argument function
		  * takes two arguments instead of a pair, which makes it possible to use with lambda placeholder parameters.
		  */
		def zipMapAll[X, O](that :Iterator[X], thisElem :E, thatElem :X)(f :(E, X) => O) :Iterator[O] =
			if (!self.hasNext && !that.hasNext) Iterator.empty
			else Iterators.zipMapAll(self, that, thisElem, thatElem)(f)

		/** Equivalent to `this.zip(rights).map`, but takes a two argument function instead of a function of a pair,
		  * which makes it possible to use with placeholder lambda parameters.
		  */
		def zipFlatMap[X, O](that :Iterator[X])(f :(E, X) => IterableOnce[O]) :Iterator[O] =
			if (!self.hasNext || !that.hasNext) Iterator.empty
			else Iterators.zipFlatMap(self, that)(f)

		/** Equivalent to [[net.noresttherein.sugar.collections.extensions.IteratorExtension.zipFlatMap zipFlatMap]],
		  * but throws an exception if the iterators are of different sizes.
		  * If exactly one of the iterators is empty, an `IllegalArgumentException` will be thrown by this method.
		  * Otherwise, a `NoSuchElementException` will be thrown when one of the iterators becomes empty
		  * while iterating over the result.
		  */
		def zipFlatMapEven[X, O](that :Iterator[X])(f :(E, X) => IterableOnce[O]) :Iterator[O] =
			if (!self.hasNext && !that.hasNext) Iterator.empty
			else Iterators.zipFlatMapEven(self, that)(f)

		/** Equivalent to `this.zipAll(that, thisElem, thatElem).map(f)`, but happens in one step
		  * and the argument function takes two arguments instead of a pair, which makes it possible to use
		  * with lambda placeholder parameters.
		  */
		def zipFlatMapAll[X, O](that :Iterator[X], thisElem :E, thatElem :X)(f :(E, X) => IterableOnce[O]) :Iterator[O] =
			if (!self.hasNext && !that.hasNext) Iterator.empty
			else Iterators.zipFlatMapAll(self, that, thisElem, thatElem)(f)


		/** Similar to [[scala.collection.Iterator.zip zip]], except it zips three iterators at once. */
		def zip3[A, B](second :Iterator[A], third :Iterator[B]) :Iterator[(E, A, B)] =
			if (!self.hasNext && !second.hasNext && !third.hasNext) Iterator.empty
			else Iterators.zip3(self, second, third)

		/** Zips three iterators, throwing an exception if they are of different sizes.
		  * If one or two iterators are empty, an `ArgumentException` will be thrown by this method.
		  * Otherwise, a `NoSuchElementException` will be thrown when one of the iterators becomes empty
		  * while iterating over the result.
		  */
		def zipEven3[A, B](second :Iterator[A], third :Iterator[B]) :Iterator[(E, A, B)] =
			if (!self.hasNext && !second.hasNext && !third.hasNext) Iterator.empty
			else Iterators.zipEven3(self, second, third)

		/** Similar to [[scala.collection.Iterator.zipAll zipAll]], but zips three iterators at once. */
		def zipAll3[U >: E, A, B](second :Iterator[A], third :Iterator[B],
		                          thisElem :U, secondElem :A, thirdElem :B) :Iterator[(U, A, B)] =
			if (!self.hasNext && !second.hasNext && !third.hasNext)
				Iterator.empty
			else
				Iterators.zipAll3(self, second, third, thisElem, secondElem, thirdElem)

		/** Iterates over consecutive element pairs of this iterator.
		  * If this iterator has no more than a single element, an empty iterator will be returned.
		  * Otherwise it is equivalent to:
		  * {{{
		  *     val list = this to LazyList
		  *     list.zip(list.tail).iterator
		  * }}}
		  */
		def zipTail :Iterator[(E, E)] =
			if (!self.hasNext) Iterator.empty
			else Iterators.zipTail(self)


		/** Maps this iterator with an accumulating state, updated by the mapping function.
		  * The state is discarded after the operation and only the mapping results (the second elements
		  * of the tuples returned by the given function) are returned.
		  */
		def mapWith[O, A](z :A)(f :(E, A) => (O, A)) :Iterator[O] =
			if (!self.hasNext) Iterator.empty[O]
			else Iterators.mapWith(self, z, f)

		/** Flat maps this iterator from left to right with an accumulating state updated by the mapping function.
		  * The state is discarded after the operation and only the mapping results (the collections returned by
		  * by the given function) are returned.
		  */
		def flatMapWith[A, O](z :A)(f :(E, A) => (IterableOnce[O], A)) :Iterator[O] =
			if (!self.hasNext) Iterator.empty[O]
			else Iterators.flatMapWith(self, z, f)

		/** Maps this iterator, passing as the second argument the index of the mapped element. */
		def mapWithIndex[O](f :(E, Int) => O) :Iterator[O] =
			if (!self.hasNext) Iterator.empty[O]
			else Iterators.mapWithIndex(self, f)

		/** Flat maps this iterator, passing as the second argument the index
		  * of the mapped element (that is, the number of elements processed before it).
		  */
		def flatMapWithIndex[O](f :(E, Int) => IterableOnce[O]) :Iterator[O] =
			if (!self.hasNext) Iterator.empty[O]
			else Iterators.flatMapWithIndex(self, f)

		/** Similar to [[collection.IterableOnceOps.collect collect]], but the collecting function takes a tuple
		  * consisting of an element of the iterator and its index.
		  */
		def collectWithIndex[O](f :PartialFunction[(E, Int), O]) :Iterator[O] =
			if (!self.hasNext) Iterator.empty[O]
			else Iterators.collectWithIndex(self, f)

		/** Maps this iterator with an accumulating state, updated by the mapping function,
		  * for as long as the state passes the given predicate. If `!pred(z)`, an empty iterator is returned.
		  * Otherwise, the last returned element is the one returned by `f` together with the first state not satisfying
		  * the predicate. The state is discarded after the operation and only the mapping results (the second elements
		  * of the tuples returned by the given function) are returned.
		  *///Consider: the order of parameters to f. On one side, it works as a foldLeft, but on the other like mapWith
		def mapWhile[O, A](z :A)(pred :A => Boolean)(f :(A, E) => (A, O)) :Iterator[O] =
			if (!self.hasNext) Iterator.empty
			else Iterators.mapWhile(self, z, pred, f)

		//commented out until Scala 3
/*
		def mapWhile[O, A](z :A)(pred :(A, E) => Boolean)(f :(A, E) => (A, O)) :Iterator[O] =
			if (!self.hasNext)
				Iterator.empty[O]
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

		/** Flat maps this iterator with an accumulating state, updated by the mapping function,
		  * for as long as the state passes a given predicate. If `!pred(z)`, an empty collection is returned.
		  * Otherwise, the last included elements are those in the collection returned by `f` together
		  * with the first state not satisfying the predicate. The state is discarded after the operation
		  * and only the mapping results (the collections returned by by the given function) are returned.
		  */
		def flatMapWhile[O, A](z :A)(pred :A => Boolean)(f :(A, E) => (A, IterableOnce[O])) :Iterator[O] =
			if (!self.hasNext) Iterator.empty
			else Iterators.flatMapWhile(self, z, pred, f)

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
		/** Maps this iterator with an accumulating state, updated by the mapping function,
		  * for as long as the function returns `false` on the first position. Once the function returns `true`,
		  * the associated mapped element is discarded, and the iteration ends. The last element of the returned
		  * iterator is the last element `x` for which `f` returned `(false, _, x)`.
		  * @param z initial state, passed as the first argument when calling `f` for the first time.
		  * @param f an all-in-one function, which takes the state returned when mapping the previous element,
		  *          an element of the collection, and returns, in order: answer to the question if mapping should stop,
		  *          an updated state value, and the value to which the collection element is mapped.
		  * @return  An iterator returning the third elements of the triples returned by
		  *          the given function applied to initial elements of this iterator, and a previously updated state,
		  *          for which the function returned `false`.
		  */
		def mapUntil[A, O](z :A)(f :(A, E) => (Boolean, A, O)) :Iterator[O] =
			if (!self.hasNext) Iterator.empty
			else Iterators.mapUntil(self, z, f)

		/** Flat maps this iterator with an accumulating state, updated by the mapping function,
		  * for as long as the function returns `false` on the first position. Once the function returns `true`,
		  * the associated collection is ignored, and the iteration ends.
		  * @param z initial state, passed as the first argument when calling `f` for the first time.
		  * @param f an all-in-one function, which takes the state returned when mapping the previous element,
		  *          an element of the collection, and returns, in order: answer to the question if mapping should stop,
		  *          an updated state value, and a collection of subsequent elements of the final result.
		  * @return An iterator returning, in order, all elements included in the collections
		  *         returned by the given function, for as long as the function returns until it returns `false`.
		  */
		def flatMapUntil[A, O](z :A)(f :(A, E) => (Boolean, A, IterableOnce[O])) :Iterator[O] =
			if (!self.hasNext) Iterator.empty
			else Iterators.flatMapUntil(self, z, f)

		/** Maps initial elements of this iterator, passing updated state between each function application.
		  * Mapping stops once `f` returns `None`.
		  * @return {{{
		  *         scanLeft(Option((z, null :E))) {
		  *             case (Some(acc, _), elem) => f(acc, elem)
		  *             case _                    => None
		  *         }.drop(1).takeWhile(_.isDefined).flatMap(_._2)
		  *         }}}
		  */
		def mapSome[A, O](z :A)(f :(A, E) => Option[(A, O)]) :Iterator[O] =
			if (!self.hasNext) Iterator.empty
			else Iterators.mapSome(self, z, f)

		/** An iterator returning all elements of collections returned by applying the given function
		  * to the elements of this iterator and state updated by the same function when mapping each element.
		  * Once the function returns `None`, the iteration stops.
		  */
		def flatMapSome[A, O](z :A)(f :(A, E) => Option[(A, IterableOnce[O])]) :Iterator[O] =
			if (!self.hasNext) Iterator.empty
			else Iterators.flatMapSome(self, z, f)


		/** Filters this iterator, keeping only those elements for which `pred`
		  * returns `true` as the first pair element, all the while passing to it the latest right element as
		  * the second argument.
		  */
		def filterWith[A](z :A)(pred :(E, A) => (Boolean, A)) :Iterator[E] =
			if (!self.hasNext) self
			else Iterators.filterWith(self, z, pred)

		/** Equivalent to `this.zipWithIndex.filter(x => pred(x._1, x._2))`. */
		def filterWithIndex(pred :(E, Int) => Boolean) :Iterator[E] =
			if (!self.hasNext) self
			else Iterators.filterWithIndex(self, pred)

		/** Splits the iterated elements into those for which `pred` returns `true` as the first pair element,
		  * and those for which it returns `false`, all the while passing to it the latest right element
		  * as the second argument.
		  */
		def partitionWith[A](z :A)(pred :(E, A) => (Boolean, A)) :(Iterator[E], Iterator[E]) =
			if (!self.hasNext)
				(self, self)
			else {
				val (i1, i2) = self.duplicate
				(Iterators.filterWith(i1, z, pred), Iterators.filterWith(i2, z, pred, false))
			}

		/** Equivalent to `this.zipWithIndex.partition(x => pred(x._1, x._2))`, but possibly more efficient. */
		def partitionWithIndex(pred :(E, Int) => Boolean) :(Iterator[E], Iterator[E]) =
			if (!self.hasNext)
				(self, self)
			else {
				val (i1, i2) = self.duplicate
				(Iterators.filterWithIndex(i1, pred), Iterators.filterWithIndex(i2, pred, false))
			}

		/** An iterator removing all elements for whose positions the predicate returns `false`. */
		def keep(pred :Int => Boolean) :Iterator[E] = Iterators.keep(self, pred)

		/** An iterator which removes all duplicates in this iterator after the first occurrence of every unique element. */
		def distinct :Iterator[E] = Iterators.distinct(self)


		/** A copy of this iterator omitting the element at the specified index. The iterator is validated:
		  * if the index is out of range for the number of the elements in the iterator,
		  * an [[IndexOutOfBoundsException]] will be thrown. If the fact can be verified immediately,
		  * the exception will be thrown by this method. Otherwise, it will be thrown once the iterator reaches
		  * its last element (without reaching `index`). If, in the meantime, the iterator is sliced
		  * (or its [[Iterator.take take]] method is called), the resulting iterator will not perform the validation,
		  * unless `index` is included in the sliced range. Methods which leave the iterator in an undefined state,
		  * such as `copyToArray`, may - or may not - enforce index validation if the iterator has advanced to it.
		  */
		@throws[IndexOutOfBoundsException]("if index is negative, or this.knownSize is non negative and not greater than index.")
		def removed(index :Int) :Iterator[E] =
			if (index < 0 || { val size = self.knownSize; size >= 0 & index >= size })
				throw new IndexOutOfBoundsException(index)
			else if (index == 0)
				if (self.hasNext) self.drop(1)
				else throw new IndexOutOfBoundsException("0 out of 0")
			else
				Iterators.removed(self, index)

		/** The reverse of [[scala.collection.Iterator.slice slice]]: cuts out a segment of this iterator
		  * with elements starting with element `from` and ending before `until`.
		  * For indices in range, it is equivalent to `val (i1, i2) = this.duplicate; i1.take(from) ++ i2.drop(until)`,
		  * but possibly faster. Specifying `until <= from` results in returning the same iterator.
		  * @return `take(from) ++ drop(until)`, but possibly more efficiently.
		  */
		//Consider: Buffer.remove validates arguments. Should we also do it for consistency?
		//Consider: taking count instead of until as an argument, the same as in Buffer.remove
		def removed(from :Int, until :Int) :Iterator[E] =
			if (until <= 0 | until <= from || !self.hasNext)
				self
			else {
				val size = self.knownSize
				val nonNegFrom = math.max(from, 0)
				val nonNegUntil = math.max(until, 0)
				if (size >= 0 && from >= size)
					self
				else
					Iterators.removed(self, nonNegFrom, nonNegUntil)
			}

		/** An iterator which substitutes `index`-th element in this iterator with `elem`.
		  * If `index` is less than zero, or greater or equal to the number of iterated elements,
		  * an [[IndexOutOfBoundsException]] will be thrown; if `index` is negative, or this iterator has known size,
		  * it will be thrown by this method. Otherwise, the exception will be thrown when the returned iterator
		  * exhausts this iterator's elements, without reaching `index`. This validation does not happen if
		  * the number of elements in the iterator is later explicitly limited by `take`/`slice`.
		  * Methods which leave the iterator in an undefined state, such as `copyToArray`,
		  * may - or may not - enforce index validation if the iterator has advanced to it.
		  */
		def updated[U >: E](index :Int, elem :U) :Iterator[U] =
			if (index < 0 || { val size = self.knownSize; size >= 0 & index >= size })
				throw new IndexOutOfBoundsException(self.toString + ".updated(" + index + ", _)")
			else
				Iterators.updated(self, index, elem)

		/** An iterator which substitutes `index`-th and following elements in this iterator with values returned
		  * by `elems`. If `index` is less than zero, or greater or equal to the number of iterated elements minus
		  * the number of elements in the patch iterator, an [[IndexOutOfBoundsException]] will be thrown.
		  * If `index` is negative, or exceeds this iterator's size, if it is known, or `index > this.size - elems.size`
		  * and the sizes of both iterators are known, it will be thrown by this method.
		  * Otherwise, the exception will be thrown when the returned iterator exhausts this iterator's elements,
		  * without reaching `index` and/or exhausting `elems` iterator. If, however, before that the iterator
		  * is instead sliced, taking fewer than `index` elements (relative to the current position),
		  * this validation will not trigger. Methods which leave the iterator in an undefined state,
		  * such as `copyToArray`, may - or may not - enforce index validation if the iterator has advanced to it.
		  * @return An iterator with the same elements (but with additional validation described above) as
		  *         {{{
		  *             this.zipWithIndex.map {
		  *                 case (_, i) if i >= index && elems.hasNext => elems.next()
		  *                 case (e, _) => e
		  *             }
		  *        }}}
		  */ //todo: implementing validation prevents efficient slice. We should overhaul all iterators to not validate.
		def updatedAll[U >: E](index :Int, elems :IterableOnce[U]) :Iterator[U] =
			Iterators.updatedAll(self, index, elems)

		/** An iterator which returns the given value between the `index`-th and `index + 1`-th elements
		  * of this iterator. If `index` is less than zero, or greater than the number of iterated elements,
		  * an [[IndexOutOfBoundsException]] will be thrown. If `index` is negative,
		  * or `index > this.knownSize && this.knownSize >= 0`, then the exception will be thrown by this method.
		  * Otherwise, it will be thrown when the iterator reaches its last element, unless its number of elements
		  * is later explicitly limited by `take`/`slice`.
		  * Methods which leave the iterator in an undefined state, such as `copyToArray`,
		  * may - or may not - enforce index validation if the iterator has advanced to it.
		  * @return An iterator equivalent to `this.take(index) ++ Iterator.single(elem) ++ this`
		  *         (note that `this` appended as last will iterate over elements past `index`, as the preceding
		  *         ones are consumed by `this.take(index)`).
		  */ //todo: permissive indexing
		def inserted[U >: E](index :Int, elem :U) :Iterator[U] =
			Iterators.inserted(self, index, elem)

		/** An iterator which returns elements of `elems` between the elements at positions `index` and `index + 1`
		  * in this iterator. If `index` is less than zero, or greater than the number of iterated elements,
		  * an [[IndexOutOfBoundsException]] will be thrown. If `index` is negative,
		  * or `index > this.knownSize && this.knownSize >= 0`, then the exception will be thrown by this method.
		  * Otherwise, it will be thrown when the iterator reaches its last element, unless its number of elements
		  * is later explicitly limited by `take`/`slice`.
		  * Methods which leave the iterator in an undefined state, such as `copyToArray`,
		  * may - or may not - enforce index validation if the iterator has advanced to it.
		  * @return An iterator equivalent to `this.take(index) ++ elems ++ this`
		  *         (note that `this` appended as last will iterate over elements past `index`, as the preceding
		  *         ones are consumed by `this.take(index)`).
		  */
		def insertedAll[U >: E](index :Int, elems :IterableOnce[U]) :Iterator[U] =
			Iterators.insertedAll(self, index, elems)

		/** An iterator which returns `elem` after all the elements of this iterator.
		  * It is equivalent to `this ++ Iterator.single(elem)`, but more efficient due to the argument not being lazy.
		  * @return `this.`[[net.noresttherein.sugar.collections.extensions.IteratorExtension.appended appended]]`(elem)`.
		  */
		@inline def +[U >: E](elem :U) :Iterator[U] = appended(elem)

		/** An iterator which returns `elem` after all the elements of this iterator.
		  * It is equivalent to `this ++ Iterator.single(elem)`, but more efficient due to the argument not being lazy.
		  * @return `this.`[[net.noresttherein.sugar.collections.extensions.IteratorExtension.appended appended]]`(elem)`.
		  */
		@inline def add[U >: E](elem :U) :Iterator[U] = appended(elem)

		/** An iterator which returns `elem` after all the elements of this iterator.
		  * It is equivalent to `this ++ Iterator.single(elem)`, but more efficient due to the argument not being lazy.
		  * @return `this.`[[net.noresttherein.sugar.collections.extensions.IteratorExtension.appended appended]]`(elem)`.
		  */
		@inline def :+[U >: E](elem :U) :Iterator[U] = appended(elem)

		/** An iterator which returns `elem` after all the elements of this iterator.
		  * It is equivalent to `this ++ Iterator.single(elem)`, but more efficient due to the argument not being lazy.
		  */
		def appended[U >: E](elem :U) :Iterator[U] = Iterators.appended(self, elem)

		/** Equivalent to `this ++ Iterator.single(first) ++ Iterator.single(second) ++ rest`.
		  * Unlike the standard `Iterator.`[[collection.Iterator.concat concat]], the arguments are not lazy,
		  * making it a slightly more efficient alternative.
		  */
		def appended[U >: E](first :U, second :U, rest :U*) :Iterator[U] =
			if (knownEmpty(rest))
				Iterators.concat(self, Iterator.two(first, second))
			else
				Iterators.concat(self, Iterators.concat(Iterator.two(first, second), rest.iterator))

		/** Equivalent to `this ++ elems`, but the argument is not lazy,
		  * making the implementation slightly more efficient.
		  */
		@inline def :++[U >: E](elems :IterableOnce[U]) :Iterator[U] = appendedAll(elems)

		/** Equivalent to `this ++ elems`, but the argument is not lazy,
		  * making the implementation slightly more efficient.
		  */
		def appendedAll[U >: E](elems :IterableOnce[U]) :Iterator[U] =
			if (knownEmpty(elems)) self
			else Iterators.concat(self, elems.iterator)

		/** An iterator which returns `elem` as its first element, followed by all elements of this iterator. */
		@inline def +:[U >: E](elem :U) :Iterator[U] = prepended(elem)

		/** An iterator which returns `elem` as its first element, followed by all elements of this iterator. */
		def prepended[U >: E](elem :U) :Iterator[U] =
			if (self.hasNext) Iterators.prepended(self, elem)
			else Iterator.single(elem)

		/** Equivalent to `Iterator.single(first) ++ Iterator.single(second) ++ rest ++ this`.
		  * Unlike the standard `Iterator.`[[collection.Iterator.concat concat]], the arguments are not lazy,
		  * making it a slightly more efficient alternative.
		  */
		def prepended[U >: E](first :U, second :U, rest :U*) :Iterator[U] =
			if (knownEmpty(rest))
				Iterators.concat(Iterator.two(first, second), self)
			else
				Iterators.concat(Iterators.concat(Iterator.two(first, second), rest.iterator), self)

		/** Equivalent to `Iterator.single(first) ++ Iterator.single(second) ++ rest ++ this`.
		  * Unlike the standard `Iterator.`[[collection.Iterator.concat concat]], the arguments are not lazy,
		  * making it a slightly more efficient alternative.
		  */
		@inline def ++:[U >: E](elems :IterableOnce[U]) :Iterator[U] = prependedAll(elems)

		/** Equivalent to `elems.iterator ++ this`. Unlike the standard `Iterator.`[[collection.Iterator.concat concat]],
		  *  the arguments are not lazy, making it a slightly more efficient alternative.
		  */
		def prependedAll[U >: E](elems :IterableOnce[U]) :Iterator[U] =
			if (knownEmpty(elems)) self
			else Iterators.concat(elems.iterator, self)

		/** An iterator with safe slicing methods. Invoking `take`, `drop`, `slice` does not invalidate this validator;
		  * instead, iterators returned by those methods share the same underlying state,
		  * including a counter of already returned elements.
		  * Calling `take` on this iterator returns a new iterator, which will not return elements past a certain index.
		  * This iterator remains unaffected by the call itself, or `take` called on the latter iterator,
		  * but advancing the latter - via `next` or `drop` - automatically also advances this iterator
		  * by the same number of elements, and vice versa.
		  *
		  * Likewise, `copyToArray` is guaranteed to advance this iterator - and all created by it -
		  * exactly by the number of written elements, as returned by the method.
		  * Note that `splitAt(n)` is different from `val drop = safe; (drop.take(n), drop)`
		  * in that the iterators in the former case are independent, while in the latter case advancing one
		  * advancing another, and `drop.next()` returns `this.next()`, not `this.drop(n).next()`.
		  * @example
		  * {{{
		  *     val iter   = source.iterator.safe
		  *     val arrays = Array.ofDim[Int](n, m)
		  *     var i = 0
		  *     while (iter.hasNext && i < n) {
		  *         iter.take(m).copyToArray(arrays(i))
		  *         i += 1
		  *     }
		  * }}}
		  */
		def safe :Iterator[E] = Iterators.slicer(self)

		/** An iterator maintaining a counter of elements advanced over, accessible through method
		  * [[net.noresttherein.sugar.collections.CountingIterator.total total]].
		  */
		@inline def counting :CountingIterator[E] = new CountingIterator(self)
//
//		/** Same as [[collection.Iterator.copyToArray copyToArray]], but accepts a boxing array. */
//		@inline def copyToRefArray[A >: E](xs :RefArray[A], start :Int = 0, len :Int = Int.MaxValue) :Int =
//			self.copyToArray(xs.asInstanceOf[Array[Any]], start, len)

		/** Copies the elements of this iterator to the given array, starting at position `start`.
		  * Copying ends when the iterator has no additional elements, or `len` or `xs.length` elements are copied,
		  * whichever is smaller. If the end of the array is reached before any of the above happens,
		  * copying resumes from the beginning of the array.
		  */ //todo: integrate this method into IterableOnceExtension.cyclicCopyToArray
		def cyclicCopyToArray[A >: E](xs :Array[A], start :Int, len :Int = Int.MaxValue) :Int = {
			val size = self.knownSize
			val length = xs.length
			val suffixSpace = length - start
			if (size == 0 | len <= 0 | length == 0)
				0
			else if (start < 0)
				outOfBounds_!(start, xs.length)
			else if (len <= suffixSpace | size >= 0 & size <= suffixSpace)
				self.copyToArray(xs, start, len)
			else if (self.isInstanceOf[ArrayIterator[_]] || self.isInstanceOf[CyclicArrayIterator[_]]) {
				//consider: a more generic test, which will include at least MatrixBufferIterator
				self.copyToArray(xs, start, len) + self.copyToArray(xs, 0, len - suffixSpace)
			} else if (size >= 0) {
				val (suffix, prefix) = self.splitAt(suffixSpace)
				val copied = math.min(math.min(len, size), length)
				suffix.copyToArray(xs, start, suffixSpace)
				prefix.copyToArray(xs, 0, copied - suffixSpace)
				copied
			} else {
				var i = start
				var end = if (len <= suffixSpace) start + len else length
				var copied = -start
				while (copied < len && self.hasNext) {
					while (i < end && self.hasNext) {
						xs(i) = self.next()
						i += 1
					}
					copied += i
					i   = 0
					end = math.min(start, len - suffixSpace)
				}
				copied
			}
		}
	}


	/** Extension methods for `String`. */
	class StringExtension private[collections] (private val self :String) /*extends AnyVal */{
		@inline def stringIterator :StringIterator = StringIterator(self)

		/** If the length of this string is lesser than `length`, prepend `this.length - length` copies of `char`
		  * to this string.
		  */
		def lpad(length :Int, char :Char = ' ') :String =
			if (self.length >= length) self
			else ((new JStringBuilder(length) /: (length - self.length))(_ append char) append self).toString

		/** If the length of this string is lesser than `length`, append `this.length - length` copies of `char`
		  * to this string.
		  */
		def rpad(length :Int, char :Char = ' ') :String =
			if (self.length >= length) self
			else ((new JStringBuilder(length) append self) /: (length - self.length))(_ append char).toString

		def segmentLength(p :Char => Boolean, from :Int = 0) :Int = {
			val end   = self.length
			val start = math.max(from, 0)
			var i = start
			while (i < end && p(self.charAt(i)))
				i += 1
			i - start
		}

		def chopped :ChoppedString = ChoppedString(self)

		def javaIterator :JavaIntIterator = new StringStepper(self)
		def javaIterator(from :Int, until :Int) :JavaIntIterator = new StringStepper(self, from, until)
		def jterator :CharJterator = new StringStepper(self).asInstanceOf[CharJterator]
		def jterator(from :Int, until :Int) :CharJterator =
			new StringStepper(self, from, until).asInstanceOf[CharJterator]
	}

	sealed trait StringExtensionConversion extends (String => StringExtension) {
		@inline final def apply(self :String)(implicit dummy :DummyImplicit) :StringExtension =
			new StringExtension(self)
	}
	private val StringExtensionConversionPrototype =
		new PriorityConversion.Wrapped[String, StringExtension](new StringExtension(_))
			with StringExtensionConversion


	/** A light wrapper over [[scala.collection.StepperShape StepperShape]] evidence, inferring the type of elements
	  * based on the type of the stepper. It is introduced because implicit `StepperShape` values resolve well
	  * when the element type `A` is given, and the stepper type `S` needs to be provided, but not vice versa.
	  * The companion object defines implicit values providing:
	  *   - `[Int, IntStepper]`
	  *   - `[Long, LongStepper]`
	  *   - `[Double, DoubleStepper]`
	  *   - `[T, AnyStepper[T]]`.
	  *   - `[T, S]` from an implicit `StepperShape[A, S]` (in generic contexts, where the latter is a context argument),
	  * including their specializations `with EfficientSplit`,
	  */
	@implicitNotFound("Cannot determine the element type ${A} of ${S}: is the latter a Stepper?")
	@SerialVersionUID(Ver)
	class StepType[A, S <: Stepper[_]] private[extensions](val stepperShape :StepperShape[A, S])
		extends AnyVal
	{
		def shape :StepperShape.Shape = stepperShape.shape
	}

	private[extensions] sealed abstract class Rank1PreferredStepperShapes {
		@inline implicit def withEfficientSplit[A, U <: Stepper[_], S <: Stepper[_]]
		                                       (implicit shape :StepperShape[A, U], split :S =:= U with EfficientSplit)
		       :StepType[A, S] =
			new StepType[A, S](shape.asInstanceOf[StepperShape[A, S]])

		@inline implicit def compatibleStepType[A, S <: Stepper[_]]
		                                       (implicit shape :StepperShape[A, S]) :StepType[A, S] =
			new StepType[A, S](shape)
	}
	@SerialVersionUID(Ver)
	object StepType extends Rank1PreferredStepperShapes {
		@inline implicit def anyStep[T, S <: AnyStepper[T]](implicit ev :S <:< AnyStepper[T]) :StepType[T, S] =
			new StepType[T, S](StepperShape.anyStepperShape.asInstanceOf[StepperShape[T, S]])

		@inline implicit def intStep[S <: IntStepper] :StepType[Int, S] =
			new StepType[Int, S](StepperShape.intStepperShape.asInstanceOf[StepperShape[Int, S]])

		@inline implicit def longStep[S <: LongStepper] :StepType[Long, S] =
			new StepType[Long, S](StepperShape.longStepperShape.asInstanceOf[StepperShape[Long, S]])

		@inline implicit def doubleStep[S <: DoubleStepper] :StepType[Double, S] =
			new StepType[Double, S](StepperShape.doubleStepperShape.asInstanceOf[StepperShape[Double, S]])
	}

	/** Adds a `++` extension method to any `Stepper[_]`. */
	class StepperExtension[E, S <: Stepper[_]] private[extensions] (private val self :S) extends AnyVal {
		/** The type of the elements returned by this `Stepper`.
		  * @return a `StepType` for one of `IntShape`, `LongShape`, `DoubleShape` and `ReferenceShape`.
		  */
		def stepType :StepType[E, S] = self match {
			case _ :IntStepper    => StepType.intStep.asInstanceOf[StepType[E, S]]
			case _ :LongStepper   => StepType.longStep.asInstanceOf[StepType[E, S]]
			case _ :DoubleStepper => StepType.doubleStep.asInstanceOf[StepType[E, S]]
			case _                => StepType.anyStep[E, AnyStepper[E]].asInstanceOf[StepType[E, S]]
		}

		/** Combines this stepper with the argument, returning a stepper of the same specialization as both.
		  * This will work only for the built in Scala steppers. This is equivalent to
		  * `this `[[net.noresttherein.sugar.collections.extensions.StepperExtension.++ ++]]` next`,
		  * except the argument is passed by value, which eliminates one layer of indirection.
		  */
		@inline def :++[E2, S2 >: S <: Stepper[_]](next :S2)(implicit stepType :StepType[E2, S2]) :S2 =
			ConcatStepper(self, next)(stepType)

		/** Combines this stepper with the argument, returning a stepper of the same specialization as both.
		  * This will work only for the built in Scala steppers. Note that the argument is passed ''by-name''.
		  */
		@inline def ++[E2, S2 >: S <: Stepper[_]](next: => S2)(implicit stepType :StepType[E2, S2]) :S2 =
			concat(next)

		@inline final def concat[E2, S2 >: S <: Stepper[_]](next :S2)(implicit stepType :StepType[E2, S2]) :S2 =
			ConcatStepper(self, LazyStepper(next))(stepType)

		@inline final def to[C](factory :Factory[E, C]) :C = self.iterator.castParam[E] to factory
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
			case reusable :ReusableBuilder[E, C]       => new ComposedReusableBuilder(reusable, f, identity[C])
			case _                                     => new ComposedBuilder(self, f, identity[C])
		}

		/** Same as `this.sizeHint(hint); this`, for convenience. */
		@inline def hinted(size :Int) :Builder[E, C] = { self sizeHint size; self }

		/** Same as `this.sizeHint(coll, delta); this`, for convenience. */
		@inline def hinted(coll :IterableOnce[_], delta :Int = 0) :Builder[E, C] = { self.sizeHint(coll, delta); self }
	}


	/** Adds extension methods for appending line separator character to a `StringBuilder`. */
	class StringBuilderExtension private[extensions] (private val self :StringBuilder) extends AnyVal {
		@inline def appendln()                          :StringBuilder = self append EOL
		@inline def appendln(obj :Any)                  :StringBuilder = self append obj append EOL
		@inline def appendln(int :Int)                  :StringBuilder = self append int append EOL
		@inline def appendln(long :Long)                :StringBuilder = self append long append EOL
		@inline def appendln(double :Double)            :StringBuilder = self append double append EOL
		@inline def appendln(float :Float)              :StringBuilder = self append float append EOL
		@inline def appendln(bool :Boolean)             :StringBuilder = self append bool append EOL
		@inline def appendln(char :Char)                :StringBuilder = self append char append EOL
		@inline def appendln(chars :String)             :StringBuilder = self append chars append EOL
		@inline def appendln(chars :Array[Char])        :StringBuilder = self append chars append EOL
		@inline def appendln(chars :CharSequence)       :StringBuilder = self append chars append EOL
		@inline def appendln(chars :IterableOnce[Char]) :StringBuilder = self.addAll(chars) append EOL

		@inline def appendln(chars :Array[Char], offset :Int, len :Int) :StringBuilder =
			self.append(chars, offset, len) append EOL

		@inline def appendln(chars :CharSequence, offset :Int, len :Int) :StringBuilder =
			self.append(chars, offset, len) append EOL

		@inline def javaIterator :JavaIntIterator = new JStringBuilderStepper(self.underlying)
		@inline def javaIterator(from :Int, until :Int) :JavaIntIterator =
			new JStringBuilderStepper(self.underlying, from, until)

		@inline def jterator :CharJterator = new JStringBuilderStepper(self.underlying).asInstanceOf[CharJterator]
		@inline def jterator(from :Int, until :Int) :CharJterator =
			new JStringBuilderStepper(self.underlying, from, until).asInstanceOf[CharJterator]
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

		//this method will not trigger the conversion, but prevents IDE errors confused by a sole setter
		@inline def length :Int = self.length
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

		@inline def appendln()                          :JStringBuilder = self append EOL
		@inline def appendln(obj :Any)                  :JStringBuilder = self append obj append EOL
		@inline def appendln(int :Int)                  :JStringBuilder = self append int append EOL
		@inline def appendln(long :Long)                :JStringBuilder = self append long append EOL
		@inline def appendln(double :Double)            :JStringBuilder = self append double append EOL
		@inline def appendln(float :Float)              :JStringBuilder = self append float append EOL
		@inline def appendln(bool :Boolean)             :JStringBuilder = self append bool append EOL
		@inline def appendln(char :Char)                :JStringBuilder = self append char append EOL
		@inline def appendln(chars :String)             :JStringBuilder = self append chars append EOL
		@inline def appendln(chars :Array[Char])        :JStringBuilder = self append chars append EOL
		@inline def appendln(chars :CharSequence)       :JStringBuilder = self append chars append EOL
		@inline def appendln(chars :IterableOnce[Char]) :JStringBuilder = addAll(chars) append EOL

		@inline def appendln(chars :Array[Char], offset :Int, len :Int) :JStringBuilder =
			self.append(chars, offset, len) append EOL

		@inline def appendln(chars :CharSequence, offset :Int, len :Int) :JStringBuilder =
			self.append(chars, offset, len) append EOL

		override def iterator :Iterator[Char] = iterator(0, self.length)

		def iterator(from :Int, until :Int) :BufferedIterator[Char] = new IndexedIterator[Char] {
			override def underlyingSize = self.length
			override var index :Int = from
			override var limit :Int = until
			adjustRange()

			override def head = self.charAt(index)

			override def next() = { val res = self.charAt(index); index += 1; res }
		}

		def javaIterator :JavaIntIterator = new JStringBuilderStepper(self)
		def javaIterator(from :Int, until :Int) :JavaIntIterator = new JStringBuilderStepper(self, from, until)
		def jterator :CharJterator = new JStringBuilderStepper(self).asInstanceOf[CharJterator]
		def jterator(from :Int, until :Int) :CharJterator =
			new JStringBuilderStepper(self, from, until).asInstanceOf[CharJterator]

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
		  * This method is particularly useful when overrdding [[Iterable]].[[scala.collection.IterableOnceOps.to to]],
		  * as it allows optimizations of returning `this` if `iterableFactory == factory.source`,
		  * or to use a dedicated implementation of the built collection type.
		  */
		def source :Any = CompanionFactory.sourceCollectionFactory(self) getOrElse self

		/** If this `Factory` was created by `IterableFactory.`[[scala.collection.IterableFactory.toFactory toFactory]],
		  * return the `IterableFactory` which created it.
		  */
		def iterableFactory[I[X]](implicit compat :C =:= I[E]) :Opt[IterableFactory[I]] =
			CompanionFactory.sourceIterableFactory(compat.substituteCo(self))

		/** If this `Factory` was created by
		  * `EvidenceIterableFactory.`[[scala.collection.EvidenceIterableFactory.toFactory toFactory]],
		  * return the `EvidenceIterableFactory` which created it.
		  */
		def evidenceIterableFactory[I[X]](implicit compat :C =:= I[E])
				:Opt[EvidenceIterableFactory[I, E] forSome { type E[v] }] =
			CompanionFactory.sourceEvidenceIterableFactory(compat.substituteCo(self))

		/** If this `Factory` was created by `MapFactory.`[[scala.collection.MapFactory.toFactory toFactory]],
		  * return the `MapFactory` which created it.
		  */
		def mapFactory[K, V, M[A, B] <: Map[A, B]](implicit elemType :E =:= (K, V), compat :C =:= M[K, V]) :Opt[MapFactory[M]] =
			CompanionFactory.sourceMapFactory(compat.substituteCo(elemType.substituteCo[({ type F[X] = Factory[X, C] })#F](self)))

		/** If this `Factory` was created by `SortedMapFactory.`[[scala.collection.SortedMapFactory.toFactory toFactory]],
		  * return the `IterableFactory` which created it.
		  */
		def sortedMapFactory[K, V, M[A, B] <: Map[A, B]](implicit elemType :E =:= (K, V), compat :C =:= M[K, V])
				:Opt[SortedMapFactory[M]] =
			CompanionFactory.sourceSortedMapFactory(
				compat.substituteCo(elemType.substituteCo[({ type F[X] = Factory[X, C] })#F](self))
			)
	}


	/** Adds a `++` extension method to any Java `Iterator[_]`. */
	class JavaIteratorExtension[I <: JavaIterator[_]] private[extensions] (private val self :I) extends AnyVal {
		/** Combines this iterator with the argument, returning an iterator of the same specialization as both.
		  * Aside from the basic `Iterator[_]`, this will work only for the standard three Java `PrimitiveIterator`s.
		  */
		def :++[A](next :I)(implicit shape :JavaIteratorShape[A, I]) :I =
			JavaConcatIterator(self, next)

		/** Combines this iterator with the by-name argument, returning an iterator of the same specialization as both.
		  * Aside from the basic `Iterator[_]`, this will work only for the standard three Java `PrimitiveIterator`s.
		  */
		def ++[A](next: => I)(implicit shape :JavaIteratorShape[A, I]) :I =
			JavaConcatIterator(self, JavaIterator.delay(next))
	}

	/** Extension methods for converting a [[java.util.PrimitiveIterator.OfInt PrimitiveIterator.OfInt]]
	  * to jterators for other value types.
	  */
	class JavaIntIteratorExtension private[extensions] (private val self :JavaIntIterator) extends AnyVal {
		/** Treats this primitive `Int` iterator as if it was an `Iterator[Char]`,
		  * by calling `toChar` on every returned value. */
		@inline def asChars  :CharJterator  = self.asInstanceOf[CharJterator]

		/** Treats this primitive `Int` iterator as if it was an `Iterator[Short]`,
		  * by calling `toShort` on every returned value. */
		@inline def asShorts :ShortJterator = self.asInstanceOf[ShortJterator]

		/** Treats this primitive `Int` iterator as if it was an `Iterator[Byte]`,
		  * by calling `toByte` on every returned value. */
		@inline def asBytes  :ByteJterator  = self.asInstanceOf[ByteJterator]

		/** Treats this primitive `Int` iterator as if it was an `Iterator[Boolean]`,
		  * treating every returned zero as `false`, and every non zero value as `true`. */
		@inline def asBooleans :BooleanJterator = self.asInstanceOf[BooleanJterator]

		/** Standard jterator/iterator API for primitive `Int` iterators. Replaces `nextInt()`
		  * with a manually specialized `next()`. */
		@inline def jterator :IntJterator   = self.asInstanceOf[IntJterator]
	}

	/** Extension method for converting a [[java.util.PrimitiveIterator.OfLong PrimitiveIterator.OfLong]]
	  * to jterators for other value types.
	  */
	class JavaLongIteratorExtension private[extensions] (private val self :JavaLongIterator) extends AnyVal {
		/** Standard jterator/iterator API for primitive `Long` iterators. Replaces `nextLong()`
		  * with a manually specialized `next()`. */
		@inline def jterator :LongJterator = self.asInstanceOf[LongJterator]
	}

	/** Extension method for converting a [[java.util.PrimitiveIterator.OfDouble PrimitiveIterator.OfDouble]]
	  * to jterators for other value types.
	  */
	class JavaDoubleIteratorExtension private[extensions] (private val self :JavaDoubleIterator) extends AnyVal {
		/** Treats this primitive `Double` iterator as if it was an `Iterator[Float]`,
		  * by calling `toFloat` on every returned value. */
		@inline def asFloats :FloatJterator  = self.asInstanceOf[FloatJterator]

		/** Standard jterator/iterator API for primitive `Double` iterators. Replaces `nextDouble()`
		  * with a manually specialized `next()`. */
		@inline def jterator :DoubleJterator = self.asInstanceOf[DoubleJterator]
	}

	class JteratorExtension[I <: Jterator[_]] private[extensions] (private val self :JavaIterator[_]) extends AnyVal {
		@inline def hasNext :Boolean = self.hasNext

		def :++[E](other :I)(implicit shape :JteratorShape[E, I]) :I = {
			implicit val iterShape = shape.javaIteratorShape.asInstanceOf[JavaIteratorShape[E, I]]
			JavaConcatIterator(self.asInstanceOf[I], other).asInstanceOf[I]
		}
		def ++[E](other: => I)(implicit shape :JteratorShape[E, I]) :I = {
			implicit val iterShape = shape.javaIteratorShape.asInstanceOf[JavaIteratorShape[E, I]]
			JavaConcatIterator(self.asInstanceOf[I], JavaIterator.delay(other)).asInstanceOf[I]
		}
	}

	//consider: moving these to companion objects.
	/** Provides standard iterator methods for an opaque [[java.util.PrimitiveIterator.OfInt PrimitiveIterator.OfInt]]. */
	class IntJteratorExtension private[extensions] (private val self :JavaIntIterator) extends AnyVal {
		@inline def hasNext :Boolean = self.hasNext
		@inline def next()  :Int = self.nextInt()
		def ++(other :IntJterator) :IntJterator =
			JavaConcatIterator(self, other.asInstanceOf[JavaIntIterator]).asInstanceOf[IntJterator]
	}
	/** Provides standard iterator methods for an opaque [[java.util.PrimitiveIterator.OfLong PrimitiveIterator.OfLong]]. */
	class LongJteratorExtension private[extensions] (private val self :JavaLongIterator) extends AnyVal {
		@inline def hasNext :Boolean = self.hasNext
		@inline def next()  :Long = self.nextLong()
		def ++(other :LongJterator) :LongJterator =
			JavaConcatIterator(self, other.asInstanceOf[JavaLongIterator]).asInstanceOf[LongJterator]
	}
	/** Provides standard iterator methods for an opaque [[java.util.PrimitiveIterator.OfDouble PrimitiveIterator.OfDouble]]. */
	class DoubleJteratorExtension private[extensions] (private val self :JavaDoubleIterator) extends AnyVal {
		@inline def hasNext :Boolean = self.hasNext
		@inline def next()  :Double = self.nextDouble()
		def ++(other :DoubleJterator) :LongJterator =
			JavaConcatIterator(self, other.asInstanceOf[JavaDoubleIterator]).asInstanceOf[LongJterator]
	}
	/** Extension methods allowing treating an opaque [[java.util.PrimitiveIterator.OfDouble PrimitiveIterator.OfDouble]]
	  * as an `Iterator[Float]`. */
	class FloatJteratorExtension private[extensions] (private val self :JavaDoubleIterator) extends AnyVal {
		@inline def hasNext :Boolean = self.hasNext
		@inline def next()  :Float = self.nextDouble().toFloat
	}
	/** Extension methods allowing treating an opaque [[java.util.PrimitiveIterator.OfInt PrimitiveIterator.OfInt]]
	  * as an `Iterator[Short]`. */
	class ShortJteratorExtension private[extensions] (private val self :JavaIntIterator) extends AnyVal {
		@inline def hasNext :Boolean = self.hasNext
		@inline def next()  :Short = self.nextInt().toShort
	}
	/** Extension methods allowing treating an opaque [[java.util.PrimitiveIterator.OfInt PrimitiveIterator.OfInt]]
	  * as an `Iterator[Char]`. */
	class CharJteratorExtension private[extensions] (private val self :JavaIntIterator) extends AnyVal {
		@inline def hasNext :Boolean = self.hasNext
		@inline def next()  :Char = self.nextInt().toChar
	}
	/** Extension methods allowing treating an opaque [[java.util.PrimitiveIterator.OfInt PrimitiveIterator.OfInt]]
	  * as an `Iterator[Byte]`. */
	class ByteJteratorExtension private[extensions] (private val self :JavaIntIterator) extends AnyVal {
		@inline def hasNext :Boolean = self.hasNext
		@inline def next()  :Byte = self.nextInt().toByte
	}
	/** Extension methods allowing treating an opaque [[java.util.PrimitiveIterator.OfInt PrimitiveIterator.OfInt]]
	  * as an `Iterator[Boolean]`. */
	class BooleanJteratorExtension private[extensions] (private val self :JavaIntIterator) extends AnyVal {
		@inline def hasNext :Boolean = self.hasNext
		@inline def next()  :Boolean = self.nextInt() != 0
	}
	/** Extension methods allowing treating an opaque [[java.util.Iterator]] in a uniform manner with other jterators. */
	class RefJteratorExtension[E] private[extensions] (private val self :JavaIterator[E]) extends AnyVal {
		@inline def hasNext :Boolean = self.hasNext
		@inline def next()  :E = self.next()
	}




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
		  * @tparam E element type of the generated collection.
		  * @return a collection containing the sequence starting with `start` and resulting from recursively applying
		  *         `next` to itself.
		  */
		@nowarn("cat=deprecation")
		@inline final def generate[E](start :E)(next :PartialFunction[E, E]) :C[E] =
			companion match {
				case Stream =>
					(start#::(next.applyAndThenOrElse(start, Stream.generate(_)(next), _ => Stream.empty[E]))).castCons[C]
				case LazyList =>
					(start#::(next.applyAndThenOrElse(start, LazyList.generate(_)(next), _ => LazyList.empty[E]))).castCons[C]
				case _ =>
					companion from Iterator.generate(start)(next)
			}

		/** Builds a collection `C[E]` by recursively reapplying the given function to the initial element.
		  * Instead of listing a fixed number of elements, this method uses the generator function `next`
		  * as the termination condition and ends the recursion once it returns `None`. It is the opposite
		  * of [[scala.collection.IterableOnceOps.reduce reduce]] in the same way as
		  * [[scala.collection.IterableFactory.unfold unfold]] is the opposite
		  * of [[scala.collection.IterableOnceOps.fold fold]].
		  * @param start The first element added to the collection.
		  * @param next  A generator function returning subsequent elements for the collection based on the previous one,
		  *              or `None` to indicate the end of recursion.
		  * @tparam E the element type of the generated collection.
		  * @return a collection containing the sequence starting with `start` and resulting from recursively applying `next`
		  *         to itself.
		  */
		@nowarn("cat=deprecation")
		final def expand[E](start :E)(next :E => Option[E]) :C[E] =
			companion match {
				case Stream =>
					(start #:: (next(start).map(Stream.expand(_)(next)) getOrElse Stream.empty)).castCons[C]
				case LazyList =>
					(start #:: (next(start).map(LazyList.expand(_)(next)) getOrElse LazyList.empty)).castCons[C]
				case _ =>
					companion from Iterator.expand(start)(next)
			}

		/** Similar to [[scala.collection.IterableFactory IterableFactory]]`.`[[scala.collection.IterableFactory.iterate iterate]],
		  * but the iterating function accepts the positional index of the next element as an additional argument.
		  * @param start The first element of the created collection.
		  * @param len   The size of the created collection.
		  * @param f     A function generating subsequent elements following start.
		  *              The second element of the collection will be `f(start, 1)`, the third `f(f(start, 1), 2)`, and so on.
		  */
		final def iterateWithIndex[E](start :E, len :Int)(f :(E, Int) => E) :C[E] =
			companion.from(Iterator.iterateWithIndex(start, len)(f))
	}


	/** Extension factory methods for single element immutable [[collection.immutable.Set Set]] companions. */
	class immutableSetFactoryExtension[C[X] <: SetOps[X, C, C[X]]] private[collections]
	                                  (private val self :IterableFactory[C])
		extends AnyVal
	{
		@inline def single[E](elem :E) :C[E] = self.empty[E] + elem
		@inline def one[E](elem :E)    :C[E] = self.empty[E] + elem
	}

	/** Extension factory methods for single element [[collection.immutable.Seq Seq]] companions. */
	class SeqFactoryExtension[C[X] <: collection.SeqOps[X, C, C[X]]] private[collections]
	                         (private val self :IterableFactory[C])
		extends AnyVal
	{
		@inline def single[E](elem :E) :C[E] = one(elem)

		def one[E](elem :E) :C[E] =
			if (self eq IndexedSeq) ConstIndexedSeq(elem, 1).castCons[C]
			else if (self eq RelayArray) RelayArray.one(elem).castCons[C]
			else if ((self eq Buffer) || (self eq mutable.Seq)) (Buffer.empty[E] += elem).castCons[C]
			else if ((self eq ArrayBuffer) || (self eq IndexedBuffer) || (self eq mutable.IndexedSeq))
				(new ArrayBuffer[E] += elem).castCons[C]
			else if (self eq ListBuffer) (ListBuffer.empty[E] += elem).castFrom[ListBuffer[E], C[E]]
			else self.empty[E] :+ elem

		def two[E](first :E, second :E) :C[E] = //not match because of 'pattern type is incompatible with matched value'
			if (self eq Seq) new Prepended2Seq(first, second, Nil).castCons[C]
			else if (self eq IndexedSeq) new Prepended2IndexedSeq(first, second, IndexedSeq.empty).castCons[C]
			else if (self eq List) (first::second::Nil).castCons[C]
			else if (self eq RelayArray) RelayArray.two(first, second).castFrom[Seq[E], C[E]]
			else if ((self eq Buffer) || (self eq mutable.Seq)) (Buffer.empty[E] += first += second).castCons[C]
			else if ((self eq ArrayBuffer) || (self eq mutable.IndexedSeq) || (self eq IndexedBuffer))
				(new ArrayBuffer[E] += first += second).castCons[C]
			else if (self eq ListBuffer) (new ListBuffer[E] += first += second).castCons[C]
			else self.from(new Prepended2Seq(first, second, Nil))

		/** The same as [[collection.SeqFactory.fill fill]], but the expression is evaluated only once,
		  * rather than for each element independently. Additionally, [[collection.immutable.Seq$ Seq]],
		  * [[collection.immutable.IndexedSeq$ IndexedSeq]] and [[collection.immutable.LinearSeq$ LinearSeq]] have
		  * dedicated implementations using `O(1)` space and `O(1)` time to create.
		  */
		def const[E](size :Int)(elem :E) :C[E] =
			if (self eq IndexedSeq)
				ConstIndexedSeq(elem, math.max(0, size)).castCons[C]
			else if (self eq LinearSeq)
				ConstLinearSeq(elem, math.max(0, size)).castCons[C]
			else if (self eq Seq)
				ConstSeq(elem, math.max(0, size)).castCons[C]
			else if (size <= 0)
				self.empty[E]
			else {
				val b = self.newBuilder[E]
				b sizeHint size
				var i = size
				while (i > 0) {
					b += elem; i -= 1
				}
				b.result()
			}

		/** An alias for [[collection.IterableFactory.empty empty]]`[E] `[[collection.SeqOps.:+ :+]]` elem`. */
		@inline def :+[E](elem :E) :C[E] = self.empty[E] :+ elem

		/** An alias for `elem `[[collection.SeqOps.+: +:]]` `[[collection.IterableFactory.empty empty]]`[E]`. */
		@inline def +:[E](elem :E) :C[E] = elem +: self.empty[E]
	}

	sealed trait immutableIndexedSeqCompanionExtension extends Any {
		@inline final def infinite[E](elem :E) :IndexedSeq[E] = ConstIndexedSeq.infinite(elem)
		@inline final def reversed[E](seq :IndexedSeq[E]) :IndexedSeq[E] = ReversedSeq(seq)
	}


	sealed trait ArrayBufferCompanionExtension extends Any {
		/** A new, empty buffer. Same as `empty`, but slightly more succinct, and puts emphasis on the element type. */
		@inline final def of[E] :ArrayBuffer[E] = new AliasingArrayBuffer[E]

		/** A new buffer, with space reserved for `capacity` elements. Works similarly to
		  * [[collection.mutable.Builder Builder]]`.`[[collection.mutable.Builder.sizeHint sizeHint]].
		  */
		@inline final def ofCapacity[E](capacity :Int) :ArrayBuffer[E] = {
			val res = new AliasingArrayBuffer[E]
			res.sizeHint(capacity)
			res
		}
	}

	/** Extension factory methods for single and two element [[Map Map]]s. */
	sealed trait immutableMapCompanionExtension extends Any {
		@inline final def single[K, V](key :K, value :V) :Map[K, V] = new Map.Map1(key, value)
		@inline final def single[K, V](entry :(K, V)) :Map[K, V] = new Map.Map1(entry._1, entry._2)
		@inline final def one[K, V](key :K, value :V) :Map[K, V] = new Map.Map1(key, value)
		@inline final def one[K, V](entry :(K, V)) :Map[K, V] = new Map.Map1(entry._1, entry._2)
		@inline final def two[K, V](key1 :K, value1 :V, key2 :K, value2 :V) :Map[K, V] =
			new Map.Map2(key1, value1, key2, value2)
		@inline final def two[K, V](entry1 :(K, V), entry2 :(K, V)) :Map[K, V] =
			new Map.Map2(entry1._1, entry1._2, entry2._1, entry2._2)
	}



	/** Adds factory methods for array iterators
	  * and a [[net.noresttherein.sugar.collections.extensions.IteratorCompanionExtension.double double]] factory method
	  * for two element iterators to object `Iterator` to complement [[scala.collection.Iterator.single single]].
	  */
	sealed trait IteratorCompanionExtension extends Any {
		/** An iterator consisting of a single element. In contrast to the standard `Iterator.single`,
		  * its `knownSize` is always non negative.
		  */
		final def one[A](elem :A) :Iterator[A] = Iterators.single(elem)

		/** An iterator consisting of two elements. */
		final def two[A](first :A, second :A) :Iterator[A] = Iterators.double(first, second)

		/** An iterator consisting of two elements. */
		final def double[A](first :A, second :A) :Iterator[A] = Iterators.double(first, second)

		/** Same as `Iterator.`[[scala.collection.Iterator.fill fill]](len)(value), but returns a constant value. */
		final def const[A](len :Int)(value :A) :Iterator[A] = new Iterators.Const(len, value)

		/** Same as `Iterator.`[[scala.collection.Iterator.continually continually]](value), but returns a constant value. */
		final def infinite[A](value :A) :Iterator[A] = new Iterators.ConstInfinite(value)

		/** An iterator over the entirety of the specified array. */
		final def over[X](array :Array[X]) :Iterator[X] = ArrayIterator(array)

		/** An iterator over a slice of an array. */
		final def slice[X](array :Array[X], from :Int, until :Int) :Iterator[X] =
			ArrayIterator.slice(array, from, until)

		/** An iterator going over the elements of an array in reverse. */
		final def reverse[X](array :Array[X]) :Iterator[X] = ReverseArrayIterator(array)

		/** An iterator over a slice of an array going in reverse. The first item returned will be at `array(until-1)`,
		  * and the last `array(from)`.
		  */
		final def reverse[X](array :Array[X], from :Int, until :Int) :Iterator[X] =
			ReverseArrayIterator.slice(array, from, until)

		/** A complement of `Iterator.iterate` and `Iterator.unfold`, which creates an iterator
		  * by recursively applying a partial function while defined to its own results and collecting
		  * all returned values. It is very similar to the standard [[scala.collection.Iterator.iterate iterate]],
		  * but instead of a fixed number of iterations, the generator function `next` is called for its return values
		  * until it is no longer applicable, which marks the end of the collection.
		  * @param start the first element returned by the iterator.
		  * @param next  a generator function returning subsequent elements of the iterator based on the previous one,
		  *              serving as the termination condition by indicating that it can no longer be applied
		  *              to the given argument.
		  * @tparam E the type of the elements returned by the iterator.
		  * @return an iterator containing a sequence starting with `start` and resulting from recursively applying
		  *         `next` to itself.
		  */
		@nowarn("cat=deprecation")
		final def generate[E](start :E)(next :PartialFunction[E, E]) :Iterator[E] = {
			val f = next
			new AbstractBufferedIterator[E] {
				push(start)
				override def hasNext = super.hasNext ||
					f.applyAndThenOrElse(last, { x => push(x); true }, _ => false)
			}
		}

		/** Creates an iterator by recursively reapplying the given function to the initial element.
		  * Instead of listing a fixed number of elements, this method uses the generator function `next`
		  * as the termination condition and ends the recursion once it returns `None`. It is the opposite
		  * of [[scala.collection.IterableOnceOps.reduce reduce]] in the same way as
		  * [[scala.collection.Iterator.unfold unfold]] is the opposite
		  * of [[scala.collection.IterableOnceOps.fold fold]].
		  * @param start The first element returned by the iterator.
		  * @param next  A generator function returning subsequent elements of the iterator based on the previous one,
		  *              or `None` to indicate the end of recursion.
		  * @tparam E the type of the elements returned by the iterator.
		  * @return an iterator containing a sequence starting with `start`,
		  *         and resulting from recursively applying `next` to itself.
		  */
		@nowarn("cat=deprecation")
		final def expand[E](start :E)(next :E => Option[E]) :Iterator[E] = {
			val f = next
			new BufferedIterator[E] {
					private[this] var cont :Option[E] = Some(start)
					override def head = cont.get
					override def hasNext = cont.isDefined
					override def next() = { val res = cont.get; cont = f(res); res }
				}
		}

		/** Similar to [[scala.collection.Iterator$ Iterator]]`.`[[scala.collection.Iterator.iterate iterate]],
		  * but the iterating function accepts the positional index of the next element as an additional argument.
		  * @param start The first element of the created iterator.
		  * @param len   The size of the created iterator.
		  * @param f     A function generating subsequent elements following start.
		  *              The second element returned by the iterator will be `f(start, 1)`,
		  *              the third `f(f(start, 1), 2)`, and so on.
		  */
		final def iterateWithIndex[E](start :E, len :Int)(f :(E, Int) => E) :Iterator[E] =
			Iterator.iterate((start, 0), len) { xi => (f(xi._1, xi._2 + 1), xi._2 + 1) }.map(_._1)
	}



	/** Adds factory methods to the [[scala.collection.Stepper$ Stepper]] singleton object, creating steppers
	  * of zero, one or two elements.
	  */
	sealed trait StepperCompanionExtension extends Any {

		/** Adapts the given Java spliterator to an `AnyStepper`. */
		@inline def apply[T](spliterator :Spliterator[T]) :AnyStepper[T] = SpliteratorStepper.ofRef(spliterator)

		/** Adapts the given java spliterator to the corresponding stepper type. */
		@inline def apply(spliterator :Spliterator.OfInt) :IntStepper = SpliteratorStepper.ofInt(spliterator)

		/** Adapts the given java spliterator to the corresponding stepper type. */
		@inline def apply(spliterator :Spliterator.OfLong) :LongStepper = SpliteratorStepper.ofLong(spliterator)

		/** Adapts the given java spliterator to the corresponding stepper type. */
		@inline def apply(spliterator :Spliterator.OfDouble) :DoubleStepper = SpliteratorStepper.ofDouble(
			spliterator)

		/** Creates an empty stepper with the specified element type.
		  * The returned object has an `apply()` method accepting an implicit
		  * [[scala.collection.StepperShape StepperShape]]`[T, S]`; this allows to split type parameter groups
		  * and provide here only the element type: `Stepper[T]()`.
		  * @see [[net.noresttherein.sugar.collections.extensions.StepperCompanionExtension.empty empty]]
		  */
		@inline final def apply[T] :EmptyStepperFactory[T] = new EmptyStepperFactory[T] {}

		/** Creates an empty `Stepper`. This method variant requires either explicit specification of type parameters,
		  * or for the element type to be abstract, with a single implicit `StepperShape[T, S]` in scope.
		  * @see [[net.noresttherein.sugar.collections.extensions.StepperCompanionExtension.apply[T] apply]]`()`.
		  */
		@inline final def empty[T, S <: Stepper[_]](implicit shape :StepperShape[T, S]) :S with EfficientSplit =
			Stepper0()

		/** Creates a stepper of a single element, with a shape proper for that element.
		  * While the created `Stepper` will not box value types, this method itself is generic and hence boxes them.
		  * You can invoke manually specialized methods instead - `ofInt`, `ofLong`, `ofDouble`, `ofAny` -
		  * to avoid boxing.
		  */
		@inline final def single[T, S <: Stepper[_]](elem :T)(implicit shape :StepperShape[T, S]) :S with EfficientSplit =
			Stepper1(elem)

		/** Creates a stepper of a single element, with a shape proper for that element.
		  * While the created `Stepper` will not box value types, this method itself is generic and hence boxes them.
		  * You can invoke manually specialized methods instead - `ofInt`, `ofLong`, `ofDouble`, `ofAny` -
		  * to avoid boxing.
		  */
		@inline final def one[T, S <: Stepper[_]](elem :T)(implicit shape :StepperShape[T, S]) :S with EfficientSplit =
			Stepper1(elem)

		/** Creates a stepper of a two elements, with a shape proper for those elements.
		  * While the created `Stepper` will not box value types, this method itself is generic and hence boxes them.
		  * You can invoke manually specialized methods instead - `ofInt`, `ofLong`, `ofDouble`, `ofAny` -
		  * to avoid boxing.
		  */
		@inline final def two[T, S <: Stepper[_]](first :T, second :T)(implicit shape :StepperShape[T, S])
				:S with EfficientSplit =
			Stepper2(first, second)

		/** Creates a stepper returning a maximum of `size` elements from an array, starting with `array(from)`.
		  * If `from <= 0` or `from >= array.length` or `from + size >= array.length`,
		  * then the arguments are adjusted to range, and negative `size` will simply result in an empty stepper.
		  */
		final def apply[T, S <: Stepper[_]](array :Array[T], from :Int, size :Int)
		                                   (implicit shape :StepperShape[T, S]) :S with EfficientSplit =
		{
			val length = array.length
			if (from <= 0 | size <= 0 | from >= length) Stepper0()
			else ArrayStepper.slice(array, from, from + math.min(length - from, size))
		}

		/** Creates a stepper iterating over the specified range of the given array. If `from < 0` or
		  * `until > array.length`, they are adjusted `0/array.length` and no exception is thrown due to indexing.
		  */
		final def slice[T, S <: Stepper[_]](array :Array[T], from :Int = 0, until :Int = Int.MaxValue)
		                                   (implicit shape :StepperShape[T, S]) :S with EfficientSplit =
			if (until <= 0 || until <= from) Stepper0()
			else ArrayStepper.slice(array, from, until)

		/** Creates an empty stepper for reference types (`AnyStepper`). */
		@inline def ofAny[T]() :AnyStepper[T] with EfficientSplit = Stepper0.ofAny

		/** Creates a single element stepper specific to reference types.
		  * When used for a value type, the value will be boxed by this call.
		  */
		@inline def ofAny[T](elem :T) :AnyStepper[T] with EfficientSplit = Stepper1.ofAny(elem)

		/** Creates a stepper of two elements specific to reference types.
		  * When used for a value type, the values will be boxed by this call.
		  */
		@inline def ofAny[T](first :T, second :T) :AnyStepper[T] with EfficientSplit = Stepper2.ofAny(first, second)

		/** Creates an empty stepper for `Int`. */
		@inline def ofInt() :IntStepper with EfficientSplit = Stepper0.ofInt

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
		@inline def ofInt(string :String, from :Int, until :Int) :IntStepper with EfficientSplit = {
			val from0 = math.min(string.length, math.max(from, 0))
			val until0 = math.min(string.length, math.max(until, from0))
			new StringStepper(string, from0, until0)
		}

		/** Creates an empty stepper for `Long`. */
		@inline def ofLong() :LongStepper with EfficientSplit = Stepper0.ofLong

		/** Creates a stepper for a single `Long`. */
		@inline def ofLong(elem :Long) :LongStepper with EfficientSplit = Stepper1.ofLong(elem)

		/** Creates a stepper for two `Long`s. */
		@inline def ofLong(first :Long, second :Long) :LongStepper with EfficientSplit = Stepper2.ofLong(first, second)

		/** Creates an empty stepper for `Double`.
		  * This method can be also used for `Float`, as it too uses `DoubleStepper`.
		  */
		@inline def ofDouble() :DoubleStepper with EfficientSplit = Stepper0.ofDouble

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

	/** An `apply()` factory method for ana empty stepper, accepting an implicit `StepperShape[T, S]`,
	  * inferring the stepper type. It is a continuation of
	  * a [[net.noresttherein.sugar.collections.extensions.StepperCompanionExtension.apply Stepper]]`[T]` call.
	  */
	sealed trait EmptyStepperFactory[T] extends Any {
		/** Creates an empty `Stepper` of shape defined by an implicit `StepperShape` for element type `T`. */
		@inline final def apply[S <: Stepper[_]]()(implicit shape :StepperShape[T, S]) :S with EfficientSplit =
			Stepper0()
	}

	/** Provides a method for explicit summoning of a `StepperShape` for a specified element type:
	  * {{{
	  *     StepperShape[Int]()
	  * }}}
	  */
	sealed trait StepperShapeCompanionExtension extends Any {
		/** Summons an implicit `StepperShape` for the element type `T` specified as the type parameter.
		  * The returned object has method `apply[S <: Stepper[_]]()(implicit shape :StepperShape[T, S]) :StepperShape[T, S]`,
		  * so the shape type can be inferred based on `T` by simply following this call with `()`:
		  * {{{
		  *     val shape = StepperShape[Int]()
		  *     shape :StepperShape[Int, IntStepper]
		  * }}}
		  */
		@inline final def apply[T] :StepperShapeObjectFactory[T] = new StepperShapeObjectFactory[T] {}
	}

	/** An `apply()` method summoning an implicit `StepperShape[T, S]`. It is a continuation
	  * of a [[net.noresttherein.sugar.collections.extensions.StepperShapeCompanionExtension.apply StepperShape]]`[T]` call.
	  */
	sealed trait StepperShapeObjectFactory[T] extends Any {
		@inline final def apply[S <: Stepper[_]]()(implicit shape :StepperShape[T, S]) :StepperShape[T, S] = shape
	}

}
