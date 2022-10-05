package net.noresttherein.sugar.collections

import java.util.Arrays

import scala.annotation.{nowarn, tailrec}
import scala.collection.{mutable, AnyStepper, BufferedIterator, DoubleStepper, IntStepper, IterableFactory, IterableOnce, IterableOps, LinearSeq, LongStepper, Stepper, StepperShape, View}
import scala.collection.Stepper.EfficientSplit
import scala.collection.immutable.{ArraySeq, IndexedSeqDefaults, MapOps}
import scala.collection.generic.IsIterableOnce
import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.reflect.ClassTag
import scala.runtime.BoxedUnit

import net.noresttherein.sugar.JavaTypes.{JIterator, JStringBuilder}
import net.noresttherein.sugar.collections.extensions.{ArrayFactoryExtension, BuilderExtension, FoldingMethods, IterableFactoryExtension, IteratorFactoryExtension, JavaIteratorExtension, JavaStringBuilderExtension, MapExtension, MappingMethods, SeqExtension, StepperExtension}
import net.noresttherein.sugar.raise
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/** Extension methods for various collection types as well as collection companion objects. */
trait extensions extends Any {
	//todo: IsIterableOnce creates multiple objects for things like Seq; It's better to have manual specializations.
	/** Adds a `foldWhile` method to any `Iterable` which implement a variant of `fold` operation with a break condition. */
	@inline implicit final def foldingMethods[C](self :C)(implicit iterable :IsIterableOnce[C]) :FoldingMethods[iterable.A] =
		new FoldingMethods[iterable.A](iterable(self))

	/** Adds method `mapWith` and `flatMapWith` which map/flat map collections while passing along additional state
	  * to any collection of the standard library framework.
	  */
	@inline implicit final def mappingMethods[C[X] <: Iterable[X], E](self :IterableOps[E, C, C[E]]) :MappingMethods[C, E] =
		new MappingMethods[C, E](self)

//	/** Adds method `mapWith` and `flatMapWith` which map/flat map arrays while passing along additional state
//	  * to any `Array`.
//	  */ //MappingMethods uses iterableFactory.newBuilder which will produce Object[], rather than a specific array
//	@inline implicit final def mappingMethods[E](self :Array[E]) :MappingMethods[Array, E]=
//		new MappingMethods[Array, E](new ArrayIterableOps(self))

	/** Alternative, safer implementations of [[scala.collection.SeqOps.indexOf indexOf]] for [[Seq]],
	  * which do not return a negative index when the element is not found.
	  */
	@inline implicit final def seqExtension[X](self :scala.collection.Seq[X]) :SeqExtension[X] =
		new SeqExtension[X](self)

	/** Alternative, safer implementations of [[scala.collection.SeqOps.indexOf indexOf]] for [[Array]],
	  * which do not return a negative index when the element is not found.
	  */
	@inline implicit final def arrayExtension[X](self :Array[X]) :SeqExtension[X] =
		new SeqExtension[X](mutable.ArraySeq.make(self))

	/** An [[net.noresttherein.sugar.collections.extensions.MapExtension.updatedIfAbsent updatedIfAbsent]] method
	  * for any `Map`.
	  */
	@inline implicit final def mapExtension[K, V, M[A, +B] <: MapOps[A, B, M, M[A, B]]]
	                                       (self :M[K, V]) :MapExtension[K, V, M] =
		new MapExtension(self)

	/** Adds a `++` method to any `Stepper` type. */
	@inline implicit final def stepperExtension[S <: Stepper[_]](self :S) =
		new StepperExtension[S](self)

//	@inline implicit final def efficientSplitStepperExtension[S <: Stepper[_]](self :S with EfficientSplit) =
//		new StepperExtension[S with EfficientSplit](self)

	/** Adds a `++` method to any standard `java.util.Iterator` subclass. */
	@inline implicit final def javaIteratorExtension[I <: JIterator[_]](self :I) =
		new JavaIteratorExtension[I](self)

	/** Additional, higher level factory methods of any [[Iterable]] type `C[_]` as extensions of its companion
	  * [[scala.collection.IterableFactory IterableFactory]]`[C]`.
	  * Adds methods [[net.noresttherein.sugar.collections.extensions.IterableFactoryExtension.expand expand]]
	  * and [[net.noresttherein.sugar.collections.extensions.IterableFactoryExtension.generate generate]]
	  * to any [[scala.collection.IterableFactory IterableFactory]].
	  */
	@inline implicit final def iterableFactoryExtension[C[_]](self :IterableFactory[C]) =
		new IterableFactoryExtension[C](self)

	/** Adds a [[net.noresttherein.sugar.collections.extensions.IteratorFactoryExtension.double double]] factory method
	  * for two element iterators to object `Iterator`.
	  */
	@inline implicit final def iteratorFactoryExtension(self :Iterator.type) = new IteratorFactoryExtension {}

	/** Adds extra extension methods to the `Array` object. */
	@inline implicit final def arrayFactoryExtension(self :Array.type) = new ArrayFactoryExtension {}


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
}




@SerialVersionUID(ver)
object extensions {
	/** A syntactic wrapper for collections, injecting methods implementing
	  * 'breakable' folding and reducing, that is folding only some prefix/suffix of the collection
	  * based on some predicate or other termination condition.
	  *
	  * @param items any collection to fold.
	  * @tparam T element type of this collection.
	  */
	class FoldingMethods[T] private[extensions] (private val items :IterableOnce[T]) extends AnyVal {
		/** Creates a Java [[java.util.Iterator Iterator]] of a proper specialization for type `T`
		  * (`Int`, `Long`, `Double`). If the underlying collection provides a specialized (non boxing)
		  * [[scala.collection.Stepper Stepper]], then the returned iterator will not box value types.
		  */
		def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[T, I]) :I = items match {
			case sugared :SugaredIterable[T]         => sugared.jiterator
			case empty :Iterable[_] if empty.isEmpty => JavaIterator()
			case _ :ArraySeq[T] | _ :mutable.ArraySeq[T] | _ :ArrayBuffer[T] =>
				items.stepper.javaIterator.asInstanceOf[I]
			case seq :collection.IndexedSeq[T]       => JavaIterator.over(seq)
			case it :Iterator[_] if !it.hasNext      => JavaIterator()
			case _                                   => items.stepper.javaIterator.asInstanceOf[I]
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
		  * to `this.`[[net.noresttherein.sugar.collections.extensions.FoldingMethods.foldWhile foldWhile]]`(start)(!pred(_))(op)`,
		  * as the latter will return the last element for which `pred` was not satisfied, while this method
		  * applies `op` once more.
		  * @param start the initial value, used as the result if the collection is empty.
		  * @param pred  a stop condition for folding, checked first for `start`, and then for every result
		  *              of function `op`. It should be an attractor state of `op`: if `pred(a)`,
		  *              then `pred(op(a, ai))` and `pred(op(ai, a))` should also be true for any `ai`
		  *              in this collection, or the result is undefined.
		  * @param op    a folding function.
		  * @return This implementation delegates
		  *         to [[net.noresttherein.sugar.collections.extensions.FoldingMethods.foldLeftUntil]]`(start)(pred)(op)`.
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
		  *    1. If, after folding the whole collection, the predicate is still not satisfied, then return `None`
		  *
		  * It is not equivalent
		  * to `this.`[[net.noresttherein.sugar.collections.extensions.FoldingMethods.foldWhileOpt foldWhileOpt]]`(start)(!pred(_))(op)`,
		  * as the latter will return the last element for which `pred` was not satisfied, while this method
		  * applies `op` once more.
		  * @param start the initial value, used as the result if the collection is empty.
		  * @param pred  a stop condition for folding, checked first for `start`, and then for every result
		  *              of function `op`. It should be an attractor state of `op`: if `pred(a)`,
		  *              then `pred(op(a, ai))` and `pred(op(ai, a))` should also be true for any `ai`
		  *              in this collection, or the result is undefined.
		  * @param op    a folding function.
		  * @return This implementation delegates
		  *         to [[net.noresttherein.sugar.collections.extensions.FoldingMethods.foldLeftUntilOpt]]`(start)(pred)(op)`
		  */
		def foldUntilOpt[A >: T](start :A)(pred :A => Boolean)(op :(A, A) => A) :Option[A] =
			foldLeftUntilOpt(start)(pred)(op)

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
		  * to `this.`[[net.noresttherein.sugar.collections.extensions.FoldingMethods.foldWhileEither foldWhileEither]]`(start)(!pred(_))(op)`,
		  * as the latter will return the last element for which `pred` was not satisfied, while this method
		  * applies `op` once more.
		  * @param start the initial value, used as the result if the collection is empty.
		  * @param pred  a stop condition for folding, checked first for `start`, and then for every result
		  *              of function `op`. It should be an attractor state of `op`: if `pred(a)`,
		  *              then `pred(op(a, ai))` and `pred(op(ai, a))` should also be true for any `ai`
		  *              in this collection, or the result is undefined.
		  * @param op    a folding function.
		  * @return This implementation delegates
		  *         to [[net.noresttherein.sugar.collections.extensions.FoldingMethods.foldLeftUntilEither]]`(start)(pred)(op)`
		  */
		def foldUntilEither[A >: T](start :A)(pred :A => Boolean)(op :(A, A) => A) :Either[A, A] =
			foldLeftUntilEither(start)(pred)(op)


		private def foldLeftUntilAndReturn[A, X](start :A)(pred :A => Boolean)(op :(A, T) => A)
		                                        (ifNotFound :A => X, ifFound :A => X) :X =
			items match {
				case _ if pred(start) => ifFound(start)
				case it :Iterable[_] if it.isEmpty => ifNotFound(start)
				case _ =>
					var item = start; var found = false; val i = items.iterator
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
		  * [[net.noresttherein.sugar.collections.extensions.FoldingMethods.foldLeftWhile foldLeftWhile]]`(start)(!pred(_))(op)`,
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
		  * [[net.noresttherein.sugar.collections.extensions.FoldingMethods.foldLeftWhileOpt foldLeftWhileOpt]]`(start)(!pred(_))(op)`,
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
		def foldLeftUntilOpt[A](start :A)(pred :A => Boolean)(op :(A, T) => A) :Option[A] =
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
		  * [[net.noresttherein.sugar.collections.extensions.FoldingMethods.foldLeftWhileEither foldLeftWhileEither]]`(start)(!pred(_))(op)`,
		  * as the latter will return the last element for which `pred` was not satisfied, while this method
		  * applies `op` once more.
		  * @param start the initial value, used as the result if the collection is empty.
		  * @param pred  a stop condition for folding, checked first for `start`, and then for every result
		  *              of function `op`.
		  * @param op    a folding function.
		  * @return The first value returned by recursive application of `op` for which `pred` is `true`:
		  *    1. If `pred(start)`, then return `Right(start)` immediately;
		  *    1. If this collection is empty, return `Left(start)`;
		  *    1. Otherwise, apply `op` recursively and return the first ai, `a_0 = start; a_i+1 = op(a_i, ei)`,
		  *       where `toSeq == Seq(e0,...,en)`, such that `pred(ai)`;
		  *    1. If `!pred(ai)` for all `ai` then return `Left(an)`.
		  */
		def foldLeftUntilEither[A](start :A)(pred :A => Boolean)(op :(A, T) => A) :Either[A, A] =
			foldLeftUntilAndReturn(start)(pred)(op)(Left.apply, Right.apply)


		private def foldRightUntilAndReturn[A, X](start :A)(pred :A => Boolean)(op :(T, A) => A)
		                                         (ifNotFound :A => X, ifFound :A => X) :X =
			items match {
				case _ if pred(start) => ifFound(start)
				case it :Iterable[_] if it.isEmpty => ifNotFound(start)
				case seq :scala.collection.IndexedSeq[T] =>
					var i = seq.length; var last = start
					if (i <= IndexedSeqDefaults.defaultApplyPreferredMaxLength) {
						while ({ i -= 1; i >= 0 } && { last = op(seq(i), last); !pred(last) })
						{}
						if (i < 0) ifNotFound(last) else ifFound(last)
					} else {
						val it = seq.reverseIterator; var found = false
						while (it.hasNext && { last = op(it.next(), last); found = pred(last); !found })
						{}
						if (found) ifFound(last) else ifNotFound(last)
					}
				case _ =>
					val it = items match {
						case it :Iterable[T] => it.view //though unlikely, scanRight may be lazy and more efficient
						case _ => items.iterator to LazyList
					}
					val (skipped, rest) = it.scanRight(start)(op).to(LazyList).reverse.span(!pred(_))
					if (rest.nonEmpty) ifFound(rest.head)
					else ifNotFound(skipped.last)
//			case _ =>
//				val inverse = (List.empty[T] /: items)((reversal, item) => item::reversal)
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
		  * [[net.noresttherein.sugar.collections.extensions.FoldingMethods.foldRightWhile foldRightWhile]]`(start)(!pred(_))(op)`,
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
		  * [[net.noresttherein.sugar.collections.extensions.FoldingMethods.foldRightWhileOpt foldRightWhileOpt]]`(start)(!pred(_))(op)`,
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
		def foldRightUntilOpt[A](start :A)(pred :A => Boolean)(op :(T, A) => A) :Option[A] =
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
		  * [[net.noresttherein.sugar.collections.extensions.FoldingMethods.foldRightWhileEither foldRightWhileEither]]`(start)(!pred(_))(op)`,
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



		def foldWhile[A >: T](start :A)(pred :A => Boolean)(op :(A, A) => A) :A =
			foldLeftWhile(start)(pred)(op)

		def foldWhileOpt[A >: T](start :A)(pred :A => Boolean)(op :(A, A) => A) :Option[A] =
			foldLeftWhileOpt(start)(pred)(op)

		def foldWhileEither[A >: T](start :A)(pred :A => Boolean)(op :(A, A) => A) :Either[A, A] =
			foldLeftWhileEither(start)(pred)(op)


		private def foldLeftWhileAndReturn[A, X](start :A)(pred :A => Boolean)(op :(A, T) => A)
		                                        (ifNotFound :A => X, ifFound :A => X) :X =
			items match {
				case _ if !pred(start) => ifNotFound(start)
				case it :Iterable[_] if it.isEmpty => ifFound(start)
				case _ =>
					var last = start; var next = start; var found = false
					val i = items.iterator
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
		  * [[net.noresttherein.sugar.collections.extensions.FoldingMethods.foldLeftUntil foldLeftUntil]]`(start)(!pred(_))(op)`,
		  * as the latter returns the first element of `numbers.scanLeft(start)(op)` falsifying the predicate,
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
				_ => throw new IllegalArgumentException(
					"foldLeftWhile: starting value " + start + " does not satisfy the predicate."
				),
				identity)

		/** Folds this collection from left to right by applying `op` to its elements and the previously returned value
		  * while the folded value satisfies the condition `pred`.
		  * Function `op` is applied recursively to the elements of this collection in its iteration order
		  * as in regular [[scala.collection.IterableOps.foldLeft foldLeft]], but, additionally, predicate `pred`
		  * is tested for the current intermediate value before each iteration. If it is not satisfied,
		  * the last value satisfying it is returned. If `!pred(start)`, then `None`. The result is the same as in
		  * {{{
		  *     scanLeft(Some(start) :Option[A])(op).takeWhile(_.exists(pred)).lastOption
		  * }}}
		  * Note that this method is ''not'' equivalent to
		  * [[net.noresttherein.sugar.collections.extensions.FoldingMethods.foldLeftUntilOpt foldLeftUntilOpt]]`(start)(!pred(_))(op)`,
		  * as the latter returns the first element of `numbers.scanLeft(start)(op)` falsifying the predicate,
		  * while this method returns the last element for which it is still true.
		  * @param start the initial value, used as the result if the collection is empty.
		  * @param pred  a stop condition for folding, checked for the folded value at the beginning of every iteration.
		  * @param op    a folding function.
		  * @return the last folding result for which `pred` is true in a `Some`, or `None` if it is false for `start`.
		  */
		def foldLeftWhileOpt[A](start :A)(pred :A => Boolean)(op :(A, T) => A) :Option[A] =
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
		  * [[net.noresttherein.sugar.collections.extensions.FoldingMethods.foldLeftUntilEither foldLeftUntilEither]]`(start)(!pred(_))(op)`,
		  * as the latter returns the first element of `numbers.scanLeft(start)(op)` falsifying the predicate,
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
			items match {
				case _ if !pred(start) => ifNotFound(start)
				case it :Iterable[_] if it.isEmpty => ifFound(start)
				case seq :scala.collection.IndexedSeq[T] =>
					var last = start; var next = start
					var i = seq.length
					if (i <= IndexedSeqDefaults.defaultApplyPreferredMaxLength) {
						while (i > 0 && { i -= 1; next = op(seq(i), last); pred(next) })
							last = next
					} else {
						val it = seq.reverseIterator
						while (it.hasNext && { next = op(it.next(), last); pred(next) })
							last = next
					}
					ifFound(last)
				case _ =>
					val it = items match {
						case it :Iterable[T] => it.view //though unlikely, scanRight may be lazy and more efficient
						case _ => items.iterator to LazyList
					}
					val last = it.scanRight(start)(op).to(LazyList).reverse.takeWhile(pred).last
					ifFound(last)
	//			case _ =>
	//				var inverse = (List.empty[T] /: items)((list, item) => item::list)
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
		  * [[net.noresttherein.sugar.collections.extensions.FoldingMethods.foldRightUntil foldRightUntil]]`(start)(!pred(_))(op)`,
		  * as the latter returns the first value falsifying the predicate,
		  * while this method returns the last value for which it is still true.
		  * @param start the initial value, used as the result if the collection is empty.
		  * @param pred  a stop condition for folding, checked for the folded value at the beginning of every iteration.
		  * @param op    a folding function.
		  * @return the last element of the longest prefix of `scanLeft(start)(op)`
		  *         such that `pred` is `true` for all its elements.
		  */
		def foldRightWhile[A](start :A)(pred :A => Boolean)(op :(T, A) => A) :A =
			foldRightWhileAndReturn(start)(pred)(op)(
				_ => throw new IllegalArgumentException(
					"foldLeftWhile: starting value " + start + " does not satisfy the predicate."
				),
				identity
			)

		def foldRightWhileOpt[A](start :A)(pred :A => Boolean)(op :(T, A) => A) :Option[A] =
			foldRightWhileAndReturn(start)(pred)(op)(_ => None, Some.apply)

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
		  * @return [[net.noresttherein.sugar.collections.extensions.FoldingMethods.partialFoldLeft partialFoldLeft]]`(start)(op)`.
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
		def partialFoldLeft[A](start :A)(op :PartialFunction[(A, T), A]) :A = {
			val lift = op.lift
			foldLeftSome(start) { (acc, elem) => lift((acc, elem)) }
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
		def partialFoldRight[A](start :A)(op :PartialFunction[(T, A), A]) :A = {
			val lift = op.lift
			foldRightSome(start) { (elem, acc) => lift((elem, acc)) }
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
		  * @return [[net.noresttherein.sugar.collections.extensions.FoldingMethods.foldLeftSome foldLeftSome]]`(start)(op)`.
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
		def foldLeftSome[A](start :A)(op :(A, T) => Option[A]) :A = items match {
			case it :Iterable[_] if it.isEmpty => start
			case it :Iterator[_] if it.isEmpty => start
			case _ =>
				val it = items.iterator
				var last = start
				while (it.hasNext && (op(last, it.next()) match {
					case Some(next) => last = next; true
					case _ => false
				}))
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
		def foldRightSome[A](start :A)(op :(T, A) => Option[A]) :A = items match {
			case it :Iterable[_] if it.isEmpty => start
			case it :Iterator[_] if it.isEmpty => start
			case seq :scala.collection.IndexedSeq[T]
				if seq.length <= IndexedSeqDefaults.defaultApplyPreferredMaxLength =>
				var i = seq.length - 1; var last = start
				while (i >= 0 && (op(seq(i), last) match {
					case Some(next) => last = next; true
					case _ => false
				}))
					i -= 1
				last
			case seq :scala.collection.Seq[T] =>
				var last = start; var next = Option(start)
				val it = seq.reverseIterator.scanLeft(next)((opt, elem) => op(elem, opt.get))
				while (it.hasNext && { next = it.next(); next.isDefined })
					last = next.get
				last
			case _ =>
				val it = items match {
					case it :Iterable[T] => it to LazyList
					case _ => items.iterator to LazyList
				}
				val stream = it.reverse.scanLeft(Option(start))((opt, elem) => op(elem, opt.get))
					.takeWhile(_.isDefined)
				stream.last.get
		}



		def reduceUntil[A >: T](pred :A => Boolean)(op :(A, A) => A) :A = reduceLeftUntil(pred)(op)

		def reduceUntilOpt[A >: T](pred :A => Boolean)(op :(A, A) => A) :Option[A] = reduceLeftUntilOpt(pred)(op)


		private def reduceLeftUntilAndReturn[A >: T, X](pred :A => Boolean)(op :(A, T) => A)
		                                               (ifEmpty: => X, ifNotFound :A => X, ifFound :A => X) :X =
			items match {
				case it :Iterable[_] if it.isEmpty => ifEmpty
				case _ =>
					val i = items.iterator
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

		def reduceLeftUntil[A >: T](pred :A => Boolean)(op :(A, T) => A) :A =
			reduceLeftUntilAndReturn(pred)(op)(
				throw new UnsupportedOperationException("empty.reduceLeftUntil"),
				_ => throw new NoSuchElementException("Predicate not satisfied after reducing the whole collection."),
				identity
			)

		def reduceLeftUntilOpt[A >: T](pred :A => Boolean)(op :(A, T) => A) :Option[A] =
			reduceLeftUntilAndReturn(pred)(op)(None, _ => None, Some.apply)


		private def reduceRightUntilAndReturn[A >: T, X](pred :A => Boolean)(op :(T, A) => A)
		                                                (ifEmpty: => X, ifNotFound :A => X, ifFound :A => X) :X =
			items match {
				case it :Iterable[_] if it.isEmpty => ifEmpty
				case it :Iterator[_] if it.isEmpty => ifEmpty
				case seq :scala.collection.IndexedSeq[T] if seq.length <= IndexedSeqDefaults.defaultApplyPreferredMaxLength =>
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
					var inverse = (List.empty[T] /: items)((list, item) => item::list)
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

		def reduceRightUntil[A >: T](pred :A => Boolean)(op :(T, A) => A) :A =
			reduceRightUntilAndReturn(pred)(op)(
				throw new UnsupportedOperationException("empty.reduceRightUntil"),
				_ => throw new NoSuchElementException("Predicate not satisfied after reducing the whole collection."),
				identity
			)

		def reduceRightUntilOpt[A >: T](pred :A => Boolean)(op :(T, A) => A) :Option[A] =
			reduceRightUntilAndReturn(pred)(op)(None, _ => None, Some.apply)



		def partialReduce[A >: T](f :PartialFunction[(A, A), A]) :A =
			partialReduceLeft(f)

		def partialReduceLeft[A >: T](f :PartialFunction[(A, T), A]) :A = {
			val lift = f.lift
			reduceLeftSome[A]((acc, elem) => lift((acc, elem)))
		}

		def partialReduceRight[A >: T](f :PartialFunction[(T, A), A]) :A = {
			val lift = f.lift
			reduceRightSome[A]((elem, acc) => lift((elem, acc)))
		}


		def reduceSome[A >: T](f :(A, A) => Option[A]) :A = reduceLeftSome(f)

		def reduceLeftSome[A >: T](f :(A, T) => Option[A]) :A = {
			val it = items.iterator
			if (it.isEmpty)
				throw new UnsupportedOperationException("empty.reduceLeftSome")
			var last :A = it.next()
			while (it.hasNext && { val next = f(last, it.next()); next match {
				case Some(a) => last = a; true
				case _ => false
			}})
			{}
			last
		}

		def reduceRightSome[A >: T](f :(T, A) => Option[A]) :A = items match {
			case it :Iterable[_] if it.isEmpty =>
				throw new UnsupportedOperationException("empty.reduceRightSome")
			case seq :scala.collection.IndexedSeq[T] if seq.length <= IndexedSeqDefaults.defaultApplyPreferredMaxLength =>
				var last :A = seq.last
				var i = seq.length - 2
				while (i >= 0 && (f(seq(i), last) match {
					case Some(next) => last = next; i -= 1; true
					case _ => false
				}))
				{}
				last
			case seq :scala.collection.Seq[T] =>
				val it = seq.reverseIterator
				var last :A = it.next()
				while (it.hasNext && (f(it.next(), last) match {
					case Some(next) => last = next; true
					case _ => false
				}))
				{}
				last
			case _ =>
				var inverse = (List.empty[T] /: items.iterator)((list, item) => item::list)
				if (inverse.isEmpty)
					throw new UnsupportedOperationException("empty.reduceRightSome")
				var last :A = inverse.head; inverse = inverse.tail
				while (inverse.nonEmpty && (f(inverse.head, last) match {
					case Some(next) => last = next; inverse = inverse.tail; true
					case _ => false
				}))
				{}
				last
		}
	}




	/** Additional extension methods for collections of the standard library framework.
	  * The common theme is performing mapping with help of a passed state/accumulator value.
	  */
	class MappingMethods[C[X], E] private[extensions] (private val self :IterableOps[E, C, C[E]]) extends AnyVal {
		/** Maps this collection from left to right with an accumulating state updated by the mapping function.
		  * The state is discarded after the operation and only the mapping results (the second elements
		  * of the tuples returned by the given function) are returned in a collection of the same dynamic type
		  * as this collection.
		  */
		def mapWith[A, O](z :A)(f :(E, A) => (O, A)) :C[O] =
			self.view.scanLeft((null.asInstanceOf[O], z)) { //safe because null is never passed to f and we are in an erased context
				(acc, e) => f(e, acc._2)
			}.tail.map(_._1).to(self.iterableFactory)

		/** Flat maps this collection from left to right with an accumulating state updated by the mapping function.
		  * The state is discarded after the operation and only the mapping results (the collections returned by
		  * by the given function) are returned in a collection of the same dynamic type as this collection.
		  */
		def flatMapWith[A, O](z :A)(f :(E, A) => (IterableOnce[O], A)) :C[O] =
			self.view.scanLeft((Nil :IterableOnce[O], z)) {
				(acc, e) => f(e, acc._2)
			}.flatMap(_._1).to(self.iterableFactory)

		/** Maps this collection in order consistent with `foreach`, passing as the first argument the index
		  * of the mapped element.
		  */
		def mapWithIndex[O](f :(E, Int) => O) :C[O] = {
			var i = 0
			val b = self.iterableFactory.newBuilder[O]
			b sizeHint self
			self foreach { e => b += f(e, i); i += 1 }
			b.result()
		}

		/** Flat maps this collection in order consistent with `foreach`, passing as the first argument the index
		  * of the mapped element in this collection (that is, the number of elements processed before it).
		  */
		def flatMapWithIndex[O](f :(E, Int) => IterableOnce[O]) :C[O] = {
			var i = 0
			val b = self.iterableFactory.newBuilder[O]
			self foreach { e => b ++= f(e, i); i += 1 }
			b.result()
		}

		/** Iterates over the collection, passing the index of the current element to the given function.
		  * Note that in collections with an undefined order, this index applies only to this particular iteration,
		  * rather than some absolute position.
		  */
		def foreachWithIndex[O, U](f :(E, Int) => U) :Unit = {
			var i = 0
			self foreach { e => f(e, i); i += 1 }
		}


		/** Zips this collection with another one and maps the result in one step.
		  * No intermediate collection is created, and the mapping function accepts two arguments rather than a tuple,
		  * making it more convenient to use with placeholder parameters.
		  */
		def zipMap[X, O](that :IterableOnce[X])(f :(E, X) => O) :C[O] = {
			val l = self.iterator
			val r = that.iterator
			val res = self.iterableFactory.newBuilder[O]
			res sizeHint self
			while (l.hasNext && r.hasNext)
				res += f(l.next(), r.next())
			res.result()
		}

		/** Equivalent to [[net.noresttherein.sugar.collections.extensions.MappingMethods.zipMap zipMap]],
		  * but throws a [[NoSuchElementException]] if the collections are of different sizes.
		  */
		def zipMapAll[X, O](that :IterableOnce[X])(f :(E, X) => O) :C[O] = {
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
		def zipMapAll[X, O](that :IterableOnce[X], thisElem :E, thatElem :X)(f :(E, X) => O) :C[O] = {
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
		def zipFlatMap[X, O](that :IterableOnce[X])(f :(E, X) => IterableOnce[O]) :C[O] = {
			val l = self.iterator
			val r = that.iterator
			val res = self.iterableFactory.newBuilder[O]
			while (l.hasNext && r.hasNext)
				res ++= f(l.next(), r.next())
			res.result()
		}

		/** Equivalent to [[net.noresttherein.sugar.collections.extensions.MappingMethods.zipFlatMap zipFlatMap]],
		  * but throws a [[NoSuchElementException]] if the collections are of different sizes.
		  */
		def zipFlatMapAll[X, O](that :IterableOnce[X])(f :(E, X) => IterableOnce[O]) :C[O] = {
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
		def zipFlatMapAll[X, O](that :IterableOnce[X], thisElem :E, thatElem :X)(f :(E, X) => IterableOnce[O]) :C[O] = {
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

		/** Equivalent to `this.zip(that).forall { case (a, b) => p(a, b) }`, but does not build an intermediate
		  * collection of tuples and accepts a two argument function rather than a function of tuple, which makes
		  * it more convenient to use with the lambda parameter placeholder syntax. Additionally, it requires
		  * that both collections are of the same size, returning `false` if it is not true.
		  */
		def zipForAll[X](that :IterableOnce[X])(p :(E, X) => Boolean) :Boolean = {
			val l = self.iterator
			val r = that.iterator
			var wasTrue = true
			while (l.hasNext && r.hasNext && { wasTrue = p(l.next(), r.next()); wasTrue })
			{}
			wasTrue && !l.hasNext && !r.hasNext
		}


		/** Maps the elements of the collection and reverses their order. The order in which the mapping function
		  * will be applied to the elements is undefined and depends on the runtime type of this collection.
		  * Note that if this collection is unordered, the order of the elements in the mapped collection
		  * is likewise undefined and depends on the implementation of this collection's builder.
		  * This operation is faster than `this.map(f).reverse`.
		  */
		def mapReverse[O](f :E => O) :C[O] = self match {
			case _ if self.isEmpty =>
				self.iterableFactory.empty
			case list :List[E] =>
				@tailrec def mapList(unmapped :List[E], mapped :List[O]) :List[O] = unmapped match {
					case h::t => mapList(t, f(h)::mapped)
					case _ => mapped
				}
				mapList(list, Nil).asInstanceOf[C[O]]
			case list :LinearSeq[E] =>
				@tailrec def mapLinear(unmapped :LinearSeq[E], mapped :LinearSeq[O]) :LinearSeq[O] =
					if (unmapped.isEmpty) mapped
					else mapLinear(unmapped.tail, f(unmapped.head) +: mapped)
				mapLinear(list, list.iterableFactory.empty).asInstanceOf[C[O]]
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

	}




	/** Extension methods of mutable and immutable sequences (and arrays through a wrapper):
	  *   1. alternative, safer implementations of [[scala.collection.SeqOps.indexOf indexOf]],
	  *      which do not return a negative index when the element is not found;
	  *   1. methods related to subsequences: sequences containing selected elements from another sequence,
	  *      in the same order.
	  */
	class SeqExtension[X] private[extensions] (private val self :scala.collection.Seq[X]) extends AnyVal {
		@inline def length :Int = self.length

		/** Finds the location of the given element in this sequence, returning its index as an option.
		  * @param x    the element, whose index is to be determined.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def getIndexOf(x :X, from :Int = 0) :Opt[Int] = self.indexOf(x, from) match {
			case n if n >= 0 => Got(n)
			case _ => Lack
		}
		/** Finds the last location of the given element in this sequence, returning its index as an option.
		  * @param x   the element, whose index is to be determined.
		  * @param end the upper, inclusive bound on the returned index.
		  */
		@inline def getLastIndexOf(x :X, end :Int = length - 1) :Opt[Int] = self.lastIndexOf(x, end) match {
			case n if n >= 0 => Got(n)
			case _ => Lack
		}
		/** Finds an element of this sequence which satisfies the predicate, returning its index as an option.
		  * @param p    a function applied consecutively to all elements with indices greater or equal `from`.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def getIndexWhere(p :X => Boolean, from :Int = 0) :Opt[Int] = self.indexOf(p, from) match {
			case n if n >= 0 => Got(n)
			case _ => Lack
		}
		/** Finds the last element of this sequence which satisfies the predicate, returning its index as an option.
		  * @param p   a function applied consecutively to all elements with indices lesser or equal `end`,
		  *            in a decreasing order.
		  * @param end the upper, inclusive bound on the returned index; elements after this position will not be checked.
		  */
		@inline def getLastIndexWhere(p :X => Boolean, end :Int = length - 1) :Opt[Int] =
			self.lastIndexWhere(p, end) match {
				case n if n >= 0 => Got(n)
				case _ => Lack
			}

		/** Finds the location of the given element in this sequence, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param x    the element, whose index is to be determined.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def sureIndexOf(x :X, from :Int = 0) :Int = self.indexOf(x, from) match {
			case n if n >= 0 => n
			case _ => throw new NoSuchElementException(indexOfErrorMessage(x, from))
		}
		/** Finds the last location of the given element in this sequence, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param x   the element, whose index is to be determined.
		  * @param end the upper, inclusive bound on the returned index.
		  */
		@inline def sureLastIndexOf(x :X, end :Int = length - 1) :Int = self.lastIndexOf(x, end) match {
			case n if n >= 0 => n
			case _ => throw new NoSuchElementException(lastIndexOfErrorMessage(x, end))
		}
		/** Finds an element of this sequence which satisfies the predicate, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param p    a function applied consecutively to all elements with indices greater or equal `from`,
		  *             until satisfied.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def sureIndexWhere(p :X => Boolean, from :Int = 0) :Int = self.indexWhere(p, from) match {
			case n if n >= 0 => n
			case _ => throw new NoSuchElementException(indexWhereErrorMessage(from))
		}
		/** Finds the last element of this sequence which satisfies the predicate, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param p   a function applied consecutively to all elements, starting with the one at position `end`,
		  *            until satisfied.
		  * @param end the upper, inclusive bound on the returned index; elements after this index are not checked.
		  */
		@inline def sureLastIndexWhere(p :X => Boolean, end :Int = length - 1) :Int =
			self.lastIndexWhere(p, end) match {
				case n if n >= 0 => n
				case _ => throw new NoSuchElementException(lastIndexWhereErrorMessage(end))
			}

		/** Finds the location of the given element in this sequence, throwing an exception `E`,
		  * given as the type parameter, if it does not exist.
		  * @tparam E   the exception to be thrown; it must contain either `(String, Throwable)` or `(String)`
		  *             constructor.
		  * @param x    the element, whose index is to be determined.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def indexOfOrThrow[E <: Exception :ClassTag](x :X, from :Int = 0) :Int =
			self.indexOf(x, from) match {
				case n if n >= 0 => n
				case _ => raise[E](indexOfErrorMessage(x, from))
			}
		/** Finds the last location of the given element in this sequence, throwing an exception `E`,
		  * given as the type parameter, if it does not exist.
		  * @tparam E  the exception to be thrown; it must contain either `(String, Throwable)` or `(String)`
		  *            constructor.
		  * @param x   the element, whose index is to be determined.
		  * @param end the upper, inclusive bound on the returned index.
		  */
		@inline def lastIndexOfOrThrow[E <: Exception :ClassTag](x :X, end :Int = length - 1) :Int =
			self.lastIndexOf(x, end) match {
				case n if n >= 0 => n
				case _ => raise[E](lastIndexOfErrorMessage(x, end))
			}
		/** Finds an element of this sequence which satisfies the predicate, throwing an exception `E`,
		  * given as the type parameter, if it does not exist.
		  * @tparam E   the exception to be thrown; it must contain either `(String, Throwable)` or `(String)`
		  *             constructor.
		  * @param p    a function applied consecutively to all elements with indices greater or equal `from`,
		  *             until satisfied.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def indexWhereOrThrow[E <: Exception :ClassTag](p :X => Boolean, from :Int = 0) :Int =
			self.indexWhere(p, from) match {
				case n if n >= 0 => n
				case _ => raise[E](indexWhereErrorMessage(from))
			}
		/** Finds the last element of this sequence which satisfies the predicate, throwing an exception `E`,
		  * given as the type parameter, if it does not exist.
		  * @tparam E  the exception to be thrown; it must contain either `(String, Throwable)` or `(String)`
		  *            constructor.
		  * @param p   a function applied consecutively to all elements, starting with the one at position `end`,
		  *            until satisfied.
		  * @param end the upper, inclusive bound on the returned index; elements after this index are not checked.
		  */
		@inline def lastIndexWhereOrThrow[E <: Exception :ClassTag](p :X => Boolean, end :Int = length - 1) :Int =
			self.lastIndexWhere(p, end) match {
				case n if n >= 0 => n
				case _ => raise[E](lastIndexWhereErrorMessage(end))
			}

		/** Returns `this.indexOf(x)`, adding an assertion that the result is not negative (the element has been found).
		  * @param x    an element of this collection.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertIndexOf(x :X, msg: => String) :Int = assertPresent(self.indexOf(x), msg)
		/** Returns `this.indexOf(x, from)`, adding an assertion that the result is not negative
		  * (the element has been found).
		  * @param x    an element of this collection.
		  * @param from an inclusive lower bound on the index of the searched element.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertIndexOf(x :X, from :Int, msg: => String) :Int = assertPresent(self.indexOf(x, from), msg)
		/** Returns `this.lastIndexOf(x)`, adding an assertion that the result is not negative
		  * (the element has been found).
		  * @param x    an element of this collection.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertLastIndexOf(x :X, msg: => String) :Int = assertPresent(self.lastIndexOf(x), msg)
		/** Returns `this.lastIndexOf(x, end)`, adding an assertion that the result is not negative
		  * (the element has been found).
		  * @param x    an element of this collection.
		  * @param end  an inclusive upper bound on the index of the searched element.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertLastIndexOf(x :X, end :Int, msg: => String) :Int = assertPresent(self.lastIndexOf(x, end), msg)
		/** Returns `this.indexWhere(p)`, adding an assertion that the result is not negative (a satisfying element
		  * has been found).
		  * @param p   a function applied consecutively to all elements, in the increasing order of indices.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertIndexWhere(p :X => Boolean, msg: => String) :Int = assertPresent(self.indexWhere(p), msg)
		/** Returns `this.indexWhere(p, from)`, adding an assertion that the result is not negative (a satisfying element
		  * has been found).
		  * @param p    a function applied consecutively to all elements, starting with index `from`,
		  *             until satisfied.
		  * @param from an index from which to start checking; elements if lower indices are not considered.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertIndexWhere(p :X => Boolean, from :Int, msg: => String) :Int =
			assertPresent(self.indexWhere(p, from), msg)
		/** Returns `this.lastIndexWhere(p)`, adding an assertion that the result is not negative (a satisfying element
		  * has been found).
		  * @param p   a function applied consecutively to all elements in a decreasing order of indices,
		  *            starting with the one at position `end`, until satisfied.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertLastIndexWhere(p :X => Boolean, msg: => String) :Int =
			assertPresent(self.lastIndexWhere(p), msg)
		/** Returns `this.lastIndexWhere(p, end)`, adding an assertion that the result is not negative
		  * (a satisfying element has been found).
		  * @param p   a function applied consecutively to all elements in a decreasing order of indices,
		  *            starting with the one at position `end`, until satisfied.
		  * @param end the upper, inclusive bound on the returned index; elements after this index are not checked.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertLastIndexWhere(p :X => Boolean, end :Int, msg: => String) :Int =
			assertPresent(self.lastIndexWhere(p, end), msg)

		@inline private[collections] def assertPresent(i :Int, msg: => String) :Int = {
			assert(i >= 0, msg)
			i
		}

		private[collections] def indexOfErrorMessage(x :X, from :Int) :String =
			"No " + x + " in " + self + (if (from == 0) "." else " at or after index " + from + ".")

		private[collections] def lastIndexOfErrorMessage(x :X, end :Int) :String =
			"No " + x + " in " + self + (if (end == length - 1) "." else " at or before index " + end + ".")

		private[collections] def indexWhereErrorMessage(from :Int) :String =
			"No element satisfying the predicate in " + self +
				(if (from == 0) "." else " at or after index " + from + ".")

		private[collections] def lastIndexWhereErrorMessage(end :Int) :String =
			"No element satisfying the predicate in " + self +
				(if (end == length - 1) "." else " at or before index " + end + ".")


		/** Checks if this sequence is a subsequence of `other`, that is there is a function `f :Int => Boolean` such that
		  * `this == other.zipWithIndex.collect { case (e, i) if f(i) => e }`.
		  */
		def subseqOf(other :scala.collection.Seq[X]) :Boolean = {
			val thisLength = self.knownSize
			val thatLength = other.knownSize
			@tailrec def sublistOf(left :scala.collection.Seq[X], right :scala.collection.Seq[X]) :Boolean =
				left.isEmpty || right.nonEmpty && (
					if (left.head == right.head) sublistOf(left.tail, right.tail)
					else sublistOf(left, right.tail)
				)
			@tailrec def indexedSubseqOf(left :scala.collection.Seq[X], leftIdx :Int,
			                             right :scala.collection.Seq[X], rightIdx :Int) :Boolean =
				leftIdx < 0 || rightIdx >= 0 && (
					if (left(leftIdx) == right(rightIdx)) indexedSubseqOf(left, leftIdx - 1, right, rightIdx - 1)
					else indexedSubseqOf(left, leftIdx, right, rightIdx - 1)
				)
			(thatLength < 0 || thisLength <= thatLength && thisLength >= 0) &&
				((self, other) match {
					case (_ :scala.collection.IndexedSeq[_], _ :scala.collection.IndexedSeq[_]) =>
						indexedSubseqOf(self, thisLength - 1, other, thatLength - 1)
					case (_:LinearSeq[_] | _:Vector[_] | _:PassedArray[_], _:LinearSeq[_] | _:Vector[_] | _:PassedArray[_]) =>
						sublistOf(self, other)
					case (_ :LinearSeq[_], _) =>
						sublistOf(self, other.toList)
					case (_, _ :LinearSeq[_]) =>
						sublistOf(self.toList, other)
					case (_, _) =>
						indexedSubseqOf(
							self to scala.collection.IndexedSeq, thisLength - 1,
							other to scala.collection.IndexedSeq, thatLength - 1
						)
			})
		}

		/** Checks if this sequence is a subsequence of `other`, that is there is a function `f :Int => Boolean` such that
		  * `this == other.zipWithIndex.collect { case (e, i) if f(i) => e }`.
		  */
		def subseqOf[U >: X](other :Array[U]) :Boolean =
			new SeqExtension[U](self).subseqOf(ArraySeq.unsafeWrapArray(other))
	}




	/** Adds an [[net.noresttherein.sugar.collections.extensions.MapExtension.updatedIfAbsent updatedIfAbsent]] extension method
	  * to [[scala.collection.immutable.Map immutable.Map]].
	  */
	class MapExtension[K, V, M[A, +B] <: MapOps[A, B, M, M[A, B]]] private[extensions] (private val self :M[K, V])
		extends AnyVal 
	{
		@inline def updatedIfAbsent[U >: V](key :K, value :U) :M[K, U] =
			if (self.contains(key)) self else self.updated(key, value)

		@inline def ?=[U >: V](entry :(K, U)) :M[K, U] = updatedIfAbsent(entry._1, entry._2)
	}


	class PreferredStepperShape[A, S <: Stepper[_]] private[extensions] (val stepperShape :StepperShape[A, S])
		extends AnyVal
	{
		def shape :StepperShape.Shape = stepperShape.shape
	}

	private[extensions] sealed abstract class Rank1PreferredStepperShapes {
		implicit def lessPreferredStepperShape[A, S <: Stepper[_]](implicit shape :StepperShape[A, S]) =
			new PreferredStepperShape[A, S](shape)
	}
	object PreferredStepperShape extends Rank1PreferredStepperShapes {
		implicit val IntStepperShape    = new PreferredStepperShape[Int, IntStepper](StepperShape.intStepperShape)
		implicit val LongStepperShape   = new PreferredStepperShape[Long, LongStepper](StepperShape.longStepperShape)
		implicit val DoubleStepperShape = new PreferredStepperShape[Double, DoubleStepper](StepperShape.doubleStepperShape)
	}

	/** Adds a `++` extension method to any `Stepper[_]`. */
	class StepperExtension[S <: Stepper[_]] private[extensions] (private val self :S) extends AnyVal {
		/** Combines this stepper with the argument, returning a stepper of the same specialization as both.
		  * This will work only for the built in Scala steppers.
		  */
		def ++[A](next :S)(implicit shape :PreferredStepperShape[A, S]) :S =
			ConcatStepper(self, next)(shape.stepperShape)
	}

	/** Adds a `++` extension method to any Java `Iterator[_]`. */
	class JavaIteratorExtension[I <: JIterator[_]] private[extensions] (private val self :I) extends AnyVal {
		/** Combines this iterator with the argument, returning an iterator of the same specialization as both.
		  * Aside from the basic `Iterator[_]`, this will work only for the standard three Java `PrimitiveIterator`s.
		  */
		def ++[A](next :I)(implicit shape :JavaIteratorShape[A, I]) :I =
			JavaConcatIterator(self, next)
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
		def iterateWithIndex[X](start :X, len :Int)(f :(X, Int) => X) :C[X] =
			companion.from(View.iterate((start, 0), len) { xi => (f(xi._1, xi._2 + 1), xi._2 + 1) }.map(_._1))
	}


	/** Extensions methods for object [[Array]] (the array factory). Adds the same methods as
	  * [[net.noresttherein.sugar.collections.extensions.IterableFactoryExtension IterableFactoryExtension]]
	  * as well as missing methods from [[scala.collection.SeqFactory SeqFactory]]
	  * and some adapters of methods in [[java.util.Arrays]].
	  */
	sealed trait ArrayFactoryExtension extends Any {
		/** Same as [[java.util.Arrays Arrays]]`.`[[java.util.Arrays.copyOfRange copyOfRange]], except it works
		  * for all element types.
		  */
		def copyOfRange[T](original :Array[T], from :Int, to :Int) :Array[T] = ((original: @unchecked) match {
			case _        :Array[BoxedUnit] => //need to check before `AnyRef`
				if (from < 0 | to < 0)
					throw new IndexOutOfBoundsException(
						"copyOfRange[Unit](Array[" + original.length + "], " + from + ", " + to + ")"
					)
				val result = new Array[Unit](to - from)
				java.util.Arrays.fill(result.asInstanceOf[Array[AnyRef]], ())
				result
			case refs     :Array[AnyRef]    => Arrays.copyOfRange(refs, from, to)
			case ints     :Array[Int]       => Arrays.copyOfRange(ints, from, to)
			case longs    :Array[Long]      => Arrays.copyOfRange(longs, from, to)
			case doubles  :Array[Double]    => Arrays.copyOfRange(doubles, from, to)
			case chars    :Array[Char]      => Arrays.copyOfRange(chars, from, to)
			case bytes    :Array[Byte]      => Arrays.copyOfRange(bytes, from, to)
			case floats   :Array[Float]     => Arrays.copyOfRange(floats, from, to)
			case booleans :Array[Boolean]   => Arrays.copyOfRange(booleans, from, to)
			case shorts   :Array[Short]     => Arrays.copyOfRange(shorts, from, to)
		}).asInstanceOf[Array[T]]

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
		def iterateWithIndex[X](start :X, len :Int)(f :(X, Int) => X)(implicit elements :ClassTag[X]) :Array[X] =
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
		def unfold[A, S](init :S)(f :S => Option[(A, S)])(implicit elements :ClassTag[A]) :Array[A] =
			Array.from(new View.Unfold(init)(f))
	}

	/** Adds a [[net.noresttherein.sugar.collections.extensions.IteratorFactoryExtension.double double]] factory method
	  * for two element iterators to object `Iterator` to complement [[scala.collection.Iterator.single single]].
	  */
	sealed trait IteratorFactoryExtension extends Any {
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
	}

	/** Adds factory methods to the [[scala.collection.Stepper$ Stepper]] singleton object, creating steppers
	  * of zero, one or two elements.
	  */
	sealed trait StepperFactoryExtension extends Any {
		/** Creates an empty `Stepper`. This method variant requires either explicit specification of type parameters,
		  * or for the element type to be abstract, with a single implicit `StepperShape[T, S]` in scope.
		  * @see [[net.noresttherein.sugar.collections.extensions.StepperFactoryExtension.apply[T] apply]]`()`.
		  */
		@inline final def empty[T, S <: Stepper[_]](implicit shape :StepperShape[T, S]) :S with EfficientSplit =
			Stepper0()
		
		/** Creates an empty stepper with the specified element type. 
		  * The returned object has an `apply()` method accepting an implicit 
		  * [[scala.collection.StepperShape StepperShape]]`[T, S]`; this allows to split type parameter groups
		  * and provide here only the element type: `Stepper[T]()`.
		  * @see [[net.noresttherein.sugar.collections.extensions.StepperFactoryExtension.empty empty]]
		  */
		@inline final def apply[T] = new EmptyStepperFactoryExtension[T] {} 
		
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
		@inline def ofDouble(first :Double, second :Double) :DoubleStepper with EfficientSplit = Stepper2.ofDouble(first, second)
	}

	/** An `apply()` method acceptin an implicit `StepperShape[T, S]`, inferring the stepper type. It is a continuation
	  *  of [[net.noresttherein.sugar.collections.extensions.StepperFactoryExtension.apply Stepper]]`[T]` call.
	  */
	sealed trait EmptyStepperFactoryExtension[T] extends Any {
		/** Creates an empty `Stepper` of shape defined by an implicit `StepperShape` for element type `T`. */
		@inline final def apply[S <: Stepper[_]]()(implicit shape :StepperShape[T, S]) :S with EfficientSplit =
			Stepper0()	
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
	}


	/** Adds Scala [[scala.collection.mutable.Growable Growable]] and [[scala.collection.mutable.Builder Builder]]
	  * methods as inlined delegates to the Java [[java.lang.StringBuilder StringBuilder]].
	  * While it essentially duplicates the functionality of standard Scala
	  * [[scala.collection.mutable.StringBuilder StringBuilder]], the wrapper is not intended to be as an object
	  * or referred to directly by the application, but rather a provider of extension methods, and returns always
	  * the original `StringBuilder`. As methods are inlined, this incurs neither the penalty of creating an additional
	  * object, nor of delegating individual calls, at least with default compiler optimisations on.
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

}






private[collections] class ComposedBuilder[-X, Y, C, +R](underlying :Builder[Y, C], mapArgs :X => Y, mapRes :C => R)
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
