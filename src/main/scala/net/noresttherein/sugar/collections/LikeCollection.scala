package net.noresttherein.sugar.collections

import java.util.Spliterator

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{AnyStepper, DoubleStepper, IntStepper, IterableOnceOps, LongStepper, Stepper, StepperShape}
import scala.collection.mutable.{ArrayBuffer, Buffer, Builder}
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.reflect.{ClassTag, classTag}

import net.noresttherein.sugar.JavaTypes.JCollection
import net.noresttherein.sugar.arrays.{ArrayFactory, IArray, IRefArray, RefArray}
import net.noresttherein.sugar.casting.{cast2TypeParamsMethods, castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.collections.LikeCollection.{LikeCollectionBasics, LikeMoreSpecific}
import net.noresttherein.sugar.collections.extensions.{IterableOnceExtension, SeqFactoryExtension, StepperCompanionExtension}
import net.noresttherein.sugar.collections.util.nothingToCopy
import net.noresttherein.sugar.exceptions.{outOfBounds_!, unsupported_!}
import net.noresttherein.sugar.extensions.IteratorExtension
import net.noresttherein.sugar.funny.extensions.PartialFunctionExtension
import net.noresttherein.sugar.illegalState_!
import net.noresttherein.sugar.reflect.extensions.classNameMethods
import net.noresttherein.sugar.typist.kinds
import net.noresttherein.sugar.typist.kinds.Any1
import net.noresttherein.sugar.vars.Maybe.{No, Yes}
import net.noresttherein.sugar.vars.{Maybe, Opt}
import net.noresttherein.sugar.vars.Opt.One




/** A type class for collection types, meant to abstract over collections not implementing any interface
  * from the standard collection library, such as `String`, but also single elements. It duplicates
  * some of the basic methods of [[collection.IterableOnceOps IterableOnceOps]], but also defines methods which reverse
  * the responsibility of adding `Xs` to another collection. It is in this similar to
  * [[collection.generic.IsIterableOnce IsIterableOnce]] family of classes, but does not wrap the type
  * in an `Iterable` adapter. This allows unbounded polymorphism without an overhead -
  * for example, methods `addOne(x :X)` and `addAll(x :IterableOnce[X])` can share the same implementation:
  * {{{
  *     def add[CC[_], C](x :C)(implicit multiple :LikeCollection[X, CC, C])
  * }}}
  * Furthermore, it lies between the minimal `IterableOnce` interface and full `IterableOnceOps` and focuses
  * on inspecting and traversing the collection, without providing an interface for manipulating it
  * or creating other collections. In particular, it may describe any component containing subcomponents,
  * even if both their number and character is predetermined. In this sense, it may even not be a 'collection',
  * because it may be unable to contain even other instances, let alone types, of elements.
  * Additionally, lack of methods for building a new collection makes it possible to create a type class
  * with a different element type `X` than the enriched collection `Xs`, working like `_.view.map()`.
  * Subclasses extend this API for methods mirroring those from major Scala collections.
  *
  * Because Scala is an object-oriented language with a high degree of type polymorphism,
  * but type classes like this one are resolved statically based on the type of the object reference,
  * it may happen that a more specific implementation of this class (for example,
  * [[net.noresttherein.sugar.collections.LikeIterable LikeIterable]] is available for a particular instance.
  * While the implementation in `LikeCollection` is required to work correctly for all instances
  * of the enriched type, those specializations may offer more efficient implementations of some methods.
  * Therefore, if the user is interested only in handling that specific object, in particular for many operations,
  * or when a particular operation is known to have a particular performance boost for some types,
  * it may be worthwhile to swap an instance of `LikeCollection[X, Xs]`, for a hopefully more suitable one.
  * This can be done by two methods:
  *   1. [[net.noresttherein.sugar.collections.LikeCollection.moreSpecific moreSpecific]]
  *      returns a [[net.noresttherein.sugar.vars.Maybe Maybe]] containing a more specific instance, if known;
  *   1. [[net.noresttherein.sugar.collections.LikeCollection.specific specific]] always returns the best known instance
  *      of the same type class and is defined universally as `moreSpecific(elems) getOrElse this`.
  *
  * Additionally, some implementations have that check built into all or some of their methods,
  * and they always look up for a better instance for an argument collection. This can be achieved by mixing in
  * [[net.noresttherein.sugar.collections.LikeCollection.LikeMoreSpecific LikeMoreSpecific]] or one of its more specific
  * subtraits, named the same way, but located in companion objects to subtraits of `LikeCollection`.
  * @see [[net.noresttherein.sugar.collections.LikeIterableOnce LikeIterableOnce]]
  * @tparam X  the type of stored elements.
  * @tparam Xs the type of the collection-like objects on which this type class operates.
  * @author Marcin Mościcki
  * @define coll collection
  */ //An alternative name: LikeAnyOf; LikeContainer;Contains.
//Consider: swapping the order of arguments, so it may be written as an infix type naturally.
trait LikeCollection[+X, -Xs] extends Serializable {
	/* We can't simply cast Xs to IterableOnceOps, Iterable, etc., because we don't know if its element type
	 * is actually X (for example, SingleValue[Seq[X]] extends LikeCollection[Seq[X], Seq[X]]),
	 *let alone the collection type.
	 */

	/** Tests whether `elems` can be repeatedly traversed.
	  * @param elems a $coll of this type class.
	  * @return `true` if [[net.noresttherein.sugar.collections.LikeCollection.iterator iterator]]
	  *         returns a new iterator with every call.
	  */
	def isTraversableAgain(elems :Xs) :Boolean

	/** @param elems a $coll of this type class.
	  * @return The number of elements in `elems`, if it can be cheaply computed,
	  *         -1 otherwise. Cheaply usually means: Not requiring a collection traversal.
	  */
	def knownSize(elems :Xs) :Int

	/** The size of `elems`.
	  * @param elems a $coll of this type class.
	  * @return the number of elements in `elems`.
	  */
	def size(elems :Xs) :Int

	/** Tests whether the $coll is empty.
	  * @param elems a $coll of this type class.
	  * @return `true` if the $coll contains no elements, `false` otherwise.
	  */
	def isEmpty(elems :Xs) :Boolean

	/** Tests whether the $coll is not empty.
	  * @param elems a $coll of this type class.
	  * @return `true` if the $coll contains at least one element, `false` otherwise.
	  */
	@inline final def nonEmpty(elems :Xs) :Boolean = !isEmpty(elems)

	/** Tests whether a predicate holds for all elements of `elems`.
	  *
	  * $mayNotTerminateInf
	  * @param elems a $coll.
	  * @param p     the predicate used to test elements.
	  * @return      `true` if `elems` is empty or the given predicate `p`
	  *               holds for all elements of `elems`, otherwise `false`.
	  */
	def forall(elems :Xs)(p :X => Boolean) :Boolean

	/** Tests whether a predicate holds for at least one element of `elems`.
	  *
	  * $mayNotTerminateInf
	  * @param elems a $coll.
	  * @param p     the predicate used to test elements.
	  * @return  `true` if the given predicate `p` is satisfied by at least one element of `elems`, otherwise `false`.
	  */
	def exists(elems :Xs)(p :X => Boolean) :Boolean

	/** Counts the number of elements in the $coll which satisfy a predicate.
	  *
	  * $willNotTerminateInf
	  * @param elems a $coll.
	  * @param p     the predicate  used to test elements.
	  * @return      the number of elements satisfying the predicate `p`.
	  */
	def count(elems :Xs)(p :X => Boolean) :Int

	/** Finds the first element of the $coll satisfying a predicate, if any.
	  *
	  * $mayNotTerminateInf
	  * $orderDependent
	  *
	  * @param elems a $coll.
	  * @param p     the predicate used to test elements.
	  * @return an option value containing the first element in the $coll that satisfies `p`, or `None` if none exists.
	  */
	def find(elems :Xs)(p :X => Boolean) :Option[X]

	/** Finds the first element of the $coll for which the given partial
	  * function is defined, and applies the partial function to it.
	  *
	  * $mayNotTerminateInf
	  * $orderDependent
	  * @tparam A    The result type of the function pf.
	  * @param elems a $coll.
	  * @param pf    the partial function
	  * @return an option value containing pf applied to the first
	  *         value for which it is defined, or `None` if none exists.
	  * @example `Seq("a", 1, 5L).collectFirst({ case x: Int => x*10 }) = Some(10)`
	  */
	def collectFirst[A](elems :Xs)(pf :PartialFunction[X, A]) :Option[A]

	/** Applies a binary operator to a start value and all elements of `elems`, going left to right.
	  * @param elems a $coll of this type class.
	  * @param z     the start  value.
	  * @param op    the binary operator.
	  * @return the result of inserting `op` between consecutive elements of `elems`, going left to right
	  *         with the start value `z` on the left: `op(...op(z, x,,1,,), x,,2,,, ..., x,,n,,)`
	  *         where `x,,1,,, ..., x,,n,,` are the elements of `elems`. Returns `z` if `elems` is empty.
	  */
	def foldLeft[A](elems :Xs)(z :A)(op :(A, X) => A) :A

	/** Applies a binary operator to all elements of `elems`,
	  * going left to right.
	  * @param elems a $coll of this type class.
	  * @param op    a binary operator.
	  * @return the result of inserting `op` between consecutive elements of `elems`, going left to right:
	  *         `op( op( ... op(x,,1,,, x,,2,,) ..., x,,n-1,,), x,,n,,)` where `x,,1,,, ..., x,,n,,`
	  *         are the elements of `elems`.
	  * @throws UnsupportedOperationException if `elems` is empty.
	  */
	def reduceLeft[U >: X](elems :Xs)(op :(U, X) => U) :U

	/** Optionally applies a binary operator to all elements of `elems`, going left to right.
	  * @param elems a $coll of this type class.
	  * @param op    a binary operator.
	  * @return an option value containing the result of `reduceLeft(op)` if `elems` is nonempty, `None` otherwise.
	  */ //consider: variant for Opt
	def reduceLeftOption[U >: X](elems :Xs)(op :(U, X) => U) :Option[U]

	/** Tests whether every element of this collection's iterator relates to the
	  * corresponding element of another collection by satisfying a test predicate.
	  *
	  * $willNotTerminateInf
	  * @tparam A    the type of the elements of `that`.
	  * @tparam O    the second collection type.
	  * @param elems a $coll.
	  * @param that  the other collection
	  * @param p     the test predicate, which relates elements from both collections
	  * @return `true` if both collections have the same length and
	  *         `p(x, y)` is `true` for all corresponding elements `x` of this iterator
	  *         and `y` of `that`, otherwise `false`
	  */
	def corresponds[A, O](elems :Xs, that :O)(p :(X, A) => Boolean)
	                     (implicit likeCollection :LikeCollection[A, O]) :Boolean

	/** Apply `f` to each element for its side effects
	  * @param elems a $coll of this type class.
	  * @param f     a function to apply to each element in `elems`.
	  */
	def foreach[U](elems :Xs)(f :X => U) :Unit



	/** Appends all values in `elems` to `seq`, preserving the sequence kind. */
	def appendedTo[U >: X, C, CC[_]](elems :Xs, seq :C)(implicit likeSeq :LikeSeq[U, C, CC, _]) :CC[U]

	/** Prepends all values in `elems` to `seq`, preserving the sequence kind. */
	def prependedTo[U >: X, C, CC[_]](elems :Xs, seq :C)(implicit likeSeq :LikeSeq[U, C, CC, _]) :CC[U]

	/** Copies all values from `elems` to the specified sequence, starting with index `index`. */
	@throws[IndexOutOfBoundsException]("if index < 0, or index > seq.length - elems.size.")
	def copiedTo[U >: X, C, CC[_]](elems :Xs, seq :C, index :Int)(implicit likeSeq :LikeSeq[U, C, CC, _]) :CC[U]

	/** Replaces `replaced` elements starting at `index` in the specified sequence with `elems`.
	  * This method behaves like [[scala.collection.SeqOps.patch SeqOps.patch]].
	  */
	def patchedOver[U >: X, C, CC[_]](elems :Xs, seq :C, index :Int, replaced :Int)
	                                 (implicit likeSeq :LikeSeq[U, C, CC, _]) :CC[U]

	/** Appends all values in `elems` to the given buffer. */ //todo: methods with max as an argument
	def appendTo[U >: X](elems :Xs, buffer :Buffer[U]) :Unit

	/** Prepends all values in `elems` to the given buffer. */
	def prependTo[U >: X](elems :Xs, buffer :Buffer[U]) :Unit

	/** Inserts all values in `elems` to the given buffer at the specified position.
	  * All elements in `[index, buffer.length)` are pushed back by `elems.size` positions.
	  */
	def insertInto[U >: X](elems :Xs, buffer :Buffer[U], index :Int) :Unit

	/** Analogue of `Buffer.`[[scala.collection.mutable.Buffer.patchInPlace patchInPlace]]. */
	def patchOver[U >: X](elems :Xs, buffer :Buffer[U], index :Int, replaced :Int) :Unit

	/** Copy values from `elems` to the given mutable sequence, starting at position `index`.
	  * Copying stops when either `elems` runs out of values, or the end of the sequence is reached,
	  * whichever happens sooner. The contract is the same
	  * as for `IterableOnceOps.`[[scala.collection.IterableOnceOps.copyToArray copyToArray]].
	  */ //todo: decide the order of delegation with MutableIndexedSeqLike.updatedAll
	def copyTo[U >: X, C](elems :Xs, seq :C, index :Int) //todo: add from and max parameters
	                     (implicit likeSeq :LikeMutableIndexedSeq[U, C, kinds.Any1, _]) :Int

//	def copyRangeToArray[A >: X](array :Array[A], start :Int, from :Int, until :Int, len :Int, xs :Xs) :Int

	/** Copy elements to an array, returning the number of elements written.
	  * Fills the given array `xs` starting at index `start` with at most `len` elements of `elems`.
	  * Copying will stop once either all the elements of `elems` have been copied,
	  * or the end of the array is reached, or `len` elements have been copied.
	  * @param elems a $coll of this type class.
	  * @param array the array to fill.
	  * @param start the starting index in `array`.
	  * @param max   the maximal number of elements to copy.
	  * @return the number of elements written to the array.
	  *
	  * @note Reuse: $consumesIterator
	  */
	def copyToArray[U >: X](elems :Xs, array :Array[U], start :Int = 0, max :Int = Int.MaxValue) :Int

	/** Copies at most `max` values from `elems` to the specified array, starting at position `index`.
	  * If the end of the array is reached before copying `max` values (or exhausting `elems`),
	  * copying resumes from the beginning of the array. If `max > array.length`, at most `array.length` values
	  * will be copied (this method will not overwrite data that it itself has copied).
	  */ //todo: cyclicCopyRangeToArray
	def cyclicCopyToArray[U >: X](elems :Xs, array :Array[U], index :Int, max :Int) :Int

	/** Adds all elements to the builder, returning the number of elements added (collection size). */
	final def addTo(elems :Xs, builder :Builder[X, Any]) :Int = addTo(elems, builder, 0, Int.MaxValue) //not robust for a Stepper

	/** Adds at most `max` first elements to the builder, returning the number of elements added. */
	final def addTo(elems :Xs, builder :Builder[X, Any], max :Int) :Int = addTo(elems, builder, 0, max)

	/** Adds a slice of this collection to the builder, and returns the number of actually added elements.
	  * Equivalent to `builder ++= elems.size(from, until)`, but the elements are accessed in a way best suited
	  * for the type of the collection, without copying to an intermediate collection.
	  */
	def addTo(elems :Xs, builder :Builder[X, Any], from :Int, until :Int) :Int

	/** A single use iterator over the elements of `elems`.
	  * @param elems a $coll of this type class.
	  */
	def iterator(elems :Xs) :Iterator[X]

	/** Equivalent to [[net.noresttherein.sugar.collections.LikeCollection.iterator iterator]]`.slice(from, until)`,
	  * but does not have to be lazy and in some cases the returned iterator may implement more specific methods,
	  * like `copyToArray`.
	  */
	def sliceIterator(elems :Xs, from :Int, until :Int) :Iterator[X] = iterator(elems).slice(from, until)

	/** Returns a [[scala.collection.Stepper]] for the elements of `elems`.
	  *
	  * The Stepper enables creating a Java stream to operate on the collection, see
	  * [[scala.jdk.StreamConverters]]. For collections holding primitive values, the Stepper can be
	  * used as an iterator which doesn't box the elements.
	  *
	  * The implicit [[scala.collection.StepperShape]] parameter defines the resulting Stepper type according to the
	  * element type of this collection.
	  *
	  *   - For collections of `Int`, `Short`, `Byte` or `Char`, an [[scala.collection.IntStepper]] is returned
	  *   - For collections of `Double` or `Float`, a [[scala.collection.DoubleStepper]] is returned
	  *   - For collections of `Long` a [[scala.collection.LongStepper]] is returned
	  *   - For any other element type, an [[scala.collection.AnyStepper]] is returned
	  *
	  * Note that this method is overridden in subclasses and the return type is refined to
	  * `S with EfficientSplit`, for example [[scala.collection.IndexedSeqOps.stepper]]. For Steppers marked with
	  * [[scala.collection.Stepper.EfficientSplit]], the converters in [[scala.jdk.StreamConverters]]
	  * allow creating parallel streams, whereas bare Steppers can be converted only to sequential
	  * streams.
	  * @param elems a $coll.
	  */
	def stepper[S <: Stepper[_]](elems :Xs)(implicit shape :StepperShape[X, S]) :S

	def toIterableOnce(elems :Xs) :IterableOnce[X]

	/** The described collection as `IterableOnceOps`. Those which already are `IterableOnceOps[X, _, Xs]`
	  * return themselves, others are recommended to return an `Iterator[X]` (rather than `Iterable[X]`),
	  * unless a wrapper to `Iterable` overrides most methods of this interface without delegating to `iterator(elems)`.
	  */
	def toIterableOnceOps(elems :Xs) :IterableOnceOps[X, kinds.Any1, Any]

	def toArray[U >: X :ClassTag](elems :Xs) :Array[U] = knownSize(elems) match {
		case  0 => ArrayFactory.empty[U]
		case -1 =>
			val buffer = ArrayBuffer.empty[U]
			appendTo[U](elems, buffer)
			buffer.toArray[U]
		case  n =>
			val res = new Array[U](n)
			copyToArray[U](elems, res, 0, Int.MaxValue)
			res
	}
	def toIArray[U >: X :ClassTag](elems :Xs) :IArray[U] = toArray[U](elems).castFrom[Array[U], IArray[U]]
	def toRefArray[U >: X](elems :Xs) :RefArray[U] = toArray[Any](elems).castFrom[Array[Any], RefArray[U]]
	def toIRefArray[U >: X](elems :Xs) :IRefArray[U] = toArray[Any](elems).castFrom[Array[Any], IRefArray[U]]

//	def slice(xs :Xs, from :Int, until :Int) :IterableOnce[X] //todo: slice and drop conflict with IterableOnceLike
//	def drop(xs :Xs, n :Int) :IterableOnce[X]
//	def consume[U](xs :Xs)(n :Int)(f :X => U) :IterableOnce[X]
	implicit val conversion :Xs => IterableOnce[X] = toIterableOnce



	/** A short (without listing all elements) textual representation of the collection, used in `Exception` messages. */
	def infoString(elems :Xs) :String

	/** If the dynamic type of the argument collection is recognized, and has a known, more specific type class,
	  * return that type class. This method may be used either by client code to retrieve that type class for later use,
	  * or by implementations which mix in
	  * [[net.noresttherein.sugar.collections.LikeCollection.LikeMoreSpecific LikeMoreSpecific]] (or some derived
	  * trait), which ''abstract overrides'' most of `LikeCollection` methods to delegate to `moreSpecific(elems)`,
	  * and fallback to `super` only if it returns `No`.
	  */
	def moreSpecific(elems :Xs) :Maybe[LikeCollection[X, elems.type]] = No

	def specific(elems :Xs) :LikeCollection[X, elems.type] = moreSpecific(elems) match {
		case Yes(specific) => specific
		case _             => this
	}

	override def toString :String = this.localClassName
}






private[collections] sealed abstract class Rank1LikeCollections {
	implicit def single[X, Y](implicit eq :X =:= Y) :LikeCollection[X, Y] = singletonPrototype.castParams[X, Y]

	def forSingleton[X] :LikeCollection[X, X] = singletonPrototype.castParam[X]

	implicit def forStepper[X] :LikeCollection[X, Stepper[X]] =
		stepperPrototype.asInstanceOf[LikeCollection[X, Stepper[X]]]

	private[this] val singletonPrototype = new SingleValue[Any]
	private[this] val stepperPrototype =
		new ForStepper[Any, Stepper[Any]] with LikeMoreSpecific[Any, Stepper[Any]] {
			override def toString = "LikeCollection.forStepper"

			override def moreSpecific(elems :Stepper[Any]) = (elems match {
				case _ :IntStepper    => Yes(LikeCollection.forIntStepper)
				case _ :LongStepper   => Yes(LikeCollection.forLongStepper)
				case _ :DoubleStepper => Yes(LikeCollection.forDoubleStepper)
				case _ :AnyStepper[_] => Yes(LikeCollection.forAnyStepper)
				case _                => No
			}).asInstanceOf[Maybe[LikeCollection[Any, elems.type]]]

			override def specific(elems :Stepper[Any]) = moreSpecific(elems) getOrElse LikeCollection.forStepper

			private def readResolve :AnyRef = LikeCollection.forStepper
		}
}


@SerialVersionUID(Ver)
object LikeCollection extends Rank1LikeCollections {
	/** A type holder for type aliases to use as type class view bound on type parameters:
	  * {{{
	  *     def addAll[C :LikeCollection.of[Int]#Coll](coll :C) = ...
	  * }}}
	  * @tparam X the type of the elements in the collection.
	  */
	type of[X] = {
		/** A type class representing `C` as a container of `X`. */
		type Coll[C] = LikeCollection[X, C]
	}

	@inline def apply[E, C](implicit ops :LikeCollection[E, C]) :ops.type = ops

	@inline def apply[C] :Summoner[C] = new Summoner[C] {}

	sealed trait Summoner[C] extends Any {
		@inline final def apply[E]()(implicit ops :LikeCollection[E, C]) :ops.type = ops
	}

	@inline def generic[E, CC[_]](implicit ops :LikeCollection[E, CC[E]]) :ops.type = ops

	@inline def generic[CC[_]] :GenericSummoner[CC] = new GenericSummoner[CC] {}

	sealed trait GenericSummoner[CC[_]] extends Any {
		@inline final def apply[E]()(implicit ops :LikeCollection[E, CC[E]]) :ops.type = ops
	}


	@inline implicit def likeIterableOnce[E, CC[_], C]
	                                     (implicit like :LikeIterableOnce[E, C, kinds.Any1, Any]) :LikeCollection[E, C] =
		like

	implicit val forIntStepper :LikeCollection[Int, IntStepper] =
		new ForStepper[Int, IntStepper] {
			override def toString = "LikeCollection.forIntStepper"
			private def readResolve :AnyRef = forIntStepper
		}

	implicit val forLongStepper :LikeCollection[Long, LongStepper] =
		new ForStepper[Long, LongStepper] {
			override def toString = "LikeCollection.forLongStepper"
			private def readResolve :AnyRef = forLongStepper
		}

	implicit val forDoubleStepper :LikeCollection[Double, DoubleStepper] =
		new ForStepper[Double, DoubleStepper] {
			override def toString = "LikeCollection.forDoubleStepper"
			private def readResolve :AnyRef = forDoubleStepper
		}

	implicit def forAnyStepper[X] :LikeCollection[X, AnyStepper[X]] =
		anyStepperPrototype.asInstanceOf[LikeCollection[X, AnyStepper[X]]]

	private[this] val anyStepperPrototype :LikeCollection[Any, Stepper[Any]] =
		new ForStepper[Any, Stepper[Any]] {
			override def toString = "LikeCollection.forAnyStepper"
			private def readResolve = anyStepperPrototype
		}



	def adapt[X, Xs](elems :Xs)(implicit likeCollection :LikeCollection[X, Xs]) :IterableOnce[X] =
		new LikeCollectionAdapter[X, elems.type](elems)(likeCollection.specific(elems))



	/** Overrides most methods of `LikeCollection` with `abstract override`, delegating them
	  * to [[net.noresttherein.sugar.collections.LikeCollection.moreSpecific moreSpecific]], if defined,
	  * and to `super` (i.e., the class it is mixed into) otherwise. Useful for default implicit values
	  * for types which belong to a large hierarchy and whose subtypes have their own type classes.
	  */
	trait LikeMoreSpecific[+X, -Xs] extends LikeCollection[X, Xs] {
//		abstract override def isTraversableAgain(elems :Xs) :Boolean =
//
//		abstract override def knownSize(elems :Xs) :Int = moreSpecific(elems) match {
//			case Yes(like) => like.knownSize(elems)
//			case _         => super.knownSize(elems)
//		}
//
//		abstract override def size(elems :Xs) :Int = moreSpecific(elems) match {
//			case Yes(like) => like.size(elems)
//			case _         => super.size(elems)
//		}
//
		abstract override def isEmpty(elems :Xs) :Boolean = moreSpecific(elems) match {
			case Yes(like) => like.isEmpty(elems)
			case No        => super.isEmpty(elems)
		}

		abstract override def forall(elems :Xs)(p :X => Boolean) :Boolean = moreSpecific(elems) match {
			case Yes(like) => like.forall(elems)(p)
			case _         => super.forall(elems)(p)
		}
		abstract override def exists(elems :Xs)(p :X => Boolean) :Boolean = moreSpecific(elems) match {
			case Yes(like) => like.exists(elems)(p)
			case _         => super.exists(elems)(p)
		}
		abstract override def count(elems :Xs)(p :X => Boolean) :Int = moreSpecific(elems) match {
			case Yes(like) => like.count(elems)(p)
			case _         => super.count(elems)(p)
		}
		abstract override def find(elems :Xs)(p :X => Boolean) :Option[X] = moreSpecific(elems) match {
			case Yes(like) => like.find(elems)(p)
			case _         => super.find(elems)(p)
		}
		abstract override def collectFirst[A](elems :Xs)(pf :PartialFunction[X, A]) :Option[A] =
			moreSpecific(elems) match {
				case Yes(like) => like.collectFirst(elems)(pf)
				case _         => super.collectFirst(elems)(pf)
			}

		abstract override def foldLeft[A](elems :Xs)(z :A)(op :(A, X) => A) :A = moreSpecific(elems) match {
			case Yes(like) => like.foldLeft(elems)(z)(op)
			case _         => super.foldLeft(elems)(z)(op)
		}
		abstract override def reduceLeft[U >: X](elems :Xs)(op :(U, X) => U) :U = moreSpecific(elems) match {
			case Yes(like) => like.reduceLeft[U](elems)(op)
			case _         => super.reduceLeft[U](elems)(op)
		}
		abstract override def reduceLeftOption[U >: X](elems :Xs)(op :(U, X) => U) :Option[U] =
			moreSpecific(elems) match {
				case Yes(like) => like.reduceLeftOption[U](elems)(op)
				case _         => super.reduceLeftOption[U](elems)(op)
			}

		abstract override def corresponds[A, O](elems :Xs, that :O)(p :(X, A) => Boolean)
		                                       (implicit likeCollection :LikeCollection[A, O]) :Boolean =
			moreSpecific(elems) match {
				case Yes(like) => like.corresponds(elems, that)(p)
				case _         => super.corresponds(elems, that)(p)
			}
		abstract override def foreach[U](elems :Xs)(f :X => U) :Unit = moreSpecific(elems) match {
			case Yes(like) => like.foreach(elems)(f)
			case _         => super.foreach(elems)(f)
		}

		abstract override def appendedTo[U >: X, C, CC[_]](elems :Xs, seq :C)
		                                                  (implicit likeSeq :LikeSeq[U, C, CC, _]) :CC[U] =
			moreSpecific(elems) match {
				case Yes(like) => like.appendedTo[U, C, CC](elems, seq)
				case _         => super.appendedTo[U, C, CC](elems, seq)
			}
		abstract override def prependedTo[U >: X, C, CC[_]](elems :Xs, seq :C)
		                                                   (implicit likeSeq :LikeSeq[U, C, CC, _]) :CC[U] =
			moreSpecific(elems) match {
				case Yes(like) => like.prependedTo[U, C, CC](elems, seq)
				case _         => super.prependedTo[U, C, CC](elems, seq)
			}
		abstract override def copiedTo[U >: X, C, CC[_]](elems :Xs, seq :C, index :Int)
		                                                (implicit likeSeq :LikeSeq[U, C, CC, _]) :CC[U] =
			moreSpecific(elems) match {
				case Yes(like) => like.copiedTo[U, C, CC](elems, seq, index)
				case _         => super.copiedTo[U, C, CC](elems, seq, index)
			}
		abstract override def patchedOver[U >: X, C, CC[_]](elems :Xs, seq :C, index :Int, replaced :Int)
		                                                   (implicit likeSeq :LikeSeq[U, C, CC, _]) :CC[U] =
			moreSpecific(elems) match {
				case Yes(like) => like.patchedOver[U, C, CC](elems, seq, index, replaced)
				case _         => super.patchedOver[U, C, CC](elems, seq, index, replaced)
			}

		abstract override def appendTo[U >: X](elems :Xs, buffer :Buffer[U]) :Unit = moreSpecific(elems) match {
			case Yes(like) => like.appendTo[U](elems, buffer)
			case _         => super.appendTo[U](elems, buffer)
		}
		abstract override def prependTo[U >: X](elems :Xs, buffer :Buffer[U]) :Unit = moreSpecific(elems) match {
			case Yes(like) => like.prependTo[U](elems, buffer)
			case _         => super.prependTo[U](elems, buffer)
		}
		abstract override def insertInto[U >: X](elems :Xs, buffer :Buffer[U], index :Int) :Unit =
			moreSpecific(elems) match {
				case Yes(like) => like.insertInto[U](elems, buffer, index)
				case _         => super.insertInto[U](elems, buffer, index)
			}
		abstract override def patchOver[U >: X](elems :Xs, buffer :Buffer[U], index :Int, replaced :Int) :Unit =
			moreSpecific(elems) match {
				case Yes(like) => like.patchOver[U](elems, buffer, index, replaced)
				case _         => super.patchOver[U](elems, buffer, index, replaced)
			}
		abstract override def copyTo[U >: X, C](elems :Xs, seq :C, index :Int)
		                                       (implicit likeSeq :LikeMutableIndexedSeq[U, C, Any1, _]) :Int =
			moreSpecific(elems) match {
				case Yes(like) => like.copyTo[U, C](elems, seq, index)
				case _         => super.copyTo[U, C](elems, seq, index)
			}

		abstract override def copyToArray[U >: X](elems :Xs, array :Array[U], start :Int, max :Int) :Int =
			moreSpecific(elems) match {
				case Yes(like) => like.copyToArray[U](elems, array, start, max)
				case _         => super.copyToArray[U](elems, array, start, max)
			}
		abstract override def cyclicCopyToArray[U >: X](elems :Xs, array :Array[U], index :Int, max :Int) :Int =
			moreSpecific(elems) match {
				case Yes(like) => like.cyclicCopyToArray[U](elems, array, index, max)
				case _         => super.cyclicCopyToArray[U](elems, array, index, max)
			}
		abstract override def addTo(elems :Xs, builder :Builder[X, Any], from :Int, until :Int) :Int =
			moreSpecific(elems) match {
				case Yes(like) => like.addTo(elems, builder, from, until)
				case _         => super.addTo(elems, builder, from, until)
			}
//		abstract override def addTo(elems :Xs, builder :Builder[X, Any], max :Int) :Int =
//			moreSpecific(elems) match {
//				case Yes(like) => like.addTo(elems, builder, max)
//				case _         => super.addTo(elems, builder, max)
//			}
//		abstract override def addTo(elems :Xs, builder :Builder[X, Any]) :Int =
//			moreSpecific(elems) match {
//				case Yes(like) => like.addTo(elems, builder)
//				case _         => super.addTo(elems, builder)
//			}

//		abstract override def toIterableOnce(elems :Xs) :IterableOnce[X] = moreSpecific(elems) match {
//			case Yes(like) => like.toIterableOnce(elems)
//			case _         => super.toIterableOnce(elems)
//		}
//		abstract override def toIterableOnceOps(elems :Xs) :IterableOnceOps[X, Any1, Any] = moreSpecific(elems) match {
//			case Yes(like) => like.toIterableOnceOps(elems)
//			case _         => super.toIterableOnceOps(elems)
//		}
//		abstract override def iterator(elems :Xs) :Iterator[X] = moreSpecific(elems) match {
//			case Yes(like) => like.iterator(elems)
//			case _         => super.iterator(elems)
//		}
		abstract override def stepper[S <: Stepper[_]](elems :Xs)(implicit shape :StepperShape[X, S]) :S =
			moreSpecific(elems) match {
				case Yes(specific) => specific.stepper(elems)
				case _             => super.stepper(elems)
			}
//		abstract override def infoString(elems :Xs) :String = moreSpecific(elems) match {
//			case Yes(like) => like.infoString(elems)
//			case _         => super.infoString(elems)
//		}
	}



	/** Implements those methods of `LikeCollection`, which can be implemented only in terms
	  * of other methods of this interface, without relying on `toOps`, `toIterableOnce`, or `iterator`.
	  * It is the minimal base class suitable for any implementation, regardless of which interfaces they use.
	  */
	trait LikeCollectionBasics[+X, -Xs] extends LikeCollection[X, Xs] {
		override def forall(elems :Xs)(p :X => Boolean) :Boolean = knownSize(elems) == 0 || find(elems)(!p(_)).isEmpty
		override def exists(elems :Xs)(p :X => Boolean) :Boolean = knownSize(elems) != 0 && find(elems)(p).nonEmpty
		override def find(elems :Xs)(p :X => Boolean) :Option[X] =
			if (knownSize(elems) == 0) None else collectFirst(elems) { case elem if p(elem) => elem }

		override def reduceLeftOption[U >: X](elems :Xs)(op :(U, X) => U) :Option[U] = knownSize(elems) match {
			case  0                   => None
			case -1 if isEmpty(elems) => unsupported_!(toString + ".reduceLeftOption(" + infoString(elems) + ")")
			case  _                   => Some(reduceLeft[U](elems)(op))
		}

		override def foreach[U](elems :Xs)(f :X => U) :Unit = foldLeft[Unit](elems)(()) { (unit, elem) => f(elem) }


		override def appendedTo[U >: X, S, SC[_]](elems :Xs, seq :S)(implicit likeSeq :LikeSeq[U, S, SC, _]) :SC[U] =
			likeSeq.appendedAll(seq, elems)(this)

		override def prependedTo[U >: X, S, SC[_]](elems :Xs, seq :S)(implicit likeSeq :LikeSeq[U, S, SC, _]) :SC[U] =
			likeSeq.prependedAll(seq, elems)(this)

		override def copiedTo[U >: X, S, SC[_]](elems :Xs, seq :S, index :Int)
		                                       (implicit likeSeq :LikeSeq[U, S, SC, _]) :SC[U] =
			likeSeq.updatedAll(seq, index, elems)(this)

		override def patchedOver[U >: X, S, SC[_]](elems :Xs, seq :S, index :Int, replaced :Int)
		                                          (implicit likeSeq :LikeSeq[U, S, SC, _]) :SC[U] =
			likeSeq.patch(seq, index, elems, replaced)(this)

		override def appendTo[U >: X](elems :Xs, buffer :Buffer[U]) :Unit = insertInto[U](elems, buffer, buffer.length)
		override def prependTo[U >: X](elems :Xs, buffer :Buffer[U]) :Unit = insertInto[U](elems, buffer, 0)

		override def copyTo[U >: X, O](elems :Xs, seq :O, index :Int)
		                              (implicit likeSeq :LikeMutableIndexedSeq[U, O, Any1, _]) :Int =
			if (knownSize(elems) == 0)
				0
			else
				likeSeq.updateAll(seq, index, elems)(this)

//		override def cyclicCopyToArray[U >: X](elems :Xs)(array :Array[U], index :Int, max :Int) :Int =

		//		def stepper[S <: Stepper[_]](elems :Xs)(implicit shape :StepperShape[X, S]) :S =
//			iterator(elems).stepper

		override def toArray[U >: X :ClassTag](elems :Xs) :Array[U] = knownSize(elems) match {
			case  0 => ArrayFactory.empty[U]
			case -1 =>
				val buffer = ArrayBuffer.empty[U]
				appendTo[U](elems, buffer)
				buffer.toArray[U]
			case  n =>
				val res = new Array[U](n)
				copyToArray[U](elems, res, 0, Int.MaxValue)
				res
		}
		override def toIArray[U >: X :ClassTag](elems :Xs) :IArray[U] = toArray[U](elems).castFrom[Array[U], IArray[U]]
		override def toRefArray[U >: X](elems :Xs) :RefArray[U] = toArray[Any](elems).castFrom[Array[Any], RefArray[U]]
		override def toIRefArray[U >: X](elems :Xs) :IRefArray[U] = toArray[Any](elems).castFrom[Array[Any], IRefArray[U]]

	}



	sealed trait FromIterableOnceSeal

	/** Collection type class converting the collection to `IterableOnce` to delegate to
	  * after a check of `knownSize` for the fast path.
	  * Any concrete class implementing this trait must also mix in either
	  * [[net.noresttherein.sugar.collections.LikeCollection.FromIterableOnceOps FromIterableOnceOps]]
	  * or [[net.noresttherein.sugar.collections.LikeCollection.FromIterator FromIterator]].
	  */
	trait FromIterableOnce[+X, -Xs] extends LikeCollectionBasics[X, Xs] { this :FromIterableOnceSeal =>
		override def isTraversableAgain(elems :Xs) :Boolean =
			knownSize(elems) == 0 || delegateOps(elems).isTraversableAgain

		override def knownSize(elems :Xs) :Int = toIterableOnce(elems).knownSize

		override def size(elems :Xs) :Int = knownSize(elems) match {
			case -1 => delegateOps(elems).size
			case  n => n
		}

		override def isEmpty(elems :Xs) :Boolean =
			knownSize(elems) == 0 || delegateOps(elems).isEmpty

		override def forall(elems :Xs)(p :X => Boolean) :Boolean =
			knownSize(elems) == 0 || delegateOps(elems).forall(p)

		override def exists(elems :Xs)(p :X => Boolean) :Boolean =
			knownSize(elems) > 0 && delegateOps(elems).exists(p)

		override def count(elems :Xs)(p :X => Boolean) :Int =
			if (knownSize(elems) == 0) 0 else delegateOps(elems).count(p)

		override def find(elems :Xs)(p :X => Boolean) :Option[X] =
			if (knownSize(elems) == 0) None else delegateOps(elems).find(p)

		override def foldLeft[A](elems :Xs)(z :A)(op :(A, X) => A) :A = knownSize(elems) match {
			case 0 => z
			case _ => delegateOps(elems).foldLeft(z)(op)
		}
		override def reduceLeft[U >: X](elems :Xs)(op :(U, X) => U) :U = delegateOps(elems).reduceLeft(op)
		override def reduceLeftOption[U >: X](elems :Xs)(op :(U, X) => U) :Option[U] = knownSize(elems) match {
			case  0 => None
			case  _ => delegateOps(elems).reduceLeftOption(op)
		}

		override def collectFirst[A](elems :Xs)(pf :PartialFunction[X, A]) :Option[A] =
			if (knownSize(elems) == 0) None else delegateOps(elems).collectFirst(pf)

		override def corresponds[A, O](elems :Xs, that :O)(p :(X, A) => Boolean)
		                              (implicit likeCollection :LikeCollection[A, O]) :Boolean =
			delegateOps(elems).corresponds(likeCollection.toIterableOnce(that))(p)

		override def foreach[U](elems :Xs)(f :X => U) :Unit =
			if (knownSize(elems) != 0)
				delegateOps(elems).foreach(f)

		override def insertInto[U >: X](elems :Xs, buffer :Buffer[U], index :Int) :Unit =
			if (knownSize(elems) != 0)
				buffer.insertAll(index, toIterableOnce(elems))

		override def patchOver[U >: X](elems :Xs, buffer :Buffer[U], index :Int, replaced :Int) :Unit =
			if (knownSize(elems) != 0)
				buffer.patchInPlace(index, toIterableOnce(elems), replaced)

		override def copyToArray[U >: X](elems :Xs, array :Array[U], start :Int, max :Int) :Int =
			if (max <= 0 || start >= array.length || knownSize(elems) == 0) 0
			else delegateOps(elems).copyToArray(array, start, max)

		//todo: implement this in subclasses
		override def cyclicCopyToArray[U >: X](elems :Xs, array :Array[U], index :Int, max :Int) :Int =
			if (max <= 0 || array.length == 0 || knownSize(elems) == 0) 0
			else toIterableOnce(elems).cyclicCopyToArray(array, index, max)

		override def addTo(elems :Xs, builder :Builder[X, Any], from :Int, until :Int) :Int =
			if (until <= 0 | until <= from)
				0
			else {
				val size = knownSize(elems)
				if (size >= 0) {
					if (from >= size)
						0
					else {
						val from0 = math.max(0, from)
						if (until >= size)
							builder addAll iterator(elems).dropInPlace(from0)
						else if (from0 == 0)
							builder addAll iterator(elems).take(until)
						else
							builder addAll iterator(elems).dropInPlace(from0).take(until - from0)
						math.min(size, until) - from0
					}
				} else {
					val it = iterator(elems).dropInPlace(from)
					if (!it.hasNext)
						0
					else {
						val counter = it.take(until - math.max(0, from)).counting
						builder addAll counter
						counter.total
					}
				}
			}


		override def toIterableOnceOps(elems :Xs) :IterableOnceOps[X, Any1, Any] = toIterableOnce(elems).toBasicOps
		
		override def iterator(elems :Xs) :Iterator[X] = toIterableOnce(elems).iterator

		override def stepper[S <: Stepper[_]](elems :Xs)(implicit shape :StepperShape[X, S]) :S =
			toIterableOnce(elems).stepper

		override def toArray[U >: X :ClassTag](elems :Xs) :Array[U] = delegateOps(elems).toArray[U]

		protected def delegateOps(elems :Xs) :IterableOnceOps[X, Any1, Any]

		override def infoString(elems :Xs) :String = knownSize(elems) match {
			case -1 => elems.localClassName
			case  n => elems.localClassName + "|" + n + "|"
		}
	}



	/** Collection type class implementing `LikeCollection` by delegating all methods to
	  * [[net.noresttherein.sugar.collections.LikeCollection.toIterableOnceOps toIterableOnceOps]]`(elems)`
	  * (which is either `elems`, if it implements `IterableOnceOps`, regardless of its second and third type parameter,
	  * or [[net.noresttherein.sugar.collections.LikeCollection.iterator iterator]]`(elems)` otherwise).
	  * @see [[net.noresttherein.sugar.collections.LikeCollection.FromIterator FromIterator]]
	  */
	trait FromIterableOnceOps[+X, -Xs] extends FromIterableOnce[X, Xs] with FromIterableOnceSeal {
		protected final override def delegateOps(elems :Xs) :IterableOnceOps[X, Any1, Any] = toIterableOnceOps(elems)
	}

	/** Collection type class implementing `LikeCollection` by delegating all methods to
	  * [[net.noresttherein.sugar.collections.LikeCollection.iterator iterator]]`(elems)`,
	  * and never to `toIterableOnceOps`/`toOps`. This makes it useful as a base trait for collection-like types
	  * from outside the Scala collection library (or simply implementing only `IterableOnce`, and to override
	  * the latter methods with adapter classes delegating to this type class, rather than the other way round.
	  * @see [[net.noresttherein.sugar.collections.LikeCollection.FromIterableOnceOps FromIterableOnceOps]]
	  */
	trait FromIterator[+X, -Xs] extends FromIterableOnce[X, Xs] with FromIterableOnceSeal {
		override def toIterableOnce(elems :Xs) :IterableOnce[X] = iterator(elems)

		protected final override def delegateOps(elems :Xs) :IterableOnceOps[X, Iterator, Iterator[X]] =
			iterator(elems)
	}


	trait ForIterableOnce[+X, -Xs <: IterableOnce[X]] extends FromIterableOnce[X, Xs] { this :FromIterableOnceSeal =>
		override def knownSize(elems :Xs) :Int = elems.knownSize
		override def iterator(elems :Xs) :Iterator[X] = elems.iterator
		override def toIterableOnce(elems :Xs) :IterableOnce[X] = elems
	}



/*
	trait ForJavaCollection[X, -Xs <: JCollection[X]] extends LikeCollectionDefaults[X, Xs] {
		override def isTraversableAgain(elems :Xs) :Boolean = true

		override def knownSize(elems :Xs) :Int = -1 //todo
		override def size(elems :Xs) :Int = elems.size
		override def isEmpty(elems :Xs) :Boolean = elems.isEmpty

		override def forall(elems :Xs)(p :X => Boolean) :Boolean =
			elems.isEmpty || {
				var ok = true
				val it = elems.iterator
				while (ok && it.hasNext)
					ok = p(it.next())
				ok
			}
		override def exists(elems :Xs)(p :X => Boolean) :Boolean =
			!elems.isEmpty || {
				var found = false
				val it = elems.iterator
				while (!found && it.hasNext)
					found = p(it.next())
				found
			}
		override def count(elems :Xs)(p :X => Boolean) :Int = {
			var res = 0
			val it = elems.iterator
			while (it.hasNext)
				if (p(it.next()))
					res += 1
			res
		}
		override def find(elems :Xs)(p :X => Boolean) :Option[X] =
			if (elems.isEmpty)
				None
			else {
				val it = elems.iterator
				var res = it.next()
				var found = p(res)
				while (!found && it.hasNext) {
					res = it.next()
					found = p(res)
				}
				if (found) Some(res) else None
		}
		override def collectFirst[A](elems :Xs)(pf :PartialFunction[X, A]) :Option[A] = {
			var res :Opt[A] = None
			val it = elems.iterator
			while (res.isEmpty && it.hasNext)
				res = pf.applyAndThenOrElse(it.next(), One.apply[A], None)
			res.toOption
		}

		override def foldLeft[A](elems :Xs)(z :A)(op :(A, X) => A) :A = elems.iterator.asScala.foldLeft(z)(op)
		override def reduceLeft[U >: X](elems :Xs)(op :(U, X) => U) :U = elems.iterator.asScala.reduceLeft(op)
		override def reduceLeftOption[U >: X](elems :Xs)(op :(U, X) => U) :Option[U] =
			elems.iterator.asScala.reduceLeftOption(op)

		override def corresponds[A, O](elems :Xs)(that :O)(p :(X, A) => Boolean)
		                              (implicit likeCollection :LikeCollection[A, O]) :Boolean =
			elems.iterator.asScala.corresponds(likeCollection.iterator(that))(p)

		override def foreach[U](elems :Xs)(f :X => U) :Unit =
			if (!elems.isEmpty) {
				val it = elems.iterator
				while (it.hasNext)
					f(it.next())
			}

		override def insertTo[U >: X](elems :Xs)(buffer :Buffer[U], index :Int) :Unit =
			if (!elems.isEmpty)
				buffer.insertAll(index, elems.asScala)

		override def patchOver[U >: X](elems :Xs)(buffer :Buffer[U], index :Int, replaced :Int) :Unit =
			buffer.patchInPlace(index, elems.asScala, replaced)

		override def copyToArray[U >: X](elems :Xs)(array :Array[U], start :Int, max :Int) :Int =
			if (util.nothingToCopy(array, start, max) || elems.isEmpty)
				0
			else if (start < 0)
				outOfBounds_!(start, array)
			else {
				var i   = start
				val end = start + math.min(array.length - start, max)
				val it  = elems.iterator
				while (start < end && it.hasNext)

			}

		override def cyclicCopyToArray[U >: X](elems :Xs)(array :Array[U], index :Int, max :Int) :Int = ???

		override def toIterableOnce(elems :Xs) :IterableOnce[X] = ???

		override def toIterableOnceOps(elems :Xs) :IterableOnceOps[X, Any1, Any] = ???

		override def iterator(elems :Xs) :Iterator[X] = ???

		override def infoString(elems :Xs) :String = ???
	}
*/

}






@SerialVersionUID(Ver)
private class ForStepper[@specialized(Int, Long, Double, AnyRef) X, Xs <: Stepper[X]]
	extends LikeCollectionBasics[X, Xs]
{
	override def isTraversableAgain(elems :Xs) :Boolean = knownSize(elems) == 0
	override def isEmpty(elems :Xs) :Boolean = !elems.hasStep

	override def knownSize(elems :Xs) :Int =
		if ((elems.characteristics & Spliterator.SIZED) == 0)
			-1
		else {
			val size = elems.estimateSize
			if (size > Int.MaxValue) -1 else size.toInt
		}

	override def size(elems :Xs) :Int = {
		val size =
			if ((elems.characteristics & Spliterator.SIZED) != 0)
				elems.estimateSize
			else {
				var res = 0L
				while (elems.hasStep & res <= Int.MaxValue) {
					res += 1
					elems.nextStep()
				}
				res
			}
		if (size > Int.MaxValue)
			unsupported_!("Stepper " + elems + " has/had more than Int.MaxValue elements.")
		size.toInt
	}

	private def find(elems :Xs, p :X => Boolean, truth :Boolean) :Opt[X] = {
		while (elems.hasStep) {
			val next = elems.nextStep()
			if (p(next) == truth)
				return One(next)
		}
		None
	}
	override def find(elems :Xs)(p :X => Boolean) :Option[X] = find(elems, p, true).toOption
	override def forall(elems :Xs)(p :X => Boolean) :Boolean = find(elems, p, false).isEmpty
	override def exists(elems :Xs)(p :X => Boolean) :Boolean = find(elems)(p).isDefined

	override def count(elems :Xs)(p :X => Boolean) :Int = {
		var res = 0
		while (elems.hasStep)
			if (p(elems.nextStep()))
				res += 1
		res
	}

	override def foldLeft[A](elems :Xs)(z :A)(op :(A, X) => A) :A = {
		var acc = z
		while (elems.hasStep)
			acc = op(acc, elems.nextStep())
		acc
	}
	override def reduceLeft[U >: X](elems :Xs)(op :(U, X) => U) :U =
		if (!elems.hasStep)
			unsupported_!("Empty stepper " + elems + ": reduceLeft")
		else
			foldLeft[U](elems)(elems.nextStep())(op)

	override def reduceLeftOption[U >: X](elems :Xs)(op :(U, X) => U) :Option[U] =
		if (!elems.hasStep) None
		else Some(reduceLeft[U](elems)(op))

	override def collectFirst[A](elems :Xs)(pf :PartialFunction[X, A]) :Option[A] = {
		while (elems.hasStep)
			pf.applyAndThenEither(elems.nextStep(), Some.apply _, _ => None) match {
				case some :Some[A] => return some
				case _ =>
			}
		None
	}

	override def corresponds[A, O](elems :Xs, that :O)(p :(X, A) => Boolean)
	                              (implicit likeCollection :LikeCollection[A, O]) :Boolean =
	{
		val thisSize = knownSize(elems)
		val thatSize = likeCollection.knownSize(that)
		if (thisSize >= 0 & thatSize >= 0 & thisSize != thatSize)
			return false
		val it = likeCollection.iterator(that)
		while (elems.hasStep && it.hasNext)
			if (!p(elems.nextStep(), it.next()))
				return false
		!elems.hasStep && !it.hasNext
	}
	override def foreach[U](elems :Xs)(f :X => U) :Unit =
		while (elems.hasStep)
			f(elems.nextStep())


	override def appendedTo[A >: X, C, CC[_]](elems :Xs, seq :C)(implicit likeSeq :LikeSeq[A, C, CC, _]) :CC[A] =
		if (!elems.hasStep)
			likeSeq.toGeneric(seq)
		else
			likeSeq.appendedAll(seq, elems)(this)

	override def prependedTo[A >: X, C, CC[_]](elems :Xs, seq :C)(implicit likeSeq :LikeSeq[A, C, CC, _]) :CC[A] =
		if (!elems.hasStep)
			likeSeq.toGeneric(seq)
		else
			likeSeq.prependedAll(seq, elems)(this)

	override def copiedTo[A >: X, C, CC[_]](elems :Xs, seq :C, index :Int)
	                                       (implicit likeSeq :LikeSeq[A, C, CC, _]) :CC[A] =
		if (!elems.hasStep)
			likeSeq.toGeneric(seq)
		else
			likeSeq.updatedAll(seq, index, elems)(this)

	override def patchedOver[A >: X, C, CC[_]](elems :Xs, seq :C, index :Int, replaced :Int)
	                                          (implicit likeSeq :LikeSeq[A, C, CC, _]) :CC[A] =
		if (!elems.hasStep)
			likeSeq.toGeneric(seq)
		else
			likeSeq.patch(seq, index, elems, replaced)(this)

	override def appendTo[U >: X](elems :Xs, buffer :Buffer[U]) :Unit =
		while (elems.hasStep)
			buffer += elems.nextStep()

	override def insertInto[A >: X](elems :Xs, buffer :Buffer[A], index :Int) :Unit =
		if (elems.hasStep)
			buffer.insertAll(index, iterator(elems))

	override def patchOver[A >: X](elems :Xs, buffer :Buffer[A], index :Int, replaced :Int) :Unit =
		if (!elems.hasStep)
			buffer.remove(index, replaced)
		else
			buffer.patchInPlace(index, iterator(elems), replaced)

	override def copyTo[A >: X, C](elems :Xs, seq :C, index :Int)
	                              (implicit likeSeq :LikeMutableIndexedSeq[A, C, kinds.Any1, _]) :Int =
		if (!elems.hasStep) 0
		else likeSeq.updateAll(seq, index, elems)(this)

	override def copyToArray[A >: X](elems :Xs, array :Array[A], start :Int, max :Int) :Int =
		(elems, array :Array[_]) match {
			case _ if max <= 0 || start >= array.length || !elems.hasStep =>
				0
			case (_ :IntStepper, a :Array[Int]) if this ne LikeCollection.forIntStepper =>
				specificCopyToArray(elems, a.asInstanceOf[Array[X]], start, max)
//				LikeCollection.forIntStepper.copyToArray(s)(a, start, max)
			case (_ :LongStepper, a :Array[Long]) if this ne LikeCollection.forLongStepper =>
				specificCopyToArray(elems, a.asInstanceOf[Array[X]], start, max)
//				LikeCollection.forLongStepper.copyToArray(s)(a, start, max)
			case (_ :DoubleStepper, a :Array[Double]) if this ne LikeCollection.forDoubleStepper =>
				specificCopyToArray(elems, a.asInstanceOf[Array[X]], start, max)
//				LikeCollection.forDoubleStepper.copyToArray(s)(a, start, max)
			case (_ :AnyStepper[AnyRef @unchecked], a :Array[AnyRef]) =>
				specificCopyToArray(elems, a.asInstanceOf[Array[X]], start, max)
			case _ =>
				val end = start + math.min(max, array.length - math.max(start, 0))
				var i   = start
				while (i < end && elems.hasStep) {
					array(i) = elems.nextStep()
					i += 1
				}
				i - start
	}
	private def specificCopyToArray(elems :Xs, array :Array[X @uncheckedVariance], start :Int, max :Int) :Int = {
		val end = start + math.min(max, array.length - math.max(start, 0))
		var i   = start
		while (i < end && elems.hasStep) {
			array(i) = elems.nextStep()
			i += 1
		}
		i - start
	}

	override def cyclicCopyToArray[A >: X](elems :Xs, array :Array[A], index :Int, max :Int) = {
		val length = array.length
		if (max <= 0 | length == 0)
			0
		else {
			val start = index % length
			val count = math.min(max, length)
			val copied = copyToArray[A](elems, array, start, count)
			copied + copyToArray[A](elems, array, 0, count - copied)
		}
	}

	override def addTo(elems :Xs, builder :Builder[X, Any], from :Int, until :Int) :Int = {
		var count = 0
		while (count < from && elems.hasStep) {
			count += 1
			elems.nextStep()
		}
		while (count < until && elems.hasStep) {
			count += 1
			builder += elems.nextStep()
		}
		count - math.max(from, 0)
	}

	override def toIterableOnce(elems :Xs) :IterableOnce[X] = iterator(elems)

	override def toIterableOnceOps(elems :Xs) :IterableOnceOps[X, kinds.Any1, Any] = iterator(elems)
	override def iterator(elems :Xs) :Iterator[X] = elems.iterator

	override def stepper[O <: Stepper[_]](elems :Xs)(implicit shape :StepperShape[X, O]) :O = elems.asInstanceOf[O]

	override def infoString(elems :Xs) :String = knownSize(elems) match {
		case -1 => elems.localClassName
		case  n => elems.localClassName + "|" + n + "|"
	}
}






@SerialVersionUID(Ver)
private class SingleValue[X] extends LikeCollection[X, X] {
	override def isTraversableAgain(xs :X) :Boolean = true

	override def knownSize(xs :X) :Int = 1
	override def size(xs :X) :Int = 1
	override def isEmpty(xs :X) :Boolean = false

	override def forall(elems :X)(p :X => Boolean) :Boolean = p(elems)
	override def exists(elems :X)(p :X => Boolean) :Boolean = p(elems)
	override def count(elems :X)(p :X => Boolean) :Int = if (p(elems)) 1 else 0
	override def find(elems :X)(p :X => Boolean) :Option[X] = if (p(elems)) Some(elems) else None

	override def foldLeft[A](xs :X)(zero :A)(op :(A, X) => A) :A = op(zero, xs)
	override def reduceLeft[A >: X](xs :X)(op :(A, X) => A) :A = xs
	override def reduceLeftOption[A >: X](xs :X)(op :(A, X) => A) :Option[A] = Some(xs)

	override def collectFirst[A](elems :X)(pf :PartialFunction[X, A]) :Option[A] =
		pf.applyAndThenEither(elems, Some.apply _, _ => None)

	override def corresponds[A, O](elems :X, that :O)(p :(X, A) => Boolean)
	                              (implicit likeCollection :LikeCollection[A, O]) :Boolean =
		likeCollection.knownSize(that) match {
			case -1 =>
				val it = likeCollection.iterator(that)
				it.hasNext && p(elems, it.next()) && !it.hasNext
			case  1 => likeCollection.forall(that)(p(elems, _))
			case  _ => false
		}

	override def foreach[U](xs :X)(f :X => U) :Unit = f(xs)

	override def appendedTo[A >: X, C, CC[_]](elems :X, seq :C)(implicit likeSeq :LikeSeq[A, C, CC, _]) :CC[A] =
		likeSeq.appended(seq, elems)

	override def prependedTo[A >: X, C, CC[_]](elems :X, seq :C)(implicit likeSeq :LikeSeq[A, C, CC, _]) :CC[A] =
		likeSeq.prepended(seq, elems)

	override def copiedTo[A >: X, C, CC[_]](elems :X, seq :C, index :Int)
	                                       (implicit likeSeq :LikeSeq[A, C, CC, _]) :CC[A] =
		likeSeq.updated(seq, index, elems)

	override def patchedOver[A >: X, C, CC[_]](elems :X, seq :C, index :Int, replaced :Int)
	                                          (implicit likeSeq :LikeSeq[A, C, CC, _]) :CC[A] =
		if (replaced == 1) copiedTo[A, C, CC](elems, seq, index)
		else likeSeq.patch(seq, index, elems, replaced)(this)

	override def appendTo[A >: X](elems :X, buffer :Buffer[A]) :Unit = buffer += elems
	override def prependTo[A >: X](elems :X, buffer :Buffer[A]) :Unit = elems +=: buffer
	override def insertInto[A >: X](elems :X, buffer :Buffer[A], index :Int) :Unit = buffer.insert(index, elems)
	override def patchOver[A >: X](elems :X, buffer :Buffer[A], index :Int, replaced :Int) :Unit =
		if (replaced <= 0)
			buffer.insert(index, elems)
		else if (replaced == 1)
			buffer.update(index, elems)
		else if (replaced >= buffer.length) {
			buffer.clear()
			buffer += elems
		} else {
			if (index <= 0) {
				buffer.remove(0, replaced - 1)
				buffer(0) = elems
			} else {
				buffer.remove(index, buffer.length - replaced)
				buffer(index) = elems
			}
		}

	override def copyTo[A >: X, C](elems :X, seq :C, index :Int)
	                              (implicit likeSeq :LikeMutableIndexedSeq[A, C, kinds.Any1, _]) :Int =
		if (likeSeq.size(seq) > index) {
			likeSeq.updated(seq, index, elems); 1
		} else
			0

	override def copyToArray[A >: X](xs :X, array :Array[A], index :Int, max :Int) :Int =
		if (max <= 0 || index >= array.length)
			0
		else if (index < 0)
			outOfBounds_!(index, array.length)
		else {
			array(index) = xs
			1
		}

	override def cyclicCopyToArray[A >: X](xs :X, array :Array[A], index :Int, max :Int) :Int =
		if (index == array.length) copyToArray[A](xs, array, 0, max)
		else copyToArray[A](xs, array, index, max)

	override def addTo(elems :X, builder :Builder[X, Any], from :Int, until :Int) :Int =
		if (from >= 1 | until <= 0)
			0
		else {
			builder += elems; 1
		}

	override def iterator(xs :X) :Iterator[X] = Iterator.single(xs)
	override def stepper[S <: Stepper[_]](elems :X)(implicit shape :StepperShape[X, S]) :S = Stepper.one(elems)
	override def toIterableOnce(elems :X) :IterableOnce[X] = Seq.single(elems)
	override def toIterableOnceOps(elems :X) :IterableOnceOps[X, Any, Any] = Iterator.single(elems)
	override def toArray[U >: X :ClassTag](elems :X) :Array[U] = {
		val res = new Array[U](1)
		res(0) = elems
		res
	}

	override def infoString(elems :X) :String = "{" + elems.localClassName + "}|1|"

	private def readResolve :AnyRef = LikeCollection.forSingleton
	override def toString = "LikeCollection.forSingleton"
}






@SerialVersionUID(Ver)
private class LikeCollectionProxy[+X, -Xs](values :LikeCollection[X, Xs]) extends LikeCollection[X, Xs] {
	override def isTraversableAgain(xs :Xs) :Boolean = values.isTraversableAgain(xs)

	override def knownSize(xs :Xs) :Int = values.knownSize(xs)
	override def size(xs :Xs) :Int = values.size(xs)
	override def isEmpty(xs :Xs) :Boolean = values.isEmpty(xs)

	override def forall(elems :Xs)(p :X => Boolean) :Boolean = values.forall(elems)(p)
	override def exists(elems :Xs)(p :X => Boolean) :Boolean = values.exists(elems)(p)
	override def count(elems :Xs)(p :X => Boolean) :Int = values.count(elems)(p)
	override def find(elems :Xs)(p :X => Boolean) :Option[X] = values.find(elems)(p)

	override def foldLeft[A](xs :Xs)(zero :A)(op :(A, X) => A) :A = values.foldLeft[A](xs)(zero)(op)
	override def reduceLeft[A >: X](xs :Xs)(op :(A, X) => A) :A = values.reduceLeft[A](xs)(op)
	override def reduceLeftOption[A >: X](xs :Xs)(op :(A, X) => A) :Option[A] = values.reduceLeftOption[A](xs)(op)

	override def collectFirst[A](elems :Xs)(pf :PartialFunction[X, A]) :Option[A] = values.collectFirst(elems)(pf)

	override def corresponds[A, O](elems :Xs, that :O)(p :(X, A) => Boolean)
	                              (implicit likeCollection :LikeCollection[A, O]) :Boolean =
		values.corresponds(elems, that)(p)

	override def foreach[U](xs :Xs)(f :X => U) :Unit = values.foreach(xs)(f)


	override def appendedTo[A >: X, C, CC[_]](elems :Xs, seq :C)(implicit likeSeq :LikeSeq[A, C, CC, _]) :CC[A] =
		values.appendedTo[A, C, CC](elems, seq)

	override def prependedTo[A >: X, C, CC[_]](elems :Xs, seq :C)(implicit likeSeq :LikeSeq[A, C, CC, _]) :CC[A] =
		values.prependedTo[A, C, CC](elems, seq)

	override def copiedTo[A >: X, C, CC[_]](elems :Xs, seq :C, index :Int)
	                                       (implicit likeSeq :LikeSeq[A, C, CC, _]) :CC[A] =
		values.copiedTo[A, C, CC](elems, seq, index)

	override def patchedOver[A >: X, C, CC[_]](elems :Xs, seq :C, index :Int, replaced :Int)
	                                          (implicit likeSeq :LikeSeq[A, C, CC, _]) :CC[A] =
		values.patchedOver[A, C, CC](elems, seq, index, replaced)

	override def appendTo[A >: X](elems :Xs, buffer :Buffer[A]) :Unit = values.appendTo[A](elems, buffer)
	override def prependTo[A >: X](elems :Xs, buffer :Buffer[A]) :Unit = values.prependTo[A](elems, buffer)
	override def insertInto[A >: X](elems :Xs, buffer :Buffer[A], index :Int) :Unit =
		values.insertInto[A](elems, buffer, index)

	 override def patchOver[A >: X](elems :Xs, buffer :Buffer[A], index :Int, replaced :Int) :Unit =
		values.patchOver[A](elems, buffer, index, replaced)

	override def copyTo[A >: X, C](elems :Xs, seq :C, index :Int)
	                              (implicit likeSeq :LikeMutableIndexedSeq[A, C, kinds.Any1, _]) :Int =
		values.copyTo[A, C](elems, seq, index)

	override def copyToArray[A >: X](xs :Xs, array :Array[A], index :Int, max :Int) :Int =
		values.copyToArray[A](xs, array, index, max)

	override def cyclicCopyToArray[A >: X](xs :Xs, array :Array[A], index :Int, max :Int) :Int =
		values.cyclicCopyToArray[A](xs, array, index, max)

//	override def addTo(elems :Xs, builder :Builder[X, Any]) :Int = values.addTo(elems, builder)
	override def addTo(elems :Xs, builder :Builder[X, Any], from :Int, until :Int) :Int =
		values.addTo(elems, builder, from, until)

	override def iterator(xs :Xs) :Iterator[X] = values.iterator(xs)
	override def stepper[S <: Stepper[_]](elems :Xs)(implicit shape :StepperShape[X, S]) :S = values.stepper(elems)
	override def toIterableOnce(elems :Xs) :IterableOnce[X] = values.toIterableOnce(elems)
	override def toIterableOnceOps(elems :Xs) :IterableOnceOps[X, kinds.Any1, Any] = values.toIterableOnceOps(elems)
	override def toArray[U >: X :ClassTag](elems :Xs) :Array[U] = values.toArray(elems)
	override def toIArray[U >: X :ClassTag](elems :Xs) :IArray[U] = values.toIArray[U](elems)
	override def toRefArray[U >: X](elems :Xs) :RefArray[U] = values.toRefArray(elems)
	override def toIRefArray[U >: X](elems :Xs) :IRefArray[U] = values.toIRefArray(elems)

	override def infoString(elems :Xs) :String = values.infoString(elems)
}






@SerialVersionUID(Ver)
private class LikeCollectionAdapter[+X, Xs](protected val elems :Xs)(implicit protected val ops :LikeCollection[X, Xs])
	extends IterableOnce[X]
{
	override def knownSize :Int = ops.knownSize(elems)
	override def iterator :Iterator[X] = ops.iterator(elems)
	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[X, S]) :S = ops.stepper(elems)
	override def toString :String = ops.toString + "(" + elems + ")"
}
