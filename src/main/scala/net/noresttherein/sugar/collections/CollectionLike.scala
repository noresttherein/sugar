package net.noresttherein.sugar.collections

import scala.collection.{IterableOnceOps, mutable}
import scala.collection.mutable.Buffer

import net.noresttherein.sugar.{funny, outOfBounds_!}
import net.noresttherein.sugar.collections.extensions.{IteratorExtension, SeqFactoryExtension}
import net.noresttherein.sugar.extensions.{cast2TypeParamsMethods, castTypeParamMethods, classNameMethods}
import net.noresttherein.sugar.funny.generic




/** A type class for collection types, meant to abstract over collections not implementing any interface
  * from the standard collection library, such as `String`, but also individual elements. It duplicates
  * some of the basic methods of [[collection.IterableOnceOps IterableOnceOps]], but also defines methods which reverse
  * the responsibility of adding `Xs` to another collection. It allows to define a single implementation
  * for collection methods accepting various arguments, for example `add(x :X)`, `addAll(xs :IterableOnce[X])`,
  * `addAll(xs :Array[X])`, etc. It thus serves a similar purpose as [[collection.generic.IsIterable IsIterable]]
  * family of type classes, but does not require boxing of the argument type.
  * @define coll collection
  * @author Marcin Mościcki
  */
trait CollectionLike[+X, -Xs] extends Serializable {
	/* We can't simply cast Xs to IterableOnceOps, Iterable, etc., because we don't know if its element type
	 * is actually X (for example, SingleValue[Seq[X]] extends CollectionLike[Seq[X], Seq[X]]),
	 *let alone the collection type.
	 */

	/** Tests whether `elems` can be repeatedly traversed.
	  * @param elems a $coll of this type class.
	  * @return `true` if [[net.noresttherein.sugar.collections.CollectionLike.iterator iterator]]
	  *         returns a new iterator with every call.
	  */
	def isTraversableAgain(elems :Xs) :Boolean = false

	/** @param elems a $coll of this type class.
	  * @return The number of elements in `elems`, if it can be cheaply computed,
	  *         -1 otherwise. Cheaply usually means: Not requiring a collection traversal.
	  */
	def knownSize(elems :Xs) :Int

	/** The size of `elems`.
	  * @param elems a $coll of this type class.
	  * @return the number of elements in `elems`.
	  */
	def size(elems :Xs) :Int = {
		val size = knownSize(elems)
		if (size >= 0) size else toIterableOnceOps(elems).size
	}

	/** Tests whether the $coll is empty.
	  * @param elems a $coll of this type class.
	  * @return `true` if the $coll contains no elements, `false` otherwise.
	  */
	def isEmpty(elems :Xs) :Boolean = knownSize(elems) match {
		case -1 => toIterableOnceOps(elems).isEmpty
		case  0 => true
		case  _ => false
	}

	/** Tests whether the $coll is not empty.
	  * @param elems a $coll of this type class.
	  * @return `true` if the $coll contains at least one element, `false` otherwise.
	  */
	@inline final def nonEmpty(elems :Xs) :Boolean = !isEmpty(elems)

	/** A single use iterator over the elements of `elems`.
	  * @param elems a $coll of this type class.
	  */
	def iterator(elems :Xs) :Iterator[X]

	def toIterableOnce(xs :Xs) :IterableOnce[X]

	/** The described collection as `IterableOnceOps`. Those which already are `IterableOnceOps[X, _, Xs]`
	  * return themselves, others are recommended to return an `Iterator[X]` (rather than `Iterable[X]`),
	  * unless a wrapper to `Iterable` overrides most methods of this interface without delegating to `iterator(elems)`.
	  */
	def toIterableOnceOps(xs :Xs) :IterableOnceOps[X, generic.Any, Any]
//	def slice(xs :Xs, from :Int, until :Int) :IterableOnce[X] //todo: slice and drop conflict with IterableOnceLike
//	def drop(xs :Xs, n :Int) :IterableOnce[X]
//	def consume[U](xs :Xs)(n :Int)(f :X => U) :IterableOnce[X]


	/** Apply `f` to each element for its side effects
	  * @param elems a $coll of this type class.
	  * @param f     a function to apply to each element in `elems`.
	  */
	def foreach[U](elems :Xs)(f :X => U) :Unit = toIterableOnceOps(elems).foreach(f)

	/** Applies a binary operator to a start value and all elements of `elems`, going left to right.
	  * @param elems a $coll of this type class.
	  * @param z     the start  value.
	  * @param op    the binary operator.
	  * @return the result of inserting `op` between consecutive elements of `elems`, going left to right
	  *         with the start value `z` on the left: `op(...op(z, x,,1,,), x,,2,,, ..., x,,n,,)`
	  *         where `x,,1,,, ..., x,,n,,` are the elements of `elems`. Returns `z` if `elems` is empty.
	  */
	def foldLeft[A](elems :Xs)(z :A)(op :(A, X) => A) :A = knownSize(elems) match {
		case 0 => z
		case _ => toIterableOnceOps(elems).foldLeft(z)(op)
	}

	/** Applies a binary operator to all elements of `elems`,
	  * going left to right.
	  * @param elems a $coll of this type class.
	  * @param op    a binary operator.
	  * @return the result of inserting `op` between consecutive elements of `elems`, going left to right:
	  *         `op( op( ... op(x,,1,,, x,,2,,) ..., x,,n-1,,), x,,n,,)` where `x,,1,,, ..., x,,n,,`
	  *         are the elements of `elems`.
	  * @throws UnsupportedOperationException if `elems` is empty.
	  */
	def reduceLeft[U >: X](elems :Xs)(op :(U, X) => U) :U = toIterableOnceOps(elems).reduceLeft(op)

	/** Optionally applies a binary operator to all elements of `elems`, going left to right.
	  * @param elems a $coll of this type class.
	  * @param op    a binary operator.
	  * @return an option value containing the result of `reduceLeft(op)` if `elems` is nonempty, `None` otherwise.
	  */
	def reduceLeftOption[U >: X](elems :Xs)(op :(U, X) => U) :Option[U] = knownSize(elems) match {
		case 0 => None
		case _ => toIterableOnceOps(elems).reduceLeftOption(op)
	}

	def appendedTo[A >: X, CC[_], C](elems :Xs)(seq :collection.SeqOps[A, CC, C]) :CC[A]
	def prependedTo[A >: X, CC[_], C](elems :Xs)(seq :collection.SeqOps[A, CC, C]) :CC[A]
	def copiedTo[A >: X, CC[_], C](elems :Xs)(seq :collection.SeqOps[A, CC, C], index :Int) :CC[A]
	def patchedOn[A >: X, CC[_], C](elems :Xs)(seq :collection.SeqOps[A, CC, C], index :Int, replaced :Int) :CC[A]

	def appendTo[A >: X](elems :Xs)(buffer :Buffer[A]) :Unit = insertTo[A](elems)(buffer, buffer.length)
	def prependTo[A >: X](elems :Xs)(buffer :Buffer[A]) :Unit = insertTo[A](elems)(buffer, 0)
	def insertTo[A >: X](elems :Xs)(buffer :Buffer[A], index :Int) :Unit
	def patchOn[A >: X](elems :Xs)(buffer :Buffer[A], index :Int, replaced :Int) :Unit
//	def setAll[A >: X](xs :Xs)(seq :mutable.Seq[A], index :Int) :Unit
	def copyTo[A >: X](elems :Xs)(seq :mutable.Seq[A], index :Int) :Int

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
	def copyToArray[A >: X](elems :Xs)(array :Array[A], start :Int = 0, max :Int = Int.MaxValue) :Int =
		iterator(elems).copyToArray(array, start, max)

	def cyclicCopyToArray[A >: X](elems :Xs)(array :Array[A], index :Int, max :Int) :Int =
		iterator(elems).cyclicCopyToArray(array, index, max)

	def infoString(elems :Xs) :String
	override def toString :String = this.innerClassName
}




sealed abstract class Rank1Values {
	implicit def single[X, Y](implicit eq :X =:= Y) :CollectionLike[X, Y] = singletonPrototype.castParams[X, Y]
	private[this] val singletonPrototype = new SingleValue[Any]

	def singleton[X] :CollectionLike[X, X] = singletonPrototype.castParam[X]
}


@SerialVersionUID(Ver)
object CollectionLike extends Rank1Values {
	@inline def apply[E, C](implicit ops :CollectionLike[E, C]) :ops.type = ops

	@inline def apply[C] :Summoner[C] = new Summoner[C] {}

	sealed trait Summoner[C] extends Any {
		@inline final def apply[E]()(implicit ops :CollectionLike[E, C]) :ops.type = ops
	}

	@inline def generic[E, CC[_]](implicit ops :CollectionLike[E, CC[E]]) :ops.type = ops

	@inline def generic[CC[_]] :GenericSummoner[CC] = new GenericSummoner[CC] {}

	sealed trait GenericSummoner[CC[_]] extends Any {
		@inline final def apply[E]()(implicit ops :CollectionLike[E, CC[E]]) :ops.type = ops
	}

	@inline implicit def fromIterableOnceLike[E, CC[_], C]
	                                         (implicit like :IterableOnceLike[E, funny.generic.Any, C]) :CollectionLike[E, C] =
		like

}



@SerialVersionUID(Ver)
private class SingleValue[X] extends CollectionLike[X, X] {
	override def isTraversableAgain(xs :X) :Boolean = true

	override def knownSize(xs :X) :Int = 1
	override def size(xs :X) :Int = 1
	override def isEmpty(xs :X) :Boolean = false

	override def iterator(xs :X) :Iterator[X] = Iterator.single(xs)
	override def toIterableOnce(xs :X) :IterableOnce[X] = Seq.single(xs)
	override def toIterableOnceOps(xs :X) :IterableOnceOps[X, Any, Any] = Iterator.single(xs)

	override def foreach[U](xs :X)(f :X => U) :Unit = f(xs)
	override def foldLeft[A](xs :X)(zero :A)(op :(A, X) => A) :A = op(zero, xs)
	override def reduceLeft[A >: X](xs :X)(op :(A, X) => A) :A = xs
	override def reduceLeftOption[A >: X](xs :X)(op :(A, X) => A) :Option[A] = Some(xs)

	override def appendedTo[A >: X, CC[_], C](elems :X)(seq :collection.SeqOps[A, CC, C]) :CC[A] = seq :+ elems
	override def prependedTo[A >: X, CC[_], C](elems :X)(seq :collection.SeqOps[A, CC, C]) :CC[A] = elems +: seq
	override def copiedTo[A >: X, CC[_], C](elems :X)(seq :collection.SeqOps[A, CC, C], index :Int) :CC[A] = seq.updated(index, elems)
	override def patchedOn[A >: X, CC[_], C](elems :X)(seq :collection.SeqOps[A, CC, C], index :Int, replaced :Int) :CC[A] =
		if (replaced == 1) copiedTo[A, CC, C](elems)(seq, index)
		else seq.patch(index, Seq.one(elems), replaced)

	override def appendTo[A >: X](elems :X)(buffer :Buffer[A]) :Unit = buffer += elems
	override def prependTo[A >: X](elems :X)(buffer :Buffer[A]) :Unit = elems +=: buffer
	override def insertTo[A >: X](elems :X)(buffer :Buffer[A], index :Int) :Unit = buffer.insert(index, elems)
	override def patchOn[A >: X](elems :X)(buffer :Buffer[A], index :Int, replaced :Int) :Unit =
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

	override def copyTo[A >: X](elems :X)(seq :mutable.Seq[A], index :Int) :Int =
		if (seq.length > index) {
			seq(index) = elems; 1
		} else
			0

	override def copyToArray[A >: X](xs :X)(array :Array[A], index :Int, max :Int) :Int =
		if (max <= 0 || index >= array.length)
			0
		else if (index < 0)
			outOfBounds_!(index, array.length)
		else {
			array(index) = xs
			1
		}

	override def cyclicCopyToArray[A >: X](xs :X)(array :Array[A], index :Int, max :Int) :Int =
		if (index == array.length) copyToArray[A](xs)(array, 0, max)
		else copyToArray[A](xs)(array, index, max)

	override def infoString(elems :X) :String = "{" + elems.localClassName + "}|1|"

	private def readResolve = CollectionLike.singleton
	override def toString = "Values[T, T]"
}




@SerialVersionUID(Ver)
private class CollectionLikeProxy[+X, -Xs](values :CollectionLike[X, Xs]) extends CollectionLike[X, Xs] {
	override def isTraversableAgain(xs :Xs) :Boolean = values.isTraversableAgain(xs)

	override def knownSize(xs :Xs) :Int = values.knownSize(xs)
	override def size(xs :Xs) :Int = values.size(xs)
	override def isEmpty(xs :Xs) :Boolean = values.isEmpty(xs)

	override def iterator(xs :Xs) :Iterator[X] = values.iterator(xs)
	override def toIterableOnce(xs :Xs) :IterableOnce[X] = values.toIterableOnce(xs)
	override def toIterableOnceOps(xs :Xs) :IterableOnceOps[X, generic.Any, Any] = values.toIterableOnceOps(xs)

	override def foreach[U](xs :Xs)(f :X => U) :Unit = values.foreach(xs)(f)
	override def foldLeft[A](xs :Xs)(zero :A)(op :(A, X) => A) :A = values.foldLeft[A](xs)(zero)(op)
	override def reduceLeft[A >: X](xs :Xs)(op :(A, X) => A) :A = values.reduceLeft[A](xs)(op)
	override def reduceLeftOption[A >: X](xs :Xs)(op :(A, X) => A) :Option[A] = values.reduceLeftOption[A](xs)(op)

	override def appendedTo[A >: X, CC[_], C](elems :Xs)(seq :collection.SeqOps[A, CC, C]) :CC[A] =
		values.appendedTo[A, CC, C](elems)(seq)

	override def prependedTo[A >: X, CC[_], C](elems :Xs)(seq :collection.SeqOps[A, CC, C]) :CC[A] =
		values.prependedTo[A, CC, C](elems)(seq)

	override def copiedTo[A >: X, CC[_], C](elems :Xs)(seq :collection.SeqOps[A, CC, C], index :Int) :CC[A] =
		values.copiedTo[A, CC, C](elems)(seq, index)

	override def patchedOn[A >: X, CC[_], C]
	                      (elems :Xs)(seq :collection.SeqOps[A, CC, C], index :Int, replaced :Int) :CC[A] =
		values.patchedOn[A, CC, C](elems)(seq, index, replaced)

	override def insertTo[A >: X](elems :Xs)(buffer :Buffer[A], index :Int) :Unit = values.insertTo[A](elems)(buffer, index)
	override def patchOn[A >: X](elems :Xs)(buffer :Buffer[A], index :Int, replaced :Int) :Unit =
		values.patchOn[A](elems)(buffer, index, replaced)

	override def copyTo[A >: X](elems :Xs)(seq :mutable.Seq[A], index :Int) :Int =
		values.copyTo[A](elems)(seq, index)

	override def copyToArray[A >: X](xs :Xs)(array :Array[A], index :Int, max :Int) :Int =
		values.copyToArray[A](xs)(array, index, max)

	override def cyclicCopyToArray[A >: X](xs :Xs)(array :Array[A], index :Int, max :Int) :Int =
		values.cyclicCopyToArray[A](xs)(array, index, max)

	override def infoString(elems :Xs) :String = values.infoString(elems)
}
