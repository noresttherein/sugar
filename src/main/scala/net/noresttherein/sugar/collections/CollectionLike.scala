package net.noresttherein.sugar.collections

import scala.annotation.tailrec
import scala.collection.{SeqOps, mutable}
import scala.collection.mutable.Buffer

import net.noresttherein.sugar.{funny, outOfBounds_!}
import net.noresttherein.sugar.collections.extensions.{IteratorExtension, SeqFactoryExtension}
import net.noresttherein.sugar.extensions.{cast2TypeParamsMethods, castTypeParamMethods, classNameMethods}




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
	/** Tests whether `elems` can be repeatedly traversed.
	  * @param elems a $coll of this type class.
	  * @return `true` if [[net.noresttherein.sugar.collections.CollectionLike.iterator iterator]]
	  *         returns a new iterator with every call.
	  */
	def isTraversableAgain(elems :Xs) :Boolean = false
//	def isConsumable(xs :Xs) :Boolean
//	def hasFastSlice(xs :Xs) :Boolean
//	def hasFastDrop(xs :Xs) :Boolean
//	def prefersDropOverIterator(xs :Xs) :Boolean //unused

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
		if (size >= 0) size
		else {
			val it  = iterator(elems)
			var len = 0
			while (it.hasNext) { len += 1; it.next() }
			len
		}
	}

	/** Tests whether the $coll is empty.
	  * @param elems a $coll of this type class.
	  * @return `true` if the $coll contains no elements, `false` otherwise.
	  */
	def isEmpty(elems :Xs) :Boolean =
		knownSize(elems) match {
			case -1 => !iterator(elems).hasNext
			case 0 => true
			case _ => false
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
//	def slice(xs :Xs, from :Int, until :Int) :IterableOnce[X] //todo: slice and drop conflict with IterableOnceLike
//	def drop(xs :Xs, n :Int) :IterableOnce[X]
//	def consume[U](xs :Xs)(n :Int)(f :X => U) :IterableOnce[X]

	/** Apply `f` to each element for its side effects
	  * @param elems a $coll of this type class.
	  * @param f     a function to apply to each element in `elems`.
	  */
	def foreach[U](elems :Xs)(f :X => U) :Unit = {
		val it = iterator(elems)
		while (it.hasNext) f(it.next())
	}


	/** Applies a binary operator to a start value and all elements of `elems`, going left to right.
	  * @param elems a $coll of this type class.
	  * @param z     the start  value.
	  * @param op    the binary operator.
	  * @return the result of inserting `op` between consecutive elements of `elems`, going left to right
	  *         with the start value `z` on the left: `op(...op(z, x,,1,,), x,,2,,, ..., x,,n,,)`
	  *         where `x,,1,,, ..., x,,n,,` are the elements of `elems`. Returns `z` if `elems` is empty.
	  */
	def foldLeft[A](elems :Xs)(z :A)(op :(A, X) => A) :A = this match {
		case seq :IndexedSeq[X @unchecked] => foldl[X, A](seq, 0, z, op)
		case _ =>
			var result = z
			val it     = iterator(elems)
			while (it.hasNext) {
				result = op(result, it.next())
			}
			result
	}
	private def foldl[U >: X, A](seq :IndexedSeq[U], start :Int, z :A, op :(A, U) => A) :A = {
		@tailrec def loop(at :Int, end :Int, acc :A) :A =
			if (at == end) acc
			else loop(at + 1, end, op(acc, seq(at)))
		loop(start, seq.length, z)
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
	def reduceLeft[U >: X](elems :Xs)(op :(U, X) => U) :U = this match {
		case seq :IndexedSeq[X @unchecked] if seq.length > 0 => foldl(seq, 1, seq(0), op)
		case _ if knownSize(elems) == 0 => throw new UnsupportedOperationException("empty.reduceLeft")
		case _ => reduceLeftIterator[U](elems)(throw new UnsupportedOperationException("empty.reduceLeft"))(op)
	}
	private final def reduceLeftIterator[U >: X](elems :Xs)(onEmpty: => U)(op :(U, X) => U) :U = {
		val it = iterator(elems)
		if (it.hasNext) {
			var acc :U = it.next()
			while (it.hasNext)
				acc = op(acc, it.next())
			acc
		} else
			onEmpty
	}

	/** Optionally applies a binary operator to all elements of `elems`, going left to right.
	  * @param elems a $coll of this type class.
	  * @param op    a binary operator.
	  * @return an option value containing the result of `reduceLeft(op)` if `elems` is nonempty, `None` otherwise.
	  */
	def reduceLeftOption[U >: X](elems :Xs)(op :(U, X) => U) :Option[U]
//		knownSize(elems) match {
//			case -1 => reduceLeftOptionIterator[U](elems)(op)
//			case 0 => None
//			case _ => Some(reduceLeft[U](elems)(op))
//		}
//	private final def reduceLeftOptionIterator[A >: X](elems :Xs)(op :(A, X) => A) :Option[A] =
//		reduceOptionIterator[X, A](iterator(elems))(op)


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

//	def toIterableOnceOps(elems :Xs) :IterableOnceOps[X, IterableOnce, IterableOnce[X]] =
//		toIterableOnce(elems).toIterableOnceOps

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

/*
//	implicit def array[X]        :Values[X, Array[X]] = arrayPrototype.castParam[X]
//	implicit def arrayLike[X]    :Values[X, ArrayLike[X]] = arrayLikePrototype.castParam[X]
//	implicit def iterableOnce[X] :Values[X, IterableOnce[X]] =
//		iterableOncePrototype.castFrom[Values[Any, IterableOnce[Any]], Values[X, IterableOnce[X]]]

	private[this] val iterableOncePrototype = new IterableOnceValues[Any, IterableOnce[Any]]
	private[this] val arrayPrototype = new ArrayValues[Any]
	private[this] val arrayLikePrototype = new ArrayLikeValues[Any]

	private class IterableOnceValues[X, Xs <: IterableOnce[X]] extends Values[X, Xs] {
		override def isTraversableAgain(xs :Xs) :Boolean = xs.toBasicOps.isTraversableAgain
//		override def isConsumable(xs :Xs) :Boolean = xs match {
//			case _  :LinearSeq[_] => true
//			case _ :Iterator[_]   => true
//			case _ => HasFastSlice(xs)
//		}
//		override def hasFastSlice(xs :Xs) :Boolean = HasFastSlice(xs)
//		override def hasFastDrop(xs :Xs) :Boolean = HasFastSlice.hasFastDrop(xs)
//		override def prefersDropOverIterator(xs :Xs) :Boolean = HasFastSlice.preferDropOverIterator(xs)

		override def knownSize(xs :Xs) :Int = xs.knownSize
		override def size(xs :Xs) :Int = xs.toBasicOps.size
		override def isEmpty(xs :Xs) :Boolean = xs.toBasicOps.isEmpty

		override def iterator(xs :Xs) :Iterator[X] = xs.iterator
		override def toIterableOnce(xs :Xs) :IterableOnce[X] = xs

//		override def slice(xs :Xs, from :Int, until :Int) :IterableOnce[X] = xs match {
//			case HasFastSlice(items)        => items.slice(from, until)
//			case it :Iterable[X @unchecked] => it.slice(from, until)
//			case _                          => xs.iterator.slice(from, until)
//		}
//		override def drop(xs :Xs, n :Int) :IterableOnce[X] = xs match {
//			case HasFastSlice(items)        => items.drop(n)
//			case it :Iterable[X @unchecked] => it.drop(n)
//			case _                          => xs.iterator.drop(n)
//		}
//		override def consume[U](xs :Xs)(n :Int)(f :X => U) :IterableOnce[X] = xs match {
//			case HasFastSlice(items) =>
//				val (init, rest) = items.splitAt(n)
//				init.toBasicOps.foreach(f)
//				rest
//			case _ =>
//				val it = xs.iterator
//				var i = 0
//				while (i < n && it.hasNext) {
//					f(it.next())
//					i += 1
//				}
//				it
//		}

		override def foreach[U](xs :Xs)(f :X => U) :Unit = xs.toBasicOps.foreach(f)
		override def foldLeft[A](xs :Xs)(zero :A)(op :(A, X) => A) :A = xs.toBasicOps.foldLeft(zero)(op)
		override def reduceLeft[A >: X](xs :Xs)(op :(A, X) => A) :A = xs.toBasicOps.reduceLeft(op)
		override def reduceLeftOption[A >: X](xs :Xs)(op :(A, X) => A) :Option[A] =
			xs.toBasicOps.reduceLeftOption(op)

		override def appendedTo[A >: X, CC[_], C](elems :Xs)(seq :SeqOps[A, CC, C]) :CC[A] = seq :++ elems
		override def prependedTo[A >: X, CC[_], C](elems :Xs)(seq :SeqOps[A, CC, C]) :CC[A] = elems ++: seq
		override def copiedTo[A >: X, CC[_], C](elems :Xs)(seq :SeqOps[A, CC, C], index :Int) :CC[A] =
			seq.updatedAll(index, elems)

		override def patchedOn[A >: X, CC[_], C](elems :Xs)(seq :SeqOps[A, CC, C], index :Int, replaced :Int) :CC[A] =
			seq.patch(index, elems, replaced)

		override def appendTo[A >: X](elems :Xs)(buffer :Buffer[A]) :Unit = buffer ++= elems
		override def prependTo[A >: X](elems :Xs)(buffer :Buffer[A]) :Unit = elems ++=: buffer
		override def insertTo[A >: X](elems :Xs)(buffer :Buffer[A], index :Int) :Unit = buffer.insertAll(index, elems)
		override def patchOn[A >: X](elems :Xs)(buffer :Buffer[A], index :Int, replaced :Int) :Unit =
			buffer.patchInPlace(index, elems, replaced)

//		override def setAll[A >: X](xs :Xs)(seq :mutable.Seq[A], index :Int) :Unit = seq match {
//			case list :LinearSeq[A] =>
//				xs.toIterableOnceOps.foldLeft(list.drop(index)) { (tail, x) => tail(0) = x; tail }
//			case _ =>
//				xs.toIterableOnceOps.foldLeft(index) { (i, x) => seq(i) = x; i + 1 }
//		}

		override def copyTo[A >: X](elems :Xs)(seq :mutable.Seq[A], index :Int) :Int =
			if (elems.toBasicOps.isEmpty)
				0
			else seq match {
//				case _ :SugaredSeq[A] =>
				case _ :collection.LinearSeq[A] =>
					var copied = 0
					elems.foldLeftUntil(seq.drop(index))(_.isEmpty) {
						(tail, elem) => tail(0) = elem; copied += 1; tail.tail
					}
					copied
				case MutableArray.Wrapped(array) =>
					elems.toBasicOps.copyToArray(array.castFrom[MutableArray[A], Array[A]], index)

				case MutableArray.Wrapped.Slice(array, from, until) =>
					elems.toBasicOps.copyToArray(array.castFrom[MutableArray[A], Array[A]], from + index, until - from)
//
//				case arr :mutable.ArraySeq[A] =>
//					xs.toBasicOps.copyToArray(arr.array.castFrom[Array[_], Array[A]], index)
//
//				case ErasedArray.Wrapped.Slice(arr :Array[A @unchecked], from, until) =>
//					xs.toBasicOps.copyToArray(arr, from + index, until - from - index)

				case _ :mutable.IndexedSeqOps[A, generic.Any, _] => elems match {
					case _ if index >= seq.length =>
						0
					case ApplyPreferred(indexed) =>
						val end = math.min(seq.length - index, indexed.length)
						var i   = 0
						while (i < end) {
							seq(index + i) = indexed(i)
							i += 1
						}
						i
					case _ if elems.knownSize >= 0 && elems.knownSize <= seq.length - index =>
						elems.toBasicOps.foldLeft(index) { (i, elem) => seq(index + i) = elem; i + 1 } - index
					case _ =>
						val end  = seq.length
						var i    = index
						val iter = elems.iterator
						while (i < end && iter.hasNext) {
							seq(i) = iter.next()
							i += 1
						}
						i - index
				}
				case buffer :Buffer[A] => seq match {
					case _ if index >= buffer.length =>
						0
					case _ :Buffer[A] if elems.knownSize >= 0 =>
						val bufferSize = buffer.length
						val size  = knownSize(elems)
						val space = bufferSize - index
						if (size <= space) {
							buffer.patchInPlace(index, elems, size)
							size
						} else {
							buffer.patchInPlace(index, elems.iterator.take(space), space)
							space
						}
					case _ :ListBuffer[A] =>
						val bufferSize = buffer.length
						val iter = elems.iterator.take(bufferSize - index).counting
						buffer.insertAll(index, iter)
						buffer.remove(index + iter.total, iter.total)
						iter.total
					case _ if isTraversableAgain(elems) =>
						val suffixSpace = buffer.length - index
						val size        = elems.toBasicOps.size
						if (size <= suffixSpace) {
							buffer.patchInPlace(index, elems, size)
							size
						} else {
							buffer.patchInPlace(index, elems.iterator.take(suffixSpace), suffixSpace)
							suffixSpace
						}
					case _ =>
						val patch = elems.toBasicOps to TemporaryBuffer
						buffer.patchInPlace(index, patch, patch.length)
						patch.length
				}
				case _ =>
					val length = seq.length
					elems.toBasicOps.foldLeftUntil(index)(_ >= length) {
						(idx, elem) => seq(idx) = elem; idx + 1
					} - index
			}
//		override def copyRangeToArray[A >: X](array :Array[A], start :Int, from :Int, until :Int, xs :Xs) :Int =
//			xs.toIterableOnceOps.copyRangeToArray(array, start, from, until, xs)

		override def copyToArray[A >: X](xs :Xs)(array :Array[A], index :Int, max :Int) :Int =
			xs.toBasicOps.copyToArray(array, index, max)

		override def cyclicCopyToArray[A >: X](xs :Xs)(array :Array[A], index :Int, max :Int) :Int = xs match {
			case items :Iterable[X] => items.cyclicCopyToArray(array, index, max)
			case _                  => xs.iterator.cyclicCopyToArray(array, index, max)
		}

		override def infoString(elems :Xs) :String = util.errorString(elems)
	}

	private class ArrayValues[X] extends Values[X, Array[X]] {
		override def isTraversableAgain(xs :Array[X]) :Boolean = true
//		override def isConsumable(xs :Array[X]) :Boolean = false
//		override def hasFastSlice(xs :Array[X]) :Boolean = false
//		override def hasFastDrop(xs :Array[X]) :Boolean = false
//		override def prefersDropOverIterator(xs :Array[X]) :Boolean = true

		override def knownSize(xs :Array[X]) :Int = xs.length
		override def size(xs :Array[X]) :Int = xs.length
		override def isEmpty(xs :Array[X]) :Boolean = xs.length == 0

		override def iterator(xs :Array[X]) :Iterator[X] = ArrayIterator(xs) //xs.iterator
		override def toIterableOnce(xs :Array[X]) :IterableOnce[X] = ErasedArray.Wrapped(xs)
//		override def slice(xs :Array[X], from :Int, until :Int) :IterableOnce[X] = ArraySlice.of(xs, from, until)
//		override def drop(xs :Array[X], n :Int) :IterableOnce[X] = ArraySlice.of(xs, n, xs.length)
//		override def consume[U](xs :Array[X])(n :Int)(f :X => U) :ArraySlice[X] = {
//			var i   = 0
//			val end = math.max(0, math.min(xs.length, n))
//			while (i < end) {
//				f(xs(i))
//				i += 1
//			}
//			ArraySlice.of(xs, 0, end)
//		}

		override def foreach[U](xs :Array[X])(f :X => U) :Unit = xs.foreach(f)
		override def foldLeft[A](xs :Array[X])(zero :A)(op :(A, X) => A) :A = xs.foldLeft(zero)(op)
		override def reduceLeft[A >: X](xs :Array[X])(op :(A, X) => A) :A = xs.reduceLeft(op)
		override def reduceLeftOption[A >: X](xs :Array[X])(op :(A, X) => A) :Option[A] = xs.reduceLeftOption(op)

		override def appendedTo[A >: X, CC[_], C](elems :Array[X])(seq :SeqOps[A, CC, C]) :CC[A] = seq :++ elems
		override def prependedTo[A >: X, CC[_], C](elems :Array[X])(seq :SeqOps[A, CC, C]) :CC[A] = elems ++: seq
		override def copiedTo[A >: X, CC[_], C](elems :Array[X])(seq :SeqOps[A, CC, C], index :Int) :CC[A] =
			seq.updatedAll(index, elems)

		override def patchedOn[A >: X, CC[_], C](elems :Array[X])(seq :SeqOps[A, CC, C], index :Int, replaced :Int) :CC[A] =
			seq.patch(index, elems, replaced)

		override def appendTo[A >: X](elems :Array[X])(buffer :Buffer[A]) :Unit = buffer ++= elems
		override def prependTo[A >: X](elems :Array[X])(buffer :Buffer[A]) :Unit = elems ++=: buffer
		override def insertTo[A >: X](elems :Array[X])(buffer :Buffer[A], index :Int) :Unit = buffer.insertAll(index, elems)

		override def patchOn[A >: X](elems :Array[X])(buffer :Buffer[A], index :Int, replaced :Int) :Unit =
			buffer.patchInPlace(index, elems, replaced)

//		override def setAll[A >: X](xs :Array[X])(seq :mutable.Seq[A], index :Int) :Unit = seq match {
//			case list :collection.LinearSeq[A] =>
//				var tail = list.drop(index)
//				var i = 0; val end = xs.length
//				while (i < end) {
//					tail(0) = xs(i)
//					i += 1
//					tail = tail.tail
//				}
//			case _ =>
//				var i = 0; val end = xs.length
//				while (i < end) {
//					seq(i) = xs(i)
//					i += 1
//				}
//		}

		override def copyTo[A >: X](elems :Array[X])(seq :mutable.Seq[A], index :Int) :Int = seq match {
			case MutableArray.Wrapped(array) =>
				val arrayLength = array.length
				val copied = math.min(elems.length, arrayLength - math.min(index, arrayLength))
				ArrayLike.copy(elems, 0, array, index, copied)
				copied
			case MutableArray.Wrapped.Slice(array, from, until) =>
				val seqSize = until - from
				val copied = math.min(elems.length, seqSize - math.min(index, seqSize))
				ArrayLike.copy(elems, 0, array, from + index, copied)
				copied
			case _ :collection.LinearSeq[A] =>
				val xsSize = elems.length
				var tail = seq.drop(index)
				var i = 0
				while (i < xsSize && tail.nonEmpty) {
					tail(0) = elems(i)
					i += 1
					tail = tail.tail
				}
				i
			case _ :mutable.IndexedSeqOps[A, generic.Any, _] =>
				val seqSize = seq.length
				val copied = math.min(elems.length, seqSize - math.min(index, seqSize))
				var i = 0
				while (i < copied) {
					seq(index + i) = elems(i)
					i += 1
				}
				i
			case buffer :Buffer[A] =>
				val bufferSize = buffer.length
				val size       = elems.length
				val space      = bufferSize - index
				if (size <= space) {
					buffer.patchInPlace(index, ArraySlice.of(elems), size)
					size
				} else {
					buffer.patchInPlace(index, ArraySlice.of(elems, 0, space), space)
					space
				}
			case _ =>
				val xsSize  = elems.length
				val seqSize = seq.length
				var i = 0; var j = index
				while (i < xsSize & j < seqSize) {
					seq(j) = elems(i)
					i += 1
					j += 1
				}
				i
		}

		override def copyToArray[A >: X](xs :Array[X])(array :Array[A], index :Int, max :Int) :Int =
			if (max <= 0 || xs.length == 0 || index > array.length)
				0
			else if (index < 0)
				outOfBounds_!(index, array.length)
			else {
				val copied = math.min(max, math.min(array.length - index, xs.length))
				ArrayLike.copy(xs, 0, array, index, copied)
				copied
			}

		override def cyclicCopyToArray[A >: X](xs :Array[X])(array :Array[A], index :Int, max :Int) :Int =
			xs.cyclicCopyToArray(array, index, max)

		override def infoString(elems :Array[X]) :String = util.errorString(elems)
	}

	private class ArrayLikeValues[X] extends Values[X, ArrayLike[X]] {
		override def isTraversableAgain(xs :ArrayLike[X]) :Boolean = true
//		override def isConsumable(xs :ArrayLike[X]) :Boolean = false
//		override def hasFastSlice(xs :ArrayLike[X]) :Boolean = false
//		override def hasFastDrop(xs :ArrayLike[X]) :Boolean = false
//		override def prefersDropOverIterator(xs :ArrayLike[X]) :Boolean = true

		override def knownSize(xs :ArrayLike[X]) :Int = xs.length
		override def size(xs :ArrayLike[X]) :Int = xs.length
		override def isEmpty(xs :ArrayLike[X]) :Boolean = xs.length == 0

		override def iterator(xs :ArrayLike[X]) :Iterator[X] = xs.iterator
		override def toIterableOnce(xs :ArrayLike[X]) :IterableOnce[X] = ArrayLike.Wrapped(xs)
//		override def drop(xs :ArrayLike[X], n :Int) :IterableOnce[X] = ArrayLikeSlice.of(xs, n, xs.length)
//		override def slice(xs :ArrayLike[X], from :Int, until :Int) :IterableOnce[X] =
//			ArrayLikeSlice.of(xs, from, until)

		override def foreach[U](xs :ArrayLike[X])(f :X => U) :Unit = xs.foreach(f)
		override def foldLeft[A](xs :ArrayLike[X])(zero :A)(op :(A, X) => A) :A = xs.foldLeft(zero)(op)
		override def reduceLeft[A >: X](xs :ArrayLike[X])(op :(A, X) => A) :A = xs.reduceLeft(op)
		override def reduceLeftOption[A >: X](xs :ArrayLike[X])(op :(A, X) => A) :Option[A] = xs.reduceLeftOption(op)
//		override def consume[U](xs :ArrayLike[X])(n :Int)(f :X => U) :ArrayLikeSlice[X] = {
//			var i = 0
//			val end = math.max(0, math.min(xs.length, n))
//			while (i < end) {
//				f(xs(i))
//				i += 1
//			}
//			ArrayLikeSlice.of(xs, 0, end)
//		}

		override def appendedTo[A >: X, CC[_], C](elems :ArrayLike[X])(seq :SeqOps[A, CC, C]) :CC[A] = seq :++ elems.toSeq
		override def prependedTo[A >: X, CC[_], C](elems :ArrayLike[X])(seq :SeqOps[A, CC, C]) :CC[A] = elems.toSeq ++: seq
		override def copiedTo[A >: X, CC[_], C](elems :ArrayLike[X])(seq :SeqOps[A, CC, C], index :Int) :CC[A] =
			seq.updatedAll(index, elems.toSeq)

		override def patchedOn[A >: X, CC[_], C](elems :ArrayLike[X])(seq :SeqOps[A, CC, C], index :Int, replaced :Int) :CC[A] =
			seq.patch(index, elems.toSeq, replaced)

		override def appendTo[A >: X](elems :ArrayLike[X])(buffer :Buffer[A]) :Unit = buffer ++= elems.toSeq
		override def prependTo[A >: X](elems :ArrayLike[X])(buffer :Buffer[A]) :Unit = elems.toSeq ++=: buffer
		override def insertTo[A >: X](elems :ArrayLike[X])(buffer :Buffer[A], index :Int) :Unit =
			buffer.insertAll(index, elems.toSeq)

		override def patchOn[A >: X](elems :ArrayLike[X])(buffer :Buffer[A], index :Int, replaced :Int) :Unit =
			buffer.patchInPlace(index, elems.toSeq, replaced)

//		override def setAll[A >: X](xs :ArrayLike[X])(seq :mutable.Seq[A], index :Int) :Unit = seq match {
//			case list :collection.LinearSeq[A] =>
//				var tail = list.drop(index)
//				var i = 0; val end = xs.length
//				while (i < end) {
//					tail(0) = xs(i)
//					i += 1
//					tail = tail.tail
//				}
//			case _ =>
//				var i = 0; val end = xs.length
//				while (i < end) {
//					seq(i) = xs(i)
//					i += 1
//				}
//		}

		override def copyTo[A >: X](elems :ArrayLike[X])(seq :mutable.Seq[A], index :Int) :Int = seq match {
//			case sugared :SugaredSeq[A] =>
			case MutableArray.Wrapped(array) =>
				val arrayLength = array.length
				val copied = math.min(elems.length, arrayLength - math.min(index, arrayLength))
				ArrayLike.copy(elems, 0, array, index, copied)
				copied
			case MutableArray.Wrapped.Slice(array, from, until) =>
				val seqSize = until - from
				val copied = math.min(elems.length, seqSize - math.min(index, seqSize))
				ArrayLike.copy(elems, 0, array, from + index, copied)
				copied
			case _ :collection.LinearSeq[A] =>
				val xsSize = elems.length
				var tail = seq.drop(index)
				var i = 0
				while (i < xsSize && tail.nonEmpty) {
					tail(0) = elems(i)
					i += 1
					tail = tail.tail
				}
				i
			case _ :mutable.IndexedSeqOps[A, generic.Any, _] =>
				val seqSize = seq.length
				val copied = math.min(elems.length, seqSize - math.min(index, seqSize))
				var i = 0
				while (i < copied) {
					seq(index + i) = elems(i)
					i += 1
				}
				i
			case buffer :Buffer[A] =>
				val bufferSize = buffer.length
				val size       = elems.length
				val space      = bufferSize - index
				if (size <= space) {
					buffer.patchInPlace(index, ArrayLikeSlice.of(elems), size)
					size
				} else {
					buffer.patchInPlace(index, ArrayLikeSlice.of(elems, 0, space), space)
					space
				}
			case _ =>
				val xsSize  = elems.length
				val seqSize = seq.length
				var i = 0; var j = index
				while (i < xsSize & j < seqSize) {
					seq(j) = elems(i)
					i += 1
					j += 1
				}
				i
		}

		override def copyToArray[A >: X](xs :ArrayLike[X])(array :Array[A], index :Int, max :Int) :Int =
			xs.copyToArray(array, index, max)

		override def cyclicCopyToArray[A >: X](xs :ArrayLike[X])(array :Array[A], index :Int, max :Int) :Int =
			xs.cyclicCopyToArray(array, index, max)

		override def infoString(elems :ArrayLike[X]) :String = util.errorString(elems)
	}
*/

}



@SerialVersionUID(Ver)
private class SingleValue[X] extends CollectionLike[X, X] {
	override def isTraversableAgain(xs :X) :Boolean = true
//		override def isConsumable(xs :X) :Boolean = false
//		override def hasFastSlice(xs :X) :Boolean = true
//		override def hasFastDrop(xs :X) :Boolean = true
//		override def prefersDropOverIterator(xs :X) :Boolean = true

	override def knownSize(xs :X) :Int = 1
	override def size(xs :X) :Int = 1
	override def isEmpty(xs :X) :Boolean = false

	override def iterator(xs :X) :Iterator[X] = Iterator.single(xs)
	override def toIterableOnce(xs :X) :IterableOnce[X] = Seq.single(xs)
//		override def drop(xs :X, n :Int) :IterableOnce[X] = if (n >= 1) None else Some(xs)
//		override def slice(xs :X, from :Int, until :Int) :IterableOnce[X] =
//			if (until <= from | until <= 0 | from > 0) None else Some(xs)
//
//		override def consume[U](xs :X)(n :Int)(f :X => U) :IterableOnce[X] =
//			if (n <= 0)
//				Some(xs)
//			else {
//				f(xs)
//				None
//			}

	override def foreach[U](xs :X)(f :X => U) :Unit = f(xs)
	override def foldLeft[A](xs :X)(zero :A)(op :(A, X) => A) :A = op(zero, xs)
	override def reduceLeft[A >: X](xs :X)(op :(A, X) => A) :A = xs
	override def reduceLeftOption[A >: X](xs :X)(op :(A, X) => A) :Option[A] = Some(xs)

	override def appendedTo[A >: X, CC[_], C](elems :X)(seq :SeqOps[A, CC, C]) :CC[A] = seq :+ elems
	override def prependedTo[A >: X, CC[_], C](elems :X)(seq :SeqOps[A, CC, C]) :CC[A] = elems +: seq
	override def copiedTo[A >: X, CC[_], C](elems :X)(seq :SeqOps[A, CC, C], index :Int) :CC[A] = seq.updated(index, elems)
	override def patchedOn[A >: X, CC[_], C](elems :X)(seq :SeqOps[A, CC, C], index :Int, replaced :Int) :CC[A] =
		if (replaced == 1) copiedTo[A, CC, C](elems)(seq, index)
//			else if (hasFastTake(seq)) seq.take(index) :+ xs :++ seq.drop(index + replace)
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
//		override def setAll[A >: X](xs :X)(seq :mutable.Seq[A], index :Int) :Unit = seq(index) = xs

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
//		override def copyRangeToArray[A >: X](array :Array[A], start :Int, from :Int, until :Int, max :Int, xs :X) :Int =
//			if (max <= 0 | until <= 0 | until <= from | from > 0 || start >= array.length)
//				0
//			else if (start < 0)
//				outOfBounds_!(start, array.length)
//			else {
//				array(start) = xs
//				1
//			}

	override def infoString(elems :X) :String = "{" + elems.localClassName + "}|1|"

	private def readResolve = CollectionLike.singleton
	override def toString = "Values[T, T]"
}




@SerialVersionUID(Ver)
private class CollectionLikeProxy[+X, -Xs](values :CollectionLike[X, Xs]) extends CollectionLike[X, Xs] {
	override def isTraversableAgain(xs :Xs) :Boolean = values.isTraversableAgain(xs)
//		override def isConsumable(xs :Xs) :Boolean = values.isConsumable(xs)
//		override def hasFastSlice(xs :Xs) :Boolean = values.hasFastSlice(xs)
//		override def hasFastDrop(xs :Xs) :Boolean = values.hasFastSlice(xs)
//		override def prefersDropOverIterator(xs :Xs) :Boolean = values.prefersDropOverIterator(xs)

	override def knownSize(xs :Xs) :Int = values.knownSize(xs)
	override def size(xs :Xs) :Int = values.size(xs)
	override def isEmpty(xs :Xs) :Boolean = values.isEmpty(xs)

	override def iterator(xs :Xs) :Iterator[X] = values.iterator(xs)
	override def toIterableOnce(xs :Xs) :IterableOnce[X] = values.toIterableOnce(xs)
//		override def slice(xs :Xs, from :Int, until :Int) :IterableOnce[X] = values.slice(xs, from, until)
//		override def drop(xs :Xs, n :Int) :IterableOnce[X] = values.drop(xs, n)
//		override def consume[U](xs :Xs)(n :Int)(f :X => U) :IterableOnce[X] = values.consume(xs)(n)(f)

	override def foreach[U](xs :Xs)(f :X => U) :Unit = values.foreach(xs)(f)
	override def foldLeft[A](xs :Xs)(zero :A)(op :(A, X) => A) :A = values.foldLeft[A](xs)(zero)(op)
	override def reduceLeft[A >: X](xs :Xs)(op :(A, X) => A) :A = values.reduceLeft[A](xs)(op)
	override def reduceLeftOption[A >: X](xs :Xs)(op :(A, X) => A) :Option[A] = values.reduceLeftOption[A](xs)(op)

	override def appendedTo[A >: X, CC[_], C](elems :Xs)(seq :SeqOps[A, CC, C]) :CC[A] =
		values.appendedTo[A, CC, C](elems)(seq)

	override def prependedTo[A >: X, CC[_], C](elems :Xs)(seq :SeqOps[A, CC, C]) :CC[A] =
		values.prependedTo[A, CC, C](elems)(seq)

	override def copiedTo[A >: X, CC[_], C](elems :Xs)(seq :SeqOps[A, CC, C], index :Int) :CC[A] =
		values.copiedTo[A, CC, C](elems)(seq, index)

	override def patchedOn[A >: X, CC[_], C](elems :Xs)(seq :SeqOps[A, CC, C], index :Int, replaced :Int) :CC[A] =
		values.patchedOn[A, CC, C](elems)(seq, index, replaced)

	override def insertTo[A >: X](elems :Xs)(buffer :Buffer[A], index :Int) :Unit = values.insertTo[A](elems)(buffer, index)
	override def patchOn[A >: X](elems :Xs)(buffer :Buffer[A], index :Int, replaced :Int) :Unit =
		values.patchOn[A](elems)(buffer, index, replaced)

//		override def setAll[A >: X](xs :Xs)(seq :mutable.Seq[A], index :Int) :Unit = values.setAll[A](xs)(seq, index)
	override def copyTo[A >: X](elems :Xs)(seq :mutable.Seq[A], index :Int) :Int =
		values.copyTo[A](elems)(seq, index)

	override def copyToArray[A >: X](xs :Xs)(array :Array[A], index :Int, max :Int) :Int =
		values.copyToArray[A](xs)(array, index, max)

	override def cyclicCopyToArray[A >: X](xs :Xs)(array :Array[A], index :Int, max :Int) :Int =
		values.cyclicCopyToArray[A](xs)(array, index, max)

	override def infoString(elems :Xs) :String = values.infoString(elems)
}
