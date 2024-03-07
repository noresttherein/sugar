package net.noresttherein.sugar.arrays

import scala.annotation.unspecialized
import scala.collection.AbstractIterator

import net.noresttherein.sugar.casting.castTypeParamMethods
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.collections.{IndexedIterator, IndexedReverseIterator, IteratorWithDrop, ValIterator}
import net.noresttherein.sugar.{??!, illegal_!, noSuch_!, null_!, outOfBounds_!}
import net.noresttherein.sugar.reflect.Specialized.MultiValue




/** An iterator over a two dimensional array.
  * Outer array may be of any size; all inner arrays must be of the same, non zero, size.
  * @param array  an array, whose indices `[from2, until2]` (upper index may be exclusive, if `until1 == 0`)
  *               all contain arrays of the same length.
  * @param idx2   the index of the current inner array.
  * @param idx1   the index of the `head` element in `array(idx2)`.
  * @param until2 the upper index in the outer array, exclusive if `until1 == 0`, inclusive otherwise.
  * @param until1 the upper index in the inner array, exclusive.
  */
private[sugar] class MatrixIterator[@specialized(MultiValue) +E]
                                   (array :Array[Array[E]], private[this] var idx2 :Int, private[this] var idx1 :Int,
                                    protected[this] final var until2 :Int, protected[this] final var until1 :Int)
	extends AbstractIterator[E] with ValIterator.Buffered[E] with IteratorWithDrop[E] with IndexedIterator[E]
{
	private[this] final val Dim1 = array(idx2).length
	private[this] var curr = array(idx2)
	private[this] var max1 = if (until2 == idx2) until1 else Dim1

	override def hasFastDrop = true
	protected final override def index :Int = idx2 * Dim1 + idx1
	protected final override def index_=(value :Int) :Unit = {
		idx2 = value / Dim1
		idx1 = value - idx2 * Dim1
		curr = array(idx2)
		max1 = if (idx2 == until2) until1 else Dim1
	}

	protected final override def limit :Int = until2 * Dim1 + until1
	protected final override def limit_=(value :Int) :Unit = {
		until2 = value / Dim1
		until1 = value - until2 * Dim1
		if (this.idx2 == until2)
			max1 = until1
	}
	protected final override def underlyingSize :Int = array.length * Dim1

	override def hasNext :Boolean = idx1 < max1
	override def next() :E = {
		if (idx1 >= max1)
			noSuch_!("Iterator.empty.next()")
		val res = curr(idx1)
		idx1 += 1
		if (idx1 >= max1 && idx2 < until2) {
			idx1  = 0
			idx2 += 1
			if (idx2 == until2)
				if (until1 == 0)
					max1 = 0
				else {
					curr = array(idx2)
					max1 = until1
				}
			else {
				curr = array(idx2)
				max1 = Dim1
			}
		}
		res
	}

/*  //For uneven arrays
	@tailrec @inline private def advance() :Unit = {
		idx1  = 0
		idx2 += 1
		if (idx2 == until2 & until1 == 0)
			max1 = 0 //hasNext will now return false
		else {
			curr = array(idx2)
			val len = curr.length
			if (len == 0)
				advance()
			else
				max1 = if (idx2 == until2) until1 else len
		}
	}
*/
	override def head :E = {
		if (idx1 >= max1)
			noSuch_!("Iterator.empty.head")
		curr(idx1)
	}

/*  //Implementation for 'uneven' two dimensional arrays.
	override def drop(n :Int) :Iterator[E] = {
		if (n > 0 & idx1 < max1) {
			@tailrec def drop(n :Int) :Unit =
				if (idx2 == until2)
					idx1 = math.min(until1, n)
				else {
					val len = array(idx2).length
					if (n <= len)
						idx1 = len - n
					else {
						idx2 += 1
						drop(n - len)
					}
				}
			drop(n)
			curr = array(idx2)
			max1 = if (idx2 == until2) until1 else curr.length
		}
		this
	}
*/
	@unspecialized override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
		if (len <= 0 | idx1 >= max1)
			0
		else if (start < 0)
			outOfBounds_!(toString + ".copyToArray(" + errorString(xs) + ", " + start + ", " + len + ")")
		else {
			val max = math.min(len, xs.length - start)
			var copied = 0
			while (copied < max && idx2 <= until2) {
				val a = array(idx2)
				val n = math.min(max - copied, (if (idx2 == until2) until1 else Dim1) - idx1)
				ArrayLike.copy(a, idx1, xs, start + copied, n)
				copied += n
				idx1  = if (idx1 + n < Dim1) idx1 + n else 0
				idx2 += 1
			}
			copied
		}

	override def toString :String = errorString(array) + ".iterator|" + knownSize + "|@(" + idx2 + ", " + idx1 + ")"
}


@SerialVersionUID(Ver)
private[sugar] case object MatrixIterator {
	private def make[E](array :Array[Array[E]], from2 :Int, from1 :Int, until2 :Int, until1 :Int)
			:MatrixIterator[E] =
		(array match {
			case a :Array[Array[AnyRef]]  => new MatrixIterator(a, from2, from1, until2, until1)
			case a :Array[Array[Int]]     => new MatrixIterator(a, from2, from1, until2, until1)
			case a :Array[Array[Long]]    => new MatrixIterator(a, from2, from1, until2, until1)
			case a :Array[Array[Double]]  => new MatrixIterator(a, from2, from1, until2, until1)
			case a :Array[Array[Char]]    => new MatrixIterator(a, from2, from1, until2, until1)
			case a :Array[Array[Byte]]    => new MatrixIterator(a, from2, from1, until2, until1)
			case a :Array[Array[Float]]   => new MatrixIterator(a, from2, from1, until2, until1)
			case a :Array[Array[Short]]   => new MatrixIterator(a, from2, from1, until2, until1)
			case a :Array[Array[Boolean]] => new MatrixIterator(a, from2, from1, until2, until1)
			case null                     => null_!("Null array passed to MatrixIterator")
		}).castParam[E]

	def slice[E](array :Array[Array[E]], from2 :Int, from1 :Int, until2 :Int, until1 :Int) :MatrixIterator[E] = {
		val outerLength  = array.length
		val clippedFrom2 = math.max(0, from2)
		val clippedFrom1 = math.max(0, from1)
		if (clippedFrom2 >= outerLength | until2 < clippedFrom2 | until2 == clippedFrom2 & until1 <= clippedFrom1)
			Empty
		else {
			val innerLength  = array(clippedFrom2).length
			if (innerLength == 0)
				Empty
			else if (from1 >= innerLength)
				slice(array, clippedFrom2 + 1, 0, until2, until1)
			else if (until2 >= outerLength)
				slice(array, clippedFrom2, clippedFrom1, outerLength - 1, innerLength)
			else if (until1 <= 0)
				slice(array, clippedFrom2, clippedFrom1, until2 - 1, innerLength)
			else
				make(array, clippedFrom2, clippedFrom1, until2, until1)
		}
	}

	def slice[E](length1 :Int, array :Array[Array[E]], from :Int, until :Int) :MatrixIterator[E] =
		if (length1 == 0)
			Empty
		else {
			val from2  = from / length1
			val until2 = until / length1
			slice(array, from2, from - length1 * from2, until2, until - length1 * until2)
		}

	def slice[E](array :Array[Array[E]], from :Int, until :Int) :MatrixIterator[E] =
		if (array.length == 0) Empty
		else slice(array(0).length, array, from, until)


	def apply[E](array :Array[Array[E]], from2 :Int, from1 :Int, size :Int) :MatrixIterator[E] = {
		val outerLength  = array.length
		val clippedFrom2 = math.max(0, math.min(from2, outerLength))
		val fromLength   = array(clippedFrom2).length
		val clippedFrom1 = math.max(0, math.min(from1, fromLength))
		val from         = clippedFrom2 * fromLength + clippedFrom1
		val remaining    = outerLength * fromLength - from
		val clippedSize  = math.max(0, math.min(remaining + Int.MinValue, size + Int.MinValue) - Int.MinValue)
		val until2       = from + clippedSize / fromLength
		val until1       = from + clippedSize - until2 * fromLength
		make(array, from2, from1, until2, until1)
	}

	def apply[E](length1 :Int, array :Array[Array[E]], from :Int, size :Int) :MatrixIterator[E] =
		if (length1 == 0)
			Empty
		else {
			val from2 = from / length1
			apply(array, from2, from - from2 * length1, size)
		}

	def apply[E](array :Array[Array[E]], from :Int, size :Int) :MatrixIterator[E] =
		if (array.length == 0) Empty
		else apply(array(0).length, array, from, size)

	private[this] val Empty = new MatrixIterator[Nothing](new Array[Array[Nothing]](0), 0, 0, 0, 0)
}




/** An iterator going over a two dimensional array in reverse.
  * Outer array may be of any size; all inner arrays must be of the same, non zero, size.
  * @param array    an array, whose indices `[from2, until2]` (upper index may be exclusive, if `until1 == 0`)
  *                 all contain arrays of the same length.
  * @param idx2     the index of the current inner array.
  * @param idx1     the index of the `head` element in `array(idx2)`.
  * @param downTo2  the lower index in the outer array (first application of `array`), inclusive.
  * @param downTo1  the lower index in the inner array (second application of `array`), inclusive.
  */
private[sugar] class ReverseMatrixIterator[@specialized(MultiValue) +E]
                                          (array :Array[Array[E]],
                                           private[this] var idx2 :Int, private[this] var idx1 :Int,
                                           protected[this] final var downTo2 :Int, protected[this] final var downTo1 :Int)
	extends AbstractIterator[E] with ValIterator.Buffered[E] with IteratorWithDrop[E] with IndexedReverseIterator[E]
{
	private[this] final val Dim1 = array(idx2).length
	private[this] var curr = array(idx2)
	private[this] var min1 = if (downTo2 == idx2) downTo1 else 0

	override def hasFastDrop = true
	protected final override def index :Int = idx2 * Dim1 + idx1 + 1
	protected final override def index_=(value :Int) :Unit = {
		idx2 = (value - 1) / Dim1
		idx1 = (value - 1) - idx2 * Dim1
		curr = array(idx2)
		min1 = if (idx2 == downTo2) downTo1 else Dim1
	}

	protected final override def limit :Int = downTo2 * Dim1 + downTo1
	protected final override def limit_=(value :Int) :Unit = {
		downTo2 = value / Dim1
		downTo1 = value - downTo2 * Dim1
		if (this.idx2 == downTo2)
			min1 = downTo1
	}
	protected final override def underlyingSize :Int = array.length * Dim1

	override def hasNext :Boolean = idx1 >= min1
	override def next() :E = {
		if (idx1 < min1)
			noSuch_!("Iterator.empty.next()")
		val res = curr(idx1)
		idx1 -= 1
		if (idx1 < 0 && idx2 > downTo2) {
			idx1  = Dim1 - 1
			idx2 -= 1
			curr  = array(idx2)
			min1  = if (idx2 == downTo2) downTo1 else 0
		}
		res
	}

	override def head :E = {
		if (idx1 < min1)
			noSuch_!("Iterator.empty.head")
		curr(idx1)
	}

	override def toString :String =
		errorString(array) + ".reverseIterator|" + knownSize + "|@(" + idx2 + ", " + idx1 + ")"
}


@SerialVersionUID(Ver)
private[sugar] case object ReverseMatrixIterator {
	private def make[E](array :Array[Array[E]], from2 :Int, from1 :Int, until2 :Int, until1 :Int)
			:ReverseMatrixIterator[E] =
		(array match {
			case a :Array[Array[AnyRef]]  => new ReverseMatrixIterator(a, from2, from1, until2, until1)
			case a :Array[Array[Int]]     => new ReverseMatrixIterator(a, from2, from1, until2, until1)
			case a :Array[Array[Long]]    => new ReverseMatrixIterator(a, from2, from1, until2, until1)
			case a :Array[Array[Double]]  => new ReverseMatrixIterator(a, from2, from1, until2, until1)
			case a :Array[Array[Char]]    => new ReverseMatrixIterator(a, from2, from1, until2, until1)
			case a :Array[Array[Byte]]    => new ReverseMatrixIterator(a, from2, from1, until2, until1)
			case a :Array[Array[Float]]   => new ReverseMatrixIterator(a, from2, from1, until2, until1)
			case a :Array[Array[Short]]   => new ReverseMatrixIterator(a, from2, from1, until2, until1)
			case a :Array[Array[Boolean]] => new ReverseMatrixIterator(a, from2, from1, until2, until1)
			case null                     => null_!("Null array passed to ReverseMatrixIterator")
		}).castParam[E]

	def slice[E](array :Array[Array[E]], from2 :Int, from1 :Int, until2 :Int, until1 :Int) :ReverseMatrixIterator[E] = {
		val outerLength  = array.length
		val clippedFrom1 = math.max(0, from1)
		val clippedFrom2 = math.max(0, from2)
		if (clippedFrom2 >= outerLength | until2 < clippedFrom2 | until2 == clippedFrom2 & until1 <= clippedFrom1)
			Empty
		else {
			val innerLength  = array(clippedFrom2).length
			if (innerLength == 0)
				Empty
			else if (from1 > innerLength)
				slice(array, clippedFrom2 + 1, 0, until2, until1)
			else if (until2 >= outerLength)
				slice(array, clippedFrom2, clippedFrom1, outerLength - 1, innerLength)
			else if (until1 <= 0)
				slice(array, clippedFrom2, clippedFrom1, until2 - 1, innerLength)
			else
				make(array, clippedFrom2, clippedFrom1, until2, until1 - 1)
		}
	}

	def slice[E](length1 :Int, array :Array[Array[E]], from :Int, until :Int) :ReverseMatrixIterator[E] =
		if (length1 == 0)
			Empty
		else {
			val from2  = from / length1
			val until2 = until / length1
			slice(array, from2, from - length1 * from2, until2, until - length1 * until2)
		}

	def slice[E](array :Array[Array[E]], from :Int, until :Int) :ReverseMatrixIterator[E] =
		if (array.length == 0) Empty
		else slice(array(0).length, array, from, until)

	def apply[E](array :Array[Array[E]], from2 :Int, from1 :Int, size :Int) :ReverseMatrixIterator[E] = {
		val outerLength = array.length
		if (size <= 0 | from2 < 0 | from2 == 0 & from1 <= 0)
			Empty
		else {
			val innerLength = array(from2).length
			if (from2 >= outerLength)
				apply(array, outerLength - 1, innerLength, size)
			else if (from1 <= 0)
				apply(array, from2 - 1, innerLength, size)
			else {
				val from        = from2 * outerLength + from1
				val clippedSize = math.max(0, math.min(size, from + Int.MinValue) - Int.MinValue)
				val until2      = (from - clippedSize) / innerLength
				val until1      = from - clippedSize - until2 * innerLength
				make(array, from2, from1, until2, until1)
			}
		}
	}

	def apply[E](length1 :Int, array :Array[Array[E]], from :Int, size :Int) :ReverseMatrixIterator[E] =
		if (length1 == 0)
			Empty
		else {
			val from2 = from / length1
			apply(array, from2, from - from2 * length1, size)
		}

	def apply[E](array :Array[Array[E]], from :Int, size :Int) :ReverseMatrixIterator[E] =
		if (array.length == 0) Empty
		else apply(array(0).length, array, from, size)

	private[this] val Empty = new ReverseMatrixIterator(new Array[Array[Nothing]](0), 0, 0, 0, 0)
}






/** An iterator over a two dimensional array, wrapping at array end if its offset plus size exceeds length of the former.
  * Outer array may be of any size; all inner arrays must be of the same, non zero, size.
  * @param array  an array, whose indices `[from2, until2]` (upper index may be exclusive, if `until1 == 0`)
  *               all contain arrays of the same length.
  * @param idx2   the index of the current inner array.
  * @param idx1   the index of the `head` element in `array(idx2)`.
  */
private[sugar] class CyclicMatrixIterator[@specialized(MultiValue) +E]
                                         (array :Array[Array[E]], private[this] var idx2 :Int, private[this] var idx1 :Int,
                                          protected[this] final var remaining :Int)
	extends AbstractIterator[E] with ValIterator.Buffered[E] with IteratorWithDrop[E] with IndexedIterator[E]
{
	private[this] final val Dim1 = array(idx2).length
	private[this] var curr = array(idx2)

	override def hasFastDrop = true
	override def knownSize = remaining
	protected final override def index :Int = 0
	protected final override def index_=(value :Int) :Unit = {
		idx2 = (idx1 + value) / Dim1
		idx1 = idx1 + value - idx2 * Dim1
		curr = array(idx2)
	}

	protected final override def limit :Int = remaining
	protected final override def limit_=(value :Int) :Unit = {
		remaining = value
	}
	protected final override def underlyingSize :Int = ??!

	override def hasNext :Boolean = remaining > 0
	override def next() :E = {
		if (remaining <= 0)
			noSuch_!("Iterator.empty.next()")
		val res = curr(idx1)
		idx1 += 1
		remaining -= 1
		if (idx1 >= Dim1 && remaining > 0) {
			idx1  = 0
			idx2 += 1
			curr = array(idx2)
		}
		res
	}

	override def head :E = {
		if (remaining <= 0)
			noSuch_!("Iterator.empty.head")
		curr(idx1)
	}

	@unspecialized override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
		if (len <= 0 | remaining <= 0)
			0
		else if (start < 0)
			outOfBounds_!(toString + ".copyToArray(" + errorString(xs) + ", " + start + ", " + len + ")")
		else {
			val max = math.min(remaining, math.min(len, xs.length - start))
			var copied = 0
			while (copied < max) {
				val a = array(idx2)
				val n = math.min(max - copied, Dim1 - idx1)
				ArrayLike.copy(a, idx1, xs, start + copied, n)
				copied += n
				idx1  = if (idx1 + n < Dim1) idx1 + n else 0
				idx2 += 1
				if (idx2 >= array.length)
					idx2 = 0
			}
			remaining -= max
			max
		}

	override def toString :String = errorString(array) + ".iterator|" + remaining + "|@(" + idx2 + ", " + idx1 + ")"
}


@SerialVersionUID(Ver)
private[sugar] object CyclicMatrixIterator {
	private def make[E](array :Array[Array[E]], from2 :Int, from1 :Int, size :Int)
			:CyclicMatrixIterator[E] =
		(array match {
			case a :Array[Array[AnyRef]]  => new CyclicMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Int]]     => new CyclicMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Long]]    => new CyclicMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Double]]  => new CyclicMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Char]]    => new CyclicMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Byte]]    => new CyclicMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Float]]   => new CyclicMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Short]]   => new CyclicMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Boolean]] => new CyclicMatrixIterator(a, from2, from1, size)
			case null                     => null_!("Null array passed to CyclicMatrixIterator")
		}).castParam[E]

	@throws[IndexOutOfBoundsException]("if array is empty")
	def apply[E](array :Array[Array[E]], from2 :Int, from1 :Int, size :Int) :CyclicMatrixIterator[E] = {
		val outerLength   = array.length
		val clippedFrom2  = math.max(0, math.min(from2, outerLength))
		val fromLength    = array(clippedFrom2).length
		val clippedSize   = //Compare unsigned
			math.max(Int.MinValue, math.min(size + Int.MinValue, outerLength * fromLength + Int.MinValue)) - Int.MinValue
		val clippedFrom1  = math.max(0, math.min(fromLength, from1))
		make(array, clippedFrom2, clippedFrom1, clippedSize)
	}

	@throws[IndexOutOfBoundsException]("if array is empty")
	def apply[E](length1 :Int, array :Array[Array[E]], from :Int, size :Int) :CyclicMatrixIterator[E] =
		if (length1 == 0)
			Empty
		else {
			val from2 = from / length1
			apply(array, from2, from - from2 * array(from2).length, size)
		}

	@inline def apply[E](array :Array[Array[E]], from :Int, size :Int) :CyclicMatrixIterator[E] =
		if (array.length == 0) Empty
		else apply(array(0).length, array, from, size)

	private[this] val Empty = new CyclicMatrixIterator(new Array[Array[Nothing]](0), 0, 0, 0)
}
