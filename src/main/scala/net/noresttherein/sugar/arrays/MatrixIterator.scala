package net.noresttherein.sugar.arrays

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.unspecialized
import scala.collection.AbstractIterator

import net.noresttherein.sugar.casting.castTypeParamMethods
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.collections.{AbstractSugaredIterator, CountdownIterator, CountdownIteratorFactory, CyclicIteratorFactory, IndexedIterator, IndexedIteratorEquals, IndexedIteratorFactory, Mutability, ReverseCountdownIterator, ReverseCountdownIteratorFactory, ReverseCyclicIteratorFactory, ReverseIndexedIterator, ReverseIndexedIteratorFactory, ValIterator}
import net.noresttherein.sugar.exceptions.{illegal_!, noSuch_!, null_!, outOfBounds_!}
import net.noresttherein.sugar.extensions.classNameMethods
import net.noresttherein.sugar.reflect.Specialized.MultiValue
import net.noresttherein.sugar.typist.kinds.Pow
import net.noresttherein.sugar.util.SingletonSerializationProxy




/** Interface of factories of iterators `I[X]` over two-dimensional arrays of kind `A[A[X]]`.
  * @define Coll `Iterator`
  * @define coll matrix iterator
  * @define source array^2^
  */
trait ArrayLike2IteratorFactory[-A[X] <: ArrayLike[X], +I[X] <: Iterator[X]]
	extends IndexedIteratorFactory[Pow[A]#_2 @uncheckedVariance, I]
{
	//Use of @uncheckedVariance is sound because the only non-covariant ArrayLike subtypes
	// are MutableArray (we won't create an instance for that type) and `Array`.
	def slice[E](array :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, until2 :Int, until1 :Int) :I[E]
	def apply[E](array :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int) :I[E]
}




/** The common interface of factories of iterators over two-dimensional arrays.
  * Only the inner arrays in the range of the created iterator must be initialized,
  * but they must all be of the same length. The exact semantics vary between implementations depending
  * on the order of visiting the elements.
  * Consult the documentation of the object/value of this type for more details.
  * @see [[net.noresttherein.sugar.arrays.MatrixIterator$]]
  * @see [[net.noresttherein.sugar.arrays.ReverseMatrixIterator$]]
  * @see [[net.noresttherein.sugar.arrays.CyclicMatrixIterator$]]
  * @see [[net.noresttherein.sugar.arrays.ReverseCyclicMatrixIterator$]]
  * @tparam A the array kind over which iterators can be created.
  * @tparam I the created iterator kind.
  * @define Coll `Iterator`
  * @define coll matrix iterator
  * @define source array^2^
  */
abstract class AbstractMatrixIteratorFactory[-A[X] <: ArrayLike[X], +I[+X] <: Iterator[X]] private[arrays]
	extends CountdownIteratorFactory[Pow[A]#_2 @uncheckedVariance, I] with ArrayLike2IteratorFactory[A, I]
{
	//Use of @uncheckedVariance is sound because the only non-covariant ArrayLike subtypes
	// are MutableArray (we won't create an instance for that type) and `Array`.
	protected def make[E](array :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, until2 :Int, until1 :Int) :I[E] =
		make(array, from2, from1, (until2 - from2) * array(from2).length + until1 - from1)

	protected def make[E](array :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int) :I[E]

	protected override def make[T](source :A[A[T]] @uncheckedVariance, first :Int, size :Int) :I[T] = {
		val length2 = source.length
		if (length2 == 0 | size <= 0)
			empty(source)
		else {
			val length1 = source(0).length
			if (length1 == 0)
				empty(source)
			else {
				val from2 = first / length1
				val from1 = first - from2 * length1
				make(source, from2, from1, size)
			}
		}
	}


	/** Creates a $Coll iterating over `array` cell range `[array(from2)(from1), array(until2)(until1))`.
	  * If either `from2` or `until2` are outside `[0, array.length]` range, they are clipped to that range.
	  * Indices `from1` and `util1` are similarly clipped to range `[0, length]`, where `length`
	  * is the length of the inner array. For both bounds (after clipping), `(i, length)` is equivalent to `(i + 1, 0)`.
	  * @param array  A two-dimensional array with iterated elements.
	  * @param from2  The lower bound on the indices in `array` with arrays containing the selected elements.
	  * @param from1  The index of the first element of the iterator in `array(from2)`.
	  * @param until2 The upper bound on the indices in `array` with arrays containing the selected elements.
	  * @param until1 The index in `array(until2)` immediately following the last element of the iterator.
	  */
	@throws[NullPointerException]("if array is null, or from2 < array.length and array(from2 max 0) is null.")
	override def slice[E](array :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, until2 :Int, until1 :Int) :I[E] = {
		val outerLength  = array.length
		val clippedFrom2 = math.max(0, from2)
		val clippedFrom1 = math.max(0, from1)
		val clippedUntil2 = math.min(until2, outerLength)
		if (clippedFrom2 >= outerLength | until2 < clippedFrom2 | clippedUntil2 == clippedFrom2 & until1 <= clippedFrom1)
			empty(array)
		else {
			val innerLength  = array(clippedFrom2).length
			if (innerLength == 0)
				empty(array)
			else if (from1 >= innerLength)
				slice(array, clippedFrom2 + 1, 0, until2, until1)
			else if (until1 <= 0 | clippedUntil2 == outerLength)
				slice(array, clippedFrom2, clippedFrom1, clippedUntil2 - 1, innerLength)
			else {
				val clippedUntil1  = {
					val until1LowBound = if (clippedUntil2 == clippedFrom2) clippedFrom1 else 0
					math.max(until1LowBound, math.min(until1, innerLength))
				}
				if (clippedFrom2 == clippedUntil2 & clippedFrom1 == clippedUntil1)
					empty(array)
				else
					make(array, clippedFrom2, clippedFrom1, clippedUntil2, clippedUntil1)
			}
		}
	}

	/** A $Coll iterating over elements `array.flatten.slice(from, until)`.
	  * The bounds specify element indices in the linearized dimension, where the index of element `array(i)(j)`
	  * is `(i * length1 + j)`, and `length1` is the length of the arrays in `array`.
	  * If `length1 == 0`, then an empty $coll is returned immediately.
	  * Index arguments are clipped to range `[0, array.length * length1]`.
	  * The first element in the range is `array(from / length1)(from % length1)`,
	  * and the last one is `array(until / length1)(until % length1)` (if `until % length1 != 0`),
	  * or `array((until / length1) - 1)(length1 - 1)` (if `until % length1 == 0`).
	  * All arrays with the iterated elements must be of length `length1`, but other arrays may be null.
	  * @param array   A two-dimensional array with iterated elements.
	  * @param length1 The length of all non-null inner arrays of `array.`
	  * @param from    The index of the first element of the iterator, in a continuous range `[0, array.length * length1]`.
	  * @param until   The index immediately following the last element of the iterator,
	  *                in a continuous range `[0, array.length * length1]`.
	  */
	@throws[NullPointerException]("if array is null or from < until and until > 0 and from < array.length * length1 " +
	                              "and array(max(from, 0) / length1) is null.")
	def slice[E](length1 :Int, array :A[A[E]] @uncheckedVariance, from :Int, until :Int) :I[E] =
		if (length1 == 0 | from > until | until <= 0)
			empty(array)
		else {
			val length2 = array.length
			val length  = length1.toLong * length2
			val from0   = math.min(length, math.max(0, from).toLong).toInt
			val until0  = math.min(length, math.max(from0, until)).toInt
			val from2   = from0 / length1
			val until2  = until0 / length1
			val from1   = from0 - length1 * from2
			val until1  = until0 - length1 * until2
			if (from0 == until0)
				empty(array)
			else if (until1 == 0)
				make(array, from2, from1, until2 - 1, length1)
			else
				make(array, from2, from1, until2, until1)
		}


	/** A $Coll iterating over elements `array.flatten.slice(from, until)`.
	  * The bounds specify element indices in the linearized dimension, where the index of element `array(i)(j)`
	  * is `(i * array(0).length + j)`.
	  * Index arguments are clipped to range `[0, array.length * array(0).length]`.
	  * The first element in the range is `array(from / length)(from % length)`,
	  * and the last one is `array(until / length)(until % length)` (if `until % length != 0`),
	  * or `array((until / length) - 1)(length - 1)` (if `until % length == 0`), where `length == array(0).length`.
	  * If `length == 0`, an empty $coll is returned instead.
	  * All arrays with the iterated elements must be of the same length, but other arrays may be null.
	  * @param array   A two-dimensional array with iterated elements.
	  * @param from    The index of the first element of the iterator
	  *                in a continuous range `[0, array.length * array(0).length]`.
	  * @param until   The index immediately following the last element of the iterator
	  *                in a continuous range `[0, array.length * array(0).length]`.
	  */
	@throws[NullPointerException]("if from < until and either array is null or array(0) is null.")
	override def slice[E](array :A[A[E]] @uncheckedVariance, from :Int, until :Int) :I[E] =
		if (from >= until | until < 0 | array.length == 0) Empty
		else slice(array(0).length, array, from, until)


	/** A $Coll iterating over at most `size` elements of `array`, starting with `array(from2)(from1)`.
	  *   - If `from2 < 0` it is clipped to zero; if `from2 >= array.length`, an empty $coll is returned immediately.
	  *   - If `from1 < 0`, it is clipped to zero; if `from2 >= array(from2).length`, it is set to zero,
	  *     and `from2` is increased by one.
	  *   - All inner arrays must have the same length.
	  *   - `array.length * array(0).length` must be not greater than `Int.MaxValue`.
	  * @param array A two-dimensional array with iterated elements.
	  * @param from2 The index in `array` of the inner array with the first element of the iterator.
	  * @param from1 The index in `array(from2)` of the first element of the iterator.
	  * @param size  The maximum size of the iterator.
	  */
	override def apply[E](array :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int) :I[E] = {
		val outerLength  = array.length
		if (outerLength == 0 | from2 >= outerLength | size <= 0)
			empty(array)
		else if (from2 < 0)
			apply(array, 0, 0, size)
		else {
			val innerLength  = array(from2).length
			if (from1 >= innerLength)
				apply(array, from2 + 1, 0, size)
			else {
				val clipped1    = math.max(0, from1)
				val from        = from2.toLong * innerLength + clipped1
				val remaining   = outerLength.toLong * innerLength - from
				val clippedSize = math.min(remaining, size).toInt
				if (clippedSize == 0L)
					empty(array)
				else
					make(array, from2, clipped1, clippedSize)
			}
		}
	}

	/** A $Coll iterating over at most `size` elements of `array`,
	  * starting with `array(from / length1)(from % length1)`.
	  * @param length1 The length of the inner arrays in `array`.
	  * @param array   A two-dimensional array with the iterated elements.
	  * @param from    A valid index of the first element of the iterator
	  *                in a continuous range `[0, array.length*length1]`.
	  * @param size    The maximum number of iterated elements.
	  */
	@throws[NullPointerException]("if array is null.")
	def apply[E](length1 :Int, array :A[A[E]] @uncheckedVariance, from :Int, size :Int) :I[E] =
		if (length1 <= 0 | size <= 0)
			empty(array)
		else {
			val from0 = math.max(from, -1)
			val from2 = from0 / length1
			val from1 = from0 - from2 * length1
			apply(array, from2, from1, size)
		}

	/** A $Coll iterating over at most `size` elements of `array`,
	  * starting with `array(from / array(0).length)(from % array(0).length)`.
	  * @param array   A two-dimensional array with the iterated elements.
	  * @param from    A valid index of the first element of the iterator
	  *                in a continuous range `[0, array.length*length1]`.
	  * @param size    The maximum number of iterated elements.
	  */
	@throws[NullPointerException]("if array is null or array(0) is null.")
	@throws[IndexOutOfBoundsException]("if from < 0 or from > array.length * length1.")
	override def apply[E](array :A[A[E]] @uncheckedVariance, from :Int, size :Int) :I[E] =
		if (array.length == 0) empty(array)
		else apply(array(0).length, array, from, size)

	override def apply[E](array :A[A[E]] @uncheckedVariance) :I[E] = {
		val len2 = array.length
		if (len2 == 0)
			Empty
		else
			make(array, 0, 0, len2 * array(0).length)
	}

	protected def empty[E](array :A[A[E]] @uncheckedVariance) :I[E] = Empty
	protected def empty[E] :I[E] = Empty

	protected val Empty :I[Nothing]

	protected final override def lengthOf[E](array :A[A[E]] @uncheckedVariance) :Int = {
		val len2 = array.length
		if (len2 == 0) 0
		else array(0).length * len2
	}
}




@SerialVersionUID(Ver)
private class MatrixIteratorFactory[-A[X] <: ArrayLike[X]] private[arrays]
                                   (name :String, self: => IndexedIteratorFactory[Pow[A]#_2, Iterator])
	extends AbstractMatrixIteratorFactory[A, MatrixIterator]
{
	protected final override def make[E](array :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int)
			:MatrixIterator[E] =
		((array :ArrayLike[_]) match {
			case a :Array[Array[AnyRef]]  => new MatrixIterator(a, from2, from1, size)
			case a :Array[Array[Int]]     => new MatrixIterator(a, from2, from1, size)
			case a :Array[Array[Long]]    => new MatrixIterator(a, from2, from1, size)
			case a :Array[Array[Double]]  => new MatrixIterator(a, from2, from1, size)
			case a :Array[Array[Char]]    => new MatrixIterator(a, from2, from1, size)
			case a :Array[Array[Byte]]    => new MatrixIterator(a, from2, from1, size)
			case a :Array[Array[Float]]   => new MatrixIterator(a, from2, from1, size)
			case a :Array[Array[Short]]   => new MatrixIterator(a, from2, from1, size)
			case a :Array[Array[Boolean]] => new MatrixIterator(a, from2, from1, size)
			case null                     => null_!("Null array passed to MatrixIterator")
		}).castParam[E]

	protected override val Empty :MatrixIterator[Nothing] = {
		//MatrixIterator assumes array(idx2) exists and is not empty.
		val array = new Array[Array[Nothing]](1)
		array(0)  = new Array[Nothing](0)
		new MatrixIterator[Nothing](array, 0, 0, 0)
	}

	override def toString :String = name

	//Our own writeReplace allows deserialization even if the actual class used for the public factory object changes.
	private def writeReplace :Any = new SingletonSerializationProxy(self)
}


@SerialVersionUID(Ver)
private class RefArrayLike2IteratorFactory[-A[X] <: RefArrayLike[X]] private[arrays]
              (override val toString :String, self: => IndexedIteratorFactory[Pow[A]#_2, Iterator])
	extends AbstractMatrixIteratorFactory[A, MatrixIterator]
{
	protected final override def make[E](array :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int)
			:MatrixIterator[E] =
		(array :ArrayLike[_]) match {
			case null => null_!("Cannot create a RefArray2Iterator for a null array.")
			case refs :Array[Array[AnyRef]] =>
				new MatrixIterator(refs, from2, from1, size).asInstanceOf[MatrixIterator[E]]
			case _ =>
				illegal_!("Cannot create a RefArray2Iterator for non RefArray: " + errorString(array) + ".")
		}
	protected override val Empty :MatrixIterator[Nothing] = {
		val array = new Array[Array[Any]](1)
		array(0)  = new Array[Any](0)
		new MatrixIterator(array, 0, 0, 0).asInstanceOf[MatrixIterator[Nothing]]
	}
	private def writeReplace :Any = new SingletonSerializationProxy(self)
}


/** An iterator over a two-dimensional array.
  * Outer array may be of any size; all inner arrays must be of the same, non-zero, size.
  * @param array  an array, whose indices `[from2, until2]` (upper index may be exclusive, if `until1 == 0`)
  *               all contain arrays of the same length.
  * @param idx2   the index of the current inner array.
  * @param idx1   the index of the `head` element in `array(idx2)`.
  */
private[sugar] class MatrixIterator[@specialized(MultiValue) +E]
                                   (array :Array[Array[E]], private[this] var idx2 :Int, private[this] var idx1 :Int,
                                    private[this] var countdown :Int)
	extends AbstractSugaredIterator[E] with ValIterator.Buffered[E]
	   with CountdownIterator[E] with IndexedIteratorEquals[E]
{
	private[this] final val Dim1 = array(idx2).length
	private[this] var curr :Array[E] = array(idx2)
	//Return a MatrixIterator so that method call is specialized and sets the specialized curr field.
	private def setCurr() :MatrixIterator[E] = { curr = array(idx2); this }

	protected final override def source :Array[_] = array
	protected final override def underlyingSize :Int = array.length * Dim1
	protected final override def remaining :Int = countdown
	protected final override def remaining_=(value :Int) :Unit = countdown = value
	protected final override def remaining_--() :Unit = countdown -= 1
	protected final override def index :Int = idx2 * Dim1 + idx1
	protected final override def index_=(value :Int) :Unit = {
		val oldIdx2 = idx2
		idx2 = value / Dim1
		if (idx2 == array.length)
			idx2 -= 1
		idx1 = value - idx2 * Dim1
		if (oldIdx2 != idx2)
			setCurr() //this is a method call, because curr is a specialized field, and this method is not.
	}
	protected final override def advance() :Int = {
		countdown -= 1
		val i1 = idx1
		val i2 = idx2
		idx1 = i1 + 1
		if (i1 == Dim1 - 1 & countdown > 0) {
			idx1 = 0
			idx2 = i2 + 1
			setCurr() //idx2 must be in range because countdown > 0
		}
		i2 * Dim1 + i1
	}


	override def hasNext :Boolean = countdown > 0
	override def next() :E = {
		if (countdown <= 0)
			noSuch_!("Iterator.empty.next()")
		val res = curr(idx1)
		idx1 += 1
		countdown -= 1 //We could u++pdate it only in the if branch, but that would throw off knownSize
		if (idx1 >= Dim1 & countdown > 0) {
			idx1  = 0
			idx2 += 1
			curr = array(idx2) //idx2 must be in range because countdown > 0
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
		if (remaining <= 0)
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
		if (len <= 0 | countdown <= 0 || start >= xs.length)
			0
		else if (start < 0)
			outOfBounds_!(toString + ".copyToArray(" + errorString(xs) + ", " + start + ", " + len + ")")
		else {
			val max = math.min(countdown, math.min(len, xs.length - start))
			var copied = 0
			while (copied < max) {
				val a = array(idx2)
				val n = math.min(max - copied, Dim1 - idx1)
				ArrayLike.copy(a, idx1, xs, start + copied, n)
				copied += n
				idx1  = if (idx1 + n < Dim1) idx1 + n else 0
				idx2 += 1
			}
			countdown -= copied
			copied
		}

	@unspecialized override def safeCopyToArray[U >: E](xs :Array[U], start :Int, len :Int) :Int =
		copyToArray(xs, start, len)

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[MatrixIterator[_]]
	override def clone :MatrixIterator[E] = new MatrixIterator(array, idx2, idx1, countdown)
	//Consider: we should globally decide on whether to use @ or # as the index prefix in all Iterator.toString.
	override def toString :String =
		array.className + "|" + array.length + "*" + Dim1 + "|.iterator@(" + idx2 + ", " + idx1 + ")|" + knownSize + "|"
}




private final class ReverseMatrixIteratorFactory[-A[X] <: ArrayLike[X]] private[arrays]
                    (override val toString :String, self: => IndexedIteratorFactory[Pow[A]#_2, Iterator])
	extends AbstractMatrixIteratorFactory[A, ReverseMatrixIterator]
	   with ReverseCountdownIteratorFactory[Pow[A]#_2 @uncheckedVariance, ReverseMatrixIterator]
{
	protected override def make[E](array :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, until2 :Int, until1 :Int)
			:ReverseMatrixIterator[E] =
	{
		val innerLength = array(from2).length
		make(array, until2, until1 - 1, (until2 - from2) * innerLength - from1 + until1)
	}

	protected override def make[E](array :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int)
			:ReverseMatrixIterator[E] =
	{   //ReverseMatrixIterator expects to get the first element to the constructor.
		((array :ArrayLike[_]) match {
			case a :Array[Array[AnyRef]]  => new ReverseMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Int]]     => new ReverseMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Long]]    => new ReverseMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Double]]  => new ReverseMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Char]]    => new ReverseMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Byte]]    => new ReverseMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Float]]   => new ReverseMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Short]]   => new ReverseMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Boolean]] => new ReverseMatrixIterator(a, from2, from1, size)
			case null                     => null_!("Null array passed to " + this)
		}).castParam[E]
	}

	override def apply[E](array :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int)
			:ReverseMatrixIterator[E] =
		if (from2 < 0 | size <= 0)
			empty(array)
		else {
			val outerLength  = array.length
			if (outerLength == 0)
				empty(array)
			else if (from2 >= outerLength) {
				val innerLength = array(outerLength - 1).length
				make(array, outerLength - 1, innerLength - 1, math.min(outerLength.toLong * innerLength, size).toInt)
			} else if (from1 < 0)
				if (from2 == 0)
					empty(array)
				else
					apply(array, from2 - 1, array(from2 - 1).length, size)
			else {
				val innerLength = array(from2).length
				val clipped1    = math.min(from1, innerLength - 1)
				val from        = from2.toLong * innerLength + clipped1
				val clippedSize = math.min(from + 1, size)
				make(array, from2, clipped1, clippedSize.toInt)
			}
		}

	protected override val Empty :ReverseMatrixIterator[Nothing] = {
		//ReverseMatrixIterator assumes array(idx2) exists and is not empty.
		val array = new Array[Array[Nothing]](1)
		array(0)  = new Array[Nothing](0)
		new ReverseMatrixIterator(array, 0, 0, 0)
	}
	private def writeReplace :Any = new SingletonSerializationProxy(self)
}




/** An iterator going over a two-dimensional array in reverse.
  * Outer array may be of any size; all inner arrays must be of the same, non-zero, size.
  * @param array    an array, whose indices `[from2, until2]` (upper index may be exclusive, if `until1 == 0`)
  *                 all contain arrays of the same length.
  * @param idx2     the index of the current inner array.
  * @param idx1     the index of the `head` element in `array(idx2)`.
  */
private[sugar] class ReverseMatrixIterator[@specialized(MultiValue) +E]
                                          (array :Array[Array[E]],
                                           private[this] var idx2 :Int, private[this] var idx1 :Int,
                                           private[this] var countdown :Int)
	extends AbstractSugaredIterator[E] with ValIterator.Buffered[E]
	   with ReverseCountdownIterator[E] with IndexedIteratorEquals[E]
{
	private[this] final val Dim1 = array(idx2).length
	private[this] var curr = array(idx2)
	private def setCurr() :ReverseMatrixIterator[E] = { curr = array(idx2); this }

	protected final override def source :Array2[_] = array
	protected final override def underlyingSize :Int = array.length * Dim1
	protected final override def remaining :Int = countdown
	protected final override def remaining_=(value :Int) :Unit = countdown = value
	protected final override def index :Int = idx2 * Dim1 + idx1
	protected final override def index_=(value :Int) :Unit = {
		val oldIdx2 = idx2
		idx2 = value / Dim1
		if (idx2 < 0)
			idx2 = 0
		idx1 = value - idx2 * Dim1
		if (oldIdx2 != idx2)
			setCurr() //this is a method call, because curr is a specialized field, and this method is not.
	}
	protected final override def advance() :Int = {
		countdown -= 1
		val i1 = idx1
		val i2 = idx2
		idx1 = i1 - 1
		if (i1 == 0 & countdown > 0) {
			idx1 = Dim1 - 1
			idx2 = i2 - 1
			setCurr() //idx2 must be in range because countdown > 0
		}
		i2 * Dim1 + i1
	}
	override def hasNext :Boolean = countdown > 0
	override def next() :E = {
		if (countdown <= 0)
			noSuch_!("Iterator.empty.next()")
		val res = curr(idx1)
		idx1 -= 1
		countdown -= 1
		if (idx1 == -1 && countdown > 0) {
			idx1  = Dim1 - 1
			idx2 -= 1
			setCurr()
		}
		res
	}

	override def head :E = {
		if (countdown <= 0)
			noSuch_!("Iterator.empty.head")
		curr(idx1)
	}

	override def copyToArray[U >: E](xs :Array[U], start :Int, len :Int) :Int =
		if (len <= 0 || countdown <= 0 || start >= xs.length)
			0
		else if (start < 0)
			outOfBounds_!(toString + ".copyToArray(" + errorString(xs) + ", " + start + ", " + len + ")")
		else {
			val max = math.min(countdown, math.min(len, xs.length - start))
			countdown -= max
			if (idx1 >= max - 1) {
				idx1 -= max - 1
				ArrayLike.reverseCopy(curr, idx1, xs, start, max)
				advance()
			} else {
				var copied = idx1 + 1
				ArrayLike.reverseCopy(curr, 0, xs, start, copied)
				val downTo2 = idx2 - (max - copied) / Dim1
				while (idx2 > downTo2) {
					idx2 -= 1
					ArrayLike.reverseCopy(array(idx2), 0, xs, start + copied, Dim1)
					copied += Dim1
				}
				idx2 -= 1
				if (idx2 >= 0)
					setCurr()
				idx1 = Dim1 - (max - copied) - 1
				if (copied < max)
					ArrayLike.reverseCopy(curr, idx1 + 1, xs, start + copied, max - copied)
			}
			max
		}

	override def safeCopyToArray[U >: E](xs :Array[U], start :Int, len :Int) :Int =
		copyToArray(xs, start, len)

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ReverseMatrixIterator[_]]
	override def clone :ReverseMatrixIterator[E] = new ReverseMatrixIterator[E](array, idx2, idx1, countdown)
	override def toString :String =
		array.className + "|" + array.length + "*" + Dim1 +
			"|.reverseIterator@(" + idx2 + ", " + idx1 + ")|" + knownSize + "|"
}





private sealed abstract class AbstractCyclicMatrixIteratorFactory
                              [-A[X] <: ArrayLike[X], +I[+X] <: Iterator[X]] private[arrays]
                              (override val toString :String, self: => IndexedIteratorFactory[Pow[A]#_2, Iterator])
	extends AbstractMatrixIteratorFactory[A, I]
{
	/** A $coll over a slice of `array`, starting at indices `from2` and `from1` in the outer and inner array,
	  * respectively, and ending immediately before indices `(until2, until1)`. Indices in the outer array
	  * - `from2` and `until2` - are always treated modulo the length of the array. Indices in the inner arrays
	  * - `from1` and `until1` - are instead clipped to range `[0, length]`, where `length` is the length
	  * of all element arrays in `array`. If `from1` or `until1` equals `length` (after clipping), it is set to zero,
	  * an `from2`/`until2` is increased by one modulo `array.length`. If `until2 * length + until1` is less than
	  * `from2 * length + from1` after all these operations, the iteration will wrap at the end of the array
	  * and continue from the beginning (or end, in case of `ReverseCyclicMatrixIterator`). If `from2 == until2`,
	  * and `from1` equals `until1` after clipping to range `[0, length]`, the iterator is empty. Otherwise,
	  * if `from2` equals `until2` modulo `array.length`, the iterator will return all elements in the array.
	  */
	override def slice[E](array :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, until2 :Int, until1 :Int) :I[E] = {
		val length2 = array.length
		if (length2 == 0)
			empty(array)
		else {
			//We need to treat the negative case separately because the expression would overflow for positive values.
			val length2L         = length2.toLong
			val length2Multiple  = length2L << 32 //A multiple greater than Int.MaxValue * 2 to assure positive signs.
			var clippedFrom2     = (length2Multiple + from2) % length2L
			var clippedUntil2    = (length2Multiple + until2) % length2L
			var clippedFrom1     = math.max(from1, 0)
			var clippedUntil1    = math.max(until1, 0)
			val length1          = array(clippedFrom2.toInt).length
			var from2Adjustment  = 0L
			if (from1 >= length1) {
				clippedFrom2 = (clippedFrom2 + 1) % length2L
				clippedFrom1 = 0
				from2Adjustment = 1L
			}
			var until2Adjustment = 0L
			if (until1 >= length1) {
				clippedUntil2 = (clippedUntil2 + 1) % length2L
				clippedUntil1 = 0
				until2Adjustment = 1L
			}
			if (length1 == 0 || from2 + from2Adjustment == until2 + until2Adjustment && clippedFrom1 == clippedUntil1)
				empty(array)
			else
				make(array, clippedFrom2.toInt, clippedFrom1, clippedUntil2.toInt, clippedUntil1)
		}
	}
	override def slice[E](length1 :Int, array :A[A[E]] @uncheckedVariance, from :Int, until :Int) :I[E] =
		if (length1 == 0 || from == until)
			empty(array)
		else {
			val length2 = array.length
			val length  = length2 * length1
			var from0   = from % length
			if (from0 < 0)
				from0 = length + from0
			var until0 = until % length
			if (until0 < 0)
				until0 = length + until0
			val from2  = from0 / length1
			val from1  = from0 - from2 * length1
			var until2 = (until0 - 1) / length1
			val until1 = until0 - until2 * length1
			if (until2 < 0)
				until2 += length2
			make(array, from2, from1, until2, until1)
		}

	override def apply[E](array :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int) :I[E] = {
		val outerLength = array.length
		if (outerLength == 0 || size <= 0)
			empty(array)
		else {
			val clippedFrom2 =
				if (from2 >= 0) from2 % outerLength
				else (outerLength + from2 % outerLength) % outerLength
			val innerLength  = array(clippedFrom2).length
			if (innerLength == 0)
				empty(array)
			else if (from1 >= innerLength)
				apply(array, clippedFrom2 + 1, 0, size)
			else {
				val totalLength  = outerLength * innerLength
				val clippedSize  = math.max(0, math.min(size, totalLength))
				make(array, clippedFrom2, math.max(0, from1), clippedSize)
			}
		}
	}
	override def apply[E](length1 :Int, array :A[A[E]] @uncheckedVariance, from :Int, size :Int) :I[E] = {
		val length2 = array.length
		if (length2 == 0 | length1 <= 0 | size <= 0)
			empty(array)
		else {
			val totalLength = length2.toLong * length1
			val from0 =
				if (from < 0) totalLength + from % totalLength
				else from % totalLength
			val from2 = from0 / length1
			val from1 = from0 - from2 * length1
			apply(array, from2.toInt, from1.toInt, size)
		}
	}

	private def writeReplace :Any = new SingletonSerializationProxy(self)
}


private final class CyclicMatrixIteratorFactory[-A[X] <: ArrayLike[X]] private[arrays]
                    (name :String, self: => IndexedIteratorFactory[Pow[A]#_2, Iterator])
	extends AbstractCyclicMatrixIteratorFactory[A, CyclicMatrixIterator](name, self)
	   with CyclicIteratorFactory[Pow[A]#_2 @uncheckedVariance, CyclicMatrixIterator]
{
	protected override def make[E](array :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, until2 :Int, until1 :Int)
			:CyclicMatrixIterator[E] =
	{
		val outerLength = array.length.toLong
		val innerLength = array(from2).length.toLong
		val from = from2 * innerLength + from1
		val until = until2 * innerLength + until1
		if (from < until)
			make(array, from2, from1, (until - from).toInt)
		else
			make(array, from2, from1, (outerLength * innerLength + until - from).toInt)
	}

	protected override def make[E](array :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int)
			:CyclicMatrixIterator[E] =
		((array :ArrayLike[_]) match {
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

	protected override val Empty :CyclicMatrixIterator[Nothing] = {
		val array = new Array[Array[Nothing]](1)
		array(0)  = new Array[Nothing](0)
		new CyclicMatrixIterator(array, 0, 0, 0)
	}
}


/** An iterator over a two-dimensional array, wrapping at array end if its offset plus size exceeds length of the former.
  * Outer array may be of any size; all inner arrays must be of the same, non-zero, size.
  * @param array  an array, whose indices `[from2, until2]` (upper index may be exclusive, if `until1 == 0`)
  *               all contain arrays of the same length.
  * @param idx2   the index of the current inner array.
  * @param idx1   the index of the `head` element in `array(idx2)`.
  */
private[sugar] class CyclicMatrixIterator[@specialized(MultiValue) +E]
                                         (array :Array[Array[E]],
                                          private[this] var idx2 :Int, private[this] var idx1 :Int,
                                          private[this] var countdown :Int)
	extends AbstractSugaredIterator[E] with ValIterator.Buffered[E]
	   with CountdownIterator[E] with IndexedIteratorEquals[E]
{
	private[this] final val Dim1 = array(idx2).length
	private[this] var curr = array(idx2)
	private def setCurr() :CyclicMatrixIterator[E] = { curr = array(idx2); this }

	protected final override def source :Array2[_] = array
	protected final override def underlyingSize :Int = array.length * Dim1
	protected final override def remaining :Int = countdown
	protected final override def remaining_=(value :Int) :Unit = countdown = value
	protected final override def index :Int = idx2 * Dim1 + idx1
	protected final override def index_=(value :Int) :Unit = {
		val abs  = value & 0xffffffffL //handle overflow gracefully
		val old2 = idx2
		idx2 = (abs / Dim1).toInt
		idx1 = (abs - idx2 * Dim1).toInt
		val Dim2 = array.length
		if (idx2 >= Dim2) {
			idx2 -= Dim2
			if (idx2 >= Dim2)
				idx2 %= Dim2
		}
		if (idx2 != old2)
			setCurr()
	}
	protected final override def advance() :Int = {
		countdown -= 1
		val i2 = idx2
		val i1 = idx1
		idx1 = i1 + 1
		if (i1 == Dim1 - 1 & countdown > 0) {
			idx1 = 0
			idx2 = i2 + 1
			if (i2 == array.length - 1)
				idx2 = 0
			setCurr()
		}
		i2 * Dim1 + i1
	}

	override def hasNext :Boolean = countdown > 0
	override def next() :E = {
		if (countdown <= 0)
			noSuch_!("Iterator.empty.next()")
		val res = curr(idx1)
		idx1 += 1
		countdown -= 1
		if (idx1 == Dim1 && countdown > 0) {
			idx1  = 0
			val i2 = idx2
			idx2 = i2 + 1
			if (i2 == array.length - 1)
				idx2 = 0
			curr = array(idx2)
		}
		res
	}

	override def head :E = {
		if (countdown <= 0)
			noSuch_!("Iterator.empty.head")
		curr(idx1)
	}

	@unspecialized override def copyToArray[U >: E](xs :Array[U], start :Int, len :Int) :Int =
		if (len <= 0 | countdown <= 0 | start >= xs.length)
			0
		else if (start < 0)
			outOfBounds_!(toString + ".copyToArray(" + errorString(xs) + ", " + start + ", " + len + ")")
		else {
			val max = math.min(countdown, math.min(len, xs.length - start))
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
			countdown -= max
			max
		}

	override def toString :String = errorString(array) + ".iterator|" + remaining + "|@(" + idx2 + ", " + idx1 + ")"
}




private final class ReverseCyclicMatrixIteratorFactory[-A[X] <: ArrayLike[X]] private[arrays]
                    (name :String, self: => IndexedIteratorFactory[Pow[A]#_2, Iterator])
	extends AbstractCyclicMatrixIteratorFactory[A, ReverseCyclicMatrixIterator](name, self)
	   with ReverseCyclicIteratorFactory[Pow[A]#_2 @uncheckedVariance, ReverseCyclicMatrixIterator]
{
	protected override def make[E](array :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, until2 :Int, until1 :Int)
			:ReverseCyclicMatrixIterator[E] =
	{
		val outerLength = array.length
		val innerLength = array(from2).length.toLong
		val from = from2 * innerLength + from1
		val until = until2 * innerLength + until1
		val size =
			if (from < until) (until - from).toInt
			else (outerLength * innerLength + until - from).toInt
		if (until1 > 0)
			make(array, until2, until1 - 1, size)
		else if (until2 == 0)
			make(array, outerLength - 1, (innerLength - 1).toInt, size)
		else
			make(array, until2 - 1, (innerLength - 1).toInt, size)
	}

	protected override def make[E](array :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int)
			:ReverseCyclicMatrixIterator[E] =
		((array :ArrayLike[_]) match {
			case a :Array[Array[AnyRef]]  => new ReverseCyclicMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Int]]     => new ReverseCyclicMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Long]]    => new ReverseCyclicMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Double]]  => new ReverseCyclicMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Char]]    => new ReverseCyclicMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Byte]]    => new ReverseCyclicMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Float]]   => new ReverseCyclicMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Short]]   => new ReverseCyclicMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Boolean]] => new ReverseCyclicMatrixIterator(a, from2, from1, size)
			case null                     => null_!("Null array passed to ReverseCyclicMatrixIterator")
		}).castParam[E]

	protected override val Empty :ReverseCyclicMatrixIterator[Nothing] = {
		val array = new Array[Array[Nothing]](1)
		array(0)  = new Array[Nothing](0)
		new ReverseCyclicMatrixIterator(array, 0, 0, 0)
	}
}


/** A reverse iterator over a two-dimensional array, wrapping back at its beginning if its offset plus size
  *  exceeds length of the former. Outer array may be of any size; all inner arrays must be of the same, non zero, size.
  * @param array  an array, whose indices `[from2, until2]` (upper index may be exclusive, if `until1 == 0`)
  *               all contain arrays of the same length.
  * @param idx2   the index of the current inner array.
  * @param idx1   the index of the `head` element in `array(idx2)`.
  */
private[sugar] class ReverseCyclicMatrixIterator[@specialized(MultiValue) +E]
                     (array :Array[Array[E]], private[this] var idx2 :Int, private[this] var idx1 :Int,
                      private[this] var countdown :Int)
	extends AbstractIterator[E] with ValIterator.Buffered[E]
	   with ReverseCountdownIterator[E] with IndexedIteratorEquals[E]
{
	private[this] final val Dim1 = array(idx2).length
	private[this] var curr = array(idx2)
	private def setCurr() :ReverseCyclicMatrixIterator[E] = { curr = array(idx2); this }

	protected final override def source :Array2[_] = array
	protected final override def underlyingSize :Int = array.length * Dim1
	protected final override def remaining :Int = countdown
	protected final override def remaining_=(value :Int) :Unit = countdown = value
	protected final override def index :Int = idx2 * Dim1 + idx1
	protected final override def index_=(value :Int) :Unit = {
		val Dim2 = array.length
		val old2 = idx2
		idx2 = value / Dim1
		idx1 = value - idx2 * Dim1
		if (idx1 < 0) {
			idx1 += Dim1
			idx2 -= 1
		}
		if (idx2 < 0)
			idx2 = (Dim2 + idx2 % Dim2) % Dim2
		if (idx2 != old2)
			setCurr()
	}
	protected final override def advance() :Int = {
		countdown -= 1
		val i1 = idx1
		val i2 = idx2
		idx1 = i1 - 1
		if (i1 == 0 && countdown > 0) {
			idx1 = Dim1 - 1
			idx2 = i2 - 1
			if (i2 == 0)
				idx2 = array.length - 1
			setCurr()
		}
		i2 * Dim1 + i1
	}

	override def hasNext :Boolean = countdown > 0
	override def next() :E = {
		if (countdown <= 0)
			noSuch_!("Iterator.empty.next()")
		val res = curr(idx1)
		idx1 -= 1
		countdown -= 1
		if (idx1 == -1 && countdown > 0) {
			idx1  = Dim1 - 1
			idx2 -= 1
			if (idx2 == -1)
				idx2 = array.length - 1
			curr = array(idx2)
		}
		res
	}
	override def head :E = {
		if (countdown <= 0)
			noSuch_!("Iterator.empty.head")
		curr(idx1)
	}

	override def copyToArray[U >: E](xs :Array[U], start :Int, len :Int) :Int =
		if (len <= 0 || countdown <= 0 || start >= xs.length)
			0
		else if (start < 0)
			outOfBounds_!(toString + ".copyToArray(" + errorString(xs) + ", " + start + ", " + len + ")")
		else {
			val max = math.min(countdown, math.min(len, xs.length - start))
			countdown -= max
			if (idx1 >= max - 1) {
				idx1 -= max - 1
				ArrayLike.reverseCopy(curr, idx1, xs, start, max)
				advance()
			} else {
				var copied = idx1 + 1
				ArrayLike.reverseCopy(curr, 0, xs, start, copied)
				val fullCopyLimit = copied + (max - copied) / Dim1 * Dim1
				while (copied < fullCopyLimit) {
					idx2 -= 1
					if (idx2 < 0)
						idx2 = array.length - 1
					ArrayLike.reverseCopy(array(idx2), 0, xs, start + copied, Dim1)
					copied += Dim1
				}
				idx2 -= 1
				if (idx2 < 0)
					idx2 = array.length - 1
				setCurr()
				idx1 = Dim1 - (max - copied) - 1
				if (copied < max)
					ArrayLike.reverseCopy(curr, idx1 + 1, xs, start + copied, max - copied)
			}
			max
		}

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ReverseCyclicMatrixIterator[_]]
	override def clone :ReverseCyclicMatrixIterator[E] = new ReverseCyclicMatrixIterator(array, idx2, idx1, countdown)
	override def toString :String =
		array.className + "|" + array.length + "*" + Dim1 +
			"|.reverseCyclicIterator@(" + idx2 + ", " + idx1 + ")|" + countdown + "|"
}


