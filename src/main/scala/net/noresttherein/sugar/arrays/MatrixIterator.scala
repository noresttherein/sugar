package net.noresttherein.sugar.arrays

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.unspecialized
import scala.collection.AbstractIterator

import net.noresttherein.sugar.casting.castTypeParamMethods
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.collections.{AbstractSugaredIterator, CountdownIterator, CyclicMatrixSliceFactory, IndexedIteratorEquals, MatrixSliceFactory, ReverseCountdownIterator, ReverseCyclicMatrixSliceFactory, ReverseMatrixSliceFactory, Slice2DFactory, ValIterator}
import net.noresttherein.sugar.exceptions.{illegal_!, noSuch_!, null_!, outOfBounds_!}
import net.noresttherein.sugar.extensions.classNameMethods
import net.noresttherein.sugar.reflect.Specialized.MultiValue
import net.noresttherein.sugar.util.SerializableSingleton




private sealed abstract class AbstractArrayLike2IteratorFactory[-A[X] <: ArrayLike[X]]
                              (name :String, self: => Slice2DFactory[A, Iterator])
	extends SerializableSingleton[Slice2DFactory[A @uncheckedVariance, Iterator]](name, self)
	   with MatrixSliceFactory[A, Iterator]
{
	protected override def lengthOf[E](source :A[E]) :Int = source.asInstanceOf[Array[E]].length
	protected override def get[E](source :A[E], index :Int) :E = source.asInstanceOf[Array[E]](index)
}




@SerialVersionUID(Ver)
private sealed class MatrixIteratorFactory[-A[X] <: ArrayLike[X]] private[arrays]
                                          (name :String, self: => Slice2DFactory[A, Iterator])
	extends AbstractArrayLike2IteratorFactory[A](name, self)
	   with MatrixSliceFactory[A, MatrixIterator]
{
	protected final override def make[E](source :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int)
			:MatrixIterator[E] =
		((source :ArrayLike[_]) match {
			case a :Array[Array[AnyRef]]       => new MatrixIterator(a, from2, from1, size)
			case a :Array[Array[Int]]          => new MatrixIterator(a, from2, from1, size)
			case a :Array[Array[Long]]         => new MatrixIterator(a, from2, from1, size)
			case a :Array[Array[Double]]       => new MatrixIterator(a, from2, from1, size)
			case a :Array[Array[Char]]         => new MatrixIterator(a, from2, from1, size)
			case a :Array[Array[Byte]]         => new MatrixIterator(a, from2, from1, size)
			case a :Array[Array[Float]]        => new MatrixIterator(a, from2, from1, size)
			case a :Array[Array[Short]]        => new MatrixIterator(a, from2, from1, size)
			case a :Array[Array[Boolean]]      => new MatrixIterator(a, from2, from1, size)
			case null                          => null_!("Null array passed to MatrixIterator")
			//Handle RefArray[ArrayLike[E]]
			case a :Array[Array[E]] @unchecked => new MatrixIterator(a, from2, from1, size)
//			case a :Array[RefArrayLike[E]] @unchecked => new RefMatrixIterator(a, from2, from1, size)
		}).castParam[E]

	protected final override val Empty :MatrixIterator[Nothing] = {
		//MatrixIterator assumes array(idx2) exists.
		val array = new Array[Array[Nothing]](1)
		array(0)  = new Array[Nothing](0)
		new MatrixIterator[Nothing](array, 0, 0, 0)
	}
}


@SerialVersionUID(Ver)
private sealed class RefMatrixIteratorFactory[-A[X] <: RefArrayLike[X]] private[arrays]
                                             (name :String, self: => Slice2DFactory[A, Iterator])
	extends AbstractArrayLike2IteratorFactory[A](name, self)
	   with MatrixSliceFactory[A, MatrixIterator]
{
	protected final override def make[E](source :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int)
			:MatrixIterator[E] =
		(source :ArrayLike[_]) match {
			case _ :Array[AnyRef] =>
				new RefMatrixIterator(source.castParam[Array[AnyRef]], from2, from1, size).asInstanceOf[MatrixIterator[E]]
			case null =>
				null_!("Cannot create a RefArray2Iterator for a null array.")
			case _ =>
				illegal_!("Cannot create a RefArray2Iterator for non Array[RefArray[T]]: " + errorString(source) + ".")
		}
	protected final override val Empty :MatrixIterator[Nothing] = {
		val array = new Array[Array[AnyRef]](1)
		array(0)  = new Array[AnyRef](0)
		new RefMatrixIterator(array, 0, 0, 0).asInstanceOf[MatrixIterator[Nothing]]
	}
}




@SerialVersionUID(Ver)
private final class CyclicMatrixIteratorFactory[-A[X] <: ArrayLike[X]] private[arrays]
                    (name :String, self: => Slice2DFactory[A, Iterator])
	extends MatrixIteratorFactory[A](name, self)
	   with CyclicMatrixSliceFactory[A, MatrixIterator]


@SerialVersionUID(Ver)
private final class CyclicRefMatrixIteratorFactory[-A[X] <: RefArrayLike[X]] private[arrays]
                    (name :String, self: => Slice2DFactory[A, Iterator])
	extends RefMatrixIteratorFactory[A](name, self)
	   with CyclicMatrixSliceFactory[A, MatrixIterator]




@SerialVersionUID(Ver)
private sealed class ReverseMatrixIteratorFactory[-A[X] <: ArrayLike[X]] private[arrays]
                     (name :String, self: => Slice2DFactory[A, Iterator])
	extends AbstractArrayLike2IteratorFactory[A](name, self)
	   with ReverseMatrixSliceFactory[A, ReverseMatrixIterator]
{
	protected final override def make[E](source :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int)
			:ReverseMatrixIterator[E] =
	{   //ReverseMatrixIterator expects to get the first element to the constructor.
		((source :ArrayLike[_]) match {
			case a :Array[Array[AnyRef]]       => new ReverseMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Int]]          => new ReverseMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Long]]         => new ReverseMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Double]]       => new ReverseMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Char]]         => new ReverseMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Byte]]         => new ReverseMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Float]]        => new ReverseMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Short]]        => new ReverseMatrixIterator(a, from2, from1, size)
			case a :Array[Array[Boolean]]      => new ReverseMatrixIterator(a, from2, from1, size)
			case null                          => null_!("Null array passed to " + this)
			case a :Array[Array[E]] @unchecked => new ReverseMatrixIterator(a, from2, from1, size)
		}).castParam[E]
	}

	protected final override val Empty :ReverseMatrixIterator[Nothing] = {
		//ReverseMatrixIterator assumes array(idx2) exists and is not empty.
		val array = new Array[Array[Nothing]](1)
		array(0)  = new Array[Nothing](0)
		new ReverseMatrixIterator(array, 0, 0, 0)
	}
}


@SerialVersionUID(Ver)
private sealed class ReverseRefMatrixIteratorFactory[-A[X] <: RefArrayLike[X]] private[arrays]
                     (name :String, self: => Slice2DFactory[A, Iterator])
	extends AbstractArrayLike2IteratorFactory[A](name, self)
	   with ReverseMatrixSliceFactory[A, ReverseMatrixIterator]
{
	protected override def make[E](source :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int)
			:ReverseMatrixIterator[E] =
		(source :ArrayLike[_]) match {
			case _ :Array[AnyRef] =>
				new ReverseRefMatrixIterator(source.castParam[Array[AnyRef]], from2, from1, size)
					.asInstanceOf[ReverseMatrixIterator[E]]
			case null =>
				null_!("Cannot create a RefArray2Iterator for a null array.")
			case _ =>
				illegal_!("Cannot create a RefArray2Iterator for non Array[RefArray[T]]: " + errorString(source) + ".")
		}
	protected override val Empty :ReverseMatrixIterator[Nothing] = {
		val array = new Array[Array[AnyRef]](1)
		array(0)  = new Array[AnyRef](0)
		new ReverseRefMatrixIterator(array, 0, 0, 0).asInstanceOf[ReverseMatrixIterator[Nothing]]
	}
}





@SerialVersionUID(Ver)
private final class ReverseCyclicMatrixIteratorFactory[-A[X] <: ArrayLike[X]] private[arrays]
                    (name :String, self: => Slice2DFactory[A, Iterator])
	extends ReverseMatrixIteratorFactory[A](name, self)
	   with ReverseCyclicMatrixSliceFactory[A, ReverseMatrixIterator]


@SerialVersionUID(Ver)
private final class ReverseCyclicRefMatrixIteratorFactory[-A[X] <: RefArrayLike[X]] private[arrays]
                    (name :String, self: => Slice2DFactory[A, Iterator])
	extends ReverseRefMatrixIteratorFactory[A](name, self)
	   with ReverseCyclicMatrixSliceFactory[A, ReverseMatrixIterator]






private[arrays] sealed trait AbstractMatrixIterator[+E]
	extends MatrixIteratorState with CountdownIterator[E] with IndexedIteratorEquals[E]
{
	protected override def source :ArrayLike[ArrayLike[E]]
	protected final override def underlyingSize :Int = source.length * Dim1
	protected final override def remaining :Int = countdown
	protected final override def remaining_=(value :Int) :Unit = countdown = value
	protected final override def index :Int = idx2 * Dim1 + idx1
	protected final override def index_=(value :Int) :Unit = {
		val Dim2 = source.length
		val i = if (value >= 0) value else value + Dim2 * Dim1
		val old2 = idx2
		idx2 = i / Dim1
		idx1 = i - idx2 * Dim1
		if (idx2 >= Dim2)
			idx2 -= Dim2
		if (idx2 != old2)
			setCurr()
	}
	final override def hasNext :Boolean = countdown > 0

	final override def safeCopyToArray[U >: E](xs :Array[U], start :Int, len :Int) :Int =
		copyToArray(xs, start, len)
}




/** An iterator over a two-dimensional array, wrapping at array end if its offset plus size exceeds length of the former.
  * Outer array may be of any size; all inner arrays must be of the same, non-zero, size.
  * @param array  an array, whose indices `[from2, until2]` (upper index may be exclusive, if `until1 == 0`)
  *               all contain arrays of the same length.
  * @param index2 the index of the current inner array.
  * @param index1 the index of the `head` element in `array(idx2)`.
  */
private[sugar] sealed class MatrixIterator[@specialized(MultiValue) +E]
                                          (array :ArrayLike[Array[E]], index2 :Int, index1 :Int, initialSize :Int)
	extends MatrixIteratorState(array(index2).length, index2, index1, initialSize)
	   with ValIterator.Buffered[E] with AbstractMatrixIterator[E]
{
	private[this] var curr = array(idx2)
	protected override def setCurr() :MatrixIterator[E] = { curr = array(idx2); this }
	protected final override def source :ArrayLike[ArrayLike[E]] = array

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

//	override def hasNext :Boolean = countdown > 0
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

	override def clone :MatrixIterator[E] =
		new MatrixIterator[E](source.asInstanceOf[ArrayLike[Array[E]]], idx2, idx1, countdown)

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[MatrixIterator[_]]

	override def toString :String =
		array.className + "|" + array.length + "*" + Dim1 + "|.iterator|" + remaining + "|@(" + idx2 + ", " + idx1 + ")"
}


private[sugar] final class RefMatrixIterator[+E <: AnyRef]
                     (array :ArrayLike[Array[E]], index2 :Int, index1 :Int, initialSize :Int)
	extends MatrixIterator[E](array, index2, index1, initialSize)
{
	private[this] var curr :Array[E] = array(idx2)
	protected override def setCurr() :MatrixIterator[E] = {
		curr = source(idx2).asInstanceOf[Array[E]]
		this
	}
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
	override def clone :MatrixIterator[E] =
		new RefMatrixIterator[E](source.asInstanceOf[ArrayLike[Array[E]]], idx2, idx1, countdown)
}




/** A reverse iterator over a two-dimensional array, wrapping back at its beginning if its offset plus size
  *  exceeds length of the former. Outer array may be of any size; all inner arrays must be of the same, non zero, size.
  * @param array  an array, whose indices `[from2, until2]` (upper index may be exclusive, if `until1 == 0`)
  *               all contain arrays of the same length.
  * @param index2 the index of the current inner array.
  * @param index1 the index of the `head` element in `array(idx2)`.
  */
private[sugar] sealed class ReverseMatrixIterator[@specialized(MultiValue) +E]
                            (array :ArrayLike[Array[E]], index2 :Int, index1 :Int, initialSize :Int)
	extends MatrixIteratorState(array(index2).length, index2, index1, initialSize)
	   with ValIterator.Buffered[E] with ReverseCountdownIterator[E] with AbstractMatrixIterator[E]
{
	private[this] var curr = array(idx2)
	protected override def setCurr() :ReverseMatrixIterator[E] = { curr = array(idx2); this }
	protected final override def source :ArrayLike[ArrayLike[E]] = array

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

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ReverseMatrixIterator[_]]
	override def clone :ReverseMatrixIterator[E] = new ReverseMatrixIterator(array, idx2, idx1, countdown)
	override def toString :String =
		array.className + "|" + array.length + "*" + Dim1 +
			"|.reverseIterator@(" + idx2 + ", " + idx1 + ")|" + countdown + "|"
}


private[sugar] final class ReverseRefMatrixIterator[+E <: AnyRef]
                           (array :ArrayLike[Array[E]], index2 :Int, index1 :Int, initialSize :Int)
	extends ReverseMatrixIterator[E](array, index2, index1, initialSize)
{
	private[this] var curr :Array[E] = array(idx2)
	protected override def setCurr() :ReverseMatrixIterator[E] = {
		curr = source(idx2).asInstanceOf[Array[E]]
		this
	}
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
	override def clone :ReverseMatrixIterator[E] =
		new ReverseRefMatrixIterator(source.asInstanceOf[ArrayLike[Array[E]]], idx2, idx1, countdown)
}
