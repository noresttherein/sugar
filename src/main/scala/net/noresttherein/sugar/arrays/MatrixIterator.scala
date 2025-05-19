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
private final class MatrixIteratorFactory[-A[X] <: ArrayLike[X]] private[arrays]
                                         (name :String, self: => Slice2DFactory[A, Iterator])
	extends AbstractArrayLike2IteratorFactory[A](name, self)
	   with MatrixSliceFactory[A, MatrixIterator]
{
	protected override def make[E](source :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int)
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

	protected override val Empty :MatrixIterator[Nothing] = {
		//MatrixIterator assumes array(idx2) exists.
		val array = new Array[Array[Nothing]](1)
		array(0)  = new Array[Nothing](0)
		new MatrixIterator[Nothing](array, 0, 0, 0)
	}
}


@SerialVersionUID(Ver)
private final class RefArrayLikeMatrixIteratorFactory[-A[X] <: RefArrayLike[X]] private[arrays]
                                                     (name :String, self: => Slice2DFactory[A, Iterator])
	extends AbstractArrayLike2IteratorFactory[A](name, self)
	   with MatrixSliceFactory[A, MatrixIterator]
{
	protected override def make[E](source :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int)
			:MatrixIterator[E] =
		(source :ArrayLike[_]) match {
			case _ :Array[AnyRef] => //todo: manually specialize for RefArray
				new MatrixIterator(source.castParam[Array[E]], from2, from1, size)
//				new RefMatrixIterator(source.castParam[RefArray[E]], from2, from1, size)
			case null =>
				null_!("Cannot create a RefArray2Iterator for a null array.")
			case _ =>
				illegal_!("Cannot create a RefArray2Iterator for non Array[RefArray[T]]: " + errorString(source) + ".")
		}
	protected override val Empty :MatrixIterator[Nothing] = {
		val array = new Array[Array[Any]](1)
		array(0)  = new Array[Any](0)
		new MatrixIterator(array, 0, 0, 0).asInstanceOf[MatrixIterator[Nothing]]
	}
}


/** An iterator over a two-dimensional array.
  * Outer array may be of any size; all inner arrays must be of the same, non-zero, size.
  * Requires `array(idx2)` to exist even if `countdown == 0`.
  * @param array  an array, whose indices `[from2, until2]` (upper index may be exclusive, if `until1 == 0`)
  *               all contain arrays of the same length.
  * @param idx2   the index of the current inner array.
  * @param idx1   the index of the `head` element in `array(idx2)`.
  */
private[sugar] class MatrixIterator[@specialized(MultiValue) +E]
                                   (array :ArrayLike[Array[E]], private[this] var idx2 :Int, private[this] var idx1 :Int,
                                    private[this] var countdown :Int)
	extends AbstractSugaredIterator[E] with ValIterator.Buffered[E]
	   with CountdownIterator[E] with IndexedIteratorEquals[E]
{
	private[this] final val Dim1 = array(idx2).length
	private[this] var curr :Array[E] = array(idx2)
	//Return a MatrixIterator so that method call is specialized and sets the specialized curr field.
	protected def setCurr() :MatrixIterator[E] = { curr = array(idx2); this }

	protected final override def source :ArrayLike[ArrayLike[E]] = array
	protected final override def underlyingSize :Int = array.length * Dim1
	protected final override def remaining :Int = countdown
	protected final override def remaining_=(value :Int) :Unit = countdown = value
	protected final override def remaining_--() :Unit = countdown -= 1
	protected final def index2 :Int = idx2
	protected final def index2_=(value :Int) :Unit = idx2 = value
	protected final def index1 :Int = idx1
	protected final def index1_=(value :Int) :Unit = idx1 = value
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
		countdown -= 1
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
		if (countdown <= 0)
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
				idx1 += n
				if (idx1 == Dim1) {
					idx1 = 0
					idx2 += 1
				}
			}
			countdown -= copied
			if (idx2 == array.length) //Implies countdown == 0
				idx2 = array.length - 1
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


/** A variant of `MatrixIterator` manually specialized for reference arrays. */
//This will require implementing in Java in order to access fields directly in order to be efficient.
/*
private[sugar] class RefMatrixIterator[+E]
                     (array :ArrayLike[RefArrayLike[E]], idx2 :Int, idx1 :Int, size :Int)
	extends MatrixIterator[E](array.castParam[Array[E]], idx2, idx1, size)
{
	private[this] var curr = array(idx2).asInstanceOf[Array[AnyRef]]
	protected override def setCurr() :MatrixIterator[E] = {
		val array = source
		curr = array(index2).asInstanceOf[Array[AnyRef]]
		this
	}

	override def next() :E = {
		//We hope for the getters/setters to be inlined.
		val rem = remaining
		if (rem <= 0)
			noSuch_!("Iterator.empty.next()")
		val idx2 = index2
		val idx1 = index1
		val res = curr(idx1)
		index2 = idx2 + 1
		remaining = rem - 1
		if (idx1 >= curr.length & rem > 1) {
			index1 = 0
			index2 = idx2 + 1
			curr = source(idx2).asInstanceOf[Array[AnyRef]]
		}
		res.asInstanceOf[E]
	}
	override def head :E = {
		if (remaining <= 0)
			noSuch_!("Iterator.empty.head")
		curr(index1)
	}
}
*/




@SerialVersionUID(Ver)
private final class ReverseMatrixIteratorFactory[-A[X] <: ArrayLike[X]] private[arrays]
                    (name :String, self: => Slice2DFactory[A, Iterator])
	extends AbstractArrayLike2IteratorFactory[A](name, self)
	   with ReverseMatrixSliceFactory[A, ReverseMatrixIterator]
{
	protected override def make[E](source :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int)
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

	protected override val Empty :ReverseMatrixIterator[Nothing] = {
		//ReverseMatrixIterator assumes array(idx2) exists and is not empty.
		val array = new Array[Array[Nothing]](1)
		array(0)  = new Array[Nothing](0)
		new ReverseMatrixIterator(array, 0, 0, 0)
	}
}




/** An iterator going over a two-dimensional array in reverse.
  * Outer array may be of any size; all inner arrays must be of the same, non-zero, size.
  * @param array    an array, whose indices `[from2, until2]` (upper index may be exclusive, if `until1 == 0`)
  *                 all contain arrays of the same length.
  * @param idx2     the index of the current inner array.
  * @param idx1     the index of the `head` element in `array(idx2)`.
  */
private[sugar] class ReverseMatrixIterator[@specialized(MultiValue) +E]
                                          (array :ArrayLike[Array[E]],
                                           private[this] var idx2 :Int, private[this] var idx1 :Int,
                                           private[this] var countdown :Int)
	extends AbstractSugaredIterator[E] with ValIterator.Buffered[E]
	   with ReverseCountdownIterator[E] with IndexedIteratorEquals[E]
{
	private[this] final val Dim1 = array(idx2).length
	private[this] var curr = array(idx2)
	private def setCurr() :ReverseMatrixIterator[E] = { curr = array(idx2); this }

	protected final override def source :Array[_] = array.asArray
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




@SerialVersionUID(Ver)
private final class CyclicMatrixIteratorFactory[-A[X] <: ArrayLike[X]] private[arrays]
                    (name :String, self: => Slice2DFactory[A, Iterator])
	extends AbstractArrayLike2IteratorFactory[A](name, self)
	   with CyclicMatrixSliceFactory[A, CyclicMatrixIterator]
{
	protected override def make[E](source :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int)
			:CyclicMatrixIterator[E] =
		((source :ArrayLike[_]) match {
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
                                         (array :ArrayLike[Array[E]],
                                          private[this] var idx2 :Int, private[this] var idx1 :Int,
                                          private[this] var countdown :Int)
	extends AbstractSugaredIterator[E] with ValIterator.Buffered[E]
	   with CountdownIterator[E] with IndexedIteratorEquals[E]
{
	private[this] final val Dim1 = array(idx2).length
	private[this] var curr = array(idx2)
	private def setCurr() :CyclicMatrixIterator[E] = { curr = array(idx2); this }

	protected final override def source :Array[_] = array.asArray
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




@SerialVersionUID(Ver)
private final class ReverseCyclicMatrixIteratorFactory[-A[X] <: ArrayLike[X]] private[arrays]
                    (name :String, self: => Slice2DFactory[A, Iterator])
	extends AbstractArrayLike2IteratorFactory[A](name, self)
	  with ReverseCyclicMatrixSliceFactory[A, ReverseCyclicMatrixIterator]
{
	protected override def make[E](source :A[A[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int)
			:ReverseCyclicMatrixIterator[E] =
		((source :ArrayLike[_]) match {
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
                     (array :ArrayLike[Array[E]], private[this] var idx2 :Int, private[this] var idx1 :Int,
                      private[this] var countdown :Int)
	extends AbstractIterator[E] with ValIterator.Buffered[E]
	   with ReverseCountdownIterator[E] with IndexedIteratorEquals[E]
{
	private[this] final val Dim1 = array(idx2).length
	private[this] var curr = array(idx2)
	private def setCurr() :ReverseCyclicMatrixIterator[E] = { curr = array(idx2); this }

	protected final override def source :Array[_] = array.asArray
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


