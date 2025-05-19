package net.noresttherein.sugar.arrays

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.unspecialized

import net.noresttherein.sugar.casting.castTypeParamMethods
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.collections.{AbstractCyclicCuboidSliceFactory, AbstractSugaredIterator, CountdownIterator, CuboidSliceFactory, CyclicCuboidSliceFactory, IndexedIteratorEquals, ReverseCountdownIterator, ReverseCuboidSliceFactory, ReverseCyclicCuboidSliceFactory, Slice3DFactory, ValIterator}
import net.noresttherein.sugar.exceptions.{illegal_!, noSuch_!, null_!, outOfBounds_!}
import net.noresttherein.sugar.extensions.classNameMethods
import net.noresttherein.sugar.reflect.Specialized.MultiValue
import net.noresttherein.sugar.util.SerializableSingleton




private sealed abstract class AbstractArrayLike3IteratorFactory[-A[X] <: ArrayLike[X]]
                              (name :String, self: => Slice3DFactory[A, Iterator])
	extends SerializableSingleton[Slice3DFactory[A @uncheckedVariance, Iterator]](name, self)
	   with CuboidSliceFactory[A, Iterator]
{
	protected override def lengthOf[E](source :A[E]) :Int = source.asInstanceOf[Array[E]].length
	protected override def get[E](source :A[E], index :Int) :E = source.asInstanceOf[Array[E]](index)
}




@SerialVersionUID(Ver)
private final class CuboidIteratorFactory[-A[X] <: ArrayLike[X]] private[arrays]
                                         (name :String, self: => Slice3DFactory[A, Iterator])
	extends AbstractArrayLike3IteratorFactory[A](name, self)
	   with CuboidSliceFactory[A, CuboidIterator]
{
	protected override def make[E](source :A[A[A[E]]] @uncheckedVariance, from3 :Int, from2 :Int, from1 :Int, size :Int)
			:CuboidIterator[E] =
		((source :ArrayLike[_]) match {
			case a :Array[Array[Array[AnyRef]]]  => new CuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Int]]]     => new CuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Long]]]    => new CuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Double]]]  => new CuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Char]]]    => new CuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Byte]]]    => new CuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Float]]]   => new CuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Short]]]   => new CuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Boolean]]] => new CuboidIterator(a, from3, from2, from1, size)
			case null                     => null_!("Null array passed to CyclicCuboidIterator")
			//Handle RefArray
			case a :Array[ArrayLike[Array[E]]] @unchecked => new CuboidIterator(a, from3, from2, from1, size)
		}).castParam[E]

	protected override val Empty :CuboidIterator[Nothing] = {
		//CyclicCuboidIterator assumes array(idx3)(idx2) exists.
		val array   = new Array[Array[Array[Nothing]]](1)
		array(0)    = new Array[Array[Nothing]](1)
		array(0)(0) = new Array[Nothing](0)
		new CuboidIterator[Nothing](array, 0, 0, 0, 0)
	}
}


@SerialVersionUID(Ver)
private final class RefCuboidIteratorFactory[-A[X] <: RefArrayLike[X]] private[arrays]
                    (name :String, self: => Slice3DFactory[A, Iterator])
	extends AbstractArrayLike3IteratorFactory[A](name, self)
	   with CuboidSliceFactory[A, CuboidIterator]
{
	protected override def make[E](array :A[A[A[E]]] @uncheckedVariance, from3 :Int, from2 :Int, from1 :Int, size :Int)
			:CuboidIterator[E] =
		(array :ArrayLike[_]) match {
			case refs :Array[AnyRef] =>
				new RefCuboidIterator(refs.asInstanceOf[A[A[Array[AnyRef]]]], from3, from2, from1, size)
					.asInstanceOf[CuboidIterator[E]]
			case null =>
				null_!("Cannot create a RefCuboidIterator for a null array.")
			case _ =>
				illegal_!("Cannot create a RefCuboidIterator for non RefArray: " + errorString(array) + ".")
		}
	protected override val Empty :CuboidIterator[Nothing] = {
		val array   = RefArray[RefArray[Array[AnyRef]]](RefArray[Array[AnyRef]](Array.empty[AnyRef]))
		new RefCuboidIterator(array, 0, 0, 0, 0).asInstanceOf[CuboidIterator[Nothing]]
	}
}




@SerialVersionUID(Ver)
private final class CyclicCuboidIteratorFactory[-A[X] <: ArrayLike[X]](name :String, self: => Slice3DFactory[A, Iterator])
	extends AbstractArrayLike3IteratorFactory[A](name, self)
	   with CyclicCuboidSliceFactory[A, CuboidIterator]
{
	protected override def make[E](source :A[A[A[E]]] @uncheckedVariance, from3 :Int, from2 :Int, from1 :Int, size :Int)
			:CuboidIterator[E] =
		((source :ArrayLike[_]) match {
			case a :Array[Array[Array[AnyRef]]]  => new CuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Int]]]     => new CuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Long]]]    => new CuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Double]]]  => new CuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Char]]]    => new CuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Byte]]]    => new CuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Float]]]   => new CuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Short]]]   => new CuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Boolean]]] => new CuboidIterator(a, from3, from2, from1, size)
			case null                     => null_!("Null array passed to CyclicCuboidIterator")
			//Handle RefArray
			case a :Array[ArrayLike[Array[E]]] @unchecked => new CuboidIterator(a, from3, from2, from1, size)
		}).castParam[E]

	protected override val Empty :CuboidIterator[Nothing] = {
		//CuboidIterator assumes array(idx3)(idx2) exists.
		val array   = new Array[Array[Array[Nothing]]](1)
		array(0)    = new Array[Array[Nothing]](1)
		array(0)(0) = new Array[Nothing](0)
		new CuboidIterator[Nothing](array, 0, 0, 0, 0)
	}
}


@SerialVersionUID(Ver)
private final class CyclicRefCuboidIteratorFactory[-A[X] <: RefArrayLike[X]] private[arrays]
                    (name :String, self: => Slice3DFactory[A, Iterator])
	extends AbstractArrayLike3IteratorFactory[A](name, self)
	   with CyclicCuboidSliceFactory[A, CuboidIterator]
{
	protected override def make[E](array :A[A[A[E]]] @uncheckedVariance, from3 :Int, from2 :Int, from1 :Int, size :Int)
			:CuboidIterator[E] =
		(array :ArrayLike[_]) match {
			case refs :Array[AnyRef] =>
				new RefCuboidIterator(refs.asInstanceOf[A[A[Array[AnyRef]]]], from3, from2, from1, size)
					.asInstanceOf[CuboidIterator[E]]
			case null =>
				null_!("Cannot create a RefCuboidIterator for a null array.")
			case _ =>
				illegal_!("Cannot create a RefCuboidIterator for non RefArray: " + errorString(array) + ".")
		}
	protected override val Empty :CuboidIterator[Nothing] = {
		val array   = RefArray[RefArray[Array[AnyRef]]](RefArray[Array[AnyRef]](Array.empty[AnyRef]))
		new RefCuboidIterator(array, 0, 0, 0, 0).asInstanceOf[CuboidIterator[Nothing]]
	}
}




@SerialVersionUID(Ver)
private final class ReverseCuboidIteratorFactory[-A[X] <: ArrayLike[X]] private[arrays]
                                                (name :String, self: => Slice3DFactory[A, Iterator])
	extends AbstractArrayLike3IteratorFactory[A](name, self)
	   with ReverseCuboidSliceFactory[A, ReverseCuboidIterator]
{
	protected override def make[E](source :A[A[A[E]]] @uncheckedVariance, from3 :Int, from2 :Int, from1 :Int, size :Int)
			:ReverseCuboidIterator[E] =
		((source :ArrayLike[_]) match {
			case a :Array[Array[Array[AnyRef]]]  => new ReverseCuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Int]]]     => new ReverseCuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Long]]]    => new ReverseCuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Double]]]  => new ReverseCuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Char]]]    => new ReverseCuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Byte]]]    => new ReverseCuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Float]]]   => new ReverseCuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Short]]]   => new ReverseCuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Boolean]]] => new ReverseCuboidIterator(a, from3, from2, from1, size)
			case null                     => null_!("Null array passed to ReverseCuboidIterator")
			//Handle RefArray
			case a :Array[ArrayLike[Array[E]]] @unchecked => new ReverseCuboidIterator(a, from3, from2, from1, size)
		}).castParam[E]

	protected override val Empty :ReverseCuboidIterator[Nothing] = {
		//CuboidIterator assumes array(idx3)(idx2) exists.
		val array   = new Array[Array[Array[Nothing]]](1)
		array(0)    = new Array[Array[Nothing]](1)
		array(0)(0) = new Array[Nothing](0)
		new ReverseCuboidIterator[Nothing](array, 0, 0, 0, 0)
	}
}


@SerialVersionUID(Ver)
private final class ReverseRefCuboidIteratorFactory[-A[X] <: RefArrayLike[X]] private[arrays]
                    (name :String, self: => Slice3DFactory[A, Iterator])
	extends AbstractArrayLike3IteratorFactory[A](name, self)
	   with ReverseCuboidSliceFactory[A, ReverseCuboidIterator]
{
	protected override def make[E](array :A[A[A[E]]] @uncheckedVariance, from3 :Int, from2 :Int, from1 :Int, size :Int)
			:ReverseCuboidIterator[E] =
		(array :ArrayLike[_]) match {
			case refs :Array[AnyRef] =>
				new ReverseRefCuboidIterator(refs.asInstanceOf[A[A[Array[AnyRef]]]], from3, from2, from1, size)
					.asInstanceOf[ReverseCuboidIterator[E]]
			case null =>
				null_!("Cannot create a RefCuboidIterator for a null array.")
			case _ =>
				illegal_!("Cannot create a RefCuboidIterator for non RefArray: " + errorString(array) + ".")
		}
	protected override val Empty :ReverseCuboidIterator[Nothing] = {
		val array   = RefArray[RefArray[Array[AnyRef]]](RefArray[Array[AnyRef]](Array.empty[AnyRef]))
		new ReverseRefCuboidIterator(array, 0, 0, 0, 0).asInstanceOf[ReverseCuboidIterator[Nothing]]
	}
}




@SerialVersionUID(Ver)
private final class ReverseCyclicCuboidIteratorFactory[-A[X] <: ArrayLike[X]]
                                                      (name :String, self: => Slice3DFactory[A, Iterator])
	extends AbstractArrayLike3IteratorFactory[A](name, self)
	   with ReverseCyclicCuboidSliceFactory[A, ReverseCuboidIterator]
{
	protected override def make[E](source :A[A[A[E]]] @uncheckedVariance, from3 :Int, from2 :Int, from1 :Int, size :Int)
			:ReverseCuboidIterator[E] =
		((source :ArrayLike[_]) match {
			case a :Array[Array[Array[AnyRef]]]  => new ReverseCuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Int]]]     => new ReverseCuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Long]]]    => new ReverseCuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Double]]]  => new ReverseCuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Char]]]    => new ReverseCuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Byte]]]    => new ReverseCuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Float]]]   => new ReverseCuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Short]]]   => new ReverseCuboidIterator(a, from3, from2, from1, size)
			case a :Array[Array[Array[Boolean]]] => new ReverseCuboidIterator(a, from3, from2, from1, size)
			case null                     => null_!("Null array passed to ReverseCuboidIterator")
			//Handle RefArray
			case a :Array[ArrayLike[Array[E]]] @unchecked => new ReverseCuboidIterator(a, from3, from2, from1, size)
		}).castParam[E]

	protected override val Empty :ReverseCuboidIterator[Nothing] = {
		//CuboidIterator assumes array(idx3)(idx2) exists.
		val array   = new Array[Array[Array[Nothing]]](1)
		array(0)    = new Array[Array[Nothing]](1)
		array(0)(0) = new Array[Nothing](0)
		new ReverseCuboidIterator[Nothing](array, 0, 0, 0, 0)
	}
}


@SerialVersionUID(Ver)
private final class ReverseCyclicRefCuboidIteratorFactory[-A[X] <: RefArrayLike[X]] private[arrays]
                    (name :String, self: => Slice3DFactory[A, Iterator])
	extends AbstractArrayLike3IteratorFactory[A](name, self)
	   with ReverseCyclicCuboidSliceFactory[A, ReverseCuboidIterator]
{
	protected override def make[E](array :A[A[A[E]]] @uncheckedVariance, from3 :Int, from2 :Int, from1 :Int, size :Int)
			:ReverseCuboidIterator[E] =
		(array :ArrayLike[_]) match {
			case refs :Array[AnyRef] =>
				new ReverseRefCuboidIterator(refs.asInstanceOf[A[A[Array[AnyRef]]]], from3, from2, from1, size)
					.asInstanceOf[ReverseCuboidIterator[E]]
			case null =>
				null_!("Cannot create a RefCuboidIterator for a null array.")
			case _ =>
				illegal_!("Cannot create a RefCuboidIterator for non RefArray: " + errorString(array) + ".")
		}
	protected override val Empty :ReverseCuboidIterator[Nothing] = {
		val array   = RefArray[RefArray[Array[AnyRef]]](RefArray[Array[AnyRef]](Array.empty[AnyRef]))
		new ReverseRefCuboidIterator(array, 0, 0, 0, 0).asInstanceOf[ReverseCuboidIterator[Nothing]]
	}
}






private[arrays] sealed trait AbstractCuboidIterator[+E]
	extends CuboidIteratorState//(array3(index3).length, array3(index3)(index2).length, index3, index2, index1, initialSize)
	   with CountdownIterator[E] with IndexedIteratorEquals[E]
{
	protected override def source :ArrayLike[ArrayLike[ArrayLike[E]]]
	protected final override def underlyingSize :Int = source.length * Dim2 * Dim1
	protected final override def remaining :Int = countdown
	protected final override def remaining_=(value :Int) :Unit = countdown = value
	protected final override def remaining_--() :Unit = countdown -= 1
	protected final override def index :Int = (idx3 * Dim2 + idx2) * Dim1 + idx1
	protected final override def index_=(value :Int) :Unit = {
		val Dim3 = source.length
		val Dim21 = Dim2 * Dim1
		val i = if (value >= 0) value else value + Dim3 * Dim21
		val oldIdx3 = idx3
		idx3 = i / Dim21
		val rem2 = i - idx3 * Dim21
		if (idx3 >= Dim3)
			idx3 -= Dim3
		val oldIdx2 = idx2
		idx2 = rem2 / Dim1
		idx1 = rem2 - idx2 * Dim1
		if (oldIdx3 != idx3 | oldIdx2 != idx2)
			setCurr() //this is a method call, because curr is a specialized field, and this method is not.
	}

	override def hasNext :Boolean = countdown > 0

	final override def safeCopyToArray[U >: E](xs :Array[U], start :Int, len :Int) :Int =
		copyToArray(xs, start, len)

	override def toString :String =
		className + "|" + knownSize + "|(" + source.className +
			"|" + source.length + "*" + Dim2 + "*" + Dim1 + "|, " + idx3 + ", " + idx2 + ", " + idx1 + ")"
}




/** An iterator over a three-dimensional array, increasing the index of the higher dimension
  * when the end of a lower dimension array's is reached. If the end of `array3` is reached before returning
  * the requested number of elements, the iterator wraps back to the start of the array (returning `array3(0)(0)(0)`).
  * The outer array may be of any size greater than zero. All the inner two- and one-dimensional arrays which contain
  * the elements of the iterator must be non-null and of the same length; the two-dimensional arrays
  * must additionally be non-empty (so that `array3(index3)(index2)` exists). For the purpose of calculating indices,
  * all arrays (even the `null` ones) are assumed to be of the same length.
  * @param array3      an array with the elements.
  * @param index3      the index of the first element in the outer array.
  * @param index2      the index of the first element in the second array
  * @param index1      the index of the first element in the inner array.
  * @param initialSize the number of elements to return.
  */
private[sugar] sealed class CuboidIterator[@specialized(MultiValue) +E]
                                          (array3 :ArrayLike[ArrayLike[Array[E]]],
                                           index3 :Int, index2 :Int, index1 :Int, initialSize :Int)
	extends CuboidIteratorState(array3(index3).length, array3(index3)(index2).length, index3, index2, index1, initialSize)
	   with ValIterator.Buffered[E] with AbstractCuboidIterator[E]
{
	private[this] var curr :Array[E] = array3(idx3)(idx2)
	//Return a CuboidIterator so that method call is specialized and sets the specialized curr field.
	protected def setCurr() :CuboidIterator[E] = { curr = array3(idx3)(idx2); this }

	protected final override def source :ArrayLike[ArrayLike[ArrayLike[E]]] = array3

	protected final override def advance() :Int = {
		countdown -= 1
		val i1 = idx1
		val i2 = idx2
		val i3 = idx3
		idx1 = i1 + 1
		if (i1 == Dim1 - 1 & countdown > 0) {
			advance2()
			setCurr() //Don't set curr manually, because RefCuboidIterator uses its own field for the current array.
//			curr = array3(idx3)(idx2)
		}
		(i3 * Dim2 + i2) * Dim1 + i1
	}
	protected final def advance2() :Unit = {
		idx1  = 0
		idx2 += 1
		if (idx2 == Dim2) {
			idx2  = 0
			idx3 += 1
			if (idx3 == source.length)
				idx3 = 0
		}
	}

	final override def hasNext :Boolean = countdown > 0

	override def next() :E = {
		if (countdown <= 0)
			noSuch_!("Iterator.empty.next()")
		val res = curr(idx1)
		idx1 += 1
		countdown -= 1 //We could u++pdate it only in the if branch, but that would throw off knownSize
		if (idx1 >= Dim1 & countdown > 0) {
			advance2()
			curr = array3(idx3)(idx2)
		}
		res
	}

	override def head :E = {
		if (countdown <= 0)
			noSuch_!("Iterator.empty.head")
		curr(idx1)
	}

	@unspecialized override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
		if (len <= 0 | countdown <= 0 || start >= xs.length)
			0
		else if (start < 0)
			outOfBounds_!(toString + ".copyToArray(" + errorString(xs) + ", " + start + ", " + len + ")")
		else {
			val max = math.min(countdown, math.min(len, xs.length - start))
			var copied = 0
			while (copied < max) {
				val a2 = array3(idx3)
				while (idx2 < Dim2 & copied < max) {
					val a = a2(idx2)
					val n = math.min(max - copied, Dim1 - idx1)
					ArrayLike.copy(a, idx1, xs, start + copied, n)
					copied += n
					if (idx1 + n < Dim1) //Implies copied == max.
						idx1 += n
					else {
						idx1 = 0
						idx2 += 1
					}
				}
				if (idx2 == Dim2) {
					idx2 = 0
					idx3 += 1
					if (idx3 == array3.length)
						idx3 = 0
				}
			}
			countdown -= copied
			setCurr()
			copied
		}

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[CuboidIterator[_]]
	override def clone :CuboidIterator[E] = new CuboidIterator(array3, idx3, idx2, idx1, countdown)
	override def className = "CyclicCuboidIterator"
}


private[sugar] final class RefCuboidIterator[+E <: AnyRef]
                                            (array3 :ArrayLike[ArrayLike[Array[E]]],
                                             index3 :Int, index2 :Int, index1 :Int, initialSize :Int)
	extends CuboidIterator[E](array3, index3, index2, index1, initialSize)
{
	private[this] var curr :Array[E] = array3(idx3)(idx2)
	protected override def setCurr() :CuboidIterator[E] = {
		curr = source(idx3)(idx2).asInstanceOf[Array[E]]; this
	}

	override def head :E =
		if (countdown <= 0) noSuch_!(toString + ".head")
		else curr(idx1)

	override def next() :E = {
		if (countdown <= 0)
			noSuch_!(toString + ".next()")
		val res = curr(idx1)
		idx1 += 1
		countdown -= 1
		if (idx1 == Dim1 && countdown > 0) {
			advance2()
			curr = source(idx3)(idx2).asInstanceOf[Array[E]]
		}
		res
	}
	override def clone :CuboidIterator[E] =
		new RefCuboidIterator(source.asInstanceOf[ArrayLike2[Array[E]]], idx3, idx2, idx1, countdown)
}




private[sugar] sealed class ReverseCuboidIterator[@specialized(MultiValue) +E]
                            (array3 :ArrayLike[ArrayLike[Array[E]]],
                             index3 :Int, index2 :Int, index1 :Int, initialSize :Int)
	extends CuboidIteratorState(array3(index3).length, array3(index3)(index2).length, index3, index2, index1, initialSize)
	   with ValIterator.Buffered[E] with ReverseCountdownIterator[E] with AbstractCuboidIterator[E]
{
	private[this] var curr :Array[E] = array3(idx3)(idx2)
	//Return a ReverseCuboidIterator so that method call is specialized and sets the specialized curr field.
	protected override def setCurr() :ReverseCuboidIterator[E] = { curr = array3(idx3)(idx2); this }
	protected final override def source :ArrayLike[ArrayLike[ArrayLike[E]]] = array3

	protected override def advance() :Int = {
		countdown -= 1
		val i1 = idx1
		val i2 = idx2
		val i3 = idx3
		idx1 = i1 - 1
		if (i1 == -1 & countdown > 0) {
			advance2()
			setCurr() //idx3 and idx2 must be in range because countdown > 0
		}
		(i3 * Dim2 + i2) * Dim1 + i1
	}

	protected final def advance2() :Unit = {
		idx1 = Dim1 - 1
		idx2 = idx2 - 1
		if (idx2 == -1) {
			idx2 = Dim2 - 1
			idx3 = idx3 - 1
			if (idx3 == -1)
				idx3 = array3.length - 1
		}
	}

	override def next() :E = {
		if (countdown <= 0)
			noSuch_!("Iterator.empty.next()")
		val res = curr(idx1)
		idx1 -= 1
		countdown -= 1 //We could update it only in the if branch, but that would throw off knownSize
		if (idx1 == -1 & countdown > 0) {
			advance2()
			curr = array3(idx3)(idx2) //idx2 must be in range because countdown > 0
		}
		res
	}

	final override def hasNext :Boolean = countdown > 0

	override def head :E = {
		if (countdown <= 0)
			noSuch_!("Iterator.empty.head")
		curr(idx1)
	}

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ReverseCuboidIterator[_]]
	override def clone :ReverseCuboidIterator[E] = new ReverseCuboidIterator(array3, idx3, idx2, idx1, countdown)
	override def className :String = "ReverseCuboidIterator"
}


private[sugar] final class ReverseRefCuboidIterator[+E <: AnyRef]
                           (array3 :ArrayLike[ArrayLike[Array[E]]],
                            index3 :Int, index2 :Int, index1 :Int, initialSize :Int)
	extends ReverseCuboidIterator[E](array3, index3, index2, index1, initialSize)
{
	private[this] var curr = array3(idx3)(idx2)
	protected override def setCurr() :ReverseCuboidIterator[E] = {
		curr = source(idx3)(idx2).asInstanceOf[Array[E]]
		this
	}
	override def next() :E = {
		if (countdown <= 0)
			noSuch_!("Iterator.empty.next()")
		val res = curr(idx1)
		idx1 -= 1
		countdown -= 1
		if (idx1 == -1 & countdown > 0) {
			advance2()
			curr = array3(idx3)(idx2) //idx2 must be in range because countdown > 0
		}
		res
	}
	override def head :E = {
		if (countdown <= 0)
			noSuch_!("Iterator.empty.head")
		curr(idx1)
	}
	override def clone :ReverseCuboidIterator[E] =
		new ReverseRefCuboidIterator(source.asInstanceOf[ArrayLike2[Array[E]]], idx3, idx2, idx1, countdown)
}
