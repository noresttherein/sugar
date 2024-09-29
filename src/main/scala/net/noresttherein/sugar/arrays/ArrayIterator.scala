package net.noresttherein.sugar.arrays

import scala.annotation.unchecked.uncheckedVariance

import net.noresttherein.sugar.casting.castTypeParamMethods
import net.noresttherein.sugar.collections.{AbstractCyclicIterator, AbstractIndexedIterator, AbstractReverseCyclicIterator, AbstractReverseIndexedIterator, ArrayIterableOnceOps, ArrayLikeSliceWrapper, CyclicIteratorFactory, IArrayLikeSlice, IndexedIterator, IndexedIteratorFactory, Mutability, ReverseCyclicIteratorFactory, ReverseIndexedIteratorFactory, ValIterator}
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.exceptions.{illegal_!, noSuch_!, null_!, outOfBounds_!}
import net.noresttherein.sugar.reflect.Specialized.{Fun2Arg, MultiValue}
import net.noresttherein.sugar.slang.extensions.hashCodeMethods
import net.noresttherein.sugar.util.SingletonSerializationProxy






private[sugar] trait ArrayIteratorOps[@specialized(MultiValue) +T]
	extends ValIterator.Buffered[T] with ArrayIterableOnceOps[T, Iterator, Iterator[T]] with IndexedIterator[T]
{
	private[sugar] final override def startIndex  :Int = index
	protected final override def underlyingSize :Int = unsafeArray.length

	@inline private[this] def array :Array[T] = unsafeArray.asInstanceOf[Array[T]]

	//consider: not implementing them here, so JVM knows there is a single implementation if only ArrayIterator is used.
	override def head :T =
		if (hasNext) array(index)
		else noSuch_!("Index " + index + " exceeds the limit of " + limit + '.')

	override def next() :T = {
		val res = head
		advance()
		res
	}

	final override def foldLeft[@specialized(Fun2Arg) A](z :A)(op :(A, T) => A) :A = {
		var first    = index
		val `last++` = limit
		var res      = z
		val a        = unsafeArray.asInstanceOf[Array[T]]
		while (first < `last++`) {
			res = op(res, a(first))
			first += 1
		}
		index = first
		res
	}

	override def toSeq :Seq[T] = toIndexedSeq
	override def toIndexedSeq :IndexedSeq[T] =
		if (index >= limit) IndexedSeq.empty
		else if (isImmutable) IArrayLikeSlice.slice(array.asInstanceOf[IArrayLike[T]], index, limit)
		else IArrayLike.Wrapped(array.slice(index, limit).asInstanceOf[IArrayLike[T]])

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :ArrayIteratorOps[_] if other.canEqual(this) =>
			(unsafeArray eq other.unsafeArray) && index == other.index && limit == other.limit
		case _ => false
	}
	def canEqual(that :Any) :Boolean = that.isInstanceOf[ArrayIteratorOps[_]]
	override def hashCode :Int = ArrayLike.Slice(array, index, limit).hashCode
}






/** Base class for array iterator factories/companions. The iterators may or may not be `@specialized` but,
  * in the former case, their specialized class is picked dynamically based on the argument array,
  * rather than the information about element type at call site.
  * @tparam A   The kind of arrays this factory iterates over.
  * @tparam I   The type of the created iterator, `I[X] <: Iterator[X]`
  * @define Coll `ArrayIterator`
  * @define coll array iterator
  */
abstract class ArrayLikeIteratorFactory[-A[X] <: ArrayLike[X], +I[X] <: Iterator[X]] private[arrays]
               (name :String, mutability :Mutability, self: => IndexedIteratorFactory[A, Iterator])
	extends IndexedIteratorFactory[A, I] with ArrayLikeSliceWrapper[A, I]
{
	protected final override def lengthOf[E](array :A[E]) :Int = array.length
//	private[this] val empty = make(Array.emptyObjectArray.asInstanceOf[A[Nothing]])
	@inline final override def wrap[E](array :A[E]) :I[E] = apply(array)

	override def isImmutable :Boolean = mutability.isImmutable
	override def isMutable   :Boolean = mutability.isMutable

	override def toString :String = name
	private def writeReplace :Any = new SingletonSerializationProxy(self)
}


@SerialVersionUID(Ver)
private class ArrayIteratorFactory[-A[X] <: ArrayLike[X]]
                                  (name :String, mutability :Mutability, self: => IndexedIteratorFactory[A, Iterator])
	extends ArrayLikeIteratorFactory[A, ArrayIterator](name, mutability, self)
{
	protected final override def make[T](array :A[T], from :Int, until :Int) :ArrayIterator[T] =
		((array :ArrayLike[_]) match {
			case a :Array[AnyRef]  => new ArrayIterator(a, from, until, isImmutable)
			case a :Array[Int]     => new ArrayIterator(a, from, until, isImmutable)
			case a :Array[Long]    => new ArrayIterator(a, from, until, isImmutable)
			case a :Array[Double]  => new ArrayIterator(a, from, until, isImmutable)
			case a :Array[Byte]    => new ArrayIterator(a, from, until, isImmutable)
			case a :Array[Char]    => new ArrayIterator(a, from, until, isImmutable)
			case a :Array[Float]   => new ArrayIterator(a, from, until, isImmutable)
			case a :Array[Short]   => new ArrayIterator(a, from, until, isImmutable)
			case a :Array[Boolean] => new ArrayIterator(a, from, until, isImmutable)
			case null              => null_!(s"ArrayIterator.over(null, $from, $until)")
//			case _                 => new ArrayIterator(array, from, until)
		}).castParam[T]
}


@SerialVersionUID(Ver)
private class GenericArrayIteratorFactory[-A[X] <: ArrayLike[X]]
              (name :String, mutability :Mutability, self: => IndexedIteratorFactory[A, Iterator])
	extends ArrayLikeIteratorFactory[A, ArrayIterator](name, mutability, self)
{
	protected final override def make[T](array :A[T], from :Int, until :Int) :ArrayIterator[T] =
		new ArrayIterator(array.asInstanceOf[Array[T]], from, until, isImmutable)
}


@SerialVersionUID(Ver)
private class RefArrayLikeIteratorFactory[-A[X] <: RefArrayLike[X]]
              (name :String, mutability :Mutability, self: => IndexedIteratorFactory[A, Iterator])
	extends ArrayLikeIteratorFactory[A, ArrayIterator](name, mutability, self)
{
	protected final override def make[E](array :A[E], from :Int, until :Int) :ArrayIterator[E] =
		(array :ArrayLike[_]) match {
			case null =>
				illegal_!(toString + " cannot create an ArrayIterator because array is null.")
			case refs :Array[AnyRef] if refs.getClass == classOf[Array[AnyRef]] =>
				new ArrayIterator(refs, from, until, isImmutable).asInstanceOf[ArrayIterator[E]]
			case _ =>
				illegal_!(
					toString + " cannot create an ArrayIterator for an array with element type different than AnyRef: "
						+ errorString(array) + "."
				)
		}
}


//todo: make private, separately compiled.
/** An iterator advancing over a slice of an array. The advantages over built in array
  * [[collection.ArrayOps.iterator iterator]] are fast, in-place `take`, and `copyToArray` delegating to `Array.copy`,
  * making it considerably faster than copying by one.
  * @param first    the index in the array of the first/next element to return.
  * @param `last++` the index in the array delimiting the iterator, that is pointing after the last element
  *                 the iterator should return.
  */ //consider: Pow2ArrayIterator using bit masking instead of modulo.
@SerialVersionUID(Ver)
private[sugar] sealed class ArrayIterator[@specialized(MultiValue) +T] private[sugar]
	                                     (array :Array[T], first :Int, `last++` :Int,
	                                      override val isImmutable :Boolean = false)
	extends AbstractIndexedIterator[T](first, `last++`) with ArrayIteratorOps[T] with Serializable
{
	def this(array :Array[T]) = this(array, 0, array.length, false)

	private[sugar] final override def unsafeArray :Array[_] = array
//	final override def index :Int = first
//	protected final override def index_=(i :Int) :Unit = first = i
//	final override def limit :Int = `last++`
//	protected final override def limit_=(i :Int) :Unit = `last++` = i

	def reverse :ReverseArrayIterator[T] = new ReverseArrayIterator[T](array, first - 1, `last++` - 1)

//	final override def hasNext :Boolean = first < `last++`
//	override def head :T =
//		if (first < `last++`) array(first)
//		else noSuch_!("Index " + first + " exceeds the limit of " + `last++` + '.')
	override def head :T =
		if (hasNext) array(index)
		else noSuch_!("Limit of " + limit + " reached for array " + errorString(array) + ".")

	override def next() :T = {
		if (!hasNext)
			noSuch_!("Limit of " + limit + " reached for array " + errorString(array) + ".")
		array(advance())
	}
//	override def next() :T = {
//		if (first >= `last++`)
//			noSuch_!("Index " + first + " exceeds the limit of " + `last++` + ".")
//		val res = array(first)
//		first += 1
//		res
//	}
	override def clone = new ArrayIterator(array, first, `last++`, isImmutable)

	override def copyToArray[B >: T](xs :Array[B], start :Int, len :Int) :Int = {
		val copied = super.copyToArray(xs, start, len) //Delegate to ArrayIterableOnce.
		index += copied
		copied
	}

	override def toString :String = "iterator" + "|" + knownSize + "|(" + errorString(array) + ")@" + index
}






/** A factory of iterators advancing over a slice of an array in the reverse direction.
  * @tparam A   The kind of arrays this factory iterates over.
  * @tparam I   The type of the created iterator, `I[X] <: Iterator[X]`
  * @define Coll `ReverseArrayIterator`
  * @define coll reverse iterator
  */
private sealed abstract class ReverseArrayLikeIteratorFactory[-A[X] <: ArrayLike[X], +I[X] <: Iterator[X]] protected
                              (name :String, mutability :Mutability, self: => IndexedIteratorFactory[A, Iterator])
	extends ArrayLikeIteratorFactory[A, I](name, mutability, self) with ReverseIndexedIteratorFactory[A, I]


@SerialVersionUID(Ver)
private class ReverseArrayIteratorFactory[-A[X] <: ArrayLike[X]] private[arrays]
              (name :String, mutability :Mutability, self: => IndexedIteratorFactory[A, Iterator])
	extends ReverseArrayLikeIteratorFactory[A, ReverseArrayIterator](name, mutability, self)
{
	protected final override def make[T](array :A[T], from :Int, until :Int) :ReverseArrayIterator[T] =
		((array :ArrayLike[_]) match {
			case a :Array[AnyRef]  => new ReverseArrayIterator(a, from - 1, until - 1)
			case a :Array[Int]     => new ReverseArrayIterator(a, from - 1, until - 1)
			case a :Array[Long]    => new ReverseArrayIterator(a, from - 1, until - 1)
			case a :Array[Double]  => new ReverseArrayIterator(a, from - 1, until - 1)
			case a :Array[Byte]    => new ReverseArrayIterator(a, from - 1, until - 1)
			case a :Array[Char]    => new ReverseArrayIterator(a, from - 1, until - 1)
			case a :Array[Float]   => new ReverseArrayIterator(a, from - 1, until - 1)
			case a :Array[Short]   => new ReverseArrayIterator(a, from - 1, until - 1)
			case a :Array[Boolean] => new ReverseArrayIterator(a, from - 1, until - 1)
			case null              => null_!(s"ReverseArrayIterator.over(null, $from, $until)")
//			case _                 => new ReverseArrayIterator(array, from, until)
		}).castParam[T]
}


@SerialVersionUID(Ver)
private class ReverseGenericArrayLikeIteratorFactory[-A[X] <: ArrayLike[X]] private[arrays]
              (name :String, mutability :Mutability, self: => IndexedIteratorFactory[A, Iterator])
	extends ReverseArrayLikeIteratorFactory[A, ReverseArrayIterator](name, mutability, self)
{
	protected final override def make[T](array :A[T], from :Int, until :Int) :ReverseArrayIterator[T] =
		new ReverseArrayIterator(array.asInstanceOf[Array[T]], from - 1, until - 1)
}


@SerialVersionUID(Ver)
private class ReverseRefArrayLikeIteratorFactory[-A[X] <: RefArrayLike[X]] private[arrays]
              (name :String, mutability :Mutability, self: => IndexedIteratorFactory[A, Iterator])
	extends ReverseArrayLikeIteratorFactory[A, ReverseArrayIterator](name, mutability, self)
{
	protected final override def make[E](array :A[E], from :Int, until :Int) :ReverseArrayIterator[E] =
		(array :ArrayLike[_]) match {
			case null =>
				illegal_!(toString + " cannot create a ReverseArrayIterator because array is null.")
			case refs :Array[AnyRef] if refs.getClass == classOf[Array[AnyRef]] =>
				new ReverseArrayIterator(refs, from - 1, until - 1).asInstanceOf[ReverseArrayIterator[E]]
			case _ =>
				illegal_!(toString +
					" cannot create a ReverseArrayIterator for an array with element type different than AnyRef: "
						+ errorString(array) + "."
				)
		}
}



/** An iterator advancing over a slice of an array in the reverse direction.
  * The advantage over `ArrayOps.reverseIterator` is `O(1)` `take` and `slice` (and fast `drop`, like in the latter).
  * @param end   the index in the array `end <= first` immediately before the last element to return (may be `-1`).
  * @param first the index in the array of the first element to return (the end of the slice).
  */
@SerialVersionUID(Ver)
private[sugar] sealed class ReverseArrayIterator[@specialized(MultiValue) +T] private[sugar]
	                        (array :Array[T], end :Int, first :Int)
	extends AbstractReverseIndexedIterator[T](end, first) with ValIterator.Buffered[T] with Serializable
{
	def this(array :Array[T]) = this(array, -1, array.length - 1)

	private def unsafeArray :Array[_] = array

	override def head :T =
		if (hasNext) array(index)
		else noSuch_!("The index has reached the lower bound of " + limit + " for " + errorString(array) + ".")

	override def next() :T = {
		if (!hasNext)
			noSuch_!("Index " + index + " has reached the lower bound for " + errorString(array) + ".")
		array(advance())
	}

	final override def foldLeft[@specialized(Fun2Arg) A](z :A)(op :(A, T) => A) :A = {
		var res = z
		val end = limit
		var i   = index
		while (end < i) {
			res = op(res, array(i))
			i -= 1
		}
		index = end
		res
	}

	final override def copyToArray[B >: T](xs :Array[B], start :Int, len :Int) :Int = {
		val xsLength = xs.length
		if (len <= 0 | start >= xsLength/* | xsLength == 0*/ || first <= end)
			0
		else if (start < 0)
			outOfBounds_!(
				errorString(this) + ".copyToArray(" + errorString(xs) + ", " + start + ", " + len + ")"
			)
		else {
			val copied = math.min(size, math.min(len, xsLength - start))
			val end = start + copied
			try
				specCopyToArray(xs.asInstanceOf[Array[T]], start, end)
			catch {
				case _ :ClassCastException | _ :ArrayStoreException =>
					var i = start
					var j = index
					while (i < end) {
						xs(i) = array(j)
						j -= 1
						i += 1
					}
			}
			index += copied
			copied
		}
	}
	private def specCopyToArray(xs :Array[T @uncheckedVariance], from :Int, until :Int) :Unit = {
		var i = from
		var j = index
		while (i < until) {
			xs(i) = array(j)
			j -= 1
			i += 1
		}
	}

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :ReverseArrayIterator[_] =>
			(array eq other.unsafeArray) && index == other.index && limit == other.limit
		case _ => false
	}
	override def hashCode :Int = (array.identityHashCode * 31 + index.hashCode) * 31 + limit.hashCode

	override def clone = new ReverseArrayIterator(array, end, first)

	override def toString :String = "reverseIterator|" + knownSize + "|(" + errorString(array) + ")@" + index
}






/** A factory of iterators advancing over array slices, which wrap at the end of the array.
  * @tparam A The kind of arrays this factory iterates over.
  * @tparam I The type of the created iterator, `I[X] <: Iterator[X]`
  * @define Coll `CyclicArrayIterator`
  * @define coll cyclic iterator
  */ //todo: make it package private and move the documentation to its values.
private abstract class CyclicArrayLikeIteratorFactory[-A[X] <: ArrayLike[X], +I[X] <: Iterator[X]] protected
                       (name :String, mutability :Mutability, self: => IndexedIteratorFactory[A, Iterator])
	extends ArrayLikeIteratorFactory[A, I](name, mutability, self) with CyclicIteratorFactory[A, I]


@SerialVersionUID(Ver)
private sealed class CyclicArrayIteratorFactory[-A[X] <: ArrayLike[X]] private[arrays]
                     (name :String, mutability :Mutability, self: => IndexedIteratorFactory[A, Iterator])
	extends CyclicArrayLikeIteratorFactory[A, CyclicArrayIterator](name, mutability, self)
{
	protected final override def make[T](array :A[T], offset :Int, length :Int) :CyclicArrayIterator[T] =
		((array :ArrayLike[_]) match {
			case a :Array[AnyRef]       => new CyclicArrayIterator(a, offset, length)
			case a :Array[Int]          => new CyclicArrayIterator(a, offset, length)
			case a :Array[Long]         => new CyclicArrayIterator(a, offset, length)
			case a :Array[Double]       => new CyclicArrayIterator(a, offset, length)
			case a :Array[Byte]         => new CyclicArrayIterator(a, offset, length)
			case a :Array[Char]         => new CyclicArrayIterator(a, offset, length)
			case a :Array[Float]        => new CyclicArrayIterator(a, offset, length)
			case a :Array[Short]        => new CyclicArrayIterator(a, offset, length)
			case a :Array[Boolean]      => new CyclicArrayIterator(a, offset, length)
			case a :Array[T @unchecked] => new CyclicArrayIterator(a, offset, length)
		}).castParam[T]
}


@SerialVersionUID(Ver)
private class CyclicGenericArrayIteratorFactory[-A[X] <: ArrayLike[X]] private[arrays]
              (name :String, mutability :Mutability, self: => IndexedIteratorFactory[A, Iterator])
	extends CyclicArrayLikeIteratorFactory[A, CyclicArrayIterator](name, mutability, self)
{
	protected final override def make[T](source :A[T], first :Int, size :Int) :CyclicArrayIterator[T] =
		new CyclicArrayIterator(source.asInstanceOf[Array[T]], first, size)
}


@SerialVersionUID(Ver)
private class CyclicRefArrayLikeIteratorFactory[-A[X] <: RefArrayLike[X]] private[arrays]
              (name :String, mutability :Mutability, self: => IndexedIteratorFactory[A, Iterator])
	extends CyclicArrayLikeIteratorFactory[A, CyclicArrayIterator](name, mutability, self)
{
	protected final override def make[T](source :A[T], first :Int, size :Int) :CyclicArrayIterator[T] =
		(source :ArrayLike[_]) match {
			case null =>
				illegal_!(toString + " cannot create a CyclicArrayIterator because array is null.")
			case refs :Array[AnyRef] => new CyclicArrayIterator(refs, first, size).asInstanceOf[CyclicArrayIterator[T]]
			case _ =>
				illegal_!(toString +
					" cannot create a CyclicArrayIterator for an array with element type different than AnyRef: "
					+ errorString(source) + "."
				)
		}
}


/** An iterator advancing over an array, potentially wrapping over the end of the array back to the beginning.
  * @param first the index in the array of the first/next element to return.
  * @param size  the remaining number of elements to iterate over.
  */ //todo: implement CyclicIndexedIterator
@SerialVersionUID(Ver)
private[sugar] sealed class CyclicArrayIterator[@specialized(MultiValue) +T] private[sugar]
	                        (array :Array[T], first :Int, size :Int)
	extends AbstractCyclicIterator[T](first, size, array.length) with ValIterator.Buffered[T] with Serializable
{
	def this(array :Array[T]) = this(array, 0, array.length)

	private def unsafeArray :Array[_] = array

	override def head :T =
		if (remaining > 0) array(index)
		else noSuch_!("Index has reached the limit of " + index + " in " + errorString(array) + ".")

	override def next() :T = {
		if (remaining <= 0)
			noSuch_!("Index has reached the limit of " + index + " in " + errorString(array) + ".")
		val res = array(index)
		advance()
		res
	}

	override def foldLeft[@specialized(Fun2Arg) A](z :A)(op :(A, T) => A) :A = {
		val idx = index
		val end = idx + remaining
		if (end + Int.MinValue <= array.length + Int.MinValue)
			ArrayLikeSpecOps.foldLeft(array, idx, { index = end; end })(z)(op)
		else {
			val acc = ArrayLikeSpecOps.foldLeft(array, idx, array.length)(z)(op)
			ArrayLikeSpecOps.foldLeft(array, 0, { index = end - array.length; end - array.length })(acc)(op)
		}
	}

	override def copyToArray[B >: T](xs :Array[B], start :Int, len :Int) :Int = {
		val xsLength = xs.length
		if (len <= 0 | start >= xsLength | /*xsLength == 0 | */remaining <= 0)
			0
		else if (start < 0)
			outOfBounds_!(start.toString + " out of [0, " + xsLength + ")")
		else {
			val idx = index
			val copied = math.min(len, math.min(remaining, xsLength - start))
			Array.cyclicCopy(array, index, xs, start, copied)
			index = (idx + copied) % array.length
			remaining -= copied
			copied
		}
	}

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :CyclicArrayIterator[_] =>
			(array eq other.unsafeArray) && index == other.index && limit == other.limit
		case _ => false
	}
	override def hashCode :Int = (System.identityHashCode(array) * 31 + index) * 31 + remaining
	override def clone = new CyclicArrayIterator(array, index, remaining)
}






/** An iterator returning elements from a possibly wrapped section of an array in the inverse order.
  * The index arguments to `apply` and `slice` are treated modulo the length of the array.
  * @tparam A   The kind of arrays this factory iterates over.
  * @tparam I   The type of the created iterator, `I[X] <: Iterator[X]`
  * @define Coll `ReverseCyclicArrayIterator`
  * @define coll reverse cyclic iterator
  */
private sealed abstract class ReverseCyclicArrayLikeIteratorFactory[-A[X] <: ArrayLike[X], +I[X] <: Iterator[X]] private[arrays]
                              (name :String, mutability :Mutability, self: => IndexedIteratorFactory[A, Iterator])
	extends ArrayLikeIteratorFactory[A, I](name, mutability, self) with ReverseCyclicIteratorFactory[A, I]


@SerialVersionUID(Ver)
private sealed class ReverseCyclicArrayIteratorFactory[-A[X] <: ArrayLike[X]] private[arrays]
                     (name :String, mutability :Mutability, self: => IndexedIteratorFactory[A, Iterator])
	extends ReverseCyclicArrayLikeIteratorFactory[A, ReverseCyclicArrayIterator](name, mutability, self)
{
	protected final override def make[T](array :A[T], offset :Int, length :Int) :ReverseCyclicArrayIterator[T] =
		((array :ArrayLike[_]) match {
			case a :Array[AnyRef]       => new ReverseCyclicArrayIterator(a, offset, length)
			case a :Array[Int]          => new ReverseCyclicArrayIterator(a, offset, length)
			case a :Array[Long]         => new ReverseCyclicArrayIterator(a, offset, length)
			case a :Array[Double]       => new ReverseCyclicArrayIterator(a, offset, length)
			case a :Array[Byte]         => new ReverseCyclicArrayIterator(a, offset, length)
			case a :Array[Char]         => new ReverseCyclicArrayIterator(a, offset, length)
			case a :Array[Float]        => new ReverseCyclicArrayIterator(a, offset, length)
			case a :Array[Short]        => new ReverseCyclicArrayIterator(a, offset, length)
			case a :Array[Boolean]      => new ReverseCyclicArrayIterator(a, offset, length)
			case a :Array[T @unchecked] => new ReverseCyclicArrayIterator(a, offset, length)
		}).castParam[T]
}


@SerialVersionUID(Ver)
private class ReverseCyclicGenericArrayIteratorFactory[-A[X] <: ArrayLike[X]] private[arrays]
              (name :String, mutability :Mutability, self: => IndexedIteratorFactory[A, Iterator])
	extends ReverseArrayLikeIteratorFactory[A, ReverseCyclicArrayIterator](name, mutability, self)
{
	protected final override def make[E](array :A[E], offset :Int, length :Int) :ReverseCyclicArrayIterator[E] =
		new ReverseCyclicArrayIterator(array.asInstanceOf[Array[E]], offset, length)
}


@SerialVersionUID(Ver)
private class ReverseCyclicRefArrayLikeIteratorFactory[-A[X] <: RefArrayLike[X]] private[arrays]
              (name :String, mutability :Mutability, self: => IndexedIteratorFactory[A, Iterator])
	extends ReverseCyclicArrayLikeIteratorFactory[A, ReverseCyclicArrayIterator](name, mutability, self)
{
	protected final override def make[T](source :A[T], first :Int, size :Int) :ReverseCyclicArrayIterator[T] =
		(source :ArrayLike[_]) match {
			case null =>
				illegal_!(toString + " cannot create a ReverseCyclicArrayIterator because array is null.")
			case refs :Array[AnyRef] =>
				new ReverseCyclicArrayIterator(refs, first, size).asInstanceOf[ReverseCyclicArrayIterator[T]]
			case _ =>
				illegal_!(toString +
					" cannot create a ReverseCyclicArrayIterator for an array with element type different than AnyRef: "
					+ errorString(source) + "."
				)
		}
}



/** An iterator advancing over an array in the reverse direction, wrapping back at index `0`
  * back to the end of the array.
  * @param first the index in the array of the next element to return.
  * @param size  the size of the iterator.
  */
@SerialVersionUID(Ver)
private[sugar] sealed class ReverseCyclicArrayIterator[@specialized(MultiValue) +T] private[sugar]
	                        (array :Array[T], first :Int, size :Int)
	extends AbstractReverseCyclicIterator[T](first, size, 0, array.length)
	   with ValIterator.Buffered[T] with Serializable
{
	def this(array :Array[T]) = this(array, array.length - 1, array.length)

	private def unsafeArray :Array[_] = array

	override def head :T =
		if (remaining > 0) array(index)
		else noSuch_!("Index has reached the lower bound of " + index + ".")

	override def next() :T = {
		if (!hasNext)
			noSuch_!("Index has reached the lower bound of " + index + ".")
		array(advance())
	}

	override def foldLeft[@specialized(Fun2Arg) A](z :A)(op :(A, T) => A) :A = {
		val inverse = (elem :T, acc :A) => op(acc, elem)
		val idx = index
		val lo  = idx - remaining
		if (lo >= -1) {
			val res = ArrayLikeSpecOps.foldRight(array, lo + 1, idx + 1)(z)(inverse)
			index = lo
			remaining = 0
			res
		} else {
			val len = array.length
			val acc = ArrayLikeSpecOps.foldRight(array, 0, idx + 1)(z)(inverse)
			val res = ArrayLikeSpecOps.foldRight(array, len + lo + 1, len)(acc)(inverse)
			index = lo + len
			remaining = 0
			res
		}
	}

	final override def copyToArray[B >: T](xs :Array[B], start :Int, len :Int) :Int = {
		val xsLength = xs.length
		if (len <= 0 | start >= xsLength/* | xsLength == 0*/ | remaining <= 0)
			0
		else if (start < 0)
			outOfBounds_!(
				errorString(this) + ".copyToArray(" + errorString(xs) + ", " + start + ", " + len + ")"
			)
		else {
			val size   = remaining
			val copied = math.min(size, math.min(len, xsLength - start))
			val until  = start + copied
			try
				specCopyToArray(xs.asInstanceOf[Array[T]], start, until)
			catch {
				case _ :ClassCastException | _ :ArrayStoreException =>
					var i   = start
					var idx = index
					var end = math.max(-1, idx - copied)
					while (i < until) {
						while (idx > end) {
							xs(i) = array(idx)
							i   += 1
							idx -= 1
						}
						if (i < until) {
							idx = array.length - 1
							end = array.length - 1 - (until - i)
						}
					}
					index = idx
			}
			remaining = size - copied
			copied
		}
	}
	private def specCopyToArray(xs :Array[T @uncheckedVariance], from :Int, until :Int) :Unit = {
		var i   = from
		var j   = index
		var end = math.max(-1, j - (until - from))
		while (i < until) {
			while (j > end) {
				xs(i) = array(j)
				i += 1
				j -= 1
			}
			if (i < until) {
				j   = array.length - 1
				end = array.length - 1 - (until - i)
			}
		}
		index = j
	}

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :ReverseCyclicArrayIterator[_] =>
			(array eq other.unsafeArray) && index == other.index && limit == other.limit
		case _ => false
	}
	override def hashCode :Int = (System.identityHashCode(array) * 31 + index) * 31 + remaining
	override def clone = new ReverseCyclicArrayIterator(array, index, remaining)
}
