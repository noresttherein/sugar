package net.noresttherein.sugar.arrays

import scala.Specializable.Everything
import scala.collection.AbstractIterator

import net.noresttherein.sugar.arrays.extensions.ArrayCompanionExtension
import net.noresttherein.sugar.casting.castTypeParamMethods
import net.noresttherein.sugar.collections.{ArrayIterableOnce, ArrayLikeSliceWrapper, ElemTypes, IArrayLikeSlice, IndexedIterator, IndexedReverseIterator, ValIterator}
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.reflect.Specialized.{Fun2Arg, MultiValue}
import net.noresttherein.sugar.slang.extensions.hashCodeMethods






private[sugar] trait ArrayIteratorOps[@specialized(MultiValue) +T]
	extends ValIterator.Buffered[T] with IndexedIterator[T] with ArrayIterableOnce[T]
{
	private[sugar] final override def startIndex  :Int = index
	protected final override def underlyingSize :Int = unsafeArray.length

	@inline private[this] def array :Array[T] = unsafeArray.asInstanceOf[Array[T]]

	//consider: not implementing them here, so JVM knows there is a single implementation if only ArrayIterator is used.
	override def head :T =
		if (index < limit) array(index)
		else throw new NoSuchElementException("Index " + index + " exceeds the limit of " + limit + '.')

	override def next() :T = {
		val i = index
		if (i >= limit)
			throw new NoSuchElementException("Index " + index + " exceeds the limit of " + limit + ".")
		val res = array(i)
		index = i + 1
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

	final override def copyToArray[A >: T](xs :Array[A], start :Int, len :Int) :Int = {
		val first    = index
		val `last++` = limit
		val xsLength = xs.length
		if (len <= 0 | xsLength == 0 | start >= xsLength | first >= `last++`)
			0
		else if (start < 0)
			throw new IndexOutOfBoundsException(start.toString + " out of [0, " + xsLength + ")")
		else {
			val copied = math.min(len, math.min(`last++` - first, xsLength - start))
			ArrayLike.copy(array, first, xs, start, copied)
			index = first + copied
			copied
		}
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
	override def hashCode :Int = ArrayLike.Wrapped.Slice(array, index, limit).hashCode
}



/** Base class for array iterator factories/companions.
  * @tparam A   The kind of arrays this factory iterates over.
  * @tparam I   The type of the created iterator, `I[X] <: Iterator[X]`
  * @define Coll `ArrayIterator`
  * @define coll array iterator
  */ //todo: reverse the order of type parameters in ArrayLikeSliceWrapper
private[sugar] abstract class ArrayLikeIteratorFactory[A[X] <: ArrayLike[X], I[X] <: Iterator[X]]
	extends ArrayLikeSliceWrapper[I, A]
{
	@inline final def apply[E](array :A[E]) :I[E] = wrap(array)

	/** Returns elements `array(first), array(first + 1), ..., array(first + length - 1)` of the given array.
	  * If reading would go past the end of the array, the excess index range is ignored. Negative `length`
	  * is equivalent to zero.
	  */
	@throws[IndexOutOfBoundsException]("if offset is negative or greater than the length of the array.")
	def apply[T](array :A[T], first :Int, length :Int) :I[T] = {
		val len   = array.length
		if (first < 0 | first > len)
			throw new IndexOutOfBoundsException(first.toString + " is out of bounds [0, " + len + ")")
		val until = first + math.min(len - first, math.max(length, 0))
		make(array, first, until)
	}

	def from[T](array :A[T], first :Int) :I[T] = {
		val length = array.length
		if (first >= length) make(array, length, length)
		else if (first <= 0) make(array, 0, length)
		else make(array, first, length)
	}

	/** Returns elements `array(from), array(from + 1), ..., array(until - 1)` of the given array.
	  * If any of indices in the `[from, until)` range are negative or greater than the array's length, they are ignored.
	  */
	override def slice[T](array :A[T], from :Int, until :Int) :I[T] = {
		val len = array.length
		if (from >= len) make(array, len, len)
		else if (until <= 0) make(array, 0, 0)
		else if (from <= 0 && until >= len) make(array, 0, len)
		else if (from <= 0) make(array, 0, until)
		else if (until >= len) make(array, from, len)
		else if (until <= from) make(array, from, from)
		else make(array, from, until)
	}
}


@SerialVersionUID(Ver)
private[sugar] sealed class ArrayIteratorFactory[A[X] <: ProperArray[X]]
	extends ArrayLikeIteratorFactory[A, ArrayIterator]
{
	protected override def make[T](array :A[T], from :Int, until :Int) :ArrayIterator[T] =
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
			case null              => throw new NullPointerException(s"ArrayIterator.over(null, $from, $until)")
//			case _                 => new ArrayIterator(array, from, until)
		}).castParam[T]

	/** Same as `ArrayIterator(array)`, but always produces an erased, not specialized instance. */
	def generic[T](array :Array[T]) :ArrayIterator[T] =
		if (array == null) throw new NullPointerException("ArrayIterator(null)")
		else new ArrayIterator(array)

	/** Same as `ArrayIterator(array, first, length)`, but always produces an erased, not specialized instance. */
	def generic[T](array :Array[T], first :Int, length :Int) :ArrayIterator[T] = {
		val len = array.length
		if (first < 0 | first > len)
			throw new IndexOutOfBoundsException(first.toString + " is out of bounds [0, " + len + ")")
		val until = first + math.min(len - first, math.max(length, 0))
		new ArrayIterator[T](array, first, until, false)
	}
}

private[sugar] sealed class RefArrayLikeIteratorFactory[A[X] <: RefArrayLike[X]]
	extends ArrayLikeIteratorFactory[A, ArrayIterator]
{
	override protected def make[E](array :A[E], from :Int, until :Int) :ArrayIterator[E] =
		(array :ArrayLike[_]) match {
			case null =>
				throw new IllegalArgumentException("Cannot create an ArrayIterator because array is null")
			case refs :Array[AnyRef] if refs.getClass == classOf[Array[AnyRef]] =>
				new ArrayIterator(refs, from, until, isImmutable).asInstanceOf[ArrayIterator[E]]
			case _ =>
				throw new IllegalArgumentException(
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
  */
@SerialVersionUID(Ver)
private[sugar] sealed class ArrayIterator[@specialized(MultiValue) +T] private[sugar]
	                                     (array :Array[T], private[this] var first :Int, private[this] var `last++` :Int,
	                                      override val isImmutable :Boolean = false)
	extends AbstractIterator[T] with ArrayIteratorOps[T] with Serializable
{
	def this(array :Array[T]) = this(array, 0, array.length, false)

	private[sugar] final override def unsafeArray :Array[_] = array
	final override def index :Int = first
	protected final override def index_=(i :Int) :Unit = first = i
	final override def limit :Int = `last++`
	protected final override def limit_=(i :Int) :Unit = `last++` = i

	def reverse :ReverseArrayIterator[T] = new ReverseArrayIterator[T](array, first, `last++`)

	final override def hasNext :Boolean = first < `last++`
	override def head :T =
		if (first < `last++`) array(first)
		else throw new NoSuchElementException("Index " + first + " exceeds the limit of " + `last++` + '.')

	override def next() :T = {
		if (first >= `last++`)
			throw new NoSuchElementException("Index " + first + " exceeds the limit of " + `last++` + ".")
		val res = array(first)
		first += 1
		res
	}
	override def clone = new ArrayIterator(array, first, `last++`, isImmutable)
}




/** A factory of iterators advancing over array slices. */
@SerialVersionUID(Ver)
private[sugar] case object ArrayIterator extends ArrayIteratorFactory[Array]

@SerialVersionUID(Ver)
private[sugar] case object IArrayIterator extends ArrayIteratorFactory[IArray] {
	override def isImmutable = true
}

@SerialVersionUID(Ver)
private[sugar] case object RefArrayIterator extends RefArrayLikeIteratorFactory[RefArray]

@SerialVersionUID(Ver)
private[sugar] case object IRefArrayIterator extends RefArrayLikeIteratorFactory[IRefArray] {
	override def isImmutable = true
}






/** An iterator advancing over a slice of an array in the reverse direction.
  * The advantage over `ArrayOps.reverseIterator` is `O(1)` `take` and `slice` (and fast `drop`, like in the latter).
  * @param last      the index in the array `last <= first++` of the last element to return
  *                  (the first index of the slice).
  * @param `first++` the index in the array pointing directly after the first/next element to return
  *                  (the end index of the slice).
  */
@SerialVersionUID(Ver)
private[sugar] sealed class ReverseArrayIterator[@specialized(Everything) +T] private[sugar]
	                        (array :Array[T], private[this] var last :Int, private[this] var `first++` :Int)
	extends AbstractIterator[T] with IndexedReverseIterator[T] with Serializable
{
	def this(array :Array[T]) = this(array, 0, array.length)

	private def unsafeArray :Array[_] = array
	protected final override def underlyingSize :Int = array.length
	final override def index :Int = `first++`
	protected final override def index_=(i :Int) :Unit = `first++` = i
	final override def limit :Int = last
	protected final override def limit_=(i :Int) :Unit = last = i

	final override def hasNext :Boolean = `first++` > last
	override def head :T =
		if (`first++` > last) array(`first++` - 1)
		else throw new NoSuchElementException("Index " + `first++` + " reached the lower bound of " + last + ".")

	override def next() :T = {
		if (`first++` <= last)
			throw new NoSuchElementException("Index " + `first++` + " reached the lower bound of " + last + ".")
		`first++` -= 1
		array(`first++`)
	}

	final override def foldLeft[A](z :A)(op :(A, T) => A) :A = {
		var res = z
		while (last < `first++`) {
			`first++` -= 1
			res = op(res, array(`first++`))
		}
		res
	}

	final override def copyToArray[B >: T](xs :Array[B], start :Int, len :Int) :Int = {
		val xsLength = xs.length
		if (len <= 0 | start >= xsLength | xsLength == 0 || `first++` <= last)
			0
		else if (start < 0)
			throw new IndexOutOfBoundsException(
				errorString(this) + ".copyToArray(" + errorString(xs) + ", " + start + ", " + len + ")"
			)
		else {
			val copied = math.min(size, math.min(len, xsLength - start))
			val end = `first++` - copied
			var i = start
			while (`first++` > end) {
				`first++` -= 1
				xs(i) = array(`first++`)
				i += 1
			}
			copied
		}
	}

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :ReverseArrayIterator[_] =>
			(array eq other.unsafeArray) && index == other.index && limit == other.limit
		case _ => false
	}
	override def hashCode :Int = (array.identityHashCode * 31 + index.hashCode) * 31 + limit.hashCode

	override def clone = new ReverseArrayIterator(array, last, `first++`)
}


/** A factory of iterators advancing over a slice of an array in the reverse direction. */
@SerialVersionUID(Ver)
private[sugar] object ReverseArrayIterator {
	private def make[T](array :Array[T], from :Int, until :Int) :ReverseArrayIterator[T] =
		((array :Array[_]) match {
			case a :Array[AnyRef]  => new ReverseArrayIterator(a, from, until)
			case a :Array[Int]     => new ReverseArrayIterator(a, from, until)
			case a :Array[Long]    => new ReverseArrayIterator(a, from, until)
			case a :Array[Double]  => new ReverseArrayIterator(a, from, until)
			case a :Array[Byte]    => new ReverseArrayIterator(a, from, until)
			case a :Array[Char]    => new ReverseArrayIterator(a, from, until)
			case a :Array[Float]   => new ReverseArrayIterator(a, from, until)
			case a :Array[Short]   => new ReverseArrayIterator(a, from, until)
			case a :Array[Boolean] => new ReverseArrayIterator(a, from, until)
			case null              => throw new NullPointerException(s"ReverseArrayIterator.over(null, $from, $until)")
//			case _                 => new ReverseArrayIterator(array, from, until)
		}).castParam[T]

	def apply[T](array :Array[T]) :ReverseArrayIterator[T] =
		make(array, 0, array.length)

	/** An iterator returning elements `seq(first), seq(first - 1), ..., seq(first - length + 1)`.
	  * If the iterator would need to access an element at index lesser than zero, the excess elements are ignored.
	  * Negative `length` is the same as zero.
	  */
	@throws[IndexOutOfBoundsException]("if offset is negative or greater or equal to the length of the array")
	def apply[@specialized(Int, Long, Double, AnyRef) T]
	         (array :Array[T], first :Int, length :Int) :ReverseArrayIterator[T] =
	{
		val len = array.length
		if (first < 0 | first >= len)
			throw new IndexOutOfBoundsException(first.toString + " is out of bounds [0, " + len + ")")
		val downTo = first + 1 - math.min(first + 1, math.max(length, 0))
		make(array, downTo, first + 1)
	}

	/** An iterator returning elements `array(hi - 1), array(hi - 2), ..., array(lo)`.
	  * If any of the indices in the `[lo, hi)` range is out of bounds for `array`, it is ignored.
	  */
	def slice[T](array :Array[T], lo :Int, hi :Int) :ReverseArrayIterator[T] = {
		val len = array.length
		if (hi <= 0) make(array, 0, 0)
		else if (lo >= len) make(array, len, len)
		else if (lo <= 0 && hi >= len) make(array, 0, len)
		else if (lo <= 0) make(array, 0, hi)
		else if (hi >= len) make(array, lo, len)
		else if (lo >= hi) make(array, hi, hi)
		else make(array, lo, hi)
	}

	/** Same as `ReverseArrayIterator(array)`, but always produces an erased, not specialized iterator. */
	def generic[T](array :Array[T]) :ReverseArrayIterator[T] =
		if (array == null) throw new NullPointerException("ReverseArrayIterator(null)")
		else new ReverseArrayIterator[T](array, 0, array.length)

	/** Same as `ReverseArrayIterator(array, first, length)`, but always produces an erased, not specialized iterator. */
	def generic[T](array :Array[T], first :Int, length :Int) :ReverseArrayIterator[T] = {
		val len = array.length
		if (first < 0 | first >= len)
			throw new IndexOutOfBoundsException(first.toString + " is out of bounds [0, " + len + ")")
		val downTo = first + 1 - math.min(first + 1, math.max(length, 0))
		new ReverseArrayIterator[T](array, downTo, first + 1)
	}
}






/** An iterator advancing over an array, potentially wrapping over the end of the array back to the beginning.
  * @param idx       the index in the array of the first/next element to return.
  * @param remaining the remaining number of elements to iterate over.
  */
@SerialVersionUID(Ver)
private[sugar] sealed class CyclicArrayIterator[@specialized(Int, Long, Double, AnyRef) +T] private[sugar]
	                        (array :Array[T], private[this] var idx :Int, private[this] var remaining :Int)
	extends AbstractIterator[T] with IndexedIterator[T] with Serializable
{
	def this(array :Array[T]) = this(array, 0, array.length)

	private[this] val len = array.length
	private def unsafeArray :Array[_] = array

	protected final override def underlyingSize :Int = len
	final override def index :Int = idx
	protected final override def index_=(i :Int) :Unit = { remaining -= i - idx; idx = i % len }
	final override def limit :Int = idx + remaining
	protected final override def limit_=(i :Int) :Unit = remaining = i - idx

	final override def knownSize :Int = remaining
	final override def hasNext :Boolean = remaining > 0
	override def head :T =
		if (remaining > 0) array(idx)
		else throw new NoSuchElementException("Index has reached the limit of " + idx + ".")

	override def next() :T = {
		if (remaining <= 0)
			throw new NoSuchElementException("Index has reached the limit of " + idx + ".")
		val res = array(idx)
		remaining -= 1
		idx = (idx + 1) % len
		res
	}

	override def foldLeft[A](z :A)(op :(A, T) => A) :A =
		if (idx + remaining <= len)
			ArrayLikeOps.foldLeft(array, idx, { idx += remaining; idx})(z)(op)
		else {
			val acc = ArrayLikeOps.foldLeft(array, idx, len)(z)(op)
			ArrayLikeOps.foldLeft(array, 0, { idx = idx + remaining - len; idx })(acc)(op)
		}

	override def copyToArray[B >: T](xs :Array[B], start :Int, len :Int) :Int = {
		val xsLength = xs.length
		if (len <= 0 | start >= xsLength | xsLength == 0 | remaining <= 0)
			0
		else if (start < 0)
			throw new IndexOutOfBoundsException(start.toString + " out of [0, " + xsLength + ")")
		else {
			val copied = math.min(len, math.min(remaining, xsLength - start))
			Array.cyclicCopy(array, idx, xs, start, copied)
			idx = (idx + copied) % this.len
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
	override def hashCode :Int = (System.identityHashCode(array) * 31 + idx) * 31 + remaining
	override def clone = new CyclicArrayIterator(array, idx, remaining)
}


/** A factory of iterators advancing over array slices. */
@SerialVersionUID(Ver)
private[sugar] object CyclicArrayIterator {
	private def make[T](array :Array[T], offset :Int, length :Int) :CyclicArrayIterator[T] =
		((array :Array[_]) match {
			case a :Array[AnyRef]  => new CyclicArrayIterator(a, offset, length)
			case a :Array[Int]     => new CyclicArrayIterator(a, offset, length)
			case a :Array[Long]    => new CyclicArrayIterator(a, offset, length)
			case a :Array[Double]  => new CyclicArrayIterator(a, offset, length)
//			case a :Array[Byte]    => new CyclicArrayIterator(a, offset, length)
//			case a :Array[Char]    => new CyclicArrayIterator(a, offset, length)
//			case a :Array[Float]   => new CyclicArrayIterator(a, offset, length)
//			case a :Array[Short]   => new CyclicArrayIterator(a, offset, length)
//			case a :Array[Boolean] => new CyclicArrayIterator(a, offset, length)
			case _                 => new CyclicArrayIterator(array, offset, length)
		}).castParam[T]

	/** An iterator returning `length` elements of the array, starting with `array(offset)`. If `offset + length`
	  * is greater than the length of the array, then the iterator wraps to the beginning of the array,
	  * returning `array(0)` following `array(array.length - 1)`, and so on, until `min(length, array.length)`
	  * elements are returned. If `length` is negative, the iterator will have no elements.
	  */
	@throws[IndexOutOfBoundsException]("if offset is negative or greater than the length of the array.")
	def apply[T](array :Array[T], offset :Int, length :Int) :CyclicArrayIterator[T] = {
		val len   = array.length
		if (offset < 0 | offset > len)
			throw new IndexOutOfBoundsException(offset.toString + " out of bounds of [0, " + len + ")")
		make(array, offset, math.min(math.max(length, 0), len))
	}

	/** An iterator returning subsequent elements of an array, starting with index `from % array.length`,
	  * and increasing modulo the length of the array until index `until` is reached (exclusive).
	  */
	def slice[T](array :Array[T], from :Int, until :Int) :CyclicArrayIterator[T] = {
		val len    = array.length
		if (len == 0) //avoid division by zero
			make(array, 0, 0)
		else {
			val from0  = if (from < 0) len + from % len else from % len
			val until0 = if (until < 0) len + until % len else until % len
			make(array, (from + 1) % len, until0 - from0)
		}
	}

	/** An iterator over `[offset, offset + length)` index range of `array`, going in reverse.
	  * The first returned element will be the one at index `offset + length - 1`, and the last one at index `from`.
	  * Negative length is treated the same as zero, and offset is interpreted modulo `array.length`.
	  */
//	@throws[IndexOutOfBoundsException]("if first is negative or greater or equal to the length of the array")
//	def reversed[T](array :Array[T], offset :Int, length :Int) :ReverseCyclicArrayIterator[T] =
//		ReverseCyclicArrayIterator(array, offset, length)

	/** Same as `CyclicArrayIterator(array, offset, length)`, but always produces an erased, not specialized instance. */
	@throws[IndexOutOfBoundsException]("if offset is negative or greater than the length of the array.")
	def generic[T](array :Array[T], offset :Int, length :Int) :CyclicArrayIterator[T] = {
		val len = array.length
		if (offset < 0 | offset > len)
			throw new IndexOutOfBoundsException(offset.toString + " out of bounds of [0, " + len + ")")
		new CyclicArrayIterator(array, offset, math.min(math.max(length, 0), len))
	}

}




/** An iterator advancing over an an array in the reverse direction, wrapping back at index `0`
  * back to the end of the array.
  * @param idx       the index in the array of the next element to return.
  * @param remaining the size of the iterator.
  */
@SerialVersionUID(Ver)
private[sugar] sealed class ReverseCyclicArrayIterator[@specialized(Int, Long, Double, AnyRef) +T] private[sugar]
	                        (array :Array[T], private[this] var idx :Int, private[this] var remaining :Int)
	extends AbstractIterator[T] with IndexedReverseIterator[T] with Serializable
{
	def this(array :Array[T]) = this(array, 0, array.length)

	private[this] val len = array.length
	private def unsafeArray :Array[_] = array

	protected final override def underlyingSize :Int = len
	final override def index :Int = idx
	protected final override def index_=(i :Int) :Unit = { remaining -= idx - i; idx = if (i < 0) len + i else i }
	final override def limit :Int = idx - remaining
	protected final override def limit_=(i :Int) :Unit = remaining = idx - i

	final override def knownSize :Int = remaining
	final override def hasNext :Boolean = remaining > 0
	override def head :T =
		if (remaining > 0)
			if (idx == 0) array(len - 1) else array(idx - 1)
		else
			throw new NoSuchElementException("Index has reached the lower bound of " + idx + ".")

	override def next() :T = {
		if (remaining < 0)
			throw new NoSuchElementException("Index has reached the lower bound of " + idx + ".")
		remaining -= 1
		if (idx == 0) idx = len - 1 else idx -= 1
		array(idx)
	}

	override def foldLeft[A](z :A)(op :(A, T) => A) :A = {
		val inverse = (elem :T, acc :A) => op(acc, elem)
		if (idx - remaining >= 0) {
			idx -= remaining
			ArrayLikeOps.foldRight(array, idx, idx + remaining)(z)(inverse)
		} else {
			val acc = ArrayLikeOps.foldRight(array, 0, idx - remaining + len)(z)(inverse)
			val res = ArrayLikeOps.foldRight(array, idx, len)(acc)(inverse)
			idx = idx - remaining + len
			res
		}
	}

	final override def copyToArray[B >: T](xs :Array[B], start :Int, len :Int) :Int = {
		val xsLength = xs.length
		if (len <= 0 | start >= xsLength | xsLength == 0 | remaining <= 0)
			0
		else if (start < 0)
			throw new IndexOutOfBoundsException(
				errorString(this) + ".copyToArray(" + errorString(xs) + ", " + start + ", " + len + ")"
			)
		else {
			val copied = math.min(remaining, math.min(len, xsLength - start))
			val end = start + copied
			var i = start
			while (i < end) {
				if (idx == 0) idx = this.len - 1
				else idx -= 1
				xs(i) = array(idx)
				i += 1
			}
			remaining -= copied
			copied
		}
	}

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :ReverseCyclicArrayIterator[_] =>
			(array eq other.unsafeArray) && index == other.index && limit == other.limit
		case _ => false
	}
	override def hashCode :Int = (System.identityHashCode(array) * 31 + idx) * 31 + remaining
	override def clone = new ReverseCyclicArrayIterator(array, idx, remaining)
}


/** A factory of iterators advancing over a slice of an array in the reverse direction. */
@SerialVersionUID(Ver)
private[sugar] object ReverseCyclicArrayIterator {
	private def make[T](array :Array[T], offset :Int, length :Int) :ReverseCyclicArrayIterator[T] =
		((array :Array[_]) match {
			case a :Array[AnyRef]  => new ReverseCyclicArrayIterator(a, offset, length)
			case a :Array[Int]     => new ReverseCyclicArrayIterator(a, offset, length)
			case a :Array[Long]    => new ReverseCyclicArrayIterator(a, offset, length)
			case a :Array[Double]  => new ReverseCyclicArrayIterator(a, offset, length)
//			case a :Array[Byte]    => new ReverseCyclicArrayIterator(a, offset, length)
//			case a :Array[Char]    => new ReverseCyclicArrayIterator(a, offset, length)
//			case a :Array[Float]   => new ReverseCyclicArrayIterator(a, offset, length)
//			case a :Array[Short]   => new ReverseCyclicArrayIterator(a, offset, length)
//			case a :Array[Boolean] => new ReverseCyclicArrayIterator(a, offset, length)
			case _                 => new ReverseCyclicArrayIterator(array, offset, length)
		}).castParam[T]

	/** An iterator returning elements of an array in the decreasing index order, starting with `array(first)`.
	  * If `length > first + 1`, then, following `array(0)`, the iterator returns `array(array.length - 1)`,
	  * `array(array.length - 2)`, and so on, until `min(length, array.length)` elements are returned.
	  * If `length` is negative, the iterator will have no elements.
	  */
	@throws[IndexOutOfBoundsException]("if first is negative or greater or equal to the length of the array")
	def apply[T](array :Array[T], first :Int, length :Int) :ReverseCyclicArrayIterator[T] = {
		val len = array.length
		if (first < 0 | first > len)
			throw new IndexOutOfBoundsException(first.toString + " is out of bounds of [0, " + len + ")")
		make(array, first + 1, math.min(len, math.max(length, 0)))
	}

	/** An iterator returning elements of an array at decreasing indices, starting with `(until - 1) % array.length`,
	  * and ending (inclusive) with `from % array.length`. If `from` is greater than `until` modulo
	  * the length of the array, the element at `0` is followed by elements at `array.length - 1, array.length - 2`, etc.
	  * If `from == until`, the iterator is empty.
	  */
	def slice[T](array :Array[T], from :Int, until :Int) :ReverseCyclicArrayIterator[T] = {
		val len = array.length
		if (len == 0) //avoid division by zero!
			make(array, 0, 0)
		else {
			val from0  = if (from < 0) len + from % len else from % len
			val until0 = if (until < 0) len + until % len else until % len
			make(array, (until0 + len - 1) % len, until0 - from0)
		}
	}

	/** Same as `ReverseCyclicArrayIterator(array, first, length)`, but always produces an erased,
	  * not specialized iterator.
	  */
	@throws[IndexOutOfBoundsException]("if first is negative or greater or equal to the length of the array")
	def generic[T](array :Array[T], first :Int, length :Int) :ReverseCyclicArrayIterator[T] = {
		val len = array.length
		if (first < 0 | first >= len)
			throw new IndexOutOfBoundsException(first.toString + " is out of bounds of [0, " + len + ")")
		new ReverseCyclicArrayIterator(array, first + 1, math.min(len, math.max(length, 0)))
	}
}

