package net.noresttherein.sugar.collections

import java.util.Spliterator.{IMMUTABLE, ORDERED, SIZED, SUBSIZED}
import java.util.Spliterator

import scala.collection.{Stepper, StepperShape}
import scala.collection.Stepper.EfficientSplit
import scala.collection.StepperShape.{ByteShape, CharShape, DoubleShape, FloatShape, IntShape, LongShape, ShortShape}

import net.noresttherein.sugar.JavaTypes.{JDouble, JDoubleIterator, JInt, JIntIterator, JLong, JLongIterator}

//implicits
import net.noresttherein.sugar.extensions.castTypeParam




/** Base trait for implementations of steppers over slices of some sequential collections.
  * Aside from [[scala.collection.Stepper Stepper]], the trait implements also
  * [[java.util.Spliterator Spliterator]] and Java [[java.util.Iterator Iterator]].
  * Concrete implementations are expected to implement also the specialized versions of the three interfaces.
  * The stepper advances over a window on the collection; it is assumed to use random indexing
  * to return the elements, but they are never handled by this class itself.
  * @see [[net.noresttherein.sugar.collections.AbstractIndexedIterator]]
  * @see [[net.noresttherein.sugar.collections.AbstractIndexedReverseStepper]]
  * @author Marcin Mościcki
  */
private trait AbstractIndexedStepper[+A, B, +Self >: Null <: AllInStepper[A, B, Self]]
	extends AllInStepper[A, B, Self] with EfficientSplit with Cloneable
{
	protected def underlyingSize :Int
	protected var index :Int
	protected var limit :Int

	if (limit > underlyingSize)
		limit = underlyingSize
	if (index > limit)
		index = limit
	else if (index < 0)
		index = 0

	override def estimateSize :Long = limit - index
	override def hasStep :Boolean = index < limit
	override def hasNext :Boolean = index < limit

	override def trySplit() :Self = {
		val from = index; val until = limit
		if (from + 1 >= until)
			null
		else {
			val split = ((until - from) >> 2) + from
			index = split
			val res = clone
			index = from
			limit = split
			res
		}
	}

	override def characteristics :Int = ORDERED | SIZED | SUBSIZED
	override def clone :Self = super.clone.asInstanceOf[Self]

	override def toString :String = "Stepper(" + index + " until " + limit + ")"
}




/** Base trait for implementations of steppers traversing in the reverse order over slices
  * of some sequential collections. The iterator advances over a window on the collection; it is assumed to use
  * random indexing to return the elements, but they are never handled by this class itself.
  * Aside from [[scala.collection.Stepper Stepper]], the trait implements also
  * [[java.util.Spliterator Spliterator]] and Java [[java.util.Iterator Iterator]].
  * Concrete implementations are expected to implement also the specialized versions of the three interfaces.
  * @see [[net.noresttherein.sugar.collections.AbstractIndexedReverseIterator]]
  * @see [[net.noresttherein.sugar.collections.AbstractIndexedStepper]]
  * @author Marcin Mościcki
  */
private trait AbstractIndexedReverseStepper[+A, B, +Self >: Null <: AllInStepper[A, B, Self]]
	extends AllInStepper[A, B, Self] with EfficientSplit with Cloneable
{
	protected def underlyingSize :Int
	protected var index :Int
	protected var limit :Int

	if (index >= underlyingSize)
		index = underlyingSize
	if (limit > index)
		limit = index
	else if (limit < 0)
		limit = 0

	override def estimateSize :Long = index - limit
	override def hasStep :Boolean = index > limit
	override def hasNext :Boolean = index > limit

	override def trySplit() :Self = {
		val i = index; val downto = limit
		if (downto + 1 >= index)
			null
		else {
			val split = ((i - downto) >> 2) + downto
			index = split
			val res = clone
			index = downto
			limit = split
			res
		}
	}

	override def characteristics :Int = ORDERED | SIZED | SUBSIZED
	override def clone :Self = super.clone.asInstanceOf[Self]
	override def toString :String = "Stepper(" + index + " down to " + limit
}


/** A [[scala.collection.Stepper Stepper]] iterating over a slice of an indexed sequence, which at the same time
  * is its [[java.util.Spliterator Spliterator]] and Java [[java.util.Iterator Iterator]] to avoid unnecessary
  * object creation.
  * Subclasses exist for all value types and as well as an erased one, implementing the proper specialized interfaces:
  * `IntStepper` for `Char`, `DoubleStepper` for `Float`, etc.
  * @param seq      the sequence with elements over which to iterate.
  * @param first    the index of the first (and later current) element in the sequence (the one returned by `next()`).
  * @param `last++` the index immediately following the index of the last element in the slice.
  * @tparam A       the Scala type of the `Stepper`'s elements (this is the specialization after widening,
  *                 not the sequence element type).
  * @tparam B       the Java class for type `A`: for reference types it is simply `A`; for primitive types
  *                 it is its Java boxing class.
  * @author Marcin Mościcki
  */ //no reverse version currently
private abstract class IndexedSeqStepper[+A, B, +Self >: Null <: IndexedSeqStepper[A, B, Self]]
	                   (seq :collection.IndexedSeq[_], private[this] var first :Int, private[this] var `last++` :Int)
	extends AbstractIndexedStepper[A, B, Self]
{
	protected final override def underlyingSize :Int = seq.length
	protected final override def index :Int = first
	protected final override def index_=(i :Int) :Unit = first = i
	protected final override def limit :Int = `last++`
	protected final override def limit_=(i :Int) :Unit = `last++` = i

	override def hasStep :Boolean = first < `last++`

	override def characteristics :Int =
		if (seq.isInstanceOf[IndexedSeq[_]]) IMMUTABLE | super.characteristics
		else super.characteristics
}


@SerialVersionUID(ver)
object IndexedSeqStepper {
	/** A [[scala.collection.Stepper Stepper]] iterating over the entirety of an indexed sequence.
	  * The stepper will box the elements, but the result will be of the proper specialization for `A` -
	  * one of `Any`, `Int`, `Long`, `Double`.
	  * @param seq      the sequence with elements over which to iterate.
	  */ //we could make it work for collection.IndexedSeq
	def apply[A, S <: Stepper[_]](seq :collection.IndexedSeq[A])
	                             (implicit shape :StepperShape[A, S]) :S with EfficientSplit =
		apply(seq, 0, seq.length)

	/** A [[scala.collection.Stepper Stepper]] iterating over a slice of an indexed sequence.
	  * The stepper will box the elements, but the result will be of the proper specialization for `A` -
	  * one of `Any`, `Int`, `Long`, `Double`.
	  * @param seq      the sequence with elements over which to iterate.
	  * @param from     the index of the first (and later current) element in the sequence (the one returned by `next()`).
	  * @param until    the index immediately following the index of the last element in the slice.
	  */
	def apply[A, S <: Stepper[_]](seq :collection.IndexedSeq[A], from :Int, until :Int)
	                             (implicit shape :StepperShape[A, S]) :S with EfficientSplit =
		(shape.shape match {
			case IntShape    => new IntIndexedSeqStepper(seq.castParam[Int], from, until)
			case LongShape   => new LongIndexedSeqStepper(seq.castParam[Long], from, until)
			case DoubleShape => new DoubleIndexedSeqStepper(seq.castParam[Double], from, until)
			case CharShape   => new CharIndexedSeqStepper(seq.castParam[Char], from, until)
			case ByteShape   => new ByteIndexedSeqStepper(seq.castParam[Byte], from, until)
			case FloatShape  => new FloatIndexedSeqStepper(seq.castParam[Float], from, until)
			case ShortShape  => new ShortIndexedSeqStepper(seq.castParam[Short], from, until)
			case _           => new AnyIndexedSeqStepper(seq.castParam[Short], from, until)
		}).asInstanceOf[S with EfficientSplit]


	/** An [[scala.collection.AnyStepper AnyStepper]] iterating over a slice of an `IndexedSeq[A]`. */
	private class AnyIndexedSeqStepper[A](seq :collection.IndexedSeq[A], from :Int, until :Int)
		extends IndexedSeqStepper[A, A, AnyIndexedSeqStepper[A]](seq, from, until) with AllInAnyStepper[A] 
	{
		override def next() :A = { 
			val i = index; val res = seq(i); index = i + 1; res 
		}
		override def iterator :Iterator[A] = new IndexedSeqIterator(seq, index, limit)
		override def clone = new AnyIndexedSeqStepper(seq, index, limit)
	}

	/** A ''boxing'' [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `IndexedSeq[Int]`. */
	private class IntIndexedSeqStepper(seq :collection.IndexedSeq[Int], from :Int, until :Int)
		extends IndexedSeqStepper[Int, JInt, IntIndexedSeqStepper](seq, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = { val i = index; val res = seq(i); index = i + 1; res }
		override def clone = new IntIndexedSeqStepper(seq, index, limit)
	}

	/** A ''boxing'' [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `IndexedSeq[Char]`. */
	private class CharIndexedSeqStepper(seq :collection.IndexedSeq[Char], from :Int, until :Int)
		extends IndexedSeqStepper[Int, JInt, CharIndexedSeqStepper](seq, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = { val i = index; val res = seq(i); index = i + 1; res }
		override def clone = new CharIndexedSeqStepper(seq, index, limit)
	}

	/** A ''boxing'' [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `IndexedSeq[Byte]`. */
	private class ByteIndexedSeqStepper(seq :collection.IndexedSeq[Byte], from :Int, until :Int)
		extends IndexedSeqStepper[Int, JInt, ByteIndexedSeqStepper](seq, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = { val i = index; val res = seq(i); index = i + 1; res }
		override def clone = new ByteIndexedSeqStepper(seq, index, limit)
	}

	/** A ''boxing'' [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `IndexedSeq[Short]`. */
	private class ShortIndexedSeqStepper(seq :collection.IndexedSeq[Short], from :Int, until :Int)
		extends IndexedSeqStepper[Int, JInt, ShortIndexedSeqStepper](seq, from, until) with AllInIntStepper 
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = { val i = index; val res = seq(i); index = i + 1; res }
		override def clone = new ShortIndexedSeqStepper(seq, index, limit)
	}

	/** A ''boxing'' [[scala.collection.LongStepper LongStepper]] iterating over a slice of an `IndexedSeq[Long]`. */
	private class LongIndexedSeqStepper(seq :collection.IndexedSeq[Long], from :Int, until :Int)
		extends IndexedSeqStepper[Long, JLong, LongIndexedSeqStepper](seq, from, until) with AllInLongStepper
		   with Spliterator.OfLong with JLongIterator //compiler complains if it doesn't get it
	{
		override def nextLong() :Long = { val i = index; val res = seq(i); index = i + 1; res }
		override def clone = new LongIndexedSeqStepper(seq, index, limit)
	}

	/** A ''boxing'' [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `IndexedSeq[Double]`. */
	private class DoubleIndexedSeqStepper(seq :collection.IndexedSeq[Double], from :Int, until :Int)
		extends IndexedSeqStepper[Double, JDouble, DoubleIndexedSeqStepper](seq, from, until) with AllInDoubleStepper 
		   with Spliterator.OfDouble with JDoubleIterator //compiler complains if it doesn't get it
	{
		override def nextDouble() :Double = { val i = index; val res = seq(i); index = i + 1; res }
		override def clone = new DoubleIndexedSeqStepper(seq, index, limit)
	}

	/** A ''boxing'' [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `IndexedSeq[Float]`. */
	private class FloatIndexedSeqStepper(seq :collection.IndexedSeq[Float], from :Int, until :Int)
		extends IndexedSeqStepper[Double, JDouble, FloatIndexedSeqStepper](seq, from, until) with AllInDoubleStepper 
		   with Spliterator.OfDouble with JDoubleIterator //compiler complains if it doesn't get it
	{
		override def nextDouble() :Double = { val i = index; val res = seq(i); index = i + 1; res }
		override def clone = new FloatIndexedSeqStepper(seq, index, limit)
	}
}






/** A [[scala.collection.Stepper Stepper]] iterating over a slice of an array, which at the same time
  * is its [[java.util.Spliterator Spliterator]] and Java [[java.util.Iterator Iterator]] to avoid unnecessary
  * object creation and serve as a handy way to obtain a specialized primitive iterator dedicated to the array type.
  * Subclasses exist for all value types as well as one for reference types, in order to avoid boxing.
  * @param array    the array with elements over which to iterate.
  * @param first    the index of the first (and later current) element of the array (the one returned by `next()`).
  * @param `last++` the index immediately following the index of the last element in the slice.
  * @tparam A       the specialization type of the stepper (the type after widening the array elements, not
  *                 the array element type).
  * @tparam B       the Java class for type `A`: for reference types it is simply `A`; for primitive types
  *                 it is its Java boxing class.
  * @author Marcin Mościcki
  */
private abstract class ArrayStepper[+A, B, +Self >: Null <: ArrayStepper[A, B, Self]]
	                   (array :Array[_], private[this] var first :Int, private[this] var `last++` :Int)
	extends AbstractIndexedStepper[A, B, Self]
{
	protected final override def underlyingSize :Int = array.length
	protected final override def index :Int = first
	protected final override def index_=(i :Int) :Unit = first = i
	protected final override def limit :Int = `last++`
	protected final override def limit_=(i :Int) :Unit = `last++` = i

	override def hasStep :Boolean = first < `last++`
}


@SerialVersionUID(ver)
object ArrayStepper {
	/** A [[scala.collection.Stepper Stepper]] iterating over the entirety of an array.
	  * The stepper will be of a proper specialization to `A` - `IntStepper` for `Char`,
	  * `DoubleStepper` for `Float`, `AnyStepper` for reference type, etc., and will not box array elements
	  * if it is a primitive array.
	  * @param array the array with elements over which to iterate.
	  */
	def apply[A, S <: Stepper[_]](array :Array[A])(implicit shape :StepperShape[A, S]) :S with EfficientSplit =
		apply(array, 0, array.length)

	/** A [[scala.collection.Stepper Stepper]] iterating over a slice of an array.
	  * The stepper will be of a proper specialization to `A` - `IntStepper` for `Char`,
	  * `DoubleStepper` for `Float`, `AnyStepper` for reference type, etc., and will not box array elements
	  * if it is a primitive array.
	  * @param array the array with elements over which to iterate.
	  * @param from  the index of the first (and later current) element in the array (the one returned by `next()`).
	  * @param until the index immediately following the index of the last element in the slice.
	  */
	def apply[A, S <: Stepper[_]](array :Array[A], from :Int, until :Int)(implicit shape :StepperShape[A, S])
			:S with EfficientSplit =
		(shape.shape match {
			case IntShape    => new IntArrayStepper(array.castParam[Int], from, until)
			case LongShape   => new LongArrayStepper(array.castParam[Long], from, until)
			case DoubleShape => new DoubleArrayStepper(array.castParam[Double], from, until)
			case CharShape   => new CharArrayStepper(array.castParam[Char], from, until)
			case ByteShape   => new ByteArrayStepper(array.castParam[Byte], from, until)
			case FloatShape  => new FloatArrayStepper(array.castParam[Float], from, until)
			case ShortShape  => new ShortArrayStepper(array.castParam[Short], from, until)
			case _           => new AnyArrayStepper(array.castParam[Short], from, until)
		}).asInstanceOf[S with EfficientSplit]

	
	/** An [[scala.collection.AnyStepper AnyStepper]] iterating over a slice of an `Array[A]`. */
	private class AnyArrayStepper[A](array :Array[A], from :Int, until :Int)
		extends ArrayStepper[A, A, AnyArrayStepper[A]](array, from, until) with AllInAnyStepper[A]
	{
		override def next() :A = { val i = index; val res = array(i); index = i + 1; res }
		override def iterator :Iterator[A] = new ArrayIterator(array, index, limit)
		override def clone = new AnyArrayStepper(array, index, limit)
	}

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Int]`. */
	private class IntArrayStepper(array :Array[Int], from :Int, until :Int)
		extends ArrayStepper[Int, JInt, IntArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = { val i = index; val res = array(i); index = i + 1; res }
		override def clone = new IntArrayStepper(array, index, limit)
	}

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Char]`. */
	private class CharArrayStepper(array :Array[Char], from :Int, until :Int)
		extends ArrayStepper[Int, JInt, CharArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = { val i = index; val res = array(i); index = i + 1; res }
		override def clone = new CharArrayStepper(array, index, limit)
	}

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Byte]`. */
	private class ByteArrayStepper(array :Array[Byte], from :Int, until :Int)
		extends ArrayStepper[Int, JInt, ByteArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = { val i = index; val res = array(i); index = i + 1; res }
		override def clone = new ByteArrayStepper(array, index, limit)
	}

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Short]`. */
	private class ShortArrayStepper(array :Array[Short], from :Int, until :Int)
		extends ArrayStepper[Int, JInt, ShortArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = { val i = index; val res = array(i); index = i + 1; res }
		override def clone = new ShortArrayStepper(array, index, limit)
	}

	/** A [[scala.collection.LongStepper LongStepper]] iterating over a slice of an `Array[Long]`. */
	private class LongArrayStepper(array :Array[Long], from :Int, until :Int)
		extends ArrayStepper[Long, JLong, LongArrayStepper](array, from, until) with AllInLongStepper
		   with Spliterator.OfLong with JLongIterator //compiler complains if it doesn't get it
	{
		override def nextLong() :Long = { val i = index; val res = array(i); index = i + 1; res }
		override def clone = new LongArrayStepper(array, index, limit)
	}

	/** A [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `Array[Double]`. */
	private class DoubleArrayStepper(array :Array[Double], from :Int, until :Int)
		extends ArrayStepper[Double, JDouble, DoubleArrayStepper](array, from, until) with AllInDoubleStepper
		   with Spliterator.OfDouble with JDoubleIterator //compiler complains if it doesn't get it
	{
		override def nextDouble() :Double = { val i = index; val res = array(i); index = i + 1; res }
		override def clone = new DoubleArrayStepper(array, index, limit)
	}

	/** A [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `Array[Float]`. */
	private class FloatArrayStepper(array :Array[Float], from :Int, until :Int)
		extends ArrayStepper[Double, JDouble, FloatArrayStepper](array, from, until) with AllInDoubleStepper
		   with Spliterator.OfDouble with JDoubleIterator //compiler complains if it doesn't get it
	{
		override def nextDouble() :Double = { val i = index; val res = array(i); index = i + 1; res }
		override def clone = new FloatArrayStepper(array, index, limit)
	}
}




/** A [[scala.collection.Stepper Stepper]] iterating over a slice of an array in the reverse direction,
  * which at the same time is its [[java.util.Spliterator Spliterator]] and Java [[java.util.Iterator Iterator]]
  * to avoid unnecessary object creation and serve as a handy way to obtain a specialized primitive iterator
  * dedicated to the array type. Subclasses exist for all built in value types and `Any` to avoid boxing.
  * @param array     the array with elements over which to iterate.
  * @param last      the inclusive lower index of an array slice to iterate over, and the index of the last returned element.
  * @param `first++` the exclusive upper index of an array slice to iterate over, and the index following
  *                  the index of the first element returned by `next()`.
  * @tparam A        the specialization type of the stepper (the type after widening the array elements, not
  *                  the array element type)
  * @tparam B        the Java class for type `A`: for reference types it is simply `A`; for primitive types
  *                  it is its Java boxing class.
  * @author Marcin Mościcki
  */
private abstract class ReverseArrayStepper[+A, B, +Self >: Null <: ReverseArrayStepper[A, B, Self]]
	                   (array :Array[_], private[this] var last :Int, private[this] var `first++` :Int)
	extends AbstractIndexedReverseStepper[A, B, Self]
{
	protected final override def underlyingSize :Int = array.length
	protected final override def index :Int = `first++`
	protected final override def index_=(i :Int) :Unit = `first++` = i
	protected final override def limit :Int = last
	protected final override def limit_=(i :Int) :Unit = last = i

	override def hasStep :Boolean = `first++` > last
}
	

@SerialVersionUID(ver)
object ReverseArrayStepper {
	/** A [[scala.collection.Stepper Stepper]] iterating over the entirety of an array in the reverse order.
	  * The stepper will be of a proper specialization to `A` - `IntStepper` for `Char`,
	  * `DoubleStepper` for `Float`, `AnyStepper` for reference type, etc., and will not box array elements
	  * if it is a primitive array.
	  * @param array the array with elements over which to iterate.
	  */
	def apply[A, S <: Stepper[_]](array :Array[A])(implicit shape :StepperShape[A, S]) :S with EfficientSplit =
		apply(array, 0, array.length)

	/** A [[scala.collection.Stepper Stepper]] iterating over a slice of an array.
	  * The stepper will be of a proper specialization to `A` - `IntStepper` for `Char`,
	  * `DoubleStepper` for `Float`, `AnyStepper` for reference type, etc., and will not box array elements
	  * if it is a primitive array.
	  * @param array the array with elements over which to iterate.
	  * @param from  the inclusive lower index of an array slice to iterate over,
	  *              and the index of the last returned element.
	  * @param until the exclusive upper index of an array slice to iterate over,
	  *              and the index following the index of the first element returned by `next()`.
	  */
	def apply[A, S <: Stepper[_]](array :Array[A], from :Int, until :Int)(implicit shape :StepperShape[A, S])
			:S with EfficientSplit =
		(shape.shape match {
			case IntShape    => new ReverseIntArrayStepper(array.castParam[Int], from, until)
			case LongShape   => new ReverseLongArrayStepper(array.castParam[Long], from, until)
			case DoubleShape => new ReverseDoubleArrayStepper(array.castParam[Double], from, until)
			case CharShape   => new ReverseCharArrayStepper(array.castParam[Char], from, until)
			case ByteShape   => new ReverseByteArrayStepper(array.castParam[Byte], from, until)
			case FloatShape  => new ReverseFloatArrayStepper(array.castParam[Float], from, until)
			case ShortShape  => new ReverseShortArrayStepper(array.castParam[Short], from, until)
			case _           => new ReverseAnyArrayStepper(array.castParam[Short], from, until)
		}).asInstanceOf[S with EfficientSplit]

	
	/** An [[scala.collection.AnyStepper AnyStepper]] iterating over a slice of an `Array[A]`. */
	private class ReverseAnyArrayStepper[A](array :Array[A], from :Int, until :Int)
		extends ReverseArrayStepper[A, A, ReverseAnyArrayStepper[A]](array, from, until) with AllInAnyStepper[A]
	{
		override def next() :A = { val i = index; index = i - 1; array(i - 1) }
		override def iterator :Iterator[A] = new ReverseArrayIterator(array, limit, index)
		override def clone = new ReverseAnyArrayStepper(array, limit, index)
	}
	
	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Int]`. */
	private class ReverseIntArrayStepper(array :Array[Int], from :Int, until :Int)
		extends ReverseArrayStepper[Int, JInt, ReverseIntArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = { val i = index; index = i - 1; array(i - 1) }
		override def clone = new ReverseIntArrayStepper(array, limit, index)
	}
	
	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Char]`. */
	private class ReverseCharArrayStepper(array :Array[Char], from :Int, until :Int)
		extends ReverseArrayStepper[Int, JInt, ReverseCharArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = { val i = index; index = i - 1; array(i - 1) }
		override def clone = new ReverseCharArrayStepper(array, limit, index)
	}
	
	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Byte]`. */
	private class ReverseByteArrayStepper(array :Array[Byte], from :Int, until :Int)
		extends ReverseArrayStepper[Int, JInt, ReverseByteArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = { val i = index; index = i - 1; array(i - 1) }
		override def clone = new ReverseByteArrayStepper(array, limit, index)
	}
	
	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Short]`. */
	private class ReverseShortArrayStepper(array :Array[Short], from :Int, until :Int)
		extends ReverseArrayStepper[Int, JInt, ReverseShortArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = { val i = index; index = i - 1; array(i - 1) }
		override def clone = new ReverseShortArrayStepper(array, limit, index)
	}
	
	/** A [[scala.collection.LongStepper LongStepper]] iterating over a slice of an `Array[Long]`. */
	private class ReverseLongArrayStepper(array :Array[Long], from :Int, until :Int)
		extends ReverseArrayStepper[Long, JLong, ReverseLongArrayStepper](array, from, until) with AllInLongStepper
		   with Spliterator.OfLong with JLongIterator //compiler complains if it doesn't get it
	{
		override def nextLong() :Long = { val i = index; index = i - 1; array(i - 1) }
		override def clone = new ReverseLongArrayStepper(array, limit, index)
	}
	
	/** A [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `Array[Double]`. */
	private class ReverseDoubleArrayStepper(array :Array[Double], from :Int, until :Int)
		extends ReverseArrayStepper[Double, JDouble, ReverseDoubleArrayStepper](array, from, until)
		   with AllInDoubleStepper with Spliterator.OfDouble with JDoubleIterator
	{
		override def nextDouble() :Double = { val i = index; index = i - 1; array(i - 1) }
		override def clone = new ReverseDoubleArrayStepper(array, limit, index)
	}
	
	/** A [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `Array[Float]`. */
	private class ReverseFloatArrayStepper(array :Array[Float], from :Int, until :Int)
		extends ReverseArrayStepper[Double, JDouble, ReverseFloatArrayStepper](array, from, until)
		   with AllInDoubleStepper with Spliterator.OfDouble with JDoubleIterator
	{
		override def nextDouble() :Double = { val i = index; index = i - 1; array(i - 1) }
		override def clone = new ReverseFloatArrayStepper(array, limit, index)
	}
}	
