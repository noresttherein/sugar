package net.noresttherein.sugar.collections

import java.util.Spliterator.{IMMUTABLE, ORDERED, SIZED, SUBSIZED}
import java.util.Spliterator

import scala.collection.{Stepper, StepperShape}
import scala.collection.Stepper.EfficientSplit
import scala.collection.StepperShape.{ByteShape, CharShape, DoubleShape, FloatShape, IntShape, LongShape, ReferenceShape, ShortShape}

import net.noresttherein.sugar.JavaTypes.{JByte, JChar, JDouble, JDoubleIterator, JInt, JIntIterator, JIterator, JLong, JLongIterator}
import net.noresttherein.sugar.extensions.classNameMethods

//implicits
import net.noresttherein.sugar.extensions.castTypeParamMethods




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
private trait AbstractIndexedStepper[+A, B, +Self >: Null <: AbstractIndexedStepper[A, B, Self]]
	extends AllInStepper[A, B, Self] with EfficientSplit with Cloneable
{
	protected def underlyingSize :Int
	protected var index :Int
	protected var limit :Int
	protected def nextIdx() :Int = {
		val i = index
		if (i >= limit)
			throw new NoSuchElementException(toString)
		index = i + 1
		i
	}

	if (limit > underlyingSize)
		limit = underlyingSize
	if (index > limit)
		index = limit
	else if (index < 0)
		index = 0

	override def estimateSize :Long = limit - index max 0
	override def hasStep :Boolean = index < limit
	override def hasNext :Boolean = index < limit

	override def trySplit() :Self = {
		val from = index; val until = limit
		if (from + 1 >= until)
			null
		else {
			val split = ((until - from) >> 2) + from
			val res = clone
			res.limit = split
			index = split
			res
		}
	}

	override def characteristics :Int = ORDERED | SIZED | SUBSIZED
	override def clone :Self = super.clone.asInstanceOf[Self]

	override def toString :String = this.innerClassName + "(" + index + " until " + limit + ")"
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
private trait AbstractIndexedReverseStepper[+A, B, +Self >: Null <: AbstractIndexedReverseStepper[A, B, Self]]
	extends AllInStepper[A, B, Self] with EfficientSplit with Cloneable
{
	protected def underlyingSize :Int
	protected var index :Int
	protected var limit :Int
	protected def nextIdx() :Int = {
		val i = index
		if (i >= limit)
			throw new NoSuchElementException(toString)
		index = i - 1
		i
	}

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
			val res = clone
			res.limit = split
			index = split
			res
		}
	}

	override def characteristics :Int = ORDERED | SIZED | SUBSIZED
	override def clone :Self = super.clone.asInstanceOf[Self]
	override def toString :String = "Stepper(" + index + " down to " + limit + ")"
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
	protected final override def nextIdx() :Int = {
		val res = first
		if (res == `last++`)
			throw new NoSuchElementException(toString)
		first += 1
		res
	}
	
	override def hasStep :Boolean = first < `last++`

	override def characteristics :Int =
		if (seq.isInstanceOf[IndexedSeq[_]]) IMMUTABLE | super.characteristics
		else super.characteristics
}


@SerialVersionUID(Ver)
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
		override def next() :A = seq(nextIdx()) 
		override def iterator :Iterator[A] = new IndexedSeqIterator(seq, index, limit)
		override def clone = new AnyIndexedSeqStepper(seq, index, limit)
	}

	/** A ''boxing'' [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `IndexedSeq[Int]`. */
	private class IntIndexedSeqStepper(seq :collection.IndexedSeq[Int], from :Int, until :Int)
		extends IndexedSeqStepper[Int, JInt, IntIndexedSeqStepper](seq, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = seq(nextIdx())
		override def clone = new IntIndexedSeqStepper(seq, index, limit)
	}

	/** A ''boxing'' [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `IndexedSeq[Char]`. */
	private class CharIndexedSeqStepper(seq :collection.IndexedSeq[Char], from :Int, until :Int)
		extends IndexedSeqStepper[Int, JInt, CharIndexedSeqStepper](seq, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = seq(nextIdx())
		override def clone = new CharIndexedSeqStepper(seq, index, limit)
	}

	/** A ''boxing'' [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `IndexedSeq[Byte]`. */
	private class ByteIndexedSeqStepper(seq :collection.IndexedSeq[Byte], from :Int, until :Int)
		extends IndexedSeqStepper[Int, JInt, ByteIndexedSeqStepper](seq, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = seq(nextIdx())
		override def clone = new ByteIndexedSeqStepper(seq, index, limit)
	}

	/** A ''boxing'' [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `IndexedSeq[Short]`. */
	private class ShortIndexedSeqStepper(seq :collection.IndexedSeq[Short], from :Int, until :Int)
		extends IndexedSeqStepper[Int, JInt, ShortIndexedSeqStepper](seq, from, until) with AllInIntStepper 
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = seq(nextIdx())
		override def clone = new ShortIndexedSeqStepper(seq, index, limit)
	}

	/** A ''boxing'' [[scala.collection.LongStepper LongStepper]] iterating over a slice of an `IndexedSeq[Long]`. */
	private class LongIndexedSeqStepper(seq :collection.IndexedSeq[Long], from :Int, until :Int)
		extends IndexedSeqStepper[Long, JLong, LongIndexedSeqStepper](seq, from, until) with AllInLongStepper
		   with Spliterator.OfLong with JLongIterator //compiler complains if it doesn't get it
	{
		override def nextLong() :Long = seq(nextIdx())
		override def clone = new LongIndexedSeqStepper(seq, index, limit)
	}

	/** A ''boxing'' [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `IndexedSeq[Double]`. */
	private class DoubleIndexedSeqStepper(seq :collection.IndexedSeq[Double], from :Int, until :Int)
		extends IndexedSeqStepper[Double, JDouble, DoubleIndexedSeqStepper](seq, from, until) with AllInDoubleStepper 
		   with Spliterator.OfDouble with JDoubleIterator //compiler complains if it doesn't get it
	{
		override def nextDouble() :Double = seq(nextIdx())
		override def clone = new DoubleIndexedSeqStepper(seq, index, limit)
	}

	/** A ''boxing'' [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `IndexedSeq[Float]`. */
	private class FloatIndexedSeqStepper(seq :collection.IndexedSeq[Float], from :Int, until :Int)
		extends IndexedSeqStepper[Double, JDouble, FloatIndexedSeqStepper](seq, from, until) with AllInDoubleStepper 
		   with Spliterator.OfDouble with JDoubleIterator //compiler complains if it doesn't get it
	{
		override def nextDouble() :Double = seq(nextIdx())
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
	protected final override def nextIdx() :Int = {
		val res = first
		if (res >= `last++`)
			throw new NoSuchElementException(toString)
		first += 1
		res
	}

	override def hasStep :Boolean = first < `last++`
}


@SerialVersionUID(Ver)
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
			case IntShape    => (array :Array[_]) match {
				case ints :Array[Int] => new IntArrayStepper(ints, from, until)
				case _ => new IntRefArrayStepper(array, from, until)
			}
			case LongShape   => (array :Array[_]) match {
				case longs :Array[Long] => new LongArrayStepper(longs, from, until)
				case _ => new LongRefArrayStepper(array, from, until)
			}
			case DoubleShape => (array :Array[_]) match {
				case doubles :Array[Double] => new DoubleArrayStepper(doubles, from, until)
				case _ => new DoubleRefArrayStepper(array, from, until)
			}
			case CharShape   => (array :Array[_]) match {
				case chars :Array[Char] => new CharArrayStepper(chars, from, until)
				case _ => new CharRefArrayStepper(array, from, until)
			}
			case ByteShape   => (array :Array[_]) match {
				case bytes :Array[Byte] => new ByteArrayStepper(bytes, from, until)
				case _ => new ByteRefArrayStepper(array, from, until)
			}
			case FloatShape   => (array :Array[_]) match {
				case floats :Array[Float] => new FloatArrayStepper(floats, from, until)
				case _ => new FloatRefArrayStepper(array, from, until)
			}
			case ShortShape   => (array :Array[_]) match {
				case shorts :Array[Short] => new ShortArrayStepper(shorts, from, until)
				case _ => new ShortRefArrayStepper(array, from, until)
			}
			case _ => new AnyArrayStepper(array, from, until)
		}).asInstanceOf[S with EfficientSplit]

	
	/** An [[scala.collection.AnyStepper AnyStepper]] iterating over a slice of an `Array[A]`. */
	private class AnyArrayStepper[A](array :Array[A], from :Int, until :Int)
		extends ArrayStepper[A, A, AnyArrayStepper[A]](array, from, until) with AllInAnyStepper[A]
	{
		override def next() :A = array(nextIdx())
		override def iterator :Iterator[A] = new ArrayIterator(array, index, limit)
		override def clone = new AnyArrayStepper(array, index, limit)
	}

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Int]`. */
	private class IntArrayStepper(array :Array[Int], from :Int, until :Int)
		extends ArrayStepper[Int, JInt, IntArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = array(nextIdx())
		override def clone = new IntArrayStepper(array, index, limit)
	}

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Char]`. */
	private class CharArrayStepper(array :Array[Char], from :Int, until :Int)
		extends ArrayStepper[Int, JInt, CharArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = array(nextIdx())
		override def clone = new CharArrayStepper(array, index, limit)
	}

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Byte]`. */
	private class ByteArrayStepper(array :Array[Byte], from :Int, until :Int)
		extends ArrayStepper[Int, JInt, ByteArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = array(nextIdx())
		override def clone = new ByteArrayStepper(array, index, limit)
	}

	private class ShortArrayStepper(array :Array[Short], from :Int, until :Int)
		extends ArrayStepper[Int, JInt, ShortArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = array(nextIdx())
		override def clone = new ShortArrayStepper(array, index, limit)
	}

	/** A [[scala.collection.LongStepper LongStepper]] iterating over a slice of an `Array[Long]`. */
	private class LongArrayStepper(array :Array[Long], from :Int, until :Int)
		extends ArrayStepper[Long, JLong, LongArrayStepper](array, from, until) with AllInLongStepper
		   with Spliterator.OfLong with JLongIterator //compiler complains if it doesn't get it
	{
		override def nextLong() :Long = array(nextIdx())
		override def clone = new LongArrayStepper(array, index, limit)
	}

	/** A [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `Array[Double]`. */
	private class DoubleArrayStepper(array :Array[Double], from :Int, until :Int)
		extends ArrayStepper[Double, JDouble, DoubleArrayStepper](array, from, until) with AllInDoubleStepper
		   with Spliterator.OfDouble with JDoubleIterator //compiler complains if it doesn't get it
	{
		override def nextDouble() :Double = array(nextIdx())
		override def clone = new DoubleArrayStepper(array, index, limit)
	}

	/** A [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `Array[Float]`. */
	private class FloatArrayStepper(array :Array[Float], from :Int, until :Int)
		extends ArrayStepper[Double, JDouble, FloatArrayStepper](array, from, until) with AllInDoubleStepper
		   with Spliterator.OfDouble with JDoubleIterator //compiler complains if it doesn't get it
	{
		override def nextDouble() :Double = array(nextIdx())
		override def clone = new FloatArrayStepper(array, index, limit)
	}



	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Any]` containing `Integer`s. */
	private class IntRefArrayStepper(array :Array[_], from :Int, until :Int)
		extends ArrayStepper[Int, JInt, IntRefArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def next() :JInt = array(nextIdx()).asInstanceOf[JInt]
		override def nextInt() :Int = array(nextIdx()).asInstanceOf[Int]
		override def clone = new IntRefArrayStepper(array, index, limit)
	}

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Any]` containing `Character`s. */
	private class CharRefArrayStepper(array :Array[_], from :Int, until :Int)
		extends ArrayStepper[Int, JInt, CharRefArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def next() :JInt = array(nextIdx()).asInstanceOf[Char].toInt
		override def nextInt() :Int = array(nextIdx()).asInstanceOf[Char].toInt
		override def clone = new CharRefArrayStepper(array, index, limit)
	}

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Any]` containing `Byte`s. */
	private class ByteRefArrayStepper(array :Array[_], from :Int, until :Int)
		extends ArrayStepper[Int, JInt, ByteRefArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def next() :JInt = array(nextIdx()).asInstanceOf[Byte].toInt
		override def nextInt() :Int = array(nextIdx()).asInstanceOf[Byte].toInt
		override def clone = new ByteRefArrayStepper(array, index, limit)
	}

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Any]` containing `Short`s. */
	private class ShortRefArrayStepper(array :Array[_], from :Int, until :Int)
		extends ArrayStepper[Int, JInt, ShortRefArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def next() :JInt = array(nextIdx()).asInstanceOf[Short].toInt
		override def nextInt() :Int = array(nextIdx()).asInstanceOf[Short].toInt
		override def clone = new ShortRefArrayStepper(array, index, limit)
	}

	/** A [[scala.collection.LongStepper LongStepper]] iterating over a slice of an `Array[Any]` containing `Long`s. */
	private class LongRefArrayStepper(array :Array[_], from :Int, until :Int)
		extends ArrayStepper[Long, JLong, LongRefArrayStepper](array, from, until) with AllInLongStepper
		   with Spliterator.OfLong with JLongIterator //compiler complains if it doesn't get it
	{
		override def next() :JLong = array(nextIdx()).asInstanceOf[JLong]
		override def nextLong() :Long = array(nextIdx()).asInstanceOf[Long]
		override def clone = new LongRefArrayStepper(array, index, limit)
	}

	/** A [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `Array[Any]` containing `Double`s. */
	private class DoubleRefArrayStepper(array :Array[_], from :Int, until :Int)
		extends ArrayStepper[Double, JDouble, DoubleRefArrayStepper](array, from, until) with AllInDoubleStepper
		   with Spliterator.OfDouble with JDoubleIterator //compiler complains if it doesn't get it
	{
		override def next() :JDouble = array(nextIdx()).asInstanceOf[JDouble]
		override def nextDouble() :Double = array(nextIdx()).asInstanceOf[Double]
		override def clone = new DoubleRefArrayStepper(array, index, limit)
	}

	/** A [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `Array[Any]` containing `Float`s. */
	private class FloatRefArrayStepper(array :Array[_], from :Int, until :Int)
		extends ArrayStepper[Double, JDouble, FloatRefArrayStepper](array, from, until) with AllInDoubleStepper
		   with Spliterator.OfDouble with JDoubleIterator //compiler complains if it doesn't get it
	{
		override def next() :JDouble = array(nextIdx()).asInstanceOf[JDouble]
		override def nextDouble() :Double = array(nextIdx()).asInstanceOf[Double]
		override def clone = new FloatRefArrayStepper(array, index, limit)
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
	protected final override def nextIdx() :Int = {
		`first++` -= 1
		if (`first++` <= last)
			throw new NoSuchElementException(toString)
		`first++`
	}

	override def hasStep :Boolean = `first++` > last
}
	

@SerialVersionUID(Ver)
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
			case IntShape    => (array :Array[_]) match {
				case ints :Array[Int] => new ReverseIntArrayStepper(ints, from, until)
				case _ => new ReverseIntRefArrayStepper(array, from, until)
			}
			case LongShape   => (array :Array[_]) match {
				case longs :Array[Long] => new ReverseLongArrayStepper(longs, from, until)
				case _ => new ReverseLongRefArrayStepper(array, from, until)
			}
			case DoubleShape => (array :Array[_]) match {
				case doubles :Array[Double] => new ReverseDoubleArrayStepper(doubles, from, until)
				case _ => new ReverseDoubleRefArrayStepper(array, from, until)
			}
			case CharShape   => (array :Array[_]) match {
				case chars :Array[Char] => new ReverseCharArrayStepper(chars, from, until)
				case _ => new ReverseCharRefArrayStepper(array, from, until)
			}
			case ByteShape   => (array :Array[_]) match {
				case bytes :Array[Byte] => new ReverseByteArrayStepper(bytes, from, until)
				case _ => new ReverseByteRefArrayStepper(array, from, until)
			}
			case FloatShape  => (array :Array[_]) match {
				case floats :Array[Float] => new ReverseFloatArrayStepper(floats, from, until)
				case _ => new ReverseFloatRefArrayStepper(array, from, until)
			}
			case ShortShape  => (array :Array[_]) match {
				case shorts :Array[Short] => new ReverseShortArrayStepper(shorts, from, until)
				case _ => new ReverseShortRefArrayStepper(array, from, until)
			}
			case _           => new ReverseAnyArrayStepper(array, from, until)
		}).asInstanceOf[S with EfficientSplit]

	
	/** An [[scala.collection.AnyStepper AnyStepper]] iterating over a slice of an `Array[A]`. */
	private class ReverseAnyArrayStepper[A](array :Array[A], from :Int, until :Int)
		extends ReverseArrayStepper[A, A, ReverseAnyArrayStepper[A]](array, from, until) with AllInAnyStepper[A]
	{
		override def next() :A = array(nextIdx())
		override def iterator :Iterator[A] = new ReverseArrayIterator(array, limit, index)
		override def clone = new ReverseAnyArrayStepper(array, limit, index)
	}
	
	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Int]`. */
	private class ReverseIntArrayStepper(array :Array[Int], from :Int, until :Int)
		extends ReverseArrayStepper[Int, JInt, ReverseIntArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = array(nextIdx())
		override def clone = new ReverseIntArrayStepper(array, limit, index)
	}
	
	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Char]`. */
	private class ReverseCharArrayStepper(array :Array[Char], from :Int, until :Int)
		extends ReverseArrayStepper[Int, JInt, ReverseCharArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = array(nextIdx())
		override def clone = new ReverseCharArrayStepper(array, limit, index)
	}
	
	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Byte]`. */
	private class ReverseByteArrayStepper(array :Array[Byte], from :Int, until :Int)
		extends ReverseArrayStepper[Int, JInt, ReverseByteArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = array(nextIdx())
		override def clone = new ReverseByteArrayStepper(array, limit, index)
	}
	
	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Short]`. */
	private class ReverseShortArrayStepper(array :Array[Short], from :Int, until :Int)
		extends ReverseArrayStepper[Int, JInt, ReverseShortArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = array(nextIdx())
		override def clone = new ReverseShortArrayStepper(array, limit, index)
	}
	
	/** A [[scala.collection.LongStepper LongStepper]] iterating over a slice of an `Array[Long]`. */
	private class ReverseLongArrayStepper(array :Array[Long], from :Int, until :Int)
		extends ReverseArrayStepper[Long, JLong, ReverseLongArrayStepper](array, from, until) with AllInLongStepper
		   with Spliterator.OfLong with JLongIterator //compiler complains if it doesn't get it
	{
		override def nextLong() :Long = array(nextIdx())
		override def clone = new ReverseLongArrayStepper(array, limit, index)
	}
	
	/** A [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `Array[Double]`. */
	private class ReverseDoubleArrayStepper(array :Array[Double], from :Int, until :Int)
		extends ReverseArrayStepper[Double, JDouble, ReverseDoubleArrayStepper](array, from, until)
		   with AllInDoubleStepper with Spliterator.OfDouble with JDoubleIterator
	{
		override def nextDouble() :Double = array(nextIdx())
		override def clone = new ReverseDoubleArrayStepper(array, limit, index)
	}
	
	/** A [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `Array[Float]`. */
	private class ReverseFloatArrayStepper(array :Array[Float], from :Int, until :Int)
		extends ReverseArrayStepper[Double, JDouble, ReverseFloatArrayStepper](array, from, until)
		   with AllInDoubleStepper with Spliterator.OfDouble with JDoubleIterator
	{
		override def nextDouble() :Double = array(nextIdx())
		override def clone = new ReverseFloatArrayStepper(array, limit, index)
	}



	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[_]` containing `Int`s. */
	private class ReverseIntRefArrayStepper(array :Array[_], from :Int, until :Int)
		extends ReverseArrayStepper[Int, JInt, ReverseIntRefArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = array(nextIdx()).asInstanceOf[Int]
		override def clone = new ReverseIntRefArrayStepper(array, limit, index)
	}

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[_]` containing `Char`s. */
	private class ReverseCharRefArrayStepper(array :Array[_], from :Int, until :Int)
		extends ReverseArrayStepper[Int, JInt, ReverseCharRefArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = array(nextIdx()).asInstanceOf[Char].toInt
		override def clone = new ReverseCharRefArrayStepper(array, limit, index)
	}

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[_]` containing `Byte`s. */
	private class ReverseByteRefArrayStepper(array :Array[_], from :Int, until :Int)
		extends ReverseArrayStepper[Int, JInt, ReverseByteRefArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = array(nextIdx()).asInstanceOf[Byte]
		override def clone = new ReverseByteRefArrayStepper(array, limit, index)
	}

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[_]` containing `Short`s. */
	private class ReverseShortRefArrayStepper(array :Array[_], from :Int, until :Int)
		extends ReverseArrayStepper[Int, JInt, ReverseShortRefArrayStepper](array, from, until) with AllInIntStepper
		   with Spliterator.OfInt with JIntIterator //compiler complains if it doesn't get it
	{
		override def nextInt() :Int = array(nextIdx()).asInstanceOf[Short]
		override def clone = new ReverseShortRefArrayStepper(array, limit, index)
	}

	/** A [[scala.collection.LongStepper LongStepper]] iterating over a slice of an `Array[_]` containing `Long`s. */
	private class ReverseLongRefArrayStepper(array :Array[_], from :Int, until :Int)
		extends ReverseArrayStepper[Long, JLong, ReverseLongRefArrayStepper](array, from, until) with AllInLongStepper
		   with Spliterator.OfLong with JLongIterator //compiler complains if it doesn't get it
	{
		override def nextLong() :Long = array(nextIdx()).asInstanceOf[Long]
		override def clone = new ReverseLongRefArrayStepper(array, limit, index)
	}

	/** A [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `Array[_]` containing `Double`s. */
	private class ReverseDoubleRefArrayStepper(array :Array[_], from :Int, until :Int)
		extends ReverseArrayStepper[Double, JDouble, ReverseDoubleRefArrayStepper](array, from, until)
		   with AllInDoubleStepper with Spliterator.OfDouble with JDoubleIterator
	{
		override def nextDouble() :Double = array(nextIdx()).asInstanceOf[Double]
		override def clone = new ReverseDoubleRefArrayStepper(array, limit, index)
	}

	/** A [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `Array[_]` containing `Float`s. */
	private class ReverseFloatRefArrayStepper(array :Array[_], from :Int, until :Int)
		extends ReverseArrayStepper[Double, JDouble, ReverseFloatRefArrayStepper](array, from, until)
		   with AllInDoubleStepper with Spliterator.OfDouble with JDoubleIterator
	{
		override def nextDouble() :Double = array(nextIdx()).asInstanceOf[Float]
		override def clone = new ReverseFloatRefArrayStepper(array, limit, index)
	}
}
