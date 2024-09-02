package net.noresttherein.sugar.collections

import java.util.Spliterator.{IMMUTABLE, NONNULL, ORDERED, SIZED, SUBSIZED}
import java.util.Spliterator
import java.util.function.Consumer

import scala.collection.{Stepper, StepperShape}
import scala.collection.Stepper.EfficientSplit
import scala.collection.StepperShape.{ByteShape, CharShape, DoubleShape, FloatShape, IntShape, LongShape, ReferenceShape, ShortShape}

import net.noresttherein.sugar.JavaTypes.{JDouble, JDoubleIterator, JInt, JIntIterator, JLong, JLongIterator, JStringBuilder}
import net.noresttherein.sugar.arrays.{ArrayIterator, ReverseArrayIterator}
import net.noresttherein.sugar.exceptions.{illegal_!, noSuch_!}
import net.noresttherein.sugar.extensions.castingMethods
import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.reflect.extensions.classNameMethods




/** Base trait for implementations of steppers over slices of some sequential collections.
  * Aside from [[scala.collection.Stepper Stepper]], the trait implements also
  * [[java.util.Spliterator Spliterator]] and Java [[java.util.Iterator Iterator]].
  * Concrete implementations are expected to implement also the specialized versions of the three interfaces.
  * The stepper advances over a window on the collection; it is assumed to use random indexing
  * to return the elements, but they are never handled by this class itself.
  * @see [[net.noresttherein.sugar.collections.IndexedIterator]]
  * @see [[net.noresttherein.sugar.collections.ReverseIndexedStepper]]
  * @author Marcin Mościcki
  */
private trait IndexedStepper[+A, B, +Self >: Null <: IndexedStepper[A, B, Self]]
	extends AllInOneStepper[A, B, Self] with EfficientSplit with Cloneable
{
	protected def underlyingSize :Int
	protected var index :Int
	protected var limit :Int
	override def estimateSize :Long = limit - index max 0

	if (limit > underlyingSize)
		limit = underlyingSize
	if (index > limit)
		index = limit
	else if (index < 0)
		index = 0

	protected def nextIdx() :Int = {
		val i = index
		if (i >= limit)
			noSuch_!(toString)
		index = i + 1
		i
	}
	override def hasStep :Boolean = index < limit
	override def hasNext :Boolean = index < limit

	override def forEachRemaining(action :Consumer[_ >: B]) :Unit = {
		var remaining = limit - index
		while (remaining > 0) {
			action.accept(next())
			remaining -= 1
		}
	}

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


private abstract class AbstractIndexedStepper[+A, B, +Self >: Null <: IndexedStepper[A, B, Self]]
	                   (private[this] var first :Int, private[this] var `last++` :Int)
	extends IndexedStepper[A, B, Self]
{
	protected final override def index :Int = first
	protected final override def index_=(i :Int) :Unit = first = i
	protected final override def limit :Int = `last++`
	protected final override def limit_=(i :Int) :Unit = `last++` = i
	@inline protected final override def nextIdx() :Int = {
		val res = first
		if (res == `last++`)
			noSuch_!(toString)
		first += 1
		res
	}
	override def hasStep :Boolean = first < `last++`
}




/** Base trait for implementations of steppers traversing in the reverse order over slices
  * of some sequential collections. The iterator advances over a window on the collection; it is assumed to use
  * random indexing to return the elements, but they are never handled by this class itself.
  * Aside from [[scala.collection.Stepper Stepper]], the trait implements also
  * [[java.util.Spliterator Spliterator]] and Java [[java.util.Iterator Iterator]].
  * Concrete implementations are expected to implement also the specialized versions of the three interfaces.
  * @see [[net.noresttherein.sugar.collections.ReverseIndexedIterator]]
  * @see [[net.noresttherein.sugar.collections.IndexedStepper]]
  * @author Marcin Mościcki
  */
private trait ReverseIndexedStepper[+A, B, +Self >: Null <: ReverseIndexedStepper[A, B, Self]]
	extends AllInOneStepper[A, B, Self] with EfficientSplit with Cloneable
{
	protected def underlyingSize :Int
	protected var index :Int
	protected var limit :Int
	protected def nextIdx() :Int = {
		val i = index
		if (i >= limit)
			noSuch_!(toString)
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


private abstract class AbstractReverseIndexedStepper[+A, B, +Self >: Null <: ReverseIndexedStepper[A, B, Self]]
	                   (private[this] var last :Int, private[this] var `first++` :Int)
	extends ReverseIndexedStepper[A, B, Self]
{
	protected final override def index :Int = `first++`
	protected final override def index_=(i :Int) :Unit = `first++` = i
	protected final override def limit :Int = last
	protected final override def limit_=(i :Int) :Unit = last = i
	protected final override def nextIdx() :Int = {
		`first++` -= 1
		if (`first++` <= last)
			noSuch_!(toString)
		`first++`
	}
	override def hasStep :Boolean = `first++` > last
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
	                   (seq :collection.IndexedSeqOps[_, generic.Any1, _], first :Int, `last++` :Int)
	extends AbstractIndexedStepper[A, B, Self](first, `last++`)
{
	protected final override def underlyingSize :Int = seq.length

	override def characteristics :Int =
		if (seq.isInstanceOf[IndexedSeq[_]]) IMMUTABLE | super.characteristics
		else super.characteristics
}


@SerialVersionUID(Ver)
object IndexedSeqStepper {
	private type Ops[A] = collection.IndexedSeqOps[A, generic.Any1, _]

	/** A [[scala.collection.Stepper Stepper]] iterating over the entirety of an indexed sequence.
	  * The stepper will box the elements, but the result will be of the proper specialization for `A` -
	  * one of `Any`, `Int`, `Long`, `Double`.
	  * @param seq      the sequence with elements over which to iterate.
	  */ //we could make it work for collection.IndexedSeq
	def apply[A, S <: Stepper[_]](seq :collection.IndexedSeqOps[A, generic.Any1, _])
	                             (implicit shape :StepperShape[A, S]) :S with EfficientSplit =
		slice(seq, 0, seq.length)

	/** A [[scala.collection.Stepper Stepper]] iterating over a slice of an indexed sequence.
	  * The stepper will box the elements, but the result will be of the proper specialization for `A` -
	  * one of `Any`, `Int`, `Long`, `Double`.
	  * @param seq      the sequence with elements over which to iterate.
	  * @param from     the index of the first (and later current) element in the sequence (the one returned by `next()`).
	  * @param sizee    the maximum number of elements returned by the stepper.
	  */
//	def apply[A, S <: Stepper[_]](seq :collection.IndexedSeqOps[A, generic.Any1, _], from :Int, size :Int)
//	                             (implicit shape :StepperShape[A, S]) :S with EfficientSplit =
//		slice(seq, from, from + math.min(Int.MaxValue - from, size))

	/** A [[scala.collection.Stepper Stepper]] iterating over a slice of an indexed sequence.
	  * The stepper will box the elements, but the result will be of the proper specialization for `A` -
	  * one of `Any`, `Int`, `Long`, `Double`.
	  * @param seq      the sequence with elements over which to iterate.
	  * @param from     the index of the first (and later current) element in the sequence (the one returned by `next()`).
	  * @param until    the index immediately following the index of the last element in the slice.
	  */
	def slice[A, S <: Stepper[_]](seq :collection.IndexedSeqOps[A, generic.Any1, _], from :Int, until :Int)
	                             (implicit shape :StepperShape[A, S]) :S with EfficientSplit =
		(shape.shape match {
			case IntShape    => new IntIndexedSeqStepper(seq.castFrom[Ops[A], Ops[Int]], from, until)
			case LongShape   => new LongIndexedSeqStepper(seq.castFrom[Ops[A], Ops[Long]], from, until)
			case DoubleShape => new DoubleIndexedSeqStepper(seq.castFrom[Ops[A], Ops[Double]], from, until)
			case CharShape   => new CharIndexedSeqStepper(seq.castFrom[Ops[A], Ops[Char]], from, until)
			case ByteShape   => new ByteIndexedSeqStepper(seq.castFrom[Ops[A], Ops[Byte]], from, until)
			case FloatShape  => new FloatIndexedSeqStepper(seq.castFrom[Ops[A], Ops[Float]], from, until)
			case ShortShape  => new ShortIndexedSeqStepper(seq.castFrom[Ops[A], Ops[Short]], from, until)
			case _           => new AnyIndexedSeqStepper(seq.castFrom[Ops[A], Ops[Short]], from, until)
		}).asInstanceOf[S with EfficientSplit]


	private abstract class OfBox[+A, B, +S >: Null <: IndexedSeqStepper[A, B, S]](seq :Ops[A], from :Int, until :Int)
		extends IndexedSeqStepper[A, B, S](seq, from, until)
	{
		override def nextStep() :A = seq(nextIdx())
		override def iterator :Iterator[A] = new IndexedSeqIterator(seq, index, limit)
	}

	/** An [[scala.collection.AnyStepper AnyStepper]] iterating over a slice of an `IndexedSeq[A]`. */
	private class AnyIndexedSeqStepper[A](seq :Ops[A], from :Int, until :Int)
		extends OfBox[A, A, AnyIndexedSeqStepper[A]](seq, from, until) with AllInOneAnyStepper[A]

	/** A ''boxing'' [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `IndexedSeq[Int]`. */
	private class IntIndexedSeqStepper(seq :Ops[Int], from :Int, until :Int)
		extends OfBox[Int, JInt, IntIndexedSeqStepper](seq, from, until) with BoxedAllInOneIntStepper
		   with Spliterator.OfInt with JavaIntIterator //compiler complains if it doesn't get it

	/** A ''boxing'' [[scala.collection.LongStepper LongStepper]] iterating over a slice of an `IndexedSeq[Long]`. */
	private class LongIndexedSeqStepper(seq :Ops[Long], from :Int, until :Int)
		extends OfBox[Long, JLong, LongIndexedSeqStepper](seq, from, until) with BoxedAllInOneLongStepper
		   with Spliterator.OfLong with JavaLongIterator //compiler complains if it doesn't get it

	/** A ''boxing'' [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `IndexedSeq[Double]`. */
	private class DoubleIndexedSeqStepper(seq :Ops[Double], from :Int, until :Int)
		extends OfBox[Double, JDouble, DoubleIndexedSeqStepper](seq, from, until) with BoxedAllInOneDoubleStepper
			with Spliterator.OfDouble with JavaDoubleIterator //compiler complains if it doesn't get it

	/** A ''boxing'' [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `IndexedSeq[Char]`. */
	private class CharIndexedSeqStepper(seq :Ops[Char], from :Int, until :Int)
		extends IndexedSeqStepper[Int, JInt, CharIndexedSeqStepper](seq, from, until) with AllInOneIntStepper
		   with Spliterator.OfInt with JavaIntIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Int = seq(nextIdx())
	}

	/** A ''boxing'' [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `IndexedSeq[Byte]`. */
	private class ByteIndexedSeqStepper(seq :Ops[Byte], from :Int, until :Int)
		extends IndexedSeqStepper[Int, JInt, ByteIndexedSeqStepper](seq, from, until) with AllInOneIntStepper
		   with Spliterator.OfInt with JavaIntIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Int = seq(nextIdx())
	}

	/** A ''boxing'' [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `IndexedSeq[Short]`. */
	private class ShortIndexedSeqStepper(seq :Ops[Short], from :Int, until :Int)
		extends IndexedSeqStepper[Int, JInt, ShortIndexedSeqStepper](seq, from, until) with AllInOneIntStepper
		   with Spliterator.OfInt with JavaIntIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Int = seq(nextIdx())
	}

	/** A ''boxing'' [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `IndexedSeq[Float]`. */
	private class FloatIndexedSeqStepper(seq :Ops[Float], from :Int, until :Int)
		extends IndexedSeqStepper[Double, JDouble, FloatIndexedSeqStepper](seq, from, until) with AllInOneDoubleStepper
		   with Spliterator.OfDouble with JavaDoubleIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Double = seq(nextIdx())
	}
}






/** A [[scala.collection.Stepper Stepper]] iterating over a slice of an array, which at the same time
  * is its [[java.util.Spliterator Spliterator]] and Java [[java.util.Iterator Iterator]] to avoid unnecessary
  * object creation and serve as a handy way to obtain a specialized primitive iterator dedicated to the array type.
  * Its main raison d'etre is, however, that there is no public Scala API allowing to create a stepper
  * over a slice of an array.
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
	extends AbstractIndexedStepper[A, B, Self](first, `last++`)
{ //consider: adding immutability info for IMMUTABLE characteristic
	protected final override def underlyingSize :Int = array.length
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
		slice(array, 0, array.length)

	/** A [[scala.collection.Stepper Stepper]] iterating over a slice of an array.
	  * The stepper will be of a proper specialization to `A` - `IntStepper` for `Char`,
	  * `DoubleStepper` for `Float`, `AnyStepper` for reference type, etc., and will not box array elements
	  * if it is a primitive array. The advantage over `ArrayOps.stepper` is serving as its own
	  * `Spliterator` and Java iterator (the corresponding methods return `this`), and being more robust
	  * with the array type: no error is produced even if `StepperShape` calls for a specialized stepper,
	  * but the array is a reference array.
	  * @param array the array with elements over which to iterate.
	  * @param from  the index of the first (and later current) element in the array (the one returned by `next()`).
	  * @param size  an upper bound on the number of returned elements.
	  */
	def apply[A, S <: Stepper[_]](array :Array[A], from :Int, size :Int = Int.MaxValue)
	                             (implicit shape :StepperShape[A, S]) :S with EfficientSplit =
	{
		val length = array.length
		if (from <= 0) slice(array, 0, size)
		else if (from >= length) slice(array, length, length)
		else slice(array, from, from + math.min(length - from, size))
	}

	/** A [[scala.collection.Stepper Stepper]] iterating over a slice of an array.
	  * The stepper will be of a proper specialization to `A` - `IntStepper` for `Char`,
	  * `DoubleStepper` for `Float`, `AnyStepper` for reference type, etc., and will not box array elements
	  * if it is a primitive array. The advantage over `ArrayOps.stepper` is serving as its own
	  * `Spliterator` and Java iterator (the corresponding methods return `this`), and being more robust
	  * with the array type: no error is produced even if `StepperShape` calls for a specialized stepper,
	  * but the array is a reference array.
	  * @param array the array with elements over which to iterate.
	  * @param from  the index of the first (and later current) element in the array (the one returned by `next()`).
	  * @param until the index immediately following the index of the last element in the slice.
	  */
	def slice[A, S <: Stepper[_]](array :Array[A], from :Int, until :Int)(implicit shape :StepperShape[A, S])
			:S with EfficientSplit =
		(shape.shape match {
			case ReferenceShape => (array :Array[_]) match {
				case refs :Array[AnyRef] => new AnyRefArrayStepper(refs, from, until)
				case _                   => new AnyArrayStepper(array, from, until)
			}
			case IntShape       => (array :Array[_]) match {
				case ints :Array[Int]    => new IntArrayStepper(ints, from, until)
				case refs :Array[AnyRef] => new IntRefArrayStepper(refs, from, until)
				case _                   =>
					illegal_!(
						"Expected either Array[Int] or an Array of Integers for IntShape, got " + array.className + "."
					)
			}
			case LongShape      => (array :Array[_]) match {
				case longs :Array[Long]   => new LongArrayStepper(longs, from, until)
				case refs  :Array[AnyRef] => new LongRefArrayStepper(refs, from, until)
				case _                    =>
					illegal_!(
						"Expected either Array[Long] or an Array of boxed Longs for LongShape, got " + array.className + "."
					)
			}
			case DoubleShape    => (array :Array[_]) match {
				case doubles :Array[Double] => new DoubleArrayStepper(doubles, from, until)
				case refs    :Array[AnyRef] => new DoubleRefArrayStepper(refs, from, until)
				case _                      =>
					illegal_!(
						"Expected either Array[Double] or an Array of boxed Doubles for DoubleShape, got " + array.className + "."
					)
			}
			case CharShape      => (array :Array[_]) match {
				case chars :Array[Char]   => new CharArrayStepper(chars, from, until)
				case refs  :Array[AnyRef] => new CharRefArrayStepper(refs, from, until)
				case _                    =>
					illegal_!(
						"Expected either Array[Char] or an Array of Characters for CharShape, got " + array.className + "."
					)
			}
			case ByteShape      => (array :Array[_]) match {
				case bytes :Array[Byte]   => new ByteArrayStepper(bytes, from, until)
				case refs  :Array[AnyRef] => new ByteRefArrayStepper(refs, from, until)
				case _                    =>
					illegal_!(
						"Expected either Array[Byte] or an Array of boxed Bytes for ByteShape, got " + array.className + "."
					)
			}
			case FloatShape     => (array :Array[_]) match {
				case floats :Array[Float]  => new FloatArrayStepper(floats, from, until)
				case refs   :Array[AnyRef] => new FloatRefArrayStepper(refs, from, until)
				case _                     =>
					illegal_!(
						"Expected either Array[Float] or an Array of boxed Floats for FloatShape, got " + array.className + "."
					)
			}
			case ShortShape     => (array :Array[_]) match {
				case shorts :Array[Short]  => new ShortArrayStepper(shorts, from, until)
				case refs   :Array[AnyRef] => new ShortRefArrayStepper(refs, from, until)
				case _                     =>
					illegal_!(
						"Expected either Array[Short] or an Array of boxed Shorts for ShortShape, got " + array.className + "."
					)
			}
			case _              => new AnyArrayStepper(array, from, until)
		}).asInstanceOf[S with EfficientSplit]


	/** An [[scala.collection.AnyStepper AnyStepper]] iterating over a slice of an `Array[A]`. */
	private final class AnyArrayStepper[A](array :Array[A], from :Int, until :Int)
		extends ArrayStepper[A, A, AnyArrayStepper[A]](array, from, until) with AllInOneAnyStepper[A]
	{
		override def nextStep() :A = array(nextIdx())
		override def iterator :Iterator[A] = new ArrayIterator(array, index, limit, false)
	}

	private abstract class RefArrayStepper[A, B, +S >: Null <: ArrayStepper[A, B, S]]
	                                      (array :Array[AnyRef], from :Int, until :Int)
		extends ArrayStepper[A, B, S](array, from, until)
	{
		override def nextStep() :A = array(nextIdx()).asInstanceOf[A]
		override def iterator :Iterator[A] = new ArrayIterator(array, index, limit, false).asInstanceOf[Iterator[A]]
	}

	/** An [[scala.collection.AnyStepper AnyStepper]] iterating over a slice of an array of a reference type. */
	private class AnyRefArrayStepper[A <: AnyRef](array :Array[AnyRef], from :Int, until :Int)
		extends RefArrayStepper[A, A, AnyRefArrayStepper[A]](array, from, until) with AllInOneAnyStepper[A]

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Int]`. */
	private final class IntArrayStepper(array :Array[Int], from :Int, until :Int)
		extends ArrayStepper[Int, JInt, IntArrayStepper](array, from, until) with AllInOneIntStepper
		   with Spliterator.OfInt with JavaIntIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Int = array(nextIdx())
	}

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Char]`. */
	private final class CharArrayStepper(array :Array[Char], from :Int, until :Int)
		extends ArrayStepper[Int, JInt, CharArrayStepper](array, from, until) with AllInOneIntStepper
		   with Spliterator.OfInt with JavaIntIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Int = array(nextIdx())
	}

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Byte]`. */
	private final class ByteArrayStepper(array :Array[Byte], from :Int, until :Int)
		extends ArrayStepper[Int, JInt, ByteArrayStepper](array, from, until) with AllInOneIntStepper
		   with Spliterator.OfInt with JavaIntIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Int = array(nextIdx())
	}

	private final class ShortArrayStepper(array :Array[Short], from :Int, until :Int)
		extends ArrayStepper[Int, JInt, ShortArrayStepper](array, from, until) with AllInOneIntStepper
		   with Spliterator.OfInt with JavaIntIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Int = array(nextIdx())
	}

	/** A [[scala.collection.LongStepper LongStepper]] iterating over a slice of an `Array[Long]`. */
	private final class LongArrayStepper(array :Array[Long], from :Int, until :Int)
		extends ArrayStepper[Long, JLong, LongArrayStepper](array, from, until) with AllInOneLongStepper
		   with Spliterator.OfLong with JavaLongIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Long = array(nextIdx())
	}

	/** A [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `Array[Double]`. */
	private final class DoubleArrayStepper(array :Array[Double], from :Int, until :Int)
		extends ArrayStepper[Double, JDouble, DoubleArrayStepper](array, from, until) with AllInOneDoubleStepper
		   with Spliterator.OfDouble with JavaDoubleIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Double = array(nextIdx())
	}

	/** A [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `Array[Float]`. */
	private final class FloatArrayStepper(array :Array[Float], from :Int, until :Int)
		extends ArrayStepper[Double, JDouble, FloatArrayStepper](array, from, until) with AllInOneDoubleStepper
		   with Spliterator.OfDouble with JavaDoubleIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Double = array(nextIdx())
	}


	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Any]` containing `Integer`s. */
	private final class IntRefArrayStepper(array :Array[AnyRef], from :Int, until :Int)
		extends RefArrayStepper[Int, JInt, IntRefArrayStepper](array, from, until)
		   with BoxedAllInOneIntStepper with Spliterator.OfInt with JavaIntIterator //compiler complains if it doesn't get it

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Any]` containing `Character`s. */
	private final class CharRefArrayStepper(array :Array[AnyRef], from :Int, until :Int)
		extends ArrayStepper[Int, JInt, CharRefArrayStepper](array, from, until) with AllInOneIntStepper
		   with Spliterator.OfInt with JavaIntIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Int = array(nextIdx()).asInstanceOf[Char].toInt
	}

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Any]` containing `Byte`s. */
	private final class ByteRefArrayStepper(array :Array[AnyRef], from :Int, until :Int)
		extends ArrayStepper[Int, JInt, ByteRefArrayStepper](array, from, until) with AllInOneIntStepper
		   with Spliterator.OfInt with JavaIntIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Int = array(nextIdx()).asInstanceOf[Byte].toInt
	}

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Any]` containing `Short`s. */
	private final class ShortRefArrayStepper(array :Array[AnyRef], from :Int, until :Int)
		extends ArrayStepper[Int, JInt, ShortRefArrayStepper](array, from, until) with AllInOneIntStepper
		   with Spliterator.OfInt with JavaIntIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Int = array(nextIdx()).asInstanceOf[Short].toInt
	}

	/** A [[scala.collection.LongStepper LongStepper]] iterating over a slice of an `Array[Any]` containing `Long`s. */
	private final class LongRefArrayStepper(array :Array[AnyRef], from :Int, until :Int)
		extends ArrayStepper[Long, JLong, LongRefArrayStepper](array, from, until) with AllInOneLongStepper
		   with Spliterator.OfLong with JavaLongIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Long = array(nextIdx()).asInstanceOf[Long]
	}

	/** A [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `Array[Any]` containing `Double`s. */
	private final class DoubleRefArrayStepper(array :Array[AnyRef], from :Int, until :Int)
		extends ArrayStepper[Double, JDouble, DoubleRefArrayStepper](array, from, until) with AllInOneDoubleStepper
		   with Spliterator.OfDouble with JavaDoubleIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Double = array(nextIdx()).asInstanceOf[Double]
	}

	/** A [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `Array[Any]` containing `Float`s. */
	private final class FloatRefArrayStepper(array :Array[AnyRef], from :Int, until :Int)
		extends ArrayStepper[Double, JDouble, FloatRefArrayStepper](array, from, until) with AllInOneDoubleStepper
		   with Spliterator.OfDouble with JavaDoubleIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Double = array(nextIdx()).asInstanceOf[Double]
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
	extends AbstractReverseIndexedStepper[A, B, Self](last, `first++`)
{
	protected final override def underlyingSize :Int = array.length
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
			case ReferenceShape => (array :Array[_]) match {
				case refs :Array[AnyRef] => new ReverseAnyRefArrayStepper(refs, from, until)
				case _                   => new ReverseAnyArrayStepper(array, from, until)
			}
			case IntShape       => (array :Array[_]) match {
				case ints :Array[Int]    => new ReverseIntArrayStepper(ints, from, until)
				case refs :Array[AnyRef] => new ReverseIntRefArrayStepper(refs, from, until)
				case _                   =>
					illegal_!(
						"Expected either Array[Int] or an Array of Integers for IntShape, got " + array.className + "."
					)
			}
			case LongShape      => (array :Array[_]) match {
				case longs :Array[Long]   => new ReverseLongArrayStepper(longs, from, until)
				case refs  :Array[AnyRef] => new ReverseLongRefArrayStepper(refs, from, until)
				case _ =>
					illegal_!(
						"Expected either Array[Long] or an Array of boxed Longs for LongShape, got " + array.className + "."
					)
			}
			case DoubleShape    => (array :Array[_]) match {
				case doubles :Array[Double] => new ReverseDoubleArrayStepper(doubles, from, until)
				case refs    :Array[AnyRef] => new ReverseDoubleRefArrayStepper(refs, from, until)
				case _                      =>
					illegal_!(
						"Expected either Array[Double] or an Array of boxed Doubles for DoubleShape, got " + array.className + "."
					)
			}
			case CharShape      => (array :Array[_]) match {
				case chars :Array[Char]   => new ReverseCharArrayStepper(chars, from, until)
				case refs  :Array[AnyRef] => new ReverseCharRefArrayStepper(refs, from, until)
				case _                    =>
					illegal_!(
						"Expected either Array[Char] or an Array of Characters for CharShape, got " + array.className + "."
					)
			}
			case ByteShape      => (array :Array[_]) match {
				case bytes :Array[Byte]   => new ReverseByteArrayStepper(bytes, from, until)
				case refs  :Array[AnyRef] => new ReverseByteRefArrayStepper(refs, from, until)
				case _                    =>
					illegal_!(
						"Expected either Array[Byte] or an Array of boxed Bytes for ByteShape, got " + array.className + "."
					)
			}
			case FloatShape     => (array :Array[_]) match {
				case floats :Array[Float]  => new ReverseFloatArrayStepper(floats, from, until)
				case refs   :Array[AnyRef] => new ReverseFloatRefArrayStepper(refs, from, until)
				case _                     =>
					illegal_!(
						"Expected either Array[Float] or an Array of boxed Floats for FloatShape, got " + array.className + "."
					)
			}
			case ShortShape     => (array :Array[_]) match {
				case shorts :Array[Short]  => new ReverseShortArrayStepper(shorts, from, until)
				case refs   :Array[AnyRef] => new ReverseShortRefArrayStepper(refs, from, until)
				case _                     =>
					illegal_!(
						"Expected either Array[Short] or an Array of boxed Shorts for ShortShape, got " + array.className + "."
					)
			}
			case _           => new ReverseAnyArrayStepper(array, from, until)
		}).asInstanceOf[S with EfficientSplit]


	private abstract class ReverseRefArrayStepper[A, B, S >: Null <: ReverseArrayStepper[A, B, S]]
	                                             (array :Array[AnyRef], from :Int, until :Int)
		extends ReverseArrayStepper[A, B, S](array, from, until)
	{
		override def nextStep() :A = array(nextIdx()).asInstanceOf[A]
		override def iterator :Iterator[A] = new ReverseArrayIterator(array, limit, index).asInstanceOf[Iterator[A]]
	}

	/** An [[scala.collection.AnyStepper AnyStepper]] iterating over a slice of an `Array[A]`. */
	private class ReverseAnyArrayStepper[A](array :Array[A], from :Int, until :Int)
		extends ReverseArrayStepper[A, A, ReverseAnyArrayStepper[A]](array, from, until) with AllInOneAnyStepper[A]
	{
		override def nextStep() :A = array(nextIdx())
		override def iterator :Iterator[A] = new ReverseArrayIterator(array, limit, index)
	}

	private class ReverseAnyRefArrayStepper[A <: AnyRef](array :Array[AnyRef], from :Int, until :Int)
		extends ReverseRefArrayStepper[A, A, ReverseAnyRefArrayStepper[A]](array, from, until) with AllInOneAnyStepper[A]
	
	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Int]`. */
	private class ReverseIntArrayStepper(array :Array[Int], from :Int, until :Int)
		extends ReverseArrayStepper[Int, JInt, ReverseIntArrayStepper](array, from, until) with AllInOneIntStepper
		   with Spliterator.OfInt with JavaIntIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Int = array(nextIdx())
	}
	
	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Char]`. */
	private class ReverseCharArrayStepper(array :Array[Char], from :Int, until :Int)
		extends ReverseArrayStepper[Int, JInt, ReverseCharArrayStepper](array, from, until) with AllInOneIntStepper
		   with Spliterator.OfInt with JavaIntIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Int = array(nextIdx())
	}
	
	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Byte]`. */
	private class ReverseByteArrayStepper(array :Array[Byte], from :Int, until :Int)
		extends ReverseArrayStepper[Int, JInt, ReverseByteArrayStepper](array, from, until) with AllInOneIntStepper
		   with Spliterator.OfInt with JavaIntIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Int = array(nextIdx())
	}
	
	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[Short]`. */
	private class ReverseShortArrayStepper(array :Array[Short], from :Int, until :Int)
		extends ReverseArrayStepper[Int, JInt, ReverseShortArrayStepper](array, from, until) with AllInOneIntStepper
		   with Spliterator.OfInt with JavaIntIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Int = array(nextIdx())
	}
	
	/** A [[scala.collection.LongStepper LongStepper]] iterating over a slice of an `Array[Long]`. */
	private class ReverseLongArrayStepper(array :Array[Long], from :Int, until :Int)
		extends ReverseArrayStepper[Long, JLong, ReverseLongArrayStepper](array, from, until) with AllInOneLongStepper
		   with Spliterator.OfLong with JavaLongIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Long = array(nextIdx())
	}
	
	/** A [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `Array[Double]`. */
	private class ReverseDoubleArrayStepper(array :Array[Double], from :Int, until :Int)
		extends ReverseArrayStepper[Double, JDouble, ReverseDoubleArrayStepper](array, from, until)
		   with AllInOneDoubleStepper with Spliterator.OfDouble with JavaDoubleIterator
	{
		override def nextStep() :Double = array(nextIdx())
	}
	
	/** A [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `Array[Float]`. */
	private class ReverseFloatArrayStepper(array :Array[Float], from :Int, until :Int)
		extends ReverseArrayStepper[Double, JDouble, ReverseFloatArrayStepper](array, from, until)
		   with AllInOneDoubleStepper with Spliterator.OfDouble with JavaDoubleIterator
	{
		override def nextStep() :Double = array(nextIdx())
	}


	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[_]` containing `Int`s. */
	private class ReverseIntRefArrayStepper(array :Array[AnyRef], from :Int, until :Int)
		extends ReverseRefArrayStepper[Int, JInt, ReverseIntRefArrayStepper](array, from, until)
		   with BoxedAllInOneIntStepper with Spliterator.OfInt with JavaIntIterator //compiler complains if it doesn't get it

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[_]` containing `Char`s. */
	private class ReverseCharRefArrayStepper(array :Array[AnyRef], from :Int, until :Int)
		extends ReverseArrayStepper[Int, JInt, ReverseCharRefArrayStepper](array, from, until) with AllInOneIntStepper
		   with Spliterator.OfInt with JavaIntIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Int = array(nextIdx()).asInstanceOf[Char].toInt
	}

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[_]` containing `Byte`s. */
	private class ReverseByteRefArrayStepper(array :Array[AnyRef], from :Int, until :Int)
		extends ReverseArrayStepper[Int, JInt, ReverseByteRefArrayStepper](array, from, until) with AllInOneIntStepper
		   with Spliterator.OfInt with JavaIntIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Int = array(nextIdx()).asInstanceOf[Byte]
	}

	/** An [[scala.collection.IntStepper IntStepper]] iterating over a slice of an `Array[_]` containing `Short`s. */
	private class ReverseShortRefArrayStepper(array :Array[AnyRef], from :Int, until :Int)
		extends ReverseArrayStepper[Int, JInt, ReverseShortRefArrayStepper](array, from, until) with AllInOneIntStepper
		   with Spliterator.OfInt with JavaIntIterator //compiler complains if it doesn't get it
	{
		override def nextStep() :Int = array(nextIdx()).asInstanceOf[Short]
	}

	/** A [[scala.collection.LongStepper LongStepper]] iterating over a slice of an `Array[_]` containing `Long`s. */
	private class ReverseLongRefArrayStepper(array :Array[AnyRef], from :Int, until :Int)
		extends ReverseRefArrayStepper[Long, JLong, ReverseLongRefArrayStepper](array, from, until)
		   with BoxedAllInOneLongStepper with Spliterator.OfLong with JavaLongIterator //compiler complains if it doesn't get it

	/** A [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `Array[_]` containing `Double`s. */
	private class ReverseDoubleRefArrayStepper(array :Array[AnyRef], from :Int, until :Int)
		extends ReverseRefArrayStepper[Double, JDouble, ReverseDoubleRefArrayStepper](array, from, until)
		   with BoxedAllInOneDoubleStepper with Spliterator.OfDouble with JavaDoubleIterator

	/** A [[scala.collection.DoubleStepper DoubleStepper]] iterating over a slice of an `Array[_]` containing `Float`s. */
	private class ReverseFloatRefArrayStepper(array :Array[AnyRef], from :Int, until :Int)
		extends ReverseArrayStepper[Double, JDouble, ReverseFloatRefArrayStepper](array, from, until)
		   with AllInOneDoubleStepper with Spliterator.OfDouble with JavaDoubleIterator
	{
		override def nextStep() :Double = array(nextIdx()).asInstanceOf[Float]
	}
}




private class StringStepper(text :String, start :Int, end :Int)
	extends AbstractIndexedStepper[Int, JInt, StringStepper](start, end)
	   with AllInOneIntStepper with Spliterator.OfInt
{
	def this(text :String) = this(text, 0, text.length)
	override def underlyingSize :Int = text.length
	override def characteristics :Int = IMMUTABLE | NONNULL | ORDERED | SIZED
	override def nextStep() = text.charAt(nextIdx())
	override def toString = "StringStepper(" + index + " until " + limit + ")"
}


private class JStringBuilderStepper(text :JStringBuilder, start :Int, end :Int)
	extends AbstractIndexedStepper[Int, JInt, JStringBuilderStepper](start, end)
	   with AllInOneIntStepper with Spliterator.OfInt
{
	def this(text :JStringBuilder) = this(text, 0, text.length)
	override def underlyingSize :Int = text.length
	override def characteristics :Int = NONNULL | ORDERED | SIZED
	override def nextStep() = text.charAt(nextIdx())
	override def toString = "StringBuilderStepper(" + index + " until " + limit + ")"
}
