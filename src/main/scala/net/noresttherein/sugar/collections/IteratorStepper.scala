package net.noresttherein.sugar.collections

import java.util.Spliterator
import java.util.Spliterator.{DISTINCT, IMMUTABLE, ORDERED, SIZED}

import scala.annotation.tailrec
import scala.collection.StepperShape.{ByteShape, CharShape, DoubleShape, FloatShape, IntShape, LongShape, ReferenceShape, ShortShape}
import scala.collection.{DoubleStepper, IndexedSeqView, IntStepper, LongStepper, SeqView, Stepper, StepperShape, View, immutable}

import net.noresttherein.sugar.JavaTypes.{JDouble, JInt, JLong}
import net.noresttherein.sugar.collections.IteratorStepper.OfAny
import net.noresttherein.sugar.extensions.{castTypeParamMethods, classNameMethods}
import net.noresttherein.sugar.funny.generic




/** Base trait for steppers wrapping an arbitrary Scala `Iterator[E]`, where `E` is a type for which the elements
  * returned by a stepper created based on `StepperShape` is `A`.
  * @author Marcin Mościcki
  */
private abstract class IteratorStepper[+A, B, +Self >: Null <: AllInOneStepper[A, B, Self]]
                       (underlying :Iterator[_], flags :Int)
	extends AllInOneStepper[A, B, Self]
{
	override def characteristics :Int = flags
	override def hasStep :Boolean = underlying.hasNext
	override def estimateSize :Long = {
		val size = underlying.knownSize
		if (size >= 0) size else Long.MaxValue
	}
	override def toString :String = "Stepper@" + this.shortHashString + "(" + underlying + ")"
}


private object IteratorStepper {

	/** Wraps `elems.iterator` in a `Stepper`. Essentially the same as `IterableOnceOps.stepper`, but additionally:
	  *   - provides special instances for collections of not more than two elements,
	  *   - handles specialized `ValIterator`,
	  *   - handles `collection.IndexedSeqOps` individually,
	  *   - sets the stepper's `characteristics` based on the type of the collection.
	  */
	def apply[A, S <: Stepper[_]](elems :IterableOnce[A])(implicit shape :StepperShape[A, S]) :S =
		elems match {
			case view   :View[A] => view match {
				case seq :IndexedSeqView[A] => seq.length match {
					case 0 => Stepper0()
					case 1 => Stepper1(view.head)
					case 2 => Stepper2(view.head, view.last)
					case _ => IndexedSeqStepper(seq)
				}
				case seq :SeqView[A] => IteratorStepper(seq.iterator, ORDERED)
				case _ => IteratorStepper(view.iterator, 0)
			}
			case small :Iterable[A] if small.sizeIs <= 2 => small.size match {
				case 0 => Stepper0()
				case 1 => Stepper1(small.head)
				case _ => Stepper2(small.head, small.last)
			}
			case seq   :collection.IndexedSeqOps[A, _, _] =>
				IndexedSeqStepper(seq)
			case _ =>
				var characteristics :Int = if (elems.knownSize >= 0) SIZED else 0
				if (elems.isInstanceOf[immutable.Iterable[_]])
					characteristics |= IMMUTABLE
				if (elems.isInstanceOf[collection.Set[_]])
					characteristics |= DISTINCT
				if (elems.isInstanceOf[collection.SeqOps[_, generic.Any, _]])
					characteristics |= ORDERED
				//We could also infer SORTED for a SortedSet, but we'd need to pass a Comparator to the stepper.
				// The problem is that we need a Comparator of the stepper element type, not input iterator type.
//				if (elems.isInstanceOf[collection.SortedSet[_]])
//					characteristics |= SORTED
				IteratorStepper(elems.iterator, characteristics)
		}

	private def apply[A, S <: Stepper[_]](elems :Iterator[A], characteristics :Int)
	                                     (implicit shape :StepperShape[A, S]) :S =
		elems match {
			case empty    :Iterator[A] if !empty.hasNext => Stepper0()
			case iterator :ValIterator[A]                => ValIteratorStepper(iterator, characteristics)
			case iterator :Iterator[A]                   =>
				(shape.shape match {
					case ReferenceShape => new OfAny(iterator, characteristics)
					case IntShape       => new FromInt(iterator.asInstanceOf[Iterator[Int]], characteristics)
					case LongShape      => new FromLong(iterator.asInstanceOf[Iterator[Long]], characteristics)
					case DoubleShape    => new FromDouble(iterator.asInstanceOf[Iterator[Double]], characteristics)
					case ByteShape      => new FromByte(iterator.asInstanceOf[Iterator[Byte]], characteristics)
					case CharShape      => new FromChar(iterator.asInstanceOf[Iterator[Char]], characteristics)
					case ShortShape     => new FromShort(iterator.asInstanceOf[Iterator[Short]], characteristics)
					case FloatShape     => new FromFloat(iterator.asInstanceOf[Iterator[Float]], characteristics)
					case _              => new OfAny(iterator, characteristics)
				}).asInstanceOf[S]
		}

	
	class OfAny[A](underlying :Iterator[A], characteristics :Int)
		extends IteratorStepper[A, A, OfAny[A]](underlying, characteristics) with AllInOneAnyStepper[A]
	{
		override def iterator :Iterator[A] = underlying
		override def nextStep() :A = underlying.next()
	}
	
	private abstract class OfBox[A, B, +Self >: Null <: AllInOneStepper[A, B, Self]]
	                            (underlying :Iterator[A], characteristics :Int)
		extends IteratorStepper[A, B, Self](underlying, characteristics)
	{
		override def iterator :Iterator[A] = underlying
		override def nextStep() :A = underlying.next()
	}
	private class FromInt(underlying :Iterator[Int], characteristics :Int)
		extends OfBox[Int, JInt, FromInt](underlying, characteristics)
		   with BoxedAllInOneIntStepper with Spliterator.OfInt
	
	private class FromLong(underlying :Iterator[Long], characteristics :Int)
		extends OfBox[Long, JLong, FromLong](underlying, characteristics)
		   with BoxedAllInOneLongStepper with Spliterator.OfLong
	
	private class FromDouble(underlying :Iterator[Double], characteristics :Int)
		extends OfBox[Double, JDouble, FromDouble](underlying, characteristics)
		   with BoxedAllInOneDoubleStepper with Spliterator.OfDouble
		
	private class FromFloat(underlying :Iterator[Float], characteristics :Int)
		extends IteratorStepper[Double, JDouble, FromFloat](underlying, characteristics)
		   with AllInOneDoubleStepper with Spliterator.OfDouble
	{
		override def nextStep() :Double = underlying.next().toDouble
	}

	private class FromByte(underlying :Iterator[Byte], characteristics :Int)
		extends IteratorStepper[Int, JInt, FromByte](underlying, characteristics)
		   with AllInOneIntStepper with Spliterator.OfInt
	{
		override def nextStep() :Int = underlying.next().toInt
	}

	private class FromChar(underlying :Iterator[Char], characteristics :Int)
		extends IteratorStepper[Int, JInt, FromChar](underlying, characteristics)
		   with AllInOneIntStepper with Spliterator.OfInt
	{
		override def nextStep() :Int = underlying.next().toInt
	}

	private class FromShort(underlying :Iterator[Short], characteristics :Int)
		extends IteratorStepper[Int, JInt, FromShort](underlying, characteristics)
		   with AllInOneIntStepper with Spliterator.OfInt
	{
		override def nextStep() :Int = underlying.next().toInt
	}
}




private object ValIteratorStepper {
	def apply[A, S <: Stepper[_]](elems :ValIterator[A], characteristics :Int)(implicit shape :StepperShape[A, S]) :S =
		if (!elems.hasNext)
			Stepper0()
		else
			(shape.shape match {
				//Can't use elems.stepper because of infinite recursion.
				case ReferenceShape => new OfAny(elems, characteristics)
				case IntShape       => new FromInt(elems.asInstanceOf[ValIterator[Int]], characteristics)
				case LongShape      => new FromLong(elems.asInstanceOf[ValIterator[Long]], characteristics)
				case DoubleShape    => new FromDouble(elems.asInstanceOf[ValIterator[Double]], characteristics)
				case ByteShape      => new FromByte(elems.asInstanceOf[ValIterator[Byte]], characteristics)
				case CharShape      => new FromChar(elems.asInstanceOf[ValIterator[Char]], characteristics)
				case ShortShape     => new FromShort(elems.asInstanceOf[ValIterator[Short]], characteristics)
				case FloatShape     => new FromFloat(elems.asInstanceOf[ValIterator[Float]], characteristics)
				case _              => new OfAny(elems, characteristics)
			}).asInstanceOf[S]

	private abstract class OfBox[@specialized(ElemTypes) A, B, +Self >: Null <: AllInOneStepper[A, B, Self]]
	                            (underlying :ValIterator[A], characteristics :Int)
		extends IteratorStepper[A, B, Self](underlying, characteristics)
	{
		override def iterator   :Iterator[A] = underlying
		override def nextStep() :A = underlying.next()
	}

	private class FromInt(underlying :ValIterator[Int], characteristics :Int)
		extends OfBox[Int, JInt, FromInt](underlying, characteristics) with AllInOneIntStepper with Spliterator.OfInt

	private class FromLong(underlying :ValIterator[Long], characteristics :Int)
		extends OfBox[Long, JLong, FromLong](underlying, characteristics)
		   with AllInOneLongStepper with Spliterator.OfLong

	private class FromDouble(underlying :ValIterator[Double], characteristics :Int)
		extends OfBox[Double, JDouble, FromDouble](underlying, characteristics)
		   with AllInOneDoubleStepper with Spliterator.OfDouble

	private class FromFloat(underlying :ValIterator[Float], characteristics :Int)
		extends IteratorStepper[Double, JDouble, FromFloat](underlying, characteristics)
		   with AllInOneDoubleStepper with Spliterator.OfDouble
	{
		override def nextStep() :Double = underlying.next().toDouble
	}

	private class FromByte(underlying :ValIterator[Byte], characteristics :Int)
		extends IteratorStepper[Int, JInt, FromByte](underlying, characteristics)
		   with AllInOneIntStepper with Spliterator.OfInt
	{
		override def nextStep() :Int = underlying.next().toInt
	}

	private class FromChar(underlying :ValIterator[Char], characteristics :Int)
		extends IteratorStepper[Int, JInt, FromChar](underlying, characteristics)
		   with AllInOneIntStepper with Spliterator.OfInt
	{
		override def nextStep() :Int = underlying.next().toInt
	}

	private class FromShort(underlying :ValIterator[Short], characteristics :Int)
		extends IteratorStepper[Int, JInt, FromShort](underlying, characteristics)
		   with AllInOneIntStepper with Spliterator.OfInt
	{
		override def nextStep() :Int = underlying.next().toInt
	}

}





private abstract class JavaIteratorStepper[+A, B, +Self >: Null <: AllInOneStepper[A, B, Self]]
                                          (underlying :JavaIterator[B], flags :Int)
	extends AllInOneStepper[A, B, Self]
{
	override def characteristics :Int = flags
	override def hasStep :Boolean = underlying.hasNext
	override def estimateSize :Long = if (!underlying.hasNext) 0 else Long.MaxValue
	override def toString :String = "Stepper@" + this.shortHashString + "(" + underlying + ")"
}


private object JavaIteratorStepper {
	def apply[E, I](iterator :I)(implicit shape :JavaIteratorShape[E, I]) :shape.Stepper = shape.shape match {
		case IntShape    => ofInt(iterator.asInstanceOf[JavaIntIterator]).asInstanceOf[shape.Stepper]
		case LongShape   => ofLong(iterator.asInstanceOf[JavaLongIterator]).asInstanceOf[shape.Stepper]
		case DoubleShape => ofDouble(iterator.asInstanceOf[JavaDoubleIterator]).asInstanceOf[shape.Stepper]
		case _           => ofAny(iterator.asInstanceOf[JavaIterator[E]]).asInstanceOf[shape.Stepper]
	}

	def ofAny[E](iterator :JavaIterator[E]) :Stepper[E] =
		if (iterator.hasNext) new OfAny(iterator, 0) else Stepper0.ofAny

	def ofInt(iterator :JavaIntIterator) :IntStepper =
		if (iterator.hasNext) new OfInt(iterator, 0) else Stepper0.ofInt

	def ofLong(iterator :JavaLongIterator) :LongStepper =
		if (iterator.hasNext) new OfLong(iterator, 0) else Stepper0.ofLong

	def ofDouble(iterator :JavaDoubleIterator) :DoubleStepper =
		if (iterator.hasNext) new OfDouble(iterator, 0) else Stepper0.ofDouble


	private class OfAny[A](underlying :JavaIterator[A], characteristics :Int)
		extends JavaIteratorStepper[A, A, OfAny[A]](underlying, characteristics) with AllInOneAnyStepper[A]
	{
		override def javaIterator[U >: A] :JavaIterator[U] = underlying.castParam[U]
		override def nextStep() :A = underlying.next()
	}

	private abstract class OfBox[A, B, +Self >: Null <: AllInOneStepper[A, B, Self], I <: JavaIterator[B]]
	                            (underlying :I, characteristics :Int)
		extends JavaIteratorStepper[A, B, Self](underlying, characteristics)
	{
		override def javaIterator[U >: A] :I = underlying
	}
	private class OfInt(underlying :JavaIntIterator, characteristics :Int)
		extends OfBox[Int, JInt, OfInt, JavaIntIterator](underlying, characteristics)
			with BoxedAllInOneIntStepper with Spliterator.OfInt
	{
		override def nextStep() :Int = underlying.nextInt()
	}

	private class OfLong(underlying :JavaLongIterator, characteristics :Int)
		extends OfBox[Long, JLong, OfLong, JavaLongIterator](underlying, characteristics)
			with BoxedAllInOneLongStepper with Spliterator.OfLong
	{
		override def nextStep() :Long = underlying.nextLong()
	}

	private class OfDouble(underlying :JavaDoubleIterator, characteristics :Int)
		extends OfBox[Double, JDouble, OfDouble, JavaDoubleIterator](underlying, characteristics)
			with BoxedAllInOneDoubleStepper with Spliterator.OfDouble
	{
		override def nextStep() :Double = underlying.nextDouble()
	}

}
