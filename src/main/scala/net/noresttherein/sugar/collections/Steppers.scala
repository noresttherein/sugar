package net.noresttherein.sugar.collections

import java.util.{PrimitiveIterator, Spliterator}
import java.util.Spliterator.{IMMUTABLE, NONNULL, ORDERED, SIZED}
import java.util.function.{Consumer, DoubleConsumer, IntConsumer, LongConsumer}

import scala.collection.{AnyStepper, DoubleStepper, IntStepper, LongStepper, Stepper, StepperShape}
import scala.collection.Stepper.EfficientSplit
import scala.collection.StepperShape.{ByteShape, CharShape, DoubleShape, FloatShape, IntShape, LongShape, ShortShape}

import net.noresttherein.sugar.JavaTypes.{JDouble, JInt, JIterator, JLong}




/** An empty `Stepper`.
  * @tparam A the element type of the Java `Spliterator` - for value types, these are Java box types.
  */
private sealed abstract class Stepper0[A]
	extends Stepper[Nothing] with EfficientSplit with Spliterator[A] with JIterator[A]
{
	override def hasStep = false
	override def hasNext = false
	override def nextStep() = throw new NoSuchElementException("Empty Stepper")
	override def next() = nextStep()
	override def tryAdvance(action :Consumer[_ >: A]) :Boolean = false
	override def trySplit() :Null = null
	override def estimateSize :Long = 0
	override def characteristics :Int = IMMUTABLE | NONNULL | ORDERED | SIZED
	override def forEachRemaining(action :Consumer[_ >: A]) :Unit = ()

	override def iterator :Iterator[Nothing] = Iterator.empty
	override def toString = "Stepper()"
}

private trait PrimitiveStepper0[A, S <: Spliterator[A], I <: JIterator[A]] extends Stepper0[A] { this :S with I =>
	override def next() = nextStep()
	override def forEachRemaining(action :Consumer[_ >: A]) :Unit = ()
	override def spliterator[B] = this
	override def javaIterator[B] = this
}


/** Empty `Stepper`s. */
@SerialVersionUID(ver)
object Stepper0 {
	def apply[A, S <: Stepper[_]]()(implicit shape :StepperShape[A, S]) :S with EfficientSplit =
		shape.shape match {
			case IntShape | CharShape | ShortShape | ByteShape => OfInt.asInstanceOf[S with EfficientSplit]
			case LongShape => ofLong.asInstanceOf[S with EfficientSplit]
			case DoubleShape | FloatShape => ofDouble.asInstanceOf[S with EfficientSplit]
			case _ => ofAny.asInstanceOf[S with EfficientSplit]
		}
	final val ofAny    :AnyStepper[Nothing] with EfficientSplit = OfAny
	final val ofInt    :IntStepper with EfficientSplit = OfInt
	final val ofLong   :LongStepper with EfficientSplit = OfLong
	final val ofDouble :DoubleStepper with EfficientSplit = OfDouble

	@SerialVersionUID(ver)
	private object OfAny extends Stepper0[Nothing] with AnyStepper[Nothing] {
		override def spliterator[U] = this.asInstanceOf[Spliterator[U]]
		override def javaIterator[U] = this.asInstanceOf[JIterator[U]]
	}

	@SerialVersionUID(ver)
	private object OfInt
		extends Stepper0[JInt] with IntStepper with Spliterator.OfInt with PrimitiveIterator.OfInt
		   with PrimitiveStepper0[JInt, Spliterator.OfInt, PrimitiveIterator.OfInt]
	{
		override def nextInt() = nextStep()
		override def tryAdvance(action :IntConsumer) :Boolean = false
		override def forEachRemaining(action :IntConsumer) :Unit = ()
	}

	@SerialVersionUID(ver)
	private object OfLong
		extends Stepper0[JLong] with LongStepper with Spliterator.OfLong with PrimitiveIterator.OfLong
		   with PrimitiveStepper0[JLong, Spliterator.OfLong, PrimitiveIterator.OfLong]
	{
		override def nextLong() = nextStep()
		override def tryAdvance(action :LongConsumer) :Boolean = false
		override def forEachRemaining(action :LongConsumer) :Unit = ()
	}

	@SerialVersionUID(ver)
	private object OfDouble
		extends Stepper0[JDouble] with DoubleStepper with Spliterator.OfDouble with PrimitiveIterator.OfDouble
		   with PrimitiveStepper0[JDouble, Spliterator.OfDouble, PrimitiveIterator.OfDouble]
	{
		override def nextDouble() = nextStep()
		override def tryAdvance(action :DoubleConsumer) :Boolean = false
		override def forEachRemaining(action :DoubleConsumer) :Unit = ()
	}
}




/** A `Stepper` of a single element.
  * @tparam A the nominal (Scala) element type of this `Stepper`.
  * @tparam B the type of the Java `Spliterator` - for value types, this is the Java box class of `A`.
  */
private sealed abstract class Stepper1[A, B](override val characteristics :Int = Stepper1.NonNullCharacteristics)
	extends Stepper[A] with EfficientSplit with Spliterator[B] with JIterator[B]
{
	var hasStep :Boolean = false
	override def hasNext :Boolean = hasStep
	override def next() :B = nextStep().asInstanceOf[B]
	override def estimateSize :Long = if (hasStep) 1 else 0
	override def trySplit() :Null = null

	override def forEachRemaining(action :Consumer[_ >: B]) :Unit = tryAdvance(action)
	override def iterator = if (hasStep) Iterator.single(nextStep()) else Iterator.empty
	
	override def toString :String = 
		if (hasStep) {
			val res = "Stepper(" + next() + ")"
			hasStep = true
			res
		} else
			"Stepper()"
}


private sealed trait PrimitiveStepper1[A, B, S <: Spliterator[B], I <: JIterator[B]] extends Stepper1[A, B] { 
	this :S with I =>
	override def forEachRemaining(action :Consumer[_ >: B]) :Unit = tryAdvance(action)
	override def next() = nextStep().asInstanceOf[B]
	override def spliterator[U >: A] :S = this
	override def javaIterator[U >: A] :I = this
}


/** Steppers consisting of a single element. */
@SerialVersionUID(ver)
object Stepper1 {
	def apply[A, S <: Stepper[_]](elem :A)(implicit shape :StepperShape[A, S]) :S with EfficientSplit =
		(shape.shape match {
			case IntShape    => ofInt(elem.asInstanceOf[Int])
			case LongShape   => ofLong(elem.asInstanceOf[Long])
			case DoubleShape => ofDouble(elem.asInstanceOf[Double])
			case CharShape   => ofChar(elem.asInstanceOf[Char])
			case ByteShape   => ofByte(elem.asInstanceOf[Byte])
			case ShortShape  => ofShort(elem.asInstanceOf[Short])
			case FloatShape  => ofFloat(elem.asInstanceOf[Float])
			case _           => ofAny(elem)
		}).asInstanceOf[S with EfficientSplit]

	def ofAny[A](elem :A)      :AnyStepper[A] with EfficientSplit = new Of(elem)
	def ofByte(elem :Byte)     :IntStepper with EfficientSplit = new OfInt(elem)
	def ofShort(elem :Short)   :IntStepper with EfficientSplit = new OfInt(elem)
	def ofChar(elem :Char)     :IntStepper with EfficientSplit = new OfInt(elem)
	def ofInt(elem :Int)       :IntStepper with EfficientSplit = new OfInt(elem)
	def ofLong(elem :Long)     :LongStepper with EfficientSplit = new OfLong(elem)
	def ofFloat(elem :Float)   :DoubleStepper with EfficientSplit = new OfDouble(elem)
	def ofDouble(elem :Double) :DoubleStepper with EfficientSplit = new OfDouble(elem)

	@SerialVersionUID(ver)
	private class Of[A](elem :A)
		extends Stepper1[A, A](if (elem == null) NullCharacteristics else NonNullCharacteristics) with AnyStepper[A]
	{
		override def nextStep() :A = elem
		override def tryAdvance(action :Consumer[_ >: A]) =
			hasStep && { action.accept(elem); hasStep = false; true }
		override def spliterator[U >: A]  = this.asInstanceOf[Spliterator[U]]
		override def javaIterator[U >: A] = this.asInstanceOf[JIterator[U]]
	}
	
	@SerialVersionUID(ver)
	private class OfInt(elem :Int) 
		extends Stepper1[Int, JInt] with IntStepper with Spliterator.OfInt with PrimitiveIterator.OfInt
		   with PrimitiveStepper1[Int, JInt, Spliterator.OfInt, PrimitiveIterator.OfInt] 
	{
		override def forEachRemaining(action :IntConsumer) :Unit = tryAdvance(action)
		override def nextInt() = elem
		override def nextStep() = elem
		override def tryAdvance(action :IntConsumer) =
			hasStep && { action.accept(elem); hasStep = false; true }
	}
	
	@SerialVersionUID(ver)
	private class OfLong(elem :Long) 
		extends Stepper1[Long, JLong] with LongStepper with Spliterator.OfLong with PrimitiveIterator.OfLong
		   with PrimitiveStepper1[Long, JLong, Spliterator.OfLong, PrimitiveIterator.OfLong] 
	{
		override def forEachRemaining(action :LongConsumer) :Unit = tryAdvance(action)
		override def nextLong() = elem
		override def nextStep() = elem
		override def tryAdvance(action :LongConsumer) =
			hasStep && { action.accept(elem); hasStep = false; true }
	}

	@SerialVersionUID(ver)
	private class OfDouble(elem :Double) 
		extends Stepper1[Double, JDouble] with DoubleStepper with Spliterator.OfDouble with PrimitiveIterator.OfDouble
		   with PrimitiveStepper1[Double, JDouble, Spliterator.OfDouble, PrimitiveIterator.OfDouble] 
	{
		override def forEachRemaining(action :DoubleConsumer) :Unit = tryAdvance(action)
		override def nextDouble() = elem
		override def nextStep() = elem
		override def tryAdvance(action :DoubleConsumer) =
			hasStep && { action.accept(elem); hasStep = false; true }
	}
	
	private final val NonNullCharacteristics :Int = IMMUTABLE | NONNULL | ORDERED | SIZED
	private final val NullCharacteristics    :Int = IMMUTABLE | ORDERED | SIZED
}




/** A `Stepper` of two elements.
  * @tparam A the nominal (Scala) element type of this `Stepper`.
  * @tparam B the type of the Java `Spliterator` - for value types, this is the Java box class of `A`.
  */
private sealed abstract class Stepper2[A, B](override val characteristics :Int = Stepper2.NonNullCharacteristics)
	extends Stepper[A] with EfficientSplit with Spliterator[B] with JIterator[B]
{
	var estimateSize :Long = 2
	override def hasNext :Boolean = estimateSize > 0
	override def hasStep :Boolean = estimateSize > 0
	override def next()  :B = nextStep().asInstanceOf[B]
	override def trySplit() :Null = null

	override def forEachRemaining(action :Consumer[_ >: B]) :Unit =
		while(tryAdvance(action)) {}
		
	override def iterator :Iterator[A] = estimateSize match {
		case 2 => val res = Iterator.double(nextStep(), nextStep()); estimateSize = 2; res
		case 1 => val res = Iterator.single(nextStep()); estimateSize = 1; res
		case _ => Iterator.empty
	}

	@inline final protected def empty() :Nothing = Stepper0.ofAny.nextStep()

	override def toString :String = estimateSize match {
		case 2 => val res = "Stepper(" + next() + "," + next() + ")"; estimateSize = 0; res
		case 1 => val res = "Stepper(" + next() + ")"; estimateSize = 1; res
		case _ => "Stepper()"
	}
}


private sealed trait PrimitiveStepper2[A, B, S <: Spliterator[B], I <: JIterator[B]] extends Stepper2[A, B] {
	this :S with I =>
	override def forEachRemaining(action :Consumer[_ >: B]) :Unit = while(tryAdvance(action)) {} 
	override def next() = nextStep().asInstanceOf[B]
	override def spliterator[U >: A] :S = this
	override def javaIterator[U >: A] :I = this
}


/** Steppers consisting of two elements. */
@SerialVersionUID(ver)
object Stepper2 {
	def apply[A, S <: Stepper[_]](_1 :A, _2 :A)(implicit shape :StepperShape[A, S]) :S with EfficientSplit =
		(shape.shape match {
			case IntShape    => ofInt(_1.asInstanceOf[Int], _2.asInstanceOf[Int])
			case LongShape   => ofLong(_1.asInstanceOf[Long], _2.asInstanceOf[Long])
			case DoubleShape => ofDouble(_1.asInstanceOf[Double], _2.asInstanceOf[Double])
			case CharShape   => ofChar(_1.asInstanceOf[Char], _2.asInstanceOf[Char])
			case ByteShape   => ofByte(_1.asInstanceOf[Byte], _2.asInstanceOf[Byte])
			case ShortShape  => ofShort(_1.asInstanceOf[Short], _2.asInstanceOf[Short])
			case FloatShape  => ofFloat(_1.asInstanceOf[Float], _2.asInstanceOf[Float])
			case _           => ofAny(_1, _2)
		}).asInstanceOf[S with EfficientSplit]

	def ofAny[A](_1 :A, _2 :A)           :AnyStepper[A] with EfficientSplit = new Of(_1, _2)
	def ofByte(_1 :Byte, _2 :Byte)       :IntStepper with EfficientSplit = new OfInt(_1, _2)
	def ofShort(_1 :Short, _2 :Short)    :IntStepper with EfficientSplit = new OfInt(_1, _2)
	def ofChar(_1 :Char, _2 :Char)       :IntStepper with EfficientSplit = new OfInt(_1, _2)
	def ofInt(_1 :Int, _2 :Int)          :IntStepper with EfficientSplit = new OfInt(_1, _2)
	def ofLong(_1 :Long, _2 :Long)       :LongStepper with EfficientSplit = new OfLong(_1, _2)
	def ofFloat(_1 :Float, _2 :Float)    :DoubleStepper with EfficientSplit = new OfDouble(_1, _2)
	def ofDouble(_1 :Double, _2 :Double) :DoubleStepper with EfficientSplit = new OfDouble(_1, _2)

	@inline final def iterator[A](_1 :A, _2 :A) :Iterator[A] = Iterator.double(_1, _2)

	@SerialVersionUID(ver)
	private class Of[A](first :A, second :A)
		extends Stepper2[A, A](if (first == null | second == null) NullCharacteristics else NonNullCharacteristics)
		   with AnyStepper[A]
	{
		override def nextStep() :A = estimateSize match {
			case 2 => estimateSize = 1; first
			case 1 => estimateSize = 0; second
			case _ => empty()
		}
		override def tryAdvance(action :Consumer[_ >: A]) = estimateSize match {
			case 2 => estimateSize = 1; action.accept(first); true
			case 1 => estimateSize = 0; action.accept(second); true
			case _ => false
		}
		override def spliterator[U]  = this.asInstanceOf[Spliterator[U]]
		override def javaIterator[U] = this.asInstanceOf[JIterator[U]]
	}
	
	@SerialVersionUID(ver)
	private class OfInt(first :Int, second :Int)
		extends Stepper2[Int, JInt] with IntStepper with Spliterator.OfInt with PrimitiveIterator.OfInt
		   with PrimitiveStepper2[Int, JInt, Spliterator.OfInt, PrimitiveIterator.OfInt] 
	{
		override def nextInt()  :Int = nextStep()
		override def nextStep() :Int = estimateSize match {
			case 2 => estimateSize = 1; first
			case 1 => estimateSize = 0; second
			case _ => empty()
		}
		override def tryAdvance(action :IntConsumer) = estimateSize match {
			case 2 => estimateSize = 1; action.accept(first); true
			case 1 => estimateSize = 0; action.accept(second); true
			case _ => false
		}
		override def forEachRemaining(action :IntConsumer) :Unit = while(tryAdvance(action)) {} 
	}
	
	@SerialVersionUID(ver)
	private class OfLong(first :Long, second :Long)
		extends Stepper2[Long, JLong] with LongStepper with Spliterator.OfLong with PrimitiveIterator.OfLong
		   with PrimitiveStepper2[Long, JLong, Spliterator.OfLong, PrimitiveIterator.OfLong] 
	{
		override def nextLong() :Long = nextStep()
		override def nextStep() :Long = estimateSize match {
			case 2 => estimateSize = 1; first
			case 1 => estimateSize = 0; second
			case _ => empty()
		}
		override def tryAdvance(action :LongConsumer) = estimateSize match {
			case 2 => estimateSize = 1; action.accept(first); true
			case 1 => estimateSize = 0; action.accept(second); true
			case _ => false
		}

		override def forEachRemaining(action :LongConsumer) :Unit = while(tryAdvance(action)) {}
	}
	
	@SerialVersionUID(ver)
	private class OfDouble(first :Double, second :Double)
		extends Stepper2[Double, JDouble] with DoubleStepper with Spliterator.OfDouble with PrimitiveIterator.OfDouble
		   with PrimitiveStepper2[Double, JDouble, Spliterator.OfDouble, PrimitiveIterator.OfDouble] 
	{
		override def nextDouble() :Double = nextStep()
		override def nextStep() :Double = estimateSize match {
			case 2 => estimateSize = 1; first
			case 1 => estimateSize = 0; second
			case _ => empty()
		}
		override def tryAdvance(action :DoubleConsumer) = estimateSize match {
			case 2 => estimateSize = 1; action.accept(first); true
			case 1 => estimateSize = 0; action.accept(second); true
			case _ => false
		}

		override def forEachRemaining(action :DoubleConsumer) :Unit = while(tryAdvance(action)) {}
	}

	private final val NonNullCharacteristics :Int = IMMUTABLE | NONNULL | ORDERED | SIZED
	private final val NullCharacteristics    :Int = IMMUTABLE | ORDERED | SIZED
}




/** A `Stepper` consisting of elements from `stepper1`, followed by elements of `stepper2`.
  * @tparam A the nominal (Scala) element type of this `Stepper`.
  * @tparam B the type of the Java `Spliterator` - for value types, this is the Java box class of `A`.
  * @tparam S the type of concatenated steppers.
  * @tparam J the type of `Spliterator`s produced by steppers `S`.
  */
private sealed abstract class ConcatStepper[A, B, S <: Stepper[A], J <: Spliterator[B], I <: JIterator[B]]
                                           (stepper1 :S, stepper2 :S)
	extends Stepper[A] with Spliterator[B] with JIterator[B]
{ this :J with I =>
	private[this] var spliterator1 :J = _
	private[this] var spliterator2 :J = _
	private[this] var firstHasNext = false
	private[this] var firstHasNextValid = false

	protected final def current = {
		if (!firstHasNextValid) {
			firstHasNext = stepper1.hasStep
			firstHasNextValid = true
		}
		if (firstHasNext) {
			firstHasNextValid = false
			stepper1
		} else
			stepper2
	}
	protected final def currentSpliterator = {
		if (!firstHasNextValid) {
			firstHasNext = stepper1.hasStep
			firstHasNextValid = true
		}
		if (firstHasNext) {
			firstHasNextValid = false
			if (spliterator1 == null)
				spliterator1 = stepper1.spliterator.asInstanceOf[J]
			spliterator1
		} else {
			if (spliterator2 == null)
				spliterator2 = stepper2.spliterator.asInstanceOf[J]
			spliterator2
		}
	}

	override def hasStep = {
		if (!firstHasNextValid)
			firstHasNext = stepper1.hasStep
		firstHasNext || stepper2.hasStep
	}
	override def hasNext = hasStep
	override def next() = nextStep().asInstanceOf[B]

	override def tryAdvance(action :Consumer[_ >: B]) = currentSpliterator.tryAdvance(action)
	override def forEachRemaining(action :Consumer[_ >: B]) :Unit = while(tryAdvance(action)) {}

	override def trySplit() :Null = null

	override def estimateSize() =
		stepper1.estimateSize match {
			case -1 => -1
			case firstSize => stepper2.estimateSize match {
				case -1 => -1
				case secondSize => firstSize + secondSize
			}
		}

	override val characteristics =
		if (stepper1 != null)
			if (stepper2.hasStep) stepper1.characteristics & stepper2.characteristics
			else stepper1.characteristics
		else
	        stepper2.characteristics

	@inline protected final def spliteratorImpl :J = {
		if (!firstHasNextValid) {
			firstHasNext = stepper1.hasStep
			firstHasNextValid = true
		}
		if (firstHasNext)
			this
		else {
			if (spliterator2 == null)
				spliterator2 = stepper2.spliterator.asInstanceOf[J]
			spliterator2
		}
	}
	@inline protected final def javaIteratorImpl :I = {
		if (!firstHasNextValid) {
			firstHasNext = stepper1.hasStep
			firstHasNextValid = true
		}
		if (firstHasNext) this else stepper2.javaIterator.asInstanceOf[I]
	}
}


private sealed trait PrimitiveConcatStepper[A, B, S <: Stepper[A], J <: Spliterator[B], I <: JIterator[B]]
	extends ConcatStepper[A, B, S, J, I]
 { this :J with I =>
	override def forEachRemaining(action :Consumer[_ >: B]) :Unit = while(tryAdvance(action)) {}
	override def next() = nextStep().asInstanceOf[B]
	override def spliterator[U >: A]  :J = spliteratorImpl
	override def javaIterator[U >: A] :I = javaIteratorImpl
}


/** `Stepper`s consisting of elements from the first stepper followed by the elements of the second stepper. */
object ConcatStepper {
	def apply[A, S <: Stepper[_]](first :S, second :S)(implicit shape :StepperShape[A, S]) :S =
		shape.shape match {
			case IntShape => 
				ofInt(first.asInstanceOf[IntStepper], second.asInstanceOf[IntStepper]).asInstanceOf[S]
			case LongShape =>
				ofLong(first.asInstanceOf[LongStepper], second.asInstanceOf[LongStepper]).asInstanceOf[S]
			case DoubleShape =>
				ofDouble(first.asInstanceOf[DoubleStepper], second.asInstanceOf[DoubleStepper]).asInstanceOf[S]
			case _ =>
				ofAny(first.asInstanceOf[Stepper[A]], second.asInstanceOf[Stepper[A]]).asInstanceOf[S]
		}
		
	def ofAny[A](first :Stepper[A], second :Stepper[A]) :AnyStepper[A] =
		if (first.hasStep) {
			if (second.hasStep)
				new OfAny(first, second)
			else
				first match {
					case any :AnyStepper[A] => any
					case _ => new OfAny(first, second)
				}
		} else
			second match {
				case any :AnyStepper[A] => any
				case _ => new OfAny(first, second)
			}


	def ofInt(first :IntStepper, second :IntStepper) :IntStepper =
		if (first.hasStep)
			if (second.hasStep) new OfInt(first, second) else first
		else
			second
	
	def ofLong(first :LongStepper, second :LongStepper) :LongStepper =
		if (first.hasStep) 
			if (second.hasStep) new OfLong(first, second) else first
		else 
			second

	def ofDouble(first :DoubleStepper, second :DoubleStepper) :DoubleStepper =
		if (first.hasStep) 
			if (second.hasStep) new OfDouble(first, second) else first
		else 
			second


	@SerialVersionUID(ver)
	private class OfAny[A](_1 :Stepper[A], _2 :Stepper[A])
		extends ConcatStepper[A, A, Stepper[A], Spliterator[A], JIterator[A]](_1, _2) with AnyStepper[A]
	{
		override def nextStep() = current.nextStep()
		override def spliterator[U] = spliteratorImpl.asInstanceOf[Spliterator[U]]
		override def javaIterator[U] = javaIteratorImpl.asInstanceOf[JIterator[U]]
	}

	@SerialVersionUID(ver)
	private class OfInt(_1 :IntStepper, _2 :IntStepper)
		extends ConcatStepper[Int, JInt, IntStepper, Spliterator.OfInt, PrimitiveIterator.OfInt](_1, _2)
		   with IntStepper with Spliterator.OfInt with PrimitiveIterator.OfInt
		   with PrimitiveConcatStepper[Int, JInt, IntStepper, Spliterator.OfInt, PrimitiveIterator.OfInt]
	{
		override def nextStep() = current.nextStep()
		override def nextInt()  = current.nextStep()
		override def tryAdvance(action :IntConsumer) = currentSpliterator.tryAdvance(action)
		override def forEachRemaining(action :IntConsumer) :Unit = while (tryAdvance(action)) {}
	}

	@SerialVersionUID(ver)
	private class OfLong(_1 :LongStepper, _2 :LongStepper)
		extends ConcatStepper[Long, JLong, LongStepper, Spliterator.OfLong, PrimitiveIterator.OfLong](_1, _2)
		   with LongStepper with Spliterator.OfLong with PrimitiveIterator.OfLong
		   with PrimitiveConcatStepper[Long, JLong, LongStepper, Spliterator.OfLong, PrimitiveIterator.OfLong]
	{
		override def nextStep() = current.nextStep()
		override def nextLong() = current.nextStep()
		override def tryAdvance(action :LongConsumer) = currentSpliterator.tryAdvance(action)
		override def forEachRemaining(action :LongConsumer) :Unit = while (tryAdvance(action)) {}
	}

	@SerialVersionUID(ver)
	private class OfDouble(_1 :DoubleStepper, _2 :DoubleStepper)
		extends ConcatStepper[Double, JDouble, DoubleStepper, Spliterator.OfDouble, PrimitiveIterator.OfDouble](_1, _2)
		   with DoubleStepper with Spliterator.OfDouble with PrimitiveIterator.OfDouble
		   with PrimitiveConcatStepper[Double, JDouble, DoubleStepper, Spliterator.OfDouble, PrimitiveIterator.OfDouble]
	{
		override def nextStep() = current.nextStep()
		override def nextDouble() = current.nextStep()
		override def tryAdvance(action :DoubleConsumer) = currentSpliterator.tryAdvance(action)
		override def forEachRemaining(action :DoubleConsumer) :Unit = while (tryAdvance(action)) {}
	}
}
