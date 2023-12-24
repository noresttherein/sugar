package net.noresttherein.sugar.collections

import java.util.{PrimitiveIterator, Spliterator}
import java.util.Spliterator.{CONCURRENT, DISTINCT, IMMUTABLE, NONNULL, ORDERED, SIZED, SUBSIZED}
import java.util.function.{Consumer, DoubleConsumer, IntConsumer, LongConsumer}

import scala.annotation.tailrec
import scala.collection.{AnyStepper, DoubleStepper, IndexedSeqView, IntStepper, LongStepper, Stepper, StepperShape}
import scala.collection.Stepper.EfficientSplit
import scala.collection.StepperShape.{ByteShape, CharShape, DoubleShape, FloatShape, IntShape, LongShape, ReferenceShape, ShortShape}

import net.noresttherein.sugar.JavaTypes.{JDouble, JDoubleIterator, JInt, JIntIterator, JIterator, JLong, JLongIterator}
import net.noresttherein.sugar.collections.ConcatStepper.AllCharacteristics
import net.noresttherein.sugar.collections.Stepper0.EmptyCharacteristics
import net.noresttherein.sugar.collections.extensions.StepType
import net.noresttherein.sugar.extensions.{BooleanExtension, IteratorCompanionExtension, castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.slang.extensions.AnyRefExtension




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
	override def characteristics :Int = EmptyCharacteristics
	override def forEachRemaining(action :Consumer[_ >: A]) :Unit = ()

	override def iterator :Iterator[Nothing] = Iterator.empty
	override def toString = "Stepper()"
}


/** Empty `Stepper`s. */
@SerialVersionUID(Ver)
private object Stepper0 {
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

	final val EmptyCharacteristics = DISTINCT | IMMUTABLE | NONNULL | ORDERED | SIZED

	@SerialVersionUID(Ver)
	object OfAny extends Stepper0[Nothing] with AnyStepper[Nothing] {
		override def spliterator[U] :Spliterator[U] = this.asInstanceOf[Spliterator[U]]
		override def javaIterator[U] :JIterator[U] = this.asInstanceOf[JIterator[U]]
	}

	trait OfPrimitive[A, S <: Spliterator[A], I <: JIterator[A]] extends Stepper0[A] { this :S with I =>
		override def next() :Nothing = nextStep()
		override def forEachRemaining(action :Consumer[_ >: A]) :Unit = ()
		override def spliterator[B] :this.type = this
		override def javaIterator[B] :this.type= this
	}

	@SerialVersionUID(Ver)
	object OfInt
		extends Stepper0[JInt] with IntStepper with Spliterator.OfInt with PrimitiveIterator.OfInt
		   with OfPrimitive[JInt, Spliterator.OfInt, PrimitiveIterator.OfInt]
	{
		override def nextInt() :Int = nextStep()
		override def tryAdvance(action :IntConsumer) :Boolean = false
		override def forEachRemaining(action :IntConsumer) :Unit = ()
		override def toString = "IntAllInStepper()"
	}

	@SerialVersionUID(Ver)
	object OfLong
		extends Stepper0[JLong] with LongStepper with Spliterator.OfLong with PrimitiveIterator.OfLong
		   with OfPrimitive[JLong, Spliterator.OfLong, PrimitiveIterator.OfLong]
	{
		override def nextLong() :Long = nextStep()
		override def tryAdvance(action :LongConsumer) :Boolean = false
		override def forEachRemaining(action :LongConsumer) :Unit = ()
		override def toString = "LongAllInStepper()"
	}

	@SerialVersionUID(Ver)
	object OfDouble
		extends Stepper0[JDouble] with DoubleStepper with Spliterator.OfDouble with PrimitiveIterator.OfDouble
		   with OfPrimitive[JDouble, Spliterator.OfDouble, PrimitiveIterator.OfDouble]
	{
		override def nextDouble() :Double = nextStep()
		override def tryAdvance(action :DoubleConsumer) :Boolean = false
		override def forEachRemaining(action :DoubleConsumer) :Unit = ()
		override def toString = "DoubleAllInStepper"
	}
}




/** A `Stepper` of a single element.
  * @tparam A the nominal (Scala) element type of this `Stepper`.
  * @tparam B the type of the Java `Spliterator` - for value types, this is the Java box class of `A`.
  */
private sealed abstract class Stepper1[@specialized(JavaIterator.Types) A, B]
                                      (elem :A, override val characteristics :Int = Stepper1.NonNullCharacteristics)
	extends Stepper[A] with EfficientSplit with Spliterator[B] with JIterator[B]
{
	private[this] var has = true
	override def hasStep :Boolean = has
	override def hasNext :Boolean = has
	override def nextStep() :A = if (has) { has = false; elem } else throw new NoSuchElementException(toString)
	override def next() :B = nextStep().asInstanceOf[B]
	override def estimateSize :Long = if (has) 1 else 0
	override def trySplit() :Null = null

	override def forEachRemaining(action :Consumer[_ >: B]) :Unit = tryAdvance(action)
	override def iterator = if (hasStep) Iterator.single(nextStep()) else Iterator.empty

	override def toString :String =
		if (has) {
			val res = "Stepper(" + elem + ")"
			res
		} else
			"Stepper()"
}


/** Steppers consisting of a single element. */
@SerialVersionUID(Ver)
private object Stepper1 {
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

	@SerialVersionUID(Ver)
	private final class Of[A](elem :A)
		extends Stepper1[A, A](elem, if (elem == null) NullCharacteristics else NonNullCharacteristics)
		   with AnyStepper[A]
	{
		override def tryAdvance(action :Consumer[_ >: A]) = hasStep && { action.accept(nextStep()); true }
		override def spliterator[U >: A]  = this.asInstanceOf[Spliterator[U]]
		override def javaIterator[U >: A] = this.asInstanceOf[JIterator[U]]
	}

	private sealed trait OfPrimitive[A, B, S <: Spliterator[B], I <: JIterator[B]] extends Stepper1[A, B] {
		this :S with I =>
		override def forEachRemaining(action :Consumer[_ >: B]) :Unit = tryAdvance(action)
		override def next() = nextStep().asInstanceOf[B]
		override def spliterator[U >: A] :S = this
		override def javaIterator[U >: A] :I = this
	}

	@SerialVersionUID(Ver)
	private final class OfInt(elem :Int)
		extends Stepper1[Int, JInt](elem) with IntStepper with Spliterator.OfInt with PrimitiveIterator.OfInt
		   with OfPrimitive[Int, JInt, Spliterator.OfInt, PrimitiveIterator.OfInt]
	{
		override def forEachRemaining(action :IntConsumer) :Unit = tryAdvance(action)
		override def tryAdvance(action :IntConsumer) = hasStep && { action.accept(nextStep()); true }
		override def nextInt() = nextStep()
		override def toString :String = "Int" + super.toString
	}
	
	@SerialVersionUID(Ver)
	private final class OfLong(elem :Long)
		extends Stepper1[Long, JLong](elem) with LongStepper with Spliterator.OfLong with PrimitiveIterator.OfLong
		   with OfPrimitive[Long, JLong, Spliterator.OfLong, PrimitiveIterator.OfLong]
	{
		override def forEachRemaining(action :LongConsumer) :Unit = tryAdvance(action)
		override def tryAdvance(action :LongConsumer) = hasStep && { action.accept(nextStep()); true }
		override def nextLong() = nextStep()
		override def toString :String = "Long" + super.toString
	}

	@SerialVersionUID(Ver)
	private class OfDouble(elem :Double) 
		extends Stepper1[Double, JDouble](elem) with DoubleStepper with Spliterator.OfDouble with PrimitiveIterator.OfDouble
		   with OfPrimitive[Double, JDouble, Spliterator.OfDouble, PrimitiveIterator.OfDouble]
	{
		override def forEachRemaining(action :DoubleConsumer) :Unit = tryAdvance(action)
		override def tryAdvance(action :DoubleConsumer) = hasStep && { action.accept(nextStep()); true }
		override def nextDouble() = nextStep()
		override def toString = "Double" + super.toString
	}
	
	private final val NonNullCharacteristics :Int = IMMUTABLE | NONNULL | ORDERED | SIZED
	private final val NullCharacteristics    :Int = IMMUTABLE | ORDERED | SIZED
}




/** A `Stepper` of two elements.
  * @tparam A the nominal (Scala) element type of this `Stepper`.
  * @tparam B the type of the Java `Spliterator` - for value types, this is the Java box class of `A`.
  */
private sealed abstract class Stepper2[@specialized(JavaIterator.Types) A, B, +S >: Null <: Stepper2[A, B, S]]
                                      (first :A, second :A)
	extends Stepper[A] with AllInStepper[A, B, S] with EfficientSplit //with Spliterator[B] with JIterator[B]
{
	private[this] var size = 2
	override def estimateSize :Long = size
	override def characteristics() :Int = size match {
		case 2 if first != null & second != null => Stepper2.NonNullCharacteristics
		case 1 if second != null                 => Stepper2.NonNullCharacteristics
		case 0                                   => Stepper2.NonNullCharacteristics
		case _                                   => Stepper2.NullCharacteristics
	}
//	override def hasNext :Boolean = size > 0
	override def hasStep :Boolean = size > 0
	override def next()  :B = nextStep().asInstanceOf[B]
	override def nextStep() :A = size match {
		case 2 => size = 1; first
		case 1 => size = 0; second
		case _ => throw new NoSuchElementException(toString)
	}
	override def trySplit() :Null = null

//	override def forEachRemaining(action :Consumer[_ >: B]) :Unit = size match {
//		case 2 => size = 0; action.accept(first.asInstanceOf[B]); action.accept(second.asInstanceOf[B])
//		case 1 => size = 0; action.accept(second.asInstanceOf[B])
//	}
//		while(tryAdvance(action)) {}
	override def tryAdvance(action :Consumer[_ >: B]) :Boolean = size match {
		case 2 => size = 1; action.accept(first.asInstanceOf[B]); true
		case 1 => size = 0; action.accept(second.asInstanceOf[B]); true
		case _ => false
	}
	override def iterator :Iterator[A] = estimateSize match {
		case 2 => Iterator.two(first, second)
		case 1 => Iterator.single(second)
		case _ => Iterator.empty
	}

//	@inline final protected def empty() :Nothing = Stepper0.ofAny.nextStep()
//
	override def toString :String = estimateSize match {
		case 2 => "Stepper(" + first + "," + second + ")"
		case 1 => "Stepper(" + second + ")"
		case _ => "Stepper()"
	}
}


/** Steppers consisting of two elements. */
@SerialVersionUID(Ver)
private object Stepper2 {
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

	@inline final def iterator[A](_1 :A, _2 :A) :Iterator[A] = Iterator.two(_1, _2)

	@SerialVersionUID(Ver)
	private class Of[A](first :A, second :A) extends Stepper2[A, A, Of[A]](first, second) with AllInAnyStepper[A] {
//		override def nextStep() :A = estimateSize match {
//			case 2 => estimateSize = 1; first
//			case 1 => estimateSize = 0; second
//			case _ => empty()
//		}
//		override def tryAdvance(action :Consumer[_ >: A]) = estimateSize match {
//			case 2 => action.accept(nextStep()); action.accept(nextStep()); true
//			case 1 => estimateSize = 0; action.accept(second); true
//			case _ => false
//		}
//		override def spliterator[U]  = this.asInstanceOf[Spliterator[U]]
//		override def javaIterator[U] = this.asInstanceOf[JIterator[U]]
	}

	private sealed trait OfPrimitive[A, B, S >: Null <: Stepper2[A, B, S]] extends Stepper2[A, B, S] { this :S =>
		override def forEachRemaining(action :Consumer[_ >: B]) :Unit = while(tryAdvance(action)) {}
//		override def next() = nextStep().asInstanceOf[B]
//		override def spliterator[U >: A] :S = this
//		override def javaIterator[U >: A] :S = this
	}

	@SerialVersionUID(Ver)
	private class OfInt(first :Int, second :Int)
		extends Stepper2[Int, JInt, OfInt](first, second)
		   with AllInIntStepper with OfPrimitive[Int, JInt, OfInt] with Spliterator.OfInt with JIntIterator
			//IntStepper with Spliterator.OfInt with PrimitiveIterator.OfInt
	{
		override def nextInt()  :Int = nextStep()
		override def tryAdvance(action :IntConsumer) :Boolean = hasStep && { action.accept(nextStep()); true }
//		override def nextStep() :Int = estimateSize match {
//			case 2 => estimateSize = 1; first
//			case 1 => estimateSize = 0; second
//			case _ => empty()
//		}
//		override def tryAdvance(action :IntConsumer) = estimateSize match {
//			case 2 => estimateSize = 1; action.accept(first); true
//			case 1 => estimateSize = 0; action.accept(second); true
//			case _ => false
//		}
//		override def forEachRemaining(action :IntConsumer) :Unit = while(tryAdvance(action)) {}
		override def toString :String = "Int" + super.toString
	}
	
	@SerialVersionUID(Ver)
	private class OfLong(first :Long, second :Long)
		extends Stepper2[Long, JLong, OfLong](first, second) with AllInLongStepper
//		   with LongStepper with Spliterator.OfLong with PrimitiveIterator.OfLong
		   with OfPrimitive[Long, JLong, OfLong] with Spliterator.OfLong with JLongIterator
	{
		override def nextLong() :Long = nextStep()
//		override def nextStep() :Long = estimateSize match {
//			case 2 => estimateSize = 1; first
//			case 1 => estimateSize = 0; second
//			case _ => empty()
//		}
//		override def tryAdvance(action :LongConsumer) = estimateSize match {
//			case 2 => estimateSize = 1; action.accept(first); true
//			case 1 => estimateSize = 0; action.accept(second); true
//			case _ => false
//		}
//		override def forEachRemaining(action :LongConsumer) :Unit = while(tryAdvance(action)) {}
//		override def tryAdvance(action :LongConsumer) :Boolean = hasStep && { action.accept(nextStep()); true }
		override def toString = "Long" + super.toString
	}
	
	@SerialVersionUID(Ver)
	private class OfDouble(first :Double, second :Double)
		extends Stepper2[Double, JDouble, OfDouble](first, second) with AllInDoubleStepper
//		   with DoubleStepper with Spliterator.OfDouble with PrimitiveIterator.OfDouble
		   with OfPrimitive[Double, JDouble, OfDouble] with Spliterator.OfDouble with JDoubleIterator
	{
		override def nextDouble() :Double = nextStep()
//		override def nextStep() :Double = estimateSize match {
//			case 2 => estimateSize = 1; first
//			case 1 => estimateSize = 0; second
//			case _ => empty()
//		}
//		override def tryAdvance(action :DoubleConsumer) = estimateSize match {
//			case 2 => estimateSize = 1; action.accept(first); true
//			case 1 => estimateSize = 0; action.accept(second); true
//			case _ => false
//		}
//		override def forEachRemaining(action :DoubleConsumer) :Unit = while(tryAdvance(action)) {}
//		override def tryAdvance(action :DoubleConsumer) :Boolean = hasStep && { action.accept(nextStep()); true }
		override def toString :String = "Double" + super.toString
	}

	private final val NonNullCharacteristics :Int = IMMUTABLE | NONNULL | ORDERED | SIZED
	private final val NullCharacteristics    :Int = IMMUTABLE | ORDERED | SIZED
}




/** A `Stepper` consisting of elements from `stepper1`, followed by elements of `stepper2`.
  * @tparam A the nominal (Scala) element type of this `Stepper`.
  * @tparam B the type of the Java `Spliterator` - for value types, this is the Java box class of `A`.
  * @tparam S the type of concatenated steppers.
  * @tparam J the type of `Spliterator`s produced by steppers `S`.
  */ //it would be useful if ConcatStepper *was* a Seq extending PassedArray
private sealed abstract class ConcatStepper
                              [A, B, +S >: Null <: Stepper[A], J >: Null <: Spliterator[B], I >: Null <: JIterator[B]]
                              (private[this] var steppers :IndexedSeq[S], //con't be mutable due to trySplit
                               private[this] var offset :Int, private[this] var limit :Int)
	extends Stepper[A] with Spliterator[B] with JIterator[B] with EfficientSplit with Cloneable
{ this :S with J with I =>
	def this(steppers :IndexedSeq[S]) = this(steppers, 0, steppers.length)
	private[this] var first :S = steppers.head
	private[this] var firstSpliterator :J = _
	private[this] var firstIterator :I = _
	private[this] var state = -1 //1 => hasNext, 0 => !hasNext, -1 => unknown
	private[this] var flags = -1

	final def parts :IndexedSeq[S] = steppers.slice(offset, limit)
	//We are cheating here, because ConcatStepper.OfAny defines it - via AnyStepper[A] - as Spliterator[A],
	// while technically it may be a Spliterator.OfInt (or any other), because Concat.OfAny accepts all kinds of steppers.
	// In the end however the autoboxing will do its job, as Spliterator.OfInt extends Spliterator[Integer].
	protected final def current :S = {
		if (state < 0 && !first.hasStep)
			hasStep
		state = -1
		first
	}
	protected final def currentSpliterator = {
		if (firstSpliterator == null || state < 0 || !first.hasStep) {
			hasStep
			firstSpliterator = first.spliterator.asInstanceOf[J]
		}
		state = -1
		firstSpliterator
	}
	protected final def currentJavaIterator :I = {
		if (firstIterator == null || state < 0 || !first.hasStep) {
			hasStep
			firstIterator = first.javaIterator.asInstanceOf[I]
		}
		state = -1
		firstIterator
	}

	final override def hasStep :Boolean = {
		if (state < 0) {
//			val count = steppers.length - 1
			val count = limit - 1
			state = first.hasStep.toInt
			if (offset < count & state == 0) {
//				var offset = 1
				offset += 1
				first = steppers(offset)
				while (offset < count && !first.hasStep) {
					offset += 1
					first = steppers(offset)
				}
//				steppers = steppers.drop(i)
				state = (offset < count || first.hasStep).toInt
				flags = -1
				firstSpliterator = null
				firstIterator    = null
			}
		}
		state == 1
	}
	final override def hasNext = hasStep

	override def nextStep() :A = current.nextStep()
	override def next() :B = nextStep().asInstanceOf[B]

	override def forEachRemaining(action :Consumer[_ >: B]) :Unit =
		while(hasStep)
			currentSpliterator.forEachRemaining(action)

	override def tryAdvance(action :Consumer[_ >: B]) = hasStep && { action.accept(next()); true }
//
	override def trySplit() :S with J = {
//		val iters = steppers.length
//		if (iters == 1)
		if (offset + 1 >= limit)
			null
		else {
//			var i = 0
			var i = offset
			if (hasCharacteristics(SIZED)) {
				val halfSize   = estimateSize >> 1
				var prefixSize = 0L
				while (i < limit && prefixSize < halfSize) {
					val nextSize = steppers(i).estimateSize
					if (nextSize > Long.MaxValue - prefixSize)
						prefixSize = Long.MaxValue
					else
						prefixSize += nextSize
					i += 1
				}
			} else
				i = (limit - offset) >> 1
				if (i == 0) i += 1
				else if (i == limit) i -= 1
//			val (prefix, suffix) =
//				if (i == 0) steppers.splitAt(i + 1)
//				else if (i == iters) steppers.splitAt(i - 1)
//				else steppers.splitAt(i)
			flags    = -1
//			steppers = prefix
			val lim  = limit
			limit    = i
			val res  = clone().asInstanceOf[S with J]
//			steppers = suffix
			limit    = lim
			offset   = i
			state    = -1
//			first    = steppers(0)
			first    = steppers(offset)
			firstSpliterator = null
			firstIterator    = null
			res
		}
	}

	override def estimateSize :Long = {
		var size  = 0L
		var i     = 0
		val iters = steppers.length
		while (i < iters) {
			val s = steppers(i).estimateSize
			if (s > Long.MaxValue - size)
				return Long.MaxValue
			size += s
			i    += 1
		}
		size
	}

	override def characteristics = {
		if (flags < 0) {
			flags = 0
//			val iters = steppers.length
//			if (iters == 0)
			if (offset == limit)
				flags = EmptyCharacteristics
			else {
				flags = AllCharacteristics
//				var i = 0
				var i = offset
				while (i < limit) {
					flags &= steppers(i).characteristics
					i += 1
				}
			}
		}
		flags
	}

	protected final def spliteratorImpl :J =
		if (limit - offset == 1) {
			state = -1
			first.spliterator.asInstanceOf[J]
		} else
			this
	protected final def javaIteratorImpl :I =
		if (limit - offset == 1) {
			state = -1
			first.javaIterator.asInstanceOf[I]
		} else
			this

	override def toString = new IndexedSeqView.Slice(steppers, offset, limit).mkString("++")
}


/** `Stepper`s consisting of elements from the first stepper followed by the elements of the second stepper. */
private object ConcatStepper {
	//Works for EfficientSplit because ConcatStepper extends it and there is StepperType[A, S with EfficientSplit].
	def apply[A, S <: Stepper[_]](first :S, second :S)(implicit elemType :StepType[A, S]) :S =
		elemType.stepperShape.shape match {
//			case ReferenceShape if first.isInstanceOf[AnyStepper[_]] & second.isInstanceOf[AnyStepper[_]] =>
//				ofRef(first.asInstanceOf[Stepper[A]], second.asInstanceOf[Stepper[A]]).asInstanceOf[S]
			case ReferenceShape =>
				ofAny(first, second).asInstanceOf[S]
			case IntShape | ByteShape | ShortShape | CharShape =>
				ofInt(first.asInstanceOf[IntStepper], second.asInstanceOf[IntStepper]).asInstanceOf[S]
			case LongShape =>
				ofLong(first.asInstanceOf[LongStepper], second.asInstanceOf[LongStepper]).asInstanceOf[S]
			case DoubleShape | FloatShape =>
				ofDouble(first.asInstanceOf[DoubleStepper], second.asInstanceOf[DoubleStepper]).asInstanceOf[S]
			case _ =>
				ofAny(first.asInstanceOf[Stepper[A]], second.asInstanceOf[Stepper[A]]).asInstanceOf[S]
		}

	def apply[A, S <: Stepper[_]](steppers :Seq[S])(implicit elemType :StepType[A, S]) :S =
		elemType.stepperShape.shape match {
			case ReferenceShape =>
				ofAny(steppers.castParam[Stepper[A]]).asInstanceOf[S]
			case IntShape | ByteShape | ShortShape | CharShape =>
				ofInt(steppers.castParam[IntStepper]).asInstanceOf[S]
			case LongShape =>
				ofLong(steppers.castParam[LongStepper]).asInstanceOf[S]
			case DoubleShape | FloatShape =>
				ofDouble(steppers.castParam[DoubleStepper]).asInstanceOf[S]
			case _ =>
				ofAny(steppers.castParam[Stepper[A]]).asInstanceOf[S]
		}

	@tailrec def ofAny[A](first :Stepper[A], second :Stepper[A]) :Stepper[A] =
		(first, second) match {
			case (l :LazyStepper[A, Stepper[A]], _) if l.isEvaluated => ofAny(l.stepper, second)
			case (_, r :LazyStepper[A, Stepper[A]]) if r.isEvaluated => ofAny(first, r.stepper)
			case (l :IntStepper, r :IntStepper)                      => ofInt(l, r).asInstanceOf[Stepper[A]]
			case (l :LongStepper, r :LongStepper)                    => ofLong(l, r).asInstanceOf[Stepper[A]]
			case (l :DoubleStepper, r :DoubleStepper)                => ofDouble(l, r).asInstanceOf[Stepper[A]]
//			case (l :AnyStepper[A], r :AnyStepper[A])                => ofRef(l, r)
			case (_ :LazyStepper[_, _], _ :LazyStepper[_, _])        => new OfAny(PassedArray.two(first, second))
			case (_ :LazyStepper[_, _], _) if !second.hasStep        => first  //if first is lazy then second is not
			case (_, _ :LazyStepper[_, _]) if !first.hasStep         => second //if second is lazy then first is not
			case (_, _) if !first.hasStep  => second //at this point we now neither is lazy
			case (_, _) if !second.hasStep => first
			case _                         => ofRef(first, second)
		}

	def ofAny[A](steppers :Seq[Stepper[A]]) :Stepper[A] =
		if (steppers.forall(_.isInstanceOf[IntStepper]))
			ofInt(steppers.castParam[IntStepper]).asInstanceOf[Stepper[A]]
		else if (steppers.forall(_.isInstanceOf[LongStepper]))
			ofLong(steppers.castParam[LongStepper]).asInstanceOf[Stepper[A]]
		else if (steppers.forall(_.isInstanceOf[DoubleStepper]))
			ofDouble(steppers.castParam[DoubleStepper]).asInstanceOf[Stepper[A]]
		else if (steppers.forall(_.isInstanceOf[AnyStepper[_]]))
			ofRef(steppers)
		else
			new OfAny(steppers.toIndexedSeq)

	@tailrec def ofRef[A](first :Stepper[A], second :Stepper[A]) :AnyStepper[A] =
		(first, second) match { //todo; debugger says this actually creates tuples, if so - rewrite it!
			case (l :LazyStepper[A, Stepper[A]], _) if l.isEvaluated => ofRef(l.stepper, second)
			case (_, r :LazyStepper[A, Stepper[A]]) if r.isEvaluated => ofRef(first, r.stepper)
//			case (_ :LazyStepper[_, _], _ :LazyStepper[_, _])      => new OfAny(PassedArray.two(first, second))
//			case (_, any :LazyStepper.OfRef[A]) if !first.hasStep  => any //don't call hesStep on a lazy stepper
//			case (any :LazyStepper.OfRef[A], _) if !second.hasStep => any
			case (_ :LazyStepper[_, _], _)                         => new OfAny(PassedArray.two(first, second))
			case (_, _ :LazyStepper[_, _])                         => new OfAny(PassedArray.two(first, second))
			case (_, any :AnyStepper[A]) if !first.hasStep         => any //now we know first is not lazy and can call hasStep
			case (any :AnyStepper[A], _) if !second.hasStep        => any
			case (cat1 :ConcatStepper[A, _, Stepper[A], _, _] @unchecked,
			      cat2 :ConcatStepper[A, _, Stepper[A], _, _] @unchecked)
			=>
				new OfAny(cat1.parts ++ cat2.parts)
			case (cat1 :ConcatStepper[A, _, Stepper[A], _, _] @unchecked, _) => new OfAny(cat1.parts :+ second)
			case (_, cat2 :ConcatStepper[A, _, Stepper[A], _, _] @unchecked) => new OfAny(first +: cat2.parts)
			case _                                                           => new OfAny(PassedArray.two(first, second))
		}

	def ofRef[A](steppers :Seq[Stepper[A]]) :AnyStepper[A] =
		new OfAny(steppers.toIndexedSeq)

	@tailrec def ofInt(first :IntStepper, second :IntStepper) :IntStepper = (first, second) match {
		case (l :LazyStepper.OfInt, _) if l.isEvaluated   => ofInt(l.stepper, second)
		case (_, r :LazyStepper.OfInt) if r.isEvaluated   => ofInt(first, r.stepper)
		case (_ :LazyStepper[_, _], _ :LazyStepper[_, _]) => new OfInt(PassedArray.two(first, second))
		case (_ :LazyStepper[_, _], _)  => if (second.hasStep) new OfInt(PassedArray.two(first, second)) else first
		case (_, _ :LazyStepper[_, _])  => if (first.hasStep) new OfInt(PassedArray.two(first, second)) else second
		case _ if !first.hasStep        => second
		case _ if !second.hasStep       => first
		case (cat1 :OfInt, cat2 :OfInt) => new OfInt(cat1.parts ++ cat2.parts)
		case (cat1 :OfInt, _)           => new OfInt(cat1.parts :+ second)
		case (_, cat2 :OfInt)           => new OfInt(first +: cat2.parts)
		case _                          => new OfInt(PassedArray.two(first, second))
	}

	def ofInt(steppers :Seq[IntStepper]) :IntStepper =
		new OfInt(steppers.toIndexedSeq)

	@tailrec def ofLong(first :LongStepper, second :LongStepper) :LongStepper = (first, second) match {
		case (l :LazyStepper.OfLong, _)                   => ofLong(l.stepper, second)
		case (_, r :LazyStepper.OfLong)                   => ofLong(first, r.stepper)
		case (_ :LazyStepper[_, _], _ :LazyStepper[_, _]) => new OfLong(PassedArray.two(first, second))
		case (_ :LazyStepper[_, _], _)    => if (second.hasStep) new OfLong(PassedArray.two(first, second)) else first
		case (_, _ :LazyStepper[_, _])    => if (first.hasStep) new OfLong(PassedArray.two(first, second)) else second
		case _ if !first.hasStep          => second
		case _ if !second.hasStep         => first
		case (cat1 :OfLong, cat2 :OfLong) => new OfLong(cat1.parts ++ cat2.parts)
		case (cat1 :OfLong, _)            => new OfLong(cat1.parts :+ second)
		case (_, cat2 :OfLong)            => new OfLong(first +: cat2.parts)
		case _                            => new OfLong(PassedArray.two(first, second))
	}

	def ofLong(steppers :Seq[LongStepper]) :LongStepper =
		new OfLong(steppers.toIndexedSeq)

	@tailrec def ofDouble(first :DoubleStepper, second :DoubleStepper) :DoubleStepper = (first, second) match {
		case (l :LazyStepper.OfDouble, _)                 => ofDouble(l.stepper, second)
		case (_, r :LazyStepper.OfDouble)                 => ofDouble(first, r.stepper)
		case (_ :LazyStepper[_, _], _ :LazyStepper[_, _]) => new OfDouble(PassedArray.two(first, second))
		case (_ :LazyStepper[_, _], _)        => if (second.hasStep) new OfDouble(PassedArray.two(first, second)) else first
		case (_, _ :LazyStepper[_, _])        => if (first.hasStep) new OfDouble(PassedArray.two(first, second)) else second
		case _ if !first.hasStep              => second
		case _ if !second.hasStep             => first
		case (cat1 :OfDouble, cat2 :OfDouble) => new OfDouble(cat1.parts ++ cat2.parts)
		case (cat1 :OfDouble, _)              => new OfDouble(cat1.parts :+ second)
		case (_, cat2 :OfDouble)              => new OfDouble(first +: cat2.parts)
		case _                                => new OfDouble(PassedArray.two(first, second))
	}

	def ofDouble(steppers :Seq[DoubleStepper]) :DoubleStepper =
		new OfDouble(steppers.toIndexedSeq)

	@SerialVersionUID(Ver)
	private class OfAny[A](steppers :IndexedSeq[Stepper[A]])
		extends ConcatStepper[A, A, Stepper[A], Spliterator[A], JIterator[A]](steppers) with AnyStepper[A]
	{
		override def trySplit() = super[ConcatStepper].trySplit().asInstanceOf[AnyStepper[A] with Spliterator[A]]
		override def spliterator[U >: A] = spliteratorImpl.asInstanceOf[Spliterator[U]]
		override def javaIterator[U >: A] = javaIteratorImpl.asInstanceOf[JIterator[U]]

		override def tryAdvance(action :Consumer[_ >: A]) = {
			val s = current
			s.hasStep && { action.accept(s.nextStep()); true }
		}
	}

	private trait OfPrimitive[A, B, S >: Null <: Stepper[A], J >: Null <: Spliterator[B], I >: Null <: JIterator[B]]
		extends ConcatStepper[A, B, S, J, I]
	{ this :S with J with I =>
	//	final override def forEachRemaining(action :Consumer[_ >: B]) :Unit = super[ConcatStepper].forEachRemaining(action)
	//	override def next() = nextStep().asInstanceOf[B]
		final override def spliterator[U >: A]  :J = spliteratorImpl
		final override def javaIterator[U >: A] :I = javaIteratorImpl
	}

	@SerialVersionUID(Ver)
	private class OfInt(steppers :IndexedSeq[IntStepper])
		extends ConcatStepper[Int, JInt, IntStepper, Spliterator.OfInt, PrimitiveIterator.OfInt](steppers)
		   with IntStepper with Spliterator.OfInt with PrimitiveIterator.OfInt
		   with OfPrimitive[Int, JInt, IntStepper, Spliterator.OfInt, PrimitiveIterator.OfInt]
	{
		override def nextStep() :Int = current.nextStep()
		override def nextInt()  :Int = current.nextStep() //currentJavaIterator.nextInt()
		override def tryAdvance(action :IntConsumer) = {
			val s = current
			s.hasStep && { action.accept(s.nextStep()); true }
		}
		override def forEachRemaining(action :IntConsumer) :Unit =
			while (hasStep)
				currentSpliterator.forEachRemaining(action)

		override def forEachRemaining(action :Consumer[_ >: JInt]) :Unit = action match {
			case int :IntConsumer => forEachRemaining(int :IntConsumer)
			case _                => super[ConcatStepper].forEachRemaining(action)
		}
		override def toString :String = parts.mkString("++[Int]")
	}

	@SerialVersionUID(Ver)
	private class OfLong(steppers :IndexedSeq[LongStepper])
		extends ConcatStepper[Long, JLong, LongStepper, Spliterator.OfLong, PrimitiveIterator.OfLong](steppers)
		   with LongStepper with Spliterator.OfLong with PrimitiveIterator.OfLong
		   with OfPrimitive[Long, JLong, LongStepper, Spliterator.OfLong, PrimitiveIterator.OfLong]
	{
		override def nextStep() :Long = current.nextStep()
		override def nextLong() :Long= current.nextStep()
		override def tryAdvance(action :LongConsumer) = {
			val s = current
			s.hasStep && { action.accept(s.nextStep()); true }
		}
		override def forEachRemaining(action :LongConsumer) :Unit =
			while (hasStep)
				currentSpliterator.forEachRemaining(action)

		override def forEachRemaining(action :Consumer[_ >: JLong]) :Unit = action match {
			case long :LongConsumer => forEachRemaining(long :LongConsumer)
			case _ => super[ConcatStepper].forEachRemaining(action)
		}
		override def toString :String = parts.mkString("++[Long]")
	}

	@SerialVersionUID(Ver)
	private class OfDouble(steppers :IndexedSeq[DoubleStepper])
		extends ConcatStepper[Double, JDouble, DoubleStepper, Spliterator.OfDouble, PrimitiveIterator.OfDouble](steppers)
		   with DoubleStepper with Spliterator.OfDouble with PrimitiveIterator.OfDouble
		   with OfPrimitive[Double, JDouble, DoubleStepper, Spliterator.OfDouble, PrimitiveIterator.OfDouble]
	{
		override def nextStep()   :Double = current.nextStep()
		override def nextDouble() :Double = current.nextStep()
		override def tryAdvance(action :DoubleConsumer) = currentSpliterator.tryAdvance(action)
		override def forEachRemaining(action :DoubleConsumer) :Unit =
			while (hasStep)
				currentSpliterator.forEachRemaining(action)

		override def forEachRemaining(action :Consumer[_ >: JDouble]) :Unit = action match {
			case double :DoubleConsumer => forEachRemaining(double :DoubleConsumer)
			case _ => current
		}
		override def toString :String = parts.mkString("++[Double]")
	}

	private final val AllCharacteristics = ORDERED | DISTINCT | SIZED | NONNULL | IMMUTABLE | CONCURRENT | SUBSIZED
}




private class LazyStepper[@specialized(JavaIterator.Types) +A, +S <: Stepper[A]](init: => S) extends Stepper[A] {
	private[this] var underlying :S = _
	
	@inline final def stepper :S = {
		if (underlying == null)
			underlying = init
		underlying
	}
	final def isEvaluated :Boolean = underlying != null
	override def estimateSize :Long = stepper.estimateSize
	override def characteristics :Int = stepper.characteristics
	override def spliterator[B >: A] :Spliterator[_] = stepper.spliterator
	override def javaIterator[B >: A] :JavaIterator[_] = stepper.javaIterator
	override def hasStep :Boolean = stepper.hasStep
	override def nextStep() :A = stepper.nextStep()

	override def trySplit() :Stepper[A] = stepper.trySplit()

	override def toString :String =
		if (underlying == null) "LazyStepper@" + this.identityHashCode else underlying.toString
}


private object LazyStepper {
	def apply[A, S <: Stepper[_]](stepper: => S)(implicit elemType :StepType[A, S]) :S =
		elemType.shape match {
			case ReferenceShape => new LazyStepper[A, Stepper[A]](stepper.asInstanceOf[Stepper[A]]).castFrom[Stepper[A], S]
			case IntShape       => new OfInt(stepper.asInstanceOf[IntStepper]).castFrom[IntStepper, S]
			case LongShape      => new OfLong(stepper.asInstanceOf[LongStepper]).castFrom[LongStepper, S]
			case DoubleShape    => new OfDouble(stepper.asInstanceOf[DoubleStepper]).castFrom[DoubleStepper, S]
			case FloatShape     => new OfDouble(stepper.asInstanceOf[DoubleStepper]).castFrom[DoubleStepper, S]
			case ByteShape | CharShape | ShortShape => new OfInt(stepper.asInstanceOf[IntStepper]).castFrom[IntStepper, S]
			case _              => new LazyStepper[A, Stepper[A]](stepper.asInstanceOf[Stepper[A]]).castFrom[Stepper[A], S]
		}

	def ofRef[A](stepper: => AnyStepper[A]) :AnyStepper[A] = new OfRef(stepper)
	def ofInt(stepper: => IntStepper) :IntStepper = new OfInt(stepper)
	def ofLong(stepper: => LongStepper) :LongStepper = new OfLong(stepper)
	def ofDouble(stepper: => DoubleStepper) :DoubleStepper = new OfDouble(stepper)
	
	class OfRef[+A](init: => AnyStepper[A]) extends LazyStepper[A, AnyStepper[A]](init) with AnyStepper[A] {
		override def spliterator[B >: A] = stepper.spliterator
		override def javaIterator[B >: A] = stepper.javaIterator
		override def trySplit() = stepper.trySplit()
	}
	class OfInt(init: => IntStepper) extends LazyStepper[Int, IntStepper](init) with IntStepper {
		override def spliterator[B >: Int] = stepper.spliterator
		override def javaIterator[B >: Int] = stepper.javaIterator
		override def trySplit() = stepper.trySplit()
	}
	class OfLong(init: => LongStepper) extends LazyStepper[Long, LongStepper](init) with LongStepper {
		override def spliterator[B >: Long] = stepper.spliterator
		override def javaIterator[B >: Long] = stepper.javaIterator
		override def trySplit() = stepper.trySplit()
	}
	class OfDouble(init: => DoubleStepper) extends LazyStepper[Double, DoubleStepper](init) with DoubleStepper {
		override def spliterator[B >: Double] = stepper.spliterator
		override def javaIterator[B >: Double] = stepper.javaIterator
		override def trySplit() = stepper.trySplit()
	}
}




private abstract class SpliteratorStepper[@specialized(JavaIterator.Types) A, B,
                                          S >: Null <: Stepper[A], J >: Null <: Spliterator[B], I <: JIterator[B]]
                                         (underlying :J)
	extends Stepper[A] with JIterator[B] with Consumer[B]
{
	private[this] var hd :A = _
	private[this] var hasHead = false
	@inline protected final def head :A = if (hasHead) hd else throw new NoSuchElementException("Stepper.empty")
	protected final def head_=(value :A) :Unit = { hd = value; hasHead = true }

	override def estimateSize :Long = underlying.estimateSize
	override def characteristics :Int = underlying.characteristics
	override def spliterator[U >: A] :Spliterator[_] = underlying
	protected def specificSpliterator :J = underlying
//	override def javaIterator[U >: A] :JavaIterator[_] = JavaIterator(underlying)
	override def accept(t :B) :Unit = { hd = unbox(t); hasHead = true }

	override def hasStep :Boolean = underlying.tryAdvance(this)
	override def hasNext :Boolean = underlying.tryAdvance(this)

	override def nextStep() :A = {
		if (!hasHead) {
			buffer(underlying)
			if (!hasHead)
				throw new NoSuchElementException("Stepper.empty")
		}
		hasHead = false
		hd
	}
	override def next() :B = box(nextStep())

	override def forEachRemaining(action :Consumer[_ >: B]) :Unit = {
		if (hasHead) {
			action.accept(box(hd))
			hasHead = false
		}
		underlying.forEachRemaining(action)
	}

	protected def buffer(spliterator :J) :Unit = spliterator.tryAdvance(this)
	protected def box(elem :A) :B
	protected def unbox(elem :B) :A
}


private object SpliteratorStepper {
	def apply[A](spliterator :Spliterator[A]) :Stepper[A] = spliterator match {
		case split :Spliterator.OfInt    => new OfInt(split).castFrom[IntStepper, Stepper[A]]
		case split :Spliterator.OfLong   => new OfLong(split).castFrom[LongStepper, Stepper[A]]
		case split :Spliterator.OfDouble => new OfDouble(split).castFrom[DoubleStepper, Stepper[A]]
		case _                           => new OfRef(spliterator)
	}
	def ofRef[A](spliterator :Spliterator[A]) :AnyStepper[A] = new OfRef(spliterator)
	def ofInt[A](spliterator :Spliterator.OfInt) :IntStepper = new OfInt(spliterator)
	def ofLong[A](spliterator :Spliterator.OfLong) :LongStepper = new OfLong(spliterator)
	def ofDouble[A](spliterator :Spliterator.OfDouble) :DoubleStepper = new OfDouble(spliterator)

	private class OfRef[A](underlying :Spliterator[A])
		extends SpliteratorStepper[A, A, AnyStepper[A], Spliterator[A], JIterator[A]](underlying)
		   with AnyStepper[A] with JIterator[A]
	{
		override def spliterator[U >: A] :Spliterator[U] = specificSpliterator.castParam[U]
		override def javaIterator[U >: A] :JIterator[U] = this.castParam[U]

		override def trySplit() :AnyStepper[A] = {
			val split = spliterator.trySplit
			if (split == null) null else new OfRef(split)
		}
		protected override def box(elem :A) :A = elem
		protected override def unbox(elem :A) :A = elem
	}

	private trait OfPrimitive
	              [A, B, S >: Null <: Stepper[A], J >: Null <: Spliterator.OfPrimitive[B, C, J], I <: JIterator[B], C]
		extends SpliteratorStepper[A, B, S, J, I] with PrimitiveIterator[B, C]
	{ this :C with I =>
		override def spliterator[U >: A] :J = specificSpliterator
		override def javaIterator[U >: A] :I = this

		override def forEachRemaining(action :C) :Unit =
			if (hasStep) {
				forHead(action)
				specificSpliterator.forEachRemaining(action)
			}
		protected def forHead(action :C) :Unit
		protected override def buffer(spliterator :J) :Unit = spliterator.tryAdvance(this)
	}

	private class OfInt(underlying :Spliterator.OfInt)
		extends SpliteratorStepper[Int, JInt, IntStepper, Spliterator.OfInt, JIntIterator](underlying)
		   with IntStepper with JIntIterator with IntConsumer
		   with OfPrimitive[Int, JInt, IntStepper, Spliterator.OfInt, JIntIterator, IntConsumer]
	{
		override def nextInt() = nextStep()

		override def trySplit() = {
			val split = specificSpliterator.trySplit()
			if (split == null) null else new OfInt(split)
		}

		override def accept(value :Int) :Unit = head = value

		protected override def forHead(action :IntConsumer) :Unit = action.accept(nextStep())
		protected override def box(elem :Int) = elem
		protected override def unbox(elem :JInt) = elem
	}

	private class OfLong(underlying :Spliterator.OfLong)
		extends SpliteratorStepper[Long, JLong, LongStepper, Spliterator.OfLong, JLongIterator](underlying)
		   with LongStepper with JLongIterator with LongConsumer
		   with OfPrimitive[Long, JLong, LongStepper, Spliterator.OfLong, JLongIterator, LongConsumer]
	{
		override def nextLong() = nextStep()

		override def trySplit() = {
			val split = specificSpliterator.trySplit()
			if (split == null) null else new OfLong(split)
		}

		override def accept(value :Long) :Unit = head = value

		protected override def forHead(action :LongConsumer) :Unit = action.accept(nextStep())
		protected override def box(elem :Long) = elem
		protected override def unbox(elem :JLong) = elem
	}

	private class OfDouble(underlying :Spliterator.OfDouble)
		extends SpliteratorStepper[Double, JDouble, DoubleStepper, Spliterator.OfDouble, JDoubleIterator](underlying)
		   with DoubleStepper with JDoubleIterator with DoubleConsumer
		   with OfPrimitive[Double, JDouble, DoubleStepper, Spliterator.OfDouble, JDoubleIterator, DoubleConsumer]
	{
		override def nextDouble() = nextStep()

		override def trySplit() = {
			val split = specificSpliterator.trySplit()
			if (split == null) null else new OfDouble(split)
		}

		override def accept(value :Double) :Unit = head = value

		protected override def forHead(action :DoubleConsumer) :Unit = action.accept(nextStep())
		protected override def box(elem :Double) = elem
		protected override def unbox(elem :JDouble) = elem
	}
}
