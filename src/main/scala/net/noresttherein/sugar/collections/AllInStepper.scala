package net.noresttherein.sugar.collections

import java.util.Spliterator
import java.util.Spliterator.NONNULL
import java.util.function.{Consumer, DoubleConsumer, IntConsumer, LongConsumer}

import scala.collection.{AnyStepper, DoubleStepper, IntStepper, LongStepper, Stepper}

import net.noresttherein.sugar.JavaTypes.{JDouble, JDoubleIterator, JInt, JIntIterator, JIterator, JLong, JLongIterator}




/** A skeleton implementation for a `Stepper` which at the same time is its own
  * [[java.util.Spliterator Spliterator]] and java [[java.util.Iterator Iterator]].
  * There are several extending traits, one for each manual specialization available for these interfaces
  * plus one for reference types (or boxing). They are designed to be mixed in last, as they resolve conflicts arising
  * from multiple implementations of shared methods.
  * @tparam A The Scala element type iterated over by this stepper.
  * @tparam B The Java class for type A. For a reference type, this is simply `A`.
  *           For built in value types (`Int`, `Long` and `Double`) this is the class of the Java boxing class.
  * @see [[net.noresttherein.sugar.collections.AllInAnyStepper]]
  * @see [[net.noresttherein.sugar.collections.AllInIntStepper]]
  * @see [[net.noresttherein.sugar.collections.AllInLongStepper]]
  * @see [[net.noresttherein.sugar.collections.AllInDoubleStepper]]
  */
private trait AllInStepper[+A, B, +Self >: Null <: AllInStepper[A, B, Self]]
	extends Stepper[A] with Spliterator[B] with JIterator[B]
{
	override def hasNext :Boolean = hasStep
//	override def next() :B = nextStep().asInstanceOf[B]
	override def forEachRemaining(action :Consumer[_ >: B]) :Unit =
		while (tryAdvance(action)) {}

	override def tryAdvance(action :Consumer[_ >: B]) :Boolean =
		hasStep && { action.accept(next()); true }

	override def trySplit() :Self = null
}


private trait AllInAnyStepper[A] extends AnyStepper[A] with AllInStepper[A, A, AllInAnyStepper[A]] {
	override def next() :A = nextStep()
	override def spliterator[U >: A]  :Spliterator[U] = this.asInstanceOf[Spliterator[U]]
	override def javaIterator[U >: A] :JIterator[U]   = this.asInstanceOf[JIterator[U]]
}


private trait AllInPrimitiveStepper[+A, B, +Self >: Null <: AllInStepper[A, B, Self], S <: Spliterator[B], I <: JIterator[B]]
	extends AllInStepper[A, B, Self]
{ this :Self with S with I =>
	override def forEachRemaining(action :Consumer[_ >: B]) :Unit = super[AllInStepper].forEachRemaining(action)
	override def spliterator[U >: A] :S  = this
	override def javaIterator[U >: A] :I = this
	abstract override def characteristics = super.characteristics | NONNULL
}


private trait AllInIntStepper
	extends IntStepper with Spliterator.OfInt with JIntIterator
	   with AllInPrimitiveStepper[Int, JInt, AllInIntStepper, Spliterator.OfInt, JIntIterator]
{   //consider: AllInStepper might become a self type
//	override def nextStep() :Int //specialize the method
	override def nextInt() :Int = nextStep()
	override def forEachRemaining(action :Consumer[_ >: JInt]) :Unit = action match {
		case consumer :IntConsumer => forEachRemaining(consumer :IntConsumer)
		case _ => super[AllInPrimitiveStepper].forEachRemaining(action)
	}
	override def forEachRemaining(action :IntConsumer) :Unit =
		while (hasStep)
			action.accept(nextInt())
//		while (tryAdvance(action)) {}

	override def tryAdvance(action :IntConsumer) :Boolean =
		hasStep && { action.accept(nextInt()); true }
}


private trait AllInLongStepper
	extends LongStepper with Spliterator.OfLong with JLongIterator
	   with AllInPrimitiveStepper[Long, JLong, AllInLongStepper, Spliterator.OfLong, JLongIterator]
{
//	override def nextStep() :Long //specialize the method
	override def nextLong() :Long = nextStep()
	override def forEachRemaining(action :Consumer[_ >: JLong]) :Unit = action match {
		case consumer :LongConsumer => forEachRemaining(consumer :LongConsumer)
		case _ => super.forEachRemaining(action)
	}
	override def forEachRemaining(action :LongConsumer) :Unit =
		while (hasStep)
			action.accept(nextLong())
//		while (tryAdvance(action)) {}

	override def tryAdvance(action :LongConsumer) :Boolean =
		hasStep && { action.accept(nextLong()); true }
}


private trait AllInDoubleStepper
	extends DoubleStepper with Spliterator.OfDouble with JDoubleIterator
	   with AllInPrimitiveStepper[Double, JDouble, AllInDoubleStepper, Spliterator.OfDouble, JDoubleIterator]
{
//	override def nextStep() :Double //specialize the method
	override def nextDouble() :Double = nextStep()
	override def forEachRemaining(action :Consumer[_ >: JDouble]) :Unit = action match {
		case consumer :DoubleConsumer => forEachRemaining(consumer :DoubleConsumer)
		case _ => super.forEachRemaining(action)
	}
	override def forEachRemaining(action :DoubleConsumer) :Unit =
		while (hasStep)
			action.accept(nextDouble())
//		while (tryAdvance(action)) {}

	override def tryAdvance(action :DoubleConsumer) :Boolean =
		hasStep && { action.accept(nextDouble()); true }
}




//private trait ErasedAllInStepper[+A, B, +S >: Null <: AllInStepper[A, B, S]] extends AllInStepper[A, B, S] {
//	override def next() :B = nextStep().asInstanceOf[B]
//}

/** A variant of [[net.noresttherein.sugar.collections.Stepper.AllInIntStepper AllInIntStepper]] for steppers
  * over sources providing already boxed values (such as Scala collections).
  * In order to avoid unnecessary unboxing and boxing, it should be mixed in to a generic, non specialized
  * base class implementing `nextStep()`.
  */
private trait BoxedAllInIntStepper
	extends IntStepper with Spliterator.OfInt with JIntIterator
		with AllInPrimitiveStepper[Int, JInt, BoxedAllInIntStepper, Spliterator.OfInt, JIntIterator]
//		with ErasedAllInStepper[Int, JInt, BoxedAllInIntStepper]
{ 
	override def nextInt() :Int = nextStep()
	override def next() :JInt = nextStep()
	override def tryAdvance(action :IntConsumer) :Boolean =
		hasStep && { action.accept(nextStep()); true }

	override def forEachRemaining(action :IntConsumer) :Unit =
		while (hasStep)
			action.accept(nextStep())
}

/** A variant of [[net.noresttherein.sugar.collections.Stepper.AllInLongStepper AllInLongStepper]] for steppers
  * over sources providing already boxed values (such as Scala collections).
  * In order to avoid unnecessary unboxing and boxing, it should be mixed in to a generic, non specialized
  * base class implementing `nextStep()`.
  */
private trait BoxedAllInLongStepper
	extends LongStepper with Spliterator.OfLong with JLongIterator
		with AllInPrimitiveStepper[Long, JLong, BoxedAllInLongStepper, Spliterator.OfLong, JLongIterator]
//		with ErasedAllInStepper[Long, JLong, BoxedAllInLongStepper]
{ 
	override def nextLong() :Long = nextStep()
	override def next() :JLong = nextStep()
	override def tryAdvance(action :LongConsumer) :Boolean =
		hasStep && { action.accept(nextStep()); true }

	override def forEachRemaining(action :LongConsumer) :Unit =
		while (hasStep)
			action.accept(nextStep())
}

/** A variant of [[net.noresttherein.sugar.collections.Stepper.AllInDoubleStepper AllInDoubleStepper]] for steppers
  * over sources providing already boxed values (such as Scala collections).
  * In order to avoid unnecessary unboxing and boxing, it should be mixed in to a generic, non specialized
  * base class implementing `nextStep()`.
  */
private trait BoxedAllInDoubleStepper
	extends DoubleStepper with Spliterator.OfDouble with JDoubleIterator
		with AllInPrimitiveStepper[Double, JDouble, BoxedAllInDoubleStepper, Spliterator.OfDouble, JDoubleIterator]
//		with ErasedAllInStepper[Double, JDouble, BoxedAllInDoubleStepper]
{ 
	override def nextDouble() :Double = nextStep()
	override def next() :JDouble = nextDouble()
	override def tryAdvance(action :DoubleConsumer) :Boolean =
		hasStep && { action.accept(nextStep()); true }

	override def forEachRemaining(action :DoubleConsumer) :Unit =
		while (hasStep)
			action.accept(nextStep())
}
