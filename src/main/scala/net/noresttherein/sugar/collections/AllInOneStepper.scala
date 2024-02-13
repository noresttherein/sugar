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
  * @see [[net.noresttherein.sugar.collections.AllInOneAnyStepper]]
  * @see [[net.noresttherein.sugar.collections.AllInOneIntStepper]]
  * @see [[net.noresttherein.sugar.collections.AllInOneLongStepper]]
  * @see [[net.noresttherein.sugar.collections.AllInOneDoubleStepper]]
  */
private trait AllInOneStepper[+A, B, +Self >: Null <: AllInOneStepper[A, B, Self]]
	extends Stepper[A] with Spliterator[B] with JavaIterator[B]
{
	override def hasNext :Boolean = hasStep
//	override def next() :B = nextStep().asInstanceOf[B]
	override def forEachRemaining(action :Consumer[_ >: B]) :Unit =
		while (tryAdvance(action)) {}

	override def tryAdvance(action :Consumer[_ >: B]) :Boolean =
		hasStep && { action.accept(next()); true }

	override def trySplit() :Self = null
}


private trait AllInOneAnyStepper[A] extends AnyStepper[A] with AllInOneStepper[A, A, AllInOneAnyStepper[A]] {
	override def next() :A = nextStep()
	override def spliterator[U >: A]  :Spliterator[U] = this.asInstanceOf[Spliterator[U]]
	override def javaIterator[U >: A] :JavaIterator[U]   = this.asInstanceOf[JavaIterator[U]]
}


private trait AllInOnePrimitiveStepper[+A, B, +Self >: Null <: AllInOneStepper[A, B, Self],
                                       S <: Spliterator[B], I <: JavaIterator[B]]
	extends AllInOneStepper[A, B, Self]
{ this :Self with S with I =>
	override def forEachRemaining(action :Consumer[_ >: B]) :Unit = super[AllInOneStepper].forEachRemaining(action)
	override def spliterator[U >: A] :S  = this
	override def javaIterator[U >: A] :I = this
	abstract override def characteristics = super.characteristics | NONNULL
}


private trait AllInOneIntStepper
	extends IntStepper with Spliterator.OfInt with JavaIntIterator
	   with AllInOnePrimitiveStepper[Int, JInt, AllInOneIntStepper, Spliterator.OfInt, JavaIntIterator]
{   //consider: AllInOneStepper might become a self type
	override def nextInt() :Int = nextStep() //Stepper is specialized, so this doesn't box
	override def forEachRemaining(action :Consumer[_ >: JInt]) :Unit = action match {
		case consumer :IntConsumer => forEachRemaining(consumer :IntConsumer)
		case _ => super[AllInOnePrimitiveStepper].forEachRemaining(action)
	}
	override def forEachRemaining(action :IntConsumer) :Unit =
		while (hasStep)
			action.accept(nextInt())
//		while (tryAdvance(action)) {}

	override def tryAdvance(action :IntConsumer) :Boolean =
		hasStep && { action.accept(nextInt()); true }
}


private trait AllInOneLongStepper
	extends LongStepper with Spliterator.OfLong with JavaLongIterator
	   with AllInOnePrimitiveStepper[Long, JLong, AllInOneLongStepper, Spliterator.OfLong, JavaLongIterator]
{
	override def nextLong() :Long = nextStep() //Stepper is specialized, so this doesn't box
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


private trait AllInOneDoubleStepper
	extends DoubleStepper with Spliterator.OfDouble with JavaDoubleIterator
	   with AllInOnePrimitiveStepper[Double, JDouble, AllInOneDoubleStepper, Spliterator.OfDouble, JavaDoubleIterator]
{
	override def nextDouble() :Double = nextStep() //Stepper is specialized, so this doesn't box
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




//private trait ErasedAllInStepper[+A, B, +S >: Null <: AllInOneStepper[A, B, S]] extends AllInOneStepper[A, B, S] {
//	override def next() :B = nextStep().asInstanceOf[B]
//}

/** A variant of [[net.noresttherein.sugar.collections.AllInOneIntStepper AllInOneIntStepper]] for steppers
  * over sources providing already boxed values (such as Scala collections).
  * In order to avoid unnecessary unboxing and boxing, it should be mixed in to a generic, non specialized
  * base class implementing `nextStep()`.
  */
private trait BoxedAllInOneIntStepper
	extends IntStepper with Spliterator.OfInt with JavaIntIterator
		with AllInOnePrimitiveStepper[Int, JInt, BoxedAllInOneIntStepper, Spliterator.OfInt, JavaIntIterator]
//		with ErasedAllInStepper[Int, JInt, BoxedAllInOneIntStepper]
{ 
	override def nextInt() :Int = nextStep()
	override def next() :JInt = nextStep()
	override def tryAdvance(action :IntConsumer) :Boolean =
		hasStep && { action.accept(nextStep()); true }

	override def forEachRemaining(action :IntConsumer) :Unit =
		while (hasStep)
			action.accept(nextStep())
}

/** A variant of [[net.noresttherein.sugar.collections.AllInOneLongStepper AllInOneLongStepper]] for steppers
  * over sources providing already boxed values (such as Scala collections).
  * In order to avoid unnecessary unboxing and boxing, it should be mixed in to a generic, non specialized
  * base class implementing `nextStep()`.
  */
private trait BoxedAllInOneLongStepper
	extends LongStepper with Spliterator.OfLong with JavaLongIterator
		with AllInOnePrimitiveStepper[Long, JLong, BoxedAllInOneLongStepper, Spliterator.OfLong, JavaLongIterator]
//		with ErasedAllInStepper[Long, JLong, BoxedAllInOneLongStepper]
{ 
	override def nextLong() :Long = nextStep()
	override def next() :JLong = nextStep()
	override def tryAdvance(action :LongConsumer) :Boolean =
		hasStep && { action.accept(nextStep()); true }

	override def forEachRemaining(action :LongConsumer) :Unit =
		while (hasStep)
			action.accept(nextStep())
}

/** A variant of [[net.noresttherein.sugar.collections.AllInOneDoubleStepper AllInOneDoubleStepper]] for steppers
  * over sources providing already boxed values (such as Scala collections).
  * In order to avoid unnecessary unboxing and boxing, it should be mixed in to a generic, non specialized
  * base class implementing `nextStep()`.
  */
private trait BoxedAllInOneDoubleStepper
	extends DoubleStepper with Spliterator.OfDouble with JavaDoubleIterator
		with AllInOnePrimitiveStepper[Double, JDouble, BoxedAllInOneDoubleStepper, Spliterator.OfDouble, JavaDoubleIterator]
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
