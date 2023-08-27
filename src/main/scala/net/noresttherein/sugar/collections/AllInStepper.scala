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
	override def nextStep() :A = next().asInstanceOf[A]
	override def forEachRemaining(action :Consumer[_ >: B]) :Unit = {
		estimateSize match {
			case -1 =>
				while (tryAdvance(action)) {}
			case n  =>
				var remaining = n
				while (remaining > 0) {
					action.accept(next())
					remaining -= 1
				}
		}
	}
	override def tryAdvance(action :Consumer[_ >: B]) :Boolean =
		hasStep && { action.accept(next()); true }

	override def trySplit() :Self = null
}


private trait AllInAnyStepper[A] extends AnyStepper[A] with AllInStepper[A, A, AllInAnyStepper[A]] {
	override def spliterator[U >: A]  :Spliterator[U] = this.asInstanceOf[Spliterator[U]]
	override def javaIterator[U >: A] :JIterator[U]   = this.asInstanceOf[JIterator[U]]
}


private trait AllInPrimitiveStepper[+A, B, +Self >: Null <: AllInStepper[A, B, Self], S <: Spliterator[B], I <: JIterator[B]]
	extends AllInStepper[A, B, Self]
{ this :Self with S with I =>
	override def forEachRemaining(action :Consumer[_ >: B]) :Unit = super[AllInStepper].forEachRemaining(action)
	override def spliterator[U >: A] :S  = this
	override def javaIterator[U >: A] :I = this
	//scala complains if we have it here
	abstract override def characteristics = super.characteristics | NONNULL
}


private trait AllInIntStepper
	extends IntStepper with Spliterator.OfInt with JIntIterator
	   with AllInPrimitiveStepper[Int, JInt, AllInIntStepper, Spliterator.OfInt, JIntIterator]
{   //consider: AllInStepper might become a self type
	override def forEachRemaining(action :Consumer[_ >: JInt]) :Unit = action match {
		case consumer :IntConsumer => forEachRemaining(consumer :IntConsumer)
		case _ => super[AllInPrimitiveStepper].forEachRemaining(action)
	}
	override def forEachRemaining(action :IntConsumer) :Unit = estimateSize match {
		case -1 =>
			while (tryAdvance(action)) {}
		case n =>
			var remaining = n
			while (remaining > 0) {
				action.accept(nextInt())
				remaining -= 1
			}
	}
	override def tryAdvance(action :IntConsumer) :Boolean =
		hasStep && { action.accept(nextInt()); true }

//	abstract override def characteristics = super.characteristics | NONNULL
}


private trait AllInLongStepper
	extends LongStepper with Spliterator.OfLong with JLongIterator
	   with AllInPrimitiveStepper[Long, JLong, AllInLongStepper, Spliterator.OfLong, JLongIterator]
{
	override def forEachRemaining(action :Consumer[_ >: JLong]) :Unit = action match {
		case consumer :LongConsumer => forEachRemaining(consumer :LongConsumer)
		case _ => super.forEachRemaining(action)
	}
	override def forEachRemaining(action :LongConsumer) :Unit = estimateSize match {
		case -1 =>
			while (tryAdvance(action)) {}
		case n =>
			var remaining = n
			while (remaining > 0) {
				action.accept(nextLong())
				remaining -= 1
			}
	}
	override def tryAdvance(action :LongConsumer) :Boolean =
		hasStep && { action.accept(nextLong()); true }

//	abstract override def characteristics = super.characteristics | NONNULL
}


private trait AllInDoubleStepper
	extends DoubleStepper with Spliterator.OfDouble with JDoubleIterator
	   with AllInPrimitiveStepper[Double, JDouble, AllInDoubleStepper, Spliterator.OfDouble, JDoubleIterator]
{
	override def forEachRemaining(action :Consumer[_ >: JDouble]) :Unit = action match {
		case consumer :DoubleConsumer => forEachRemaining(consumer :DoubleConsumer)
		case _ => super.forEachRemaining(action)
	}
	override def forEachRemaining(action :DoubleConsumer) :Unit = estimateSize match {
		case -1 =>
			while (tryAdvance(action)) {}
		case n =>
			var remaining = n
			while (remaining > 0) {
				action.accept(nextDouble())
				remaining -= 1
			}
	}
	override def tryAdvance(action :DoubleConsumer) :Boolean =
		hasStep && { action.accept(nextDouble()); true }

//	abstract override def characteristics = super.characteristics | NONNULL
}
