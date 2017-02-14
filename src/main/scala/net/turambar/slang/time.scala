package net.turambar.slang

import java.time.{Instant, Duration}
import java.util.concurrent.TimeUnit


import scala.Ordering.Implicits.infixOrderingOps
import scala.concurrent.duration
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

/**
  * @author Marcin Mościcki
  */
/*
object time {

	/** A period of time, serving as a light wrapper over `java.time.Duration` providing nicer syntax. */
	implicit class Spell(val duration :Duration) extends AnyVal {
		@inline def -(other :Spell) :Spell = duration minus other.duration
		@inline def +(other :Spell) :Spell = duration plus other.duration
		@inline def /(divisor :Long) :Spell = duration dividedBy divisor
		@inline def ~/(other :Spell) :Long = duration.toNanos / other.toNanos

		@inline def *(multiplicand :Long) :Spell = duration multipliedBy multiplicand

		@inline def fromNow :Moment = Moment.now + this

		@inline def ago :Moment = Moment.now - this

		@inline def since(moment :Moment) :Moment = moment + this

		@inline def before(moment :Moment) :Moment = moment - this

		@inline def toNanos = duration.toNanos
		@inline def toMicros = duration.toNanos/1000
		@inline def toMillis = duration.toMillis
		@inline def toSeconds = duration.toMillis/1000
		@inline def toMinutes = duration.toMinutes
		@inline def toHours = duration.toHours

		@inline def isNegative = duration.isNegative
		@inline def isPositive = !(duration.isNegative || duration.isZero)
		@inline def isZero = duration.isZero

//		@inline def asScala :FiniteDuration =
//			FiniteDuration(duration.getSeconds, TimeUnit.SECONDS) + FiniteDuration(duration.getNano, TimeUnit.NANOSECONDS)
		@inline def asScala :FiniteDuration = try {
			duration.toNanos match {
				case nanos if nanos % 1000 != 0 => FiniteDuration(duration.toNanos, TimeUnit.NANOSECONDS)
				case nanos if nanos % 1000000 !=0 => FiniteDuration(duration.toMicros, TimeUnit.MICROSECONDS)
				case nanos if nanos % 1000000000L !=0 => FiniteDuration(duration.toMillis, TimeUnit.MILLISECONDS)
				case nanos if nanos % 1000000000000L != 0 => FiniteDuration(duration.toSeconds, TimeUnit.SECONDS)
				case nanos if nanos % 60000000000000L != 0 => FiniteDuration(duration.toMinutes, TimeUnit.MINUTES)
				case nanos if nanos % 3600000000000000L != 0 => FiniteDuration(duration.toHours, TimeUnit.HOURS)
				case nanos if nanos % (24*3600000000000000L) !=0 => FiniteDuration(duration.toDays,  TimeUnit.DAYS)
				case _ => FiniteDuration(duration.getSeconds, TimeUnit.SECONDS) + FiniteDuration(duration.getNano, TimeUnit.NANOSECONDS)
			}
		} catch {
			case _:ArithmeticException => FiniteDuration(duration.getSeconds, TimeUnit.SECONDS) + FiniteDuration(duration.getNano, TimeUnit.NANOSECONDS)
		}



//		override def equals(that :Any) = that match {
//			case p :Period => duration==p.duration
//			case d :Duration => duration == d
//			case _ => false
//		}
//
//		override def hashCode = duration.hashCode

		@inline override def toString = duration.toString
	}



	object Spell {
		val Zero :Spell = Duration.ZERO

		@inline def nanos(nanoSeconds :Long) :Spell = new Spell(Duration.ofNanos(nanoSeconds))
		@inline def micros(microSeconds :Long) :Spell = new Spell(Duration.ofNanos(microSeconds*1000))
		@inline def millis(milliSeconds :Long) :Spell = new Spell(Duration.ofMillis(milliSeconds))
		@inline def seconds(seconds :Long) :Spell = new Spell(Duration.ofSeconds(seconds))
		@inline def minutes(minutes :Long) :Spell = new Spell(Duration.ofMinutes(minutes))
		@inline def hours(hours  :Long) :Spell = new Spell(Duration.ofHours(hours))

		@inline def since(moment :Moment) :Spell = Moment.now - moment

		@inline def until(moment :Moment) :Spell = moment - Moment.now

		@inline def millisUntil(moment :Moment) :Long = moment.toEpochMilli - System.currentTimeMillis

		@inline implicit def FromScala(dur :FiniteDuration) :Spell = new Spell(Duration.ofNanos(dur.toNanos))

		@inline implicit def ToScala(period :Spell) :FiniteDuration = period.asScala

		@inline implicit def AsJavaDuration(dur :Spell) :Duration = dur.duration

//		@inline implicit def AsScalaDuration(dur :Period) :FiniteDuration = dur.asScala

		implicit object ordering extends Ordering[Spell] {
			override def compare(x: Spell, y: Spell): Int =
				implicitly[Ordering[Duration]].compare(x.duration, y.duration)
		}


	}


	/** A precise moment in time, serving as a light wrapper over `java.time.Instant`, but providing nicer scala syntax. */
	implicit class Moment(val instant :Instant) extends AnyVal {
		def - (other :Moment) :Spell = Duration.between(other, this)
		def - (other :Spell) :Instant = instant minus other.duration
		def + (period :Spell) :Instant = instant plus period.duration

		@inline override def toString = instant.toString

		@inline def isPast = instant.toEpochMilli < System.currentTimeMillis
		@inline def isFuture = instant.toEpochMilli > System.currentTimeMillis

//		override def equals(that :Any) = that match {
//			case m :Moment => m.instant == instant
//			case i :Instant => instant == i
//			case _ => false
//		}
//
//		override def hashCode = instant.hashCode
	}


	object Moment {
		@inline def now :Moment = new Moment(Instant.now)

		val epochStart :Moment = Instant.ofEpochSecond(0)



		@inline implicit def AsJavaInstant(instant :Moment) :Instant = instant.instant


		implicit object ordering extends Ordering[Moment] {
			override def compare(x: Moment, y: Moment): Int =
				implicitly[Ordering[Instant]].compare(x.instant, y.instant)
		}
	}




	class Frequency(val hertz :Int) extends AnyVal {
		/** Returns `1s /hertz` as a Duration. Note that rounding will occur! */
		def period :Spell = 1.seconds / hertz

		def duration(samples :Int) :Spell = Spell.seconds(samples) / hertz
		def duration(samples :Long) :Spell = Spell.seconds(samples) / hertz

		def samples(duration :Spell) :Long = duration ~/ period //duration.toNanos * hertz / 1000000000

		@inline override def toString = s"$hertz hertz"
	}

	object Frequency {
		def hertz(rate :Int) :Frequency = new Frequency(rate)
		def none :Frequency = new Frequency(0)
	}


	@inline implicit def ImplicitTime(amount :Int) :ImplicitTime = new ImplicitTime(amount)
	@inline implicit def ImplicitTime(amount :Long) :ImplicitTime = new ImplicitTime(amount)

	class ImplicitTime(private val x :Long) extends AnyVal {
		@inline def hertz :Frequency = Frequency.hertz(x.toInt)

		@inline def nanos :Spell = Spell.nanos(x)
		@inline def micros :Spell = Spell.micros(x)
		@inline def millis :Spell = Spell.millis(x)
		@inline def seconds :Spell = Spell.seconds(x)
		@inline def minutes :Spell = Spell.minutes(x)
		@inline def hours :Spell = Spell.hours(x)
	}

}
*/
