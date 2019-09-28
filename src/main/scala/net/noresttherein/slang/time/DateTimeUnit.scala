package net.noresttherein.slang.time

import java.{time => j}
import java.time.temporal.{ChronoUnit, TemporalUnit}
import java.util.concurrent.{TimeUnit => JTimeUnit}

import DateTimeUnit._


//sealed trait BaseUnit

/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
sealed trait DateTimeUnit extends Any with Ordered[DateTimeUnit] with Serializable {
	def span :TimeSpan = toJava.getDuration
	def inNanos :Long = toJava.getDuration.toNanos

	def toJava :ChronoUnit

	@inline override def compare(that :DateTimeUnit) :Int = (that.inNanos - inNanos).signum



	//todo:
	def finer :Option[DateTimeUnit] = { //todo
		val duration = inNanos
		if (duration == 1L) None
		else if (duration < NanosInSecond)
			if (duration == NanosInMicro) SomeMicros
			else SomeMillis
		else if (duration < NanosInHour)
			if (duration == NanosInSecond) SomeSeconds
			else SomeMinutes
		else
			if (duration == NanosInHour) SomeHours
			else SomeDays
	}

	//todo:
	def coarser :Option[DateTimeUnit] = {
		val duration = inNanos
		if (duration == NanosInDay) None
		else if (duration < NanosInMilli)
			if (duration == 1L) SomeNanos
			else SomeMicros
		else if (duration < NanosInMinute)
			if (duration == NanosInMilli) SomeMillis
			else SomeSeconds
		else
			if (duration == NanosInMinute) SomeMinutes
			else SomeHours
	}


}






object DateTimeUnit {
	@inline final val Nanos = new TimeUnit(ChronoUnit.NANOS)
	@inline final val Micros = new TimeUnit(ChronoUnit.MICROS)
	@inline final val Millis = new TimeUnit(ChronoUnit.MILLIS)
	@inline final val Seconds = new TimeUnit(ChronoUnit.SECONDS)
	@inline final val Minutes = new TimeUnit(ChronoUnit.MINUTES)
	@inline final val Hours = new TimeUnit(ChronoUnit.HOURS)
	@inline final val HalfDays = new TimeUnit(ChronoUnit.HALF_DAYS)

	@inline final val Days = new DateUnit(ChronoUnit.DAYS)
	@inline final val Weeks = new DateUnit(ChronoUnit.WEEKS)
	@inline final val Months = new DateUnit(ChronoUnit.MONTHS)
	@inline final val Years = new DateUnit(ChronoUnit.YEARS)

	@inline final val Decades = new DateUnit(ChronoUnit.DECADES)
	@inline final val Centuries = new DateUnit(ChronoUnit.CENTURIES)
	@inline final val Millenia = new DateUnit(ChronoUnit.MILLENNIA)

	@inline final val Eras = new SpecialUnit(ChronoUnit.ERAS)
	@inline final val Forever = new SpecialUnit(ChronoUnit.FOREVER)


	@inline implicit def fromJava(unit :ChronoUnit) :DateTimeUnit = {
		val length = unit.getDuration.getSeconds
		if (length < SecondsInDay) new TimeUnit(unit)
		else if (length < SecondsInEra) new DateUnit(unit)
		else new SpecialUnit(unit)
	}

	@inline implicit def toJava(unit :DateTimeUnit) :TemporalUnit = unit.toJava

	@inline implicit def fromConcurrentUnit(unit :JTimeUnit) :DateTimeUnit = fromJava(unit.toChronoUnit)
	@inline implicit def toConcurrentUnit(unit :DateTimeUnit) :JTimeUnit = JTimeUnit.of(unit.toJava)




	private final val SomeNanos = Some(Nanos)
	private final val SomeMicros = Some(Micros)
	private final val SomeMillis = Some(Millis)
	private final val SomeSeconds = Some(Seconds)
	private final val SomeMinutes = Some(Minutes)
	private final val SomeHours = Some(Hours)
	private final val SomeDays = Some(Days)
}






class TimeUnit private[time] (override val toJava :ChronoUnit) extends AnyVal with DateTimeUnit {
	@inline override def span :TimeSpan = toJava.getDuration

	@inline override def inNanos :Long = toJava.getDuration.toNanos

	def *(number :Long) :Duration = new Duration(j.Duration.of(number, toJava))

	def unapply(time :TimeSpan) :Option[(Long, TimeSpan)] = time match {
		case finite :FiniteTimeSpan => Some((finite /% toJava.getDuration, time % this))
		case _ => None
	}


	def symbol :String = toJava match {
		case ChronoUnit.NANOS => "ns"
		case ChronoUnit.MICROS => "µs"
		case ChronoUnit.MILLIS => "ms"
		case ChronoUnit.SECONDS => "s"
		case ChronoUnit.MINUTES => "min"
		case ChronoUnit.HOURS => "h"
		case _ => toString
	}

	override def toString :String = toJava match {
		case ChronoUnit.NANOS => "nanos"
		case ChronoUnit.MICROS => "micros"
		case ChronoUnit.MILLIS => "millis"
		case ChronoUnit.SECONDS => "seconds"
		case ChronoUnit.MINUTES => "minutes"
		case ChronoUnit.HOURS => "hours"
		case ChronoUnit.HALF_DAYS => "half-days"
		case _ => toJava.toString
	}
}



object TimeUnit {
	@inline final val Nanos = new TimeUnit(ChronoUnit.NANOS)
	@inline final val Micros = new TimeUnit(ChronoUnit.MICROS)
	@inline final val Millis = new TimeUnit(ChronoUnit.MILLIS)
	@inline final val Seconds = new TimeUnit(ChronoUnit.SECONDS)
	@inline final val Minutes = new TimeUnit(ChronoUnit.MINUTES)
	@inline final val Hours = new TimeUnit(ChronoUnit.HOURS)

	@inline final val HalfDays = new TimeUnit(ChronoUnit.HALF_DAYS)
}






class DateUnit private[time] (override val toJava :ChronoUnit) extends AnyVal with DateTimeUnit {
	override def span :TimeSpan = toJava.getDuration

	override def inNanos :Long = toJava.getDuration.toNanos

	def base :DateUnit = toJava match {
		case ChronoUnit.DAYS | ChronoUnit.MONTHS | ChronoUnit.YEARS => this
		case ChronoUnit.WEEKS => DateUnit.Days
		case _ => DateUnit.Years
	}

	def baseAmount :Int = toJava match {
		case ChronoUnit.DAYS | ChronoUnit.MONTHS | ChronoUnit.YEARS => 1
		case ChronoUnit.WEEKS => 7
		case ChronoUnit.DECADES => 10
		case ChronoUnit.CENTURIES => 100
		case ChronoUnit.MILLENNIA => 1000
	}

	def *(number :Int) :Period = toJava match {
		case ChronoUnit.DAYS => Period.days(number)
		case ChronoUnit.MONTHS => Period.months(number)
		case ChronoUnit.YEARS => Period.years(number)
		case ChronoUnit.WEEKS if number <= Int.MaxValue / 7 => Period.days(7 * number)
		case ChronoUnit.DECADES if number <= Int.MaxValue / 10 => Period.years(number * 10)
		case ChronoUnit.CENTURIES if number <= Int.MaxValue / 100 => Period.years(100 * number)
		case ChronoUnit.MILLENNIA if number <= Int.MaxValue / 1000 => Period.years(1000 * number)
		case _ => throw new ArithmeticException(s"Int overflow: " + number + " " + this)
	}


	override def toString :String = toJava match {
		case ChronoUnit.DAYS => "days"
		case ChronoUnit.MONTHS => "months"
		case ChronoUnit.YEARS => "years"
		case ChronoUnit.WEEKS => "weeks"
		case ChronoUnit.DECADES => "decades"
		case ChronoUnit.CENTURIES => "centuries"
		case ChronoUnit.MILLENNIA => "millennia"
	}

}



object DateUnit {
	@inline final val Days = new DateUnit(ChronoUnit.DAYS)
	@inline final val Weeks = new DateUnit(ChronoUnit.WEEKS)
	@inline final val Months = new DateUnit(ChronoUnit.MONTHS)
	@inline final val Years = new DateUnit(ChronoUnit.YEARS)
	@inline final val Decades = new DateUnit(ChronoUnit.DECADES)
	@inline final val Centuries = new DateUnit(ChronoUnit.CENTURIES)
	@inline final val Millennia = new DateUnit(ChronoUnit.MILLENNIA)
}






class SpecialUnit private[time] (override val toJava :ChronoUnit) extends AnyVal with DateTimeUnit {
	override def span :TimeSpan = if (toJava == ChronoUnit.FOREVER) Eternity else toJava.getDuration //todo: this is an estimate for Eras
	override def inNanos :Long  = throw new UnsupportedOperationException(s"$this.inNanos")
}

