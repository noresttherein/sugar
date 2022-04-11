package net.noresttherein.sugar.time

import java.{time => j}
import java.time.temporal.{ChronoUnit, TemporalUnit}
import java.time.temporal.ChronoUnit._
import java.util.concurrent.{TimeUnit => JTimeUnit}

import DateTimeUnit._
import DateUnit._




/** A unit used to measure elapsed time. Has three distinct subclasses:
  * [[net.noresttherein.sugar.time.TimeUnit TimeUnit]] for units describing time spans of fixed lengths (such as seconds
  * or hours), [[net.noresttherein.sugar.time.DateUnit DateUnit]] for calendar-based units such as months and
  * [[net.noresttherein.sugar.time.PseudoUnit PseudoUnit]] for adapters of not unit instances of `java.time.ChronoUnit`
  * such as `FOREVER` and `ERAS`.
  * @author Marcin Mościcki
  */
sealed trait DateTimeUnit extends Any with Ordered[DateTimeUnit] with Serializable {

	/** The amount of time measured of this unit, which will be variable for a `DateUnit`
	  * and infinite for a `PseudoUnit`.
	  */
	def time :TimeExtent //= toJava.getDuration

	/** The length of this unit, which may be variable for a `DateUnit`.
	  * @throws UnsupportedOperationException if this unit is a `PseudoUnit`
	  */
	def span :TimeFrame

	/** Estimated/averaged fixed length of this unit as returned by the underlying java `ChronoUnit`.
	  * Will be equal to both `time` and `span` for a `TimeUnit`, while a `DateUnit` will return an approximate value.
	  */
	def approx :TimeSpan

	def toJava :ChronoUnit



	/** The longest of date time units shorter than this unit from the list of: year, month, day, hour, minute, second,
	  * millisecond, microsecond, nanosecond.
	  */
	def finer :Option[DateTimeUnit] = {
		val duration = toJava.getDuration
		val seconds = duration.toSeconds
		val nanos = duration.toNanos
		if (seconds <= 1L)
			if (nanos <= NanosInMicro)
				if (nanos == 1L) None
				else SomeNanos
			else
				if (nanos <= NanosInMilli) SomeMicros
				else SomeMillis
		else
			if (seconds <= SecondsInHour)
				if (seconds <= SecondsInMinute) SomeSeconds
				else SomeMinutes
			else if (seconds <= SecondsInMonth)
				if (seconds <= SecondsInDay) SomeHours
				else SomeDays
			else
				if (seconds <= SecondsInYear) SomeMonths
				else SomeYears
	}



	/** The shortest of date time units longer than this unit from the list of: year, month, day, hour, minute, second,
	  * millisecond, microsecond, nanosecond.
	  */
	def coarser :Option[DateTimeUnit] = {
		val duration = toJava.getDuration
		val seconds = duration.toSeconds
		val nanos = duration.toNanos
		if (seconds == 0L)
			if (nanos < NanosInMicro)
				if (nanos == 0) SomeNanos
				else SomeMicros
			else
				if (nanos < NanosInMilli) SomeMillis
				else SomeSeconds
		else
			if (seconds < SecondsInHour)
				if (seconds < SecondsInMinute) SomeMinutes
				else SomeHours
			else if (seconds < SecondsInMonth)
				if (seconds < SecondsInDay) SomeDays
				else SomeMonths
			else
				if (seconds < SecondsInYear) SomeYears
				else None
	}


}






object DateTimeUnit {
	@inline final val Nanos      = new TimeUnit(NANOS)
	@inline final val Micros     = new TimeUnit(MICROS)
	@inline final val Millis     = new TimeUnit(MILLIS)
	@inline final val Seconds    = new TimeUnit(SECONDS)
	@inline final val Minutes    = new TimeUnit(MINUTES)
	@inline final val Hours      = new TimeUnit(HOURS)
	@inline final val HalfDays   = new TimeUnit(HALF_DAYS)

	@inline final val Days       = new DateUnit(DAYS)
	@inline final val Weeks      = new DateUnit(WEEKS)
	@inline final val Months     = new DateUnit(MONTHS)
	@inline final val Years      = new DateUnit(YEARS)

	@inline final val Decades    = new DateUnit(DECADES)
	@inline final val Centuries  = new DateUnit(CENTURIES)
	@inline final val Millennia  = new DateUnit(MILLENNIA)

	@inline final val Eras       = new PseudoUnit(ERAS)
	@inline final val Forever    = new PseudoUnit(FOREVER)


	@inline def apply(unit :ChronoUnit) :DateTimeUnit = {
		val length = unit.getDuration.getSeconds
		if (length < SecondsInDay) new TimeUnit(unit)
		else if (length < SecondsInEra) new DateUnit(unit)
		else new PseudoUnit(unit)
	}

	@inline def unapply(unit :DateTimeUnit) :Some[ChronoUnit] = Some(unit.toJava)


	@inline implicit def fromJava(unit :ChronoUnit) :DateTimeUnit = apply(unit)
	@inline implicit def toJava(unit :DateTimeUnit) :TemporalUnit = unit.toJava

	@inline implicit def fromConcurrentUnit(unit :JTimeUnit) :DateTimeUnit = apply(unit.toChronoUnit)
	@inline implicit def toConcurrentUnit(unit :DateTimeUnit) :JTimeUnit = JTimeUnit.of(unit.toJava)


	private final val SomeNanos   = Some(Nanos)
	private final val SomeMicros  = Some(Micros)
	private final val SomeMillis  = Some(Millis)
	private final val SomeSeconds = Some(Seconds)
	private final val SomeMinutes = Some(Minutes)
	private final val SomeHours   = Some(Hours)
	private final val SomeDays    = Some(Days)
	private final val SomeMonths  = Some(Months)
	private final val SomeYears   = Some(Years)
}






/** A time unit of fixed length, i.e. a standard time unit and half-days. */
@SerialVersionUID(1L)
class TimeUnit private[time] (override val toJava :ChronoUnit) extends AnyVal with DateTimeUnit {
	@inline override def time :TimeSpan = toJava.getDuration
	@inline override def span :TimeSpan = toJava.getDuration
	@inline override def approx :Duration = toJava.getDuration
	@inline def length  :Duration = toJava.getDuration
	@inline def inNanos :Long     = toJava.getDuration.toNanos
	@inline def toNanos :Long     = toJava.getDuration.toNanos


	override def compare(that :DateTimeUnit) :Int = that match {
		case time :TimeUnit =>
			val n1 = inNanos; val n2 = time.inNanos
			if (n1 < n2) -1 else if (n2 < n1) 1 else 0
		case _ => -1
	}


	@inline def *(number :Long) :Duration = new Duration(j.Duration.of(number, toJava))

	def unapply(time :TimeInterval) :Option[(Long, TimeInterval)] = time match {
		case finite :TimeSpan => Some((finite quot toJava.getDuration, time % this))
		case _ => None
	}


	def symbol :String = toJava match {
		case NANOS   => "ns"
		case MICROS  => "µs"
		case MILLIS  => "ms"
		case SECONDS => "s"
		case MINUTES => "min"
		case HOURS   => "h"
		case _ => toString
	}

	override def toString :String = toJava match {
		case NANOS     => "nanos"
		case MICROS    => "micros"
		case MILLIS    => "millis"
		case SECONDS   => "seconds"
		case MINUTES   => "minutes"
		case HOURS     => "hours"
		case HALF_DAYS => "half-days"
		case _ => toJava.toString
	}

}



object TimeUnit {
	@inline final val Nanos    = new TimeUnit(ChronoUnit.NANOS)
	@inline final val Micros   = new TimeUnit(ChronoUnit.MICROS)
	@inline final val Millis   = new TimeUnit(ChronoUnit.MILLIS)
	@inline final val Seconds  = new TimeUnit(ChronoUnit.SECONDS)
	@inline final val Minutes  = new TimeUnit(ChronoUnit.MINUTES)
	@inline final val Hours    = new TimeUnit(ChronoUnit.HOURS)

	@inline final val HalfDays = new TimeUnit(ChronoUnit.HALF_DAYS)
}






/** A time unit of variable length, varying from days to millenia. */
@SerialVersionUID(1L)
class DateUnit private[time] (override val toJava :ChronoUnit) extends AnyVal with DateTimeUnit {
	@inline override def time   :DateSpan = length
	@inline override def span   :DateSpan = length
	@inline override def approx :Duration       = toJava.getDuration

	def length :Period = (toJava: @unchecked) match {
		case DAYS => OneDay
		case WEEKS => OneWeek
		case MONTHS => OneMonth
		case YEARS => OneYear
		case DECADES => OneDecade
		case CENTURIES => OneCentury
		case MILLENNIA => OneMillennium
	}


	override def compare(that :DateTimeUnit) :Int = that match {
		case _ :TimeUnit => 1
		case date :DateUnit => approx.toSeconds compare date.approx.toSeconds
		case _ => -1
	}


	def base :DateUnit = toJava match {
		case DAYS | MONTHS | YEARS => this
		case WEEKS => DateUnit.Days
		case _ => DateUnit.Years
	}

	def baseAmount :Int = (toJava: @unchecked) match {
		case DAYS | MONTHS | YEARS => 1
		case WEEKS => 7
		case DECADES => 10
		case CENTURIES => 100
		case MILLENNIA => 1000
	}

	def *(number :Int) :Period = toJava match {
		case DAYS => Period.days(number)
		case MONTHS => Period.months(number)
		case YEARS => Period.years(number)
		case WEEKS if number <= Int.MaxValue / 7 => Period.days(7 * number)
		case DECADES if number <= Int.MaxValue / 10 => Period.years(number * 10)
		case CENTURIES if number <= Int.MaxValue / 100 => Period.years(100 * number)
		case MILLENNIA if number <= Int.MaxValue / 1000 => Period.years(1000 * number)
		case _ => throw new ArithmeticException(s"Int overflow: " + number + " " + this)
	}


	override def toString :String = (toJava: @unchecked) match {
		case DAYS => "days"
		case MONTHS => "months"
		case YEARS => "years"
		case WEEKS => "weeks"
		case DECADES => "decades"
		case CENTURIES => "centuries"
		case MILLENNIA => "millennia"
	}

}



object DateUnit {
	@inline final val Days      = new DateUnit(ChronoUnit.DAYS)
	@inline final val Weeks     = new DateUnit(ChronoUnit.WEEKS)
	@inline final val Months    = new DateUnit(ChronoUnit.MONTHS)
	@inline final val Years     = new DateUnit(ChronoUnit.YEARS)
	@inline final val Decades   = new DateUnit(ChronoUnit.DECADES)
	@inline final val Centuries = new DateUnit(ChronoUnit.CENTURIES)
	@inline final val Millennia = new DateUnit(ChronoUnit.MILLENNIA)

	private final val OneDay        = Period(days = 1)
	private final val OneWeek       = Period(days = 7)
	private final val OneMonth      = Period(months = 1)
	private final val OneYear       = Period(years = 1)
	private final val OneDecade     = Period(years = 10)
	private final val OneCentury    = Period(years = 100)
	private final val OneMillennium = Period(years = 1000)

}






/** Non-cyclic time 'units' of infinite length adapting `ChronoUnit.FOREVER` and `ChronoUnit.ERAS`. */
@SerialVersionUID(1L)
class PseudoUnit private[time](override val toJava :ChronoUnit) extends AnyVal with DateTimeUnit {
	@inline override def time :InfiniteTimeInterval = Eternity

	override def span :TimeFrame =
		throw new UnsupportedOperationException(toString + ".span")

	@inline override def approx :Duration = toJava.getDuration


	override def compare(that :DateTimeUnit) :Int = that match {
		case pseudo :PseudoUnit =>
			if (toJava == ERAS)
				if (pseudo.toJava == ERAS) 0 else -1
			else
				if (pseudo.toJava == ERAS) 1 else 0
		case _ => 1
	}

	override def toString :String =
		if (toJava == FOREVER) "Forever" else "Eras"
}

