package net.noresttherein.slang

import java.{time => j}
import java.time.{Clock, ZoneId}
import java.time.temporal.{ChronoField, ChronoUnit, TemporalField}

import net.noresttherein.slang.time.DateOfYear.DateDayWithMonth
import net.noresttherein.slang.time.TimeOfDay.HourWithMinute

/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
package object time {

	@inline final val NanosInMicro = 1000L
	@inline final val NanosInMilli = 1000000L
	@inline final val NanosInSecond = 1000000000L
	@inline final val NanosInMinute = 60L * NanosInSecond
	@inline final val NanosInHour = 60L * NanosInMinute
	@inline final val NanosInDay = 24L * NanosInHour

	@inline final val MicrosInMilli = 1000L

	@inline final val MillisInSecond = 1000L
	@inline final val MillisInMinute = 60 * MillisInSecond
	@inline final val MillisInHour = 60 * MillisInSecond
	@inline final val MillisInDay = 24 * MillisInHour

	@inline final val SecondsInMinute = 60L
	@inline final val SecondsInHour = 3600L
	@inline final val SecondsInDay = 24 * SecondsInHour
	private[time] final val SecondsInEra = ChronoUnit.ERAS.getDuration.getSeconds

	@inline private[time] final val MaxNanoDuration = Long.MaxValue / NanosInMilli
	@inline private[time] final val IntNanosInSecond = 1000000000
	@inline private[time] final val IntSecondsInHalfDay = 12 * 60 * 60




	implicit class JavaInstantConverter(private val instant :j.Instant) extends AnyVal {
		@inline def toTimestamp = new Timestamp(instant)
		@inline def toUTC = new UTCDateTime(instant.atOffset(j.ZoneOffset.UTC).toLocalDateTime)
		@inline def in(zone :TimeZone) = new ZoneDateTime(instant.atZone(zone.toJava))
	}

	implicit class JavaZonedDateTimeConverter(private val time :j.ZonedDateTime) extends AnyVal {
		@inline def toTimestamp = new Timestamp(time.toInstant)
		@inline def toUTC = new UTCDateTime(j.LocalDateTime.ofEpochSecond(time.toEpochSecond, time.getNano, j.ZoneOffset.UTC))
		@inline def toZoneDateTime = new ZoneDateTime(time)
		@inline def in(zone :TimeZone) = new ZoneDateTime(j.ZonedDateTime.ofInstant(time.toInstant, zone.toJava))
	}

	implicit class JavaLocalDateTimeConverter(private val time :j.LocalDateTime) extends AnyVal {
		@inline def toDateTime = new DateTime(time)
		@inline def toUTC = new UTCDateTime(time)
		@inline def in(zone :TimeZone) = new ZoneDateTime(time.atZone(zone.toJava))
	}

	implicit class JavaLocalDateConverter(private val time :j.LocalDate) extends AnyVal {
		@inline def toDate = new Date(time)
	}

	implicit class JavaLocalTimeConverter(private val time :j.LocalTime) extends AnyVal {
		@inline def toTimeOfDay = new TimeOfDay(time)
	}

	implicit class JavaDurationConverter(private val time :j.Duration) extends AnyVal {
		@inline def toTimeSpan :Duration = new Duration(time)
	}

	implicit class JavaPeriodConverter(private val time :j.Period) extends AnyVal {
		@inline implicit def toPeriod = new Period(time)
	}



	/** Throws `ArithmeticException` to indicate `Long` arithmetic overflow during operation `this op other`.
	  * Extracted to minimize inlined footprint of calling methods. Access is `protected[slang]` to avoid
	  * leaking of the API while still allowing inlining of calling methods (because it translates to `public`
	  * in the bytecode).
	  */
	private[slang] def overflow(left :Any, op :String, right :Any) :Nothing =
		throw new ArithmeticException("Long overflow: " + left + op + right)

	/** Throws `ArithmeticException` to indicate `Long` arithmetic overflow during method `method`.
	  * Extracted to minimize inlined footprint of calling methods. Access is `protected[slang]` to avoid
	  * leaking of the API while still allowing inlining of calling methods (because it translates to `public`
	  * in the bytecode).
	  */
	private[slang] def overflow(self :Any, method :String) :Nothing =
		throw new ArithmeticException("Long overflow: (" + self + ")." + method)


	@inline private[slang] def twoDigit(ord :Int) :String =
		if (ord < 10) "0" + ord else ord.toString

}
