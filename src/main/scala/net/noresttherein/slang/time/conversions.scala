package net.noresttherein.slang.time

import java.{time => j}
import java.time.ZoneId
import java.time.temporal.{ChronoUnit, TemporalField}



/** Implicit converters adding methods to `java.time` classes for converting them to their counterparts from this package.
  * Additional implicit factory methods can be found in the `net.noresttherein.slang.time.dsl` package.
  */
object conversions {
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

	implicit class JavaOffsetDateTimeConverter(private val time :j.OffsetDateTime) extends AnyVal {
		@inline def toTimestamp = new Timestamp(time.toInstant)
		@inline def toUTC = new UTCDateTime(time.atZoneSameInstant(j.ZoneOffset.UTC).toLocalDateTime)
		@inline def toZoneDateTime = new ZoneDateTime(time.toZonedDateTime)
		@inline def in(zone :TimeZone) = new ZoneDateTime(time.atZoneSameInstant(zone.toJava))
	}

	implicit class JavaLocalDateTimeConverter(private val time :j.LocalDateTime) extends AnyVal {
		@inline def toDateTime = new DateTime(time)
		@inline def toUTC = new UTCDateTime(time)
		@inline def in(zone :TimeZone) = new ZoneDateTime(time.atZone(zone.toJava))
	}

	implicit class JavaLocalDateConverter(private val time :j.LocalDate) extends AnyVal {
		@inline def toDate = new Date(time)
	}

	implicit class JavaYearConverter(private val year :j.Year) extends AnyVal {
		@inline def toYear = Year(year)
	}

	implicit class JavaMonthConverter(private val month :j.Month) extends AnyVal {
		@inline def toMonth = Month(month)
	}

	implicit class JavaMonthDayConverter(private val date :j.MonthDay) extends AnyVal {
		@inline implicit def toDateOfYear = DateOfYear(date)
	}

	implicit class JavaYearMonthConverter(private val date :j.YearMonth) extends AnyVal {
		@inline implicit def toMonthInYear = MonthInYear(date)
	}

	implicit class JavaDayOfWeekConverter(private val day :j.DayOfWeek) extends AnyVal {
		@inline def toDay = Day(day)
	}

	implicit class JavaLocalTimeConverter(private val time :j.LocalTime) extends AnyVal {
		@inline def toTimeOfDay = TimeOfDay(time)
	}

	implicit class JavaOffsetTimeConverter(private val time :j.OffsetTime) extends AnyVal {
		@inline def toOffsetTime = OffsetTime(time)
	}

	implicit class JavaDurationConverter(private val time :j.Duration) extends AnyVal {
		@inline def toDuration = Duration(time)
		@inline def toTimeSpan :Duration = Duration(time)
	}

	implicit class JavaPeriodConverter(private val time :j.Period) extends AnyVal {
		@inline def toPeriod = Period(time)
	}

	implicit class JavaZoneIdConverter(private val zone :j.ZoneId) extends AnyVal {
		@inline def toTimeZone = new TimeZone(zone)
	}

	implicit class JavaZoneOffsetConverter(private val offset :j.ZoneOffset) extends AnyVal {
		@inline def toTimeOffset = new TimeOffset(offset)
	}

	implicit class JavaTemporalFieldConverter(private val field :TemporalField) extends AnyVal {
		@inline def toCycle = Cycle(field)
	}



	implicit class JavaChronoUnitConverter(private val unit :ChronoUnit) extends AnyVal {
		@inline def toDateTimeUnit = DateTimeUnit(unit)
		@inline def toTimeUnit :TimeUnit = DateTimeUnit(unit).asInstanceOf[TimeUnit]
		@inline def toDateUnit :DateUnit = DateTimeUnit(unit).asInstanceOf[DateUnit]
	}



	implicit class JavaClockConverter(private val clock :j.Clock) extends AnyVal {
		@inline def time = new Time(clock)
	}


}
