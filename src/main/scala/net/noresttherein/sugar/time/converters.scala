package net.noresttherein.sugar.time

import java.{time => j}
import java.time.chrono.IsoEra
import java.time.temporal.{ChronoUnit, TemporalField}

import scala.concurrent.{duration => s}

import net.noresttherein.sugar.time.converters.{JavaChronoUnitConverter, JavaClockConverter, JavaDayOfWeekConverter, JavaDurationConverter, JavaInstantConverter, JavaIsoEraConverter, JavaLocalDateConverter, JavaLocalDateTimeConverter, JavaLocalTimeConverter, JavaMonthConverter, JavaMonthDayConverter, JavaOffsetDateTimeConverter, JavaOffsetTimeConverter, JavaPeriodConverter, JavaTemporalFieldConverter, JavaYearConverter, JavaYearMonthConverter, JavaZonedDateTimeConverter, JavaZoneIdConverter, JavaZoneOffsetConverter, ScalaDurationConverter}
import net.noresttherein.sugar.typist.Rank
import net.noresttherein.sugar.typist.Rank.Rank0




/** Implicit converters adding methods to `java.time` classes for converting them to their counterparts from this sugar.
  * Additional implicit factory methods can be found in the `net.noresttherein.sugar.time.dsl` sugar.
  */
trait converters[+R <: Rank] {
	@inline implicit def javaInstantConverter(self :j.Instant)               = new JavaInstantConverter[R](self)
	@inline implicit def javaZonedDateTimeConverter(self :j.ZonedDateTime)   = new JavaZonedDateTimeConverter[R](self)
	@inline implicit def javaOffsetDateTimeConverter(self :j.OffsetDateTime) = new JavaOffsetDateTimeConverter[R](self)
	@inline implicit def javaLocalDateTimeConverter(self :j.LocalDateTime)   = new JavaLocalDateTimeConverter[R](self)
	@inline implicit def javaLocalDateConverter(self :j.LocalDate)           = new JavaLocalDateConverter[R](self)
	@inline implicit def javaYearConverter(self :j.Year)                     = new JavaYearConverter[R](self)
	@inline implicit def javaMonthConverter(self :j.Month)                   = new JavaMonthConverter[R](self)
	@inline implicit def javaYearMonthConverter(self :j.YearMonth)           = new JavaYearMonthConverter[R](self)
	@inline implicit def javaMonthDayConverter(self :j.MonthDay)             = new JavaMonthDayConverter[R](self)
	@inline implicit def javaDayOfWeekConverter(self :j.DayOfWeek)           = new JavaDayOfWeekConverter[R](self)
	@inline implicit def javaOffsetDateTimeConverter(self :j.OffsetTime)     = new JavaOffsetTimeConverter[R](self)
	@inline implicit def javaLocalTimeConverter(self :j.LocalTime)           = new JavaLocalTimeConverter[R](self)
	@inline implicit def javaIsoEraConverter(self :IsoEra)                   = new JavaIsoEraConverter[R](self)

	@inline implicit def javaPeriodConverter(self :j.Period)                 = new JavaPeriodConverter[R](self)
	@inline implicit def javaDurationConverter(self :j.Duration)             = new JavaDurationConverter[R](self)

	@inline implicit def javaZoneIdConverter(self :j.ZoneId)                 = new JavaZoneIdConverter[R](self)
	@inline implicit def javaZoneOffsetConverter(self :j.ZoneOffset)         = new JavaZoneOffsetConverter[R](self)
	@inline implicit def javaTemporalFieldConverter(self :TemporalField)     = new JavaTemporalFieldConverter[R](self)
	@inline implicit def javaChronoUnitConverter(self :ChronoUnit)           = new JavaChronoUnitConverter[R](self)
	@inline implicit def javaClockConverter(self :j.Clock)                   = new JavaClockConverter[R](self)

	@inline implicit def scalaDurationConverter(self :s.Duration)            = new ScalaDurationConverter[R](self)
}




object converters extends converters[Rank0] {
	class JavaInstantConverter[+R](private val instant :j.Instant) extends AnyVal {
		@inline def toTimestamp :Timestamp = new Timestamp(instant)
		@inline def toUTC :UTCDateTime = new UTCDateTime(instant.atOffset(j.ZoneOffset.UTC).toLocalDateTime)
		@inline def in(zone :TimeZone) :ZoneDateTime = new ZoneDateTime(instant.atZone(zone.toJava))
	}

	class JavaZonedDateTimeConverter[+R](private val time :j.ZonedDateTime) extends AnyVal {
		@inline def toTimestamp = new Timestamp(time.toInstant)
		@inline def toUTC = new UTCDateTime(j.LocalDateTime.ofEpochSecond(time.toEpochSecond, time.getNano, j.ZoneOffset.UTC))
		@inline def toZoneDateTime = new ZoneDateTime(time)
		@inline def toScala = new ZoneDateTime(time)
		@inline def in(zone :TimeZone) = new ZoneDateTime(j.ZonedDateTime.ofInstant(time.toInstant, zone.toJava))
	}

	class JavaOffsetDateTimeConverter[+R](private val time :j.OffsetDateTime) extends AnyVal {
		@inline def toTimestamp :Timestamp = new Timestamp(time.toInstant)
		@inline def toUTC :UTCDateTime = new UTCDateTime(time.atZoneSameInstant(j.ZoneOffset.UTC).toLocalDateTime)
		@inline def toZoneDateTime :ZoneDateTime = new ZoneDateTime(time.toZonedDateTime)
		@inline def toScala :ZoneDateTime = new ZoneDateTime(time.toZonedDateTime)
		@inline def in(zone :TimeZone) :ZoneDateTime = new ZoneDateTime(time.atZoneSameInstant(zone.toJava))
	}

	class JavaLocalDateTimeConverter[+R](private val time :j.LocalDateTime) extends AnyVal {
		@inline def toDateTime :DateTime    = new DateTime(time)
		@inline def toUTC      :UTCDateTime = new UTCDateTime(time)
		@inline def toScala    :DateTime    = new DateTime(time)
		@inline def in(zone :TimeZone) :ZoneDateTime = new ZoneDateTime(time.atZone(zone.toJava))
	}

	class JavaLocalDateConverter[+R](private val time :j.LocalDate) extends AnyVal {
		@inline def toDate  :Date = new Date(time)
		@inline def toScala :Date = new Date(time)
	}

	class JavaYearConverter[+R](private val year :j.Year) extends AnyVal {
		@inline def toYear  :Year = Year(year)
		@inline def toScala :Year = Year(year)
	}

	class JavaMonthConverter[+R](private val month :j.Month) extends AnyVal {
		@inline def toMonth :Month = Month(month)
		@inline def toScala :Month = Month(month)
	}

	class JavaYearMonthConverter[+R](private val date :j.YearMonth) extends AnyVal {
		@inline def toMonthOfYear :MonthOfYear = MonthOfYear(date)
		@inline def toScala       :MonthOfYear = MonthOfYear(date)
	}

	class JavaMonthDayConverter[+R](private val date :j.MonthDay) extends AnyVal {
		@inline def toDateOfYear :DateOfYear = DateOfYear(date)
		@inline def toScala      :DateOfYear = DateOfYear(date)
	}

	class JavaDayOfWeekConverter[+R](private val day :j.DayOfWeek) extends AnyVal {
		@inline def toDay   :Day = Day(day)
		@inline def toScala :Day = Day(day)
	}

	class JavaOffsetTimeConverter[+R](private val time :j.OffsetTime) extends AnyVal {
		@inline def toOffsetTime :OffsetTime = OffsetTime(time)
		@inline def toScala      :OffsetTime = OffsetTime(time)
	}

	class JavaLocalTimeConverter[+R](private val time :j.LocalTime) extends AnyVal {
		@inline def toTimeOfDay :TimeOfDay = TimeOfDay(time)
		@inline def toScala     :TimeOfDay = TimeOfDay(time)
	}

	class JavaIsoEraConverter[+R](private val era :IsoEra) extends AnyVal {
		@inline def toEra   :Era = new Era(era)
		@inline def toScala :Era = new Era(era)
	}


	class JavaPeriodConverter[+R](private val time :j.Period) extends AnyVal {
		@inline def toPeriod :Period = Period(time)
		@inline def toScala  :Period = Period(time)
	}

	class JavaDurationConverter[+R](private val time :j.Duration) extends AnyVal {
		@inline def toDuration :Duration = Duration(time)
		@inline def toTimeSpan :Duration = Duration(time)
		@inline def toScala    :Duration = Duration(time)
	}

	class JavaZoneIdConverter[+R](private val zone :j.ZoneId) extends AnyVal {
		@inline def toTimeZone :TimeZone = new TimeZone(zone)
		@inline def toScala    :TimeZone = new TimeZone(zone)
	}

	class JavaZoneOffsetConverter[+R](private val offset :j.ZoneOffset) extends AnyVal {
		@inline def toTimeOffset :TimeOffset = new TimeOffset(offset)
		@inline def toScala      :TimeOffset   = new TimeOffset(offset)
	}

	class JavaTemporalFieldConverter[+R](private val field :TemporalField) extends AnyVal {
		@inline def toCycle :Cycle.LongValueCycle = Cycle(field)
		@inline def toScala :Cycle.LongValueCycle = Cycle(field)
	}


	class JavaChronoUnitConverter[+R](private val unit :ChronoUnit) extends AnyVal {
		@inline def toDateTimeUnit :DateTimeUnit = DateTimeUnit(unit)
		@inline def toTimeUnit     :TimeUnit     = DateTimeUnit(unit).asInstanceOf[TimeUnit]
		@inline def toDateUnit     :DateUnit     = DateTimeUnit(unit).asInstanceOf[DateUnit]
		@inline def toScala        :DateTimeUnit = DateTimeUnit(unit)
	}


	class JavaClockConverter[+R](private val clock :j.Clock) extends AnyVal {
		@inline def time    :Time = new Time(clock)
		@inline def toScala :Time = new Time(clock)
	}


	class ScalaDurationConverter[+R](private val duration :s.Duration) extends AnyVal {
		@inline def toTimeInterval :TimeInterval = TimeInterval(duration)
		@inline def toScala        :TimeInterval = TimeInterval(duration)
	}
}
