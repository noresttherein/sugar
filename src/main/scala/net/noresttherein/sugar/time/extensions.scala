package net.noresttherein.sugar.time

import java.{time => j}
import java.time.chrono.IsoEra
import java.time.temporal.{ChronoUnit, TemporalField}

import scala.concurrent.{duration => s}

import net.noresttherein.sugar.time.extensions.{JavaChronoUnitConverter, JavaClockConverter, JavaDayOfWeekConverter, JavaDurationConverter, JavaInstantConverter, JavaIsoEraConverter, JavaLocalDateConverter, JavaLocalDateTimeConverter, JavaLocalTimeConverter, JavaMonthConverter, JavaMonthDayConverter, JavaOffsetDateTimeConverter, JavaOffsetTimeConverter, JavaPeriodConverter, JavaTemporalFieldConverter, JavaYearConverter, JavaYearMonthConverter, JavaZonedDateTimeConverter, JavaZoneIdConverter, JavaZoneOffsetConverter, ScalaDurationConverter}
import net.noresttherein.sugar.typist.Rank
import net.noresttherein.sugar.typist.Rank.Rank0




/** Implicit converters adding methods to `java.time` classes for converting them to their counterparts from this package.
  * Additional implicit factory methods can be found in the `net.noresttherein.sugar.time.dsl` package.
  */
trait extensions[+R <: Rank] extends Any {
	@inline implicit final def JavaInstantConverter(self :j.Instant) :JavaInstantConverter[R] =
		new JavaInstantConverter[R](self)

	@inline implicit final def JavaZonedDateTimeConverter(self :j.ZonedDateTime) :JavaZonedDateTimeConverter[R] =
		new JavaZonedDateTimeConverter[R](self)

	@inline implicit final def JavaOffsetDateTimeConverter(self :j.OffsetDateTime) :JavaOffsetDateTimeConverter[R] =
		new JavaOffsetDateTimeConverter[R](self)

	@inline implicit final def JavaLocalDateTimeConverter(self :j.LocalDateTime) :JavaLocalDateTimeConverter[R] =
		new JavaLocalDateTimeConverter[R](self)

	@inline implicit final def JavaLocalDateConverter(self :j.LocalDate) :JavaLocalDateConverter[R] =
		new JavaLocalDateConverter[R](self)

	@inline implicit final def JavaYearConverter(self :j.Year) :JavaYearConverter[R] = new JavaYearConverter[R](self)

	@inline implicit final def JavaMonthConverter(self :j.Month) :JavaMonthConverter[R] = new JavaMonthConverter[R](self)

	@inline implicit final def JavaYearMonthConverter(self :j.YearMonth) :JavaYearMonthConverter[R] =
		new JavaYearMonthConverter[R](self)

	@inline implicit final def JavaMonthDayConverter(self :j.MonthDay) :JavaMonthDayConverter[R] =
		new JavaMonthDayConverter[R](self)

	@inline implicit final def JavaDayOfWeekConverter(self :j.DayOfWeek) :JavaDayOfWeekConverter[R] =
		new JavaDayOfWeekConverter[R](self)

	@inline implicit final def JavaOffsetDateTimeConverter(self :j.OffsetTime) :JavaOffsetTimeConverter[R] =
		new JavaOffsetTimeConverter[R](self)

	@inline implicit final def JavaLocalTimeConverter(self :j.LocalTime) :JavaLocalTimeConverter[R] =
		new JavaLocalTimeConverter[R](self)

	@inline implicit final def JavaIsoEraConverter(self :IsoEra) :JavaIsoEraConverter[R] =
		new JavaIsoEraConverter[R](self)

	@inline implicit final def JavaPeriodConverter(self :j.Period) :JavaPeriodConverter[R] =
		new JavaPeriodConverter[R](self)

	@inline implicit final def JavaDurationConverter(self :j.Duration) :JavaDurationConverter[R] =
		new JavaDurationConverter[R](self)

	@inline implicit final def JavaZoneIdConverter(self :j.ZoneId) :JavaZoneIdConverter[R] =
		new JavaZoneIdConverter[R](self)

	@inline implicit final def JavaZoneOffsetConverter(self :j.ZoneOffset) :JavaZoneOffsetConverter[R] =
		new JavaZoneOffsetConverter[R](self)

	@inline implicit final def JavaTemporalFieldConverter(self :TemporalField) :JavaTemporalFieldConverter[R] =
		new JavaTemporalFieldConverter[R](self)

	@inline implicit final def JavaChronoUnitConverter(self :ChronoUnit) :JavaChronoUnitConverter[R] =
		new JavaChronoUnitConverter[R](self)

	@inline implicit final def JavaClockConverter(self :j.Clock) :JavaClockConverter[R] =
		new JavaClockConverter[R](self)

	@inline implicit final def ScalaDurationConverter(self :s.Duration) :ScalaDurationConverter[R] =
		new ScalaDurationConverter[R](self)
}




@SerialVersionUID(Ver)
object extensions extends extensions[Rank0] {
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
		@inline def toDateOfYear :Anniversary = Anniversary(date)
		@inline def toScala      :Anniversary = Anniversary(date)
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

	//todo: allow conversions only from ChronoUnit singleton type and provide only type safe methods.
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
