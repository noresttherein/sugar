package net.noresttherein.slang.time

import java.{time => j}
import java.time.DateTimeException
import java.time.temporal.ChronoField

import net.noresttherein.slang.time.Cycle.HourOfDay
import net.noresttherein.slang.time.dsl.PartialTimeDesignators.{DateDayWithMonth, HourWithMinute}



/** Constants for temporal fields such as month and day names as well as implicit conversions adding to `Int` and `Long`
  * types factory methods for creating the classes from `net.noresttherein.slang.time` in a naturally looking way
  * such as `11 Jan 1981`, `11 :- 59 :- 59` or `1000_0000.nanos`.
  * @see [[net.noresttherein.slang.time.dsl.ISOSymbolMethods$]]
  * @author Marcin Mościcki
  */
package object dsl {

	final val Midnight = TimeOfDay(0, 0)
	final val Noon = TimeOfDay(12, 0)

	@inline final val Jan = Month.January
	@inline final val Feb = Month.February
	@inline final val Mar = Month.March
	@inline final val Apr = Month.April
	@inline final val May = Month.May
	@inline final val Jun = Month.June
	@inline final val Jul = Month.July
	@inline final val Aug = Month.August
	@inline final val Sep = Month.September
	@inline final val Oct = Month.October
	@inline final val Nov = Month.November
	@inline final val Dec = Month.December

	@inline final val Mon = Day.Monday
	@inline final val Tue = Day.Tuesday
	@inline final val Wed = Day.Wednesday
	@inline final val Thu = Day.Thursday
	@inline final val Fri = Day.Friday
	@inline final val Sat = Day.Saturday
	@inline final val Sun = Day.Sunday



	implicit class DateFactoryMethods(private val day :Int) extends AnyVal {
		@inline def Jan(year :Int) :Date = Date(year, 1, day)
		@inline def Feb(year :Int) :Date = Date(year, 2, day)
		@inline def Mar(year :Int) :Date = Date(year, 3, day)
		@inline def Apr(year :Int) :Date = Date(year, 4, day)
		@inline def May(year :Int) :Date = Date(year, 5, day)
		@inline def Jun(year :Int) :Date = Date(year, 6, day)
		@inline def Jul(year :Int) :Date = Date(year, 7, day)
		@inline def Aug(year :Int) :Date = Date(year, 8, day)
		@inline def Sep(year :Int) :Date = Date(year, 9, day)
		@inline def Oct(year :Int) :Date = Date(year, 10, day)
		@inline def Nov(year :Int) :Date = Date(year, 11, day)
		@inline def Dec(year :Int) :Date = Date(year, 12, day)

		@inline def Jan :DateOfYear = DateOfYear(dsl.Jan, day)
		@inline def Feb :DateOfYear = DateOfYear(dsl.Feb, day)
		@inline def Mar :DateOfYear = DateOfYear(dsl.Mar, day)
		@inline def Apr :DateOfYear = DateOfYear(dsl.Apr, day)
		@inline def May :DateOfYear = DateOfYear(dsl.May, day)
		@inline def Jun :DateOfYear = DateOfYear(dsl.Jun, day)
		@inline def Jul :DateOfYear = DateOfYear(dsl.Jul, day)
		@inline def Aug :DateOfYear = DateOfYear(dsl.Aug, day)
		@inline def Sep :DateOfYear = DateOfYear(dsl.Sep, day)
		@inline def Oct :DateOfYear = DateOfYear(dsl.Oct, day)
		@inline def Nov :DateOfYear = DateOfYear(dsl.Nov, day)
		@inline def Dec :DateOfYear = DateOfYear(dsl.Dec, day)

		@inline def :/(month :Int) :DateDayWithMonth = new DateDayWithMonth(DateOfYear(Month(month), day))
		@inline def :/(month :Month) :DateDayWithMonth = new DateDayWithMonth(DateOfYear(month, day))


		@inline def CE :Year =
			if (day == 0) throw new DateTimeException("0.CE: no year zero in ISO chronology")
			else new Year(day)

		@inline def BCE :Year =
			if (day == 0) throw new DateTimeException("0.BCE: no year zero in ISO chronology")
			else new Year(1 - day)
	}



	implicit class TimeOfDayFactoryMethod(private val hour :Int) extends AnyVal {
		@inline def :-(minute :Int) :HourWithMinute = new HourWithMinute(j.LocalTime.of(hour, minute))
	}



	object PartialTimeDesignators {

		class DateDayWithMonth(private val dateOfYear :DateOfYear) extends AnyVal {
			@inline def :/(year :Int) :Date = new Date(j.LocalDate.of(year, dateOfYear.month, dateOfYear.day))
		}

		object DateDayWithMonth {
			@inline implicit def toDateOfYear(dayMonth :DateDayWithMonth) :DateOfYear = dayMonth.dateOfYear
		}

		class HourWithMinute private[time] (private val timeOfDay :j.LocalTime) extends AnyVal {
			@inline def :-(second :Int) :HourWithMinuteAndSecond =
				new HourWithMinuteAndSecond(timeOfDay.plusSeconds(second))
		}

		object HourWithMinute {
			@inline implicit def toTimeOfDay(hourMinute :HourWithMinute) :TimeOfDay =
				new TimeOfDay(hourMinute.timeOfDay)
		}

		class HourWithMinuteAndSecond(private val timeOfDay :j.LocalTime) extends AnyVal {
			@inline def :-(nanosecond :Int) :TimeOfDay =
				new TimeOfDay(timeOfDay.`with`(ChronoField.NANO_OF_SECOND, nanosecond))
		}

		object HourWithMinuteAndSecond {
			@inline implicit def toTimeOfDay(hms :HourWithMinuteAndSecond) :TimeOfDay = new TimeOfDay(hms.timeOfDay)
		}

	}




	implicit class LongTimeLapseMethods(private val number :Long) extends AnyVal {
		@inline def nanos :Duration = new Duration(j.Duration.ofNanos(number))
		@inline def millis :Milliseconds = new Milliseconds(number)
		@inline def seconds :Duration = new Duration(j.Duration.ofSeconds(number))
		@inline def minutes :Duration = new Duration(j.Duration.ofMinutes(number))
		@inline def hours :Duration = new Duration(j.Duration.ofHours(number))
		@inline def days :Duration = new Duration(j.Duration.ofDays(number))
	}

	implicit class IntTimeLapseMethods(private val number :Int) extends AnyVal {
		@inline def nanos :Duration = new Duration(j.Duration.ofNanos(number))
		@inline def millis :Milliseconds = new Milliseconds(number)
		@inline def seconds :Milliseconds =	new Milliseconds(number * MillisInSecond)
		@inline def minutes :Milliseconds = new Milliseconds(number * MillisInMinute)
		@inline def hours :Milliseconds = new Milliseconds(number * MillisInHour)

		@inline def days :Period = j.Period.ofDays(number)
		@inline def months :Period = j.Period.ofMonths(number)
		@inline def years :Period = j.Period.ofYears(number)

		@inline def oClock :HourOfDay.Phase = HourOfDay(number)
	}



	object ISOSymbolMethods {
		implicit class LongISOSymbols(private val number :Long) extends AnyVal {
			@inline def ns :Duration = new Duration(j.Duration.ofNanos(number))
			@inline def ms :Milliseconds = new Milliseconds(number)
			@inline def s :Duration = new Duration(j.Duration.ofSeconds(number))

			@inline def µs :Duration =
				new Duration(j.Duration.ofSeconds(number / 1000000L, number % 1000000L * 1000L))

			@inline def h :Duration = new Duration(j.Duration.ofDays(number))
		}

		implicit class IntISOSymbols(private val number :Int) extends AnyVal {
			@inline def ns :Duration = new Duration(j.Duration.ofNanos(number))
			@inline def ms :Milliseconds = new Milliseconds(number)
			@inline def s :Milliseconds = new Milliseconds(number * MillisInSecond)
			@inline def µs :Milliseconds = new Milliseconds(number * 1000L)
			@inline def h :Milliseconds = new Milliseconds(number * MillisInHour)
		}
	}

}
