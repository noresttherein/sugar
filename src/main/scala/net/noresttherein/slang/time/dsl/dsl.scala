package net.noresttherein.slang.time

import java.{time => j}
import java.time.DateTimeException

import net.noresttherein.slang.time.DateOfYear.DateDayWithMonth
import net.noresttherein.slang.time.TimeOfDay.HourWithMinute



/**
  * @author Marcin Mościcki marcin@moscicki.net
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

	@inline final val Mon = DayOfWeek.Monday
	@inline final val Tue = DayOfWeek.Tuesday
	@inline final val Wed = DayOfWeek.Wednesday
	@inline final val Thu = DayOfWeek.Thursday
	@inline final val Fri = DayOfWeek.Friday
	@inline final val Sat = DayOfWeek.Saturday
	@inline final val Sun = DayOfWeek.Sunday



	implicit class DateFactoryMethods(private val day :Int) extends AnyVal {
		def Jan(year :Int) :Date = Date(year, 1, day)
		def Feb(year :Int) :Date = Date(year, 2, day)
		def Mar(year :Int) :Date = Date(year, 3, day)
		def Apr(year :Int) :Date = Date(year, 4, day)
		def May(year :Int) :Date = Date(year, 5, day)
		def Jun(year :Int) :Date = Date(year, 6, day)
		def Jul(year :Int) :Date = Date(year, 7, day)
		def Aug(year :Int) :Date = Date(year, 8, day)
		def Sep(year :Int) :Date = Date(year, 9, day)
		def Oct(year :Int) :Date = Date(year, 10, day)
		def Nov(year :Int) :Date = Date(year, 11, day)
		def Dec(year :Int) :Date = Date(year, 12, day)

		def Jan :DateOfYear = DateOfYear(dsl.Jan, day)
		def Feb :DateOfYear = DateOfYear(dsl.Feb, day)
		def Mar :DateOfYear = DateOfYear(dsl.Mar, day)
		def Apr :DateOfYear = DateOfYear(dsl.Apr, day)
		def May :DateOfYear = DateOfYear(dsl.May, day)
		def Jun :DateOfYear = DateOfYear(dsl.Jun, day)
		def Jul :DateOfYear = DateOfYear(dsl.Jul, day)
		def Aug :DateOfYear = DateOfYear(dsl.Aug, day)
		def Sep :DateOfYear = DateOfYear(dsl.Sep, day)
		def Oct :DateOfYear = DateOfYear(dsl.Oct, day)
		def Nov :DateOfYear = DateOfYear(dsl.Nov, day)
		def Dec :DateOfYear = DateOfYear(dsl.Dec, day)

		def :/(month :Int) :DateDayWithMonth = new DateDayWithMonth(DateOfYear(Month(month), day))
		def :/(month :Month) :DateDayWithMonth = new DateDayWithMonth(DateOfYear(month, day))


		@inline def CE :Year =
			if (day == 0) throw new DateTimeException("0.CE: no year zero in ISO chronology")
			else new Year(day)

		@inline def BCE :Year =
			if (day == 0) throw new DateTimeException("0.BCE: no year zero in ISO chronology")
			else new Year(1 - day)
	}

	implicit class TimeOfDayFactoryMethod(private val hour :Int) extends AnyVal {
		def :-(minute :Int) :HourWithMinute = new HourWithMinute(j.LocalTime.of(hour, minute))
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
