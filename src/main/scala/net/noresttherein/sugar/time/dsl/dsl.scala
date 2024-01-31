package net.noresttherein.sugar.time

import java.{time => j}
import java.time.DateTimeException
import java.time.temporal.ChronoField

import net.noresttherein.sugar.time.Cycle.HourOfDay
import net.noresttherein.sugar.time.constants.{MillisInHour, MillisInMinute, MillisInSecond}
import net.noresttherein.sugar.time.dsl.partialTimeDesignators.{DateDayWithMonth, HourWithMinute, HourWithMinuteAndSecond}



/** Constants for temporal fields such as month and day names as well as extension methods for `Int` and `Long`
  * creating the classes from `net.noresttherein.sugar.time` in a naturally looking way,
  * such as `11 Jan 1981`, `11 :- 59 :- 59` or `1000_0000.nanos`.
  * @see [[net.noresttherein.sugar.time.dsl.ISOSymbolMethods$]]
  * @author Marcin Mościcki
  */ //todo: rename to syntax
package object dsl {

	final val Midnight = TimeOfDay(0, 0)
	final val Noon     = TimeOfDay(12, 0)

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

	@inline def today(implicit clock :Time = Time.Local) :Date = Date.now

	/** Extension methods which starts a call chain building a [[net.noresttherein.sugar.time.Date Date]],
	  * [[net.noresttherein.sugar.time.Anniversary DateOfYear]] or
	  * [[net.noresttherein.sugar.time.dsl.partialTimeDesignators.DateDayWithMonth DateDayWithMonth]],
	  * beginning with `this` `Int` as the day of month.
	  */
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

		@inline def Jan :Anniversary = Anniversary(dsl.Jan, day)
		@inline def Feb :Anniversary = Anniversary(dsl.Feb, day)
		@inline def Mar :Anniversary = Anniversary(dsl.Mar, day)
		@inline def Apr :Anniversary = Anniversary(dsl.Apr, day)
		@inline def May :Anniversary = Anniversary(dsl.May, day)
		@inline def Jun :Anniversary = Anniversary(dsl.Jun, day)
		@inline def Jul :Anniversary = Anniversary(dsl.Jul, day)
		@inline def Aug :Anniversary = Anniversary(dsl.Aug, day)
		@inline def Sep :Anniversary = Anniversary(dsl.Sep, day)
		@inline def Oct :Anniversary = Anniversary(dsl.Oct, day)
		@inline def Nov :Anniversary = Anniversary(dsl.Nov, day)
		@inline def Dec :Anniversary = Anniversary(dsl.Dec, day)

		@inline def :/(month :Int)   :DateDayWithMonth = DateDayWithMonth(Anniversary(Month(month), day))
		@inline def :/(month :Month) :DateDayWithMonth = DateDayWithMonth(Anniversary(month, day))

		/** Creates a `Year` of the [[net.noresttherein.sugar.time.Era.CE current era]] represented by this `Int`.
		  * A negative integer results in a year of that absolute value [[net.noresttherein.sugar.time.Era.BCE BCE]]
		  * instead. Zero is illegal.
		  */
		@throws[DateTimeException]("if this value is zero.")
		@inline def CE :Year =
			if (day == 0) throw SugaredDateTimeException("0.CE: no year zero in ISO chronology")
			else new Year(day)

		/** Creates a [[net.noresttherein.sugar.time.Era.BCE BCE]] year represented by this `Int`.
		  * A negative integer results in a year of that absolute value [[net.noresttherein.sugar.time.Era.CE CE]]
		  * instead. Zero is illegal.
		  */
		@throws[DateTimeException]("if this value is zero.")
		@inline def BCE :Year =
			if (day == 0) throw SugaredDateTimeException("0.BCE: no year zero in ISO chronology")
			else new Year(1 - day)
	}



	/** An `Int` extension method creating a [[net.noresttherein.sugar.time.TimeOfDay TimeOfDay]] using the syntax
	  * of `hour :- minute`.
	  */
	implicit class TimeOfDayFactoryMethod(private val hour :Int) extends AnyVal {
		@inline def :-(minute :Int) :HourWithMinute = new HourWithMinute(j.LocalTime.of(hour, minute))

		def am :TimeOfDayFactoryMethod =
			if (hour == 12)
				new TimeOfDayFactoryMethod(0)
			else if (hour >= 0 & hour <= 11)
				new TimeOfDayFactoryMethod(hour)
			else
				throw SugaredDateTimeException(hour.toString + "am")

		def pm :TimeOfDayFactoryMethod =
			if (hour == 12)
				new TimeOfDayFactoryMethod(12)
			else if (hour >= 0 & hour <= 11)
				new TimeOfDayFactoryMethod(12 + hour)
			else
				throw SugaredDateTimeException(hour.toString + "pm")
	}



	object partialTimeDesignators {

		/** A wrapper of [[net.noresttherein.sugar.time.Anniversary DateOfYear]] adding a factory method
		  * [[net.noresttherein.sugar.time.dsl.partialTimeDesignators.DateDayWithMonth.:/ :/]] creating
		  * a [[net.noresttherein.sugar.time.Date Date]].
		  *
		  * There is an implicit conversion from this value to the wrapped `DateOfYear`.
		  * The whole method chain enables the syntax of `day :/ month :/ year`.
		  */
		class DateDayWithMonth private[time] (private val dateOfYear :Anniversary) extends AnyVal {
			/** Factory method creating a full date, made public by type alias
			  * [[net.noresttherein.sugar.time.dsl.partialTimeDesignators.DateDayWithMonth DateDayWithMonth]].
			  * Enables the syntax of `day :/ month :/ year`.
			  * Note that this can further be followed by [[net.noresttherein.sugar.time.Date.at at]] `time`,
			  * to the total syntax of `day :/ month :/ year at hour :- minute`
			  * (and, optionally, `:- second :- nanosecond`).
			  */
			@inline def :/(year :Int) :Date = new Date(j.LocalDate.of(year, dateOfYear.month, dateOfYear.day))
		}

		object DateDayWithMonth {
			@inline private[time] def apply(dateOfYear :Anniversary) :DateDayWithMonth = new DateDayWithMonth(dateOfYear)
			@inline implicit def toDateOfYear(dayMonth :DateDayWithMonth) :Anniversary = dayMonth.dateOfYear
		}

		/** A wrapper of [[net.noresttherein.sugar.time.TimeOfDay TimeOfDay]] adding a factory method
		  * [[net.noresttherein.sugar.time.dsl.partialTimeDesignators.HourWithMinute.:- :-]] creating
		  * a [[net.noresttherein.sugar.time.dsl.partialTimeDesignators.HourWithMinuteAndSecond HourWithMinuteAndSecond]]
		  * - another wrapper of `TimeOfDay`, allowing further refinement of the hour by specifying nanoseconds
		  * of the given second.
		  *
		  * There is an implicit conversion from this value to `TimeOfDay`. The whole method chain enables the syntax
		  * of `hour :- minute :- second :- nanosecond`, where specifying the nanosecond and second is optional.
		  */
		class HourWithMinute private[time] (private val timeOfDay :j.LocalTime) extends AnyVal {
			@inline def :-(second :Int) :HourWithMinuteAndSecond =
				new HourWithMinuteAndSecond(timeOfDay.plusSeconds(second))
		}

		object HourWithMinute {
			@inline private[time] def apply(timeOfDay :j.LocalTime) :HourWithMinute = new HourWithMinute(timeOfDay)

			@inline implicit def toTimeOfDay(hourMinute :HourWithMinute) :TimeOfDay =
				new TimeOfDay(hourMinute.timeOfDay)
		}

		/** A wrapper of [[net.noresttherein.sugar.time.TimeOfDay TimeOfDay]] adding a factory method
		  * [[net.noresttherein.sugar.time.dsl.partialTimeDesignators.HourWithMinuteAndSecond.:- :-]] creating
		  * a `TimeOfDay` the specified number of nanoseconds after this moment.
		  *
		  * There is an implicit conversion from this value to `TimeOfDay`. The whole method chain enables the syntax
		  * of `hour :- minute :- second :- nanosecond`, where specifying the nanosecond and second is optional.
		  */
		class HourWithMinuteAndSecond private[time] (private val timeOfDay :j.LocalTime) extends AnyVal {
			@inline def :-(nanosecond :Int) :TimeOfDay =
				new TimeOfDay(timeOfDay.`with`(ChronoField.NANO_OF_SECOND, nanosecond))
		}

		object HourWithMinuteAndSecond {
			@inline private[time] def apply(timeOfDay :j.LocalTime) = new HourWithMinuteAndSecond(timeOfDay)
			@inline implicit def toTimeOfDay(hms :HourWithMinuteAndSecond) :TimeOfDay = new TimeOfDay(hms.timeOfDay)
		}
	}




	/** Extension methods for `Long` adding natural language factory methods of
	  * [[net.noresttherein.sugar.time.TimeSpan time spans]] by specifying a time unit.
	  * They differ from `Int` [[net.noresttherein.sugar.time.dsl.IntTimeLapseMethods extension methods]]
	  * in that the result is a [[net.noresttherein.sugar.time.Duration Duration]] in all cases
	  * with the exception of `this.`[[net.noresttherein.sugar.time.dsl.LongTimeLapseMethods.millis millis]],
	  * which return [[net.noresttherein.sugar.time.Milliseconds Milliseconds]], while the latter
	  * always creates `Milliseconds`, with the exception of
	  * [[net.noresttherein.sugar.time.dsl.IntTimeLapseMethods.nanos nanos]], which create `Duration` instead.
	  */
	implicit class LongTimeLapseMethods(private val number :Long) extends AnyVal {
		@inline def nanos   :Duration = new Duration(j.Duration.ofNanos(number))
		@inline def millis  :Milliseconds = new Milliseconds(number)
		@inline def seconds :Duration = new Duration(j.Duration.ofSeconds(number))
		@inline def minutes :Duration = new Duration(j.Duration.ofMinutes(number))
		@inline def hours   :Duration = new Duration(j.Duration.ofHours(number))
		@inline def days    :Duration = new Duration(j.Duration.ofDays(number))
	}

	/** Extension methods for `Int` adding natural language factory methods of
	  * [[net.noresttherein.sugar.time.TimeSpan time spans]] by specifying a time unit.
	  * This differs from `Long` [[net.noresttherein.sugar.time.dsl.LongTimeLapseMethods extension methods]]
	  * in that the return type is [[net.noresttherein.sugar.time.Milliseconds Milliseconds]] in all cases
	  * with the exception of `this.`[[net.noresttherein.sugar.time.dsl.IntTimeLapseMethods.nanos nanos]],
	  * which return a [[net.noresttherein.sugar.time.Duration Duration]], while the latter
	  * always creates a `Duration`, with the exception of
	  * [[net.noresttherein.sugar.time.dsl.LongTimeLapseMethods.millis millis]], which create `Milliseconds` instead.
	  * Additionally, these methods include also [[net.noresttherein.sugar.time.Period periods]] of the specified
	  * number of days/months/years.
	  */
	implicit class IntTimeLapseMethods(private val number :Int) extends AnyVal {
		@inline def nanos   :Duration = new Duration(j.Duration.ofNanos(number))
		@inline def millis  :Milliseconds = new Milliseconds(number)
		@inline def seconds :Milliseconds =	new Milliseconds(number * MillisInSecond)
		@inline def minutes :Milliseconds = new Milliseconds(number * MillisInMinute)
		@inline def hours   :Milliseconds = new Milliseconds(number * MillisInHour)

		@inline def days   :Period = j.Period.ofDays(number)
		@inline def months :Period = j.Period.ofMonths(number)
		@inline def years  :Period = j.Period.ofYears(number)

		/** An hour of day, representing an integer from the range of `0..23` as an aspect/property of various
		  * [[net.noresttherein.sugar.time.DefiniteTime DefiniteTime]] subtypes.
		  */
		@inline def oClock :HourOfDay.Phase = HourOfDay(number)
	}


	/** Groups extension methods of `Int` and `Long` named after ISO time unit symbols, creating
	  * [[net.noresttherein.sugar.time.TimeSpan time spans]] of the specified length.
	  * They need to be imported separately from `dsl._` due to a higher likelihood of name conflicts
	  * with extension methods from other libraries.
	  */
	object ISOSymbolMethods {
		/** `Long` extension methods creating a [[net.noresttherein.sugar.time.Duration Duration]]
		  * (or [[net.noresttherein.sugar.time.Milliseconds Milliseconds]]) of this length in the specified units.
		  * They differ from `Int` [[net.noresttherein.sugar.time.dsl.ISOSymbolMethods.IntISOSymbols extension methods]]
		  * in that the latter return `Milliseconds` (with the exception of
		  * [[net.noresttherein.sugar.time.dsl.ISOSymbolMethods.IntISOSymbols.ns nanoseconds]], returning
		  * a [[net.noresttherein.sugar.time.Duration Duration]]), while these favor `Duration`,
		  * with only [[net.noresttherein.sugar.time.dsl.ISOSymbolMethods.LongISOSymbols.ms milliseconds]]
		  * returning `Milliseconds`.
		  */
		implicit class LongISOSymbols(private val number :Long) extends AnyVal {
			@inline def ns :Duration = new Duration(j.Duration.ofNanos(number))
			@inline def ms :Milliseconds = new Milliseconds(number)
			@inline def s  :Duration = new Duration(j.Duration.ofSeconds(number))

			@inline def µs :Duration =
				new Duration(j.Duration.ofSeconds(number / 1000000L, number % 1000000L * 1000L))

			@inline def h  :Duration = new Duration(j.Duration.ofDays(number))
		}

		/** `Int` extension methods creating [[net.noresttherein.sugar.time.Milliseconds Milliseconds]]
		  * (or a [[net.noresttherein.sugar.time.Duration Duration]]) of this length in the specified units.
		  * They differ from `Long` [[net.noresttherein.sugar.time.dsl.ISOSymbolMethods.LongISOSymbols extension methods]]
		  * in that the latter return `Duration` (with the exception of
		  * [[net.noresttherein.sugar.time.dsl.ISOSymbolMethods.LongISOSymbols.ms milliseconds]], returning
		  * `Milliseconds`), while these favor `Milliseconds` instead,
		  * with only [[net.noresttherein.sugar.time.dsl.ISOSymbolMethods.IntISOSymbols.ns nanoseconds]]
		  * returning a `Duration`.
		  */
		implicit class IntISOSymbols(private val number :Int) extends AnyVal {
			@inline def ns :Duration = new Duration(j.Duration.ofNanos(number))
			@inline def ms :Milliseconds = new Milliseconds(number)
			@inline def s  :Milliseconds = new Milliseconds(number * MillisInSecond)
			@inline def µs :Milliseconds = new Milliseconds(number * 1000L)
			@inline def h  :Milliseconds = new Milliseconds(number * MillisInHour)
		}
	}

}
