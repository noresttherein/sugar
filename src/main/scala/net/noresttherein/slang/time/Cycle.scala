package net.noresttherein.slang.time

import java.{time => j}
import java.time.temporal.{ChronoField, TemporalField}
import java.time.temporal.ChronoField._

import net.noresttherein.slang.time.Cycle.Phase



/** A periodically repeating sequence of time periods, such as a month of the year or a day of the week.
  * Instances of this class represent the whole cycle, while each individual period within it, such as January
  * or Tuesday is represented by the member type `Cycle#`[[net.noresttherein.slang.time.Cycle.Phase Phase]].
  * The convention here is that a class, such as [[net.noresttherein.slang.time.Month Month]], implements `Phase`,
  * while its companion object (for example, [[net.noresttherein.slang.time.Month$ Month object]]
  * implements the `Cycle` of that phase.
  *
  * The rough counterpart of this type in  `java.time` package is [[java.time.temporal.TemporalField TemporalField]].
  * @author Marcin Mościcki
  */
trait Cycle extends Serializable {
	/** The type representing a single, specific iteration of this cycle. */
	type Phase <: Cycle.Phase with Ordered[Phase]
	/** The last phase in this cycle. */
	val Max :Phase
	/** The first phase in this cycle. */
	val Min :Phase
	/** The number of phases in this cycle. */
	val Length :Long

	def toJava :TemporalField

	/** The current phase of this cycle at the particular moment in time. */
	def on(time :TimePoint)    :Phase = time(this) //todo: shouldn't these take a Time?

	/** The current phase of this cycle at the particular moment in time.
	  * Uses `Time.`[[net.noresttherein.slang.time.Time.Local Local]] as the reference.
	  */
	def on(time :PosixTime)    :Phase = on(time in Time.Local.zone)

	/** The current phase of this cycle at the particular moment in time. */
	def on(time :UTCDateTime)  :Phase = on(time.toZoneDateTime)

	/** The current phase of this cycle at the particular moment in time. */
	def on(time :ZoneDateTime) :Phase

	/** The current phase of this cycle at the particular moment in time.
	  * Uses `Time.`[[net.noresttherein.slang.time.Time.Local Local]] as the reference.
	  */
	def on(time :Timestamp)    :Phase = on(time in Time.Local.zone)
//	def of(time :TimeLimes) :Phase = throw new UnsupportedOperationException(s"$this at $time")
	/** The current phase of this cycle on the particular date in the [[net.noresttherein.slang.time.Time.Local local]]
	  * [[net.noresttherein.slang.time.TimeZone time zone]].
	  */
	def ofLocal(time :DateTime) :Phase = on(time in Time.Local.zone)

	/** Extracts the current phase of this cycle at the particular moment in time. Equivalent to `this.on(time)`. */
	@inline def unapply(time :TimePoint)    :Some[Phase] = Some(on(time))
	/** Extracts the current phase of this cycle at the particular moment in time. Equivalent to `this.on(time)`. */
	@inline def unapply(time :PosixTime)    :Some[Phase] = Some(on(time))
	/** Extracts the current phase of this cycle at the particular moment in time. Equivalent to `this.on(time)`. */
	@inline def unapply(time :UTCDateTime)  :Some[Phase] = Some(on(time))
	/** Extracts the current phase of this cycle at the particular moment in time. Equivalent to `this.on(time)`. */
	@inline def unapply(time :ZoneDateTime) :Some[Phase] = Some(on(time))
	/** Extracts the current phase of this cycle at the particular moment in time. Equivalent to `this.on(time)`. */
	@inline def unapply(time :Timestamp)    :Some[Phase] = Some(on(time))
//	@inline def unapply(time :TimeLimes) :Option[Phase] = None

}



/** A factory and deconstructor of temporal [[net.noresttherein.slang.time.Cycle Cycle]]s.
  * Serves also as a name space providing the values of all (or most) cycles occurring in the Gregorian calendar.
  */
object Cycle {
	trait Phase extends Any with Serializable


	def apply(field :TemporalField) :LongValueCycle = new TemporalFieldCycle(field)

	def unapply(cycle :Cycle) :Some[TemporalField] = Some(cycle.toJava)


	@inline implicit def fromJavaTemporalField(field :TemporalField) :LongValueCycle =
		new TemporalFieldCycle(field)

	@inline implicit def toJavaTemporalField(cycle :Cycle) :TemporalField = cycle.toJava


	final val Epoch          = Cycle(ERA)
	final val YearOfEra      = Cycle(YEAR_OF_ERA)
	final val MonthOfYear    = Cycle(MONTH_OF_YEAR)
	final val DayOfYear      = Cycle(DAY_OF_YEAR)
	final val DayOfMonth     = Cycle(DAY_OF_MONTH)
	final val DayOfWeek      = Cycle(DAY_OF_WEEK)
	final val AMPM :AMPM     = new TemporalFieldCycle(AMPM_OF_DAY) with AMPM
	final val HourOfDay      = Cycle(HOUR_OF_DAY)
	final val ClockHour24    = Cycle(CLOCK_HOUR_OF_DAY)
	final val Hour12         = Cycle(HOUR_OF_AMPM)
	final val ClockHour      = Cycle(CLOCK_HOUR_OF_AMPM)
	final val MinuteOfDay    = Cycle(MINUTE_OF_DAY)
	final val MinuteOfHour   = Cycle(MINUTE_OF_HOUR)
	final val SecondOfDay    = Cycle(SECOND_OF_DAY)
	final val SecondOfMinute = Cycle(SECOND_OF_MINUTE)
	final val MicroOfDay     = Cycle(MICRO_OF_DAY)
	final val MicroOfSecond  = Cycle(MICRO_OF_SECOND)
	final val MilliOfDay     = Cycle(MILLI_OF_DAY)
	final val MilliOfSecond  = Cycle(MILLI_OF_SECOND)
	final val NanoOfDay      = Cycle(NANO_OF_DAY)
	final val NanoOfSecond   = Cycle(NANO_OF_SECOND)

	final val EpochDay       = Cycle(EPOCH_DAY)
	final val EpochSecond    = Cycle(INSTANT_SECONDS)



	trait AMPM extends LongValueCycle {
		final val AM = apply(0)
		final val PM = apply(1)
	}



	/** A phase of a `Cycle` described simply by its increasing ordinal number of type `Long`. */
	@SerialVersionUID(1L)
	class LongValuePhase[C](val no :Long) extends AnyVal with Phase with Ordered[LongValuePhase[C]] {

		override def compare(that :LongValuePhase[C]) :Int =
			if (no < that.no) -1
			else if (no > that.no) 1
			else 0
	}


	/** A cycle whose each individual [[net.noresttherein.slang.time.Cycle.Phase Phase]] can be identified
	  * by a single `Long` value.
	  * @see [[net.noresttherein.slang.time.Cycle.LongValuePhase]]
	  */
	trait LongValueCycle extends Cycle {
		type Phase <: LongValuePhase[this.type] with Ordered[Phase]

		/** The n-th phase of this cycle. */
		def apply(phase :Long) :Phase
	}

	@SerialVersionUID(1L)
	private[time] class TemporalFieldCycle(val toJava :TemporalField) extends LongValueCycle {
		override type Phase = LongValuePhase[this.type]

		override val Max = new Phase(toJava.range().getMaximum)
		override val Min = new Phase(toJava.range().getMinimum)

		override val Length :Long =
			if (if (Min.no >= 0) Max.no >= Long.MinValue + Min.no else Max.no <= Long.MaxValue + Min.no)
				Max.no - Min.no
			else
				-1

		override def apply(phase :Long) :LongValuePhase[this.type] =
			if (phase < Min.no | phase > Max.no)
				throw new IllegalArgumentException("Phase " + phase +" is out of range for " + this)
			else
				new LongValuePhase[this.type](phase)

		override def on(time :ZoneDateTime) :Phase =
			new LongValuePhase(time.toJava.getLong(toJava))

		override def equals(that :Any) :Boolean = that match {
			case cycle :TemporalFieldCycle => toJava == cycle.toJava
//			case Month => toJava == MONTH_OF_YEAR
//			case Day => toJava == DAY_OF_WEEK
			case _ => false
		}

		override def hashCode :Int = toJava.hashCode

		override def toString :String = toJava.toString
	}
}






/** Month of the year, from January (as numeric `1`), to December (as numeric `12`). */
@SerialVersionUID(1L)
class Month private (val toJava :j.Month) extends AnyVal with Phase with Ordered[Month] with Serializable {
	@inline def no    :Int = toJava.getValue
	@inline def toInt :Int = toJava.getValue
//	@inline def toJava :j.Month = j.Month.of(no)

	def maxLength :Int = no match {
		case 2 => 29
		case n => 30 + (n + n / 8) % 2
	}
	def minLength :Int = no match {
		case 2 => 28
		case n => 30 + (n + n / 8) % 2
	}
	@inline def length(year :Year) :Int = length(year.isLeap)

	def length(isLeapYear :Boolean) :Int = no match {
		case 2 => if (isLeapYear) 29 else 28
		case n => 30 + (n + n / 8) % 2
	}

	@inline def of(year :Year) :MonthInYear = MonthInYear(year, this)
	@inline def on(day :Int)   :DateOfYear  = DateOfYear(this, day)
	@inline def onFirst        :DateOfYear  = DateOfYear(this, 1)

	@inline def +(months :Int) :Month = new Month(toJava plus months)
	@inline def -(months :Int) :Month = new Month(toJava minus months)


	@inline override def compare(that :Month) :Int     = toJava.getValue - that.toJava.getValue
	@inline override def <=(that :Month)      :Boolean = toJava.getValue <= that.toJava.getValue
	@inline override def < (that :Month)      :Boolean = toJava.getValue < that.toJava.getValue
	@inline override def >=(that :Month)      :Boolean = toJava.getValue >= that.toJava.getValue
	@inline override def > (that :Month)      :Boolean = toJava.getValue > that.toJava.getValue

	@inline def min(that :Month) :Month = if (toJava.getValue <= that.toJava.getValue) this else that
	@inline def max(that :Month) :Month = if (toJava.getValue >= that.toJava.getValue) this else that

	@inline def digits :String = twoDigit(no)

	def name :String = {
		val num = no
		if (num <= 6)
			if (num <= 3) no match {
				case 1 => "January"
				case 2 => "February"
				case 3 => "March"
			} else num match {
				case 4 => "April"
				case 5 => "May"
				case 6 => "June"
			}
		else
			if (num <= 9) num match {
				case 7 => "July"
				case 8 => "August"
				case 9 => "September"
			} else num match {
				case 10 => "October"
				case 11 => "November"
				case 12 => "December"
			}
	}

	override def toString :String = {
		val num = toJava.getValue
		if (num <= 6)
			if (num <= 3) no match {
				case 1 => "Jan"
				case 2 => "Feb"
				case 3 => "Mar"
			} else num match {
				case 4 => "Apr"
				case 5 => "May"
				case 6 => "Jun"
			}
		else
			if (num <= 9) num match {
				case 7 => "Jul"
				case 8 => "Aug"
				case 9 => "Sep"
			} else num match {
				case 10 => "Oct"
				case 11 => "Nov"
				case 12 => "Dec"
			}
	}
}



/** The cycle of months of a year. */
object Month extends Cycle {
	type Phase = Month

	import j.Month._

	@inline final val January   = new Month(JANUARY)
	@inline final val February  = new Month(FEBRUARY)
	@inline final val March     = new Month(MARCH)
	@inline final val April     = new Month(APRIL)
	@inline final val May       = new Month(MAY)
	@inline final val June      = new Month(JUNE)
	@inline final val July      = new Month(JULY)
	@inline final val August    = new Month(AUGUST)
	@inline final val September = new Month(SEPTEMBER)
	@inline final val October   = new Month(OCTOBER)
	@inline final val November  = new Month(NOVEMBER)
	@inline final val December  = new Month(DECEMBER)

	@inline final val Min    = January
	@inline final val Max    = December
	@inline final val Length = 12L

	@inline final val toJava = MONTH_OF_YEAR

	@inline def apply(number :Int)    :Month = new Month(j.Month.of(number))
	@inline def apply(month :j.Month) :Month = new Month(month)


	@inline override def on(time :UTCDateTime)   :Month = time.month
	@inline override def on(time :ZoneDateTime)  :Month = time.month
	@inline override def ofLocal(time :DateTime) :Month = time.month
	@inline def ofLocal(date :Date) :Month = date.month

	@inline def unapply(date :Date)     :Some[Month] = Some(date.month)
	@inline def unapply(time :DateTime) :Some[Month] = Some(time.month)

	@inline def now(implicit time :Time = Time.Local) :Month = new Month(j.LocalDate.now(time.clock).getMonth)


	@inline implicit def fromJavaMonth(month :j.Month) :Month = new Month(month)
	@inline implicit def toJavaMonth(month :Month) :j.Month = j.Month.of(month.no)

	object implicits {
		@inline implicit def monthToInt(month :Month) :Int = month.no
		@inline implicit def intToMonth(month :Int) :Month = Month(month)
	}
}






/** Day of week represented by a numeral from the `1..7` range, starting with Monday as `1`. */
@SerialVersionUID(1L)
class Day private(val no :Int) extends AnyVal with Phase with Ordered[Day] with Serializable {
	@inline def toInt :Int = no

	@inline def toJava :j.DayOfWeek = j.DayOfWeek.of(no)

	@inline def +(days :Int) :Day = new Day((no + days - 1) % 7 + 1)
	@inline def -(days :Int) :Day = new Day((no - days - 1) % 7 + 1)


	@inline override def compare(that :Day) :Int = no - that.no
	@inline override def <=(that :Day) :Boolean = no <= that.no
	@inline override def < (that :Day) :Boolean = no < that.no
	@inline override def >=(that :Day) :Boolean = no >= that.no
	@inline override def > (that :Day) :Boolean = no > that.no

	@inline def min(that :Day) :Day = if (no <= that.no) this else that
	@inline def max(that :Day) :Day = if (no >= that.no) this else that


	override def toString :String =
		if (no <= 4)
			if (no <= 2)
				if (no == 1) "Mon" else "Tue"
			else
			if (no == 3) "Wed" else "Thu"
		else
	        no match {
				case 5 => "Fri"
				case 6 => "Sat"
				case _ => "Sun"
			}

}



/** The cycle of days of the week. */
object Day extends Cycle {
	type Phase = Day

	@inline final val Monday    = new Day(1)
	@inline final val Tuesday   = new Day(2)
	@inline final val Wednesday = new Day(3)
	@inline final val Thursday  = new Day(4)
	@inline final val Friday    = new Day(5)
	@inline final val Saturday  = new Day(6)
	@inline final val Sunday    = new Day(7)

	@inline final val Min = Monday
	@inline final val Max = Sunday
	@inline final val Length = 7L

	@inline final val toJava :ChronoField = DAY_OF_WEEK

	@inline def apply(number :Int) :Day =
		if (number < 1 || number > 7)
			throw new IllegalArgumentException("Day(" + number + ")")
		else
			new Day(number)

	@inline def apply(day :j.DayOfWeek) :Day = new Day(day.getValue)

	@inline override def on(time :UTCDateTime)  :Day = time.dayOfWeek
	@inline override def on(time :ZoneDateTime) :Day = time.dayOfWeek

	@inline implicit def fromJavaDayOfWeek(day :j.DayOfWeek) :Day = new Day(day.getValue)
	@inline implicit def toJavaDayOfWeek(day :j.DayOfWeek) :j.DayOfWeek = day.toJava

	object implicits {
		@inline implicit def dayOfWeekToInt(day :Day) :Int = day.no
		@inline implicit def intToDayOfWeek(day :Int) :Day = Day(day)
	}
}

