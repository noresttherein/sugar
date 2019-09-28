package net.noresttherein.slang.time

import java.{time => j}
import java.time.temporal.{ChronoField, TemporalField}

import net.noresttherein.slang.time.Cycle.Phase
import net.noresttherein.slang.time.TimePoint.TimeLimes



/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait Cycle {
	type Phase <: Cycle.Phase
	val Max :Phase
	val Min :Phase
	val Length :Long

	def toJava :TemporalField

	def of(time :TimePoint) :Phase = time(this)
	def of(time :UnixTime) :Phase = of(time in Time.Local.zone)
	def of(time :UTCDateTime) :Phase = of(time.toZoneDateTime)
	def of(time :ZoneDateTime) :Phase
	def of(time :Timestamp) :Phase = of(time in Time.Local.zone)
	def of(time :TimeLimes) :Phase = throw new UnsupportedOperationException(s"$this at $time")

	def ofLocal(time :DateTime) :Phase = of(time in Time.Local.zone)
}



object Cycle {
	trait Phase extends Any


	def apply(field :TemporalField) :Cycle { type Phase <: LongValuePhase } = new TemporalFieldCycle(field)


	class LongValuePhase(val no :Long) extends AnyVal with Phase with Ordered[LongValuePhase] {

		override def compare(that :LongValuePhase) :Int =
			if (no < that.no) -1
			else if (no > that.no) 1
			else 0
	}

	@inline implicit def fromJavaTemporalField(field :TemporalField) :Cycle { type Phase <: LongValuePhase } =
		new TemporalFieldCycle(field)

	@inline implicit def toJavaTemporalField(cycle :Cycle) :TemporalField = cycle.toJava



	private class TemporalFieldCycle(val toJava :TemporalField) extends Cycle with Serializable {
		type Phase = LongValuePhase
		val Max = new LongValuePhase(toJava.range().getMaximum)
		val Min = new LongValuePhase(toJava.range().getMinimum)
		val Length :Long = toJava.range().getMaximum -  toJava.range().getMinimum //todo: overflow

		override def of(time :ZoneDateTime) :LongValuePhase =
			new LongValuePhase(time.toJava.getLong(toJava))
	}
}






class Month private (val toJava :j.Month) extends AnyVal with Phase with Ordered[Month] with Serializable {
	@inline def no :Int = toJava.getValue
	@inline def toInt :Int = toJava.getValue
//	@inline def toJava :j.Month = j.Month.of(no)

	@inline def maxLength :Int = no match {
		case 2 => 29
		case n => 30 + (n + n / 8) % 2
	}

	@inline def minLength :Int = no match {
		case 2 => 28
		case n => 30 + (n + n / 8) % 2
	}

	@inline def length(year :Year) :Int = length(year.isLeap)

	def length(isLeapYear :Boolean) :Int = no match {
		case 2 => if (isLeapYear) 29 else 28
		case n => 30 + (n + n / 8) % 2
	}

	def of(year :Year) :MonthOfYear = MonthOfYear(year, this)
	def on(day :Int) :DateOfYear = DateOfYear(this, day)
	def onFirst :DateOfYear = DateOfYear(this, 1)

	@inline def +(months :Int) :Month = new Month(toJava plus months)
	@inline def -(months :Int) :Month = new Month(toJava minus months)


	@inline override def compare(that :Month) :Int = toJava.getValue - that.toJava.getValue
	@inline override def <=(that :Month) :Boolean = toJava.getValue <= that.toJava.getValue
	@inline override def < (that :Month) :Boolean = toJava.getValue < that.toJava.getValue
	@inline override def >=(that :Month) :Boolean = toJava.getValue >= that.toJava.getValue
	@inline override def > (that :Month) :Boolean = toJava.getValue > that.toJava.getValue

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



object Month extends Cycle with Serializable {
	type Phase = Month

	import j.Month._

	@inline final val January = new Month(JANUARY)
	@inline final val February = new Month(FEBRUARY)
	@inline final val March = new Month(MARCH)
	@inline final val April = new Month(APRIL)
	@inline final val May = new Month(MAY)
	@inline final val June = new Month(JUNE)
	@inline final val July = new Month(JULY)
	@inline final val August = new Month(AUGUST)
	@inline final val September = new Month(SEPTEMBER)
	@inline final val October = new Month(OCTOBER)
	@inline final val November = new Month(NOVEMBER)
	@inline final val December = new Month(DECEMBER)

	@inline final val Min = January
	@inline final val Max = December
	@inline final val Length = 12L

	@inline final val toJava = ChronoField.MONTH_OF_YEAR

	@inline def apply(number :Int) :Month = new Month(j.Month.of(number))

	def apply(month :j.Month) :Month = new Month(month)



	override def of(time :UTCDateTime) :Month = time.month
	override def of(time :ZoneDateTime) :Month = time.month
	override def ofLocal(time :DateTime) :Month = time.month
	def ofLocal(date :Date) :Month = date.month

	def unapply(date :Date) :Some[Int] = Some(date.month.no)

	def unapply(time :DateTime) :Some[Int] = Some(time.month.no)

	def unapply(time :ZoneDateTime) :Some[Int] = Some(time.month.no)

	@inline def now(implicit time :Time = Time.Local) :Month = new Month(j.LocalDate.now(time.clock).getMonth)


	@inline implicit def fromJavaMonth(month :j.Month) :Month = new Month(month)
	@inline implicit def toJavaMonth(month :Month) :j.Month = j.Month.of(month.no)

	object implicits {
		@inline implicit def monthToInt(month :Month) :Int = month.no
		@inline implicit def intToMonth(month :Int) :Month = Month(month)
	}

}






class DayOfWeek private (val no :Int) extends AnyVal with Phase with Ordered[DayOfWeek] with Serializable {
	@inline def toInt :Int = no

	@inline def toJava :j.DayOfWeek = j.DayOfWeek.of(no)

	@inline def +(days :Int) :DayOfWeek = new DayOfWeek((no + days - 1) % 7 + 1)
	@inline def -(days :Int) :DayOfWeek = new DayOfWeek((no - days - 1) % 7 + 1)


	@inline override def compare(that :DayOfWeek) :Int = no - that.no
	@inline override def <=(that :DayOfWeek) :Boolean = no <= that.no
	@inline override def < (that :DayOfWeek) :Boolean = no < that.no
	@inline override def >=(that :DayOfWeek) :Boolean = no >= that.no
	@inline override def > (that :DayOfWeek) :Boolean = no > that.no

	@inline def min(that :DayOfWeek) :DayOfWeek = if (no <= that.no) this else that
	@inline def max(that :DayOfWeek) :DayOfWeek = if (no >= that.no) this else that


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



object DayOfWeek extends Cycle {
	type Phase = DayOfWeek

	@inline final val Monday = new DayOfWeek(1)
	@inline final val Tuesday = new DayOfWeek(2)
	@inline final val Wednesday = new DayOfWeek(3)
	@inline final val Thursday = new DayOfWeek(4)
	@inline final val Friday = new DayOfWeek(5)
	@inline final val Saturday = new DayOfWeek(6)
	@inline final val Sunday = new DayOfWeek(7)

	@inline final val Min = Monday
	@inline final val Max = Sunday
	@inline final val Length = 7L

	@inline final val toJava :ChronoField = ChronoField.DAY_OF_WEEK

	@inline def apply(number :Int) :DayOfWeek =
		if (number < 1 || number > 7)
			throw new IllegalArgumentException("DayOfWeek(" + number + ")")
		else
			new DayOfWeek(number)

	@inline def apply(day :j.DayOfWeek) :DayOfWeek = new DayOfWeek(day.getValue)

	override def of(time :UTCDateTime) :DayOfWeek = time.dayOfWeek
	override def of(time :ZoneDateTime) :DayOfWeek = time.dayOfWeek

	@inline implicit def fromJavaDayOfWeek(day :j.DayOfWeek) :DayOfWeek = new DayOfWeek(day.getValue)
	@inline implicit def toJavaDayOfWeek(day :j.DayOfWeek) :j.DayOfWeek = day.toJava

	object implicits {
		@inline implicit def dayOfWeekToInt(day :DayOfWeek) :Int = day.no
		@inline implicit def intToDayOfWeek(day :Int) :DayOfWeek = DayOfWeek(day)
	}

}

//DayOfMonth
//DayOfYear
//HourOfDay
//MinuteOfHour
//SecondOFMinute
//MicroOfSecond
//NanoOfMicro
//
//Hour12
//AMPM
//Era
//YearOfEra
