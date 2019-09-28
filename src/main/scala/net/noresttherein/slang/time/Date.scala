package net.noresttherein.slang.time

import java.{time => j}
import java.time.chrono.IsoEra



/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
class Date private[time] (val toJava :j.LocalDate) extends AnyVal with Ordered[Date] with Serializable {
	@inline def day :Int = toJava.getDayOfMonth
	@inline def dayOfWeek :DayOfWeek = toJava.getDayOfWeek
	@inline def dayOfYear :Int = toJava.getDayOfYear
	@inline def month :Month = toJava.getMonth
	@inline def year :Year = new Year(toJava.getYear)
	@inline def era :Era = new Era(toJava.getEra)

	@inline def daysInMonth :Int = toJava.lengthOfMonth
	@inline def daysInYear :Int = toJava.lengthOfYear

	@inline def midnight :DateTime = new DateTime(toJava.atStartOfDay)
	@inline def midnight(zone :TimeZone) :ZoneDateTime = new ZoneDateTime(toJava.atStartOfDay(zone.toJava))
	@inline def at(time :TimeOfDay) :DateTime = new DateTime(j.LocalDateTime.of(toJava, time.toJava))

	@inline def copy(year :Year = this.year, month :Month = this.month, day :Int = this.day) :Date =
		new Date(j.LocalDate.of(year.no, month.no, day))

	@inline def +(period :FiniteDateSpan) :Date = new Date(toJava plus period.toPeriod.toJava)
	@inline def -(period :FiniteDateSpan) :Date = new Date(toJava minus period.toPeriod.toJava)

	@inline def +(period :Period) :Date = new Date(toJava plus period.toJava)
	@inline def -(period :Period) :Date = new Date(toJava minus period.toJava)

	@inline def -(date :Date) :Period = new Period(j.Period.between(date.toJava, toJava))

	@inline override def compare(that :Date) :Int = toJava compareTo that

	@inline override def <=(that :Date) :Boolean = !(toJava isAfter that.toJava)
	@inline override def < (that :Date) :Boolean = toJava isBefore that.toJava
	@inline override def >=(that :Date) :Boolean = !(toJava isBefore that.toJava)
	@inline override def > (that :Date) :Boolean = toJava isAfter that.toJava

	@inline def min(that :Date) :Date = if (that.toJava isBefore toJava) that else this
	@inline def max(that :Date) :Date = if (that.toJava isAfter toJava) that else this


	override def toString :String = twoDigit(day) + " " + month + " " + year

}



object Date {
	@inline def apply(year :Int, month :Int, day :Int) :Date = new Date(j.LocalDate.of(year, month, day))

	@inline def apply(date :j.LocalDate) :Date = new Date(date)

	@inline def apply()(implicit time :Time = Time.Local) :Date = new Date(j.LocalDate.now(time.clock))

	@inline def now(implicit time :Time = Time.Local) :Date = new Date(j.LocalDate.now(time.clock))
	@inline def today(implicit time :Time = Time.Local) :Date =	new Date(j.LocalDate.now(time.clock))

	@inline implicit def fromJava(date :j.LocalDate) :Date = new Date(date)
	@inline implicit def toJava(date :Date) :j.LocalDate = date.toJava

}






class DateOfYear private (private val dayAndMonth :Int) extends Ordered[DateOfYear] with Serializable {

	@inline def day :Int = dayAndMonth & 0x1f
	@inline def month :Month = Month(dayAndMonth >> 5)

	@inline def toJava :j.MonthDay = j.MonthDay.of(month, day)

	@inline def of(year :Year) :Date = new Date(j.LocalDate.of(year, month, day))

	@inline def ofYear(year :Int) :Date = new Date(j.LocalDate.of(year, month, day))

	@inline def thisYear(implicit time :Time = Time.Local) :Date = ofYear(time.date.year)

	def next(implicit time :Time = Time.Local) :Date = {
		val today = time.date
		val date = ofYear(today.year)
		if (today <= date)
			date
		else
			ofYear(today.year + 1)
	}

	@inline def copy(month :Month = this.month, day :Int = this.day) :DateOfYear =
		if (day < 1 || day > month.maxLength)
			throw new IllegalArgumentException(s"month $month does not have $day days")
		else new DateOfYear(month.no << 5 | day)



	@inline override def compare(that :DateOfYear) :Int = dayAndMonth - that.dayAndMonth

	@inline override def <=(that :DateOfYear) :Boolean = dayAndMonth <= that.dayAndMonth
	@inline override def < (that :DateOfYear) :Boolean = dayAndMonth < that.dayAndMonth
	@inline override def >=(that :DateOfYear) :Boolean = dayAndMonth >= that.dayAndMonth
	@inline override def > (that :DateOfYear) :Boolean = dayAndMonth > that.dayAndMonth

	@inline def min(that :DateOfYear) :DateOfYear = if (dayAndMonth <= that.dayAndMonth) this else that
	@inline def max(that :DateOfYear) :DateOfYear = if (dayAndMonth >= that.dayAndMonth) this else that



	override def toString :String =
		twoDigit(day) + "." + month

}



object DateOfYear {

	def apply(month :Month, day :Int) :DateOfYear =
		if (day < 1 || day > month.maxLength)
			throw new IllegalArgumentException("Month " + month.name + " does not have " + day + " days")
		else
	        new DateOfYear(month.no << 5 | day)

	@inline def apply(monthDay :j.MonthDay) :DateOfYear =
		new DateOfYear(monthDay.getMonthValue << 5 | monthDay.getDayOfMonth)


	@inline implicit def fromJavaMonthDay(monthDay :j.MonthDay) :DateOfYear =
		new DateOfYear(monthDay.getMonthValue << 5 | monthDay.getDayOfMonth)

	@inline implicit def toJavaMonthDay(date :DateOfYear) :j.MonthDay =
		j.MonthDay.of(date.month.no, date.day)


	@inline def apply()(implicit time :Time = Time.Local) :DateOfYear = {
		val date = j.MonthDay.now(time.clock)
		new DateOfYear(date.getMonthValue << 5 | date.getDayOfMonth)
	}

	@inline def now(implicit time :Time = Time.Local) :DateOfYear = {
		val date = j.MonthDay.now(time.clock)
		new DateOfYear(date.getMonthValue << 5 | date.getDayOfMonth)
	}

	@inline def today(implicit time :Time = Time.Local) :DateOfYear = {
		val date = j.MonthDay.now(time.clock)
		new DateOfYear(date.getMonthValue << 5 | date.getDayOfMonth)
	}




	class DateDayWithMonth(private val dateOfYear :DateOfYear) extends AnyVal {
		def :/(year :Int) :Date = new Date(j.LocalDate.of(year, dateOfYear.month, dateOfYear.day))
	}

	object DateDayWithMonth {
		@inline implicit def toDateOfYear(dayMonth :DateDayWithMonth) :DateOfYear = dayMonth.dateOfYear
	}

}






class MonthOfYear private(private val yearAndMonth :Long) extends AnyVal with Ordered[MonthOfYear] with Serializable {

	@inline def year :Year = new Year((yearAndMonth >> 32).toInt)
	@inline def month :Month = Month(yearAndMonth.toInt)

	@inline def toJava :j.YearMonth = j.YearMonth.of(year.no, month.no)

	@inline def length :Int = yearAndMonth.toInt match {
		case 2 => if (j.Year.isLeap(yearAndMonth >> 32)) 29 else 28
		case n => 30 + (n + n / 8) % 2
	}

	@inline def onFirst :Date = new Date(j.LocalDate.of((yearAndMonth >> 32).toInt, yearAndMonth.toInt, 1))

	@inline def onLast :Date =
		new Date(j.LocalDate.of((yearAndMonth >> 32).toInt, yearAndMonth.toInt, length))

	@inline def on(day :Int) = new Date(j.LocalDate.of((yearAndMonth >> 32).toInt, yearAndMonth.toInt, day))


	@inline def copy(year :Year = this.year, month :Month = this.month) :MonthOfYear =
		new MonthOfYear(year.no.toLong << 32 | month.no)


	@inline def +(period :Period) :MonthOfYear =
		MonthOfYear(yearAndMonth >> 32 + period.toJava.getYears, yearAndMonth & 0xf + period.toJava.getMonths)

	@inline def +(period :FiniteDateSpan) :MonthOfYear =
		MonthOfYear(yearAndMonth >> 32 + period.years, yearAndMonth & 0xf + period.months)

	@inline def -(period :Period) :MonthOfYear =
		MonthOfYear(yearAndMonth >> 32 - period.toJava.getYears, yearAndMonth & 0xf - period.toJava.getMonths)

	@inline def -(period :FiniteDateSpan) :MonthOfYear =
		MonthOfYear(yearAndMonth >> 32 - period.years, yearAndMonth & 0xf - period.months)


	override def compare(that :MonthOfYear) :Int =
		if (yearAndMonth < that.yearAndMonth) -1
		else if (yearAndMonth > that.yearAndMonth) 1
		else 0

	@inline override def <=(that :MonthOfYear) :Boolean = yearAndMonth <= that.yearAndMonth
	@inline override def < (that :MonthOfYear) :Boolean = yearAndMonth < that.yearAndMonth
	@inline override def >=(that :MonthOfYear) :Boolean = yearAndMonth >= that.yearAndMonth
	@inline override def > (that :MonthOfYear) :Boolean = yearAndMonth > that.yearAndMonth

	@inline def min(that :MonthOfYear) :MonthOfYear = if (yearAndMonth <= that.yearAndMonth) this else that
	@inline def max(that :MonthOfYear) :MonthOfYear = if (yearAndMonth >= that.yearAndMonth) this else that


	override def toString :String = month + " " + year

}



object MonthOfYear {
	@inline def apply(year :Year, month :Month) :MonthOfYear = new MonthOfYear(year.no.toLong << 32 | month.no)

	@inline def apply(date :j.YearMonth) :MonthOfYear = new MonthOfYear(date.getYear.toLong << 32 | date.getMonthValue)

	private[slang] def apply(year :Long, month :Long) :MonthOfYear = { //private[slang] is public in the byte code, can be inlined
		var m = month - 1 //swtich to zero-based months
		var y = year + m / 12
		m %= 12
		if (m < 0) {
			y -= 1; m += 12
		}
		m += 1
		if (y < Int.MinValue | y > Int.MaxValue)
			throw new ArithmeticException(s"Int overflow: MonthOfYear($year, $month)")
		new MonthOfYear(y << 32 | m)
	}

	@inline implicit def fromJavaYearMonth(ym :j.YearMonth) :MonthOfYear =
		new MonthOfYear(ym.getYear.toLong << 32 | ym.getMonth.getValue.toLong)

	@inline implicit def toJavaYearMonth(ym :MonthOfYear) :j.YearMonth =
		j.YearMonth.of((ym.yearAndMonth >> 32).toInt, ym.yearAndMonth.toInt)

	@inline implicit def toMonth(ym :MonthOfYear) :Month = ym.month


	@inline def apply()(implicit time :Time = Time.Local) :MonthOfYear = {
		val date = j.YearMonth.now(time.clock)
		new MonthOfYear(date.getYear.toLong << 32 | date.getMonthValue)
	}

	@inline def now(implicit time :Time = Time.Local) :MonthOfYear = {
		val date = j.YearMonth.now(time.clock)
		new MonthOfYear(date.getYear.toLong << 32 | date.getMonthValue)
	}

	@inline def today(implicit time :Time = Time.Local) :MonthOfYear = now

}





class Year private[time] (val no :Int) extends AnyVal with Ordered[Year] with Serializable {
	@inline def toInt :Int = no

	@inline def era :Era = if (no > 0) Era.CE else Era.BCE

	@inline def inEra :Int = if (no > 0) no else 1 - no

	@inline def toJava :j.Year = j.Year.of(no)

	@inline def isLeap :Boolean = j.Year.isLeap(no)

	@inline def length :Int = if (j.Year.isLeap(no)) 366 else 365

	@inline def at(dateOfYear :DateOfYear) :Date = dateOfYear of this


	@inline def +(years :Int) :Year = {
		if (if (no > 0) years > Int.MaxValue - no else years < Int.MinValue - no)
			throw new ArithmeticException("Int overflow: " + this + " + " + years)
		new Year(no + years)
	}

	@inline def -(years :Int) :Year = {
		if (if (no > 0) years < no - Int.MaxValue else years > no - Int.MinValue)
			throw new ArithmeticException("Int overflow: " + this + " - "+ years)
		new Year(no - years)
	}


	@inline def compare(that :Year) :Int = (no.toLong - that.no.toLong).toInt

	override def toString :String = if (no > 0) no.toString else 1 - no + "BCE"
}



object Year {
	final val CE1 = new Year(1)
	final val BCE1 = new Year(0)

	@inline def apply(year :Int) :Year = new Year(year)

	@inline implicit def yearToInt(year :Year) :Int = year.no
	@inline implicit def fromJavaYear(year :j.Year) :Year = new Year(year.getValue)
	@inline implicit def toJavaYear(year :Year) :Year = year.toJava

	object implicits {
		@inline implicit def intToYear(year :Int) :Year = new Year(year)
	}


	@inline def apply()(implicit time :Time = Time.Local) :Year = new Year(j.Year.now(time.clock).getValue)
	@inline def now(implicit time :Time = Time.Local) :Year = new Year(j.Year.now(time.clock).getValue)
	@inline def today(implicit time :Time = Time.Local) :Year = new Year(j.Year.now(time.clock).getValue)

}






class Era private[time] (val toJava :IsoEra) extends AnyVal with Serializable



object Era {
	def apply(era :IsoEra) :Era = new Era(era)

	@inline implicit def fromJavaEra(era :IsoEra) :Era = new Era(era)
	@inline implicit def toJavaEra(era :Era) :IsoEra = era.toJava

	final val CE = new Era(IsoEra.CE)
	final val BCE = new Era(IsoEra.BCE)
}



