package net.noresttherein.sugar.time

import java.{time => j}
import java.time.chrono.IsoEra




/** An ISO date without a time offset, such as description of a birthday. Serves as a light wrapper over
  * `java.time.LocalDate`.
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
class Date private[time] (val toJava :j.LocalDate) extends AnyVal with TimeProjection with Ordered[Date] {
	@inline def day         :Int   = toJava.getDayOfMonth
	@inline def dayOfWeek   :Day   = toJava.getDayOfWeek
	@inline def dayOfYear   :Int   = toJava.getDayOfYear
	@inline def month       :Month = toJava.getMonth
	@inline def monthFrom0  :Int   = toJava.getMonthValue - 1
	@inline def monthFrom1  :Int   = toJava.getMonthValue
	@inline def year        :Year  = new Year(toJava.getYear)
	@inline def yearOfEra   :Int   = year.inEra
	@inline def era         :Era   = new Era(toJava.getEra)
	@inline def anniversary :Anniversary = Anniversary(Month(toJava.getMonthValue), toJava.getDayOfMonth)
	@inline def monthOfYear :MonthOfYear = MonthOfYear(year, month)


	@inline def daysInMonth :Int = toJava.lengthOfMonth
	@inline def daysInYear  :Int = toJava.lengthOfYear

	@inline def midnight :DateTime = new DateTime(toJava.atStartOfDay)
	@inline def midnight(zone :TimeZone) :ZoneDateTime = new ZoneDateTime(toJava.atStartOfDay(zone.toJava))
	@inline def at(time :TimeOfDay) :DateTime = new DateTime(j.LocalDateTime.of(toJava, time.toJava))

	@inline def is(date :Anniversary) :Boolean = anniversary == date

	@inline def copy(year :Year = this.year, month :Month = this.month, day :Int = this.day) :Date =
		new Date(j.LocalDate.of(year.no, month.no, day))

	@inline def +(period :DateSpan) :Date = new Date(toJava plus period.toPeriod.toJava)
	@inline def -(period :DateSpan) :Date = new Date(toJava minus period.toPeriod.toJava)

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



@SerialVersionUID(Ver)
case object Date extends TimeProjector {
	override type Projection = Date

	@inline def apply(year :Int, month :Int, day :Int) :Date = new Date(j.LocalDate.of(year, month, day))

	@inline def apply(date :j.LocalDate) :Date = new Date(date)

//	@inline def apply(timestamp :Timestamp)(implicit time :Time = Time.Local) :Date =
//		new Date(j.LocalDate.ofInstant(timestamp, time.zone))
//

	@inline def apply(time :Time = Time.Local)            :Date = new Date(j.LocalDate.now(time.clock))
	@inline def current(implicit time :Time = Time.Local) :Date = new Date(j.LocalDate.now(time.clock))
	@inline def today(implicit time :Time = Time.Local)   :Date = new Date(j.LocalDate.now(time.clock))


	@inline def unapply(date :Date) :Some[(Year, Month, Int)] = Some((date.year, date.month, date.day))

	@inline implicit def DateFromJavaLocalDate(date :j.LocalDate) :Date = new Date(date)
	@inline implicit def DateToJavaLocalDate(date :Date)      :j.LocalDate = date.toJava
}






/** A yearly reoccurring date such as an anniversary described by a month and day of month. */
@SerialVersionUID(Ver)
class Anniversary private(private val dayAndMonth :Int) extends TimeProjection with Ordered[Anniversary] {

	@inline def day   :Int = dayAndMonth & 0x1f
	@inline def month :Month = Month(dayAndMonth >> 5)

	@inline def toJava :j.MonthDay = j.MonthDay.of(month, day)

	@inline def of(year :Year)    :Date = new Date(j.LocalDate.of(year, month, day))
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

	def copy(month :Month = this.month, day :Int = this.day) :Anniversary =
		if (day < 1 || day > month.maxLength)
			throw new IllegalArgumentException(s"month $month does not have $day days")
		else new Anniversary(month.no << 5 | day)


	@inline override def compare(that :Anniversary) :Int = dayAndMonth - that.dayAndMonth

	@inline override def <=(that :Anniversary) :Boolean = dayAndMonth <= that.dayAndMonth
	@inline override def < (that :Anniversary) :Boolean = dayAndMonth < that.dayAndMonth
	@inline override def >=(that :Anniversary) :Boolean = dayAndMonth >= that.dayAndMonth
	@inline override def > (that :Anniversary) :Boolean = dayAndMonth > that.dayAndMonth

	@inline def min(that :Anniversary) :Anniversary = if (dayAndMonth <= that.dayAndMonth) this else that
	@inline def max(that :Anniversary) :Anniversary = if (dayAndMonth >= that.dayAndMonth) this else that


	override def toString :String = twoDigit(day) + "." + month
}



@SerialVersionUID(Ver)
case object Anniversary extends TimeProjector {
	override type Projection = Anniversary

	def apply(month :Month, day :Int) :Anniversary =
		if (day < 1 || day > month.maxLength)
			throw new IllegalArgumentException("Month " + month.name + " does not have " + day + " days")
		else
	        new Anniversary(month.no << 5 | day)

	@inline def apply(monthDay :j.MonthDay) :Anniversary =
		new Anniversary(monthDay.getMonthValue << 5 | monthDay.getDayOfMonth)


	@inline def apply(time :Time = Time.Local) :Anniversary = current
	@inline def today(implicit time :Time = Time.Local) :Anniversary = current

	def current(implicit time :Time = Time.Local) :Anniversary = {
		val date = j.MonthDay.now(time.clock)
		new Anniversary(date.getMonthValue << 5 | date.getDayOfMonth)
	}


	@inline implicit def AnniversaryFromJavaMonthDay(monthDay :j.MonthDay) :Anniversary = apply(monthDay)
	@inline implicit def AnniversaryToJavaMonthDay(date :Anniversary)       :j.MonthDay =
		j.MonthDay.of(date.month.no, date.day)
}






/** A combination of a year and a month, such as ''1981 Jan''. */
@SerialVersionUID(Ver) //Java uses the name 'month of year' for our Month
class MonthOfYear private (private val yearAndMonth :Long)
	extends AnyVal with TimeProjection with Ordered[MonthOfYear]
{
	@inline def year  :Year  = new Year((yearAndMonth >> 32).toInt)
	@inline def month :Month = Month(yearAndMonth.toInt)

	@inline def toJava :j.YearMonth = j.YearMonth.of(year.no, month.no)

	def length :Int = yearAndMonth.toInt match {
		case 2 => if (j.Year.isLeap(yearAndMonth >> 32)) 29 else 28
		case n => 30 + (n + n / 8) % 2
	}

	@inline def onFirst :Date =
		new Date(j.LocalDate.of((yearAndMonth >> 32).toInt, yearAndMonth.toInt, 1))

	@inline def onLast  :Date =
		new Date(j.LocalDate.of((yearAndMonth >> 32).toInt, yearAndMonth.toInt, length))

	@inline def on(day :Int) = new Date(j.LocalDate.of((yearAndMonth >> 32).toInt, yearAndMonth.toInt, day))


	@inline def copy(year :Year = this.year, month :Month = this.month) :MonthOfYear =
		new MonthOfYear(year.no.toLong << 32 | month.no)


	@inline def +(period :Period) :MonthOfYear =
		MonthOfYear(yearAndMonth >> 32 + period.toJava.getYears, yearAndMonth & 0xf + period.toJava.getMonths)

	@inline def +(period :DateSpan) :MonthOfYear =
		MonthOfYear(yearAndMonth >> 32 + period.years, yearAndMonth & 0xf + period.months)

	@inline def -(period :Period) :MonthOfYear =
		MonthOfYear(yearAndMonth >> 32 - period.toJava.getYears, yearAndMonth & 0xf - period.toJava.getMonths)

	@inline def -(period :DateSpan) :MonthOfYear =
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


	override def toString :String = month.toString + " " + year
}



@SerialVersionUID(Ver)
case object MonthOfYear extends TimeProjector {
	override type Projection = MonthOfYear

	def apply(year :Year, month :Month) :MonthOfYear = new MonthOfYear(year.no.toLong << 32 | month.no)

	def apply(date :j.YearMonth) :MonthOfYear = new MonthOfYear(date.getYear.toLong << 32 | date.getMonthValue)

	private[sugar] def apply(year :Long, month :Long) :MonthOfYear = {
		var m = month - 1 //switch to zero-based months
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


	@inline def apply(time :Time = Time.Local) :MonthOfYear = current
	@inline def today(implicit time :Time = Time.Local) :MonthOfYear = current

	override def current(implicit time :Time = Time.Local) :MonthOfYear = {
		val date = j.YearMonth.now(time.clock)
		new MonthOfYear(date.getYear.toLong << 32 | date.getMonthValue)
	}



	@inline implicit def MonthOfYearFromJavaYearMonth(ym :j.YearMonth) :MonthOfYear = apply(ym)

	@inline implicit def MonthOfYearToJavaYearMonth(ym :MonthOfYear)   :j.YearMonth =
		j.YearMonth.of((ym.yearAndMonth >> 32).toInt, ym.yearAndMonth.toInt)

	@inline implicit def MonthOfYearToMonth(ym :MonthOfYear) :Month = ym.month

}





/** Proleptic year wrapping an `Int`. Proleptic year values are equal to standard date years for all
  * `CE` years, with zero denoting the first year `BCE` and negative numbers consecutive preceding years.
  */
@SerialVersionUID(Ver)
class Year private[time] (val no :Int) extends AnyVal with TimeProjection with Ordered[Year] {
	@inline def toInt  :Int     = no //consider: renaming no to int and getting rid of this method
	@inline def era    :Era     = if (no > 0) Era.CE else Era.BCE
	@inline def inEra  :Int     = if (no > 0) no else 1 - no
	@inline def toJava :j.Year  = j.Year.of(no)
	@inline def isLeap :Boolean = j.Year.isLeap(no)
	@inline def length :Int     = if (j.Year.isLeap(no)) 366 else 365

	@inline def at(dateOfYear :Anniversary) :Date = dateOfYear of this


	def +(years :Int) :Year = {
		if (if (no > 0) years > Int.MaxValue - no else years < Int.MinValue - no)
			throw new ArithmeticException("Int overflow: " + this + " + " + years)
		new Year(no + years)
	}

	def -(years :Int) :Year = {
		if (if (no > 0) years < no - Int.MaxValue else years > no - Int.MinValue)
			throw new ArithmeticException("Int overflow: " + this + " - "+ years)
		new Year(no - years)
	}


	@inline def compare(that :Year) :Int = (no.toLong - that.no.toLong).toInt

	override def toString :String = if (no > 0) no.toString else (1 - no).toString + "BCE"
}



@SerialVersionUID(Ver)
case object Year extends TimeProjector {
	override type Projection = Year

	final val CE1  = new Year(1)
	final val BCE1 = new Year(0)

	@inline def apply(year :Int)    :Year = new Year(year)
	@inline def apply(year :j.Year) :Year = new Year(year.getValue)

	@inline def apply(time :Time = Time.Local) :Year = new Year(j.Year.now(time.clock).getValue)
	@inline def today(implicit time :Time = Time.Local) :Year = new Year(j.Year.now(time.clock).getValue)
	@inline override def current(implicit time :Time = Time.Local) :Year = new Year(j.Year.now(time.clock).getValue)



	@inline implicit def YearToInt(year :Year)          :Int  = year.no
	@inline implicit def YearFromJavaYear(year :j.Year) :Year = new Year(year.getValue)
	@inline implicit def YearToJavaYear(year :Year)     :Year = year.toJava

	object implicits {
		@inline implicit def IntToYear(year :Int) :Year = new Year(year)
	}
}






/** An ISO-8601 era, being one of `Era.CE` for the current era and `Era.BCE` for the preceding years.
  * Wraps a `java.time.IsoEra` instance.
  */
@SerialVersionUID(Ver)
class Era private[time] (val toJava :IsoEra) extends AnyVal with TimeProjection



@SerialVersionUID(Ver)
case object Era extends TimeProjector {
	override type Projection = Era

	@inline def apply(era :IsoEra) :Era = new Era(era)

	override def current(implicit time :Time) :Era = if (time.utc.year.no >= 0) CE else BCE

	@inline implicit def EraFromJavaEra(era :IsoEra) :Era = new Era(era)
	@inline implicit def EraToJavaEra(era :Era) :IsoEra = era.toJava

	//consider: moving these constants (and other names) to package time
	final val CE  = new Era(IsoEra.CE)
	final val BCE = new Era(IsoEra.BCE)
}


