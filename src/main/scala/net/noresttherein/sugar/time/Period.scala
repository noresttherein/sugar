package net.noresttherein.sugar.time

import java.{time=>j}



/** Elapsed time measured in calendar units of years, months and days. Instances do not form a complete order
  * and have variable length, depending on leap years and daylight saving time. This is a simple value type wrapping
  * a `java.time.Period`.
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
class Period private[time] (val toJava :j.Period) extends AnyVal with DateSpan with Serializable {
	@inline override def days   :Int = toJava.getDays
	@inline override def months :Int = toJava.getMonths
	@inline override def years  :Int = toJava.getYears

	@inline override def inMonths   :Int = years * 12 + months
	@inline override def inYears    :Int = years + months / 12
	@inline override def normalized :Period = new Period(toJava.normalized)

	@inline override def toPeriod :Period = this

	@inline override def variable :Period   = this
	@inline override def fixed    :Duration = Duration.Zero

	@inline override def isZero :Boolean = toJava.isZero

	@inline def apply(unit :DateUnit) :Int = toJava.get(unit.toJava).toInt

	@inline def unary_- :Period = new Period(toJava.negated)


	override def +(other :TimeFrame) :TimeFrame = other match {
		case period :DateSpan =>  this + period
		case _ if other.isZero => this
		case _ if isZero => other
		case time :TimeSpan => new DateTimeSpan(toPeriod, time.toDuration)
	}

	override def -(other :TimeFrame) :TimeFrame = other match {
		case period :DateSpan => this + period
		case _ if other.isZero => this
		case _ if isZero => -other
		case time :TimeSpan => DateTimeSpan.periodMinusTime(this, time)
	}


	override def +(other :DateSpan) :DateSpan = other match {
		case period :Period => new Period(toJava plus period.toJava)
		case _ =>
			val ld = days.toLong + other.days
			val lm = months.toLong + other.months
			val ly = years.toLong + other.years
			if ((ld & 0xffffffffL) != ld || (lm & 0xffffffffL) != lm || (ly & 0xffffffffL) != ly)
				throw new ArithmeticException("Int overflow: " + this + " + " + other)
			new Period(j.Period.of(ly.toInt, lm.toInt, ld.toInt))
	}

	override def -(other :DateSpan) :DateSpan = other match {
		case period :Period => new Period(toJava minus period.toJava)
		case _ =>
			val ld = days.toLong - other.days
			val lm = months.toLong - other.months
			val ly = years.toLong - other.years
			if ((ld & 0xffffffffL) != ld || (lm & 0xffffffffL) != lm || (ly & 0xffffffffL) != ly)
				throw new ArithmeticException("Int overflow: " + this + " - " + other)
			new Period(j.Period.of(ly.toInt, lm.toInt, ld.toInt))

	}

	@inline def +(other :Period) :Period = new Period(toJava plus other.toJava) //Period(years + other.years, months + other.months, days + other.days)
	@inline def -(other :Period) :Period = new Period(toJava minus other.toJava) //Period(years - other.years, months - other.months, days - other.days)
	@inline def *(scalar :Int)   :Period = new Period(toJava multipliedBy scalar)

	@inline def copy(years :Int = this.years, months :Int = this.months, days :Int = this.days) :Period =
		new Period(j.Period.of(years, months, days))


	override def ==(other :TimeExtent) :Boolean = other match {
		case period :Period => toJava == period.toJava
		case period :DateSpan => days == period.days && months == period.months && years == period.years
		case _ => false
	}

	override def toString :String = toJava.toString
}






@SerialVersionUID(Ver)
case object Period {
	final val Zero = new Period(j.Period.ZERO)

	@inline def apply(years :Int = 0, months :Int = 0, days :Int = 0) :Period =
		new Period(j.Period.of(years, months, days))

	@inline def apply(number :Int, unit :DateUnit) :Period = unit * number

	@inline def apply(period :j.Period) :Period = new Period(period)

	@inline def between(start :Date, end :Date) :Period = new Period(j.Period.between(start.toJava, end.toJava))

	@inline def days(number :Int) :Period = new Period(j.Period.ofDays(number))
	@inline def months(number :Int) :Period = new Period(j.Period.ofMonths(number))
	@inline def years(number :Int) :Period = new Period(j.Period.ofYears(number))

	@inline def until(date :Date)(implicit time :Time = Time.Local) :Period = between(time.date, date)
	@inline def since(date :Date)(implicit time :Time = Time.Local) :Period = between(date, time.date)


	@inline def unapply(time :TimeExtent) :Option[(Int, Int, Int)] = time match {
		case period :Period => Some(period.years, period.months, period.days)
		case _ => None
	}


	@inline implicit def PeriodToJavaPeriod(period :Period) :j.Period = period.toJava
	@inline implicit def PeriodFromJavaPeriod(period :j.Period) :Period = new Period(period)
}
