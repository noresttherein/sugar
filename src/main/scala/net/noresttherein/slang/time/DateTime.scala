package net.noresttherein.slang.time

import java.{time=>j}

/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
class DateTime private[time] (val toJava :j.LocalDateTime) extends AnyVal with Ordered[DateTime] with Serializable {
	@inline def date :Date = new Date(toJava.toLocalDate)
	@inline def time :TimeOfDay = new TimeOfDay(toJava.toLocalTime)

	@inline def year :Int = toJava.getYear
	@inline def month :Month = toJava.getMonth
	@inline def day :Int = toJava.getDayOfMonth
	@inline def hour :Int = toJava.getHour
	@inline def minute :Int = toJava.getMinute
	@inline def second :Int = toJava.getSecond
	@inline def nano :Int = toJava.getNano


	@inline def in(zone :TimeZone) :ZoneDateTime = new ZoneDateTime(toJava.atZone(zone.toJava))


	def +(time :FiniteTimeLapse) :DateTime = time match {
		case t :FiniteTimeSpan => this + t
		case p :FiniteDateSpan => this + p
		case _ => this + time.period + time.duration
	}

	def -(time :FiniteTimeLapse) :DateTime = time match {
		case t :FiniteTimeSpan => this - t
		case p :FiniteDateSpan => this - p
		case _ => this - time.period - time.duration
	}

	@inline def +(period :FiniteDateSpan) :DateTime = new DateTime(toJava plus period.period.toJava)
	@inline def -(period :FiniteDateSpan) :DateTime = new DateTime(toJava minus period.period.toJava)

	@inline def +(period :Period) :DateTime = new DateTime(toJava plus period.toJava)
	@inline def -(period :Period) :DateTime = new DateTime(toJava minus period.toJava)

	@inline def +(time :FiniteTimeSpan) :DateTime = new DateTime(toJava plus time.toJava)
	@inline def -(time :FiniteTimeSpan) :DateTime = new DateTime(toJava minus time.toJava)

	@inline def +(time :Duration) :DateTime = new DateTime(toJava plus time.toJava)
	@inline def -(time :Duration) :DateTime = new DateTime(toJava minus time.toJava)


	@inline def -(other :DateTime) :TimeSpan = new Duration(j.Duration.between(other.toJava, toJava))



//	override def toString :String = date + " " + time
	@inline override def compare(that :DateTime) :Int =
		if (toJava isBefore that.toJava) -1
		else if (toJava isAfter that.toJava) 1
		else 0

	@inline override def <=(that :DateTime) :Boolean = !(toJava isAfter that.toJava)
	@inline override def < (that :DateTime) :Boolean = toJava isBefore that.toJava
	@inline override def >=(that :DateTime) :Boolean = !(toJava isBefore that.toJava)
	@inline override def > (that :DateTime) :Boolean = toJava isAfter that.toJava
	@inline def min(that :DateTime) :DateTime = if (that.toJava isBefore toJava) that else this
	@inline def max(that :DateTime) :DateTime = if (that.toJava isAfter toJava) that else this

	@inline def ===(that :DateTime) :Boolean = compareTo(that) == 0

}



object DateTime {

	def apply(time :j.LocalDateTime) :DateTime = new DateTime(time)

	def apply(date :Date, time :TimeOfDay) :DateTime = new DateTime(j.LocalDateTime.of(date, time))

	def local(implicit time :Time = Time.Local) :DateTime = new DateTime(j.LocalDateTime.now(time.clock))

	@inline implicit def toJava(time :DateTime) :j.LocalDateTime = time.toJava
	@inline implicit def fromJava(time :j.LocalDateTime) :DateTime = new DateTime(time)
}
