package net.noresttherein.slang.time

import java.{time=>j}



/** Contains information about date and time of day abstracting over time zone/offset. Can be combined
  * with a `TimeZone`
  * @author Marcin Mościcki marcin@moscicki.net
  */
@SerialVersionUID(1L)
class DateTime private[time] (val toJava :j.LocalDateTime) extends AnyVal with Ordered[DateTime] with Serializable {
	@inline def date :Date = new Date(toJava.toLocalDate)
	@inline def time :TimeOfDay = new TimeOfDay(toJava.toLocalTime)

	@inline def era :Era = Era(toJava.toLocalDate.getEra)
	@inline def year :Year = Year(toJava.getYear)
	@inline def yearOfEra :Int = year.inEra
	@inline def month :Month = toJava.getMonth
	@inline def monthOfYear :Int = toJava.getMonthValue
	@inline def day :Int = toJava.getDayOfMonth
	@inline def hour :Int = toJava.getHour
	@inline def minute :Int = toJava.getMinute
	@inline def second :Int = toJava.getSecond
	@inline def nano :Int = toJava.getNano


	@inline def copy(year :Year = this.year, month :Month = this.month, day :Int = this.day,
	                 hour :Int = this.hour, minute :Int = this.minute, second :Int = this.second, nano :Int = this.nano)
			:DateTime =
		new DateTime(j.LocalDateTime.of(year.no, month.toJava, day, hour, minute, second, nano))

	@inline def copy(date :Date) :DateTime = new DateTime(j.LocalDateTime.of(date.toJava, toJava.toLocalTime))
	@inline def copy(time :TimeOfDay) :DateTime = new DateTime(j.LocalDateTime.of(toJava.toLocalDate, time.toJava))


	@inline def in(zone :TimeZone) :ZoneDateTime = new ZoneDateTime(toJava.atZone(zone.toJava))
	@inline def at(offset :TimeOffset) :ZoneDateTime = ZoneDateTime(toJava.atOffset(offset.toJava))


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
	override def compare(that :DateTime) :Int =
		if (toJava isBefore that.toJava) -1
		else if (toJava isAfter that.toJava) 1
		else 0

	@inline override def <=(that :DateTime) :Boolean = !(toJava isAfter that.toJava)
	@inline override def < (that :DateTime) :Boolean = toJava isBefore that.toJava
	@inline override def >=(that :DateTime) :Boolean = !(toJava isBefore that.toJava)
	@inline override def > (that :DateTime) :Boolean = toJava isAfter that.toJava
	@inline def min(that :DateTime) :DateTime = if (that.toJava isBefore toJava) that else this
	@inline def max(that :DateTime) :DateTime = if (that.toJava isAfter toJava) that else this


	/** Tests if `this` and `that` describe the same instant, abstracting over the time zone.
	  * Consistent with the ordering on `DateTime`.
	  */
	@inline def ==(that :DateTime) :Boolean = compareTo(that) == 0

}






object DateTime {
	@inline def apply(time :j.LocalDateTime) :DateTime = new DateTime(time)

	@inline def apply(date :Date, time :TimeOfDay) :DateTime = new DateTime(j.LocalDateTime.of(date, time))

//	@inline def apply(timestamp :Timestamp)(implicit time :Time = Time.Local) :DateTime =
//		new DateTime(j.LocalDateTime.ofInstant(timestamp, time.zone))
//
	@inline def apply()(implicit time :Time = Time.Local) :DateTime = new DateTime(j.LocalDateTime.now(time.clock))
	@inline def current(implicit time :Time = Time.Local) :DateTime = new DateTime(j.LocalDateTime.now(time.clock))

	@inline def utc :DateTime = current(Time.UTC)

	@inline implicit def toJava(time :DateTime) :j.LocalDateTime = time.toJava
	@inline implicit def fromJava(time :j.LocalDateTime) :DateTime = new DateTime(time)
}

