package net.noresttherein.slang.time

import java.{time => j}
import java.time.temporal.ChronoField


/** 'Wall clock time' with nanosecond precision. Wraps a `java.time.LocalTime` instance.
  * @author Marcin Mościcki marcin@moscicki.net
  */
@SerialVersionUID(1L)
class TimeOfDay private[time] (val toJava :j.LocalTime) extends AnyVal with Ordered[TimeOfDay] with Serializable {
	@inline def hour :Int = toJava.getHour
	@inline def minute :Int = toJava.getMinute
	@inline def second :Int = toJava.getSecond
	@inline def nano :Int = toJava.getNano

	@inline def on(date :Date) :DateTime = DateTime(date, this)

	@inline def at(offset :TimeOffset) :OffsetTime = new OffsetTime(toJava atOffset offset.toJava)


	@inline def copy(hour :Int = this.hour, minute :Int = this.minute, second :Int = this.second, nano :Int = this.nano) :TimeOfDay =
		new TimeOfDay(j.LocalTime.of(hour, minute, second, nano))


	def +(time :FiniteTimeSpan) :TimeOfDay = time match {
		case millis :Milliseconds => this + millis
		case duration :Duration => this + duration
		case _ => this + time.toDuration
	}

	def -(time :FiniteTimeSpan) :TimeOfDay = time match {
		case millis :Milliseconds => this - millis
		case duration :Duration => this - duration
		case _ => this - time.toDuration
	}

	@inline def +(time :Milliseconds) :TimeOfDay =
		new TimeOfDay(toJava plusSeconds time.inMillis / 1000L plusNanos time.inMillis % 1000L * NanosInMilli)
	
	@inline def -(time :Milliseconds) :TimeOfDay =
		new TimeOfDay(toJava minusSeconds time.inMillis / 1000L minusNanos time.inMillis % 1000L * NanosInMilli)

	@inline def +(time :Duration) :TimeOfDay = new TimeOfDay(toJava plus time.toJava)
	@inline def -(time :Duration) :TimeOfDay = new TimeOfDay(toJava minus time.toJava)


	@inline def -(time :TimeOfDay) :Duration = new Duration(j.Duration.between(time.toJava, toJava))
	@inline def -(time :OffsetTime) :Duration = new Duration(j.Duration.between(time.toJava.toLocalTime, toJava))


	@inline override def compare(that :TimeOfDay) :Int = toJava compareTo that.toJava

	@inline override def <=(that :TimeOfDay) :Boolean = !(toJava isAfter that.toJava)
	@inline override def < (that :TimeOfDay) :Boolean = toJava isBefore that.toJava
	@inline override def >=(that :TimeOfDay) :Boolean = !(toJava isBefore that.toJava)
	@inline override def > (that :TimeOfDay) :Boolean = toJava isAfter that.toJava

	@inline def min(that :TimeOfDay) :TimeOfDay = if (that.toJava isBefore this) that else this
	@inline def max(that :TimeOfDay) :TimeOfDay = if (that.toJava isAfter this) that else this

}



object TimeOfDay {

	@inline def apply(hour :Int) :TimeOfDay =
		new TimeOfDay(j.LocalTime.of(hour, 0))

	@inline def apply(hour :Int, minute :Int) :TimeOfDay =
		new TimeOfDay(j.LocalTime.of(hour, minute))

	@inline def apply(hour :Int, minute :Int, second :Int) :TimeOfDay =
		new TimeOfDay(j.LocalTime.of(hour, minute, second))

	@inline def apply(time :j.LocalTime) :TimeOfDay = new TimeOfDay(time)


	@inline def apply()(implicit time :Time = Time.Local) :TimeOfDay = new TimeOfDay(j.LocalTime.now(time.clock))

	@inline def current(implicit time :Time = Time.Local) :TimeOfDay = new TimeOfDay(j.LocalTime.now(time.clock))

	@inline def utc :TimeOfDay = new TimeOfDay(j.LocalTime.now(Time.UTC))


	@inline def unapply(time :TimeOfDay)  :Some[(Int, Int, Int, Int)] =
		Some((time.hour, time.minute, time.second, time.nano))



	@inline implicit def fromJavaLocalTime(time :j.LocalTime) :TimeOfDay = new TimeOfDay(time)
	@inline implicit def toJavaLocalTime(time :TimeOfDay) :j.LocalTime = time.toJava

}






/** Time of day with an offset from `UTC`, reflecting a unique instant. Wraps a `java.time.OffsetTime`. */
@SerialVersionUID(1L)
class OffsetTime private[time] (val toJava :j.OffsetTime) extends AnyVal with Ordered[OffsetTime] with Serializable {
	@inline def hour :Int = toJava.getHour
	@inline def minute :Int = toJava.getMinute
	@inline def second :Int = toJava.getSecond
	@inline def nano :Int = toJava.getNano
	
	@inline def time :TimeOfDay = new TimeOfDay(toJava.toLocalTime)
	@inline def offset :TimeOffset = new TimeOffset(toJava.getOffset)

	@inline def at(offset :TimeOffset) :OffsetTime = new OffsetTime(toJava withOffsetSameInstant offset.toJava)

	@inline def on(date :Date) :ZoneDateTime = new ZoneDateTime((toJava atDate date.toJava).toZonedDateTime)

	@inline def copy(hour :Int=this.hour, minute :Int=this.minute, second :Int=this.second, nano :Int=this.nano,
	                 offset :TimeOffset=this.offset) :OffsetTime =
		new OffsetTime(j.OffsetTime.of(j.LocalTime.of(hour, minute, second, nano), offset.toJava))

	def +(time :FiniteTimeSpan) :OffsetTime = time match {
		case millis :Milliseconds => this + millis
		case duration :Duration => this + duration
		case _ => this + time.toDuration
	}

	def -(time :FiniteTimeSpan) :OffsetTime = time match {
		case millis :Milliseconds => this - millis
		case duration :Duration => this - duration
		case _ => this - time.toDuration
	}

	@inline def +(time :Milliseconds) :OffsetTime =
		new OffsetTime(toJava plusSeconds time.inMillis / 1000L plusNanos time.inMillis % 1000L * NanosInMilli)

	@inline def -(time :Milliseconds) :OffsetTime =
		new OffsetTime(toJava minusSeconds time.inMillis / 1000L minusNanos time.inMillis % 1000L * NanosInMilli)

	@inline def +(time :Duration) :OffsetTime = new OffsetTime(toJava plus time.toJava)
	@inline def -(time :Duration) :OffsetTime = new OffsetTime(toJava minus time.toJava)
	

	@inline def -(other :TimeOfDay) :Duration = new Duration(j.Duration.between(toJava.toLocalTime, other.toJava))
	@inline def -(other :OffsetTime) :Duration = new Duration(j.Duration.between(other.toJava, toJava))


	@inline override def compare(that :OffsetTime) :Int = //toJava compareTo that.toJava
		if (toJava isBefore that.toJava) -1
		else if (toJava isAfter that.toJava) 1
		else 0

	@inline override def <=(that :OffsetTime) :Boolean = !(toJava isAfter that.toJava)
	@inline override def < (that :OffsetTime) :Boolean = toJava isBefore that.toJava
	@inline override def >=(that :OffsetTime) :Boolean = !(toJava isBefore that.toJava)
	@inline override def > (that :OffsetTime) :Boolean = toJava isAfter that.toJava

	@inline def min(that :OffsetTime) :OffsetTime = if (that.toJava isBefore toJava) that else this
	@inline def max(that :OffsetTime) :OffsetTime = if (that.toJava isAfter toJava) that else this

	@inline def ===(that :OffsetTime) :Boolean = compare(that) == 0

}



object OffsetTime {
	@inline def apply(time :j.OffsetTime) :OffsetTime = new OffsetTime(time)

	@inline def apply(offset :TimeOffset, time :TimeOfDay) :OffsetTime =
		new OffsetTime(j.OffsetTime.of(time.toJava, offset.toJava))

	@inline def apply(offset :TimeOffset, hour :Int = 0, minute :Int = 0, second :Int = 0, nano :Int = 0) :OffsetTime =
		new OffsetTime(j.OffsetTime.of(j.LocalTime.of(hour, minute, second, nano), offset.toJava))


	@inline def current(implicit time :Time = Time.Local) :OffsetTime =
		new OffsetTime(j.OffsetTime.now(time.clock))

	@inline def at(offset :TimeOffset)(implicit time :Time = Time.Local) :OffsetTime =
		new OffsetTime(j.OffsetTime.now(time.clock) withOffsetSameInstant offset.toJava)

	@inline def utc :OffsetTime = new OffsetTime(j.OffsetTime.now(Time.UTC))


	@inline def fromJavaOffsetTime(time :j.OffsetTime) :OffsetTime = new OffsetTime(time)
	@inline def toJavaOffsetTime(time :OffsetTime) :j.OffsetTime = time.toJava

}
