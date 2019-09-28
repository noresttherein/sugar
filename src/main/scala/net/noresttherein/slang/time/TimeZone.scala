package net.noresttherein.slang.time

import java.{time=>j}

/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
class TimeZone private[time] (val toJava :j.ZoneId) extends AnyVal with Serializable {
	@inline def id :String = toJava.getId

	def offset(time :DateTime) :TimeOffset = new TimeOffset(toJava.getRules.getOffset(time.toJava))

	def offset(time :Timestamp) :TimeOffset = new TimeOffset(toJava.getRules.getOffset(time.toJava))

	def currentOffset(implicit time :Time = Time.Local) :TimeOffset =
		new TimeOffset(toJava.getRules.getOffset(time.clock.instant))

	def standardOffset(implicit time :Time = Time.Local) :TimeOffset =
		new TimeOffset(toJava.getRules.getStandardOffset(time.now.toInstant))

	def standardOffset(time :Timestamp) :TimeOffset =
		new TimeOffset(toJava.getRules.getStandardOffset(time.toInstant))

	//todo: DST and transitions

	override def toString :String = toJava.toString
}

object TimeZone {
	final val UTC :TimeZone = j.ZoneOffset.UTC

	@inline def apply(zone :j.ZoneId) :TimeZone = new TimeZone(zone)
	@inline def apply(zoneId :String) :TimeZone = new TimeZone(j.ZoneId.of(zoneId))
//	@inline def apply(offset :TimeOffset) :TimeZone = new TimeZone(j.ZoneId.ofOffset("UTC", offset.toJava))

	@inline implicit def fromJavaZoneId(zone :j.ZoneId) :TimeZone = new TimeZone(zone)
	@inline implicit def toJavaZoneId(zone :TimeZone) :j.ZoneId = zone.toJava
	@inline implicit def fromTimeOffset(offset :TimeOffset) :TimeZone = new TimeZone(offset.toJava)
}





class TimeOffset private[time] (val toJava :j.ZoneOffset) extends AnyVal with Ordered[TimeOffset] with Serializable {
	def offset :Milliseconds = new Milliseconds(toJava.getTotalSeconds)

	@inline def normalized :TimeOffset =
		new TimeOffset(j.ZoneOffset.ofTotalSeconds(toJava.getTotalSeconds % IntSecondsInHalfDay))

	//todo: think about +/-

	@inline override def compare(that :TimeOffset) :Int = toJava compareTo that.toJava
	@inline override def <=(that :TimeOffset) :Boolean = toJava.getTotalSeconds <= that.toJava.getTotalSeconds
	@inline override def < (that :TimeOffset) :Boolean = toJava.getTotalSeconds < that.toJava.getTotalSeconds
	@inline override def >=(that :TimeOffset) :Boolean = toJava.getTotalSeconds >= that.toJava.getTotalSeconds
	@inline override def > (that :TimeOffset) :Boolean = toJava.getTotalSeconds > that.toJava.getTotalSeconds

	@inline def min(that :TimeOffset) :TimeOffset =
		if (toJava.getTotalSeconds <= that.toJava.getTotalSeconds) this else that

	@inline def max(that :TimeOffset) :TimeOffset =
		if (toJava.getTotalSeconds >= that.toJava.getTotalSeconds) this else that

}



object TimeOffset {
	final val UTC = new TimeOffset(j.ZoneOffset.UTC)
	final val Zero = UTC

	@inline def apply(offset :j.ZoneOffset) :TimeOffset = new TimeOffset(offset)

	@inline def apply(offset :TimeSpan) :TimeOffset =
		new TimeOffset(j.ZoneOffset.ofTotalSeconds(offset.inSeconds.toInt)) //todo: overflow validation

	@inline def apply(offset :Milliseconds) :TimeOffset =
		new TimeOffset(j.ZoneOffset.ofTotalSeconds(offset.inSeconds.toInt))

	@inline def apply(hours :Int, minutes :Int = 0, seconds :Int = 0) :TimeOffset =
		new TimeOffset(j.ZoneOffset.ofHoursMinutesSeconds(hours, minutes, seconds))


	@inline implicit def fromJavaZoneOffset(offset :j.ZoneOffset) :TimeOffset = new TimeOffset(offset)
	@inline implicit def toJavaZoneOffset(offset :TimeOffset) :j.ZoneOffset = offset.toJava
}