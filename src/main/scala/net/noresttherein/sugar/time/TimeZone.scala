package net.noresttherein.sugar.time

import java.{time => j}

import net.noresttherein.sugar.time.constants.IntSecondsInHalfDay




//consider: we can make TimeZone a supertype of TimeOffset through a type alias/refinement, but we'll lose
// the distinction in a generic context.
/** Representation of a standard time zone, such as 'Paris', which associates time points with corresponding dates
  * at the location. This is a simple value type wrapping a `java.time.ZoneId`.
  * @author Marcin Mościcki
  */
@SerialVersionUID(1L)
class TimeZone private[time] (val toJava :j.ZoneId) extends AnyVal with Serializable {
	@inline def id :String = toJava.getId

	@inline def offset(time :DateTime)  :TimeOffset = new TimeOffset(toJava.getRules.getOffset(time.toJava))
	@inline def offset(time :Timestamp) :TimeOffset = new TimeOffset(toJava.getRules.getOffset(time.toJava))

	@inline def currentOffset(implicit time :Time = Time.Local) :TimeOffset =
		new TimeOffset(toJava.getRules.getOffset(time.clock.instant))

	@inline def standardOffset(implicit time :Time = Time.Local) :TimeOffset =
		new TimeOffset(toJava.getRules.getStandardOffset(time.now.toInstant))

	@inline def standardOffset(time :Timestamp) :TimeOffset =
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






/** Representation of time offset from `UTC`, that is the difference (in seconds) between time 'at this offset'
  * and UTC. Each `TimeOffset` is associated with a synthetic `TimeZone` which applies this offset for all time points.
  * This is a simple value type backed by a `java.time.ZoneOffset`.
  */
@SerialVersionUID(1L)
class TimeOffset private[time] (val toJava :j.ZoneOffset) extends AnyVal with Ordered[TimeOffset] with Serializable {
	@inline def offset :Milliseconds = new Milliseconds(toJava.getTotalSeconds)

	@inline def zone :TimeZone = new TimeZone(toJava)

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

	@inline def apply(offset :TimeInterval) :TimeOffset =
		new TimeOffset(j.ZoneOffset.ofTotalSeconds(offset.toSeconds.toInt)) //todo: overflow validation

	@inline def apply(offset :Milliseconds) :TimeOffset =
		new TimeOffset(j.ZoneOffset.ofTotalSeconds(offset.toSeconds.toInt))

	@inline def apply(hours :Int, minutes :Int = 0, seconds :Int = 0) :TimeOffset =
		new TimeOffset(j.ZoneOffset.ofHoursMinutesSeconds(hours, minutes, seconds))


	@inline implicit def fromJavaZoneOffset(offset :j.ZoneOffset) :TimeOffset = new TimeOffset(offset)
	@inline implicit def toJavaZoneOffset(offset :TimeOffset) :j.ZoneOffset = offset.toJava
}

