package net.noresttherein.sugar.time

import java.{time => j}
import java.time.temporal.Temporal

import net.noresttherein.sugar.time.TimePoint.TimeLimes
import scala.concurrent.duration.{Deadline, FiniteDuration}

import net.noresttherein.sugar.unsupported_!



/** A precise moment in time. This class encompasses both finite time points
  * ([[net.noresttherein.sugar.time.DefiniteTime DefiniteTime]] and the two infinite limits:
  * [[net.noresttherein.sugar.time.DawnOfTime DawnOfTime]] and [[net.noresttherein.sugar.time.EndOfTime EndOfTime]].
  * All time points, regardless of implementation, form a complete order with the two infinities as the bounds.
  * Equals however is class specific, meaning a `Timestamp()` and `ZoneDateTime()` will always be unequal, as expected.
  * In order to compare for equality in the ordering sense, use `===` instead.
  * @author Marcin Mościcki
  */
sealed trait TimePoint extends Any with Ordered[TimePoint] with Serializable {

	def apply(cycle :Cycle) :cycle.Phase

	def isDefinite     :Boolean
	def forcedDefinite :DefiniteTime

	def nano        :Int
	def epochSecond :Long
	def epochMilli  :Long

	def toPosix     :PosixTime = new PosixTime(epochMilli)
	def toTimestamp :Timestamp
	def toUTC       :UTCDateTime

	def in(zone :TimeZone)     :ZoneDateTime
	def at(offset :TimeOffset) :ZoneDateTime
	def local(implicit time :Time = Time.Local) :ZoneDateTime = in(time.zone)

	def toJava     :Temporal
	def toInstant  :j.Instant
	def toDeadline :Deadline = toTimestamp.toDeadline

	def +(time :TimeInterval) :TimePoint
	def -(time :TimeInterval) :TimePoint
	def -(time :TimePoint)    :TimeInterval


	def ==(other :TimePoint) :Boolean = compare(other) == 0

	def min(that :TimePoint) :TimePoint = if (compare(that) <= 0) this else that
	def max(that :TimePoint) :TimePoint = if (compare(that) >= 0) this else that

}



@SerialVersionUID(Ver)
case object TimePoint {

	@inline def now(implicit time :Time = Time.Local) :TimePoint = Timestamp.now

	def after(extent :TimeExtent)(implicit time :Time = Time.Local) :TimePoint = extent match {
		case duration :Duration     => Timestamp after duration
		case millis   :Milliseconds => PosixTime after millis
		case span     :TimeSpan     => Timestamp.now + span
		case finite   :TimeFrame    => ZoneDateTime.now + finite
		case Eternity               => EndOfTime
		case MinusEternity          => DawnOfTime
	}

	def before(extent :TimeExtent)(implicit time :Time = Time.Local) :TimePoint = extent match {
		case duration :Duration     => Timestamp before duration
		case millis   :Milliseconds => PosixTime before millis
		case span     :TimeSpan     => Timestamp.now - span
		case finite   :TimeFrame    => ZoneDateTime.now - finite
		case Eternity               => DawnOfTime
		case MinusEternity          => EndOfTime
	}


	@inline implicit def TimePointFromJavaInstant(instant :j.Instant) :Timestamp = new Timestamp(instant)


	/** Base class for the two infinite limits of the time line: 'before time' (minus inf) and 'after time' (plus inf).
	  * @see [[net.noresttherein.sugar.time.DawnOfTime DawnOfTime]]
	  * @see [[net.noresttherein.sugar.time.EndOfTime EndOfTime]]
	  */
	sealed abstract class TimeLimes extends TimePoint with Serializable {

		final override def isDefinite = false

		final override def apply(cycle :Cycle) :cycle.Phase = cycle.on(this)

		final override def nano        :Int         = unsupported_!(s"($this).nano")
		final override def epochSecond :Long        = unsupported_!(s"($this).epochSecond")
		final override def epochMilli  :Long        = unsupported_!(s"($this).epochMilli")
		final override def toPosix     :PosixTime   = unsupported_!(s"($this).toUnix")
		final override def toUTC       :UTCDateTime = unsupported_!(s"($this).toUTC")
		final override def toTimestamp :Timestamp   = unsupported_!(s"($this).toTimestamp")
		final override def toInstant   :j.Instant   = unsupported_!(s"($this).toInstant")
		final override def toJava      :Temporal    = unsupported_!(s"($this).toJava")

		final override def in(zone :TimeZone)     :ZoneDateTime =
			unsupported_!(s"$this in $zone")

		final override def at(offset :TimeOffset) :ZoneDateTime =
			unsupported_!(s"$this at $offset")

	}

}






/** Base trait for time points representing valid, finite values on the time line.
  * All [[net.noresttherein.sugar.time.TimePoint TimePoint]] implementations other than provided
  * [[net.noresttherein.sugar.time.DawnOfTime DawnOfTime]] and [[net.noresttherein.sugar.time.EndOfTime EndOfTime]]
  * must extend this trait rather than `TimePoint` directly.
  */
trait DefiniteTime extends Any with TimePoint {

	final override def isDefinite = true
	override def forcedDefinite :DefiniteTime = this

	override def +(time :TimeInterval) :TimePoint = time match {
		case finite :TimeSpan => this + finite
		case Eternity => EndOfTime
		case MinusEternity => DawnOfTime
	}
	override def -(time :TimeInterval) :TimePoint = time match {
		case finite :TimeSpan => this + finite
		case Eternity => DawnOfTime
		case MinusEternity => EndOfTime
	}
	override def -(other :TimePoint) :TimeInterval = other match {
		case finite :DefiniteTime => this - finite
		case DawnOfTime => Eternity
		case EndOfTime => MinusEternity
	}

	def +(time :TimeSpan) :DefiniteTime
	def -(time :TimeSpan) :DefiniteTime

	def +(time :Duration) :DefiniteTime
	def -(time :Duration) :DefiniteTime

	def -(other :DefiniteTime) :TimeSpan =
		new Duration(j.Duration.between(other.toInstant, toInstant))

	def -(other :Timestamp) :Duration =
		new Duration(j.Duration.between(other.toJava, toInstant))


	override def compare(that :TimePoint) :Int = that match {
		case finite :DefiniteTime =>
			val s1 = epochSecond; val s2 = finite.epochSecond
			if (s1 < s2) -1
			else if (s1 > s2) 1
			else java.lang.Long.signum(nano.toLong - finite.nano)
		case DawnOfTime => 1
		case EndOfTime => -1
	}

	@inline final def min(that :DefiniteTime) :DefiniteTime = if (compare(that) <= 0) this else that
	@inline final def max(that :DefiniteTime) :DefiniteTime = if (compare(that) >= 0) this else that

	override def ==(that :TimePoint) :Boolean = that match {
		case finite :DefiniteTime => epochSecond == finite.epochSecond && nano == finite.nano
		case _ => false
	}

}




@SerialVersionUID(Ver)
case object DefiniteTime {

	@inline def apply(time :Time = Time.Local) :DefiniteTime = Timestamp.now

	@inline def now(implicit time :Time = Time.Local) :DefiniteTime = Timestamp.now


	def after(extent :TimeFrame)(implicit time :Time = Time.Local) :DefiniteTime = extent match {
		case duration :Duration => Timestamp after duration
		case millis :Milliseconds => PosixTime after millis
		case span :TimeSpan => Timestamp.now + span
		case _ => ZoneDateTime.now + extent
	}

	def before(extent :TimeFrame)(implicit time :Time = Time.Local) :DefiniteTime = extent match {
		case duration :Duration => Timestamp before duration
		case millis :Milliseconds => PosixTime before millis
		case span :TimeSpan => Timestamp.now - span
		case _ => ZoneDateTime.now - extent
	}


	@inline implicit def DefiniteTimeToJavaInstant(time :DefiniteTime) :j.Instant = time.toInstant
}







/** Base trait for finite time points carrying information about the zone/offset and thus also full date and clock time.
  */
trait DateTimePoint extends Any with DefiniteTime {
	def zone   :TimeZone
	def offset :TimeOffset
	def date   :Date
	def local  :DateTime
	def time   :TimeOfDay

	def day    :Int   = date.day
	def month  :Month = date.month
	def year   :Year  = date.year
	def hour   :Int   = time.hour
	def minute :Int   = time.minute
	def second :Int   = time.second
	override def nano :Int = time.nano

	def dayOfWeek :Day

	def toZoneDateTime :ZoneDateTime

	def +(time :TimeExtent) :TimePoint
	def -(time :TimeExtent) :TimePoint
	def +(time :TimeFrame) :DateTimePoint
	def -(time :TimeFrame) :DateTimePoint

	def +(period :Period) :DateTimePoint
	def -(period :Period) :DateTimePoint

	override def +(time :TimeSpan) :DateTimePoint
	override def -(time :TimeSpan) :DateTimePoint

	override def +(time :Duration) :DateTimePoint
	override def -(time :Duration) :DateTimePoint


	@inline final def min(that :DateTimePoint) :DateTimePoint = if (compare(that) <= 0) this else that
	@inline final def max(that :DateTimePoint) :DateTimePoint = if (compare(that) >= 0) this else that

}



@SerialVersionUID(Ver)
case object DateTimePoint {
	@inline implicit def DateTimePointToJavaOffsetDateTime(time :DateTimePoint) :j.OffsetDateTime =
		time.toZoneDateTime.toJava.toOffsetDateTime

	@inline implicit def DateTimePointToJavaOffsetDateTime(time :ZoneDateTime) :j.OffsetDateTime =
		time.toJava.toOffsetDateTime

	@inline implicit def DateTimePointFromJavaOffsetDateTime(time :j.OffsetDateTime) :ZoneDateTime =
		new ZoneDateTime(time.toZonedDateTime)

}






/** A singularity of the time line, a point which follows in ordering all other `TimePoint` instances.
  * It may be returned as a result of an arithmetic operation involving infinite time extents `Eternity`
  * or `MinusEternity`.
  */
@SerialVersionUID(Ver)
case object EndOfTime extends TimeLimes {

	override def forcedDefinite :Timestamp = Timestamp.Max

	override def +(time :TimeInterval) :TimePoint = time match {
		case MinusEternity => DawnOfTime
		case _ => this
	}
	override def -(time :TimeInterval) :TimePoint = time match {
		case Eternity => DawnOfTime
		case _ => this
	}
	override def -(time :TimePoint) :TimeInterval =
		if (time == this) Immediate
		else Eternity

	override def compare(that :TimePoint) :Int =
		if (that == this) 0
		else 1

	override def toString = "End of time"
}



/** A singularity of the time line, a point which precedes in ordering
  * all other [[net.noresttherein.sugar.time.TimePoint TimePoint]] instances. It may be returned
  * as a result of an arithmetic operation involving infinite time extents `Eternity` or `MinusEternity`.
  */
@SerialVersionUID(Ver)
case object DawnOfTime extends TimeLimes {

	override def forcedDefinite :Timestamp = Timestamp.Min

	override def +(time :TimeInterval) :TimePoint = time match {
		case Eternity => EndOfTime
		case _ => this
	}
	override def -(time :TimeInterval) :TimePoint = time match {
		case MinusEternity => EndOfTime
		case _ => this
	}

	override def -(time :TimePoint) :TimeInterval =
		if (time == this) Immediate
		else Eternity


	override def compare(that :TimePoint) :Int =
		if (that == this) 0
		else -1

	override def toString = "Dawn of time"
}

