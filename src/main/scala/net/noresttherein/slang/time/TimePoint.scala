package net.noresttherein.slang.time

import java.{time => j}
import java.time.temporal.Temporal

import net.noresttherein.slang.time.TimePoint.TimeLimes

import scala.concurrent.duration.{Deadline, FiniteDuration}



/** A precise moment in time. This class encompasses both finite time points
  * ([[net.noresttherein.slang.time.DefiniteTime DefiniteTime]] and the two infinite limits:
  * [[net.noresttherein.slang.time.DawnOfTime DawnOfTime]] and [[net.noresttherein.slang.time.EndOfTime EndOfTime]].
  * All time points, regardless of implementation, form a complete order with the two infinities as the bounds.
  * Equals however is class specific, meaning a `Timestamp()` and `ZoneDateTime()` will always be unequal, as expected.
  * In order to compare for equality in the ordering sense, use `===` instead.
  * @author Marcin Mościcki marcin@moscicki.net
  */
sealed trait TimePoint extends Any with Ordered[TimePoint] with Serializable {

	def apply(cycle :Cycle) :cycle.Phase

	def isDefinite :Boolean
	def forcedDefinite :DefiniteTime

	def nano :Int
	def epochSecond :Long
	def epochMilli :Long

	def toUnix :UnixTime = new UnixTime(epochMilli)
	def toTimestamp :Timestamp
	def toUTC :UTCDateTime

	def in(zone :TimeZone): ZoneDateTime
	def at(offset :TimeOffset) :ZoneDateTime

	def toJava :Temporal
	def toInstant :j.Instant
	def toDeadline :Deadline = toTimestamp.toDeadline

	def +(time :TimeSpan) :TimePoint
	def -(time :TimeSpan) :TimePoint

	def -(time :TimePoint) :TimeSpan


	def ==(other :TimePoint) :Boolean = compare(other) == 0

	def min(that :TimePoint) :TimePoint = if (compare(that) <= 0) this else that
	def max(that :TimePoint) :TimePoint = if (compare(that) >= 0) this else that

}



object TimePoint {

	@inline def now(implicit time :Time = Time.Local) :TimePoint = Timestamp.now

	def after(lapse :TimeLapse)(implicit time :Time = Time.Local) :TimePoint = lapse match {
		case duration :Duration => Timestamp after duration
		case millis :Milliseconds => UnixTime after millis
		case span :FiniteTimeSpan => Timestamp.now + span
		case finite :FiniteTimeLapse => ZoneDateTime.now + finite
		case Eternity => EndOfTime
		case MinusEternity => DawnOfTime
	}

	def before(lapse :TimeLapse)(implicit time :Time = Time.Local) :TimePoint = lapse match {
		case duration :Duration => Timestamp before duration
		case millis :Milliseconds => UnixTime before millis
		case span :FiniteTimeSpan => Timestamp.now - span
		case finite :FiniteTimeLapse => ZoneDateTime.now - finite
		case Eternity => DawnOfTime
		case MinusEternity => EndOfTime
	}


	@inline implicit def fromJavaInstant(instant :j.Instant) :Timestamp = new Timestamp(instant)


	/** Base class for the two infinite limits of the time line: 'before time' (minus inf) and 'after time' (plus inf).
	  * @see [[net.noresttherein.slang.time.DawnOfTime DawnOfTime]]
	  * @see [[net.noresttherein.slang.time.EndOfTime EndOfTime]]
	  */
	sealed abstract class TimeLimes extends TimePoint with Serializable {

		final override def isDefinite = false

		final override def apply(cycle :Cycle) :cycle.Phase = cycle.of(this)

		final override def nano :Int = throw new UnsupportedOperationException(s"($this).nano")
		final override def epochSecond :Long = throw new UnsupportedOperationException(s"($this).epochSecond")
		final override def epochMilli :Long = throw new UnsupportedOperationException(s"($this).epochMilli")
		final override def toUnix :UnixTime = throw new UnsupportedOperationException(s"($this).toUnix")
		final override def toUTC :UTCDateTime = throw new UnsupportedOperationException(s"($this).toUTC")
		final override def toTimestamp :Timestamp = throw new UnsupportedOperationException(s"($this).toTimestamp")
		final override def toInstant :j.Instant = throw new UnsupportedOperationException(s"($this).toInstant")
		final override def toJava :Temporal = throw new UnsupportedOperationException(s"($this).toJava")

		final override def in(zone :TimeZone) :ZoneDateTime = throw new UnsupportedOperationException(s"$this in $zone")

		final override def at(offset :TimeOffset) :ZoneDateTime = throw new UnsupportedOperationException(s"$this at $offset")

	}


}






/** Base trait for time points representing valid, finite values on the time line.
  * All [[net.noresttherein.slang.time.TimePoint TimePoint]] implementations other than provided
  * [[net.noresttherein.slang.time.DawnOfTime DawnOfTime]] and [[net.noresttherein.slang.time.EndOfTime EndOfTime]]
  * must extend this trait rather than `TimePoint` directly.
  */
trait DefiniteTime extends Any with TimePoint {

	final override def isDefinite = true
	override def forcedDefinite :DefiniteTime = this

	override def +(time :TimeSpan) :TimePoint = time match {
		case finite :FiniteTimeSpan => this + finite
		case Eternity => EndOfTime
		case MinusEternity => DawnOfTime
	}

	override def -(time :TimeSpan) :TimePoint = time match {
		case finite :FiniteTimeSpan => this + finite
		case Eternity => DawnOfTime
		case MinusEternity => EndOfTime
	}

	override def -(other :TimePoint) :TimeSpan = other match {
		case finite :DefiniteTime => this - finite
		case DawnOfTime => Eternity
		case EndOfTime => MinusEternity
	}


	def +(time :FiniteTimeSpan) :DefiniteTime
	def -(time :FiniteTimeSpan) :DefiniteTime

	def +(time :Duration) :DefiniteTime
	def -(time :Duration) :DefiniteTime

	def -(other :DefiniteTime) :FiniteTimeSpan =
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




object DefiniteTime {

	@inline def apply()(implicit time :Time = Time.Local) :DefiniteTime = Timestamp.now

	@inline def now(implicit time :Time = Time.Local) :DefiniteTime = Timestamp.now



	def after(lapse :FiniteTimeLapse)(implicit time :Time = Time.Local) :DefiniteTime = lapse match {
		case duration :Duration => Timestamp after duration
		case millis :Milliseconds => UnixTime after millis
		case span :FiniteTimeSpan => Timestamp.now + span
		case _ => ZoneDateTime.now + lapse
	}

	def before(lapse :FiniteTimeLapse)(implicit time :Time = Time.Local) :DefiniteTime = lapse match {
		case duration :Duration => Timestamp before duration
		case millis :Milliseconds => UnixTime before millis
		case span :FiniteTimeSpan => Timestamp.now - span
		case _ => ZoneDateTime.now - lapse
	}


	@inline implicit def toJavaInstant(time :DefiniteTime) :j.Instant = time.toInstant

}







/** Base trait for finite time points carrying information about the zone/offset and thus also full date and clock time.
  */
trait DateTimePoint extends Any with DefiniteTime {
	def zone :TimeZone
	def offset :TimeOffset
	def date :Date
	def local :DateTime
	def time :TimeOfDay

	def day :Int = date.day
	def month :Month = date.month
	def year :Year = date.year
	def hour :Int = time.hour
	def minute :Int = time.minute
	def second :Int = time.second
	override def nano :Int = time.nano

	def dayOfWeek :Day

	def toZoneDateTime :ZoneDateTime

	def +(time :TimeLapse) :TimePoint
	def -(time :TimeLapse) :TimePoint
	def +(time :FiniteTimeLapse) :DateTimePoint
	def -(time :FiniteTimeLapse) :DateTimePoint

	def +(period :Period) :DateTimePoint
	def -(period :Period) :DateTimePoint

	override def +(time :FiniteTimeSpan) :DateTimePoint
	override def -(time :FiniteTimeSpan) :DateTimePoint

	override def +(time :Duration) :DateTimePoint
	override def -(time :Duration) :DateTimePoint


	@inline final def min(that :DateTimePoint) :DateTimePoint = if (compare(that) <= 0) this else that
	@inline final def max(that :DateTimePoint) :DateTimePoint = if (compare(that) >= 0) this else that

}



object DateTimePoint {
	@inline implicit def toJavaOffsetDateTime(time :DateTimePoint) :j.OffsetDateTime =
		time.toZoneDateTime.toJava.toOffsetDateTime

	@inline implicit def toJavaOffsetDateTime(time :ZoneDateTime) :j.OffsetDateTime =
		time.toJava.toOffsetDateTime

	@inline implicit def fromJavaOffsetDateTime(time :j.OffsetDateTime) :ZoneDateTime =
		new ZoneDateTime(time.toZonedDateTime)

}






/** A singularity of the time line, a point which follows in ordering all other `TimePoint` instances.
  * It may be returned as a result of an arithmetic operation involving infinite time lapses `Eternity`
  * or `MinusEternity`.
  */
@SerialVersionUID(1L)
case object EndOfTime extends TimeLimes {

	override def forcedDefinite :Timestamp = Timestamp.Max

	override def +(time :TimeSpan) :TimePoint = time match {
		case MinusEternity => DawnOfTime
		case _ => this
	}

	override def -(time :TimeSpan) :TimePoint = time match {
		case Eternity => DawnOfTime
		case _ => this
	}

	override def -(time :TimePoint) :TimeSpan =
		if (time == this) Immediate
		else Eternity

	override def compare(that :TimePoint) :Int =
		if (that == this) 0
		else 1

	override def toString = "End of time"
}



/** A singularity of the time line, a point which precedes in ordering all other `TimePoint` instances.
  * It may be returned as a result of an arithmetic operation involving infinite time lapses `Eternity`
  * or `MinusEternity`.
  */
@SerialVersionUID(1L)
case object DawnOfTime extends TimeLimes {

	override def forcedDefinite :Timestamp = Timestamp.Min

	override def +(time :TimeSpan) :TimePoint = time match {
		case Eternity => EndOfTime
		case _ => this
	}

	override def -(time :TimeSpan) :TimePoint = time match {
		case MinusEternity => EndOfTime
		case _ => this
	}


	override def -(time :TimePoint) :TimeSpan =
		if (time == this) Immediate
		else Eternity


	override def compare(that :TimePoint) :Int =
		if (that == this) 0
		else -1

	override def toString = "Dawn of time"

}

