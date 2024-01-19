package net.noresttherein.sugar.time

import java.{time => j}
import java.time.chrono.{Chronology, IsoChronology}

import scala.concurrent.duration.Deadline

import net.noresttherein.sugar.time.constants.{MillisInSecond, NanosInMilli}


/** A time point set in the special UTC time zone, to which all date fields are related. It is a simple value type
  * wrapping a `java.time.LocalDateTime` and interpreting it with the fixed `ZoneOffset.UTC`.
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
class UTCDateTime private[time] (val toJava :j.LocalDateTime) extends AnyVal with DateTimePoint with Serializable {

	@inline override def year   :Year  = new Year(toJava.getYear)
	@inline override def month  :Month = Month(toJava.getMonth)
	@inline override def day    :Int   = toJava.getDayOfMonth
	@inline override def hour   :Int   = toJava.getHour
	@inline override def minute :Int   = toJava.getMinute
	@inline override def second :Int   = toJava.getSecond
	@inline override def nano   :Int   = toJava.getNano

	@inline override def dayOfWeek :Day = Day(toJava.getDayOfWeek)

	@inline override def apply(cycle :Cycle) :cycle.Phase = cycle.on(this)


	@inline override def epochSecond :Long = toJava.toEpochSecond(j.ZoneOffset.UTC)

	override def epochMilli :Long = {
		val time = toJava.atZone(j.ZoneOffset.UTC)
		val s = time.toEpochSecond; val m = time.getNano / NanosInMilli
		val max = (Long.MaxValue - m) / MillisInSecond
		if (s > max || s > -max)
			overflow(toString, "epochMilli")
		s * MillisInSecond + m
	}

	@inline override def date   :Date       = new Date(toJava.toLocalDate)
	@inline override def time   :TimeOfDay  = new TimeOfDay(toJava.toLocalTime)
	@inline override def local  :DateTime   = new DateTime(toJava)
	@inline override def zone   :TimeZone   = TimeZone.UTC
	@inline override def offset :TimeOffset = TimeOffset.UTC

	@inline override def toPosix         :PosixTime = new PosixTime(epochMilli)
	@inline override def toTimestamp    :Timestamp = new Timestamp(toJava.toInstant(j.ZoneOffset.UTC))
	@inline override def toZoneDateTime :ZoneDateTime = new ZoneDateTime(toJava atZone j.ZoneOffset.UTC)
	@inline override def toUTC          :UTCDateTime = this
	@inline override def toInstant      :j.Instant = toJava.toInstant(j.ZoneOffset.UTC)
	@inline override def toDeadline     :Deadline = new Timestamp(toJava.toInstant(j.ZoneOffset.UTC)).toDeadline

	@inline override def in(zone :TimeZone) :ZoneDateTime =
		new ZoneDateTime(toJava.toInstant(j.ZoneOffset.UTC).atZone(zone.toJava))

	@inline override def at(offset :TimeOffset) :ZoneDateTime =
		new ZoneDateTime(toJava.toInstant(j.ZoneOffset.UTC).atZone(offset.toJava))



	override def +(time :TimeExtent) :TimePoint = time match {
		case finite :TimeSpan => this + finite
		case period :DateSpan => this + period.toPeriod
		case finite :TimeFrame => this + finite.period + finite.duration //todo
		case Eternity => EndOfTime
		case MinusEternity => DawnOfTime
	}
	override def +(time :TimeFrame) :UTCDateTime = time match {
		case t :TimeSpan => this + t
		case period :DateSpan => this + period.toPeriod
		case _ => this + time.period + time.duration
	}

	@inline override def +(period :Period) :UTCDateTime = new UTCDateTime(toJava plus period.toJava)

	override def +(time :TimeInterval) :TimePoint = time match {
		case finite :TimeSpan => this + finite
		case Eternity => EndOfTime
		case MinusEternity => DawnOfTime
		case _ => throw new IllegalArgumentException(s"($this) + ($time): time extent is neither TimeSpan nor InfiniteTimeInterval")
	}
	@inline override def +(time :TimeSpan) :UTCDateTime =
		new UTCDateTime(toJava plusSeconds time.toSeconds plusNanos time.nanos)

	@inline override def +(time :Duration) :UTCDateTime =
		new UTCDateTime(toJava plus time.toJava)



	override def -(time :TimeExtent) :TimePoint = time match {
		case finite :TimeSpan => this - finite
		case period :DateSpan => this - period.toPeriod
		case finite :TimeFrame => this - finite.period - finite.duration
		case Eternity => DawnOfTime
		case MinusEternity => EndOfTime
	}
	override def -(time :TimeFrame) :UTCDateTime = time match {
		case t :TimeSpan => this - t
		case p :DateSpan => this - p.toPeriod
		case _ => this - time.period - time.duration
	}

	@inline override def -(period :Period) :UTCDateTime = new UTCDateTime(toJava minus period.toJava)

	override def -(time :TimeInterval) :TimePoint = time match {
		case finite :TimeSpan => this - finite
		case Eternity => DawnOfTime
		case MinusEternity => EndOfTime
		case _ => throw new IllegalArgumentException(s"($this) - ($time): time extent is neither TimeSpan nor InfiniteTimeInterval")
	}
	@inline override def -(time :TimeSpan) :UTCDateTime =
		new UTCDateTime(toJava minusSeconds time.toSeconds minusNanos time.nanos)

	@inline override def -(time :Duration) :UTCDateTime =
		new UTCDateTime(toJava minus time.toJava)


	override def -(time :TimePoint) :TimeInterval = time match {
		case utc :UTCDateTime => new Duration(j.Duration.between(utc.toJava, toJava))
		case EndOfTime => MinusEternity
		case DawnOfTime => Eternity
		case _ =>
			new Duration(j.Duration.between(time.toInstant, toJava.toInstant(j.ZoneOffset.UTC)))
	}

	override def -(time :DefiniteTime) :Duration = time match {
		case utc :UTCDateTime => new Duration(j.Duration.between(utc.toJava, toJava))
		case _ => new Duration(j.Duration.between((time in TimeZone.UTC).toJava.toLocalDateTime, toJava))
	}

	@inline override def -(time :Timestamp) :Duration =
		new Duration(j.Duration.between(time.toJava, toJava.toInstant(j.ZoneOffset.UTC)))

	@inline def -(time :UTCDateTime) :Duration =
		new Duration(j.Duration.between(time.toJava, toJava))



	override def compare(that :TimePoint) :Int = that match {
		case utc :UTCDateTime => toJava compareTo utc.toJava
		case finite :DefiniteTime =>
			val time = toJava.toInstant(j.ZoneOffset.UTC)
			val sec1 = time.getEpochSecond; val sec2 = finite.epochSecond
			if (sec1 < sec2) -1
			else if (sec2 < sec1) 1
			else java.lang.Long.signum(time.getNano.toLong - finite.nano)
		case EndOfTime => -1
		case DawnOfTime => 1
	}

	/** Comparison of the relative position of the two points on the time line. */
	@inline def compare(that :UTCDateTime) :Int =  toJava compareTo that.toJava

	@inline def <=(that :UTCDateTime) :Boolean = !(toJava isAfter that.toJava)
	@inline def < (that :UTCDateTime) :Boolean = toJava isBefore that.toJava
	@inline def >=(that :UTCDateTime) :Boolean = !(toJava isBefore that.toJava)
	@inline def > (that :UTCDateTime) :Boolean = toJava isAfter that.toJava

	def <=(that :Timestamp) :Boolean =
		lte(toJava.toEpochSecond(Time.UTC.offset), toJava.getNano, that.toJava.getEpochSecond, that.toJava.getNano)

	def < (that :Timestamp) :Boolean =
		lt(toJava.toEpochSecond(Time.UTC.offset), toJava.getNano, that.toJava.getEpochSecond, that.toJava.getNano)

	def >=(that :Timestamp) :Boolean =
		gte(toJava.toEpochSecond(Time.UTC.offset), toJava.getNano, that.toJava.getEpochSecond, that.toJava.getNano)

	def > (that :Timestamp) :Boolean =
		gt(toJava.toEpochSecond(Time.UTC.offset), toJava.getNano, that.toJava.getEpochSecond, that.toJava.getNano)

	def <=(that :ZoneDateTime) :Boolean =
		lte(toJava.toEpochSecond(Time.UTC.offset), toJava.getNano, that.toJava.toEpochSecond, that.toJava.getNano)

	def < (that :ZoneDateTime) :Boolean =
		lt(toJava.toEpochSecond(Time.UTC.offset), toJava.getNano, that.toJava.toEpochSecond, that.toJava.getNano)

	def >=(that :ZoneDateTime) :Boolean =
		gte(toJava.toEpochSecond(Time.UTC.offset), toJava.getNano, that.toJava.toEpochSecond, that.toJava.getNano)

	def > (that :ZoneDateTime) :Boolean =
		gt(toJava.toEpochSecond(Time.UTC.offset), toJava.getNano, that.toJava.toEpochSecond, that.toJava.getNano)

	def <=(that :PosixTime) :Boolean =
		lte(toJava.toEpochSecond(Time.UTC.offset), toJava.getNano, that.epochSecond, that.nano)

	def < (that :PosixTime) :Boolean =
		lt(toJava.toEpochSecond(Time.UTC.offset), toJava.getNano, that.epochSecond, that.nano)

	def >=(that :PosixTime) :Boolean =
		gte(toJava.toEpochSecond(Time.UTC.offset), toJava.getNano, that.epochSecond, that.nano)

	def > (that :PosixTime) :Boolean =
		gt(toJava.toEpochSecond(Time.UTC.offset), toJava.getNano, that.epochSecond, that.nano)
	
	@inline def min(that :UTCDateTime) :UTCDateTime = if (that.toJava isBefore toJava) that else this
	@inline def max(that :UTCDateTime) :UTCDateTime = if (that.toJava isAfter toJava) that else this

	override def ==(that :TimePoint) :Boolean = that match {
		case finite :DefiniteTime =>
			val time = toJava.toInstant((j.ZoneOffset.UTC))
			time.getEpochSecond == finite.epochSecond && time.getNano == finite.nano
		case _ => false
	}
	@inline def ==(that :UTCDateTime) :Boolean = toJava == that.toJava

	@inline def ==(that :Timestamp) :Boolean =
		toJava.toEpochSecond(Time.UTC.offset) == that.toJava.getEpochSecond && toJava.getNano == that.toJava.getNano

	@inline def ==(that :ZoneDateTime) :Boolean =
		toJava.toEpochSecond(Time.UTC.offset) == that.toJava.toEpochSecond && toJava.getNano == that.toJava.getNano

	@inline def ==(that :PosixTime) :Boolean =
		epochMilli == that.epochMilli && toJava.getNano % NanosInMilli == 0L

	override def toString :String = toJava.toString + "Z"
}






@SerialVersionUID(Ver)
case object UTCDateTime {

	def apply(time :j.LocalDateTime) :UTCDateTime = {
		val chrono = time.getChronology
		if (chrono != null && chrono != IsoChronology.INSTANCE)
			throw new IllegalArgumentException("UTCDateTime accepts only dates in IsoChronology; got " + chrono)
		new UTCDateTime(time)
	}

	def apply(date :Date, time :TimeOfDay) :UTCDateTime = {
		val chrono = date.getChronology
		if (chrono != null && chrono != IsoChronology.INSTANCE)
			throw new IllegalArgumentException("UTCDateTime accepts only dates in IsoChronology; got " + chrono)
		new UTCDateTime(j.LocalDateTime.of(date.toJava, time.toJava))
	}

	@inline def apply(time :Timestamp) :UTCDateTime = new UTCDateTime(j.LocalDateTime.ofInstant(time, Time.UTC.zone))


	@inline def apply(time :Time = Time.UTC) :UTCDateTime =
		new UTCDateTime(j.LocalDateTime.now(time.clock.withZone(j.ZoneOffset.UTC)))

	@inline def now(implicit time :Time = Time.UTC) :UTCDateTime =
		new UTCDateTime(j.LocalDateTime.now(time.clock.withZone(j.ZoneOffset.UTC)))


	def after(extent :TimeFrame)(implicit time :Time = Time.UTC) :UTCDateTime = {
		val now = j.LocalDateTime.now(time.clock.withZone(j.ZoneOffset.UTC))
		extent match {
			case t :TimeSpan =>
				new UTCDateTime(now plus t.toDuration.toJava)
			case p :DateSpan =>
				new UTCDateTime(now plus p.toPeriod.toJava)
			case _ =>
				new UTCDateTime(now plus extent.period.toJava plus extent.duration.toJava)
		}
	}

	def before(extent :TimeFrame)(implicit time :Time = Time.UTC) :UTCDateTime = {
		val now = j.LocalDateTime.now(time.clock.withZone(j.ZoneOffset.UTC))
		extent match {
			case t :TimeSpan => new UTCDateTime(now minus t.toDuration.toJava)
			case p :DateSpan => new UTCDateTime(now minus p.toPeriod.toJava)
			case _ => new UTCDateTime(now minus extent.period.toJava minus extent.duration.toJava)
		}
	}



	def unapply(time :TimePoint) :Option[(Date, TimeOfDay)] = time match {
		case utc :UTCDateTime => Some((utc.date, utc.time))
		case _ => None
	}


	@inline implicit def UTCDateTimeToTimestamp(time :UTCDateTime)   :Timestamp   = time.toTimestamp
	@inline implicit def UTCDateTimeFromTimestamp(time :Timestamp)   :UTCDateTime = UTCDateTime(time)
	@inline implicit def UTCDateTimeToJavaInstant(time :UTCDateTime) :j.Instant   = time.toInstant

	@inline implicit def UTCDateTimeFromJavaInstant(time :j.Instant) :UTCDateTime =
		new UTCDateTime(time.atOffset(j.ZoneOffset.UTC).toLocalDateTime)

	@inline implicit def UTCDateTimeToJavaZonedDateTime(time :UTCDateTime) :j.ZonedDateTime =
		time.toJava atZone j.ZoneOffset.UTC


	final val Max = new UTCDateTime(j.LocalDateTime.MAX)
	final val Min = new UTCDateTime(j.LocalDateTime.MIN)

}

