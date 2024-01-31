package net.noresttherein.sugar.time

import java.{time => j}

import scala.concurrent.duration.Deadline

import net.noresttherein.sugar.time.constants.{MillisInSecond, NanosInMilli}
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}


/** A time point carrying time zone information and thus reflecting a unique date. It is a lightweight value type
  * wrapping a `java.time.ZonedDateTime`.
  * @author Marcin Mościcki
  */ //todo: OffsetDateTime?
@SerialVersionUID(Ver) //consider: renaming to DateTimeAtZone/DateTimeInZone
class ZoneDateTime private[time] (override val toJava :j.ZonedDateTime)
	extends AnyVal with DateTimePoint with Serializable
{
	@inline override def date  :Date      = new Date(toJava.toLocalDate)
	@inline override def local :DateTime  = new DateTime(toJava.toLocalDateTime)
	@inline override def time  :TimeOfDay = new TimeOfDay(toJava.toLocalTime)

	@inline override def zone   :TimeZone   = new TimeZone(toJava.getZone)
	@inline override def offset :TimeOffset = new TimeOffset(toJava.getOffset)

	@inline override def year   :Year  = new Year(toJava.getYear)
	@inline override def month  :Month = toJava.getMonth
	@inline override def day    :Int   = toJava.getDayOfMonth
	@inline override def hour   :Int   = toJava.getHour
	@inline override def minute :Int   = toJava.getMinute
	@inline override def second :Int   = toJava.getSecond
	@inline override def nano   :Int   = toJava.getNano

	@inline override def dayOfWeek :Day = toJava.getDayOfWeek

	@inline override def apply(cycle :Cycle) :cycle.Phase = cycle.on(this)


	@inline override def epochSecond :Long = toJava.toEpochSecond

	override def epochMilli :Long = {
		val s = toJava.toEpochSecond; val n = toJava.getNano
		val max = (Long.MaxValue - n / NanosInMilli) / MillisInSecond
		if (s > max || s < -max)
			overflow(toString, "epochMilli")
		s * MillisInSecond + n / NanosInMilli
	}


	@inline override def toTimestamp    :Timestamp    = new Timestamp(toJava.toInstant)
	@inline override def toPosix         :PosixTime    = new PosixTime(epochMilli)
	@inline override def toZoneDateTime :ZoneDateTime = this
	@inline override def toInstant      :j.Instant    = toJava.toInstant
	@inline override def toDeadline     :Deadline     = new Timestamp(toJava.toInstant).toDeadline
	@inline override def toUTC          :UTCDateTime  =
		new UTCDateTime(toJava.withZoneSameInstant(j.ZoneOffset.UTC).toLocalDateTime)

	@inline override def in(zone :TimeZone)     :ZoneDateTime =
		new ZoneDateTime(toJava.withZoneSameInstant(zone.toJava))

	@inline override def at(offset :TimeOffset) :ZoneDateTime =
		new ZoneDateTime(toJava.withZoneSameInstant(offset.toJava))

	override def +(time :TimeExtent) :TimePoint = time match {
		case finite :TimeSpan => this + finite
		case period :DateSpan => this + period.toPeriod
		case finite :TimeFrame => this + finite.period + finite.duration
		case Eternity => EndOfTime
		case MinusEternity => DawnOfTime
	}

	override def +(time :TimeFrame) :ZoneDateTime = time match {
		case t :TimeSpan => this + t
		case p :DateSpan => this + p.toPeriod
		case _ => this + time.period + time.duration
	}

	@inline override def +(time :Period) :ZoneDateTime = new ZoneDateTime(toJava plus time.toJava)

	override def +(time :TimeInterval) :TimePoint = time match {
		case finite :TimeSpan => this + finite
		case Eternity => EndOfTime
		case MinusEternity => DawnOfTime
	}

	@inline override def +(time :TimeSpan) :ZoneDateTime =
		new ZoneDateTime(toJava plusSeconds time.toSeconds plusNanos time.nanos)

	@inline override def +(time :Duration) :ZoneDateTime = new ZoneDateTime(toJava plus time.toJava)

	override def -(time :TimeExtent) :TimePoint = time match {
		case finite :TimeSpan => this - finite
		case period :DateSpan => this - period
		case finite :TimeFrame => this - finite.period - finite.duration
		case Eternity => DawnOfTime
		case MinusEternity => EndOfTime
	}

	override def -(time :TimeFrame) :ZoneDateTime = time match {
		case t :TimeSpan => this - t
		case p :DateSpan => this - p.toPeriod
		case _ => this - time.period - time.duration
	}

	@inline override def -(period :Period) :ZoneDateTime = new ZoneDateTime(toJava minus period)

	override def -(time :TimeInterval) :TimePoint = time match {
		case finite :TimeSpan => this - finite
		case Eternity => DawnOfTime
		case MinusEternity => EndOfTime
	}

	@inline override def -(time :TimeSpan) :ZoneDateTime =
		new ZoneDateTime(toJava minusSeconds time.toSeconds minusNanos time.nanos)

	@inline override def -(time :Duration) :ZoneDateTime = new ZoneDateTime(toJava minus time.toJava)


	override def -(time :TimePoint) :TimeInterval = time match {
		case finite :DefiniteTime => new Duration(j.Duration.between(finite.toInstant, toJava))
		case DawnOfTime => Eternity
		case EndOfTime => MinusEternity
	}

	@inline override def -(time :DefiniteTime) :Duration = new Duration(j.Duration.between(time.toInstant, toJava))
	@inline override def -(time :Timestamp)    :Duration = new Duration(j.Duration.between(time, toJava))
	@inline def -(time :ZoneDateTime)          :Duration = new Duration(j.Duration.between(time.toJava, toJava))



	/** Comparison of the relative position of the two points on the time line. This is equivalent to
	  * `this.toInstant compareTo that.toInstant`; two instances compare as equal even if they are in different
	  * offsets or use different chronologies (unlike `toJava compareTo that.toJava`). This means that equivalency
	  * relation defined by this method is a proper superset of equivalency as defined by `equals`.
	  */
	override def compare(that :TimePoint) :Int = that match {
		case finite :DefiniteTime =>
			val s1 = toJava.toEpochSecond; val s2 = finite.epochSecond
			if (s1 < s2) -1
			else if (s1 > s2) 1
			else java.lang.Long.signum(toJava.getNano.toLong - finite.nano)
		case DawnOfTime => 1
		case EndOfTime => -1
	}

	/** Comparison of the relative position of the two points on the time line. This is equivalent to
	  * `this.toInstant compareTo that.toInstant`; two instances compare as equal even if they are in different
	  * offsets or use different chronologies (unlike `toJava compareTo that.toJava`). This means that equivalency
	  * relation defined by this method is a proper superset of equivalency as defined by `equals`.
	  */
	def compare(that :ZoneDateTime) :Int = {
		val s1 = toJava.toEpochSecond; val s2 = that.toJava.toEpochSecond
		if (s1 < s2) -1
		else if (s1 > s2) 1
		else java.lang.Long.signum(toJava.getNano.toLong - that.toJava.getNano)
	}

	@inline def <=(that :ZoneDateTime) :Boolean = !(toJava isAfter that.toJava)
	@inline def < (that :ZoneDateTime) :Boolean = toJava isBefore that.toJava
	@inline def >=(that :ZoneDateTime) :Boolean = !(toJava isBefore that.toJava)
	@inline def > (that :ZoneDateTime) :Boolean = toJava isAfter that.toJava
	
	def <=(that :Timestamp) :Boolean =
		lte(toJava.toEpochSecond, toJava.getNano, that.toJava.getEpochSecond, that.toJava.getNano)

	def < (that :Timestamp) :Boolean =
		lt(toJava.toEpochSecond, toJava.getNano, that.toJava.getEpochSecond, that.toJava.getNano)

	def >=(that :Timestamp) :Boolean =
		gte(toJava.toEpochSecond, toJava.getNano, that.toJava.getEpochSecond, that.toJava.getNano)

	def > (that :Timestamp) :Boolean =
		gt(toJava.toEpochSecond, toJava.getNano, that.toJava.getEpochSecond, that.toJava.getNano)

	def <=(that :UTCDateTime) :Boolean =
		lte(toJava.toEpochSecond, toJava.getNano, that.toJava.toEpochSecond(Time.UTC.offset), that.toJava.getNano)

	def < (that :UTCDateTime) :Boolean =
		lt(toJava.toEpochSecond, toJava.getNano, that.toJava.toEpochSecond(Time.UTC.offset), that.toJava.getNano)

	def >=(that :UTCDateTime) :Boolean =
		gte(toJava.toEpochSecond, toJava.getNano, that.toJava.toEpochSecond(Time.UTC.offset), that.toJava.getNano)

	def > (that :UTCDateTime) :Boolean =
		gt(toJava.toEpochSecond, toJava.getNano, that.toJava.toEpochSecond(Time.UTC.offset), that.toJava.getNano)

	def <=(that :PosixTime) :Boolean =
		lte(toJava.toEpochSecond, toJava.getNano, that.epochSecond, that.nano)

	def < (that :PosixTime) :Boolean =
		lt(toJava.toEpochSecond, toJava.getNano, that.epochSecond, that.nano)

	def >=(that :PosixTime) :Boolean =
		gte(toJava.toEpochSecond, toJava.getNano, that.epochSecond, that.nano)

	def > (that :PosixTime) :Boolean =
		gt(toJava.toEpochSecond, toJava.getNano, that.epochSecond, that.nano)

	@inline def min(that :ZoneDateTime) :ZoneDateTime = if (that.toJava isBefore toJava) that else this
	@inline def max(that :ZoneDateTime) :ZoneDateTime = if (that.toJava isAfter toJava) that else this

	override def ==(that :TimePoint) :Boolean = that match {
		case finite :DefiniteTime => toJava.toEpochSecond == finite.epochSecond && toJava.getNano == finite.nano
		case _ => false
	}
	@inline def ==(that :ZoneDateTime) :Boolean = toJava == that.toJava

	@inline def ==(that :Timestamp) :Boolean =
		toJava.toEpochSecond == toJava.toEpochSecond && toJava.getNano == that.toJava.getNano

	@inline def ==(that :UTCDateTime) :Boolean =
		toJava.toEpochSecond == that.toJava.toEpochSecond(Time.UTC.offset) && toJava.getNano == that.toJava.getNano

	@inline def ==(that :PosixTime) :Boolean =
		epochMilli == that.epochMilli && toJava.getNano % NanosInMilli == 0L

	override def toString :String = toJava.toString
}






@SerialVersionUID(Ver)
case object ZoneDateTime {
	@inline def apply(time :j.ZonedDateTime) :ZoneDateTime = new ZoneDateTime(time)
	@inline def apply(time :j.OffsetDateTime) :ZoneDateTime = new ZoneDateTime(time.toZonedDateTime)

	@inline def apply(date :Date, time :TimeOfDay, zone :TimeZone) :ZoneDateTime =
		new ZoneDateTime(j.ZonedDateTime.of(date.toJava, time.toJava, zone.toJava))

	@inline def apply(date :Date, time :TimeOfDay, offset :TimeOffset) :ZoneDateTime =
		new ZoneDateTime(j.ZonedDateTime.of(date.toJava, time.toJava, offset.toJava))

//	@inline def apply(timestamp: Timestamp)(implicit time :Time = Time.Local) :ZoneDateTime =
//		new ZoneDateTime(j.ZonedDateTime.ofInstant(timestamp, time.zone))

	@inline def apply(time :Time = Time.Local) :ZoneDateTime =
		new ZoneDateTime(j.ZonedDateTime.now(time.clock))

	@inline def now(implicit time :Time = Time.Local) :ZoneDateTime =
		new ZoneDateTime(j.ZonedDateTime.now(time.clock))

	@inline def in(zone :TimeZone)(implicit time :Time = Time.Local) :ZoneDateTime =
		new ZoneDateTime(j.ZonedDateTime.now(time.clock.withZone(zone.toJava)))

	@inline def at(offset :TimeOffset)(implicit time :Time = Time.Local) :ZoneDateTime =
		new ZoneDateTime(j.ZonedDateTime.now(time.clock.withZone(offset.toJava)))

	@inline def utc :ZoneDateTime = new ZoneDateTime(j.ZonedDateTime.now(Time.UTC))


	def after(extent :TimeFrame)(implicit time :Time = Time.Local) :ZoneDateTime =
		new ZoneDateTime(extent match {
			case t :TimeSpan => j.ZonedDateTime.now(time.clock) plus t.toDuration.toJava
			case p :DateSpan => j.ZonedDateTime.now(time.clock) plus p.toPeriod.toJava
			case _ =>j.ZonedDateTime.now(time.clock) plus extent.period.toJava plus extent.duration.toJava
		})

	def before(extent :TimeFrame)(implicit time :Time = Time.Local) :ZoneDateTime =
		new ZoneDateTime(extent match {
			case t :TimeSpan => j.ZonedDateTime.now(time.clock) minus t.toDuration.toJava
			case p :DateSpan => j.ZonedDateTime.now(time.clock) minus p.toPeriod.toJava
			case _ => j.ZonedDateTime.now(time.clock) minus extent.period.toJava minus extent.duration.toJava
		})



	def unapply(time :TimePoint) :Maybe[(Date, TimeOfDay, TimeZone)] = time match {
		case t :ZoneDateTime => Yes((t.date, t.time, t.zone))
		case _               => No
	}


	@inline implicit def ZoneDateTimeToJavaZonedDateTime(time :ZoneDateTime) :j.ZonedDateTime = time.toJava
	@inline implicit def ZoneDateTimeFromJavaZonedDateTime(time :j.ZonedDateTime) :ZoneDateTime = new ZoneDateTime(time)
	@inline implicit def ZoneDateTimeToTimestamp(time :ZoneDateTime) :Timestamp = time.toTimestamp
}

