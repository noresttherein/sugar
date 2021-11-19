package net.noresttherein.slang.time

import java.{time => j}
import scala.concurrent.duration.Deadline


/** A time point carrying time zone information and thus reflecting a unique date. It is a lightweight value type
  * wrapping a `java.time.ZonedDateTime`.
  * @author Marcin Mościcki marcin@moscicki.net
  */ //todo: OffsetDateTime?
@SerialVersionUID(1L)
class ZoneDateTime private[time] (override val toJava :j.ZonedDateTime)
	extends AnyVal with DateTimePoint with Serializable
{
	@inline override def date :Date = new Date(toJava.toLocalDate)
	@inline override def local :DateTime = new DateTime(toJava.toLocalDateTime)
	@inline override def time :TimeOfDay = new TimeOfDay(toJava.toLocalTime)

	@inline override def zone :TimeZone = new TimeZone(toJava.getZone)
	@inline override def offset :TimeOffset = new TimeOffset(toJava.getOffset)

	@inline override def year :Year = new Year(toJava.getYear)
	@inline override def month :Month = toJava.getMonth
	@inline override def day :Int = toJava.getDayOfMonth
	@inline override def hour :Int = toJava.getHour
	@inline override def minute :Int = toJava.getMinute
	@inline override def second :Int = toJava.getSecond
	@inline override def nano :Int = toJava.getNano

	@inline override def dayOfWeek :Day = toJava.getDayOfWeek

	@inline override def apply(cycle :Cycle) :cycle.Phase = cycle.of(this)


	@inline override def epochSecond :Long = toJava.toEpochSecond

	override def epochMilli :Long = {
		val s = toJava.toEpochSecond; val n = toJava.getNano
		val max = (Long.MaxValue - n / NanosInMilli) / MillisInSecond
		if (s > max || s < -max)
			overflow(toString, "epochMilli")
		s * MillisInSecond + n / NanosInMilli
	}


	@inline override def toTimestamp :Timestamp = new Timestamp(toJava.toInstant)

	@inline override def toUnix :UnixTime = new UnixTime(epochMilli)

	@inline override def toUTC :UTCDateTime = new UTCDateTime(toJava.withZoneSameInstant(j.ZoneOffset.UTC).toLocalDateTime)

	@inline override def toZoneDateTime :ZoneDateTime = this

	@inline override def toInstant :j.Instant = toJava.toInstant

	@inline override def toDeadline :Deadline = new Timestamp(toJava.toInstant).toDeadline

	@inline override def in(zone :TimeZone) :ZoneDateTime = new ZoneDateTime(toJava.withZoneSameInstant(zone.toJava))

	@inline override def at(offset :TimeOffset) :ZoneDateTime = new ZoneDateTime(toJava.withZoneSameInstant(offset.toJava))

	override def +(time :TimeLapse) :TimePoint = time match {
		case finite :FiniteTimeSpan => this + finite
		case period :FiniteDateSpan => this + period.toPeriod
		case finite :FiniteTimeLapse => this + finite.period + finite.duration
		case Eternity => EndOfTime
		case MinusEternity => DawnOfTime
	}

	override def +(time :FiniteTimeLapse) :ZoneDateTime = time match {
		case t :FiniteTimeSpan => this + t
		case p :FiniteDateSpan => this + p.toPeriod
		case _ => this + time.period + time.duration
	}

	@inline override def +(time :Period) :ZoneDateTime = new ZoneDateTime(toJava plus time.toJava)

	override def +(time :TimeSpan) :TimePoint = time match {
		case finite :FiniteTimeSpan => this + finite
		case Eternity => EndOfTime
		case MinusEternity => DawnOfTime
	}

	@inline override def +(time :FiniteTimeSpan) :ZoneDateTime =
		new ZoneDateTime(toJava plusSeconds time.inSeconds plusNanos time.nanos)

	@inline override def +(time :Duration) :ZoneDateTime = new ZoneDateTime(toJava plus time.toJava)

	override def -(time :TimeLapse) :TimePoint = time match {
		case finite :FiniteTimeSpan => this - finite
		case period :FiniteDateSpan => this - period
		case finite :FiniteTimeLapse => this - finite.period - finite.duration
		case Eternity => DawnOfTime
		case MinusEternity => EndOfTime
	}

	override def -(time :FiniteTimeLapse) :ZoneDateTime = time match {
		case t :FiniteTimeSpan => this - t
		case p :FiniteDateSpan => this - p.toPeriod
		case _ => this - time.period - time.duration
	}

	@inline override def -(period :Period) :ZoneDateTime = new ZoneDateTime(toJava minus period)

	override def -(time :TimeSpan) :TimePoint = time match {
		case finite :FiniteTimeSpan => this - finite
		case Eternity => DawnOfTime
		case MinusEternity => EndOfTime
	}

	@inline override def -(time :FiniteTimeSpan) :ZoneDateTime =
		new ZoneDateTime(toJava minusSeconds time.inSeconds minusNanos time.nanos)

	@inline override def -(time :Duration) :ZoneDateTime = new ZoneDateTime(toJava minus time.toJava)


	override def -(time :TimePoint) :TimeSpan = time match {
		case finite :DefiniteTime => new Duration(j.Duration.between(finite.toInstant, toJava))
		case DawnOfTime => Eternity
		case EndOfTime => MinusEternity
	}

	@inline override def -(time :DefiniteTime) :Duration = new Duration(j.Duration.between(time.toInstant, toJava))

	@inline override def -(time :Timestamp) :Duration = new Duration(j.Duration.between(time, toJava))

	@inline def -(time :ZoneDateTime) :Duration = new Duration(j.Duration.between(time.toJava, toJava))



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

	def <=(that :UnixTime) :Boolean =
		lte(toJava.toEpochSecond, toJava.getNano, that.epochSecond, that.nano)

	def < (that :UnixTime) :Boolean =
		lt(toJava.toEpochSecond, toJava.getNano, that.epochSecond, that.nano)

	def >=(that :UnixTime) :Boolean =
		gte(toJava.toEpochSecond, toJava.getNano, that.epochSecond, that.nano)

	def > (that :UnixTime) :Boolean =
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

	@inline def ==(that :UnixTime) :Boolean =
		epochMilli == that.epochMilli && toJava.getNano % NanosInMilli == 0L

	override def toString :String = toJava.toString
}






object ZoneDateTime {
	@inline def apply(time :j.ZonedDateTime) :ZoneDateTime = new ZoneDateTime(time)
	@inline def apply(time :j.OffsetDateTime) :ZoneDateTime = new ZoneDateTime(time.toZonedDateTime)

	@inline def apply(date :Date, time :TimeOfDay, zone :TimeZone) :ZoneDateTime =
		new ZoneDateTime(j.ZonedDateTime.of(date.toJava, time.toJava, zone.toJava))

	@inline def apply(date :Date, time :TimeOfDay, offset :TimeOffset) :ZoneDateTime =
		new ZoneDateTime(j.ZonedDateTime.of(date.toJava, time.toJava, offset.toJava))

//	@inline def apply(timestamp: Timestamp)(implicit time :Time = Time.Local) :ZoneDateTime =
//		new ZoneDateTime(j.ZonedDateTime.ofInstant(timestamp, time.zone))

	@inline def apply()(implicit time :Time = Time.Local) :ZoneDateTime =
		new ZoneDateTime(j.ZonedDateTime.now(time.clock))

	@inline def now(implicit time :Time = Time.Local) :ZoneDateTime =
		new ZoneDateTime(j.ZonedDateTime.now(time.clock))

	@inline def in(zone :TimeZone)(implicit time :Time = Time.Local) :ZoneDateTime =
		new ZoneDateTime(j.ZonedDateTime.now(time.clock.withZone(zone.toJava)))

	@inline def at(offset :TimeOffset)(implicit time :Time = Time.Local) :ZoneDateTime =
		new ZoneDateTime(j.ZonedDateTime.now(time.clock.withZone(offset.toJava)))

	@inline def utc :ZoneDateTime = new ZoneDateTime(j.ZonedDateTime.now(Time.UTC))


	def after(lapse :FiniteTimeLapse)(implicit time :Time = Time.Local) :ZoneDateTime =
		new ZoneDateTime(lapse match {
			case t :FiniteTimeSpan => j.ZonedDateTime.now(time.clock) plus t.toDuration.toJava
			case p :FiniteDateSpan => j.ZonedDateTime.now(time.clock) plus p.toPeriod.toJava
			case _ =>j.ZonedDateTime.now(time.clock) plus lapse.period.toJava plus lapse.duration.toJava
		})

	def before(lapse :FiniteTimeLapse)(implicit time :Time = Time.Local) :ZoneDateTime =
		new ZoneDateTime(lapse match {
			case t :FiniteTimeSpan => j.ZonedDateTime.now(time.clock) minus t.toDuration.toJava
			case p :FiniteDateSpan => j.ZonedDateTime.now(time.clock) minus p.toPeriod.toJava
			case _ => j.ZonedDateTime.now(time.clock) minus lapse.period.toJava minus lapse.duration.toJava
		})



	def unapply(time :TimePoint) :Option[(Date, TimeOfDay, TimeZone)] = time match {
		case t :ZoneDateTime => Some((t.date, t.time, t.zone))
		case _ => None
	}


	@inline implicit def toJavaZonedDateTime(time :ZoneDateTime) :j.ZonedDateTime = time.toJava
	@inline implicit def fromJavaZonedDateTime(time :j.ZonedDateTime) :ZoneDateTime = new ZoneDateTime(time)
	@inline implicit def toTimestamp(time :ZoneDateTime) :Timestamp = time.toTimestamp
}

