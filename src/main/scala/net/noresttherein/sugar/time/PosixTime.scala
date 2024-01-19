package net.noresttherein.sugar.time

import java.{time => j}

import scala.concurrent.duration.{Deadline, FiniteDuration}

import net.noresttherein.sugar.time.constants.{MillisInSecond, NanosInMilli, NanosInSecond}


/** A time point defined in terms of the ''epoch milli'' (aka 'Unix time'), that is the number of milliseconds
  * elapsed since 1970-01-01 UTC. This is the most lightweight [[net.noresttherein.sugar.time.TimePoint TimePoint]]
  * implementation, being a simple value class wrapping a `Long` value. All arithmetic operations will return
  * another `PosixTime` or `Milliseconds` instance if possible.
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
class PosixTime(override val epochMilli :Long) extends AnyVal with DefiniteTime with Serializable {

	@inline override def apply(cycle :Cycle) :cycle.Phase = cycle.on(this)

	@inline override def epochSecond :Long = {
		val s = epochMilli / MillisInSecond
		if (epochMilli >= 0 | s * MillisInSecond == epochMilli) s
		else s - 1 //ensure that this.nano is not negative
	}

	@inline override def nano :Int = {
		val res = epochMilli % MillisInSecond * NanosInMilli
		if (res >= 0) res.toInt
		else (res + NanosInSecond).toInt
	}



	@inline override def toUTC :UTCDateTime =
		new UTCDateTime(j.LocalDateTime.ofEpochSecond(epochMilli / 1000, (epochMilli % 1000).toInt, j.ZoneOffset.UTC))

	@inline override def toPosix     :PosixTime = this
	@inline override def toTimestamp :Timestamp = j.Instant.ofEpochMilli(epochMilli)
	@inline override def toInstant   :j.Instant = j.Instant.ofEpochMilli(epochMilli)
	@inline override def toJava      :j.Instant = j.Instant.ofEpochMilli(epochMilli)
	@inline override def toDeadline  :Deadline  = Deadline(FiniteDuration(epochMilli, TimeUnit.Millis))

	@inline override def in(zone :TimeZone) :ZoneDateTime =
		new ZoneDateTime(j.ZonedDateTime.ofInstant(toInstant, zone.toJava))

	@inline override def at(offset :TimeOffset) :ZoneDateTime =
		new ZoneDateTime(j.ZonedDateTime.ofInstant(toInstant, offset.toJava))



	override def +(time :TimeInterval) :TimePoint = time match {
		case millis :Milliseconds if (if (epochMilli > 0) millis.toMillis <= Long.MaxValue - epochMilli
		                              else millis.toMillis >= Long.MinValue - epochMilli) =>
			new PosixTime(epochMilli + millis.toMillis)
		case finite :TimeSpan => this + finite
		case Eternity => EndOfTime
		case MinusEternity => DawnOfTime
	}

	override def -(time :TimeInterval) :TimePoint = time match {
		case millis :Milliseconds if (if (epochMilli > 0) millis.toMillis >= epochMilli - Long.MaxValue
		                              else millis.toMillis <= epochMilli - Long.MinValue) =>
			new PosixTime(epochMilli - millis.toMillis)
		case finite :TimeSpan => this - finite
		case Eternity => DawnOfTime
		case MinusEternity => EndOfTime
	}


	@inline override def +(time :TimeSpan) :Timestamp = plus(time.seconds, time.nanos)
	@inline override def -(time :TimeSpan) :Timestamp = minus(time.seconds, time.nanos)

	@inline override def +(time :Duration) :Timestamp = plus(time.toJava.getSeconds, time.toJava.getNano)
	@inline override def -(time :Duration) :Timestamp = minus(time.toJava.getSeconds, time.toJava.getNano)

	private[sugar] def plus(seconds :Long, nanos :Int) :Timestamp = {
		val s1 = epochMilli / 1000L
		if (if (s1 > 0) seconds > Long.MaxValue - s1 else seconds < Long.MinValue - s1)
			overflow(toString, " + ", s"${seconds}s ${nanos}ms")
		new Timestamp(j.Instant.ofEpochSecond(s1 + seconds, epochMilli % 1000 * NanosInMilli + nanos))
	}

	private[sugar] def minus(seconds :Long, nanos :Int) :Timestamp = {
		val s1 = epochMilli / 1000L
		if (if (s1 > 0) seconds < s1 - Long.MaxValue else seconds > s1 - Long.MinValue)
			overflow(toString, " - ", s"${seconds}s ${nanos}ns")
		new Timestamp(j.Instant.ofEpochSecond(epochMilli / 1000 - seconds, epochMilli % 1000 * NanosInMilli - nanos))
	}


	def +(millis :Milliseconds) :PosixTime =
		if (if (epochMilli > 0) millis.toMillis > Long.MaxValue - epochMilli
		    else millis.toMillis < Long.MinValue - epochMilli
		)
			overflow(toString, " + ", millis.toString)
		else new PosixTime(epochMilli + millis.toMillis)

	def -(millis :Milliseconds) :PosixTime =
		if (if (epochMilli > 0) millis.toMillis < epochMilli - Long.MaxValue
		    else millis.toMillis > epochMilli - Long.MinValue
		)
			overflow(toString, " - ", millis.toString)
		else new PosixTime(epochMilli - millis.toMillis)


	override def -(time :TimePoint) :TimeInterval = time match {
		case other :PosixTime if (if (epochMilli > 0) other.epochMilli < epochMilli - Long.MaxValue
		                         else other.epochMilli > epochMilli - Long.MinValue) =>
			new Milliseconds(epochMilli - other.epochMilli)
		case finite :DefiniteTime => this - finite
		case DawnOfTime => Eternity
		case EndOfTime => MinusEternity
	}
	override def -(time :DefiniteTime) :Duration = {
		val s1 = epochMilli / 1000L; val s2 = time.epochSecond
		if (if (s1 > 0) s2 < s1 - Long.MaxValue else s2 > s1 - Long.MinValue)
			overflow(toString, " - ", time)
		j.Duration.ofSeconds(s1 - s2, epochMilli % 1000L * NanosInMilli - time.nano)
	}
	override def -(time :Timestamp) :Duration = {
		val s1 = epochMilli / 1000L; val s2 = time.toJava.getEpochSecond
		if (if (s1 > 0) s2 < s1 - Long.MaxValue else s2 > s1 - Long.MinValue)
			overflow(toString, " - ", time)
		j.Duration.ofSeconds(s1 - s2, epochMilli % 1000L * NanosInMilli - time.toJava.getNano)
	}

	def minus(other :PosixTime) :Milliseconds =
		if (if (epochMilli > 0) other.epochMilli < epochMilli - Long.MaxValue
		    else other.epochMilli > epochMilli - Long.MinValue
		)
			overflow(toString, " - ", other.toString)
		else new Milliseconds(epochMilli - other.epochMilli)



	override def compare(that :TimePoint) :Int = that match {
		case other :PosixTime =>
			if (epochMilli < other.epochMilli) -1
			else if (epochMilli > other.epochMilli) 1
			else 0
		case finite :DefiniteTime =>
			val s1 = epochMilli / 1000L; val s2 = finite.epochSecond
			if (s1 < s2) -1
			else if (s1 > s2) 1
			else java.lang.Long.signum((epochMilli % 1000L * NanosInMilli) - finite.nano)
		case DawnOfTime => 1
		case EndOfTime => -1
	}


	@inline def compare(that :PosixTime) :Int =
		if (epochMilli < that.epochMilli) -1
		else if (epochMilli > that.epochMilli) 1
		else 0

	@inline def <=(that :PosixTime) :Boolean = epochMilli <= that.epochMilli
	@inline def < (that :PosixTime) :Boolean = epochMilli < that.epochMilli
	@inline def >=(that :PosixTime) :Boolean = epochMilli >= that.epochMilli
	@inline def > (that :PosixTime) :Boolean = epochMilli > that.epochMilli

	def <=(that :Timestamp) :Boolean =
		lte(epochSecond, nano, that.toJava.getEpochSecond, that.toJava.getNano)

	def < (that :Timestamp) :Boolean =
		lt(epochSecond, nano, that.toJava.getEpochSecond, that.toJava.getNano)

	def >=(that :Timestamp) :Boolean =
		gte(epochSecond, nano, that.toJava.getEpochSecond, that.toJava.getNano)

	def > (that :Timestamp) :Boolean =
		gt(epochSecond, nano, that.toJava.getEpochSecond, that.toJava.getNano)

	def <=(that :ZoneDateTime) :Boolean =
		lte(epochSecond, nano, that.toJava.toEpochSecond, that.toJava.getNano)

	def < (that :ZoneDateTime) :Boolean =
		lt(epochSecond, nano, that.toJava.toEpochSecond, that.toJava.getNano)

	def >=(that :ZoneDateTime) :Boolean =
		gte(epochSecond, nano, that.toJava.toEpochSecond, that.toJava.getNano)

	def > (that :ZoneDateTime) :Boolean =
		gt(epochSecond, nano, that.toJava.toEpochSecond, that.toJava.getNano)

	def <=(that :UTCDateTime) :Boolean =
		lte(epochSecond, nano, that.toJava.toEpochSecond(Time.UTC.offset), that.toJava.getNano)

	def < (that :UTCDateTime) :Boolean =
		lt(epochSecond, nano, that.toJava.toEpochSecond(Time.UTC.offset), that.toJava.getNano)

	def >=(that :UTCDateTime) :Boolean =
		gte(epochSecond, nano, that.toJava.toEpochSecond(Time.UTC.offset), that.toJava.getNano)

	def > (that :UTCDateTime) :Boolean =
		gt(epochSecond, nano, that.toJava.toEpochSecond(Time.UTC.offset), that.toJava.getNano)

	@inline def min(that :PosixTime) :PosixTime = if (epochMilli <= that.epochMilli) this else that
	@inline def max(that :PosixTime) :PosixTime = if (epochMilli >= that.epochMilli) this else that

	override def ==(that :TimePoint) :Boolean = that match {
		case finite :DefiniteTime =>
			epochMilli / 1000 == finite.epochSecond && epochMilli % 1000 * NanosInMilli == finite.nano
		case _ => false
	}
	@inline def ==(that :PosixTime) :Boolean = epochMilli == that.epochMilli

	@inline def ==(that :Timestamp) :Boolean =
		epochMilli == that.toJava.toEpochMilli && that.toJava.getNano % NanosInMilli == 0

	@inline def ==(that :ZoneDateTime) :Boolean =
		epochMilli == that.epochMilli && that.toJava.getNano % NanosInMilli == 0

	@inline def ==(that :UTCDateTime) :Boolean =
		epochMilli == that.epochMilli && that.toJava.getNano % NanosInMilli == 0


	override def toString :String = toUTC.toString

}






@SerialVersionUID(Ver)
case object PosixTime {
	@inline def apply(epochMillis :Milliseconds) :PosixTime = new PosixTime(epochMillis.toMillis)


	@inline def apply(time :Time = Time.Local) :PosixTime = new PosixTime(time.clock.millis)

	@inline def now(implicit time :Time = Time.Local) :PosixTime = new PosixTime(time.clock.millis)

	@inline def after(interval :Milliseconds)(implicit time :Time = Time.Local) :PosixTime =
		new PosixTime(time.clock.millis + interval.toMillis)

	@inline def before(interval :Milliseconds)(implicit time :Time = Time.Local) :PosixTime =
		new PosixTime(time.clock.millis - interval.toMillis)


	def unapply(time :TimePoint) :Option[Long] = time match {
		case t :PosixTime => Some(t.epochMilli)
		case _ => None
	}


	@inline implicit def PosixTimeToJavaInstant(time :PosixTime)   :j.Instant = time.toInstant
	@inline implicit def PosixTimeFromJavaInstant(time :j.Instant) :PosixTime = new PosixTime(time.toEpochMilli)
	@inline implicit def PosixTimeToTimestamp(time :PosixTime)     :Timestamp = time.toTimestamp
//	@inline implicit def fromTimestamp(time :Timestamp) :UnixTime = new UnixTime(time.toEpochMilli)

	final val Max = new PosixTime(Long.MaxValue)
	final val Min = new PosixTime(Long.MinValue)
}
