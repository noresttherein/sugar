package net.noresttherein.slang.time

import java.{time => j}
import scala.concurrent.duration.{Deadline, FiniteDuration}


/** A time point defined in terms of the ''epoch milli'', that is the number of milliseconds elapsed since
  * 1970-01-01 UTC. This is the most lightweight `TimePoint` implementation, being a simple value class
  * wrapping a `Long` value. All arithmetic operations will return another `UnixTime` or `Milliseconds` instance
  * if possible.
  * @author Marcin Mościcki marcin@moscicki.net
  */
@SerialVersionUID(1L)
class UnixTime(val epochMilli :Long) extends AnyVal with DefiniteTime with Serializable {

	@inline override def apply(cycle :Cycle) :cycle.Phase = cycle.of(this)

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

	@inline override def toUnix :UnixTime = this

	@inline override def toTimestamp :Timestamp = j.Instant.ofEpochMilli(epochMilli)

	@inline override def toUTC :UTCDateTime =
		new UTCDateTime(j.LocalDateTime.ofEpochSecond(epochMilli / 1000, (epochMilli % 1000).toInt, j.ZoneOffset.UTC))

	@inline override def toInstant :j.Instant = j.Instant.ofEpochMilli(epochMilli)

	@inline override def toJava :j.Instant = j.Instant.ofEpochMilli(epochMilli)

	@inline override def toDeadline :Deadline = Deadline(FiniteDuration(epochMilli, TimeUnit.Millis))

	@inline override def in(zone :TimeZone) :ZoneDateTime =
		new ZoneDateTime(j.ZonedDateTime.ofInstant(toInstant, zone.toJava))

	@inline override def at(offset :TimeOffset) :ZoneDateTime =
		new ZoneDateTime(j.ZonedDateTime.ofInstant(toInstant, offset.toJava))



	override def +(time :TimeSpan) :TimePoint = time match {
		case millis :Milliseconds if (if (epochMilli > 0) millis.inMillis <= Long.MaxValue - epochMilli
		                              else millis.inMillis >= Long.MinValue - epochMilli) =>
			new UnixTime(epochMilli + millis.inMillis)
		case finite :FiniteTimeSpan => this + finite
		case Eternity => EndOfTime
		case MinusEternity => DawnOfTime
	}

	override def -(time :TimeSpan) :TimePoint = time match {
		case millis :Milliseconds if (if (epochMilli > 0) millis.inMillis >= epochMilli - Long.MaxValue
		                              else millis.inMillis <= epochMilli - Long.MinValue) =>
			new UnixTime(epochMilli - millis.inMillis)
		case finite :FiniteTimeSpan => this - finite
		case Eternity => DawnOfTime
		case MinusEternity => EndOfTime
	}


	@inline override def +(time :FiniteTimeSpan) :Timestamp = plus(time.seconds, time.nanos)
	@inline override def -(time :FiniteTimeSpan) :Timestamp = minus(time.seconds, time.nanos)

	@inline override def +(time :Duration) :Timestamp = plus(time.toJava.getSeconds, time.toJava.getNano)
	@inline override def -(time :Duration) :Timestamp = minus(time.toJava.getSeconds, time.toJava.getNano)

	private[slang] def plus(seconds :Long, nanos :Int) :Timestamp = {
		val s1 = epochMilli / 1000L
		if (if (s1 > 0) seconds > Long.MaxValue - s1 else seconds < Long.MinValue - s1)
			overflow(toString, " + ", s"${seconds}s ${nanos}ms")
		new Timestamp(j.Instant.ofEpochSecond(s1 + seconds, epochMilli % 1000 * NanosInMilli + nanos))
	}

	private[slang] def minus(seconds :Long, nanos :Int) :Timestamp = {
		val s1 = epochMilli / 1000L
		if (if (s1 > 0) seconds < s1 - Long.MaxValue else seconds > s1 - Long.MinValue)
			overflow(toString, " - ", s"${seconds}s ${nanos}ns")
		new Timestamp(j.Instant.ofEpochSecond(epochMilli / 1000 - seconds, epochMilli % 1000 * NanosInMilli - nanos))
	}


	def +(millis :Milliseconds) :UnixTime =
		if (if (epochMilli > 0) millis.inMillis > Long.MaxValue - epochMilli
		    else millis.inMillis < Long.MinValue - epochMilli
		)
			overflow(toString, " + ", millis.toString)
		else new UnixTime(epochMilli + millis.inMillis)

	def -(millis :Milliseconds) :UnixTime =
		if (if (epochMilli > 0) millis.inMillis < epochMilli - Long.MaxValue
		    else millis.inMillis > epochMilli - Long.MinValue
		)
			overflow(toString, " - ", millis.toString)
		else new UnixTime(epochMilli - millis.inMillis)



	override def -(time :TimePoint) :TimeSpan = time match {
		case other :UnixTime if (if (epochMilli > 0) other.epochMilli < epochMilli - Long.MaxValue
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

	def minus(other :UnixTime) :Milliseconds =
		if (if (epochMilli > 0) other.epochMilli < epochMilli - Long.MaxValue
		    else other.epochMilli > epochMilli - Long.MinValue
		)
			overflow(toString, " - ", other.toString)
		else new Milliseconds(epochMilli - other.epochMilli)



	override def compare(that :TimePoint) :Int = that match {
		case other :UnixTime =>
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


	@inline def compare(that :UnixTime) :Int =
		if (epochMilli < that.epochMilli) -1
		else if (epochMilli > that.epochMilli) 1
		else 0

	@inline def <=(that :UnixTime) :Boolean = epochMilli <= that.epochMilli
	@inline def < (that :UnixTime) :Boolean = epochMilli < that.epochMilli
	@inline def >=(that :UnixTime) :Boolean = epochMilli >= that.epochMilli
	@inline def > (that :UnixTime) :Boolean = epochMilli > that.epochMilli
	@inline def min(that :UnixTime) :UnixTime = if (epochMilli <= that.epochMilli) this else that
	@inline def max(that :UnixTime) :UnixTime = if (epochMilli >= that.epochMilli) this else that

	override def ===(that :TimePoint) :Boolean = that match {
		case finite :DefiniteTime =>
			epochMilli / 1000 == finite.epochSecond && epochMilli % 1000 * NanosInMilli == finite.nano
		case _ => false
	}
	@inline def ===(that :UnixTime) :Boolean = epochMilli == that.epochMilli


	override def toString :String = toUTC.toString

}






object UnixTime {
	@inline def apply(epochMillis :Milliseconds) :UnixTime = new UnixTime(epochMillis.inMillis)


	@inline def apply()(implicit time :Time = Time.Local) :UnixTime = new UnixTime(time.clock.millis)

	@inline def now(implicit time :Time = Time.Local) :UnixTime = new UnixTime(time.clock.millis)

	@inline def after(lapse :Milliseconds)(implicit time :Time = Time.Local) :UnixTime =
		new UnixTime(time.clock.millis + lapse.inMillis)

	@inline def before(lapse :Milliseconds)(implicit time :Time = Time.Local) :UnixTime =
		new UnixTime(time.clock.millis - lapse.inMillis)


	def unapply(time :TimePoint) :Option[Long] = time match {
		case t :UnixTime => Some(t.epochMilli)
		case _ => None
	}



	@inline implicit def toJavaInstant(time :UnixTime) :j.Instant = time.toInstant
	@inline implicit def fromJavaInstant(time :j.Instant) :UnixTime = new UnixTime(time.toEpochMilli)

	final val Max = new UnixTime(Long.MaxValue)
	final val Min = new UnixTime(Long.MinValue)
}
