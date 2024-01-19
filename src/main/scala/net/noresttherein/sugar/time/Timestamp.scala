package net.noresttherein.sugar.time

import java.{time => j}

import scala.concurrent.duration.{Deadline, FiniteDuration}

import net.noresttherein.sugar.time.constants.{NanosInMilli, NanosInSecond}


/** A unique point in time specified with nanosecond precision, consisting of a 64-bit signed second part and a 32-bit
  * non-negative nanosecond part of the second. This is a lightweight value type wrapping a `java.time.Instant`,
  * carrying no date/time zone information.
  * @author Marcin Mościcki
  */ //consider: renaming to Instant
@SerialVersionUID(Ver)
class Timestamp private[time] (override val toJava :j.Instant) extends AnyVal with DefiniteTime with Serializable {

	@inline override def apply(cycle :Cycle) :cycle.Phase = cycle.on(this)

	@inline override def nano        :Int         = toJava.getNano
	@inline override def epochSecond :Long        = toJava.getEpochSecond
	@inline override def epochMilli  :Long        = toJava.toEpochMilli
	@inline override def toPosix      :PosixTime    = new PosixTime(toJava.toEpochMilli)
	@inline override def toTimestamp :Timestamp   = this
	@inline override def toUTC       :UTCDateTime = new UTCDateTime(j.LocalDateTime.ofInstant(toJava, j.ZoneOffset.UTC))
	@inline override def toInstant   :j.Instant   = toJava

	override def toDeadline :Deadline = {
		val s = toJava.getEpochSecond; val n = toJava.getNano
		if (if (n > 0) s > (Long.MaxValue - n) / NanosInSecond else s < (Long.MinValue - n) / NanosInSecond)
			overflow(toString, "toDeadline")
		Deadline(FiniteDuration(s * NanosInSecond + n, TimeUnit.Nanos))
	}

	@inline override def in(zone :TimeZone)     :ZoneDateTime = toJava.atZone(zone.toJava)
	@inline override def at(offset :TimeOffset) :ZoneDateTime = toJava.atOffset(offset.toJava)


	override def +(time :TimeInterval) :TimePoint = time match {
		case finite :TimeSpan => this + finite
		case Eternity => EndOfTime
		case MinusEternity => DawnOfTime
	}
	override def +(time :TimeSpan) :Timestamp = {
		val s1 = toJava.getEpochSecond; val s2 = time.toSeconds
		if (if (s1 > 0) s2 > Long.MaxValue - s1 else s2 < Long.MinValue - s1)
			overflow(toString, " + ", time)
		new Timestamp(j.Instant.ofEpochSecond(s1 + s2, nano + time.nanos))
	}
	@inline override def +(time :Duration) :Timestamp = new Timestamp(toJava plus time.toJava)


	override def -(time :TimeInterval) :TimePoint = time match {
		case finite :TimeSpan => this - finite
		case Eternity => DawnOfTime
		case MinusEternity => EndOfTime
	}
	override def -(time :TimeSpan) :Timestamp = {
		val s1 = toJava.getEpochSecond; val s2 = time.toSeconds
		if (if (s2 > 0) s1 < Long.MinValue + s2 else s1 > Long.MaxValue + s2)
			overflow(toString, " - ", time)
		new Timestamp(j.Instant.ofEpochSecond(s1 - s2, nano - time.nanos))
	}
	@inline override def -(time :Duration) :Timestamp = new Timestamp(toJava minus time.toJava)


	override def -(time :TimePoint) :TimeInterval = time match {
		case finite :DefiniteTime =>
			val s1 = toJava.getEpochSecond; val s2 = finite.epochSecond
			if (if (s2 > 0) s1 < Long.MinValue + s2 else s1 > Long.MaxValue + s2)
				overflow(toString, " - ", time)
			j.Duration.ofSeconds(epochSecond - time.epochSecond, nano - finite.nano)
		case DawnOfTime => Eternity
		case EndOfTime => MinusEternity
	}
	override def -(time :DefiniteTime) :Duration = {
		val s1 = toJava.getEpochSecond; val s2 = time.epochSecond
		if (if (s2 > 0) s1 < Long.MinValue + s2 else s1 > Long.MaxValue + s2)
			overflow(toString, " - ", time)
		j.Duration.ofSeconds(epochSecond - time.epochSecond, nano - time.nano)
	}
	@inline override def -(time :Timestamp) :Duration = new Duration(j.Duration.between(time.toJava, toJava))


	override def compare(that :TimePoint) :Int = that match {
		case time :Timestamp => toJava compareTo time.toJava
		case finite :DefiniteTime =>
			val s1 = toJava.getEpochSecond; val s2 = finite.epochSecond
			if (s1 < s2) -1
			else if (s1 > s2) 1
			else java.lang.Long.signum(nano.toLong - finite.nano)
		case DawnOfTime => 1
		case EndOfTime => -1
	}

	@inline def compare(that :Timestamp) :Int = toJava compareTo that.toJava

	def <=(that :Timestamp) :Boolean =
		lte(toJava.getEpochSecond, toJava.getNano, that.toJava.getEpochSecond, that.toJava.getNano)
	
	def < (that :Timestamp) :Boolean =
		lt(toJava.getEpochSecond, toJava.getNano, that.toJava.getEpochSecond, that.toJava.getNano)

	def >=(that :Timestamp) :Boolean = 
		gte(toJava.getEpochSecond, toJava.getNano, that.toJava.getEpochSecond, that.toJava.getNano)
	
	def > (that :Timestamp) :Boolean =
		gt(toJava.getEpochSecond, toJava.getNano, that.toJava.getEpochSecond, that.toJava.getNano)

	def <=(that :ZoneDateTime) :Boolean = 
		lte(toJava.getEpochSecond, toJava.getNano, that.toJava.toEpochSecond, that.toJava.getNano)
	
	def < (that :ZoneDateTime) :Boolean =
		lt(toJava.getEpochSecond, toJava.getNano, that.toJava.toEpochSecond, that.toJava.getNano)

	def >=(that :ZoneDateTime) :Boolean = 
		gte(toJava.getEpochSecond, toJava.getNano, that.toJava.toEpochSecond, that.toJava.getNano)
	
	def > (that :ZoneDateTime) :Boolean =
		gt(toJava.getEpochSecond, toJava.getNano, that.toJava.toEpochSecond, that.toJava.getNano)

	def <=(that :UTCDateTime) :Boolean = 
		lte(toJava.getEpochSecond, toJava.getNano, that.toJava.toEpochSecond(Time.UTC.offset), that.toJava.getNano)
	
	def < (that :UTCDateTime) :Boolean =
		lt(toJava.getEpochSecond, toJava.getNano, that.toJava.toEpochSecond(Time.UTC.offset), that.toJava.getNano)

	def >=(that :UTCDateTime) :Boolean = 
		gte(toJava.getEpochSecond, toJava.getNano, that.toJava.toEpochSecond(Time.UTC.offset), that.toJava.getNano)
	
	def > (that :UTCDateTime) :Boolean =
		gt(toJava.getEpochSecond, toJava.getNano, that.toJava.toEpochSecond(Time.UTC.offset), that.toJava.getNano)

	def <=(that :PosixTime) :Boolean =
		lte(toJava.getEpochSecond, toJava.getNano, that.epochSecond, that.nano)
	
	def < (that :PosixTime) :Boolean =
		lt(toJava.getEpochSecond, toJava.getNano, that.epochSecond, that.nano)

	def >=(that :PosixTime) :Boolean =
		gte(toJava.getEpochSecond, toJava.getNano, that.epochSecond, that.nano)
	
	def > (that :PosixTime) :Boolean =
		gt(toJava.getEpochSecond, toJava.getNano, that.epochSecond, that.nano)


	@inline def min(that :Timestamp) :Timestamp = if (this <= that) this else that
	@inline def max(that :Timestamp) :Timestamp = if (this >= that) this else that

	override def ==(that :TimePoint) :Boolean = that match {
		case finite :DefiniteTime => toJava.getEpochSecond == finite.epochSecond && toJava.getNano == finite.nano
		case _ => false
	}
	@inline def ==(that :Timestamp) :Boolean = toJava == that.toJava

	@inline def ==(that :ZoneDateTime) :Boolean =
		toJava.getEpochSecond == that.toJava.toEpochSecond && toJava.getNano == that.toJava.getNano

	@inline def ==(that :UTCDateTime) :Boolean =
		toJava.getEpochSecond == that.toJava.toEpochSecond(Time.UTC.offset) && toJava.getNano == that.toJava.getNano

	@inline def ==(that :PosixTime) :Boolean =
		toJava.toEpochMilli == that.epochMilli && toJava.getNano % NanosInMilli == 0L

	override def toString :String = toJava.toString
}






@SerialVersionUID(Ver)
case object Timestamp {

	@inline def apply(time :j.Instant) :Timestamp = new Timestamp(time)

	@inline def ofEpochMilli(millis :Long)   :Timestamp = new Timestamp(j.Instant.ofEpochMilli(millis))
	@inline def ofEpochSecond(seconds :Long) :Timestamp = new Timestamp(j.Instant.ofEpochSecond(seconds))

	@inline def ofEpochSecond(seconds :Long, nano :Long) :Timestamp =
		new Timestamp(j.Instant.ofEpochSecond(seconds, nano))


	@inline def after(duration :Duration)(implicit time :Time = Time.Local) :Timestamp =
		new Timestamp(j.Instant.now(time.clock) plus duration.toJava)

	@inline def before(duration :Duration)(implicit time :Time = Time.Local) :Timestamp =
		new Timestamp(j.Instant.now(time.clock) minus duration.toJava)


	@inline def apply(time :Time = Time.Local) :Timestamp = new Timestamp(j.Instant.now(time.clock))

	@inline def now(implicit time :Time = Time.Local) :Timestamp = new Timestamp(j.Instant.now(time.clock))


	@inline implicit def TimestampToJavaInstant(time :Timestamp) :j.Instant = time.toJava
	@inline implicit def TimestampFromJavaInstant(time :j.Instant) :Timestamp = new Timestamp(time)

	final val Max = new Timestamp(j.Instant.MAX)
	final val Min = new Timestamp(j.Instant.MIN)

}
