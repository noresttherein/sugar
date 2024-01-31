package net.noresttherein.sugar.time

import java.{time => j}
import java.util.concurrent.{TimeUnit => JTimeUnit}

import scala.concurrent.{duration => s}

import net.noresttherein.sugar.exceptions.SugaredArithmeticException
import net.noresttherein.sugar.time.constants.{NanosInDay, NanosInHour, NanosInMicro, NanosInMilli, NanosInMinute, NanosInSecond}
import net.noresttherein.sugar.time.dsl.LongTimeLapseMethods
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}







/** Implementation of a finite duration backed by a `java.time.Duration`. All methods return a `Duration` if possible. */
@SerialVersionUID(Ver)
class Duration private[time] (override val toJava: j.Duration) extends AnyVal with TimeSpan with Serializable {
	@inline override def toNanos :Long   = toJava.toNanos
	@inline override def inNanos :Double =
		toJava.toNanosPart.toDouble + toJava.toSeconds.toDouble * NanosInSecond

	override def toMicros :Long = {
		val seconds = toJava.getSeconds
		val max = Long.MaxValue / 1000000L
		if (seconds == 0L)
			toJava.getNano / 1000L
		else if (seconds > max || seconds < -max)
			overflow(toString, "toMicros")
		else
			seconds * 1000000L + toJava.getNano / 1000L
	}
	@inline override def inMicros :Double = toJava.getSeconds * 1000000d + toJava.getNano / 1000d

	@inline override def toMillis :Long    = toJava.toMillis
	@inline override def inMillis :Double  = toJava.getSeconds * 1000d + toJava.getNano.toDouble * NanosInMilli
	@inline override def asMillis :Milliseconds = new Milliseconds(toJava.toMillis)

	@inline override def toSeconds :Long   = toJava.toSeconds
	@inline override def inSeconds :Double = toJava.toSeconds + toJava.toNanos / NanosInSecond.toDouble
	@inline override def toMinutes :Long   = toJava.toMinutes
	@inline override def inMinutes :Double = toJava.toSeconds / 60d + toJava.toNanos / NanosInMinute.toDouble
	@inline override def toHours   :Long   = toJava.toHours
	@inline override def inHours   :Double = toJava.toSeconds / 3600d + toJava.toNanos / NanosInHour.toDouble
	@inline override def toDays    :Long   = toJava.toDays
	@inline override def inDays    :Double = toJava.toSeconds / 86400d + toJava.toNanos / NanosInDay.toDouble

	override def to(unit :TimeUnit) :Long = {
		val nanoLen = unit.inNanos
		val seconds = toJava.getSeconds; val nanos = toJava.getNano
		if (nanoLen < NanosInSecond) {
			val multiplier = NanosInSecond / nanoLen
			val nanoPart = nanos / nanoLen
			val max = (Long.MaxValue - nanoPart) / multiplier
			if (seconds > max || seconds < -max)
				overflow(toString," in ", unit.toString)
			else
				seconds * multiplier + nanoPart
		} else
			seconds / (nanoLen / NanosInSecond)
	}

	@inline override def in(unit :TimeUnit) :Double = {
		val nanoLen = unit.inNanos
		if (nanoLen < NanosInSecond)
			toJava.getSeconds.toDouble * (NanosInSecond / nanoLen) + toJava.getNano.toDouble / nanoLen
		else
			toJava.getSeconds.toDouble / (nanoLen / NanosInSecond) + toJava.getNano.toDouble / nanoLen
	}

	@inline override def nanos   :Int  = toJava.toNanosPart
	@inline override def seconds :Int  = toJava.toSecondsPart
	@inline override def minutes :Int  = toJava.toNanosPart
	@inline override def hours   :Long = toJava.toHours

	override def unit :TimeUnit = (toJava.toSeconds, toJava.getNano) match {
		case (s, 0) =>
			if (s % NanosInMinute == s)
				if (s % NanosInHour == s) DateTimeUnit.Hours
				else DateTimeUnit.Minutes
			else DateTimeUnit.Seconds
		case (_, n) =>
			if (n % NanosInMicro == n)
				if (n % NanosInMilli == n) DateTimeUnit.Millis
				else DateTimeUnit.Micros
			else DateTimeUnit.Nanos
	}


	@inline override def toMilliseconds :Milliseconds = new Milliseconds(toMillis)
	@inline override def toDuration     :Duration     = this

	@inline override def toScala         :s.Duration   =
		s.Duration(toJava.getSeconds * NanosInSecond + toJava.getNano, JTimeUnit.NANOSECONDS)

	@inline override def isZero :Boolean = toJava.isZero

	override def signum :Int = {
		val seconds = toJava.getSeconds
		if (seconds > 0) 1
		else if (seconds < 0) -1
		else java.lang.Integer.signum(toJava.getNano)
	}

	override def abs :Duration =
		if (signum >= 0) this
		else {
			val seconds = toJava.getSeconds
			if (seconds == Long.MinValue)
				overflow(toString, "abs")
			new Duration(j.Duration.ofSeconds(-toJava.getSeconds, -toJava.getNano))
		}

	override def unary_- :Duration = {
		val seconds = toJava.getSeconds
		val nanos = toJava.getNano
		if (seconds == 0L && nanos == 0)
			this
		else if (seconds == Long.MinValue)
			overflow(toString, "-")
		else
			new Duration(j.Duration.ofSeconds(-seconds, -nanos))
	}


	override def +(time :TimeInterval) :TimeInterval = {
		val s = toJava.getSeconds; val n = toJava.getNano
		if (s == 0 && n == 0) time
		else if (time.signum == 0) this
		else time.add(s, n)
	}
	@inline override def +(time :TimeSpan) :TimeSpan = time.add(time.toSeconds, time.nanos)
	@inline def +(time :Duration)                :Duration       = new Duration(toJava plus time.toJava)

	override def add(seconds :Long, nanos :Int) :Duration = {
		val s = toJava.getSeconds
		if (if (s > 0) seconds > Long.MaxValue - s else s < Long.MinValue - s)
			overflow(s"(${seconds}s ${nanos}ns)", " + ", toString)
		new Duration(j.Duration.ofSeconds(s + seconds, this.nanos + nanos))
	}


	@inline override def -(time :TimeInterval) :TimeInterval =
		if (time.signum == 0) this
		else time.subtractFrom(toJava.getSeconds, toJava.getNano)

	@inline override def -(time :TimeSpan) :TimeSpan = time.subtractFrom(toJava.getSeconds, toJava.getNano)
	@inline def -(time :Duration)                :Duration       = new Duration(toJava minus time.toJava)

	override def subtractFrom(seconds :Long, nanos :Int) :Duration = {
		val s = toJava.getSeconds
		if (if (s > 0) seconds < Long.MinValue + s else seconds > Long.MaxValue + s)
			overflow(s"(${seconds}s ${nanos}ns)", " - ", toString)
		new Duration(j.Duration.ofSeconds(seconds -s, nanos - this.nanos))
	}


	@inline override def /(time :TimeInterval) :Double =
		if (time.isInfinite) time.signum.toDouble
		else divideBy(time.toSeconds, time.nanos)

	@inline def /(time :Duration) :Double = divideBy(time.getSeconds, time.getNano)

	@inline def /~(time :Duration)   :Long = toJava dividedBy time.toJava
	@inline def quot(time :Duration) :Long = toJava dividedBy time.toJava

	private[sugar] def divideBy(seconds :Long, nano :Int) :Double = {
		val s1 = toJava.getSeconds;	val n1 = toJava.getNano
		if (seconds == 0 && nano == 0)
			throw SugaredArithmeticException(s"($this) / 0")
		else if (s1 == 0 && n1 == 0)
			0d
		else
			((BigDecimal(s1) * NanosInSecond + n1) / (BigDecimal(seconds) * NanosInSecond + nano)).toDouble
	}

	@inline override def /(d :Long) :Duration = new Duration(toJava dividedBy d)

	override def /(d :Double) :Duration =
		if (d == 0d)
			throw SugaredArithmeticException(s"($this) / 0")
		else {
			val length = (BigDecimal(toJava.getSeconds) * NanosInSecond + toJava.getNano) / d
			val seconds = length / NanosInSecond
			if (!seconds.isValidLong)
				throw SugaredArithmeticException(s"Long overflow: $this / $d")
			new Duration(j.Duration.ofSeconds(seconds.toLong, (length % NanosInSecond).toInt))
		}


	@inline override def *(d :Long) :Duration =
		if (d == 0) Duration.Zero
		else new Duration(toJava.multipliedBy(d))

	override def *(d :Double) :Duration =
		if (d == 0d) Duration.Zero
		else (toJava.getSeconds, toJava.getNano) match {
			case (0L, 0) => this
			case (s, n) => new Duration(j.Duration.ofSeconds((s / d).toLong, (n / d).toInt))
		}

	@inline override def /(unit :TimeUnit) :Double = {
		val len = unit.inNanos
		divideBy(len / NanosInSecond, (len % NanosInSecond).toInt)
	}

	override def %(unit :TimeUnit) :Duration = {
		val duration = unit.inNanos
		if (duration <= NanosInSecond)
			new Duration(j.Duration.ofSeconds(0, toJava.getNano % duration))
		else
			new Duration(j.Duration.ofSeconds(toJava.getSeconds % (duration / NanosInSecond), toJava.getNano))
	}


	def compare(that :Duration) :Int = toJava compareTo that.toJava

	def <=(that :Duration) :Boolean =
		lte(toJava.getSeconds, toJava.getNano, that.toJava.getSeconds, that.toJava.getNano)

	def < (that :Duration) :Boolean =
		lt(toJava.getSeconds, toJava.getNano, that.toJava.getSeconds, that.toJava.getNano)

	def >=(that :Duration) :Boolean =
		gte(toJava.getSeconds, toJava.getNano, that.toJava.getSeconds, that.toJava.getNano)

	def > (that :Duration) :Boolean =
		gt(toJava.getSeconds, toJava.getNano, that.toJava.getSeconds, that.toJava.getNano)

	def <=(that :Milliseconds) :Boolean =
		lte(toJava.getSeconds, toJava.getNano, that.toSeconds, that.nanos)

	def < (that :Milliseconds) :Boolean =
		lt(toJava.getSeconds, toJava.getNano, that.toSeconds, that.nanos)

	def >=(that :Milliseconds) :Boolean =
		gte(toJava.getSeconds, toJava.getNano, that.toSeconds, that.nanos)

	def > (that :Milliseconds) :Boolean =
		gt(toJava.getSeconds, toJava.getNano, that.toSeconds, that.nanos)


	@inline def min(that :Duration) :Duration = if (this <= that) this else that
	@inline def max(that :Duration) :Duration = if (this >= that) this else that

	@inline def ==(that :Duration) :Boolean = toJava == that.toJava

	override def toString :String = toJava.toString
}






@SerialVersionUID(Ver)
case object Duration {

	@inline def apply(duration :j.Duration)         :Duration = new Duration(duration)
	@inline def apply(length :Long, unit :TimeUnit) :Duration = j.Duration.of(length, unit.toJava)
	@inline def apply(seconds :Long)                :Duration = new Duration(j.Duration.ofSeconds(seconds))
	@inline def apply(seconds :Long, nanos :Int)    :Duration = new Duration(j.Duration.ofSeconds(seconds, nanos))


	@inline def unapply(span :TimeInterval) :Maybe[(Long, Int)] = span match {
		case d :Duration => Yes(d.toJava.getSeconds, d.toJava.getNano)
		case _           => No
	}


	@inline def between(from :Timestamp, until :Timestamp) :Duration =
		new Duration(j.Duration.between(from.toJava, until.toJava))

	@inline def between(from :DefiniteTime, until :DefiniteTime) :Duration =
		new Duration(j.Duration.between(from.toInstant, until.toInstant))

	@inline def between(from :TimeOfDay, until :TimeOfDay) :Duration =
		new Duration(j.Duration.between(from.toJava, until.toJava))

	@inline def between(from :DateTime, until :DateTime) :Duration =
		new Duration(j.Duration.between(from.toJava, until.toJava))


	@inline def since(moment :DefiniteTime)(implicit time :Time = Time.Local) :Duration =
		new Duration(j.Duration.between(moment.toInstant, time.clock.instant))

	@inline def until(moment :DefiniteTime)(implicit time :Time = Time.Local) :Duration =
		new Duration(j.Duration.between(time.clock.instant, moment.toInstant))

	@inline implicit def DurationFromJavaDuration(duration :j.Duration) :Duration = new Duration(duration)
	@inline implicit def DurationToJavaDuration(duration :Duration)     :j.Duration = duration.toJava


	final val Zero = new Duration(j.Duration.ZERO)
	final val Max  = new Duration(j.Duration.ofSeconds(Long.MaxValue, 0))
	final val Min  = new Duration(j.Duration.ofSeconds(Long.MinValue, 0))
}



