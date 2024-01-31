package net.noresttherein.sugar.time

import java.{time => j}

import scala.concurrent.{duration => s}

import net.noresttherein.sugar.time.constants.{MaxNanoDuration, MicrosInMilli, MillisInDay, MillisInHour, MillisInMinute, MillisInSecond, NanosInHour, NanosInMilli, NanosInMinute, NanosInSecond}
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}



/** A lightweight value class wrapping a `Long` value of milliseconds elapsed since 1970-01-01 UTC.
  * All arithmetic operations return `Milliseconds` if possible; this means some methods might throw
  * an `ArithmeticException` where `this.toDuration` would not due to the larger range.
  * You can import `net.noresttherein.sugar.time.dsl._` to add factory methods to `Int` and `Long`
  * allowing to write `42.millis`.
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
class Milliseconds(override val toMillis :Long) extends AnyVal with TimeSpan with Serializable {

	@inline def toLong :Long = toMillis

	@inline override def asMillis :Milliseconds = this

	override def toNanos :Long =
		if (toMillis > MaxNanoDuration || toMillis < -MaxNanoDuration)
			overflow(toString, "toNanos")
		else toMillis * NanosInMilli

	override def toMicros :Long =
		if (if (toMillis >= 0) toMillis <= Long.MaxValue / MicrosInMilli else toMillis >= Long.MinValue / MicrosInMilli)
			toMillis * MicrosInMilli
		else
			overflow(toString, "toMicros")

	@inline override def toSeconds :Long = toMillis / MillisInSecond
	@inline override def toMinutes :Long = toMillis / MillisInMinute
	@inline override def toHours   :Long = toMillis / MillisInHour
	@inline override def toDays    :Long = toMillis / MillisInDay

	@inline override def inNanos   :Double = toMillis.toDouble * NanosInMilli
	@inline override def inMicros  :Double = toMillis.toDouble * MicrosInMilli
	@inline override def inMillis  :Double = toMillis.toDouble
	@inline override def inSeconds :Double = toMillis.toDouble / MillisInSecond
	@inline override def inMinutes :Double = toMillis.toDouble / MillisInMinute
	@inline override def inHours   :Double = toMillis.toDouble / MillisInHour
	@inline override def inDays    :Double = toMillis.toDouble / MillisInDay

	@inline override def toMilliseconds :Milliseconds = this
	@inline override def toDuration     :Duration     = new Duration(j.Duration.ofMillis(toMillis))
	@inline override def toJava         :j.Duration   = j.Duration.ofMillis(toMillis)

	override def toScala :s.Duration =
		if (toMillis > MaxNanoDuration || toMillis < -MaxNanoDuration)
			overflow(toString, "toScala")
		else s.Duration(toMillis, DateTimeUnit.Millis)


	override def to(unit :TimeUnit) :Long = {
		val divisor = unit.inNanos
		if (divisor < NanosInMilli) {
			val multiplier = NanosInMilli / divisor
			val max = Long.MaxValue / multiplier
			if (toMillis > max || toMillis < -max)
				overflow(toString, " in ", unit.toString)
			else toMillis * multiplier
		} else
			  toMillis / (divisor / NanosInMilli)
	}

	override def in(unit :TimeUnit) :Double = toMillis / (unit.inNanos.toDouble / NanosInMilli)

	@inline override def nanos   :Int  = (toMillis % NanosInMilli).toInt
	@inline override def seconds :Int  = ((toMillis / MillisInSecond) % 60).toInt
	@inline override def minutes :Int  = ((toMillis / MillisInMinute) % 60).toInt
	@inline override def hours   :Long = toMillis / MillisInHour


	override def unit :TimeUnit =
		if (toMillis % NanosInMinute == toMillis)
			if (toMillis % NanosInHour == toMillis) DateTimeUnit.Hours
			else DateTimeUnit.Minutes
		else
			if (toMillis % NanosInSecond == toMillis) DateTimeUnit.Seconds
			else DateTimeUnit.Millis



	@inline override def isZero :Boolean = toMillis == 0L
	@inline override def signum :Int = toMillis.sign.toInt

	override def abs :Milliseconds =
		if (toMillis >= 0)
			this
		else
			if (toMillis == Long.MinValue)
				overflow(toString, "abs")
			else
				new Milliseconds(-toMillis)

	override def unary_- :Milliseconds =
		if (toMillis == Long.MinValue)
			overflow(toString, "-")
		else
			new Milliseconds(-toMillis)



	def +(millis :Milliseconds) :Milliseconds =
		if (toMillis > 0 && millis.toMillis > Long.MaxValue - toMillis
			|| toMillis < 0 && millis.toMillis < Long.MinValue - toMillis
		)
			overflow(toString," + ", millis.toString)
		else
			new Milliseconds(toMillis + millis.toMillis)

	def -(millis :Milliseconds) :Milliseconds =
		if (toMillis > 0 && millis.toMillis < toMillis - Long.MaxValue
			|| toMillis < 0 && millis.toMillis > toMillis - Long.MinValue
		)
			overflow(toString," - ", toMillis.toString)
		else
			new Milliseconds(toMillis - millis.toMillis)

	def /(millis :Milliseconds) :Double = {
		val max =
			if (millis.toMillis > 0) Long.MaxValue / millis.toMillis
			else -Long.MaxValue / millis.toMillis
		if (toMillis > max || toMillis < -max)
			overflow(toString, " / ", millis.toString)
		toMillis.toDouble / millis.toMillis
	}

	def /~(millis :Milliseconds) :Long = {
		val max =
			if (millis.toMillis > 0) Long.MaxValue / millis.toMillis
			else -Long.MaxValue / millis.toMillis
		if (toMillis > max || toMillis < -max)
			overflow(toString," /~ ", millis.toString)
		toMillis / millis.toMillis
	}
	@inline def quot(millis :Milliseconds) :Long = this /~ millis

	override def +(time :TimeSpan) :TimeSpan = time match {
		case _ if toMillis == 0 => time
		case _ if time.signum == 0 => this
		case millis :Milliseconds =>
			if (toMillis > 0 && millis.toMillis <= Long.MaxValue - toMillis
				|| toMillis < 0 && millis.toMillis >= Long.MinValue - toMillis)
				new Milliseconds(toMillis + millis.toMillis)
			else {
				val seconds = toMillis / 1000L + millis.toMillis / 1000L
				val nanos = ((toMillis % 1000L + millis.toMillis % 1000L) * NanosInMilli).toInt
				new Duration(j.Duration.ofSeconds(seconds, nanos))
			}
		case _ => time.add(toMillis / 1000L, (toMillis % 1000L * NanosInMilli).toInt)
	}

	override def -(time :TimeSpan) :TimeSpan = time match {
		case _ if time.signum == 0 => this
		case millis :Milliseconds =>
			if (toMillis > 0 && millis.toMillis >= toMillis - Long.MaxValue
				|| toMillis < 0 && millis.toMillis <= toMillis - Long.MinValue)
				new Milliseconds(toMillis - time.toMillis)
			else {
				val seconds = toMillis / 1000L - millis.toMillis / 1000L
				val nanos = ((toMillis % 1000L - millis.toMillis % 1000L) * NanosInMilli).toInt
				new Duration(j.Duration.ofSeconds(seconds, nanos))
			}
		case _ => time.subtractFrom(toMillis / 1000L, (toMillis % 1000L * NanosInMilli).toInt)
	}


	override def +(time :TimeInterval) :TimeInterval = time match {
		case finite :TimeSpan => this + finite
		case _ => time.add(toMillis / 1000L, (toMillis % 1000L * NanosInMilli).toInt)
	}

	override def -(time :TimeInterval) :TimeInterval = time match {
		case finite :TimeSpan => this - finite
		case _ => time.subtractFrom(toMillis / 1000L, (toMillis % 1000L * NanosInMilli).toInt)
	}


	override def *(n :Int) :Milliseconds = {
		val max = if (n > 0) Long.MaxValue / n else -Long.MaxValue / n
		if (toMillis > max || toMillis < -max)
			overflow(toString" * ", n.toString)
		new Milliseconds(toMillis * n)
	}

	override def *(n :Long) :TimeSpan =
		if (toMillis == 0)
			this
		else if (n == 0)
			Milliseconds.ZeroRef.asInstanceOf[Milliseconds]
		else {
			val max = if (n > 0) Long.MaxValue / n else -Long.MaxValue / n
			if (toMillis > 0)
				if (max >= toMillis)
					new Milliseconds(toMillis * n)
				else if (max >= toMillis / 1000L)
					new Duration(j.Duration.ofSeconds(toMillis / 1000L * n, toMillis % 1000L * n * NanosInMilli))
				else
					overflow(toString" * ", n.toString)
			else
				if (-max <= toMillis)
					new Milliseconds(toMillis * n)
				else if (-max <= toMillis / 1000L)
					new Duration(j.Duration.ofSeconds(toMillis / 1000L * n, toMillis % 1000L * n * NanosInMilli))
				else
					overflow(toString" * ", n.toString)
		}

	@inline override def *(n :Double) :TimeInterval =
		if (toMillis == 0L) this
		else if (n == 0d) Milliseconds.ZeroRef.asInstanceOf[Milliseconds]
		else spanOfMillis(toMillis * n)


	@inline override def /(n :Long) :Milliseconds =
		if (n == 0L) divZero(toString)
		else if (toMillis == 0L) this
		else new Milliseconds(toMillis / n)

	@inline override def /(n :Double) :TimeInterval =
		if (n == 0) divZero(toString)
		else if (toMillis == 0) this
		else spanOfMillis(toMillis / n)


	private[sugar] def spanOfMillis(millis :Double) :TimeInterval =
		if (millis.isInfinite)
			if (millis > 0) Eternity
			else MinusEternity
		else {
			val int = millis.toLong
			if (int.toDouble == millis)
				new Milliseconds(int)
			else
				new Duration(j.Duration.ofSeconds(
					(millis / 1000d).toLong, ((millis - seconds * 1000d) * NanosInMilli).toInt
				))
		}


	@inline override def /(span :TimeInterval) :Double = toMillis / span.inMillis

	@inline override def /(unit :TimeUnit) :Double = toMillis.toDouble * NanosInMilli / unit.inNanos

	@inline override def %(unit :TimeUnit) :Milliseconds = {
		val divisor = unit.inNanos
		new Milliseconds(if (divisor <= NanosInMilli) 0 else toMillis % (divisor / NanosInMilli))
	}


	@inline override def add(seconds :Long, nanos :Int) :TimeSpan =
		new Duration(j.Duration.ofSeconds(seconds + toMillis / 1000L, nanos + (toMillis % 1000L).toInt))

	@inline override def subtractFrom(seconds :Long, nanos :Int) :TimeSpan =
		new Duration(j.Duration.ofSeconds(seconds - toMillis / 1000L, nanos - (toMillis % 1000L).toInt))


	@inline def compare(that :Milliseconds) :Int =
		if (toMillis < that.toMillis) -1
		else if (toMillis > that.toMillis) 1
		else 0

	@inline def <=(that :Milliseconds) :Boolean = toMillis <= that.toMillis
	@inline def < (that :Milliseconds) :Boolean = toMillis < that.toMillis
	@inline def >=(that :Milliseconds) :Boolean = toMillis >= that.toMillis
	@inline def > (that :Milliseconds) :Boolean = toMillis > that.toMillis

	def <=(that :Duration) :Boolean =
		lte(toMillis / MillisInSecond, (toMillis % NanosInMilli).toInt, that.toJava.getSeconds, that.toJava.getNano)

	def < (that :Duration) :Boolean =
		lt(toMillis / MillisInSecond, (toMillis % NanosInMilli).toInt, that.toJava.getSeconds, that.toJava.getNano)

	def >=(that :Duration) :Boolean =
		gte(toMillis / MillisInSecond, (toMillis % NanosInMilli).toInt, that.toJava.getSeconds, that.toJava.getNano)

	def > (that :Duration) :Boolean =
		gt(toMillis / MillisInSecond, (toMillis % NanosInMilli).toInt, that.toJava.getSeconds, that.toJava.getNano)

	@inline def min(that :Milliseconds) :Milliseconds = if (toMillis <= that.toMillis) this else that
	@inline def max(that :Milliseconds) :Milliseconds = if (toMillis >= that.toMillis) this else that

	@inline def ==(that :Milliseconds) :Boolean = toMillis == that.toMillis

	@inline def ==(that :Duration) :Boolean =
		toMillis / MillisInSecond == that.toJava.getSeconds && (toMillis % NanosInMilli).toInt == that.toJava.getNano

	override def toString :String = {
		val u = unit
		(toMillis / (unit.inNanos / 1000)).toString + u.symbol
	}

}






@SerialVersionUID(Ver)
case object Milliseconds {
	@inline def apply(millis :Long) :Milliseconds = new Milliseconds(millis)

	@inline def unapply(span :TimeInterval) :Maybe[Long] = span match {
		case millis :Milliseconds => Yes(millis.toMillis)
		case _                    => No
	}

	def between(start :PosixTime, until :PosixTime) :Milliseconds =
		if (if (start.epochMilli > 0) until.epochMilli > Long.MinValue + start.epochMilli
		    else until.epochMilli < Long.MaxValue + start.epochMilli)
			overflow(until.toString, " - ", start.toString)
		else
			new Milliseconds(until.epochMilli - start.epochMilli)


	def since(moment :PosixTime)(implicit time :Time = Time.Local) :Milliseconds = {
		val now = time.clock.millis
		if (moment.epochMilli < now - Long.MaxValue)
			overflow(new PosixTime(now).toString, " - ", moment.toString)
		else
	        new Milliseconds(now - moment.epochMilli)
	}

	def until(moment :PosixTime)(implicit time :Time = Time.Local) :Milliseconds = {
		val now = time.clock.millis
		if (moment.epochMilli < Long.MinValue + now)
			overflow(moment.toString, " - ", new PosixTime(now).toString)
		else
			new Milliseconds(moment.epochMilli - now)
	}


	private[time] final val ZeroRef :Any = new Milliseconds(0L)
	final val Zero = new Milliseconds(0L)
	final val Max  = new Milliseconds(Long.MaxValue)
	final val Min  = new Milliseconds(Long.MinValue)

}
