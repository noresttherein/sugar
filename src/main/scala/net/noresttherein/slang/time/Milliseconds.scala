package net.noresttherein.slang.time

import java.{time => j}
import scala.concurrent.{duration => s}



/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
class Milliseconds(override val inMillis :Long) extends AnyVal with FiniteTimeSpan with Serializable {

	@inline override def asMillis :Milliseconds = this

	@inline override def inNanos :Long =
		if (inMillis > MaxNanoDuration || inMillis < -MaxNanoDuration)
			overflow(toString, "inNanos")
		else inMillis * NanosInMilli

	@inline override def inMicros :Long = inMillis * MicrosInMilli
	@inline override def inSeconds :Long = inMillis / MillisInSecond
	@inline override def inMinutes :Long = inMillis / MillisInMinute
	@inline override def inHours :Long = inMillis / MillisInHour
	@inline override def inDays :Long = inMillis / MillisInDay

	@inline override def toMicros :Double = inMillis.toDouble * MicrosInMilli
	@inline override def toMillis :Double = inMillis.toDouble
	@inline override def toSeconds :Double = inMillis.toDouble / MillisInSecond
	@inline override def toMinutes :Double = inMillis.toDouble / MillisInMinute
	@inline override def toHours :Double = inMillis.toDouble / MillisInHour
	@inline override def toDays :Double = inMillis.toDouble / MillisInDay

	@inline override def toJava :j.Duration = j.Duration.ofMillis(inMillis)

	@inline override def toScala :s.Duration =
		if (inMillis > MaxNanoDuration || inMillis < -MaxNanoDuration)
			overflow(toString, "toScala")
		else s.Duration(inMillis, DateTimeUnit.Millis)



	override def in(unit :TimeUnit) :Long = {
		val divisor = unit.inNanos
		if (divisor < NanosInMilli) {
			val multiplier = NanosInMilli / divisor
			val max = Long.MaxValue / multiplier
			if (inMillis > max || inMillis < -max)
				overflow(toString, " in ", unit.toString)
			else inMillis * multiplier
		} else
			  inMillis / (divisor / NanosInMilli)
	}

	override def to(unit :TimeUnit) :Double = inMillis / (unit.inNanos.toDouble / NanosInMilli)

	@inline override def nanos :Int = (inMillis % NanosInMilli).toInt
	@inline override def seconds :Int = ((inMillis / MillisInSecond) % 60).toInt
	@inline override def minutes :Int = ((inMillis / MillisInMinute) % 60).toInt
	@inline override def hours :Long = inMillis / MillisInHour

	override def unit :TimeUnit =
		if (inMillis % NanosInMinute == inMillis)
			if (inMillis % NanosInHour == inMillis) DateTimeUnit.Hours
			else DateTimeUnit.Minutes
		else
			if (inMillis % NanosInSecond == inMillis) DateTimeUnit.Seconds
			else DateTimeUnit.Millis



	@inline override def isZero :Boolean = inMillis == 0L

	@inline override def signum :Int = inMillis.signum

	@inline override def abs :Milliseconds =
		if (inMillis >= 0)
			this
		else
			if (inMillis == Long.MinValue)
				overflow(toString, "abs")
			else
				new Milliseconds(-inMillis)

	@inline override def unary_- :Milliseconds =
		if (inMillis == Long.MinValue)
			overflow(toString, "-")
		else
			new Milliseconds(-inMillis)



	@inline def +(millis :Milliseconds) :Milliseconds =
		if (inMillis > 0 && millis.inMillis > Long.MaxValue - inMillis
			|| inMillis < 0 && millis.inMillis < Long.MinValue - inMillis
		)
			overflow(toString," + ", millis.toString)
		else
			new Milliseconds(inMillis + millis.inMillis)


	@inline def -(millis :Milliseconds) :Milliseconds =
		if (inMillis > 0 && millis.inMillis < inMillis - Long.MaxValue
			|| inMillis < 0 && millis.inMillis > inMillis - Long.MinValue
		)
			overflow(toString," - ", inMillis.toString)
		else
			new Milliseconds(inMillis - millis.inMillis)


	@inline def /(millis :Milliseconds) :Double = {
		val max =
			if (millis.inMillis > 0) Long.MaxValue / millis.inMillis
			else -Long.MaxValue / millis.inMillis
		if (inMillis > max || inMillis < -max)
			overflow(toString" / ", millis.toString)
		inMillis.toDouble / millis.inMillis
	}


	@inline def /%(millis :Milliseconds) :Long = {
		val max =
			if (millis.inMillis > 0) Long.MaxValue / millis.inMillis
			else -Long.MaxValue / millis.inMillis
		if (inMillis > max || inMillis < -max)
			overflow(toString," / ", millis.toString)
		inMillis / millis.inMillis
	}



	override def +(time :FiniteTimeSpan) :FiniteTimeSpan = time match {
		case _ if inMillis == 0 => time
		case _ if time.signum == 0 => this
		case millis :Milliseconds =>
			if (inMillis > 0 && millis.inMillis <= Long.MaxValue - inMillis
				|| inMillis < 0 && millis.inMillis >= Long.MinValue - inMillis)
				new Milliseconds(inMillis + millis.inMillis)
			else {
				val seconds = inMillis / 1000L + millis.inMillis / 1000L
				val nanos = ((inMillis % 1000L + millis.inMillis % 1000L) * NanosInMilli).toInt
				new Duration(j.Duration.ofSeconds(seconds, nanos))
			}
		case _ => time.add(inMillis / 1000L, (inMillis % 1000L * NanosInMilli).toInt)
	}


	override def -(time :FiniteTimeSpan) :FiniteTimeSpan = time match {
		case _ if time.signum == 0 => this
		case millis :Milliseconds =>
			if (inMillis > 0 && millis.inMillis >= inMillis - Long.MaxValue
				|| inMillis < 0 && millis.inMillis <= inMillis - Long.MinValue)
				new Milliseconds(inMillis - time.inMillis)
			else {
				val seconds = inMillis / 1000L - millis.inMillis / 1000L
				val nanos = ((inMillis % 1000L - millis.inMillis % 1000L) * NanosInMilli).toInt
				new Duration(j.Duration.ofSeconds(seconds, nanos))
			}
		case _ => time.subtractFrom(inMillis / 1000L, (inMillis % 1000L * NanosInMilli).toInt)
	}



	@inline override def +(time :TimeSpan) :TimeSpan = time match {
		case finite :FiniteTimeSpan => this + finite
		case _ => time.add(inMillis / 1000L, (inMillis % 1000L * NanosInMilli).toInt)
	}

	@inline override def -(time :TimeSpan) :TimeSpan = time match {
		case finite :FiniteTimeSpan => this - finite
		case _ => time.subtractFrom(inMillis / 1000L, (inMillis % 1000L * NanosInMilli).toInt)
	}


	@inline override def *(n :Int) :Milliseconds = {
		val max = if (n > 0) Long.MaxValue / n else -Long.MaxValue / n
		if (inMillis > max || inMillis < -max)
			overflow(toString" * ", n.toString)
		new Milliseconds(inMillis * n)
	}

	override def *(n :Long) :FiniteTimeSpan =
		if (inMillis == 0) this
		else if (n == 0) Milliseconds.ZeroRef.asInstanceOf[Milliseconds]
		else {
			val max = if (n > 0) Long.MaxValue / n else -Long.MaxValue / n
			if (inMillis > 0)
				if (max >= inMillis)
					new Milliseconds(inMillis * n)
				else if (max >= inMillis / 1000L)
					     new Duration(j.Duration.ofSeconds(inMillis / 1000L * n, inMillis % 1000L * n * NanosInMilli))
				else
					overflow(toString" * ", n.toString)
			else
			if (-max <= inMillis)
				new Milliseconds(inMillis * n)
			else if (-max <= inMillis / 1000L)
				     new Duration(j.Duration.ofSeconds(inMillis / 1000L * n, inMillis % 1000L * n * NanosInMilli))
			else
				overflow(toString" * ", n.toString)
		}


	@inline override def *(n :Double) :TimeSpan =
		if (inMillis == 0L) this
		else if (n == 0d) Milliseconds.ZeroRef.asInstanceOf[Milliseconds]
		else spanOfMillis(inMillis * n)


	@inline override def /(n :Long) :Milliseconds =
		if (n == 0L) throw new ArithmeticException(this + " / 0")
		else if (inMillis == 0L) this
		else new Milliseconds(inMillis / n)


	@inline override def /(n :Double) :TimeSpan =
		if (n == 0) throw new ArithmeticException(this + " / 0")
		else if (inMillis == 0) this
		else spanOfMillis(inMillis / n)



	private[slang] def spanOfMillis(millis :Double) :TimeSpan =
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



	@inline override def /(span :TimeSpan) :Double = inMillis / span.toMillis



	@inline override def /(unit :TimeUnit) :Double = inMillis.toDouble * NanosInMilli / unit.inNanos

	@inline override def %(unit :TimeUnit) :Milliseconds = {
		val divisor = unit.inNanos
		if (divisor <= NanosInMilli) new Milliseconds(0)
		else new Milliseconds(inMillis % (divisor / NanosInMilli))
	}


	override def add(seconds :Long, nanos :Int) :FiniteTimeSpan =
		new Duration(j.Duration.ofSeconds(seconds + inMillis / 1000L, nanos + (inMillis % 1000L).toInt))

	override def subtractFrom(seconds :Long, nanos :Int) :FiniteTimeSpan =
		new Duration(j.Duration.ofSeconds(seconds - inMillis / 1000L, nanos - (inMillis % 1000L).toInt))


	@inline def compare(that :Milliseconds) :Int =
		if (inMillis < that.inMillis) -1
		else if (inMillis > that.inMillis) 1
		else 0

	@inline def <=(that :Milliseconds) :Boolean = inMillis <= that.inMillis
	@inline def < (that :Milliseconds) :Boolean = inMillis < that.inMillis
	@inline def >=(that :Milliseconds) :Boolean = inMillis >= that.inMillis
	@inline def > (that :Milliseconds) :Boolean = inMillis > that.inMillis
	@inline def min(that :Milliseconds) :Milliseconds = if (inMillis <= that.inMillis) this else that
	@inline def max(that :Milliseconds) :Milliseconds = if (inMillis >= that.inMillis) this else that

	@inline def ===(that :Milliseconds) :Boolean = inMillis == that.inMillis


	override def toString :String = {
		val u = unit
		inMillis / (unit.inNanos / 1000) + u.symbol
	}

}






object Milliseconds {
	@inline def apply(millis :Long) :Milliseconds = new Milliseconds(millis)

	@inline def unapply(span :TimeSpan) :Option[Long] = span match {
		case millis :Milliseconds => Some(millis.inMillis)
		case _ => None
	}

	@inline def between(start :UnixTime, until :UnixTime) :Milliseconds =
		if (if (start.epochMilli > 0) until.epochMilli > Long.MinValue + start.epochMilli
		    else until.epochMilli < Long.MaxValue + start.epochMilli)
			overflow(until.toString, " - ", start.toString)
		else
			new Milliseconds(until.epochMilli - start.epochMilli)



	@inline def since(moment :UnixTime)(implicit time :Time = Time.Local) :Milliseconds = {
		val now = time.clock.millis
		if (moment.epochMilli < now - Long.MaxValue)
			overflow(new UnixTime(now).toString, " - ", moment.toString)
		else
	        new Milliseconds(now - moment.epochMilli)
	}

	@inline def until(moment :UnixTime)(implicit time :Time = Time.Local) :Milliseconds = {
		val now = time.clock.millis
		if (moment.epochMilli < Long.MinValue + now)
			overflow(moment.toString, " - ", new UnixTime(now).toString)
		else
			new Milliseconds(moment.epochMilli - now)
	}


	private[time] final val ZeroRef :Any = new Milliseconds(0L)
	final val Zero = new Milliseconds(0L)
	final val Max = new Milliseconds(Long.MaxValue)
	final val Min = new Milliseconds(Long.MinValue)

}
