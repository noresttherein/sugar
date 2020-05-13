package net.noresttherein.slang.time

import java.{time => j}
import java.time.temporal.ChronoUnit
import java.time.DateTimeException

import scala.concurrent.{duration => s}



/** An umbrella term for specifying length of elapsed time which encompasses both infinite time
  * ([[net.noresttherein.slang.time.Eternity]] and [[net.noresttherein.slang.time.MinusEternity]] and finite
  * ISO ''durations'' (definite time spans with a fixed length) represented as [[net.noresttherein.slang.time.TimeSpan]],
  * ''periods'' (variable spans specified in term of months and years) represented by
  * [[net.noresttherein.slang.time.DateSpan]], as well as combinations of thereof. Two instances of `TimeLapse`
  * are considered equal only if they are of the same class. This means that `Duration(0)` does ''not'' equal
  * `Milliseconds(0)` does ''not'' equal `Immediate`, as they have different precisions. If you want to compare
  * instances of different classes in terms of their comparative length, use `===` instead. Two instances
  * are considered equal in terms of `===` if they are both the same infinity or both their fixed portion expressed in
  * seconds and nanosecond adjustment are equal and their variable portion expressed in years, months and days are equal
  * on all coordinates. This is also consistent with ordering defined on `FiniteTimeSpan`.
  * @see [[net.noresttherein.slang.time.InfiniteTimeLapse]]
  * @see [[net.noresttherein.slang.time.FiniteTimeLapse]]
  * @see [[net.noresttherein.slang.time.DateSpan]]
  * @see [[net.noresttherein.slang.time.TimeSpan]]
  * @author Marcin Mościcki marcin@moscicki.net
  */
sealed trait TimeLapse extends Any with Serializable {
	def variable :DateSpan
	def fixed :TimeSpan

	def normalized :TimeLapse

	def isInfinite :Boolean
	def isFinite :Boolean = !isInfinite
	def isZero :Boolean

	def unary_- :TimeLapse
	def +(other :TimeLapse) :TimeLapse
	def -(other :TimeLapse) :TimeLapse

	def ===(other :TimeLapse) :Boolean

}



object TimeLapse {
	final val Zero :TimeLapse = Immediate
	final val Inf :TimeLapse = Eternity
	final val MinusInf :TimeLapse = MinusEternity

	def apply(period :DateSpan, time :TimeSpan) :TimeLapse =
		if (period.isZero) time
		else if (time.isZero) period
		else (period, time) match {
			case (p :FiniteDateSpan, t :FiniteTimeSpan) =>
				new CombinedTimeLapse(p.toPeriod, t.toDuration)
			case (_ :InfiniteTimeLapse, _ :InfiniteTimeLapse) =>
				if (period != time)
					throw new IllegalArgumentException("TimeLapse(" + period + ", " + time + ")")
				else period
			case (_ :InfiniteTimeLapse, _ ) => period
			case _ => time
		}

	@inline def unapply(time :TimeLapse) :Some[(DateSpan, TimeSpan)] = Some((time.variable, time.fixed))

	@inline implicit def fromJavaDuration(time :j.Duration) :Duration = new Duration(time)
	@inline implicit def fromJavaPeriod(time :j.Period) :Period = new Period(time)

	@inline implicit def fromScalaDuration(time :s.Duration) :TimeLapse = time match {
		case s.Duration.Inf => Eternity
		case s.Duration.MinusInf => MinusEternity
		case _ =>
			val nanos = time.toNanos
			new Duration(j.Duration.ofSeconds(nanos / NanosInSecond, nanos % NanosInSecond))
	}

}







/** Base trait for measured elapsed time in both fixed length time units and variable length calendar units.
  * Instances include [[net.noresttherein.slang.time.FiniteTimeSpan]] for time-based spans,
  * [[net.noresttherein.slang.time.FiniteDateSpan]] for date-based spans,
  * special zero-length [[net.noresttherein.slang.time.Immediate]] and an implementation containing both non-zero
  * fixed and variable span parts.
  */
trait FiniteTimeLapse extends Any with TimeLapse {
	override def variable :FiniteDateSpan = period
	override def fixed :FiniteTimeSpan = duration
	def period :Period
	def duration :Duration

	override def isFinite = true
	override def isInfinite = false

	override def unary_- :FiniteTimeLapse

	override def normalized :FiniteTimeLapse

	def from(time :DateTimePoint) :DateTimePoint = time + this
	def before(time :DateTimePoint) :DateTimePoint = time - this

	def +(other :FiniteTimeLapse) :FiniteTimeLapse
	def -(other :FiniteTimeLapse) :FiniteTimeLapse

	override def +(other :TimeLapse) :TimeLapse = other match {
		case finite :FiniteTimeLapse =>
			this + finite
		case Eternity => Eternity
		case MinusEternity => MinusEternity
	}

	override def -(other :TimeLapse) :TimeLapse = other match {
		case finite :FiniteTimeLapse =>
			this - finite
		case Eternity => MinusEternity
		case MinusEternity => Eternity
	}


}



object FiniteTimeLapse {

	@inline def apply(period :FiniteDateSpan, time :FiniteTimeSpan) :FiniteTimeLapse =
		if (period.isZero) time
		else if (time.isZero) period
		else new CombinedTimeLapse(period.toPeriod, time.toDuration)

	@inline def apply(period :Period, time :Duration) :FiniteTimeLapse =
		if (period.isZero) time
		else if (time.isZero) period
		else new CombinedTimeLapse(period, time)

	def between(start :TimeOfDay, end :TimeOfDay) :FiniteTimeLapse =
		new Duration(j.Duration.between(start.toJava, end.toJava))

	def between(start :Date, end :Date) :FiniteTimeLapse = new Period(j.Period.between(start.toJava, end.toJava))

	def between(start :DateTime, end :DateTime) :FiniteTimeLapse = apply(
		new Period(j.Period.between(start.toJava.toLocalDate, end.toJava.toLocalDate)),
		new Duration(j.Duration.between(start.toJava.toLocalTime, end.toJava.toLocalTime))
	)

	def between(start :DefiniteTime, end :DefiniteTime) :FiniteTimeLapse = end - start



	def unapply(time :TimeLapse) :Option[(Period, Duration)] = time match {
		case finite :FiniteTimeLapse => Some((finite.period, finite.duration))
		case _ => None
	}



	final val Zero :FiniteTimeLapse = Immediate
}






/** Elapsed time of fixed length. This includes infinite both `Eternity`/`MinusEternity`
  * and finite `FiniteTimeSpan` subclasses. Instances of all subclasses, including infinite implementations,
  * form a combined linear order. Any two `TimeSpan` values can be tested with `===` which is consistent
  * with the ordering and compares their lengths. Standard equality is class-based, with `Milliseconds(0)`,
  * `Duration(0)` and `Immediate` all being unequal.
  * @see [[net.noresttherein.slang.time.FiniteTimeSpan]]
  */
sealed trait TimeSpan extends Any with TimeLapse with Ordered[TimeSpan] {
	def inNanos :Long
	def inMicros :Long
	def toMicros :Double
	def inMillis :Long
	def toMillis :Double
	def asMillis :Milliseconds = new Milliseconds(inMillis)
	def inSeconds :Long
	def toSeconds :Double
	def inMinutes :Long
	def toMinutes :Double
	def inHours :Long
	def toHours :Double
	def inDays :Long
	def toDays :Double

	def in(unit :TimeUnit) :Long
	def to(unit :TimeUnit) :Double

	def nanos :Int
	def seconds :Int
	def minutes :Int
	def hours :Long

	def unit :DateTimeUnit

	override def fixed :TimeSpan = this
	override def variable :DateSpan = Immediate

	override def normalized :TimeSpan = this

	def toDuration :Duration = new Duration(toJava)
	def toJava :j.Duration = j.Duration.ofSeconds(inSeconds, nanos)
	def toScala :s.Duration


	def signum :Int

	def abs :TimeSpan =
		if (signum >= 0) this
		else -this

	def unary_- :TimeSpan
	def +(time :TimeSpan) :TimeSpan
	def -(time :TimeSpan) :TimeSpan

	def /(span :TimeSpan) :Double

	def /(n :Int) :TimeSpan = this / n.toLong
	def /(n :Long) :TimeSpan
	def /(n :Double) :TimeSpan

	def *(n :Int) :TimeSpan = this * n.toLong
	def *(n :Long) :TimeSpan
	def *(n :Double) :TimeSpan

	def /(unit :TimeUnit) :Double = this / unit.span
	def %(unit :TimeUnit) :TimeSpan

	def add(seconds :Long, nanos :Int) :TimeSpan
	def subtractFrom(seconds :Long, nanos :Int) :TimeSpan

	def from(instant :Timestamp) :TimePoint = instant + this
	def before(instant :Timestamp) :TimePoint = instant - this



	override def compare(that :TimeSpan) :Int = that match {
		case inf :InfiniteTimeLapse => -inf.signum
		case _ => inSeconds compare that.inSeconds match {
			case 0 => java.lang.Long.signum(nanos.toLong - that.nanos)
			case res => res
		}
	}

	def ===(that :TimeSpan) :Boolean = compare(that) == 0

	def ===(that :TimeLapse) :Boolean = that match {
		case time :TimeSpan => compare(time) == 0
		case period :DateSpan if period.isZero => isZero
		case _ => false
	}

	@inline final def min(that :TimeSpan) :TimeSpan = if (compare(that) <= 0) this else that
	@inline final def max(that :TimeSpan) :TimeSpan = if (compare(that) >= 0) this else that


}





object TimeSpan {

	@inline def between(start :TimeOfDay, end :TimeOfDay) :TimeSpan = new Duration(j.Duration.between(start, end))

	@inline def between(start :DateTime, end :DateTime) :TimeSpan = new Duration(j.Duration.between(start, end))

	@inline def between(from :TimePoint, until :TimePoint) :TimeSpan = until - from



	def since(moment :TimePoint)(implicit time :Time = Time.Local) :TimeSpan = moment match {
		case unix :UnixTime =>
			val now = time.clock.millis
			if (unix.epochMilli < now - Long.MaxValue)
				overflow("TimeSpan", " since ", moment)
			new Milliseconds(now - unix.epochMilli)
		case utc :UTCDateTime =>
			new Duration(j.Duration.between(utc.in(time.clock.getZone).toJava, time.now.toJava))
		case finite :DefiniteTime => new Duration(j.Duration.between(finite.toJava, time.clock.instant))
		case DawnOfTime => Eternity
		case EndOfTime => MinusEternity
	}

	def until(moment :TimePoint)(implicit time :Time = Time.Local) :TimeSpan = moment match {
		case unix :UnixTime =>
			val now = time.clock.millis
			if (unix.epochMilli < Long.MinValue + now)
				overflow("TimeSpan", " until ", moment)
			new Milliseconds(unix.epochMilli - now)
		case utc :UTCDateTime =>
			new Duration(j.Duration.between(time.now.toJava, utc.in(time.clock.getZone).toJava))
		case finite :DefiniteTime =>
			new Duration(j.Duration.between(time.clock.instant, finite.toJava))
		case DawnOfTime => MinusEternity
		case EndOfTime => Eternity
	}


	@inline implicit def fromJavaDuration(time :j.Duration) :Duration = new Duration(time)

	@inline implicit def toScalaDuration(time :TimeSpan) :s.Duration = time.toScala

	implicit def fromScalaDuration(time :s.Duration) :TimeSpan =
		if (!time.isFinite)
			if (time == s.Duration.Inf) Eternity
			else MinusEternity
		else {
			val nanos = time.toNanos
			val rem = nanos % NanosInSecond
			if (rem == 0) new Milliseconds(nanos / NanosInSecond)
			else new Duration(j.Duration.ofSeconds(nanos / NanosInSecond, rem))
		}




	final val Inf :TimeSpan = Eternity
	final val MinusInf :TimeSpan = MinusEternity
	final val Zero :TimeSpan = Immediate

}







/** Elapsed time measured in time-based units representing the ISO-8601 concept of ''duration''. */
trait FiniteTimeSpan extends Any with TimeSpan with FiniteTimeLapse {

	override def period :Period = Period.Zero
	override def duration :Duration = toDuration
	override def fixed :FiniteTimeSpan = this

	override def normalized :FiniteTimeSpan = this

	override def unit :TimeUnit

	override def abs :FiniteTimeSpan =
		if (signum >= 0) this
		else -this

	override def unary_- :FiniteTimeSpan


	override def +(time :FiniteTimeLapse) :FiniteTimeLapse = time match {
		case span :FiniteTimeSpan => this + span
		case _ =>
			val other = time.fixed
			val s1 = inSeconds; val s2 = other.inSeconds
			if (if (s1 > 0) s2 > Long.MaxValue - s1 else s2 < Long.MinValue - s1)
				overflow(toString, " + ", time.toString)
			val s = s1 + s2; val n = nanos + other.nanos
			if (s == 0 && n == 0)
				time.variable
			else
				new CombinedTimeLapse(time.period, new Duration(j.Duration.ofSeconds(s, n)))
	}

	override def -(time :FiniteTimeLapse) :FiniteTimeLapse = time match {
		case span :FiniteTimeSpan => this - span
		case _ =>
			val other = time.fixed
			val s1 = inSeconds; val s2 = other.inSeconds
			if (if (s1 > 0) s2 > s1 - Long.MaxValue else s2 < s1 - Long.MinValue)
				overflow(toString, " - ", time.toString)
			val s = s1 - s2; val n = nanos - other.nanos
			if (s == 0 && n == 0)
				time.variable
			else
				new CombinedTimeLapse(time.period, new Duration(j.Duration.ofSeconds(s, n)))
	}


	override def +(time :TimeSpan) :TimeSpan =
		if (isZero) time
		else if (time.isZero) this
		else time.add(inSeconds, nanos)

	override def -(time :TimeSpan) :TimeSpan =
		if (time.isZero) this
		else time.subtractFrom(inSeconds, nanos)

	def +(time :FiniteTimeSpan) :FiniteTimeSpan =
		if (time.isZero) this
		else (inSeconds, nanos) match {
			case (0, 0) => time
			case (s, n) => time.add(s, n)
		}

	def -(time :FiniteTimeSpan) :FiniteTimeSpan =
		if (time.isZero) this
		else time.subtractFrom(inSeconds, nanos)


	def /%(time :FiniteTimeSpan) :Long = {
		import java.math.BigDecimal.valueOf
		val x = valueOf(inSeconds) multiply valueOf(NanosInSecond) add valueOf(nanos)
		val y = valueOf(time.inSeconds) multiply valueOf(NanosInSecond) add valueOf(time.nanos)
		val res = x divideToIntegralValue y
		res.longValueExact()
	}

	override def /(n :Int) :FiniteTimeSpan = this / n.toLong
	override def /(n :Long) :FiniteTimeSpan

	override def *(n :Int) :FiniteTimeSpan = this * n.toLong
	override def *(n :Long) :FiniteTimeSpan

	override def %(unit :TimeUnit) :FiniteTimeSpan

	override def add(seconds :Long, nanos :Int) :FiniteTimeSpan
	override def subtractFrom(seconds :Long, nanos :Int) :FiniteTimeSpan


	override def from(instant :Timestamp) :Timestamp = instant + this
	override def before(instant :Timestamp) :Timestamp = instant - this

	@inline final def min(that :FiniteTimeSpan) :FiniteTimeSpan = if (compare(that) <= 0) this else that
	@inline final def max(that :FiniteTimeSpan) :FiniteTimeSpan = if (compare(that) >= 0) this else that


}



object FiniteTimeSpan {

	@inline def between(start :TimeOfDay, end :TimeOfDay) :FiniteTimeSpan =
		new Duration(j.Duration.between(start.toJava, end.toJava))

	@inline def between(start :DateTime, end :DateTime) :FiniteTimeSpan =
		new Duration(j.Duration.between(start, end))

	@inline def between(start :DefiniteTime, end :DefiniteTime) :FiniteTimeSpan = end - start

	def unapply(time :TimeLapse) :Option[(Long, Int)] = time match {
		case finite :FiniteTimeSpan => Some((finite.inSeconds, finite.nanos))
		case _ => None
	}

	@inline implicit def toJavaDuration(time :FiniteTimeSpan) :j.Duration = time.toJava


	def since(moment :DefiniteTime)(implicit time :Time = Time.Local) :FiniteTimeSpan = moment match {
		case unix :UnixTime =>
			val now = time.clock.millis
			if (unix.epochMilli < now - Long.MaxValue) //this assumes now >= 0
				overflow("FiniteTimeSpan", " since ", moment)
			new Milliseconds(now - unix.epochMilli)
		case utc :UTCDateTime =>
			new Duration(j.Duration.between(utc.in(time.clock.getZone).toJava, time.clock.instant))
		case _ => new Duration(j.Duration.between(moment.toJava, time.clock.instant))
	}

	def until(moment :DefiniteTime)(implicit time :Time = Time.Local) :FiniteTimeSpan = moment match {
		case unix :UnixTime =>
			val now = time.clock.millis
			if (unix.epochMilli < Long.MinValue + now) //this assumes now >= 0
				overflow("TimeSpan", " until ", moment)
			new Milliseconds(unix.epochMilli - now)
		case utc :UTCDateTime =>
			new Duration(j.Duration.between(time.clock.instant, utc.in(time.clock.getZone).toJava))
		case _ => new Duration(j.Duration.between(time.clock.instant, moment.toJava))
	}



	final val Zero :FiniteTimeSpan = Immediate

}






/** Elapsed time measured in calendar units with the addition of special `Eternity` and `MinusEternity` singletons.
  * @see [[net.noresttherein.slang.time.FiniteDateSpan]]
  */
sealed trait DateSpan extends Any with TimeLapse {
	def days :Int
	def months :Int
	def years :Int

	def inMonths :Int = years * 12 + months
	def inYears :Int = years + months / 12

	def apply(unit :DateUnit) :Int

	override def variable :DateSpan = this
	override def fixed :TimeSpan = Immediate

	override def normalized :DateSpan

	override def unary_- :DateSpan


	def +(other :DateSpan) :DateSpan
	def -(other :DateSpan) :DateSpan
	def *(scalar :Int) :DateSpan


}






/** Proper, finite time spans measured in date-based units and thus having variable length, depending on
  * both leap years and daylight saving time. It represents the ISO-8601 concept of a ''period'' and its
  * default concrete implementation is [[net.noresttherein.slang.time.Period]].
  */
trait FiniteDateSpan extends Any with DateSpan with FiniteTimeLapse {
	override def variable :FiniteDateSpan = this
	override def period :Period = toPeriod
	override def duration :Duration = Duration.Zero

	override def normalized :FiniteDateSpan

	def toPeriod :Period


	override def unary_- :FiniteDateSpan



	override def +(other :TimeLapse) :TimeLapse = other match {
		case period :FiniteDateSpan => this + period
		case _ if other.isZero => this
		case _ if isZero || other.isInfinite => other
		case time :TimeSpan => new CombinedTimeLapse(this.toPeriod, time.toDuration)
	}

	override def -(other :TimeLapse) :TimeLapse = other match {
		case period :FiniteDateSpan => this - period
		case _ if other.isZero => this
		case _ if isZero || other.isInfinite => -other
		case time :FiniteTimeSpan => CombinedTimeLapse.periodMinusTime(toPeriod, time)
	}

	override def +(other :FiniteTimeLapse) :FiniteTimeLapse = other match {
		case period :FiniteDateSpan =>  this + period
		case _ if other.isZero => this
		case _ if isZero => other
		case time :FiniteTimeSpan => new CombinedTimeLapse(toPeriod, time.toDuration)
	}

	override def -(other :FiniteTimeLapse) :FiniteTimeLapse = other match {
		case period :FiniteDateSpan => this + period
		case _ if other.isZero => this
		case _ if isZero => -other
		case time :FiniteTimeSpan => CombinedTimeLapse.periodMinusTime(toPeriod, time)
	}

	override def +(other :DateSpan) :DateSpan = other match {
		case finite :FiniteDateSpan => this + finite
		case infinite => infinite
	}

	override def -(other :DateSpan) :DateSpan = other match {
		case finite :FiniteDateSpan => this - finite
		case infinite => -infinite
	}

	def +(other :FiniteDateSpan) :FiniteDateSpan

	def -(other :FiniteDateSpan) :FiniteDateSpan

	override def *(scalar :Int) :FiniteDateSpan


	override def ===(other :TimeLapse) :Boolean = other match {
		case period :FiniteDateSpan => days == period.days && months == period.months && years == period.years
		case _ => isZero && other.isZero
	}

}



object FiniteDateSpan {

	def between(start :Date, end :Date) :FiniteDateSpan = Period.between(start, end)

	@inline def unapply(time :TimeLapse) :Option[(Int, Int, Int)] = time match {
		case span :FiniteDateSpan => Some((span.years, span.months, span.days))
		case _ => None
	}

	@inline implicit def fromJavaPeriod(period :j.Period) :Period = new Period(period)
	@inline implicit def toJavaPeriod(period :FiniteDateSpan) :j.Period = period.toPeriod.toJava

	final val Zero :FiniteDateSpan = Period.Zero
}






/** Base trait for the two infinite `TimeLapse` implementations: `Eternity` and `MinusEternity`. */
sealed abstract class InfiniteTimeLapse extends TimeSpan with DateSpan with Serializable {

	final override def nanos :Nothing = throw new UnsupportedOperationException(s"$this.nanos")
	final override def seconds :Nothing = throw new UnsupportedOperationException(s"$this.seconds")
	final override def minutes :Nothing = throw new UnsupportedOperationException(s"$this.minutes")
	final override def hours :Nothing = throw new UnsupportedOperationException(s"$this.hours")
	final override def days :Nothing = throw new UnsupportedOperationException(s"$this.days")
	final override def months :Nothing = throw new UnsupportedOperationException(s"$this.months")
	final override def years :Nothing = throw new UnsupportedOperationException(s"$this.years")

	final override def inNanos :Nothing = throw new UnsupportedOperationException(s"$this.inNanos")
	final override def inMicros :Nothing = throw new UnsupportedOperationException(s"$this.inMicros")
	final override def toMicros :Double = infinity
	final override def inMillis :Nothing = throw new UnsupportedOperationException(s"$this.inMillis")
	final override def toMillis :Double = infinity
	final override def asMillis :Nothing = throw new UnsupportedOperationException(s"$this.asMillis")
	final override def inSeconds :Nothing = throw new UnsupportedOperationException(s"$this.inSeconds")
	final override def toSeconds :Double = infinity
	final override def inMinutes :Nothing = throw new UnsupportedOperationException(s"$this.inMinutes")
	final override def toMinutes :Double = infinity
	final override def inHours :Nothing = throw new UnsupportedOperationException(s"$this.inHours")
	final override def toHours :Double = infinity
	final override def inDays :Nothing = throw new UnsupportedOperationException(s"$this.inDays")
	final override def toDays :Double = infinity
	final override def inMonths :Nothing = throw new UnsupportedOperationException(s"$this.inMonths")
	final override def inYears :Nothing = throw new UnsupportedOperationException(s"$this.inYears")

	final override def in(unit :TimeUnit) :Nothing = throw new UnsupportedOperationException(s"$this in $unit")
	final override def to(unit :TimeUnit) :Double = infinity
	final override def apply(unit :DateUnit) :Nothing = throw new UnsupportedOperationException(s"$this($unit)")

	protected[this] def infinity :Double

	final override def toDuration :Nothing = throw new UnsupportedOperationException(s"$this.toDuration")
	final override def toJava :Nothing = throw new UnsupportedOperationException(s"$this.toJava")
	override def toScala :s.Duration.Infinite

	final override def unit :DateUnit = new DateUnit(ChronoUnit.FOREVER)


	final override def variable :InfiniteTimeLapse = this
	final override def fixed :InfiniteTimeLapse = this

	final override def normalized :InfiniteTimeLapse = this

	final override def isFinite :Boolean = false
	final override def isInfinite :Boolean = true
	final override def isZero :Boolean = false

	override def unary_- :InfiniteTimeLapse

	final override def abs :InfiniteTimeLapse = Eternity



	final override def add(seconds :Long, nanos :Int) :InfiniteTimeLapse = this
	final override def subtractFrom(seconds :Long, nanos :Int) :InfiniteTimeLapse = -this



	@inline final override def +(time :TimeLapse) :InfiniteTimeLapse =
		if (time.isInfinite && time != this)
			throw new DateTimeException(s"$this + $time")
		else this

	@inline final override def -(time :TimeLapse) :InfiniteTimeLapse =
		if (time == this) throw new DateTimeException(s"$this - $this")
		else this


	@inline final override def +(time :DateSpan) :InfiniteTimeLapse = this + (time :TimeLapse)
	@inline final override def -(time :DateSpan) :InfiniteTimeLapse = this - (time :TimeLapse)

	@inline final override def +(time :TimeSpan) :InfiniteTimeLapse = this + (time :TimeLapse)
	@inline final override def -(time :TimeSpan) :InfiniteTimeLapse = this - (time :TimeLapse)



	final override def /(time :TimeSpan) :Double = time match {
		case _ if time.isInfinite => 1d * signum * time.signum
		case _ if time.signum == 0 => throw new ArithmeticException(s"$this / $time")
		case _ if signum * time.signum == 1 => Double.PositiveInfinity
		case _ => Double.NegativeInfinity
	}

	final override def /(n :Long) :InfiniteTimeLapse =
		if (n > 0) this
		else if (n < 0) -this
		else throw new ArithmeticException(s"$this / 0")

	final override def /(n :Double) :InfiniteTimeLapse =
		if (n.isInfinite || n.isNaN) throw new DateTimeException(s"$this / $n")
		else if (n > 0) this
		else if (n < 0) -this
		else throw new ArithmeticException(s"$this / 0")

	final override def *(n :Long) :InfiniteTimeLapse =
		if (n > 0) this
		else if (n < 0) -this
		else throw new DateTimeException(s"$this * 0")

	final override def *(n :Int) :InfiniteTimeLapse =
		if (n > 0) this
		else if (n < 0) -this
		else throw new DateTimeException(s"$this * 0")

	final override def *(n :Double) :InfiniteTimeLapse =
		if (n == 0d || n.isInfinite || n.isNaN) throw new DateTimeException(s"$this * $n")
		else if (n > 0) this
		else -this


	final override def /(unit :TimeUnit) :Double = if (signum < 0) Double.NegativeInfinity else Double.PositiveInfinity
	final override def %(unit :TimeUnit) :Nothing = throw new UnsupportedOperationException(s"$this % $unit")

	final override def compare(that :TimeSpan) :Int = that match {
		case inf :InfiniteTimeLapse => signum - inf.signum
		case _ => signum
	}

	final override def ===(that :TimeSpan) :Boolean = this == that
	final override def ===(that :TimeLapse) :Boolean = this == that

}



/** Representation of positive infinite amount of time. Adding it to any finite `TimeLapse` returns itself,
  * subtracting it from a finite `TimeLapse` returns `MinusEternity`, and the same operations on any time point
  * will yield `EndOfTime` and `DawnOfTime`, respectively. Indefinite operations such as subtracting it from itself or
  * multiplying by zero all throw a `DateTimeException`, with the exception of dividing by zero, which throws
  * an `ArithmeticException` as expected.
  */
object Eternity extends InfiniteTimeLapse {
	override protected[this] def infinity :Double = Double.PositiveInfinity

	override def unary_- :InfiniteTimeLapse = MinusEternity

	override def toScala :s.Duration.Infinite = s.Duration.Inf

	override def signum :Int = 1

	override def toString = "Eternity"
}



/** Representation of negative infinite amount of time. Adding it to any finite `TimeLapse` returns itself,
  * subtracting it from a finite `TimeLapse` returns `Eternity`, and the same operations on any time point
  * will yield `DawnOfTime` and `EndOfTime`, respectively. Indefinite operations such as subtracting it from itself or
  * multiplying by zero all throw a `DateTimeException`, with the exception of dividing by zero, which throws
  * an `ArithmeticException` as expected.
  */
object MinusEternity extends InfiniteTimeLapse {
	override protected[this] def infinity :Double = Double.NegativeInfinity

	override def unary_- :InfiniteTimeLapse = Eternity

	override def toScala :s.Duration.Infinite = s.Duration.MinusInf

	override def signum :Int = -1

	override def toString = "-Eternity"
}






/** a `TimeSpan` of zero length. */
object Immediate extends FiniteTimeSpan with FiniteDateSpan with Serializable {

	override def nanos :Int = 0
	override def seconds :Int = 0
	override def minutes :Int = 0
	override def hours :Long = 0
	override def days :Int = 0
	override def months :Int = 0
	override def years :Int = 0

	override def inNanos = 0L
	override def inMicros = 0L
	override def toMicros = 0d
	override def inMillis = 0L
	override def toMillis = 0d
	override def asMillis :Milliseconds = new Milliseconds(0L)
	override def inSeconds = 0L
	override def toSeconds = 0d
	override def inMinutes = 0L
	override def toMinutes = 0d
	override def inHours = 0L
	override def toHours = 0d
	override def inDays = 0L
	override def toDays = 0d
	override def inMonths = 0
	override def inYears = 0

	override def in(unit :TimeUnit) :Long = 0L
	override def to(unit :TimeUnit) :Double = 0d
	override def apply(unit :DateUnit) :Int = 0

	override def toDuration :Duration = Duration.Zero
	override def toPeriod :Period = Period.Zero
	override def toJava :j.Duration = j.Duration.ZERO
	override def toScala :s.Duration = s.Duration.Zero


	override def unit :TimeUnit = new TimeUnit(ChronoUnit.NANOS)

	override def isInfinite :Boolean = false
	override def isZero :Boolean = true

	override def unary_- :this.type = this
	override def abs :this.type = this
	override def signum = 0

	override def variable :FiniteDateSpan = this
	override def fixed :FiniteTimeSpan = this

	override def normalized :this.type = this

	override def add(seconds :Long, nanos :Int) :Duration =
		new Duration(j.Duration.ofSeconds(seconds, nanos))

	override def subtractFrom(seconds :Long, nanos :Int) :Duration =
		new Duration(j.Duration.ofSeconds(seconds, nanos))



	@inline override def +(time :TimeLapse) :TimeLapse = time
	@inline override def -(time :TimeLapse) :TimeLapse = -time

	@inline override def +(time :FiniteTimeLapse) :FiniteTimeLapse = time
	@inline override def -(time :FiniteTimeLapse) :FiniteTimeLapse = -time

	@inline override def +(time :DateSpan) :DateSpan = time
	@inline override def -(time :DateSpan) :DateSpan = -time

	@inline override def +(time :FiniteDateSpan) :FiniteDateSpan = time
	@inline override def -(time :FiniteDateSpan) :FiniteDateSpan = time

	@inline override def +(time :TimeSpan) :TimeSpan = time
	@inline override def -(time :TimeSpan) :TimeSpan = -time

	@inline override def +(time :FiniteTimeSpan) :FiniteTimeSpan = time
	@inline override def -(time :FiniteTimeSpan) :FiniteTimeSpan = -time



	override def /(time :TimeSpan) :Double = time.signum match {
		case 0 => throw new ArithmeticException("0ns / 0")
		case _ => 0d
	}

	override def /(n :Long) :this.type =
		if (n == 0L) throw new ArithmeticException("0ns / 0")
		else this

	override def /(n :Double) :this.type =
		if (n == 0d) throw new ArithmeticException(s"$this / 0")
		else this

	override def *(n :Long) :this.type = this

	override def *(n :Int) :this.type = this

	override def *(n :Double) :this.type =
		if (n.isInfinite || n.isNaN) throw new DateTimeException(s"0ns * $n")
		else this


	override def /(unit :TimeUnit) :Double = 0d
	override def %(unit :TimeUnit) :this.type = this

	override def compare(that :TimeSpan) :Int = that.signum

	override def ===(that :TimeSpan) :Boolean = that.isZero
	override def ===(that :TimeLapse) :Boolean = that.isZero

	def unapply(that :TimeLapse) :Boolean = that.isZero


	override def toString :String = "0ns"

}






private[time] final class CombinedTimeLapse(val period :Period, val duration :Duration)
	extends FiniteTimeLapse with Serializable
{
	assert(!period.isZero && !duration.isZero, s"Zero component given to CombinedTimeLapse($period, $duration)")

	override def variable :Period = period
	override def fixed :FiniteTimeSpan = duration

	override def normalized :FiniteTimeLapse = new CombinedTimeLapse(period.normalized, duration)

	override def isZero :Boolean = period.isZero && duration.isZero

	override def unary_- :FiniteTimeLapse = new CombinedTimeLapse(-period, -duration)


	override def +(other :FiniteTimeLapse) :FiniteTimeLapse = {
		val time = other.fixed
		val s1 = duration.inSeconds; val s2 = time.inSeconds
		if (if (s2 > 0) s1 > Long.MaxValue - s2 else s1 < Long.MinValue - s2)
			overflow(toString, " + ", other.toString)
		result(" + ", other)(period + other.period, s1 + s2, duration.nanos + time.nanos)
	}


	override def -(other :FiniteTimeLapse) :FiniteTimeLapse = {
		val time = other.fixed
		val s1 = duration.inSeconds; val s2 = time.inSeconds
		if (if (s2 > 0) s1 < Long.MinValue + s2 else s1 > Long.MaxValue - s1)
			overflow(toString, " - ", other.toString)
		result(" - ", other)(period - other.period, s1 - s2, duration.nanos - time.nanos)
	}

	private def result(op :String, other :TimeLapse)(period :Period, seconds :Long, nanos :Int) :FiniteTimeLapse = {
		var s = seconds
		var n = nanos
		if (nanos >= IntNanosInSecond)
			if (seconds == Long.MaxValue)
				overflow(toString, op, other.toString)
			else {
				s += 1
				n -= IntNanosInSecond
			}
		else if (nanos <= -IntNanosInSecond)
		     if (seconds == Long.MinValue)
			     overflow(toString, op, other.toString)
		     else {
			     s -= 1
			     n += IntNanosInSecond
		     }
		if (s == 0 && n == 0)
			if (period.isZero)
				FiniteTimeLapse.Zero
			else
				period
		else
			if (period.isZero)
				new Duration(j.Duration.ofSeconds(s, n))
			else
				new CombinedTimeLapse(period, new Duration(j.Duration.ofSeconds(s, n)))

	}



	override def ===(other :TimeLapse) :Boolean = this == other


	override def toString :String = period.toString + " " + duration
}






private[time] object CombinedTimeLapse {
	@inline private[time] def periodMinusTime(period :Period, time :FiniteTimeSpan) :CombinedTimeLapse = {
		val seconds = time.inSeconds
		if (seconds == Long.MinValue)
			overflow(period, " - ", time)
		new CombinedTimeLapse(period, new Duration(j.Duration.ofSeconds(-seconds, -time.nanos)))

	}
}