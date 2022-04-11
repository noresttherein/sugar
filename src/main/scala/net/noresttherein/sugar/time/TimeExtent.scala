package net.noresttherein.sugar.time

import java.{time => j}
import java.time.temporal.ChronoUnit
import java.time.DateTimeException

import scala.concurrent.{duration => s}



/** An umbrella term for specifying length of elapsed time which encompasses both infinite time
  * ([[net.noresttherein.sugar.time.Eternity Eternity]] and [[net.noresttherein.sugar.time.MinusEternity MinusEternity]]
  * and finite ISO ''durations'' (finite time extents with a fixed length) represented as
  * [[net.noresttherein.sugar.time.TimeSpan TimeSpan]], ''periods'' (variable spans specified in term of months and years)
  * represented by [[net.noresttherein.sugar.time.DateSpan DateSpan]], as well as combinations of thereof.
  * Two instances of `TimeExtent` are equal in terms of `==` if they are of equivalent length, defined as both being
  * the same infinity, or if their fixed portions expressed in seconds and nanosecond adjustments are of equal length,
  * and their variable portion expressed in years, months and days are equal on all coordinates.
  * This is also consistent with ordering defined on `TimeSpan`. However, standard `equals` method yields `true`
  * only if they are of the same class, as value classes have predefined equality semantics.
  * The result is that `Map`, `Set` as well as any other generic code will compare using a different relation than
  * standard application code, where the types of compared objects are known to be `TimeExtent` (or some of its subtypes).
  * For example,
  * [[net.noresttherein.sugar.time.Duration Duration]]`(0) == `[[net.noresttherein.sugar.time.Milliseconds Milliseconds]]`(0)`,
  * but `!Set(Duration(0))(Milliseconds(0))`.
  * @see [[net.noresttherein.sugar.time.InfiniteTimeInterval]]
  * @see [[net.noresttherein.sugar.time.TimeFrame]]
  * @see [[net.noresttherein.sugar.time.DateInterval]]
  * @see [[net.noresttherein.sugar.time.TimeInterval]]
  * @author Marcin Mościcki
  */
sealed trait TimeExtent extends Any with Serializable {
	/** The part of this time extent which is of variable length, that is counted in days and longer units.
	  * For finite time extents this will be a [[net.noresttherein.sugar.time.Period Period]], with the exception
	  * of the zero instance [[net.noresttherein.sugar.time.Immediate Immediate]], which returns itself.
	  * Infinite spans return themselves.
	  */
	def variable :DateInterval

	/** The part of this time extent which is of fixed length, that is counted in standard hours and shorter units,
	  * rather than days and other calendar units. Infinite extents
	  * and [[net.noresttherein.sugar.time.Immediate Immediate]] return themselves,
	  * finite instances will return a [[net.noresttherein.sugar.time.Duration Duration]],
	  * with the exception of [[net.noresttherein.sugar.time.Milliseconds Milliseconds]], which returns itself.
	  */
	def fixed :TimeInterval

	/** This time extent with the variable part normalized as per
	  * `java.time.`[[java.time.Period Period]]`.`[[java.time.Period.normalized normalized]]:
	  *
	  * Returns a copy of this period with the years and months normalized. This normalizes the years and months units,
	  * leaving the days unit unchanged. The months unit is adjusted to have an absolute value less than 12,
	  * with the years unit being adjusted to compensate. For example, a period of "1 Year and 15 months"
	  * will be normalized to "2 years and 3 months". The sign of the years and months units will be the same after
	  * normalization. For example, a period of "1 year and -25 months" will be normalized to "-1 year and -1 month".
	  *
	  * [[net.noresttherein.sugar.time.TimeSpan Fixed]] and [[net.noresttherein.sugar.time.InfiniteTimeInterval infinite]]
	  * time extents return themselves.
	  */
	def normalized :TimeExtent

	/** True for [[net.noresttherein.sugar.time.Eternity Eternity]]
	  * and [[net.noresttherein.sugar.time.MinusEternity MinusEternity]], false for all other instances.
	  */
	def isInfinite :Boolean

	/** True for all instances other than [[net.noresttherein.sugar.time.Eternity Eternity]]
	  * and [[net.noresttherein.sugar.time.MinusEternity MinusEternity]].
	  * @return `!isInfinite`.
	  */
	def isFinite :Boolean = !isInfinite

	/** True for time extents of zero length, regardless of their type. */
	def isZero :Boolean

	/** Negates the length of this time extent. The [[net.noresttherein.sugar.time.TimeExtent.fixed fixed]] part
	  * of this instance simply changes signs, while the [[net.noresttherein.sugar.time.TimeExtent.variable variable]]
	  * part changes the sign on all calendar units. [[net.noresttherein.sugar.time.InfiniteTimeInterval Infinite]]
	  * time extents return the opposite infinity.
	  */
	def unary_- :TimeExtent

	/** A time extent which is the sum of the two operand extents.
	  * [[net.noresttherein.sugar.time.Period Period]]s are summed on the coordinates; due to their variable nature,
	  * `Time.now + this + other` may be unequal to `Time.now + (this + other)`.
	  */
	def +(other :TimeExtent) :TimeExtent

	/** A time extent which is the signed difference between the two operand extents.
	  * [[net.noresttherein.sugar.time.Period Period]]s are subtracted on the coordinates; due to their variable nature,
	  * `Time.now + this - other` may be unequal to `Time.now + (this - other)`.
	  */
	def -(other :TimeExtent) :TimeExtent

	/** Compares the length of the two time extends. This is different from `equals` in that it abstracts
	  * over the exact type of each side, as long as their lengths are equal, unlike the former, which compares equal
	  * only for instances of the same type.
	  */
	def ==(other :TimeExtent) :Boolean
}



object TimeExtent {
	final val Zero :TimeExtent = Immediate
	final val Inf :TimeExtent = Eternity
	final val MinusInf :TimeExtent = MinusEternity

	def apply(period :DateInterval, time :TimeInterval) :TimeExtent =
		if (period.isZero) time
		else if (time.isZero) period
		else (period, time) match {
			case (p :DateSpan, t :TimeSpan) =>
				new DateTimeSpan(p.toPeriod, t.toDuration)
			case (_ :InfiniteTimeInterval, _ :InfiniteTimeInterval) =>
				if (period != time)
					throw new IllegalArgumentException("TimeExtent(" + period + ", " + time + ")")
				else period
			case (_ :InfiniteTimeInterval, _ ) => period
			case _ => time
		}

	@inline def unapply(time :TimeExtent) :Some[(DateInterval, TimeInterval)] = Some((time.variable, time.fixed))

	@inline implicit def fromJavaDuration(time :j.Duration) :Duration = new Duration(time)
	@inline implicit def fromJavaPeriod(time :j.Period) :Period = new Period(time)

	implicit def fromScalaDuration(time :s.Duration) :TimeExtent = time match {
		case s.Duration.Inf => Eternity
		case s.Duration.MinusInf => MinusEternity
		case _ =>
			val nanos = time.toNanos
			new Duration(j.Duration.ofSeconds(nanos / NanosInSecond, nanos % NanosInSecond))
	}
}







/** Base trait for measured finite, elapsed time in both fixed length time units and variable length calendar units.
  * Instances include [[net.noresttherein.sugar.time.TimeSpan TimeSpan]] for time-based spans,
  * [[net.noresttherein.sugar.time.DateSpan DateSpan]] for date-based spans,
  * special zero-length [[net.noresttherein.sugar.time.Immediate Immediate]] and an implementation containing
  * both non-zero fixed and variable span parts.
  */
trait TimeFrame extends Any with TimeExtent {
	override def variable :DateSpan = period
	override def fixed :TimeSpan = duration
	def period :Period
	def duration :Duration

	def durationFrom(timePoint :DefiniteTime)(implicit time :Time = Time.Local) :Duration
	def timeSpanFrom(timePoint :DefiniteTime)(implicit time :Time = Time.Local) :TimeSpan =
		durationFrom(timePoint)

	override def isFinite = true
	override def isInfinite = false

	override def unary_- :TimeFrame

	override def normalized :TimeFrame

	def from(time :DateTimePoint) :DateTimePoint = time + this
	def before(time :DateTimePoint) :DateTimePoint = time - this

	def +(other :TimeFrame) :TimeFrame
	def -(other :TimeFrame) :TimeFrame

	override def +(other :TimeExtent) :TimeExtent = other match {
		case finite :TimeFrame =>
			this + finite
		case Eternity => Eternity
		case MinusEternity => MinusEternity
	}

	override def -(other :TimeExtent) :TimeExtent = other match {
		case finite :TimeFrame =>
			this - finite
		case Eternity => MinusEternity
		case MinusEternity => Eternity
	}
}



object TimeFrame {

	def apply(period :DateSpan, time :TimeSpan) :TimeFrame =
		if (period.isZero) time
		else if (time.isZero) period
		else new DateTimeSpan(period.toPeriod, time.toDuration)

	def apply(period :Period, time :Duration) :TimeFrame =
		if (period.isZero) time
		else if (time.isZero) period
		else new DateTimeSpan(period, time)

	@inline def between(start :TimeOfDay, end :TimeOfDay) :TimeFrame =
		new Duration(j.Duration.between(start.toJava, end.toJava))

	@inline def between(start :Date, end :Date) :TimeFrame =
		new Period(j.Period.between(start.toJava, end.toJava))

	def between(start :DateTime, end :DateTime) :TimeFrame = apply(
		new Period(j.Period.between(start.toJava.toLocalDate, end.toJava.toLocalDate)),
		new Duration(j.Duration.between(start.toJava.toLocalTime, end.toJava.toLocalTime))
	)

	@inline def between(start :DefiniteTime, end :DefiniteTime) :TimeFrame = end - start



	@inline def unapply(time :TimeExtent) :Option[(Period, Duration)] = time match {
		case finite :TimeFrame => Some((finite.period, finite.duration))
		case _ => None
	}



	final val Zero :TimeFrame = Immediate
}






/** Elapsed time of fixed length. This includes both infinite
  * [[net.noresttherein.sugar.time.Eternity Eternity]]/[[net.noresttherein.sugar.time.MinusEternity MinusEternity]]
  * and finite [[net.noresttherein.sugar.time.TimeSpan TimeSpan]] subclasses. Instances of all subclasses,
  * including infinite implementations, form a combined total order. Any two `TimeInterval` values can be tested
  * with `===`, which is consistent with the ordering and compares their lengths. Standard equality on the other hand
  * is class-based, with [[net.noresttherein.sugar.time.Milliseconds Milliseconds]]`(0)`,
  * [[net.noresttherein.sugar.time.Duration Duration]]`.Zero` and [[net.noresttherein.sugar.time.Immediate Immediate]]
  * all being unequal.
  * @see [[net.noresttherein.sugar.time.TimeSpan]]
  */
sealed trait TimeInterval extends Any with TimeExtent with Ordered[TimeInterval] {
	/** The length of this time extent in nanoseconds.
	  * @throws UnsupportedOperationException if this extent is infinite.
	  * @throws ArithmeticException if the length of this time extent in nanoseconds cannot fit within `Long` precision.
	  */
	def toNanos :Long

	/** The length of this time extent in microseconds.
	  * @throws UnsupportedOperationException if this extent is infinite.
	  */
	def inNanos :Double

	/** The floor of the length of this time extent in microseconds.
	  * @throws UnsupportedOperationException if this extent is infinite.
	  * @throws ArithmeticException if the length of this time extent in microseconds cannot fit within `Long` precision.
	  */
	def toMicros :Long

	/** the length of this time extent in microseconds.
	  * @throws UnsupportedOperationException if this extent is infinite.
	  */
	def inMicros :Double

	/** The floor of the length of this time extent in milliseconds.
	  * @throws UnsupportedOperationException if this extent is infinite.
	  * @throws ArithmeticException if the length of this time extent in milliseconds cannot fit within `Long` precision.
	  */
	def toMillis :Long

	/** the length of this time extent in milliseconds.
	  * @throws UnsupportedOperationException if this extent is infinite.
	  */
	def inMillis :Double

	/** The floor of the length of this time extent in milliseconds as a `Milliseconds` time span.
	  * @throws UnsupportedOperationException if this extent is infinite.
	  * @throws ArithmeticException if the length of this time extent in milliseconds cannot fit within `Long` precision.
	  */
	def asMillis :Milliseconds = new Milliseconds(toMillis)

	/** The floor of the length of this time extent in seconds. The nanosecond remainder is available
	  * as the [[net.noresttherein.sugar.time.TimeInterval.nanos nanos]] property.
	  * @throws UnsupportedOperationException if this extent is infinite.
	  */
	def toSeconds :Long

	/** the length of this time extent in seconds.
	  * @throws UnsupportedOperationException if this extent is infinite.
	  */
	def inSeconds :Double

	/** The floor of the length of this time extent in minutes.
	  * @throws UnsupportedOperationException if this extent is infinite.
	  */
	def toMinutes :Long

	/** the length of this time extent in minutes.
	  * @throws UnsupportedOperationException if this extent is infinite.
	  */
	def inMinutes :Double

	/** The floor of the length of this time extent in hours.
	  * @throws UnsupportedOperationException if this extent is infinite.
	  */
	def toHours :Long

	/** the length of this time extent in hours.
	  * @throws UnsupportedOperationException if this extent is infinite.
	  */
	def inHours :Double

	/** The floor of the length of this time extent in standard days, counted as 24 hours.
	  * @throws UnsupportedOperationException if this extent is infinite.
	  */
	def toDays :Long

	/** the length of this time extent in standard days, counted as 24 hours.
	  * @throws UnsupportedOperationException if this extent is infinite.
	  */
	def inDays :Double

	/** The floor of the length of this time extent counted in the given units.
	  * @throws UnsupportedOperationException if this extent is infinite.
	  * @throws ArithmeticException if the length of this extent counted in the specified units
	  *                             does not fit into `Long` precision.
	  */
	def to(unit :TimeUnit) :Long

	/** the length of this time extent in the given units.
	  * @throws UnsupportedOperationException if this extent is infinite.
	  */
	def in(unit :TimeUnit) :Double

	/** The nanosecond remainder of the length of this time extent in seconds.
	  * It is always a value in the [0..1000 000 000) range.
	  * @throws UnsupportedOperationException if this extent is infinite.
	  */
	def nanos :Int

	/** The remaining seconds in this time extent that do not fit into full minutes.
	  * The result is a floor from the division of [[net.noresttherein.sugar.time.TimeInterval.toSeconds toSeconds]] by `60`.
	  * @throws UnsupportedOperationException if this extent is infinite.
	  */
	def seconds :Int

	/** The remaining minutes in this time extent that do not fit into full hours.
	  * The result is a floor from the division of [[net.noresttherein.sugar.time.TimeInterval.toSeconds toSeconds]] by `3600`.
	  * @throws UnsupportedOperationException if this extent is infinite.
	  */
	def minutes :Int

	/** The remaining hours in this time extent after dividing its length in seconds by the number of seconds
	  * in a standard 24 hour day.
	  */
	def hours :Long

	/** The largest time unit (from
	  * [[net.noresttherein.sugar.time.DateTimeUnit$ DateTimeUnit]]`.`[[net.noresttherein.sugar.time.DateTimeUnit.Nanos Nanos]]
	  * to `DateTimeUnit.`[[net.noresttherein.sugar.time.DateTimeUnit.Hours Hours]] such that
	  * `Duration(this.`[[net.noresttherein.sugar.time.TimeInterval.to to]]`(unit), unit) == this`.
	  * [[net.noresttherein.sugar.time.InfiniteTimeInterval Infinite]] time extents return
	  * `DateTimeUnit.`[[net.noresttherein.sugar.time.DateTimeUnit.Forever Forever]] instead.
	  */
	def unit :DateTimeUnit

	override def fixed    :TimeInterval = this
	override def variable :DateInterval = Immediate

	override def normalized :TimeInterval = this

	def toMilliseconds :Milliseconds
	def toDuration     :Duration     = new Duration(toJava)
	def toJava         :j.Duration   = j.Duration.ofSeconds(toSeconds, nanos)
	def toScala        :s.Duration


	def signum :Int

	def abs :TimeInterval =
		if (signum >= 0) this
		else -this

	def unary_- :TimeInterval
	def +(time :TimeInterval) :TimeInterval
	def -(time :TimeInterval) :TimeInterval

	def /(span :TimeInterval) :Double

	def /(n :Int)    :TimeInterval = this / n.toLong
	def /(n :Long)   :TimeInterval
	def /(n :Double) :TimeInterval

	def *(n :Int)    :TimeInterval = this * n.toLong
	def *(n :Long)   :TimeInterval
	def *(n :Double) :TimeInterval

	def /(unit :TimeUnit) :Double = this / unit.span
	def %(unit :TimeUnit) :TimeInterval

	def add(seconds :Long, nanos :Int)          :TimeInterval
	def subtractFrom(seconds :Long, nanos :Int) :TimeInterval

	def from(instant :Timestamp) :TimePoint = instant + this
	def before(instant :Timestamp) :TimePoint = instant - this



	override def compare(that :TimeInterval) :Int = that match {
		case inf :InfiniteTimeInterval => -inf.signum
		case _ => toSeconds compare that.toSeconds match {
			case 0 => java.lang.Long.signum(nanos.toLong - that.nanos)
			case res => res
		}
	}

	def ==(that :TimeInterval) :Boolean = compare(that) == 0

	def ==(that :TimeExtent) :Boolean = that match {
		case time :TimeInterval => compare(time) == 0
		case period :DateInterval if period.isZero => isZero
		case _ => false
	}

	@inline final def min(that :TimeInterval) :TimeInterval = if (compare(that) <= 0) this else that
	@inline final def max(that :TimeInterval) :TimeInterval = if (compare(that) >= 0) this else that
}





object TimeInterval {

	def apply(time :s.Duration) :TimeInterval =
		if (!time.isFinite)
			if (time == s.Duration.Inf) Eternity
			else MinusEternity
		else {
			val nanos = time.toNanos
			val rem = nanos % NanosInSecond
			if (rem == 0) new Milliseconds(nanos / NanosInSecond)
			else new Duration(j.Duration.ofSeconds(nanos / NanosInSecond, rem))
		}

	@inline def between(start :TimeOfDay, end :TimeOfDay) :TimeInterval = new Duration(j.Duration.between(start, end))
	@inline def between(start :DateTime, end :DateTime) :TimeInterval = new Duration(j.Duration.between(start, end))
	@inline def between(from :TimePoint, until :TimePoint) :TimeInterval = until - from


	def since(moment :TimePoint)(implicit time :Time = Time.Local) :TimeInterval = moment match {
		case unix :PosixTime =>
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

	def until(moment :TimePoint)(implicit time :Time = Time.Local) :TimeInterval = moment match {
		case unix :PosixTime =>
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
	@inline implicit def toScalaDuration(time :TimeInterval) :s.Duration = time.toScala
	@inline implicit def fromScalaDuration(time :s.Duration) :TimeInterval = TimeInterval(time)


	final val Inf      :TimeInterval = Eternity
	final val MinusInf :TimeInterval = MinusEternity
	final val Zero     :TimeInterval = Immediate
}







/** Elapsed time measured in time-based units representing the ISO-8601 concept of ''duration''.
  * The default implementation is [[net.noresttherein.sugar.time.Duration Duration]]; this trait exists
  * primarily to allow interoperability
  * with the lightweight span of [[net.noresttherein.sugar.time.Milliseconds Milliseconds]]
  * and [[net.noresttherein.sugar.time.Immediate Immediate]]
  * (a zero-length duration which is also a [[net.noresttherein.sugar.time.DateSpan DateSpan]]).
  */
trait TimeSpan extends Any with TimeInterval with TimeFrame {

	override def period   :Period = Period.Zero
	override def duration :Duration = toDuration
	override def fixed    :TimeSpan = this

	override def normalized :TimeSpan = this

	override def durationFrom(timePoint :DefiniteTime)(implicit time :Time = Time.Local) :Duration = toDuration
	override def timeSpanFrom(timePoint :DefiniteTime)(implicit time :Time = Time.Local) :TimeSpan = this

	override def unit :TimeUnit

	override def abs :TimeSpan =
		if (signum >= 0) this else -this

	override def unary_- :TimeSpan


	override def +(time :TimeFrame) :TimeFrame = time match {
		case span :TimeSpan => this + span
		case _ =>
			val other = time.fixed
			val s1 = toSeconds; val s2 = other.toSeconds
			if (if (s1 > 0) s2 > Long.MaxValue - s1 else s2 < Long.MinValue - s1)
				overflow(toString, " + ", time.toString)
			val s = s1 + s2; val n = nanos + other.nanos
			if (s == 0 && n == 0)
				time.variable
			else
				new DateTimeSpan(time.period, new Duration(j.Duration.ofSeconds(s, n)))
	}

	override def -(time :TimeFrame) :TimeFrame = time match {
		case span :TimeSpan => this - span
		case _ =>
			val other = time.fixed
			val s1 = toSeconds; val s2 = other.toSeconds
			if (if (s1 > 0) s2 > s1 - Long.MaxValue else s2 < s1 - Long.MinValue)
				overflow(toString, " - ", time.toString)
			val s = s1 - s2; val n = nanos - other.nanos
			if (s == 0 && n == 0)
				time.variable
			else
				new DateTimeSpan(time.period, new Duration(j.Duration.ofSeconds(s, n)))
	}


	override def +(time :TimeInterval) :TimeInterval =
		if (isZero) time
		else if (time.isZero) this
		else time.add(toSeconds, nanos)

	override def -(time :TimeInterval) :TimeInterval =
		if (time.isZero) this
		else time.subtractFrom(toSeconds, nanos)

	def +(time :TimeSpan) :TimeSpan =
		if (time.isZero) this
		else (toSeconds, nanos) match {
			case (0, 0) => time
			case (s, n) => time.add(s, n)
		}

	def -(time :TimeSpan) :TimeSpan =
		if (time.isZero) this
		else time.subtractFrom(toSeconds, nanos)


	def /~(time :TimeSpan) :Long = {
		import java.math.BigDecimal.valueOf
		val x = valueOf(toSeconds) multiply valueOf(NanosInSecond) add valueOf(nanos)
		val y = valueOf(time.toSeconds) multiply valueOf(NanosInSecond) add valueOf(time.nanos)
		val res = x divideToIntegralValue y
		res.longValueExact()
	}
	@inline final def quot(time :TimeSpan) :Long = this /~ time

	override def /(n :Int) :TimeSpan = this / n.toLong //not inlined because can be implemented more cheaply than for Long
	override def /(n :Long) :TimeSpan

	override def *(n :Int) :TimeSpan = this * n.toLong
	override def *(n :Long) :TimeSpan

	override def %(unit :TimeUnit) :TimeSpan

	override def add(seconds :Long, nanos :Int) :TimeSpan
	override def subtractFrom(seconds :Long, nanos :Int) :TimeSpan


	override def from(instant :Timestamp) :Timestamp = instant + this
	override def before(instant :Timestamp) :Timestamp = instant - this

	@inline final def min(that :TimeSpan) :TimeSpan = if (compare(that) <= 0) this else that
	@inline final def max(that :TimeSpan) :TimeSpan = if (compare(that) >= 0) this else that


}



object TimeSpan {

	@inline def between(start :TimeOfDay, end :TimeOfDay) :TimeSpan =
		new Duration(j.Duration.between(start.toJava, end.toJava))

	@inline def between(start :DateTime, end :DateTime) :TimeSpan =
		new Duration(j.Duration.between(start, end))

	@inline def between(start :DefiniteTime, end :DefiniteTime) :TimeSpan = end - start

	def unapply(time :TimeExtent) :Option[(Long, Int)] = time match {
		case finite :TimeSpan => Some((finite.toSeconds, finite.nanos))
		case _ => None
	}

	@inline implicit def toJavaDuration(time :TimeSpan) :j.Duration = time.toJava


	def since(moment :DefiniteTime)(implicit time :Time = Time.Local) :TimeSpan = moment match {
		case unix :PosixTime =>
			val now = time.clock.millis
			if (unix.epochMilli < now - Long.MaxValue) //this assumes now >= 0
				overflow("TimeSpan", " since ", moment)
			new Milliseconds(now - unix.epochMilli)
		case utc :UTCDateTime =>
			new Duration(j.Duration.between(utc.in(time.clock.getZone).toJava, time.clock.instant))
		case _ => new Duration(j.Duration.between(moment.toJava, time.clock.instant))
	}

	def until(moment :DefiniteTime)(implicit time :Time = Time.Local) :TimeSpan = moment match {
		case unix :PosixTime =>
			val now = time.clock.millis
			if (unix.epochMilli < Long.MinValue + now) //this assumes now >= 0
				overflow("TimeSpan", " until ", moment)
			new Milliseconds(unix.epochMilli - now)
		case utc :UTCDateTime =>
			new Duration(j.Duration.between(time.clock.instant, utc.in(time.clock.getZone).toJava))
		case _ => new Duration(j.Duration.between(time.clock.instant, moment.toJava))
	}



	final val Zero :TimeSpan = Immediate
}






/** Elapsed time measured in calendar units with the addition of special
  * [[net.noresttherein.sugar.time.Eternity Eternity]] and [[net.noresttherein.sugar.time.MinusEternity MinusEternity]]
  * singletons.
  * @see [[net.noresttherein.sugar.time.DateSpan]]
  */
sealed trait DateInterval extends Any with TimeExtent {
	def days :Int
	def months :Int
	def years :Int

	def inMonths :Int = years * 12 + months
	def inYears :Int = years + months / 12

	def apply(unit :DateUnit) :Int

	override def variable :DateInterval = this
	override def fixed :TimeInterval = Immediate

	override def normalized :DateInterval

	override def unary_- :DateInterval


	def +(other :DateInterval) :DateInterval
	def -(other :DateInterval) :DateInterval
	def *(scalar :Int) :DateInterval
}






/** Proper, finite time extents measured in date-based units and thus having variable length, depending on
  * both leap years and daylight saving time. It represents the ISO-8601 concept of a ''period'' and its
  * default concrete implementation is [[net.noresttherein.sugar.time.Period Period]].
  * This trait is separate from `Period` primarily to allow [[net.noresttherein.sugar.time.Immediate Immediate]]
  * to extend it.
  */
trait DateSpan extends Any with DateInterval with TimeFrame {
	override def variable :DateSpan = this
	override def period   :Period   = toPeriod
	override def duration :Duration = Duration.Zero

	override def normalized :DateSpan

	def toPeriod :Period

	override def durationFrom(timePoint :DefiniteTime)(implicit time :Time = Time.Local) :Duration =
		timePoint.local + this - timePoint

	override def unary_- :DateSpan


	override def +(other :TimeExtent) :TimeExtent = other match {
		case period :DateSpan => this + period
		case _ if other.isZero => this
		case _ if isZero || other.isInfinite => other
		case time :TimeInterval => new DateTimeSpan(this.toPeriod, time.toDuration)
		case timeframe :TimeFrame =>
			new DateTimeSpan(this.toPeriod + timeframe.period, timeframe.duration)
	}

	override def -(other :TimeExtent) :TimeExtent = other match {
		case period :DateSpan => this - period
		case _ if other.isZero => this
		case _ if isZero || other.isInfinite => -other
		case time :TimeSpan => DateTimeSpan.periodMinusTime(toPeriod, time)
		case timeframe :TimeFrame =>
			new DateTimeSpan(this.toPeriod - timeframe.period, timeframe.duration)
		case infinite => infinite
	}

	override def +(other :TimeFrame) :TimeFrame = other match {
		case period :DateSpan =>  this + period
		case _ if other.isZero => this
		case _ if isZero => other
		case time :TimeSpan => new DateTimeSpan(toPeriod, time.toDuration)
		case timeframe =>
			new DateTimeSpan(this.toPeriod + timeframe.period, timeframe.duration)
	}

	override def -(other :TimeFrame) :TimeFrame = other match {
		case period :DateSpan => this + period
		case _ if other.isZero => this
		case _ if isZero => -other
		case time :TimeSpan => DateTimeSpan.periodMinusTime(toPeriod, time)
		case timeframe =>
			new DateTimeSpan(this.toPeriod - timeframe.period, timeframe.duration)
	}

	override def +(other :DateInterval) :DateInterval = other match {
		case finite :DateSpan => this + finite
		case infinite => infinite
	}

	override def -(other :DateInterval) :DateInterval = other match {
		case finite :DateSpan => this - finite
		case infinite => -infinite
	}

	def +(other :DateSpan) :DateSpan

	def -(other :DateSpan) :DateSpan

	override def *(scalar :Int) :DateSpan


	override def ==(other :TimeExtent) :Boolean = other match {
		case period :DateSpan => days == period.days && months == period.months && years == period.years
		case _ => isZero && other.isZero
	}
}



object DateSpan {

	@inline def between(start :Date, end :Date) :DateSpan = Period.between(start, end)

	@inline def unapply(time :TimeExtent) :Option[(Int, Int, Int)] = time match {
		case span :DateSpan => Some((span.years, span.months, span.days))
		case _ => None
	}

	@inline implicit def fromJavaPeriod(period :j.Period) :Period = new Period(period)
	@inline implicit def toJavaPeriod(period :DateSpan) :j.Period = period.toPeriod.toJava

	final val Zero :DateSpan = Period.Zero
}






/** Base trait for the two infinite `TimeExtent` implementations: `Eternity` and `MinusEternity`.
  * Both are not only [[net.noresttherein.sugar.time.TimeInterval TimeSpan]]s,
  * but also [[net.noresttherein.sugar.time.DateInterval DateInterval]]s.
  */
sealed abstract class InfiniteTimeInterval extends TimeInterval with DateInterval with Serializable {
	//we don't bother with inlining as all calls are most likely polymorphic through its interfaces
	final override def nanos   :Nothing = throw new UnsupportedOperationException(s"$this.nanos")
	final override def seconds :Nothing = throw new UnsupportedOperationException(s"$this.seconds")
	final override def minutes :Nothing = throw new UnsupportedOperationException(s"$this.minutes")
	final override def hours   :Nothing = throw new UnsupportedOperationException(s"$this.hours")
	final override def days    :Nothing = throw new UnsupportedOperationException(s"$this.days")
	final override def months  :Nothing = throw new UnsupportedOperationException(s"$this.months")
	final override def years   :Nothing = throw new UnsupportedOperationException(s"$this.years")

	final override def toNanos   :Nothing = throw new UnsupportedOperationException(s"$this.toNanos")
	final override def inNanos   :Double  = infinity
	final override def toMicros  :Nothing = throw new UnsupportedOperationException(s"$this.toMicros")
	final override def inMicros  :Double  = infinity
	final override def toMillis  :Nothing = throw new UnsupportedOperationException(s"$this.toMillis")
	final override def inMillis  :Double  = infinity
	final override def asMillis  :Nothing = throw new UnsupportedOperationException(s"$this.asMillis")
	final override def toSeconds :Nothing = throw new UnsupportedOperationException(s"$this.toSeconds")
	final override def inSeconds :Double  = infinity
	final override def toMinutes :Nothing = throw new UnsupportedOperationException(s"$this.toMinutes")
	final override def inMinutes :Double  = infinity
	final override def toHours   :Nothing = throw new UnsupportedOperationException(s"$this.toHours")
	final override def inHours   :Double  = infinity
	final override def toDays    :Nothing = throw new UnsupportedOperationException(s"$this.toDays")
	final override def inDays    :Double  = infinity
	final override def inMonths  :Nothing = throw new UnsupportedOperationException(s"$this.inMonths")
	final override def inYears   :Nothing = throw new UnsupportedOperationException(s"$this.inYears")

	final override def to(unit :TimeUnit)    :Nothing = throw new UnsupportedOperationException(s"$this in $unit")
	final override def in(unit :TimeUnit)    :Double  = infinity
	final override def apply(unit :DateUnit) :Nothing = throw new UnsupportedOperationException(s"$this($unit)")

	protected[this] def infinity :Double

	final override def toMilliseconds :Nothing = throw new UnsupportedOperationException(s"$this.toMilliseconds")
	final override def toDuration     :Nothing = throw new UnsupportedOperationException(s"$this.toDuration")
	final override def toJava         :Nothing = throw new UnsupportedOperationException(s"$this.toJava")
	override def toScala              :s.Duration.Infinite

	final override def unit :DateUnit = new DateUnit(ChronoUnit.FOREVER)


	final override def variable :InfiniteTimeInterval = this
	final override def fixed    :InfiniteTimeInterval = this

	final override def normalized :InfiniteTimeInterval = this

	final override def isFinite   :Boolean = false
	final override def isInfinite :Boolean = true
	final override def isZero     :Boolean = false

	override def unary_-   :InfiniteTimeInterval

	final override def abs :InfiniteTimeInterval = Eternity


	final override def add(seconds :Long, nanos :Int) :InfiniteTimeInterval = this
	final override def subtractFrom(seconds :Long, nanos :Int) :InfiniteTimeInterval = -this


	final override def +(time :TimeExtent) :InfiniteTimeInterval =
		if (time.isInfinite && time != this)
			throw new DateTimeException(s"$this + $time")
		else this

	final override def -(time :TimeExtent) :InfiniteTimeInterval =
		if (time == this) throw new DateTimeException(s"$this - $this")
		else this


	@inline final override def +(time :DateInterval) :InfiniteTimeInterval = this + (time :TimeExtent)
	@inline final override def -(time :DateInterval) :InfiniteTimeInterval = this - (time :TimeExtent)

	@inline final override def +(time :TimeInterval) :InfiniteTimeInterval = this + (time :TimeExtent)
	@inline final override def -(time :TimeInterval) :InfiniteTimeInterval = this - (time :TimeExtent)


	final override def /(time :TimeInterval) :Double = time match {
		case _ if time.isInfinite => 1d * signum * time.signum
		case _ if time.signum == 0 => throw new ArithmeticException(s"$this / $time")
		case _ if signum * time.signum == 1 => Double.PositiveInfinity
		case _ => Double.NegativeInfinity
	}

	final override def /(n :Long) :InfiniteTimeInterval =
		if (n > 0) this
		else if (n < 0) -this
		else throw new ArithmeticException(s"$this / 0")

	final override def /(n :Double) :InfiniteTimeInterval =
		if (n.isInfinite || n.isNaN) throw new DateTimeException(s"$this / $n")
		else if (n > 0) this
		else if (n < 0) -this
		else throw new ArithmeticException(s"$this / 0")

	final override def *(n :Long) :InfiniteTimeInterval =
		if (n > 0) this
		else if (n < 0) -this
		else throw new DateTimeException(s"$this * 0")

	final override def *(n :Int) :InfiniteTimeInterval =
		if (n > 0) this
		else if (n < 0) -this
		else throw new DateTimeException(s"$this * 0")

	final override def *(n :Double) :InfiniteTimeInterval =
		if (n == 0d || n.isInfinite || n.isNaN) throw new DateTimeException(s"$this * $n")
		else if (n > 0) this
		else -this


	final override def /(unit :TimeUnit) :Double = if (signum < 0) Double.NegativeInfinity else Double.PositiveInfinity
	final override def %(unit :TimeUnit) :Nothing = throw new UnsupportedOperationException(s"$this % $unit")

	final override def compare(that :TimeInterval) :Int = that match {
		case inf :InfiniteTimeInterval => signum - inf.signum
		case _ => signum
	}

	final override def ==(that :TimeInterval) :Boolean = this equals that
	final override def ==(that :TimeExtent) :Boolean = this equals that
}



/** Representation of positive infinite amount of time. Adding it to any finite `TimeExtent` returns itself,
  * subtracting it from a finite `TimeExtent` returns `MinusEternity`, and the same operations on any time point
  * will yield `EndOfTime` and `DawnOfTime`, respectively. Indefinite operations such as subtracting it from itself or
  * multiplying by zero all throw a `DateTimeException`, with the exception of dividing by zero, which throws
  * an `ArithmeticException` as expected.
  */
@SerialVersionUID(1L)
object Eternity extends InfiniteTimeInterval {
	override protected[this] def infinity :Double = Double.PositiveInfinity

	@inline override def unary_- :InfiniteTimeInterval = MinusEternity

	@inline override def toScala :s.Duration.Infinite = s.Duration.Inf

	@inline override def signum :Int = 1

	@inline override def toString = "Eternity"
}



/** Representation of negative infinite amount of time. Adding it to any finite `TimeExtent` returns itself,
  * subtracting it from a finite `TimeExtent` returns `Eternity`, and the same operations on any time point
  * will yield `DawnOfTime` and `EndOfTime`, respectively. Indefinite operations such as subtracting it from itself or
  * multiplying by zero all throw a `DateTimeException`, with the exception of dividing by zero, which throws
  * an `ArithmeticException` as expected.
  */
@SerialVersionUID(1L)
object MinusEternity extends InfiniteTimeInterval {
	override protected[this] def infinity :Double = Double.NegativeInfinity

	@inline override def unary_- :InfiniteTimeInterval = Eternity

	@inline override def toScala :s.Duration.Infinite = s.Duration.MinusInf

	@inline override def signum :Int = -1

	@inline override def toString = "-Eternity"
}






/** a `TimeSpan` of zero length. */
@SerialVersionUID(1L)
object Immediate extends TimeSpan with DateSpan with Serializable {

	override def nanos   :Int  = 0
	override def seconds :Int  = 0
	override def minutes :Int  = 0
	override def hours   :Long = 0
	override def days    :Int  = 0
	override def months  :Int  = 0
	override def years   :Int  = 0

	override def toNanos   = 0L
	override def inNanos   = 0d
	override def toMicros  = 0L
	override def inMicros  = 0d
	override def toMillis  = 0L
	override def inMillis  = 0d
	override def asMillis  = new Milliseconds(0L)
	override def toSeconds = 0L
	override def inSeconds = 0d
	override def toMinutes = 0L
	override def inMinutes = 0d
	override def toHours   = 0L
	override def inHours   = 0d
	override def toDays    = 0L
	override def inDays    = 0d
	override def inMonths  = 0
	override def inYears   = 0

	override def to(unit :TimeUnit) :Long   = 0L
	override def in(unit :TimeUnit) :Double = 0d
	override def apply(unit :DateUnit) :Int = 0

	override def toMilliseconds :Milliseconds = Milliseconds.Zero
	override def toDuration     :Duration     = Duration.Zero
	override def toPeriod       :Period       = Period.Zero
	override def toJava         :j.Duration   = j.Duration.ZERO
	override def toScala        :s.Duration   = s.Duration.Zero


	override def unit :TimeUnit = new TimeUnit(ChronoUnit.HOURS) //to conform with the contract of Duration

	override def isInfinite :Boolean = false
	override def isZero     :Boolean = true

	override def unary_- :this.type = this
	override def abs     :this.type = this
	override def signum = 0

	override def variable :DateSpan = this
	override def fixed    :TimeSpan = this

	override def normalized :this.type = this

	override def add(seconds :Long, nanos :Int) :Duration =
		new Duration(j.Duration.ofSeconds(seconds, nanos))

	override def subtractFrom(seconds :Long, nanos :Int) :Duration =
		new Duration(j.Duration.ofSeconds(seconds, nanos))



	@inline override def +(time :TimeExtent) :TimeExtent = time
	@inline override def -(time :TimeExtent) :TimeExtent = -time

	@inline override def +(time :TimeFrame) :TimeFrame = time
	@inline override def -(time :TimeFrame) :TimeFrame = -time

	@inline override def +(time :DateInterval) :DateInterval = time
	@inline override def -(time :DateInterval) :DateInterval = -time

	@inline override def +(time :DateSpan) :DateSpan = time
	@inline override def -(time :DateSpan) :DateSpan = time

	@inline override def +(time :TimeInterval) :TimeInterval = time
	@inline override def -(time :TimeInterval) :TimeInterval = -time

	@inline override def +(time :TimeSpan) :TimeSpan = time
	@inline override def -(time :TimeSpan) :TimeSpan = -time



	override def /(time :TimeInterval) :Double = time.signum match {
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

	override def compare(that :TimeInterval) :Int = that.signum

	override def ==(that :TimeInterval) :Boolean = that.isZero
	override def ==(that :TimeExtent) :Boolean = that.isZero

	def unapply(that :TimeExtent) :Boolean = that.isZero


	override def toString :String = "0ns"
}






@SerialVersionUID(1L)
private[time] final class DateTimeSpan(val period :Period, val duration :Duration)
	extends TimeFrame with Serializable
{
	assert(!period.isZero && !duration.isZero, s"Zero component given to CombinedTimeLapse($period, $duration)")

	override def variable :Period = period
	override def fixed :TimeSpan = duration

	override def normalized :TimeFrame = new DateTimeSpan(period.normalized, duration)

	override def durationFrom(timePoint :DefiniteTime)(implicit time :Time) :Duration =
		timePoint.local + period + duration - timePoint

	override def isZero :Boolean = period.isZero && duration.isZero

	override def unary_- :TimeFrame = new DateTimeSpan(-period, -duration)


	override def +(other :TimeFrame) :TimeFrame = {
		val time = other.fixed
		val s1 = duration.toSeconds; val s2 = time.toSeconds
		if (if (s2 > 0) s1 > Long.MaxValue - s2 else s1 < Long.MinValue - s2)
			overflow(toString, " + ", other.toString)
		result(" + ", other)(period + other.period, s1 + s2, duration.nanos + time.nanos)
	}


	override def -(other :TimeFrame) :TimeFrame = {
		val time = other.fixed
		val s1 = duration.toSeconds; val s2 = time.toSeconds
		if (if (s2 > 0) s1 < Long.MinValue + s2 else s1 > Long.MaxValue - s1)
			overflow(toString, " - ", other.toString)
		result(" - ", other)(period - other.period, s1 - s2, duration.nanos - time.nanos)
	}

	private def result(op :String, other :TimeExtent)(period :Period, seconds :Long, nanos :Int) :TimeFrame = {
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
				TimeFrame.Zero
			else
				period
		else
			if (period.isZero)
				new Duration(j.Duration.ofSeconds(s, n))
			else
				new DateTimeSpan(period, new Duration(j.Duration.ofSeconds(s, n)))

	}


	override def ==(other :TimeExtent) :Boolean = this == other


	override def toString :String = period.toString + " " + duration
}






private[time] object DateTimeSpan {
	private[time] def periodMinusTime(period :Period, time :TimeSpan) :DateTimeSpan = {
		val seconds = time.toSeconds
		if (seconds == Long.MinValue)
			overflow(period, " - ", time)
		new DateTimeSpan(period, new Duration(j.Duration.ofSeconds(-seconds, -time.nanos)))

	}
}
