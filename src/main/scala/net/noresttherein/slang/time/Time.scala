package net.noresttherein.slang.time

import java.{time => j}
import java.time.Clock

import net.noresttherein.slang.time.conversions._



/** The main class of the package, abstracting over the general concept of a time line and wrapping a `java.time.Clock`.
  * Serves as a factory for time points describing the 'current' moment or moments after/before certain time span.
  * Most factory methods of classes (and objects) in this package which refer to the current moment or local
  * time zone/offset accept an implicit `Time` instance to serve as the provider, with the default value of
  * `Time.Local` in case of no implicit is available. This allows easy abstraction over the time zone and testing by
  * providing your own implicit instance. Alternatively, importing `Time.implicits.ImplicitUTCTime` will set
  * the time to current `UTC` time.
  * @author Marcin Mościcki marcin@moscicki.net
  */
class Time(val clock :Clock) extends AnyVal with Serializable {
	@inline def toJava :Clock = clock

	@inline def zone :TimeZone = new TimeZone(clock.getZone)

	@inline def offset :TimeOffset = clock.getZone match {
		case offset :j.ZoneOffset => new TimeOffset(offset)
		case zone => zone.getRules.getOffset(clock.instant)
	}


	@inline def epochMilli :Long = clock.millis
	@inline def apply() :ZoneDateTime = new ZoneDateTime(j.ZonedDateTime.now(clock))
	@inline def now :Timestamp = new Timestamp(clock.instant)
	@inline def unix :UnixTime = new UnixTime(clock.millis)
	@inline def epoch :UnixTime = new UnixTime(clock.millis)
	@inline def utc :UTCDateTime = UTCDateTime.now(clock)
	@inline def current :ZoneDateTime = new ZoneDateTime(j.ZonedDateTime.now(clock))

	@inline def date :Date = new Date(j.LocalDate.now(clock))
	@inline def time :TimeOfDay = new TimeOfDay(j.LocalTime.now(clock))
	@inline def here :OffsetTime = new OffsetTime(j.OffsetTime.now(clock))
	@inline def local :DateTime = new DateTime(j.LocalDateTime.now(clock))

	@inline def after(time :TimeSpan) :TimePoint = new Timestamp(clock.instant) + time
	@inline def after(time :FiniteTimeSpan) :DefiniteTime = new Timestamp(clock.instant) + time
	@inline def after(millis :Milliseconds) :UnixTime = new UnixTime(clock.millis) + millis
	@inline def after(time :Duration) :Timestamp = new Timestamp(clock.instant) + time

	@inline def before(time :TimeSpan) :TimePoint = new Timestamp(clock.instant) - time
	@inline def before(time :FiniteTimeSpan) :DefiniteTime = new Timestamp(clock.instant) - time
	@inline def before(time :Milliseconds) :UnixTime = new UnixTime(clock.millis) - time
	@inline def before(time :Duration) :Timestamp = new Timestamp(clock.instant) - time

	@inline def until(point :TimePoint) :TimeSpan = point - new Timestamp(clock.instant)
	@inline def until(point :DefiniteTime) :FiniteTimeSpan = point - new Timestamp(clock.instant)
	@inline def until(point :UnixTime) :Milliseconds = Milliseconds.between(new UnixTime(clock.millis), point)
	@inline def until(point :Timestamp) :Duration = point - new Timestamp(clock.instant)
	@inline def until(point :DateTime) :TimeSpan =
		point.toJava.toInstant(clock.getZone.getRules.getOffset(point.toJava)).toTimestamp - now

	@inline def since(point :TimePoint) :TimeSpan = new Timestamp(clock.instant) - point
	@inline def since(point :DefiniteTime) :FiniteTimeSpan = new Timestamp(clock.instant) - point
	@inline def since(point :UnixTime) :Milliseconds = Milliseconds.between(point, new UnixTime(clock.millis))
	@inline def since(point :Timestamp) :Duration = new Timestamp(clock.instant) - point
	@inline def since(point :DateTime) :TimeSpan =
		now - point.toJava.toInstant(clock.getZone.getRules.getOffset(point.toJava)).toTimestamp
}






object Time {
	final val UTC = new Time(Clock.systemUTC())
	final val Local = new Time(Clock.systemDefaultZone())

	@inline def in(zone :TimeZone) :Time = new Time(Clock.system(zone))

	@inline def apply(clock :Clock) :Time = new Time(clock)

	def now :Timestamp = new Timestamp(j.Instant.now())


	@inline implicit def fromClock(clock :Clock) :Time = new Time(clock)
	@inline implicit def toClock(time :Time) :Clock = time.clock


	object implicits {
		final implicit val ImplicitUTCTime = new Time(Clock.systemUTC)
		final implicit val ImplicitLocalTime = new Time(Clock.systemDefaultZone)
	}
}


