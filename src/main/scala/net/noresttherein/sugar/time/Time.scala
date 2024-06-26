package net.noresttherein.sugar.time

import java.{time => j}
import java.time.Clock

import net.noresttherein.sugar.time.extensions._
import net.noresttherein.sugar.typist.CompanionObject



/** The main class of the package, abstracting over the general concept of a time line and wrapping a [[java.time.Clock]].
  * Serves as a factory for [[net.noresttherein.sugar.time.TimePoint time points]] describing the 'current' moment
  * or moments after/before a certain [[net.noresttherein.sugar.time.TimeExtent time extent]].
  * Most factory methods of classes (and objects) in this package which refer to the current moment or local
  * time zone/offset accept an implicit `Time` instance to serve as the provider, with the default value of
  * `Time.`[[net.noresttherein.sugar.time.Time.Local Local]] in case no implicit is available.
  * This allows easy abstraction over the time zone and testing by providing your own implicit instance.
  * Alternatively, importing
  * `Time.`[[net.noresttherein.sugar.time.Time.implicits implicits]]`.`[[net.noresttherein.sugar.time.Time.implicits.ImplicitUTCTime ImplicitUTCTime]]
  * will set the time to current `UTC` time.
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
class Time(val clock :Clock) extends AnyVal with Serializable {
	@inline def toJava :Clock = clock

	@inline def zone   :TimeZone   = new TimeZone(clock.getZone)

	@inline def offset :TimeOffset = clock.getZone match {
		case offset :j.ZoneOffset => new TimeOffset(offset)
		case zone => zone.getRules.getOffset(clock.instant)
	}


	@inline def epochMilli :Long         = clock.millis
	@inline def apply()    :ZoneDateTime = new ZoneDateTime(j.ZonedDateTime.now(clock))
	@inline def now        :Timestamp    = new Timestamp(clock.instant)
//	@inline def today      ://todo: what `today` should return?
	@inline def posix      :PosixTime    = new PosixTime(clock.millis)
	@inline def epoch      :PosixTime    = new PosixTime(clock.millis)
	@inline def utc        :UTCDateTime  = UTCDateTime.now(clock)
	@inline def current    :ZoneDateTime = new ZoneDateTime(j.ZonedDateTime.now(clock))

	@inline def date  :Date       = new Date(j.LocalDate.now(clock))
	@inline def time  :TimeOfDay  = new TimeOfDay(j.LocalTime.now(clock))
	@inline def here  :OffsetTime = new OffsetTime(j.OffsetTime.now(clock))
	@inline def local :DateTime   = new DateTime(j.LocalDateTime.now(clock))

	@inline def after(time :TimeInterval)   :TimePoint    = new Timestamp(clock.instant) + time
	@inline def after(time :TimeSpan)       :DefiniteTime = new Timestamp(clock.instant) + time
	@inline def after(millis :Milliseconds) :PosixTime    = new PosixTime(clock.millis) + millis
	@inline def after(time :Duration)       :Timestamp    = new Timestamp(clock.instant) + time

	@inline def before(time :TimeInterval)  :TimePoint    = new Timestamp(clock.instant) - time
	@inline def before(time :TimeSpan)      :DefiniteTime = new Timestamp(clock.instant) - time
	@inline def before(time :Milliseconds)  :PosixTime    = new PosixTime(clock.millis) - time
	@inline def before(time :Duration)      :Timestamp    = new Timestamp(clock.instant) - time

	@inline def until(point :TimePoint)     :TimeInterval = point - new Timestamp(clock.instant)
	@inline def until(point :DefiniteTime)  :TimeSpan     = point - new Timestamp(clock.instant)
	@inline def until(point :PosixTime)     :Milliseconds = Milliseconds.between(new PosixTime(clock.millis), point)
	@inline def until(point :Timestamp)     :Duration     = point - new Timestamp(clock.instant)
	@inline def until(point :DateTime)      :TimeInterval =
		point.toJava.toInstant(clock.getZone.getRules.getOffset(point.toJava)).toTimestamp - now

	@inline def since(point :TimePoint)     :TimeInterval = new Timestamp(clock.instant) - point
	@inline def since(point :DefiniteTime)  :TimeSpan     = new Timestamp(clock.instant) - point
	@inline def since(point :PosixTime)     :Milliseconds = Milliseconds.between(point, new PosixTime(clock.millis))
	@inline def since(point :Timestamp)     :Duration     = new Timestamp(clock.instant) - point
	@inline def since(point :DateTime)      :TimeInterval =
		now - point.toJava.toInstant(clock.getZone.getRules.getOffset(point.toJava)).toTimestamp
}






@SerialVersionUID(Ver)
case object Time extends CompanionObject[Time] {
	final val UTC = new Time(Clock.systemUTC())
	final val Local = new Time(Clock.systemDefaultZone())

	@inline def in(zone :TimeZone) :Time = new Time(Clock.system(zone))

	@inline def apply(clock :Clock) :Time = new Time(clock)

	@inline def now :Timestamp = new Timestamp(j.Instant.now())

	@inline def today :ZoneDateTime = new ZoneDateTime(j.ZonedDateTime.now)


	@inline implicit def TimeFromJavaClock(clock :Clock) :Time = new Time(clock)
	@inline implicit def TimeToJavaClock(time :Time) :Clock = time.clock


	object implicits {
		final implicit val ImplicitUTCTime   :Time = new Time(Clock.systemUTC)
		final implicit val ImplicitLocalTime :Time = new Time(Clock.systemDefaultZone)
	}
}


