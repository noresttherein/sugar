package net.noresttherein.slang.time

import java.{time => j}



/** Constants related to the special UTC time zone and factory methods for `TimeOffset` instances.
  * @author Marcin Mościcki marcin@moscicki.net
  */
object UTC {
	final val zone :TimeZone = TimeZone.UTC
	final val offset :TimeOffset = TimeOffset.UTC
	final val time :Time = Time.UTC

	@inline def now :UTCDateTime = new UTCDateTime(j.LocalDateTime.now(time.clock))

	@inline def +(offset :FiniteTimeSpan) :TimeOffset = TimeOffset(offset)
	@inline def +(offset :Milliseconds) :TimeOffset = TimeOffset(offset)
	@inline def +(offset :Duration) :TimeOffset = TimeOffset(offset) //todo
	@inline def -(offset :FiniteTimeSpan) :TimeOffset = TimeOffset(-offset)
	@inline def -(offset :Milliseconds) :TimeOffset = TimeOffset(-offset)
	@inline def -(offset :Duration) :TimeOffset = TimeOffset(-offset) //todo
}
