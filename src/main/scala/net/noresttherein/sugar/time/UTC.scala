package net.noresttherein.sugar.time

import java.{time => j}



/** Constants related to the special UTC time zone and factory methods for `TimeOffset` instances.
  * @author Marcin Mościcki
  */
object UTC {
	final val zone   :TimeZone   = TimeZone.UTC
	final val offset :TimeOffset = TimeOffset.UTC
	final val time   :Time       = Time.UTC

	@inline def now :UTCDateTime = new UTCDateTime(j.LocalDateTime.now(time.clock))

	@inline def +(offset :TimeSpan) :TimeOffset = TimeOffset(offset)
	@inline def +(offset :Milliseconds)   :TimeOffset = TimeOffset(offset)
	@inline def +(offset :Duration)       :TimeOffset = TimeOffset(offset) //todo
	@inline def -(offset :TimeSpan) :TimeOffset = TimeOffset(-offset)
	@inline def -(offset :Milliseconds)   :TimeOffset = TimeOffset(-offset)
	@inline def -(offset :Duration)       :TimeOffset = TimeOffset(-offset) //todo
}
