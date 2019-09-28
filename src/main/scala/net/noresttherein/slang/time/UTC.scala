package net.noresttherein.slang.time

import java.{time => j}

/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
object UTC {
	val zone :TimeZone = TimeZone.UTC
	val offset :TimeOffset = TimeOffset.UTC
	val time :Time = Time.UTC

	def now :UTCDateTime = new UTCDateTime(j.LocalDateTime.now(time.clock))

	def +(offset :FiniteTimeSpan) :TimeOffset = TimeOffset(offset)
	def +(offset :Milliseconds) :TimeOffset = TimeOffset(offset)
	def +(offset :Duration) :TimeOffset = TimeOffset(offset) //todo
	def -(offset :FiniteTimeSpan) :TimeOffset = TimeOffset(-offset)
	def -(offset :Milliseconds) :TimeOffset = TimeOffset(-offset)
	def -(offset :Duration) :TimeOffset = TimeOffset(-offset) //todo
}
