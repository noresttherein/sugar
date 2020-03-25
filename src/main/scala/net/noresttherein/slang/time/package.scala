package net.noresttherein.slang

import java.time.temporal.ChronoUnit



/** Light wrappers over classes from the `java.time` package providing a nicer scala interface, including
  * arithmetic operations and implicit passing of a `java.time.Clock`.
  * @author Marcin Mościcki marcin@moscicki.net
  */
package object time {

	@inline final val NanosInMicro = 1000L
	@inline final val NanosInMilli = 1000000L
	@inline final val NanosInSecond = 1000000000L
	@inline final val NanosInMinute = 60L * NanosInSecond
	@inline final val NanosInHour = 60L * NanosInMinute
	@inline final val NanosInDay = 24L * NanosInHour

	@inline final val MicrosInMilli = 1000L

	@inline final val MillisInSecond = 1000L
	@inline final val MillisInMinute = 60 * MillisInSecond
	@inline final val MillisInHour = 60 * MillisInSecond
	@inline final val MillisInDay = 24 * MillisInHour

	@inline final val SecondsInMinute = 60L
	@inline final val SecondsInHour = 3600L
	@inline final val SecondsInDay = 24 * SecondsInHour
	@inline private[time] final val SecondsInMonth = ChronoUnit.MONTHS.getDuration.getSeconds
	@inline private[time] final val SecondsInYear = ChronoUnit.YEARS.getDuration.getSeconds
	@inline private[time] final val SecondsInEra = ChronoUnit.ERAS.getDuration.getSeconds

	@inline private[time] final val MaxNanoDuration = Long.MaxValue / NanosInMilli
	@inline private[time] final val IntNanosInSecond = 1000000000
	@inline private[time] final val IntSecondsInHalfDay = 12 * 60 * 60






	/** Throws `ArithmeticException` to indicate `Long` arithmetic overflow during operation `this op other`.
	  * Extracted to minimize inlined footprint of calling methods. Access is `protected[slang]` to avoid
	  * leaking of the API while still allowing inlining of calling methods (because it translates to `public`
	  * in the bytecode).
	  */
	private[slang] def overflow(left :Any, op :String, right :Any) :Nothing =
		throw new ArithmeticException("Long overflow: " + left + op + right)

	/** Throws `ArithmeticException` to indicate `Long` arithmetic overflow during method `method`.
	  * Extracted to minimize inlined footprint of calling methods. Access is `protected[slang]` to avoid
	  * leaking of the API while still allowing inlining of calling methods (because it translates to `public`
	  * in the bytecode).
	  */
	private[slang] def overflow(self :Any, method :String) :Nothing =
		throw new ArithmeticException("Long overflow: (" + self + ")." + method)


	@inline private[slang] def twoDigit(ord :Int) :String =
		if (ord < 10) "0" + ord else ord.toString

}
