package net.noresttherein.sugar

import java.time.temporal.ChronoUnit

import net.noresttherein.sugar.exceptions.{FlexibleExceptionFactory, LazyException, SugaredArithmeticException, ThrowableFactory}



/** Light wrappers over classes from the `java.time` package providing a nicer scala interface, including
  * arithmetic operations and implicit passing of a `java.time.Clock`.
  * @author Marcin Mościcki
  */
package object time {
	private[time] final val Ver = 1L

	private[time] val SugaredDateTimeException :FlexibleExceptionFactory =
		ThrowableFactory(new SugaredDateTimeException(_, _, _))

	/** Throws an `ArithmeticException` to indicate `Long` arithmetic overflow during operation `this op other`.
	  * Extracted to minimize inlined footprint of calling methods. Access is `private[sugar]` to avoid
	  * leaking of the API while still allowing inlining of calling methods (because it translates to `public`
	  * in the bytecode).
	  */
	private[sugar] def overflow(left :Any, op :String, right :Any) :Nothing =
		throw SugaredArithmeticException("Long overflow: " + left + op + right)

	/** Throws an `ArithmeticException` to indicate that a division by zero was requested.
	  * Extracted to minimize the footprint of inlined methods. Access is `private[sugar]` to avoid
	  * leaking of the API while still allowing inlining of calling methods (because it translates to `public`
	  * in the bytecode).
	  */
	private[sugar] def divZero(what :String) :Nothing =
		throw SugaredArithmeticException(what + " / 0")

	/** Throws an `ArithmeticException` to indicate `Long` arithmetic overflow during method `method`.
	  * Extracted to minimize inlined footprint of calling methods. Access is `private[sugar]` to avoid
	  * leaking of the API while still allowing inlining of calling methods (because it translates to `public`
	  * in the bytecode).
	  */
	private[sugar] def overflow(self :Any, method :String) :Nothing =
		throw SugaredArithmeticException("Long overflow: (" + self + ")." + method)


	@inline private[sugar] def twoDigit(ord :Int) :String =
		if (ord < 10) "0" + ord else ord.toString





	@inline private[time] def lte(sec1 :Long, nano1 :Int, sec2 :Long, nano2 :Int) :Boolean =
		sec1 < sec2 || sec1 == sec2 && nano1 <= nano2

	@inline private[time] def lt(sec1 :Long, nano1 :Int, sec2 :Long, nano2 :Int) :Boolean =
		sec1 < sec2 || sec1 == sec2 && nano1 < nano2

	@inline private[time] def gte(sec1 :Long, nano1 :Int, sec2 :Long, nano2 :Int) :Boolean =
		sec1 > sec2 || sec1 == sec2 && nano1 >= nano2

	@inline private[time] def gt(sec1 :Long, nano1 :Int, sec2 :Long, nano2 :Int) :Boolean =
		sec1 > sec2 || sec1 == sec2 && nano1 > nano2
}




package time {

	import java.time.DateTimeException

	import net.noresttherein.sugar.exceptions.ImplException

	@SerialVersionUID(Ver)
	private[time] class SugaredDateTimeException(message :String, protected override var lazyMsg :() => String, cause :Throwable)
		extends DateTimeException(message, cause) with LazyException with ImplException

	object constants {
		@inline final val NanosInMicro    = 1000L
		@inline final val NanosInMilli    = 1000_000L
		@inline final val NanosInSecond   = 1000_000_000L
		@inline final val NanosInMinute   = 60L * NanosInSecond
		@inline final val NanosInHour     = 60L * NanosInMinute
		@inline final val NanosInDay      = 24L * NanosInHour

		@inline final val MicrosInMilli   = 1000L

		@inline final val MillisInSecond  = 1000L
		@inline final val MillisInMinute  = 60 * MillisInSecond
		@inline final val MillisInHour    = 60 * MillisInSecond
		@inline final val MillisInDay     = 24 * MillisInHour

		@inline final val SecondsInMinute = 60L
		@inline final val SecondsInHour   = 3600L
		@inline final val SecondsInDay    = 24 * SecondsInHour
		@inline private[time] final val SecondsInEra   = ChronoUnit.ERAS.getDuration.getSeconds
		@inline private[time] final val SecondsInYear  = 31556952L //ChronoUnit.YEARS.getDuration.getSeconds
		@inline private[time] final val SecondsInMonth = 31556952L / 12L //ChronoUnit.MONTHS.getDuration.getSeconds

		@inline private[time] final val MaxNanoDuration     = Long.MaxValue / NanosInMilli
		@inline private[time] final val IntNanosInSecond    = 1000_000_000
		@inline private[time] final val IntSecondsInHalfDay = 12 * 60 * 60
	}
}
