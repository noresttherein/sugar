package net.noresttherein.sugar.numeric

import java.lang.{Math => math}
import java.io.{IOException, InputStream, OutputStream}
import java.math.{BigInteger, MathContext, RoundingMode, BigDecimal => JavaBigDecimal}
import java.math.RoundingMode.{CEILING, DOWN, FLOOR, HALF_DOWN, HALF_EVEN, HALF_UP, UNNECESSARY, UP}

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange
import scala.math.ScalaNumericAnyConversions

import net.noresttherein.sugar.exceptions.{InternalException, SugaredArithmeticException, SugaredNumberFormatException}
import net.noresttherein.sugar.numeric.Decimal64.{Decimal64AsIfIntegral, DoublePowersOf10, ExactDoublePowersOf10, ExactFloatPowersOf10, ExtendedPrecision, FloatPowersOf10, LongPowerBounds, LongPowersOf10, LongPrecision, MaxDigitsInPlainString, MaxDigitsInWholeString, MaxExponent, MaxFractionalDigitsInPlainString, MaxLeadingZerosInString, MaxLongPowerOf10, MaxLongPrecision, MaxLongValue, MaxPrecision, MaxUnscaled, MaxWholeDigitsInPlainString, MinLongValue, MinScale, MinUnscaled, MinusOne, NegativeExponentFormat, One, PositiveExponentFormat, PowersOf10, Precision, PrecisionExceededException, Round, ScaleBits, ScaleMask, ScaleSign, SignificandBits, SignificandMask, Zero, divideByDigits, divideLong, throwArithmeticException, trailingZeros}
import net.noresttherein.sugar.numeric.Decimal64.implicits.{IntScientificDecimal64Notation, LongScientificDecimal64Notation}
import net.noresttherein.sugar.numeric.Decimal64.Round.{Extended, ExtendedExact, ExtendedHalfEven, isNearestNeighbour, to16digits, to17digits, toMaxDigits}
import net.noresttherein.sugar.{illegal_!, io_!, oops}
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}
import net.noresttherein.sugar.witness.Optionally






/** A value class implementing rational numbers with a finite decimal expansion. Each value is represented as
  * [[net.noresttherein.sugar.numeric.Decimal64.unscaled unscaled]]` * 10^-`[[net.noresttherein.sugar.numeric.Decimal64.scale scale]],
  * where `unscaled` is a seven byte signed integer and `scale` is a single byte signed integer.
  * It mostly follows the IEEE 754R ''Decimal64'' format with a precision of 16 digits,
  * and is similar to [[scala.BigDecimal BigDecimal]] with a fixed [[java.math.MathContext MathContext]]
  * of [[java.math.MathContext.DECIMAL64 DECIMAL64]] (available also as
  * [[net.noresttherein.sugar.numeric.Decimal64$ Decimal64]]`.`[[net.noresttherein.sugar.numeric.Decimal64.Round.Standard Standard]]).
  * Comparing it in more detail to standard Scala and Java implementations,
  *   1. No objects are ever allocated on the heap, even when computation requires (temporarily) a higher precision
  *      than 64 bits. The only exceptions are methods returning or accepting
  *      a [[String]], [[Float]], [[Double]], [[java.math.BigDecimal]], or [[scala.math.BigDecimal]].
  *   1. All instances are normalized by dropping the maximum possible number of trailing zeros from the significand.
  *      'Maximum possible' means here reformatting to a decimal of the same value
  *      and [[net.noresttherein.sugar.numeric.Decimal64.MinScale minimum scale]] (maximum exponent) fitting
  *      in `Decimal64`'s range of `[-128, 127]`. This will never however result in the rounding of the significand,
  *      regardless of the `MathContext` specified. This is necessary to preserve value class equality
  *      on the wrapped `Long`.
  *   1. Because of the above, the unscaled value and scale are not exposed to the application.
  *      Instead, instances of `Decimal64` are represented on a more abstract level as
  *      [[net.noresttherein.sugar.numeric.Decimal64.significand significand]]`*10`^[[net.noresttherein.sugar.numeric.Decimal64.exponent exponent]]^.
  *      To compensate, it introduces properties [[net.noresttherein.sugar.numeric.Decimal64.precision precision]],
  *      [[net.noresttherein.sugar.numeric.Decimal64.fractionalDigits fractionalDigits]] and
  *      [[net.noresttherein.sugar.numeric.Decimal64.wholeDigits wholeDigits]].
  *   1. Like `java.math.BigDecimal`, but unlike `scala.math.BigDecimal`, it has no inherent associated
  *      [[java.math.MathContext MathContext]], which instead can be provided to its methods as an argument.
  *   1. Unlike in Java, methods not taking a `MathContext`, instead compute to maximum possible precision
  *      and round [[java.math.RoundingMode.HALF_EVEN HALF_EVEN]].
  *   1. `MathContext` is interpreted slightly differently, with `precision == 0` meaning 'maximum possible precision
  *      of the result which can still fit in a `Decimal64`', rather than 'unlimited'.
  *      In Java and Scala counterparts it calls for exact calculations, the same as if the rounding mode
  *      [[java.math.RoundingMode.UNNECESSARY UNNECESSARY]] was specified. In order to enforce precise results
  *      with a `Decimal64`, a rounding mode `UNNECESSARY` should be specified instead (which works also for
  *      its bigger brothers). Note that, as a consequence of this,
  *      `MathContext.`[[java.math.MathContext.UNLIMITED UNLIMITED]], which specifies precision of `0` but rounding
  *      `HALF_UP`, no longer enforces exact computations. Use
  *      [[net.noresttherein.sugar.numeric.Decimal64$ Decimal64]]`.`[[net.noresttherein.sugar.numeric.Decimal64.Round Round]]`.`[[net.noresttherein.sugar.numeric.Decimal64.Round.Exact Exact]]
  *      or [[net.noresttherein.sugar.numeric.Decimal64$ Decimal64]]`.`[[net.noresttherein.sugar.numeric.Decimal64.Round Round]]`.`[[net.noresttherein.sugar.numeric.Decimal64.Round.ExtendedExact ExtendedExact]]
  *      mode instead. The 'extended' rounding modes defined in
  *      [[net.noresttherein.sugar.numeric.Decimal64$ Decimal64]]`.`[[net.noresttherein.sugar.numeric.Decimal64.Round Round]]
  *      specify this behaviour, with [[net.noresttherein.sugar.numeric.Decimal64.Round.Extended Extended]]
  *      (equal to [[net.noresttherein.sugar.numeric.Decimal64.Round.ExtendedHalfEven ExtendedHalfEven]])
  *      being the default mode used if no `MathContext` is provided, explicitly or implicitly.
  *   1. Some secondary methods, most notably [[net.noresttherein.sugar.numeric.Decimal64.round round]],
  *      have different signatures and slightly different semantics than their official counterparts,
  *      mostly due to the difference from the point above.
  *   1. Among additional methods not available for its alternatives, there are dedicated ''operationExact'' methods
  *      which behave as Scala/Java `BigDecimal` with an [[java.math.MathContext.UNLIMITED UNLIMITED]] context.
  *   1. All operations for which this class does not throw an exception, with the provision for the mentioned
  *      slight differences in signatures and `MathContext` handling, will return values equal (nominally,
  *      not in terms of the `equals` method) as both `BigDecimal`s would.
  *      The only exception is [[net.noresttherein.sugar.numeric.Decimal64.pow pow]], which doesn't use `UNLIMITED`
  *      precision as both `BigDecimal`s, but 'best possible', as described above.
  *
  * The advantage over those standard implementations is that it is erased at runtime to a `Long` value, requiring
  * no heap allocation, unless used as an abstract type (provided as a type parameter to a generic method),
  * put into a collection or otherwise forcing a promotion to a reference representation, as specified by the SLS.
  * This makes it considerably more efficient.
  *
  * The difference from the standard `Double` type is that
  * the latter uses powers of 2 instead of 10, making this class more suitable for 'human oriented' values which
  * must meet exact precision requirements, typically defined in terms of the decimal notation.
  *
  * All method of this class which return another `Decimal64` can throw an [[ArithmeticException]]
  * unless stated differently.
  * @author Marcin Mościcki
  **/
@SerialVersionUID(Ver)
class Decimal64 private (private val bits :Long)
	extends AnyVal with ScalaNumericAnyConversions with Serializable
{
	/** The raw binary representation of this value. */
	@inline def toLongBits :Long = bits

	/** The value of this `Decimal64` with the decimal fraction point shift right
	  * by [[net.noresttherein.sugar.numeric.Decimal64.scale scale]].
	  * It is `x` in the `x * e^10` underlying representation of this value.
	  * If this decimal contains no more than `-`[[net.noresttherein.sugar.numeric.Decimal64.MinScale MinScale]] zeros,
	  * then `unscaled` has no trailing zeros; otherwise the excess over that number will form the lowest digits
	  * of `unscaled`. The range of values allowed in this implementation is
	  * `[`[[net.noresttherein.sugar.numeric.Decimal64.MinUnscaled MinUnscaled]]..[[net.noresttherein.sugar.numeric.Decimal64.MaxUnscaled MaxUnscaled]]`]`).
	  * @see [[net.noresttherein.sugar.numeric.Decimal64.scale scale]]
	  * @see [[net.noresttherein.sugar.numeric.Decimal64.significand coefficient]]
	  **/
	@inline private def unscaled :Long = bits >> ScaleBits

	/** The scale of this `Decimal64`, that is the number of fractional digits in the decimal representation
	  * of this number (if this value has a fractional part), or the negated number of zeros following
	  * [[net.noresttherein.sugar.numeric.Decimal64.unscaled unscaled]] in the decimal representation
	  * of this number (for whole numbers).
	  * Note that, as the `unscaled` can contain trailing zeros if scale is negative, it does not represent
	  * the highest power of ten divisor of this value.
	  *
	  * The scale range in this implementation is `[-128..127]`
	  * (`[`[[net.noresttherein.sugar.numeric.Decimal64.MinScale MinScale]]..[[net.noresttherein.sugar.numeric.Decimal64.MaxScale MaxScale]]`]`).
	  * The scale of zero is zero.
	  **/ //the implementation typically immediately negates it to get the exponent, as it's more intuitive that way
	@inline private def scale :Int = (bits << SignificandBits >> SignificandBits).toInt

	/** The [[net.noresttherein.sugar.numeric.Decimal64.unscaled unscaled]] value of this decimal divided
	  * by its maximal power of ten divisor.
	  * @see [[net.noresttherein.sugar.numeric.Decimal64.exponent exponent]]
	  **/
	def significand :Long =
		if (bits == 0L) 0
		else scale match {
			case Decimal64.MinScale => unscaled match {
				case m if (m & 1L) != 0L || m % 10L != 0L => m
				case m => m / LongPowersOf10(Decimal64.trailingZeros(m / 10L) + 1)
			}
			case _ => unscaled
		}

	/** The number of trailing zeros in the decimal representation of this number (for whole numbers),
	  * or its negative number of fractional digits (for numbers with a fractional part).
	  * For all decimal `x` the following is mathematically true:
	  * `x = x.significand * 10^x.exponent`, where `e^n` stands for the `n`-th power of `e`.
	  * Note that this is not the same as `-`[[net.noresttherein.sugar.numeric.Decimal64.scale scale]],
	  * as the latter does not take into account
	  * that [[net.noresttherein.sugar.numeric.Decimal64.unscaled unscaled]] may have trailing zeros.
	  * Returns `0` if this number equals zero.
	  **/
	def exponent :Int = scale match {
		case Decimal64.MinScale => unscaled match { //unscaled may have trailing zeros
			case m if (m & 1L) != 0L || m % 10L != 0L => -Decimal64.MinScale
			case m => Decimal64.trailingZeros(m / 10L) + 1 - Decimal64.MinScale
		}
		case n => -n //no trailing zeros in unscaled
	}

	/** Number of digits in [[net.noresttherein.sugar.numeric.Decimal64.unscaled unscaled]].
	  * This counts also any trailing zeros in excess over 127. Returns `1` if `this == 0`.
	  **/
	@inline private def unscaledDigits :Int = Decimal64.precision(java.lang.Math.abs(unscaled))

	/** The number of decimal digits in [[net.noresttherein.sugar.numeric.Decimal64.significand significand]],
	  * that is the number of digits in this decimal, not counting trailing zeros (on whole numbers).
	  * Note that this is different from [[net.noresttherein.sugar.numeric.Decimal64.unscaledDigits unscaledDigits]],
	  * in that the latter counts trailing zeros in [[net.noresttherein.sugar.numeric.Decimal64.unscaled unscaled]].
	  * it complements well the [[net.noresttherein.sugar.numeric.Decimal64.fractionalDigits fractionalDigits]] property,
	  * which likewise deals on the more abstract level of the actual value, rather than its binary representation.
	  **/
	def precision :Int = Decimal64.precision(java.lang.Math.abs(significand))

	/** The total number of decimal digits in this value, both fractional and whole. Returns `1` if `-1 < this < 1`.
	  * @return a value equal to `wholeDigits + fractionalDigits`.
	  * @see [[net.noresttherein.sugar.numeric.Decimal64.wholeDigits]]
	  * @see [[net.noresttherein.sugar.numeric.Decimal64.fractionalDigits]]
	  **/
	def digits :Int = unscaledDigits - (scale max 0)

	/** The number of decimal digits before the fractional point.
	  * Returns `1` if `-9 < this < 9` (in particular, for proper fractions).
	  * @see [[net.noresttherein.sugar.numeric.Decimal64.digits]]
	  * @see [[net.noresttherein.sugar.numeric.Decimal64.fractionalDigits]]
	  **/
	def wholeDigits :Int = unscaledDigits - scale min 0

	/** Number of fractional digits in this value's decimal representation with no trailing zeros.
	  * @see [[net.noresttherein.sugar.numeric.Decimal64.precision]]
	  * @see [[net.noresttherein.sugar.numeric.Decimal64.exponent]]
	  * @return `-exponent min 0`.
	  **/
	@inline def fractionalDigits :Int = -exponent min 0

	/** The sign of this decimal: `-1` for negative numbers, `1` for positive and `0` if `this == 0`.
	  * This method does not throw any exceptions.
	  **/
	@inline def signum :Int = (bits >> 63 | (~bits & -bits) >>> 63).toInt

	/** The sign of this decimal: `-1` for negative numbers, `1` for positive and `0` if `this == 0`.
	  * This method does not throw any exceptions.
	  **/
	@inline def sign :Decimal64 = new Decimal64(signum << ScaleBits)

	/** The absolute value of this number.
	  * @see [[net.noresttherein.sugar.numeric.Decimal64.minusAbs]]
	  **/
	@throws[ArithmeticException]("if this is the minimal (negative) representable Decimal64 and overflow occurs.")
	def abs :Decimal64 = {
		if ((bits & SignificandMask) == Long.MinValue) //unscaled == MinUnscaled
			throw SugaredArithmeticException("Unscaled overflow: abs(" + this + ").")
		new Decimal64((bits & ~bits >> 63 | (-bits & bits >> 63)) & SignificandMask | bits & ScaleMask)
	}

	/** Overflow safe `-this.abs`. Due to details of binary encoding, just as with inbuilt value types,
	  * `Decimal64` with a significand of [[net.noresttherein.sugar.numeric.Decimal64.MinUnscaled MinUnscaled]]
	  * does not have a positive counterpart. For these values, [[net.noresttherein.sugar.numeric.Decimal64.abs abs]]
	  * will throw an [[ArithmeticException]]. It is thus better to compare negated absolute values,
	  * as negation of every positive decimal has a `Decimal64` representation. This method is also more efficient
	  * than `abs`.
	  **/
	def minusAbs :Decimal64 =
		new Decimal64((bits >> 63) & bits | (~bits >> 63) & (-(bits & SignificandMask) | bits & ScaleMask))

	/** The opposite value of this number. */ //todo: check if opposite is the proper English term.
	@throws[ArithmeticException]("if this is the minimal (negative) representable Decimal64 and overflow occurs.")
	def unary_- :Decimal64 = {
		if ((bits & SignificandMask) == Long.MinValue) //unscaled == MinUnscaled
			throw SugaredArithmeticException("Unscaled overflow: " + toString.substring(1) + ".")
		new Decimal64(-(bits & SignificandMask) | bits & ScaleMask)
	}

	/** The order of magnitude of this number.
	  * It is the largest non negative power of 10 not greater than `this.abs`, or `0` if `-1 < this < 1`.
	  * This method does not throw any exceptions.
	  **/
	def magnitude :Decimal64 =
		if (bits == 0L) this
		else significand match {
			case 1L | -1L if exponent >= 0 => this //we are a power of 10
			case _ => Decimal64.make(1L, 1 - wholeDigits)
		}

	/** Moves the decimal dot right by the number of digits in the fractional part of this decimal, if non-negative.
	  * If this value is whole (an exact integer), return `this`.
	  * Together with [[net.noresttherein.sugar.numeric.Decimal64.denominator denominator]] it presents
	  * a decimal fraction in the form of `[w].[f]`, where `w` and `f` are strings of digits, as `[w][f]/10^n` where `n`
	  * is the number of digits in the fractional part. In the latter notation, `[w][f] == this.numerator`
	  * and `n == this.denominator`.
	  *
	  * This method does not throw any exceptions.
	  * @see [[net.noresttherein.sugar.numeric.Decimal64.toRatio]]
	  * @see [[net.noresttherein.sugar.numeric.Decimal64.toIntRatio]]
	  **/
	def numerator :Decimal64 =
		if ((bits & ScaleSign) != 0L) this
		else Decimal64(bits & SignificandMask) //set exponent to zero

	/** If this decimal number has `n >= 0` digits in its fractional part, return `10^n`; otherwise return `1`.
	  * If this value is whole (an exact integer), return `1`.
	  * Together with [[net.noresttherein.sugar.numeric.Decimal64.numerator numerator]] it presents a decimal fraction
	  * in the form of `[w].[f]`, where `w` and `f` are strings of digits, as `[w][f]/10^n` where `n` is the number
	  * of digits in the fractional part. In the latter notation, `[w][f] == this.numerator` and `n == this.denominator`.
	  *
	  * This method does not throw any exceptions.
	  * @see [[net.noresttherein.sugar.numeric.Decimal64.toRatio]]
	  * @see [[net.noresttherein.sugar.numeric.Decimal64.toIntRatio]]
	  **/
	def denominator :Decimal64 =
		if ((bits & ScaleSign) != 0L) One
		else PowersOf10(-(bits & ScaleMask).toInt)

	/** Calculates `1 / this` to the precision defined by an implicit [[java.math.MathContext MathContext]]
	  * (defaults to [[net.noresttherein.sugar.numeric.Decimal64.Round.Extended Extended]], which is the maximum
	  * precision with which the significand of the result can fit in 54 bits).
	  **/
	@throws[ArithmeticException]("if significand overflow/underflow occurs.")
	@inline def reciprocal(implicit mode :MathContext = Extended) :Decimal64 = One / this

	/** True if this number has a non-zero fractional part. */
	@inline def isFraction :Boolean = !isWhole

	/** True if this number's [[net.noresttherein.sugar.numeric.Decimal64.whole whole]] part is zero.
	  * Note that this will return `true` also for `Zero`.
	  **/
	@inline def isProper   :Boolean = whole == Zero

	/** True if this number's fractional part is zero. */
	@inline override def isWhole :Boolean = (bits & ScaleSign) != 0L

	/** The fractional part o this decimal number. For negative values, it is equal to `this.abs.fraction`.
	  * This method does not throw any exceptions.
	  * @return a number equal to `this - `[[net.noresttherein.sugar.numeric.Decimal64.whole whole]].
	  **/
	def fraction :Decimal64 = this - whole

	/** The integral part of this decimal number. For negative values, it is equal to `this.abs.whole`
	  * (the method is rounding down). This method does not throw any exceptions.
	  **/
	def whole :Decimal64 = scale match {
		case n if n <= 0 => this            //no fractional digits
		case n if n >= MaxPrecision => Zero //fractional point further to the left than there can be digits in unscaled
		case n => Decimal64(unscaled / LongPowersOf10(n)) //note that the quotient may now have trailing zeros
	}

	/** The highest integral (whole) number not greater than this value, as a `Decimal64`. */
	@throws[ArithmeticException]("if underflow occurs.")
	def floor :Decimal64 =
		if (isWhole) this
		else if (bits >= 0) whole
		else whole - 1

	/** The lowest integral (whole) number not lesser than this value, as a `Decimal64`. */
	@throws[ArithmeticException]("if overflow occurs.")
	def ceiling :Decimal64 =
		if (isWhole) this
		else if (bits >= 0) whole + 1
		else whole

	/** This decimal number rounded towards lesser absolute value.
	  * For negative values, this method is equivalent to [[net.noresttherein.sugar.numeric.Decimal64.ceiling ceiling]];
	  * for non-negative, the result equals [[net.noresttherein.sugar.numeric.Decimal64.floor floor]].
	  * This method does not throw any exceptions.
	  * @return `this.`[[net.noresttherein.sugar.numeric.Decimal64.whole whole]].
	  **/
	@throws[ArithmeticException]("if underflow occurs.")
	def roundDown :Decimal64 = whole //round(0, DOWN)

	/** This decimal number rounded towards greater absolute value.
	  * For negative values, this method is equivalent to [[net.noresttherein.sugar.numeric.Decimal64.floor floor]];
	  * for non-negative, the result equals [[net.noresttherein.sugar.numeric.Decimal64.ceiling ceiling]].
	  * @return [[net.noresttherein.sugar.numeric.Decimal64.round(fractional:Int,rounding:RoundingMode* round]]`(0, UP)`.
	  **/
	@throws[ArithmeticException]("if overflow occurs.")
	def roundUp   :Decimal64 = round(0, UP)

	/** This number with its fractional part rounded according to the specified mode. */
	@throws[ArithmeticException]("if overflow or underflow occurs.")
	def round(rounding :RoundingMode) :Decimal64 = round(0, rounding)

	/** This number with its fractional part rounded according to the specified mode. */
	@throws[ArithmeticException]("if overflow or underflow occurs.")
	def round(rounding :BigDecimal.RoundingMode.Value) :Decimal64 =
		round(0, RoundingMode.valueOf(rounding.id))

	/** This number rounded to the given number of digits ''after the decimal point''.
	  * Rounding happens according to the default [[java.math.RoundingMode.HALF_EVEN HALF_EVEN]] mode.
	  **/
	@throws[IllegalArgumentException]("if the specified number of fractional digits is negative.")
	@throws[ArithmeticException]("if overflow or underflow occurs.")
	def round(fractional :Int) :Decimal64 = round(fractional, HALF_EVEN)

	/** This number rounded to the given number of digits ''after the decimal point''.
	  * Rounding happens according to the given enum value.
	  **/
	@throws[IllegalArgumentException]("if the specified number of fractional digits is negative.")
	@throws[ArithmeticException]("if underflow or overflow occurs.") //consider: accept negative values
	def round(fractional :Int, rounding :BigDecimal.RoundingMode.Value) :Decimal64 =
		round(fractional, RoundingMode.valueOf(rounding.id))

	/** This number rounded to the given number of digits ''after the decimal point''.
	  * Rounding happens according to the given enum value.
	  **/
	@throws[IllegalArgumentException]("if the specified number of fractional digits is negative.")
	@throws[ArithmeticException]("if underflow or overflow occurs.") //consider: accept negative values
	def round(fractional :Int, rounding :RoundingMode) :Decimal64 = {
		if (fractional < 0)
			illegal_!("Negative fractional digits: " + fractional + ".")
		val m = unscaled; val s = scale
		if (s <= fractional)
			this
		else {
			val precision = Decimal64.precision(Math.abs(m))
			if (s - precision >= fractional)
				Zero
			else  //fractional < scale < precision + fractional
				Decimal64.divideToMaxPrecision(m, LongPowersOf10(s - fractional), -fractional, rounding)
		}
	}

	/** Rounds this number to `mode.`[[java.math.MathContext.getPrecision getPrecision]] number of leading digits,
	  * whole or fractional, and according to `mode.`[[java.math.MathContext.getRoundingMode getRoundingMode]].
	  * If no implicit [[java.math.MathContext MathContext]] is specified,
	  * [[net.noresttherein.sugar.numeric.Decimal64.Round.Extended Extended]] context is used, which rounds
	  * to maximum possible precision for this value. Note that the behaviour of this method
	  * is different than `round(mode.getPrecision, mode.getRoundingMode)`, as the latter rounds the fractional part
	  * only (including leading zeros introduced by the exponent) to the given precision, while this method
	  * simply reduces the significand to at most `mode.getPrecision` digits, regardless of the `exponent`.
	  **/
	@throws[ArithmeticException]("if overflow or underflow occurs.")
	def round(implicit mode: MathContext = Extended): Decimal64 =
		toPrecision(mode.getPrecision, mode.getRoundingMode)

	/** Rounds this number to the given precision, using the default [[java.math.RoundingMode.HALF_EVEN HALF_EVEN]] mode.
	  * The decimal expansion of the result equals the decimal expansion of this number on the leading `precision - 1`
	  * digits, and the immediately following digit is within `1` of the digit at `precision - 1` in this number,
	  * depending on the value of the remaining digits in this number. All digits following the first `precision`
	  * digits are zero.
	  *
	  * Passing zero is equivalent to specifying a maximum precision and thus has no effect.
	  **/
	@throws[IllegalArgumentException]("if the precision is negative.")
	@throws[ArithmeticException]("if overflow or underflow occurs.")
	def toPrecision(precision :Int) :Decimal64 = toPrecision(precision, HALF_EVEN)

	/** Rounds this value to the given precision and according to the given mode.
	  * The result has at most `precision` non zero leading digits. It's decimal expansion equals the decimal expansion
	  * of this number on the first `precision-1` digits, and is within one ''unit in the last place'' of this number.
	  *
	  * A precision of zero is equivalent to specifying a maximum precision and thus remains this value unchanged.
	  **/
	@throws[IllegalArgumentException]("if the precision is negative.")
	@throws[ArithmeticException]("if overflow or underflow occurs.")
	def toPrecision(precision :Int, rounding :BigDecimal.RoundingMode.Value) :Decimal64 =
		toPrecision(precision, RoundingMode.valueOf(rounding.id))

	/** Rounds this value to the given precision and according to the given mode.
	  * The result has at most `precision` non zero leading digits. It's decimal expansion equals the decimal expansion
	  * of this number on the first `precision-1` digits, and is within one ''unit in the last place'' of this number.
	  *
	  * A precision of zero is equivalent to specifying as maximum precision and thus remains this value unchanged.
	  **/
	@throws[IllegalArgumentException]("if the precision is negative.")
	@throws[ArithmeticException]("if overflow or underflow occurs.")
	def toPrecision(precision :Int, rounding :RoundingMode) :Decimal64 =
		if (bits == 0L || precision == ExtendedPrecision || precision >= this.precision)
			this
		else  //won't throw as it can be represented with the same precision and exponent as this
			Decimal64(unscaled, scale, rounding, precision)



	@inline def + (that :Decimal64)(implicit mode :MathContext = Extended) :Decimal64 = plus(that, mode)
	@inline def - (that :Decimal64)(implicit mode :MathContext = Extended) :Decimal64 = minus(that, mode)
	@inline def * (that :Decimal64)(implicit mode :MathContext = Extended) :Decimal64 = times(that, mode)
	@inline def / (that :Decimal64)(implicit mode :MathContext = Extended) :Decimal64 = div(that, mode)
	@inline def % (that :Decimal64)(implicit mode :MathContext = Extended) :Decimal64 = rem(that, mode)
	@inline def /~(that :Decimal64)(implicit mode :MathContext = Extended) :Decimal64 = quot(that, mode)
	@inline def **(exponent :Int)(implicit mode :MathContext = Extended)   :Decimal64 = pow(exponent, mode)
	@inline def ^ (exponent :Int)(implicit mode :MathContext = Extended)   :Decimal64 = pow(exponent, mode)
	@inline def >>(n :Int) :Decimal64 = movePointLeft(n)
	@inline def <<(n :Int) :Decimal64 = movePointRight(n)
	@inline def /%(that :Decimal64)(implicit mode :MathContext = Extended) :(Decimal64, Decimal64) = divAndRem(that)

	def exactPlus(that :Decimal64) :Decimal64 = plus(that, ExtendedExact)
	def plus(that :Decimal64) :Decimal64 = plus(that, Extended)
	def plus(that :Decimal64, rounding :RoundingMode) :Decimal64 = plus(that, toMaxDigits(rounding))
	def plus(that :Decimal64, rounding :BigDecimal.RoundingMode.Value) :Decimal64 =
		plus(that, RoundingMode.valueOf(rounding.id))

	def plus(that :Decimal64, mode :MathContext) :Decimal64 =
		try {
			if (that.bits == 0L)
				round(mode)
			else if (bits == 0L)
				that.round(mode)
			else if (scale <= that.scale)
				Decimal64.sum(unscaled, -scale, that.unscaled, -that.scale, mode.getRoundingMode, mode.getPrecision)
			else
				Decimal64.sum(that.unscaled, -that.scale, unscaled, -scale, mode.getRoundingMode, mode.getPrecision)
		} catch {
			case e :Exception => throwArithmeticException(this, " + ", that, mode, e)
		}

	def exactMinus(that :Decimal64) :Decimal64 = minus(that, ExtendedExact)
	def minus(that :Decimal64) :Decimal64 = minus(that, Extended)
	def minus(that :Decimal64, rounding :RoundingMode) :Decimal64 = minus(that, toMaxDigits(rounding))
	def minus(that :Decimal64, rounding :BigDecimal.RoundingMode.Value) :Decimal64 =
		minus(that, RoundingMode.valueOf(rounding.id))

	def minus(that :Decimal64, mode :MathContext) :Decimal64 =
		try {
			if (that.bits == 0L)
				round(mode)
			else if (bits == 0L)
				if (that.unscaled == MinUnscaled)
					mode.getPrecision match {
						case ExtendedPrecision =>
							Decimal64.divideToMaxPrecision(
								-MinUnscaled, 10L, 1 - that.scale, mode.getRoundingMode
							)
						case n if n > Precision => throw PrecisionExceededException
						case n =>
							val delta = MaxPrecision - n
							val roundingPower = LongPowersOf10(delta)
							val exponent = delta - that.scale
							Decimal64.divideAndScale(-MinUnscaled, roundingPower, exponent, mode.getRoundingMode)
					}
				else
					(-that).round(mode)
			else if (scale <= that.scale)
				Decimal64.sum(unscaled, -scale, -that.unscaled, -that.scale, mode.getRoundingMode, mode.getPrecision)
			else
				Decimal64.sum(-that.unscaled, -that.scale, unscaled, -scale, mode.getRoundingMode, mode.getPrecision)
		} catch {
			case e :Exception => throwArithmeticException(this, " - ", that, mode, e)
		}



	def exactTimes(that :Decimal64) :Decimal64 = times(that, ExtendedExact)
	def times(that :Decimal64) :Decimal64 = times(that, Extended)
	def times(that :Decimal64, rounding :RoundingMode) :Decimal64 = times(that, toMaxDigits(rounding))
	def times(that :Decimal64, rounding :BigDecimal.RoundingMode.Value) :Decimal64 =
		times(that, RoundingMode.valueOf(rounding.id))

	def times(that :Decimal64, mode :MathContext) :Decimal64 = try {
		var m1 = unscaled; var m2 = that.unscaled
		val sign = if ((m1 >= 0) == (m2 >= 0)) 1L else -1L
		if (m1 < 0)
			m1 = -m1
		if (m2 < 0)
			m2 = -m2
		var e1 = -scale; var e2 = -that.scale
		var e = -scale - that.scale
		val targetPrecision = mode.getPrecision
		if (m1 == 0L | m2 == 0L)                            //trivial cases of multiplications by 0 or a power of 10
			Zero
		else if (m1 == 1L && sign * m2 != -MinUnscaled) {
			//unscaled may have trailing zeros: fast path only if we don't need to normalize it.
			if ((targetPrecision == ExtendedPrecision | targetPrecision >= MaxPrecision) & (e1 >= 0 | e > MaxExponent))
				Decimal64.validate(sign * m2, -e)
			else
				Decimal64(sign * m2, -e, mode)
		} else if (m2 == 1L && sign * m1 != -MinUnscaled) {
			if ((targetPrecision == ExtendedPrecision | targetPrecision >= MaxPrecision) & (e2 >= 0 | e > MaxExponent))
				Decimal64.validate(sign * m1, -e)
			else
				Decimal64(sign * m1, -e, mode)
		} else {
			val Powers = LongPowersOf10                     //normalize m1 and m2 so that m1, m2 > 0 && m1, m2 % 10 != 0
			if (e1 == MaxExponent & (m1 & 1L) == 0L) {      //fast checks to see if there is a chance of trailing zeros
				val zeros = Decimal64.trailingZeros(m1)
				m1 /= Powers(zeros)
				e1 += zeros
				e += zeros
			}
			if (e2 == MaxExponent & (m2 & 1L) == 0L) {      //fast checks to see if there is a chance of trailing zeros
				val zeros = Decimal64.trailingZeros(m2)
				m2 /= Powers(zeros)
				e2 += zeros
				e += zeros
			}
			if (m1 <= Long.MaxValue / m2)                   //the easy case, we can just multiply within Long
				Decimal64(sign * m1 * m2, -e, mode)         //Will throw an exception on overflow
			else {                                          //do the multiplication (Long, Long) * (Long, Long)
				/* A product of two n-digit numbers can have at most 2n-1 digits and, obviously,
				 * a sum of two 2n-1 digits can have at most 2n digits and is less than 2*10^2n.
				 * Thus, for n = LongPrecision, a sum of two such products is always less than Long.MaxValue.
				 * Split m1*m2 into two portions *at decimal digit boundary* so we can calculate precision.
				 */
				val rounding = mode.getRoundingMode
				val loLimit = Powers(LongPrecision / 2)     //LongPrecision is even so it's exactly a half
				val hi1 = m1 / loLimit; val lo1 = m1 % loLimit
				val hi2 = m2 / loLimit; val lo2 = m2 % loLimit
				//let lo hold the lower LongPrecision - 1 digits and hi the rest (effectively having exponent +LongPrecision)
				var lo = lo1 * lo2                          //exponent: e1 + e2; holds the lower LongPrecision-1 digits
				var hi = hi1 * hi2                          //exponent: e1 + e2 + LongPrecision
				val lo1hi2 = lo1 * hi2                      //exponent: e1 + e2 + LongPrecision / 2
				hi += lo1hi2 / loLimit
				lo += lo1hi2 % loLimit * loLimit            //*loLimit because its the exponent delta of hi2
				if (lo >= MaxLongPowerOf10) {
					hi += lo / MaxLongPowerOf10
					lo %= MaxLongPowerOf10
				}
				val lo2hi1 = lo2 * hi1
				hi += lo2hi1 / loLimit
				lo += lo2hi1 % loLimit * loLimit
				if (lo >= MaxLongPowerOf10) {
					hi += lo / MaxLongPowerOf10
					lo %= MaxLongPowerOf10
				}                                           //try to reduce to a single Long, if possible
				if (lo != 0L & hi != 0L & (lo & 1L) == 0L && lo % 10L == 0L) {
					val zeros = Decimal64.trailingZeros(lo / 10L) + 1
					val pow = Powers(zeros)
					lo = lo / pow + hi % pow * Powers(LongPrecision - zeros)
					hi /= pow
					e += zeros
				}
				if (lo == 0L)
					Decimal64(sign * hi, -e - LongPrecision, rounding, targetPrecision)
				else if (hi == 0L)
					Decimal64(sign * lo, -e, rounding, targetPrecision)
				else
					Decimal64.roundDoubleLong(sign * hi, sign * lo, -e, rounding, targetPrecision)
			}
		}
	} catch {
		case e :Exception => throwArithmeticException(this, " * ", that, mode, e)
	}


	def exactDiv(that :Decimal64) :Decimal64 = div(that, ExtendedExact)
	def div(that :Decimal64) :Decimal64 = div(that, Extended)
	def div(that :Decimal64, rounding :RoundingMode) :Decimal64 = div(that, toMaxDigits(rounding))
	def div(that :Decimal64, rounding :BigDecimal.RoundingMode.Value) :Decimal64 =
		div(that, RoundingMode.valueOf(rounding.id))

	/** Divides `this` by `that`, rounding to the requested precision according to `mode.getRoundingMode` mode.
	  * If `mode.getPrecision` is zero, the result will be rounded to `16` or `17` digits, depending whether
	  * its significand scaled to `17` digits falls within
	  * the `[`[[net.noresttherein.sugar.numeric.Decimal64.MinUnscaled MinUnscaled]]`, `[[net.noresttherein.sugar.numeric.Decimal64.MaxUnscaled MaxUnscaled]]`]`
	  * range. If `mode.getRoundingMode` equals `RoundingMode.UNNECESSARY` and the value cannot be represented exactly
	  * as a `Decimal64` an [[ArithmeticException]] is thrown.
	  *
	  * For values which fall within the precision of `Decimal64`, the result equals
	  * `this.`[[net.noresttherein.sugar.numeric.Decimal64.toJavaBigDecimal toJavaBigDecimal]]`.divide(that.toJavaBigDecimal, new MathContext(precision, mode.getRoundingMode))`,
	  * where `precision` equals `mode.getPrecision` if the latter is non zero,
	  * or the [[net.noresttherein.sugar.numeric.Decimal64.Round.maxPrecision maximal precision]] with which the result
	  * can be represented as a `Decimal64`.
	  **/
	@throws[ArithmeticException]("if that is zero, the result cannot be represented as a Decimal64 with the requested" +
	                             " precision or mode.getRoundingMode is UNNECESSARY and rounding is necessary.")
	def div(that :Decimal64, mode :MathContext) :Decimal64 =
		if (that.bits == 0L)
			throw SugaredArithmeticException("Division by zero.")
		else if (bits == 0L | that == One)
			round(mode)
		else if (that == MinusOne)
			-round(mode)
		else if (that.unscaled == 1L)
			//unscaled might have been denormalized with zeros
			Decimal64(unscaled, scale - that.scale, mode.getRoundingMode, mode.getPrecision)
		else if (that.unscaled == -1L) unscaled match {
			case MinUnscaled =>        //-MinUnscaled out of range, we must round
				//round this and then negate to avoid overflow: for this, we must inverse sign-aware rounding directions
				val rounding =  mode.getRoundingMode match {
					case FLOOR   => CEILING
					case CEILING => FLOOR
					case mode    => mode
				}
				val p = mode.getPrecision match {
					case 0 => Precision
					case n if n >= MaxPrecision =>
						throw SugaredArithmeticException(
							"Cannot represent " + this + " / " + that +
								(if (unscaled < 0L) " == " + -unscaled else " == -" + unscaled) +
								"e" + (scale - that.scale) + " as a Decimal64 with precision " + n + "."
						)
					case p => p
				}
				-Decimal64(unscaled, scale - that.scale, rounding, p)
			case n =>
				Decimal64(-n, scale - that.scale, mode.getRoundingMode, mode.getPrecision)
		} else
			divideNonTrivial(that, mode, false)


	def exactQuot(that :Decimal64) :Decimal64 = quot(that, ExtendedExact)
	def quot(that :Decimal64) :Decimal64 = quot(that, Extended)
	def quot(that :Decimal64, rounding :RoundingMode) :Decimal64 = quot(that, toMaxDigits(rounding))
	def quot(that :Decimal64, rounding :BigDecimal.RoundingMode.Value) :Decimal64 =
		quot(that, RoundingMode.valueOf(rounding.id))

	/** Performs integral division of `this` by `that`, i.e. returns a maximal in absolute magnitude integral `q`
	  * such that `this = q * that + r` where `r.abs < that.abs` and `r.signum == this.signum`.
	  * The result is rounded to the precision specified by `mode.getPrecision` according
	  * to the `mode.getRoundingMode` mode. If `mode.getPrecision` is `0`, then the result's significand will be rounded
	  * to 16 or 17 digits, depending on whether its scaled value falls within the
	  * `[`[[net.noresttherein.sugar.numeric.Decimal64.MinUnscaled MinUnscaled]]`, `[[net.noresttherein.sugar.numeric.Decimal64.MaxUnscaled MaxUnscaled]]`]`
	  * range. If `mode.getRoundingMode` equals `RoundingMode.UNNECESSARY` and the result cannot be represented precisely
	  * as a `Decimal64`, an [[ArithmeticException]] is thrown.
	  *
	  * For values which fall within the precision of `Decimal64`, the result equals
	  * `this`.[[net.noresttherein.sugar.numeric.Decimal64.toJavaBigDecimal toJavaBigDecimal]]`.divideToIntegralValue(that.toJavaBigDecimal, MathContext.`[[java.math.MathContext.UNLIMITED UNLIMITED]]`).`[[java.math.BigDecimal.round round]]`(new MathContext(precision, DOWN))`,
	  * where `precision` equals `mode.getPrecision` if non zero,
	  * or the [[net.noresttherein.sugar.numeric.Decimal64.Round.maxPrecision maximal precision]] with which the result
	  * can be represented as a `Decimal64`. Note that this means that, unlike in the `BigDecimal` integral division,
	  * the result ''may be inexact''. This method is consistent with
	  * [[net.noresttherein.sugar.numeric.Decimal64.div div]] and other binary arithmetic operations
	  * implemented by this class in terms how rounding is handled, but in the result inconsistent
	  * in how it compares to the corresponding method of `BigDecimal`. This departure from equivalency
	  * is caused by limited precision of this class, which would cause the method to fail
	  * with an `ArithmeticException` in a large number of cases, making this operation of comparatively limited utility.
	  * It is still possible to enforce exact calculations by specifying the rounding mode as `UNNECESSARY`
	  * or requesting a precision exceeding [[net.noresttherein.sugar.numeric.Decimal64.MaxPrecision MaxPrecision]].
	  **/
	@throws[ArithmeticException]("if that is zero, the result cannot be represented as a Decimal64 with the requested" +
	                             " precision or mode.getRoundingMode is UNNECESSARY and rounding is necessary.")
	def quot(that :Decimal64, mode :MathContext) :Decimal64 = //roundingMode from mode is ignored, only precision is taken into account
		if (that.bits == 0L)
			throw SugaredArithmeticException("Division by zero.")
		else if (bits == 0L | that == One)
			round(mode)
		else if (that == MinusOne)
			-round(mode)
		else if (minusAbs > that.minusAbs)
			Zero
		else
			divideNonTrivial(that, mode, true)


	/** Implementation of both `div` and `quot`, defaulted to after easy hanging fruits are checked.
	  * Setting the flag `isQuot` results in truncating the precision to whole digits, unless the requested
	  * precision is lesser, and overriding the rounding mode to DOWN.
	  * Neither dividend nor divisor should be zero.
	  **/
	private def divideNonTrivial(that :Decimal64, mode :MathContext, isQuot :Boolean) :Decimal64 =
		try {
			//1 / aEX = (1 / a)E-X
			//observation: format(m / m2) == format(m quot m2) + '.'  + format(m % m2 / m2)
			//idea: iteratively repeat the above transformation for the remainder until we have MaxPrecision digits
			val Powers = LongPowersOf10
			val m1 = unscaled
			val m2 = that.unscaled
			val e = that.scale - scale              //exponent of the result
			val m1minus = Decimal64.minusAbs(m1)
			val m2minus = Decimal64.minusAbs(m2)

			val precision1 = Decimal64.precision(-m1minus)
			val precision2 = Decimal64.precision(-m2minus)
			val resultDigits = {                    //total number of digits in the result, including trailing zeros
				val precisionDiff = precision1 - precision2
				val dividendDominates = precisionDiff match {
					case 0 => m1minus < m2minus
					case n if n > 0 => m1minus < m2minus * Powers(n)
					case n => m1minus * Powers(-n) < m2minus
				}
				if (dividendDominates) e + precisionDiff + 1
				else e + precisionDiff
			}
			val resultPrecision = mode.getPrecision match {
				case 0 => 0
				case p if isQuot & resultDigits < p => resultDigits
				case p => p
			}
			val underPrecision =                    //number of digits Long division is short of the requested precision
				if (resultPrecision > 0)
					resultPrecision - (resultDigits - e)
				else
					MaxPrecision - (resultDigits - e)

			val rounding = if (isQuot) DOWN else mode.getRoundingMode
			val sign = if ((m1 >= 0) == (m2 >= 0)) 1L else -1L
			var result = new Decimal64(-sign)       //an impossible result to use as an uninitialized marker

			if (underPrecision >= 0) {              //if underPrecision > 0, then m1 / m2 doesn't offer enough precision.
				/* Divide m1/m2 (i.e., m1/`divisor`), but before resorting to the slow, iterative
				 * `divideByDigits`, see if we can either multiply m1 by a power of 10, or divide m2 by such power,
				 * increasing the significand at the cost of the final exponent, so we can perform the division
				 * in one step. The loop will continue at most once: after deflating divisor,
				 * either inflation == 0 and we take the first branch, or divisor is not divisible by 10,
				 * and all non-deflating branches set the result to something other than -sign.
				 */
				var inflation = underPrecision      //exponent of a power of 10 by which we must multiply m1
				var divisor = m2
				var exponent = e
				//the loop will continue at most once: either log == inflation and we enter the first branch,
				//  or divisor is not divisible by 10 and we enter the last branch
				while (result.bits == -sign) {
					if (inflation == 0) {
						val res = Decimal64.divideLong(m1, divisor, rounding)
						//the number of digits is correct except if precision == ExtendedPrecision when it might be +1
						result = Decimal64(res, inflation - exponent, rounding, resultPrecision)

					} else if (inflation <= LongPrecision && -m1minus <= LongPowerBounds(inflation)) {
						val res = divideLong(m1 * Powers(inflation), divisor, rounding)
						result = Decimal64(res, inflation - exponent, rounding, resultPrecision)

					} else if ((divisor & 1L) == 0L && divisor % 10L == 0L) {
						//decrease the level of required dividend inflation and then try again
						var log = trailingZeros(divisor / 10L) + 1
						if (log > inflation)
							log = inflation
						divisor /= Powers(log)
						inflation -= log
						exponent -= log
					} else {
						result = divideByDigits(m1, divisor, exponent, rounding, resultPrecision)
					}
				}
			} else {                                //no fractional digits in the result
				/* Divide m1/m2 (i.e., `dividend` by `m2`), but before resorting to the slow, iterative
				 * `divideByDigits`, see if we can either multiply m2 by a power of 10, or divide m1 by that power,
				 * decreasing the significand at the cost of the final exponent, so that `Long` division
				 * leaves us with just the precision we need. The loop will continue at most once:
				 * all other branches set the result, and after deflation either deflation == 0
				 * and we enter the first branch, or dividend is not divisible by 10 any more and we must take
				 * any other, non-deflating one.
				 */
				var deflation = -underPrecision
				var dividend = m1
				var exponent = e
				while (result.bits == -sign) {
					//we don't include the case deflation == 0, but it's covered by the following one
					if (deflation <= LongPrecision && -m2minus <= LongPowerBounds(deflation)) {
						val res = divideLong(dividend, m2 * Powers(deflation), rounding)
						result = Decimal64(res, -deflation - exponent, rounding, resultPrecision)

					} else if ((dividend & 1L) == 0L && dividend % 10L == 0L) {
						//deflate the dividend so that the final result is lower (and perhaps we can inflate divisor
						// by enough to do the division in one step).
						var log = trailingZeros(dividend / 10L) + 1
						if (log > deflation)
							log = deflation
						deflation -= log
						dividend /= Powers(log)
						exponent += log
					} else {
						//don't use the deflated dividend, take the original values
						result = divideByDigits(m1, m2, e, rounding, resultPrecision)
					}
				}
			}

			if (isQuot) {
				val s = result.scale
				if (s > MaxPrecision)
					result = Zero
				else if (s > 0)
					result = Decimal64(result.unscaled / Powers(s))
			}
			result
		} catch {
			case e :Exception => throwArithmeticException(this, if (isQuot) " /~ " else " / ", that, mode, e)
		}



	def exactRem(that :Decimal64) :Decimal64 = rem(that, ExtendedExact)
	def rem(that :Decimal64) :Decimal64 = rem(that, Extended)
	def rem(that :Decimal64, rounding :RoundingMode) :Decimal64 = rem(that, toMaxDigits(rounding))
	def rem(that :Decimal64, rounding :BigDecimal.RoundingMode.Value) :Decimal64 =
		rem(that, RoundingMode.valueOf(rounding.id))

	/** Calculates the approximated remainder of an integral division of this number by `that`.
	  * The remainder is defined here as `this - this /~ that * that`, where the final subtraction is rounded
	  * according to the passed `MathContext`, while the intermediate integral division and multiplication
	  * are performed with maximum possible precision and the rounding mode specified in the given `MathContext`.
	  *
	  * Note that, as all above operations can yield inexact results, unless a suitably high precision or
	  * rounding mode `UNNECESSARY` is specified, this remainder may also be a rounded value.
	  * This differs from [[java.math.BigDecimal.remainder remainder]] method of [[java.math.BigDecimal BigDecimal]],
	  * which is always exact - or throws an `ArithmeticException`.
	  **/
	def rem(that :Decimal64, mode :MathContext) :Decimal64 = {
		val maxPrecision = toMaxDigits(mode.getRoundingMode)
		minus(quot(that, maxPrecision).times(that, maxPrecision), mode)
	}


	def exactDivAndRem(that :Decimal64) :(Decimal64, Decimal64) = divAndRem(that, ExtendedExact)

	def divAndRem(that :Decimal64) :(Decimal64, Decimal64) =
		divAndRem(that, Extended)

	def divAndRem(that :Decimal64, rounding :RoundingMode) :(Decimal64, Decimal64) =
		divAndRem(that, toMaxDigits(rounding))

	def divAndRem(that :Decimal64, rounding :BigDecimal.RoundingMode.Value) :(Decimal64, Decimal64) =
		divAndRem(that, RoundingMode.valueOf(rounding.id))

	def divAndRem(that :Decimal64, mode :MathContext) :(Decimal64, Decimal64) = {
		val q = quot(that, mode)
		(q, minus(q.times(that, mode)))
	}


	def exactPow(n :Int) :Decimal64 = pow(n, ExtendedExact)
	def pow(n :Int) :Decimal64 = pow(n, Extended)
	def pow(n :Int, rounding :RoundingMode) :Decimal64 = pow(n, toMaxDigits(rounding))
	def pow(n :Int, rounding :BigDecimal.RoundingMode.Value) :Decimal64 = pow(n, RoundingMode.valueOf(rounding.id))

	/** Raises this number to the given integral power.
	  *   - If `n` is zero, then [[net.noresttherein.sugar.numeric.Decimal64.One One]] is returned.
	  *   - If `n` is positive, then the algorithm performs recursive squaring, taking `O(log n)` time.
	  *   - If `n` is negative, then the result is that of `One / this ** -n`.
	  * All above operations happen with maximum precision, with the rounding specified in the argument `MathContext`,
	  * and only the final result is rounded to the requested precision.
	  * Because this procedure is composite in nature, rounding errors will accumulate. This, and the fact
	  * that [[java.math.BigDecimal BigDecimal]] may use higher precision for these calculations than what is available
	  * to `Decimal64`, make it impossible to duplicate the results of the former exactly,
	  * especially that the latter also guarantees accuracy only to two units in the last place.
	  * The results of the two implementations may thus differ on the last digit. The theoretical upper bound
	  * on the error is `log`_2_`n` ulps (units in the last place), but tests show results within a single ulp.
	  * For the same reason, if `mode.getRounding == UNNECESSARY`, then in some cases either of the algorithms
	  * may throw an [[ArithmeticException]], where other does not. However, for precisions lower than `16` and small
	  * exponents, this algorithm will be actually more precise, the more the lower the requested precision,
	  * due to how temporary precision is calculated by `BigDecimal`.
      **/
	@throws[IllegalArgumentException]("if n equals Int.MinValue.")
	@throws[ArithmeticException]("if this Decimal64 is zero and n is negative, " +
	                             "or if the result cannot be represented exactly as a Decimal64 with the specified precision.")
	def pow(n :Int, mode :MathContext) :Decimal64 = {
		if (n < 0)
			if (n == Int.MinValue)
				illegal_!(toString + " ** Int.MinValue")
			else mode.getRoundingMode match {
				case _ if mode.getPrecision > Precision => One.div(pow(-n, mode), mode)
				case UNNECESSARY => One.div(pow(-n, ExtendedExact), mode)
				case rounding => One.div(pow(-n, toMaxDigits(rounding)), mode)
			}
		else
			try {
				val rounding  = mode.getRoundingMode
				val precision = mode.getPrecision
				var mode1 = rounding match {
					case _ if precision > Precision => Round(precision, HALF_UP)
					case UNNECESSARY => ExtendedExact
					case _ => toMaxDigits(HALF_UP)
				}
				var mode2 = rounding match {
					case _ if precision > Precision => Round(precision, HALF_DOWN)
					case UNNECESSARY => ExtendedExact
					case _ => toMaxDigits(HALF_DOWN)
				}
				var res = One
				var i   = 32 - Integer.numberOfLeadingZeros(n)  //< 32 because exp >= 0
				while (i >= 0) {
					res = res.times(res, mode1)
					val m = mode1; mode1 = mode2; mode2 = m

					if ((n >> i & 1) == 1) {
						res = res.times(this, mode1)
						val m = mode1; mode1 = mode2; mode2 = m
					}
					i -= 1
				}
				res.round(mode)
			} catch {
				case e :ArithmeticException =>
					throw SugaredArithmeticException("Cannot calculate " + this + " ** " + n + ". " + e.getMessage)
						.initCause(e)
			}
	}


	def movePointLeft(n :Int) :Decimal64 = if (n == 0) this else Decimal64(unscaled, scale + n)
	def movePointRight(n :Int) :Decimal64 = if (n == 0) this else Decimal64(unscaled, scale - n)

	def until(end: Decimal64)(implicit mode :Optionally[MathContext]): Range.Partial[Decimal64, NumericRange.Exclusive[Decimal64]] =
		new Range.Partial(until(end, _))

	def until(end: Decimal64, step: Decimal64)(implicit mode :Optionally[MathContext]): NumericRange.Exclusive[Decimal64] =
		new NumericRange.Exclusive(this, end, step)(Decimal64AsIfIntegral(mode getOrElse Extended))

	def to(end: Decimal64)(implicit mode :Optionally[MathContext]): Range.Partial[Decimal64, NumericRange.Inclusive[Decimal64]] =
		new Range.Partial(to(end, _))

	def to(end: Decimal64, step: Decimal64)(implicit mode :Optionally[MathContext]): NumericRange.Inclusive[Decimal64] =
		new NumericRange.Inclusive(this, end, step)(Decimal64AsIfIntegral(mode getOrElse Extended))


//	@inline def ==(that: Long) :Boolean = (this compare that) == 0

	@inline def max(that :Decimal64) :Decimal64 = if (this >= that) this else that
	@inline def min(that :Decimal64) :Decimal64 = if (this <= that) this else that


	/** True if this `Decimal64` converts to a `Float` equal to its decimal representation,
	  * within maximum precision of this class.
	  * @return `Decimal64.`[[net.noresttherein.sugar.numeric.Decimal64.round(decimal:Float)* round]]`(this.`[[net.noresttherein.sugar.numeric.Decimal64.toFloat toFloat]]`)`
	  **/
	def isDecimalFloat :Boolean = scale match {
		case 0 => true
		case n if n < 0 & -n < ExactFloatPowersOf10 => isDecimalFloat(unscaled * FloatPowersOf10(-n))
		case n if n > 0 & n < ExactFloatPowersOf10  => isDecimalFloat(unscaled / FloatPowersOf10(n))
		case n =>
			val before = decimalString(1).toString
			val f = before.toFloat
			java.lang.Float.isFinite(f) && f == f && //NaN check
				(n >= 0 && { val whole = f.toLong; whole.toFloat == f && whole == toLong }) ||
				equal(before, java.lang.Float.toString(f))
	}

	/** True if this `Decimal64` converts to a `Double` equal to its decimal representation,
	  * within maximum precision of this class.
	  * @return `Decimal64.`[[net.noresttherein.sugar.numeric.Decimal64.round(decimal:Double)* round]]`(this.`[[net.noresttherein.sugar.numeric.Decimal64.toDouble toDouble]]`)`
	  **/
	def isDecimalDouble :Boolean = scale match {
		case 0 => true
		case n if n < 0 & -n < ExactDoublePowersOf10 => isDecimalDouble(unscaled * DoublePowersOf10(-n))
		case n if n > 0 & n < ExactDoublePowersOf10 => isDecimalDouble(unscaled / DoublePowersOf10(n))
		case n =>
			val before = decimalString(1).toString
			val d      = before.toDouble
			java.lang.Double.isFinite(d) && d == d && //NaN check
				(n >= 0 && { val whole = d.toLong; whole.toDouble == d && whole == toLong }) ||
				equal(before, java.lang.Double.toString(d))
	}

	@inline private def isDecimalFloat(f :Float) :Boolean =
		java.lang.Float.isFinite(f) && f == f && this == Decimal64.round(java.lang.Float.toString(f))

	@inline private def isDecimalDouble(d :Double) :Boolean =
		java.lang.Double.isFinite(d) && d == d && this == Decimal64.round(java.lang.Double.toString(d))

	@inline private def equal(thisString :String, otherString :String) :Boolean =
		thisString == otherString || otherString.lastIndexOf('E') >= 0 && this == Decimal64.round(otherString)

	/** True if this `Decimal64` holds, to the maximum precision of this class,
	  * the binary representation of a `Float` value.
	  **/
	def isBinaryFloat :Boolean = scale match {
		case 0 => true
		case n if n < 0 & -n < ExactFloatPowersOf10 => isBinaryFloat(unscaled * FloatPowersOf10(-n))
		case n if n > 0 & n < ExactFloatPowersOf10  => isBinaryFloat(unscaled / FloatPowersOf10(n))
		case n =>
			val f = decimalString(1).toString.toFloat
			java.lang.Float.isFinite(f) && f == f &&
				(n >= 0 && { val whole = f.toLong; whole.toFloat == f && whole == toLong }) ||
				this == Decimal64.round(new JavaBigDecimal(f)) 
	}

	/** True if this `Decimal64` holds, to the maximum precision of this class,
	  * the binary representation of a `Double` value.
	  **/
	def isBinaryDouble :Boolean = scale match {
		case 0 => true
		case n if n < 0 & -n < ExactDoublePowersOf10 => isBinaryDouble(unscaled * DoublePowersOf10(-n))
		case n if n > 0 & n < ExactDoublePowersOf10  => isBinaryDouble(unscaled / DoublePowersOf10(n))
		case n =>
			val d = decimalString(1).toString.toDouble
			java.lang.Double.isFinite(d) && d == d &&
				(n >= 0 && { val whole = d.toLong; whole.toDouble == d && whole == toLong }) ||
					this == Decimal64.round(new JavaBigDecimal(d)) 
	}
	@inline private def isBinaryFloat(f :Float) :Boolean =
		java.lang.Float.isFinite(f) && f == f && this == Decimal64.round(new JavaBigDecimal(f))

	@inline private def isBinaryDouble(d :Double) :Boolean =
		java.lang.Double.isFinite(d) && d == d && this == Decimal64.round(new JavaBigDecimal(d))

	/** True if this `Decimal64` is exactly equal to the closest `Float` value. */
	def isExactFloat :Boolean = {
		val res = exactFloat
		res == res
	}

	/** True if this `Decimal64` is exactly equal to the closest `Double` value. */
	def isExactDouble :Boolean = {
		val res = exactDouble
		res == res
	}

	private def exactFloat :Float =
		try {
			scale match {
				case 0 => 0.0f
				case n if n < 0 & -n < ExactFloatPowersOf10 => exactFloat(unscaled * FloatPowersOf10(-n))
				case n if n > 0 & n < ExactFloatPowersOf10  => exactFloat(unscaled / FloatPowersOf10(n))
				case n =>
					val f = decimalString(1).toString.toFloat
					val isExact =
						java.lang.Float.isFinite(f) && f == f &&
							(n >= 0 && { val whole = f.toLong; whole.toFloat == f && whole == toLong }) ||
							this == Decimal64(new JavaBigDecimal(f)) //todo: we really need methods which return a Maybe/Option
					if (isExact) f else Float.NaN
			}
		} catch {
			case _ :InternalException | _ :ArithmeticException => Float.NaN
		}
	private def exactDouble   :Double =
		try {
			scale match {
				case 0 => 0.0
				case n if n < 0 & -n < ExactDoublePowersOf10 => exactDouble(unscaled * DoublePowersOf10(-n))
				case n if n > 0 & n < ExactDoublePowersOf10  => exactDouble(unscaled / DoublePowersOf10(n))
				case n =>
					val d = decimalString(1).toString.toDouble
					val isExact =
						java.lang.Double.isFinite(d) && d == d &&
							(n >= 0 && { val whole = d.toLong; whole.toDouble == d && whole == toLong }) ||
							this == Decimal64(new JavaBigDecimal(d)) //todo: we really need methods which return a Maybe/Option
					if (isExact) d else Double.NaN
			}
		} catch {
			case _ :InternalException | _ :ArithmeticException => Double.NaN
		}

	@inline private def exactFloat(f :Float) :Float =
		if (java.lang.Float.isFinite(f) && f == f && this == Decimal64(f)) f
		else Float.NaN

	@inline private def exactDouble(d :Double) :Double =
		if (java.lang.Double.isFinite(d) && d == d && this == Decimal64(d)) d
		else Double.NaN


	override def isValidInt :Boolean =
		scale <= 0 && this >= Decimal64.MinInt && this <= Decimal64.MaxInt

	def isValidLong :Boolean =
		scale <= 0 && this >= MinLongValue && this >= MaxLongValue

	def isValidUInt :Boolean =
		(bits & Long.MinValue) == 0L && scale <= 0 && this <= Decimal64.MaxUInt

	def isValidULong :Boolean =
		(bits & Long.MinValue) == 0L && scale <= 0 && this <= Decimal64.MaxULong

	@inline override def toChar   :Char   = intValue.toChar
	@inline override def toByte   :Byte   = intValue.toByte
	@inline override def toShort  :Short  = intValue.toShort
	@inline override def toInt    :Int    = intValue
	@inline override def toLong   :Long   = longValue
	@inline override def toFloat  :Float  = floatValue
	@inline override def toDouble :Double = doubleValue

	def toUInt  :UInt  = new UInt(longValue.toInt)
	def toULong	:ULong = new ULong(longValue)

	def toBigInt :BigInt = whole.toBigIntExact

	def toBigInteger :BigInteger = whole.toBigIntegerExact

	def toIntRatio :IntRatio = toIntRatioExact
	def toRatio :Ratio = toRatioExact //Ratio(numerator.toLong, denominator.toLong)

	def toBigDecimal(implicit math :MathContext = Extended) :BigDecimal =
		new BigDecimal(toJavaBigDecimal, math)

	def toJavaBigDecimal(implicit math :MathContext = Extended) :JavaBigDecimal =
		if (bits == 0L) new JavaBigDecimal(0, math)
		else {
			val m = unscaled; val s = scale
			if (s > MinScale)
				new JavaBigDecimal(BigInteger.valueOf(m), scale, math)
			else {
				val log = Decimal64.trailingZeros(m)
				new JavaBigDecimal(BigInteger.valueOf(m / LongPowersOf10(log)), scale - log, math)
			}
		}

	def toByteExact :Byte =
		if (isValidByte) byteValue
		else throw SugaredArithmeticException("Decimal64 " + this + " is not a Byte.")

	def toShortExact :Short =
		if (isValidShort) shortValue
		else throw SugaredArithmeticException("Decimal64 " + this + " is not a Short.")

	def toIntExact :Int =
		if (isValidInt) intValue
		else throw SugaredArithmeticException("Decimal64 " + this + " is not an Int.")

	def toLongExact :Long =
		if (isValidLong) longValue
		else throw SugaredArithmeticException("Decimal64 " + this + " is not a Long.")

	def toFloatExact :Float = {
		val res = exactFloat
		if (res != res)
			throw SugaredArithmeticException("Decimal64 " + this + " is not a Float.")
		res
	}
	def toDoubleExact :Double = {
		val res = exactDouble
		if (res != res)
			throw SugaredArithmeticException("Decimal64 " + this + " is not a Double.")
		res
	}

	def toUIntExact :UInt =
		if (isValidUInt) toUInt
		else throw SugaredArithmeticException("Decimal64 " + this + " is not an UInt.")

	def toULongExact :ULong =
		if (isValidLong) toULong
		else throw SugaredArithmeticException("Decimal64" + this + " is not an ULong")

	
	def toBigIntExact :BigInt = BigInt(toBigIntegerExact)

	def toBigIntegerExact :BigInteger =
		if (scale <= 0) BigInteger.valueOf(unscaled).pow(-scale)
		else throw SugaredArithmeticException("Decimal64 " + this + " is not an integer.")

	def toIntRatioExact :IntRatio = IntRatio(numerator.toIntExact, denominator.toIntExact)
	def toRatioExact    :Ratio    = Ratio(numerator.toLongExact, denominator.toLongExact)

	def toBigDecimalExact :BigDecimal = toBigDecimal(toMaxDigits(UNNECESSARY))

	def toJavaBigDecimalExact :JavaBigDecimal = toJavaBigDecimal(toMaxDigits(UNNECESSARY))

	@inline override def byteValue  :Byte  = longValue.toByte
	@inline override def shortValue :Short = longValue.toShort
	@inline override def intValue   :Int   = longValue.toInt

	override def longValue :Long =
		if (bits == 0L)
			0L
		else scale match {
			case 0 => significand
			case n if n > 0 => unscaledDigits match {
				case digits if digits - n <= 0 => 0L
				case digits => unscaled / LongPowersOf10(digits - n) //digits <= MaxPrecision -> digits - n <= MaxPrecision
			}
			case n if n < -64 => 0L
			case n if -n <= LongPrecision =>
				unscaled * LongPowersOf10(-n) //overflows, but exactly the way we want
			//let m = a * 2^n; let M = Long.MaxValue; let P = 5^63 mod M
			//m*10^e mod M = a * 5^e * 2^(n + e) mod M = (a mod M) * (2^(n + e) mod M) * (5^e mod M)
			// = (a mod M) * 2^((n + e) mod 63) * 5^(e mod 63) * P*(e quot 63)
			case n => //todo: calculate it ourselves
				JavaBigDecimal.valueOf(unscaled, n).longValue
		}

	override def floatValue :Float = scale match {
		case 0 => unscaled.toFloat
		case n if n < 0 & -n < FloatPowersOf10.length => unscaled * FloatPowersOf10(-n)
		case n if n > 0 & n < FloatPowersOf10.length => unscaled / FloatPowersOf10(n)
		case _ => toString.toFloat
	}

	override def doubleValue :Double = scale match {
		case 0 => unscaled.toDouble
		case n if n < 0 & -n < DoublePowersOf10.length => unscaled * DoublePowersOf10(-n)
		case n if n > 0 & n < DoublePowersOf10.length => unscaled / DoublePowersOf10(n)
		case _ => toString.toDouble
	}

	//consider: should these be padded in the front?
	def format(precision :Int, scale :Int) :String = format(scale, Round(precision))
	def format(precision :Int, scale :Int, rounding :RoundingMode) :String = format(scale, Round(precision, rounding))
	def format(precision :Int, scale :Int, rounding :BigDecimal.RoundingMode.Value) :String =
		format(scale, Round(precision, rounding))

	def format(scale :Int, context :MathContext = Extended) :String = {
		if (scale < 0)
			illegal_!(
				"Cannot format " + this + " to a negative number of fractional digits: " + scale + "."
			)
		val rounded = round(context)
		val fraction = rounded.fractionalDigits
		if (fraction <= scale)
			rounded.decimalString(scale).toString
		else {
			val res = rounded.decimalString(0)
			res.delete(res.length, res.length - (fraction - scale)).toString
		}
	}
	def format(context :MathContext) :String =
		Decimal64.round(unscaled, -scale)(context).toString

	/** Formats this number as "''significand''e''exponent''". */
	def toFormalString :String = significand.toString + 'e' + exponent

	/** Formats this number as its full decimal expansion, with no trailing zeros
	  * (except for a single zero if this number is zero).
	  **/
	def toPlainString :String =
		if (scale == 0) java.lang.Long.toString(unscaled)
		else decimalString(0).toString

	private def decimalString(minScale :Int) :java.lang.StringBuilder = {
		val s = this.scale
		val m = unscaled
		val p = precision
		val signChars = if (m < 0) 1 else 0
		val trailingZeros = math.max(-s, 0)
		val scale  = math.max(minScale, s)
		val length =
			if (scale == 0) p + trailingZeros + signChars
			else if (p > s) p - s + 1 + scale + signChars
			else scale + 2 + signChars
		val res    = new java.lang.StringBuilder(length)
		if (s <= 0) {
			res append m
			var i = s
			while (i < 0) {
				res append '0'
				i += 1
			}
			if (minScale > 0) {
				res append '.'
				i = 0
				while (i < minScale) {
					res append '0'
					i += 1
				}
			}
		} else if (p > s) {
			val string = String.valueOf(m)
			res append string.substring(0, p - s + signChars)
			res append '.'
			res append string.substring(p - s + signChars)
			var i = s
			while (i < minScale) {
				res append '0'
				i += 1
			}
		} else {
			if (m < 0) res append "-0."
			else res append "0."
			var i = s
			while (i > p) {
				res append '0'
				i -= 1
			}
			if (m < 0) res append -m
			else res append m

		}
		res
	}

	/** Formats this number using an engineering notation of ''nEe'', where `e` is a multiple of three,
	  * and `n` is a number in the 0-999 range, or a decimal fraction with up to two leading zeros
	  * after the decimal point. If exponent is zero, the whole exponent part of the number is omitted.
	  **/
	def toEngineeringString :String = JavaBigDecimal.valueOf(significand, -exponent).toEngineeringString

	override def toString :String =
		if (bits == 0L)
			"0"
		else {
			var m = unscaled
			var e = -scale
			val zeros = trailingZeros(m)
			m /= LongPowersOf10(zeros)
			e += zeros
			val s = String.valueOf(m)
			val signChar = ((m & Long.MinValue) >>> 63).toInt
			val digits = s.length - signChar
			if (e >= 0)
				if (digits + e <= MaxDigitsInWholeString)
					if (e == 0) s else s + "0" * e
				else
					s.substring(0, 1 + signChar) + '.' + s.substring(1 + signChar) +
						PositiveExponentFormat + (e + digits - 1)
			else digits + e match {
				case 0 if m > 0 => "0." + s
				case 0          => "-0." + s.substring(1)
				case 1 if m > 0 => s.substring(0, 1) + '.' + s.substring(1)
				case 1          => s.substring(0, 2) + '.' + s.substring(2)
				case whole if whole > 0 && digits <= MaxDigitsInPlainString =>
					s.substring(0, signChar + whole)  + '.' + s.substring(signChar + whole)
				case whole if whole > 0 & m > 0 =>
					s.substring(0, 1) + '.'  + s.substring(1) + PositiveExponentFormat + (e + digits - 1)
				case whole if whole > 0 =>
					s.substring(0, 2) + '.'  + s.substring(2) + NegativeExponentFormat + (e + digits - 1)
				case zeros if digits <= MaxDigitsInPlainString & m > 0 =>
					"0." + "0" * -zeros + s
				case zeros if digits <= MaxDigitsInPlainString =>
					"-0." + "0" * -zeros + s.substring(1)
				case _ if m > 0 =>
					s.substring(0, 1)  + '.' + s.substring(1) +
						PositiveExponentFormat + (e + s.length - 1)
				case _ =>
					"-" + s.substring(1, 2) +'.' + s.substring(2) +
						NegativeExponentFormat + (e + s.length - 2)
			}
		}


	/** Writes the underlying 8 bytes to the stream. The first 7 bytes are the significand, highest byte first,
	  * and the 8-th byte is the scale (a negation of the exponent).
	  * @see [[net.noresttherein.sugar.numeric.Decimal64.read]]
	  **/
	def write(out :OutputStream) :Unit = {
		var i = 56
		while (i >= 0) {
			out.write((bits >> i).toInt)
			i -= 8
		}
	}

}






//consider: use Int.MaxValue for 'max possible precision', rather than 0.
// Using zero makes MathContext.UNLIMITED be interpreted as such, rather than really unlimited.
/** A factory of [[net.noresttherein.sugar.numeric.Decimal64! Decimal64]] numbers, additionally grouping constants
  * for various computation modes ([[java.math.MathContext MathContext]] instances).
  *
  * As a rule, all `apply` methods which do not accept a `MathContext` or a rounding mode do not perform any rounding;
  * if an argument (or arguments) cannot be represented precisely, an [[ArithmeticException]] is thrown, as if passed
  * [[net.noresttherein.sugar.numeric.Decimal64.Round Mode]]`.`[[net.noresttherein.sugar.numeric.Decimal64.Round.Exact Exact]]
  * as an argument. Those which accept only a [[java.math.RoundingMode RoundingMode]], without precision,
  * round the value to highest possible precision representable as a `Decimal64`.
  *
  * On the other hand, `round` methods express intent explicitly and thus accept an implicit `MathContext`
  * defining the precision and rounding direction; if no such implicit value exists,
  * [[net.noresttherein.sugar.numeric.Decimal64.Round Mode]]`.`[[net.noresttherein.sugar.numeric.Decimal64.Round.Extended Best]]
  * will be used, which rounds to the highest representable precision for the given significand.
  **/
@SerialVersionUID(Ver)
object Decimal64 {

	/** The maximal number such that all values of that many integral digits fit in the
	  * `[`[[net.noresttherein.sugar.numeric.Decimal64.MinUnscaled MinUnscaled]]`..`[[net.noresttherein.sugar.numeric.Decimal64.MaxUnscaled MaxUnscaled]]`]`
	  * range. It is the number of significant digits defined by the eponymous standard of `64` bit decimal values.
	  * Note that, depending on the rounding precision [[java.math.MathContext MathContext]] used,
	  * a `Decimal64` can contain a significand of `17` digits for some values.
	  * Even larger values can still be represented as a `Decimal64` as long as they consist solely
	  * of zero digits after first 16/17 digits.
	  * @see [[net.noresttherein.sugar.numeric.Decimal64.MaxUnscaled]]
	  * @see [[net.noresttherein.sugar.numeric.Decimal64.MinUnscaled]]
	  **/
	final val Precision = 16
	/** Maximum attainable precision by a `Decimal64` value. Not all values of that many significant digits
	  * can be represented exactly, only those whose absolute value
	  * is less or equal to [[net.noresttherein.sugar.numeric.Decimal64.MaxUnscaled MaxUnscaled]].
	  * Equal to [[net.noresttherein.sugar.numeric.Decimal64.Precision Precision]]` + 1`.
	  **/
	final val MaxPrecision = 17
	/** Precision value which signifies rounding to the maximum possible precision within a `Decimal64`
	  * for a particular value.
	  **/
	final val ExtendedPrecision = 0


	/** The maximum value of a significand within `Decimal64`'s precision.
	  * All `Long` values in range `[MinUnscaled, MaxUnscaled]` can be represented exactly as `Decimal64`.
	  **/ //the first digit is 3; this is helpful when determining a rounding direction
	final val MaxUnscaled  = 0x007fffffffffffffL
	/** The minimum (negative) value of a significand within `Decimal64`'s precision.
	  * All `Long` values in range `[MinUnscaled, MaxUnscaled]` can be represented exactly as `Decimal64`.
	  **/ //the first digit is 3; this is helpful when determining a rounding direction
	final val MinUnscaled  = 0xff80000000000000L

	/** Maximum power of 10 under extended `Decimal64` precision: `10^16`. */
	final val MaxPowerOf10 = 10000000000000000L

	/** The maximum scale (number of fractional digits in a `Decimal64`) within precision.
	  * It is a negation of the minimal (negative) exponent. */
	final val MaxScale     = 0x7f
	/** The minimum scale (number of fractional digits in a `Decimal64`) within precision.
	  * It is a negation of the maximal ''encoded'' exponent. It is still possible to represent values
	  * `a * 10^e` with a higher exponent as long as the significand can be scaled accordingly by adding trailing zeros. */
	final val MinScale     = 0xffffff80

	/** The maximum value that a `Decimal64` can take. Note that there are numbers between `MinValue` and `MaxValue`
	  * which cannot be represented as a `Decimal64`. */
	final val MaxValue :Decimal64 = new Decimal64(0x7fffffffffffff80L)
	/** The minimum value that a `Decimal64` can take. Note that there are numbers between `MinValue` and `MaxValue`
	  * which cannot be represented as a `Decimal64`. */
	final val MinValue :Decimal64 = new Decimal64(0x8000000000000080L)

	final val Zero     :Decimal64 = new Decimal64(0L)
	final val One      :Decimal64 = new Decimal64(0x100L)
	final val Ten      :Decimal64 = new Decimal64(0xa00L)
	private final val MinusOne    = new Decimal64(0x7fffffffffffff00L)

	/** The number of bits used by the significand (56). */
	final val SignificandBits = 56
	/** The number of bits used by the exponent/scale (8).  */
	final val ScaleBits = 8

	/** `Long` bitmask for the bits of the significand in a `Decimal64`. */
	private final val SignificandMask = 0xFFFFFFFFFFFFFF00L
	/** `Long` bitmask for the bits of the scale/exponent in a `Decimal64`. */
	private final val ScaleMask = 0xFFL

	/** A mask for the sign bit of the scale part of the binary `Long` format of a `Decimal64`. */
	private final val ScaleSign = 0x80L

	//does not take into account possible zeros in unscaled, hence private
	private final val MaxExponent = -MinScale
//	private final val MinExponent = -MaxScale

	/** Largest precision to which any number can be represented as a `Long` (`18`).
	  * It is the maximum power of `10` under `Long` precision. */
	private final val LongPrecision = 18
	/** Maximum possible number of digits a `Long` value can have (`19`). */
	private final val MaxLongPrecision = 19

	/** The exponent of the maximum Decimal64 representable exactly as a `Long`. */
	private final val MaxLongValueLog = 3L
	/** `10`^`MaxLongValueLog`^. */
	private final val MaxLongValueRounding = 1000L

	/** The largest `Decimal64` which can be represented exactly as a `Long`. All instances
	  * [[net.noresttherein.sugar.numeric.Decimal64.MinLongValue MinLongValue]]` <= decimal <= MaxLongValue`
	  * are valid `Long` values. There are, however, smaller `Long` values which cannot be represented as a `Decimal64`;
	  * [[net.noresttherein.sugar.numeric.Decimal64.MaxUnscaled MaxUnscaled]] is the largest `Long` such that
	  * all lesser values (greater or equal to [[net.noresttherein.sugar.numeric.Decimal64.MinUnscaled MinUnscaled]])
	  * are representable exactly as `Long` values.
	  **/
	final val MaxLongValue = new Decimal64(Long.MaxValue / MaxLongValueRounding << ScaleBits | -MaxLongValueLog & ScaleMask)

	/** The smallest (negative) `Decimal64` which can be represented exactly as a `Long`. All instances
	  * `MinLongValue <= decimal <= `[[net.noresttherein.sugar.numeric.Decimal64.MaxLongValue MaxLongValue]]
	  * are valid `Long` values. There are, however, larger `Long` values which cannot be represented as a `Decimal64`;
	  * [[net.noresttherein.sugar.numeric.Decimal64.MinUnscaled MinUnscaled]] is the smallest `Long` such that
	  * all greater values (lesser or equal to [[net.noresttherein.sugar.numeric.Decimal64.MaxUnscaled MaxUnscaled]])
	  * are representable exactly as `Long` values.
	  **/
	final val MinLongValue = new Decimal64(Long.MinValue / MaxLongValueRounding << ScaleBits | -MaxLongValueLog & ScaleMask)


	/** [[java.math.MathContext MathContext]] constants with various rounding modes and precision of 16 or maximum digits.
	  * Three distinct sets are defined:
	  *   1. standard, named after [[java.math.RoundingMode RoundingMode]] values,
	  *      which use a fixed precision of 16 digits. This in particular includes
	  *      [[net.noresttherein.sugar.numeric.Decimal64.Round.Standard Standard]].
	  *   1. extended precision, which will round a significand to the highest precision which can be encoded on 56 bits:
	  *      16 or 17 digits. They are named similarly to the above constants, but with an `Extended` prefix.
	  *      [[net.noresttherein.sugar.numeric.Decimal64.Round.Extended Extended]] precision itself
	  *      (rounding [[java.math.RoundingMode.HALF_EVEN HALF_EVEN]] is the default mode
	  *      used by [[net.noresttherein.sugar.numeric.Decimal64 Decimal64]] class methods when no `MathContext`
	  *      was provided.
	  *   1. [[net.noresttherein.sugar.numeric.Decimal64.Round.Exact Exact]] - a single rounding mode demanding
	  *      precise calculation. All methods passed it as an argument will throw an [[ArithmeticException]]
	  *      instead o rounding the result.
	  *
	  * Additionally, methods returning one of the above constants based on the rounding mode passed as an argument
	  * are provided for convenience.
	  **/
    @SerialVersionUID(Ver)
	object Round {
		private[this] val modes = Array.tabulate(18, RoundingMode.values.length) {
			(precision, rounding) => new MathContext(precision, RoundingMode.valueOf(rounding))
		}

		/** A `Decimal64` representation in extended precision, maximum for a given number in 56 bits
		  * (16 or 17 digits, depending on the significand). Results are rounded towards zero
		  * and no [[ArithmeticException]] is thrown if a value does not fit in this precision. */
		final val ExtendedDown     = modes(0)(DOWN.ordinal)//new MathContext(0, DOWN)

		/** A `Decimal64` representation in extended precision, maximum for a given number in 56 bits
		  * (16 or 17 digits, depending on the significand). Results are rounded away from zero
		  * and no [[ArithmeticException]] is thrown if a value does not fit in this precision. */
		final val ExtendedUp       = modes(0)(UP.ordinal) //new MathContext(0, UP)

		/** A `Decimal64` representation in extended precision, maximum for a given number in 56 bits
		  * (16 or 17 digits, depending on the significand). Results are rounded to the lesser neighbour
		  * and no [[ArithmeticException]] is thrown if a value does not fit in this precision. */
		final val ExtendedFloor    = modes(0)(FLOOR.ordinal) //new MathContext(0, FLOOR)

		/** A `Decimal64` representation in extended precision, maximum for a given number in 56 bits
		  * (16 or 17 digits, depending on the significand). Results are rounded to the greater neighbour
		  * and no [[ArithmeticException]] is thrown if a value does not fit in this precision. */
		final val ExtendedCeiling  = modes(0)(CEILING.ordinal)//new MathContext(0, CEILING)

		/** A `Decimal64` representation in extended precision, maximum for a given number in 56 bits
		  * (16 or 17 digits, depending on the significand). Results are rounded towards the nearest neighbour,
		  * with the last digit of `5` being rounded towards zero.
		  * No [[ArithmeticException]] is thrown if a value does not fit in this precision. */
		final val ExtendedHalfDown = modes(0)(HALF_DOWN.ordinal) //new MathContext(0, HALF_DOWN)

		/** A `Decimal64` representation in extended precision, maximum for a given number in 56 bits
		  * (16 or 17 digits, depending on the significand). Results are rounded towards the nearest neighbour,
		  * with the last digit of `5` being rounded away from zero.
		  * No [[ArithmeticException]] is thrown if a value does not fit in this precision. */
		final val ExtendedHalfUp   = modes(0)(HALF_UP.ordinal) //new MathContext(0, HALF_UP)

		/** A `Decimal64` representation in extended precision, maximum for a given number in 56 bits
		  * (16 or 17 digits, depending on the significand). Results are rounded towards the nearest neighbour,
		  * with the last digit of `5` being rounded to the even neighbour.
		  * No [[ArithmeticException]] is thrown if a value does not fit in this precision. */
		final val ExtendedHalfEven = modes(0)(HALF_EVEN.ordinal) //new MathContext(0, HALF_EVEN)

		/** A `Decimal64` representation in extended precision, maximum for a given number in 56 bits
		  * (16 or 17 digits, depending on the significand), demanding exact computations.
		  * If a result would require rounding to fit the maximum precision for its value,
		  * an [[ArithmeticException]] is thrown. */
		final val ExtendedExact = modes(0)(UNNECESSARY.ordinal) //new MathContext(0, HALF_EVEN)

		/** A `Decimal64` representation in extended precision, maximum for a given number in 56 bits
		  * (16 or 17 digits, depending on the significand), and standard `HALF_EVEN` rounding.
		  * It is equal to [[net.noresttherein.sugar.numeric.Decimal64.Round.ExtendedHalfEven ExtendedHalfEven]].
		  * It is the mode used by factory methods which do not accept a `MathContext`.
		  * @see [[net.noresttherein.sugar.numeric.Decimal64.Round.Standard]] */
		final val Extended = ExtendedHalfEven

		/** A `Decimal64` representation with a maximal possible precision of 16. Results are rounded towards zero. */
		final val Down     = modes(Precision)(DOWN.ordinal)//new MathContext(Precision, DOWN)

		/** A `Decimal64` representation with a maximal possible precision of 16. Results are rounded away from zero. */
		final val Up       = modes(Precision)(UP.ordinal)//new MathContext(Precision, UP)

		/** A `Decimal64` representation with a maximal possible precision of 16.
		  * Results are rounded to the lesser neighbour. */
		final val Floor    = modes(Precision)(FLOOR.ordinal)//new MathContext(Precision, FLOOR)

		/** A `Decimal64` representation with a maximal possible precision of 16.
		  * Results are rounded to the greater neighbour. */
		final val Ceiling  = modes(Precision)(CEILING.ordinal)//new MathContext(Precision, CEILING)

		/** A `Decimal64` representation with a maximal possible precision of 16.
		  * Results are rounded towards the nearest neighbour, with the last digit of `5` being rounded towards zero. */
		final val HalfDown = modes(Precision)(HALF_DOWN.ordinal)//new MathContext(Precision, HALF_DOWN)

		/** A `Decimal64` representation with a maximal possible precision of 16.
		  * Results are rounded towards the nearest neighbour, with the last digit of `5` being rounded away from zero. */
		final val HalfUp   = modes(Precision)(HALF_UP.ordinal)//new MathContext(Precision, HALF_UP)

		/** A `Decimal64` representation with a maximal possible precision of 16.
		  * Results are rounded towards the nearest neighbour, with the last digit of `5` being rounded
		  * to the even neighbour. */
		final val HalfEven = modes(Precision)(HALF_EVEN.ordinal)//MathContext.DECIMAL64

		/** A `Decimal64` representation with a maximal possible precision of 16 .
		  * Calculations in this mode will throw an [[ArithmeticException]] if a value
		  * cannot be represented exactly as a `Decimal64`. */
		final val Exact    = modes(Precision)(UNNECESSARY.ordinal)//new MathContext(Precision, UNNECESSARY)

		/** A MathContext object with a precision setting matching the IEEE 754R Decimal64 format, 16 digits,
		  * and a rounding mode of `HALF_EVEN`, the IEEE 754R default. This mode is equal to
		  * `Decimal64.`[[net.noresttherein.sugar.numeric.Decimal64.Round.HalfEven HalfEven]].
		  * @see [[net.noresttherein.sugar.numeric.Decimal64.Round.Extended]] */
		final val Standard = HalfEven //MathContext.DECIMAL64



		/** Calculates the maximum possible precision with which the given number can be represented
		  * as a `Decimal64` after rounding according to the given rounding mode.
		  * @return `16` or `17`, depending on the leading digits of the argument.
		  **/
		@throws[ArithmeticException]("if value.mc.getRoundingMode == UNNECESSARY but value cannot be represented precisely as a Decimal64.")
		@inline def maxPrecision(value :BigDecimal) :Int =
			maxPrecision(value.bigDecimal, value.mc.getRoundingMode)

		/** Calculates the maximum possible precision with which the given number can be represented
		  * as a `Decimal64` after default rounding.
		  * @return `16` or `17`, depending on the leading digits of the argument.
		  **/
		@inline def maxPrecision(value :JavaBigDecimal) :Int = maxPrecision(value, HALF_EVEN)

		/** Calculates the maximum possible precision with which the given number can be represented
		  * as a `Decimal64` after rounding according to the given rounding mode.
		  * @return `16` or `17`, depending on the leading digits of the argument.
		  **/
		@throws[ArithmeticException]("if UNNECESSARY is specified as the rounding mode but value cannot be represented precisely as a Decimal64.")
		def maxPrecision(value :JavaBigDecimal, rounding :RoundingMode) :Int = {
			val unscaled = value.unscaledValue
			if (unscaled.compareTo(BigMinLong) >= 0 && unscaled.compareTo(BigMaxLong) <= 0)
				maxPrecision(unscaled.longValue, rounding)
			else {
				val normalized = value.stripTrailingZeros.unscaledValue
				if (normalized.compareTo(BigMinLong) >= 0 && normalized.compareTo(BigMaxLong) <= 0)
					maxPrecision(normalized.longValue, rounding)
				else if (rounding == UNNECESSARY)
					throw SugaredArithmeticException(
						"Rounding of " + value + " is necessary for a precision of " + MaxPrecision + "."
					)
				else {
					val rounded = value.round(to17digits(rounding))
					val normalized = rounded.unscaledValue
					if (normalized.compareTo(BigMinUnscaled) >= 0 && normalized.compareTo(BigMaxUnscaled) <= 0)
						MaxPrecision
					else
						Precision
				}
			}
		}

		/** Calculates the maximum possible precision with which the given number can be represented
		  * as a `Decimal64` after rounding according to the given rounding mode.
		  * @return `16` or `17`, depending on the leading digits of the argument.
		  **/
		@throws[ArithmeticException]("if UNNECESSARY is specified as the rounding mode but significand cannot be represented precisely as a Decimal64.")
		def maxPrecision(significand :Long, rounding :RoundingMode = HALF_EVEN) :Int =
			if (significand == 0)
				MaxPrecision
			else if (significand == Long.MinValue) //for overflows in abs
				if (rounding == UNNECESSARY)
					throw SugaredArithmeticException(
						"Rounding of " + significand + " is necessary for a precision of " + MaxPrecision + "."
					)
				else
					Precision
			else {
				def whenInRange(significand :Long, precision :Int) =
					if (precision == MaxPrecision) //separate from the case below for the case significand == MinUnscaled
						if (significand >= MinUnscaled & significand <= MaxUnscaled)
							MaxPrecision
						else Precision
					else if (Math.abs(significand) <= UnscaledPowerBounds(MaxPrecision - precision))
						MaxPrecision
					else
						Precision

				val abs = Math.abs(significand)
				var p = precision(abs)
				if (p > MaxPrecision) {
					val zeros = trailingZeros(significand)
					p -= zeros
					val value = significand / LongPowersOf10(zeros)
					if (p <= MaxPrecision)
						whenInRange(value, p)
					else if (rounding == UNNECESSARY)
						throw SugaredArithmeticException(
							"Rounding of " + significand + " is necessary for a precision of " + MaxPrecision + "."
						)
					else try {
						val rounded = divideLong(value, LongPowersOf10(p - MaxPrecision), rounding)
						if (MinUnscaled <= rounded & rounded <= MaxUnscaled)
							MaxPrecision
						else
							Precision
					} catch {
						case e :InternalException =>
							oops("Failed to calculate the maximum possible precision for " + significand +
								": " + e.getMessage + ".")
					}
				} else
					whenInRange(significand, p)
			}

		/** A [[java.math.MathContext MathContext]] with a variable precision and the given rounding mode.
		  * The values are rounded to the largest precision possible - 16 or 17 digits, except for
		  * rounding mode `UNNECESSARY`, in which case operations will throw an [[ArithmeticException]] if a value
		  * were to be rounded. This method returns one of the constants declared by this object.
		  **/
		def toMaxDigits(rounding :BigDecimal.RoundingMode.Value) :MathContext = modes(0)(rounding.id)

		/** A [[java.math.MathContext MathContext]] with a variable precision and the given rounding mode.
		  * The values are rounded to the largest precision possible - 16 or 17 digits, except for
		  * rounding mode `UNNECESSARY`, in which case operations will throw an [[ArithmeticException]] if a value
		  * were to be rounded. This method returns one of the constants declared by this object.
		  **/
		def toMaxDigits(rounding :RoundingMode) :MathContext = modes(0)(rounding.ordinal)

		/** A [[java.math.MathContext MathContext]] with a precision of 16 and the given rounding mode.
		  * This method returns one of the constants declared by this object.
		  **/
		def to16digits(rounding :BigDecimal.RoundingMode.Value) :MathContext = modes(16)(rounding.id)

		/** A [[java.math.MathContext MathContext]] with a precision of 16 and the given rounding mode.
		  * This method returns one of the constants declared by this object.
		  **/
		def to16digits(rounding :RoundingMode) :MathContext = modes(16)(rounding.ordinal)

		private[Decimal64] def to17digits(rounding :RoundingMode) :MathContext = modes(17)(rounding.ordinal)

		/** A factory method creating a `MathContext` with a precision of `n` and the given rounding mode.
		  * If the precision is within the `[0, `[[net.noresttherein.sugar.numeric.Decimal64.MaxPrecision MaxPrecision]]`]`
		  * range, the instance will be a cached constant.
		  **/
		def apply(n :Int, rounding :RoundingMode = RoundingMode.HALF_EVEN) :MathContext =
			if (n >= 0 & n <= MaxPrecision && rounding.ordinal < RoundingMode.values.length)
				modes(n)(rounding.ordinal)
			else //WTF?
				new MathContext(n, rounding)

		/** A factory method creating a `MathContext` with a precision of `n` and the given rounding mode.
		  * If the precision is within the `[0, `[[net.noresttherein.sugar.numeric.Decimal64.MaxPrecision MaxPrecision]]`]`
		  * range, the instance will be a cached constant.
		  **/
		def apply(n :Int, rounding :BigDecimal.RoundingMode.Value) :MathContext =
			if (n >= 0 & n <= MaxPrecision && rounding.id < RoundingMode.values.length)
				modes(n)(rounding.id)
			else //WTF?
				new MathContext(n, RoundingMode.valueOf(rounding.id))

		/** Extractor deconstructing a `MathContext` into its [[java.math.MathContext.getPrecision precision]]
		  * and [[java.math.MathContext.getRoundingMode roundingMode]] properties.
		  **/
		@inline def unapply(mode :MathContext) :Maybe[(Int, RoundingMode)] =
			Yes((mode.getPrecision, mode.getRoundingMode))

		@inline private[Decimal64] def isNearestNeighbour(rounding :RoundingMode) :Boolean =
			rounding == HALF_DOWN || rounding == HALF_UP || rounding == HALF_EVEN
	}


	/** All `Float` values which exactly represent non negative powers of 10. */
	private final val FloatPowersOf10 =
		Array(1.0e0f, 1.0e1f, 1.0e2f, 1.0e3f, 1.0e4f, 1.0e5f, 1.0e6f, 1.0e7f, 1.0e8f, 1.0e9f, 1.0e10f)

	/** All `Float` values which exactly represent non negative powers of 10. */
	private final val DoublePowersOf10 = Array(
		1.0e0, 1.0e1, 1.0e2, 1.0e3, 1.0e4, 1.0e5, 1.0e6, 1.0e7, 1.0e8, 1.0e10,
		1.0e11, 1.0e12, 1.0e13, 1.0e14, 1.0e15, 1.0e16, 1.0e17, 1.0e18, 1.0e19, 1.0e20, 1.0e21, 1.0e22
	)
	/** The number of non negative powers of 10 which can be represented exactly as a `Float`.
	  * Equals `FloatPowersOf10.length`.
	  */
	private final val ExactFloatPowersOf10  = 11
	/** The number of non negative powers of 10 which can be represented exactly as a `Double`.
	  * Equals `DoublePowersOf10.length`.
	  */
	private final val ExactDoublePowersOf10 = 23

	/** Maximum absolute values which, multiplied by `10^i`, fit in a `Long`. */
	private final val LongPowerBounds = Array.iterate(Long.MaxValue, LongPrecision + 1)(_ / 10L)
	/** Maximum absolute values which, multiplied by `10^i`, fit in a the unscaled range of `Decimal64`
	  * `[`[[net.noresttherein.sugar.numeric.Decimal64.MinUnscaled MinUnscaled]]`, `[[net.noresttherein.sugar.numeric.Decimal64.MaxUnscaled MaxUnscaled]]`]`. */
	private final val UnscaledPowerBounds = Array.iterate(MaxUnscaled, MaxPrecision)(_ / 10L)
	/** `LongPowersOf10(i) == 10^i`. */
	private final val LongPowersOf10 = Array.iterate(1L, MaxLongPrecision)(_ * 10L)
	/** `10^i` for `i== 0..MaxExponent` as `BigInteger`. */
	private final val BigIntegerPowersOf10 = Array.iterate(BigInteger.ONE, MaxExponent + 1)(_ multiply BigInteger.TEN)
//	/** `PowersOf10Halves(i) == 5*10^i`. */
//	private final val LongPowersOf10Halves = LongPowersOf10.map(_ * 5L)
	/** `PowersOf10(i) == Decimal64(1, -i)`. */
	private final val PowersOf10 = Array.iterate(One, MaxScale)(_ * 10)
	/** The maximum power of 10 (value, not exponent) which fits in a `Long`: `10^18`. */
	private final val MaxLongPowerOf10     = 1000000000000000000L
	/** The maximum power of 10 (value, not exponent) lesser than `MaxUnscaled`, i.e. `10^16`. */
	private final val MaxUnscaledPowerOf10 = 10000000000000000L
	private final val MaxInt   = new Decimal64(((1L << 31) - 1L) << ScaleBits)
	private final val MinInt   = new Decimal64(0xffffffff80000000L << ScaleBits)
	private final val MaxUInt  = new Decimal64(0xffffffffL << ScaleBits)
	private final val MaxULong = new Decimal64((Long.MaxValue / 10L << 1) / 100L << ScaleBits | MaxLongValueLog & ScaleMask)
	private final val BigMaxUnscaled = BigInteger.valueOf(MaxUnscaled)
	private final val BigMinUnscaled = BigInteger.valueOf(MinUnscaled)
	private final val BigMaxLong     = BigInteger.valueOf(Long.MaxValue)
	private final val BigMinLong     = BigInteger.valueOf(Long.MinValue)
	private final val BigMinusTwo    = BigInteger.TWO.negate
	private final val DecimalMaxLong = JavaBigDecimal.valueOf(Long.MaxValue)
	private final val DecimalMinLong = JavaBigDecimal.valueOf(Long.MinValue)

	private final val PositiveExponentFormat = "e" //could be, for example, "E+
	private final val NegativeExponentFormat = "e"
	/** Integer `Decimal64` numbers will be formatted without an exponent if their precision does not exceed this number. */
	private final val MaxDigitsInWholeString = MaxPrecision
//	private final val MaxWholeDigitsInString = 4
	/** The maximum number of digits a formatted `Decimal64` can take without an exponent part. */
	private final val MaxDigitsInPlainString = 7
	/** The maximum number of consecutive zeros to the right of the decimal point in a `Decimal64`
	  * formatted without an exponent part. */
	private final val MaxLeadingZerosInString = 1
	/** `Decimal64` numbers with more digits to the left of the decimal point will be formatted with an exponent. */
	private final val MaxWholeDigitsInPlainString = 4
	/** `Decimal64` numbers with more digits to the right of the decimal point will be formatted with an exponent. */
	private final val MaxFractionalDigitsInPlainString = 2

	private val SignificandOutOfRangeException = new InternalException("significand out of range")
	private val ScaleOutOfRangeException       = new InternalException("scale out of range")
	private val PrecisionExceededException     = new InternalException("Decimal64 precision exceeded")
	private val OverflowException              = new InternalException("arithmetic overflow")
	private val UnderflowException             = new InternalException("arithmetic underflow")
//	private val DivisionByZeroException        = new InternalException("division by zero")
	private val RoundingNecessaryException     = new InternalException("rounding necessary")
//	private val InfiniteExpansionException     = new InternalException("infinite decimal expansion")
	private val DecimalFormatException         = new InternalException("invalid decimal format")



	//consider: make single-arg apply methods use Round.Standard, and add exact methods using ExtendedExact
	/** Accurately represents an `Int` as a `Decimal64`. */
	def apply(int :Int) :Decimal64 =
		if (int % 10 != 0) make(int, 0)
		else Decimal64(int, 0)

	/** Attempts to create a `Decimal64` equal to the given `Long` value.
	  * @param int an integer value which satisfies
	  *            [[net.noresttherein.sugar.numeric.Decimal64.MinUnscaled MinUnscaled]]` <= int <= `[[net.noresttherein.sugar.numeric.Decimal64.MaxUnscaled MaxUnscaled]].
	  * @return `Decimal(int, 0)`.
	  **/
	@throws[ArithmeticException]("if the argument is outside the [MinUnscaled, MaxUnscaled] range.")
	@inline def apply(int :Long) :Decimal64 = apply(int, 0)

	/** Creates a normalized `Decimal64` instance equal to `significand * 10E-scale`.
	  * All trailing zeros in the significand are removed and instead its exponent is increased by that number,
	  * unless this would cause exponent (`scale`) to overflow, in which case the decimal point is moved left
	  * until the `scale` limit of `-128` (corresponding to an exponent of `128`) is reached.
	  * If the significand after normalization doesn't fall in the range of
	  * `[`[[net.noresttherein.sugar.numeric.Decimal64.MinUnscaled MinUnscaled]]`, `[[net.noresttherein.sugar.numeric.Decimal64.MaxUnscaled MaxUnscaled]]`]`,
	  * or if the `scale`, despite normalization, fails outside of
	  * `[`[[net.noresttherein.sugar.numeric.Decimal64.MinScale MinScale]]`,`[[net.noresttherein.sugar.numeric.Decimal64.MaxScale MaxScale]]`]`,
	  * then an [[ArithmeticException]] is thrown.
	  *
	  * This is a simpler equivalent of
	  * [[net.noresttherein.sugar.numeric.Decimal64.round(significand:Long,scale:Int)(implicit*, round]]`(significand, scale)(`[[net.noresttherein.sugar.numeric.Decimal64.Round.ExtendedExact ExtendedExact]]`)`
	  * @param significand A value containing all leading non zero digits of the created `Decimal64`.
	  *                    The decimal representation of the whole number consists of the decimal format of `significand`,
	  *                    followed by `-scale` number of zeros (if `scale` is non negative), or of `significand`
	  *                    with a fractional point between `-scale`-th and `-scale + 1`-th least significant digit.
	  *                    If `scale > precision`, where `precision` is the number of digits in `significand`
	  *                    (without any leading zeros, except for `0`, which has a precision of `1`),
	  *                    then the number is represented by a `0.`, followed by `scale - precision` zeros,
	  *                    followed by the significand.
	  * @param scale       The number of least significant digits in `significand` which consist of the fractional
	  *                    portion of the created `Decimal64` (if `scale is positive)`, or the number of trailing zeros
	  *                    following the significand.
	  **/
	@throws[ArithmeticException]("if significand * 10^-scale cannot be represented precisely as a Decimal64.")
	def apply(significand :Long, scale :Int) :Decimal64 =
		try from(significand, scale) catch {
			case e :InternalException =>
				throw SugaredArithmeticException(
					"Decimal64(" + significand + "L, " + scale + "): " + e.getMessage + "."
				)
		}

	/** Creates `Decimal64` of value `significand*10^-scale`, rounded to the maximum possible precision
	  * (16 or 17 digits) in the direction specified by the passed [[scala.math.BigDecimal.RoundingMode RoundingMode]].
	  * For arguments on which neither method throws an exception, this is equivalent to
	  * `Decimal64(significand, scale).round(mode)`, but this method performs rounding before the creation
	  * of the result and thus accepts any `Long` as a significand, providing it fits in a `Decimal64` after rounding
	  * to [[net.noresttherein.sugar.numeric.Decimal64.Precision Precision]].
	  **/
	@throws[ArithmeticException]("if rounding is UNNECESSARY and the value exceeds the precision of Decimal64.")
	def apply(significand :Long, scale :Int, rounding :BigDecimal.RoundingMode.Value) :Decimal64 =
		Decimal64(significand, scale, RoundingMode.valueOf(rounding.id), 0)

	/** Creates `Decimal64` of value `significand*10^-scale`, rounded to the maximum possible precision
	  * (16 or 17 digits) in the direction specified by the passed [[java.math.RoundingMode RoundingMode]].
	  * For arguments on which neither method throws an exception, this is equivalent to
	  * `Decimal64(significand, scale).round(mode)`, but this method performs rounding before the creation
	  * of the result and thus accepts any `Long` as a significand, providing it fits in a `Decimal64` after rounding
	  * to [[net.noresttherein.sugar.numeric.Decimal64.Precision Precision]].
	  **/
	@throws[ArithmeticException]("if rounding is UNNECESSARY and the value exceeds the precision of Decimal64.")
	def apply(significand :Long, scale :Int, rounding :RoundingMode) :Decimal64 =
		Decimal64(significand, scale, rounding, 0)

	/** Creates `Decimal64` of value `significand*10^-scale`, rounded
	  * according to the specified [[java.math.MathContext MathContext]].
	  * This method is exactly equivalent to
	  * [[net.noresttherein.sugar.numeric.Decimal64.round(significand:Long,scale:Int)* round]]`(significand, scale)(mode)`,
	  * the only difference being that the `MathContext` argument is explicit, rather than implicit.
	  * For arguments on which neither method throws an exception, this is equivalent to
	  * `Decimal64(significand, scale).round(mode)`, but this method performs rounding before the creation
	  * of the result and thus accepts any `Long` as a significand, providing it fits in a `Decimal64` after rounding
	  * to `mode.`[[java.math.MathContext.getPrecision getPrecision]].
	  **/
	@throws[ArithmeticException]("if the value cannot be represented as a Decimal64 to the requested precision.")
	@inline def apply(significand :Long, scale :Int, mode :MathContext) :Decimal64 =
		Decimal64(significand, scale, mode.getRoundingMode, mode.getPrecision)


	/** Creates a `Decimal64` closest in value to the argument `Float`.
	  * This is analogous to [[scala.math.BigDecimal BigDecimal]]`.`[[scala.math.BigDecimal.decimal decimal]]`(decimal)`,
	  * bar precision differences.
	  **/
	@throws[ArithmeticException]("if the value exceeds the precision of Decimal64.")
	@inline def apply(decimal :Float) :Decimal64 = Decimal64(decimal, ExtendedExact)

	/** Creates `Decimal64` equal to the given `Float` value, rounded to the maximum possible precision
	  * (16 or 17 digits) in the direction specified by the passed [[scala.math.BigDecimal.RoundingMode RoundingMode]].
	  * For arguments on which neither method throws an exception, this is equivalent to
	  * `Decimal64(significand, scale).round(mode)`, but this method performs rounding before the creation
	  * of the result and thus accepts any `Long` as a significand, providing it fits in a `Decimal64` after rounding
	  * to [[net.noresttherein.sugar.numeric.Decimal64.Precision Precision]].
	  **/
	@throws[ArithmeticException]("if rounding is UNNECESSARY and the value exceeds the precision of Decimal64.")
	@inline def apply(decimal :Float, rounding :BigDecimal.RoundingMode.Value) :Decimal64 =
		Decimal64(decimal, RoundingMode.valueOf(rounding.id))

	/** Creates `Decimal64` equal to the given `Float` value, rounded to the maximum possible precision
	  * (16 or 17 digits) in the direction specified by the passed [[java.math.RoundingMode RoundingMode]].
	  **/
	@throws[ArithmeticException]("if rounding is UNNECESSARY and the value exceeds the precision of Decimal64.")
	@inline def apply(decimal :Float, rounding :RoundingMode) :Decimal64 =
		Decimal64(decimal, toMaxDigits(rounding))

	/** Creates a `Decimal64` equal to the `Float` value rounded
	  * according to the specified [[java.math.MathContext MathContext]]. `Double`. This is analogous
	  * to [[scala.math.BigDecimal BigDecimal]]`.`[[scala.math.BigDecimal.decimal decimal]]`(decimal, mode)`,
	  * bar precision differences.
	  **/
	@throws[ArithmeticException]("if the value exceeds the precision of Decimal64.")
	def apply(decimal :Float, mode :MathContext) :Decimal64 =
		try from(decimal, mode) catch {
			case e :InternalException =>
				throw SugaredArithmeticException(
					"Cannot accurately represent Double " + decimal + " as a Decimal64 with a precision of " +
						mode.getPrecision + ", rounding " + mode.getRoundingMode + ": " + e.getMessage + "."
				)
		}


	/** Creates a `Decimal64` exactly equal in value to the argument `Double`.
	  * This is analogous to [[scala.math.BigDecimal BigDecimal]]`.`[[scala.math.BigDecimal.decimal decimal]]`(decimal)`,
	  * bar precision differences.
	  **/
	@throws[ArithmeticException]("if the value exceeds the precision of Decimal64.")
	@inline def apply(decimal :Double) :Decimal64 = Decimal64(decimal, ExtendedExact)

	/** Creates `Decimal64` equal to the given `Double` value, rounded to the maximum possible precision
	  * (16 or 17 digits) in the direction specified by the passed [[scala.math.BigDecimal.RoundingMode RoundingMode]].
	  * For arguments on which neither method throws an exception, this is equivalent to
	  * `Decimal64(significand, scale).round(mode)`, but this method performs rounding before the creation
	  * of the result and thus accepts any `Long` as a significand, providing it fits in a `Decimal64` after rounding
	  * to [[net.noresttherein.sugar.numeric.Decimal64.Precision Precision]].
	  **/
	@throws[ArithmeticException]("if rounding is UNNECESSARY and the value exceeds the precision of Decimal64.")
	@inline def apply(decimal :Double, rounding :BigDecimal.RoundingMode.Value) :Decimal64 =
		Decimal64(decimal, RoundingMode.valueOf(rounding.id))

	/** Creates `Decimal64` equal to the given `Double` value, rounded to the maximum possible precision
	  * (16 or 17 digits) in the direction specified by the passed [[scala.math.BigDecimal.RoundingMode RoundingMode]].
	  **/
	@throws[ArithmeticException]("if rounding is UNNECESSARY and the value exceeds the precision of Decimal64.")
	@inline def apply(decimal :Double, rounding :RoundingMode) :Decimal64 =
		Decimal64(decimal, toMaxDigits(rounding))

	/** Creates a `Decimal64` equal to the `Double` value rounded
	  * according to the specified [[java.math.MathContext MathContext]]. `Double`. This is analogous
	  * to [[scala.math.BigDecimal BigDecimal]]`.`[[scala.math.BigDecimal.decimal decimal]]`(decimal, mode)`,
	  * bar precision differences.
	  **/
	@throws[ArithmeticException]("if the value exceeds the precision of Decimal64.")
	def apply(decimal :Double, mode :MathContext) :Decimal64 =
		try from(decimal, mode) catch {
			case e :InternalException =>
				throw SugaredArithmeticException(
					"Cannot accurately represent Double " + decimal + " as a Decimal64 with a precision of " +
						mode.getPrecision + ", rounding " + mode.getRoundingMode + ": " + e.getMessage + "."
				)
		}


	/** Creates a `Decimal64` of an integral value equal to `big`, with a scale of zero. */
	@throws[ArithmeticException]("if the value exceeds the precision of Decimal64.")
	@inline def apply(big :BigInt) :Decimal64 = Decimal64(big.bigInteger, ExtendedExact)

	/** Creates a `Decimal64` by rounding the given [[scala.math.BigInt]] to the maximum
	  * available precision of `16` or `17` digits, depending on the value of the significand.
	  **/
	@throws[ArithmeticException]("if rounding is UNNECESSARY and the value exceeds the precision of Decimal64.")
	@inline def apply(big :BigInt, rounding :BigDecimal.RoundingMode.Value) :Decimal64 =
		Decimal64(big.bigInteger, rounding)

	/** Creates a `Decimal64` by rounding the given [[scala.math.BigInt]] according to the given mode to the maximum
	  * available precision of `16` or `17` digits, depending on the value of the significand.
	  **/
	@throws[ArithmeticException]("if rounding is UNNECESSARY and the value exceeds the precision of Decimal64.")
	@inline def apply(big :BigInt, rounding :RoundingMode) :Decimal64 = Decimal64(big.bigInteger, rounding)

	/** Rounds the given [[scala.math.BigInt]] to the precision and according to the rounding mode
	  * specified by the given `MathContext` and returns the result as a `Decimal64`.
	  **/
	@throws[ArithmeticException]("if the value cannot be represented as a Decimal64 to the requested precision.")
	@inline def apply(big :BigInt, mode :MathContext) :Decimal64 = Decimal64(big.bigInteger, mode)


	/** Creates a `Decimal64` of an integral value equal to `big`, with a scale of zero. */
	@throws[ArithmeticException]("if the value exceeds the precision of Decimal64.")
	def apply(big :BigInteger) :Decimal64 = big match {
		case BigInteger.ZERO => Zero
		case BigInteger.ONE  => One
		case BigInteger.TEN  => Ten
		case _ => Decimal64(big, ExtendedExact)
	}

	/** Creates a `Decimal64` by rounding the given [[java.math.BigInteger]] to the maximum
	  * available precision of `16` or `17` digits, depending on the value of the significand.
	  **/
	@throws[ArithmeticException]("if rounding is UNNECESSARY and the value exceeds the precision of Decimal64.")
	@inline def apply(big :BigInteger, rounding :BigDecimal.RoundingMode.Value) :Decimal64 =
		Decimal64(big, toMaxDigits(rounding))

	/** Creates a `Decimal64` by rounding the given [[java.math.BigInteger]] according to the given mode to the maximum
	  * available precision of `16` or `17` digits, depending on the value of the significand.
	  **/
	@throws[ArithmeticException]("if rounding is UNNECESSARY and the value exceeds the precision of Decimal64.")
	def apply(big :BigInteger, rounding :RoundingMode) :Decimal64 = Decimal64(big, toMaxDigits(rounding))

	/** Rounds the given [[java.math.BigInteger]] to the precision and according to the rounding mode
	  * specified by the given `MathContext` and returns the result as a `Decimal64`.
	  **/
	@throws[ArithmeticException]("if the value cannot be represented as a Decimal64 to the requested precision.")
	def apply(big :BigInteger, mode :MathContext) :Decimal64 =
		try from(big, mode) catch {
			case e :InternalException =>
				throw SugaredArithmeticException(
					"Cannot represent BigInteger " + big + " as a Decimal64 with a precision of " +
						mode.getPrecision + ", rounding " + mode.getRoundingMode +": " + e.getMessage + "."
				)
		}


	/** Creates a `Decimal64` exactly equal in value to the argument [[scala.math.BigDecimal]], to the precision
	  * specified by its [[java.math.MathContext MathContext]]. Note that this makes it different
	  * from `Decimal64(big :java.math.BigDecimal)`, as the latter will attempt to create a value exactly equal
	  * to its argument. However, this is usually of little importance, as the default `MathContext`
	  * of `scala.math.BigDecimal` is [[java.math.MathContext.DECIMAL128 DECIMAL128]], which exceeds the precision
	  * of `Decimal64`.
	  **/
	@throws[ArithmeticException]("if the value exceeds the precision of Decimal64.")
	def apply(big :BigDecimal) :Decimal64 =
		if (big.mc.getPrecision == 0) Decimal64(big.bigDecimal, ExtendedExact)
		else Decimal64(big.bigDecimal, big.mc)

	/** Creates a `Decimal64` by rounding the given [[scala.math.BigDecimal]] to the maximum
	  * available precision of `16` or `17` digits, depending on the value of the significand.
	  **/
	@throws[ArithmeticException]("if rounding is UNNECESSARY and the value exceeds the precision of Decimal64.")
	@inline def apply(big :BigDecimal, rounding :BigDecimal.RoundingMode.Value) :Decimal64 =
		Decimal64(big.bigDecimal, RoundingMode.valueOf(rounding.id))

	/** Creates a `Decimal64` by rounding the given [[scala.math.BigDecimal]] according to the given mode to the maximum
	  * available precision of `16` or `17` digits, depending on the value of the significand.
	  **/
	@throws[ArithmeticException]("if rounding is UNNECESSARY and the value exceeds the precision of Decimal64.")
	@inline def apply(big :BigDecimal, rounding :RoundingMode) :Decimal64 = Decimal64(big.bigDecimal, rounding)

	/** Rounds the given [[scala.math.BigDecimal]] to the precision and according to the rounding mode
	  * specified by the given `MathContext` and returns the result as a `Decimal64`.
	  **/
	@throws[ArithmeticException]("if the value cannot be represented as a Decimal64 to the requested precision.")
	@inline def apply(big :BigDecimal, mode :MathContext) :Decimal64 = Decimal64(big.bigDecimal, mode)


	/** Creates a `Decimal64` exactly equal in value to the argument [[java.math.BigDecimal]]. */
	@throws[ArithmeticException]("if the value exceeds the precision of Decimal64.")
	@inline def apply(big :JavaBigDecimal) :Decimal64 = Decimal64(big, ExtendedExact)

	/** Creates a `Decimal64` by rounding the given [[java.math.BigDecimal]] to the maximum
	  * available precision of `16` or `17` digits, depending on the value of the significand.
	  **/
	@throws[ArithmeticException]("if rounding is UNNECESSARY and the value exceeds the precision of Decimal64.")
	@inline def apply(big :JavaBigDecimal, rounding :BigDecimal.RoundingMode.Value) :Decimal64 =
		Decimal64(big, RoundingMode.valueOf(rounding.id))

	/** Creates a `Decimal64` by rounding the given [[java.math.BigDecimal]] according to the given mode to the maximum
	  * available precision of `16` or `17` digits, depending on the value of the significand.
	  **/
	@throws[ArithmeticException]("if rounding is UNNECESSARY and the value exceeds the precision of Decimal64.")
	@inline def apply(big :JavaBigDecimal, rounding :RoundingMode) :Decimal64 = Decimal64(big, toMaxDigits(rounding))

	/** Rounds the given [[java.math.BigDecimal]] to the precision and according to the rounding mode
	  * specified by the given `MathContext` and returns the result as a `Decimal64`.
	  **/
	@throws[ArithmeticException]("if the value cannot be represented as a Decimal64 to the requested precision.")
	def apply(big :JavaBigDecimal, mode :MathContext) :Decimal64 =
		try from(big, mode) catch {
			case e :InternalException =>
				throw SugaredArithmeticException(
					"Cannot accurately represent java.math.BigDecimal " + big + " as a Decimal64 with a precision of " +
						mode.getPrecision + ", rounding " + mode.getRoundingMode + ": " + e.getMessage + "."
				)
		}


	/** Parses a `String` with a number in the decimal format. Accepted formats are the same
	  * as by [[java.math.BigDecimal java.math.BigDecimal]] constructor.
	  **/
	@throws[ArithmeticException]("if the value exceeds the precision of Decimal64.")
	@throws[NumberFormatException]("if the string does not represent a valid decimal number.")
	@inline def apply(string :String) :Decimal64 = Decimal64(string, ExtendedExact)

	/** Parses a `String` with a number in the decimal format. Accepted formats are the same
	  * as by [[java.math.BigDecimal java.math.BigDecimal]] constructor. The value will be rounded in the given mo,de
	  * to the maximum precision with which it can fit in a `Decimal64`.
	  **/
	@throws[ArithmeticException]("if rounding is UNNECESSARY and the value exceeds the precision of Decimal64.")
	@throws[NumberFormatException]("if the string does not represent a valid decimal number.")
	def apply(string :String, rounding :BigDecimal.RoundingMode.Value) :Decimal64 =
		Decimal64(string, toMaxDigits(rounding))

	/** Parses a `String` with a number in the decimal format. Accepted formats are the same
	  * as by [[java.math.BigDecimal java.math.BigDecimal]] constructor. The value will be rounded in the given mode
	  * to the maximum precision with which it can fit in a `Decimal64`.
	  **/
	@throws[ArithmeticException]("if rounding is UNNECESSARY and the value exceeds the precision of Decimal64.")
	@throws[NumberFormatException]("if the string does not represent a valid decimal number.")
	def apply(string :String, rounding :RoundingMode) :Decimal64 = Decimal64(string, toMaxDigits(rounding))

	/** Parses a `String` with a number in the decimal format. The value will be rounded according
	  * to the [[java.math.MathContext MathContext]] argument. Accepted formats are the same
	  * as by [[java.math.BigDecimal java.math.BigDecimal]] constructor.
	  **/
	@throws[ArithmeticException]("if the value cannot be represented as a Decimal64 to the requested precision.")
	@throws[NumberFormatException]("if the string does not represent a valid decimal number.")
	def apply(string :String, mode :MathContext) :Decimal64 =
		try from(string, mode) catch {
			case DecimalFormatException =>
				throw SugaredNumberFormatException(
					"Cannot convert '" + string + "' to a Decimal64 with a precision of " + mode.getPrecision +
						", rounding " + mode.getRoundingMode + ": invalid decimal format."
				)
			case e :InternalException =>
				throw SugaredArithmeticException(
					"Cannot convert '" + string + "' to a Decimal64 with a precision of " + mode.getPrecision +
						", rounding " + mode.getRoundingMode + ": " + e.getMessage + "."
				)
		}

	/** Implementation of both `Decimal64.round(significand, scale)(mode)` and class method `round(mode)`.
	  * Passing precision `0` means that the value will be rounded to the maximum possible precision
	  * and no [[ArithmeticException]] will be thrown unless rounding mode is `UNNECESSARY` and rounding is necessary.
	  **/
	@throws[ArithmeticException]("if the value exceeds the precision of Decimal64.")
	private def apply(significand :Long, scale :Int, rounding :RoundingMode, precision :Int) :Decimal64 =
		try from(significand, scale, rounding, precision) catch {
			case e :InternalException =>
				throw SugaredArithmeticException(
					s"Cannot round $rounding ${significand}e${-scale} to $precision precision: " +
						e.getMessage + "."
				)
		}


	/** Creates a `Decimal64` with a value equal to `significand * 10^exponent`.
	  * This method is exactly equivalent to `Decimal64(significand, -exponent)`, but accepts an exponent
	  * rather than its negation, which may be more convenient to read.
	  **/
	@throws[ArithmeticException]("if significand * 10^exponent cannot be represented precisely as a Decimal64. ")
	def raise(significand :Long, exponent :Int) :Decimal64 =
		if (exponent == Int.MinValue)
			scaleOutOfRangeException(significand, -exponent.toLong)
		else Decimal64(significand, -exponent)

	/** Creates a `Decimal64` with a value equal to `significand * 10^exponent` rounded to fit `Decimal64` precision
	  * according to the given rounding mode.
	  * This method is exactly equivalent to `Decimal64(significand, -exponent, rounding)`, but accepts an exponent
	  * rather than its negation, which may be more convenient to read.
	  **/
	@throws[ArithmeticException]("if significand * 10^exponent cannot be represented precisely as a Decimal64. ")
	def raise(significand :Long, exponent :Int, rounding :RoundingMode) :Decimal64 =
		if (exponent == Int.MinValue)
			scaleOutOfRangeException(significand, -exponent.toLong)
		else Decimal64(significand, -exponent, rounding)

	/** Creates a `Decimal64` with a value equal to `significand * 10^exponent` rounded to the given precision
	  * according to `mode.getRoundingMode`. If the requested precision exceeds the precision of `Decimal64`
	  * and rounding of significand is necessary, an [[ArithmeticException]] is thrown.
	  *
	  * This method is exactly equivalent to `Decimal64(significand, -exponent, mode)`, but accepts an exponent
	  * rather than its negation, which may be more convenient to read.
	  **/
	@throws[ArithmeticException]("if significand * 10^exponent cannot be represented precisely as a Decimal64. ")
	def raise(significand :Long, exponent :Int, mode :MathContext) :Decimal64 =
		if (exponent == Int.MinValue)
			scaleOutOfRangeException(significand, -exponent.toLong)
		else Decimal64(significand, -exponent, mode)


	/** Rounds the given integral number according to the rounding mode and to the precision specified
	  * by an implicit `MathContext`. If no such implicit exists,
	  * [[net.noresttherein.sugar.numeric.Decimal64.Round Round]]`.`[[net.noresttherein.sugar.numeric.Decimal64.Round.Extended Extended]]
	  * is used, representing a maximal possible precision.
	  **/
	@throws[ArithmeticException]("if the value exceeds the precision of Decimal64 or the one from the implicit MathContext.")
	@inline def round(int :Long)(implicit mode :Optionally[MathContext]) :Decimal64 = round(int, 0)

	/** Creates `Decimal64` of value `significand*10^-scale`, rounded
	  * according to the implicit [[java.math.MathContext MathContext]]. If no such instance exists,
	  * [[net.noresttherein.sugar.numeric.Decimal64.Round Round]]`.`[[net.noresttherein.sugar.numeric.Decimal64.Round.Extended Extended]]
	  * is used. For arguments on which neither method throws an exception, this is equivalent to
	  * `Decimal64(significand, scale).round(mode)`, but this method performs rounding before the creation
	  * of the result and thus accepts any `Long` as a significand, providing it fits in a `Decimal64` after rounding
	  * to `mode.`[[java.math.MathContext.getPrecision getPrecision]].
	  **/
	@throws[ArithmeticException]("if the value exceeds the precision of Decimal64 or the one from the implicit MathContext.")
	def round(significand :Long, scale :Int)(implicit mode :Optionally[MathContext]) :Decimal64 = {
		val ctx = mode getOrElse Extended
		Decimal64(significand, scale, ctx.getRoundingMode, ctx.getPrecision)
	}

	/** Creates a `Decimal64` base on the decimal expansion of the given `Float`, and rounds it
	  * according to the rounding mode and to the precision specified by an implicit `MathContext`.
	  * If no such instance exists,
	  * [[net.noresttherein.sugar.numeric.Decimal64.Round Round]]`.`[[net.noresttherein.sugar.numeric.Decimal64.Round.Extended Extended]]
	  * is used instead. This method is equivalent to
	  * [[net.noresttherein.sugar.numeric.Decimal64.apply(decimal:Double,mode:MathContext)* Decimal64]]`(big, mode)`,
	  * the only difference is that the `MathContext` argument is implicit.
	  * @note this is a counterpart of [[scala.math.BigDecimal$ BigDecimal]]`.`[[scala.math.Bigecimal.decimal decimal]](decimal)`
	  */
	@throws[ArithmeticException]("if the value exceeds the precision of Decimal64 or the one from the implicit MathContext.")
	@inline def round(decimal :Float)(implicit mode :Optionally[MathContext]) :Decimal64 =
		Decimal64(decimal, mode getOrElse Extended)

	/** Creates a `Decimal64` base on the decimal expansion of the given `Double`, and rounds it
	  * according to the rounding mode and to the precision specified by an implicit `MathContext`.
	  * If no such instance exists,
	  * [[net.noresttherein.sugar.numeric.Decimal64.Round Round]]`.`[[net.noresttherein.sugar.numeric.Decimal64.Round.Extended Extended]]
	  * is used instead. This method is equivalent to
	  * [[net.noresttherein.sugar.numeric.Decimal64.apply(decimal:Double,mode:MathContext)* Decimal64]]`(big, mode)`,
	  * the only difference is that the `MathContext` argument is implicit.
	  * @note this is a counterpart of [[scala.math.BigDecimal$ BigDecimal]]`.`[[scala.math.Bigecimal.decimal decimal]](decimal)`
	  */
	@throws[ArithmeticException]("if the value exceeds the precision of Decimal64 or the one from the implicit MathContext.")
	@inline def round(decimal :Double)(implicit mode :Optionally[MathContext]) :Decimal64 =
		Decimal64(decimal, mode getOrElse Extended)

	/** Rounds the given [[scala.math.BigInt]] according to the rounding mode and to the precision specified
	  * by an implicit `MathContext`. If no such instance exists,
	  * [[net.noresttherein.sugar.numeric.Decimal64.Round Round]]`.`[[net.noresttherein.sugar.numeric.Decimal64.Round.Extended Extended]]
	  * is used instead. This method is equivalent to
	  * [[net.noresttherein.sugar.numeric.Decimal64.apply(big:scala.math.BigInt,mode:MathContext)* Decimal64]]`(big, mode)`,
	  * the only difference is that the `MathContext` argument is implicit.
	  **/
	@throws[ArithmeticException]("if the value exceeds the precision of Decimal64 or the one from the implicit MathContext.")
	@inline def round(int :BigInt)(implicit mode :Optionally[MathContext]) :Decimal64 =
		Decimal64(int.bigInteger, mode getOrElse Extended)

	/** Rounds the given [[java.math.BigInteger]] according to the rounding mode and to the precision specified
	  * by an implicit `MathContext`. If no such instance exists,
	  * [[net.noresttherein.sugar.numeric.Decimal64.Round Round]]`.`[[net.noresttherein.sugar.numeric.Decimal64.Round.Extended Extended]]
	  * is used instead. This method is equivalent to
	  * [[net.noresttherein.sugar.numeric.Decimal64.apply(big:java.math.BigInteger,mode:MathContext)* Decimal64]]`(big, mode)`,
	  * the only difference is that the `MathContext` argument is implicit.
	  **/
	@throws[ArithmeticException]("if the value exceeds the precision of Decimal64 or the one from the implicit MathContext.")
	@inline def round(int :BigInteger)(implicit mode :Optionally[MathContext]) :Decimal64 =
		Decimal64(int, mode getOrElse Extended)

	/** Rounds the given [[scala.math.BigDecimal]] according to the rounding mode and to the precision specified
	  * by an implicit `MathContext`. If no such instance exists,
	  * [[net.noresttherein.sugar.numeric.Decimal64.Round Round]]`.`[[net.noresttherein.sugar.numeric.Decimal64.Round.Extended Extended]]
	  * is used instead. This method is equivalent to
	  * [[net.noresttherein.sugar.numeric.Decimal64.apply(big:scala.math.BigDecimal,mode:MathContext)* Decimal64]]`(big, mode)`,
	  * the only difference is that the `MathContext` argument is implicit.
	  **/
	@throws[ArithmeticException]("if the value exceeds the precision of Decimal64 or the one from the implicit MathContext.")
	@inline def round(big :BigDecimal)(implicit mode :Optionally[MathContext]) :Decimal64 =
		Decimal64(big.bigDecimal, mode getOrElse Extended)

	/** Rounds the given [[java.math.BigDecimal]] according to the rounding mode and to the precision specified
	  * by an implicit `MathContext`. If no such instance exists,
	  * [[net.noresttherein.sugar.numeric.Decimal64.Round Round]]`.`[[net.noresttherein.sugar.numeric.Decimal64.Round.Extended Extended]]
	  * is used instead. This method is equivalent to
	  * [[net.noresttherein.sugar.numeric.Decimal64.apply(big:java.math.BigDecimal,mode:MathContext)* Decimal64]]`(big, mode)`,
	  * the only difference is that the `MathContext` argument is implicit.
	  **/
	@throws[ArithmeticException]("if the value exceeds the precision of Decimal64 or the one from the implicit MathContext.")
	@inline def round(big :JavaBigDecimal)(implicit mode :Optionally[MathContext]) :Decimal64 =
		Decimal64(big, mode getOrElse Extended)

	/** Parses the given `String` as a decimal and rounds it according the rounding mode and to the precision
	  * specified by an implicit `MathContext`. If no such instance exists,
	  * [[net.noresttherein.sugar.numeric.Decimal64.Round Round]]`.`[[net.noresttherein.sugar.numeric.Decimal64.Round.Extended Extended]]
	  * is used instead. This method is equivalent to [[net.noresttherein.sugar.numeric.Decimal64.parse parse]],
	  * except the latter returns the result in an `Maybe` instead of throwing an exception.
	  **/
	@throws[NumberFormatException]("if the string does not represent a decimal number in an accepted format.")
	@throws[ArithmeticException]("if the value exceeds the precision of Decimal64 or the one from the implicit MathContext.")
	@inline def round(string :String)(implicit mode :Optionally[MathContext]) :Decimal64 =
		Decimal64(string, mode getOrElse Extended)


	/** Attempts to create a `Decimal64` equal to the given `Long` value.
	  * @param int an integer value which satisfies
	  *            [[net.noresttherein.sugar.numeric.Decimal64.MinUnscaled MinUnscaled]]` <= int <= `[[net.noresttherein.sugar.numeric.Decimal64.MaxUnscaled MaxUnscaled]].
	  * @return `Yes(Decimal(int, 0))`, or `No` if the former would throw an exception.
	  **/
	def exact(int :Long) :Maybe[Decimal64] =
		try Yes(from(int)) catch {
			case _ :InternalException => No
		}

	/** Creates a normalized `Decimal64` instance equal to `significand * 10E-scale`.
	  * All trailing zeros in the significand are removed and instead its exponent is increased by that number,
	  * unless this would cause exponent (`scale`) to overflow, in which case the decimal point is moved left
	  * until the `scale` limit of `-128` (corresponding to an exponent of `128`) is reached.
	  * If the significand after normalization doesn't fall in the range of
	  * `[`[[net.noresttherein.sugar.numeric.Decimal64.MinUnscaled MinUnscaled]]`, `[[net.noresttherein.sugar.numeric.Decimal64.MaxUnscaled MaxUnscaled]]`]`,
	  * or if the `scale`, despite normalization, fails outside of
	  * `[`[[net.noresttherein.sugar.numeric.Decimal64.MinScale MinScale]]`,`[[net.noresttherein.sugar.numeric.Decimal64.MaxScale MaxScale]]`]`,
	  * then no such `Decimal64` exists, and `No` is returned.
	  *
	  * @param significand A value containing all leading non zero digits of the created `Decimal64`.
	  *                    The decimal representation of the whole number consists of the decimal format of `significand`,
	  *                    followed by `-scale` number of zeros (if `scale` is non negative), or of `significand`
	  *                    with a fractional point between `-scale`-th and `-scale + 1`-th least significant digit.
	  *                    If `scale > precision`, where `precision` is the number of digits in `significand`
	  *                    (without any leading zeros, except for `0`, which has a precision of `1`),
	  *                    then the number is represented by a `0.`, followed by `scale - precision` zeros,
	  *                    followed by the significand.
	  * @param scale       The number of least significant digits in `significand` which consist of the fractional
	  *                    portion of the created `Decimal64` (if `scale is positive)`, or the number of trailing zeros
	  *                    following the significand.
	  * @return `Yes(Decimal(significand, scale))`, or `No` if the former would throw an exception.
	  **/
	def exact(significand :Long, scale :Int) :Maybe[Decimal64] =
		try Yes(from(significand, scale)) catch {
			case _ :InternalException => No
		}

	/** Creates a `Decimal64` equal exactly to the given `Float`, using maximum available precision.
	  * If the argument cannot be represented exactly as a `Decimal64`, a `No` is returned.
	  * There is a subtle difference between this method and
	  * `Decimal64(decimal, Round.`[[net.noresttherein.sugar.numeric.Decimal64.Round.ExtendedExact ExtendedExact]]`)`:
	  * other methods accepting a `Double` create a `Decimal64` based on its decimal representation,
	  * while this method will attempt to create an instance ''exactly'' equal to the argument.
	  * If you wish to obtain the default behaviour, call `Decimal64.exact(decimal.toString)` instead.
	  **/
	def exact(decimal :Float) :Maybe[Decimal64] =
		try Yes(from(new JavaBigDecimal(decimal, MathContext.UNLIMITED), Round.ExtendedExact)) catch {
			case _ :InternalException => No
		}

	/** Creates a `Decimal64` equal exactly to the given `Double`, using maximum available precision.
	  * If the argument cannot be represented exactly as a `Decimal64`, `No` is returned.
	  * There is a subtle difference between this method and
	  * `Decimal64(decimal, Round.`[[net.noresttherein.sugar.numeric.Decimal64.Round.ExtendedExact ExtendedExact]]`)`:
	  * other methods accepting a `Double` create a `Decimal64` based on its decimal representation,
	  * while this method will attempt to create an instance ''exactly'' equal to the argument.
	  * If you wish to obtain the default behaviour, call `Decimal64.exact(decimal.toString)` instead.
	  **/
	def exact(decimal :Double) :Maybe[Decimal64] =
		try Yes(from(new JavaBigDecimal(decimal, MathContext.UNLIMITED), Round.ExtendedExact)) catch {
			case _ :InternalException => No
		}

	/** Creates a `Decimal64` equal exactly to the given `BigInt`, using maximum available precision.
	  * @return `Yes(Decimal64(big, Round.`[[net.noresttherein.sugar.numeric.Decimal64.Round.ExtendedExact ExtendedExact]]`))`,
	  *         or `No` if the former would throw an [[ArithmeticException]].
	  **/
	def exact(big :BigInt) :Maybe[Decimal64] =
		try Yes(from(big.bigInteger, Round.ExtendedExact)) catch {
			case _ :InternalException => No
		}

	/** Creates a `Decimal64` equal exactly to the given `BigInteger`, using maximum available precision.
	  * @return `Yes(Decimal64(big, Round.`[[net.noresttherein.sugar.numeric.Decimal64.Round.ExtendedExact ExtendedExact]]`))`,
	  *         or `No`, if the former would throw an [[ArithmeticException]].
	  **/
	def exact(big :BigInteger) :Maybe[Decimal64] =
		try Yes(from(big, Round.ExtendedExact)) catch {
			case _ :InternalException => No
		}

	/** Creates a `Decimal64` equal exactly to the given `BigDecimal`, using maximum available precision.
	  * @return `Yes(Decimal64(big, Round.`[[net.noresttherein.sugar.numeric.Decimal64.Round.ExtendedExact ExtendedExact]]`))`,
	  *         or `No`, if the former would throw an exception.
	  **/
	def exact(big :BigDecimal) :Maybe[Decimal64] =
		try Yes(from(big.bigDecimal, Round.ExtendedExact)) catch {
			case _ :InternalException => No
		}

	/** Creates a `Decimal64` equal exactly to the given `BigDecimal`, using maximum available precision.
	  * @return `Yes(Decimal64(big, Round.`[[net.noresttherein.sugar.numeric.Decimal64.Round.ExtendedExact ExtendedExact]]`))`,
	  *         or `No`, if the former would throw an exception.
	  **/
	def exact(big :JavaBigDecimal) :Maybe[Decimal64] =
		try Yes(from(big, Round.ExtendedExact)) catch {
			case _ :InternalException => No
		}

	/** Creates a `Decimal64` whose textual representation equals exactly the given `String`.
	  * @return `Yes(Decimal64(decimal))` or `No`, if the former would throw an exception.
	  **/
	def exact(decimal :String) :Maybe[Decimal64] =
		try Yes(from(new JavaBigDecimal(decimal, MathContext.UNLIMITED), Round.ExtendedExact)) catch {
			case _ :InternalException => No
		}


	/** Parses a `String` with a number in the decimal format and rounds it according
	  * to the [[java.math.MathContext MathContext]] argument. Accepted formats are the same
	  * as by [[java.math.BigDecimal java.math.BigDecimal]] constructor.
	  * This method is exactly equivalent to
	  * [[net.noresttherein.sugar.numeric.Decimal64.from(string:String)* round]]`(string)`,
	  * with the exception that [[net.noresttherein.sugar.numeric.Decimal64.Round.ExtendedExact ExtendedExact]]
	  * context is used as a default (if no implicit context is present),
	  * rather than [[net.noresttherein.sugar.numeric.Decimal64.Round.Extended Extended]],
	  * so the method will return `No` if the value cannot be represented exactly.
	  **/
	@throws[ArithmeticException]("if the parsed number exceeds both the precision of Decimal64 or the one from an implicit MathContext.")
	@throws[NumberFormatException]("if the string does not represent a valid decimal number.")
	def parse(string :String)(implicit mode :Optionally[MathContext]) :Maybe[Decimal64] =
		try Yes(from(string, mode getOrElse ExtendedExact)) catch {
			case _ :ArithmeticException | _ :NumberFormatException => No
		}


	@throws[InternalException]("if the value cannot be represented as a Decimal64 with the requested precision.")
	private def from(significand :Long, scale :Int) :Decimal64 =
		if (significand == 0)
			Zero
		else if (scale < MinScale) //try denormalizing by adding trailing zeros to the significand
			denormalize(significand, scale)
		else
			normalize(significand, scale)

	/** Implementation of both  `Decimal64.round(significand, scale)(mode)` and class method `round(mode)`.
	  * Passing precision `0` means that the value will be rounded to the maximum possible precision
	  * and no [[InternalException]] will be thrown unless rounding mode is `UNNECESSARY` and rounding is necessary.
	  **/
	@throws[InternalException]("if the value cannot be represented as a Decimal64 with the requested precision.")
	private def from(significand :Long, scale :Int, rounding :RoundingMode, precision :Int) :Decimal64 =
		if (significand == 0)
			Zero
		else if (scale > MaxScale + MaxPrecision | scale < MinScale - MaxPrecision) //for overflow/underflow
			throw ScaleOutOfRangeException
		else if (precision < 0)
			illegal_!(
				s"Cannot round $rounding ${significand}e${-scale} to a negative precision: $precision."
			)
		else {
			val Powers = LongPowersOf10
			val zeros = trailingZeros(significand)
			val normalized = significand / Powers(zeros)
			val normalizedScale = scale - zeros
			if (precision == ExtendedPrecision) {
				if (MinUnscaled <= normalized & normalized <= MaxUnscaled) //No rounding necessary
					validate(normalized, normalizedScale)
				else if (rounding == UNNECESSARY)
					throw RoundingNecessaryException
				else { //determine the number of digits to round and divide by the corresponding power of 10
					val minus = if (normalized <= 0) normalized else -normalized
					val digits = //digits >= MaxPrecision because significand does not fit in MinUnscaled..MaxUnscaled
						if (normalized == Long.MinValue) MaxLongPrecision
						else Decimal64.precision(-minus)
					var roundedDigits = digits - Precision
					if (roundedDigits <= 0)
						validate(normalized, normalizedScale)
					else {
						var pow = Powers(roundedDigits - 1) //0,1,10 or 100
						if  (minus < -MaxUnscaled * pow)
							pow = Powers(roundedDigits)
						else
							roundedDigits -= 1
						var result = divideLong(normalized, pow, rounding)
						if (result < MinUnscaled | result > MaxUnscaled) {
							roundedDigits += 1
							pow = Powers(roundedDigits)
							result = divideLong(normalized, pow, rounding)
						}
						from(result, normalizedScale - roundedDigits)
					}
				}
			} else {
				val digits =
					if (normalized == Long.MinValue) MaxLongPrecision
					else Decimal64.precision(Math.abs(normalized))
				val roundedDigits = digits - precision //<= 18
				if (roundedDigits <= 0)
					validate(normalized, normalizedScale)
				else {
					val pow = Powers(roundedDigits)
					if (rounding == UNNECESSARY && normalized % pow != 0)
						throw RoundingNecessaryException
					val unscaled = divideLong(normalized, pow, rounding)
					from(unscaled, normalizedScale - roundedDigits)
				}
			}
		}

	@throws[InternalException]("if the value cannot be represented as a Decimal64 with the requested precision.")
	@inline private def from(int :Long) :Decimal64 = from(int, 0)

	@throws[InternalException]("if the value cannot be represented as a Decimal64 with the requested precision.")
	@inline private def from(decimal :Float, mode :MathContext) :Decimal64 =
		from(new JavaBigDecimal(java.lang.Float.toString(decimal), mode), mode)

	@throws[InternalException]("if the value cannot be represented as a Decimal64 with the requested precision.")
	@inline private def from(decimal :Double, mode :MathContext) :Decimal64 =
		from(new JavaBigDecimal(java.lang.Double.toString(decimal), mode), mode)

	@throws[InternalException]("if the value cannot be represented as a Decimal64 with the requested precision.")
	private def from(int :BigInteger, mode :MathContext) :Decimal64 = {
		val signum = int.signum
		val isValidLong = (signum: @unchecked) match {
			case -1 => int.compareTo(BigMinLong) >= 0
			case  0 => true
			case  1 => int.compareTo(BigMaxLong) <= 0
		}
		if (isValidLong)
			from(int.longValueExact, 0, mode.getRoundingMode, mode.getPrecision)
		else if (mode.getRoundingMode == UNNECESSARY) {
			var unscaled = if (int.signum < 0) int.negate else int
			var scale = 0
			var qr :Array[BigInteger] = null
			while ({
				scale > MinScale && !unscaled.testBit(0) && {
					qr = unscaled.divideAndRemainder(BigInteger.TEN)
					qr(1).signum == 0
				}
			}) {
				unscaled = qr(0)
				scale -= 1
				qr = null
			}
			val isUnscaledLong = (signum: @unchecked) match {
				case -1 => unscaled.compareTo(BigMinLong) >= 0
				case  1 => unscaled.compareTo(BigMaxLong) <= 0
			}
			if (isUnscaledLong)
				from(unscaled.longValueExact, scale, mode.getRoundingMode, mode.getPrecision)
			else
				throw RoundingNecessaryException
		} else if (mode.getPrecision > MaxPrecision)
			throw PrecisionExceededException
		else {
			val precision = Decimal64.precision(int) //precision >= MaxPrecision because unscaledValue does not fit in a Long
			val toPrecision = mode.getPrecision
			var trim = precision - (if (toPrecision == ExtendedPrecision) MaxPrecision else toPrecision)
			if (trim > MaxExponent)
				throw ScaleOutOfRangeException
			val divisor = BigIntegerPowersOf10(trim)
			val qr      = int.divideAndRemainder(divisor)
			val quot    = qr(0).longValueExact
			val rem     = qr(1)
			val rounded =
				if (toPrecision != ExtendedPrecision)
					if (trim < MaxLongPrecision)
						roundDivision(quot, LongPowersOf10(trim), rem.longValueExact, mode.getRoundingMode)
					else
						roundDivision(quot, divisor, rem, mode.getRoundingMode)
				else if (MinUnscaled <= quot & quot <= MaxUnscaled) {
					val significand =
						if (trim < MaxLongPrecision)
							roundDivision(quot, LongPowersOf10(trim), rem.longValueExact, mode.getRoundingMode)
						else
							roundDivision(quot, divisor, rem, mode.getRoundingMode)
					if (MinUnscaled <= significand & significand <= MaxUnscaled)
						significand
					else {
						trim += 1
						roundDivision(quot / 10L, 10L, quot % 10L, rem.signum, mode.getRoundingMode)
					}
				} else if (trim == MaxExponent)
					throw ScaleOutOfRangeException
				else {
					trim += 1
					roundDivision(quot / 10L, 10L, quot % 10L, rem.signum, mode.getRoundingMode)
				}
			from(rounded, -trim)
		}
	}

	@throws[InternalException]("if the value exceeds both the precision of Decimal64 or the one specified by implicit MathContext.")
	private def from(big :JavaBigDecimal, mode :MathContext) :Decimal64 = {
		val signum = big.signum
		val scale = big.scale
		val isValidLong = (signum: @unchecked) match {
			case -1 => scale <= 0 && big.compareTo(DecimalMinLong) >= 0
			case  0 => true
			case  1 => scale <= 0 && big.compareTo(DecimalMaxLong) <= 0
		}
		if (isValidLong)
			from(big.longValueExact, 0, mode.getRoundingMode, mode.getPrecision)
		else if (big.precision < MaxLongPrecision)
			if (scale == 0) from(big.longValueExact, 0, mode.getRoundingMode, mode.getPrecision)
			else from(big.scaleByPowerOfTen(scale).longValueExact, scale, mode.getRoundingMode, mode.getPrecision)
		else {
			val normalized = big.stripTrailingZeros
			val isNormalizedLong =
				(normalized ne big) && normalized.scale <= 0 && ((signum: @unchecked) match {
					case -1 => normalized.compareTo(DecimalMinLong) >= 0
					case  1 => normalized.compareTo(DecimalMaxLong) <= 0
				})
			if (isNormalizedLong)
				from(normalized.longValueExact, normalized.scale, mode.getRoundingMode, mode.getPrecision)
			else {
				val unscaled = normalized.unscaledValue
				val isUnscaledLong =
					(normalized ne big) && ((signum: @unchecked) match {
						case -1 => unscaled.compareTo(BigMinLong) >= 0
						case  1 => unscaled.compareTo(BigMaxLong) <= 0
					})
				if (isUnscaledLong)
					from(unscaled.longValueExact, normalized.scale, mode.getRoundingMode, mode.getPrecision)
				else if (mode.getRoundingMode == UNNECESSARY)
					throw RoundingNecessaryException
				else if (mode.getPrecision == ExtendedPrecision) {
					//normalized.precision >= MaxPrecision because unscaled does not fit in a Long
					val precision = normalized.precision //precision > MaxPrecision because unscaledValue does not fit in a Long
					val rounded =
						if (precision == MaxLongPrecision) { //avoid the two step path of else when we know we won't fit in 17 digits
							normalized.round(to16digits(mode.getRoundingMode)).stripTrailingZeros
						} else {
							val res = normalized.round(to17digits(mode.getRoundingMode)).stripTrailingZeros
							val unscaled = res.unscaledValue
							val isValidUnscaled = (signum : @unchecked) match {
								case -1 => unscaled.compareTo(BigMinUnscaled) >= 0
								case  1 => unscaled.compareTo(BigMaxUnscaled) <= 0
							}
							if (isValidUnscaled)
								res
							else
								normalized.round(to16digits(mode.getRoundingMode)).stripTrailingZeros
						}
					from(rounded.unscaledValue.longValueExact, rounded.scale) //validates scale and strips any trailing zeros which appeared in unscaled
				} else if (mode.getPrecision > MaxPrecision)
					throw PrecisionExceededException
				else { //precision <= Precision, we can round at least the unscaled value
					val rounded = normalized.round(mode)
					from(rounded.unscaledValue.longValueExact, rounded.scale)
				}
			}
		}
	}

	@throws[InternalException]("if the value exceeds both the precision of Decimal64 or the one specified by implicit MathContext.")
	private def from(string :String, mode :MathContext) :Decimal64 =
		from(new JavaBigDecimal(string, mode), mode)


	/** Reads the first 64 bits out of the given input stream and interprets them as a `Decimal64`.
	  * The value should have been written with class method [[net.noresttherein.sugar.numeric.Decimal64.write write]]
	  * and is validated before returning.
	  **/
	@throws[IOException]("if the first 64 bits of the stream do not constitute a valid Decimal64.")
	def read(is :InputStream) :Decimal64 = {
		val hi = is.read
		val lo = is.read
		val bits = hi.toLong << 32 | lo & 0xffffffffL
		//new Decimal64(bits)
		val significand = bits >> 8
		val exponent = lo & 0xff
		val res = Decimal64(significand, exponent)
		if (res.bits != bits)
			io_!("Corrupt Decimal64: " + bits.toHexString + ".")
		res
	}





	implicit def intToDecimal64(int :Int) :Decimal64 = Decimal64(int)
	implicit def shortToDecimal64(short :Short) :Decimal64 = Decimal64(short)
	implicit def byteToDecimal64(byte :Byte) :Decimal64 = Decimal64(byte)

	//extracted because of conflicts after erasure
	@inline implicit def comparingDecimal64ToLong(decimal :Decimal64) :ComparingDecimal64ToLong =
		new ComparingDecimal64ToLong(decimal.bits)

	@inline implicit def comparingLongToDecimal64(long :Long) :ComparingLongToDecimal64 =
		new ComparingLongToDecimal64(long)

	@inline implicit def comparingDecimal64(decimal :Decimal64) :ComparingDecimal64 =
		new ComparingDecimal64(decimal.bits)


	class ComparingDecimal64(private val bits: Long) extends AnyVal {
		def compare(that :Decimal64) :Int =
			if (bits == 0L) -java.lang.Long.signum(that.bits)
			else if (that.bits == 0L) java.lang.Long.signum(bits)
			else if ((bits >= 0) != (that.bits >= 0)) java.lang.Long.signum(bits)
			else {
				val self = new Decimal64(bits)
				val m1 = self.unscaled; val e1 = -self.scale
				val m2 = that.unscaled; val e2 = -that.scale
				val sign = java.lang.Long.signum(bits)
				if (e1 == e2) java.lang.Long.compare(m1, m2) //covers this == 0 && that == 0
				else if (e1 - e2 >= MaxPrecision) sign
				else if (e2 - e1 >= MaxPrecision) -sign
				else {
					val p1 = self.unscaledDigits; val p2 = that.unscaledDigits
					if (e1 + p1 > e2 + p2) sign
					else if (e1 + p1 < e2 + p2) -sign
					//doesn't overflow because the result will have the same precision as m2 and Long.MaxValue > 10 * MaxUnscaled
					else if (e1 > e2) java.lang.Long.compare(m1 * LongPowersOf10(e1 - e2), m2)
					else java.lang.Long.compare(m1, m2 * LongPowersOf10(e2 - e1))
				}
			}
		@inline def < (that: Decimal64) :Boolean = (this compare that) <  0
		@inline def > (that: Decimal64) :Boolean = (this compare that) >  0
		@inline def <=(that: Decimal64) :Boolean = (this compare that) <= 0
		@inline def >=(that: Decimal64) :Boolean = (this compare that) >= 0
	}

	class ComparingDecimal64ToLong(private val bits :Long) extends AnyVal {
		def compare(that :Long) :Int = {
			val self = new Decimal64(bits)
			val sign = self.signum; val thatSign = java.lang.Long.signum(that)
			if (sign != thatSign)
				java.lang.Integer.compare(sign, thatSign)
			else if (sign == 0)
				0
			else if (self.isFraction)  //a cheaply computed case
				if (sign > 0)
					if (self.longValue >= that) 1 else -1
				else
					if (self.longValue <= that) -1 else 1
			else
				if (self < MinLongValue) -1
				else if (MaxLongValue < self) 1
				else java.lang.Long.compare(self.longValue, that)
		}
		@inline def < (that: Long) :Boolean = (new Decimal64(bits) compare that) <  0
		@inline def > (that: Long) :Boolean = (new Decimal64(bits) compare that) >  0
		@inline def <=(that: Long) :Boolean = (new Decimal64(bits) compare that) <= 0
		@inline def >=(that: Long) :Boolean = (new Decimal64(bits) compare that) >= 0
//		@inline def ==(that: Long) :Boolean = (new Decimal64(bits) compare that) == 0
	}

	class ComparingLongToDecimal64(private val self :Long) extends AnyVal {
		@inline def compare(that :Decimal64) :Int = -(that compare self)
		@inline def <=(that :Decimal64) :Boolean = that >= self
		@inline def < (that :Decimal64) :Boolean = that > self
		@inline def >=(that :Decimal64) :Boolean = that <= self
		@inline def > (that :Decimal64) :Boolean = that < self
		@inline def ==(that :Decimal64) :Boolean = (that compare self) == 0
	}

	sealed abstract class Decimal64IsNumeric(implicit mode :MathContext = Extended) extends Numeric[Decimal64] {
		override def plus(x :Decimal64, y :Decimal64) :Decimal64 = x + y
		override def minus(x :Decimal64, y :Decimal64) :Decimal64 = x - y
		override def times(x :Decimal64, y :Decimal64) :Decimal64 = x * y
		override def negate(x :Decimal64) :Decimal64 = -x
		override def fromInt(x :Int) :Decimal64 = Decimal64(x)

		override def toInt(x :Decimal64) :Int = x.toInt
		override def toLong(x :Decimal64) :Long = x.toLong
		override def toFloat(x :Decimal64) :Float = x.toFloat
		override def toDouble(x :Decimal64) :Double = x.toDouble

		override def compare(x :Decimal64, y :Decimal64) :Int = x compare y

		override def parseString(str :String) :Option[Decimal64] =
			try { Some(Decimal64(str, mode)) }
			catch { case _ :Exception => None }
	}

	@SerialVersionUID(Ver)
	sealed class Decimal64IsFractional(implicit mode :MathContext = Extended)
		extends Decimal64IsNumeric with Fractional[Decimal64]
	{
		override def div(x :Decimal64, y :Decimal64) :Decimal64 = x / y
	}
	@SerialVersionUID(Ver)
	object Decimal64IsFractional extends Decimal64IsFractional()(Extended)

	implicit def Decimal64IsFractional(implicit mode :MathContext = Extended) :Fractional[Decimal64] =
		if (mode == Extended) Decimal64.Decimal64IsFractional
		else new Decimal64.Decimal64IsFractional

	@SerialVersionUID(Ver)
	sealed class Decimal64AsIfIntegral(implicit mode :MathContext = Extended)
		extends Decimal64IsNumeric with Integral[Decimal64]
	{
		override def quot(x :Decimal64, y :Decimal64) :Decimal64 = x quot y
		override def rem(x :Decimal64, y :Decimal64) :Decimal64 = x % y
	}
	@SerialVersionUID(Ver)
	object Decimal64AsIfIntegral extends Decimal64AsIfIntegral()(Extended)

	def Decimal64AsIfIntegral(implicit mode :MathContext = Extended) :Integral[Decimal64] =
		if (mode == Extended) Decimal64.Decimal64AsIfIntegral
		else new Decimal64.Decimal64AsIfIntegral



	/** Extension method for `Int` and `Long` */
	trait implicits {
		implicit def scientificDecimalNotation(significand :Int) :IntScientificDecimal64Notation =
			new IntScientificDecimal64Notation(significand)

		implicit def scientificDecimalNotation(significand :Long) :LongScientificDecimal64Notation =
			new LongScientificDecimal64Notation(significand)
	}

	/** Extension method for `Int` and `Long`, creating a `Decimal64` using syntax resembling the scientific notation:
	  * `significand e exponent`.
	  **/
	@SerialVersionUID(Ver)
	object implicits extends implicits {
		class IntScientificDecimal64Notation(private val mantissa :Int) extends AnyVal {
			def e(exponent :Int) :Decimal64 = Decimal64(mantissa, -exponent)
		}
		class LongScientificDecimal64Notation(private val mantissa :Long) extends AnyVal {
			def e(exponent :Int) :Decimal64 = Decimal64(mantissa, -exponent)
		}
	}


	/** Adds a `toDecimal64` method to [[java.math.BigDecimal]]. */
	class JavaBigDecimalConverter(private val self :JavaBigDecimal) extends AnyVal {
		/** Creates a `Decimal64` whose value equals exactly `this` value.
		  * Throws an [[ArithmeticException]] if rounding would occur.
		  */
		@inline def toDecimal64Exact :Decimal64 = Decimal64(self)

		/** Returns a `Decimal64` which most closely matches this value. */
		@inline def toDecimal64 :Decimal64 = Decimal64.round(self)
	}

	/** Adds a `toDecimal64` method to [[BigDecimal]]. */
	class BigDecimalConverter(private val self :BigDecimal) extends AnyVal {
		/** Creates a `Decimal64` whose value equals exactly `this` value.
		  * Throws an [[ArithmeticException]] if rounding would occur.
		  **/
		@inline def toDecimal64Exact :Decimal64 = Decimal64(self)

		/** Returns a `Decimal64` which most closely matches this value. */
		@inline def toDecimal64 :Decimal64 = Decimal64.round(self)
	}




	private def scaleOutOfRangeException(significand :Long, scale :Long) :Nothing =
		throw SugaredArithmeticException(
			s"Unable to precisely represent ${significand}e${-scale} as a Decimal64: scale out of range."
		)
//	private def throwArithmeticException(x :Decimal64, method :String, e :Exception) :Nothing =
//		e match {
//			case _ :InternalException =>
//				throw SugaredArithmeticException(x.toString + "." + method + ": " + e.getMessage + ".")
//			case _ =>
//				throw SugaredArithmeticException("Error when executing " + x + "." + method +". " + e.getMessage)
//					.initCause(e)
//		}
	private def throwArithmeticException(x :Decimal64, op :String, y :Decimal64, mc :MathContext, e :Exception) :Nothing =
		e match {
			case _ :InternalException =>
				throw SugaredArithmeticException("Cannot calculate " + x + op + y + " (" + mc + "): " + e.getMessage + ".")
			case _ =>
				throw SugaredArithmeticException("Cannot calculate " + x + op + y + " (" + mc + "). " + e.getMessage)
					.initCause(e)
		}



	/** Reinterprets the binary representation of the given `Long` value as a `Decimal64`.
	  * No validation takes place, if `bits` is not a value returned by
	  * `Decimal64.`[[net.noresttherein.sugar.numeric.Decimal64.toLongBits toLongBits]], then the created object
	  * may not satisfy the invariants.
	  **/
	@inline private[sugar] def fromLongBits(bits :Long) :Decimal64 = new Decimal64(bits)

	/** Trusted constructor which combines the `mantissa` and `scale` into 64 bits, without any validation
	  * or normalization.
	  **/
	@inline private def make(significand :Long, scale :Long) :Decimal64 =
		new Decimal64(significand << ScaleBits | scale & ScaleMask)

	/** Creates a new `Decimal64` validating that `significand` and `scale` fall in their acceptable ranges.
	  * If `scale < MinScale`, it will attempt to denormalize the value by multiplying `significand` by a power of `10`
	  * to compensate, but it will not remove trailing zeros from `significand`, assuming instead
	  * that this has been done by the caller.
	  **/
	private def validate(significand :Long, scale :Int) :Decimal64 =
		if (scale < MinScale)
			denormalize(significand, scale)
		else if (scale > MaxScale)
			throw ScaleOutOfRangeException
		else if (significand > MaxUnscaled | significand < MinUnscaled)
			throw SignificandOutOfRangeException
		else
			make(significand, scale)

	/** Creates a `Decimal64` equal in value to `significand * 10^-scale`. All trailing zeros from `significand`
	  * are stripped and the value is rescaled accordingly, unless this would result in `scale` going out of range,
	  * in which case the result is rescaled only by the maximal number of zeros which can be stripped without
	  * causing an overflow. If it is impossible, an `InternalException` is thrown.
	  */
	private def normalize(significand :Long, scale :Int) :Decimal64 = {
		val zeros = trailingZeros(significand)
		val newScale = scale - zeros
		val normalized = significand / LongPowersOf10(zeros)
		validate(normalized, newScale)
	}
	/** Creates a `Decimal64` equal to `significand * 10^-scale`, where `scale < MinScale`.
	  * The result uses a scale of `MinScale` and a significand with `MinScale - scale` additional trailing zeros.
	  * If this would result in an overflow of the significand, an `InternalException` is thrown.
	  **/
	private def denormalize(significand :Long, scale :Int) :Decimal64 = {
		val log = MinScale - scale
		if (log > Precision)
			throw ScaleOutOfRangeException
		if (log < 0)
			make(significand, scale)
		val factor = LongPowersOf10(log)
		if (significand > MaxUnscaled / factor | significand < MinUnscaled / factor)
			throw SignificandOutOfRangeException
		make(significand * factor, MinScale)
	}
	/** Creates a `Decimal64` equal to `significand * 10^-scale`. If `significand` is out of range,
	  * it is scaled down by dropping a sufficient number of trailing zeros to reduce it
	  * to the `[MinUnscaled, MaxUnscaled]` range. If not enough trailing zeros are present,
	  * an `InternalException` is thrown.
	  **/
	private def scaleDown(significand :Long, scale :Int) :Decimal64 = {
		var result = significand
		var s = scale
		var power = 1L
		while (result < MinUnscaled | result > MaxUnscaled) {
			result /= 10L
			power *= 10L
			s -= 1
		}
		if (s != scale && significand - result * power != 0L)
			throw PrecisionExceededException
		Decimal64(result, s)
	}


	/** Overflow safe -value.abs. */
	@inline private def minusAbs(value :Long) :Long = (value >> 63) & value | (~value >> 63) & -value

//	/** If division of a dividend with precision `dividendPrecision` precision by a divisor
//	  * with `divisorPrecision` precision has a finite expansion, it is no longer than this value.
//	  **/
//	private def maxDivisionExpansion(dividendPrecision :Int, divisorPrecision :Int) :Int =
//		dividendPrecision + (math.ceil(10.0 * divisorPrecision) / 3.0).toInt


	/** The lowest power of 10 strictly greater than `significand`.
	  * It is the number of digits in the formatted significand, with zero consisting of one digit.
	  * Implementation adapted from `java.math.BigDecimal.longDigitLength`
	  * @param  significand a non-negative number.
	  * @return a value in the `[1, 19]` (`[1, LongPrecision+1]`) range.
	  **/
	@tailrec private def precision(significand :Long) :Int =
		/*
         * As described in "Bit Twiddling Hacks" by Sean Anderson,
         * (http://graphics.stanford.edu/~seander/bithacks.html)
         * integer log 10 of x is within 1 of (1233/4096)* (1 +
         * integer log 2 of x). The fraction 1233/4096 approximates
         * log10(2). So we first do a version of log2 (a variant of
         * Long class with pre-checks and opposite directionality) and
         * then scale and check against powers table. This is a little
         * simpler in present context than the version in Hacker's
         * Delight sec 11-4. Adding one to bit length allows comparing
         * downward from the LONG_TEN_POWERS_TABLE that we need
         * anyway.
         */
		if (significand < 0L)
			precision(-significand)
		else if (significand < 10L) // must screen for 0, might as well 10
			1
		else {
			val log   = ((64 - java.lang.Long.numberOfLeadingZeros(significand) + 1) * 1233) >>> 12
			// if r >= length, must have max possible digits for long// if r >= length, must have max possible digits for long
			if (log >= MaxLongPrecision || significand < LongPowersOf10(log)) log
			else log + 1
		}

	/** The lowest power of 10 strictly greater than `significand`.
	  * It is the number of digits in the formatted significand, with zero consisting of one digit.
	  * Implementation adapted from `java.math.BigDecimal.bigDigitLength`
	  * @param significand a non-negative number.
	  * @return a value in the `[1, 19]` (`[1, LongPrecision+1]`) range.
	  * */
	@tailrec private def precision(significand :BigInteger) :Int = significand.signum match {
		case -1 => precision(significand.negate)
		case  0 => 1
		case _ =>
		val log = (((significand.bitLength.toLong + 1) * 646456993) >>> 31).toInt
		val pow10 = if (log <= MaxExponent) BigIntegerPowersOf10(log) else BigInteger.TEN.pow(log)
		if (significand.compareTo(pow10) < 0) log
		else log + 1
	}

	/** The maximum power of `10` by which `significand` can be multiplied within `Long` (not `Decimal64`) precision. */
	private def maxLeftShift(significand :Long) :Int = {
		val shift = LongPrecision - precision(significand)
		if (shift < 0) 0
		else {
			val scaled = significand * LongPowersOf10(shift)
			if (scaled <= Long.MaxValue / 10L) shift + 1
			else shift
		}
	}

	/** Calculates the number of trailing zeros in the decimal representation of `unscaled`.
	  * Returns `0` for `0`.
	  **/
	private def trailingZeros(significand :Long) :Int =
		if ((significand & 1L) != 0L && significand % 10L != 0L)
			0
		else {
			var lo = 0
			var hi = LongPrecision
			val powers = LongPowersOf10
			while (lo < hi) { //bin search for the highest power of 10 which is a divisor of m
				val a = (hi + lo + 1) / 2
				if (significand % powers(a) == 0L) lo = a
				else hi = a - 1
			}
			lo
		}



	/** Overflow safe signum of `2*rem - divisor`. If it equals `-1`, result should be rounded down; if `1` - up,
	  * and 0 states that `rem` exactly equals `divisor/2`. This method is used to preliminarily
	  * determine the rounding direction.
	  **/
	private def roundingWeight(rem :Long, divisor :Long) :Int =
		if (rem == 0)
			-1
		else if (rem > (Long.MaxValue >> 1) | rem < (Long.MinValue >> 1))
			1  //2*rem does not fit in Long, while divisor does
		else { //compare negative values to avoid overflow
			val minusRem = if (rem < 0) rem << 1 else -rem << 1       //don't divisor >> 1 else divisor may be odd
			val minusDivisor = if (divisor < 0) divisor else -divisor
			//switched signs because we compare negatives
			if (minusRem > minusDivisor) -1 else if (minusRem == minusDivisor) 0 else 1
		}

	/** Compares `2 * rem` with `divisor`, returning `-1`, `0`, or `1`, depending on their relative magnitudes.
	  **/
	private def roundingWeight(rem :BigInteger, divisor :BigInteger) :Int = rem.signum match {
		case -1 => divisor.signum match {
			case -1 => -BigInteger.TWO.multiply(rem).compareTo(divisor)
			case  1 => BigMinusTwo.multiply(rem).compareTo(divisor)
		}
		case  0 => -1
		case  1 => divisor.signum match {
			case -1 => -BigMinusTwo.multiply(rem).compareTo(divisor)
			case  1 => BigInteger.TWO.multiply(rem).compareTo(divisor)
		}
	}

	/** Rounds according to the given mode a result of an integral division based on the value of its remainder.
	  * The arguments should be initialized in a manner consistent with the following code:
	  * {{{
	  *     val quot = dividend / div
	  *     val rem  = dividend % div
	  * }}}
	  * The method compares `2*rem` with `div` and optionally increases the absolute magnitude of `quot` by `1`,
	  * depending on the result and the rounding mode. An `InternalException` is thrown if rounding is necessary
	  * but `rounding` equals `UNNECESSARY`. This method does not check for overflow/underflow if `quot` equals
	  * `Long.MinValue` or `Long.MaxValue`.
	  **/
	@inline private def roundDivision(quot :Long, div :Long, rem :Long, rounding :RoundingMode) :Long =
		roundDivision(quot, div, rem, 0L, rounding)

	/** Rounds a result of two integral divisions based on their remainders, according to the given rounding mode.
	  * It is the rounding procedure taken in cases equivalent to the following one:
	  * {{{
	  *     val quot1  = dividend / div1
	  *     val rem1   = dividend % div1
	  *     val quot   = quot1 / div
	  *     val rem    = quot1 % div
	  *     val result = roundDivision(quot, div, rem, rem1, rounding)
	  * }}}
	  * The second remainder (`rem1` in the above example) does not need to be an exact remainder. Instead,
	  * if it is not zero, and `2 * rem == div`, then `quot` is rounded up if `rounding` is any of the rounding modes
	  * dependent on the remainder.
	  * if rounding is `UNNECESSARY` but at least one of `rem1`, `rem2` is non zero, then an `InternalException` is thrown.
	  **/
	private def roundDivision(quot :Long, div :Long, rem :Long, weightOnEven :Long, rounding :RoundingMode) :Long =
		if (rem == 0L & weightOnEven == 0L)
			quot
		else {
			val sign = java.lang.Long.signum(quot)
			val adjust = rounding match {
				case DOWN        => 0
				case UP          => sign
				case FLOOR       => if (sign < 0) sign else 0
				case CEILING     => if (sign > 0) sign else 0
				case UNNECESSARY => throw RoundingNecessaryException
				case _           => roundingWeight(rem, div) match {
					case -1 => 0      //round down
					case  1 => sign
					case  _ =>
						if (weightOnEven != 0L)
							sign
						else rounding match {
							case HALF_DOWN => 0
							case HALF_UP   => sign
							case _ => if ((quot & 1L) == 0L) 0 else sign
						}
				}
			}
			quot + adjust
		}

	/** Rounds according to the given mode a result of an integral division based on the value of its remainder.
	  * The arguments should be initialized in a manner consistent with the following code:
	  * {{{
	  *     val quot = dividend / div
	  *     val rem  = dividend % div
	  * }}}
	  * The method compares `2*rem` with `div` and optionally increases the absolute magnitude of `quot` by `1`,
	  * depending on the result and the rounding mode. An `InternalException` is thrown if rounding is necessary
	  * but `rounding` equals `UNNECESSARY`. This method does not check for overflow/underflow if `quot` equals
	  * `Long.MinValue` or `Long.MaxValue`.
	  **/
	@inline private def roundDivision(quot :Long, div :BigInteger, rem :BigInteger, rounding :RoundingMode) :Long =
		roundDivision(quot, div, rem, 0, rounding)

	/** Rounds a result of two integral divisions based on their remainders, according to the given rounding mode.
	  * It is the rounding procedure taken in cases equivalent to the following one:
	  * {{{
	  *     val quot1  = dividend / div1
	  *     val rem1   = dividend % div1
	  *     val quot   = quot1 / div
	  *     val rem    = quot1 % div
	  *     val result = roundDivision(quot, div, rem, rem1, rounding)
	  * }}}
	  * The second remainder (`rem1` in the above example) does not need to be an exact remainder. Instead,
	  * if it is not zero, and `2 * rem == div`, then `quot` is rounded up if `rounding` is any of the rounding modes
	  * dependent on the remainder.
	  * if rounding is `UNNECESSARY` but at least one of `rem1`, `rem2` is non zero, then an `InternalException` is thrown.
	  **/
	private def roundDivision(quot :Long, div :BigInteger, rem :BigInteger, weightOnEven :Int, rounding :RoundingMode)
			:Long =
		if (weightOnEven == 0L && rem.signum == 0)
			quot
		else {
			val sign = java.lang.Long.signum(quot)
			val adjust = rounding match {
				case DOWN        => 0
				case UP          => sign
				case FLOOR       => if (sign < 0) sign else 0
				case CEILING     => if (sign > 0) sign else 0
				case UNNECESSARY => throw RoundingNecessaryException
				case _           => roundingWeight(rem, div) match {
					case -1 => 0      //round down
					case  1 => sign
					case  _ =>
						if (weightOnEven != 0L)
							sign
						else rounding match {
							case HALF_DOWN => 0
							case HALF_UP   => sign
							case _ => if ((quot & 1L) == 0L) 0 else sign
						}
				}
			}
			quot + adjust
		}

	/** Rounds `quot` to the maximum precision allowable by a `Decimal64`. the rounding direction is determined based on
	  * the given rounding mode and relative magnitude of `div` and `2*rem`, assumed to be the divisor of the division
	  * resulting in quot and the remainder of that division.
	  */
	private def roundToMaxPrecision(quot :Long, div :Long, rem :Long, scale :Int, rounding :RoundingMode)
			:Decimal64 =
	{
		var result = roundDivision(quot, div, rem, rounding)
		var power = 10L
		var scale2 = scale
		while (result < MinUnscaled | result > MaxUnscaled) {
			val q = quot / power
			val r = quot - q * power
			result = roundDivision(q, power, r, rem, rounding)
			power *= 10L
			scale2 -= 1
		}
		Decimal64(result, scale2)
	}

	/** Rounds the result `quot` of a division by `div` with remainder `rem` and returns as a `Decimal64`
	  * with an exponent of `exponent`. If `quot`, after rounding, exceeds the significand precision,
	  * last digits are truncated until the result is within `[MinUnscaled, MaxUnscaled]` range;
	  * if any of the truncated digits is not zero, an `InternalException` is thrown.
	  * If `scale` is out of range, an `ArithmeticException` is thrown.
	  **/
	@inline private def roundAndScale(quot :Long, div :Long, rem :Long, scale :Int, rounding :RoundingMode) :Decimal64 =
		roundAndScale(quot, div, rem, 0L, scale, rounding)

	/** Rounds the result `quot` of a division by `div` with remainder `rem` and returns as a `Decimal64`
	  * with an exponent of `exponent`. Rounding is based on a comparison between `rem` and `2*div`; however,
	  * if `trail` is non zero, then equal results - that is cases, where the result is equidistant
	  * from two neighbouring integers - round 'up'. If `quot`, after rounding, exceeds the significand precision,
	  * last digits are truncated until the result is within `[MinUnscaled, MaxUnscaled]` range;
	  * if any of the truncated digits is not zero, an `InternalException` is thrown.
	  * If `scale` is out of range, an `ArithmeticException` is thrown.
	  **/
	private def roundAndScale(quot :Long, div :Long, rem :Long, trail :Long, scale :Int, rounding :RoundingMode)
			:Decimal64 =
	{
		val unscaled = roundDivision(quot, div, rem, trail, rounding)
		var result = unscaled
		var scale2 = scale
		var power = 1L
		while (result < MinUnscaled | result > MaxUnscaled) {
			result /= 10L
			power *= 10L
			scale2 -= 1
		}
		if (scale2 != scale && unscaled - result * power != 0L)
			throw PrecisionExceededException
		Decimal64(result, scale2)
	}


	/** Rounds a value equal to `(hi * MaxLongPowerOf10 + lo) * 10^-scale` to the specified precision and according
	  * to the given rounding mode. The signs of `hi` and `lo` must be equal.
	  */
	private def roundDoubleLong(hi :Long, lo :Long, scale :Int, rounding :RoundingMode, precision :Int) :Decimal64 = {
		val exponent = -scale
		val Powers = LongPowersOf10
		val hiPrecision :Int =
			if (hi == Long.MinValue) MaxLongPrecision
			else Decimal64.precision(Math.abs(hi))
		if (hi == 0L)
			Decimal64(lo, scale, rounding, precision)
		else if (lo == 0L)
			Decimal64(hi, -exponent - LongPrecision, rounding, precision)
		else if (precision == ExtendedPrecision) {
			if (hiPrecision >= MaxPrecision)
				//we have all the digits we need in hi, however unlikely - determine the maximum possible precision
				roundToMaxPrecision(hi, MaxLongPowerOf10, lo, -(exponent + LongPrecision), rounding)
			else if (hiPrecision == Precision && { val limit = MaxUnscaled / 10L; hi < -limit | hi > limit }) {
				//hi already has maximum possible precision for its value, no digits to drop, no digits to add
				val result = roundDivision(hi, MaxLongPowerOf10, lo, rounding)
				Decimal64(result, -exponent - LongPrecision)
			} else {
				//the result consists of hi followed by some of the higher digits of lo
				val loDigits = MaxPrecision - hiPrecision
				val droppedLoDigits = LongPrecision - loDigits
				val loDivisor = Powers(droppedLoDigits)
				val result = hi * Powers(loDigits) + lo / loDivisor
				roundToMaxPrecision(result, loDivisor, lo % loDivisor, -(exponent + droppedLoDigits), rounding)
			}
		} else if (precision < hiPrecision) {
			val droppedDigits = hiPrecision - precision
			val e = exponent + LongPrecision + droppedDigits
			divideAndScale(hi, Powers(droppedDigits), lo, e, rounding)
		} else if (precision == hiPrecision)
			roundAndScale(hi, MaxLongPowerOf10, lo, -(exponent + LongPrecision), rounding)
		else if (precision <= LongPrecision) {              //hiPrecision < precision; we can round in a `Long`
			val neededLoDigits = precision - hiPrecision      //0 < neededLoDigits <= LongPrecision - 1
			val droppedLoDigits = LongPrecision - neededLoDigits
			val power = Powers(droppedLoDigits)
			val rescaledLo = lo / power
			val nonRounded = hi * Powers(neededLoDigits) + lo / power
			roundAndScale(nonRounded, power, lo - rescaledLo * power, -(exponent + droppedLoDigits), rounding)
		} else if (hiPrecision >= MaxPrecision) {             //precision > hiPrecision; take only digits from hi
			val droppedHiDigits = hiPrecision - MaxPrecision
			if (precision >= hiPrecision + LongPrecision) {
				//both hi and lo within precision, but lo != 0 and already hiPrecision >= MaxPrecision
				throw PrecisionExceededException
			} else { //hiPrecision < precision < hiPrecision + LongPrecision
				//We need to round on a digit somewhere within `lo`, all higher digits (and lower hi digits) must be 0.
				//Lets round `lo` as a stand alone number, carry over potential overflow to hi, and then truncate hi.
				val droppedLoDigits = hiPrecision + LongPrecision - precision
				val roundedLo = divideLong(lo, Powers(droppedLoDigits), rounding)
				val carryOverPower = Powers(LongPrecision - droppedLoDigits)
				val carry = roundedLo / carryOverPower
				val rem = roundedLo - carry * carryOverPower
				val result = hi + carry //can this overflow?
				if (rem != 0L)
					throw PrecisionExceededException
				divideAndScaleExact(result, Powers(droppedHiDigits), exponent + droppedLoDigits)
			}
		} else {
			val takenLoDigits = MaxPrecision - hiPrecision    //0 < takenLoDigits < LongPrecision
			val droppedLoDigits = LongPrecision - takenLoDigits
			val loDivisor = Powers(droppedLoDigits)
			val rem =
				if (precision >= hiPrecision + LongPrecision) //hiPrecision < MaxPrecision
					//both hi and lo within precision - no rounding allowed
					lo
				else//hiPrecision < MaxPrecision < LongPrecision < precision < hiPrecision + LongPrecision
					//Some of lo digits are within precision, but not all - need to round lo
					divideLong(lo, loDivisor, rounding)
			val result = hi * Powers(takenLoDigits) + rem / loDivisor
			if (rem % loDivisor != 0L)
				throw PrecisionExceededException
			scaleDown(result, -(exponent + droppedLoDigits))
		}
	}




	/** Divides `dividend / divisor` using a single `Long` division operation,
	  * and rounds the last digit of the result in the given mode.
	  **/
	private def divideLong(dividend :Long, divisor :Long, rounding :RoundingMode) :Long = {
		var quot = dividend / divisor
		val rem = dividend - quot * divisor
		if (rounding != DOWN & rem != 0L)
			quot = roundDivision(quot, divisor, rem, rounding)
		quot
	}

	/** Returns `dividend / divisor * 10^exponent`, rounded according to the given mode.
	  * Division happens in `Long` precision and rounding is on the first truncated digit.
	  * Afterwards, the value is scaled to fit in the `Decimal64` significand range; if the truncated digits
	  * are not zero, an `InternalException` is thrown.
	  * This method is used when the result is known to have the exactly the required precision.
	  **/
	@inline private def divideAndScale(dividend :Long, divisor :Long, exponent :Int, rounding :RoundingMode) :Decimal64 =
		Decimal64(divideLong(dividend, divisor, rounding), -exponent)

	/** Returns a `Decimal64` equal exactly to `dividend / divisor * 10^exponent`.
	  * If the remainder of the division is not zero, an `InternalException` is thrown.
	  * If the result significand is out of range, it is rescaled by dropping a sufficient number of trailing zeros
	  * and increasing the exponent. If this is not possible due to a lack of trailing zeros in the quotient,
	  * an `InternalException` is thrown.
	  **/
	private def divideAndScaleExact(dividend :Long, divisor :Long, exponent :Int) :Decimal64 = {
		val unscaled = dividend / divisor
		val rem = dividend - unscaled * divisor
		if (rem != 0L)
			throw PrecisionExceededException
		Decimal64(unscaled, -exponent)
	}

	/** Returns `dividend / divisor * 10^exponent`, rounded according to the highest precision with which
	  * the result can fit in the significand range and according to the specified mode.
	  * Division happens in `Long` precision and the value is rounded on the first truncated digit.
	  * If the result exceeds the significand range, the step is repeated with dividend divided by 10
	  * and two tier rounding until the result fits in the `Decimal64` precision.
	  * This method is used when the requested precision is zero, signifying maximum possible precision.
	  **/
	@inline private def divideToMaxPrecision(dividend :Long, divisor :Long, exponent :Int, rounding :RoundingMode)
			:Decimal64 =
		divideToMaxPrecision(dividend, divisor, 0L, exponent, rounding)

	/** Returns `dividend / divisor * 10^exponent`, rounded according to the highest precision with which
	  * the result can fit in the significand range and according to the specified mode.
	  * Division happens in `Long` precision and the value is rounded on the first truncated digit.
	  * If `trail` is non zero, it influences the rounding weight and all 'half' cases round 'up'.
	  * If the result exceeds the significand range, the step is repeated with dividend divided by 10
	  * and two tier rounding until the result fits in the `Decimal64` precision.
	  * This method is used when the requested precision is zero, signifying maximum possible precision.
	  **/
	private def divideToMaxPrecision(dividend :Long, divisor :Long, trail :Long, exponent :Int, rounding :RoundingMode)
			:Decimal64 =
	{
		var unscaled = dividend / divisor
		val rem = dividend - unscaled * divisor
		if (rounding != DOWN && (rem != 0L | trail != 0L))
			unscaled = roundDivision(unscaled, divisor, rem, trail, rounding)
		var result = unscaled
		var e = exponent
		var div = divisor
		while (result < MinUnscaled | result > MaxUnscaled) {
			div *= 10L
			val q = dividend / div
			val r = dividend - q * divisor
			result = roundDivision(q, div, r, trail, rounding)
			e += 1
		}
		Decimal64(result, -e)
	}

	/** Performs a division `dividend / divisor` within `Long` precision and using
	  * [[net.noresttherein.sugar.numeric.Decimal64.roundDivision two-tier rounding]] with `tail` as the second remainder.
	  * The result will equal `(dividend / divisor) * 10^exponent`, rounded to the precision of integral `Long` division
	  * between `dividend` and `divisor` and according to the given rounding mode.
	  */
	private def divideAndScale(dividend :Long, divisor :Long, tail :Long, exponent :Int, rounding :RoundingMode)
			:Decimal64 =
	{
		var unscaled = dividend / divisor //divideLong(dividend, divisor, rounding)
		val rem = dividend - unscaled * divisor
		if (rounding != DOWN && (rem != 0L | tail != 0L))
			unscaled = roundDivision(unscaled, divisor, rem, tail, rounding)
		Decimal64(unscaled, -exponent)
	}


	/** Shared division implementation for adjusted arguments. Slow divide process computing digits by a handful,
	  * iteratively shifting left and dividing the remainder of `dividend` as a `Long`.
	  * @param dividend   A non zero significand of the dividend, with a precision not exceeding MaxPrecision.
	  * @param divisor    A non zero significand of the divisor.
	  * @param exponent   The exponent of the result, after adjusting (shifting) `dividend` and `divisor`.
	  * @param rounding   the rounding mode used for the result. If `UNNECESSARY` is specified,
	  *                   but rounding is required, `RoundingNecessary` exception is thrown.
	  * @param precision  The number of the calculated digits before rounding. If precision exceeds this maximum,
	  *                   an `InternalException` is thrown. Zero signifies the maximum possible number of 16 or 17,
	  *                   in which case exception is not thrown unless the rounding mode is `UNNECESSARY`.
	  **/
	@throws[InternalException]("if there are non-zero digits after precision digits in the decimal expansion of the result.")
	@throws[ArithmeticException]("if the calculated result does not fit in a Decimal64 after rounding.")
	private def divideByDigits(dividend :Long, divisor :Long, exponent :Int, rounding :RoundingMode, precision :Int)
			:Decimal64 =
	{
		//Note (0): let q = x quot y; r = x rem y; then x/y = (q + r/y)
		//Note (1): the digits of q and r/y do not overlap
		val Powers = LongPowersOf10
		val sign = if ((dividend > 0) == (divisor > 0)) 1L else -1L //because dividend != 0 && divisor != 0
		var shift = LongPrecision - this.precision(Math.abs(dividend))
		val scaled = dividend * Powers(shift)             //pad dividend with zeros so that the quotient has more digits
		var e = exponent - shift
		val div =                                         //normalize divisor and decrease the exponent if necessary
			if ((divisor & 1L) != 0L && divisor % 10L == 0L) {
				val log = Decimal64.trailingZeros(divisor / 10L) + 1
				e -= log
				divisor / Powers(log)                     //divide m2 by its highest power of 10 divisor
			} else
				divisor                                   //the exact result equals (dividend / div)*10^e
		var q = scaled / div
		var r = scaled - q * div

		var resultPrecision =                             //precision of the result - optimistically assume MaxPrecision
			if (precision == ExtendedPrecision) MaxPrecision
			else math.min(MaxPrecision, precision)
		var remainingDigits = resultPrecision
		remainingDigits -= Decimal64.precision(sign * q)  //for the digits we have in q
		shift = remainingDigits                           //declared here for the sake of the loop invariant only
		e -= shift                                        //we'll multiply q by 10^shift subsequently
		var quot = 0L
		while (r != 0L & remainingDigits > 0) {           //repeat dividing the remainder by the divisor
			/* let qDigits = maxPrecision - shift
			 * Invariant (1): result equals dividend/divisor on all digits above remainingDigits + qDigits.
			 * Invariant (2): lower qDigits digits of q equal digits [remainingDigits, remainingDigits + qDigits)
			 *                of dividend/divisor.
			 * Corollary: result + q * Powers(remainingDigits) equals dividend/divisor on all digits
			 *            above remainingDigits.
			 */
			quot += q * Powers(remainingDigits)           //First, rescale the ready division result
			//We require the most significant remainingDigits from r/divisor and r quot divisor == 0, so we need
			// to shift r left by remainingDigits. We might not have enough precision, so lets take what we can.
			shift = Decimal64.maxLeftShift(Math.abs(r))   //we'll take that many digits from r / divisor
			r *= Powers(shift)
			q = r / div
			r = r - q * div
			remainingDigits -= shift                      //divisor < MaxUnscaled => shift > 0 => the loop ends
		}                                                 //remainingDigits is the number of digits we don't take from q
		/* Together with quot concat q now have all the digits we need. However, if remainingDigits < 0,
		 * then we must add only leading -remainingDigits of q, and we end up with two remainders: lower digits of q
		 * and r itself. To complicate matters further, if precision == ExtendedPrecision, we have optimistically
		 * assumed that the result will have MaxPrecision digits; only now we can learn the truth,
		 * and it might be we need to round the last digit off. It gets worse: it is theoretically possible that,
		 * after rounding, the result will exceed either the requested precision, or the unscaled range,
		 * in which case we must redo the rounding on a higher digit.
		 * What we cannot do is to cascade the rounding, applying more than one: if rounding is needed, it must happen
		 * once, on the highest truncated position of q.
		 */
		if (remainingDigits > 0) {                        //exact division, last remainingDigits are zeros. We are done.
			quot += q * Powers(remainingDigits)
			Decimal64(quot, -e)                           //no rounding necessary, as we have at least one zero to spare
		} else if (remainingDigits == 0 & r == 0L) {      //exact, but we still might have to round a single digit
			quot += q
			Decimal64(quot, -e, rounding, precision)
		} else {
			/* q has a higher precision than we need; get only the highest precision(q) + remainingDigits
			 * Determine if the last digit of the result must be rounded due to exceeded precision.
			 * last power of 10 digit in q which is still a part of result
			 */
			//a power of 10 corresponding to the digit in q which becomes the lowest digit in result
			var lastQuotDigitPower = Powers(-remainingDigits)
			var result = quot + q / lastQuotDigitPower
			if (result < MinUnscaled | result > MaxUnscaled) {
				//remainingDigits *before* current q was > 0, so the digit we drop came from q
				resultPrecision -= 1
				remainingDigits -= 1
				lastQuotDigitPower *= 10L
				result /= 10L
				e += 1
			}
			var rem = q % lastQuotDigitPower
			if (rem == 0L && r == 0L)
				Decimal64(result, -e)
			else {
				//we round after resultPrecision digits rather than after precision.
				//however, we assert that all digits after resultPrecision are zero, hence the result is the same
				result =
					if (remainingDigits == 0) roundDivision(result, div, r, rounding)
					else roundDivision(result, lastQuotDigitPower, rem, r, rounding)
				if (result < MinUnscaled | result > MaxUnscaled) { //rounding got us out of range, round again
					//if we enter this branch, then result must have been in range and we don't need to divide quot
					resultPrecision -= 1
					remainingDigits -= 1
					lastQuotDigitPower *= 10L
					e += 1
					rem = q % lastQuotDigitPower
					result = quot + q / lastQuotDigitPower
					result = roundDivision(result, lastQuotDigitPower, rem, r, rounding)
				}
				//if we are under the requested precision, then all digits following result should be zero
				if (precision > resultPrecision) {
					//the combined total number of digits in result and rem
					val resultWithRemPrecision = resultPrecision - remainingDigits
					if (precision <= resultWithRemPrecision) {
						if (rem / Powers(resultWithRemPrecision - precision) != 0L)
							throw if (sign > 0) OverflowException else UnderflowException
					} else {
						if (rem != 0L)
							throw if (sign > 0) OverflowException else UnderflowException
						val underPrecision = precision - resultWithRemPrecision
						val trailing = divideByDigits(r, div, underPrecision, rounding, underPrecision).whole
						if (trailing.bits != 0)
						//we could try to compute also the higher digits of r, but it's tricky,
						// as div was not a power of 10 - it would require essentially continuation of the loop
							throw if (sign > 0) OverflowException else UnderflowException
					}
				}
				Decimal64(result, -e)
			}
		}
	}



	/** Adds to decimal numbers, given as `xSignificand * 10^xExponent` and `ySignificand * 10^yExponent`
	  * to the given precision and according to the given rounding mode. Assumes `yExponent <= xExponent`
	  * and `xSignificand, ySignificand != 0`. This is an implementation method to which class methods
	  * `+`, `-`, `plus` and `minus` delegate.
	  **/
	private def sum(xSignificand :Long, xExponent :Int, ySignificand :Long, yExponent :Int,
	                rounding :RoundingMode, precision :Int) :Decimal64 =
	{
		val Powers = LongPowersOf10
		//try to bring the exponents as close to each other as possible by scaling the significands
		var m1 = xSignificand; var m2 = ySignificand
		var e1 = xExponent; var e2 = yExponent
		var expDiff = e1 - e2
		var p1 = 0
		if (expDiff > 0) {
			p1 = Decimal64.precision(Math.abs(m1))
			var raise = LongPrecision - p1
			if (raise > expDiff)
				raise = expDiff
			else if (raise < expDiff) {
				if ((m2 & 1L) == 0L && m2 % 10L == 0L) {
					expDiff -= raise
					var lower = trailingZeros(m2 / 10L) + 1
					if (lower > expDiff)
						lower = expDiff
					if (lower > 0) {
						m2 /= Powers(lower)
						e2 += lower
						expDiff -= lower
					}
				}
			}
			m1 *= Powers(raise)
			e1 -= raise
		}
		if (expDiff == 0)                                  //plain sum, exponents match
			sum(m1, m2, e1, rounding, precision)
		else if (expDiff <= LongPrecision)                 //significands can overlap when scaled to the same powers of 10
			sum128(m1, e1, m2, e2, rounding, precision)
		else if (xSignificand <= 0 == ySignificand <= 0) { //there is no overlap between non-zero digits of x and y
			var sign = 1
			var x = xSignificand
			var y = ySignificand
			if (xSignificand < 0) {
				sign = -1
				x = -xSignificand
				y = -ySignificand
			}
			if (p1 == 0)
				p1 = Decimal64.precision(x)
			val p2 = Decimal64.precision(y)
			sumDifferentMagnitudes(sign, x, xExponent, p1, y, yExponent, p2, rounding, precision)
		} else {                                           //x and y do not overlap and have opposite signs
			var sign = 1
			var x = xSignificand
			var y = -ySignificand
			if (xSignificand < 0) {
				sign = -1
				x = -xSignificand
				y = ySignificand
			}
			if (p1 == 0)
				p1 = Decimal64.precision(x)
			val p2 = Decimal64.precision(y)
			subtractDifferentMagnitudes(sign, x, xExponent, p1, y, yExponent, p2, rounding, precision)
		}
	}


	/** Calculates the sum of `xSignificand * 10^exponent + ySignificand * 10^exponent`,
	  * rounding according to the given mode. The result may have a different exponent than the shared exponent
	  * of the summands if precision is exceeded. The result is correct even if `Long` overflows/underflows.
	  **/
	private def sum(xSignificand :Long, ySignificand :Long, exponent :Int, rounding :RoundingMode, precision :Int)
			:Decimal64 =
	{
		val Powers = LongPowersOf10
		val total = xSignificand + ySignificand
		if (((xSignificand ^ total) & (ySignificand ^ total)) >= 0L) //no overflow, according to java.lang.Math.addExact
			Decimal64(total, -exponent, rounding, precision)
		else {
			//overflow => total < 0, total >>> 1 is unsigned div 2; underflow => total > 0 => we insert the sign bit
			val half = (total >>> 1) | (total ^ Long.MinValue)
			val div10 = half / 5L
			val totalDiv10rem =
				if ((total & 1L) == 0L) //if a mod 5 = r then 2a mod 10 = 2r
					Math.abs(2 * (half - 5L * div10))
				else                    //if a mod 5 = r then 2a + 1 mod 10 = 2r + 1 mod 10
					Math.abs(2 * (half - 5L * div10)) match {
						case 9 => 0
						case n => n + 1
					}
			val div10Precision = if (div10 >= MaxLongPowerOf10) MaxLongPrecision else LongPrecision
			var precisionDelta = div10Precision - MaxPrecision
			var divisor = Powers(precisionDelta)
			var nonRounded = div10 / divisor
			var maxPrecision = MaxPrecision //maximum final precision of the result (unless rounding brings us over it)
			if (nonRounded > MaxUnscaled | nonRounded < MinUnscaled) {
				precisionDelta += 1
				divisor = Powers(precisionDelta)
				nonRounded /= 10L
				maxPrecision = Precision
			}
			if (precision < maxPrecision & precision != ExtendedPrecision) {
				precisionDelta = div10Precision - precision
				divisor = Powers(precisionDelta)
				nonRounded = div10 / divisor
			}
			val rem = div10 - nonRounded * divisor
			if (rem == 0L && totalDiv10rem == 0L)
				Decimal64(nonRounded, -exponent - 1 - precisionDelta)
			else if (precision == ExtendedPrecision)
				//we could just use roundToMaxPrecision(div10, 10, totalDiv10rem, rounding), but this saves a couple of iterations
				divideToMaxPrecision(div10, divisor, totalDiv10rem, exponent + precisionDelta + 1, rounding)
			else if (precision <= maxPrecision)
				roundAndScale(nonRounded, divisor, rem, totalDiv10rem, -exponent - precisionDelta - 1, rounding)
			else if (precision > div10Precision)
				//rem != 0 || totalDiv10rem != 0; note that totalDiv10rem is the first digit following div10
				throw PrecisionExceededException
			else { //precision > maxPrecision; overflow, unless the truncated digits are zeros
				precisionDelta = div10Precision - precision
				divideAndScale(div10, Powers(precisionDelta), totalDiv10rem, exponent + 1 + precisionDelta, rounding)
			}
		}
	}


	/** Calculates the sum of two decimal numbers with exponent difference under `LongPrecision`.
	  * The summands' significands may partially overlap when aligned to the same power of `10`, but do not fit
	  * in a single `Long` precision. This method splits the combined range on which either of the summands have
	  * non zero digits into a pair of `Long` values, with the lower one holding the lower `LongPrecision` digits
	  * of the result. Subsequently, it sums them separately with a standard `Long` addition, performs any carry over,
	  * and takes the highest `precision` digits of the result.
	  * @return a `Decimal64` equal to `xSignificand * 10^xExponent + ySignificand * 10^yExponent` after rounding.
	  **/
	private def sum128(xSignificand :Long, xExponent :Int, ySignificand :Long, yExponent :Int,
	                   rounding :RoundingMode, precision :Int) :Decimal64 =
	{
		val Powers = LongPowersOf10
		val exponentDiff = xExponent - yExponent
		val multiplier = Powers(exponentDiff)
		val divisor = Powers(LongPrecision - exponentDiff)
		var hi = xSignificand / divisor
		var lo = ySignificand + (xSignificand - hi * divisor) * multiplier
		if (hi == 0L)
			Decimal64(lo, -yExponent, rounding, precision)
		else if (lo == 0L)
			Decimal64(hi, -yExponent - LongPrecision, rounding, precision)
		else {
			val carry = lo / MaxLongPowerOf10
			lo -= carry * MaxLongPowerOf10
			hi += carry
			if (hi <= 0 != lo <= 0) {
				val sign = hi.sign
				hi -= sign
				lo += sign * MaxLongPowerOf10
			}
			roundDoubleLong(hi, lo, -yExponent, rounding, precision)
		}
	}

	/** Calculates the sum of two positive numbers having non-zero digits on exclusive decimal positions.
	  * The result's signum is given as `sign`.
	  * More precisely, `xExponent` must be greater than or equal to `precision(ySignificand) + yExponent`.
	  * Additionally, both `(xSignificand, xExponent)` and `(ySignificand, yExponent)` must form valid,
	  * normalized/denormalized `Decimal64` values.
	  * @return `sign * (xSignificand * 10^xExponent + ySignificand * 10^yExponent` after rounding to `precision`
	  *         digits, according to mode `rounding`.
	  **/
	private def sumDifferentMagnitudes(sign :Int, xSignificand :Long, xExponent :Int, xPrecision :Int,
	                                   ySignificand :Long, yExponent :Int, yPrecision :Int,
	                                   rounding :RoundingMode, precision :Int) :Decimal64 =
	{
		val Powers = LongPowersOf10
		//number of zero digits between the two significands when scaled
		val gap = xExponent - (yPrecision + yExponent)
		//precision of xSignificand with all trailing zeros preceding y
		val paddedXPrecision = xPrecision + gap
		val yMagnitude = Powers(yPrecision)
		if (precision == ExtendedPrecision) {                   //round as much as is necessary
			if (MaxPrecision < paddedXPrecision) {              //rounding a zero digit between the significands
				//divisor and rem are chosen arbitrarily so that 0 < rem < divisor/2
				// we round at ${xSignificand}0*" | "0+${ySignificand}", so we know the above holds the real values
				val delta = MaxPrecision - xPrecision
				roundToMaxPrecision(sign * xSignificand * Powers(delta), 10L, 1L, delta - xExponent, rounding)
			} else if (MaxPrecision >= xPrecision + xExponent - yExponent) {
				val power = Powers(xExponent - yExponent)
				val result = xSignificand * power + ySignificand
				roundToMaxPrecision(sign * result, 1L, 0L, -yExponent, rounding)
			} else {                                            //rounding within ySignificand
				val xMultiplier = Powers(MaxPrecision - xPrecision)
				val droppedYDigits = paddedXPrecision + yPrecision - MaxPrecision
				val yDivisor = Powers(droppedYDigits)
				val yScaled = ySignificand / yDivisor
				val rem = ySignificand - yScaled * yDivisor
				val result = xSignificand * xMultiplier + yScaled
				roundToMaxPrecision(sign * result, yDivisor, rem, -(yExponent + droppedYDigits), rounding)
			}
		} else if (precision < xPrecision) {                    //only the digits in xSignificand matter
			val delta = xPrecision - precision
			divideAndScale(sign * xSignificand, Powers(delta), ySignificand, xExponent + delta, rounding)
		} else if (precision == xPrecision) {
			if (gap == 0)
				roundAndScale(sign * xSignificand, yMagnitude, ySignificand, -xExponent, rounding)
			else
				roundAndScale(sign * xSignificand, 1L, 0L, ySignificand, -xExponent, rounding)
		} else if (precision <= paddedXPrecision) {             //ySignificand is used only for rounding
			val roundUp =
				if (precision < paddedXPrecision)               //rounding on a zero digit between xSignificand and y
					rounding match {
						case UP          => true
						case FLOOR       => sign <= 0
						case CEILING     => sign >= 0
						case UNNECESSARY => throw RoundingNecessaryException
						case _           => false
					}
				else                                            //rounding on the first digit of ySignificand
					rounding match {
						case DOWN => false
						case UP => true
						case FLOOR => sign <= 0
						case CEILING => sign >= 0
						case UNNECESSARY => throw RoundingNecessaryException
						case _ if ySignificand < (yMagnitude >> 1) => false
						case _ if ySignificand > (yMagnitude >> 1) => true
						case HALF_DOWN => false
						case HALF_UP => true
						case HALF_EVEN => gap == 0 && (xSignificand & 1L) != 0L
					}
			if (roundUp) {
				if (precision > MaxPrecision)
					throw PrecisionExceededException
				val delta = precision - xPrecision
				scaleDown(sign * (xSignificand * Powers(delta) + 1), delta - xExponent)
			} else
				make(sign * xSignificand, -xExponent)
		} else if (precision <= MaxPrecision) {                 //take xSignificand and some of the higher digits of y
			var droppedYDigits = paddedXPrecision + yPrecision - precision
			if (droppedYDigits < 0)
				droppedYDigits = 0
			val lastYDigitMagnitude = Powers(droppedYDigits)
			val lo = ySignificand / lastYDigitMagnitude
			val rem = ySignificand - lo * lastYDigitMagnitude
			val nonRounded = xSignificand * Powers(precision - xPrecision) + lo
			roundAndScale(sign * nonRounded, lastYDigitMagnitude, rem, -(yExponent + droppedYDigits), rounding)
		} else if (precision >= paddedXPrecision + yPrecision) { //round on a zero below ySignificand
			var droppedYDigits = paddedXPrecision + yPrecision - MaxPrecision
			if (droppedYDigits < 0)
				droppedYDigits = 0
			val lastYDigitMagnitude = Powers(droppedYDigits)
			val lo = ySignificand / lastYDigitMagnitude
			val rem = ySignificand - lo * lastYDigitMagnitude
			if (rem != 0L)                                       //almost certain, unless y is denormalized
				throw PrecisionExceededException
			val result = xSignificand * Powers(MaxPrecision - xPrecision) + lo
			Decimal64(sign * result, -(yExponent + droppedYDigits))
		} else {                                                //round within ySignificand
			val insignificantYDigits = paddedXPrecision + yPrecision - precision
			val significantLo = divideLong(sign * ySignificand, Powers(insignificantYDigits), rounding)
			val droppedLoDigits = precision - MaxPrecision
			val lastLoDigitMagnitude = Powers(droppedLoDigits)
			val lo = significantLo / lastLoDigitMagnitude
			val rem = significantLo - lo * lastLoDigitMagnitude
			if (rem != 0L)                                      //almost certain, unless y is denormalized
				throw PrecisionExceededException
			val result = sign * xSignificand * Powers(MaxPrecision - xPrecision) + lo
			Decimal64(result, -(yExponent + insignificantYDigits + droppedLoDigits))
		}
	}



	/** Calculates the difference of two positive numbers having non-zero digits on exclusive decimal positions.
	  * The result's signum is given as `sign`.
	  * More precisely, `xExponent` must be greater than or equal to `precision(ySignificand) + yExponent`.
	  * Additionally, both `(xSignificand, xExponent)` and `(ySignificand, yExponent)` must form valid,
	  * normalized/denormalized `Decimal64` values.
	  * @return `sign * (xSignificand * 10^xExponent - ySignificand * 10^yExponent` after rounding to `precision`
	  *         digits, according to mode `rounding`.
	  **/
	private def subtractDifferentMagnitudes(sign :Int, xSignificand :Long, xExponent :Int, xPrecision :Int,
	                                        ySignificand :Long, yExponent :Int, yPrecision :Int,
	                                        rounding :RoundingMode, precision :Int) :Decimal64 =
	{
		val Powers = LongPowersOf10
		//exchange 1 * 10^xExponent for U(9 * 10^i | i = yExponent + yPrecision + 1..xExponent-1) + 10^(yExponent + yPrecision)
		val x = xSignificand - 1
		val xDecreasedPrecision =
			if (xSignificand == Powers(xPrecision - 1)) xPrecision - 1
			else xPrecision
		//number of digits between the two significands when scaled - all are 9
		val gap = xExponent - (yPrecision + yExponent)
		//precision of x with all trailing nines preceding y
		val paddedXPrecision = xDecreasedPrecision + gap
		val yMagnitude = Powers(yPrecision)
		val y = yMagnitude - ySignificand
		//the exact result, as a string, equals x.toString + ("9" * gap) + y + ("0" * yExponent)
		if (precision == ExtendedPrecision) {
			if (MaxPrecision < paddedXPrecision) {
				val delta = MaxPrecision - xPrecision
				val result = xSignificand * Powers(delta) - 1L
				roundToMaxPrecision(sign * result, 10L, 9L, -(xExponent - delta), rounding)
			} else if (MaxPrecision >= xPrecision + xExponent - yExponent) {
				val result = (xSignificand * Powers(gap) - 1L) * yMagnitude + y
				roundToMaxPrecision(sign * result, 1L, 0L, -yExponent, rounding)
			} else {
				val droppedYDigits = paddedXPrecision + yPrecision - MaxPrecision
				val yDivisor = Powers(droppedYDigits)
				val yScaled = y / yDivisor
				val rem = y - yScaled * yDivisor
				val result = (xSignificand * Powers(gap) - 1L) * Powers(MaxPrecision - paddedXPrecision) + yScaled
				roundToMaxPrecision(sign * result, yDivisor, rem, -(yExponent + droppedYDigits), rounding)
			}
		} else if (precision < xDecreasedPrecision) {
			val delta = xDecreasedPrecision - precision
			val trailing = if (gap == 0) y else 9                                     //influence rounding on even
			divideAndScale(sign * x, Powers(delta), trailing, xExponent + delta, rounding)
		} else if (precision == xDecreasedPrecision) {
			if (gap == 0)
				roundAndScale(sign * x, yMagnitude, y, -xExponent, rounding)
			else
				roundAndScale(sign * x, 10L, 9L, y, -xExponent, rounding)
		} else if (precision <= paddedXPrecision) {                                   //include only x and gap digits
			if (rounding == UNNECESSARY)
				throw RoundingNecessaryException
			val RoundingBoundary = yMagnitude / 2
			val roundDown =
				rounding == DOWN || rounding == (if (sign >= 0) FLOOR else CEILING) ||
					(precision == paddedXPrecision && (y match {
						case RoundingBoundary =>
							rounding == HALF_DOWN || rounding == HALF_EVEN && gap == 0 && (x & 1L) == 0L
						case n if n < RoundingBoundary => isNearestNeighbour(rounding)
						case _ => false
					}))
			if (roundDown) {                                                          //shift x and add '9' digits
				if (precision > MaxPrecision)
					throw PrecisionExceededException
				val powerDelta = precision - xDecreasedPrecision
				val xMultiplier = Powers(powerDelta)
				val result = x * xMultiplier + (xMultiplier - 1L)
				Decimal64(sign * result, powerDelta - xExponent)
			} else                                                                    //round up back to xSignificand
				Decimal64(sign * xSignificand, -xExponent)
		} else if (precision <= MaxPrecision) {                                       //round within y
			var droppedYDigits = paddedXPrecision + yPrecision - precision
			if (droppedYDigits < 0)
				droppedYDigits = 0
			val lastYDigitMagnitude = Powers(droppedYDigits)
			val lo = y / lastYDigitMagnitude
			val rem = y - lo * lastYDigitMagnitude
			val nonRounded = (xSignificand * Powers(gap) - 1L) * Powers(precision - paddedXPrecision) + lo
			roundAndScale(sign * nonRounded, lastYDigitMagnitude, rem, -(yExponent + droppedYDigits), rounding)
		} else if (precision >= paddedXPrecision + yPrecision) {                      //include whole x and y
			var droppedYDigits = paddedXPrecision + yPrecision - MaxPrecision
			if (droppedYDigits < 0)
				droppedYDigits = 0
			val lastYDigitMagnitude = Powers(droppedYDigits)
			val lo = y / lastYDigitMagnitude
			val rem = y - lo * lastYDigitMagnitude
			if (rem != 0L)
				throw PrecisionExceededException
			val result = (xSignificand * Powers(gap) - 1L) * Powers(MaxPrecision - paddedXPrecision) + lo
			Decimal64(sign * result, -(yExponent + droppedYDigits))
		} else {
			val insignificantYDigits = paddedXPrecision + yPrecision - precision
			val significantLo = sign * divideLong(sign * y, Powers(insignificantYDigits), rounding)
			val truncLoDigits = precision - MaxPrecision                              //truncLoDigits <= yPrecision + gap
			val excludedLoDigits = truncLoDigits + insignificantYDigits
			val lastLoDigitMagnitude = Powers(truncLoDigits)
			val lo = significantLo / lastLoDigitMagnitude
			val rem = significantLo - lo * lastLoDigitMagnitude
			if (rem != 0L | truncLoDigits > yPrecision)                             //almost certain, unless y is denormalized
				throw PrecisionExceededException
			val result = (xSignificand * Powers(gap) - 1L) * Powers(yPrecision - excludedLoDigits) + lo
			Decimal64(sign * result, -(yExponent + excludedLoDigits))
		}
	}
}
