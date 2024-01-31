package net.noresttherein.sugar.numeric

import java.math.{BigInteger, MathContext}
import java.math.BigInteger.{ONE, ZERO}

import scala.annotation.tailrec

import net.noresttherein.sugar.exceptions.{SugaredArithmeticException, SugaredNumberFormatException}
import net.noresttherein.sugar.numeric.BigRatio.GCD
import net.noresttherein.sugar.numeric.Decimal64.Round.ExtendedExact




/** Simple implementation of rational numbers (fractions of integer values), represented by a pair of `BigInt` values.
  * The fraction is always in its canonical form: the greatest common divisor of the numerator
  * and the denominator is `1` and the denominator is always a positive integer.
  * For the public constructor,
  * see [[net.noresttherein.sugar.numeric.BigRatio.apply(numerator:Int, denominator:Int)* BigRatio()]].
  * There is also an extension method [[net.noresttherein.sugar.numeric.BigRatio.bigRatio_%/ bigRatio_%/]]
  * (available also as `sugar.`[[net.noresttherein.sugar.numeric.extensions.BigIntExtension(self:BigInt) method_%/]])
  * for `BigInt` values, accepting another `BigInt` to use as a denominator.
  * @author Marcin Mościcki marcin@moscicki.net
  */
@SerialVersionUID(Ver)
final class BigRatio private (n :BigInteger, d :BigInteger,
                              private[this] var num :BigInt, private[this] var den :BigInt) extends Number
{
	private def this(n :BigInt, d :BigInt) = this(n.bigInteger, d.bigInteger, n, d)
	private def this(n :BigInteger, d :BigInteger) = this(n, d, null, null)

	@inline private def newBigRatio(numerator :BigInteger, denominator :BigInteger) :BigRatio =
		new BigRatio(numerator, denominator, if (numerator eq n) num else null, if (denominator eq d) den else null)

	private def javaNumerator = n
	private def javaDenominator = d

	/** The numerator of this fraction (the dividend). */
	def numerator :BigInt = { //we don't care that res is not volatile, because BigInt is an immutable class
		var res = num
		if (res == null) {
			res = BigInt(n)
			num = res
		}
		res
	}

	/** The denominator of this fraction (the divisor). The denominator is always a positive number. */
	def denominator :BigInt = {
		var res = den
		if (res == null) {
			res = BigInt(d)
			den = res
		}
		res
	}

	/** The sign of this rational number as a `Int`.
	  * @return `1` if this rational is positive, `-1` if it is negative, and `0` if it is zero.
	  */
	@inline def sign :Int = n.signum

	/** The absolute value of this rational number: `numerator.abs %/ denominator`. */
	def abs :BigRatio =
		if ((n compareTo ZERO) >= 0) this
		else newBigRatio(n.negate, d)

	/** `-this.abs`. This method exists only for consistency with
	  *  [[net.noresttherein.sugar.numeric.Ratio Ratio]]`.`[[net.noresttherein.sugar.numeric.Ratio.minusAbs minusAbs]]:
	  *  `BigInt` does not overflow and there is no particular need for using `minusAbs` instead of
	  *  [[net.noresttherein.sugar.numeric.BigRatio.abs abs]].
	  */
	def minusAbs :BigRatio =
		if ((n compareTo ZERO) <= 0) this
		else newBigRatio(n.negate, d)

	/** The opposite rational number: `-numerator %/ denominator`. */
	def unary_- :BigRatio = newBigRatio(n.negate, d)

	/** The fraction resulting from swapping the places of numerator and denominator. For all rational numbers
	  * `r <: BigRatio` `r * r.reciprocal == BigRatio(1)`
	  * (reciprocal is the opposite element with regard to multiplication).
	  * @return `denominator * this.sign %/ numerator * this.sign`
	  */
	def reciprocal :BigRatio = n compareTo d match {
		case 0 => throw SugaredArithmeticException("Division by zero: (0/1).reciprocal")
		case 1 => newBigRatio(d, n)
		case _ => newBigRatio(d.negate, n.negate)
	}

	/** Does this ratio represent an integral number, that is its numerator is a multiple of its denominator?. */
	def isWhole :Boolean = n.remainder(d) == ZERO

	/** Is this a proper fraction, meaning `numerator.abs < denominator`?. */
	@inline def isProper :Boolean =
		if (n.compareTo(ZERO) >= 0) n.compareTo(d) < 0
		else n.negate.compareTo(d) < 0

	/** The whole part of this fraction, i.e. `numerator / denominator` (integral division). */
	@inline def whole :BigInt = BigInt(n.divide(d))

	/** The fractional part of this fraction, i.e. `BigRatio(numerator % denominator, denominator)`. */
	@inline def fraction :BigRatio = newBigRatio(n remainder d, d)



	private def plus(num :BigInteger, den :BigInteger) :BigRatio = {
		val bigNum = (num multiply d) add (n multiply den)
		val bigDen = d multiply den
		val gcd = GCD(bigNum, bigDen)
		val resNum = bigNum divide gcd; val resDen = bigDen divide gcd
		newBigRatio(resNum, resDen)
	}

	def +(other :BigRatio) :BigRatio = plus(other.javaNumerator, other.javaDenominator)
	def -(other :BigRatio) :BigRatio = plus(other.javaNumerator.negate, other.javaDenominator)

	/** Multiplies this rational number by another rational. This algorithm promotes the values to the `Long` type
	  * for the calculation process to avoid temporary arithmetic overflow resulting from `Int` multiplication. The
	  * overflow in the result can still occur however if the numerator and denominator after reduction by their ''GCD''
	  * doesn't fit in the `Int` type.
	  */
	def *(other :BigRatio) :BigRatio = {
		val num = n multiply other.javaNumerator
		val den = d multiply other.javaDenominator
		val gcd = GCD(num, den)
		val reducedNum = num divide gcd
		val reducedDen = den divide gcd
		new BigRatio(reducedNum, reducedDen)
	}

	/** Divides this rational number by another rational. */
	def /(other :BigRatio) :BigRatio =
		BigRatio(n multiply other.javaDenominator, d multiply other.javaNumerator)


	def <(other :BigRatio) :Boolean = {
		val n2 = other.javaNumerator; val d2 = other.javaDenominator
		if (n.compareTo(ZERO) < 0)
			n2.compareTo(ZERO) >= 0 || (n.multiply(d2) compareTo n2.multiply(d)) < 0
		else
			n2.compareTo(ZERO) > 0 && (n.multiply(d2) compareTo n2.multiply(d)) < 0
	}

	def <=(other :BigRatio) :Boolean = {
		val n2 = other.javaNumerator; val d2 = other.javaDenominator
		if (n.compareTo(ZERO) <= 0)
			n2.compareTo(ZERO) >= 0 || (n.multiply(d2) compareTo n2.multiply(d)) <= 0
		else
			n2.compareTo(ZERO) > 0 && (n.multiply(d2) compareTo n2.multiply(d)) <= 0
	}

	@inline def >(other :BigRatio) :Boolean = !(this <= other)
	@inline def >=(other :BigRatio) :Boolean = !(this < other)

	@inline def min(other :BigRatio) :BigRatio = if (this <= other) this else other
	@inline def max(other :BigRatio) :BigRatio = if (this < other) other else this


	def toInt            :Int        = n.divide(d).intValue
	def toFloat          :Float      = n.floatValue / d.floatValue
	def toDouble         :Double     = n.doubleValue / d.doubleValue
	def toShort          :Short      = (n divide d).shortValue
	def toLong           :Long       = (n divide d).longValue
	def toIntRatio       :IntRatio   = IntRatio(n.intValue, d.intValue)
	def toRatio          :Ratio      = Ratio(n.longValue, d.longValue)
	def toBigInt         :BigInt     = BigInt(n divide d)
	def toBigInteger     :BigInteger = n divide d
	def toDecimal64      :Decimal64  = Decimal64(n) / Decimal64(d)
	def toDecimal64Exact :Decimal64  = Decimal64(n).div(Decimal64(d), ExtendedExact)

	@inline def toBigDecimal(implicit mc :MathContext = BigDecimal.defaultMathContext) :BigDecimal =
		BigDecimal(n, mc) / BigDecimal(d)

	@inline def toBigDecimalExact :BigDecimal = toBigDecimal(MathContext.UNLIMITED)

	@inline def toJavaBigDecimal(implicit mc :MathContext = MathContext.UNLIMITED) :java.math.BigDecimal =
		new java.math.BigDecimal(n).divide(new java.math.BigDecimal(d), mc)


	@inline override def intValue    :Int    = toInt
	@inline override def floatValue  :Float  = toFloat
	@inline override def doubleValue :Double = toDouble
	@inline override def shortValue  :Short  = toShort
	@inline override def longValue   :Long   = toLong


	override def equals(that :Any) :Boolean = that match {
		case r :BigRatio => n == r.javaNumerator & d == r.javaDenominator
		case _ => false
	}

	override def hashCode :Int = d.hashCode * 31 + n.hashCode

	override def toString :String = n.toString + "/" + d
}






/** Companion object for the [[net.noresttherein.sugar.numeric.BigRatio! BigRatio]] class representing rational numbers
  * as fractions in their canonical form. Contains implicit conversions promoting `Int`s, `Long`s, `BigInt`s
  * and `BigInteger`s to `BigRatio`s when given as the parameter to `BigRatio`'s methods.
  */
@SerialVersionUID(Ver)
object BigRatio {//extends BigRatioImplicits {

	/** Number zero as a `BigRatio` value: `0/1`. */
	final val Zero :BigRatio = new BigRatio(0, 1)

	/** Number one as a `BigRatio` value: `1/1`. */
	final val One :BigRatio = new BigRatio(1, 1)

//	private final val MAX_INT = BigInteger.valueOf(Int.MaxValue)
	private final val MINUS_ONE = ONE.negate
	private final val HUNDRED = BigInteger.valueOf(100)
	private final val MINUS_HUNDRED = BigInteger.valueOf(-100)

	private final val BigZero     = BigInt(0)
	private final val BigOne      = BigInt(1)
	private final val BigMinusOne = BigInt(-1)

	private final val CachedIntegers = 100
	private final val CACHED_INTEGERS = HUNDRED
	private final val MINUS_CACHED_INTEGERS = CACHED_INTEGERS.negate
	private val Integers =
		(-CachedIntegers to -1).map(new BigRatio(_, 1)) ++
			(0 to CachedIntegers).map(new BigRatio(_, 1)) to Array

	private final val CachedUnits = 100
	private final val CACHED_UNITS = HUNDRED
	private final val MINUS_CACHED_UNITS = CACHED_UNITS.negate
	private val Units =
		(CachedUnits to 1 by -1).map(new BigRatio(-1, _)) ++
			(1 to CachedUnits).map(new BigRatio(1, _)) to Array
	private val Percents =
		(-100 until 0).map(reduce(_, 100)) ++ (0 to 100).map(reduce(_, 100)) to Array


	private def reduce(numerator :BigInteger, denominator :BigInteger) :BigRatio = {
		val gcd = GCD(numerator, denominator)
		if (gcd == BigOne) new BigRatio(numerator, denominator)
		else new BigRatio(numerator divide gcd, denominator divide gcd)
	}
	private def reduce(numerator :BigInt, denominator :BigInt) :BigRatio = {
		val gcd = GCD(numerator.bigInteger, denominator.bigInteger)
		if  (gcd == ONE) new BigRatio(numerator, denominator)
		else new BigRatio(numerator.bigInteger divide gcd, denominator.bigInteger divide gcd)
	}

	private[numeric] def newBigRatio(numerator :BigInteger, denominator :BigInteger) :BigRatio =
		denominator match {
			case ONE => BigRatio(numerator)
			case HUNDRED => percent(numerator)
			case MINUS_HUNDRED => percent(numerator.negate)
			case ZERO => throw SugaredArithmeticException("BigRatio " + numerator + "/0")
			case _ if numerator equals ONE => unit(denominator)
			case _ => new BigRatio(numerator, denominator)
		}

	private[numeric] def newBigRatio(numerator :Long, denominator :Long) :BigRatio =
		denominator match {
			case 1L => BigRatio(numerator)
			case 100L if numerator.toInt == numerator => percent(numerator.toInt)
//			case -100L if numerator.toInt == numerator => -percent(-numerator.toInt)
			case 0L => throw SugaredArithmeticException("BigRatio " + numerator + "/0")
			case _ if numerator == 1L => unit(denominator)
			case _ => new BigRatio(BigInteger.valueOf(numerator), BigInteger.valueOf(denominator))
		}



	/** Creates a unit fraction, that is one with `1` as the numerator and the given denominator.
	  * Passing a negative value will result in a negative `BigRatio` with `-1` as the numerator
	  * and `denominator.abs` as the denominator.
	  * @param denominator requested denominator of the fraction.
	  * @return a `BigRatio` equal to `1 / denominator`.
	  * @throws ArithmeticException if denominator equals zero.
	  */
	def unit(denominator :BigInteger) :BigRatio =
		if (denominator.compareTo(ZERO) > 0)
			if (denominator.compareTo(CACHED_UNITS) <= 0)
				Units(CachedUnits + denominator.intValue - 1)
			else
				new BigRatio(ONE, denominator)
		else if (denominator.compareTo(ZERO) < 0)
			if (denominator.compareTo(MINUS_CACHED_UNITS) >= 0)
				Units(CachedUnits + denominator.intValue)
			else
				new BigRatio(MINUS_ONE, denominator.negate)
		else
			throw SugaredArithmeticException("BigRatio 1/0")

	/** Creates a unit fraction, that is one with `1` as the numerator and the given denominator.
	  * Passing a negative value will result in a negative `BigRatio` with `-1` as the numerator
	  * and `denominator.abs` as the denominator.
	  * @param denominator requested denominator of the fraction.
	  * @return a `BigRatio` equal to `1 / denominator`.
	  * @throws ArithmeticException if denominator equals zero.
	  */
	def unit(denominator :BigInt) :BigRatio =
		if (denominator > BigZero)
			if (denominator.isValidInt && denominator.intValue <= CachedUnits)
				Units(CachedUnits + denominator.intValue - 1)
			else
				new BigRatio(BigOne, denominator)
		else if (denominator < BigZero)
			if (denominator.isValidInt && denominator.intValue >= -CachedUnits)
				Units(CachedUnits + denominator.intValue)
			else
				new BigRatio(BigMinusOne, -denominator)
		else
			throw SugaredArithmeticException("BigRatio 1/0")


	/** A reduced fraction `n / 100`.*/
	def percent(n :Int) :BigRatio =
		if (-100 <= n && n <= 100)
			Percents(100 + n)
		else
			reduce(BigInteger.valueOf(n), HUNDRED)

	/** A reduced fraction `n / 100`.*/
	def percent(n :BigInteger) :BigRatio =
		if (MINUS_HUNDRED.compareTo(n) <= 0 && n.compareTo(HUNDRED) <= 0)
			Percents(100 + n.intValue)
		else
			reduce(n, HUNDRED)



	/** Creates a whole rational (a fraction with `1` as the denominator and the argument as the numerator).
	  * @return an `BigRatio` of `integer/1`.
	  */
	def apply(integer :Int) :BigRatio = apply(integer.toLong)

	/** Creates a whole rational (a fraction with `1` as the denominator and the argument as the numerator).
	  * @return an `BigRatio` of `integer/1`.
	  */
	def apply(integer :Long) :BigRatio =
		if (-CachedIntegers <= integer & integer <= CachedIntegers)
			Integers(CachedIntegers + integer.toInt)
		else
			new BigRatio(BigInteger.valueOf(integer), ONE, null, BigOne)

	/** Creates a whole rational (a fraction with `1` as the denominator and the argument as the numerator).
	  * @return an `BigRatio` of `integer/1`.
	  */
	def apply(integer :BigInteger) :BigRatio =
		if (integer.compareTo(MINUS_CACHED_INTEGERS) >= 0 && integer.compareTo(CACHED_INTEGERS) <= 0)
			Integers(CachedIntegers + integer.intValue)
		else
			new BigRatio(integer, ONE, null, BigOne)

	/** Creates a whole rational (a fraction with `1` as the denominator and the argument as the numerator).
	  * @return an `BigRatio` of `integer/1`.
	  */
	def apply(integer :BigInt) :BigRatio =
		if (integer.isValidInt && -CachedIntegers <= integer.toInt && integer.toInt <= CachedIntegers)
			Integers(CachedIntegers + integer.toInt)
		else
			new BigRatio(integer, BigOne)


	/** Creates a rational number in the canonical form.
	  * @param numerator the dividend (the numerator of the fraction)
	  * @param denominator the divisor (the denominator of the fraction)
	  * @throws ArithmeticException if `denominator` is zero.
	  * @return an `BigRatio` representing the fraction `numerator / denominator` after dividing them by their greatest
	  *         common divisor and adjusting the signums so that denominator is positive.
	  */
	def apply(numerator :Int, denominator :Int) :BigRatio = apply(numerator.toLong, denominator.toLong)

	/** Creates a rational number in the canonical form.
	  * @param numerator the dividend (the numerator of the fraction).
	  * @param denominator the divisor (the denominator of the fraction).
	  * @throws ArithmeticException if `denominator` is zero.
	  * @return an `BigRatio` representing the fraction `numerator / denominator` after dividing them by their greatest
	  *         common divisor and adjusting the signums so that denominator is positive.
	  */
	def apply(numerator :Long, denominator :Long) :BigRatio =
		if (denominator == Long.MinValue || numerator == Long.MinValue) //GCD overflows as it computes based on absolute values
			BigRatio(BigInteger.valueOf(numerator), BigInteger.valueOf(denominator))
		else if (denominator == 100L && numerator.toInt.toLong == numerator)
			percent(numerator.toInt)
		else if (denominator > 0) {
			val divisor = Ratio.GCD(numerator, denominator)
			newBigRatio(numerator / divisor, denominator / divisor)
		} else if (denominator < 0) {
			val divisor = Ratio.GCD(numerator, denominator)
			newBigRatio(-(numerator / divisor), -(denominator / divisor))
		} else
			throw SugaredArithmeticException("BigRatio: division by zero.")

	/** Creates a rational number in the canonical form.
	  * @param numerator the dividend (the numerator of the fraction).
	  * @param denominator the divisor (the denominator of the fraction).
	  * @throws ArithmeticException if `denominator` is zero.
	  * @return an `BigRatio` representing the fraction `numerator / denominator` after dividing them by their greatest
	  *         common divisor and adjusting the signums so that denominator is positive.
	  */
	def apply(numerator :BigInteger, denominator :BigInteger) :BigRatio =
		if (denominator == HUNDRED && numerator.compareTo(MINUS_HUNDRED) >= 0 && numerator.compareTo(HUNDRED) >= 0)
			Percents(numerator.intValue)
		else if (denominator == MINUS_HUNDRED && numerator.compareTo(MINUS_HUNDRED) >= 0 && numerator.compareTo(HUNDRED) >= 0)
			Percents(-numerator.intValue)
		else if (denominator.compareTo(ZERO) > 0)
			reduce(numerator, denominator)
		else if (denominator.compareTo(ZERO) < 0)
			reduce(numerator.negate, denominator.negate)
		else
			throw SugaredArithmeticException("BigRatio: division by zero.")

	/** Creates a rational number in the canonical form.
	  * @param numerator the dividend (the numerator of the fraction).
	  * @param denominator the divisor (the denominator of the fraction).
	  * @throws ArithmeticException if `denominator` is zero.
	  * @return an `BigRatio` representing the fraction `numerator / denominator` after dividing them by their greatest
	  *         common divisor and adjusting the signums so that denominator is positive.
	  */
	def apply(numerator :BigInt, denominator :BigInt) :BigRatio =
		if (denominator.isValidInt && denominator.intValue == 100 && numerator.isValidInt)
			percent(numerator.toInt)
		else denominator compare BigZero match {
			case  1 => reduce(numerator, denominator)
			case -1 => reduce(numerator.bigInteger.negate, denominator.bigInteger.negate)
			case _  => throw SugaredArithmeticException("BigRatio: division by zero.")
		}

	/** Parses the string as a ratio of two `Int` numbers.
	  * @param string a rational number in one of the following formats:
	  *                 1. a base 10 `Int` number, acceptable by `string.toInt`,
	  *                 1. a decimal fraction using '.' character as the fractional part separator,
	  *                 1. ''numerator/denominator'', where both the numerator and denominator are valid `Int` numbers
	  *                    and denominator is positive.
	  * @return the number as a canonical fraction.
	  */
	def apply(string :String) :BigRatio =
		try {
			string.indexOf('/') match {
				case -1 =>
					val decimal = new java.math.BigDecimal(string).stripTrailingZeros
					if (decimal.scale <= 0)
						BigRatio(decimal.toBigInteger)
					else {
						val whole = decimal.toBigInteger
						val numerator = (decimal subtract new java.math.BigDecimal(whole)).unscaledValue
						val denominator = BigInteger.valueOf(decimal.scale)
						BigRatio(whole.multiply(denominator).add(numerator), denominator)
					}
				case n =>
					BigRatio(new BigInteger(string.substring(0, n)), new BigInteger(string.substring(n + 1)))
			}
		} catch {
			case e :NullPointerException => throw e
			case e :Exception =>
				throw SugaredNumberFormatException("\"" + string + "\" is not a valid BigRatio.").initCause(e)
		}


	/** Implicit promotion of an integer to a rational. Will be automatically performed without importing when an `Int`
	  * is passed to any method of a `BigRatio` object.
	  */
	@inline implicit def intToBigRatio(int :Int) :BigRatio = apply(int)

	/** Implicit promotion of a long integer to a rational. Will be automatically performed without importing
	  * when a `Long` is passed to any method of a `BigRatio` object.
	  */
	@inline implicit def longToBigRatio(int :Long) :BigRatio = apply(int)

	/** Implicit promotion of a `BigInteger` to a rational. Will be automatically performed without importing
	  * when a `BigInteger` is passed to any method of a `BigRatio` object.
	  */
	@inline implicit def bigIntegerToBigRatio(int :BigInteger) :BigRatio = apply(int)

	/** Implicit promotion of a `BigInt` to a rational. Will be automatically performed without importing
	  * when a `BigInt` is passed to any method of a `BigRatio` object.
	  */
	@inline implicit def bigIntToBigRatio(int :BigInt) :BigRatio = apply(int)



	/** The `Fractional` type class for `BigRatio` values. */
	@SerialVersionUID(Ver)
	implicit object BigRatioIsFractional extends Fractional[BigRatio] {
		override def div(x :BigRatio, y :BigRatio) :BigRatio = x / y
		override def plus(x :BigRatio, y :BigRatio) :BigRatio = x + y
		override def minus(x :BigRatio, y :BigRatio) :BigRatio = x - y
		override def times(x :BigRatio, y :BigRatio) :BigRatio = x * y
		override def negate(x :BigRatio) :BigRatio = -x
		override def fromInt(x :Int) :BigRatio = x
		override def toInt(x :BigRatio) :Int = x.toInt
		override def toLong(x :BigRatio) :Long = x.toLong
		override def toFloat(x :BigRatio) :Float = x.toFloat
		override def toDouble(x :BigRatio) :Double = x.toDouble

		override def compare(x :BigRatio, y :BigRatio) :Int =
			if (x < y) -1 else if (y < x) 1 else 0

		override def parseString(str :String) :Option[BigRatio] =
			try { Some(BigRatio(str)) } catch {
				case _ :NumberFormatException => None
			}
	}


	/** Introduces method [[net.noresttherein.sugar.numeric.BigRatio.BigIntNumerator.%/ %/]] extension method to `Int`,
	  * allowing to create a [[net.noresttherein.sugar.numeric.BigRatio BigRatio]] through a natural looking expression
	  * `numerator %/ denominator`.
	  */
	implicit def int_%/(numerator :Int) :BigIntNumerator = new BigIntNumerator(numerator)
	implicit def long_%/(numerator :Long) :BigIntNumerator = new BigIntNumerator(numerator)
	implicit def bigInteger_%/(numerator :BigInteger) :BigIntNumerator = new BigIntNumerator(numerator)
	implicit def bigInt_%/(numerator :BigInt) :BigIntNumerator = new BigIntNumerator(numerator)

	/** A builder of [[net.noresttherein.sugar.numeric.BigRatio BigRatio]] objects, accepting an `Int` denominator
	  * and constructing the rational number representing the division of the wrapped numerator values by the argument.
	  * @param numerator the numerator of created rational numbers (before reduction)
	  */
	class BigIntNumerator(private val numerator :BigInt) extends AnyVal {
		/** Creates a reduced `BigRatio` of `this/100`. */
		@inline def % :BigRatio = percent(numerator.bigInteger)

		/** Divides this `BigInt` by the argument, creating a [[net.noresttherein.sugar.numeric.BigRatio BigRatio]]
		  * number representing the result.
		  * @param denominator the denominator of the created rational (before reduction)
		  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
		  */
		@inline def %/(denominator :Int) :BigRatio = BigRatio(numerator, denominator)

		/** Divides this `BigInt` by the argument, creating a [[net.noresttherein.sugar.numeric.BigRatio BigRatio]]
		  * number representing the result.
		  * @param denominator the denominator of the created rational (before reduction)
		  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
		  */
		@inline def %/(denominator :Long) :BigRatio = BigRatio(numerator, denominator)

		/** Divides this `BigInt` by the argument, creating a [[net.noresttherein.sugar.numeric.BigRatio BigRatio]]
		  * number representing the result.
		  * @param denominator the denominator of the created rational (before reduction)
		  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
		  */
		@inline def %/(denominator :BigInteger) :BigRatio = BigRatio(numerator, denominator)

		/** Divides this `BigInt` by the argument, creating a [[net.noresttherein.sugar.numeric.BigRatio BigRatio]]
		  * number representing the result.
		  * @param denominator the denominator of the created rational (before reduction)
		  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
		  */
		@inline def %/(denominator :BigInt) :BigRatio = BigRatio(numerator, denominator)
	}



	/** Calculates the greatest common divisor of any two integer values.
	  * @return The greatest integer `d` such that `a / d` and `b / d` are integral numbers.
	  */
	@inline def GCD(a :BigInt, b :BigInt) :BigInt = BigInt(GCD(a.bigInteger, b.bigInteger))

	@inline def GCD(a :BigInteger, b :BigInteger) :BigInteger = naturalGCD(
		if (a.compareTo(ZERO) < 0) a.negate else a,
		if (b.compareTo(ZERO) < 0) b.negate else b
	)

	private def naturalGCD(a :BigInteger, b :BigInteger) :BigInteger = {
		@tailrec def rec(a :BigInteger, b :BigInteger) :BigInteger =
			if (b.compareTo(ZERO) == 0) a
			else rec(b, a remainder b)
		if ((a compareTo b) >= 0) rec(a, b)
		else rec(b, a)
	}

//	private def naturalGCD(a :BigInt, b :BigInt) :BigInt = BigInt(naturalGCD(a.bigInteger, b.bigInteger))


}
