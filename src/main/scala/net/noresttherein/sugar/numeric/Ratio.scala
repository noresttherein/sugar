package net.noresttherein.sugar.numeric

import java.math.{BigInteger, MathContext}

import net.noresttherein.sugar.numeric.Ratio.{naturalGCD, GCD}
import scala.annotation.tailrec

import net.noresttherein.sugar.numeric.Decimal64.Round.ExtendedExact




/** A simple implementation of rational numbers using `Long` values for the numerator and the denominator.
  * The fraction is always in its canonical form: the greatest common divisor of the numerator and the denominator
  * is `1` and the denominator is always a positive number. Unlike [[net.noresttherein.sugar.numeric.IntRatio IntRatio]],
  * this class does not check for arithmetic overflow/underflow. Operators are implemented in a way eliminating the
  * possibility of ''temporary'' overflows, but no exception is thrown if the result value overflows.
  * For the public constructor, see [[net.noresttherein.sugar.numeric.Ratio$.apply Ratio()]].
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
final class Ratio private[numeric](n :Long, d :Long) extends Number {

	/** The numerator of this fraction (the dividend). */
	@inline def numerator :Long = n

	/** The denominator of this fraction (the divisor). The denominator is always a positive number. */
	@inline def denominator :Long = d

	/** The sign of this rational number as a `Long`.
	  * @return `1` if this rational is positive, `-1` if it is negative, and `0` if it is zero.
	  */
	@inline def sign :Long = java.lang.Long.signum(n)

	/** The absolute value of this rational number: `numerator.abs %/ denominator`. */
	@inline def abs :Ratio =
		if (n >= 0) this else new Ratio(-n, d)

	/** Overflow safe `-this.abs`. Method `Ratio.`[[net.noresttherein.sugar.numeric.Ratio.abs abs]] can overflow
	  * if the numerator is equal to `Long.MaxValue`, as `-Long.MaxValue` equals itself and is not a positive number.
	  * It is thus always better to compare `minusAbs` values for any two numbers rather than `abs`, as negation
	  * of `Long.MaxValue` does not underflow.
	  */
	@inline def minusAbs :Ratio = if (n <= 0) this else new Ratio(-n, d)

	/** The opposite rational number: `-numerator %/ denominator`. */
	@inline def unary_- :Ratio = new Ratio(-n, d)

	/** The fraction resulting from swapping the places of numerator and denominator. For all rational numbers
	  * `r <: Ratio` `r * r.reciprocal == Ratio(1)` (reciprocal is the opposite element with regard to multiplication).
	  * @return `denominator * this.signum %/ numerator * this.signum`
	  */
	def reciprocal :Ratio =
		if (n == 0)
			throw new ArithmeticException("Division by zero: (0/1).reciprocal")
		else if (n > 0)
			new Ratio(d, n)
		else
			new Ratio(-d, -n)


	/** Does this ratio represent an integral number, that is its numerator is a multiple of its denominator?. */
	@inline def isWhole :Boolean = n % d == 0

	/** Is this a proper fraction, meaning `numerator.abs < denominator`?. */
	@inline def isProper :Boolean = (if (n < 0) -n else n) < denominator

	/** The whole part of this fraction, i.e. `numerator / denominator` (integral division). */
	@inline def whole :Long = n / d

	/** The fractional part of this fraction, i.e. `Ratio(numerator % denominator, denominator)`. */
	@inline def fraction :Ratio = new Ratio(n % d, d)



	private def plus(num :Long, den :Long) :Ratio = {
		val gcd = naturalGCD(d, den)
		val multiplier = d / gcd
		val lcm = multiplier * den
		val resNum = multiplier * num + den / gcd * n
		val resGCD = naturalGCD(if (resNum < 0) -resNum else resNum, lcm)
		new Ratio(resNum / resGCD, lcm / resGCD)
	}

	def +(other :Ratio) :Ratio = plus(other.numerator, other.denominator)

	@inline def -(other :Ratio) :Ratio = plus(-other.numerator, other.denominator)

	private def multiply(num :Long, den :Long) :Ratio = {
		if (den == 0)
			throw new ArithmeticException("Division by zero: " + this + " * " + num + "/" + den)
		val gcd_\ = GCD(n, den); val gcd_/ = GCD(d, num)
		val resNum = (n / gcd_\) * (num / gcd_/)
		val resDen = (d / gcd_/) * (den / gcd_\)
		if (resDen < 0)
			new Ratio(-resNum, -resDen)
		else
			new Ratio(resNum, resDen)
	}

	/** Multiplies this rational number by another rational. This implementation performs cross reduction before actual
	  * multiplication, but may still result in a quiet overflow.
	  */
	def *(other :Ratio) :Ratio = multiply(other.numerator, other.denominator)

	/** Divides this rational number by another rational. This implementation performs cross reduction before actual
	  * multiplication, but may still result in a quiet overflow.
	  */
	def /(other :Ratio) :Ratio = multiply(other.denominator, other.numerator)

	def <(other :Ratio) :Boolean = {
		val n2 = other.numerator; val d2 = other.denominator
		if (n < 0)
			n2 >= 0 || { val gcd = naturalGCD(d, d2); d2 / gcd * n < d / gcd * n2 }
		else
			n2 > 0 && { val gcd = naturalGCD(d, d2); d2 / gcd * n < d / gcd * n2 }
	}

	def <=(other :Ratio) :Boolean = {
		val n2 = other.numerator; val d2 = other.denominator
		if (n <= 0)
			n2 >= 0 || { val gcd = naturalGCD(d, d2); d2 / gcd * n <= d / gcd * n2 }
		else
			n2 > 0 && { val gcd = naturalGCD(d, d2); d2 / gcd * n <= d / gcd * n2 }
	}

	@inline def > (other :Ratio) :Boolean = !(this <= other)
	@inline def >=(other :Ratio) :Boolean = !(this < other)


	@inline def min(other :Ratio) :Ratio = if (this <= other) this else other
	@inline def max(other :Ratio) :Ratio = if (this < other) other else this


	@inline def toLong           :Long       = n / d
	@inline def toFloat          :Float      = n.toFloat / d.toFloat
	@inline def toDouble         :Double     = n.toDouble / d.toDouble
	@inline def toShort          :Short      = (n / d).toShort
	@inline def toInt            :Int        = (n / d).toInt
	@inline def toBigInt         :BigInt     = BigInt(toLong)
	@inline def toBigInteger     :BigInteger = BigInteger.valueOf(toLong)
	@inline def toDecimal64      :Decimal64  = Decimal64(n) / Decimal64(d)
	@inline def toDecimal64Exact :Decimal64  = Decimal64(n).div(Decimal64(d), ExtendedExact)

	@inline def toBigDecimal(implicit mc :MathContext = BigDecimal.defaultMathContext) :BigDecimal =
		BigDecimal(n, mc) / d

	@inline def toBigDecimalExact :BigDecimal = toBigDecimal(MathContext.UNLIMITED)

	@inline def toJavaBigDecimal(implicit mc :MathContext = MathContext.UNLIMITED) :java.math.BigDecimal =
		new java.math.BigDecimal(n).divide(new java.math.BigDecimal(d), mc)


	@inline def toIntRatio :IntRatio   = IntRatio(n.toInt, d.toInt)

	def toIntRatioExact    :IntRatio   =
		if (numerator < Int.MinValue | numerator > Int.MaxValue | denominator < Int.MinValue | denominator > Int.MaxValue)
			throw new ArithmeticException("Cannot represent " + this + " as IntRatio.")
		else
			IntRatio.newIntRatio(n.toInt, d.toInt)


	@inline override def intValue :Int = (n / d).toInt
	@inline override def floatValue :Float = n.toFloat / d.toFloat
	@inline override def doubleValue :Double = n.toDouble / d.toDouble
	@inline override def shortValue :Short = (n / d).toShort
	@inline override def longValue :Long = n / d



	override def equals(that :Any) :Boolean = that match {
		case r :Ratio => n == r.numerator & d == r.denominator
		case _ => false
	}

	override def hashCode :Int = java.lang.Long.hashCode(d) * 31 + java.lang.Long.hashCode(n)

	override def toString :String = n.toString + "/" + d
}






/** Companion object for the `Ratio` class representing rational numbers as fractions in their canonical form.
  * Contains an implicit conversion promoting `Long`s to `Ratio`s when given as the parameter to `Ratio`'s methods.
  */
@SerialVersionUID(Ver)
object Ratio {

	/** Number zero represented as a ratio of `0/1`. */
	final val Zero :Ratio = new Ratio(0, 1)

	/** Number one represented as a ration of `1/1`. */
	final val One :Ratio = new Ratio(1, 1)

	/** Maximal value held by this type: `Long.MaxValue / 1`. */
	final val MaxValue = new Ratio(Long.MaxValue, 1)

	/** Minimal value held by this type: `Long.MinValue / 1`. */
	final val MinValue = new Ratio(Long.MinValue, 1)

	/** Minimal positive value held by this type: `1 / Long.MaxValue`. As the ratio `1/Long.MinValue` can not be held
	  * by this type because of lack of `Long` representation of `-Long.MinValue` and the positive denominator requirement,
	  * it is also the smallest possible precision and difference between two `Ratio` values.
	  */
	final val Precision = new Ratio(1, Long.MaxValue)

	private final val CachedIntegers = 100
	private val Integers =
		(-CachedIntegers to -1).map(new Ratio(_, 1)) ++ (0 to CachedIntegers).map(new Ratio(_, 1)) to Array
	private final val CachedUnits = 100
	private val Units =
		(CachedUnits to 1 by -1).map(new Ratio(-1, _)) ++ (1 to CachedUnits).map(new Ratio(1, _)) to Array
	private val Percents =
		(-100 until 0).map(reduce(_, 100)) ++ (0 to 100).map(reduce(_, 100)) to Array

	private def reduce(numerator :Long, denominator :Long) :Ratio = {
		val gcd = GCD(numerator, denominator)
		new Ratio(numerator / gcd, denominator / gcd)
	}
	private def newRatio(numerator :Long, denominator :Long) :Ratio = denominator match {
		case 1L => Ratio(numerator)
		case 100L => percent(numerator)
		case 0L => throw new ArithmeticException("Ratio " + numerator + "/0")
		case _ if numerator == 1L => unit(denominator)
		case _ => new Ratio(numerator, denominator)
	}



	/** Creates a unit fraction, that is one with `1` as the numerator and the given denominator.
	  * Passing a negative value will result in a negative `Ratio` with `-1` as the numerator and `denominator.abs`
	  * as the denominator.
	  * @param denominator requested denominator of the fraction.
	  * @return a `Ratio` of `1 / denominator`.
	  * @throws ArithmeticException if `denominator` equals zero.
	  * @throws IllegalArgumentException if `denominator` equals `Long.MinValue`.
	  */
	def unit(denominator :Long) :Ratio =
		if (denominator > 0)
			if (denominator <= CachedUnits) Units((CachedUnits + denominator - 1L).toInt)
			else new Ratio(1, denominator)
		else if (denominator < 0)
			if (denominator >= -CachedUnits)
				Units((CachedUnits + denominator).toInt)
			else if (denominator == Long.MinValue)
				throw new IllegalArgumentException("Can't represent 1/Long.MinValue as a Ratio")
			else
                new Ratio(-1, -denominator)
		else
			throw new ArithmeticException("Ratio 1/0")

	/** A reduced fraction `n/100`. */
	def percent(n :Long) :Ratio =
		if (-100L <= n & n <= 100L) Percents((100 + n).toInt)
		else reduce(n, 100)

	/** Creates a whole rational (a fraction with `1` as the denominator and the argument as the numerator). */
	def apply(integer :Long) :Ratio =
		if (-CachedIntegers <= integer & integer <= CachedIntegers)
			Integers((CachedIntegers + integer).toInt)
		else new Ratio(integer, 1)


	/** Creates a rational number in the canonical form.
	  * @param numerator the dividend (the numerator of the fraction)
	  * @param denominator the divisor (the denominator of the fraction)
	  * @throws ArithmeticException if denominator is zero or one of the arguments is `Long.MinValue` and result would overflow.
	  * @return a `Ratio` representing the fraction `numerator / denominator` after dividing them by their greatest
	  *         common divisor and adjusting the signums so that denominator is positive.
	  */
	def apply(numerator :Long, denominator :Long) :Ratio =
		if (denominator == 100L)
			percent(numerator)
		else if (denominator > 0) {
			val divisor =
				if (numerator >= 0)
					naturalGCD(numerator, denominator)
				else if (numerator == Long.MinValue) {
					val d = naturalGCD(-numerator, denominator)
					if (d == 1L)
						throw new ArithmeticException(s"Can't represent $numerator/$denominator as a Ratio: no representation of -Long.MinValue")
					d
				} else
                    naturalGCD(-numerator, denominator)
			newRatio(numerator / divisor, denominator / divisor)
		} else if (denominator < 0) {
			if (denominator == Long.MinValue || numerator==Long.MinValue) {
				val divisor = naturalGCD(if (numerator >= 0) numerator.toLong else -numerator.toLong, -denominator.toLong)
				if (divisor == 1)
					throw new ArithmeticException(s"Can't represent $numerator/$denominator as a Ratio: no representation of -Long.MinValue")
				newRatio(numerator = -(numerator / divisor), denominator = -(denominator / divisor))
			} else {
				val divisor = naturalGCD(if (numerator > 0) numerator else -numerator, -denominator)
				newRatio(-(numerator / divisor), -(denominator / divisor))
			}
		} else
			  throw new ArithmeticException("Ratio: division by zero")


	/** Parses the string as a ratio of two `Long` numbers.
	  * @param string a rational number in one of the following formats:
	  *                 1. a base 10 `Long` number, acceptable by `string.toLong`,
	  *                 1. a decimal fraction using '.' character as the fractional part separator,
	  *                 1. ''numerator/denominator'', where both the numerator and denominator are valid `Long` numbers
	  *                    and denominator is positive.
	  * @return the number as a canonical fraction.
	  */
	@throws[NumberFormatException]("if the string is not a valid Ratio.")
	def apply(string :String) :Ratio =
		try {
			string.indexOf('/') match {
				case div if div >= 0 =>
					val num = string.substring(0, div).toLong
					val den = string.substring(div + 1).toLong
					Ratio(num, den)
				case _ => string.indexOf('.') match {
					case dot if dot >= 0 =>
						val whole = string.substring(0, dot).trim.toLong
						val fraction = string.substring(dot + 1).trim
						val num = fraction.toLong
						var den = 1L
						var i = fraction.length - 1
						while (i >= 0) {
							if (!fraction.charAt(i).isDigit)
								throw new NumberFormatException(
									"non-digit character in the fractional portion: '" + fraction.charAt(i) + "'"
								)
							if (den > Long.MaxValue / 10)
								throw new ArithmeticException("denominator overflow")
							den *= 10
							i -= 1
						}
						Ratio(whole) + Ratio(num, den)
					case _ =>
						Ratio(string.toLong)
				}
			}
		} catch {
			case n :NullPointerException => throw n
			case e :Exception =>
				throw new NumberFormatException(s"'$string' is not a valid Ratio: ${e.getMessage}.").initCause(e)
		}


	/** Implicit promotion of `Long` values to `Ratio`. Will be automatically performed without importing when a `Long`
	  * is passed to any method of a `Ratio` object.
	  */
	@inline implicit def longToRatio(int :Long) :Ratio = new Ratio(int, 1)

	/** Promotes an `Int`-based rational number implementation to use `Long` values. */
	@inline implicit def intRatioToRatio(ratio :IntRatio) :Ratio = ratio.toRatio


	/** The `Fractional` type class for type `Ratio`. */
	@SerialVersionUID(Ver)
	implicit object RatioIsFractional extends Fractional[Ratio] {
		override def div(x :Ratio, y :Ratio) :Ratio = x / y
		override def plus(x :Ratio, y :Ratio) :Ratio = x + y
		override def minus(x :Ratio, y :Ratio) :Ratio = x - y
		override def times(x :Ratio, y :Ratio) :Ratio = x * y
		override def negate(x :Ratio) :Ratio = -x
		override def fromInt(x :Int) :Ratio = new Ratio(x, 1)
		override def toInt(x :Ratio) :Int = x.toInt
		override def toLong(x :Ratio) :Long = x.toLong
		override def toFloat(x :Ratio) :Float = x.toFloat
		override def toDouble(x :Ratio) :Double = x.toDouble

		override def compare(x :Ratio, y :Ratio) :Int =
			if (x == y) 0 else if (x < y) -1 else 1 //checking for equality avoids checking both for < and >

		override def parseString(str :String) :Option[Ratio] =
			try { Some(Ratio(str)) } catch {
				case _ :Exception => None
			}
	}

	/** Introduces method [[net.noresttherein.sugar.numeric.Ratio.LongNumerator.%/ %/]] extension method to `Int`,
	  * allowing to create a [[net.noresttherein.sugar.numeric.Ratio Ratio]] through a natural looking expression
	  * `numerator %/ denominator`.
	  */
	implicit def int_%/(numerator :Int) :LongNumerator = new LongNumerator(numerator)
	/** Introduces method [[net.noresttherein.sugar.numeric.Ratio.LongNumerator.%/ %/]] extension method to `Long`,
	  * allowing to create a [[net.noresttherein.sugar.numeric.Ratio Ratio]] through a natural looking expression
	  * `numerator %/ denominator`.
	  */
	implicit def long_%/(numerator :Long) :LongNumerator = new LongNumerator(numerator)


	/** A factory of [[net.noresttherein.sugar.numeric.Ratio Ratio]] objects, accepting a `Long` denominator
	  * and constructing the rational number representing the division of the wrapped numerator value by the argument.
	  * @param numerator the numerator of created rational numbers (before reduction)
	  */
	class LongNumerator(private val numerator :Long) extends AnyVal {
		/** Creates the reduced form of fraction `this/100`. */
		@inline def % :Ratio = Ratio.percent(numerator)

		/** Creates the reduced form of fraction `this/100`. */
		@inline def percent :Ratio = Ratio.percent(numerator)

		/** Divides this `Long` by the argument, creating a [[net.noresttherein.sugar.numeric.Ratio Ratio]]
		  * number representing the result.
		  * @param denominator the denominator of the created rational (before reduction)
		  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
		  */
		@inline def %/(denominator :Long) :Ratio = Ratio(numerator, denominator)

		/** Divides this `Long` by the argument, creating a [[net.noresttherein.sugar.numeric.Ratio Ratio]]
		  * number representing the result.
		  * @param denominator the denominator of the created rational (before reduction)
		  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
		  */
		@inline def %/(denominator :Int) :Ratio = Ratio(numerator, denominator)
	}



	/** Calculates the greatest common divisor of any two integer values.
	  * @return The greatest integer `d` such that `a / d` and `b / d` are integral numbers.
	  */
	@inline def GCD(a :Long, b :Long) :Long =
		naturalGCD(if (a < 0) -a else a, if (b < 0) -b else b)

	private[numeric] def naturalGCD(a :Long, b :Long) :Long = {
		@tailrec def rec(a :Long, b :Long) :Long =
			if (b == 0) a
			else rec(b, a % b)
		if (a > b) rec(a, b)
		else rec(b, a)
	}

}

