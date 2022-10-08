package net.noresttherein.sugar.numeric

import java.math.{BigInteger, MathContext}

import scala.annotation.tailrec

import net.noresttherein.sugar.numeric.Decimal64.Round.ExtendedExact
import net.noresttherein.sugar.numeric.IntRatio.{naturalGCD, newIntRatio}




/** Simple implementation of rational numbers (fractions of integer values). The fraction is always
  * in its canonical form: the greatest common divisor of the numerator and the denominator is `1` and the denominator
  * is always a positive integer. All arithmetic operators check for overflow, throwing an [[ArithmeticException]]
  * if the result would not fit in a `IntRatio` value.
  * For the public constructor,
  * see [[net.noresttherein.sugar.numeric.IntRatio.apply(numerator:Int, denominator:Int)* IntRatio()]].
  * @author Marcin Mościcki marcin@moscicki.net
  */
@SerialVersionUID(ver)
final class IntRatio private(n :Int, d :Int) extends Number {

	/** The numerator of this fraction (the dividend). */
	@inline def numerator :Int = n

	/** The denominator of this fraction (the divisor). The denominator is always a positive number. */
	@inline def denominator :Int = d

	/** The sign of this rational number as a `Int`.
	  * @return `1` if this rational is positive, `-1` if it is negative, and `0` if it is zero.
	  */
	@inline def sign :Int = java.lang.Integer.signum(n)

	/** The absolute value of this rational number: `numerator.abs %/ denominator`. */
	@inline def abs :IntRatio =
		if (n >= 0) this else new IntRatio(-n, d)

	/** Overflow safe `-this.abs`. Method `Ratio.`[[net.noresttherein.sugar.numeric.Ratio.abs abs]] can overflow
	  * if the numerator is equal to `Long.MaxValue`, as `-Long.MaxValue` equals itself and is not a positive number.
	  * It is thus always better to compare `minusAbs` values for any two numbers rather than `abs`, as negation
	  * of `Long.MaxValue` does not underflow.
	  */
	@inline def minusAbs :Ratio = if (n <= 0) this else new Ratio(-n, d)

	/** The opposite rational number: `-numerator %/ denominator`. */
	@inline def unary_- :IntRatio = new IntRatio(-n, d)

	/** The fraction resulting from swapping the places of numerator and denominator. For all rational numbers
	  * `r <: IntRatio` `r * r.reciprocal == IntRatio(1)`
	  * (reciprocal is the opposite element with regard to multiplication).
	  * @return `denominator * this.sign %/ numerator * this.sign`
	  */
	@inline def reciprocal :IntRatio =
		if (n == 0)
			throw new ArithmeticException("Division by zero: (0/1).reciprocal")
		else if (n > 0)
			new IntRatio(d, n)
		else
			new IntRatio(-d, -n)

	/** Does this ratio represent an integral number, that is its numerator is a multiple of its denominator?. */
	@inline def isWhole :Boolean = n % d == 0

	/** Is this a proper fraction, meaning `numerator.abs < denominator`?. */
	@inline def isProper :Boolean = (if (n<0) -n else n) < denominator

	/** The whole part of this fraction, i.e. `numerator / denominator` (integral division). */
	@inline def whole :Int = n / d

	/** The fractional part of this fraction, i.e. `IntRatio(numerator % denominator, denominator)`. */
	@inline def fraction :IntRatio = new IntRatio(n % d, d)



	private def plus(num :Long, den :Long) :IntRatio = {
		val bigNum = num * d + n * den
		val bigDen = d * den
		val gcd = naturalGCD(if (bigNum < 0) -bigNum else bigNum, bigDen)
		val resNum = bigNum / gcd; val resDen = bigDen / gcd
		val intNum = resNum.toInt; val intDen = resDen.toInt
		if (-resNum != -intNum || -resDen != -intDen)
			throw new ArithmeticException(
				s"IntRatio overflow: $this + $num/$den = $resNum/$resDen cannot be represented as an Int fraction."
			)
		newIntRatio(intNum, intDen)
	}

	@inline def +(other :IntRatio) :IntRatio = plus(other.numerator, other.denominator)

	@inline def -(other :IntRatio) :IntRatio = plus(-other.numerator, other.denominator)

	/** Multiplies this rational number by another rational. This algorithm promotes the values to the `Long` type
	  * for the calculation process to avoid temporary arithmetic overflow resulting from `Int` multiplication. The
	  * overflow in the result can still occur however if the numerator and denominator after reduction by their ''GCD''
	  * doesn't fit in the `Int` type.
	  */
	def *(other :IntRatio) :IntRatio = {
		val num = n * other.numerator.toLong
		val den = d * other.denominator.toLong
		val gcd = naturalGCD(if (num < 0) -num else num, den)
		val reducedNum = num / gcd; val resNum = reducedNum.toInt
		val reducedDen = den / gcd; val resDen = reducedDen.toInt
		if (-reducedNum != -resNum || -reducedDen != -resDen) //checks both for overflow and Int.MinValue
			throw new ArithmeticException(s"IntRatio multiplication overflow: $reducedNum/$reducedDen.")
		new IntRatio(resNum, resDen)
	}

	/** Divides this rational number by another rational. This algorithm promotes the values to the `Long` type
	  * for the calculation process to avoid temporary arithmetic overflow resulting from `Int` multiplication. The
	  * overflow in the result can still occur however if the numerator and denominator after reduction by their ''GCD''
	  * doesn't fit in the `Int` type.
	  */
	@inline def /(other :IntRatio) :IntRatio = IntRatio(n * other.denominator.toLong, d * other.numerator.toLong)


	def <(other :IntRatio) :Boolean = {
		val n2 = other.numerator; val d2 = other.denominator
		if (n < 0)
			n2 >= 0 || { val gcd = naturalGCD(d, d2).toLong; d2 / gcd * n < d / gcd * n2 }
		else
			n2 > 0 && { val gcd = naturalGCD(d, d2).toLong; d2 / gcd * n < d / gcd * n2 }
	}

	def <=(other :IntRatio) :Boolean = {
		val n2 = other.numerator; val d2 = other.denominator
		if (n <= 0)
			n2 >= 0 || { val gcd = naturalGCD(d, d2).toLong; d2 / gcd * n <= d / gcd * n2 }
		else
			n2 > 0 && { val gcd = naturalGCD(d, d2).toLong; d2 / gcd * n <= d / gcd * n2 }
	}

	@inline def >(other :IntRatio) :Boolean = !(this <= other)
	@inline def >=(other :IntRatio) :Boolean = !(this < other)


	@inline def min(other :IntRatio) :IntRatio = if (this <= other) this else other
	@inline def max(other :IntRatio) :IntRatio = if (this < other) other else this


	@inline def toInt            :Int        = n / d
	@inline def toFloat          :Float      = n.toFloat / d.toFloat
	@inline def toDouble         :Double     = n.toDouble / d.toDouble
	@inline def toShort          :Short      = (n / d).toShort
	@inline def toLong           :Long       = (n / d).toLong
	@inline def toRatio          :Ratio      = new Ratio(n, d)
	@inline def toBigInt         :BigInt     = BigInt(toLong)
	@inline def toBigInteger     :BigInteger = BigInteger.valueOf(toLong)
	@inline def toDecimal64      :Decimal64  = Decimal64(n) / Decimal64(d)
	@inline def toDecimal64Exact :Decimal64  = Decimal64(n).div(Decimal64(d), ExtendedExact)

	@inline def toBigDecimal(implicit mc :MathContext = BigDecimal.defaultMathContext) :BigDecimal =
		BigDecimal(n, mc) / d

	@inline def toBigDecimalExact :BigDecimal = toBigDecimal(MathContext.UNLIMITED)

	@inline def toJavaBigDecimal(implicit mc :MathContext = MathContext.UNLIMITED) :java.math.BigDecimal =
		new java.math.BigDecimal(n).divide(new java.math.BigDecimal(d), mc)


	@inline override def intValue :Int = n / d
	@inline override def floatValue :Float = n.toFloat / d.toFloat
	@inline override def doubleValue :Double = n.toDouble / d.toDouble
	@inline override def shortValue :Short = (n / d).toShort
	@inline override def longValue :Long = (n / d).toLong


	override def equals(that :Any) :Boolean = that match {
		case r :IntRatio => n == r.numerator & d == r.denominator
		case _ => false
	}

	override def hashCode :Int = d * 31 + n

	override def toString :String = n.toString + "/" + d
}






/** Companion object for the [[net.noresttherein.sugar.numeric.IntRatio! IntRatio]] class representing rational numbers
  * as fractions in their canonical form. Contains an implicit conversion promoting `Int`s to `IntRatio`s
  * when given as the parameter to `IntRatio`'s methods.
  */
@SerialVersionUID(ver)
object IntRatio {//extends IntRatioImplicits {

	/** Number zero as a `IntRatio` value: `0/1`. */
	final val Zero :IntRatio = new IntRatio(0, 1)

	/** Number one as a `IntRatio` value: `1/1`. */
	final val One :IntRatio = new IntRatio(1, 1)

	/** Maximum value held by the `IntRatio` type: `Int.MaxValue / 1`. */
	final val MaxValue = new IntRatio(Int.MaxValue, 1)

	/** Minimum value held by the `IntRatio` type: `-Int.MaxValue / 1`.
	  * Note that this is actually larger by one than `Int.MinValue`, as the `Int.MinValue/1` is not a valid `IntRatio`.
	  * This type checks for arithmetic overflows and underflows, and it ensures that unary `-` and `abs` operations
	  * will be properly defined and never throw any exception. In addition, it reduces the number of checks required
	  * to ensure validity in the constructor and arithmetic operations.
	  */
	final val MinValue = new IntRatio(Int.MinValue + 1, 1)

	/** The smallest positive value held by this type: `1/Int.MaxValue`. As `-Precision` is also the largest negative
	  * `IntRatio` value, it is the smallest possible non-zero difference between two values.
	  */
	final val Precision = new IntRatio(1, Int.MaxValue)

	private final val CachedIntegers = 100
	private val Integers =
		(-CachedIntegers to -1).map(new IntRatio(_, 1)) ++
			(0 to CachedIntegers).map(new IntRatio(_, 1)) to Array
	private final val CachedUnits = 100
	private val Units =
		(CachedUnits to 1 by -1).map(new IntRatio(-1, _)) ++
			(1 to CachedUnits).map(new IntRatio(1, _)) to Array
	private val Percents =
		(-100 until 0).map(reduce(_, 100)) ++ (0 to 100).map(reduce(_, 100)) to Array


	@inline private def reduce(numerator :Int, denominator :Int) :IntRatio = {
		val gcd = GCD(numerator, denominator)
		new IntRatio(numerator / gcd, denominator / gcd)
	}

	private[numeric] def newIntRatio(numerator :Int, denominator :Int) :IntRatio = denominator match {
		case 1 => IntRatio(numerator)
		case 100 => percent(numerator)
		case 0 => throw new ArithmeticException("IntRatio " + numerator + "/0")
		case _ if numerator == 1 => unit(denominator)
		case _ => new IntRatio(numerator, denominator)
	}



	/** Creates a unit fraction, that is one with `1` as the numerator and the given denominator.
	  * Passing a negative value will result in a negative `IntRatio` with `-1` as the numerator
	  * and `denominator.abs` as the denominator.
	  * @param denominator requested denominator of the fraction.
	  * @return an `IntRatio` equal to `1 / denominator`.
	  * @throws ArithmeticException if `denominator` equals zero.
	  * @throws IllegalArgumentException if `denominator` equals `Int.MinValue`.
	  */
	def unit(denominator :Int) :IntRatio =
		if (denominator > 0)
			if (denominator <= CachedUnits)
				Units(CachedUnits + denominator - 1)
			else
				new IntRatio(1, denominator)
		else if (denominator < 0)
			if (denominator >= -CachedUnits)
				Units(CachedUnits + denominator)
			else if (denominator == Int.MinValue)
				throw new IllegalArgumentException("Can't represent 1/Int.MinValue as an IntRatio.")
			else
				new IntRatio(-1, -denominator)
		else
			throw new ArithmeticException("Ratio 1/0")

	/** A reduced fraction `n / 100`.*/
	def percent(n :Int) :IntRatio =
		if (-100 <= n & n <= 100) Percents(100 + n)
		else reduce(n, 100)

	/** Creates a whole rational (a fraction with `1` as the denominator and the argument as the numerator).
	  * @return an `IntRatio` of `integer/1`.
	  * @throws IllegalArgumentException if integer equals Int.MinValue.
	  */
	def apply(integer :Int) :IntRatio =
		if (-CachedIntegers <= integer & integer <= CachedIntegers)
			Integers(CachedIntegers + integer)
		else if (integer == Int.MinValue)
			throw new IllegalArgumentException("Can't represent Int.MinValue as an IntRatio.")
		else
	        new IntRatio(integer, 1)


	/** Creates a rational number in the canonical form.
	  * @param numerator the dividend (the numerator of the fraction)
	  * @param denominator the divisor (the denominator of the fraction)
	  * @throws ArithmeticException if denominator is zero or one of the arguments is `Int.MinValue`
	  *                             and result would overflow.
	  * @return an `IntRatio` representing the fraction `numerator / denominator` after dividing them by their greatest
	  *         common divisor and adjusting the signums so that denominator is positive.
	  */
	def apply(numerator :Int, denominator :Int) :IntRatio =
		if (denominator == 100)
			percent(numerator)
		else if (denominator > 0) {
			val divisor =
				if (numerator >= 0)
					naturalGCD(numerator, denominator)
				else if (numerator == Int.MinValue) {
					val d = naturalGCD(-numerator.toLong, denominator.toLong)
					if (d == 1)
						throw new ArithmeticException(
							s"Can't represent $numerator/$denominator as an IntRatio: no representation of -Int.MinValue."
						)
					d.toInt
				} else
					naturalGCD(-numerator, denominator)
			newIntRatio(numerator / divisor, denominator / divisor)
		} else if (denominator < 0) {
			if (denominator == Int.MinValue || numerator == Int.MinValue) {
				val divisor = naturalGCD(if (numerator >= 0) numerator.toLong else -numerator.toLong, -denominator.toLong)
				if (divisor == 1)
					throw new ArithmeticException(
						s"Can't represent $numerator/$denominator as an IntRatio: no representation of -Int.MinValue."
					)
				newIntRatio((-(numerator / divisor)).toInt, (-(denominator / divisor)).toInt)
			} else {
				val divisor = naturalGCD(if (numerator > 0) numerator else -numerator, -denominator)
				newIntRatio(-(numerator / divisor), -(denominator / divisor))
			}
		} else
			throw new ArithmeticException("IntRatio: division by zero.")


	/** Creates a rational number in the canonical form. Returned [[net.noresttherein.sugar.numeric.IntRatio IntRatio]]
	  * object still uses `Int`s as the numerator and denominator types, but accepting `Long` arguments here
	  * gives a chance of avoiding overflow in arithmetic operations if the values after division by their ''GCD''
	  * fit into `Int`s.
	  * @param numerator the dividend (the numerator of the fraction)
	  * @param denominator the divisor (the denominator of the fraction)
	  * @throws ArithmeticException if denominator is zero or arithmetic overflow/underflow occurs during reduction.
	  * @return an `IntRatio` representing the fraction `numerator / denominator` after dividing them by their greatest
	  *         common divisor and adjusting the signums so that denominator is positive.
	  */
	def apply(numerator :Long, denominator :Long) :IntRatio =
		if (denominator > 0) {
			if (numerator >= 0) {
				val divisor = naturalGCD(numerator, denominator)
				val n = numerator / divisor; val d = denominator / divisor
				if (n > Int.MaxValue || d > Int.MaxValue)
					throw new ArithmeticException(s"IntRatio overflow: $n/$d ($numerator/$denominator).")
				newIntRatio(n.toInt, d.toInt)
			} else if (numerator == Long.MinValue)
				throw new IllegalArgumentException(s"IntRatio overflow: $numerator/$denominator.")
			else {
				val divisor = naturalGCD(-numerator, denominator)
				val n = numerator / divisor
				val d = denominator / divisor
				if (n <= Int.MinValue || d > Int.MaxValue)
					throw new ArithmeticException(s"IntRatio overflow: $n/$d ($numerator/$denominator).")
				newIntRatio(n.toInt, d.toInt)
			}
		} else if (denominator < 0) {
			if (denominator == Long.MinValue)
				throw new IllegalArgumentException(s"IntRatio overflow: $numerator/$denominator.")
			else if (numerator >= 0) {
				val divisor = naturalGCD(numerator, -denominator)
				val n = numerator / divisor; val d = denominator / divisor
				if (n > Int.MaxValue || d <= Int.MinValue)
					throw new ArithmeticException(s"IntRatio overflow: $n/$d ($numerator/$denominator).")
				newIntRatio(-n.toInt, -d.toInt)
			} else if (numerator == Long.MinValue)
		       throw new IllegalArgumentException(s"IntRatio overflow: $numerator/$denominator.")
			else {
				val divisor = naturalGCD(-numerator, -denominator)
				val n = numerator / divisor
				val d = denominator / divisor
				if (n <= Int.MinValue || d <= Int.MinValue)
					throw new ArithmeticException(s"IntRatio overflow: $n/$d ($numerator/$denominator).")
				newIntRatio(-n.toInt, -d.toInt)
			}
		} else
			  throw new ArithmeticException("IntRatio: division by zero.")


	/** Parses the string as a ratio of two `Int` numbers.
	  * @param string a rational number in one of the following formats:
	  *                 1. a base 10 `Int` number, acceptable by `string.toInt`,
	  *                 1. a decimal fraction using '.' character as the fractional part separator,
	  *                 1. ''numerator/denominator'', where both the numerator and denominator are valid `Int` numbers
	  *                    and denominator is positive.
	  * @return the number as a canonical fraction.
	  */
	def apply(string :String) :IntRatio = {
		val res = Ratio(string)
		if (res.numerator.isValidInt && res.denominator.isValidInt)
			newIntRatio(res.numerator.toInt, res.denominator.toInt)
		else
			throw new NumberFormatException(s"'$string' is not a valid IntRatio: arithmetic overflow.")
	}


	/** Implicit promotion of an integer to a rational. Will be automatically performed without importing when an `Int`
	  * is passed to any method of a `IntRatio` object.
	  */
	@inline implicit def intToIntRatio(int :Int) :IntRatio = apply(int)



	/** The `Fractional` type class for `IntRatio` values. */
	@SerialVersionUID(ver)
	implicit object IntRatioIsFractional extends Fractional[IntRatio] {
		override def div(x :IntRatio, y :IntRatio) :IntRatio = x / y
		override def plus(x :IntRatio, y :IntRatio) :IntRatio = x + y
		override def minus(x :IntRatio, y :IntRatio) :IntRatio = x - y
		override def times(x :IntRatio, y :IntRatio) :IntRatio = x * y
		override def negate(x :IntRatio) :IntRatio = -x
		override def fromInt(x :Int) :IntRatio = x
		override def toInt(x :IntRatio) :Int = x.toInt
		override def toLong(x :IntRatio) :Long = x.toLong
		override def toFloat(x :IntRatio) :Float = x.toFloat
		override def toDouble(x :IntRatio) :Double = x.toDouble

		override def compare(x :IntRatio, y :IntRatio) :Int =
			if (x < y) -1 else if (y < x) 1 else 0

		override def parseString(str :String) :Option[IntRatio] =
			try { Some(IntRatio(str)) } catch {
				case _ :NumberFormatException => None
			}
	}


	/** Introduces method [[net.noresttherein.sugar.numeric.IntRatio.IntNumerator.%/ %/]] extension method to `Int`,
	  * allowing to create a [[net.noresttherein.sugar.numeric.IntRatio IntRatio]] through a natural looking expression
	  * `numerator %/ denominator`.
	  */
	implicit def intRatio_%/(numerator :Int) :IntNumerator = new IntNumerator(numerator)

	/** A builder of [[net.noresttherein.sugar.numeric.IntRatio IntRatio]] objects, accepting an `Int` denominator
	  * and constructing the rational number representing the division of the wrapped numerator values by the argument.
	  * @param numerator the numerator of created rational numbers (before reduction)
	  */
	class IntNumerator(private val numerator :Int) extends AnyVal {
		/** Creates a reduced `IntRatio` of `this/100`. */
		@inline def % :IntRatio = percent(numerator)

		/** Divides this `Int` by the argument, creating a [[net.noresttherein.sugar.numeric.IntRatio IntRatio]] number
		  * representing the result.
		  * @param denominator the denominator of the created rational (before reduction)
		  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
		  */
		@inline def %/(denominator :Int) :IntRatio = IntRatio(numerator, denominator)

		/** Divides this `Int` by the argument, creating a [[net.noresttherein.sugar.numeric.Ratio Ratio]]
		  * number representing the result.
		  * @param denominator the denominator of the created rational (before reduction)
		  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
		  */
		@inline def %/(denominator :Long) :Ratio = Ratio(numerator, denominator)
	}



	/** Calculates the greatest common divisor of any two integer values.
	  * @return The greatest integer `d` such that `a / d` and `b / d` are integral numbers.
	  */
	@inline def GCD(a :Int, b :Int) :Int =
		naturalGCD(if (a < 0) -a else a, if (b < 0) -b else b)

	private def naturalGCD(a :Int, b :Int) :Int = {
		@tailrec def rec(a :Int, b :Int) :Int =
			if (b==0) a
			else rec(b, a % b)
		if (a > b) rec(a, b)
		else rec(b, a)
	}


	private def naturalGCD(a :Long, b :Long) :Long = {
		@tailrec def rec(a :Long, b :Long) :Long =
			if (b==0) a
			else rec(b, a % b)
		if (a > b) rec(a, b)
		else rec(b, a)
	}


}
