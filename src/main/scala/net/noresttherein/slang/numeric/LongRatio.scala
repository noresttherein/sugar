package net.noresttherein.slang.numeric

import net.noresttherein.slang.numeric.LongRatio.{GCD, naturalGCD}

import scala.annotation.tailrec


/** Simple implementation of rational numbers using `Long` values for the numerator and the denominator. 
  * The fraction is always in its canonical form: the greatest common divisor of the numerator and the denominator 
  * is `1` and the denominator is always a positive number. Unlike [[net.noresttherein.slang.numeric.Ratio Ratio]],
  * this class does not check for arithmetic overflow/underflow. Operators are implemented in a way eliminating the
  * possibility of ''temporary'' overflows, but no exception is thrown if the result value overflows.
  * For the public constructor, see [[net.noresttherein.slang.numeric.LongRatio$#apply LongRatio()]].
  */
final class LongRatio private[numeric] (n :Long, d :Long) extends Number {

	/** The numerator of this fraction (the dividend). */
	@inline def numerator :Long = n

	/** The denominator of this fraction (the divisor). The denominator is always a positive number. */
	@inline def denominator :Long = d

	/** The sign of this rational number as a `Long`.
	  * @return `1` if this rational is positive, `-1` if it is negative, and `0` if it is zero.
	  */
	@inline def sign :Long = java.lang.Long.signum(n)

	/** The opposite rational number: `-numerator /% denominator`. */
	@inline def unary_- :LongRatio = new LongRatio(-n, d)

	/** The absolute value of this rational number: `numerator.abs /% denominator`. */
	@inline def abs :LongRatio =
		if (n >= 0) this else new LongRatio(-n, d)

	/** The fraction resulting from swapping the places of numerator and denominator. For all rational numbers
	  * `r &lt;: LongRatio` `r * r.reciprocal == LongRatio(1)` (reciprocal is the opposite element with regard to multiplication).
	  * @return `denominator * this.signum /% numerator * this.signum`
	  */
	def reciprocal :LongRatio =
		if (n == 0)
			throw new ArithmeticException("Division by zero: (0/1).reciprocal")
		else if (n > 0)
			new LongRatio(d, n)
		else
			new LongRatio(-d, -n)


	/** Does this ratio represent an integral number, that is its numerator is a multiple of its denominator?. */
	@inline def isWhole :Boolean = n % d == 0

	/** Is this a proper fraction, meaning `numerator.abs < denominator`?. */
	@inline def isProper :Boolean = (if (n<0) -n else n) < denominator

	/** The whole part of this fraction, i.e. `numerator / denominator` (integral division). */
	@inline def whole :Long = n / d

	/** The fractional part of this fraction, i.e. `LongRatio(numerator % denominator, denominator)`. */
	@inline def fraction :LongRatio = new LongRatio(n % d, d)



	private def plus(num :Long, den :Long) :LongRatio = {
		val gcd = naturalGCD(d, den)
		val multiplier = d / gcd
		val lcm = multiplier * den
		val resNum = multiplier * num + den / gcd * n
		val resGCD = naturalGCD(if (resNum < 0) -resNum else resNum, lcm)
		new LongRatio(resNum / resGCD, lcm / resGCD)
	}

	def +(other :LongRatio) :LongRatio = plus(other.numerator, other.denominator)

	/** The right-associative variant of [[net.noresttherein.slang.numeric.LongRatio#+ +]]. Equivalent to `other + this`,
	  * but can implicitly promote `Long` values on the left hand side of the operator to the required rational.
	  */
	@inline def +:(other :LongRatio) :LongRatio = other.plus(n, d)


	@inline def -(other :LongRatio) :LongRatio = plus(-other.numerator, other.denominator)

	/** The right-associative variant of [[net.noresttherein.slang.numeric.LongRatio#- -]]. Equivalent to `other - this`,
	  * but can implicitly promote `Long` values on the left hand side of the operator to the required rational.
	  */
	@inline def -:(other :LongRatio) :LongRatio = other.plus(-n, d)


	private def multiply(num :Long, den :Long) :LongRatio = {
		val gcd_\ = GCD(n, den); val gcd_/ = GCD(d, num)
		val resNum = (n / gcd_\) * (num / gcd_/)
		val resDen = (d / gcd_/) * (den / gcd_\)
		if (resDen < 0)
			new LongRatio(-resNum, -resDen)
		else
			new LongRatio(resNum, resDen)
	}

	/** Multiplies this rational number by another rational. This implementation performs cross reduction before actual
	  * multiplication, but may still result in quiet overflow.
	  */
	def *(other :LongRatio) :LongRatio = {
		val n1 = other.numerator; val d1 = other.denominator
		val gcd_\ = naturalGCD(if (n >= 0) n else -n, d1)
		val gcd_/ = naturalGCD(d, if (n1 >= 0) n1 else -n1)
		val num = (n / gcd_\) * (n1 / gcd_/)
		val den = (d / gcd_/) * (d1 / gcd_\)
		if (den < 0)
			new LongRatio(-num, -den)
		else
			new LongRatio(num, den)
	}

	/** Multiplies this rational by another rational. This method shared implementation
	  * with [[net.noresttherein.slang.numeric.LongRatio#* *]], but is right associative and can promote `Long` values
	  * on the left side of the operator via an implicit conversion.
	  */
	@inline def *:(other :LongRatio) :LongRatio = other * this


	/** Divides this rational number by another rational. This implementation performs cross reduction before actual
	  * multiplication, but may still result in quiet overflow.
	  */
	def /(other :LongRatio) :LongRatio = {
		val n1 = other.numerator; val d1 = other.denominator
		if (n1 == 0)
			throw new ArithmeticException(s"Rational division by zero: $this / $other")
		val gcd_- = naturalGCD(if (n >= 0) n else -n, if (n1 >= 0) n1 else -n1)
		val gcd__ = naturalGCD(d, d1)
		val num = (n / gcd_-) * (d1 / gcd__)
		val den = (d / gcd__) * (n1 / gcd_-)
		if (den < 0)
			new LongRatio(-num, -den)
		else
			new LongRatio(num, den)
	}

	/** Divides the rational given as the argument by this rational. This is equivalent to `other / this`,
	  * but, being right-associative, can promote `Long` values on the left side of the operator via an implicit conversion.
	  */
	@inline def /:(other :LongRatio) :LongRatio = other / this


	def <(other :LongRatio) :Boolean = {
		val n2 = other.numerator; val d2 = other.denominator
		if (n < 0)
			n2 >= 0 || { val gcd = naturalGCD(d, d2); d2 / gcd * n < d / gcd * n2 }
		else
			n2 > 0 && { val gcd = naturalGCD(d, d2); d2 / gcd * n < d / gcd * n2 }
	}

	def <=(other :LongRatio) :Boolean = {
		val n2 = other.numerator; val d2 = other.denominator
		if (n <= 0)
			n2 >= 0 || { val gcd = naturalGCD(d, d2); d2 / gcd * n <= d / gcd * n2 }
		else
			n2 > 0 && { val gcd = naturalGCD(d, d2); d2 / gcd * n <= d / gcd * n2 }
	}

	@inline def >(other :LongRatio) :Boolean = !(this <= other)

	@inline def >=(other :LongRatio) :Boolean = !(this < other)



	@inline def min(other :LongRatio) :LongRatio = if (this <= other) this else other

	@inline def max(other :LongRatio) :LongRatio = if (this < other) other else this


	@inline def toLong :Long = n / d

	@inline def toFloat :Float = n.toFloat / d.toFloat

	@inline def toDouble :Double = n.toDouble / d.toDouble

	@inline def toShort :Short = (n / d).toShort

	@inline def toInt :Int = (n / d).toInt



	@inline override def intValue :Int = (n / d).toInt

	@inline override def floatValue :Float = n.toFloat / d.toFloat

	@inline override def doubleValue :Double = n.toDouble / d.toDouble

	@inline override def shortValue :Short = (n / d).toShort

	@inline override def longValue :Long = n / d



	override def equals(that :Any) :Boolean = that match {
		case r :LongRatio => n == r.numerator & d == r.denominator
		case _ => false
	}

	override def hashCode :Int = java.lang.Long.hashCode(d) * 31 + java.lang.Long.hashCode(n)

	override def toString :String = n.toString + "/" + d
}




/** Companion object for the `LongRatio` class representing rational numbers as fractions in their canonical form.
  * Contains an implicit conversion promoting `Long`s to `LongRatio`s when given as the parameter to `LongRatio`'s methods.
  */
object LongRatio {

	/** Number zero represented as a ratio of `0/1`. */
	final val Zero :LongRatio = new LongRatio(0, 1)

	/** Number one represented as a ration of `1/1`. */
	final val One :LongRatio = new LongRatio(1, 1)

	/** Maximal value held by this type: `Long.MaxValue / 1`. */
	final val MaxValue = new LongRatio(Long.MaxValue, 1)

	/** Minimal value held by this type: `Long.MinValue / 1`. */
	final val MinValue = new LongRatio(Long.MinValue, 1)

	/** Minimal positive value held by this type: `1 / Long.MaxValue`. As the ratio `1/Long.MinValue` can not be held
	  * by this type because of lack of `Long` representation of `-Long.MinValue` and the positive denominator requirement,
	  * it is also the smallest possible precision and difference between two `LongRatio` values.
	  */
	final val Precision = new LongRatio(1, Long.MaxValue)



	/** Creates a unit fraction, that is one with `1` as the numerator and the given denominator. Note that passing
	  * a negative value will result in a negative `LongRatio` with `-1` as the numerator and `denominator.abs` as the denominator.
	  * @param denominator requested denominator of the fraction.
	  * @return Ratio equal to `1 / denominator`
	  * @throws ArithmeticException if denominator equals zero.
	  * @throws IllegalArgumentException if denominator equals `Long.MinValue`.
	  */
	def unit(denominator :Long) :LongRatio =
		if (denominator > 0)
			new LongRatio(1, denominator)
		else if (denominator < 0)
			if (denominator == Long.MinValue)
				throw new IllegalArgumentException("Can't represent 1/Long.MinValue as a LongRatio")
			else
                new LongRatio(-1, -denominator)
		else
			throw new ArithmeticException("LongRatio 1/0")


	/** Creates a whole rational (a fraction with `1` as the denominator and the argument as the numerator). */
	def apply(integer :Long) :LongRatio = new LongRatio(integer, 1)


	/** Creates a rational number in the canonical form.
	  * @param numerator the dividend (the numerator of the fraction)
	  * @param denominator the divisor (the denominator of the fraction)
	  * @throws ArithmeticException if denominator is zero or one of the arguments is `Long.MinValue` and result would overflow.
	  * @return a `LongRatio` representing the fraction `numerator / denominator` after dividing them by their greatest
	  *         common divisor and adjusting the signums so that denominator is positive.
	  */
	def apply(numerator :Long, denominator :Long) :LongRatio =
		if (denominator > 0) {
			val divisor =
				if (numerator >= 0)
					naturalGCD(numerator, denominator)
				else if (numerator == Long.MinValue) {
					val d = naturalGCD(-numerator.toLong, denominator.toLong)
					if (d == 1)
						throw new ArithmeticException(s"Can't represent $numerator/$denominator as a LongRatio: no representation of -Long.MinValue")
					d.toLong
				} else
                    naturalGCD(-numerator, denominator)
			new LongRatio(numerator / divisor, denominator / divisor)
		} else if (denominator < 0) {
			if (denominator == Long.MinValue || numerator==Long.MinValue) {
				val divisor = naturalGCD(if (numerator >= 0) numerator.toLong else -numerator.toLong, -denominator.toLong)
				if (divisor == 1)
					throw new ArithmeticException(s"Can't represent $numerator/$denominator as a LongRatio: no representation of -Long.MinValue")
				new LongRatio((-(numerator / divisor)).toLong, (-(denominator / divisor)).toLong)
			} else {
				val divisor = naturalGCD(if (numerator > 0) numerator else -numerator, -denominator)
				new LongRatio(-(numerator / divisor), -(denominator / divisor))
			}
		} else
			  throw new ArithmeticException("LongRatio: division by zero")




	/** Implicit promotion of Long values to `Rational`. Will be automatically performed without importing when an `Long`
	  * is passed to any method of a `LongRatio` object.
	  */
	@inline implicit def longToLongRatio(int :Long) :LongRatio = new LongRatio(int, 1)


	/** Promotes an `Int`-based rational number implementation to use `Long` values. */
	@inline implicit def ratioToLongRatio(ratio :Ratio) :LongRatio = ratio.toLongRatio

	/** The `Fractional` type class for `LongRatio` values. */
	implicit object LongRatioIsFractional extends Fractional[LongRatio] {
		override def div(x :LongRatio, y :LongRatio) :LongRatio = x / y

		override def plus(x :LongRatio, y :LongRatio) :LongRatio = x + y

		override def minus(x :LongRatio, y :LongRatio) :LongRatio = x - y

		override def times(x :LongRatio, y :LongRatio) :LongRatio = x * y

		override def negate(x :LongRatio) :LongRatio = -x

		override def fromInt(x :Int) :LongRatio = new LongRatio(x, 1)

		override def toInt(x :LongRatio) :Int = x.toInt

		override def toLong(x :LongRatio) :Long = x.toLong

		override def toFloat(x :LongRatio) :Float = x.toFloat

		override def toDouble(x :LongRatio) :Double = x.toDouble



		override def compare(x :LongRatio, y :LongRatio) :Int =
			if (x < y) -1 else if (y < x) 1 else 0

		override def parseString(str :String) :Option[LongRatio] = str.toLongOption.map(LongRatio.apply)
	}


	/** Implicit conversion extending `Long` values with a `/%` method accepting other another `Long` and
	  * constructing a [[net.noresttherein.slang.numeric.LongRatio LongRatio]] instance as an alternative to `LongRatio`
	  * object's factory method. If you wish to perform other arithmetic operations with `Long` values
	  * as the left-hand argument use the appropriate right-associative method of the `LongRatio` class.
	  * @param numerator this integer, serving as thee numerator of the future rational
	  * @return a builder object accepting the denominator for the rational result.
	  */
	@inline implicit def /%(numerator :Long) :DivisionLongRatioConstructor = new DivisionLongRatioConstructor(numerator)

	/** A builder of [[net.noresttherein.slang.numeric.LongRatio LongRatio]] objects, accepting an `Long` denominator
	  * and constructing the rational number representing the division of the wrapped numerator values by the argument.
	  * @param numerator the numerator of created rational numbers (before reduction)
	  */
	class DivisionLongRatioConstructor(private val numerator :Long) extends AnyVal {
		/** Divides this `Long` by the argument, creating a [[net.noresttherein.slang.numeric.LongRatio LongRatio]]
		  * number representing the result.
		  * @param denominator the denominator of the created rational (before reduction)
		  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
		  */
		@inline def /%(denominator :Long) :LongRatio = LongRatio(numerator, denominator)
	}



	/** Calculates the greatest common divisor of any two integer values.
	  * @return The greatest integer `d` such that `a / d` and `b / d` are integral numbers.
	  */
	@inline def GCD(a :Long, b :Long) :Long =
		naturalGCD(if (a < 0) -a else a, if (b < 0) -b else b)

	private def naturalGCD(a :Long, b :Long) :Long = {
		@tailrec def rec(a :Long, b :Long) :Long =
			if (b==0) a
			else rec(b, a % b)
		if (a > b) rec(a, b)
		else rec(b, a)
	}




}
