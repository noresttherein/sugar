package net.noresttherein.slang.numeric

import scala.annotation.tailrec
import net.noresttherein.slang.numeric.Ratio.naturalGCD


/** Simple implementation of rational numbers (fractions of integer values). The fraction is always in its canonical form:
  * the greatest common divisor of the numerator and the denominator is `1` and the denominator is always a positive integer.
  * All arithmetic operators check for overflow, throwing `ArithmeticException` if the result would not fit in a `Ratio`
  * value.
  * For the public constructor, see [[net.noresttherein.slang.numeric.Ratio.apply(numerator:Int, denominator:Int)* Ratio()]].
  * @author Marcin Mościcki marcin@moscicki.net
  */
final class Ratio private(n :Int, d :Int) extends Number {

	/** The numerator of this fraction (the dividend). */
	@inline def numerator :Int = n

	/** The denominator of this fraction (the divisor). The denominator is always a positive number. */
	@inline def denominator :Int = d

	/** The sign of this rational number as a `Int`.
	  * @return `1` if this rational is positive, `-1` if it is negative, and `0` if it is zero.
	  */
	@inline def sign :Int = java.lang.Integer.signum(n)

	/** The opposite rational number: `-numerator /% denominator`. */
	@inline def unary_- :Ratio = new Ratio(-n, d)

	/** The absolute value of this rational number: `numerator.abs /% denominator`. */
	@inline def abs :Ratio =
		if (n >= 0) this else new Ratio(-n, d)

	/** The fraction resulting from swapping the places of numerator and denominator. For all rational numbers 
	  * `r <: Ratio` `r * r.reciprocal == Ratio(1)` (reciprocal is the opposite element with regard to multiplication).
	  * @return `denominator * this.sign /% numerator * this.sign`
	  */
	@inline def reciprocal :Ratio =
		if (n == 0)
			throw new ArithmeticException("Division by zero: (0/1).reciprocal")
		else if (n > 0)
			new Ratio(d, n)
		else
			new Ratio(-d, -n)

	/** Does this ratio represent an integral number, that is its numerator is a multiple of its denominator?. */
	@inline def isWhole :Boolean = n % d == 0

	/** Is this a proper fraction, meaning `numerator.abs < denominator`?. */
	@inline def isProper :Boolean = (if (n<0) -n else n) < denominator

	/** The whole part of this fraction, i.e. `numerator / denominator` (integral division). */
	@inline def whole :Int = n / d

	/** The fractional part of this fraction, i.e. `Ratio(numerator % denominator, denominator)`. */
	@inline def fraction :Ratio = new Ratio(n % d, d)



	private def plus(num :Long, den :Long) :Ratio = {
		val bigNum = num * d + n * den
		val bigDen = d * den
		val gcd = naturalGCD(if (bigNum < 0) -bigNum else bigNum, bigDen)
		val resNum = bigNum / gcd; val resDen = bigDen / gcd
		val intNum = resNum.toInt; val intDen = resDen.toInt
		if (-resNum != -intNum || -resDen != -intDen)
			throw new ArithmeticException(s"Ratio overflow: $this + $num/$den = $resNum/$resDen cannot be represented as an Int fraction")
		new Ratio(intNum, intDen)
	}


	@inline def +(other :Ratio) :Ratio = plus(other.numerator, other.denominator)

	/** The right-associative variant of [[net.noresttherein.slang.numeric.Ratio.+ +]]. Equivalent to `other + this`,
	  * but can implicitly promote `Int` values on the left hand side of the operator to the required rational.
	  */
	@inline def +:(other :Ratio) :Ratio = other.plus(n, d)


	@inline def -(other :Ratio) :Ratio = plus(-other.numerator, other.denominator)

	/** The right-associative variant of [[net.noresttherein.slang.numeric.Ratio.- -]]. Equivalent to `other - this`,
	  * but can implicitly promote `Int` values on the left hand side of the operator to the required rational.
	  */
	@inline def -:(other :Ratio) :Ratio = other.plus(-n, d)


	/** Multiplies this rational number by another rational. This algorithm promotes the values to the `Long` type
	  * for the calculation process to avoid temporary arithmetic overflow resulting from `Int` multiplication. The
	  * overflow in the result can still occur however if the numerator and denominator after reduction by their ''GCD''
	  * doesn't fit in the `Int` type.
	  */
	def *(other :Ratio) :Ratio = {
		val num = n * other.numerator.toLong
		val den = d * other.denominator.toLong
		val gcd = naturalGCD(if (num < 0) -num else num, den)
		val reducedNum = num / gcd; val resNum = reducedNum.toInt
		val reducedDen = den / gcd; val resDen = reducedDen.toInt
		if (-reducedNum != -resNum || -reducedDen != -resDen) //checks both for overflow and Int.MinValue
			throw new ArithmeticException(s"Ratio multiplication overflow: $reducedNum/$reducedDen")
		new Ratio((num / gcd).toInt, (den / gcd).toInt)
	}

	/** Multiplies this rational by another rational. This is the same algorithm as
	  * [[net.noresttherein.slang.numeric.Ratio.\* *]], but it is right associative and can promote `Int` values
	  * on the left side of the operator via an implicit conversion.
	  */
	@inline def *:(other :Ratio) :Ratio = other * this


	/** Divides this rational number by another rational. This algorithm promotes the values to the `Long` type
	  * for the calculation process to avoid temporary arithmetic overflow resulting from `Int` multiplication. The
	  * overflow in the result can still occur however if the numerator and denominator after reduction by their ''GCD''
	  * doesn't fit in the `Int` type.
	  */
	@inline def /(other :Ratio) :Ratio = Ratio(n * other.denominator.toLong, d * other.numerator.toLong)

	/** Divides the rational given as the argument by this rational. This is exactly equivalent to `other / this`,
	  * but, being right-associative, can promote `Int` values on the left side of the operator via an implicit conversion.
	  */
	@inline def /:(other :Ratio) :Ratio = Ratio(other.numerator.toLong * d, other.denominator.toLong * n)


	def <(other :Ratio) :Boolean = {
		val n2 = other.numerator; val d2 = other.denominator
		if (n < 0)
			n2 >= 0 || { val gcd = naturalGCD(d, d2).toLong; d2 / gcd * n < d / gcd * n2 }
		else
			n2 > 0 && { val gcd = naturalGCD(d, d2).toLong; d2 / gcd * n < d / gcd * n2 }
	}

	def <=(other :Ratio) :Boolean = {
		val n2 = other.numerator; val d2 = other.denominator
		if (n <= 0)
			n2 >= 0 || { val gcd = naturalGCD(d, d2).toLong; d2 / gcd * n <= d / gcd * n2 }
		else
			n2 > 0 && { val gcd = naturalGCD(d, d2).toLong; d2 / gcd * n <= d / gcd * n2 }
	}

	@inline def >(other :Ratio) :Boolean = !(this <= other)

	@inline def >=(other :Ratio) :Boolean = !(this < other)



	@inline def min(other :Ratio) :Ratio = if (this <= other) this else other

	@inline def max(other :Ratio) :Ratio = if (this < other) other else this


	@inline def toInt :Int = n / d

	@inline def toFloat :Float = n.toFloat / d.toFloat

	@inline def toDouble :Double = n.toDouble / d.toDouble

	@inline def toShort :Short = (n / d).toShort

	@inline def toLong :Long = (n / d).toLong

	@inline def toLongRatio :LongRatio = new LongRatio(n, d)


	@inline override def intValue :Int = n / d

	@inline override def floatValue :Float = n.toFloat / d.toFloat

	@inline override def doubleValue :Double = n.toDouble / d.toDouble

	@inline override def shortValue :Short = (n / d).toShort

	@inline override def longValue :Long = (n / d).toLong



	override def equals(that :Any) :Boolean = that match {
		case r :Ratio => n == r.numerator & d == r.denominator
		case _ => false
	}

	override def hashCode :Int = d * 31 + n

	override def toString :String = n.toString + "/" + d
}




/** Companion object for the `Ratio` class representing rational numbers as fractions in their canonical form.
  * Contains an implicit conversion promoting `Int`s to `Ratio`s when given as the parameter to `Ratio`'s methods.
  *
  */
object Ratio {

	/** Number zero as a `Ratio` value: `0/1`. */
	final val Zero :Ratio = new Ratio(0, 1)

	/** Number one as a `Ratio` value: `1/1`. */
	final val One :Ratio = new Ratio(1, 1)

	/** Maximum value held by the `Ratio` type: `Int.MaxValue / 1`. */
	final val MaxValue = new Ratio(Int.MaxValue, 1)

	/** Minimum value held by the `Ratio` type: `-Int.MaxValue / 1`.
	  * Note that this is actually larger by one than `Int.MinValue`, as the `Int.MinValue/1` is not a valid `Ratio`.
	  * This type checks for arithmetic overflows and underflows, and it ensures that unary `-` and `abs` operations
	  * will be properly defined and never throw any exception. In addition, it reduces the number of checks required
	  * to ensure validity in the constructor and arithmetic operations.
	  */
	final val MinValue = new Ratio(Int.MinValue+1, 1)

	/** The smallest positive value held by this type: `1/Int.MaxValue`. As `-Precision` is also the largest negative
	  * `Ratio` value, it is the smallest possible non-zero difference between two values.
	  */
	final val Precision = new Ratio(1, Int.MaxValue)



	/** Creates a unit fraction, that is one with `1` as the numerator and the given denominator. Note that passing
	  * a negative value will result in a negative `Ratio` with `-1` as the numerator and `denominator.abs` as the denominator.
	  * @param denominator requested denominator of the fraction.
	  * @return Ratio equal to `1 / denominator`
	  * @throws ArithmeticException if denominator equals zero.
	  * @throws IllegalArgumentException if denominator equals `Int.MinValue`.
	  */
	def unit(denominator :Int) :Ratio =
		if (denominator > 0)
			new Ratio(1, denominator)
		else if (denominator < 0)
			if (denominator == Int.MinValue)
				throw new IllegalArgumentException("Can't represent 1/Int.MinValue as a Ratio")
			else
				new Ratio(-1, -denominator)
		else
			throw new ArithmeticException("Ratio 1/0")



	/** Creates a whole rational (a fraction with `1` as the denominator and the argument as the numerator).
	  * @return a Ratio `integer/1`.
	  * @throws IllegalArgumentException if integer equals Int.MinValue.
	  */
	def apply(integer :Int) :Ratio =
		if (integer == Int.MinValue)
			throw new IllegalArgumentException("Can't represent Int.MinValue as a Ratio")
		else
	        new Ratio(integer, 1)



	/** Creates a rational number in the canonical form.
	  * @param numerator the dividend (the numerator of the fraction)
	  * @param denominator the divisor (the denominator of the fraction)
	  * @throws ArithmeticException if denominator is zero or one of the arguments is `Int.MinValue` and result would overflow.
	  * @return a `Ratio` representing the fraction `numerator / denominator` after dividing them by their greatest
	  *         common divisor and adjusting the signums so that denominator is positive.
	  */
	def apply(numerator :Int, denominator :Int) :Ratio =
		if (denominator > 0) {
			val divisor =
				if (numerator >= 0)
					naturalGCD(numerator, denominator)
				else if (numerator == Int.MinValue) {
					val d = naturalGCD(-numerator.toLong, denominator.toLong)
					if (d == 1)
						throw new ArithmeticException(s"Can't represent $numerator/$denominator as a Ratio: no representation of -Int.MinValue")
					d.toInt
				} else
					naturalGCD(-numerator, denominator)
			new Ratio(numerator / divisor, denominator / divisor)
		} else if (denominator < 0) {
			if (denominator == Int.MinValue || numerator == Int.MinValue) {
				val divisor = naturalGCD(if (numerator >= 0) numerator.toLong else -numerator.toLong, -denominator.toLong)
				if (divisor == 1)
					throw new ArithmeticException(s"Can't represent $numerator/$denominator as a Ratio: no representation of -Int.MinValue")
				new Ratio((-(numerator / divisor)).toInt, (-(denominator / divisor)).toInt)
			} else {
				val divisor = naturalGCD(if (numerator > 0) numerator else -numerator, -denominator)
				new Ratio(-(numerator / divisor), -(denominator / divisor))
			}
		} else
			throw new ArithmeticException("Ratio: division by zero")



	/** Creates a rational number in the canonical form. Returned [[net.noresttherein.slang.numeric.Ratio Ratio]] object
	  * still uses `Int`s as the numerator and denominator types, but accepting `Long` arguments here gives a chance
	  * of avoiding overflow in arithmetic operations if the values after division by their ''GCD'' fit into `Int`s.
	  * @param numerator the dividend (the numerator of the fraction)
	  * @param denominator the divisor (the denominator of the fraction)
	  * @throws ArithmeticException if denominator is zero or arithmetic overflow/underflow occurs during reduction.
	  * @return a `Ratio` representing the fraction `numerator / denominator` after dividing them by their greatest
	  *         common divisor and adjusting the signums so that denominator is positive.
	  */
	def apply(numerator :Long, denominator :Long) :Ratio =
		if (denominator > 0) {
			if (numerator >= 0) {
				val divisor = naturalGCD(numerator, denominator)
				val n = numerator / divisor; val d = denominator / divisor
				if (n > Int.MaxValue || d > Int.MaxValue)
					throw new ArithmeticException(s"Ratio overflow: $n/$d ($numerator/$denominator)")
				new Ratio(n.toInt, d.toInt)
			} else if (numerator == Long.MinValue)
				throw new IllegalArgumentException(s"Ratio overflow: $numerator/$denominator")
			else {
				val divisor = naturalGCD(-numerator, denominator)
				val n = numerator / divisor
				val d = denominator / divisor
				if (n <= Int.MinValue || d > Int.MaxValue)
					throw new ArithmeticException(s"Ratio overflow: $n/$d ($numerator/$denominator)")
				new Ratio(n.toInt, d.toInt)
			}
		} else if (denominator < 0) {
			if (denominator == Long.MinValue)
				throw new IllegalArgumentException(s"Ratio overflow: $numerator/$denominator")
			else if (numerator >= 0) {
				val divisor = naturalGCD(numerator, -denominator)
				val n = numerator / divisor; val d = denominator / divisor
				if (n > Int.MaxValue || d <= Int.MinValue)
					throw new ArithmeticException(s"Ratio overflow: $n/$d ($numerator/$denominator)")
				new Ratio(-n.toInt, -d.toInt)
			} else if (numerator == Long.MinValue)
		       throw new IllegalArgumentException(s"Ratio overflow: $numerator/$denominator")
			else {
				val divisor = naturalGCD(-numerator, -denominator)
				val n = numerator / divisor
				val d = denominator / divisor
				if (n <= Int.MinValue || d <= Int.MinValue)
					throw new ArithmeticException(s"Ratio overflow: $n/$d ($numerator/$denominator)")
				new Ratio(-n.toInt, -d.toInt)
			}
		} else
			  throw new ArithmeticException("Ratio: division by zero")


	/** Implicit promotion of integers to rational. Will be automatically performed without importing when an `Int`
	  * is passed to any method of a `Ratio` object.
	  */
	@inline implicit def intToRatio(int :Int) :Ratio = apply(int)



	/** The `Fractional` type class for `Ratio` values. */
	implicit object RatioIsFractional extends Fractional[Ratio] {
		override def div(x :Ratio, y :Ratio) :Ratio = x / y

		override def plus(x :Ratio, y :Ratio) :Ratio = x + y

		override def minus(x :Ratio, y :Ratio) :Ratio = x - y

		override def times(x :Ratio, y :Ratio) :Ratio = x * y

		override def negate(x :Ratio) :Ratio = -x

		override def fromInt(x :Int) :Ratio = x

		override def toInt(x :Ratio) :Int = x.toInt

		override def toLong(x :Ratio) :Long = x.toLong

		override def toFloat(x :Ratio) :Float = x.toFloat

		override def toDouble(x :Ratio) :Double = x.toDouble



		override def compare(x :Ratio, y :Ratio) :Int =
			if (x < y) -1 else if (y < x) 1 else 0


		override def parseString(str :String) :Option[Ratio] = str.toIntOption.map(Ratio.apply)
	}


	/** Implicit conversion extending `Int` values with a `/%` method accepting other another integer and
	  * constructing a [[net.noresttherein.slang.numeric.Ratio Ratio]] instance as an alternative
	  * to the `Ratio` object's factory method. If you wish to perform other arithmetic operations with `Int` values
	  * as the left-hand argument use the appropriate right-associative method of the `Ratio` class.
	  * @param numerator this integer, serving as thee numerator of the future rational
	  * @return a builder object accepting the denominator for the rational result.
	  */
	@inline implicit def /%(numerator :Int) :DivisionRatioConstructor = new DivisionRatioConstructor(numerator)

	/** A builder of [[net.noresttherein.slang.numeric.Ratio Ratio]] objects, accepting an `Int` denominator
	  * and constructing the rational number representing the division of the wrapped numerator values by the argument.
	  * @param numerator the numerator of created rational numbers (before reduction)
	  */
	class DivisionRatioConstructor(private val numerator :Int) extends AnyVal {
		/** Divides this `Int` by the argument, creating a [[net.noresttherein.slang.numeric.Ratio Ratio]] number
		  * representing the result. @param denominator the denominator of the created rational (before reduction)
		  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
		  */
		@inline def /%(denominator :Int) :Ratio = Ratio(numerator, denominator)
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
