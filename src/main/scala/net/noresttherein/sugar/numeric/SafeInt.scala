package net.noresttherein.sugar.numeric

import java.math.{BigInteger, BigDecimal => JavaBigDecimal}
import java.{lang => jl}

import scala.collection.immutable.NumericRange
import scala.math.ScalaNumericAnyConversions

import net.noresttherein.sugar.exceptions.SugaredArithmeticException




/** An `Int` value checking for overflows, underflows and loss of precision in all operations.
  * Unless stated otherwise, all methods returning another number type, with the exception of bitwise operations,
  * can throw an [[ArithmeticException]].
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
class SafeInt private[numeric] (override val toInt :Int)
	extends AnyVal with Ordered[SafeInt] with ScalaNumericAnyConversions with Serializable
{
	@inline override def isWhole     : Boolean = true
	@inline override def isValidByte : Boolean = (toInt & 0xff) == toInt
	@inline override def isValidShort: Boolean = (toInt & 0xffff) == toInt
	@inline override def isValidChar : Boolean = (toInt & 0xffff) == toInt
	@inline override def isValidInt  : Boolean = true

	/** Interprets the lowest byte of this number as a `Byte`.
	  * This will silently truncate values outside of the `Byte` range.
	  */
	@inline override def byteValue  : Byte   = toInt.toByte

	/** Interprets the lowest two bytes of this number as a `Short`.
	  * This will silently truncate values outside of the `Short` range.
	  */
	@inline override def shortValue : Short  = toInt.toShort
	@inline override def intValue   : Int    = toInt
	@inline override def longValue  : Long   = toInt.toLong
	@inline override def floatValue : Float  = toInt.toFloat
	@inline override def doubleValue: Double = toInt.toDouble
	@inline def charValue: Char = toInt.toChar

	@inline override def toByte  : Byte   = { testRange(Byte.MinValue, Byte.MaxValue, "Byte"); toInt.toByte }
	@inline override def toShort : Short  = {
		testRange(Short.MinValue, Short.MaxValue, "Short"); toInt.toShort
	}
	@inline override def toChar  : Char   = { testRange(Char.MinValue, Char.MaxValue, "Char"); toInt.toChar }
	@inline override def toLong  : Long   = toInt.toLong
	@inline override def toFloat : Float  = {
		val res = toInt.toFloat; if (res.toInt != toInt) outOfPrecision("Float"); res
	}
	@inline override def toDouble: Double = {
		val res = toInt.toDouble; if (res.toInt != toInt) outOfPrecision("Double"); res
	}
	@inline def toBoolean        : Boolean   = toInt != 0
	@inline def toSafeLong       : SafeLong  = new SafeLong(toInt)
	@inline def toUInt           : UInt      = UInt(toInt)
	@inline def toULong          : ULong     = ULong(toInt)
	@inline def toDecimal64      : Decimal64 = Decimal64(toInt)

	@inline def toBigInt        : BigInt         = BigInt(toInt)
	@inline def toBigInteger    : BigInteger     = BigInteger.valueOf(toInt)
	@inline def toBigDecimal    : BigDecimal     = BigDecimal(toInt)
	@inline def toJavaBigDecimal: JavaBigDecimal = JavaBigDecimal.valueOf(toInt)

	@inline override def toString   : String = String.valueOf(toInt)
	@inline def toString(radix :Int): String = jl.Integer.toString(toInt, radix)
	@inline def toBinaryString      : String = jl.Integer.toBinaryString(toInt)
	@inline def toOctalString       : String = jl.Integer.toOctalString(toInt)
	@inline def toHexString         : String = jl.Integer.toHexString(toInt)

	@inline def sign    : Int = (toInt >> 31) | (-toInt >>> 31)
	@inline def abs     : SafeInt = {
		if (toInt == Int.MinValue) overflow("-Int.MinValue"); new SafeInt(-toInt)
	}
	@inline def unary_~ : SafeInt = new SafeInt(~toInt)
	@inline def unary_+ : SafeInt = this
	@inline def unary_- : SafeInt = {
		if (toInt == Int.MinValue) overflow("-Int.MinValue"); new SafeInt(-toInt)
	}

	@deprecated("Adding a number and a String is deprecated. Use the string interpolation `s\"$num$str\"`", "Scala 2.13.0")
	@inline def +(x: String): String = toInt.toString + x

	@inline def <<(x: Int)  : SafeInt = new SafeInt(toInt << x)
	@inline def >>>(x: Int) : SafeInt = new SafeInt(toInt >>> x)
	@inline def >>(x: Int)  : SafeInt = new SafeInt(toInt >> x)

	@inline def ==(x: Byte)  : Boolean = toInt == x
	@inline def ==(x: Short) : Boolean = toInt == x
	@inline def ==(x: Char)  : Boolean = toInt == x
	@inline def ==(x: Int)   : Boolean = toInt == x
	@inline def ==(x: Long)  : Boolean = toInt == x
	@inline def ==(x: Float) : Boolean = toInt == x
	@inline def ==(x: Double): Boolean = toInt == x
	@inline def !=(x: Byte)  : Boolean = toInt != x
	@inline def !=(x: Short) : Boolean = toInt != x
	@inline def !=(x: Char)  : Boolean = toInt != x
	@inline def !=(x: Int)   : Boolean = toInt != x
	@inline def !=(x: Long)  : Boolean = toInt != x
	@inline def !=(x: Float) : Boolean = toInt != x
	@inline def !=(x: Double): Boolean = toInt != x

	@inline def < (x: Byte)  : Boolean = toInt < x
	@inline def < (x: Short) : Boolean = toInt < x
	@inline def < (x: Char)  : Boolean = toInt < x
	@inline def < (x: Int)   : Boolean = toInt < x
	@inline def < (x: Long)  : Boolean = toInt < x
	@inline def < (x: Float) : Boolean = toFloat < x
	@inline def < (x: Double): Boolean = toDouble < x
	@inline def <=(x: Byte)  : Boolean = toInt <= x
	@inline def <=(x: Short) : Boolean = toInt <= x
	@inline def <=(x: Char)  : Boolean = toInt <= x
	@inline def <=(x: Int)   : Boolean = toInt <= x
	@inline def <=(x: Long)  : Boolean = toInt <= x
	@inline def <=(x: Float) : Boolean = toFloat <= x
	@inline def <=(x: Double): Boolean = toDouble <= x
	@inline def > (x: Byte)  : Boolean = toInt > x
	@inline def > (x: Short) : Boolean = toInt > x
	@inline def > (x: Char)  : Boolean = toInt > x
	@inline def > (x: Int)   : Boolean = toInt > x
	@inline def > (x: Long)  : Boolean = toInt > x
	@inline def > (x: Float) : Boolean = toFloat > x
	@inline def > (x: Double): Boolean = toDouble > x
	@inline def >=(x: Byte)  : Boolean = toInt >= x
	@inline def >=(x: Short) : Boolean = toInt >= x
	@inline def >=(x: Char)  : Boolean = toInt >= x
	@inline def >=(x: Int)   : Boolean = toInt >= x
	@inline def >=(x: Long)  : Boolean = toInt >= x
	@inline def >=(x: Float) : Boolean = toFloat >= x
	@inline def >=(x: Double): Boolean = toDouble >= x

	@inline def compare(other :SafeInt) :Int = jl.Integer.compareUnsigned(toInt, other.toInt)

	@inline def min(other :SafeInt) :SafeInt = new SafeInt(jl.Math.min(toInt, other.toInt))
	@inline def max(other :SafeInt) :SafeInt = new SafeInt(jl.Math.max(toInt, other.toInt))

	/** Returns `this max other`. */
	@inline def atLeast(other :UInt) :SafeInt = new SafeInt(jl.Math.max(toInt, other.toInt))

	/** Returns `this min other`. */
	@inline def atMost(other :UInt) :Long = new SafeInt(jl.Math.min(toInt, other.toInt))

	/** Returns this `SafeInt`, or `0` if the condition is false. */
	@inline def orZeroIf(condition :Boolean) :SafeInt = if (condition) new SafeInt(0) else this

	/** Returns this `SafeInt`, or `0` if it does not satisfy the predicate. */
	@inline def orZeroIf(condition :SafeInt => Boolean) :SafeInt =
		if (condition(this)) new SafeInt(0) else this

	@inline def |(x: Byte) : SafeInt  = new SafeInt(toInt | x)
	@inline def |(x: Short): SafeInt  = new SafeInt(toInt | x)
	@inline def |(x: Char) : SafeInt  = new SafeInt(toInt | x)
	@inline def |(x: Int)  : SafeInt  = new SafeInt(toInt | x)
	@inline def |(x: Long) : SafeLong = new SafeLong(toInt | x)
	@inline def &(x: Byte) : SafeInt  = new SafeInt(toInt & x)
	@inline def &(x: Short): SafeInt  = new SafeInt(toInt & x)
	@inline def &(x: Char) : SafeInt  = new SafeInt(toInt & x)
	@inline def &(x: Int)  : SafeInt  = new SafeInt(toInt & x)
	@inline def &(x: Long) : SafeLong = new SafeLong(toInt & x)
	@inline def ^(x: Byte) : SafeInt  = new SafeInt(toInt ^ x)
	@inline def ^(x: Short): SafeInt  = new SafeInt(toInt ^ x)
	@inline def ^(x: Char) : SafeInt  = new SafeInt(toInt ^ x)
	@inline def ^(x: Int)  : SafeInt  = new SafeInt(toInt ^ x)
	@inline def ^(x: Long) : SafeLong = new SafeLong(toInt ^ x)

	@inline def +(x: Byte)  : SafeInt  = new SafeInt(Math.addExact(toInt, x))
	@inline def +(x: Short) : SafeInt  = new SafeInt(Math.addExact(toInt, x))
	@inline def +(x: Char)  : SafeInt  = new SafeInt(Math.addExact(toInt, x))
	@inline def +(x: Int)   : SafeInt  = new SafeInt(Math.addExact(toInt, x))
	@inline def +(x: Long)  : SafeLong = new SafeLong(Math.addExact(toInt, x))
	@inline def +(x: Float) : Float    = checkFloatResult(x, "+", toFloat + x)
	@inline def +(x: Double): Double   = checkDoubleResult(x, "+", toDouble + x)
	@inline def -(x: Byte)  : SafeInt  = new SafeInt(Math.subtractExact(toInt, x))
	@inline def -(x: Short) : SafeInt  = new SafeInt(Math.subtractExact(toInt, x))
	@inline def -(x: Char)  : SafeInt  = new SafeInt(Math.subtractExact(toInt, x))
	@inline def -(x: Int)   : SafeInt  = new SafeInt(Math.subtractExact(toInt, x))
	@inline def -(x: Long)  : SafeLong = new SafeLong(Math.subtractExact(toInt, x))
	@inline def -(x: Float) : Float    = checkFloatResult(x, "-", toFloat - x)
	@inline def -(x: Double): Double   = checkDoubleResult(x, "-", toDouble - x)
	@inline def *(x: Byte)  : SafeInt  = new SafeInt(Math.multiplyExact(toInt, x))
	@inline def *(x: Short) : SafeInt  = new SafeInt(Math.multiplyExact(toInt, x))
	@inline def *(x: Char)  : SafeInt  = new SafeInt(Math.multiplyExact(toInt, x))
	@inline def *(x: Int)   : SafeInt  = new SafeInt(Math.multiplyExact(toInt, x))
	@inline def *(x: Long)  : SafeLong = new SafeLong(Math.multiplyExact(toInt, x))
	@inline def *(x: Float) : Float    = checkFloatResult(x, "*", toFloat * x)
	@inline def *(x: Double): Double   = checkDoubleResult(x, "*", toDouble * x)
	@inline def /(x: Byte)  : SafeInt  = this / x.toInt
	@inline def /(x: Short) : SafeInt  = this / x.toInt
	@inline def /(x: Char)  : SafeInt  = this / x.toInt
	@inline def /(x: Int)   : SafeInt  =
		if (toInt == Int.MinValue & x == -1) overflow(String.valueOf(toInt) + " / " + x)
		else new SafeInt(toInt / x)
	@inline def /(x: Long)  : SafeInt =
		if (toInt == Int.MinValue && x == -1L) overflow(String.valueOf(toInt) + " / " + x)
		else new SafeInt((toInt / x).toInt)
	@inline def /(x: Float) : Float   = checkFloatResult(x, "/", toFloat / x)
	@inline def /(x: Double): Double  = checkDoubleResult(x, "/", toDouble / x)
	@inline def %(x: Byte)  : SafeInt = new SafeInt(toInt % x)
	@inline def %(x: Short) : SafeInt = new SafeInt(toInt % x)
	@inline def %(x: Char)  : SafeInt = new SafeInt(toInt % x)
	@inline def %(x: Int)   : SafeInt = new SafeInt(toInt % x)
	@inline def %(x: Long)  : SafeInt = new SafeInt((toInt % x).toInt)
	@inline def %(x: Float) : Float   = checkFloatResult(x, "%", toFloat % x)
	@inline def %(x: Double): Double  = checkDoubleResult(x, "%", toDouble % x)
	@inline def **(n: Int)  : SafeInt = pow(n)

	/** Raises this `SafeInt` to the given power. If the argument is negative, `0` is returned. */
	def pow(exp :Int) :SafeInt =
		if (exp < 0)
			new SafeInt(0)
		else {
			var res = new SafeInt(1)
			var i   = 32 - jl.Integer.numberOfLeadingZeros(exp) //< 32 because exp >= 0
			while (i >= 0) {
				res = res * res
				if ((exp >> i & 1) == 1)
					res = res * this
				i -= 1
			}
			res
		}

	/** Divides this `SafeInt` by the argument, creating a [[net.noresttherein.sugar.numeric.Ratio Ratio]]
	  * number representing the result.
	  * @param denominator the denominator of the created rational (before reduction)
	  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
	  */
	@inline def %/(denominator :Long) :Ratio = Ratio(toInt, denominator)

	/** Divides this `SafeInt` by the argument, creating a [[net.noresttherein.sugar.numeric.Ratio Ratio]]
	  * number representing the result.
	  * @param denominator the denominator of the created rational (before reduction)
	  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
	  */
	@inline def %/(denominator :Int) :Ratio = Ratio(toInt, denominator)

	type ResultWithoutStep = NumericRange[SafeInt]
	@inline def to(end: SafeInt): NumericRange.Inclusive[SafeInt] =
		NumericRange.inclusive(this, end, new SafeInt(1))(SafeInt.SafeIntIsIntegral)

	@inline def to(end: SafeInt, step: SafeInt): NumericRange.Inclusive[SafeInt] =
		NumericRange.inclusive(this, end, step)(SafeInt.SafeIntIsIntegral)

	@inline def until(end: SafeInt): NumericRange.Exclusive[SafeInt] = NumericRange(this, end, new SafeInt(1))
	@inline def until(end: SafeInt, step: SafeInt): NumericRange.Exclusive[SafeInt] =
		NumericRange(this, end, step)

	@inline def in(range: NumericRange[SafeInt]): Boolean = range.containsTyped(this)


	@inline private def overflow(msg :String) :Nothing =
		throw SugaredArithmeticException("Arithmetic overflow: " + msg + ".")

	@inline private def underflow(msg :String) :Nothing =
		throw SugaredArithmeticException("Arithmetic overflow: " + msg + ".")

	@inline private def checkFloatResult(arg :Float, op :String, result :Float) :Float = {
		if (result != result | result == jl.Float.POSITIVE_INFINITY | result == jl.Float.NEGATIVE_INFINITY)
			if (result == jl.Float.POSITIVE_INFINITY)
				overflow(String.valueOf(toInt) + " " + op + " " + arg + " is Float.POSITIVE_INFINITY.")
			else if (result == jl.Float.NEGATIVE_INFINITY)
				underflow(String.valueOf(toInt) + " " + op + " " + arg + " is Float.NEGATIVE_INFINITY.")
			else
				throw SugaredArithmeticException(String.valueOf(toInt) + " " + op + " " + arg + " == NaN")
		result
	}

	@inline private def checkDoubleResult(arg :Double, op :String, result :Double) :Double = {
		if (result != result | result == jl.Double.POSITIVE_INFINITY | result == jl.Double.NEGATIVE_INFINITY)
			if (result == jl.Double.POSITIVE_INFINITY)
				overflow(String.valueOf(toInt) + " " + op + " " + arg + " is Double.POSITIVE_INFINITY.")
			else if (result == jl.Double.NEGATIVE_INFINITY)
				underflow(String.valueOf(toInt) + " " + op + " " + arg + " is Double.NEGATIVE_INFINITY.")
			else
				throw SugaredArithmeticException(String.valueOf(toInt) + " " + op + " " + arg + " == NaN")
		result
	}

	@inline private[numeric] def outOfPrecision(typeName :String) :Nothing =
		throw SugaredArithmeticException("Value " + toInt + " cannot be exactly represented as a " + typeName + ".")

	private[numeric] def outOfRange(typeName :String) :Nothing =
		throw SugaredArithmeticException("Value " + toInt + " does not fit in a " + typeName + ".")

	@inline private[numeric] def testRange(min :Int, max :Int, typeName :String) :Unit =
		if (toInt < min | toInt > max)
			outOfRange(typeName)
}




@SerialVersionUID(Ver)
object SafeInt {
	@inline def apply(value :Int) :SafeInt = new SafeInt(value)

	@inline def apply(string :String, radix :Int = 10) :SafeInt =
		new SafeInt(jl.Integer.parseInt(string, radix))

	@inline def decode(string :String) :SafeInt =
		new SafeInt(jl.Integer.decode(string))

	@inline def parse(string :String) :Option[SafeInt] =
		Numeric.IntIsIntegral.parseString(string).map(new SafeInt(_))

	@inline implicit def safeIntFromByte(value :Byte) :SafeInt = new SafeInt(value)
	@inline implicit def safeIntFromShort(value :Short) :SafeInt = new SafeInt(value)
	@inline implicit def safeIntFromInt(value :Int) :SafeInt = new SafeInt(value)
	@inline implicit def safeIntToInt(value :SafeInt) :Int = value.toInt
	@inline implicit def safeIntToLong(value :SafeInt) :Long = value.toInt.toLong
	@inline implicit def safeIntToSafeLong(value :SafeInt) :SafeLong = new SafeLong(value.toInt.toLong)

	sealed abstract class SafeIntIsNumeric extends Numeric[SafeInt] {
		override def plus(x :SafeInt, y :SafeInt) :SafeInt = x + y
		override def minus(x :SafeInt, y :SafeInt) :SafeInt = x - y
		override def times(x :SafeInt, y :SafeInt) :SafeInt = x * y
		override def negate(x :SafeInt) :SafeInt = -x
		override def fromInt(x :Int) :SafeInt = new SafeInt(x)
		override def parseString(str :String) :Option[SafeInt] = SafeInt.parse(str)
		override def toInt(x :SafeInt) :Int = x.toInt
		override def toLong(x :SafeInt) :Long = x.toLong
		override def toFloat(x :SafeInt) :Float = x.toFloat
		override def toDouble(x :SafeInt) :Double = x.toDouble
		override def compare(x :SafeInt, y :SafeInt) :Int = x compare y
	}
	@SerialVersionUID(Ver)
	implicit object SafeIntIsIntegral extends SafeIntIsNumeric with Integral[SafeInt] {
		override def quot(x :SafeInt, y :SafeInt) :SafeInt = x / y
		override def rem(x :SafeInt, y :SafeInt) :SafeInt = x % y
	}
	@SerialVersionUID(Ver)
	object SafeIntAsIfFractional extends SafeIntIsNumeric with Fractional[SafeInt] {
		override def div(x :SafeInt, y :SafeInt) :SafeInt = x / y
	}
}
