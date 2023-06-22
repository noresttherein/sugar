package net.noresttherein.sugar.numeric

import java.math.{BigInteger, BigDecimal => JavaBigDecimal}
import java.{lang => jl}

import scala.collection.immutable.NumericRange
import scala.math.ScalaNumericAnyConversions




/** A `Long` value checking for overflows, underflows and loss of precision in all operations.
  * Unless stated otherwise, all mutating methods, as well as those promoting `Long` to `Float` or `Double`,
  * can throw an [[ArithmeticException]] with the exception of bitwise operations.
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
class SafeLong private[numeric] (override val toLong :Long)
	extends AnyVal with Ordered[SafeLong] with ScalaNumericAnyConversions with Serializable
{
	@inline override def isWhole     : Boolean = true
	@inline override def isValidByte : Boolean = (toLong & 0xffL) == toLong
	@inline override def isValidShort: Boolean = (toLong & 0xffffL) == toLong
	@inline override def isValidChar : Boolean = (toLong & 0xffffL) == toLong
	@inline override def isValidInt  : Boolean = (toLong & 0xffffffffL) == toLong

	/** Interprets the lowest byte of this number as a `Byte`.
	  * This will silently truncate values outside of the `Byte` range.
	  */
	@inline override def byteValue  : Byte   = toLong.toByte

	/** Interprets the lowest two bytes of this number as a `Short`.
	  * This will silently truncate values outside of the `Short` range.
	  */
	@inline override def shortValue : Short  = toLong.toShort

	@inline override def intValue   : Int    = toLong.toInt
	@inline override def longValue  : Long   = toLong
	@inline override def floatValue : Float  = toLong.toFloat
	@inline override def doubleValue: Double = toLong.toDouble
	@inline def charValue: Char = toLong.toChar

	@inline override def toByte  : Byte   = { testRange(Byte.MinValue, Byte.MaxValue, "Byte"); toLong.toByte }
	@inline override def toShort : Short  = {
		testRange(Short.MinValue, Short.MaxValue, "Short"); toLong.toShort
	}
	@inline override def toChar  : Char   = {
		testRange(Char.MinValue, Char.MaxValue, "Char"); toLong.toChar
	}
	@inline override def toInt   : Int    = { testRange(Int.MinValue, Int.MaxValue, "Int"); toLong.toInt }
	@inline override def toFloat : Float  = {
		val res = toLong.toFloat; if (res.toLong != toLong) outOfPrecision("Float"); res
	}
	@inline override def toDouble: Double = {
		val res = toLong.toDouble; if (res.toLong != toLong) outOfPrecision("Double"); res
	}
	@inline def toBoolean        : Boolean = toLong != 0L
	@inline def toSafeInt        : SafeInt = {
		testRange(Int.MinValue, Int.MaxValue, "Int"); new SafeInt(toLong.toInt)
	}
	@inline def toUInt            : UInt = {
		testRange(0L, (1L << 32) - 1L, "UInt"); new UInt(toLong.toInt)
	}
	@inline def toULong           : ULong = ULong.from(toLong)
	@inline def toDecimal64       : Decimal64 = Decimal64(toLong)

	@inline def toBigInt        : BigInt         = BigInt(toLong)
	@inline def toBigInteger    : BigInteger     = BigInteger.valueOf(toLong)
	@inline def toBigDecimal    : BigDecimal     = BigDecimal(toLong)
	@inline def toJavaBigDecimal: JavaBigDecimal = JavaBigDecimal.valueOf(toLong)

	@inline override def toString   : String = String.valueOf(toLong)
	@inline def toString(radix :Int): String = jl.Long.toString(toLong, radix)
	@inline def toBinaryString      : String = jl.Long.toBinaryString(toLong)
	@inline def toOctalString       : String = jl.Long.toOctalString(toLong)
	@inline def toHexString         : String = jl.Long.toHexString(toLong)

	@inline def sign    : Long = (toLong >> 63) | (-toLong >>> 63)
	@inline def abs     : SafeLong = {
		if (toLong == Long.MinValue) overflow("-Long.MinValue"); new SafeLong(-toLong)
	}
	@inline def unary_~ : SafeLong = new SafeLong(~toLong)
	@inline def unary_+ : SafeLong = this
	@inline def unary_- : SafeLong = {
		if (toLong == Long.MinValue) overflow("-Long.MinValue"); new SafeLong(-toLong)
	}

	@deprecated("Adding a number and a String is deprecated. Use the string interpolation `s\"$num$str\"`", "2.13.0")
	@inline def +(x: String): String = toLong.toString + x

	@inline def <<(x: Int)  : SafeLong = new SafeLong(toLong << x)
	@inline def <<(x: Long) : SafeLong = new SafeLong(toLong << x)
	@inline def >>>(x: Int) : SafeLong = new SafeLong(toLong >>> x)
	@inline def >>>(x: Long): SafeLong = new SafeLong(toLong >>> x)
	@inline def >>(x: Int)  : SafeLong = new SafeLong(toLong >> x)
	@inline def >>(x: Long) : SafeLong = new SafeLong(toLong >> x)

	@inline def ==(x: Byte)  : Boolean = toLong == x
	@inline def ==(x: Short) : Boolean = toLong == x
	@inline def ==(x: Char)  : Boolean = toLong == x
	@inline def ==(x: Int)   : Boolean = toLong == x
	@inline def ==(x: Long)  : Boolean = toLong == x
	@inline def ==(x: Float) : Boolean = toLong == x
	@inline def ==(x: Double): Boolean = toLong == x
	@inline def !=(x: Byte)  : Boolean = toLong != x
	@inline def !=(x: Short) : Boolean = toLong != x
	@inline def !=(x: Char)  : Boolean = toLong != x
	@inline def !=(x: Int)   : Boolean = toLong != x
	@inline def !=(x: Long)  : Boolean = toLong != x
	@inline def !=(x: Float) : Boolean = toLong != x
	@inline def !=(x: Double): Boolean = toLong != x

	@inline def < (x: Byte)  : Boolean = toLong < x
	@inline def < (x: Short) : Boolean = toLong < x
	@inline def < (x: Char)  : Boolean = toLong < x
	@inline def < (x: Int)   : Boolean = toLong < x
	@inline def < (x: Long)  : Boolean = toLong < x
	@inline def < (x: Float) : Boolean = toFloat < x
	@inline def < (x: Double): Boolean = toDouble < x
	@inline def <=(x: Byte)  : Boolean = toLong <= x
	@inline def <=(x: Short) : Boolean = toLong <= x
	@inline def <=(x: Char)  : Boolean = toLong <= x
	@inline def <=(x: Int)   : Boolean = toLong <= x
	@inline def <=(x: Long)  : Boolean = toLong <= x
	@inline def <=(x: Float) : Boolean = toFloat <= x
	@inline def <=(x: Double): Boolean = toDouble <= x
	@inline def > (x: Byte)  : Boolean = toLong > x
	@inline def > (x: Short) : Boolean = toLong > x
	@inline def > (x: Char)  : Boolean = toLong > x
	@inline def > (x: Int)   : Boolean = toLong > x
	@inline def > (x: Long)  : Boolean = toLong > x
	@inline def > (x: Float) : Boolean = toFloat > x
	@inline def > (x: Double): Boolean = toDouble > x
	@inline def >=(x: Byte)  : Boolean = toLong >= x
	@inline def >=(x: Short) : Boolean = toLong >= x
	@inline def >=(x: Char)  : Boolean = toLong >= x
	@inline def >=(x: Int)   : Boolean = toLong >= x
	@inline def >=(x: Long)  : Boolean = toLong >= x
	@inline def >=(x: Float) : Boolean = toFloat >= x
	@inline def >=(x: Double): Boolean = toDouble >= x

	@inline def compare(x :SafeLong) :Int = jl.Long.compare(toLong, x.toLong)

	@inline def min(other :SafeLong) :SafeLong = new SafeLong(jl.Math.min(toLong, other.toLong))
	@inline def max(other :SafeLong) :SafeLong = new SafeLong(jl.Math.max(toLong, other.toLong))

	/** Returns `this max other`. */
	@inline def atLeast(other :ULong) :SafeLong = new SafeLong(jl.Math.max(toLong, other.toLong))

	/** Returns `this min other`. */
	@inline def atMost(other :ULong) :SafeLong = new SafeLong(jl.Math.min(toLong, other.toLong))

	/** Returns this `Long`, or `0` if the condition is false. */
	@inline def orZeroIf(condition :Boolean) :SafeLong = if (condition) new SafeLong(0) else this

	/** Returns this `Long`, or `0` if it does not satisfy the predicate. */
	@inline def orZeroIf(condition :SafeLong => Boolean) :SafeLong =
		if (condition(this)) new SafeLong(0) else this

	@inline def |(x: Byte) : SafeLong = new SafeLong(toLong | x)
	@inline def |(x: Short): SafeLong = new SafeLong(toLong | x)
	@inline def |(x: Char) : SafeLong = new SafeLong(toLong | x)
	@inline def |(x: Int)  : SafeLong = new SafeLong(toLong | x)
	@inline def |(x: Long) : SafeLong = new SafeLong(toLong | x)
	@inline def &(x: Byte) : SafeLong = new SafeLong(toLong & x)
	@inline def &(x: Short): SafeLong = new SafeLong(toLong & x)
	@inline def &(x: Char) : SafeLong = new SafeLong(toLong & x)
	@inline def &(x: Int)  : SafeLong = new SafeLong(toLong & x)
	@inline def &(x: Long) : SafeLong = new SafeLong(toLong & x)
	@inline def ^(x: Byte) : SafeLong = new SafeLong(toLong ^ x)
	@inline def ^(x: Short): SafeLong = new SafeLong(toLong ^ x)
	@inline def ^(x: Char) : SafeLong = new SafeLong(toLong ^ x)
	@inline def ^(x: Int)  : SafeLong = new SafeLong(toLong ^ x)
	@inline def ^(x: Long) : SafeLong = new SafeLong(toLong ^ x)

	@inline def +(x: Byte)  : SafeLong = new SafeLong(Math.addExact(toLong, x))
	@inline def +(x: Short) : SafeLong = new SafeLong(Math.addExact(toLong, x))
	@inline def +(x: Char)  : SafeLong = new SafeLong(Math.addExact(toLong, x))
	@inline def +(x: Int)   : SafeLong = new SafeLong(Math.addExact(toLong, x))
	@inline def +(x: Long)  : SafeLong = new SafeLong(Math.addExact(toLong, x))
	@inline def +(x: Float) : Float    = checkFloatResult(x, "+", toFloat + x)
	@inline def +(x: Double): Double   = checkDoubleResult(x, "+", toDouble + x)
	@inline def -(x: Byte)  : SafeLong = new SafeLong(Math.subtractExact(toLong, x))
	@inline def -(x: Short) : SafeLong = new SafeLong(Math.subtractExact(toLong, x))
	@inline def -(x: Char)  : SafeLong = new SafeLong(Math.subtractExact(toLong, x))
	@inline def -(x: Int)   : SafeLong = new SafeLong(Math.subtractExact(toLong, x))
	@inline def -(x: Long)  : SafeLong = new SafeLong(Math.subtractExact(toLong, x))
	@inline def -(x: Float) : Float    = checkFloatResult(x, "-", toFloat - x)
	@inline def -(x: Double): Double   = checkDoubleResult(x, "-", toDouble - x)
	@inline def *(x: Byte)  : SafeLong = new SafeLong(Math.multiplyExact(toLong, x))
	@inline def *(x: Short) : SafeLong = new SafeLong(Math.multiplyExact(toLong, x))
	@inline def *(x: Char)  : SafeLong = new SafeLong(Math.multiplyExact(toLong, x))
	@inline def *(x: Int)   : SafeLong = new SafeLong(Math.multiplyExact(toLong, x))
	@inline def *(x: Long)  : SafeLong = new SafeLong(Math.multiplyExact(toLong, x))
	@inline def *(x: Float) : Float    = checkFloatResult(x, "*", toFloat * x)
	@inline def *(x: Double): Double   = checkDoubleResult(x, "*", toDouble * x)
	@inline def /(x: Byte)  : SafeLong = this / x.toLong
	@inline def /(x: Short) : SafeLong = this / x.toLong
	@inline def /(x: Char)  : SafeLong = this / x.toLong
	@inline def /(x: Int)   : SafeLong = this / x.toLong
	@inline def /(x: Long)  : SafeLong =
		if (toLong == Long.MinValue & x == -1) overflow(String.valueOf(toLong) + " / " + x)
		else new SafeLong(toLong / x)
	@inline def /(x: Float) : Float    = checkFloatResult(x, "/", toFloat / x)
	@inline def /(x: Double): Double   = checkDoubleResult(x, "/", toDouble / x)
	@inline def %(x :Byte)  : SafeLong = new SafeLong(toLong % x)
	@inline def %(x: Short) : SafeLong = new SafeLong(toLong % x)
	@inline def %(x: Char)  : SafeLong = new SafeLong(toLong % x)
	@inline def %(x: Int)   : SafeLong = new SafeLong(toLong % x)
	@inline def %(x: Long)  : SafeLong = new SafeLong(toLong % x)
	@inline def %(x: Float) : Float    = checkFloatResult(x, "%", toFloat % x)
	@inline def %(x: Double): Double   = checkDoubleResult(x, "%", toDouble % x)
	@inline def **(n :Int)  : SafeLong = pow(n)

	/** Raises this `SafeLong` to the given power. If the argument is negative, `0` is returned. */
	def pow(exp :Int): SafeLong =
		if (exp < 0)
			new SafeLong(0L)
		else {
			var res = new SafeLong(1L)
			var i   = 32 - jl.Integer.numberOfLeadingZeros(exp)  //< 32 because exp >= 0
			while (i >= 0) {
				res = res * res
				if ((exp >> i & 1) == 1)
					res = res * this
				i -= 1
			}
			res
		}

	/** Divides this `SafeLong` by the argument, creating a [[net.noresttherein.sugar.numeric.Ratio Ratio]]
	  * number representing the result.
	  * @param denominator the denominator of the created rational (before reduction)
	  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
	  */
	@inline def %/(denominator :Long) :Ratio = Ratio(toInt, denominator)

	type ResultWithoutStep = NumericRange[SafeLong]
	@inline def to(end :SafeLong) :NumericRange.Inclusive[SafeLong] =
		NumericRange.inclusive(this, end, new SafeLong(1))

	@inline def to(end :SafeLong, step :SafeLong) :NumericRange.Inclusive[SafeLong] =
		NumericRange.inclusive(this, end, step)

	@inline def until(end :SafeLong) :NumericRange.Exclusive[SafeLong] = NumericRange(this, end, new SafeLong(1))
	@inline def until(end :SafeLong, step :SafeLong) :NumericRange.Exclusive[SafeLong] =
		NumericRange(this, end, step)

	@inline def in(range: NumericRange[SafeLong]): Boolean = range.containsTyped(this)

	@inline private def overflow(msg :String) :Nothing =
		throw new ArithmeticException("Arithmetic overflow: " + msg + ".")

	@inline private def underflow(msg :String) :Nothing =
		throw new ArithmeticException("Arithmetic overflow: " + msg + ".")

	@inline private def checkFloatResult(arg :Float, op :String, result :Float) :Float = {
		if (result != result | result == jl.Float.POSITIVE_INFINITY | result == jl.Float.NEGATIVE_INFINITY)
			if (result == jl.Float.POSITIVE_INFINITY)
				overflow(String.valueOf(toLong) + " " + op + " " + arg + " is Float.POSITIVE_INFINITY.")
			else if (result == jl.Float.NEGATIVE_INFINITY)
				underflow(String.valueOf(toLong) + " " + op + " " + arg + " is Float.NEGATIVE_INFINITY.")
			else
				throw new ArithmeticException(String.valueOf(toLong) + " " + op + " " + arg + " == NaN")
		result
	}

	@inline private def checkDoubleResult(arg :Double, op :String, result :Double) :Double = {
		if (result != result | result == jl.Double.POSITIVE_INFINITY | result == jl.Double.NEGATIVE_INFINITY)
			if (result == jl.Double.POSITIVE_INFINITY)
				overflow(String.valueOf(toLong) + " " + op + " " + arg + " is Double.POSITIVE_INFINITY.")
			else if (result == jl.Double.NEGATIVE_INFINITY)
				underflow(String.valueOf(toLong) + " " + op + " " + arg + " is Double.NEGATIVE_INFINITY.")
			else
				throw new ArithmeticException(String.valueOf(toLong) + " " + op + " " + arg + " == NaN")
		result
	}

	private[numeric] def outOfPrecision(typeName :String) :Nothing =
		throw new ArithmeticException("Value " + toLong + " cannot be exactly represented as a " + typeName + ".")

	private[numeric] def outOfRange(typeName :String) :Nothing =
			throw new ArithmeticException("Value " + toLong + " does not fit in a " + typeName + ".")

	@inline private[numeric] def testRange(min :Long, max :Long, typeName :String) :Unit =
		if (toLong < min | toLong > max)
			outOfRange(typeName)
}




@SerialVersionUID(Ver)
object SafeLong {
	@inline def apply(value :Long) :SafeLong = new SafeLong(value)

	@inline def apply(string :String, radix :Int = 10) :SafeLong =
		new SafeLong(jl.Long.parseLong(string, radix))

	@inline def decode(string :String) :SafeLong =
		new SafeLong(jl.Long.decode(string))

	@inline def parse(string :String) :Option[SafeLong] =
		Numeric.IntIsIntegral.parseString(string).map(new SafeLong(_))

	@inline implicit def safeLongFromByte(value :Byte) :SafeLong = new SafeLong(value)
	@inline implicit def safeLongFromShort(value :Short) :SafeLong = new SafeLong(value)
	@inline implicit def safeLongFromInt(value :Int) :SafeLong = new SafeLong(value)
	@inline implicit def safeLongFromLong(value :Long) :SafeLong = new SafeLong(value)
	@inline implicit def safeLongToLong(value :SafeLong) :Long = value.toLong

	sealed abstract class SafeLongIsNumeric extends Numeric[SafeLong] {
		override def plus(x :SafeLong, y :SafeLong) :SafeLong = x + y
		override def minus(x :SafeLong, y :SafeLong) :SafeLong = x - y
		override def times(x :SafeLong, y :SafeLong) :SafeLong = x * y
		override def negate(x :SafeLong) :SafeLong = -x
		override def fromInt(x :Int) :SafeLong = new SafeLong(x)
		override def parseString(str :String) :Option[SafeLong] = SafeLong.parse(str)
		override def toInt(x :SafeLong) :Int = x.toInt
		override def toLong(x :SafeLong) :Long = x.toLong
		override def toFloat(x :SafeLong) :Float = x.toFloat
		override def toDouble(x :SafeLong) :Double = x.toDouble
		override def compare(x :SafeLong, y :SafeLong) :Int = x compare y
	}
	@SerialVersionUID(Ver)
	implicit object SafeLongIsIntegral extends SafeLongIsNumeric with Integral[SafeLong] {
		override def quot(x :SafeLong, y :SafeLong) :SafeLong = x / y
		override def rem(x :SafeLong, y :SafeLong) :SafeLong = x % y
	}
	@SerialVersionUID(Ver)
	object SafeLongAsFractional extends SafeLongIsNumeric with Fractional[SafeLong] {
		override def div(x :SafeLong, y :SafeLong) :SafeLong = x / y
	}

}

