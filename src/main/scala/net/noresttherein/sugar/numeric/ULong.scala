package net.noresttherein.sugar.numeric

import java.{lang => jl}
import java.lang.Long.{compareUnsigned, divideUnsigned, highestOneBit, parseUnsignedLong, remainderUnsigned, toUnsignedString}
import java.math.{BigInteger, BigDecimal => JavaBigDecimal}

import scala.Long.MinValue
import scala.annotation.tailrec
import scala.collection.immutable.NumericRange
import scala.math.ScalaNumericAnyConversions

import net.noresttherein.sugar.exceptions.{SugaredArithmeticException, SugaredNumberFormatException, illegal_!}
import net.noresttherein.sugar.numeric.ULong.{BigDecimalMaxLongTimes2, BigIntMaxLongTimes2, BigIntegerMaxLongTimes2, Decimal64MaxLongTimes2, DoubleMaxLongTimes2, FloatMaxLongTimes2, JavaBigDecimalMaxLongTimes2}
import net.noresttherein.sugar.numeric.extensions.LongExtension
import net.noresttherein.sugar.typist.CompanionObject
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}
import net.noresttherein.sugar.witness.Ignored




/** An unsigned 64-bit integer backed by a `Long` value. All comparisons are done as if unsigned, and `toString`
  * and other formatting methods treat the underlying `Long` as unsigned.
  * This type doesn't check for overflows and underflows, meaning `ULong(1) - ULong(2)` will return `ULong.MaxValue`.
  *
  * Arithmetic with `Long` is not provided by default, as it is not clear which type should be converted
  * to another. The same applies to implicit conversions between these types.
  * For this purpose, either use `Long` (to which `UInt` is automatically converted), explicitly convert
  * either of the values to the other type, or import one of the conversions
  *   - [[net.noresttherein.sugar.numeric.ULong.conversions.ULongToLong]], or
  *   - [[net.noresttherein.sugar.numeric.ULong.conversions.LongToULong]].
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
class ULong private[numeric] (override val toLong: Long)
	extends AnyVal with ScalaNumericAnyConversions with Serializable
{
	@inline override def isWhole     : Boolean = true
	@inline override def isValidByte : Boolean = (toLong & 0x7fL) == toLong
	@inline override def isValidShort: Boolean = (toLong & 0x7fffL) == toLong
	@inline override def isValidChar : Boolean = (toLong & 0xffffL) == toLong
	@inline override def isValidInt  : Boolean = (toLong & 0x7fffffffL) == toLong
	@inline def isValidLong: Boolean = toLong >= 0

	@inline def charValue: Char = toLong.toChar
	@inline override def byteValue  : Byte    = toLong.toByte
	@inline override def shortValue : Short   = toLong.toShort
	@inline override def intValue   : Int     = toLong.toInt
	@inline override def longValue  : Long    = toLong
	@inline override def floatValue : Float   = if (toLong >= 0) toLong.toFloat else FloatMaxLongTimes2 + toLong.toFloat
	@inline override def doubleValue: Double  =
		if (toLong >= 0) toLong.toDouble
		else DoubleMaxLongTimes2 + toLong.toDouble

	@inline override def toByte  : Byte   = toLong.toByte
	@inline override def toShort : Short  = toLong.toShort
	@inline override def toInt   : Int    = toLong.toInt
	@inline override def toFloat : Float  = if (toLong >= 0) toLong.toFloat else FloatMaxLongTimes2 + toLong.toFloat
	@inline override def toDouble: Double = if (toLong >= 0) toLong.toDouble else DoubleMaxLongTimes2 + toLong.toDouble

	@inline def toByteExact  : Byte   = if ((toLong & 0x7fL) != toLong) outOfRange("Byte") else toLong.toByte
	@inline def toShortExact : Short  = if ((toLong & 0x7fffL) != toLong) outOfRange("Short") else toLong.toShort
	@inline def toCharExact  : Char   = if ((toLong & 0xffffL) != toLong) outOfRange("Char") else toLong.toChar
	@inline def toIntExact   : Int    = if ((toLong & 0x7fffffffL) != toLong) outOfRange("Int") else toLong.toInt
	@inline def toLongExact  : Long   = if (toLong < 0) underflow(".toLong") else toLong
	@inline def toUShortExact: UShort =
		if ((toLong & 0xffffL) != toLong) outOfRange("UShort") else new UShort(toLong.toShort)
	@inline def toUIntExact  : UInt   =
		if ((toLong & 0xffffffffL) != toLong) outOfRange("UInt") else new UInt(toLong.toInt)

	/** Returns `toLong != 0`. */
	@inline def toBoolean: Boolean = toLong != 0L

	@inline def toBigInt: BigInt =
		if (toLong >= 0) BigInt(toLong)
		else BigIntMaxLongTimes2 + BigInt(toLong)

	@inline def toBigInteger: BigInteger =
		if (toLong >= 0) BigInteger.valueOf(toLong)
		else BigIntegerMaxLongTimes2.add(BigInteger.valueOf(toLong))

	@inline def toBigDecimal: BigDecimal =
		if (toLong >= 0) BigDecimal(toLong)
		else BigDecimalMaxLongTimes2 + BigDecimal(toLong)

	@inline def toJavaBigDecimal: JavaBigDecimal =
		if (toLong >= 0) JavaBigDecimal.valueOf(toLong)
		else JavaBigDecimalMaxLongTimes2.add(JavaBigDecimal.valueOf(toLong))

	@inline def toUShort   : UShort    = new UShort(toShort)
	@inline def toUInt     : UInt      = new UInt(toInt)
	@inline def toSafeInt  : SafeInt   = new SafeInt(toIntExact)
	@inline def toSafeLong : SafeLong  = new SafeLong(toInt & 0xffffffffL)
	@inline def toIntRatio : IntRatio  = IntRatio(toInt)
	@inline def toRatio    : Ratio     = Ratio(toInt & 0xffffffffL)
	@inline def toDecimal64: Decimal64 =
		if (toLong >= 0) Decimal64.round(toLong)
		else Decimal64MaxLongTimes2 + Decimal64.round(toLong)

	@inline def toDecimal64Exact: Decimal64 =
		if (toLong >= 0) Decimal64(toLong)
		else throw SugaredArithmeticException(toString + " cannot be represented exactly as a Decimal64.")

	@inline override def toString   : String = toUnsignedString(toLong)
	@inline def toString(radix: Int): String = toUnsignedString(toLong, radix)
	@inline def toBinaryString      : String = toLong.toBinaryString
	@inline def toOctalString       : String = toLong.toOctalString
	@inline def toHexString         : String = toLong.toHexString

	@deprecated("Adding a number and a String is deprecated. Use the string interpolation `s\"$num$str\"`", "Scala 2.13.0")
	@inline def +(x: String): String = toLong.toString + x

	@inline def <<(x: Int) : ULong = new ULong(toLong << x)
	@inline def >>>(x: Int): ULong = new ULong(toLong >>> x)
	@inline def >>(x: Int) : ULong = new ULong(toLong >> x)


	@inline def ==(x: Int)   : Boolean = x >= 0 & toLong == x
	@inline def ==(x: Long)(implicit __ :Ignored) : Boolean = x >= 0 & toLong == x
	@inline def ==(x: Float) : Boolean = toFloat == x
	@inline def ==(x: Double): Boolean = toDouble == x
	@inline def ==(x: ULong) : Boolean = x >= 0 & toLong == x.toLong
	@inline def !=(x: Int)   : Boolean = toLong < 0 | toLong != x
	@inline def !=(x: Long)(implicit __ :Ignored) : Boolean = toLong < 0 | toLong == x
	@inline def !=(x: Float) : Boolean = toFloat != x
	@inline def !=(x: Double): Boolean = toDouble != x
	@inline def !=(x: ULong) : Boolean = toLong != x.toLong

	@inline def < (x: Int)   : Boolean = x > 0 & toLong >= 0 & toLong < x
	@inline def < (x: Long)(implicit __ :Ignored) : Boolean = x > 0 & toLong >= 0 & toLong < x
	@inline def < (x: Float) : Boolean = toFloat < x
	@inline def < (x: Double): Boolean = toDouble < x
	@inline def < (x: ULong) : Boolean = toLong + Long.MinValue < x.toLong + Long.MinValue
	@inline def <=(x: Int)   : Boolean = x >= 0 & toLong >= 0 & toLong <= x
	@inline def <=(x: Long)(implicit __ :Ignored)  : Boolean = x >= 0 & toLong >= 0 & toLong <= x
	@inline def <=(x: Float) : Boolean = toFloat <= x
	@inline def <=(x: Double): Boolean = toDouble <= x
	@inline def <=(x: ULong) : Boolean = toLong + Long.MinValue <= x.toLong + Long.MinValue
	@inline def > (x: Int)   : Boolean = x < 0 | toLong < 0 | toLong > x
	@inline def > (x: Long)(implicit __ :Ignored) : Boolean = x < 0 | toLong < 0 | toLong > x
	@inline def > (x: Float) : Boolean = toFloat > x
	@inline def > (x: Double): Boolean = toDouble > x
	@inline def > (x: ULong) : Boolean = toLong + Long.MinValue > x.toLong + Long.MinValue
	@inline def >=(x: Int)   : Boolean = x < 0 | toLong < 0 | toLong >= x
	@inline def >=(x: Long)(implicit __ :Ignored)  : Boolean = x < 0 | toLong < 0 | toLong >= x
	@inline def >=(x: Float) : Boolean = toFloat >= x
	@inline def >=(x: Double): Boolean = toDouble >= x
	@inline def >=(x: ULong) : Boolean = toLong + Long.MinValue >= x.toLong + Long.MinValue

	@inline def compare(other: Long)(implicit __ :Ignored): Int =
		if (other < 0) 1 else compareUnsigned(toLong, other)

	@inline def compare(other: ULong): Int = compareUnsigned(toLong, other.toLong)

	@inline def min(other: ULong): ULong = new ULong(jl.Math.min(toLong + MinValue, other.toLong + MinValue) - MinValue)
	@inline def max(other: ULong): ULong = new ULong(jl.Math.max(toLong + MinValue, other.toLong + MinValue) - MinValue)
	@inline def clip(min: ULong, max: ULong): ULong =
		new ULong(jl.Math.max(toLong + MinValue, jl.Math.min(max.toLong + Long.MinValue, toLong + MinValue)) - MinValue)

	/** Returns `this max other`. */
	@inline def atLeast(other: ULong): ULong = if (toLong + MinValue >= other.toLong + MinValue) this else other

	/** Returns `this min other`. */
	@inline def atMost(other: ULong): ULong = if (toLong + MinValue <= other.toLong + MinValue) this else other

	/** Returns this `ULong`, or `0` if the condition is false. */
	@inline def orZeroIf(condition: Boolean): ULong = if (condition) new ULong(0L) else this

	/** Returns this `ULong`, or `0` if it does not satisfy the predicate. */
	@inline def orZeroIf(condition: ULong => Boolean): ULong = if (condition(this)) new ULong(0L) else this

	@inline def isPowerOf2: Boolean = toLong.bitCount == 1

	/** The greatest power of 2 lesser or equal to `this`, or zero if this `Long` equals zero. */
	@inline def powerOf2Floor: ULong = new ULong(highestOneBit(toLong))

	/** The greatest power of 2 lesser or equal to `this`, or zero if this `Long` equals zero. */
	@inline def lastPowerOf2: ULong = new ULong(highestOneBit(toLong))

	/** The least power of 2 greater or equal to `this`, or zero if this `Long` equals zero. */
	def powerOf2Ceil: ULong = { //todo: a better name; zero is not a power of 2. doubleFloor would almost fit.
		val lowerPow2 = highestOneBit(toLong)
		val pow2Mask  = toLong - 1 & lowerPow2                    //if (self > lowerPow2) lowerPow2 else 0
		val ifGtPow2  = pow2Mask << 1                             //if (self > lowerPow2) lowerPow2 else 0
		val ifEqPow2  = ~pow2Mask & lowerPow2                     //if (self == lowerPow2) lowerPow2 else 0
		new ULong(ifGtPow2 | ifEqPow2)
	}

	/** The least power of 2 greater or equal to `this`. */
	def nextPowerOf2: ULong = {
		val lowerPow2 = highestOneBit(toLong)
		val pow2Mask  = toLong - 1 & lowerPow2                    //if (self > lowerPow2) lowerPow2 else 0
		val ifGtPow2  = pow2Mask << 1                             //if (self > lowerPow2) lowerPow2 else 0
		val ifEqPow2  = ~pow2Mask & lowerPow2                     //if (self == lowerPow2) lowerPow2 else 0
		val ifZero    = (-lowerPow2 >>> 63) ^ 1L                  //if (self == 0) 1 else 0
		new ULong(ifGtPow2 | ifEqPow2 | ifZero)
	}

	@inline def |(x: Long): ULong = new ULong(toLong | x)
	@inline def &(x: Long): ULong = new ULong(toLong & x)
	@inline def ^(x: Long): ULong = new ULong(toLong ^ x)


	type ResultWithoutStep = NumericRange[ULong]
	@inline def to(end: ULong): NumericRange.Inclusive[ULong] = //I have no idea why scalac doesn't see this implicit
		NumericRange.inclusive(this, end, new ULong(1L))(ULong.ULongIsIntegral)

	@inline def to(end: ULong, step: ULong): NumericRange.Inclusive[ULong] =
		NumericRange.inclusive(this, end, step)(ULong.ULongIsIntegral)

	@inline def until(end: ULong): NumericRange.Exclusive[ULong] = NumericRange(this, end, new ULong(1L))
	@inline def until(end: ULong, step: ULong): NumericRange.Exclusive[ULong] =
		NumericRange(this, end, step)

	@inline def in(range: NumericRange[ULong]): Boolean = range.containsTyped(this)


	private def underflow(method: String): Nothing =
		throw SugaredArithmeticException("Arithmetic underflow: " + this + "." + method + ".")

	private def outOfRange(typeName: String): Nothing =
		throw SugaredArithmeticException("Value " + this + " is out of" + typeName + " range.")
}




@SerialVersionUID(Ver)
object ULong extends CompanionObject[ULong] {
	/** `2`^64^` - 1 == 18_446_744_073_709_551_615`. */
	final val MaxValue = new ULong(0xffffffffffffffffL)
	/** Zero. */
	final val MinValue = new ULong(0L)

	@throws[IllegalArgumentException]("if value is negative.")
	@inline def apply(value: Long): ULong =
		if (value < 0L) throwIllegalArgumentException(value)
		else new ULong(value)

	@throws[ArithmeticException]("if value is negative.")
	@inline def from(value: Long): ULong =
		if (value < 0L) throwArithmeticException(value)
		else new ULong(value)

	@throws[NumberFormatException]("if the string does not contain a Long value, or it is negative.")
	@inline def apply(string: String, radix: Int = 10): ULong = new ULong(parseUnsignedLong(string, radix))

	@throws[NumberFormatException]("if the string does not contain a Long value, or it is negative.")
	def decode(string: String): ULong = {
		val int = jl.Long.decode(string)
		if (int < 0L)
			throwNumberFormatException(string)
		new ULong(int)
	}

	def parse(string: String): Maybe[ULong] =
		Numeric.LongIsIntegral.parseString(string) match {
			case Some(long) => if (long >= 0) Yes(new ULong(long)) else No
			case None =>
				try Yes(new ULong(parseUnsignedLong(string))) catch {
					case _ :Exception => No
				}
			case _ => No
		}

	private def throwArithmeticException(value: Long): Nothing =
		throw SugaredArithmeticException("Value out of [0.." + MaxValue + "] range: " + value)

	private def throwIllegalArgumentException(value: Long): Nothing =
		illegal_!("Value out of [0.." + MaxValue + "] range: " + value)

	private def throwNumberFormatException(value: String): Nothing =
		throw SugaredNumberFormatException("Value out of [0.." + MaxValue + "] range: " + value)

	@inline implicit def UByteToULong(number: UByte): ULong = new ULong(number.toInt & 0xffL)
	@inline implicit def UShortToULong(number: UShort): ULong = new ULong(number.toInt & 0xffffL)
	@inline implicit def UIntToULong(number: UInt): ULong = new ULong(number.toInt & 0xffffffffL)

	//todo: in Scala3 create conversions from non negative Int literals
	@inline implicit def ULongSignedOps(self: ULong): ULongSignedOps = new ULongSignedOps(self.toLong)
	@inline implicit def ULongUnsignedOps(self: ULong): ULongUnsignedOps = new ULongUnsignedOps(self.toLong)

	class ULongUnsignedOps private[ULong](private val toLong: Long) extends AnyVal {
		@inline def +(x: UByte):  ULong = new ULong(toLong + (x.toByte & 0xffL))
		@inline def +(x: UShort): ULong = new ULong(toLong + (x.toShort & 0xffffL))
		@inline def +(x: UInt):   ULong = new ULong(toLong + (x.toInt & 0xffffffffL))
		@inline def +(x: ULong):  ULong = new ULong(toLong + x.toLong)
		@inline def -(x: UByte):  ULong = new ULong(toLong - (x.toByte & 0xffL))
		@inline def -(x: UShort): ULong = new ULong(toLong - (x.toShort & 0xffffL))
		@inline def -(x: UInt):   ULong = new ULong(toLong - (x.toInt & 0xffffffffL))
		@inline def -(x: ULong):  ULong = new ULong(toLong - x.toLong)
		@inline def *(x: UByte):  ULong = new ULong(toLong * (x.toByte & 0xffL))
		@inline def *(x: UShort): ULong = new ULong(toLong * (x.toShort & 0xffffL))
		@inline def *(x: UInt):   ULong = new ULong(toLong * (x.toInt & 0xffffffffL))
		@inline def *(x: ULong):  ULong = new ULong(toLong * x.toLong)
		@inline def /(x: UByte):  ULong = new ULong(divideUnsigned(toLong, x.toByte & 0xffL))
		@inline def /(x: UShort): ULong = new ULong(divideUnsigned(toLong, x.toShort & 0xffffL))
		@inline def /(x: UInt):   ULong = new ULong(divideUnsigned(toLong, x.toInt & 0xffffffffL))
		@inline def /(x: ULong):  ULong = new ULong(divideUnsigned(toLong, x.toLong))
		@inline def %(x: UByte):  ULong = new ULong(remainderUnsigned(toLong, x.toByte & 0xffL))
		@inline def %(x: UShort): ULong = new ULong(remainderUnsigned(toLong, x.toShort & 0xffffL))
		@inline def %(x: UInt):   ULong = new ULong(remainderUnsigned(toLong, x.toInt & 0xffffffffL))
		@inline def %(x: ULong):  ULong = new ULong(remainderUnsigned(toLong, x.toLong))

		/** Returns the quotient and the remainder of the division of this `ULong` by the argument. */
		@inline def /%(x: UByte): (ULong, ULong) = {
			val d = x.toByte & 0xffL
			val q = divideUnsigned(toLong, d)
			val r = toLong - q * d
			(new ULong(q), new ULong(r))
		}

		/** Returns the quotient and the remainder of the division of this `ULong` by the argument. */
		@inline def /%(x: UShort): (ULong, ULong) = {
			val d = x.toShort & 0xffffL
			val q = divideUnsigned(toLong, d)
			val r = toLong - q * d
			(new ULong(q), new ULong(r))
		}

		/** Returns the quotient and the remainder of the division of this `ULong` by the argument. */
		@inline def /%(x: UInt): (ULong, ULong) = {
			val q = divideUnsigned(toLong, x.toInt & 0xffffffffL)
			val r = toLong - q * (x.toInt & 0xffffffffL)
			(new ULong(q), new ULong(r))
		}

		/** Returns the quotient and the remainder of the division of this `ULong` by the argument. */
		@inline def /%(x: ULong): (ULong, ULong) = {
			val q = divideUnsigned(toLong & 0xffffffffL, x.toLong)
			val r = toLong - q * x.toLong
			(new ULong(q), new ULong(r))
		}

		@inline def **(n: UByte):  ULong = new ULong(toLong.pow(n.toByte & 0xff))
		@inline def **(n: UShort): ULong = new ULong(toLong.pow(n.toShort & 0xffff))
		@inline def **(n: UInt):   ULong = new ULong(toLong.pow(n.toInt))
	}

	class ULongSignedOps private[ULong](private val toLong: Long) extends AnyVal {
		@inline def +(x: Byte)  : ULong  = new ULong(toLong + (x & 0xffL))
		@inline def +(x: Short) : ULong  = new ULong(toLong + (x & 0xffffL))
		@inline def +(x: Int)   : ULong  = new ULong(toLong + (x & 0xffffffffL))
		@inline def +(x: Long)  : ULong  = new ULong(toLong + x)
		@inline def +(x: Float) : Float  = new ULong(toLong).toFloat + x
		@inline def +(x: Double): Double = new ULong(toLong).toDouble + x
		@inline def -(x: Byte)  : ULong  = new ULong(toLong - (x & 0xffL))
		@inline def -(x: Short) : ULong  = new ULong(toLong - (x & 0xffffL))
		@inline def -(x: Int)   : ULong  = new ULong(toLong - (x & 0xffffffffL))
		@inline def -(x: Long)  : ULong  = new ULong(toLong - x)
		@inline def -(x: Float) : Float  = new ULong(toLong).toFloat - x
		@inline def -(x: Double): Double = new ULong(toLong).toDouble - x

		@inline def *(x: Byte)  : ULong  = new ULong(toLong * (x & 0xffL))
		@inline def *(x: Short) : ULong  = new ULong(toLong * (x & 0xffffL))
		@inline def *(x: Int)   : ULong  = new ULong(toLong * (x & 0xffffffffL))
		@inline def *(x: Long)  : ULong  = new ULong(toLong * x)
		@inline def *(x: Float) : Float  = new ULong(toLong).toFloat * x
		@inline def *(x: Double): Double = new ULong(toLong).toDouble * x
		@inline def /(x: Byte)  : ULong  = new ULong(divideUnsigned(toLong, x & 0xffL))
		@inline def /(x: Short) : ULong  = new ULong(divideUnsigned(toLong, x & 0xffffL))
		@inline def /(x: Int)   : ULong  = new ULong(divideUnsigned(toLong, x & 0xffffffffL))
		@inline def /(x: Long)  : ULong  = new ULong(divideUnsigned(toLong, x))
		@inline def /(x: Float) : Float  = new ULong(toLong).toFloat / x
		@inline def /(x: Double): Double = new ULong(toLong).toDouble / x
		@inline def %(x: Byte)  : ULong  = new ULong(remainderUnsigned(toLong, x & 0xffL))
		@inline def %(x: Short) : ULong  = new ULong(remainderUnsigned(toLong, x & 0xffffL))
		@inline def %(x: Int)   : ULong  = new ULong(remainderUnsigned(toLong, x & 0xffffffffL))
		@inline def %(x: Long)  : ULong  = new ULong(remainderUnsigned(toLong, x))
		@inline def %(x: Float) : Float  = new ULong(toLong).toFloat % x
		@inline def %(x: Double): Double = new ULong(toLong).toDouble % x

		@inline def /%(x: Byte) : (ULong, ULong) = /%(x & 0xffL)
		@inline def /%(x: Short): (ULong, ULong) = /%(x & 0xffffL)
		@inline def /%(x: Int)  : (ULong, ULong) = /%(x & 0xffffffffL)
		/** Returns the quotient and the remainder of the division of this `ULong` by the argument. */
		@inline def /%(x: Long): (ULong, ULong) = {
			val q = divideUnsigned(toLong, x)
			val r = toLong - q * x
			(new ULong(q), new ULong(r))
		}

		@inline def **(n: Byte) : ULong  = new ULong(toLong.pow(n))
		@inline def **(n: Short): ULong  = new ULong(toLong.pow(n))
		@inline def **(n: Int)  : ULong  = new ULong(toLong.pow(n))

		/** Divides this `ULong` by the argument, creating a [[net.noresttherein.sugar.numeric.Ratio Ratio]]
		  * number representing the result.
		  * @param denominator the denominator of the created rational (before reduction)
		  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
		  */
		@inline def %/(denominator: Long): Ratio =
			if (toLong >= 0) Ratio(toLong, denominator)
			else {
				val gcd = unsignedGCD(toLong, denominator.abs)
				Ratio(divideUnsigned(toLong, gcd), divideUnsigned(denominator, gcd))
			}

		private def unsignedGCD(a: Long, b: Long): Long = {
			@tailrec def rec(a: Long, b: Long): Long =
				if (b == 0) a
				else rec(b, remainderUnsigned(a, b))
			if (a + Long.MinValue > b + Long.MinValue) rec(a, b)
			else rec(b, a)
		}
	}

	@SerialVersionUID(Ver)
	object conversions {
		@inline implicit def ULongToLong(number: ULong): Long = number.toLong
		@inline implicit def LongToULong(number: Long): ULong = new ULong(number)
		@inline implicit def checkedULongToLong(number: ULong): Long = number.toLongExact
		@inline implicit def checkedLongToULong(number: Long): ULong = ULong.from(number)
	}

	sealed abstract class ULongIsNumeric extends Numeric[ULong] {
		import ULong.ULongUnsignedOps

		override def plus(x: ULong, y: ULong): ULong = x + y
		override def minus(x: ULong, y: ULong): ULong = x - y
		override def times(x: ULong, y: ULong): ULong = x * y
		override def negate(x: ULong): ULong =
			throw SugaredArithmeticException("Cannot negate an unsigned number " + x)

		override def fromInt(x: Int): ULong =
			if (x < 0) throw SugaredArithmeticException("Cannot convert " + x + " to an unsigned integer")
			else new ULong(x)

		override def parseString(str: String): Option[ULong] = ULong.parse(str).toOption
		override def toInt(x: ULong): Int = x.toInt
		override def toLong(x: ULong): Long = x.toLong
		override def toFloat(x: ULong): Float = x.toFloat
		override def toDouble(x: ULong): Double = x.toDouble
		override def compare(x: ULong, y: ULong): Int =
			jl.Long.compare(x.toLong + Long.MinValue, y.toLong + Long.MinValue)
	}

	@SerialVersionUID(Ver)
	implicit object ULongIsIntegral extends ULongIsNumeric with Integral[ULong] {
		import ULong.ULongUnsignedOps

		override def quot(x: ULong, y: ULong): ULong = x / y
		override def rem(x: ULong, y: ULong): ULong = x % y
	}

	@SerialVersionUID(Ver)
	object ULongAsIfFractional extends ULongIsNumeric with Fractional[ULong] {
		import ULong.ULongUnsignedOps

		override def div(x: ULong, y: ULong): ULong = x / y
	}
//
//	@inline private def toBinaryString(x: Long): String = jl.Long.toBinaryString(x)
//	@inline private def toOctalString(x: Long): String = jl.Long.toOctalString(x)
//	@inline private def toHexString(x: Long): String = jl.Long.toHexString(x)

	private final val FloatMaxLongTimes2          = Long.MaxValue.toFloat * 2.0f
	private final val DoubleMaxLongTimes2         = Long.MaxValue.toDouble * 2.0
	private final val BigIntMaxLongTimes2         = BigInt(Long.MaxValue) * 2
	private final val BigIntegerMaxLongTimes2     = BigIntMaxLongTimes2.bigInteger
	private final val BigDecimalMaxLongTimes2     = BigDecimal(BigIntMaxLongTimes2)
	private final val JavaBigDecimalMaxLongTimes2 = BigDecimalMaxLongTimes2.bigDecimal
	private final val Decimal64MaxLongTimes2      = Decimal64.round(Long.MaxValue) * 2
}







