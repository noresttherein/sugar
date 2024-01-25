package net.noresttherein.sugar.numeric

import java.math.{BigInteger, BigDecimal => JavaBigDecimal}
import java.{lang => jl}

import scala.Long.MinValue
import scala.collection.immutable.NumericRange
import scala.math.ScalaNumericAnyConversions

import net.noresttherein.sugar.numeric.ULong.{BigDecimalMaxLongTimes2, BigIntMaxLongTimes2, BigIntegerMaxLongTimes2, Decimal64MaxLongTimes2, DoubleMaxLongTimes2, FloatMaxLongTimes2, JavaBigDecimalMaxLongTimes2}
import net.noresttherein.sugar.numeric.extensions.LongExtension
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{Yes, No}




/** An unsigned 64 bit integer backed by a `Long` value. All comparisons are done as if unsigned, and `toString`
  * and other formatting methods treat the underlying `Long` as unsigned.
  * This type doesn't check for overflows and underflows, meaning `ULong(1) - ULong(2)` will return `ULong.MaxValue`.
  *
  * Arithmetic with `Long` is not provided by default, as it is not clear which type should be converted
  * to another. The same applies to implicit conversions between these types.
  * For this purpose, either use `Long` (to which `UInt` is automatically converted), explicitly convert
  * either of the values to the other type, or import one of the conversions
  *   - [[net.noresttherein.sugar.numeric.ULong.conversions.ULongToLong]], or
  *   - [[net.noresttherein.sugar.numeric.UIong.conversions.LongToULong]].
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
class ULong private[numeric] (override val toLong: Long)
	extends AnyVal with Ordered[ULong] with ScalaNumericAnyConversions with Serializable
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
	@inline override def toFloat : Float  = if (toLong >= 0) toLong.toFloat else -toLong.toFloat
	@inline override def toDouble: Double = if (toLong >= 0) toLong.toDouble else -toLong.toDouble

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
		else throw new ArithmeticException(toString + " cannot be represented exactly as a Decimal64.")

	@inline override def toString   : String = jl.Long.toUnsignedString(toLong)
	@inline def toString(radix: Int): String = jl.Long.toUnsignedString(toLong, radix)
	@inline def toBinaryString      : String = jl.Long.toBinaryString(toLong)
	@inline def toOctalString       : String = jl.Long.toOctalString(toLong)
	@inline def toHexString         : String = jl.Long.toHexString(toLong)

	@deprecated("Adding a number and a String is deprecated. Use the string interpolation `s\"$num$str\"`", "Scala 2.13.0")
	@inline def +(x: String): String = toLong.toString + x

	@inline def <<(x: Int) : ULong = new ULong(toLong << x)
	@inline def >>>(x: Int): ULong = new ULong(toLong >>> x)
	@inline def >>(x: Int) : ULong = new ULong(toLong >> x)


	//Consider: the problem with all these comparisons is that they exclude ULong, which is handled by methods
	// inherited from Ordered. This works, because the erased signature is >(x :Object).
	// Unfortunately, it also means that it boxes both operands.
	// We may migrate to extension methods, but they won't work for == and !=.
	@inline def ==(x: Byte)  : Boolean = x >= 0 & toLong == x
	@inline def ==(x: Short) : Boolean = x >= 0 & toLong == x
	@inline def ==(x: Char)  : Boolean = toLong == x
	@inline def ==(x: Int)   : Boolean = x >= 0 & toLong == x
	@inline def ==(x: Long)  : Boolean = x >= 0 & toLong == x
	@inline def ==(x: Float) : Boolean = toFloat == x
	@inline def ==(x: Double): Boolean = toDouble == x
	@inline def !=(x: Byte)  : Boolean = toLong < 0 | toLong != x
	@inline def !=(x: Short) : Boolean = toLong < 0 | toLong != x
	@inline def !=(x: Char)  : Boolean = toLong != x
	@inline def !=(x: Int)   : Boolean = toLong < 0 | toLong != x
	@inline def !=(x: Long)  : Boolean = toLong < 0 | toLong != x
	@inline def !=(x: Float) : Boolean = toFloat != x
	@inline def !=(x: Double): Boolean = toDouble != x

	@inline def < (x: Byte)  : Boolean = x > 0 & toLong >= 0 & toLong < x
	@inline def < (x: Short) : Boolean = x > 0 & toLong >= 0 & toLong < x
	@inline def < (x: Char)  : Boolean = toLong >= 0 & toLong < x
	@inline def < (x: Int)   : Boolean = x > 0 & toLong >= 0 & toLong < x
	@inline def < (x: Long)  : Boolean = x > 0 & toLong >= 0 & toLong < x
	@inline def < (x: Float) : Boolean = toFloat < x
	@inline def < (x: Double): Boolean = toDouble < x
	@inline def <=(x: Byte)  : Boolean = x >= 0 & toLong >= 0 & toLong <= x
	@inline def <=(x: Short) : Boolean = x >= 0 & toLong >= 0 & toLong <= x
	@inline def <=(x: Char)  : Boolean = toLong >= 0 & toLong <= x
	@inline def <=(x: Int)   : Boolean = x >= 0 & toLong >= 0 & toLong <= x
	@inline def <=(x: Long)  : Boolean = x >= 0 & toLong >= 0 & toLong <= x
	@inline def <=(x: Float) : Boolean = toFloat <= x
	@inline def <=(x: Double): Boolean = toDouble <= x
	@inline def > (x: Byte)  : Boolean = x < 0 | toLong < 0 | toLong > x
	@inline def > (x: Short) : Boolean = x < 0 | toLong < 0 | toLong > x
	@inline def > (x: Char)  : Boolean = toLong < 0 | toLong > x
	@inline def > (x: Int)   : Boolean = x < 0 | toLong < 0 | toLong > x
	@inline def > (x: Long)  : Boolean = x < 0 | toLong < 0 | toLong > x
	@inline def > (x: Float) : Boolean = toFloat > x
	@inline def > (x: Double): Boolean = toDouble > x
	@inline def >=(x: Byte)  : Boolean = x < 0 | toLong < 0 | toLong >= x
	@inline def >=(x: Short) : Boolean = x < 0 | toLong < 0 | toLong >= x
	@inline def >=(x: Char)  : Boolean = toLong < 0 | toLong >= x
	@inline def >=(x: Int)   : Boolean = x < 0 | toLong < 0 | toLong >= x
	@inline def >=(x: Long)  : Boolean = x < 0 | toLong < 0 | toLong >= x
	@inline def >=(x: Float) : Boolean = toFloat >= x
	@inline def >=(x: Double): Boolean = toDouble >= x

	@inline def compare(other: ULong): Int = jl.Long.compare(toLong + MinValue, other.toLong + MinValue)

	@inline def min(other: ULong): ULong = new ULong(jl.Math.min(toLong + MinValue, other.toLong + MinValue) - MinValue)
	@inline def max(other: ULong): ULong = new ULong(jl.Math.max(toLong + MinValue, other.toLong + MinValue) - MinValue)

	/** Returns `this max other`. */
	@inline def atLeast(other: ULong): ULong = if (toLong + MinValue >= other.toLong + MinValue) this else other

	/** Returns `this min other`. */
	@inline def atMost(other: ULong): ULong = if (toLong + MinValue <= other.toLong + MinValue) this else other

	/** Returns this `ULong`, or `0` if the condition is false. */
	@inline def orZeroIf(condition: Boolean): ULong = if (condition) new ULong(0L) else this

	/** Returns this `ULong`, or `0` if it does not satisfy the predicate. */
	@inline def orZeroIf(condition: ULong => Boolean): ULong = if (condition(this)) new ULong(0L) else this

	@inline def |(x: Long): ULong = new ULong(toLong | x)
	@inline def &(x: Long): ULong = new ULong(toLong & x)
	@inline def ^(x: Long): ULong = new ULong(toLong ^ x)

	@inline def +(x: Char)  : ULong  = new ULong(toLong + x)
	@inline def +(x: UInt)  : ULong  = new ULong(toLong + x.toLong)
	@inline def +(x: ULong) : ULong  = new ULong(toLong + x.toLong)
	@inline def +(x: Float) : Float  = toFloat + x
	@inline def +(x: Double): Double = toDouble + x
	@inline def -(x: Char)  : ULong  = new ULong(toLong - x) //consider: making all subtractions return a signed integer
	@inline def -(x: UInt)  : ULong  = new ULong(toLong - x.toLong)
	@inline def -(x: ULong) : ULong  = new ULong(toLong - x.toLong)
	@inline def -(x: Float) : Float  = toFloat - x
	@inline def -(x: Double): Double = toDouble - x

	@inline def *(x: Char)  : ULong  = new ULong(toLong * x)
	@inline def *(x: UInt)  : ULong  = new ULong(toLong * x.toLong)
	@inline def *(x: ULong) : ULong  = new ULong(toLong * x.toLong)
	@inline def *(x: Float) : Float  = toFloat * x
	@inline def *(x: Double): Double = toDouble * x
	@inline def /(x: Char)  : ULong  = new ULong(jl.Long.divideUnsigned(toLong, x))
	@inline def /(x: UInt)  : ULong  = new ULong(jl.Long.divideUnsigned(toLong, x.toLong))
	@inline def /(x: ULong) : ULong  = new ULong(jl.Long.divideUnsigned(toLong, x.toLong))
	@inline def /(x: Float) : Float  = toFloat / x
	@inline def /(x: Double): Double = toDouble / x
	@inline def %(x: Char)  : ULong  = new ULong(jl.Long.remainderUnsigned(toLong, x))
	@inline def %(x: UInt)  : ULong  = new ULong(jl.Long.remainderUnsigned(toLong, x.toLong))
	@inline def %(x: ULong) : ULong  = new ULong(jl.Long.remainderUnsigned(toLong, x.toLong))
	@inline def %(x: Float) : Float  = toFloat % x
	@inline def %(x: Double): Double = toDouble % x
	@inline def **(n: Int)  : ULong  = new ULong(toLong.pow(n))

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
		throw new ArithmeticException("Arithmetic underflow: " + this + "." + method + ".")

	private def outOfRange(typeName: String): Nothing =
		throw new ArithmeticException("Value " + this + " is out of" + typeName + " range.")
}




@SerialVersionUID(Ver)
object ULong {
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
	@inline def apply(string: String, radix: Int = 10): ULong = new ULong(jl.Long.parseUnsignedLong(string, radix))

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
				try Yes(new ULong(jl.Long.parseUnsignedLong(string))) catch {
					case _ :Exception => No
				}
			case _ => No
		}

	private def throwArithmeticException(value: Long): Nothing =
		throw new ArithmeticException("Value out of [0.." + MaxValue + "] range: " + value)

	private def throwIllegalArgumentException(value: Long): Nothing =
		throw new IllegalArgumentException("Value out of [0.." + MaxValue + "] range: " + value)

	private def throwNumberFormatException(value: String): Nothing =
		throw new NumberFormatException("Value out of [0.." + MaxValue + "] range: " + value)


	@SerialVersionUID(Ver)
	object conversions {
		@inline implicit def ULongToLong(number: ULong): Long = number.toLong
		@inline implicit def LongToULong(number: Long): ULong = new ULong(number)
		@inline implicit def checkedULongToLong(number: ULong): Long = number.toLongExact
		@inline implicit def checkedLongToULong(number: Long): ULong = ULong.from(number)
	}

	sealed abstract class ULongIsNumeric extends Numeric[ULong] {
		override def plus(x: ULong, y: ULong): ULong = x + y
		override def minus(x: ULong, y: ULong): ULong = x - y
		override def times(x: ULong, y: ULong): ULong = x * y
		override def negate(x: ULong): ULong =
			throw new ArithmeticException("Cannot negate an unsigned number " + x)

		override def fromInt(x: Int): ULong =
			if (x < 0) throw new ArithmeticException("Cannot convert " + x + " to an unsigned integer")
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
		override def quot(x: ULong, y: ULong): ULong = x / y
		override def rem(x: ULong, y: ULong): ULong = x % y
	}
	@SerialVersionUID(Ver)
	object ULongAsIfFractional extends ULongIsNumeric with Fractional[ULong] {
		override def div(x: ULong, y: ULong): ULong = x / y
	}

	private final val FloatMaxLongTimes2          = Long.MaxValue.toFloat * 2.0f
	private final val DoubleMaxLongTimes2         = Long.MaxValue.toDouble * 2.0
	private final val BigIntMaxLongTimes2         = BigInt(Long.MaxValue) * 2
	private final val BigIntegerMaxLongTimes2     = BigIntMaxLongTimes2.bigInteger
	private final val BigDecimalMaxLongTimes2     = BigDecimal(BigIntMaxLongTimes2)
	private final val JavaBigDecimalMaxLongTimes2 = BigDecimalMaxLongTimes2.bigDecimal
	private final val Decimal64MaxLongTimes2      = Decimal64.round(Long.MaxValue) * 2
}







