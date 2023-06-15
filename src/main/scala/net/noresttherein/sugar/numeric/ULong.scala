package net.noresttherein.sugar.numeric

import java.{lang => jl}

import scala.Long.MinValue
import scala.collection.immutable.NumericRange
import scala.math.ScalaNumericAnyConversions




/** An unsigned 64 bit integer backed by a `Long` value. All comparisons are done as if unsigned, and `toString`
  * and other formatting methods treat the underlying `Int` as unsigned. Provided conversion methods fall into
  * three categories:
  *   1. [[net.noresttherein.sugar.numeric.ULong.byteValue byteValue]],
  *      [[net.noresttherein.sugar.numeric.ULong.shortValue shortValue]],
  *      [[net.noresttherein.sugar.numeric.ULong.intValue intValue]],
  *      [[net.noresttherein.sugar.numeric.ULong.longValue longValue]], and
  *      [[net.noresttherein.sugar.numeric.ULong.charValue charValue]]
  *      simply reinterpret the binary value of an appropriate number of lower bytes as the desired type,
  *      which may result in returning negative values.
  *   1. [[net.noresttherein.sugar.numeric.ULong.toByte toByte]], [[net.noresttherein.sugar.numeric.ULong.toShort toShort]],
  *      [[net.noresttherein.sugar.numeric.ULong.toInt toInt]], [[net.noresttherein.sugar.numeric.ULong.toLong toLong]],
  *      and [[net.noresttherein.sugar.numeric.ULong.toChar toChar]]
  *      check if the value can be represented by that type, and throw an [[ArithmeticException]] if not.
  *   1. For `Float`, and `Double` methods `toX` and `asX` are equivalent,
  *      and return `if (toLong >= 0) toLong.toX else -toLong.toFloat` (or `-toLong.toDouble`).
  *
  * However, this type doesn't check for overflows and underflows, meaning `ULong(1) - ULong(2)`
  * will return `UInt.MaxValue`.
  *
  * Arithmetic with `Long` is not provided by default, as it is not clear which type should be converted
  * to another. The same applies to implicit conversions between these types.
  * For this purpose, either use `Long` (to which `UInt` is automatically converted), explicitly convert
  * either of the values to the other type, or import one of the conversions
  *   - [[net.noresttherein.sugar.numeric.UInt.conversions.UIntToInt]], or
  *   - [[net.noresttherein.sugar.numeric.UInt.conversions.IntToUInt]].
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
class ULong private[numeric] (private val asLong :Long)
	extends AnyVal with Ordered[ULong] with ScalaNumericAnyConversions with Serializable
{
	@inline override def isWhole     : Boolean = true
	@inline override def isValidByte : Boolean = (asLong & 0x7fL) == asLong
	@inline override def isValidShort: Boolean = (asLong & 0x7fffL) == asLong
	@inline override def isValidChar : Boolean = (asLong & 0x7fffL) == asLong
	@inline override def isValidInt  : Boolean = (asLong & 0x7fffffffL) == asLong

	@inline override def byteValue  : Byte    = asLong.toByte
	@inline override def shortValue : Short   = asLong.toShort
	@inline override def intValue   : Int     = asLong.toInt
	@inline override def longValue  : Long    = asLong
	@inline override def floatValue : Float   = if (asLong >= 0) asLong.toFloat else -asLong.toFloat
	@inline override def doubleValue: Double  = if (asLong >= 0) asLong.toDouble else -asLong.toDouble
	@inline def charValue: Char = asLong.toChar
//
//	@inline def asByte   : Byte   = asLong.toByte
//	@inline def asShort  : Short  = asLong.toShort
//	@inline def asChar   : Char   = asLong.toChar
//	@inline def asLong   : Long   = asLong & 0xffffffffL
//	@inline def asFloat  : Float  = (asLong & 0xffffffffL).toFloat
//	@inline def asDouble : Double = (asLong & 0xffffffffL).toDouble
//	/** Returns `asLong == 0`. */
//	@inline def asBoolean :Boolean = asLong == 0

	@inline override def toByte   : Byte   = { testRange(Byte.MaxValue, "Byte"); asLong.toByte }
	@inline override def toShort  : Short  = { testRange(Short.MaxValue, "Short"); asLong.toShort }
	@inline override def toChar   : Char   = { testRange(Char.MaxValue, "Char"); asLong.toChar }
	@inline override def toInt    : Int    = { testRange(Int.MaxValue, "Int"); asLong.toInt }
	@inline override def toLong   : Long   = if (asLong < 0) underflow(".toLong") else asLong
	@inline override def toFloat  : Float  = if (asLong >= 0) asLong.toFloat else -asLong.toFloat
	@inline override def toDouble : Double = if (asLong >= 0) asLong.toDouble else -asLong.toDouble
	/** Returns `asLong == 0`. */
	@inline def toBoolean: Boolean = asLong != 0L

	@inline def toString(radix: Int): String = jl.Long.toUnsignedString(asLong, radix)
	@inline override def toString   : String = jl.Long.toUnsignedString(asLong)
	@inline def toBinaryString      : String = jl.Long.toBinaryString(asLong)
	@inline def toOctalString       : String = jl.Long.toOctalString(asLong)
	@inline def toHexString         : String = jl.Long.toHexString(asLong)

	@deprecated("Adding a number and a String is deprecated. Use the string interpolation `s\"$num$str\"`", "Scala 2.13.0")
	@inline def +(x: String): String = asLong.toString + x

	@inline def <<(x: Int)  : ULong = new ULong(asLong << x)
	@inline def >>>(x: Int) : ULong = new ULong(asLong >>> x)
	@inline def >>(x: Int)  : ULong = new ULong(asLong >> x)

	@inline def ==(x: Byte)  : Boolean = x >= 0 & asLong == x
	@inline def ==(x: Short) : Boolean = x >= 0 & asLong == x
	@inline def ==(x: Char)  : Boolean = asLong == x
	@inline def ==(x: Int)   : Boolean = x >= 0 & asLong == x
	@inline def ==(x: Long)  : Boolean = x >= 0 & asLong == x
	@inline def ==(x: Float) : Boolean = toFloat == x
	@inline def ==(x: Double): Boolean = toDouble == x
	@inline def !=(x: Byte)  : Boolean = asLong < 0 | asLong != x
	@inline def !=(x: Short) : Boolean = asLong < 0 | asLong != x
	@inline def !=(x: Char)  : Boolean = asLong != x
	@inline def !=(x: Int)   : Boolean = asLong < 0 | asLong != x
	@inline def !=(x: Long)  : Boolean = asLong < 0 | asLong != x
	@inline def !=(x: Float) : Boolean = toFloat != x
	@inline def !=(x: Double): Boolean = toDouble != x

	@inline def < (x: Byte)  : Boolean = x > 0 & asLong >= 0 & asLong < x
	@inline def < (x: Short) : Boolean = x > 0 & asLong >= 0 & asLong < x
	@inline def < (x: Char)  : Boolean = asLong >= 0 & asLong < x
	@inline def < (x: Int)   : Boolean = x > 0 & asLong >= 0 & asLong < x
	@inline def < (x: Long)  : Boolean = x > 0 & asLong >= 0 & asLong < x
	@inline def < (x: Float) : Boolean = toFloat < x
	@inline def < (x: Double): Boolean = toDouble < x
	@inline def <=(x: Byte)  : Boolean = x >= 0 & asLong >= 0 & asLong <= x
	@inline def <=(x: Short) : Boolean = x >= 0 & asLong >= 0 & asLong <= x
	@inline def <=(x: Char)  : Boolean = asLong >= 0 & asLong <= x
	@inline def <=(x: Int)   : Boolean = x >= 0 & asLong >= 0 & asLong <= x
	@inline def <=(x: Long)  : Boolean = x >= 0 & asLong >= 0 & asLong <= x
	@inline def <=(x: Float) : Boolean = toFloat <= x
	@inline def <=(x: Double): Boolean = toDouble <= x
	@inline def > (x: Byte)  : Boolean = x < 0 | asLong < 0 | asLong > x
	@inline def > (x: Short) : Boolean = x < 0 | asLong < 0 | asLong > x
	@inline def > (x: Char)  : Boolean = asLong < 0 | asLong > x
	@inline def > (x: Int)   : Boolean = x < 0 | asLong < 0 | asLong > x
	@inline def > (x: Long)  : Boolean = x < 0 | asLong < 0 | asLong > x
	@inline def > (x: Float) : Boolean = toFloat > x
	@inline def > (x: Double): Boolean = toDouble > x
	@inline def >=(x: Byte)  : Boolean = x < 0 | asLong < 0 | asLong >= x
	@inline def >=(x: Short) : Boolean = x < 0 | asLong < 0 | asLong >= x
	@inline def >=(x: Char)  : Boolean = asLong < 0 | asLong >= x
	@inline def >=(x: Int)   : Boolean = x < 0 | asLong < 0 | asLong >= x
	@inline def >=(x: Long)  : Boolean = x < 0 | asLong < 0 | asLong >= x
	@inline def >=(x: Float) : Boolean = toFloat >= x
	@inline def >=(x: Double): Boolean = toDouble >= x

	@inline def compare(other: ULong) :Int = jl.Long.compare(asLong + MinValue, other.asLong + MinValue)

	@inline def min(other: ULong): ULong = new ULong(jl.Math.min(asLong + MinValue, other.asLong + MinValue) - MinValue)
	@inline def max(other: ULong): ULong = new ULong(jl.Math.max(asLong + MinValue, other.asLong + MinValue) - MinValue)

//	@inline def |(x: Byte) : ULong = new ULong(asLong | x)
//	@inline def |(x: Short): ULong = new ULong(asLong | x)
//	@inline def |(x: Char) : ULong = new ULong(asLong | x)
//	@inline def |(x: Int)  : ULong = new ULong(asLong | x)
	@inline def |(x: Long) : ULong = new ULong(asLong | x)
//	@inline def &(x: Byte) : ULong = new ULong(asLong & x)
//	@inline def &(x: Short): ULong = new ULong(asLong & x)
//	@inline def &(x: Char) : ULong = new ULong(asLong & x)
//	@inline def &(x: Int)  : ULong = new ULong(asLong & x)
	@inline def &(x: Long) : ULong = new ULong(asLong & x)
//	@inline def ^(x: Byte) : ULong = new ULong(asLong ^ x)
//	@inline def ^(x: Short): ULong = new ULong(asLong ^ x)
//	@inline def ^(x: Char) : ULong = new ULong(asLong ^ x)
//	@inline def ^(x: Int)  : ULong = new ULong(asLong ^ x)
	@inline def ^(x: Long) : ULong = new ULong(asLong ^ x)

//	@inline def +(x: Byte)  : ULong  = new ULong(asLong + x)
//	@inline def +(x: Short) : ULong  = new ULong(asLong + x)
	@inline def +(x: Char)  : ULong  = new ULong(asLong + x)
//	@inline def +(x: Int)   : ULong  = new ULong(asLong + x)
//	@inline def +(x: Long)  : ULong  = new ULong(asLong + x)
	@inline def +(x: UInt)  : ULong  = new ULong(asLong + x.toLong)
	@inline def +(x: ULong) : ULong  = new ULong(asLong + x.asLong)
	@inline def +(x: Float) : Float  = toFloat + x
	@inline def +(x: Double): Double = toDouble + x
//	@inline def -(x: Byte)  : ULong  = new ULong(asLong - x)
//	@inline def -(x: Short) : ULong  = new ULong(asLong - x)
	@inline def -(x: Char)  : ULong  = new ULong(asLong - x)
//	@inline def -(x: Int)   : ULong  = new ULong(asLong - x)
//	@inline def -(x: Long)  : ULong  = new ULong(asLong - x)
	@inline def -(x: UInt)  : ULong  = new ULong(asLong - x.toLong)
	@inline def -(x: ULong) : ULong  = new ULong(asLong - x.asLong)
	@inline def -(x: Float) : Float  = toFloat - x
	@inline def -(x: Double): Double = toDouble - x
	//consider: should these check if x is non negative?
//	@inline def *(x: Byte)  : ULong  = new ULong(asLong * x)
//	@inline def *(x: Short) : ULong  = new ULong(asLong * x)
	@inline def *(x: Char)  : ULong  = new ULong(asLong * x)
//	@inline def *(x: Int)   : ULong  = new ULong(asLong * x)
//	@inline def *(x: Long)  : ULong  = new ULong(asLong * x)
	@inline def *(x: UInt)  : ULong  = new ULong(asLong * x.toLong)
	@inline def *(x: ULong) : ULong  = new ULong(asLong * x.asLong)
	@inline def *(x: Float) : Float  = toFloat * x
	@inline def *(x: Double): Double = toDouble * x
//	@inline def /(x: Byte)  : ULong  = new ULong(asLong / x)
//	@inline def /(x: Short) : ULong  = new ULong(asLong / x)
	@inline def /(x: Char)  : ULong  = new ULong(asLong / x)
//	@inline def /(x: Int)   : ULong  = new ULong(asLong / x)
//	@inline def /(x: Long)  : ULong  = new ULong(asLong / x)
	@inline def /(x: UInt)  : ULong  = new ULong(asLong / x.toLong)
	@inline def /(x: ULong) : ULong  = new ULong(asLong / x.asLong)
	@inline def /(x: Float) : Float  = toFloat / x
	@inline def /(x: Double): Double = toDouble / x
//	@inline def %(x: Byte)  : ULong  = new ULong(asLong % x)
//	@inline def %(x: Short) : ULong  = new ULong(asLong % x)
	@inline def %(x: Char)  : ULong  = new ULong(asLong % x)
//	@inline def %(x: Int)   : ULong  = new ULong(asLong % x)
//	@inline def %(x: Long)  : ULong  = new ULong(asLong % x)
	@inline def %(x :UInt)  : ULong  = new ULong(asLong % x.toLong)
	@inline def %(x :ULong) : ULong  = new ULong(asLong % x.asLong)
	@inline def %(x: Float) : Float  = toFloat % x
	@inline def %(x: Double): Double = toDouble % x

	type ResultWithoutStep = NumericRange[ULong]
	@inline def to(end: ULong): NumericRange.Inclusive[ULong] = //I have no idea why scalac doesn't see this implicit
		NumericRange.inclusive(this, end, new ULong(1L))(ULong.ULongIsIntegral)

	@inline def to(end: ULong, step: ULong): NumericRange.Inclusive[ULong] =
		NumericRange.inclusive(this, end, step)(ULong.ULongIsIntegral)

	@inline def until(end: ULong): NumericRange.Exclusive[ULong] = NumericRange(this, end, new ULong(1L))
	@inline def until(end: ULong, step: ULong): NumericRange.Exclusive[ULong] =
		NumericRange(this, end, step)

	@inline def in(range: NumericRange[ULong]): Boolean = range.containsTyped(this)


	private[numeric] def underflow(method :String) :Nothing =
		throw new ArithmeticException("Arithmetic underflow: " + this + "." + method + ".")

	private[numeric] def outOfRange(typeName :String) :Nothing =
		throw new ArithmeticException("Value " + this + " is out of" + typeName + " range.")

	@inline private[numeric] def testRange(max :Int, typeName :String) :Unit =
		if (asLong + MinValue > max + MinValue)
			outOfRange(typeName)
}




@SerialVersionUID(Ver)
object ULong {
	final val MaxValue = new ULong(0xffffffffffffffffL)
	final val MinValue = new ULong(0L)
	
	@inline def apply(value: Long): ULong =
		if (value < 0L) throw new IllegalArgumentException("negative value: " + value)
		else new ULong(value)

	@inline def from(value: Long): ULong =
		if (value < 0L) throw new ArithmeticException("negative value: " + value)
		else new ULong(value)

	@inline def apply(string: String, radix: Int = 10): ULong = new ULong(jl.Long.parseUnsignedLong(string, radix))

	@inline def decode(string: String): ULong = {
		val int = jl.Long.decode(string)
		if (int < 0L)
			throw new NumberFormatException(string + " is negative")
		new ULong(int)
	}

	@inline def parse(string: String): Option[ULong] =
		Numeric.LongIsIntegral.parseString(string).map(new ULong(_))

	@SerialVersionUID(Ver)
	object conversions {
		@inline implicit def ULongToLong(number: ULong): Long = number.asLong
		@inline implicit def LongToULong(number: Long): ULong = new ULong(number)
		@inline implicit def checkedULongToLong(number: ULong): Long = number.toLong
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

		override def parseString(str: String): Option[ULong] = ULong.parse(str)
		override def toInt(x: ULong): Int = x.toInt
		override def toLong(x: ULong): Long = x.toLong
		override def toFloat(x: ULong): Float = x.toFloat
		override def toDouble(x: ULong): Double = x.toDouble
		override def compare(x: ULong, y: ULong) :Int =
			jl.Long.compare(x.asLong + Long.MinValue, y.asLong + Long.MinValue)
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
}







