package net.noresttherein.sugar.numeric

import java.{lang => jl}

import scala.Int.MinValue
import scala.collection.immutable.NumericRange
import scala.math.ScalaNumericAnyConversions




/** An unsigned 32 bit integer backed by an `Int` value. All comparisons are done as if unsigned, and `toString`
  * and other formatting methods treat the underlying `Int` as unsigned. Provided conversion methods fall into
  * three categories:
  *   1. [[net.noresttherein.sugar.numeric.UInt.byteValue byteValue]],
  *      [[net.noresttherein.sugar.numeric.UInt.shortValue shortValue]],
  *      [[net.noresttherein.sugar.numeric.UInt.intValue intValue]], and
  *      [[net.noresttherein.sugar.numeric.UInt.charValue charValue]]
  *      simply reinterpret the binary value of an appropriate number of lower bytes as the desired type,
  *      which may result in returning negative values.
  *   1. [[net.noresttherein.sugar.numeric.UInt.toByte toByte]], [[net.noresttherein.sugar.numeric.UInt.toShort toShort]],
  *      [[net.noresttherein.sugar.numeric.UInt.toInt toInt]], and [[net.noresttherein.sugar.numeric.UInt.toChar toChar]]
  *      check if the value can be represented by that type, and throw an [[ArithmeticException]] if not.
  *   1. For `Long`, `Float`, and `Double` methods `toX` and `asX` are equivalent, and return `toLong.toX`.
  *
  * However, this type doesn't check for overflows and underflows, meaning `UInt(1) - UInt(2)`
  * will return `UInt.MaxValue`.
  *
  * Arithmetic with `Int` is not provided by default, as it is not clear which type should be converted
  * to another. The same applies to implicit conversions between these types.
  * For this purpose, either use `Long` (to which `UInt` is automatically converted), explicitly convert
  * either of the values to the other type, or import one of the conversions
  *   - [[net.noresttherein.sugar.numeric.UInt.conversions.UIntToInt]], or
  *   - [[net.noresttherein.sugar.numeric.UInt.conversions.IntToUInt]].
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
class UInt private[numeric] (private val asInt :Int)
	extends AnyVal with Ordered[UInt] with ScalaNumericAnyConversions with Serializable
{
	@inline override def isWhole     : Boolean = true
	@inline override def isValidByte : Boolean = (asInt & 0x7f) == asInt
	@inline override def isValidShort: Boolean = (asInt & 0x7fff) == asInt
	@inline override def isValidChar : Boolean = (asInt & 0x7fff) == asInt
	@inline override def isValidInt  : Boolean = true

	@inline override def byteValue  : Byte    = asInt.toByte
	@inline override def shortValue : Short   = asInt.toShort
	@inline override def intValue   : Int     = asInt
	@inline override def longValue  : Long    = asInt & 0xffffffffL
	@inline override def floatValue : Float   = (asInt & 0xffffffffL).toFloat
	@inline override def doubleValue: Double  = (asInt & 0xffffffffL).toDouble
	@inline def charValue: Char = asInt.toChar
//
//	@inline def asByte   : Byte   = asInt.toByte
//	@inline def asShort  : Short  = asInt.toShort
//	@inline def asChar   : Char   = asInt.toChar
//	@inline def asLong   : Long   = asInt & 0xffffffffL
//	@inline def asFloat  : Float  = (asInt & 0xffffffffL).toFloat
//	@inline def asDouble : Double = (asInt & 0xffffffffL).toDouble

	@inline override def toByte   : Byte   = { testRange(Byte.MaxValue, "Byte"); asInt.toByte }
	@inline override def toShort  : Short  = { testRange(Short.MaxValue, "Short"); asInt.toShort }
	@inline override def toChar   : Char   = { testRange(Char.MaxValue, "Char"); asInt.toChar }
	@inline override def toInt    : Int    = { if (asInt < 0) underflow("toInt"); asInt }
	@inline override def toLong   : Long   = asInt.toLong
	@inline override def toFloat  : Float  = asInt.toLong.toFloat
	@inline override def toDouble : Double = asInt.toLong.toDouble
	/** Returns `asInt == 0`. */
	@inline def toBoolean: Boolean = asInt != 0

	@inline def toString(radix: Int): String = jl.Integer.toUnsignedString(asInt, radix)
	@inline override def toString   : String = String.valueOf(asInt & 0xffffffffL)
	@inline def toBinaryString      : String = jl.Integer.toBinaryString(asInt)
	@inline def toOctalString       : String = jl.Integer.toOctalString(asInt)
	@inline def toHexString         : String = jl.Integer.toHexString(asInt)

	@deprecated("Adding a number and a String is deprecated. Use the string interpolation `s\"$num$str\"`", "Scala 2.13.0")
	@inline def +(x: String): String = asInt.toString + x

	@inline def <<(x: Int)  : UInt = new UInt(asInt << x)
	@inline def >>>(x: Int) : UInt = new UInt(asInt >>> x)
	@inline def >>(x: Int)  : UInt = new UInt(asInt >> x)

	@inline def ==(x: Byte)  : Boolean = asInt >= 0 & asInt == x
	@inline def ==(x: Short) : Boolean = asInt >= 0 & asInt == x
	@inline def ==(x: Char)  : Boolean = asInt == x
	@inline def ==(x: Int)   : Boolean = asInt >= 0 & asInt == x
	@inline def ==(x: Long)  : Boolean = (asInt & 0xffffffffL) == x
	@inline def ==(x: Float) : Boolean = (asInt & 0xffffffffL).toFloat == x
	@inline def ==(x: Double): Boolean = (asInt & 0xffffffffL).toDouble == x
	@inline def !=(x: Byte)  : Boolean = asInt < 0 | asInt != x
	@inline def !=(x: Short) : Boolean = asInt < 0 | asInt != x
	@inline def !=(x: Char)  : Boolean = asInt < 0 | asInt != x
	@inline def !=(x: Int)   : Boolean = asInt != x
	@inline def !=(x: Long)  : Boolean = (asInt & 0xffffffffL) != x
	@inline def !=(x: Float) : Boolean = (asInt & 0xffffffffL).toFloat != x
	@inline def !=(x: Double): Boolean = (asInt & 0xffffffffL).toDouble != x

	@inline def < (x: Byte)  : Boolean = x > 0 & asInt > 0 & asInt < x
	@inline def < (x: Short) : Boolean = x > 0 & asInt > 0 & asInt < x
	@inline def < (x: Char)  : Boolean = x > 0 & asInt > 0 & asInt < x
	@inline def < (x: Int)   : Boolean = x > 0 & asInt > 0 & asInt < x
	@inline def < (x: Long)  : Boolean = (asInt & 0xffffffffL) < x
	@inline def < (x: Float) : Boolean = (asInt & 0xffffffffL).toFloat < x
	@inline def < (x: Double): Boolean = (asInt & 0xffffffffL).toDouble < x
	@inline def <=(x: Byte)  : Boolean = x >= 0 & asInt >= 0 & asInt <= x
	@inline def <=(x: Short) : Boolean = x >= 0 & asInt >= 0 & asInt <= x
	@inline def <=(x: Char)  : Boolean = asInt >= 0 & asInt <= x
	@inline def <=(x: Int)   : Boolean = x >= 0 & asInt >= 0 & asInt <= x
	@inline def <=(x: Long)  : Boolean = x <= (asInt & 0xffffffffL)
	@inline def <=(x: Float) : Boolean = x <= (asInt & 0xffffffffL).toFloat
	@inline def <=(x: Double): Boolean = x <= (asInt & 0xffffffffL).toDouble
	@inline def > (x: Byte)  : Boolean = x < 0 | asInt < 0 | asInt > x
	@inline def > (x: Short) : Boolean = x < 0 | asInt < 0 | asInt > x
	@inline def > (x: Char)  : Boolean = asInt < 0 | asInt > x
	@inline def > (x: Int)   : Boolean = x < 0 | asInt < 0 | asInt > x
	@inline def > (x: Long)  : Boolean = (asInt & 0xffffffffL) > x
	@inline def > (x: Float) : Boolean = (asInt & 0xffffffffL).toFloat > x
	@inline def > (x: Double): Boolean = (asInt & 0xffffffffL).toDouble > x
	@inline def >=(x: Byte)  : Boolean = x < 0 | asInt < 0 | asInt >= x
	@inline def >=(x: Short) : Boolean = x < 0 | asInt < 0 | asInt >= x
	@inline def >=(x: Char)  : Boolean = asInt < 0 | asInt >= x
	@inline def >=(x: Int)   : Boolean = x < 0 | asInt < 0 | asInt >= x
	@inline def >=(x: Long)  : Boolean = (asInt & 0xffffffffL) >= x
	@inline def >=(x: Float) : Boolean = (asInt & 0xffffffffL).toFloat >= x
	@inline def >=(x: Double): Boolean = (asInt & 0xffffffffL).toDouble >= x

	@inline def compare(other: UInt): Int = jl.Integer.compare(asInt + MinValue, other.asInt + MinValue)

	@inline def min(other: UInt): UInt = new UInt(jl.Math.min(asInt + MinValue, other.asInt + MinValue) - MinValue)
	@inline def max(other: UInt): UInt = new UInt(jl.Math.max(asInt + MinValue, other.asInt + MinValue) - MinValue)

//	@inline def |(x: Byte) : UInt  = new UInt(asInt | x)
//	@inline def |(x: Short): UInt  = new UInt(asInt | x)
//	@inline def |(x: Char) : UInt  = new UInt(asInt | x)
	@inline def |(x: Int)  : UInt  = new UInt(asInt | x)
	@inline def |(x: Long) : Long  = asInt & 0xffffffffL | x
//	@inline def &(x: Byte) : UInt  = new UInt(asInt & x)
//	@inline def &(x: Short): UInt  = new UInt(asInt & x)
//	@inline def &(x: Char) : UInt  = new UInt(asInt & x)
	@inline def &(x: Int)  : UInt  = new UInt(asInt & x)
	@inline def &(x: Long) : Long  = asInt & 0xffffffffL & x
//	@inline def ^(x: Byte) : UInt  = new UInt(asInt ^ x)
//	@inline def ^(x: Short): UInt  = new UInt(asInt ^ x)
//	@inline def ^(x: Char) : UInt  = new UInt(asInt ^ x)
	@inline def ^(x: Int)  : UInt  = new UInt(asInt ^ x)
	@inline def ^(x: Long) : Long  = asInt & 0xffffffffL ^ x

//	@inline def +(x: Byte)  : UInt   = new UInt(asInt + x)
//	@inline def +(x: Short) : UInt   = new UInt(asInt + x)
	@inline def +(x: Char)  : UInt   = new UInt(asInt + x)
//	@inline def +(x: Int)   : UInt   = new UInt(asInt + x)
	@inline def +(x: Long)  : Long   = (asInt & 0xffffffffL) + x
	@inline def +(x: Float) : Float  = (asInt & 0xffffffffL) + x
	@inline def +(x: Double): Double = (asInt & 0xffffffffL) + x
//	@inline def -(x: Byte)  : UInt   = new UInt(asInt - x)
//	@inline def -(x: Short) : UInt   = new UInt(asInt - x)
	@inline def -(x: Char)  : UInt   = new UInt(asInt - x)
//	@inline def -(x: Int)   : UInt   = new UInt(asInt - x)
	@inline def -(x: Long)  : Long   = (asInt & 0xffffffffL) - x
	@inline def -(x: Float) : Float  = (asInt & 0xffffffffL) - x
	@inline def -(x: Double): Double = (asInt & 0xffffffffL) - x
	//consider: should these check if x is non negative?
//	@inline def *(x: Byte)  : UInt   = new UInt(asInt * x)
//	@inline def *(x: Short) : UInt   = new UInt(asInt * x)
	@inline def *(x: Char)  : UInt   = new UInt(asInt * x)
//	@inline def *(x: Int)   : UInt   = new UInt(asInt * x)
	@inline def *(x: Long)  : Long   = (asInt & 0xffffffffL) * x
	@inline def *(x: Float) : Float  = (asInt & 0xffffffffL) * x
	@inline def *(x: Double): Double = (asInt & 0xffffffffL) * x
//	@inline def /(x: Byte)  : UInt   = new UInt(asInt / x)
//	@inline def /(x: Short) : UInt   = new UInt(asInt / x)
	@inline def /(x: Char)  : UInt   = new UInt(asInt / x)
//	@inline def /(x: Int)   : UInt   = new UInt(asInt / x)
	@inline def /(x: Long)  : Long   = (asInt & 0xffffffffL) / x
	@inline def /(x: Float) : Float  = (asInt & 0xffffffffL) / x
	@inline def /(x: Double): Double = (asInt & 0xffffffffL) / x
//	@inline def %(x: Byte)  : UInt   = new UInt(asInt % x)
//	@inline def %(x: Short) : UInt   = new UInt(asInt % x)
	@inline def %(x: Char)  : UInt   = new UInt(asInt % x)
//	@inline def %(x: Int)   : UInt   = new UInt(asInt % x)
	@inline def %(x: Long)  : Long   = (asInt & 0xffffffffL) % x
	@inline def %(x: Float) : Float  = (asInt & 0xffffffffL) % x
	@inline def %(x: Double): Double = (asInt & 0xffffffffL) % x

	type ResultWithoutStep = NumericRange[UInt]
	@inline def to(end: UInt): NumericRange.Inclusive[UInt] =
		NumericRange.inclusive(this, end, new UInt(1))(UInt.UIntIsIntegral)

	@inline def to(end: UInt, step: UInt): NumericRange.Inclusive[UInt] =
		NumericRange.inclusive(this, end, step)(UInt.UIntIsIntegral)

	@inline def until(end: UInt): NumericRange.Exclusive[UInt] = NumericRange(this, end, new UInt(1))
	@inline def until(end: UInt, step: UInt): NumericRange.Exclusive[UInt] =
		NumericRange(this, end, step)

	@inline def in(range: NumericRange[UInt]): Boolean = range.containsTyped(this)


	private[numeric] def underflow(method :String) :Nothing =
		throw new ArithmeticException("Arithmetic underflow: " + this + "." + method + ".")

	private[numeric] def outOfRange(typeName :String) :Nothing =
		throw new ArithmeticException("Value " + this + " is out of " + typeName + " range.")

	@inline private[numeric] def testRange(max :Int, typeName :String) :Unit =
		if (asInt + MinValue > max + MinValue)
			outOfRange(typeName)
}




@SerialVersionUID(Ver)
object UInt {
	final val MaxValue = new UInt(0xffffffff)
	final val MinValue = new UInt(0)

	@inline def apply(value: Int): UInt =
		if (value < 0) throw new IllegalArgumentException("negative value: " + value)
		else new UInt(value)

	@inline def from(value: Int): UInt =
		if (value < 0) throw new ArithmeticException("negative value: " + value)
		else new UInt(value)

	@inline def apply(string: String, radix: Int = 10): UInt = new UInt(jl.Integer.parseUnsignedInt(string, radix))

	@inline def decode(string: String): UInt = {
		val int = jl.Integer.decode(string)
		if (int < 0)
			throw new NumberFormatException(string + " is negative")
		new UInt(int)
	}

	@inline def parse(string: String): Option[UInt] =
		Numeric.IntIsIntegral.parseString(string).map(new UInt(_))

	//todo: in Scala3 create conversions from non negative Int literals
//	@inline implicit def UIntToULong(number :UInt) :ULong = new ULong(number.asInt)

	@SerialVersionUID(Ver)
	object conversions {
		@inline implicit def UIntToInt(number: UInt): Int = number.asInt
		@inline implicit def IntToUInt(number: Int): UInt = new UInt(number)
		@inline implicit def checkedUIntToInt(number: UInt): Int = number.toInt
		@inline implicit def checkedIntToUInt(number: Int): UInt = UInt.from(number)
	}

	sealed abstract class UIntIsNumeric extends Numeric[UInt] {
		override def plus(x: UInt, y: UInt): UInt = x + y
		override def minus(x: UInt, y: UInt): UInt = x - y
		override def times(x: UInt, y: UInt): UInt = x * y
		override def negate(x: UInt): UInt =
			throw new ArithmeticException("Cannot negate an unsigned number " + x)

		override def fromInt(x: Int): UInt = UInt.from(x)
		override def parseString(str: String): Option[UInt] = UInt.parse(str)
		override def toInt(x: UInt): Int = x.toInt
		override def toLong(x: UInt): Long = x.toLong
		override def toFloat(x: UInt): Float = x.toFloat
		override def toDouble(x: UInt): Double = x.toDouble
		override def compare(x: UInt, y: UInt): Int =
			jl.Integer.compare(x.asInt + Int.MinValue, y.asInt + Int.MinValue)
	}
	@SerialVersionUID(Ver)
	implicit object UIntIsIntegral extends UIntIsNumeric with Integral[UInt] {
		override def quot(x: UInt, y: UInt): UInt = x / y
		override def rem(x: UInt, y: UInt): UInt = x % y
	}
	@SerialVersionUID(Ver)
	object UIntAsIfFractional extends UIntIsNumeric with Fractional[UInt] {
		override def div(x: UInt, y: UInt): UInt = x / y
	}
}

