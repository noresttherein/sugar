package net.noresttherein.sugar.numeric

import java.math.{BigInteger, BigDecimal => JavaBigDecimal}
import java.{lang => jl}

import scala.Int.MinValue
import scala.collection.immutable.NumericRange
import scala.math.ScalaNumericAnyConversions

import net.noresttherein.sugar.exceptions.{SugaredArithmeticException, SugaredNumberFormatException}
import net.noresttherein.sugar.illegal_!
import net.noresttherein.sugar.numeric.extensions.IntExtension
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




/** An unsigned 32 bit integer backed by an `Int` value. All comparisons are done as if unsigned, and `toString`
  * and other formatting methods treat the underlying `Int` as unsigned.
  * This type doesn't check for overflows and underflows, meaning `UInt(1) - UInt(2)` will return `UInt.MaxValue`.
  *
  * Arithmetic with `Int` is not provided by default, as it is not clear which type should be converted
  * to another, as both conversions may overflow/underflow. The same applies to implicit conversions between these types.
  * For this purpose, either use `Long` (to which `UInt` is automatically converted), explicitly convert
  * either of the values to the other type, or import one of the conversions
  *   - [[net.noresttherein.sugar.numeric.UInt.conversions.UIntToInt]], or
  *   - [[net.noresttherein.sugar.numeric.UInt.conversions.IntToUInt]].
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
class UInt private[numeric] (override val toInt: Int)
	extends AnyVal with Ordered[UInt] with ScalaNumericAnyConversions with Serializable
{
	@inline override def isWhole     : Boolean = true
	@inline override def isValidByte : Boolean = (toInt & 0x7f) == toInt
	@inline override def isValidShort: Boolean = (toInt & 0x7fff) == toInt
	@inline override def isValidChar : Boolean = (toInt & 0xffff) == toInt
	@inline override def isValidInt  : Boolean = toInt >= 0

	@inline override def byteValue  : Byte    = toInt.toByte
	@inline override def shortValue : Short   = toInt.toShort
	@inline override def intValue   : Int     = toInt
	@inline override def longValue  : Long    = toInt & 0xffffffffL
	@inline override def floatValue : Float   = (toInt & 0xffffffffL).toFloat
	@inline override def doubleValue: Double  = (toInt & 0xffffffffL).toDouble
	@inline def charValue: Char = toInt.toChar

	@inline override def toByte  : Byte  = toInt.toByte
	@inline override def toShort : Short = toInt.toShort
	@inline override def toLong  : Long   = toInt.toLong
	@inline override def toFloat : Float  = toInt.toLong.toFloat
	@inline override def toDouble: Double = toInt.toLong.toDouble

	@inline def toByteExact  : Byte   = if ((toInt & 0x7f) != toInt) outOfRange("Byte") else toInt.toByte
	@inline def toShortExact : Short  = if ((toInt & 0x7fff) != toInt) outOfRange("Short") else toInt.toShort
	@inline def toCharExact  : Char   = if ((toInt & 0xffff) != toInt) outOfRange("Char") else toInt.toChar
	@inline def toIntExact   : Int    = { if (toInt < 0) underflow("toInt"); toInt }
	@inline def toUShortExact: UShort = if ((toInt & 0xff) != toInt) outOfRange("UShort") else new UShort(toInt.toShort)

	/** Returns `toInt != 0`. */
	@inline def toBoolean       : Boolean        = toInt != 0
	@inline def toBigInt        : BigInt         = BigInt(toInt & 0xffffffffL)
	@inline def toBigInteger    : BigInteger     = BigInteger.valueOf(toInt & 0xffffffffL)
	@inline def toBigDecimal    : BigDecimal     = BigDecimal(toInt & 0xffffffffL)
	@inline def toJavaBigDecimal: JavaBigDecimal = JavaBigDecimal.valueOf(toInt & 0xffffffffL)

	@inline def toDecimal64: Decimal64 = Decimal64(toInt & 0xffffffffL)
	@inline def toUShort   : UShort    = new UShort(toInt.toShort)
	@inline def toULong    : ULong     = new ULong(toInt & 0xffffffffL)
	@inline def toSafeInt  : SafeInt   = new SafeInt(toIntExact)
	@inline def toSafeLong : SafeLong  = new SafeLong(toInt & 0xffffffffL)
	@inline def toIntRatio : IntRatio  = IntRatio(toInt)
	@inline def toRatio    : Ratio     = Ratio(toInt & 0xffffffffL)

	@inline override def toString   : String = String.valueOf(toInt & 0xffffffffL)
	@inline def toString(radix: Int): String = jl.Integer.toUnsignedString(toInt, radix)
	@inline def toBinaryString      : String = jl.Integer.toBinaryString(toInt)
	@inline def toOctalString       : String = jl.Integer.toOctalString(toInt)
	@inline def toHexString         : String = jl.Integer.toHexString(toInt)

	@deprecated("Adding a number and a String is deprecated. Use the string interpolation `s\"$num$str\"`", "Scala 2.13.0")
	@inline def +(x: String): String = toInt.toString + x

	@inline def <<(x: Int) : UInt = new UInt(toInt << x)
	@inline def >>>(x: Int): UInt = new UInt(toInt >>> x)
	@inline def >>(x: Int) : UInt = new UInt(toInt >> x)

	//Consider: the problem with all these comparisons is that they exclude UInt, which is handled by methods
	// inherited from Ordered. This works, because the erased signature is >(x :Object).
	// Unfortunately, it also means that it boxes both operands.
	// We may migrate to extension methods, but they won't work for == and !=.
	@inline def ==(x: Byte)  : Boolean = toInt >= 0 & toInt == x
	@inline def ==(x: Short) : Boolean = toInt >= 0 & toInt == x
	@inline def ==(x: Char)  : Boolean = toInt == x
	@inline def ==(x: Int)   : Boolean = toInt >= 0 & toInt == x
	@inline def ==(x: Long)  : Boolean = (toInt & 0xffffffffL) == x
	@inline def ==(x: Float) : Boolean = (toInt & 0xffffffffL).toFloat == x
	@inline def ==(x: Double): Boolean = (toInt & 0xffffffffL).toDouble == x
	@inline def !=(x: Byte)  : Boolean = toInt < 0 | toInt != x
	@inline def !=(x: Short) : Boolean = toInt < 0 | toInt != x
	@inline def !=(x: Char)  : Boolean = toInt < 0 | toInt != x
	@inline def !=(x: Int)   : Boolean = toInt != x
	@inline def !=(x: Long)  : Boolean = (toInt & 0xffffffffL) != x
	@inline def !=(x: Float) : Boolean = (toInt & 0xffffffffL).toFloat != x
	@inline def !=(x: Double): Boolean = (toInt & 0xffffffffL).toDouble != x

	@inline def < (x: Byte)  : Boolean = x > 0 & toInt > 0 & toInt < x
	@inline def < (x: Short) : Boolean = x > 0 & toInt > 0 & toInt < x
	@inline def < (x: Char)  : Boolean = x > 0 & toInt > 0 & toInt < x
	@inline def < (x: Int)   : Boolean = x > 0 & toInt > 0 & toInt < x
	@inline def < (x: Long)  : Boolean = (toInt & 0xffffffffL) < x
	@inline def < (x: Float) : Boolean = (toInt & 0xffffffffL).toFloat < x
	@inline def < (x: Double): Boolean = (toInt & 0xffffffffL).toDouble < x
	@inline def <=(x: Byte)  : Boolean = x >= 0 & toInt >= 0 & toInt <= x
	@inline def <=(x: Short) : Boolean = x >= 0 & toInt >= 0 & toInt <= x
	@inline def <=(x: Char)  : Boolean = toInt >= 0 & toInt <= x
	@inline def <=(x: Int)   : Boolean = x >= 0 & toInt >= 0 & toInt <= x
	@inline def <=(x: Long)  : Boolean = (toInt & 0xffffffffL) <= x
	@inline def <=(x: Float) : Boolean = (toInt & 0xffffffffL).toFloat <= x
	@inline def <=(x: Double): Boolean = (toInt & 0xffffffffL).toDouble <= x
	@inline def > (x: Byte)  : Boolean = x < 0 | toInt < 0 | toInt > x
	@inline def > (x: Short) : Boolean = x < 0 | toInt < 0 | toInt > x
	@inline def > (x: Char)  : Boolean = toInt < 0 | toInt > x
	@inline def > (x: Int)   : Boolean = x < 0 | toInt < 0 | toInt > x
	@inline def > (x: Long)  : Boolean = (toInt & 0xffffffffL) > x
	@inline def > (x: Float) : Boolean = (toInt & 0xffffffffL).toFloat > x
	@inline def > (x: Double): Boolean = (toInt & 0xffffffffL).toDouble > x
	@inline def >=(x: Byte)  : Boolean = x < 0 | toInt < 0 | toInt >= x
	@inline def >=(x: Short) : Boolean = x < 0 | toInt < 0 | toInt >= x
	@inline def >=(x: Char)  : Boolean = toInt < 0 | toInt >= x
	@inline def >=(x: Int)   : Boolean = x < 0 | toInt < 0 | toInt >= x
	@inline def >=(x: Long)  : Boolean = (toInt & 0xffffffffL) >= x
	@inline def >=(x: Float) : Boolean = (toInt & 0xffffffffL).toFloat >= x
	@inline def >=(x: Double): Boolean = (toInt & 0xffffffffL).toDouble >= x

	@inline def compare(other: UInt): Int = jl.Integer.compare(toInt + MinValue, other.toInt + MinValue)

	@inline def min(other: UInt): UInt = new UInt(jl.Math.min(toInt + MinValue, other.toInt + MinValue) - MinValue)
	@inline def max(other: UInt): UInt = new UInt(jl.Math.max(toInt + MinValue, other.toInt + MinValue) - MinValue)

	/** Returns `this max other`. */
	@inline def atLeast(other :UInt) :UInt = if (toInt + MinValue >= other.toInt + MinValue) this else other

	/** Returns `this min other`. */
	@inline def atMost(other :UInt) :UInt = if (toInt + MinValue <= other.toInt + MinValue) this else other

	/** Returns this `UInt`, or `0` if the condition is false. */
	@inline def orZeroIf(condition :Boolean) :UInt = if (condition) new UInt(0) else this

	/** Returns this `UInt`, or `0` if it does not satisfy the predicate. */
	@inline def orZeroIf(condition :UInt => Boolean) :UInt = if (condition(this)) new UInt(0) else this

	@inline def |(x: Int)  : UInt  = new UInt(toInt | x)
	@inline def |(x: Long) : Long  = toInt & 0xffffffffL | x
	@inline def &(x: Int)  : UInt  = new UInt(toInt & x)
	@inline def &(x: Long) : Long  = toInt & 0xffffffffL & x
	@inline def ^(x: Int)  : UInt  = new UInt(toInt ^ x)
	@inline def ^(x: Long) : Long  = toInt & 0xffffffffL ^ x

	@inline def +(x: Char)  : UInt   = new UInt(toInt + x)
	@inline def +(x: Long)  : Long   = (toInt & 0xffffffffL) + x
	@inline def +(x: Float) : Float  = (toInt & 0xffffffffL) + x
	@inline def +(x: Double): Double = (toInt & 0xffffffffL) + x
	@inline def +(x: UInt)  : UInt   = new UInt(toInt + x.toInt)
	@inline def -(x: Char)  : UInt   = new UInt(toInt - x) //consider: making all subtractions return a signed integer
	@inline def -(x: Long)  : Long   = (toInt & 0xffffffffL) - x
	@inline def -(x: Float) : Float  = (toInt & 0xffffffffL) - x
	@inline def -(x: Double): Double = (toInt & 0xffffffffL) - x
	@inline def -(x: UInt)  : UInt   = new UInt(toInt - x.toInt)

	@inline def *(x: Char)  : UInt   = new UInt(toInt * x)
	@inline def *(x: Long)  : Long   = (toInt & 0xffffffffL) * x
	@inline def *(x: Float) : Float  = (toInt & 0xffffffffL) * x
	@inline def *(x: Double): Double = (toInt & 0xffffffffL) * x
	@inline def *(x: UInt)  : UInt   = new UInt(toInt * x.toInt)
	@inline def /(x: Char)  : UInt   = new UInt(((toInt & 0xffffffffL) / x).toInt)
	@inline def /(x: Long)  : Long   = (toInt & 0xffffffffL) / x
	@inline def /(x: Float) : Float  = (toInt & 0xffffffffL) / x
	@inline def /(x: Double): Double = (toInt & 0xffffffffL) / x
	@inline def /(x: UInt)  : UInt   = new UInt(((toInt & 0xffffffffL) / (x.toInt & 0xffffffffL)).toInt)
	@inline def %(x: Char)  : UInt   = new UInt(((toInt & 0xffffffffL) % x).toInt)
	@inline def %(x: Long)  : Long   = (toInt & 0xffffffffL) % x
	@inline def %(x: Float) : Float  = (toInt & 0xffffffffL) % x
	@inline def %(x: Double): Double = (toInt & 0xffffffffL) % x
	@inline def %(x: UInt)  : UInt   = new UInt(((toInt & 0xffffffffL) % (x.toInt & 0xffffffffL)).toInt)
	@inline def **(n: Int)  : UInt   = new UInt(toInt.pow(n))

	/** Divides this `UInt` by the argument, creating a [[net.noresttherein.sugar.numeric.Ratio Ratio]]
	  * number representing the result.
	  * @param denominator the denominator of the created rational (before reduction)
	  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
	  */
	@inline def %/(denominator: UInt): Ratio = Ratio(toInt & 0xffffffffL, denominator.toInt & 0xffffffffL)

	/** Divides this `UInt` by the argument, creating a [[net.noresttherein.sugar.numeric.Ratio Ratio]]
	  * number representing the result.
	  * @param denominator the denominator of the created rational (before reduction)
	  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
	  */
	@inline def %/(denominator: Long): Ratio = Ratio(toInt & 0xffffffffL, denominator)

	type ResultWithoutStep = NumericRange[UInt]
	@inline def to(end: UInt): NumericRange.Inclusive[UInt] =
		NumericRange.inclusive(this, end, new UInt(1))(UInt.UIntIsIntegral)

	@inline def to(end: UInt, step: UInt): NumericRange.Inclusive[UInt] =
		NumericRange.inclusive(this, end, step)(UInt.UIntIsIntegral)

	@inline def until(end: UInt): NumericRange.Exclusive[UInt] = NumericRange(this, end, new UInt(1))
	@inline def until(end: UInt, step: UInt): NumericRange.Exclusive[UInt] =
		NumericRange(this, end, step)

	@inline def in(range: NumericRange[UInt]): Boolean = range.containsTyped(this)


	private def underflow(method: String): Nothing =
		throw SugaredArithmeticException("Arithmetic underflow: " + this + "." + method + ".")

	private def outOfRange(typeName: String): Nothing =
		throw SugaredArithmeticException("Value " + this + " is out of " + typeName + " range.")
}




@SerialVersionUID(Ver)
object UInt {
	/** `2`^32^` - 1 == 4294967295` */
	final val MaxValue = new UInt(0xffffffff)
	/** Zero. */
	final val MinValue = new UInt(0)

	@throws[IllegalArgumentException]("If value is negative")
	@inline def apply(value: Int): UInt =
		if (value < 0) throwIllegalArgumentException(value)
		else new UInt(value)

	@throws[NumberFormatException]("if the string does not contain a parseable integer, or it is negative.")
	@inline def apply(string: String, radix: Int = 10): UInt = new UInt(jl.Integer.parseUnsignedInt(string, radix))

	@throws[ArithmeticException]("If value is negative")
	@inline def from(value: Int): UInt =
		if (value < 0) throwArithmeticException(value)
		else new UInt(value)

	@throws[NumberFormatException]("if the string does not contain a parseable integer, or it is negative.")
	def decode(string: String): UInt = {
		val long = jl.Long.decode(string)
		if (long < 0L | long > 0xffffffffL)
			throwNumberFormatException(string)
		new UInt(long.toInt)
	}

	def parse(string: String): Maybe[UInt] =
		Numeric.LongIsIntegral.parseString(string) match {
			case Some(long) if long >= 0L && long <= 0xffffffffL => Yes(new UInt(long.toInt))
			case _ => No
		}

	private def throwArithmeticException(value: Int): Nothing =
		throw SugaredArithmeticException("Value out of [0.." + MaxValue + "] range: " + value)

	private def throwIllegalArgumentException(value: Int): Nothing =
		illegal_!("Value out of [0.." + MaxValue + "] range: " + value)

	private def throwNumberFormatException(value: String): Nothing =
		throw SugaredNumberFormatException("Value out of [0.." + MaxValue + "] range: " + value)


	//todo: in Scala3 create conversions from non negative Int literals
//	@inline implicit def UIntToULong(number :UInt) :ULong = new ULong(number.asInt)

	@SerialVersionUID(Ver)
	object conversions {
		@inline implicit def UIntToInt(number: UInt): Int = number.toInt
		@inline implicit def IntToUInt(number: Int): UInt = new UInt(number)
		@inline implicit def checkedUIntToInt(number: UInt): Int = number.toIntExact
		@inline implicit def checkedIntToUInt(number: Int): UInt = UInt.from(number)
	}

	sealed abstract class UIntIsNumeric extends Numeric[UInt] {
		override def plus(x: UInt, y: UInt): UInt = x + y
		override def minus(x: UInt, y: UInt): UInt = x - y
		override def times(x: UInt, y: UInt): UInt = x * y
		override def negate(x: UInt): UInt =
			throw SugaredArithmeticException("Cannot negate an unsigned number " + x)

		override def fromInt(x: Int): UInt = UInt.from(x)
		override def parseString(str: String): Option[UInt] = UInt.parse(str).toOption
		override def toInt(x: UInt): Int = x.toInt
		override def toLong(x: UInt): Long = x.toLong
		override def toFloat(x: UInt): Float = x.toFloat
		override def toDouble(x: UInt): Double = x.toDouble
		override def compare(x: UInt, y: UInt): Int =
			jl.Integer.compare(x.toInt + Int.MinValue, y.toInt + Int.MinValue)
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

