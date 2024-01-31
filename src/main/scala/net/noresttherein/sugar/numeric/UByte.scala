package net.noresttherein.sugar.numeric

import java.math.{BigInteger, BigDecimal => JavaBigDecimal}
import java.{lang => jl}

import scala.collection.immutable.NumericRange
import scala.math.ScalaNumericAnyConversions

import net.noresttherein.sugar.exceptions.{SugaredArithmeticException, SugaredNumberFormatException}
import net.noresttherein.sugar.illegal_!
import net.noresttherein.sugar.numeric.extensions.IntExtension
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




/** An unsigned 16 bit integer backed by an `Short` value. All comparisons are done as if unsigned, and `toString`
  * and other formatting methods treat the underlying `Short` as unsigned.
  * However, this type doesn't check for overflows and underflows, meaning `UByte(1) - UByte(2)`
  * will return `UByte.MaxValue`.
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
class UByte private[numeric](override val toByte: Byte)
	extends AnyVal with Ordered[UByte] with ScalaNumericAnyConversions with Serializable
{
	@inline override def isWhole     : Boolean = true
	@inline override def isValidByte : Boolean = (toByte & 0x7f) == toByte
	@inline override def isValidShort: Boolean = toByte >= 0
	@inline override def isValidChar : Boolean = true
	@inline override def isValidInt  : Boolean = true

	@inline override def byteValue  : Byte    = toByte
	@inline override def shortValue : Short   = toByte.toShort
	@inline override def intValue   : Int     = toByte.toInt
	@inline override def longValue  : Long    = toByte & 0xffL
	@inline override def floatValue : Float   = (toByte & 0xffL).toFloat
	@inline override def doubleValue: Double  = (toByte & 0xffL).toDouble
	@inline def charValue: Char = toByte.toChar

	@inline override def toShort : Short  = toByte.toShort
	@inline override def toInt   : Int    = toByte.toInt
	@inline override def toLong  : Long   = toByte.toLong
	@inline override def toFloat : Float  = toByte.toLong.toFloat
	@inline override def toDouble: Double = toByte.toLong.toDouble

	@inline def toByteExact : Byte  = if ((toByte & 0x7f) != toByte) outOfRange("Byte") else toByte

	/** Returns `toInt != 0`. */
	@inline def toBoolean       : Boolean        = toByte != 0
	@inline def toBigInt        : BigInt         = BigInt(toByte & 0xffL)
	@inline def toBigInteger    : BigInteger     = BigInteger.valueOf(toByte & 0xffL)
	@inline def toBigDecimal    : BigDecimal     = BigDecimal(toByte & 0xffL)
	@inline def toJavaBigDecimal: JavaBigDecimal = JavaBigDecimal.valueOf(toByte & 0xffL)

	@inline def toDecimal64: Decimal64 = Decimal64(toByte & 0xff)
	@inline def toUInt     : UInt      = new UInt(toByte & 0xff)
	@inline def toULong    : ULong     = new ULong(toByte & 0xffL)
	@inline def toSafeInt  : SafeInt   = new SafeInt(toByte & 0xff)
	@inline def toSafeLong : SafeLong  = new SafeLong(toByte & 0xffL)
	@inline def toIntRatio : IntRatio  = IntRatio(toByte & 0xff)
	@inline def toRatio    : Ratio     = Ratio(toByte & 0xffL)

	@inline override def toString   : String = String.valueOf(toByte & 0xff)
	@inline def toString(radix: Int): String = jl.Integer.toString(toByte & 0xff, radix)
	@inline def toBinaryString      : String = jl.Integer.toBinaryString(toByte & 0xff)
	@inline def toOctalString       : String = jl.Integer.toOctalString(toByte & 0xff)
	@inline def toHexString         : String = jl.Integer.toHexString(toByte & 0xff)

	@deprecated("Adding a number and a String is deprecated. Use the string interpolation `s\"$num$str\"`", "Scala 2.13.0")
	@inline def +(x: String): String = toByte.toString + x

	//Consider: the problem with all these comparisons is that they exclude UByte, which is handled by methods
	// inherited from Ordered. This works, because the erased signature is >(x :Object).
	// Unfortunately, it also means that it boxes both operands.
	// We may migrate to extension methods, but they won't work for == and !=.
	@inline def ==(x: Byte)  : Boolean = (toByte & 0xff) == x
	@inline def ==(x: Short) : Boolean = (toByte & 0xff) == x
	@inline def ==(x: Char)  : Boolean = (toByte & 0xff) == x
	@inline def ==(x: Int)   : Boolean = (toByte & 0xff) == x
	@inline def ==(x: Long)  : Boolean = (toByte & 0xffL) == x
	@inline def ==(x: Float) : Boolean = (toByte & 0xffL).toFloat == x
	@inline def ==(x: Double): Boolean = (toByte & 0xffL).toDouble == x
	@inline def !=(x: Byte)  : Boolean = (toByte & 0xff) != x
	@inline def !=(x: Short) : Boolean = (toByte & 0xff) != x
	@inline def !=(x: Char)  : Boolean = (toByte & 0xff) != x
	@inline def !=(x: Int)   : Boolean = (toByte & 0xff) != x
	@inline def !=(x: Long)  : Boolean = (toByte & 0xffL) != x
	@inline def !=(x: Float) : Boolean = (toByte & 0xffL).toFloat != x
	@inline def !=(x: Double): Boolean = (toByte & 0xffL).toDouble != x

	@inline def < (x: Byte)  : Boolean = (toByte & 0xff) < x
	@inline def < (x: Short) : Boolean = (toByte & 0xff) < x
	@inline def < (x: Char)  : Boolean = (toByte & 0xff) < x
	@inline def < (x: Int)   : Boolean = (toByte & 0xff) < x
	@inline def < (x: Long)  : Boolean = (toByte & 0xffL) < x
	@inline def < (x: Float) : Boolean = (toByte & 0xffL).toFloat < x
	@inline def < (x: Double): Boolean = (toByte & 0xffL).toDouble < x
	@inline def <=(x: Byte)  : Boolean = (toByte & 0xff) <= x
	@inline def <=(x: Short) : Boolean = (toByte & 0xff) <= x
	@inline def <=(x: Char)  : Boolean = (toByte & 0xff) <= x
	@inline def <=(x: Int)   : Boolean = (toByte & 0xff) <= x
	@inline def <=(x: Long)  : Boolean = (toByte & 0xffL) <= x
	@inline def <=(x: Float) : Boolean = (toByte & 0xffL).toFloat <= x
	@inline def <=(x: Double): Boolean = (toByte & 0xffL).toDouble <= x
	@inline def > (x: Byte)  : Boolean = (toByte & 0xff) > x
	@inline def > (x: Short) : Boolean = (toByte & 0xff) > x
	@inline def > (x: Char)  : Boolean = (toByte & 0xff) > x
	@inline def > (x: Int)   : Boolean = (toByte & 0xff) > x
	@inline def > (x: Long)  : Boolean = (toByte & 0xffL) > x
	@inline def > (x: Float) : Boolean = (toByte & 0xffL).toFloat > x
	@inline def > (x: Double): Boolean = (toByte & 0xffL).toDouble > x
	@inline def >=(x: Byte)  : Boolean = (toByte & 0xff) >= x
	@inline def >=(x: Short) : Boolean = (toByte & 0xff) >= x
	@inline def >=(x: Char)  : Boolean = (toByte & 0xff) >= x
	@inline def >=(x: Int)   : Boolean = (toByte & 0xff) >= x
	@inline def >=(x: Long)  : Boolean = (toByte & 0xffL) >= x
	@inline def >=(x: Float) : Boolean = (toByte & 0xffL).toFloat >= x
	@inline def >=(x: Double): Boolean = (toByte & 0xffL).toDouble >= x

	@inline def compare(other: UByte): Int = jl.Integer.compare(toByte & 0xff, other.toByte & 0xff)

	@inline def min(other: UByte): UByte = new UByte(jl.Math.min(toByte & 0xff, other.toByte & 0xff).toByte)
	@inline def max(other: UByte): UByte = new UByte(jl.Math.max(toByte & 0xff, other.toByte & 0xff).toByte)

	/** Returns `this max other`. */
	@inline def atLeast(other :UByte) :UByte = new UByte(jl.Math.max(toByte & 0xff, other.toByte & 0xff).toByte)

	/** Returns `this min other`. */
	@inline def atMost(other :UByte) :UByte = new UByte(jl.Math.min(toByte & 0xff, other.toByte & 0xff).toByte)

	/** Returns this `UByte`, or `0` if the condition is false. */
	@inline def orZeroIf(condition :Boolean) :UByte = if (condition) new UByte(0) else this

	/** Returns this `UInt`, or `0` if it does not satisfy the predicate. */
	@inline def orZeroIf(condition :UByte => Boolean) :UByte = if (condition(this)) new UByte(0) else this

//	@inline def +(x: Char)  : UByte = new UByte(((toByte & 0xff) + x).toByte)
	@inline def +(x: Short) : Short  = ((toByte & 0xff) + x).toShort
	@inline def +(x: Int)   : Int    = (toByte & 0xff) + x
	@inline def +(x: Long)  : Long   = (toByte & 0xffL) + x
	@inline def +(x: Float) : Float  = (toByte & 0xffL) + x
	@inline def +(x: Double): Double = (toByte & 0xffL) + x
	@inline def +(x: UByte) : UByte  = new UByte(((toByte & 0xff) + (x.toByte & 0xff)).toByte)
//	@inline def +(x: UInt)  : UInt   = new UInt((toByte & 0xff) + x.toInt)
//	@inline def -(x: Char)  : UByte = new UByte(((toByte & 0xff) - x).toByte)
	@inline def -(x: Short) : Short  = ((toByte & 0xff) - x).toShort
	@inline def -(x: Int)   : Int    = (toByte & 0xff) - x
	@inline def -(x: Long)  : Long   = (toByte & 0xffL) - x
	@inline def -(x: Float) : Float  = (toByte & 0xffL) - x
	@inline def -(x: Double): Double = (toByte & 0xffL) - x
	@inline def -(x: UByte): UByte   = new UByte((toByte - x.toByte).toByte)

//	@inline def *(x: Char)  : UByte = new UByte(((toByte & 0xff) * x).toByte)
	@inline def *(x: Short) : Short  = ((toByte & 0xff) * x).toShort
	@inline def *(x: Int)   : Int    = (toByte & 0xff) * x
	@inline def *(x: Long)  : Long   = (toByte & 0xffL) * x
	@inline def *(x: Float) : Float  = (toByte & 0xffL) * x
	@inline def *(x: Double): Double = (toByte & 0xffL) * x
	@inline def *(x: UByte) : UByte  = new UByte((toByte * x.toByte).toByte)
//	@inline def /(x: Char)  : UByte  = new UByte(((toByte & 0xff) / x).toByte)
	@inline def /(x: Short) : Short  = ((toByte & 0xff) / x).toShort
	@inline def /(x: Int)   : Int    = (toByte & 0xff) / x
	@inline def /(x: Long)  : Long   = (toByte & 0xffL) / x
	@inline def /(x: Float) : Float  = (toByte & 0xffL) / x
	@inline def /(x: Double): Double = (toByte & 0xffL) / x
	@inline def /(x: UByte) : UByte  = new UByte(((toByte & 0xff) / (toByte & 0xff)).toByte)
//	@inline def %(x: Char)  : UByte = new UByte(((toByte & 0xff) % x).toByte)
	@inline def %(x: Short) : Short  = ((toByte & 0xff) % x).toShort
	@inline def %(x: Int)   : Int    = (toByte & 0xff) % x
	@inline def %(x: Long)  : Long   = (toByte & 0xffL) % x
	@inline def %(x: Float) : Float  = (toByte & 0xffL) % x
	@inline def %(x: Double): Double = (toByte & 0xffL) % x
	@inline def %(x: UByte) : UByte  = new UByte(((toByte & 0xff) % (toByte & 0xff)).toByte)
	@inline def **(n: Int)  : UInt   = new UInt((toByte & 0xff).pow(n))

	/** Divides this `UByte` by the argument, creating a [[net.noresttherein.sugar.numeric.Ratio Ratio]]
	  * number representing the result.
	  * @param denominator the denominator of the created rational (before reduction)
	  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
	  */
	@inline def %/(denominator :UByte) :Ratio = Ratio(toByte & 0xffL, denominator.toByte & 0xffL)

	/** Divides this `UByte` by the argument, creating a [[net.noresttherein.sugar.numeric.Ratio Ratio]]
	  * number representing the result.
	  * @param denominator the denominator of the created rational (before reduction)
	  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
	  */
	@inline def %/(denominator :UShort) :Ratio = Ratio(toByte & 0xffL, denominator.toShort & 0xffffL)

	/** Divides this `UByte` by the argument, creating a [[net.noresttherein.sugar.numeric.Ratio Ratio]]
	  * number representing the result.
	  * @param denominator the denominator of the created rational (before reduction)
	  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
	  */
	@inline def %/(denominator :Int) :Ratio = Ratio(toByte & 0xffL, denominator)

	/** Divides this `UByte` by the argument, creating a [[net.noresttherein.sugar.numeric.Ratio Ratio]]
	  * number representing the result.
	  * @param denominator the denominator of the created rational (before reduction)
	  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
	  */
	@inline def %/(denominator :Long) :Ratio = Ratio(toByte & 0xffL, denominator)

	type ResultWithoutStep = NumericRange[UByte]
	@inline def to(end: UByte): NumericRange.Inclusive[UByte] =
		NumericRange.inclusive(this, end, new UByte(1))(UByte.UByteIsIntegral)

	@inline def to(end: UByte, step: UByte): NumericRange.Inclusive[UByte] =
		NumericRange.inclusive(this, end, step)(UByte.UByteIsIntegral)

	@inline def until(end: UByte): NumericRange.Exclusive[UByte] = NumericRange(this, end, new UByte(1))
	@inline def until(end: UByte, step: UByte): NumericRange.Exclusive[UByte] =
		NumericRange(this, end, step)

	@inline def in(range: NumericRange[UByte]): Boolean = range.containsTyped(this)


//	private def underflow(method :String) :Nothing =
//		throw SugaredArithmeticException("Arithmetic underflow: " + this + "." + method + ".")

	private def outOfRange(typeName :String) :Nothing =
		throw SugaredArithmeticException("Value " + this + " is out of " + typeName + " range.")

//	@inline private def testRange(max :Int, typeName :String) :Unit =
//		if (toInt + MinValue > max + MinValue)
//			outOfRange(typeName)
}




@SerialVersionUID(Ver)
object UByte {
	/** `2`^8^` - 1 == 255` */
	final val MaxValue = new UByte(-1)
	/** Zero. */
	final val MinValue = new UByte(0)

	@throws[IllegalArgumentException]("If value is negative")
	@inline def apply(value: Byte): UByte =
		if (value < 0) throwIllegalArgumentException(value)
		else new UByte(value)

	@throws[NumberFormatException]("if the string does not contain a parsable UByte.")
	@inline def apply(string: String, radix: Int = 10): UByte = {
		val int = jl.Integer.parseInt(string, radix)
		if (int < 0 | int > 0xff) throwNumberFormatException("\"" + string + "\" is out of range for UByte.")
		else new UByte(int.toByte)
	}

	@throws[ArithmeticException]("If value is negative or greater than 2^16-1.")
	@inline def from(value: Int): UByte =
		if (value < 0 | value > 0xff) throwArithmeticException(value)
		else new UByte(value.toByte)

	@throws[NumberFormatException]("if the string does not contain a parsable UByte.")
	def decode(string: String): UByte = {
		val long = jl.Long.decode(string)
		if (long < 0L | long > 0xffL)
			throwNumberFormatException(string)
		new UByte(long.toByte)
	}

	def parse(string: String): Maybe[UByte] =
		Numeric.IntIsIntegral.parseString(string) match {
			case Some(int) if int >= 0 & int <= 0xff => Yes(new UByte(int.toByte))
			case _ => No
		}

	private def throwArithmeticException(value: Int): Nothing =
		throw SugaredArithmeticException("Value out of [0.." + MaxValue + "] range: " + value)

	private def throwIllegalArgumentException(value: Int): Nothing =
		illegal_!("Value out of [0.." + MaxValue + "] range: " + value)

	private def throwNumberFormatException(value: String): Nothing =
		throw SugaredNumberFormatException("Value out of [0.." + MaxValue + "] range: " + value)


	//todo: in Scala3 create conversions from non negative Int literals
	@inline implicit def UByteToShort(number: UByte) : Short  = (number.toByte & 0xff).toShort
	@inline implicit def UByteToUShort(number: UByte): UShort = new UShort((number.toByte & 0xff).toShort)
	@inline implicit def UByteToInt(number: UByte)   : Int    = number.toByte & 0xff
	@inline implicit def UByteToUInt(number: UByte)  : UInt   = new UInt(number.toByte & 0xff)
	@inline implicit def UByteToLong(number: UByte)  : Long   = number.toByte & 0xffL
	@inline implicit def UByteToULong(number: UByte) : ULong  = new ULong(number.toByte & 0xffL)

	@SerialVersionUID(Ver)
	object conversions {
		@inline implicit def UByteToByte(number: UByte): Byte  = number.toByte
		@inline implicit def ByteToUByte(number: Byte) : UByte = new UByte(number)
		@inline implicit def checkedUByteToByte(number: UByte): Byte  = number.toByteExact
		@inline implicit def checkedByteToUByte(number: Byte) : UByte = UByte.from(number)
	}

	sealed abstract class UByteIsNumeric extends Numeric[UByte] {
		override def plus(x: UByte, y: UByte): UByte = x + y
		override def minus(x: UByte, y: UByte): UByte = x - y
		override def times(x: UByte, y: UByte): UByte = x * y
		override def negate(x: UByte): UByte =
			throw SugaredArithmeticException("Cannot negate an unsigned number " + x)

		override def fromInt(x: Int): UByte = UByte.from(x)
		override def parseString(str: String): Option[UByte] = UByte.parse(str).toOption
		override def toInt(x: UByte): Int = x.toInt
		override def toLong(x: UByte): Long = x.toLong
		override def toFloat(x: UByte): Float = x.toFloat
		override def toDouble(x: UByte): Double = x.toDouble
		override def compare(x: UByte, y: UByte): Int =
			jl.Integer.compare(x.toByte & 0xff, y.toByte & 0xff)
	}
	@SerialVersionUID(Ver)
	implicit object UByteIsIntegral extends UByteIsNumeric with Integral[UByte] {
		override def quot(x: UByte, y: UByte): UByte = x / y
		override def rem(x: UByte, y: UByte): UByte = x % y
	}
	@SerialVersionUID(Ver)
	object UByteAsIfFractional extends UByteIsNumeric with Fractional[UByte] {
		override def div(x: UByte, y: UByte): UByte = x / y
	}
}

