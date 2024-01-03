package net.noresttherein.sugar.numeric

import java.math.{BigInteger, BigDecimal => JavaBigDecimal}
import java.{lang => jl}

import scala.collection.immutable.NumericRange
import scala.math.ScalaNumericAnyConversions

import net.noresttherein.sugar.numeric.extensions.IntExtension
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/** An unsigned 16 bit integer backed by an `Short` value. All comparisons are done as if unsigned, and `toString`
  * and other formatting methods treat the underlying `Short` as unsigned.
  * However, this type doesn't check for overflows and underflows, meaning `UShort(1) - UShort(2)`
  * will return `UShort.MaxValue`.
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
class UShort private[numeric](override val toShort: Short)
	extends AnyVal with Ordered[UShort] with ScalaNumericAnyConversions with Serializable
{
	@inline override def isWhole     : Boolean = true
	@inline override def isValidByte : Boolean = (toShort & 0x7f) == toShort
	@inline override def isValidShort: Boolean = (toShort & 0x7fff) == toShort
	@inline override def isValidChar : Boolean = true
	@inline override def isValidInt  : Boolean = true

	@inline override def byteValue  : Byte    = toShort.toByte
	@inline override def shortValue : Short   = toShort
	@inline override def intValue   : Int     = toShort.toInt
	@inline override def longValue  : Long    = toShort & 0xffffL
	@inline override def floatValue : Float   = (toShort & 0xffffL).toFloat
	@inline override def doubleValue: Double  = (toShort & 0xffffL).toDouble
	@inline def charValue: Char = toShort.toChar

	@inline override def toByte  : Byte   = toShort.toByte
	@inline override def toInt   : Int    = toShort.toInt
	@inline override def toLong  : Long   = toShort.toLong
	@inline override def toFloat : Float  = toShort.toLong.toFloat
	@inline override def toDouble: Double = toShort.toLong.toDouble

	@inline def toByteExact : Byte   = if ((toShort & 0x7f) != toShort) outOfRange("Byte") else toShort.toByte
	@inline def toShortExact: Short  = if ((toShort & 0x7fff) != toShort) outOfRange("Short") else toShort
	@inline def toCharExact : Char   = toShort.toChar

	/** Returns `toInt != 0`. */
	@inline def toBoolean       : Boolean        = toShort != 0
	@inline def toBigInt        : BigInt         = BigInt(toShort & 0xffffL)
	@inline def toBigInteger    : BigInteger     = BigInteger.valueOf(toShort & 0xffffL)
	@inline def toBigDecimal    : BigDecimal     = BigDecimal(toShort & 0xffffL)
	@inline def toJavaBigDecimal: JavaBigDecimal = JavaBigDecimal.valueOf(toShort & 0xffffL)

	@inline def toDecimal64: Decimal64 = Decimal64(toShort & 0xffff)
	@inline def toUInt     : UInt      = new UInt(toShort & 0xffff)
	@inline def toULong    : ULong     = new ULong(toShort & 0xffffL)
	@inline def toSafeInt  : SafeInt   = new SafeInt(toShort & 0xffff)
	@inline def toSafeLong : SafeLong  = new SafeLong(toShort & 0xffffL)
	@inline def toIntRatio : IntRatio  = IntRatio(toShort & 0xffff)
	@inline def toRatio    : Ratio     = Ratio(toShort & 0xffffL)

	@inline override def toString   : String = String.valueOf(toShort & 0xffff)
	@inline def toString(radix: Int): String = jl.Integer.toString(toShort & 0xffff, radix)
	@inline def toBinaryString      : String = jl.Integer.toBinaryString(toShort & 0xffff)
	@inline def toOctalString       : String = jl.Integer.toOctalString(toShort & 0xffff)
	@inline def toHexString         : String = jl.Integer.toHexString(toShort & 0xffff)

	@deprecated("Adding a number and a String is deprecated. Use the string interpolation `s\"$num$str\"`", "Scala 2.13.0")
	@inline def +(x: String): String = toShort.toString + x

	//Consider: the problem with all these comparisons is that they exclude UShort, which is handled by methods
	// inherited from Ordered. This works, because the erased signature is >(x :Object).
	// Unfortunately, it also means that it boxes both operands.
	// We may migrate to extension methods, but they won't work for == and !=.
	@inline def ==(x: Byte)  : Boolean = (toShort & 0xffff) == x
	@inline def ==(x: Short) : Boolean = (toShort & 0xffff) == x
	@inline def ==(x: Char)  : Boolean = toShort == x
	@inline def ==(x: Int)   : Boolean = (toShort & 0xffff) == x
	@inline def ==(x: Long)  : Boolean = (toShort & 0xffffL) == x
	@inline def ==(x: Float) : Boolean = (toShort & 0xffffL).toFloat == x
	@inline def ==(x: Double): Boolean = (toShort & 0xffffL).toDouble == x
	@inline def !=(x: Byte)  : Boolean = (toShort & 0xffff) != x
	@inline def !=(x: Short) : Boolean = (toShort & 0xffff) != x
	@inline def !=(x: Char)  : Boolean = (toShort & 0xffff) != x
	@inline def !=(x: Int)   : Boolean = (toShort & 0xffff) != x
	@inline def !=(x: Long)  : Boolean = (toShort & 0xffffL) != x
	@inline def !=(x: Float) : Boolean = (toShort & 0xffffL).toFloat != x
	@inline def !=(x: Double): Boolean = (toShort & 0xffffL).toDouble != x

	@inline def < (x: Byte)  : Boolean = (toShort & 0xffff) < x
	@inline def < (x: Short) : Boolean = (toShort & 0xffff) < x
	@inline def < (x: Char)  : Boolean = (toShort & 0xffff) < x
	@inline def < (x: Int)   : Boolean = (toShort & 0xffff) < x
	@inline def < (x: Long)  : Boolean = (toShort & 0xffffL) < x
	@inline def < (x: Float) : Boolean = (toShort & 0xffffL).toFloat < x
	@inline def < (x: Double): Boolean = (toShort & 0xffffL).toDouble < x
	@inline def <=(x: Byte)  : Boolean = (toShort & 0xffff) <= x
	@inline def <=(x: Short) : Boolean = (toShort & 0xffff) <= x
	@inline def <=(x: Char)  : Boolean = (toShort & 0xffff) <= x
	@inline def <=(x: Int)   : Boolean = (toShort & 0xffff) <= x
	@inline def <=(x: Long)  : Boolean = (toShort & 0xffffL) <= x
	@inline def <=(x: Float) : Boolean = (toShort & 0xffffL).toFloat <= x
	@inline def <=(x: Double): Boolean = (toShort & 0xffffL).toDouble <= x
	@inline def > (x: Byte)  : Boolean = (toShort & 0xffff) > x
	@inline def > (x: Short) : Boolean = (toShort & 0xffff) > x
	@inline def > (x: Char)  : Boolean = (toShort & 0xffff) > x
	@inline def > (x: Int)   : Boolean = (toShort & 0xffff) > x
	@inline def > (x: Long)  : Boolean = (toShort & 0xffffL) > x
	@inline def > (x: Float) : Boolean = (toShort & 0xffffL).toFloat > x
	@inline def > (x: Double): Boolean = (toShort & 0xffffL).toDouble > x
	@inline def >=(x: Byte)  : Boolean = (toShort & 0xffff) >= x
	@inline def >=(x: Short) : Boolean = (toShort & 0xffff) >= x
	@inline def >=(x: Char)  : Boolean = (toShort & 0xffff) >= x
	@inline def >=(x: Int)   : Boolean = (toShort & 0xffff) >= x
	@inline def >=(x: Long)  : Boolean = (toShort & 0xffffL) >= x
	@inline def >=(x: Float) : Boolean = (toShort & 0xffffL).toFloat >= x
	@inline def >=(x: Double): Boolean = (toShort & 0xffffL).toDouble >= x

	@inline def compare(other: UShort): Int = jl.Integer.compare(toShort & 0xffff, other.toShort & 0xffff)

	@inline def min(other: UShort): UShort = new UShort(jl.Math.min(toShort & 0xffff, other.toShort & 0xffff).toShort)
	@inline def max(other: UShort): UShort = new UShort(jl.Math.max(toShort & 0xffff, other.toShort & 0xffff).toShort)

	/** Returns `this max other`. */
	@inline def atLeast(other :UShort) :UShort = new UShort(jl.Math.max(toShort & 0xffff, other.toShort & 0xffff).toShort)

	/** Returns `this min other`. */
	@inline def atMost(other :UShort) :UShort = new UShort(jl.Math.min(toShort & 0xffff, other.toShort & 0xffff).toShort)

	/** Returns this `UShort`, or `0` if the condition is false. */
	@inline def orZeroIf(condition :Boolean) :UShort = if (condition) new UShort(0) else this

	/** Returns this `UInt`, or `0` if it does not satisfy the predicate. */
	@inline def orZeroIf(condition :UShort => Boolean) :UShort = if (condition(this)) new UShort(0) else this

	@inline def +(x: Char)  : UShort = new UShort(((toShort & 0xffff) + x).toShort)
	@inline def +(x: Int)   : Int    = (toShort & 0xffff) + x
	@inline def +(x: Long)  : Long   = (toShort & 0xffffL) + x
	@inline def +(x: Float) : Float  = (toShort & 0xffffL) + x
	@inline def +(x: Double): Double = (toShort & 0xffffL) + x
	@inline def +(x: UShort): UShort = new UShort(((toShort & 0xffff) + (x.toShort & 0xffff)).toShort)
//	@inline def +(x: UInt)  : UInt   = new UInt((toShort & 0xffff) + x.toInt)
	@inline def -(x: Char)  : UShort = new UShort(((toShort & 0xffff) - x).toShort)
	@inline def -(x: Int)   : Int    = (toShort & 0xffff) - x
	@inline def -(x: Long)  : Long   = (toShort & 0xffffL) - x
	@inline def -(x: Float) : Float  = (toShort & 0xffffL) - x
	@inline def -(x: Double): Double = (toShort & 0xffffL) - x
	@inline def -(x: UShort): UShort = new UShort((toShort - x.toShort).toShort)

	@inline def *(x: Char)  : UShort = new UShort(((toShort & 0xffff) * x).toShort)
	@inline def *(x: Int)   : Int    = (toShort & 0xffff) * x
	@inline def *(x: Long)  : Long   = (toShort & 0xffffL) * x
	@inline def *(x: Float) : Float  = (toShort & 0xffffL) * x
	@inline def *(x: Double): Double = (toShort & 0xffffL) * x
	@inline def *(x: UShort): UShort = new UShort((toShort * x.toShort).toShort)
	@inline def /(x: Char)  : UShort = new UShort(((toShort & 0xffff) / x).toShort)
	@inline def /(x: Int)   : Int    = (toShort & 0xffff) / x
	@inline def /(x: Long)  : Long   = (toShort & 0xffffL) / x
	@inline def /(x: Float) : Float  = (toShort & 0xffffL) / x
	@inline def /(x: Double): Double = (toShort & 0xffffL) / x
	@inline def /(x: UShort): UShort = new UShort(((toShort & 0xffff) / (toShort & 0xffff)).toShort)
	@inline def %(x: Char)  : UShort = new UShort(((toShort & 0xffff) % x).toShort)
	@inline def %(x: Int)   : Int    = (toShort & 0xffff) % x
	@inline def %(x: Long)  : Long   = (toShort & 0xffffL) % x
	@inline def %(x: Float) : Float  = (toShort & 0xffffL) % x
	@inline def %(x: Double): Double = (toShort & 0xffffL) % x
	@inline def %(x: UShort): UShort = new UShort(((toShort & 0xffff) % (toShort & 0xffff)).toShort)
	@inline def **(n: Int)  : UInt   = new UInt((toShort & 0xffff).pow(n))

	/** Divides this `UShort` by the argument, creating a [[net.noresttherein.sugar.numeric.Ratio Ratio]]
	  * number representing the result.
	  * @param denominator the denominator of the created rational (before reduction)
	  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
	  */
	@inline def %/(denominator :UShort) :Ratio = Ratio(toShort & 0xffffL, denominator.toShort & 0xffffL)

	/** Divides this `UShort` by the argument, creating a [[net.noresttherein.sugar.numeric.Ratio Ratio]]
	  * number representing the result.
	  * @param denominator the denominator of the created rational (before reduction)
	  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
	  */
	@inline def %/(denominator :Int) :Ratio = Ratio(toShort & 0xffffL, denominator)

	/** Divides this `UShort` by the argument, creating a [[net.noresttherein.sugar.numeric.Ratio Ratio]]
	  * number representing the result.
	  * @param denominator the denominator of the created rational (before reduction)
	  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
	  */
	@inline def %/(denominator :Long) :Ratio = Ratio(toShort & 0xffffL, denominator)

	type ResultWithoutStep = NumericRange[UShort]
	@inline def to(end: UShort): NumericRange.Inclusive[UShort] =
		NumericRange.inclusive(this, end, new UShort(1))(UShort.UShortIsIntegral)

	@inline def to(end: UShort, step: UShort): NumericRange.Inclusive[UShort] =
		NumericRange.inclusive(this, end, step)(UShort.UShortIsIntegral)

	@inline def until(end: UShort): NumericRange.Exclusive[UShort] = NumericRange(this, end, new UShort(1))
	@inline def until(end: UShort, step: UShort): NumericRange.Exclusive[UShort] =
		NumericRange(this, end, step)

	@inline def in(range: NumericRange[UShort]): Boolean = range.containsTyped(this)


//	private def underflow(method :String) :Nothing =
//		throw new ArithmeticException("Arithmetic underflow: " + this + "." + method + ".")

	private def outOfRange(typeName :String) :Nothing =
		throw new ArithmeticException("Value " + this + " is out of " + typeName + " range.")

//	@inline private def testRange(max :Int, typeName :String) :Unit =
//		if (toInt + MinValue > max + MinValue)
//			outOfRange(typeName)
}




@SerialVersionUID(Ver)
object UShort {
	/** `2`^16^` - 1 == 65535` */
	final val MaxValue = new UShort(-1)
	/** Zero. */
	final val MinValue = new UShort(0)

	@throws[IllegalArgumentException]("If value is negative")
	@inline def apply(value: Short): UShort =
		if (value < 0) throwIllegalArgumentException(value)
		else new UShort(value)

	@throws[NumberFormatException]("if the string does not contain a parseable UShort.")
	@inline def apply(string: String, radix: Int = 10): UShort = {
		val int = jl.Integer.parseInt(string, radix)
		if (int < 0 | int > 0xff) throwNumberFormatException("\"" + string + "\" is out of range for UShort.")
		else new UShort(int.toShort)
	}

	@throws[ArithmeticException]("If value is negative or greater than 2^16-1.")
	@inline def from(value: Int): UShort =
		if (value < 0 | value > 0xffff) throwArithmeticException(value)
		else new UShort(value.toShort)

	@throws[NumberFormatException]("if the string does not contain a parseable UShort.")
	def decode(string: String): UShort = {
		val long = jl.Long.decode(string)
		if (long < 0L | long > 0xffffL)
			throwNumberFormatException(string)
		new UShort(long.toShort)
	}

	def parse(string: String): Opt[UShort] =
		Numeric.IntIsIntegral.parseString(string) match {
			case Some(int) if int >= 0 & int <= 0xffff => Got(new UShort(int.toShort))
			case _ => Lack
		}

	private def throwArithmeticException(value: Int): Nothing =
		throw new ArithmeticException("Value out of [0.." + MaxValue + "] range: " + value)

	private def throwIllegalArgumentException(value: Int): Nothing =
		throw new IllegalArgumentException("Value out of [0.." + MaxValue + "] range: " + value)

	private def throwNumberFormatException(value: String): Nothing =
		throw new NumberFormatException("Value out of [0.." + MaxValue + "] range: " + value)


	//todo: in Scala3 create conversions from non negative Int literals
	@inline implicit def UShortToInt(number: UShort)  : Int = number.toShort & 0xffff
	@inline implicit def UShortToUInt(number: UShort) : UInt = new UInt(number.toShort & 0xffff)
	@inline implicit def UShortToLong(number: UShort) : Long = number.toShort & 0xffffL
	@inline implicit def UShortToULong(number: UShort): ULong = new ULong(number.toShort & 0xffffL)

	@SerialVersionUID(Ver)
	object conversions {
		@inline implicit def UShortToShort(number: UShort): Short = number.toShort
		@inline implicit def ShortToUShort(number: Short): UShort = new UShort(number)
		@inline implicit def checkedUShortToShort(number: UShort): Short = number.toShortExact
		@inline implicit def checkedShortToUShort(number: Short): UShort = UShort.from(number)
	}

	sealed abstract class UShortIsNumeric extends Numeric[UShort] {
		override def plus(x: UShort, y: UShort): UShort = x + y
		override def minus(x: UShort, y: UShort): UShort = x - y
		override def times(x: UShort, y: UShort): UShort = x * y
		override def negate(x: UShort): UShort =
			throw new ArithmeticException("Cannot negate an unsigned number " + x)

		override def fromInt(x: Int): UShort = UShort.from(x)
		override def parseString(str: String): Option[UShort] = UShort.parse(str).toOption
		override def toInt(x: UShort): Int = x.toInt
		override def toLong(x: UShort): Long = x.toLong
		override def toFloat(x: UShort): Float = x.toFloat
		override def toDouble(x: UShort): Double = x.toDouble
		override def compare(x: UShort, y: UShort): Int =
			jl.Integer.compare(x.toShort & 0xffff, y.toShort & 0xffff)
	}
	@SerialVersionUID(Ver)
	implicit object UShortIsIntegral extends UShortIsNumeric with Integral[UShort] {
		override def quot(x: UShort, y: UShort): UShort = x / y
		override def rem(x: UShort, y: UShort): UShort = x % y
	}
	@SerialVersionUID(Ver)
	object UShortAsIfFractional extends UShortIsNumeric with Fractional[UShort] {
		override def div(x: UShort, y: UShort): UShort = x / y
	}
}

