package net.noresttherein.slang.numeric

import java.{lang => jl}






/** An `Int` value checking for overflows, underflows and loss of precision in all operations.
  * Unless stated otherwise, all methods returning another number type, with the exception of bitwise operations,
  * can throw an [[ArithmeticException]] .
  * @author Marcin Mościcki
  */
@SerialVersionUID(1L)
class SafeInt(val toInt :Int) extends AnyVal with Serializable {

	def toByte  : Byte   = { testRange(Byte.MinValue, Byte.MaxValue, "Byte"); toLong.toByte }
	def toShort : Short  = { testRange(Short.MinValue, Short.MaxValue, "Short"); toLong.toShort }
	def toChar  : Char   = { testRange(Char.MinValue, Char.MaxValue, "Char"); toLong.toChar }
	def toLong  : Long   = toInt.toLong
	def toFloat : Float  =
		{ val res = toInt.toFloat; if (res.toInt != toInt) outOfPrecision("Float"); res }
	def toDouble: Double =
		{ val res = toInt.toDouble; if (res.toInt != toInt) outOfPrecision("Double"); res }

	@inline def unary_~ : SafeInt = new SafeInt(~toInt)
	@inline def unary_+ : SafeInt = this
	@inline def unary_- : SafeInt =
		{ if (toInt == Int.MinValue) overflow("-Int.MinValue"); new SafeInt(-toInt) }

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

	@inline def compare(other :SafeInt) :Int = java.lang.Integer.compare(toInt, other.toInt)

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
	@inline def %(x: Short) : SafeInt = new SafeInt(toInt % x)
	@inline def %(x: Char)  : SafeInt = new SafeInt(toInt % x)
	@inline def %(x: Int)   : SafeInt = new SafeInt(toInt % x)
	@inline def %(x: Long)  : SafeInt = new SafeInt((toInt % x).toInt)
	@inline def %(x: Float) : Float   = checkFloatResult(x, "%", toFloat % x)
	@inline def %(x: Double): Double  = checkDoubleResult(x, "%", toDouble % x)


	@inline private def overflow(msg :String) :Nothing =
		throw new ArithmeticException("Arithmetic overflow: " + msg + ".")

	@inline private def underflow(msg :String) :Nothing =
		throw new ArithmeticException("Arithmetic overflow: " + msg + ".")

	@inline private def checkFloatResult(arg :Float, op :String, result :Float) :Float = {
		if (result != result | result == jl.Float.POSITIVE_INFINITY | result == jl.Float.NEGATIVE_INFINITY)
			if (result == jl.Float.POSITIVE_INFINITY)
				overflow(String.valueOf(toInt) + " " + op + " " + arg + " is Float.POSITIVE_INFINITY.")
			else if (result == jl.Float.NEGATIVE_INFINITY)
				underflow(String.valueOf(toInt) + " " + op + " " + arg + " is Float.NEGATIVE_INFINITY.")
			else
				throw new ArithmeticException(String.valueOf(toInt) + " " + op + " " + arg + " == NaN")
		result
	}

	@inline private def checkDoubleResult(arg :Double, op :String, result :Double) :Double = {
		if (result != result | result == jl.Double.POSITIVE_INFINITY | result == jl.Double.NEGATIVE_INFINITY)
			if (result == jl.Double.POSITIVE_INFINITY)
				overflow(String.valueOf(toInt) + " " + op + " " + arg + " is Double.POSITIVE_INFINITY.")
			else if (result == jl.Double.NEGATIVE_INFINITY)
				underflow(String.valueOf(toInt) + " " + op + " " + arg + " is Double.NEGATIVE_INFINITY.")
			else
				throw new ArithmeticException(String.valueOf(toInt) + " " + op + " " + arg + " == NaN")
		result
	}

	@inline private def outOfRange(typeName :String) :Nothing =
		throw new ArithmeticException("Value " + toInt + " does not fit in a " + typeName + ".")

	@inline private def outOfPrecision(typeName :String) :Nothing =
		throw new ArithmeticException("Value " + toInt + " cannot be exactly represented as a " + typeName + ".")

	@inline private def testRange(min :Int, max :Int, typeName :String) :Unit =
		if (toInt < min | toInt > max)
			throw new ArithmeticException("value " + toInt + " does not fit in a " + typeName + ".")


	override def toString :String = String.valueOf(toLong)
}




object SafeInt {
	@inline def apply(value :Int) :SafeInt = new SafeInt(value)

	@inline implicit def fromInt(value :Int) :SafeInt = new SafeInt(value)
	@inline implicit def toInt(value :SafeInt) :Int = value.toInt
	@inline implicit def toLong(value :SafeInt) :Long = value.toInt.toLong

	sealed abstract class SafeIntIsNumeric extends Numeric[SafeInt] {
		override def plus(x :SafeInt, y :SafeInt) :SafeInt = x + y
		override def minus(x :SafeInt, y :SafeInt) :SafeInt = x - y
		override def times(x :SafeInt, y :SafeInt) :SafeInt = x * y
		override def negate(x :SafeInt) :SafeInt = -x
		override def fromInt(x :Int) :SafeInt = new SafeInt(x)
		override def parseString(str :String) :Option[SafeInt] =
			Numeric.IntIsIntegral.parseString(str).map(new SafeInt(_))

		override def toInt(x :SafeInt) :Int = x.toInt
		override def toLong(x :SafeInt) :Long = x.toLong
		override def toFloat(x :SafeInt) :Float = x.toFloat
		override def toDouble(x :SafeInt) :Double = x.toDouble
		override def compare(x :SafeInt, y :SafeInt) :Int = x compare y
	}
	implicit object SafeIntIsIntegral extends SafeIntIsNumeric with Integral[SafeInt] {
		override def quot(x :SafeInt, y :SafeInt) :SafeInt = x / y
		override def rem(x :SafeInt, y :SafeInt) :SafeInt = x % y
	}
	object SafeIntAsIfFractional extends SafeIntIsNumeric with Fractional[SafeInt] {
		override def div(x :SafeInt, y :SafeInt) :SafeInt = x / y
	}

	/** Adds method `safe` to an `Int` value, converting it to an overflow checking
	  *  [[net.noresttherein.slang.numeric.SafeInt SafeInt]].
	  */
	class SafeIntConversion(private val self :Int) extends AnyVal {
		/** Converts this `Int` into an overflow checking `SafeInt`. */
		@inline def safe :SafeInt = new SafeInt(self)
	}

}

