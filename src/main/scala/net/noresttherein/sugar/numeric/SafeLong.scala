package net.noresttherein.sugar.numeric

import java.{lang => jl}




/** A `Long` value checking for overflows, underflows and loss of precision in all operations.
  * Unless stated otherwise, all mutating methods, as well as those promoting `Long` to `Float` or `Double`,
  * can throw an [[ArithmeticException]] with the exception of bitwise operations.
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
class SafeLong(val toLong :Long) extends AnyVal with Serializable {

	def toByte  : Byte   = { testRange(Byte.MinValue, Byte.MaxValue, "Byte"); toLong.toByte }
	def toShort : Short  = { testRange(Short.MinValue, Short.MaxValue, "Short"); toLong.toShort }
	def toChar  : Char   = { testRange(Char.MinValue, Char.MaxValue, "Char"); toLong.toChar }
	def toInt   : Int    = { testRange(Int.MinValue, Int.MaxValue, "Int"); toLong.toInt }
	def toFloat : Float  =
		{ val res = toLong.toFloat; if (res.toLong != toLong) outOfPrecision("Float"); res }
	def toDouble: Double =
		{ val res = toLong.toDouble; if (res.toLong != toLong) outOfPrecision("Double"); res }

	@inline def unary_~ : SafeLong = new SafeLong(~toLong)
	@inline def unary_+ : SafeLong = this
	@inline def unary_- : SafeLong =
		{ if (toLong == Long.MinValue) overflow("-Long.MinValue"); new SafeLong(-toLong) }

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

	@inline def compare(x :SafeLong) :Int = java.lang.Long.compare(toInt, x.toInt)

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
	@inline def %(x: Short) : SafeLong = new SafeLong(toLong % x)
	@inline def %(x: Char)  : SafeLong = new SafeLong(toLong % x)
	@inline def %(x: Int)   : SafeLong = new SafeLong(toLong % x)
	@inline def %(x: Long)  : SafeLong = new SafeLong(toLong % x)
	@inline def %(x: Float) : Float    = checkFloatResult(x, "%", toFloat % x)
	@inline def %(x: Double): Double   = checkDoubleResult(x, "%", toDouble % x)


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

//	@inline private def outOfRange(typeName :String) :Nothing =
//		throw new ArithmeticException("Value " + toLong + " does not fit in a " + typeName + ".")

	@inline private def outOfPrecision(typeName :String) :Nothing =
		throw new ArithmeticException("Value " + toLong + " cannot be exactly represented as a " + typeName + ".")

	@inline private def testRange(min :Long, max :Long, typeName :String) :Unit =
		if (toLong < min || toLong > max)
			throw new ArithmeticException("value " + toLong + " does not fit in a " + typeName + ".")


	override def toString :String = String.valueOf(toLong)
}




@SerialVersionUID(Ver)
object SafeLong {
	@inline def apply(value :Long) :SafeLong = new SafeLong(value)

	@inline implicit def safeLongFromInt(value :Int) :SafeLong = new SafeLong(value)
	@inline implicit def safeLongFromLong(value :Long) :SafeLong = new SafeLong(value)
	@inline implicit def safeLongToLong(value :SafeLong) :Long = value.toLong

	sealed abstract class SafeLongIsNumeric extends Numeric[SafeLong] {
		override def plus(x :SafeLong, y :SafeLong) :SafeLong = x + y
		override def minus(x :SafeLong, y :SafeLong) :SafeLong = x - y
		override def times(x :SafeLong, y :SafeLong) :SafeLong = x * y
		override def negate(x :SafeLong) :SafeLong = -x
		override def fromInt(x :Int) :SafeLong = new SafeLong(x)
		override def parseString(str :String) :Option[SafeLong] =
			Numeric.IntIsIntegral.parseString(str).map(new SafeLong(_))

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


	/** Adds method `safe` to an `Long` value, converting it to an overflow checking
	  *  [[net.noresttherein.sugar.numeric.SafeLong SafeLong]].
	  */
	class SafeLongConverter(private val self :Long) extends AnyVal {
		/** Converts this `Long` into an overflow checking `SafeLong`. */
		@inline def safe :SafeLong = new SafeLong(self)
		/** Converts this `Long` into an overflow checking `SafeLong`. */
		@inline def toSafeLong :SafeLong = new SafeLong(self)
	}
}

