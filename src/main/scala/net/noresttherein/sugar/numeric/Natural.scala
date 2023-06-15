package net.noresttherein.sugar.numeric




/** A natural number. Any operation returning a `Natural` number will throw an [[ArithmeticException]] if the result
  * is negative, including arithmetic underflows.
  */
//consider: renaming to Nat
//consider: removing it, duplicates UInt
class Natural private (val toInt :Int) extends AnyVal {

	@inline def toByte  : Byte   = toInt.toByte
	@inline def toShort : Short  = toInt.toShort
	@inline def toChar  : Char   = toInt.toChar
	@inline def toLong  : Long   = toInt.toLong
	@inline def toFloat : Float  = toInt.toFloat
	@inline def toDouble: Double = toInt.toDouble

	@deprecated("Adding a number and a String is deprecated. Use the string interpolation `s\"$num$str\"`", "Scala 2.13.0")
	@inline def +(x: String): String = toInt.toString + x

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

//	@inline def < (x: Byte)  : Boolean = toInt < x
//	@inline def < (x: Short) : Boolean = toInt < x
//	@inline def < (x: Char)  : Boolean = toInt < x
//	@inline def < (x: Int)   : Boolean = toInt < x
//	@inline def < (x: Long)  : Boolean = toInt < x
//	@inline def < (x: Float) : Boolean = toFloat < x
//	@inline def < (x: Double): Boolean = toDouble < x
	@inline def < (x :Natural): Boolean = toInt < x.toInt
//	@inline def <=(x: Byte)  : Boolean = toInt <= x
//	@inline def <=(x: Short) : Boolean = toInt <= x
//	@inline def <=(x: Char)  : Boolean = toInt <= x
//	@inline def <=(x: Int)   : Boolean = toInt <= x
//	@inline def <=(x: Long)  : Boolean = toInt <= x
//	@inline def <=(x: Float) : Boolean = toFloat <= x
//	@inline def <=(x: Double): Boolean = toDouble <= x
	@inline def <=(x :Natural): Boolean = toInt <= x.toInt
//	@inline def > (x: Byte)  : Boolean = toInt > x
//	@inline def > (x: Short) : Boolean = toInt > x
//	@inline def > (x: Char)  : Boolean = toInt > x
//	@inline def > (x: Int)   : Boolean = toInt > x
//	@inline def > (x: Long)  : Boolean = toInt > x
//	@inline def > (x: Float) : Boolean = toFloat > x
//	@inline def > (x: Double): Boolean = toDouble > x
	@inline def > (x :Natural): Boolean = toInt > x.toInt
//	@inline def >=(x: Byte)  : Boolean = toInt >= x
//	@inline def >=(x: Short) : Boolean = toInt >= x
//	@inline def >=(x: Char)  : Boolean = toInt >= x
//	@inline def >=(x: Int)   : Boolean = toInt >= x
//	@inline def >=(x: Long)  : Boolean = toInt >= x
//	@inline def >=(x: Float) : Boolean = toFloat >= x
//	@inline def >=(x: Double): Boolean = toDouble >= x
	@inline def >=(x :Natural): Boolean = toInt >= x.toInt

	@inline def compare(other :Natural) :Int = java.lang.Integer.compare(toInt, other.toInt)
	
	@inline def +(x: Byte)  : Natural  = new Natural(Math.addExact(toInt, x))
	@inline def +(x: Short) : Natural  = new Natural(Math.addExact(toInt, x))
	@inline def +(x: Char)  : Natural  = new Natural(Math.addExact(toInt, x))
	@inline def +(x: Int)   : Natural  = new Natural(Math.addExact(toInt, x))
	@inline def +(x: Long)  : Long     = toInt + x
	@inline def +(x: Float) : Float    = toInt + x
	@inline def +(x: Double): Double   = toInt + x
//	@inline def -(x: Byte)  : Int      = toInt - x
//	@inline def -(x: Short) : Int      = toInt - x
//	@inline def -(x: Char)  : Int      = toInt - x
//	@inline def -(x: Int)   : Int      = toInt - x
//	@inline def -(x: Long)  : Long     = toInt - x
	@inline def -(x: Float) : Float    = toInt - x
//	@inline def -(x: Double): Double   = toInt - x
	@inline def -(x :Natural): Double  =
		if (x.toInt > toInt) throw new ArithmeticException(toInt.toString + " - " + x.toInt + " is not a natural number")
		else new Natural(toInt - x.toInt)

//	@inline def *(x: Byte)  : Int      = toInt * x
//	@inline def *(x: Short) : Int      = toInt * x
//	@inline def *(x: Char)  : Int      = toInt * x
//	@inline def *(x: Int)   : Int      = toInt * x
//	@inline def *(x: Long)  : Long     = toInt * x
	@inline def *(x: Float) : Float    = toInt * x
//	@inline def *(x: Double): Double   = toInt * x
	@inline def *(x :Natural): Natural = new Natural(Math.multiplyExact(toInt, x.toInt))
//	@inline def /(x: Byte)  : Int      = toInt / x
//	@inline def /(x: Short) : Int      = toInt / x.toInt
//	@inline def /(x: Char)  : Int      = toInt / x.toInt
//	@inline def /(x: Int)   : Int      = toInt / x
//	@inline def /(x: Long)  : Int      = (toInt / x).toInt
	@inline def /(x: Float) : Float    = toInt / x
//	@inline def /(x: Double): Double   = toInt / x
	@inline def /(x :Natural) :Natural = new Natural(toInt / x.toInt)
//	@inline def %(x: Short) : Int      = toInt % x
//	@inline def %(x: Char)  : Int      = toInt % x
//	@inline def %(x: Int)   : Int      = toInt % x
//	@inline def %(x: Long)  : Int      = (toInt % x).toInt
	@inline def %(x: Float) : Float    = toInt % x
//	@inline def %(x: Double): Double   = toInt % x
	@inline def %(x :Natural): Natural = new Natural(toInt % x.toInt)

	override def toString :String = toInt.toString
}


private[numeric] sealed abstract class Rank2NaturalImplicits {
	@inline implicit def naturalToDouble(x :Natural) :Double = x.toInt
}

private[numeric] sealed abstract class Rank1NaturalImplicits extends Rank2NaturalImplicits {
	@inline implicit def naturalToLong(x :Natural) :Long = x.toInt
}

object Natural extends Rank1NaturalImplicits {
	@inline def apply(n :Int) :Natural =
		if (n < 0) negativeNatural(n)
		else new Natural(n)

	@inline implicit def naturalToInt(x :Natural)    :Int = x.toInt

	private[numeric] def negativeNatural(n :Int) =
		throw new ArithmeticException(n.toString + " is not a natural number")
}
