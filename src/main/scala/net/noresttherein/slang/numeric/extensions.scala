package net.noresttherein.slang.numeric

import java.math.{BigInteger, MathContext, BigDecimal => JavaBigDecimal}

import net.noresttherein.slang.numeric.Decimal64.{BigDecimalConverter, JavaBigDecimalConverter}
import net.noresttherein.slang.numeric.Ratio.LongNumerator
import net.noresttherein.slang.numeric.SafeInt.SafeIntConversion
import net.noresttherein.slang.numeric.SafeLong.SafeLongConversion
import net.noresttherein.slang.numeric.extensions.BigDecimalIsFractional
import net.noresttherein.slang.numeric.BigRatio.BigIntNumerator


trait extensions extends Any {
	@inline implicit final def method_%/(self :Int) = new LongNumerator(self)
	@inline implicit final def method_%/(self :Long) = new LongNumerator(self)
	@inline implicit final def method_%/(self :BigInteger) = new BigIntNumerator(self)
	@inline implicit final def method_%/(self :BigInt) = new BigIntNumerator(self)
	@inline implicit final def safeIntConversion(self :Int) = new SafeIntConversion(self)
	@inline implicit final def safeLongConversion(self :Long) = new SafeLongConversion(self)
	@inline implicit final def bigDecimalConverter(self :BigDecimal) = new BigDecimalConverter(self)
	@inline implicit final def javaBigDecimalConverter(self :JavaBigDecimal) = new JavaBigDecimalConverter(self)

	@inline implicit final def bigIntegerIsIntegral :Integral[BigInteger] = extensions.BigIntegerIsIntegral

	@inline implicit final def bigDecimalIsFractional(implicit math :MathContext = MathContext.DECIMAL128)
			:Fractional[JavaBigDecimal] =
		if (math == MathContext.DECIMAL128) extensions.BigDecimal128IsFractional
		else new BigDecimalIsFractional
}




object extensions {
	sealed class BigIntegerIsNumeric extends Numeric[BigInteger] {
		override def plus(x :BigInteger, y :BigInteger) :BigInteger = x add y
		override def minus(x :BigInteger, y :BigInteger) :BigInteger = x subtract y
		override def times(x :BigInteger, y :BigInteger) :BigInteger = x multiply y
		override def negate(x :BigInteger) :BigInteger = x.negate
		override def fromInt(x :Int) :BigInteger = BigInteger.valueOf(x)
 		override def parseString(str :String) :Option[BigInteger] =
		    try { Some(new BigInteger(str)) } catch {
			    case _ :NumberFormatException => None
		    }

		override def toInt(x :BigInteger) :Int = x.intValue
		override def toLong(x :BigInteger) :Long = x.longValue
		override def toFloat(x :BigInteger) :Float = x.floatValue
		override def toDouble(x :BigInteger) :Double = x.doubleValue

		override def compare(x :BigInteger, y :BigInteger) :Int = x compareTo y
	}
	
	object BigIntegerIsIntegral extends BigIntegerIsNumeric with Integral[BigInteger] {
		override def quot(x :BigInteger, y :BigInteger) :BigInteger = x divide y
		override def rem(x :BigInteger, y :BigInteger) :BigInteger = x remainder y
	}
	
	object BigIntegerAsIfFractional extends BigIntegerIsNumeric with Fractional[BigInteger] {
		override def div(x :BigInteger, y :BigInteger) :BigInteger = x divide y
	}
	
	
	
	class BigDecimalIsFractional(implicit ctx :MathContext) extends Fractional[JavaBigDecimal] {
		override def div(x :JavaBigDecimal, y :JavaBigDecimal) :JavaBigDecimal = x.divide(y, ctx)
		override def plus(x :JavaBigDecimal, y :JavaBigDecimal) :JavaBigDecimal = x.add(y, ctx)
		override def minus(x :JavaBigDecimal, y :JavaBigDecimal) :JavaBigDecimal = x.subtract(y, ctx)
		override def times(x :JavaBigDecimal, y :JavaBigDecimal) :JavaBigDecimal = x.multiply(y, ctx)
		override def negate(x :JavaBigDecimal) :JavaBigDecimal = x.negate(ctx)
		override def fromInt(x :Int) :JavaBigDecimal = JavaBigDecimal.valueOf(x)
		override def parseString(str :String) :Option[JavaBigDecimal] =
			try { Some(new JavaBigDecimal(str)) } catch {
				case _ :Exception => None
			}

		override def toInt(x :JavaBigDecimal) :Int = x.intValue
		override def toLong(x :JavaBigDecimal) :Long = x.longValue
		override def toFloat(x :JavaBigDecimal) :Float = x.floatValue
		override def toDouble(x :JavaBigDecimal) :Double = x.doubleValue
		override def compare(x :JavaBigDecimal, y :JavaBigDecimal) :Int = x compareTo y
	}
	
	
	final val BigDecimal128IsFractional = new BigDecimalIsFractional()(MathContext.DECIMAL128)
}
