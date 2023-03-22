package net.noresttherein.sugar.numeric

import java.math.{BigInteger, MathContext, BigDecimal => JavaBigDecimal}

import net.noresttherein.sugar.numeric.Decimal64.{BigDecimalConverter, JavaBigDecimalConverter}
import net.noresttherein.sugar.numeric.Ratio.LongNumerator
import net.noresttherein.sugar.numeric.SafeInt.SafeIntConversion
import net.noresttherein.sugar.numeric.SafeLong.SafeLongConversion
import net.noresttherein.sugar.numeric.extensions.{BigDecimalIsFractional, ByteExtension, DoubleExtension, FloatExtension, IntExtension, LongExtension, OrderedExtension, ShortExtension}
import net.noresttherein.sugar.numeric.BigRatio.BigIntNumerator




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

	@inline implicit final def ByteExtension(self :Byte) = new ByteExtension(self)
	@inline implicit final def ShortExtension(self :Short) = new ShortExtension(self)
	@inline implicit final def IntExtension(self :Int) = new IntExtension(self)
	@inline implicit final def LongExtension(self :Long) = new LongExtension(self)
	@inline implicit final def FloatExtension(self :Float) = new FloatExtension(self)
	@inline implicit final def DoubleExtension(self :Double) = new DoubleExtension(self)
	@inline implicit final def OrderedExtension[T](self :T) = new OrderedExtension(self)
}




@SerialVersionUID(Ver)
object extensions extends extensions {
	/** Exposes methods `max` as `atLeast` and `min` as `atMost`.
	  * Standard `max n` and `min n` can be confusing, as in everyday language they has a meaning of
	  * "not more than n" and "at least n", which is the opposite of what those functions do.
	  */
	class ByteExtension(private val self :Byte) extends AnyVal {
		@inline def atLeast(other :Byte) :Int = math.max(self, other)
		@inline def atMost(other :Byte) :Int = math.min(self, other)
	}
	/** Exposes methods `max` as `atLeast` and `min` as `atMost`.
	  * Standard `max n` and `min n` can be confusing, as in everyday language they has a meaning of
	  * "not more than n" and "at least n", which is the opposite of what those functions do.
	  */
	class ShortExtension(private val self :Short) extends AnyVal {
		@inline def atLeast(other :Short) :Int = math.max(self, other)
		@inline def atMost(other :Short) :Int = math.min(self, other)
	}
	/** Exposes methods `max` as `atLeast` and `min` as `atMost`.
	  * Standard `max n` and `min n` can be confusing, as in everyday language they has a meaning of
	  * "not more than n" and "at least n", which is the opposite of what those functions do.
	  */
	class IntExtension(private val self :Int) extends AnyVal {
		@inline def atLeast(other :Int) :Int = math.max(self, other)
		@inline def atMost(other :Int) :Int = math.min(self, other)
	}
	/** Exposes methods `max` as `atLeast` and `min` as `atMost`.
	  * Standard `max n` and `min n` can be confusing, as in everyday language they has a meaning of
	  * "not more than n" and "at least n", which is the opposite of what those functions do.
	  */
	class LongExtension(private val self :Long) extends AnyVal {
		@inline def atLeast(other :Long) :Long = math.max(self, other)
		@inline def atMost(other :Long) :Long = math.min(self, other)
	}
	/** Exposes methods `max` as `atLeast` and `min` as `atMost`.
	  * Standard `max n` and `min n` can be confusing, as in everyday language they has a meaning of
	  * "not more than n" and "at least n", which is the opposite of what those functions do.
	  */
	class FloatExtension(private val self :Float) extends AnyVal {
		@inline def atLeast(other :Float) :Float = math.max(self, other)
		@inline def atMost(other :Float) :Float = math.min(self, other)
	}
	/** Exposes methods `max` as `atLeast` and `min` as `atMost`.
	  * Standard `max n` and `min n` can be confusing, as in everyday language they has a meaning of
	  * "not more than n" and "at least n", which is the opposite of what those functions do.
	  */
	class DoubleExtension(private val self :Double) extends AnyVal {
		@inline def atLeast(other :Double) :Double = math.max(self, other)
		@inline def atMost(other :Double) :Double = math.min(self, other)
	}
	/** Exposes methods `max` as `atLeast` and `min` as `atMost`.
	  * Standard `max n` and `min n` can be confusing, as in everyday language they has a meaning of
	  * "not more than n" and "at least n", which is the opposite of what those functions do.
	  */
	class OrderedExtension[T](private val self :T) extends AnyVal {
		@inline def atLeast(other :T)(implicit ordering :Ordering[T]) :T = ordering.max(self, other)
		@inline def atMost(other :T)(implicit ordering :Ordering[T]) :T = ordering.min(self, other)
	}


	@SerialVersionUID(Ver)
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
	@SerialVersionUID(Ver)
	object BigIntegerIsIntegral extends BigIntegerIsNumeric with Integral[BigInteger] {
		override def quot(x :BigInteger, y :BigInteger) :BigInteger = x divide y
		override def rem(x :BigInteger, y :BigInteger) :BigInteger = x remainder y
	}
	@SerialVersionUID(Ver)
	object BigIntegerAsIfFractional extends BigIntegerIsNumeric with Fractional[BigInteger] {
		override def div(x :BigInteger, y :BigInteger) :BigInteger = x divide y
	}
	
	
	@SerialVersionUID(Ver)
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
