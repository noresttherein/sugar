package net.noresttherein.sugar.numeric

import java.math.{BigInteger, MathContext, BigDecimal => JavaBigDecimal}

import scala.util.Random

import net.noresttherein.sugar.numeric
import net.noresttherein.sugar.numeric.Decimal64.{BigDecimalConverter, JavaBigDecimalConverter}
import net.noresttherein.sugar.numeric.Ratio.LongNumerator
import net.noresttherein.sugar.numeric.SafeInt.SafeIntConversion
import net.noresttherein.sugar.numeric.SafeLong.SafeLongConversion
import net.noresttherein.sugar.numeric.extensions.{BigDecimalIsFractional, BooleanObjectExtension, ByteExtension, ByteObjectExtension, CharObjectExtension, DoubleExtension, DoubleObjectExtension, FloatExtension, FloatObjectExtension, IntExtension, IntObjectExtension, LongExtension, LongObjectExtension, OrderedExtension, ShortExtension, ShortObjectExtension}
import net.noresttherein.sugar.numeric.BigRatio.BigIntNumerator




trait extensions extends Any {
//	@inline implicit final def method_%/(self :Int) :LongNumerator = new LongNumerator(self)
//	@inline implicit final def method_%/(self :Long) :LongNumerator = new LongNumerator(self)
//	@inline implicit final def method_%/(self :BigInteger) :BigIntNumerator = new BigIntNumerator(self)
//	@inline implicit final def method_%/(self :BigInt) :BigIntNumerator = new BigIntNumerator(self)

	@inline implicit final def safeIntConversion(self :Int) :SafeIntConversion = new SafeIntConversion(self)
	@inline implicit final def safeLongConversion(self :Long) :SafeLongConversion = new SafeLongConversion(self)

	@inline implicit final def bigDecimalConverter(self :BigDecimal) :BigDecimalConverter =
		new BigDecimalConverter(self)

	@inline implicit final def javaBigDecimalConverter(self :JavaBigDecimal) :JavaBigDecimalConverter =
		new JavaBigDecimalConverter(self)

	@inline implicit final def bigIntegerIsIntegral :Integral[BigInteger] = extensions.BigIntegerIsIntegral

	@inline implicit final def bigDecimalIsFractional(implicit math :MathContext = MathContext.DECIMAL128)
			:Fractional[JavaBigDecimal] =
		if (math == MathContext.DECIMAL128) extensions.BigDecimal128IsFractional
		else new BigDecimalIsFractional

	@inline implicit final def byteExtension(self :Byte) :ByteExtension = new ByteExtension(self)
	@inline implicit final def shortExtension(self :Short) :ShortExtension = new ShortExtension(self)
	@inline implicit final def intExtension(self :Int) :IntExtension = new IntExtension(self)
	@inline implicit final def longExtension(self :Long) :LongExtension = new LongExtension(self)
	@inline implicit final def floatExtension(self :Float) :FloatExtension = new FloatExtension(self)
	@inline implicit final def doubleExtension(self :Double) :DoubleExtension = new DoubleExtension(self)
	@inline implicit final def orderedExtension[T](self :T) :OrderedExtension[T] = new OrderedExtension(self)
	@inline implicit final def bigIntegerExtension(self :BigInteger) :BigIntNumerator = new BigIntNumerator(self)
	@inline implicit final def bigIntExtension(self :BigInt) :BigIntNumerator = new BigIntNumerator(self)

	/** Adds `random` extension methods to `Byte` singleton object. */
	@inline implicit final def byteObjectExtension(self :Byte.type) :ByteObjectExtension = new ByteObjectExtension {}

	/** Adds `random` extension methods to `Short` singleton object. */
	@inline implicit final def shortObjectExtension(self :Short.type) :ShortObjectExtension = new ShortObjectExtension {}

	/** Adds `random` extension methods to `Char` singleton object. */
	@inline implicit final def charObjectExtension(self :Char.type) :CharObjectExtension = new CharObjectExtension {}

	/** Adds `random` extension methods to `Int` singleton object. */
	@inline implicit final def intObjectExtension(self :Int.type) :IntObjectExtension = new IntObjectExtension {}

	/** Adds `random` extension methods to `Long` singleton object. */
	@inline implicit final def longObjectExtension(self :Long.type) :LongObjectExtension = new LongObjectExtension {}

	/** Adds `random` extension methods to `Float` singleton object. */
	@inline implicit final def floatObjectExtension(self :Float.type) :FloatObjectExtension =
		new FloatObjectExtension {}

	/** Adds `random` extension methods to `Double` singleton object. */
	@inline implicit final def doubleObjectExtension(self :Double.type) :DoubleObjectExtension =
		new DoubleObjectExtension {}

	/** Adds `random` extension methods to `Boolean` singleton object. */
	@inline implicit final def booleanObjectExtension(self :Boolean.type) :BooleanObjectExtension =
		new BooleanObjectExtension {}
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
		/** Creates the reduced form of fraction `this/100`. */
		@inline def % :Ratio = Ratio.percent(self)

		/** Creates the reduced form of fraction `this/100`. */
		@inline def percent :Ratio = Ratio.percent(self)

		/** Divides this `Long` by the argument, creating a [[net.noresttherein.sugar.numeric.Ratio Ratio]]
		  * number representing the result.
		  * @param denominator the denominator of the created rational (before reduction)
		  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
		  */
		@inline def %/(denominator :Long) :Ratio = Ratio(self, denominator)

		/** Divides this `Long` by the argument, creating a [[net.noresttherein.sugar.numeric.Ratio Ratio]]
		  * number representing the result.
		  * @param denominator the denominator of the created rational (before reduction)
		  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
		  */
		@inline def %/(denominator :Int) :Ratio = Ratio(self, denominator)

		/** Returns `this max other`. */
		@inline def atLeast(other :Int) :Int = math.max(self, other)

		/** Returns `this min other`. */
		@inline def atMost(other :Int) :Int = math.min(self, other)
	}

	/** Exposes methods `max` as `atLeast` and `min` as `atMost`.
	  * Standard `max n` and `min n` can be confusing, as in everyday language they has a meaning of
	  * "not more than n" and "at least n", which is the opposite of what those functions do.
	  */
	class LongExtension(private val self :Long) extends AnyVal {
		/** Creates the reduced form of fraction `this/100`. */
		@inline def % :Ratio = Ratio.percent(self)

		/** Creates the reduced form of fraction `this/100`. */
		@inline def percent :Ratio = Ratio.percent(self)

		/** Divides this `Long` by the argument, creating a [[net.noresttherein.sugar.numeric.Ratio Ratio]]
		  * number representing the result.
		  * @param denominator the denominator of the created rational (before reduction)
		  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
		  */
		@inline def %/(denominator :Long) :Ratio = Ratio(self, denominator)

		/** Divides this `Long` by the argument, creating a [[net.noresttherein.sugar.numeric.Ratio Ratio]]
		  * number representing the result.
		  * @param denominator the denominator of the created rational (before reduction)
		  * @return a rational number representing the canonical form of the `numerator/denominator` fraction.
		  */
		@inline def %/(denominator :Int) :Ratio = Ratio(self, denominator)

		/** Returns `this max other`. */
		@inline def atLeast(other :Long) :Long = math.max(self, other)

		/** Returns `this min other`. */
		@inline def atMost(other :Long) :Long = math.min(self, other)
	}
	/** Exposes methods `max` as `atLeast` and `min` as `atMost`.
	  * Standard `max n` and `min n` can be confusing, as in everyday language they has a meaning of
	  * "not more than n" and "at least n", which is the opposite of what those functions do.
	  */
	class FloatExtension(private val self :Float) extends AnyVal {
		/** Returns `this max other`. */
		@inline def atLeast(other :Float) :Float = math.max(self, other)
		/** Returns `this min other`. */
		@inline def atMost(other :Float) :Float = math.min(self, other)
	}

	/** Exposes methods `max` as `atLeast` and `min` as `atMost`.
	  * Standard `max n` and `min n` can be confusing, as in everyday language they has a meaning of
	  * "not more than n" and "at least n", which is the opposite of what those functions do.
	  */
	class DoubleExtension(private val self :Double) extends AnyVal {
		/** Returns `this max other`. */
		@inline def atLeast(other :Double) :Double = math.max(self, other)
		/** Returns `this min other`. */
		@inline def atMost(other :Double) :Double = math.min(self, other)
	}

	/** Exposes methods `max` as `atLeast` and `min` as `atMost`.
	  * Standard `max n` and `min n` can be confusing, as in everyday language they has a meaning of
	  * "not more than n" and "at least n", which is the opposite of what those functions do.
	  */
	class OrderedExtension[T](private val self :T) extends AnyVal {
		/** Returns `ordering.max(this, other)`. */
		@inline def atLeast(other :T)(implicit ordering :Ordering[T]) :T = ordering.max(self, other)
		/** Returns `ordering.min(this, other)`. */
		@inline def atMost(other :T)(implicit ordering :Ordering[T]) :T = ordering.min(self, other)
	}


	sealed trait ByteObjectExtension extends Any {
		/** A random `Byte` retrieved from the implicit `Random`. */
		@inline final def random(implicit gen :Random) :Byte = gen.self.nextInt(0xff).toByte

		/** A random `Byte` from range `[0, bound)`, retrieved from the implicit `Random`. */
		@inline final def random(bound :Byte)(implicit gen :Random) :Byte = gen.self.nextInt(bound).toByte

		/** A random `Byte` from range `[from, until)`, retrieved from the implicit `Random`. */
		@inline final def random(from :Byte, until :Byte)(implicit gen :Random) :Byte =
			gen.self.nextInt(from, until).toByte
	}
	
	sealed trait ShortObjectExtension extends Any {
		/** A random `Short` retrieved from the implicit `Random`. */
		@inline final def random(implicit gen :Random) :Short = gen.self.nextInt(0xffff).toShort

		/** A random `Short` from range `[0, bound)`, retrieved from the implicit `Random`. */
		@inline final def random(bound :Short)(implicit gen :Random) :Short = gen.self.nextInt(bound).toShort

		/** A random `Short` from range `[from, until)`, retrieved from the implicit `Random`. */
		@inline final def random(from :Short, until :Short)(implicit gen :Random) :Short =
			gen.self.nextInt(from, until).toShort
	}
		
	sealed trait CharObjectExtension extends Any {
		/** A random `Char` retrieved from the implicit `Random`. */
		@inline final def random(implicit gen :Random) :Char = gen.self.nextInt(0xffff).toChar

		/** A random `Char` from range `[0, bound)`, retrieved from the implicit `Random`. */
		@inline final def random(bound :Char)(implicit gen :Random) :Char = gen.self.nextInt(bound).toChar

		/** A random `Char` from range `[from, until)`, retrieved from the implicit `Random`. */
		@inline final def random(from :Char, until :Char)(implicit gen :Random) :Char =
			gen.self.nextInt(from, until).toChar
	}

	sealed trait IntObjectExtension extends Any {
		/** Returns `implicitly[Random].nextInt`. */
		@inline final def random(implicit gen :Random) :Int = gen.self.nextInt

		/** Returns `implicitly[Random].nextInt(bound)`. The argument must be positive. */
		@inline final def random(bound :Int)(implicit gen :Random) :Int = gen.self.nextInt(bound)

		/** Returns `implicitly[Random].nextInt(from, until)`. */
		@inline final def random(from :Int, until :Int)(implicit gen :Random) :Int = gen.self.nextInt(from, until)
	}

	sealed trait LongObjectExtension extends Any {
		/** Returns `implicitly[Random].nextLong`. */
		@inline final def random(implicit gen :Random) :Long = gen.self.nextLong

		/** Returns `implicitly[Random].nextLong(bound)`. The argument must be positive. */
		@inline final def random(bound :Long)(implicit gen :Random) :Long = gen.self.nextLong(bound)

		/** Returns `implicitly[Random].nextLong(from, until)`. */
		@inline final def random(from :Long, until :Long)(implicit gen :Random) :Long = gen.self.nextLong(from, until)
	}

	sealed trait FloatObjectExtension extends Any {
		/** Returns `implicitly[Random].nextFloat`. */
		@inline final def random(implicit gen :Random) :Float = gen.self.nextFloat

		/** Returns `implicitly[Random].nextFloat(bound)`. The argument must be positive. */
		@inline final def random(bound :Float)(implicit gen :Random) :Float = gen.self.nextFloat(bound)

		/** Returns `implicitly[Random].nextFloat(from, until)`. */
		@inline final def random(from :Float, until :Float)(implicit gen :Random) :Float = gen.self.nextFloat(from, until)
	}
	
	sealed trait DoubleObjectExtension extends Any {
		/** Returns `implicitly[Random].nextDouble`. */
		@inline final def random(implicit gen :Random) :Double = gen.self.nextDouble

		/** Returns `implicitly[Random].nextDouble(bound)`. The argument must be positive. */
		@inline final def random(bound :Double)(implicit gen :Random) :Double = gen.self.nextDouble(bound)

		/** Returns `implicitly[Random].nextDouble(from, until)`. */
		@inline final def random(from :Double, until :Double)(implicit gen :Random) :Double =
			gen.self.nextDouble(from, until)
	}

	sealed trait BooleanObjectExtension extends Any {
		/** A random `Boolean` retrieved from the implicit `Random`. */
		@inline def random(implicit gen :Random) :Boolean = gen.self.nextInt(2) == 0
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
