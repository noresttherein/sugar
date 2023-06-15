package net.noresttherein.sugar.numeric

import java.{lang => jl}
import java.lang.{Math => math}
import java.math.{BigInteger, MathContext, BigDecimal => JavaBigDecimal}

import scala.util.Random

import net.noresttherein.sugar.numeric.Decimal64.{BigDecimalConverter, JavaBigDecimalConverter}
import net.noresttherein.sugar.numeric.extensions.{hasOrderingExtension, JBigDecimalIsFractional, BooleanExtension, BooleanObjectExtension, ByteExtension, ByteObjectExtension, CharExtension, CharObjectExtension, DoubleExtension, DoubleObjectExtension, FloatExtension, FloatObjectExtension, IntExtension, IntObjectExtension, LongExtension, LongObjectExtension, ShortExtension, ShortObjectExtension}
import net.noresttherein.sugar.numeric.BigRatio.BigIntNumerator




trait extensions extends Any {

	@inline implicit final def BigDecimalConverter(self :BigDecimal) :BigDecimalConverter =
		new BigDecimalConverter(self)

	@inline implicit final def JavaBigDecimalConverter(self :JavaBigDecimal) :JavaBigDecimalConverter =
		new JavaBigDecimalConverter(self)

	@inline implicit def BigIntegerIsIntegral :Integral[BigInteger] = extensions.BigIntegerIsIntegral

	@inline implicit final def JavaBigDecimalIsFractional(implicit math :MathContext = MathContext.DECIMAL128)
			:Fractional[JavaBigDecimal] =
		if (math == MathContext.DECIMAL128) extensions.JBigDecimal128IsFractional
		else new JBigDecimalIsFractional

	@inline implicit final def BooleanExtension(self :Boolean) :BooleanExtension = new BooleanExtension(self)
	@inline implicit final def ByteExtension(self :Byte) :ByteExtension = new ByteExtension(self)
	@inline implicit final def ShortExtension(self :Short) :ShortExtension = new ShortExtension(self)
	@inline implicit final def CharExtension(self :Char) :CharExtension = new CharExtension(self)
	@inline implicit final def IntExtension(self :Int) :IntExtension = new IntExtension(self)
	@inline implicit final def LongExtension(self :Long) :LongExtension = new LongExtension(self)
	@inline implicit final def FloatExtension(self :Float) :FloatExtension = new FloatExtension(self)
	@inline implicit final def DoubleExtension(self :Double) :DoubleExtension = new DoubleExtension(self)
	@inline implicit final def BigIntegerExtension(self :BigInteger) :BigIntNumerator = new BigIntNumerator(self)
	@inline implicit final def BigIntExtension(self :BigInt) :BigIntNumerator = new BigIntNumerator(self)
	@inline implicit final def hasOrderingExtension[T](self :T) :hasOrderingExtension[T] =
		new hasOrderingExtension(self)

	/** Adds `random` extension methods to `Byte` singleton object. */
	@inline implicit final def ByteObjectExtension(self :Byte.type) :ByteObjectExtension = new ByteObjectExtension {}

	/** Adds `random` extension methods to `Short` singleton object. */
	@inline implicit final def ShortObjectExtension(self :Short.type) :ShortObjectExtension = new ShortObjectExtension {}

	/** Adds `random` extension methods to `Char` singleton object. */
	@inline implicit final def CharObjectExtension(self :Char.type) :CharObjectExtension = new CharObjectExtension {}

	/** Adds `random` extension methods to `Int` singleton object. */
	@inline implicit final def IntObjectExtension(self :Int.type) :IntObjectExtension = new IntObjectExtension {}

	/** Adds `random` extension methods to `Long` singleton object. */
	@inline implicit final def LongObjectExtension(self :Long.type) :LongObjectExtension = new LongObjectExtension {}

	/** Adds `random` extension methods to `Float` singleton object. */
	@inline implicit final def FloatObjectExtension(self :Float.type) :FloatObjectExtension =
		new FloatObjectExtension {}

	/** Adds `random` extension methods to `Double` singleton object. */
	@inline implicit final def DoubleObjectExtension(self :Double.type) :DoubleObjectExtension =
		new DoubleObjectExtension {}

	/** Adds `random` extension methods to `Boolean` singleton object. */
	@inline implicit final def BooleanObjectExtension(self :Boolean.type) :BooleanObjectExtension =
		new BooleanObjectExtension {}
}




@SerialVersionUID(Ver)
object extensions extends extensions {
	/** Conversions from a `Boolean` to a `0` or `1` in various integral value types. */
	class BooleanExtension private[extensions] (private val self :Boolean) extends AnyVal {
		/** `1` if this `Boolean` is `ture`, or `0` otherwise. */
		@inline final def toByte :Int = if (self) 1.toByte else 0.toByte
		/** `1` if this `Boolean` is `ture`, or `0` otherwise. */
		@inline final def toShort :Short = if (self) 1.toByte else 0.toByte
		/** `1` if this `Boolean` is `ture`, or `0` otherwise. */
		@inline final def toInt :Int = if (self) 1 else 0
		/** `1` if this `Boolean` is `ture`, or `0` otherwise. */
		@inline final def toLong :Long = if (self) 1L else 0L
		/** `1` if this `Boolean` is `ture`, or `0` otherwise. */
		@inline final def toNatural :Natural = if (self) 1.toNatural else 0.toNatural
	}

	/** Exposes methods `max` as `atLeast` and `min` as `atMost`.
	  * Standard `max n` and `min n` can be confusing, as in everyday language they has a meaning of
	  * "not more than n" and "at least n", which is the opposite of what those functions do.
	  */
	class ByteExtension private[extensions] (private val self :Byte) extends AnyVal {
		/** Returns `this max other`. */
		@inline def atLeast(other :Byte) :Int = math.max(self, other)

		/** Returns `this min other`. */
		@inline def atMost(other :Byte) :Int = math.min(self, other)

		/** This byte as a natural number.
		  * @throws ArithmeticException if this number is negative.
		  */
		@inline def toNatural :Natural = Natural(self)
	}

	/** Exposes methods `max` as `atLeast` and `min` as `atMost`.
	  * Standard `max n` and `min n` can be confusing, as in everyday language they has a meaning of
	  * "not more than n" and "at least n", which is the opposite of what those functions do.
	  */
	class ShortExtension private[extensions] (private val self :Short) extends AnyVal {
		/** Returns `this max other`. */
		@inline def atLeast(other :Short) :Int = math.max(self, other)

		/** Returns `this min other`. */
		@inline def atMost(other :Short) :Int = math.min(self, other)

		/** This byte as a natural number.
		  * @throws ArithmeticException if this number is negative.
		  */
		@inline def toNatural :Natural = Natural(self)
	}

	/** Exposes methods `max` as `atLeast` and `min` as `atMost`.
	  * Standard `max n` and `min n` can be confusing, as in everyday language they has a meaning of
	  * "not more than n" and "at least n", which is the opposite of what those functions do.
	  */
	class CharExtension private[extensions] (private val self :Char) extends AnyVal {
		/** Returns `this max other`. */
		@inline def atLeast(other :Char) :Int = math.max(self, other)

		/** Returns `this min other`. */
		@inline def atMost(other :Char) :Int = math.min(self, other)

		/** This byte as a natural number.
		  * @throws ArithmeticException if this number is negative.
		  */
		@inline def toNatural :Natural = Natural(self)
	}

	/** Exposes methods `max` as `atLeast` and `min` as `atMost`.
	  * Standard `max n` and `min n` can be confusing, as in everyday language they has a meaning of
	  * "not more than n" and "at least n", which is the opposite of what those functions do.
	  */
	class IntExtension private[extensions] (private val self :Int) extends AnyVal {
		/** Raises this `Int` to the given power. If the argument is negative, `0` is returned. */
		def pow(exp :Int) :Int = 
			if (exp < 0)
				0
			else {
				var res = 1
				var i   = jl.Integer.highestOneBit(exp)
				while (i >= 0) {
					res = res * res
					if ((exp >> i & 1) == 1)
						res = res * self
					i -= 1
				}
				res
			}

		/** Raises this `Int` to the given power. If the argument is negative, `0` is returned. */
		@inline def **(exp :Int) :Int = pow(exp)
		
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

		/** True if this `Int` has no divisors other than `1` and `2`. */
		@inline def isPowerOf2 :Boolean = jl.Integer.bitCount(self) == 1

		/** Number of one bits in the binary representation of this `Int`. */
		@inline def bitCount :Int = jl.Integer.bitCount(self)

		/** The number of consecutive zero bits in the highest positions of this `Int`. */
		@inline def leadingZeros  :Int = jl.Integer.numberOfLeadingZeros(self)

		/** The number of consecutive zero bits in the lowest positions of this `Int`. */
		@inline def trailingZeros :Int = jl.Integer.numberOfTrailingZeros(self)

		/** The highest one bit (with zero being the least significant bit). */
		@inline def highestOneBit :Int = jl.Integer.highestOneBit(self)

		/** The lowest one bit (with zero being the least significant bit). */
		@inline def lowestOneBit :Int = jl.Integer.lowestOneBit(self)

		/** An `Int` whose binary representation contains bits in the reverse order. */
		@inline def reverse :Int = jl.Integer.reverse(self)

		/** Reverses the order of bytes in this `Int`. */
		@inline def reverseBytes :Int = jl.Integer.reverseBytes(self)

		/** Shifts all bits by `n` position up, with the highest `n` bits being moved to the lowest `n` bits of the result. */
		@inline def rotateLeft(n :Int) :Int = jl.Integer.rotateLeft(self, n)

		/** Shifts all bits by `n` position down, with the lowest `n` bits being moved to the highest `n` bits of the result. */
		@inline def rotateRight(n :Int) :Int = jl.Integer.rotateRight(self, n)

		/** This byte as a natural number.
		  * @throws ArithmeticException if this number is negative.
		  */
		@inline def toNatural :Natural = Natural(self)

		/** Converts this `Int` to an unsigned `UInt`. */
		@throws[ArithmeticException]("if this Int is negative")
		@inline def toUInt :UInt = UInt.from(self)

		/** Reinterprets this `Int`'s binary format as an unsigned 32 bit integer.
		  * Negative values will be converted to values greater than `Int.MaxValue`.
		  */
		@inline def asUInt :UInt = new UInt(self)

		/** Converts this `Int` to an unsigned `ULong`. */
		@throws[ArithmeticException]("if this Int is negative")
		@inline def toULong :ULong = ULong.from(self)

		/** Converts  this `Int` to a 64 bit unsigned integer.
		  * Negative values will result in underflowing to values greater than `Int.MaxValue`.
		  */
		@inline def asULong :ULong = new ULong(self & 0xffffffffL)

		/** Converts this `Int` to an unsigned `UInt`. */
		@throws[ArithmeticException]("if this Int is negative")
		@inline def unsigned :UInt = UInt.from(self)

		/** Reinterprets this `Int`'s binary format as an unsigned 32 bit integer.
		  * Negative values will be converted to values greater than `Int.MaxValue`.
		  */
		@inline def asUnsigned :UInt = new UInt(self)

		/** Converts this `Int` into an overflow checking `SafeInt`. */
		@inline def safe :SafeLong = new SafeInt(self)

		/** Converts this `Int` into an overflow checking `SafeInt`. */
		@inline def toSafeInt :SafeInt = new SafeInt(self)

		/** Converts this `Int` into an overflow checking `SafeLong`. */
		@inline def toSafeLong :SafeLong = new SafeLong(self)
	}

	/** Exposes methods `max` as `atLeast` and `min` as `atMost`.
	  * Standard `max n` and `min n` can be confusing, as in everyday language they has a meaning of
	  * "not more than n" and "at least n", which is the opposite of what those functions do.
	  */
	class LongExtension private[extensions] (private val self :Long) extends AnyVal {
		/** Raises this `Long` to the given power. If the argument is negative, `0` is returned. */
		def pow(exp :Int) :Long =
			if (exp < 0)
				0
			else {
				var res = 1L
				var i   = jl.Long.highestOneBit(exp)
				while (i >= 0) {
					res = res * res
					if ((exp >> i.toInt & 1) == 1)
						res = res * self
					i -= 1
				}
				res
			}

		/** Raises this `Long` to the given power. If the argument is negative, `0` is returned. */
		@inline def **(exp :Int) :Long = pow(exp)

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


		/** True if this `Long` has no divisors other than `1` and `2`. */
		@inline def isPowerOf2 :Boolean = jl.Long.bitCount(self) == 1

		/** Number of one bits in the binary representation of this `Long`. */
		@inline def bitCount :Long = jl.Long.bitCount(self)

		/** The number of consecutive zero bits in the highest positions of this `Long`. */
		@inline def leadingZeros :Long = jl.Long.numberOfLeadingZeros(self)

		/** The number of consecutive zero bits in the lowest positions of this `Long`. */
		@inline def trailingZeros :Long = jl.Long.numberOfTrailingZeros(self)

		/** The highest one bit (with zero being the least significant bit). */
		@inline def highestOneBit :Long = jl.Long.highestOneBit(self)

		/** The lowest one bit (with zero being the least significant bit). */
		@inline def lowestOneBit :Long = jl.Long.lowestOneBit(self)

		/** An `Long` whose binary representation contains bits in the reverse order. */
		@inline def reverse :Long = jl.Long.reverse(self)

		/** Reverses the order of bytes in this `Long`. */
		@inline def reverseBytes :Long = jl.Long.reverseBytes(self)

		/** Shifts all bits by `n` position up, with the highest `n` bits being moved to the lowest `n` bits of the result. */
		@inline def rotateLeft(n :Int) :Long = jl.Long.rotateLeft(self, n)

		/** Shifts all bits by `n` position down, with the lowest `n` bits being moved to the highest `n` bits of the result. */
		@inline def rotateRight(n :Int) :Long = jl.Long.rotateRight(self, n)

		/** This byte as a natural number.
		  * @throws ArithmeticException if this number is negative or greater than `Int.MaxValue`.
		  */
		@inline def toNatural :Natural = Natural(self.toInt)

		/** Converts this `Long` to an unsigned `UInt`. */
		@throws[ArithmeticException]("if this Long is negative or greater than Int.MaxValue")
		@inline def toUInt :UInt =
			if (self > Int.MaxValue) throw new ArithmeticException(self + " is negative")
			else UInt.from(self.toInt)

		/** Interprets lower 4 bytes in this `Long`'s binary format as an unsigned 32 bit integer.
		  * Negative values will be converted to values greater than `Int.MaxValue`.
		  */
		@inline def asUInt :UInt = new UInt(self.toInt)

		/** Converts this `Long` to an unsigned `ULong`. */
		@throws[ArithmeticException]("if this Long is negative")
		@inline def toULong :ULong =
			if (self < 0) throw new ArithmeticException(self + " is negative")
			else new ULong(self)

		/** Reinterprets this `Long`'s binary format as an unsigned 64 bit integer.
		  * Negative values will be converted to values greater than `Long.MaxValue`.
		  */
		@inline def asULong :ULong = new ULong(self)

		/** Converts this `Long` to an unsigned `ULong`. */
		@throws[ArithmeticException]("if this Long is negative")
		@inline def unsigned :ULong =
			if (self < 0) throw new ArithmeticException(self + " is negative")
			else new ULong(self)

		/** Reinterprets this `Long`'s binary format as an unsigned 64 bit integer.
		  * Negative values will be converted to values greater than `Long.MaxValue`.
		  */
		@inline def asUnsigned :ULong = new ULong(self)

		/** Converts this `Long` into an overflow checking `SafeLong`. */
		@inline def safe :SafeLong = new SafeLong(self)

		/** Converts this `Long` into an overflow checking `SafeInt`. */
		@throws[ArithmeticException]("if this Long does not fit in the [Int.MinValue, Int.MaxValue] range.")
		@inline def toSafeInt :SafeInt = new SafeLong(self).toSafeInt

		/** Converts this `Long` into an overflow checking `SafeLong`. */
		@inline def toSafeLong :SafeLong = new SafeLong(self)
	}

	/** Exposes methods `max` as `atLeast` and `min` as `atMost`.
	  * Standard `max n` and `min n` can be confusing, as in everyday language they has a meaning of
	  * "not more than n" and "at least n", which is the opposite of what those functions do.
	  */
	class FloatExtension private[extensions] (private val self :Float) extends AnyVal {
		/** Raises this `Float` to the given power. */
		@inline def pow(exp :Float) :Double = math.pow(self, exp)

		/** Raises this `Float` to the given power. */
		@inline def **(exp :Float) :Double = math.pow(self, exp)

		/** Returns `this max other`. */
		@inline def atLeast(other :Float) :Float = math.max(self, other)

		/** Returns `this min other`. */
		@inline def atMost(other :Float) :Float = math.min(self, other)

		/** This byte as a natural number.
		  * @throws ArithmeticException if this number is negative or greater than `Int.MaxValue`.
		  * @return `this.toInt.toNatural`.
		  */
		@inline def toNatural :Natural = Natural(self.toInt)
	}

	/** Exposes methods `max` as `atLeast` and `min` as `atMost`.
	  * Standard `max n` and `min n` can be confusing, as in everyday language they has a meaning of
	  * "not more than n" and "at least n", which is the opposite of what those functions do.
	  */
	class DoubleExtension private[extensions] (private val self :Double) extends AnyVal {
		/** Square root of this `Double`. */
		@inline def sqrt :Double = math.sqrt(self)

		/** Cube root of this `Double`. */
		@inline def cbrt :Double = math.cbrt(self)

		/** Raises this `Double` to the given power. */
		@inline def pow(exp :Double) :Double = math.pow(self, exp)

		/** Raises this `Double` to the given power. */
		@inline def **(exp :Double) :Double = math.pow(self, exp)

		/** Returns `this max other`. */
		@inline def atLeast(other :Double) :Double = math.max(self, other)

		/** Returns `this min other`. */
		@inline def atMost(other :Double) :Double = math.min(self, other)

		/** This byte as a natural number.
		  * @throws ArithmeticException if this number is negative or greater than `Int.MaxValue`.
		  * @return `this.toInt.toNatural`.
		  */
		@inline def toNatural :Natural = Natural(self.toInt)
	}

	/** Exposes methods `max` as `atLeast` and `min` as `atMost`.
	  * Standard `max n` and `min n` can be confusing, as in everyday language they has a meaning of
	  * "not more than n" and "at least n", which is the opposite of what those functions do.
	  */
	class hasOrderingExtension[T](private val self :T) extends AnyVal {
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
		@inline final def random(implicit gen :Random) :Boolean = gen.self.nextInt(2) == 0
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
	implicit override object BigIntegerIsIntegral extends BigIntegerIsNumeric with Integral[BigInteger] {
		override def quot(x :BigInteger, y :BigInteger) :BigInteger = x divide y
		override def rem(x :BigInteger, y :BigInteger) :BigInteger = x remainder y
	}
	@SerialVersionUID(Ver)
	object BigIntegerAsIfFractional extends BigIntegerIsNumeric with Fractional[BigInteger] {
		override def div(x :BigInteger, y :BigInteger) :BigInteger = x divide y
	}
	
	
	@SerialVersionUID(Ver)
	class JBigDecimalIsFractional(implicit ctx :MathContext) extends Fractional[JavaBigDecimal] {
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
	
	
	final val JBigDecimal128IsFractional = new JBigDecimalIsFractional()(MathContext.DECIMAL128)
}
