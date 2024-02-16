package net.noresttherein.sugar.numeric

import java.{lang => jl}
import java.lang.{Math => math}
import java.math.{BigInteger, MathContext, BigDecimal => JavaBigDecimal}
import java.util.Comparator

import scala.math.Numeric.{ByteIsIntegral, DoubleIsFractional, FloatIsFractional, IntIsIntegral, LongIsIntegral, ShortIsIntegral}
import scala.util.Random

import net.noresttherein.sugar.exceptions.SugaredArithmeticException
import net.noresttherein.sugar.extensions.OptionExtension
import net.noresttherein.sugar.numeric.Decimal64.{BigDecimalConverter, JavaBigDecimalConverter}
import net.noresttherein.sugar.numeric.extensions.{BooleanCompanionExtension, BooleanExtension, ByteCompanionExtension, ByteExtension, CharCompanionExtension, CharExtension, ComparableExtension, ComparatorHasAsScala, DoubleCompanionExtension, DoubleExtension, FloatCompanionExtension, FloatExtension, IntCompanionExtension, IntExtension, JBigDecimalIsFractional, LongCompanionExtension, LongExtension, ShortCompanionExtension, ShortExtension, hasComparatorExtension, hasOrderingExtension}
import net.noresttherein.sugar.numeric.BigRatio.BigIntNumerator
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




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
	@inline implicit final def ByteAsIntExtension(self :Byte) :IntExtension = new IntExtension(self)
	@inline implicit final def ShortExtension(self :Short) :ShortExtension = new ShortExtension(self)
	@inline implicit final def ShortAsIntExtension(self :Short) :IntExtension = new IntExtension(self)
	@inline implicit final def CharExtension(self :Char) :CharExtension = new CharExtension(self)
	@inline implicit final def IntExtension(self :Int) :IntExtension = new IntExtension(self)
	@inline implicit final def LongExtension(self :Long) :LongExtension = new LongExtension(self)
	@inline implicit final def FloatExtension(self :Float) :FloatExtension = new FloatExtension(self)
	@inline implicit final def DoubleExtension(self :Double) :DoubleExtension = new DoubleExtension(self)
	@inline implicit final def hasOrderingExtension[T](self :T) :hasOrderingExtension[T] =
		new hasOrderingExtension(self)

	/** Adds `random` extension methods to `Byte` singleton object. */
	@inline implicit final def ByteCompanionExtension(self :Byte.type) :ByteCompanionExtension = new ByteCompanionExtension {}

	/** Adds `random` extension methods to `Short` singleton object. */
	@inline implicit final def ShortCompanionExtension(self :Short.type) :ShortCompanionExtension = new ShortCompanionExtension {}

	/** Adds `random` extension methods to `Char` singleton object. */
	@inline implicit final def CharCompanionExtension(self :Char.type) :CharCompanionExtension = new CharCompanionExtension {}

	/** Adds `random` extension methods to `Int` singleton object. */
	@inline implicit final def IntCompanionExtension(self :Int.type) :IntCompanionExtension = new IntCompanionExtension {}

	/** Adds `random` extension methods to `Long` singleton object. */
	@inline implicit final def LongCompanionExtension(self :Long.type) :LongCompanionExtension = new LongCompanionExtension {}

	/** Adds `random` extension methods to `Float` singleton object. */
	@inline implicit final def FloatCompanionExtension(self :Float.type) :FloatCompanionExtension =
		new FloatCompanionExtension {}

	/** Adds `random` extension methods to `Double` singleton object. */
	@inline implicit final def DoubleCompanionExtension(self :Double.type) :DoubleCompanionExtension =
		new DoubleCompanionExtension {}

	/** Adds `random` extension methods to `Boolean` singleton object. */
	@inline implicit final def BooleanCompanionExtension(self :Boolean.type) :BooleanCompanionExtension =
		new BooleanCompanionExtension {}

	@inline implicit final def BigIntegerExtension(self :BigInteger) :BigIntNumerator = new BigIntNumerator(self)
	@inline implicit final def BigIntExtension(self :BigInt) :BigIntNumerator = new BigIntNumerator(self)

	@inline implicit final def ComparableExtension[T <: Comparable[T]](self :T) :ComparableExtension[T] =
		new  ComparableExtension(self)

	@inline implicit final def hasComparatorExtension[T](self :T)
	                                                    (implicit comparator :Comparator[T]) :hasComparatorExtension[T] =
		new hasComparatorExtension(self)

	@inline implicit final def ComparatorHasAsScala[T](self :Comparator[T]) :ComparatorHasAsScala[T] =
		new ComparatorHasAsScala(self)
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
		@inline final def toUInt :UInt = new UInt(if (self) 1 else 0)
		/** `1` if this `Boolean` is `ture`, or `0` otherwise. */
		@inline final def toULong :ULong = new ULong(if (self) 1L else 0L)
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
		@inline def toUInt :UInt = new UInt(self)

		/** This byte as a natural number.
		  * @throws ArithmeticException if this number is negative.
		  */
		@inline def toULong :ULong = new ULong(self)
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
				var i   = 32 - jl.Integer.numberOfLeadingZeros(exp) //< 32 because exp >= 0
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

		/** Addition which clips to `Int.MinValue`/`Int.MaxValue` instead of overflowing/underflowing. */
		@inline def +~(other :Int) :Int = {
			val sum = self + other
			val selfLtZero  = self >> 31
			val otherLtZero = other >> 31
			val sumLtZero   = sum >> 31
			val sumGteZero  = ~sum >> 31
			~self >> 31 & ~other >> 31 & (sumGteZero & sum | sumLtZero & Int.MaxValue) | //if self >= 0 && other >= 0
				selfLtZero & otherLtZero & (sumLtZero & sum | sumGteZero & Int.MinValue) | //if self < 0 && other < 0
				(selfLtZero ^ otherLtZero) & sum
		}

		/** Subtraction which clips to `Int.MinValue`/`Int.MaxValue` instead of overflowing/underflowing. */
		@inline def -~(other :Int) :Int = { //Can't just this +~ -other, because -Int.MinValue == Int.MinValue
			val sum = self - other
			val sumLtZero   = sum >> 31
			val sumGteZero  = ~sum >> 31
			val selfLtZero  = self >> 31
			val selfGteZero = ~self >> 31
			val otherLtZero = other >> 31
			selfGteZero & otherLtZero & (sumGteZero & sum | sumLtZero & Int.MaxValue) |
				selfLtZero & ~other >> 31 & (sumLtZero & sum | sumGteZero & Int.MinValue) |
				(selfLtZero ^ otherLtZero) & sum
		}


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

		/** Returns this `Int`, or `0` if the condition is false. */
		@inline def orZeroIf(condition :Boolean) :Int = if (condition) 0 else self

		/** Returns this `Int`, or `0` if it does not satisfy the predicate. */
		@inline def orZeroIf(condition :Int => Boolean) :Int = if (condition(self)) 0 else self


		/** True if this `Int` has no divisors other than `1` and `2`. */
		@inline def isPowerOf2 :Boolean = jl.Integer.bitCount(self) == 1

		/** The smallest in absolute value power of 2 greater or equal than the absolute value of this `Int`.
		  * If `this >= 0`, then the result will be greater or equal to `this`; otherwise, it will be lesser or equal
		  * to `this`.
		  * @note the method does not check for arithmetic overflow.
		  */
		@inline def nextPowerOf2 :Int = {
			val lowerPowerOf2 = jl.Integer.highestOneBit(self)
			if (lowerPowerOf2 == self) self else lowerPowerOf2 << 1
		}

		/** The number of digits in this number's decimal expansion. */
		@inline def digitCount :Int = { //adapted from java.math.BigDecimal.longDigitLength
			val x = if (self < 0) -self else self
			if (x < 10L)
				1
			else {
				val log = ((32 - java.lang.Integer.numberOfLeadingZeros(x) + 1) * 1233) >>> 12
				// if r >= length, must have max possible digits for long// if r >= length, must have max possible digits for long
				if (log >= IntPrecision || self < IntPowersOf10(log)) log
				else log + 1
			}
		}

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

		/** Converts this `Int` to an unsigned `UInt`. */
		@throws[ArithmeticException]("if this Int is negative")
		@inline def toUIntExact :UInt = UInt.from(self)

		/** Reinterprets this `Int`'s binary format as an unsigned 32 bit integer.
		  * Negative values will be converted to values greater than `Int.MaxValue`.
		  */
		@inline def toUInt :UInt = new UInt(self)

		/** Converts this `Int` to an unsigned `ULong`. */
		@throws[ArithmeticException]("if this Int is negative")
		@inline def toULongExact :ULong = ULong.from(self)

		/** Converts  this `Int` to a 64 bit unsigned integer.
		  * Negative values will result in underflowing to values greater than `Int.MaxValue`.
		  */
		@inline def toULong :ULong = new ULong(self & 0xffffffffL)

		/** Converts this `Int` to an unsigned `UInt`. */
		@throws[ArithmeticException]("if this Int is negative")
		@inline def unsigned :UInt = UInt.from(self)

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
				var i   = jl.Integer.numberOfLeadingZeros(exp) //< 32 because exp >= 0
				while (i >= 0) {
					res = res * res
					if ((exp >> i & 1) == 1)
						res = res * self
					i -= 1
				}
				res
			}

		/** Raises this `Long` to the given power. If the argument is negative, `0` is returned. */
		@inline def **(exp :Int) :Long = pow(exp)

		/** Addition which clips to `Long.MinValue`/`Long.MaxValue` instead of overflowing/underflowing. */
		@inline def +~(other :Long) :Long = {
			val sum = self + other
			val selfLtZero  = self >> 63
			val otherLtZero = other >> 63
			val sumLtZero   = sum >> 63
			val sumGteZero  = ~sum >> 63
			~self >> 63 & ~other >> 63 & (sumGteZero & sum | sumLtZero & Long.MaxValue) | //if self >= 0 && other >= 0
				selfLtZero & otherLtZero & (sumLtZero & sum | sumGteZero & Long.MinValue) | //if self < 0 && other < 0
				(selfLtZero ^ otherLtZero) & sum
		}

		/** Subtraction which clips to `Long.MinValue`/`Long.MaxValue` instead of overflowing/underflowing. */
		@inline def -~(other :Long) :Long = { //Can't just this +~ -other, because -Long.MinValue == Long.MinValue
			val sum = self - other
			val sumLtZero   = sum >> 63
			val sumGteZero  = ~sum >> 63
			val selfLtZero  = self >> 63
			val selfGteZero = ~self >> 63
			val otherLtZero = other >> 63
			selfGteZero & otherLtZero & (sumGteZero & sum | sumLtZero & Long.MaxValue) |
				selfLtZero & ~other >> 63 & (sumLtZero & sum | sumGteZero & Long.MinValue) |
				(selfLtZero ^ otherLtZero) & sum
		}

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

		/** Returns this `Long`, or `0` if the condition is false. */
		@inline def orZeroIf(condition :Boolean) :Long = if (condition) 0 else self

		/** Returns this `Long`, or `0` if it does not satisfy the predicate. */
		@inline def orZeroIf(condition :Long => Boolean) :Long = if (condition(self)) 0 else self


		/** True if this `Long` has no divisors other than `1` and `2`. */
		@inline def isPowerOf2 :Boolean = jl.Long.bitCount(self) == 1

		/** The smallest in absolute value power of 2 greater or equal than the absolute value of this `Long`.
		  * If `this >= 0`, then the result will be greater or equal to `this`
		  * otherwise, it will be lesser or equal to `this`.
		  * @note the method does not check for arithmetic overflow.
		  */
		@inline def nextPowerOf2 :Long = {
			val lowerPowerOf2 = jl.Long.highestOneBit(self)
			if (lowerPowerOf2 == self) self else lowerPowerOf2 << 1
		}

		/** The number of digits in this number's decimal expansion. */
		@inline def digitCount :Int = { //adapted from java.math.BigDecimal.longDigitLength
			val x = if (self < 0) -self else self
			if (x < 10L)
				1
			else {
				val log = ((64 - java.lang.Long.numberOfLeadingZeros(x) + 1) * 1233) >>> 12
				// if r >= length, must have max possible digits for long// if r >= length, must have max possible digits for long
				if (log >= LongPrecision || self < LongPowersOf10(log)) log
				else log + 1
			}
		}

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

		/** Converts this `Long` to an unsigned `UInt`. */
		@throws[ArithmeticException]("if this Long is negative or greater than Int.MaxValue")
		@inline def toUIntExact :UInt =
			if (self > Int.MaxValue | self < 0) throw SugaredArithmeticException("Not an unsigned Int: " + self)
			else new UInt(self.toInt)

		/** Interprets lower 4 bytes in this `Long`'s binary format as an unsigned 32 bit integer.
		  * Negative values will be converted to values greater than `Int.MaxValue`.
		  */
		@inline def toUInt :UInt = new UInt(self.toInt)

		/** Converts this `Long` to an unsigned `ULong`. */
		@throws[ArithmeticException]("if this Long is negative")
		@inline def toULongExact :ULong = ULong.from(self)

		/** Reinterprets this `Long`'s binary format as an unsigned 64 bit integer.
		  * Negative values will be converted to values greater than `Long.MaxValue`.
		  */
		@inline def toULong :ULong = new ULong(self)

		/** Converts this `Long` to an unsigned `ULong`. */
		@throws[ArithmeticException]("if this Long is negative")
		@inline def unsigned :ULong = ULong.from(self)

		/** Converts this `Long` into an overflow checking `SafeLong`. */
		@inline def safe :SafeLong = new SafeLong(self)

		/** Converts this `Long` into an overflow checking `SafeInt`. */
		@throws[ArithmeticException]("if this Long does not fit in the [Int.MinValue, Int.MaxValue] range.")
		@inline def toSafeInt :SafeInt = new SafeLong(self).toSafeInt

		/** Converts this `Long` into an overflow checking `SafeLong`. */
		@inline def toSafeLong :SafeLong = new SafeLong(self)

		/** Converts this `Long` to a decimal number, rounding if necessary. */
		@inline def toDecimal64 :Decimal64 = Decimal64.round(self)
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

		/** Converts this `Float` to a decimal number, rounding if necessary. */
		@inline def toDecimal64 :Decimal64 = Decimal64.round(self)
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

		/** Converts this `Double` to a decimal number, rounding if necessary. */
		@inline def toDecimal64 :Decimal64 = Decimal64.round(self)
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


	sealed trait ByteCompanionExtension extends Any {
		/** A random `Byte` retrieved from the implicit `Random`. */
		@inline final def random(implicit gen :Random) :Byte = gen.self.nextInt(0xff).toByte

		/** A random `Byte` from range `[0, bound)`, retrieved from the implicit `Random`. */
		@inline final def random(bound :Byte)(implicit gen :Random) :Byte = gen.self.nextInt(bound).toByte

		/** A random `Byte` from range `[from, until)`, retrieved from the implicit `Random`. */
		@inline final def random(from :Byte, until :Byte)(implicit gen :Random) :Byte =
			gen.self.nextInt(from, until).toByte

		/** Parses a `Byte` value from the `String`
		  * @return [[scala.math.Numeric.ByteIsIntegral.parseString ByteIsIntegral.parseString]].
		  */
		@inline final def parse(string :String) :Maybe[Byte] = ByteIsIntegral.parseString(string).toMaybe

		/** Parses a `Byte` value from the `String`
		  * @return [[java.lang.Byte.parseByte Byte.parseByte]]`(string)`.
		  */
		@inline final def parse(string :String, radix :Int) :Maybe[Byte] =
			try Yes(jl.Byte.parseByte(string, radix)) catch {
				case _ :NumberFormatException => No
			}
		
		/** Parses a `Byte` value from the `String`
		  * @return [[java.lang.Byte.parseByte Byte.parseByte]]`(string)`.
		  */
		@inline final def apply(string :String, radix :Int = 10) :Byte = jl.Byte.parseByte(string, radix)
	}

	sealed trait ShortCompanionExtension extends Any {
		/** A random `Short` retrieved from the implicit `Random`. */
		@inline final def random(implicit gen :Random) :Short = gen.self.nextInt(0xffff).toShort

		/** A random `Short` from range `[0, bound)`, retrieved from the implicit `Random`. */
		@inline final def random(bound :Short)(implicit gen :Random) :Short = gen.self.nextInt(bound).toShort

		/** A random `Short` from range `[from, until)`, retrieved from the implicit `Random`. */
		@inline final def random(from :Short, until :Short)(implicit gen :Random) :Short =
			gen.self.nextInt(from, until).toShort

		/** Parses a `Short` value from the `String`
		  * @return [[scala.math.Numeric.ShortIsIntegral.parseString ShortIsIntegral.parseString]]`(string)`.
		  */
		@inline final def parse(string :String) :Maybe[Short] = ShortIsIntegral.parseString(string).toMaybe

		/** Parses a `Short` value from the `String`
		  * @return [[java.lang.Short.parseShort Short.parseShort]]`(string)`.
		  */
		@inline final def parse(string :String, radix :Int) :Maybe[Short] =
			try Yes(jl.Short.parseShort(string, radix)) catch {
				case _ :NumberFormatException => No
			}

		/** Parses a `Short` value from the `String`
		  * @return [[java.lang.Short.parseShort Short.parseShort]]`(string)`.
		  */
		@inline final def apply(string :String, radix :Int = 10) :Short = jl.Short.parseShort(string, radix)
	}

	sealed trait CharCompanionExtension extends Any {
		/** A random `Char` retrieved from the implicit `Random`. */
		@inline final def random(implicit gen :Random) :Char = gen.self.nextInt(0xffff).toChar

		/** A random `Char` from range `[0, bound)`, retrieved from the implicit `Random`. */
		@inline final def random(bound :Char)(implicit gen :Random) :Char = gen.self.nextInt(bound).toChar

		/** A random `Char` from range `[from, until)`, retrieved from the implicit `Random`. */
		@inline final def random(from :Char, until :Char)(implicit gen :Random) :Char =
			gen.self.nextInt(from, until).toChar
	}

	sealed trait IntCompanionExtension extends Any {
		/** Returns `implicitly[Random].nextInt`. */
		@inline final def random(implicit gen :Random) :Int = gen.self.nextInt

		/** Returns `implicitly[Random].nextInt(bound)`. The argument must be positive. */
		@inline final def random(bound :Int)(implicit gen :Random) :Int = gen.self.nextInt(bound)

		/** Returns `implicitly[Random].nextInt(from, until)`. */
		@inline final def random(from :Int, until :Int)(implicit gen :Random) :Int = gen.self.nextInt(from, until)

		/** Parses an `Int` value from the `String`
		  * @return [[scala.math.Numeric.IntIsIntegral.parseString IntIsIntegral.parseString]]`(string)`.
		  */
		@inline final def parse(string :String) :Maybe[Int] = IntIsIntegral.parseString(string).toMaybe

		/** Parses a `Short` value from the `String`
		  * @return [[scala.math.Numeric.ShortIsIntegral.parseString ShortIsIntegral.parseString]]`(string)`.
		  */
		@inline final def parse(string :String, radix :Int) :Maybe[Int] =
			try Yes(jl.Integer.parseInt(string, radix)) catch {
				case _ :NumberFormatException => No
			}
//
//		/** Parses a `Int` value from the `CharSequence`
//		  * @see [[java.lang.Integer.parseInt Integer.parseInt]].
//		  */
//		@inline final def parse(chars :CharSequence, from :Int, until :Int, radix :Int) :Int =
//			jl.Integer.parseInt(chars, from, until, radix)

		/** Parses a `Int` value from the `String`
		  * @return [[java.lang.Integer.parseInt Integer.parseInt]]`(string)`.
		  */
		@inline final def apply(string :String, radix :Int = 10) :Int = jl.Integer.parseInt(string, radix)
	}

	sealed trait LongCompanionExtension extends Any {
		/** Returns `implicitly[Random].nextLong`. */
		@inline final def random(implicit gen :Random) :Long = gen.self.nextLong

		/** Returns `implicitly[Random].nextLong(bound)`. The argument must be positive. */
		@inline final def random(bound :Long)(implicit gen :Random) :Long = gen.self.nextLong(bound)

		/** Returns `implicitly[Random].nextLong(from, until)`. */
		@inline final def random(from :Long, until :Long)(implicit gen :Random) :Long = gen.self.nextLong(from, until)

		/** Parses a `Long` value from the `String`
		  * @see [[scala.math.Numeric.LongIsIntegral.parseString LongIsIntegral.parseString]]`(string)`.
		  */
		@inline final def parse(string :String) :Maybe[Long] = LongIsIntegral.parseString(string).toMaybe

		/** Parses a `Long` value from the `String`
		  * @return [[java.lang.Long.parseLong Long.parseLong]]`(string)`.
		  */
		@inline final def parse(string :String, radix :Int) :Maybe[Long] =
			try Yes(jl.Long.parseLong(string, radix)) catch {
				case _ :NumberFormatException => No
			}
//
//		/** Parses a `Long` value from the `CharSequence`
//		  * @see [[java.lang.Long.parseLong Long.parseLong]].
//		  */
//		@inline final def parse(chars :CharSequence, from :Int, until :Int, radix :Int) :Long =
//			jl.Long.parseLong(chars, from, until, radix)

		/** Parses a `Long` value from the `String`
		  * @return [[java.lang.Long.parseLong Long.parseLong]]`(string)`.
		  */
		@inline final def apply(string :String, radix :Int = 10) :Long = jl.Long.parseLong(string, radix)
	}

	sealed trait FloatCompanionExtension extends Any {
		/** Returns `implicitly[Random].nextFloat`. */
		@inline final def random(implicit gen :Random) :Float = gen.self.nextFloat

		/** Returns `implicitly[Random].nextFloat(bound)`. The argument must be positive. */
		@inline final def random(bound :Float)(implicit gen :Random) :Float = gen.self.nextFloat(bound)

		/** Returns `implicitly[Random].nextFloat(from, until)`. */
		@inline final def random(from :Float, until :Float)(implicit gen :Random) :Float = gen.self.nextFloat(from, until)

		/** Parses a `Float` value from the `String`
		  * @return [[scala.math.Numeric.FloatIsFractional.parseString FloatIsFractional.parseString]]`(string)`.
		  */
		@inline final def parse(string :String) :Maybe[Float] = FloatIsFractional.parseString(string).toMaybe

		/** Parses a `Float` value from the `String`
		  * @return [[java.lang.Float.parseFloat parseFloat]]`(string)`.
		  */
		@inline final def apply(string :String) :Float = jl.Float.parseFloat(string)
	}

	sealed trait DoubleCompanionExtension extends Any {
		/** Returns `implicitly[Random].nextDouble`. */
		@inline final def random(implicit gen :Random) :Double = gen.self.nextDouble

		/** Returns `implicitly[Random].nextDouble(bound)`. The argument must be positive. */
		@inline final def random(bound :Double)(implicit gen :Random) :Double = gen.self.nextDouble(bound)

		/** Returns `implicitly[Random].nextDouble(from, until)`. */
		@inline final def random(from :Double, until :Double)(implicit gen :Random) :Double =
			gen.self.nextDouble(from, until)

		/** Parses a `Double` value from the `String`
		  * @return [[scala.math.Numeric.DoubleIsFractional.parseString DoubleIsFactional.parseString]]`(string)`.
		  */
		@inline final def parse(string :String) :Maybe[Double] = DoubleIsFractional.parseString(string).toMaybe

		/** Parses a `Double` value from the `String`
		  * @return [[java.lang.Double.parseDouble Double.parseDouble]]`(string)`.
		  */
		@inline final def apply(string :String) :Double = jl.Double.parseDouble(string)
	}

	sealed trait BooleanCompanionExtension extends Any {
		/** A random `Boolean` retrieved from the implicit `Random`. */
		@inline final def random(implicit gen :Random) :Boolean = gen.self.nextInt(2) == 0

		/** Tests if the given string equals `"true"`, ignoring case. */
		@inline final def parse(string :String) :Boolean = jl.Boolean.parseBoolean(string)
	}



	/** Standard magnitude comparison extension methods for a [[java.util.Comparable Comparable]]`[T]`. */
	class ComparableExtension[T <: Comparable[T]] private[numeric] (private val self :T) extends AnyVal {
		@inline def compare(other :T) :Int = self compareTo other
		@inline def min(other :T) :T = if (self.compareTo(other) <= 0) self else other
		@inline def max(other :T) :T = if (self.compareTo(other) >= 0) self else other
		@inline def < (other :T) :Boolean = self.compareTo(other) < 0
		@inline def <=(other :T) :Boolean = self.compareTo(other) <= 0
		@inline def > (other :T) :Boolean = self.compareTo(other) > 0
		@inline def >=(other :T) :Boolean = self.compareTo(other) >= 0
	}

	/** Standard magnitude comparison extension methods for any type with an implicit [[java.util.Comparator Comparator]]. */
	class hasComparatorExtension[T] private[numeric](private val self :T) extends AnyVal {
		@inline def compare(other :T)(implicit comparator :Comparator[T]) :Int = comparator.compare(self, other)

		@inline def min(other :T)(implicit comparator :Comparator[T]) :T =
			if (comparator.compare(self, other) <= 0) self else other

		@inline def max(other :T)(implicit comparator :Comparator[T]) :T =
			if (comparator.compare(self, other) >= 0) self else other

		@inline def < (other :T)(implicit comparator :Comparator[T]) :Boolean = comparator.compare(self, other) < 0
		@inline def <=(other :T)(implicit comparator :Comparator[T]) :Boolean = comparator.compare(self, other) <= 0
		@inline def > (other :T)(implicit comparator :Comparator[T]) :Boolean = comparator.compare(self, other) > 0
		@inline def >=(other :T)(implicit comparator :Comparator[T]) :Boolean = comparator.compare(self, other) >= 0
	}

	/** Extension method `asScala` converting this [[java.util.Comparator Comparator]] to an [[Ordering Ordering]]. */
	class ComparatorHasAsScala[T] private[numeric] (private val self :Comparator[T]) extends AnyVal {
		@inline def asScala :Ordering[T] = Ordering.comparatorToOrdering(self)
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

	private final val IntPrecision   = 9
	private final val IntPowersOf10  = Array.iterate(1, IntPrecision + 1)(_ * 10)
	private final val LongPrecision  = 18
	private final val LongPowersOf10 = Array.iterate(1L, LongPrecision + 1)(_ * 10L)
}
