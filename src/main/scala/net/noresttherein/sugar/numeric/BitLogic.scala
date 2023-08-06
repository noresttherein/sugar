package net.noresttherein.sugar.numeric

import java.lang.{Math => math}

import scala.Specializable.Integral
import scala.reflect.ClassTag

import net.noresttherein.sugar.numeric.BitLogic.BitLogicOps
import net.noresttherein.sugar.reflect.ArrayClass




/** A type class for types which support binary logic. Implicit instances exist for all integral types,
  * as well as for arrays of types which support binary logic.
  * @author Marcin Mościcki
  */
abstract class BitLogic[@specialized(Integral) X] extends Serializable {
	def zero :X
	def isZeros(x :X) :Boolean
	def copy(x :X) :X
	def or (left :X, right :X) :X
	def and(left :X, right :X) :X
	def xor(left :X, right :X) :X
	def xnor(left :X, right :X) :X = not_!(xor(left, right))
	def not(x :X) :X
	def shiftLeft(x :X, n :Long) :X
	def shiftRight(x :X, n :Long) :X
	def signShiftRight(x :X, n :Long) :X
	def clear(x :X, from :Long, until :Long) :X

	/** Same as `or`, but `left` may (or may not) be mutated in the result. */
	def or_! (left :X, right :X) :X
	/** Same as `and`, but `left` may (or may not) be mutated in the result. */
	def and_!(left :X, right :X) :X
	/** Same as `xor`, but `left` may (or may not) be mutated in the result. */
	def xor_!(left :X, right :X) :X
	/** Same as `xnor`, but `left` may (or may not) be mutated in the result. */
	def xnor_!(left :X, right :X) :X = not_!(xor_!(left, right))
	/** Same as `not`, but `x` may (or may not) be mutated in the result. */
	def not_!(x :X) :X
	/** Same as `shiftLeft`, but `x` may (or may not) be mutated in the result. */
	def shiftLeft_!(x :X, n :Long) :X
	/** Same as `shiftRight`, but `x` may (or may not) be mutated in the result. */
	def shiftRight_!(x :X, n :Long) :X
	/** Same as `signShiftRight`, but `x` may (or may not) be mutated in the result. */
	def signShiftRight_!(x :X, n :Long) :X
	/** Same as `clear`, but `x` may (or may not) be mutated in the result. */
	def clear_!(x :X, from :Long, until :Long) :X


	//todo: implementations which treat left and right as continuous bit streams
	def orAllInPlace(left :Array[X], right :Array[X]) :Unit = {
		val min = math.min(left.length, right.length)
		var i = 0
		while (i < min) {
			left(i) = or_!(left(i), right(i))
			i += 1
		}
	}
	def andAllInPlace(left :Array[X], right :Array[X]) :Unit = {
		val min = math.min(left.length, right.length)
		var i = 0
		while (i < min) {
			left(i) = and_!(left(i), right(i))
			i += 1
		}
	}
	def xorAllInPlace(left :Array[X], right :Array[X]) :Unit = {
		val min = math.min(left.length, right.length)
		var i = 0
		while (i < min) {
			left(i) = xor_!(left(i), right(i))
			i += 1
		}
	}
	def xnorAllInPlace(left :Array[X], right :Array[X]) :Unit = {
		val min = math.min(left.length, right.length)
		var i = 0
		while (i < min) {
			left(i) = xnor_!(left(i), right(i))
			i += 1
		}
	}
	def notAllInPlace(xs :Array[X]) :Unit = {
		var i = xs.length
		while (i  > 0) {
			i -= 1
			xs(i) = not_!(xs(i))
		}
	}

	def shiftAllLeftInPlace(xs :Array[X], n :Long) :Unit =
		if (n < 0) {
			if (n > Long.MinValue)
				shiftAllRightInPlace(xs, -n)
		} else if (n != 0 && n < Long.MaxValue && xs.length != 0) {
			val length = xs.length
			val bitLengths   = new Array[Long](length)
			var getIdx       = length                  //The index of the element in xs with the first not shifted bit.
			var total        = 0L                      //The number of bits preceding element getIdx.
			var getBitLength = 0L                      //The number of bits in xs(getIdx).
			while (getIdx > length && {
				getIdx -= 1
				getBitLength       = bitSizeOf(xs(getIdx))
				bitLengths(getIdx) = getBitLength
				total + getBitLength <= n
			})
				total += getBitLength

			if (getIdx < length) {
				var getOffset    = n - total           //The offset in xs(getIdx) of the first not shifted bit.
				var get          = xs(getIdx)          //The currently read element (first element with not shifted bits).
				var setIdx       = 0                   //The index of the currently written element (target of shift).
				var set          = xs(setIdx)          //The currently updated element.
				var setBitLength = bitLengths(0)       //The number of bits in set.
				var setOffset    = 0L                  //The next bit to write in set.
				while (getIdx < length) {
					if (getOffset == 0L & setOffset == 0L & getBitLength == setBitLength) {
						xs(setIdx) = get
						xs(getIdx) = set
						getOffset += getBitLength
						setOffset += setBitLength
					} else {
						val next    = shiftLeft(get, getOffset)
						val aligned = shiftRight_!(next, setOffset)
						set         = or_!(set, aligned)
						val shift   = math.min(getBitLength - getOffset, setBitLength - setOffset)
						setOffset  += shift
						getOffset  += shift
					}
					if (getOffset == getBitLength) {
						getIdx += 1
						if (getIdx < length) {
							get                = xs(getIdx)
							getBitLength       = bitSizeOf(get)
							bitLengths(getIdx) = getBitLength
							getOffset     = 0
						}
					}
					if (setOffset == setBitLength) {
						xs(setIdx)    = set
						setIdx       += 1
						set           = xs(setIdx)
						setBitLength  = bitLengths(setIdx)
					}
				}
				xs(setIdx) = set
			}
		}

	def shiftAllRightInPlace(xs :Array[X], n :Long) :Unit =
		if (n < 0) {
			if (n > Long.MinValue)
				shiftAllRightInPlace(xs, -n)
		} else if (n != 0 && n < Long.MaxValue && xs.length != 0) {
			val length = xs.length
			val bitLengths   = new Array[Long](length)
			var getIdx       = length                  //The index of the element in xs with the first not shifted bit.
			var total        = 0L                      //The number of bits preceding element getIdx.
			var getBitLength = 0L                      //The number of bits in xs(getIdx).
			while (getIdx > 0 && {
				getIdx -= 1
				getBitLength       = bitSizeOf(xs(getIdx))
				bitLengths(getIdx) = getBitLength
				total + getBitLength <= n
			}) {
				total += getBitLength
			}
			if (getIdx >= 0) {
				var get          = xs(getIdx)          //The currently read element (first element with not shifted bits).
				var getOffset    = n - total           //The offset from the right in get of the first not shifted bit.
				var setIdx       = length - 1          //The index of the currently written element (target of shift).
				var set          = xs(setIdx)          //The currently updated element.
				var setBitLength = bitLengths(setIdx)  //The number of bits in set.
				var setOffset    = 0L                  //The next bit from the right to write in set.
				while (getIdx < length) {
					if (getOffset == 0L & setOffset == 0L & getBitLength == setBitLength) {
						xs(setIdx) = get
						xs(getIdx) = set
						getOffset += getBitLength
						setOffset += setBitLength
					} else {
						val next    = shiftRight(get, getOffset)
						val aligned = shiftLeft_!(next, setOffset)
						set         = or_!(set, aligned)
						val shift   = math.min(getBitLength - getOffset, setBitLength - setOffset)
						setOffset  += shift
						getOffset  += shift
					}
					if (getOffset == getBitLength) {
						getIdx -= 1
						if (getIdx >= 0) {
							get                = xs(getIdx)
							getBitLength       = bitSizeOf(get)
							bitLengths(getIdx) = getBitLength
							getOffset          = 0
						}
					}
					if (setOffset == setBitLength) {
						xs(setIdx)    = set
						setIdx       -= 1
						set           = xs(setIdx)
						setBitLength  = bitLengths(setIdx)
					}
				}
				xs(setIdx) = set
			}
		}

	def bitSizeOf(x :X) :Long

	implicit def ops(x :X) :BitLogicOps[X] = new BitLogicOps(x)(this)

	implicit def forArray :BitLogic[Array[X]] = new ArrayBitLogic[X]()(this)
	implicit def classTag :ClassTag[X]
	def typeName :String
	override def toString :String = "BinaryLogic[" + typeName + "]"
}




@SerialVersionUID(Ver)
object BitLogic {
	@inline def apply[X](implicit logic :BitLogic[X]) :logic.type = logic

	@inline def bitSize[X](implicit logic :FixedBitSizeLogic[X]) :Int = bitSize(logic)

	@inline implicit def infixBitLogicOps[@specialized(Integral) X :BitLogic](x :X) :BitLogicOps[X] =
		new BitLogicOps(x)

	final class BitLogicOps[@specialized(Integral) X](self :X)(implicit logic :BitLogic[X]) {
		@inline def |(other :X) :X = logic.or(self, other)
		@inline def &(other :X) :X = logic.and(self, other)
		@inline def ^(other :X) :X = logic.xor(self, other)
		@inline def unary_~ :X = logic.not(self)
		@inline def >>>(n :Int) :X = logic.shiftLeft(self, n)
		@inline def <<(n :Int) :X = logic.shiftRight(self, n)
	}


	/** A variant of `BitLogic` type class for types of a well defined bit size, such as all inbuilt integral types.
	  * @note the implementation assumes that `bitLength <= Long.MaxValue / Int.MaxValue`
	  */
	trait FixedBitSizeLogic[@specialized(Integral) X] extends BitLogic[X] {
		def bitLength :Long
		override def bitSizeOf(x :X) :Long = bitLength
//		def bitLengthOf(array :Array[X]) :Long = array.length.toLong * bitLength

/*
		override def orAllInPlace(left :Array[X], right :Array[X]) :Unit = {
			val min = math.min(left.length, right.length)
			var i = 0
			while (i < min) {
				left(i) = or_!(left(i), right(i))
				i += 1
			}
		}
		override def andAllInPlace(left :Array[X], right :Array[X]) :Unit = {
			val min = math.min(left.length, right.length)
			var i = 0
			while (i < min) {
				left(i) = and_!(left(i), right(i))
				i += 1
			}
		}
		override def xorAllInPlace(left :Array[X], right :Array[X]) :Unit = {
			val min = math.min(left.length, right.length)
			var i = 0
			while (i < min) {
				left(i) = xor_!(left(i), right(i))
				i += 1
			}
		}
		override def notAllInPlace(xs :Array[X]) :Unit = {
			var i = xs.length
			while (i  > 0) {
				i -= 1
				xs(i) = not_!(xs(i))
			}
		}
*/
		override def shiftAllLeftInPlace(xs :Array[X], n :Long) :Unit =
			if (n < 0) {
				if (n < Long.MinValue)
					shiftAllRightInPlace(xs, -n)
			} else if (n != 0 && xs.length != 0) {
				val size      = bitLength
				val elemShift = (n / size).toInt
				val bitShift  = n - elemShift * size
				if (bitShift == 0) {
					var i = 0; val end = xs.length - elemShift
					while (i < end) {
						val src = xs(i + elemShift)
						xs(i + elemShift) = xs(i)
						xs(i) = src
						i += 1
					}
				} else {
					var i = 0; val end = xs.length - elemShift - 1
					while (i < end) {
						val lower = shiftLeft(xs(i + elemShift), size - bitShift)
						val upper = shiftRight(xs(i + elemShift + 1), size - bitShift)
						val or = or_!(lower, upper)
						xs(i) = or
						i += 1
					}
				}
			}
		override def shiftAllRightInPlace(xs :Array[X], n :Long) :Unit =
			if (n < 0) {
				if (n < Long.MinValue)
					shiftAllLeftInPlace(xs, -n)
			} else if (n != 0 && xs.length != 0) {
				val size      = bitLength
				val elemShift = (n / size).toInt
				val bitShift  = n - elemShift * size
				if (bitShift == 0) {
					var i = xs.length - elemShift
					while (i > elemShift) {
						i -= 1
						val src = xs(i - elemShift)
						xs(i - elemShift) = xs(i)
						xs(i) = src
					}
				} else {
					var i = xs.length - elemShift - 1
					while (i > elemShift) {
						val lower = shiftRight(xs(i - elemShift), size - bitShift)
						val upper = shiftLeft(xs(i - elemShift - 1), size - bitShift)
						val or    = or_!(lower, upper)
						xs(i) = or
						i -= 1
					}
				}
			}

		override def forArray :BitLogic[Array[X]] =
			new ArrayBitLogic[X]()(this) with FixedElementSizeArrayLogic[X] {
				override val element = FixedBitSizeLogic.this
			}

	}

	trait ValueTypeBitLogic[@specialized(Integral) X] extends BitLogic[X] {
		override def clear(x :X, from :Long, until :Long) :X = {
			if (from == 0) shiftRight(shiftLeft(x, until), until)
			else {
				val size = bitSizeOf(x)
				if (until == size)
					shiftLeft(shiftRight(x, size - from), size - from)
				else {
					val prefix = shiftLeft(shiftRight(x, size - from), size - from)
					val suffix = shiftRight(shiftLeft(x, until), until)
					or(prefix, suffix)
				}
			}
		}
		override def or_!(left :X, right :X) :X = or(left, right)
		override def and_!(left :X, right :X) :X = and(left, right)
		override def xor_!(left :X, right :X) :X = xor(left, right)
		override def not_!(x :X) :X = not(x)
		override def shiftLeft_!(x :X, n :Long) :X = shiftLeft(x, n)
		override def shiftRight_!(x :X, n :Long) :X = shiftRight(x, n)
		override def signShiftRight_!(x :X, n :Long) :X = signShiftRight(x, n)
		override def clear_!(x :X, from :Long, until :Long) :X = clear(x, from, until)
	}

	@SerialVersionUID(Ver)
	implicit object ByteBitLogic
		extends BitLogic[Byte] with FixedBitSizeLogic[Byte] with ValueTypeBitLogic[Byte]
	{
		@inline override def zero = 0
		@inline override def isZeros(x :Byte) :Boolean = x == 0
		@inline override def copy(x :Byte) :Byte = x
		@inline override def or(left :Byte, right :Byte) :Byte = (left | right).toByte
		@inline override def and(left :Byte, right :Byte) :Byte = (left & right).toByte
		@inline override def xor(left :Byte, right :Byte) :Byte = (left ^ right).toByte
		@inline override def xnor(left :Byte, right :Byte) :Byte = (~(left ^ right)).toByte
		@inline override def not(x :Byte) :Byte = (~x).toByte
		@inline override def shiftLeft(x :Byte, n :Long) :Byte = ((x & 0xff) << n.toInt).toByte
		@inline override def shiftRight(x :Byte, n :Long) :Byte = ((x & 0xff) >>> n.toInt).toByte
		@inline override def signShiftRight(x :Byte, n :Long) :Byte = (x >> n.toInt).toByte
		@inline override def clear(x :Byte, from :Long, until :Long) :Byte = {
			val from0  = from.toInt
			val until0 = until.toInt
			(((x & 0xff) >>> until0 << until0) | ((x & 0xff) >>> 8 - from0 << 8 - from0)).toByte
		}
		@inline override def forArray :ByteArrayBitLogic.type = ByteArrayBitLogic
		@inline override def bitLength = 8
		@inline override def classTag :ClassTag[Byte] = ClassTag.Byte
		@inline override def typeName = "Byte"
	}
	@SerialVersionUID(Ver)
	implicit object ShortBitLogic
		extends BitLogic[Short] with FixedBitSizeLogic[Short] with ValueTypeBitLogic[Short]
	{
		@inline override def zero = 0
		@inline override def isZeros(x :Short) :Boolean = x == 0
		@inline override def copy(x :Short) :Short = x
		@inline override def or(left :Short, right :Short) :Short = (left | right).toShort
		@inline override def and(left :Short, right :Short) :Short = (left & right).toShort
		@inline override def xor(left :Short, right :Short) :Short = (left ^ right).toShort
		@inline override def xnor(left :Short, right :Short) :Short = (~(left ^ right)).toShort
		@inline override def not(x :Short) :Short = (~x).toShort
		@inline override def shiftLeft(x :Short, n :Long) :Short = ((x & 0xffff) << n.toInt).toShort
		@inline override def shiftRight(x :Short, n :Long) :Short = ((x & 0xffff) >>> n.toInt).toShort
		@inline override def signShiftRight(x :Short, n :Long) :Short = (x >> n.toInt).toShort
		@inline override def clear(x :Short, from :Long, until :Long) :Short = {
			val from0  = from.toInt
			val until0 = until.toInt
			(((x & 0xffff) >>> until0 << until0) | ((x & 0xffff) >>> 16 - from0 << 16 - from0)).toShort
		}
		@inline override def forArray :ShortArrayBitLogic.type = ShortArrayBitLogic
		@inline override def bitLength = 16
		@inline override def classTag :ClassTag[Short] = ClassTag.Short
		@inline override def typeName = "Short"
	}
	@SerialVersionUID(Ver)
	implicit object IntBitLogic
		extends BitLogic[Int] with FixedBitSizeLogic[Int] with ValueTypeBitLogic[Int]
	{
		@inline override def zero = 0
		@inline override def isZeros(x :Int) :Boolean = x == 0
		@inline override def copy(x :Int) :Int = x
		@inline override def or(left :Int, right :Int) :Int = left | right
		@inline override def and(left :Int, right :Int) :Int = left & right
		@inline override def xor(left :Int, right :Int) :Int = left ^ right
		@inline override def xnor(left :Int, right :Int) :Int = ~(left ^ right)
		@inline override def not(x :Int) :Int = ~x
		@inline override def shiftLeft(x :Int, n :Long) :Int = x << n.toInt & ~((n.toInt & 32) << 26 >> 31)
		@inline override def shiftRight(x :Int, n :Long) :Int = x >>> n.toInt & ~((n.toInt & 32) << 26 >> 31)
		@inline override def signShiftRight(x :Int, n :Long) :Int = x >> n.toInt & ~(((n.toInt & 32) << 26 & x) >> 31)
		@inline override def clear(x :Int, from :Long, until :Long) :Int = {
			val from0       = from.toInt
			val until0      = until.toInt
			x >>> until0 << until0 & ~((until0 & 32) << 26 >> 31) |
				x >>> 32 - from0 << 32 - from0 & ~((32 - from0 & 32) << 26 >> 31)
		}
		@inline override def forArray :IntArrayBitLogic.type = IntArrayBitLogic
		@inline override def classTag :ClassTag[Int] = ClassTag.Int
		@inline override def bitLength = 32
		@inline override def typeName = "Int"
	}
	@SerialVersionUID(Ver)
	implicit object LongBitLogic
		extends BitLogic[Long] with FixedBitSizeLogic[Long] with ValueTypeBitLogic[Long]
	{
		@inline override def zero = 0L
		@inline override def isZeros(x :Long) :Boolean = x == 0L
		@inline override def copy(x :Long) :Long = x
		@inline override def or(left :Long, right :Long) :Long = left | right
		@inline override def and(left :Long, right :Long) :Long = left & right
		@inline override def xor(left :Long, right :Long) :Long = left ^ right
		@inline override def xnor(left :Long, right :Long) :Long = ~(left ^ right)
		@inline override def not(x :Long) :Long = ~x
		@inline override def shiftLeft(x :Long, n :Long) :Long = x << n & ~((n & 64L) << 57 >> 63)
		@inline override def shiftRight(x :Long, n :Long) :Long = x >>> n & ~((n & 64L) << 57 >> 63)
		@inline override def signShiftRight(x :Long, n :Long) :Long = x >> n & ~(((n & 64L) << 57 & x) >> 63)
		@inline override def clear(x :Long, from :Long, until :Long) :Long = {
			val suffixShift = 64 - from.toInt
			x >>> until << until & ~((until.toInt & 64) << 57 >> 63) |
				x >>> 64 - from << 64 - from & ~((suffixShift & 64) << 57 >> 63)
		}
		@inline override def forArray :LongArrayBitLogic.type = LongArrayBitLogic
		@inline override def classTag :ClassTag[Long] = ClassTag.Long
		@inline override def bitLength = 63
		@inline override def typeName = "Long"
	}


	trait AbstractArrayBitLogic[@specialized(Integral) X] extends BitLogic[Array[X]] {
		override def bitSizeOf(x :Array[X]) :Long = {
			val elem  = element
			var total = 0L
			var i     = x.length
			while (i > 0) {
				i -= 1
				total += elem.bitSizeOf(x(i))
			}
			total
		}

		override def copy(x :Array[X]) :Array[X] = Array.copyOf(x, x.length)
		override def zero :Array[X] = {
			val z = Array.ofDim(1)(element.classTag)
			z(0) = element.zero
			z
		}
		override def isZeros(x :Array[X]) :Boolean = x.forall(element.isZeros)

		override def or(left :Array[X], right :Array[X]) :Array[X] =
			if (left.length >= right.length) {
				val res = copy(left)
				or_!(res, right)
				res
			} else {
				val res = copy(right)
				or_!(res, left)
				res
			}
		override def and(left :Array[X], right :Array[X]) :Array[X] =
			if (left.length >= right.length) {
				val res = copy(left)
				and_!(res, right)
				res
			} else {
				val res = copy(right)
				and_!(res, left)
				res
			}
		override def xor(left :Array[X], right :Array[X]) :Array[X] =
			if (left.length >= right.length) {
				val res = copy(left)
				xor_!(res, right)
				res
			} else {
				val res = copy(right)
				xor_!(res, left)
				res
			}
		override def not(x :Array[X]) :Array[X] = {
			val res = copy(x)
			not_!(res)
			res
		}
		override def shiftLeft(x :Array[X], n :Long) :Array[X] = {
			val res = copy(x)
			shiftLeft_!(res, n)
			res
		}
		override def shiftRight(x :Array[X], n :Long) :Array[X] = {
			val res = copy(x)
			shiftRight_!(res, n)
			res
		}
		override def signShiftRight(x :Array[X], n :Long) :Array[X] = {
			val res = copy(x)
			signShiftRight_!(res, n)
			res
		}
		override def clear(x :Array[X], from :Long, until :Long) :Array[X] = {
			val res = copy(x)
			clear_!(res, from, until)
			res
		}

		override def or_!(left :Array[X], right :Array[X]) :Array[X] = { element.orAllInPlace(left, right); left }
		override def and_!(left :Array[X], right :Array[X]) :Array[X] = { element.andAllInPlace(left, right); left }
		override def xor_!(left :Array[X], right :Array[X]) :Array[X] = { element.xorAllInPlace(left, right); left }
		override def xnor_!(left :Array[X], right :Array[X]) :Array[X] = { element.xnorAllInPlace(left, right); left }
		override def not_!(x :Array[X]) :Array[X] = { element.notAllInPlace(x); x }
		override def shiftLeft_!(x :Array[X], n :Long) :Array[X] = {
			val element       = this.element
			element.shiftAllLeftInPlace(x, n)
			//and now clear the upper `n` bits
			var remainingBits = n
			var i             = x.length
			while (i > 0 & remainingBits > 0) {
				i -= 1
				val elem = x(i)
				val size = element.bitSizeOf(elem)
				if (size <= remainingBits)
					x(i) = element.xor_!(elem, elem)
				else
					x(i) = element.clear_!(x(i), size - remainingBits, size)
				remainingBits -= size
			}
			x
		}
		override def shiftRight_!(x :Array[X], n :Long) :Array[X] = {
			val element       = this.element
			element.shiftAllRightInPlace(x, n)
			//and now clear the lower `n` bits
			val length        = x.length
			var remainingBits = n
			var i             = 0
			while (i < length & remainingBits > 0) {
				i -= 1
				val elem = x(i)
				val size = element.bitSizeOf(elem)
				if (size <= remainingBits)
					x(i) = element.xor_!(elem, elem)
				else
					x(i) = element.clear_!(x(i), 0, remainingBits)
				remainingBits -= size
			}
			x
		}
		override def signShiftRight_!(x :Array[X], n :Long) :Array[X] = {
			val length = x.length
			if (length > 0) {
				val size0 = element.bitSizeOf(x(0))
				if (n <= size0) {
					val lead = element.signShiftRight(x(0), n)
					element.shiftAllRightInPlace(x, n)
					x(0) = lead
				} else {
					val sign = element.signShiftRight(x(0), size0 - 1)
					element.shiftAllRightInPlace(x, n)
					x(0) = sign
					var remainingBits = n
					var i = 1
					if (element.isZeros(sign))
						while (i < length & remainingBits > 0) {
							val elem = x(i)
							val size = element.bitSizeOf(elem)
							if (size <= remainingBits)
								x(i) = element.xor_!(elem, elem)
							else
								x(i) = element.clear_!(elem, 0, remainingBits)
							remainingBits -= 1
							i += 1
						}
					else
						while (i < length & remainingBits > 0) {
							val elem = x(i)
							val size = element.bitSizeOf(elem)
							if (size <= remainingBits)
								x(i) = element.xnor_!(elem, elem)
							else {
								val one = element.xnor(elem, elem)
								x(i) = element.or_!(elem, element.shiftLeft_!(one, size - remainingBits))
							}
							remainingBits -= 1
							i += 1
						}
				}
			}
			x
		}

		override def clear_!(x :Array[X], from :Long, until :Long) :Array[X] =
			if (until <= 0 | until <= from)
				x
			else {
				val element  = this.element
				val length   = x.length
				var bitCount = 0L
				var i = 0
				while (i < length & bitCount < from) {
					val elem = x(i)
					val size = element.bitSizeOf(elem)
					if (bitCount + size >= from)
						if (bitCount + size <= until)
							x(i) = element.clear(elem, from - bitCount, size)
						else
							x(i) = element.clear(elem, from - bitCount, until - bitCount)
					bitCount += size
					i += 1
				}
				while (i < length & bitCount < until) {
					val elem = x(i)
					val size = element.bitSizeOf(elem)
					if (bitCount + size <= until)
						x(i) = element.xor_!(elem, elem)
					else
						x(i) = element.clear(elem, 0, until - bitCount)
					bitCount += size
					i += 1
				}
				x
			}

		implicit override def classTag :ClassTag[Array[X]] = ClassTag(ArrayClass(element.classTag.runtimeClass))
		override def typeName :String = "Array[" + element.typeName + "]"
		protected def element :BitLogic[X]
	}

	trait FixedElementSizeArrayLogic[@specialized(Integral) X] extends AbstractArrayBitLogic[X] {
		override def bitSizeOf(x :Array[X]) :Long = x.length * element.bitLength
		protected override def element :FixedBitSizeLogic[X]
	}


	@SerialVersionUID(Ver)
	implicit object ByteArrayBitLogic extends BitLogic[Array[Byte]] with FixedElementSizeArrayLogic[Byte] {
		protected override val element :ByteBitLogic.type = ByteBitLogic
		override def typeName = "Array[Byte]"
	}
	@SerialVersionUID(Ver)
	implicit object ShortArrayBitLogic extends BitLogic[Array[Short]] with FixedElementSizeArrayLogic[Short] {
		protected override val element :ShortBitLogic.type = ShortBitLogic
		override def typeName = "Array[Short]"
	}
	@SerialVersionUID(Ver)
	implicit object IntArrayBitLogic extends BitLogic[Array[Int]] with FixedElementSizeArrayLogic[Int] {
		protected override val element :IntBitLogic.type = IntBitLogic
		override def typeName = "Array[Int]"
	}
	@SerialVersionUID(Ver)
	implicit object LongArrayBitLogic extends BitLogic[Array[Long]] with FixedElementSizeArrayLogic[Long] {
		protected override val element :LongBitLogic.type = LongBitLogic
		override def typeName = "Array[Long]"
	}

	implicit def ArrayBitLogic[X](implicit logic :BitLogic[X]) :BitLogic[Array[X]] = logic.forArray

	/** Bit logic working on arrays of fixed length. Use of this type class is preferable over the default implicit
	  * [[net.noresttherein.sugar.numeric.BitLogic.ArrayBitLogic ArrayBitLogic]], as it implements
	  * more efficient shifting.
	  */
	def FixedLengthArrayBitLogic[X](length :Int)(implicit logic :FixedBitSizeLogic[X]) :FixedBitSizeLogic[Array[X]] =
		new ArrayBitLogic[X]()(logic) with FixedBitSizeLogic[Array[X]] {
			override def zero :Array[X] = Array.ofDim[X](length)(element.classTag)
			override val bitLength = length * logic.bitLength
			override def typeName = "Array[" + element.typeName + "]|" + length + "|"
		}


	@SerialVersionUID(Ver)
	private sealed class ArrayBitLogic[X](implicit override val element :BitLogic[X])
		extends BitLogic[Array[X]] with AbstractArrayBitLogic[X]
}



