package net.noresttherein.slang.vars

import java.{lang => j}
import java.util.concurrent.atomic
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.function.{IntBinaryOperator, IntUnaryOperator, LongBinaryOperator, LongUnaryOperator}

import net.noresttherein.slang.vars.Var.SpecializedTypes
import net.noresttherein.slang.vars.InOut.{TypeEquiv, TypeIdent}




/** Atomic variables providing several 'test-and-set' operations.
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait Atomic[T] extends InOut[T] with Serializable {

	override def apply(f :T => T) :T = {
		var current = get; var assign = f(current)
		while (!testAndSet(current, assign)) {
			current = get; assign = f(current)
		}
		assign
	}

	override def /:(acc :T)(foldLeft :(T, T) => T) :T = {
		var current = get; var assign = foldLeft(acc, current)
		while (!testAndSet(current, assign)) {
			current = get; assign = foldLeft(acc, assign)
		}
		assign
	}

	override def :\(acc :T)(foldRight :(T, T) => T) :T = {
		var current = get; var assign = foldRight(current, acc)
		while (!testAndSet(current, assign)) {
			current = get; assign = foldRight(assign, acc)
		}
		assign
	}

}



object Atomic {

	/** Create a new atomic variable initialized with the given value. Returned implementation depends on the type
	  * of the argument
	  * @param init
	  * @tparam T
	  * @return
	  */
	def apply[@specialized(SpecializedTypes) T](init :T) :Atomic[T] =
		(new TypeIdent[T] match {
			case InOut.IntEq => new AtomicInt(init.asInstanceOf[Int])
			case InOut.LongEq => new AtomicLong(init.asInstanceOf[Long])
			case InOut.BoolEq => new AtomicBoolean(init.asInstanceOf[Boolean])
			case InOut.DoubleEq => new AtomicDouble(init.asInstanceOf[Double])
			case InOut.FloatEq => new AtomicFloat(init.asInstanceOf[Float])
			case InOut.CharEq => new AtomicChar(init.asInstanceOf[Char])
			case InOut.ByteEq => new AtomicByte(init.asInstanceOf[Byte])
			case InOut.ShortEq => new AtomicShort(init.asInstanceOf[Short])
			case _ => new AtomicRef[T](init)
		}).asInstanceOf[Atomic[T]]








	class AtomicRef[T](x :AtomicReference[T]) extends Atomic[T] {

		def this(init :T) = this(new AtomicReference[T](init))

		override def get :T = x.get
		override def value :T = x.get
		override def value_=(newValue :T) :Unit = x.set(newValue)
		override def :=(newValue :T) :Unit = x.lazySet(newValue)
		override def ?=(newValue :T) :T = x.getAndSet(newValue)

		override def testAndSet(expect :T, assign :T) :Boolean = {
			var current = x.get
			while (current == expect && !x.compareAndSet(current, assign))
				current = x.get
			current == expect
		}



		/************************************** Boolean methods *******************************************************/
		private[vars] override def bool_&=(other :Boolean)(implicit ev :TypeEquiv[T, Boolean]) :Unit = ev(this)(_ & other)
		private[vars] override def bool_|=(other :Boolean)(implicit ev :TypeEquiv[T, Boolean]) :Unit = ev(this)(_ | other)
		private[vars] override def bool_&&=(other : => Boolean)(implicit ev :TypeEquiv[T, Boolean]) :Unit = ev(this)(_ && other)
		private[vars] override def bool_||=(other : => Boolean)(implicit ev :TypeEquiv[T, Boolean]) :Unit = ev(this)(_ && other)
		private[vars] override def bool_^=(other :Boolean)(implicit ev :TypeEquiv[T, Boolean]) :Unit = ev(this)(_ ^ other)
		private[vars] override def bool_!=(implicit ev :TypeEquiv[T, Boolean]) :Boolean = ev(this)(! _)

		/************************************** Int methods ***********************************************************/
		private[vars] override def int_+=(other :Int)(implicit ev :TypeEquiv[T, Int]) :Int = ev(this)(_ + other)
		private[vars] override def int_*=(other :Int)(implicit ev :TypeEquiv[T, Int]) :Int = ev(this)(_ * other)
		private[vars] override def int_/=(other :Int)(implicit ev :TypeEquiv[T, Int]) :Int = ev(this)(_ / other)
		private[vars] override def int_%=(other :Int)(implicit ev :TypeEquiv[T, Int]) :Int = ev(this)(_ % other)
		private[vars] override def int_~(implicit ev :TypeEquiv[T, Int]) :Int = ev(this)(~ _)
		private[vars] override def int_-(implicit ev :TypeEquiv[T, Int]) :Int = ev(this)(- _)
		private[vars] override def int_|=(other :Int)(implicit ev :TypeEquiv[T, Int]) :Int = ev(this)(_ | other)
		private[vars] override def int_&=(other :Int)(implicit ev :TypeEquiv[T, Int]) :Int = ev(this)(_ & other)
		private[vars] override def int_^=(other :Int)(implicit ev :TypeEquiv[T, Int]) :Int = ev(this)(_ ^ other)
		private[vars] override def int_>>=(other :Int)(implicit ev :TypeEquiv[T, Int]) :Int = ev(this)(_ >> other)
		private[vars] override def int_>>>=(other :Int)(implicit ev :TypeEquiv[T, Int]) :Int = ev(this)(_ >>> other)
		private[vars] override def int_<<=(other :Int)(implicit ev :TypeEquiv[T, Int]) :Int = ev(this)(_ << other)

		/*************************************** Long methods *********************************************************/
		private[vars] override def long_+=(other :Long)(implicit ev :TypeEquiv[T, Long]) :Long = ev(this)(_ + other)
		private[vars] override def long_*=(other :Long)(implicit ev :TypeEquiv[T, Long]) :Long = ev(this)(_ * other)
		private[vars] override def long_/=(other :Long)(implicit ev :TypeEquiv[T, Long]) :Long = ev(this)(_ / other)
		private[vars] override def long_%=(other :Long)(implicit ev :TypeEquiv[T, Long]) :Long = ev(this)(_ % other)
		private[vars] override def long_~(implicit ev :TypeEquiv[T, Long]) :Long = ev(this)(~ _)
		private[vars] override def long_-(implicit ev :TypeEquiv[T, Long]) :Long = ev(this)(- _)
		private[vars] override def long_|=(other :Long)(implicit ev :TypeEquiv[T, Long]) :Long = ev(this)(_ | other)
		private[vars] override def long_&=(other :Long)(implicit ev :TypeEquiv[T, Long]) :Long = ev(this)(_ & other)
		private[vars] override def long_^=(other :Long)(implicit ev :TypeEquiv[T, Long]) :Long = ev(this)(_ ^ other)
		private[vars] override def long_>>=(n :Int)(implicit ev :TypeEquiv[T, Long]) :Long = ev(this)(_ >> n)
		private[vars] override def long_>>>=(n :Int)(implicit ev :TypeEquiv[T, Long]) :Long = ev(this)(_ >>> n)
		private[vars] override def long_<<=(n :Int)(implicit ev :TypeEquiv[T, Long]) :Long = ev(this)(_ << n)

		/************************************** Float methods *********************************************************/
		private[vars] override def float_+=(other :Float)(implicit ev :TypeEquiv[T, Float]) :Float = ev(this)(_ + other)
		private[vars] override def float_*=(other :Float)(implicit ev :TypeEquiv[T, Float]) :Float = ev(this)(_ * other)
		private[vars] override def float_/=(other :Float)(implicit ev :TypeEquiv[T, Float]) :Float = ev(this)(_ / other)
		private[vars] override def float_-(implicit ev :TypeEquiv[T, Float]) :Float = ev(this)(- _)

		/************************************** Double methods ********************************************************/
		private[vars] override def double_+=(other :Double)(implicit ev :TypeEquiv[T, Double]) :Double = ev(this)(_ + other)
		private[vars] override def double_*=(other :Double)(implicit ev :TypeEquiv[T, Double]) :Double = ev(this)(_ * other)
		private[vars] override def double_/=(other :Double)(implicit ev :TypeEquiv[T, Double]) :Double = ev(this)(_ / other)
		private[vars] override def double_-(implicit ev :TypeEquiv[T, Double]) :Double = ev(this)(- _)

	}







	class AtomicBoolean(x :atomic.AtomicBoolean) extends Atomic[Boolean] {

		def this(init :Boolean) = this(new atomic.AtomicBoolean(init))


		override def get :Boolean = x.get
		override def value :Boolean = x.get
		override def value_=(value :Boolean) :Unit = x.set(value)
		override def :=(value :Boolean) :Unit = x.lazySet(value)
		override def ?=(newValue :Boolean) :Boolean = x.getAndSet(newValue)

		override def testAndSet(expect :Boolean, assign :Boolean) :Boolean = x.compareAndSet(expect, assign)


		private def stubbornTestAndSet(ifFalse :Boolean, ifTrue :Boolean) :Boolean = {
			while (true) {
				if (x.compareAndSet(true, ifTrue))
					return ifTrue
				if (x.compareAndSet(false, ifFalse))
					return ifFalse
			}
			throw new RuntimeException("unreachable point reached???")
		}

		override def apply(f :Boolean => Boolean) :Boolean = {
			val ifFalse = f(false)
			if (!x.compareAndSet(false, ifFalse)) {
				stubbornTestAndSet(ifFalse, f(true))
			}
			ifFalse
		}

		override def /:(acc :Boolean)(foldLeft :(Boolean, Boolean) => Boolean) :Boolean = {
			val ifFalse = foldLeft(acc, false)
			if (!x.compareAndSet(false, ifFalse)) {
				stubbornTestAndSet(ifFalse, foldLeft(acc, true))
			}
			ifFalse
		}

		override def :\(acc :Boolean)(foldRight :(Boolean, Boolean) => Boolean) :Boolean = {
			val ifFalse = foldRight(false, acc)
			if (!x.compareAndSet(false, ifFalse)) {
				stubbornTestAndSet(ifFalse, foldRight(true, acc))
			}
			ifFalse
		}


		private[vars] override def bool_&=(other :Boolean)(implicit ev :Boolean TypeEquiv Boolean) :Unit =
			if (!other) x.set(false)

		private[vars] override def bool_|=(other :Boolean)(implicit ev :Boolean TypeEquiv Boolean) :Unit =
			if (other) x.set(true)

		private[vars] override def bool_&&=(other: => Boolean)(implicit ev :Boolean TypeEquiv Boolean) :Unit =
			if (x.get && !other) x.set(false)


		private[vars] override def bool_||=(other: =>Boolean)(implicit ev :Boolean TypeEquiv Boolean) :Unit =
			if (!x.get && other) x.set(true)

		private[vars] override def bool_^=(other :Boolean)(implicit ev :Boolean TypeEquiv Boolean) :Unit =
			stubbornTestAndSet(other, !other)

		private[vars] override def bool_!=(implicit ev :Boolean TypeEquiv Boolean) :Boolean =
			stubbornTestAndSet(true, false)

	}






	private[this] val multiplyInt :IntBinaryOperator = (x, y) => x * y
	private[this] val divideInt :IntBinaryOperator = (x, y) => x / y
	private[this] val remainderInt :IntBinaryOperator = (x, y) => x % y
	private[this] val minusInt :IntUnaryOperator = x => -x
	private[this] val orInt :IntBinaryOperator = (x, y) => x | y
	private[this] val andInt :IntBinaryOperator = (x, y) => x & y
	private[this] val xorInt :IntBinaryOperator = (x, y) => x ^ y
	private[this] val notInt :IntUnaryOperator = x => ~x
	private[this] val rightShiftInt :IntBinaryOperator = (x, n) => x >> n
	private[this] val rightLogicShiftInt :IntBinaryOperator = (x, n) => x >>> n
	private[this] val leftShiftInt :IntBinaryOperator = (x, n) => x << n



	class AtomicInt(x :AtomicInteger) extends Atomic[Int] {

		def this(init :Int) = this(new AtomicInteger(init))

		private[Atomic] def atom :AtomicInteger = x

		override def get :Int = x.get
		override def value :Int = x.get
		override def value_=(value :Int) :Unit = x.set(value)
		override def :=(value :Int) :Unit = x.lazySet(value)
		override def ?=(newValue :Int) :Int = x.getAndSet(newValue)


		override def testAndSet(expect :Int, assign :Int) :Boolean = x.compareAndSet(expect, assign)


		override def apply(f :Int => Int) :Int = { x.updateAndGet(f(_)) }

		override def /:(acc :Int)(foldLeft :(Int, Int) => Int) :Int = { x.updateAndGet(foldLeft(acc, _)) }

		override def :\(acc :Int)(foldRight :(Int, Int) => Int) :Int = { x.updateAndGet(foldRight(_, acc)) }




		private[vars] override def int_+=(other :Int)(implicit ev :TypeEquiv[Int, Int]) :Int =
			x.addAndGet(other)

		private[vars] override def int_*=(other :Int)(implicit ev :TypeEquiv[Int, Int]) :Int =
			x.accumulateAndGet(other, multiplyInt)

		private[vars] override def int_/=(other :Int)(implicit ev :TypeEquiv[Int, Int]) :Int =
			x.accumulateAndGet(other, divideInt)

		private[vars] override def int_%=(other :Int)(implicit ev :TypeEquiv[Int, Int]) :Int =
			x.accumulateAndGet(other, remainderInt)

		private[vars] override def int_-(implicit ev :TypeEquiv[Int, Int]) :Int =
			x.updateAndGet(minusInt)

		private[vars] override def int_~(implicit ev :TypeEquiv[Int, Int]) :Int =
			x.updateAndGet(notInt)

		private[vars] override def int_|=(other :Int)(implicit ev :TypeEquiv[Int, Int]) :Int =
			x.accumulateAndGet(other, orInt)

		private[vars] override def int_&=(other :Int)(implicit ev :TypeEquiv[Int, Int]) :Int =
			x.accumulateAndGet(other, andInt)

		private[vars] override def int_^=(other :Int)(implicit ev :TypeEquiv[Int, Int]) :Int =
			x.accumulateAndGet(other, xorInt)

		private[vars] override def int_>>=(other :Int)(implicit ev :TypeEquiv[Int, Int]) :Int =
			x.accumulateAndGet(other, rightShiftInt)

		private[vars] override def int_>>>=(other :Int)(implicit ev :TypeEquiv[Int, Int]) :Int =
			x.accumulateAndGet(other, rightLogicShiftInt)

		private[vars] override def int_<<=(other :Int)(implicit ev :TypeEquiv[Int, Int]) :Int =
			x.accumulateAndGet(other, leftShiftInt)



		/** Increases the value of this variable by the specified number. */
		@inline def +=(n :Int) :Unit = x.addAndGet(n)

		/** Decreases the value of this variable by the specified number. */
		@inline def -=(n :Int) :Unit = x.addAndGet(-n)

		/** Multiplies the value of this variable by the specified number. */
		@inline def *=(n :Int) :Unit = x.accumulateAndGet(n, multiplyInt)

		/** Divides the value of this variable by the specified number. */
		@inline def /=(n :Int) :Unit = x.accumulateAndGet(n, divideInt)

		/** Assigns to this variable the reminder of dividing it by the specified number. */
		@inline def %=(n :Int) :Unit = x.accumulateAndGet(n, remainderInt)

		/** Increments this variable by `1`, C-style. */
		@inline def ++ :Unit = x.incrementAndGet()

		/** Decrements this variable by `1`, C-style. */
		@inline def -- :Unit = x.decrementAndGet()



		/** Increases the value of this variable by the specified number, returning the updated value. */
		@inline def inc(n :Int) :Int = x.addAndGet(n)

		/** Decreases the value of this variable by the specified number, returning the updated value. */
		@inline def dec(n :Int) :Int = x.addAndGet(-n)

		/** Multiplies the value of this variable by the specified number, returning the updated value. */
		@inline def mult(n :Int) :Int = x.accumulateAndGet(n, multiplyInt)

		/** Divides the value of this variable by the specified number, returning the updated value. */
		@inline def div(n :Int) :Int = x.accumulateAndGet(n, divideInt)

		/** Assigns to this variable the reminder of dividing it by the specified number, returning the updated value. */
		@inline def rem(n :Int) :Int = x.accumulateAndGet(n, remainderInt)

		/** Increments this variable by `1`, C-style, returning the updated value. */
		@inline def inc() :Int = x.incrementAndGet()

		/** Decrements this variable by `1`, C-style, returning the updated value. */
		@inline def dec() :Int = x.decrementAndGet()

		/** Sets this variable to minus its value, returning the new (opposite) value. */
		@inline def neg() :Int = x.updateAndGet(minusInt)

		/** Performs bitwise disjunction on this variable with the given number. */
		@inline def |=(n :Int) :Unit = x.accumulateAndGet(n, orInt)

		/** Performs bitwise conjunction on this variable with the given number. */
		@inline def &=(n :Int) :Unit = x.accumulateAndGet(n, andInt)

		/** Performs bitwise exclusive disjunction on this variable with the given number. */
		@inline def ^=(n :Int) :Unit = x.accumulateAndGet(n, xorInt)

		/** Bit-shifts right the value of this variable by the specified number of bits, replacing shifted higher bits with the sign bit. */
		@inline def >>=(n :Int) :Unit = x.accumulateAndGet(n, rightShiftInt)

		/** Bit-shifts right the value of this variable by the specified number of bits, replacing shifted higher bits with zeros. */
		@inline def >>>=(n :Int) :Unit = x.accumulateAndGet(n, rightLogicShiftInt)

		/** Bit-shifts left the value of this variable by the specified number of bits, replacing shifted lower bits with zeros. */
		@inline def <<=(n :Int) :Unit = x.accumulateAndGet(n, leftShiftInt)

		/** Assigns this variable its bitwise negation: `this := !this.value`. */
		@inline def flip() :Unit = x.updateAndGet(notInt)

	}






	private[this] val multiplyLong :LongBinaryOperator = (x, y) => x * y
	private[this] val divideLong :LongBinaryOperator = (x, y) => x / y
	private[this] val remainderLong :LongBinaryOperator = (x, y) => x % y
	private[this] val minusLong :LongUnaryOperator = x => -x
	private[this] val orLong :LongBinaryOperator = (x, y) => x | y
	private[this] val andLong :LongBinaryOperator = (x, y) => x & y
	private[this] val xorLong :LongBinaryOperator = (x, y) => x ^ y
	private[this] val notLong :LongUnaryOperator = x => ~x
	private[this] val rightShiftLong :LongBinaryOperator = (x, n) => x >> n
	private[this] val rightLogicShiftLong :LongBinaryOperator = (x, n) => x >>> n
	private[this] val leftShiftLong :LongBinaryOperator = (x, n) => x << n



	class AtomicLong(x :atomic.AtomicLong) extends Atomic[Long] {

		def this(init :Long) = this(new atomic.AtomicLong(init))

		override def get :Long = x.get
		override def value :Long = x.get
		override def value_=(value :Long) :Unit = x.set(value)
		override def :=(value :Long) :Unit = x.lazySet(value)
		override def ?=(newValue :Long) :Long = x.getAndSet(newValue)


		override def testAndSet(expect :Long, assign :Long) :Boolean = x.compareAndSet(expect, assign)


		override def apply(f :Long => Long) :Long = { x.updateAndGet(f(_)) }

		override def /:(acc :Long)(foldLeft :(Long, Long) => Long) :Long = { x.updateAndGet(foldLeft(acc, _)) }

		override def :\(acc :Long)(foldRight :(Long, Long) => Long) :Long = { x.updateAndGet(foldRight(_, acc)) }




		private[vars] override def long_+=(other :Long)(implicit ev :TypeEquiv[Long, Long]) :Long =
			x.addAndGet(other)

		private[vars] override def long_*=(other :Long)(implicit ev :TypeEquiv[Long, Long]) :Long =
			x.accumulateAndGet(other, multiplyLong)

		private[vars] override def long_/=(other :Long)(implicit ev :TypeEquiv[Long, Long]) :Long =
			x.accumulateAndGet(other, divideLong)

		private[vars] override def long_%=(other :Long)(implicit ev :TypeEquiv[Long, Long]) :Long =
			x.accumulateAndGet(other, remainderLong)

		private[vars] override def long_-(implicit ev :TypeEquiv[Long, Long]) :Long =
			x.updateAndGet(minusLong)

		private[vars] override def long_~(implicit ev :TypeEquiv[Long, Long]) :Long =
			x.updateAndGet(notLong)

		private[vars] override def long_|=(other :Long)(implicit ev :TypeEquiv[Long, Long]) :Long =
			x.accumulateAndGet(other, orLong)

		private[vars] override def long_&=(other :Long)(implicit ev :TypeEquiv[Long, Long]) :Long =
			x.accumulateAndGet(other, andLong)

		private[vars] override def long_^=(other :Long)(implicit ev :TypeEquiv[Long, Long]) :Long =
			x.accumulateAndGet(other, xorLong)

		private[vars] override def long_>>=(other :Int)(implicit ev :TypeEquiv[Long, Long]) :Long =
			x.accumulateAndGet(other, rightShiftLong)

		private[vars] override def long_>>>=(other :Int)(implicit ev :TypeEquiv[Long, Long]) :Long =
			x.accumulateAndGet(other, rightLogicShiftLong)

		private[vars] override def long_<<=(other :Int)(implicit ev :TypeEquiv[Long, Long]) :Long =
			x.accumulateAndGet(other, leftShiftLong)



		/** Increases the value of this variable by the specified number. */
		@inline def +=(n :Long) :Unit = x.addAndGet(n)

		/** Decreases the value of this variable by the specified number. */
		@inline def -=(n :Long) :Unit = x.addAndGet(-n)

		/** Multiplies the value of this variable by the specified number. */
		@inline def *=(n :Long) :Unit = x.accumulateAndGet(n, multiplyLong)

		/** Divides the value of this variable by the specified number. */
		@inline def /=(n :Long) :Unit = x.accumulateAndGet(n, divideLong)

		/** Assigns to this variable the reminder of dividing it by the specified number. */
		@inline def %=(n :Long) :Unit = x.accumulateAndGet(n, remainderLong)

		/** Increments this variable by `1`, C-style. */
		@inline def ++ :Unit = x.incrementAndGet()

		/** Decrements this variable by `1`, C-style. */
		@inline def -- :Unit = x.decrementAndGet()



		/** Increases the value of this variable by the specified number, returning the updated value. */
		@inline def inc(n :Long) :Long = x.addAndGet(n)

		/** Decreases the value of this variable by the specified number, returning the updated value. */
		@inline def dec(n :Long) :Long = x.addAndGet(-n)

		/** Multiplies the value of this variable by the specified number, returning the updated value. */
		@inline def mult(n :Long) :Long = x.accumulateAndGet(n, multiplyLong)

		/** Divides the value of this variable by the specified number, returning the updated value. */
		@inline def div(n :Long) :Long = x.accumulateAndGet(n, divideLong)

		/** Assigns to this variable the reminder of dividing it by the specified number, returning the updated value. */
		@inline def rem(n :Long) :Long = x.accumulateAndGet(n, remainderLong)

		/** Increments this variable by `1`, C-style, returning the updated value. */
		@inline def inc() :Long = x.incrementAndGet()

		/** Decrements this variable by `1`, C-style, returning the updated value. */
		@inline def dec() :Long = x.decrementAndGet()

		/** Sets this variable to minux its value, returning the updated value. */
		@inline def neg() :Long = x.updateAndGet(minusLong)


		/** Performs bitwise disjunction on this variable with the given number. */
		@inline def |=(n :Long) :Unit = x.accumulateAndGet(n, orLong)

		/** Performs bitwise conjunction on this variable with the given number. */
		@inline def &=(n :Long) :Unit = x.accumulateAndGet(n, andLong)

		/** Performs bitwise exclusive disjunction on this variable with the given number. */
		@inline def ^=(n :Long) :Unit = x.accumulateAndGet(n, xorLong)

		/** Bit-shifts right the value of this variable by the specified number of bits, replacing shifted higher bits with the sign bit. */
		@inline def >>=(n :Long) :Unit = x.accumulateAndGet(n, rightShiftLong)

		/** Bit-shifts right the value of this variable by the specified number of bits, replacing shifted higher bits with zeros. */
		@inline def >>>=(n :Long) :Unit = x.accumulateAndGet(n, rightLogicShiftLong)

		/** Bit-shifts left the value of this variable by the specified number of bits, replacing shifted lower bits with zeros. */
		@inline def <<=(n :Long) :Unit = x.accumulateAndGet(n, leftShiftLong)

		/** Assigns this variable its bitwise negation: `this := !this.value`. */
		@inline def flip() :Unit = x.updateAndGet(notLong)

	}







	abstract class AtomicAsInt[@specialized(Byte, Char, Short, Float) T](init :T) extends Atomic[T] {
		private[this] val x = new AtomicInteger(pack(init))

		@inline private[Atomic] final def atom :AtomicInteger = x

		protected[this] def pack(t :T) :Int
		protected[this] def unpack(current :Int) :T

		override def  get :T = unpack(x.get)
		override def value :T = unpack(x.get)
		override def value_=(newValue :T) :Unit= x.set(pack(newValue))
		override def :=(newValue :T) :Unit = x.lazySet(pack(newValue))
		override def ?=(newValue :T) :T = unpack(x.getAndSet(pack(newValue)))

		override def testAndSet(expect :T, assign :T) :Boolean = x.compareAndSet(pack(expect), pack(assign))

	}



	final class AtomicByte(init :Byte) extends AtomicAsInt[Byte](init) {
		override protected def pack(t :Byte) :Int = t.toInt
		override protected def unpack(current :Int) :Byte = current.toByte
	}

	final class AtomicChar(init :Char) extends AtomicAsInt[Char](init) {
		override protected def pack(value :Char) :Int = value.toInt
		override protected def unpack(current :Int) :Char = current.toChar
	}

	final class AtomicShort(init :Short) extends AtomicAsInt[Short](init) {
		override protected def pack(value :Short) :Int = value.toInt
		override protected def unpack(current :Int) :Short = current.toShort
	}



	import j.Float.{intBitsToFloat => intToFloat}

	@inline private[this] final def floatToInt(value :Float) :Int =
		if (value == 0F) ZeroFloat //collate -0 and 0
		else if (value != value) NaNFloat //collate NaN
		else j.Float.floatToRawIntBits(value)

	private[this] final val ZeroFloat = j.Float.floatToIntBits(0F)
	private[this] final val NaNFloat = j.Float.floatToIntBits(j.Float.NaN)
	private[this] final val addFloat :IntBinaryOperator = (x, y) => floatToInt(intToFloat(x) + intToFloat(y))
	private[this] final val multiplyFloat :IntBinaryOperator = (x, y) => floatToInt(intToFloat(x) * intToFloat(y))
	private[this] final val divideFloat :IntBinaryOperator = (x, y) => floatToInt(intToFloat(x) / intToFloat(y))
	private[this] final val minusFloat :IntUnaryOperator = x => floatToInt(-intToFloat(x))

	final class AtomicFloat(init :Float) extends AtomicAsInt[Float](init) {
		override protected def pack(value :Float) :Int = floatToInt(value)

		override protected def unpack(current :Int) :Float = intToFloat(current)

		private[vars] override def float_+=(other :Float)(implicit ev :TypeEquiv[Float, Float]) :Float =
			intToFloat(atom.accumulateAndGet(floatToInt(other), addFloat))

		private[vars] override def float_*=(other :Float)(implicit ev :TypeEquiv[Float, Float]) :Float =
			intToFloat(atom.accumulateAndGet(floatToInt(other), multiplyFloat))

		private[vars] override def float_/=(other :Float)(implicit ev :TypeEquiv[Float, Float]) :Float =
			intToFloat(atom.accumulateAndGet(floatToInt(other), divideFloat))




		/** Increases the value of this variable by the specified number. */
		@inline def +=(n :Float) :Unit = atom.accumulateAndGet(floatToInt(n), addFloat)

		/** Decreases the value of this variable by the specified number. */
		@inline def -=(n :Float) :Unit = atom.accumulateAndGet(floatToInt(-n), addFloat)

		/** Multiplies the value of this variable by the specified number. */
		@inline def *=(n :Float) :Unit = atom.accumulateAndGet(floatToInt(n), multiplyFloat)

		/** Divides the value of this variable by the specified number. */
		@inline def /=(n :Float) :Unit = atom.accumulateAndGet(floatToInt(n), divideFloat)




		/** Increases the value of this variable by the specified number, returning the updated value. */
		@inline def inc(n :Float) :Float = intToFloat(atom.accumulateAndGet(floatToInt(n), addFloat))

		/** Decreases the value of this variable by the specified number, returning the updated value. */
		@inline def dec(n :Float) :Float = intToFloat(atom.accumulateAndGet(floatToInt(-n), addFloat))

		/** Multiplies the value of this variable by the specified number, returning the updated value. */
		@inline def mult(n :Float) :Float = intToFloat(atom.accumulateAndGet(floatToInt(n), multiplyFloat))

		/** Divides the value of this variable by the specified number, returning the updated value. */
		@inline def div(n :Float) :Float = intToFloat(atom.accumulateAndGet(floatToInt(n), divideFloat))

		/** Sets this variable to minus its value, returning the updated (opposite) value. */
		@inline def neg() :Float = intToFloat(atom.updateAndGet(minusFloat))

	}









	import j.Double.{longBitsToDouble => longToDouble}
	@inline private[this] final def doubleToLong(value :Double) :Long =
		if (value == 0F) ZeroDouble //collate -0 and 0
		else if (value != value) NaNDouble //collate NaN
		else j.Double.doubleToRawLongBits(value)

	private[this] final val ZeroDouble = j.Double.doubleToLongBits(0F)
	private[this] final val NaNDouble = j.Double.doubleToLongBits(j.Double.NaN)
	private[this] final val addDouble :LongBinaryOperator = (x, y) => doubleToLong(longToDouble(x) + longToDouble(y))
	private[this] final val multiplyDouble :LongBinaryOperator = (x, y) => doubleToLong(longToDouble(x) * longToDouble(y))
	private[this] final val divideDouble :LongBinaryOperator = (x, y) => doubleToLong(longToDouble(x) / longToDouble(y))
	private[this] final val minusDouble :LongUnaryOperator = x => doubleToLong(-longToDouble(x))


	final class AtomicDouble(init :Double) extends Atomic[Double] {
		private[this] val x = new atomic.AtomicLong(doubleToLong(init))

		override def get :Double = longToDouble(x.get)
		override def value :Double = longToDouble(x.get)
		override def value_=(newValue :Double) :Unit= x.set(doubleToLong(newValue))
		override def :=(newValue :Double) :Unit = x.lazySet(doubleToLong(newValue))
		override def ?=(newValue :Double) :Double = longToDouble(x.getAndSet(doubleToLong(newValue)))


		override def testAndSet(expect :Double, assign :Double) :Boolean = 
			x.compareAndSet(doubleToLong(expect), doubleToLong(assign))


		override def apply(f :Double => Double) :Double = {
			var current = x.get; var mod = f(longToDouble(current))
			while(!x.compareAndSet(current, doubleToLong(mod))) {
				current = x.get; mod = f(longToDouble(current))
			}
			mod
		}

		override def /:(acc :Double)(foldLeft :(Double, Double) => Double) :Double = {
			var current = x.get; var mod = foldLeft(acc, longToDouble(current))
			while(!x.compareAndSet(current, doubleToLong(mod))) {
				current = x.get; mod = foldLeft(acc, longToDouble(current))
			}
			mod
		}

		override def :\(acc :Double)(foldRight :(Double, Double) => Double) :Double = {
			var current = x.get; var mod = foldRight(longToDouble(current), acc)
			while (!x.compareAndSet(current, doubleToLong(mod))) {
				current = x.get; mod = foldRight(longToDouble(current), acc)
			}
			mod
		}


		private[vars] override def double_+=(other :Double)(implicit ev :TypeEquiv[Double, Double]) :Double = this inc other
		private[vars] override def double_*=(other :Double)(implicit ev :TypeEquiv[Double, Double]) :Double = this mult other
		private[vars] override def double_/=(other :Double)(implicit ev :TypeEquiv[Double, Double]) :Double = this div other

		
		
		/** Increases the value of this variable by the specified number. */
		@inline def +=(n :Double) :Unit = x.accumulateAndGet(doubleToLong(n), addDouble)

		/** Decreases the value of this variable by the specified number. */
		@inline def -=(n :Double) :Unit = x.accumulateAndGet(doubleToLong(-n), addDouble)

		/** Multiplies the value of this variable by the specified number. */
		@inline def *=(n :Double) :Unit = x.accumulateAndGet(doubleToLong(n), multiplyDouble)

		/** Divides the value of this variable by the specified number. */
		@inline def /=(n :Double) :Unit = x.accumulateAndGet(doubleToLong(n), divideDouble)



		/** Increases the value of this variable by the specified number, returning the updated value. */
		@inline def inc(n :Double) :Double = longToDouble(x.accumulateAndGet(doubleToLong(n), addDouble))

		/** Decreases the value of this variable by the specified number, returning the updated value. */
		@inline def dec(n :Double) :Double = longToDouble(x.accumulateAndGet(doubleToLong(-n), addDouble))

		/** Multiplies the value of this variable by the specified number, returning the updated value. */
		@inline def mult(n :Double) :Double = longToDouble(x.accumulateAndGet(doubleToLong(n), multiplyDouble))

		/** Divides the value of this variable by the specified number, returning the updated value. */
		@inline def div(n :Double) :Double = longToDouble(x.accumulateAndGet(doubleToLong(n), divideDouble))

		/** Sets this variable to minus its value, returning the updated (opposite) value. */
		@inline def neg() :Double = longToDouble(x.updateAndGet(minusDouble))
	}
	

}
