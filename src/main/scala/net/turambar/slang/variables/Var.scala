package net.turambar.slang.variables


/** A box/reference for a mutable value. Allows for in/out paramters to functions. */
final class Var[@specialized(Var.SpecializedTypes) T](private[this] var value :T) extends Serializable {
	/** Current value of this variable. */
	@inline final def get :T = value

	/** Assigns a new value to this variable. */
	@inline final def :=(newValue :T) :Unit = value = newValue

	@inline final def +=(other :T)(implicit num :Numeric[T]) :Unit =
		value = num.plus(value, other)

	@inline final def -=(other :T)(implicit num :Numeric[T]) :Unit =
		value = num.minus(value, other)

	@inline final def ++(implicit num :Integral[T]) :Unit =
		value = num.plus(value, num.one)

	@inline final def --(implicit num :Integral[T]) :Unit =
		value = num.minus(value, num.one)

	@inline final def *=(other :T)(implicit num :Numeric[T]) :Unit =
		value = num.times(value, other)

	@inline final def /=(other :T)(implicit num :Fractional[T]) :Unit =
		value = num.div(value, other)

	@inline final def /=(other :T)(implicit num :Integral[T]) :Unit =
		value = num.quot(value, other)

	@inline final def %=(other :T)(implicit num :Integral[T]) :Unit =
		value = num.rem(value, other)

	@inline final def &&=(other :Boolean)(implicit bool :T=:=Boolean) :Unit =
		value = (bool(value) && other).asInstanceOf[T]

	@inline final def ||=(other :Boolean)(implicit bool :T=:=Boolean) :Unit =
		value = (bool(value) || other).asInstanceOf[T]

	/** Assuming this variable is of `Boolean` type, negate the current value.
	  * Equivalent to `this := !this.get`
	  */
	@inline final def =!(implicit bool :T=:=Boolean) :Unit =
		value = (!(value.asInstanceOf[Boolean])).asInstanceOf[T]

	override def equals(that :Any) = that match {
		case v :Var[_] => (v eq this) || v.get == value
		case _ => false
	}

	override def hashCode = value.hashCode

	override def toString = value.toString
}



object Var {
	final val SpecializedTypes = new Specializable.Group(Byte, Short, Char, Int, Long, Float, Double, Boolean)

	@inline final implicit def unboxVar[@specialized(SpecializedTypes) T](variable :Var[T]) :T = variable.get
	@inline final implicit def fromIntVar(variable :IntVar) :Var[Int] = new Var[Int](variable.get)
	@inline final implicit def toIntVar(variable :Var[Int]) :IntVar = new IntVar(variable.get)
	@inline final implicit def fromLongVar(variable :LongVar) :Var[Long] = new Var[Long](variable.get)
	@inline final implicit def toLongVar(variable :Var[Long]) :LongVar = new LongVar(variable.get)


	@inline def apply[@specialized(SpecializedTypes) T](value :T) :Var[T] = new Var[T](value)

	object implicits {
		@inline implicit def boxVar[@specialized(SpecializedTypes) T](value :T) :Var[T] = new Var[T](value)
	}


}


/** An integer variable which can be used as a counter and shared with blocks outside the scope of its declaration.
  * In particular, it forms a useful in/out method parameter.
  * @param value current value of this variable
  */
final class IntVar(private[this] var value :Int) extends Serializable {
	/** Current value of this variable. */
	@inline final def get :Int = value
	/** Assign a new value to this variable. */
	@inline final def :=(newValue :Int) :Unit = value = newValue

	@inline final def +=(other :Int) :Unit = value += other

	@inline final def -=(other :Int) :Unit = value -= other

	@inline final def ++ :Unit = value += 1

	@inline final def -- :Unit = value -= 1

	@inline final def *=(other :Int) :Unit = value *= other

	@inline final def /=(other :Int) :Unit = value /= other

	@inline final def %=(other :Int) :Unit = value %= other


	@inline final def >>>=(shift :Int) :Unit = value >>>= shift

	@inline final def >>=(shift :Int) :Unit = value >>= shift

	@inline final def <<=(shift :Int) :Unit = value <<= shift

	@inline final def &=(other :Int) :Unit = value &= other

	@inline final def |=(other :Int) :Unit = value |= other

	@inline final def ^=(other :Int) :Unit = value ^= other

	/** Change the value of this variable to its bitwise negative. Equivalent to `this := ~this.get`. */
	@inline final def =~ :Unit = value = ~value

	@inline final def toLongVar :LongVar = new LongVar(value)

	override def equals(that :Any) = that match {
		case v :IntVar => v.get == value
		case _ => false
	}

	override def hashCode = value

	override def toString = value.toString

}

object IntVar {
	@inline final def apply(i :Int) :IntVar = new IntVar(i)

	@inline final implicit def varToInt(i :IntVar) :Int = i.get

	object implicits {
		@inline final implicit def boxVar(i :Int) :IntVar = new IntVar(i)
	}
}




/** A variable of type `Long` which can be used as a counter and shared with blocks outside the scope of its declaration.
  * In particular, it forms a useful in/out method parameter.
  * @param value current value of this variable
  */
final class LongVar(private[this] var value :Long) extends Serializable {
	/** Current value of this variable. */
	@inline final def get :Long = value

	/** Assign a new value to this variable. */
	@inline final def :=(newValue :Long) :Unit = value = newValue

	@inline final def +=(other :Long) :Unit =
		value += other

	@inline final def -=(other :Long) :Unit =
		value -= other

	@inline final def ++ :Unit =
		value += 1

	@inline final def -- :Unit =
		value -= 1

	@inline final def *=(other :Long) :Unit =
		value *= other

	@inline final def /=(other :Long) :Unit =
		value /= other

	@inline final def %=(other :Long) :Unit =
		value %= other

	@inline final def >>>=(shift :Long) :Unit =
		value >>>= shift

	@inline final def >>=(shift :Long) :Unit =
		value >>= shift

	@inline final def <<=(shift :Long) :Unit =
		value <<= shift

	@inline final def &=(other :Long) :Unit =
		value &= other

	@inline final def |=(other :Long) :Unit =
		value |= other

	@inline final def ^=(other :Long) :Unit =
		value ^= other

	/** Change the value of this variable to its bitwise negative. Equivalent to `this := ~this.get`. */
	@inline final def =~ :Unit =
		value = ~value

	override def equals(that :Any) = that match {
		case v :LongVar => v.get == value
		case _ => false
	}

	override def hashCode = value.toInt ^ (value >>> 32).toInt

	override def toString = value.toString

}

object LongVar {
	@inline final def apply(i :Long) :LongVar = new LongVar(i)

	@inline final implicit def varToLong(i :LongVar) :Long = i.get

	object implicits {
		@inline final implicit def boxVar(i :Long) :LongVar = new LongVar(i)
	}
}

