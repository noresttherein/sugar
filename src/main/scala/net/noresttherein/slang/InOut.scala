package net.noresttherein.slang

import net.noresttherein.slang.Var.SpecializedTypes


/** An interface for mutable values being reference wrappers over `var` fields. Designed for in/out parameters to functions.
  * Implemented by several boxing classes which may provide additional features such as synchronization.
  * Implicit conversions exist providing arithmetic suitable to the type of the boxed value, so for example
  * you can write `param += 1` for `param :InOut[Int]`.
  * @tparam T type of this variable
  * @see [[net.noresttherein.slang.Var]]
  * @see [[net.noresttherein.slang.Volatile]]
  * @see [[net.noresttherein.slang.SyncVar]]
  */
trait InOut[@specialized(SpecializedTypes) T] {

	/** Current value of this variable. */
	def get :T

	/** Current value of this variable. */
	def value :T = get


	/** Assign a new value to this variable. */
	def value_=(value :T) :Unit

	/** Assigns a new value to this variable. */
	def :=(newValue :T) :Unit = value = newValue


	/** Assigns a new value returning the previous value. */
	def ?=(newValue :T) :T = { val res = value; value = newValue; res }


	override def equals(that :Any) :Boolean = that match {
		case v :InOut[_] => (v eq this) || (v canEqual this) && v.get == get
		case _ => false
	}

	def canEqual(that :InOut[_]) :Boolean = true

	override def hashCode :Int = get.hashCode

	override def toString :String = get.toString
}





/** Factory of boxed in/out method parameters. */
object InOut {
	/** Unbox the value hold by an `InOut` wrapper. */
	@inline final implicit def unboxInOut[@specialized(SpecializedTypes) T](variable :InOut[T]) :T = variable.get

	/** Create a wrapper over a '''`var`''' of type `T` which can be passed as an in/out method parameter. */
	@inline def apply[@specialized(SpecializedTypes) T](value :T) :InOut[T] = new Var[T](value)


	final val SpecializedTypes = new Specializable.Group(Byte, Short, Char, Int, Long, Float, Double, Boolean)


	/** Extra implicits which might be helpful but can also lead to tricky bugs or cause conflicts. */
	object implicits {
		/** Implicitly creates a `InOut` instance with a given value. This implicit is optional as the main use of `InOut[T]`
		  * is to be used as in/out method parameters. In that scenario, using a value identifier instead of a `InOut[T]`
		  * makes no sense and would likely be an error.
		  */
		@inline implicit def boxInOutParam[@specialized(SpecializedTypes) T](value :T) :InOut[T] = new Var[T](value)

		/** Implicit extension of values of any type allowing chained assignments to compatible variables in the form of
		  * `x1 =: x2 =: x3 =: value` or `(x1, x2, x3) =: value`. This conversion needs to be manually imported
		  * as the wide scope of accepted input values can easily lead to conflicts with different libraries.
		  * @param value value to assign to a chain of variables.
		  */
		@inline implicit def InOutMultiAssignment[@specialized(SpecializedTypes) T](value :T) :InOutMultiAssignment[T, InOut[T]] =
			new InOutMultiAssignment[T, InOut[T]](value)

		/** Implicit extension of values of any type allowing chained assignments to compatible variables in the form of
		  * `x1 =: x2 =: x3 =: value` or `(x1, x2, x3) =: value`. This conversion needs to be manually imported
		  * as the wide scope of accepted input values can easily lead to conflicts with different libraries.
		  * @param value value to assign to a chain of variables.
		  */
		class InOutMultiAssignment[T, V <: InOut[T]](private val value :T) extends AnyVal {

			/** Assign the right-hand value (this) to the left-hand variable, returning this value.
			  * Allows C-like chained assignments: `x1 =: x2 =: x3 =: 0`.
			  * @param v variable which should be assigned the new value
			  * @return assigned value
			  */
			@inline def =:(v :V) :T = { v := value; value }

			/** Assigns this (right-hand) value to multiple variables given on the left-hand side of the operator.
			  * `(x1, x2, x3) =: 0`. This is an arguably more readable altennative to chaining individual assignments,
			  * although slightly less effective.
			  */
			@inline def =:(v1 :V, v2 :V, vs :V*) :T = {
				v1 := value; v2 := value
				vs match {
					case list :List[T] =>
						var l :List[T] = list
						while (l.nonEmpty) {
							l.head := value; l = l.tail
						}
					case _ => vs foreach { _ := value }
				}
				value
			}
		}

	}


	implicit class InOutBooleanOps(private val x :InOut[Boolean]) extends AnyVal {
		@inline def &=(other :Boolean) :Unit = x := x.get && other
		@inline def &&=(other: =>Boolean) :Unit = x := x.get && other
		@inline def |=(other :Boolean) :Unit = x := x.get || other
		@inline def ||=(other: =>Boolean) :Unit = x := x.get || other
		@inline def ^=(other :Boolean) :Unit = x := x.get ^ other

		/** Negates this boolean variable, assigning it the opposite of the current value. */
		@inline def neg() :Unit = x := !x.get

	}


	implicit class InOutIntArithmetic(private val x :InOut[Int]) extends AnyVal {
		@inline def +=(n :Int) :Unit = x := x.get + n
		@inline def -=(n :Int) :Unit = x := x.get - n
		@inline def *=(n :Int) :Unit = x := x.get * n
		@inline def /=(n :Int) :Unit = x := x.get / n
		@inline def %=(n :Int) :Unit = x := x.get % n

		/** Increments this variable by `1`, C-style. */
		@inline def ++ :Unit = x := x.get + 1

		/** Decrements this variable by `1`, C-style. */
		@inline def -- :Unit = x := x.get - 1

		@inline def |=(n :Int) :Unit = x := x.get | n
		@inline def &=(n :Int) :Unit = x := x.get & n
		@inline def ^=(n :Int) :Unit = x := x.get ^ n

		@inline def >>=(n :Int) :Unit = x := x.get >> n
		@inline def >>>=(n :Int) :Unit = x := x.get >>> n
		@inline def <<=(n :Int) :Unit = x := x.get << n

		/** Assigns this variable its bitwise negation: `this := !this.value`. */
		@inline def flip() :Unit = x := ~x.get
	}


	implicit class InOutLongArithmetic(private val x :InOut[Long]) extends AnyVal {
		@inline def +=(n :Long) :Unit = x := x.get + n
		@inline def -=(n :Long) :Unit = x := x.get - n
		@inline def *=(n :Long) :Unit = x := x.get * n
		@inline def /=(n :Long) :Unit = x := x.get / n
		@inline def %=(n :Long) :Unit = x := x.get % n

		/** Increments this variable by `1`, C-style. */
		@inline def ++ :Unit = x := x.get + 1L

		/** Decrements this variable by `1`, C-style. */
		@inline def -- :Unit = x := x.get - 1L

		@inline def |=(n :Long) :Unit = x := x.get | n
		@inline def &=(n :Long) :Unit = x := x.get & n
		@inline def ^=(n :Long) :Unit = x := x.get ^ n

		@inline def >>=(n :Int) :Unit = x := x.get >> n
		@inline def >>>=(n :Int) :Unit = x := x.get >>> n
		@inline def <<=(n :Int) :Unit = x := x.get << n

		/** Assigns this variable its bitwise negation: `this := !this.value`. */
		@inline def flip() :Unit = x := ~x.get
	}


	implicit class InOutFloatArithmetic(private val x :InOut[Float]) extends AnyVal {
		@inline def +=(n :Float) :Unit = x := x.get + n
		@inline def -=(n :Float) :Unit = x := x.get - n
		@inline def *=(n :Float) :Unit = x := x.get * n
		@inline def /=(n :Float) :Unit = x := x.get / n
	}


	implicit class InOutDoubleArithmetic(private val x :InOut[Double]) extends AnyVal {
		@inline def +=(n :Double) :Unit = x := x.get + n
		@inline def -=(n :Double) :Unit = x := x.get - n
		@inline def *=(n :Double) :Unit = x := x.get * n
		@inline def /=(n :Double) :Unit = x := x.get / n
	}

}
