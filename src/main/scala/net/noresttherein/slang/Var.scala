package net.noresttherein.slang

/** An simple `@specialized` boxed `var`. Allows for in/out parameters to functions.
  * Implicit conversions exist providing arithmetic suitable to the type of the boxed value, so for example
  * you can write `param += 1` for `param :Var[Int]`.
  * @tparam T type of this variable
  * @see [[Var]]
  */
final class Var[@specialized(Var.SpecializedTypes) T](private[this] var x :T) extends Serializable {
	/** Current value of this variable. */
	@inline def get :T = x

	/** Current value of this variable. */
	@inline def value :T = x

	/** Assigns a new value to this variable. */
	@inline def value_=(newValue :T) :Unit = x = newValue

	/** Assigns a new value to this variable. */
	@inline def :=(newValue :T) :Unit = x = newValue

	/** Assigns a new value returning the previous value at the same time. */
	@inline def ?=(newValue :T) :T = { val res = x; x = newValue; res }


	override def equals(that :Any) :Boolean = that match {
		case v :Var[_] => (v eq this) || v.get == value
		case _ => false
	}

	override def hashCode :Int = value.hashCode

	override def toString :String = value.toString
}



object Var {
	/** Types for which [[net.noresttherein.slang.Var]] is specialized. */
	final val SpecializedTypes = new Specializable.Group(Byte, Short, Char, Int, Long, Float, Double, Boolean)

	/** Unbox the value hold by a `Var`. */
	@inline final implicit def unboxVar[@specialized(SpecializedTypes) T](variable :Var[T]) :T = variable.get

	/** Create a wrapper over a '''`var`''' of type `T` which can be passed as in/out method parameter. */
	@inline def apply[@specialized(SpecializedTypes) T](value :T) :Var[T] = new Var[T](value)



	/** Extra implicits which might be helpful but can also lead to tricky bugs. */
	object implicits {
		/** Implicitly creates a `Var` with a given value. This implicit is optional as the main use of `Var[T]`
		  * is to be used as in/out method parameters. In that scenario, using a value identifier instead of a `Var[T]`
		  * makes no sense and would likely be an error.
		  */
		@inline implicit def boxVar[@specialized(SpecializedTypes) T](value :T) :Var[T] = new Var[T](value)


		/** Implicit extension of values of any type allowing chained assignments to compatible variables in the form of
		  * `x1 =: x2 =: x3 =: newValue` or `(x1, x2, x3) =: newValue`. This conversion needs to be manually imported
		  * as the wide scope of accepted input values can easily lead to conflicts with different libraries.
		  * @param value value to assign to a chain of variables.
		  */
		implicit class VarMultiAssignment[T](private val value :T) extends AnyVal {

			/** Assign the right-hand value (this) to the left-hand variable, returning this value.
			  * Allows for C-like chained assignments: `x1 =: x2 =: x3 =: 0`.
			  * @param v variable which should be assigned the new value
			  * @return assigned value
			  */
			@inline def =:(v :Var[T]) :T = { v := value; value }

			/** Assigns this (right-hand) value to multiple variables given on the left-hand side of the operator.
			  * `(x1, x2, x3) =: 0`. This is an arguably more readable alternative to chaining individual assignments,
			  * although slightly less efficient.
			  */
			@inline def =:(v1 :Var[T], v2 :Var[T], vs :Var[T]*) :T = {
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


	/** Implicit conversion of `Var[Boolean]` values providing logical operators. */
	implicit class BooleanVarLogic(private val x :Var[Boolean]) extends AnyVal {
		@inline def &=(other :Boolean) :Unit = x := x.get & other
		@inline def &&=(other: =>Boolean) :Unit = x := x.get && other
		@inline def |=(other :Boolean) :Unit = x := x.get | other
		@inline def ||=(other: =>Boolean) :Unit = x := x.get || other
		@inline def ^=(other :Boolean) :Unit = x := x.get ^ other

		@inline def neg() :Unit = x := !x.get
	}


	/** Implicit conversion of `Var[Int]` values providing arithmetic modifications. */
	implicit class IntVarArithmetic(private val x :Var[Int]) extends AnyVal {
		@inline def +=(n :Int) :Unit = x := x.get + n
		@inline def -=(n :Int) :Unit = x := x.get - n
		@inline def *=(n :Int) :Unit = x := x.get * n
		@inline def /=(n :Int) :Unit = x := x.get / n
		@inline def %=(n :Int) :Unit = x := x.get % n

		@inline def ++ :Unit = x := x.get + 1
		@inline def -- :Unit = x := x.get - 1

		@inline def |=(n :Int) :Unit = x := x.get | n
		@inline def &=(n :Int) :Unit = x := x.get & n
		@inline def ^=(n :Int) :Unit = x := x.get ^ n

		@inline def >>=(n :Int) :Unit = x := x.get >> n
		@inline def >>>=(n :Int) :Unit = x := x.get >>> n
		@inline def <<=(n :Int) :Unit = x := x.get << n

		@inline def flip() :Unit = x := ~x.get
	}


	/** Implicit conversion of `Var[Long]` values providing arithmetic modifications. */
	implicit class LongVarArithmetic(private val x :Var[Long]) extends AnyVal {
		@inline def +=(n :Long) :Unit = x := x.get + n
		@inline def -=(n :Long) :Unit = x := x.get - n
		@inline def *=(n :Long) :Unit = x := x.get * n
		@inline def /=(n :Long) :Unit = x := x.get / n
		@inline def %=(n :Long) :Unit = x := x.get % n

		@inline def ++ :Unit = x := x.get + 1L
		@inline def -- :Unit = x := x.get - 1L

		@inline def |=(n :Long) :Unit = x := x.get | n
		@inline def &=(n :Long) :Unit = x := x.get & n
		@inline def ^=(n :Long) :Unit = x := x.get ^ n

		@inline def >>=(n :Int) :Unit = x := x.get >> n
		@inline def >>>=(n :Int) :Unit = x := x.get >>> n
		@inline def <<=(n :Int) :Unit = x := x.get << n

		@inline def flip() :Unit = x := ~x.get
	}


	/** Implicit conversion of `Var[Float]` values providing arithmetic modifications. */
	implicit class FloatVarArithmetic(private val x :Var[Float]) extends AnyVal {
		@inline def +=(n :Float) :Unit = x := x.get + n
		@inline def -=(n :Float) :Unit = x := x.get - n
		@inline def *=(n :Float) :Unit = x := x.get * n
		@inline def /=(n :Float) :Unit = x := x.get / n
	}


	/** Implicit conversion of `Var[Double]` values providing arithmetic modifications. */
	implicit class DoubleVarArithmetic(private val x :Var[Double]) extends AnyVal {
		@inline def +=(n :Double) :Unit = x := x.get + n
		@inline def -=(n :Double) :Unit = x := x.get - n
		@inline def *=(n :Double) :Unit = x := x.get * n
		@inline def /=(n :Double) :Unit = x := x.get / n
	}
	
	
	

}

