package net.turambar.slang


/** A box/reference for a mutable value. Allows for in/out parameters to functions. */
final class Var[@specialized(Var.SpecializedTypes) T](private[this] var value :T) extends Serializable {
	/** Current value of this variable. */
	@inline def get :T = value

	/** Assigns a new value to this variable. */
	@inline def :=(newValue :T) :Unit = value = newValue


	override def equals(that :Any) :Boolean = that match {
		case v :Var[_] => (v eq this) || v.get == value
		case _ => false
	}

	override def hashCode :Int = value.hashCode

	override def toString :String = value.toString
}



object Var {
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

	}
	

	implicit class BooleanVarOps(private val x :Var[Boolean]) extends AnyVal {
		@inline def &=(other :Boolean) :Unit = x := x.get && other
		@inline def &&=(other: =>Boolean) :Unit = x := x.get && other
		@inline def |=(other :Boolean) :Unit = x := x.get || other
		@inline def ||=(other: =>Boolean) :Unit = x := x.get || other
		@inline def ^=(other :Boolean) :Unit = x := x.get ^ other
//		@inline def not() :Unit = x := !x.get
	}
	
	
	implicit class IntVarOps(private val x :Var[Int]) extends AnyVal {
		@inline def +=(n :Int) :Unit = x := x.get + n
		@inline def -=(n :Int) :Unit = x := x.get - n
		@inline def *=(n :Int) :Unit = x := x.get * n
		@inline def /=(n :Int) :Unit = x := x.get / n
		@inline def %=(n :Int) :Unit = x := x.get % n

		@inline def ++ :Unit = x := x.get + 1
		@inline def -- :Unit = x := x.get - 1

//		@inline def neg() :Unit = x := - x.get

		@inline def |=(n :Int) :Unit = x := x.get | n
		@inline def &=(n :Int) :Unit = x := x.get & n
		@inline def ^=(n :Int) :Unit = x := x.get ^ n

		@inline def >>=(n :Int) :Unit = x := x.get >> n
		@inline def >>>=(n :Int) :Unit = x := x.get >>> n
		@inline def <<=(n :Int) :Unit = x := x.get << n
		@inline def flip() :Unit = x := ~x.get
	}


	implicit class LongVarOps(private val x :Var[Long]) extends AnyVal {
		@inline def +=(n :Long) :Unit = x := x.get + n
		@inline def -=(n :Long) :Unit = x := x.get - n
		@inline def *=(n :Long) :Unit = x := x.get * n
		@inline def /=(n :Long) :Unit = x := x.get / n
		@inline def %=(n :Long) :Unit = x := x.get % n

//		@inline def neg() :Unit = x := -x.get

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


	implicit class FloatVarOps(private val x :Var[Float]) extends AnyVal {
		@inline def +=(n :Float) :Unit = x := x.get + n
		@inline def -=(n :Float) :Unit = x := x.get - n
		@inline def *=(n :Float) :Unit = x := x.get * n
		@inline def /=(n :Float) :Unit = x := x.get / n
	}


	implicit class DoubleVarOps(private val x :Var[Double]) extends AnyVal {
		@inline def +=(n :Double) :Unit = x := x.get + n
		@inline def -=(n :Double) :Unit = x := x.get - n
		@inline def *=(n :Double) :Unit = x := x.get * n
		@inline def /=(n :Double) :Unit = x := x.get / n
	}
	
	
	

}

