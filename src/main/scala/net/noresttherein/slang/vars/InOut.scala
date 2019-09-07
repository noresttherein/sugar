package net.noresttherein.slang.vars

import net.noresttherein.slang.vars.InOut.{InOutNumeric, InOutOrdering, TestAndSet, TypeEquiv}
import net.noresttherein.slang.vars.Var.SpecializedTypes



/** An interface for mutable values being reference wrappers over `var` fields. Designed for in/out parameters to functions.
  * Implemented by several boxing classes which may provide additional features such as synchronization.
  * Implicit conversions exist providing arithmetic suitable to the type of the boxed value, so for example
  * you can write `param += 1` for `param :InOut[Int]`.
  * @tparam T type of this variable
  * @see [[Var]]
  * @see [[Volatile]]
  * @see [[SyncVar]]
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



	/** Assigns a new value to this variable providing the current value is equal to the expected value.
	  * The default implementation does it the direct way without any guarantees about multi-threaded semantics.
	  * This method is of real practical use only in concurrent `InOut` implementations such as [[SyncVar]] and [[Atomic]]
	  * (and to a very limited extent in [[Volatile]]).
	  * @param expect value to compare with current value
	  * @param assign new value for this variable
	  * @return `true` if previous value equaled `expect` and the variable has been set to `assign`.
	  */
	def testAndSet(expect :T, assign :T) :Boolean =
		(value == expect) && { value = assign; true }


	/** A ''test-and-set'' operation divided syntactically into two binary operators.
	  * `x :? expect := value` is equivalent to `x.testAndSet(expect, value)`.
	  * @param expect value to compare with current value of this variable
	  * @return an intermediate object which will perform the comparison and assign the value given to its
	  *         [[InOut.TestAndSet#:=]] method.
	  * @see [[InOut.testAndSet testAndSet()]]
	  */
	@inline final def :?(expect :T) :TestAndSet[T] = new TestAndSet(this, expect)


	/** Updates the value of this variable with the given function. Default implementation is equivalent to
	  * `val res = f(this.get); this.value = res; res` and has no benefit over direct application in client code.
	  * This method comes to use with concurrent `InOut` implementations such as [[SyncVar]] and [[Atomic]] -
	  * the semantics of simple default [[Var]] offers no guarantees in multi-threaded environments.
	  * @param f function to apply to the value of this variable. Should have no side effects as it may be invoked
	  *          several times.
	  * @return result of applying `f` to the current value.
	  */
	def apply(f :T => T) :T = { val res = f(get); value = res; res }


	/** Combines the value of this variable with another value of the same type, assigning the result of application
	  * back to this variable before returning it. It uses this variable as an accumulator, updated iteratively with
	  * new values in a way similar to an in place ''foldLeft'' operation on a singleton collection.
	  * Default implementation naively performs this directly without any guarantees about multi-threaded semantics
	  * and is equivalent to `val res = foldLeft(acc, value); value = res; res`. Scala specific fold notation is
	  * chosen here to remind through associativity that this variable becomes the second (right) parameter of the folding
	  * function, with the argument on the left side of the operator being the first. This method comes to use with
	  * concurrent `InOut` implementations such as [[SyncVar]] or [[Atomic]].
	  * @param acc accumulator value to pass as the first argument to the `foldLeft` function, together with the current
	  *            value of this variable.
	  * @param foldLeft a function applied to the accumulator and this variable which result should be set to this variable.
	  * @return result of applying `foldLeft` to the accumulator and this variable.
	  */
	def /:(acc :T)(foldLeft :(T, T) => T) :T = { val res = foldLeft(acc, value); value = res; res }



	/** Combines the value of this variable with another value of the same type, assigning the result of application
	  * back to this variable before returning it. It uses this variable as an accumulator, updated iteratively with
	  * new values in a way similar to an in place ''foldRight'' operation on a singleton collection.
	  * Default implementation naively performs this directly without any guarantees about multi-threaded semantics
	  * and is equivalent to `val res = foldRight(value, acc); value = res; res`. Scala specific fold notation is
	  * chosen here to remind through associativity that this variable becomes the first (left) parameter of the folding
	  * function, with the argument on the right side of the operator being the second. This method comes to use with
	  * concurrent `InOut` implementations such as [[SyncVar]] or [[Atomic]].
	  * @param acc accumulator value to pass as the second argument to the `foldRight` function, together with the current
	  *            value of this variable.
	  * @param foldRight a function applied to the accumulator and this variable which result should be set to this variable.
	  * @return result of applying `foldLeft` to the accumulator and this variable.
	  */	
	def :\(acc :T)(foldRight :(T, T) => T) :T = { val res = foldRight(value, acc); value = res; res }





	/************************************** Boolean methods ***********************************************************/
	
	
	private[vars] def bool_&=(other :Boolean)(implicit ev :T TypeEquiv Boolean) :Unit = {
		val self = ev(this)
		self.value = self.value & other
	}

	private[vars] def bool_|=(other :Boolean)(implicit ev :T TypeEquiv Boolean) :Unit = {
		val self = ev(this)
		self.value = self.value | other
	}
	
	private[vars] def bool_&&=(other: => Boolean)(implicit ev :T TypeEquiv Boolean) :Unit = {
		val self = ev(this)
		self.value = self.value && other
	}

	private[vars] def bool_||=(other: =>Boolean)(implicit ev :T TypeEquiv Boolean) :Unit = {
		val self = ev(this)
		self.value = self.value || other
	}

	private[vars] def bool_^=(other :Boolean)(implicit ev :T TypeEquiv Boolean) :Unit = {
		val self = ev(this)
		self.value = self.value ^ other
	}

	private[vars] def bool_!=(implicit ev :T TypeEquiv Boolean) :Boolean = {
		val self = ev(this)
		val res = !self.value; self.value = res; res
	}

	
	
	
	/************************************** Int methods ***************************************************************/
	
	
	private[vars] def int_+=(other :Int)(implicit ev :T TypeEquiv Int) :Int = {
		val self = ev(this)
		val res = self.value + other; self.value = res; res
	}

	private[vars] def int_*=(other :Int)(implicit ev :T TypeEquiv Int) :Int = {
		val self = ev(this)
		val res = self.value * other; self.value = res; res
	}

	private[vars] def int_/=(other :Int)(implicit ev :T TypeEquiv Int) :Int = {
		val self = ev(this)
		val res = self.value / other; self.value = res; res
	}

	private[vars] def int_%=(other :Int)(implicit ev :T TypeEquiv Int) :Int = {
		val self = ev(this)
		val res = self.value % other; self.value = res; res
	}

	private[vars] def int_-(implicit ev :T TypeEquiv Int) :Int = {
		val self = ev(this)
		val res = -self.value; self.value = res; res
	}

	private[vars] def int_~(implicit ev :T TypeEquiv Int) :Int = {
		val self = ev(this)
		val res = ~self.value; self.value = res; res
	}
	
	private[vars] def int_|=(other :Int)(implicit ev :T TypeEquiv Int) :Int = {
		val self = ev(this)
		val res = self.value | other; self.value = res; res
	}

	private[vars] def int_&=(other :Int)(implicit ev :T TypeEquiv Int) :Int = {
		val self = ev(this)
		val res = self.value & other; self.value = res; res
	}

	private[vars] def int_^=(other :Int)(implicit ev :T TypeEquiv Int) :Int = {
		val self = ev(this)
		val res = self.value ^ other; self.value = res; res
	}

	private[vars] def int_>>=(other :Int)(implicit ev :T TypeEquiv Int) :Int = {
		val self = ev(this)
		val res = self.value >> other; self.value = res; res
	}

	private[vars] def int_>>>=(other :Int)(implicit ev :T TypeEquiv Int) :Int = {
		val self = ev(this)
		val res = self.value >>> other; self.value = res; res
	}

	private[vars] def int_<<=(other :Int)(implicit ev :T TypeEquiv Int) :Int = {
		val self = ev(this)
		val res = self.value << other; self.value = res; res
	}





	/************************************** Long methods **************************************************************/


	private[vars] def long_+=(other :Long)(implicit ev :T TypeEquiv Long) :Long = {
		val self = ev(this)
		val res = self.value + other; self.value = res; res
	}

	private[vars] def long_*=(other :Long)(implicit ev :T TypeEquiv Long) :Long = {
		val self = ev(this)
		val res = self.value * other; self.value = res; res
	}

	private[vars] def long_/=(other :Long)(implicit ev :T TypeEquiv Long) :Long = {
		val self = ev(this)
		val res = self.value / other; self.value = res; res
	}

	private[vars] def long_%=(other :Long)(implicit ev :T TypeEquiv Long) :Long = {
		val self = ev(this)
		val res = self.value % other; self.value = res; res
	}

	private[vars] def long_-(implicit ev :T TypeEquiv Long) :Long = {
		val self = ev(this)
		val res = -self.value; self.value = res; res
	}

	private[vars] def long_~(implicit ev :T TypeEquiv Long) :Long = {
		val self = ev(this)
		val res = ~self.value; self.value = res; res
	}

	private[vars] def long_|=(other :Long)(implicit ev :T TypeEquiv Long) :Long = {
		val self = ev(this)
		val res = self.value | other; self.value = res; res
	}

	private[vars] def long_&=(other :Long)(implicit ev :T TypeEquiv Long) :Long = {
		val self = ev(this)
		val res = self.value & other; self.value = res; res
	}

	private[vars] def long_^=(other :Long)(implicit ev :T TypeEquiv Long) :Long = {
		val self = ev(this)
		val res = self.value ^ other; self.value = res; res
	}

	private[vars] def long_>>=(n :Int)(implicit ev :T TypeEquiv Long) :Long = {
		val self = ev(this)
		val res = self.value >> n; self.value = res; res
	}

	private[vars] def long_>>>=(n :Int)(implicit ev :T TypeEquiv Long) :Long = {
		val self = ev(this)
		val res = self.value >>> n; self.value = res; res
	}

	private[vars] def long_<<=(n :Int)(implicit ev :T TypeEquiv Long) :Long = {
		val self = ev(this)
		val res = self.value << n; self.value = res; res
	}





	/************************************** Float methods *************************************************************/


	private[vars] def float_+=(other :Float)(implicit ev :T TypeEquiv Float) :Float = {
		val self = ev(this)
		val res = self.value + other; self.value = res; res
	}

	private[vars] def float_*=(other :Float)(implicit ev :T TypeEquiv Float) :Float = {
		val self = ev(this)
		val res = self.value * other; self.value = res; res
	}

	private[vars] def float_/=(other :Float)(implicit ev :T TypeEquiv Float) :Float = {
		val self = ev(this)
		val res = self.value / other; self.value = res; res
	}

	private[vars] def float_-(implicit ev :T TypeEquiv Float) :Float = {
		val self = ev(this)
		val res = -self.value; self.value = res; res
	}



	/************************************** Double methods ************************************************************/


	private[vars] def double_+=(other :Double)(implicit ev :T TypeEquiv Double) :Double = {
		val self = ev(this)
		val res = self.value + other; self.value = res; res
	}

	private[vars] def double_*=(other :Double)(implicit ev :T TypeEquiv Double) :Double = {
		val self = ev(this)
		val res = self.value * other; self.value = res; res
	}

	private[vars] def double_/=(other :Double)(implicit ev :T TypeEquiv Double) :Double = {
		val self = ev(this)
		val res = self.value / other; self.value = res; res
	}

	private[vars] def double_-(implicit ev :T TypeEquiv Double) :Double = {
		val self = ev(this)
		val res = -self.value; self.value = res; res
	}







	override def equals(that :Any) :Boolean = that match {
		case v :InOut[_] => (v eq this) || (v canEqual this) && v.get == get
		case _ => false
	}

	def canEqual(that :InOut[_]) :Boolean = true

	override def hashCode :Int = get.hashCode

	override def toString :String = get.toString
}



sealed class InOutOrderingImplicits private[vars]() {
	@inline implicit def InOutOrdering[T](implicit ordering :Ordering[T]) :Ordering[InOut[T]] =
		new InOutOrdering(ordering)
}


sealed class InOutNumericImplicits private[vars] () extends InOutOrderingImplicits {
	@inline implicit def InOutNumeric[T](implicit numeric :Numeric[T]) :Numeric[InOut[T]] =
		new InOutNumeric(numeric)
}



/** Factory of boxed in/out method parameters. */
object InOut extends InOutNumericImplicits {

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

	
	

	/** An intermediate value of a ''test-and-set'' operation initiated by [[InOut#:?]]. */
	final class TestAndSet[@specialized(SpecializedTypes) T] private[vars] (x :InOut[T], expect :T) {
		/** If the current value of tested variable equals the preceding value, assign to it the new value. */
		@inline  def :=(value :T) :Boolean = x.testAndSet(expect, value)
	}


	/** Similarly to `=:=`, attests that `X` and `Y` can be safely cast from one to another, i.e. that they are
	  * the same type. This trait is specialized in order to enforce specialization of accepting methods, which
	  * wouldn't be the case with `=:=`.
	  */
	private[vars] trait TypeEquiv[@specialized(SpecializedTypes) X, Y] {
		def apply(param :InOut[X]) :InOut[Y]
	}
	
	private[vars] final class TypeIdent[@specialized(SpecializedTypes) X] extends TypeEquiv[X, X] {
		override def apply(param :InOut[X]) :InOut[X] = param

		override def equals(other :Any) :Boolean = other match {
			case ident :TypeIdent[_] => getClass == ident.getClass
			case _ => false
		}
	}
	
	private[vars] implicit val BoolEq = new TypeIdent[Boolean]
	private[vars] implicit val IntEq = new TypeIdent[Int]
	private[vars] implicit val LongEq = new TypeIdent[Long]
	private[vars] implicit val FloatEq = new TypeIdent[Float]
	private[vars] implicit val DoubleEq = new TypeIdent[Double]
	private[vars] implicit val ShortEq = new TypeIdent[Short]
	private[vars] implicit val ByteEq = new TypeIdent[Byte]
	private[vars] implicit val CharEq = new TypeIdent[Char]
	
	
	
	
	
/******************************* Ordering and Numeric related type classes ********************************************/	
	
	

	/** Base class for type classes derived from `Ordering[InOut[T]]` backed by a corresponding type class for its value type. */
	class InOutOrderingLike[T, C[X] <: Ordering[X]](inner :C[T]) extends Ordering[InOut[T]] {
		@inline final protected[this] def vals :C[T] = inner

		override def compare(x :InOut[T], y :InOut[T]) :Int = vals.compare(x.get, y.get)
	}


	/** Implicit `Ordering` type class for `InOut[T]` available whenever implicit value for `Ordering[T]` can be found. */
	class InOutOrdering[T](ordering :Ordering[T]) extends InOutOrderingLike[T, Ordering](ordering)


	/** Base class for type classes on `InOut[T]` derived from `Numeric[InOut[T]]` and backed by the corresponding type class on
	  * the value type `T`.
	  */
	class InOutNumericLike[T, C[X] <: Numeric[X]](nums :C[T]) extends InOutOrderingLike[T, C](nums) with Numeric[InOut[T]] {
		override def plus(x :InOut[T], y :InOut[T]) :InOut[T] = InOut(vals.plus(x.get, y.get))

		override def minus(x :InOut[T], y :InOut[T]) :InOut[T] = InOut(vals.minus(x.get, y.get))

		override def times(x :InOut[T], y :InOut[T]) :InOut[T] = InOut(vals.times(x.get, y.get))

		override def negate(x :InOut[T]) :InOut[T] = InOut(vals.negate(x.get))

		override def fromInt(x :Int) :InOut[T] = InOut(vals.fromInt(x))

		override def toInt(x :InOut[T]) :Int = vals.toInt(x.get)

		override def toLong(x :InOut[T]) :Long = vals.toLong(x.get)

		override def toFloat(x :InOut[T]) :Float = vals.toFloat(x.get)

		override def toDouble(x :InOut[T]) :Double = vals.toDouble(x.get)
	}


	/** Implicit type class `Numeric[InOut[T]]` providing numeric operations on `InOut[T]` whenever an implicit value for
	  * `Numeric[T]` is available.
	  */
	class InOutNumeric[T](nums :Numeric[T]) extends InOutNumericLike[T, Numeric](nums)


	/** Implicit type class `Integral[InOut[T]]` providing integral operations on `InOut[T]` whenever an implicit value for
	  * `Numeric[T]` is available.
	  */
	class InOutIntegral[T](nums :Integral[T]) extends InOutNumericLike[T, Integral](nums) with Integral[InOut[T]] {
		override def quot(x :InOut[T], y :InOut[T]) :InOut[T] = InOut(vals.quot(x.get, y.get))

		override def rem(x :InOut[T], y :InOut[T]) :InOut[T] = InOut(vals.rem(x.get, y.get))
	}

	implicit def InOutIntegral[T](implicit integral :Integral[T]) :Integral[InOut[T]] = new InOutIntegral(integral)


	/** Implicit type class `Fractional[InOut[T]]` providing numeric operations on `InOut[T]` whenever an implicit value for
	  * `Numeric[T]` is available.
	  */
	class InOutFractional[T](nums :Fractional[T]) extends InOutNumericLike[T, Fractional](nums) with Fractional[InOut[T]] {
		override def div(x :InOut[T], y :InOut[T]) :InOut[T] = InOut(vals.div(x.get, y.get))
	}

	implicit def InOutFractional[T](implicit fractional :Fractional[T]) :Fractional[InOut[T]] = new InOutFractional(fractional)



	
/******************************** Implicits with type-specific arithmetic *********************************************/	




	/** Implicit extension of `InOut[Boolean]` providing logical operations on the variable.
	  * All these methods are polymorphic (''virtual''), so if the underlying dynamic type of the patched
	  * variable is, for example, synchronized, they will take advantage of that.
	  * If you wish to avoid them and to invoke the appropriate methods statically with possible inlining,
	  * use the appropriate type of the variable ([[Var]] or
	  * [[SyncVar]].
	  */
	implicit class InOutBooleanOps(private val x :InOut[Boolean]) extends AnyVal {
		
		/** Assigns this variable its (eager) logical conjunction with the given argument: `x := x & other`.
		  * This should be preferred whenever the argument is readily available (such as a `val` member), as
		  * the lazy alternative will not be inlined in most scenarios and require an actual function call.
		  * As the static type of this variable is the generic `InOut[Boolean]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def &=(other :Boolean) :Unit =  x.bool_&=(other)

		/** Assigns this variable its (lazy) logical conjunction with the given argument: `x := x && other`.
		  * As the static type of this variable is the generic `InOut[Boolean]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def &&=(other: =>Boolean) :Unit = x.bool_&&=(other)

		/** Assigns this variable its (eager) logical disjunction with the given argument: `x := x | other`.
		  * This should be preferred whenever the argument is readily available (such as a `val` member), as
		  * the lazy alternative will not be inlined in most scenarios and require an actual function call.
		  * As the static type of this variable is the generic `InOut[Boolean]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def |=(other :Boolean) :Unit = x.bool_|=(other)

		/** Assigns this variable its (lazy) logical disjunction with the given argument: `x := x || other`.
		  * As the static type of this variable is the generic `InOut[Boolean]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def ||=(other: =>Boolean) :Unit = x.bool_||=(other)

		/** Performs on this variable logical ''xor'' with the given argument: `x := x ^ other`.
		  * As the static type of this variable is the generic `InOut[Boolean]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def ^=(other :Boolean) :Unit = x.bool_^=(other)

		/** Negates the value of this variable: `x := !x`.
		  * As the static type of this variable is the generic `InOut[Boolean]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  * @return the new (negated) value of this variable
		  */
		@inline def neg() :Boolean = x.bool_!=

		/** Negates the value of this variable: `x := !x`.
		  * As the static type of this variable is the generic `InOut[Boolean]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def flip() :Unit = x.bool_!=

	}


	
	
	
	/** Implicit conversion of a `InOut[Int]` variable providing basic arithmetic and bitwise operations. 
	  * All these methods are polymorphic (''virtual''), so if the underlying dynamic type of the patched
	  * variable is, for example, synchronized, they will take advantage of that.
	  * If you wish to avoid them and to invoke the appropriate methods statically with possible inlining,
	  * use the appropriate type of the variable ([[Var]] or
	  * [[SyncVar]].
	  */
	implicit class InOutIntArithmetic(private val x :InOut[Int]) extends AnyVal {

		/** Increases the value of this variable by the specified number. 
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def +=(n :Int) :Unit = x.int_+=(n)

		/** Decreases the value of this variable by the specified number. 
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def -=(n :Int) :Unit = x.int_+=(-n)

		/** Multiplies the value of this variable by the specified number. 
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def *=(n :Int) :Unit = x.int_*=(n)

		/** Divides the value of this variable by the specified number. 
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def /=(n :Int) :Unit = x.int_/=(n)

		/** Assigns to this variable the remainder of division by the specified number. 
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def %=(n :Int) :Unit = x.int_%=(n)

		/** Increments this variable by `1`, C-style. 
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def ++ :Unit = x.int_+=(1)

		/** Decrements this variable by `1`, C-style.
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def -- :Unit = x.int_+=(-1)



		/** Increases the value of this variable by the specified number, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def inc(n :Int) :Int = x.int_+=(n)

		/** Decreases the value of this variable by the specified number, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def dec(n :Int) :Int = x.int_+=(-n)

		/** Multiplies the value of this variable by the specified number, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def mult(n :Int) :Int = x.int_*=(n)

		/** Divides the value of this variable by the specified number, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def div(n :Int) :Int = x.int_/=(n)

		/** Assigns to this variable the reminder of division by the specified number, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def rem(n :Int) :Int = x.int_%=(n)

		/** Increments this variable by `1`, C-style, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def inc() :Int = x.int_+=(1)

		/** Decrements this variable by `1`, C-style, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def dec() :Int = x.int_+=(-1)

		/** Assigns this variable its opposite value (`this := -this`), returning the updated value.
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def neg() :Int = x.int_-
	
		
		/** Performs bitwise disjunction on this variable with the given number. 
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def |=(n :Int) :Unit = x.int_|=(n)

		/** Performs bitwise conjunction on this variable with the given number. 
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def &=(n :Int) :Unit = x.int_&=(n)

		/** Performs bitwise exclusive disjunction on this variable with the given number. 
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def ^=(n :Int) :Unit = x.int_^=(n)

		/** Bit-shifts right the value of this variable by the specified number of bits, replacing shifted higher bits with the sign bit. 
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def >>=(n :Int) :Unit = x.int_>>=(n)

		/** Bit-shifts right the value of this variable by the specified number of bits, replacing shifted higher bits with zeros. 
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def >>>=(n :Int) :Unit = x.int_>>>=(n)

		/** Bit-shifts left the value of this variable by the specified number of bits, replacing shifted higher bits with zeros. 
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def <<=(n :Int) :Unit = x.int_<<=(n)

		/** Assigns this variable its bitwise negation: `this := !this.value`. 
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def flip() :Unit = x := x.int_~
	}

	
	
	



	/** Implicit conversion of a `InOut[Long]` variable providing basic arithmetic and bitwise operations. 
	  * All these methods are polymorphic (''virtual''), so if the underlying dynamic type of the patched
	  * variable is, for example, synchronized, they will take advantage of that.
	  * If you wish to avoid them and to invoke the appropriate methods statically with possible inlining,
	  * use the appropriate type of the variable ([[Var]] or
	  * [[SyncVar]].
	  */
	implicit class InOutLongArithmetic(private val x :InOut[Long]) extends AnyVal {

		/** Increases the value of this variable by the specified number. 
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def +=(n :Long) :Unit = x.long_+=(n)

		/** Decreases the value of this variable by the specified number. 
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def -=(n :Long) :Unit = x.long_+=(-n)

		/** Multiplies the value of this variable by the specified number. 
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def *=(n :Long) :Unit = x.long_*=(n)

		/** Divides the value of this variable by the specified number. 
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def /=(n :Long) :Unit = x.long_/=(n)

		/** Assigns to this variable the remainder of division by the specified number. 
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def %=(n :Long) :Unit = x.long_%=(n)

		/** Increments this variable by `1`, C-style. 
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def ++ :Unit = x.long_+=(1)

		/** Decrements this variable by `1`, C-style.
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def -- :Unit = x.long_+=(-1)

		/** Increases the value of this variable by the specified number, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def inc(n :Long) :Long = x.long_+=(n)

		/** Decreases the value of this variable by the specified number, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def dec(n :Long) :Long = x.long_+=(-n)

		/** Multiplies the value of this variable by the specified number, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def mult(n :Long) :Long = x.long_*=(n)

		/** Divides the value of this variable by the specified number, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def div(n :Long) :Long = x.long_/=(n)

		/** Assigns to this variable the reminder of division by the specified number, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def rem(n :Long) :Long = x.long_%=(n)

		/** Increments this variable by `1`, C-style, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def inc() :Long = x.long_+=(1)

		/** Decrements this variable by `1`, C-style, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def dec() :Long = x.long_+=(-1)

		/** Assigns this variable its opposite value (`this := -this`), returning the updated value.
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def neg() :Long = x.long_-
		
		
		/** Performs bitwise disjunction on this variable with the given number. 
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def |=(n :Long) :Unit = x.long_|=(n)

		/** Performs bitwise conjunction on this variable with the given number. 
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def &=(n :Long) :Unit = x.long_&=(n)

		/** Performs bitwise exclusive disjunction on this variable with the given number. 
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def ^=(n :Long) :Unit = x.long_^=(n)

		/** Bit-shifts right the value of this variable by the specified number of bits, replacing shifted higher bits with the sign bit. 
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def >>=(n :Int) :Unit = x.long_>>=(n)

		/** Bit-shifts right the value of this variable by the specified number of bits, replacing shifted higher bits with zeros. 
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def >>>=(n :Int) :Unit = x.long_>>>=(n)

		/** Bit-shifts left the value of this variable by the specified number of bits, replacing shifted higher bits with zeros.
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def <<=(n :Int) :Unit = x.long_<<=(n)

		/** Assigns this variable its bitwise negation: `this := !this.value`. 
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def flip() :Unit = x := x.long_~

	}






	/** Implicit conversion of a `InOut[Float]` variable providing basic arithmetic and bitwise operations. 
	  * All these methods are polymorphic (''virtual''), so if the underlying dynamic type of the patched
	  * variable is, for example, synchronized, they will take advantage of that.
	  * If you wish to avoid them and to invoke the appropriate methods statically with possible inlining,
	  * use the appropriate type of the variable ([[Var]] or
	  * [[SyncVar]].
	  */
	implicit class InOutFloatArithmetic(private val x :InOut[Float]) extends AnyVal {

		/** Increases the value of this variable by the specified number. 
		  * As the static type of this variable is the generic `InOut[Float]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def +=(n :Float) :Unit = x.float_+=(n)

		/** Decreases the value of this variable by the specified number. 
		  * As the static type of this variable is the generic `InOut[Float]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def -=(n :Float) :Unit = x.float_+=(-n)

		/** Multiplies the value of this variable by the specified number. 
		  * As the static type of this variable is the generic `InOut[Float]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def *=(n :Float) :Unit = x.float_*=(n)

		/** Divides the value of this variable by the specified number. 
		  * As the static type of this variable is the generic `InOut[Float]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def /=(n :Float) :Unit = x.float_/=(n)



		/** Increases the value of this variable by the specified number, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Float]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def inc(n :Float) :Float = x.float_+=(n)

		/** Decreases the value of this variable by the specified number, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Float]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def dec(n :Float) :Float = x.float_+=(-n)

		/** Multiplies the value of this variable by the specified number, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Float]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def mult(n :Float) :Float = x.float_*=(n)

		/** Divides the value of this variable by the specified number, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Float]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def div(n :Float) :Float = x.float_/=(n)

		/** Assigns this variable its opposite value (`this := -this`), returning the updated value.
		  * As the static type of this variable is the generic `InOut[Float]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def neg() :Float = x.float_-
		
	}








	/** Implicit conversion of a `InOut[Double]` variable providing basic arithmetic and bitwise operations. 
	  * All these methods are polymorphic (''virtual''), so if the underlying dynamic type of the patched
	  * variable is, for example, synchronized, they will take advantage of that.
	  * If you wish to avoid them and to invoke the appropriate methods statically with possible inlining,
	  * use the appropriate type of the variable ([[Var]] or
	  * [[SyncVar]].
	  */
	implicit class InOutDoubleArithmetic(private val x :InOut[Double]) extends AnyVal {

		/** Increases the value of this variable by the specified number. 
		  * As the static type of this variable is the generic `InOut[Double]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def +=(n :Double) :Unit = x.double_+=(n)

		/** Decreases the value of this variable by the specified number. 
		  * As the static type of this variable is the generic `InOut[Double]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def -=(n :Double) :Unit = x.double_+=(-n)

		/** Multiplies the value of this variable by the specified number. 
		  * As the static type of this variable is the generic `InOut[Double]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def *=(n :Double) :Unit = x.double_*=(n)

		/** Divides the value of this variable by the specified number. 
		  * As the static type of this variable is the generic `InOut[Double]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def /=(n :Double) :Unit = x.double_/=(n)




		/** Increases the value of this variable by the specified number, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Double]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def inc(n :Double) :Double = x.double_+=(n)

		/** Decreases the value of this variable by the specified number, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Double]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def dec(n :Double) :Double = x.double_+=(-n)

		/** Multiplies the value of this variable by the specified number, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Double]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def mult(n :Double) :Double = x.double_*=(n)

		/** Divides the value of this variable by the specified number, returning the updated value. 
		  * As the static type of this variable is the generic `InOut[Double]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def div(n :Double) :Double = x.double_/=(n)

		/** Assigns this variable its opposite value (`this := -this`), returning the updated value.
		  * As the static type of this variable is the generic `InOut[Double]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def neg() :Double = x.double_-
		
	}

}
