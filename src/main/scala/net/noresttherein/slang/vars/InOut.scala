package net.noresttherein.slang.vars

import scala.Specializable.Args
import scala.annotation.nowarn

import net.noresttherein.slang.optional.Opt
import net.noresttherein.slang.optional.Opt.Got
import net.noresttherein.slang.vars.InOut.{InOutNumeric, InOutOrdering, SpecializedVars, TestAndSet, TypeEquiv}
import net.noresttherein.slang.witness.DefaultValue



/** An interface for mutable values being reference wrappers over `var` fields. Designed for in/out parameters to functions.
  * Implemented by several boxing classes which may provide additional features such as synchronization.
  * Implicit conversions exist providing arithmetic suitable to the type of the boxed value, so for example
  * you can write `param += 1` for `param :InOut[Int]`.
  * @tparam T type of this variable
  * @see [[net.noresttherein.slang.vars.Var]]
  * @see [[net.noresttherein.slang.vars.Volatile]]
  * @see [[net.noresttherein.slang.vars.SyncVar]]
  */
trait InOut[@specialized(SpecializedVars) T] extends Ref[T] {

	/** The wrapped value.
	  * @return `this.`[[net.noresttherein.slang.vars.InOut.value value]].
	  */
	override def apply() :T = value

	/** The current value of this variable. */
	def value :T

	/** Assign a new value to this variable. */
	def value_=(newValue :T) :Unit

	/** Assigns a new value to this variable.
	  *  Equivalent to `this.value `[[net.noresttherein.slang.vars.InOut.value_= =]]` newValue`.
	  */
	def :=(newValue :T) :Unit = value = newValue

	/** Assigns a new value returning the previous value.
	  * No guarantee is made by this interface about atomicity of this operation.
	  */
	def ?=(newValue :T) :T = { val res = value; value = newValue; res }

	override def opt :Opt[T] = Got(value)
	
	/** Assigns a new value to this variable providing the current value is equal to the expected value.
	  * The default implementation does it the direct way without any guarantees about multi-threaded semantics.
	  * This method is of real practical use only in concurrent `InOut` implementations such as
	  * [[net.noresttherein.slang.vars.SyncVar SyncVar]], [[net.noresttherein.slang.vars.Atomic Atomic]]
	  * and [[net.noresttherein.slang.vars.Volatile Volatile]].
	  * @param expect   a value to compare with current value.
	  * @param newValue a new value for this variable.
	  * @return `true` if the previous value equaled `expect` and the variable has been set to `newValue`.
	  */
	def testAndSet(expect :T, newValue :T) :Boolean =
		(value == expect) && { value = newValue; true }

	/** A ''test-and-set'' operation divided syntactically into two binary operators.
	  * `x :? expect := value` is equivalent to `x.testAndSet(expect, value)`.
	  * @param expect a value to compare with current value of this variable.
	  * @return an intermediate object which will perform the comparison and assign the value given to its
	  *         [[net.noresttherein.slang.vars.InOut.TestAndSet.:=]] method.
	  * @see [[net.noresttherein.slang.vars.InOut.testAndSet testAndSet()]]
	  */
	@inline final def :?(expect :T) :TestAndSet[T] = new TestAndSet(this, expect)


	/** Updates the value of this variable with the given function. Default implementation is equivalent to
	  * `val res = f(this.get); this.value = res; res` and has no benefit over direct application in client code.
	  * This method comes to use with concurrent `InOut` implementations such as
	  * [[net.noresttherein.slang.vars.SyncVar SyncVar]] and [[net.noresttherein.slang.vars.Atomic Atomic]] -
	  * the semantics of simple default [[net.noresttherein.slang.vars.Var Var]] offers no guarantees in multi-threaded
	  * environments.
	  * @param f function to apply to the value of this variable. Should have no side effects as it may be invoked
	  *          several times.
	  * @return the result of applying `f` to the current value.
	  */
	def apply(f :T => T) :T = { val res = f(value); value = res; res }

	/** Combines the value of this variable with a value of some other type, assigning the result of application
	  * back to this variable before returning it. It uses this variable as an accumulator, updated iteratively with
	  * new values in a way similar to an in place ''foldLeft'' operation on a singleton collection; the difference
	  * from `foldLeft` is that the function's result is the type of this variable, rather than the argument.
	  * Default implementation naively performs this directly without any guarantees about multi-threaded semantics
	  * and is equivalent to `val res = foldLeft(z, value); value = res; res`. Scala specific fold notation is
	  * chosen here to remind through associativity that this variable becomes the second (right) parameter
	  * of the folding function, with the argument on the left side of the operator being the first. This method comes
	  * to use with concurrent `InOut` implementations such as [[net.noresttherein.slang.vars.SyncVar SyncVar]]
	  * or [[net.noresttherein.slang.vars.Atomic Atomic]].
	  * @param z        an accumulator value to pass as the first argument to the `foldLeft` function, together
	  *                 with the current value of this variable.
	  * @param foldLeft a function applied to the argument and this variable whose result should be set
	  *                 to this variable.
	  * @return the result of applying `foldLeft` to the argument and this variable.
	  */
	@inline final def /=:[@specialized(Args) A](z :A)(foldLeft :(A, T) => T) :T = applyLeft(z)(foldLeft)

	/** Combines the value of this variable with a value of some other type, assigning the result of application
	  * back to this variable before returning it. It uses this variable as an accumulator, updated iteratively with
	  * new values in a way similar to an in place ''foldRight'' operation on a singleton collection; the difference
	  * from `foldRight` is that the function's result is the type of this variable, rather than the argument.
	  * Default implementation naively performs this directly without any guarantees about multi-threaded semantics
	  * and is equivalent to `val res = foldRight(value, z); value = res; res`. Scala specific fold notation is
	  * chosen here to remind through associativity that this variable becomes the first (left) parameter of the folding
	  * function, with the argument on the right side of the operator being the second. This method comes to use with
	  * concurrent `InOut` implementations such as [[net.noresttherein.slang.vars.SyncVar SyncVar]]
	  * or [[net.noresttherein.slang.vars.Atomic Atomic]].
	  * @param z         an accumulator value to pass as the second argument to the `foldRight` function, together
	  *                  with the current value of this variable.
	  * @param foldRight a function applied to this variable and the argument, whose result should be set
	  *                  to this variable.
	  * @return the result of applying `foldLeft` to this variable and the argument.
	  */
	@inline final def :\=[@specialized(Args) A](z :A)(foldRight :(T, A) => T) :T = applyRight(z)(foldRight)

	/** Combines the value of this variable with a value of some other type, assigning the result of application
	  * back to this variable before returning it. It uses this variable as an accumulator, updated iteratively with
	  * new values in a way similar to an in place ''foldLeft'' operation on a singleton collection; the difference
	  * from `foldLeft` is that the function's result is the type of this variable, rather than the argument.
	  * Default implementation naively performs this directly without any guarantees about multi-threaded semantics
	  * and is equivalent to `val res = f(z, value); value = res; res`. This method comes
	  * to use with concurrent `InOut` implementations such as [[net.noresttherein.slang.vars.SyncVar SyncVar]]
	  * or [[net.noresttherein.slang.vars.Atomic Atomic]].
	  * @param z accumulator value to pass as the first argument to the `f` function, together with the current
	  *          value of this variable.
	  * @param f a function applied to the argument and this variable, whose result should be set to this variable.
	  * @return the result of applying `f` to this variable and the argument.
	  */
	def applyLeft[@specialized(Args) A](z :A)(f :(A, T) => T) :T = {
		val res = f(z, value); value = res; res
	}

	/** Combines the value of this variable with a value of some other type, assigning the result of application
	  * back to this variable before returning it. It uses this variable as an accumulator, updated iteratively with
	  * new values in a way similar to an in place ''foldRight'' operation on a singleton collection; the difference
	  * from `foldRight` is that the function's result is the type of this variable, rather than the argument.
	  * Default implementation naively performs this directly without any guarantees about multi-threaded semantics
	  * and is equivalent to `val res = f(value, z); value = res; res`. This method comes
	  * to use with concurrent `InOut` implementations such as [[net.noresttherein.slang.vars.SyncVar SyncVar]]
	  * or [[net.noresttherein.slang.vars.Atomic Atomic]].
	  * @param z accumulator value to pass as the second argument to the `f` function, together with the current
	  *          value of this variable.
	  * @param f a function applied to the this variable and the argument, whose result should be set to this variable.
	  * @return the result of applying `f` to this variable and the argument.
	  */
	def applyRight[@specialized(Args) A](z :A)(f :(T, A) => T) :T = {
		val res = f(value, z); value = res; res
	}



	/************************************** Boolean methods ***********************************************************/

	private[vars] def bool_&=(other :Boolean)(implicit ev :T TypeEquiv Boolean) :Unit = ev(this).applyLeft(other)(_ & _)
	private[vars] def bool_|=(other :Boolean)(implicit ev :T TypeEquiv Boolean) :Unit = ev(this).applyLeft(other)(_ | _)
	private[vars] def bool_&&=(other: => Boolean)(implicit ev :T TypeEquiv Boolean) :Unit = ev(this)(_ && other)
	private[vars] def bool_||=(other: => Boolean)(implicit ev :T TypeEquiv Boolean) :Unit = ev(this)(_ || other)
	private[vars] def bool_^=(other :Boolean)(implicit ev :T TypeEquiv Boolean) :Unit = ev(this).applyLeft(other)(_ ^ _)
	private[vars] def bool_!=(implicit ev :T TypeEquiv Boolean) :Boolean = ev(this)(!_)


	/************************************** Int methods ***************************************************************/

	private[vars] def int_+=(other :Int)(implicit ev :T TypeEquiv Int) :Int = ev(this).applyRight(other)(_ + _)
	private[vars] def int_*=(other :Int)(implicit ev :T TypeEquiv Int) :Int = ev(this).applyRight(other)(_ * _)
	private[vars] def int_/=(other :Int)(implicit ev :T TypeEquiv Int) :Int = ev(this).applyRight(other)(_ / _)
	private[vars] def int_%=(other :Int)(implicit ev :T TypeEquiv Int) :Int = ev(this).applyRight(other)(_ % _)
	private[vars] def int_-(implicit ev :T TypeEquiv Int) :Int = ev(this)(-_)
	private[vars] def int_~(implicit ev :T TypeEquiv Int) :Int = ev(this)(~_)
	private[vars] def int_|=(other :Int)(implicit ev :T TypeEquiv Int) :Int = ev(this).applyRight(other)(_ | _)
	private[vars] def int_&=(other :Int)(implicit ev :T TypeEquiv Int) :Int = ev(this).applyRight(other)(_ & _)
	private[vars] def int_^=(other :Int)(implicit ev :T TypeEquiv Int) :Int = ev(this).applyRight(other)(_ ^ _)
	private[vars] def int_>>=(n :Int)(implicit ev :T TypeEquiv Int) :Int = ev(this).applyRight(n)(_ >> _)
	private[vars] def int_>>>=(n :Int)(implicit ev :T TypeEquiv Int) :Int = ev(this).applyRight(n)(_ >>> _)
	private[vars] def int_<<=(n :Int)(implicit ev :T TypeEquiv Int) :Int = ev(this).applyRight(n)(_ << _)


	/************************************** Long methods **************************************************************/

	private[vars] def long_+=(other :Long)(implicit ev :T TypeEquiv Long) :Long = ev(this).applyRight(other)(_ + _)
	private[vars] def long_*=(other :Long)(implicit ev :T TypeEquiv Long) :Long = ev(this).applyRight(other)(_ * _)
	private[vars] def long_/=(other :Long)(implicit ev :T TypeEquiv Long) :Long = ev(this).applyRight(other)(_ / _)
	private[vars] def long_%=(other :Long)(implicit ev :T TypeEquiv Long) :Long = ev(this).applyRight(other)(_ % _)
	private[vars] def long_-(implicit ev :T TypeEquiv Long) :Long = ev(this)(-_)
	private[vars] def long_~(implicit ev :T TypeEquiv Long) :Long = ev(this)(~_)
	private[vars] def long_|=(other :Long)(implicit ev :T TypeEquiv Long) :Long = ev(this).applyRight(other)(_ | _)
	private[vars] def long_&=(other :Long)(implicit ev :T TypeEquiv Long) :Long = ev(this).applyRight(other)(_ & _)
	private[vars] def long_^=(other :Long)(implicit ev :T TypeEquiv Long) :Long = ev(this).applyRight(other)(_ ^ _)
	private[vars] def long_>>=(n :Int)(implicit ev :T TypeEquiv Long) :Long = ev(this).applyRight(n)(_ >> _)
	private[vars] def long_>>>=(n :Int)(implicit ev :T TypeEquiv Long) :Long = ev(this).applyRight(n)(_ >>> _)
	private[vars] def long_<<=(n :Int)(implicit ev :T TypeEquiv Long) :Long = ev(this).applyRight(n)(_ << _)


	/************************************** Float methods *************************************************************/

	private[vars] def float_+=(other :Float)(implicit ev :T TypeEquiv Float) :Float = ev(this).applyRight(other)(_ + _)
	private[vars] def float_*=(other :Float)(implicit ev :T TypeEquiv Float) :Float = ev(this).applyRight(other)(_ * _)
	private[vars] def float_/=(other :Float)(implicit ev :T TypeEquiv Float) :Float = ev(this).applyRight(other)(_ / _)
	private[vars] def float_-(implicit ev :T TypeEquiv Float) :Float = ev(this)(-_)


	/************************************** Double methods ************************************************************/

	private[vars] def double_+=(other :Double)(implicit ev :T TypeEquiv Double) :Double = (ev(this) :\= other)(_ + _)
	private[vars] def double_*=(other :Double)(implicit ev :T TypeEquiv Double) :Double = (ev(this) :\= other)(_ * _)
	private[vars] def double_/=(other :Double)(implicit ev :T TypeEquiv Double) :Double = (ev(this) :\= other)(_ / _)
	private[vars] def double_-(implicit ev :T TypeEquiv Double) :Double = ev(this)(-_)



	override def equals(that :Any) :Boolean = that match {
		case v :InOut[_] => (v eq this) || (v canEqual this) && v.opt == opt
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[InOut[_]]
	override def hashCode :Int = opt.hashCode

	override def toString :String = String.valueOf(value)
}






sealed abstract class InOutOrderingImplicits {
	@inline implicit def InOutOrdering[T](implicit ordering :Ordering[T]) :Ordering[InOut[T]] =
		new InOutOrdering(ordering)
}

sealed abstract class InOutNumericImplicits extends InOutOrderingImplicits {
	@inline implicit def InOutNumeric[T](implicit numeric :Numeric[T]) :Numeric[InOut[T]] =
		new InOutNumeric(numeric)
}



/** Factory of boxed in/out method parameters. */
object InOut extends InOutNumericImplicits {

	/** Unbox the value hold by an `InOut` wrapper. */
	@inline final implicit def unboxInOut[@specialized(SpecializedVars) T](variable :InOut[T]) :T = variable.value




	/** Create a wrapper over a '''`var`''' of type `T` which can be passed as an in/out method parameter. */
	@inline def apply[@specialized(SpecializedVars) T](value :T) :InOut[T] = new Var[T](value)

	/** Create a wrapper over a '''`var`''' of type `T` which can be passed as an in/out method parameter.*/
	@inline def apply[@specialized(SpecializedVars) T](implicit default :DefaultValue[T]) :InOut[T] =
		new Var[T](default.default)


	final val SpecializedVars = new Specializable.Group(Byte, Short, Char, Int, Long, Float, Double, Boolean)



	/** Extra implicits which might be helpful but can also lead to tricky bugs or cause conflicts. */
	object implicits {
		/** Implicitly creates a `InOut` instance with a given value. This implicit is optional as the main use of `InOut[T]`
		  * is to be used as in/out method parameters. In that scenario, using a value identifier instead of a `InOut[T]`
		  * makes no sense and would likely be an error.
		  */
		@inline implicit def boxInOutParam[@specialized(SpecializedVars) T](value :T) :InOut[T] = new Var[T](value)

		/** Implicit extension of values of any type allowing chained assignments to compatible variables in the form of
		  * `x1 =: x2 =: x3 =: value` or `(x1, x2, x3) =: value`. This conversion needs to be manually imported
		  * as the wide scope of accepted input values can easily lead to conflicts with different libraries.
		  * @param value value to assign to a chain of variables.
		  */
		@inline implicit def InOutMultiAssignment[@specialized(SpecializedVars) T](value :T) :InOutMultiAssignment[T, InOut[T]] =
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
					case list :List[T @unchecked] =>
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



	/** An intermediate value of a ''test-and-set'' operation initiated by [[net.noresttherein.slang.vars.InOut.:? :?]]. */
	final class TestAndSet[@specialized(SpecializedVars) T] private[vars](x :InOut[T], expect :T) {
		/** If the current value of tested variable equals the preceding value, assign to it the new value. */
		@inline  def :=(value :T) :Boolean = x.testAndSet(expect, value)
	}



	/** Similarly to `=:=`, attests that `X` and `Y` can be safely cast from one to another, i.e. that they are
	  * the same type. This trait is specialized in order to enforce specialization of accepting methods, which
	  * wouldn't be the case with `=:=`.
	  */
	private[vars] trait TypeEquiv[@specialized(SpecializedVars) X, Y] {
		def apply[B[_]](param :B[X]) :B[Y]
	}

	private[vars] final class TypeIdent[@specialized(SpecializedVars) X] extends TypeEquiv[X, X] {
		override def apply[B[_]](param :B[X]) :B[X] = param

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
	private[vars] val AnyRefEq = new TypeIdent[AnyRef]





/******************************* Ordering and Numeric related type classes ********************************************/



	/** Base class for type classes derived from `Ordering[InOut[T]]` backed by a corresponding type class for its value type. */
	class InOutOrderingLike[T, C[X] <: Ordering[X]](inner :C[T]) extends Ordering[InOut[T]] {
		@inline final protected[this] def vals :C[T] = inner

		override def compare(x :InOut[T], y :InOut[T]) :Int = vals.compare(x.value, y.value)
	}


	/** Implicit `Ordering` type class for `InOut[T]` available whenever implicit value for `Ordering[T]` can be found. */
	class InOutOrdering[T](ordering :Ordering[T]) extends InOutOrderingLike[T, Ordering](ordering)


	/** Base class for type classes on `InOut[T]` derived from `Numeric[InOut[T]]` and backed by the corresponding type class on
	  * the value type `T`.
	  */
	class InOutNumericLike[T, C[X] <: Numeric[X]](nums :C[T]) extends InOutOrderingLike[T, C](nums) with Numeric[InOut[T]] {
		override def plus(x :InOut[T], y :InOut[T]) :InOut[T] = InOut(vals.plus(x.value, y.value))

		override def minus(x :InOut[T], y :InOut[T]) :InOut[T] = InOut(vals.minus(x.value, y.value))

		override def times(x :InOut[T], y :InOut[T]) :InOut[T] = InOut(vals.times(x.value, y.value))

		override def negate(x :InOut[T]) :InOut[T] = InOut(vals.negate(x.value))

		override def fromInt(x :Int) :InOut[T] = InOut(vals.fromInt(x))

		override def toInt(x :InOut[T]) :Int = vals.toInt(x.value)

		override def toLong(x :InOut[T]) :Long = vals.toLong(x.value)

		override def toFloat(x :InOut[T]) :Float = vals.toFloat(x.value)

		override def toDouble(x :InOut[T]) :Double = vals.toDouble(x.value)

		override def parseString(str :String) :Option[InOut[T]] = nums.parseString(str).map(InOut(_))
	}


	/** Implicit type class `Numeric[InOut[T]]` providing numeric operations on `InOut[T]` whenever an implicit value for
	  * `Numeric[T]` is available.
	  */
	class InOutNumeric[T](nums :Numeric[T]) extends InOutNumericLike[T, Numeric](nums)


	/** Implicit type class `Integral[InOut[T]]` providing integral operations on `InOut[T]` whenever an implicit value for
	  * `Numeric[T]` is available.
	  */
	class InOutIntegral[T](nums :Integral[T]) extends InOutNumericLike[T, Integral](nums) with Integral[InOut[T]] {
		override def quot(x :InOut[T], y :InOut[T]) :InOut[T] = InOut(vals.quot(x.value, y.value))

		override def rem(x :InOut[T], y :InOut[T]) :InOut[T] = InOut(vals.rem(x.value, y.value))
	}

	implicit def InOutIntegral[T](implicit integral :Integral[T]) :Integral[InOut[T]] = new InOutIntegral(integral)


	/** Implicit type class `Fractional[InOut[T]]` providing numeric operations on `InOut[T]` whenever an implicit value for
	  * `Numeric[T]` is available.
	  */
	class InOutFractional[T](nums :Fractional[T]) extends InOutNumericLike[T, Fractional](nums) with Fractional[InOut[T]] {
		override def div(x :InOut[T], y :InOut[T]) :InOut[T] = InOut(vals.div(x.value, y.value))
	}

	implicit def InOutFractional[T](implicit fractional :Fractional[T]) :Fractional[InOut[T]] = new InOutFractional(fractional)




/******************************** Implicits with type-specific arithmetic *********************************************/


	/** Implicit extension of `InOut[Boolean]` providing logical operations on the variable.
	  * All these methods are polymorphic and guaranteed to have at least the same memory access semantics as
	  * [[net.noresttherein.slang.vars.InOut.applyRight applyRight]]. So if, for example, this variable is synchronized,
	  * then the whole operation will be `synchronized`. The variable class may however use a different (optimized)
	  * implementation, which might be observable if the argument expression is not idempotent.
	  * If you wish to avoid them and to invoke the appropriate methods statically with possible inlining,
	  * use the appropriate type of the variable ([[net.noresttherein.slang.vars.Var Var]] or
	  * [[net.noresttherein.slang.vars.SyncVar SyncVar]]).
	  */
	implicit class InOutBooleanOps(private val x :InOut[Boolean]) extends AnyVal {

		/** Assigns this variable its (eager) logical conjunction with the given argument: `x := x & other`.
		  * It should be preferred to `&&` whenever the argument is readily available (such as a `val` member), as
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
		  * It should be preferred to `||` whenever the argument is readily available (such as a `val` member), as
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
	  * All these methods are polymorphic and guaranteed to have the same memory access semantics as
	  * [[net.noresttherein.slang.vars.InOut.applyRight applyRight]], although the implementation details may differ.
	  * So if, for example, this variable is synchronized, then the whole operation will be `synchronized`.
	  * If you wish to bypass virtual calls and invoke the appropriate methods statically with possible inlining,
	  * use the appropriate type of the variable ([[net.noresttherein.slang.vars.Var Var]] or
	  * [[net.noresttherein.slang.vars.SyncVar SyncVar]]).
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
		@nowarn @inline def ++ :Unit = x.int_+=(1)

		/** Decrements this variable by `1`, C-style.
		  * As the static type of this variable is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@nowarn @inline def -- :Unit = x.int_+=(-1)



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
	  * All these methods are polymorphic and guaranteed to have the same memory access semantics as
	  * [[net.noresttherein.slang.vars.InOut.applyRight applyRight]], although the implementation details may differ.
	  * So if, for example, this variable is synchronized, then the whole operation will be `synchronized`.
	  * If you wish to bypass virtual calls and invoke the appropriate methods statically with possible inlining,
	  * use the appropriate type of the variable ([[net.noresttherein.slang.vars.Var Var]] or
	  * [[net.noresttherein.slang.vars.SyncVar SyncVar]]).
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
		@nowarn @inline def ++ :Unit = x.long_+=(1)

		/** Decrements this variable by `1`, C-style.
		  * As the static type of this variable is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@nowarn @inline def -- :Unit = x.long_+=(-1)

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
	  * All these methods are polymorphic and guaranteed to have the same memory access semantics as
	  * [[net.noresttherein.slang.vars.InOut.applyRight applyRight]], although the implementation details may differ.
	  * So if, for example, this variable is synchronized, then the whole operation will be `synchronized`.
	  * If you wish to bypass virtual calls and invoke the appropriate methods statically with possible inlining,
	  * use the appropriate type of the variable ([[net.noresttherein.slang.vars.Var Var]] or
	  * [[net.noresttherein.slang.vars.SyncVar SyncVar]]).
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
	  * All these methods are polymorphic and guaranteed to have the same memory access semantics as
	  * [[net.noresttherein.slang.vars.InOut.applyRight applyRight]], although the implementation details may differ.
	  * So if, for example, this variable is synchronized, then the whole operation will be `synchronized`.
	  * If you wish to bypass virtual calls and invoke the appropriate methods statically with possible inlining,
	  * use the appropriate type of the variable ([[net.noresttherein.slang.vars.Var Var]] or
	  * [[net.noresttherein.slang.vars.SyncVar SyncVar]]).
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
