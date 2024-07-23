package net.noresttherein.sugar.vars

import scala.Specializable.Args
import scala.annotation.nowarn
import scala.collection.mutable

import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.extensions.classNameMethods
import net.noresttherein.sugar.funny.generic.Any1
import net.noresttherein.sugar.{noSuch_!, outOfBounds_!}
import net.noresttherein.sugar.vars.InOut.{SpecializedVars, TestAndSet}
import net.noresttherein.sugar.vars.Maybe.Yes
import net.noresttherein.sugar.vars.Opt.One
import net.noresttherein.sugar.witness.DefaultValue




/** An interface for mutable values being reference wrappers over `var` fields. Designed for in/out parameters
  * of functions. Implemented by several boxing classes which may provide additional features such as synchronization.
  * Implicit conversions exist providing arithmetic suitable to the type of the boxed value, so for example
  * you can write `param += 1` for `param :InOut[Int]`. An unboxing implicit conversion `(v :InOut[T]) => v.value`
  * further reduces the syntax overhead for using this $ref. Note however, that certain special purpose implementations
  * may throw a `NoSuchElementException` from their `value` method.
  *
  * Note that while by default, and for all typical implementations such as [[net.noresttherein.sugar.vars.Var Var]]
  * or [[net.noresttherein.sugar.vars.Atomic Atomic]] method [[net.noresttherein.sugar.vars.InOut.get get]]
  * returns `this.`[[net.noresttherein.sugar.vars.InOut.value value]], these two properties can have different
  * semantics in some classes, such as [[net.noresttherein.sugar.vars.Freezer Freezer]].
  * Where applicable, the subclasses use `value` as the 'current' value and `get` for a more final/public version.
  * Furthermore, instances of some implementations can have no current value
  * (i.e., `value` throws a [[NoSuchElementException]] and [[net.noresttherein.sugar.vars.Ref.option option]]
  * returns `None`).
  * @tparam T the type of this variable.
  * @see [[net.noresttherein.sugar.vars.Var]]
  * @see [[net.noresttherein.sugar.vars.Volatile]]
  * @see [[net.noresttherein.sugar.vars.SyncVar]]
  * @define Ref `InOut`
  * @define ref variable
  */
trait InOut[@specialized(SpecializedVars) T] extends Ref[T] {

	override def apply() :T = get

	/** Assigns a new value to this variable. */
	@throws[IllegalStateException]("if the Ref is final.")
	def value_=(newValue :T) :Unit

	/** Assigns a new value to this variable.
	  * Equivalent to `this.value `[[net.noresttherein.sugar.vars.InOut.value_= =]]` newValue`.
	  */
	@throws[IllegalStateException]("if the Ref is final.")
	@inline final def :=(newValue :T) :Unit = value = newValue

	/** Assigns a new value returning the previous value.
	  * No guarantee is made by this interface about atomicity of this operation,
	  * although some subclasses do implement it.
	  */
	@throws[IllegalStateException]("if the Ref doesn't allow subsequent modifications.")
	@throws[NoSuchElementException]("if the Ref currently doesn't have a value.")
	@throws[UnsupportedOperationException]("if the value of this Ref can be set only once.")
	def ?=(newValue :T) :T = { val res = value; value = newValue; res }

	/** Assigns a new value to this variable providing the current value is equal to the expected value.
	  * The default implementation does it the direct way without any guarantees about multi-threaded semantics.
	  * This method is of real practical use only in concurrent `InOut` implementations such as
	  * [[net.noresttherein.sugar.vars.SyncVar SyncVar]], [[net.noresttherein.sugar.vars.Atomic Atomic]]
	  * and [[net.noresttherein.sugar.vars.Volatile Volatile]].
	  * @param expect   a value to compare with current value.
	  * @param newValue a new value for this variable.
	  * @return `true` if the previous value equaled `expect` and the variable has been set to `newValue`.
	  */
	@throws[UnsupportedOperationException]("if the value of this Ref can be set only once.")
	def testAndSet(expect :T, newValue :T) :Boolean =
		(value == expect) && { value = newValue; true }

	/** A ''test-and-set'' operation divided syntactically into two binary operators.
	  * `x :? expect := value` is equivalent to `x.testAndSet(expect, value)`.
	  * @param expect a value to compare with current value of this variable.
	  * @return an intermediate object which will perform the comparison and assign the value given to its
	  *         [[net.noresttherein.sugar.vars.InOut.TestAndSet.:=]] method.
	  * @see [[net.noresttherein.sugar.vars.InOut.testAndSet testAndSet()]]
	  */
	@inline final def :?(expect :T) :TestAndSet[T] = new TestAndSet(this, expect)


	/** Updates the value of this variable with the given function. Default implementation is equivalent to
	  * `val res = f(this.get); this.value = res; res` and has no benefit over direct application in client code.
	  * This method comes to use with concurrent `InOut` implementations such as
	  * [[net.noresttherein.sugar.vars.SyncVar SyncVar]] and [[net.noresttherein.sugar.vars.Volatile Volatile]] -
	  * the semantics of simple default [[net.noresttherein.sugar.vars.Var Var]] offers no guarantees in multi-threaded
	  * environments.
	  * @param f a function to apply to the value of this variable. Should have no side effects as it may be invoked
	  *          several times.
	  * @return the result of applying `f` to the current value.
	  */
	@throws[NoSuchElementException]("if the Ref currently doesn't have a value.")
	@throws[UnsupportedOperationException]("if the value of this Ref can be set only once.")
	def update(f :T => T) :T = { val res = f(value); value = res; res }

	/** Combines the value of this variable with a value of some other type, assigning the result of application
	  * back to this variable before returning it. It uses this variable as an accumulator, updated iteratively with
	  * new values in a way similar to an in place ''foldLeft'' operation on a singleton collection; the difference
	  * from `foldLeft` is that the function's result is the type of this variable, rather than the argument.
	  * Default implementation naively performs this directly without any guarantees about multi-threaded semantics
	  * and is equivalent to `val res = foldLeft(z, value); value = res; res`. Scala specific fold notation is
	  * chosen here to remind through associativity that this variable becomes the second (right) parameter
	  * of the folding function, with the argument on the left side of the operator being the first. This method comes
	  * to use with concurrent `InOut` implementations such as [[net.noresttherein.sugar.vars.SyncVar SyncVar]]
	  * or [[net.noresttherein.sugar.vars.Volatile Volatile]].
	  * @param z        an accumulator value to pass as the first argument to the `foldLeft` function, together
	  *                 with the current value of this variable.
	  * @param foldLeft a function applied to the argument and this variable whose result should be set
	  *                 to this variable.
	  * @return the result of applying `foldLeft` to the argument and this variable.
	  */
	@throws[NoSuchElementException]("if the Ref currently doesn't have a value.")
	@throws[UnsupportedOperationException]("if the value of this Ref can be set only once.")
	@inline final def /=:[@specialized(Args) A](z :A)(foldLeft :(A, T) => T) :T = updateLeft(z)(foldLeft)

	/** Combines the value of this variable with a value of some other type, assigning the result of application
	  * back to this variable before returning it. It uses this variable as an accumulator, updated iteratively with
	  * new values in a way similar to an in place ''foldRight'' operation on a singleton collection; the difference
	  * from `foldRight` is that the function's result is the type of this variable, rather than the argument.
	  * Default implementation naively performs this directly without any guarantees about multi-threaded semantics
	  * and is equivalent to `val res = foldRight(value, z); value = res; res`. Scala specific fold notation is
	  * chosen here to remind through associativity that this variable becomes the first (left) parameter of the folding
	  * function, with the argument on the right side of the operator being the second. This method comes to use with
	  * concurrent `InOut` implementations such as [[net.noresttherein.sugar.vars.SyncVar SyncVar]]
	  * or [[net.noresttherein.sugar.vars.Volatile Volatile]].
	  * @param z         an accumulator value to pass as the second argument to the `foldRight` function, together
	  *                  with the current value of this variable.
	  * @param foldRight a function applied to this variable and the argument, whose result should be set
	  *                  to this variable.
	  * @return the result of applying `foldLeft` to this variable and the argument.
	  */
	@throws[NoSuchElementException]("if the Ref currently doesn't have a value.")
	@throws[UnsupportedOperationException]("if the value of this Ref can be set only once.")
	@inline final def :\=[@specialized(Args) A](z :A)(foldRight :(T, A) => T) :T = updateRight(z)(foldRight)

	/** Combines the value of this variable with a value of some other type, assigning the result of application
	  * back to this variable before returning it. It uses this variable as an accumulator, updated iteratively with
	  * new values in a way similar to an in place ''foldLeft'' operation on a singleton collection; the difference
	  * from `foldLeft` is that the function's result is the type of this variable, rather than the argument.
	  * Default implementation naively performs this directly without any guarantees about multi-threaded semantics
	  * and is equivalent to `val res = f(z, value); value = res; res`. This method comes
	  * to use with concurrent `InOut` implementations such as [[net.noresttherein.sugar.vars.SpinVar SyncVar]]
	  * or [[net.noresttherein.sugar.vars.Volatile Volatile]].
	  * @param z accumulator value to pass as the first argument to the `f` function, together with the current
	  *          value of this variable.
	  * @param f a function applied to the argument and this variable, whose result should be set to this variable.
	  * @return the result of applying `f` to this variable and the argument.
	  */
	@throws[NoSuchElementException]("if the Ref currently doesn't have a value.")
	@throws[UnsupportedOperationException]("if the value of this Ref can be set only once.")
	def updateLeft[@specialized(Args) A](z :A)(f :(A, T) => T) :T = {
		val res = f(z, value); value = res; res
	}

	/** Combines the value of this variable with a value of some other type, assigning the result of application
	  * back to this variable before returning it. It uses this variable as an accumulator, updated iteratively with
	  * new values in a way similar to an in place ''foldRight'' operation on a singleton collection; the difference
	  * from `foldRight` is that the function's result is the type of this variable, rather than the argument.
	  * Default implementation naively performs this directly without any guarantees about multi-threaded semantics
	  * and is equivalent to `val res = f(value, z); value = res; res`. This method comes
	  * to use with concurrent `InOut` implementations such as [[net.noresttherein.sugar.vars.SyncVar SyncVar]]
	  * or [[net.noresttherein.sugar.vars.Volatile Volatile]].
	  * @param z accumulator value to pass as the second argument to the `f` function, together with the current
	  *          value of this variable.
	  * @param f a function applied to the this variable and the argument, whose result should be set to this variable.
	  * @return the result of applying `f` to this variable and the argument.
	  */
	@throws[NoSuchElementException]("if the Ref currently doesn't have a value.")
	@throws[UnsupportedOperationException]("if the value of this Ref can be set only once.")
	def updateRight[@specialized(Args) A](z :A)(f :(T, A) => T) :T = {
		val res = f(value, z); value = res; res
	}

	//todo: specialization
	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case v :InOut[_] if v canEqual this => (maybe, v.maybe) match {
			case (Yes(v1), Yes(v2)) => v1 == v2
			case _ => false
		}
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[InOut[_]]
	override def hashCode :Int = maybe.hashCode

}






/** Factory of boxed in/out method parameters.
  * @define Ref `InOut`
  * @define ref variable
  */
@SerialVersionUID(Ver)
case object InOut {
	final val SpecializedVars = new Specializable.Group(Byte, Short, Char, Int, Long, Float, Double, Boolean)

	/** Create a wrapper over a '''`var`''' of type `T` which can be passed as an in/out method parameter. */
	@inline def apply[@specialized(SpecializedVars) T](value :T) :InOut[T] = Var(value)

	/** Create a wrapper over a '''`var`''' of type `T` which can be passed as an in/out method parameter.*/
	@inline def apply[@specialized(SpecializedVars) T](implicit default :DefaultValue[T]) :InOut[T] =
		Var[T](default.get)



	/** An intermediate value of a ''test-and-set'' operation initiated by [[net.noresttherein.sugar.vars.InOut.:? :?]].
	  * Offers the actual setter method [[net.noresttherein.sugar.vars.InOut.TestAndSet.:= :=]].
	  */
	final class TestAndSet[@specialized(SpecializedVars) T] private[vars](x :InOut[T], expect :T) {
		/** If the current value of tested $ref equals the preceding value, assign to it the new value. */
		@throws[UnsupportedOperationException]("if the value of this Ref can be set only once.")
		@inline def :=(value :T) :Boolean = x.testAndSet(expect, value)
	}



	/** Unbox the value hold by an `InOut` wrapper. */
	@inline final implicit def unboxInOut[@specialized(SpecializedVars) T](variable :InOut[T]) :T = variable.value

	/** Extra implicits which might be helpful but can also lead to tricky bugs or cause conflicts. */
	@SerialVersionUID(Ver)
	object implicits {
//		/** Implicitly creates a `InOut` instance with a given value. This implicit is optional as the main use of `InOut[T]`
//		  * is to be used as in/out method parameters. In that scenario, using a value identifier instead of a `InOut[T]`
//		  * makes no sense and would likely be an error.
//		  */
//		@inline implicit def boxInOutParam[@specialized(SpecializedVars) T](value :T) :InOut[T] =   Var[T](value)

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
		class InOutMultiAssignment[X, V <: InOut[X]](private val value :X) extends AnyVal {

			/** Assign the right-hand value (this) to the left-hand $ref, returning this value.
			  * Allows C-like chained assignments: `x1 =: x2 =: x3 =: 0`.
			  * @param v $ref which should be assigned the new value
			  * @return assigned value
			  */
			@inline def =:(v :V) :X = { v := value; value }

			/** Assigns this (right-hand) value to multiple variables given on the left-hand side of the operator.
			  * `(x1, x2, x3) =: 0`. This is an arguably more readable alternative to chaining individual assignments,
			  * although slightly less efficient.
			  */
			@inline def =:(v1 :V, v2 :V, vs :V*) :X = {
				v1 := value; v2 := value
				vs match {
					case list :List[V @unchecked] =>
						var l :List[V] = list
						while (l.nonEmpty) {
							l.head.value = value; l = l.tail
						}
					case _ =>
						vs foreach { _ := value }
						//this would be faster on reference values, but does a lot of boxing on value types
//						vs.foldLeft(value) { (x, v) => v.value = x; x }
				}
				value
			}
		}

	}


	@SerialVersionUID(Ver)
	private[vars] class InOutOrdering[V[X] <: InOut[X], T](implicit content :Ordering[T]) extends Ordering[V[T]] {
		override def compare(x :V[T], y :V[T]) :Int = content.compare(x.get, y.get)
	}



/******************************** Implicits with type-specific arithmetic *********************************************/


	/** Implicit extension of `InOut[Boolean]` providing logical operations on the $ref.
	  * All these methods are polymorphic and guaranteed to have at least the same memory access semantics as
	  * [[net.noresttherein.sugar.vars.InOut.updateRight applyRight]]. So if, for example, this $ref is synchronized,
	  * then the whole operation will be `synchronized`. The $Ref subclass may however use a different (optimized)
	  * implementation, which might be observable if the argument expression is not idempotent.
	  * If you wish to avoid them and to invoke the appropriate methods statically with possible inlining,
	  * use the appropriate type of the variable ([[net.noresttherein.sugar.vars.Var Var]] or
	  * [[net.noresttherein.sugar.vars.SyncVar SyncVar]]).
	  */
	implicit class InOutBooleanLogic(private val x :InOut[Boolean]) extends AnyVal {
		/** Assigns this $ref its (eager) logical conjunction with the given argument: `x := x & other`.
		  * It should be preferred to `&&` whenever the argument is readily available (such as a `val` member), as
		  * the lazy alternative will not be inlined in most scenarios and require an actual function call.
		  * As the static type of this $ref is the generic `InOut[Boolean]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def &=(other :Boolean) :Unit =  x.updateRight(other)(_ & _)

		/** Assigns this $ref its (lazy) logical conjunction with the given argument: `x := x && other`.
		  * As the static type of this $ref is the generic `InOut[Boolean]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def &&=(other: =>Boolean) :Unit =
			//Race condition doesn't change the result, because it would set this to the same value as 'if'.
			if (x.value && !other)
				x.value = false


		/** Assigns this $ref its (eager) logical disjunction with the given argument: `x := x | other`.
		  * It should be preferred to `||` whenever the argument is readily available (such as a `val` member), as
		  * the lazy alternative will not be inlined in most scenarios and require an actual function call.
		  * As the static type of this $ref is the generic `InOut[Boolean]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def |=(other :Boolean) :Unit = x.updateRight(other)(_ | _)

		/** Assigns this $ref its (lazy) logical disjunction with the given argument: `x := x || other`.
		  * As the static type of this $ref is the generic `InOut[Boolean]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def ||=(other: =>Boolean) :Unit =
			if (!x.value && other)
				x.value = true


		/** Performs on this $ref logical ''xor'' with the given argument: `x := x ^ other`.
		  * As the static type of this $ref is the generic `InOut[Boolean]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def ^=(other :Boolean) :Unit = x.updateRight(other)(_ ^ _)

		/** Negates the value of this $ref: `x := !x`.
		  * As the static type of this $ref is the generic `InOut[Boolean]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  * @return the new (negated) value of this $ref
		  */
		@inline def neg() :Boolean = x.update(!_)

		/** Negates the value of this $ref: `x := !x`.
		  * As the static type of this $ref is the generic `InOut[Boolean]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def flip() :Unit = x.update(!_)


		/** Assigns `false` to this $ref and returns `true` ''iff'' it was `true` at the beginning of this call. */
		@inline def falsify() :Boolean = x.testAndSet(true, false)

		/** Assigns `true` to this $ref and returns `true` ''iff'' it was `false` at the beginning of this call. */
		@inline def flag() :Boolean = !x.testAndSet(false, true)
	}



	/** Implicit conversion of a `InOut[Int]` variable providing basic arithmetic and bitwise operations.
	  * All these methods are polymorphic and guaranteed to have the same memory access semantics as
	  * [[net.noresttherein.sugar.vars.InOut.updateRight applyRight]], although the implementation details may differ.
	  * So if, for example, this $ref is synchronized, then the whole operation will be `synchronized`.
	  * If you wish to bypass virtual calls and invoke the appropriate methods statically with possible inlining,
	  * use the appropriate type of the variable ([[net.noresttherein.sugar.vars.Var Var]] or
	  * [[net.noresttherein.sugar.vars.SyncVar SyncVar]]).
	  */
	implicit class InOutIntArithmetic(private val x :InOut[Int]) extends AnyVal {
		/** Increases the value of this $ref by the specified number.
		  * As the static type of this $ref is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def +=(n :Int) :Unit = x.updateRight(n)(_ + _)

		/** Decreases the value of this $ref by the specified number.
		  * As the static type of this $ref is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def -=(n :Int) :Unit = x.updateRight(n)(_ - _)

		/** Multiplies the value of this $ref by the specified number.
		  * As the static type of this $ref is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def *=(n :Int) :Unit = x.updateRight(n)(_ * _)

		/** Divides the value of this $ref by the specified number.
		  * As the static type of this $ref is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def /=(n :Int) :Unit = x.updateRight(n)(_ / _)

		/** Assigns to this $ref the remainder of division by the specified number.
		  * As the static type of this $ref is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def %=(n :Int) :Unit = x.updateRight(n)(_ % _)

		/** Increments this $ref by `1`, C-style.
		  * As the static type of this $ref is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@nowarn @inline def ++ :Unit = x.update(_ + 1)

		/** Decrements this $ref by `1`, C-style.
		  * As the static type of this $ref is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@nowarn @inline def -- :Unit = x.update(_ - 1)


		/** Increases the value of this $ref by the specified number, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def inc(n :Int) :Int = x.updateRight(n)(_ + _)

		/** Decreases the value of this $ref by the specified number, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def dec(n :Int) :Int = x.updateRight(n)(_ - _)

		/** Multiplies the value of this $ref by the specified number, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def mult(n :Int) :Int = x.updateRight(n)(_ * _)

		/** Divides the value of this $ref by the specified number, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def div(n :Int) :Int = x.updateRight(n)(_ / _)

		/** Assigns to this $ref the reminder of division by the specified number, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def rem(n :Int) :Int = x.updateRight(n)(_ % _)

		/** Increments this $ref by `1`, C-style, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def inc() :Int = x.update(_ + 1)

		/** Decrements this $ref by `1`, C-style, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def dec() :Int = x.update(_ - 1)

		/** Assigns this $ref its opposite value (`this := -this`), returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def neg() :Int = x.update(-_)


		/** Performs bitwise disjunction on this $ref with the given number.
		  * As the static type of this $ref is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def |=(n :Int) :Unit = x.updateRight(n)(_ | _)

		/** Performs bitwise conjunction on this $ref with the given number.
		  * As the static type of this $ref is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def &=(n :Int) :Unit = x.updateRight(n)(_ & _)

		/** Performs bitwise exclusive disjunction on this $ref with the given number.
		  * As the static type of this $ref is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def ^=(n :Int) :Unit = x.updateRight(n)(_ ^ _)

		/** Bit-shifts right the value of this $ref by the specified number of bits,
		  * replacing shifted higher bits with the sign bit. As the static type of this $ref is the generic `InOut[Int]`,
		  * this results in a polymorphic method call to enforce any additional contract or functionality
		  * possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def >>=(n :Int) :Unit = x.updateRight(n)(_ >> _)

		/** Bit-shifts right the value of this $ref by the specified number of bits,
		  * replacing shifted higher bits with zeros. As the static type of this $ref is the generic `InOut[Int]`,
		  * this results in a polymorphic method call to enforce any additional contract or functionality
		  * possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def >>>=(n :Int) :Unit = x.updateRight(n)(_ >>> _)

		/** Bit-shifts left the value of this $ref by the specified number of bits,
		  * replacing shifted higher bits with zeros. As the static type of this $ref is the generic `InOut[Int]`,
		  * this results in a polymorphic method call to enforce any additional contract or functionality
		  * possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def <<=(n :Int) :Unit = x.updateRight(n)(_ << _)

		/** Assigns this $ref its bitwise negation: `this := !this.value`.
		  * As the static type of this $ref is the generic `InOut[Int]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def flip() :Unit = x := x.update(~_)
	}






	/** Implicit conversion of a `InOut[Long]` variable providing basic arithmetic and bitwise operations.
	  * All these methods are polymorphic and guaranteed to have the same memory access semantics as
	  * [[net.noresttherein.sugar.vars.InOut.updateRight applyRight]], although the implementation details may differ.
	  * So if, for example, this $ref is synchronized, then the whole operation will be `synchronized`.
	  * If you wish to bypass virtual calls and invoke the appropriate methods statically with possible inlining,
	  * use the appropriate type of the variable ([[net.noresttherein.sugar.vars.Var Var]] or
	  * [[net.noresttherein.sugar.vars.SyncVar SyncVar]]).
	  */
	implicit class InOutLongArithmetic(private val x :InOut[Long]) extends AnyVal {
		/** Increases the value of this $ref by the specified number.
		  * As the static type of this $ref is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def +=(n :Long) :Unit = x.updateRight(n)(_ + _)

		/** Decreases the value of this $ref by the specified number.
		  * As the static type of this $ref is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def -=(n :Long) :Unit = x.updateRight(n)(_ - _)

		/** Multiplies the value of this $ref by the specified number.
		  * As the static type of this $ref is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def *=(n :Long) :Unit = x.updateRight(n)(_ * _)

		/** Divides the value of this $ref by the specified number.
		  * As the static type of this $ref is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def /=(n :Long) :Unit = x.updateRight(n)(_ / _)

		/** Assigns to this $ref the remainder of division by the specified number.
		  * As the static type of this $ref is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def %=(n :Long) :Unit = x.updateRight(n)(_ % _)

		/** Increments this $ref by `1`, C-style.
		  * As the static type of this $ref is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@nowarn @inline def ++ :Unit = x.update(_ + 1L)

		/** Decrements this $ref by `1`, C-style.
		  * As the static type of this $ref is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@nowarn @inline def -- :Unit = x.update(_ - 1L)


		/** Increases the value of this $ref by the specified number, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def inc(n :Long) :Long = x.updateRight(n)(_ + _)

		/** Decreases the value of this $ref by the specified number, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def dec(n :Long) :Long = x.updateRight(n)(_ - _)

		/** Multiplies the value of this $ref by the specified number, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def mult(n :Long) :Long = x.updateRight(n)(_ * _)

		/** Divides the value of this $ref by the specified number, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def div(n :Long) :Long = x.updateRight(n)(_ / _)

		/** Assigns to this $ref the reminder of division by the specified number, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def rem(n :Long) :Long = x.updateRight(n)(_ % _)

		/** Increments this $ref by `1`, C-style, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def inc() :Long = x.update(_ + 1L)

		/** Decrements this $ref by `1`, C-style, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def dec() :Long = x.update(_ - 1L)

		/** Assigns this $ref its opposite value (`this := -this`), returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def neg() :Long = x.update(-_)


		/** Performs bitwise disjunction on this $ref with the given number.
		  * As the static type of this $ref is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def |=(n :Long) :Unit = x.updateRight(n)(_ | _)

		/** Performs bitwise conjunction on this $ref with the given number.
		  * As the static type of this $ref is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def &=(n :Long) :Unit = x.updateRight(n)(_ & _)

		/** Performs bitwise exclusive disjunction on this $ref with the given number.
		  * As the static type of this $ref is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def ^=(n :Long) :Unit = x.updateRight(n)(_ ^ _)

		/** Bit-shifts right the value of this $ref by the specified number of bits,
		  * replacing shifted higher bits with the sign bit. As the static type of this $ref is the generic `InOut[Long]`,
		  * this results in a polymorphic method call to enforce any additional contract or functionality
		  * possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def >>=(n :Int) :Unit = x.updateRight(n)(_ >> _)

		/** Bit-shifts right the value of this $ref by the specified number of bits,
		  * replacing shifted higher bits with zeros. As the static type of this $ref is the generic `InOut[Long]`,
		  * this results in a polymorphic method call to enforce any additional contract or functionality
		  * possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def >>>=(n :Int) :Unit = x.updateRight(n)(_ >>> _)

		/** Bit-shifts left the value of this $ref by the specified number of bits,
		  * replacing shifted higher bits with zeros. As the static type of this $ref is the generic `InOut[Long]`,
		  * this results in a polymorphic method call to enforce any additional contract or functionality
		  * possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def <<=(n :Int) :Unit = x.updateRight(n)(_ << _)

		/** Assigns this $ref its bitwise negation: `this := !this.value`.
		  * As the static type of this $ref is the generic `InOut[Long]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def flip() :Unit = x := x.update(~_)
	}



	/** Implicit conversion of a `InOut[Float]` variable providing basic arithmetic and bitwise operations.
	  * All these methods are polymorphic and guaranteed to have the same memory access semantics as
	  * [[net.noresttherein.sugar.vars.InOut.updateRight applyRight]], although the implementation details may differ.
	  * So if, for example, this $ref is synchronized, then the whole operation will be `synchronized`.
	  * If you wish to bypass virtual calls and invoke the appropriate methods statically with possible inlining,
	  * use the appropriate type of the variable ([[net.noresttherein.sugar.vars.Var Var]] or
	  * [[net.noresttherein.sugar.vars.SyncVar SyncVar]]).
	  */
	implicit class InOutFloatArithmetic(private val x :InOut[Float]) extends AnyVal {
		/** Increases the value of this $ref by the specified number.
		  * As the static type of this $ref is the generic `InOut[Float]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def +=(n :Float) :Unit = x.updateRight(n)(_ + _)

		/** Decreases the value of this $ref by the specified number.
		  * As the static type of this $ref is the generic `InOut[Float]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def -=(n :Float) :Unit = x.updateRight(n)(_ - _)

		/** Multiplies the value of this $ref by the specified number.
		  * As the static type of this $ref is the generic `InOut[Float]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def *=(n :Float) :Unit = x.updateRight(n)(_ * _)

		/** Divides the value of this $ref by the specified number.
		  * As the static type of this $ref is the generic `InOut[Float]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def /=(n :Float) :Unit = x.updateRight(n)(_ / _)


		/** Increases the value of this $ref by the specified number, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Float]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def inc(n :Float) :Float = x.update(_ + 1f)

		/** Decreases the value of this $ref by the specified number, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Float]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def dec(n :Float) :Float = x.update(_ - 1f)

		/** Multiplies the value of this $ref by the specified number, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Float]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def mult(n :Float) :Float = x.updateRight(n)(_ * _)

		/** Divides the value of this $ref by the specified number, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Float]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def div(n :Float) :Float = x.updateRight(n)(_ / _)

		/** Assigns this $ref its opposite value (`this := -this`), returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Float]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def neg() :Float = x.update(-_)
	}



	/** Implicit conversion of a `InOut[Double]` variable providing basic arithmetic and bitwise operations.
	  * All these methods are polymorphic and guaranteed to have the same memory access semantics as
	  * [[net.noresttherein.sugar.vars.InOut.updateRight applyRight]], although the implementation details may differ.
	  * So if, for example, this $ref is synchronized, then the whole operation will be `synchronized`.
	  * If you wish to bypass virtual calls and invoke the appropriate methods statically with possible inlining,
	  * use the appropriate type of the variable ([[net.noresttherein.sugar.vars.Var Var]] or
	  * [[net.noresttherein.sugar.vars.SyncVar SyncVar]]).
	  */
	implicit class InOutDoubleArithmetic(private val x :InOut[Double]) extends AnyVal {
		/** Increases the value of this $ref by the specified number.
		  * As the static type of this $ref is the generic `InOut[Double]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def +=(n :Double) :Unit = x.updateRight(n)(_ + _)

		/** Decreases the value of this $ref by the specified number.
		  * As the static type of this $ref is the generic `InOut[Double]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def -=(n :Double) :Unit = x.updateRight(n)(_ - _)

		/** Multiplies the value of this $ref by the specified number.
		  * As the static type of this $ref is the generic `InOut[Double]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def *=(n :Double) :Unit = x.updateRight(n)(_ * _)

		/** Divides the value of this $ref by the specified number.
		  * As the static type of this $ref is the generic `InOut[Double]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def /=(n :Double) :Unit = x.updateRight(n)(_ / _)


		/** Increases the value of this $ref by the specified number, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Double]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def inc(n :Double) :Double = x.update(_ + 1.0)

		/** Decreases the value of this $ref by the specified number, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Double]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def dec(n :Double) :Double = x.update(_ - 1.0)

		/** Multiplies the value of this $ref by the specified number, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Double]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def mult(n :Double) :Double = x.updateRight(n)(_ * _)

		/** Divides the value of this $ref by the specified number, returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Double]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def div(n :Double) :Double = x.updateRight(n)(_ / _)

		/** Assigns this $ref its opposite value (`this := -this`), returning the updated value.
		  * As the static type of this $ref is the generic `InOut[Double]`, this results in a polymorphic method
		  * call to enforce any additional contract or functionality possibly provided by its actual dynamic type.
		  * In particular, if the underlying variable is atomic or synchronized, this operation will be, too.
		  */
		@inline def neg() :Double = x.update(-_)
	}






	/** Similarly to `=:=`, attests that `X` and `Y` can be safely cast from one to another, i.e. that they are
	  * the same type. This trait is specialized in order to enforce specialization of accepting methods, which
	  * wouldn't be the case with `=:=`.
	  */
	private[vars] trait TypeEquiv[@specialized(SpecializedVars) X, Y] extends Serializable {
		def apply[B[_]](param :B[X]) :B[Y]
	}

	@SerialVersionUID(Ver)
	private[vars] final class TypeIdent[@specialized(SpecializedVars) X] extends TypeEquiv[X, X] {
		override def apply[B[_]](param :B[X]) :B[X] = param

		override def equals(other :Any) :Boolean = other match {
			case ident :TypeIdent[_] => getClass == ident.getClass
			case _ => false
		}
	}

	private[vars] implicit val BoolEq   :TypeIdent[Boolean] = new TypeIdent[Boolean]
	private[vars] implicit val IntEq    :TypeIdent[Int]     = new TypeIdent[Int]
	private[vars] implicit val LongEq   :TypeIdent[Long]    = new TypeIdent[Long]
	private[vars] implicit val FloatEq  :TypeIdent[Float]   = new TypeIdent[Float]
	private[vars] implicit val DoubleEq :TypeIdent[Double]  = new TypeIdent[Double]
	private[vars] implicit val ShortEq  :TypeIdent[Short]   = new TypeIdent[Short]
	private[vars] implicit val ByteEq   :TypeIdent[Byte]    = new TypeIdent[Byte]
	private[vars] implicit val CharEq   :TypeIdent[Char]    = new TypeIdent[Char]
	private[vars] val AnyRefEq          :TypeIdent[AnyRef]  = new TypeIdent[AnyRef]

}






abstract class AbstractInOut[E] extends InOut[E] {
	override def isFinal = false
	override def isFinalizable = false
	override def isConst = false

	override def isDefined :Boolean = isDefinite

	override def value    :E = opt match {
		case One(res) => res
		case _        => noSuch_!(toString + ".value")
	}
	override def get      :E = value
	override def const    :Nothing = noSuch_!(toString + ".const")
	override def toOpt    :Opt[E] = opt
	override def constOpt :Opt[E] = None

	override def toString = opt match {
		case One(value) => String.valueOf(value)
		case _          => this.localClassName + "()"
	}
}




@SerialVersionUID(Ver)
private[sugar] class IndexedSeqLens[E](seq :mutable.IndexedSeqOps[E, Any1, _], idx :Int)
	extends AbstractInOut[E] with Serializable
{
	if (idx < 0 | idx > seq.length)
		outOfBounds_!("Index " + idx + " out of bounds for IndexedSeqLens(" + errorString(seq) + ")")

	override def isDefinite = idx >= 0 & idx < seq.length
	override def opt      :Opt[E] = if (idx >= 0 & idx < seq.length) One(seq(idx)) else None

	override def value_=(newValue :E) :Unit = seq(idx) = newValue
	override def value =
		try seq(idx) catch {
			case e :IndexOutOfBoundsException => //Possible in case of a buffer.
				noSuch_!("Index " + idx + " out of bounds for IndexedSeqLens(" + errorString(seq) + ")", e)
		}
	override def toString :String =
		if (idx >= 0 && idx < seq.length) String.valueOf(value)
		else "IndexedSeqLens(invalid: " + errorString(seq) + "(" + idx + "))"
}


@SerialVersionUID(Ver)
private[sugar] class ArrayLens[E](array :Array[E], idx :Int) extends AbstractInOut[E] with Serializable {
	if (idx < 0 | idx > array.length)
		outOfBounds_!("Index " + idx + " out of bounds for ArrayLens(" + errorString(array) + ")")

	override def isDefinite = true
	override def opt      :Opt[E] = One(array(idx))

	override def value_=(newValue :E) :Unit = array(idx) = newValue
	override def value = array(idx)

	override def toString :String = String.valueOf(array(idx))
}
