package net.noresttherein.slang.vars

import scala.Specializable.Args
import scala.annotation.nowarn

import net.noresttherein.slang.vars.InOut.{DefaultValue, SpecializedVars, TypeEquiv}




/** A boxed variable with synchronized access which can be used as in/out parameter to functions.
  * All synchronization uses the monitor associated with this instance, which client code can take advantage of
  * by creating wider critical sections with manual synchronization: `v.synchronized { ... }`.
  * @param x initial value of this variable.
  * @tparam T type of this variable
  * @author Marcin Mościcki marcin@moscicki.net
  */
@SerialVersionUID(1L)
sealed class SyncVar[@specialized(SpecializedVars) T](private[this] var x :T) extends InOut[T] with Serializable {

	@inline override def value :T = synchronized { x }

	@inline final override def value_=(value :T) :Unit = synchronized { x = value }

	@inline final override def ?=(value :T) :T = synchronized { val res = x; x = value; res }

	
	/** Assigns a new value to this variable providing the current value is equal to the expected value.
	  * @param expect value to compare with current value
	  * @param assign new value for this variable
	  * @return `true` if previous value equaled `expect` and the variable has been set to `assign`.
	  */
	final override def testAndSet(expect :T, assign :T) :Boolean = synchronized {
		(x == expect) && { x = assign; true }
	}


	/** Atomically updates the value of this variable with the given function. This is equivalent to 
	  * `this := f(this); this.get` with the guarantee that no other thread will modify the value of this variable
	  * between individual operations.
	  * @param f function to apply to the value of this variable. 
	  * @return result of applying `f` to the current value.
	  */
	final override def apply(f :T => T) :T = synchronized {
		val res = f(x); x = res; res
	}

	final override def applyLeft[@specialized(Args) A](z :A)(f :(A, T) => T) :T = synchronized {
		val res = f(z, x); x = res; res
	}

	final override def applyRight[@specialized(Args) A](z :A)(f :(T, A) => T) :T = synchronized {
		val res = f(x, z); x = res; res
	}


	//overriden to avoid creating a functional object closure
	private[vars] override def bool_&&=(other: => Boolean)(implicit ev :T TypeEquiv Boolean) :Unit = synchronized {
		val self = ev(this)
		self.value = self.value && other
	}

	private[vars] override def bool_||=(other: =>Boolean)(implicit ev :T TypeEquiv Boolean) :Unit = synchronized {
		val self = ev(this)
		self.value = self.value || other
	}

}






/** Factory of synchronized variables. Provides implicit conversions of greater precedence to make arithmetic reassignments synchronized.*/
object SyncVar {

	/** Create a wrapper over a '''`var`''' of type `T` which can be passed as an in/out method parameter. */
	@inline def apply[@specialized(SpecializedVars) T](value :T) :SyncVar[T] = new SyncVar[T](value)

	/** Create a wrapper over a '''`var`''' of type `T` which can be passed as an in/out method parameter. */
	@inline def apply[@specialized(SpecializedVars) T](implicit default :DefaultValue[T]) :SyncVar[T] =
		new SyncVar[T](default.value)

	



	/** Implicit conversion of `SyncVar[Boolean]` values providing logical operators.
	  * This type extension performs synchronization and all operations directly to facilitate hot spot inlining.
	  */
	implicit class BooleanSyncVarLogic(private val x :SyncVar[Boolean]) extends AnyVal {
		/** Atomically assigns this variable its (eager) logical conjunction with the given argument: `x := x & other`. */
		@inline def &=(other :Boolean) :Unit = x.synchronized { x := x.get & other }
		
		/** Atomically assigns this variable its logical conjunction with the given argument: `x := x && other`. */
		@inline def &&=(other: =>Boolean) :Unit = x.synchronized { x := x.get && other }

		/** Atomically assigns this variable its (eager) logical disjunction with the given argument: `x := x | other`. */
		@inline def |=(other :Boolean) :Unit = x.synchronized { x := x.get | other }

		/** Atomically assigns this variable its logical disjunction with the given argument: `x := x || other`. */
		@inline def ||=(other: =>Boolean) :Unit = x.synchronized { x := x.get || other }

		/** Atomically logically ''xor''s this variable with the given argument: `x := x ^ other`. */
		@inline def ^=(other :Boolean) :Unit = x.synchronized { x := x.get ^ other }

		/** Atomically negates this boolean variable, assigning it the opposite of the current value.
		  * @return the updated value of this variable (after negation).
		  */
		@inline def neg() :Boolean = x.synchronized { x := !x.get; x.get }

		/** Atomically negates this booleana variable, assigning it the opposite of the current value. */
		@inline def flip() :Unit = x.synchronized { x.value = !x.get }

		/** Assigns `false` to this variable and returns `true` ''iff'' it was `true` at the beginning of this call. */
		@inline def falsify() :Boolean = x.synchronized { val res = x.get; x := false; x }

		/** Assigns `true` to this variable and returns `true` ''iff'' it was `false` at the beginning of this call. */
		@inline def flag() :Boolean = x.synchronized { val res = !x.get; x := true; res }

	}







	/** Implicit conversion of `SyncVar[Int]` values providing arithmetic modifications.
	  * This type extension performs synchronization and all operations directly to facilitate hot spot inlining.
	  */
	implicit class IntSyncVarOps(private val x :SyncVar[Int]) extends AnyVal {
		/** Atomically increases this variable by the specified amount. */
		@inline def +=(n :Int) :Unit = x.synchronized { x := x.get + n }

		/** Atomically increases this variable by the specified amount, returning its updated value. */
		@inline def inc(n :Int) :Int = x.synchronized { val res = x.get + n; x := res; res }

		/** Atomically decreases this variable by the specified amount. */
		@inline def -=(n :Int) :Unit = x.synchronized { x := x.get - n }

		/** Atomically decreases this variable by the specified amount, returning its updated value. */
		@inline def dec(n :Int) :Int = x.synchronized { val res = x.get - n; x := res; res }

		/** Atomically multiplies this variable by the specified amount. */
		@inline def *=(n :Int) :Unit = x.synchronized { x := x.get * n }

		/** Atomically multiplies this variable by the specified amount, returning its updated value. */
		@inline def mult(n :Int) :Int = x.synchronized { val res = x.get * n; x := res; res }

		/** Atomically divides this variable by the specified amount. */
		@inline def /=(n :Int) :Unit = x.synchronized { x := x.get / n }

		/** Atomically divides this variable by the specified amount, returning its updated value. */
		@inline def div(n :Int) :Int = x.synchronized { val res = x.get / n; x := res; res }

		/** Atomically assigns this variable the rest from division by the specified amount. */
		@inline def %=(n :Int) :Unit = x.synchronized { x := x.get % n }

		/** Atomically assigns to this variable the reminder of dividing it by the specified amount and returns its updated value. */
		@inline def rem(n :Int) :Int = x.synchronized { val res = x.get % n; x := res; res }

		/** Atomically increments this variable by `1`, C-style. */
		@nowarn @inline def ++ :Unit = x.synchronized { x := x.get + 1 }

		/** Atomically increments this variable by `1`, returning the updated value. */
		@inline def inc() :Int = x.synchronized { val res = x.get + 1; x := res; res }

		/** Atomically decrements this variable by `1`, C-style. */
		@nowarn @inline def -- :Unit = x.synchronized { x := x.get - 1 }

		/** Atomically decrements this variable by `1`, returning the updated value. */
		@inline def dec() :Int = x.synchronized { val res = x.get - 1; x := res; res }

		/** Atomically sets this variable to its opposite value, returning the updated value (with new sign) */
		@inline def neg() :Int = x.synchronized { val res = -x.get; x := res; res }

		/** Atomically assigns this variable its bitwise disjunction with the specified value. */
		@inline def |=(n :Int) :Unit = x.synchronized { x := x.get | n }
		
		/** Atomically assigns this variable its bitwise conjunction with the specified value. */
		@inline def &=(n :Int) :Unit = x.synchronized { x := x.get & n }

		/** Atomically assigns this variable its bitwise ''xor'' with the specified value. */
		@inline def ^=(n :Int) :Unit = x.synchronized { x := x.get ^ n }

		/** Atomically bit-shifts this variable right by the specified number of bits. */
		@inline def >>=(n :Int) :Unit = x.synchronized { x := x.get >> n }
		
		/** Atomically bit-shifts this variable right by the specified number of bits, setting the freed higher bits to zero. */
		@inline def >>>=(n :Int) :Unit = x.synchronized { x := x.get >>> n }

		/** Atomically bit-shifts this variable left by the specified number of bits. */
		@inline def <<=(n :Int) :Unit = x.synchronized { x := x.get << n }

		/** Atomically assigns this variable its bitwise negation. */
		@inline def flip() :Unit = x.synchronized { x := ~x.get }

		/** Atomically tests if this variable equals zero and, if so, assigns it the given value
		  * @param ifZero new value for this variable
		  * @return `true` ''iff'' this variable changed in the effect of this call (its initial value was zero).
		  */
		@inline def testAndSet(ifZero :Int) :Boolean = x.testAndSet(0, ifZero)
	}








	/** Implicit conversion of `SyncVar[Long]` values providing arithmetic modifications.
	  * This type extension performs synchronization and all operations directly to facilitate hot spot inlining.
	  */
	implicit class LongSyncVarOps(private val x :SyncVar[Long]) extends AnyVal {
		/** Atomically increases this variable by the specified amount. */
		@inline def +=(n :Long) :Unit = x.synchronized { x := x.get + n }

		/** Atomically increases this variable by the specified amount, returning its updated value. */
		@inline def inc(n :Long) :Long = x.synchronized { val res = x.get + n; x := res; res }

		/** Atomically decreases this variable by the specified amount. */
		@inline def -=(n :Long) :Unit = x.synchronized { x := x.get - n }

		/** Atomically decreases this variable by the specified amount, returning its updated value. */
		@inline def dec(n :Long) :Long = x.synchronized { val res = x.get - n; x := res; res }

		/** Atomically multiplies this variable by the specified amount. */
		@inline def *=(n :Long) :Unit = x.synchronized { x := x.get * n }

		/** Atomically multiplies this variable by the specified amount, returning its updated value. */
		@inline def mult(n :Long) :Long = x.synchronized { val res = x.get * n; x := res; res }

		/** Atomically divides this variable by the specified amount. */
		@inline def /=(n :Long) :Unit = x.synchronized { x := x.get / n }

		/** Atomically divides this variable by the specified amount, returning its updated value. */
		@inline def div(n :Long) :Long = x.synchronized { val res = x.get / n; x := res; res }

		/** Atomically assigns this variable the rest from division by the specified amount. */
		@inline def %=(n :Long) :Unit = x.synchronized { x := x.get % n }

		/** Atomically assigns to this variable the reminder of dividing it by the specified amount and returns its updated value. */
		@inline def rem(n :Long) :Long = x.synchronized { val res = x.get % n; x := res; res }

		/** Atomically increments this variable by `1`, C-style. */
		@nowarn @inline def ++ :Unit = x := x.synchronized { x.get + 1 }

		/** Atomically increases this variable by `1`, returning its updated value. */
		@inline def inc() :Long = x.synchronized { val res = x.get + 1; x := res; res }

		/** Atomically decrements this variable by `1`, C-style. */
		@nowarn @inline def -- :Unit = x := x.synchronized { x.get - 1 }

		/** Atomically decreases this variable by `1`, returning its updated value. */
		@inline def dec() :Long = x.synchronized { val res = x.get - 1; x := res; res }

		/** Atomically sets this variable to its opposite value, returning the updated value (with new sign) */
		@inline def neg() :Long = x.synchronized { val res = -x.get; x := res; res }


		/** Atomically assigns this variable its bitwise disjunction with the specified value. */
		@inline def |=(n :Long) :Unit = x.synchronized { x := x.get | n }

		/** Atomically assigns this variable its bitwise conjunction with the specified value. */
		@inline def &=(n :Long) :Unit = x.synchronized { x := x.get & n }

		/** Atomically assigns this variable its bitwise ''xor'' with the specified value. */
		@inline def ^=(n :Long) :Unit = x.synchronized { x := x.get ^ n }

		/** Atomically bit-shifts this variable right by the specified number of bits. */
		@inline def >>=(n :Long) :Unit = x.synchronized { x := x.get >> n }

		/** Atomically bit-shifts this variable right by the specified number of bits, setting the freed higher bits to zero. */
		@inline def >>>=(n :Long) :Unit = x.synchronized { x := x.get >>> n }

		/** Atomically bit-shifts this variable left by the specified number of bits. */
		@inline def <<=(n :Long) :Unit = x.synchronized { x := x.get << n }

		/** Atomically assigns this variable its bitwise negation. */
		@inline def flip() :Unit = x.synchronized { x := ~x.get }

		/** Atomically tests if this variable equals zero and, if so, assigns it the given value
		  * @param ifZero new value for this variable
		  * @return `true` ''iff'' this variable changed in the effect of this call (its initial value was zero).
		  */
		@inline def testAndSet(ifZero :Long) :Boolean = x.testAndSet(0, ifZero)	
	
	}






	/** Implicit conversion of `SyncVar[Float]` values providing arithmetic modifications.
	  * This type extension performs synchronization and all operations directly to facilitate hot spot inlining.
	  */
	implicit class FloatSyncVarOps(private val x :SyncVar[Float]) extends AnyVal {
		/** Atomically increases this variable by the specified amount. */
		@inline def +=(n :Float) :Unit = x.synchronized { x := x.get + n }

		/** Atomically increases this variable by the specified amount, returning its updated value. */
		@inline def inc(n :Float) :Float = x.synchronized { val res = x.get + n; x := res; res }

		/** Atomically decreases this variable by the specified amount. */
		@inline def -=(n :Float) :Unit = x.synchronized { x := x.get - n }

		/** Atomically decreases this variable by the specified amount, returning its updated value. */
		@inline def dec(n :Float) :Float = x.synchronized { val res = x.get - n; x := res; res }

		/** Atomically multiplies this variable by the specified amount. */
		@inline def *=(n :Float) :Unit = x.synchronized { x := x.get * n }

		/** Atomically multiplies this variable by the specified amount, returning its updated value. */
		@inline def multi(n :Float) :Float = x.synchronized { val res = x.get * n; x := res; res }

		/** Atomically divides this variable by the specified amount. */
		@inline def /=(n :Float) :Unit = x.synchronized { x := x.get / n }

		/** Atomically divides this variable by the specified amount, returning its updated value. */
		@inline def div(n :Float) :Float = x.synchronized { val res = x.get / n; x := res; res }

		/** Atomically sets this variable to its opposite value, returning the updated value (with new sign) */
		@inline def neg() :Float = x.synchronized { val res = -x.get; x := res; res }


		/** Atomically tests if this variable equals zero and, if so, assigns it the given value
		  * @param ifZero new value for this variable
		  * @return `true` ''iff'' this variable changed in the effect of this call (its initial value was zero).
		  */
		@inline def testAndSet(ifZero :Float) :Boolean = x.testAndSet(0.0F, ifZero)
	}






	/** Implicit conversion of `SyncVar[Double]` values providing arithmetic modifications.
	  * This type extension performs synchronization and all operations directly to facilitate hot spot inlining.
	  */
	implicit class DoubleSyncVarOps(private val x :SyncVar[Double]) extends AnyVal {
		/** Atomically increases this variable by the specified amount. */
		@inline def +=(n :Double) :Unit = x.synchronized { x := x.get + n }

		/** Atomically increases this variable by the specified amount, returning its updated value. */
		@inline def inc(n :Double) :Double = x.synchronized { val res = x.get + n; x := res; res }

		/** Atomically decreases this variable by the specified amount. */
		@inline def -=(n :Double) :Unit = x.synchronized { x := x.get - n }

		/** Atomically decreases this variable by the specified amount, returning its updated value. */
		@inline def dec(n :Double) :Double = x.synchronized { val res = x.get - n; x := res; res }

		/** Atomically multiplies this variable by the specified amount. */
		@inline def *=(n :Double) :Unit = x.synchronized { x := x.get * n }

		/** Atomically multiplies this variable by the specified amount, returning its updated value. */
		@inline def multi(n :Double) :Double = x.synchronized { val res = x.get * n; x := res; res }

		/** Atomically divides this variable by the specified amount. */
		@inline def /=(n :Double) :Unit = x.synchronized { x := x.get / n }

		/** Atomically divides this variable by the specified amount, returning its updated value. */
		@inline def div(n :Double) :Double = x.synchronized { val res = x.get / n; x := res; res }

		/** Atomically sets this variable to its opposite value, returning the updated value (with new sign) */
		@inline def neg() :Double = x.synchronized { val res = -x.get; x := res; res }


		/** Atomically tests if this variable equals zero and, if so, assigns it the given value
		  * @param ifZero new value for this variable
		  * @return `true` ''iff'' this variable changed in the effect of this call (its initial value was zero).
		  */
		@inline def testAndSet(ifZero :Double) :Boolean = x.testAndSet(0.0, ifZero)
	}

}