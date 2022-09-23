package net.noresttherein.sugar.vars

import scala.Specializable.Args
import scala.annotation.nowarn

import net.noresttherein.sugar.vars.InOut.{SpecializedVars, TypeEquiv}
import net.noresttherein.sugar.witness.DefaultValue




/** A boxed variable with synchronized access which can be used as in/out parameter to functions.
  * All synchronization uses the monitor associated with this instance, which client code can take advantage of
  * by creating wider critical sections with manual synchronization: `v.synchronized { ... }`.
  * If custom critical sections as above are not required, then [[net.noresttherein.sugar.vars.Volatile Volatile]]
  * or [[net.noresttherein.sugar.vars.Atomic Atomic]] will be a better choice.
  * 
  * @tparam T the type of this variable
	* @define Ref `SyncVar`
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait SyncVar[@specialized(SpecializedVars) T]
//sealed class SyncVar[@specialized(SpecializedVars) T] private[vars] (private[this] var self :T)
	extends Mutable[T] with Serializable
{
	@inline final override def value :T = synchronized { unsync }
	@inline final override def value_=(newValue :T) :Unit = synchronized { unsync = newValue }
	private[vars] var unsync :T

	@inline final override def ?=(newValue :T) :T = synchronized { val res = unsync; unsync = newValue; res }

	/** Assigns a new value to this variable providing the current value is equal to the expected value.
	  * @param expect   a value to compare with current value.
	  * @param newValue a new value for this variable.
	  * @return `true` if previous value equaled `expect` and the variable has been set to `newValue`.
	  */
	final override def testAndSet(expect :T, newValue :T) :Boolean = synchronized {
		(unsync == expect) && { unsync = newValue; true }
	}


	/** Atomically updates the value of this variable with the given function. This is equivalent to 
	  * `this := f(this); this.get` with the guarantee that no other thread will modify the value of this variable
	  * between the individual operations.
	  * @param f function to apply to the value of this variable. 
	  * @return result of applying `f` to the current value.
	  */
	final override def apply(f :T => T) :T = synchronized {
		val res = f(unsync); unsync = res; res
	}

	final override def applyLeft[@specialized(Args) A](z :A)(f :(A, T) => T) :T = synchronized {
		val res = f(z, unsync); unsync = res; res
	}

	final override def applyRight[@specialized(Args) A](z :A)(f :(T, A) => T) :T = synchronized {
		val res = f(unsync, z); unsync = res; res
	}


	//overriden to avoid creating a functional object closure
	private[vars] override def bool_&&=(other: => Boolean)(implicit ev :T TypeEquiv Boolean) :Unit = synchronized {
		val self = ev(this)
		self.unsync = self.unsync && other
	}

	private[vars] override def bool_||=(other: => Boolean)(implicit ev :T TypeEquiv Boolean) :Unit = synchronized {
		val self = ev(this)
		self.unsync = self.unsync || other
	}


	private[vars] override def isSpecialized :Boolean = getClass != classOf[SyncVar[_]]
}






/** Factory of synchronized variables. Provides implicit extensions of greater precedence reducing syn 
  * to provide inlining o synchronized operations.
  */
object SyncVar {

	/** Create a wrapper over a '''`var`''' of type `T` which can be passed as an in/out method parameter. */
	@inline def apply[@specialized(SpecializedVars) T](value :T) :SyncVar[T] = new BasicSyncVar[T](value)

	/** Create a wrapper over a '''`var`''' of type `T` which can be passed as an in/out method parameter. */
	@inline def apply[@specialized(SpecializedVars) T](implicit default :DefaultValue[T]) :SyncVar[T] =
		new BasicSyncVar[T](default.get)

	
//	implicit def SyncVarOrdering[T :Ordering] :Ordering[SyncVar[T]] = new InOutOrdering[SyncVar, T]


	/** Implicit conversion of `SyncVar[Boolean]` values providing logical operators.
	  * This type extension performs synchronization and all operations directly to facilitate hot spot inlining.
	  */
	implicit class SyncVarBooleanLogic(private val self :SyncVar[Boolean]) extends AnyVal {
		/** Atomically assigns this variable its (eager) logical conjunction with the given argument: `this := this & other`. */
		@inline def &=(other :Boolean) :Unit = self.synchronized { self.unsync = self.unsync & other }
		
		/** Atomically assigns this variable its logical conjunction with the given argument: `this := this && other`. */
		@inline def &&=(other: =>Boolean) :Unit = self.synchronized { self.unsync = self.unsync && other }

		/** Atomically assigns this variable its (eager) logical disjunction with the given argument: `this := this | other`. */
		@inline def |=(other :Boolean) :Unit = self.synchronized { self.unsync = self.unsync | other }

		/** Atomically assigns this variable its logical disjunction with the given argument: `this := this || other`. */
		@inline def ||=(other: =>Boolean) :Unit = self.synchronized { self.unsync = self.unsync || other }

		/** Atomically logically ''xor''s this variable with the given argument: `this := this ^ other`. */
		@inline def ^=(other :Boolean) :Unit = self.synchronized { self.unsync = self.unsync ^ other }

		/** Atomically negates this boolean variable, assigning it the opposite of the current value.
		  * @return the updated value of this variable (after negation).
		  */
		@inline def neg() :Boolean = self.synchronized { val res = !self.unsync; self.unsync = !res; res }

		/** Atomically negates this booleana variable, assigning it the opposite of the current value. */
		@inline def flip() :Unit = self.synchronized { self.unsync = !self.unsync }

		/** Assigns `false` to this variable and returns `true` ''iff'' it was `true` at the beginning of this call. */
		@inline def falsify() :Boolean = self.synchronized { val res = self.unsync; self.unsync = false; res }

		/** Assigns `true` to this variable and returns `true` ''iff'' it was `false` at the beginning of this call. */
		@inline def flag() :Boolean = self.synchronized { val res = !self.unsync; self.unsync = true; res }
	}



	/** Implicit conversion of `SyncVar[Int]` values providing arithmetic modifications.
	  * This type extension performs synchronization and all operations directly to facilitate hot spot inlining.
	  */
	implicit class SyncVarIntArithmetic(private val self :SyncVar[Int]) extends AnyVal {
		/** Atomically increases this variable by the specified amount. */
		@inline def +=(n :Int) :Unit = self.synchronized { self.unsync = self.unsync + n }

		/** Atomically increases this variable by the specified amount, returning its updated value. */
		@inline def inc(n :Int) :Int = self.synchronized { val res = self.unsync + n; self.unsync = res; res }

		/** Atomically decreases this variable by the specified amount. */
		@inline def -=(n :Int) :Unit = self.synchronized { self.unsync = self.unsync - n }

		/** Atomically decreases this variable by the specified amount, returning its updated value. */
		@inline def dec(n :Int) :Int = self.synchronized { val res = self.unsync - n; self.unsync = res; res }

		/** Atomically multiplies this variable by the specified amount. */
		@inline def *=(n :Int) :Unit = self.synchronized { self.unsync = self.unsync * n }

		/** Atomically multiplies this variable by the specified amount, returning its updated value. */
		@inline def mult(n :Int) :Int = self.synchronized { val res = self.unsync * n; self.unsync = res; res }

		/** Atomically divides this variable by the specified amount. */
		@inline def /=(n :Int) :Unit = self.synchronized { self.unsync = self.unsync / n }

		/** Atomically increments this variable by `1`, C-style. */
		@nowarn @inline def ++ :Unit = self.synchronized { self.unsync = self.unsync + 1 }

		/** Atomically decrements this variable by `1`, C-style. */
		@nowarn @inline def -- :Unit = self.synchronized { self.unsync = self.unsync - 1 }


		/** Atomically divides this variable by the specified amount, returning its updated value. */
		@inline def div(n :Int) :Int = self.synchronized { val res = self.unsync / n; self.unsync = res; res }

		/** Atomically assigns this variable the rest from division by the specified amount. */
		@inline def %=(n :Int) :Unit = self.synchronized { self.unsync = self.unsync % n }

		/** Atomically assigns to this variable the reminder of dividing it by the specified amount and returns its updated value. */
		@inline def rem(n :Int) :Int = self.synchronized { val res = self.unsync % n; self.unsync = res; res }

		/** Atomically increments this variable by `1`, returning the updated value. */
		@inline def inc() :Int = self.synchronized { val res = self.unsync + 1; self.unsync = res; res }

		/** Atomically decrements this variable by `1`, returning the updated value. */
		@inline def dec() :Int = self.synchronized { val res = self.unsync - 1; self.unsync = res; res }

		/** Atomically sets this variable to its opposite value, returning the updated value (with new sign) */
		@inline def neg() :Int = self.synchronized { val res = -self.unsync; self.unsync = res; res }


		/** Atomically assigns this variable its bitwise disjunction with the specified value. */
		@inline def |=(n :Int) :Unit = self.synchronized { self.unsync = self.unsync | n }
		
		/** Atomically assigns this variable its bitwise conjunction with the specified value. */
		@inline def &=(n :Int) :Unit = self.synchronized { self.unsync = self.unsync & n }

		/** Atomically assigns this variable its bitwise ''xor'' with the specified value. */
		@inline def ^=(n :Int) :Unit = self.synchronized { self.unsync = self.unsync ^ n }

		/** Atomically bit-shifts this variable right by the specified number of bits. */
		@inline def >>=(n :Int) :Unit = self.synchronized { self.unsync = self.unsync >> n }
		
		/** Atomically bit-shifts this variable right by the specified number of bits, setting the freed higher bits to zero. */
		@inline def >>>=(n :Int) :Unit = self.synchronized { self.unsync = self.unsync >>> n }

		/** Atomically bit-shifts this variable left by the specified number of bits. */
		@inline def <<=(n :Int) :Unit = self.synchronized { self.unsync = self.unsync << n }

		/** Atomically assigns this variable its bitwise negation. */
		@inline def flip() :Unit = self.synchronized { self.unsync = ~self.unsync }


		/** Atomically tests if this variable equals zero and, if so, assigns it the given value
		  * @param ifZero new value for this variable
		  * @return `true` ''iff'' this variable changed in the effect of this call (its initial value was zero).
		  */
		@inline def testAndSet(ifZero :Int) :Boolean = self.testAndSet(0, ifZero)
	}



	/** Implicit conversion of `SyncVar[Long]` values providing arithmetic modifications.
	  * This type extension performs synchronization and all operations directly to facilitate hot spot inlining.
	  */
	implicit class SyncVarLongArithmetic(private val self :SyncVar[Long]) extends AnyVal {
		/** Atomically increases this variable by the specified amount. */
		@inline def +=(n :Long) :Unit = self.synchronized { self.unsync = self.unsync + n }

		/** Atomically increases this variable by the specified amount, returning its updated value. */
		@inline def inc(n :Long) :Long = self.synchronized { val res = self.unsync + n; self.unsync = res; res }

		/** Atomically decreases this variable by the specified amount. */
		@inline def -=(n :Long) :Unit = self.synchronized { self.unsync = self.unsync - n }

		/** Atomically decreases this variable by the specified amount, returning its updated value. */
		@inline def dec(n :Long) :Long = self.synchronized { val res = self.unsync - n; self.unsync = res; res }

		/** Atomically multiplies this variable by the specified amount. */
		@inline def *=(n :Long) :Unit = self.synchronized { self.unsync = self.unsync * n }

		/** Atomically multiplies this variable by the specified amount, returning its updated value. */
		@inline def mult(n :Long) :Long = self.synchronized { val res = self.unsync * n; self.unsync = res; res }

		/** Atomically divides this variable by the specified amount. */
		@inline def /=(n :Long) :Unit = self.synchronized { self.unsync = self.unsync / n }

		/** Atomically increments this variable by `1`, C-style. */
		@nowarn @inline def ++ :Unit = self.unsync = self.synchronized { self.unsync + 1 }

		/** Atomically decrements this variable by `1`, C-style. */
		@nowarn @inline def -- :Unit = self.unsync = self.synchronized { self.unsync - 1 }


		/** Atomically divides this variable by the specified amount, returning its updated value. */
		@inline def div(n :Long) :Long = self.synchronized { val res = self.unsync / n; self.unsync = res; res }

		/** Atomically assigns this variable the rest from division by the specified amount. */
		@inline def %=(n :Long) :Unit = self.synchronized { self.unsync = self.unsync % n }

		/** Atomically assigns to this variable the reminder of dividing it by the specified amount and returns its updated value. */
		@inline def rem(n :Long) :Long = self.synchronized { val res = self.unsync % n; self.unsync = res; res }

		/** Atomically increases this variable by `1`, returning its updated value. */
		@inline def inc() :Long = self.synchronized { val res = self.unsync + 1; self.unsync = res; res }

		/** Atomically decreases this variable by `1`, returning its updated value. */
		@inline def dec() :Long = self.synchronized { val res = self.unsync - 1; self.unsync = res; res }

		/** Atomically sets this variable to its opposite value, returning the updated value (with new sign) */
		@inline def neg() :Long = self.synchronized { val res = -self.unsync; self.unsync = res; res }


		/** Atomically assigns this variable its bitwise disjunction with the specified value. */
		@inline def |=(n :Long) :Unit = self.synchronized { self.unsync = self.unsync | n }

		/** Atomically assigns this variable its bitwise conjunction with the specified value. */
		@inline def &=(n :Long) :Unit = self.synchronized { self.unsync = self.unsync & n }

		/** Atomically assigns this variable its bitwise ''xor'' with the specified value. */
		@inline def ^=(n :Long) :Unit = self.synchronized { self.unsync = self.unsync ^ n }

		/** Atomically bit-shifts this variable right by the specified number of bits. */
		@inline def >>=(n :Long) :Unit = self.synchronized { self.unsync = self.unsync >> n }

		/** Atomically bit-shifts this variable right by the specified number of bits, setting the freed higher bits to zero. */
		@inline def >>>=(n :Long) :Unit = self.synchronized { self.unsync = self.unsync >>> n }

		/** Atomically bit-shifts this variable left by the specified number of bits. */
		@inline def <<=(n :Long) :Unit = self.synchronized { self.unsync = self.unsync << n }

		/** Atomically assigns this variable its bitwise negation. */
		@inline def flip() :Unit = self.synchronized { self.unsync = ~self.unsync }


		/** Atomically tests if this variable equals zero and, if so, assigns it the given value
		  * @param ifZero new value for this variable
		  * @return `true` ''iff'' this variable changed in the effect of this call (its initial value was zero).
		  */
		@inline def testAndSet(ifZero :Long) :Boolean = self.testAndSet(0, ifZero)	
	}



	/** Implicit conversion of `SyncVar[Float]` values providing arithmetic modifications.
	  * This type extension performs synchronization and all operations directly to facilitate hot spot inlining.
	  */
	implicit class SyncVarFloatArithmetic(private val self :SyncVar[Float]) extends AnyVal {
		/** Atomically increases this variable by the specified amount. */
		@inline def +=(n :Float) :Unit = self.synchronized { self.unsync = self.unsync + n }

		/** Atomically increases this variable by the specified amount, returning its updated value. */
		@inline def inc(n :Float) :Float = self.synchronized { val res = self.unsync + n; self.unsync = res; res }

		/** Atomically decreases this variable by the specified amount. */
		@inline def -=(n :Float) :Unit = self.synchronized { self.unsync = self.unsync - n }

		/** Atomically decreases this variable by the specified amount, returning its updated value. */
		@inline def dec(n :Float) :Float = self.synchronized { val res = self.unsync - n; self.unsync = res; res }

		/** Atomically multiplies this variable by the specified amount. */
		@inline def *=(n :Float) :Unit = self.synchronized { self.unsync = self.unsync * n }

		/** Atomically multiplies this variable by the specified amount, returning its updated value. */
		@inline def multi(n :Float) :Float = self.synchronized { val res = self.unsync * n; self.unsync = res; res }

		/** Atomically divides this variable by the specified amount. */
		@inline def /=(n :Float) :Unit = self.synchronized { self.unsync = self.unsync / n }

		/** Atomically divides this variable by the specified amount, returning its updated value. */
		@inline def div(n :Float) :Float = self.synchronized { val res = self.unsync / n; self.unsync = res; res }

		/** Atomically sets this variable to its opposite value, returning the updated value (with new sign) */
		@inline def neg() :Float = self.synchronized { val res = -self.unsync; self.unsync = res; res }


		/** Atomically tests if this variable equals zero and, if so, assigns it the given value
		  * @param ifZero new value for this variable
		  * @return `true` ''iff'' this variable changed in the effect of this call (its initial value was zero).
		  */
		@inline def testAndSet(ifZero :Float) :Boolean = self.testAndSet(0.0F, ifZero)
	}



	/** Implicit conversion of `SyncVar[Double]` values providing arithmetic modifications.
	  * This type extension performs synchronization and all operations directly to facilitate hot spot inlining.
	  */
	implicit class SyncVarDoubleArithmetic(private val self :SyncVar[Double]) extends AnyVal {
		/** Atomically increases this variable by the specified amount. */
		@inline def +=(n :Double) :Unit = self.synchronized { self.unsync = self.unsync + n }

		/** Atomically increases this variable by the specified amount, returning its updated value. */
		@inline def inc(n :Double) :Double = self.synchronized { val res = self.unsync + n; self.unsync = res; res }

		/** Atomically decreases this variable by the specified amount. */
		@inline def -=(n :Double) :Unit = self.synchronized { self.unsync = self.unsync - n }

		/** Atomically decreases this variable by the specified amount, returning its updated value. */
		@inline def dec(n :Double) :Double = self.synchronized { val res = self.unsync - n; self.unsync = res; res }

		/** Atomically multiplies this variable by the specified amount. */
		@inline def *=(n :Double) :Unit = self.synchronized { self.unsync = self.unsync * n }

		/** Atomically multiplies this variable by the specified amount, returning its updated value. */
		@inline def multi(n :Double) :Double = self.synchronized { val res = self.unsync * n; self.unsync = res; res }

		/** Atomically divides this variable by the specified amount. */
		@inline def /=(n :Double) :Unit = self.synchronized { self.unsync = self.unsync / n }

		/** Atomically divides this variable by the specified amount, returning its updated value. */
		@inline def div(n :Double) :Double = self.synchronized { val res = self.unsync / n; self.unsync = res; res }

		/** Atomically sets this variable to its opposite value, returning the updated value (with new sign) */
		@inline def neg() :Double = self.synchronized { val res = -self.unsync; self.unsync = res; res }


		/** Atomically tests if this variable equals zero and, if so, assigns it the given value
		  * @param ifZero new value for this variable
		  * @return `true` ''iff'' this variable changed in the effect of this call (its initial value was zero).
		  */
		@inline def testAndSet(ifZero :Double) :Boolean = self.testAndSet(0.0, ifZero)
	}




	@SerialVersionUID(1L)
	private class BasicSyncVar[@specialized(SpecializedVars) T](override var unsync :T)
		extends SyncVar[T] with Serializable
	{
		override def mkString = mkString("SyncVar")
	}
}