package net.noresttherein.slang

import net.noresttherein.slang.SyncVar.TestAndSet
import net.noresttherein.slang.Var.SpecializedTypes

/** A boxed variable with synchronized access which can be used as in/out parameter to functions.
  * All synchronization uses the monitor associated with this instance, which client code can take advantage of
  * by creating wider critical sections with manual synchronization: `v.synchronized { ... }`.
  * @param x initial value of this variable.
  * @tparam T type of this variable
  * @author Marcin Mościcki marcin@moscicki.net
  */
sealed class SyncVar[@specialized(SpecializedTypes) T](private[this] var x :T) extends InOut[T] with Serializable {

	@inline final override def get :T = synchronized { x }

	@inline final override def value :T = synchronized { x }

	@inline final override def value_=(value :T) :Unit = synchronized { x = value }

	@inline final override def :=(value :T) :Unit = synchronized { x = value }

	@inline final override def ?=(value :T) :T = synchronized { val res = x; x = value; res }

	/** Assigns a new value to this variable providing the current value is equal to the expected value.
	  * @param expect value to compare with current value
	  * @param assign new value for this variable
	  * @return `true` if previous value equaled `expect` and the variable has been set to `assign`.
	  */
	@inline def testAndSet(expect :T, assign :T) :Boolean = synchronized {
		(x == expect) && { x = assign; true }
	}


	/** A ''test-and-set'' operation divided syntactically into two binary operators.
	  * `x :? expect := value` is equivalent to `x.testAndSet(expect, value)`.
	  * @param expect value to compare with current value of this variable
	  * @return an intermediate object which will perform the comparison and assign the value given to its
	  *         [[net.noresttherein.slang.SyncVar.TestAndSet#:=]] method.
	  */
	@inline def :?(expect :T) :TestAndSet[T] = new TestAndSet(this, expect)
}



/** Factory of synchronized variables. Provides implicit conversions of greater precedence to make arithmetic reassignments synchronized.*/
object SyncVar {

	/** Create a wrapper over a '''`var`''' of type `T` which can be passed as an in/out method parameter. */
	@inline def apply[@specialized(SpecializedTypes) T](value :T) :SyncVar[T] = new SyncVar[T](value)


	/** An intermediate value of a ''test-and-set'' operation initiated by [[net.noresttherein.slang.SyncVar#:?]]. */
	final class TestAndSet[@specialized(SpecializedTypes) T](x :SyncVar[T], expect :T) {
		/** If the current value of tested variable equals the preceding value, assign to it the new value. */
		@inline  def :=(value :T) :Boolean = x.testAndSet(expect, value)
	}


	/** Implicit conversion of `SyncVar[Boolean]` values providing logical operators. */
	implicit class BooleanVarLogic(private val x :SyncVar[Boolean]) extends AnyVal {
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

		/** Negates this boolean variable, assigning it the opposite of the current value. */
		@inline def neg() :Unit = x.synchronized { x := !x.get }


		/** Assigns `false` to this variable and returns `true` ''iff'' it was `true` at the beginning of this call. */
		@inline def falsify() :Boolean = x.synchronized { val res = x.get; x := false; x }

		/** Assigns `true` to this variable and returns `true` ''iff'' it was `false` at the beginning of this call. */
		@inline def flag() :Boolean = x.synchronized { val res = !x.get; x := true; res }

	}


	/** Implicit conversion of `SyncVar[Int]` values providing arithmetic modifications. */
	implicit class IntVarOps(private val x :SyncVar[Int]) extends AnyVal {
		/** Atomically increases this variable by the specified amount. */
		@inline def +=(n :Int) :Unit = x.synchronized { x := x.get + n }
		
		/** Atomically decreases this variable by the specified amount. */
		@inline def -=(n :Int) :Unit = x.synchronized { x := x.get - n }

		/** Atomically multiplies this variable by the specified amount. */
		@inline def *=(n :Int) :Unit = x.synchronized { x := x.get * n }

		/** Atomically divides this variable by the specified amount. */
		@inline def /=(n :Int) :Unit = x.synchronized { x := x.get / n }

		/** Atomically assigns this variable the rest from division by the specified amount. */
		@inline def %=(n :Int) :Unit = x.synchronized { x := x.get % n }

		/** Atomically increments this variable by `1`, C-style. */
		@inline def ++ :Unit = x := x.synchronized { x.get + 1 }

		/** Atomically decrements this variable by `1`, C-style. */
		@inline def -- :Unit = x := x.synchronized { x.get - 1 }

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


	/** Implicit conversion of `SyncVar[Long]` values providing arithmetic modifications. */
	implicit class LongVarOps(private val x :SyncVar[Long]) extends AnyVal {
		/** Atomically increases this variable by the specified amount. */
		@inline def +=(n :Long) :Unit = x.synchronized { x := x.get + n }

		/** Atomically decreases this variable by the specified amount. */
		@inline def -=(n :Long) :Unit = x.synchronized { x := x.get - n }

		/** Atomically multiplies this variable by the specified amount. */
		@inline def *=(n :Long) :Unit = x.synchronized { x := x.get * n }

		/** Atomically divides this variable by the specified amount. */
		@inline def /=(n :Long) :Unit = x.synchronized { x := x.get / n }

		/** Atomically assigns this variable the rest from division by the specified amount. */
		@inline def %=(n :Long) :Unit = x.synchronized { x := x.get % n }

		/** Atomically increments this variable by `1`, C-style. */
		@inline def ++ :Unit = x := x.synchronized { x.get + 1 }

		/** Atomically decrements this variable by `1`, C-style. */
		@inline def -- :Unit = x := x.synchronized { x.get - 1 }

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


	/** Implicit conversion of `SyncVar[Float]` values providing arithmetic modifications. */
	implicit class FloatVarOps(private val x :SyncVar[Float]) extends AnyVal {
		/** Atomically increases this variable by the specified amount. */
		@inline def +=(n :Float) :Unit = x.synchronized { x := x.get + n }

		/** Atomically decreases this variable by the specified amount. */
		@inline def -=(n :Float) :Unit = x.synchronized { x := x.get - n }

		/** Atomically multiplies this variable by the specified amount. */
		@inline def *=(n :Float) :Unit = x.synchronized { x := x.get * n }

		/** Atomically divides this variable by the specified amount. */
		@inline def /=(n :Float) :Unit = x.synchronized { x := x.get / n }

		/** Atomically tests if this variable equals zero and, if so, assigns it the given value
		  * @param ifZero new value for this variable
		  * @return `true` ''iff'' this variable changed in the effect of this call (its initial value was zero).
		  */
		@inline def testAndSet(ifZero :Float) :Boolean = x.testAndSet(0.0F, ifZero)
	}


	/** Implicit conversion of `SyncVar[Double]` values providing arithmetic modifications. */
	implicit class DoubleVarOps(private val x :SyncVar[Double]) extends AnyVal {
		/** Atomically increases this variable by the specified amount. */
		@inline def +=(n :Double) :Unit = x.synchronized { x := x.get + n }

		/** Atomically decreases this variable by the specified amount. */
		@inline def -=(n :Double) :Unit = x.synchronized { x := x.get - n }

		/** Atomically multiplies this variable by the specified amount. */
		@inline def *=(n :Double) :Unit = x.synchronized { x := x.get * n }

		/** Atomically divides this variable by the specified amount. */
		@inline def /=(n :Double) :Unit = x.synchronized { x := x.get / n }

		/** Atomically tests if this variable equals zero and, if so, assigns it the given value
		  * @param ifZero new value for this variable
		  * @return `true` ''iff'' this variable changed in the effect of this call (its initial value was zero).
		  */
		@inline def testAndSet(ifZero :Double) :Boolean = x.testAndSet(0.0, ifZero)
	}

}