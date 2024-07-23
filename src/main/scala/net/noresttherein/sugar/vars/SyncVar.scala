package net.noresttherein.sugar.vars

import scala.Specializable.Args
import scala.annotation.nowarn

import net.noresttherein.sugar.collections.IndexedIterable.ApplyPreferred
import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.witness.{DefaultValue, ReferentialOrdering}




/** A boxed variable with synchronized access which can be used as in/out parameter to functions.
  * All synchronization uses the monitor associated with this instance, which client code can take advantage of
  * by creating wider critical sections with manual synchronization: `v.synchronized { ... }`.
  * If custom critical sections as above are not required, then [[net.noresttherein.sugar.vars.Volatile Volatile]]
  * or [[net.noresttherein.sugar.vars.Atomic Atomic]] will be a better choice.
  * 
  * @tparam T the type of this variable
  * @define Ref `SyncVar`
  * @define ref synchronized variable
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait SyncVar[@specialized(SpecializedVars) T] extends AtomicOps.AtomicVar[T] with Mutable[T] with Serializable {
	protected override def factory :AtomicOps[SyncVar] = SyncVar

	@inline final override def value :T = synchronized { unsafe }
	@inline final override def value_=(newValue :T) :Unit = synchronized { unsafe = newValue }

	/** Unsynchronized access to the variable field. */
	private[vars] var unsafe :T

	/** Atomically assigns a new value to this $ref, returning the current value. */
	@inline final override def ?=(newValue :T) :T = synchronized { val res = unsafe; unsafe = newValue; res }

	/** Assigns a new value to this variable providing the current value is equal to the expected value.
	  * @param expect   a value to compare with current value.
	  * @param newValue a new value for this variable.
	  * @return `true` if previous value equaled `expect` and the variable has been set to `newValue`.
	  */
	final override def testAndSet(expect :T, newValue :T) :Boolean = synchronized {
		(unsafe == expect) && { unsafe = newValue; true }
	}


	/** Atomically updates the value of this variable with the given function. This is equivalent to
	  * `this := f(this); this.get` with the guarantee that no other thread will modify the value of this variable
	  * between the individual operations.
	  * @param f function to apply to the value of this variable.
	  * @return result of applying `f` to the current value.
	  */
	final override def update(f :T => T) :T = synchronized {
		val res = f(unsafe); unsafe = res; res
	}

	final override def updateLeft[@specialized(Args) A](z :A)(f :(A, T) => T) :T = synchronized {
		val res = f(z, unsafe); unsafe = res; res
	}

	final override def updateRight[@specialized(Args) A](z :A)(f :(T, A) => T) :T = synchronized {
		val res = f(unsafe, z); unsafe = res; res
	}

	/** Applies given function to the value of this $ref, while holding the lock for this variable.
	  * This can be used to acquire monitors of several variables, recursively, or using ''for comprehension'' syntax:
	  * {{{
	  *     val sync1 = SyncVar(1)
	  *     val sync2 = SyncVar(2)
	  *     for { x <- sync1; y <- sync2 } yield x + y
	  * }}}
	  */
	def flatMap[O](f :T => O) :O = synchronized(f(unsafe))

	/** Applies given function to the value of this $ref, while holding the lock for this variable.
	  * This can be used to acquire monitors of several variables, recursively, or using ''for comprehension'' syn tax
	  * @see [[net.noresttherein.sugar.vars.SyncVar.flatMap flatMap]]
	  */
	def map[O](f :T => O) :O = synchronized(f(unsafe))

	/** Applies given function to the value of this $ref, while holding the lock for this variable.
	  * This can be used to acquire monitors of several variables, recursively, or using ''for comprehension'' syntax:
	  * {{{
	  *     val sync1 = SyncVar(1)
	  *     val sync2 = SyncVar(2)
	  *     for { x <- sync1; y <- sync2 } {
	  *         sync1 := y
	  *         sync2 := y
	  *     }
	  * }}}
	  */
	@inline final def foreach[U](f :T => U) :Unit = flatMap(f)

	private[vars] override def isSpecialized :Boolean = getClass != classOf[SyncVar[_]]
}






/** Factory of synchronized variables. Provides implicit extensions of greater precedence reducing syntax required
  * to provide inlining of synchronized operations.
  */
@SerialVersionUID(Ver)
case object SyncVar extends AtomicOps[SyncVar] {

	/** Create a wrapper over a '''`var`''' of type `T` which can be passed as an in/out method parameter. */
	def apply[@specialized(SpecializedVars) T](value :T) :SyncVar[T] = {
		val res = new Plain[T]
		//This is a synchronized call, but otherwise we would not have guarantee that all threads see it properly initialized.
		res.value = value
		res
	}

	/** Create a wrapper over a '''`var`''' of type `T` which can be passed as an in/out method parameter. */
	def apply[@specialized(SpecializedVars) T](implicit default :DefaultValue[T]) :SyncVar[T] = {
		val res = new Plain[T]
		res.value = default.get
		res
	}

	protected override def getAndSet[@specialized(SpecializedVars) T](v :AtomicOps.AtomicVar[T], newValue :T) :T =
		v.synchronized {
			val res = v.value; v.value = newValue; res
		}
	protected override def testAndSet[@specialized(SpecializedVars) T]
	                                 (v :AtomicOps.AtomicVar[T], expect :T, newValue :T) :Boolean =
		v.synchronized {
			v.value == expect && { v.value = newValue; true }
		}

	protected override def weakTestAndSet[@specialized(SpecializedVars) T]
	                                     (v :AtomicOps.AtomicVar[T], expect :T, newValue :T) :Boolean =
		testAndSet(v, expect, newValue)

	protected override def weakTestAndSetBool(v :AtomicOps.AtomicVar[Boolean], expect :Boolean, newValue :Boolean)
			:Boolean =
		testAndSet(v, expect, newValue)

	protected override def repeatTestAndSetBool(v :AtomicOps.AtomicVar[Boolean], expect :Boolean,
	                                            ifExpected :Boolean, ifNotExpected :Boolean) :Boolean =
		v.synchronized {
			if (v.value == expect) {
				v.value = ifExpected
				ifExpected
			} else {
				v.value = ifNotExpected
				ifNotExpected
			}
		}



	/** Implicit conversion of `SyncVar[Boolean]` values providing logical operators.
	  * This type extension performs synchronization and all operations directly to facilitate hot spot inlining.
	  */
	implicit class SyncVarBooleanLogic(private val self :SyncVar[Boolean]) extends AnyVal {
		/** Atomically assigns this variable its (eager) logical conjunction with the given argument: `this := this & other`. */
		@inline def &=(other :Boolean) :Unit = self.synchronized { self.unsafe = self.unsafe & other }

		/** Atomically assigns this variable its logical conjunction with the given argument: `this := this && other`. */
		@inline def &&=(other: =>Boolean) :Unit = self.synchronized { self.unsafe = self.unsafe && other }

		/** Atomically assigns this variable its (eager) logical disjunction with the given argument: `this := this | other`. */
		@inline def |=(other :Boolean) :Unit = self.synchronized { self.unsafe = self.unsafe | other }

		/** Atomically assigns this variable its logical disjunction with the given argument: `this := this || other`. */
		@inline def ||=(other: =>Boolean) :Unit = self.synchronized { self.unsafe = self.unsafe || other }

		/** Atomically logically ''xor''s this variable with the given argument: `this := this ^ other`. */
		@inline def ^=(other :Boolean) :Unit = self.synchronized { self.unsafe = self.unsafe ^ other }

		/** Atomically negates this boolean variable, assigning it the opposite of the current value.
		  * @return the updated value of this variable (after negation).
		  */
		@inline def neg() :Boolean = self.synchronized { val res = !self.unsafe; self.unsafe = res; res }

		/** Atomically negates this booleana variable, assigning it the opposite of the current value. */
		@inline def flip() :Unit = self.synchronized { self.unsafe = !self.unsafe }

		/** Assigns `false` to this variable and returns `true` ''iff'' it was `true` at the beginning of this call. */
		@inline def falsify() :Boolean = self.synchronized { val res = self.unsafe; self.unsafe = false; res }

		/** Assigns `true` to this variable and returns `true` ''iff'' it was `false` at the beginning of this call. */
		@inline def flag() :Boolean = self.synchronized { val res = !self.unsafe; self.unsafe = true; res }
	}



	/** Implicit conversion of `SyncVar[Int]` values providing arithmetic modifications.
	  * This type extension performs synchronization and all operations directly to facilitate hot spot inlining.
	  */
	implicit class SyncVarIntArithmetic(private val self :SyncVar[Int]) extends AnyVal {
		/** Atomically increases this variable by the specified amount. */
		@inline def +=(n :Int) :Unit = self.synchronized { self.unsafe = self.unsafe + n }

		/** Atomically increases this variable by the specified amount, returning its updated value. */
		@inline def inc(n :Int) :Int = self.synchronized { val res = self.unsafe + n; self.unsafe = res; res }

		/** Atomically decreases this variable by the specified amount. */
		@inline def -=(n :Int) :Unit = self.synchronized { self.unsafe = self.unsafe - n }

		/** Atomically decreases this variable by the specified amount, returning its updated value. */
		@inline def dec(n :Int) :Int = self.synchronized { val res = self.unsafe - n; self.unsafe = res; res }

		/** Atomically multiplies this variable by the specified amount. */
		@inline def *=(n :Int) :Unit = self.synchronized { self.unsafe = self.unsafe * n }

		/** Atomically multiplies this variable by the specified amount, returning its updated value. */
		@inline def mult(n :Int) :Int = self.synchronized { val res = self.unsafe * n; self.unsafe = res; res }

		/** Atomically divides this variable by the specified amount. */
		@inline def /=(n :Int) :Unit = self.synchronized { self.unsafe = self.unsafe / n }

		/** Atomically increments this variable by `1`, C-style. */
		@nowarn @inline def ++ :Unit = self.synchronized { self.unsafe = self.unsafe + 1 }

		/** Atomically decrements this variable by `1`, C-style. */
		@nowarn @inline def -- :Unit = self.synchronized { self.unsafe = self.unsafe - 1 }


		/** Atomically divides this variable by the specified amount, returning its updated value. */
		@inline def div(n :Int) :Int = self.synchronized { val res = self.unsafe / n; self.unsafe = res; res }

		/** Atomically assigns this variable the rest from division by the specified amount. */
		@inline def %=(n :Int) :Unit = self.synchronized { self.unsafe = self.unsafe % n }

		/** Atomically assigns to this variable the reminder of dividing it by the specified amount and returns its updated value. */
		@inline def rem(n :Int) :Int = self.synchronized { val res = self.unsafe % n; self.unsafe = res; res }

		/** Atomically increments this variable by `1`, returning the updated value. */
		@inline def inc() :Int = self.synchronized { val res = self.unsafe + 1; self.unsafe = res; res }

		/** Atomically decrements this variable by `1`, returning the updated value. */
		@inline def dec() :Int = self.synchronized { val res = self.unsafe - 1; self.unsafe = res; res }

		/** Atomically sets this variable to its opposite value, returning the updated value (with new sign) */
		@inline def neg() :Int = self.synchronized { val res = -self.unsafe; self.unsafe = res; res }


		/** Atomically assigns this variable its bitwise disjunction with the specified value. */
		@inline def |=(n :Int) :Unit = self.synchronized { self.unsafe = self.unsafe | n }

		/** Atomically assigns this variable its bitwise conjunction with the specified value. */
		@inline def &=(n :Int) :Unit = self.synchronized { self.unsafe = self.unsafe & n }

		/** Atomically assigns this variable its bitwise ''xor'' with the specified value. */
		@inline def ^=(n :Int) :Unit = self.synchronized { self.unsafe = self.unsafe ^ n }

		/** Atomically bit-shifts this variable right by the specified number of bits. */
		@inline def >>=(n :Int) :Unit = self.synchronized { self.unsafe = self.unsafe >> n }

		/** Atomically bit-shifts this variable right by the specified number of bits, setting the freed higher bits to zero. */
		@inline def >>>=(n :Int) :Unit = self.synchronized { self.unsafe = self.unsafe >>> n }

		/** Atomically bit-shifts this variable left by the specified number of bits. */
		@inline def <<=(n :Int) :Unit = self.synchronized { self.unsafe = self.unsafe << n }

		/** Atomically assigns this variable its bitwise negation. */
		@inline def flip() :Unit = self.synchronized { self.unsafe = ~self.unsafe }


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
		@inline def +=(n :Long) :Unit = self.synchronized { self.unsafe = self.unsafe + n }

		/** Atomically increases this variable by the specified amount, returning its updated value. */
		@inline def inc(n :Long) :Long = self.synchronized { val res = self.unsafe + n; self.unsafe = res; res }

		/** Atomically decreases this variable by the specified amount. */
		@inline def -=(n :Long) :Unit = self.synchronized { self.unsafe = self.unsafe - n }

		/** Atomically decreases this variable by the specified amount, returning its updated value. */
		@inline def dec(n :Long) :Long = self.synchronized { val res = self.unsafe - n; self.unsafe = res; res }

		/** Atomically multiplies this variable by the specified amount. */
		@inline def *=(n :Long) :Unit = self.synchronized { self.unsafe = self.unsafe * n }

		/** Atomically multiplies this variable by the specified amount, returning its updated value. */
		@inline def mult(n :Long) :Long = self.synchronized { val res = self.unsafe * n; self.unsafe = res; res }

		/** Atomically divides this variable by the specified amount. */
		@inline def /=(n :Long) :Unit = self.synchronized { self.unsafe = self.unsafe / n }

		/** Atomically increments this variable by `1`, C-style. */
		@nowarn @inline def ++ :Unit = self.unsafe = self.synchronized { self.unsafe + 1 }

		/** Atomically decrements this variable by `1`, C-style. */
		@nowarn @inline def -- :Unit = self.unsafe = self.synchronized { self.unsafe - 1 }


		/** Atomically divides this variable by the specified amount, returning its updated value. */
		@inline def div(n :Long) :Long = self.synchronized { val res = self.unsafe / n; self.unsafe = res; res }

		/** Atomically assigns this variable the rest from division by the specified amount. */
		@inline def %=(n :Long) :Unit = self.synchronized { self.unsafe = self.unsafe % n }

		/** Atomically assigns to this variable the reminder of dividing it by the specified amount and returns its updated value. */
		@inline def rem(n :Long) :Long = self.synchronized { val res = self.unsafe % n; self.unsafe = res; res }

		/** Atomically increases this variable by `1`, returning its updated value. */
		@inline def inc() :Long = self.synchronized { val res = self.unsafe + 1; self.unsafe = res; res }

		/** Atomically decreases this variable by `1`, returning its updated value. */
		@inline def dec() :Long = self.synchronized { val res = self.unsafe - 1; self.unsafe = res; res }

		/** Atomically sets this variable to its opposite value, returning the updated value (with new sign) */
		@inline def neg() :Long = self.synchronized { val res = -self.unsafe; self.unsafe = res; res }


		/** Atomically assigns this variable its bitwise disjunction with the specified value. */
		@inline def |=(n :Long) :Unit = self.synchronized { self.unsafe = self.unsafe | n }

		/** Atomically assigns this variable its bitwise conjunction with the specified value. */
		@inline def &=(n :Long) :Unit = self.synchronized { self.unsafe = self.unsafe & n }

		/** Atomically assigns this variable its bitwise ''xor'' with the specified value. */
		@inline def ^=(n :Long) :Unit = self.synchronized { self.unsafe = self.unsafe ^ n }

		/** Atomically bit-shifts this variable right by the specified number of bits. */
		@inline def >>=(n :Long) :Unit = self.synchronized { self.unsafe = self.unsafe >> n }

		/** Atomically bit-shifts this variable right by the specified number of bits, setting the freed higher bits to zero. */
		@inline def >>>=(n :Long) :Unit = self.synchronized { self.unsafe = self.unsafe >>> n }

		/** Atomically bit-shifts this variable left by the specified number of bits. */
		@inline def <<=(n :Long) :Unit = self.synchronized { self.unsafe = self.unsafe << n }

		/** Atomically assigns this variable its bitwise negation. */
		@inline def flip() :Unit = self.synchronized { self.unsafe = ~self.unsafe }


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
		@inline def +=(n :Float) :Unit = self.synchronized { self.unsafe = self.unsafe + n }

		/** Atomically increases this variable by the specified amount, returning its updated value. */
		@inline def inc(n :Float) :Float = self.synchronized { val res = self.unsafe + n; self.unsafe = res; res }

		/** Atomically decreases this variable by the specified amount. */
		@inline def -=(n :Float) :Unit = self.synchronized { self.unsafe = self.unsafe - n }

		/** Atomically decreases this variable by the specified amount, returning its updated value. */
		@inline def dec(n :Float) :Float = self.synchronized { val res = self.unsafe - n; self.unsafe = res; res }

		/** Atomically multiplies this variable by the specified amount. */
		@inline def *=(n :Float) :Unit = self.synchronized { self.unsafe = self.unsafe * n }

		/** Atomically multiplies this variable by the specified amount, returning its updated value. */
		@inline def multi(n :Float) :Float = self.synchronized { val res = self.unsafe * n; self.unsafe = res; res }

		/** Atomically divides this variable by the specified amount. */
		@inline def /=(n :Float) :Unit = self.synchronized { self.unsafe = self.unsafe / n }

		/** Atomically divides this variable by the specified amount, returning its updated value. */
		@inline def div(n :Float) :Float = self.synchronized { val res = self.unsafe / n; self.unsafe = res; res }

		/** Atomically sets this variable to its opposite value, returning the updated value (with new sign) */
		@inline def neg() :Float = self.synchronized { val res = -self.unsafe; self.unsafe = res; res }


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
		@inline def +=(n :Double) :Unit = self.synchronized { self.unsafe = self.unsafe + n }

		/** Atomically increases this variable by the specified amount, returning its updated value. */
		@inline def inc(n :Double) :Double = self.synchronized { val res = self.unsafe + n; self.unsafe = res; res }

		/** Atomically decreases this variable by the specified amount. */
		@inline def -=(n :Double) :Unit = self.synchronized { self.unsafe = self.unsafe - n }

		/** Atomically decreases this variable by the specified amount, returning its updated value. */
		@inline def dec(n :Double) :Double = self.synchronized { val res = self.unsafe - n; self.unsafe = res; res }

		/** Atomically multiplies this variable by the specified amount. */
		@inline def *=(n :Double) :Unit = self.synchronized { self.unsafe = self.unsafe * n }

		/** Atomically multiplies this variable by the specified amount, returning its updated value. */
		@inline def multi(n :Double) :Double = self.synchronized { val res = self.unsafe * n; self.unsafe = res; res }

		/** Atomically divides this variable by the specified amount. */
		@inline def /=(n :Double) :Unit = self.synchronized { self.unsafe = self.unsafe / n }

		/** Atomically divides this variable by the specified amount, returning its updated value. */
		@inline def div(n :Double) :Double = self.synchronized { val res = self.unsafe / n; self.unsafe = res; res }

		/** Atomically sets this variable to its opposite value, returning the updated value (with new sign) */
		@inline def neg() :Double = self.synchronized { val res = -self.unsafe; self.unsafe = res; res }


		/** Atomically tests if this variable equals zero and, if so, assigns it the given value
		  * @param ifZero new value for this variable
		  * @return `true` ''iff'' this variable changed in the effect of this call (its initial value was zero).
		  */
		@inline def testAndSet(ifZero :Double) :Boolean = self.testAndSet(0.0, ifZero)
	}



	/** Extension methods for acquiring locks of multiple variables at the same time. */
	implicit class SyncVarSeqExtension(private val self :Seq[SyncVar[_]]) extends AnyVal {
		/** Acquires monitors of all variables in this sequence and executes the specified expression,
		  * returning its value. This sorts the sequence according to [[System.identityHashCode identityHashcode]],
		  * before nesting `synchronized` blocks, in order to avoid deadlocks stemming from acquiring the locks
		  * in different order by different threads. Note that this order may differ between different runs
		  * of the program.
		  */
		def synchronizedInOrder[T](f: => T) :T = {
			val array = self.toArray
			val len   = array.length
			java.util.Arrays.sort(array, ReferentialOrdering)
			def sync(i :Int) :T =
				if (i == len) f else array(i).synchronized(sync(i + 1))
			sync(0)
		}

		/** Acquires monitors of all variables in this sequence in the order in which they appear,
		  * and executes the specified expression, returning its value.
		  * Application is responsible for ensuring that every time any two of these variables are locked,
		  * `synchronized` blocks are always nested in the order in which they appear in this sequence.
		  */
		def synchronizedAll[T](f: => T) :T = self match {
			case list :collection.LinearSeq[SyncVar[_]] =>
				def syncList(list :collection.LinearSeq[SyncVar[_]]) :T =
					if (list.isEmpty) f else list.head.synchronized(syncList(list.tail))
				syncList(list)
			case ApplyPreferred(seq) =>
				def syncIndexed(i :Int, end :Int) :T =
					if (i == end) f else seq(i).synchronized(syncIndexed(i + 1, end))
				syncIndexed(0, seq.length)
			case _ =>
				def syncIter(it :Iterator[SyncVar[_]]) :T =
					if (it.hasNext) it.next().synchronized(syncIter(it)) else f
				syncIter(self.iterator)
		}
	}



	@SerialVersionUID(Ver)
	private class Plain[@specialized(SpecializedVars) T] extends SyncVar[T] with Serializable {
		override var unsafe :T = _
		override def mkString = mkString("SyncVar")
	}
}
