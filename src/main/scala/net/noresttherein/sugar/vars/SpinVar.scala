package net.noresttherein.sugar.vars

import java.lang.invoke.MethodHandles

import scala.Specializable.Args

import net.noresttherein.sugar.collections.IndexedIterable.ApplyPreferred
import net.noresttherein.sugar.concurrent.currentThreadId
import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.SpinVar.{Unlocked, stateField}
import net.noresttherein.sugar.witness.{DefaultValue, ReferentialOrdering}




/** A boxed variable with synchronized access under a spin lock on a `@volatile` variable.
  * All ''test-and-set'' and similar operations are guaranteed atomic, and it is possible to acquire locks
  * for several variables by calling [[net.noresttherein.sugar.vars.SpinVar.locked locked]],
  * similarly to `synchronized` blocks:
  * {{{
  *     val spin1 = SpinVar(1)
  *     val spin2 = SpinVar(2)
  *     spin1.locked {
  *         spin2.locked {
  *             val x = spin1.get
  *             spin1 := spin2
  *             spin2 := x
  *         }
  *     }
  * }}}
  * The same can also be accomplished using ''for comprehension'' syntax:
  * {{{
  *     for {
  *         x <- spin1
  *         y <- spin2
  *     } {
  *         spin1 := y
  *         spin2 := x
  *     }
  * }}}
  * Overall, it is almost a perfect mirror of [[net.noresttherein.sugar.vars.SyncVar SyncVar]].
  * However, because all threads waiting for the lock are not suspended, this feature should be used only
  * if the critical section is very short, for example assigning values to several `SpinVar`s.
  * Additionally, just as with `SyncVar`/`synchronized`, attempts to acquire the locks in different order
  * by different parts of the program risk causing deadlocks.
  *
  * This implementation is perceiveably slower than [[net.noresttherein.sugar.vars.Volatile Volatile]]`[T]`
  * and [[net.noresttherein.sugar.vars.Atomic Atomic]]`[T]`,
  * so use it only if you need the combined locking functionality.
  * @tparam T the type of this variable
  * @define Ref `SpinVar`
  * @define ref locked variable
  * @author Marcin Mościcki marcin@moscicki.net
  */
@SerialVersionUID(Ver)
sealed class SpinVar[@specialized(SpecializedVars) T] private[vars]
	extends AtomicOps.AtomicVar[T] with Mutable[T] with Serializable
{
	protected override def factory :AtomicOps[SpinVar] = SpinVar

	/** The underlying variable field. */
	private[this] var x :T = _

	/** Id of the thread currently owning the critical section, or `Unknown` if the variable is not locked. */
	@volatile private[this] var mutex :Long   = Unlocked

	/** Unsynchronized access to this variable. */
	private[vars] def unsafe :T = x
	/** Unsynchronized access to this variable. */
	private[vars] def unsafe_=(value :T) :Unit = x = value

	override def value :T = {
		lock()
		val res = x
		mutex = Unlocked
		res
	}
	override def value_=(newValue :T) :Unit = {
		lock()
		x = newValue
		mutex = Unlocked
	}

	@inline private def lock() :Unit = {
		val threadId = currentThreadId
		while (!stateField.weakCompareAndSet(this :AnyRef, Unlocked, threadId))
			{}
	}
	@inline private def unlock() :Unit = mutex = Unlocked

	/** Executes the given expression while holding the lock for this variable
	  * (the same under which all reads and writes are performed). This is not the monitor lock acquired
	  * by method `synchronized` of `AnyRef`, but the method may be used in the same manner, to nest critical sections
	  * for several variables.
	  */
	def locked[O](expr: => O) :O =
		try { lock(); expr } finally mutex = Unlocked

	/** Atomically assigns a new value to this $ref, returning the current value. */
	override def ?=(newValue :T) :T = {
		lock()
		val res = x
		x = newValue
		mutex = Unlocked
		res
	}

	/** Assigns a new value to this variable providing the current value is equal to the expected value.
	  * @param expect   a value to compare with current value.
	  * @param newValue a new value for this variable.
	  * @return `true` if previous value equaled `expect` and the variable has been set to `newValue`.
	  */
	override def testAndSet(expect :T, newValue :T) :Boolean =
		try { //We must do it in a try block because we are invoking x.equals
			lock()
			x == expect && { x = newValue; true }
		} finally
			mutex = Unlocked


	/** Atomically updates the value of this variable with the given function. This is equivalent to
	  * `this := f(this); this.get` with the guarantee that no other thread will modify the value of this variable
	  * between the individual operations.
	  * @note function `f` is executed under a spin lock, so must be very brief.
	  * @param f function to apply to the value of this variable.
	  * @return result of applying `f` to the current value.
	  */
	override def update(f :T => T) :T =
		try {
			lock()
			x = f(x)
			x
		} finally
			mutex = Unlocked

	override def updateLeft[@specialized(Args) A](z :A)(f :(A, T) => T) :T =
		try {
			lock()
			x = f(z, x)
			x
		} finally
			mutex = Unlocked

	override def updateRight[@specialized(Args) A](z :A)(f :(T, A) => T) :T =
		try {
			lock()
			x = f(x, z)
			x
		} finally
			mutex = Unlocked


	/** Applies the given function to the value of this $ref, while holding the lock for this variable.
	  * This can be used to acquire monitors of several variables, recursively, or using ''for comprehension'' syntax:
	  * {{{
	  *     val var1 = SpinVar(1)
	  *     val var2 = SpinVar(2)
	  *     for { x <- var1; y <- var2 } yield x + y
	  * }}}
	  */
	def flatMap[O](f :T => O) :O =
		try { lock(); f(x) } finally mutex = Unlocked

	/** Applies the given function to the value of this $ref, while holding the lock for this variable.
	  * This can be used to acquire monitors of several variables, recursively, or using ''for comprehension'' syn tax
	  * @see [[net.noresttherein.sugar.vars.SpinVar.flatMap flatMap]]
	  */
	def map[O](f :T => O) :O =
		try { lock(); f(x) } finally mutex = Unlocked

	/** Applies the given function to the value of this $ref, while holding the lock for this variable.
	  * This can be used to acquire monitors of several variables, recursively, or using ''for comprehension'' syntax:
	  * {{{
	  *     val var1 = SpinVar(1)
	  *     val var2 = SpinVar(2)
	  *     for { x <- sync1; y <- sync2 } {
	  *         var1 := y
	  *         var2 := y
	  *     }
	  * }}}
	  */
	@inline def foreach[U](f :T => U) :Unit = flatMap(f)

	private[vars] override def isSpecialized :Boolean = getClass != classOf[SpinVar[_]]
}






/** Factory of synchronized variables. Provides implicit extensions of greater precedence reducing syn
  * to provide inlining o synchronized operations.
  */
@SerialVersionUID(Ver)
case object SpinVar extends AtomicOps[SpinVar] {

	/** Create a wrapper over a '''`var`''' of type `T` which can be passed as an in/out method parameter. */
	def apply[@specialized(SpecializedVars) T](value :T) :SpinVar[T] = {
		val res = new SpinVar[T]
		res.unsafe = value
		res.unlock() //Volatile access to make sure that the object is seen as completely initialized.
		res
	}

	/** Create a wrapper over a '''`var`''' of type `T` which can be passed as an in/out method parameter. */
	@inline def apply[@specialized(SpecializedVars) T](implicit default :DefaultValue[T]) :SpinVar[T] =
		SpinVar(default.get)	


	protected override def getAndSet[@specialized(SpecializedVars) T](v :AtomicOps.AtomicVar[T], newValue :T) :T =
		v.asInstanceOf[SpinVar[T]] ?= newValue

	protected override def testAndSet[@specialized(SpecializedVars) T]
	                                 (v :AtomicOps.AtomicVar[T], expect :T, newValue :T) :Boolean =
		v.asInstanceOf[SpinVar[T]].testAndSet(expect, newValue)

	protected override def weakTestAndSet[@specialized(SpecializedVars) T]
	                                     (v :AtomicOps.AtomicVar[T], expect :T, newValue :T) :Boolean =
		testAndSet(v, expect, newValue)
//unused:
	protected override def testAndSetRef[T](v :AtomicOps.AtomicVar[T], expect :T, newValue :T) :Boolean =
		genericTestAndSet(v, expect, newValue)

	protected override def weakTestAndSetRef[T](v :AtomicOps.AtomicVar[T], expect :T, newValue :T) :Boolean =
		genericTestAndSet(v, expect, newValue)

	protected override def weakTestAndSetBool(v :AtomicOps.AtomicVar[Boolean], expect :Boolean, newValue :Boolean)
			:Boolean =
		testAndSet(v, expect, newValue)

	protected override def repeatTestAndSetBool(v :AtomicOps.AtomicVar[Boolean], expect :Boolean,
	                                            ifExpected :Boolean, ifNotExpected :Boolean) :Boolean =
	{
		val spin = v.asInstanceOf[SpinVar[Boolean]]
		spin.locked {
			if (spin.unsafe == expect) {
				spin.unsafe = ifExpected
				ifExpected
			} else {
				spin.unsafe = ifNotExpected
				ifNotExpected
			}
		}
	}


	/** Implicit conversion of `SpinVar[Boolean]` values providing logical operators.
	  * These are special variants of extension methods already available through
	  * `InOut.`[[net.noresttherein.sugar.vars.InOut.InOutBooleanLogic InOutBooleanLogic]].
	  */
	implicit class SpinVarBooleanLogic(private val self :SpinVar[Boolean]) extends AnyVal {
		/** Atomically assigns this variable its logical conjunction with the given argument: `this := this && other`. */
		@inline def &&=(other: => Boolean) :Unit =
			try { self.lock(); self.unsafe = self.unsafe && other } finally self.unlock()

		/** Atomically assigns this variable its logical disjunction with the given argument: `this := this || other`. */
		@inline def ||=(other: => Boolean) :Unit =
			try { self.lock(); self.unsafe = self.unsafe || other } finally self.unlock()
	}



	/** Extension methods for acquiring locks of multiple variables at the same time. */
	implicit class SpinVarSeqExtension(private val self :Seq[SpinVar[_]]) extends AnyVal {
		/** Acquires locks of all variables in this sequence and executes the specified expression,
		  * returning its value. This sorts the sequence according to [[System.identityHashCode identityHashcode]],
		  * before nesting [[net.noresttherein.sugar.vars.SpinVar.locked locked]] blocks, in order to avoid deadlocks
		  * stemming from acquiring the locks in different order by different threads.
		  * Note that this order may differ between different runs of the program.
		  */
		def lockedInOrder[T](f: => T) :T = {
			val array = self.toArray
			val len   = array.length
			java.util.Arrays.sort(array, ReferentialOrdering)
			def sync(i :Int) :T =
				if (i == len) f else array(i).synchronized(sync(i + 1))
			sync(0)
		}

		/** Acquires locks of all variables in this sequence in the order in which they appear, and executes
		  * the specified expression, returning its value. Application is responsible for ensuring that every time
		  * any two of these variables are locked, [[net.noresttherein.sugar.vars.SpinVar.locked locked]] blocks
		  * are always nested in the order in which they appear in this sequence.
		  */
		def lockedAll[T](f: => T) :T = self match {
			case list :collection.LinearSeq[SpinVar[_]] =>
				def syncList(list :collection.LinearSeq[SpinVar[_]]) :T =
					if (list.isEmpty) f else list.head.synchronized(syncList(list.tail))
				syncList(list)
			case ApplyPreferred(seq) =>
				def syncIndexed(i :Int, end :Int) :T =
					if (i == end) f else seq(i).synchronized(syncIndexed(i + 1, end))
				syncIndexed(0, seq.length)
			case _ =>
				def syncIter(it :Iterator[SpinVar[_]]) :T =
					if (it.hasNext) it.next().synchronized(syncIter(it)) else f
				syncIter(self.iterator)
		}
	}



	private final val stateField = MethodHandles.lookup().findVarHandle(
		classOf[SpinVar[Any]], "mutex", java.lang.Long.TYPE
	)
	private final val Unlocked = -1L
}






/** An atomic variable updated under a spin lock which uses referential equality (`eq`)
  * to compare new value with the old one.
  * @see [[net.noresttherein.sugar.vars.SpinVar! SpinVar]]
  * @define Ref `SpinRef`
  * @define ref spin reference
  */  //todo: this probably shouldn't be a SpinVar, as it inherits only implementation.
@SerialVersionUID(Ver)
final class SpinRef[T] private (v :T) extends SpinVar[T] {
	value = v

	override def testAndSet(expect :T, newValue :T) :Boolean = locked {
		(unsafe.asInstanceOf[AnyRef] eq expect.asInstanceOf[AnyRef]) && { unsafe = newValue; true }
	}
	private[vars] override def isSpecialized = false
}


/** An atomic variable updated under a spin lock which uses referential equality (`eq`)
  * to compare new value with the old one.
  * @see [[net.noresttherein.sugar.vars.SpinVar$ SpinVar]]
  * @define Ref `SpinRef`
  * @define ref spin reference
  */
@SerialVersionUID(Ver)
case object SpinRef {
	/** Create a wrapper over a '''`var`''' of type `T` which can be passed as an in/out method parameter. */
	def apply[T](value :T) :SpinRef[T] = new SpinRef(value)

	/** Create a wrapper over a '''`var`''' of type `T` which can be passed as an in/out method parameter. */
	def apply[T](implicit default :DefaultValue[T]) :SpinRef[T] = new SpinRef(default.get)
}
