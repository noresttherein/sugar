package net.noresttherein.sugar.vars

import java.lang.invoke.MethodHandles

import scala.Specializable.Args

import net.noresttherein.sugar.collections.IndexedIterable.ApplyPreferred
import net.noresttherein.sugar.concurrent.currentThreadId
import net.noresttherein.sugar.vars.InOut.{SpecializedVars, TypeEquiv}
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
  * This implementation perceiveably slower than [[net.noresttherein.sugar.vars.Volatile Volatile]]`[T]`,
  * so use it only if you need the combined locking functionality, or the ''test-and-set'' operations
  * to use object equality (`equals` method), rather than referential equality (`eq`).
  *
  * @tparam T the type of this variable
  * @define Ref `SpinVar`
  * @define ref locked variable
  * @author Marcin Mościcki marcin@moscicki.net
  */
@SerialVersionUID(Ver)
class SpinVar[@specialized(SpecializedVars) T] private[vars] extends Mutable[T] with Serializable {
	/** The underlying variable field. */
	private[this] var x :T = _

	/** Id of the thread currently owning the critical section, or `Unknown` if the variable is not locked. */
	@volatile private[this] var mutex :Long   = Unlocked

	/** Unsynchronized access to this variable. */
	private[vars] def unsafe :T = x
	/** Unsynchronized access to this variable. */
	private[vars] def unsafe_=(value :T) :Unit = x = value

	final override def value :T = {
		lock()
		val res = x
		mutex = Unlocked
		res
	}
	final override def value_=(newValue :T) :Unit = {
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
	final def locked[O](expr: => O) :O =
		try { lock(); expr } finally mutex = Unlocked

	/** Atomically assigns a new value to this $ref, returning the current value. */
	final override def ?=(newValue :T) :T = {
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
	final override def testAndSet(expect :T, newValue :T) :Boolean =
		try { //We must do it in a try block because we are invoking x.equals
			lock()
			x == expect && { x = newValue; true }
		} finally
			mutex = Unlocked


	/** Atomically updates the value of this variable with the given function. This is equivalent to
	  * `this := f(this); this.get` with the guarantee that no other thread will modify the value of this variable
	  * between the individual operations.
	  * @param f function to apply to the value of this variable.
	  * @return result of applying `f` to the current value.
	  */
	final override def update(f :T => T) :T =
		try {
			lock()
			x = f(x)
			x
		} finally
			mutex = Unlocked

	final override def updateLeft[@specialized(Args) A](z :A)(f :(A, T) => T) :T =
		try {
			lock()
			x = f(z, x)
			x
		} finally
			mutex = Unlocked

	final override def updateRight[@specialized(Args) A](z :A)(f :(T, A) => T) :T =
		try {
			lock()
			x = f(x, z)
			x
		} finally
			mutex = Unlocked


	/** Applies given function to the value of this $ref, while holding the lock for this variable.
	  * This can be used to acquire monitors of several variables, recursively, or using ''for comprehension'' syntax:
	  * {{{
	  *     val var1 = SpinVar(1)
	  *     val var2 = SpinVar(2)
	  *     for { x <- var1; y <- var2 } yield x + y
	  * }}}
	  */
	def flatMap[O](f :T => O) :O =
		try { lock(); f(x) } finally mutex = Unlocked

	/** Applies given function to the value of this $ref, while holding the lock for this variable.
	  * This can be used to acquire monitors of several variables, recursively, or using ''for comprehension'' syn tax
	  * @see [[net.noresttherein.sugar.vars.SpinVar.flatMap flatMap]]
	  */
	def map[O](f :T => O) :O =
		try { lock(); f(x) } finally mutex = Unlocked

	/** Applies given function to the value of this $ref, while holding the lock for this variable.
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

	//overridden to avoid creating a functional object closure
	private[vars] override def bool_&&=(other: => Boolean)(implicit ev :T TypeEquiv Boolean) :Unit =
		try {
			lock()
			val self = ev(this)
			self.unsafe = self.unsafe && other
		} finally
			mutex = Unlocked

	private[vars] override def bool_||=(other: => Boolean)(implicit ev :T TypeEquiv Boolean) :Unit =
		try {
			val self = ev(this)
			self.unsafe = self.unsafe || other
		} finally
			mutex = Unlocked


	private[vars] override def isSpecialized :Boolean = getClass != classOf[SpinVar[_]]
}






/** Factory of synchronized variables. Provides implicit extensions of greater precedence reducing syn
  * to provide inlining o synchronized operations.
  */
@SerialVersionUID(Ver)
object SpinVar {

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


	/** Implicit conversion of `SpinVar[Boolean]` values providing logical operators.
	  * These are special variants of extension methods already available through
	  * `InOut.`[[net.noresttherein.sugar.vars.InOut.InOutBooleanLogic InOutBooleanLogic]].
	  */
	implicit class SpinVarBooleanLogic(private val self :SpinVar[Boolean]) extends AnyVal {
		/** Atomically assigns this variable its logical conjunction with the given argument: `this := this && other`. */
		@inline def &&=(other: =>Boolean) :Unit =
			try { self.lock(); self.unsafe = self.unsafe && other } finally self.unlock()

		/** Atomically assigns this variable its logical disjunction with the given argument: `this := this || other`. */
		@inline def ||=(other: =>Boolean) :Unit =
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
