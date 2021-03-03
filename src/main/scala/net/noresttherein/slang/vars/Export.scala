package net.noresttherein.slang.vars

import java.lang.invoke.MethodHandles

import scala.Specializable.Args

import net.noresttherein.slang
import net.noresttherein.slang.vars.Export.{Immutable, Mutable, Mutated}
import net.noresttherein.slang.vars.InOut.{DefaultValue, SpecializedVars, TypeEquiv}




/** A boxed, thread-safe variable which value can become frozen at some point, causing all future reads to return
  * the same, frozen value. Its life cycle consists of two phases: during the initialization phase, the variable can
  * be mutated and read just as an atomic [[net.noresttherein.slang.vars.Atomic Atomic]] instance.
  * Once the [[net.noresttherein.slang.vars.Export.export export()]] method is called by any thread, the value
  * of the `@volatile` variable can no longer be updated, and the read-only phase begins. This phase transition
  * is also thread-safe: no writes will be lost. Any future updates to the variable will throw an `IllegalStateException`
  * and the variable will become from this point effectively immutable. As an optimization, its value is also copied
  * to a standard `var` which can be placed in the processor cache, bypassing the `@volatile` access penalty altogether.
  *
  * The difference between `Export[T]` and `Freezer[T]` lies in atomicity: in the latter, only the frozen, second
  * phase is thread-safe, and an update might be lost during freezing (it will be neither visible by any future read
  * nor will it fail with an exception). This variable is fully thread-safe in both phases at the cost of more expensive
  * access in the initialization phase. Additionally, this variable does not accept `null` values and will
  * throw a `NullPointerException` when assigned one.
  *
  * @see [[net.noresttherein.slang.vars.Freezer Freezer]]
  * @author Marcin Mościcki marcin@moscicki.net
  */
@SerialVersionUID(1L)
sealed class Export[@specialized(SpecializedVars) T](init :T) extends InOut[T] with Serializable {
	if (init == null)
		throw new NullPointerException("Export(null)")

	@scala.volatile private[this] var x :T = init
	@scala.volatile private[this] var lock :Int = 0 //0: mutable; 1: in crit section; 2: immutable
	private[this] var cached :T = _
	private[this] var isCached = false


	final override def value :T =
		if (isCached) cached else x

	/** Assigns a new value to this variable.
	  * @throws NullPointerException if value is null.
	  * @throws IllegalStateException if the `export()` method has been called.
	  */
	final override def value_=(newValue :T) :Unit = {
		acquire()
		x = newValue
		lock = Mutable
	}

	final def exported :T =
		if (lock != 2)
			throw new IllegalStateException(s"Export($x) is not exported.")
		else if (isCached)
			cached
		else {
			cached = x
			isCached = true
			cached
		}

	/** Freezes the value of this variable. Any future attempts to assign it a new value, be it direct or through
	  * one of the more complex method, will throw an  `IllegalStateException`. The value returned by `get` and `value`
	  * will be the result of the last assignment, with no assignments being lost.
	  */
	final def export() :Unit =
		if (lock != Immutable) {
			while(!(Export.lock.compareAndSet(Mutable, Mutated) :Boolean))
				if (lock == Immutable)
					return
			cached = x      //this is wonky as, in theory, these two writes can be reordered
			isCached = true
			slang.publishMutable()
			lock = Immutable
		}

	/** Checks if this variable is now immutable. */
	@inline final def isExported :Boolean = lock == 2

	override def ?=(newValue :T) :T = {
		acquire()
		val res = x
		x = newValue
		lock = Mutable
		res
	}

	override def testAndSet(expect :T, assign :T) :Boolean = {
		acquire()
		val old = x
		if (old == expect) {
			x = assign
			lock = Mutable
			true
		} else {
			lock = Mutable
			false
		}
	}


	override def apply(f :T => T) :T = {
		acquire()
		try { val res = f(x); x = res; res }
		finally { lock = 0 }
	}

	override def applyLeft[@specialized(Args) A](z :A)(f :(A, T) => T) :T = {
		acquire()
		try { val res = f(z, x); x = res; res }
		finally { lock = Mutable }
	}

	override def applyRight[@specialized(Args) A](z :A)(f :(T, A) => T) :T = {
		acquire()
		try { val res = f(x, z); x = res; res }
		finally { lock = Mutable }
	}


	private def acquire(): Unit =
		while(!(Export.lock.compareAndSet(this :Export[_], Mutable, Mutated) :Boolean))
			if (Export.lock.getVolatile(this :Export[_]) == Immutable)
				throw new IllegalStateException(s"Export($x) is exported.")

}






/** Factory of boxed `Export` variables. */
object Export {

	/** Create a new thread-safe variable which can become frozen at some point, turning it into an immutable value. */
	@inline def apply[@specialized(SpecializedVars) T](value :T) :Export[T] = new Export(value)

	/** Create a new thread-safe variable which can become frozen at some point, turning it into an immutable value. */
	@inline def apply[@specialized(SpecializedVars) T](implicit default :DefaultValue[T]) :Export[T] =
		new Export(default.value)



	@inline implicit def unboxVar[@specialized(SpecializedVars) T](vol :Export[T]) :T = vol.get

	private final val Mutable = 0
	private final val Mutated = 1
	private final val Immutable = 2

	private val lock = MethodHandles.lookup().findVarHandle(
		classOf[Export[Any]], "net$noresttherein$slang$vars$Export$$lock", java.lang.Integer.TYPE
	)

}


