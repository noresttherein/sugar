package net.noresttherein.slang.vars

import net.noresttherein.slang.vars.InOut.DefaultValue
import net.noresttherein.slang.vars.Var.SpecializedTypes


/** A boxed, thread-safe variable which value can become frozen at some point, causing all future reads to return
  * the same, frozen value. Its life cycle consists of two phases: during the initialization phase, the variable can
  * be mutated and read just as a synchronized [[net.noresttherein.slang.vars.SyncVar SyncVar]] instance.
  * Once the [[net.noresttherein.slang.vars.Export#export export()]] method is called by any thread,
  * the value of the mutable variable is copied to an 'immutable' `@volatile` field, which is initialized only once
  * and the read-only phase begins. Any future updates to the variable will throw a `IllegalStateException` and
  * the variable will become from this point effectively immutable and thread safe.
  * As an optimization taking advantage of the immutability of the `@volatile` field, its value is then copied
  * to a standard `var` which can be placed in the processor cache, bypassing the `@volatile` access penalty altogether.
  *
  * The difference between `Export[T]` and `Freezer[T]` lies in synchronization: in the latter, only the frozen, second
  * phase is thread-safe, and an update might be lost during freezing (it will be neither visible by any future read
  * nor will it fail with an exception). This variable is fully thread-safe in both phases at the cost of more expensive
  * access in the initialization phase. Additionally, this variable does not accept `null` values and will
  * throw a `NullPointerException` when assigned one.
  *
  * @see [[net.noresttherein.slang.vars.Freezer Freezer]]
  * @author Marcin Mościcki marcin@moscicki.net
  */
sealed class Export[T](init :T) extends SyncVar[T](init) with Serializable {
	if (init == null)
		throw new NullPointerException("Export(null)")

	@volatile private[this] var immutable :T = _
	private[this] var cached :T = _


	final override def get :T =
		if (cached != null)
			cached
		else if (immutable != null) {
			cached = immutable
			cached
		} else super.get



	/** Assigns a new value to this variable.
	  * @throws NullPointerException if value is null.
	  * @throws IllegalStateException if the `export()` method has been called.
	  */
	final override def :=(newValue :T) :Unit = synchronized {
		if (get == null)
			throw new IllegalStateException(s"Export($immutable) is exported.")
		else if (newValue == null)
			throw new NullPointerException(s"Export($get) := null")
		else
			super.:=(newValue)
	}



	/** Freezes the value of this variable. Any future attempts to assign it a new value, be it direct or through
	  * one of the more complex method, will throw an  `IllegalStateException`. The value returned by `get` and `value`
	  * will be the result of the last assignment, with no assignments being lost.
	  */
	@inline final def export() :Unit =
		if (cached == null && immutable == null) synchronized {
			if (cached == null && immutable == null) {
				cached = super.get
				super.:=(immutable) //set null
				immutable = cached
			}
		}

	/** Checks if this variable is now immutable. */
	@inline final def isExported :Boolean = cached != null || immutable != null

	/** Checks if this variable is still mutable. */
	@inline final def isMutable :Boolean = cached == null && immutable == null

}













/** Factory of boxed `Export` variables. */
object Export {

	/** Create a new thread-safe variable which can become frozen at some point, turning it into an immutable value. */
	@inline def apply[@specialized(SpecializedTypes) T](value :T) :Export[T] = new Export(value)

	/** Create a new thread-safe variable which can become frozen at some point, turning it into an immutable value. */
	@inline def apply[@specialized(SpecializedTypes) T](implicit default :DefaultValue[T]) :Export[T] =
		new Export(default.value)



	@inline implicit def unboxVar[@specialized(SpecializedTypes) T](vol :Export[T]) :T = vol.get

}







