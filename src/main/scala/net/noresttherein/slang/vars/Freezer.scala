package net.noresttherein.slang.vars

import net.noresttherein.slang.vars.InOut.DefaultValue
import net.noresttherein.slang.vars.Var.SpecializedTypes


/** A boxed variable which value can become frozen at some point, causing all future reads to return the same,
  * frozen value. Its life cycle consists of two phases: during the initialization phase, the variable can
  * be mutated and read just as an unsynchronized [[net.noresttherein.slang.vars.Var Var]] instance, with writes
  * using no synchronization, and reads requiring access to a `@volatile` variable. This phase is not thread-safe.
  * Once the [[net.noresttherein.slang.vars.Freezer#.freeze freeze()]] method is called by any thread,
  * the value of the mutable variable is copied to an 'immutable' `@volatile` field, which is initialized only once
  * and the read-only phase begins. Any future updates to the variable will throw a `IllegalStateException` and
  * the variable will become from this point effectively immutable and thread safe.
  * As an optimization taking advantage of the immutability of the `@volatile` field, its value is then copied
  * to a standard `var` which can be placed in the processor cache, bypassing the `@volatile` access penalty altogether.
  *
  * The difference between `Export[T]` and `Freezer[T]` lies in synchronization: in this class, only the frozen, second
  * phase is thread-safe, and an update might be lost during freezing (it will be neither visible by any future read
  * nor will it fail with an exception). The latter is fully thread-safe in both phases at the cost of more expensive
  * access in the initialization phase.
  * @see [[net.noresttherein.slang.vars.Export Export]]
  * @author Marcin Mościcki marcin@moscicki.net
  */
sealed class Freezer[@specialized(SpecializedTypes) T](init :T) extends InOut[T] with Serializable {
	private[this] var mutable = init
	@volatile private[this] var immutable = init
	@volatile private[this] var frozen = false
	private[this] var fast = init
	private[this] var copied = false


	final override def get :T =
		if (copied)
			fast
		else if (frozen) {
			fast = immutable
			copied = true
			fast
		} else {
			mutable
		}

	@inline final override def value :T = get

	@inline final override def value_=(value :T) :Unit =
		if (frozen)
			throw new IllegalStateException(s"Freezer($immutable) is frozen.")
		else
            mutable = value

	@inline final override def :=(newValue :T) :Unit = this.value = value

	/** Freezes the value of this variable, making it immutable. Future updates will throw an `IllegalStateException`.
	  * While this call can be made concurrently to the modifications, some updates may be lost: happen after
	  * transferring the value to the inner immutable variable, but not result in an exception.
	  */
	@inline final def freeze() :Unit =
		if (!copied && !frozen) {
			fast = mutable
			copied = true
			immutable = fast
			frozen = true
		}

	/** Checks if this variable has been frozen and is now immutable. */
	@inline final def isFrozen :Boolean = copied || frozen

	/** Checks if this variable is still mutable. */
	@inline final def isMutable :Boolean = !copied && !frozen
}






/** Factory of boxed `Freezer` variables. */
object Freezer {

	/** Create a new variable which can become frozen at some point, turning it into a thread-safe, immutable value. */
	@inline def apply[@specialized(SpecializedTypes) T](value :T) :Freezer[T] = new Freezer(value)

	/** Create a new variable which can become frozen at some point, turning it into a thread-safe, immutable value. */
	@inline def apply[@specialized(SpecializedTypes) T](implicit default :DefaultValue[T]) :Freezer[T] =
		new Freezer(default.value)



	@inline implicit def unboxVar[@specialized(SpecializedTypes) T](vol :Freezer[T]) :T = vol.get

}

