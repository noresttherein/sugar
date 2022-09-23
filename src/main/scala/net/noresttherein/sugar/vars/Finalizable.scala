package net.noresttherein.sugar.vars

import java.lang.invoke.MethodHandles

import scala.Specializable.Args

import net.noresttherein.sugar.vars.Finalizable.stateField
import net.noresttherein.sugar.vars.InOut.{SpecializedVars, TypeEquiv}
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.witness.DefaultValue




/** A boxed, thread-safe variable whose value can become finalized at some point, causing all future reads to return
  * the same, frozen value. Its life cycle consists of two phases: during the initialization phase, the variable can
  * be mutated and read just like a [[net.noresttherein.sugar.vars.Volatile Volatile]] instance.
  * Once [[net.noresttherein.sugar.vars.Finalizable.makeFinal makeFinal]] method is called by any thread, the value
  * can no longer be updated, and the read-only phase begins. This phase transition
  * is also thread-safe: no writes will be lost. Any future updates to the variable
  * will throw an [[IllegalStateException]] and the variable will become from this point effectively immutable.
  *
  * Aside of the variable [[net.noresttherein.sugar.vars.InOut InOut]] interface, this class
  * implements also immutable API [[net.noresttherein.sugar.vars.Val Val]].
  * While [[net.noresttherein.sugar.vars.Finalizable.value value]] property can be accessed at any time returning
  * the current value of the variable, [[net.noresttherein.sugar.vars.Finalizable.apply apply()]] will throw
  * an [[IllegalStateException]] if the variable has not been finalized, and return the same value once the variable
  * becomes finalized. Method [[net.noresttherein.sugar.vars.Finalizable.isDefined isDefined]] can be used to determine
  * if the variable has been finalized (although `false` results may become outdated before being read by the caller)
  * and [[net.noresttherein.sugar.vars.Ref.option option]] and [[net.noresttherein.sugar.vars.Ref.opt opt]] have also
  * semantics of the immutable `Val`, returning `None`/`Lack` if the variable is not finalized.
	* @define Ref `Finalizable`
  * @author Marcin Mościcki marcin@moscicki.net
  */ //consider: making it a Ref, not a Val, so that equality compares current values
@SerialVersionUID(1L) //todo: SignalFinalizable
sealed class Finalizable[@specialized(SpecializedVars) T] private[vars] (init :T)
	extends InOut[T] with Val[T] with Serializable //consider: does it make sense for it to be Serializable
{
	import Finalizable.{Immutable, Locked, Mutable}
	@volatile private[this] var x     :T = init
	@volatile private[this] var state :Int = Mutable

	/** Returns `false`. */
	override def isEmpty       :Boolean = false

	/** Returns `true`. */
	override def isFinalizable :Boolean = true

	/** True if this variable is finalized. */
	override def isConst       :Boolean = state == Immutable

	/** True if this variable is finalized. */
	override def isDefined     :Boolean = state == Immutable

	/** True if this variable is finalized. */
	override def isDefinite    :Boolean = state == Immutable

	/** Transitions this variable to an immutable state. From this time on, all future assignments
	  * will throw an [[IllegalStateException]], [[net.noresttherein.sugar.vars.Finalizable.isDefined isDefined]]
	  * will always return `true`, and [[net.noresttherein.sugar.vars.Finalizable.apply() apply]]`()` will
	  * return the current value.
	  */
	def makeFinal() :Unit =
		while (state != Immutable && !stateField.weakCompareAndSet(this :AnyRef, Mutable, Immutable))
			{}

	/** The current value of this `Finalizable`. This method never throws an exception. */
	final override def value :T = x

	/** Sets the value of this variable to `newValue`.
		* @throws IllegalStateException if this variable is finalized.
		* @see [[net.noresttherein.sugar.vars.Finalizable.const_=]]
		*/
	override def value_=(newValue :T) :Unit = {
		lock()
		x = newValue
		state = Mutable
	}
	private[vars] final def set(newValue :T) :Unit = x = newValue

	/** The finalized value of this variable.
		* @throws NoSuchElementException if this variable is not finalized.
		*/
	final override def get :T =
		if (state == Immutable) x else throw new NoSuchElementException(toString + " is not finalized.")

	/** The finalized value of this variable.
		* @throws IllegalStateException if this variable is not finalized.
		*/
	override def const :T =
		if (state == Immutable) x else throw new UnsupportedOperationException(toString + " is not finalized.")

	/** Sets the final, [[net.noresttherein.sugar.vars.Finalizable.const constant]] value of this `Val`.
		* Same as `this.`[[net.noresttherein.sugar.vars.Finalizable.const_= const]]` = value`.
		*/
	@inline final def finalized_=(value :T) :Unit = const = value

	/** Transitions this variable to an immutable `Val` state with the given value. It is equivalent to an atomic
		* version of
		* {{{
		*   this.value = finalValue; this.makeFinal()
		* }}}
		* From this time on, all future assignments
		* will throw an [[IllegalStateException]], [[net.noresttherein.sugar.vars.Finalizable.isDefined isDefined]]
		* will always return `true`, and [[net.noresttherein.sugar.vars.Finalizable.apply() apply]]`()` will
		* return `finalValue`.
		*/
	@throws[IllegalStateException]("if this variable is already final.")
	def const_=(finalValue :T): Unit = {
		lock()
		x = finalValue
		state = Immutable
	}


	/** The current value of this variable: `Some(`[[net.noresttherein.sugar.vars.Finalizable.value value]]`)`. */
	override def option :Option[T] = Some(x)

	/** The current value of this variable: `Got(`[[net.noresttherein.sugar.vars.Finalizable.value value]]`)`. */
	override def opt    :Opt[T] = Got(x)

	/** The current value of this variable: `Sure(`[[net.noresttherein.sugar.vars.Finalizable.value value]]`)`. */
	override def unsure :Unsure[T] = Sure(x)

	/** The value of this variable if it is in the immutable state (finalized). */
	@inline final override def toOption :Option[T] = constOption

	/** The value of this variable if it is in the immutable state (finalized).  */
	@inline final override def toOpt    :Opt[T]  = constOpt

	/** The value of this variable if it is in the immutable state (finalized). */
	@inline final override def toUnsure :Unsure[T] = constUnsure

	/** The value of this variable if it is in the immutable state (finalized). */
	override def constOption :Option[T] = if (state == Immutable) Some(x) else None

	/** The value of this variable if it is in the immutable state (finalized). */
	override def constOpt    :Opt[T] = if (state == Immutable) Got(x) else Lack

	/** The value of this variable if it is in the immutable state (finalized). */
	override def constUnsure :Unsure[T] = if (state == Immutable) Sure(x) else Missing

	override def ?=(newValue :T) :T = {
		lock()
		val res = x
		x = newValue
		state = Mutable
		res
	}

	override def testAndSet(expect :T, newValue :T) :Boolean = {
		while (state != Immutable && !stateField.weakCompareAndSet(this :AnyRef, Mutable, Locked))
			{}
		if (state == Immutable)
			false
		else {
			val res = x == expect
			if (res)
				x = newValue
			state = Mutable
			res
		}
	}

	override def apply(f :T => T) :T = {
		lock()
		val res = f(x)
		x = res
		state = Mutable
		res
	}
	override def applyLeft[@specialized(Args) A](z :A)(f :(A, T) => T) :T = {
		lock()
		val res = f(z, x)
		x = res
		state = Mutable
		res
	}
	override def applyRight[@specialized(Args) A](z :A)(f :(T, A) => T) :T = {
		lock()
		val res = f(x, z)
		x = res
		state = Mutable
		res
	}

	private def lock() :Unit =
		while (!stateField.weakCompareAndSet(this :AnyRef, Mutable, Locked))
			if (state == Immutable)
				throw new IllegalStateException(toString + " is finalized.")

	//overridden to avoid the creation of a closure object capturing other
	private[vars] override def bool_&&=(other : => Boolean)(implicit ev :T TypeEquiv Boolean) :Unit = {
		lock()
		if (ev(this).value && !other)
			ev(this).set(false)
		state = Mutable
	}

	private[vars] override def bool_||=(other : => Boolean)(implicit ev :T TypeEquiv Boolean) :Unit = {
		lock()
		if (!ev(this).value && other)
			ev(this).set(true)
		state = Mutable
	}

	private[vars] override def isSpecialized :Boolean = getClass != classOf[Finalizable[_]]

}




/** Factory of boxed [[net.noresttherein.sugar.vars.Finalizable Finalizable]] variables.
  */
object Finalizable {
	/** Create a new finalizable variable which can be shared by multiple threads. */
	def apply[@specialized(SpecializedVars) T](init :T) :Finalizable[T] = new Finalizable(init)

	/** Create a new finalizable variable which can be shared by multiple threads. */
	def apply[@specialized(SpecializedVars) T](implicit default :DefaultValue[T]) :Finalizable[T] =
		apply(default.get)

	@inline implicit def unboxFinalizable[@specialized(SpecializedVars) T](ref :Finalizable[T]) :T = ref.value

	implicit def finalizableOrdering[T :Ordering] :Ordering[Finalizable[T]] = Val.valOrdering

	private final val stateField = MethodHandles.lookup().findVarHandle(
		classOf[Finalizable[Any]], "state", Integer.TYPE
	)
	private final val Mutable   = 0
	private final val Locked    = 1
	private final val Immutable = 2
}


