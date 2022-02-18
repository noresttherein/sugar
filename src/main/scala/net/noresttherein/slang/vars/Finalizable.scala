package net.noresttherein.slang.vars

import java.lang.invoke.MethodHandles

import scala.Specializable.Args

import net.noresttherein.slang.optional.Opt
import net.noresttherein.slang.optional.Opt.{Got, Lack}
import net.noresttherein.slang.vars.Finalizable.{stateField, Immutable, Locked, Mutable}
import net.noresttherein.slang.vars.InOut.{SpecializedVars, TypeEquiv}
import net.noresttherein.slang.witness.DefaultValue




/** A boxed, thread-safe variable whose value can become finalized at some point, causing all future reads to return
  * the same, frozen value. Its life cycle consists of two phases: during the initialization phase, the variable can
  * be mutated and read just like a [[net.noresttherein.slang.vars.Volatile Volatile]] instance.
  * Once [[net.noresttherein.slang.vars.Finalizable.makeFinal makeFinal]] method is called by any thread, the value
  * can no longer be updated, and the read-only phase begins. This phase transition
  * is also thread-safe: no writes will be lost. Any future updates to the variable
  * will throw an [[IllegalStateException]] and the variable will become from this point effectively immutable.
  *
  * Aside of the variable [[net.noresttherein.slang.vars.InOut InOut]] interface, this class
  * implements also immutable API [[net.noresttherein.slang.vars.Val Val]].
  * While [[net.noresttherein.slang.vars.Finalizable.value value]] property can be accessed at any time returning
  * the current value of the variable, [[net.noresttherein.slang.vars.Finalizable.apply apply()]] will throw
  * an [[IllegalStateException]] if the variable has not been finalized, and return the same value once the variable
  * becomes finalized. Method [[net.noresttherein.slang.vars.Finalizable.isDefined isDefined]] can be used to determine
  * if the variable has been finalized (although `false` results may become outdated before being read by the caller)
  * and [[net.noresttherein.slang.vars.Ref.get get]] and [[net.noresttherein.slang.vars.Ref.opt opt]] have also
  * semantics of the immutable `Val`, returning `None`/`Lack` if the variable is not finalized.
  * @author Marcin Mościcki marcin@moscicki.net
  */
@SerialVersionUID(1L)
sealed class Finalizable[@specialized(SpecializedVars) T] private[vars] (init :T)
	extends InOut[T] with Val[T] with Serializable
{
	@volatile private[this] var x :T = init
	@volatile private[this] var state :Int = Mutable //0 - mutable; 1 - locked in crit section; 2 - immutable

	/** True if this variable is finalized. */
	override def isDefined :Boolean = state == Immutable

	/** Returns [[net.noresttherein.slang.vars.Finalizable.isDefined isDefined]]. */
	def isFinal :Boolean = isDefined

	/** Transitions this variable to an immutable state. From this time on, all future assignments
	  * will throw an [[IllegalStateException]], [[net.noresttherein.slang.vars.Finalizable.isDefined isDefined]]
	  * will always return `true`, and [[net.noresttherein.slang.vars.Finalizable.apply() apply]]`()` will
	  * return the current value.
	  */
	def makeFinal() :Unit =
		while (state != Immutable && !stateField.weakCompareAndSet(this :AnyRef, Mutable, Immutable))
			{}

	/** Transitions this variable to an immutable `Val` state with the given value. It is equivalent to an atomic
	  * version of
	  * {{{
	  *   this.value = newValue; this.makeFinal()
	  * }}}
	  * From this time on, all future assignments
	  * will throw an [[IllegalStateException]], [[net.noresttherein.slang.vars.Finalizable.isDefined isDefined]]
	  * will always return `true`, and [[net.noresttherein.slang.vars.Finalizable.apply() apply]]`()` will
	  * return `newValue`.
	  * @throws IllegalStateException if this variable is already final.
	  */
	def makeFinal(newValue :T) :Unit = {
		lock()
		x = newValue
		state = Immutable
	}

	/** The value of this variable if it is in the immutable state (finalized).  */
	override def opt :Opt[T] =
		if (state == Immutable) Got(x)
		else Lack

	/** The finalized value of this variable.
	  * @throws IllegalStateException if this variable is not finalized.
	  */
	override def apply() :T =
		if (state == Immutable) x
		else throw new IllegalStateException(toString + " is not finalized.")

	def finalized :T = apply()

	final override def value :T = x

	/** Sets the value of this variable to `newValue`.
	  * @throws IllegalStateException if this variable is finalized.
	  */
	override def value_=(newValue :T) :Unit = {
		lock()
		x = newValue
		state = Mutable
	}
	private[vars] def set(newValue :T) :Unit = x = newValue


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

	//overriden to avoid the creation of a closure object capturing other
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

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Finalizable[_]]
	override def hashCode :Int = super[InOut].hashCode
}






/** Factory of boxed [[net.noresttherein.slang.vars.Finalizable Finalizable]] variables.
  */
object Finalizable {
	/** Create a new finalizable variable which can be shared by multiple threads. */
	def apply[@specialized(SpecializedVars) T](init :T) :Finalizable[T] = new Finalizable(init)

	/** Create a new finalizable variable which can be shared by multiple threads. */
	def apply[@specialized(SpecializedVars) T](implicit default :DefaultValue[T]) :Finalizable[T] =
		apply(default.default)

	@inline implicit def unboxFinalizable[@specialized(SpecializedVars) T](ref :Finalizable[T]) :T = ref.value

	private final val stateField = MethodHandles.lookup().findVarHandle(
		classOf[Finalizable[Any]], "state", Integer.TYPE
	)
	private final val Mutable = 0
	private final val Locked = 1
	private final val Immutable = 2
}


