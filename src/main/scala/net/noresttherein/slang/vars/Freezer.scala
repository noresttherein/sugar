package net.noresttherein.slang.vars

import java.lang.invoke.{MethodHandles, VarHandle}

import scala.annotation.unspecialized

//import net.noresttherein.slang.vars.Freezer.frozenField
//import net.noresttherein.slang.vars.InOut.{DefaultValue, SpecializedVars}
//import net.noresttherein.slang.vars.Ref.Undefined
//
//
//
//
///** A boxed variable whose value can become frozen at some point, causing all future reads to return the same,
//  * frozen value. Its life cycle consists of two phases: during the initialization phase, the variable can
//  * be mutated and read just as an synchronized [[net.noresttherein.slang.vars.Var Var]] instance, with writes
//  * using no synchronization. This phase is not thread-safe, with no guarantees concerning the ordering of operations
//  * or their atomicity. Once the [[net.noresttherein.slang.vars.Freezer.freeze freeze()]] method is called by any thread,
//  * a ''release fence'' is invoked, synchronizing its value between the memory and processor cache, and the read-only
//  * phase begins. Any future updates to the variable will throw an [[IllegalStateException]] and the variable
//  * will become from this point effectively immutable and thread safe.
//  *
//  * The difference between `Export[T]` and `Freezer[T]` lies in atomicity: in this class, only the frozen, second
//  * phase is thread-safe, and an update might be lost during freezing (it will be neither visible by any future read
//  * nor will it fail with an exception). The latter is fully thread-safe in both phases at the cost of more expensive
//  * access in the initialization phase.
//  * @see [[net.noresttherein.slang.vars.Finalizable Finalizable]]
//  * @author Marcin Mościcki marcin@moscicki.net
//  */
//@SerialVersionUID(1L)
//sealed class Freezer[@specialized(SpecializedVars) T] private[vars] (private[this] var x :T)
//	extends InOut[T] with Val[T] with Serializable
//{
//	/* An implementation suitable for value types (or classes consisting ''only'' of `final` fields),
//	 * taking advantage of the fact that assigning a value to a variable is atomic and thread safe
//	 * as long as the value itself is immutable.
//	 */
//	private[this] var _frozen :Any = Undefined
//	private[this] var _isFrozen = false
//
//	/** Checks if this variable is [[net.noresttherein.slang.vars.Freezer.freeze frozen]].
//	  * If `true`, `this()` will return the frozen value.
//	  * @return [[net.noresttherein.slang.vars.Freezer.isFrozen]]
//	  */
//	override def isDefined :Boolean = isFrozen
//
//	/** Checks if this variable is [[net.noresttherein.slang.vars.Freezer.freeze frozen]].
//	  * If `true`, `this()` will return the frozen value.
//	  */
//	def isFrozen :Boolean = _frozen.asInstanceOf[AnyRef] ne Undefined
//
//	/** Makes this object immutable. This method must be called by the same (only) thread which created
//	  * this instance and has (possibly repeatedly) set its value using
//	  * `this.`[[net.noresttherein.slang.vars.Freezer.value_= value]]` = _`
//	  * or `this `[[net.noresttherein.slang.vars.InOut.:= :=]]` _`. Additionally, this object should not be shared
//	  * with other threads before calling this method, or they may see it in non frozen state, possibly indefinitely.
//	  */
//	def freeze() :Unit = {
//		_isFrozen = true
//		if (!frozenField.compareAndSet(this, Undefined, x))
//			throw new IllegalStateException(toString + " is already frozen.")
//	}
//
//	@unspecialized def frozen :T = apply()
//
//	/** Returns the value of this variable if it has been frozen, throwing a [[NoSuchElementException]] otherwise.
//	  * As long as the variable through which this object is accessed has been set by the thread which called
//	  * [[net.noresttherein.slang.vars.Freezer.freeze freeze]]`()` after that call, this is guaranteed
//	  * to return the frozen value, that is `this.value` as visible by the freezing thread at the point of freezing.
//	  * Once this method returns without throwing an exception, all subsequent calls will return the same value.
//	  * Note however that if multiple threads [[net.noresttherein.slang.vars.Freezer.value_= set]] the value
//	  * of this variable then, in the absence of external synchronization, the value returned by this method
//	  * can be different than `this.`[[net.noresttherein.slang.vars.Freezer.value value]], that is the latter may
//	  * include 'lost' assignments due to mutating thread not noticing the freezing performed by another thread.
//	  */
//	@unspecialized override def apply() :T = {
//		val res = _frozen
//		if (res.asInstanceOf[AnyRef] eq Undefined)
//			throw new NoSuchElementException(toString + " is not frozen.")
//		res.asInstanceOf[T]
//	}
//
//	override def value :T = x
//
//	override def value_=(newValue :T) :Unit = {
//		if (_isFrozen)
//			throw new IllegalStateException(toString + " is not frozen.")
//		x = newValue
//	}
//
//	private[Freezer] def frozenFieldHandle :VarHandle = MethodHandles.lookup().findVarHandle(
//		classOf[Freezer[Any]], "_frozen", classOf[Any]
//	)
//
//	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Freezer[_]]
//	override def hashCode :Int = super[InOut].hashCode
//}
//
//
//
//
//
//
///** Factory of boxed `Freezer` variables. */
//object Freezer {
//
//	/** Create a new variable which can become frozen at some point, turning it into a thread-safe, immutable value. */
//	@inline def apply[@specialized(SpecializedVars) T](value :T) :Freezer[T] =
//		new Freezer[T](value) match {
//			case ref if ref.getClass == UnspecializedClass => new RefFreezer[Any](value).asInstanceOf[Freezer[T]]
//			case res => res
//		}
//
//	/** Create a new variable which can become frozen at some point, turning it into a thread-safe, immutable value. */
//	@inline def apply[@specialized(SpecializedVars) T](implicit default :DefaultValue[T]) :Freezer[T] =
//		Freezer(default.value)
//
//
//	@inline implicit def unboxFreezer[@specialized(SpecializedVars) T](ref :Freezer[T]) :T = ref()
//
//
//
//	private class RefFreezer[T](init :T) extends Freezer[T](init) {
//		override def isFrozen :Boolean = frozenField.getAcquire(this) ne Undefined
//
//		override def apply() :T = {
//			val res = frozenField.getAcquire(this)
//			if (res == Undefined)
//				throw new NoSuchElementException(toString + " is not frozen.")
//			res.asInstanceOf[T]
//		}
//	}
//
//	private val frozenField = new Freezer[Any](null).frozenFieldHandle
//
//	private val UnspecializedClass = new Freezer[Any](null).getClass
//}
