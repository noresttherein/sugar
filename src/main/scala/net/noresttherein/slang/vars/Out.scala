package net.noresttherein.slang.vars

import java.lang.invoke.MethodHandles

import net.noresttherein.slang.vars.InOut.SpecializedVars
import net.noresttherein.slang.vars.Opt.{Got, Lack}




/** An emulation of an 'out' parameter of a procedure. It is a wrapper over a `@specialized var` of type `T`
  * which will throw an [[IllegalStateException]] if accessed before the value is set with one of the methods
  * `this.value = x` or `this := x`, as well as when a second attempt to set its value is made which would
  * override the previously set value. This class is thread safe with ''volatile'' semantics.
  * @author Marcin Mościcki
  */
@SerialVersionUID(1L)
sealed class Out[@specialized(SpecializedVars) T] extends InOut[T] with Val[T] with Serializable {
	@scala.volatile private[this] var x :T = _       //todo: make sure that writes to the two cannot be reordered.
	@scala.volatile private[this] var isSet = false //set through InOut.isSetField VarHandle

	override def isDefined :Boolean = isSet

	override def opt    :Opt[T] = if (isSet) Got(x) else Lack
	override def asShot :Shot[T] = if (isSet) Hit(x) else Miss

	final override def value :T = {
		if (!isSet)
			throw new IllegalStateException("Out value not set.")
		x
	}
	final override def value_=(newValue :T) :Unit = {
		if (!(Out.isSetField.compareAndSet(this :Out[_], false, true) :Boolean))
			throw new IllegalStateException("Out value already initialized: " + x + ".")
		x = newValue
	}

	private[vars] override def isSpecialized :Boolean = getClass == classOf[Out[Any]]

	override def toString :String = if (isSet) String.valueOf(value) else "?"
}




/** An emulation of an 'out' parameter of a procedure, creating uninitialized [[net.noresttherein.slang.vars.Out Out]]
  * instances. This [[net.noresttherein.slang.vars.InOut InOut]] extension allows the value of the variable to be set
  * at most once.
  * @author Marcin Mościcki
  */
object Out {
	/** Create an uninitialized [[net.noresttherein.slang.vars.InOut InOut]] which cannot be accessed before
	  * its value is explicitly initialized, and which value can be set at most once.
	  */
	def apply[@specialized(SpecializedVars) T]() = new Out[T]

	private val isSetField = MethodHandles.lookup().findVarHandle(
		classOf[Out[Any]], "net$noresttherein$slang$vars$Out$$isSet", java.lang.Boolean.TYPE
	)
}
