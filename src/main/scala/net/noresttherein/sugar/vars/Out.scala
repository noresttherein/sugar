package net.noresttherein.sugar.vars

import java.lang.invoke.MethodHandles

import scala.annotation.nowarn

import net.noresttherein.sugar.exceptions.{illegalState_!, noSuch_!}
import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Opt.One
import net.noresttherein.sugar.vars.Ref.undefined




/** An emulation of an 'out' parameter of a procedure. It is a wrapper over a `@specialized var` of type `T`
  * which will throw an [[NoSuchElementException]] if accessed before the value is set with one of the methods
  * `this.value = x` or `this := x`, as well as when a second attempt to set its value is made which would
  * override the previously set value. The default implementation returned by the companion object is not thread safe;
  * see [[net.noresttherein.sugar.vars.VolatileOut VolatileOut]] for an `Out` with ''volatile'' semantics.
  * @define Ref `Out`
  * @define ref value
  * @author Marcin Mościcki
  */
sealed trait Out[@specialized(SpecializedVars) T] extends InOut[T] with Val[T] with Serializable {
	@inline final override def isEmpty       :Boolean = !isDefined
	@inline final override def isFinalizable :Boolean = isDefined
	@inline final override def isConst       :Boolean = isDefined
	@inline final override def isDefinite    :Boolean = isDefined

	@inline final override def get :T = value

	@inline final override def const :T =
		if (isDefined) value else noSuch_!("Out.const")

//	override def option      :Option[T] = if (isDefined) Some(value) else None
	override def opt         :Opt[T] = if (isDefined) One(value) else None
	override def unsure      :Unsure[T] = if (isDefined) Sure(value) else Missing
	@inline final override def toOption    :Option[T] = option
	@inline final override def constOption :Option[T] = option
	@inline final override def toOpt       :Opt[T] = opt
	@inline final override def constOpt    :Opt[T] = opt
	@inline final override def toUnsure    :Unsure[T] = unsure
	@inline final override def unsureConst :Unsure[T] = unsure

	//overridden because it mixes in Function0 which overrides it
	override def toString :String = if (isDefined) String.valueOf(value) else undefined.toString
}




/** An emulation of an 'out' parameter of a procedure, creating uninitialized [[net.noresttherein.sugar.vars.Out Out]]
  * instances. This [[net.noresttherein.sugar.vars.InOut InOut]] extension allows the value of the variable to be set
  * at most once.
  */
@SerialVersionUID(Ver)
case object Out {
	/** Create an uninitialized [[net.noresttherein.sugar.vars.InOut InOut]] which cannot be accessed before
	  * its value is explicitly initialized, and which value can be set at most once.
	  */
	def apply[@specialized(SpecializedVars) T]() :Out[T] = new LocalOut[T]

	implicit def outOrdering[T :Ordering] :Ordering[Out[T]] = Val.valOrdering

	@SerialVersionUID(Ver)
	private sealed class LocalOut[@specialized(SpecializedVars) T] private[vars] extends Out[T] {
		private[this] var x :T = _
		private[this] var isSet = false //set through InOut.isSetField VarHandle

		@inline final override def isDefined     :Boolean = isSet

		final override def value :T = {
			if (!isSet)
				noSuch_!("Out value not set.")
			x
		}
		@throws[IllegalStateException]("if this instance is already initialized.")
		final override def value_=(newValue :T) :Unit =
			if (isSet)
				illegalState_!("Out value already initialized: " + x  + ".")
			else {
				x     = newValue
				isSet = true
			}

		override def opt         :Opt[T] = if (isSet) One(x) else None
		override def unsure      :Unsure[T] = if (isSet) Sure(x) else Missing

		private[vars] override def isSpecialized :Boolean = getClass != classOf[LocalOut[Any]]

		override def mkString :String = mkString("Out")
	}

}






/** An emulation of an 'out' parameter of a procedure. It is a wrapper over a `@specialized var` of type `T`
  * which will throw an [[NoSuchElementException]] if accessed before the value is set with one of the methods
  * `this.value = x` or `this := x`, as well as when a second attempt to set its value is made which would
  * override the previously set value. This class is thread safe with ''volatile'' semantics.
  * @define Ref `VolatileOut`
  * @define ref out value
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
sealed class VolatileOut[@specialized(SpecializedVars) T] private[vars] extends Out[T] {
	@scala.volatile private[this] var x :T = _
	@scala.volatile @nowarn private[this] var isSet = false //set through InOut.isSetField VarHandle

	override def isDefined     :Boolean = isSet

	final override def value :T = {
		if (!isSet)
			noSuch_!("Out value not set.")
		x
	}
	@throws[IllegalStateException]("if this instance is already initialized.")
	final override def value_=(newValue :T) :Unit = {
		if (!(VolatileOut.isSetField.compareAndSet(this :VolatileOut[_], false, true) :Boolean))
			illegalState_!("Out value already initialized: " + x  + ".")
		x = newValue
	}
	override def opt         :Opt[T] = if (isSet) One(x) else None
	override def unsure      :Unsure[T] = if (isSet) Sure(x) else Missing

	private[vars] override def isSpecialized :Boolean = getClass != classOf[VolatileOut[Any]]
}




/** An emulation of an 'out' parameter of a procedure,
  * creating uninitialized [[net.noresttherein.sugar.vars.VolatileOut VolatileOut]] instances.
  * This [[net.noresttherein.sugar.vars.InOut InOut]] extension allows the value of the variable to be set at most once.
  */
@SerialVersionUID(Ver)
case object VolatileOut {
	/** Create an uninitialized [[net.noresttherein.sugar.vars.InOut InOut]] which cannot be accessed before
	  * its value is explicitly initialized, and which value can be set at most once.
	  */
	def apply[@specialized(SpecializedVars) T]() = new VolatileOut[T]

//	implicit def outOrdering[T :Ordering] :Ordering[Out[T]] = Val.valOrdering

	private val isSetField = MethodHandles.lookup().findVarHandle(
		classOf[VolatileOut[Any]], "net$noresttherein$sugar$vars$VolatileOut$$isSet", java.lang.Boolean.TYPE
	)
}
