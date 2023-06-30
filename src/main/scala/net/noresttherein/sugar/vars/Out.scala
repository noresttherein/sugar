package net.noresttherein.sugar.vars

import java.lang.invoke.MethodHandles

import scala.annotation.nowarn

import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.vars.Ref.undefined




/** An emulation of an 'out' parameter of a procedure. It is a wrapper over a `@specialized var` of type `T`
  * which will throw an [[NoSuchElementException]] if accessed before the value is set with one of the methods
  * `this.value = x` or `this := x`, as well as when a second attempt to set its value is made which would
  * override the previously set value. This class is thread safe with ''volatile'' semantics.
  * @define Ref `Out`
  * @author Marcin Mościcki
  */ //todo: not thread safe LocalOut version
@SerialVersionUID(Ver) //consider: does it make sense for it to be Serializable?
sealed class Out[@specialized(SpecializedVars) T] private[vars] extends InOut[T] with Val[T] with Serializable {
	@scala.volatile private[this] var x :T = _
	@scala.volatile @nowarn private[this] var isSet = false //set through InOut.isSetField VarHandle

	override def isEmpty       :Boolean = !isSet
	override def isFinalizable :Boolean = isSet
	override def isConst       :Boolean = !isSet
	override def isDefined     :Boolean = isSet
	override def isDefinite    :Boolean = isSet

	final override def value :T = {
		if (!isSet)
			throw new NoSuchElementException("Out value not set.")
		x
	}
	@throws[IllegalStateException]("if this instance is already initialized.")
	final override def value_=(newValue :T) :Unit = {
		if (!(Out.isSetField.compareAndSet(this :Out[_], false, true) :Boolean))
			throw new IllegalStateException("Out value already initialized: " + x  + ".")
		x = newValue
	}
	@inline final override def get :T = value

	override def const :T =
		if (isSet) x else throw new UnsupportedOperationException("Out.const")

	override def option      :Option[T] = if (isSet) Some(x) else None
	override def toOption    :Option[T] = option
	override def constOption :Option[T] = option
	override def opt         :Opt[T] = if (isSet) Got(x) else Lack
	override def toOpt       :Opt[T] = opt
	override def constOpt    :Opt[T] = opt
	override def unsure      :Unsure[T] = if (isSet) Sure(x) else Missing
	override def toUnsure    :Unsure[T] = unsure
	override def constUnsure :Unsure[T] = unsure

	private[vars] override def isSpecialized :Boolean = getClass != classOf[Out[Any]]

	override def toString :String = if (isSet) String.valueOf(value) else undefined.toString
}




/** An emulation of an 'out' parameter of a procedure, creating uninitialized [[net.noresttherein.sugar.vars.Out Out]]
  * instances. This [[net.noresttherein.sugar.vars.InOut InOut]] extension allows the value of the variable to be set
  * at most once.
  */
@SerialVersionUID(Ver)
object Out {
	/** Create an uninitialized [[net.noresttherein.sugar.vars.InOut InOut]] which cannot be accessed before
	  * its value is explicitly initialized, and which value can be set at most once.
	  */
	def apply[@specialized(SpecializedVars) T]() = new Out[T]

	implicit def outOrdering[T :Ordering] :Ordering[Out[T]] = Val.valOrdering

	private val isSetField = MethodHandles.lookup().findVarHandle(
		classOf[Out[Any]], "net$noresttherein$sugar$vars$Out$$isSet", java.lang.Boolean.TYPE
	)
}
