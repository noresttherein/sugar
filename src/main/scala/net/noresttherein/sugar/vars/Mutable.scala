package net.noresttherein.sugar.vars

import net.noresttherein.sugar.extensions.classNameMethods
import net.noresttherein.sugar.vars.InOut.{InOutOrdering, SpecializedVars}
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.witness.DefaultValue




/** A marker interface of `InOut` variables which contain a value at all times.
  * No method of this object, including those inherited and those defined only in subclasses,
  * will throw a [[NoSuchElementException]]. `Mutable` instances do not have
  * [[net.noresttherein.sugar.vars.Mutable.const const]] values,
  * and their default [[net.noresttherein.sugar.vars.Mutable.get get]] method
  * returns their current [[net.noresttherein.sugar.vars.Mutable.value value]].
  * @define Ref `Mutable`
  * @author Marcin Mościcki
  */
trait Mutable[@specialized(SpecializedVars) T] extends InOut[T] {
	/** Returns `false`. */
	override def isFinal        :Boolean = false
	/** Returns `false`. */
	final override def isEmpty  :Boolean = false
//	/** Returns `true`. */
//	final override def nonEmpty :Boolean = true
	/** Returns `false`. */
	override def isFinalizable  :Boolean = false
	/** Returns `false`. */
	override def isConst        :Boolean = false
	/** Returns `true`. */
	override def isDefined      :Boolean = true
	/** Returns `true`. */
	override def isDefinite     :Boolean = true
	/** Same as [[net.noresttherein.sugar.vars.Mutable.value value]]. */
	override def get   :T = value
	/** Throws an [[UnsupportedOperationException]]. */
	override def const :T = throw new UnsupportedOperationException(this.localClassName + ".const")
	/** Same as [[net.noresttherein.sugar.vars.Mutable.get get]]. */
	@inline final override def apply() :T = get
	/** Returns `Some(`[[net.noresttherein.sugar.vars.Mutable.value value]]`)`. */
	override def option      :Some[T] = Some(value)
	/** Same as [[net.noresttherein.sugar.vars.Mutable.option option]]. */
	override def toOption    :Some[T] = option
	/** Returns `None`. */
	override def constOption :Option[T] = None
	/** Returns `Got(`[[net.noresttherein.sugar.vars.Mutable.value value]]`)`. */
	override def opt         :Got[T]  = Got(value)
	/** Same as [[net.noresttherein.sugar.vars.Mutable.opt opt]]. */
	override def toOpt       :Got[T]  = opt
	/** Returns [[net.noresttherein.sugar.vars.Opt.Lack Lack]]. */
	override def constOpt    :Opt[T]  = Lack
	/** Returns `Sure(`[[net.noresttherein.sugar.vars.Mutable.value value]]`)`. */
	override def unsure      :Sure[T] = Sure(value)
	/** Same as [[net.noresttherein.sugar.vars.Mutable.unsure unsure]]. */
	override def toUnsure    :Sure[T] = unsure
	/** Returns [[net.noresttherein.sugar.vars.Missing Missing]]. */
	override def constUnsure :Unsure[T] = Missing

	private def specializedEquals(left :Mutable[T], right :Any) :Boolean = right match {
		case self :AnyRef if left eq self => true
		case other :Mutable[_] if other canEqual this =>
			if (left.getClass == right.getClass)
				left.value == other.asInstanceOf[Mutable[T]].value
			else
				left.value == other.value
		case other :Ref[_] if other canEqual this => other.toOpt match {
			case Got(v) => value == v
			case _ => false
		}
	}
	override def equals(that :Any) :Boolean = specializedEquals(this, that)
	override def hashCode :Int = value.hashCode

	override def mkString(prefix :String) :String = prefix + "(" + value + ")"
	override def toString :String = String.valueOf(value)
}




@SerialVersionUID(Ver)
object Mutable {
	/** Creates a [[net.noresttherein.sugar.vars.Var Var]] holding the given value. */
	@inline def apply[@specialized(SpecializedVars) T](value :T) :Mutable[T] = Var(value)

	/** Creates a [[net.noresttherein.sugar.vars.Var Var]] initialized with the default value of `T`, as specified
	  * by [[net.noresttherein.sugar.witness.DefaultValue DefaultValue]] type class.
	  */
	@inline def apply[@specialized(SpecializedVars) T :DefaultValue] :Mutable[T] = Var[T]

	implicit def MutableOrdering[V[X] <: Mutable[X], T :Ordering] :Ordering[V[T]] = new InOutOrdering[V, T]
}
