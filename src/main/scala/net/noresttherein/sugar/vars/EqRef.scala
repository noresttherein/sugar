package net.noresttherein.sugar.vars

import net.noresttherein.sugar.vars.Maybe.{No, Yes}
import net.noresttherein.sugar.vars.Opt.One
import net.noresttherein.sugar.vars.Ref.FinalRef




/** A light wrapper over a value of any type defining equality as referential equality of underlying values
  * (`this.get eq that.get`). Typically used as `Map` keys.
  * @see [[net.noresttherein.sugar.collections.EqSet]]
  * @see [[net.noresttherein.sugar.collections.EqMap]]
  * @see [[net.noresttherein.sugar.collections.MutableEqSet]]
  * @see [[net.noresttherein.sugar.collections.MutableEqMap]]
  * @define Ref `EqRef`
  * @define ref identity reference
  */
@SerialVersionUID(Ver)
final class EqRef[+T] private (x :T) extends Ref[T] with FinalRef[T] with Serializable {
	override def isEmpty :Boolean = false
	override def get     :T = x
	override def option  :Option[T] = Some(x)
	override def opt     :Opt[T] = One(x)
	override def unsure  :Unsure[T] = Sure(x)

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :EqRef[_] => get.asInstanceOf[AnyRef] eq other.get.asInstanceOf[AnyRef]
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[EqRef[_]]
	override def hashCode :Int = System.identityHashCode(get)

	override def mkString(prefix :String) :String = prefix + "(" + get + ")"
	override def toString :String = "{" + get + "}@" + Integer.toHexString(hashCode)
}




/** A factory of light wrappers defining equality as referential equality (`eq`) of their underlying values. */
@SerialVersionUID(Ver)
case object EqRef {
	/** Creates an immutable wrapper over `value` defining equality as `value eq _`. */
	def apply[T](value :T) :EqRef[T] = new EqRef(value)

	def unapply[T](ref :Ref[T]) :Maybe[T] = ref match {
		case ref :EqRef[T] => Yes(ref.get)
		case _ => No
	}
}
