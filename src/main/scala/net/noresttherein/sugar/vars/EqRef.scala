package net.noresttherein.sugar.vars

import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/** A light wrapper over a value of any type defining equality as referential equality of underlying values
  * (`this.get eq that.get`). Typically used as `Map` keys.
  * @see [[net.noresttherein.sugar.collection.EqSet]]
  * @see [[net.noresttherein.sugar.collection.EqMap]]
  * @see [[net.noresttherein.sugar.collection.MutableEqSet]]
  * @see [[net.noresttherein.sugar.collection.MutableEqMap]]
  */
final class EqRef[+T] private (override val get :T) extends Val[T] with Serializable {
	override def isDefined :Boolean = true

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :EqRef[_] => get.asInstanceOf[AnyRef] eq other.get.asInstanceOf[AnyRef]
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[EqRef[_]]
	override def hashCode :Int = System.identityHashCode(get)

	override def toString :String = "{" + get + "}@" + Integer.toHexString(hashCode)
}




/** A factory of light wrappers defining equality as referential equality (`eq`) of their underlying values. */
object EqRef {
	/** Creates an immutable wrapper over `value` defining equality as `value eq _`. */
	def apply[T](value :T) :EqRef[T] = new EqRef(value)

	def unapply[T](ref :Ref[T]) :Opt[T] = ref match {
		case ref :EqRef[T] => Got(ref.get)
		case _ => Lack
	}
}
