package net.noresttherein.sugar.vars

import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Opt.Got
import net.noresttherein.sugar.vars.Ref.Undefined




/** The root type of [[net.noresttherein.sugar.vars.InOut InOut]] and [[net.noresttherein.sugar.vars.Val Val]]
  * class hierarchy of value wrappers with various implementations of referential variables, lazy values and others.
  */
trait Ref[@specialized(SpecializedVars) +T] extends Any with Equals {

	/** Checks if this object currently contains a value. In a non concurrent (or synchronized) context,
	  * if `true`, then [[net.noresttherein.sugar.vars.Ref.get get]] will not block and return a value
	  * without throwing an exception. Depending on the implementation, this information
	  * may be already incorrect when returned to the application. Unless the specific type of this `Ref` is known
	  * and its contract guarantees that any particular answer is guaranteed to remain true, prefer polling
	  * with [[net.noresttherein.sugar.vars.Ref.asOption asOption]]/[[net.noresttherein.sugar.vars.Ref.opt opt]].
	  */
	def isDefined :Boolean// = opt.isDefined

	/** The wrapped value, if available. Depending on the actual implementation,
	  * it can throw a [[NoSuchElementException]] or block.
	  * This is an alias for [[net.noresttherein.sugar.vars.Ref.get get]].
	  */
	@inline final def apply() :T = get

	/** The wrapped value, if available. Depending on the actual implementation,
	  * it can throw a [[NoSuchElementException]]. It is equivalent
	  * to [[net.noresttherein.sugar.vars.Ref!.apply apply]]`()`, but can be more readable if the returned value
	  * is a function (or other object defining an `apply` method) which is being immediately applied.
	  * Some subclasses define also a `value` property which also accesses the value of this instance,
	  * but its semantics may differ, depending on the exact implementation.
	  */
	def get :T

	/** The value of this instance, if it is available. Lazily initialized objects (containing their initializers)
	  * will proceed with the initialization if necessary, but subclasses which require external setting of the value
	  * will return [[None]].
	  * This method can block to wait for another thread only if initialization is currently in progress/the value
	  * is currently mutated, but not if no other thread currently accesses this variable.
	  */
	def ? :Option[T] = opt.asOption

	/** The value of this instance, if it is available. This is an alias for [[net.noresttherein.sugar.vars.Ref.? ?]]. */
	@inline final def asOption :Option[T] = ? //not toOption, because Box defined asOption_= (better than toOption_=)

	/** The value of this instance, if it is available. Lazily initialized objects (containing their initializers)
	  * will proceed with the initialization if necessary, but subclasses which require external setting of the value
	  * will return [[net.noresttherein.sugar.vars.Opt.Lack Lack]].
	  * This method can block to wait for another thread only if initialization is currently in progress/the value
	  * is currently mutated, but not if no other thread currently accesses this variable.
	  */
	def opt :Opt[T]

	/** The value of this instance, if it is available, as an instance of a specialized, option-like `Unsure`.
	  * Lazily initialized objects (containing their initializers) will proceed with the initialization if necessary,
	  * but subclasses which require external setting of the value will return [[net.noresttherein.sugar.vars.Blank Blank]].
	  * This method can block to wait for another thread only if initialization is currently in progress/the value
	  * is currently mutated, but not if no other thread currently accesses this variable.
	  */
	def unsure :Unsure[T] = opt.unsure

	/** True if the content type is known to be a value type and the class is specialized for it.
	  * Signifies that usage of [[net.noresttherein.sugar.vars.Unsure Unsure]] is preferable
	  * over [[net.noresttherein.sugar.vars.Opt Opt]]. Should not be relied upon for anything critical
	  * and code should work correctly if this method returns a false result, in particular
	  * a false negative (the default return value being `false`).
	  */
	private[vars] def isSpecialized :Boolean = false

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Ref[_]]

	override def toString :String = opt match {
		case Got(v) => String.valueOf(v)
		case _ => Undefined.toString
	}
}



object Ref {
	@inline def unapply[T](ref :Ref[T]) :Opt[T] = ref.opt


	implicit def RefPartialOrdering[V[X] <: Ref[X], T :PartialOrdering] :PartialOrdering[V[T]] =
		new RefPartialOrdering[V, T]

	private[vars] class RefPartialOrdering[V[X] <: Ref[X], T](implicit content :PartialOrdering[T])
		extends PartialOrdering[V[T]]
	{
		private[this] final val ComparableEquiv = Some(0)

		override def tryCompare(x :V[T], y :V[T]) :Option[Int] =
			if (x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef])
				ComparableEquiv
			else if (x.isSpecialized && y.isSpecialized)
				(x.unsure, y.unsure) match {
					case (Sure(vx), Sure(vy)) => content.tryCompare(vx, vy)
					case _ => None
				}
			else
				(x.opt, y.opt) match {
					case (Got(vx), Got(vy)) =>  content.tryCompare(vx, vy)
					case _ => None
				}

		override def lteq(x :V[T], y :V[T]) :Boolean =
			if (x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef])
				true
			else if (x.isSpecialized && y.isSpecialized)
				(x.unsure, y.unsure) match {
					case (Sure(vx), Sure(vy)) => content.lteq(vx, vy)
					case _ => false
				}
			else
				(x.opt, y.opt) match {
					case (Got(vx), Got(vy)) => content.lteq(vx, vy)
					case _ => false
				}
	}

	private[vars] class RefOrdering[V[X] <: Ref[X], T](implicit content :Ordering[T]) extends Ordering[V[T]] {
		override def compare(x :V[T], y :V[T]) :Int = content.compare(x.get, y.get)
	}

	private[vars] abstract class RefNumeric[V[X] <: Ref[X], T](implicit content :Numeric[T])
		extends RefOrdering[V, T] with Numeric[V[T]]
	{
		protected def inner :Numeric[T] = content

		private[this] val + = content.plus _
		private[this] val - = content.minus _
		private[this] val * = content.times _
		private[this] val ~ = content.negate _
		private[this] val V = apply _

		override def plus(x :V[T], y :V[T]) :V[T] = fmap(x, y)(+)
		override def minus(x :V[T], y :V[T]) :V[T] = fmap(x, y)(-)
		override def times(x :V[T], y :V[T]) :V[T] = fmap(x, y)(*)

		override def negate(x :V[T]) :V[T] = map(x)(~)

		override def fromInt(x :Int) :V[T] = apply(content.fromInt(x))
		override def parseString(str :String) :Option[V[T]] = content.parseString(str).map(V)
		override def toInt(x :V[T]) :Int = content.toInt(x.get)
		override def toLong(x :V[T]) :Long = content.toLong(x.get)
		override def toFloat(x :V[T]) :Float = content.toFloat(x.get)
		override def toDouble(x :V[T]) :Double = content.toDouble(x.get)

		protected def fmap(x :V[T], y :V[T])(op :(T, T) => T) :V[T] = apply(op(x.get, y.get))
		protected def map(x :V[T])(f :T => T) :V[T] = apply(f(x.get))
		protected def apply(x :T) :V[T]
	}

	abstract class RefIntegral[V[X] <: Ref[X], T](implicit content :Integral[T])
		extends RefNumeric[V, T] with Integral[V[T]]
	{
		protected override def inner :Integral[T] = content
		private[this] val / = content.quot _
		private[this] val % = content.rem _

		override def quot(x :V[T], y :V[T]) :V[T] = fmap(x, y)(/)
		override def rem(x :V[T], y :V[T]) :V[T] = fmap(x, y)(%)
	}

	abstract class RefFractional[V[X] <: Ref[X], T](implicit content :Fractional[T])
		extends RefNumeric[V, T] with Fractional[V[T]]
	{
		protected override def inner :Fractional[T] = content
		private[this] val / = content.div _

		override def div(x :V[T], y :V[T]) :V[T] = fmap(x, y)(/)
	}



	/** A marker object used by some implementations to signify that a reference has no actual value. */
	private[vars] object Undefined {
		override def toString = "<undefined>"
	}

}
