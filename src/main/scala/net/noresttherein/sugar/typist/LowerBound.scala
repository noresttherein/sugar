package net.noresttherein.sugar.typist

import scala.annotation.implicitNotFound




/** Witnesses that `L` is a subtype of both `X` and `Y`. Implicit evidence exists which, when left without bounds for `L`,
  * calculates the ''greatest lower bound'' type for `X` and `Y`.
  * @param _1 evidence that `L <: X`
  * @param _2 evidence that `L <: Y`
  */
@implicitNotFound("Cannot calculate lower bound for types ${X} and ${Y} (or prove it to be ${L}).")
final class LowerBound[X, Y, L] private[typist] (val _1 :L <:< X, val _2 :L <:< Y) {
	type T = L
	@inline def left(l :L) :X = l.asInstanceOf[X]
	@inline def right(l :L) :Y = l.asInstanceOf[Y]
}


private[typist] sealed abstract class ProperLowerBoundImplicits private[typist] {
	@inline implicit final def properLowerBound[X >: L, Y >: L, L] :LowerBound[X, Y, L] =
		instance.asInstanceOf[LowerBound[X, Y, L]]

	protected[typist] final val instance = new LowerBound[Any, Any, Any](implicitly[Any <:< Any], implicitly[Any <:< Any])
}

private[typist] sealed abstract class SelfLowerBoundImplicits private[typist] extends ProperLowerBoundImplicits {
	@inline implicit final def leftLowerBound[X <: Y, Y] :LowerBound[X, Y, X] = instance.asInstanceOf[LowerBound[X, Y, X]]
	@inline implicit final def rightLowerBound[X, Y <: X] :LowerBound[X, Y, Y] = instance.asInstanceOf[LowerBound[X, Y, Y]]
}


@SerialVersionUID(Ver)
object LowerBound extends SelfLowerBoundImplicits {
	/** Calculates explicitly the greatest lower bound of its type parameters via its `apply()` method. */
	final class Binder[X, Y] private[LowerBound] {
		@inline def apply[L]()(implicit l :LowerBound[X, Y, L]) :LowerBound[X, Y, L] = l
	}

	/** Explicitly summon a [[net.noresttherein.sugar.typist.LowerBound LowerBound]] instance for `X` and `Y`
	  * to calculate their greatest lower bound. This is an indirect operation returning
	  * a [[net.noresttherein.sugar.typist.LowerBound.Binder Binder]] which will return the desired evidence via its
	  * parameterless `apply()` method: `LowerBound[Int, String]()`.
	  */
	@inline def apply[X, Y] :Binder[X, Y] = binder.asInstanceOf[Binder[X, Y]]

	private[this] val binder = new Binder[Any, Any]


	@inline implicit final def identityLowerBound[X] :LowerBound[X, X, X] = instance.asInstanceOf[LowerBound[X, X, X]]
}
