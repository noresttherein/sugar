package net.noresttherein.sugar.typist

import scala.annotation.implicitNotFound


/** Witnesses that `U` is a supertype of both `X` and `Y`. Implicit evidence exists which, when left without bounds for `U`,
  * calculates the ''least upper bound'' type for `X` and `Y`.
  * @param _1 evidence that `X <: U`
  * @param _2 evidence that `Y <: U`
  */
@implicitNotFound("Cannot calculate upper bound for types ${X} and ${Y} (or prove it to be ${U}).")
final class UpperBound[X, Y, U] private[typist] (val _1 :X<=:U, val _2 :Y<=:U) {
	type T = U
	/** Identity conversion from `X` to its super type `U`. */
	@inline def left(x :X) :U = x.asInstanceOf[U]
	/** Identity conversion from `Y` to its super type `U`. */
	@inline def right(y :Y) :U = y.asInstanceOf[U]
}



sealed abstract class ProperUpperBoundImplicits private[typist] {
	@inline implicit final def properUpperBound[X<:U, Y<:U, U] :UpperBound[X, Y, U] = instance.asInstanceOf[UpperBound[X, Y, U]]

	final protected[typist] val instance = new UpperBound[Any, Any, Any](implicitly[Any<=:Any], implicitly[Any<=:Any])
}

sealed abstract class SelfUpperBoundImplicits private[typist] extends ProperUpperBoundImplicits {
	@inline implicit final def leftUpperBound[X, Y<:X] :UpperBound[X, Y, X] = instance.asInstanceOf[UpperBound[X, Y, X]]
	@inline implicit final def rightUpperBound[X<:Y, Y] :UpperBound[X, Y, Y] = instance.asInstanceOf[UpperBound[X, Y, Y]]
}


object UpperBound extends SelfUpperBoundImplicits {
	/** Calculates explicitly the least upper bound of its type parameters via its `apply()` method. */
	final class Binder[X, Y] private[UpperBound] {
		@inline def apply[U]()(implicit u :UpperBound[X, Y, U]) :UpperBound[X, Y, U] = u
	}

	/** Explicitly summon an [[net.noresttherein.sugar.typist.UpperBound UpperBound]] instance for `X` and `Y`
	  * to calculate their least upper bound. This is an indirect operation returning
	  * a [[net.noresttherein.sugar.typist.UpperBound.Binder Binder]] which will return the desired evidence via its
	  * parameterless `apply()` method: `UpperBound[Int, String]()`.
	  */
	@inline def apply[X, Y] :Binder[X, Y] = binder.asInstanceOf[Binder[X, Y]]

	private[this] val binder = new Binder[Any, Any]


	@inline implicit final def identityUpperBound[X] :UpperBound[X, X, X] = leftUpperBound[X, X]

}


