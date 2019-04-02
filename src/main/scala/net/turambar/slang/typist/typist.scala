package net.turambar.slang

/**
  * @author Marcin Mościcki
  */
package object typist {

	/** Witnesses that `U` is a supertype of both `X` and `Y`. Implicit evidence exists which, when left without bounds for `U`,
	  * calculates the ''least upper bound'' type for `X` and `Y`.
	  * @param _1 evidence that `X <: U`
	  * @param _2 evidence that `Y <: U`
	  */
	final class UpperBound[X, Y, U] private[typist] (val _1 :X<:<U, val _2 :Y<:<U) {
		type T = U
		/** Identity conversion from `X` to its super type `U`. */
		@inline def left(x :X) :U = x.asInstanceOf[U]
		/** Identity conversion from `Y` to its super type `U`. */
		@inline def right(y :Y) :U = y.asInstanceOf[U]
	}



	sealed abstract class ProperUpperBound private[typist] {
		@inline implicit final def properUpperBound[X<:U, Y<:U, U] :UpperBound[X, Y, U] = instance.asInstanceOf[UpperBound[X, Y, U]]

		final protected[this] val instance = new UpperBound[Any, Any, Any](implicitly[Any<:<Any], implicitly[Any<:<Any])
	}

	sealed abstract class SelfUpperBound private[typist] extends ProperUpperBound {
		@inline implicit final def leftUpperBound[X, Y<:X] :UpperBound[X, Y, X] = instance.asInstanceOf[UpperBound[X, Y, X]]
		@inline implicit final def rightUpperBound[X<:Y, Y] :UpperBound[X, Y, Y] = instance.asInstanceOf[UpperBound[X, Y, Y]]
	}


	object UpperBound extends SelfUpperBound {
		/** Calculates explicitly the least upper bound of its type parameters via its `apply()` method. */
		final class Binder[X, Y] private[UpperBound] {
			@inline def apply[U]()(implicit u :UpperBound[X, Y, U]) :UpperBound[X, Y, U] = u
		}

		/** Explicitly summon an [[UpperBound]] instance for `X` and `Y` to calculate their least upper bound.
		  * This is an indirect operation returning a [[Binder]] which will return the desired evidence via its
		  * parameterless `apply()` method: `UpperBound[Int, String]()`.
		  */
		@inline def apply[X, Y] :Binder[X, Y] = binder.asInstanceOf[Binder[X, Y]]

		private[this] val binder = new Binder[Any, Any]


		@inline implicit final def identityUpperBound[X] :UpperBound[X, X, X] = leftUpperBound[X, X].asInstanceOf[UpperBound[X, X, X]]

	}



	/** Witnesses that `L` is a subtype of both `X` and `Y`. Implicit evidence exists which, when left without bounds for `L`,
	  * calculates the ''greatest lower bound'' type for `X` and `Y`.
	  * @param _1 evidence that `L <: X`
	  * @param _2 evidence that `L <: Y`
	  */
	final class LowerBound[X, Y, L] private[typist] (val _1 :L<:<X, val _2 :L<:<Y) {
		type T = L
		@inline def left(l :L) :X = l.asInstanceOf[X]
		@inline def right(l :L) :Y = l.asInstanceOf[Y]
	}


	sealed abstract class ProperLowerBound private[typist] {
		@inline implicit def properLowerBound[X>:L, Y>:L, L] :LowerBound[X, Y, L] = instance.asInstanceOf[LowerBound[X, Y, L]]

		final protected[this] val instance = new LowerBound[Any, Any, Any](implicitly[Any<:<Any], implicitly[Any<:<Any])
	}

	sealed abstract class SelfLowerBound private[typist] extends ProperLowerBound {
		@inline implicit final def leftLowerBound[X, Y](implicit ev :X<:<Y) :LowerBound[X, Y, X] = instance.asInstanceOf[LowerBound[X, Y, X]]
		@inline implicit final def rightLowerBound[X, Y](implicit ev :Y<:<X) :LowerBound[X, Y, Y] = instance.asInstanceOf[LowerBound[X, Y, Y]]


	}

	object LowerBound extends SelfLowerBound {
		/** Calculates explicitly the greatest lower bound of its type parameters via its `apply()` method. */
		final class Binder[X, Y] private[LowerBound] {
			@inline def apply[L]()(implicit l :LowerBound[X, Y, L]) :LowerBound[X, Y, L] = l
		}

		/** Explicitly summon a [[LowerBound]] instance for `X` and `Y` to calculate their greatest lower bound.
		  * This is an indirect operation returning a [[Binder]] which will return the desired evidence via its
		  * parameterless `apply()` method: `LowerBound[Int, String]()`.
		  */
		@inline def apply[X, Y] :Binder[X, Y] = binder.asInstanceOf[Binder[X, Y]]

		private[this] val binder = new Binder[Any, Any]


		@inline implicit final def identityLowerBound[X, Y](implicit ev :Y=:=X) :LowerBound[X, Y, X] = instance.asInstanceOf[LowerBound[X, Y, X]]
	}


}

