package net.noresttherein.sugar

import net.noresttherein.sugar


package object funny { //consider: renaming to funsies

	final class ReturnTypeOf[F <: Nothing => Any] private { type Return }

	@SerialVersionUID(Ver)
	object ReturnTypeOf {
		implicit def returnTypeOf[X, Y] :ReturnTypeOf[X => Y] { type Return = Y } =
			instance.asInstanceOf[ReturnTypeOf[X => Y] { type Return = Y }]
		private[this] val instance = new ReturnTypeOf[Nothing]
	}

	private[funny] final val Ver = sugar.Ver
}
