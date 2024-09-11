package net.noresttherein.sugar

import net.noresttherein.sugar



package object funny { //consider: renaming to funsies
	private[funny] final val Ver = sugar.Ver

	type Curry2[R[_, _]] = { type A1[X] = { type A2[Y] = R[X, Y] } }

	type Curry3[R[_, _, _]] = { type A1[X] = { type A2[Y] = { type A3[Z] = R[X, Y, Z] } } }
}



package funny {

	final class ReturnTypeOf[F <: Nothing => Any] private { type Return }

	@SerialVersionUID(Ver)
	object ReturnTypeOf {
		implicit def returnTypeOf[X, Y] :ReturnTypeOf[X => Y] { type Return = Y } =
			instance.asInstanceOf[ReturnTypeOf[X => Y] { type Return = Y }]
		private[this] val instance = new ReturnTypeOf[Nothing]
	}
}
