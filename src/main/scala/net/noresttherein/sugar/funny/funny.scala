package net.noresttherein.sugar




package object funny {

	final class ReturnTypeOf[F <: Nothing => Any] private { type Return }

	@SerialVersionUID(Ver)
	object ReturnTypeOf {
		implicit def returnTypeOf[X, Y] :ReturnTypeOf[X => Y] { type Return = Y } =
			instance.asInstanceOf[ReturnTypeOf[X => Y] { type Return = Y }]
		private[this] val instance = new ReturnTypeOf[Nothing]
	}

	private[funny] final val Ver = 1L
}
