package net.noresttherein.sugar.witness

import scala.annotation.implicitNotFound


/** An implicit evidence class witnessing that there is an implicit value available for `E1` or `E2`, but not both.
  * @author Marcin Mościcki marcin@moscicki.net
  */
@implicitNotFound("Can't witness (${E1} Xor ${E2}).")
final class Xor[E1, E2] private[witness](val which :Either[E1, E2]) extends AnyVal






object Xor {

	@inline implicit def witnessFirst[E1, E2](implicit e1 :E1): E1 Xor E2 = new Xor(Left(e1))
	@inline implicit def witnessSecond[E1, E2](implicit e2 :E2): E1 Xor E2 = new Xor(Right(e2))


	@inline def unapply[E1, E2](xor :Xor[E1, E2]) :Some[Either[E1, E2]] = Some(xor.which)
}



