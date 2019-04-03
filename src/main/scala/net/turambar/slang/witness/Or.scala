package net.turambar.slang.witness

import scala.annotation.implicitNotFound


/** An implicit evidence class witnessing that there is an implicit value available for `E1`, `E2` or both.
  * @author Marcin Mościcki marcin@moscicki.net
  */
@implicitNotFound("Can't witness (${E1} Or ${E2}): no implicit value available for either type parameter.")
final class Or[E1, E2] private[witness](
	/** Implicit value available for `E1`, if present in the referencing context. */ val first :Option[E1],
	/** Implicit value available for `E2`, if present in the referencing context. */ val second :Option[E2]
)


sealed abstract class IndividualOrEvidence {
	implicit def witnessFirst[E1, E2](implicit e1 :E1): E1 Or E2 = new Or(Some(e1), None)
	implicit def witnessSecond[E1, E2](implicit e2 :E2): E1 Or E2 = new Or(None, Some(e2))
}

object Or extends IndividualOrEvidence {

	implicit def witnessBoth[E1, E2](implicit e1 :E1, e2 :E2): E1 Or E2 = new Or(Some(e1), Some(e2))


	object First {
		def apply[E1](implicit ev :E1 Or _) :Option[E1] = ev.first

		def unapply[E1, E2](or :E1 Or E2) :Option[E1] = or.first
	}

	object Second {
		def apply[E2](implicit ev :_ Or E2) :Option[E2] = ev.second

		def unapply[E1, E2](or :E1 Or E2) :Option[E2] = or.second
	}

	object Both {
		def unapply[E1, E2](or :E1 Or E2) :Option[(E1, E2)] = (or.first, or.second) match {
			case (Some(first), Some(second)) => Some(first, second)
			case _ => None
		}
	}
}
