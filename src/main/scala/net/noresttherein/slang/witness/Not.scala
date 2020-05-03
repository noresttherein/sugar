package net.noresttherein.slang.witness

import scala.annotation.implicitNotFound


/** An evidence class witnessing that no implicit value of type `P` is present.
  * For any type `P` and any scope, an implicit value of `Not[P]` is available ''iff'' no implicit value for `P` exists
  * in that scope.
  * @author Marcin Mościcki marcin@moscicki.net
  */
@implicitNotFound("Cannot prove that Not[${P}] as evidence for ${P} exists.")
final class Not[P] private ()



object Not {
	private[this] val not = new Not[Any]

	@inline implicit def default[P] :Not[P] = not.asInstanceOf[Not[P]]

	@inline implicit def evidenceFound[P](implicit ev :P) :Not[P] = default[P]

}

