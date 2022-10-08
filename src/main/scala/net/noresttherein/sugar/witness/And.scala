package net.noresttherein.sugar.witness

import scala.annotation.implicitNotFound



/** Witnesses the existence of both `X` and `Y` implicit values. Declaring an implicit parameter
  * `(implicit both :X And Y)` is equivalent to declaring `(implicit x :X, y :Y)`, but the conjunction can still
  * be useful for creating more complex boolean formulas with implicit parameters as predicates.
  * Additionally, where one would normally introduce a new evidence type derived from existence of
  * some other implicit values, one can now simply declare a type alias for (possibly nested)
  * `And` values, saving the coding of a class, companion object and implicit values altogether.
  *
  * @author Marcin Mościcki
  */
@implicitNotFound("Can't witness (${X} And ${Y}): missing implicit value for one of these types.")
class And[X, Y] private[witness] (val first :X, val second :Y) {
	def isEmpty = false
	def get :(X, Y) = first -> second
}



@SerialVersionUID(ver)
object And {

	@inline implicit def both[X, Y](implicit x :X, y :Y) :X And Y = new And(x, y)

	@inline def unaply[X, Y](and :X And Y) :X And Y = and
}


