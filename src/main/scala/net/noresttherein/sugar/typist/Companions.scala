package net.noresttherein.sugar.typist

import net.noresttherein.sugar.time.TimeProjector




/** A marker interface mixed in by a companion object to type/class `T`, used to associate the object's singleton type
  * with the companion class for the purpose of [[net.noresttherein.sugar.typist.Companions Companions]] type class.
  * Allows generic code to infer type `T` from a value of `CompanionObject[T]`, as a way of providing a value argument
  * instead of a type argument.
  * @tparam T the companion class/type to this object.
  */
trait CompanionObject[T]



/** A type class associating a type `T` with the singleton type of its companion object `O`. Offers no API.
  * It allows to specify a type argument by providing the type's companion object as a value argument instead,
  * which may be useful when creating DSLs. Implicit instances exist for built in value types and commonly used types
  * from the Scala standard library, and for collection companions (derived from `IterableFactory[O]`)
  * @tparam O the singleton type of a companion object to type `T`.
  * @tparam T a type with a companion object.
  */
class Companions[O, T]

object Companions {
	implicit val Long       :Companions[scala.Long.type, Long]       = new Companions
	implicit val Int        :Companions[scala.Int.type, Int]         = new Companions
	implicit val Short      :Companions[scala.Short.type, Short]     = new Companions
	implicit val Char       :Companions[scala.Char.type, Char]       = new Companions
	implicit val Byte       :Companions[scala.Byte.type, Byte]       = new Companions
	implicit val Float      :Companions[scala.Float.type, Float]     = new Companions
	implicit val Double     :Companions[scala.Double.type, Double]   = new Companions
	implicit val Boolean    :Companions[scala.Boolean.type, Boolean] = new Companions
	implicit val BigInt     :Companions[scala.BigInt.type, BigInt]   = new Companions
	implicit val bigDecimal :Companions[BigDecimal.type, BigDecimal] = new Companions

	implicit def timeProjection[O <: TimeProjector { type Phase = T }, T]
	                           (implicit inferPhase :O <:< (TimeProjector { type Projection = T })) :Companions[O, T] =
		instance.asInstanceOf[Companions[O, T]]

	implicit def companionObject[O <: CompanionObject[T], T]
	                            (implicit inferType :O <:< CompanionObject[T]) :Companions[O, T] =
		instance.asInstanceOf[Companions[O, T]]

	private[this] val instance = new Companions
}
