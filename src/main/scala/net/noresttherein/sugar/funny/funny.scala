package net.noresttherein.sugar




package object funny {
	/** Transform the given partial function into an equivalent function returning its result as an option. */
	def lift[X, Y](f :PartialFunction[X, Y]) :X => Option[Y] = f.lift

	/** Transform the given function returning an option into an equivalent partial function defined for arguments
	  * for which the argument function returns `Some`. */
	def lower[X, Y](f :X => Option[Y]) :PartialFunction[X, Y] = new PartialFunctionOptionAdapter(f)

	/** Extends functions in the form `A=>Option[B]` with a method `lower`, which transforms it into a partial function. */
	implicit class lowerToPartialFunction[A, B](private val f :A => Option[B]) extends AnyVal {
		def lower :PartialFunction[A, B] = new PartialFunctionOptionAdapter(f)
	}

	@SerialVersionUID(ver)
	class PartialFunctionOptionAdapter[A, B](override final val lift :A => Option[B]) extends PartialFunction[A, B] {
		override def apply(v1: A): B = lift(v1).get

		override def isDefinedAt(x: A): Boolean = lift(x).isDefined

		override def applyOrElse[A1 <: A, B1 >: B](x: A1, default: (A1) => B1): B1 =
			lift(x) getOrElse default(x)
	}



	final class ReturnTypeOf[F <: Nothing => Any] private { type Return }

	@SerialVersionUID(ver)
	object ReturnTypeOf {
		implicit def returnTypeOf[X, Y] :ReturnTypeOf[X => Y] { type Return = Y } =
			instance.asInstanceOf[ReturnTypeOf[X => Y] { type Return = Y }]
		private[this] val instance = new ReturnTypeOf[Nothing]
	}

	final val ver = 1L
}