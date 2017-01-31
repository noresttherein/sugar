package net.turambar.slang

package object funny {
	def lift[X, Y](f :PartialFunction[X, Y]) :X=>Option[Y] = f.lift

	def lower[X, Y](f :X=>Option[Y]) :PartialFunction[X, Y] = f.lower

	implicit class lowerToPartialFunction[A, B](private val f :A=>Option[B]) extends AnyVal {
		def lower :PartialFunction[A, B] = new PartialFunctionOptionAdapter(f)
	}

	class PartialFunctionOptionAdapter[A, B](override final val lift :A=>Option[B]) extends PartialFunction[A, B] {
		override def apply(v1: A): B = lift(v1).get

		override def isDefinedAt(x: A): Boolean = lift(x).isDefined

		override def applyOrElse[A1 <: A, B1 >: B](x: A1, default: (A1) => B1): B1 =
			lift(x) getOrElse default(x)
	}
}