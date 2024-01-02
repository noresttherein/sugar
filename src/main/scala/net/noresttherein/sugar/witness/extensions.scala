package net.noresttherein.sugar.witness

import net.noresttherein.sugar.witness.extensions.InfixOrderingOps




trait extensions extends Any {
	/** Infix ordering operators which do not require boxing of the left hand side. */
	@inline implicit final def infixOrderingOps[T](self :T) :InfixOrderingOps[T] = new InfixOrderingOps(self)
}



@SerialVersionUID(Ver)
object extensions extends extensions {
	class InfixOrderingOps[T] private[extensions] (private val lhs :T) extends AnyVal {
		@inline def <(rhs :T)(implicit ord :Ordering[T]) :Boolean = ord.lt(lhs, rhs)
		@inline def <=(rhs :T)(implicit ord :Ordering[T]): Boolean = ord.lteq(lhs, rhs)
		@inline def >(rhs :T)(implicit ord :Ordering[T]) :Boolean = ord.gt(lhs, rhs)
		@inline def >=(rhs :T)(implicit ord :Ordering[T]) :Boolean = ord.gteq(lhs, rhs)
		@inline def equiv(rhs :T)(implicit ord :Ordering[T]) :Boolean = ord.equiv(lhs, rhs)
		@inline def max(rhs :T)(implicit ord :Ordering[T]) :T = ord.max(lhs, rhs)
		@inline def min(rhs :T)(implicit ord :Ordering[T]) :T = ord.min(lhs, rhs)
	}
}
