package net.noresttherein.slang.typist

import net.noresttherein.slang.funny.ReturnTypeOf


/**
  * @author Marcin Mościcki
  */
object casting {

	implicit class saferCasting[S](private val self :S) extends AnyVal {
		@inline def downcastTo[T <: S] :T = self.asInstanceOf[T]
		@inline def castTo[F >: S, T] :T = self.asInstanceOf[T]
		@inline def castFrom[F >: S] :SaferCasting[F] = new SaferCasting(self)
		@inline def castWith[F <: S => Any](implicit function :ReturnTypeOf[F]) :function.Return =
			self.asInstanceOf[function.Return]
	}
	class SaferCasting[S](private val self :S) extends AnyVal {
		@inline def to[T] :T = self.asInstanceOf[T]
		@inline def downTo[T <: S] :T = self.asInstanceOf[T]
	}

	implicit class downcastParam[T[A <: X], X](private val self :T[X]) extends AnyVal {
		@inline def downtypedWith[A <: X] :T[A] = self.asInstanceOf[T[A]]
	}

	implicit class downcastParams2[T[A <: X, B <: Y], X, Y](private val self :T[X, Y]) extends AnyVal {
		@inline def downtypedWith[A <: X, B <: Y] :T[A, B] = self.asInstanceOf[T[A, B]]
	}

	implicit class downcastParams3[T[A <: X, B <: Y, C <: Z], X, Y, Z](private val self :T[X, Y, Z]) extends AnyVal {
		@inline def downtype[A <: X, B <: Y, C <: Z] :T[A, B, C] = self.asInstanceOf[T[A, B, C]]
	}
}
