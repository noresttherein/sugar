package net.noresttherein.slang.typist

import scala.reflect.ClassTag

import net.noresttherein.slang.funny.ReturnTypeOf


/**
  * @author Marcin Mościcki
  */
object casting {

	implicit class saferCasting[X](private val self :X) extends AnyVal {
		@inline def downcastTo[T <: X] :T = self.asInstanceOf[T]

		/** A safer casting expression which, in addition to the target type, accepts also the type of the cast
		  * expression itself (`this`).
		  * Providing both is a defence against inadvertent casting from a wrongly presumed source type and,
		  * more importantly, against an expression changing type silently due to a refactor.
		  * @tparam U the (super)type of `this` expression.
		  * @tparam Y the target type of the expression after casting.
		  */
		@inline def castFrom[U >: X, Y] :Y = self.asInstanceOf[Y]

		/** A safer casting expression which, in addition to the target type, accepts also the type of the cast
		  * expression itself (`this`). Both types are given as a single argument function `X => Y`,
		  * with `X` being the type to which `this` must conform, and `Y` the desired target type.
		  * Providing both is a defence against inadvertent casting from a wrongly presumed source type and,
		  * more importantly, against an expression changing type silently due to a refactor.
		  */
		@inline def castWith[F <: X => Any](implicit function :ReturnTypeOf[F]) :function.Return =
			self.asInstanceOf[function.Return]

		/** A safer casting expression intended for cases where the cast is an optimisation meant to
		  * eliminate the re-creation of a composite object solely to change its type signature (typically
		  * phantom type parameters), while the implementation is reusable for the target type.
		  * It accepts an (unused) function re-creating the object `Y` in a type safe manner, which both
		  * serves as an illustration and documentation of why the cast is safe, and specifies the target type.
		  */
		@inline def castLike[Y](like : => X => Y) :Y = self.asInstanceOf[Y]

		@inline def ifInstanceOf[T] = new IfInstanceOf[T](self)
		@inline def asInstanceOpt[T](implicit tag :ClassTag[T]) :Option[T] = tag.unapply(self)
	}

	class IfInstanceOf[T](private val self :Any) extends AnyVal {
		@inline def apply[X](f :T => X)(implicit tag :ClassTag[T]) :Option[X] =
			if (tag.runtimeClass.isInstance(self)) Some(f(self.asInstanceOf[T]))
			else None
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
