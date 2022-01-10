package net.noresttherein.slang.typist

import scala.reflect.ClassTag

import net.noresttherein.slang.funny.ReturnTypeOf




/** Extension methods casting an expression to related types, less powerful (and dangerous) than `asInstanceOf`. */
object casting {

	/** Extension casting methods for any type `X` putting constraints on the target type in relation to `X`,
	  * intended to prevent devious bugs introduced by refactors changing the type of the cast expression
	  * (which would not produce any compiler warning with `asInstanceOf`).
	  */
	implicit class saferCasting[X](private val self :X) extends AnyVal {
		/** Casts this expression to its subtype `T`. */
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
		  * eliminate the re-creation of a composite object solely to change its type signature
		  * (such as with phantom type parameters), while the implementation is reusable for the target type.
		  * It accepts an (unused) function re-creating the object `Y` in a type safe manner, which both
		  * serves as an illustration and documentation of why the cast is safe, and specifies the target type.
		  */
		@inline def castAsWith[Y](like : => X => Y) :Y = self.asInstanceOf[Y]

		/** Applies the given function to `this` if `this.isInstanceOf[T]`, returning the result in an [[Option]].
		  * {{{
		  *     animal.ifInstanceOf[Cat](_.meow)
		  * }}}
		  * Note that as with `isInstanceOf`, this method is implemented in terms of the runtime ''class'',
		  * and the conformance of erased type parameters is not checked.
		  * @return an instance of a SAM type accepting a function `[X] T=>X`, and returning an `Option[X]`.
		  */
		@inline def ifInstanceOf[T] = new IfInstanceOf[T](self)

		/** Returns `this` as a [[net.noresttherein.slang.optional.Opt.Got Got]]`(this)` if `this.isInstanceOf[T]`,
		  * or [[net.noresttherein.slang.optional.Opt.Lack Lack]] otherwise.
		  */
		@inline def asInstanceOpt[T](implicit tag :ClassTag[T]) :Option[T] = tag.unapply(self)
	}

	class IfInstanceOf[T](private val self :Any) extends AnyVal {
		@inline def apply[X](f :T => X)(implicit tag :ClassTag[T]) :Option[X] =
			if (tag.runtimeClass.isInstance(self)) Some(f(self.asInstanceOf[T]))
			else None
	}



	/** Extension casting methods for the single type parameter of a higher type,
	  * preserving the original type constructor.
	  */
	implicit class castTypeParam[T[A], X](private val self :T[X]) extends AnyVal {
		/** Casts the type parameter of this expression's type, preserving its type constructor. */
		@inline def castParam[A] :T[A] = self.asInstanceOf[T[A]]
	}

	/** Extension downcasting methods for the single type parameter of a higher type,
	  * preserving the original type constructor.
	  */
	implicit class downcastTypeParam[T[A <: X], X](private val self :T[X]) extends AnyVal {
		/** Casts down the type parameter of this expression's type. */
		@inline def downcastParam[A <: X] :T[A] = self.asInstanceOf[T[A]]
	}

	/** Extension casting methods for the type parameters of a higher type,
	  * preserving the original, binary type constructor.
	  */
	implicit class cast2TypeParams[T[_, _], X, Y](private val self :T[X, Y]) extends AnyVal {
		/** Casts both type parameters of this expression's type, preserving its type constructor. */
		@inline def castParams[A, B] :T[A, B] = self.asInstanceOf[T[A, B]]

		/** Casts the first type parameter of this expression's type,
		  * preserving its type constructor and the second parameter.
		  */
		@inline def castParam1[A] :T[A, Y] = self.asInstanceOf[T[A, Y]]

		/** Casts the second type parameter of this expression's type,
		  * preserving its type constructor and the first parameter.
		  */
		@inline def castParam2[B] :T[X, B] = self.asInstanceOf[T[X, B]]
	}

	/** Extension downcasting methods for the type parameters of a higher type,
	  * preserving the original, binary type constructor.
	  */
	implicit class downcast2TypeParams[T[_1 <: X, _2 <: Y], X, Y](private val self :T[X, Y]) extends AnyVal {
		/** Casts down both type parameters of this expression's type. */
		@inline def downcastParams[A <: X, B <: Y] :T[A, B] = self.asInstanceOf[T[A, B]]

		/** Casts down the first type parameter of this expression's type. */
		@inline def downcastParam1[A <: X] :T[A, Y] = self.asInstanceOf[T[A, Y]]

		/** Casts down the second type parameter of this expression's type. */
		@inline def downcastParam2[B <: Y] :T[X, B] = self.asInstanceOf[T[X, B]]
	}

	/** Extension casting methods for the type parameters of a higher type,
	  * preserving the original, ternary type constructor.
	  */
	implicit class cast3TypeParams[T[_, _, _], X, Y, Z](private val self :T[X, Y, Z]) extends AnyVal {
		/** Casts the type parameters of this expression's type, preserving its type constructor. */
		@inline def castParams[A, B, C] :T[A, B, C] = self.asInstanceOf[T[A, B, C]]

		/** Casts the first type parameter of this expression's type,
		  * preserving the type constructor and the rest of the parameters unchanged.
		  */
		@inline def castParam1[A] :T[A, Y, Z] = self.asInstanceOf[T[A, Y, Z]]

		/** Casts the second type parameter of this expression's type,
		  * preserving the type constructor and the rest of the parameters unchanged.
		  */
		@inline def castParam2[B] :T[X, B, Z] = self.asInstanceOf[T[X, B, Z]]

		/** Casts the third type parameter of this expression's type,
		  * preserving the type constructor and the rest of the parameters unchanged.
		  */
		@inline def castParam3[C] :T[X, Y, C] = self.asInstanceOf[T[X, Y, C]]
	}

	/** Extension casting methods for the type parameters of a higher type,
	  * preserving the original, ternary type constructor.
	  */
	implicit class downcast3TypeParams[T[_1 <: X, _2 <: Y, _3 <: Z], X, Y, Z]
	                                  (private val self :T[X, Y, Z]) extends AnyVal
	{
		/** Casts own all type parameters of this expression's type. */
		@inline def downcastParams[A <: X, B <: Y, C <: Z] :T[A, B, C] = self.asInstanceOf[T[A, B, C]]

		/** Casts down the first type parameter of this expression's type,
		  * preserving the type constructor and the rest of the parameters unchanged.
		  */
		@inline def downcastParam1[A <: X] :T[A, Y, Z] = self.asInstanceOf[T[A, Y, Z]]

		/** Casts down the second type parameter of this expression's type,
		  * preserving the type constructor and the rest of the parameters unchanged.
		  */
		@inline def downcastParam2[B <: Y] :T[X, B, Z] = self.asInstanceOf[T[X, B, Z]]

		/** Casts down the third type parameter of this expression's type,
		  * preserving the type constructor and the rest of the parameters unchanged.
		  */
		@inline def downcastParam3[C <: Z] :T[X, Y, C] = self.asInstanceOf[T[X, Y, C]]
	}

}

