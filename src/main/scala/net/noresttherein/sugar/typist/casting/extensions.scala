package net.noresttherein.sugar.typist.casting

import scala.reflect.ClassTag

import net.noresttherein.sugar.funny.ReturnTypeOf
import net.noresttherein.sugar.typist
import net.noresttherein.sugar.typist.casting.extensions.{cast2TypeParamsMethods, cast3TypeParamsMethods, castTypeParamMethods, downcast2TypeParamsMethods, downcast3TypeParamsMethods, downcastTypeParamMethods, castingMethods}




/** Extension methods casting an expression to related types, less powerful (and dangerous) than `asInstanceOf`. */
trait extensions extends Any { //consider extending by the package object
	@inline implicit final def castingMethods[X](self :X) :castingMethods[X] = new castingMethods[X](self)

	@inline implicit final def castTypeParamMethods[T[A], X](self :T[X]) :castTypeParamMethods[T, X] =
		new castTypeParamMethods[T, X](self)

	@inline implicit final def downcastTypeParamMethodsd[T[A <: X], X](self :T[X]) :downcastTypeParamMethods[T, X] =
		new downcastTypeParamMethods[T, X](self)

	@inline implicit final def cast2TypeParamsMethods[T[_, _], X, Y](self :T[X, Y]) :cast2TypeParamsMethods[T, X, Y] =
		new cast2TypeParamsMethods[T, X, Y](self)

	@inline implicit final def downcast2TypeParamsMethods[T[A <: X, B <: Y], X, Y]
		                                                 (self :T[X, Y]) :downcast2TypeParamsMethods[T, X, Y] =
		new downcast2TypeParamsMethods[T, X, Y](self)

	@inline implicit final def cast3TypeParamsMethods[T[_, _, _], X, Y, Z]
		                                             (self :T[X, Y, Z]) :cast3TypeParamsMethods[T, X, Y, Z] =
		new cast3TypeParamsMethods[T, X, Y, Z](self)

	@inline implicit final def downcast3TypeParamsMethods[T[A <: X, B <: Y, C <: Z], X, Y, Z]
		                                                 (self :T[X, Y, Z]) :downcast3TypeParamsMethods[T, X, Y, Z] =
		new downcast3TypeParamsMethods[T, X, Y, Z](self)
}




@SerialVersionUID(typist.Ver)
object extensions extends extensions {

	//todo: use macros
	/** Extension casting methods for any type `X` putting constraints on the target type in relation to `X`,
	  * intended to prevent devious bugs introduced by refactors changing the type of the cast expression
	  * (which would not produce any compiler warning with `asInstanceOf`).
	  */
	class castingMethods[X](private val self :X) extends AnyVal {
		/** Promotes `this` value to its `AnyRef` runtime representation. */
		@inline def asAnyRef :AnyRef = self.asInstanceOf[AnyRef]

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
//
//		/** A safer casting expression which, in addition to the target type, accepts also the type of the cast
//		  * expression itself (`this`). Both types are given as a single argument function `X => Y`,
//		  * with `X` being the type to which `this` must conform, and `Y` the desired target type.
//		  * Providing both is a defence against inadvertent casting from a wrongly presumed source type and,
//		  * more importantly, against an expression changing type silently due to a refactor.
//		  */
//		@inline def castWith[F <: X => Any](implicit function :ReturnTypeOf[F]) :function.Return =
//			self.asInstanceOf[function.Return]

		/** A safer casting expression intended for cases where the cast is an optimisation meant to
		  * eliminate the re-creation of a composite object solely to change its type signature
		  * (such as with phantom type parameters), while the implementation is reusable for the target type.
		  * It accepts an (unused) function re-creating the object `Y` in a type safe manner, which both
		  * serves as an illustration and documentation of why the cast is safe, and specifies the target type.
		  */
		@inline def castAsIfBy[Y](like : => X => Y) :Y = self.asInstanceOf[Y]

		/** Applies the given function to `this` if `this.isInstanceOf[T]`, returning the result in an `Option`.
		  * {{{
		  *     animal.ifInstanceOf[Cat](_.meow)
		  * }}}
		  * Note that as with `isInstanceOf`, this method is implemented in terms of the runtime ''class'',
		  * and the conformance of erased type parameters is not checked.
		  * @return an instance of a SAM type accepting a function `[X] T=>X`, and returning an `Option[X]`.
		  */
		@inline def ifInstanceOf[T] = new IfInstanceOf[T](self)

		/** Returns `this` as a [[Some]]`(this)` if `this.isInstanceOf[T]`, or [[None]] otherwise. */
		@inline def asInstanceOpt[T](implicit tag :ClassTag[T]) :Option[T] = tag.unapply(self)

		/** Applies the given function to `this` if `this.isInstanceOf[T]`, returning the result in an `Option`.
		  * {{{
		  *     animal.ifInstanceOf[Cat](_.meow)
		  * }}}.
		  * Note that this check is based on class tag only, which will ignore any type parameters that `X` might have.
		  * This is essentially the same as `this.ifInstanceOf[X]`, but requires `X` to be verified
		  * as a subtype of the cast expression. This additional bound guards against casting to an unrelated type
		  * accidentally or as a result of a refactor. While of course `x :Any` and `X <: Any` are always true,
		  * this method isn't any more strict, but the type of this expression is typically inferred as the most specific
		  * non-singleton type, and the compiler will not implicitly convert `t :T` to `Any`
		  * in order to conform to the bound on this method's type parameter.
		  */
		@inline def ifSubclass[T <: X] = new IfInstanceOf[T](self)

		/** Returns `Some(this.asInstanceOf[X])` if `this.isInstanceOf[X]`. Note that this check is based on class tag
		  * only, which will ignore any type parameters that `X` might have. This is essentially the same as
		  * `this.asInstanceOpt[X]`, but requires `X` to be verified as the subtype of the cast expression.
		  * This additional bound guards against casting to an unrelated type accidentally or as a result of a refactor.
		  * While of course `x :Any` and `X <: Any` are always true, this method isn't any more strict, but the type
		  * of this expression is typically inferred as the most specific non-singleton type, and the compiler will
		  * not implicitly convert `t :T` to `Any` in order to conform to the bound on this method's type parameter.
		  * @return `classTag[X].unapply(this)`
		  */
		@inline def asSubclassOpt[T <: X](implicit tag :ClassTag[T]) :Option[T] = tag.unapply(self)
	}


	class IfInstanceOf[T](private val self :Any) extends AnyVal {
		@inline def apply[X](f :T => X)(implicit tag :ClassTag[T]) :Option[X] =
			if (tag.runtimeClass.isInstance(self)) Some(f(self.asInstanceOf[T]))
			else None
	}




	/** Extension casting methods for the single type parameter of a higher type,
	  * preserving the original type constructor.
	  */
	class castTypeParamMethods[T[A], X](private val self :T[X]) extends AnyVal {
		/** Casts the type parameter of this expression's type, preserving its type constructor. */
		@inline def castParam[A] :T[A] = self.asInstanceOf[T[A]]
	}

	/** Extension downcasting methods for the single type parameter of a higher type,
	  * preserving the original type constructor.
	  */
	class downcastTypeParamMethods[T[A <: X], X](private val self :T[X]) extends AnyVal {
		/** Casts down the type parameter of this expression's type. */
		@inline def downcastParam[A <: X] :T[A] = self.asInstanceOf[T[A]]
	}




	/** Extension casting methods for the type parameters of a higher type,
	  * preserving the original, binary type constructor.
	  */
	class cast2TypeParamsMethods[T[_, _], X, Y](private val self :T[X, Y]) extends AnyVal {
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
	class downcast2TypeParamsMethods[T[_1 <: X, _2 <: Y], X, Y](private val self :T[X, Y]) extends AnyVal {
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
	class cast3TypeParamsMethods[T[_, _, _], X, Y, Z](private val self :T[X, Y, Z]) extends AnyVal {
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
	class downcast3TypeParamsMethods[T[_1 <: X, _2 <: Y, _3 <: Z], X, Y, Z](private val self :T[X, Y, Z]) extends AnyVal {
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
