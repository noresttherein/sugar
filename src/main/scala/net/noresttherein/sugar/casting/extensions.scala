package net.noresttherein.sugar.casting

import scala.reflect.ClassTag

import net.noresttherein.sugar.casting.extensions.{cast2TypeParamsMethods, cast3TypeParamsMethods, castTypeConstructor2Methods, castTypeConstructor3Methods, castTypeConstructorMethods, castTypeParamMethods, castingMethods, downcast2TypeParamsMethods, downcast3TypeParamsMethods, downcastTypeParamMethods, inferredCastingMethods}
import net.noresttherein.sugar.extensions.ClassExtension
import net.noresttherein.sugar.funny.ReturnTypeOf




/** Extension methods casting an expression to related types, less powerful (and dangerous) than `asInstanceOf`. */
//consider: making the package object extend it, as there is nothing else but extensions in casting
trait extensions extends Any { //consider extending by the package object
	@inline implicit final def castingMethods[X](self :X) :castingMethods[X] = new castingMethods[X](self)

	@inline implicit final def castTypeParamMethods[T[A], X](self :T[X]) :castTypeParamMethods[T, X] =
		new castTypeParamMethods[T, X](self)

	@inline implicit final def downcastTypeParamMethods[T[A <: X], X](self :T[X]) :downcastTypeParamMethods[T, X] =
		new downcastTypeParamMethods[T, X](self)

	@inline implicit final def castTypeConstructorMethods[T[_ >: X <: X], X]
		                                                 (self :T[X]) :castTypeConstructorMethods[T, X] =
		new castTypeConstructorMethods(self)

	@inline implicit final def cast2TypeParamsMethods[T[_, _], X, Y](self :T[X, Y]) :cast2TypeParamsMethods[T, X, Y] =
		new cast2TypeParamsMethods[T, X, Y](self)

	@inline implicit final def downcast2TypeParamsMethods[T[A <: X, B <: Y], X, Y]
		                                                 (self :T[X, Y]) :downcast2TypeParamsMethods[T, X, Y] =
		new downcast2TypeParamsMethods[T, X, Y](self)

	@inline implicit final def castTypeConstructor2Methods[T[_1 >: X <: X, _2 >: Y <: Y], X, Y]
		                                                  (self :T[X, Y]) :castTypeConstructor2Methods[T, X, Y] =
		new castTypeConstructor2Methods(self)

	@inline implicit final def cast3TypeParamsMethods[T[_, _, _], X, Y, Z]
		                                             (self :T[X, Y, Z]) :cast3TypeParamsMethods[T, X, Y, Z] =
		new cast3TypeParamsMethods[T, X, Y, Z](self)

	@inline implicit final def downcast3TypeParamsMethods[T[A <: X, B <: Y, C <: Z], X, Y, Z]
		                                                 (self :T[X, Y, Z]) :downcast3TypeParamsMethods[T, X, Y, Z] =
		new downcast3TypeParamsMethods[T, X, Y, Z](self)

	@inline implicit final def castTypeConstructor3Methods[T[_1 >: X <: X, _2 >: Y <: Y, _3 >: Z <: Z], X, Y, Z]
	                                                      (self :T[X, Y, Z]) :castTypeConstructor3Methods[T, X, Y, Z] =
		new castTypeConstructor3Methods(self)

	@inline implicit final def inferredCastingMethods[T](self :T) :inferredCastingMethods[T] =
		new inferredCastingMethods(self)
}




@SerialVersionUID(Ver)
object extensions {

	//todo: use macros
	/** Extension casting methods for any type `X` putting constraints on the target type in relation to `X`,
	  * intended to prevent devious bugs introduced by refactors changing the type of the cast expression
	  * (which would not produce any compiler warning with `asInstanceOf`).
	  */
	class castingMethods[X](private val self :X) extends AnyVal {
		/** Promotes `this` value to its `AnyRef` runtime representation. */
		@inline def asAnyRef :AnyRef = self.asInstanceOf[AnyRef]

		/** Casts this expression to its subtype `T`. */
		@inline def asSubtype[T <: X] :T = self.asInstanceOf[T]

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

		/** Applies the given function for side effects to `this` if `this.isInstanceOf[T]`.
		  * {{{
		  *     animal.forInstanceOf[Cat](_.pushOff(vase))
		  * }}}
		  * Note that, as with `isInstanceOf`, this method is implemented in terms of the runtime ''class'',
		  * and the conformance of erased type parameters is not checked.
		  */
		@inline def forInstanceOf[T](f :T => Any)(implicit tag :ClassTag[T]) :Unit =
			if (tag.runtimeClass.isBoxInstance(self))
				f(self.asInstanceOf[T])

		/** Applies the given function to `this` if `this.isInstanceOf[T]`, returning the result in an `Option`.
		  * {{{
		  *     animal.ifInstanceOf[Cat](_.meow)
		  * }}}
		  * Implemented as an applicable object rather than a method so that the returned type can be inferred.
		  * Note that, as with `isInstanceOf`, this method is implemented in terms of the runtime ''class'',
		  * and the conformance of erased type parameters is not checked.
		  * @return an instance of a SAM type accepting a function `[X] T=>X`, and returning an `Option[X]`.
		  */
		@inline def ifInstanceOf[T] = new ifInstanceOf[T](self)

		//consider: renaming to castTo; it would probably warrant renaming castFrom in turn.
		/** Returns `this` as a [[Some]]`(this)` if `this.isInstanceOf[T]`, or [[None]] otherwise. */
		@inline def asInstanceOpt[T](implicit tag :ClassTag[T]) :Option[T] = tag.unapply(self)

		/** Applies the given function for side effects to `this` if `this.isInstanceOf[T]`. This is the same as
		  * [[net.noresttherein.sugar.casting.castingMethods.forInstanceOf forInstanceOf]],
		  * but this method additionally restricts the caller to subtypes of this object's type.
		  */
		@inline def forSubclass[T <: X](f :T => Any)(implicit tag :ClassTag[T]) :Unit =
			if (tag.runtimeClass.isBoxInstance(self))
				f(self.asInstanceOf[T])

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
		@inline def ifSubclass[T <: X] = new ifInstanceOf[T](self)

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

		/** Casts this object to its subtype shared with type `R`.
		  * There is nothing special in the implementation of this method; it is intended to be used only
		  * with structural types containing only member types.
		  * This is useful in several type level techniques, for example:
		  *   1. 'Labeling' an instance for injecting it as an implicit parameter, in presence of several values
		  *      of the same type:
		  *      {{{
		  *         val adventurer = new Adventurer().refine[{ type name = "Varric" }]
		  *         val varric = implicitly[Adventurer { type name = "Varric" }]
		  *      }}}
		  *    1. Marking an object as exhibiting a certain property, checked at runtime:
		  *      {{{
		  *          class Mage(maybeFamiliar :Option[Familiar]) {
		  *             type hasFamiliar
		  *             def familiar(implicit ev: this.type <:< (Mage { hasFamiliar = true }) :Familiar =
		  *                 maybeFamiliar.get
		  *             def withFamiliar :Option[Mage { hasFamiliar = true }] =
		  *                 if (maybeFamiliar.isDefined) Some(this.refine[{ hasFamiliar = true }] else None
		  *          }
		  *      }}}
		  *    1. Recursive implicit evidence with a single implementation, containing a composed member type:
		  *      {{{
		  *          class Lens[-T <: Tuple, Idx <: Int with Singleton](val idx :Idx) extends AnyVal {
		  *             type E
		  *             def get(tuple :T) :E = elem.productElement(idx).asInstanceOf[E]
		  *          }
		  *          implicit def first[H] :Lens[H*:Tuple, 0] { type E = H } =
		  *             (new Lens[H*:Tuple, 0](0)).refine[{ type E = H }]
		  *          implicit next[T <: Tuple, I <: Int with Singleton](implicit ev :Lens[T, I])
		  *                 :Lens[Any*:T, succ[I] { type E = ev.E }] =
		  *             (new Lens[Any*:T, succ[I]](ev.idx + 1)).refine[{ type E = ev.E }]
		  *      }}}
		  *
		  * While often the same effect can be obtained by introducing a type parameter to the class,
		  * it is often not desirable (for example, the number of type parameters would be impractical),
		  * practical (for example, to use a member type as an 'Out' type in implicit evidence,
		  * in order not to confuse the type inferer), or even possible (if the refined class is not owned
		  * by the programmer, or the functionality depending on the refinement is completely orthogonal to it.
		  */
		@inline def refine[R] :X with R = self.asInstanceOf[X with R]
	}


	class ifInstanceOf[T](private val self :Any) extends AnyVal {
		@inline def apply[X](f :T => X)(implicit tag :ClassTag[T]) :Option[X] =
			if (tag.runtimeClass.isInstance(self)) Some(f(self.asInstanceOf[T]))
			else None
	}



	/** Extension methods casting any value of a higher type with a single type parameter to another higher type
	  * with the same type parameter.
	  */
	class castTypeConstructorMethods[T[A >: X <: X], X](private val self :T[X]) extends AnyVal {
		/** Casts the type constructor of this expression's type, preserving the type parameter. */
		@inline def castCons[O[A >: X <: X]] :O[X] = self.asInstanceOf[O[X]]

		/** Casts the type constructor of this expression's type, preserving the type parameter.
		  * For added safety, the current type constructor must be specified explicitly as the first type argument.
		  */
		@inline def castConsFrom[U[A >: X <: X] >: T[A] <: T[A], O[A >: X <: X]] :O[X] = self.asInstanceOf[O[X]]

		/** Downcasts the type constructor of this expression's type, preserving the type parameter. */
		@inline def downcastCons[O[A >: X <: X] <: T[A]] :O[X] = self.asInstanceOf[O[X]]

		/** Casts the type constructor of this expression's type, preserving the type parameter.
		  * For added safety, the current type constructor must be specified explicitly as the first type argument.
		  */
		@inline def downcastConsFrom[U[A >: X <: X] >: T[A], O[A >: X <: X] <: T[A]] :O[X] =
			self.asInstanceOf[O[X]]
	}

	/** Extension methods casting any value of a higher type with a two type parameters to another higher type
	  * with the same type parameters.
	  */
	class castTypeConstructor2Methods[T[_1 >: X <: X, _2 >: Y <: Y], X, Y](private val self :T[X, Y]) extends AnyVal {
		/** Casts the type constructor of this expression's type, preserving the type parameter. */
		@inline def castCons[O[_ >: X <: X, _ >: Y <: Y]] :O[X, Y] = self.asInstanceOf[O[X, Y]]

		/** Casts the type constructor of this expression's type, preserving the type parameter.
		  * For added safety, the type constructor of the cast expression must be specified explicitly
		  * as the first type argument.
		  */
		@inline def castConsFrom[U[A >: X <: X, B >: Y <: Y] >: T[A, B] <: T[A, B], O[_ >: X <: X, _ >: Y <: Y]] :O[X, Y] =
			self.asInstanceOf[O[X, Y]]

		/** Casts down the type constructor of this expression's type, preserving the type parameter. */
		@inline def downcastCons[O[A >: X <: X, B >: Y <: Y] <: T[A, B]] :O[X, Y] = self.asInstanceOf[O[X, Y]]

		/** Casts down the type constructor of this expression's type, preserving the type parameter.
		  * For added safety, the type constructor of the cast expression must be specified explicitly
		  * as the first type argument.
		  */
		@inline def downcastConsFrom[U[A >: X <: X, B >: Y <: Y] >: T[A, B], O[A >: X <: X, B >: Y <: Y] <: T[A, B]] :O[X, Y] =
			self.asInstanceOf[O[X, Y]]
	}

	/** Extension methods casting any value of a higher type with a two type parameters to another higher type
	  * with the same type parameters.
	  */
	class castTypeConstructor3Methods[T[_1 >: X <: X, _2 >: Y <: Y, _3 >: Z <: Z], X, Y, Z](private val self :T[X, Y, Z])
		extends AnyVal
	{
		/** Casts the type constructor of this expression's type, preserving the type parameter. */
		@inline def castCons[O[_ >: X <: X, _ >: Y <: Y, _ >: Z <: Z]] :O[X, Y, Z] = self.asInstanceOf[O[X, Y, Z]]

		/** Casts the type constructor of this expression's type, preserving the type parameter.
		  * For added safety, the current type constructor of this expression must be specified explicitly
		  * as the first type argument.
		  */
		@inline def castConsFrom[U[A >: X <: X, B >: Y <: Y, C >: Z <: Z] >: T[A, B, C] <: T[A, B, C],
		                         O[A >: X <: X, B >: Y <: Y, C >: Z <: Z] <: T[A, B, C]]
				:O[X, Y, Z] =
			self.asInstanceOf[O[X, Y, Z]]

		/** Casts down the type constructor of this expression's type, preserving the type parameter. */
		@inline def downcastCons[O[_1 >: X <: X, _2 >: Y <: Y, _3 >: Z <: Z] <: T[_1, _2, _3]] :O[X, Y, Z] =
			self.asInstanceOf[O[X, Y, Z]]

		/** Casts down the type constructor of this expression's type, preserving the type parameter.
		  * For added safety, the current type constructor of this expression must be specified explicitly
		  * as the first type argument.
		  */
		@inline def downcastConsFrom[U[A >: X <: X, B >: Y <: Y, C >: Z <: Z] >: T[A, B, C],
		                             O[A >: X <: X, B >: Y <: Y, C >: Z <: Z] <: T[A, B, C]]
				:O[X, Y, Z] =
			self.asInstanceOf[O[X, Y, Z]]
	}



	/** Extension casting methods for the single type parameter of a higher type,
	  * preserving the original type constructor.
	  */
	class castTypeParamMethods[T[A], X](private val self :T[X]) extends AnyVal {
		/** Casts the type parameter of this expression's type, preserving its type constructor. */
		@inline def castParam[A] :T[A] = self.asInstanceOf[T[A]]
//		@inline def asWithParam[A] :T[A] = self.asInstanceOf[T[A]]
		/** Casts the type parameter of this expression's type, preserving its type constructor.
		  * For added safety, the type parameter of `this` must be specified as the first type argument.
		  */
		@inline def castParamFrom[U >: X <: X, A] :T[A] = self.asInstanceOf[T[A]]
	}

	/** Extension down casting methods for the single type parameter of a higher type,
	  * preserving the original type constructor.
	  * @note not only these methods are preferable to more generic
	  *       [[net.noresttherein.sugar.casting.extensions.castTypeParamMethods castTypeParamMethods]]
	  *       due to lesser potential for misuse, but they also work for type constructors with upper type bounds,
	  *       which the latter does not.
	  */
	class downcastTypeParamMethods[T[A <: X], X](private val self :T[X]) extends AnyVal {
		/** Casts down the type parameter of this expression's type. */
		@inline def downcastParam[A <: X] :T[A] = self.asInstanceOf[T[A]]
		/** Casts down the type parameter of this expression's type, preserving its type constructor.
		  * For added safety, the type parameter of `this` must be specified as the first type argument.
		  */
		@inline def downcastParamFrom[U >: X <: X, A <: X] :T[A] = self.asInstanceOf[T[A]]
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
		@inline def castParam1[O] :T[O, Y] = self.asInstanceOf[T[O, Y]]

		/** Casts the second type parameter of this expression's type,
		  * preserving its type constructor and the first parameter.
		  */
		@inline def castParam2[B] :T[X, B] = self.asInstanceOf[T[X, B]]

		/** Casts the first type parameter of this expression's type,
		  * preserving its type constructor and the second parameter.
		  * For added safety, the current first type parameter of this expression must be specified explicitly
		  * as the first type argument.
		  */
		@inline def castParam1From[U >: X <: X, A] :T[A, Y] = self.asInstanceOf[T[A, Y]]

		/** Casts the second type parameter of this expression's type,
		  * preserving its type constructor and the first parameter.
		  * For added safety, the current second type parameter of this expression must be specified explicitly
		  * as the first type argument.
		  */
		@inline def castParam2From[U >: Y <: Y, B] :T[B, Y] = self.asInstanceOf[T[B, Y]]
	}

	/** Extension down casting methods for the type parameters of a higher type,
	  * preserving the original, binary type constructor.
	  * @note not only these methods are preferable to more generic
	  *       [[net.noresttherein.sugar.casting.extensions.cast2TypeParamsMethods cast2TypeParamsMethods]]
	  *       due to lesser potential for misuse, but they also work for type constructors with upper type bounds,
	  *       which the latter does not.
	  */
	class downcast2TypeParamsMethods[T[_1 <: X, _2 <: Y], X, Y](private val self :T[X, Y]) extends AnyVal {
		/** Casts down both type parameters of this expression's type. */
		@inline def downcastParams[A <: X, B <: Y] :T[A, B] = self.asInstanceOf[T[A, B]]

		/** Casts down the first type parameter of this expression's type. */
		@inline def downcastParam1[A <: X] :T[A, Y] = self.asInstanceOf[T[A, Y]]

		/** Casts down the second type parameter of this expression's type. */
		@inline def downcastParam2[B <: Y] :T[X, B] = self.asInstanceOf[T[X, B]]

		/** Casts down the first type parameter of this expression's type,
		  * preserving its type constructor and the second parameter.
		  * For added safety, the current first type parameter of this expression must be specified explicitly
		  * as the first type argument.
		  */
		@inline def downcastParam1From[U >: X, A <: X] :T[A, Y] = self.asInstanceOf[T[A, Y]]

		/** Casts down the second type parameter of this expression's type,
		  * preserving its type constructor and the first parameter.
		  * For added safety, the current second type parameter of this expression must be specified explicitly
		  * as the first type argument.
		  */
		@inline def downcastParam2From[U >: Y, B <: Y] :T[X, B] = self.asInstanceOf[T[X, B]]
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

		/** Casts the first type parameter of this expression's type,
		  * preserving its type constructor and the other parameters.
		  * For added safety, the current first type parameter of this expression must be specified explicitly
		  * as the first type argument.
		  */
		@inline def castParam1From[U >: X <: X, A] :T[A, Y, Z] = self.asInstanceOf[T[A, Y, Z]]

		/** Casts the second type parameter of this expression's type,
		  * preserving its type constructor and the other parameter.
		  * For added safety, the current second type parameter of this expression must be specified explicitly
		  * as the first type argument.
		  */
		@inline def castParam2From[U >: Y <: Y, B] :T[X, B, Z] = self.asInstanceOf[T[X, B, Z]]

		/** Casts the third type parameter of this expression's type,
		  * preserving its type constructor and the other parameter.
		  * For added safety, the current third type parameter of this expression must be specified explicitly
		  * as the first type argument.
		  */
		@inline def castParam3From[U >: Z <: Z, C] :T[X, Y, C] = self.asInstanceOf[T[X, Y, C]]
	}

	/** Extension casting methods for the type parameters of a higher type,
	  * preserving the original, ternary type constructor.
	  * @note not only these methods are preferable to more generic
	  *       [[net.noresttherein.sugar.casting.extensions.cast3TypeParamsMethods cast3TypeParamsMethods]]
	  *       due to lesser potential for misuse, but they also work for type constructors with upper type bounds,
	  *       which the latter does not.
	  */
	class downcast3TypeParamsMethods[T[_1 <: X, _2 <: Y, _3 <: Z], X, Y, Z](private val self :T[X, Y, Z])
		extends AnyVal
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

		/** Casts down the first type parameter of this expression's type,
		  * preserving its type constructor and the other parameters.
		  * For added safety, the current first type parameter of this expression must be specified explicitly
		  * as the first type argument.
		  */
		@inline def downcastParam1From[U >: X <: X, A <: X] :T[A, Y, Z] = self.asInstanceOf[T[A, Y, Z]]

		/** Casts down the second type parameter of this expression's type,
		  * preserving its type constructor and the other parameter.
		  * For added safety, the current second type parameter of this expression must be specified explicitly
		  * as the first type argument.
		  */
		@inline def downcastParam2From[U >: Y <: Y, B <: Y] :T[X, B, Z] = self.asInstanceOf[T[X, B, Z]]

		/** Casts down the third type parameter of this expression's type,
		  * preserving its type constructor and the other parameter.
		  * For added safety, the current third type parameter of this expression must be specified explicitly
		  * as the first type argument.
		  */
		@inline def downcastParam3From[U >: Z <: Z, C <: Z] :T[X, Y, C] = self.asInstanceOf[T[X, Y, C]]
	}



	/** Casting methods whose target type may be inferred from the expected type. */
	class inferredCastingMethods[T](private val self :T) extends AnyVal {
		/** Equivalent to `this.asIndexedOf[X]`, but the type may be inferred. Use with caution! */
		@inline def inferredCast[X] :X = self.asInstanceOf[X]

		/** Equivalent to `this.asIndexedOf[X]`, but can be used only to cast to a subtype of this type,
		  * and the type may be inferred. Use with caution!
		  */
		@inline def inferredDowncast[X <: T] :X = self.asInstanceOf[X]
	}
}
