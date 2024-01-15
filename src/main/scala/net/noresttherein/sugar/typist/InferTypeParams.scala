package net.noresttherein.sugar.typist

import scala.annotation.implicitNotFound



/** A simple class combining the evidence that `X &lt; T with M with U`, designed to alleviate the limitation
  * of scala type inference of type arguments for generic classes. An implicit instance `InferTypeParams[X, X, X]`
  * is available for any type `X`.
  * Given some type constructor `F[X]`, we would often like to declare a generic method with regard to both
  * the specific type of `x :F[X]` and the provided type argument at the same time:
  * {{{
  *     class F[X] { def get :X = ??? }
  *     class X extends F[String]
  *
  *     def m[X <: F[X], X](x :X) :(X, X) = ???
  *
  *     m(new X) //does not compile
  * }}}
  * Unfortunately, the compiler will infer `X, Nothing` as `m`'s type parameters in the call from the above example.
  * This is the more problematic because the need to explicitly provide type arguments prevents any implicit
  * conversion with a similar signature. However, Type inference will work for both the type `X` and the type parameter
  * `String` given to `F`, just not both at the same time:
  * {{{
  *     def m1[X <: F[_]](x :X) = ???
  *     def m2[X](x :F[X]) = ???
  *
  *     m1(new X) //compiles
  *     m2(new X) //compiles
  * }}}
  * By duplicating the occurrence of type `X` in this type signature and having the last type parameter covariant,
  * we can now write:
  * {{{
  *     def m[A, X <: F[X], X](x :A)(implicit help :Conforms[A, X, F[X]]) :(X, X) = help(x) -> help(x).get
  * }}}
  *
  * By convention, the second type parameter should be the complete inferred type, while the third one its upper bound
  * used to capture its type parameters. For convenience, this trait implements `X => T`, so simply declaring an
  * implicit parameter of this type introduces an implicit conversion from the generic, unbound type to the desired one.
  *
  * This inference ''should'' work as long as the type `T` does not occur in the method's signature before this argument.
  * It is guaranteed however only if the actual parameterized type `T` is a free type variable, that is does not occur
  * in the method signature outside this implicit parameter. One common failure case is when `T` occurs as part
  * of the a method's return type, and the caller specifies the expected result type explicitly (in terms of `T`),
  * for example by returning the method's result directly. The exact algorithm that scala
  * uses for instantiating type parameters is not documented and changes over time, but accepting `InferTypeParameters`
  * as the first implicit parameter generally allows the following implicit parameters to use all the types
  * present in its definition. An occurrence of `T` in the return type of the method accepting this implicit
  * value can often unfortunately likewise result in a failure to infer the type parameters as in the standard scenario.
  * If possible, try to use the type `X` instead, which is always equivalent to `T` for the caller, and use
  * the `_1 : X=:=T` property to cast back the result.
  * @tparam X the type of the formal parameter of the generic method with only concrete bounds, for example
  *           `X &lt: Generic[_]`
  * @tparam T the desired, inferred type of the argument, with bounds dependent on other type parameters,
  *           for example `T <: Generic[X]`. It will always be `T =:= X`.
  * @tparam U the parameterized upper bound for types `X`, `T` with free type parameters only on the first level and
  *           with concrete (or instantiated by the inferer) bounds, for example `Generic[X]`.
  * @author Marcin Mościcki
  */ //consider: moving it to witness.
@implicitNotFound("Cannot infer type arguments: cannot prove ${X} =:= ${T} <:< ${U}.\n" +
                  "This may be caused by the second type parameter occurring before " +
                  "the InferTypeParams[${X}, ${T}, ${U}] argument in the method signature or its return type.")
sealed abstract class InferTypeParams[X, T <: U, +U] extends (X => T) {
	/** Witness of equivalency between the argument type `X` and the type `T` with inferred type arguments
	  * for their supertype `U`, used in the implementation of the method.
	  */
	def _1 :X =:= T
	/** Witness of the argument's type conformity to a generic upper bound type `U` with inferred type arguments. */
	def _2 :X <:< U
	/** Witness of the argument's type conformity to its inferred parameterized form and their generic upper bound `U` */
	def conjunction :X <:< T with U

	/** Witness of the inferred type's `T` conformity to the generic upper bound `U` provided with type arguments by
	  * `T` (by directly or indirectly extending type `U`).
	  */
	def ub :T <:< U
}



@SerialVersionUID(Ver)
object InferTypeParams {

	@inline def conforms[T] :InferTypeParams[T, T, T] = unify[T]

	/** Summon an implicitly available evidence `InferTypeParams[T, L, M, R]` witnessing that `T <: L with M with R`. */
	@inline def apply[T, L <: R, R](implicit hint :InferTypeParams[T, L, R]) :InferTypeParams[T, L, R] = hint

	implicit def unify[T] :InferTypeParams[T, T, T] =
		conforms.asInstanceOf[InferTypeParams[T, T, T]]

	private[this] final val conforms = new InferTypeParams[Any, Any, Any] {
		override def _1 = implicitly[Any =:= Any]
		override def _2 = implicitly[Any <:< Any]
		override def ub = implicitly[Any <:< Any]
		override def conjunction = implicitly[Any <:< Any]

		override def apply(x :Any) = x

		override def andThen[A](g :Any => A) = g
		override def compose[A](g :A => Any) = g
	}


}

