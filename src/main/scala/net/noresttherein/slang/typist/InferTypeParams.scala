package net.noresttherein.slang.typist

import scala.annotation.implicitNotFound


/** A simple value class combining the evidence that `T &lt; L with M with R`, designed to alleviate the limitation
  * of scala type inference of type arguments for generic classes. An implicit instance `InferTypeParams[X, X, X]`
  * is available for any type `X`, which can be additionally simplified by the type alias
  * `Conforms[X, L, R] = InferTypeParams[X, L, R]` declared in the companion object.
  * Given some type constructor `F[T]`, we would often like to declare a generic method with regard to both
  * the specific type of `x :F[T]` and the provided type argument at the same time:
  * {{{
  *     class F[T] { def get :T = ??? }
  *     class X extends F[String]
  *
  *     def m[X <: F[T], T](x :X) :(X, T) = ???
  *
  *     m(new X) //does not compile
  * }}}
  * Unfortunately, the compiler will infer `X, Nothing` as `m`'s type parameters in the call from the above example.
  * This is the more problematic because the need to explicitly provide type arguments prevents any implicit
  * conversion with a similar signature. However, Type inference will work for both the type `X` and the type parameter
  * `String` given to `F`, just not both at the same time:
  * {{{
  *     def m1[X <: F[_]](x :X) = ???
  *     def m2[T](x :F[T]) = ???
  *
  *     m1(new X) //compiles
  *     m2(new X) //compiles
  * }}}
  * By duplicating the occurrence of type `X` in this type signature and having the last type parameter covariant,
  * we can now write:
  * {{{
  *     def m[A, X <: F[T], T](x :A)(implicit help :Conforms[A, X, F[T]]) :(X, T) = help(x) -> help(x).get
  * }}}
  *
  * By convention, the second type parameter should be the complete inferred type, while the third one its upper bound
  * used to capture its type parameters. For convenience, this trait implements `T => L`, so simply declaring an
  * implicit parameter of this type introduces an implicit conversion from the generic, unbound type to the desired one.
  * @tparam T the type of the formal parameter of the generic method with only concrete bounds.
  * @tparam L the desired, inferred type of the argument, with bounds dependent on other type parameters.
  *           This will always be `L =:= T`.
  * @tparam R the parameterized upper bound for types `T`, `L` with free type parameters only on the first level and
  *           with concrete (or instantiated by the inferer) bounds.
  * @see [[net.noresttherein.slang.typist.InferTypeParams.Conforms Conforms]]
  * @author Marcin Mościcki
  */
@implicitNotFound("Cannot infer type arguments: type ${T} is not a subtype of ${L} with ${R}.")
sealed abstract class InferTypeParams[-T, L <: R, +R] extends (T => L) {
	def _1 :T <:< L
	def _2 :T <:< R
	def conjunction :T <:< L with R

	def l2r :L <:< R
}



object InferTypeParams {

	/** An isomorphic alias with a shorter name for `InferTypeParams[T, L, R]`.
	  * Guides the compiler to infer the type arguments of type `R` given by its subtype `T`.
	  * Given type parameters `[X &lt;: F[P], P]` and types `C &lt;: F[A] forSome { type C; type A }`,
	  * accepting an implicit argument of `Conforms[T, X, F[P]]` together with an argument of type `T`
	  * will make the inferer will correctly instantiate `X =:= C` and `P =:= A` from an argument `x :C`.
	  * @see [[net.noresttherein.slang.typist.InferTypeParams InferTypeParams]]
	  */
	type Conforms[-T, L <: R, +R] = InferTypeParams[T, L, R]

	@inline def Conforms[T] :Conforms[T, T, T] = unify[T]

	/** Summon an implicitly available evidence `InferTypeParams[T, L, M, R]` witnessing that `T &lt;: L with M with R`. */
	@inline def apply[T, L <: R, R](implicit hint :InferTypeParams[T, L, R]) :InferTypeParams[T, L, R] = hint

	implicit def unify[T] :Conforms[T, T, T] =
		conforms.asInstanceOf[Conforms[T, T, T]]

	private[this] final val conforms = new Conforms[Any, Any, Any] {
		override def _1 = implicitly[Any <:< Any] //implicitly[Any =:= Any]
		override def _2 = implicitly[Any <:< Any]
		override def l2r = implicitly[Any <:< Any]
		override def conjunction = implicitly[Any <:< Any]

		override def apply(x :Any) = x

		override def andThen[A](g :Any => A) = g
		override def compose[A](g :A => Any) = g
	}


}
