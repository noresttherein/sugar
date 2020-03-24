package net.noresttherein.slang.typist




/** A simple value class wrapping a value of type `X &lt;: L with R` designed to alleviate the limitation
  * of scala type inference of generic type parameters. There is an implicit conversion
  * `X => TypeParameterInferenceHelp[X, X]` for any type `X`, which can be abbreviated with the type alias
  * `IsBoth[X, L, R] = X => TypeParameterInferenceHelper[L, R]` declared in the companion object.
  *
  * Given some type constructor `F[T]`, we would often like to declare a method generic with regard to both
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
  * conversion with a similar signature. However, Type inference will work both type `X` and type parameter `String`
  * given to `F`, just not both at the same time:
  * {{{
  *     def m1[X <: F[_]](x :X) = ???
  *     def m2[T](x :F[T]) = ???
  *
  *     m1(new X) //compiles
  *     m2(new X) //compiles
  * }}}
  * By duplicating the occurrence of type `X` in this type signature and having both type parameters covariant,
  * we can now write:
  * {{{
  *     def m[A, X <: F[T], T](x :A)(implicit help :IsBoth[A, X, F[T]]) :(X, T) = help(x).left -> help(x).left.get
  * }}}
  */
class TypeParameterInferenceHelp[+L, +R] private[slang](private val self :L with R) extends AnyVal {
	@inline def left :L = self
	@inline def right :R = self
}



object TypeParameterInferenceHelp {
	type IsBoth[-T, +L, +R] = T => TypeParameterInferenceHelp[L, R]

	@inline implicit def duplicateType[T](subject :T) :TypeParameterInferenceHelp[T, T] =
		new TypeParameterInferenceHelp[T, T](subject)
}
