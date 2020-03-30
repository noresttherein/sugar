package net.noresttherein.slang.typist




/** A simple value class combining the evidence that `T &lt; L with M with R`, designed to alleviate the limitation
  * of scala type inference of generic type parameters. An implicit instance `InferTypeParams[X, X, X, X]`
  * is available for any type `X`, which can be additionally simplified by the type alias
  * `IsBoth[X, L, R] = TypeParameterInferenceHelper[X, L, R, R]` declared in the companion object.
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
  * conversion with a similar signature. However, Type inference will work for both type `X` and type parameter `String`
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
  *
  * The full, four parameter form of this trait can be analogously used to infer nested type parameters of types
  * such as `Seq[Set[T]]`.
  *
  *
  * By convention, the second type parameter should be the complete inferred type, while the third one its upper bound
  * used to capture its type parameters. For convenience, this trait implements `T => L`, so simply declaring an
  * implicit parameter of this type introduces an implicit conversion from the generic, unbound type to the desired
  * @author Marcin Mościcki
  */
sealed abstract class InferTypeParams[-T, +L, +M, +R] extends (T => L) {
	def left :T <:< L
	def middle :T <:< M //(L with R)
	def right :T <:< R
	def conjunction :T <:< L with M with R
}



object InferTypeParams {
	/** Evidence that `T &lt;: L with R` used to unify a complete type `L` such as `Seq[Int]` with its base type `R`
	  * capturing the type parameter of `L` such as `Iterable[Int]` in this example. An implicit instance
	  * of `IsBoth[T, T, T]` is available for any type `T`, which, thanks to declared variance, covers all cases
	  * of `T &lt;: L with R`.
	  * @see [[net.noresttherein.slang.typist.InferTypeParams]]
	  */
	type IsBoth[-T, +L, +R] = InferTypeParams[T, L, R, R]

	@inline def IsBoth[T] :IsBoth[T, T, T] = unify[T]

	/** Summon an implicitly available evidence `InferTypeParams[T, L, M, R]` witnessing that `T &lt;: L with M with R`. */
	@inline def apply[T, L, M, R](implicit hint :InferTypeParams[T, L, M, R]) :InferTypeParams[T, L, M, R] = hint

	implicit def unify[T] :InferTypeParams[T, T, T, T] =
		evidence.asInstanceOf[InferTypeParams[T, T, T, T]]

	private[this] final val evidence = new InferTypeParams[Any, Any, Any, Any] {
		override def left = implicitly[Any <:< Any]
		override def middle = implicitly[Any <:< Any]
		override def right = implicitly[Any <:< Any]
		override def conjunction = implicitly[Any <:< Any]

		override def apply(v1 :Any) = v1

		override def andThen[A](g :Any => A) = g
		override def compose[A](g :A => Any) = g
	}
}
