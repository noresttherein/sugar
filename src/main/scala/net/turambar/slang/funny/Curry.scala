package net.turambar.slang.funny

import net.turambar.slang.funny.Curry.Curried.__
import net.turambar.slang.funny.Curry.{=>:, NextArg}
import net.turambar.slang.typist.LowerBound


/** Represents a type (and function) constructor of a partially applied, curried function `F` which is of the form
  * `X0 => ... => Xn => X => T` for any natural `n`, where X is the type of the first argument after (partial) application.
  * Provides methods for manipulating functions `F` around this argument.
  * @tparam Args[R] result of mapping an intermediate result `(X=>T)` of function `F` to `R`; `F =:= Args[X=>T]`
  * @tparam X type of the first argument after partial application
  * @tparam T result type of function F partially applied to all its arguments up to and including X
  */
sealed abstract class Curry[Args[+R], X, T] { prev =>

	/** Full type of the curried function (including all arguments). */
	type F = Args[X=>T]

	/** Result of partial application of this function `Args[X=>T]` up to and including parameter `X`. */
	type Result = T

	/** Replace X=>T with `G` as the result type of `Args[X=>T]`. */
	type Mapped[+G] = Args[G]

	/** A function which takes argument `W` instead of `X` at this position. */
	type Composed[W] = Args[W=>T]

	/** A function type accepting an additional argument `W` before `X`, equivalent to `(Args :=> W)#F[T]`. */
	type Accepting[W] = Args[W=>X=>T]

	/** A function of 'X' returning this function, equivalent to `(W =>: Args)#F[T]`. */
	type From[W] = W => Args[X=>T]

	/** Provide a fixed value for this argument, removing it from the argument list. Same as `apply(f)(x)` for use
	  * when resolution of apply would be ambiguous.
	  */
	@inline final def set(f :Args[X=>T])(x :X) :Mapped[T] = map[T](f)(_(x))

	/** Provide a fixed value for this argument, removing it from the argument list.
	  * Args[X=>T]or example, the result of `Curry{a :Any => b :Byte => c :Char => s"&dollar;a&dollar;b&dollar;c" }().set(1.toByte)`
	  * (after inlining) would be a function `{a :Any => c :Char => s"&dollar;a&dollar;{1.toByte}&dollar;c" }`.
	  */
	@inline final def apply(f :Args[X=>T])(x :X) :Mapped[T] = map[T](f)(_(x))

	/** Compose the return type `X=>T` of partially applied `F` with another function.
	  * Substitutes the type of next argument `X` with type `W` by mapping the argument with the given function before applying `f`.
	  * Args[X=>T]or example, given a function `f :Args[X=>T] &lt;: D=>O=>X=>T` and `x :W=>X`, the result is `{d :D => o :O => w :W => f(d)(o)(x(w)) }`.
	  */
	@inline final def compose[W](f :Args[X=>T])(x :W=>X) :Mapped[W=>T] = map[W=>T](f){ r :(X=>T) => (w :W) => r(x(w)) }

	/** Create a new function resulting from `f` by inserting an ignored argument of type `W` before argument `X`. */
	@inline final def accept[W](f :Args[X=>T]) :Mapped[W=>X=>T] = map[W=>X=>T](f) { r :(X=>T) => _ :W => r }

	/** Map the result of partial application of this function up to argument `X` (not including).
	  * Args[X=>T]or example, if `Args[X=>T] =:= K=>L=>X=>T`, the result is a function `{k :K => l :L => map(f(k)(l)) }`.
	  * @param map function taking the result of applying Args[X=>T] up until argument `X`.
	  * @return resul
	  */
	def map[O](f :Args[X=>T])(map :(X => T) => O) :Mapped[O]

	/** Partially apply functions `self` and `other` sharing the same arguments up to `X` (this argument, not including)
	  * and combine the results using the passed function into another function of arguments up to this curry point.
	  */
	def combine[Y, O](self :Args[X=>T], other :Args[Y])(combine :(X=>T, Y) => O) :Args[O]

	/** If the result of this partial application is a function `T &lt;: Y=>R`, swap the order of arguments
	  * in function `Args[X=>T]` from `=>X=>Y=>R` to `=>Y=>X=>R`.
	  */
	@inline final def transpose[Y, R](f :Args[X=>T])(implicit ev :T<:<(Y=>R)) :Mapped[Y=>X=>R] = map[Y=>X=>R](f) {
		r :(X=>T) => y :Y => x :X => ev(r(x))(y)
	}

	/** Return representation of partially applying function `Args[Y=>R]` to all arguments already applied in this type. */
	def mapped[Y, R] :Curry[Args, Y, R]

	/** Return an instance representing partial application of function `W=>Args[X=>T]` up to the same argument as in this instance (i.e., before `X`).
	  * Loosely speaking, it 'adds' another argument of type `W` before all arguments of `F`, representing a function `W=>F`.
	  */
	def from[W] :Curry[(W =>: Args)#F, X, T]

	/** Assuming that result type `T` of `F` is itself a function `Y=>R`, skip to the next argument `Y` following `X` in `F`.
	  * @return an instance operating on the result of applying this function to argument `X`. */
	@inline final def apply[Y, R]()(implicit ev :T<:<(Y=>R)) = new NextArg[Args, X, Y, R](mapped[X, Y=>R])

	/** Skip to the next argument, i.e return an instance operating on the result of applying this function to argument `X`.
	  * Same as `apply()`, for when resolution of `this()` would be ambigous.
	  */
	@inline final def __[Y, R](implicit ev :T<:<(Y=>R)) = new NextArg[Args, X, Y, R](mapped[X, Y=>R])
}


/** Operations on curried functions. */
object Curry {

	/** Identity type constructor mapping any type to itself. Used in conjunction with [[:=>]] and [[=>:]]
	  * to denote argument lists consisting of a single argument. For example,
	  * `(X =>: Ident)#F[Y] =:= X => Y =:= (Ident :=> X)#F[Y]`.
	  */
	type Ident[+R] = R

	/** A wrapper for a type constructor `F`, representing a curried argument list `X0 => ... => Xn => X =>`, resulting
	  * from appending a new argument `X` to the argument list `A` denoting `X0 => ... Xn =>` (for any natural `n`).
	  * @tparam A type constructor representing a curried argument list; `A[Y] =:= X0 => ... => Xn => Y`.
	  * @tparam X a new argument type which should be accepted by the resulting type after all arguments of `A`.
	  * @see [[=>:]]
	  */
	type :=>[A[+R], X] = {
		/** Type constructor for curried functions accepting the arguments represented by this type `A :=> X`.
		  * `F[Y]` is a function type `X0 => ... => Xn => X => Y`. It is used by all types here to represent an argument list `X, X0, ..., Xn`. */
		type F[+Y] = A[X=>Y]
	}

	/** A wrapper for a type constructor `F`, representing a curried argument list `X => X0 => ... => Xn =>`,
	  * resulting from prepending a new argument `X` to the argument list `A` denoting `X0 => ... Xn =>` (for any natural `n`).
	  * @tparam X a new argument type which should be accepted by the resulting type before all arguments of `A`.
	  * @tparam A type constructor representing a curried argument list; `A[Y] =:= X0 => ... => Xn => Y`.
	  * @see [[:=>]]
	  */
	type =>:[X, A[+R]] = {
		/** Type constructor for curried functions accepting the arguments represented by this type `X =>: A`.
		  * `F[Y]` is a function type `X => X0 => ... => Xn => Y`. It is used by all types here to represent an argument list `X, X0, ..., Xn`. */
		type F[+Y] = X => A[Y]
	}

	/** Explicitly summon an instance of `Curry[Ident, X, Y]` representing unapplied functions `X=>Y`
	  * and manipulating them at argument `X`. If the result `Y` is also a curried function, returned
	  * type class will provide possibility to manipulate arguments succeeding `X`.
 	  * @tparam X argument type of the represented function type
	  * @tparam Y result type of the represented function type
	  * @return a type class representing a curried function `X=>Y` before application to its argument.
	  */
	@inline final def apply[X, Y] :Curry[Ident, X, Y] = ErasedArg0.asInstanceOf[Arg0[X, Y]] //new Arg0


	/** Explicitly summon an instance of `Curry[Ident, X, Y]` representing unapplied functions `X=>Y`
	  * and manipulating them at argument `X`. This is the same as [[apply()]], the argument is irrelevant
	  * and used only to enable automatic inference of type parameters `X` and `Y`, so they needn't be
	  * provided explicitly.
	  * @tparam X argument type of the represented function type
	  * @tparam Y result type of the represented function type
	  * @param instance any function of the type we would like manipulate, used only for type inference.
	  * @return a type class representing a curried function `X=>Y` before application to its argument.
	  */
	@inline final def apply[X, Y](instance :X=>Y) :Curry[Ident, X, Y] = ErasedArg0.asInstanceOf[Arg0[X, Y]] //new Arg0



	/** Operations on the first argument of this function. */
	final class Arg0[X, Y] extends Curry[Ident, X, Y] {

		override def mapped[A, B]: Curry[Ident, A, B] = Curry[A, B] //todo: just cast this

		override def from[W]: Curry[(W =>: Ident)#F, X, Y] =
			new NextArg[Ident, W, X, Y](new Arg0[W, X=>Y])

		def map[G](f :X=>Y)(res: (X=>Y) => G) :G = res(f)

		override def combine[U, O](f :X=>Y, other: U)(combiner: (X => Y, U) => O): O =
			combiner(f, other)
	}
	private[this] final val ErasedArg0 = new Arg0[Any, Any]


	/** Operations on `n+1`-th argument `Y` of function `C[X=>Y=>A]`. */
	final class NextArg[C[+G], X, Y, A](private[funny] val prev :Curry[C, X, Y=>A]) extends Curry[(C :=> X)#F, Y, A] {

		/** Return an instance representing the same argument, but of function `W=>F`. `type Mapped[G] = W=>C[X=>G]`. */
		override def from[W]: Curry[(W =>: (C :=> X)#F)#F, Y, A] =
			new NextArg[(W =>: C)#F, X, Y, A](prev.from[W])

		override def mapped[W, B]: Curry[(C :=> X)#F, W, B] = prev.mapped[X, W=>B].__ //todo: just cast this

		override def map[G](f :F)(res: (Y => A) => G): prev.Mapped[X => G] =
			prev.map[X=>G](f) { g :(X=>Y=>A) => x :X => res(g(x)) }

		override def combine[U, O](self :F, other: C[X=>U])(combine: (Y => A, U) => O): C[X=>O] =
			prev.combine[X=>U, X=>O](self, other)((me :X=>Y=>A, her :X=>U) => { x :X => combine(me(x), her(x))} )
	}







	/** Witnesses that `F &lt;: ... => X=>Y` or (`F &lt;: this.Mapped[X=>Y]`), i.e.
	  * that `Y` is a type returned by (possibly partial) application of function `F`.
	  * Implicit instances are available for 'final' return types, i.e first returned type which is not a function itself.
	  * Note that in case of not fully instantiated types, 'final' is a relative term and represents the static knowledge
	  * about `F` in the context where this instance was summoned.
	  * @tparam F &lt;: X=>X function in a curried form which returns `Y` after partial application, as witnessed by this instance.
	  * @tparam X last argument of `F` before returning `Y`, i.e. `F &lt;: X1=>..=>Xn=>X=>Y` for some `n>=0`.
	  * @tparam Y returned type after applying `F` to a series of parameters (possibly all).
	  */
	sealed abstract class ReturnType[F, X, Y] {
		/** Result type of mapping partial application of `F` up until and not including argument `X`.
		  * It is the type resulting from substituting `X=>Y` with `R` in `F`.
		  * Same as `curry.Mapped[R] forSome { val curry :Curry[Mapped, X, Y] }`.
		  */
		type Mapped[+R]

//		/** Upper bound for all curried functions taking the same number of arguments as `F` (according to this instance).
//		  * Assuming `F =:= X0 => ... => Xn => X => Y`, concrete subclasses will define it as `Nothing => ... => Nothing => Any`,
//		  * where argument `Nothing` occurs `n+1` times in `UpperBound`.
//		  * Used to check conformance of kinds between curried functions.
//		  */
//		type UpperBound >: F //Mapped[Nothing=>Any]
//
//		/** Lower bound for all curried functions taking the same arguments as `F` (according to this instance).
//		  * Assuming `F =:= X0 => ... => Xn => X => Y`, concrete subclasses will define it as `X0 => ... => Xn => X => Nothing`,
//		  * where argument `Any` occurs `n+1` times in `LowerBound`.
//		  */
//		type LowerBound <: F with Mapped[X=>Nothing] with UpperBound


		/** X curry instance representing partial application of `X=>X` until argument `X`, i.e. returning `X=>Y`. */
		def curry :Curry[Mapped, X, Y]

		/** As this instance witnesses that `X=>X &lt;: Mapped[X=>Y]`, convert the given function so that it conforms to the return type.
		  * This may, but need not return the same instance.
		  */
		def cast(f :F) :Mapped[X=>Y]
	}

	sealed class FallbackReturnType private[Curry] {

		/** Low priority implicit return type attesting that `Y` is the return type of `X=>Y` for when no other implicit value
		  * is available for type `Y`.
		  */
		implicit def returnedValue[X, Y] :ReturnType[X=>Y, X, Y] { type Mapped[+G] = G; /*type UpperBound=Nothing=>Any; type LowerBound=X=>Nothing*/ } =
			new ReturnType[X=>Y, X, Y] {
				type Mapped[+G] = G
//				type UpperBound = Nothing => Any
//				type LowerBound = X=>Nothing

				override def curry = Curry[X, Y]

				override def cast(f: X => Y) :X=>Y = f

			}
	}

	object ReturnType extends FallbackReturnType {

		/** Implicit evidence that `R` is the return type of `X=>Y=>Z` if `R` is the return type of `Y=>Z`. */
		implicit def returnedFunction[X, Y, Z, L, R](implicit res :ReturnType[Y=>Z, L, R])
				:ReturnType[X=>Y=>Z, L, R] { type Mapped[+G] = X=>res.Mapped[G]; /*type UpperBound = Nothing=>res.UpperBound; type LowerBound = X=>res.LowerBound*/ } =
			new ReturnType[X=>Y=>Z, L, R] {
				type Mapped[+G] = X => res.Mapped[G]

//				type UpperBound = Nothing => res.UpperBound
//				type LowerBound = X => res.LowerBound

				override def curry: Curry[Mapped, L, R] = res.curry.from[X]

				override def cast(f: (X) => (Y) => Z) :X=>res.Mapped[L=>R] =
					{x :X => res.cast(f(x)) }
			}
	}






	/** Classes needed for performing function unification and mapping transforming functions by mapping or combining their result values.
	  * Import `Curry.:*:` to enrich function types with unification and reduce operations.
	  */
	object FunctionUnification {

		/** Pimps any function type `G` with a right-associative method `:*:` for pairing it with another function `F`.
		  * Returned `F:*:G` instance has an implicit conversion via overloaded `Curry.:*:` to `FunReduce` which can be used
		  * to create another function accepting same (as defined by term unification of `F` and `G`) arguments and returning
		  * the result of combining return values of `f` and `g` for these arguments with a reducing function.
		  */
		final class FunctionFactor[G <: (_ => _)](private val g :G) extends AnyVal {

			/** Bind together this (right-hand value) function `G` with another function `f :F` into a `F:*:G` instance
			  * which can be used to combine both functions into a single function by unifying argument lists of `F` and `G`
			  * and combining values returned by both for the given arguments with a function operating on their return types.
			  */
			@inline def :*:[F <: (_ => _ )](f :F) : F:*:G = new :*:(f, g)
		}


		/** Denotes term unification on two curried function types `F` and `G`. Unification in this context carries
		  * two types of information : shared argument list defined by `this.Returning` and return types of both these
		  * functions after unification. Assuming `F =:= F_1 => F_2 => .. => F_n => L`, `G =:= G_1 => G_2 => .. => G_m => R`,
		  * and that `n &lt; m` (the other case is symmetrical), unified forms of these functions would be `F &lt;: LB[F_1, G_1] => ... => LB[F_n, G_n] => (L)`
		  * and `G &lt;: LB[F_1, G_1] => ... => LB[F_n, G_n] => (G_{n+1} => ... => R)`, where `LB[X, Y]` denotes maximum lower bound of types `X, Y`
		  * and the types in parenthesis' are the return types of unified forms.
		  * Note that there is no requirement for this unification to be maximal, i.e. that either `LeftResult` or `RightResult` is not
		  * a function of a single argument, but all implicit values provided by the companion object
		  * are maximal in terms of static types of arguments.
		  */
		sealed abstract class Unification[F, G] private[FunctionUnification] {
			/** Result type of `F` after unification with `G` (might be another function, as it doesn't necessarily 'drop' all arguments of `F`. */
			type LeftResult
			/** Result type of `G` after unification with `F` (might be another function, as it doesn't necessarily 'drop' all arguments of `G`. */
			type RightResult

			/** Curried function type taking arguments shared by `F` and `G`
			  * (that is, which arguments are lower bounds of respective arguments of `F` and `G`) and returning value of type `O`.
			  */
			type Returning[O]

			/** Create a function sharing the unified argument list with both `F` and `G` and returning the result of applying the
			  * given function to the values returned by applying them to these arguments.
			  */
			def combine[O](f :F, g :G)(combine :(LeftResult, RightResult)=>O) :Returning[O]
		}

		/** Low priority implicit [[FunctionUnification]] values. */
		sealed abstract class FallbackUnification {

			/** Fallback implicit result of unifying two functions `X1=>Y1` and `X2=>Y2` as functions of `X >: X1, X2`
			  * and giving return types `Y1` and `Y2` as their final return types after unification of arguments.
			  * This implicit value is used only when `Y1` and `Y2` aren't both functions and no further recursion can be performed.
			  */
			implicit def fallbackUnification[X1, X2, X, Y1, Y2](implicit argType :LowerBound[X1, X2, X])
					:Unification[X1=>Y1, X2=>Y2] { type LeftResult=Y1; type RightResult=Y2; type Returning[O] = X=>O } =
				new Unification[X1=>Y1, X2=>Y2] {
					type LeftResult = Y1
					type RightResult = Y2
					type Returning[O] = X=>O

					override def combine[O](f: X1 => Y1, g: X2 => Y2)(combine: (Y1, Y2) => O): X=>O =
						{ x :X => combine(f(argType.left(x)), g(argType.right(x))) }
				}
		}


		/** Performs term unification on two curried function types `F` and `G`. Unification in this context carries
		  * two types of information : shared argument list defined by `[[FallbackUnification.Returning]]` and return types of both these
		  * functions after unification. Assuming `F =:= F_1 => F_2 => .. => F_n => L`, `G =:= G_1 => G_2 => .. => G_m => R`,
		  * and that `n &lt; m`, unified forms of these functions would be `F &lt;: LB[F_1, G_1] => ... => LB[F_n, G_n] => (L)` and
		  * `G &lt;: LB[F_1, G_1] => ... => LB[F_n, G_n] => (G_{n+1} => ... => R)`, where `LB[X, Y]` denotes maximum lower bound of types `X, Y`
		  * and the types in parenthesis' are the return types of unified forms.
		  * Note that there is no requirement for this unification to be maximal, i.e. that either `LeftResult` or `RightResult` is not
		  * a function of a single argument, but all implicit values provided by the companion object
		  * are maximal in terms of static types of arguments.
		  */
		object Unification extends FallbackUnification {
			/** Given unification of two types `Y1` and `Y2` (assumed to be functions), perform unification of any two functions
			  * `X1=>Y1` and `X2=>Y2` by listing the lower bound of `X1` and `X2` as unified argument type, and passing on
			  * result types reported for `Y1` and `Y2` (after unification).
			  */
			implicit def resultUnification[X1, X2, X, Y1, Y2](implicit results :Unification[Y1, Y2], argType :LowerBound[X1, X2, X])
					:Unification[X1=>Y1, X2=>Y2] { type LeftResult = results.LeftResult; type RightResult = results.RightResult; type Returning[O] = X => results.Returning[O] } =
				new Unification[X1=>Y1, X2=>Y2] {
					type LeftResult = results.LeftResult
					type RightResult = results.RightResult
					type Returning[O] = X => results.Returning[O]

					override def combine[O](f: X1 => Y1, g: X2 => Y2)(combine: (results.LeftResult, results.RightResult) => O): X => results.Returning[O] =
						{ x :X => results.combine(f(argType.left(x)), g(argType.right(x)))(combine) }
				}
		}


		/** Given two function types `F` and `G` and their unification type specified as their common argument list and types returned by applying them to these arguments,
		  * allow to create a unification function resulting from applying both to their common arguments and combining their results with an arbitrary function.
		  * @tparam F 'left' curried function type in the form of `F1=>...=>Fn=>Y`, `F &lt;: Returning[Y]`
		  * @tparam Y type returned by `F` after applying to arguments common with `G`
		  * @tparam G 'right' curried function type in the form of `G1=>..=>Gn=>Z`, `~G &lt;: Returning[Z]`
		  * @tparam Z type returned by `G` after applying to arguments common with `F`
		  * @tparam R curried function type with return type `O` (type parameter of `R`) and which arguments are lower bounds of respective arguments of `F` and `G`.
		  */
		class FunReduce[F, Y, G, Z, R[O]](f :F, g :G)(implicit val unification :Unification[F, G] { type LeftResult=Y; type RightResult=Z; type Returning[O] = R[O] }) {
			type Returning[O] = unification.Returning[O]

			/** Create a new function `r` which returns the value given by applying passed `reduce` function
			  * to the results of applying functions `f` (left-hand argument) and `g` (right-hand argument) to its arguments.
			  * Returned function is in the curried form and takes 'n' arguments as specified by the asssociated unification instance
			  * (in case of implicit `Unification` values the minimum of `f` and `g` argument numbers), where each argument is a subtype
			  * of the respective arguments of `f` and `g`.
			  */
			def apply[O](reduce :(Y, Z)=>O) :R[O] = unification.combine[O](f, g)(reduce)
		}

	}
	import FunctionUnification._

	/** Pairs two values (presumably curried functions) for subsequent combining them into a single function by
	  * the means of a function operating on their result values. This class has no methods; instead, there is
	  * an implicit conversion with the shared name (so importing this symbol will import the conversion at the same time)
	  * to `FunReduce`, which provides `apply()` method performing the actual reducing of these functions. This is because
	  * unification of their argument lists relies on implicit parameters, and having a method with implicit parameter list
	  * here would prevent us from directly calling `apply` on the returned function in the shortened form.
	  *
	  * Instances of this class can be created by invoking `:*:` method on the right-hand argument, which is made available
	  * by an implicit conversion function under the same symbol (so again, importing `:*:` will provide all conversions needed
	  * for sugared syntax).
	  * @param _1 function on the left side of the operator `:*:` used to create this instance
	  * @param _2 function on the right side of the operator `:*:` used to create this instance
	  * @tparam F function on the left side of the operator `:*:` used to create this instance
	  * @tparam G function on the right side of the operator `:*:` used to create this instance
	  */
	final class :*:[F, G](private[Curry] val _1 :F, private[Curry] val _2 :G)

	/** Enrich function type `G` with a right-associative method `:*:` which will pair it with another function so both
	  * can be combined into a single function by operating on their respective return types.
	  */
	@inline implicit def :*:[G <: (_ => _)](g :G) : FunctionFactor[G] = new FunctionFactor[G](g)

	/** Find the maximum unification of functions `F` and `G` (understood as list of arguments which can be given to both
	  * of them and return types of these applications). Returned object provides an `apply` method accepting a function
	  * operating on the return types of unified forms of `F` and `G` and returning a new function which will use it
	  * to produce the result.
	  */
	@inline implicit def :*:[F, Y, G, R](funs :F:*:G)(implicit unification :Unification[F, G])
			: FunReduce[F, unification.LeftResult, G, unification.RightResult, unification.Returning] =
		new FunReduce[F, unification.LeftResult, G, unification.RightResult, unification.Returning](funs._1, funs._2)(unification)






	/** Base trait for partially applied functions introduced as a workaround for no implicit resolution of types of higher kinds.
	  * Wraps a function `F` of type `X0 => ... => Xn => X => T` and represents its partial application up to, not including,
	  * argument `X`. All instances `c :PartiallyApplied[X, T]` are also instances of `Curried[c.Mapped, X, T]`.
	  * @see [[Curried]]
	  */
	sealed abstract class PartiallyApplied[X, T] {
		/** Type of a function taking the same arguments up to `X` exclusively, but returning type `R` instead of `X=>T`. */
		type Mapped[+R]

		/** The whole function `F` itself being wrapped by this instance. */
		val unapplied :Mapped[X=>T]

		/** Provide a fixed value for argument `X`, removing it from `F`'s argument list. */
		def set[Y, O](x :X)(implicit ev :T<:<(Y=>O)) :Curried[Mapped, Y, O]

		/** Compose the returned function with a given function `W=>X`, essentially changing argument type from `X` to `W`.
		  * Wrapped function will map all arguments provided for `W` with the given function before invoking this function.
		  * @tparam W new argument type
		  * @return a partially applied function representation resulting from substituting argument `X` with `W` in this instance.
		  */
		def compose[W](x :W=>X) :Curried[Mapped, W, T]

		/** Map the returned function to another result type `G` (likely also a function), returning a function of all
		  * arguments preceeding `X` in `F` and returning `G`.
		  * @tparam G new result type for the preceeding argument list, substituting `X=>T` in `F`.
		  */
		def mapped[G](map :(X => T) => G) :Mapped[G]

		/** Map the returned function to another function `Y=>U`, returning a curried
		  * function of all arguments of `F` preceeding `X`, with result type `Y=>U` instead of `X=>T`.
		  * @tparam Y new argument type substituting argument `X` in `F`
		  * @tparam U new result type of `F` applied to all `F`'s arguments preceeding `X` as well as new argument `Y`.
		  *           Substitutes type `T` in `F`.
		  */
		def map[Y, U](map :(X=>T) => (Y=>U)) :Curried[Mapped, Y, U]

		/** Assuming that return type `T` is itself a function `Y=>U`, skip to the next argument `Y` in `F`.
		  * @tparam Y argument type succeeding `X` in `F`
		  * @tparam U return type of `F` applied to all arguments preceeding X, as well as `X` and `Y`.
		  * @return a curried function representing `F` partially applied to all arguments up to and including `X`.
		  */
		def next[Y, U](implicit ev :T<:<(Y=>U)) :Curried[(Mapped :=> X)#F, Y, U]

		/** Given another function `H <: X0 => ... => Xn => Y => U` (taking the same arguments as `F` up to not including `X`),
		  * create a new function with arguments `X0, ..., Xn`, applying them to both this and the argument function and combining
		  * their results with the given operator.
		  * @param other another curried function sharing arguments with `F` preceeding `X`.
		  * @param combine a function accepting result types of this function and `other` and providing the result for the created function
		  * @tparam Y argument of `H` in the same position as `X` in `F`
		  * @tparam U return type of partially applied `H`, corresponding to type `T` in `F`.
		  * @tparam G return type of the combined function taking place of type `X=>T` in `F`.
		  * @return a function accepting shared arguments of `this` and `other` and returning value resulting from combining their
		  *         returned results with function `combine`.
		  */
		def combined[Y, U, G](other :Mapped[Y=>U])(combine :(X=>T, Y=>U) => G) :Mapped[G]

		/** Create a function of type `X0 => ... => Xn => W => X => T`, accepting a new ignored argument `W` before argument `X`.
		  * Allows to unify types of several functions of similar arguments, by making `F` conform to desired type `Mapped[W=>X=>T]`.
		  * @tparam W new ignored argument preceeding `X` in the returned function
		  * @return a wrapper function applying this function to all arguments it receives except for `W`.
		  */
		def accept[W] :Curried[Mapped, W, X=>T]
	}




	object PartiallyApplied {

		/** Extends a `Curried` partially applied function returning a function `Y=>T`, providing methods for advancing to the next argument `Y`. */
		@inline implicit final def CurryOn[X, Y, T](curried :PartiallyApplied[X, Y=>T]) :CurryOn[curried.Mapped, X, Y, T] =
			new CurryOn[curried.Mapped, X, Y, T](curried)

		/** Extends a curried function type of more than one argument with methods for advancing through argument list. */
		@inline implicit final def CurryOn[X, Y, T](f :X=>Y=>T) :CurryOn[Ident, X, Y, T] = new CurryOn[Ident, X, Y, T](Curried(f))

		/** Extends a `Curried` partially applied function returning a function `Y=>T`, providing methods for advancing to the next argument `Y`.
		  * This class is an implicit extension of `PartiallyApplied` (and `Curried`) instances. It defines methods which would require
		  * implicit parameters if declared within parent `Curried`, which would prevent from directly calling apply on their results.
		  * By moving the implicit parameters to be the part of implicit conversion, we were able to remove them from the methods signature.
		  */
		final class CurryOn[A[+G], X, Y, T](private val curried :PartiallyApplied[X, Y=>T] { type Mapped[+G] = A[G] }) extends AnyVal  {

			/** Move to the next argument, i.e. for `this` representing partial application of `F` until return type `X=>Y=>T`,
			  * return an instance representing the result of further application of `F` to `X`, with return type `Y=>T`.
			  * @return a `Curried` instance for the same underlying function, but representing applying it to the next argument `X`.
			  */
			@inline def apply() :Curried[(A :=> X)#F, Y, T] = curried.next

			/** Take an additional, ignored argument of type `W` and automatically 'apply' it, so that curry point remains
			  * before argument `X` (and after the new argument `W`). You may specify the type of the parameter
			  * by providing type arguments either to this method, or the parameterless `Curried.__.apply[X]`, for example:
			  * `Curry(f)()(__[W])`.
			  * @param newArg a marker object used solely to specify the type of the argument and offer syntax similar
			  *               to the scala convention of using `_` as a wildcard.
			  * @tparam W new, additional argument type
			  * @return a `Curried` function resulting from mapping the intermediate result `X=>Y=>T` of this function to
			  *         a constant function `W=>X=>Y=>T` returning the given result for all arguments `W`.
			  *         Returned instance will represent the application of that function up to argument `W` and before `X`.
			  */
			@inline def apply[W](newArg : __[W]) :Curried[(A :=> W)#F, X, Y=>T] = curried.accept[W].next

			/** Provide a fixed value for this argument, removing it from the argument list and return a `Curried`
			  * instance representing first parameter after `X`, providing one exists. Created function is the same
			  * as with `applied(x)`, but allows subsequent modification in chained calls. This method is equivalent to `self.set(x)`.
			  * @return a Curried instance representing same application point (before argument `Y`) in the transformed function.
			  */
			@inline def apply(x :X) :Curried[A, Y, T] = curried.set(x)

			/** Substitute this argument by mapping intended argument type `W` to `X` before applying `f`.
			  * For example, given a function `f :F &lt;: D=>O=>X=>A` and `x :W=>X`, the result is `{d :D => o :O => w :W => f(d)(o)(x(w)) }`.*
			  * This method is equivalent to `self.swap(swap).>>`.
			  * @tparam W argument type replacing `X` in `F`.
			  * @return a `Curried` instance representing the next position (after argument `W` and before `Y`) in the transformed function.
			  */
			@inline def apply[W](swap :W=>X) :Curried[(A :=> W)#F, Y, T] = curried.compose(swap).next


			/** Move to the next argument, i.e. for `this` representing partial application of `F` until return type `X=>Y=>T`,
			  * return an instance representing the result of further application of `F` to `X`, with return type `Y=>T`.
			  * Same as `apply()`, but avoids ambiguity if this instance is the return value of a function taking implicit parameters.
			  * @return a `Curried` instance representing the same function, applied to its next argument `X`.
			  */
			@inline def __ :Curried[(A :=> X)#F, Y, T] = curried.next

			/** Transpose next argument `X` with following it argument `Y`, automatically advancing over `Y` to maintain the application
			  * point before `X`. This is equivalent to `this.transpose.next`
			  * @return a `Curried` instance for a function resulting from mapping the intermediate result `X=>Y=>T` to function `Y=>X=>T`
			  *         by swapping the application order of arguments `X` and `Y`, partially applied up to argument `Y` (and before argument `X`).
			  * @see [[<>>>]]
			  * @see [[transpose]]
			  */
			@inline def <>> :Curried[(A :=> Y)#F, X, T] = curried.map[Y, X=>T](transposition).next

			/** Transpose next argument `X` with following it argument `Y`, automatically advancing over both.
			  * This is equivalent to `this.transpose.next.next`.
			  * @param ev proof that `T` is a `Function1` and thus can be represented as a partially applied curried function.
			  * @tparam Z argument following `Y` in `F`
			  * @tparam R result of applying `F` to all arguments ending with `X, Y, Z`.
			  * @return a `Curried` instance for a function resulting from mapping the intermediate result `X=>Y=>T` to function `Y=>X=>T`
			  *         by swapping the order of arguments, partially applied up to and including argument `Y` (assuming that `T` is a function).
			  * @see [[<>>]]
			  * @see [[transpose]]
			  */
			@inline def <>>>[Z, R](implicit ev :T<:<(Z=>R)) :Curried[((A :=> Y)#F :=> X)#F, Z, R] =
				curried.map[Y, X=>T](transposition).next.next(ev)

			/** Convert `F` into a function with arguments `X, Y` in reverse order, i.e. from `Mapped[X=>Y=>T]` to `Mapped[Y=>X=>T]`. */
			@inline def transposed :A[Y=>X=>T] = //curried.curry.transpose(curried.unapplied)
				curried.mapped[Y=>X=>T](transposition) //{ res => y :Y => x :X => res(x)(y) }

			/** Return a `Curried` instance representing partial application of transposition of arguments `X, Y` in `F`.
			  * Represented curried function takes arguments `X` and `Y` in reverse order, and is partially applied to same arguments
			  * as this instance (that is, all arguments preceding `X` in `F`).
			  */
			@inline def transpose :Curried[A, Y, X=>T] = curried.map[Y, X=>T](transposition)

			@inline private def transposition = { res :(X=>Y=>T) => y :Y => x :X => res(x)(y) }

//			@inline def andThen[U](res :T=>U)
		}


	}

	/** A wrapper over a curried function `f :F` (with an arbitrary number of single-element argument lists) `F &lt;: A[X=>T]`, representing
	  * partial application of `f` up to (not including) `X` and providing methods for modifying this function from this argument on.
	  * Naming convention to which (most of) methods of this class conform is that an imperative verb form is used by methods returning
	  * another `Curried` instance (and thus allowing for modification of subsequent argumetns), while past perfect is reserved for
	  * methods returning the underlying function after requested transformation, signalling end of transformations; in particular, `unapplied`
	  * returns the underlying function itself as-is. In addition, there is an implicit conversion from instances of `Curried` which don't
	  * represent the last argument of the function (i.e. `T &lt;: _=>_` ) to `CurryOn` instance which provide much nicer `apply` syntax
	  * which advances to the next argument after each operation allowing for more succint - and hopefully readable - code.
	  *
	  * Example: `Curried{b:Byte => s:Short => i:Int => l:Long => s"\$b\$s\$i\$l"}()((_:String).toShort)(42).unapplied :Byte=>String=>Long=>String`.
	  * @param unapplied underlying function to which all functions created by this instance will delegate
	  * @param curry type constructor for partial application
	  * @tparam A Represents all arguments of `F` before `X`; `A[G] =:= A1=>A2=>...=>An=>G`, providing `F =:= A1=>A2=>...=>an=>X=>T`.
	  * @tparam X type of the first argument after partial application of F
	  * @tparam T result type of applying `f` with all arguments up to `X` (inclusive), possibly another function.
	  */
	final class Curried[A[+R], X, T](val unapplied :A[X=>T])(val curry :Curry[A, X, T]) extends PartiallyApplied[X, T] {
		/**Full type of this function. */
		type F = A[X=>T]
		/** Result type of partially applying `Fun` up to and including `X`. */
		type Applied = T
		/** Function `A1 => ... => An =>R`, where `A1..An` are all arguments of F preceeding `X`. */
		type Mapped[+R] = A[R]
		/** Type of `F` after replacing `X` with `W`. */
		type Composed[W] = Mapped[W=>T]

		/** Represent this instance (and the underlying function) as a function taking a sub-type `W` of argument `X` instead of `X`. */
		@inline def as[W<:X] :Curried[A, W, T] = new Curried[A, W, T](unapplied)(curry.mapped[W, T]) //todo: just cast this

		/** Return the underlying function as its supertype accepting narrower argument type at this position. */
		@inline def arg[W<:X] :A[W=>T] = unapplied

		/** Provide a fixed value for this argument, removing it from the argument list.
		  * @return a function with same arguments as the underlying function but with `X` ommited, which invokes this function passing `x`
		  *         for this argument and its own arguments for others.
		  */
		@inline def applied(x :X) :Mapped[T] = curry.set(unapplied)(x)


		/** Provide a fixed value for this argument, removing it from the argument list and return a `Curried`
		  * instance representing first parameter after `X`, providing one exists. Created function is the same
		  * as with `apply(x)`, but allows subsequent modification in chained calls. If this instance represents
		  * the last argument, you may use `applied(x)` instead which returns the underlying function instead of
		  * a `Curried` instance. There is also an `apply` variant of this method available by implicit conversion
		  * to [[PartiallyApplied.CurryOn]]: `this(x)`.
		  * @return a Curried instance representing same application point (before argument `Y`) in the transformed function.
		  * @see [[PartiallyApplied.CurryOn#apply(X)]]
		  */
		@inline def set[Y, R](x :X)(implicit ev :T<:<(Y=>R)) :Curried[A, Y, R] =
			new Curried[A, Y, R](curry.map[Y=>R](unapplied){ res :(X=>T) => ev(res(x))})(curry.mapped[Y, R])


		/** Composes the result of partial application of `F` with a function mapping new argument type `W` to the corresponding
		  * argument `X` in `F`. Has the effect of substituting this argument with a new argument type and mapping it on each
		  * application before invoking this function.
		  * For example, given a function `f :F &lt;: M=>I=>X=>T` and `x :N=>X`, the result is a function of type `M=>I=>N=>T`:
		  * `{m :M => i :I => n :N => f(m)(i)(x(n)) }`.
		  * @return a function resulting from substituting the argument `X` in the underlying function to `W` and mapping its value
		  *         using the passed function.
		  */
		@inline def composed[W](x :W=>X) :Mapped[W=>T] = curry.compose(unapplied)(x)


		/** Compose the function resulting from partially applying `F` up to this point with another function specifying
		  * new argument type and return a `Curried` instance representing its partial application to the same arguments (up until `W`).
		  * Has the effect of substituting argument `X` by mapping intended argument type `W` to `X` before applying `f`.
		  * For example, given a function `f :F &lt;: M=>I=>X=>T` and `x :N=>X`, the result is a function of type `M=>I=>N=>T`:
		  * `{m :M => i :I => n :N => f(m)(i)(x(n)) }`.
		  * There is also an `apply` variant of this method advancing to the next argument,
		  * available by implicit conversion to [[PartiallyApplied.CurryOn]] : `this((w :W) => w.toX)`.
		  * @return a `Curried` instance representing the same position (before argument `W`) in the transformed function.
		  */
		@inline def compose[W](x :W=>X) :Curried[A, W, T] =
			new Curried[A, W, T](curry.compose(unapplied)(x))(curry.mapped[W, T])


		/** Map the result of partial application of this function up to argument `X` (not including).
		  * For example, if `F =:= K=>L=>X=>T`, the result is a function `{k :K => l :L => map(f(k)(l)) }`.
		  * @param map function taking the result of applying F up until argument `X`.
		  * @see [[returning]]
		  * @see [[map]]
		  */
		@inline def mapped[G](map :(X => T) => G) :Mapped[G] = curry.map(unapplied)(map)

		/** Map the result of this partial application to another function `Y=>U`, returning a `Curried` instance representing
		  * the new function.
		  */
		@inline def map[Y, U](map :(X=>T) => (Y=>U)) :Curried[A, Y, U] =
			new Curried[A, Y, U](curry.map(unapplied)(map))(curry.mapped[Y, U])

		/** Map the result of partial application of this function up to argument X. Differs from [[mapped]] in that
		  * it maps the value returned after applying to `X`, while `mapped` maps whole function `X=>T`.
		  */
		@inline def returning[Y](map :T=>Y) :Mapped[X=>Y] =
			curry.map(unapplied){ _ andThen map }

		/** Map the result of partial application of this function up to argument X to another function. Differs from `map` in that
		  * it maps the value returned after applying to `X`, while `map` maps whole function `X=>T`.
		  * @return a `Curried` instance representing the function resulting from applying `F` to all arguments up to `X`
		  *         and mapping the result `T` to a function `Y=>U`, before applying to `Y`.
		  */
		@inline def andThen[Y, U](map :T => Y => U) :Curried[(A :=> X)#F, Y, U] =
			new Curried[(A :=> X)#F, Y, U](curry.map(unapplied)(_ andThen map))(curry.mapped[X, Y=>U]())



		/** Create a function taking an additional, ignored argument `W` before argument `X`. */
		@inline def accepting[W] :A[W=>X=>T] = mapped[W=>X=>T]{ f => _ => f }

		/** Insert a new, ignored argument `W` before `X`, remaining in the same application point (before `W`). */
		@inline def accept[W] :Curried[A, W, X=>T] = new Curried[A, W, X=>T](accepting[W])(curry.mapped[W, X=>T])



		/** Create a function `W=>F` ignoring its first argument and returning this function `F` (unapplied).
		  * This essentially prepends a new ignored argument of type `W` before all arguments specified by [[Mapped]].
		  * @return a constant function returning [[unapplied]] for all arguments.
		  */
		@inline def returned[W] :W=>A[X=>T] = (w :W) => unapplied


		/** Create an instance representing partial application of function `W=>F` up to `X` (excluding).
		  * This essentially prepends a new ignored argument before all arguments specified by [[Mapped]].
		  * @return a `Curried` instance representing a constant function returning this function for all arguments,
		  *         applied up to the same argument `X`.
		  */
		@inline def from[W] :Curried[(W =>: A)#F, X, T] =
			new Curried[(W =>: A)#F, X, T]((w :W) => unapplied)(curry.from[W])


		/** Create an instance representing partial application of function `W=>F` up to `X` (excluding).
		  * This essentially prepends a new ignored argument before all arguments specified by [[Mapped]].
		  * @return a `Curried` instance representing a constant function returning this function for all arguments,
		  *         applied up to the same argument `X`.
		  */
		@inline def =>:[W](w :__[W]) :Curried[(W =>: A)#F, X, T] =
			new Curried[(W =>: A)#F, X, T]((w :W) => unapplied)(curry.from[W])


		/** Given another function sharing with `F` arguments preceding X, and a function combining results of partial application
		  * of both functions, return a single function taking shared arguments and invoking the combining function on results
		  * of applying `this.result` and `other` to received arguments.
		  */
		@inline def combined[Y, B, G](other :A[Y=>B])(combine :(X=>T, Y=>B) => G) :A[G] = curry.combine(unapplied, other)(combine)

		/** Given another `Curried` instance representing a function sharing the arguments preceding `X` with this function,
		  * combine the results of partial application to shared arguments using the function passed as the second argument.
		  * Note that this method is right-associative (and correspondingly, `this` function is the second argument of `combine`.
		  * Usage : `(Curried(other)()() :*: Curried(self)()()){ case (her, me) => ??? }`.
		  */
		@inline def *[Y, B, G](other :Curried[A, Y, B])(combine :(X=>T, Y=>B) => G) :A[G] =
			curry.combine(unapplied, other.unapplied)(combine)


		/** Skip to the next argument, i.e return an instance operating on the result of applying this function to argument `X`.
		  * Represents using an identity function on argument `x` before taking any subsequent arguments. An implicit conversion
		  * exists providing the same functionality with sweeter syntax (and without the implicit argument list): `this()` and/or `this.__`,
		  * for when `apply()` would be ambiguous. This duplicity stems from the fact that implicit argument list required for implementation
		  * as a method prevents from shortened `apply` calls on the result of `>>`, but on the other hand IntelliJ IDE doesn't currently recognise
		  * implicit conversions using dependent types (and scala doesn't allow for implicit conversions to types parameterized with higher kinds).
		  */
		@inline def next[Y, R](implicit ev :T<:<(Y=>R)) :Curried[(A :=> X)#F, Y, R] =
			new Curried[(A :=> X)#F, Y, R](curry.map[X=>Y=>R](unapplied)((rest :X=>T) => rest andThen ev))(curry())


	}


	abstract sealed class LowPriorityCurriedImplicits private[Curry] {
		/** Implicit conversion of a `Curried[C, A, B]` to the underlying function type `C[A=>B]`. */
		@inline implicit def implicitlyCurried[A, B](curried :PartiallyApplied[A, B]) :curried.Mapped[A=>B] =
			curried.unapplied

	}

	/** Wraps a function in a curried form (with single-argument lists) into a `Curried` class representing partial application
	  * of that function and providing convenient methods for mapping the underlying function without applying it to actual arguments.
	  */
	object Curried extends LowPriorityCurriedImplicits {

		/** Extension methods for modifying curried functions at their first argument (and a source for advancing to subsequent arguments. */
		@inline def apply[A, B](f :A=>B) :Curried[Ident, A, B] = new Curried[Ident, A, B](f)(new Arg0)

		/** Lifts a constant of type `C` to a function `Any=&gt;C` ignoring its argument; primarily used to make the value conform
		  * to a common curried function type (by optional further modifications) for combining with other functions with same argument list.
		  */
		@inline def ofAny[C](const :C) :Curried[Ident, Any, C] = apply{ _ :Any => const }

		/** Represent the constant given as the argument (to object returned by this method) as a function of ignored argument `X`. */
		@inline def of[X] = new CurriedFunctionConstructor[X]

		/** Convert any contant to a function of `X`. */
		final class CurriedFunctionConstructor[X] {
			/** Convert the given constant value to a function `X=>C` which ignores its argument before returning the given value. */
			def apply[C](const :C) :Curried[Ident, X, C] = Curried{ _ :X => const }
		}


		/** Return an object representing partial application of `f` up until its last argument `X`. */
		@inline def last[A, B, X, Y](f :A=>B)(implicit res :ReturnType[A=>B, X, Y]) :Curried[res.Mapped, X, Y] =
			new Curried[res.Mapped, X, Y](res.cast(f))(res.curry)


//		/** Implicit conversion providing extension methods on curried function types. Same as `apply`, but doesn't pollute namespace as much. */
//		@inline implicit def implicitlyCurried[A, B](f :A=>B) :Curried[Self, A, B] = new Curried[Self, A, B](f)(new Arg0)
//
//
//		/** Extends a curried function type of more than one argument with methods for advancing through argument list. */
//		@inline implicit final def implicitlyCurried[X, Y, R](f :X=>Y=>R) :CurryOn[Self, X, Y, R] = new CurryOn[Self, X, Y, R](Curried(f))


		/** A marker class used as an argument for `CurryOn` (and `Curried`) instances to denote a new, ignored parameter. */
		sealed class __[X] private[Curried]  {

			/** Create a representation of a constant curried function `X=>A[Y=>T]`, applied to an ignored argument `W`
			  * and all arguments `curried` is applied to.
			  * @param curried a partially applied curried function
			  * @tparam A arguments to which `curried` is applied to as a type constructor accepting function result type.
			  * @tparam Y first unapplied argument in `curried`
			  * @tparam T result type returned by `curried` after applying to `Y`.
			  * @return a function `{ _ :W => curried.unapplied }` wrapped in a `Curried` instance representing
			  *         applying it up until argument `Y`.
			  */
			@inline final def :=>[A[+G], Y, T](curried :Curried[A, Y, T]) :Curried[(X =>: A)#F, Y, T] =
				new Curried[(X =>: A)#F, Y, T]((_ :X) => curried.unapplied)(curried.curry.from[X])

			/** A constant function `{ _ :X => T }` wrapped in a `Curried` instance representing its first argument `X`. */
			@inline final def :=>[T](res :T) :Curried[Ident, X, T] =
				new Curried[Ident, X, T](( _ :X) => res)(Curry[X, T])
		}

		/** A marker object used as an argument for `CurryOn` (and `Curried`) instances to denote a new, ignored parameter. */
		object __ extends __[Any] {
			/** A marker class used as an argument for `CurryOn` (and `Curried`) instances to denote a new, ignored parameter.
			  * For example, `Curried(f)()(__[X])` will create a function with same signature as `f` except it will take an additional
			  * argument after the first of type `X`.
			  */
			def apply[X] :__[X] = this.asInstanceOf[__[X]]
		}

	}





	/** Wrapper for a function `f :F &lt;: C[X=>Y]` representing result of partially applying it up to argument `X` (not including).
	  * Provides methods for transforming function `f` by mapping result type `Y`. For all arguments except the last,
	  * the result is the same as with a Curry[C, X, Y]() (i.e, next argument), but provides a convenient way
	  * of operating on the final result type of `F` (first return type in composition chain which is not a function).
	  */
	final class Result[C[+G], L, R] private[Curry] (private[this] val curried :PartiallyApplied[L, R] { type Mapped[+G] = C[G] })  { result =>
		type Mapped[+G] = C[L=>G]
		type AnyResult = C[L=>Any]
		type F = C[L=>R]

		@inline def toFunction :C[L=>R] = curried.unapplied

		@inline def map[Z](res :R=>Z) :C[L=>Z] =
			curried.mapped { _ andThen res }

//		@inline def combine[S, O](other :Mapped[S])(combine :(R, S)=>O) :C[L=>O] =
//			curried.combined(other){ (me :L=>R, her :L=>S) => l :L => combine(me(l), her(l)) }
//
//		@inline def *[S, O](other :C[L=>S])(combine :(R, S)=>O) :C[L=>O] = //this.combine(other)(combine)
//			curried.combined(other){ (me :L=>R, her :L=>S) => l :L => combine(me(l), her(l)) }
	}


	object Result {
		def apply[X, Y, L, R](f :X=>Y)(implicit res :ReturnType[X=>Y, L, R]) :Result[res.Mapped, L, R] =
			new Result[res.Mapped, L, R](new Curried[res.Mapped, L, R](res.cast(f))(res.curry))

		implicit def implicitCurryResult[X, Y, L, R](f :X=>Y)(implicit res :ReturnType[X=>Y, L, R]) :Result[res.Mapped, L, R] =
			new Result[res.Mapped, L, R](new Curried[res.Mapped, L, R](res.cast(f))(res.curry))

	}


}

