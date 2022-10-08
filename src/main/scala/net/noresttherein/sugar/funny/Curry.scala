package net.noresttherein.sugar.funny

import net.noresttherein.sugar.funny.Curry.{=>:, LastArg, NextArg, ReturnType}
import net.noresttherein.sugar.funny.Curry.Curried.__
import net.noresttherein.sugar.funny.Curry.PartiallyApplied.{CurryOn, InjectedFunction, InlinedFunction}
import net.noresttherein.sugar.typist.LowerBound

import scala.annotation.implicitNotFound




/** Represents a type (and function) constructor of a partially applied, curried function `F` of the form
  * `X0 => ... => Xn => X => Y` for any natural `n`, where X is the type of the first argument after (partial) application.
  * Provides methods for manipulating functions `F` around this argument.
  * @tparam Args[R] the result of mapping an intermediate result `(X => Y)` of function `F` to `R`; `F =:= Args[X => Y]`.
  * @tparam X the type of the first argument after partial application.
  * @tparam Y result type of function F partially applied to all its arguments up to and including X.
  */
sealed abstract class Curry[Args[+R], X, Y] extends Serializable { prev =>

	/** Full type of the curried function (including all arguments). */
	type F = Args[X => Y]

	/** Result of partial application of this function `Args[X => Y]` up to and including parameter `X`. */
	type Result = Y

	/** Replace X => Y with `G` as the result type of `F`. This is simply a type alias for type parameter `Args[G]`.*/
	type Mapped[+G] = Args[G]

	/** A function which takes argument `W` instead of `X` at this position. */
	type Composed[W] = Args[W => Y]

	/** A function type accepting an additional argument `W` before `X`, equivalent to `(Args :=> W)#F[X => Y]`. */
	type Accepting[W] = Args[W => X => Y]

	/** A function of 'W' returning this function, equivalent to `(W =>: Args)#F[X => Y]`. */
	type From[W] = W => Args[X => Y]

	/** Apply the given function to the given fixed argument `X`, removing it from the argument list.
	  * Same as `apply(f)(x)` for use when resolution of `apply` would be ambiguous.
	  * @return a function `X0 => ... => Xn => Y`, applying `f` to all its argument followed by the given argument `x`
	  *         and returning its result.
	  */
	@inline final def set(f :Args[X => Y])(x :X) :Mapped[Y] = map[Y](f)(_(x))

	/** Apply the given function to the given fixed argument `X`, removing it from the argument list.
	  * For example, the result of `Curry{a :Any => b :Byte => c :Char => s"&dollar;a&dollar;b&dollar;c" }().set(1.toByte)`
	  * (after inlining) would be a function `{a :Any => c :Char => s"&dollar;a&dollar;{1.toByte}&dollar;c" }`.
	  * @return a function `X0 => ... => Xn => Y`, applying `f` to all its argument followed by the given argument `x`
	  *         and returning its result.
	  */
	@inline final def apply(f :Args[X => Y])(x :X) :Mapped[Y] = map[Y](f)(_(x))

	/** Compose the return type `X => Y` of a partially applied `F` with another function. Substitutes the type of
	  * the next argument `X` with type `W` by mapping the argument with the given function before applying `f`.
	  * For example, given functions `f :F <: P=>O=>X => Y` and `x :N=>X`, the result would be  `{p :P => o :O => n :N => f(p)(o)(x(w)) }`.
	  * @return a function `X0 => ... => Xn => W => Y` which applies `f` to its arguments 0 through n, followed by x(_).
	  */
	@inline final def compose[W](f :Args[X => Y])(x :W=>X) :Mapped[W => Y] = 
		map[W => Y](f){ r :(X => Y) => (w :W) => r(x(w)) }

	/** Compose the return type `X => Y` of a partially applied `F` with another curried function `G`.
	  * Substitutes the argument `X` in `F` with an inlined argument list of function `x :G`.
	  * @param f the function which next argument `X` is provided by the function `x`.
	  * @param x a curried function of type `A1 => ... => An => W => X` (for any `n >= 0`) which provides the value
	  *          for the argument `X` of `f`.
	  * @return a function `X0 => ... => Xn => A1 => ... => An => W => Y` which applies `f` to the arguments `X0...Xn`
	  *         and then to the result of applying `x` to arguments `A1...An` and `W`.
	  */
	@inline final def inline[W, Z](f :Args[X => Y])(x :W => Z)
	                              (implicit result :ReturnType[Z, X]) :Mapped[W => result.Result[Y]] =
		map[W => result.Result[Y]](f) {
			r :(X => Y) => (w :W) => result.map[Y](x(w))(r)
		}

	/** Create a new function derived from `f` by inserting an ignored argument of type `W` before argument `X`. */
	@inline final def accept[W](f :Args[X => Y]) :Mapped[W => X => Y] = map[W => X => Y](f) { r :(X => Y) => _ :W => r }

	/** Map the result of partial application of this function up to argument `X` (not including).
	  * For example, if `F =:= P=>O=>X => Y`, the result is a function `{ p :P => o :O => map(f(p)(o)) }`.
	  * @param map function taking the result of applying `F` up until argument `X`.
	  */
	def map[O](f :Args[X => Y])(map :(X => Y) => O) :Mapped[O]

	/** Partially apply functions `self` and `other` sharing the same arguments up to `X` (this argument, not including)
	  * and combine the results using the passed function into another function of arguments up to this curry point.
	  * @return a function `X0 => ... => Xn => T = { x0 => .. => xn => combine(self(x0)...(xn), other(x0)...(xn))`.
	  */
	def combine[O, T](self :Args[X => Y], other :Args[O])(combine :(X => Y, O) => T) :Args[T]

	/** If the result of this partial application is a function `Y <: Z=>T`, swap the order of arguments
	  * in function `F` from `=>X=>Z=>T` to `=>Z=>X=>T`.
	  */
	@inline final def transpose[Z, T](f :Args[X => Y])(implicit ev :Y <:< (Z => T)) :Mapped[Z => X => T] =
		map[Z => X => T](f) { r :(X => Y) => z :Z => x :X => ev(r(x))(z) }

	/** Representation of partially applying another function `G` sharing the argument list `X0 => ... Xn` with `F`.
	  * @return a `Curry` instance for functions `G =:= Args[S => T]` applied up to the same point as this instance,
	  *         as defined by `Args`.
	  */
	def mapped[S, T] :Curry[Args, S, T]

	/** Return an instance representing partial application of function `A=>Args[X => Y]` up to the same argument as
	  * in this instance (i.e., before `X`). Loosely speaking, it 'adds' another argument of type `A` before all arguments
	  * of `F`, representing a function `A=>F`.
	  * @return a `Curry` instance for functions `A => X0 => ... => Xn => X => Y`.
	  */
	def from[A] :Curry[(A =>: Args)#F, X, Y]

	/** Assuming that result type `Y` of `F` is itself a function `Z=>T`, skip to the next argument `Z` following `X` in `F`.
	  * @return an instance operating on the same function `F`, but after applying argument `X`.
	  */
	@inline final def apply[Z, T]()(implicit ev :Y <:< (Z => T)) = new NextArg[Args, X, Z, T](mapped[X, Z => T])

	/** Skip to the next argument, i.e return an instance operating on the result of applying this function to argument `X`.
	  * Same as `apply()`, for when resolution of `this()` would be ambiguous.
	  * @return an instance operating on the same function `F`, but after applying argument `X`.
	  */
	@inline final def __[Z, T](implicit ev :Y <:< (Z => T)) = new NextArg[Args, X, Z, T](mapped[X, Z => T])

}






/** Operations on curried functions. */
@SerialVersionUID(ver)
object Curry {

	/** Identity type constructor mapping any type to itself. Used in conjunction with
	  * [[net.noresttherein.sugar.funny.Curry.:=>]] and [[net.noresttherein.sugar.funny.Curry.=>:]]
	  * to denote argument lists consisting of a single argument. For example,
	  * `(X =>: Ident)#F[Y] =:= (X => Y) =:= (Ident :=> X)#F[Y]`.
	  */
	type Ident[+R] = R

	/** A wrapper for a type constructor `F`, representing a curried argument list `X0 => ... => Xn => X =>`, resulting
	  * from appending a new argument `X` to the argument list `A` denoting `X0 => ... Xn =>` (for any natural `n`).
	  * For example, if `A[Y] =:= (X0 => ... => Xn => Y)`, then `(A :=> X)#F[Y]` reads 'a function ''F'' accepting
	  * arguments `A`, followed by the argument `X`, and returning `Y`.
	  * @tparam A a type constructor representing a curried argument list; `A[Y] =:= X0 => ... => Xn => Y`.
	  * @tparam X a new argument type which should be accepted by the resulting type after all arguments of `A`.
	  * @see [[net.noresttherein.sugar.funny.Curry.=>: =>:]]
	  * @see [[net.noresttherein.sugar.funny.Curry.:==> :==>]]
	  */
	type :=>[A[+R], X] = {
		/** Type constructor for curried functions accepting the arguments represented by this type `A :=> X`.
		  * `F[Y]` is a function type `X0 => ... => Xn => X => Y`. It is used by all types here to represent
		  * an argument list `X0, ..., Xn, X`.
		  */
		type F[+Y] = A[X => Y]
	}

	/** A wrapper for a type constructor `F` representing concatenation of two curried argument lists.
	  * @tparam A a type constructor representing the first curried argument list; `A[Y] =:= A0 => ... => An => Y`.
	  * @tparam B a type constructor representing the second curried argument list; `B[Y] =:= B0 => ... => Bn => Y`.
	  * @see [[net.noresttherein.sugar.funny.Curry.:=> :=>]]
	  */
	type :==>[A[+R], B[+R]] = {
		/** Type constructor for curried functions accepting the arguments represented by `A` followed by the arguments
		  * represented by `B`; `F[Y] =:= A[B[Y]] =:= A0 => ... => An => B0 => ... => Bn => Y`.
		  */
		type F[+Y] = A[B[Y]]
	}

	/** A wrapper for a type constructor `F`, representing a curried argument list `X => X0 => ... => Xn =>`,
	  * resulting from prepending a new argument `X` to the argument list `A` denoting `X0 => ... Xn =>` (for any natural `n`).
	  * Type `(X =>: A)#F[Y]` reads 'a function ''F'' accepting argument `X`, followed by the argument list `A`, and returning `Y`.
	  * @tparam X a new argument type which should be accepted by the resulting type before all arguments of `A`.
	  * @tparam A a type constructor representing a curried argument list; `A[Y] =:= X0 => ... => Xn => Y`.
	  * @see [[net.noresttherein.sugar.funny.Curry.:=> :=>]]
	  */
	type =>:[X, A[+R]] = {
		/** Type constructor for curried functions accepting the arguments represented by this type `X =>: A`.
		  * `F[Y]` is a function type `X => X0 => ... => Xn => Y`. It is used by all types here to represent
		  * an argument list `X, X0, ..., Xn`.
		  */
		type F[+Y] = X => A[Y]
	}

	/** Explicitly summon an instance of `Curry[Ident, X, Y]` representing not applied functions `X => Y`
	  * and manipulating them at argument `X`. If the result `Y` is also a curried function, returned
	  * type class will provide possibility to manipulate arguments succeeding `X`.
 	  * @tparam X the argument type of the represented function type.
	  * @tparam Y the result type of the represented function type.
	  * @return a type class representing a curried function `X => Y` before application to its argument.
	  */
	@inline final def apply[X, Y] :Curry[Ident, X, Y] = ErasedArg0.asInstanceOf[Arg0[X, Y]] //new Arg0


	/** Explicitly summon an instance of `Curry[Ident, X, Y]` representing not applied functions `X => Y`
	  * and manipulating them at argument `X`. This is the same as [[net.noresttherein.sugar.funny.Curry$.apply[X, Y] apply]],
	  * the argument is ignored and present only to enable automatic inference of type parameters `X` and `Y`, so they
	  * need not be provided explicitly.
	  * @tparam X the argument type of the represented function type
	  * @tparam Y the result type of the represented function type
	  * @param instance any function of the type we would like manipulate, used only for type inference.
	  * @return a type class representing a curried functions of the same type as the argument, before any application.
	  */
	@inline final def apply[X, Y](instance :X => Y) :Curry[Ident, X, Y] = ErasedArg0.asInstanceOf[Arg0[X, Y]]



	/** Operations on the first argument of this function. */
	@SerialVersionUID(ver)
	final class Arg0[X, Y] extends Curry[Ident, X, Y] {

		override def mapped[A, B]: Curry[Ident, A, B] = Curry[A, B] //todo: just cast this

		override def from[W]: Curry[(W =>: Ident)#F, X, Y] =
			new NextArg[Ident, W, X, Y](new Arg0[W, X => Y])

		def map[G](f :X => Y)(res: (X => Y) => G) :G = res(f)

		override def combine[U, O](f :X => Y, other: U)(combiner: (X => Y, U) => O): O =
			combiner(f, other)
	}
	private[this] final val ErasedArg0 = new Arg0[Any, Any]


	/** Operations on `n+1`-th argument `Y` of function `A[X => Y=>Z]`. */
	@SerialVersionUID(ver)
	final class NextArg[A[+G], X, Y, Z](private[funny] val prev :Curry[A, X, Y=>Z]) extends Curry[(A :=> X)#F, Y, Z] {

		override def from[W]: Curry[(W =>: (A :=> X)#F)#F, Y, Z] =
			new NextArg[(W =>: A)#F, X, Y, Z](prev.from[W])

		override def mapped[W, B]: Curry[(A :=> X)#F, W, B] = prev.mapped[X, W=>B].__ //todo: just cast this

		override def map[G](f :F)(res: (Y => Z) => G): prev.Mapped[X => G] =
			prev.map[X => G](f) { g :(X => Y => Z) => x :X => res(g(x)) }

		override def combine[U, O](self :F, other: A[X => U])(combine: (Y => Z, U) => O): A[X => O] =
			prev.combine[X => U, X => O](self, other)(
				(me :X => Y => Z, her :X => U) => { x :X => combine(me(x), her(x)) }
			)
	}





	/** Witnesses that `F <: X1 => ... => Xn => Y` for some `n`.
	  * An implicit unbound value `ReturnType[F, _]` exists for every type F, specifying `Y` as the final return type
	  * (i.e. first type in `F`'s type signature that is not a function itself. If `F` is not a function, than `F =:= Y`.
	  * Implicit bound values `ReturnType[F, Y]` exist for every pair such that `Y` is the return type of function `F`,
	  * potentially partially applied.
	  */
	@implicitNotFound("${Y} is not a return type of ${F}")
	sealed abstract class ReturnType[F, Y] extends Serializable {
		/** A function type resulting from substituting the result type `Y` of `F` with `R`. */
		type Result[+R]

		/** Performs the deep composition of functions `f` and `z`, mapping the result type `Y` of `f` with `z`. */
		def map[Z](f :F)(z :Y => Z) :Result[Z]

		/** As this instance witnesses that `F <: Result[Y]`, convert the given function so that it conforms to the return type.
		  * This may, but need not, return the same instance.
		  */
		def cast(f :F) :Result[Y]

		private[Curry] def skip[A[+G], P, L, R]
		                       (curry :Curry[A, P, Result[L => R]]) :Curry[((A :=> P)#F :==> Result)#F, L, R]
	}





	/** Witnesses that `F <: ... => X => Y` or (`F <: this.Mapped[X => Y]`), i.e.
	  * that `Y` is a type returned by (possibly partial) application of function `F` and `X` its last argument type
	  * before returning `Y`. Implicit instances are available for 'final' return types, i.e first returned type which
	  * is not a function itself. Note that in case of not fully instantiated types, 'final' is a relative term
	  * and represents the static knowledge about `F` in the context where this instance was summoned.
	  * @tparam F a function type in a curried form `X0 => ... => Xn => X => Y` for some `n &gt; 0`.
	  * @tparam X last argument of `F` before returning `Y`, i.e. `F <: X1=>..=>Xn=>X => Y` for some `n>=0`.
	  * @tparam Y returned type after applying `F` to a series of parameters (possibly all).
	  */
	sealed abstract class LastArg[F, X, Y] extends ReturnType[F, Y] {
		/** A function type resulting from substituting the result type `Y` of `F` with `R`. */
		type Result[+R] = Mapped[X => R]

		/** Result type of mapping partial application of `F` up until and not including argument `X`.
		  * It is the type resulting from substituting `X => Y` with `R` in `F`.
		  * Same as `curry.Mapped[R] forSome { val curry :Curry[Mapped, X, Y] }`.
		  */
		type Mapped[+R]


		/** A curry instance representing partial application of `F` until argument `X`, i.e. returning `X => Y`. */
		def curry :Curry[Mapped, X, Y]

	}



	sealed abstract class FallbackReturnType {
		/** Lowest priority implicit value for `ReturnType` witnessing `Y` itself as its return type.
		  * Used when no other implicit values can be found (either `Y` is not a function or `Y` is bound).
		  */
		implicit def selfType[Y] :ReturnType[Y, Y] { type Result[+R] = R } =
			new ReturnType[Y, Y] {
				type Result[+R] = R

				override def map[Z](f :Y)(z :Y => Z) :Z = z(f)

				override def cast(f :Y) : Y = f

				override def skip[A[+G], P, L, R](curry :Curry[A, P, L => R]) :Curry[(A:=>P)#F, L, R] = curry.__
			}

		def recurse[X, Y, Z](implicit result :ReturnType[Y, Z])
				:ReturnType[X => Y, Z] { type Result[+R] = X => result.Result[R] } =
			new ReturnType[X => Y, Z] {
				type Result[+R] = X => result.Result[R]

				override def map[O](f :X => Y)(z :Z => O) :Result[O] = { x :X => result.map(f(x))(z) }

				override def cast(f :X => Y) :X => result.Result[Z] = { x :X => result.cast(f(x))}

				override def skip[A[+G], P, L, R](curry :Curry[A, P, X => result.Result[L => R]]) =
					result.skip[(A :=> P)#F, X, L, R](curry.__)
			}
	}



	sealed abstract class FallbackLastArg extends FallbackReturnType {

		/** Low priority implicit `LastArg`, attesting that `Y` is the return type of `X => Y` for when no other
		  * implicit value is available for type `Y`.
		  */
		implicit def returnedValue[X, Y] :LastArg[X => Y, X, Y] { type Mapped[+G] = G } =
			new LastArg[X => Y, X, Y] {
				type Mapped[+G] = G

				override def curry = Curry[X, Y]

				override def cast(f: X => Y) :X => Y = f

				override def map[Z](f :X => Y)(z :Y => Z) :X => Z = f andThen z

				override def skip[A[+G], P, L, R](curry :Curry[A, P, X => L => R]) =
					curry.__.__
			}
	}



	@SerialVersionUID(ver)
	object ReturnType extends FallbackLastArg {

		/** Implicit proof that `R` is the return type of `X => Y=>Z` if `R` is the return type of `Y=>Z`. */
		implicit def returnedFunction[X, Y, Z, L, R](implicit res :LastArg[Y => Z, L, R])
				:LastArg[X => Y => Z, L, R] { type Mapped[+G] = X => res.Mapped[G] } =
			new LastArg[X => Y => Z, L, R] {
				type Mapped[+G] = X => res.Mapped[G]

				override def curry: Curry[Mapped, L, R] = res.curry.from[X]

				override def cast(f: X => Y => Z) :X=>res.Mapped[L => R] = { x :X => res.cast(f(x)) }

				override def map[O](f :X => Y => Z)(z :R => O) :Result[O] = { x :X => res.map(f(x))(z) }

				override def skip[A[+G], P, W, V](curry :Curry[A, P, X => res.Result[W => V]]) =
					res.skip[(A :=> P)#F, X, W, V](curry.__)
			}
	}






	/** Classes needed for performing function unification and transforming functions by mapping or combining their result values.
	  * These are mostly accepted implicit parameter types or intermediate result types which rarely will be needed to
	  * be specified explicitly by the client code.
	  */
	@SerialVersionUID(ver)
	object FunctionUnification {

		/** Extends any function type `F` with a method `<=>` for pairing it with another function `G`.
		  * Returned `F<=>G` instance has an implicit conversion via overloaded `Curry.<=>` to `reduce_<=>` 
		  * which can be used to create another function accepting the same (as defined by term unification
		  * of `F` and `G`) arguments and returning the result of combining return values of `f` and `g`
		  * for these arguments with a reducing function.
		  * @see [[net.noresttherein.sugar.funny.Curry.<=>]]
		  * @see [[net.noresttherein.sugar.funny.Curry.FunctionUnification.reduce_<=>]]
		  */
		final class build_<=>[F <: (_ => _)](private val f :F) extends AnyVal {

			/** Bind together this (right-hand value) function `G` with another function `f :F` into a `F<=>G` instance
			  * which can be used to combine both functions into a single function by unifying argument lists of `F` and `G`
			  * and combining values returned by both for the given arguments with a function operating on their return types.
			  */
			@inline def <=>[G <: (_ => _ )](g :G) : F <=> G = new <=>(f, g)
		}


		/** Denotes term unification on two curried function types `F` and `G`. Unification in this context carries
		  * two types of information: the shared argument list defined by `this.Returning` and return types of both these
		  * functions after unification. Assuming `F =:= F_1 => F_2 => .. => F_n => L`, `G =:= G_1 => G_2 => .. => G_m => R`,
		  * and that `n &lt; m` (the other case is symmetrical), the unified forms of these functions would be
		  * `F <: LB[F_1, G_1] => ... => LB[F_n, G_n] => (L)` and `G <: LB[F_1, G_1] => ... => LB[F_n, G_n] => (G_{n+1} => ... => R)`,
		  * where `LB[X, Y]` denotes maximum lower bound of types `X, Y` and the types in parenthesis' are the return types
		  * of the unified forms. Note that there is no requirement for this unification to be maximal, i.e. that either
		  * `LeftResult` or `RightResult` is not a function of a single argument, but all implicit values provided by the
		  * companion object are maximal in terms of static types of arguments.
		  */
		sealed abstract class Unification[F, G] private[FunctionUnification] extends Serializable {
			/** Result type of `F` after unification with `G` (might be another function, as it doesn't necessarily 'drop'
			  * all arguments of `F`.
			  */
			type LeftResult

			/** Result type of `G` after unification with `F` (might be another function, as it doesn't necessarily 'drop'
			  * all arguments of `G`.
			  */
			type RightResult

			/** Curried function type taking arguments shared by `F` and `G` (that is, which arguments are lower bounds
			  * of the respective arguments of `F` and `G`), and returning a value of type `O`.
			  */
			type Returning[O]

			/** Create a function sharing the unified argument list with both `F` and `G` and returning the result of
			  * applying the given function to the values returned by applying them to these arguments.
			  */
			def combine[O](f :F, g :G)(combine :(LeftResult, RightResult) => O) :Returning[O]
		}

		/** Low priority implicit [[net.noresttherein.sugar.funny.Curry.FunctionUnification.Unification Unification]]
		  * values. */
		sealed abstract class FallbackUnification {

			/** Fallback implicit result of unifying two functions `X1=>Y1` and `X2=>Y2` as functions of `X >: X1, X2`
			  * and giving return types `Y1` and `Y2` as their final return types after unification of arguments.
			  * This implicit value is used only when `Y1` and `Y2` aren't both functions and no further recursion can be performed.
			  */
			implicit def fallbackUnification[X1, X2, X, Y1, Y2](implicit argType :LowerBound[X1, X2, X])
					:Unification[X1=>Y1, X2=>Y2]
						{ type LeftResult = Y1; type RightResult = Y2; type Returning[O] = X => O } =
				new Unification[X1 => Y1, X2 => Y2] {
					type LeftResult = Y1
					type RightResult = Y2
					type Returning[O] = X => O

					override def combine[O](f: X1 => Y1, g: X2 => Y2)(combine: (Y1, Y2) => O): X => O =
						{ x :X => combine(f(argType.left(x)), g(argType.right(x))) }
				}
		}


		/** Performs term unification on two curried function types `F` and `G` by providing implicit values of
		  * [[net.noresttherein.sugar.funny.Curry.FunctionUnification.Unification Unification]], carrying information
		  * about their shared argument list (after type unification by argument type generalization) and corresponding
		  * return types.
		  * @see [[net.noresttherein.sugar.funny.Curry.FunctionUnification.Unification]]
		  */
		@SerialVersionUID(ver)
		object Unification extends FallbackUnification {
			/** Given unification of two types `Y1` and `Y2` (assumed to be functions), perform unification of any two functions
			  * `X1=>Y1` and `X2=>Y2` by listing the lower bound of `X1` and `X2` as unified argument type, and passing on
			  * result types reported for `Y1` and `Y2` (after unification).
			  */
			implicit def resultUnification[X1, X2, X, Y1, Y2]
			                              (implicit results :Unification[Y1, Y2], argType :LowerBound[X1, X2, X])
					:Unification[X1 => Y1, X2 => Y2] {
						type LeftResult = results.LeftResult
						type RightResult = results.RightResult
						type Returning[O] = X => results.Returning[O]
					} =
				new Unification[X1 => Y1, X2 => Y2] {
					type LeftResult = results.LeftResult
					type RightResult = results.RightResult
					type Returning[O] = X => results.Returning[O]

					override def combine[O](f: X1 => Y1, g: X2 => Y2)
					                       (combine: (results.LeftResult, results.RightResult) => O)
							:X => results.Returning[O] =
						{ x :X => results.combine(f(argType.left(x)), g(argType.right(x)))(combine) }
				}
		}


		/** Given two function types `F` and `G` and their unification type specified as their common argument list
		  * and types returned by applying them to these arguments, allow to create a unification function resulting
		  * from applying both to the same arguments and combining their results with an arbitrary function.
		  * An implicit conversion [[net.noresttherein.sugar.funny.Curry.<=>[F,Y,G,R]* Curry.<=>]]
		  * performs unification of function pairs of type [[net.noresttherein.sugar.funny.Curry.<=>[F, G]! F<=>G]],
		  * converting them to instances of `reduce_<=>`. This conversion can be enforced by simply invoking
		  * this class's `apply` method, as it is not declared in the `<=>` class itself.
		  * @tparam F 'left' curried function type in the form of `F1=>...=>Fn=>Y`, `F <: Returning[Y]`.
		  * @tparam Y type returned by `F` after applying to arguments common with `G`.
		  * @tparam G 'right' curried function type in the form of `G1=>..=>Gn=>Z`, `G <: Returning[Z]`.
		  * @tparam Z type returned by `G` after applying to arguments common with `F`.
		  * @tparam R curried function type with return type `O` (type parameter of `R`) and which arguments are
		  *           lower bounds of respective arguments of `F` and `G`; `F <: R[Y]` and `G <: R[Z]`.
		  * @see [[net.noresttherein.sugar.funny.Curry.<=>[F, G]!]]
		  */
		class reduce_<=>[F, Y, G, Z, R[O]](f :F, g :G)
		                                  (implicit val unification :Unification[F, G] {
			                                  type LeftResult = Y
			                                  type RightResult = Z
			                                  type Returning[O] = R[O]
		                                  })
		{
			type Returning[O] = unification.Returning[O]

			/** Create a new function `r` which returns the value given by applying passed `reduce` function
			  * to the results of applying functions `f` (left-hand argument) and `g` (right-hand argument) to its arguments.
			  * Returned function is in the curried form and takes 'n' arguments as specified by the associated unification instance
			  * (in case of implicit `Unification` values the minimum of `f` and `g` argument numbers), where each argument is a subtype
			  * of the respective arguments of `f` and `g`.
			  */
			def apply[O](reduce :(Y, Z) => O) :R[O] = unification.combine[O](f, g)(reduce)
		}

	}

	import FunctionUnification._

	/** Pairs two values (presumably curried functions) for subsequent combining them into a single function by
	  * the means of a function operating on their result values. This class has no methods; instead, there is
	  * an implicit conversion with the shared name (so importing this symbol will import the conversion at the same time)
	  * to `reduce_<=>`, which provides
	  * the [[net.noresttherein.sugar.funny.Curry.FunctionUnification.reduce_<=>.apply reduce_<=>.apply]] method
	  * performing the actual reducing of these functions. This roundabout way is forced because the alternative
	  * of declaring that method here with implicit unification parameters accepted by the conversion would first,
	  * prevent applying the result to its proper arguments (as the following '(...)' parameter group would be interpreted
	  * as the implicit parameter group of the `apply` method) and, second, result in proper type inference for the
	  * `apply`'s argument function, because all required types have been already determined by the conversion (instead
	  * of being instantiated with implicit resolution ''following'' the actual argument).
	  *
	  * Instances of this class can be created by invoking overloaded `<=>` method on the right-hand argument,
	  * which is made available again by an overloaded implicit conversion method `Curry.<=>`, so that importing the
	  * `<=>` symbol imports the associated class and all implicit conversions required for its functioning:
	  * {{{
	  *     val f :X1=>X2=>X3=>Y
	  *     val g :X1=>X2=>X3=>Z
	  *     val c :(Y, Z) => R
	  *     import Curry.<=>
	  *     val r = (f <=> g)(c)(x1)(x2)(x3)
	  * }}}
	  * @param _1 function on the left side of the operator `<=>` used to create this instance.
	  * @param _2 function on the right side of the operator `<=>` used to create this instance.
	  * @tparam F type of the function on the left side of the operator `<=>` used to create this instance.
	  * @tparam G type of the function on the right side of the operator `<=>` used to create this instance.
	  * @see [[net.noresttherein.sugar.funny.Curry.<=>[G](g:G)]]
	  * @see [[net.noresttherein.sugar.funny.Curry.<=>[F,Y,G,R](funs:F<=>G)]]
	  * @see [[net.noresttherein.sugar.funny.Curry.FunctionUnification.reduce_<=>[F,Y,G,Z,R]]]
	  */
	final class <=>[F, G](private[Curry] val _1 :F, private[Curry] val _2 :G)

	/** Enrich function type `G` with a right-associative method `<=>` which will pair it with another function so both
	  * can be combined into a single function by operating on their respective return types.
	  * @see [[net.noresttherein.sugar.funny.Curry.FunctionUnification.build_<=>]]
	  * @see [[net.noresttherein.sugar.funny.Curry.<=>[F, G]!]]
	  */
	@inline implicit def <=>[G <: (_ => _)](g :G) : build_<=>[G] = new build_<=>[G](g)

	/** Find the maximum unification of functions `F` and `G` (understood as list of arguments which can be given to both
	  * of them and return types of these applications). Returned object provides an `apply` method accepting a function
	  * operating on the return types of unified forms of `F` and `G` and returning a new function which will use it
	  * to produce the result.
	  * @see [[net.noresttherein.sugar.funny.Curry.FunctionUnification.reduce_<=>]]
	  */
	@inline implicit def <=>[F, Y, G, R](funs :F<=>G)(implicit unification :Unification[F, G])
			: reduce_<=>[F, unification.LeftResult, G, unification.RightResult, unification.Returning] =
		new reduce_<=>[F, unification.LeftResult, G, unification.RightResult, unification.Returning](
			funs._1, funs._2)(unification
		)






	/** Base trait for partially applied functions implemented as [[net.noresttherein.sugar.funny.Curry.Curried Curried]].
	  * Introduced as a workaround for lack of implicit resolution of types of higher kinds (the `Mapped` type defined here
	  * and accepted by `Curried` as a higher kind type parameter). All instances `f :PartiallyApplied[X, Y]` are also
	  * instances of `Curried[f.Mapped, X, Y]`.
	  *
	  * Wraps a function `F` of type `X0 => ... => Xn => X => Y` and represents its partial application up to, not including,
	  * argument `X`.
	  * @see [[net.noresttherein.sugar.funny.Curry.Curried]]
	  */
	sealed abstract class PartiallyApplied[X, Y] extends Serializable { self =>
		/** Type of functions taking the same arguments up to `X` exclusively, but returning type `R` instead of `X => Y`. */
		type Mapped[+R]

		/** The whole function `F` itself being wrapped by this instance. */
		val unapplied :Mapped[X => Y]

		def curry :Curry[Mapped, X, Y]

		/** Provide a fixed value for argument `X`, removing it from `F`'s argument list. This method can be invoked
		  * only when `Y` itself is a function `Z => O`, as witnessed by the implicit parameter.
		  * @return a `Curried` instance wrapping a curried function accepting the same initial parameters `X0, ..., XN`
		  *         as this one and returning the result of applying `f` to them, followed by the argument `x` passed here.
		  */
		def set[Z, O](x :X)(implicit ev :Y <:< (Z => O)) :Curried[Mapped, Z, O]

		/** Compose the returned function `X => Y` with a given function `W=>X`, essentially changing argument type from `X` to `W`.
		  * Wrapped function will map all arguments provided for `W` with the given function before invoking this function.
		  * @param  x a function mapping the replacement parameter to `X` before invoking this function.
		  * @tparam W new argument type.
		  * @return a partially applied function representation resulting from substituting argument `X` with `W` in this instance.
		  */
		def compose[W](x :W => X) :Curried[Mapped, W, Y]

		/** Map the returned function `X => Y` to another result type `G` (possibly also a function), returning a function
		  * of all arguments preceding `X` in `F` and returning `G`.
		  * @tparam G new result type for the preceding argument list, substituting `X=>T` in `F`.
		  * @param  map function mapping the intermediate result `X => Y` of this function.
		  * @return a `Curried` wrapper over function `{ x0 => ... => xn => map(f(x0)...(xn)) }`.
		  */
		def mapped[G](map :(X => Y) => G) :Mapped[G]

		/** Map the returned function to another function `Z=>O`, returning a curried
		  * function of all arguments of `F` preceding `X`, with the result type `Z=>O` instead of `X=>T`.
		  * @tparam Z new argument type substituting argument `X` in `F`.
		  * @tparam O new result type of `F` applied to all `F`'s arguments preceding `X` as well as the new argument `Z`.
		  *           Replaces type `Y` in `F`.
		  */
		def map[Z, O](map :(X => Y) => (Z => O)) :Curried[Mapped, Z, O]

		/** Substitutes the argument `X` with the arguments of a given curried function `x` returning `X`.
		  * This is similar to [[net.noresttherein.sugar.funny.Curry.Curried.compose compose]], but the substitute
		  * function can take any number of parameters. Returned object is an intermediate value from which,
		  * providing that `W=>Z <: W=>Z1=>...=>Zn=>X`, an implicit conversion exists to `Curried[A, W, Z1=>...=>Zn=>Y]`,
		  * representing the inlined function applied to the same arguments as this instance.
		  * This intermediate step is introduced in order to move the implicit parameter `ReturnType[Z, X]` from the
		  * method signature, where it prevents sugared calls to `apply` as declared by
		  * [[net.noresttherein.sugar.funny.Curry.PartiallyApplied.CurryOn CurryOn]], to the implicit conversion.
		  * @param x a function providing the value for the next parameter of this function, which arguments should be
		  *          inlined in the argument list of this function in place of `X`.
		  * @see [[net.noresttherein.sugar.funny.Curry.Curried.inlined inlined]]
		  */
		def inline[W, Z](x :W=>Z) :InlinedFunction[W, Z, X, Y] { type Mapped[+R] = self.Mapped[R] }

		/** Assuming that return type `Y` is itself a function `Z=>O`, skip to the next argument `Z` in `F`.
		  * @tparam Z argument type succeeding `X` in `F`.
		  * @tparam O return type of `F` applied to all arguments preceding X, followed by `X` and `Y`.
		  * @return a curried function representing `F` partially applied to all arguments up to and including `X`.
		  */
		def next[Z, O](implicit ev :Y <:< (Z => O)) :Curried[(Mapped :=> X)#F, Z, O]

		/** Given another function `H <: X0 => ... => Xn => Y => U` (taking the same arguments as `F` up to, not including, `X`),
		  * create a new function with arguments `X0, ..., Xn`, applying them to both this and the argument function and combining
		  * their results with the given operator.
		  * @param other another curried function sharing arguments with `F` preceding `X`.
		  * @param combine a function accepting result types of this function and `other` and providing the result for the created function
		  * @tparam S argument of `H` in the same position as `X` in `F`
		  * @tparam T return type of partially applied `H`, corresponding to type `Y` in `F`.
		  * @tparam R return type of the returned combined function taking place of type `X => Y` in `F`.
		  * @return a function `{ x0 :X0 => ... => xn => XN => combine(f(x0)...(xn), other(x0)...(xn)) }`.
		  */
		def combined[S, T, R](other :Mapped[S => T])(combine :(X => Y, S => T) => R) :Mapped[R]

		/** Create a function of type `X0 => ... => Xn => W => X => T`, accepting a new ignored argument `W` before argument `X`.
		  * Allows to unify types of several functions of similar arguments, by making `F` conform to desired type `Mapped[W=>X=>T]`.
		  * @tparam W new ignored argument preceding `X` in the returned function
		  * @return a wrapper function applying this function to all arguments it receives except for `W`.
		  */
		def accept[W] :Curried[Mapped, W, X => Y]
	}




	/** Implicit conversions enriching `PartiallyApplied` instances (and thus `Curried` instances) by adding
	  * methods advancing over the next argument. Additionally, contains helper classes which are part of the public API
	  * but unlikely to be actually referenced by the client code due to their temporary nature: intermediate results
	  * and implicit evidence.
	  * @see [[net.noresttherein.sugar.funny.Curry.PartiallyApplied.CurryOn]]
	  * @see [[net.noresttherein.sugar.funny.Curry.Curried]]
	  */
	@SerialVersionUID(ver)
	object PartiallyApplied {

		/** Implicit conversion of a `Curried[C, A, B]` to the underlying function type `C[A => B]`. */
		@inline implicit def implicitlyCurried[A, B](curried :PartiallyApplied[A, B]) :curried.Mapped[A => B] =
			curried.unapplied

		/** Implicitly extends a `Curried` partially applied function returning a function `Y=>T`, providing methods
		  * for advancing to the next argument `Y`.
		  */
		@inline implicit final def CurryOn[X, Y, T](curried :PartiallyApplied[X, Y => T])
				:CurryOn[curried.Mapped, X, Y, T] =
			new CurryOn[curried.Mapped, X, Y, T](curried)

		/** Extends a curried function type of more than one argument with methods for advancing through argument list. */
		@inline implicit final def CurryOn[X, Y, T](f :X => Y => T) :CurryOn[Ident, X, Y, T] =
			new CurryOn[Ident, X, Y, T](Curried(f))



		/** Extends a `Curried` partially applied function returning a function `Y=>T`, providing methods for advancing
		  * to the next argument `Y`. This class is an implicit extension of `PartiallyApplied` (and `Curried`) instances.
		  * It defines methods which would require implicit parameters if declared within parent `Curried`, which would
		  * prevent the caller from directly calling `apply` on their results (as the following parameter group would be
		  * treated as the implicit parameter group rather than `apply`'s parameters). By moving the implicit parameters
		  * to be the part of the implicit conversion, we were able to remove them from the methods signature, 'squeezing'
		  * them in before the invocation of the desired  methods.
		  * @see [[net.noresttherein.sugar.funny.Curry.Curried]]
		  */
		@SerialVersionUID(ver)
		final class CurryOn[A[+G], X, Y, T](private val curried :PartiallyApplied[X, Y => T] { type Mapped[+G] = A[G] })
			extends AnyVal
		{

			/** Move to the next argument, i.e. for `this` representing partial application of `F` until return type `X => Y=>T`,
			  * return an instance representing the result of further application of `F` to `X`, with return type `Y=>T`.
			  * @return a `Curried` instance for the same underlying function, but representing applying it to the next argument `X`.
			  */
			@inline def apply() :Curried[(A :=> X)#F, Y, T] = curried.next

			/** Take an additional, ignored argument of type `W` and automatically 'apply' it, so that the curry point
			  * remains before the argument `X` (and after the new argument `W`). You may specify the type of the parameter
			  * by providing type arguments either to this method, or the parameterless `Curried.__.apply[X]`, for example:
			  * `Curry(f)()(__[W])`.
			  * @param newArg a marker object used solely to specify the type of the argument and offer syntax similar
			  *               to the scala convention of using `_` as a wildcard. Valid arguments can be obtained from
			  *               [[net.noresttherein.sugar.funny.Curry.Curried.__.apply]] generic method invoked by simply
			  *               providing the type parameter: `__[W]` or the `__` object itself, which is of type `__[Any]`.
			  * @tparam W a new, additional argument type.
			  * @return a `Curried` function resulting from mapping the intermediate result `X => Y=>T` of this function to
			  *         a constant function `W=>X => Y=>T` returning the former for all arguments `W`.
			  *         Returned instance will represent the application of that function up to argument `W` and before `X`.
			  */
			@inline def apply[W](newArg : __[W]) :Curried[(A :=> W)#F, X, Y => T] = curried.accept[W].next

			/** Provide a fixed value for this argument, removing it from the argument list and return a `Curried`
			  * instance representing the parameter `Y` following `X`. Created function is the same
			  * as with `applied(x)`, but this method allows subsequent modification in chained calls. It is equivalent
			  * to `self.set(x)`.
			  * @return a `Curried` instance with the same result type `Y=>T`, applied to `X`.
			  */
			@inline def apply(x :X) :Curried[A, Y, T] = curried.set(x)

			/** Replace this argument by mapping the intended argument type `W` to `X` before applying `f` and advance
			  * to the argument `Y` following `X`. For example, given a function `f :F <: P=>O=>X=Y` and `x :N=>X`,
			  * the result is `{ p :P => o :O => n :N => f(p)(o)(x(n}) }`. This method is equivalent to `self.swap(x).>>`.
			  * @tparam W argument type replacing `X` in `F`.
			  * @return a `Curried` instance representing the next position (after argument `W` and before `Y`) in the
			  *         transformed function.
			  */
			@inline def apply[W](x :W => X) :Curried[(A :=> W)#F, Y, T] = curried.compose(x).next

			/** Replace the argument `X` with the arguments of the given function, using its returned value as the parameter
			  * for `X`. This results in the same function as [[net.noresttherein.sugar.funny.Curry.Curried.inline inline]],
			  * but in case of this method the application point is advanced past all the inlined arguments of `x`.
			  * This method returns an intermediate object for which implicit conversions exist to both `Curry` and
			  * `CurryOn` (providing type `T` is a function itself) as long as `x =:= W => A1 => ... => An => X` for some
			  * `A1,...,An`.
			  * @tparam W first argument of the replacement function.
			  * @tparam Z the composite return type which should be in the form of `A1 => ... An => X` for some `n >= 0`.
			  * @param x a function providing the value for parameter `X`
			  * @return an object representing the inlining of arguments of `x` in place of the argument `X` of this function.
			  */
			@inline def inject[W, Z](x :W=>Z) :InjectedFunction[W, Z, X, Y, T] { type Mapped[+R] = A[R] } =
				new InjectedFunction[W, Z, X, Y, T] {
					type Mapped[+R] = A[R]
					override val inline = curried.inline(x) :InlinedFunction[W, Z, X, Y => T] { type Mapped[+R] = A[R] }
				}


			/** Move to the next argument, i.e. for `this` representing partial application of `F` until return type `X => Y=>T`,
			  * return an instance representing the result of further application of `F` to `X`, with return type `Y=>T`.
			  * Same as `apply()`, but avoids ambiguity if this instance is the return value of a function taking implicit parameters.
			  * @return a `Curried` instance representing the same function, applied to its next argument `X`.
			  */
			@inline def __ :Curried[(A :=> X)#F, Y, T] = curried.next

			/** Transpose the next argument `X` with the following argument `Y`, automatically advancing over `Y` to maintain
			  * the application point before `X`. This is equivalent to `this.transpose.next`.
			  * @return a `Curried` instance for a function resulting from mapping the intermediate result `X => Y=>T` to
			  *         function `Y=>X=>T` by swapping the application order of arguments `X` and `Y`, partially applied
			  *         up to argument `Y` (and before argument `X`).
			  * @see [[net.noresttherein.sugar.funny.Curry.PartiallyApplied.CurryOn.<>>> <>>>]]
			  * @see [[net.noresttherein.sugar.funny.Curry.PartiallyApplied.CurryOn.transpose transpose]]
			  */
			@inline def <>> :Curried[(A :=> Y)#F, X, T] = curried.map[Y, X=>T](transposition).next

			/** Transpose the next argument `X` with the following argument `Y`, automatically advancing over both.
			  * This is equivalent to `this.transpose.next.next`.
			  * @param ev proof that `T` is a `Function1` and thus can be represented as a partially applied curried function.
			  * @tparam Z argument following `Y` in `F`
			  * @tparam O result of applying `F` to all arguments ending with `X, Y, Z`.
			  * @return a `Curried` instance for a function resulting from mapping the intermediate result `X => Y=>T` to
			  *         function `Y=>X=>T` by swapping the order of arguments, partially applied up to and including
			  *         argument `Y` (assuming that `T` is a function).
			  * @see [[net.noresttherein.sugar.funny.Curry.PartiallyApplied.CurryOn.<>> <>>]]
			  * @see [[net.noresttherein.sugar.funny.Curry.PartiallyApplied.CurryOn.transpose transpose]]
			  */
			@inline def <>>>[Z, O](implicit ev :T <:< (Z => O)) :Curried[((A :=> Y)#F :=> X)#F, Z, O] =
				curried.map[Y, X=>T](transposition).next.next(ev)

			/** Convert `F` into a function with arguments `X, Y` in reverse order, i.e. from `Mapped[X => Y=>T]` to `Mapped[Y=>X=>T]`. */
			@inline def transposed :A[Y => X => T] = curried.mapped[Y=>X=>T](transposition)

			/** Return a `Curried` instance representing partial application after transposition of arguments `X, Y` in `F`.
			  * Represented curried function takes arguments `X` and `Y` in reverse order, and is partially applied to
			  * the same arguments as this instance (that is, all arguments preceding `X` in `F` and `Y` in the result).
			  */
			@inline def transpose :Curried[A, Y, X => T] = curried.map[Y, X => T](transposition)

			@inline private def transposition =
				{ res :(X => Y => T) => y :Y => x :X => res(x)(y) }

		}






		/** An intermediate object representing substitution of argument `X` of a curried function `Mapped[X => Y]` with
		  * inlined arguments of function `W=>Z <: W => Z0 => .. => Zn => X`. There are implicit conversions
		  * to both [[net.noresttherein.sugar.funny.Curry.Curried Curried]] and
		  * [[net.noresttherein.sugar.funny.Curry.PartiallyApplied.CurryOn CurryOn]] which require the witness
		  * `ReturnType[Z, X]`. After the conversion, the application point of the new function remains unmoved,
		  * i.e. before parameter `W`.
		  * @see [[net.noresttherein.sugar.funny.Curry.Curried#inline Curried.inline]]
		  * @see [[net.noresttherein.sugar.funny.Curry.PartiallyApplied.InjectedFunction InjectedFunction]]
		  */
		sealed abstract class InlinedFunction[W, Z, X, Y] {
			private[Curry] type Mapped[+R]

			private[Curry] def inline(implicit result :ReturnType[Z, X]) :Curried[Mapped, W, result.Result[Y]]

			private[Curry] def inlined(implicit result :ReturnType[Z, X]) :Mapped[W => result.Result[Y]]
		}

		@SerialVersionUID(ver)
		object InlinedFunction {

			@inline implicit def inlined[W, Z, X, Y](injection :InlinedFunction[W, Z, X, Y])
			                                        (implicit result :ReturnType[Z, X])
					:injection.Mapped[W => result.Result[Y]] =
				injection.inlined(result)

			@inline implicit def Curried[W, Z, X, Y](injection :InlinedFunction[W, Z, X, Y])
			                                        (implicit result :ReturnType[Z, X])
					:Curried[injection.Mapped, W, result.Result[Y]] =
				injection.inline(result)

			@inline implicit final def CurryOn[V, W, Z, X, Y](injection :InlinedFunction[V, W=>Z, X, Y])
			                                                 (implicit result :ReturnType[Z, X])
					:CurryOn[injection.Mapped, V, W, result.Result[Y]] =
				new CurryOn[injection.Mapped, V, W, result.Result[Y]](injection.inline(ReturnType.recurse(result)))

			@inline implicit def CurryOver[W, X, Y, T](injection :InlinedFunction[W, X, X, Y=>T])
					:CurryOn[injection.Mapped, W, Y, T] =
				new CurryOn[injection.Mapped, W, Y, T](injection.inline)

		}



		/** An intermediate object representing substitution of argument `X` of a curried function `Mapped[X => Y=>R]` with
		  * inlined arguments of function `W=>Z <: W => Z0 => .. => Zn => X`. There are implicit conversions
		  * to both [[net.noresttherein.sugar.funny.Curry.Curried Curried]] and
		  * [[net.noresttherein.sugar.funny.Curry.PartiallyApplied.CurryOn CurryOn]], providing an implicit witness
		  * `ReturnType[Z, X]` exists. After the conversion, the application point is advanced over all inlined arguments
		  * before the argument `Y` of the original function.
		  * @see [[net.noresttherein.sugar.funny.Curry.PartiallyApplied.CurryOn.inject CurryOn.inject]]
		  * @see [[net.noresttherein.sugar.funny.Curry.PartiallyApplied.InlinedFunction InlinedFunction]]
		  */
		sealed abstract class InjectedFunction[W, Z, X, Y, R] { self =>
			private[Curry] type Mapped[+G]
			private[Curry] val inline :InlinedFunction[W, Z, X, Y => R] { type Mapped[+G] = self.Mapped[G] }
		}

		@SerialVersionUID(ver)
		object InjectedFunction {

			@inline implicit def Curried[W, Z, X, N, Y](injection :InjectedFunction[W, Z, X, N, Y])
			                                           (implicit result :ReturnType[Z, X])
					:Curried[((injection.Mapped :=> W)#F :==> result.Result)#F, N, Y] =
			{
				val inlined = injection.inline.inline
				val curry = result.skip[injection.Mapped, W, N, Y](inlined.curry)
				new Curried[((injection.Mapped :=> W)#F :==> result.Result)#F, N, Y](inlined.unapplied)(curry)
			}

			@inline implicit def CurryOn[W, Z, X, N, M, Y](injection :InjectedFunction[W, Z, X, N, M=>Y])
			                                              (implicit result :ReturnType[Z, X])
					:CurryOn[((injection.Mapped :=> W)#F :==> result.Result)#F, N, M, Y] =
				new CurryOn[((injection.Mapped :=> W)#F :==> result.Result)#F, N, M, Y](Curried(injection)(result))
		}
	}



	/** A wrapper over a curried function `f :F` (with an arbitrary number of single-element argument lists), `F <: A[X=>T]`,
	  * representing partial application of `f` up to (not including) `X` and providing methods for modifying this function
	  * from this argument on. Naming convention to which (most of) methods of this class conform is that an imperative
	  * verb form is used for methods returning another `Curried` instance (and thus allowing for modification of subsequent
	  * arguments), while passive voice is reserved for methods returning the underlying function after requested
	  * transformation, signalling end of transformations; in particular, `unapplied` returns the underlying function itself.
	  * So, for example, `this.map(f).unapplied` is equivalent to `this.mapped(f)`, both returning a function resulting
	  * from mapping the `X => Y` result of this function, but the former going through an intermediate step of returning
	  * a new `Curry` instance for that function.
	  *
	  * In addition, there is an implicit conversion from instances of `Curried` which don't represent the last argument
	  * of the function (i.e. `Y <: _ => _` ) to `CurryOn` instances which provide much nicer `apply` syntax
	  * for advancing to the next argument after each operation allowing for more succinct - and hopefully readable - code.
	  * Thus for some methods declared here transforming `f`, there are implicitly added by `CurryOn` corresponding `apply`
	  * methods performing the same transformation, but additionally advancing to the next argument. This offers
	  * syntax similar to normal function application, for example `Curried(f)(const)()(compose)()(_[W])` would create
	  * a function which first applies `f`, in order, to the given value, first parameter, a parameter mapped from the
	  * next parameter, third parameter, and then takes an ignored parameter of type `W`. A concrete example:
	  * {{{
	  *     val f = Curried{b:Byte => s:Short => i:Int => l:Long => s"\$b\$s\$i\$l"}
	  *     val res = f()((_:String).toShort)(42).unapplied :Byte=>String=>Long=>String.
	  * }}}
	  * @param unapplied underlying function to which all functions created by this instance will delegate
	  * @param curry type constructor for partial application
	  * @tparam A Represents all arguments of `F` before `X`; `A[G] =:= A1=>A2=>...=>An=>G`, providing `F =:= A1=>A2=>...=>AN=>X => Y`.
	  * @tparam X type of the first argument after partial application of F.
	  * @tparam Y result type of applying `f` with all arguments up to `X` (inclusive), possibly another function.
	  */
	@SerialVersionUID(ver)
	final class Curried[A[+R], X, Y](val unapplied :A[X => Y])(val curry :Curry[A, X, Y]) extends PartiallyApplied[X, Y] {
		/**Full type of this function. */
		type F = A[X => Y]
		/** Result type of partially applying `Fun` up to and including `X`. */
		type Applied = Y
		/** Function `A1 => ... => An =>R`, where `A1..An` are all arguments of F preceding `X`. */
		type Mapped[+R] = A[R]
		/** Type of `F` after replacing `X` with `W`. */
		type Composed[W] = Mapped[W => Y]

		/** Represent this instance (and the underlying function) as a function taking a sub-type `W` of argument `X` instead of `X`. */
		@inline def as[W<:X] :Curried[A, W, Y] = new Curried[A, W, Y](unapplied)(curry.mapped[W, Y]) //todo: just cast this

		/** Return the underlying function as its supertype accepting narrower argument type at this position. */
		@inline def arg[W <: X] :A[W => Y] = unapplied


		/** Provide a fixed value for this argument, removing it from the argument list.
		  * @return a function with same arguments as the underlying function but with `X` omitted, which invokes this
		  *         function passing `x` for this argument and its own arguments for the others.
		  * @see [[net.noresttherein.sugar.funny.Curry.PartiallyApplied.CurryOn.apply(x:X)*]]
		  * @see [[net.noresttherein.sugar.funny.Curry.set]]
		  * @see [[net.noresttherein.sugar.funny.Curry.Curried.set]]
		  */
		@inline def applied(x :X) :Mapped[Y] = curry.set(unapplied)(x)

		/** Provide a fixed value for this argument, removing it from the argument list, and return a `Curried`
		  * instance representing the first parameter after `X`, providing one exists. Created function is the same
		  * as with `apply(x)`, but allows subsequent modification in chained calls. If this instance represents
		  * the last argument, you may use `applied(x)` instead which returns the underlying function instead of
		  * a `Curried` instance. There is also an `apply` variant of this method available by implicit conversion
		  * to [[net.noresttherein.sugar.funny.Curry.PartiallyApplied.CurryOn CurryOn]]: `this(x)`.
		  * @return a Curried instance representing same application point (before argument `Y`) in the transformed function.
		  * @see [[net.noresttherein.sugar.funny.Curry.PartiallyApplied.CurryOn.apply(x:X)*]]
		  */
		@inline def set[Z, O](x :X)(implicit ev :Y <:< (Z => O)) :Curried[A, Z, O] =
			new Curried[A, Z, O](curry.map[Z => O](unapplied){ res :(X => Y) => ev(res(x))})(curry.mapped[Z, O])


		/** Composes the result of partial application of `F` with a function mapping new argument type `W` to the corresponding
		  * argument `X` in `F`. Has the effect of substituting this argument with a new argument type and mapping it on each
		  * application before invoking this function.
		  * For example, given a function `f :F <: M=>I=>X=>T` and `x :N=>X`, the result is a function of type `M=>I=>N=>T`:
		  * `{m :M => i :I => n :N => f(m)(i)(x(n)) }`.
		  * @return a function resulting from substituting the argument `X` in the underlying function to `W` and mapping
		  *         its value using the passed function.
		  * @see [[net.noresttherein.sugar.funny.Curry.compose]]
		  */
		@inline def composed[W](x :W => X) :Mapped[W => Y] = curry.compose(unapplied)(x)

		/** Compose the function resulting from partially applying `F` up to this point with another function specifying
		  * a new argument type and return a `Curried` instance representing its partial application to the same arguments
		  * (up until `W`). Has the effect of substituting argument `X` by mapping intended argument type `W` to `X` before
		  * applying `f`. For example, given a function `f :F <: M=>I=>X=>T` and `x :N=>X`, the result is a function
		  * of type `M=>I=>N=>T`: `{m :M => i :I => n :N => f(m)(i)(x(n)) }`.
		  * There is also an `apply` variant of this method advancing to the next argument, available by implicit conversion
		  * to [[net.noresttherein.sugar.funny.Curry.PartiallyApplied.CurryOn CurryOn]] : `this((w :W) => w.toX)`.
		  * @return a `Curried` instance representing the same position (before argument `W`) in the transformed function.
		  * @see [[net.noresttherein.sugar.funny.Curry.Curried.composed composed]]
		  * @see [[net.noresttherein.sugar.funny.Curry.PartiallyApplied.CurryOn.apply(x:W=>X)* apply(x :W=>X)]]
		  */
		@inline def compose[W](x :W => X) :Curried[A, W, Y] =
			new Curried[A, W, Y](curry.compose(unapplied)(x))(curry.mapped[W, Y])


		/** Substitutes the argument `X` with the arguments of a given curried function `x` returning `X`.
		  * This is similar to [[net.noresttherein.sugar.funny.Curry.Curried.composed composed]], but the substitute
		  * function can take any number of arguments.
		  * @param x a function providing the value for the next parameter of this function, which arguments should be
		  *          inlined in the argument list of this function in place of `X`.
		  * @return a function taking the same arguments preceding `X` as this function, followed by all arguments of `x`
		  *         preceding the return type `X`, and returns the result of applying this function up to `X` and then
		  *         to the result of applying `x` to the following arguments.
		  * @see [[net.noresttherein.sugar.funny.Curry.Curried.inline inline]]
		  */
		def inlined[W, Z](x :W => Z)(implicit result :ReturnType[Z, X]) :A[W => result.Result[Y]] =
			curry.inline(unapplied)(x)(result)

		/** Substitutes the argument `X` with the arguments of a given curried function `x` returning `X`.
		  * This is similar to [[net.noresttherein.sugar.funny.Curry.Curried.compose compose]], but the substitute
		  * function can take any number of parameters. Returned object is an intermediate value from which,
		  * providing that `W=>Z <: W=>Z1=>...=>Zn=>X`, an implicit conversion exists to `Curried[A, W, Z1=>...=>Zn=>Y]`.
		  * This intermediate step is introduced in order to move the implicit parameter `ReturnType[Z, X]` from the
		  * method signature, where it prevents sugared calls to `apply` as declared by
		  * [[net.noresttherein.sugar.funny.Curry.PartiallyApplied.CurryOn CurryOn]], to the implicit conversion.
		  * @param x a function providing the value for the next parameter of this function, which arguments should be
		  *          inlined in the argument list of this function in place of `X`.
		  * @see [[net.noresttherein.sugar.funny.Curry.Curried.inlined inlined]]
		  */
		def inline[W, Z](x :W => Z) :InlinedFunction[W, Z, X, Y] { type Mapped[+R] = A[R] } =
			new InlinedFunction[W, Z, X, Y] {
				type Mapped[+R] = A[R]

				def inline(implicit result :ReturnType[Z, X]) :Curried[A, W, result.Result[Y]] = {
					val inlined = curry.inline(unapplied)(x)(result)
					new Curried[Mapped, W, result.Result[Y]](inlined)(curry.mapped[W, result.Result[Y]])
				}

				override def inlined(implicit result :ReturnType[Z, X]) :A[W => result.Result[Y]] =
					curry.inline(unapplied)(x)(result)
			}


		/** Map the result of partial application of this function up to argument `X` (not including).
		  * For example, if `F =:= P=>O=>X => Y`, the result is the function `{p :P => o :O => map(f(p)(o)) }`.
		  * @param map function taking the result of applying F up until argument `X`.
		  * @see [[net.noresttherein.sugar.funny.Curry.Curried.returning]]
		  * @see [[net.noresttherein.sugar.funny.Curry.Curried.map]]
		  * @see [[net.noresttherein.sugar.funny.Curry.combine]]
		  */
		@inline def mapped[G](map :(X => Y) => G) :Mapped[G] = curry.map(unapplied)(map)

		/** Map the result of this partial application to another function `Z=>O`, returning a `Curried` instance representing
		  * the new function.
		  * @see [[net.noresttherein.sugar.funny.Curry.map]]
		  */
		@inline def map[Z, O](map :(X => Y) => (Z => O)) :Curried[A, Z, O] =
			new Curried[A, Z, O](curry.map(unapplied)(map))(curry.mapped[Z, O])


		/** Map the result of partial application of this function up to argument X. Differs from
		  * [[net.noresttherein.sugar.funny.Curry.Curried.mapped mapped]] in that
		  * it maps the value returned after applying to `X`, while `mapped` maps the whole function `X => Y`.
		  * @see [[net.noresttherein.sugar.funny.Curry.Curried.andThen]]
		  */
		@inline def returning[O](map :Y => O) :A[X => O] =
			curry.map(unapplied){ _ andThen map }

		/** Map the result of partial application of this function up to argument X to another function. Differs from `map`
		  * in that it maps the value returned after applying to `X`, while `map` maps whole function `X => Y`.
		  * @return a `Curried` instance representing the function resulting from applying `F` to all arguments up to `X`
		  *         and mapping the result `Y` to a function `Z=>O`, before applying to `Z`.
		  * @see [[net.noresttherein.sugar.funny.Curry.Curried.returning]]
		  */
		@inline def andThen[Z, O](map :Y => Z => O) :Curried[(A :=> X)#F, Z, O] =
			new Curried[(A :=> X)#F, Z, O](curry.map(unapplied)(_ andThen map))(curry.mapped[X, Z => O]())


		/** Create a function taking an additional, ignored argument `W` before the argument `X`.
		  * @see [[net.noresttherein.sugar.funny.Curry.PartiallyApplied.CurryOn.apply(newArg:__[W])*]]
		  * @see [[net.noresttherein.sugar.funny.Curry.accept]]
		  */
		@inline def accepting[W] :A[W => X => Y] = mapped[W => X => Y]{ f => _ => f }

		/** Insert a new, ignored argument `W` before `X`, remaining in the same application point (before `W`).
		  * @see [[net.noresttherein.sugar.funny.Curry.PartiallyApplied.CurryOn.apply(newArg:__[W])*]]
		  * @see [[net.noresttherein.sugar.funny.Curry.accept]]
		  */
		@inline def accept[W] :Curried[A, W, X => Y] = new Curried[A, W, X => Y](accepting[W])(curry.mapped[W, X => Y])


		/** Create a function `W=>F` ignoring its first argument and returning this function `F` (unapplied).
		  * This essentially prepends a new ignored argument of type `W` before all arguments specified by
		  * [[net.noresttherein.sugar.funny.Curry.Curried.Mapped Mapped]].
		  * @return a constant function returning [[net.noresttherein.sugar.funny.Curry.Curried.unapplied unapplied]]
		  *         for all arguments.
		  */
		@inline def returned[W] :W => A[X => Y] = (_ :W) => unapplied

		/** Create an instance representing partial application of function `W=>F` up to `X` (excluding).
		  * This essentially prepends a new ignored argument before all arguments specified by
		  * [[net.noresttherein.sugar.funny.Curry.Curried.Mapped Mapped]].
		  * @return a `Curried` instance representing a constant function returning this function for all arguments,
		  *         applied up to the same argument `X`.
		  * @see [[net.noresttherein.sugar.funny.Curry.from]]
		  */
		@inline def from[W] :Curried[(W =>: A)#F, X, Y] =
			new Curried[(W =>: A)#F, X, Y]((_ :W) => unapplied)(curry.from[W])

		/** Create an instance representing partial application of function `W=>F` up to `X` (excluding).
		  * This essentially prepends a new ignored argument before all arguments specified by
		  * [[net.noresttherein.sugar.funny.Curry.Curried.Mapped]].
		  * The [[net.noresttherein.sugar.funny.Curry.Curried.__.:=>[A,Y,T]* :=>]] is a left-associative version of
		  * this method with identical semantics, but can have worse type inference from the compiler.
		  * @param w a marker object used solely to specify the type of the desired argument. Valid values can be obtained
		  *          from [[net.noresttherein.sugar.funny.Curry.Curried.__.apply __.apply]] generic method invoked by
		  *          simply providing the type parameter: `__[W]` or the `__` object itself, which is of type `__[Any]`.
		  * @return a `Curried` instance representing a constant function returning this function for all arguments,
		  *         applied up to the same argument `X`.
		  * @see [[net.noresttherein.sugar.funny.Curry.Curried.__.:=>[A,Y,T]*]]
		  */
		@inline def =>:[W](w :__[W]) :Curried[(W =>: A)#F, X, Y] =
			new Curried[(W =>: A)#F, X, Y]((_ :W) => unapplied)(curry.from[W])


		/** Given another function sharing with `F` arguments preceding `X` and a function combining results of partial
		  * application of both functions, return a single function taking shared arguments and invoking the combining
		  * function on results of applying `this.result` and `other` to received arguments.
		  * @see [[net.noresttherein.sugar.funny.Curry.combine]]
		  */
		@inline def combined[S, T, O](other :A[S=>T])(combine :(X => Y, S => T) => O) :A[O] =
			curry.combine(unapplied, other)(combine)

		/** Given another `Curried` instance representing a function sharing the arguments preceding `X` with this function,
		  * combine the results of partial application to shared arguments using the function passed as the second argument.
		  * Usage : `(Curried(self)()() * Curried(other)()()){ case (me, her) => ??? }`.
		  */
		@inline def *[S, T, O](other :Curried[A, S, T])(combine :(X => Y, S => T) => O) :A[O] =
			curry.combine(unapplied, other.unapplied)(combine)


		/** Skip to the next argument, i.e return an instance operating on the result of applying this function to argument `X`.
		  * Represents using an identity function on argument `x` before taking any subsequent arguments. An implicit
		  * conversion exists providing the same functionality with sweeter syntax (and without the implicit argument list):
		  * `this()` and/or `this.__`, for when `apply()` would be ambiguous. This duplicity stems from the fact that
		  * implicit argument list required for implementation as a method prevents from shortened `apply` calls on
		  * the result of `>>`, but on the other hand IntelliJ IDE doesn't currently recognise implicit conversions
		  * using dependent types (and scala doesn't allow for implicit conversions to types parameterized with higher kinds).
		  * @see [[net.noresttherein.sugar.funny.Curry.PartiallyApplied.CurryOn.apply()]]
		  * @see [[net.noresttherein.sugar.funny.Curry.PartiallyApplied.CurryOn.__]]
		  */
		@inline def next[Z, R](implicit ev :Y <:< (Z => R)) :Curried[(A :=> X)#F, Z, R] =
			new Curried[(A :=> X)#F, Z, R](curry.map[X => Z => R](unapplied)((rest :X => Y) => rest andThen ev))(curry())


		/** Returns a wrapper over the underlying function which operates on its last argument and final result type.
		  * @tparam L last argument of `F`, `F =:= A[X=>...=>L=>R]`.
		  * @tparam R the type returned by `F` after application up to and including argument `L`.
		  */
		@inline def result[L, R](implicit result :LastArg[A[X => Y], L, R]) :Result[result.Mapped, L, R] =
			new Result[result.Mapped, L, R](new Curried[result.Mapped, L, R](result.cast(unapplied))(result.curry))
	}



	/** Wraps a function in a curried form (with single-argument lists) into a `Curried` class representing partial application
	  * of that function and providing convenient methods for mapping the underlying function without applying it to actual arguments.
	  */
	@SerialVersionUID(ver)
	object Curried {

		/** Extension methods for modifying curried functions at their first argument (and a source for advancing to
		  * subsequent arguments.
		  */
		@inline def apply[A, B](f :A => B) :Curried[Ident, A, B] = new Curried[Ident, A, B](f)(new Arg0)

		/** Lifts a constant of type `C` to a function `Any=&gt;C` ignoring its argument; primarily used to make the value
		  * conform to a common curried function type (by optional further modifications) for combining with other functions
		  * with the same argument list.
		  */
		@inline def ofAny[C](const :C) :Curried[Ident, Any, C] = apply{ _ :Any => const }

		/** Represent the constant given as the argument (to object returned by this method) as a function of ignored
		  * argument `X`. In other words, `Curried.of[X](y)` is equivalent to `Curried { _ :X => y }`.
		  */
		@inline def of[X] = new CurriedFunctionConstructor[X]

		/** Convert any constant to a function of `X`. */
		@SerialVersionUID(ver)
		final class CurriedFunctionConstructor[X] extends Serializable {
			/** Convert the given constant value to a function `X=>C` which ignores its argument before returning the given value. */
			def apply[C](const :C) :Curried[Ident, X, C] = Curried { _ :X => const }
		}


		/** Return an object representing partial application of `f` up until its last argument `X`. */
		@inline def last[A, B, X, Y](f :A => B)(implicit res :LastArg[A => B, X, Y]) :Curried[res.Mapped, X, Y] =
			new Curried[res.Mapped, X, Y](res.cast(f))(res.curry)




		/** A marker class used as an argument for `CurryOn` and `Curried` instances to denote a new, ignored parameter.
		  * Additionally, it can serve as a factory for functions of `X` ignoring their first argument.
		  */
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
		@SerialVersionUID(ver)
		object __ extends __[Any] {
			/** A marker class used as an argument for `CurryOn` (and `Curried`) instances to denote a new, ignored parameter.
			  * For example, `Curried(f)()(__[X])` will create a function with same signature as `f` except it will take an additional
			  * argument after the first of type `X`.
			  */
			def apply[X] :__[X] = this.asInstanceOf[__[X]]
		}

	}





	/** Wrapper for a function `f :F <: A[X => Y]` representing result of partially applying it up to argument `X` (not including).
	  * Provides methods for transforming function `f` by mapping result type `Y`. For all arguments except the last,
	  * the result is the same as with a `Curry[A, X, Y]()` (i.e, next argument), but provides a convenient way
	  * of operating on the final result type of `F` (first return type in composition chain which is not a function).
	  * @tparam A type constructor for curried functions sharing all arguments to the left of application point of f.
	  * @tparam X type of the last parameter of `F` before returning `Y` (not necessarily actually its last parameter).
	  * @tparam Y result type (possibly intermediate) of `F` being mapped by this instances `map` method.
	  */
	@SerialVersionUID(ver)
	final class Result[A[+G], X, Y] private[Curry](private[this] val curried :Curried[A, X, Y]) extends Serializable {
		result =>
		/** A function with the same parameters as `F`, including `X`, returning `G` instead of `Y`. */
		type Mapped[+G] = A[X => G]
		/** A function sharing the argument list (before the result `Y`) with `F` and returning `Any`. */
		type AnyResult = A[X => Any]
		/** The mapped function type. */
		type F = A[X => Y]

		/** The underlying function being mapped. */
		@inline def toFunction :A[X => Y] = curried.unapplied

		/** Create a function mapping the result of the wrapped function with the argument function, with the same arguments as `F`. */
		@inline def map[Z](res :Y => Z) :A[X => Z] =
			curried.mapped { _ andThen res }

	}


	@SerialVersionUID(ver)
	object Result {
		def apply[X, Y, L, R](f :X => Y)(implicit res :LastArg[X => Y, L, R]) :Result[res.Mapped, L, R] =
			new Result[res.Mapped, L, R](new Curried[res.Mapped, L, R](res.cast(f))(res.curry))

		implicit def implicitCurryResult[X, Y, L, R](f :X => Y)(implicit res :LastArg[X => Y, L, R])
				:Result[res.Mapped, L, R] =
			new Result[res.Mapped, L, R](new Curried[res.Mapped, L, R](res.cast(f))(res.curry))
	}


}

