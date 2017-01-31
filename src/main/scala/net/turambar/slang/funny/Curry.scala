package net.turambar.slang.funny

import Curry.PartiallyApplied.CurryOn
import net.turambar.slang.funny.Curry.Curried.__
import net.turambar.slang.funny.Curry.{Compose, NextArg}
import net.turambar.slang.typist.LowerBound


/** Represents a type (and function) constructor of partially applied, curried function `F` which is of the form `... X => A`,
  * where X is the type of the first argument after (partial) application.
  * Provides methods for manipulating functions `F` around this argument.
  * @tparam C[G] result of mapping partial result `(X=>A)` of function `F` to `G`; `F =:= C[X=>A]`
  * @tparam X type of the first argument after partial application
  * @tparam A result type of function F partially applied up to and including argument X
  */
sealed abstract class Curry[C[+G], X, A] { prev =>
	/** Full type of the curried function (including all arguments). */
	type F = C[X=>A]
	/** Result of partial application of this function C[X=>A] up to and including parameter `X`. */
	type Applied = A
	/** Replace X=>A with G as the result type of C[X=>A]. */
	type Mapped[+G] = C[G]
	/** A function which takes argument `W` instead of `X` at this position. */
	type Swapped[W] = Mapped[W=>A]

	/** A function of 'X' returning this function. */
	type Composed[W] = W => C[X=>A]

	/** Provide a fixed value for this argument, removing it from the argument list. Same as `apply(f)(x)` for use
	  * when resolution of apply would be ambigous.
	  */
	def set(f :C[X=>A])(x :X) :Mapped[A] = map[A](f)(_(x))

	/** Provide a fixed value for this argument, removing it from the argument list.
	  * C[X=>A]or example, the result of `Curry{a :Any => b :Byte => c :Char => s"&dollar;a&dollar;b&dollar;c" }().set(1.toByte)`
	  * (after inlining) would be a function `{a :Any => c :Char => s"&dollar;a&dollar;{1.toByte}&dollar;c" }`.
	  */
	def apply(f :C[X=>A])(x :X) :Mapped[A] = map[A](f)(_(x))

	/** Substitute the type of this argument `X` with type `W` by mapping the argument with the given function before applying `f`.
	  * C[X=>A]or example, given a function `f :C[X=>A] &lt;: D=>O=>X=>A` and `x :W=>X`, the result is `{d :D => o :O => w :W => f(d)(o)(x(w)) }`.
	  */
	def swap[W](f :C[X=>A])(x :W=>X) :Mapped[W=>A] = map[W=>A](f){ r :(X=>A) => (w :W) => r(x(w)) }

	/** Map the result of partial application of this function up to argument `X` (not including).
	  * C[X=>A]or example, if `C[X=>A] =:= K=>L=>X=>A`, the result is a function `{k :K => l :L => map(f(k)(l)) }`.
	  * @param map function taking the result of applying C[X=>A] up until argument `X`.
	  * @return resul
	  */
	def map[G](f :C[X=>A])(map :((X => A) => G)) :Mapped[G]

	/** Partially apply functions `self` and `other` sharing the same arguments up to `X` (this argument, not including)
	  * and combine the results using the passed function into another function of arguments up to this curry point.
	  */
	def combine[Y, B, G](self :C[X=>A], other :C[Y=>B])(combine :(X=>A, Y=>B) => G) :C[G]

	/** If the result of this partial application is a function `A &lt;: Y=>R`, swap the order of arguments
	  * in function `C[X=>A]` from `=>X=>Y=>` to `=>Y=>X=>`.
	  */
	def transpose[Y, R](f :C[X=>A])(implicit ev :A<:<(Y=>R)) :Mapped[Y=>X=>R] = map[Y=>X=>R](f) {
		r :(X=>A) => y :Y => x :X => ev(r(x))(y)
	}

	/** Return representation of partially applying function `C[Y=>B]` to all arguments already applied in this type. */
	def mapped[Y, B] :Curry[C, Y, B]

	/** Return an instance representing partial application of function `W=>C[X=>A]` up to the same argument as in this instance (i.e., before `X`). */
	def composed[W] :Curry[(C Compose W)#T, X, A]

	/** Skip to the next argument, i.e return an instance operating on the result of applying this function to argument `X`. */
	@inline final def apply[Y, R]()(implicit ev :A<:<(Y=>R)) = new NextArg[C, X, Y, R](mapped[X, Y=>R])

	/** Skip to the next argument, i.e return an instance operating on the result of applying this function to argument `X`.
	  * Same as apply, for when resolution of `this()` would be ambigous.
	  */
	@inline final def __[Y, R](implicit ev :A<:<(Y=>R)) = new NextArg[C, X, Y, R](mapped[X, Y=>R])
}


/** Operations on curried functions. */
object Curry {
	type Self[+G] = G
	type AppliedTo[C[+G], X] = { type T[+G] = C[X=>G] }
	type Compose[C[+G], X] = { type T[+G] = X => C[G] }

	@inline final def apply[A, B] :Curry[Self, A, B] = new Arg0

	@inline final def apply[A, B](instance :A=>B) :Curry[Self, A, B] = new Arg0

	//		def apply[A, B](f :A=>B) :Curried[Self, A, B] = new Curried[Self, A, B](f)(new Arg0)


	/** Operations on the first argument of this function. */
	final class Arg0[X, Y] extends Curry[Self, X, Y] {

		override def mapped[A, B]: Curry[Self, A, B] = Curry[A, B]

		override def composed[W]: Curry[(Self Compose W)#T, X, Y] =
			new NextArg[Self, W, X, Y](new Arg0[W, X=>Y])

		def map[G](f :X=>Y)(res: (X=>Y) => G) :G = res(f)

		override def combine[A, B, G](f :X=>Y, other: A=>B)(combine: (X => Y, A => B) => G): G =
			combine(f, other)
	}

	/** Operations on `n+1`-th argument `Y` of function `C[X=>Y=>A]`. */
	final class NextArg[C[+G], X, Y, A](private[funny] val prev :Curry[C, X, Y=>A]) extends Curry[(C AppliedTo X)#T, Y, A] {

		/** Return an instance representing the same argument, but of function `W=>F`. `type Mapped[G] = W=>C[X=>G]`. */
		override def composed[W]: Curry[Compose[AppliedTo[C, X]#T, W]#T, Y, A] =
			new NextArg[(C Compose W )#T, X, Y, A](prev.composed[W])

		override def mapped[W, B]: Curry[(C AppliedTo X)#T, W, B] = prev.mapped[X, W=>B].__

		override def map[G](f :F)(res: (Y => A) => G): prev.Mapped[X => G] =
			prev.map[X=>G](f) { g :(X=>Y=>A) => x :X => res(g(x)) }

		override def combine[M, N, G](self :F, other: C[X=>M=>N])(combine: (Y => A, M=>N) => G): C[X=>G] =
			prev.combine[X, M=>N, X=>G](self, other)((me :X=>Y=>A, her :X=>M=>N) => { x :X => combine(me(x), her(x))} )
	}







	/** Witnessess that `X=>Y &lt;: ... => A=>R` or (`X=>Y &lt;: this.Mapped[A=>R]`), i.e.
	  * that `R` is a type returned by (possibly partial) application of `X=>Y`. Implicit
	  * instances are available for 'final' return types, i.e first returned type which is not a function itself.
	  * @tparam F &lt;: X=>Y function in a curried form which returns `R` after partial application, as witnessed by this instance.
	  * @tparam A last argument of `F` before returning `R`, i.e. `F &lt;: X1=>..=>Xn=>A=>R` for some `n>=0`.
	  * @tparam R returned type after applying `F` to a series of parameters (possibly all).
	  */
	sealed abstract class ReturnType[F, A, R] {
		/** Result type of mapping partial application of `X=>Y` up until and not including argument `A`.
		  * Same as `curry.Mapped[G] forSome { val curry :Curry[Mapped, A, R] }`.
		  */
		type Mapped[+G]
		type UpperBound >: F //Mapped[Nothing=>Any]
		type LowerBound <: F with Mapped[A=>Nothing] with UpperBound
		//			type Result[G<:F]
		//			type Source[G] = X => G

		/** A curry instance representing partial application of `X=>Y` until argument `A`, i.e. returning `A=>R`. */
		def curry :Curry[Mapped, A, R]

		/** As this instance witnesses that `X=>Y &lt;: Mapped[A=>R]`, convert the given function so that it conforms to the return type.
		  * This may, but need not return the same instance.
		  */
		def cast(f :F) :Mapped[A=>R]
	}

	class FallbackReturnType private[Curry] {

		/** Low priority implicit return type attesting that `Y` is the return type of `X=>Y` for when no other implicit value
		  * is available for type `Y`.
		  */
		implicit def returnedValue[X, Y] :ReturnType[X=>Y, X, Y] { type Mapped[+G] = G; type UpperBound=Nothing=>Any; type LowerBound=X=>Nothing } =
			new ReturnType[X=>Y, X, Y] {
				type Mapped[+G] = G
				type UpperBound = Nothing => Any
				type LowerBound = X=>Nothing

				override def curry = Curry[X, Y]

				override def cast(f: X => Y) :X=>Y = f

			}
	}

	object ReturnType extends FallbackReturnType {
		/** Implicit evidence that `R` is the return type of `X=>Y=>Z` if `R` is the return type of `Y=>Z`. */
		implicit def returnedFunction[X, Y, Z, L, R](implicit res :ReturnType[Y=>Z, L, R])
				:ReturnType[X=>Y=>Z, L, R] { type Mapped[+G] = X=>res.Mapped[G]; type UpperBound = Nothing=>res.UpperBound; type LowerBound = X=>res.LowerBound } =
			new ReturnType[X=>Y=>Z, L, R] {
				type Mapped[+G] = X => res.Mapped[G]

				type UpperBound = Nothing => res.UpperBound
				type LowerBound = X => res.LowerBound

				override def curry: Curry[Mapped, L, R] = res.curry.composed[X]

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
		final class FunctionFactor[G <: (_ => _)](val g :G) extends AnyVal {

			/** Bind together this (right-hand value) function `G` with another function `f :F` into a `F:*:G` instance
			  * which can be used to combine both functions into a single function by unifying argument lists of `F` and `G`
			  * and combining values returned by both for the given arguments with a function operating on their return types.
			  */
			@inline def :*:[F<: (_ => _ )](f :F) : F:*:G = new :*:(f, g)
		}


		/** Performs term unification on two curried function types `F` and `G`. Unification in this context carries
		  * two types of information : shared argument list defined by `this.Returning` and return types of both these
		  * functions after unification. Assuming `F =:= F_1 => F_2 => .. => F_n => L`, `G =:= G_1 => G_2 => .. => G_m => R`,
		  * and that `n &lt; m`, unified forms of these functions would be `F &lt;: LB[F_1, G_1] => ... => LB[F_n, G_n] => (L)` and
		  * `G &lt;: LB[F_1, G_1] => ... => LB[F_n, G_n] => (G_{n+1} => ... => R)`, where `LB[X, Y]` denotes maximum lower bound of types `X, Y`
		  * and the types in parenthesis' are the return types of unified forms.
		  * Note that there is no requirement for this unification to be maximal, i.e. that either `LeftResult` or `RightResult` is not
		  * a function of a single argument, but all implicit values provided by the companion object
		  * are maximal in terms of static types of arguments.
		  */
		sealed abstract class Unification[F, G] private[FunctionUnification] {
			type LeftResult
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
		  * two types of information : shared argument list defined by `this.Returning` and return types of both these
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


		/** Given two function types `F` and `G` and their unification specified as their common argument list and types returned by applying to these arguments,
		  * create a new function
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

	/** Pairs two values (presumably curried functions) for future combining them into a single function by
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
	final class :*:[F, G](val _1 :F, val _2 :G)

	/** Enrich function type `G` with a right-associative method `:*:` which will pair it with another function so both
	  * can be combined into a single function by operating on their respective return types.
	  */
	@inline implicit def :*:[G <: (_ => _)](g :G) : FunctionFactor[G] = new FunctionFactor[G](g)

	/** Find the maximum unification of functions `F` and `G` (understood as list of arguments which can be given to both of them and return types of these applications).
	  * Returned object provides an `apply` method accepting a function operating on the return types of unified forms of `F` and `G` and returning a new
	  * function which will use it to produce the result.
	  */
	@inline implicit def :*:[F, Y, G, R](funs :F:*:G)(implicit unification :Unification[F, G])
			: FunReduce[F, unification.LeftResult, G, unification.RightResult, unification.Returning] =
		new FunReduce[F, unification.LeftResult, G, unification.RightResult, unification.Returning](funs._1, funs._2)(unification)






	/** Base trait for partially applied functions as a workaround for no implicit resolution of higher-kinded types. */
	sealed abstract class PartiallyApplied[X, A] {
		type Mapped[+G]

		val unapplied :Mapped[X=>A]

		def set[Y, R](x :X)(implicit ev :A<:<(Y=>R)) :Curried[Mapped, Y, R]

		def swap[W](x :W=>X) :Curried[Mapped, W, A]

		def mapped[G](map :((X => A) => G)) :Mapped[G]

		def map[Y, B](map :((X=>A) => (Y=>B))) :Curried[Mapped, Y, B]

		def >>[Y, R](implicit ev :A<:<(Y=>R)) :Curried[(Mapped AppliedTo X)#T, Y, R]

		def combined[Y, B, G](other :Mapped[Y=>B])(combine :(X=>A, Y=>B) => G) :Mapped[G]

		def take[W] :Curried[Mapped, W, X=>A]
	}




	object PartiallyApplied {

		/** Extends a `Curried` partially applied function returning a function `Y=>R`, providing methods for advancing to the next argument `Y`. */
		@inline implicit final def CurryOn[X, Y, R](curried :PartiallyApplied[X, Y=>R]) :CurryOn[curried.Mapped, X, Y, R] =
			new CurryOn[curried.Mapped, X, Y, R](curried)

		/** Extends a curried function type of more than one argument with methods for advancing through argument list. */
		@inline implicit final def CurryOn[X, Y, R](f :X=>Y=>R) :CurryOn[Self, X, Y, R] = new CurryOn[Self, X, Y, R](Curried(f))

		/** Extends a `Curried` partially applied function returning a function `Y=>R`, providing methods for advancing to the next argument `Y`.
		  * This class is an implicit extension of `BaseCurried` (and `Curried`) instances. It defines methods which would require
		  * implicit parameters if declared within parent `Curried`, which would prevent from directly calling apply on their results.
		  * By moving the implicit parameters to be the part of implicit conversion, we were able to remove them from the methods signature.
		  */
		final class CurryOn[C[+G], X, Y, R](private[this] val curried :PartiallyApplied[X, Y=>R] { type Mapped[+G] = C[G] })  {

			/** Move to the next argument, i.e. for `this` representing partial application of `F` until return type `X=>Y=>R`,
			  * return an instance representing the result of further application of `F` to `X`, with return type `Y=>R`.
			  */
			@inline def apply() :Curried[(C AppliedTo X)#T, Y, R] = curried.>>

			/** Take an additional, ignored argument of type `W` and automatically 'apply' it, so that curry point remains
			  * before argument `X` (and after the new argument `W`). You may specify the type of the parameter
			  * by providing type arguments either to this method, or parameterless `Curried.__.apply[X]`, for example:
			  * `Curry(f)()(__[W])`.
			  * @param newArg a marker class object
			  * @tparam W
			  * @return
			  */
			@inline def apply[W](newArg : __[W]) :Curried[(C AppliedTo W)#T, X, Y=>R] = curried.take[W].>>

			/** Provide a fixed value for this argument, removing it from the argument list and return a `Curried`
			  * instance representing first parameter after `X`, providing one exists. Created function is the same
			  * as with `applied(x)`, but allows subsequent modification in chained calls. This method is equivalent to `self.set(x)`.
			  * @return a Curried instance representing same application point (before argument `Y`) in the transformed function.
			  */
			@inline def apply(x :X) :Curried[C, Y, R] = curried.set(x)

			/** Substitute this argument by mapping intended argument type `W` to `X` before applying `f`.
			  * For example, given a function `f :F &lt;: D=>O=>X=>A` and `x :W=>X`, the result is `{d :D => o :O => w :W => f(d)(o)(x(w)) }`.*
			  * This method is equivalent to `self.swap(swap).>>`.
			  * @return a `Curried` instance representing the next position (after argument `W` and before `Y`) in the transformed function.
			  */
			@inline def apply[W](swap :W=>X) :Curried[(C AppliedTo W)#T, Y, R] = curried.swap(swap).>>


			/** Move to the next argument, i.e. for `this` representing partial application of `F` until return type `X=>Y=>R`,
			  * return an instance representing the result of further application of `F` to `X`, with return type `Y=>R`.
			  * Same as `apply()`, but avoids ambiguity if this instance is the return value of a function taking implicit parameters.
			  */
			@inline def __ :Curried[(C AppliedTo X)#T, Y, R] = curried.>>

			/** Convert `F` into a function with arguments `X, Y` in reverse order, i.e. from `Mapped[X=>Y=>R]` to `Mapped[Y=>X=>R]`. */
			@inline def transposed :C[Y=>X=>R] = //curried.curry.transpose(curried.unapplied)
				curried.mapped[Y=>X=>R](transposition) //{ res => y :Y => x :X => res(x)(y) }

			/** Return a `Curried` instance representing partial application of transposition of arguments `X, Y` in `F`.
			  * Represented curried function takes argumetns `X` and `Y` in reverse order, and is partially applied to same arguments
			  * as this instance (that is, all arguments preceeding `X` in `F`).
			  */
			@inline def transpose :Curried[C, Y, X=>R] = curried.map[Y, X=>R](transposition)

			@inline private def transposition = { res :(X=>Y=>R) => y :Y => x :X => res(x)(y) }

		}


		object CurryOn {
//			final class NextArg[N] private[Curry] (val curryOn :N)
//
//			sealed abstract class ThatWasTheLastOne {
//				@inline implicit def afterLast[X, A](curried :PartiallyApplied[X, A]) :NextArg[Result[curried.Mapped, X, A]] =
//					new NextArg(new Result[curried.Mapped, X, A](curried))
//			}
//
//			object NextArg extends ThatWasTheLastOne {
//				@inline implicit def nextArg[X, Y, A](curried :PartiallyApplied[X, Y=>A]) :NextArg[Curried[(curried.Mapped AppliedTo X)#T, Y, A]] =
//					new NextArg(curried.>>)
//			}
		}

	}

	/** A wrapper over a curried function `f :F` (with an arbitrary number of single-element argument lists) `F &lt;: C[X=>A]`, representing
	  * partial application of `f` up to (not including) `X` and providing methods for modifying this function from this argument on.
	  * Naming convention to which (most of) methods of this class conform is that an imperative verb form is used by methods returning
	  * another `Curried` instance (and thus allowing for modification of subsequent argumetns), while past perfect is reserved for
	  * methods returning the underlying function after requested transformation, signalling end of transformations; in particular, `unapplied`
	  * returns the underlying function itself as-is. In addition, there is an implicit conversion from instances of `Curried` which don't
	  * represent the last argument of the function (i.e. `A &lt;: _=>_` ) to `CurryOn` instance which provide much nicer `apply` syntax
	  * which advances to the next argument after each operation allowing for more succint - and hopefully readable - code.
	  *
	  * Example: `Curried{b:Byte => s:Short => i:Int => l:Long => s"\$b\$s\$i\$l"}()((_:String).toShort)(42).unapplied :Byte=>String=>Long=>String`.
	  * @param unapplied underlying function to which all functions created by this instance will delegate
	  * @param curry type constructor for partial application
	  * @tparam C Represents all arguments of `F` before `X`; `C[G] =:= A1=>A2=>...=>An=>G`, providing `F =:= A1=>A2=>...=>an=>X=>A`.
	  * @tparam X type of the first argument after partial application of F
	  * @tparam A result type of applying `f` with all arguments up to `X` (inclusive), possibly another function.
	  */
	final class Curried[C[+G], X, A](val unapplied :C[X=>A])(val curry :Curry[C, X, A]) extends PartiallyApplied[X, A] {
		/**Full type of this function. */
		type Fun = C[X=>A]
		/** Result type of partially applying `Fun` up to and including `X`. */
		type Applied = A
		/** Function `A1 => ... => An =>G`, where `A1..An` are all arguments of F preceeding `X`. */
		type Mapped[+G] = C[G]
		/** Type of `F` replacing `X` with `W`. */
		type Swapped[W] = Mapped[W=>A]

		/** Represent this instance (and the underlying function) as a function taking a sub-type `W` of argument `X` instead of `X`. */
		@inline def as[W<:X] :Curried[C, W, A] = new Curried[C, W, A](unapplied)(curry.mapped[W, A])

		/** Return the underlying function as its supertype accepting narrower argument type at this position. */
		@inline def arg[W<:X] :C[W=>A] = unapplied

		/** Provide a fixed value for this argument, removing it from the argument list.
		  * @return a function with same arguments as the underlying function but with `X` ommited, which invokes this function passing `x`
		  *         for this argument and its own arguments for others.
		  */
		@inline def applied(x :X) :Mapped[A] = curry.set(unapplied)(x)


		/** Provide a fixed value for this argument, removing it from the argument list and return a `Curried`
		  * instance representing first parameter after `X`, providing one exists. Created function is the same
		  * as with `apply(x)`, but allows subsequent modification in chained calls. If this instance represents
		  * the last argument, you may use `applied(x)` instead which returns the underlying function instead of
		  * a `Curried` instance. There is also an `apply` variant of this method available by implicit conversion
		  * to `CurryOn`: `self(x)`.
		  * @return a Curried instance representing same application point (before argument `Y`) in the transformed function.
		  */
		@inline def set[Y, R](x :X)(implicit ev :A<:<(Y=>R)) :Curried[C, Y, R] =
			new Curried[C, Y, R](curry.map[Y=>R](unapplied){ res :(X=>A) => ev(res(x))})(curry.mapped[Y, R])


		/** Substitute this argument by mapping intended argument type `W` to `X` before applying `f`.
		  * For example, given a function `f :F &lt;: D=>O=>X=>A` and `x :W=>X`, the result is `{d :D => o :O => w :W => f(d)(o)(x(w)) }`.
		  * @return a function resulting from substituting the argument `X` in the underlying function to `W` and mapping its value
		  *         using the passed function.
		  */
		@inline def swapped[W](x :W=>X) :Mapped[W=>A] = curry.swap(unapplied)(x)


		/** Substitute this argument by mapping intended argument type `W` to `X` before applying `f`.
		  * For example, given a function `f :F &lt;: D=>O=>X=>A` and `x :W=>X`, the result is `{d :D => o :O => w :W => f(d)(o)(x(w)) }`.
		  * There is a `apply` variant of this method available by implicit conversion to `CurryOn` which automatically advances to the next argument:
		  * `self{ w :W => w.toX }`.
		  * @return a `Curried` instance representing the same position (before argument `W`) in the transformed function.
		  */
		@inline def swap[W](x :W=>X) :Curried[C, W, A] =
			new Curried[C, W, A](curry.swap(unapplied)(x))(curry.mapped[W, A])


		/** Map the result of partial application of this function up to argument `X` (not including).
		  * For example, if `F =:= K=>L=>X=>A`, the result is a function `{k :K => l :L => map(f(k)(l)) }`.
		  * @param map function taking the result of applying F up until argument `X`.
		  */
		@inline def mapped[G](map :((X => A) => G)) :Mapped[G] = curry.map(unapplied)(map)

		/** Map the result of this partial application to another function `Y=>B`, returning a `Curried` instance representing
		  * the new function.
		  */
		@inline def map[Y, B](map :((X=>A) => (Y=>B))) :Curried[C, Y, B] =
			new Curried[C, Y, B](curry.map(unapplied)(map))(curry.mapped[Y, B])

		/** Map the result of partial application of this function up to argument X. Differs from `map` in that
		  * it maps the value returned after applying to `X`, while `map` maps whole function `X=>A`.
		  */
		@inline def returning[Y](map :A=>Y) :Mapped[X=>Y] =
			curry.map(unapplied){ _ andThen map }

		/** Create a function `W=>F` ignoring its first argument and returning this function `F` (unapplied). */
		@inline def composed[W] :W=>C[X=>A] = (w :W) => unapplied


		/** Create an instance representing partial application of function `W=>F` up to `X` (excluding).
		  * Underlying function will ignore its first argument.
		  */
		@inline def compose[W] :Curried[(C Compose W)#T, X, A] =
			new Curried[(C Compose W)#T, X, A]((w :W) => unapplied)(curry.composed[W])



		/** Create a function taking an additional, ignored argument `W` before argument `X`. */
		@inline def taking[W] :C[W=>X=>A] = mapped[W=>X=>A]{ f => _ => f }

		/** Insert a new, ignored argument `W` before `X`, remaining in the same application point (before `W`). */
		@inline def take[W] :Curried[C, W, X=>A] = new Curried[C, W, X=>A](taking[W])(curry.mapped[W, X=>A])



		/** Given another function sharing with `F` arguments preceding X, and a function combining results of partial application
		  * of both functions, return a single function taking shared arguments and invoking the combining function on results
		  * of applying `this.result` and `other` to received arguments.
		  */
		@inline def combined[Y, B, G](other :C[Y=>B])(combine :(X=>A, Y=>B) => G) :C[G] = curry.combine(unapplied, other)(combine)

		/** Given another `Curried` instance representing a funcction sharing the arguments preceding `X` with this function,
		  * combine the results of partial application to shared arguments using the function passed as the second argument.
		  * Note that this method is right-associative (and correspondingly, `this` function is the second argument of `combine`.
		  * Usage : `(Curried(other)()() :*: Curried(self)()()){ case (her, me) => ??? }`.
		  */
		@inline def *[Y, B, G](other :Curried[C, Y, B])(combine :(X=>A, Y=>B) => G) :C[G] =
			curry.combine(unapplied, other.unapplied)(combine)
		//curry.mapped[Y, B].combine(other.unapplied, unapplied)(combine)

//			@inline def *[B, G](other :Curried[C, X, B])(combine :(A, B) => G) :Curried[C, X, G] =
//				new Curried[C, X, G](
//					curry.combine(unapplied, other.unapplied){ (l :X=>A, r :X=>B) => x :X => combine(l(x), r(x)) }
//				)(curry.mapped[X, G])

		/** Skip to the next argument, i.e return an instance operating on the result of applying this function to argument `X`.
		  * Represents using an identity function on argument `x` before taking any subsequent arguments. An implicit conversion
		  * exists providing the same functionality with sweeter syntax (and without the implicit argument list): `this()` and/or `this.__`,
		  * for when `apply()` would be ambigous. This duplicity stems from the fact that implicit argument list required for implementation
		  * as a method prevents from shortened `apply` calls on the result of `>>`, but on the other hand IntelliJ IDE doesn't currently recognise
		  * implicit conversions using depenednent types (and scala doesn't allow for implicit conversions to types parameterized with higher kinds).
		  */
		@inline def >>[Y, R](implicit ev :A<:<(Y=>R)) :Curried[(C AppliedTo X)#T, Y, R] =
			new Curried[(C AppliedTo X)#T, Y, R](curry.map[X=>Y=>R](unapplied)((rest :X=>A) => rest andThen ev))(curry())


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
		@inline def apply[A, B](f :A=>B) :Curried[Self, A, B] = new Curried[Self, A, B](f)(new Arg0)

		/** Lifts a constant of type `C` to a function `Any=&gt;C` ignoring its argument; primarily used to make the value conform
		  * to a common curried function type (by optional further modifications) for combining with other functions with same argument list.
		  */
		@inline def ofAny[C](const :C) :Curried[Self, Any, C] = apply{ _ :Any => const }

		/** Represent the constant given as the argument (to object returned by this method) as a function of ignored argument `X`. */
		@inline def of[X] = new CurriedFunctionConstructor[X]

		/** Convert any contant to a function of `X`. */
		final class CurriedFunctionConstructor[X] {
			/** Convert the given constant value to a function `X=>C` which ignores its argument before returning the given value. */
			def apply[C](const :C) :Curried[Self, X, C] = Curried{ _ :X => const }
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
		sealed class __[X] private[Curried]()

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

