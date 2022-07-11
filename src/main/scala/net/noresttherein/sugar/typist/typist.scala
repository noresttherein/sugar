package net.noresttherein.sugar

import scala.annotation.unspecialized

import net.noresttherein.sugar.funny.fun.{ComposableFun, Identity}
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.vars.Opt






package object typist {

	implicit class extensions_<:<[A, B](private val ev :A <:< B) extends AnyVal {
		/** Provides counterparts of lift/substitute methods of `A <:< B` working with functors
		  * with an upper bound `U` on their argument(s).
		  */
		@inline def ub[U] = new bound_<:<[A with B with U, A with U, B with U, U](ev.liftCo[({ type F[+X] = X with U })#F])

		/** Provides counterparts of lift/substitute methods of `A <:< B` working with functors
		  * with a lower bound `L` on their argument(s).
		  */
		@inline def lb[L <: A with B] = new bound_<:<[L, A, B, Any](ev)
	}

	implicit class extensions_=:=[A, B](private val ev :A =:= B) extends AnyVal {
		/** Provides counterparts of lift/substitute methods of `A =:= B` working with functors
		  * with an upper bound `U` on their argument(s).
		  */
		@inline def ub[U] = new bound_=:=[Nothing, A with U, B with U, U](ev.liftCo[({ type F[X] = X with U })#F])

		/** Provides counterparts of lift/substitute methods of `A =:= B` working with functors
		  * with a lower bound `U` on their argument(s).
		  */
		@inline def lb[L <: A with B] = new bound_=:=[L, A, B, Any](ev)
	}

	/** Overloads of standard methods of `A <:< B` working with covariant and contravariant functors with type bounds
	  * placed on arguments `F[_ >: L <: B]` instead of freely applicable `F[_]`.
	  */
	class bound_<:<[L <: U, A >: L <: U, B >: L <: U, U] private[typist] (private val ev :A <:< B) extends AnyVal {
		/** Provides counterparts of lift/substitute methods of `A <:< B` working with functors
		  * with argument bounds `_ >: T <: U`.
		  */
		@inline final def lb[T <: A with B] = new bound_<:<[T, A, B, U](ev)
//
//		/** Provides counterparts of lift/substitute methods of `A <:< B` working with functors
//		  * with argument bounds `_ >: L <: T`.
//		  */
//		@inline final def ub[T >: U]        = new bound_<:<[L, A, B, T](ev)

		@inline final def liftCo[F[+_ >: L <: U]]     :F[A] <:< F[B] = ev.asInstanceOf[F[A] <:< F[B]]
		@inline final def liftContra[F[-_ >: L <: U]] :F[B] <:< F[A] = ev.asInstanceOf[F[B] <:< F[A]]

		@inline final def substituteCo[F[+_ >: L <: U]](f :F[A])                    :F[B] = f.asInstanceOf[F[B]]
		@inline final def substituteContra[F[-_ >: L <: U]](f :F[B])                :F[A] = f.asInstanceOf[F[A]]
		@inline final def substituteBoth[F[-X >: L <: U, +Y >: L <: U]](f :F[B, A]) :F[A, B] = f.asInstanceOf[F[A, B]]
	}

	/** Overloads of standard methods of `A =:= B` working with covariant and contravariant functors with type bounds
	  * placed on arguments `F[_ >: L <: B]` instead of freely applicable `F[_]`.
	  */
	class bound_=:=[L <: U, A >: L <: U, B >: L <: U, U] private[typist] (private val ev :A =:= B) extends AnyVal {
		/** Provides counterparts of lift/substitute methods of `A =:= B` working with functors
		  * with argument bounds `_ >: T <: U`.
		  */
		@inline final def lb[T <: A with B] = new bound_=:=[T, A, B, U](ev)
//
//		/** Provides counterparts of lift/substitute methods of `A =:= B` working with functors
//		  * with argument bounds `_ >: L <: T`.
//		  */
//		@inline final def ub[T >: U]        = new bound_=:=[L, A, B, T](ev)

		@inline final def liftCo[F[_ >: L <: U]]     :F[A] =:= F[B] = ev.asInstanceOf[F[A] =:= F[B]]
		@inline final def liftContra[F[_ >: L <: U]] :F[B] =:= F[A] = ev.asInstanceOf[F[B] =:= F[A]]

		@inline final def substituteCo[F[_ >: L <: U]](f :F[A])                   :F[B] = f.asInstanceOf[F[B]]
		@inline final def substituteContra[F[_ >: L <: U]](f :F[B])               :F[A] = f.asInstanceOf[F[A]]
		@inline final def substituteBoth[F[_ >: L <: U, _ >: L <: U]](f :F[B, A]) :F[A, B] = f.asInstanceOf[F[A, B]]
	}




	/** Tests if `left eq right`, executing the given block with the evidence to the identity as its argument,
	  * returning its result in an [[net.noresttherein.sugar.vars.Opt Opt]].
	  */
	def ifeq[T](left :AnyRef, right :AnyRef)(block :(left.type =:= right.type) => T) :Opt[T] =
		if (left eq right)
			Got(block(implicitly[left.type =:= left.type].asInstanceOf[left.type =:= right.type]))
		else Lack

	/** Curried type constructor for the function type X => Y. Accepts the desired return type as the type parameter
	  * and creates a type with a member type constructor `F` accepting the desired argument type. Designed to be used
	  * as a type class (context bound of generic classes and methods):
	  * {{{
	  *     def add[T: <%<[Int]#F](x :T, y :T) :Int = x + y
	  * }}}
	  * The name was chosen to bring to mind the old conversion type bound `X <% Y`.
	  */
	type <%<[Y] = { type F[-X] = X => Y }




	final val ValueTypes = new Specializable.Group(Int, Long, Float, Double, Boolean, Char)


	object <=: {
		implicit def typeEquiv[@specialized(ValueTypes) X] :X ==: X = new TypeEquiv[X]

		implicit def scalaSubtypeRelation[X, Y](sub :X<=:Y) :X<:<Y = sub.<:<
		implicit def scalaTypeEquivalency[X, Y](equiv :X==:Y) :X=:=Y = equiv.=:=
	}
	private[this] final val ErasedEquiv = new TypeEquiv[Any]
	@inline private[this] def genericEquiv[X] = ErasedEquiv.asInstanceOf[TypeEquiv[X]]


	/** A specialized equivalent of standard scala `<:<`, attesting that type `X` is a subtype of type `Y`.
	  * Introduced for specialization (including forcing specialization on methods declaring it as a parameter)
	  * and to provide transitive by covariance and contravariance instances for higher types constructed using `X` and `Y`.
	  * An implicit instance exists everywhere where the compiler would accept a value of `X` for type `Y`.
	  * Being a function `X=>Y`, an implicit value serves as an implicit (identity) conversion from `X` to `Y`.
	  * @see [[net.noresttherein.sugar.typist.==: ==:]]
	  */
	sealed trait <=:[@specialized(ValueTypes) -X, @specialized(ValueTypes) +Y] extends ComposableFun[X, Y] {
		def <:< :X<:<Y

		def cotype[F[+T]] :F[X]<=:F[Y]
		def contratype[F[-T]] :F[Y]<=:F[X]
		def copair[F[+A, -R]] :F[X, Y]<=:F[Y, X]
		def contrapair[F[-A, +R]] :F[Y, X]<=:F[X, Y]

	}



	/** A specialized equivalent of standard scala `=:=`, attesting that type `X` is equivalent to type `Y`
	  * (values of `X` are accepted for type `Y` and vice versa). Introduced for specialization and
	  * to provide instances for equivalency relation inferred by inversion and higher type application.
	  * An implicit instance exists everywhere where the compiler would accept a value of `X` for type `Y` and vice versa.
	  */
	sealed trait ==:[@specialized(ValueTypes) X, @specialized(ValueTypes) Y] extends <=:[X, Y] {
		def =:= :X=:=Y
		def >:> :Y<:<X

		override def cotype[F[+T]] :F[X]==:F[Y]
		override def contratype[F[-T]] :F[Y]==:F[X]
		override def copair[F[A, R]] :F[X, Y]==:F[Y, X]
		override def contrapair[F[A, R]] :F[Y, X]==:F[X, Y]

		@unspecialized
		def inverse :Y==:X

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[==:[_, _]]

		override def toString :String = {
			val tpe = domainString
			tpe + "==:" + tpe
		}
	}



	private final class TypeEquiv[@specialized(ValueTypes) X] extends (X==:X) with Identity[X] with Serializable {
		override def <:< :X <:< X = implicitly[X<:<X]
		override def >:> :X <:< X = implicitly[X<:<X]
		override def =:= :X =:= X = implicitly[X=:=X]


		override def cotype[F[+T]] :F[X] ==: F[X] = genericEquiv
		override def contratype[F[-T]] :F[X] ==: F[X] = genericEquiv
		override def copair[F[A, R]] :F[X, X] ==: F[X, X] = genericEquiv
		override def contrapair[F[A, R]] :F[X, X] ==: F[X, X] = genericEquiv

		@unspecialized
		override def inverse: X==:X = this

	}



	/** A root of a phantom type hierarchy used to introduce subtyping relation to a single type `T`
	  * (typically an implicit witness) by an addition of an artificial type parameter `R` to the latter.
	  * This can be particularly useful when there is a need to introduce precedence between otherwise identical
	  * implicit values. For example, Let us define a generic, ''invariant'' evidence type class:
	  * {{{
	  *     class Evidence[T]
	  * }}}
	  * Given two types, `A` and `B`, we might want for `Evidence[A]` to be preferable to `Evidence[B]` where both are
	  * applicable (for example, if `A <: B`). Because `Evidence` is invariant, there is no precedence of
	  * `Evidence[A]` over `Evidence[B]`. While it can be achieved by introducing subtyping between their declaration
	  * scopes (or their companions), it is not always possible (as with definitions declared directly in a package)
	  * or desirable (if users are expected to provide their own `Evidence` instances and cannot take advantage
	  * of the precedence coming from subtyping of declaration scopes). A phantom type parameter of `Rank`
	  * comes to rescue:
	  * {{{
	  *     class Evidence[T, +R]
	  *     implicit def evidenceA :Evidence[A, Rank0] = ???
	  *     implicit def evidenceB :Evidence[B, Rank1] = ??? //has precedence over evidenceA
	  * }}}
	  * Such declarations are also cleaner than splitting them between several base classes.
	  * Finally note, that a covariant declaration `+R` means values
	  * of [[net.noresttherein.sugar.typist.Rank.Rank0 Rank0]] have ''lower'' precedence
	  * than those of [[net.noresttherein.sugar.typist.Rank.Rank1 Rank1]], as it is typically more convenient
	  * to start with the most generic case and then introduce more specific cases with a higher precedence by
	  * increasing the rank, it is possible to invert this scheme by declaring the rank parameter as contravariant:
	  * {{{
	  *     class Evidence[T, -R]
	  *     implicit def evidenceA :Evidence[A, Rank1] = ???
	  *     implicit def evidenceB :Evidence[B, Rank0] = ??? //same as before, has precedence over evidenceA
	  * }}}
	  * Whatever the decision was made at introduction, it is always possible to add cases with a ''lower'' precedence
	  * to the existing ones (or higher, if the `Rank` type parameter is contravariant)
	  * with [[net.noresttherein.sugar.typist.Rank.- -]]`[R]`:
	  * {{{
	  *     class Evidence[T, +R]
	  *     implicit def evidenceA :Evidence[A, Rank0]
	  *     implicit def evidenceO :Evidence[O, -[Rank0]] //has lesser precedence than evidenceA
	  * }}}
	  *
	  * In another use case, the same implicit value/conversion can be defined in several places
	  * (a companion object to some related class, a `syntax` package containing all implicits in the library,
	  * or a `imports` trait to be extended by application classes/package objects) - introducing
	  * a `Rank` type parameter forces a precedence between these definitions if otherwise several candidates
	  * are available (for example, by an explicit import from a specific location and an IDE-introduced wildcard import).
	  */
	type Rank

}





package typist {

	object Rank {
		/** A higher rank than `R`. `R` is assumed to be a [[net.noresttherein.sugar.typist.Rank Rank]] subtype,
		  * but the type bound is omitted for brevity. Types parameterized with `+[R]` are subtypes of the same
		  * type constructors applied to `R` (assuming covariance in the `Rank` parameter). Multiple predefined aliases
		  * for composition of `+` with `Rank` are defined in the same scope:
		  * [[net.noresttherein.sugar.typist.Rank.Rank0 Rank0]], [[net.noresttherein.sugar.typist.Rank.Rank1 Rank1]],
		  * [[net.noresttherein.sugar.typist.Rank.Rank0 Rank2]], [[net.noresttherein.sugar.typist.Rank.Rank1 Rank3]], ...
		  */
		type +[R] <: R
		/** A lower rank than `R`. This a supertype of `R` and thus grants lower precedence to the parameterized type
		  * if it is covariant in the `Rank` parameter, or higher precedence if it is contravariant.
		  * @see [[net.noresttherein.sugar.typist.Rank.+]]
		  */
		type -[R <: Rank] >: R <: Rank
		/** The 'base' rank, the supertype of all other `RankX` type aliases. */
		type Rank0 <: Rank
		type Rank1 >: +[Rank0] <: Rank0//= +[Rank0]
		type Rank2 >: +[Rank1] <: Rank1//= +[+[Rank0]]//+[Rank1]
		type Rank3 >: +[Rank2] <: Rank2//= +[+[+[Rank0]]]//+[Rank2]
		type Rank4 >: +[Rank3] <: Rank3//= +[+[+[+[Rank0]]]]//+[Rank3]
		type Rank5 >: +[Rank4] <: Rank4//= +[+[+[+[+[Rank0]]]]]//+[Rank4]
		type Rank6 >: +[Rank5] <: Rank5//= +[Rank5]
		type Rank7 >: +[Rank6] <: Rank6//= +[Rank6]
		type Rank8 >: +[Rank7] <: Rank7//= +[Rank7]
		type Rank9 >: +[Rank8] <: Rank8//= +[Rank8]
	}

}