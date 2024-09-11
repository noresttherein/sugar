package net.noresttherein.sugar

import net.noresttherein.sugar.vars.Maybe.{Yes, No}
import net.noresttherein.sugar.vars.Maybe





//consider: moving everything to witness, and bringing casting under sugar.
package object typist {

	private[typist] final val Ver = 1L

	/** Tests if `left eq right`, executing the given block with the evidence to the identity as its argument,
	  * returning its result in a [[net.noresttherein.sugar.vars.Maybe Maybe]].
	  */
	def ifeq[T](left :AnyRef, right :AnyRef)(block :(left.type =:= right.type) => T) :Maybe[T] =
		if (left eq right)
			Yes(block(implicitly[left.type =:= left.type].asInstanceOf[left.type =:= right.type]))
		else No

	/** Curried type constructor for the function type X => Y.
	  * Accepts the desired return type as the type parameter and creates a type with a member type constructor `F`/`__`,
	  * accepting the desired argument type. Designed to be used as a type class
	  * (otherwise known as context bounds of generic classes and methods):
	  * {{{
	  *     def add[T: <%<[Int]#F](x :T, y :T) :Int = x + y
	  * }}}
	  * The name was chosen to bring to mind the old conversion type bound `X <% Y`.
	  */
	type <%<[Y] = {
		type F[-X] = X => Y
		type __[-X] = X => Y
	}

	/** Curried type constructor for the function type X => Y. Equivalent to [[net.noresttherein.sugar.typist.<%< <%<]];
	  * accepts the desired return type as the type parameter and creates a type
	  * with a member type constructor `Conversion` accepting the desired argument type.
	  * Designed to be used as a type class (context bound of generic classes and methods):
	  * {{{
	  *     def add[T: To[Int]#Conversion](x :T, y :T) :Int = x + y
	  * }}}
	  */
	type To[Y] = {
		type Conversion[-X] = X => Y
	}

	/** Lifts `X <:< Y` to a type class (AKA context bound) of `X`.
	  * @example {{{
	  *     def append[T: <:<#[Int]#_](first :Int, second :T) :Seq[Int] = Seq(first, second)
	  * }}}
	  * @see [[net.noresttherein.sugar.typist.>:>#]]
	  * @tparam Y a supertype of a type specified as type argument to member type `__[X]`.
	  */
	type <:<#[Y] = { type __[-X] = X <:< Y }

	/** Lifts `X <:< Y` to a type class (AKA context bound) of `X`.
	  * Same as [[net.noresttherein.sugar.typist.<:<# <:<#]]`[Y]#__`.
	  * @example {{{
	  *     def append[T: Supertype[Int]#Of](first :Int, second :T) :Seq[Int] = Seq(first, second)
	  * }}}
	  * @see [[net.noresttherein.sugar.typist.Subtype Subtype]]
	  * @tparam Y a supertype of a type specified as type argument to member type `Of[X]`.
	  */
	type Supertype[Y] = {
		type Of[-X] = X <:< Y
		type __[-X] = X <:< Y
	}

	/** Lifts `X <:< Y` to a type class (AKA context bound) of `Y`.
	  * @example {{{
	  *     def convert[Y: >:>#[X]#__](value :X, buffer :Buffer[Y]) :buffer.type = buffer += value
	  * }}}
	  * @tparam X a subtype of a type specified as type argument to member type `Of[Y]`.
	  * @see [[net.noresttherein.sugar.typist.<:<#]]
	  */
	type >:>#[X] = { type __[+Y] = X <:< Y }

	/** Lifts `X <:< Y` to a type class (AKA context bound) of `Y`.
	  * Same as [[net.noresttherein.sugar.typist.>:># >:>#]]`[X]#__`.
	  * @example {{{
	  *     def convert[Y: Subtype[X]#Of](value :X, buffer :Buffer[Y]) :buffer.type = buffer += value
	  * }}}
	  * @tparam X a subtype of a type specified as type argument to member type `Of[Y]`.
	  * @see [[net.noresttherein.sugar.typist.Subtype Subtype]]
	  */
	type Subtype[X] = { type Of[+Y] = X <:< Y }



	/** A lower bound of a linear phantom type hierarchy used to introduce subtyping relation to a single type `T`
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
	  * Note that, for `X < Y`, type `RankX >: RankY` rather than the other way round. This is to achieve the intuitive
	  * behaviour of definitions of lower rank having a higher precedence for covariant type parameters,
	  * as in the preceding example. Additionally, it is typically more convenient to start with the most
	  * generic, widely applicable definition, and then provide successively more specific definitions of higher
	  * precedence, than the opposite.
	  *
	  * Finally, note that a covariant declaration `+R` means values
	  * of [[net.noresttherein.sugar.typist.Rank.Rank0 Rank0]] have ''lower'' precedence
	  * than those of [[net.noresttherein.sugar.typist.Rank.Rank1 Rank1]], as it is typically more convenient
	  * to start with the most generic case and then introduce more specific declarations of a higher precedence by
	  * increasing the rank. It is, however, possible to invert this scheme by declaring the rank parameter
	  * as contravariant:
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
	  * In another use case, it may be desirable to offer the same implicit values under different paths,
	  * namely to allow wildcard imports to cover a wider or narrower list of definitions. For example,
	  * any collection extension class defined in `sugar.collections.extensions` is also available
	  * from `sugar.extensions`, to give the user a choice if they want to introduce all extensions
	  * defined by this library, or only those pertaining to a certain domain. Adding a `Rank` type parameter
	  * to an extension class and making sure that it differs depending on the scope it is imported from,
	  * avoids creating a conflict even if the user imports it from several scopes. In particular,
	  * IDEs tend to prefer wildcard imports over explicit importing of a large number of symbols,
	  * which may cause such a conflict without a direct action by the programmer.
	  * {{{
	  *     package A {
	  *         class Evidence[T, +R]
	  *         trait imports[+R] {
	  *             implicit def evidence[T] :Evidence[T, R] = ???
	  *         }
	  *     }
	  *     package object A extends imports[Rank0]
	  *     object all extends A.imports[Rank1] with B.imports[Rank1]
	  *
	  *     import A._
	  *     import all._ //No conflict, all uses of symbol `evidence` are resolved to `all.evidence`.
	  * }}}
	  */
	type Rank

	/** An abstract hack type, used primarily when there is a need to cast several higher types
	  * so they share a type parameter, whose value is not actually known. Avoiding use of any concrete type
	  * is safe/future-proof with regard to compiler inlining. This is particularly useful when encountering issues
	  * with existential/wildcard types, or to maintain a polymorphism of arrays, as casting an array to `Array[Any]`,
	  * or even `Array[E]`, if the code becomes inlined and in the caller's context `E` is a value type.
	  */
	type Unknown

}





package typist {

	object Rank {
		/** A higher rank than `R`. `R` is expected to be a [[net.noresttherein.sugar.typist.Rank Rank]] subtype,
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




package typist {

	import scala.reflect.ClassTag

	import net.noresttherein.sugar.typist.kinds
	//consider: moving it to witness
	/** A function class used for implicit conversions in order to force precedence of one definition over another,
	  * despite having the same argument and return types.
	  * Defining a conversion as `PriorityConversion[X, Y]` means it will be chosen (if in the implicit search scope,
	  * or important) over any competing implicit definition of a `X => Y`, or an implicit method
	  * with the same signature.
	  *
	  * Note: the conversion will box the argument and return value if either is an universal trait or a value class.
	  * However, extending `PriorityConversion` directly and overriding `apply` will lead to a compiler error
	  * if the wrapped value erases to `java.lang.Object`, due to bridge method for `apply` clashing
	  * with the overriding method. This can be avoided be extending
	  * [[net.noresttherein.sugar.typist.PriorityConversion.Wrapped PriorityConversion.Wrapped]] and defining
	  * a non-overriding `apply` method with a dummy implicit parameter:
	  * {{{
	  *     class Extension[T](val self :T) extends AnyVal {
	  *         //extension methods
	  *         def hello = "Hello, " + self
	  *     }
	  *     implicit def Extension[T] :ExtensionConversion =
	  *         new PriorityConversion.Wrapped(new Extension(_)) with ExtensionConversion[T]
	  *
	  *     trait ExtensionConversion[T] extends PriorityConversion[T, Extension[T]] {
	  *         def apply(self :T)(implicit dummy :DummyImplicit) = new Extension(self)
	  *     }
	  *     "Imoen".hello //<- the compiler generates a call to the non-boxing `apply` with an implicit argument.
	  * }}}
	  */
	trait PriorityConversion[-X, +Y] extends (X => Y)

	object PriorityConversion {
		/** Forces a SAM type promotion of a function literal `X => Y` to a `SpecificConversion[X, Y]` and immediately
		  * returns it.
		  */
		@inline def apply[X, Y](conversion :PriorityConversion[X, Y]) :PriorityConversion[X, Y] = conversion

		/** Promotes a function to a `SpecificConversion[X, Y]`. */
		@inline def wrap[X, Y](f :X => Y) :PriorityConversion[X, Y] = f(_)

		class Wrapped[-X, +Y](f :X => Y) extends PriorityConversion[X, Y] {
			override def apply(v1 :X) :Y = f(v1)
		}
	}


	/** A generic variant of `<:<`, projecting the relation from types to type constructors. */
	//consider: a better name.
	sealed abstract class <:?<[-A[_], +B[_]] extends Serializable {
		def apply[X](value :A[X]) :B[X]
	}

	object <:?< {
		implicit def summon[A[_]] :A =:?= A = instance.asInstanceOf[A =:?= A]
		private[this] val instance = new Evidence[kinds.Any1]

		private class Evidence[A[_]] extends =:?=[A, A] {
			override def apply[X](value :A[X]) :A[X] = value
		}
	}

	/** A generic variant of `=:=`, projecting the relation from types to type constructors. */
	sealed abstract class =:?=[A[_], B[_]] extends <:?<[A, B]

	object Unknown {
		implicit val unknownAsAnyClassTag :ClassTag[Unknown] = ClassTag(classOf[Any])
	}
}
