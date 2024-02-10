package net.noresttherein.sugar.typist

import net.noresttherein.sugar.funny.fun.Identity
import net.noresttherein.sugar.reflect.Specialized.{NotCached, Primitives, Vals}
import net.noresttherein.sugar.typist.extensions.{extensions_<:<, extensions_=:=}
import net.noresttherein.sugar.witness.Ignored


private[typist] sealed trait extensionsLowPriority extends Any {
	implicit final def extensions_<:<[A, B](ev :A <:< B) :extensions_<:<[A, B] = new extensions_<:<[A, B](ev)
}


trait extensions extends Any with extensionsLowPriority {
	implicit final def extensions_=:=[A, B](ev :A =:= B) :extensions_=:=[A, B] = new extensions_=:=(ev)

	implicit final def unlift[F[_ >: L <: U], L <: U, U, A >: L <: U, B >: L <: U]
			:extensions.unlift[F, L, U, F[A], F[B]] { type _1 = A; type _2 = B } =
		new extensions.unlift[F, L, U, F[A], F[B]] {
			override type _1 = A
			override type _2 = B
		}
}




/** Declarations of classes with extension methods for `<:<` and `=:=`
  * and the implicit conversions to these classes. They can also be imported together with all other
  * extension methods in this library from object `sugar.`[[net.noresttherein.sugar.extensions extensions]].
  */ //Consider: if we moved this to witness, we could bring casting to top level.
@SerialVersionUID(Ver)
object extensions extends extensions {
	@SerialVersionUID(Ver)
	object spec_<:< {
		implicit def identity[X] :X spec_=:= X = RefIsRef.asInstanceOf[X spec_=:= X]
		private case object RefIsRef extends Identity[Unknown] with spec_=:=[Unknown, Unknown]
		implicit case object ByteIsByte extends Identity[Byte] with spec_=:=[Byte, Byte]
		implicit case object ShortIsShort extends Identity[Short] with spec_=:=[Short, Short]
		implicit case object CharIsChar extends Identity[Char] with spec_=:=[Char, Char]
		implicit case object IntIsInt extends Identity[Int] with spec_=:=[Int, Int]
		implicit case object LongIsLong extends Identity[Long] with spec_=:=[Long, Long]
		implicit case object FloatIsFloat extends Identity[Float] with spec_=:=[Float, Float]
		implicit case object DoubleIsDouble extends Identity[Double] with spec_=:=[Double, Double]
		implicit case object BooleanIsBoolean extends Identity[Boolean] with spec_=:=[Boolean, Boolean]
	}

	/** A `@specialized` variant of standard `<:<`. Can be obtained through an extension method from any `<:<`. */
	@SerialVersionUID(Ver)
	sealed trait spec_<:<[@specialized(Vals) -X, @specialized(Vals) +Y] extends (X => Y) with Serializable {
		def unspec :X <:< Y
		final override def apply(sub :X) :Y = sub.asInstanceOf[Y]
	}

	/** A `@specialized` variant of standard `=:=`. Can be obtained through an extension method from any `=:=`. */
	@SerialVersionUID(Ver)
	sealed trait spec_=:=[@specialized(Vals) X, @specialized(Vals) Y] extends spec_<:<[X, Y] {
		final override def unspec :X =:= Y = <:<.refl.asInstanceOf[X =:= Y]
		@inline final def flip :spec_=:=[Y, X] = this.asInstanceOf[spec_=:=[Y, X]]
	}


	class extensions_<:<[A, B](private val ev :A <:< B) extends AnyVal {
		/** A `@specialized` version of `A <:< B`.
		  * It is an implicit value, hence importing it will automatically enable implicit conversion.
		  */ //fixme: this makes no sense, as we'll never get a specialized instance.
		@inline implicit def spec :A spec_<:< B = spec_<:<.identity.asInstanceOf[A spec_<:< B]

		/** Provides counterparts of lift/substitute methods of `A <:< B` working with functors
		  * with an upper bound `U` on their argument(s).
		  */
		@inline def ub[U] = new bound_<:<[A with B with U, A with U, B with U, U](ev.liftCo[({ type F[+X] = X with U })#F])

		/** Provides counterparts of lift/substitute methods of `A <:< B` working with functors
		  * with a lower bound `L` on their argument(s).
		  */
		@inline def lb[L <: A with B] = new bound_<:<[L, A, B, Any](ev)

		/** Unwraps evidence type parameters `F[X] <:< F[Y]` into `X <:< Y`. */
		@inline def unliftCo[F[+_]](implicit unlift :unlift[F, Nothing, Any, A, B]) :unlift._1 <:< unlift._2 =
			ev.asInstanceOf[unlift._1 <:< unlift._2]

		@inline def unliftCoUp[F[+_ <: U], U](implicit unlift :unlift[F, Nothing, U, A, B]) :unlift._1 <:< unlift._2 =
			ev.asInstanceOf[unlift._1 <:< unlift._2]

		@inline def unliftCoLow[F[+_ >: L], L](implicit unlift :unlift[F, L, Any, A, B]) :unlift._1 <:< unlift._2 =
			ev.asInstanceOf[unlift._1 <:< unlift._2]

		@inline def unliftCoBoth[F[+_ >: L <: U], L <: U, U]
		                        (implicit unlift :unlift[F, L, U, A, B]) :unlift._1 <:< unlift._2 =
			ev.asInstanceOf[unlift._1 <:< unlift._2]

		/** Unwraps evidence type parameters `F[X] <:< F[Y]`, reversing their order, into `Y <:< X`. */
		@inline def unliftContra[F[-_]](implicit unlift :unlift[F, Nothing, Any, A, B]) :unlift._2 <:< unlift._1 =
			ev.asInstanceOf[unlift._2 <:< unlift._1]

		@inline def unliftContraUp[F[-_ <: U], U](implicit unlift :unlift[F, Nothing, U, A, B]) :unlift._2 <:< unlift._1 =
			ev.asInstanceOf[unlift._2 <:< unlift._1]

		@inline def unliftContraLow[F[-_ >: L], L](implicit unlift :unlift[F, L, Any, A, B]) :unlift._2 <:< unlift._1 =
			ev.asInstanceOf[unlift._2 <:< unlift._1]

		@inline def unliftContraBoth[F[-_ >: L <: U], L <: U, U]
		                            (implicit unlift :unlift[F, L, U, A, B]) :unlift._2 <:< unlift._1 =
			ev.asInstanceOf[unlift._2 <:< unlift._1]
	}

	class extensions_=:=[A, B](private val ev :A =:= B) extends AnyVal {
		/** A `@specialized` version of `A =:= B`.
		  * It is an implicit value, hence importing it will automatically enable implicit conversion.
		  */
		@inline implicit def spec :A spec_=:= B = spec_<:<.identity.asInstanceOf[A spec_=:= B]

		/** Provides counterparts of lift/substitute methods of `A =:= B` working with functors
		  * with an upper bound `U` on their argument(s).
		  */
		@inline def ub[U] = new bound_=:=[Nothing, A with U, B with U, U](ev.liftCo[({ type F[X] = X with U })#F])

		/** Provides counterparts of lift/substitute methods of `A =:= B` working with functors
		  * with a lower bound `U` on their argument(s).
		  */
		@inline def lb[L <: A with B] = new bound_=:=[L, A, B, Any](ev)

		/** Swaps the order of type parameter in this type equivalence evidence. */
		@inline def swap :B =:= A = ev.asInstanceOf[B =:= A]

		@inline def unlift[F[_]](implicit unlift :unlift[F, Nothing, Any, A, B]) :unlift._1 =:= unlift._2 =
			ev.asInstanceOf[unlift._1 =:= unlift._2]

		@inline def unliftUp[F[_ <: U], U](implicit unlift :unlift[F, Nothing, U, A, B]) :unlift._1 =:= unlift._2 =
			ev.asInstanceOf[unlift._1 =:= unlift._2]

		@inline def unliftLow[F[_ >: L], L](implicit unlift :unlift[F, L, Any, A, B]) :unlift._1 =:= unlift._2 =
			ev.asInstanceOf[unlift._1 =:= unlift._2]

		@inline def unliftBoth[F[_ >: L <: U], L <: U, U]
		                      (implicit unlift :unlift[F, L, U, A, B]) :unlift._1 =:= unlift._2 =
			ev.asInstanceOf[unlift._1 =:= unlift._2]
	}

	/** Overloads of standard methods of `A <:< B` working with covariant and contravariant functors with type bounds
	  * placed on arguments `F[_ >: L <: B]` instead of freely applicable `F[_]`.
	  */
	class bound_<:<[L <: U, A >: L <: U, B >: L <: U, U] private[typist] (private val ev :A <:< B) extends AnyVal {
		/** Provides counterparts of lift/substitute methods of `A <:< B` working with functors
		  * with argument bounds `_ >: T <: U`.
		  */
		@inline final def lb[T <: A with B] = new bound_<:<[T, A, B, U](ev)

		/** Provides counterparts of lift/substitute methods of `A <:< B` working with functors
		  * with argument bounds `_ >: L <: T`.
		  */
		@inline final def ub[T >: L] = new bound_<:<[L, A with T, B with T, T](ev.asInstanceOf[(A with T) <:< (B with T)])

		@inline final def liftCo[F[+_ >: L <: U]]     :F[A] <:< F[B] = ev.asInstanceOf[F[A] <:< F[B]]
		@inline final def liftContra[F[-_ >: L <: U]] :F[B] <:< F[A] = ev.asInstanceOf[F[B] <:< F[A]]

		@inline final def substituteCo[F[+_ >: L <: U]](f :F[A])                    :F[B]    = f.asInstanceOf[F[B]]
		@inline final def substituteContra[F[-_ >: L <: U]](f :F[B])                :F[A]    = f.asInstanceOf[F[A]]
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

		/** Provides counterparts of lift/substitute methods of `A =:= B` working with functors
		  * with argument bounds `_ >: L <: T`.
		  */
		@inline final def ub[T >: L] = new bound_=:=[L, A with T, B with T, T](ev.asInstanceOf[(A with T) =:= (B with T)])

		@inline final def liftCo[F[_ >: L <: U]]     :F[A] =:= F[B] = ev.asInstanceOf[F[A] =:= F[B]]
		@inline final def liftContra[F[_ >: L <: U]] :F[B] =:= F[A] = ev.asInstanceOf[F[B] =:= F[A]]

		@inline final def substituteCo[F[_ >: L <: U]](f :F[A])                   :F[B]    = f.asInstanceOf[F[B]]
		@inline final def substituteContra[F[_ >: L <: U]](f :F[B])               :F[A]    = f.asInstanceOf[F[A]]
		@inline final def substituteBoth[F[_ >: L <: U, _ >: L <: U]](f :F[B, A]) :F[A, B] = f.asInstanceOf[F[A, B]]
	}



	sealed trait unlift[F[_ >: L <: U], L <: U, U, A, B] extends Any {
		type _1 >: L <: U
		type _2 >: L <: U
	}


	class unliftCo_<:<[A <: U, B <: U, U, F[+_ >: A with B <: U]](private val ev :F[A] <:< F[B]) extends AnyVal {
		def unliftCo :A <:< B = ev.asInstanceOf[A <:< B]
	}
	class unlifitContra_<:<[A <: U, B <: U, U, F[-_ >: A with B <: U]](private val ev :F[A] <:< F[B])
		extends AnyVal
	{
		def unliftContra :B <:< A = ev.asInstanceOf[B <:< A]
	}

	def unlift_=:=[A, B, F[_ >: A with B]](ev :F[A] =:= F[B]) = new unlift_=:=[A, B, Any, F](ev)

	class unlift_=:=[A <: U, B <: U, U, F[_ >: A with B <: U]](private val ev :F[A] =:= F[B]) extends AnyVal {
		def unlift :A =:= B = ev.asInstanceOf[A =:= B]
	}

}
