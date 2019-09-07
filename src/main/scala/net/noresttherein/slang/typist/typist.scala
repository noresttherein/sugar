package net.noresttherein.slang

import net.noresttherein.slang.funny.fun.{ComposableFun, Identity}

import scala.annotation.unspecialized

package object typist {

	/** Curried type constructor for the function type X => Y. Accepts the desired return type as the type parameter
	  * and creates aa type with a member type constructor `F` accepting the desired argument type. Designed to be used
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


	/** A specialized equivalent of standard scala `&lt;:&lt;`, attesting that type `X` is a subtype of type `Y`.
	  * Introduced for specialization (including forcing specialization on methods declaring it as a parameter)
	  * and to provide transitive by covariance and contravariance instances for higher types constructed using `X` and `Y`.
	  * An implicit instance exists everywhere where the compiler would accept a value of `X` for type `Y`.
	  * Being a function `X=>Y`, an implicit value serves as an implicit (identity) conversion from `X` to `Y`.
	  * @see [[net.noresttherein.slang.typist.==: ==:]]
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



}

