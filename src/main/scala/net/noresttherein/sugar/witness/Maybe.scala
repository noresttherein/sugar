package net.noresttherein.sugar.witness

import net.noresttherein.sugar.extensions.castingMethods
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.witness
import net.noresttherein.sugar.witness.Maybe.NoImplicit


/** A type for which an implicit value is always present, however, if an implicit value for `T` can be found,
  * it is exposed as `Got[T]` through this instances [[net.noresttherein.sugar.witness.Maybe.opt opt]] method.
  */ //consider: renaming to Optional/Wanted
class Maybe[+T] private[witness] (private val content :AnyRef) extends AnyVal {
	def opt :Opt[T] = if (content == NoImplicit) Lack else Got(content.asInstanceOf[T])

	/** Returns the content of this maybe or a default alternative if not content is available. */
	@inline def getOrElse[O >: T](alternative: => O) :O  = content match {
		case NoImplicit => alternative
		case o :O @unchecked => o
	}

	@inline def isDefined :Boolean = opt.isDefined

	override def toString = content.toString
}



private[witness] sealed abstract class MaybeNoImplicit {
	@inline implicit final def maybeNo[T] :Maybe[T] = Maybe.none
}



/** Provides optional implicit values if they are available wrapped as `Maybe[T]`. */
@SerialVersionUID(Ver)
object Maybe extends MaybeNoImplicit {
	/** A wrapper type for a type constructor `F` allowing to use a `Maybe` for a generic type as a type class
	  * (a ''view bound'') using syntax
	  * {{{
	  *     trait CanSing[T] { def sing(title :String) :Unit }
	  *
	  *     def method[X :TypeClass[CanSing]#Maybe](x :X) =
	  *         Maybe[CanSing[X]].opt.foreach { _.sing("Drunken sailor") }
	  * }}}
	  */
	type TypeClass[F[_]] = {
		/** A type constructor for [[net.noresttherein.sugar.witness.Maybe Maybe]]`[F[T]]`,
		  * where `F` is a type class/type constructor specified as the preceding argument to `TypeCass`.
		  */
		type Maybe[X] = witness.Maybe[F[X]]
	}

	/** A shorter alternative for [[net.noresttherein.sugar.witness.Maybe.TypeClass TypeClass]]`[F]` composing
	  * [[net.noresttherein.sugar.witness.Maybe! Maybe]] with a specified a type constructor
	  * by defining a member type constructor `T` applying said type constructor to its type argument
	  * as a `Maybe[F[X]]`. In this way, `Maybe.a[F]#T` is a view bound for any view bound `F`.
	  */
	type a[F[_]] = { type T[X] = Maybe[F[X]] }

	/** Summons an optional implicit `T` instance as an `Opt[T]` instance. */
	@inline def apply[T](implicit maybe :Maybe[T]) :Opt[T] = maybe.opt

	/** Returns an implicit instance o `T` or a default alternative provided as an argument.  */
	@inline def orElse[T](alternative: => T)(implicit maybe :Maybe[T]) :T = maybe.opt match {
		case Got(value) => value
		case _ => alternative
	}

	/** Extracts the optional content of a `Maybe[T]`. This pattern will match only 'yes' instances. */
	def unapply[T](maybe :Maybe[T]) :Opt[T] = maybe.opt

	/** An empty `Maybe` conforming to any `Maybe[TT]` type. */
	final val none = new Maybe[Nothing](NoImplicit)

	/** Explicitly wraps an implicit evidence instance o `T` in a `Maybe`. */
	def some[T](implicit evidence :T) :Maybe[T] = new Maybe(evidence.asAnyRef)


	@inline implicit def maybeYes[T](implicit e :T) :Maybe[T] = new Maybe(e.asAnyRef)

	/** An implicit conversion from any value of type `T` to a `Maybe[T]`. */
	implicit def anyToMaybe[T](value :T) :Maybe[T] = new Maybe(value.asAnyRef)

	private[witness] case object NoImplicit
}




sealed trait WithDefault[+T, +D] //todo: get :T|D


object WithDefault {
	implicit def provided[T](implicit evidence :T) :Provided[T] = Provided(evidence)
	implicit def default[D](implicit evidence :D) :Default[D] = Default(evidence)

	final case class Provided[+T](get :T) extends WithDefault[T, Nothing]
	final case class Default[+D](get :D) extends WithDefault[Nothing, D]
}
