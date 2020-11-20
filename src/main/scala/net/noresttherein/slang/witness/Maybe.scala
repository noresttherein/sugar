package net.noresttherein.slang.witness




/** A type for which an implicit value is always present, however, if an implicit value for `T` can be found,
  * it is exposed as `Some[T]` through this instances [[net.noresttherein.slang.witness.Maybe.opt opt]] method.
  */
class Maybe[+T] private[witness] (val opt :Option[T]) extends AnyVal


sealed abstract class MaybeNoImplicit {
	implicit final val maybeNo :Maybe[Nothing] = new Maybe(None)
}

/** Provides optional implicit values if they are available wrapped as `Maybe[T]`. */
object Maybe extends MaybeNoImplicit {
	def apply[T](implicit maybe :Maybe[T]) :Option[T] = maybe.opt

	def unapply[T](maybe :Maybe[T]) :Option[T] = maybe.opt


	final val none = new Maybe[Nothing](None)

	def some[T](implicit evidence :T) :Maybe[T] = new Maybe(Some(evidence))


	implicit def maybeYes[T](implicit e :T) :Maybe[T] = new Maybe(Some(e))

	implicit def explicit[T](value :T) :Maybe[T] = new Maybe(Some(value))

}




sealed trait WithDefault[+T, +D] //todo: get :T|D


object WithDefault {
	implicit def provided[T](implicit evidence :T) :Provided[T] = Provided(evidence)
	implicit def default[D](implicit evidence :D) :Default[D] = Default(evidence)

	final case class Provided[+T](get :T) extends WithDefault[T, Nothing]
	final case class Default[+D](get :D) extends WithDefault[Nothing, D]
}
