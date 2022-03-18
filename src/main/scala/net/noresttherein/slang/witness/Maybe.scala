package net.noresttherein.slang.witness

import net.noresttherein.slang.extensions.saferCasting
import net.noresttherein.slang.vars.Opt
import net.noresttherein.slang.vars.Opt.{Got, Lack}
import net.noresttherein.slang.witness.Maybe.NoContent


/** A type for which an implicit value is always present, however, if an implicit value for `T` can be found,
  * it is exposed as `Got[T]` through this instances [[net.noresttherein.slang.witness.Maybe.opt opt]] method.
  */
class Maybe[+T] private[witness] (private val content :AnyRef) extends AnyVal {
	def opt :Opt[T] = if (content == NoContent) Lack else Got(content.asInstanceOf[T])

	@inline def getOrElse[O >: T](alternative: => O) :O  = content match {
		case NoContent => alternative
		case o :O @unchecked => o
	}
}



private[witness] sealed abstract class MaybeNoImplicit {
	private val no = new Maybe[Nothing](NoContent)
	implicit final def maybeNo[T] :Maybe[T] = no
}



/** Provides optional implicit values if they are available wrapped as `Maybe[T]`. */
object Maybe extends MaybeNoImplicit {
	def apply[T](implicit maybe :Maybe[T]) :Opt[T] = maybe.opt

	def unapply[T](maybe :Maybe[T]) :Opt[T] = maybe.opt


	final val none = new Maybe[Nothing](NoContent)

	def some[T](implicit evidence :T) :Maybe[T] = new Maybe(evidence.asAnyRef)


	implicit def maybeYes[T](implicit e :T) :Maybe[T] = new Maybe(e.asAnyRef)

	implicit def explicit[T](value :T) :Maybe[T] = new Maybe(value.asAnyRef)

	private[witness] case object NoContent
}




sealed trait WithDefault[+T, +D] //todo: get :T|D


object WithDefault {
	implicit def provided[T](implicit evidence :T) :Provided[T] = Provided(evidence)
	implicit def default[D](implicit evidence :D) :Default[D] = Default(evidence)

	final case class Provided[+T](get :T) extends WithDefault[T, Nothing]
	final case class Default[+D](get :D) extends WithDefault[Nothing, D]
}
