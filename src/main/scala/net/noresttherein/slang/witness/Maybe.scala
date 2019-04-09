package net.noresttherein.slang.witness

/** A type for which an implicit value is always present, however, if an implicit value for `T` can be found,
  * it is exposed as `Some[T]` through this instances [[net.noresttherein.slang.witness.Maybe#opt opt]] method.
  */
class Maybe[+T] private[witness] (val opt :Option[T])


sealed abstract class MaybeNoImplicit {
	implicit val maybeNone :Maybe[Nothing] = new Maybe(None)
}

/** Provides optional implicit values if they are available wrapped as `Maybe[T]`. */
object Maybe extends MaybeNoImplicit {
	implicit def maybeYes[T](implicit e :T) :Maybe[T] = new Maybe(Some(e))
}