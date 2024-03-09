package net.noresttherein.sugar.witness

import net.noresttherein.sugar.vars.{Maybe, Opt, Pill}
import net.noresttherein.sugar.vars.Maybe.{No, Yes}
import net.noresttherein.sugar.vars.Opt.One
import net.noresttherein.sugar.vars.Pill.{Blue, Red}
import net.noresttherein.sugar.witness
import net.noresttherein.sugar.witness.WithDefault.Default




/** A type for which an implicit value is always present, however, if an implicit value for `T` can be found,
  * it is exposed as `Yes[T]` through this instances [[net.noresttherein.sugar.witness.Optionally.maybe opt]] method.
  * @note there is an implicit conversion from `T` to `Optionally[T]`, so values of `T` can be passed explicitly
  *       as arguments to methods expecting an `Optionally[T]`.
  */
class Optionally[+T] private[witness](val opt :Opt[T]) extends AnyVal {
	def maybe :Maybe[T] = opt.maybe

	/** Returns the content of this maybe or a default alternative if not content is available. */
	@inline def getOrElse[O >: T](alternative: => O) :O = maybe match {
		case Yes(o :O @unchecked) => o
		case _                    => alternative
	}

	@inline def isDefined :Boolean = opt.isDefined

	override def toString :String = opt.toString
}



private[witness] sealed abstract class OptionallyNoneImplicit {
	@inline implicit final def optionallyNone[T] :Optionally[T] = Optionally.none
}



/** Provides optional implicit values if they are available wrapped as `Optionally[T]`. */
@SerialVersionUID(Ver)
object Optionally extends OptionallyNoneImplicit {
	/** A wrapper type for a type constructor `F` allowing to use a `Optionally` for a generic type as a type class
	  * (a ''view bound'') using syntax
	  * {{{
	  *     trait CanSing[T] { def sing(title :String) :Unit }
	  *
	  *     def method[X :TypeClass[CanSing]#Optionally](x :X) =
	  *         Optionally[CanSing[X]].opt.foreach { _.sing("Drunken sailor") }
	  * }}}
	  */ //todo: rename to something shorter.
	type TypeClass[F[_]] = {
//		/** A type constructor for [[net.noresttherein.sugar.witness.Optionally Optionally]]`[F[T]]`,
//		  * where `F` is a type class/type constructor specified as the preceding argument to `TypeCass`.
//		  */
		type Optionally[X] = witness.Optionally[F[X]]
	}

	/** A shorter alternative for [[net.noresttherein.sugar.witness.Optionally.TypeClass TypeClass]]`[F]` composing
	  * [[net.noresttherein.sugar.witness.Optionally! Optionally]] with a specified a type constructor
	  * by defining a member type constructor `T` applying said type constructor to its type argument
	  * as a `Optionally[F[X]]`. In this way, `Optionally.a[F]#T` is a view bound for any view bound `F`.
	  */
	type a[F[_]] = { type T[X] = Optionally[F[X]] }

	/** Summons an optional implicit `T` instance as an `Opt[T]` instance. */
	@inline def apply[T](implicit maybe :Optionally[T]) :Opt[T] = maybe.opt

	/** Returns an implicit instance o `T` or a default alternative provided as an argument.  */
	@inline def orElse[T](alternative: => T)(implicit maybe :Optionally[T]) :T = maybe.maybe match {
		case Yes(value) => value
		case _ => alternative
	}

	/** Extracts the optional content of a `Optionally[T]`. This pattern will match only 'yes' instances. */
	def unapply[T](maybe :Optionally[T]) :Maybe[T] = maybe.maybe

	/** An empty `Optionally` conforming to any `Optionally[TT]` type. */
	final val none = new Optionally[Nothing](None)

	/** Explicitly wraps an implicit evidence instance o `T` in a `Optionally`. */
	def some[T](implicit evidence :T) :Optionally[T] = new Optionally(One(evidence))


	@inline implicit def optionallySome[T](implicit e :T) :Optionally[T] = new Optionally(One(e))

	/** An implicit conversion from any value of type `T` to a `Optionally[T]`. */
	implicit def anyToOptionally[T](value :T) :Optionally[T] = new Optionally(One(value))
}




/** An 'orElse' implicit value, wrapping a value of `T`
  * as [[net.noresttherein.sugar.witness.WithDefault.Preferred Preferred]]`[T]` if an implicit one is available,
  * or an implicit `D` as [[net.noresttherein.sugar.witness.WithDefault.Default Default]]`[D]` otherwise.
  */
class WithDefault[+T, +D] private (val toPill :Pill[D, T]) extends AnyVal //todo: get :T|D


private[witness] sealed abstract class WithDefaultEvidence {
	implicit final def default[D](implicit evidence :D) :Default[D] = Default(evidence)
	@inline implicit final def withDefault[D](default :D) :Default[D] = Default(default)
}

object WithDefault extends WithDefaultEvidence {
	implicit def preferred[T](implicit evidence :T) :Preferred[T] = Preferred(evidence)

	@inline implicit def anyPreferred[T](value :T) :Preferred[T] = Preferred(value)

	type Preferred[+T] = WithDefault[T, Nothing]
	type Default[+T] = WithDefault[Nothing, T]

	object Preferred {
		def apply[T](value :T) :Preferred[T] = new WithDefault(Blue(value))
		def unapply[T](withDefault :WithDefault[T, Any]) :Maybe[T] = withDefault.toPill.toMaybe
	}
	object Default {
		def apply[T](value :T) :Default[T] = new WithDefault(Red(value))
		def unapply[T](withDefault :WithDefault[Any, T]) :Maybe[T] = withDefault.toPill match {
			case Red(default) => Yes(default)
			case _            => No
		}
	}
}
