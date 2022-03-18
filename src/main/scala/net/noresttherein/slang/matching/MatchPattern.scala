package net.noresttherein.slang.matching


import scala.reflect.ClassTag
import scala.Specializable.{Arg, Return}

import net.noresttherein.slang.extensions.{optionExtension, saferCasting, downcast2TypeParams, downcastTypeParam}
import net.noresttherein.slang.matching.MatchPattern.SpecializedArgs
import net.noresttherein.slang.prettyprint.classNameMethods
import net.noresttherein.slang.vars.Opt



/** An skeleton for extractor objects which can be used both in pattern matching and to directly attempt to narrow
  * down or extract `Out` out of a `In` type value. It is a single abstract method type (''SAM'') and thus
  * the compiler will convert a lambda function `In => Opt[Out]` where the type `MatchPattern[In, Out]` is expected,
  * eliminating the overhead of wrapping a function while preserving the convenience of the shortened definition.
  * It is similar to [[net.noresttherein.slang.matching.MatchFunction MatchFunction]], but its `apply` method
  * is equivalent to `unapply`, returning an [[net.noresttherein.slang.vars.Opt Opt]].
  * @tparam In type of matched (argument) values
  * @tparam Out type of extracted (result) values.
  * @see [[net.noresttherein.slang.matching.Unapply]]
  * @see [[net.noresttherein.slang.matching.MatchFunction]]
  */
@SerialVersionUID(1L)
trait MatchPattern[@specialized(SpecializedArgs) -In, +Out] extends Serializable {
	/** The single abstract method of this class, possibly extracting a value of `Out` from an `In` as an `Opt`. */
	def unapply(arg :In) :Opt[Out]

	/** Equivalent to `unapply(arg).get` - forces a result value out of the argument failing with an exception instead
	  * of returning `Lack`.
	  * @throws NoSuchElementException if `unapply` returns `Lack`.
	  */
	@inline final def force(arg :In) :Out = unapply(arg) getOrElse {
		throw new NoSuchElementException(s"$this.get($arg)")
	}

	/** Same as [[net.noresttherein.slang.matching.MatchPattern.unapply unapply]]. Directly dispatches this call
	  * to the latter. */
	@inline final def apply(arg :In) :Opt[Out] = unapply(arg)

	/** Converts this pattern to an `Option` returning function. */
	def lift :In => Option[Out] = unapply(_).asOption

	/** Converts this pattern to an equivalent [[net.noresttherein.slang.matching.MatchFunction MatchFunction]]. */
	def partial :MatchFunction[In, Out] = MatchFunction.unlift(lift)

	override def toString: String = this.abbrevClassName
}



/** Factory object for [[net.noresttherein.slang.matching.MatchPattern MatchPattern]] extractor objects.
  * @see [[net.noresttherein.slang.matching.Unapply]]
  */
object MatchPattern {
	/** Adapt a given option returning function to an extractor object usable in pattern matching. */
	def apply[@specialized(SpecializedArgs) In, Out](f :In => Option[Out]) :MatchPattern[In, Out] =
		new MatchPattern[In, Out] {
			override def unapply(in :In) = f(in).toOpt
			override def lift = f
			override def toString = s"MatchPattern(${f.innerClassName})"
		}

	/** An object attempting to cast values of `X` to `Y` and returning values of type `X with Y` if successful.
	  * The difference from a straightforward language-level cast is that the result type is the conjunction of
	  * both the argument and narrowed type. There is currently no way to narrow down an instance of `X` to `X with Y`
	  * without generating compiler warnings or using the `@unchecked` annotation. Note that the cast is done
	  * the only way possible, based on the class tag for `Y` (and indirectly the associated `Class[_]` instance),
	  * which means that only class is available and all information about any type parameters given at instantiation
	  * level to generic types is lost. This means that, just as using the `ClassTag` or language level casts,
	  * a cast from `Seq[Int]` to `List[Short]` will succeed if only the object is an instance of `List`, resulting
	  * in a later failure when the elements are actually accessed. This is obviously '''not''' type safe and doesn't
	  * generate compiler warnings unlike direct casts (but just as using the associated `ClassTag.unapply` method).
	  * @tparam X type of cast values.
	  * @tparam Y type of argument values after narrowing.
	  * @return an [[net.noresttherein.slang.matching.MatchPattern Unapply]] instance implementing a filtering
	  *         of `X` values to those conforming to `X with Y`.
	  */
	def narrow[X, Y :ClassTag] :MatchPattern[X, Y] = new MatchPattern[X, Y with X] {
		private[this] val Result :ClassTag[Y] = implicitly[ClassTag[Y]]

		override def unapply(arg :X) :Opt[X with Y] = Result.unapply(arg).downcastParam[X with Y].toOpt
		override def lift = Result.unapply(_).asInstanceOf[Option[X  with Y]]

		override def toString :String = "narrow[_=>" + Result.runtimeClass.getName + "]"
	}

	final val SpecializedArgs = new Specializable.Group(Byte, Char, Int, Long, Float, Double)
}



/** Forces a function literal given as the argument
  * to be SAM-converted to a [[net.noresttherein.slang.matching.MatchPattern MatchPattern]].
  */
object Unapply {
	/** Forces a function literal given as the argument
	  * to be SAM-converted to a [[net.noresttherein.slang.matching.MatchPattern MatchPattern]].
	  */
	@inline def apply[In, Out](pattern :MatchPattern[In, Out]) :MatchPattern[In, Out] = pattern
}






/** A multi-purpose extractor for values of `Out` from parameter `In`. Can be used in pattern matching or
  * as a partial function. It is a single abstract method type (''SAM'') and thus the compiler will convert a lambda
  * function `In => Out` where the type `MatchFunction[In, Out]` is expected, eliminating the overhead of wrapping
  * a function while preserving the convenience of the shortened definition.
  * @tparam In the argument type.
  * @tparam Out the type of extracted value.
  */
trait MatchFunction[@specialized(Specializable.Arg) -In, @specialized(Specializable.Return) +Out]
	extends PartialFunction[In, Out] with (In => Out) //for specialization
{
	def unapply(in :In) :Option[Out]

	override def isDefinedAt(x: In): Boolean = unapply(x).isDefined

	override def apply(x: In): Out = unapply(x) getOrElse {
		throw new NoSuchElementException(s"$this($x)")
	}

	override def applyOrElse[A1 <: In, B1 >: Out](x :A1, default :A1 => B1) :B1 =
		unapply(x) getOrElse default(x)

	/** This partial function as a `MatchPattern`. */
	def pattern :MatchPattern[In, Out] = Unapply(unapply(_).toOpt)

	override def toString :String = this.abbrevClassName
}



/** Companion object for [[net.noresttherein.slang.matching.MatchFunction! MatchFunction]] extractors,
  * providing several factory methods.
  * @see [[net.noresttherein.slang.matching.Match]]
  */
object MatchFunction {
	/** Turn a given function returning an `Option[Out]` for input values `In` into an extractor
	  * that can be used in pattern matching or as a partial function.
	  * @tparam In  the argument type.
	  * @tparam Out the extracted result type.
	  * @param f function extracting `Out` values from `In` arguments.
	  * @return a partial function extractor wrapping the given function `f`.
	  */
	def unlift[@specialized(Arg) In, @specialized(Return) Out](f :In => Option[Out]) :MatchFunction[In, Out] =
		new OptionFunction(f, s"MatchFunction(${f.innerClassName})")

	/** An equivalent of [[net.noresttherein.slang.matching.MatchFunction.unlift unlift]] for those not into neologisms.
	  * Turns a given function returning an `Option[Out]` for input values `In` into an extractor
	  * that can be used in pattern matching or as a partial function.
	  * @tparam In  the argument type.
	  * @tparam Out the extracted result type.
	  * @param f function extracting `Out` values from `In` arguments.
	  * @return a partial function extractor wrapping the given function `f`.
	  */
	@inline def lower[@specialized(Arg) In, @specialized(Return) Out](f :In => Option[Out]) :MatchFunction[In, Out] =
		unlift(f)


	/** Adapts a partial function `X => Y` to a [[net.noresttherein.slang.matching.MatchFunction MatchFunction]].
	  * The application is split into two steps in order to provide an explicit argument type parameter first,
	  * with the return type being inferred.
	  * @return an object with method
	  *         [[net.noresttherein.slang.matching.Match.AdaptPartialFunction.apply apply]]`(f :PartialFunction[X, Y])`
	  *         returning a `MatchFunction[X, Y]`.
	  */
	@inline def partial[@specialized(Arg) In] :Match.AdaptPartialFunction[In] = Match[In]


	/** Forces a function literal of type `(In, In => Out) => Out` to become an instance
	  * of [[net.noresttherein.slang.matching.MatchFunction MatchFunction]] using it
	  * as its [[net.noresttherein.slang.matching.MatchFunction!.applyOrElse applyOrElse]] method, to which other methods
	  * delegate. Note that, due to a limitation of implementation, the result type `Out` is still boxed, so the benefit
	  * could be seen only in reference types and simple functions.
	  */
	@inline final def applyOrElse[@specialized(Arg) In, Out](applyOrElse :ApplyOrElse[In, Out]) :MatchFunction[In, Out] =
		applyOrElse

	/** A ''single abstract method'' subtype of [[net.noresttherein.slang.matching.MatchFunction MatchFunction]]
	  * requiring from subclasses an implementation of [[PartialFunction]]'s
	  * [[net.noresttherein.slang.matching.MatchFunction.applyOrElse applyOrElse]] method
	  * (as the abstract [[net.noresttherein.slang.matching.MatchFunction.ApplyOrElse.getOrElse getOrElse]]
	  * method). Used primarily in conjunction with
	  * `MatchFunction.`[[net.noresttherein.slang.matching.MatchFunction.partial partial]] to create instances
	  * of `MatchFunction` which do not resort to intermediate boxing to `Option` in that method.
	  */
	trait ApplyOrElse[@specialized(Arg) -In, +Out]
		extends MatchFunction[In, Out]
	{
		protected def getOrElse[A1 <: In, B1 >: Out](x :A1, default :A1 => B1) :B1
		final override def applyOrElse[A1 <: In, B1 >: Out](x :A1, default :A1 => B1) :B1 =
			getOrElse(x, default)

		final override def unapply(a :In) :Option[Out] = {
			val z = getOrElse(a, Fallback.downcastParams[In, Out])
			if (z.asAnyRef eq Fallback) Some(z) else None
		}
	}
	private[this] final val Fallback :Any => Any = _ => Fallback

	private class OptionFunction[@specialized(Specializable.Arg) -In, @specialized(Specializable.Return) +Out]
	                            (f :In => Option[Out], override val toString :String)
		extends MatchFunction[In, Out]
	{
		override def lift :In => Option[Out] = f
		override def unapply(in: In): Option[Out] = f(in)
		override def pattern :MatchPattern[In, Out] = MatchPattern(f)
	}
}



/** Adapts a partial function `X => Y` to a [[net.noresttherein.slang.matching.MatchFunction MatchFunction]].
  * @see [[net.noresttherein.slang.matching.Match.apply]]
  */
object Match {
	/** Adapts a partial function `X => Y` to a [[net.noresttherein.slang.matching.MatchFunction MatchFunction]].
	  * The application is split into two steps in order to provide an explicit argument type parameter first,
	  * with the return type being inferred.
	  * @return an object with method
	  *         [[net.noresttherein.slang.matching.Match.AdaptPartialFunction.apply apply]]`(f :PartialFunction[X, Y])`
	  *         returning a `MatchFunction[X, Y]`.
	  */
	def apply[@specialized(Specializable.Arg) X] :AdaptPartialFunction[X] = new AdaptPartialFunction[X] {}

	sealed trait AdaptPartialFunction[@specialized(Specializable.Arg) X] extends Any {
		@inline def apply[@specialized(Specializable.Return) Y](f :PartialFunction[X, Y]) :MatchFunction[X, Y] =
			new MatchFunction[X, Y] {
				override val lift = f.lift
				override def unapply(a :X) :Option[Y] = lift(a)
				override def apply(a :X) :Y = f(a)
				override def applyOrElse[A1 <: X, B1 >: Y](x :A1, default :A1 => B1) :B1 = f.applyOrElse(x, default)
				override def isDefinedAt(x :X) :Boolean = f.isDefinedAt(x)
				override def pattern = MatchPattern(lift)
			}
	}
}
