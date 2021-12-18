package net.noresttherein.slang.matching


import scala.reflect.ClassTag

import net.noresttherein.slang.prettyprint.classNameMethods



/** An skeleton for extractor objects which can be used both in pattern matching and to directly attempt to narrow
  * down or extract `Out` out of a `In` type value. It is a single abstract method type (''SAM'') and thus
  * the compiler will convert a lambda function `In => Out` where the type `Unapply[In, Out]` is expected,
  * eliminating the overhead of wrapping a function while preserving the convenience of the shortened definition.
  * It is similar to [[net.noresttherein.slang.matching.MatchFunction MatchFunction]], but its `apply` method
  * is equivalent to `unapply`, returning an `Option`.
  * @tparam In type of matched (argument) values
  * @tparam Out type of extracted (result) values.
  */
@SerialVersionUID(1L)
abstract class MatchPattern[-In, +Out] extends Serializable {
	def unapply(arg :In) :Option[Out]

	/** Equivalent to `unapply(arg).get` - forces a result value out of the argument failing with an exception instead
	  * of returning `None`.
	  * @throws NoSuchElementException if `unapply` returns `None`.
	  */
	@inline final def get(arg :In) :Out = unapply(arg) getOrElse {
		throw new NoSuchElementException(s"$this.get($arg)")
	}

	/** Same as [[net.noresttherein.slang.matching.MatchPattern.unapply unapply]]. Directly dispatches this call
	  * to the latter. */
	@inline final def apply(arg :In) :Option[Out] = unapply(arg)

	override def toString: String = this.abbrevClassName
}



/** Factory object for [[net.noresttherein.slang.matching.MatchPattern Unapply]] extractor objects. */
object MatchPattern {
	/** Adapt a given option returning function to an extractor object usable in pattern matching. */
	def apply[In, Out](f :In => Option[Out]) :MatchPattern[In, Out] = new MatchPattern[In, Out] {
		override def unapply(in :In) = f(in)

		override def toString = s"Unapply(${f.innerClassName})"
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
	  * @return an [[net.noresttherein.slang.matching.MatchPattern Unapply]] instance implementing a filtering of `X` values to those conforming to `X with Y`.
	  */
	def narrow[X, Y :ClassTag] :MatchPattern[X, Y] = new MatchPattern[X, Y with X] {
		private[this] val Result :ClassTag[Y] = implicitly[ClassTag[Y]]

		override def unapply(arg :X) :Option[X with Y] = Result.unapply(arg).asInstanceOf[Option[X with Y]]

		override def toString :String = "narrow[_=>" + Result.runtimeClass.getName + "]"
	}
}



/** Forces a function literal given as the argument
  * to be SAM-converted to [[net.noresttherein.slang.matching.MatchPattern MatchPattern]].
  */
object Unapply {
	/** Forces a function literal given as the argument
	  * to be SAM-converted to [[net.noresttherein.slang.matching.MatchPattern MatchPattern]].
	  */
	@inline def apply[In, Out](pattern :MatchPattern[In, Out]) :MatchPattern[In, Out] = pattern
}






/** A multi-purpose extractor for values of `Out` from parameter `In`. Can be used in pattern matching or
  * as a partial function. It is a single abstract method type (''SAM'') and thus the compiler will convert a lambda
  * function `In => Out` where the type `MatchFunction[In, Out]` is expected, eliminating the overhead of wrapping
  * a function while preserving the convenience of the shortened definition.
  * @tparam In argument type
  * @tparam Out type of extracted value
  */
abstract class MatchFunction[-In, +Out] extends PartialFunction[In, Out] {
	def unapply(in :In) :Option[Out]

	override def isDefinedAt(x: In): Boolean = unapply(x).isDefined

	override def apply(x: In): Out = unapply(x) getOrElse {
		throw new NoSuchElementException(s"$this($x)")
	}

	override def toString :String = this.abbrevClassName
}



/** Companion object for extractors. */
object MatchFunction {
	/** Turn a given function returning an `Option[Out]` for input values `In` into an extractor
	  * that can be used in pattern matching or as a partial function.
	  * @param f function extracting `Out` values from `In` arguments.
	  * @tparam In argument type
	  * @tparam Out extracted result type
	  * @return a partial function extractor wrapping the given function `f`.
	  */
	def apply[In, Out](f :In => Option[Out]) :MatchFunction[In, Out] =
		new OptionFunction(f, s"MatchFunction(${f.innerClassName})")

	private class OptionFunction[-In, +Out](f :In => Option[Out], override val toString :String)
		extends MatchFunction[In, Out]
	{
		override def unapply(in: In): Option[Out] = f(in)
	}
}



/** Adapts a partial function `X => Y` to a [[net.noresttherein.slang.matching.MatchFunction MatchFunction]]. */
object Match {
	/** Adapts a partial function `X => Y` to a [[net.noresttherein.slang.matching.MatchFunction MatchFunction]].
	  * The application is split into two steps in order to provide an explicit argument type parameter first,
	  * with the return type being inferred.
	  * @return an object with method
	  *         [[net.noresttherein.slang.matching.Match.AdaptPartialFunction.apply apply]]`(f :PartialFunction[X, Y])`
	  *         returning a `MatchFunction[X, Y]`.
	  */
	def apply[X] :AdaptPartialFunction[X] = new AdaptPartialFunction[X] {}

	sealed trait AdaptPartialFunction[X] extends Any {
		@inline def apply[Y](f :PartialFunction[X, Y]) :MatchFunction[X, Y] =
			new MatchFunction[X, Y] {
				override val lift = super.lift
				override def unapply(a :X) :Option[Y] = lift(a)
				override def apply(a :X) :Y = f(a)
				override def applyOrElse[A1 <: X, B1 >: Y](x :A1, default :A1 => B1) :B1 = f.applyOrElse(x, default)
				override def isDefinedAt(x :X) :Boolean = f.isDefinedAt(x)
			}
	}
}
