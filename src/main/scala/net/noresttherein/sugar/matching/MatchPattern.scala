package net.noresttherein.sugar.matching


import scala.reflect.ClassTag

import net.noresttherein.sugar.extensions.{OptionExtension, castingMethods, downcastTypeParamMethods}
import net.noresttherein.sugar.matching.MatchPattern.SpecializedArgs
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}

//implicits
import net.noresttherein.sugar.extensions.classNameMethods




/** An skeleton for extractor objects which can be used both in pattern matching and to directly attempt to narrow
  * down or extract `Out` out of a `In` type value. It is a single abstract method type (''SAM'') and thus
  * the compiler will convert a lambda function `In => Opt[Out]` where the type `MatchPattern[In, Out]` is expected,
  * eliminating the overhead of wrapping a function while preserving the convenience of the shortened definition.
  * It is similar to [[PartialFunction]], but its `apply` method is equivalent to `unapply`,
  * goth returning an [[net.noresttherein.sugar.vars.Opt Opt]].
  * @tparam In type of matched (argument) values
  * @tparam Out type of extracted (result) values.
  */
@SerialVersionUID(Ver)
trait MatchPattern[@specialized(SpecializedArgs) -In, +Out] extends Serializable {
	/** The single abstract method of this class, possibly extracting a value of `Out` from an `In` as an `Opt`. */
	def unapply(arg :In) :Opt[Out]

	/** Equivalent to `unapply(arg).get` - forces a result value out of the argument failing with an exception instead
	  * of returning `Lack`.
	  * @throws NoSuchElementException if `unapply` returns `Lack`.
	  */
	@inline final def force(arg :In) :Out = unapply(arg) orNoSuch s"$this.force($arg)"

	/** Same as [[net.noresttherein.sugar.matching.MatchPattern.unapply unapply]]. Directly dispatches this call
	  * to the latter. */
	@inline final def apply(arg :In) :Opt[Out] = unapply(arg)

	/** Converts this pattern to an `Option` returning function. */
	def lift :In => Option[Out] = unapply(_).option

	/** Converts this pattern to an equivalent `PartialFunction`. */
	def partial :PartialFunction[In, Out] = new PartialFunction[In, Out] {
		override def isDefinedAt(x :In) :Boolean = MatchPattern.this(x).isDefined

		override def apply(v1 :In) :Out =
			MatchPattern.this(v1) getOrElse { throw new MatchError(s"${MatchPattern.this}($v1).get") }

		override def applyOrElse[A1 <: In, B1 >: Out](x :A1, default :A1 => B1) :B1 =
			MatchPattern.this(x) getOrElse default(x)
	}

	/** Composes this pattern with another one in a way analogous to `Function.andThen`. */
	def andThen[O](pattern :MatchPattern[Out, O]) :MatchPattern[In, O] =
		new MatchPattern[In, O] {
			override def unapply(arg :In) :Opt[O] = MatchPattern.this.unapply(arg).flatMap(pattern.unapply)
			override def toString = pattern.toString + "*" + MatchPattern.this
		}

	/** Composes this pattern with another one in a way analogous to `Function.compose`. */
	def compose[O](pattern :MatchPattern[O, In]) :MatchPattern[O, Out] =
		pattern andThen this

	override def toString: String = this.abbrevClassName
}




/** Factory object for [[net.noresttherein.sugar.matching.MatchPattern MatchPattern]] extractor objects.
  */
@SerialVersionUID(Ver)
object MatchPattern { //todo: a constructor macro adapting a partial function literal
	/** Forces a function literal given as the argument
	  * to be SAM-converted to a [[net.noresttherein.sugar.matching.MatchPattern MatchPattern]].
	  */
	@inline def apply[In, Out](pattern :MatchPattern[In, Out]) :MatchPattern[In, Out] = pattern

	/** Adapt a given option returning function to an extractor object usable in pattern matching.
	  * @param name a textual identifier of the extractor, used in its `toString` implementation.
	  * @param f    an extractor function.
	  */
	def adapt[@specialized(SpecializedArgs) In, Out](name :String)(f :In => Opt[Out]) :MatchPattern[In, Out] =
		new MatchPattern[In, Out] {
			override def unapply(in :In) = f(in)
			override def lift = f andThen (_.toOption)
			override def toString = name
		}

	/** Adapt a given option returning function to an extractor object usable in pattern matching. */
	def adapt[@specialized(SpecializedArgs) In, Out](f :In => Opt[Out]) :MatchPattern[In, Out] =
		MatchPattern.adapt(s"MatchPattern(${f.innerClassName})")(f)

	/** Adapt a given partial function to an extractor object usable in pattern matching.
	  * @param name a textual identifier of the extractor, used in its `toString` implementation.
	  * @param f    an extractor function.
	  */
	def when[@specialized(SpecializedArgs) In, Out](name :String)(f :PartialFunction[In, Out]) :MatchPattern[In, Out] =
		new MatchPattern[In, Out] {
			override def unapply(in :In) = {
				val res = f.applyOrElse(in, fallback)
				if (res.asAnyRef eq fallback) Lack
				else Got(res.asInstanceOf[Out])
			}
			override def lift = f.lift
			override def toString = "name"
		}

	/** Adapt a given partial function to an extractor object usable in pattern matching. */
	def when[@specialized(SpecializedArgs) In, Out](f :PartialFunction[In, Out]) :MatchPattern[In, Out] =
		when(s"MatchPattern(${f.innerClassName})")(f)

	/** Creates a `MatchPattern` which always succeeds based on the given getter function.
	  * @param name a textual identifier of the extractor, used in its `toString` implementation.
	  * @param f    a getter function.
	  */
	def sure[@specialized(SpecializedArgs) In, Out](name :String)(f :In => Out) :MatchPattern[In, Out] =
		new MatchPattern[In, Out] {
			override def unapply(in :In) = Got(f(in))
			override def lift = f andThen Some.apply
			override def toString = name
		}

	/** Creates a `MatchPattern` which always succeeds based on the given getter function. */
	def sure[@specialized(SpecializedArgs) In, Out](f :In => Out) :MatchPattern[In, Out] =
		MatchPattern.sure(s"MatchPattern.sure(${f.innerClassName})")(f)

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
	  * @return an [[net.noresttherein.sugar.matching.MatchPattern Unapply]] instance implementing a filtering
	  *         of `X` values to those conforming to `X with Y`.
	  */
	def narrow[X, Y :ClassTag] :MatchPattern[X, Y] = new MatchPattern[X, Y with X] {
		private[this] val Result :ClassTag[Y] = implicitly[ClassTag[Y]]

		override def unapply(arg :X) :Opt[X with Y] = Result.unapply(arg).downcastParam[X with Y].toOpt
		override def lift = Result.unapply(_).asInstanceOf[Option[X  with Y]]

		override def toString :String = "narrow[_=>" + Result.runtimeClass.getName + "]"
	}

	final val SpecializedArgs = new Specializable.Group(Byte, Char, Int, Long, Float, Double)

	final val Bool = BooleanMatchPattern

	private[this] val fallback :Any => Any = _ => fallback
}




/** An skeleton for matching pattern objects which can be also used directly to verify if the argument
  * matches this pattern. It is a single abstract method type (''SAM'') and thus
  * the compiler will convert a lambda function `In => Boolean` where the type `BooleanMatchPattern[In]` is expected,
  * eliminating the overhead of wrapping a function while preserving the convenience of the shortened definition.
  * @tparam In type of matched (argument) values
  */
@SerialVersionUID(Ver)
trait BooleanMatchPattern[@specialized(SpecializedArgs) -In] extends Serializable {
	/** The single abstract method of this class, checking if the argument matches this pattern. */
	def unapply(arg :In) :Boolean

	/** Same as [[net.noresttherein.sugar.matching.MatchPattern.unapply unapply]]. Directly dispatches this call
	  * to the latter. */
	@inline final def apply(arg :In) :Boolean = unapply(arg)

	override def toString: String = this.abbrevClassName
}




/** Factory object for [[net.noresttherein.sugar.matching.BooleanMatchPattern BooleanMatchPattern]] extractor objects.
  */
@SerialVersionUID(Ver)
object BooleanMatchPattern {
	/** Forces a function literal given as the argument
	  * to be SAM-converted to a [[net.noresttherein.sugar.matching.BooleanMatchPattern BooleanMatchPattern]].
	  */
	@inline def apply[In](pattern :BooleanMatchPattern[In]) :BooleanMatchPattern[In] = pattern

	/** Adapt a given option returning function to an extractor object usable in pattern matching.
	  * @param name a textual identifier of the extractor, used in its `toString` implementation.
	  * @param f    an extractor function.
	  */
	def adapt[@specialized(SpecializedArgs) In](name :String)(f :In => Boolean) :BooleanMatchPattern[In] =
		new BooleanMatchPattern[In] {
			override def unapply(in :In) = f(in)
			override def toString = name
		}

	/** Adapt a given option returning function to an extractor object usable in pattern matching. */
	def adapt[@specialized(SpecializedArgs) In](f :In => Boolean) :BooleanMatchPattern[In] =
		adapt(s"BooleanMatchPattern(${f.innerClassName})")(f)

	/** Adapt a given partial function to an extractor object usable in pattern matching.
	  * The pattern will match if the function is both defined for the matched object, and returns `true`.
	  * @param name a textual identifier of the extractor, used in its `toString` implementation.
	  * @param f    an extractor function.
	  */
	def when[@specialized(SpecializedArgs) In](name :String)(f :PartialFunction[In, Boolean]) :BooleanMatchPattern[In] =
		new BooleanMatchPattern[In] {
			override def unapply(in :In) = f.applyOrElse(in, (_ :In) => false)
			override def toString = "name"
		}

	/** Adapt a given partial function to an extractor object usable in pattern matching.
	  * The pattern will match if the function is both defined for the matched object, and returns `true`.
	  */
	def when[@specialized(SpecializedArgs) In](f :PartialFunction[In, Boolean]) :BooleanMatchPattern[In] =
		when(s"BooleanMatchPattern(${ f.innerClassName })")(f)

}
