package net.noresttherein.sugar.matching

import scala.Specializable.{Arg, Return}
import scala.util.matching.Regex

import net.noresttherein.sugar.extensions.{cast2TypeParams, classNameMethods, downcast2TypeParams, downcastTypeParam, saferCasting}
import net.noresttherein.sugar.matching.extensions.{matchesMethod, FunctionExtension, OptFunctionExtension, PartialFunctionExtension, PartialFunctionFactoryExtension, RegexpGroupMatchPattern}
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




trait extensions extends Any {
	@inline implicit final def matchesMethod[T](self :T) :matchesMethod[T] = new matchesMethod[T](self)

	@inline implicit final def regexpGroupMatchPattern(self :StringContext) :RegexpGroupMatchPattern =
		new RegexpGroupMatchPattern(self)

	@inline implicit final def functionExtension[X, Y](f :X => Y) :FunctionExtension[X, Y] =
		new FunctionExtension(f)

	@inline implicit final def optFunctionExtension[X, Y](f :X => Opt[Y]) :OptFunctionExtension[X, Y] =
		new OptFunctionExtension(f)

	@inline implicit final def partialFunctionExtension[X, Y](f :PartialFunction[X, Y]) :PartialFunctionExtension[X, Y] =
		new PartialFunctionExtension(f)

	@inline implicit final def partialFunctionFactoryExtension(pf :PartialFunction.type) :PartialFunctionFactoryExtension =
		new PartialFunctionFactoryExtension {}

//	@inline implicit final def partialFunctionFactoryExtension(self :PartialFunction.type)
//			:PartialFunctionFactoryExtension =
//		new PartialFunctionFactoryExtension {}
}




@SerialVersionUID(Ver)
object extensions extends extensions {

	class matchesMethod[T](val value :T) extends AnyVal {
		def matches[X](expr :PartialFunction[T, X]) :Boolean = expr.isDefinedAt(value)
	}


	class FunctionExtension[X, Y] private[matching] (private val self :X => Y) extends AnyVal {
		/** Adapts this function to a `PartialFunction` which always returns `true` from `isDefinedAt`. */
		def toPartialFunction :PartialFunction[X, Y] = new FunctionAsPartialFunction(self)
	}

	class OptFunctionExtension[X, Y] private[matching] (private val self :X => Opt[Y]) extends AnyVal {
		/** Turn a given function returning an `Option[Out]` for input values `In` into an extractor
		  * that can be used in pattern matching or as a partial function.
		  * @return a partial function extractor wrapping this function.
		  */
		def unlift :PartialFunction[X, Y] = new OptFunction(self, "<unlifted function>")
	}

	class PartialFunctionExtension[X, Y](private val self :PartialFunction[X, Y]) extends AnyVal {
		/** Same as `PartialFunction.`[[PartialFunction.unapply unapply]], but does not box. */
		def get(x :X) :Opt[Y] = {
			val y = self.applyOrElse(x, Fallback.downcastParams[X, Y])
			if (y.asAnyRef eq Fallback) Lack else Got(y)
		}
	}



	/** Extension factory method `applyOrElse` for `PartialFunction` object. */
	sealed trait PartialFunctionFactoryExtension extends Any {
		@inline def identity[X] :PartialFunction[X, X] = Identity.downcastParam[X]

		/** Forces a function literal of type `(In, In => Out) => Out` to become an instance
		  * of [[PartialFunction]] using it as its `applyOrElse` method, to which other methods
		  * delegate.
		  */
		@inline final def apply[In, Out](applyOrElse :ApplyOrElse[In, Out]) :PartialFunction[In, Out] =
			applyOrElse

		/** Creates a [[PartialFunction]] based on its [[PartialFunction.applyOrElse applyOrElse]] implementation. */
		final def applyOrElse[X, Y](f :(X, X => Y) => Y) :PartialFunction[X, Y] =
			new PartialFunction[X, Y] {
				override def isDefinedAt(x :X) :Boolean =
					f(x, Fallback.downcastParams[X, Y]).asAnyRef ne Fallback

				override def apply(x :X) :Y = {
					val res = f(x, Fallback.downcastParams[X, Y])
					if (res.asAnyRef eq Fallback)
						throw new MatchError(x)
					res
				}

				override def applyOrElse[A1 <: X, B1 >: Y](x :A1, default :A1 => B1) =
					f(x, default.castParams[X, Y])
			}


		/** Turn a given function returning an `Option[Out]` for input values `In` into an extractor
		  * that can be used in pattern matching or as a partial function.
		  * @tparam In  the argument type.
		  * @tparam Out the extracted result type.
		  * @param name a textual identifier used in `toString` method of the created `MatchFunction`.
		  * @param f    a function extracting `Out` values from `In` arguments.
		  * @return a partial function extractor wrapping the given function `f`.
		  */
		def unlift[@specialized(Arg) In, Out]
		         (name :String)(f :In => Opt[Out]) :PartialFunction[In, Out] =
			new OptFunction(f, name)

		/** Turn a given function returning an `Option[Out]` for input values `In` into an extractor
		  * that can be used in pattern matching or as a partial function.
		  * @tparam In  the argument type.
		  * @tparam Out the extracted result type.
		  * @param f    a function extracting `Out` values from `In` arguments.
		  * @return a partial function extractor wrapping the given function `f`.
		  */
		def unlift[@specialized(Arg) In, Out](f :In => Opt[Out]) :PartialFunction[In, Out] =
			new OptFunction(f, "<unlifted function>")
	}


	private[extensions] trait ComposablePartialFunction[-In, +Out] extends PartialFunction[In, Out] {
		override def orElse[A1 <: In, B1 >: Out](that :PartialFunction[A1, B1]) :PartialFunction[A1, B1] =
			PartialFunction[A1, B1] {
				(x :A1, default :A1 => B1) =>
					val out = applyOrElse(x, Fallback.downcastParams[In, Out])
					if (out.asAnyRef ne Fallback) out else that.applyOrElse(x, default)
			}
		override def andThen[C](k :PartialFunction[Out, C]) :PartialFunction[In, C] =
			if (k eq Identity)
				k compose this
			else
				PartialFunction[In, C] {
					(x :In, default :In => C) =>
						val out = applyOrElse(x, Fallback.downcastParams[In, Out])
						if (out.asAnyRef eq Fallback)
							default(x)
						else
							k(out)
				}
		override def compose[R](k :PartialFunction[R, In]) :PartialFunction[R, Out] =
			if (k eq Identity)
				k andThen this
			else
				PartialFunction[R, Out] {
					(x :R, default :R => Out) =>
						val out = applyOrElse(k(x), Fallback.downcastParams[In, Out])
						if (out.asAnyRef ne Fallback) out else default(x)
				}
	}

	/** A ''single abstract method'' subtype of [[PartialFunction]]
	  * requiring from subclasses an implementation of [[PartialFunction.applyOrElse applyOrElse]] method
	  * (as abstract [[net.noresttherein.sugar.matching.extensions.ApplyOrElse.getOrElse getOrElse]]
	  * method). Used primarily in conjunction with
	  * `PartialFunction.`[[net.noresttherein.sugar.matching.extensions.PartialFunctionFactoryExtension.apply apply]]
	  * to create instances of `PartialFunction` which do not resort to intermediate boxing to `Option` in that method.
	  */
	trait ApplyOrElse[In, Out] extends PartialFunction[In, Out] with ComposablePartialFunction[In, Out] {
		def getOrElse(x :In, default :In => Out) :Out

		final override def applyOrElse[A1 <: In, B1 >: Out](x :A1, default :A1 => B1) :B1 =
			getOrElse(x, default.asInstanceOf[In => Out])

		final override def apply(v1 :In) :Out = {
			val z = getOrElse(v1, Fallback.downcastParams[In, Out])
			if (z.asAnyRef eq Fallback)
				throw new MatchError(z)
			z
		}

		final override def isDefinedAt(x :In) :Boolean = {
			val z = getOrElse(x, Fallback.downcastParams[In, Out])
			z.asAnyRef ne Fallback
		}
	}

	private[this] object Fallback extends (Any => Any) {
		override def apply(v1 :Any) :Any = Fallback
	}
	private[this] object Identity extends IdentityPartialFunction[Any]

	private class IdentityPartialFunction[X] extends PartialFunction[X, X] {
		override def isDefinedAt(x :X) :Boolean = true
		override def apply(v1 :X) :X = v1
		override def applyOrElse[A1 <: X, B1 >: X](x :A1, default :A1 => B1) :B1 = x

		override def orElse[A1 <: X, B1 >: X](that :PartialFunction[A1, B1]) = this
//		override def andThen[C](k :X => C) = k match {
//			case partial :PartialFunction[X, C] => partial
//			case _ => super.andThen(k)
//		}
		override def andThen[C](k :PartialFunction[X, C]) = k
		override def compose[R](k :PartialFunction[R, X]) = k
//		override def compose[A](g :A => X) = super.compose(g)

		override def toString = "identity"
	}

	private class FunctionAsPartialFunction[X, Y](f :X => Y) extends ComposablePartialFunction[X, Y] {
		override def isDefinedAt(x :X) :Boolean = true
		override def apply(v1 :X) :Y = f(v1)
		override def applyOrElse[A1 <: X, B1 >: Y](x :A1, default :A1 => B1) :B1 = f(x)

		override def orElse[A1 <: X, B1 >: Y](that :PartialFunction[A1, B1]) :PartialFunction[A1, B1] = this

		override def andThen[C](k :PartialFunction[Y, C]) :PartialFunction[X, C] =
			if (k eq Identity)
				k compose this
			else
				PartialFunction[X, C] {
					(x :X, default :X => C) =>
						val c = k.applyOrElse(f(x), Fallback.downcastParams[Y, C])
						if (c.asAnyRef ne Fallback) c else default(x)
				}

		override def compose[R](k :PartialFunction[R, X]) :PartialFunction[R, Y] =
			if (k eq Identity)
				k andThen this
			else
				PartialFunction[R, Y] { (arg :R, default :R => Y) =>
					val x = k.applyOrElse(arg, Fallback.downcastParams[R, X])
					if (x.asAnyRef eq Fallback) default(arg) else f(x)
				}

		override def toString = f.toString
	}

	@SerialVersionUID(Ver)
	private final class OptFunction[@specialized(Specializable.Arg) -In, @specialized(Specializable.Return) +Out]
	                               (f :In => Opt[Out], override val toString :String)
		extends ComposablePartialFunction[In, Out]
	{
		override def isDefinedAt(x :In) :Boolean = f(x).isDefined

		override def apply(v1 :In) :Out = f(v1) getOrElse { throw new MatchError(v1) }

		override def applyOrElse[A1 <: In, B1 >: Out](x :A1, default :A1 => B1) :B1 =
			f(x) getOrElse default(x)
	}



	/** This implicit extension produces group-capturing extractor for use in pattern matching using string interpolation.
	  * It can be used in the following way:
	  * {{{
	  *     email match {
	  *         case r"($user[^@]+)@($domain.+)" => println s"$user at $domain"
	  *         case _ => println "not an email!"
	  *     }
	  * }}}
	  * This will work also with arbitrary match patterns accepting `String` values as the argument to their
	  * `unapply`/`unapplySeq` method: `r"email: (${Email(user, domain)}[^@]+@.+)"`. You can escape the '$' character
	  * by doubling it.
	  * @see http://hootenannylas.blogspot.com.au/2013/02/pattern-matching-with-string.html
	  *      for an explanation of string interpolation in pattern matching
	  */
	class RegexpGroupMatchPattern(private val sc :StringContext) extends AnyVal {

		/** Turns the following string with a regular expression into a pattern for use with scala pattern matching.
		  * Any capturing group inside the regular expression can start with an interpolated scala expression which forms
		  * a valid match pattern - a simple identifier bound to the value of the matched fragment, a type-bound expression
		  * or even arbitrary extractors (objects with a `unapply` or `unapplySeq` method). The capturing groups,
		  * aside from the interpolated scala expression, must additionally contain the regular expression which should
		  * be matched/bound to the scala expression. So, having `r"email: (${Email(email)})"` is not enough; the pattern
		  * should be `r"email: (${Email(email)}$EmailRX)"` (where EmailRX is a string constant with the appropriate
		  * regular expression). Any groups ('('-')' pairs) in the string which do not start with an interpolated
		  * scala expression are turned into non-capturing groups. Note that while interpolated expressions are allowed
		  * anywhere in the string, those not immediately following a '(' character should be valid constant patterns,
		  * that is, identifiers for stable values (scala `val`'s) starting with an upper case letter. Embedding a binding
		  * pattern anywhere else will cause the match to always fail due to invalid number of groups.
		  * This method does not perform standard Java string special character substitution other than unicode literals;
		  * instead the escape sequences (characters preceded by '\') are treated as per the Java regular expression
		  * specification. This means however that any regexp escape sequence requires only a single '\' character
		  * instead of two as when used in Java: write simply "\b" instead of "\\b".
		  */
		def r = new Regex(toString)

		override def toString :String = {
			var parts = sc.parts
			val res = new StringBuilder((0 /: parts) (_ + _.length))
			var groupStart = false //last character was an unescaped '('
			var escaped = false //the next character is escaped with a '\' to prevent interpreting by the regexp
			while (parts.nonEmpty) {
				val fragment = parts.head
				val len = fragment.length
				parts = parts.tail
				var i = 0
				while (i < len) {
					val char = fragment charAt i
					if (groupStart && char != '?') //change the just opened group into a non-capturing group
						res append '?'
					res append char
					i += 1
					char match {
						case '\\' =>
							escaped = !escaped
							groupStart = false
						case '(' if !escaped =>
							groupStart = true
						case _ =>
							escaped = false
							groupStart = false
					}
				}
			}
			res.toString
		}

	}

}
