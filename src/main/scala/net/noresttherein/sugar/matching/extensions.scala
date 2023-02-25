package net.noresttherein.sugar.matching

import scala.util.matching.Regex

import net.noresttherein.sugar.extensions.{downcast2TypeParams, saferCasting}
import net.noresttherein.sugar.matching.extensions.{matchesMethod, RegexpGroupMatchPattern}




trait extensions extends Any {
	@inline implicit final def matchesMethod[T](self :T) :matchesMethod[T] = new matchesMethod[T](self)

	@inline implicit final def regexpGroupMatchPattern(self :StringContext) :RegexpGroupMatchPattern =
		new RegexpGroupMatchPattern(self)
//
//	@inline implicit final def partialFunctionFactoryExtension(self :PartialFunction.type)
//			:PartialFunctionFactoryExtension =
//		new PartialFunctionFactoryExtension {}
}




@SerialVersionUID(ver)
object extensions extends extensions {

	class matchesMethod[T](val value :T) extends AnyVal {
		def matches[X](expr :PartialFunction[T, X]) :Boolean = expr.isDefinedAt(value)
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


//	/** Additional extension factory methods for the `PartialFunction` singleton object creating partial functions. */
//	sealed trait PartialFunctionFactoryExtension extends Any {
//		/** Creates a [[PartialFunction]] based on its [[PartialFunction.applyOrElse applyOrElse]] implementation. */
//		final def applyOrElse[X, Y](f :(X, X => Y) => Y) :PartialFunction[X, Y] =
//			new PartialFunction[X, Y] {
//				override def isDefinedAt(x :X) :Boolean =
//					f(x, Fallback.downcastParams[X, Y]).asAnyRef ne Fallback
//
//				override def apply(x :X) :Y = {
//					val res = f(x, Fallback.downcastParams[X, Y])
//					if (res.asAnyRef eq Fallback)
//						throw new MatchError(x)
//					res
//				}
//				override def applyOrElse[A <: X, B >: Y](x :X, default :A => B) :B =
//					f(x, default.castParams[X, Y])
//			}
//	}
//	private val Fallback :Any => Any = _ => Fallback

}