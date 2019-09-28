package net.noresttherein.slang

import prettyprint.classNameMethods

import scala.reflect.ClassTag
import scala.util.matching.Regex



package object matching {



	/** Combines two extractor patterns into a logical conjunction which matches only if both of the patterns
	  * match (and binds values to both patterns). Usage: `{ case P1(x) && P2(y) => ??? }`.
	  */
	object && {
		def unapply[T](x :T) :Some[(T, T)] = Some(x, x)
	}



	implicit class matchesMethod[T](val value :T) extends AnyVal {
		def matches[X](expr :PartialFunction[T, X]) :Boolean = expr.isDefinedAt(value)
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

		@inline final override def apply(x: In): Out = unapply(x) getOrElse {
			throw new NoSuchElementException(s"$this($x)")
		}

		override def toString :String = this.shortClassName
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
		def apply[In, Out](f :In=>Option[Out]) :MatchFunction[In, Out] = new OptionFunction(f, s"MatchFunction(${f.localClassName})")


		private class OptionFunction[-In, +Out](f :In=>Option[Out], override val toString :String) extends MatchFunction[In, Out] {
			override def unapply(in: In): Option[Out] = f(in)
		}
	}


	/** An skeleton for extractor objects which can be used both in pattern matching and to directly attempt to narrow
	  * down or extract `Out` out of a `In` type value. It is a single abstract method type (''SAM'') and thus
	  * the compiler will convert a lambda function `In => Out` where the type `Unapply[In, Out]` is expected,
	  * eliminating the overhead of wrapping a function while preserving the convenience of the shortened definition.
	  * It is similar to [[net.noresttherein.slang.matching.MatchFunction MatchFunction]], but its `apply` method
	  * is equivalent to `unapply`, returning an `Option`.
	  * @tparam In type of matched (argument) values
	  * @tparam Out type of extracted (result) values.
	  */
	abstract class Unapply[-In, +Out] {
		def unapply(arg :In) :Option[Out]

		/** Equivalent to `unapply(arg).get` - forces a result value out of the argument failing with an exception instead
		  * of returning `None`.
		  * @throws NoSuchElementException if `unapply` returns `None`.
		  */
		@inline final def get(arg :In) :Out = unapply(arg) getOrElse {
			throw new NoSuchElementException(s"$this.get($arg)")
		}

		/** Same as [[net.noresttherein.slang.matching.Unapply.unapply]]. Directly dispatches this call to the latter. */
		@inline final def apply(arg :In) :Option[Out] = unapply(arg)

		override def toString: String = this.shortClassName
	}


	/** Factory object for [[net.noresttherein.slang.matching.Unapply Unapply]] extractor objects. */
	object Unapply {
		/** Adapt a given option returning function to an extractor object usable in pattern matching. */
		def apply[In, Out](f :In => Option[Out]) :Unapply[In, Out] = new Unapply[In, Out] {
			override def unapply(in :In) = f(in)

			override def toString = s"Unapply(${f.localClassName})"
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
		  * generate compiler warnings unlike direct casts (but just as using the associated [[ClassTag.unapply]] method).
		  * @tparam X type of cast values.
		  * @tparam Y type of argument values after narrowing.
		  * @return an [[Unapply]] instance implementing a filtering of `X` values to those conforming to `X with Y`.
		  */
		def narrow[X, Y :ClassTag] :Unapply[X, Y] = new Unapply[X, Y with X] {
			private[this] val Result :ClassTag[Y] = implicitly[ClassTag[Y]]

			override def unapply(arg :X) :Option[X with Y] = Result.unapply(arg).asInstanceOf[Option[X with Y]]

			override def toString :String = "narrow[_=>" + Result.runtimeClass.getName+"]"
		}
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
	implicit class RegexpGroupMatcher(val sc :StringContext) extends AnyVal {

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
			val res = new StringBuilder((0 /: parts)(_ + _.length))
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