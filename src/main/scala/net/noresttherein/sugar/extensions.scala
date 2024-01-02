package net.noresttherein.sugar

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION

import net.noresttherein.sugar.extensions.{feedToMethod, notNullMethod}
import net.noresttherein.sugar.typist.casting
import net.noresttherein.sugar.typist.Rank.Rank1




/** Type aliases and forwarders for most useful types and implicit conversions in the library providing new syntax.
  * They can be imported into user code either by an explicit import of the contents of the companion object
  * [[net.noresttherein.sugar.extensions$ extensions]] (which extends this trait), or by having a package object
  * containing classes using these features extend this trait. This is of course true also for any other object or class.
  * As a mnemonic, class and method members here are (re)named after the most prominent declared method.
  * @author Marcin Mościcki marcin@moscicki.net
  */ //consider: having it simply extend the traits for individual packages.
trait extensions extends Any
	with arrays.extensions with casting.extensions with collections.extensions with exceptions.extensions
	with funny.extensions with matching.extensions with numeric.extensions with optional.extensions
	with reflect.extensions with repeat.extensions with slang.extensions with time.extensions[Rank1]
	with tuples.extensions with typist.extensions with witness.extensions
{
	/** Adds a `feedTo` method to any value which applies a given function to `this`. */
	@inline implicit final def feedToMethod[X](x :X) :feedToMethod[X] = new feedToMethod(x)

	/** Adds a [[net.noresttherein.sugar.extensions.notNullMethod.notNull notNull]]`(msg: String)` method to any value. */
	@inline implicit final def notNullMethod[X](x :X) :notNullMethod[X] = new notNullMethod(x)
}




/** (Almost) all extension methods and implicit conversions providing new syntax in this library .
  * Grouping them here not only allows a wildcard import, but also makes single imports stand out due to object name.
  *
  * As a mnemonic, class and method members here are named either
  *   - `xxxExtension`, where `Xxx` is the name of the enriched type, if it is they are not general purpose methods,
  *     but work on specific types such as [[Option]] or [[Iterable]], or
  *   - `xxxMethod`/`xxxMethods`, where `xxx` is the name of the most prominent declared method.
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
object extensions extends extensions {

	/** Adds a `feedTo` method to any value which applies a given function to `this`. */
	class feedToMethod[X] private[sugar] (private val x :X) extends AnyVal {
		//todo: maybe some special character name?
		/** Applies the argument function to the 'self' argument. As self is eagerly computed, `expr feedTo f`
		  * is equivalent to `{ val x = expr; f(x) }`, but may be more succinct and convenient to write, especially
		  * when applying an argument to a composed function expression:
		  * {{{
		  *     x feedTo (f andThen g andThen h)
		  * }}}
		  */
		def feedTo[T](f :X => T) :T = f(x)

		/** Applies the argument function to the 'self' argument. As self is eagerly computed, `expr feedTo f`
		  * is equivalent to `{ val x = expr; f(x) }`, but may be more succinct and convenient to write, especially
		  * when applying an argument to a composed function expression:
		  * {{{
		  *     x \=> (f andThen g andThen h)
		  * }}}
		  */
		def \=>[T](f :X => T) :T = f(x)
	}


	/** An extension method for any object throwing an [[AssertionError]] if it is `null`. */
	class notNullMethod[X] private[sugar] (private val x :X) extends AnyVal {
		/** An extension method for any object throwing an [[AssertionError]] if it is `null`. */
		@elidable(ASSERTION) @inline def notNull(msg: => String) :X = {
			if (x == null)
				throw new AssertionError(msg)
			x
		}
	}
//	object conditionalExpression {
//		class IfFalse[+T] private[sugar] (private val ifFalse: () => T) {
//			@inline def /:[U >: T](ifTrue: => U) :ConditionalExpressionAlternatives[U] =
//				new ConditionalExpressionAlternatives[U](ifTrue, ifFalse)
//		}
//		class ConditionalExpressionAlternatives[+T] private[sugar] (ifTrue: => T, ifFalse: () => T) {
//			@inline def ?:(condition :Boolean) :T = if (condition) ifTrue else ifFalse()
//		}
//	}
}
