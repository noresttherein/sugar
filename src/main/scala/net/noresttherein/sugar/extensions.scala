package net.noresttherein.sugar

import net.noresttherein.sugar.extensions.feedToMethod
import net.noresttherein.sugar.tuples.{RichTuple10, RichTuple11, RichTuple12, RichTuple13, RichTuple14, RichTuple15, RichTuple16, RichTuple17, RichTuple18, RichTuple19, RichTuple2, RichTuple20, RichTuple21, RichTuple22, RichTuple3, RichTuple4, RichTuple5, RichTuple6, RichTuple7, RichTuple8, RichTuple9}
import net.noresttherein.sugar.typist.casting




/** (Almost) all extension methods and implicit conversions providing new syntax in this library .
  * Grouping them here not only allows a wildcard import, but also makes single imports stand out due to object name.
  *
  * As a mnemonic, class and method members here are named either
  *   - `xxxExtension`, where `Xxx` is the name of the enriched type, if it is they are not general purpose methods,
  *     but work on specific types such as [[Option]] or [[Iterable]], or
  *   - `xxxMethod`/`xxxMethods`, where `xxx` is the name of the most prominent declared method.
  * @author Marcin Mościcki
  */
object extensions extends extensions {

	/** Adds a `feedTo` method to any value which applies a given function to `this`. */
	class feedToMethod[X](private val x :X) extends AnyVal {
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
		  *     x \-> (f andThen g andThen h)
		  * }}}
		  */
		def \=>[T](f :X => T) :T = f(x)
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




/** Type aliases and forwarders for most useful types and implicit conversions in the library providing new syntax.
  * They can be imported into user code either by an explicit import of the contents of the companion object
  * [[net.noresttherein.sugar.implicits$ implicits]] (which extends this trait), or by having a package object
  * containing classes using these features extend this trait. This is of course true also for any other object or class.
  * As a mnemonic, class and method members here are (re)named after the most prominent declared method.
  * @author Marcin Mościcki marcin@moscicki.net
  */ //consider: having it simply extend the traits for individual packages.
trait extensions extends Any
	with casting.extensions with collection.extensions with exceptions.extensions
	with numeric.extensions with optional.extensions with prettyprint.extensions
	with reflect.extensions with repeat.extensions
{
	/** Adds a `feedTo` method to any value which applies a given function to `this`. */
	@inline implicit final def feedToMethod[X](x :X) = new feedToMethod(x)
//
//	/** Adds a right associative [[net.noresttherein.sugar.implicits.conditionalExpression.IfFalse./: /:]] method
//	  * to any expression, allowing to create a Java-style conditional expression from right to left:
//	  * {{{
//	  *     tasty(cheesecake) ?: eatMore(cheesecake) /: sayNo()
//	  * }}}
//	  */
//	@inline implicit final def conditionalExpression[T](ifFalse: => T) :conditionalExpression.IfFalse[T] =
//		new conditionalExpression.IfFalse(() => ifFalse)



	/** Adds `++`, `:+` and `+: methods to `Tuple2`.` */
	@inline implicit final def tupleConcat[T1, T2](t :(T1, T2)) :RichTuple2[T1, T2] = new RichTuple2(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple3`.` */
	@inline implicit final def tupleConcat[T1, T2, T3](t :(T1, T2, T3)) :RichTuple3[T1, T2, T3] = new RichTuple3(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple4`.` */
	@inline implicit final def tupleConcat[T1, T2, T3, T4](t :(T1, T2, T3, T4)) :RichTuple4[T1, T2, T3, T4] = new RichTuple4(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple5`.` */
	@inline implicit final def tupleConcat[T1, T2, T3, T4, T5](t :(T1, T2, T3, T4, T5)) :RichTuple5[T1, T2, T3, T4, T5] = new RichTuple5(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple6`.` */
	@inline implicit final def tupleConcat[T1, T2, T3, T4, T5, T6](t :(T1, T2, T3, T4, T5, T6)) :RichTuple6[T1, T2, T3, T4, T5, T6] = new RichTuple6(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple7`.` */
	@inline implicit final def tupleConcat[T1, T2, T3, T4, T5, T6, T7](t :(T1, T2, T3, T4, T5, T6, T7)) :RichTuple7[T1, T2, T3, T4, T5, T6, T7] = new RichTuple7(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple8`.` */
	@inline implicit final def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8](t :(T1, T2, T3, T4, T5, T6, T7, T8)) :RichTuple8[T1, T2, T3, T4, T5, T6, T7, T8] = new RichTuple8(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple9`.` */
	@inline implicit final def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9)) :RichTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9] = new RichTuple9(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple10`.` */
	@inline implicit final def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)) :RichTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = new RichTuple10(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple11`. */
	@inline implicit final def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)) :RichTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] = new RichTuple11(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple12`.` */
	@inline implicit final def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)) :RichTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] = new RichTuple12(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple13`.` */
	@inline implicit final def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)) :RichTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = new RichTuple13(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple14`.` */
	@inline implicit final def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)) :RichTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] = new RichTuple14(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple15`.` */
	@inline implicit final def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)) :RichTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] = new RichTuple15(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple16. */
	@inline implicit final def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)) :RichTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] = new RichTuple16(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple17`.` */
	@inline implicit final def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)) :RichTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] = new RichTuple17(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple18`.` */
	@inline implicit final def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)) :RichTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] = new RichTuple18(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple19`.` */
	@inline implicit final def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)) :RichTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = new RichTuple19(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple20`.` */
	@inline implicit final def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)) :RichTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] = new RichTuple20(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple21`.` */
	@inline implicit final def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)) :RichTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] = new RichTuple21(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple22`.` */
	@inline implicit final def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)) :RichTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] = new RichTuple22(t)

}
