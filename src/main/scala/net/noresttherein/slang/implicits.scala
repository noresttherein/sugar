package net.noresttherein.slang

import net.noresttherein.slang.tuples.{RichTuple10, RichTuple11, RichTuple12, RichTuple13, RichTuple14, RichTuple15, RichTuple16, RichTuple17, RichTuple18, RichTuple19, RichTuple2, RichTuple20, RichTuple21, RichTuple22, RichTuple3, RichTuple4, RichTuple5, RichTuple6, RichTuple7, RichTuple8, RichTuple9}



/** Type aliases and forwarders for most useful types and implicit conversions in the library providing new syntax.
  * It is recommended to import those declarations from here rather than the package of their actual definition.
  * Not only this saves the user searching the packages for a class before its use, but the import clause will
  * explicitly name the members as implicit, making it also easier to find the applied conversion in existing code.
  * As a further mnemonic, class and method members here are (re)named after the most prominent declared method.
  * Finally, the lazy and the brave can import (almost) all new syntax with a single wildcard statement.
  * @author Marcin Mościcki marcin@moscicki.net
  */
object implicits {




	type ifTrue = optional.IfTrue
	@inline implicit final def ifTrue(condition :Boolean) :ifTrue = new ifTrue(condition)

	type satisfying[T] = optional.Satisfying[T]
	@inline implicit final def satisfying[T](subject :T) :satisfying[T] = new satisfying(subject)

	type providing[T] = optional.Providing[T]
	@inline implicit final def providing[T](subject :T) :providing[T] = new providing[T](subject)

	type customEnsuring[T] = optional.CustomEnsuring[T]
	@inline implicit final def customEnsuring[T](subject :T) :customEnsuring[T] = new customEnsuring(subject)



	type foldWhile[T] = repeatedly.foldWhile[T]
	@inline implicit final def foldWhile[T](col :Iterable[T]) :foldWhile[T] = new foldWhile(col)

	type repeatTimes = repeatedly.repeatTimes
	@inline implicit final def repeatTimes(iterations :Int) :repeatTimes = new repeatTimes(iterations)



	type localClassName = prettyprint.ClassNameOf
	@inline implicit final def localClassName(any :Any) :localClassName = new localClassName(any)

	type fieldsString[T] = prettyprint.ObjectFormatter[T]
	@inline implicit final def fieldsString[T](obj :T) = new fieldsString(obj)

	type yesno = prettyprint.YesNo
	@inline implicit final def yesno(boolean :Boolean) = new yesno(boolean)


	@inline implicit def tupleConcat[T1, T2](t :(T1, T2)) :RichTuple2[T1, T2] = new RichTuple2(t)

	@inline implicit def tupleConcat[T1, T2, T3](t :(T1, T2, T3)) :RichTuple3[T1, T2, T3] = new RichTuple3(t)

	@inline implicit def tupleConcat[T1, T2, T3, T4](t :(T1, T2, T3, T4)) :RichTuple4[T1, T2, T3, T4] = new RichTuple4(t)

	@inline implicit def tupleConcat[T1, T2, T3, T4, T5](t :(T1, T2, T3, T4, T5)) :RichTuple5[T1, T2, T3, T4, T5] = new RichTuple5(t)

	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6](t :(T1, T2, T3, T4, T5, T6)) :RichTuple6[T1, T2, T3, T4, T5, T6] = new RichTuple6(t)

	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7](t :(T1, T2, T3, T4, T5, T6, T7)) :RichTuple7[T1, T2, T3, T4, T5, T6, T7] = new RichTuple7(t)

	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8](t :(T1, T2, T3, T4, T5, T6, T7, T8)) :RichTuple8[T1, T2, T3, T4, T5, T6, T7, T8] = new RichTuple8(t)

	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9)) :RichTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9] = new RichTuple9(t)

	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)) :RichTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = new RichTuple10(t)

	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)) :RichTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] = new RichTuple11(t)

	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)) :RichTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] = new RichTuple12(t)

	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)) :RichTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = new RichTuple13(t)

	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)) :RichTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] = new RichTuple14(t)

	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)) :RichTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] = new RichTuple15(t)

	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)) :RichTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] = new RichTuple16(t)

	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)) :RichTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] = new RichTuple17(t)

	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)) :RichTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] = new RichTuple18(t)

	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)) :RichTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = new RichTuple19(t)

	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)) :RichTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] = new RichTuple20(t)

	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)) :RichTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] = new RichTuple21(t)

	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)) :RichTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] = new RichTuple22(t)

}
