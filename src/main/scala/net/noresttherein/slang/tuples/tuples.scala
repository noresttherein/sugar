package net.noresttherein.slang

/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
package object tuples {

	/** Adds `++`, `:+` and `+: methods to `Tuple2`.` */
	@inline implicit def tupleConcat[T1, T2](t :(T1, T2)) :RichTuple2[T1, T2] = new RichTuple2(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple3`.` */
	@inline implicit def tupleConcat[T1, T2, T3](t :(T1, T2, T3)) :RichTuple3[T1, T2, T3] = new RichTuple3(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple4`.` */
	@inline implicit def tupleConcat[T1, T2, T3, T4](t :(T1, T2, T3, T4)) :RichTuple4[T1, T2, T3, T4] = new RichTuple4(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple5`.` */
	@inline implicit def tupleConcat[T1, T2, T3, T4, T5](t :(T1, T2, T3, T4, T5)) :RichTuple5[T1, T2, T3, T4, T5] = new RichTuple5(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple6`.` */
	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6](t :(T1, T2, T3, T4, T5, T6)) :RichTuple6[T1, T2, T3, T4, T5, T6] = new RichTuple6(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple7`.` */
	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7](t :(T1, T2, T3, T4, T5, T6, T7)) :RichTuple7[T1, T2, T3, T4, T5, T6, T7] = new RichTuple7(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple8`.` */
	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8](t :(T1, T2, T3, T4, T5, T6, T7, T8)) :RichTuple8[T1, T2, T3, T4, T5, T6, T7, T8] = new RichTuple8(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple9`.` */
	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9)) :RichTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9] = new RichTuple9(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple10`.` */
	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)) :RichTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = new RichTuple10(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple11`. */
	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)) :RichTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] = new RichTuple11(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple12`.` */
	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)) :RichTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] = new RichTuple12(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple13`.` */
	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)) :RichTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = new RichTuple13(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple14`.` */
	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)) :RichTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] = new RichTuple14(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple15`.` */
	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)) :RichTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] = new RichTuple15(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple16. */
	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)) :RichTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] = new RichTuple16(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple17`.` */
	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)) :RichTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] = new RichTuple17(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple18`.` */
	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)) :RichTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] = new RichTuple18(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple19`.` */
	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)) :RichTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = new RichTuple19(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple20`.` */
	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)) :RichTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] = new RichTuple20(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple21`.` */
	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)) :RichTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] = new RichTuple21(t)

	/** Adds `++`, `:+` and `+: methods to `Tuple22`.` */
	@inline implicit def tupleConcat[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)) :RichTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] = new RichTuple22(t)

}
