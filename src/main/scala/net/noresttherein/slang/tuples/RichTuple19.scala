package net.noresttherein.slang.tuples

/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
final class RichTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](private val t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)) extends AnyVal {
	import t.{_1 => x1, _10 => x10, _11 => x11, _12 => x12, _13 => x13, _14 => x14, _15 => x15, _16 => x16, _17 => x17, _18 => x18, _19 => x19, _2 => x2, _3 => x3, _4 => x4, _5 => x5, _6 => x6, _7 => x7, _8 => x8, _9 => x9}
	
	@inline def map1[O](f :T1 => O) :(O, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) = (f apply x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)
	
	@inline def map2[O](f :T2 => O) :(T1, O, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) = (x1, f apply x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)

	@inline def map3[O](f :T3 => O) :(T1, T2, O, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) = (x1, x2, f apply x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)

	@inline def map4[O](f :T4 => O) :(T1, T2, T3, O, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) = (x1, x2, x3, f apply x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)

	@inline def map5[O](f :T5 => O) :(T1, T2, T3, T4, O, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) = (x1, x2, x3, x4, f apply x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)

	@inline def map6[O](f :T6 => O) :(T1, T2, T3, T4, T5, O, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) = (x1, x2, x3, x4, x5, f apply x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)

	@inline def map7[O](f :T7 => O) :(T1, T2, T3, T4, T5, T6, O, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) = (x1, x2, x3, x4, x5, x6, f apply x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)

	@inline def map8[O](f :T8 => O) :(T1, T2, T3, T4, T5, T6, T7, O, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) = (x1, x2, x3, x4, x5, x6, x7, f apply x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)

	@inline def map9[O](f :T9 => O) :(T1, T2, T3, T4, T5, T6, T7, T8, O, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) = (x1, x2, x3, x4, x5, x6, x7, x8, f apply x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)

	@inline def map10[O](f :T10 => O) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, O, T11, T12, T13, T14, T15, T16, T17, T18, T19) = (x1, x2, x3, x4, x5, x6, x7, x8, x9, f apply x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)

	@inline def map11[O](f :T11 => O) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, O, T12, T13, T14, T15, T16, T17, T18, T19) = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, f apply x11, x12, x13, x14, x15, x16, x17, x18, x19)

	@inline def map12[O](f :T12 => O) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, O, T13, T14, T15, T16, T17, T18, T19) = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, f apply x12, x13, x14, x15, x16, x17, x18, x19)
	
	@inline def map13[O](f :T13 => O) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, O, T14, T15, T16, T17, T18, T19) = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, f apply x13, x14, x15, x16, x17, x18, x19)
	
	@inline def map14[O](f :T14 => O) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, O, T15, T16, T17, T18, T19) = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, f apply x14, x15, x16, x17, x18, x19)
	
	@inline def map15[O](f :T15 => O) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, O, T16, T17, T18, T19) = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, f apply x15, x16, x17, x18, x19)
	
	@inline def map16[O](f :T16 => O) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, O, T17, T18, T19) = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, f apply x16, x17, x18, x19)
	
	@inline def map17[O](f :T17 => O) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, O, T18, T19) = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, f apply x17, x18, x19)

	@inline def map18[O](f :T18 => O) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, O, T19) = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, f apply x18, x19)

	@inline def map19[O](f :T19 => O) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, O) = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, f apply x19)

	
	

	@inline def :+[TN](xn :TN) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, TN) = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, xn)
	
	@inline def +:[T0](x0 :T0) :(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) = (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)
	
	
	@inline def ++[_1, _2](other :(_1, _2)) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, _1, _2) = {
		import other._
		(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, _1, _2)
	}

	@inline def ++[_1, _2, _3](other :(_1, _2, _3)) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, _1, _2, _3) = {
		import other._
		(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, _1, _2, _3)
	}


}
