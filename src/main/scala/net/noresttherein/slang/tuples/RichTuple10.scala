package net.noresttherein.slang.tuples

/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
final class RichTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](private val t :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)) extends AnyVal {
	import t.{_1 => x1, _2 => x2, _3 => x3, _4 => x4, _5 => x5, _6 => x6, _7 => x7, _8 => x8, _9 => x9, _10 => x10}
	
	@inline def map1[O](f :T1 => O) :(O, T2, T3, T4, T5, T6, T7, T8, T9, T10) = (f apply x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
	
	@inline def map2[O](f :T2 => O) :(T1, O, T3, T4, T5, T6, T7, T8, T9, T10) = (x1, f apply x2, x3, x4, x5, x6, x7, x8, x9, x10)

	@inline def map3[O](f :T3 => O) :(T1, T2, O, T4, T5, T6, T7, T8, T9, T10) = (x1, x2, f apply x3, x4, x5, x6, x7, x8, x9, x10)

	@inline def map4[O](f :T4 => O) :(T1, T2, T3, O, T5, T6, T7, T8, T9, T10) = (x1, x2, x3, f apply x4, x5, x6, x7, x8, x9, x10)

	@inline def map5[O](f :T5 => O) :(T1, T2, T3, T4, O, T6, T7, T8, T9, T10) = (x1, x2, x3, x4, f apply x5, x6, x7, x8, x9, x10)

	@inline def map6[O](f :T6 => O) :(T1, T2, T3, T4, T5, O, T7, T8, T9, T10) = (x1, x2, x3, x4, x5, f apply x6, x7, x8, x9, x10)

	@inline def map7[O](f :T7 => O) :(T1, T2, T3, T4, T5, T6, O, T8, T9, T10) = (x1, x2, x3, x4, x5, x6, f apply x7, x8, x9, x10)

	@inline def map8[O](f :T8 => O) :(T1, T2, T3, T4, T5, T6, T7, O, T9, T10) = (x1, x2, x3, x4, x5, x6, x7, f apply x8, x9, x10)

	@inline def map9[O](f :T9 => O) :(T1, T2, T3, T4, T5, T6, T7, T8, O, T10) = (x1, x2, x3, x4, x5, x6, x7, x8, f apply x9, x10)

	@inline def map10[O](f :T10 => O) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, O) = (x1, x2, x3, x4, x5, x6, x7, x8, x9, f apply x10)



	@inline def :+[TN](xn :TN) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, TN) = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, xn)
	
	@inline def +:[T0](x0 :T0) :(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) = (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
	
	
	@inline def ++[_1, _2](other :(_1, _2)) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, _1, _2) = {
		import other._
		(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, _1, _2)
	}

	@inline def ++[_1, _2, _3](other :(_1, _2, _3)) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, _1, _2, _3) = {
		import other._
		(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, _1, _2, _3)
	}

	@inline def ++[_1, _2, _3, _4](other :(_1, _2, _3, _4)) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, _1, _2, _3, _4) = {
		import other._
		(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, _1, _2, _3, _4)
	}

	@inline def ++[_1, _2, _3, _4, _5](other :(_1, _2, _3, _4, _5)) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, _1, _2, _3, _4, _5) = {
		import other._
		(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, _1, _2, _3, _4, _5)
	}

	@inline def ++[_1, _2, _3, _4, _5, _6](other :(_1, _2, _3, _4, _5, _6)) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, _1, _2, _3, _4, _5, _6) = {
		import other._
		(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, _1, _2, _3, _4, _5, _6)
	}

	@inline def ++[_1, _2, _3, _4, _5, _6, _7](other :(_1, _2, _3, _4, _5, _6, _7)) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, _1, _2, _3, _4, _5, _6, _7) = {
		import other._
		(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, _1, _2, _3, _4, _5, _6, _7)
	}

	@inline def ++[_1, _2, _3, _4, _5, _6, _7, _8](other :(_1, _2, _3, _4, _5, _6, _7, _8)) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, _1, _2, _3, _4, _5, _6, _7, _8) = {
		import other._
		(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, _1, _2, _3, _4, _5, _6, _7, _8)
	}

	@inline def ++[_1, _2, _3, _4, _5, _6, _7, _8, _9](other :(_1, _2, _3, _4, _5, _6, _7, _8, _9)) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, _1, _2, _3, _4, _5, _6, _7, _8, _9) = {
		import other._
		(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, _1, _2, _3, _4, _5, _6, _7, _8, _9)
	}

	@inline def ++[_1, _2, _3, _4, _5, _6, _7, _8, _9, _10](other :(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10)) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10) = {
		import other._
		(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10)
	}

	@inline def ++[_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11](other :(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11)) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11) = {
		import other._
		(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11)
	}

	@inline def ++[_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12](other :(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12)) :(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12) = {
		import other._
		(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12)
	}

}
