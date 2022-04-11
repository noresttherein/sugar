package net.noresttherein.sugar.tuples

import net.noresttherein.sugar.tuples.Nat.++

import scala.annotation.implicitNotFound


/** Type level implementation of natural numbers used to index variable length tuples. In order to identify the type
  * of the element at the given index, the latter has to be statically encoded. A special subtype
  * [[net.noresttherein.sugar.tuples.Nat._0 Nat._0]] is used to denote zero, while every positive value is represented by
  * recursively applying successor subclass [[net.noresttherein.sugar.tuples.Nat.++ Nat.++]]
  * to the zero type. For example, `++[++[_0]]` is the representation of the natural number `2`.
  * @param number numerical value of this number
  */
sealed abstract class Nat protected (final val number :Int) {

	@inline def toInt :Int = number

	override def toString :String = number.toString

	override def hashCode :Int = number

	override def equals(that :Any) :Boolean = that match {
		case n :Nat => n.number == number
		case _ => false
	}
}





/** Low priority implicit [[net.noresttherein.sugar.tuples.Nat Nat]] values for every natural number
  * by recursively applying it to itself. */
sealed abstract class NatImplicitInduction {
	/** Given an implicit encoding of the natural number `n` provide implicit value representing `n+1`. */
	implicit def ++[N <: Nat](implicit n :N) : ++[N] = new ++(n)
}



/** Implicit values and type aliases for encoding of the first 23 natural numbers as types. */
object Nat extends NatImplicitInduction {

	implicit def NatToInt(n :Nat) :Int = n.toInt

	/** Implicit extension of `Nat` number types providing a successor operator returning the value for the next natural number. */
	implicit class N_++[N <: Nat](private val n :N) extends AnyVal {
		def ++ : ++[N] = new ++[N](n)

		def +[M <: Nat, X <: Nat](m :M)(implicit sum :Sum[N, M, X]) :X =
			if (n.number + m.number == 0) _0.asInstanceOf[X]
			else new ++[Nat](n.number + m.number).asInstanceOf[X]
	}



	/** Implicit evidence that `X` is the sum of `N` and `M`. */
	@implicitNotFound("Can't calculate the sum of integers ${N} and ${M}: both types need to be fully instantiated (or the result is not ${X})")
	final class Sum[N <: Nat, M <: Nat, X <: Nat] private()

	object Sum {
		private[this] final val instance = new Sum[_0, _0, _0]

		@inline implicit def plusZero[N <: Nat] :Sum[N, _0, N] = instance.asInstanceOf[Sum[N, _0, N]]

		@inline implicit def plusOne[N <: Nat, M <: Nat, S <: Nat](implicit sum :Sum[N, M, S]) :Sum[N, ++[M], ++[S]] =
			instance.asInstanceOf[Sum[N, ++[M], ++[S]]]
	}

	type <=[N <: Nat, M <: Nat] = Sum[N, _ <:Nat, M]


	/** Type encoding of the natural number `0`. */
	final class _0 private[Nat]() extends Nat(0)
	/** Implicitly available constant representing the natural number `0` at type level. */
	final implicit val _0 : _0 = new _0

	/** Type constructor implementing successor operation for natural numbers. Given a type-level encoding of number `N`,
	  * creates a type representing `N+1`.
	  * @tparam N a natural number.
	  */
	final class ++[N <: Nat] private[Nat](n :Int) extends Nat(n) {
		def this(pred :N) = this(pred.number + 1)
	}


	/** Type encoding of the natural number `1`. */
	type _1 = ++[_0]
	/** Implicitly available constant representing the natural number `1` at type level. */
	implicit val _1 : _1 = new ++(_0)

	/** Type encoding of the natural number `2`. */
	type _2 = ++[_1]
	/** Implicitly available constant representing the natural number `2` at type level. */
	implicit val _2 : _2 = new ++(_1)


	/** Type encoding of the natural number `3`. */
	type _3 = ++[_2]
	/** Implicitly available constant representing the natural number `3` at type level. */
	implicit val _3 : _3 = new ++(_2)

	/** Type encoding of the natural number `4`. */
	type _4 = ++[_3]
	/** Implicitly available constant representing the natural number `4` at type level. */
	implicit val _4 : _4 = new ++(_3)

	/** Type encoding of the natural number `5`. */
	type _5 = ++[_4]
	/** Implicitly available constant representing the natural number `5` at type level. */
	implicit val _5 : _5 = new ++(_4)

	/** Type encoding of the natural number `6`. */
	type _6 = ++[_5]
	/** Implicitly available constant representing the natural number `6` at type level. */
	implicit val _6 : _6 = new ++(_5)

	/** Type encoding of the natural number `7`. */
	type _7 = ++[_6]
	/** Implicitly available constant representing the natural number `7` at type level. */
	implicit val _7 : _7 = new ++(_6)

	/** Type encoding of the natural number `8`. */
	type _8 = ++[_7]
	/** Implicitly available constant representing the natural number `8` at type level. */
	implicit val _8 : _8 = new ++(_7)

	/** Type encoding of the natural number `9`. */
	type _9 = ++[_8]
	/** Implicitly available constant representing the natural number `9` at type level. */
	implicit val _9 : _9 = new ++(_8)

	/** Type encoding of the natural number `10`. */
	type _10 = ++[_9]
	/** Implicitly available constant representing the natural number `10` at type level. */
	implicit val _10 : _10 = new ++(_9)

	/** Type encoding of the natural number `11`. */
	type _11 = ++[_10]
	/** Implicitly available constant representing the natural number `11` at type level. */
	implicit val _11 : _11 = new ++(_10)

	/** Type encoding of the natural number `12`. */
	type _12 = ++[_11]
	/** Implicitly available constant representing the natural number `12` at type level. */
	implicit val _12 : _12 = new ++(_11)

	/** Type encoding of the natural number `13`. */
	type _13 = ++[_12]
	/** Implicitly available constant representing the natural number `13` at type level. */
	implicit val _13 : _13 = new ++(_12)

	/** Type encoding of the natural number `14`. */
	type _14 = ++[_13]
	/** Implicitly available constant representing the natural number `14` at type level. */
	implicit val _14 : _14 = new ++(_13)

	/** Type encoding of the natural number `15`. */
	type _15 = ++[_14]
	/** Implicitly available constant representing the natural number `15` at type level. */
	implicit val _15 : _15 = new ++(_14)

	/** Type encoding of the natural number `16`. */
	type _16 = ++[_15]
	/** Implicitly available constant representing the natural number `16` at type level. */
	implicit val _16 : _16 = new ++(_15)

	/** Type encoding of the natural number `17`. */
	type _17 = ++[_16]
	/** Implicitly available constant representing the natural number `17` at type level. */
	implicit val _17 : _17 = new ++(_16)

	/** Type encoding of the natural number `18`. */
	type _18 = ++[_17]
	/** Implicitly available constant representing the natural number `18` at type level. */
	implicit val _18 : _18 = new ++(_17)

	/** Type encoding of the natural number `19`. */
	type _19 = ++[_18]
	/** Implicitly available constant representing the natural number `19` at type level. */
	implicit val _19 : _19 = new ++(_18)

	/** Type encoding of the natural number `20`. */
	type _20 = ++[_19]
	/** Implicitly available constant representing the natural number `20` at type level. */
	implicit val _20 : _20 = new ++(_19)

	/** Type encoding of the natural number `21`. */
	type _21 = ++[_20]
	/** Implicitly available constant representing the natural number `21` at type level. */
	implicit val _21 : _21 = new ++(_20)

	/** Type encoding of the natural number `22`. */
	type _22 = ++[_21]
	/** Implicitly available constant representing the natural number `22` at type level. */
	implicit val _22 : _22 = new ++(_21)

}
