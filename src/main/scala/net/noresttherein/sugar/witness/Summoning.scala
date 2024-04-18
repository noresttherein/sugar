package net.noresttherein.sugar.witness

import net.noresttherein.sugar.witness.Summoning.{Circle1Bound, Circle1Bounds, Circle2Bound, Circle2Bounds, Circle3Bound, Circle3Bounds}




/** Summoning methods requiring both lower and upper bound for every type parameter of the summoned type.
  * Extracted from `Summoning` to dodge erasure conflicts.
  */
private[witness] sealed abstract class DoublyBoundSummoning {
	@inline def summon[T[_ >: L <: U], L <: U, U](implicit circle :Circle1Bounds[T, L, U]) :circle.Genie = circle.genie

	@inline def summon[T[_ >: L1 <: U1, _ >: L2 <: U2], L1 <: U1, U1, L2 <: U2, U2]
	                  (implicit circle :Circle2Bounds[T, L1, U1, L2, U2]) :circle.Genie = circle.genie

	@inline def summon[T[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3], L1 <: U1, U1, L2 <: U2, U2, L3 <: U3, U3]
	                  (implicit circle :Circle3Bounds[T, L1, U1, L2, U2, L3, U3]) :circle.Genie = circle.genie

	@inline def circle[T[_ >: L <: U], L <: U, U](implicit circle :Circle1Bounds[T, L, U]) :circle.type = circle

	@inline def circle[T[_ >: L1 <: U1, _ >: L2 <: U2], L1 <: U1, U1, L2 <: U2, U2]
	                  (implicit circle :Circle2Bounds[T, L1, U1, L2, U2]) :circle.type = circle

	@inline def circle[T[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3], L1 <: U1, U1, L2 <: U2, U2, L3 <: U3, U3]
	                  (implicit circle :Circle3Bounds[T, L1, U1, L2, U2, L3, U3]) :circle.type = circle
}


/** Summoning methods with an upper bound. Extracted from `Summoning` to dodge erasure conflicts. */
private[witness] sealed abstract class BoundSummoning extends DoublyBoundSummoning {
	@inline def summon[T[_ <: U], U](implicit circle :Circle1Bound[T, U]) :circle.Genie = circle.genie
	@inline def summon[T[_ <: U1, _ <: U2], U1, U2]
	                  (implicit circle :Circle2Bound[T, U1, U2]) :circle.Genie = circle.genie

	@inline def summon[T[_ <: U1, _ <: U2, _ <: U3], U1, U2, U3]
	                  (implicit circle :Circle3Bound[T, U1, U2, U3]) :circle.Genie = circle.genie

	@inline def circle[T[_ <: U], U](implicit circle :Circle1Bound[T, U]) :circle.type = circle
	@inline def circle[T[_ <: U1, _ <: U2], U1, U2]
	                  (implicit circle :Circle2Bound[T, U1, U2]) :circle.type = circle

	@inline def circle[T[_ <: U1, _ <: U2, _ <: U3], U1, U2, U3]
	                  (implicit circle :Circle3Bound[T, U1, U2, U3]) :circle.type = circle
}



/** Helper methods for summoning implicit evidence of higher types with free type parameters.
  * Provides summoning methods for type constructors of arities 1, 2, and 3, without bounds on type parameter,
  * with upper bounds, and with both bounds present.
  * There are two categories of methods:
  *   1. `summon[T, ...]` returns an implicit instance of `T`, if present, and its type arguments conform to
  *      optional type bounds listed in `...`.
  *   1. `circle[T, ...]` returns an evidence object wrapping the same value as above, but, additionally,
  *      defining member types equal to the type arguments of the summoned instance.
  *
  * Both method categories are heavily overloaded, and which is called depends on the type arguments given.
  * The first type argument is always the type constructor `T`.
  *    - if it is the sole parameter, then its type parameters must not have bounds.
  *    - otherwise, if additional `N` type arguments should are provided to the method,
  *      where `N` is the number of type parameters in `T`, then they are treated as upper bounds for subsequent
  *      type arguments of the summoned instance;
  *    - otherwise, if additional `2N` type arguments are provided for an N-ary type `T`,
  *      then type argument `2n - 1` is the lower bound, and type argument `2n` the upper bound on
  *      on the `n`-th type parameter of `T`, counting from one.
  *
  * @example {{{
  *     sealed trait Element
  *     case object Fire extends Element
  *     case object Water extends Element
  *     case object Air extends Element
  *
  *     class Spirit[E <: Element]
  *
  *     implicit val waterSpirit = new Spirit[Water.type]
  *
  *     val spirit = summon[Spirit, Element] //Spirit[Water.type]
  *     assert(spirit eq waterSpirit)
  *
  *     val summoned = circle[Spirit, Element] //Circle1Bound[Spirit, Element]
  *     assert(summoned.genie eq waterSpirit)
  *     implicitly[summoned.Genie <:< Spirit[Water.type]]
  *     implicitly[summoned.P <:< Water.type]
  * }}}
  */
object Summoning extends BoundSummoning {
	//These three differ from implicitly[T[...]], in that circle.Genie is a singleton type, and the type arguments
	// are instantiated, while the type of the value returned by implicitly exactly equals T[_]/T[_, _]/T[_, _, _]
	@inline def summon[T[_]](implicit circle :Circle1[T]) :circle.Genie = circle.genie
	@inline def summon[T[_, _]](implicit circle :Circle2[T]) :circle.Genie = circle.genie
	@inline def summon[T[_, _, _]](implicit circle :Circle3[T]) :circle.Genie = circle.genie

	@inline def circle[T[_]](implicit circle :Circle1[T]) :circle.Genie = circle.genie
	@inline def circle[T[_, _]](implicit circle :Circle2[T]) :circle.Genie = circle.genie
	@inline def circle[T[_, _, _]](implicit circle :Circle3[T]) :circle.Genie = circle.genie



	sealed abstract class SummoningCircle {
		type Genie
		val genie :Genie
	}
	sealed abstract class Circle1Bounds[T[_ >: L <: U], L <: U, U] extends SummoningCircle {
		type P >: L <: U
		type Genie <: T[P]
	}
	sealed abstract class Circle1Bound[T[_ <: U], U] extends Circle1Bounds[T, Nothing, U]
	sealed abstract class Circle1[T[_]] extends Circle1Bound[T, Any]

	@inline implicit def Circle1Bounds[T[_ >: L <: U], L <: U, U, X >: L <: U](implicit ev :T[X])
			:Circle1Bounds[T, L, U] { type P = X; type Genie = ev.type } =
		new Circle1Bounds[T, L, U] {
			override type P = X
			override type Genie = ev.type
			override val genie = ev
		}
	@inline implicit def Circle1Bound[T[_ <: U], U, X <: U](implicit ev :T[X])
			:Circle1Bound[T, U] { type P = X; type Genie = ev.type } =
		new Circle1Bound[T, U] {
			override type P = X
			override type Genie = ev.type
			override val genie = ev
		}
	@inline implicit def Circle1[T[_], X](implicit ev :T[X]) :Circle1[T] { type P = X; type Genie = ev.type } =
		new Circle1[T] {
			override type P = X
			override type Genie = ev.type
			override val genie = ev
		}



	sealed abstract class Circle2Bounds[T[_ >: L1 <: U1, _ >: L2 <: U2], L1 <: U1, U1, L2 <: U2, U2]
		extends SummoningCircle
	{
		type P1 >: L1 <: U1
		type P2 >: L2 <: U2
		type Genie <: T[P1, P2]
	}
	sealed abstract class Circle2Bound[T[_ <: U1, _ <: U2], U1, U2] extends Circle2Bounds[T, Nothing, U1, Nothing, U2]
	sealed abstract class Circle2[T[_, _]] extends Circle2Bound[T, Any, Any]

	@inline implicit def Circle2Bounds[T[_ >: L1 <: U1, _ >: L2 <: U2], L1 <: U1, U1, L2 <: U2, U2,
	                                   X1 >: L1 <: U1, X2 >: L2 <: U2]
	                                  (implicit ev :T[X1, X2])
			:Circle2Bounds[T, L1, U1, L2, U2] { type P1 = X1; type P2 = X2; type Genie = ev.type } =
		new Circle2Bounds[T, L1, U1, L2, U2] {
			override type P1 = X1
			override type P2 = X2
			override type Genie = ev.type
			override val genie = ev
		}

	@inline implicit def Circle2Bound[T[_ <: U1, _ <: U2], U1, U2, X1 <: U1, X2 <: U2](implicit ev :T[X1, X2])
			:Circle2Bound[T, U1, U2] { type P1 = X1; type P2 = X2; type Genie = ev.type } =
		new Circle2Bound[T, U1, U2] {
			override type P1 = X1
			override type P2 = X2
			override type Genie = ev.type
			override val genie = ev
		}

	@inline implicit def Circle2[T[_, _], X1, X2](implicit ev :T[X1, X2])
			:Circle2[T] { type P1 = X1; type P2 = X2; type Genie = ev.type } =
		new Circle2[T] {
			override type P1 = X1
			override type P2 = X2
			override type Genie = ev.type
			override val genie = ev
		}



	sealed abstract class Circle3Bounds[T[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3], 
	                                    L1 <: U1, U1, L2 <: U2, U2, L3 <: U3, U3]
		extends SummoningCircle
	{
		type P1 >: L1 <: U1
		type P2 >: L2 <: U2
		type P3 >: L3 <: U3
		type Genie <: T[P1, P2, P3]
	}
	sealed abstract class Circle3Bound[T[_ <: U1, _ <: U2, _ <: U3], U1, U2, U3]
		extends Circle3Bounds[T, Nothing, U1, Nothing, U2, Nothing, U3]
		
	sealed abstract class Circle3[T[_, _, _]] extends Circle3Bound[T, Any, Any, Any]

	
	@inline implicit def Circle3Bounds[T[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3],
	                                   L1 <: U1, U1, L2 <: U2, U2, L3 <: U3, U3,
	                                   X1 >: L1 <: U1, X2 >: L2 <: U2, X3 >: L3 <: U3]
	                                  (implicit ev :T[X1, X2, X3])
			:Circle3Bounds[T, L1, U1, L2, U2, L3, U3] { type P1 = X1; type P2 = X2; type P3 = X3; type Genie = ev.type } =
		new Circle3Bounds[T, L1, U1, L2, U2, L3, U3] {
			override type P1 = X1
			override type P2 = X2
			override type P3 = X3
			override type Genie = ev.type
			override val genie = ev
		}

	@inline implicit def Circle3Bound[T[_ <: U1, _ <: U2, _ <: U3], U1, U2, U3, X1 <: U1, X2 <: U2, X3 <: U3]
	                                 (implicit ev :T[X1, X2, X3])
			:Circle3Bound[T, U1, U2, U3] { type P1 = X1; type P2 = X2; type P3 = X3; type Genie = ev.type } =
		new Circle3Bound[T, U1, U2, U3] {
			override type P1 = X1
			override type P2 = X2
			override type P3 = X3
			override type Genie = ev.type
			override val genie = ev
		}

	@inline implicit def Circle3[T[_, _, _], X1, X2, X3](implicit ev :T[X1, X2, X3])
			:Circle3[T] { type P1 = X1; type P2 = X2; type P3 = X3; type Genie = ev.type } =
		new Circle3[T] {
			override type P1 = X1
			override type P2 = X2
			override type P3 = X3
			override type Genie = ev.type
			override val genie = ev
		}

}
