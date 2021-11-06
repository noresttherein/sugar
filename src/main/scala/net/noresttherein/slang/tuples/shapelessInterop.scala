package net.noresttherein.slang.tuples

import net.noresttherein.slang.tuples.Tuple.{<>, ><}
import shapeless.{HList, HNil, ::}


/** Implicits for conversion of `Tuple`s to shapeless `HList`s extracted as an optional dependency.
  * @author Marcin Mościcki marcin@moscicki.net
  */
object shapelessInterop {
	sealed abstract class HListTupleConversions[U <: Tuple, T <: HList, L <:HList] {
		def apply(tuple :U, list :T) :L
		def unapply(list :L) :Option[(U, T)]
	}

	object HListTupleConversions {
		implicit def appendEmpty[L <: HList] : HListTupleConversions[<>, L, L] =
			new HListTupleConversions[<>, L, L] {
				def apply(tuple : <>, list :L) = list
				def unapply(list :L) =  Some(<> -> list)
			}


		implicit def appendLast[P <: Tuple, X, T <: HList, L <: HList](implicit Init :HListTupleConversions[P, X::T, L])
		                       :HListTupleConversions[P><X, T, L] =
			new HListTupleConversions[P><X, T, L] {
				override def apply(tuple :P><X, list :T) :L = Init(tuple.init, tuple.last::HList.hlistOps(list))
				override def unapply(list :L) :Option[(P >< X, T)] = list match {
					case Init(p, x::t) => Some((p :+ x, t))
					case _ => None
				}
			}
	}

	implicit def TupleToHList[T <: Tuple, L <: HList](t :T)(implicit appender :HListTupleConversions[T, HNil, L]) :L =
		appender(t, HNil)

	implicit def HListToTuple[T <: Tuple, L <: HList](l :L)(implicit Split :HListTupleConversions[T, L, HNil]) :T = {
		val Split(t, _) = l
		t
	}

}
