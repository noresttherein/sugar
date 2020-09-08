package net.noresttherein.slang.typist

import net.noresttherein.slang.matching.Unapply

import scala.reflect.ClassTag


//trait |[+L, +R]

/**
  * @author Marcin Mościcki
  */
object | {

	@inline def apply[U <: Any | Any](implicit matcher :UnionMatcher[U]) :UnionMatcher[U] = matcher



	class UnionMatcher[U] private[|] (val Matcher :Unapply[Any, U]) extends AnyVal


	sealed abstract class UnionMatcherImplicits {
		@inline implicit def exactClassMatcher[X](cls :ClassTag[X]) :UnionMatcher[X] =
			new UnionMatcher[X](cls.unapply(_))
	}

	object UnionMatcher extends UnionMatcherImplicits {
		implicit def unionMatcher[L, R](left :UnionMatcher[L], right :UnionMatcher[R]) :UnionMatcher[L | R] =
			new UnionMatcher(x => (left.Matcher.unapply(x) match {
				case None => right.Matcher.unapply(x)
				case some => some
			}).asInstanceOf[Option[L | R]])
	}





	abstract class UnionContainsLevel4Implicits {
		//		implicit def implicitUnionUnification[L, R, U <: Any | Any](value :L | R)(implicit left :L => U, right :R => U) :U =
		//			value.asInstanceOf[U]
		//		implicit def implicitUnionUnification[L, R, U <: Any | Any](implicit left :L => U, right :R => U) :(L | R) => U =
		//			left.asInstanceOf[(L | R) => U]
		implicit def implicitUnionUnification[L, R, U <: Any | Any](implicit left :UnionContains[L, U], right :UnionContains[R, U])
				:UnionContains[L | R, U] =
			witness.asInstanceOf[UnionContains[L | R, U]]
	}

	sealed abstract class UnionContainsLevel3Implicits extends UnionContainsLevel4Implicits {
		implicit def implicitRightComposedUnionMember[X, L, R <: Any | Any]
		             (implicit union :UnionContains[X, R]) :UnionContains[X, L | R] =
			witness.asInstanceOf[UnionContains[X, L | R]]
	}

	sealed abstract class UnionContainsLevel2Implicits extends UnionContainsLevel3Implicits {
		implicit def implicitLeftComposedUnionMember[X, L <: Any | Any, R]
		             (implicit union :UnionContains[X, L]) :UnionContains[X, L | R] =
			witness.asInstanceOf[UnionContains[X, L | R]]
	}

	sealed abstract class UnionContainsLevel1Implicits extends UnionContainsLevel2Implicits {
		implicit def implicitRightUnionMember[L, R] :UnionContains[R, L | R] =
			witness.asInstanceOf[UnionContains[R, L | R]]
		implicit def implicitLeftUnionMember[L, R] :UnionContains[L, L | R] =
			witness.asInstanceOf[UnionContains[L, L | R]]
	}

	abstract class UnionContainsImplicits private[slang] extends UnionContainsLevel1Implicits {
		implicit def implicitDuplicateUnionMember[X] :UnionContains[X, X | X] =
			witness.asInstanceOf[UnionContains[X, X | X]]

//		implicit def implicitUnionWidening[X, U](value :X)(implicit member :UnionContains[U, X]) :U =
//			value.asInstanceOf[U]
	}

	class UnionContains[X, U] /*extends (X => U)*/ {
		def apply(x :X) = x.asInstanceOf[U]
	}

	object UnionContains extends UnionContainsImplicits
	private[this] val witness = new UnionContains[Any | Any, Any]

	private[this] val cast = identity[Any] _

	abstract class TypeUnionLevel4Implicits {
		implicit def implicitUnionUnification[L, R, U <: Any | Any](implicit left :L => U, right :R => U) :(L | R) => U =
			left.asInstanceOf[(L | R) => U]
	}

	sealed abstract class TypeUnionLevel3Implicits extends TypeUnionLevel4Implicits {
		implicit def implicitRightComposedUnionMember[X, L, R <: Any | Any](implicit right :X => R) :X => (L | R) =
			right.asInstanceOf[X => (L | R)]
	}

	sealed abstract class TypeUnionLevel2Implicits extends TypeUnionLevel3Implicits {
		implicit def implicitLeftComposedUnionMember[X, L <: Any | Any, R](implicit left :X => L) :X => (L | R) =
			left.asInstanceOf[X => (L | R)]
	}

	sealed abstract class TypeUnionLevel1Implicits extends TypeUnionLevel2Implicits {
		implicit def implicitRightUnionMember[L, R] :R => (L | R) = cast.asInstanceOf[R => (L | R)]
	}

//	abstract class TypeUnionImplicits private[slang] extends TypeUnionLevel1Implicits {
//		implicit def implicitLeftUnionMember[L, R] :L => (L | R) = cast.asInstanceOf[L => (L | R)]
//	}

	abstract class TypeUnionImplicits {
		implicit def implicitUnionWidening[X, U](value :X)(implicit member :UnionContains[U, X]) :U =
			value.asInstanceOf[U]
	}

	//	object union extends TypeUnionImplicits {
//		type |[+L, +R]
//	}
}
