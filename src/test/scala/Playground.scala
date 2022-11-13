import java.math.{MathContext, RoundingMode}
import java.math.MathContext.{DECIMAL128, DECIMAL32}
import java.math.RoundingMode.{DOWN, FLOOR, HALF_DOWN, HALF_EVEN, HALF_UP, UNNECESSARY, UP}

import scala.math.BigDecimal.RoundingMode.CEILING

import net.noresttherein.sugar.numeric.{Decimal64, Decimal64Spec}
import net.noresttherein.sugar.numeric.Decimal64.Round.{maxPrecision, Extended, ExtendedExact, Standard}
import net.noresttherein.sugar.numeric.Decimal64.implicits.scientificDecimalNotation
import net.noresttherein.sugar.numeric.Decimal64.{Precision, Round}


object Playground {
	class Test {
		type Co[+E] = E
	}
	class Dupa { type T }
	trait E[-F] {
		def r(dupa :Dupa { type T <: F })
	}
	class X
	class Y
	class Z
	class Lift[X, Y]
	sealed class LiftImplicits {
		implicit def identity[X] = new Lift[X, X]
	}

	object Lift extends LiftImplicits {
		implicit val x2z = new Lift[X, Z]
		implicit val y2z = new Lift[Y, Z]
	}

	trait =~=[L, R] { type Unified }
	object =~= {
		implicit def lift[X, Y, Z](implicit left :Lift[X, Z], right :Lift[Y, Z])
		:(X =~= Y) { type Unified = Z } =
			???
	}

	def equalize[L, R, U](l :L, r :R)(implicit left :Lift[L, U], right :Lift[R, U]) :Unit = ???
	equalize(new X, new Y)

	implicitly[Lift[X, Z]]
	implicitly[Lift[Y, Z]]
	implicitly[X =~= Y]
}
