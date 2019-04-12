package net.noresttherein.slang

import net.noresttherein.slang.matching.Unapply
import net.noresttherein.slang.numeric.Ratio
import net.noresttherein.slang.numeric.Ratio.{/%, One, Zero}
import net.noresttherein.slang.optional.allOf
import net.noresttherein.slang.optional.Providing
import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop._




object RatioSpec extends Properties("Ratio") {

	val Tolerance = 0.000000000000001

	implicit class AlmostEquals(private val lh :Double) extends AnyVal {
		@inline def =~=(rh :Double) :Prop = {
			val tolerance = lh.abs * Tolerance max rh.abs * Tolerance
			(lh - rh).abs < tolerance
		}
	}

	private def force(x :Int) :Int = if (x == Int.MinValue) -Int.MaxValue else x

	val Force = Unapply { x :Int => Some(if (x == Int.MinValue) -Int.MaxValue else x) }
	val InRange = Unapply { x :Int => if (x==Int.MinValue) None else Some(x) }



	private def constructorProp(f :(Int, Int) => Ratio) :Prop = forAll { (n :Int, d :Int) =>
		if (d == 0)
			Prop(throws(classOf[ArithmeticException]) { f(n, d) }) :| "throws ArithmeticException on division by zero"
		else if (n == Int.MinValue)
			Prop(throws(classOf[ArithmeticException]) { f (n, 1) }) :| "throws ArithmeticException for Int.MinValue"
		else if (d == Int.MinValue)
			Prop(throws(classOf[ArithmeticException]) { f (1, d) }) :| "throws ArithmeticException for 1/Int.MinValue"
		else {
			val r = f(n, d)
			(r.toDouble ?= n.toDouble / d) &&
				(r.signum ?= n.signum * d.signum) :| s"($r).signum=${r.signum}; should be ${n.signum * d.signum} for $n / $d" &&
				forAll { i :Int => Prop(i != 0) ==>
					(Prop(r.numerator % i == 0 && r.denominator % i == 0) ==> (i == 1  || i == -1) label
						s"$i divides ${r.numerator}, ${r.denominator}")
				}
		}
	}

	property("apply(Int, Int)") = constructorProp(Ratio.apply)
	property("/%") = constructorProp(_ /% _)
	property("apply(Long, Long)") = constructorProp((n, d) => Ratio(n.toLong, d.toLong))


	private def shouldEqualSelf(num :Int, den :Int) :Prop = (num /% den ?= num /% den) && forAll { d :Int =>
		if (d == 0 || d == Int.MinValue || num * d / d != num || den * d / d != den)
			Prop(true) //overflow
		else
			((num * d) /% (den * d) ?= num /% den) :| s"$num * $d / $den * $d ?= $num / $den"
	}

	property("==") = forAll { (a :Int, b :Int) =>
		if (a != Int.MinValue && b != Int.MinValue)
			if (b == 0)
				if (a==0) Prop(true)
				else shouldEqualSelf(b, a)
			else
				if (a==0)
                    shouldEqualSelf(a, b)
				else if (a.abs == b.abs)
					shouldEqualSelf(a, b) && (a /% b ?= b /% a)
				else
					shouldEqualSelf(a, b) && Prop(a /% b != b /% a) :| s"$a / $b != $b / $a"
		else
			Prop.passed
	}


	property("unary_-") = forAll { (a :Int, b :Int) =>
		if (a == Int.MinValue || b == Int.MinValue)
			Prop.passed
		else if (b == 0)
			if (a==0 || a == Int.MinValue) -Zero ?= Zero
			else (-(b /% a) ?= -b /% a) && (-(b /% a) ?= (b /% -a))
		else
			(-(a /% b) ?= -a /% b) && (-(a /% b) ?= (a /% -b))
	}

	property("abs") = forAll { (a :Int, b :Int) =>
		if (b == 0 || a == Int.MinValue || b==Int.MinValue)
			Zero.abs =? Zero.abs && One.abs =? One && One =? (-One).abs
		else if (a.signum * b.signum >= 0)
			(a /% b).abs ?= (a /% b)
		else
			(a /% b).abs ?= -(a /% b)
	}

	property("+") = additionProp(_ + _, "+")
	property("+:") = additionProp(_ +: _, "+:")

	property("-") = subtractionProp(_ - _, "-")
	property("-:") = subtractionProp(_ -: _, "-:")


	property("*") = multiplicationProp(_ * _, "*")
	property("*:") = multiplicationProp(_ *: _, "*:")

	property("/") = divisionProp(_ / _, "/")
	property("/:") = divisionProp(_ /: _, "/:")






	private def additionProp(rational :(Ratio, Ratio) => Ratio, s :String) :Prop =
		forAll { (x1 :Int, x2 :Int, x3 :Int, x4 :Int) =>
			val nums = Seq(x1, x2, x3, x4)
			val nonZeros = nums.filterNot(_ == 0)
			nonZeros match {
				case Seq() =>
					(Zero =? rational(Zero, Zero) label s"0 =? 0 $s 0") && (One =? rational(Zero, One) label s"1 =? 0+ 1")
				case Seq(Force(x)) =>
					(Ratio(x) =? rational(x, 0) label s"x =? x $s 0") && (Ratio.unit(x) =? rational(1 /% x, 0) label s"1 /% x =? 1 /% x $s 0")
				case Seq(Force(x), Force(y)) =>
					val prop =
						if (x + y == x.toLong + y && x + y != Int.MinValue)
							Ratio(x + y) =? rational(x, y) label s"x=$x, y=$y: (x + y) /% 1 =? x /% 1 $s y /% 1"
						else
							Prop.passed
					if (x * y +1 != x.toLong * y + 1)
						prop
					else
						((x * y + 1) /% y =? rational(x, 1 /% y) label s"x=$x, y=$y: (x*y + 1) /% y =? x $s 1 /% y") &&
							((x * y + 1) /% x =? rational(y, 1 /% x) label s"x=$x, y=$y: (x*y + 1) /% x =? y $s 1 /% x") &&
							((x + y) /% (x * y) =? rational(1 /% x, 1 /% y) label s"x=$x, y=$y: (x + y) /% x * y =? 1 /% x $s 1 /% y")

				case Seq(Force(x), Force(y), Force(z)) =>
					def props(x :Int, y :Int, z :Int) :Prop = {
						val prop =
							if (x * y != x.toLong * y || x * y == Int.MinValue ||
								y + z * x != y + z * x.toLong || y + z * x == Int.MinValue)
								Prop.passed
							else
                                Ratio(y + z * x, x * y) =? rational(1 /% x, z /% y) label
	                                s"x=$x, y=$y, z=$z: (y + z * x) /% (x * y) =? 1 /% x $s z /% y"
						if (x * y + z != x.toLong * y + z || x * y + z == Int.MinValue)
							prop
						else
							prop && (Ratio(x * y + z, y) =? rational(x, z /% y) label s"x=$x, y=$y, z=$z: (x * y + z) /% y =? x $s z /% y")
					}
					props(x, y, z) && props(x, z, y) && props(z, x, y) && props(z, y, x) && props(y, z, x) && props(y, x, z)
				case _ =>
					def props4(w :Int, x :Int, y :Int, z :Int) :Prop =
						if (w * x + y * z != w.toLong * x + y.toLong * z || w * x + y * z == Int.MinValue ||
							x * y != x.toLong * y || x * y == Int.MinValue)
							Prop.passed
						else
							Ratio(w * x + y * z, x * y) =? rational(w /% y, z /% x) label
								s"w=$w, x=$x, y=$y, z=$z: (w * x + y * z) /% (x * y) =? w /% y $s z /% x"
					nonZeros.map(force).permutations.map{ case Seq(w, x, y, z) => props4(w, x, y, z) }.reduce(_ && _)
			}
		}








	private def subtractionProp(rational :(Ratio, Ratio) => Ratio, s :String) :Prop =
		forAll { (x1 :Int, x2 :Int, x3 :Int, x4 :Int) =>
			val nums = Seq(x1, x2, x3, x4)
			val nonZeros = nums.filterNot(_ == 0)
			nonZeros match {
				case Seq() =>
					(Zero =? rational(Zero, Zero) label s"0 =? 0 $s 0") && (One =? rational(One, Zero) label s"1 =? 0+ 1")
				case Seq(Force(x)) =>
					(Ratio(x) =? rational(x, 0) label s"x =? x $s 0") && (Ratio.unit(x) =? rational(1 /% x, 0) label s"1 /% x =? 1 /% x $s 0") &&
					(Ratio(-x) =? rational(0, x) label s"-x =? 0 $s x") && (Ratio.unit(-x) =? rational(0, 1 /% x) label s"-1 /% x =? 0 $s 1 /% x")
				case Seq(Force(x), Force(y)) => all(allOf(
					Ratio(x - y) =? rational(x, y) label s"x=$x, y=$y: (x - y) /% 1 =? x /% 1 $s y /% 1" providing
						(x - y == x.toLong - y && x - y != Int.MinValue),
					((x * y - 1) /% y =? rational(x, 1 /% y) label s"x=$x, y=$y: (x*y - 1) /% y =? x $s 1 /% y") &&
					((1 - x * y) /% x =? rational(1 /% x, y) label s"x=$x, y=$y: (1 - x*y) /% x =? 1 /% x $s y") providing
						(x * y - 1 == x.toLong * y - 1 && x * y - 1 != Int.MinValue),
					((x - y) /% (x * y) =? rational(1 /% y, 1 /% x) label s"x=$x, y=$y: (x - y) /% x * y =? 1 /% y $s 1 /% x") providing
						(x - y == x.toLong - y && x * y == x.toLong * y && x - y != Int.MinValue && x * y != Int.MinValue)
				) :_*)
				case Seq(Force(x), Force(y), Force(z)) =>
					def props(x :Int, y :Int, z :Int) :Prop = all(allOf(
						Ratio(y - z * x, x * y) =? rational(1 /% x, z /% y) label
							s"x=$x, y=$y, z=$z: (y - z * x) /% (x * y) =? 1 /% x $s z /% y" unless
							(x * y != x.toLong * y || x * y == Int.MinValue ||
								y - z * x != y + z * x.toLong || y - z * x == Int.MinValue),
						Ratio(x * y - z, y) =? rational(x, z /% y) label s"x=$x, y=$y, z=$z: (x * y - z) /% y =? x $s z /% y" unless
							(x * y - z != x.toLong * y - z || x * y - z == Int.MinValue)
					):_*)
					props(x, y, z) && props(x, z, y) && props(z, x, y) && props(z, y, x) && props(y, z, x) && props(y, x, z)
				case _ =>
					def props4(w :Int, x :Int, y :Int, z :Int) :Prop =
						if (w * x - y * z != w.toLong * x - y.toLong * z || w * x - y * z == Int.MinValue ||
							x * y != x.toLong * y || x * y == Int.MinValue)
							Prop.passed
						else
							Ratio(w * x - y * z, x * y) =? rational(w /% y, z /% x) label
								s"w=$w, x=$x, y=$y, z=$z: (w * x - y * z) /% (x * y) =? w /% y $s z /% x"
					nonZeros.map(force).permutations.map{ case Seq(w, x, y, z) => props4(w, x, y, z) }.reduce(_ && _)
			}
		}










	private def multiplicationProp(rational :(Ratio, Ratio) => Ratio, s :String) :Prop =
		forAll { (x1 :Int, x2 :Int, x3 :Int, x4 :Int) =>
			val nums = Seq(x1, x2, x3, x4)
			val nonZeros = nums.filterNot(_ == 0)
			nonZeros match {
				case Seq() =>
					(Zero =? rational(Zero, One) label s"0 $s 1 =? 0") && (Zero =? rational(One, Zero) label s"1 $s 0 =? 0")
				case Seq(Force(x)) =>
					(Ratio(x) =? rational(x, One) label s"x=$x: x $s 1 =? x") &&
						(1 /% x =? rational(One, 1 /% x) label s"x=$x: 1 /% x =? 1 $s 1 /% x")
				case Seq(Force(x), Force(y)) =>
					val prop = (x /% y =? rational(x, 1 /% y)) :| s"x=$x, y=$y: x /% y =? x $s 1 /% y"
					if (x * y == x.toLong * y.toLong)
						prop && (Ratio(x * y) =? rational(x, y)) :| s"x=$x, y=$y: x*y/1 =? x $s y" &&
							(Ratio.unit(x * y) =? rational(1 /% y, 1 /% x)) :| s"x=$x, y=$y: 1/x*y =? 1 /% y $s 1 /% x"
					else
						prop
				case Seq(Force(x), Force(y), Force(z)) =>
					def props(x :Int, y :Int, z :Int) :Prop =
						if (x * y == x.toLong * y)
							((x * y) /% z =? rational(x /% z, y)) :| s"x=$x, y=$y, z=$z: (x * y) /% z =? x /% z $s y" &&
								((x * y) /% z =? rational(y /% z, x)) :| s"x=$x, y=$y, z=$z: (x * y) /% z =? y /% z $s x" &&
								(z /% (x * y) =? rational(z /% x, 1 /% y)) :| s"x=$x, y=$y, z=$z: z /% (x * y) =? z /% x $s 1 /% y" &&
								(z /% (x * y) =? rational(1 /% x, z /% y)) :| s"x=$x; y=$y, z=$z: z /% (x * y) =? 1 /% x $s z /% y"
						else
							Prop.passed
					props(x, y, z) && props(x, z, y) && props(z, x, y) && props(z, y, x) && props(y, z, x) && props(y, x, z)
				case _ =>
					def props4(w :Int, x :Int, y :Int, z :Int) :Prop =
						if (w * x == w.toLong * x && y * z == y.toLong * z)
							((w * x) /% (y * z) =? rational(w /% y, x /% z)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) /% (y * z) =? w /% y $s x /% z" &&
								((w * x) /% (y * z) =? rational(w /% z, x /% y)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) /% (y * z) =? w /% z $s x /% y" &&
								((w * x) /% (y * z) =? rational(x /% y, w /% z)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) /% (y * z) =? x /% y $s w /% z" &&
								((w * x) /% (y * z) =? rational(x /% z, w /% y)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) /% (y * z) =? x /% z $s w /% y"
						else
							Prop.passed
					props4(force(x1), force(x2), force(x3), force(x4)) && props4(force(x1), force(x3), force(x2), force(x4)) &&
						props4(force(x3), force(x4), force(x1), force(x2)) && props4(force(x2), force(x4), force(x1), force(x3))

			}

		}






	private def divisionProp(rational :(Ratio, Ratio) => Ratio, s :String) :Prop =
		forAll { (x1 :Int, x2 :Int, x3 :Int, x4 :Int) =>
			val nums = Seq(x1, x2, x3, x4)
			val nonZeros = nums.filterNot(_ == 0)
			nonZeros match {
				case Seq() =>
					Zero =? rational(Zero, One) label s"0 $s 1 == 0"
				case Seq(Force(x)) =>
					(Ratio(x) =? rational(x, One) label s"x=$x: x/1 $s 1 ?= x/1") &&
						(1 /% x =? rational(One, x) label s"x=$x: 1 $s x = 1/x")
				case Seq(Force(x), Force(y)) =>
					val prop = (x /% y =? rational(x, y)) :| s"x=$x, y=$y: x /% y =? x/1 $s y/1"
					if (x * y == x.toLong * y.toLong)
						prop && (Ratio(x * y) =? rational(x, 1 /% y)) :| s"x=$x, y=$y: x*y/1 =? x $s 1/y" &&
							(Ratio(x * y) =? rational(y, 1 /% x)) :| s"x=$x, y=$y: x*y/1 =? y $s 1/x" &&
							(1 /% (x * y) =? rational(1 /% x, y)) :| s"x=$x, y=$y: 1/x*y =? 1/x $s y" &&
							(1 /% (x * y) =? rational(1 /% y, x)) :| s"x=$x, y=$y: 1/x*y =? 1/y $s x"
					else
						prop
				case Seq(Force(x), Force(y), Force(z)) =>
					def props(x :Int, y :Int, z :Int) :Prop =
						if (x * y == x.toLong * y)
							((x * y) /% z =? rational(x /% z, 1 /% y)) :| s"x=$x, y=$y, z=$z: (x * y) /% z =? x /% z $s 1 /% y" &&
								((x * y) /% z =? rational(y, z /% x)) :| s"x=$x, y=$y, z=$z: (x * y) /% z =? (y $s z /% x" &&
								(z /% (x * y) =? rational(z /% x, y)) :| s"x=$x, y=$y, z=$z: z /% (x * y) =? z /% x $s y" &&
								(z /% (x * y) =? rational(1 /% x, y /% z)) :| s"x=$x; y=$y, z=$z: z /% (x * y) =? 1 /% x $s y /% z"
						else
							Prop.passed
					props(x, y, z) && props(x, z, y) && props(z, x, y) && props(z, y, x) && props(y, z, x) && props(y, x, z)
				case _ =>
					def props4(w :Int, x :Int, y :Int, z :Int) :Prop =
						if (w * x == w.toLong * x && y * z == y.toLong * z)
							((w * x) /% (y * z) =? rational(w /% y, z /% x)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) /% (y * z) =? w /% y $s z /% x" &&
								((w * x) /% (y * z) =? rational(w /% z, y /% x)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) /% (y * z) =? w /% z $s y /% x" &&
								((w * x) /% (y * z) =? rational(x /% y, z /% w)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) /% (y * z) =? x /% y $s z /% w" &&
								((w * x) /% (y * z) =? rational(x /% z, y /% w)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) /% (y * z) =? x /% z $s y /% w"
						else
							Prop.passed
					props4(force(x1), force(x2), force(x3), force(x4)) && props4(force(x1), force(x3), force(x2), force(x4)) &&
						props4(force(x3), force(x4), force(x1), force(x2)) && props4(force(x2), force(x4), force(x1), force(x3))

			}

		}



}
