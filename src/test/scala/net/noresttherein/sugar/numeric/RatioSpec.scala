package net.noresttherein.sugar.numeric

import net.noresttherein.sugar.numeric.Ratio.{ratio_%/, One, Zero}
import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop._




object RatioSpec extends Properties("Ratio") {

	/* All tests done on Int values to avoid overflows and underflows. */

	private def constructorProp(f :(Int, Int) => Ratio) :Prop = forAll { (n :Int, d :Int) =>
		if (d == 0)
			Prop(throws(classOf[ArithmeticException]) { f(n, d) }) :| "throws ArithmeticException on division by zero"
		else {
			val r = f(n, d)
			(r.toDouble ?= n.toDouble / d) &&
				(r.sign ?= n.sign * d.sign) :| s"($r).sign=${r.sign}; should be ${n.sign * d.sign} for $n / $d" &&
				forAll { i :Int => Prop(i != 0) ==>
					(Prop(r.numerator % i == 0 && r.denominator % i == 0) ==> (i == 1  || i == -1) label
						s"$i divides ${r.numerator}, ${r.denominator}")
				}
		}
	}

	property("apply(Int, Int)") = constructorProp(Ratio(_, _))
	property("%/") = constructorProp(_ %/ _)


	private def shouldEqualSelf(num :Long, den :Long) :Prop = (num %/ den ?= num %/ den) && forAll { d :Int =>
		if (d == 0 || d == Long.MinValue || num * d / d != num || den * d / d != den)
			Prop(true) //overflow
		else
			((num * d) %/ (den * d) ?= num %/ den) :| s"$num * $d / $den * $d ?= $num / $den"
	}

	property("==") = forAll { (a :Long, b :Long) =>
		if (a != Long.MinValue && b != Long.MinValue)
			if (b == 0)
				if (a==0) Prop(true)
				else shouldEqualSelf(b, a)
			else
			if (a==0)
				shouldEqualSelf(a, b)
			else if (a.abs == b.abs)
                shouldEqualSelf(a, b) && (a %/ b ?= b %/ a)
			else
				shouldEqualSelf(a, b) && Prop(a %/ b != b %/ a) :| s"$a / $b != $b / $a"
		else
			Prop.passed
	}


	property("unary_-") = forAll { (a :Long, b :Long) =>
		if (a == Long.MinValue || b == Long.MinValue)
			Prop.passed
		else if (b == 0)
			if (a==0 || a == Long.MinValue) -Zero ?= Zero
			else (-(b %/ a) ?= -b %/ a) && (-(b %/ a) ?= (b %/ -a))
		else
			(-(a %/ b) ?= -a %/ b) && (-(a %/ b) ?= (a %/ -b))
	}

	property("abs") = forAll { (a :Long, b :Long) =>
		if (b == 0 || a == Long.MinValue || b==Long.MinValue)
			Zero.abs =? Zero && One.abs =? One && One =? (-One).abs
		else if (a.sign * b.sign >= 0)
			(a %/ b).abs ?= (a %/ b)
		else
			(a %/ b).abs ?= -(a %/ b)
	}

	property("+") = additionProp(_ + _, "+")

	property("-") = subtractionProp(_ - _, "-")

	property("*") = multiplicationProp(_ * _, "*")

	property("/") = divisionProp(_ / _, "/")



	private def additionProp(rational :(Ratio, Ratio) => Ratio, s :String) :Prop =
		forAll { (x1 :Int, x2 :Int, x3 :Int, x4 :Int) =>
			val nums = Seq[Long](x1, x2, x3, x4)
			val nonZeros = nums.filterNot(_ == 0)
			nonZeros match {
				case Seq() =>
					(Zero =? rational(Zero, Zero) label s"0 =? 0 $s 0") && (One =? rational(Zero, One) label s"1 =? 0+ 1")
				case Seq(x) =>
					(Ratio(x) =? rational(x, 0) label s"x =? x $s 0") && (Ratio.unit(x) =? rational(1 %/ x, 0) label s"1 %/ x =? 1 %/ x $s 0")
				case Seq(x, y) =>
					(Ratio(x + y) =? rational(x, y) label s"x=$x, y=$y: (x + y) %/ 1 =? x %/ 1 $s y %/ 1") &&
					((x * y + 1) %/ y =? rational(x, 1 %/ y) label s"x=$x, y=$y: (x*y + 1) %/ y =? x $s 1 %/ y") &&
					((x * y + 1) %/ x =? rational(y, 1 %/ x) label s"x=$x, y=$y: (x*y + 1) %/ x =? y $s 1 %/ x") &&
					((x + y) %/ (x * y) =? rational(1 %/ x, 1 %/ y) label s"x=$x, y=$y: (x + y) %/ x * y =? 1 %/ x $s 1 %/ y")
				case Seq(x, y, z) =>
					def props(x :Long, y :Long, z :Long) :Prop =
						(Ratio(y + z * x, x * y) =? rational(1 %/ x, z %/ y) label
							s"x=$x, y=$y, z=$z: (y + z * x) %/ (x * y) =? 1 %/ x $s z %/ y") &&
						(Ratio(x * y + z, y) =? rational(x, z %/ y) label s"x=$x, y=$y, z=$z: (x * y + z) %/ y =? x $s z %/ y")
					props(x, y, z) && props(x, z, y) && props(z, x, y) && props(z, y, x) && props(y, z, x) && props(y, x, z)
				case _ =>
					nonZeros.permutations.map{ case Seq(w, x, y, z) =>
						Ratio(w * x + y * z, x * y) =? rational(w %/ y, z %/ x) label
							s"w=$w, x=$x, y=$y, z=$z: (w * x + y * z) %/ (x * y) =? w %/ y $s z %/ x"
					}.reduce(_ && _)
			}
		}



	private def subtractionProp(rational :(Ratio, Ratio) => Ratio, s :String) :Prop =
		forAll { (x1 :Int, x2 :Int, x3 :Int, x4 :Int) =>
			val nums = Seq[Long](x1, x2, x3, x4)
			val nonZeros = nums.filterNot(_ == 0)
			nonZeros match {
				case Seq() =>
					(Zero =? rational(Zero, Zero) label s"0 =? 0 $s 0") && (One =? rational(One, Zero) label s"1 =? 0+ 1")
				case Seq(x) =>
					(Ratio(x) =? rational(x, 0) label s"x =? x $s 0") &&
						(Ratio.unit(x) =? rational(1 %/ x, 0) label s"1 %/ x =? 1 %/ x $s 0") &&
						(Ratio(-x) =? rational(0, x) label s"-x =? 0 $s x") &&
						(Ratio.unit(-x) =? rational(0, 1 %/ x) label s"-1 %/ x =? 0 $s 1 %/ x")
				case Seq(x, y) => all(
					Ratio(x - y) =? rational(x, y) label s"x=$x, y=$y: (x - y) %/ 1 =? x %/ 1 $s y %/ 1",
					(x * y - 1) %/ y =? rational(x, 1 %/ y) label s"x=$x, y=$y: (x*y - 1) %/ y =? x $s 1 %/ y",
					(1 - x * y) %/ x =? rational(1 %/ x, y) label s"x=$x, y=$y: (1 - x*y) %/ x =? 1 %/ x $s y",
					(x - y) %/ (x * y) =? rational(1 %/ y, 1 %/ x) label s"x=$x, y=$y: (x - y) %/ x * y =? 1 %/ y $s 1 %/ x"
                )
				case Seq(x, y, z) =>
					def props(x :Long, y :Long, z :Long) :Prop = all(
						Ratio(y - z * x, x * y) =? rational(1 %/ x, z %/ y) label
							s"x=$x, y=$y, z=$z: (y - z * x) %/ (x * y) =? 1 %/ x $s z %/ y",
						Ratio(x * y - z, y) =? rational(x, z %/ y) label s"x=$x, y=$y, z=$z: (x * y - z) %/ y =? x $s z %/ y"
					)
					props(x, y, z) && props(x, z, y) && props(z, x, y) && props(z, y, x) && props(y, z, x) && props(y, x, z)
				case _ =>
					nonZeros.permutations.map{ case Seq(w, x, y, z) =>
						Ratio(w * x - y * z, x * y) =? rational(w %/ y, z %/ x) label
							s"w=$w, x=$x, y=$y, z=$z: (w * x - y * z) %/ (x * y) =? w %/ y $s z %/ x"
					}.reduce(_ && _)
			}
		}



	private def multiplicationProp(rational :(Ratio, Ratio) => Ratio, s :String) :Prop =
		forAll { (x1 :Int, x2 :Int, x3 :Int, x4 :Int) =>
			val nums = Seq[Long](x1, x2, x3, x4)
			val nonZeros = nums.filterNot(_ == 0)
			nonZeros match {
				case Seq() =>
					(Zero =? rational(Zero, One) label s"0 $s 1 =? 0") && (Zero =? rational(One, Zero) label s"1 $s 0 =? 0")
				case Seq(x) =>
					(Ratio(x) =? rational(x, One) label s"x=$x: x $s 1 =? x") &&
						(1 %/ x =? rational(One, 1 %/ x) label s"x=$x: 1 %/ x =? 1 $s 1 %/ x")
				case Seq(x, y) => all(
					(x %/ y =? rational(x, 1 %/ y)) :| s"x=$x, y=$y: x %/ y =? x $s 1 %/ y",
					(Ratio(x * y) =? rational(x, y)) :| s"x=$x, y=$y: x*y/1 =? x $s y",
					(Ratio.unit(x * y) =? rational(1 %/ y, 1 %/ x)) :| s"x=$x, y=$y: 1/x*y =? 1 %/ y $s 1 %/ x"
				)
				case Seq(x, y, z) =>
					def props(x :Long, y :Long, z :Long) :Prop = all(
						((x * y) %/ z =? rational(x %/ z, y)) :| s"x=$x, y=$y, z=$z: (x * y) %/ z =? x %/ z $s y",
						((x * y) %/ z =? rational(y %/ z, x)) :| s"x=$x, y=$y, z=$z: (x * y) %/ z =? y %/ z $s x",
						(z %/ (x * y) =? rational(z %/ x, 1 %/ y)) :| s"x=$x, y=$y, z=$z: z %/ (x * y) =? z %/ x $s 1 %/ y",
						(z %/ (x * y) =? rational(1 %/ x, z %/ y)) :| s"x=$x; y=$y, z=$z: z %/ (x * y) =? 1 %/ x $s z %/ y"
					)
					props(x, y, z) && props(x, z, y) && props(z, x, y) && props(z, y, x) && props(y, z, x) && props(y, x, z)
				case _ =>
					def props4(w :Int, x :Int, y :Int, z :Int) :Prop =
						if (w * x == w.toLong * x && y * z == y.toLong * z)
							((w * x) %/ (y * z) =? rational(w %/ y, x %/ z)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) %/ (y * z) =? w %/ y $s x %/ z" &&
								((w * x) %/ (y * z) =? rational(w %/ z, x %/ y)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) %/ (y * z) =? w %/ z $s x %/ y" &&
								((w * x) %/ (y * z) =? rational(x %/ y, w %/ z)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) %/ (y * z) =? x %/ y $s w %/ z" &&
								((w * x) %/ (y * z) =? rational(x %/ z, w %/ y)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) %/ (y * z) =? x %/ z $s w %/ y"
						else
							Prop.passed
					nums.permutations.map { case Seq(w, x, y, z) =>
						((w * x) %/ (y * z) =? rational(w %/ y, x %/ z)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) %/ (y * z) =? w %/ y $s x %/ z" &&
							((w * x) %/ (y * z) =? rational(w %/ z, x %/ y)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) %/ (y * z) =? w %/ z $s x %/ y" &&
							((w * x) %/ (y * z) =? rational(x %/ y, w %/ z)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) %/ (y * z) =? x %/ y $s w %/ z" &&
							((w * x) %/ (y * z) =? rational(x %/ z, w %/ y)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) %/ (y * z) =? x %/ z $s w %/ y"
					}.reduce(_ && _)

			}

		}



	private def divisionProp(rational :(Ratio, Ratio) => Ratio, s :String) :Prop =
		forAll { (x1 :Int, x2 :Int, x3 :Int, x4 :Int) =>
			val nums = Seq[Long](x1, x2, x3, x4)
			val nonZeros = nums.filterNot(_ == 0)
			nonZeros match {
				case Seq() =>
					Zero =? rational(Zero, One) label s"0 $s 1 == 0"
				case Seq(x) =>
					(Ratio(x) =? rational(x, One) label s"x=$x: x/1 $s 1 ?= x/1") &&
						(1 %/ x =? rational(One, x) label s"x=$x: 1 $s x = 1/x")
				case Seq(x, y) => all(
					(x %/ y =? rational(x, y)) :| s"x=$x, y=$y: x %/ y =? x/1 $s y/1",
					(Ratio(x * y) =? rational(x, 1 %/ y)) :| s"x=$x, y=$y: x*y/1 =? x $s 1/y",
					(Ratio(x * y) =? rational(y, 1 %/ x)) :| s"x=$x, y=$y: x*y/1 =? y $s 1/x",
					(1 %/ (x * y) =? rational(1 %/ x, y)) :| s"x=$x, y=$y: 1/x*y =? 1/x $s y",
					(1 %/ (x * y) =? rational(1 %/ y, x)) :| s"x=$x, y=$y: 1/x*y =? 1/y $s x"
				)
				case Seq(x, y, z) =>
					def props(x :Long, y :Long, z :Long) :Prop = all(
						((x * y) %/ z =? rational(x %/ z, 1 %/ y)) :| s"x=$x, y=$y, z=$z: (x * y) %/ z =? x %/ z $s 1 %/ y",
						((x * y) %/ z =? rational(y, z %/ x)) :| s"x=$x, y=$y, z=$z: (x * y) %/ z =? (y $s z %/ x",
						(z %/ (x * y) =? rational(z %/ x, y)) :| s"x=$x, y=$y, z=$z: z %/ (x * y) =? z %/ x $s y",
						(z %/ (x * y) =? rational(1 %/ x, y %/ z)) :| s"x=$x; y=$y, z=$z: z %/ (x * y) =? 1 %/ x $s y %/ z"
					)
					props(x, y, z) && props(x, z, y) && props(z, x, y) && props(z, y, x) && props(y, z, x) && props(y, x, z)
				case _ =>
					nonZeros.permutations.map { case Seq(w, x, y, z) =>
						((w * x) %/ (y * z) =? rational(w %/ y, z %/ x)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) %/ (y * z) =? w %/ y $s z %/ x" &&
						((w * x) %/ (y * z) =? rational(w %/ z, y %/ x)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) %/ (y * z) =? w %/ z $s y %/ x" &&
						((w * x) %/ (y * z) =? rational(x %/ y, z %/ w)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) %/ (y * z) =? x %/ y $s z %/ w" &&
						((w * x) %/ (y * z) =? rational(x %/ z, y %/ w)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) %/ (y * z) =? x %/ z $s y %/ w"
					}.reduce(_ && _)
			}

		}

}
