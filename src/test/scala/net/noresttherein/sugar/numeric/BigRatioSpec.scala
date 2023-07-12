package net.noresttherein.sugar.numeric

import org.scalacheck.{Prop, Properties, Test}
import org.scalacheck.Prop._
import net.noresttherein.sugar.numeric.BigRatio.{One, Zero, bigInt_%/, int_%/, long_%/}
import org.scalacheck.util.ConsoleReporter



object BigRatioSpec extends Properties("BigRatio") {

	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140)).withMinSuccessfulTests(500)

	val Tolerance = 0.000000000000001

	implicit class AlmostEquals(private val lh :Double) extends AnyVal {
		@inline def =~=(rh :Double) :Prop = {
			val tolerance = lh.abs * Tolerance max rh.abs * Tolerance
			(lh - rh).abs <= tolerance label s"$lh =~= $rh +-$tolerance"
		}
	}

	private def constructorProp(f :(BigInt, BigInt) => BigRatio)(n :BigInt, d :BigInt) :Prop = {
		if (d == 0)
			Prop(throws(classOf[ArithmeticException]) { f(n, d) }) :| "throws ArithmeticException on division by zero"
		else {
			val r = f(n, d)
			(r.toDouble =~= n.toDouble / d.toDouble label s"$r =~= $n / $d") &&
				(r.sign ?= n.sign.toInt * d.sign.toInt) :| s"($r).sign=${r.sign}; should be ${n.sign * d.sign} for $n / $d" &&
				(r.denominator.sign ?= 1) :| s"non positive denominator in $n / $d = $r" &&
				forAll { i :BigInt =>
					(i != 1 && i != -1 && i != 0) ==>
						Prop(r.numerator % i != 0 || r.denominator % i != 0) label
							s"$i divides ${r.numerator}, ${r.denominator}"
				}
		}
	}

	property("apply(BigInt, BigInt)") = forAll(constructorProp(BigRatio.apply) _)
	property("%/") = forAll(constructorProp(_ %/ _) _)
	property("apply(Int, Int)") = forAll { (n :Int, d :Int) =>
		constructorProp((n, d) => BigRatio(n.toInt, d.toInt))(n, d)
	}
	property("apply(Long, Long)") = forAll { (n :Long, d :Long) =>
		constructorProp((n, d) => BigRatio(n.toLong, d.toLong))(n, d)
	}


	private def shouldEqualSelf(num :BigInt, den :BigInt) :Prop = (num %/ den ?= num %/ den) && forAll { d :BigInt =>
		if (d == 0)
			Prop(true)
		else
			((num * d) %/ (den * d) ?= num %/ den) :| s"$num * $d / $den * $d ?= $num / $den"
	}

	property("==") = forAll { (a :BigInt, b :BigInt) =>
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
	}


	property("unary_-") = forAll { (a :BigInt, b :BigInt) =>
		if (b == 0)
			if (a==0) -Zero ?= Zero
			else (-(b %/ a) ?= -b %/ a) && (-(b %/ a) ?= (b %/ -a))
		else
			(-(a %/ b) ?= -a %/ b) && (-(a %/ b) ?= (a %/ -b))
	}

	property("abs") = forAll { (a :BigInt, b :BigInt) =>
		if (b == 0)
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



	private def additionProp(rational :(BigRatio, BigRatio) => BigRatio, s :String) :Prop =
		forAll { (x1 :BigInt, x2 :BigInt, x3 :BigInt, x4 :BigInt) =>
			val nums = Seq(x1, x2, x3, x4)
			val nonZeros = nums.filterNot(_ == 0)
			nonZeros match {
				case Seq() =>
					(Zero =? rational(Zero, Zero) label s"0 =? 0 $s 0") && (One =? rational(Zero, One) label s"1 =? 0+ 1")
				case Seq(x) =>
					(BigRatio(x) =? rational(x, 0) label s"x =? x $s 0") &&
						(BigRatio.unit(x) =? rational(1 %/ x, 0) label s"1 %/ x =? 1 %/ x $s 0")
				case Seq(x, y) =>
					(BigRatio(x + y) =? rational(x, y) label s"x=$x, y=$y: (x + y) %/ 1 =? x %/ 1 $s y %/ 1") &&
						((x * y + 1) %/ y =? rational(x, 1 %/ y) label s"x=$x, y=$y: (x*y + 1) %/ y =? x $s 1 %/ y") &&
							((x * y + 1) %/ x =? rational(y, 1 %/ x) label s"x=$x, y=$y: (x*y + 1) %/ x =? y $s 1 %/ x") &&
							((x + y) %/ (x * y) =? rational(1 %/ x, 1 %/ y) label s"x=$x, y=$y: (x + y) %/ x * y =? 1 %/ x $s 1 %/ y")

				case Seq(x, y, z) =>
					def props(x :BigInt, y :BigInt, z :BigInt) :Prop = {
						(BigRatio(y + z * x, x * y) =? rational(1 %/ x, z %/ y) label
                            s"x=$x, y=$y, z=$z: (y + z * x) %/ (x * y) =? 1 %/ x $s z %/ y") &&
							(BigRatio(x * y + z, y) =? rational(x, z %/ y) label
								s"x=$x, y=$y, z=$z: (x * y + z) %/ y =? x $s z %/ y")
					}
					props(x, y, z) && props(x, z, y) && props(z, x, y) && props(z, y, x) && props(y, z, x) && props(y, x, z)
				case _ =>
					def props4(w :BigInt, x :BigInt, y :BigInt, z :BigInt) :Prop =
						BigRatio(w * x + y * z, x * y) =? rational(w %/ y, z %/ x) label
							s"w=$w, x=$x, y=$y, z=$z: (w * x + y * z) %/ (x * y) =? w %/ y $s z %/ x"
					nonZeros.permutations.map{ case Seq(w, x, y, z) => props4(w, x, y, z) }.reduce(_ && _)
			}
		}



	private def subtractionProp(rational :(BigRatio, BigRatio) => BigRatio, s :String) :Prop =
		forAll { (x1 :BigInt, x2 :BigInt, x3 :BigInt, x4 :BigInt) =>
			val nums = Seq(x1, x2, x3, x4)
			val nonZeros = nums.filterNot(_ == 0)
			nonZeros match {
				case Seq() =>
					(Zero =? rational(Zero, Zero) label s"0 =? 0 $s 0") && (One =? rational(One, Zero) label s"1 =? 0+ 1")
				case Seq(x) =>
					(BigRatio(x) =? rational(x, 0) label s"x =? x $s 0") && (BigRatio.unit(x) =? rational(1 %/ x, 0) label s"1 %/ x =? 1 %/ x $s 0") &&
					(BigRatio(-x) =? rational(0, x) label s"-x =? 0 $s x") && (BigRatio.unit(-x) =? rational(0, 1 %/ x) label s"-1 %/ x =? 0 $s 1 %/ x")
				case Seq(x, y) => all(
						BigRatio(x - y) =? rational(x, y) label s"x=$x, y=$y: (x - y) %/ 1 =? x %/ 1 $s y %/ 1",
						(x * y - 1) %/ y =? rational(x, 1 %/ y) label s"x=$x, y=$y: (x*y - 1) %/ y =? x $s 1 %/ y",
						(1 - x * y) %/ x =? rational(1 %/ x, y) label s"x=$x, y=$y: (1 - x*y) %/ x =? 1 %/ x $s y",
						(x - y) %/ (x * y) =? rational(1 %/ y, 1 %/ x) label s"x=$x, y=$y: (x - y) %/ x * y =? 1 %/ y $s 1 %/ x"
					)
				case Seq(x, y, z) =>
					def props(x :BigInt, y :BigInt, z :BigInt) :Prop = all(
						BigRatio(y - z * x, x * y) =? rational(1 %/ x, z %/ y) label
							s"x=$x, y=$y, z=$z: (y - z * x) %/ (x * y) =? 1 %/ x $s z %/ y",
						BigRatio(x * y - z, y) =? rational(x, z %/ y) label s"x=$x, y=$y, z=$z: (x * y - z) %/ y =? x $s z %/ y"
					)
					props(x, y, z) && props(x, z, y) && props(z, x, y) && props(z, y, x) && props(y, z, x) && props(y, x, z)
				case _ =>
					def props4(w :BigInt, x :BigInt, y :BigInt, z :BigInt) :Prop =
						BigRatio(w * x - y * z, x * y) =? rational(w %/ y, z %/ x) label
							s"w=$w, x=$x, y=$y, z=$z: (w * x - y * z) %/ (x * y) =? w %/ y $s z %/ x"
					nonZeros.permutations.map{ case Seq(w, x, y, z) => props4(w, x, y, z) }.reduce(_ && _)
			}
		}



	private def multiplicationProp(rational :(BigRatio, BigRatio) => BigRatio, s :String) :Prop =
		forAll { (x1 :BigInt, x2 :BigInt, x3 :BigInt, x4 :BigInt) =>
			val nums = Seq(x1, x2, x3, x4)
			val nonZeros = nums.filterNot(_ == 0)
			nonZeros match {
				case Seq() =>
					(Zero =? rational(Zero, One) label s"0 $s 1 =? 0") && (Zero =? rational(One, Zero) label s"1 $s 0 =? 0")
				case Seq(x) =>
					(BigRatio(x) =? rational(x, One) label s"x=$x: x $s 1 =? x") &&
						(1 %/ x =? rational(One, 1 %/ x) label s"x=$x: 1 %/ x =? 1 $s 1 %/ x")
				case Seq(x, y) =>
					val prop = (x %/ y =? rational(x, 1 %/ y)) :| s"x=$x, y=$y: x %/ y =? x $s 1 %/ y"
					if (x * y == x.toLong * y.toLong)
						prop && (BigRatio(x * y) =? rational(x, y)) :| s"x=$x, y=$y: x*y/1 =? x $s y" &&
							(BigRatio.unit(x * y) =? rational(1 %/ y, 1 %/ x)) :| s"x=$x, y=$y: 1/x*y =? 1 %/ y $s 1 %/ x"
					else
						prop
				case Seq(x, y, z) =>
					def props(x :BigInt, y :BigInt, z :BigInt) :Prop =
						if (x * y == x.toLong * y)
							((x * y) %/ z =? rational(x %/ z, y)) :| s"x=$x, y=$y, z=$z: (x * y) %/ z =? x %/ z $s y" &&
								((x * y) %/ z =? rational(y %/ z, x)) :| s"x=$x, y=$y, z=$z: (x * y) %/ z =? y %/ z $s x" &&
								(z %/ (x * y) =? rational(z %/ x, 1 %/ y)) :| s"x=$x, y=$y, z=$z: z %/ (x * y) =? z %/ x $s 1 %/ y" &&
								(z %/ (x * y) =? rational(1 %/ x, z %/ y)) :| s"x=$x; y=$y, z=$z: z %/ (x * y) =? 1 %/ x $s z %/ y"
						else
							Prop.passed
					props(x, y, z) && props(x, z, y) && props(z, x, y) && props(z, y, x) && props(y, z, x) && props(y, x, z)
				case _ =>
					def props4(w :BigInt, x :BigInt, y :BigInt, z :BigInt) :Prop =
						((w * x) %/ (y * z) =? rational(w %/ y, x %/ z)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) %/ (y * z) =? w %/ y $s x %/ z" &&
							((w * x) %/ (y * z) =? rational(w %/ z, x %/ y)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) %/ (y * z) =? w %/ z $s x %/ y" &&
							((w * x) %/ (y * z) =? rational(x %/ y, w %/ z)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) %/ (y * z) =? x %/ y $s w %/ z" &&
							((w * x) %/ (y * z) =? rational(x %/ z, w %/ y)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) %/ (y * z) =? x %/ z $s w %/ y"
					props4(x1, x2, x3, x4) && props4(x1, x3, x2, x4) && props4(x3, x4, x1, x2) && props4(x2, x4, x1, x3)
			}
		}



	private def divisionProp(rational :(BigRatio, BigRatio) => BigRatio, s :String) :Prop =
		forAll { (x1 :BigInt, x2 :BigInt, x3 :BigInt, x4 :BigInt) =>
			val nums = Seq(x1, x2, x3, x4)
			val nonZeros = nums.filterNot(_ == 0)
			nonZeros match {
				case Seq() =>
					Zero =? rational(Zero, One) label s"0 $s 1 == 0"
				case Seq(x) =>
					(BigRatio(x) =? rational(x, One) label s"x=$x: x/1 $s 1 ?= x/1") &&
						(1 %/ x =? rational(One, x) label s"x=$x: 1 $s x = 1/x")
				case Seq(x, y) =>
					((x %/ y =? rational(x, y)) :| s"x=$x, y=$y: x %/ y =? x/1 $s y/1") &&
						(BigRatio(x * y) =? rational(x, 1 %/ y)) :| s"x=$x, y=$y: x*y/1 =? x $s 1/y" &&
						(BigRatio(x * y) =? rational(y, 1 %/ x)) :| s"x=$x, y=$y: x*y/1 =? y $s 1/x" &&
						(1 %/ (x * y) =? rational(1 %/ x, y)) :| s"x=$x, y=$y: 1/x*y =? 1/x $s y" &&
						(1 %/ (x * y) =? rational(1 %/ y, x)) :| s"x=$x, y=$y: 1/x*y =? 1/y $s x"
				case Seq(x, y, z) =>
					def props(x :BigInt, y :BigInt, z :BigInt) :Prop =
						((x * y) %/ z =? rational(x %/ z, 1 %/ y)) :| s"x=$x, y=$y, z=$z: (x * y) %/ z =? x %/ z $s 1 %/ y" &&
							((x * y) %/ z =? rational(y, z %/ x)) :| s"x=$x, y=$y, z=$z: (x * y) %/ z =? (y $s z %/ x" &&
							(z %/ (x * y) =? rational(z %/ x, y)) :| s"x=$x, y=$y, z=$z: z %/ (x * y) =? z %/ x $s y" &&
							(z %/ (x * y) =? rational(1 %/ x, y %/ z)) :| s"x=$x; y=$y, z=$z: z %/ (x * y) =? 1 %/ x $s y %/ z"
					props(x, y, z) && props(x, z, y) && props(z, x, y) && props(z, y, x) && props(y, z, x) && props(y, x, z)
				case _ =>
					def props4(w :BigInt, x :BigInt, y :BigInt, z :BigInt) :Prop =
						((w * x) %/ (y * z) =? rational(w %/ y, z %/ x)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) %/ (y * z) =? w %/ y $s z %/ x" &&
							((w * x) %/ (y * z) =? rational(w %/ z, y %/ x)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) %/ (y * z) =? w %/ z $s y %/ x" &&
							((w * x) %/ (y * z) =? rational(x %/ y, z %/ w)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) %/ (y * z) =? x %/ y $s z %/ w" &&
							((w * x) %/ (y * z) =? rational(x %/ z, y %/ w)) :| s"w=$w, x=$x, y=$y, z=$z: (w * x) %/ (y * z) =? x %/ z $s y %/ w"
					props4(x1, x2, x3, x4) && props4(x1, x3, x2, x4) && props4(x3, x4, x1, x2) && props4(x2, x4, x1, x3)

		}
	}
}

