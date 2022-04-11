package net.noresttherein.slang.numeric

import java.math.{BigInteger, MathContext, RoundingMode, BigDecimal => JavaBigDecimal}
import java.math.MathContext.{DECIMAL128, DECIMAL32, DECIMAL64, UNLIMITED}
import java.math.RoundingMode.{DOWN, HALF_EVEN, UNNECESSARY}

import scala.util.Try

import net.noresttherein.slang.numeric.Decimal64.Round.{Extended, Standard}
import net.noresttherein.slang.numeric.Decimal64.{MaxPrecision, Precision, Round}
import org.scalacheck.Prop.{all, forAll, propBoolean, throws, AnyOperators}
import org.scalacheck.{Arbitrary, Prop, Properties, Shrink, Test}
import org.scalacheck.util.ConsoleReporter

//implicits
import net.noresttherein.slang.extensions.{bigDecimalIsFractional, bigIntegerIsIntegral}



object Decimal64Spec extends Properties("Decimal64") {

	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140)).withMinSuccessfulTests(1000)

	import Ordering.Implicits.infixOrderingOps

	private val MaxUnscaled =  BigInteger.valueOf(Decimal64.MaxUnscaled)
	private val MinUnscaled = BigInteger.valueOf(Decimal64.MinUnscaled)

	private val MaxLong = BigInteger.valueOf(Long.MaxValue)
	private val MinLong = BigInteger.valueOf(Long.MinValue)
//	val MaxLong = java.math.BigDecimal.valueOf(Long.MaxValue) //BigInteger.valueOf(Long.MaxValue)
//	val MinLong = java.math.BigDecimal.valueOf(Long.MinValue) //BigInteger.valueOf(Long.MinValue)

	def maxPrecision(value :JavaBigDecimal, rounding :RoundingMode) :MathContext = {
		val max = new MathContext(MaxPrecision, rounding)
		val rounded = value.round(max)
		val normalized = rounded.movePointRight(rounded.scale + MaxPrecision - rounded.precision).toBigIntegerExact
//		val normalized = rounded.movePointRight(MaxPrecision - rounded.scale).unscaledValue
		if (normalized < MinUnscaled || normalized > MaxUnscaled)
			new MathContext(Decimal64.Precision, rounding)
		else
			new MathContext(MaxPrecision, rounding)
	}
	def maxPrecision(value :JavaBigDecimal) :MathContext = maxPrecision(value, HALF_EVEN)
	def maxPrecision(value :Long, rounding :RoundingMode = HALF_EVEN) :MathContext =
		maxPrecision(JavaBigDecimal.valueOf(value), rounding)

	def bigDecimal(decimal :Decimal64) :JavaBigDecimal = JavaBigDecimal.valueOf(decimal.unscaled, decimal.scale)

	implicit val ArbitraryDecimal64 = Arbitrary(Arbitrary.arbitrary[Long].map { long =>
		Decimal64(long >> 8, (long & 0xffL).toByte)
	})
	implicit val ShrinkDecimal64 :Shrink[Decimal64] = Shrink { n :Decimal64 =>
		try { Shrink.shrinkFractional[Decimal64].shrink(n) } catch {
			case _ :ArithmeticException => Stream.empty
		}
	}
//	implicit val ctx = Standard



	private def trailingZeros(long :Long, acc :Int = 0) :Int =
		if (long == 0) 0
		else if (long % 10 == 0) trailingZeros(long / 10, acc + 1)
		else acc

	private def guard(significand :Long, scale :Int)(test :JavaBigDecimal => Prop) :Prop =
		guard(JavaBigDecimal.valueOf(significand, scale))(test)

	private def guard(expect :JavaBigDecimal)(test :JavaBigDecimal => Prop) :Prop = {
		val normalized = expect.stripTrailingZeros
		val unscaled = normalized.unscaledValue; val scale = normalized.scale
		if (unscaled > MaxUnscaled || unscaled < MinUnscaled) {
			if (throws(classOf[ArithmeticException])(test(normalized)))
				Prop(true)
			else
				(test(normalized) && Prop(false)) :| s"throws[ArithmeticException] when significand $unscaled out of range"
		} else {
			val maxTrailingZeros = maxPrecision(unscaled.longValueExact).getPrecision - normalized.precision
			if (unscaled != BigInteger.ZERO && (scale > Byte.MaxValue || scale < Byte.MinValue - maxTrailingZeros))
				if (throws(classOf[ArithmeticException])(test(normalized)))
					Prop(true)
				else
					(test(normalized) && Prop(false)) :| s"throws[ArithmeticException] when scale $scale out of range"
			else try {
				test(normalized) :| "expect=" + expect
			} catch {
				case e :Exception => Prop.exception(e) :|
					"expect=" + expect + "; min scale=" + (Byte.MinValue - maxTrailingZeros)
			}
		}
	}

	private def equivalent(expect: => JavaBigDecimal)(test: => Decimal64) :Prop =
		if (throws(classOf[ArithmeticException])(expect))
			throws(classOf[ArithmeticException])(test) :| "throws[ArithmeticException]"
		else {
			val result = expect
			val normalized = result.stripTrailingZeros
			val unscaled = normalized.unscaledValue; val scale = normalized.scale
			if (unscaled > MaxUnscaled || unscaled < MinUnscaled)
				if (throws(classOf[ArithmeticException])(test))
					Prop(true)
				else {
					val got = test
					Prop(false) :| s"expected throws[ArithmeticException] when unscaled $unscaled out o range, got $got"
				}
			else {
				val maxTrailingZeros = maxPrecision(unscaled.longValueExact).getPrecision - normalized.precision
				if (unscaled != BigInteger.ZERO && (scale > Byte.MaxValue || scale < Byte.MinValue - maxTrailingZeros))
					if (throws(classOf[ArithmeticException])(test))
						Prop(true)
					else {
						val got = test
						Prop(false) :| s"expected throws[ArithmeticException] when scale $scale out of range, got $got"
					}
				else try {
					val decimal = test
					((bigDecimal(decimal) compareTo result) ?= 0) :| s"$decimal ?= $result"
				} catch {
					case e :Exception => Prop.exception(e) :| s"Failed to replicate $result as a Decimal64: $e"
				}
			}
		}


	private def binaryProp(expect :(JavaBigDecimal, JavaBigDecimal) => JavaBigDecimal)
	                      (subject :(Decimal64, Decimal64) => Decimal64)(x :Decimal64, y :Decimal64) :Prop =
	{
		val bigX = bigDecimal(x); val bigY = bigDecimal(y)
		Try {
			expect(bigX, bigY) :JavaBigDecimal
		}.map { big =>
			guard(big) { bd =>
				val result = subject(x, y)
				val big = bigDecimal(result)
				((big compareTo bd) ?= 0) :| s"Expected $bd; got $result ($big)"
			}
		}.recover {
			case e :ArithmeticException =>
				throws(classOf[ArithmeticException])(subject(x, y)) :|
					"throws[ArithmeticException] when reference expression throws " + e
		}.get
	}
	private def testBinary(name :String)(expect :(JavaBigDecimal, JavaBigDecimal) => JavaBigDecimal)
	                      (subject :(Decimal64, Decimal64) => Decimal64) :Unit =
		property(name) = forAll { (x :Decimal64, y :Decimal64) =>
			val bigX = bigDecimal(x); val bigY = bigDecimal(y)
			val big = expect(bigX, bigY)
			guard(big) { bd => bigDecimal(subject(x, y)) ?= bd }
		}


	/** * * * * * * * * * * * Properties of the companion object * * * * * * * * * * * * */

	/* * * * * * * * * * * * * * * Round methods * * * * * * * * * * * * * * * * * * * */

	property("Round.maxPrecision(Long, RoundingMode)") = forAll { significand :Long =>
		all(RoundingMode.values().map { rounding =>
			(if (rounding == UNNECESSARY)
				if (throws(classOf[ArithmeticException])(maxPrecision(significand, rounding)))
					if (throws(classOf[ArithmeticException])(Round.maxPrecision(significand, rounding)))
						Prop.passed
					else
						Prop.falsified :|
							"Expected a thrown ArithmeticException, but got " + Round.maxPrecision(significand, rounding)
				else
					Round(Round.maxPrecision(significand, rounding), rounding) ?= maxPrecision(significand, rounding)
			else
				Round(Round.maxPrecision(significand, rounding), rounding) ?= maxPrecision(significand, rounding)
			) :| "Rounding " + rounding
		}:_*)
	}

	property("Round.maxPrecision(JavaBigDecimal, RoundingMode)") = forAll { value :BigDecimal =>
		all(RoundingMode.values().map{ rounding =>
			val big = value.bigDecimal
			(if (rounding == UNNECESSARY)
				if (throws(classOf[ArithmeticException])(maxPrecision(big, rounding)))
					if (throws(classOf[ArithmeticException])(Round.maxPrecision(big, rounding)))
						Prop.passed
					else
						Prop.falsified :| "Expected a thrown ArithmeticException, but got " +
							Round.maxPrecision(big, rounding)
				else
					Round(Round.maxPrecision(big, rounding), rounding) ?= maxPrecision(big, rounding)
			else
				Round(Round.maxPrecision(big, rounding), rounding) ?= maxPrecision(big, rounding)
			) :| "Rounding " + rounding
		} :_*)
	}

	/* * * * * * * * * * * * * * * Factory methods * * * * * * * * * * * * * * * * * * * */

	property("Decimal64.apply(Int)") = forAll { value :Int =>
		val dec = Decimal64(value)
		val got = BigDecimal(dec.unscaled, dec.scale)
		got ?= BigDecimal(value)
	}
	property("Decimal64.apply(Long)") = forAll { value :Long =>
		val significand = { var n = value; while (n != 0 && n % 10 == 0) n /= 10; n  }
		if (Decimal64.MinUnscaled <= significand && significand <= Decimal64.MaxUnscaled) {
			val expect = BigDecimal(value, Extended)
			val dec = Decimal64(value)
			val got = BigDecimal(dec.unscaled, dec.scale)
			(got ?= expect) :| s"Decimal64($value) == $dec == $got ?= $expect"
		} else {
			val unscaled = value >> Decimal64.ScaleBits
			val scale = (value.toInt & 0xff).toByte
			val expect = BigDecimal(unscaled, scale, maxPrecision(unscaled))
			val decimal = Decimal64(unscaled, scale)
			val got = decimal.toBigDecimal
			(got ?= expect) :| s"Decimal64($value) == $decimal == $got ?= $expect" &&
			(throws(classOf[ArithmeticException])(Decimal64(value)) :| "throw[ArithmeticException]")
		}
	}
	property("Decimal64.apply(Long, Int)") = forAll { (significand :Long, scale :Short) =>
		guard(significand, scale) { expect =>
			val dec = Decimal64(significand, scale)
			val result = bigDecimal(dec)
			(result ?= expect) :| s"$dec == $result ?= $expect"
		}
	}
	property("Decimal64.apply(Long, Int, RoundingMode)") = forAll { (significand :Long, scale :Short) =>
		all(RoundingMode.values().map { rounding =>
			def ctx = maxPrecision(significand, rounding)
			equivalent(JavaBigDecimal.valueOf(significand, scale).round(ctx)) {
				Decimal64(significand, scale, rounding)
			} :| s"Decimal64($significand, $scale, $rounding) ?= BigDecimal($significand, $scale).round($rounding)"
		}:_*)
	}
	property("Decimal64.apply(Long, Int, MathContext)") = forAll { (significand :Long, scale :Short) =>
		val expect = JavaBigDecimal.valueOf(significand, scale)
		all(
			Seq(DECIMAL32, DECIMAL64, DECIMAL128).map { ctx =>
				equivalent(expect.round(ctx)) {
					Decimal64(significand, scale.toInt, ctx)
				} :| s"Decimal64($significand, $scale, $ctx) ?= ${expect.round(ctx)}"
			} :_*
		)
	}
	property("Decimal64.apply(JavaBigDecimal)") = forAll { decimal :BigDecimal =>
		equivalent(decimal.bigDecimal)(Decimal64(decimal))
	}
	property("Decimal64.apply(BigDecimal)") = forAll { decimal :BigDecimal =>
		equivalent(decimal.bigDecimal)(Decimal64(decimal))
	}
	property("Decimal64.apply(JavaBigDecimal, RoundingMode)") = forAll { decimal :BigDecimal =>
		all(RoundingMode.values().map { rounding =>
			def ctx = maxPrecision(decimal.bigDecimal, rounding)
			roundingProperty(decimal.bigDecimal.round(ctx))(Decimal64(decimal.bigDecimal, rounding)) :|
				"Rounding mode: " + rounding
		} :_*)
	}
	property("Decimal64.apply(BigDecimal, RoundingMode)") = forAll { decimal :BigDecimal =>
		all(RoundingMode.values().map { rounding =>
			def ctx = maxPrecision(decimal.bigDecimal, rounding)
			roundingProperty(decimal.round(ctx).bigDecimal)(Decimal64(decimal, rounding))
		} :_*)
	}
	property("Decimal64.apply(JavaBigDecimal, MathContext)") = forAll { decimal :BigDecimal =>
		all(
			Seq(DECIMAL32, DECIMAL64, DECIMAL128).map { ctx =>
				equivalent(decimal.bigDecimal.round(ctx)) {
					Decimal64(decimal.bigDecimal, ctx)
				} :| s"Decimal64($decimal, $ctx) ?= $decimal.round($ctx)"
			} :_*
		)
	}
	property("Decimal64.apply(BigDecimal, MathContext)") = forAll { decimal :BigDecimal =>
		all(
			Seq(DECIMAL32, DECIMAL64, DECIMAL128).map { ctx =>
				equivalent(decimal.round(ctx).bigDecimal) {
					Decimal64(decimal.bigDecimal, ctx)
				} :| s"Decimal64($decimal, $ctx) ?= $decimal.round($ctx)"
			} :_*
		)
	}

	property("Decimal64.round(Long, Int)(MathContext)") = forAll { (significand :Long, scale :Short) =>
		equivalent(JavaBigDecimal.valueOf(significand, scale).round(DECIMAL32)) {
			implicit val ctx = DECIMAL32
			Decimal64.round(significand, scale.toInt)
		}
	}
	property("Decimal64(String, MathContext)") = forAll { decimal :BigDecimal =>
		val formatted = decimal.round(DECIMAL64).toString
		guard(new JavaBigDecimal(formatted, DECIMAL64)) { expect =>
			val result = Decimal64(formatted, DECIMAL64)
			((bigDecimal(result) compareTo expect) ?= 0) :| s"parse \"$formatted\": expected: $expect; got: $result"
		}
	}

	private def roundingProperty(expect: => JavaBigDecimal)(test: => Decimal64) =
		if (throws(classOf[ArithmeticException])(expect))
			if (throws(classOf[ArithmeticException])(test))
				Prop(true)
			else
				Prop(false) :| "Expected a thrown ArithmeticException, got: " + test
		else {
			val normalized = expect.stripTrailingZeros
			val unscaled = normalized.unscaledValue
			val scale = normalized.scale
			val maxTrailingZeros = maxPrecision(unscaled.longValueExact).getPrecision - normalized.precision
			if (unscaled != BigInteger.ZERO && (scale > Byte.MaxValue || scale < Byte.MinValue - maxTrailingZeros))
				if (throws(classOf[ArithmeticException])(test))
					Prop(true)
				else {
					val got = test
					Prop(false) :| s"expected throws[ArithmeticException] when scale $scale out of range, got $got"
				} else try {
					val result = test
					((bigDecimal(result) compareTo expect) ?= 0) :| s"$result ?= $expect"
				} catch {
					case e :Exception => Prop.exception(e) :| "expected=" + expect
				}
		}



	/** * * * * * * * * * * * * * * * * Class properties * * * * * * * * * * * * * * * * * */

	/* * * * * * * * * * * * * * * * * Conversion methods ** * * * * * * * * * * * * * * * */


	property("toBigDecimal") = forAll { (decimal :Decimal64) => {
		val exact = BigDecimal(BigInt(decimal.unscaled), decimal.scale)
		(((decimal.toBigDecimal compareTo exact) ?= 0) :| s"$decimal.toBigDecimal(Extended)") &&
		(((decimal.toBigDecimal(Standard) compareTo exact.round(Standard)) ?= 0) :| s"$decimal.toBigDecimal(Standard)") &&
		(((decimal.toBigDecimal(DECIMAL32) compareTo exact.round(DECIMAL32)) ?= 0) :| s"$decimal.toBigDecimal(DECIMAL32)")
	}}
	property("toJavaBigDecimal") = forAll { (decimal :Decimal64) => {
		val exact = bigDecimal(decimal)
		(((decimal.toJavaBigDecimal compareTo exact) ?= 0) :| s"$decimal.toJavaBigDecimal(Extended)") &&
			(((decimal.toJavaBigDecimal(Standard) compareTo exact.round(Standard)) ?= 0) :| s"$decimal.toBigJavaDecimal(Standard)") &&
			(((decimal.toJavaBigDecimal(DECIMAL32) compareTo exact.round(DECIMAL32)) ?= 0) :| s"$decimal.toJavaBigDecimal(DECIMAL32)")
	}}
	property("toPlainString") = forAll { (decimal :Decimal64) =>
		decimal.toPlainString ?= bigDecimal(decimal).toPlainString
	}
	property("toEngineeringString") = forAll { (decimal :Decimal64) =>
		decimal.toEngineeringString ?= bigDecimal(decimal).toEngineeringString
	}


	/* * * * * * * * * * * * * * * * * Arithmetic operations ** * * * * * * * * * * * * * * * */


	property("*") = forAll { (x :Decimal64, y :Decimal64) =>
		all(RoundingMode.values().map { rounding =>
			{
				implicit val ctx = Round.to16digits(rounding)
				binaryProp(_.multiply(_, ctx))(_ * _)(x, y) :| "With MathContext " + ctx
			} && {
				implicit val ctx = Round.toMaxDigits(rounding)
				val result = bigDecimal(x).multiply(bigDecimal(y), DECIMAL128)
				binaryProp(_.multiply(_, maxPrecision(result, rounding)))(_ * _)(x, y) :|
					"Extended context: " + ctx
			} && {
				implicit val ctx = Round(7, rounding)
				binaryProp(_.multiply(_, ctx))(_ * _)(x, y) :| "With MathContext " + ctx
			}
		} :_*)
	}
	property("/") = forAll { (x :Decimal64, y :Decimal64) =>
		if (y == Decimal64.Zero)
			throws(classOf[ArithmeticException])(x / y) :| "throws[ArithmeticException] when dividing by zero"
		else {
			all(RoundingMode.values().map { rounding =>
				{
					implicit val ctx = Round.to16digits(rounding)
					binaryProp(_.divide(_, ctx))(_ / _)(x, y) :| "With MathContext " + ctx
				} && {
					implicit val ctx = Round.toMaxDigits(rounding)
					val result = bigDecimal(x).divide(bigDecimal(y), DECIMAL128)
					binaryProp(_.divide(_, maxPrecision(result, rounding)))(_ / _)(x, y) :|
						"Extended context: " + ctx
				} && {
					implicit val ctx = Round(7, rounding)
					binaryProp(_.divide(_, ctx))(_ / _)(x, y) :| "With MathContext " + ctx
				}
			} :_*)

		}
	}
	property("/~") = forAll { (x :Decimal64, y :Decimal64) =>
		if (y == Decimal64.Zero)
			throws(classOf[ArithmeticException])(x quot y) :| "throws[ArithmeticException] when dividing by zero"
		else all(
			{
				implicit val ctx = Round.toMaxDigits(DOWN)
//				val result = bigDecimal(x).divideToIntegralValue(bigDecimal(y), UNLIMITED)
				binaryProp { (a, b) =>
					val res = a.divideToIntegralValue(b, UNLIMITED)
					res.round(maxPrecision(res, DOWN))
				}(_ /~ _)(x, y) :| "Extended context"
			} +:
			Seq(DECIMAL32, DECIMAL64, DECIMAL128).map { implicit ctx =>
				binaryProp { (a, b) =>
					val res = a.divideToIntegralValue(b, UNLIMITED)
					res.round(new MathContext(ctx.getPrecision, DOWN))
				}(_ /~ _)(x, y) :| "With MathContext " + ctx
			}
		:_*)
	}
	property("**") = forAll { (x :Decimal64, n :Short) =>
		all(RoundingMode.values().map { rounding =>
			{
				implicit val ctx = Round.to16digits(rounding)
				equivalent(bigDecimal(x).pow(n, ctx))(x ** n) :| s"$x ** $n ($ctx)"
			} && {
				implicit val ctx = Round.toMaxDigits(rounding)
				def result = bigDecimal(x).pow(n, DECIMAL128)
				equivalent(bigDecimal(x).pow(n, maxPrecision(result, rounding)))(x ** n) :| s"$x ** $n ($ctx)"
			} && {
				implicit val ctx = Round(7, rounding)
				equivalent(bigDecimal(x).pow(n, ctx))(x ** n) :| s"$x ** $n ($ctx)"
			}
		} :_*)
	}

}