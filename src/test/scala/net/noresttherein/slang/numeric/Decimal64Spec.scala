package net.noresttherein.slang.numeric

import net.noresttherein.slang.numeric.Decimal64.{MaxUnscaled, MinUnscaled}
import org.scalacheck.{Arbitrary, Prop, Properties}
import org.scalacheck.Prop._




object Decimal64Spec extends Properties("Decimal64") {
	implicit val ArbitraryDecimal64 = Arbitrary(Arbitrary.arbitrary[Long].map { long =>
		Decimal64(long >> 8, long.toInt & 0xff)
	})

	private def trailingZeros(long :Long, acc :Int = 0) :Int =
		if (long == 0) 0
		else if (long % 10 == 0) trailingZeros(long / 10, acc + 1)
		else acc

	implicit val ctx = Decimal64.Standard

	/* * * * * * * * * * * * Properties of the companion object * * * * * * * * * * * * */

	property("Decimal64.apply(Int)") = forAll { value :Int =>
		Decimal64(value).toBigDecimal ?= BigDecimal(value)
	}
	property("Decimal64.apply(Long)") = forAll { value :Long =>
		val significand = { var n = value; while (n != 0 && n % 10 == 0) n /= 10; n  }
		if (MinUnscaled <= significand && significand <= MaxUnscaled) {
			val expect = BigDecimal(value, ctx)
			val got = Decimal64(value).toBigDecimal
			(got ?= expect) :| s"${Decimal64(value)} == $got ?= $expect"
		} else {
			val expect = BigDecimal(value >> Decimal64.ScaleBits, (value.toInt & 0xff).toByte, ctx)
			val decimal = Decimal64(value >> Decimal64.ScaleBits, (value.toInt & 0xff).toByte)
			val got = decimal.toBigDecimal
			(got ?= expect) :| s"$decimal == $got ?= $expect" &&
			(throws(classOf[IllegalArgumentException])(Decimal64(value)) :| "throw[IllegalArgumentException]")
		}
	}
	property("Decimal64.apply(Long, Int)") = forAll {
		(significand :Long, scale :Short) =>
			if (significand > Decimal64.MaxUnscaled | significand < Decimal64.MinUnscaled)
				throws(classOf[IllegalArgumentException])(Decimal64(significand, scale)) :| "significand out of range"
			else if (significand != 0 && (scale > Byte.MaxValue || scale < Byte.MinValue - trailingZeros(significand)))
				throws(classOf[IllegalArgumentException])(Decimal64(significand, scale)) :| "scale out of range"
			else {
				val dec = Decimal64(significand, scale); val result = dec.toBigDecimal
				val expect = BigDecimal(significand, scale, ctx)
					(result ?= expect) :| s"$dec == $expect ?= $result"
			}
	}



	property("toBigDecimal") = forAll { (decimal :Decimal64) =>
		decimal.toBigDecimal ?= BigDecimal(BigInt(decimal.significand), decimal.scale)

	}
//	def unaryProperty(result :Decimal64 => Decimal64, expect :BigDecimal => BigDecimal) =
//		forAll { (bits :Long) =>
//			val arg = Decimal64(bits >> Decimal64.ScaleBits, bits.toInt & Decimal64.ScaleMask)
//			if (significand > Decimal64.MaxUnscaled | significand < Decimal64.MinUnscaled)
//				Prop.passed
//			else
//		}

}
