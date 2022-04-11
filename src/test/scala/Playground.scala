import java.math.{MathContext, RoundingMode}
import java.math.MathContext.{DECIMAL128, DECIMAL32}
import java.math.RoundingMode.{DOWN, FLOOR, HALF_DOWN, HALF_EVEN, HALF_UP, UNNECESSARY, UP}

import scala.math.BigDecimal.RoundingMode.CEILING

import net.noresttherein.slang.numeric.{Decimal64, Decimal64Spec}
import net.noresttherein.slang.numeric.Decimal64.Round.{maxPrecision, Extended, ExtendedExact, Standard}
import net.noresttherein.slang.numeric.Decimal64.implicits.scientificDecimalNotation
import net.noresttherein.slang.numeric.Decimal64.{Precision, Round}




/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
object Playground extends App {
//	implicit val ctx :MathContext = Round.toMaxDigits(UNNECESSARY) //Decimal64.Round.Extended
//	implicit val ctx :MathContext = Round.toMaxDigits(UP) //Decimal64.Round.Extended
//	implicit val ctx :MathContext = Round.Extended
//	implicit val ctx :MathContext = Round.Standard
//	implicit val ctx :MathContext = Round.to16digits(CEILING)
//	implicit val ctx :MathContext = Round.to16digits(HALF_DOWN)
//	implicit val ctx :MathContext = Round.ExtendedUp
//	implicit val ctx :MathContext = DECIMAL32
//	implicit val ctx :MathContext = DECIMAL128
	implicit val ctx :MathContext = new MathContext(7, HALF_DOWN)

//	val bigDividend = new java.math.BigDecimal("3.6028797018963967e17")
//	val bigDivisor  = new java.math.BigDecimal("1.0431160155578814")
	val bigDividend = new java.math.BigDecimal("2.8319985000541197e143")
	val bigDivisor  = new java.math.BigDecimal("1")
	val exponent = 1
//	val bigSum      = bigDividend.add(bigDivisor, DECIMAL128)
//	val bigMinus    = bigDividend.subtract(bigDivisor, DECIMAL128)
//	val bigDividend = new java.math.BigDecimal("-36028797018963968")
//	val bigDivisor  = new java.math.BigDecimal("-0.625")
//	val bigMultiply = bigDividend.multiply(bigDivisor, DECIMAL128)
//	val bigDivision = bigDividend.divide(bigDivisor, MathContext.DECIMAL128)
//	val bigQuot     = bigDividend.divideToIntegralValue(bigDivisor, MathContext.UNLIMITED)
	val bigPower    = bigDividend.pow(exponent, DECIMAL128)
//	val sumCtx =
//		if (ctx.getPrecision == 0)
//			Round(maxPrecision(bigSum, ctx.getRoundingMode), ctx.getRoundingMode)
//		else ctx
//	val minusCtx =
//		if (ctx.getPrecision == 0)
//			Round(maxPrecision(bigMinus, ctx.getRoundingMode), ctx.getRoundingMode)
//		else ctx
//	val multCtx =
//		if (ctx.getPrecision == 0)
//			Round(maxPrecision(bigMultiply, ctx.getRoundingMode), ctx.getRoundingMode)
//		else ctx
//	val divCtx =
//		if (ctx.getPrecision == 0)
//			Round(maxPrecision(bigDivision, ctx.getRoundingMode), ctx.getRoundingMode)
//		else ctx
//	val quotCtx =
//		if (ctx.getPrecision == 0)
//			Round(maxPrecision(bigQuot, DOWN), DOWN)
//		else
//			Round(ctx.getPrecision, DOWN)
	val powCtx =
		if (ctx.getPrecision == 0)
			Round(maxPrecision(bigPower, ctx.getRoundingMode), ctx.getRoundingMode)
		else ctx
//	println("max precision: " + maxPrecision(bigDivision, ctx.getRoundingMode) + "; round " + ctx.getRoundingMode)
//	println("max precision: " + maxPrecision(bigQuot, DOWN))
//	println("max precision: " + maxPrecision(bigMultiply, multCtx.getRoundingMode))
//	println("context: " + multCtx)
//	val sum           = bigDividend.add(bigDivisor, sumCtx)
//	val minus         = bigDividend.subtract(bigDivisor, minusCtx)
//	val mult          = bigDividend.multiply(bigDivisor, multCtx)
//	val division      = bigDividend.divide(bigDivisor, divCtx)
//	val quot          = bigDividend.divideToIntegralValue(bigDivisor, quotCtx)
//	val quot          = bigQuot.round(quotCtx)
	val pow           = bigDividend.pow(exponent, powCtx)

	val dividend = Decimal64(bigDividend.unscaledValue.longValueExact, bigDividend.scale)
	val divisor  = Decimal64(bigDivisor.unscaledValue.longValueExact, bigDivisor.scale)

	println("using context " + ctx)
//	println(s"should be: $bigDividend + $bigDivisor = $sum ($bigSum)")
//	println(s"           $dividend + $divisor = ${dividend + divisor}")
//	println(s"should be: $bigDividend - $bigDivisor = $minus ($bigMinus)")
//	println(s"           $dividend - $divisor = ${dividend - divisor}")
//	println(s"should be: $bigDividend * $bigDivisor = $mult ($bigMultiply)")
//	println(s"           $dividend * $divisor = ${dividend * divisor}")
//	println(s"should be: $bigDividend /  $bigDivisor = $division ($bigDivision)")
//	println(s"           $dividend /  $divisor = ${dividend / divisor}")
//	println(s"should be: $bigDividend /~ $bigDivisor = $quot ($bigQuot)")
//	println(s"           $dividend /~ $divisor = ${dividend /~ divisor}")
	println(s"should be: $bigDividend ** $exponent = $pow ($bigPower)")
	println(s"           $dividend ** $exponent = ${dividend ** exponent}")

}



