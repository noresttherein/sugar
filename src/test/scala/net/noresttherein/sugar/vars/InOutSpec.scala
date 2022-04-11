package net.noresttherein.sugar.vars

import net.noresttherein.sugar.vars.InOut.SpecializedVars
import org.scalacheck.{Arbitrary, Prop, Properties, Shrink}
import org.scalacheck.Prop._
import org.scalacheck.util.Pretty

abstract class BaseInOutPropsGroup {

	def newVar[@specialized(SpecializedVars) T](x :T) :InOut[T] //= InOut(x)

	def varClassName :String


	/** Test of the generic `InOut[T]` interface */
	abstract class BasicInOutProps[T :Arbitrary :Shrink](varName :String)(implicit pretty :T => Pretty)
		extends Properties(varName)
	{

		protected def unaryFunctions :Map[String, T => T]

		protected def binaryFunctions :Map[String, (T, T) => T]


		property("value") = forAll { (x1 :T, x2 :T) =>
			val v = newVar(x1); (v.value ?= x1 ) :| "getter" && {
			v.value = x2; v.value ?= x2 } :| "setter"
		}

		property(":=") = forAll { (x1 :T, x2 :T) => val v = newVar(x1); v := x2; v.value ?= x2 }

		property("?=") = forAll {
			(x1 :T, x2 :T) => val v = newVar(x1); ((v ?= x2) ?= x1) :| "previous" && (v.value ?= x2) :| "next"
		}

		property("testAndSet") = forAll { (x1 :T, x2 :T, x3 :T) =>
			val v = newVar(x1)
			(Prop(v.testAndSet(x1, x2)) && (v.value ?= x2)) :| "when expect up to date" && (
				if (x2 == x1) Prop(v.testAndSet(x1, x3)) && (v.value ?= x3) :| "update same"
				else Prop(!v.testAndSet(x1, x3)) && (v.value ?= x2) :| "when expect not up to date"
			)
		}

		property(":?") = forAll { (x1 :T, x2 :T, x3 :T) =>
			val v = newVar(x1)
			(Prop(v :? x1 := x2) && (v.value ?= x2)) :| "when expect up to date" && (
				if (x2 == x1) Prop(v :? x1 := x3) && (v.value ?= x3) :| "update same"
				else Prop(!(v :? x1 := x3)) && (v.value ?= x2) :| "when expect not up to date"
				)
		}




		property("apply") = forAll { (x :T) =>  all(
	        unaryFunctions.map {
		        case (name, f) => {
		            val v = newVar(x); (v(f) ?= f(x)) :| "return" && (v.value ?= f(x)) :| "assign"
		        } :| ("function: x => " + name)
	        }.toSeq : _*
		)}

		property("/:") = forAll { (x1 :T, x2 :T) => all(
			binaryFunctions.map {
				case (name, f) => {
					val v = newVar(x2); ((x1 /=: v)(f) ?= f(x1, x2)) :| "return" && ((v.value ?= f(x1, x2)) :| "assign")
				} :| ("function: (x1, x2) => " + name)
			}.toSeq :_*
		)}

		property("/:") = forAll { (x1 :T, x2 :T) => all(
			binaryFunctions.map {
				case (name, f) => {
					val v = newVar(x1); ((v :\= x2)(f) ?= f(x1, x2)) :| "return" && ((v.value ?= f(x1, x2)) :| "assign")
				} :| ("function: (x1, x2) =>  " + name)
			}.toSeq :_*
		)}

	}






/************************* Test of `InOut[Boolean]` and its implicit extensions ***************************************/

	class InOutBooleanProps(name :String = varClassName + "[Boolean]") extends BasicInOutProps[Boolean](name) {

		override def unaryFunctions :Map[String, Boolean => Boolean] =
			Map("x" -> { x => x }, "!x" -> { x => !x })

		override def binaryFunctions :Map[String, (Boolean, Boolean) => Boolean] = Map(
			"x1 || x2" -> { _ | _ }, "x1 && x2" -> 	{ _ && _ }, "x1 ^ x2" -> { _ ^ _ }
		)



		property("&=") = forAll { (x1 :Boolean, x2 :Boolean) => val v = newVar(x1); v &= x2; v.value ?= x1 & x2 }

		property("&&=") = forAll { (x1 :Boolean, x2 :Boolean) =>
			val v = newVar(x1); val arg = Lazy(x2)
			v &&= arg
			(v.value ?= x1 && x2) :| "correctness" && (arg.isDefined ?= x1) :| "laziness"
		}

		property("|=") = forAll { (x1 :Boolean, x2 :Boolean) => val v = newVar(x1); v |= x2; v.value ?= x1 | x2 }

		property("||=") = forAll { (x1 :Boolean, x2 :Boolean) =>
			val v = newVar(x1); val arg = Lazy(x2)
			v ||= arg
			(v.value ?= x1 || x2) :| "correctness" || (arg.isDefined ?= x1) :| "laziness"
		}

		property("^=") = forAll { (x1 :Boolean, x2 :Boolean) => val v = newVar(x1); v ^= x2; v.value ?= x1 ^ x2 }


		property("flip()") = forAll { x :Boolean => val v = newVar(x); v.flip(); v.value ?= !x }

		property("neg()") = forAll { x :Boolean => val v = newVar(x); (v.neg() ?= !x) :| "returns" && (v.value ?= !x) :| "asisgns" }

	}







/************************* Test of `InOut[Int]` and its implicit extensions *******************************************/

	class InOutIntProps(name :String = varClassName + "[Int]") extends BasicInOutProps[Int](name) {

		override def unaryFunctions :Map[String, Int => Int] = Map(
			"x" -> { x => x }, "x + x" -> { x => x + x }, "x * x" -> { x => x * x }, "x + 1" -> { x => x + 1 },
			"x + MinInt" -> { x => x + Int.MinValue }, "x - MinInt" -> { x => x - Int.MinValue }, "x >> 5" -> { x => x >> 5 }
	    )

		override def binaryFunctions :Map[String, (Int, Int) => Int] = Map(
			"x1 + x2" -> { (x1, x2) => x1 + x2 }, "x1 - x2" -> { (x1, x2) => x1 - x2 }, "x1 * x2" -> { (x1, x2) => x1 * x2 },
			"x1 >>> x2" -> { (x1, x2) => x1 >>> x2 }, "x1 min x2" -> { (x1, x2) => x1 min x2}, "x1 max x2" -> { (x1, x2) => x1 max x2 }
	    )


		property("+=") = forAll { (x1 :Int, x2 :Int) => val v = newVar(x1); v += x2; v.value ?= x1 + x2 }

		property("inc(Int)") = forAll { (x1 :Int, x2 :Int) =>
			val v = newVar(x1); ((v inc x2) ?= x1 + x2) :| "return" && (v.value ?= x1 + x2) :| "assign"
		}

		property("-=") = forAll { (x1 :Int, x2 :Int) => val v = newVar(x1); v -= x2; v.value ?= x1 - x2 }

		property("dec(Int)") = forAll { (x1 :Int, x2 :Int) =>
			val v = newVar(x1); ((v dec x2) ?= x1 - x2) :| "return" && (v.value ?= x1 - x2) :| "assign"
		}

		property("*=") = forAll { (x1 :Int, x2 :Int) => val v = newVar(x1); v *= x2; v.value ?= x1 * x2 }

		property("mult") = forAll { (x1 :Int, x2 :Int) =>
			val v = newVar(x1); ((v mult x2) ?= x1 * x2) :| "return" && (v.value ?= x1 * x2) :| "assign"
		}

		property("/=") = forAll { (x1 :Int, x2 :Int) =>
			val v = newVar(x1)
			if (x2 != 0) { v /= x2; v.value ?= x1 / x2 }
			else Prop(throws(classOf[ArithmeticException]) { v /= x2 })
		}

		property("div") = forAll { (x1 :Int, x2 :Int) =>
			val v = newVar(x1)
			if (x2 != 0) ((v div x2) ?= x1 / x2) :| "return" && (v.value ?= x1 / x2) :| "assign"
			else Prop(throws(classOf[ArithmeticException]) { v div x2 })
		}

		property("%=") = forAll { (x1 :Int, x2 :Int) =>
			val v = newVar(x1)
			if (x2 != 0) {v %= x2; v.value ?= x1 % x2}
			else Prop(throws(classOf[ArithmeticException]) {v %= x2})
		}

		property("rem") = forAll { (x1 :Int, x2 :Int) =>
			val v = newVar(x1)
			if (x2 != 0) ((v rem x2) ?= x1 % x2) :| "return" && (v.value ?= x1 % x2) :| "assign"
			else Prop(throws(classOf[ArithmeticException]) { v rem x2})
		}

		property("neg()") = forAll { x :Int => val v = newVar(x); (v.neg() ?= -x) :| "return" && (v.value ?= -x) :| "assign" }

		property("++") = forAll { x :Int => val v = newVar(x); v.++; v.value ?= x + 1 }

		property("inc()") = forAll { x1 :Int => val v = newVar(x1); (v.inc() ?= x1 + 1) :| "return" && (v.value ?= x1 + 1) :| "assign"}

		property("--") = forAll { x :Int => val v = newVar(x); v.--; v.value ?= x - 1 }

		property("dec()") = forAll { x1 :Int => val v = newVar(x1); (v.dec() ?= x1 - 1) :| "return" && (v.value ?= x1 - 1) :| "assign"}



		property("|=") = forAll { (x1 :Int, x2 :Int) => val v = newVar(x1); v |= x2; v.value ?= x1 | x2}

		property("&=") = forAll { (x1 :Int, x2 :Int) => val v = newVar(x1); v &= x2; v.value ?= x1 & x2 }

		property("^=") = forAll { (x1 :Int, x2 :Int) => val v = newVar(x1); v ^= x2; v.value ?= x1 ^ x2 }

		property(">>=") = forAll { (x :Int, n :Int) => val v = newVar(x); v >>= n; v.value ?= x >> n }

		property(">>>=") = forAll { (x :Int, n :Int) => val v = newVar(x); v >>>= n; v.value ?= x >>> n }

		property("<<=") = forAll { (x :Int, n :Int) => val v = newVar(x); v <<= n; v.value ?= x << n }

		property("flip()") = forAll { x :Int => val v = newVar(x); v.flip(); v.value ?= ~x }

	}







/************************* Test of `InOut[Long]` and its implicit extensions ******************************************/

	class InOutLongProps(name :String = varClassName + "[Long]") extends BasicInOutProps[Long](name) {

		override def unaryFunctions :Map[String, Long => Long] = Map(
	        "x" -> { x => x }, "x + x" -> { x => x + x }, "x * x" -> { x => x * x }, "x + 1" -> { x => x + 1 },
	        "x + MinLong" -> { x => x + Long.MinValue }, "x - MinLong" -> { x => x - Long.MinValue }, "x >> 5" -> { x => x >> 5 }
	    )

		override def binaryFunctions :Map[String, (Long, Long) => Long] = Map(
	        "x1 + x2" -> { (x1, x2) => x1 + x2 }, "x1 - x2" -> { (x1, x2) => x1 - x2 }, "x1 * x2" -> { (x1, x2) => x1 * x2 },
	        "x1 >>> x2" -> { (x1, x2) => x1 >>> x2 }, "x1 min x2" -> { (x1, x2) => x1 min x2}, "x1 max x2" -> { (x1, x2) => x1 max x2 }
	    )


		property("+=") = forAll { (x1 :Long, x2 :Long) => val v = newVar(x1); v += x2; v.value ?= x1 + x2 }

		property("inc(Long)") = forAll { (x1 :Long, x2 :Long) =>
			val v = newVar(x1); ((v inc x2) ?= x1 + x2) :| "return" && (v.value ?= x1 + x2) :| "assign"
		}

		property("-=") = forAll { (x1 :Long, x2 :Long) => val v = newVar(x1); v -= x2; v.value ?= x1 - x2 }

		property("dec(Long)") = forAll { (x1 :Long, x2 :Long) =>
			val v = newVar(x1); ((v dec x2) ?= x1 - x2) :| "return" && (v.value ?= x1 - x2) :| "assign"
		}

		property("*=") = forAll { (x1 :Long, x2 :Long) => val v = newVar(x1); v *= x2; v.value ?= x1 * x2 }

		property("mult") = forAll { (x1 :Long, x2 :Long) =>
			val v = newVar(x1); ((v mult x2) ?= x1 * x2) :| "return" && (v.value ?= x1 * x2) :| "assign"
		}

		property("/=") = forAll { (x1 :Long, x2 :Long) =>
			val v = newVar(x1)
			if (x2 != 0) { v /= x2; v.value ?= x1 / x2 }
			else Prop(throws(classOf[ArithmeticException]) { v /= x2 })
		}

		property("div") = forAll { (x1 :Long, x2 :Long) =>
			val v = newVar(x1)
			if (x2 != 0) ((v div x2) ?= x1 / x2) :| "return" && (v.value ?= x1 / x2) :| "assign"
			else Prop(throws(classOf[ArithmeticException]) { v div x2 })
		}

		property("%=") = forAll { (x1 :Long, x2 :Long) =>
			val v = newVar(x1)
			if (x2 != 0) {v %= x2; v.value ?= x1 % x2}
			else Prop(throws(classOf[ArithmeticException]) {v %= x2})
		}

		property("rem") = forAll { (x1 :Long, x2 :Long) =>
			val v = newVar(x1)
			if (x2 != 0) ((v rem x2) ?= x1 % x2) :| "return" && (v.value ?= x1 % x2) :| "assign"
			else Prop(throws(classOf[ArithmeticException]) { v rem x2})
		}

		property("++") = forAll { x :Long => val v = newVar(x); v.++; v.value ?= x + 1 }

		property("inc()") = forAll { x1 :Long => val v = newVar(x1); (v.inc() ?= x1 + 1) :| "return" && (v.value ?= x1 + 1) :| "assign"}

		property("--") = forAll { x :Long => val v = newVar(x); v.--; v.value ?= x - 1 }

		property("dec()") = forAll { x1 :Long => val v = newVar(x1); (v.dec() ?= x1 - 1) :| "return" && (v.value ?= x1 - 1) :| "assign"}

		property("neg()") = forAll { x :Long => val v = newVar(x); (v.neg() ?= -x) :| "return" && (v.value ?= -x) :| "assign" }


		property("|=") = forAll { (x1 :Long, x2 :Long) => val v = newVar(x1); v |= x2; v.value ?= x1 | x2}

		property("&=") = forAll { (x1 :Long, x2 :Long) => val v = newVar(x1); v &= x2; v.value ?= x1 & x2 }

		property("^=") = forAll { (x1 :Long, x2 :Long) => val v = newVar(x1); v ^= x2; v.value ?= x1 ^ x2 }

		property(">>=") = forAll { (x :Long, n :Long) => val v = newVar(x); v >>= n.toInt; v.value ?= x >> n }

		property(">>>=") = forAll { (x :Long, n :Long) => val v = newVar(x); v >>>= n.toInt; v.value ?= x >>> n }

		property("<<=") = forAll { (x :Long, n :Long) => val v = newVar(x); v <<= n.toInt; v.value ?= x << n }

		property("flip()") = forAll { x :Long => val v = newVar(x); v.flip(); v.value ?= ~x }

	}






/************************* Test of `InOut[Float]` and its implicit extensions *****************************************/

	class InOutFloatProps(name :String = varClassName + "[Float]") extends BasicInOutProps[Float](name) {

		override def unaryFunctions :Map[String, Float => Float] = Map(
	        "x" -> { x => x }, "x + x" -> { x => x + x }, "x * x" -> { x => x * x }, "x + 1" -> { x => x + 1 },
	        "x + MinFloat" -> { x => x + Float.MinValue }, "x - MinFloat" -> { x => x - Float.MinValue }
	    )

		override def binaryFunctions :Map[String, (Float, Float) => Float] = Map(
	        "x1 + x2" -> { (x1, x2) => x1 + x2 }, "x1 - x2" -> { (x1, x2) => x1 - x2 }, "x1 * x2" -> { (x1, x2) => x1 * x2 },
	        "x1 min x2" -> { (x1, x2) => x1 min x2}, "x1 max x2" -> { (x1, x2) => x1 max x2 }
	    )


		property("+=") = forAll { (x1 :Float, x2 :Float) => val v = newVar(x1); v += x2; v.value ?= x1 + x2 }

		property("inc(Float)") = forAll { (x1 :Float, x2 :Float) =>
			val v = newVar(x1); ((v inc x2) ?= x1 + x2) :| "return" && (v.value ?= x1 + x2) :| "assign"
		}

		property("-=") = forAll { (x1 :Float, x2 :Float) => val v = newVar(x1); v -= x2; v.value ?= x1 - x2 }

		property("dec(Float)") = forAll { (x1 :Float, x2 :Float) =>
			val v = newVar(x1); ((v dec x2) ?= x1 - x2) :| "return" && (v.value ?= x1 - x2) :| "assign"
		}

		property("*=") = forAll { (x1 :Float, x2 :Float) => val v = newVar(x1); v *= x2; v.value ?= x1 * x2 }

		property("mult") = forAll { (x1 :Float, x2 :Float) =>
			val v = newVar(x1); ((v mult x2) ?= x1 * x2) :| "return" && (v.value ?= x1 * x2) :| "assign"
		}

		property("/=") = forAll { (x1 :Float, x2 :Float) =>
			val v = newVar(x1)
			if (x2 != 0) { v /= x2; v.value ?= x1 / x2 }
			else Prop(throws(classOf[ArithmeticException]) { v /= x2 })
		}

		property("div") = forAll { (x1 :Float, x2 :Float) =>
			val v = newVar(x1)
			if (x2 != 0) ((v div x2) ?= x1 / x2) :| "return" && (v.value ?= x1 / x2) :| "assign"
			else Prop(throws(classOf[ArithmeticException]) { v div x2 })
		}

		property("neg()") = forAll { x :Float => val v = newVar(x); (v.neg() ?= -x) :| "return" && (v.value ?= -x) :| "assign" }

	}











/************************* Test of `InOut[Double]` and its implicit extensions ****************************************/

	class InOutDoubleProps(name :String = varClassName + "[Double]") extends BasicInOutProps[Double](name) {

		override def unaryFunctions :Map[String, Double => Double] = Map(
	        "x" -> { x => x }, "x + x" -> { x => x + x }, "x * x" -> { x => x * x }, "x + 1" -> { x => x + 1 },
	        "x + MinDouble" -> { x => x + Double.MinValue }, "x - MinDouble" -> { x => x - Double.MinValue }
	    )

		override def binaryFunctions :Map[String, (Double, Double) => Double] = Map(
			"x1 + x2" -> { (x1, x2) => x1 + x2 }, "x1 - x2" -> { (x1, x2) => x1 - x2 }, "x1 * x2" -> { (x1, x2) => x1 * x2 },
			"x1 min x2" -> { (x1, x2) => x1 min x2}, "x1 max x2" -> { (x1, x2) => x1 max x2 }
		)


		property("+=") = forAll { (x1 :Double, x2 :Double) => val v = newVar(x1); v += x2; v.value ?= x1 + x2 }

		property("inc(Double)") = forAll { (x1 :Double, x2 :Double) =>
			val v = newVar(x1); ((v inc x2) ?= x1 + x2) :| "return" && (v.value ?= x1 + x2) :| "assign"
		}

		property("-=") = forAll { (x1 :Double, x2 :Double) => val v = newVar(x1); v -= x2; v.value ?= x1 - x2 }

		property("dec(Double)") = forAll { (x1 :Double, x2 :Double) =>
			val v = newVar(x1); ((v dec x2) ?= x1 - x2) :| "return" && (v.value ?= x1 - x2) :| "assign"
		}

		property("*=") = forAll { (x1 :Double, x2 :Double) => val v = newVar(x1); v *= x2; v.value ?= x1 * x2 }

		property("mult") = forAll { (x1 :Double, x2 :Double) =>
			val v = newVar(x1); ((v mult x2) ?= x1 * x2) :| "return" && (v.value ?= x1 * x2) :| "assign"
		}

		property("/=") = forAll { (x1 :Double, x2 :Double) =>
			val v = newVar(x1)
			if (x2 != 0) { v /= x2; v.value ?= x1 / x2 }
			else Prop(throws(classOf[ArithmeticException]) { v /= x2 })
		}

		property("div") = forAll { (x1 :Double, x2 :Double) =>
			val v = newVar(x1)
			if (x2 != 0) ((v div x2) ?= x1 / x2) :| "return" && (v.value ?= x1 / x2) :| "assign"
			else Prop(throws(classOf[ArithmeticException]) { v div x2 })
		}

		property("neg()") = forAll { x :Double => val v = newVar(x); (v.neg() ?= -x) :| "return" && (v.value ?= -x) :| "assign" }

	}






/************************* Test of `InOut[Char]` and its implicit extensions ******************************************/


	class InOutCharProps(name :String = varClassName + "[Char]") extends BasicInOutProps[Char](name) {

		override def unaryFunctions :Map[String, Char => Char] =
			Map("x.toUpper" -> { _.toUpper }, "x.toLower" -> { _.toLower }, "~x" -> { x => (~x).toChar })

		override def binaryFunctions :Map[String, (Char, Char) => Char] =
			Map("x1 min x2" -> { _ min _}, "x1 max x2" -> { _ max _})
	}





/*********************** Test of `InOut[Short]` and its implicit extensions *******************************************/

	class InOutShortProps(name :String = varClassName + "[Short]") extends BasicInOutProps[Short](name) {

		override def unaryFunctions :Map[String, Short => Short] =
			Map("-x" -> { x => (-x).toShort }, "x & 0xff" -> { x => (x & 0xff).toShort }, "x >>> 3" -> { x => (x >>> 3).toShort })

		override def binaryFunctions :Map[String, (Short, Short) => Short] = Map(
			"x1 min x2" -> { _ min _ }, "x1 max x2" -> { _ max _ }, "x1 + x2" -> { (x1, x2) => (x1 + x2).toShort }
		)
	}







/************************* Test of `InOut[Byte]` and its implicit extensions ******************************************/

	class InOutByteProps(name :String = varClassName + "[Byte]") extends BasicInOutProps[Byte](name) {

		override def unaryFunctions :Map[String, Byte => Byte] = Map(
			"~x" -> { x => (~x).toByte }, "-x" -> { x => (-x).toByte }
		)

		override def binaryFunctions :Map[String, (Byte, Byte) => Byte] = Map(
			"x1 & x2" -> { (x1, x2) => (x1 & x2).toByte }, "x1 | x2" -> { (x1, x2) => (x1 | x2).toByte },
			"x1 ^ x2" -> { (x1, x2) => (x1 ^ x2).toByte }
		)
	}



/******************** Test of `InOut[BigInt]` (erased variant) and its implicit extensions ****************************/

	class InOutBigIntProps(name :String = varClassName + "[BigInt]") extends BasicInOutProps[BigInt](name) {

		override def unaryFunctions :Map[String, BigInt => BigInt] = Map(
	        "x" -> { x => x }, "x + x" -> { x => x + x }, "x * x" -> { x => x * x }, "x + 1" -> { x => x + 1 }
	    )

		override def binaryFunctions :Map[String, (BigInt, BigInt) => BigInt] = Map(
			"x1 + x2" -> { (x1, x2) => x1 + x2 }, "x1 - x2" -> { (x1, x2) => x1 - x2 }, "x1 * x2" -> { (x1, x2) => x1 * x2 },
			"x1 min x2" -> { (x1, x2) => x1 min x2}, "x1 max x2" -> { (x1, x2) => x1 max x2 }
		)


	/*
		property("+=") = forAll { (x1 :BigInt, x2 :BigInt) => val v = newVar(x1); v += x2; v.get ?= x1 + x2 }

		property("inc(BigInt)") = forAll { (x1 :BigInt, x2 :BigInt) =>
			val v = newVar(x1); ((v inc x2) ?= x1 + x2) :| "return" && (v.get ?= x1 + x2) :| "assign"
		}

		property("-=") = forAll { (x1 :BigInt, x2 :BigInt) => val v = newVar(x1); v -= x2; v.get ?= x1 - x2 }

		property("dec(BigInt)") = forAll { (x1 :BigInt, x2 :BigInt) =>
			val v = newVar(x1); ((v dec x2) ?= x1 - x2) :| "return" && (v.get ?= x1 - x2) :| "assign"
		}

		property("*=") = forAll { (x1 :BigInt, x2 :BigInt) => val v = newVar(x1); v *= x2; v.get ?= x1 * x2 }

		property("mult(BigInt)") = forAll { (x1 :BigInt, x2 :BigInt) =>
			val v = newVar(x1); ((v mult x2) ?= x1 * x2) :| "return" && (v.get ?= x1 * x2) :| "assign"
		}

		property("/=") = forAll { (x1 :BigInt, x2 :BigInt) =>
			val v = newVar(x1)
			if (x2 != 0) { v /= x2; v.get ?= x1 / x2 }
			else Prop(throws(classOf[ArithmeticException]) { v /= x2 })
		}

		property("div(BigInt)") = forAll { (x1 :BigInt, x2 :BigInt) =>
			val v = newVar(x1)
			if (x2 != 0) ((v div x2) ?= x1 / x2) :| "return" && (v.get ?= x1 / x2) :| "assign"
			else Prop(throws(classOf[ArithmeticException]) { v div x2 })
		}

		property("%=") = forAll { (x1 :BigInt, x2 :BigInt) =>
			val v = newVar(x1)
			if (x2 != 0) {v %= x2; v.get ?= x1 % x2}
			else Prop(throws(classOf[ArithmeticException]) {v %= x2})
		}

		property("rem(BigInt)") = forAll { (x1 :BigInt, x2 :BigInt) =>
			val v = newVar(x1)
			if (x2 != 0) ((v rem x2) ?= x1 % x2) :| "return" && (v.get ?= x1 % x2) :| "assign"
			else Prop(throws(classOf[ArithmeticException]) { v rem x2})
		}

		property("neg()") = forAll { x :BigInt => val v = newVar(x); (v.neg() ?= -x) :| "return" && (v.get ?= -x) :| "assign" }
	*/

	}

}


class InOutPropsGroup(override val varClassName :String = "InOut") extends BaseInOutPropsGroup {

	override def newVar[@specialized(SpecializedVars) T](x :T) :InOut[T] = InOut(x)

	def includeIn(container :Properties) = {
		import container.include

		include(new InOutIntProps)
		include(new InOutLongProps)
		include(new InOutFloatProps)
		include(new InOutDoubleProps)
		include(new InOutCharProps)
		include(new InOutShortProps)
		include(new InOutByteProps)
		include(new InOutBigIntProps)
	}
}



class ErasedInOutPropsGroup extends InOutPropsGroup("InOut<erased>") {
	override def newVar[@specialized(SpecializedVars) T](x :T) :InOut[T] = erasedVar(x)

	def erasedVar[T](x :T) :InOut[T] = InOut(x)
}





object InOutSpec extends Properties("vars.InOut") {

	(new InOutPropsGroup).includeIn(this)
	(new ErasedInOutPropsGroup).includeIn(this)


	property("implicits.InOutMultiAssignment") = forAll { x :String =>
		import InOut.implicits.InOutMultiAssignment
		val v1 = InOut("a"); val v2 = InOut("b"); val v3 = InOut("d")
		v1 =: v2 =: v3 =: x
		(v1.value ?= x) && (v2.value ?= x) && (v3.value ?= x)

	}


//	def testOrdering[T](x1 :T, x2 :T)(implicit t :Ordering[T], inout :Ordering[InOut[T]]) :Prop = {
//		import Ordering.Implicits.infixOrderingOps
//		if (x1 < x2) Prop(InOut(x1) < InOut(x2)) :| "<"
//		else Prop(InOut(x1) >= InOut(x2)) :| ">="
//	}
//
//	property("InOutOrdering") = forAll { (x1 :String, x2 :String) => testOrdering(x1, x2) }


/*
	private def testNumeric[T](x1 :T, x2 :T)(implicit t :Numeric[T], inout :Numeric[InOut[T]]) :Prop = {
		import Numeric.Implicits.infixNumericOps
		testOrdering(x1, x2) && 
			((InOut(x1) + InOut(x2)).value ?= (x1 + x2)) :| "+" &&
			((InOut(x1) - InOut(x2)).value ?= (x1 - x2)) :| "-" &&
			((InOut(x1) * InOut(x2)).value ?= (x1 * x2)) :| "*" &&
			((-InOut(x1)).value ?= -x1) :| "unary_-"
	}
	

	property("InOutNumeric") = forAll { (x1 :Int, x2 :Int) => testNumeric(x1, x2) }
	
	
	private def testIntegral[T](x1 :T, x2 :T)(implicit t :Integral[T], inout :Integral[InOut[T]]) :Prop = {
		import Integral.Implicits.infixIntegralOps
		if (x2 == implicitly[Integral[T]].zero)
			testNumeric(x1, x2)
		else
			testNumeric(x1, x2) &&
				((InOut(x1) / InOut(x2)).value ?= (x1 / x2)) :| "/" &&
				((InOut(x1) % InOut(x2)).value ?= (x1 % x2)) :| "%"
	}
	
	property("InOutIntegral") = forAll { (x1 :Int, x2 :Int) => testIntegral(x1, x2) }


	private def testFractional[T](x1 :T, x2 :T)(implicit t :Fractional[T], inout :Fractional[InOut[T]]) :Prop = {
		import Fractional.Implicits.infixFractionalOps
		if (x2 == implicitly[Fractional[T]].zero)
			testNumeric(x1, x2)
		else
			testNumeric(x1, x2) && ((InOut(x1) / InOut(x2)).value ?= (x1 / x2)) :| "/"
	}

	property("InOutFractional") = forAll { (x1 :Double, x2 :Double) => testFractional(x1, x2) }
*/

}
