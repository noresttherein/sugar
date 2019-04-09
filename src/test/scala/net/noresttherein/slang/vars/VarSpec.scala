package net.noresttherein.slang.vars

import net.noresttherein.slang.Lazy
import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop._



class VarPropsGroup(override val varClassName :String = "Var") extends BaseInOutPropsGroup {

	override def newVar[@specialized(Var.SpecializedTypes) T](x :T) :Var[T] = Var(x)


	/*************************** Test of `Var[Boolean]` and its implicit extensions ***************************************/

	class VarBooleanProps(name :String = varClassName + "[Boolean]") extends Properties(name) {

		val asInOut = new InOutBooleanProps("asInstanceOf[InOut[Boolean]]")

		include(asInOut) //test the polymorphic behaviour; methods below use direct, statically resolved calls


		property("&=") = forAll { (x1 :Boolean, x2 :Boolean) => val v = Var(x1); v &= x2; v.get ?= x1 & x2 }

		property("&&=") = forAll { (x1 :Boolean, x2 :Boolean) =>
			val v = Var(x1); val arg = Lazy(x2)
			v &&= arg
			(v.get ?= x1 && x2) :| "correctness" && (arg.isEvaluated ?= x1) :| "laziness"
		}

		property("|=") = forAll { (x1 :Boolean, x2 :Boolean) => val v = Var(x1); v |= x2; v.get ?= x1 | x2 }

		property("||=") = forAll { (x1 :Boolean, x2 :Boolean) =>
			val v = Var(x1); val arg = Lazy(x2)
			v ||= arg
			(v.get ?= x1 || x2) :| "correctness" || (arg.isUndefined ?= x1) :| "laziness"
		}

		property("^=") = forAll { (x1 :Boolean, x2 :Boolean) => val v = Var(x1); v ^= x2; v.get ?= x1 ^ x2 }


		property("flip()") = forAll { x :Boolean => val v = Var(x); v.flip(); v.get ?= !x }

		property("neg()") = forAll { x :Boolean => val v = Var(x); (v.neg() ?= !x) :| "returns" && (v.get ?= !x) :| "asisgns" }

	}







/*************************** Test of `Var[Int]` and its implicit extensions *******************************************/

	class VarIntProps(name :String = varClassName + "[Int]") extends Properties(name) {

		val asInOut = new InOutIntProps("asInstanceOf[InOut[Int]]")
		include(asInOut)


		property("+=") = forAll { (x1 :Int, x2 :Int) => val v = Var(x1); v += x2; v.get ?= x1 + x2 }

		property("inc(Int)") = forAll { (x1 :Int, x2 :Int) =>
			val v = Var(x1); ((v inc x2) ?= x1 + x2) :| "return" && (v.get ?= x1 + x2) :| "assign"
		}

		property("-=") = forAll { (x1 :Int, x2 :Int) => val v = Var(x1); v -= x2; v.get ?= x1 - x2 }

		property("dec(Int)") = forAll { (x1 :Int, x2 :Int) =>
			val v = Var(x1); ((v dec x2) ?= x1 - x2) :| "return" && (v.get ?= x1 - x2) :| "assign"
		}

		property("*=") = forAll { (x1 :Int, x2 :Int) => val v = Var(x1); v *= x2; v.get ?= x1 * x2 }

		property("mult") = forAll { (x1 :Int, x2 :Int) =>
			val v = Var(x1); ((v mult x2) ?= x1 * x2) :| "return" && (v.get ?= x1 * x2) :| "assign"
		}

		property("/=") = forAll { (x1 :Int, x2 :Int) =>
			val v = Var(x1)
			if (x2 != 0) { v /= x2; v.get ?= x1 / x2 }
			else Prop(throws(classOf[ArithmeticException]) { v /= x2 })
		}

		property("div") = forAll { (x1 :Int, x2 :Int) =>
			val v = Var(x1)
			if (x2 != 0) ((v div x2) ?= x1 / x2) :| "return" && (v.get ?= x1 / x2) :| "assign"
			else Prop(throws(classOf[ArithmeticException]) { v div x2 })
		}

		property("%=") = forAll { (x1 :Int, x2 :Int) =>
			val v = Var(x1)
			if (x2 != 0) {v %= x2; v.get ?= x1 % x2}
			else Prop(throws(classOf[ArithmeticException]) {v %= x2})
		}

		property("rem") = forAll { (x1 :Int, x2 :Int) =>
			val v = Var(x1)
			if (x2 != 0) ((v rem x2) ?= x1 % x2) :| "return" && (v.get ?= x1 % x2) :| "assign"
			else Prop(throws(classOf[ArithmeticException]) { v rem x2})
		}

		property("neg()") = forAll { x :Int => val v = Var(x); (v.neg() ?= -x) :| "return" && (v.get ?= -x) :| "assign" }

		property("++") = forAll { x :Int => val v = Var(x); v.++; v.get ?= x + 1 }

		property("inc()") = forAll { x1 :Int => val v = Var(x1); (v.inc() ?= x1 + 1) :| "return" && (v.get ?= x1 + 1) :| "assign"}

		property("--") = forAll { x :Int => val v = Var(x); v.--; v.get ?= x - 1 }

		property("dec()") = forAll { x1 :Int => val v = Var(x1); (v.dec() ?= x1 - 1) :| "return" && (v.get ?= x1 - 1) :| "assign"}



		property("|=") = forAll { (x1 :Int, x2 :Int) => val v = Var(x1); v |= x2; v.get ?= x1 | x2}

		property("&=") = forAll { (x1 :Int, x2 :Int) => val v = Var(x1); v &= x2; v.get ?= x1 & x2 }

		property("^=") = forAll { (x1 :Int, x2 :Int) => val v = Var(x1); v ^= x2; v.get ?= x1 ^ x2 }

		property(">>=") = forAll { (x :Int, n :Int) => val v = Var(x); v >>= n; v.get ?= x >> n }

		property(">>>=") = forAll { (x :Int, n :Int) => val v = Var(x); v >>>= n; v.get ?= x >>> n }

		property("<<=") = forAll { (x :Int, n :Int) => val v = Var(x); v <<= n; v.get ?= x << n }

		property("flip()") = forAll { x :Int => val v = Var(x); v.flip(); v.get ?= ~x }

	}







/*************************** Test of `Var[Long]` and its implicit extensions ******************************************/

	class VarLongProps(name :String = varClassName+"[Long]") extends Properties(name) {

		val asInOut = new InOutLongProps("asInstanceOf[InOut[Long]]")
		include(asInOut) //test the polymorphic behaviour; methods below use direct, statically resolved calls



		property("+=") = forAll { (x1 :Long, x2 :Long) => val v = Var(x1); v += x2; v.get ?= x1 + x2 }

		property("inc(Long)") = forAll { (x1 :Long, x2 :Long) =>
			val v = Var(x1); ((v inc x2) ?= x1 + x2) :| "return" && (v.get ?= x1 + x2) :| "assign"
		}

		property("-=") = forAll { (x1 :Long, x2 :Long) => val v = Var(x1); v -= x2; v.get ?= x1 - x2 }

		property("dec(Long)") = forAll { (x1 :Long, x2 :Long) =>
			val v = Var(x1); ((v dec x2) ?= x1 - x2) :| "return" && (v.get ?= x1 - x2) :| "assign"
		}

		property("*=") = forAll { (x1 :Long, x2 :Long) => val v = Var(x1); v *= x2; v.get ?= x1 * x2 }

		property("mult") = forAll { (x1 :Long, x2 :Long) =>
			val v = Var(x1); ((v mult x2) ?= x1 * x2) :| "return" && (v.get ?= x1 * x2) :| "assign"
		}

		property("/=") = forAll { (x1 :Long, x2 :Long) =>
			val v = Var(x1)
			if (x2 != 0) { v /= x2; v.get ?= x1 / x2 }
			else Prop(throws(classOf[ArithmeticException]) { v /= x2 })
		}

		property("div") = forAll { (x1 :Long, x2 :Long) =>
			val v = Var(x1)
			if (x2 != 0) ((v div x2) ?= x1 / x2) :| "return" && (v.get ?= x1 / x2) :| "assign"
			else Prop(throws(classOf[ArithmeticException]) { v div x2 })
		}

		property("%=") = forAll { (x1 :Long, x2 :Long) =>
			val v = Var(x1)
			if (x2 != 0) {v %= x2; v.get ?= x1 % x2}
			else Prop(throws(classOf[ArithmeticException]) {v %= x2})
		}

		property("rem") = forAll { (x1 :Long, x2 :Long) =>
			val v = Var(x1)
			if (x2 != 0) ((v rem x2) ?= x1 % x2) :| "return" && (v.get ?= x1 % x2) :| "assign"
			else Prop(throws(classOf[ArithmeticException]) { v rem x2})
		}

		property("++") = forAll { x :Long => val v = Var(x); v.++; v.get ?= x + 1 }

		property("inc()") = forAll { x1 :Long => val v = Var(x1); (v.inc() ?= x1 + 1) :| "return" && (v.get ?= x1 + 1) :| "assign"}

		property("--") = forAll { x :Long => val v = Var(x); v.--; v.get ?= x - 1 }

		property("dec()") = forAll { x1 :Long => val v = Var(x1); (v.dec() ?= x1 - 1) :| "return" && (v.get ?= x1 - 1) :| "assign"}

		property("neg()") = forAll { x :Long => val v = Var(x); (v.neg() ?= -x) :| "return" && (v.get ?= -x) :| "assign" }


		property("|=") = forAll { (x1 :Long, x2 :Long) => val v = Var(x1); v |= x2; v.get ?= x1 | x2}

		property("&=") = forAll { (x1 :Long, x2 :Long) => val v = Var(x1); v &= x2; v.get ?= x1 & x2 }

		property("^=") = forAll { (x1 :Long, x2 :Long) => val v = Var(x1); v ^= x2; v.get ?= x1 ^ x2 }

		property(">>=") = forAll { (x :Long, n :Long) => val v = Var(x); v >>= n.toInt; v.get ?= x >> n }

		property(">>>=") = forAll { (x :Long, n :Long) => val v = Var(x); v >>>= n.toInt; v.get ?= x >>> n }

		property("<<=") = forAll { (x :Long, n :Long) => val v = Var(x); v <<= n.toInt; v.get ?= x << n }

		property("flip()") = forAll { x :Long => val v = Var(x); v.flip(); v.get ?= ~x }

	}






/************************* Test of `Var[Float]` and its implicit extensions *******************************************/

	class VarFloatProps(name :String = varClassName + "[Float]") extends Properties(name) {

		val asInOut = new InOutFloatProps("asInstanceOf[InOut[Float]]")
		include(asInOut) //test the polymorphic behaviour; methods below use direct, statically resolved calls




		property("+=") = forAll { (x1 :Float, x2 :Float) => val v = Var(x1); v += x2; v.get ?= x1 + x2 }

		property("inc(Float)") = forAll { (x1 :Float, x2 :Float) =>
			val v = Var(x1); ((v inc x2) ?= x1 + x2) :| "return" && (v.get ?= x1 + x2) :| "assign"
		}

		property("-=") = forAll { (x1 :Float, x2 :Float) => val v = Var(x1); v -= x2; v.get ?= x1 - x2 }

		property("dec(Float)") = forAll { (x1 :Float, x2 :Float) =>
			val v = Var(x1); ((v dec x2) ?= x1 - x2) :| "return" && (v.get ?= x1 - x2) :| "assign"
		}

		property("*=") = forAll { (x1 :Float, x2 :Float) => val v = Var(x1); v *= x2; v.get ?= x1 * x2 }

		property("mult") = forAll { (x1 :Float, x2 :Float) =>
			val v = Var(x1); ((v mult x2) ?= x1 * x2) :| "return" && (v.get ?= x1 * x2) :| "assign"
		}

		property("/=") = forAll { (x1 :Float, x2 :Float) =>
			val v = Var(x1)
			if (x2 != 0) { v /= x2; v.get ?= x1 / x2 }
			else Prop(throws(classOf[ArithmeticException]) { v /= x2 })
		}

		property("div") = forAll { (x1 :Float, x2 :Float) =>
			val v = Var(x1)
			if (x2 != 0) ((v div x2) ?= x1 / x2) :| "return" && (v.get ?= x1 / x2) :| "assign"
			else Prop(throws(classOf[ArithmeticException]) { v div x2 })
		}

		property("neg()") = forAll { x :Float => val v = Var(x); (v.neg() ?= -x) :| "return" && (v.get ?= -x) :| "assign" }

	}








/*************************** Test of `Var[Double]` and its implicit extensions ****************************************/

	class VarDoubleProps(name :String = varClassName + "[Double]") extends Properties(name) {

		val asInOut = new InOutDoubleProps("asInstanceOf[InOut[Double]]")
		include(asInOut) //test the polymorphic behaviour; methods below use direct, statically resolved calls




		property("+=") = forAll { (x1 :Double, x2 :Double) => val v = Var(x1); v += x2; v.get ?= x1 + x2 }

		property("inc(Double)") = forAll { (x1 :Double, x2 :Double) =>
			val v = Var(x1); ((v inc x2) ?= x1 + x2) :| "return" && (v.get ?= x1 + x2) :| "assign"
		}

		property("-=") = forAll { (x1 :Double, x2 :Double) => val v = Var(x1); v -= x2; v.get ?= x1 - x2 }

		property("dec(Double)") = forAll { (x1 :Double, x2 :Double) =>
			val v = Var(x1); ((v dec x2) ?= x1 - x2) :| "return" && (v.get ?= x1 - x2) :| "assign"
		}

		property("*=") = forAll { (x1 :Double, x2 :Double) => val v = Var(x1); v *= x2; v.get ?= x1 * x2 }

		property("mult") = forAll { (x1 :Double, x2 :Double) =>
			val v = Var(x1); ((v mult x2) ?= x1 * x2) :| "return" && (v.get ?= x1 * x2) :| "assign"
		}

		property("/=") = forAll { (x1 :Double, x2 :Double) =>
			val v = Var(x1)
			if (x2 != 0) { v /= x2; v.get ?= x1 / x2 }
			else Prop(throws(classOf[ArithmeticException]) { v /= x2 })
		}

		property("div") = forAll { (x1 :Double, x2 :Double) =>
			val v = Var(x1)
			if (x2 != 0) ((v div x2) ?= x1 / x2) :| "return" && (v.get ?= x1 / x2) :| "assign"
			else Prop(throws(classOf[ArithmeticException]) { v div x2 })
		}

		property("neg()") = forAll { x :Double => val v = Var(x); (v.neg() ?= -x) :| "return" && (v.get ?= -x) :| "assign" }

	}



	def includeIn(container :Properties) :Unit = {
		import container.include

		include(new VarBooleanProps)
		include(new VarIntProps)
		include(new VarLongProps)
		include(new VarFloatProps)
		include(new VarDoubleProps)
		include(new InOutCharProps(varClassName + "[Char]"))
		include(new InOutShortProps(varClassName + "[Short]"))
		include(new InOutByteProps(varClassName + "[Byte]"))
		include(new InOutBigIntProps(varClassName + "[BigInt]"))

	}

}



class ErasedVarPropsGroup extends VarPropsGroup("Var<erased>") {
	override def newVar[@specialized(Var.SpecializedTypes) T](x :T) :Var[T] = erasedVar(x)

	def erasedVar[T](x :T) :Var[T] = Var(x)
}



/** The purpose of this test is mostly to verify that implicit patching works correctly, rather than testing simple assignments.
  * @author Marcin Mościcki marcin@moscicki.net
  */
object VarSpec extends Properties("vars.Var") {

	(new VarPropsGroup).includeIn(this)
	(new ErasedVarPropsGroup).includeIn(this)

	property("implicits.InOutMultiAssignment") = forAll { x :String =>
		import Var.implicits.VarMultiAssignment
		val v1 = Var("a"); val v2 = Var("b"); val v3 = Var("d")
		v1 =: v2 =: v3 =: x
		(v1.get ?= x) && (v2.get ?= x) && (v3.get ?= x)

	}


}
