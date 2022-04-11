package net.noresttherein.sugar.vars

import net.noresttherein.sugar.vars.InOut.SpecializedVars
import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop._


class SyncVarPropsGroup(override val varClassName :String = "SyncVar") extends BaseInOutPropsGroup {

	override def newVar[@specialized(SpecializedVars) T](x :T) :SyncVar[T] = SyncVar(x)



/************************* Test of `SyncVar[Boolean]` and its implicit extensions**************************************/

	class SyncVarBooleanProps(name :String = varClassName + "[Boolean]") extends Properties(name) {

		val asInOut = new InOutBooleanProps("asInstanceOf[InOut[Boolean]]")

		include(asInOut) //test the polymorphic behaviour; methods below use direct, statically resolved calls


		property("&=") = forAll { (x1 :Boolean, x2 :Boolean) => val v = SyncVar(x1); v &= x2; v.value ?= x1 & x2 }

		property("&&=") = forAll { (x1 :Boolean, x2 :Boolean) =>
			val v = SyncVar(x1); val arg = Lazy(x2)
			v &&= arg
			(v.value ?= x1 && x2) :| "correctness" && (arg.isDefined ?= x1) :| "laziness"
		}

		property("|=") = forAll { (x1 :Boolean, x2 :Boolean) => val v = SyncVar(x1); v |= x2; v.value ?= x1 | x2 }

		property("||=") = forAll { (x1 :Boolean, x2 :Boolean) =>
			val v = SyncVar(x1); val arg = Lazy(x2)
			v ||= arg
			(v.value ?= x1 || x2) :| "correctness" || (arg.isDefined ?= x1) :| "laziness"
		}

		property("^=") = forAll { (x1 :Boolean, x2 :Boolean) => val v = SyncVar(x1); v ^= x2; v.value ?= x1 ^ x2 }


		property("flip()") = forAll { x :Boolean => val v = SyncVar(x); v.flip(); v.value ?= !x }

		property("neg()") = forAll { x :Boolean => val v = SyncVar(x); (v.neg() ?= !x) :| "returns" && (v.value ?= !x) :| "asisgns" }

	}







/************************* Test of `SyncVar[Int]` and its implicit extensions *****************************************/

	class SyncVarIntProps(name :String = varClassName + "[Int]") extends Properties(name) {

		val asInOut = new InOutIntProps("asInstanceOf[InOut[Int]]")

		include(asInOut)


		property("+=") = forAll { (x1 :Int, x2 :Int) => val v = SyncVar(x1); v += x2; v.value ?= x1 + x2 }

		property("inc(Int)") = forAll { (x1 :Int, x2 :Int) =>
			val v = SyncVar(x1); ((v inc x2) ?= x1 + x2) :| "return" && (v.value ?= x1 + x2) :| "assign"
		}

		property("-=") = forAll { (x1 :Int, x2 :Int) => val v = SyncVar(x1); v -= x2; v.value ?= x1 - x2 }

		property("dec(Int)") = forAll { (x1 :Int, x2 :Int) =>
			val v = SyncVar(x1); ((v dec x2) ?= x1 - x2) :| "return" && (v.value ?= x1 - x2) :| "assign"
		}

		property("*=") = forAll { (x1 :Int, x2 :Int) => val v = SyncVar(x1); v *= x2; v.value ?= x1 * x2 }

		property("mult") = forAll { (x1 :Int, x2 :Int) =>
			val v = SyncVar(x1); ((v mult x2) ?= x1 * x2) :| "return" && (v.value ?= x1 * x2) :| "assign"
		}

		property("/=") = forAll { (x1 :Int, x2 :Int) =>
			val v = SyncVar(x1)
			if (x2 != 0) { v /= x2; v.value ?= x1 / x2 }
			else Prop(throws(classOf[ArithmeticException]) { v /= x2 })
		}

		property("div") = forAll { (x1 :Int, x2 :Int) =>
			val v = SyncVar(x1)
			if (x2 != 0) ((v div x2) ?= x1 / x2) :| "return" && (v.value ?= x1 / x2) :| "assign"
			else Prop(throws(classOf[ArithmeticException]) { v div x2 })
		}

		property("%=") = forAll { (x1 :Int, x2 :Int) =>
			val v = SyncVar(x1)
			if (x2 != 0) {v %= x2; v.value ?= x1 % x2}
			else Prop(throws(classOf[ArithmeticException]) {v %= x2})
		}

		property("rem") = forAll { (x1 :Int, x2 :Int) =>
			val v = SyncVar(x1)
			if (x2 != 0) ((v rem x2) ?= x1 % x2) :| "return" && (v.value ?= x1 % x2) :| "assign"
			else Prop(throws(classOf[ArithmeticException]) { v rem x2})
		}

		property("neg()") = forAll { x :Int => val v = SyncVar(x); (v.neg() ?= -x) :| "return" && (v.value ?= -x) :| "assign" }

		property("++") = forAll { x :Int => val v = SyncVar(x); v.++; v.value ?= x + 1 }

		property("inc()") = forAll { x1 :Int => val v = SyncVar(x1); (v.inc() ?= x1 + 1) :| "return" && (v.value ?= x1 + 1) :| "assign"}

		property("--") = forAll { x :Int => val v = SyncVar(x); v.--; v.value ?= x - 1 }

		property("dec()") = forAll { x1 :Int => val v = SyncVar(x1); (v.dec() ?= x1 - 1) :| "return" && (v.value ?= x1 - 1) :| "assign"}



		property("|=") = forAll { (x1 :Int, x2 :Int) => val v = SyncVar(x1); v |= x2; v.value ?= x1 | x2}

		property("&=") = forAll { (x1 :Int, x2 :Int) => val v = SyncVar(x1); v &= x2; v.value ?= x1 & x2 }

		property("^=") = forAll { (x1 :Int, x2 :Int) => val v = SyncVar(x1); v ^= x2; v.value ?= x1 ^ x2 }

		property(">>=") = forAll { (x :Int, n :Int) => val v = SyncVar(x); v >>= n; v.value ?= x >> n }

		property(">>>=") = forAll { (x :Int, n :Int) => val v = SyncVar(x); v >>>= n; v.value ?= x >>> n }

		property("<<=") = forAll { (x :Int, n :Int) => val v = SyncVar(x); v <<= n; v.value ?= x << n }

		property("flip()") = forAll { x :Int => val v = SyncVar(x); v.flip(); v.value ?= ~x }

	}







/************************* Test of `SyncVar[Long]` and its implicit extensions ****************************************/

	class SyncVarLongProps(name :String = varClassName + "[Long]") extends Properties(name) {

		val asInOut = new InOutLongProps("asInstanceOf[InOut[Long]]")

		include(asInOut) //test the polymorphic behaviour; methods below use direct, statically resolved calls



		property("+=") = forAll { (x1 :Long, x2 :Long) => val v = SyncVar(x1); v += x2; v.value ?= x1 + x2 }

		property("inc(Long)") = forAll { (x1 :Long, x2 :Long) =>
			val v = SyncVar(x1); ((v inc x2) ?= x1 + x2) :| "return" && (v.value ?= x1 + x2) :| "assign"
		}

		property("-=") = forAll { (x1 :Long, x2 :Long) => val v = SyncVar(x1); v -= x2; v.value ?= x1 - x2 }

		property("dec(Long)") = forAll { (x1 :Long, x2 :Long) =>
			val v = SyncVar(x1); ((v dec x2) ?= x1 - x2) :| "return" && (v.value ?= x1 - x2) :| "assign"
		}

		property("*=") = forAll { (x1 :Long, x2 :Long) => val v = SyncVar(x1); v *= x2; v.value ?= x1 * x2 }

		property("mult") = forAll { (x1 :Long, x2 :Long) =>
			val v = SyncVar(x1); ((v mult x2) ?= x1 * x2) :| "return" && (v.value ?= x1 * x2) :| "assign"
		}

		property("/=") = forAll { (x1 :Long, x2 :Long) =>
			val v = SyncVar(x1)
			if (x2 != 0) { v /= x2; v.value ?= x1 / x2 }
			else Prop(throws(classOf[ArithmeticException]) { v /= x2 })
		}

		property("div") = forAll { (x1 :Long, x2 :Long) =>
			val v = SyncVar(x1)
			if (x2 != 0) ((v div x2) ?= x1 / x2) :| "return" && (v.value ?= x1 / x2) :| "assign"
			else Prop(throws(classOf[ArithmeticException]) { v div x2 })
		}

		property("%=") = forAll { (x1 :Long, x2 :Long) =>
			val v = SyncVar(x1)
			if (x2 != 0) {v %= x2; v.value ?= x1 % x2}
			else Prop(throws(classOf[ArithmeticException]) {v %= x2})
		}

		property("rem") = forAll { (x1 :Long, x2 :Long) =>
			val v = SyncVar(x1)
			if (x2 != 0) ((v rem x2) ?= x1 % x2) :| "return" && (v.value ?= x1 % x2) :| "assign"
			else Prop(throws(classOf[ArithmeticException]) { v rem x2})
		}

		property("++") = forAll { x :Long => val v = SyncVar(x); v.++; v.value ?= x + 1 }

		property("inc()") = forAll { x1 :Long => val v = SyncVar(x1); (v.inc() ?= x1 + 1) :| "return" && (v.value ?= x1 + 1) :| "assign"}

		property("--") = forAll { x :Long => val v = SyncVar(x); v.--; v.value ?= x - 1 }

		property("dec()") = forAll { x1 :Long => val v = SyncVar(x1); (v.dec() ?= x1 - 1) :| "return" && (v.value ?= x1 - 1) :| "assign"}

		property("neg()") = forAll { x :Long => val v = SyncVar(x); (v.neg() ?= -x) :| "return" && (v.value ?= -x) :| "assign" }


		property("|=") = forAll { (x1 :Long, x2 :Long) => val v = SyncVar(x1); v |= x2; v.value ?= x1 | x2}

		property("&=") = forAll { (x1 :Long, x2 :Long) => val v = SyncVar(x1); v &= x2; v.value ?= x1 & x2 }

		property("^=") = forAll { (x1 :Long, x2 :Long) => val v = SyncVar(x1); v ^= x2; v.value ?= x1 ^ x2 }

		property(">>=") = forAll { (x :Long, n :Long) => val v = SyncVar(x); v >>= n.toInt; v.value ?= x >> n }

		property(">>>=") = forAll { (x :Long, n :Long) => val v = SyncVar(x); v >>>= n.toInt; v.value ?= x >>> n }

		property("<<=") = forAll { (x :Long, n :Long) => val v = SyncVar(x); v <<= n.toInt; v.value ?= x << n }

		property("flip()") = forAll { x :Long => val v = SyncVar(x); v.flip(); v.value ?= ~x }

	}






/************************* Test of `SyncVar[Float]` and its implicit extensions ***************************************/

	class SyncVarFloatProps(name :String = varClassName + "[Float]") extends Properties(name) {

		val asInOut = new InOutFloatProps("asInstanceOf[InOut[Float]]")

		include(asInOut) //test the polymorphic behaviour; methods below use direct, statically resolved calls




		property("+=") = forAll { (x1 :Float, x2 :Float) => val v = SyncVar(x1); v += x2; v.value ?= x1 + x2 }

		property("inc(Float)") = forAll { (x1 :Float, x2 :Float) =>
			val v = SyncVar(x1); ((v inc x2) ?= x1 + x2) :| "return" && (v.value ?= x1 + x2) :| "assign"
		}

		property("-=") = forAll { (x1 :Float, x2 :Float) => val v = SyncVar(x1); v -= x2; v.value ?= x1 - x2 }

		property("dec(Float)") = forAll { (x1 :Float, x2 :Float) =>
			val v = SyncVar(x1); ((v dec x2) ?= x1 - x2) :| "return" && (v.value ?= x1 - x2) :| "assign"
		}

		property("*=") = forAll { (x1 :Float, x2 :Float) => val v = SyncVar(x1); v *= x2; v.value ?= x1 * x2 }

		property("mult") = forAll { (x1 :Float, x2 :Float) =>
			val v = SyncVar(x1); ((v mult x2) ?= x1 * x2) :| "return" && (v.value ?= x1 * x2) :| "assign"
		}

		property("/=") = forAll { (x1 :Float, x2 :Float) =>
			val v = SyncVar(x1)
			if (x2 != 0) { v /= x2; v.value ?= x1 / x2 }
			else Prop(throws(classOf[ArithmeticException]) { v /= x2 })
		}

		property("div") = forAll { (x1 :Float, x2 :Float) =>
			val v = SyncVar(x1)
			if (x2 != 0) ((v div x2) ?= x1 / x2) :| "return" && (v.value ?= x1 / x2) :| "assign"
			else Prop(throws(classOf[ArithmeticException]) { v div x2 })
		}

		property("neg()") = forAll { x :Float => val v = SyncVar(x); (v.neg() ?= -x) :| "return" && (v.value ?= -x) :| "assign" }

	}








/************************* Test of `SyncVar[Double]` and its implicit extensions **************************************/

	class SyncVarDoubleProps(name :String = varClassName + "[Double]") extends Properties(name) {

		val asInOut = new InOutDoubleProps("asInstanceOf[InOut[Double]]")

		include(asInOut) //test the polymorphic behaviour; methods below use direct, statically resolved calls



		property("+=") = forAll { (x1 :Double, x2 :Double) => val v = SyncVar(x1); v += x2; v.value ?= x1 + x2 }

		property("inc(Double)") = forAll { (x1 :Double, x2 :Double) =>
			val v = SyncVar(x1); ((v inc x2) ?= x1 + x2) :| "return" && (v.value ?= x1 + x2) :| "assign"
		}

		property("-=") = forAll { (x1 :Double, x2 :Double) => val v = SyncVar(x1); v -= x2; v.value ?= x1 - x2 }

		property("dec(Double)") = forAll { (x1 :Double, x2 :Double) =>
			val v = SyncVar(x1); ((v dec x2) ?= x1 - x2) :| "return" && (v.value ?= x1 - x2) :| "assign"
		}

		property("*=") = forAll { (x1 :Double, x2 :Double) => val v = SyncVar(x1); v *= x2; v.value ?= x1 * x2 }

		property("mult") = forAll { (x1 :Double, x2 :Double) =>
			val v = SyncVar(x1); ((v mult x2) ?= x1 * x2) :| "return" && (v.value ?= x1 * x2) :| "assign"
		}

		property("/=") = forAll { (x1 :Double, x2 :Double) =>
			val v = SyncVar(x1)
			if (x2 != 0) { v /= x2; v.value ?= x1 / x2 }
			else Prop(throws(classOf[ArithmeticException]) { v /= x2 })
		}

		property("div") = forAll { (x1 :Double, x2 :Double) =>
			val v = SyncVar(x1)
			if (x2 != 0) ((v div x2) ?= x1 / x2) :| "return" && (v.value ?= x1 / x2) :| "assign"
			else Prop(throws(classOf[ArithmeticException]) { v div x2 })
		}

		property("neg()") = forAll { x :Double => val v = SyncVar(x); (v.neg() ?= -x) :| "return" && (v.value ?= -x) :| "assign" }

	}






	def includeIn(container :Properties) :Unit = {
		import container.include

		include(new SyncVarBooleanProps)
		include(new SyncVarIntProps)
		include(new SyncVarLongProps)
		include(new SyncVarFloatProps)
		include(new SyncVarDoubleProps)
		include(new InOutCharProps(varClassName + "[Char]"))
		include(new InOutShortProps(varClassName + "[Short]"))
		include(new InOutByteProps(varClassName + "[Byte]"))
		include(new InOutBigIntProps(varClassName + "[BigInt]"))

	}

}


class ErasedSyncVarPropsGroup extends SyncVarPropsGroup("SyncVar<erased>") {
	override def newVar[@specialized(SpecializedVars) T](x :T) :SyncVar[T] = erasedVar(x)

	def erasedVar[T](x :T) :SyncVar[T] = SyncVar[T](x)
}


/** The purpose of this test is mostly to verify that implicit patching works correctly, rather than testing simple assignments.
  * @author Marcin Mościcki marcin@moscicki.net
  */
object SyncVarSpec extends Properties("vars.SyncVar") {

	(new SyncVarPropsGroup).includeIn(this)
	(new ErasedSyncVarPropsGroup).includeIn(this)

	def testOrdering[T](x1 :T, x2 :T)(implicit t :Ordering[T], inout :Ordering[SyncVar[T]]) :Prop = {
		import Ordering.Implicits.infixOrderingOps
		if (x1 < x2) Prop(SyncVar(x1) < SyncVar(x2)) :| "<"
		else Prop(SyncVar(x1) >= SyncVar(x2)) :| ">="
	}

	property("SyncVarOrdering") = forAll { (x1 :String, x2 :String) => testOrdering(x1, x2) }
}
