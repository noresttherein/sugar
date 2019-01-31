package net.turambar.slang

import net.turambar.slang.variables.Var
import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop._

/** The purpose of this test is mostly to verify that implicit patching works correctly, rather than testing simple assignments. 
  * @author Marcin Mościcki marcin@moscicki.net
  */
object VarSpec extends Properties("Var") {

	property(":=") = forAll { x :Int => val v = Var[Int](0); v := x;  v.get ?= x }

	property("Var[Boolean].&=") = forAll { (a :Boolean, b :Boolean) => 
		val v = Var(a); v &= b;  v.get ?= (a && b) 
	}
	property("Var[Boolean].&&=") = forAll { (a :Boolean, b :Boolean) =>
		var v = Var(a); var eager = false;
		v &&= {eager = true; b}
		(v.get ?= (a && b)) && (eager ?= a) //second argument computed only if first is true
	}
	property("Var[Boolean].|=") = forAll { (a :Boolean, b :Boolean) =>
		val v = Var(a); v |= b;  v.get ?= (a || b)
	}
	property("Var[Boolean].||=") = forAll { (a :Boolean, b :Boolean) =>
		var v = Var(a); var eager = false
		v ||= {eager = true; b}
		(v.get ?= (a || b)) && (eager ?= !a) //second argument computed only if first is false
	}
	property("Var[Boolean].^=") = forAll { (a :Boolean, b :Boolean) =>
		var v = Var(a); v ^= b; v.get ?= (a ^ b)
	}
//	property("Var[Boolean].not") = forAll { a :Boolean => 
//		var v = Var(a); v.not() v.get ?= !a
//	}
	
	
	
	
	
	property("Var[Int].+=") = forAll { (a :Int, b :Int) => 
		val v = Var(a); v += b; v.get ?= (a + b) 
	}
	property("Var[Int].-=") = forAll { (a :Int, b :Int) =>
		val v = Var(a); v -= b; v.get ?= (a - b)
	}
	property("Var[Int].*=") = forAll { (a :Int, b :Int) =>
		val v = Var(a); v *= b; v.get ?= (a * b)
	}
	property("Var[Int]./=") = forAll { (a :Int, b :Int) =>
		if (b != 0) { val v = Var(a); v /= b; v.get ?= (a / b) }
		else Prop(throws(classOf[ArithmeticException]) { var v = Var(a); v /= b})
	}
	property("Var[Int].%=") = forAll { (a :Int, b :Int) =>
		if (b != 0) { val v = Var(a); v %= b; v.get ?= (a % b) }
		else Prop(throws(classOf[ArithmeticException]) { var v = Var(a); v %= b})
	}
	property("Var[Int].++") = forAll { a :Int =>
		val v = Var(a); v.++; v.get ?= (a + 1)
	}
	property("Var[Int].--") = forAll { a :Int =>
		val v = Var(a); v.--; v.get ?= (a - 1)
	}

	property("Var[Int].&=") = forAll { (a :Int, b :Int) =>
		val v = Var(a); v &= b; v.get ?= (a & b)
	}
	property("Var[Int].|=") = forAll { (a :Int, b :Int) =>
		val v = Var(a); v |= b; v.get ?= (a | b)
	}
	property("Var[Int].^=") = forAll { (a :Int, b :Int) =>
		val v = Var(a); v ^= b; v.get ?= (a ^ b)
	}
	property("Var[Int].>>=") = forAll { (a :Int, b :Int) =>
		val v = Var(a); v >>= b; v.get ?= (a >> b)
	}
	property("Var[Int].>>>=") = forAll { (a :Int, b :Int) =>
		val v = Var(a); v >>>= b; v.get ?= (a >>> b)
	}
	property("Var[Int].<<=") = forAll { (a :Int, b :Int) =>
		val v = Var(a); v <<= b; v.get ?= (a << b)
	}
	
	property("Var[Int].flip()") = forAll { a :Int =>
		var v = Var(a); v.flip(); v.get ?= ~a
	}

	



	property("Var[Long].+=") = forAll { (a :Long, b :Long) =>
		val v = Var(a); v += b; v.get ?= (a + b)
	}
	property("Var[Long].-=") = forAll { (a :Long, b :Long) =>
		val v = Var(a); v -= b; v.get ?= (a - b)
	}
	property("Var[Long].*=") = forAll { (a :Long, b :Long) =>
		val v = Var(a); v *= b; v.get ?= (a * b)
	}
	property("Var[Long]./=") = forAll { (a :Long, b :Long) =>
		if (b != 0) { val v = Var(a); v /= b; v.get ?= (a / b) }
		else Prop(throws(classOf[ArithmeticException]) { var v = Var(a); v /= b})
	}
	property("Var[Long].%=") = forAll { (a :Long, b :Long) =>
		if (b != 0) { val v = Var(a); v %= b; v.get ?= (a % b) }
		else Prop(throws(classOf[ArithmeticException]) { var v = Var(a); v %= b})
	}
	property("Var[Long].++") = forAll { a :Long =>
		val v = Var(a); v.++; v.get ?= (a + 1)
	}
	property("Var[Long].--") = forAll { a :Long =>
		val v = Var(a); v.--; v.get ?= (a - 1)
	}

	property("Var[Long].&=") = forAll { (a :Long, b :Long) =>
		val v = Var(a); v &= b; v.get ?= (a & b)
	}
	property("Var[Long].|=") = forAll { (a :Long, b :Long) =>
		val v = Var(a); v |= b; v.get ?= (a | b)
	}
	property("Var[Long].^=") = forAll { (a :Long, b :Long) =>
		val v = Var(a); v ^= b; v.get ?= (a ^ b)
	}
	property("Var[Long].>>=") = forAll { (a :Long, b :Int) =>
		val v = Var(a); v >>= b; v.get ?= (a >> b)
	}
	property("Var[Long].>>>=") = forAll { (a :Long, b :Int) =>
		val v = Var(a); v >>>= b; v.get ?= (a >>> b)
	}
	property("Var[Long].<<=") = forAll { (a :Long, b :Int) =>
		val v = Var(a); v <<= b; v.get ?= (a << b)
	}

	property("Var[Long].flip()") = forAll { a :Long =>
		var v = Var(a); v.flip(); v.get ?= ~a
	}


	

	property("Var[Float].+=") = forAll { (a :Float, b :Float) =>
		val v = Var(a); v += b; v.get ?= (a + b)
	}
	property("Var[Float].-=") = forAll { (a :Float, b :Float) =>
		val v = Var(a); v -= b; v.get ?= (a - b)
	}
	property("Var[Float].*=") = forAll { (a :Float, b :Float) =>
		val v = Var(a); v *= b; v.get ?= (a * b)
	}
	property("Var[Float]./=") = forAll { (a :Float, b :Float) =>
		if (b != 0) { val v = Var(a); v /= b; v.get ?= (a / b) }
		else Prop(throws(classOf[ArithmeticException]) { var v = Var(a); v /= b})
	}




	property("Var[Double].+=") = forAll { (a :Double, b :Double) =>
		val v = Var(a); v += b; v.get ?= (a + b)
	}
	property("Var[Double].-=") = forAll { (a :Double, b :Double) =>
		val v = Var(a); v -= b; v.get ?= (a - b)
	}
	property("Var[Double].*=") = forAll { (a :Double, b :Double) =>
		val v = Var(a); v *= b; v.get ?= (a * b)
	}
	property("Var[Double]./=") = forAll { (a :Double, b :Double) =>
		if (b != 0) { val v = Var(a); v /= b; v.get ?= (a / b) }
		else Prop(throws(classOf[ArithmeticException]) { var v = Var(a); v /= b})
	}
	

}
