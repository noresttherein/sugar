import java.lang.System.identityHashCode

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.{switch, unspecialized}
import scala.collection.immutable.{ArraySeq, TreeSet}
import scala.collection.mutable.ArrayBuffer

import net.noresttherein.sugar.collections.{ArraySliceBuffer, StringSet}
import net.noresttherein.sugar.collections.extensions.IteratorExtension
import net.noresttherein.sugar.vars.{PhantomRef, Pure, SoftRef, Transient, Volatile, VolatileOut, Watched, WeakRef}
import shapeless.Lazy




object Playground extends App {
	val out = VolatileOut[Int]()
	out := 42
	out.value
/*	val Var1 = ValVar(10000001)
	val Var2 :SpecVar[Int] = Var1
	val Var3 :BaseVar[Int] = Var1
	def printState() :Unit = {
		val ref = Var1.getRef
		val spec = Var1.value
		def generic[T](v :BaseVar[T]) :Unit = {
			val erased = v.value
			println(s"erased=${erased}@${identityHashCode(erased)}")
		}
		print(s"ref=${ref}@${identityHashCode(ref)}; value=${spec}; ")
		generic(Var1)
	}

	printState()
	Var1.value = 20000001
	printState()
	Var1.setRef(30000001)
	printState()
	Var3.value = 40000001
	printState()
/*	val v1   = Var1.value
	println(s"Concrete (10000001): $v1")
	val r1   = Var1.getRef
	println(s"Ref (10000001): $r1")
	val e1
	Var1.setRef(20000001)
	val r2   = Var1.getRef
	println(s"Ref (20000001): $r2")
	val v2   = Var1.value
	println(s"Concrete(10000001): $v2")
	Var1.value = 30000001
	val v3 = Var1.value
	println(s"Concrete (30000001): $v3")
	val r3 = Var1.getRef
	println(s"Ref (20000001): $r3")
	val v4 = Var2.value
	println(s"Super (20000001): $v4")
	Var2.value = 30000001
	val v5 = Var2.value
	println(s"Super (30000001): $v5")

	genericAccess(Var1, 30000001, 40000001)

	val Var3 = new SimpleSpecVar(10000001)
	val v6   = Var3.value
	println(s"Simple (10000001): $v6")
	Var3.value = 20000001
	val v7   = Var3.value
	println(s"Simple (20000001): $v7")

	genericAccess(Var3, 20000001, 30000001)
*/
	def genericAccess[T](v :SpecVar[T], expect :T, value :T) :Unit = {
		val v1 = v.value
		val v2 = v.value
		println(s"Generic ($expect): $v1, $v2")
		v.value = value
		val v3 = v.value
		println(s"Generic ($value): $v3")
		println("Box preserved: " + (v1.asInstanceOf[AnyRef] eq v2.asInstanceOf[AnyRef]))
	}
*/
}

