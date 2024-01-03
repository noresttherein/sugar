import java.lang.System.identityHashCode

import scala.annotation.unspecialized
import scala.collection.immutable.{ArraySeq, TreeSet}
import scala.collection.mutable.ArrayBuffer

import net.noresttherein.sugar.collections.{ArraySliceBuffer, StringSet}
import net.noresttherein.sugar.collections.extensions.IteratorExtension
import net.noresttherein.sugar.vars.{PhantomRef, Pure, SoftRef, Transient, Volatile, Watched, WeakRef}
import shapeless.Lazy


trait BaseVar[T] {
	def value :T
	def value_=(value :T) :Unit
	def getVal :Any
	def setVal(value :Any) :Unit
	def setRef(value :Any) :Unit
	def getRef :Any
}

trait SpecVar[@specialized T] extends BaseVar[T] {
	override def value :T = getSpec
	override def value_=(value :T) :Unit = {
		setVal(value)
		setRef(null)
	}
	def getSpec :T
	def setSpec(value :T) :Unit
}

class SimpleSpecVar[@specialized T](private[this] var x :T) extends SpecVar[T] {
	override def getSpec :T = x
	override def setSpec(value :T) :Unit = x = value
	override def getVal :Any = x
	override def setVal(value :Any) :Unit = x = value.asInstanceOf[T]
	@unspecialized override def getRef :T = x
	@unspecialized override def setRef(value :Any) :Unit = x = value.asInstanceOf[T]
}

trait TopSandwichVar[T] { this :SpecVar[T] =>
//	@unspecialized override def value :T =
}

trait BottomSandwichVar[T] extends BaseVar[T] {
	override def value :T = {
		var res = getRef
		if (res == null) {
			res = getVal
			setRef(res)
		}
		res.asInstanceOf[T]
	}
	override def value_=(value :T) :Unit = {
		setVal(value)
		setRef(value)
	}
}

class ValVar[@specialized T]
	extends TopSandwichVar[T] with SpecVar[T] with BottomSandwichVar[T]
{
	private[this] var x :T = _

	private[this] final def initialize(value :T) :Unit = x = value

	override def getSpec :T = x
	override def setSpec(value :T) :Unit = x = value

	override def getVal :Any = getSpec
	override def setVal(value :Any) :Unit = setSpec(value.asInstanceOf[T])

	/** Method, which in the generic version of this class, returns the value of the erased field,
	  * and in the specialized version boxed specialized field.
	  */
	private[this] final def get :Any = x

	/** Method, which in the generic version of this class, sets the value of the erased field,
	  * and in the specialized version unboxes the argument to a value type and sets the specialized field.
	  */
	private[this] final def set(value :Any) :Unit = x = value.asInstanceOf[T]

	/** Unspecialized method, which has a definition only in the erased version of this class,
	  * and hence always returns the value of the erased field (including for the specialized class).
	  */
	override def getRef :Any = get

	/** Unspecialized method, which has a definition only in the erased version of this class,
	  * and hence always sets the value of the erased field (including for the specialized class).
	  */
	override def setRef(value :Any) :Unit = set(value)
}

object ValVar {
	def apply[@specialized T](value :T) :ValVar[T] = {
		val res = new ValVar[T]
		res.setSpec(value)
		res
	}
}

//abstract class AbstractVar[T] extends SpecVar[T] {
//	private[this] var x :T
//}

object Playground extends App {
	val Var1 = ValVar(10000001)
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
}

