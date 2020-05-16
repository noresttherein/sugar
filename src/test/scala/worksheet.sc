import net.noresttherein.slang.funny.fun

class A(val name :String)
class B(val name :String)
object B {
	implicit def AtoB(a :A) :B = new B(a.name)
	implicit def BtoA(b :B) :A = new A(b.name)
}

val a = new A("a")
val b = a :B


