import scala.collection.mutable.ListBuffer

import net.noresttherein.sugar.collections.{OrderedItems, PassedArray, Ranking}
import net.noresttherein.sugar.extensions.IterableExtension

object Playground extends App {
//	println(Ranking(0, -1).scanLeft(0)(_ + _))

       class A { def boo = ??? }
       class B
       object B { implicit def c2a(c :C) :A = ??? }

       class C extends B
       trait BaseC// { implicit def c2a(c :C) :A = ??? }
       object C extends BaseC
	   (new C).boo

	println(reflect.runtime.universe.reify(
		(new C).boo
	))
}