
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.unspecialized
import scala.collection.{IterableFactory, IterableFactoryDefaults, IterableOps, View}
import scala.collection.immutable.{AbstractSeq, ArraySeq}

import net.noresttherein.sugar.arrays.{ArrayCompanionExtension, IArray, IRefArray}
import net.noresttherein.sugar.collections.{ArraySliceBuffer, Cat, Cuboid2Iterator, Fingers, RelayArray, StrictView, StringSet, ValIterator}
import net.noresttherein.sugar.extensions.{ClassExtension, IterableOnceExtension, SeqExtension, classNameMethods}
import net.noresttherein.sugar.slang.SerializationProxy


//class Spec[@specialized T](param1 :T, param2 :T, val cls :Class[_]) {
//	def this(param1 :T, param2 :T) = this(param1, param2, param1.getClass)
//}


private object Playground extends App {
	val nothing = new Array[Nothing](0)
	val any = new Array[Any](0).asInstanceOf[Array[Nothing]]
	val arrayNothing = Array.emptyNothingArray
	val iArrayNothing = IArray.emptyNothingArray
	val iRefArrayNothing = IRefArray.ofNothing
	import net.noresttherein.sugar.collections.CatSpec._
	val cat = Cat(2147483647, -649280805, 958591695, -1).patch(1, Vector(0, 0), 0)
	val vector = cat.toVector
	val xs = new Array[Int](1) //Array.iterate(0, 100)(_ + 1)//new Array[Int](100)
	val ys = new Array[Int](1) //Array.iterate(0, 100)(_ + 1)//new Array[Int](100)
//	cat.copyRangeToArray(xs, 0, 0, Int.MaxValue)
//	vector.copyRangeToArray(ys, 0, 0, Int.MaxValue)
//	val a1 = ArraySeq.unsafeWrapArray(xs)
//	val a2 = ArraySeq.unsafeWrapArray(ys)
//	println(a1 zip a2)
//	println(a1 == a2)
//	println(cat.length)
	val init = cat.init
	println(init zip vector.init)
	println(init == vector.init)
	println(cat.length)
	println(init.length)
/*
	var ok = true
	var i = 0
	var jack = Fingers.empty[Int]
	while (i < 2048 && ok) {
		try {
			jack :+= i
			val range = 0 to i
			ok = jack == range
			if (!ok) {
				println(i.toString + ":")
				println(jack)
			}
		} catch {
			case e :Exception =>
				System.err.println(i.toString + ": " + e)
				throw e
		}
		i += 1
	}
*/
}

