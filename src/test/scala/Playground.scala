
import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.unspecialized
import scala.collection.{IterableFactory, IterableFactoryDefaults, IterableOps, View}
import scala.collection.immutable.{AbstractSeq, ArraySeq}

import net.noresttherein.sugar.arrays.IArray
import net.noresttherein.sugar.collections.{ArraySliceBuffer, Cat, Cuboid2Iterator, Fingers, RelayArray, StrictView, StringSet, ValIterator}
import net.noresttherein.sugar.extensions.{ClassExtension, SeqExtension, classNameMethods}


//class Spec[@specialized T](param1 :T, param2 :T, val cls :Class[_]) {
//	def this(param1 :T, param2 :T) = this(param1, param2, param1.getClass)
//}

private object Playground extends App {
	val relay = RelayArray(1, 2, 3, 4, 5)
	val seq = ArraySeq(-1, -2, -3, -4)
	val concat = seq ++: relay
	println(concat)
	val cat = Cat.empty :+ 1
	val tail = cat.tail


/*
	var ok = true
	var i = 0
	while (i < 2048 && ok) {
		try {
			val b = Jack.newBuilder[Int]
			for (j <- 0 until i)
				b += j
			val jack = b.result()
			val range = 0 until i
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

