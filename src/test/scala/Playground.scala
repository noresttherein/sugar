import java.lang.System.identityHashCode

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.{switch, unspecialized}
import scala.collection.immutable.{ArraySeq, TreeSet}
import scala.collection.mutable.ArrayBuffer

import net.noresttherein.sugar.collections.{Cuboid2Iterator, ArraySliceBuffer, StringSet}
import net.noresttherein.sugar.collections.extensions.IteratorExtension
import net.noresttherein.sugar.vars.Maybe.Yes
import net.noresttherein.sugar.vars.{EvalOpt, LocalLazy, Maybe, PhantomRef, Pure, SoftRef, Transient, Volatile, VolatileOut, Watched, WeakRef, Opt}
import net.noresttherein.sugar.witness.NullValue
import shapeless.Lazy




object Playground extends App {

/*
	val jack = 0 until 15 to Jack
	jack to List
	println(jack)
	var ok = true
	var i = 0
	while (i < 256 && ok) {
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
		i += 1
	}
*/
}

