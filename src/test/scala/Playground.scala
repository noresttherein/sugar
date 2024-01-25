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



//class Spec0[@specialized(Int) A](a :Array[A]) {
//	private[this] var idx = 0
//	private[this] var head = a(idx)
//}
//
//class Spec1[@specialized(Int) A](a :Array[A]) {
//	private[this] var idx = 0
//	private var head = a(idx)
//}
//
//class Spec2[@specialized(Int) A](a :Array[A]) {
//	private var idx = 0
//	private[this] var head = a(idx)
//}
//
//class Spec3[@specialized(Int) +A](a :Array[A]) {
//	private var idx = 0
//	private val head = a(idx)
//}

object Playground extends App {
	LocalLazy(() => 1 + 1)
	EvalOpt(() => Yes(42))


	println(implicitly[NullValue[Unit]])

	val array = Array.ofDim[Int](4, 4)
//	val iter  = new Array2Iterator(array).init(0, (3 << 14) + 3)
//	iter.next()
//	val  prefix = iter.take(10)
//	val seq = iter.toSeq
}

