import scala.collection.immutable.{ArraySeq, TreeSet}
import scala.collection.mutable.ArrayBuffer

import net.noresttherein.sugar.collections.{ArraySliceBuffer, StringSet}
import net.noresttherein.sugar.collections.extensions.IteratorExtension



object Playground extends App {
	val strings = StringSet.fromSpecific(List("aa", "b"))
//	val i = strings.reverseIterator
	val keys = strings.take(3)
	println(keys)
	println(keys.size)
}

