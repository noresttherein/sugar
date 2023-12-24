import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

import net.noresttherein.sugar.collections.ArraySliceBuffer
import net.noresttherein.sugar.collections.extensions.IteratorExtension



object Playground extends App {
	//ArraySliceBuffer|419-1522/63438848|
	val it = Iterator.iterate(1)(_ + 1)
//	val b = new ArraySliceBuffer[Int](63438848)
	val b = ArraySliceBuffer.from(it.take(2) to Vector)
//	b ++= it.take(1522)
//	b.remove(0, 419)
	b.insertAll(1, it.take(8) to List)
//	b.prependAll(it.take(59) to List)
//	val b = new ArraySliceBuffer[Int](72)
//	b ++= it.take(58)
//	b.remove(2, 8)
//	b.prependAll(it.take(20) to List)
//	b.prependAll(it.take(22) to List)
//	b.prepend(it.next())
//	b.patchInPlace(8, it.take(9) to ArraySeq, 5)
//	b.addOne(it.next())
//	b.addOne(it.next())
//	b.insert(6, it.next())
//	b.prepend(it.next())
//	b.addAll(it.take(7) to ArraySeq)
//	b.addOne(it.next())
//	b.prepend(it.next())
//	b.insertAll(9, it.take(11) to Vector)
//	b.insertAll(38, it.take(17) to ArraySeq)
	println(b)
}

