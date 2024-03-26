
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.unspecialized
import scala.collection.{IterableFactory, IterableFactoryDefaults, IterableOps, View}
import scala.collection.immutable.{AbstractSeq, ArraySeq, HashSet, SortedSet, TreeSet}
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

import net.noresttherein.sugar.arrays.{ArrayCompanionExtension, IArray, IRefArray, RefArray}
import net.noresttherein.sugar.collections.{ArraySliceBuffer, Cat, Cuboid2Iterator, Fingers, IndexedSet, OrderedItems, OrderedSet, Ranking, RelayArray, SList, StrictView, StringSet, TestFingers, UnorderedItems, ValIterator}
import net.noresttherein.sugar.extensions.{ClassExtension, IterableExtension, IterableOnceExtension, IteratorExtension, SeqExtension, classNameMethods}
import net.noresttherein.sugar.slang.SerializationProxy



private object Playground extends App {
	val list = Ranking(1460337669, -1, 282133925, -2147483648)
	val arg = new OrderedSet(List(1378310141, 259512036, 586236955))
	println(list.size)
	println(arg.size + "/" + arg.toSet.size + " unique")
//	println(arg.toSet[Any].intersect(list.toSet).toString +  common)

	var i = 0
	val ll = { i += 1; i } #:: { i += 1; i } #:: { i += 1; i } #:: LazyList.empty
	val removed = ll.removed(1)
	val slice = RefArray.Slice(RefArray(0, 1, 2, 3), 1, 3)
	val zipped = slice.zip(slice.tail)
	println(slice.tail)
	println(zipped)
	val res = list.replaceAll(0, arg)
//	println(res == list)
	println(res)
	println(res.size)
}
