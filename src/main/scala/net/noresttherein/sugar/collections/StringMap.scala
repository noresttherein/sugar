package net.noresttherein.sugar.collections

import java.lang.System.arraycopy

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{AbstractIterator, Factory, SpecificIterableFactory, immutable}
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.{AbstractMap, AbstractSet, MapOps, SortedSet, SortedSetOps}
import scala.collection.mutable.{Builder, ReusableBuilder}

import net.noresttherein.sugar.outOfBounds_!
import net.noresttherein.sugar.JavaTypes.JStringBuilder
import net.noresttherein.sugar.arrays.extensions.ArrayCompanionExtension
import net.noresttherein.sugar.collections.ElementIndex.Absent
import net.noresttherein.sugar.collections.PrefixTree.{EmptyChildrenArray, compareRange}
import net.noresttherein.sugar.collections.extensions.IterableOnceExtension
import net.noresttherein.sugar.numeric.extensions.BooleanExtension
import net.noresttherein.sugar.typist.casting.extensions.{castingMethods, castTypeParamMethods}
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/** Interface of map implementations storing keys in a specific order.
  * The difference from [[scala.collection.immutable.SortedMap SortedMap]]
  * is that the ordering, or even the key type, may be specific to the implementation, rather than arbitrary.
  * `SortedMap`'s interface doesn't make provisions for returning a type such as
  * [[net.noresttherein.sugar.collections.StringMap StringMap]] from most of its methods.
  */
trait SpecificSortedMapOps[K, +V, +C <: MapOps[K, V, Map, C]] extends MapOps[K, V, Map, C] {
	def ordering :Ordering[K]

	def iteratorFrom(key :K) :Iterator[(K, V)]
	def keysIteratorFrom(key :K) :Iterator[K]

	def range(from :K, until :K) :C = rangeImpl(Some(from), Some(until))
	def rangeFrom(from :K) :C = rangeImpl(Some(from), None)
	def rangeUntil(until :K) :C = rangeImpl(None, Some(until))
	def rangeImpl(from :Option[K], until :Option[K]) :C
}




/** A prefix tree mapping strings into values of `V`.
  * Entries are stored in the alphabetical order, as specified by default `Char` ordering.
  * It isn't a `SortedMap` because the latter's `+` and other operations are final
  * and do not allow for returning a map with a fixed key type
  * @define Coll `StringMap`
  * @define coll string map
  */
@SerialVersionUID(Ver)
final class StringMap[+V] private (root :PrefixTree[V])
	extends AbstractMap[String, V] with SugaredIterable[(String, V)]
	   with SpecificSortedMapOps[String, V, StringMap[V]] with MapOps[String, V, Map, StringMap[V]]
       with SugaredIterableOps[(String, V), immutable.Iterable, StringMap[V]]
	   with SlicingOps[(String, V), StringMap[V]] with DefaultSerializable
{
	override def size :Int = root.size
	override def knownSize :Int = root.size
	def ordering :Ordering[String] = Ordering.String

	@inline private def wrap[V1 >: V](tree :PrefixTree[V1]) :StringMap[V1] =
		if (tree eq root) this
		else if (tree.size == 0) StringMap.empty
		else new StringMap(tree)

	override def head :(String, V) =
		if (root.size == 0) throw new NoSuchElementException("StringMap().head") else root.at(0).entry.get

	override def last :(String, V) =
		if (root.size == 0) throw new NoSuchElementException("StringMap().last") else root.at(root.size - 1).entry.get

	/** The `n`-th key in this map. */
	@throws[IndexOutOfBoundsException]("if n < 0 or n >= size")
	def key(n :Int) :String = root.at(n).firstKey

	/** The `n`-th entry in this map. */
	@throws[IndexOutOfBoundsException]("if n < 0 or n >= size")
	def entry(n :Int) :(String, V) = root.at(n).entry.get

	/** The `n`-th value in this map, in key order. */
	@throws[IndexOutOfBoundsException]("if n < 0 or n >= size")
	def value(n :Int) :V = root.at(n).value.get

	override def apply(key :String) :V = root.get(key) match {
		case Got(value) => value
		case _          => default(key)
	}
	override def get(key :String) :Option[V] = root.get(key).toOption

	def indexOf(key :String) :ElementIndex = root.indexOf(key)

	override def getOrElse[V1 >: V](key :String, default: => V1) :V1 = root.get(key) match {
		case Got(value) => value
		case _          => default
	}

	override def removed(key :String) :StringMap[V] = wrap(root.remove(key))

	override def updated[V1 >: V](key :String, value :V1) :StringMap[V1] = wrap(root.inserted(key, value))

	override def updatedWith[V1 >: V](key :String)(remappingFunction :Option[V] => Option[V1]) :StringMap[V1] =
		if (root.size == 0)
			this
		else {
			val previousValue = this.get(key)
			remappingFunction(previousValue) match {
				case None if previousValue.isDefined => removed(key)
				case None                            => this
				case Some(nextValue) =>
					if (previousValue.exists(_.asInstanceOf[AnyRef] eq nextValue.asInstanceOf[AnyRef])) this
					else updated(key, nextValue)
			}
		}

	override def +[V1 >: V](kv :(String, V1)) :StringMap[V1] = updated(kv._1, kv._2)
	override def ++[V1 >: V](elems :IterableOnce[(String, V1)]) :StringMap[V1] = concat(elems)

	override def concat[V1 >: V](elems :IterableOnce[(String, V1)]) :StringMap[V1] =
		elems.toBasicOps.foldLeft(this :StringMap[V1])(_ + _)


	override def foreach[U](f :((String, V)) => U) :Unit = root.foreach(f)
	override def foreachEntry[U](f :(String, V) => U) :Unit = root.foreachEntry(f)
	override def foldLeft[A](z :A)(op :(A, (String, V)) => A) :A = root.foldLeft(z)(op)
	override def foldRight[A](z :A)(op :((String, V), A) => A) :A = root.foldRight(z)(op)

	override def transform[W](f: (String, V) => W): StringMap[W] = map { case (k, v) => (k, f(k, v)) }

	def map[W](f :((String, V)) => (String, W)) :StringMap[W] = StringMap.from(iterator.map(f))

	def flatMap[W](f :((String, V)) => IterableOnce[(String, W)]) :StringMap[W] = StringMap.from(iterator.flatMap(f))


	override def keySet :StringSet = if (root.size == 0) StringSet.empty else new StringSet(root)

	override def rangeImpl(from :Option[String], until :Option[String]) :StringMap[V] = {
		val lo = from match {
			case Some(key) => root.indexOf(key).index
			case _         => 0
		}
		val hi = until match {
			case Some(key) => root.indexOf(key).index
			case _         => root.size
		}
		slice(lo, hi)
	}

	protected override def trustedSlice(from :Int, until :Int) :StringMap[V] = new StringMap(root.slice(from, until))


	override def iteratorFrom(start :String) :Iterator[(String, V)] =
		if (root.size == 0) Iterator.empty else new PrefixTreeEntryIterator(root).rangeFrom(start)

	override def keysIteratorFrom(start :String) :Iterator[String] =
		if (root.size == 0) Iterator.empty else new PrefixTreeKeyIterator(root).rangeFrom(start)

	override def iterator :Iterator[(String, V)] =
		if (root.size == 0) Iterator.empty else new PrefixTreeEntryIterator(root)

	override def keysIterator :Iterator[String] =
		if (root.size == 0) Iterator.empty else new PrefixTreeKeyIterator(root)

	override def valuesIterator :Iterator[V] =
		if (root.size == 0) Iterator.empty else new PrefixTreeValueIterator(root)

	def reverseIterator :Iterator[(String, V)] =
		if (root.size == 0) Iterator.empty else new PrefixTreeEntryReverseIterator(root)

	def reverseKeysIterator :Iterator[String] =
		if (root.size == 0) Iterator.empty else new PrefixTreeKeyReverseIterator(root)

	def reverseValuesIterator :Iterator[V] =
		if (root.size == 0) Iterator.empty else new PrefixTreeValueIterator(root)

	override def empty :StringMap[V] = StringMap.empty

	protected override def fromSpecific(coll :IterableOnce[(String, V @uncheckedVariance)]) :StringMap[V] =
		StringMap.from(coll)

	protected override def newSpecificBuilder :Builder[(String, V) @uncheckedVariance, StringMap[V]] =
		StringMap.newBuilder


	protected override def className :String = "StringMap"
}




/**
  * $factoryInfo
  * @define Coll `ChoppedString`
  * @define coll chopped string
  */
@SerialVersionUID(Ver)
case object StringMap {
	def apply[V](items :(String, V)*) :StringMap[V] = from(items)

	def from[V](items :IterableOnce[(String, V)]) :StringMap[V] = items match {
		case map   :StringMap[V @unchecked] => map
		case empty :Iterable[_] if empty.isEmpty => this.empty
		case empty :Iterator[_] if empty.isEmpty => this.empty
		case _ =>
			val root = ((PrefixTree.Empty :PrefixTree[V]) /: items.toIterableOnceOps) {
				case (tree, (key, value)) => tree.inserted(key, value)
			}
			if (root.size == 0) Empty else new StringMap(root)
	}

	def empty[V] :StringMap[V] = Empty

	private[this] val Empty = new StringMap(PrefixTree.Empty)

	def newBuilder[V] :Builder[(String, V), StringMap[V]] = new StringMapBuilder[V]

	private class StringMapBuilder[V] extends ReusableBuilder[(String, V), StringMap[V]] {
		private[this] var tree = PrefixTree.Empty :PrefixTree[V]
		override def knownSize: Int = tree.knownSize
		override def clear() :Unit = { tree = PrefixTree.Empty }
		override def result() :StringMap[V] =
			if (tree.size == 0)
				Empty
			else {
				val root = tree
				tree = PrefixTree.Empty
				new StringMap(root)
			}
		override def addOne(elem :(String, V)) :this.type = { tree = tree.inserted(elem._1, elem._2); this }
	}
}





/** A set of strings in the alphabetical order (derived from natural `Char` comparison).
  * @define Coll `StringSet`
  * @define coll string set
  */
final class StringSet(root :PrefixTree[_])
	extends AbstractSet[String] with SortedSet[String] with SortedSetOps[String, SortedSet, StringSet]
	   with SugaredIterable[String] with SugaredIterableOps[String, Set, StringSet]
	   with SpecificIterableFactoryDefaults[String, Set, StringSet]
	   with SlicingOps[String, StringSet] with DefaultSerializable
{
	override def ordering :Ordering[String] = Ordering.String
	override def size :Int = root.size
	override def knownSize :Int = root.size

	@inline private def wrap(tree :PrefixTree[_]) :StringSet =
		if (tree eq root) this
		else if (tree.size == 0) StringSet.empty
		else new StringSet(tree)

	override def head :String =
		if (root.size == 0) throw new NoSuchElementException("StringSet().head") else root.at(0).firstKey

	override def last :String =
		if (root.size == 0) throw new NoSuchElementException("StringSet().last") else root.at(root.size - 1).firstKey

	//todo: think of a universal name for OrderedSet
	def key(i :Int) :String = root.at(i).firstKey

	def indexOf(key :String) :ElementIndex = root.indexOf(key)

	override def contains(elem :String) :Boolean = root.get(elem).isDefined

	override def incl(elem :String) :StringSet = wrap(root.inserted(elem, elem))

	override def excl(elem :String) :StringSet = wrap(root.remove(elem))


	override def iterator :Iterator[String] =
		if (root.size == 0) Iterator.empty else new PrefixTreeKeyIterator(root)

	override def iteratorFrom(start :String) :Iterator[String] =
		if (root.size == 0) Iterator.empty else new PrefixTreeKeyIterator(root).rangeFrom(start)

	def reverseIterator :Iterator[String] =
		if (root.size == 0) Iterator.empty else new PrefixTreeKeyReverseIterator(root)

	override def foreach[U](f :String => U) :Unit = root.foreachKey(f)
	override def foldLeft[A](z :A)(op :(A, String) => A) :A = root.foldLeftKeys(z)(op)
	override def foldRight[A](z :A)(op :(String, A) => A) :A = root.foldRightKeys(z)(op)

	def map(f :String => String) :StringSet = StringSet.fromSpecific(iterator.map(f))

	def flatMap(f :String => IterableOnce[String]) :StringSet = StringSet.fromSpecific(iterator.flatMap(f))

	override def rangeImpl(from :Option[String], until :Option[String]) :StringSet = {
		val lo = from match {
			case Some(key) => root.indexOf(key).index
			case _         => 0
		}
		val hi = until match {
			case Some(key) => root.indexOf(key).index
			case _         => root.size
		}
		slice(lo, hi)
	}

	protected override def trustedSlice(from :Int, until :Int) :StringSet = new StringSet(root.slice(from, until))

	override def specificFactory :SpecificIterableFactory[String, StringSet] = StringSet

	protected[this] override def className = "StringSet"
}




/**
  * $factoryInfo
  * @define Coll `StringSet`
  * @define coll string set
  */
@SerialVersionUID(Ver)
case object StringSet extends SpecificIterableFactory[String, StringSet] {
	override val empty :StringSet = new StringSet(PrefixTree.Empty)

	override def fromSpecific(it :IterableOnce[String]) :StringSet = it match {
		case strings :StringSet => strings
		case empty   :Iterable[_] if empty.isEmpty => StringSet.empty
		case empty   :Iterator[_] if !empty.hasNext => StringSet.empty
		case _ =>
			val root = ((PrefixTree.Empty :PrefixTree[String]) /: it.toIterableOnceOps) {
				case (tree, key) => tree.inserted(key, key)
			}
			if (root.size == 0) empty else new StringSet(root)
	}

	override def newBuilder :Builder[String, StringSet] = new StringSetBuilder

	private class StringSetBuilder extends ReusableBuilder[String, StringSet] {
		private[this] var tree :PrefixTree[String] = PrefixTree.Empty
		override def knownSize = tree.knownSize
		override def clear() :Unit = tree = PrefixTree.Empty
		override def result() :StringSet =
			if (tree.size == 0)
				empty
			else {
				val res = new StringSet(tree)
				 tree = PrefixTree.Empty
				res
			}
		override def addOne(elem :String) :this.type = { tree = tree.inserted(elem, elem); this }
	}
}






private object PrefixTree {
	@inline def apply(key :String) :PrefixTree[String] =
		new PrefixTree(key.length, key, Got(key), EmptyChildrenArray.castParam[PrefixTree[String]], 1, 1)

	@inline def apply[V](key :String, value :V) :PrefixTree[V] =
		new PrefixTree(key.length, key, Got(value), EmptyChildrenArray.castParam[PrefixTree[V]], 1, 1)

//	@inline def apply[V](entry :(String, V)) :PrefixTree[V] =
//		new PrefixTree(entry._1.length, entry._1, Got(entry._2), EmptyChildrenArray.castParam[PrefixTree[V]], 1, 1)

	@inline def apply[V](key :String, value :V, child :PrefixTree[V]) :PrefixTree[V] =
		new PrefixTree(key.length, key, Got(value), Array.one(child), child.size + 1, child.depth + 1)

	/** Joins to valid prefix trees which diverge at `offset` in a single node for the divergence point. */
	@inline def apply[V](offset :Int, left :PrefixTree[V], right :PrefixTree[V]) :PrefixTree[V] = {
		val key = left.firstKey
		val depth = math.max(left.depth, right.depth) + 1
		new PrefixTree(offset, key, Lack, Array.two(left, right), left.size + right.size, depth)
	}
//	@inline def apply[V](offset :Int, key :String, left :PrefixTree[V], right :PrefixTree[V]) :PrefixTree[V] = {
//	}

	/** Joins to legal prefix trees into one tree.
	  * Requires `first.firstKey.take(first.offset) < second.firstKey.take(second.offset)`.
	  */
	def apply[V](first :PrefixTree[V], second :PrefixTree[V]) :PrefixTree[V] = {
		val offset1 = first.offset
		val offset2 = second.offset
		val key1    = first.firstKey
		val key2    = second.firstKey
		val limit   = math.min(offset1, offset2)
		val i       = mismatch(key1, key2, 0, limit)
		if (offset1 == offset2)
			PrefixTree(i, first, second)
		else if (i == offset1)  //offset < offset2
			new PrefixTree(
				offset1, key1, first.value, first.children :+ second,
				first.size + second.size, math.max(first.depth, second.depth + 1)
			)
		else if (i == offset2) //offset > offset2
			new PrefixTree(
				offset2, key2, second.value, first +: second.children,
				first.size + second.size, math.max(second.depth, first.depth + 1)
			)
		else
			PrefixTree(i, first, second)
	}

	val EmptyChildrenArray = new Array[PrefixTree[Nothing]](0)

	val Empty = new PrefixTree[Nothing](0, "", Lack, EmptyChildrenArray, 0)


	private final val StackRecursionDepthLimit = 512

	def compareRange(s1 :String, s2 :String, from :Int, until :Int) :Int = {
		var i   = from
		var cmp = 0
		while (i < until && cmp == 0) {
			val c1 = s1.charAt(i)
			val c2 = s2.charAt(i)
			cmp = Character.compare(c1, c2)
			i += 1
		}
//		cmp
		if (cmp == 0) 0
		else if (cmp < 0) -i
		else i
	}

	def mismatch(s1 :String, s2 :String, from :Int, until :Int) :Int = {
		var i   = from
		var cmp = 0
		while (i < until && cmp == 0) {
			val c1 = s1.charAt(i)
			val c2 = s2.charAt(i)
			cmp = Character.compare(c1, c2)
			i += 1
		}
		if (cmp != 0) i - 1 else i
	}

	abstract class PrefixTreeBuilder[V, E, +C] extends ReusableBuilder[E, C] {
		private[this] var tree = PrefixTree.Empty :PrefixTree[V]
		override def knownSize: Int = tree.knownSize
		def clear() :Unit = { tree = PrefixTree.Empty }
		def result() :C = {
			val root = tree
			tree = PrefixTree.Empty
			wrap(root)
		}
		protected def wrap(tree :PrefixTree[V]) :C
	}

}


private final class PrefixTree[+V](val offset :Int, val firstKey :String, val value :Opt[V],
                                   val children :Array[PrefixTree[V @uncheckedVariance]], val size :Int, val depth :Int)
	extends IterableOnce[V]
{
	def this(offset :Int, key :String, value :Opt[V], children :Array[PrefixTree[V]], size :Int) =
		this(offset, key, value, children, size, {
			var depth = 1
			var i = children.length - 1
			while (i >= 0) {
				depth = math.max(children(i).depth, depth)
				i -= 1
			}
			depth
		})
	def this(offset :Int, key :String, value :Opt[V], children :Array[PrefixTree[V]]) =
		this(offset, key, value, children, {
			var size = if (value.isDefined) 1 else 0
			var i = children.length
			while (i > 0) {
				i -= 1
				size += children(i).size
			}
			size
		})

	def isDeep = depth > PrefixTree.StackRecursionDepthLimit
	override def knownSize :Int = size

	def entry :Opt[(String, V)] = value.map((firstKey, _))

	/** Compares key <> this.key.substring(from, offset). Requires key.length >= offset. */
	def compareKeyFrom(key :String, from :Int) :Int = {
		val thisKey = this.firstKey
		var i   = from
		var cmp = 0
		while (i < offset & cmp == 0) {
			val c1 = thisKey(i)
			val c2 = key(i)
			cmp    = Character.compare(c2, c1)
			i += 1
		}
		cmp
	}


	def at(i :Int) :PrefixTree[V] = {
		@tailrec def findByIndex(node :PrefixTree[V], i :Int) :PrefixTree[V] =
			if (i < 0 || i >= node.size)
				null
			else if (i == 0)
				if (node.value.isDefined) node
				else findByIndex(node.children(0), i)
			else {
				val children = node.children
				var count    = node.value.isDefined.toInt
				var childIdx = 0
				var child    = children(childIdx)
				while (count + child.size <= i && childIdx < children.length - 1) {
					count    += child.size
					childIdx += 1
					child     = children(childIdx)
				}
				findByIndex(child, i - count)
			}
		findByIndex(this, i) match {
			case null => outOfBounds_!(i, size)
			case node => node
		}
	}

	def get(key :String) :Opt[V] = {
		val length = key.length
		var node   = this
		var cmpTo  = 0
		while (node != null) {
			val offset = node.offset
			if (offset > length)
				return Lack
			val cmp = compareRange(key, node.firstKey, cmpTo, offset)
			if (cmp < 0)
				return Lack
			if (offset == length)
				return if (cmp == 0) node.value else Lack
			cmpTo = offset
			node  = node.child(key.charAt(cmpTo))
		}
		Lack
	}

	def indexOf(key :String) :ElementIndex = {
		val length = key.length
		var node   = this
		var cmpTo  = 0
		var prefix = 0
		while ({
			val offset = node.offset
			var cmp = compareRange(key, node.firstKey, cmpTo, math.min(offset, length))
			if (cmp == 0)
				cmp = length - offset
			if (cmp < 0)
				return Absent(prefix)
			if (cmp == 0)
				return ElementIndex(node.value.isDefined, prefix)
			if (node.value.isDefined)
				prefix += 1
			if (length <= offset)
				return Absent(prefix)
			//below this point cmp >= 0 && offset < length
			cmpTo = offset
			val children   = node.children
			val childrenNo = children.length
			val char       = key.charAt(cmpTo)
			node           = null
			var skip       = 0
			var i = 0
			while (i < childrenNo && {
				val next = children(i)
				skip = next.size
				i  += 1
				cmp = Character.compare(char, next.firstKey.charAt(cmpTo))
				if (cmp >= 0)
					node = next
				cmp > 0
			})
				prefix += skip
			node != null && cmp == 0
		}) {}
		Absent(prefix)
	}

	override def iterator :Iterator[V] = if (size == 0) Iterator.empty else new PrefixTreeValueIterator[V](this)


	def childIdx(char :Char) :Int =
		if (children.length == 0)
			-1
		else {
			var lo     = 0
			var hi     = children.length
			val offset = this.offset
			while (lo < hi) {
				val middle = (lo + hi) >>> 1
				val child = children(middle)
				if (char > child.firstKey.charAt(offset)) lo = middle + 1
				else hi = middle
			}
			if (lo < children.length && children(lo).firstKey.charAt(offset) == char) lo
			else -lo - 1
		}

	/** Returns the child of this node whose key equals `next` at index `this.depth`,
	  * or `null` if no such node exists.
	  */
	def child(next :Char) :PrefixTree[V] = {
		val idx = childIdx(next)
		if (idx < 0) null
		else children(idx)
	}

	def firstChild :PrefixTree[V] = if (children.length == 0) null else children(0)
	def lastChild  :PrefixTree[V] = if (children.length == 0) null else children(children.length - 1)

	//Drops also the key in this node, if present
	def dropFirstChild :PrefixTree[V] = {
		val childrenNo = children.length
		if (childrenNo <= 1)
			null
		else if (childrenNo == 2)
			children(1)
		else {
			val first    = children(0)
			val firstKey = children(1).firstKey
			if (first.depth < depth - 1)
				new PrefixTree(offset, firstKey, Lack, children.tail, size - first.size, depth)
			else
				new PrefixTree(offset, firstKey, Lack, children.tail, size - first.size)
		}
	}

	def dropLastChild :PrefixTree[V] = children.length match {
		case 0 => null
		case 1 => value match {
			case Got(v) => PrefixTree(firstKey, v)
			case _      => null
		}
		case n =>
			val last = children(n - 1)
			if (last.depth < depth - 1)
				new PrefixTree(offset, firstKey, value, children.init, size - last.size, depth)
			else
				new PrefixTree(offset, firstKey, value, children.init, size - last.size)
	}

	/** Adds a new node to this node's children. Requires `key >= this.key`. */
	def add[V1 >: V](key :String, value :V1) :PrefixTree[V1] = {
		val i      = -childIdx(key.charAt(offset)) - 1
		val length = children.length
		val array  = new Array[PrefixTree[V1]](length + 1)
		arraycopy(children, 0, array, 0, i)
		arraycopy(children, i, array, i + 1, length - i)
		array(i) = PrefixTree(key, value)
		val firstKey = if (i == 0 && this.value.isEmpty) key else this.firstKey
		new PrefixTree[V1](offset, firstKey, this.value, array, size + 1, depth)
	}

	/** Substitutes the current child whose key at `this.depth` equals `char` with the given node.
	  * Assumes a child starting with `char` already exists.
	  */
	def updated[V1 >: V](char :Char, child :PrefixTree[V1]) :PrefixTree[V1] = {
		val i = childIdx(char)
		val length = children.length
		val array = Array.copyOf(children, length).castParam[PrefixTree[V1]]
		val old = array(i)
		array(i) = child
		//Free the key for garbage collection in case it belonged to a removed node
		val key = if (i == 0 && value.isEmpty) child.firstKey else firstKey
		if (child.depth >= old.depth)
			new PrefixTree(offset, key, value, array, size + child.size - old.size, math.max(depth, child.depth + 1))
		else
			new PrefixTree(offset, key, value, array, size + child.size - old.size)
	}

//	/** Updates/sets the value on this node. */
//	def updated[V1 >: V](value :V1) :PrefixTree[V1] = updated(key, value)

	/** Updates/sets the value on this node. The key must equal `this.firstKey.substring(0, this.offset)`. */
	def updated[V1 >: V](key :String, value :V1) :PrefixTree[V1] =
		if (this.value.isEmpty)
			new PrefixTree(offset, key, Got(value.asInstanceOf[V]), children, size + 1, depth)
		else {
			val x = this.value.get
			val equal = value match {
				case v :AnyRef  => v eq x.asInstanceOf[AnyRef]
				case v :Int     => v == x
				case v :Long    => v == x
				case v :Double  => v == x
				case v :Char    => v == x
				case v :Float   => v == x
				case v :Byte    => v == x
				case v :Short   => v == x
				case v :Boolean => v == x
			}
			if (equal) this
			else new PrefixTree(offset, key, Got(value.asInstanceOf[V]), children, size, depth)
		}

	@inline def -(char :Char) :PrefixTree[V] = remove(char)
	def remove(char :Char) :PrefixTree[V] = {
		val length = children.length
		if (length == 1) {
			val noChildren = EmptyChildrenArray.castFrom[Array[PrefixTree[Nothing]], Array[PrefixTree[V]]]
			new PrefixTree(offset, firstKey, value, noChildren, value.size, 1)
		} else {
			val i = childIdx(char)
			val old = children(i)
			val array = new Array[PrefixTree[V]](length -1)
			arraycopy(children, 0, array, 0, i)
			arraycopy(children, i + 1, array, i, length - i - 1)
			val key = if (i == 0 && value.isEmpty && array.length > 0) array(0).firstKey else firstKey
			if (old.depth < depth - 1)
				new PrefixTree(offset, key, value, array, size - old.size, depth)
			else
				new PrefixTree(offset, key, value, array, size - old.size)
		}
	}

	def removeKey :PrefixTree[V] =
		if (value.isEmpty) this
		else if (children.length == 1) children(0)
		else if (children.length == 0) PrefixTree.Empty
		else new PrefixTree(offset, children(0).firstKey, Lack, children, size - 1, depth)

	@inline def -(key :String) :PrefixTree[V] = remove(key)

	def remove(key :String) :PrefixTree[V] = {
		val length = key.length
		//invariant: depth <= key.length and node.key.substring(0, depth) == key.substring(0, depth) and depth <= node.depth
		def stackRemove(node :PrefixTree[V], offset :Int) :PrefixTree[V] = {
			val nodeKey   = node.firstKey
			val nodeDepth = node.offset
			var i         = offset
			val end       = math.min(length, nodeDepth)
			val keyAti    = key.charAt(i)
			while (i < end) {
				if (keyAti != nodeKey.charAt(i))               //key not present in the tree
					return node
				i += 1
			}
			if (i == length)
				if (i == nodeDepth) node.removeKey              //key corresponds exactly to node
				else node                                       //all keys starting with key.take(i) are longer than key
			else { //i == nodeDepth
				val char  = key.charAt(nodeDepth)
				val child = node.child(char)
				if (child == null)
					node
				else {
					val substitute = stackRemove(child, nodeDepth)
					if (substitute eq child) node
					else if (substitute.size == 0) node - char
					else node.updated(char, substitute)
				}
			}
		}
		if (!isDeep)
			stackRemove(this, 0)
		else if (key.length == 0)
			if (offset == 0) removeKey else this
		else {
			val length = key.length
			var stack  = new Array[PrefixTree[V]](32)
			stack(0)   = this
			var top    = 0
			var start  = 0
			var node   = this
			while (start < length) {
				val offset = node.offset
				if (offset > length)
					return this
				val cmp = compareRange(node.firstKey, key, start, offset)
				if (cmp != 0)
					return this
				if (offset == length)
					if (node.value.isEmpty)
						return this
					else if (node.size == 1)
						node = null
					else //loop termination
						node = new PrefixTree(
							node.offset, node.children(0).firstKey, Lack, node.children, node.size - 1, node.depth
						)
				else {
					node = node.child(key.charAt(offset))
					if (node == null)
						return this
					top += 1
					if (top == stack.length)
						stack = Array.copyOf(stack, stack.length << 1)
					stack(top) = node
				}
				start = offset
			}
			//If we are here, we have found the key in node. If node==null, it must be removed from its parent.
			// Otherwise, substitute node for its old version in its parent on the stack
			while (top >= 0) {
				val parent = stack(top)
				top -= 1
				if (node == null)
					node = parent.remove(key.charAt(parent.offset))
				else
					node = node.updated(key.charAt(parent.offset), node)
			}
			node
		}
	}

	def inserted[V1 >: V](key :String, value :V1) :PrefixTree[V1] = {
		val length = key.length
		//invariant: offset <= key.length and node.key.substring(0, offset) == key.substring(0, offset) and offset <= node.offset
		def update(node :PrefixTree[V], offset :Int) :PrefixTree[V1] = {
			val nodeKey   = node.firstKey
			val nodeDepth = node.offset
			var i         = offset
			val end       = math.min(length, nodeDepth)
			while (i < end) {
				val keyChar  = key.charAt(i)
				val nodeChar = nodeKey.charAt(i)
				if (keyChar != nodeChar)
					if (keyChar < nodeChar)
						return PrefixTree(i, PrefixTree(key, value), node)
					else
						return PrefixTree(i, node, PrefixTree(key, value))
				i += 1
			}
			if (i == length)
				if (i == nodeDepth) node.updated(key, value)  //length == nodeDepth
				else PrefixTree(key, value, node)             //length < nodeDepth
			else {                                            //length > nodeDepth
				val keyChar = key.charAt(nodeDepth)
				val child   = node.child(keyChar)
				if (child == null)
					if (node.size == 0) PrefixTree(key, value)
					else node.add(key, value)
				else {
					val substitute = update(child, nodeDepth)
					if (substitute eq child) node
					else node.updated(keyChar, substitute)
				}
			}
		}
		if (!isDeep)
			update(this, 0)
		else if (size == 0)
			PrefixTree(key, value)
		else if (key.length == 0)
			if (offset == 0) updated(key, value) else PrefixTree("", value, this)
		else {
			val length   = key.length
			var stack    = new Array[PrefixTree[V1]](32)
			stack(0)     = this
			var top      = 0
			var start    = 0
			var node     :PrefixTree[V1] = this
			var CONTINUE = true
			//For each 0 <= i < top: stack(i) contains a child which equals stack(i+1).firstKey
			// up to index stack(i).offset + 1 (including). That is, we always descend into an existing child.
			while (CONTINUE) {
				val offset = node.offset
				CONTINUE   = false
				val end = math.min(length, offset)
				val cmp = compareRange(node.firstKey, key, start, end)
				if (cmp < 0)
					node = PrefixTree(-cmp - 1, node, PrefixTree(key, value))
				else if (cmp > 0)
					node = PrefixTree(cmp - 1, PrefixTree(key, value), node)
				else if (offset == length)
					node = node.updated(key, value)
				else if (offset > length)
					node = PrefixTree(key, value, node)
				else {
					val i = node.childIdx(key.charAt(offset))
					if (i >= 0) { //Descend into the child starting with the same letter as key.
						CONTINUE = true
						node     = node.children(i)
						top     += 1
						if (top == stack.length)
							stack = Array.copyOf(stack, stack.length << 1)
					} else { //No child equal to key at position offset. Update node with a new child and end the loop.
						val idx       = -i - 1
						val firstKey  = if (idx == 0 && node.value.isEmpty) key else node.firstKey
						val depth     = math.max(node.depth, 2)
						val children  = node.children
						val inserted  = Array.copyOf(children, children.length + 1)
						arraycopy(children, idx, inserted, idx + 1, children.length - idx)
						inserted(idx) = PrefixTree(key, value)
						node          = new PrefixTree(offset, firstKey, node.value, inserted, node.size + 1, depth)
					}
				}
				stack(top) = node
				start      = offset
			}
			while (top > 0) {
				top -= 1
				val parent = stack(top)
				node = stack(top).updated(key.charAt(parent.offset), node) //guaranteed to exist by the previous loop.
			}
			node
		}
	}


	//todo: recursion with a heap stack
	def slice(from :Int, until :Int) :PrefixTree[V] =
		if (from <= 0 && until >= size)
			this
		else if (value.isDefined & until == 1)
			if (from <= 0) PrefixTree(firstKey, value.get)
			else PrefixTree.Empty
		else if (isDeep)
			new PrefixTreeKeyIterator(this).take(until).drop(from).toPrefixTree
		else {
			val length     = children.length
			var start      = 0
			var firstChild = children(0)
			var lastSize   = firstChild.size
			var prefixSize = lastSize
			if (value.isDefined)
				prefixSize += 1
			while (start + 1 < length && prefixSize <= from) {
				start += 1
				firstChild  = children(start)
				lastSize    = firstChild.size
				prefixSize += lastSize
			}
			val relativeStart = from - (prefixSize - lastSize)
			var lastChild     = firstChild
			var end           = start
			while (end + 1 < length && prefixSize < until) {
				end += 1
				lastChild   = children(end)
				lastSize    = lastChild.size
				prefixSize += lastSize
			}
			val relativeEnd = until - (prefixSize - lastSize)
			if (start == end && (from > 0 | value.isEmpty))
				lastChild.slice(relativeStart, relativeEnd)
			else {
				val slicedChildren = children.slice(start, end + 1)
				if (relativeStart > 0)
					slicedChildren(0) = firstChild.slice(relativeStart, firstChild.size)
				if (relativeEnd < lastSize)
					slicedChildren(end - start) = lastChild.slice(0, relativeEnd)
				if (from == 0 && value.isDefined)
					new PrefixTree(offset, firstKey, value, slicedChildren, until - from)
				else {
					val k = if (firstKey.length == offset) firstKey else slicedChildren(0).firstKey
					new PrefixTree(offset, k, Lack, slicedChildren, until - from)
				}
			}
		}


	def foreach[U](f :((String, V)) => U) :Unit =
		if (size > 0)
			if (isDeep)
				new PrefixTreeEntryIterator(this).foreach(f)
			else {
				if (value.nonEmpty)
					f((firstKey, value.get))
				val end = children.length
				var i   = 0
				while (i < end) {
					children(i).foreach(f)
					i += 1
				}
			}
	def foreachEntry[U](f :(String, V) => U) :Unit =
		if (size > 0)
			if (isDeep)
				new PrefixTreeEntryIterator(this).foreach { case (key, value) => f(key, value) }
			else {
				if (value.nonEmpty)
					f(firstKey, value.get)
				val end = children.length
				var i   = 0
				while (i < end) {
					children(i).foreachEntry(f)
					i += 1
				}
			}
	def foreachKey[U](f :String => U) :Unit =
		if (size > 0)
			if (isDeep)
				new PrefixTreeKeyIterator(this).foreach(f)
			else {
				if (value.nonEmpty)
					f(firstKey)
				val end = children.length
				var i   = 0
				while (i < end) {
					children(i).foreachKey(f)
					i += 1
				}
			}
	def foldLeft[A](z :A)(f :(A, (String, V)) => A) :A =
		if (size == 0)
			z
		else if (isDeep)
			new PrefixTreeEntryIterator(this).foldLeft(z)(f)
		else {
			var acc = if (value.nonEmpty) f(z, entry.get) else z
			val end = children.length
			var i   = 0
			while (i < end) {
				acc = children(i).foldLeft(acc)(f)
				i  += 1
			}
			acc
		}
	def foldLeftKeys[A](z :A)(f :(A, String) => A) :A =
		if (size == 0)
			z
		else if (isDeep)
			new PrefixTreeKeyIterator(this).foldLeft(z)(f)
		else {
			var acc = if (value.nonEmpty) f(z, firstKey) else z
			val end = children.length
			var i   = 0
			while (i < end) {
				acc = children(i).foldLeftKeys(acc)(f)
				i  += 1
			}
			acc
		}
	def foldRight[A](z :A)(f :((String, V), A) => A) :A =
		if (size == 0)
			z
		else if (isDeep)
			new PrefixTreeEntryReverseIterator(this).foldLeft(z) { (acc, entry) => f(entry, acc) }
		else {
			var acc = z
			var i   = children.length
			while (i > 0) {
				i -= 1
				acc = children(i).foldRight(acc)(f)
			}
			if (value.isEmpty) acc else f(entry.get, acc)
		}
	def foldRightKeys[A](z :A)(f :(String, A) => A) :A =
		if (size == 0)
			z
		else if (isDeep)
			new PrefixTreeKeyReverseIterator(this).foldLeft(z) { (acc, key) => f(key, acc) }
		else {
			var acc = z
			var i   = children.length
			while (i > 0) {
				i -= 1
				acc = children(i).foldRightKeys(acc)(f)
			}
			if (value.isEmpty) acc else f(firstKey, acc)
		}

	override def toString :String = toString(new JStringBuilder).toString

	def toString(res :JStringBuilder) :JStringBuilder = {
		res append "{"
		if (value.isDefined) {
			res append String.valueOf(value.get)
			if (children.length > 0)
				res append ":"
		}
		children.indices.foreach { idx =>
			if (idx > 0)
				res append ", "
			val child = children(idx)
			res append child.firstKey.substring(offset, child.offset)
			child.toString(res)
		}
		res append '}'
	}
}






private abstract class AbstractPrefixTreeIterator[V, +E](root :PrefixTree[V])
	extends AbstractIterator[E] with IteratorSlicing[E]
{
	//Invariant: top < 0 || stack(top).value.isDefined) & stack.take(top - 1).forall(t => t.value.isEmpty & t.size > 0)
	//Can't delegate to recursive PrefixTree methods, because it is used to avoid blowing the stack.
	private[this] var top                         = -1
	private[this] var stack :Array[PrefixTree[V]] = null
	if (root.size > 0) {
		top = 0
		stack = new Array[PrefixTree[V]](32)
		stack(0) = root
		advance()
	}
//
//	@inline private[this] def push(node :PrefixTree[V]) :Unit = {
//		top += 1
//		if (top == stack.length)
//			stack = Array.copyOf(stack, stack.length << 1)
//		stack(top) = node
//	}

	protected def value(leaf :PrefixTree[V]) :E

	override def hasNext :Boolean = top >= 0

	override def next() :E = {
		val res = stack(top)
		if (res.size == 1) {
			stack(top) = null
			top -= 1
		} else
			stack(top) = res.removeKey
		advance()
		value(res)
	}

	/** Descend recursively into the first child until one contains a value.
	  * Remove the first child from every node on the path except the last, and put it on the stack if it's not empty.
	  */
	private def advance() :Unit =
		if (top >= 0) {
			var node = stack(top)
			while (node.value.isEmpty) {
				val parent = node.dropFirstChild
				if (parent != null) {
					stack(top) = parent
					top += 1
				}
				node = node.firstChild
				if (top == stack.length)
					stack = Array.copyOf(stack, stack.length << 1)
				stack(top) = node
			}
		}

	override def knownSize :Int = {
		var i   = top
		var res = 0
		while (i >= 0) {
			val node = stack(i)
			res += node.size
			i -= 1
		}
		res
	}

//	override def slice(from :Int, until :Int) :this.type = take(until).drop(from)

	override def drop(n :Int) :this.type =
		if (n <= 0 | top < 0)
			this
		else {
			var remaining = n
			while (top >= 0 && remaining > 0) {
				val node       = stack(top)
				if (node.size <= remaining) {
					remaining -= node.size
					stack(top) = null
					top -= 1
				} else {
					var prefixSize = 0
					if (node.value.isDefined)
						prefixSize = 1
					val children   = node.children
					val childrenNo = children.length
					var size       = children(0).size
					var first = 0
					while (prefixSize + size <= remaining) {
						first += 1
						prefixSize += size
						size = children(first).size
					}
					remaining -= prefixSize
					if (first == childrenNo - 1)
						stack(top) = children(first)
					else if (remaining == 0)
						stack(top) = new PrefixTree(
							node.offset, children(first).firstKey, Lack, children.drop(first), node.size - prefixSize
						)
					else {
						if (first == childrenNo - 2)
							stack(top) = children(childrenNo - 1)
						else {
							val newSize = node.size - prefixSize - size
							stack(top) = new PrefixTree(
								node.offset, children(first + 1).firstKey, Lack, children.drop(first + 1), newSize
							)
						}
						top += 1
						if (top == stack.length)
							stack = Array.copyOf(stack, stack.length << 1)
						stack(top) = children(first)
					}
				}
			}
			this
		}

	override def take(n :Int) :this.type =
		if (n <= 0) {
			top   = -1
			stack = null
			this
		} else {
			val size = knownSize
			dropRight(size - n)
		}

	def dropRight(n :Int) :this.type =
		if (n <= 0 | top < 0)
			this
		else {
			var remaining = n
			var depth     = 0
			var node      = stack(0)
			var size      = node.size
			//First, determine which of the nodes on the bottom of the stack must be dropped completely.
			while (depth < top && remaining >= size) {
				remaining -= size
				depth += 1
				node   = stack(depth)
				size   = node.size
			}
			if (remaining >= size) {
				top = -1
				stack = null
			} else {
				var tailStack = if (depth == top) stack else new Array[PrefixTree[V]](32)
				tailStack(0) = node
				var i = 0
				//Recursively drop trailing children of `tailStack(i)` and descend to its child, if needed.
				while (remaining > 0) {
					//In each iteration, either remaining decreases or node.size decreases (or both).
					//None of these variables ever increases.
					if (remaining >= node.size) { //If node.size drops below the number of elements to drop, end the loop.
						remaining = 0
						tailStack(i) = null
						i -= 1
					} else {
						val children   = node.children
						val childrenNo = children.length
						var suffixSize = 0
						var j          = childrenNo
						while (suffixSize < remaining) { //note that children.sumBy(_.size) < remaining
							j -= 1
							suffixSize += children(j).size
						}
						if (suffixSize == remaining) { //suffixSize < node.size, so the created node will be non empty.
							remaining    = 0
							size         = node.size - suffixSize
							node         = new PrefixTree(node.offset, node.firstKey, node.value, children.take(j), size)
							tailStack(i) = node
						} else {
							val child = children(j)
							size      = child.size
							remaining -= suffixSize - size //size was added to suffixSize, so suffixSize - size >= 0
							if (j == 0 && node.value.isEmpty)
								tailStack(i) = child
							else {
								//We might create here an illegal node with a single child and no key,
								// but when we go back down tailStack, we'll add a non-empty child to it, making it right.
								val parentSize = node.size - suffixSize
								tailStack(i) = new PrefixTree(
									node.offset, node.firstKey, node.value, children.take(j), parentSize
								)
								i += 1
								if (i == tailStack.length)
									tailStack = Array.copyOf(tailStack, tailStack.length << 1)
								tailStack(i) = child
							}
							node = child
						}
					}
				}
				while (i > 0) {
					tailStack(i) = null
					i -= 1
					val parent = tailStack(i)
					node = new PrefixTree(
						parent.offset, parent.firstKey, parent.value, parent.children :+ node,
						parent.size + node.size, math.max(parent.depth, node.depth + 1)
					)
				}
				if (depth == top) {
					tailStack(0) = node
					stack = tailStack
					top   = 0
					advance()
				} else {
					stack(depth) = node
					arraycopy(stack, depth, stack, 0, top + 1 - depth)
					top -= depth
				}
			}
			this
		}

	def rangeFrom(key :String) :this.type =
		if (top < 0 || key.length == 0)
			this
		else {
			val length = key.length
			var start  = 0
			var depth  = -1
			var offset = 0
			var cmp    = 0
			var node :PrefixTree[V] = null
			//First we descend down the stack for as long as the key is equal to node.key up to node.offset.
			//If we encounter a node with only greater/equal keys, then the iterator is already in the right position.
			while (depth < top & cmp == 0 & offset < length) {
				depth  += 1
				start   = offset
				node    = stack(depth)
				offset  = node.offset
				val end = math.min(offset, length)
				cmp     = compareRange(key, node.firstKey, start, end)
			}
			if (cmp > 0) { //all keys in node are lesser than key.
				depth -= 1
				if (depth >= 0) { //retreat one step to the point where the node prefix matched the key
					node = stack(depth)
					cmp = 0
				} //otherwise we are already empty
			} else if (cmp == 0)
				start = offset
			/* 1. If cmp < 0, then the iterator already contains keys only greater than key. We are done.
			 * 2. If cmp > 0, then stack(depth) contains a key lesser than key, and we need to recursively drop children.
			 * 3. If cmp == 0, then the key equals up to offset the first key, and:
			 *      - either length <= offset, so the key is shorter, and thus lesser than the first key in the iterator
			 *      - and/or depth==top, and we might have to drop some nodes, as in 2).
			 */
			//Invariant: all nodes on the stack below depth have only keys greater than key.
			//Lets recursively drop from stack(depth) all children with any keys lesser than key,
			// but if one may contain a key greater/equal than key, then put it on top of the stack, increasing depth.
			while (start < length & cmp == 0) {
				val children   = node.children
				val childrenNo = children.length
				val char       = key.charAt(start)
				val nextIdx    = node.childIdx(char)
				if (nextIdx >= 0) {
					//All keys in children(nextIdx-1) < key and all keys in children(nextIdx+1) > key.
					node   = children(nextIdx)
					offset = node.offset
					cmp    = compareRange(key, node.firstKey, start, math.min(offset, length))
					start  = offset
					if (cmp > 0 | cmp == 0 & offset < length) { //firstKey in node is lesser or equal to key
						if (nextIdx < childrenNo - 1) {
							stack(depth) = new PrefixTree(start, node.firstKey, Lack, children.drop(nextIdx + 1))
							depth += 1
							if (depth == stack.length)
								stack = Array.copyOf(stack, stack.length << 1)
						}
						stack(depth) = node
					} //If the key is less or equal than all keys in node then we've found the node with the first key.
				} else if (nextIdx > -childrenNo - 1) {
					//key falls between two children of node, this is the last iteration.
					node = children(-nextIdx - 1)
					if (nextIdx > -childrenNo) {
						stack(depth) = new PrefixTree(start, node.firstKey, Lack, children.drop(-nextIdx))
						depth += 1
						if (depth == stack.length)
							stack = Array.copyOf(stack, stack.length << 1)
					}
					stack(depth) = node
					cmp = -1
				} else {
					//All keys in node are lesser than key. Retreat to our ancestor on the stack (if any) and end.
					depth -= 1
					cmp = -1
				}
			}
			while (depth < top) {
				stack(top) = null
				top -= 1
			}
			top = depth
			if (top >= 0 && stack(top).value.isEmpty)
				advance()
			this
		}

/*	def rangeUntil(until :String) :Iterator[E] =
		if (top < 0)
			this
		else {
			val length = until.length
			var node   :PrefixTree[V] = null
			var depth  = -1
			var cmp    = -1
			var offset = 0
			while (depth < top && cmp < 0) {
				depth += 1
				node   = stack(depth)
				offset = node.offset
				cmp    = compareRange(until, node.firstKey, 0, offset)
				if (cmp == 0)
					cmp = length - offset
			}
			/* Nodes in stack.slice(0, depth) have only keys greater than until and must be dropped.
			 * 1. If cmp < 0, then depth == top and until is lesser than all keys in the iterator. Drop everything.
			 * 2. If cmp > 0, then nodes in stack.slice(depth, top + 1) have keys only lesser than until,
			 *    and we must shift the stack.
			 * 3. If cmp == 0, then the key falls somewhere within stack(depth):
			 *      - if the key is shorter than all the keys in node, then it's lesser than them, and it's case 2)
			 *      - otherwise we must descend and trim trailing children of nodes under node.
			 */
			//stack.slice(0, depth) must be dropped
			if (cmp < 0 || cmp == 0 && node.value.isEmpty) {
				stack = null
				top   = -1
			} else if (cmp > 0) {
				arraycopy(stack, depth, stack, 0, top - depth)
				top -= depth
			} else {
				//We might need to remove non consecutive nodes from the stack, so mark them by nulls for now.
				var i = 0
				while (i < depth) {
					stack(i) = null
					i += 1
				}
//				var tailStack = new Array[PrefixTree[V]](32)
//				tailStack(0)  = node
//				var i         = 0
				while (offset < length && cmp == 0) {
					val children   = node.children
					val childrenNo = children.length
					val char       = until.charAt(offset)
					val nextIdx    = node.childIdx(char)
					if (nextIdx >= 0) {
						if (nextIdx > 0 || node.value.isDefined)
							stack(depth) = new PrefixTree(offset, node.firstKey)
							node =
						else {
							stack(depth) =
						}
					}
				}
			}
			this
		}
*/
	def toPrefixTree :PrefixTree[V] =
		if (top < 0)
			PrefixTree.Empty
		else if (top == 0)
			stack(top)
		else {
			var i = top
			var prefix = stack(top)
			while (i > 0) {
				i -= 1
				//This is suboptimal, because it compares the keys from 0 every time, as we don't know where they diverge.
				//If we went top-down, treating PrefixTree as mutable, then we could compare from the parent offset.
				//This would require that at least firstKey, size and depth are vars.
				prefix = PrefixTree(prefix, stack(i))
			}
			prefix
/*
			var suffix = stack(0)
			var offset = 0
			var i = 0
			while (i < top) {
				i += 1
				val prefix  = stack(i)
				val offset1 = prefix.offset
				val offset2 = suffix.offset
				val limit   = math.min(offset1, offset2)
				offset      = mismatch(prefix.firstKey, suffix.firstKey, offset, limit)
				if (i == limit)
					//Keys can't be equal, so offset1 != offset2. It's impossible for prefix to be a parent of suffix,
					// so offset2 must be less than offset1
					suffix = new PrefixTree(
						offset2, suffix.firstKey, suffix.value, suffix.children :+ prefix,
						prefix.size + suffix.size, math.max(suffix.depth, prefix.depth + 1)
					)
				else //Remember that prefix.firstKey < suffix.firstKey.
					suffix = PrefixTree(i, prefix, suffix)
			}
*/
		}
}


private class PrefixTreeEntryIterator[V](root :PrefixTree[V]) extends AbstractPrefixTreeIterator[V, (String, V)](root) {
	protected override def value(leaf :PrefixTree[V]) :(String, V) = leaf.entry.get
}

private class PrefixTreeKeyIterator[V](root :PrefixTree[V]) extends AbstractPrefixTreeIterator[V, String](root) {
	protected override def value(leaf :PrefixTree[V]) :String = leaf.firstKey

	override def to[C1](factory :Factory[String, C1]) :C1 = factory match {
		case CompanionFactory(StringSet | Set | collection.Set) => toPrefixTree.asInstanceOf[C1]
		case _ => super.to(factory)
	}
}

private class PrefixTreeValueIterator[V](root :PrefixTree[V]) extends AbstractPrefixTreeIterator[V, V](root) {
	protected override def value(leaf :PrefixTree[V]) :V = leaf.value.get
}




private abstract class AbstractPrefixTreeReverseIterator[V, +E](root :PrefixTree[V]) extends Iterator[E] {
	//Invariant: top <= 0 || stack(top).size == 1 && stack.slice(0, top).forall(n => n.size > 0)
	private[this] var top                         = -1
	private[this] var stack :Array[PrefixTree[V]] = null
	if (root.size > 0) {
		stack    = new Array[PrefixTree[V]](32)
		top      = 0
		stack(0) = root
		advance()
	}

	protected def value(leaf :PrefixTree[V]) :E

	override def hasNext :Boolean = top >= 0

	override def next() :E = {
		val res = stack(top)
		stack(top) = null
		top -= 1
		if (top < 0)
			stack = null
		else
			advance()
		value(res)
	}
	//Descends down the tree, removing the last child from the top node and pushing it on the stack,
	// until the top node has a single key and no children.
	private def advance() :Unit = {
		var parent     = stack(top)
		var parentSize = parent.size
		var children   = parent.children
		var childrenNo = children.length
		while (childrenNo > 0) { //parent.size > 0, so if it doesn't have children, it must contain a value
			val child = children(childrenNo - 1)
			val size  = child.size
			if (parent.value.isEmpty && childrenNo == 2 && { size + children(0).size == parentSize })
				//Don't create an illegal node with a single child and no value.
				stack(top) = children(0)
			else if (child.depth + 1 < parent.depth) {
				val siblings = children.take(childrenNo - 1)
				stack(top) = new PrefixTree(
					parent.offset, parent.firstKey, parent.value, siblings, parentSize - size, parent.depth
				)
			} else {
				val siblings = children.take(childrenNo - 1)
				stack(top) = new PrefixTree(parent.offset, parent.firstKey, parent.value, siblings, parentSize - size)
			}
			top += 1
			if (top == stack.length)
				stack = Array.copyOf(stack, stack.length << 1)
			stack(top) = child
			parent     = child
			parentSize = size
			children   = parent.children
			childrenNo = children.length
		}
	}

	override def knownSize :Int = {
		var i   = top
		var res = 0
		while (i >= 0) {
			val node = stack(i)
			res += node.size
			i -= 1
		}
		res
	}
}


private class PrefixTreeEntryReverseIterator[V](root :PrefixTree[V])
	extends AbstractPrefixTreeReverseIterator[V, (String, V)](root)
{
	protected override def value(leaf :PrefixTree[V]) :(String, V) = leaf.entry.get
}

private class PrefixTreeKeyReverseIterator[V](root :PrefixTree[V])
	extends AbstractPrefixTreeReverseIterator[V, String](root)
{
	protected override def value(leaf :PrefixTree[V]) :String = leaf.firstKey
}

private class PrefixTreeValueReverseIterator[V](root :PrefixTree[V])
	extends AbstractPrefixTreeReverseIterator[V, V](root)
{
	protected override def value(leaf :PrefixTree[V]) :V = leaf.value.get
}
