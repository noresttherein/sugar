package net.noresttherein.sugar.collections

import java.lang.System.arraycopy

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{SpecificIterableFactory, immutable}
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.{AbstractMap, AbstractSet, SeqMap}
import scala.collection.mutable.{Builder, ImmutableBuilder}

import net.noresttherein.sugar.outOfBounds_!
import net.noresttherein.sugar.JavaTypes.JStringBuilder
import net.noresttherein.sugar.arrays.extensions.ArrayObjectExtension
import net.noresttherein.sugar.collections.PrefixTree.EmptyChildrenArray
import net.noresttherein.sugar.collections.extensions.IterableOnceExtension
import net.noresttherein.sugar.numeric.extensions.BooleanExtension
import net.noresttherein.sugar.typist.casting.extensions.castTypeParamMethods
import net.noresttherein.sugar.typist.casting.extensions.castingMethods
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/** A prefix tree mapping strings into values of `V`.
  * Entries are stored in the alphabetical order, as specified by default `Char` ordering.
  * @define Coll `StringMap`
  * @define coll string map
  */
@SerialVersionUID(Ver)
final class StringMap[+V] private (root :PrefixTree[V])
	extends AbstractMap[String, V] with SeqMap[String, V] with SugaredIterable[(String, V)]
	   with SugaredIterableOps[(String, V), immutable.Iterable, StringMap[V]] with SlicingOps[(String, V), StringMap[V]]
	   with DefaultSerializable
{
	override def size :Int = root.size
	override def knownSize :Int = root.size

	override def head :(String, V) =
		if (root.size == 0) throw new NoSuchElementException("StringMap().head") else root.at(0).entry.get

	override def last :(String, V) =
		if (root.size == 0) throw new NoSuchElementException("StringMap().last") else root.at(root.size - 1).entry.get

	override def apply(key :String) :V = root.get(key) match {
		case Got(value) => value
		case _          => default(key)
	}
	override def get(key :String) :Option[V] = root.get(key).toOption

	override def getOrElse[V1 >: V](key :String, default: => V1) :V1 = root.get(key) match {
		case Got(value) => value
		case _          => default
	}

	override def removed(key :String) :StringMap[V] = {
		val node = root - key
		if (node eq root) this else StringMap(node)
	}

	override def updated[V1 >: V](key :String, value :V1) :StringMap[V1] = {
		val node = root.inserted(key, value)
		if (node eq root) this else StringMap(node)
	}

	override def updatedWith[V1 >: V](key :String)(remappingFunction :Option[V] => Option[V1]) :StringMap[V1] =
		if (root == null)
			this
		else {
			val previousValue = this.get(key)
			remappingFunction(previousValue) match {
				case None if previousValue.isDefined => removed(key)
				case None                            => this
				case Some(nextValue) =>
					if (previousValue.exists(_.asInstanceOf[AnyRef] eq nextValue.asInstanceOf[AnyRef])) coll
					else coll.updated(key, nextValue)
			}
		}

	override def +[V1 >: V](kv :(String, V1)) :StringMap[V1] = updated(kv._1, kv._2)
	override def ++[V1 >: V](elems :IterableOnce[(String, V1)]) :StringMap[V1] = concat(elems)

	override def concat[V1 >: V](elems :IterableOnce[(String, V1)]) :StringMap[V1] =
		elems.toBasicOps.foldLeft(this :StringMap[V1])(_ + _)


	protected override def trustedSlice(from :Int, until :Int) :StringMap[V] =
		new StringMap(root.slice(from, until))


	override def foreach[U](f :((String, V)) => U) :Unit =
		if (root.isDeep) iterator.foreach(f)
		else root.foreach(f)

	override def foreachEntry[U](f :(String, V) => U) :Unit =
		if (root.isDeep) super.foreachEntry(f)
		else root.foreachEntry(f)

	override def foldLeft[A](z :A)(op :(A, (String, V)) => A) :A =
		if (root.isDeep) iterator.foldLeft(z)(op)
		else root.foldLeft(z)(op)

	override def foldRight[A](z :A)(op :((String, V), A) => A) :A =
		if (root.isDeep) iterator.foldRight(z)(op)
		else root.foldRight(z)(op)


	override def transform[W](f: (String, V) => W): StringMap[W] = map { case (k, v) => (k, f(k, v)) }

	def map[W](f :((String, V)) => (String, W)) :StringMap[W] = StringMap.from(iterator.map(f))

	def flatMap[W](f :((String, V)) => IterableOnce[(String, W)]) :StringMap[W] = StringMap.from(iterator.flatMap(f))

	override def keySet :StringSet = if (root.size == 0) StringSet.empty else new StringSet(root)

	override def iterator :Iterator[(String, V)] =
		if (root.size == 0) Iterator.empty else new PrefixTreeEntryIterator(root)

	override def keysIterator :Iterator[String] =
		if (root.size == 0) Iterator.empty else new PrefixTreeKeyIterator(root)

	override def valuesIterator :Iterator[V] =
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

	private def apply[V](tree :PrefixTree[V]) :StringMap[V] =
		if (tree.size == 0) Empty else new StringMap(tree)

	def from[V](items :IterableOnce[(String, V)]) :StringMap[V] = items match {
		case map :StringMap[V @unchecked] => map
		case empty :Iterable[_] if empty.isEmpty => this.empty
		case empty :Iterator[_] if empty.isEmpty => this.empty
		case _ =>
			val root = ((PrefixTree.Empty :PrefixTree[V]) /: items.toIterableOnceOps) {
				case (tree, (key, value)) => tree.inserted(key, value)
			}
			if (root == null) Empty else new StringMap(root)
	}

	def empty[V] :StringMap[V] = Empty

	private[this] val Empty = new StringMap[Nothing](null)

	def newBuilder[V] :Builder[(String, V), StringMap[V]] = new PrefixTreeBuilder[V].mapResult(apply(_))

	private class PrefixTreeBuilder[V] extends ImmutableBuilder[(String, V), PrefixTree[V]](PrefixTree.Empty) {
		override def addOne(elem :(String, V)) :this.type = { elems = elems.inserted(elem._1, elem._2); this }
	}
}





/** A set of strings in the alphabetical order (derived from natural `Char` comparison).
  * @define Coll `StringSet`
  * @define coll string set
  */
final class StringSet(root :PrefixTree[_])
	extends AbstractSet[String] with SpecificIterableFactoryDefaults[String, Set, StringSet]
	   with SugaredIterable[String] with SugaredIterableOps[String, Set, StringSet]
	   with SlicingOps[String, StringSet] with DefaultSerializable
{
	override def size :Int = root.size
	override def knownSize :Int = root.size

	override def head :String =
		if (root.size == 0) throw new NoSuchElementException("StringSet().head") else root.at(0).key

	override def last :String =
		if (root.size == 0) throw new NoSuchElementException("StringSet().last") else root.at(root.size - 1).key

	//todo: think of a universal name for OrderedSet
	def at(i :Int) :String = root.at(i).key

	override def contains(elem :String) :Boolean = root.get(elem).isDefined

	override def incl(elem :String) :Set[String] = {
		val node = root.inserted(elem, elem)
		if (node eq root) this else StringSet(node)
	}

	override def excl(elem :String) :Set[String] = {
		val node = root - elem
		if (node eq root) this else StringSet(node)
	}

	override def iterator :Iterator[String] = if (root.size == 0) Iterator.empty else new PrefixTreeKeyIterator(root)

	override def foreach[U](f :String => U) :Unit =
		if (root.isDeep) iterator.foreach(f)
		else root.foreachKey(f)

	override def foldLeft[A](z :A)(op :(A, String) => A) :A =
		if (root.isDeep) iterator.foldLeft(z)(op)
		else root.foldLeftKeys(z)(op)

	override def foldRight[A](z :A)(op :(String, A) => A) :A =
		if (root.isDeep) iterator.foldRight(z)(op)
		else root.foldRightKeys(z)(op)

	def map(f :String => String) :StringSet = StringSet.fromSpecific(iterator.map(f))

	def flatMap(f :String => IterableOnce[String]) :StringSet = StringSet.fromSpecific(iterator.flatMap(f))

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
			apply(root)
	}

	private def apply(tree :PrefixTree[_]) :StringSet = if (tree.size == 0) empty else new StringSet(tree)

	override def newBuilder :Builder[String, StringSet] = (new PrefixTreeBuilder).mapResult(apply)

	private class PrefixTreeBuilder extends ImmutableBuilder[String, PrefixTree[String]](PrefixTree.Empty) {
		override def addOne(elem :String) :this.type = { elems = elems.inserted(elem, elem); this }
	}
}






private object PrefixTree {
	@inline def apply(key :String) :PrefixTree[String] =
		new PrefixTree(key.length, key, Got(key), EmptyChildrenArray.castParam[PrefixTree[String]], 1, 1)

	@inline def apply[V](key :String, value :V) :PrefixTree[V] =
		new PrefixTree(key.length, key, Got(value), EmptyChildrenArray.castParam[PrefixTree[V]], 1, 1)

//	@inline def apply[V](entry :(String, V)) :PrefixTree[V] =
//		new PrefixTree(entry._1.length, entry._1, Got(entry._2), EmptyChildrenArray.castParam[PrefixTree[V]], 1, 1)

	@inline def apply[V](offset :Int, key :String, left :PrefixTree[V], right :PrefixTree[V]) :PrefixTree[V] = {
		val depth = math.max(left.depth, right.depth) + 1
		new PrefixTree(offset, key, Lack, Array.two(left, right), left.size + right.size, depth)
	}

	@inline def apply[V](key :String, value :V, child :PrefixTree[V]) :PrefixTree[V] =
		new PrefixTree(key.length, key, Got(value), Array.one(child), child.size + 1, child.depth + 1)

	val EmptyChildrenArray = new Array[PrefixTree[Nothing]](0)

	val Empty = new PrefixTree[Nothing](0, "", Lack, EmptyChildrenArray, 0)

	private final val StackRecursionDepthLimit = 512
}


private final class PrefixTree[+V](val offset :Int, val key :String, val value :Opt[V],
                                   children :Array[PrefixTree[V]], val size :Int, val depth :Int)
	extends IterableOnce[V]
{
	def this(offset :Int, key :String, value :Opt[V], children :Array[PrefixTree[V]], size :Int) =
		this(offset, key, value, children, size, if (children.length == 0) 1 else children.maxBy(_.depth).depth + 1)

	def isDeep = depth > PrefixTree.StackRecursionDepthLimit

	override def knownSize :Int = size
	override def iterator :Iterator[V] = if (size == 0) Iterator.empty else new PrefixTreeValueIterator[V](this)

//	def copy[V1 >: V](offset :Int = offset, key :String = key, value :Opt[V1] = value,
//	                  children :Array[PrefixTree[V1]] = this.children.castFrom[Array[PrefixTree[V]], Array[PrefixTree[V1]]],
//	                  size :Int = size, depth :Int = depth) :PrefixTree[V1] =
//		new PrefixTree(offset, key, value, children, size, depth)

	def entry :Opt[(String, V)] = value.map((key, _))

	private def childIdx(char :Char) :Int =
		if (children.length == 0)
			-1
		else {
			var lo     = 0
			var hi     = children.length
			val offset = this.offset
			while (lo < hi) {
				val middle = (lo + hi) >>> 1
				val child = children(middle)
				if (char > child.key.charAt(offset)) lo = middle + 1
				else hi = middle
			}
			if (lo < children.length && children(lo).key.charAt(offset) == char) lo
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

	def dropFirstChild :PrefixTree[V] =
		if (children.length <= 1)
			null
		else if (children.length == 2 && value.isEmpty)
			children(1)
		else {
			val first = children(0)
			val k = if ((key eq first.key) && key.length != offset && children.length > 0) children(1).key else key
			if (first.depth < depth - 1)
				new PrefixTree(offset, k, value, children.tail, size - first.size, depth)
			else
				new PrefixTree(offset, k, value, children.tail, size - first.size)
		}

//	def add[V1 >: V](entry :(String, V1)) :PrefixTree[V1] = add(entry._1, entry._2)
//
//	def +[V1 >: V](entry :(String, V1)) :PrefixTree[V1] = {
//		val i      = -childIdx(entry._1.charAt(offset)) - 1
//		val length = children.length
//		val array  = new Array[PrefixTree[V1]](length + 1)
//		arraycopy(children, 0, array, 0, i)
//		arraycopy(children, i, array, i + 1, length - i)
//		array(i) = PrefixTree(entry)
//		new PrefixTree[V1](offset, this.key, value, array, size + 1, depth)
//	}

	/** Adds a new node to this node's children.
	  * Requires `key.length > depth && key(this.depth) != child.key(this.depth)` for all children of this node.
	  */
	def add[V1 >: V](key :String, value :V1) :PrefixTree[V1] = {
		val i      = -childIdx(key.charAt(offset)) - 1
		val length = children.length
		val array  = new Array[PrefixTree[V1]](length + 1)
		arraycopy(children, 0, array, 0, i)
		arraycopy(children, i, array, i + 1, length - i)
		array(i) = PrefixTree(key, value)
		new PrefixTree[V1](offset, this.key, this.value, array, size + 1, depth)
	}

	/** Substitutes the current child whose key at `this.depth` equals `char` with the given node.
	  * Assumes a child starting with `char` already exists.
	  */
	def updated[V1 >: V](char :Char, child :PrefixTree[V1]) :PrefixTree[V1] = {
		val i = childIdx(char)
		val length = children.length
		val array = new Array[PrefixTree[V1]](length)
		arraycopy(children, 0, array, 0, length)
		val old = array(i)
		array(i) = child
		//Free the key for garbage collection in case it belonged to a removed node
		val k = if (key.length == offset) key else child.key
		if (child.depth >= old.depth)
			new PrefixTree(offset, k, value, array, size + child.size - old.size, math.max(depth, child.depth + 1))
		else
			new PrefixTree(offset, k, value, array, size + child.size - old.size)
	}

//	/** Updates/sets the value on this node. */
//	def updated[V1 >: V](value :V1) :PrefixTree[V1] = updated(key, value)

	/** Updates/sets the value on this node. */
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

	def -(char :Char) :PrefixTree[V] = {
		val length = children.length
		if (length == 1) {
			val noChildren = EmptyChildrenArray.castFrom[Array[PrefixTree[Nothing]], Array[PrefixTree[V]]]
			new PrefixTree(offset, key, value, noChildren, value.size, 1)
		} else {
			val i = childIdx(char)
			val old = children(i)
			val array = new Array[PrefixTree[V]](length -1)
			arraycopy(children, 0, array, 0, i)
			arraycopy(children, i + 1, array, i, length - i - 1)
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
		else new PrefixTree(offset, children(0).key, Lack, children, size - 1, depth)


	def at(i :Int) :PrefixTree[V] =
		if (i < 0 || i >= size)
			outOfBounds_!(i, size)
		else if (i == 0)
			if (value.isDefined) this
			else children(0).at(0)
		else {
			var count    = value.isDefined.toInt
			var childIdx = 0
			var child    = children(childIdx)
			while (count + child.size <= i && childIdx < children.length - 1) {
				count    += child.size
				childIdx += 1
				child     = children(childIdx)
			}
			child.at(i - count)
		}

	def get(key :String) :Opt[V] = {
		val length = key.length
		var node   = this
		var depth  = 0
		while (node != null && {
			depth = node.offset
			depth < length
		})
			node = node.child(key.charAt(depth))
		if (node == null || depth > length)
			Lack
		else
			node.value
	}

	//todo: we should have an implementation using a stack on the heap for all these methods to avoid stack overflows
	def -(key :String) :PrefixTree[V] = {
		val length = key.length
		//invariant: depth <= key.length and node.key.substring(0, depth) == key.substring(0, depth) and depth <= node.depth
		def remove(node :PrefixTree[V], offset :Int) :PrefixTree[V] = {
			val nodeKey   = node.key
			val nodeDepth = node.offset
			var i         = offset
			val end       = math.min(length, nodeDepth)
			while (i < end) {
				if (key.charAt(i) != nodeKey.charAt(i))         //key not present in the tree
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
					val substitute = remove(child, nodeDepth)
					if (substitute eq child) node
					else if (substitute.size == 0) node - char
					else node.updated(char, substitute)
				}
			}
		}
		remove(this, 0)
	}

	def inserted[V1 >: V](key :String, value :V1) :PrefixTree[V1] = {
		val length = key.length
		//invariant: offset <= key.length and node.key.substring(0, offset) == key.substring(0, offset) and offset <= node.offset
		def update(node :PrefixTree[V], offset :Int) :PrefixTree[V1] =
			if (offset == length)
				if (offset == node.offset) node.updated(key, value)
				else PrefixTree(key, value, node)
			else {
				val nodeKey   = node.key
				val nodeDepth = node.offset
				var i         = offset
				val end       = math.min(length, nodeDepth)
				while (i < end) {
					val keyChar  = key.charAt(i)
					val nodeChar = nodeKey.charAt(i)
					if (keyChar != nodeChar)
						if (keyChar < nodeChar)
							return PrefixTree(i, key, PrefixTree(key, value), node)
						else
							return PrefixTree(i, key, node, PrefixTree(key, value))
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
		update(this, 0)
	}


	//todo: recursion with a heap stack
	def slice(from :Int, until :Int) :PrefixTree[V] =
		if (from <= 0 && until >= size)
			this
		else if (value.isDefined & until == 1)
			if (from <= 0) PrefixTree(key, value.get)
			else PrefixTree.Empty
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
					new PrefixTree(offset, key, value, slicedChildren, until - from)
				else {
					val k = if (key.length == offset) key else slicedChildren(0).key
					new PrefixTree(offset, k, Lack, slicedChildren, until - from)
				}
			}
		}

	def foreach[U](f :((String, V)) => U) :Unit = {
		if (value.nonEmpty)
			f((key, value.get))
		val end = children.length
		var i   = 0
		while (i < end) {
			children(i).foreach(f)
			i += 1
		}
	}
	def foreachEntry[U](f :(String, V) => U) :Unit = {
		if (value.nonEmpty)
			f(key, value.get)
		val end = children.length
		var i   = 0
		while (i < end) {
			children(i).foreachEntry(f)
			i += 1
		}
	}
	def foreachKey[U](f :String => U) :Unit = {
		if (value.nonEmpty)
			f(key)
		val end = children.length
		var i   = 0
		while (i < end) {
			children(i).foreachKey(f)
			i += 1
		}
	}
	def foldLeft[A](z :A)(f :(A, (String, V)) => A) :A = {
		var acc = if (value.nonEmpty) f(z, entry.get) else z
		val end = children.length
		var i   = 0
		while (i < end) {
			acc = children(i).foldLeft(acc)(f)
			i  += 1
		}
		acc
	}
	def foldLeftKeys[A](z :A)(f :(A, String) => A) :A = {
		var acc = if (value.nonEmpty) f(z, key) else z
		val end = children.length
		var i   = 0
		while (i < end) {
			acc = children(i).foldLeftKeys(acc)(f)
			i  += 1
		}
		acc
	}
	def foldRight[A](z :A)(f :((String, V), A) => A) :A = {
		var acc = z
		var i   = children.length
		while (i > 0) {
			i -= 1
			acc = children(i).foldRight(acc)(f)
		}
		if (value.isEmpty) acc else f(entry.get, acc)
	}
	def foldRightKeys[A](z :A)(f :(String, A) => A) :A = {
		var acc = z
		var i   = children.length
		while (i > 0) {
			i -= 1
			acc = children(i).foldRightKeys(acc)(f)
		}
		if (value.isEmpty) acc else f(key, acc)
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
			res append child.key.substring(offset, child.offset)
			child.toString(res)
		}
		res append '}'
	}
}






private abstract class AbstractPrefixTreeIterator[V, +E](root :PrefixTree[V]) extends Iterator[E] {
//		def this(root :Node[V]) = this()
	private[this] var top                         = 0
	private[this] var stack :Array[PrefixTree[V]] = new Array[PrefixTree[V]](32)
	stack(0) = root
	advance()

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

	protected def value(leaf :PrefixTree[V]) :E
}


private class PrefixTreeEntryIterator[V](root :PrefixTree[V]) extends AbstractPrefixTreeIterator[V, (String, V)](root) {
	protected override def value(leaf :PrefixTree[V]) :(String, V) = leaf.entry.get
}

private class PrefixTreeKeyIterator[V](root :PrefixTree[V]) extends AbstractPrefixTreeIterator[V, String](root) {
	protected override def value(leaf :PrefixTree[V]) :String = leaf.key
}

private class PrefixTreeValueIterator[V](root :PrefixTree[V]) extends AbstractPrefixTreeIterator[V, V](root) {
	protected override def value(leaf :PrefixTree[V]) :V = leaf.value.get
}
