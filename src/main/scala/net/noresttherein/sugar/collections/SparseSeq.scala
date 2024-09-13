package net.noresttherein.sugar.collections

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.generic.DefaultSerializationProxy
import scala.collection.{EvidenceIterableFactory, Factory, SeqFactory, SpecificIterableFactory, StrictOptimizedIterableOps, immutable}
import scala.collection.immutable.{AbstractMap, AbstractSeq, IndexedSeqOps, IntMap, SortedMap, SortedMapOps, StrictOptimizedSeqOps, TreeMap}
import scala.collection.mutable.{Builder, ReusableBuilder}

import net.noresttherein.sugar.collections.SparseSeq.Missing
import net.noresttherein.sugar.collections.extensions.{BuilderExtension, IterableOnceExtension, IteratorExtension}
import net.noresttherein.sugar.exceptions.{illegalState_!, noSuch_!, outOfBounds_!}
import net.noresttherein.sugar.witness.DefaultValue




/** A sequence which consists mostly of repetitions of the same element.
  * Implemented by a `Map[Int, E]` with a default element. With an empty map it becomes an efficient implementation
  * of a constant sequence.
  * @note This is a `SeqOps[E, SparseSeq, SparseSeq]` using `SparseSeq` as its generic collection type so that
  *       for concatenation, append/prepend and similar methods return the same type. Unfortunately, some `SeqOps`
  *       methods, most notably `flatMap`, which return `CC[_]` for a different element type, do not produce
  *       a default element for the new element type. They must still return a `SparseSeq`, but the implementation
  *       will store all the elements in the underlying map. An important exception is `map[O](f: E => O)`,
  *       which returns a `SparseSeq[O]` using `f(this.default)` as the dominating element.
  * @author Marcin Mościcki
  * @define Coll `SparseSeq`
  * @define coll sparse sequence
  */ //todo: test
//ConstSeq still has its place because it produces a normal sequence, so can be flat mapped without headaches.
@SerialVersionUID(Ver)
final class SparseSeq[+E] private (private val offset :Int, private val map :SortedMap[Int, E],
                                   override val length :Int, val default :E)
	extends AbstractSeq[E] with IndexedSeq[E] with IndexedSeqOps[E, SparseSeq, SparseSeq[E]]
	   with SugaredIterable[E] with SugaredSeqOps[E, SparseSeq, SparseSeq[E]]
	   with SugaredSlicingOps[E, SparseSeq, SparseSeq[E]]
	   with StrictOptimizedSeqOps[E, SparseSeq, SparseSeq[E]]
//	   with EvidenceIterableFactoryDefaults[E, SparseSeq, DefaultValue]
{
	//An alias to easily switch implementation if desired.
	private type IndexMap[+V] = SortedMap[Int, V]

	/** Indices of non-default elements in this sequence, in their exact order. */
	def updatedIndices :Seq[Int] =
//		if (map.size == 0) Nil else RelayArray.from(map.keys).sorted
		if (map.size == 0) Nil else DefaultIndexedSeq from map.keys

	override def apply(i :Int) :E =
		if (i < 0 | i >= length)
			outOfBounds_!(i, this)
		else {
			val elem = map.getOrElse(offset + i, Missing[E])
			if (elem.asInstanceOf[AnyRef] eq Missing) default
			else elem
		}

	//Override conflict.
	override def segmentLength(p :E => Boolean, from :Int) :Int = super.segmentLength(p, from)

	override def empty :SparseSeq[E] = SparseSeq.empty(default)

	protected override def clippedSlice(from :Int, until :Int) :SparseSeq[E] =
		if (map.isEmpty)
			new SparseSeq(0, map, until - from, default)
		else {
			val newMap = TreeMap from map.range(from + offset, until + offset)
			//map.dropWhile(_._1 + offset < from).takeWhile(_._1 + offset < until)
			new SparseSeq(offset + from, newMap, until - from, default)
		}

	override def updated[U >: E](i :Int, elem :U) :SparseSeq[U] =
		if (i < 0 | i >= length)
			outOfBounds_!(i, this)
		else
			new SparseSeq(offset, updatedMap(i, elem), length, default)

	override def inserted[U >: E](index :Int, elem :U) :SparseSeq[U] =
		if (index < 0 | index > length)
			outOfBounds_!(index, this, "inserted")
		else if (length == Int.MaxValue)
			illegalState_!("Cannot insert another element: the maximum sequence size of Int.MaxValue reached.")
		else if (index == 0)
			prepended(elem)
		else if (index == length)
			appended(elem)
		else
			take(index).appended(elem).appendedAll(drop(index))

	//todo:
//	override def updatedAll[U >: E](index :Int, elems :IterableOnce[U]) :SparseSeq[U] = ???
//	override def overwritten[U >: E](index :Int, elems :IterableOnce[U]) :SparseSeq[U] = ???
//	override def insertedAll[U >: E](index :Int, elems :IterableOnce[U]) :SparseSeq[U] = ???

	/** Creates a new sequence with `elem` at position `i`, and other positions taken by corresponding elements
	  * from this $coll. This is the same as [[net.noresttherein.sugar.collections.SparseSeq.updated updated]],
	  * but if `i >= length`, the new sequence is extended to length `i + 1`, and all positions in range `[length, i)`
	  * are set to the default element.
	  */
	@throws[IndexOutOfBoundsException]("if i is negative or equals Int.MaxValue.")
	def set[U >: E](i :Int, elem :U) :SparseSeq[U] =
		if (i < 0 | i == Int.MaxValue)
			outOfBounds_!(i, this)
		else if (i >= length)
			if (i < Int.MaxValue - offset)
				new SparseSeq(offset, updatedMap(i, elem), i + 1, default)
			else {
				//We don't simply shift to zero because we are likely used as a queue, with elements dropped from the front.
				val newOffset  = (0xffffffff - i - 1 >>> 1) + Int.MinValue //unsigned arithmetic
				val shiftedMap = shiftTo(newOffset)
				updateOutOfRange(shiftedMap, newOffset, i, elem, i + 1)
			}
		else
			new SparseSeq(offset, updatedMap(i, elem), length, default)

	@inline private def updatedMap[U >: E](i :Int, elem :U) :IndexMap[U] =
		if (elem == default) map.removed(offset + i) else map.updated(offset + i, elem)

	@inline private def updateOutOfRange[U >: E](map :IndexMap[U], offset :Int, i :Int, elem :U, length :Int) = {
		val newMap = if (elem == default) map else map.updated(offset + i, elem)
		new SparseSeq(offset, newMap, length, default)
	}
	private def shiftTo(newOffset :Int) :IndexMap[E] =
		map.map { case (i, elem) => (i - offset + newOffset, elem) }

	override def appended[U >: E](elem :U) :SparseSeq[U] = set(length, elem)

	override def prepended[U >: E](elem :U) :SparseSeq[U] =
		if (length == Int.MaxValue)
			illegalState_!("Cannot prepend another element: maximum length reached.")
		else if (offset == Int.MinValue) {
			val newOffset  = (0xffffffff - length >>> 1) + Int.MinValue //unsigned arithmetic
			val shiftedMap = shiftTo(newOffset)
			updateOutOfRange(shiftedMap, newOffset - 1, 0, elem, length + 1)
		} else
			new SparseSeq(offset - 1, updatedMap(-1, elem), length + 1, default)

	override def appendedAll[U >: E](suffix :IterableOnce[U]) :SparseSeq[U] = suffix match {
		case _ if suffix.knownSize == 0 =>
			this
		case that :SparseSeq[U] if default == that.default =>
			SparseSeq.concat(this, that)
		case _ =>
			val newMap = suffix.foldLeftWithIndex(map :IndexMap[U]) {
				(map, elem, i) => map.updated(offset + length + i, elem)
			}
			new SparseSeq(offset, newMap, length + newMap.size - map.size, default)
	}
	override def prependedAll[U >: E](prefix :IterableOnce[U]) :SparseSeq[U] = {
		val thatSize = prefix.knownSize
		prefix match {
			case _ if thatSize == 0 =>
				this
			case that :SparseSeq[U] if default == that.default =>
				SparseSeq.concat(that, this)
			case _ if thatSize > 0 =>
				val newOffset = offset - thatSize
				val newMap = prefix.toBasicOps.foldLeftWithIndex(map :IndexMap[U]) {
					(map, elem, i) => map.updated(newOffset + i, elem)
				}
				new SparseSeq(newOffset, newMap, length + thatSize, default)
			case _ =>
				(SparseSeq.withDefault[U](default) ++= prefix ++= this).result()
		}
	}


	override def map[O](f :E => O) :SparseSeq[O] = {
		val newDefault =
			if (default.asInstanceOf[AnyRef] eq Missing) default.asInstanceOf[O]
			else f(default)
		val newMap = map.foldLeft(SortedMap.empty[Int, O]) {
			case (map, (idx, elem)) =>
				val newElem = f(elem)
				if (newElem == newDefault) map else map.updated(idx - offset, newElem)
		}
		new SparseSeq(0, newMap, length, newDefault)
	}

//	implicit protected override def iterableEvidence :DefaultValue[E] = DefaultValue(default)
//
//	override def evidenceIterableFactory :EvidenceIterableFactory[SparseSeq, DefaultValue] = SparseSeq

	protected override def fromSpecific(coll :IterableOnce[E @uncheckedVariance]) :SparseSeq[E] =
		SparseSeq.from(coll, default)

	protected override def newSpecificBuilder :Builder[E @uncheckedVariance, SparseSeq[E]] =
		SparseSeq.withDefault(default)

	override def iterator :Iterator[E] = IndexedSeqIterator(this)
	override def reverseIterator :Iterator[E] = ReverseIndexedSeqIterator(this)

	override def iterableFactory :SeqFactory[SparseSeq] = SparseSeq.dense

	protected[this] override def className :String = "SparseSeq"

	override def toString :String =
		map.iterator.map(kv => "#" + kv._1 + ": " + kv._2).mkString(
			"SparseSeq(" + default + "; ", ", ", ")"
		)
//	override def specificFactory :SpecificIterableFactory[E, SparseSeq[E]] = SparseSeq(default)
}




/** Builds sparse sequences - sequences where most elements are equal. The dominating ('default')
  * element may be specified either explicitly, or through `DefaultValue` type class.
  * @define Coll `SparseSeq`
  * @define coll sparse sequence
  */
@SerialVersionUID(Ver)
case object SparseSeq extends EvidenceIterableFactory[SparseSeq, DefaultValue] {
	/** A $Coll of length `length` and all elements equal to `elem` (that is, `elem` itself appearing at all positions. */
	def const[E](length :Int)(elem :E) :SparseSeq[E] =
		new SparseSeq(0, SortedMap.empty[Int, E], length, elem)

	/** A factory producing only sequences with the given default element. */
	def of[E](elem :E) :SpecificIterableFactory[E, SparseSeq[E]] = new SparseSeqFactory(elem)

	def empty[E](default :E) :SparseSeq[E] = new SparseSeq(0, SortedMap.empty[Int, E], 0, default)

	override def empty[E :DefaultValue] :SparseSeq[E] = new SparseSeq(0, SortedMap.empty, 0, DefaultValue[E]().value)

	def from[E](source :IterableOnce[E], default :E) :SparseSeq[E] = source match {
		case seq :SparseSeq[E] if default == seq.default => seq
		case _ if source.knownSize == 0 => empty(default)
		case _ => (withDefault(default) ++= source).result()
	}

	override def from[E :DefaultValue](it :IterableOnce[E]) :SparseSeq[E] = from(it, DefaultValue[E]().value)

	def withDefault[E](default :E) :Builder[E, SparseSeq[E]] =
		new SparseSeqBuilder(default, SortedMap.empty, 0, 0)

	override def newBuilder[E :DefaultValue] :Builder[E, SparseSeq[E]] = withDefault(DefaultValue[E]().value)


	/** A factory of [[net.noresttherein.sugar.collections.SparseSeq SparseSeq]] which store all their elements
	  * unlike the default implementation. Used by methods such as `flatMap` which produce no default element
	  * for the created sequence.
	  * @define Coll `SparseSeq`
	  * @define coll non-sparse sequence
	  */
	object dense extends SeqFactory[SparseSeq] {
		override def from[E](source :IterableOnce[E]) :SparseSeq[E] = source match {
			case seq :SparseSeq[E]          => seq
			case _ if source.knownSize == 0 => Empty
//			case _ => SparseSeq.from(source, Default).asInstanceOf[SparseSeq[E]]
			case _ =>
				val map = DenseIntMap.from(source.iterator.mapWithIndex { (v, i) => (i, v) })
				new SparseSeq(0, map, map.size, Missing[E])
		}
		override def empty[A] :SparseSeq[A] = Empty

		override def newBuilder[E] :Builder[E, SparseSeq[E]] =
			new SparseSeqBuilder(Missing[E], DenseIntMap.empty[E], 0, 0)
//			new SparseSeqBuilder(Missing, SortedMap.empty[Int, Any], 0, 0).asInstanceOf[Builder[E, SparseSeq[E]]]

		private[this] val Empty =
			new SparseSeq(0, SortedMap.empty[Int, Any], 0, Missing).asInstanceOf[SparseSeq[Nothing]]
	}

	private case object Missing {
		@inline def apply[V] :V = Missing.asInstanceOf[V]
	}


	/** A factory of [[net.noresttherein.sugar.collections.SparseSeq SparseSeq]] with a specific 'default' (dominating)
	  * element.
	  * @define Coll `SparseSeq`
	  * @define coll sparse sequence
	  */
	final class SparseSeqFactory[E](val default :E) extends SpecificIterableFactory[E, SparseSeq[E]] {
		override val empty :SparseSeq[E] = new SparseSeq(0, SortedMap.empty[Int, E], 0, default)

		override def newBuilder :Builder[E, SparseSeq[E]] =
			new SparseSeqBuilder(default, SortedMap.empty[Int, E], 0, 0)

		override def fromSpecific(it :IterableOnce[E]) :SparseSeq[E] = it match {
			case _ if it.knownSize == 0 => empty
			case seq :SparseSeq[E] if default == seq.default => seq
			case _ => (newBuilder ++= it).result()
		}
		override def hashCode :Int = default.hashCode
		override def equals(that :Any) :Boolean = that match {
			case other :SparseSeqFactory[_] => default == other.default
			case _                          => false
		}
		override def toString :String = "SparseSeqFactory(" + default + ")"
	}


	private class SparseSeqBuilder[E](default :E, private[this] var map :SortedMap[Int, E],
	                                  private[this] var size :Int, offset :Int)
		extends ReusableBuilder[E, SparseSeq[E]]
	{
		override def knownSize = size
		override def clear() :Unit = { map = SortedMap.empty; size = 0 }

		override def result() = {
			val res = new SparseSeq(offset, map, size, default)
			clear()
			res
		}
		override def addOne(elem :E) = {
			if (elem != default)
				map = map.updated(offset + size, elem)
			if (offset + size == Int.MaxValue)
				illegalState_!("Maximum size reached: " + size + " elements added to builder at offset " + offset + ".")
			size += 1
			this
		}
		override def addAll(elems :IterableOnce[E]) :this.type = elems match {
			case _ if elems.knownSize == 0 => this
			case seq :SparseSeq[E] if default == seq.default =>
				if (seq.size > Int.MaxValue - math.max(offset, 0) - size)
					illegalState_!(
						"Cannot add " + seq.size + " elements to a builder of size " + size + " at offset " + offset + "."
					)
				val shift = offset + size - seq.offset
				map = seq.map.foldLeft(map) {
					case (map, (idx, elem)) => map.updated(idx + shift, elem)
				}
				size += seq.length
				this
			case _ => super.addAll(elems)
		}
	}


	/** Concatenates two sequences with the same default element. */
	private def concat[E](seq1 :SparseSeq[E], seq2 :SparseSeq[E]) :SparseSeq[E] = {
		val size1 = seq1.length
		val size2 = seq2.length
		val offset1 = seq1.offset
		val offset2 = seq2.offset
		if (size2 > Int.MaxValue - size1)
			illegalState_!(
				"Cannot add " + size1 + " elements to a sequence of length " + size2 + ": exceeded maximum size."
			)
		else if (seq1.map.size >= seq2.map.size && size2 <= Int.MaxValue - offset1 - size1) {
			val shift = offset1 + size1 - offset2
			val newMap = seq2.map.foldLeft(seq1.map) {
				case (map, (idx, elem)) => map.updated(idx + shift, elem)
			}
			new SparseSeq(offset1, newMap, size1 + size2, seq1.default)
		} else if (offset2 >= Int.MinValue + size1) {
			val newOffset = offset2 - size1
			val shift = newOffset - offset1
			val newMap = seq1.map.foldLeft(seq2.map) {
				case (map, (idx, elem)) => map.updated(idx + shift, elem)
			}
			new SparseSeq(newOffset, newMap, size1 + size2, seq1.default)
		} else {
			val size = size1 + size2
			val newOffset = (Int.MaxValue - size - Int.MinValue >> 1) - Int.MinValue
			val map1   = seq1.map.map { case (i, elem) => (i - offset1 + newOffset, elem) }
			val shift  = newOffset + size1 - offset2
			val newMap = seq2.map.foldLeft(map1) { case (map, (i, elem)) => map.updated(i + shift, elem) }
			new SparseSeq(newOffset, newMap, size, seq1.default)
		}
	}
}







@SerialVersionUID(Ver)
private class DenseIntMap[+V] private (data :IntMap[V])
	extends AbstractMap[Int, V] with SortedMap[Int, V] with SortedMapOps[Int, V, SortedMap, DenseIntMap[V]]
	   with StrictOptimizedIterableOps[(Int, V), immutable.Iterable, DenseIntMap[V]]
{
	override def ordering :Ordering[Int] = Ordering.Int
	override def size :Int = data.size
	override def knownSize :Int = data.knownSize

	override def apply(key :Int) :V =
		try data(key ^ Int.MinValue) catch {
			case _ :NoSuchElementException => noSuch_!(key.toString)
		}
	override def get(key :Int) :Option[V] = data.get(key ^ Int.MinValue)

	override def updated[U >: V](key :Int, value :U) :DenseIntMap[U] =
		new DenseIntMap(data.updated(key ^ Int.MinValue, value))

	override def removed(key :Int) :DenseIntMap[V] = {
		val updated = data.removed(key)
		if (updated eq data) this else new DenseIntMap(updated)
	}
	override def empty :DenseIntMap[V] = DenseIntMap.empty

	override def iterator :Iterator[(Int, V)] = data.iterator.map { case (k, v) => (k ^ Int.MinValue, v) }
	override def keysIterator :Iterator[Int] = data.keysIterator.map(_ ^ Int.MinValue)
	override def iteratorFrom(start :Int) :Iterator[(Int, V)] = iterator.dropWhile(_._1 < start)
	override def keysIteratorFrom(start :Int) :Iterator[Int] = keysIterator.dropWhile(_ < start)

	override def rangeImpl(from :Option[Int], until :Option[Int]) :DenseIntMap[V] = {
		var it = from match {
			case Some(v) => iteratorFrom(v)
			case _       => iterator
		}
		until match {
			case Some(v) => it = it.takeWhile(_._1 < v)
			case _ =>
		}
		new DenseIntMap(IntMap.from(it))
	}

	protected override def fromSpecific(coll :IterableOnce[(Int, V @uncheckedVariance)]) :DenseIntMap[V] =
		DenseIntMap.from(coll)

	protected override def newSpecificBuilder :Builder[(Int, V @uncheckedVariance), DenseIntMap[V]] =
		DenseIntMap.newBuilder

	protected[this] override def className :String = "DenseIntMap"
	private def writeReplace :AnyRef = new DefaultSerializationProxy(DenseIntMap.factory[V], this)
}


@SerialVersionUID(Ver)
private case object DenseIntMap {
	def from[V](items :IterableOnce[(Int, V)]) :DenseIntMap[V] = items match {
		case map :DenseIntMap[V]       => map
		case _ if items.knownSize == 0 => Empty
		case _                         =>
			val it = items.iterator.map { case (k, v) => (k ^ Int.MinValue, v) }
			new DenseIntMap(IntMap.from(it))
	}

	def newBuilder[V] :Builder[(Int, V), DenseIntMap[V]] =
		IntMap.newBuilder[V].mapInput((kv :(Int, V)) => (kv._1 ^ Int.MinValue, kv._2)).mapResult(new DenseIntMap(_))

	def factory[V] :Factory[(Int, V), DenseIntMap[V]] =
		new Factory[(Int, V), DenseIntMap[V]] {
			override def fromSpecific(it :IterableOnce[(Int, V)]) :DenseIntMap[V] = from(it)
			override def newBuilder :Builder[(Int, V), DenseIntMap[V]] = DenseIntMap.newBuilder
		}

	def empty[V] :DenseIntMap[V] = Empty

	private[this] val Empty = new DenseIntMap(IntMap.empty)
}



/*
@SerialVersionUID(Ver)
private class DenseIntMap[+V](data :Vector[V], min :Int, max :Int, override val size :Int)
	extends AbstractMap[Int, V] with SortedMap[Int, V]
	   with StrictOptimizedIterableOps[(Int, V), immutable.Iterable, DenseIntMap[V]]
{
	override def ordering :Ordering[Int] = Ordering.Int

	override def apply(key :Int) :V = {
		val v = try data(key - min) catch {
			case _ :IndexOutOfBoundsException => noSuch_!(key.toString)
		}
		if (v.asInstanceOf[AnyRef] eq DenseIntMap.Missing)
			noSuch_!(key.toString)
		v
	}
	override def get(key :Int) :Option[V] =
		if (key < min | key > max)
			None
		else (data(key - min) :Any) match {
			case DenseIntMap.Missing => noSuch_!(key.toString)
			case v                   => Some(v.asInstanceOf[V])
		}

	override def updated[U >: V](key :Int, value :U) :SortedMap[Int, U] =
		if (key >= min & key <= max) {
			val newSize = if (data(key - min).asInstanceOf[AnyRef] eq DenseIntMap.Missing) size + 1 else size
			new DenseIntMap(data.updated(key - min, value), min, max, newSize)
		} else if (key == max + 1)
			new DenseIntMap(data :+ value, min, key, size + 1)
		else if (key == min - 1)
			new DenseIntMap(value +: data, key, max, size + 1)
		else if (key > max) {
			val b = Vector.newBuilder[U]
			b sizeHint key + 1 - min
			b ++= data
			var i = max + 1


			while (i < key) {
				b += DenseIntMap.Missing[U]
				i += 1
			}
			b += value
			new DenseIntMap(b.result(), min, key, size + 1)
		} else { //key < min
			val b = Vector.newBuilder[U]
			b sizeHint max + 1 - key
			b += value
			var i = key + 1
			while (i < min) {
				b += DenseIntMap.Missing[U]
				i += 1
			}
			b ++= data
			new DenseIntMap(b.result(), key, max, size + 1)
		}

	override def removed(key :Int) :SortedMap[Int, V] =
		if (key < min | key > max)
			this
		else if (data(key - min).asInstanceOf[AnyRef] eq DenseIntMap.Missing)
			this
		else if (key == min) {
			val newData = data.dropWhile(_.asInstanceOf[AnyRef] eq DenseIntMap.Missing)
			new DenseIntMap(newData, min + data.length - newData.length, max, size - 1)
		} else if (key == max) {
			val newData = data.dropRightWhile(_.asInstanceOf[AnyRef] eq DenseIntMap.Missing)
			new DenseIntMap(newData, min, max - (data.length - newData.length), size - 1)
		} else
			new DenseIntMap(data.updated(key - min, DenseIntMap.Missing[V]), min, max, size - 1)

	override def iterator :Iterator[(Int, V)] = data.iterator.collectWithIndex {
		case (elem, idx) if elem.asInstanceOf[AnyRef] ne DenseIntMap.Missing => (min + idx, elem)
	}
	override def keysIterator :Iterator[Int] = data.iterator.collectWithIndex {
		case (elem, idx) if elem.asInstanceOf[AnyRef] ne DenseIntMap.Missing => min + idx
	}
	override def iteratorFrom(start :Int) :Iterator[(Int, V)] = iterator.dropWhile(_._1 < start)
	override def keysIteratorFrom(start :Int) :Iterator[Int] = keysIterator.dropWhile(_ < start)

	override def rangeImpl(from :Option[Int], until :Option[Int]) :SortedMap[Int, V] = {
		//fixme: this should be a view
		var it = iterator
		from match {
			case Some(v) => it = it.dropWhile(_._1 < v)
			case _ =>
		}
		until match {
			case Some(v) => it = it.takeWhile(_._1 < v)
			case _ =>
		}
		DenseIntMap.from(it)
	}

	protected override def fromSpecific(coll :IterableOnce[(Int, V @uncheckedVariance)]) :DenseIntMap[V] =
		DenseIntMap.from(coll)

	protected override def newSpecificBuilder :Builder[(Int, V @uncheckedVariance), DenseIntMap[V]] =
		DenseIntMap.newBuilder[V]

	protected[this] override def className = "DenseIntMap"
}


@SerialVersionUID(Ver)
case object DenseIntMap {
	def from[V](items :IterableOnce[(Int, V)]) :DenseIntMap[V] = items match {
		case map :DenseIntMap[V]                                    => map
		case _ if items.knownSize == 0                              => Empty
		case map :SortedMap[Int, V] if map.ordering == Ordering.Int => from(map)
//		case seq :collection.IndexedSeqOps[V, Any1, _]              =>
		case view :View[_] if items.knownSize == -1                 => from(items.iterator)
		case _ if items.toBasicOps.isEmpty                          => Empty
		case _                                                      =>
			val sorted = items.toBasicOps.toArray[(Int, Any)].asInstanceOf[Array[(Int, V)]]
			sorted.sortInPlaceBy(_._1)
			val min = sorted(0)._1
			val max = sorted(sorted.length - 1)._1
			fromSorted(sorted.iterator, min, max)
	}
	private def from[V](map :SortedMap[Int, V]) :DenseIntMap[V] =
		if (map.isEmpty) //
			Empty
		else {
			val it     = map.iterator
			val last   = map.last._1
			val offset = map.head._1
			fromSorted(it, offset, last)
		}
	private def fromSorted[V](it :Iterator[(Int, V)], min :Int, max :Int) :DenseIntMap[V] = {
		val size = max - min + 1
		var i    = 0
		val plug = Missing.asInstanceOf[V]
		val b    = Vector.newBuilder[V]
		b sizeHint size
		while (it.hasNext) {
			val (j, elem) = it.next()
			while (i + min < j) {
				b += plug
				i += 1
			}
			b += elem
			i += 1
		}
		new DenseIntMap(b.result(), min, max, size)
	}


	def empty[V] :DenseIntMap[V] = Empty

	private[this] val Empty = new DenseIntMap[Nothing](0, new Array[Nothing](0), 0)

	private case object Missing {
		@inline def apply[V] :V = this.asInstanceOf[V]
	}


	def newBuilder[V] :Builder[(Int, V), DenseIntMap[V]] = new DenseIntMapBuilder[V]

	private final class DenseIntMapBuilder[V]
		extends ReusableBuilder[(Int, V), DenseIntMap[V]] with BaseBuilder[(Int, V), DenseIntMap[V]]
	{
		private[this] var input :Array[(Int, V)] = _
		private[this] var capacity = 0
		private[this] var size = 0

		override def knownSize = size

		override def sizeHint(size :Int) :Unit =
			if (size > capacity & size > MinBuilderArraySize) {
				if (capacity == 0) {
					input = new Array[(Int, Any)](size).asInstanceOf[Array[(Int, V)]]
					capacity = size
				} else {
					capacity = math.max(math.min(Int.MaxValue >> 1, capacity) << 1, size)
					input = Array.copyOfRange(input, 0, size, capacity)
				}
			}

		override def clear() :Unit = {
			input    = null
			capacity = 0
			size     = 0
		}
		override def result() =
			if (size == 0) {
				clear()
				Empty
			} else {
				val min = input(0)._1
				val max = input(size - 1)._1
				input.sortInPlaceBy(0, size)(_._1)
				val it = ArrayIterator(input, 0, size)
				val res = fromSorted(it, min, max)
				clear()
				res
			}

		override def addOne(elem :(Int, V)) = {
			if (size == capacity)
				if (capacity == 0) {
					input = new Array[(Int, Any)](MinBuilderArraySize).asInstanceOf[Array[(Int, V)]]
					capacity = MinBuilderArraySize
				} else {
					capacity <<= 1
					input = Array.copyOf(input, capacity)
				}
			input(size) = elem
			size += 1
			this
		}
	}

	private final val MinBuilderArraySize = 16
}
*/
