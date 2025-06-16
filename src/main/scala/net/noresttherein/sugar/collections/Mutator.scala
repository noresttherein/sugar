package net.noresttherein.sugar.collections

import scala.Specializable.AllNumeric
import scala.collection.mutable.{Buffer, IndexedBuffer}
import scala.collection.{AbstractIterator, BufferedIterator, BuildFrom, Factory, IterableOnceOps, mutable}

import net.noresttherein.sugar.JavaTypes.JStringBuilder
import net.noresttherein.sugar.arrays.MutableArray
import net.noresttherein.sugar.casting.castTypeParamMethods
import net.noresttherein.sugar.exceptions.{noSuch_!, unsupported_!}
import net.noresttherein.sugar.typist.kinds.Any1
import net.noresttherein.sugar.vars.Opt.One
import net.noresttherein.sugar.vars.{OptionalInOut, InOut, IndexedSeqLens, Opt}




//object MutatorFactories {
//	implicit def mutatorFactory[V](factory :Mutator.type) :Factory[V, Mutator[V]] =
//		Mutator.factory
//}
//private[collections] sealed abstract class MutatorFactories

//todo: think how to make `seq to Mutator` work
@SerialVersionUID(Ver)
case object Mutator /*extends MutatorFactories */{
//	def from[K, V](source :mutable.Map[K, V]) :MapMutator[K, V] = ???
//	def from[E](source :mutable.Set[E])     :Mutator[E] = ???
	def from[E](source :MutableArray[E]) :Mutator[E] = ???
	def from[E](source :StringBuilder) :Mutator[E] = ???
	def from[E](source :JStringBuilder) :Mutator[E] = ???
	def from[E](source :mutable.IndexedSeq[E]) :Mutator[E] = ???
	def from[E](source :mutable.SortedSet[E]) :Mutator[E] = ???
	def from[K, V](source :mutable.SortedMap[K, V]) :MapMutator[K, V] = ???

	def empty[E] :Mutator[E] = EmptyMutator.asInstanceOf[Mutator[E]]

	def delay[E](mutator: => Mutator[E]) :Mutator[E] = new LazyMutator[E](mutator)
//	def factory[E] :Factory[E, Mutator[E]]

	implicit def buildFromInOutSet[E]     :BuildFrom[mutable.Set[E], E, Mutator[E]] = ???
//	implicit def buildFromInOutSortedSet[E] :BuildFrom[mutable.SortedSet[E], E, Mutator[E]] = ???
	implicit def buildFromInOutMap[K, V]  :BuildFrom[mutable.Map[K, V], (K, V), MapMutator[K, V]] = ???
	implicit def buildFromInOutSeq[E]     :BuildFrom[mutable.Seq[E], E, Mutator[E]] = ???
	implicit def buildFromInOutArray[E]   :BuildFrom[MutableArray[E], E, Mutator[E]] = ???
	implicit def buildFromStringBuilder[E]  :BuildFrom[StringBuilder, E, Mutator[E]] = ???
	implicit def buildFromJStringBuilder[E] :BuildFrom[JStringBuilder, E, Mutator[E]] = ???

	private[this] val EmptyMutator = new Mutator[Nothing] with Serializable {
		override def head :Nothing = noSuch_!("Mutator.empty.head")
		override def head_=(value :Nothing) :Unit = noSuch_!("Mutator.empty.head")
		override def access :InOut[Nothing] = noSuch_!("Mutator.empty.lens")
		override def next() :Nothing = noSuch_!("Mutator.empty.head")
		override def hasNext :Boolean = false
	}
}




/** An iterator over mutable collections, allowing to insert, remove and update elements.
  * Mutators never throw `ConcurrentModificationException`.
  * @note unless the underlying collection is ordered, updating an element may cause the mutator to point
  *       to other, unrelated elements, and there are no guarantees that inserted elements will happen
  *       at the mutation point.
  * @define Coll `Mutator`
  * @define coll mutator
  * @author Marcin Mościcki
  */ //todo: Bimutator extends Mutator with Biterator
trait Mutator[E] extends BufferedIterator[E] with IterableOnceOps[E, Iterator, Mutator[E]] { self =>
	/** Updates the value of the element this $coll is pointing to.
	  * If the underlying collection is not a `Seq` or a similar type allowing for arbitrary element order,
	  * the update has the semantics of
	  * [[net.noresttherein.sugar.collections.Mutator.remove remove]]`().`[[net.noresttherein.sugar.collections.Mutator.insert insert]]`(elem)`,
	  * and may potentially cause reordering of the underlying collection. For collections which list elements
	  * in the order derived from the elements themselves (such as `SortedSet`
	  * or [[net.noresttherein.sugar.collections.StringMap StringMap]]), the $coll will point to the element directly
	  * following the element which preceded `this.head` before the update, or the start of the collection
	  * if the $coll pointed at the first element. That is, the mutator is tied to actual elements,
	  * not to an absolute position in the collection (unless it has semantics of `Seq`).
	  */ //todo: move the iterator so that this.head = value when the method returns.
	def head_=(value :E) :Unit

	/** Represents the next element of the $coll as a mutable variable which can be used to update
	  * the underlying collection by replacing the element to which this $coll currently points.
	  * Advancing the $coll has no effect on the returned accessor.
	  */
	def access :InOut[E]

	/** Returns an [[net.noresttherein.sugar.collections.Mutator.access accessor]] for the next element of this $coll
	  * and advances it past that element.
	  */
	def accessNext() :InOut[E] = { val res = access; skip(); res }

	def replace(value :E) :this.type = remove().insert(value)

	/** Swaps the next element of the $mutator in the underlying collection for the given value, returning itself.
	  * @return `this.head = value; this`.
	  */ //todo: make
	def update(value :E) :this.type = { head = value; this }

	/** Updates the next element and advances the mutator.
	  * @return `this.head = value; skip()`
	  */
	def updateAndSkip(value :E) :this.type = { head = value; skip() }

	/** Returns [[net.noresttherein.sugar.collections.Mutator.updateAndSkip updateAndSkip]]`(value)`. */
	@inline final def >>=(value :E) :this.type = updateAndSkip(value)

	/** Removes the element the $coll is pointing to from the underlying collection.
	  * If the collection is not ordered (either according to an `Ordering`, or arbitrarily, like `Buffer`),
	  * the order of elements may change and the position of this $coll is unspecified:
	  * the listed elements may both include this $coll has already iterated over, and exclude elements
	  * which would be listed by it if the mutation did not happen.
	  * @note not all implementations support this method.
	  */
	@throws[UnsupportedOperationException]("If the underlying collection does not support adding and removing elements.")
	def remove() :this.type = unsupported_!(toString + ".remove()")

	/** Inserts the given element to the underlying collection. If the latter is a `Seq`, an `Array`,
	  * or another stream with an arbitrary element order, the element will be added directly in front of the iterator,
	  * before its current [[net.noresttherein.sugar.collections.Mutator.head head]].
	  * @note not all implementations support this method.
	  */
	@throws[UnsupportedOperationException]("If the underlying collection does not support adding and removing elements.")
	def insert(value :E) :this.type = unsupported_!(toString + ".insert()")

	@throws[UnsupportedOperationException]("If the underlying collection does not support adding and removing elements.")
	def insertAndSkip(value :E) :this.type = { insert(value); skip() }

	/** Returns [[net.noresttherein.sugar.collections.Mutator.insertAndSkip insertAndSkip]]`(value)`. */
	@inline final def >>+=(value :E) :this.type = insertAndSkip(value)

	/** Steps over the next element in the iteration order, returning this iterator.
	  * Equivalent to `next(); this`.
	  */ //todo: make skip() abstract, and implement next() with skip() and head.
	def skip() :this.type = { next(); this }

	override def buffered :this.type = this

	override def withFilter(p :E => Boolean) :Mutator[E] = filter(p)

	/** Returns [[net.noresttherein.sugar.collections.Mutator.filter filter]]`(p, true)`. */
	override def filter(p :E => Boolean) :Mutator[E] = filter(p, true)

	/** Returns [[net.noresttherein.sugar.collections.Mutator.filter filter]]`(p, false)`. */
	override def filterNot(p :E => Boolean) :Mutator[E] = filter(p, false)

	protected def filter(p :E => Boolean, take :Boolean) :Mutator[E] = Mutators.filter(this, p, take)

	override def partition(p :E => Boolean) :(Mutator[E], Mutator[E]) = Mutators.partition(this, p)

	override def splitAt(n :Int) :(Mutator[E], Mutator[E]) = Mutators.splitAt(this, n)
	override def slice(from :Int, until :Int) :Mutator[E] = Mutators.slice(this, from, until)
	override def take(n :Int) :Mutator[E] = Mutators.take(this, n)
	override def drop(n :Int) :Mutator[E] = Mutators.drop(this, n)
	override def takeWhile(p :E => Boolean) :Mutator[E] = ???
	override def dropWhile(p :E => Boolean) :Mutator[E] = ???
	override def span(p :E => Boolean) :(Mutator[E], Mutator[E]) = ???

//	override def distinct :Mutator[E] = ???
//	override def distinctBy[A](f :E => A) :Mutator[E] = ???

	override def tapEach[U](f :E => U) :Mutator[E] = ???

	def flatMap[O](f :E => Mutator[O]) :Mutator[O] = ???
//	def flatten[O](implicit ev :E <:< Mutator[O]) :Mutator[O] = ???

	@inline final def ++[U >: E](xs: => Mutator[U]) :Mutator[U] = concat(xs)
	def concat[U >: E](xs: => Mutator[U]) :Mutator[U] = ???

	//todo: we should implement them all as ValMutators/ValIterators, as the generic class can be used
	// for base interfaces.
	override def zipWithIndex :Mutator[(E, Int)] = new Mutator[(E, Int)] {
		private[this] var idx = 0
		override def head_=(value :(E, Int)) :Unit = Mutator.this.head = value._1
		override def head :(E, Int) = (Mutator.this.head, idx)
		override def access :InOut[(E, Int)] = new OptionalInOut[(E, Int)] {
			private[this] val index = idx
			private[this] val lens = Mutator.this.access
			override def value = (lens.value, index)
			override def value_=(newValue :(E, Int)) :Unit = lens.value = newValue._1
			override def isDefinite :Boolean = lens.isDefinite
			override def opt :Opt[(E, Int)] = lens.opt.map((_, index))
		}
		override def hasNext :Boolean = Mutator.this.hasNext
		override def next() :(E, Int) = { idx += 1; (Mutator.this.next(), idx - 1) }

		override def toString :String = Mutator.this.toString + ".zipWithIndex"
	}

	def zip[O](that :Mutator[O]) :Mutator[(E, O)] = new Mutator[(E, O)] {
		override def head :(E, O) = (self.head, that.head)
		override def head_=(value :(E, O)) :Unit = { self.head = value._1; that.head = value._2 }
		override def next() :(E, O) = (self.next(), that.next())
		override def hasNext :Boolean = self.hasNext && that.hasNext
		override def access :InOut[(E, O)] = new OptionalInOut[(E, O)] {
			private[this] val _1 = self.access
			private[this] val _2 = that.access
			override def value_=(newValue :(E, O)) :Unit = {
				_1.value = newValue._1; _2.value = newValue._2
			}
			override def isDefinite :Boolean = _1.isDefinite && _2.isDefinite

			override def opt :Opt[(E, O)] = {
				val opt1 = _1.opt
				val opt2 = _2.opt
				if (opt1.isDefined & opt2.isDefined) One((opt1.get, opt2.get)) else None
			}
		}
		override def toString = self.toString + ".zip(" + that + ")"
	}

	override def duplicate :(Mutator[E], Mutator[E]) = ???

	/** Converts this $coll to a regular iterator containing mutable lenses for the elements of this $coll.
	  * All `InOut` instances will be [[net.noresttherein.sugar.vars.Ref.isDefinite definite]], and contain
	  * elements of this $coll in the same exact order. Updating the value of the variable will have the effect
	  * of calling `this.head = value` for this mutator.
	  */
	def toLensIterator :BufferedIterator[InOut[E]] = new SugaredIterator[InOut[E]] with BufferedIterator[InOut[E]] {
		private[this] var hd :InOut[E] = _
		override def head :InOut[E] = {
			if (hd eq null)
				hd = Mutator.this.access
			hd
		}
		override def hasNext :Boolean = Mutator.this.hasNext
		override def next() :InOut[E] = {
			hd = null; Mutator.this.accessNext()
		}
		override def toString = Mutator.this.toString + ".toLensIterator"
	}

	override def toString :String = "Mutator(" + head + ",...)"
}




abstract class AbstractMutator[E] extends AbstractIterator[E] with Mutator[E]




/** A mutable iterator advancing over a slice of an `IndexedSeq`.
  * @param first    the index in the sequence of the first/next element to return.
  * @param `last++` the index in the sequence delimiting the iterator, that is pointing after the last element
  *                 the iterator should return.
  */
private sealed class IndexedSeqMutator[T] private[collections]
	                                  (seq :mutable.IndexedSeqOps[T, Any1, _ <: AnyRef],
	                                   first :Int, `last++` :Int)
	extends AbstractIndexedIterator[T](first, `last++`) with Mutator[T]
{
	def this(seq :mutable.IndexedSeqOps[T, Any1, _ <: AnyRef], idx :Int) = this(seq, idx, seq.length)
	def this(seq :mutable.IndexedSeqOps[T, Any1, _ <: AnyRef]) = this(seq, 0, seq.length)

	private def underlying = seq
	protected final override def underlyingSize :Int = seq.length

	override def access :InOut[T] =
		if (hasNext) new IndexedSeqLens(seq, index)
		else noSuch_!("Index " + index + " exceeds the upper bound of " + limit + ".")

	override def head :T = seq(index)
	override def head_=(value :T) :Unit =
		if (hasNext)
			seq(index) = value
		else
			unsupported_!("Index " + index + " exceeds the upper bound of " + limit + ".")

	override def next() :T = {
		val idx = index
		val end = limit
		if (idx >= end)
			noSuch_!("Index " + first + " exceeds the upper bound of " + `last++` + ".")
		val res = seq(idx)
		index = idx + 1
		res
	}
	override def skip() :this.type = super[AbstractIndexedIterator].skip()

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :IndexedSeqMutator[_] =>
			(seq eq other.underlying) && index == other.index && limit == other.limit
		case _ => false
	}
	override def hashCode :Int = seq.slice(index, limit).hashCode
	override def clone = new IndexedSeqMutator(seq, first, `last++`)
}


@SerialVersionUID(Ver)
private object IndexedSeqMutator extends ExpandedSliceFactory[mutable.IndexedSeq, IndexedSeqMutator] {
	protected override def totalSizeOf[T](source :mutable.IndexedSeq[T]) :Int = source.length
	protected override def make[T](source :mutable.IndexedSeq[T], from :Int, until :Int) :IndexedSeqMutator[T] =
		new IndexedSeqMutator(source, from, until)
}


private class IndexedBufferMutator[E] private[collections]
                                  (buffer :IndexedBuffer[E],
                                   private[this] var first :Int, private[this] var `last++` :Int)
	extends AbstractIterator[E] with IndexedIterator[E] with Mutator[E]
{
	def this(buffer :IndexedBuffer[E], idx :Int) = this(buffer, idx, buffer.length)
	def this(buffer :IndexedBuffer[E]) = this(buffer, 0, buffer.length)

	@inline private def updateLimit() :Unit = {
		val len = buffer.length
		if (len < `last++`)
			`last++` = len
	}
	final override def index :Int = first
	final override def index_=(value :Int) :Unit = first = value
	final override def limit :Int = { updateLimit(); `last++` }
	final override def limit_=(value :Int) :Unit = `last++` = math.min(buffer.length, value)
	final override def underlyingSize :Int = buffer.length

	final override def hasNext :Boolean = { updateLimit(); first < `last++` }

	override def access :InOut[E] = new IndexedSeqLens(buffer, first)

	override def head :E = {
		updateLimit()
		if (first >= `last++`)
			noSuch_!("Index " + first + " exceeds the upper bound of " + `last++` + ".")
		buffer(first)
	}
	override def head_=(value :E) :Unit = {
		updateLimit()
		if (first >= `last++`)
			noSuch_!("Index " + first + " exceeds the upper bound of " + `last++` + ".")
		buffer(first) = value
	}
	final override def skip() :this.type = { next(); this }

	override def next() :E = {
		val len = buffer.length
		if (len < `last++`)
			`last++` = len
		if (first >= `last++`)
			noSuch_!("Index " + first + " exceeds the upper bound of " + `last++` + ".")
		val res = buffer(first)
		first += 1
		res
	}

}






trait MapMutator[K, V] extends Mutator[(K, V)] with IterableOnceOps[(K, V), Iterator, MapMutator[K, V]] {
	def key :K = head._1
	def key_=(key :K) :Unit = head = (key, head._2)

	def value :V = head._2
	def value_=(value :V) :Unit = head = (head._1, value)

	@throws[UnsupportedOperationException]("If the underlying collection does not support adding and removing elements.")
	def insert(key :K, value :V) :this.type = insert((key, value))

	@throws[UnsupportedOperationException]("If the underlying collection does not support adding and removing elements.")
	def insertAndSkip(key :K, value :V) :this.type = { insert(key, value); skip() }


	/** Returns [[net.noresttherein.sugar.collections.MapMutator.filter filter]]`(p, true)`. */
	override def filter(p :((K, V)) => Boolean) :MapMutator[K, V] = filter(p, true)

	/** Returns [[net.noresttherein.sugar.collections.MapMutator.filter filter]]`(p, false)`. */
	override def filterNot(p :((K, V)) => Boolean) :MapMutator[K, V] = filter(p, false)

	protected override def filter(p :((K, V)) => Boolean, take :Boolean) :MapMutator[K, V] = ???

	override def partition(p :((K, V)) => Boolean) :(MapMutator[K, V], MapMutator[K, V]) = ???


	/** Returns [[net.noresttherein.sugar.collections.MapMutator.filter filter]]`(p, true)`. */
	def filter(p :(K, V) => Boolean) :MapMutator[K, V] = filter(p, true)

	/** Returns [[net.noresttherein.sugar.collections.MapMutator.filter filter]]`(p, false)`. */
	def filterNot(p :(K, V) => Boolean) :MapMutator[K, V] = filter(p, false)

	protected def filter(p :(K, V) => Boolean, take :Boolean) :MapMutator[K, V] =
		filter((entry :(K, V)) => p(entry._1, entry._2), take)

	def partition(p :(K, V) => Boolean) :(MapMutator[K, V], MapMutator[K,V]) =
		partition((entry :(K, V)) => p(entry._1, entry._2))


	/** Returns [[net.noresttherein.sugar.collections.MapMutator.filterKeys filterKeys]]`(p, true)`. */
	def filterKeys(p :K => Boolean) :MapMutator[K, V] = filterKeys(p, true)

	/** Returns [[net.noresttherein.sugar.collections.MapMutator.filterKeys filterKeys]]`(p, false)`. */
	def filterKeysNot(p :K => Boolean) :MapMutator[K, V] = filterKeys(p, false)

	protected def filterKeys(p :K => Boolean, take :Boolean) :MapMutator[K, V] =
		filter(entry => p(entry._1), take)

	def partitionKeys(p :K => Boolean) :(MapMutator[K, V], MapMutator[K, V]) =
		partition(entry => p(entry._1))


	/** Returns [[net.noresttherein.sugar.collections.MapMutator.filterValues(take* filterValues]]`(true)(p)`. */
	def filterValues(p :V => Boolean) :MapMutator[K, V] = filterValues(p, true)

	/** Returns [[net.noresttherein.sugar.collections.MapMutator.filterValues(take* filterValues]]`(false)(p)`. */
	def filterValuesNot(p :V => Boolean) :MapMutator[K, V] = filterValues(p, false)

	protected def filterValues(p :V => Boolean, take :Boolean) :MapMutator[K, V] =
		filter(entry => p(entry._2), take)

	def partitionValues(p :V => Boolean) :(MapMutator[K, V], MapMutator[K, V]) =
		partition(entry => p(entry._2))


	override def splitAt(n :Int) :(MapMutator[K, V], MapMutator[K, V]) = ???
	override def slice(from :Int, until :Int) :MapMutator[K, V] = ???
	override def take(n :Int) :MapMutator[K, V] = ???
	override def drop(n :Int) :MapMutator[K, V] = ???
	override def takeWhile(p :((K, V)) => Boolean) :MapMutator[K, V] = ???
	override def dropWhile(p :((K, V)) => Boolean) :MapMutator[K, V] = ???
	override def span(p :((K, V)) => Boolean) :(MapMutator[K, V], MapMutator[K, V]) = (takeWhile(p), dropWhile(p))

//	override def distinct :Mutator[E] = ???
//	override def distinctBy[A](f :E => A) :Mutator[E] = ???

	override def tapEach[U](f :((K, V)) => U) :MapMutator[K, V] = ???
	def tapEach[U](f :(K, V) => U) :MapMutator[K, V] = tapEach(entry => f(entry._1, entry._2))
	def tapEachKey[U](f :K => U)   :MapMutator[K, V] = tapEach(entry => f(entry._1))
	def tapEachValue[U](f :V => U) :MapMutator[K, V] = tapEach(entry => f(entry._2))

	def flatMap[K1, V1](f :((K, V)) => MapMutator[K1, V1]) :MapMutator[K1, V1] = ???
	def flatMap[K1, V1](f :(K, V) => MapMutator[K1, V1]) :MapMutator[K1, V1] = ???
	def flatMapValues[V1](f :V => MapMutator[K, V1]) :MapMutator[K, V1] =
		flatMap[K, V1]((entry :((K, V))) => f(entry._2))

//	def flatten[K1, V1](implicit ev :E <:< MapMutator[K1, V1]) :MapMutator[K1, V1] = ???

	@inline final def ++[V1 >: V](xs: => MapMutator[K, V1]) :MapMutator[K, V1] = concat(xs)
	def concat[V1 >: V](xs: => MapMutator[K, V1]) :MapMutator[K, V1] = ???

	override def duplicate :(MapMutator[K, V], MapMutator[K, V]) = ???


	def toValueMutator :Mutator[V] = ???
	def toValueIterator :Iterator[InOut[V]] = ???

	override def toString :String = "MapMutator(" + key + "->" + value + ",...)"
}




trait ValMutator[@specialized(AllNumeric) V] extends ValIterator.Buffered[V] with Mutator[V] {
	override def buffered :this.type = this
	override def filter(p :V => Boolean) :ValMutator[V] = filter(p, true)
	override def filterNot(p :V => Boolean) :ValMutator[V] = filter(p, false)
	//todo: filtering
	protected override def filter(p :V => Boolean, take :Boolean) :ValMutator[V] = ???
}




private class LazyMutator[E](lzy: => Mutator[E]) extends AbstractMutator[E] {
	private[this] lazy val evaluated = lzy
	override def access :InOut[E] = lzy.access
	override def head :E = evaluated.head
	override def head_=(value :E) :Unit = evaluated.head = value
	override def next() :E = evaluated.next()
	override def hasNext :Boolean = evaluated.hasNext
	override def toString :String = evaluated.toString
}

