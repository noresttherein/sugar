package net.noresttherein.sugar.collections

import scala.annotation.tailrec
import scala.collection.immutable.{SortedSet, StrictOptimizedSortedSetOps}
import scala.collection.{Factory, SortedIterableFactory, SortedSetFactoryDefaults}
import scala.collection.mutable.{Builder, ReusableBuilder}

import net.noresttherein.sugar.extensions.SeqExtension
import net.noresttherein.sugar.vars.Opt.Got






/** A `SortedSet` backed by a sorted array or `IndexedSeq`.
  * Very fast traversing, lookup and range operations, but modifications are O(n).
  */
trait IndexedSet[E]
	extends SortedSet[E] with StrictOptimizedSortedSetOps[E, IndexedSet, IndexedSet[E]]
	   with SortedSetFactoryDefaults[E, IndexedSet, Set]
{
	override def toSeq :Seq[E] = toIndexedSeq

	override def to[C1](factory :Factory[E, C1]) :C1 = sourceCollectionFactory(factory) match {
		case Got(Seq) | Got(IndexedSeq) => toIndexedSeq.asInstanceOf[C1]
		case _ => super.to(factory)
	}
	override def sortedIterableFactory :SortedIterableFactory[IndexedSet] = IndexedSet
	override def className = "IndexedSet"
}




/** $factoryInfo
  *
  * @define Coll `IndexedSet`
  * @define coll indexed set
  */
@SerialVersionUID(Ver)
object IndexedSet extends SortedIterableFactory[IndexedSet] {
	override def from[E :Ordering](it :IterableOnce[E]) :IndexedSet[E] = it match {
		case empty :Iterable[E] if empty.knownSize == 0 => new EmptyIndexedSet[E]
		case single :Iterable[E] if single.knownSize == 1 => new SingletonIndexedSet[E](single.head)
		case seq :IndexedSeq[E] =>
			if (seq.isSorted) new IndexedSeqSet(seq, 0, seq.length)
			else new ArrayIndexedSet(seq.toArray[Any].asInstanceOf[Array[E]])
		case _ => (newBuilder[E] ++= it).result()
	}

	override def empty[E :Ordering] :IndexedSet[E] = new EmptyIndexedSet[E]

	override def newBuilder[E :Ordering] :Builder[E, IndexedSet[E]] =
		new ReusableBuilder[E, IndexedSet[E]] {
			private[this] val elems = ArrayAsSeq.untagged.newBuilder[E]

			override def sizeHint(size :Int) :Unit = elems.sizeHint(size)

			override def addOne(elem :E) = { elems.addOne(elem); this }
			override def addAll(xs :IterableOnce[E]) = { elems.addAll(xs); this }

			override def clear() :Unit = elems.clear()

			override def result() = {
				val array = elems.result()
				if (array.length == 0)
					new EmptyIndexedSet[E]
				else {
					val sorted = array.sorted
					val res = ArrayAsSeq.untagged.newBuilder[E]
					res.sizeHint(array.length)
					var last = sorted(0)
					res += last
					var i = 1; val end = array.length
					while (i < end) {
						val next = sorted(i)
						if (next != last) {
							res += next
							last = next
						}
						i += 1
					}
					new ArrayIndexedSet(res.result())
				}
			}

		}

	def unsafeWrapSortedArray[E :Ordering](array :Array[E]) :IndexedSet[E] = new ArrayIndexedSet(array)


	private abstract class AbstractIndexedSet[E](start :Int, end :Int)(implicit val ordering :Ordering[E])
		extends IndexedSet[E]
	{
		override def knownSize = end - start
		protected def outerSize :Int
		protected def at(index :Int) :E

		override def head :E =
			if (end <= start) throw new NoSuchElementException("IndexedSet().head")
			else at(start)

		override def last :E =
			if (end <= start) throw new NoSuchElementException("IndexedSet().last")
			else at(end)

		override def headOption :Option[E] = if (end <= start) None else Some(at(start))
		override def lastOption :Option[E] = if (end <= start) None else Some(at(end))

		override def tail :IndexedSet[E] =
			if (end <= start) throw new UnsupportedOperationException("IndexedSet.empty.tail")
			else trustedSlice(start + 1, end)

		override def init :IndexedSet[E] =
			if (end <= start) throw new UnsupportedOperationException("IndexedSet.empty.init")
			else trustedSlice(start, end - 1)

		override def take(n :Int) :IndexedSet[E] =
			if (n < 0) IndexedSet.empty[E]
			else if (n >= knownSize) this
			else trustedSlice(start, start + n)

		override def drop(n :Int) :IndexedSet[E] =
			if (n < 0) this
			else if (n >= knownSize) IndexedSet.empty[E]
			else trustedSlice(start + n, end)

		override def slice(from :Int, until :Int) :IndexedSet[E] =
			if (from <= 0 && until >= knownSize) this
			else if (from >= until | until <= 0 || from >= knownSize) IndexedSet.empty[E]
			else if (from <= 0) trustedSlice(start, start + until)
			else if (until >= knownSize) trustedSlice(start + from, end)
			else trustedSlice(start + from, start + until)

		protected def trustedSlice(start :Int, end :Int) :IndexedSet[E]


		//invariant: (array(from - 1) < x) && array(to) >= x
		protected def search(x :E, from :Int = start, to :Int = end) :Int =
			if (from <= to)
				from
			else {
				val middle = (from + to) >> 1
				ordering.compare(x, at(middle)) match {
					case 1 => search(x, middle + 1, to)
					case _ => search(x, from, middle)
				}
			}

		override def incl(elem :E) :IndexedSet[E] = {
			val i = search(elem)
			if (i < end && at(i) == elem)
				this
			else if (start == end) //important to know in the following cases that this.nonEmpty
				new SingletonIndexedSet[E](elem)
			else if (i == end && end < outerSize && at(i) == elem)
				trustedSlice(start, end + 1)
			else if (i == start && start > 0 && at(start - 1) == elem)
				trustedSlice(start - 1, end)
			else {
				val array = new Array[Any](end - start + 1).asInstanceOf[Array[E]]
				trustedCopyToArray(start, array, 0, i - start)
				array(i) = elem
				trustedCopyToArray(i, array, i - start + 1, end - i)
				new ArrayIndexedSet(array)
			}
		}
		override def excl(elem :E) :IndexedSet[E] = {
			val i = search(elem)
			if (i == end || at(i) != elem)
				this
			else if (start == end - 1)
				IndexedSet.empty
			else if (i == end - 1)
				trustedSlice(start, end - 1)
			else if (i == start)
				trustedSlice(start + 1, end)
			else {
				val array = new Array[Any](end - start - 1).asInstanceOf[Array[E]]
				trustedCopyToArray(start, array, 0, i - start)
				trustedCopyToArray(i + 1, array, i - start, end - i - 1)
				new ArrayIndexedSet(array)
			}
		}

		override def contains(elem :E) :Boolean = {
			val i = search(elem)
			i < end && at(i) == elem
		}

		override def rangeImpl(from :Option[E], until :Option[E]) :IndexedSet[E] =
			from match {
				case Some(a) =>
					val i = search(a)
					until match {
						case _ if i == end => IndexedSet.empty
						case Some(b) =>
							val j = search(b)
							if (j <= i)
								IndexedSet.empty
							else if (j == end && i == start)
								this
							else
								trustedSlice(i, j)
						case _ if i == start =>
							this
						case _ =>
							trustedSlice(i, end)
					}
				case _ => until match {
					case Some(b) =>
						val j = search(b)
						if (j == end)
							this
						else
							trustedSlice(start, j)
					case _ =>
						this
				}
			}

		override def copyToArray[U >: E](xs :Array[U], start :Int, len :Int) :Int =
			if (len <= 0 | start > xs.length | this.start == end)
				0
			else {
				//Underflows, but only if copying will throw an exception, so it doesn't matter
				val copied = Math.min(end - this.start, Math.min(len, xs.length - Math.max(start, 0)))
				trustedCopyToArray(this.start, xs, start, copied)
				copied
			}
		protected def trustedCopyToArray[U >: E](from :Int, xs :Array[U], start :Int, len :Int) :Unit
	}


	@SerialVersionUID(Ver)
	private class ArrayIndexedSet[E](elems :Array[E], start :Int, end :Int)(implicit override val ordering :Ordering[E])
		extends AbstractIndexedSet[E](start, end)
	{
		def this(elems :Array[E])(implicit ordering :Ordering[E]) = this(elems, 0, elems.length)

		assert(end >= start & start >= 0 & start <= elems.length & end <= elems.length,
			"ArrayIndexedSet([" + elems.length + "], " + start + ", " + end + ")"
		)
		protected override def outerSize = elems.length
		protected override def at(index :Int) = elems(index)

		protected override def trustedSlice(start :Int, end :Int) :IndexedSet[E] =
			new ArrayIndexedSet(elems, start, end)

		protected override def trustedCopyToArray[U >: E](from :Int, xs :Array[U], start :Int, len :Int) :Unit =
			Array.copy(elems, from, xs, start, len)

		//invariant: (array(from - 1) < x) && array(to) >= x
		@tailrec final override def search(x :E, from :Int = start, to :Int = end) :Int =
			if (from <= to)
				from
			else {
				val middle = (from + to) >> 1
				ordering.compare(x, elems(middle)) match {
					case 1 => search(x, middle + 1, to)
					case _ => search(x, from, middle)
				}
			}

		override def iteratorFrom(start :E) :Iterator[E] = ArrayIterator(elems, search(start), end)
		override def iterator :Iterator[E] = ArrayIterator(elems, start, end)

		override def toIndexedSeq :IndexedSeq[E] = PassedArrayInternals.wrap(elems, start, end)

		@inline final override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
			if (len <= 0 | start > xs.length | elems.length == 0 || start < 0 && len != 0 && xs.length != 0)
				0
			else {
				//this overflows, but only if Array.copy throws an exception, so it doesn't matter
				val copied = start + Math.min(elems.length, Math.min(xs.length - start, len))
				Array.copy(elems, 0, xs, start, copied)
				copied
			}
	}


	@SerialVersionUID(Ver)
	private final class IndexedSeqSet[E](override val toIndexedSeq :IndexedSeq[E], start :Int, end :Int)
	                                    (implicit override val ordering :Ordering[E])
		extends AbstractIndexedSet[E](start, end)
	{
		protected override def outerSize :Int = toIndexedSeq.length
		protected override def at(index :Int) :E = toIndexedSeq(index)

		protected override def trustedSlice(start :Int, end :Int) :IndexedSet[E] =
			new IndexedSeqSet(toIndexedSeq, start, end)

		protected override def trustedCopyToArray[U >: E](from :Int, xs :Array[U], start :Int, len :Int) :Unit =
			if (from == 0)
				toIndexedSeq.copyToArray(xs, start, len)
			else
				toIndexedSeq.slice(from, from + len).copyToArray(xs, start, len)

		override def iteratorFrom(start :E) :Iterator[E] = IndexedSeqIterator(toIndexedSeq, search(start), end)
		override def iterator :Iterator[E] = IndexedSeqIterator(toIndexedSeq, start, end)
	}


	@SerialVersionUID(Ver)
	private class EmptyIndexedSet[E](implicit override val ordering :Ordering[E]) extends IndexedSet[E] {
		override def knownSize = 0
		override def iterator :Iterator[E] = Iterator.empty
		override def iteratorFrom(start :E) :Iterator[E] = Iterator.empty
		override def incl(elem :E) :IndexedSet[E] = new SingletonIndexedSet[E](elem)
		override def excl(elem :E) :IndexedSet[E] = this
		override def contains(elem :E) :Boolean = false
		override def rangeImpl(from :Option[E], until :Option[E]) :IndexedSet[E] = this
		override def toIndexedSeq :IndexedSeq[E] = IndexedSeq.empty
	}


	@SerialVersionUID(Ver)
	private class SingletonIndexedSet[E](override val head :E)(implicit override val ordering :Ordering[E])
		extends IndexedSet[E]
	{
		override def knownSize = 1
		override def last = head
		override def iterator :Iterator[E] = Iterator.single(head)
		override def iteratorFrom(start :E) :Iterator[E] =
			if (ordering.compare(start, head) <= 0) Iterator.single(head)
			else Iterator.empty

		override def incl(elem :E) :IndexedSet[E] = ordering.compare(elem, head) match {
			case 0 => this
			case -1 =>
				val array = new Array[AnyRef](2).asInstanceOf[Array[E]]
				array(0) = elem
				array(1) = head
				new ArrayIndexedSet(array)
			case _ =>
				val array = new Array[AnyRef](2).asInstanceOf[Array[E]]
				array(0) = head
				array(1) = elem
				new ArrayIndexedSet(array)
		}
		override def excl(elem :E) :IndexedSet[E] = if (elem == head) IndexedSet.empty else this

		override def contains(elem :E) :Boolean = head == elem

		override def rangeImpl(from :Option[E], until :Option[E]) :IndexedSet[E] =
			from match {
				case Some(lo) if ordering.compare(lo, head) > 0 => IndexedSet.empty
				case _ => until match {
					case Some(hi) if ordering.compare(hi, head) <= 0 => IndexedSet.empty
					case _ => this
				}
			}

		override def toIndexedSeq :IndexedSeq[E] = Vector.empty[E] :+ head
	}

}
