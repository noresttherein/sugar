package net.noresttherein.sugar.collections

import scala.annotation.tailrec
import scala.collection.immutable.{SortedSet, StrictOptimizedSortedSetOps}
import scala.collection.{Factory, SortedIterableFactory, SortedSetFactoryDefaults}
import scala.collection.mutable.{Builder, ReusableBuilder}

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
		case empty :Iterable[E] if empty.isEmpty => new EmptyIndexedSet[E]
		case single :Iterable[E] if single.sizeIs == 1 => new SingletonIndexedSet[E](single.head)
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

	}


	@SerialVersionUID(Ver)
	private class ArrayIndexedSet[E](elems :Array[E], start :Int, end :Int)(implicit override val ordering :Ordering[E])
		extends AbstractIndexedSet[E](start, end)
	{
		def this(elems :Array[E])(implicit ordering :Ordering[E]) = this(elems, 0, elems.length)

		assert(end >= start & start >= 0 & start <= elems.length & end <= elems.length,
			"ArrayIndexedSet([" + elems.length + "], " + start + ", " + end + ")"
		)

		override def head :E =
			if (end <= start) throw new NoSuchElementException("IndexedSet.empty.head")
			else elems(start)

		override def last :E =
			if (end <= start) throw new NoSuchElementException("IndexedSet.empty.last")
			else elems(end - 1)

		protected override def trustedSlice(start :Int, end :Int) :IndexedSet[E] =
			new ArrayIndexedSet(elems, start, end)


		//invariant: (array(from - 1) < x) && array(to) >= x
		@tailrec private def search(x :E, from :Int = start, to :Int = end) :Int =
			if (from == to)
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

		override def incl(elem :E) :IndexedSet[E] = {
			val i = search(elem)
			if (i < end && elems(i) == elem)
				this
			else if (start == end) //important to know in the following cases that this.nonEmptyy
				new SingletonIndexedSet[E](elem)
			else if (i == end && end < elems.length && elems(i) == elem)
				new ArrayIndexedSet(elems, start, end + 1)
			else if (i == start && start > 0 && elems(start - 1) == elem)
				new ArrayIndexedSet(elems, start - 1, end)
			else {
				val array = new Array[Any](end - start + 1).asInstanceOf[Array[E]]
				System.arraycopy(elems, 0, array, 0, i - start)
				array(i) = elem
				System.arraycopy(elems, i, array, i + 1, end - i)
				new ArrayIndexedSet(array)
			}
		}
		override def excl(elem :E) :IndexedSet[E] = {
			val i = search(elem)
			if (i == end || elems(i) != elem)
				this
			else if (i == end - 1)
				new ArrayIndexedSet(elems, start, end - 1)
			else if (i == start)
				new ArrayIndexedSet(elems, start + 1, end)
			else {
				val array = new Array[Any](elems.length - 1).asInstanceOf[Array[E]]
				System.arraycopy(elems, 0, array, 0, i)
				System.arraycopy(elems, i + 1, array, i, elems.length - i - 1)
				new ArrayIndexedSet(elems)
			}
		}

		override def contains(elem :E) :Boolean = {
			val i = search(elem)
			i != end && elems(i) == elem
		}

		override def rangeImpl(from :Option[E], until :Option[E]) :IndexedSet[E] =
			from match {
				case Some(a) =>
					val i = search(a)
					until match {
						case _ if i == end => new EmptyIndexedSet[E]
						case Some(b) =>
							val j = search(b)
							if (j == end)
								if (i == start)
									this
								else
									new ArrayIndexedSet(elems, i, j)
							else if (j <= i)
								new EmptyIndexedSet()
							else if (elems(j) == b)
								new ArrayIndexedSet(elems, i, j - 1)
							else
								new ArrayIndexedSet(elems, i, j)
						case _ if i == start && elems(i) == a =>
							this
						case _ =>
							new ArrayIndexedSet(elems, i, end)
					}
				case _ => until match {
					case Some(b) =>
						val j = search(b)
						if (j == end)
							this
						else if (elems(j) == b)
							new ArrayIndexedSet(elems, start, end - 1)
						else
							new ArrayIndexedSet(elems, start, end)
					case _ =>
						this
				}
			}

		override def toIndexedSeq :IndexedSeq[E] = PassedArrayInternals.wrap(elems, start, end)
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
