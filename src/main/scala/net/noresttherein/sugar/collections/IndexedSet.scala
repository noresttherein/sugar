package net.noresttherein.sugar.collections

import java.lang.{Math => math}

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.{AbstractSet, SortedSet, SortedSetOps, StrictOptimizedSortedSetOps}
import scala.collection.{Factory, SortedIterableFactory, SortedSetFactoryDefaults}
import scala.collection.mutable.{ArrayBuffer, Builder, ReusableBuilder}

import net.noresttherein.sugar.arrays.{ArrayIterator, ArrayLike, ErasedArray, IRefArray}
import net.noresttherein.sugar.arrays.extensions.ArrayExtension
import net.noresttherein.sugar.collections.CompanionFactory.sourceCollectionFactory
import net.noresttherein.sugar.collections.IndexedIterable
import net.noresttherein.sugar.collections.IndexedSet.{ArrayIndexedSet, IndexedSeqSet}
import net.noresttherein.sugar.collections.extensions.{IterableOnceExtension, IteratorExtension, IterableExtension, SeqExtension}
import net.noresttherein.sugar.exceptions.outOfBounds_!
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.Yes






/** Set operations (union, intersection, difference) on sorted set which assume that
  * [[scala.collection.SortedSet.ordering ordering]] on both this instance,
  * and any [[scala.collection.SortedSet SortedSet]] given as the argument,
  * is consistent with equals, that is `ordering.compare(a, b) == 0` ''iff'' `a == b`.
  * If both sets in an operation share the same `ordering`, then the lists are merged
  * and a new set is created from an already sorted and unique list with
  * [[net.noresttherein.sugar.collections.ConsistentOptimizedSortedSetOps.fromSorted fromSorted]].
  * Note that default `equals` implementation on `SortedSet` already makes the assumption of consistent ordering
  * and implements a similar optimization for sorted set arguments.
  */
trait ConsistentOptimizedSortedSetOps[E, +CC[X] <: SortedSet[X], +C <: SortedSetOps[E, CC, C]]
	extends StrictOptimizedSortedSetOps[E, CC, C]
{
	override def intersect(that :collection.Set[E]) :C = that match {
		case _ if isEmpty || that.isEmpty => empty
		case same :SortedSet[E] if same.ordering == ordering =>
			val ord = ordering
			val res = new ArrayBuffer[E](math.max(knownSize, same.knownSize))
			val i   = iterator
			val j   = same.iterator
			var a   = i.nextOpt()
			var b   = j.nextOpt()
			while (a.isDefined && b.isDefined)
				ord.compare(a.get, b.get) match {
					case -1 => a = i.nextOpt()
					case  1 => b = j.nextOpt()
					case  _ =>
						res += a.get
						a = i.nextOpt()
						b = j.nextOpt()
				}
			fromSorted(res)
		case _ => super.intersect(that)
	}

	override def concat(that :IterableOnce[E]) :C = that match {
		case _ if isEmpty             => fromSpecific(that)
		case _ if that.knownSize == 0 => coll
		case same :SortedSet[E] if same.ordering == ordering =>
			val order = ordering
			val size1 = knownSize
			val size2 = same.knownSize
			val res   =
				if (size1 >= 0 && size2 >= 0 && size1 <= Int.MaxValue - size2)
					new ArrayBuffer[E](size1 + size2)
				else
					new ArrayBuffer[E]
			val i     = iterator
			val j     = that.iterator
			var opt1  = i.nextOpt()
			var opt2  = j.nextOpt()
			while (opt1.isDefined && opt2.isDefined) {
				val a = opt1.get
				val b = opt2.get
				order.compare(a, b) match {
					case -1 => res += a; opt1 = i.nextOpt()
					case  1 => res += b; opt2 = j.nextOpt()
					case  _ => res += a; opt1 = i.nextOpt(); opt2 = j.nextOpt()
				}
			}
			if (opt1.isDefined) res += opt1.get ++= i
			else if (opt2.isDefined) res += opt2.get ++= j
			fromSorted(res)
		case _ => super.concat(that)
	}

	override def diff(that :collection.Set[E]) :C = that match {
		case _ if knownSize == 0 || that.knownSize == 0 => coll
		case same :SortedSet[E] if same.ordering == ordering =>
			val order = ordering
			val res   = new ArrayBuffer[E](size)
			val i     = iterator
			val j     = that.iterator
			var opt1  = i.nextOpt()
			var opt2  = j.nextOpt()
			while (opt1.isDefined && opt2.isDefined) {
				val a = opt1.get
				order.compare(a, opt2.get) match {
					case -1 => res += a; opt1 = i.nextOpt()
					case  1 => opt2 = j.nextOpt()
					case  _ => opt1 = i.nextOpt(); opt2 = j.nextOpt()
				}
			}
			if (opt1.isDefined)
				res += opt1.get ++= i
			fromSorted(res)
		case _ => super.diff(that)
	}

	protected def fromSorted(elems :IterableOnce[E]) :C
}




/** A `SortedSet` backed by a sorted array or `IndexedSeq`.
  * Very fast traversing, lookup and range operations, but modifications are O(n).
  *
  * @define Coll `IndexedSet`
  * @define coll indexed set
  */
trait IndexedSet[E]
	extends SortedSet[E] with SugaredIterable[E]
	   with SugaredIterableOps[E, Set, Set[E]]
	   with ConsistentOptimizedSortedSetOps[E, IndexedSet, IndexedSet[E]]
	   with SortedSetFactoryDefaults[E, IndexedSet, Set] with Serializable
{
	def key(index :Int) :E
	override def toSeq :Seq[E] = toIndexedSeq

	protected override def fromSorted(elems :IterableOnce[E]) :IndexedSet[E] = elems match {
		case ErasedArray.Wrapped(array) =>
			new ArrayIndexedSet[E](array.asInstanceOf[Array[E]])(ordering)
		case ErasedArray.Wrapped.Slice(array, from, until) =>
			new ArrayIndexedSet(array.asInstanceOf[Array[E]], from, until)(ordering)
		case _ =>
			new IndexedSeqSet[E](elems.toBasicOps.toIndexedSeq)(ordering)
	}

	override def to[C1](factory :Factory[E, C1]) :C1 = sourceCollectionFactory(factory) match {
		case Yes(IndexedSet | Set) => this.asInstanceOf[C1]
		case Yes(Seq | IndexedSeq) => toIndexedSeq.asInstanceOf[C1]
		case _ => super.to(factory)
	}
	private[collections] def applyPreferredMaxLength :Int = Int.MaxValue
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
		//we can't unfortunately return it if it is an IndexedSet, as it might use Ordering[_ <: E]
		case set    :IndexedSet[E] if set.ordering == Ordering[E] => set
		case empty  :Iterable[E] if empty.knownSize == 0 => new EmptyIndexedSet[E]
		case single :Iterable[E] if single.knownSize == 1 => new SingletonIndexedSet[E](single.head)
		case set    :SortedSet[E] if set.ordering == Ordering[E] =>
			new ArrayIndexedSet(set.toArray[Any].asInstanceOf[Array[E]])
		case set    :Set[e] =>
			val array = set.toArray[Any].asInstanceOf[Array[E]]
			array.sortInPlace()
			new ArrayIndexedSet[E](array)
		case seq    :IndexedSeq[E] if seq.forallConsecutive(Ordering[E].lt) =>
			new IndexedSeqSet(seq, 0, seq.length)
		case _ =>
			(newBuilder[E] ++= it).result()
	}

	override def empty[E :Ordering] :IndexedSet[E] = new EmptyIndexedSet[E]

	override def newBuilder[E :Ordering] :Builder[E, IndexedSet[E]] =
		new ReusableBuilder[E, IndexedSet[E]] {
			private[this] val elems = ErasedArray.newBuilder[E]

			override def sizeHint(size :Int) :Unit = elems.sizeHint(size)

			override def addOne(elem :E) = { elems.addOne(elem); this }
			override def addAll(xs :IterableOnce[E]) = { elems.addAll(xs); this }

			override def clear() :Unit = elems.clear()

			override def result() = {
				val array = elems.result()
				array.length match {
					case 0 => new EmptyIndexedSet[E]
					case 1 => new SingletonIndexedSet(array(0))
					case len =>
						array.sortInPlace()
						val res = ErasedArray.newBuilder[E]
						res.sizeHint(len)
						var last = array(0)
						res += last
						var i = 1; val end = len
						while (i < end) {
							val next = array(i)
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
		extends AbstractSet[E] with IndexedSet[E] with SlicingOps[E, IndexedSet[E]]
	{
		protected override def emptySlice :IndexedSet[E] = IndexedSet.empty
		override def knownSize = end - start
		protected def outerSize :Int
		protected def at(index :Int) :E

		override def key(index :Int) :E =
			if (index < 0 | index > end - start) outOfBounds_!(index, this)
			else at(index)

		override def head :E =
			if (end <= start) throw new NoSuchElementException("IndexedSet().head")
			else at(start)

		override def last :E =
			if (end <= start) throw new NoSuchElementException("IndexedSet().last")
			else at(end - 1)

		override def headOption :Option[E] = if (end <= start) None else Some(at(start))
		override def lastOption :Option[E] = if (end <= start) None else Some(at(end - 1))

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
				clippedSlice(0, end - start + 1)
			else if (i == start && start > 0 && at(start - 1) == elem)
				clippedSlice(-1, end - start)
			else {
				val array = ErasedArray.ofDim[E](end - start + 1)
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
				init
			else if (i == start)
				tail
			else {
				val array = ErasedArray.ofDim[E](end - start - 1)
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
								clippedSlice(i - start, j - start)
						case _ if i == start =>
							this
						case _ =>
							clippedSlice(i - start, end - start)
					}
				case _ => until match {
					case Some(b) =>
						val j = search(b)
						if (j == end)
							this
						else
							clippedSlice(0, j - start)
					case _ =>
						this
				}
			}

		override def copyToArray[U >: E](xs :Array[U], start :Int, len :Int) :Int =
			if (len <= 0 | start > xs.length | this.start == end)
				0
			else {
				//Underflows, but only if copying will throw an exception, so it doesn't matter
				val copied = math.min(end - this.start, math.min(len, xs.length - math.max(start, 0)))
				trustedCopyToArray(this.start, xs, start, copied)
				copied
			}
		protected def trustedCopyToArray[U >: E](from :Int, xs :Array[U], start :Int, len :Int) :Unit
	}


	@SerialVersionUID(Ver)
	private class ArrayIndexedSet[E](elems :Array[E], start :Int, end :Int)(implicit override val ordering :Ordering[E])
		extends AbstractIndexedSet[E](start, end) with ArraySliceOps[E, Set, IndexedSet[E]]
	{
		def this(elems :Array[E])(implicit ordering :Ordering[E]) = this(elems, 0, elems.length)

		assert(end >= start & start >= 0 & start <= elems.length & end <= elems.length,
			"ArrayIndexedSet([" + elems.length + "], " + start + ", " + end + ")"
		)

		private[sugar] override def unsafeArray :Array[_] = elems
		private[sugar] override def startIndex  :Int = start
		private[sugar] override def isImmutable :Boolean = true

		protected override def outerSize = elems.length
		protected override def at(index :Int) = elems(index)

		protected override def clippedSlice(from :Int, until :Int) :IndexedSet[E] =
			new ArrayIndexedSet(elems, start + from, start + until)

		protected override def trustedCopyToArray[U >: E](from :Int, xs :Array[U], start :Int, len :Int) :Unit =
			ArrayLike.copy(elems, from, xs, start, len)

		//invariant: (array(from - 1) < x) && array(to) >= x
		@tailrec final override def search(x :E, from :Int = start, to :Int = end) :Int =
			if (to <= from)
				from
			else {
				val middle = (from + to) >> 1
				ordering.compare(x, elems(middle)) match {
					case 1 => search(x, middle + 1, to)
					case _ => search(x, from, middle)
				}
			}

		override def iteratorFrom(start :E) :Iterator[E] = ArrayIterator.slice(elems, search(start), end)
//		override def iterator :Iterator[E] = ArrayIterator(elems, start, end)

		override def toIndexedSeq :IndexedSeq[E] = IRefArray.Wrapped.Slice(elems.asInstanceOf[IRefArray[E]], start, end)

		private def writeReplace :Serializable =
			new ArraySerializationProxy[E](new ArrayIndexedSet(_), elems, start, end - start)

	}


	@SerialVersionUID(Ver)
	private final class IndexedSeqSet[E](underlying :IndexedSeq[E], start :Int, end :Int)
	                                    (implicit override val ordering :Ordering[E])
		extends AbstractIndexedSet[E](start, end)
		   with DefaultSerializable
	{
		def this(underlying :IndexedSeq[E])(implicit ordering :Ordering[E]) = this(underlying, 0, underlying.length)

		protected override def outerSize :Int = underlying.length
		protected override def at(index :Int) :E = underlying(index)

		override def toIndexedSeq :IndexedSeq[E] = SeqSlice(underlying, start, end)

		protected override def clippedSlice(from :Int, until :Int) :IndexedSet[E] =
			new IndexedSeqSet(underlying, start + from, start + until)

		protected override def trustedCopyToArray[U >: E](from :Int, xs :Array[U], start :Int, len :Int) :Unit =
			underlying.copyRangeToArray(xs, start, from, len)

		override def iteratorFrom(start :E) :Iterator[E] = new IndexedSeqIterator(underlying, search(start), end)
		override def iterator :Iterator[E] = new IndexedSeqIterator(underlying, start, end)

		override def applyPreferredMaxLength :Int = IndexedIterable.applyPreferredMaxLength(underlying)
	}


	@SerialVersionUID(Ver)
	private class EmptyIndexedSet[E](implicit override val ordering :Ordering[E])
		extends AbstractSet[E] with IndexedSet[E]
		//cannot be EmptyIterableOps because we can't create an empty IndexedSet[X] for any X without Ordering
	{
		override def key(index :Int) :E = outOfBounds_!(index, "IndexedSet()")
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
		extends AbstractSet[E] with IndexedSet[E]
	{
		override def knownSize = 1
		override def last = head
		override def key(index :Int) :E =
			if (index == 0) head else outOfBounds_!(index, this)

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
