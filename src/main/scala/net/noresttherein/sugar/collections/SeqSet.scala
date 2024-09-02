package net.noresttherein.sugar.collections

import scala.annotation.tailrec
import scala.collection.generic.DefaultSerializable
import scala.collection.{AbstractIterator, IterableFactory, IterableFactoryDefaults, StrictOptimizedSetOps}
import scala.collection.immutable.{AbstractSet, HashMap, SetOps}
import scala.collection.mutable.Builder

import net.noresttherein.sugar.casting.castTypeParamMethods
import net.noresttherein.sugar.collections.VectorSet.{Gap, MinFillRatio, VectorSetIterator}
import net.noresttherein.sugar.collections.extensions.IterableOnceExtension
import net.noresttherein.sugar.exceptions.{noSuch_!, unsupported_!}




/** The API of [[net.noresttherein.sugar.collections.SeqSet SeqSet]] and its subclasses. */
trait SeqSetOps[E, +CC[_], +C <: SeqSetOps[E, CC, C]] extends SetOps[E, CC, C] with SugaredIterableOps[E, CC, C] {
//	def indexOf(elem :E) :Int
//
//	override def contains(elem :E) :Boolean = indexOf(elem) >= 0

	@inline final def :+(elem :E) :C = appended(elem)
	def appended(elem :E) :C = excl(elem).incl(elem)

	@inline final def :++(elems :IterableOnce[E]) :C = appendedAll(elems)
	def appendedAll(elems :IterableOnce[E]) :C = elems.toBasicOps.foldLeft(coll)(_ :+ _)
//	@inline final def +:(elem :E) :C = prepended(elem)
//	def prepended(elem :E) :C

//	def reverseIterator :Iterator[E]
}




/** An immutable `Set` implementation which preserves the order of added elements.
  * The order is irrelevant when comparing two sets, and, in particular, a $Coll will equal any other `Set`
  * with the same elements. However, all methods iterate over elements in the same order in which they were added.
  * Adding an already present element with standard `Set` methods `incl/+` returns the same set;
  * if you wish for the element to be moved to the end of iteration order, use
  * [[net.noresttherein.sugar.collections.SeqSetOps.appended appended]]/[[net.noresttherein.sugar.collections.SeqSetOps.:+ :+]].
  *
  * This makes it similar to [[java.util.LinkedHashSet]].
  * The default implementation is [[net.noresttherein.sugar.collections.VectorSet VectorSet]].
  * @define Coll `SeqSet`
  * @define coll sequential set
  * @author Marcin Mościcki
  */
trait SeqSet[E]
	extends Set[E] with SugaredIterable[E]
	   with SeqSetOps[E, SeqSet, SeqSet[E]] with IterableFactoryDefaults[E, SeqSet]
{
	override def iterableFactory :IterableFactory[SeqSet] = SeqSet
	protected[this] override def className :String = "SeqSet"
}


/** $factoryInfo
  * @define Coll `SeqSet`
  * @define coll sequential set
  */
@SerialVersionUID(Ver)
case object SeqSet extends IterableFactory.Delegate[SeqSet](VectorSet) {
	override def from[E](it :IterableOnce[E]) :SeqSet[E] = it match {
		case set :SeqSet[E] => set
		case _              => VectorSet from it
	}
}




/** The default implementation of [[net.noresttherein.sugar.collections.SeqSet SeqSet]],
  * very similar to [[scala.collection.immutable.VectorMap VectorMap]] from the standard Scala library.
  * Maintains a sequence of added elements, which may contain gaps if an element is removed or moved to the end
  * of the sequence. If the number of gaps reaches a certain threshold, the next removal will rebuild the set.
  *
  * Membership check is `O(1)`, and both removal and adding of an element take amortized constant time
  * if they happen in uniformly random order.
  * @tparam E the type of elements stored in this set.
  * @define Coll `VectorSet`
  * @define coll vector set
  */
final class VectorSet[E] private (items :Vector[Any], index :Map[E, Int])
	extends AbstractSet[E] with StrictOptimizedSetOps[E, VectorSet, VectorSet[E]]
	  with SeqSet[E] with SeqSetOps[E, VectorSet, VectorSet[E]]
      with IterableFactoryDefaults[E, VectorSet] with DefaultSerializable
{
	private def this(unique :Vector[E]) = this(unique, HashMap from unique.view.zipWithIndex)

	def this() = this(Vector.empty, HashMap.empty)

	override def knownSize :Int = index.knownSize
	override def size :Int = index.size

	override def contains(elem :E) :Boolean = index.contains(elem)

	override def incl(elem :E) :VectorSet[E] = index.getOrElse(elem, -1) match {
		case -1 => new VectorSet(items :+ elem, index.updated(elem, items.length))
		case  _ => this
	}

	override def excl(elem :E) :VectorSet[E] = index.getOrElse(elem, -1) match {
		case -1                         => this
		case  _ if index.knownSize == 1 => VectorSet.empty
		case  n if index.knownSize * 100L >= MinFillRatio * items.length =>
			val next = nextIndex(n + 1)
			new VectorSet(items.updated(n, Gap(next - n)), index.removed(elem))
		case  n =>
			val res = VectorSet.newBuilder[E]
			res sizeHint index.knownSize - 1
			res ++= new Iterator[E] {
				private[this] var idx = nextIndex(0)
				override def hasNext :Boolean = idx < n
				override def next() =
					if (idx >= n)
						noSuch_!
					else {
						val res = items(idx).asInstanceOf[E]
						idx = nextIndex(idx + 1)
						res
					}
			}
			val succ = nextIndex(n + 1)
			if (succ < items.length)
				res ++= new Iterator[E] {
					private[this] val end = items.length
					private[this] var idx = succ
					override def hasNext = idx < end
					override def next() = {
						val res = items(idx)
						idx = nextIndex(idx + 1)
						res.asInstanceOf[E]
					}
				}
			res.result()
	}

	override def concat(that :IterableOnce[E]) :VectorSet[E] = that.knownSize match {
		//Covers also n== -1
		case n if n < index.size => that.toBasicOps.foldLeft(this)(_ incl _)
		case _                   => (VectorSet.newBuilder[E] ++= this ++= that).result()
	}
	override def appendedAll(elems :IterableOnce[E]) :VectorSet[E] = elems.knownSize match {
		case n if n < index.size => super.appendedAll(elems)
		case _                   => (VectorSet.appendingBuilder[E] ++= this ++= elems).result()
	}

	override def removedAll(that :IterableOnce[E]) :VectorSet[E] = that.knownSize match {
		case -1 =>
			that.toBasicOps.foldLeft(this)(_ excl _)
		case  n if (index.size - n) * 100L >= items.length * MinFillRatio =>
			that.toBasicOps.foldLeft(this)(_ excl _)
		case  _ =>
			val set = (that :IterableOnce[Any]).toBasicOps.toSet
			val newItems = items.filterNot(set).castParam[E]
			new VectorSet(newItems)
	}


	override def head :E =
		if (index.knownSize == 0) noSuch_!("VectorSet.empty.head")
		else items(nextIndex(0)).asInstanceOf[E]

	override def tail :VectorSet[E] =
		if (index.knownSize == 0) unsupported_!("VectorSet.empty.tail")
		else excl(head)

	override def foreach[U](f :E => U) :Unit = {
		val end = items.length
		var i   = 0
		while (i < end) items(i) match {
			case Gap(next)          => i += next
			case elem :E @unchecked => f(elem); i += 1
		}
	}
	override def foldLeft[A](z :A)(op :(A, E) => A) :A = {
		val end = items.length
		var acc = z
		var i   = 0
		while (i < end) items(i) match {
			case Gap(next)          => i += next
			case elem :E @unchecked => acc = op(acc, elem); i += 1
		}
		acc
	}

	override def iterator :Iterator[E] = knownSize match {
		case 0 => Iterator.empty
		case n => new VectorSetIterator(items, 0, n)
	}

	private def nextIndex(idx :Int) :Int = {
		val size = items.length
		var i    = idx
		while (i < size && (items(i) match {
			case Gap(next) => i += next; true
			case _         => false
		})) {}
		i
	}

	protected override def className = "VectorSet"
	override def iterableFactory :IterableFactory[VectorSet] = VectorSet
}


/** $factoryInfo
  * @define Coll `VectorSet`
  * @define coll vector set
  */
@SerialVersionUID(Ver)
case object VectorSet extends IterableFactory[VectorSet] {
	override def from[A](source :IterableOnce[A]) :VectorSet[A] = {
		def uniqueToIndex(unique :Iterable[A]) =
			unique.foldLeft(Map.empty[A, Int]) { (map, elem) => map.updated(elem, map.size) }
		source match {
			case set :VectorSet[A]          => set
			case _ if source.knownSize == 0 => empty
			case set :Set[A]                => new VectorSet(set.toVector, uniqueToIndex(set))
			case ranking :Ranking[A]        => new VectorSet(ranking.toVector, uniqueToIndex(ranking))
			case _                          => (newBuilder[A] ++= source).result()
		}
	}

	override def empty[A] :VectorSet[A] = Empty.asInstanceOf[VectorSet[A]]

	/** A builder of $Coll which preserves insertion order, but keeps the first added elements in case of duplicates. */
	override def newBuilder[A] :Builder[A, VectorSet[A]] = new VectorSetBuilder[A]

	/** A builder of $Coll which preserves insertion order, and moves an element to the end of the sequence
	  * if it is added again.
	  */
	def appendingBuilder[A] :Builder[A, VectorSet[A]] = new AppendingBuilder[A]

	private[this] val Empty = new VectorSet[Nothing](Vector.empty, HashMap.empty)

	private final val MinFillRatio = 50L

	private case class Gap private (next :Int)

	private object Gap {
		private final val CacheSize = 32
		private[this] val cache = Array.tabulate(CacheSize)(new Gap(_))
		final val One :Gap = cache(1)

		def apply(skip :Int) :Gap = if (skip <= CacheSize) cache(skip) else new Gap(skip)
	}

	private final class VectorSetBuilder[T] extends Builder[T, VectorSet[T]] {
		private[this] var len = 0
		private[this] val seq = Vector.newBuilder[T]
		private[this] var map = HashMap.empty[T, Int]

		override def sizeHint(size :Int) :Unit = seq sizeHint size
		override def knownSize = len

		override def clear() :Unit = { seq.clear(); map = HashMap.empty }

		override def result() = {
			val res = new VectorSet(seq.result(), map)
			clear()
			res
		}

		override def addOne(elem :T) = {
			if (!map.contains(elem)) {
				map = map.updated(elem, len)
				seq += elem
				len += 1
			}
			this
		}
	}

	private final class AppendingBuilder[T] extends Builder[T, VectorSet[T]] {
		private[this] var seq  = Vector.newBuilder[Any]
		private[this] var map  = HashMap.empty[T, Int]
		private[this] var len  = 0
		private[this] var size = 0

		override def sizeHint(size :Int) :Unit = seq sizeHint size
		override def knownSize = size

		override def clear() :Unit = { seq.clear(); map = HashMap.empty }

		override def result() = new VectorSet(seq.result(), map)

		override def addOne(elem :T) = map.getOrElse(elem, -1) match {
			case -1 =>
				map   = map.updated(elem, len)
				seq  += elem
				len  += 1
				size += 1
				this
			case  n if size * 100L < len * MinFillRatio =>
				seq  = Vector.newBuilder[Any] ++= seq.result().updated(n, Gap.One) += elem
				map  = map.updated(elem, len)
				len += 1
				this
			case  _ =>
				val updated = seq.result().view.filterNot(e => e == elem || e.isInstanceOf[Gap]).toVector.castParam[T]
				seq  = Vector.newBuilder[Any] ++= updated += elem
				len  = updated.length + 1
				size = len + 1
				map  = HashMap.from(updated.view.zipWithIndex).updated(elem, len - 1)
				this
		}
	}

	private final class VectorSetIterator[+T](items :IndexedSeq[Any],
	                                          private[this] var idx :Int, private[this] var left :Int)
		extends AbstractIterator[T]
	{
		override def knownSize = math.max(left, 0)
		override def hasNext = left > 0
		override def next() :T = {
			var res :T = null.asInstanceOf[T]
			while (res == null) {
				try
					items(idx) match {
						case Gap(skip)          => idx += skip
						case elem :T @unchecked => idx += 1; left -= 1; res = elem
					}
				catch {
					case e :IndexOutOfBoundsException => noSuch_!("VectorSet.iterator|0|.empty.next", e)
				}
			}
			res
		}

		override def take(n :Int) :Iterator[T] =
			if (n <= 0) Iterator.empty
			else if (n >= left) this
			else { left = n; this }

		override def slice(from :Int, until :Int) :Iterator[T] = take(until).drop(from)

		override def toString = "VectorSet.iterator|" + knownSize + "|"
	}
}
