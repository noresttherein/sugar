package net.noresttherein.sugar.collections

import scala.collection.generic.DefaultSerializable
import scala.collection.{IterableFactory, IterableOps, SeqFactory, StrictOptimizedIterableOps, StrictOptimizedSeqFactory, StrictOptimizedSetOps, mutable}
import scala.collection.mutable.{Builder, Growable, GrowableBuilder, Shrinkable}

import net.noresttherein.sugar.arrays.{ArrayIterator, CyclicArrayIterator, MutableArrayExtension, arraycopy}
import net.noresttherein.sugar.casting.castTypeParamMethods
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.exceptions.outOfBounds_!
import net.noresttherein.sugar.illegal_!




trait CappedIterableFactory[+CC[X] <: CappedIterable[X]] extends IterableFactory[CC] {
	override def from[E](source :IterableOnce[E]) :CC[E] = source.knownSize match {
		case  0 => empty
		case -1 => (newBuilder[E] ++= source).result()
		case  n => empty[E](n) ++= source
	}
	override def empty[E] :CC[E] = empty[E](0)

	def empty[E](max :Int) :CC[E]
	def newBuilder[E](max :Int) :Builder[E, CC[E]] = new GrowableBuilder[E, CC[E]](empty(max))

	def max(cap :Int) :IterableFactory[CC]
}

object CappedIterableFactory {
	trait Defaults[E, +CC[X] <: CappedIterable[X]] extends IterableOps[E, CC, CC[E]] {
		def cap :Int
		override def empty :CC[E] = cappedFactory.empty(cap)
		protected override def fromSpecific(coll :IterableOnce[E]) :CC[E] = cappedFactory.empty[E](cap) ++= coll
		protected override def newSpecificBuilder :Builder[E, CC[E]] = cappedFactory.newBuilder[E](cap)
		override def iterableFactory :IterableFactory[CC] = cappedFactory.max(cap)
		def cappedFactory :CappedIterableFactory[CC]
	}
}




@SerialVersionUID(Ver)
case object CappedIterable extends CappedIterableFactory[CappedIterable] {
	override def from[E](source :IterableOnce[E]) :CappedIterable[E] = CappedArraySeq.from(source)
	override def empty[E](max :Int) :CappedIterable[E] = CappedArraySeq.empty[E](max)
	override def newBuilder[E](max :Int) :Builder[E, CappedIterable[E]] = CappedArraySeq.newBuilder(max)
	override def newBuilder[A] :Builder[A, CappedIterable[A]] = CappedArraySeq.newBuilder

	override def max(cap :Int) :IterableFactory[CappedIterable] = CappedArraySeq.max(cap)
}

/**
  * @author Marcin Mościcki
  */
trait CappedIterable[E]
	extends mutable.Iterable[E] with IterableOps[E, CappedIterable, CappedIterable[E]]
	   with CappedIterableFactory.Defaults[E, CappedIterable] with Growable[E] with Shrinkable[E]
{
	override def knownSize :Int = -1
	override def cap :Int
	def cappedFactory :CappedIterableFactory[CappedIterable] = CappedIterable
}




trait CappedSeqFactory[+CC[X] <: CappedIterable[X] with collection.SeqOps[X, collection.Seq, collection.Seq[X]]]
	extends CappedIterableFactory[CC] with SeqFactory[CC]
{
	override def max(cap :Int) :SeqFactory[CC]
}

object CappedSeqFactory {
	class Delegate[CC[X] <: CappedIterable[X] with collection.SeqOps[X, collection.Seq, collection.Seq[X]]]
	              (factory :CappedSeqFactory[CC])
		extends SeqFactory.Delegate[CC](factory) with CappedSeqFactory[CC]
	{
		override def from[E](source :IterableOnce[E]) :CC[E] = factory.from(source)
		override def empty[E](max :Int) :CC[E] = factory.empty(max)
		override def newBuilder[E](max :Int) :Builder[E, CC[E]] = factory.newBuilder(max)

		override def max(cap :Int) :SeqFactory[CC] = factory.max(cap)
	}
}




@SerialVersionUID(Ver)
case object CappedSeq extends CappedSeqFactory.Delegate[CappedSeq](CappedArraySeq)

trait CappedSeq[E]
	extends mutable.Seq[E] with mutable.SeqOps[E, CappedSeq, CappedSeq[E]]
	   with CappedIterable[E] with SugaredSeqOps[E, CappedSeq, CappedSeq[E]]
	   with CappedIterableFactory.Defaults[E, CappedSeq]
{
	override def subtractOne(elem :E) :this.type = indexOf(elem) match {
		case -1 => this
		case  i => remove(i); this
	}
	@throws[IndexOutOfBoundsException]("if idx < 0 or idx >= size")
	def remove(idx :Int) :E

	@throws[IllegalArgumentException]("if count < 0")
	@throws[IndexOutOfBoundsException]("if idx < 0 or idx > size - count")
	def remove(idx :Int, count :Int) :Unit

	@inline final def removeHead() :E = remove(0)
	@inline final def removeLast() :E = remove(size - 1)
	@inline final def removeHeadOption() :Option[E] = if (length == 0) None else Some(removeHead())
	@inline final def removeLastOption() :Option[E] = if (isEmpty) None else Some(removeLast())

	override def cappedFactory   :CappedSeqFactory[CappedSeq] = CappedSeq
	override def iterableFactory :SeqFactory[CappedSeq] = cappedFactory.max(cap)
//	protected[this] override def className :String = "CappedSeq[" + cap + "]"
}




@SerialVersionUID(Ver)
case object CappedIndexedSeq extends CappedSeqFactory.Delegate[CappedIndexedSeq](CappedArraySeq)

trait CappedIndexedSeq[E]
	extends mutable.IndexedSeq[E] with mutable.IndexedSeqOps[E, CappedIndexedSeq, CappedIndexedSeq[E]]
	   with CappedSeq[E] with CappedIterableFactory.Defaults[E, CappedIndexedSeq]
{
	override def cappedFactory   :CappedSeqFactory[CappedIndexedSeq] = CappedIndexedSeq
	override def iterableFactory :SeqFactory[CappedIndexedSeq] = cappedFactory.max(cap)
//	protected[this] override def className = "CappedIndexedSeq"
}




@SerialVersionUID(Ver)
case object CappedArraySeq extends CappedSeqFactory[CappedArraySeq] {
	override def empty[A](max :Int) :CappedArraySeq[A] = new CappedArraySeq[A](max)

	override def newBuilder[A] :Builder[A, CappedArraySeq[A]] =
		new ArrayBasedBuilder[Any, A, CappedArraySeq[A]] {
			override def result(array :Array[Any], size :Int) :CappedArraySeq[A] = {
				val buffer = if (size < array.length) array.slice(0, size + 1) else Array.copyOf(array, size + 1)
				new CappedArraySeq(buffer, 0, size)
			}
		}

	override def max(cap :Int) :SeqFactory[CappedArraySeq] = new CappedArraySeqFactory(cap)

	private class CappedArraySeqFactory(max :Int) extends StrictOptimizedSeqFactory[CappedArraySeq] {
		override def from[A](source :IterableOnce[A]) :CappedArraySeq[A] = new CappedArraySeq[A](max) ++= source
		override def empty[A] :CappedArraySeq[A] = new CappedArraySeq[A](max)
		override def newBuilder[A] :Builder[A, CappedArraySeq[A]] = CappedArraySeq.newBuilder(max)
		override def toString = "CappedArraySeq[" + max + "]"
	}

	private final val PlaceholderArray = new Array[Any](1)
}


@SerialVersionUID(Ver) //todo: refactor
final class CappedArraySeq[E] private (buffer :Array[Any], private[this] var start :Int, private[this] var end :Int)
	extends mutable.IndexedSeqOps[E, CappedArraySeq, CappedArraySeq[E]]
	   with CappedIndexedSeq[E] with CappedIterableFactory.Defaults[E, CappedArraySeq] with DefaultSerializable
{
	def this(capacity :Int) = this(new Array[Any](math.max(capacity, 0) + 1), 0, 0)
	def this() = this(CappedArraySeq.PlaceholderArray, 0, 0)

	def cap :Int = buffer.length - 1
	override def length :Int = (if (start <= end) end else buffer.length + end) - start

	override def apply(idx :Int) :E = {
		if (idx < 0 | idx >= length)
			outOfBounds_!(idx, this)
		val cap = buffer.length
		val i = if (idx < cap - start) start + idx else start - cap + idx
		buffer(i).asInstanceOf[E]
	}
	override def update(idx :Int, elem :E) :Unit = {
		if (idx < 0 | idx >= length)
			outOfBounds_!(idx, this)
		val cap = buffer.length
		val i = if (idx < cap - start) start + idx else start - cap + idx
		buffer(i) = elem
	}

	override def addOne(elem :E) :this.type = {
		buffer(end) = elem
		end += 1
		if (end == buffer.length) {
			end = 0
			if (start == 0) {
				buffer(0) = null
				start = 1
			}
		} else if (end == start) {
			buffer(start) = null
			if (start == buffer.length - 1) start = 0
			else start += 1
		}
		this
	}
	override def clear() :Unit = {
		if (start <= end)
			buffer.clear(start, end)
		else {
			buffer.clear(end, buffer.length)
			buffer.clear(0, start)
		}
		start = 0
		end   = 0
	}

	override def remove(idx :Int) :E = {
		val elem = apply(idx)
		remove(idx, 1)
		elem
	}

	override def remove(idx :Int, count :Int) :Unit = {
		val size = length
		if (count < 0)
			illegal_!("Negative count: " + errorString(this) + ".remove(" + idx + ", " + count + ").")
		if (idx < 0 | idx > size - count)
			outOfBounds_!(errorString(this) + ".remove(" + idx + ", " + count + ")")
		if (count > 0) {
			val cap = buffer.length
			val suffixOffset = start + idx + count
			if (start <= end | end == 0) {           //Data is not wrapped.
				arraycopy(buffer, suffixOffset, buffer, start + idx, size - idx - count)
				buffer.clear(start + idx, start + idx + count)
				end -= count
			} else if (idx + count <= cap - start) { //The whole removed fragment resides in the suffix (before wrapping).
				arraycopy(buffer, suffixOffset, buffer, start + idx, cap - suffixOffset)
				if (end < count) {                   //After removal the data is not wrapped anymore.
					arraycopy(buffer, 0, buffer, cap - count, end)
					buffer.clear(0, end)
					end = end - count + cap
					buffer.clear(end, cap)
				} else {
					end -= count
					arraycopy(buffer, 0, buffer, cap - count, count)
					arraycopy(buffer, count, buffer, 0, end)
					buffer.clear(end, end + count)
				}
			} else if (idx < cap) {                  //The removed fragment is wrapped.
				val offset = suffixOffset - cap
				if (cap - count > end - offset) {    //After removal the data is not wrapped anymore.
					arraycopy(buffer, offset, buffer, start + idx, end - offset)
					end = end - count + cap
				} else {
					end -= count
					arraycopy(buffer, offset, buffer, start + idx, cap - start - idx)
					arraycopy(buffer, count, buffer, 0, end - count)
				}
				buffer.clear(end, end + count)
			} else {                                 //The whole removed fragment resides in the prefix (after wrapping).
				val absolute = start - cap + idx
				arraycopy(buffer, absolute + count, buffer, absolute, size - idx - count)
				buffer.clear(end - count, end)
				end -= count
			}
		}
	}

	override def iterator :Iterator[E] = Integer.compare(start, end) match {
		case 0 => Iterator.empty
		case 1 => CyclicArrayIterator(buffer, start, buffer.length + end - start).castParam[E]
		case _ => ArrayIterator(buffer, start, end - start).castParam[E]
	}

	override def cappedFactory   :CappedSeqFactory[CappedArraySeq] = CappedArraySeq
	override def iterableFactory :SeqFactory[CappedArraySeq] = CappedArraySeq.max(cap)

	protected[this] override def className :String = "CappedArraySeq[" + cap + "]"
}




/*@SerialVersionUID(Ver)
case object CappedSet extends CappedIterableFactory[CappedSet] {
	override def empty[E](max :Int) :CappedSet[E] = new Impl(max)

	override def newBuilder[A] :Builder[A, CappedSet[A]] =
		Ranking.appendingBuilder[A] mapResult {
			ranking =>
				val seq = Fingers.from(ranking.toSeq)
				val set = ranking.to(mutable.Set)
				new Impl(seq.length, seq, set)
		}

	override def max(cap :Int) :IterableFactory[CappedSet] =

	private class Impl[E](override val cap :Int, private[this] var seq :Seq[E], set :mutable.Set[E])
		extends CappedSet[E]
	{

	}
}


trait CappedSet[E]
	extends CappedIterable[E] with mutable.Set[E] with mutable.SetOps[E, CappedSet, CappedSet[E]]
	   with CappedIterableFactory.Defaults[E, CappedSet] with DefaultSerializable
{
	override def cappedFactory :CappedIterableFactory[CappedSet] = CappedSet
	protected[this] override def className :String = "CappedSet"
}

*/
