package net.noresttherein.sugar.collections

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.{AbstractSeq, AbstractSet, IndexedSeqOps, SeqOps, SetOps, StrictOptimizedSeqOps, StrictOptimizedSetOps}
import scala.collection.{IterableFactory, IterableFactoryDefaults, IterableOps, SeqFactory, immutable, mutable}
import scala.collection.mutable.{Builder, Growable, GrowableBuilder, Shrinkable}

import net.noresttherein.sugar.arrays.{ArrayIterator, CyclicArrayIterator, MutableArrayExtension, arraycopy}
import net.noresttherein.sugar.casting.castTypeParamMethods
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.exceptions.outOfBounds_!
import net.noresttherein.sugar.extensions.IteratorExtension
import net.noresttherein.sugar.illegal_!
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




@SerialVersionUID(Ver)
object CappedIterableFactory {
	class Delegate[+CC[x] <: CappedIterable[x]](delegate :CappedIterableFactory[CC]) extends CappedIterableFactory[CC] {
		override def empty[E](max :Int) :CC[E] = delegate.empty(max)
		override def ofMax(cap :Int) :IterableFactory[CC] = delegate.ofMax(cap)
		override def newBuilder[A] :Builder[A, CC[A]] = delegate.newBuilder
		override def newBuilder[E](max :Int) :Builder[E, CC[E]] = delegate.newBuilder[E]
	}

	trait Mutable[+CC[X] <: CappedGrowable[X]] extends CappedIterableFactory[CC] {
		override def from[E](source :IterableOnce[E]) :CC[E] = source.knownSize match {
			case  0 => empty
			case -1 => (newBuilder[E] ++= source).result()
			case  n => empty[E](n) ++= source
		}
		override def newBuilder[E](max :Int) :Builder[E, CC[E]] = new GrowableBuilder[E, CC[E]](empty(max))
		override def ofMax(cap :Int) :IterableFactory[CC] = new CappedFactory(cap)

		protected class CappedFactory(max :Int) extends IterableFactory[CC] {
			override def from[A](source :IterableOnce[A]) :CC[A] = Mutable.this.empty[A](max) ++= source
			override def empty[A] :CC[A] = Mutable.this.empty(max)
			override def newBuilder[A] :Builder[A, CC[A]] = Mutable.this.newBuilder(max)
			override def toString :String = Mutable.this.toString + "[" + max + "]"
		}
	}

	trait Immutable[+CC[X] <: CappedCollection[X] with IterableOps[X, CC, CC[X]]] extends CappedIterableFactory[CC] {
		private[this] val Empty = empty[Nothing](0)

		override def from[E](source :IterableOnce[E]) :CC[E] = source.knownSize match {
			case  0 => empty
			case -1 => (newBuilder[E] ++= source).result()
			case  n => empty[E](n) ++ source
		}

		override def empty[E] :CC[E] = Empty.asInstanceOf[CC[E]]
		override def newBuilder[E](max :Int) :Builder[E, CC[E]] = new AdditiveBuilder[E, CC](empty(max))
		override def ofMax(cap :Int) :IterableFactory[CC] = new CappedFactory(cap)

		protected class CappedFactory(max :Int) extends IterableFactory[CC] {
			override def from[A](source :IterableOnce[A]) :CC[A] = Immutable.this.empty[A](max) concat source
			override def empty[A] :CC[A] = Immutable.this.empty[A](max)
			override def newBuilder[A] :Builder[A, CC[A]] = Immutable.this.newBuilder(max)
			override def toString :String = Immutable.this.toString + "[" + max + "]"
		}
	}
}


/** $factoryInfo
  * $evictionInfo
  * @see [[net.noresttherein.sugar.collections.CappedIterableFactory.ofMax max]]
  * @define factoryInfo  A factory of collections with an upper size bound equal to the size of the created $Coll.
  *                      Serves also as a factory of iterable factories creating ${coll}s with arbitrary size bounds.
  * @define evictionInfo Once the collection reaches its maximum size, existing elements become removed
  *                      to make space for subsequently added elements.
  * @define Coll `CappedIterable`
  * @define coll capped collection
  */ //todo: explain mutable vs immutable
trait CappedIterableFactory[+CC[X] <: CappedIterable[X]] extends IterableFactory[CC] {
	override def from[E](source :IterableOnce[E]) :CC[E] = (newBuilder[E] ++= source).result()

	/** Returns an empty instance with zero capacity for new elements. */
	override def empty[E] :CC[E] = empty[E](0)

	/** An empty $Coll with an upper size bound of `max` elements.
	  * $evictionInfo
	  */
	def empty[E](max :Int) :CC[E]

	/** A builder for a $Coll consisting of at most `max` most recent elements added to the builder. */
	def newBuilder[E](max :Int) :Builder[E, CC[E]] //= new GrowableBuilder[E, CC[E]](empty(max))

	/** A standard `IterableFactory` building $Coll instances with a size limit of `cap`.
	  * If the number of added elements exceeds `cap`, the collection will contain only the last `cap` elements.
	  */
	def ofMax(cap :Int) :IterableFactory[CC]
}




@SerialVersionUID(Ver)
object CappedSeqFactory {
	class Delegate[CC[X] <: CappedIterable[X] with collection.SeqOps[X, collection.Seq, collection.Seq[X]]]
	              (factory :CappedSeqFactory[CC])
		extends SeqFactory.Delegate[CC](factory) with CappedSeqFactory[CC]
	{
//		override def from[E](source :IterableOnce[E]) :CC[E] = factory.from(source)
		override def empty[E](max :Int) :CC[E] = factory.empty(max)
		override def newBuilder[E](max :Int) :Builder[E, CC[E]] = factory.newBuilder(max)
		override def ofMax(cap :Int) :SeqFactory[CC] = factory.ofMax(cap)
	}
}

/** $factoryInfo
  * $evictionInfo
  * @define Coll `CappedSeq`
  * @define coll capped sequence
  * @define evictionInfo Whenever new elements are appended to a $Coll and the resulting $coll's size would exceed
  *                      the upper size bound defined at its creation, leading elements are removed to reduce
  *                      the collection size to the limit. Conversely, when prepending new elements,
  *                      trailing elements are removed if necessary.
  */
trait CappedSeqFactory[+CC[X] <: CappedIterable[X] with collection.SeqOps[X, collection.Seq, collection.Seq[X]]]
	extends CappedIterableFactory[CC] with SeqFactory[CC]
{
	override def ofMax(cap :Int) :SeqFactory[CC]
}






/** $factoryInfo
  * $evictionInfo
  * @define Coll `CappedIterable`
  * @define coll capped collection
  */
@SerialVersionUID(Ver)
case object CappedIterable extends CappedIterableFactory.Delegate[CappedIterable](CappedArrayBuffer) {
	type Mutable[E] = CappedGrowable[E]
	type Immutable[E] = CappedCollection[E]

	trait Defaults[+E, +CC[X] <: CappedIterable[X] with IterableOps[X, CC, CC[X]]]
		extends IterableFactoryDefaults[E, CC] with IterableOps[E, CC, CC[E @uncheckedVariance]]
	{
		def cap :Int
		override def empty :CC[E @uncheckedVariance] = cappedFactory.empty(cap)
//		protected override def fromSpecific(coll :IterableOnce[E @uncheckedVariance]) :CC[E @uncheckedVariance] = ???

		protected override def newSpecificBuilder :Builder[E, CC[E]] @uncheckedVariance =
			cappedFactory.newBuilder[E](cap)

		override def iterableFactory :IterableFactory[CC] = cappedFactory.ofMax(cap)
		def cappedFactory :CappedIterableFactory[CC]
	}
}

/** $Description
  * $evictionInfo
  * @define Description  A mutable $Unbound containing at most [[net.noresttherein.sugar.collections.CappedIterable.cap cap]]
  *                      elements. Whenever a new element is added to a $unbound already containing `cap` elements,
  *                      the oldest element is removed.
  * @define evictionInfo Whenever new elements are appended to a $Coll and the resulting $coll's size would exceed
  *                      the upper size bound defined at its creation, leading elements are removed to reduce
  *                      the collection size to the limit. Conversely, when prepending new elements,
  *                      trailing elements are removed if necessary.
  * @define Coll `CappedIterable`
  * @define coll capped collection
  * @define Unbound `Iterable`
  * @define unbound collection
  */
trait CappedIterable[+E]
	extends Iterable[E] with IterableOps[E, CappedIterable, CappedIterable[E]]
	   with CappedIterable.Defaults[E, CappedIterable]// with Growable[E] with Shrinkable[E]
{
	def cappedFactory :CappedIterableFactory[CappedIterable] = CappedIterable
	protected[this] override def className :String = iterableFactory.toString + "[" + cap + "]"
}




/** $factoryInfo
  * $evictionInfo
  * @define Coll `CappedCollection`
  * @define coll capped immutable collection
  */
@SerialVersionUID(Ver)
case object CappedCollection
	extends CappedIterableFactory.Delegate[CappedCollection](CappedSeq)
	   with CappedIterableFactory.Immutable[CappedCollection]
{
	trait Defaults[+E, +CC[X] <: CappedIterable[X] with IterableOps[X, CC, CC[X]]]
		extends CappedIterable.Defaults[E, CC]
	{
		protected override def fromSpecific(coll :IterableOnce[E @uncheckedVariance]) :CC[E @uncheckedVariance] =
			cappedFactory.empty[E](cap) ++ coll
	}
}

/** $Description
  * $evictionInfo
  * @define Coll    `CappedCollection`
  * @define coll    capped immutable collection
  * @define Unbound `immutable.Iterable`
  * @define unbound immutable collection
  */
trait CappedCollection[+E]
	extends immutable.Iterable[E] with IterableOps[E, CappedCollection, CappedCollection[E]]
	   with CappedIterable[E] with CappedCollection.Defaults[E, CappedCollection]
{
	override def cappedFactory :CappedIterableFactory[CappedCollection] = CappedCollection
}




/** $factoryInfo
  * $evictionInfo
  * @define Coll `CappedGrowable`
  * @define coll capped mutable collection
  */
@SerialVersionUID(Ver)
case object CappedGrowable
	extends CappedIterableFactory.Delegate[CappedGrowable](CappedArrayBuffer)
	   with CappedIterableFactory.Mutable[CappedGrowable]
{
	trait Defaults[+E, +CC[X] <: CappedGrowable[X] with IterableOps[X, CC, CC[X]]]
		extends CappedIterable.Defaults[E, CC]
	{
		protected override def fromSpecific(coll :IterableOnce[E @uncheckedVariance]) :CC[E @uncheckedVariance] =
			cappedFactory.empty[E](cap) ++= coll
	}
}

/** $Description
  * $evictionIno
  * @define Coll    `CappedGrowable`
  * @define coll    capped mutable collection
  * @define Unbound `mutable.Iterable`
  * @define unbound mutable collection
  */
trait CappedGrowable[E]
	extends mutable.Iterable[E] with IterableOps[E, CappedGrowable, CappedGrowable[E]]
	   with Growable[E] with Shrinkable[E]
	   with CappedIterable[E] with CappedGrowable.Defaults[E, CappedGrowable]
{
	override def knownSize :Int = -1
	override def cappedFactory :CappedIterableFactory[CappedGrowable] = CappedGrowable
}






/** $factoryInfo
  * $evictionInfo
  * @define Coll `CappedSeq`
  * @define coll capped immutable sequence
  */
@SerialVersionUID(Ver)
case object CappedSeq extends CappedSeqFactory.Delegate[CappedIndexedSeq](CappedIndexedSeq) {
	trait DelegateOps[+E, +UC[+X] <: SeqOps[X, UC, UC[X]], +CC[+X] <: CappedSeq[X] with SeqOps[X, CC, CC[X]]]
		extends /*AbstractSeq[E] with CappedSeq[E] with*/ SeqOps[E, CC, CC[E]]
		   with SeqSlicingOps[E, CC, CC[E]]
		   with CappedCollection.Defaults[E, CC] with IterableProxy[E]
	{ This :CC[E] =>
//		def cap :Int
		protected override def underlying :UC[E]
		protected def underlyingFactory :IterableFactory[UC]
		protected def unapply[U >: E](elems :IterableOnce[U]) :Maybe[UC[U]]
		protected def copy[U >: E](seq :UC[U] @uncheckedVariance) :CC[U]

		override def length :Int = underlying.length
		override def apply(i :Int) :E = underlying(i)

		protected override def clippedSlice(from :Int, until :Int) :CC[E] =
			copy(underlying.slice(from, until))

		override def appended[U >: E](elem :U) :CC[U] = copy(
			if (underlying.length == cap) underlying.tail :+ elem
			else underlying :+ elem
		)

		override def prepended[U >: E](elem :U) :CC[U] = copy(
			if (underlying.length == cap) elem +: underlying.init
			else elem +: underlying
		)

		override def appendedAll[U >: E](elems :IterableOnce[U]) :CC[U] = elems.knownSize match {
			case  0 => this
			case -1 =>
				var res  = underlying :UC[U]
				var free = cap - res.length
				val itr  = elems.iterator
				while (free > 0 & itr.hasNext) {
					free -= 1
					res = res :+ itr.next()
				}
				while (itr.hasNext) {
					free = cap
					while (free > 0) {
						free -= 1
						res = res :+ itr.next()
					}
					res = res.takeRight(cap)
				}
				copy(res)
			case size if size <= cap - underlying.length =>
				copy(underlying :++ elems)
			case size if size >= cap => elems match {
				case This(seq) => copy(seq.takeRight(cap))
				case _         => copy(underlyingFactory from elems.iterator.takeRight(cap))
			}
			case _ =>
				copy(underlying :++ elems takeRight cap)
		}
		override def prependedAll[U >: E](elems :IterableOnce[U]) :CC[U] = elems.knownSize match {
			case  0 =>
				this
			case -1 =>
				val prefix = underlyingFactory from elems.iterator.take(cap)
				val res = prefix :++ underlying.take(cap - prefix.length)
				copy(res)
			case size if size <= cap - underlying.length =>
				copy(elems ++: underlying)
			case size if size >= cap => elems match {
				case This(seq) => copy(seq.take(cap))
				case _         => copy(underlyingFactory from elems.iterator.take(cap))
			}
			case _ =>
				copy(elems ++: underlying take cap)
		}
	}
}


/** $Description
  * $evictionInfo
  * @define Coll `CappedSeq`
  * @define coll capped immutable sequence
  * @define Unbound `immutable.Seq`
  * @define unbound immutable sequence
  */
trait CappedSeq[+E] //Should it even be a Seq? We could instead have a Seq adapter.
	extends Seq[E] with SeqOps[E, CappedSeq, CappedSeq[E]]
	   with CappedCollection[E] with CappedCollection.Defaults[E, CappedSeq]
{
	override def iterableFactory :SeqFactory[CappedSeq] = CappedSeq.ofMax(cap)
	override def cappedFactory :CappedSeqFactory[CappedSeq] = CappedSeq
}




/** $factoryInfo
  * $evictionInfo
  * @define Coll `CappedIndexedSeq`
  * @define coll capped immutable indexed sequence
  */
@SerialVersionUID(Ver)
case object CappedIndexedSeq extends CappedSeqFactory.Delegate[CappedIndexedSeq](CappedVector)

/** $Description
  * $evictionInfo
  * @define Coll    `CappedIndexedSeq`
  * @define coll    capped immutable indexed sequence
  * @define Unbound `IndexedSeq`
  * @define unbound immutable indexed sequence
  */
trait CappedIndexedSeq[+E]
	extends IndexedSeq[E] with IndexedSeqOps[E, CappedIndexedSeq, CappedIndexedSeq[E]]
	   with CappedSeq[E] with CappedCollection.Defaults[E, CappedIndexedSeq]
{
	override def iterableFactory :SeqFactory[CappedIndexedSeq] = CappedIndexedSeq.ofMax(cap)
	override def cappedFactory :CappedSeqFactory[CappedIndexedSeq] = CappedIndexedSeq
}




/** $factoryInfo
  * $evictionInfo
  * @define Coll `CappedVector`
  * @define coll capped vector
  */
@SerialVersionUID(Ver)
case object CappedVector extends CappedIterableFactory.Immutable[CappedVector] with CappedSeqFactory[CappedVector] {
	override def ofMax(cap :Int) :SeqFactory[CappedVector] =
		new CappedFactory(cap) with SeqFactory[CappedVector]

	override def empty[E](max :Int) :CappedVector[E] = new CappedVector(Vector.empty, max)
	override def newBuilder[A] :Builder[A, CappedVector[A]] =
		Vector.newBuilder[A].mapResult(vec => new CappedVector(vec, vec.size))
}

/** $Description
  * $evictionInfo
  * @define Coll `CappedVector`
  * @define coll capped vector
  * @define Unbound `Vector`
  * @define unbound vector
  */
@SerialVersionUID(Ver)
final class CappedVector[+E] private (override val underlying :Vector[E], override val cap :Int)
	extends AbstractSeq[E] with IndexedSeqOps[E, CappedVector, CappedVector[E]]
	   with StrictOptimizedSeqOps[E, CappedVector, CappedVector[E]]
	   with CappedIndexedSeq[E] with CappedSeq.DelegateOps[E, Vector, CappedVector]
	   with DefaultSerializable
{
	override def iterableFactory :SeqFactory[CappedVector] = CappedVector
	override def cappedFactory :CappedSeqFactory[CappedVector] = CappedVector
	protected override def underlyingFactory :IterableFactory[Vector] = Vector
	protected override def unapply[U >: E](elems :IterableOnce[U]) :Maybe[Vector[U]] = elems match {
		case vec :Vector[U] => Yes(vec)
		case _              => No
	}
	protected override def copy[U >: E](seq :Vector[U]) :CappedVector[U] =
		if (seq eq underlying) this else new CappedVector(seq, cap)
}






/** $factoryInfo
  * $evictionInfo
  * @define Coll `CappedISet`
  * @define coll capped immutable set
  */
@SerialVersionUID(Ver)
case object CappedISet extends CappedIterableFactory.Delegate[CappedISet](CappedSeqSet)

/** $Description
  * $evictionInfo
  * @define Coll `CappedISet`
  * @define coll capped immutable set
  * @define Unbound `Set`
  * @define unbound set
  */
trait CappedISet[E]
	extends Set[E] with SetOps[E, CappedISet, CappedISet[E]]
	   with CappedCollection[E] with CappedCollection.Defaults[E, CappedISet]
{
	override def cappedFactory :CappedIterableFactory[CappedISet] = CappedISet
}




/** $factoryInfo
  * $evictionInfo
  * @define Coll `CappedSeqSet`
  * @define coll capped LIFO set
  */
@SerialVersionUID(Ver)
case object CappedSeqSet extends CappedIterableFactory.Immutable[CappedSeqSet] {
	override def empty[E](max :Int) :CappedSeqSet[E] = new CappedSeqSet(SeqSet.empty, max)

	override def newBuilder[E] :Builder[E, CappedSeqSet[E]] =
		SeqSet.newBuilder[E].mapResult(set => new CappedSeqSet[E](set, set.size))
}

/** $Description
  * $evictionInfo
  * @define Coll `CappedSeqSet`
  * @define coll capped LIFO set
  */
@SerialVersionUID(Ver)
final class CappedSeqSet[E] private (set :SeqSet[E], override val cap :Int)
	extends AbstractSet[E] with StrictOptimizedSetOps[E, CappedSeqSet, CappedSeqSet[E]]
	   with CappedISet[E] with CappedCollection.Defaults[E, CappedSeqSet]
{
	override def knownSize :Int = set.knownSize
	override def size :Int = set.size
	override def contains(elem :E) :Boolean = set.contains(elem)

	override def incl(elem :E) :CappedSeqSet[E] =
		if (set.contains(elem)) this
		else if (set.size == cap) new CappedSeqSet(set.tail :+ elem, cap)
		else new CappedSeqSet(set :+ elem, cap)

	override def excl(elem :E) :CappedSeqSet[E] = {
		val res = set - elem
		if (set eq res) this else new CappedSeqSet(res, cap)
	}

	override def diff(that :collection.Set[E]) :CappedSeqSet[E] = {
		val res = set diff that
		if (res eq set) this else new CappedSeqSet(res, cap)
	}
	override def removedAll(that :IterableOnce[E]) :CappedSeqSet[E] = {
		val res = set removedAll that
		if (res eq set) this else new CappedSeqSet(res, cap)
	}

	override def iterator :Iterator[E] = set.iterator

	override def cappedFactory :CappedIterableFactory[CappedSeqSet] = CappedSeqSet
}






/** $factoryInfo
  * $evictionInfo
  * @define Coll `CappedBuffer`
  * @define coll capped buffer
  */
@SerialVersionUID(Ver) //consider: renaming to CappedSeq, as this is not a Buffer. Or LIFO/LIFOSeq.
case object CappedBuffer extends CappedSeqFactory.Delegate[CappedBuffer](CappedArrayBuffer)

/** $Description
  * It is not a `mutable.Buffer` to avoid unexpected dropping of elements by the clients.
  * @define Description  A mutable LIFO buffer with a size bound. Unlike in `Buffer`, elements can be added
  *                      only at the back of the $unbound, and when the buffer's size reaches
  *                      [[net.noresttherein.sugar.collections.CappedIterable.cap cap]] elements,
  *                      adding a next element pushes out the oldest (first) element.
  * @define evictionInfo Once the buffer reaches its maximum size, the oldest (first) elements are evicted
  *                      before appending additional elements, maintaining the same size.
  * @define Coll    `CappedBuffer`
  * @define coll    capped buffer
  * @define Unbound `Seq`
  * @define unbound sequence
  */
trait CappedBuffer[E] //consider: not extending SugaredSeqOps; arguably non mutable methods should return a normal Seq
	extends mutable.Seq[E] with mutable.SeqOps[E, CappedBuffer, CappedBuffer[E]]
	   with CappedGrowable[E] with SugaredSeqOps[E, CappedBuffer, CappedBuffer[E]]
	   with CappedGrowable.Defaults[E, CappedBuffer]
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

	override def cappedFactory   :CappedSeqFactory[CappedBuffer] = CappedBuffer
	override def iterableFactory :SeqFactory[CappedBuffer] = cappedFactory.ofMax(cap)
//	protected[this] override def className :String = "CappedSeq[" + cap + "]"
}




/** $factoryInfo
  * $evictionInfo
  * @define Coll    `CappedIndexedSeq`
  * @define coll    capped indexed sequence
  * @define Unbound `IndexedSeq`
  * @define unbound indexed sequence
  */
@SerialVersionUID(Ver)
case object CappedIndexedBuffer extends CappedSeqFactory.Delegate[CappedIndexedBuffer](CappedArrayBuffer)

/** $Description
  * @define Coll    `CappedIndexedSeq`
  * @define coll    capped indexed sequence
  * @define Unbound `IndexedSeq`
  * @define unbound indexed sequence
  */
trait CappedIndexedBuffer[E]
	extends mutable.IndexedSeq[E] with mutable.IndexedSeqOps[E, CappedIndexedBuffer, CappedIndexedBuffer[E]]
	   with CappedBuffer[E] with CappedGrowable.Defaults[E, CappedIndexedBuffer]
{
	override def cappedFactory   :CappedSeqFactory[CappedIndexedBuffer] = CappedIndexedBuffer
	override def iterableFactory :SeqFactory[CappedIndexedBuffer] = cappedFactory.ofMax(cap)
//	protected[this] override def className = "CappedIndexedSeq"
}




/** $factoryInfo
  * $evictionInfo
  * @define Coll `CappedArrayBuffer`
  * @define coll capped array buffer
  */
@SerialVersionUID(Ver)
case object CappedArrayBuffer
	extends CappedSeqFactory[CappedArrayBuffer] with CappedIterableFactory.Mutable[CappedArrayBuffer]
{
	override def empty[A](max :Int) :CappedArrayBuffer[A] = new CappedArrayBuffer[A](max)

	override def newBuilder[A] :Builder[A, CappedArrayBuffer[A]] =
		new ArrayBasedBuilder[Any, A, CappedArrayBuffer[A]] {
			override def result(array :Array[Any], size :Int) :CappedArrayBuffer[A] = {
				val buffer = if (size < array.length) array.slice(0, size + 1) else Array.copyOf(array, size + 1)
				new CappedArrayBuffer(buffer, 0, size)
			}
		}
	override def ofMax(cap :Int) :SeqFactory[CappedArrayBuffer] =
		new CappedFactory(cap) with SeqFactory[CappedArrayBuffer]

	private final val PlaceholderArray = new Array[Any](1)
}


/** $Description The $coll is implemented as a circular buffer storing elements in a pre reserved array
  * of the maximum size.
  * @define Coll    `CappedArrayBuffer`
  * @define coll    capped array buffer
  * @define Unbound `Array`
  * @define unbound array
  */ //No ArraySliceSeqOps because data wraps around.
@SerialVersionUID(Ver) //todo: refactor to use offset+length rather than start+end.
final class CappedArrayBuffer[E] private(buffer :Array[Any], private[this] var start :Int, private[this] var end :Int)
	extends mutable.AbstractSeq[E] with mutable.IndexedSeqOps[E, CappedArrayBuffer, CappedArrayBuffer[E]]
	   with collection.StrictOptimizedSeqOps[E, CappedArrayBuffer, CappedArrayBuffer[E]]
	   with CappedIndexedBuffer[E] with CappedGrowable.Defaults[E, CappedArrayBuffer]
	   with DefaultSerializable
{
	def this(capacity :Int) = this(new Array[Any](math.max(capacity, 0) + 1), 0, 0)
	def this() = this(CappedArrayBuffer.PlaceholderArray, 0, 0)

	def cap :Int = buffer.length - 1
	override def length :Int = (if (start <= end) end else buffer.length + end) - start

	private[sugar] def unsafeArray :Array[Any] = buffer
	private[sugar] def startIndex  :Int = start


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

	override def cappedFactory   :CappedSeqFactory[CappedArrayBuffer] = CappedArrayBuffer
	override def iterableFactory :SeqFactory[CappedArrayBuffer] = CappedArrayBuffer.ofMax(cap)
}






/** $factoryInfo
  * $evictionInfo
  * @define Unbound `Set`
  * @define unbound set
  * @define Coll    `CappedSet`
  * @define coll    capped mutable set
  */ //CappedSetBuffer? CappedMutSet? MutCappedSet? EvictSet? LIFOSet?
@SerialVersionUID(Ver)
case object CappedSet extends CappedIterableFactory.Mutable[CappedSet] {
	override def empty[E](max :Int) :CappedSet[E] = new Impl(max)

	override def newBuilder[A] :Builder[A, CappedSet[A]] =
		VectorSet.newBuilder[A].mapResult(set => new Impl(set.size, set))

	private class Impl[E](override val cap :Int, private[this] var set :VectorSet[E])
		extends mutable.AbstractSet[E] with collection.StrictOptimizedSetOps[E, CappedSet, CappedSet[E]]
		   with CappedSet[E] with DefaultSerializable
	{
		def this(cap :Int) = this(cap, VectorSet.empty)

		override def size = set.size
		override def contains(elem :E) = set.contains(elem)
		override def subtractOne(elem :E) = { set = set - elem; this }
		override def addOne(elem :E) = {
			if (set.contains(elem))
				set = set.excl(elem).incl(elem)
			else if (set.size == cap)
				set = set.tail.incl(elem)
			else
				set = set.incl(elem)
			this
		}
		override def clear() :Unit = set = VectorSet.empty

		override def iterator = set.iterator
	}
}


/** $Description
  * The elements are stored in the insertion order, and adding an already present element 'touches' it,
  * making it the youngest element and moving it to the end of the iteration order.
  * @define Coll    `CappedSet`
  * @define coll    capped set
  * @define Unbound `Set`
  * @define unbound mutable set
  */
trait CappedSet[E]
	extends mutable.Set[E] with mutable.SetOps[E, CappedSet, CappedSet[E]]
	   with collection.StrictOptimizedSetOps[E, CappedSet, CappedSet[E]]
	   with CappedGrowable[E] with CappedGrowable.Defaults[E, CappedSet]
{
	override def cappedFactory :CappedIterableFactory[CappedSet] = CappedSet
}
