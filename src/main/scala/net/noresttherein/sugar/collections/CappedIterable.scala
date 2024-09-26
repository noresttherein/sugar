package net.noresttherein.sugar.collections

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{AbstractIterable, Factory, IterableFactory, IterableFactoryDefaults, IterableOps, immutable, mutable}
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.{IndexedSeqOps, SeqOps, SetOps, StrictOptimizedSeqOps, StrictOptimizedSetOps}
import scala.collection.mutable.{Builder, Growable, GrowableBuilder, Shrinkable}

import net.noresttherein.sugar.arrays.{ArrayIterator, ArrayLike, CyclicArrayIterator, MutableArrayExtension, ReverseArrayIterator, ReverseCyclicArrayIterator, arraycopy}
import net.noresttherein.sugar.casting.castTypeParamMethods
import net.noresttherein.sugar.collections.CappedIterableFactory.CappedFactory
import net.noresttherein.sugar.collections.CappedSeq.UnapplyCappedWrapper
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.exceptions.{illegal_!, outOfBounds_!}
import net.noresttherein.sugar.extensions.{IterableOnceExtension, IteratorExtension}
import net.noresttherein.sugar.typist.kinds.Any1
import net.noresttherein.sugar.util.CachesHashCode
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




@SerialVersionUID(Ver)
object CappedIterableFactory {

	@inline implicit def cappedIterableFactoryToFactory[X, CC[_]](cappedFactory :CappedIterableFactory[CC])
			:Factory[X, CC[X]] =
		cappedFactory.factory[X]

	class Delegate[+CC[x] <: CappedIterable[x]](delegate :CappedIterableFactory[CC])
		extends CappedIterableFactory[CC]
	{
		override def from[E](source :IterableOnce[E]) :CC[E] = delegate.from(source)
		override def full[E](source :IterableOnce[E]) :CC[E] = delegate.full(source)
		override def firstOf[E](max :Int, source :IterableOnce[E]) :CC[E] = delegate.firstOf(max, source)
		override def lastOf[E](max :Int, source :IterableOnce[E]) :CC[E] = delegate.lastOf(max, source)
		override def ofMax[E](max :Int) :CC[E] = delegate.ofMax(max)
		override def cappedFactory(cap :Int) :IterableFactory[CC] = delegate.cappedFactory(cap)
		override def newBuilder[A] :Builder[A, CC[A]] = delegate.newBuilder
		override def newBuilder[E](max :Int) :Builder[E, CC[E]] = delegate.newBuilder[E]
	}

	trait Mutable[+CC[X] <: SpillGrowable[X]] extends CappedIterableFactory[CC] {
		override def firstOf[E](max :Int, source :IterableOnce[E]) :CC[E] = source.knownSize match {
			case  _ if max <= 0 => empty[E]
			case -1             => source.iterator.foldLeftWhile(ofMax[E](max))(_.size <= max)(_ += _)
			case  n if n <= max => ofMax[E](max) ++= source
			case  _             => source.iterator.foldLeftWhile(ofMax[E](max))(_.size <= max)(_ += _)
		}
		override def lastOf[E](max :Int, source :IterableOnce[E]) :CC[E] =
			if (max <= 0) empty[E]
			else ofMax[E](max) ++= source

		override def newBuilder[E](max :Int) :Builder[E, CC[E]] = new GrowableBuilder[E, CC[E]](ofMax(max))
		override def cappedFactory(cap :Int) :IterableFactory[CC] = new CappedFactory(cap, this)
	}

	trait Immutable[+CC[X] <: IterableOps[X, CC, CC[X]]] extends CappedIterableFactory[CC] {
		private[this] val Empty = ofMax[Nothing](0)

		override def firstOf[E](max :Int, source :IterableOnce[E]) :CC[E] = source match {
			case _ if max <= 0 =>
				empty
			case _ :collection.SetOps[E, Any1, _] | _ :RankingOps[E, IterableOnce, _] =>
				val k = source.knownSize
				val prefix = if (k >= 0 & k <= max) source else source.iterator.take(max)
				ofMax[E](max) ++ prefix
			case _ =>
				val k = source.knownSize
				if (k >= 0 & k <= max)
					ofMax[E](max) ++ source
				else
					source.iterator.foldLeftWhile(newBuilder[E](max))(_.knownSize <= max)(_ += _).result()
		}
		override def lastOf[E](max :Int, source :IterableOnce[E]) :CC[E] =
			if (max <= 0) empty
			else ofMax[E](max) ++ source

		override def empty[E] :CC[E] = Empty.asInstanceOf[CC[E]]
		override def newBuilder[E](max :Int) :Builder[E, CC[E]] = new AdditiveBuilder[E, CC](ofMax(max))
		override def cappedFactory(cap :Int) :IterableFactory[CC] = new CappedFactory[CC](cap, this)
	}

	//consider: swapping names with CappedIterableFactory which is not an IterableFactory
	class CappedFactory[CC[_]](max :Int, factory :CappedIterableFactory[CC]) extends IterableFactory[CC] {
		override def from[A](source :IterableOnce[A]) :CC[A] = factory.lastOf(max, source)
		override def empty[A] :CC[A] = factory.ofMax(max)
		override def newBuilder[A] :Builder[A, CC[A]] = factory.newBuilder(max)
		override def toString :String = factory.toString + "[" + max + "]"
	}
}


/** $factoryInfo
  * $evictionInfo
  * @see [[net.noresttherein.sugar.collections.CappedIterableFactory.cappedFactory cappedFactory]]
  * @define factoryInfo  A factory of collections with an upper size bound equal to the size of the created $Coll.
  *                      Serves also as a factory of iterable factories creating ${coll}s with arbitrary size bounds.
  * @define evictionInfo Once the collection reaches its maximum size, existing elements become removed
  *                      to make space for subsequently added elements.
  * @define Coll `CappedIterable`
  * @define coll capped collection
  */
trait CappedIterableFactory[+CC[_]] /*extends IterableFactory[CC] */{
	def from[E](source :IterableOnce[E]) :CC[E] = source match {
		case capped :CappedIterable[E] => lastOf(capped.cap, capped)
		case _                               => full(source)
	}

	/** Creates a full $Coll with elements of `source`, that is one with
	  * [[net.noresttherein.sugar.collections.CappedIterable.Defaults.cap max]] size equal its size.
	  * This is different from [[net.noresttherein.sugar.collections.CappedIterableFactory.from from]] in that
	  * the latter will preserve the `cap` if the argument already is
	  * a [[net.noresttherein.sugar.collections.CappedIterable CappedImpureIterable]].
	  */
	def full[E](source :IterableOnce[E]) :CC[E] = source.knownSize match {
		case   0 => empty
		case  -1 => (newBuilder[E] ++= source).result()
		case   n => lastOf(n, source)
	}

	/** Creates a $Coll of `max` upper size bound, containing the first `max` elements of `source`
	  * (or its entirety, if smaller).
	  */
	def firstOf[E](max :Int, source :IterableOnce[E]) :CC[E] =
		if (max <= 0 || source.knownSize == 0)
			empty[E]
		else {
			val res = newBuilder[E](max)
			val size = source.knownSize
			if (size >= 0 & size <= max)
				res ++= source
			else
				res ++= source.iterator.take(max)
			res.result()
		}

	/** Creates a $Coll of `max` upper size bound, containing the last `max` elements of `source`
	  * (or its entirety, if smaller).
	  */
	def lastOf[E](max :Int, source :IterableOnce[E]) :CC[E] =
		if (max <= 0 || source.knownSize == 0)
			empty[E]
		else
			(newBuilder[E](max) ++= source).result()

	/** Returns an empty instance with zero capacity for new elements. */
	def empty[E] :CC[E] = ofMax[E](0)

	/** An empty $Coll with an upper size bound of `max` elements.
	  * $evictionInfo
	  */
	def ofMax[E](max :Int) :CC[E]

	/** A builder for a full $Coll, that is with the size bound equal to its size. */
	def newBuilder[E] :Builder[E, CC[E]]

	/** A builder for a $Coll consisting of at most `max` most recent elements added to the builder. */
	def newBuilder[E](max :Int) :Builder[E, CC[E]]

	def factory[E](max :Int) :Factory[E, CC[E]] = new Factory[E, CC[E]] {
		override def newBuilder :Builder[E, CC[E]] = CappedIterableFactory.this.newBuilder(max)
		override def fromSpecific(it :IterableOnce[E]) :CC[E] =
			(CappedIterableFactory.this.newBuilder[E](max) ++= it).result()

		override def toString = CappedIterableFactory.this.toString + ".factory[" + max + "]"
	}

	def factory[E] :Factory[E, CC[E]] = fullFactory.asInstanceOf[Factory[E, CC[E]]]

	private val fullFactory = new Factory[Any, CC[Any]] {
		override def fromSpecific(it :IterableOnce[Any]) :CC[Any] = CappedIterableFactory.this.from(it)
		override def newBuilder :Builder[Any, CC[Any]] = CappedIterableFactory.this.newBuilder
	}

	//This method conflicts with IterableFactory.apply. If we had a better name, we could extend IterableFactory
	def apply(cap :Int) :IterableFactory[CC] = cappedFactory(cap)

	/** A standard `IterableFactory` building $Coll instances with a size limit of `cap`.
	  * If the number of added elements exceeds `cap`, the collection will contain only the last `cap` elements.
	  */ //IterableFactory.iterableFactory returns a Factory, so cappedFactory may return an IterableFactory
	def cappedFactory(cap :Int) :IterableFactory[CC]
}






/** $factoryInfo
  * $evictionInfo
  * @define Coll `CappedIterable`
  * @define coll capped collection
  */
//Consider: it would be useful to have a different name,
// to reflect the difference between immutable CappedXxx and mutable SpillXxx. LimitedIterable? BoundIterable?
// We could then perhaps even bring BoundBuffer under the umbrella.
@SerialVersionUID(Ver)
case object CappedIterable extends CappedIterableFactory.Delegate[CappedIterable](CappedPureIterable) {
	type Mutable[E] = SpillGrowable[E]
	type Immutable[E] = CappedPureIterable[E]

	trait Defaults[+E, +CC[X] <: IterableOps[X, CC, CC[X]]]
		extends IterableFactoryDefaults[E, CC] with IterableOps[E, CC, CC[E @uncheckedVariance]]
	{
		/** The maximum number of elements for this $coll and any of the same type crated from it. */
		def cap :Int

		/** A new $coll with the specified maximum size, initialized with the elements of this collection.
		  * If `cap < this.size`, the new $Coll will contain the last `cap` elements of `this`.
		  * Otherwise, the returned collection will equal this one, with only `cap` property being modified.
		  */
		def ofMax(cap :Int) :CC[E @uncheckedVariance] = cappedFactory.lastOf(cap, this)

		override def empty :CC[E @uncheckedVariance] = cappedFactory.ofMax(cap)

		override def concat[B >: E](suffix :IterableOnce[B]) :CC[B] =
			cappedFactory.lastOf(cap, iterator :++ suffix.iterator)

		protected override def fromSpecific(coll :IterableOnce[E @uncheckedVariance]) :CC[E @uncheckedVariance] =
			(newSpecificBuilder ++= coll).result()

		protected override def newSpecificBuilder :Builder[E, CC[E]] @uncheckedVariance =
			cappedFactory.newBuilder[E](cap)

		override def iterableFactory :IterableFactory[CC] = cappedFactory.cappedFactory(cap)
		def cappedFactory :CappedIterableFactory[CC]
	}
}

/** $Description
  * $evictionInfo
  * @define Description An $Unbound containing at most [[net.noresttherein.sugar.collections.CappedIterable.cap cap]]
  *                     elements. For [[net.noresttherein.sugar.collections.SpillGrowable mutable]] collections,
  *                     this is simply an upper bound on the size they can reach.
  *                     For [[net.noresttherein.sugar.collections.CappedPureIterable immutable]] collection,
  *                     it is an additional bound observed and passed on whenever a new collection of the type is built
  *                     by methods of the current collection - whether they add or remove elements.
  * @define evictionInfo Adding new elements will result in removing existing elements, with a strategy depending
  *                      on collection type.
  * @define Coll         `CappedImpureIterable`
  * @define coll         capped collection
  * @define Unbound      `Iterable`
  * @define unbound      collection
  */
trait CappedIterable[+E] extends Iterable[E] with CappedIterable.Defaults[E, CappedIterable] {
	def cappedFactory :CappedIterableFactory[CappedIterable] = CappedIterable
	protected[this] override def className :String = cappedFactory.toString + "[" + cap + "]"
}




/** $factoryInfo
  * $evictionInfo
  * @define evictionInfo Whenever new elements are added to a $Coll and the resulting $coll's size would exceed
  *                      the upper size bound defined at its creation, older elements are removed to reduce
  *                      the collection size to the limit.
  * @define Coll `CappedIterable`
  * @define coll capped immutable collection
  */
@SerialVersionUID(Ver)
case object CappedPureIterable
	extends CappedIterableFactory.Delegate[CappedPureIterable](CappedSeq)
	   with CappedIterableFactory.Immutable[CappedPureIterable]
{
	override def from[E](source :IterableOnce[E]) :CappedPureIterable[E] = source match {
		case capped :CappedPureIterable[E] => capped
		case _                             => CappedSeq.from(source)
	}
	override def firstOf[E](max :Int, source :IterableOnce[E]) :CappedPureIterable[E] = source match {
		case capped :CappedPureIterable[E] if capped.cap == max => capped
		case _                                                  => CappedSeq.firstOf(max, source)
	}
	override def lastOf[E](max :Int, source :IterableOnce[E]) :CappedPureIterable[E] = source match {
		case capped :CappedPureIterable[E] if capped.cap == max => capped
		case _                                                  => CappedSeq.lastOf(max, source)
	}

	trait Defaults[+E, +CC[X] <: IterableOps[X, CC, CC[X]]]
		extends CappedIterable.Defaults[E, CC]
	{
		protected override def fromSpecific(coll :IterableOnce[E @uncheckedVariance]) :CC[E @uncheckedVariance] =
			cappedFactory.lastOf(cap, coll)
	}
}

/** $Description
  * $evictionInfo
  * @define Description  An $Unbound containing at most [[net.noresttherein.sugar.collections.CappedIterable.cap cap]]
  *                      elements. All methods adding or removing elements preserve this maximum.
  * @define evictionInfo Whenever new elements are added to a $Coll and the resulting $coll's size would exceed
  *                      the upper size bound defined at its creation, older elements are removed to reduce
  *                      the collection size to the limit.
  * @define Coll    `CappedIterable`
  * @define coll    capped immutable collection
  * @define Unbound `immutable.Iterable`
  * @define unbound immutable collection
  */
trait CappedPureIterable[+E]
	extends immutable.Iterable[E]
	   with CappedIterable[E] with CappedPureIterable.Defaults[E, CappedPureIterable]
{
	override def cappedFactory :CappedIterableFactory[CappedPureIterable] = CappedPureIterable
}






/** $factoryInfo
  * $evictionInfo
  * @define Coll `CappedSeq`
  * @define coll capped immutable sequence
  */
@SerialVersionUID(Ver)
case object CappedSeq extends CappedIterableFactory.Delegate[CappedSeq](CappedIndexedSeq) {
	override def from[E](source :IterableOnce[E]) :CappedSeq[E] = source match {
		case capped :CappedSeq[E] => capped
		case _                    => CappedIndexedSeq.from(source)
	}
	override def firstOf[E](max :Int, source :IterableOnce[E]) :CappedSeq[E] = source match {
		case capped :CappedSeq[E] if capped.cap == max => capped
		case _                                         => CappedIndexedSeq.firstOf(max, source)
	}
	override def lastOf[E](max :Int, source :IterableOnce[E]) :CappedSeq[E] = source match {
		case capped :CappedSeq[E] if capped.cap == max => capped
		case _                                         => CappedIndexedSeq.lastOf(max, source)
	}

	def unapplySeq[A](x: CappedSeq[A]): UnapplyCappedWrapper[A] = new UnapplyCappedWrapper(x)

	//todo: move it someplace reusable with unapplySeq may return not a Seq.
	final class UnapplyCappedWrapper[A](private val c: collection.SeqOps[A, CappedIterable, CappedIterable[A]])
		extends AnyVal
	{
		def isEmpty: false = false
		def get: UnapplyCappedWrapper[A] = this
		def lengthCompare(len: Int): Int = c.lengthCompare(len)
		def apply(i: Int): A = c(i)
		def drop(n: Int): CappedIterable[A] = c.drop(n)
		def toSeq: scala.Seq[A] = c.toSeq
	}


	trait Defaults[+E, +CC[X] <: CappedSeq[X] with IterableOps[X, CC, CC[X]]]
		extends CappedPureIterable.Defaults[E, CC] with SeqOps[E, CC, CC[E @uncheckedVariance]]
	{
		override def appendedAll[B >: E](suffix :IterableOnce[B]) :CC[B] = super[Defaults].concat(suffix)
	}


	/** The interface for [[net.noresttherein.sugar.collections.CappedSeq CappedSeq]] implementations
	  * backed by a regular seq `underlying`.
	  * @tparam E  the type of the elements in this $Coll.
	  * @tparam UC the type constructor for the underlying sequence containing the elements.
	  * @tparam CC the type constructor for the implementing class (a self type).
	  */
	trait DelegateOps[+E, +UC[+X] <: SeqOps[X, UC, UC[X]], +CC[+X] <: CappedSeq[X] with SeqOps[X, CC, CC[X]]]
		extends IterableProxy[E] with CappedSeq.Defaults[E, CC] with SeqSlicingOps[E, CC, CC[E]]
	{ This :CC[E] =>
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
  *
  * Note that this collection extends `SeqOps`, but not `Seq` itself, due to different append/prepend semantics.
  * @define evictionInfo Whenever new elements are appended to a $Coll and the resulting $coll's size would exceed
  *                      the upper size bound defined at its creation, leading elements are removed to reduce
  *                      the collection size to the limit. Conversely, when prepending new elements,
  *                      trailing elements are removed if necessary.
  * @define Coll `CappedSeq`
  * @define coll capped immutable sequence
  * @define Unbound `immutable.SeqOps`
  * @define unbound immutable sequence
  */
trait CappedSeq[+E]
	extends CappedPureIterable[E] with SeqOps[E, CappedSeq, CappedSeq[E]] with CappedSeq.Defaults[E, CappedSeq] {
//	override def iterableFactory :IterableFactory[CappedSeq] = CappedSeq.cappedFactory(cap)
	override def cappedFactory :CappedIterableFactory[CappedSeq] = CappedSeq
}




/** $factoryInfo
  * $evictionInfo
  * @define Coll `CappedIndexedSeq`
  * @define coll capped immutable indexed sequence
  */
@SerialVersionUID(Ver)
case object CappedIndexedSeq extends CappedIterableFactory.Delegate[CappedIndexedSeq](CappedVector) {
	override def from[E](source :IterableOnce[E]) :CappedIndexedSeq[E] = source match {
		case capped :CappedIndexedSeq[E] => capped
		case _                           => CappedVector.from(source)
	}
	override def firstOf[E](max :Int, source :IterableOnce[E]) :CappedIndexedSeq[E] = source match {
		case capped :CappedIndexedSeq[E] if capped.cap == max => capped
		case _                                                => CappedVector.firstOf(max, source)
	}
	override def lastOf[E](max :Int, source :IterableOnce[E]) :CappedIndexedSeq[E] = source match {
		case capped :CappedIndexedSeq[E] if capped.cap == max => capped
		case _                                                => CappedVector.lastOf(max, source)
	}
	def unapplySeq[A](x: CappedIndexedSeq[A]): UnapplyCappedWrapper[A] = new UnapplyCappedWrapper(x)
}

/** $Description
  * $evictionInfo
  * @define Coll    `CappedIndexedSeq`
  * @define coll    capped immutable indexed sequence
  * @define unbound immutable sequence
  */
trait CappedIndexedSeq[+E]
	extends CappedSeq[E] with IndexedSeqOps[E, CappedIndexedSeq, CappedIndexedSeq[E]]
	   with CappedSeq.Defaults[E, CappedIndexedSeq]
{
//	override def iterableFactory :IterableFactory[CappedIndexedSeq] = CappedIndexedSeq.cappedFactory(cap)
	override def cappedFactory :CappedIterableFactory[CappedIndexedSeq] = CappedIndexedSeq
	override def toSeq :Seq[E] = toIndexedSeq
}




/** $factoryInfo
  * $evictionInfo
  * @define Coll `CappedVector`
  * @define coll capped vector
  */
@SerialVersionUID(Ver)
case object CappedVector extends CappedIterableFactory.Immutable[CappedVector] {
	override def cappedFactory(cap :Int) :IterableFactory[CappedVector] = new CappedFactory(cap, this)
//		new CappedFactory(cap, this) //with SeqFactory[CappedVector]

	def unapplySeq[A](x: CappedVector[A]): UnapplyCappedWrapper[A] = new UnapplyCappedWrapper(x)

	override def from[E](source :IterableOnce[E]) :CappedVector[E] = source match {
		case capped :CappedVector[E]         => capped
		case capped :CappedIterable[E] => new CappedVector(capped.toVector, capped.cap)
		case _                               => new CappedVector(source.toBasicOps.toVector)
	}
	override def firstOf[E](max :Int, source :IterableOnce[E]) :CappedVector[E] = source match {
		case capped :CappedVector[E] if capped.cap == max => capped
		case _ =>
			val k = source.knownSize
			if (k >= 0 & k <= max) new CappedVector(source.toBasicOps.toVector, max)
			else                   new CappedVector(source.iterator.take(max).toVector, max)
	}
	override def lastOf[E](max :Int, source :IterableOnce[E]) :CappedVector[E] = source match {
		case capped :CappedVector[E] if capped.cap == max => capped
		case _ => source.knownSize match {
			case -1             => ofMax[E](max) ++ source
			case  n if n <= max => new CappedVector(source.toBasicOps.toVector, max)
			case  n             => new CappedVector(source.iterator.dropInPlace(max - n).toVector, max)
		}
	}
	override def ofMax[E](max :Int) :CappedVector[E] = new CappedVector(Vector.empty, max)
	override def newBuilder[A] :Builder[A, CappedVector[A]] =
		Vector.newBuilder[A].mapResult(vec => new CappedVector(vec, vec.size))
}

/** $Description
  * $evictionInfo
  * @define Coll `CappedVector`
  * @define coll capped vector
  * @define unbound sequence
  */
@SerialVersionUID(Ver)
final class CappedVector[+E] private (protected override val underlying :Vector[E], override val cap :Int)
	extends AbstractIterable[E]
	   with CappedIndexedSeq[E] with IndexedSeqOps[E, CappedVector, CappedVector[E]]
	   with StrictOptimizedSeqOps[E, CappedVector, CappedVector[E]]
	   with CappedSeq.DelegateOps[E, Vector, CappedVector]
	   with DefaultSerializable with CachesHashCode
{
	private def this(vector :Vector[E]) = this(vector, vector.length)

//	override def iterableFactory :IterableFactory[CappedVector] = CappedVector
	override def cappedFactory :CappedIterableFactory[CappedVector] = CappedVector
	protected override def underlyingFactory :IterableFactory[Vector] = Vector
	protected override def unapply[U >: E](elems :IterableOnce[U]) :Maybe[Vector[U]] = elems match {
		case vec :Vector[U] => Yes(vec)
		case _              => No
	}
	protected override def copy[U >: E](seq :Vector[U]) :CappedVector[U] =
		if (seq eq underlying) this else new CappedVector(seq, cap)

	override def toIndexedSeq :IndexedSeq[E] = underlying
	override def toVector :Vector[E] = underlying
}






/** $factoryInfo
  * $evictionInfo
  * @define Coll `CappedSet`
  * @define coll capped immutable set
  */
@SerialVersionUID(Ver)
case object CappedSet extends CappedIterableFactory.Delegate[CappedSet](CappedSeqSet) {
	trait Defaults[E, +C[X] <: SetOps[X, C, C[X]]]
		extends CappedPureIterable.Defaults[E, C] with SetOps[E, C, C[E]]
	{
		override def concat(that :IterableOnce[E]) :C[E] = fromSpecific(that)
	}
}

/** $Description
  * $evictionInfo
  *
  * Note that this collection extends `SetOps`, but not `Set` itself, due to different union semantics.
  * @define Coll `CappedSet`
  * @define coll capped set
  * @define unbound set
  */
trait CappedSet[E]
	extends CappedPureIterable[E] with CappedSet.Defaults[E, CappedSet]
//	   with SetOps[E, CappedSet, CappedSet[E]]
{
	override def cappedFactory :CappedIterableFactory[CappedSet] = CappedSet
}




/** $factoryInfo
  * $evictionInfo
  * @define Coll `CappedSeqSet`
  * @define coll capped LIFO set
  */
@SerialVersionUID(Ver)
case object CappedSeqSet extends CappedIterableFactory.Immutable[CappedSeqSet] {
	override def from[E](source :IterableOnce[E]) :CappedSeqSet[E] = source match {
		case set :CappedSeqSet[E @unchecked] => set
		case set :VectorSet[E @unchecked]    => new CappedSeqSet(set, set.size)
		case _ if source.knownSize == 0      => empty
		case _                               => new CappedSeqSet(VectorSet.from(source))
	}
	override def ofMax[E](max :Int) :CappedSeqSet[E] = new CappedSeqSet(VectorSet.empty, max)

	override def newBuilder[E] :Builder[E, CappedSeqSet[E]] =
		VectorSet.newBuilder[E].mapResult(set => new CappedSeqSet[E](set, set.size))
}

/** $Description
  * $evictionInfo
  * @define Coll `CappedSeqSet`
  * @define coll capped LIFO set
  */ //We require a VectorSet because it is essentially covariant, and we don't need to recreate it in CappedSeqSet.from.
@SerialVersionUID(Ver)
final class CappedSeqSet[E] private (set :VectorSet[E], override val cap :Int)
	extends AbstractIterable[E] with CappedSet[E]
	   with StrictOptimizedSetOps[E, CappedSeqSet, CappedSeqSet[E]]
	   with CappedSet.Defaults[E, CappedSeqSet]
	   with DefaultSerializable with CachesHashCode
{
	private def this(set :VectorSet[E]) = this(set, set.size)

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
	override def toSet[U >: E] :Set[U] = set.toSet

	override def cappedFactory :CappedIterableFactory[CappedSeqSet] = CappedSeqSet
}






/** $factoryInfo
  * $evictionInfo
  * @define evictionInfo All methods adding new elements will remove the oldest elements to make room,
  *                      if the total would exceed the limit.
  * @define Coll `SpillGrowable`
  * @define coll capped mutable collection
  */
@SerialVersionUID(Ver)
case object SpillGrowable
	extends CappedIterableFactory.Delegate[SpillGrowable](SpillArrayBuffer)
	   with CappedIterableFactory.Mutable[SpillGrowable]
{
	trait Defaults[+E, +CC[X] <: SpillGrowable[X] with IterableOps[X, CC, CC[X]]]
		extends CappedIterable.Defaults[E, CC]
	{
		protected override def fromSpecific(coll :IterableOnce[E @uncheckedVariance]) :CC[E @uncheckedVariance] =
			cappedFactory.ofMax[E](cap) ++= coll
	}
}

/** $Description
  * $evictionIno
  * @define Description A mutable $Unbound containing at most
  *                     [[net.noresttherein.sugar.collections.CappedIterable.cap cap]] elements.
  * @define evictionInfo All methods adding new elements will remove the oldest elements to make room,
  *                      if the total would exceed the limit.
  * @define Coll    `SpillGrowable`
  * @define coll    evicting mutable collection
  * @define Unbound `mutable.Iterable`
  * @define unbound mutable collection
  */
trait SpillGrowable[E]
	extends mutable.Iterable[E] with IterableOps[E, SpillGrowable, SpillGrowable[E]]
	   with Growable[E] with Shrinkable[E]
	   with CappedIterable[E] with SpillGrowable.Defaults[E, SpillGrowable]
{
	override def knownSize :Int = -1
	override def cappedFactory :CappedIterableFactory[SpillGrowable] = SpillGrowable
}






/** $factoryInfo
  * $evictionInfo
  * @define Coll `SpillBuffer`
  * @define coll evicting buffer
  */
@SerialVersionUID(Ver) //consider: renaming to SpillSeq, as this is not a Buffer. Or LIFO/LIFOSeq/CappedLIFO, CappedQueue.
case object SpillBuffer extends CappedIterableFactory.Delegate[SpillBuffer](SpillArrayBuffer) {
	def unapplySeq[A](x: SpillBuffer[A]): UnapplyCappedWrapper[A] = new UnapplyCappedWrapper(x)
}

/** $Description
  * It is not a `mutable.Buffer` to avoid unexpected dropping of elements by the clients.
  * @see [[net.noresttherein.sugar.collections.BoundBuffer]]
  * @define Description A mutable LIFO buffer with a size bound. Unlike in `Buffer`, elements can be added
  *                     only at the back of the $unbound, and when the buffer's size reaches
  *                     [[net.noresttherein.sugar.collections.CappedIterable.cap cap]] elements,
  *                     adding a next element pushes out the oldest (first) element.
  * @define evictionInfo Once the buffer reaches its maximum size, the oldest (first) elements are evicted
  *                      before appending additional elements, maintaining the same size.
  * @define Coll    `CappedBuffer`
  * @define coll    capped buffer
  * @define Unbound `Seq`
  * @define unbound sequence
  */
trait SpillBuffer[E] //consider: not extending SugaredSeqOps; arguably non mutable methods should return a normal Seq
	extends SpillGrowable[E] with mutable.SeqOps[E, SpillBuffer, SpillBuffer[E]]
	   with SpillGrowable.Defaults[E, SpillBuffer]
	   with SugaredSeqOps[E, SpillBuffer, SpillBuffer[E]]
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

	override def cappedFactory   :CappedIterableFactory[SpillBuffer] = SpillBuffer
//	override def iterableFactory :SeqFactory[SpillBuffer] = cappedFactory.cappedFactory(cap)
}




/** $factoryInfo
  * $evictionInfo
  * @define Coll    `SpillIndexedBuffer`
  * @define coll    evicting indexed buffer
  * @define Unbound `IndexedSeq`
  * @define unbound indexed sequence
  */
@SerialVersionUID(Ver)
case object SpillIndexedBuffer extends CappedIterableFactory.Delegate[SpillIndexedBuffer](SpillArrayBuffer) {
	def unapplySeq[A](x: SpillIndexedBuffer[A]): UnapplyCappedWrapper[A] = new UnapplyCappedWrapper(x)
}

/** $Description
  * @define Coll    `SpillIndexedBuffer`
  * @define coll    evicting indexed buffer
  * @define Unbound `IndexedSeq`
  * @define unbound indexed sequence
  */
trait SpillIndexedBuffer[E]
	extends SpillBuffer[E] with mutable.IndexedSeqOps[E, SpillIndexedBuffer, SpillIndexedBuffer[E]]
	   with SpillGrowable.Defaults[E, SpillIndexedBuffer]
{
	override def knownSize :Int = length
	override def cappedFactory   :CappedIterableFactory[SpillIndexedBuffer] = SpillIndexedBuffer
//	override def iterableFactory :SeqFactory[SpillIndexedBuffer] = cappedFactory.cappedFactory(cap)
}




/** $factoryInfo
  * $evictionInfo
  * @define Coll `SpillArrayBuffer`
  * @define coll evicting array buffer
  */
@SerialVersionUID(Ver)
case object SpillArrayBuffer
	extends CappedIterableFactory.Mutable[SpillArrayBuffer]
{
	def unapplySeq[A](x: SpillArrayBuffer[A]): UnapplyCappedWrapper[A] = new UnapplyCappedWrapper(x)

	override def ofMax[A](max :Int) :SpillArrayBuffer[A] = new SpillArrayBuffer[A](max)

	override def newBuilder[A] :Builder[A, SpillArrayBuffer[A]] =
		new ArrayBasedBuilder[Any, A, SpillArrayBuffer[A]] {
			override def result(array :Array[Any], size :Int) :SpillArrayBuffer[A] = {
				val buffer = if (size < array.length) array.slice(0, size + 1) else Array.copyOf(array, size + 1)
				new SpillArrayBuffer(buffer, 0, size)
			}
		}
	override def cappedFactory(cap :Int) :CappedFactory[SpillArrayBuffer] = new CappedFactory(cap, this)
//		new CappedFactory(cap) with SeqFactory[SpillArrayBuffer]

	private final val PlaceholderArray = new Array[Any](1)
}


/** $Description The $coll is implemented as a circular buffer storing elements in a pre reserved array
  * of the maximum size.
  * @define Coll    `SpillArrayBuffer`
  * @define coll    evicting array buffer
  * @define Unbound `Array`
  * @define unbound array
  */ //No ArraySliceSeqOps because data wraps around.
@SerialVersionUID(Ver) //Consider: specialized arrays
final class SpillArrayBuffer[E] private(buffer :Array[Any], private[this] var start :Int, private[this] var len :Int)
	extends mutable.AbstractIterable[E]
	   with SpillIndexedBuffer[E] with mutable.IndexedSeqOps[E, SpillArrayBuffer, SpillArrayBuffer[E]]
	   with collection.StrictOptimizedSeqOps[E, SpillArrayBuffer, SpillArrayBuffer[E]]
	   with SpillGrowable.Defaults[E, SpillArrayBuffer]
	   with DefaultSerializable
{
	def this(capacity :Int) = this(new Array[Any](math.max(capacity, 0)), 0, 0)
	def this() = this(SpillArrayBuffer.PlaceholderArray, 0, 0)

	def cap :Int = buffer.length
	override def length :Int = len

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
		val cap = buffer.length
		val end = (start + len) % cap
		buffer(end) = elem
		if (len < cap)
			len += 1
		else
			start = (start + 1) % cap
		this
	}
	override def clear() :Unit = {
		val cap = buffer.length
		if (len == cap)
			buffer.clear()
		else if (len <= cap - start)
			buffer.clear(start, start + len)
		else {
			buffer.clear(start, cap)
			buffer.clear(0, (start + len) % cap)
		}
		start = 0
		len   = 0
	}

	override def remove(idx :Int) :E = {
		val elem = apply(idx)
		remove(idx, 1)
		elem
	}

	override def remove(idx :Int, count :Int) :Unit = {
		if (count < 0)
			illegal_!("Negative count: " + errorString(this) + ".remove(" + idx + ", " + count + ").")
		if (idx < 0 | idx > len - count)
			outOfBounds_!(errorString(this) + ".remove(" + idx + ", " + count + ")")
		if (count > 0) {
			//TODO: we should check if idx < len - idx - count, i.e., which part is smaller and faster to move.
			val cap = buffer.length
			val suffixStart = start + idx + count    //May overflow!
			if (idx == 0) {                          //Just move the start marker.
				if (count < cap - start) {           //Move start forward.
					buffer.clear(start, start + count)
					start += count
				} else {                             //Wrap the start of the buffer.
					buffer.clear(start, cap)
					start = start + count - cap
					buffer.clear(0, start)
				}
			} else if (len <= cap - start) {         //Data is not wrapped.
				val suffixLength = len - idx - count
				arraycopy(buffer, suffixStart, buffer, start + idx, start + suffixLength)
				buffer.clear(start + len - count, start + len)
			} else if (idx + count <= cap - start) { //The whole removed fragment resides in the suffix (before wrapping).
				arraycopy(buffer, suffixStart, buffer, start + idx, cap - suffixStart)
				val end = start + len - count        //May overflow!
				if (cap - end >= 0) {                //After the removal the data is not wrapped anymore.
					arraycopy(buffer, 0, buffer, start + len - count, end)
					buffer.clear(end, cap)
					buffer.clear(0, start + len - cap)
				} else {                             //The suffix at the front of the buffer must be split and wrapped.
					arraycopy(buffer, 0, buffer, cap - count, count)
					arraycopy(buffer, count, buffer, 0, end - count)
					buffer.clear(end - count, end)
				}
			} else if (start < cap - idx) {          //The removed fragment is wrapped. Don't overflow!
				val offset = suffixStart - cap
				val end = start + len - count        //May overflow!
				if (cap - end >= 0)                  //After the removal the data is not wrapped anymore.
					arraycopy(buffer, offset, buffer, start + idx, len - idx - count)
				else {                               //Split the suffix.
					arraycopy(buffer, offset, buffer, start + idx, cap - start - idx)
					arraycopy(buffer, count, buffer, 0, end - cap)
				}
				buffer.clear(end - cap, start + len - cap)
			} else {                                 //The whole removed fragment resides in the prefix (after wrapping).
				val offset = suffixStart - cap
				val suffixLength = len - idx - count
				arraycopy(buffer, offset, buffer, start + idx - cap, suffixLength)
				buffer.clear(start + len - count - cap, start + len - cap)
			}
			len -= count
		}
	}

	override def subtractAll(xs :IterableOnce[E]) :this.type = xs.knownSize match {
		case 0 => this
		case 1 => xs match {
			case item :Iterable[E] => subtractOne(item.head)
			case _                 => subtractOne(xs.iterator.next())
		}
		case _ =>
			val removedIndices = mutable.SortedSet.empty[Int]
			xs.toBasicOps.foldLeft(removedIndices)(_ += indexOf(_))
			if (removedIndices.nonEmpty) {
				val cap  = buffer.length
				val it   = removedIndices.iterator
				var next = it.next() //The next removed index.
				var i    = next + 1  //Index to copy from.
				var j    = next      //Index to copy to; invariant: j < i.
				while (i < len) {
					next = if (it.hasNext) it.next() else len
					val copied = next - i
					val from   = if (i >= cap - start) start + i - cap else start + i
					val to     = if (j >= cap - start) start + j - cap else start + j
					ArrayLike.cyclicCopy(buffer, from, buffer, to, copied)
					j += copied
					i = next + 1
				}
			}
			this
	}

	override def iterator :Iterator[E] = {
		val cap = buffer.length
		if (len == 0)
			Iterator.empty
		else if (len <= cap - start)
			ArrayIterator(buffer, start, len).castParam[E]
		else
			CyclicArrayIterator(buffer, start, start + len - cap).castParam[E]
	}
	override def reverseIterator :Iterator[E] = {
		val cap = buffer.length
		if (len == 0)
			Iterator.empty
		else if (len <= cap - start)
			ReverseArrayIterator.slice(buffer, start, start + len).castParam[E]
		else
			ReverseCyclicArrayIterator.slice(buffer, start, start + len - cap).castParam[E]
	}

	override def cappedFactory   :CappedIterableFactory[SpillArrayBuffer] = SpillArrayBuffer
}






/** $factoryInfo
  * $evictionInfo
  * @define Unbound `Set`
  * @define unbound set
  * @define Coll    `SpillSet`
  * @define coll    evicting set
  */
@SerialVersionUID(Ver)
case object SpillSet extends CappedIterableFactory.Mutable[SpillSet] {
	override def ofMax[E](max :Int) :SpillSet[E] = new Impl(max)

	override def newBuilder[A] :Builder[A, SpillSet[A]] =
		VectorSet.newBuilder[A].mapResult(set => new Impl(set.size, set))

	private class Impl[E](override val cap :Int, private[this] var set :VectorSet[E])
		extends mutable.AbstractIterable[E] with collection.StrictOptimizedSetOps[E, SpillSet, SpillSet[E]]
		   with SpillSet[E] with DefaultSerializable
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
		override def toSet[U >: E] :Set[U] = set.toSet
	}
}


/** $Description
  * The elements are stored in the insertion order, and adding an already present element 'touches' it,
  * making it the youngest element and moving it to the end of the iteration order.
  * @define Coll    `SpillSet`
  * @define coll    evicting set
  * @define Unbound `Set`
  * @define unbound mutable set
  */
trait SpillSet[E]
	extends SpillGrowable[E] with mutable.SetOps[E, SpillSet, SpillSet[E]]
	   with SpillGrowable.Defaults[E, SpillSet]
{
	override def cappedFactory :CappedIterableFactory[SpillSet] = SpillSet
}
