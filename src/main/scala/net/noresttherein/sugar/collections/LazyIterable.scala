package net.noresttherein.sugar.collections

import scala.annotation.tailrec
import scala.collection.{AbstractIterator, IterableFactory, IterableFactoryDefaults, IterableOps, SortedOps, Stepper, StepperShape, immutable}
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.Builder

import net.noresttherein.sugar.collections.extensions.SeqFactoryExtension
import net.noresttherein.sugar.slang.NamedObject
import net.noresttherein.sugar.vars.AbstractDelayed




/** $lazyFactoryInfo
  * @tparam C     $C
  * @tparam LC    $LC
  * @define C     The standard Scala collection supertype of built lazy collections.
  * @define LC    The type constructor of built lazy collections.
  * @define Coll  `LazyIterable`
  * @define coll  lazy collection
  * @define Eager `Iterable`
  * @define lazyFactoryInfo A factory converting lazy expressions of $Eager type to $Coll collections.
  */
trait DelayedFactory[C[_], +LC[_]] {
	/** Adapts a ''by-name'' $Eager to a $Coll. */
	@inline final def apply[X](items: => C[X]) :LC[X] = from(() => items)

	/** Adapts a `Function0[`$Eager`]` to a $Coll. */
	def from[X](items: () => C[X]) :LC[X]

	/** The standard `IterableFactory` building ${coll}s. Note that, in many cases, the contents are passed eagerly,
	  * and thus the built collections can be lazy only in name.
	  */
	val factory :IterableFactory[LC]
}
/*


@SerialVersionUID(Ver)
case object LazyFactory {

	/**
	  * Fixes the element type of `factory` to `A`
	  * @param factory The factory to fix the element type
	  * @tparam A Type of elements
	  * @tparam CC Collection type constructor of the factory (e.g. `Seq`, `List`)
	  * @return A [[Factory]] that uses the given `factory` to build a collection of elements
	  *         of type `A`
	  */
	implicit def toFactory[A, CC[_], LCC[X] <: CC[X]](factory: LazyFactory[CC, LCC]): ToFactory[A, CC, LCC] =
		new ToFactory[A, CC, LCC](factory)

	@SerialVersionUID(3L)
	class ToFactory[A, CC[_], LCC[X] <: CC[X]](private val factory: LazyFactory[CC, LCC])
		extends AnyVal with Factory[A, LCC[A]] with Serializable
	{
		def fromSpecific(it: IterableOnce[A]): LCC[A] = factory.factory.from[A](it)
		def newBuilder: Builder[A, LCC[A]] = factory.factory.newBuilder[A]
	}

	implicit def toBuildFrom[A, CC[_], LCC[X] <: CC[X]](factory: LazyFactory[CC, LCC]): BuildFrom[Any, A, LCC[A]] =
		new BuildFrom[Any, A, LCC[A]] {
			def fromSpecific(from: Any)(it: IterableOnce[A]) = factory.factory.from(it)
			def newBuilder(from: Any) = factory.factory.newBuilder
		}
}
*/






/** A proxy to a lazily evaluated (delayed) iterator instance. */
private final class LazyIterator[+T] private[collections] (lzy: => Iterator[T]) extends AbstractIterator[T] {
	private[this] lazy val evaluated = lzy
	override def hasNext :Boolean = evaluated.hasNext
	override def next() :T = evaluated.next()
}

@SerialVersionUID(Ver)
case object LazyIterator {
	def apply[A](iterator: => Iterator[A]) :Iterator[A] = new LazyIterator(iterator)
	def from[X](items :() => Iterator[X]) :Iterator[X] = new LazyIterator(items())
//	override final val factory = Iterator
}






/** Interface of lazily evaluated, immutable collections with variants of collection operations from `IterableOps`
  * which accept lazy arguments and produce a $coll in return, as well as some lazy collection specific methods.
  * Two varieties of such collections exist:
  *   1. A collection can be simply ''delayed'', serving as a proxy to another, lazily evaluated collection.
  *      These collections are always evaluated in their entirety whenever any individual element is accessed.
  *   1. Element-wise lazy collections are proper implementations with laziness on element level built in.
  *      Typically, each of their elements is a lazy expression itself, and they are not evaluated unless
  *      specifically that element is accessed. An example of such collection from Scala standard library
  *      is [[scala.collection.immutable.LazyList LazyList]].
  *
  * All lazy collections allow lazy adding of elements, that is implement at least
  * [[net.noresttherein.sugar.collections.LazyIterableOps.added added]] method accepting a by-name argument,
  * but implementations of specific collection interfaces typically provide lazy counterparts of other standard methods.
  * @note No subtype of this trait should mix in `IterableOps[E, _, T]` for a strict subtype `T` of `C`,
  *       that is for every `C1 <: IterableOps[E, _, C1]`, if `C <: LazyIterableOps[E, _, C2, _, _]`, then `C2 <: C1`.
  * @tparam E     $E
  * @tparam CC    $CC
  * @tparam C     $C
  * @tparam LCC   $LCC
  * @tparam LC    $LC
  * @define E     The type of elements in this collection.
  * @define CC    The type constructor of the standard, non-lazy Scala collection.
  * @define C     The standard, non-lazy Scala collection supertype.
  * @define LCC   The type constructor for this lazy collection.
  * @define LC    The self type of this lazy collection, returned from methods like `filter`, `take`, etc.
  * @define Coll  `LazyIterable`
  * @define coll  lazy collection
  * @define Eager `Iterable`
  * @define lazyCollectionInfo A lazily evaluated version of Scala $Eager.
  */ //todo: drop AbstractLazy from public interfaces
trait LazyIterableOps[+E, +CC[X] <: IterableOps[X, CC, _], +C <: IterableOps[E, CC, C],
                      +LCC[X] <: IterableOps[X, LCC, _], +LC <: IterableOps[E, LCC, LC]]
	extends AbstractDelayed[C] with IterableOps[E, LCC, LC] with Serializable
{   //Implementation uses strict, rather than definite, on methods returning LC
	// in order to avoid wrapping a LazyIterableOps in another LazyIterableOps.
	override def iterableFactory :IterableFactory[LCC] = lazyFactory.factory
	protected def lazyFactory :DelayedFactory[CC @uncheckedVariance, LCC]
	protected def lazySpecific(items: => C @uncheckedVariance) :LC
	protected def lazyGeneric[O](items: => CC[O] @uncheckedVariance) :LCC[O] = lazyFactory(items)

	override def knownSize :Int = if (initializer == null) definite.knownSize else -1
	override def size :Int = definite.size

	@inline final def isEvaluated :Boolean = isDefinite
	override def isEmpty :Boolean = definite.isEmpty

	private def isDefinite :Boolean = initializer == null

	/** True if all elements of the collection are computed and
	  * a [[net.noresttherein.sugar.collections.LazyIterableOps.strict strict]] $Eager can be cheaply produced.
	  */
	def isStrict :Boolean = {
		@tailrec def rec(items :C) :Boolean = items match {
			case lzy :LazyIterableOps[E, CC, C, LCC, LC] @unchecked =>
				if (lzy.isDefinite) rec(lzy.definite) else false
			case _  => true
		}
		initializer == null && rec(definite)
	}

	def added[U >: E](elem: => U) :LCC[U] = lazyGeneric(strict.concat(Seq.one(elem)))
	@inline final def +#[U >: E](elem: => U) :LCC[U] = added(elem)

	def addedAll[U >: E](elems: => IterableOnce[U]) :LCC[U] = lazyGeneric(strict.concat(elems))
	@inline final def ++#[U >: E](elems: => IterableOnce[U]) :LCC[U] = addedAll(elems)

	override def concat[O >: E](suffix :IterableOnce[O]) :LCC[O] = lazyGeneric(strict.concat(suffix))

	override def headOption :Option[E] = definite.headOption
	override def lastOption :Option[E] = definite.lastOption
	override def head :E = definite.head
	override def last :E = definite.last
	override def tail :LC = definite match {
		case lzy :LazyIterableOps[E, CC, C, LCC, LC] @unchecked => lzy.tail
		case _                                                  => lazySpecific(strict.tail)
	}
	override def init :LC = definite match {
		case lzy :LazyIterableOps[E, CC, C, LCC, LC] @unchecked => lzy.init
		case _                                                  => lazySpecific(strict.init)
	}

	//take,drop,slice, etc.
	//filter, filterNot, partition, etc

	override def transpose[O](implicit asIterable :E => Iterable[O]) :LCC[LCC[O] @uncheckedVariance] =
		lazyGeneric(strict.transpose[O].map(lazyGeneric(_)))

	override def groupBy[K](f :E => K) :LazyMap[K, LC] =
		LazyMap.from(() => strict.groupBy(f).map { case (key, values) => (key, lazySpecific(values)) })

	override def groupMap[K, O](key :E => K)(f :E => O) :LazyMap[K, LCC[O]] =
		LazyMap.from(() => strict.groupMap(key)(f).map { case (key, values) => (key, lazyGeneric(values)) })

	override def groupMapReduce[K, B](key :E => K)(f :E => B)(reduce :(B, B) => B) :LazyMap[K, B] =
		LazyMap.from(() => strict.groupMapReduce(key)(f)(reduce))

	override def scanRight[O](z :O)(op :(E, O) => O) :LCC[O] = lazyGeneric(strict.scanRight(z)(op))

	//map, flatMap, flatten, collect, partitionMap
	//zip, zipAll, etc.

	override def empty :LC = lazySpecific(strict.empty)

	override def iterator :Iterator[E] =
		if (initializer == null) definite.iterator else new LazyIterator(strict.iterator)

	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S =
		if (initializer == null) definite.stepper else LazyStepper(strict.stepper)

	/** Recursively evaluates all lazy components of this $coll and returns its eager $Eager equivalent. */
	def strict :C = {
		@tailrec def rec(items :C) :C = items match {
			case lzy :LazyIterableOps[E, CC, C, LCC, LC] @unchecked => rec(lzy.definite)
			case strict                                             => strict
		}
		rec(definite)
	}
	protected def unwrap :C = {
		@tailrec def rec(items :C) :C = items match {
			case lzy :LazyIterableOps[E, CC, C, LCC, LC] @unchecked if lzy.isDefinite => rec(lzy.definite)
			case _ => items
		}
		rec(definite)
	}

//	protected def writeReplace :Any = definite
}




trait LazyIterableFactoryDefaults[+E, +CC[x] <: IterableOps[x, CC, CC[x]], +LCC[x] <: IterableOps[x, LCC, LCC[x]]]
	extends LazyIterableOps[E, CC, CC[E @uncheckedVariance], LCC, LCC[E @uncheckedVariance]]
	   with IterableFactoryDefaults[E, LCC]
{
//	protected override def lazyGeneric[O](items: => CC[O] @uncheckedVariance) :LCC[O] = lazyFactory.from(() => items)
	protected override def lazySpecific(items : => CC[E] @uncheckedVariance) :LCC[E @uncheckedVariance] =
		lazyFactory.from(() => items)
}


/** A supertype of all lazy collections other than [[net.noresttherein.sugar.collections.LazyIterator LazyIterator]].
  * @tparam E     $E
  * @define Coll  `LazyIterable`
  * @define coll  lazy collection
  * @define Eager `Iterable`
  *///consider: should serialization evaluate the contents?
trait LazyIterable[+E]
	extends IterableProxy[E] with immutable.Iterable[E]
	   with LazyIterableFactoryDefaults[E, Iterable, LazyIterable]
//	   with LazyIterableOps[E, Iterable, Iterable[E], LazyIterable, LazyIterable[E]]
//	   with IterableFactoryDefaults[E, LazyIterable]
{
	protected override def underlying :Iterable[E] = definite
//	protected override def lazySpecific(items : => Iterable[E @uncheckedVariance]) :LazyIterable[E] =
//		LazyIterable.from(() => items)

	protected[this] override def className :String = lazyFactory.toString

	override def toString :String =
		if (initializer == null) mkString(className + "(", ", ", ")") else className + "(...)"
}


/** $lazyFactoryInfo
  * @define Coll  `LazyIterable`
  * @define coll  lazy collection
  * @define Eager `Iterable`
  */
@SerialVersionUID(Ver)
case object LazyIterable extends DelayedFactory[Iterable, LazyIterable] {
	override def from[E](items: () => Iterable[E]) :LazyIterable[E] = new Delayed(items)

	/** $factoryInfo
	  * @define Coll `LazyIterable`
	  * @define coll lazy collection
	  */
	@SerialVersionUID(Ver)
	override object factory extends NamedObject(LazyIterable.toString + ".factory") with IterableFactory[LazyIterable] {
		override def from[E](source :IterableOnce[E]) :LazyIterable[E] = new Delayed(() => LazyList.from(source))

		override def empty[E] :LazyIterable[E] = new Delayed(() => LazyList.empty)

		override def newBuilder[E] :Builder[E, LazyIterable[E]] = LazyList.newBuilder[E].mapResult(from(_))
	}

	private class Delayed[E](protected[this] override var initializer :() => Iterable[E])
		extends LazyIterable[E] with Serializable
	{
		protected override def lazyFactory :DelayedFactory[Iterable, LazyIterable] = LazyIterable
	}
}






/** A lazy proxy to another `Ordering[X]` instance. */
@SerialVersionUID(Ver) //consider: moving it to another package; numeric?. slang?
class LazyOrdering[X] private[collections] (protected override var initializer :() => Ordering[X])
	extends AbstractDelayed[Ordering[X]] with Ordering[X] with Serializable
{
	override def compare(x :X, y :X) :Int = definite.compare(x, y)

	def strict :Ordering[X] = LazyOrdering.strict(definite)
	def unwrap :Ordering[X] = LazyOrdering.unwrap(definite)

	def isStrict :Boolean =
		isDefinite && (
			definite match {
				case lzy :LazyOrdering[X] => lzy.isStrict
				case _                    => true
			}
		)
}


@SerialVersionUID(Ver)
object LazyOrdering {
	@tailrec def unwrap[E](ordering :Ordering[E]) :Ordering[E] = ordering match {
		case lzy :LazyOrdering[E] if lzy.initializer == null => unwrap(lzy.definite)
		case _                                               => ordering
	}
	@tailrec def strict[E](ordering :Ordering[E]) :Ordering[E] = ordering match {
		case lzy :LazyOrdering[E] => strict(lzy.definite)
		case _                    => ordering
	}
}




trait LazySortedOps[K, +C] extends SortedOps[K, C] {
//	protected def isOrderingDefinite :Boolean
	protected def isOrderingStrict :Boolean = ordering match {
		case lzy :LazyOrdering[K] => lzy.isStrict
		case _                    => true
	}
	def strictOrdering :Ordering[K] = LazyOrdering.strict(ordering)
}
