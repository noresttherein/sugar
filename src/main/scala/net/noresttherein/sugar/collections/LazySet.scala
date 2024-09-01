package net.noresttherein.sugar.collections

import scala.collection.{IterableFactory, SortedIterableFactory, SortedSetFactoryDefaults}
import scala.collection.immutable.{AbstractSet, HashSet, SetOps, SortedSet, SortedSetOps}
import scala.collection.mutable.Builder

import net.noresttherein.sugar.vars.AbstractDelayed




/** Lazy operations on ${coll}s.
  * @tparam E     $E
  * @tparam C     $C
  * @tparam LC    $LC
  * @define Coll  `LazySet`
  * @define coll  lazy set
  * @define Eager `Set`
  */
sealed trait LazySetOps[E, +C <: Set[E] with SetOps[E, Set, C], +LC <: Set[E] with SetOps[E, LazySet, LC]]
	extends SetOps[E, LazySet, LC] with LazyIterableOps[E, Set, C, LazySet, LC]
{
	override def contains(elem :E) :Boolean = definite.contains(elem)
	override def excl(elem :E) :LC = lazySpecific(strict.excl(elem))
	override def incl(elem :E) :LC = lazySpecific(strict.incl(elem))

	override def intersect(that :collection.Set[E]) :LC = lazySpecific(strict.intersect(that))
	override def diff(that :collection.Set[E]) :LC = lazySpecific(strict.diff(that))
	override def removedAll(that :IterableOnce[E]) :LC = lazySpecific(strict.removedAll(that))

	@inline final def added(elem: => E) :LC = lazyIncl(elem)
	def lazyIncl(elem: => E) :LC = lazySpecific(strict.incl(elem))
	def lazyExcl(elem: => E) :LC = lazySpecific(strict.excl(elem)) //consider: other names. plus/minus, without

	@inline final def +#(elem: => E) :LC = lazyIncl(elem)
	@inline final def -#(elem: => E) :LC = lazyExcl(elem)

	def incl(elem: () => E) :LC = lazyIncl(elem())
	def excl(elem: () => E) :LC = lazyExcl(elem())

	@inline final def +(elem: () => E) :LC = incl(elem)
	@inline final def -(elem: () => E) :LC = excl(elem)

	def lazyDiff(that: => collection.Set[E]) :LC = lazySpecific(strict.diff(that))
	def lazyUnion(that: => collection.Set[E]) :LC = lazyConcat(that) //lazySpecific(strict.concat(that))
	def lazyIntersect(that: => collection.Set[E]) :LC = lazySpecific(strict.intersect(that))

	/** Lazily evaluated difference of the two sets, that is a $Coll containing all elements of this set
	  * not present in the argument.
	  */
	@inline final def &&~(that: => collection.Set[E]) :LC = lazyDiff(that)

	/** A lazily evaluated sum of two sets, that is a $Coll containing all elements of both sets. */
	@inline final def ||(that: => collection.Set[E]) :LC = lazyUnion(that)

	/** A lazily evaluated product of two sets, that is a $Coll containing only elements present in both sets. */
	@inline final def &&(that: => collection.Set[E]) :LC = lazyIntersect(that)

	@inline final def ++(that :() => IterableOnce[E]) :LC = concat(that)

	def diff(that: () => collection.Set[E]) :LC = lazyDiff(that())
	def union(that: () => collection.Set[E]) :LC = lazyUnion(that())
	def intersect(that: () => collection.Set[E]) :LC = lazyIntersect(that())
	def concat(that :() => IterableOnce[E]) :LC = lazyConcat(that())

	@inline final def &~(that: () => collection.Set[E]) :LC = diff(that)
	@inline final def |(that: () => collection.Set[E]) :LC = union(that)
	@inline final def &(that: () => collection.Set[E]) :LC = intersect(that)

	@inline final def addedAll(that: => IterableOnce[E]) :LC = lazyConcat(that)
	def lazyConcat(that: => IterableOnce[E]) :LC = lazySpecific(strict.concat(that))
	def lazyRemovedAll(that: => IterableOnce[E]) :LC = lazySpecific(strict.removedAll(that))

	@inline final def ++#(that: => IterableOnce[E]) :LC = lazyConcat(that)
	@inline final def --#(that: => IterableOnce[E]) :LC = lazyRemovedAll(that)
}




/** $lazyCollectionInfo
  * @note The element iteration order may change for each method call. This does not affect lazy sorted sets.
  * @tparam E     $E
  * @define Coll  `LazySet`
  * @define coll  lazy set
  * @define Eager `Set`
  */
sealed abstract class LazySet[E]
	extends AbstractSet[E] with LazyIterable[E] with LazySetOps[E, Set[E], LazySet[E]]
	   with LazyIterableFactoryDefaults[E, Set, LazySet]
//	   with IterableFactoryDefaults[E, LazySet] with Serializable
{
	protected override def lazyFactory :DelayedFactory[Set, LazySet] = LazySet
//	protected override def lazySpecific(items : => Set[E]) :LazySet[E] = LazySet.from(() => items)
}


/** $lazyFactoryInfo
  * @note The element iteration order may be different for repeated calls of the same method, unless a subclass
  *       has a specific order (like [[net.noresttherein.sugar.collections.LazySortedSet LazySortedSet]]).
  * @note When adding an element already present in the set (nominally), it is unspecified which of the two elements
  *       is retained.
  * @define Coll  `LazySet`
  * @define coll  lazy set
  * @define Eager `Set`
  */ //todo: a DelayedSeq object
@SerialVersionUID(Ver)
case object LazySet extends DelayedFactory[Set, LazySet] {
	override def from[E](set: () => Set[E]) :LazySet[E] = new Delayed(set)

	def empty[A] :LazySet[A] = Empty.asInstanceOf[LazySet[A]]

	private[this] val Empty = new PartiallyLazySet[Nothing](HashSet.empty[Nothing])

	/** $factoryInfo
	  * @define Coll `LazySet`
	  * @define coll lazy set
	  */
	@SerialVersionUID(Ver)
	override object factory extends IterableFactory[LazySet] {
		override def from[E](it :IterableOnce[E]) :LazySet[E] = it match {
			case lzy :LazySet[E @unchecked] => lzy
			case _                          => new PartiallyLazySet(LazyLinearSeq.factory.from(it))
		}
		override def empty[E] :LazySet[E] = LazySet.empty

		override def newBuilder[A] :Builder[A, LazySet[A]] =
			HashSet.newBuilder[A].mapResult(new PartiallyLazySet(_))

		override def toString = "LazySet.factory"
	}

	@SerialVersionUID(Ver)
	private class Delayed[E](protected[this] override var initializer :() => Set[E])
		extends LazySet[E] with Serializable
}




@SerialVersionUID(Ver)
private class PartiallyLazySet[E] private (@volatile private[this] var contents :(Set[E], LazyLinearSeq[E]))
	extends LazySet[E] with Serializable
{
	def this(contents :LazyLinearSeq[E]) = this((Set.empty[E], contents))
	def this(contents :Set[E]) = this((contents, LazyLinearSeq.empty))
	def this(eager :Set[E], lzy :LazyLinearSeq[E]) = this((eager, lzy))

	protected[this] override var initializer = () => moveAll()

//	private def moveOne() :E = {
//		val (prefix, suffix) = contents //todo: check if this creates a copy of contents before extracting.
//		val elem = suffix.head
//		contents = (prefix incl elem, suffix.tail)
//		elem
//	}
	private def moveAll() :Set[E] = {
		var (prefix, suffix) = contents
		if (suffix.nonEmpty) {
			prefix = prefix concat suffix
			contents = (prefix, LazyLinearSeq.empty)
		}
		prefix
	}
	protected def copy(prefix :Set[E], suffix :LazyLinearSeq[E]) :LazySet[E] = new PartiallyLazySet[E]((prefix, suffix))

	override def knownSize :Int = {
		val (prefix, suffix) = contents
		val size = prefix.knownSize
		if (size >= 0 && suffix.knownSize == 0) size else -1
	}
	override def size :Int = moveAll().size

	override def contains(elem :E) :Boolean = {
		var (prefix, suffix) = contents
		prefix.contains(elem) || {
			var notFound = true
			while (suffix.nonEmpty && {
				val next = suffix.head
				suffix = suffix.tail
				prefix = prefix incl next
				notFound = next != elem
				notFound
			}) {}
			contents = (prefix, suffix)
			!notFound
		}
	}

	override def incl(elem :E) :LazySet[E] = {
		val (prefix, suffix) = contents
		if (prefix contains elem) this else copy(prefix incl elem, suffix)
	}
	override def excl(elem :E) :LazySet[E]= {
		val prefix = moveAll()
		if (prefix.contains(elem)) copy(prefix excl elem, LazyLinearSeq.empty) else this
	}

	override def lazyIncl(elem: => E) :LazySet[E] = {
		val (prefix, suffix) = contents
		copy(prefix, elem #+: suffix)
	}

	override def concat(that: IterableOnce[E]) :LazySet[E] = {
		val (prefix, suffix) = contents
		if (suffix.isEmpty) copy(prefix, LazyLinearSeq.factory.from(that))
		else copy(prefix, LazyLinearSeq.from(() => that.iterator.filterNot(prefix) reverse_++: suffix))
	}
	override def lazyConcat(that: => IterableOnce[E]) :LazySet[E] = {
		val (prefix, suffix) = contents
		copy(prefix, suffix.lazyReversePrependedAll(that))
	}

	override def foreach[U](f :E => U) :Unit = moveAll().foreach(f)

	override def iterator :Iterator[E] = {
		val (prefix, suffix) = contents
		prefix.iterator ++ suffix.iterator
	}

	override def toString :String = {
		val (prefix, suffix) = contents
		if (suffix.knownSize == 0) prefix.mkString(className + "(", ", ", ")")
		else prefix.mkString(className + "(", ", ", ", ...)")
	}

}




/** $lazyCollectionInfo
  * @tparam E     $E
  * @define Coll  `LazySortedSet`
  * @define coll  lazy sorted set
  * @define Eager `SortedSet`
  */
sealed abstract class LazySortedSet[E]
	extends LazySet[E] with SortedSet[E] with SortedSetOps[E, LazySortedSet, LazySortedSet[E]]
	   with AbstractDelayed[SortedSet[E]] with LazySetOps[E, SortedSet[E], LazySortedSet[E]]
	   with LazySortedOps[E, LazySortedSet[E]]
	   with SortedSetFactoryDefaults[E, LazySortedSet, LazySet]
{
	protected override def lazySpecific(items: => SortedSet[E]) :LazySortedSet[E] =
		if (isOrderingStrict)
			LazySortedSet.reorder.from(() => items)(ordering)
		else
			LazySortedSet.from(() => items)

	override def rangeImpl(from :Option[E], until :Option[E]) :LazySortedSet[E] =
		if (isOrderingStrict)
			LazySortedSet.reorder.from(() => strict.rangeImpl(from, until))(ordering)
		else
			LazySortedSet.from(() => strict.rangeImpl(from, until))

	override def iteratorFrom(start :E) :Iterator[E] =
		if (initializer == null) definite.iteratorFrom(start) else new LazyIterator(strict.iteratorFrom(start))

	override def sortedIterableFactory :SortedIterableFactory[LazySortedSet] = LazySortedSet.factory
}


/** A factory wrapping lazy expressions of type `SortedSet` in $Coll instances.
  * @define Coll  `LazySortedSet`
  * @define coll  lazy sorted set
  * @define Eager `SortedSet`
  */
@SerialVersionUID(Ver)
case object LazySortedSet {
	@inline def apply[E](set: => SortedSet[E]) :LazySortedSet[E] = from(() => set)

	def from[E](set: () => SortedSet[E]) :LazySortedSet[E] = new Derived[E](set)

	@inline def apply[E :Ordering](items: => IterableOnce[E]) :LazySortedSet[E] = from(() => items)

	def from[E :Ordering](items: () => IterableOnce[E]) :LazySortedSet[E] =
		new Delayed(() => SortedSet.from(items()))

	def empty[E :Ordering] :LazySortedSet[E] = factory.empty


	/** $factoryInfo
	  * @define Coll `LazySortedSet`
	  * @define coll lazy sorted set
	  */
	@SerialVersionUID(Ver)
	object factory extends SortedIterableFactory[LazySortedSet] {
		override def from[E :Ordering](it :IterableOnce[E]) :LazySortedSet[E] = it match {
			case ss :SortedSet[E @unchecked] if LazyOrdering.unwrap(ss.ordering) == LazyOrdering.unwrap(Ordering[E]) =>
				new Delayed(() => ss)
			case _ =>
				new Delayed(() => SortedSet.from(it))
		}

		override def empty[E :Ordering] :LazySortedSet[E] = new Delayed(() => SortedSet.empty)
		override def newBuilder[E :Ordering] :Builder[E, LazySortedSet[E]] =
			SortedSet.newBuilder[E].mapResult(set => new Delayed(() => set))

		override def toString = "LazySortedSet.factory"
	}

	@SerialVersionUID(Ver)
	object reorder {
		@inline def apply[E :Ordering](set: => SortedSet[E]) :LazySortedSet[E] = from(() => set)

		@inline def from[E :Ordering](set :() => SortedSet[E]) :LazySortedSet[E] =
			LazySortedSet.from(set: () => IterableOnce[E])
	}

	@SerialVersionUID(Ver)
	private class Derived[E](protected override var initializer: () => SortedSet[E])
		extends LazySortedSet[E] with Serializable
	{
		override def ordering :Ordering[E] =
			if (initializer == null) definite.ordering else new LazyOrdering(() => strictOrdering)
	}

	@SerialVersionUID(Ver)
	private class Delayed[E](protected override var initializer: () => SortedSet[E])
	                                  (implicit override val ordering :Ordering[E])
		extends LazySortedSet[E] with Serializable
	{
		override def isOrderingStrict :Boolean = true
	}
}
