package net.noresttherein.sugar.collections

import scala.collection.immutable.SeqOps
import scala.collection.{IterableOps, SpecificIterableFactory, mutable}
import scala.collection.mutable.Builder

import net.noresttherein.sugar.collections.extensions.IteratorExtension
import net.noresttherein.sugar.collections.util.rangeCheck
import net.noresttherein.sugar.exceptions.unsupported_!
import net.noresttherein.sugar.extensions.{IterableOnceExtension, SeqFactoryExtension}
import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.funny.generic.Any1
import net.noresttherein.sugar.vars.Maybe.Yes




trait SugaredIterableOps[+E, +CC[_], +C] extends Any with IterableOps[E, CC, C] {
	/** Equivalent to `this.slice(from, until).foreach(f)`, but may be more efficient. */
	def foreach[U](from :Int, until :Int)(f :E => U) :Unit = {
		val length = knownSize
		if (until > 0 & until > from & (length < 0 | from < length))
			if (length >= 0 & until >= length)
				if (from <= 0) foreach(f)           //We must use iterator, because there is no bound on drop(from) :C.
				else iterator.drop(from).foreach(f)
			else if (from <= 0)
				iterator.take(until).foreach(f)
			else
				iterator.slice(from, until).foreach(f)
	}

	/** Executes the given action for every element of this $coll in an undefined, possibly changing order.
	  * Offers implementations a potential possibility of more efficient implementation.
	  */
	def outOfOrder[U](f :E => U) :Unit = foreach(f)

	/** Maps the elements of this $coll with the given function as `map` does, but possibly not in the order defined
	  * by its iterator. May potentially be slightly more efficient than `map` for some implementations.
	  */
	def pureMap[O](f :E => O) :CC[O] = map(f)

	/** Flat maps the elements of this $coll with the given function as `flatMap` does, but possibly not in the order
	  * defined by its iterator. May potentially be slightly more efficient than `map` for some implementations.
	  */
	def pureFlatMap[O](f :E => IterableOnce[O]) :CC[O] = flatMap[O](f)

	/** A Java [[java.util.Iterator Iterator]] over this $coll in the most specialized version available
	  * for the type of elements in this collection. The method relies on static type information at call site
	  * and works similarly to standard [[scala.collection.IterableOnce.stepper stepper]]: it relies on an implicit
	  * witness providing the suitable iterator type. The advantage over Scala iterators is that built in value types
	  * are widened to one of `Int`, `Long` or `Double`, each having a dedicated iterator type allowing to access
	  * the elements of the collection without boxing. Additionally, functions are specialized for these argument types.
	  * @see [[net.noresttherein.sugar.collections.SugaredIterable.jterator jterator]] - a more convenient,
	  *      manually specialized alternative to Java primitive iterators.
	  * @return `this.jterator.javaIterator`.
	  */
	def javaIterator[I <: JavaIterator[_]](implicit shape :JavaIteratorShape[E, I]) :I =
		jterator(shape.jteratorShape).asJava(shape.jteratorShape)

	/** An opaque representation of a Java [[java.util.Iterator Iterator]] over this $coll, in the most specialized
	  * subclass for the type of elements in this collection, as seen from the point of view of the caller.
	  * Jterator API is provided in the form of extension methods which can be imported from
	  * `sugar.collections.extensions`, and have signatures of standard `Iterator` methods, but manually
	  * specialized for each element type.
	  *
	  * The difference between a `Jterator` and a `JavaIterator`, as returned by method
	  * [[net.noresttherein.sugar.collections.SugaredIterableOps.javaIterator javaIterator]],
	  * is that, for value types, the latter will return one of the three [[java.util.PrimitiveIterator PrimitiveIterator]]
	  * subtypes which match the element type the closest. This method will however always return a type parameterized
	  * with the exact element type of this collection (not enforced by the method signature
	  * due to problems wth  inference). So, a `SugaredIterable[Char].javaIterator`
	  * will return a `PrimitiveIterator.OfInt`, but `SugaredIterable[Char].jterator`
	  * will return a [[net.noresttherein.sugar.collections.IntJterator IntJterator]].
	  * While underneath both these calls amount to the same, the latter saves the user the hassle of converting
	  * every returned element. Additionally, it prevents a common mistake of calling unspecialized `next()`
	  * instead of the properly specialized variant like `nextInt()`.
	  */
	def jterator[I <: Jterator[_]](implicit shape :JteratorShape[E, I]) :I =
		stepper(shape.stepperShape).javaIterator.asInstanceOf[I]



	//todo: tailored implementations in subclasses.
	//todo: SugaredStrictIterableOps which use take/drop/slice
	def removed(index :Int) :C = fromSpecific(Iterators.removed(iterator, index))
	def removed(from :Int, until :Int) :C = fromSpecific(Iterators.removed(iterator, from, until))

//	override def copyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int =
//		copyRangeToArray(xs, start, 0, len)

	//todo: move from argument before xs.
	/** Equivalent to
	  * [[collection.IterableOnceOps.drop drop]]`(from).`[[collection.IterableOnceOps.copyToArray copyToArray]]`(xs, start, len)`,
	  * but avoids, if possible, potentially expensive `drop`.
	  * @return the number of elements copied.
	  */ //It's tempting to accept MutableArray instead of Array, but that would complicate delegating to copyToArray.
	def copyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int =
		//Check first if anything is to be copied before delegating to copyToArray
		// to shield us from its inconsistent semantics.
		if (rangeCheck(this, from, xs, start, len))
			0
		else if (from <= 0)
			copyToArray(xs, start, len)
		else
			iterator.drop(from).copyToArray(xs, start, len)

	@inline final def copyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int) :Int =
		copyRangeToArray(xs, start, from, Int.MaxValue)

	@inline final def copyRangeToArray[A >: E](xs :Array[A], from :Int) :Int =
		copyRangeToArray(xs, 0, from, Int.MaxValue)

	/** Copies the elements of this $coll to the given array, starting with the `from`-th element.
	  * Copying ends when the iterator has no additional elements, or `len` or `xs.length` elements are copied,
	  * whichever is smaller. The first element is written at index `start`, and if the end of the $coll is reached
	  * before any of the above happens, copying resumes from the beginning of the array.
	  * @return the number of elements copied.
	  * @throws IndexOutOfBoundsException if `start` is less than zero or greater than `xs.length`.
	  */ //todo: remove these two and make them only extension methods.
	def cyclicCopyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int = Int.MaxValue) :Int = {
		val length = xs.length
		if (from <= 0)
			cyclicCopyToArray(xs, start, len)
		else if (len <= 0 | length == 0)
			0
		else if (len <= length - start || { val size = knownSize; size >= 0 & size <= length - start })
			copyRangeToArray(xs, start, from, len)
		else
			iterator.drop(from).cyclicCopyToArray(xs, start, len)
	}

	/** Copies the elements of this $coll to the given array.
	  * Copying ends when the iterator has no additional elements, or `len` or `xs.length` elements are copied,
	  * whichever is smaller. The first element is written at index `start`, and if the end of the $coll is reached
	  * before any of the above happens, copying resumes from the beginning of the array.
	  * @return the number of elements copied.
	  * @throws IndexOutOfBoundsException if `start` is less than zero or greater than `xs.length`.
	  */
	def cyclicCopyToArray[A >: E](xs :Array[A], start :Int, len :Int = Int.MaxValue) :Int = {
		val length = xs.length
		if (len <= 0 | length == 0)
			0
		else if (len <= length - start || { val size = knownSize; size >= 0 & size <= length - start })
			copyToArray(xs, start, len)
		else
			iterator.cyclicCopyToArray(xs, start, len)
	}

	/** Overridden to fix arithmetic overflow and shoddy out of range semantics,
	  * and to optimize the simple cases.
	  * @return defaults to `iterator.copyToArray(xs, start, len)`.
	  */
	override def copyToArray[U >: E](xs :Array[U], start :Int, len :Int) :Int =
		if (rangeCheck(this, xs, start, len))
			0
		else iterator.copyToArray(xs, start, len)
}


trait SugaredIterable[+A] extends Iterable[A] with SugaredIterableOps[A, Iterable, Iterable[A]]


/** An interface for [[Iterable]] implementations of a specific element type `E`.
  * Their self type `C` is a proper subtype of `CC[E]`, and their methods preserving the collection type,
  * like `filter` and `slice`, return a different type than those which may result in any element type, like `map`.
  * One example would be a String-like text class, which always has `Char` as its element type.
  * Similarly to [[collection.IterableFactoryDefaults]],
  * it defines [[net.noresttherein.sugar.collections.SpecificIterableFactoryDefaults.specificFactory specificFactory]]
  * property of type [[collection.SpecificIterableFactory SpecificIterableFactory]], to which it delegates
  * `empty`, `fromSpecific` and `newSpecificBuilder`.
  */
trait SpecificIterableFactoryDefaults[E, +CC[_], +C <: CC[E]] extends SugaredIterableOps[E, CC, C] {
	protected override def fromSpecific(coll :IterableOnce[E]) :C = specificFactory.fromSpecific(coll)
	protected override def newSpecificBuilder :Builder[E, C] = specificFactory.newBuilder
	override def empty :C = specificFactory.empty
	def specificFactory :SpecificIterableFactory[E, C]
}

/*
trait OrderedIterableOps[+E, +CC[_], +C] extends SugaredIterableOps[E, CC, C] {
	def appended[U >: E](elem :U) :CC[U]
	def appended[U >: E](first :U, second :U, rest :U*) :CC[U] = rest match {
		case _ :LinearSeq[U] => appendedAll(first +: second +: rest)
		case _ => appendedAll(Iterator.double(first, second) :++ rest)
	}
	def appendedAll[U >: E](elems :IterableOnce[U]) :CC[U]
	def prepended[U >: E](elem :U) :CC[U]
	def prependedAll[U >: E](elems :IterableOnce[U]) :CC[U]
}

trait OrderedIterable[+A]
	extends SugaredIterable[A] with OrderedIterableOps[A, OrderedIterable, OrderedIterable[A]]
	   with IterableFactoryDefaults[A, OrderedIterable]
{
	override def iterableFactory :IterableFactory[OrderedIterable] = OrderedIterable
}

object OrderedIterable extends IterableFactory[OrderedIterable] {
	override def from[A](source :IterableOnce[A]) :OrderedIterable[A] = source match {
		case view    :View[A]            => from(view.iterator)
		case items   :Iterable[A]        => empty
		case ordered :OrderedIterable[A] => ordered
		case _ =>
			(newBuilder[A] ++= source).result()
	}

	override def empty[A] :OrderedIterable[A] = ???

	override def newBuilder[A] :Builder[A, OrderedIterable[A]] = ???
}
*/




/** Implements all methods returning a slice of this collection by delegating them to a single
  * [[net.noresttherein.sugar.collections.SlicingOps.clippedSlice clippedSlice]] after validation.
  * Assumes fast `size` operation.
  * @tparam E The type of elements stored in this $collection.
  * @tparam C The collection type returned from all slicing methods. Note that this can be a subtype of
  *           the last type parameter to `IterableOps` mixed in by implementation classes.
  */ //Most methods don't exist in IterableOnceOps, so we can't use it for iterators, too.
trait SlicingOps[+E, +C] extends Any with IterableOps[E, generic.Any1, Any] {
	protected def hasFastSlice :Boolean = false
	protected override def coll :C

	/** The instance returned from `slice` and related methods when the returned collection is empty. */
	protected def emptySlice :C

	/** The instance returned from `slice` and related methods when the slice includes the whole collection.
	  * Defaults to `this.coll`, but should be overridden in mutable subclasses to return a fresh copy.
	  */
	protected def fullSlice :C = coll

	override def tail :C = {
		val size = this.size
		if (size == 0) unsupported_!(toString + ".tail")
		else clippedSlice(1, size)
	}
	override def init :C = {
		val size = this.size
		if (size == 0) unsupported_!(toString + ".init")
		else clippedSlice(0, size - 1)
	}
	override def take(n :Int) :C = slice(0, n)
	override def drop(n :Int) :C = slice(n, size)
	override def dropRight(n :Int) :C = if (n <= 0) coll else slice(0, size - n)
	override def takeRight(n :Int) :C = if (n <= 0) emptySlice else { val size = this.size; slice(size - n, size) }
	override def slice(from :Int, until :Int) :C = {
		val size = this.size
		if (until <= from | until <= 0 || from >= size) emptySlice
		else if (from <= 0 & until >= size) fullSlice
		else if (from <= 0) clippedSlice(0, until)
		else if (until >= size) clippedSlice(from, size)
		else clippedSlice(from, until)
	}
	override def splitAt(n :Int) :(C, C) = {
		val size = this.size
		if (n <= 0) (emptySlice, coll)
		else if (n >= size) (coll, emptySlice)
		else (clippedSlice(0, n), clippedSlice(n, size))
	}
	override def takeWhile(p :E => Boolean) :C = slice(0, segmentLength(p, 0))
	override def dropWhile(p :E => Boolean) :C = slice(segmentLength(p, 0), size)
	override def span(p :E => Boolean) :(C, C) = splitAt(segmentLength(p, 0))

	/** Method to which `slice` delegates to, after ensuring that `0 <= from < until <= size`. */
	protected def clippedSlice(from :Int, until :Int) :C

	protected def segmentLength(p :E => Boolean, from :Int) :Int = {
		val iter = iterator.drop(from)
		var i    = 0
		while (iter.hasNext && p(iter.next())) {
			i += 1
		}
		i
	}
}

//Public classes extending this trait :StringSet, StringMap, SubstringOps, ChoppedString and kinda ArraySlicingOps
trait SugaredSlicingOps[+E, +CC[_], +C] extends Any with SugaredIterableOps[E, CC, C] with SlicingOps[E, C] {
	protected override def emptySlice :C = empty
	//Can't implement any methods from SugaredIterableOps without a IterableOnce bound on C.
}

/** A convenience base trait for sequence implementations which resolves the conflict from inheriting
  * `segmentLength` method from both `SeqOps` and `SugaredSlicingOps`.
  */ //It is preferable for public classes to extend only SugaredSeqOps, and treat this one as an implementation mixin.
trait SeqSlicingOps[+E, +CC[_], +C]
	extends Any with SeqOps[E, CC, C] with SugaredSeqOps[E, CC, C] with SugaredSlicingOps[E, CC, C]
{
	override def segmentLength(p :E => Boolean, from :Int) :Int = super.segmentLength(p, from)
}



/** Additional methods for collections with an inherent, arbitrary order, such as `Seq`.
  * @tparam E  The type of elements stored in this $collection.
  * @tparam CC The collection type returned from all updating methods. Note that it unrelated here to either
  *            the second or the third type parameter of `IterableOps`, to allow implementing classes
  *            to return a more specific, or more generic collections from these methods.
  */ //Extracted from SugaredSeqOps primarily to allow collections such as Cat to extend SeqOps[E, Seq, Cat[E]].
trait PatchingOps[+E, +CC[_]] extends Any with IterableOps[E, Any1, Any] {
	/** For indices in range, functionally equivalent to [[collection.SeqOps.patch patch]]`(index, elems, elems.size)`.
	  * It does ''not'' however use `size` method and may be implemented in a different manner, and the index
	  * must be in `0..this.length - elems.length` range, or an [[IndexOutOfBoundsException]] is thrown,
	  * which may makes it slightly more efficient than `patch`.
	  */ //todo: review implementations and verify that they throw an exception if elems is infinite.
	@throws[IndexOutOfBoundsException]("if index < 0 or index + elems.size > length")
	def updatedAll[U >: E](index :Int, elems :IterableOnce[U]) :CC[U]

	/** Updates the element at `index` and following elements with the specified values.
	  * @return The same result as {{{
	  *         (first +: second +: rest).zip(Iterator.iterate(index)(_ + 1)).foldLeft(this) {
	  *             case (res, (e, i)) => res.updated(i, e)
	  *         }
	  *  }}}
	  */
	@throws[IndexOutOfBoundsException]("if index < 0 or index + 2 + rest.length > length")
	def updatedAll[U >: E](index :Int, first :U, second :U, rest :U*) :CC[U] =
		updatedAll(index, Prepended2Seq(first, second, rest))

	/** Updates the element at `index` and following elements with the specified values.
	  * For indices in range, it is the same as
	  * [[net.noresttherein.sugar.collections.SugaredSeqOps.updatedAll updatedAll]]`(index, elems)`.
	  * It does not however throw exceptions if the index is out of range. Instead, if the index is negative,
	  * it updates elements starting at index zero, but after dropping `-index` initial elements from
	  * `elems`. Similarly, if `this.length < index + elems.size`, then the remaining elements are simply ignored.
	  *///consider: renaming to overlay. However, overwrite is better for mutable Seq.
	def overwritten[U >: E](index :Int, elems :IterableOnce[U]) :CC[U] = {
		if (index == Int.MinValue)
			return updatedAll(0, Nil) //the only way we have here of returning this as CC[U]
		val size = knownSize
		if (size == 0)
			return updatedAll(0, Nil)
		if (size == -1) {
			val these = iterator
			val those = elems.iterator
			if (!these.hasNext || !those.hasNext)
				return updatedAll(0, Nil)
			val droppedThese = if (index > 0) these.drop(index) else these
			val droppedThose = if (index < 0) those.drop(-index) else those
			if (!these.hasNext || !those.hasNext)
				return updatedAll(0, Nil)
			val clippedElems = droppedThese.zipMap(droppedThose)((e, u) => u)
			updatedAll(math.max(0, index), clippedElems)
		} else {
			if (index >= size)
				return updatedAll(0, Nil)
			val elemsSize = elems.knownSize
			if (elemsSize == 0)
				return updatedAll(0, Nil)
			if (elemsSize > 0 & index >= 0 & index + elemsSize <= size)
				return updatedAll(index, elems)
			val those =
				if (index >= 0) elems.iterator.take(size - index)
				else if (index < size - Int.MaxValue) elems.iterator.drop(-index).take(size)
				else elems.iterator.slice(-index, size - index)
			updatedAll(math.max(index, 0), those)
		}
	}

	/** Updates the element at `index` and following elements with the specified values.
	  * For indices in range, it is the same as `updatedAll(index, first, second, rest :_*)`.
	  * It does not however throw exceptions if the index is out of range. Instead, if the index is negative,
	  * it updates elements starting at index zero, but after dropping `-index` initial elements from
	  * `first +: second +: rest`. Similarly, if `this.length < index + 2 + rest.length`, then the remaining
	  * elements are simply ignored.
	  */
	def overwritten[U >: E](index :Int, first :U, second :U, rest :U*) :CC[U] =
		overwritten(index, Prepended2Seq(first, second, rest))

	/** Inserts a new element to this sequence at the specified position, pushing all elements at `index`
	  * and beyond by one position. Equivalent to
	  * [[collection.SeqOps.patch patch]]`(index, Seq(elem), 1)`.
	  */ //todo: permissive indexing
	def inserted[U >: E](index :Int, elem :U) :CC[U]

	/** Equivalent to [[collection.SeqOps.patch patch]]`(index, elems, 0)`. */
	def insertedAll[U >: E](index :Int, elems :IterableOnce[U]) :CC[U]

	/** Equivalent to [[collection.SeqOps.patch patch]]`(index, first +: second +: elems, 0)`. */
	def insertedAll[U >: E](index :Int, first :U, second :U, rest :U*) :CC[U] =
		insertedAll(index, Prepended2Seq(first, second, rest))

}


/** Extra methods for sequences. They allow updating whole segments of the sequence
  * and inserting a new element or elements into the sequence. While these goals can in principle be also achieved
  * by using [[scala.collection.SeqOps.patch patch]], it is usually more convenient to have dedicated methods
  * for simple cases, which can also be implemented more efficiently.
  */
trait SugaredSeqOps[+E, +CC[_], +C]
	extends Any with collection.SeqOps[E, CC, C] with SugaredIterableOps[E, CC, C] with PatchingOps[E, CC]
{
	//todo: test these!
	@inline final def reverse_++:[U >: E](elems :IterableOnce[U]) :CC[U] = reversePrependedAll(elems)
	def reversePrependedAll[U >: E](elems :IterableOnce[U]) :CC[U] = Defaults.reversePrependedAll(this, elems)

	override def updatedAll[U >: E](index :Int, elems :IterableOnce[U]) :CC[U] = Defaults.updatedAll(this, index, elems)

	override def overwritten[U >: E](index :Int, elems :IterableOnce[U]) :CC[U] =
		Defaults.overwritten(this, index, elems)

	override def inserted[U >: E](index :Int, elem :U) :CC[U] = Defaults.inserted(this, index, elem)

	override def insertedAll[U >: E](index :Int, elems :IterableOnce[U]) :CC[U] =
		Defaults.insertedAll(this, index, elems)

	//todo: remove these once the bug is fixed in SeqOps
	override def startsWith[U >: E](that: IterableOnce[U], offset: Int = 0): Boolean =
		offset >= 0 && { val size = knownSize; size < 0 | offset <= size } && {
			var i = iterator
			(offset == 0 || {
				i = i drop offset - 1
				val inRange = i.hasNext
				if (inRange)
					i.next()
				inRange
			}) && {
				val j = that.iterator
				while (j.hasNext && i.hasNext)
					if (i.next() != j.next())
						return false
				!j.hasNext
			}
		}

	override def indexOfSlice[U >: E](that :collection.Seq[U], from :Int) :Int =
		if (from <= 0)
			super.indexOfSlice(that, 0)
		else {
			val thisSize = knownSize
			if (thisSize >= 0)
				if (from > thisSize) -1 else super.indexOfSlice(that, from)
			else {
				var list = this match {
					case seq :collection.LinearSeq[E] => seq
					case _                            => LazyList from iterator
				}
				list = list.drop(from - 1)
				if (list.isEmpty)
					-1
				else {
					val res = list.tail.indexOfSlice(that, 0)
					if (res == -1) -1 else res + from
				}
			}
		}

/*	override def indexOfSlice[U >: E](that :collection.Seq[U], from :Int) :Int = {
		val thisSize = knownSize
		val thatSize = that.knownSize
		if (thisSize >= 0 & thatSize >= 0)
			super.indexOfSlice(that, from)
		else if (that.isEmpty)
			from //Disputable what should be returned here.
		else if (this.isEmpty)
			-1
		else if (thatSize == 1 || that.sizeIs == 1)
			indexOf(that, from)
		else {
			val from0 = math.max(from, 0)
			var i = from0
			case list :collection.LinearSeq[E] =>
				var suffix = list.drop(from)
				if (thatSize >= 0) {
					val indexed = that match {
						case seq :collection.IndexedSeqOps[E, Any1, _] => seq
						case _ => TemporaryBuffer from that
					}
					//jumpTable(i - 1) = the longest proper prefix of that.take(i) which is also a suffix of that.take(i)
					val jumpTable = new Array[Int](thatSize)
					jumpTable(0) = -1
					jumpTable(1) = 0
					var longestPrefix = 0
					var end = 1
					val max = thatSize - 1
					while (end < max) {
						if (indexed(end) == indexed(longestPrefix)) {
							longestPrefix += 1
							end += 1
							jumpTable(end) = longestPrefix
						} else if (longestPrefix > 0)
							longestPrefix = jumpTable(longestPrefix)
						else {
							end += 1
							jumpTable(end) = 0
						}
					}
					while (true) {

					}
				} else {
					while (suffix.nonEmpty) {
						val itr = that.iterator
						if (startsWith(itr, 0))
							return i
						if (!itr.hasNext) {
							val relative = suffix.indexOfSlice(TemporaryBuffer from that, i)
							return if (relative == -1) -1 else relative + i
						}
						suffix = suffix.tail
					}
					-1
				}
			case seq :collection.IndexedSeqOps[E, Any1, Any] => //that can't be an IndexedSeq as its of unknown size.
				val len1 = length //just in case
				while (i < len1) {
					val itr = that.iterator
					if (startsWith(itr, i))
						return i
					if (!itr.hasNext)
						return indexOfSlice[U](TemporaryBuffer from that, i)
					i += 1
				}
				-1
		}
	}
*/
}



trait MutSugaredSeqOps[E, +CC[_], +C <: AnyRef] extends mutable.SeqOps[E, CC, C] with SugaredSeqOps[E, CC, C] {
	//todo: mutator
//	def mutator :Mutator[E] = ???

	/** Updates position `idx` to `first`, and the following positions to `second` and `rest`.
	  * @return the number of updated elements, that is `rest.length + 2`.
	  */
	@throws[IndexOutOfBoundsException]("if idx < 0 or idx >= this.length - 2 - rest.length")
	def updateAll(idx :Int, first :E, second :E, rest :E*) :Int = {
		update(idx, first)
		update(idx + 1, second)
		updateAll(idx + 2, rest) + 2
	}

	/** Updates positions `idx`, `idx + 1`, ..., `idx + elems.size - 1` to subsequent elements of `elems`.
	  * @return `elems.size`
	  */
	@throws[IndexOutOfBoundsException]("if idx < 0 or idx >= this.length - elems.size")
	def updateAll(idx :Int, elems :IterableOnce[E]) :Int =
		elems.toBasicOps.foldLeft(idx) { (i, elem) =>
			update(i, elem); i + 1
		} - idx


	/** Equivalent to `overwrite(idx, first +: second +: rest)`.
	  * @return the number of updated elements
	  */ //consider: renaming to edit
	def overwrite(idx :Int, first :E, second :E, rest :E*) :Int = idx match {
		case  0 =>
			if (length <= 0)
				0
			else {
				update(0, first)
				if (length == 1)
					1
				else {
					update(1, second)
					overwrite(2, rest) + 2
				}
			}
		case -1 =>
			if (length <= 0)
				0
			else {
				update(0, second)
				overwrite(1, rest) + 1
			}
		case -2 =>
			overwrite(0, rest)
		case _ =>
			val length = this.length
			if (idx >= length)
				0
			else {
				update(idx, first)
				if (idx == length - 1)
					1
				else {
					update(idx + 1, second)
					overwrite(idx + 2, rest) + 2
				}
			}
	}

	/** Updates positions `idx`, `idx + 1`, ..., `idx + elems.size - 1` to subsequent elements of `elems`.
	  * Unlike [[net.noresttherein.sugar.collections.MutSugaredSeqOps.updateAll updateAll]], this method is permissive
	  * If `idx >= 0` and `idx + elems.size <= length`, then this is exactly equivalent to
	  * in regard to `idx` or `elems.size` being out of bounds:
	  *   - if `idx >= length` or `elems.size == 0`, then the method returns immediately;
	  *   - if `idx >= 0` and `idx + elems.size <= this.length`, then the call is equivalent to `updateAll(idx, elems)`;
	  *   - if `idx + elems.size > this.length`,
	  *     then the last `elems.size - (length - idx)` elements are ignored, and/or
	  *   - if `idx < 0`, then the first `-idx` elements of `elems` are ignored.
	  *
	  * In simple terms, for the purpose of indexing, this $coll is treated as an infinite sequence,
	  * with slots preceding its elements having negative indices, and writes outside the collection being ignored.
	  * This $coll does not grow as a result of this operation.
	  */ //todo: a common implementation with mutableIndexedSeqExtension.overwrite
	def overwrite(idx :Int, elems :IterableOnce[E]) :Int = {
		val size = elems.knownSize
		if (idx >= length || (size >= 0 && idx <= -size))
			0
		else {
			val length = this.length
			if (size >= 0 && idx <= length - size)
				if (idx >= 0)
					updateAll(idx, elems)
				else if (HasFastSlice.preferDropOverIterator(elems))
					updateAll(idx, elems.toIterableOnceOps.drop(-idx))
				else
					updateAll(idx, elems.iterator.drop(-idx))
			else {
				val index = math.max(0, idx)
				val from  = -math.min(0, idx)
				val until = from + length - index
				updateAll(index, HasFastSlice.slice(elems, from, until))
			}
		}
	}
}
