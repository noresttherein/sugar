package net.noresttherein.sugar.collections

import scala.collection.immutable.SeqOps
import scala.collection.{IterableOps, SpecificIterableFactory}
import scala.collection.mutable.Builder

import net.noresttherein.sugar.collections.extensions.IteratorExtension
import net.noresttherein.sugar.exceptions.unsupported_!
import net.noresttherein.sugar.funny.generic




trait SugaredIterableOps[+E, +CC[_], +C] extends Any with IterableOps[E, CC, C] {
	/** Equivalent to `this.slice(from, until).foreach(f)`, but may be more efficient. */
	def foreach[U](from :Int, until :Int)(f :E => U) :Unit = {
		val length = knownSize
		if (until > 0 & until > from & (length < 0 | from < length))
			if (length >= 0 & until >= length)
				if (from <= 0) foreach(f)
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
	  */ //consider: renaming to jterator for lolz
	def javaIterator[I <: JavaIterator[_]](implicit shape :JavaIteratorShape[E, I]) :I =
		stepper(shape.stepperShape).javaIterator.asInstanceOf[I]

	/** An opaque representation of a Java [[java.util.Iterator Iterator]] over this $coll, in the most specialized
	  * subclass for the type of elements in this collection, as seen from the point of view of the caller.
	  * Jterator API is provided in the form of extension methods which can be imported from
	  * `sugar.collections.extensions`, which have signatures of standard `Iterator` methods, but manually
	  * specialized for each element type.
	  *
	  * The difference between a `Jterator` and a `JavaIterator`, as returned by method
	  * [[net.noresttherein.sugar.collections.SugaredIterableOps.javaIterator javaIterator]],
	  * is that, for value types, the latter will return one of the three [[java.util.PrimitiveIterator PrimitiveIterator]]
	  * subtypes whic match the element type the closest. This method will however always return a type parameterized
	  * with the exact element type of this collection (not enforced by the method signature
	  * due to problems wth  inferrence). So, a `SugaredIterable[Char].javaIterator`
	  * will return a `PrimitiveIterator.OfInt`, but `SugaredIterable[Char].jterator`
	  * will return a [[net.noresttherein.sugar.collections.IntJterator IntJterator]].
	  * While underneath both thes calls amount to the same, the latter saves the user the hassle of converting
	  * every returned element. Additionally, it prevents a common mistake of calling unspecialized `next()`
	  * instead of the properly specialized variant like `nextInt()`.
	  */ //todo: make this the primary method and delegate javaIterator to it instead.
	def jterator[I <: Jterator[_]](implicit shape :JteratorShape[E, I]) :I =
		javaIterator(shape.javaIteratorShape).asInstanceOf[I]



	//todo: tailored implementations in subclasses.
	def removed(index :Int) :C = fromSpecific(Iterators.removed(iterator, index))
	def removed(from :Int, until :Int) :C = fromSpecific(Iterators.removed(iterator, from, until))

	/** Equivalent to
	  * [[collection.IterableOnceOps.drop drop]]`(from).`[[collection.IterableOnceOps.copyToArray copyToArray]]`(xs, start, len)`,
	  * but avoids, if possible, potentially expensive `drop`.
	  * @return the number of elements copied.
	  */ //consider: renaming to copyFrom
	def copyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int =
		if (from <= 0)
			copyToArray(xs, start, len)
		else {
			val size = knownSize
			if (len <= 0 | size >= 0 & from >= size || start >= xs.length)
				0
			else {
				val from0 = math.max(from, 0)
				val i = iterator.drop(from0)
				i.copyToArray(xs, start, len)
			}
		}

	@inline final def copyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int) :Int =
		copyRangeToArray(xs, start, from, Int.MaxValue)

	@inline final def copyRangeToArray[A >: E](xs :Array[A], from :Int) :Int =
		copyRangeToArray(xs, 0, from, Int.MaxValue)
//
//	/** Similar to [[net.noresttherein.sugar.collections.SugaredIterable.copyRangeToArray copyRangeToArray]],
//	  * but copies to a boxing reference array.
//	  * @return the number of elements copied.
//	  */
//	@inline final def copyRangeToRefArray[A >: E](xs :RefArray[Any], from :Int, start :Int = 0, len :Int = Int.MaxValue) :Int =
//		copyRangeToArray(xs.asAnyArray, from, start, len)

	/** Copies the elements of this $coll to the given array, starting with the `from`-th element.
	  * Copying ends when the iterator has no additional elements, or `len` or `xs.length` elements are copied,
	  * whichever is smaller. The first element is written at index `start`, and if the end of the $coll is reached
	  * before any of the above happens, copying resumes from the beginning of the array.
	  * @return the number of elements copied.
	  * @throws IndexOutOfBoundsException if `start` is less than zero or greater than `xs.length`.
	  */
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
  * [[net.noresttherein.sugar.collections.SlicingOps.clippedSlice trustedSlice]] after validation.
  * Assumes fast `size` operation.
  */
//It would be very convenient if we extended only IterableOps[E, generic.Any, Any],
// as ZigZag could then extend this trait, but we have no way of overriding empty to return C.
trait SlicingOps[+E, +C] extends IterableOps[E, generic.Any, Any] {
//	override def empty :C
	protected override def coll :C
	protected def emptySlice :C
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
		else if (from <= 0 & until >= size) coll
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

	protected def clippedSlice(from :Int, until :Int) :C
	private[collections] final def `->trustedSlice`(from :Int, until :Int) :C = clippedSlice(from, until)

	protected def segmentLength(p :E => Boolean, from :Int) :Int = {
		val iter = iterator.drop(from)
		var i    = 0
		while (iter.hasNext && p(iter.next())) {
			i += 1
		}
		i
	}

//	override def copyRangeToArray[A >: E](xs :Array[A], from :Int, start :Int, len :Int) :Int =
//		if (from <= 0)
//			copyToArray(xs, start, len)
//		else if (len <= 0 || start > xs.length || from >= size)
//			0
//		else {
//			val copied = math.min(xs.length - start, math.min(len, size - from))
//			slice(from, from + copied).copyToArray(xs, start, len)
//		}
}

trait SugaredSlicingOps[+E, +CC[_], +C] extends SugaredIterableOps[E, CC, C] with SlicingOps[E, C] {
	protected override def emptySlice :C = empty
}

trait SeqSlicingOps[+E, +CC[_], +C] extends SugaredSlicingOps[E, CC, C] with SeqOps[E, CC, C] {
//	override def indexWhere(p :E => Boolean, from :Int) :Int = {
//		val i = segmentLength(!p(_), from)
//		if (i == length) -1 else i
//	}
	override def segmentLength(p :E => Boolean, from :Int) :Int = super.segmentLength(p, from)
}