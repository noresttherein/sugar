package net.noresttherein.sugar.collections

import scala.collection.immutable.LinearSeq
import scala.collection.{IterableFactory, IterableFactoryDefaults, IterableOps, SpecificIterableFactory, View}
import scala.collection.mutable.Builder

import net.noresttherein.sugar.JavaTypes.JIterator
import net.noresttherein.sugar.collections.extensions.{IteratorExtension, IteratorObjectExtension}
import net.noresttherein.sugar.collections.util.knownEmpty




trait SugaredIterableOps[+A, +CC[_], +C] extends Any with IterableOps[A, CC, C] {
	/** Equivalent to `this.slice(from, until).foreach(f)`, but may be more efficient. */
	def foreach[U](from :Int, until :Int)(f :A => U) :Unit = {
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
	def outOfOrder[U](f :A => U) :Unit = foreach(f)

	/** Maps the elements of this $coll with the given function as `map` does, but possibly not in the order defined
	  * by its iterator. May potentially be slightly more efficient than `map` for some implementations.
	  */
	def pureMap[O](f :A => O) :CC[O] = map(f)

	/** Flat maps the elements of this $coll with the given function as `flatMap` does, but possibly not in the order
	  * defined by its iterator. May potentially be slightly more efficient than `map` for some implementations.
	  */
	def pureFlatMap[O](f :A => IterableOnce[O]) :CC[O] = flatMap[O](f)

	/** A Java [[java.util.Iterator Iterator]] over this $coll in the most specialized version available
	  * for the type of elements in this collection. The method relies on static type information at call site
	  * and works similarly to standard [[scala.collection.IterableOnce.stepper stepper]]: it relies on an implicit
	  * witness providing the suitable iterator type. The advantage over Scala iterators is that built in value types
	  * are widened to one of `Int`, `Long` or `Double`, each having a dedicated iterator type allowing to access
	  * the elements of the collection without boxing. Additionally, functions are specialized for these argument types.
	  */
	def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[A, I]) :I =
		stepper(shape.stepperShape).javaIterator.asInstanceOf[I]


	//todo: specialized implementations in subclasses.
	def removed(index :Int) :C = fromSpecific(Iterators.removed(iterator, index))
	def removed(from :Int, until :Int) :C = fromSpecific(Iterators.removed(iterator, from, until))

	/** Equivalent to
	  * [[collection.IterableOnceOps.slice slice]]`(from, until).`[[collection.IterableOnceOps.copyToArray copyToArray]]`(xs, start)`,
	  * but avoids potentially expensive `slice`.
	  */
	def copyRangeToArray[B >: A](xs :Array[B], start :Int, from :Int, until :Int) :Int = {
		val size = knownSize
		if (until <= from | until <= 0 | size >= 0 & from >= size || start >= xs.length)
			0
		else if (from <= 0 & size >=0 & until >= size)
			copyToArray(xs, start, size)
		else {
			val from0 = math.max(from, 0)
			val i = iterator.drop(from0)
			i.copyToArray(xs, start, until - from0)
		}
	}

	/** Equivalent to
	  * [[collection.IterableOnceOps.slice slice]]`(from, until).`[[collection.IterableOnceOps.copyToArray copyToArray]]`(xs)`,
	  * but avoids potentially expensive `slice`.
	  */
	@inline final def copyRangeToArray[B >: A](xs :Array[B], from :Int, until :Int) :Int =
		copyRangeToArray(xs, 0, from, until)
}


trait SugaredIterable[+A] extends Iterable[A] with SugaredIterableOps[A, Iterable, Iterable[A]]


/** An interface for [[Iterable]] implementations of a specific element type `E`.
  * Their self type `C` is a proper subtype of `CC[E]`, and their methods preserving the collection type,
  * like `filter` and `slice`, return a different type than those which may result in any element type, like `map`.
  * One example would be a String-like text class, which always has `Char` as its element type.
  * Similarly to [[collection.IterableFactoryDefaults]],
  * it defines [[net.noresttherein.sugar.collections.SpecificIterableOps.specificFactory specificFactory]] property
  * of type [[collection.SpecificIterableFactory SpecificIterableFactory]], to which it delegates
  * `empty`, `fromSpecific` and `newSpecificBuilder`.
  */
trait SpecificIterableOps[E, +CC[_], +C <: CC[E]] extends SugaredIterableOps[E, CC, C] {
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
