package net.noresttherein.sugar.collections

import scala.collection.{IterableOps, SpecificIterableFactory}
import scala.collection.mutable.Builder

import net.noresttherein.sugar.JavaTypes.JIterator




trait SugaredIterableOps[+A, +CC[_], +C] extends Any with IterableOps[A, CC, C] {
	/** Executes the given action for every element of this $coll in an undefined, possibly changing order.
	  * Offers implementations a potential possibility of more efficient implementation.
	  */
	def outOfOrder[U](f :A => U) :Unit = foreach(f)

	/** Maps the elements of this $coll with the given function as `map` does, but possibly not in the order defined
	  * by its iterator. May potentially be slightly more efficient than `map` for some implementations.
	  */
	def pureMap[O](f :A => O) :CC[O] = map(f)

	/** Flat maps the elements of this $coll with the given function as `flatMap` does, but possibly not in the order
	  *  defined by its iterator. May potentially be slightly more efficient than `map` for some implementations.
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
}


trait SugaredIterable[+A] extends Iterable[A] with SugaredIterableOps[A, Iterable, Iterable[A]]


trait SpecificIterableOps[E, +CC[_], +C <: CC[E]] extends SugaredIterableOps[E, CC, C] {
	protected override def fromSpecific(coll :IterableOnce[E]) :C = specificFactory.fromSpecific(coll)
	protected override def newSpecificBuilder :Builder[E, C] = specificFactory.newBuilder
	override def empty :C = specificFactory.empty
	protected def specificFactory :SpecificIterableFactory[E, C]
}




