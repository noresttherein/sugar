package net.noresttherein.sugar.collections

import scala.collection.Factory


/** A mixin trait for `Iterable` implementations backed by another `Iterable`, which contains all its elements.
  * Forwards all calls to traversing methods which return a single value rather than a collection, such as
  * `fold`, etc., to the underlying instance, in case it offers optimized implementations.
  */
trait IterableProxy[+E] extends Iterable[E] {
	protected def underlying :Iterable[E]

	override def isEmpty   :Boolean = underlying.isEmpty
	override def knownSize :Int = underlying.knownSize
//	override def size :Int      = underlying.size //as we mix in this trait to collections with a final implementation.

	override def iterator :Iterator[E] = underlying.iterator

	override def foreach[U](f :E => U)   :Unit      = underlying.foreach(f)
	override def forall(p :E => Boolean) :Boolean   = underlying.forall(p)
	override def exists(p :E => Boolean) :Boolean   = underlying.exists(p)
	override def count(p :E => Boolean)  :Int       = underlying.count(p)
	override def find(p :E => Boolean)   :Option[E] = underlying.find(p)

	override def foldLeft[A](z :A)(op :(A, E) => A)         :A         = underlying.foldLeft(z)(op)
	override def foldRight[A](z :A)(op :(E, A) => A)        :A         = underlying.foldRight(z)(op)
	override def fold[A1 >: E](z :A1)(op :(A1, A1) => A1)   :A1        = underlying.fold(z)(op)
	override def reduce[A >: E](op :(A, A) => A)            :A         = underlying.reduce(op)
	override def reduceOption[A >: E](op :(A, A) => A)      :Option[A] = underlying.reduceOption(op)
	override def reduceLeft[A >: E](op :(A, E) => A)        :A         = underlying.reduceLeft(op)
	override def reduceRight[A >: E](op :(E, A) => A)       :A         = underlying.reduceRight(op)
	override def reduceLeftOption[A >: E](op :(A, E) => A)  :Option[A] = underlying.reduceLeftOption(op)
	override def reduceRightOption[A >: E](op :(E, A) => A) :Option[A] = underlying.reduceRightOption(op)
	override def collectFirst[A](pf :PartialFunction[E, A]) :Option[A] = underlying.collectFirst(pf)

	override def sum[A >: E](implicit num :Numeric[A])                :A         = underlying.sum[A]
	override def product[A >: E](implicit num :Numeric[A])            :A         = underlying.product[A]
	override def min[A >: E](implicit ord :Ordering[A])               :E         = underlying.min[A]
	override def minOption[A >: E](implicit ord :Ordering[A])         :Option[E] = underlying.minOption[A]
	override def max[A >: E](implicit ord :Ordering[A])               :E         = underlying.max[A]
	override def maxOption[A >: E](implicit ord :Ordering[A])         :Option[E] = underlying.maxOption[A]
	override def maxBy[A](f :E => A)(implicit cmp :Ordering[A])       :E         = underlying.maxBy(f)
	override def maxByOption[A](f :E => A)(implicit cmp :Ordering[A]) :Option[E] = underlying.maxByOption(f)
	override def minBy[A](f :E => A)(implicit cmp :Ordering[A])       :E         = underlying.minBy(f)
	override def minByOption[A](f :E => A)(implicit cmp :Ordering[A]) :Option[E] = underlying.minByOption(f)

	override def corresponds[A](that :IterableOnce[A])(p :(E, A) => Boolean) :Boolean = underlying.corresponds(that)(p)
	override def copyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int = underlying.copyToArray(xs, start, len)

	override def to[C](factory :Factory[E, C]) :C = underlying to factory
}
