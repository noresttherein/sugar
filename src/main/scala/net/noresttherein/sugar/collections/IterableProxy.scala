package net.noresttherein.sugar.collections




/** A mixin trait for `Iterable` implementations backed by another `Iterable`, which contains all its elements.
  * Forwards all calls to traversing methods which return a single value rather than a collection, such as
  * `fold`, etc., to the underlying instance, in case it offers optimized implementations.
  */
trait IterableProxy[+E] extends Iterable[E] {
	protected def underlying :Iterable[E]

	override def isEmpty   :Boolean = underlying.isEmpty
	override def knownSize :Int = underlying.knownSize
//	override def size :Int      = underlying.size

	override def iterator :Iterator[E] = underlying.iterator

	override def foreach[U](f :E => U)   :Unit      = underlying.foreach(f)
	override def forall(p :E => Boolean) :Boolean   = underlying.forall(p)
	override def exists(p :E => Boolean) :Boolean   = underlying.exists(p)
	override def count(p :E => Boolean)  :Int       = underlying.count(p)
	override def find(p :E => Boolean)   :Option[E] = underlying.find(p)

	override def foldLeft[B](z :B)(op :(B, E) => B)         :B         = underlying.foldLeft(z)(op)
	override def foldRight[B](z :B)(op :(E, B) => B)        :B         = underlying.foldRight(z)(op)
	override def fold[A1 >: E](z :A1)(op :(A1, A1) => A1)   :A1        = underlying.fold(z)(op)
	override def reduce[B >: E](op :(B, B) => B)            :B         = underlying.reduce(op)
	override def reduceOption[B >: E](op :(B, B) => B)      :Option[B] = underlying.reduceOption(op)
	override def reduceLeft[B >: E](op :(B, E) => B)        :B         = underlying.reduceLeft(op)
	override def reduceRight[B >: E](op :(E, B) => B)       :B         = underlying.reduceRight(op)
	override def reduceLeftOption[B >: E](op :(B, E) => B)  :Option[B] = underlying.reduceLeftOption(op)
	override def reduceRightOption[B >: E](op :(E, B) => B) :Option[B] = underlying.reduceRightOption(op)
	override def collectFirst[B](pf :PartialFunction[E, B]) :Option[B] = underlying.collectFirst(pf)

	override def sum[B >: E](implicit num :Numeric[B])                :B         = underlying.sum[B]
	override def product[B >: E](implicit num :Numeric[B])            :B         = underlying.product[B]
	override def min[B >: E](implicit ord :Ordering[B])               :E         = underlying.min[B]
	override def minOption[B >: E](implicit ord :Ordering[B])         :Option[E] = underlying.minOption[B]
	override def max[B >: E](implicit ord :Ordering[B])               :E         = underlying.max[B]
	override def maxOption[B >: E](implicit ord :Ordering[B])         :Option[E] = underlying.maxOption[B]
	override def maxBy[B](f :E => B)(implicit cmp :Ordering[B])       :E         = underlying.maxBy(f)
	override def maxByOption[B](f :E => B)(implicit cmp :Ordering[B]) :Option[E] = underlying.maxByOption(f)
	override def minBy[B](f :E => B)(implicit cmp :Ordering[B])       :E         = underlying.minBy(f)
	override def minByOption[B](f :E => B)(implicit cmp :Ordering[B]) :Option[E] = underlying.minByOption(f)

	override def corresponds[B](that :IterableOnce[B])(p :(E, B) => Boolean) :Boolean = underlying.corresponds(that)(p)
	override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int = underlying.copyToArray(xs, start, len)
}
