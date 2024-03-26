package net.noresttherein.sugar.collections

import scala.annotation.{switch, tailrec}
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{AbstractView, IndexedSeqView, SeqView, Stepper, StepperShape, StrictOptimizedIterableOps, View}
import scala.collection.Searching.{Found, InsertionPoint, SearchResult}
import scala.collection.immutable.{IndexedSeqOps, SeqOps}
import scala.reflect.ClassTag

import net.noresttherein.sugar.collections.EmptySeqOps.searchResult
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.exceptions.{illegal_!, noSuch_!, outOfBounds_!, unsupported_!}
import net.noresttherein.sugar.extensions.{ArrayCompanionExtension, BuilderExtension, IterableOnceExtension, IteratorExtension, PartialFunctionExtension}




/** Base trait for specialized implementations of empty collections. */
private[noresttherein] trait EmptyIterableOps[+E, +CC[_], +C]
	extends SugaredIterableOps[E, CC, C] with StrictOptimizedIterableOps[E, CC, C]
{
	protected def unsupported(method :String) = unsupported_!(toString + '.' + method)
	protected def noSuch(method :String) = noSuch_!(toString + "." + method)
	protected def outOfBounds(method :String, index :Int) =
		outOfBounds_!(index.toString + " for " + this + '.' + method)

	protected def genericEmpty[X] :CC[X] = iterableFactory.empty
	protected def one[T](elem :T) :CC[T] = (iterableFactory.newBuilder[T] hinted 1 addOne elem).result()
	protected def double :(C, C) = (coll, coll)

	override def knownSize :Int = 0
//	override def isEmpty  :Boolean = true

	override def forall(p :E => Boolean) :Boolean = true
	override def exists(p :E => Boolean) :Boolean = false
	override def count(p :E => Boolean)  :Int     = 0
	override def find(p :E => Boolean)   :Option[E] = None
	override def corresponds[A](that :IterableOnce[A])(p :(E, A) => Boolean) :Boolean = that.toBasicOps.isEmpty

	override def sum[A >: E](implicit num :Numeric[A]) :A = num.zero
	override def product[A >: E](implicit num :Numeric[A]) :A = num.one
	override def min[A >: E](implicit ord :Ordering[A]) :E = unsupported("min")
	override def max[A >: E](implicit ord :Ordering[A]) :E = unsupported("max")
	override def maxBy[A](f :E => A)(implicit cmp :Ordering[A]) :E = unsupported("maxBy")
	override def minBy[A](f :E => A)(implicit cmp :Ordering[A]) :E = unsupported("minBy")
	override def minOption[A >: E](implicit ord :Ordering[A]) :Option[E] = None
	override def maxOption[A >: E](implicit ord :Ordering[A]) :Option[E] = None
	override def maxByOption[A](f :E => A)(implicit cmp :Ordering[A]) :Option[E] = None
	override def minByOption[A](f :E => A)(implicit cmp :Ordering[A]) :Option[E] = None

	override def collectFirst[A](pf :PartialFunction[E, A]) :Option[A] = None
	override def foldLeft[A](z :A)(op :(A, E) => A) :A = z
	override def foldRight[A](z :A)(op :(E, A) => A) :A = z
	override def fold[A1 >: E](z :A1)(op :(A1, A1) => A1) :A1 = z

	override def reduce[A >: E](op :(A, A) => A) :A = unsupported("reduce")
	override def reduceOption[A >: E](op :(A, A) => A) :Option[A] = None
	override def reduceLeft[A >: E](op :(A, E) => A) :A = unsupported("reduceLeft")
	override def reduceRight[A >: E](op :(E, A) => A) :A = unsupported("reduceRight")
	override def reduceLeftOption[A >: E](op :(A, E) => A) :Option[A] = None
	override def reduceRightOption[A >: E](op :(E, A) => A) :Option[A] = None

	override def scan[A >: E](z :A)(op :(A, A) => A) :CC[A] = one(z)
	override def scanLeft[A](z :A)(op :(A, E) => A)  :CC[A] = one(z)
	override def scanRight[A](z :A)(op :(E, A) => A) :CC[A] = one(z)


	override def empty :C = coll

	override def foreach[U](f :E => U) :Unit = ()
	override def foreach[U](from :Int, until :Int)(f :E => U) :Unit = ()
	override def filter(pred :E => Boolean) :C = coll
	override def filterNot(pred :E => Boolean) :C = coll
	override def partition(p :E => Boolean) :(C, C) = double

	override def splitAt(n :Int) :(C, C) = double
	override def take(n :Int) :C = coll
	override def takeRight(n :Int) :C = coll
	override def takeWhile(p :E => Boolean) :C = coll
	override def drop(n :Int) :C = coll
	override def dropRight(n :Int) :C = coll
	override def dropWhile(p :E => Boolean) :C = coll
	override def slice(from :Int, until :Int) :C = coll
	override def span(p :E => Boolean) :(C, C) = double

	override def removed(from :Int, until :Int) :C = coll
	override def removed(index :Int) :C = outOfBounds("removed", index)

	override def grouped(size :Int) :Iterator[C] = Iterator.empty
	override def sliding(size :Int, step :Int) :Iterator[C] = Iterator.empty

	override def head :E = noSuch("head")
	override def last :E = noSuch("last")
	override def headOption :Option[E] = None
	override def lastOption :Option[E] = None
	override def tail :C = unsupported("tail")
	override def init :C = unsupported("init")
	override def tails :Iterator[C] = Iterator.empty
	override def inits :Iterator[C] = Iterator.empty

	override def tapEach[U](f :E => U) :C = coll

	override def collect[A](pf :PartialFunction[E, A]) :CC[A] = genericEmpty
	override def partitionMap[A1, A2](f :E => Either[A1, A2]) :(CC[A1], CC[A2]) = (genericEmpty, genericEmpty)

	override def groupMap[K, B](key :E => K)(f :E => B) :Map[K, CC[B]] = Map.empty
	override def groupMapReduce[K, B](key :E => K)(f :E => B)(reduce :(B, B) => B) :Map[K, B] =
		Map.empty
	override def groupBy[K](f :E => K) :Map[K, C] = Map.empty

	override def map[A](f :E => A) :CC[A] = genericEmpty
	override def flatMap[A](f :E => IterableOnce[A]) :CC[A] = genericEmpty
	override def flatten[A](implicit asIterable :E => IterableOnce[A]) :CC[A] = genericEmpty

	override def transpose[A](implicit asIterable :E => Iterable[A]) :CC[CC[A] @uncheckedVariance] = genericEmpty

	override def zip[B](that :IterableOnce[B]) :CC[(E @uncheckedVariance, B)] = genericEmpty
	override def zipWithIndex :CC[(E @uncheckedVariance, Int)] = genericEmpty
	override def zipAll[A1 >: E, B](that :Iterable[B], thisElem :A1, thatElem :B) :CC[(A1, B)] =
		if (that.isEmpty) genericEmpty
		else iterableFactory.from(that.view.map((thisElem, _)))

	override def unzip[A1, A2](implicit asPair :E => (A1, A2)) :(CC[A1], CC[A2]) = (genericEmpty, genericEmpty)
	override def unzip3[A1, A2, A3](implicit asTriple :E => (A1, A2, A3)) :(CC[A1], CC[A2], CC[A3]) =
		(genericEmpty, genericEmpty, genericEmpty)

	override def copyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int = 0
	override def copyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int = 0
	override def cyclicCopyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int = 0
	override def cyclicCopyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int = 0

	override def view     :View[E] = EmptyIterableOps.view
	override def iterator :Iterator[E] = Iterator.empty
	override def toList   :List[E] = Nil
	override def toVector :Vector[E] = Vector.empty
	override def toSet[B >: E] :Set[B] = Set.empty
	override def toMap[K, V](implicit ev :E <:< (K, V)) :Map[K, V] = Map.empty
	override def toArray[B >: E :ClassTag] :Array[B] = Array.empty[B]

	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S with Stepper.EfficientSplit =
		Stepper0()

	override def addString(b :StringBuilder, start :String, sep :String, end :String) :b.type = b
}


@SerialVersionUID(Ver)
private[noresttherein] object EmptyIterableOps {
	trait Variant[+CC[+_], +C <: CC[Nothing]] extends EmptyIterableOps[Nothing, CC, C] {
		override def genericEmpty[X] :CC[X] = coll
	}
	type Generic[+CC[+_]] = Variant[CC, CC[Nothing]]

	val view :View[Nothing] = View.empty
}




private[noresttherein] trait EmptyNonSeqOps[+E, +CC[_], +C] extends EmptyIterableOps[E, CC, C] {
	override def size :Int = 0
	override def isEmpty = true
	override def concat[B >: E](suffix :IterableOnce[B]) :CC[B] = iterableFactory.from(suffix)
	override def toSeq :Seq[E] = Nil
	override def toIndexedSeq :IndexedSeq[E] = Vector.empty
}


@SerialVersionUID(Ver)
private[noresttherein] object EmptyNonSeqOps {
	trait Variant[+CC[+_], +C <: CC[Nothing]]
		extends EmptyIterableOps.Variant[CC, C] with EmptyNonSeqOps[Nothing, CC, CC[Nothing]]

	type Generic[+CC[+_]] = Variant[CC, CC[Nothing]]
}




private[noresttherein] trait EmptySeqOps[+E, +CC[_], +C]
	extends SeqOps[E, CC, C] with SugaredSeqOps[E, CC, C] with EmptyIterableOps[E, CC, C] with SlicingOps[E, C]
{
	//overridden because EmptyIterableOps must be mixed in before SeqOps
//	override def isEmpty :Boolean = true

	override def length :Int = 0
	override def head   :E = noSuch("head")
	override def apply(i :Int) :E = outOfBounds("apply", i)
	override def isDefinedAt(idx :Int) = false

	override def search[U >: E](elem :U)(implicit ord :Ordering[U]) :SearchResult = searchResult
	override def search[U >: E](elem :U, from :Int, to :Int)(implicit ord :Ordering[U]) :SearchResult =
		if (from < to) searchResult else InsertionPoint(from)

	override def findLast(p :E => Boolean) :Option[E] = None
	override def indexWhere(p :E => Boolean, from :Int) :Int = -1
	override def indexOf[U >: E](elem :U, from :Int) :Int = -1
	override def lastIndexOf[U >: E](elem :U, end :Int) :Int = -1
	override def lastIndexWhere(p :E => Boolean, end :Int) :Int = -1

	override def indexOfSlice[U >: E](that :collection.Seq[U], from :Int) :Int =
		if (from <= 0 && that.isEmpty) 0 else -1

	override def lastIndexOfSlice[U >: E](that :collection.Seq[U], end :Int) :Int =
		if (end >= 0 && that.isEmpty) 0 else -1

	override def contains[U >: E](elem :U) = false
	override def containsSlice[U >: E](that :collection.Seq[U]) = false

	override def segmentLength(p :E => Boolean, from :Int) :Int = 0
	override def endsWith[U >: E](that :Iterable[U]) :Boolean = that.isEmpty
	override def startsWith[U >: E](that :IterableOnce[U], offset :Int) :Boolean =
		offset == 0 && that.toBasicOps.isEmpty

	override def padTo[B >: E](len :Int, elem :B) = iterableFactory.fill(len)(elem)

	override def permutations :Iterator[C] = Iterator.single(coll)
	override def combinations(n :Int) :Iterator[C] = if (n == 0) Iterator.single(coll) else Iterator.empty


	override def diff[U >: E](that :collection.Seq[U]) :C = coll
	override def intersect[U >: E](that :collection.Seq[U]) :C = coll
	override def patch[U >: E](from :Int, other :IterableOnce[U], replaced :Int) :CC[U] =
		iterableFactory.from(other)

	override def updated[U >: E](index :Int, elem :U) :Nothing = outOfBounds("updated", index)
	override def updatedAll[U >: E](index :Int, elems :IterableOnce[U]) :CC[U] =
		if (index == 0 && elems.toBasicOps.isEmpty) genericEmpty
		else outOfBounds_!(errorString(this) + ".updatedAll(" + index + ", " + errorString(elems) + ")")

	override def updatedAll[U >: E](index :Int, first :U, second :U, rest :U*) :CC[U] =
		outOfBounds_!(errorString(this) + ".updatedAll(" + index + ", _, _, " + errorString(rest) + ")")

	override def overwritten[U >: E](index :Int, elems :IterableOnce[U]) :CC[U] = genericEmpty
	override def overwritten[U >: E](index :Int, first :U, second :U, rest :U*) :CC[U] = genericEmpty

	override def inserted[U >: E](index :Int, elem :U) :CC[U] =
		if (index == 0) one(elem) else outOfBounds_!(errorString(this) + ".inserted(" + index + ", _)")

	override def insertedAll[U >: E](index :Int, elems :IterableOnce[U]) :CC[U] =
		if (index == 0) iterableFactory.from(elems)
		else outOfBounds_!(errorString(this) + ".insertedAll(" + index + ", " + errorString(elems) + ")")

	override def prepended[U >: E](elem :U) :CC[U] = one(elem)
	override def appended[U >: E](elem :U)  :CC[U] = one(elem)
	override def prependedAll[U >: E](prefix :IterableOnce[U]) :CC[U] = insertedAll(0, prefix)
	override def appendedAll[B >: E](suffix :IterableOnce[B]) :CC[B] = insertedAll(0, suffix)

	override def sorted[U >: E](implicit ord :Ordering[U]) :C = coll
	override def sortWith(lt :(E, E) => Boolean) :C = coll
	override def sortBy[B](f :E => B)(implicit ord :Ordering[B]) :C = coll

	override def distinct :C = coll
	override def distinctBy[B](f :E => B) :C = coll
	override def reverse :C = coll

	override def view :SeqView[E] = EmptySeqOps.view
	override def reverseIterator :Iterator[E] = Iterator.empty

	override def corresponds[B](that :collection.Seq[B])(p :(E, B) => Boolean) = that.isEmpty

	override def sameElements[B >: E](that :IterableOnce[B]) :Boolean = that.toBasicOps.isEmpty

	protected override def clippedSlice(from :Int, until :Int) :C = coll
	protected override def emptySlice :C = coll
	protected override def hasFastSlice = true
}


@SerialVersionUID(Ver)
private[noresttherein] object EmptySeqOps {
	val searchResult = InsertionPoint(0)

	trait Variant[+CC[+_], +C <: CC[Nothing]]
		extends EmptyIterableOps.Variant[CC, C] with EmptySeqOps[Nothing, CC, C]

	type Generic[+CC[+_]] = Variant[CC, CC[Nothing]]

	val view :SeqView[Nothing] = new EmptyView

	@SerialVersionUID(Ver)
	private class EmptyView
		extends EmptyIterableOps.Generic[View] with SeqView[Nothing]
		   with EmptySeqOps.Generic[View] with SlicingOps[Nothing, SeqView[Nothing]]
   {
		override def prepended[B >: Nothing](elem :B) :SeqView[B] = super[SeqView].prepended(elem)
		override def appended[B >: Nothing](elem :B) :SeqView[B] = super[SeqView].appended(elem)

		override def reverse :SeqView[Nothing] = super[SeqView].reverse
		override def sorted[B >: Nothing](implicit ord :Ordering[B]) :SeqView[Nothing] = super[SeqView].sorted[B]

        protected override def clippedSlice(from :Int, until :Int) = this
		protected override def emptySlice = this
		private def readResolve :AnyRef = EmptySeqOps.view
	}
}




private[noresttherein] trait EmptyIndexedSeqOps[+E, +CC[_], +C]
	extends IndexedSeqOps[E, CC, C] with EmptyIterableOps[E, CC, C] with EmptySeqOps[E, CC, C]
{ this :C =>
	//overridden because EmptyIterableOps must be mixed in before IndexedSeqOps
	override def head :E = noSuch_!(toString + ".head")
	override def last :E = noSuch_!(toString + ".last")
	override def iterator :Iterator[E] = Iterator.empty
	override def slice(from :Int, until :Int) :C = this
	override def view :IndexedSeqView[E] = EmptyIndexedSeqOps.view
}


@SerialVersionUID(Ver)
private[noresttherein] object EmptyIndexedSeqOps {
	trait Variant[+CC[+_], +C <: CC[Nothing]]
		extends EmptySeqOps.Variant[CC, C] with EmptyIndexedSeqOps[Nothing, CC, C]
	{ this :C => }

	type Generic[+CC[+_]] = Variant[CC, CC[Nothing]]

	val view :IndexedSeqView[Nothing] = new EmptyView

	@SerialVersionUID(Ver)
	private class EmptyView
		extends EmptyIterableOps.Variant[View, View[Nothing]]
		   with IndexedSeqView[Nothing] with EmptyIndexedSeqOps.Variant[View, View[Nothing]]
		   with SlicingOps[Nothing, IndexedSeqView[Nothing]]
	{
//		override def length :Int = 0
//		override def apply(i :Int) :Nothing = outOfBounds_!(i, this)

		override def prepended[B >: Nothing](elem :B) :IndexedSeqView[B] = super[IndexedSeqView].prepended(elem)
		override def appended[B >: Nothing](elem :B) :IndexedSeqView[B] = super[IndexedSeqView].appended(elem)
		override def slice(from :Int, until :Int) :this.type = this
		override def reverse :this.type = this
		override def sorted[B >: Nothing](implicit ord :Ordering[B]) :this.type = this

		protected override def emptySlice = this
		protected override def clippedSlice(from :Int, until :Int) = this
		private def readResolve :AnyRef = EmptyIndexedSeqOps.view
	}
}






/** Base trait for specialized implementations of collections containing a single element. */
private[noresttherein] trait SingletonIterableOps[+E, +CC[_], +C] extends SugaredIterableOps[E, CC, C] {
	protected def outOfBounds(method :String, index :Int) :Nothing =
		outOfBounds_!(index.toString + " for " + errorString(this) + "." + method)

	override def knownSize :Int = 1

	protected def asGeneric[T >: E] :CC[T] = one(head)
	protected def one[T](elem :T) :CC[T] = (iterableFactory.newBuilder[T] hinted 1 addOne elem).result()
	protected def two[T](first :T, second :T) :CC[T] =
		(iterableFactory.newBuilder[T] hinted 1 addOne first addOne second).result()

	override def forall(p :E => Boolean) :Boolean = p(head)
	override def exists(p :E => Boolean) :Boolean = p(head)
	override def count(p :E => Boolean)  :Int     = if (p(head)) 1 else 0
	override def find(p :E => Boolean)   :Option[E] = if (p(head)) Some(head) else None
	override def corresponds[A](that :IterableOnce[A])(p :(E, A) => Boolean) :Boolean = that match {
		case it :Iterable[A] => it.sizeIs == 1 && p(head, it.head)
		case _ =>
			val i = that.iterator
			i.hasNext && p(head, i.next()) && !i.hasNext
	}

	override def sum[A >: E](implicit num :Numeric[A]) :A = head
	override def product[A >: E](implicit num :Numeric[A]) :A = head
	override def min[A >: E](implicit ord :Ordering[A]) :E = head
	override def max[A >: E](implicit ord :Ordering[A]) :E = head
	override def maxBy[A](f :E => A)(implicit cmp :Ordering[A]) :E = head
	override def minBy[A](f :E => A)(implicit cmp :Ordering[A]) :E = head
	override def minOption[A >: E](implicit ord :Ordering[A]) :Option[E] = headOption
	override def maxOption[A >: E](implicit ord :Ordering[A]) :Option[E] = headOption
	override def maxByOption[A](f :E => A)(implicit cmp :Ordering[A]) :Option[E] = headOption
	override def minByOption[A](f :E => A)(implicit cmp :Ordering[A]) :Option[E] = headOption

	override def collectFirst[A](pf :PartialFunction[E, A]) :Option[A] =
		pf.applyAndThenOrElse(head, Some(_), _ => None)

	override def foldLeft[A](z :A)(op :(A, E) => A) :A = op(z, head)
	override def foldRight[A](z :A)(op :(E, A) => A) :A = op(head, z)
	override def fold[A1 >: E](z :A1)(op :(A1, A1) => A1) :A1 = op(z, head)

	override def reduce[A >: E](op :(A, A) => A) :A = head
	override def reduceOption[A >: E](op :(A, A) => A) :Option[A] = headOption
	override def reduceLeft[A >: E](op :(A, E) => A) :A = head
	override def reduceRight[A >: E](op :(E, A) => A) :A = head
	override def reduceLeftOption[A >: E](op :(A, E) => A) :Option[A] = headOption
	override def reduceRightOption[A >: E](op :(E, A) => A) :Option[A] = headOption

	override def scan[A >: E](z :A)(op :(A, A) => A) :CC[A] = two(z, op(z, head))
	override def scanLeft[A](z :A)(op :(A, E) => A)  :CC[A] = two(z, op(z, head))
	override def scanRight[A](z :A)(op :(E, A) => A) :CC[A] = two(op(head, z), z)


	override def foreach[U](from :Int, until :Int)(f :E => U) :Unit =
		if (from <= 0 && until >= 1)
			f(head)
	override def foreach[U](f :E => U) :Unit = f(head)
	override def filter(pred :E => Boolean) :C = if (pred(head)) coll else empty
	override def filterNot(pred :E => Boolean) :C = if (pred(head)) empty else coll
	override def partition(p :E => Boolean) :(C, C) = if (p(head)) (coll, empty) else (empty, coll)

	override def splitAt(n :Int) :(C, C) = if (n <= 0) (empty, coll) else (coll, empty)
	override def take(n :Int) :C = if (n <= 0) empty else coll
	override def takeRight(n :Int) :C = if (n <= 0) empty else coll
	override def takeWhile(p :E => Boolean) :C = if (p(head)) coll else empty
	override def drop(n :Int) :C = if (n <= 0) coll else empty
	override def dropRight(n :Int) :C = if (n <= 0) coll else empty
	override def dropWhile(p :E => Boolean) :C = if (p(head)) empty else coll
	override def slice(from :Int, until :Int) :C = if (until <= from | until <= 0 | from >= 1) empty else coll
	override def span(p :E => Boolean) :(C, C) = if (p(head)) (coll, empty) else (empty, coll)

	override def removed(index :Int) :C = if (index == 0) empty else outOfBounds("removed", index)
	override def removed(from :Int, until :Int) :C =
		if (from > 0 | until <= 0 | from == until) coll else empty

	override def grouped(size :Int) :Iterator[C] =
		if (size <= 0) illegal_!("size=" +  size) else Iterator.single(coll)
	override def sliding(size :Int, step :Int) :Iterator[C] =
		if (size <= 0 | step <= 0) illegal_!("size=" + size + ", step=" + step)
		else Iterator.single(coll)

	override def last :E = head
	override def headOption :Option[E] = Some(head)
	override def lastOption :Option[E] = headOption
	override def tail :C = empty
	override def init :C = empty
	override def tails :Iterator[C] = Iterator.single(empty)
	override def inits :Iterator[C] = Iterator.single(empty)

	override def tapEach[U](f :E => U) :C = { f(head); coll }

	override def collect[A](pf :PartialFunction[E, A]) :CC[A] =
		pf.applyAndThenOrElse(head, one(_), _ => iterableFactory.empty)

	override def partitionMap[A1, A2](f :E => Either[A1, A2]) :(CC[A1], CC[A2]) = f(head) match {
		case Left(a1) => (one(a1), iterableFactory.empty)
		case Right(a2) => (iterableFactory.empty, one(a2))
	}
	override def groupMap[K, V](key :E => K)(f :E => V) :Map[K, CC[V]] =
		Map.empty[K, CC[V]].updated(key(head), one(f(head)))

	override def groupMapReduce[K, A](key :E => K)(f :E => A)(reduce :(A, A) => A) :Map[K, A] =
		Map.empty[K, A].updated(key(head), f(head))

	override def groupBy[K](f :E => K) :Map[K, C] = Map.empty.updated(f(head), coll)

	override def map[B](f :E => B) :CC[B] = one(f(head))
	override def flatMap[A](f :E => IterableOnce[A]) :CC[A] = iterableFactory.from(f(head))
	override def flatten[A](implicit asIterable :E => IterableOnce[A]) :CC[A] = iterableFactory.from(head)

	override def transpose[A](implicit asIterable :E => Iterable[A]) :CC[CC[A] @uncheckedVariance] =
		iterableFactory.from(asIterable(head).view.map(one))

	override def zip[A](that :IterableOnce[A]) :CC[(E @uncheckedVariance, A)] =
		that match {
			case empty :Iterable[_] if empty.isEmpty => iterableFactory.empty
			case it :Iterable[A] => one((head, it.head))
			case _ =>
				val i = that.iterator
				if (!i.hasNext) iterableFactory.empty
				else one((head, i.next()))
		}
	override def zipWithIndex :CC[(E @uncheckedVariance, Int)] = one((head, 0))

	override def zipAll[A1 >: E, A](that :Iterable[A], thisElem :A1, thatElem :A) :CC[(A1, A)] =
		that match {
			case empty :Iterable[A] if empty.isEmpty => one((head, thatElem))
			case it :Iterable[A] if it.sizeIs == 1 => one((head, it.head))
			case _ => super.zipAll(that, thisElem, thatElem)
		}

	override def unzip[A1, A2](implicit asPair :E => (A1, A2)) :(CC[A1], CC[A2]) = (one(head._1), one(head._2))
	override def unzip3[A1, A2, A3](implicit asTriple :E => (A1, A2, A3)) :(CC[A1], CC[A2], CC[A3]) =
		(one(head._1), one(head._2), one(head._3))


	override def copyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int =
		if (util.rangeCheck(1, from, xs, start, len))
			0
		else {
			xs(start) = head; 1
		}
	override def copyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int =
		if (len <= 0 || start >= xs.length)
			0
		else {
			xs(start) = head; 1
		}

	override def cyclicCopyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int =
		if (len <= 0 | from > 0 || xs.length == 0)
			0
		else {
			xs(start % xs.length) = head
			1
		}
	override def cyclicCopyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int =
		if (len <= 0 || xs.length == 0)
			0
		else {
			xs(start % xs.length) = head
			1
		}


	override def view :View[E] = new SingletonIterableOps.View(head)
	override def iterator :Iterator[E] = Iterator.single(head)
	override def toList :List[E] = head::Nil
	override def toVector :Vector[E] = Vector.empty :+ head
	override def toSet[A >: E] :Set[A] = this match {
		case set :Set[A @unchecked] => set
		case _                      => Set.empty[A] + head
	}
	override def toMap[K, V](implicit ev :E <:< (K, V)) :Map[K, V] = this match {
		case map :Map[K, V] @unchecked => map
		case _                         => Map.empty[K, V] + head
	}
	override def toArray[A >: E :ClassTag] :Array[A] = Array.one[A](head)

	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) = Stepper1(head)

	override def addString(b :StringBuilder, start :String, sep :String, end :String) :b.type =
		b ++= start ++= head.toString ++= end

}


@SerialVersionUID(Ver)
private[noresttherein] object SingletonIterableOps {
	trait Variant[+E, +CC[+_], +C <: CC[E]] extends SingletonIterableOps[E, CC, C] {
		protected override def asGeneric[T >: E] :CC[T] = coll
	}
	type Generic[+E, +CC[+_]] = Variant[E, CC, CC[E]]

	private class View[E](override val head :E)
		extends AbstractView[E] with SingletonIterableOps[E, collection.View, collection.View[E]]

	def view[E](elem :E) :collection.View[E] = new View[E](elem)
}




private[noresttherein] sealed trait SingletonNonSeqOps[+E, +CC[_], +C] extends SingletonIterableOps[E, CC, C] {
	override def size :Int = 1
	override def isEmpty :Boolean = false

	override def concat[B >: E](suffix :IterableOnce[B]) :CC[B] =
		if (suffix.toBasicOps.isEmpty) asGeneric else super.concat(suffix)
	override def toSeq :Seq[E] = head::Nil
	override def toIndexedSeq :IndexedSeq[E] = RelayArray.one(head)
}


@SerialVersionUID(Ver)
private[noresttherein] object SingletonNonSeqOps {
	trait Variant[+E, +CC[+_], +C <: CC[E]]
		extends SingletonIterableOps.Variant[E, CC, C] with SingletonNonSeqOps[E, CC, C]

	type Generic[+E, +CC[+_]] = Variant[E, CC, CC[E]]
}




private[noresttherein] trait SingletonSeqOps[+E, +CC[_], +C]
	extends SeqOps[E, CC, C] with SugaredSeqOps[E, CC, C] with SingletonIterableOps[E, CC, C] with SlicingOps[E, C]
{
	private def outOfBounds(i :Int) = outOfBounds_!(i.toString + " out of 1")

	override def length :Int = 1
	override def apply(i :Int) :E = if (i == 0) head else outOfBounds(i)
	override def isDefinedAt(idx :Int) = idx == 0

	override def search[B >: E](elem :B)(implicit ord :Ordering[B]) :SearchResult = searchImpl(elem, 0, 1)
	override def search[B >: E](elem :B, from :Int, to :Int)(implicit ord :Ordering[B]) :SearchResult =
		searchImpl(elem, from, to)

	@tailrec private[this] def searchImpl[B >: E](elem :B, from :Int, to :Int)
	                                             (implicit ord :Ordering[B]) :SearchResult =
	{
		if (from < 0)
			searchImpl(elem, 0, to)
		else if (to > 1)
			searchImpl(elem, from, length)
		else if (to <= from)
			InsertionPoint(from)
		else
			ord.compare(elem, head) match {
				case 0          => Found(0)
				case n if n < 0 => InsertionPoint(0)
				case _          => InsertionPoint(1)
			}
	}

	override def findLast(p :E => Boolean) :Option[E] = find(p)
	override def indexWhere(p :E => Boolean, from :Int) :Int = if (from <= 0 && p(head)) 0 else -1
	override def indexOf[B >: E](elem :B, from :Int) :Int = if (from <= 0 && elem == head) 0 else -1
	override def lastIndexWhere(p :E => Boolean, end :Int) :Int = if (end >= 0 && p(head)) 0 else -1
	override def lastIndexOf[B >: E](elem :B, end :Int) :Int = if (end >= 0 && elem == head) 0 else -1

	override def indexOfSlice[B >: E](that :collection.Seq[B], from :Int) :Int =
		if (from <= 0)
			if (that.isEmpty || that.sizeIs == 1 && head == that.head) 0
			else -1
		else if (from == 1 && that.isEmpty)
			1
		else
			-1

	override def lastIndexOfSlice[B >: E](that :collection.Seq[B], end :Int) :Int =
		if (that.isEmpty)
			if (end >= 1) 1 else if (end == 0) 0 else -1
		else if (end >= 0 && that.sizeIs == 1 && head == that.head)
			0
		else
			-1

	override def segmentLength(p :E => Boolean, from :Int) :Int = if (from <= 0 && p(head)) 1 else 0
	override def endsWith[B >: E](that :Iterable[B]) :Boolean = that.isEmpty || that.sizeIs == 1 && that.head == head
	override def startsWith[B >: E](that :IterableOnce[B], offset :Int) :Boolean =
		offset match {
			case 1 => that.toBasicOps.isEmpty
			case 0 => that.toBasicOps.isEmpty || sameElements(that)
			case _ => false
		}

	override def permutations :Iterator[C] = Iterator.single(coll)
	override def combinations(n :Int) :Iterator[C] = n match {
		case 0 => Iterator.single(empty)
		case 1 => Iterator.single(coll)
		case _ => Iterator.empty
	}


	override def diff[B >: E](that :collection.Seq[B]) :C = if (that.contains(head)) empty else coll
	override def intersect[B >: E](that :collection.Seq[B]) :C = if (that.contains(head)) coll else empty
//	override def patch[B >: E](from :Int, other :IterableOnce[B], replaced :Int) :CC[B] =
//		iterableFactory.from(other)
	override def updated[U >: E](index :Int, elem :U) :CC[U] = if (index != 0) outOfBounds(index) else one(elem)
	override def updatedAll[U >: E](index :Int, elems :IterableOnce[U]) :CC[U] = elems match {
		case _ if index < 0 | index > 1 =>
			outOfBounds_!(errorString(this) + ".updatedAll(" + index + ", " + errorString(elems) + ")")
		case _ if elems.knownSize == 0 =>
			asGeneric
		case view  :View[U] => updatedAll(index, view.iterator)
		case items :Iterable[U] if items.isEmpty => asGeneric
		case items :Iterable[U] if index == 0 && items.sizeIs == 1 => one(items.head)
		case _ =>
			val it = elems.iterator
			if (!it.hasNext)
				asGeneric
			else {
				val first = it.next()
				if (it.hasNext)
					outOfBounds_!(errorString(this) + ".updatedAll(" + index + ", " + errorString(elems) + "/size>=1)")
				one(first)
			}
	}
	override def updatedAll[U >: E](index :Int, first :U, second :U, rest :U*) :CC[U] =
		outOfBounds_!(errorString(this) + ".updatedAll(" + index + ", _, _, " + errorString(rest) + ")")

	override def overwritten[U >: E](index :Int, elems :IterableOnce[U]) :CC[U] = {
		val s = elems.knownSize
		elems match {
			case _ if index >= 1 | s == 0 | s > 0 & index < 0 & index <= -s => asGeneric
			case view  :View[U]                                             => overwritten(index, view.iterator)
			case items :Iterable[U] if items.isEmpty                        => asGeneric
			case items :Iterable[U] if index == 0 && items.sizeIs == 1      => one(items.head)
			case _ if index == Int.MinValue                                 => asGeneric
			case _ =>
				var it = elems.iterator
				if (index < 0)
					it = it.drop(-index)
				if (it.hasNext) one(it.next()) else asGeneric
		}
	}
	override def overwritten[U >: E](index :Int, first :U, second :U, rest :U*) :CC[U] = math.min(index, 1) match {
		case  1           => asGeneric
		case  0           => one(first)
		case -1           => one(second)
		case -2           => overwritten(0, rest)
		case  _ => //index < -2
			val k = rest.knownSize
			val toDrop = -(index + 2)
			rest match { //index < -2
				case _ if k == 0 | k > 0 & index <= -k - 2        => asGeneric
				case HasFastSlice(_) | _ :collection.LinearSeq[_] => overwritten(0, rest.drop(toDrop))
				case _                                            => overwritten(0, rest.iterator.drop(toDrop))
			}
	}

	override def inserted[U >: E](index :Int, elem :U) :CC[U] = index match {
		case 0 => two(elem, head)
		case 1 => two(head, elem)
		case _ => outOfBounds_!(errorString(this) + ".inserted(" + index + ", _)")
	}

	override def insertedAll[U >: E](index :Int, elems :IterableOnce[U]) :CC[U] = elems match {
		case _ if index < 0 | index > 1 =>
			outOfBounds_!(errorString(this) + ".insertedAll(" + index + ", " + errorString(elems) + ")")
		case _ if elems.knownSize == 0           => asGeneric
		case view :View[U] if index == 0         => iterableFactory.from(view.iterator :+ head)
		case view :View[U]                       => iterableFactory.from(head +: view.iterator)
		case items :Iterable[U] if items.isEmpty => asGeneric
		case _ if index == 0                     => iterableFactory.from(elems.iterator :+ head)
		case _                                   => iterableFactory.from(head +: elems.iterator)
	}

	override def prepended[B >: E](elem :B) :CC[B] = two(elem, head)
	override def appended[B >: E](elem :B)  :CC[B] = two(head, elem)
	override def prependedAll[B >: E](prefix :IterableOnce[B]) :CC[B] =
		if (prefix.toBasicOps.isEmpty) asGeneric else super.prependedAll(prefix)

	override def appendedAll[B >: E](suffix :IterableOnce[B]) :CC[B] =
		if (suffix.toBasicOps.isEmpty) asGeneric else super.appendedAll(suffix)

	override def sorted[B >: E](implicit ord :Ordering[B]) :C = coll
	override def sortWith(lt :(E, E) => Boolean) :C = coll
	override def sortBy[B](f :E => B)(implicit ord :Ordering[B]) :C = coll

	override def distinct :C = coll
	override def distinctBy[B](f :E => B) :C = coll
	override def reverse :C = coll

	override def reverseIterator :Iterator[E] = Iterator.single(head)

	override def view :SeqView[E] = new SingletonSeqOps.View[E](head)

	override def corresponds[B](that :collection.Seq[B])(p :(E, B) => Boolean) = that.isEmpty

	override def sameElements[B >: E](that :IterableOnce[B]) :Boolean = that match {
		case it :Iterable[B] => it.sizeIs == 1 && head == it.head
		case _ =>
			val i = that.iterator
			i.hasNext && head == i.next() && !i.hasNext
	}

	protected override def clippedSlice(from :Int, until :Int) :C = if (from == 0 & until > 0) coll else empty
	protected override def emptySlice :C = empty
	protected override def hasFastSlice = true
}


@SerialVersionUID(Ver)
private[noresttherein] object SingletonSeqOps {
	trait Variant[+E, +CC[+_], +C <: CC[E]]
		extends SingletonIterableOps.Variant[E, CC, C] with SingletonSeqOps[E, CC, C]

	type Generic[+E, +CC[+_]] = Variant[E, CC, CC[E]]

	private class View[+E](override val head :E)
		extends SingletonIterableOps.Generic[E, collection.View] with SeqView[E]
		   with Generic[E, collection.View] with SlicingOps[E, SeqView[E]]
	{
		override def prepended[A >: E](elem :A) :SeqView[A] = super[SeqView].prepended(elem)
		override def appended[A >: E](elem :A) :SeqView[A] = super[SeqView].appended(elem)
		override def reverse :this.type = this
		override def sorted[A >: E](implicit ord :Ordering[A]) :this.type = this

		protected override def emptySlice = EmptySeqOps.view
		protected override def clippedSlice(from :Int, until :Int) = this
	}//with Generic[E, SeqView]

	def view[E](elem :E) :SeqView[E] = new View(elem)
}




private[noresttherein] trait SingletonIndexedSeqOps[+E, +CC[_], +C]
	extends IndexedSeqOps[E, CC, C] with SingletonIterableOps[E, CC, C] with SingletonSeqOps[E, CC, C]
{
	override def iterator :Iterator[E] = Iterator.single(head)
	override def view :IndexedSeqView[E] = new SingletonIndexedSeqOps.View(head)
}


@SerialVersionUID(Ver)
private[noresttherein] object SingletonIndexedSeqOps {
	trait Variant[+E, +CC[+_], +C <: CC[E]]
		extends SingletonSeqOps.Variant[E, CC, C] with SingletonIndexedSeqOps[E, CC, C]

	type Generic[+E, +CC[+_]] = Variant[E, CC, CC[E]]

	private class View[+E](override val head :E)
		extends SingletonIterableOps.Generic[E, collection.View] with IndexedSeqView[E]
		   with Generic[E, collection.View] with SlicingOps[E, IndexedSeqView[E]]
	{
//		override def empty :IndexedSeqView[E] = EmptyIndexedSeqOps.view
		override def prepended[A >: E](elem :A) :IndexedSeqView[A] = super[IndexedSeqView].prepended(elem)
		override def appended[A >: E](elem :A) :IndexedSeqView[A] = super[IndexedSeqView].appended(elem)
		override def reverse :this.type = this
		override def sorted[A >: E](implicit ord :Ordering[A]) :this.type = this

		override def slice(from :Int, until :Int) :IndexedSeqView[E] = this
		protected override def emptySlice = EmptyIndexedSeqOps.view
		protected override def clippedSlice(from :Int, until :Int) = this
	}

	def view[E](elem :E) :IndexedSeqView[E] = new View(elem)
}
