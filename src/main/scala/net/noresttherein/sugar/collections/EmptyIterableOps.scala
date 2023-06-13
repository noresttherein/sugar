package net.noresttherein.sugar.collections

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{IndexedSeqView, IterableFactory, IterableOps, SeqView, Stepper, StepperShape, View}
import scala.collection.Searching.{Found, InsertionPoint, SearchResult}
import scala.collection.immutable.{IndexedSeqOps, SeqOps}
import scala.collection.mutable.Builder
import scala.reflect.ClassTag

import net.noresttherein.sugar.collections.EmptySeqOps.searchResult
import net.noresttherein.sugar.extensions.{ArrayObjectExtension, BuilderExtension, IterableOnceExtension, PartialFunctionExtension}




/** Base trait for specialized implementations of empty collections. */
private[noresttherein] trait EmptyIterableOps[+E, +CC[_], +C] extends SugaredIterableOps[E, CC, C] {
	this :C =>
	override def knownSize :Int = 0
	//This method is problematic, because we normally want to mix this trait late to override standard implementations,
	// but it is final in IndexedSeqOps, which means the latter must be mixed in after this trait.
	// The same goes for concat.
	override def size      :Int = 0
	override def isEmpty  :Boolean = true

	private def noSuch(method :String) = throw new NoSuchElementException(toString + "." + method)
	protected def genericEmpty[X] :CC[X] = iterableFactory.empty
	protected def one[T](elem :T) :CC[T] = (iterableFactory.newBuilder[T] hinted 1 addOne elem).result()
	protected def double :(C, C) = (this, this)

	private def unsupported(method :String) = throw new UnsupportedOperationException(toString + '.' + method)

	override def forall(p :E => Boolean) :Boolean = true
	override def exists(p :E => Boolean) :Boolean = false
	override def count(p :E => Boolean)  :Int     = 0
	override def find(p :E => Boolean)   :Option[E] = None
	override def corresponds[B](that :IterableOnce[B])(p :(E, B) => Boolean) :Boolean = that.toIterableOnceOps.isEmpty

	override def sum[B >: E](implicit num :Numeric[B]) :B = num.zero
	override def product[B >: E](implicit num :Numeric[B]) :B = num.one
	override def min[B >: E](implicit ord :Ordering[B]) :E = unsupported("min")
	override def max[B >: E](implicit ord :Ordering[B]) :E = unsupported("max")
	override def maxBy[B](f :E => B)(implicit cmp :Ordering[B]) :E = unsupported("maxBy")
	override def minBy[B](f :E => B)(implicit cmp :Ordering[B]) :E = unsupported("minBy")
	override def minOption[B >: E](implicit ord :Ordering[B]) :Option[E] = None
	override def maxOption[B >: E](implicit ord :Ordering[B]) :Option[E] = None
	override def maxByOption[B](f :E => B)(implicit cmp :Ordering[B]) :Option[E] = None
	override def minByOption[B](f :E => B)(implicit cmp :Ordering[B]) :Option[E] = None

	override def collectFirst[B](pf :PartialFunction[E, B]) :Option[B] = None
	override def foldLeft[B](z :B)(op :(B, E) => B) :B = z
	override def foldRight[B](z :B)(op :(E, B) => B) :B = z
	override def fold[A1 >: E](z :A1)(op :(A1, A1) => A1) :A1 = z

	override def reduce[B >: E](op :(B, B) => B) :B = unsupported("reduce")
	override def reduceOption[B >: E](op :(B, B) => B) :Option[B] = None
	override def reduceLeft[B >: E](op :(B, E) => B) :B = unsupported("reduceLeft")
	override def reduceRight[B >: E](op :(E, B) => B) :B = unsupported("reduceRight")
	override def reduceLeftOption[B >: E](op :(B, E) => B) :Option[B] = None
	override def reduceRightOption[B >: E](op :(E, B) => B) :Option[B] = None

	override def scan[B >: E](z :B)(op :(B, B) => B) :CC[B] = one(z)
	override def scanLeft[B](z :B)(op :(B, E) => B)  :CC[B] = one(z)
	override def scanRight[B](z :B)(op :(E, B) => B) :CC[B] = one(z)


	override def empty :C = this

	override def foreach[U](f :E => U) :Unit = ()
	override def foreach[U](from :Int, until :Int)(f :E => U) :Unit = ()
	override def filter(pred :E => Boolean) :C = this
	override def filterNot(pred :E => Boolean) :C = this
	override def partition(p :E => Boolean) :(C, C) = double

	override def splitAt(n :Int) :(C, C) = double
	override def take(n :Int) :C = this
	override def takeRight(n :Int) :C = this
	override def takeWhile(p :E => Boolean) :C = this
	override def drop(n :Int) :C = this
	override def dropRight(n :Int) :C = this
	override def dropWhile(p :E => Boolean) :C = this
	override def slice(from :Int, until :Int) :C = this
	override def span(p :E => Boolean) :(C, C) = double

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

	override def tapEach[U](f :E => U) :C = this

	override def collect[B](pf :PartialFunction[E, B]) :CC[B] = genericEmpty
	override def partitionMap[A1, A2](f :E => Either[A1, A2]) :(CC[A1], CC[A2]) = (genericEmpty, genericEmpty)

	override def groupMap[K, B](key :E => K)(f :E => B) :Map[K, CC[B]] = Map.empty
	override def groupMapReduce[K, B](key :E => K)(f :E => B)(reduce :(B, B) => B) :Map[K, B] =
		Map.empty
	override def groupBy[K](f :E => K) :Map[K, C] = Map.empty

	override def map[B](f :E => B) :CC[B] = genericEmpty
	override def flatMap[B](f :E => IterableOnce[B]) :CC[B] = genericEmpty
	override def flatten[B](implicit asIterable :E => IterableOnce[B]) :CC[B] = genericEmpty

	override def transpose[A](implicit asIterable :E => Iterable[A]) :CC[CC[A] @uncheckedVariance] = genericEmpty

	override def concat[B >: E](suffix :IterableOnce[B]) :CC[B] = iterableFactory.from(suffix)

	override def zip[B](that :IterableOnce[B]) :CC[(E @uncheckedVariance, B)] = genericEmpty
	override def zipWithIndex :CC[(E @uncheckedVariance, Int)] = genericEmpty
	override def zipAll[A1 >: E, B](that :Iterable[B], thisElem :A1, thatElem :B) :CC[(A1, B)] =
		if (that.isEmpty) genericEmpty
		else iterableFactory.from(that.view.map((thisElem, _)))

	override def unzip[A1, A2](implicit asPair :E => (A1, A2)) :(CC[A1], CC[A2]) = (genericEmpty, genericEmpty)
	override def unzip3[A1, A2, A3](implicit asTriple :E => (A1, A2, A3)) :(CC[A1], CC[A2], CC[A3]) =
		(genericEmpty, genericEmpty, genericEmpty)

	override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int = 0
	override def copyRangeToArray[B >: E](xs :Array[B], start :Int, from :Int, until :Int) :Int = 0


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
	trait Variant[+CC[+_], +C <: CC[Nothing]] extends EmptyIterableOps[Nothing, CC, C] { this :C =>
		override def genericEmpty[X] :CC[X] = this
	}

	val view :View[Nothing] = View.empty
}



private[noresttherein] trait EmptySeqOps[+E, +CC[_], +C]
	extends EmptyIterableOps[E, CC, C] with SeqOps[E, CC, C]
{ this :C =>
	private def outOfBounds(i :Int) = throw new IndexOutOfBoundsException(i.toString + " out of 0")
	private def noSuch(method :String) = throw new NoSuchElementException(toString + "." + method)

	//overridden because EmptyIterableOps must be mixed in before SeqOps
	override def isEmpty :Boolean = true

	override def length :Int = 0
	override def head   :E = noSuch("head")
	override def apply(i :Int) :E = outOfBounds(i)
	override def updated[B >: E](index :Int, elem :B) :CC[B] = outOfBounds(index)
	override def isDefinedAt(idx :Int) = false

	override def search[B >: E](elem :B)(implicit ord :Ordering[B]) :SearchResult = searchResult
	override def search[B >: E](elem :B, from :Int, to :Int)(implicit ord :Ordering[B]) :SearchResult =
		if (from < to) searchResult else InsertionPoint(from)

	override def findLast(p :E => Boolean) :Option[E] = None
	override def indexWhere(p :E => Boolean, from :Int) :Int = -1
	override def indexOf[B >: E](elem :B, from :Int) :Int = -1
	override def lastIndexOf[B >: E](elem :B, end :Int) :Int = -1
	override def lastIndexWhere(p :E => Boolean, end :Int) :Int = -1

	override def indexOfSlice[B >: E](that :collection.Seq[B], from :Int) :Int =
		if (from <= 0 && that.isEmpty) 0 else -1

	override def lastIndexOfSlice[B >: E](that :collection.Seq[B], end :Int) :Int =
		if (end >= 0 && that.isEmpty) 0 else -1

	override def contains[A1 >: E](elem :A1) = false
	override def containsSlice[B >: E](that :collection.Seq[B]) = false

	override def segmentLength(p :E => Boolean, from :Int) :Int = 0
	override def endsWith[B >: E](that :Iterable[B]) :Boolean = that.isEmpty
	override def startsWith[B >: E](that :IterableOnce[B], offset :Int) :Boolean =
		offset == 0 && that.toIterableOnceOps.isEmpty

	override def padTo[B >: E](len :Int, elem :B) = iterableFactory.fill(len)(elem)

	override def permutations :Iterator[C] = Iterator.single(this)
	override def combinations(n :Int) :Iterator[C] = if (n == 0) Iterator.single(this) else Iterator.empty


	override def diff[B >: E](that :collection.Seq[B]) :C = this
	override def intersect[B >: E](that :collection.Seq[B]) :C = this
	override def patch[B >: E](from :Int, other :IterableOnce[B], replaced :Int) :CC[B] =
		iterableFactory.from(other)

	override def prepended[B >: E](elem :B) :CC[B] = one(elem)
	override def appended[B >: E](elem :B)  :CC[B] = one(elem)
	override def prependedAll[B >: E](prefix :IterableOnce[B]) :CC[B] = iterableFactory.from(prefix)
	override def appendedAll[B >: E](suffix :IterableOnce[B]) :CC[B] = iterableFactory.from(suffix)

	override def sorted[B >: E](implicit ord :Ordering[B]) :C = this
	override def sortWith(lt :(E, E) => Boolean) :C = this
	override def sortBy[B](f :E => B)(implicit ord :Ordering[B]) :C = this

	override def distinct :C = this
	override def distinctBy[B](f :E => B) :C = this
	override def reverse :C = this

	override def view :SeqView[E] = EmptySeqOps.view
	override def reverseIterator :Iterator[E] = Iterator.empty

	override def corresponds[B](that :collection.Seq[B])(p :(E, B) => Boolean) = that.isEmpty

	override def sameElements[B >: E](that :IterableOnce[B]) :Boolean = that.toIterableOnceOps.isEmpty

}


@SerialVersionUID(Ver)
private[noresttherein] object EmptySeqOps {
	val searchResult = InsertionPoint(0)

	trait Variant[+CC[+_], +C <: CC[Nothing]]
		extends EmptyIterableOps.Variant[CC, C] with EmptySeqOps[Nothing, CC, C]
	{ this :C => }

	val view :SeqView[Nothing] = new EmptyView

	@SerialVersionUID(Ver)
	private class EmptyView
		extends EmptyIterableOps.Variant[View, View[Nothing]]
			with SeqView[Nothing] with EmptySeqOps.Variant[View, View[Nothing]]
	{
		override def prepended[B >: Nothing](elem :B) :SeqView[B] = super[SeqView].prepended(elem)
		override def appended[B >: Nothing](elem :B) :SeqView[B] = super[SeqView].appended(elem)

		override def reverse :SeqView[Nothing] = super[SeqView].reverse
		override def sorted[B >: Nothing](implicit ord :Ordering[B]) :SeqView[Nothing] = super[SeqView].sorted[B]
		private def readResolve = EmptySeqOps.view
	}
}




private[noresttherein] trait EmptyIndexedSeqOps[+E, +CC[_], +C]
	extends EmptyIterableOps[E, CC, C] with IndexedSeqOps[E, CC, C] with EmptySeqOps[E, CC, C]
{ this :C =>
	//overridden because EmptyIterableOps must be mixed in before IndexedSeqOps
	override def head :E = throw new NoSuchElementException(toString + ".head")
	override def last :E = throw new NoSuchElementException(toString + ".last")
	override def iterator :Iterator[E] = Iterator.empty
	override def slice(from :Int, until :Int) :C = this
	override def view :IndexedSeqView[E] = EmptyIndexedSeqOps.view
}


@SerialVersionUID(Ver)
private[noresttherein] object EmptyIndexedSeqOps {
	trait Variant[+CC[+_], +C <: CC[Nothing]]
		extends EmptySeqOps.Variant[CC, C] with EmptyIndexedSeqOps[Nothing, CC, C]
	{ this :C => }

	val view :IndexedSeqView[Nothing] = new EmptyView

	@SerialVersionUID(Ver)
	private class EmptyView
		extends EmptyIterableOps.Variant[View, View[Nothing]]
		   with IndexedSeqView[Nothing] with EmptyIndexedSeqOps.Variant[View, View[Nothing]]
	{
		override def prepended[B >: Nothing](elem :B) :IndexedSeqView[B] = super[IndexedSeqView].prepended(elem)
		override def appended[B >: Nothing](elem :B) :IndexedSeqView[B] = super[IndexedSeqView].appended(elem)
		override def slice(from :Int, until :Int) :this.type = this
		override def reverse :this.type = this
		override def sorted[B >: Nothing](implicit ord :Ordering[B]) :this.type = this

		private def readResolve = EmptyIndexedSeqOps.view
	}
}






/** Base trait for specialized implementations of collections containing a single element. */
private[noresttherein] trait SingletonIterableOps[+E, +CC[_], +C] extends SugaredIterableOps[E, CC, C] { this :C =>
	override def knownSize :Int = 1
	override def size      :Int = 1
	override def isEmpty  :Boolean = false

	protected def asGeneric[T >: E] :CC[T] = one(head)
	protected def one[T](elem :T) :CC[T] = (iterableFactory.newBuilder[T] hinted 1 addOne elem).result()
	protected def two[T](first :T, second :T) :CC[T] =
		(iterableFactory.newBuilder[T] hinted 1 addOne first addOne second).result()

	override def forall(p :E => Boolean) :Boolean = p(head)
	override def exists(p :E => Boolean) :Boolean = p(head)
	override def count(p :E => Boolean)  :Int     = if (p(head)) 1 else 0
	override def find(p :E => Boolean)   :Option[E] = if (p(head)) Some(head) else None
	override def corresponds[B](that :IterableOnce[B])(p :(E, B) => Boolean) :Boolean = that match {
		case it :Iterable[B] => it.sizeIs == 1 && p(head, it.head)
		case _ =>
			val i = that.iterator
			i.hasNext && p(head, i.next()) && !i.hasNext
	}

	override def sum[B >: E](implicit num :Numeric[B]) :B = head
	override def product[B >: E](implicit num :Numeric[B]) :B = head
	override def min[B >: E](implicit ord :Ordering[B]) :E = head
	override def max[B >: E](implicit ord :Ordering[B]) :E = head
	override def maxBy[B](f :E => B)(implicit cmp :Ordering[B]) :E = head
	override def minBy[B](f :E => B)(implicit cmp :Ordering[B]) :E = head
	override def minOption[B >: E](implicit ord :Ordering[B]) :Option[E] = headOption
	override def maxOption[B >: E](implicit ord :Ordering[B]) :Option[E] = headOption
	override def maxByOption[B](f :E => B)(implicit cmp :Ordering[B]) :Option[E] = headOption
	override def minByOption[B](f :E => B)(implicit cmp :Ordering[B]) :Option[E] = headOption

	override def collectFirst[B](pf :PartialFunction[E, B]) :Option[B] =
		pf.applyAndThenOrElse(head, Some(_), _ => None)

	override def foldLeft[B](z :B)(op :(B, E) => B) :B = op(z, head)
	override def foldRight[B](z :B)(op :(E, B) => B) :B = op(head, z)
	override def fold[A1 >: E](z :A1)(op :(A1, A1) => A1) :A1 = op(z, head)

	override def reduce[B >: E](op :(B, B) => B) :B = head
	override def reduceOption[B >: E](op :(B, B) => B) :Option[B] = headOption
	override def reduceLeft[B >: E](op :(B, E) => B) :B = head
	override def reduceRight[B >: E](op :(E, B) => B) :B = head
	override def reduceLeftOption[B >: E](op :(B, E) => B) :Option[B] = headOption
	override def reduceRightOption[B >: E](op :(E, B) => B) :Option[B] = headOption

	override def scan[B >: E](z :B)(op :(B, B) => B) :CC[B] = two(z, op(z, head))
	override def scanLeft[B](z :B)(op :(B, E) => B)  :CC[B] = two(z, op(z, head))
	override def scanRight[B](z :B)(op :(E, B) => B) :CC[B] = two(op(head, z), z)


	override def foreach[U](from :Int, until :Int)(f :E => U) :Unit =
		if (from <= 0 && until >= 1)
			f(head)
	override def foreach[U](f :E => U) :Unit = f(head)
	override def filter(pred :E => Boolean) :C = if (pred(head)) this else empty
	override def filterNot(pred :E => Boolean) :C = if (pred(head)) empty else this
	override def partition(p :E => Boolean) :(C, C) = if (p(head)) (this, empty) else (empty, this)

	override def splitAt(n :Int) :(C, C) = if (n <= 0) (empty, this) else (this, empty)
	override def take(n :Int) :C = if (n <= 0) empty else this
	override def takeRight(n :Int) :C = if (n <= 0) empty else this
	override def takeWhile(p :E => Boolean) :C = if (p(head)) this else empty
	override def drop(n :Int) :C = if (n <= 0) this else empty
	override def dropRight(n :Int) :C = if (n <= 0) this else empty
	override def dropWhile(p :E => Boolean) :C = if (p(head)) empty else this
	override def slice(from :Int, until :Int) :C = if (until <= from | until < 0 | from >= 1) empty else this
	override def span(p :E => Boolean) :(C, C) = if (p(head)) (this, empty) else (empty, this)

	override def grouped(size :Int) :Iterator[C] =
		if (size <= 0) throw new IllegalArgumentException("size=" +  size) else Iterator.single(this)
	override def sliding(size :Int, step :Int) :Iterator[C] =
		if (size <= 0 | step <= 0) throw new IllegalArgumentException("size=" + size + ", step=" + step)
		else Iterator.single(this)

	override def last :E = head
	override def headOption :Option[E] = Some(head)
	override def lastOption :Option[E] = headOption
	override def tail :C = empty
	override def init :C = empty
	override def tails :Iterator[C] = Iterator.single(empty)
	override def inits :Iterator[C] = Iterator.single(empty)

	override def tapEach[U](f :E => U) :C = { f(head); this }

	override def collect[B](pf :PartialFunction[E, B]) :CC[B] =
		pf.applyAndThenOrElse(head, one(_), _ => iterableFactory.empty)

	override def partitionMap[A1, A2](f :E => Either[A1, A2]) :(CC[A1], CC[A2]) = f(head) match {
		case Left(a1) => (one(a1), iterableFactory.empty)
		case Right(a2) => (iterableFactory.empty, one(a2))
	}
	override def groupMap[K, B](key :E => K)(f :E => B) :Map[K, CC[B]] =
		Map.empty[K, CC[B]].updated(key(head), one(f(head)))

	override def groupMapReduce[K, B](key :E => K)(f :E => B)(reduce :(B, B) => B) :Map[K, B] =
		Map.empty[K, B].updated(key(head), f(head))

	override def groupBy[K](f :E => K) :Map[K, C] = Map.empty.updated(f(head), this)

	override def map[B](f :E => B) :CC[B] = one(f(head))
	override def flatMap[B](f :E => IterableOnce[B]) :CC[B] = iterableFactory.from(f(head))
	override def flatten[B](implicit asIterable :E => IterableOnce[B]) :CC[B] = iterableFactory.from(head)

	override def transpose[B](implicit asIterable :E => Iterable[B]) :CC[CC[B] @uncheckedVariance] =
		iterableFactory.from(asIterable(head).view.map(one))

	override def concat[B >: E](suffix :IterableOnce[B]) :CC[B] =
		if (suffix.toIterableOnceOps.isEmpty) asGeneric else super.concat(suffix)

	override def zip[B](that :IterableOnce[B]) :CC[(E @uncheckedVariance, B)] =
		that match {
			case empty :Iterable[_] if empty.isEmpty => iterableFactory.empty
			case it :Iterable[B] => one((head, it.head))
			case _ =>
				val i = that.iterator
				if (!i.hasNext) iterableFactory.empty
				else one((head, i.next()))
		}
	override def zipWithIndex :CC[(E @uncheckedVariance, Int)] = one((head, 0))

	override def zipAll[A1 >: E, B](that :Iterable[B], thisElem :A1, thatElem :B) :CC[(A1, B)] =
		that match {
			case empty :Iterable[B] if empty.isEmpty => one((head, thatElem))
			case it :Iterable[B] if it.sizeIs == 1 => one((head, it.head))
			case _ => super.zipAll(that, thisElem, thatElem)
		}

	override def unzip[A1, A2](implicit asPair :E => (A1, A2)) :(CC[A1], CC[A2]) = (one(head._1), one(head._2))
	override def unzip3[A1, A2, A3](implicit asTriple :E => (A1, A2, A3)) :(CC[A1], CC[A2], CC[A3]) =
		(one(head._1), one(head._2), one(head._3))


	override def copyRangeToArray[B >: E](xs :Array[B], start :Int, from :Int, until :Int) :Int =
		if (start > xs.length || until < from | until <= 0 | from > 0)
			0
		else {
			xs(start) = head; 1
		}

	override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
		if (len <= 0 || start >= xs.length)
			0
		else {
			xs(start) = head; 1
		}

	override def iterator :Iterator[E] = Iterator.single(head)
	override def toList :List[E] = head::Nil
	override def toVector :Vector[E] = Vector.empty :+ head
	override def toIndexedSeq :IndexedSeq[E] = PassedArray.one(head)
	override def toSet[B >: E] :Set[B] = Set.empty[B] + head
	override def toMap[K, V](implicit ev :E <:< (K, V)) :Map[K, V] = Map.empty[K, V] + head
	override def toArray[B >: E :ClassTag] :Array[B] = Array.one[B](head)

	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) = Stepper1(head)

	override def addString(b :StringBuilder, start :String, sep :String, end :String) :b.type =
		b ++= start ++= head.toString ++= end

}



@SerialVersionUID(Ver)
private[noresttherein] object SingletonIterableOps {
	trait Variant[+E, +CC[+_], +C <: CC[E]] extends SingletonIterableOps[E, CC, C] { this :C =>
		protected override def asGeneric[T >: E] :CC[T] = this
	}
}



private[noresttherein] trait SingletonSeqOps[+E, +CC[_], +C]
	extends SingletonIterableOps[E, CC, C] with SeqOps[E, CC, C]
{ this :C =>
	private def outOfBounds(i :Int) = throw new IndexOutOfBoundsException(i.toString + " out of 1")

	override def length :Int = 1
	override def apply(i :Int) :E = if (i == 0) head else outOfBounds(i)
	override def updated[B >: E](i :Int, elem :B) :CC[B] = if (i != 0) outOfBounds(i) else one(elem)
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
			case 1 => that.toIterableOnceOps.isEmpty
			case 0 => that.toIterableOnceOps.isEmpty || sameElements(that)
			case _ => false
		}

	override def permutations :Iterator[C] = Iterator.single(this)
	override def combinations(n :Int) :Iterator[C] = n match {
		case 0 => Iterator.single(empty)
		case 1 => Iterator.single(this)
		case _ => Iterator.empty
	}


	override def diff[B >: E](that :collection.Seq[B]) :C = if (that.contains(head)) empty else this
	override def intersect[B >: E](that :collection.Seq[B]) :C = if (that.contains(head)) this else empty
//	override def patch[B >: E](from :Int, other :IterableOnce[B], replaced :Int) :CC[B] =
//		iterableFactory.from(other)

	override def prepended[B >: E](elem :B) :CC[B] = two(elem, head)
	override def appended[B >: E](elem :B)  :CC[B] = two(head, elem)
	override def prependedAll[B >: E](prefix :IterableOnce[B]) :CC[B] =
		if (prefix.toIterableOnceOps.isEmpty) asGeneric
		else super.prependedAll(prefix)
	override def appendedAll[B >: E](suffix :IterableOnce[B]) :CC[B] =
		if (suffix.toIterableOnceOps.isEmpty) asGeneric else super.appendedAll(suffix)

	override def sorted[B >: E](implicit ord :Ordering[B]) :C = this
	override def sortWith(lt :(E, E) => Boolean) :C = this
	override def sortBy[B](f :E => B)(implicit ord :Ordering[B]) :C = this

	override def distinct :C = this
	override def distinctBy[B](f :E => B) :C = this
	override def reverse :C = this

	override def reverseIterator :Iterator[E] = Iterator.single(head)

	override def corresponds[B](that :collection.Seq[B])(p :(E, B) => Boolean) = that.isEmpty

	override def sameElements[B >: E](that :IterableOnce[B]) :Boolean = that match {
		case it :Iterable[B] => it.sizeIs == 1 && head == it.head
		case _ =>
			val i = that.iterator
			i.hasNext && head == i.next() && !i.hasNext
	}
}



@SerialVersionUID(Ver)
private[noresttherein] object SingletonSeqOps {
	trait Variant[+E, +CC[+_], +C <: CC[E]]
		extends SingletonIterableOps.Variant[E, CC, C] with SingletonSeqOps[E, CC, C]
	{ this :C => }
}



private[noresttherein] trait SingletonIndexedSeqOps[+E, +CC[_], +C]
	extends SingletonIterableOps[E, CC, C] with IndexedSeqOps[E, CC, C] with SingletonSeqOps[E, CC, C]
{ this :C =>
	override def iterator :Iterator[E] = Iterator.single(head)
}



@SerialVersionUID(Ver)
private[noresttherein] object SingletonIndexedSeqOps {
	trait Variant[+E, +CC[+_], +C <: CC[E]]
		extends SingletonSeqOps.Variant[E, CC, C] with SingletonIndexedSeqOps[E, CC, C]
	{ this :C => }
}
