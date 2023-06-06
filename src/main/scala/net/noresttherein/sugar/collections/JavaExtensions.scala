package net.noresttherein.sugar.collections

import java.util.function.Consumer

import scala.collection.immutable.IndexedSeqOps
import scala.jdk.CollectionConverters.ListHasAsScala

import net.noresttherein.sugar.JavaTypes.{JArrayList, JCollection, JLinkedList, JList}
import net.noresttherein.sugar.collections.util.knownEmpty
import net.noresttherein.sugar.extensions.classNameMethods


/*
trait JavaExtensions {

}



object JavaExtensions {
	private abstract class JListOps[E, +CC[_], +C](private val underlying :JLinkedList[E]) extends collect{

	}
	class JavaListExtension[E](private val self :JList[E]) extends AnyVal {
		self.asScala
		@inline def knownSize :Int = self.size
		@inline def length :Int = self.size
		@inline def isEmpty  :Boolean = self.isEmpty
		@inline def nonEmpty :Boolean = !self.isEmpty
		@inline def sizeIs :Int = self.size
		@inline def lengthIs :Int = self.size
		@inline def sizeCompare(otherSize :Int) :Int= Integer.compare(self.size, otherSize)
		@inline def lengthCompare(len :Int) :Int = Integer.compare(self.size, len)

		@inline def apply(i :Int) :E = self.get(i)
		@inline def head :E =
			if (self.isEmpty) self.get(0)
			else throw new NoSuchElementException(self.className + "<0>.head")

		@inline def last :E =
			if (self.isEmpty) self.get(self.size)
			else throw new NoSuchElementException(self.className + "<0>.last")

		@inline def headOption :Option[E] = if (self.isEmpty) None else Some(self.get(0))
		@inline def lastOption :Option[E] = if (self.isEmpty) None else Some(self.get(self.size - 1))

		def forall(p :E => Boolean) :Boolean =
			self.isEmpty || {
				val i = self.iterator
				while (i.hasNext)
					if (!p(i.next()))
						return false
				true
			}
		def exists(p :E => Boolean) :Boolean =
			!self.isEmpty && {
				val i = self.iterator
				while (i.hasNext)
					if (p(i.next()))
						return true
				false
			}
		def count(p :E => Boolean) :Int =
			if (self.isEmpty)
				0
			else {
				class Counter extends Consumer[E] {
					private[this] var n = 0
					def result = n
					override def accept(t :E) :Unit = if (p(t)) n += 1
				}
				val res = new Counter
				self.forEach(res)
				res.result
			}
		def find(p :E => Boolean) :Option[E] =
			if (self.isEmpty)
				None
			else {
				val i = self.iterator
				var found = false
				while (i.hasNext) {
					val last = i.next()
					if (p(last))
						return Some(last)
				}
				None
			}
		def findLast(p :E => Boolean) :Option[E] =
			if (self.isEmpty)
				None
			else self match {
				case a :JArrayList[E] =>
					var i = a.size - 1
					while (i > 0) {
						i -= 1
						val last = a.get(i)
						if (p(last))
							return Some(last)
					}
					None
				case _ =>
					val i = self.iterator
					var res :Option[E] = None
					while (i.hasNext) {
						val last = i.next()
						if (p(last))
							res = Some(last)
					}
					res
			}
		def indexWhere(p :E => Boolean, from :Int = 0) :Int =
			if (from >= self.size)
				-1
			else {
				var i = math.max(from, 0); val end = self.size
				val it = self.listIterator(i)
				while (i < end) {
					if (p(it.next()))
						return i
					i += 1
				}
				-1
			}
		def lastIndexWhere(p :E => Boolean, end :Int = Int.MaxValue) :Int =
			if (end < 0)
				-1
			else if (self.isEmpty)
				-1
			else self match {
				case a :JArrayList[E] =>
					var i = math.min(end, self.size) - 1
					while (i >= 0 && !p(self.get(i)))
						i -= 1
					i
				case _ =>
					val it = self.iterator
					var i = 0; val end = self.size
					var found = -1
					while (i < end) {
						if (p(it.next()))
							found = i
						i += 1
					}
					found
			}
		def indexOf(elem :E, from :Int = 0) :Int =
			if (from >= self.size)
				-1
			else if (from == 0)
				self.indexOf(elem)
			else
				indexWhere(_ == elem, from)

		def lastIndexOf(elem :E, end :Int = Int.MaxValue) :Int =
			if (end < 0)
				-1
			else if (end >= self.size)
				self.lastIndexOf(elem)
			else
				lastIndexWhere(_ == elem, end)

//		@inline def contains(elem :E) :Boolean = self.contains(elem)

		def segmentLength(p :E => Boolean, from :Int = 0) :Int =
			if (from >= self.size)
				0
			else {
				val start = math.max(from, 0)
				var i = start
				val it = self.listIterator(i)
				while (it.hasNext && p(it.next()))
					i += 1
				i - start
			}

		def startsWith[A >: E](that :IterableOnce[A], offset :Int = 0) :Boolean =
			knownEmpty(that) || offset <= self.size && {
				val thisIter = self.listIterator(math.max(0, offset))
				val thatIter = that.iterator
				while (thisIter.hasNext && thatIter.hasNext) {
					if (thisIter.next() != thatIter.next())
						return false
				}
				true
			}
		def startsWith[A >: E](that :JCollection[A], offset :Int) :Boolean = {
			val thisIter = self.listIterator(math.max(offset, 0))
			val thatIter = that.iterator
			while (thisIter.hasNext && thatIter.hasNext) {
				if (thisIter.next() != thatIter.next())
					return false
			}
			true
		}
		@inline def startsWith[A >: E](that :JCollection[A]) :Boolean = startsWith(that, 0)

		def endsWith[A >: E](that :Iterable[A]) :Boolean = {
			val thisSize = self.size
			val thatSize = that.size
			var i        = thisSize - thatSize
			i >= 0 && {
				val thisIter = self.listIterator(i)
				val thatIter = that.iterator
				while (i < thisSize) {
					if (thisIter.next() != thatIter.next())
						return false
					i += 1
				}
				true
			}
		}
		def endsWith[A >: E](that :JCollection[A]) :Boolean = {
			val thisSize = self.size
			val thatSize = that.size
			var i        = thisSize - thatSize
			i >= 0 && {
				val thisIter = self.listIterator(i)
				val thatIter = that.iterator
				while (i < thisSize) {
					if (thisIter.next() != thatIter.next())
						return false
					i += 1
				}
				true
			}
		}
		def indexOfSlice[A >: E](that :collection.Seq[A], from :Int = 0) :Int =
			if (knownEmpty(that))
				math.min(self.size, math.max(from, 0))
			else
				self.asScala.indexOfSlice(that, from)

		def lastIndexOfSlice[A >: E](that :collection.Seq[A], end :Int = Int.MaxValue) :Int =
			if (knownEmpty(that))
				math.min(self.size, math.max(end, 0))
			else
				self.asScala.lastIndexOfSlice(that)

		@inline def containsSlice[A >: E](that :collection.Seq[A]) :Boolean = indexOfSlice(that) >= 0

		def corresponds[A](that :IterableOnce[A])(p :(E, A) => Boolean) :Boolean =
			self.asScala.corresponds(that)(p)

		@inline def sum[A >: E](implicit num :Numeric[A]) :A = self.asScala.sum[A]
		@inline def product[A >: E](implicit num :Numeric[A]) :A = self.asScala.product[A]
		@inline def min[A >: E](implicit ord :Ordering[A]) :E = self.asScala.min[A]
		@inline def minOption[A >: E](implicit ord :Ordering[A]) :Option[E] = self.asScala.minOption[A]
		@inline def max[A >: E](implicit ord :Ordering[A]) :E = self.asScala.max[A]
		@inline def maxOption[A >: E](implicit ord :Ordering[A]) :Option[E] = self.asScala.maxOption[A]
		@inline def maxBy[A](f :E => A)(implicit cmp :Ordering[A]) :E = self.asScala.maxBy(f)
		@inline def maxByOption[A](f :E => A)(implicit cmp :Ordering[A]) :Option[E] = self.asScala.maxByOption(f)
		@inline def minBy[A](f :E => A)(implicit cmp :Ordering[A]) :E = self.asScala.minBy(f)
		@inline def minByOption[A](f :E => A)(implicit cmp :Ordering[A]) :Option[E] = self.asScala.minByOption(f)

		@inline def /: [A](z :A)(op :(A, E) => A) :A = foldLeft[A](z)(op)
		@inline def :\ [A](z :A)(op :(E, A) => A) :A = foldRight[A](z)(op)

		def foldLeft[A](z :A)(op :(A, E) => A) :A =
			if (self.isEmpty)
				z
			else self match {
				case _ :JArrayList[E] =>
					var i = 0; val end = self.size
					var acc = z
					while (i < end) {
						acc = op(acc, self.get(i))
						i += 1
					}
					acc
				case _ =>
					val i = self.iterator
					var acc = z
					while (i.hasNext)
						acc = op(acc, i.next())
					acc
			}
		def foldRight[A](z :A)(op :(E, A) => A) :A =
			if (self.isEmpty)
				z
			else self match {
				case _ :JArrayList[E] =>
					var i = self.size - 1
					var acc = z
					while (i >= 0) {
						acc = op(self.get(i), acc)
						i -= 1
					}
					acc
				case _ =>
					var reverse :List[E] = Nil
					val i = self.iterator
					while (i.hasNext)
						reverse = i.next()::reverse
					reverse.foldLeft(z)((e, a) => op(a, e))
			}
		@inline def fold[A >: E](z :A)(op :(A, A) => A) :A = foldLeft(z)(op)

		def reduceLeft[A >: E](op :(A, E) => A) :A =
			if (self.isEmpty)
				throw new UnsupportedOperationException(self.toString + ".reduceLeft")
			else self match {
				case _ :JArrayList[E] =>
					var i = 1; val end = self.size
					var acc :A = self.get(0)
					while (i < end) {
						acc = op(acc, self.get(i))
						i += 1
					}
					acc
				case _ =>
					val i = self.iterator
					var acc :A = i.next()
					while (i.hasNext)
						acc = op(acc, i.next())
					acc
			}
		def reduceRight[A >: E](op :(E, A) => A) :A =
			if (self.isEmpty)
				throw new UnsupportedOperationException(self.toString + ".reduceRight")
			else self match {
				case _ :JArrayList[E] =>
					var i = self.size - 1
					var acc :A = self.get(i)
					while (i >= 0) {
						i -= 1
						acc = op(self.get(i), acc)
					}
					acc
				case _ =>
					var reverse :List[E] = Nil
					val i = self.iterator
					while (i.hasNext)
						reverse = i.next()::reverse
					reverse.reduceLeft[A]((e, a) => op(a, e))
			}
		@inline def reduce[A >: E](op :(A, A) => A) :A = reduceLeft(op)
		@inline def reduceOption[A >: E](op :(A, A) => A) :Option[A] =
			if (self.isEmpty) None else Some(reduce(op))

		@inline def reduceLeftOption[A >: E](op :(A, E) => A) :Option[A] =
			if (self.isEmpty) None else Some(reduceLeft(op))

		@inline def reduceRightOption[A >: E](op :(E, A) => A) :Option[A] =
			if (self.isEmpty) None else Some(reduceRight(op))

		def scanLeft[A](z :A)(op :(A, E) => A) :JList[A] = {
			val res = self match {
				case _ :JLinkedList[E] => new JLinkedList[A]
				case _ => new JArrayList[A](self.size + 1)
			}
			res.add(z)
			var acc = z
			val i = self.iterator
			while (i.hasNext) {
				acc = op(acc, i.next())
				res add acc
			}
			res
		}
		def scanRight[A](z :A)(op :(E, A) => A) :JList[A] = self match {
			case a :JArrayList[E] =>
				a.scanRight(z)(op)
			case _ if self.isEmpty =>
				val res = new JLinkedList[A]
				 res add z
				res
			case _ =>
				val res = new JLinkedList
				var reverse :List[A] = Nil
				val i = self.iterator
				while (i.hasNext)
		}
		@inline def scan[A >: E](z :A)(op :(A, A) => A) :JList[A] = scanLeft(z)(op)

		@inline def take(n :Int) :IArray[E] = array.take(n).asInstanceOf[IArray[E]]
		@inline def drop(n :Int) :IArray[E] = array.drop(n).asInstanceOf[IArray[E]]
		@inline def takeRight(n :Int) :IArray[E] = array.takeRight(n).asInstanceOf[IArray[E]]
		@inline def dropRight(n :Int) :IArray[E] = array.dropRight(n).asInstanceOf[IArray[E]]
		@inline def takeWhile(p :E => Boolean) :IArray[E] = array.takeWhile(p).asInstanceOf[IArray[E]]
		@inline def dropWhile(p :E => Boolean) :IArray[E] = array.dropWhile(p).asInstanceOf[IArray[E]]
		@inline def slice(from :Int, until :Int) :IArray[E] = array.slice(from, until).asInstanceOf[IArray[E]]

		@inline def splitAt(n :Int) :(IArray[E], IArray[E]) =
			array.splitAt(n).asInstanceOf[(IArray[E], IArray[E])]

		@inline def span(p :E => Boolean) :(IArray[E], IArray[E]) =
			array.span(p).asInstanceOf[(IArray[E], IArray[E])]

		@inline def reverse :IArray[E] = array.reverse.asInstanceOf[IArray[E]]

		@inline def sortWith(lt :(E, E) => Boolean) :IArray[E] = array.sortWith(lt).asInstanceOf[IArray[E]]
		@inline def sortBy[A](f :E => A)(implicit ord: Ordering[A]): IArray[E] =
			array.sortBy(f).asInstanceOf[IArray[E]]

		@inline def sorted[A >: E](implicit ordering :Ordering[A]) :IArray[E] =
			array.sorted[A].asInstanceOf[IArray[E]]

		@inline def distinct :IArray[E] = array.distinct.asInstanceOf[IArray[E]]
		@inline def distinctBy[A](f :E => A) :IArray[E] = array.distinctBy(f).asInstanceOf[IArray[E]]


		@inline def tail :IArray[E] = array.tail.asInstanceOf[IArray[E]]
		@inline def init :IArray[E] = array.init.asInstanceOf[IArray[E]]

		@inline def tails :Iterator[IArray[E]] = array.tails.castFrom[Iterator[Array[E]], Iterator[IArray[E]]]
		@inline def inits :Iterator[IArray[E]] = array.inits.castFrom[Iterator[Array[E]], Iterator[IArray[E]]]
		@inline def grouped(size :Int) :Iterator[IArray[E]] =
			array.grouped(size).castFrom[Iterator[Array[E]], Iterator[IArray[E]]]

		@inline def sliding(size :Int, step :Int = 1) :Iterator[IArray[E]] =
			array.sliding(size, step).castFrom[Iterator[Array[E]], Iterator[IArray[E]]]

		@inline def combinations(n :Int) :Iterator[IArray[E]] =
			array.combinations(n).castFrom[Iterator[Array[E]], Iterator[IArray[E]]]

		@inline def permutations :Iterator[IArray[E]] =
			array.permutations.castFrom[Iterator[Array[E]], Iterator[IArray[E]]]

		@inline def iterator :Iterator[E] = array.iterator //ArrayIterator(array)
		@inline def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S = array.stepper
		@inline def reverseIterator :Iterator[E] = array.reverseIterator

		@inline def withFilter(p :E => Boolean) :WithFilter[E] = new WithFilter(p, array)
		@inline def filter(p :E => Boolean) :IArray[E] = array.filter(p).asInstanceOf[IArray[E]]
		@inline def filterNot(p :E => Boolean) :IArray[E] = array.filterNot(p).asInstanceOf[IArray[E]]
		@inline def partition(p :E => Boolean) :(IArray[E], IArray[E]) =
			array.partition(p).asInstanceOf[(IArray[E], IArray[E])]

		@inline def collectFirst[A](pf :PartialFunction[E, A]) :Option[A] = array.collectFirst(pf)
		@inline def collect[A :ClassTag](pf :PartialFunction[E, A]) :IArray[A] =
			array.collect(pf).asInstanceOf[IArray[A]]

		@inline def partitionMap[E1: ClassTag, E2: ClassTag](f: E => Either[E1, E2]) :(IArray[E1], IArray[E2]) =
			array.partitionMap(f).castFrom[(Array[E1], Array[E2]), (IArray[E1], IArray[E2])]

		@inline def map[A :ClassTag](f :E => A) :IArray[A] = array.map(f).asInstanceOf[IArray[A]]
		@inline def flatMap[A :ClassTag](f :E => IterableOnce[A]) :IArray[A] =
			array.flatMap(f).asInstanceOf[IArray[A]]

		@inline def flatMap[As, A](f :E => As)(implicit asIterable :As => Iterable[A], m :ClassTag[A]) :IArray[A] =
			array.flatMap(f).asInstanceOf[IArray[A]]

		@inline def flatten[A :ClassTag](implicit asIterable :E => IterableOnce[A]) :IArray[A] =
			array.flatten.asInstanceOf[IArray[A]]

//		@inline def groupBy[K](f: E => K) :Map[K, IArray[E]] =
//			array.groupBy(f).castFrom[Map[K, Array[E]], Map[K, IArray[E]]]
//
		@inline def groupMap[K, A :ClassTag](key :E => K)(f :E => A) :Map[K, IArray[A]] =
			array.groupMap(key)(f).castFrom[Map[K, Array[A]], Map[K, IArray[A]]]

		@inline def tapEach[U](f :E => U) :IArray[E] = array.tapEach(f).asInstanceOf[IArray[E]]
		@inline def foreach[U](f :E => U) :Unit = array.foreach(f)

		@inline def zipWithIndex :IArray[(E, Int)] = array.zipWithIndex.asInstanceOf[IArray[(E, Int)]]
		@inline def zip[A](that :IterableOnce[A]) :IArray[(E, A)] = array.zip(that).asInstanceOf[IArray[(E, A)]]

		@inline def lazyZip[A](that: Iterable[A]) :LazyZip2[E, A, IArray[E]] =
			array.lazyZip(that).asInstanceOf[LazyZip2[E, A, IArray[E]]]

		@inline def zipAll[A >: E, B](that :Iterable[B], thisElem :A, thatElem :B) :IArray[(A, B)] =
			array.zipAll(that, thisElem, thatElem).castFrom[Array[(A, B)], IArray[(A, B)]]

		@inline def unzip[E1, E2](implicit asPair :E => (E1, E2), ct1 :ClassTag[E1], ct2 :ClassTag[E2])
				:(IArray[E1], IArray[E2]) =
			array.unzip.castFrom[(Array[E1], Array[E2]), (IArray[E1], IArray[E2])]

		@inline def unzip3[E1, E2, E3](implicit asTriple :E => (E1, E2, E3), ct1 :ClassTag[E1], ct2 :ClassTag[E2],
		                               ct3 :ClassTag[E3]) :(IArray[E1], IArray[E2], IArray[E3]) =
			array.unzip3.castFrom[(Array[E1], Array[E2], Array[E3]), (IArray[E1], IArray[E2], IArray[E3])]

		@inline def updated[A >: E :ClassTag](index :Int, x :A) :IArray[A] =
			array.updated(index, x).asInstanceOf[IArray[A]]

		@inline def updated[A >: E](index :Int, first :A, second :A, rest :A*) :IArray[A] =
			array.updated(index, first, second, rest).asInstanceOf[IArray[A]]

//		def updatedAll[U >: E :ClassTag](index :Int, elems :IterableOnce[U]) :IArray[U] = {
//			val length = array.length
//			val size   = elems.knownSize
//			if (index < 0 || size >= 0 && index + size >= length)
//				throw new IndexOutOfBoundsException(
//					s"${array.localClassName}<$length>.updatedAll($index, IterableOnce<$size>)"
//				)
//			val res = ArrayAsSeq.copyAs[U](array, length)
//			elems.foreachWithIndex { (elem, i) => res(index + i) = elem }
//			res.asInstanceOf[IArray[E]]
//		}
//
//		@inline def inserted[A >: E :ClassTag](index :Int, elem :A) :IArray[A] =
//			array.inserted(index, elem).asInstanceOf[IArray[A]]
//
//		@inline def inserted[A >: E :ClassTag](index :Int, first :A, second :A, rest :A*) :IArray[A] =
//			array.inserted(index, first, second, rest :_*).asInstanceOf[IArray[A]]
//
//		@inline def insertedAll[A >: E :ClassTag](index :Int, elems :IterableOnce[A]) :IArray[A] =
//			array.insertedAll(index, elems).asInstanceOf[IArray[A]]

		@inline def :+[A >: E :ClassTag](x :A) :IArray[A] = array.appended(x).asInstanceOf[IArray[A]]
		@inline def +:[A >: E :ClassTag](x :A) :IArray[A] = array.prepended(x).asInstanceOf[IArray[A]]
		@inline def appended[A >: E :ClassTag](x :A) :IArray[A] = array.appended(x).asInstanceOf[IArray[A]]
		@inline def appended[A >: E :ClassTag](first :A, second :A, rest :A*) :IArray[A] =
			array.appended(first, second, rest :_*).asInstanceOf[IArray[A]]

		@inline def prepended[A >: E :ClassTag](x :A) :IArray[A] = array.prepended(x).asInstanceOf[IArray[A]]

		@inline def prepended[A >: E :ClassTag](first :A, second :A, rest :A*) :IArray[A] =
			array.prepended(first, second, rest :_*).asInstanceOf[IArray[A]]

		@inline def ++[A >: E :ClassTag](suffix :IterableOnce[A]) :IArray[A] =
			array.appendedAll(suffix).asInstanceOf[IArray[A]]

		@inline def ++[A >: E :ClassTag](suffix :IArray[A]) :IArray[A] =
			array.appendedAll(suffix.asInstanceOf[Array[A]]).asInstanceOf[IArray[A]]

		@inline def concat[A >: E :ClassTag](suffix :IterableOnce[A]) :IArray[A] =
			array.appendedAll(suffix).asInstanceOf[IArray[A]]

		@inline def concat[A >: E :ClassTag](suffix :IArray[A]) :IArray[A] =
			array.appendedAll(suffix.asInstanceOf[Array[A]]).asInstanceOf[IArray[A]]

		@inline def :++[A >: E :ClassTag](suffix :IterableOnce[A]) :IArray[A] =
			array.appendedAll(suffix).asInstanceOf[IArray[A]]

		@inline def :++[A >: E :ClassTag](suffix :IArray[A]) :IArray[A] =
			array.appendedAll(suffix.asInstanceOf[Array[A]]).asInstanceOf[IArray[A]]

		@inline def :++[A >: E](suffix :IArray[A])(implicit tag :Maybe[ClassTag[A]]) :IArray[A] =
			array.appendedAll(suffix.asInstanceOf[Array[A]])(
				tag getOrElse ClassTag.Any.castParam[A]
			).asInstanceOf[IArray[A]]

		@inline def appendedAll[A >: E :ClassTag](suffix :IterableOnce[A]) :IArray[A] =
			array.appendedAll(suffix).asInstanceOf[IArray[A]]

		@inline def appendedAll[A >: E :ClassTag](suffix :IArray[A]) :IArray[A] =
			array.appendedAll(suffix.asInstanceOf[Array[A]]).asInstanceOf[IArray[A]]

		@inline def appendedAll[A >: E](suffix :IArray[A])(implicit tag :Maybe[ClassTag[A]]) :IArray[A] =
			array.appendedAll(suffix.asInstanceOf[Array[A]])(
				tag getOrElse ClassTag.Any.castParam[A]
			).asInstanceOf[IArray[A]]

		@inline def ++:[A >: E :ClassTag](suffix :IterableOnce[A]) :IArray[A] =
			array.prependedAll(suffix).asInstanceOf[IArray[A]]

		@inline def ++:[A >: E :ClassTag](suffix :IArray[A]) :IArray[A] =
			array.prependedAll(suffix.asInstanceOf[Array[A]]).asInstanceOf[IArray[A]]

		@inline def prependedAll[A >: E :ClassTag](suffix :IterableOnce[A]) :IArray[A] =
			array.prependedAll(suffix).asInstanceOf[IArray[A]]

		@inline def prependedAll[A >: E :ClassTag](suffix :IArray[A]) :IArray[A] =
			array.prependedAll(suffix.asInstanceOf[Array[A]]).asInstanceOf[IArray[A]]

		@inline def patch[A >: E :ClassTag](from :Int, elems :IterableOnce[A], replaced :Int) :IArray[A] =
			array.patch(from, elems, replaced).asInstanceOf[IArray[A]]

		@inline def patch[A >: E](from :Int, elems :Array[A], replaced :Int) :IArray[A] =
			array.patch(from, elems, replaced).asInstanceOf[IArray[A]]

		@inline def patch[A >: E :ClassTag](from :Int, elems :ArrayLike[A], replaced :Int) :IArray[A] =
			array.patch(from, mutable.ArraySeq.make(elems.asInstanceOf[Array[A]]), replaced).asInstanceOf[IArray[A]]

		@inline def diff[A >: E](that :collection.Seq[A]) :IArray[E] = array.diff(that).asInstanceOf[IArray[E]]
//		@inline def diff[A >: E](that :IArray[A]) :IArray[E] =
//			array.diff(mutable.ArraySeq.make(that.asInstanceOf[Array[A]])).asInstanceOf[IArray[E]]

		@inline def intersect[A >: E](that :collection.Seq[A]) :IArray[E] =
			array.intersect(that).asInstanceOf[IArray[E]]
//
//		@inline def intersect[A >: E](that :IArray[A]) :IArray[E] =
//			array.intersect(mutable.ArraySeq.make(that.asInstanceOf[Array[A]])).asInstanceOf[IArray[E]]

		@inline def transpose[A](implicit asArray :E => IArray[A]): IArray[IArray[A]] =
			array.transpose(asArray.asInstanceOf[E => Array[A]]).castFrom[Array[Array[A]], IArray[IArray[A]]]

		@inline def view :IndexedSeqView[E] = array.view
		@inline def indices :Range = Range(0, array.length)

		def toOps :IndexedSeqOps[E, IArray, IArray[E]] = new IArrayAsSeq[E](array.asInstanceOf[IArray[E]])

//		@inline def to[C1](factory :Factory[E, C1]) :C1 = array.to(factory)
		@inline def toSeq :Seq[E] = WrappedArray(array)
		@inline def toIndexedSeq :IndexedSeq[E] = WrappedArray(array)

		@inline def toList :List[E] = array.toList
		@inline def toVector :Vector[E] = array.toVector
		@inline def toSet[A >: E] :Set[A] = array.toSet
		@inline def toMap[K, V](implicit ev :E <:< (K, V)) :Map[K, V] = array.toMap
		@inline def toArray[A >: E :ClassTag] :Array[A] = array.toArray
		@inline def toBuffer[A >: E]: Buffer[A] = Buffer.from(array)

		@inline def copyToArray[A >: E](xs :Array[A]) :Int = array.copyToArray(xs, 0, xs.length)
		@inline def copyToArray[A >: E](xs :Array[A], start :Int) :Int = array.copyToArray(xs, start, xs.length)
		@inline def copyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int = array.copyToArray(xs, start, len)

		@inline def mkString :String = array.mkString("", "", "")
		@inline def mkString(separator :String) :String = array.mkString("", separator, "")
		@inline def mkString(prefix :String, separator :String, suffix :String) :String =
			array.mkString(prefix, separator, suffix)

		@inline def addString(b :StringBuilder) :b.type = array.addString(b, "", "", "")
		@inline def addString(b :StringBuilder, sep :String) :b.type = array.addString(b, "", sep, "")
		@inline def addString(b :StringBuilder, start :String, sep :String, end :String) :b.type =
			array.addString(b, start, sep, end)

		@inline def sameElements(other :IArray[_]) :Boolean = array sameElements other.asInstanceOf[Array[_]]
		@inline def sameElements(other :IterableOnce[_]) :Boolean = array sameElements other

	}
}
*/
