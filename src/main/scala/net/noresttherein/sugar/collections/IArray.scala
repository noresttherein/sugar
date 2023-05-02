package net.noresttherein.sugar.collections

import scala.collection.mutable.{ArrayBuilder, Buffer, Builder}
import scala.collection.{ClassTagIterableFactory, EvidenceIterableFactory, Factory, IndexedSeqView, IterableFactory, LazyZip2, Stepper, StepperShape, mutable}
import scala.reflect.ClassTag
import scala.Array.UnapplySeqWrapper
import scala.collection.immutable.{ArraySeq, IndexedSeqOps}

import net.noresttherein.sugar.collections.IArray.{BooleanIArrayExtension, ByteIArrayExtension, CharIArrayExtension, DoubleIArrayExtension, FloatIArrayExtension, GenericIArrayExtension, IArrayExtension, IntIArrayExtension, LongIArrayExtension, RefIArrayExtension, ShortIArrayExtension}
import net.noresttherein.sugar.collections.extensions._
import net.noresttherein.sugar.extensions.{castTypeParam, saferCasting}
import net.noresttherein.sugar.witness.Maybe



/*

case object BaseArray extends EvidenceIterableFactory.Delegate[BaseArray, ClassTag](IArray) {
	val untagged :IterableFactory[BaseArray] = IArray.untagged

	class BaseArrayExtension[Arr[X] <: BaseArray[X], E] private[collections] (private[IArray] val array :Array[E])
		extends AnyVal//with IterableOnce[E]
	{
		@inline def knownSize :Int = array.length
		@inline def length :Int = array.length
		@inline def size :Int = array.length
		@inline def isEmpty  :Boolean = array.length == 0
		@inline def nonEmpty :Boolean = array.length > 0
		@inline def sizeIs :Int = array.length
		@inline def lengthIs :Int = array.length
		@inline def sizeCompare(otherSize :Int) :Int= Integer.compare(array.length, otherSize)
		@inline def lengthCompare(len :Int) :Int = Integer.compare(array.length, len)

		@inline def apply(n :Int) :E = array(n)
		@inline def head :E = array.head
		@inline def last :E = array.last
		@inline def headOption :Option[E] = array.headOption
		@inline def lastOption :Option[E] = array.lastOption

		@inline def forall(p :E => Boolean) :Boolean = array.forall(p)
		@inline def exists(p :E => Boolean) :Boolean = array.exists(p)
		@inline def count(p :E => Boolean) :Int = array.count(p)
		@inline def find(p :E => Boolean) :Option[E] = array.find(p)
		@inline def indexWhere(p :E => Boolean) :Int = array.indexWhere(p)
		@inline def lastIndexWhere(p :E => Boolean) :Int = array.lastIndexWhere(p)
		@inline def indexOf(elem :E, from :Int = 0) :Int = array.indexOf(elem, from)
		@inline def lastIndexOf(elem :E, end :Int = 0) :Int = array.lastIndexOf(elem, end)
		@inline def contains(elem :E) :Boolean = array.contains(elem)

		@inline def segmentLength(p :E => Boolean, from :Int = 0) :Int = array.segmentLength(p, from)
		@inline def startsWith[A >: E](that :IterableOnce[A]) :Boolean = array.startsWith(that, 0)
		@inline def startsWith[A >: E](that :IterableOnce[A], offset :Int) :Boolean = array.startsWith(that, offset)
		@inline def startsWith[A >: E](that :Array[A]) :Boolean = array.startsWith(that, 0)
		@inline def startsWith[A >: E](that :Array[A], offset :Int) :Boolean =
			array.startsWith(that, offset)

		@inline def endsWith[A >: E](that :Iterable[A]) :Boolean = array.endsWith(that)
		@inline def endsWith[A >: E](that :Array[A]) :Boolean = array.endsWith(that)

		@inline def corresponds[A](that :IterableOnce[A])(p :(E, A) => Boolean) :Boolean =
			array.corresponds(that)(p)

		@inline def corresponds[A](that :Array[A])(p :(E, A) => Boolean) :Boolean = array.corresponds(that)(p)

		@inline def sum[A >: E](implicit num :Numeric[A]) :A = array.sum[A]
		@inline def product[A >: E](implicit num :Numeric[A]) :A = array.product[A]
		@inline def min[A >: E](implicit ord :Ordering[A]) :E = array.min[A]
		@inline def minOption[A >: E](implicit ord :Ordering[A]) :Option[E] = array.minOption[A]
		@inline def max[A >: E](implicit ord :Ordering[A]) :E = array.max[A]
		@inline def maxOption[A >: E](implicit ord :Ordering[A]) :Option[E] = array.maxOption[A]
		@inline def maxBy[A](f :E => A)(implicit cmp :Ordering[A]) :E = array.maxBy(f)
		@inline def maxByOption[A](f :E => A)(implicit cmp :Ordering[A]) :Option[E] = array.maxByOption(f)
		@inline def minBy[A](f :E => A)(implicit cmp :Ordering[A]) :E = array.minBy(f)
		@inline def minByOption[A](f :E => A)(implicit cmp :Ordering[A]) :Option[E] = array.minByOption(f)

		@inline def /: [A](z :A)(op :(A, E) => A) :A = array.foldLeft[A](z)(op)
		@inline def :\ [A](z :A)(op :(E, A) => A) :A = array.foldRight[A](z)(op)

		@inline def foldLeft[A](z :A)(op :(A, E) => A) :A = array.foldLeft(z)(op)
		@inline def foldRight[A](z :A)(op :(E, A) => A) :A = array.foldRight(z)(op)
		@inline def fold[A >: E](z :A)(op :(A, A) => A) :A = array.fold(z)(op)
		@inline def reduce[A >: E](op :(A, A) => A) :A = array.reduce(op)
		@inline def reduceOption[A >: E](op :(A, A) => A) :Option[A] = array.reduceOption(op)
		@inline def reduceLeft[A >: E](op :(A, E) => A) :A = array.reduceLeft(op)
		@inline def reduceRight[A >: E](op :(E, A) => A) :A = array.reduceRight(op)
		@inline def reduceLeftOption[A >: E](op :(A, E) => A) :Option[A] = array.reduceLeftOption(op)
		@inline def reduceRightOption[A >: E](op :(E, A) => A) :Option[A] = array.reduceRightOption(op)

		@inline def scanLeft[A](z :A)(op :(A, E) => A)(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.scanLeft(z)(op)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]

		@inline def scanRight[A](z :A)(op :(E, A) => A)(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.scanRight(z)(op)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]

		@inline def scan[A >: E](z :A)(op :(A, A) => A)(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.scan(z)(op)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]

		@inline def take(n :Int) :Arr[E] = array.take(n).asInstanceOf[Arr[E]]
		@inline def drop(n :Int) :Arr[E] = array.drop(n).asInstanceOf[Arr[E]]
		@inline def takeRight(n :Int) :Arr[E] = array.takeRight(n).asInstanceOf[Arr[E]]
		@inline def dropRight(n :Int) :Arr[E] = array.dropRight(n).asInstanceOf[Arr[E]]
		@inline def takeWhile(p :E => Boolean) :Arr[E] = array.takeWhile(p).asInstanceOf[Arr[E]]
		@inline def dropWhile(p :E => Boolean) :Arr[E] = array.dropWhile(p).asInstanceOf[Arr[E]]
		@inline def slice(from :Int, until :Int) :Arr[E] = array.slice(from, until).asInstanceOf[Arr[E]]

		@inline def splitAt(n :Int) :(Arr[E], Arr[E]) =
			array.splitAt(n).asInstanceOf[(Arr[E], Arr[E])]

		@inline def span(p :E => Boolean) :(Arr[E], Arr[E]) =
			array.span(p).asInstanceOf[(Arr[E], Arr[E])]

		@inline def reverse :Arr[E] = array.reverse.asInstanceOf[Arr[E]]

		@inline def sortWith(lt :(E, E) => Boolean) :Arr[E] = array.sortWith(lt).asInstanceOf[Arr[E]]
		@inline def sortBy[A](f :E => A)(implicit ord: Ordering[A]): Arr[E] =
			array.sortBy(f).asInstanceOf[Arr[E]]

		@inline def sorted[A >: E](implicit ordering :Ordering[A]) :Arr[E] =
			array.sorted[A].asInstanceOf[Arr[E]]

		@inline def distinct :Arr[E] = array.distinct.asInstanceOf[Arr[E]]
		@inline def distinctBy[A](f :E => A) :Arr[E] = array.distinctBy(f).asInstanceOf[Arr[E]]


		@inline def tail :Arr[E] = array.tail.asInstanceOf[Arr[E]]
		@inline def init :Arr[E] = array.init.asInstanceOf[Arr[E]]

		@inline def tails :Iterator[Arr[E]] = array.tails.castFrom[Iterator[Array[E]], Iterator[Arr[E]]]
		@inline def inits :Iterator[Arr[E]] = array.inits.castFrom[Iterator[Array[E]], Iterator[Arr[E]]]
		@inline def grouped(size :Int) :Iterator[Arr[E]] =
			array.grouped(size).castFrom[Iterator[Array[E]], Iterator[Arr[E]]]

		@inline def sliding(size :Int, step :Int = 1) :Iterator[Arr[E]] =
			array.sliding(size, step).castFrom[Iterator[Array[E]], Iterator[Arr[E]]]

		@inline def combinations(n :Int) :Iterator[Arr[E]] =
			array.combinations(n).castFrom[Iterator[Array[E]], Iterator[Arr[E]]]

		@inline def permutations :Iterator[Arr[E]] =
			array.permutations.castFrom[Iterator[Array[E]], Iterator[Arr[E]]]

		@inline def iterator :Iterator[E] = array.iterator //ArrayIterator(array)
		@inline def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S = array.stepper
		@inline def reverseIterator :Iterator[E] = array.reverseIterator

		@inline def withFilter(p :E => Boolean) :ArrayLikeWithFilter[Arr, E] = new ArrayLikeWithFilter(p, array)
		@inline def filter(p :E => Boolean) :Arr[E] = array.filter(p).asInstanceOf[Arr[E]]
		@inline def filterNot(p :E => Boolean) :Arr[E] = array.filterNot(p).asInstanceOf[Arr[E]]
		@inline def partition(p :E => Boolean) :(Arr[E], Arr[E]) =
			array.partition(p).asInstanceOf[(Arr[E], Arr[E])]

		@inline def collectFirst[A](pf :PartialFunction[E, A]) :Option[A] = array.collectFirst(pf)
		@inline def collect[A](pf :PartialFunction[E, A])(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.collect(pf)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]

		@inline def partitionMap[E1, E2](f: E => Either[E1, E2])
		                                (implicit tag1 :Maybe[ClassTag[E1]], tag2 :Maybe[ClassTag[E2]])
				:(Arr[E1], Arr[E2]) =
			array.partitionMap(f)(
				tag1 getOrElse ClassTag.Any.castParam[E1], tag2 getOrElse ClassTag.Any.castParam[E2]
			).castFrom[(Array[E1], Array[E2]), (Arr[E1], Arr[E2])]

		@inline def map[A](f :E => A)(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.map(f)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]

		@inline def flatMap[A](f :E => IterableOnce[A])(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.flatMap(f)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]

		@inline def flatMap[As, A](f :E => As)(implicit asIterable :As => Iterable[A], m :ClassTag[A]) :Arr[A] =
			array.flatMap(f).asInstanceOf[Arr[A]]

		@inline def flatten[A](implicit asIterable :E => IterableOnce[A], tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.flatten(asIterable, tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]

		@inline def groupBy[K](f: E => K) :Map[K, Arr[E]] =
			array.groupBy(f).castFrom[Map[K, Array[E]], Map[K, Arr[E]]]

		@inline def groupMap[K, A](key :E => K)(f :E => A)(implicit tag :Maybe[ClassTag[A]]) :Map[K, Arr[A]] =
			array.groupMap(key)(f)(tag getOrElse ClassTag.Any.castParam[A]).castFrom[Map[K, Array[A]], Map[K, Arr[A]]]

		@inline def tapEach[U](f :E => U) :Arr[E] = array.tapEach(f).asInstanceOf[Arr[E]]
		@inline def foreach[U](f :E => U) :Unit = array.foreach(f)

		@inline def zipWithIndex :Arr[(E, Int)] = array.zipWithIndex.asInstanceOf[Arr[(E, Int)]]
		@inline def zip[A](that :IterableOnce[A]) :Arr[(E, A)] = array.zip(that).asInstanceOf[Arr[(E, A)]]
		@inline def zip[A](that :Array[A]) :Arr[(E, A)] = array.zip(new ArrayAsSeq(that)).asInstanceOf[Arr[(E, A)]]

		@inline def lazyZip[A](that: Iterable[A]) :LazyZip2[E, A, Arr[E]] =
			array.lazyZip(that).asInstanceOf[LazyZip2[E, A, Arr[E]]]

		@inline def lazyZip[A](that: Array[A]) :LazyZip2[E, A, Arr[E]] =
			array.lazyZip(new ArrayAsSeq(that)).asInstanceOf[LazyZip2[E, A, Arr[E]]]

		@inline def zipAll[A >: E, B](that :Iterable[B], thisElem :A, thatElem :B) :Arr[(A, B)] =
			array.zipAll(that, thisElem, thatElem).castFrom[Array[(A, B)], Arr[(A, B)]]

		@inline def zipAll[A >: E, B](that :Array[B], thisElem :A, thatElem :B) :Arr[(A, B)] =
			array.zipAll(new ArrayAsSeq(that), thisElem, thatElem).castFrom[Array[(A, B)], Arr[(A, B)]]

		@inline def unzip[E1, E2](implicit asPair :E => (E1, E2), ct1 :ClassTag[E1], ct2 :ClassTag[E2])
				:(Arr[E1], Arr[E2]) =
			array.unzip.castFrom[(Array[E1], Array[E2]), (Arr[E1], Arr[E2])]

		@inline def unzip3[E1, E2, E3](implicit asTriple :E => (E1, E2, E3), ct1 :ClassTag[E1], ct2 :ClassTag[E2],
		                               ct3 :ClassTag[E3]) :(Arr[E1], Arr[E2], Arr[E3]) =
			array.unzip3.castFrom[(Array[E1], Array[E2], Array[E3]), (Arr[E1], Arr[E2], Arr[E3])]


		@inline def :+[A >: E](x :A)(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.appended(x)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]

		@inline def +:[A >: E](x :A)(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.prepended(x)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]

		@inline def appended[A >: E](x :A)(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.appended(x)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]

		@inline def prepended[A >: E](x :A)(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.prepended(x)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]


		@inline def ++[A >: E](suffix :IterableOnce[A])(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.appendedAll(suffix)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]

		@inline def ++[A >: E](suffix :Array[A])(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.appendedAll(suffix)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]

		@inline def concat[A >: E](suffix :IterableOnce[A])(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.appendedAll(suffix)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]

		@inline def concat[A >: E](suffix :Array[A])(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.appendedAll(suffix)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]


		@inline def :++[A >: E](suffix :IterableOnce[A])(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.appendedAll(suffix)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]

		@inline def :++[A >: E](suffix :Array[A])(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.appendedAll(suffix)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]

		@inline def appendedAll[A >: E](suffix :IterableOnce[A])(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.appendedAll(suffix)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]

		@inline def appendedAll[A >: E](suffix :Array[A])(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.appendedAll(suffix)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]


		@inline def ++:[A >: E](suffix :IterableOnce[A])(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.prependedAll(suffix)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]

		@inline def ++:[A >: E](suffix :Array[A])(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.prependedAll(suffix)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]

		@inline def prependedAll[A >: E](suffix :IterableOnce[A])(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.prependedAll(suffix)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]

		@inline def prependedAll[A >: E](suffix :Array[A])(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.prependedAll(suffix)(tag getOrElse ClassTag.Any.castParam[A]).asInstanceOf[Arr[A]]

	//	@inline def :+[A >: E :ClassTag](x :A) :Arr[A] = array.appended(x).asInstanceOf[Arr[E]]
	//	@inline def +:[A >: E :ClassTag](x :A) :Arr[A] = array.prepended(x).asInstanceOf[Arr[E]]
	//	@inline def appended[A >: E :ClassTag](x :A) :Arr[A] = array.appended(x).asInstanceOf[Arr[E]]
	//	@inline def prepended[A >: E :ClassTag](x :A) :Arr[A] = array.prepended(x).asInstanceOf[Arr[E]]
	//
	//	@inline def :+[A >: E :ClassTag](x :A) :Arr[A] = array.appended(x).asInstanceOf[Arr[E]]
	//	@inline def +:[A >: E :ClassTag](x :A) :Arr[A] = array.prepended(x).asInstanceOf[Arr[E]]
	//	@inline def appended[A >: E :ClassTag](x :A) :Arr[A] = array.appended(x).asInstanceOf[Arr[E]]
	//	@inline def prepended[A >: E :ClassTag](x :A) :Arr[A] = array.prepended(x).asInstanceOf[Arr[E]]
	//
	//	@inline def ++[A >: E :ClassTag](suffix :IterableOnce[A]) :Arr[A] =
	//		array.appendedAll(suffix).asInstanceOf[Arr[A]]
	//
	//	@inline def ++[A >: E :ClassTag](suffix :Arr[A]) :Arr[A] =
	//		array.appendedAll(suffix.asInstanceOf[Array[A]]).asInstanceOf[Arr[A]]
	//
	//	@inline def concat[A >: E :ClassTag](suffix :IterableOnce[A]) :Arr[A] =
	//		array.appendedAll(suffix).asInstanceOf[Arr[A]]
	//
	//	@inline def concat[A >: E :ClassTag](suffix :Arr[A]) :Arr[A] =
	//		array.appendedAll(suffix.asInstanceOf[Array[A]]).asInstanceOf[Arr[A]]
	//
	//
	//	@inline def :++[A >: E :ClassTag](suffix :IterableOnce[A]) :Arr[A] =
	//		array.appendedAll(suffix).asInstanceOf[Arr[A]]
	//
	//	@inline def :++[A >: E :ClassTag](suffix :Arr[A]) :Arr[A] =
	//		array.appendedAll(suffix.asInstanceOf[Array[A]]).asInstanceOf[Arr[A]]
	//
	//	@inline def appendedAll[A >: E :ClassTag](suffix :IterableOnce[A]) :Arr[A] =
	//		array.appendedAll(suffix).asInstanceOf[Arr[A]]
	//
	//	@inline def appendedAll[A >: E :ClassTag](suffix :Arr[A]) :Arr[A] =
	//		array.appendedAll(suffix.asInstanceOf[Array[A]]).asInstanceOf[Arr[A]]
	//
	//
	//	@inline def ++:[A >: E :ClassTag](suffix :IterableOnce[A]) :Arr[A] =
	//		array.prependedAll(suffix).asInstanceOf[Arr[A]]
	//
	//	@inline def ++:[A >: E :ClassTag](suffix :Arr[A]) :Arr[A] =
	//		array.prependedAll(suffix.asInstanceOf[Array[A]]).asInstanceOf[Arr[A]]
	//
	//	@inline def prependedAll[A >: E :ClassTag](suffix :IterableOnce[A]) :Arr[A] =
	//		array.prependedAll(suffix).asInstanceOf[Arr[A]]
	//
	//	@inline def prependedAll[A >: E :ClassTag](suffix :Arr[A]) :Arr[A] =
	//		array.prependedAll(suffix.asInstanceOf[Array[A]]).asInstanceOf[Arr[A]]

	//
	//	@inline def ++(that :Array[E]) :Arr[E] =
	//		array.appendedAll(that)(ClassTag[E](array.getClass.getComponentType)).asInstanceOf[Arr[E]]
	//
	//	@inline def :++(that :Array[E]) :Arr[E] =
	//		array.appendedAll(that)(ClassTag[E](array.getClass.getComponentType)).asInstanceOf[Arr[E]]
	//
	//	@inline def ++:(that :Array[E]) :Arr[E] =
	//		array.prependedAll(that)(ClassTag[E](array.getClass.getComponentType)).asInstanceOf[Arr[E]]
	//
	//	@inline def concat(that :Array[E]) :Arr[E] =
	//		array.appendedAll(that)(ClassTag[E](array.getClass.getComponentType)).asInstanceOf[Arr[E]]
	//
	//	@inline def appendedAll(that :Array[E]) :Arr[E] =
	//		array.appendedAll(that)(ClassTag[E](array.getClass.getComponentType)).asInstanceOf[Arr[E]]
	//
	//	@inline def prependedAll(that :Array[E]) :Arr[E] =
	//		array.prependedAll(that)(ClassTag[E](array.getClass.getComponentType)).asInstanceOf[Arr[E]]

		@inline def diff[A >: E](that :collection.Seq[A]) :Arr[E] = array.diff(that).asInstanceOf[Arr[E]]
		@inline def diff[A >: E](that :Array[A]) :Arr[E] = array.diff(mutable.ArraySeq.make(that)).asInstanceOf[Arr[E]]

		@inline def intersect[A >: E](that :collection.Seq[A]) :Arr[E] =
			array.intersect(that).asInstanceOf[Arr[E]]

		@inline def intersect[A >: E](that :Array[A]) :Arr[E] =
			array.intersect(mutable.ArraySeq.make(that)).asInstanceOf[Arr[E]]

		@inline def transpose[A](implicit asArray :E => Arr[A]): Arr[Arr[A]] =
			array.transpose(asArray.asInstanceOf[E => Array[A]]).castFrom[Array[Array[A]], Arr[Arr[A]]]

		@inline def view :IndexedSeqView[E] = array.view
		@inline def indices :Range = Range(0, array.length)

		@inline def to[C1](factory :Factory[E, C1]) :C1 = array.to(factory)
		@inline def toIterable :Iterable[E] = mutable.ArraySeq.make(array)
		@inline def toSeq :Seq[E] = array.toSeq
		@inline def toIndexedSeq :IndexedSeq[E] = array.toIndexedSeq
		@inline def toList :List[E] = array.toList
		@inline def toVector :Vector[E] = array.toVector
		@inline def toSet[A >: E] :Set[A] = array.toSet
		@inline def toMap[K, V](implicit ev :E <:< (K, V)) :Map[K, V] = array.toMap
		@inline def toBuffer[A >: E]: Buffer[A] = Buffer.from(array)
		@inline def toArray[A >: E :ClassTag] :Array[A] = array.toArray
	//	@inline def toArray[A >: E](implicit tag :Maybe[ClassTag[A]]) :Array[A] =
	//		array.toArray(tag getOrElse ClassTag.Any.castParam[A])

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

		@inline def sameElements(other :Arr[_]) :Boolean = array sameElements other.asInstanceOf[Array[_]]
		@inline def sameElements(other :IterableOnce[_]) :Boolean = array sameElements other
	}


	/** A lazy filtered array. No filtering is applied until one of `foreach`, `map` or `flatMap` is called. */
	class ArrayLikeWithFilter[Arr[_], E] private[collections] (p :E => Boolean, xs :ArrayLikeExtension[Arr, E]) {

		/** Apply `f` to each element for its side effects.
		  * Note: [U] parameter needed to help scalac's type inference.
		  */
		def foreach[U](f :E => U) :Unit = {
			val len = xs.array.length
			var i = 0
			while (i < len) {
				val x = xs.array(i)
				if (p(x)) f(x)
				i += 1
			}
		}

		/** Builds a new array by applying a function to all elements of this array.
		  * @param f      the function to apply to each element.
		  * @tparam A     the element type of the returned array.
		  * @return a new array resulting from applying the given function
		  *         `f` to each element of this array and collecting the results.
		  */
		def map[A](f :E => A)(implicit tag :Maybe[ClassTag[A]]) :Arr[A] = {
			val b = ArrayBuilder.make[A](tag getOrElse ClassTag.Any.castParam[A])
			var i = 0
			while (i < xs.length) {
				val x = xs(i)
				if (p(x)) b += f(x)
				i = i + 1
			}
			b.result().asInstanceOf[Arr[A]]
		}

		/** Builds a new array by applying a function to all elements of this array
		  * and using the elements of the resulting collections.
		  *
		  * @param f      the function to apply to each element.
		  * @tparam A     the element type of the returned array.
		  * @return a new array resulting from applying the given collection-valued function
		  *         `f` to each element of this array and concatenating the results.
		  */
		def flatMap[A](f :E => IterableOnce[A])(implicit tag :Maybe[ClassTag[A]]) :Arr[A] = {
			val b = ArrayAsSeq.newBuilder(tag getOrElse ClassTag.Any.castParam[A])
			var i = 0
			while (i < xs.length) {
				val x = xs(i)
				if (p(x)) b ++= f(xs(i))
				i += 1
			}
			b.result().asInstanceOf[Arr[A]]
		}

		def flatMap[As, A](f :E => As)(implicit asIterable :As => Iterable[A], tag :Maybe[ClassTag[A]]) :Arr[A] =
			flatMap[A](x => asIterable(f(x)))

		/** Creates a new non-strict filter which combines this filter with the given predicate. */
		def withFilter(q :E => Boolean) :ArrayLikeWithFilter[Arr, E] =
			new ArrayLikeWithFilter[Arr, E](a => p(a) && q(a), xs)
	}

}
*/




private[collections] sealed abstract class IArrayRank2Implicits {
	@inline implicit def wrap[E](array :IArray[E]) :IndexedSeq[E] = ArraySeq.unsafeWrapArray(array.asInstanceOf[Array[E]])
}

private[collections] sealed abstract class IArrayRank1Implicits extends IArrayRank2Implicits {
	@inline implicit def wrapByte(array :IArray[Byte]) :ArraySeq.ofByte =
		new ArraySeq.ofByte(array.asInstanceOf[Array[Byte]])

	@inline implicit def wrapShort(array :IArray[Short]) :ArraySeq.ofShort =
		new ArraySeq.ofShort(array.asInstanceOf[Array[Short]])

	@inline implicit def wrapChar(array :IArray[Char]) :ArraySeq.ofChar =
		new ArraySeq.ofChar(array.asInstanceOf[Array[Char]])

	@inline implicit def wrapInt(array :IArray[Int]) :ArraySeq.ofInt =
		new ArraySeq.ofInt(array.asInstanceOf[Array[Int]])

	@inline implicit def wrapLong(array :IArray[Long]) :ArraySeq.ofLong =
		new ArraySeq.ofLong(array.asInstanceOf[Array[Long]])

	@inline implicit def wrapFloat(array :IArray[Float]) :ArraySeq.ofFloat =
		new ArraySeq.ofFloat(array.asInstanceOf[Array[Float]])

	@inline implicit def wrapDouble(array :IArray[Double]) :ArraySeq.ofDouble =
		new ArraySeq.ofDouble(array.asInstanceOf[Array[Double]])

	@inline implicit def wrapBoolean(array :IArray[Boolean]) :ArraySeq.ofBoolean =
		new ArraySeq.ofBoolean(array.asInstanceOf[Array[Boolean]])

	@inline implicit def wrapRef[E <: AnyRef](array :IArray[E]) :ArraySeq.ofRef[E] =
		new ArraySeq.ofRef(array.asInstanceOf[Array[E]])
}


/** Factory of immutable arrays: `IArray` backed by an `Array`, without exposing any mutating methods.
  * @define Coll `IArray`
  * @define coll immutable array
  */
@SerialVersionUID(Ver)
case object IArray extends IArrayRank1Implicits with ClassTagIterableFactory[IArray] {

	@inline class ByteIArrayExtension private[collections] (private val array :Array[Byte]) extends AnyVal {
		@inline def head :Byte = array(0)
		@inline def last :Byte = array(array.length - 1)
		@inline def apply(n :Int) :Byte = array(n)
		@inline def toArraySeq :ArraySeq.ofByte = new ArraySeq.ofByte(array)
	}
	@inline class ShortIArrayExtension private[collections] (private val array :Array[Short]) extends AnyVal {
		@inline def head :Short = array(0)
		@inline def last :Short = array(array.length - 1)
		@inline def apply(n :Int) :Short = array(n)
		@inline def toArraySeq :ArraySeq.ofShort = new ArraySeq.ofShort(array)
	}
	@inline class CharIArrayExtension private[collections] (private val array :Array[Char]) extends AnyVal {
		@inline def head :Char = array(0)
		@inline def last :Char = array(array.length - 1)
		@inline def apply(n :Int) :Char = array(n)
		@inline def toArraySeq :ArraySeq.ofChar = new ArraySeq.ofChar(array)
	}
	@inline class IntIArrayExtension private[collections] (private val array :Array[Int]) extends AnyVal {
		@inline def head :Int = array(0)
		@inline def last :Int = array(array.length - 1)
		@inline def apply(n :Int) :Int = array(n)
		@inline def toArraySeq :ArraySeq.ofInt = new ArraySeq.ofInt(array)
	}
	@inline class LongIArrayExtension private[collections] (private val array :Array[Long]) extends AnyVal {
		@inline def head :Long = array(0)
		@inline def last :Long = array(array.length - 1)
		@inline def apply(n :Int) :Long = array(n)
		@inline def toArraySeq :ArraySeq.ofLong = new ArraySeq.ofLong(array)
	}
	@inline class FloatIArrayExtension private[collections] (private val array :Array[Float]) extends AnyVal {
		@inline def head :Float = array(0)
		@inline def last :Float = array(array.length - 1)
		@inline def apply(n :Int) :Float = array(n)
		@inline def toArraySeq :ArraySeq.ofFloat = new ArraySeq.ofFloat(array)
	}
	@inline class DoubleIArrayExtension private[collections] (private val array :Array[Double]) extends AnyVal {
		@inline def head :Double = array(0)
		@inline def last :Double = array(array.length - 1)
		@inline def apply(n :Int) :Double = array(n)
		@inline def toArraySeq :ArraySeq.ofDouble = new ArraySeq.ofDouble(array)
	}
	@inline class BooleanIArrayExtension private[collections] (private val array :Array[Boolean]) extends AnyVal {
		@inline def head :Boolean = array(0)
		@inline def last :Boolean = array(array.length - 1)
		@inline def apply(n :Int) :Boolean = array(n)
		@inline def toArraySeq :ArraySeq.ofBoolean = new ArraySeq.ofBoolean(array)
	}
	@inline class RefIArrayExtension[E <: AnyRef] private[collections] (private val array :Array[E]) extends AnyVal {
		@inline def head :E = array(0)
		@inline def last :E = array(array.length - 1)
		@inline def apply(n :Int) :E = array(n)
		@inline def toArraySeq :ArraySeq.ofRef[E] = new ArraySeq.ofRef(array)
	}
	class GenericIArrayExtension[E] private[collections] (private val array :Array[E]) extends AnyVal {
		@inline def head :E = array(0)
		@inline def last :E = array(array.length - 1)
		@inline def apply(n :Int) :E = array(n)
		@inline def toArraySeq :ArraySeq[E] = ArraySeq.unsafeWrapArray(array)
	}


	class IArrayExtension[E](private[IArray] val array :Array[E])
		extends AnyVal
	{
		@inline def knownSize :Int = array.length
		@inline def length :Int = array.length
		@inline def size :Int = array.length
		@inline def isEmpty  :Boolean = array.length == 0
		@inline def nonEmpty :Boolean = array.length > 0
		@inline def sizeIs :Int = array.length
		@inline def lengthIs :Int = array.length
		@inline def sizeCompare(otherSize :Int) :Int= Integer.compare(array.length, otherSize)
		@inline def lengthCompare(len :Int) :Int = Integer.compare(array.length, len)

//		@inline def head :E = array.head
//		@inline def last :E = array.last
		@inline def headOption :Option[E] = array.headOption
		@inline def lastOption :Option[E] = array.lastOption

		@inline def forall(p :E => Boolean) :Boolean = array.forall(p)
		@inline def exists(p :E => Boolean) :Boolean = array.exists(p)
		@inline def count(p :E => Boolean) :Int = array.count(p)
		@inline def find(p :E => Boolean) :Option[E] = array.find(p)
		@inline def indexWhere(p :E => Boolean) :Int = array.indexWhere(p)
		@inline def lastIndexWhere(p :E => Boolean) :Int = array.lastIndexWhere(p)
		@inline def indexOf(elem :E, from :Int = 0) :Int = array.indexOf(elem, from)
		@inline def lastIndexOf(elem :E, end :Int = 0) :Int = array.lastIndexOf(elem, end)
		@inline def contains(elem :E) :Boolean = array.contains(elem)

		@inline def segmentLength(p :E => Boolean, from :Int = 0) :Int = array.segmentLength(p, from)
		@inline def startsWith[A >: E](that :IterableOnce[A]) :Boolean = array.startsWith(that, 0)
		@inline def startsWith[A >: E](that :IterableOnce[A], offset :Int) :Boolean = array.startsWith(that, offset)

		@inline def startsWith[A >: E](that :IArray[A], offset :Int) :Boolean =
			array.startsWith(that.asInstanceOf[Array[A]], offset)

		@inline def startsWith[A >: E](that :IArray[A]) :Boolean =
			array.startsWith(that.asInstanceOf[Array[A]], 0)

		@inline def endsWith[A >: E](that :Iterable[A]) :Boolean = array.endsWith(that)
		@inline def endsWith[A >: E](that :IArray[A]) :Boolean = array.endsWith(that.asInstanceOf[Array[A]])

		@inline def corresponds[A](that :IArray[A])(p :(E, A) => Boolean) :Boolean =
			array.corresponds(new IArrayAsSeq(that))(p)

		@inline def sum[A >: E](implicit num :Numeric[A]) :A = array.sum[A]
		@inline def product[A >: E](implicit num :Numeric[A]) :A = array.product[A]
		@inline def min[A >: E](implicit ord :Ordering[A]) :E = array.min[A]
		@inline def minOption[A >: E](implicit ord :Ordering[A]) :Option[E] = array.minOption[A]
		@inline def max[A >: E](implicit ord :Ordering[A]) :E = array.max[A]
		@inline def maxOption[A >: E](implicit ord :Ordering[A]) :Option[E] = array.maxOption[A]
		@inline def maxBy[A](f :E => A)(implicit cmp :Ordering[A]) :E = array.maxBy(f)
		@inline def maxByOption[A](f :E => A)(implicit cmp :Ordering[A]) :Option[E] = array.maxByOption(f)
		@inline def minBy[A](f :E => A)(implicit cmp :Ordering[A]) :E = array.minBy(f)
		@inline def minByOption[A](f :E => A)(implicit cmp :Ordering[A]) :Option[E] = array.minByOption(f)

		@inline def /: [A](z :A)(op :(A, E) => A) :A = array.foldLeft[A](z)(op)
		@inline def :\ [A](z :A)(op :(E, A) => A) :A = array.foldRight[A](z)(op)

		@inline def foldLeft[A](z :A)(op :(A, E) => A) :A = array.foldLeft(z)(op)
		@inline def foldRight[A](z :A)(op :(E, A) => A) :A = array.foldRight(z)(op)
		@inline def fold[A >: E](z :A)(op :(A, A) => A) :A = array.fold(z)(op)
		@inline def reduce[A >: E](op :(A, A) => A) :A = array.reduce(op)
		@inline def reduceOption[A >: E](op :(A, A) => A) :Option[A] = array.reduceOption(op)
		@inline def reduceLeft[A >: E](op :(A, E) => A) :A = array.reduceLeft(op)
		@inline def reduceRight[A >: E](op :(E, A) => A) :A = array.reduceRight(op)
		@inline def reduceLeftOption[A >: E](op :(A, E) => A) :Option[A] = array.reduceLeftOption(op)
		@inline def reduceRightOption[A >: E](op :(E, A) => A) :Option[A] = array.reduceRightOption(op)

		@inline def scanLeft[A :ClassTag](z :A)(op :(A, E) => A) :IArray[A] =
			array.scanLeft(z)(op).asInstanceOf[IArray[A]]

		@inline def scanRight[A :ClassTag](z :A)(op :(E, A) => A) :IArray[A] =
			array.scanRight(z)(op).asInstanceOf[IArray[A]]

		@inline def scan[A >: E :ClassTag](z :A)(op :(A, A) => A) :IArray[A] =
			array.scan(z)(op).asInstanceOf[IArray[E]]

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

		@inline def groupBy[K](f: E => K) :Map[K, IArray[E]] =
			array.groupBy(f).castFrom[Map[K, Array[E]], Map[K, IArray[E]]]

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

		@inline def :+[A >: E :ClassTag](x :A) :IArray[A] = array.appended(x).asInstanceOf[IArray[E]]
		@inline def +:[A >: E :ClassTag](x :A) :IArray[A] = array.prepended(x).asInstanceOf[IArray[E]]
		@inline def appended[A >: E :ClassTag](x :A) :IArray[A] = array.appended(x).asInstanceOf[IArray[E]]
		@inline def prepended[A >: E :ClassTag](x :A) :IArray[A] = array.prepended(x).asInstanceOf[IArray[E]]

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
		@inline def to[C1](factory :Factory[E, C1]) :C1 = array.to(factory)
		@inline def toSeq :Seq[E] = ArraySeq.unsafeWrapArray(array)
		@inline def toIndexedSeq :IndexedSeq[E] = ArraySeq.unsafeWrapArray(array)
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


	/** A lazy filtered array. No filtering is applied until one of `foreach`, `map` or `flatMap` is called. */
	class WithFilter[E] private[IArray] (p :E => Boolean, xs :Array[E]) {

		/** Apply `f` to each element for its side effects.
		  * Note: [U] parameter needed to help scalac's type inference.
		  */
		def foreach[U](f :E => U) :Unit = {
			val len = xs.array.length
			var i = 0
			while (i < len) {
				val x = xs(i)
				if (p(x)) f(x)
				i += 1
			}
		}

		/** Builds a new array by applying a function to all elements of this array.
		  * @param f      the function to apply to each element.
		  * @tparam A     the element type of the returned array.
		  * @return a new array resulting from applying the given function
		  *         `f` to each element of this array and collecting the results.
		  */
		def map[A :ClassTag](f :E => A) :IArray[A] = {
			val b = IArray.newBuilder[A]
			var i = 0
			while (i < xs.length) {
				val x = xs(i)
				if (p(x)) b += f(x)
				i = i + 1
			}
			b.result()
		}

		/** Builds a new array by applying a function to all elements of this array
		  * and using the elements of the resulting collections.
		  *
		  * @param f      the function to apply to each element.
		  * @tparam A     the element type of the returned array.
		  * @return a new array resulting from applying the given collection-valued function
		  *         `f` to each element of this array and concatenating the results.
		  */
		def flatMap[A :ClassTag](f :E => IterableOnce[A]) :IArray[A] = {
			val b = IArray.newBuilder[A]
			var i = 0
			while (i < xs.length) {
				val x = xs(i)
				if (p(x)) b ++= f(xs(i))
				i += 1
			}
			b.result()
		}

		def flatMap[As, A](f :E => As)(implicit asIterable :As => Iterable[A], tag :ClassTag[A]) :IArray[A] =
			flatMap[A](x => asIterable(f(x)))

		/** Creates a new non-strict filter which combines this filter with the given predicate. */
		def withFilter(q :E => Boolean) :WithFilter[E] =
			new WithFilter[E](a => p(a) && q(a), xs)
	}



	val emptyBooleanIArray :IArray[Boolean] = new Array[Boolean](0).asInstanceOf[IArray[Boolean]]
	val emptyByteIArray    :IArray[Byte]    = new Array[Byte](0).asInstanceOf[IArray[Byte]]
	val emptyCharIArray    :IArray[Char]    = new Array[Char](0).asInstanceOf[IArray[Char]]
	val emptyDoubleIArray  :IArray[Double]  = new Array[Double](0).asInstanceOf[IArray[Double]]
	val emptyFloatIArray   :IArray[Float]   = new Array[Float](0).asInstanceOf[IArray[Float]]
	val emptyIntIArray     :IArray[Int]     = new Array[Int](0).asInstanceOf[IArray[Int]]
	val emptyLongIArray    :IArray[Long]    = new Array[Long](0).asInstanceOf[IArray[Long]]
	val emptyShortIArray   :IArray[Short]   = new Array[Short](0).asInstanceOf[IArray[Short]]
	val emptyObjectIArray  :IArray[Object]  = new Array[Object](0).asInstanceOf[IArray[Object]]
	val emptyUnitIArray    :IArray[Unit]    = new Array[Unit](0).asInstanceOf[IArray[Unit]]


//	def wrap[E](array :IArray[E]) :IndexedSeqOps[E, IArray, IArray[E]] = new IArrayAsSeq(array)

	override def from[E :ClassTag](it :IterableOnce[E]) :IArray[E] = it match {
		case elems :Iterable[E] if elems.isEmpty => Array.empty[E].asInstanceOf[IArray[E]]
		case iter :Iterator[E] if !iter.hasNext  => Array.empty[E].asInstanceOf[IArray[E]]
		case elems :IArrayAsSeq[E]               => elems.coll
		case elems :Iterable[E]                  => elems.toArray[E].asInstanceOf[IArray[E]]
		case _                                   => it.iterator.toArray[E].asInstanceOf[IArray[E]]
	}

	override def empty[E :ClassTag] :IArray[E] = ArrayAsSeq.empty[E].asInstanceOf[IArray[E]]

	def empty[E](elementType :Class[E]) :IArray[E] = ArrayAsSeq.empty(elementType).asInstanceOf[IArray[E]]

	def one[E :ClassTag](elem :E) :IArray[E] = {
		val a = new Array[E](1)
		a(0) = elem
		a.asInstanceOf[IArray[E]]
	}

	override def newBuilder[E :ClassTag] :Builder[E, IArray[E]] =
		ArrayAsSeq.newBuilder[E].asInstanceOf[Builder[E, IArray[E]]]

	def newBuilder[E](elementType :Class[E]) :Builder[E, IArray[E]] =
		ArrayAsSeq.newBuilder(elementType).asInstanceOf[Builder[E, IArray[E]]]

	def unapplySeq[E](array :IArray[E]) :UnapplySeqWrapper[E] =
		new UnapplySeqWrapper(array.castFrom[IArray[E], Array[E]])


	/** A factory of `IArray[E]` instances represented in runtime always as `Array[Any]` (i.e., `Object[]`).
	  * Value types are always stored in their boxed form. The advantage is the lack of dependence on `ClassTag[E]`,
	  * and thus the ability to extend the basic `IterableFactory`.
	  */
	object untagged extends IterableFactory[IArray] {
		private[this] val Empty :IArray[Nothing] = new Array[Any](0).asInstanceOf[IArray[Nothing]]

		override def empty[E] :IArray[E] = Empty

		override def from[E](it :IterableOnce[E]) :IArray[E] = it match {
			case it :Iterable[E] if it.isEmpty => Empty
			case it :Iterator[E] if !it.hasNext => Empty
			case it :IArrayAsSeq[E] => it.coll
			case it :Iterable[E] => it.toArray[Any].asInstanceOf[IArray[E]]
			case _ => it.iterator.toArray[Any].asInstanceOf[IArray[E]]
		}

		def one[E](elem :E) :IArray[E] = {
			val a = new Array[Any](1)
			a(0) = elem
			a.asInstanceOf[IArray[E]]
		}

		override def newBuilder[E] :Builder[E, IArray[E]] = Array.newBuilder[Any].asInstanceOf[Builder[E, IArray[E]]]
	}
}




/** Factory of erased arrays: `RefArray[E]` represented in runtime by an `Array[Any]`.
  * @define Coll `RefArray`
  * @define coll immutable array
  */
@SerialVersionUID(Ver)
case object RefArray extends IterableFactory[Array] {
	private[this] val Empty = Array.emptyObjectArray
	override def empty[E] :Array[E] = Empty.asInstanceOf[Array[E]]

	def one[E](elem :E) :Array[E] = {
		val a = new Array[Any](1)
		a(0) = elem
		a.asInstanceOf[Array[E]]
	}

	/** Build an array with erased element type from the iterable collection.
	  *  @param  it the iterable collection
	  *  @return an array consisting of elements of the iterable collection
	  */
	override def from[E](it :IterableOnce[E]) :Array[E] = it match {
		case it :Iterable[E] if it.isEmpty => Empty.asInstanceOf[Array[E]]
		case it :Iterator[E] if it.isEmpty => Empty.asInstanceOf[Array[E]]
		case it :ArrayAsSeq[E] if it.coll.getClass.getComponentType == classOf[AnyRef] => it.coll
		case it :Iterable[E] => it.toArray[Any].asInstanceOf[Array[E]]
		case _ => it.iterator.toArray[Any].asInstanceOf[Array[E]]
	}

	override def newBuilder[E] :Builder[E, Array[E]] =
		ArrayAsSeq.newBuilder[Any](classOf[Any]).asInstanceOf[Builder[E, Array[E]]]

	def unapplySeq[E](array :Array[E]) :UnapplySeqWrapper[E] =
		new UnapplySeqWrapper(array)

}
