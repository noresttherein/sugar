package net.noresttherein.sugar.collections

import scala.Array.UnapplySeqWrapper
import scala.annotation.nowarn
import scala.collection.{ClassTagIterableFactory, Factory, IndexedSeqView, IterableFactory, LazyZip2, Stepper, StepperShape, View, mutable}
import scala.collection.generic.IsSeq
import scala.collection.immutable.{ArraySeq, IndexedSeqOps}
import scala.collection.mutable.{Buffer, Builder}
import scala.reflect.{ClassTag, classTag}

import net.noresttherein.sugar.collections.ArrayLike.{ArrayLikeExtension, RefArrayLikeExtension}
import net.noresttherein.sugar.extensions.{castTypeParam, classNameMethods, saferCasting}
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.witness.Maybe




object ArrayLike {
	class ArrayLikeExtension[E, Arr[X]] private[collections] (private[ArrayLike] val array :Array[Any])
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
		@inline def headOption :Option[E] = array.headOption.asInstanceOf[Option[E]]
		@inline def lastOption :Option[E] = array.lastOption.asInstanceOf[Option[E]]

		@inline def forall(p :E => Boolean) :Boolean = array.forall(p.asInstanceOf[Any => Boolean])
		@inline def exists(p :E => Boolean) :Boolean = array.exists(p.asInstanceOf[Any => Boolean])
		@inline def count(p :E => Boolean) :Int = array.count(p.asInstanceOf[Any => Boolean])
		@inline def find(p :E => Boolean) :Option[E] = array.find(p.asInstanceOf[Any => Boolean]).asInstanceOf[Option[E]]
		@inline def indexWhere(p :E => Boolean, from :Int = 0) :Int =
			array.indexWhere(p.asInstanceOf[Any => Boolean], from)
		@inline def lastIndexWhere(p :E => Boolean, end :Int = Int.MaxValue) :Int =
			array.lastIndexWhere(p.asInstanceOf[Any => Boolean], end)
		@inline def indexOf(elem :E, from :Int = 0) :Int = array.indexOf(elem, from)
		@inline def lastIndexOf(elem :E, end :Int = 0) :Int = array.lastIndexOf(elem, end)
		@inline def contains(elem :E) :Boolean = array.contains(elem)

		@inline def segmentLength(p :E => Boolean, from :Int = 0) :Int =
			array.segmentLength(p.asInstanceOf[Any => Boolean], from)

		@inline def startsWith[A >: E](that :IterableOnce[A]) :Boolean =
			array.startsWith(that.asInstanceOf[IterableOnce[Any]], 0)

		@inline def startsWith[A >: E](that :IterableOnce[A], offset :Int) :Boolean =
			array.startsWith(that.asInstanceOf[IterableOnce[Any]], offset)

		@inline def startsWith[A >: E](that :ArrayLike[A], offset :Int) :Boolean =
			array.startsWith(that.asInstanceOf[Array[Any]], offset)

		@inline def startsWith[A >: E](that :ArrayLike[A]) :Boolean =
			array.startsWith(that.asInstanceOf[Array[Any]], 0)

		@inline def endsWith[A >: E](that :Iterable[A]) :Boolean = array.endsWith(that.asInstanceOf[Iterable[Any]])
		@inline def endsWith[A >: E](that :ArrayLike[A]) :Boolean = array.endsWith(that.asInstanceOf[Array[Any]])

		@inline def corresponds[A](that :IterableOnce[A])(p :(E, A) => Boolean) :Boolean =
			array.corresponds(that.asInstanceOf[IterableOnce[Any]])(p.asInstanceOf[(Any, Any) => Boolean])

		@inline def corresponds[A](that :ArrayLike[A])(p :(E, A) => Boolean) :Boolean =
			array.corresponds(new ArrayAsSeq(that.asInstanceOf[Array[Any]]))(p.asInstanceOf[(Any, Any) => Boolean])

		@inline def sum[A >: E](implicit num :Numeric[A]) :A = array.sum(num.castParam[Any]).asInstanceOf[A]
		@inline def product[A >: E](implicit num :Numeric[A]) :A = array.product(num.castParam[Any]).asInstanceOf[A]
		@inline def min[A >: E](implicit ord :Ordering[A]) :E = array.min(ord.castParam[Any]).asInstanceOf[E]
		@inline def max[A >: E](implicit ord :Ordering[A]) :E = array.max(ord.castParam[Any]).asInstanceOf[E]
		@inline def maxOption[A >: E](implicit ord :Ordering[A]) :Option[E] = array.maxOption(ord.castParam[Any]).asInstanceOf[Option[E]]
		@inline def minOption[A >: E](implicit ord :Ordering[A]) :Option[E] = array.minOption(ord.castParam[Any]).asInstanceOf[Option[E]]
		@inline def minBy[A](f :E => A)(implicit cmp :Ordering[A]) :E = array.minBy(f.asInstanceOf[Any => A]).asInstanceOf[E]
		@inline def maxBy[A](f :E => A)(implicit cmp :Ordering[A]) :E = array.maxBy(f.asInstanceOf[Any => A]).asInstanceOf[E]
		@inline def minByOption[A](f :E => A)(implicit cmp :Ordering[A]) :Option[E] = array.minByOption(f.asInstanceOf[Any => A]).asInstanceOf[Option[E]]
		@inline def maxByOption[A](f :E => A)(implicit cmp :Ordering[A]) :Option[E] = array.maxByOption(f.asInstanceOf[Any => A]).asInstanceOf[Option[E]]

		@inline def /: [A](z :A)(op :(A, E) => A) :A = array.foldLeft[A](z)(op.asInstanceOf[(A, Any) => A])
		@inline def :\ [A](z :A)(op :(E, A) => A) :A = array.foldRight[A](z)(op.asInstanceOf[(Any, A) => A])

		@inline def foldLeft[A](z :A)(op :(A, E) => A) :A = array.foldLeft(z)(op.asInstanceOf[(A, Any) => A])
		@inline def foldRight[A](z :A)(op :(E, A) => A) :A = array.foldRight(z)(op.asInstanceOf[(Any, A) => A])
		@inline def fold[A >: E](z :A)(op :(A, A) => A) :A = array.fold(z)(op.asInstanceOf[(Any, Any) => Any]).asInstanceOf[A]
		@inline def reduce[A >: E](op :(A, A) => A) :A = array.reduce(op.asInstanceOf[(Any, Any) => Any]).asInstanceOf[A]
		@inline def reduceOption[A >: E](op :(A, A) => A) :Option[A] = array.reduceOption(op.asInstanceOf[(Any, Any) => Any]).asInstanceOf[Option[A]]
		@inline def reduceLeft[A >: E](op :(A, E) => A) :A = array.reduceLeft(op.asInstanceOf[(Any, Any) => Any]).asInstanceOf[A]
		@inline def reduceRight[A >: E](op :(E, A) => A) :A = array.reduceRight(op.asInstanceOf[(Any, Any) => Any]).asInstanceOf[A]
		@inline def reduceLeftOption[A >: E](op :(A, E) => A) :Option[A] = array.reduceLeftOption(op.asInstanceOf[(Any, Any) => Any]).asInstanceOf[Option[A]]
		@inline def reduceRightOption[A >: E](op :(E, A) => A) :Option[A] = array.reduceRightOption(op.asInstanceOf[(Any, Any) => Any]).asInstanceOf[Option[A]]

//		@inline def scanLeft[A :ClassTag](z :A)(op :(A, E) => A) :Arr[A] =
//			array.scanLeft(z)(op.asInstanceOf[(A, Any) => A]).asInstanceOf[Arr[A]]
//
//		@inline def scanRight[A :ClassTag](z :A)(op :(E, A) => A) :Arr[A] =
//			array.scanRight(z)(op.asInstanceOf[(Any, A) => A]).asInstanceOf[Arr[A]]
//
//		@inline def scan[A >: E :ClassTag](z :A)(op :(A, A) => A) :Arr[A] =
//			array.scan(z)(op.asInstanceOf[(Any, Any) => Any]).asInstanceOf[Arr[E]]

		@inline def take(n :Int) :Arr[E] = array.take(n).asInstanceOf[Arr[E]]
		@inline def drop(n :Int) :Arr[E] = array.drop(n).asInstanceOf[Arr[E]]
		@inline def takeRight(n :Int) :Arr[E] = array.takeRight(n).asInstanceOf[Arr[E]]
		@inline def dropRight(n :Int) :Arr[E] = array.dropRight(n).asInstanceOf[Arr[E]]
		@inline def takeWhile(p :E => Boolean) :Arr[E] = array.takeWhile(p.asInstanceOf[Any => Boolean]).asInstanceOf[Arr[E]]
		@inline def dropWhile(p :E => Boolean) :Arr[E] = array.dropWhile(p.asInstanceOf[Any => Boolean]).asInstanceOf[Arr[E]]
		@inline def slice(from :Int, until :Int) :Arr[E] = array.slice(from, until).asInstanceOf[Arr[E]]

		@inline def splitAt(n :Int) :(Arr[E], Arr[E]) =
			array.splitAt(n).asInstanceOf[(Arr[E], Arr[E])]

		@inline def span(p :E => Boolean) :(Arr[E], Arr[E]) =
			array.span(p.asInstanceOf[Any => Boolean]).asInstanceOf[(Arr[E], Arr[E])]

		@inline def reverse :Arr[E] = array.reverse.asInstanceOf[Arr[E]]

		@inline def sortWith(lt :(E, E) => Boolean) :Arr[E] = array.sortWith(lt.asInstanceOf[(Any, Any) => Boolean]).asInstanceOf[Arr[E]]
		@inline def sortBy[A](f :E => A)(implicit ord: Ordering[A]): Arr[E] =
			array.sortBy(f.asInstanceOf[Any => A]).asInstanceOf[Arr[E]]

		@inline def sorted[A >: E](implicit ordering :Ordering[A]) :Arr[E] =
			array.sorted(ordering.asInstanceOf[Ordering[Any]]).asInstanceOf[Arr[E]]

		@inline def distinct :Arr[E] = array.distinct.asInstanceOf[Arr[E]]
		@inline def distinctBy[A](f :E => A) :Arr[E] = array.distinctBy(f.asInstanceOf[Any => A]).asInstanceOf[Arr[E]]


		@inline def tail :Arr[E] = array.tail.asInstanceOf[Arr[E]]
		@inline def init :Arr[E] = array.init.asInstanceOf[Arr[E]]

		@inline def tails :Iterator[Arr[E]] = array.tails.castFrom[Iterator[Array[_]], Iterator[Arr[E]]]
		@inline def inits :Iterator[Arr[E]] = array.inits.castFrom[Iterator[Array[_]], Iterator[Arr[E]]]
		@inline def grouped(size :Int) :Iterator[Arr[E]] =
			array.grouped(size).castFrom[Iterator[Array[_]], Iterator[Arr[E]]]

		@inline def sliding(size :Int, step :Int = 1) :Iterator[Arr[E]] =
			array.sliding(size, step).castFrom[Iterator[Array[_]], Iterator[Arr[E]]]

		@inline def combinations(n :Int) :Iterator[Arr[E]] =
			array.combinations(n).castFrom[Iterator[Array[_]], Iterator[Arr[E]]]

		@inline def permutations :Iterator[Arr[E]] =
			array.permutations.castFrom[Iterator[Array[_]], Iterator[Arr[E]]]

		@inline def iterator :Iterator[E] = array.iterator.asInstanceOf[Iterator[E]] //ArrayIterator(array)
		@inline def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S =
			array.asInstanceOf[Array[Any]].stepper(shape.asInstanceOf[StepperShape[Any, S]])

		@inline def reverseIterator :Iterator[E] = array.reverseIterator.asInstanceOf[Iterator[E]]

//		@inline def withFilter(p :E => Boolean) :WithFilter[E] = new WithFilter(p, array)
		@inline def filter(p :E => Boolean) :Arr[E] = array.filter(p.asInstanceOf[Any => Boolean]).asInstanceOf[Arr[E]]
		@inline def filterNot(p :E => Boolean) :Arr[E] = array.filterNot(p.asInstanceOf[Any => Boolean]).asInstanceOf[Arr[E]]
		@inline def partition(p :E => Boolean) :(Arr[E], Arr[E]) =
			array.partition(p.asInstanceOf[Any => Boolean]).asInstanceOf[(Arr[E], Arr[E])]

		@inline def collectFirst[A](pf :PartialFunction[E, A]) :Option[A] = array.collectFirst(pf.asInstanceOf[PartialFunction[Any, A]])
/*
		@inline def collect[A :ClassTag](pf :PartialFunction[E, A]) :Arr[A] =
			array.collect(pf.asInstanceOf[PartialFunction[Any, A]]).asInstanceOf[Arr[A]]

		@inline def partitionMap[E1: ClassTag, E2: ClassTag](f: E => Either[E1, E2]) :(Arr[E1], Arr[E2]) =
			array.partitionMap(f).castFrom[(Array[E1], Array[E2]), (Arr[E1], Arr[E2])]

		@inline def map[A :ClassTag](f :E => A) :Arr[A] = array.map(f).asInstanceOf[Arr[A]]
		@inline def flatMap[A :ClassTag](f :E => IterableOnce[A]) :Arr[A] =
			array.flatMap(f).asInstanceOf[Arr[A]]

		@inline def flatMap[As, A](f :E => As)(implicit asIterable :As => Iterable[A], m :ClassTag[A]) :Arr[A] =
			array.flatMap(f).asInstanceOf[Arr[A]]

		@inline def flatten[A :ClassTag](implicit asIterable :E => IterableOnce[A]) :Arr[A] =
			array.flatten.asInstanceOf[Arr[A]]
*/

		@inline def groupBy[K](f: E => K) :Map[K, Arr[E]] =
			array.groupBy(f.asInstanceOf[Any => K]).castFrom[Map[K, Array[_]], Map[K, Arr[E]]]

//		@inline def groupMap[K, A :ClassTag](key :E => K)(f :E => A) :Map[K, Arr[A]] =
//			array.groupMap(key)(f).castFrom[Map[K, Array[A]], Map[K, Arr[A]]]

		@inline def tapEach[U](f :E => U) :Arr[E] = array.tapEach(f.asInstanceOf[Any => U]).asInstanceOf[Arr[E]]
		@inline def foreach[U](f :E => U) :Unit = array.foreach(f.asInstanceOf[Any => U])

		@inline def zipWithIndex :Arr[(E, Int)] = array.zipWithIndex.asInstanceOf[Arr[(E, Int)]]
		@inline def zip[A](that :IterableOnce[A]) :Arr[(E, A)] = array.zip(that).asInstanceOf[Arr[(E, A)]]

		@inline def lazyZip[A](that: Iterable[A]) :LazyZip2[E, A, Arr[E]] =
			array.lazyZip(that).asInstanceOf[LazyZip2[E, A, Arr[E]]]

		@inline def zipAll[A >: E, B](that :Iterable[B], thisElem :A, thatElem :B) :Arr[(A, B)] =
			array.zipAll(that, thisElem.asInstanceOf[Any], thatElem).castFrom[Array[(Any, B)], Arr[(A, B)]]

/*
		@inline def unzip[E1, E2](implicit asPair :E => (E1, E2), ct1 :ClassTag[E1], ct2 :ClassTag[E2])
				:(Arr[E1], Arr[E2]) =
			array.unzip.castFrom[(Array[E1], Array[E2]), (Arr[E1], Arr[E2])]

		@inline def unzip3[E1, E2, E3](implicit asTriple :E => (E1, E2, E3), ct1 :ClassTag[E1], ct2 :ClassTag[E2],
		                               ct3 :ClassTag[E3]) :(Arr[E1], Arr[E2], Arr[E3]) =
			array.unzip3.castFrom[(Array[E1], Array[E2], Array[E3]), (Arr[E1], Arr[E2], Arr[E3])]

		@inline def updated[A >: E :ClassTag](index :Int, x :A) :Arr[A] =
			array.updated(index, x).asInstanceOf[Arr[A]]

		@inline def :+[A >: E :ClassTag](x :A) :Arr[A] = array.appended(x).asInstanceOf[Arr[E]]
		@inline def +:[A >: E :ClassTag](x :A) :Arr[A] = array.prepended(x).asInstanceOf[Arr[E]]
		@inline def appended[A >: E :ClassTag](x :A) :Arr[A] = array.appended(x).asInstanceOf[Arr[E]]
		@inline def prepended[A >: E :ClassTag](x :A) :Arr[A] = array.prepended(x).asInstanceOf[Arr[E]]

		@inline def ++[A >: E :ClassTag](suffix :IterableOnce[A]) :Arr[A] =
			array.appendedAll(suffix).asInstanceOf[Arr[A]]

		@inline def ++[A >: E :ClassTag](suffix :Arr[A]) :Arr[A] =
			array.appendedAll(suffix.asInstanceOf[Array[A]]).asInstanceOf[Arr[A]]

		@inline def concat[A >: E :ClassTag](suffix :IterableOnce[A]) :Arr[A] =
			array.appendedAll(suffix).asInstanceOf[Arr[A]]

		@inline def concat[A >: E :ClassTag](suffix :Arr[A]) :Arr[A] =
			array.appendedAll(suffix.asInstanceOf[Array[A]]).asInstanceOf[Arr[A]]

		@inline def :++[A >: E :ClassTag](suffix :IterableOnce[A]) :Arr[A] =
			array.appendedAll(suffix).asInstanceOf[Arr[A]]

		@inline def :++[A >: E :ClassTag](suffix :Arr[A]) :Arr[A] =
			array.appendedAll(suffix.asInstanceOf[Array[A]]).asInstanceOf[Arr[A]]

		@inline def :++[A >: E](suffix :Arr[A])(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.appendedAll(suffix.asInstanceOf[Array[A]])(
				tag getOrElse ClassTag.Any.castParam[A]
			).asInstanceOf[Arr[A]]

		@inline def appendedAll[A >: E :ClassTag](suffix :IterableOnce[A]) :Arr[A] =
			array.appendedAll(suffix).asInstanceOf[Arr[A]]

		@inline def appendedAll[A >: E :ClassTag](suffix :Arr[A]) :Arr[A] =
			array.appendedAll(suffix.asInstanceOf[Array[A]]).asInstanceOf[Arr[A]]

		@inline def appendedAll[A >: E](suffix :Arr[A])(implicit tag :Maybe[ClassTag[A]]) :Arr[A] =
			array.appendedAll(suffix.asInstanceOf[Array[A]])(
				tag getOrElse ClassTag.Any.castParam[A]
			).asInstanceOf[Arr[A]]

		@inline def ++:[A >: E :ClassTag](suffix :IterableOnce[A]) :Arr[A] =
			array.prependedAll(suffix).asInstanceOf[Arr[A]]

		@inline def ++:[A >: E :ClassTag](suffix :Arr[A]) :Arr[A] =
			array.prependedAll(suffix.asInstanceOf[Array[A]]).asInstanceOf[Arr[A]]

		@inline def prependedAll[A >: E :ClassTag](suffix :IterableOnce[A]) :Arr[A] =
			array.prependedAll(suffix).asInstanceOf[Arr[A]]

		@inline def prependedAll[A >: E :ClassTag](suffix :Arr[A]) :Arr[A] =
			array.prependedAll(suffix.asInstanceOf[Array[A]]).asInstanceOf[Arr[A]]
*/

		@inline def diff[A >: E](that :collection.Seq[A]) :Arr[E] = array.diff(that.asInstanceOf[collection.Seq[Any]]).asInstanceOf[Arr[E]]
//		@inline def diff[A >: E](that :Arr[A]) :Arr[E] =
//			array.diff(mutable.ArraySeq.make(that.asInstanceOf[Array[A]])).asInstanceOf[Arr[E]]

		@inline def intersect[A >: E](that :collection.Seq[A]) :Arr[E] =
			array.intersect(that.asInstanceOf[collection.Seq[Any]]).asInstanceOf[Arr[E]]
//
//		@inline def intersect[A >: E](that :Arr[A]) :Arr[E] =
//			array.intersect(mutable.ArraySeq.make(that.asInstanceOf[Array[A]])).asInstanceOf[Arr[E]]

		@inline def transpose[A](implicit asArray :E => Arr[A]): Arr[Arr[A]] =
			array.transpose(asArray.asInstanceOf[Any => Array[A]]).castFrom[Array[Array[A]], Arr[Arr[A]]]

		@inline def view :IndexedSeqView[E] = array.view.asInstanceOf[IndexedSeqView[E]]
		@inline def indices :Range = Range(0, array.length)

		@inline def to[C1](factory :Factory[E, C1]) :C1 = array.to(factory.asInstanceOf[Factory[Any, C1]])
		@inline def toSeq :Seq[E] = ArraySeq.unsafeWrapArray(array).asInstanceOf[ArraySeq[E]]
		@inline def toIndexedSeq :IndexedSeq[E] = ArraySeq.unsafeWrapArray(array).asInstanceOf[ArraySeq[E]]
		@inline def toList :List[E] = array.toList.asInstanceOf[List[E]]
		@inline def toVector :Vector[E] = array.toVector.asInstanceOf[Vector[E]]
		@inline def toSet[A >: E] :Set[A] = array.toSet[Any].asInstanceOf[Set[A]]
		@inline def toMap[K, V](implicit ev :E <:< (K, V)) :Map[K, V] = array.toMap(ev.asInstanceOf[Any <:< (K, V)])
		@inline def toArray[A >: E :ClassTag] :Array[A] = array.toArray(classTag[A].asInstanceOf[ClassTag[Any]]).asInstanceOf[Array[A]]
		@inline def toBuffer[A >: E]: Buffer[A] = Buffer.from(array).asInstanceOf[Buffer[A]]

		@inline def copyToArray[A >: E](xs :Array[A]) :Int = array.copyToArray(xs.asInstanceOf[Array[Any]], 0, xs.length)
		@inline def copyToArray[A >: E](xs :Array[A], start :Int) :Int = array.copyToArray(xs.asInstanceOf[Array[Any]], start, xs.length)
		@inline def copyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int = array.copyToArray(xs.asInstanceOf[Array[Any]], start, len)

		@inline def mkString :String = array.mkString("", "", "")
		@inline def mkString(separator :String) :String = array.mkString("", separator, "")
		@inline def mkString(prefix :String, separator :String, suffix :String) :String =
			array.mkString(prefix, separator, suffix)

		@inline def addString(b :StringBuilder) :b.type = array.addString(b, "", "", "")
		@inline def addString(b :StringBuilder, sep :String) :b.type = array.addString(b, "", sep, "")
		@inline def addString(b :StringBuilder, start :String, sep :String, end :String) :b.type =
			array.addString(b, start, sep, end)

//		@inline def sameElements[A](other :A)(implicit isArray :A <:< Array[_]) :Boolean = array sameElements isArray(other)
		@inline def sameElements(other :ArrayLike[_]) :Boolean = array sameElements other.asInstanceOf[Array[_]]
		@inline def sameElements(other :IterableOnce[_]) :Boolean = array sameElements other
	}


	class RefArrayLikeExtension[E, Arr[_]] private[collections] (private val array :Array[Any]) extends AnyVal {
		@inline def head :E = array.head.asInstanceOf[E]
		@inline def last :E = array.last.asInstanceOf[E]
//		@inline def headOption :Option[E] = array.headOption
//		@inline def lastOption :Option[E] = array.lastOption

		@inline def scanLeft[A](z :A)(op :(A, E) => A) :Arr[A] =
			array.scanLeft(z :Any)(op.asInstanceOf[(Any, Any) => Any]).asInstanceOf[Arr[A]]

		@inline def scanRight[A](z :A)(op :(E, A) => A) :Arr[A] =
			array.scanRight(z :Any)(op.asInstanceOf[(Any, Any) => Any]).asInstanceOf[Arr[A]]

		@inline def scan[A >: E](z :A)(op :(A, A) => A) :Arr[A] =
			array.scan[Any](z)(op.asInstanceOf[(Any, Any) => Any]).asInstanceOf[Arr[A]]

		@inline def collect[A](pf :PartialFunction[E, A]) :Arr[A] =
			array.collect(pf.asInstanceOf[PartialFunction[Any, Any]]).asInstanceOf[Arr[A]]

		@inline def partitionMap[E1, E2](f: E => Either[E1, E2]) :(Arr[E1], Arr[E2]) =
			array.partitionMap(f.asInstanceOf[Any => Either[Any, Any]]).castFrom[(Array[_], Array[_]), (Arr[E1], Arr[E2])]

		@inline def map[A](f :E => A) :Arr[A] = array.map(f.asInstanceOf[Any => Any]).asInstanceOf[Arr[A]]
		@inline def flatMap[A](f :E => IterableOnce[A]) :Arr[A] =
			array.flatMap(f.asInstanceOf[Any => IterableOnce[Any]]).asInstanceOf[Arr[A]]

		@inline def flatMap[As, A](f :E => As)(implicit asIterable :As => Iterable[A]) :Arr[A] =
			array.flatMap(f.asInstanceOf[Any => Iterable[Any]]).asInstanceOf[Arr[A]]

		@inline def flatten[A](implicit asIterable :E => IterableOnce[A]) :Arr[A] =
			array.flatten(asIterable.asInstanceOf[Any => IterableOnce[Any]], ClassTag.Any).asInstanceOf[Arr[A]]


		@inline def unzip[E1, E2](implicit asPair :E => (E1, E2))
				:(Arr[E1], Arr[E2]) =
			array.unzip(asPair.asInstanceOf[Any => (Any, Any)], ClassTag.Any, ClassTag.Any).asInstanceOf[(Arr[E1], Arr[E2])]

		@inline def unzip3[E1, E2, E3](implicit asTriple :E => (E1, E2, E3)) :(Arr[E1], Arr[E2], Arr[E3]) =
			array.unzip3(asTriple.asInstanceOf[Any => (Any, Any, Any)], ClassTag.Any, ClassTag.Any, ClassTag.Any).castFrom[
				(Array[_], Array[_], Array[_]), (Arr[E1], Arr[E2], Arr[E3])
			]

		@inline def updated[A >: E](index :Int, x :A) :Arr[A] = array.updated[Any](index, x).asInstanceOf[Arr[A]]

		@inline def :+[A >: E](x :A) :Arr[A] = array.appended(x).asInstanceOf[Arr[A]]
		@inline def +:[A >: E](x :A) :Arr[A] = array.prepended(x).asInstanceOf[Arr[A]]
		@inline def appended[A >: E](x :A) :Arr[A] = array.appended(x).asInstanceOf[Arr[A]]
		@inline def prepended[A >: E](x :A) :Arr[A] = array.prepended(x).asInstanceOf[Arr[A]]

		@inline def ++[A >: E](suffix :IterableOnce[A]) :Arr[A] = array.appendedAll(suffix).asInstanceOf[Arr[A]]

		@inline def ++[A >: E](suffix :Arr[A]) :Arr[A] =
			array.appendedAll(suffix.asInstanceOf[Array[Any]]).asInstanceOf[Arr[A]]

		@inline def concat[A >: E](suffix :IterableOnce[A]) :Arr[A] =
			array.appendedAll(suffix).asInstanceOf[Arr[A]]

		@inline def concat[A >: E](suffix :Arr[A]) :Arr[A] =
			array.appendedAll(suffix.asInstanceOf[Array[Any]]).asInstanceOf[Arr[A]]

		@inline def :++[A >: E](suffix :IterableOnce[A]) :Arr[A] =
			array.appendedAll(suffix).asInstanceOf[Arr[A]]

		@inline def :++[A >: E](suffix :Arr[A]) :Arr[A] =
			array.appendedAll(suffix.asInstanceOf[Array[A]]).asInstanceOf[Arr[A]]

		@inline def appendedAll[A >: E](suffix :IterableOnce[A]) :Arr[A] =
			array.appendedAll(suffix).asInstanceOf[Arr[A]]

		@inline def appendedAll[A >: E](suffix :Arr[A]) :Arr[A] =
			array.appendedAll(suffix.asInstanceOf[Array[Any]]).asInstanceOf[Arr[A]]

		@inline def ++:[A >: E](suffix :IterableOnce[A]) :Arr[A] =
			array.prependedAll(suffix).asInstanceOf[Arr[A]]

		@inline def ++:[A >: E](suffix :Arr[A]) :Arr[A] =
			array.prependedAll(suffix.asInstanceOf[Array[Any]]).asInstanceOf[Arr[A]]

		@inline def prependedAll[A >: E](suffix :IterableOnce[A]) :Arr[A] =
			array.prependedAll(suffix).asInstanceOf[Arr[A]]

		@inline def prependedAll[A >: E](suffix :Arr[A]) :Arr[A] =
			array.prependedAll(suffix.asInstanceOf[Array[Any]]).asInstanceOf[Arr[A]]

	}


	/** A lazy filtered array. No filtering is applied until one of `foreach`, `map` or `flatMap` is called. */
	class WithFilter[Arr[_], E] private[collections] (p :E => Boolean, xs :Array[_], factory :IterableFactory[Arr]) {
		/** Apply `f` to each element for its side effects.
		  * Note: [U] parameter needed to help scalac's type inference.
		  */
		def foreach[U](f :E => U) :Unit = {
			val pred = p.asInstanceOf[Any => Boolean]
			val fun  = f.asInstanceOf[Any => U]
			val len = xs.array.length
			var i = 0
			while (i < len) {
				val x = xs(i)
				if (pred(x)) fun(x)
				i += 1
			}
		}

		/** Builds a new array by applying a function to all elements of this array.
		  * @param f      the function to apply to each element.
		  * @tparam A     the element type of the returned array.
		  * @return a new array resulting from applying the given function
		  *         `f` to each element of this array and collecting the results.
		  */
		def map[A](f :E => A) :Arr[A] = {
			val pred = p.asInstanceOf[Any => Boolean]
			val fun  = f.asInstanceOf[Any => A]
			val b = factory.newBuilder[A]
			var i = 0
			while (i < xs.length) {
				val x = xs(i)
				if (pred(x)) b += fun(x)
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
		def flatMap[A](f :E => IterableOnce[A]) :Arr[A] = {
			val pred = p.asInstanceOf[Any => Boolean]
			val fun  = f.asInstanceOf[Any => IterableOnce[A]]
			val b = factory.newBuilder[A]
			var i = 0
			while (i < xs.length) {
				val x = xs(i)
				if (pred(x)) b ++= fun(xs(i))
				i += 1
			}
			b.result()
		}

		def flatMap[As, A](f :E => As)(implicit asIterable :As => Iterable[A]) :Arr[A] =
			flatMap(x => asIterable(f(x)))

		/** Creates a new non-strict filter which combines this filter with the given predicate. */
		def withFilter(q :E => Boolean) :WithFilter[Arr, E] =
			new WithFilter[Arr, E](a => p(a) && q(a), xs, factory)
	}

}






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


	class IArrayExtension[E] private[collections] (private[IArray] val array :Array[E])
		extends AnyVal
	{
/*
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
		@inline def indexWhere(p :E => Boolean, from :Int = 0) :Int = array.indexWhere(p, from)
		@inline def lastIndexWhere(p :E => Boolean) :Int = array.lastIndexWhere(p)
		@inline def lastIndexWhere(p :E => Boolean, end :Int = Int.MaxValue) :Int = array.lastIndexWhere(p, end)
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
*/

/*
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
*/
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

/*
		@inline def tapEach[U](f :E => U) :IArray[E] = array.tapEach(f).asInstanceOf[IArray[E]]
		@inline def foreach[U](f :E => U) :Unit = array.foreach(f)

		@inline def zipWithIndex :IArray[(E, Int)] = array.zipWithIndex.asInstanceOf[IArray[(E, Int)]]
		@inline def zip[A](that :IterableOnce[A]) :IArray[(E, A)] = array.zip(that).asInstanceOf[IArray[(E, A)]]

		@inline def lazyZip[A](that: Iterable[A]) :LazyZip2[E, A, IArray[E]] =
			array.lazyZip(that).asInstanceOf[LazyZip2[E, A, IArray[E]]]

		@inline def zipAll[A >: E, B](that :Iterable[B], thisElem :A, thatElem :B) :IArray[(A, B)] =
			array.zipAll(that, thisElem, thatElem).castFrom[Array[(A, B)], IArray[(A, B)]]
*/

		@inline def unzip[E1, E2](implicit asPair :E => (E1, E2), ct1 :ClassTag[E1], ct2 :ClassTag[E2])
				:(IArray[E1], IArray[E2]) =
			array.unzip.castFrom[(Array[E1], Array[E2]), (IArray[E1], IArray[E2])]

		@inline def unzip3[E1, E2, E3](implicit asTriple :E => (E1, E2, E3), ct1 :ClassTag[E1], ct2 :ClassTag[E2],
		                               ct3 :ClassTag[E3]) :(IArray[E1], IArray[E2], IArray[E3]) =
			array.unzip3.castFrom[(Array[E1], Array[E2], Array[E3]), (IArray[E1], IArray[E2], IArray[E3])]

		@inline def updated[A >: E :ClassTag](index :Int, x :A) :IArray[A] =
			array.updated(index, x).asInstanceOf[IArray[A]]

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

/*
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
*/

		def toOps :IndexedSeqOps[E, IArray, IArray[E]] = new IArrayAsSeq[E](array.asInstanceOf[IArray[E]])

//		@inline def to[C1](factory :Factory[E, C1]) :C1 = array.to(factory)
		@inline def toSeq :Seq[E] = WrappedArray(array)
		@inline def toIndexedSeq :IndexedSeq[E] = WrappedArray(array)


/*
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
*/
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


	implicit def iArrayIsSeq[E] :IsSeq[IArray[E]] { type A = E; type C = IArray[E] } =
		isSeqPrototype.asInstanceOf[IsSeq[IArray[E]] { type A = E; type C = IArray[E] }]

	private[this] val isSeqPrototype = new IsSeq[IArray[Any]] with Serializable {
		type C = IArray[Any]
		override type A = Any
		override def apply(array :IArray[Any]) = new IndexedSeqOps[Any, Seq, IArray[Any]] {
			override def length = array.length
			override def apply(i :Int) = array.asInstanceOf[Array[Any]].apply(i)
			override def fromSpecific(coll :IterableOnce[Any]) = IArray.from(coll)
			override def newSpecificBuilder = IArray.newBuilder(coll.getClass.getComponentType)
			override def iterator = array.iterator

			override def toIterable = WrappedIArray(array)
			override def coll = array
			override def iterableFactory = IndexedSeq
		}
		private def readResolve = IArray.iArrayIsSeq
	}



	override def from[E :ClassTag](it :IterableOnce[E]) :IArray[E] = it match {
		case elems :View[E]                      => from(elems.iterator)
		case elems :Iterable[E] if elems.isEmpty => Array.empty[E].asInstanceOf[IArray[E]]
		case iter  :Iterator[E] if !iter.hasNext => Array.empty[E].asInstanceOf[IArray[E]]

		case elems :ArraySeq[E] if classTag[E].runtimeClass isAssignableFrom elems.unsafeArray.getClass.getComponentType =>
			elems.unsafeArray.asInstanceOf[IArray[E]]
		case elems :AbstractPassedArray[E]
			if elems.length == elems.unsafeArray.length
				&& classTag[E].runtimeClass.isAssignableFrom(elems.getClass.getComponentType) =>
			elems.unsafeArray.asInstanceOf[IArray[E]]
		case elems :IArrayAsSeq[E]               => elems.coll
		case elems :Iterable[E]                  => elems.toArray[E].asInstanceOf[IArray[E]]
		case _                                   => it.iterator.toArray[E].asInstanceOf[IArray[E]]
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


	override def empty[E :ClassTag] :IArray[E] = ArrayAsSeq.empty[E].asInstanceOf[IArray[E]]

	def empty[E](elementType :Class[E]) :IArray[E] = ArrayAsSeq.empty(elementType).asInstanceOf[IArray[E]]

	override def newBuilder[E :ClassTag] :Builder[E, IArray[E]] =
		ArrayAsSeq.newBuilder[E].asInstanceOf[Builder[E, IArray[E]]]

	def newBuilder[E](elementType :Class[E]) :Builder[E, IArray[E]] =
		ArrayAsSeq.newBuilder(elementType).asInstanceOf[Builder[E, IArray[E]]]

	def one[E :ClassTag](elem :E) :IArray[E] = {
		val a = new Array[E](1)
		a(0) = elem
		a.asInstanceOf[IArray[E]]
	}

	def two[E :ClassTag](first :E, second :E) :IArray[E] = {
		val a = new Array[E](2)
		a(0) = first
		a(1) = second
		a.asInstanceOf[IArray[E]]
	}

	def copyOf[E](array :Array[E]) :IArray[E] = Array.copyOf(array, array.length).asInstanceOf[IArray[E]]
	def copyOf[E](array :Array[E], newLength :Int) :IArray[E] = Array.copyOf(array, newLength).asInstanceOf[IArray[E]]
	def copyOfRange[E](array :Array[E], from :Int, until :Int) :IArray[E] =
		array.slice(from, until).asInstanceOf[IArray[E]]

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

		def two[E](first :E, second :E) :IArray[E] = {
			val a = new Array[Any](1)
			a(0) = first
			a(1) = second
			a.asInstanceOf[IArray[E]]
		}

		override def newBuilder[E] :Builder[E, IArray[E]] = Array.newBuilder[Any].asInstanceOf[Builder[E, IArray[E]]]

		override def toString = "IArray.untagged"
	}


	private[collections] sealed trait IArrayLowPriorityExtensions extends Any {
		@inline implicit def GenericIArrayExtension[E](array :IArray[E]) :GenericIArrayExtension[E] =
			new GenericIArrayExtension[E](array.asInstanceOf[Array[E]])
	}

	private[collections] trait extensions extends Any with IArrayLowPriorityExtensions {

		@inline implicit def ByteIArrayExtension(array :IArray[Byte]) :ByteIArrayExtension =
			new ByteIArrayExtension(array.asInstanceOf[Array[Byte]])

		@inline implicit def ShortIArrayExtension(array :IArray[Short]) :ShortIArrayExtension =
			new ShortIArrayExtension(array.asInstanceOf[Array[Short]])

		@inline implicit def CharIArrayExtension(array :IArray[Char]) :CharIArrayExtension =
			new CharIArrayExtension(array.asInstanceOf[Array[Char]])

		@inline implicit def IntIArrayExtension(array :IArray[Int]) :IntIArrayExtension =
			new IntIArrayExtension(array.asInstanceOf[Array[Int]])

		@inline implicit def LongIArrayExtension(array :IArray[Long]) :LongIArrayExtension =
			new LongIArrayExtension(array.asInstanceOf[Array[Long]])

		@inline implicit def FloatIArrayExtension(array :IArray[Float]) :FloatIArrayExtension =
			new FloatIArrayExtension(array.asInstanceOf[Array[Float]])

		@inline implicit def DoubleIArrayExtension(array :IArray[Double]) :DoubleIArrayExtension =
			new DoubleIArrayExtension(array.asInstanceOf[Array[Double]])

		@inline implicit def BooleanIArrayExtension(array :IArray[Boolean]) :BooleanIArrayExtension =
			new BooleanIArrayExtension(array.asInstanceOf[Array[Boolean]])

		@inline implicit def RefIArrayExtension[E <: AnyRef](array :IArray[E]) :RefIArrayExtension[E] =
			new RefIArrayExtension(array.asInstanceOf[Array[E]])

		@inline implicit def IArrayExtension[E](array :IArray[E]) :IArrayExtension[E] =
			new IArrayExtension(array.asInstanceOf[Array[E]])

		@inline implicit def IArrayAsArrayLikeExtension[E](array :IArray[E]) :ArrayLikeExtension[E, IArray] =
			new ArrayLikeExtension(array.asInstanceOf[Array[Any]])
	}


	object extensions extends extensions
}







/**
  * @define Coll `RefArrayLike`
  * @tparam coll object array
  */
private[collections] sealed abstract class RefArrayLikeFactory[Arr[_]] extends IterableFactory[Arr] {

	/** Boxes the element if necessary, places it in a singleton `Array[AnyRef]`, and returns it as a $Coll`[E]`. */
	def one[E](elem :E) :Arr[E] = ErasedArray.one(elem).asInstanceOf[Arr[E]]

	/** Boxes the elements, if necessary, places them in an `Array[AnyRef]` of length 2, and returns as a $Coll`[E]`. */
	def two[E](first :E, second :E) :Arr[E] = ErasedArray.two(first, second).asInstanceOf[Arr[E]]

	/** A new `Array[AnyRef]` of the specified length, cast to $Coll`[E]`. */
	@inline def ofDim[E](length :Int) :Arr[E] = new Array[Any](length).asInstanceOf[Arr[E]]

	/** Allocates a new `Array[AnyRef]` and copies all elements from the argument, returning it as a $Coll`[E]`.
	  * If the argument is a value array, the elements will be boxed.
	  */
	def copyOf[E](array :ArrayLike[E]) :Arr[E] = {
		val length = array.asInstanceOf[Array[Any]].length
		val res    = new Array[Any](length)
		if (array.isInstanceOf[Array[AnyRef]])
			System.arraycopy(array, 0, res, 0, length).asInstanceOf[Arr[E]]
		else
			Array.copy(array, 0, res, 0, length).asInstanceOf[Arr[E]]
		res.asInstanceOf[Arr[E]]
	}

	/** Reallocates the given array as a $Coll`[E]` of a new size, and copies `min(newLength, array.length)`
	  * of its first elements. If the argument is a value array, the elements will be boxed.
	  */
	def copyOf[E](array :ArrayLike[E], newLength :Int) :Arr[E] = {
		val res = new Array[Any](newLength)
		if (array.isInstanceOf[Array[AnyRef]])
			System.arraycopy(array, 0, res, 0, newLength)
		else
			Array.copy(array, 0, res, 0, newLength)
		res.asInstanceOf[Arr[E]]
	}

	/** Copies the elements of `array` in the index range `[from, until)` to a new array with an erased element type.
	  * @param array The sliced array.
	  * @param from  The index of the element in `array` to be copied as the first element of the new array.
	  *              Must be in range `[0, array.length]`, or an `IndexOutOfBoundsException` will be thrown.
	  * @param until The index after the last copied element in `array`. If less than `from`, an empty array is returned.
	  *              If `until > array.length`, then the new array will contain `until - array.length` `null` elements
	  *              in its suffix.
	  * @return An `Array[AnyRef]` of length `until - from` as a $Coll`[E]`, with the copied slice.
	  */
	def copyOfRange[E](array :ArrayLike[E], from :Int, until :Int) :Arr[E] =
		if (from < 0 | from > array.asInstanceOf[Array[Any]].length)
			throw new IndexOutOfBoundsException(
				toString + ".copyOfRange(" + array.localClassName + "[" + array.asInstanceOf[Array[Any]].length +
					"], " + from + ", " + until + ")"
			)
		else if (until <= from)
			empty
		else {
			val res    = new Array[Any](until - from)
			val copied = Math.min(until - from, array.asInstanceOf[Array[Any]].length - from)
			if (array.isInstanceOf[Array[AnyRef]])
				System.arraycopy(array, from, res, 0, copied)
			else
				Array.copy(array, from, res, 0, copied)
			res.asInstanceOf[Arr[E]]
		}


	override def from[A](source :IterableOnce[A]) :Arr[A] = source match {
		case empty if empty.knownSize == 0 => Empty.asInstanceOf[Arr[A]]
//		case ops :collection.IndexedSeqOps[A, _, _] =>
//			var from = 0
//			val array = ops match {
//				case seq :ArraySeq[A]            => seq.unsafeArray
//				case seq :mutable.ArraySeq[A]    => seq.array
//				case seq :AbstractPassedArray[A] => from = seq.startIndex; seq.unsafeArray
//				case seq :ArraySlice[A]          => from = seq.startIndex; seq.unsafeArray
//				case seq :ArrayAsSeq[A]          => seq.coll
//				case seq :IArrayAsSeq[A]         => seq.coll
//				case _ => null
//			}
//			if (array == null)
//				ops.toArray[Any].asInstanceOf[RefArray[A]]
//			else
//				ErasedArray.copyOfRange(array.asInstanceOf[Array[A]], from, from + ops.length)
		case items :Iterable[A] =>
			items.toArray[Any].asInstanceOf[Arr[A]]
		case _                  =>
			source.iterator.toArray[Any].asInstanceOf[Arr[A]]
	}

	override def empty[E] :Arr[E] = Empty.asInstanceOf[Arr[E]]

	override def newBuilder[E] :Builder[E, Arr[E]] = Array.newBuilder(ClassTag.Any).asInstanceOf[Builder[E, Arr[E]]]

	private[this] val Empty = Array.emptyObjectArray

	def unapplySeq[E](array :Arr[E]) :UnapplySeqWrapper[E] =
		new UnapplySeqWrapper(array.asInstanceOf[Array[E]])


	protected def isSeq[E] :IsSeq[Arr[E]] { type A = E; type C = Arr[E] } =
		refArrayIsSeqPrototype.asInstanceOf[IsSeq[Arr[E]] {type A = E; type C = Arr[E] }]

	private[this] val refArrayIsSeqPrototype = new IsSeq[Arr[Any]] with Serializable {
		override type A = Any
		override type C = Arr[Any]

		override def apply(array :Arr[Any]) =
			new IndexedSeqOps[Any, IndexedSeq, Arr[Any]] {
				override def apply(i :Int) :Any = array.asInstanceOf[Array[Any]].apply(i)
				override def length :Int = array.asInstanceOf[Array[Any]].length

				override def fromSpecific(coll :IterableOnce[Any]) = RefArrayLikeFactory.this.from(coll)
				override def newSpecificBuilder = newBuilder

				@nowarn("cat=deprecation")
				override def toIterable = new IndexedSeqArrayAdapter(array.asInstanceOf[Array[Any]])
				override def coll = array
				override def iterableFactory = IndexedSeq

				override def iterator = array.asInstanceOf[Array[Any]].iterator
			}
		private def readResolve = RefArrayLikeFactory.this.isSeq
	}

}




/** A factory of `RefArray` - values with `Array` API available as extension methods, represented in runtime
  * always as `Array[AnyRef]`, regardless of their actual type parameter.
  * @define Coll `RefArray`
  * @define coll reference object array
  */
case object RefArray extends RefArrayLikeFactory[RefArray] with IterableFactory[RefArray] {
	class RefArrayExtension[A] private[collections] (private val array :Array[Any]) extends AnyVal {
		def update(idx :Int, elem :A) :Unit = array(idx) = elem

		@inline def toSeq :Seq[A] = WrappedArray(ErasedArray.copyOf(array)).asInstanceOf[Seq[A]]
		@inline def toIndexedSeq :IndexedSeq[A] = WrappedArray(ErasedArray.copyOf(array)).asInstanceOf[IndexedSeq[A]]
		@inline def toOps :collection.IndexedSeqOps[A, RefArray, RefArray[A]] =
			new RefArrayAsSeq(array.asInstanceOf[RefArray[A]])
	}

	implicit def refArrayToSeq[A](array :RefArray[A]) :mutable.IndexedSeq[A] =
		mutable.ArraySeq.make(array.asInstanceOf[Array[A]])

	implicit def refArrayIsSeq[E] :IsSeq[RefArray[E]] { type A = E; type C = RefArray[A] } =
		isSeq

	private[collections] trait extensions extends Any {
		@inline implicit def RefArrayExtension[A](array :RefArray[A]) :RefArrayExtension[A] =
			new RefArrayExtension(array.asInstanceOf[Array[Any]])

		@inline implicit def RefArrayAsArrayLikeExtension[A](array :RefArray[A]) :ArrayLikeExtension[A, RefArray] =
			new ArrayLikeExtension[A, RefArray](array.asInstanceOf[Array[Any]])

		@inline implicit def RefArrayAsRefArrayLikeExtension[A](array :RefArray[A]) :RefArrayLikeExtension[A, RefArray] =
			new RefArrayLikeExtension(array.asInstanceOf[Array[Any]])
	}

	object extensions extends extensions

}




/** A factory of `IRefArray` - immutable values with `Array` API available as extension methods, represented in runtime
  * always as `Array[AnyRef]`, regardless of their actual type parameter.
  * @define Coll `IRefArray`
  * @define coll immutable reference object array
  */
case object IRefArray extends RefArrayLikeFactory[IRefArray] with IterableFactory[IRefArray] {
	class IRefArrayExtension[A](private val array :Array[Any]) extends AnyVal {
		@inline def toSeq :Seq[A] = WrappedArray(array).asInstanceOf[Seq[A]]
		@inline def toIndexedSeq :IndexedSeq[A] = WrappedArray(array).asInstanceOf[IndexedSeq[A]]
		@inline def toOps :IndexedSeqOps[A, IRefArray, IRefArray[A]] =
			new IRefArrayAsSeq(array.asInstanceOf[IRefArray[A]])
	}

	implicit def iRefArrayToSeq[A](array :IRefArray[A]) :IndexedSeq[A] =
		DefaultArraySeq(array).asInstanceOf[IndexedSeq[A]]

	implicit def iRefArrayIsSeq[E] :IsSeq[IRefArray[E]] { type A = E; type C = IRefArray[A] } =
		isSeq

	override def from[A](source :IterableOnce[A]) :IRefArray[A] = source match {
		case _ if source.knownSize == 0 => empty
		case ops :collection.IndexedSeqOps[A, _, _] =>
			var from = 0
			val array = ops match {
				case seq :ArraySeq[A]            => seq.unsafeArray
				case seq :AbstractPassedArray[A] => from = seq.startIndex; seq.unsafeArray
//				case seq :ArraySlice[A]          => from = seq.startIndex; seq.unsafeArray
				case seq :IArrayAsSeq[A]         => seq.coll
				case _ => null
			}
			if (array == null || !array.isInstanceOf[Array[AnyRef]])
				ops.toArray[Any].asInstanceOf[IRefArray[A]]
			else
				copyOfRange(array.asInstanceOf[IRefArray[A]], from, from + ops.length)
		case items :Iterable[A] =>
			items.toArray[Any].asInstanceOf[IRefArray[A]]
		case _                  =>
			source.iterator.toArray[Any].asInstanceOf[IRefArray[A]]
	}


	private[collections] trait extensions extends Any {
		@inline implicit def IRefArrayExtension[A](array :RefArray[A]) :IRefArrayExtension[A] =
			new IRefArrayExtension(array.asInstanceOf[Array[Any]])

		@inline implicit def IRefArrayAsArrayLikeExtension[A](array :IRefArray[A]) :ArrayLikeExtension[A, IRefArray] =
			new ArrayLikeExtension(array.asInstanceOf[Array[Any]])

		@inline implicit def IRefArrayAsRefArrayLikeExtension[A](array :IRefArray[A]) :RefArrayLikeExtension[A, IRefArray] =
			new RefArrayLikeExtension(array.asInstanceOf[Array[Any]])
	}


	object extensions extends extensions

}




/** Factory of erased arrays: `Array[E]` represented in runtime by an `Array[Any]`.
  * ''This will work only in generic contexts: '' `ErasedArray.ofDim[Int](1)` will throw a [[ClassCastException]].
  * @define Coll `Array`
  * @define coll array
  */
@SerialVersionUID(Ver)
private[noresttherein] case object ErasedArray extends IterableFactory[Array] {

	/** Boxes the element if necessary, places it in a singleton `Array[AnyRef]`, and returns it as a $Coll`[E]`. */
	def one[E](elem :E) :Array[E] = {
		val a = new Array[Any](1)
		a(0) = elem
		a.asInstanceOf[Array[E]]
	}

	/** Boxes the elements, if necessary, places them in an `Array[AnyRef]` of length 2, and returns as a $Coll`[E]`. */
	def two[E](first :E, second :E) :Array[E] = {
		val a = new Array[Any](2)
		a(0) = first
		a(1) = second
		a.asInstanceOf[Array[E]]
	}

	/** A new `Array[AnyRef]` of the specified length, cast to $Coll`[E]`. */
	@inline def ofDim[E](length :Int) :Array[E] = new Array[Any](length).asInstanceOf[Array[E]]

	/** Allocates a new `Array[AnyRef]` and copies all elements from the argument, returning it as a $Coll`[E]`.
	  * If the argument is a value array, the elements will be boxed.
	  */
	def copyOf[E](array :Array[E]) :Array[E] = {
		val length = array.asInstanceOf[Array[Any]].length
		val res    = new Array[Any](length)
		if (array.isInstanceOf[Array[AnyRef]])
			System.arraycopy(array, 0, res, 0, length).asInstanceOf[Array[E]]
		else
			Array.copy(array, 0, res, 0, length).asInstanceOf[Array[E]]
		res.asInstanceOf[Array[E]]
	}

	/** Reallocates the given array as a $Coll`[E]` of a new size, and copies `min(newLength, array.length)`
	  * of its first elements. If the argument is a value array, the elements will be boxed.
	  */
	def copyOf[E](array :Array[E], newLength :Int) :Array[E] = {
		val res = new Array[Any](newLength)
		if (array.isInstanceOf[Array[AnyRef]])
			System.arraycopy(array, 0, res, 0, newLength)
		else
			Array.copy(array, 0, res, 0, newLength)
		res.asInstanceOf[Array[E]]
	}

	/** Copies the elements of `array` in the index range `[from, until)` to a new array with an erased element type.
	  * @param array The sliced array.
	  * @param from  The index of the element in `array` to be copied as the first element of the new array.
	  *              Must be in range `[0, array.length]`, or an `IndexOutOfBoundsException` will be thrown.
	  * @param until The index after the last copied element in `array`. If less than `from`, an empty array is returned.
	  *              If `until > array.length`, then the new array will contain `until - array.length` `null` elements
	  *              in its suffix.
	  * @return An `Array[AnyRef]` of length `until - from` as a $Coll`[E]`, with the copied slice.
	  */
	def copyOfRange[E](array :Array[E], from :Int, until :Int) :Array[E] =
		if (from < 0 | from > array.asInstanceOf[Array[Any]].length)
			throw new IndexOutOfBoundsException(
				toString + ".copyOfRange(" + array.localClassName + "[" + array.asInstanceOf[Array[Any]].length +
					"], " + from + ", " + until + ")"
			)
		else if (until <= from)
			empty
		else {
			val res    = new Array[Any](until - from)
			val copied = Math.min(until - from, array.asInstanceOf[Array[Any]].length - from)
			if (array.isInstanceOf[Array[AnyRef]])
				System.arraycopy(array, from, res, 0, copied)
			else
				Array.copy(array, from, res, 0, copied)
			res.asInstanceOf[Array[E]]
		}


	/** Build an array with erased element type from the iterable collection.
	  *  @param  source the iterable collection
	  *  @return an array consisting of elements of the iterable collection
	  */
	override def from[E](source :IterableOnce[E]) :Array[E] = source match {
		case it :View[E] => from(it.iterator)
		case it :Iterable[E] if it.isEmpty => empty
		case it :Iterator[E] if it.isEmpty => empty
		case it :ArrayAsSeq[E] if it.coll.isInstanceOf[Array[AnyRef]] => it.coll
		case it :IArrayAsSeq[E] if it.coll.isInstanceOf[Array[AnyRef]] => it.coll.asInstanceOf[Array[E]]
//		case it :ArrayAsSeq[E] if it.coll.getClass == classOf[Array[AnyRef]] => it.coll
//		case it :IArrayAsSeq[E] if it.coll.getClass == classOf[Array[AnyRef]] => it.coll.asInstanceOf[Array[E]]
		case it :Iterable[E] => it.toArray[Any].asInstanceOf[Array[E]]
		case _ => source.iterator.toArray[Any].asInstanceOf[Array[E]]
	}

	override def empty[A] :Array[A] = Empty.asInstanceOf[Array[A]]

	private[this] val Empty = Array.emptyObjectArray

	override def newBuilder[E] :Builder[E, Array[E]] = Array.newBuilder(ClassTag.Any).asInstanceOf[Builder[E, Array[E]]]


	def unapply[E](elems :IterableOnce[E]) :Opt[Array[E]] = elems match {
		case seq :mutable.ArraySeq[E] if seq.array.getClass == classOf[Array[AnyRef]] =>
			Got(seq.array.asInstanceOf[Array[E]])
		case seq :ArrayAsSeq[E] if seq.coll.getClass == classOf[Array[AnyRef]] =>
			Got(seq.coll)
		case _ =>
			Lack
	}

	def unapplySeq[E](array :Array[E]) :UnapplySeqWrapper[E] =
		new UnapplySeqWrapper(array)

	/** ClassTag`[E]` with its `runtimeClass` equal to `Any`. */
	def erasedTag[E] :ClassTag[E] = ClassTag.Any.castParam[E]
}
