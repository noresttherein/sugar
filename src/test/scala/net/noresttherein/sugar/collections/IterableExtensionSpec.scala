package net.noresttherein.sugar.collections

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

import net.noresttherein.sugar.extensions.iterableExtension
import org.scalacheck.{Arbitrary, Prop, Properties, Shrink, Test}
import org.scalacheck.Prop._
import org.scalacheck.util.ConsoleReporter





object IterableExtensionSpec extends Properties("IterableExtension") {

	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140)).withMinSuccessfulTests(1000)

	private val Sum = 100

	def mappingProperty(prop :Iterable[Int] => Prop) :Prop =
		forAll { list :List[Int] => prop(list) } &&
			forAll { seq :IndexedSeq[Int] => prop(seq) } &&
				forAll { col :OrderedItems[Int] => prop(col) }

	def flatMappingProperty(prop :Iterable[Iterable[Int]] => Prop) :Prop =
		forAll { list :List[List[Int]] => prop(list) } &&
			forAll { seq :IndexedSeq[IndexedSeq[Int]] => prop(seq) } &&
			forAll { col :OrderedItems[OrderedItems[Int]] => prop(col) }

	property("mapWith") = mappingProperty { list :Iterable[Int] =>
		list.mapWith(0) { (e, sum) => (e + sum, e + sum) }.toSeq ?= list.toSeq.scanLeft(0)(_ + _).tail
	}
	property("flatMapWith") = flatMappingProperty { list :Iterable[Iterable[Int]] =>
		val flatMapped = list.flatMapWith(0) { (es, sum) =>
			val res = es.toSeq.scanLeft(sum)(_ + _)
			(res.tail, res.last)
		}
		flatMapped.toSeq ?= list.flatten.scanLeft(0)(_ + _).tail.toSeq
	}

	property("mapWithIndex") = mappingProperty { list :Iterable[Int] =>
		list.mapWithIndex { (e, i) => (e, i) }.toSeq ?= list.toSeq.zipWithIndex
	}
	property("flatMapWithIndex") = flatMappingProperty { list :Iterable[Iterable[Int]] =>
		list.flatMapWithIndex { (e, i) => e.map { (_, i) } }.toSeq ?=
			list.toSeq.zipWithIndex.flatMap { case (e, i) => e.toSeq.map((_, i)) }
	}

	property("mapWhile") = mappingProperty { list :Iterable[Int] =>
		list.mapWhile(0)(_ <= Sum)((acc, e) => (e + acc, 2 * e + acc)) ?=
			list.scanLeft((0, 0)) { case (acc, e) => (e + acc._1, 2 * e + acc._1) }.tail.takeWhile(_._1 <= Sum).map(_._2)
	}
	property("flatMapWhile") = flatMappingProperty { list :Iterable[Iterable[Int]] =>
		list.flatMapWhile(0)(_ <= Sum)((acc, es) => (acc + es.sum, es.map(_ + acc))) ?= {
			val sums = list.map(_.sum).scanLeft(0)(_ + _)
			list.zip(sums).map { case (es, acc) => es.map(_ + acc) }.zip(sums.tail).takeWhile(_._2 <= Sum).flatMap(_._1)
		}
	}

	property("filterWith") = mappingProperty { list :Iterable[Int] =>
		list.filterWith(0) { (e, total) => (e > total, total + e) } ?=
			list.zip(list.scanLeft(0)(_ + _)).collect { case (e, total) if e > total => e }
	}
	property("filterWithIndex") = mappingProperty { list :Iterable[Int] =>
		list.filterWithIndex { (e, i) => e % 2 == i % 2 } ?=
			list.zipWithIndex.collect { case (e, i) if e % 2 == i % 2 => e }
	}


	def zipMapProperty(prop :(Iterable[Int], Iterable[Int]) => Prop) :Prop =
		forAll { (l :List[Int], r :List[Int]) => prop(l, r) } &&
			forAll { (l :IndexedSeq[Int], r :IndexedSeq[Int]) => prop(l, r) } &&
				forAll { (l :OrderedItems[Int], r :OrderedItems[Int]) => prop(l, r) }

	property("zipMap") = zipMapProperty { (l :Iterable[Int], r :Iterable[Int]) =>
		l.zipMap(r)((a, b) => (a + b) * 2) ?= l.zip(r).map { case (a, b) => (a + b) * 2 }
	}
	property("zipMapAll") = zipMapProperty { (l :Iterable[Int], r :Iterable[Int]) =>
		if (l.size != r.size)
			throws(classOf[NoSuchElementException])(l.zipMapAll(r)((a, b) => (a + b) * 2))
		else
			l.zipMap(r)((a, b) => (a + b) * 2) ?= l.zip(r).map { case (a, b) => (a + b) * 2 }
	}
	property("zipMapAll+") = zipMapProperty { (l :Iterable[Int], r :Iterable[Int]) =>
		l.zipMapAll(r, 42, 1000)((a, b) => (a + b) * 2) ?=
			l.zipAll(r, 42, 1000).map { case (a, b) => (a + b) * 2 }
	}

	private def zipFlatMapProperty(prop :(Iterable[Iterable[Int]], Iterable[Iterable[Int]]) => Prop) :Prop =
		forAll { (l :List[List[Int]], r :List[List[Int]]) => prop(l, r) } &&
			forAll { (l :Vector[ArraySeq[Int]], r :Vector[ArraySeq[Int]]) => prop(l, r) } &&
			forAll { (l :OrderedItems[OrderedItems[Int]], r :OrderedItems[OrderedItems[Int]]) => prop(l, r) }

	property("zipFlatMap") = zipFlatMapProperty { (l :Iterable[Iterable[Int]], r :Iterable[Iterable[Int]]) =>
		l.zipFlatMap(r)((a, b) => a.zip(b).map { case (x, y) => x + y }) ?=
			l.zip(r).flatMap { case (a, b) => a.zip(b).map { case (x, y) => x + y} }
	}
	property("zipFlatMapAll") = zipFlatMapProperty { (l :Iterable[Iterable[Int]], r :Iterable[Iterable[Int]]) =>
		def zipFlatMap = l.zipFlatMapAll(r) { (a, b) => a.zip(b).map { case (x, y) => x + y }}
		if (l.size != r.size)
			throws(classOf[NoSuchElementException])(zipFlatMap)
		else
			zipFlatMap ?= l.zipAll(r, Nil, Nil).flatMap { case (a, b) => a.zip(b).map { case (x, y) => x + y }}
	}
	property("zipFlatMapAll+") = zipFlatMapProperty { (l :Iterable[Iterable[Int]], r :Iterable[Iterable[Int]]) =>
		l.zipFlatMapAll(r, Nil, Nil) { (a, b) => a.zipAll(b, 42, 1000).map { case (x, y) => x + y }} ?=
			l.zipAll(r, Nil, Nil).flatMap { case (a, b) => a.zipAll(b, 42, 1000).map { case (x, y) => x + y } }
	}


	property("mapReverse") = mappingProperty { seq :Iterable[Int] =>
		seq.mapReverse(s => s + s).toSeq ?= seq.map(s => s + s).toSeq.reverse
	}
}
