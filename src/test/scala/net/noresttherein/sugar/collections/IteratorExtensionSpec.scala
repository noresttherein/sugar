package net.noresttherein.sugar.collections

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.Builder

import net.noresttherein.sugar.extensions.{IterableExtension, IterableOnceExtension, IteratorExtension, satisfyingMethods}
import net.noresttherein.sugar.testing.scalacheck.extensions._
import org.scalacheck.Prop._
import org.scalacheck.util.{Buildable, ConsoleReporter, Pretty}
import org.scalacheck.{Arbitrary, Prop, Properties, Shrink, Test}


object IteratorExtensionSpec extends Properties("IteratorExtension") {

	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140)).withMinSuccessfulTests(1000)

	import IterableOnceExtensionSpec.buildableArraySliceSeq
//
//	implicit def buildableIterableOnce[A] = new Buildable[A, IterableOnce[A]] {
//		override def builder :Builder[A, IterableOnce[A]] = List.newBuilder[A].mapResult(_.iterator)
//	}

	implicit def iterableOnceGenerator[A :Arbitrary] :Arbitrary[IterableOnce[A]] =
		Arbitrary(Arbitrary.arbitrary[List[A]].map(_.iterator))

	private val Sum = 100

	def iteratorProperty(prop :(() => Iterator[Int]) => Prop) :Prop =
		forAll { list :List[Int] => prop(() => list.iterator) :| "List" } &&
			forAll { seq :Vector[Int] => prop(() => seq.iterator) :| "Vector" } &&
			forAll { seq :ArraySliceSeq[Int] => prop(() => seq.iterator) :| "ArraySlice" }
//			forAll { col :OrderedItems[Int] => prop(() => col.iterator) :| "Iterable" }

	def mappingProperty[A](f :(() => Iterator[Int]) => (Iterator[A], Iterator[A])) :Prop = {
		def prop(items :Iterable[Int]) = {
			val (l, r) = f(() => items.iterator)
			r sameElements l lbl {
				val (l, r) = f(() => items.iterator)
				s"Expected: ${l.toSeq};\n     got: ${r.toSeq}"
			}
		}
		forAll { list :List[Int] => prop(list) :| "List" } &&
			forAll { seq :Vector[Int] => prop(seq) :| "Vector" } &&
			forAll { seq :ArraySliceSeq[Int] => prop(seq) :| "ArraySlice" } && {
				var i = 0
				val list = { i += 1; i } #:: { i += 1; i } #:: { i += 1; i } #:: LazyList.empty
				f(() => list.iterator)
				(i <= 1) :| "LazyList(1, 2, 3) had " + i + " evaluated elements" && prop(list) :| "LazyList"
			}
	}

	def flatMappingProperty[A](f :(() => Iterator[IterableOnce[Int]]) => (Iterator[A], Iterator[A])) :Prop = {
		def prop(items :Iterable[IterableOnce[Int]]) = {
			val (l, r) = f(() => items.iterator)
			l sameElements r lbl {
				val (l, r) = f(() => items.iterator)
				s"Expected: ${l.toSeq};\n     got: ${r.toSeq}"
			}
		}
		forAll { list :List[List[Int]] => prop(list) :| "List" } &&
			forAll { seq :Vector[Vector[Int]] => prop(seq) :| "Vector" } &&
//			forAll { col :ArraySliceSeq[ArraySliceSeq[Int]] => prop(col) :| "ArraySlice" } &&
			forAll { items :List[Int] =>
				var i = 0
				val list = { i += 1; items } #:: { i += 1; items.map(_ + 1) } #:: { i += 1; items.map(_ - 1) } #::
					LazyList.empty
				f(() => list.iterator)
				(i <= 1) :| "LazyList had " + i + " evaluated elements" && (prop(list) :| "LazyList")
//				(if (items.nonEmpty) i ?= 1
//				else i ?= 3
//				) :| "LazyList had " + i + " evaluated elements" && (r =? l lbl "LazyList")
			}
	}

	property("mapWith") = mappingProperty { iter :(() => Iterator[Int]) =>
		iter().mapWith(0) { (e, sum) => (e + sum, e + sum) } -> iter().scanLeft(0)(_ + _).drop(1)
	}
	property("flatMapWith") = flatMappingProperty { iter :(() => Iterator[IterableOnce[Int]]) =>
		val flatMapped = iter().flatMapWith(0) { (es, sum) =>
			val res = es.iterator.scanLeft(sum)(_ + _) to Vector
			(res.tail, res.last)
		}
		flatMapped -> iter().flatten.scanLeft(0)(_ + _).drop(1)
	}

	property("mapWithIndex") = mappingProperty { iter :(() => Iterator[Int]) =>
		iter().mapWithIndex { (e, i) => (e, i) } -> iter().zipWithIndex
	}
/*
	property("flatMapWithIndex") = flatMappingProperty { iter :(() => Iterator[IterableOnce[Int]]) =>
		iter().flatMapWithIndex { (e, i) => e.iterator.map { (_, i) } to ArraySeq } ->
			iter().zipWithIndex.flatMap { case (e, i) => e.iterator.map((_, i)) to ArraySeq }
	}
	property("collectWithIndex") = mappingProperty { iter :(() => Iterator[Int]) =>
		iter().collectWithIndex { case (e, i) if e > 0 => e.toLong * i } ->
			iter().zipWithIndex.collect { case (e, i) if e > 0 => e.toLong * i }
	}

	property("mapWhile") = mappingProperty { iter :(() => Iterator[Int]) =>
		iter().mapWhile(0)(_ <= Sum)((acc, e) => (e + acc, 2 * e + acc)) ->
			iter().scanLeft((0, 0, 0)) {
				case (acc, e) => (acc._2, e + acc._2, 2 * e + acc._2)
			}.drop(1).takeWhile(_._1 <= Sum).map(_._3)
	}
	property("flatMapWhile") = flatMappingProperty { iter :(() => Iterator[IterableOnce[Int]]) =>
		iter().flatMapWhile(0)(_ <= Sum) { (acc, es) =>
			val seq = es.toIterableOnceOps.toSeq
			(acc + seq.sum, seq.map(_ + acc))
		} -> {
			val sums = iter().map(_.iterator.sum).scanLeft(0)(_ + _).takeWhile(_ <= Sum)
			iter().zip(sums).flatMap { case (es, acc) => es.iterator.map(_ + acc) }
		}
	}

	property("mapUntil") = mappingProperty { iter :(() => Iterator[Int]) =>
		iter().mapUntil(0)((acc, e) => (e + acc > Sum, e + acc, 2 * e + acc)) ->
			iter().scanLeft((0, 0)) {
				case (acc, e) => (e + acc._1, 2 * e + acc._1)
			}.drop(1).takeWhile(_._1 <= Sum).map(_._2)
	}
	property("flatMapUntil") = flatMappingProperty { iter :(() => Iterator[IterableOnce[Int]]) =>
		iter().flatMapUntil(0) { (acc, es) =>
			val seq = es.toIterableOnceOps.toSeq
			(acc + seq.sum > Sum, acc + seq.sum, seq.map(_ + acc))
		} -> {
			val sums = iter().map(_.toIterableOnceOps.sum).scanLeft(0)(_ + _)
			iter().zip(sums).map {
				case (es, acc) => es.iterator.map(_ + acc)
			}.zip(sums.drop(1)).takeWhile(_._2 <= Sum).flatMap(_._1)
		}
	}

	property("mapSome") = mappingProperty { iter :(() => Iterator[Int]) =>
		iter().mapSome(0)((acc, e) => if (acc + e <= Sum) Some((acc + e, e * e)) else None) ->
			iter().scanLeft((0, 0)) { case ((acc, _), e) => (acc + e, e * e) }.drop(1).takeWhile(_._1 <= Sum).map(_._2)
	}
	property("flatMapSome") = flatMappingProperty { (iter :() => Iterator[IterableOnce[Int]]) =>
		iter().flatMapSome(0) { (acc, e) =>
			val seq = e.toIterableOnceOps.toSeq
			(acc + seq.sum, e) satisfying (_._1 <= Sum)
		} ->
			iter().scanLeft((0, Nil :IterableOnce[Int])) {
				case ((acc, _), e) => val seq = e.toIterableOnceOps.toSeq; (acc + seq.sum, seq)
			}.drop(1).takeWhile(_._1 <= Sum).flatMap(_._2)
	}
	//no mapReverse for Iterator
//	property("mapReverse") = iteratorProperty { iter :(() => Iterator[Int]) =>
//		iter().mapReverse(s => s + s).toSeq ?= seq.map(s => s + s).to(LazyList).reverse
//	}


	property("filterWith") = mappingProperty { iter :(() => Iterator[Int]) =>
		iter().filterWith(0) { (e, total) => (e > total, total + e) } ->
			iter().zip(iter().scanLeft(0)(_ + _)).collect { case (e, total) if e > total => e }
	}
	property("filterWithIndex") = mappingProperty { iter :(() => Iterator[Int]) =>
		iter().filterWithIndex { (e, i) => e % 2 == i % 2 } ->
			iter().zipWithIndex.collect { case (e, i) if e % 2 == i % 2 => e }
	}
	property("partitionWith._1") = mappingProperty { iter :(() => Iterator[Int]) =>
		val result = iter().partitionWith(0) { (e, total) => (e > total, total + e) }
		val zipped = iter().zip(iter().scanLeft(0)(_ + _))
		val expectYes = zipped.collect { case (e, total) if e > total =>  e }
//		val expectNo  = zipped.collect { case (e, total) if e <= total =>  e }
		result._1 -> expectYes
	}
	property("partitionWith._2") = mappingProperty { iter :(() => Iterator[Int]) =>
		val result = iter().partitionWith(0) { (e, total) => (e > total, total + e) }
		val zipped = iter().zip(iter().scanLeft(0)(_ + _))
//		val expectYes = zipped.collect { case (e, total) if e > total =>  e }
		val expectNo  = zipped.collect { case (e, total) if e <= total =>  e }
		result._2 -> expectNo
	}
	property("partitionWithIndex._1") = mappingProperty { iter :(() => Iterator[Int]) =>
		val result = iter().partitionWithIndex { (e, i) => e % 2 == i % 2 }
		val expectYes = iter().zipWithIndex.collect { case (e, i) if e % 2 == i % 2 => e }
//		val expectNo  = iter().zipWithIndex.collect { case (e, i) if e % 2 != i % 2 => e }
		result._1 -> expectYes
	}
	property("partitionWithIndex._2") = mappingProperty { iter :(() => Iterator[Int]) =>
		val result = iter().partitionWithIndex { (e, i) => e % 2 == i % 2 }
//		val expectYes = iter().zipWithIndex.collect { case (e, i) if e % 2 == i % 2 => e }
		val expectNo  = iter().zipWithIndex.collect { case (e, i) if e % 2 != i % 2 => e }
		result._2 -> expectNo
	}



	private def lazyZipProperty[X :Arbitrary, A]
	                           (f :(() => Iterator[X], () => Iterator[X]) => (Iterator[A], Iterator[A])) :Prop =
	{
		def x() = Arbitrary.arbitrary[X].sample.get
		var i = 0
		val left = { i += 1; x() } #:: { i += 1; x() } #:: { i += 1; x() } #:: LazyList.empty
		var j = 0
		val right = { j += 1; x() } #:: { j += 1; x() } #:: { j += 1; x() } #:: LazyList.empty
		val (l, r) = f(() => left.iterator, () => right.iterator)
		(i ?= 0) :| "evaluated " + i + " elements of left " + left &&
			(j ?= 0) :| "evaluated " + j + "elements of right " + right &&
			(l sameElements r) lbl {
				val (a, b) = f(() => left.iterator, () => right.iterator)
				s"${a.toSeq} ?= ${b.toSeq}"
			}
	}
//	private def lazyListProperty[A](f :(LazyList[Int], LazyList[Int]) => (A, A)) :Prop = lazyListProperty(Random.nextInt)(f)

	private def lazyZip3Property[X :Arbitrary, A]
		                        (f :(() => Iterator[X], () => Iterator[X], () => Iterator[X]) => (Iterator[A], Iterator[A]))
			:Prop =
		lazyZipProperty[X, A]((a, b) => f(a, b, b))

	def zipProperty[A](f :(() => Iterator[Int], () => Iterator[Int]) => (Iterator[A], Iterator[A])) :Prop = {
		def prop(a :Iterable[Int], b :Iterable[Int]) = {
			val (l, r) = f(() => a.iterator, () => b.iterator)
			r sameElements l lbl {
				val (l, r) = f(() => a.iterator, () => b.iterator)
				s"${l.toSeq} ?= ${r.toSeq}"
			}
		}
		forAll { (a :List[Int], b :List[Int]) => prop(a, b) :| "List" } &&
			forAll { (a :Vector[Int], b :Vector[Int]) => prop(a, b) :| "Vector" } &&
			forAll { (a :ArraySliceSeq[Int], b :ArraySliceSeq[Int]) => prop(a, b) :| "ArraySlice" } &&
			lazyZipProperty(f)
	}

	def zipEvenProperty[X, A](test :(() => Iterator[X], () => Iterator[X]) => Iterator[A],
	                          expect :(() => Iterator[X], () => Iterator[X]) => Iterator[A])
	                         (implicit a :Arbitrary[X], s :Shrink[X], pretty :X => Pretty)
			:Prop =
	{
		def prop(l :Iterable[X], r :Iterable[X]) =
			if (l.size == r.size)
				test(() => l.iterator, () => r.iterator) ?= expect(() => l.iterator, () => r.iterator)
			else
				test(() => l.iterator, () => r.iterator).toString.throws[NoSuchElementException] lbl expect(() => l.iterator, () => r.iterator).toString
		forAll { (l :List[X], r :List[X]) => prop(l, r) :| "List" } &&
			forAll { (l :Vector[X], r :Vector[X]) => prop(l, r) :| "Vector" } &&
			forAll { (l :ArraySliceSeq[X], r :ArraySliceSeq[X]) => prop(l, r) :| "Iterable" } &&
			forAll { (l :LazyList[X], r :LazyList[X]) =>
				val res = test(() => l.iterator, () => r.iterator)
				(try
					res sameElements expect(() => l.iterator, () => r.iterator) lbl {
						val a = test(() => l.iterator, () => r.iterator)
						val b = expect(() => l.iterator, () => r.iterator)
						s"${a.toSeq} ?= ${b.toSeq}"
					}
				catch {
					case _ :NoSuchElementException if l.size != r.size => Prop.passed
				}) :| "LazyList"
			} && lazyZipProperty {
				(l :() => Iterator[X], r :() => Iterator[X]) => test(l, r) -> expect(l, r)
			}
	}


	property("zipEven") = zipEvenProperty(
		(l :() => Iterator[Int], r :() => Iterator[Int]) => l() zipEven r(),
		(l :() => Iterator[Int], r :() => Iterator[Int]) => l() zip r()
	)

	property("zipMap") = zipProperty { (l :() => Iterator[Int], r :() => Iterator[Int]) =>
		l().zipMap(r())((a, b) => (a + b) * 2) -> l().zip(r()).map { case (a, b) => (a + b) * 2 }
	}
	property("zipMapEven") = zipEvenProperty(
		(l :() => Iterator[Int], r :() => Iterator[Int]) => l().zipMapEven(r())((a, b) => (a + b) * 2),
		(l :() => Iterator[Int], r :() => Iterator[Int]) => l().zip(r()).map { case (a, b) => (a + b) * 2 }
	)
	property("zipMapAll") = zipProperty { (l :() => Iterator[Int], r :() => Iterator[Int]) =>
		l().zipMapAll(r(), 42, 1000)((a, b) => (a + b) * 2) ->
			l().zipAll(r(), 42, 1000).map { case (a, b) => (a + b) * 2 }
	}

	private def zipFlatMapProperty[A](f :(() => Iterator[IterableOnce[Int]], () => Iterator[IterableOnce[Int]])
	                                      => (Iterator[A], Iterator[A])) :Prop =
	{
		def prop(a :Iterable[Iterable[Int]], b :Iterable[Iterable[Int]]) = {
			val (l, r) = f(() => a.iterator, () => b.iterator)
			r sameElements l lbl {
				val (l, r) = f(() => a.iterator, () => b.iterator)
				s"${l.toSeq} ?= ${r.toSeq}"
			}
		}
		forAll { (a :List[List[Int]], b :List[List[Int]]) => prop(a, b) :| "List" } &&
			forAll { (a :Vector[ArraySeq[Int]], b :Vector[ArraySeq[Int]]) => prop(a, b) :| "Vector" } &&
			forAll { (a :ArraySliceSeq[ArraySliceSeq[Int]], b :ArraySliceSeq[ArraySliceSeq[Int]]) =>
				prop(a, b) :| "ArraySlice"
		} && lazyZipProperty(f)
	}

//	private def zipFlatMapEvenProperty(prop :(Iterable[Iterable[Int]], Iterable[Iterable[Int]]) => Prop) :Prop =
//		forAll { (l :List[List[Int]], r :List[List[Int]]) => prop(l, r) } &&
//			forAll { (l :Vector[ArraySeq[Int]], r :Vector[ArraySeq[Int]]) => prop(l, r) } &&
//			forAll { (l :OrderedItems[OrderedItems[Int]], r :OrderedItems[OrderedItems[Int]]) => prop(l, r) }

	property("zipFlatMap") = zipFlatMapProperty {
		(l :() => Iterator[IterableOnce[Int]], r :() => Iterator[IterableOnce[Int]]) =>
			l().zipFlatMap(r())((a, b) => a.iterator.zip(b.iterator).map { case (x, y) => x + y }) ->
				l().zip(r()).flatMap { case (a, b) => a.iterator.zip(b.iterator).map { case (x, y) => x + y } }
		}
	property("zipFlatMapEven") = zipEvenProperty(
		(l :() => Iterator[IterableOnce[Int]], r :() => Iterator[IterableOnce[Int]]) =>
			l().zipFlatMapEven(r()) { (a, b) => a.iterator.zip(b.iterator).map { case (x, y) => x + y }},
		(l :() => Iterator[IterableOnce[Int]], r :() => Iterator[IterableOnce[Int]]) =>
			l().zipAll(r(), Nil, Nil).flatMap { case (a, b) => a.iterator.zip(b.iterator).map { case (x, y) => x + y }}
	)
	property("zipFlatMapAll") = zipFlatMapProperty {
		(l :() => Iterator[IterableOnce[Int]], r :() => Iterator[IterableOnce[Int]]) =>
			l().zipFlatMapAll(r(), Nil, Nil) {
				(a, b) => a.iterator.zipAll(b, 42, 1000).map { case (x, y) => x + y }
			} -> l().zipAll(r(), Nil, Nil).flatMap {
					case (a, b) => a.iterator.zipAll(b, 42, 1000).map { case (x, y) => x + y }
				}
		}

	def zip3Property[A](f :(() => Iterator[Int], () => Iterator[Int], () => Iterator[Int]) => (Iterator[A], Iterator[A]))
			:Prop =
	{
		def prop(a :Iterable[Int], b :Iterable[Int], c :Iterable[Int]) = {
			val (l, r) = f(() => a.iterator, () => b.iterator, () => c.iterator)
			l sameElements r lbl {
				val (l, r) = f(() => a.iterator, () => b.iterator, () => c.iterator)
				s"${l.toSeq} ?= ${r.toSeq}"
			}
		}
		forAll { (a :List[Int], b :List[Int], c :List[Int]) => prop(a, b, c) :| "List" } &&
			forAll { (a :Vector[Int], b :Vector[Int], c :Vector[Int]) => prop(a, b, c) :| "Vector" } &&
			forAll { (a :ArraySliceSeq[Int], b :ArraySliceSeq[Int], c :ArraySliceSeq[Int]) =>
				prop(a, b, c) :| "ArraySlice"
		} && lazyZip3Property(f)
	}

	property("zip3") = zip3Property { (a :() => Iterator[Int], b :() => Iterator[Int], c :() => Iterator[Int]) =>
		a().zip3(b(), c()) -> a().zip(b()).zip(c()).map { case ((a, b), c) => (a, b, c) }
	}
	property("zipEven3") = zipEvenProperty(
		(a :() => Iterator[Int], b :() => Iterator[Int]) => a().zipEven3(b(), b()),
		(a :() => Iterator[Int], b :() => Iterator[Int]) => a().zip(b()).zip(b()).map { case ((a, b), c) => (a, b, c) }
	)
	property("zipAll3") = zip3Property { (a :() => Iterator[Int], b :() => Iterator[Int], c :() => Iterator[Int]) =>
		a().zipAll3(b(), c(), 42, 2501, 1024) ->
			a().zipAll(b(), 42, 2501).zipAll(c(), (42, 2501), 1024).map { case ((a, b), c) => (a, b, c) }
	}


//	property("removed") = forAll { i :Int =>
//		mappingProperty { list :Iterable[Int] =>
//			list.removed(i) -> (list.take(i) ++ (if (i == Int.MaxValue) Nil else list.drop(i + 1)))
//		}
//	}
	property("removed") = forAll { i :Int =>
		iteratorProperty { iter :(() => Iterator[Int]) =>
			if (i < 0)
				iter().removed(i).throws[IndexOutOfBoundsException]
			else if (i >= iter().size) {
				iter().removed(i)
				iter().removed(i).toSeq.throws[IndexOutOfBoundsException]
			} else
				iter().removed(i) ?= (iter().take(i) ++ (if (i == Int.MaxValue) Nil else iter().drop(i + 1)))
		} && forAll { seq :Vector[Int] =>
			if (i < 0 || seq.isEmpty)
				LazyList.from(seq).iterator.removed(i).throws[IndexOutOfBoundsException]
			else if (i >= seq.size) {
				val iter = LazyList.from(seq).iterator.removed(i) //validates it doesn't throw an exception here
				Vector.from(iter).throws[IndexOutOfBoundsException]
			} else {
				var evaluated :Boolean = false
				val iter = ({ evaluated = true; 42 } #:: LazyList.from(seq)).iterator.removed(i)
				Prop(!evaluated) :| "LazyList.iterator.removed evaluated before actual access" &&
					(iter sameElements (42 +: seq).take(i) ++ seq.drop(i)) //i < seq.size, so i < Int.MaxValue
			}

		}
	}
	property("removed(from, until)") = forAll { (from :Int, until :Int) =>
		mappingProperty { iter :(() => Iterator[Int]) =>
			iter().removed(from, until) -> (
				if (until <= 0 | until <= from) iter()
				else iter().take(from) ++ iter().drop(until)
			)
		}
	}
*/

}
