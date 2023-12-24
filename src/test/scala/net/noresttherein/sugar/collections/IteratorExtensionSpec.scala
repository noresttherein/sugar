package net.noresttherein.sugar.collections

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Shrink, Test}
import org.scalacheck.Prop._
import org.scalacheck.util.{ConsoleReporter, Pretty}
import net.noresttherein.sugar.extensions.{BooleanExtension, IntExtension, IterableOnceExtension, IteratorExtension, IteratorCompanionExtension, satisfyingMethods}
import net.noresttherein.sugar.testing.scalacheck.extensions._




object IteratorExtensionSpec extends Properties("IteratorExtension") {

	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140)).withMinSuccessfulTests(500).withMaxSize(128)

	import net.noresttherein.sugar.testing.scalacheck.typeClasses._
//
//	implicit def buildableIterableOnce[A] = new Buildable[A, IterableOnce[A]] {
//		override def builder :Builder[A, IterableOnce[A]] = List.newBuilder[A].mapResult(_.iterator)
//	}

	private val Sum = 100

	private def iteratorProperty(prop :(() => Iterator[Int]) => Prop) :Prop =
		forAll { list :List[Int] => prop(() => list.iterator) :| "List" } &&
			forAll { seq :Vector[Int] => prop(() => seq.iterator) :| "Vector" } &&
			forAll { seq :RefArraySlice[Int] => prop(() => seq.iterator) :| "ArraySlice" }
//			forAll { col :OrderedItems[Int] => prop(() => col.iterator) :| "Iterable" }

	private def lazyZipProperty[X :Arbitrary, A]
	                           (f :(() => Iterator[X], () => Iterator[X]) => (Iterator[A], Iterator[A])) :Prop =
	{
		def x() = Arbitrary.arbitrary[X].sample.get
		var i = 0
		val left = { i += 1; x() } #:: { i += 1; x() } #:: { i += 1; x() } #:: LazyList.empty
		var j = 0
		val right = { j += 1; x() } #:: { j += 1; x() } #:: { j += 1; x() } #:: LazyList.empty
		val (l, r) = f(() => left.iterator, () => right.iterator)
		(i <= 1) :| "evaluated " + i + " elements of left " + left &&
			(j <= 1) :| "evaluated " + j + "elements of right " + right &&
			(l sameElements r) lbl {
				val (a, b) = f(() => left.iterator, () => right.iterator)
				s"left:  $left\nright: $right\n${a.toSeq} ?= ${b.toSeq}"
			}
	}
	private def lazyFlatZipProperty[E :Arbitrary, A]
	            (f :(() => Iterator[IterableOnce[E]], () => Iterator[IterableOnce[E]]) => (Iterator[A], Iterator[A]))
			:Prop =
		lazyZipProperty { (l :() => Iterator[Seq[E]], r :() => Iterator[Seq[E]]) => f(l, r) }
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
			forAll { (a :RefArraySlice[Int], b :RefArraySlice[Int]) => prop(a, b) :| "ArraySlice" } &&
			forAll { (a :LazyList[Int], b :LazyList[Int]) => prop(a, b) :| "LazyList" } &&
			lazyZipProperty(f)
	}

	def zipEvenProperty[X, A](test :(() => Iterator[X], () => Iterator[X]) => Iterator[A],
	                          expect :(() => Iterator[X], () => Iterator[X]) => Iterator[A])
	                         (implicit a :Arbitrary[X], s :Shrink[X], pretty :X => Pretty)
			:Prop =
	{
		def prop(l :Iterable[X], r :Iterable[X]) =
			if (l.size == r.size)
				test(() => l.iterator, () => r.iterator) sameElements expect(() => l.iterator, () => r.iterator) lbl {
					test(() => l.iterator, () => r.iterator).toSeq.toString + " ?= " +
						expect(() => l.iterator, () => r.iterator).toSeq
				}
			else
				test(() => l.iterator, () => r.iterator).toSeq.throws[NoSuchElementException] lbl {
						expect(() => l.iterator, () => r.iterator).toSeq.toString
				}
		forAll { (l :List[X], r :List[X]) => prop(l, r) :| "List" } &&
			forAll { (l :Vector[X], r :Vector[X]) => prop(l, r) :| "Vector" } &&
			forAll { (l :RefArraySlice[X], r :RefArraySlice[X]) => prop(l, r) :| "Iterable" } &&
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
				s"left:  $a\nright: $b\n${l.toSeq} ?= ${r.toSeq}"
			}
		}
		forAll { (a :List[List[Int]], b :List[List[Int]]) => prop(a, b) :| "List" } &&
			forAll { (a :Vector[ArraySeq[Int]], b :Vector[ArraySeq[Int]]) => prop(a, b) :| "Vector" } &&
			forAll { (a :RefArraySlice[RefArraySlice[Int]], b :RefArraySlice[RefArraySlice[Int]]) =>
				prop(a, b) :| "ArraySlice"
		} && lazyFlatZipProperty { (l :() => Iterator[IterableOnce[Int]], r :() => Iterator[IterableOnce[Int]]) =>
			f(() => l().map(_.toBasicOps.toSeq), () => r().map(_.toBasicOps.toSeq))
		} :| "LazyList"
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
			forAll { (a :RefArraySlice[Int], b :RefArraySlice[Int], c :RefArraySlice[Int]) =>
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

	property("zipTail") = iteratorProperty { (it :(() => Iterator[Int])) =>
		val i = it()
		if (!i.hasNext)
			i.zipTail.toSeq ?= Seq.empty
		else {
			val i1 = it()
			val i2 = it()
			i2.next()
			i.zipTail.toSeq ?= i1.zip(i2).toSeq
		}
	}


	private def mappingProperty[A](f :(() => Iterator[Int]) => (Iterator[A], Iterator[A])) :Prop = {
		def prop(items :Iterable[Int]) = {
			val (l, r) = f(() => items.iterator)
			r sameElements l lbl {
				val (l, r) = f(() => items.iterator)
				s"Expected: ${r.toSeq};\n     got: ${l.toSeq}"
			}
		}
		forAll { list :List[Int] => prop(list) :| "List" } &&
			forAll { seq :Vector[Int] => prop(seq) :| "Vector" } &&
			forAll { seq :RefArraySlice[Int] => prop(seq) :| "ArraySlice" } && {
				var i = 0
				val list = { i += 1; i } #:: { i += 1; i } #:: { i += 1; i } #:: LazyList.empty
				f(() => list.iterator)
				//i <= 1, not i == 0, because hasNext initializes the first value
				(i <= 1) :| "LazyList(1, 2, 3) had " + i + " evaluated elements" && prop(list) :| "LazyList"
			}
	}

	private def flatMappingProperty[A](f :(() => Iterator[IterableOnce[Int]]) => (Iterator[A], Iterator[A])) :Prop = {
		def prop(items :Iterable[IterableOnce[Int]]) = {
			val (l, r) = f(() => items.iterator)
			l sameElements r lbl {
				val (l, r) = f(() => items.iterator)
				s"Expected: ${r.toSeq};\n     got: ${l.toSeq}"
			}
		}
		forAll { list :List[List[Int]] => prop(list) :| "List" } &&
			forAll { seq :Vector[Vector[Int]] => prop(seq) :| "Vector" } &&
			forAll { col :RefArraySlice[RefArraySlice[Int]] => prop(col) :| "ArraySlice" } &&
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
			val seq = es.toBasicOps.toSeq
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
			val seq = es.toBasicOps.toSeq
			(acc + seq.sum > Sum, acc + seq.sum, seq.map(_ + acc))
		} -> {
			val (sums1, sums2) = iter().map(_.toBasicOps.sum).scanLeft(0)(_ + _).duplicate
			iter().zip(sums1).map {
				case (es, acc) => es.iterator.map(_ + acc)
			}.zip(sums2.drop(1)).takeWhile(_._2 <= Sum).flatMap(_._1)
		}
	}

	property("mapSome") = mappingProperty { iter :(() => Iterator[Int]) =>
		iter().mapSome(0)((acc, e) => if (acc + e <= Sum) Some((acc + e, e * e)) else None) ->
			iter().scanLeft((0, 0)) { case ((acc, _), e) => (acc + e, e * e) }.drop(1).takeWhile(_._1 <= Sum).map(_._2)
	}
	property("flatMapSome") = flatMappingProperty { (iter :() => Iterator[IterableOnce[Int]]) =>
		iter().flatMapSome(0) { (acc, e) =>
			val seq = e.toBasicOps.toSeq
			(acc + seq.sum, e) satisfying (_._1 <= Sum)
		} ->
			iter().scanLeft((0, Nil :IterableOnce[Int])) {
				case ((acc, _), e) => val seq = e.toBasicOps.toSeq; (acc + seq.sum, seq)
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


	private def lazyIndexProperty(idx :Int, knownDeltaSizeLimit :Int = -1, deltaSizeLimit :Int = -1)
	                             (f :(() => Iterator[Int]) => (Iterator[Int], IterableOnce[Int])) =
		forAll { seq :Vector[Int] =>
			val list = LazyList.from(seq)
			if (idx < 0 || idx > knownDeltaSizeLimit && list.iterator.knownSize == 0)
				f(() => list.iterator).throws[IndexOutOfBoundsException]
			else if (idx > seq.size + deltaSizeLimit) {
				val (result, _) = f(() => list.iterator) //checks that it doesn't throw an exception here
				Vector.from(result).throws[IndexOutOfBoundsException]
			} else {
				var x = 0
				val list = { x += 1; 42 } #:: { x += 1; 44 } #:: LazyList.from(seq)
				val (result, expect) = f(() => list.iterator)
				Prop(x <= 1) :| "LazyList.iterator.removed evaluated before actual access" &&
					(result sameElements expect)
			}
		} :| "LazyList"


	property("keep") = iteratorProperty { iter :(() => Iterator[Int]) =>
		implicit val infiniteLazyList :Arbitrary[LazyList[Boolean]] =
			Arbitrary(Gen.infiniteLazyList(Arbitrary.arbBool.arbitrary))
		implicit val shrink :Shrink[LazyList[Boolean]] = Shrink { list :LazyList[Boolean] => list #:: Stream.empty }
		implicit val pretty :LazyList[Boolean] => Pretty = list => Pretty { _ => list.toString }
		forAll { bools :LazyList[Boolean] =>
			val pred = bools.take(iter().size).toIndexedSeq
			val expect = iter().zipWithIndex.collect { case (elem, i) if pred(i) => elem }.toSeq
			expect =? iter().keep(pred).toSeq
		}
	}

	property("distinct") = iteratorProperty { iter :(() => Iterator[Int]) =>
		val seen = new mutable.HashSet[Int]
		val expect = iter().toList.collect { case elem if seen.add(elem) => elem }
		expect =? iter().distinct.toList
	}
//	property("removed") = forAll { i :Int =>
//		mappingProperty { list :Iterable[Int] =>
//			list.removed(i) -> (list.take(i) ++ (if (i == Int.MaxValue) Nil else list.drop(i + 1)))
//		}
//	}
	property("removed") = forAll { i :Int =>
		def expect(iter :() => Iterator[Int]) :Iterator[Int] =
			iter().take(i) ++ (if (i == Int.MaxValue) Nil else iter().drop(i + 1))
		iteratorProperty { iter :(() => Iterator[Int]) =>
			if (i < 0 || iter().isEmpty || { val size = iter().knownSize; size >= 0 & i >= size })
				iter().removed(i).throws[IndexOutOfBoundsException]
			else if (i >= iter().size) {
				iter().removed(i)
				iter().removed(i).toSeq.throws[IndexOutOfBoundsException]
			} else
				iter().removed(i).toSeq ?= expect(iter).toSeq
		} && lazyIndexProperty(i) {
			iter => iter().removed(i) -> expect(iter)
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

	property("updated") = forAll { (i :Int, value :Int) =>
		def expect(iter :() => Iterator[Int]) :Iterator[Int] =
			iter().take(i) ++ Iterator.single(value) ++ (if (i == Int.MaxValue) Iterator.empty else iter().drop(i + 1))
		iteratorProperty { iter :(() => Iterator[Int]) =>
			val it = iter()
			if (i < 0 || !it.hasNext || it.knownSize >= 0 && i >= it.knownSize)
				iter().updated(i, value).throws[IndexOutOfBoundsException]
			else if (i >= iter().size) {
				iter().updated(i, value)
				iter().updated(i, value).toSeq.throws[IndexOutOfBoundsException]
			} else
				iter().updated(i, value).toSeq ?= expect(iter).toSeq
		} && lazyIndexProperty(i) {
			iter => iter().updated(i, value) -> expect(iter)
		}
	}
	//permissive index version
//	property("updatedAll") = forAll { (i :Int, values :Iterable[Int]) =>
//		mappingProperty { iter :(() => Iterator[Int]) =>
//			iter().updatedAll(i, values.iterator) -> (
//				if (i < 0) values.slice(-i, -i + iter().size).iterator ++ iter().drop(values.size + i)
//				else iter().take(i) ++ values.take(iter().size - i) ++ iter().drop(i + values.size)
//			)
//		}
//	}
	property("updatedAll") = forAll { (i :Int, values :Iterable[Int]) =>
		def expect(iter :() => Iterator[Int]) :Iterator[Int] =
			iter().take(i) ++ values.iterator ++ iter().drop(i).drop(values.size)
		iteratorProperty { iter :(() => Iterator[Int]) =>
			val it = iter()
			val limit = if (values.knownSize >= 0) it.knownSize - values.size else it.knownSize
			if (i < 0 || it.knownSize >= 0 && i > limit)
				iter().updatedAll(i, values).throws[IndexOutOfBoundsException]
			else if (i > iter().size - values.size) {
				iter().updatedAll(i, values)
				iter().updatedAll(i, values).toSeq.throws[IndexOutOfBoundsException]
			} else
				iter().updatedAll(i, values).toSeq ?= expect(iter).toSeq
		} && lazyIndexProperty(i, if (values.knownSize >= 0) -values.knownSize else 0, -values.size) {
			iter => iter().updatedAll(i, values) -> expect(iter)
		}
	}

	property("inserted") = forAll { (i :Int, value :Int) =>
		def expect(iter :() => Iterator[Int]) :Iterator[Int] =
			iter().take(i) ++ Iterator.single(value) ++ iter().drop(i)
		iteratorProperty { iter :(() => Iterator[Int]) =>
			val it = iter()
			if (i < 0 || it.knownSize >= 0 && i > it.knownSize)
				iter().inserted(i, value).throws[IndexOutOfBoundsException]
			else if (i > iter().size) {
				iter().inserted(i, value)
				iter().inserted(i, value).toSeq.throws[IndexOutOfBoundsException]
			} else
				iter().inserted(i, value).toSeq ?= expect(iter).toSeq
		} && lazyIndexProperty(i, 0, 0) {
			iter => iter().inserted(i, value) -> expect(iter)
		}
	}
	property("insertedAll") = forAll { (i :Int, values :Iterable[Int]) =>
		def expect(iter :() => Iterator[Int]) :Iterator[Int] =
			iter().take(i) ++ values.iterator ++ iter().drop(i)
		iteratorProperty { iter :(() => Iterator[Int]) =>
			val it = iter()
			if (i < 0 || it.knownSize >= 0 && i > it.knownSize)
				iter().insertedAll(i, values).throws[IndexOutOfBoundsException]
			else if (i > iter().size) {
				iter().insertedAll(i, values)
				iter().insertedAll(i, values).toSeq.throws[IndexOutOfBoundsException]
			} else
				iter().insertedAll(i, values).toSeq ?= expect(iter).toSeq
		} && lazyIndexProperty(i, 0, 0) {
			iter => iter().insertedAll(i, values) -> expect(iter)
		}
	}

	private def concatProperty(f :(Iterator[Int], Iterator[Int]) => Iterator[Int]) =
		zipProperty { (first :() => Iterator[Int], second :() => Iterator[Int]) =>
			f(first(), second()) -> (first() ++ second())
		}
	property(":++")          = concatProperty { (first, second) => first :++ second }
	property("appendedAll")  = concatProperty { (first, second) => first appendedAll second }
	property("++:")          = concatProperty { (first, second) => first ++: second }
	property("prependedAll") = concatProperty { (first, second) => second prependedAll first }


	//todo: multiarg prepended/appended tests
	private def concat1Property(f :(() => Iterator[Int], Int) => (Iterator[Int], Iterator[Int])) =
		forAll { elem :Int =>
			mappingProperty { iter :(() => Iterator[Int]) => f(iter, elem) }
		}
	property(":+")        = concat1Property { (iter, elem) => (iter() :+ elem) -> (iter() ++ Iterator.single(elem)) }
	property("appended")  = concat1Property { (iter, elem) => (iter() appended elem) -> (iter() ++ Iterator.single(elem)) }
	property("+")         = concat1Property { (iter, elem) => (iter() + elem) -> (iter() ++ Iterator.single(elem)) }
	property("add")       = concat1Property { (iter, elem) => (iter() add elem) -> (iter() ++ Iterator.single(elem)) }
	property("+:")        = concat1Property { (iter, elem) => (elem +: iter()) -> (Iterator.single(elem) ++ iter()) }
	property("prepended") = concat1Property { (iter, elem) => (iter() prepended elem) -> (Iterator.single(elem) ++ iter()) }

	property("safe") = iteratorProperty { (iter :() => Iterator[Int]) =>
		val dim    = 5
		val elems  = iter().toVector :collection.Seq[Int]
		val (prefix, suffix) = elems.splitAt(dim * dim)
		val safe   = iter().safe
		val arrays = Array.ofDim[Int](dim, dim)
		var copied = 0
		var i = 0
		while (i < dim && safe.hasNext) {
			copied += safe.take(dim).copyToArray(arrays(i))
			i += 1
		}
		val next = safe.take(dim)
		val tail = safe.toSeq
		val concat = arrays.take(i).foldLeft(new ArrayBuffer[Int](dim * dim)) {
			(b, a) => b ++= a.take(math.min(dim, copied - b.length))
		}
		(math.min(dim * dim, elems.length) =? copied) &&
			(prefix =? concat lbl "copied") &&
			(elems.drop(copied) =? tail lbl "tail") &&
			(Seq.empty[Int] =? next.toSeq lbl "exhausted side iterator") lbl
				"copied " + copied + ": " + concat lbl "remained: " + tail
	}

	property("counting")  = iteratorProperty { iter :(() => Iterator[Int]) =>
		val counter = iter().counting
		(counter sameElements iter() lbl s"${iter().counting.toList} ?= ${iter().toList}") &&
			(counter.total ?= iter().size)
	}
	property("counting.drop") = iteratorProperty { iter :(() => Iterator[Int]) =>
		forAll { (n :Int, m :Int) =>
			val counter = iter().counting
			var i = n
			while (i > 0 && counter.hasNext) {
				counter.next()
				i -= 1
			}
			val dropped = counter.drop(m)
			val tail = dropped.toList
			val nm = if (n <= Int.MaxValue - m.orZeroIf(m < 0)) n.orZeroIf(n < 0) + m.orZeroIf(m < 0) else Int.MaxValue
			val expect = iter().drop(nm).toList
			(tail ?= expect) && (dropped.total ?= iter().size)
		}
	}
	property("counting.take") = iteratorProperty { iter :(() => Iterator[Int]) =>
		forAll { (n :Int, m :Int) =>
			val counter = iter().counting
			var i = n
			while (i > 0 && counter.hasNext) {
				counter.next()
				i -= 1
			}
			val taken = counter.take(m)
			val tail = taken.toList
			val until = n.orZeroIf(n < 0) + m.orZeroIf(m < 0)
			val expect = iter().slice(n, if (until < 0) Int.MaxValue else until).toList
			(tail ?= expect) && (taken.total ?= expect.size + math.min(iter().size, n.orZeroIf(n < 0)))
		}
	}
	property("counting.slice") = iteratorProperty { iter :(() => Iterator[Int]) =>
		forAll { (n :Int, m :Int) =>
			val counter = iter().counting.slice(n, m)
			val tail   = counter.toList
			val expect = iter().slice(n, m.orZeroIf(m < 0)).toList
			(tail ?= expect) && (counter.total ?= expect.size + math.min(iter().size, n.orZeroIf(n < 0)))
		}
	}

	//todo: test cyclicCopyToArray
}





//todo: test copyToArray, take, drop, slice
object IteratorCompanionExtensionSpec extends Properties("Iterator$") {
	property("two")   = Iterator.two(1, 2).toSeq ?= Seq(1, 2)
	property("const") =
		(Iterator.const(10)(42).toSeq ?= Iterator.fill(10)(42).toSeq) &&
			(Iterator.const(0)(42).toSeq ?= Iterator.fill(0)(42).toSeq) &&
			(Iterator.const(-1)(42).toSeq ?= Iterator.fill(-1)(42).toSeq)
	property("over")  = forAll { array :Array[Int] => Iterator.over(array).toSeq ?= ArraySeq.unsafeWrapArray(array) }
//	property("over")         = Iterator.over(testArray).toSeq ?= testArray.toSeq
	property("slice") = forAll { (array :Array[Int], from :Int, until :Int) =>
		Iterator.slice(array, from, until).toSeq ?= array.iterator.slice(from, until).toSeq
	}
	property("reverse") = forAll { (array :Array[Int]) =>
		Iterator.reverse(array).toSeq ?= ArraySeq.unsafeWrapArray(array).reverse
	}
	property("reverse(Array, Int, Int)") = forAll { (array :Array[Int], from :Int, until :Int) =>
		Iterator.reverse(array, from, until).toSeq ?= ArraySeq.unsafeWrapArray(array.slice(from, until)).reverse
	}
}
