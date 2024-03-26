package net.noresttherein.sugar.collections

import scala.collection.immutable.{ArraySeq, StringOps}
import scala.util.Random

import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Shrink, Test}
import org.scalacheck.Prop._
import org.scalacheck.util.{ConsoleReporter, Pretty}
import net.noresttherein.sugar.extensions.{IterableExtension, providingMethods, satisfyingMethods}
import net.noresttherein.sugar.testing.scalacheck.extensions._




object IterableExtensionSpec extends Properties("IterableExtension") {

	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140)).withMinSuccessfulTests(500).withMaxSize(127)

	import net.noresttherein.sugar.testing.scalacheck.typeClasses._
	private val Sum = 100

	def iterableProperty(prop :Iterable[Int] => Prop) :Prop = {
		implicit val ints :Arbitrary[Int] = Arbitrary(Gen.choose(-100, 100))
		forAll { list :List[Int] => prop(list) :| "List" } &&
			forAll { seq :IndexedSeq[Int] => prop(seq) :| "IndexedSeq" } &&
			forAll { seq :RefArraySlice[Int] => prop(seq) :| "ArraySlice" } &&
			forAll { col :OrderedItems[Int] => prop(col) :| "Iterable" }
	}

	private def lazyListProperty[X :Arbitrary, A](f :(LazyList[X], LazyList[X]) => (A, A)) :Prop = {
		def x() = Arbitrary.arbitrary[X].sample.get
		var i = 0
		val left = { i += 1; x() } #:: { i += 1; x() } #:: { i += 1; x() } #:: LazyList.empty
		var j = 0
		val right = { j += 1; x() } #:: { j += 1; x() } #:: { j += 1; x() } #:: LazyList.empty
		val (l, r) = f(left, right)
		(i ?= 0) :| "evaluated " + i + " elements of left " + left &&
			(j ?= 0) :| "evaluated " + j + "elements of right " + right &&
			(l ?= r) :| "lazyListProperty"
	}
//	private def lazyListProperty[A](f :(LazyList[Int], LazyList[Int]) => (A, A)) :Prop = lazyListProperty(Random.nextInt)(f)

	private def lazyListProperty[X :Arbitrary, A](f :(LazyList[X], LazyList[X], LazyList[X]) => (A, A)) :Prop =
		lazyListProperty[X, A]((a, b) => f(a, b, b))

	def zipProperty[A](f :(Iterable[Int], Iterable[Int]) => (A, A)) :Prop =
		forAll { (a :List[Int], b :List[Int]) => val (l, r) = f(a, b);  r =? l label "List" } &&
			forAll { (a :IndexedSeq[Int], b :IndexedSeq[Int]) => val (l, r) = f(a, b); r =? l label "IndexedSeq" } &&
			forAll { (a :RefArraySlice[Int], b :RefArraySlice[Int]) => val (l, r) = f(a, b); r =? l label "ArraySlice" } &&
			forAll { (a :OrderedItems[Int], b :OrderedItems[Int]) => val (l, r) = f(a, b); r =? l label "Iterable" } &&
			lazyListProperty(f)

	def zipEvenProperty[X, A](test :(Iterable[X], Iterable[X]) => A, expect :(Iterable[X], Iterable[X]) => A)
	                         (implicit a :Arbitrary[X], s :Shrink[X], pretty :X => Pretty)
			:Prop =
	{
		def prop(l :Iterable[X], r :Iterable[X]) =
			if (l.size == r.size)
				test(l, r) ?= expect(l, r)
			else
				test(l, r).toString.throws[NoSuchElementException] lbl expect(l, r).toString
		forAll { (l :List[X], r :List[X]) => prop(l, r) label "List" } &&
			forAll { (l :IndexedSeq[X], r :IndexedSeq[X]) => prop(l, r) label "IndexedSeq" } &&
			forAll { (l :OrderedItems[X], r :OrderedItems[X]) => prop(l, r) label "Iterable" } &&
			forAll { (l :RefArraySlice[X], r :RefArraySlice[X]) => prop(l, r) label "ArraySlice" } &&
			forAll { (l :LazyList[X], r :LazyList[X]) =>
				val res = test(l, r)
				(try res ?= expect(l, r) catch {
					case _ :NoSuchElementException if l.size != r.size => Prop.passed
				}) lbl "LazyList"
			} && lazyListProperty((l :LazyList[X], r :LazyList[X]) => test(l, r) -> expect(l, r))
	}


	property("zipEven") = zipEvenProperty(
		(l :Iterable[Int], r :Iterable[Int]) => l zipEven r,
		(l :Iterable[Int], r :Iterable[Int]) => l zip r
	)

	property("zipMap") = zipProperty { (l :Iterable[Int], r :Iterable[Int]) =>
		l.zipMap(r)((a, b) => (a + b) * 2) -> l.zip(r).map { case (a, b) => (a + b) * 2 }
	}
	property("zipMapEven") = zipEvenProperty(
		(l :Iterable[Int], r :Iterable[Int]) => l.zipMapEven(r)((a, b) => (a + b) * 2),
		(l :Iterable[Int], r :Iterable[Int]) => l.zip(r).map { case (a, b) => (a + b) * 2 }
	)
	property("zipMapAll") = zipProperty { (l :Iterable[Int], r :Iterable[Int]) =>
		l.zipMapAll(r, 42, 1000)((a, b) => (a + b) * 2) ->
			l.zipAll(r, 42, 1000).map { case (a, b) => (a + b) * 2 }
	}

	private def zipFlatMapProperty[A](f :(Iterable[Iterable[Int]], Iterable[Iterable[Int]]) => (A, A)) :Prop =
		forAll { (a :List[List[Int]], b :List[List[Int]]) =>
			val (l, r) = f(a, b); r =? l lbl "List"
		} && forAll { (a :Vector[ArraySeq[Int]], b :Vector[ArraySeq[Int]]) =>
			val (l, r) = f(a, b); r =? l lbl "Vector"
		} && forAll { (a :OrderedItems[OrderedItems[Int]], b :OrderedItems[OrderedItems[Int]]) =>
			val (l, r) = f(a, b); r =? l lbl "Iterable"
		} && lazyListProperty(f)

//	private def zipFlatMapEvenProperty(prop :(Iterable[Iterable[Int]], Iterable[Iterable[Int]]) => Prop) :Prop =
//		forAll { (l :List[List[Int]], r :List[List[Int]]) => prop(l, r) } &&
//			forAll { (l :Vector[ArraySeq[Int]], r :Vector[ArraySeq[Int]]) => prop(l, r) } &&
//			forAll { (l :OrderedItems[OrderedItems[Int]], r :OrderedItems[OrderedItems[Int]]) => prop(l, r) }

	property("zipFlatMap") = zipFlatMapProperty { (l :Iterable[Iterable[Int]], r :Iterable[Iterable[Int]]) =>
		l.zipFlatMap(r)((a, b) => a.zip(b).map { case (x, y) => x + y }) ->
			l.zip(r).flatMap { case (a, b) => a.zip(b).map { case (x, y) => x + y} }
	}
	property("zipFlatMapEven") = zipEvenProperty(
		(l :Iterable[Iterable[Int]], r :Iterable[Iterable[Int]]) =>
			l.zipFlatMapEven(r) { (a, b) => a.zip(b).map { case (x, y) => x + y }},
		(l :Iterable[Iterable[Int]], r :Iterable[Iterable[Int]]) =>
			l.zipAll(r, Nil, Nil).flatMap { case (a, b) => a.zip(b).map { case (x, y) => x + y }}
	)
	property("zipFlatMapAll") = zipFlatMapProperty { (l :Iterable[Iterable[Int]], r :Iterable[Iterable[Int]]) =>
		l.zipFlatMapAll(r, Nil, Nil) { (a, b) => a.zipAll(b, 42, 1000).map { case (x, y) => x + y }} ->
			l.zipAll(r, Nil, Nil).flatMap { case (a, b) => a.zipAll(b, 42, 1000).map { case (x, y) => x + y } }
	}

	def zip3Property[A](f :(Iterable[Int], Iterable[Int], Iterable[Int]) => (A, A)) :Prop =
		forAll { (a :List[Int], b :List[Int], c :List[Int]) =>
			val (l, r) = f(a, b, c); r =? l lbl "List"
		} && forAll { (a :IndexedSeq[Int], b :IndexedSeq[Int], c :IndexedSeq[Int]) =>
				val (l, r) = f(a, b, c); r =? l lbl "IndexedSeq"
		} && forAll { (a :OrderedItems[Int], b :OrderedItems[Int], c :OrderedItems[Int]) =>
			val (l, r) = f(a, b, c); r =? l lbl "Iterable"
		} && lazyListProperty(f)

	property("zip3") = zip3Property { (a :Iterable[Int], b :Iterable[Int], c :Iterable[Int]) =>
		a.zip3(b, c) -> a.zip(b).zip(c).map { case ((a, b), c) => (a, b, c) }
	}
	property("zipEven3") = zipEvenProperty(
		(a :Iterable[Int], b :Iterable[Int]) => a.zipEven3(b, b),
		(a :Iterable[Int], b :Iterable[Int]) => a.zip(b).zip(b).map { case ((a, b), c) => (a, b, c) }
	)
	property("zipAll3") = zip3Property { (a :Iterable[Int], b :Iterable[Int], c :Iterable[Int]) =>
		a.zipAll3(b, c, 42, 2501, 1024) ->
			a.zipAll(b, 42, 2501).zipAll(c, (42, 2501), 1024).map { case ((a, b), c) => (a, b, c) }
	}

	property("zipTail") = iterableProperty { items :Iterable[Int] =>
		if (items.sizeIs <= 1) items.zipTail ?= items.iterableFactory.empty
		else items.zipTail ?= items.zip(items.tail)
	}



	def mappingProperty[A](f :Iterable[Int] => (A, A)) :Prop = {
		implicit val ints :Arbitrary[Int] = Arbitrary(Gen.choose(-100, 100))
		forAll { list :List[Int] => val (l, r) = f(list); r =? l label "List" } &&
			forAll { seq :IndexedSeq[Int] => val (l, r) = f(seq); r =? l label "IndexedSeq" } &&
			forAll { col :OrderedItems[Int] => val (l, r) = f(col); r =? l label "Iterable" } &&
			forAll { seq :RefArraySlice[Int] => val (l, r) = f(seq); r =? l label "ArraySlice" } &&
			{
				var i = 0
				val list = { i += 1; i } #:: { i += 1; i } #:: { i += 1; i } #:: LazyList.empty
				val (l, r) = f(list)
				(i ?= 0) :| "LazyList(1, 2, 3) had " + i + " evaluated elements" && (r =? l label "LazyList")
			}
	}
	//					forAll { col :LazyList[Int] => val (l, r) = f(col); r ?= l lbl "LazyList" }

	def flatMappingProperty[A](f :Iterable[Iterable[Int]] => (A, A)) :Prop = {
		implicit val ints :Arbitrary[Int] = Arbitrary(Gen.choose(-100, 100))
		forAll { list :List[List[Int]] => val (l, r) = f(list); r =? l lbl "List" } &&
			forAll { seq :IndexedSeq[IndexedSeq[Int]] => val (l, r) = f(seq); r =? l lbl "IndexedSeq" } &&
			forAll { col :OrderedItems[OrderedItems[Int]] => val (l, r) = f(col); r =? l lbl "Iterable" } &&
			forAll { seq :RefArraySlice[RefArraySlice[Int]] => val (l, r) = f(seq); r =? l label "ArraySlice" } &&
			forAll { items :List[Int] =>
				var i = 0
				val list = { i += 1; items } #:: { i += 1; items.map(_ + 1) } #:: { i += 1; items.map(_ - 1) } #::
					LazyList.empty
				val (l, r) = f(list)
				(i ?= 0) :| "LazyList had " + i + " evaluated elements" && (r =? l lbl "LazyList")
//				(if (items.nonEmpty) i ?= 1
//				else i ?= 3
//				) :| "LazyList had " + i + " evaluated elements" && (r =? l lbl "LazyList")
			}
	}

	property("mapWith") = mappingProperty { list :Iterable[Int] =>
		list.mapWith(0) { (e, sum) => (e + sum, e + sum) }.toSeq -> list.toSeq.scanLeft(0)(_ + _).tail
	}
	property("flatMapWith") = flatMappingProperty { list :Iterable[Iterable[Int]] =>
		val flatMapped = list.flatMapWith(0) { (es, sum) =>
			val res = es.toSeq.scanLeft(sum)(_ + _)
			(res.tail, res.last)
		}
		flatMapped.toSeq -> list.flatten.scanLeft(0)(_ + _).tail.toSeq
	}

	property("mapWithIndex") = mappingProperty { list :Iterable[Int] =>
		list.mapWithIndex { (e, i) => (e, i) }.toSeq -> list.toSeq.zipWithIndex
	}
	property("flatMapWithIndex") = flatMappingProperty { list :Iterable[Iterable[Int]] =>
		list.flatMapWithIndex { (e, i) => e.map { (_, i) } }.toSeq ->
			list.toSeq.zipWithIndex.flatMap { case (e, i) => e.toSeq.map((_, i)) }
	}
	property("collectWithIndex") = mappingProperty { list :Iterable[Int] =>
		list.collectWithIndex { case (e, i) if e > 0 => e.toLong * i }.toSeq ->
			list.toSeq.zipWithIndex.collect { case (e, i) if e > 0 => e.toLong * i }
	}

	property("mapWhile") = mappingProperty { list :Iterable[Int] =>
		list.mapWhile(0)(_ <= Sum)((acc, e) => (e + acc, 2 * e + acc)) ->
			list.scanLeft((0, 0, 0)) {
				case (acc, e) => (acc._2, e + acc._2, 2 * e + acc._2)
			}.tail.takeWhile(_._1 <= Sum).map(_._3)
	}
	property("flatMapWhile") = flatMappingProperty { list :Iterable[Iterable[Int]] =>
		list.flatMapWhile(0)(_ <= Sum)((acc, es) => (acc + es.sum, es.map(_ + acc))) -> {
			val sums = list.map(_.sum).scanLeft(0)(_ + _).takeWhile(_ <= Sum)
			list.zip(sums).flatMap { case (es, acc) => es.map(_ + acc) }
		}
	}

	property("mapUntil") = mappingProperty { list :Iterable[Int] =>
		list.mapUntil(0)((acc, e) => (e + acc > Sum, e + acc, 2 * e + acc)) ->
			list.scanLeft((0, 0)) { case (acc, e) => (e + acc._1, 2 * e + acc._1) }.tail.takeWhile(_._1 <= Sum).map(_._2)
	}
	property("flatMapUntil") = flatMappingProperty { list :Iterable[Iterable[Int]] =>
		list.flatMapUntil(0)((acc, es) => (acc + es.sum > Sum, acc + es.sum, es.map(_ + acc))) -> {
			val sums = list.map(_.sum).scanLeft(0)(_ + _)
			list.zip(sums).map { case (es, acc) => es.map(_ + acc) }.zip(sums.tail).takeWhile(_._2 <= Sum).flatMap(_._1)
		}
	}

	property("mapSome") = mappingProperty { list :Iterable[Int] =>
		list.mapSome(0)((acc, e) => if (acc + e <= Sum) Some((acc + e, e * e)) else None) ->
			list.scanLeft((0, 0)) { case ((acc, _), e) => (acc + e, e * e) }.tail.takeWhile(_._1 <= Sum).map(_._2)
	}
	property("flatMapSome") = flatMappingProperty { (list :Iterable[Iterable[Int]]) =>
		list.flatMapSome(0)((acc, e) => (acc + e.sum, e) satisfying (_._1 <= Sum)) ->
			list.scanLeft((0, Nil :Iterable[Int])) {
				case ((acc, _), e) => (acc + e.sum, e)
			}.tail.takeWhile(_._1 <= Sum).flatMap(_._2)
	}

	property("mapReverse") = iterableProperty { seq :Iterable[Int] =>
		seq.mapReverse(s => s + s).toSeq ?= seq.map(s => s + s).to(LazyList).reverse
	}


	property("filterWith") = mappingProperty { list :Iterable[Int] =>
		list.filterWith(0) { (e, total) => (e > total, total + e) } ->
			list.zip(list.scanLeft(0)(_ + _)).collect { case (e, total) if e > total => e }
	}
	property("filterWithIndex") = mappingProperty { list :Iterable[Int] =>
		list.filterWithIndex { (e, i) => e % 2 == i % 2 } ->
			list.zipWithIndex.collect { case (e, i) if e % 2 == i % 2 => e }
	}
	property("partitionWith") = mappingProperty { list :Iterable[Int] =>
		val result = list.partitionWith(0) { (e, total) => (e > total, total + e) }
		val zipped = list.zip(list.scanLeft(0)(_ + _))
		val expectYes = zipped.collect { case (e, total) if e > total =>  e }
		val expectNo  = zipped.collect { case (e, total) if e <= total =>  e }
		result -> (expectYes, expectNo)
	}
	property("partitionWithIndex") = mappingProperty { list :Iterable[Int] =>
		val result = list.partitionWithIndex { (e, i) => e % 2 == i % 2 }
		val expectYes = list.zipWithIndex.collect { case (e, i) if e % 2 == i % 2 => e }
		val expectNo  = list.zipWithIndex.collect { case (e, i) if e % 2 != i % 2 => e }
		result -> (expectYes, expectNo)
	}


	//todo: tests for keep, unique, firstOccurrences, lastOccurrences
//	property("removed") = forAll { i :Int =>
//		mappingProperty { list :Iterable[Int] =>
//			list.removed(i) -> (list.take(i) ++ (if (i == Int.MaxValue) Nil else list.drop(i + 1)))
//		}
//	}
	property("removed") = forAll { i :Int =>
		iterableProperty { list :Iterable[Int] =>
			if (i < 0 || i >= list.size) {
				list.removed(i).throws[IndexOutOfBoundsException]
			} else
				list.removed(i) ?= (list.take(i) ++ (if (i == Int.MaxValue) Nil else list.drop(i + 1)))
		} && forAll { seq :Vector[Int] =>
			if (i < 0 || seq.isEmpty)
				LazyList.from(seq).removed(i).throws[IndexOutOfBoundsException]
			else if (i >= seq.size) {
				val list = LazyList.from(seq).removed(i) //validates it doesn't throw an exception here
				Vector.from(list).throws[IndexOutOfBoundsException]
			} else {
				var evaluated :Boolean = false
				val list = ({ evaluated = true; 42 } #:: LazyList.from(seq)).removed(i)
				Prop(!evaluated) :| "LazyList.removed evaluated before actual access" &&
					((list :Seq[Int]) ?= (42 +: seq).take(i) ++ seq.drop(i)) //i < seq.size, so i < Int.MaxValue
			}

		}
	}
	property("removed(from, until)") = forAll { (from :Int, until :Int) =>
		mappingProperty { list :Iterable[Int] =>
			list.removed(from, until) -> (if (until <= 0 | until <= from) list else list.take(from) ++ list.drop(until))
		}
	}


	property("takeUntil") = mappingProperty { items :Iterable[Int] =>
		items.takeUntil(0)(_ >= Sum)(_ + _) -> {
			var sum = 0
			var end = false
			items.takeWhile { x =>
				sum += x
				!end && (sum < Sum || { end = true; true })
			}
		}
	}
	property("dropUntil") = mappingProperty { items :Iterable[Int] =>
		items.dropUntil(0)(_ >= Sum)(_ + _) -> {
			var sum = 0
			var end = false
			items.dropWhile { x =>
				sum += x
				!end && (sum < Sum || { end = true; true })
			}
		}
	}
	property("takeWith") = mappingProperty { items :Iterable[Int] =>
		items.takeWith(0)(_ <= Sum)(_ + _) -> {
			var sum = 0; items.takeWhile { x => sum += x; sum <= Sum }
		}
	}
	property("dropWith") = mappingProperty { items :Iterable[Int] =>
		items.dropWith(0)(_ <= Sum)(_ + _) -> {
			var sum = 0; items.dropWhile { x => sum += x; sum <= Sum }
		}
	}

	//todo: test add

	//todo: test copyRangeToArray, cyclicCopyRangeToArray
}
