package net.noresttherein.sugar.collections

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

import org.scalacheck.{Prop, Properties, Test}
import org.scalacheck.Prop._
import org.scalacheck.util.ConsoleReporter

import net.noresttherein.sugar.collections.IterableExtensionSpec.{mappingProperty, zipMapProperty}
import net.noresttherein.sugar.extensions.{IterableOnceExtension, satisfyingMethods}





object IterableOnceExtensionSpec extends Properties("IterableOnceExtension") {

	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140)).withMinSuccessfulTests(1000)

	val Sum = 100

	private def foldingProperty(prop :(Int, Iterable[Int]) => Prop) :Prop =
		forAll { (start :Int, numbers :List[Int]) => prop(start, numbers) } &&
			forAll { (start :Int, numbers :IndexedSeq[Int]) => prop(start, numbers) } &&
			forAll { (start :Int, numbers :OrderedItems[Int]) => prop(start, numbers to OrderedItems) }


	private def expectedFoldUntilResult(start :Int, numbers :Iterable[Int]) = {
		numbers.toSeq.scanLeft(start)(_ + _).span(_ < Sum) match {
			case (all, empty) if empty.isEmpty => all.last
			case (_, rest) => rest.head
		}
	}
	private def expectedFoldUntilOptResult(start :Int, numbers :Iterable[Int]) =
		numbers.toSeq.scanLeft(start)(_ + _).dropWhile(_ < Sum).headOption

	private def expectedFoldUntilEitherResult(start :Int, numbers :Iterable[Int]) = {
		numbers.toSeq.scanLeft(start)(_ + _).span(_ < Sum) match {
			case (scanned, empty) if empty.isEmpty => Left(scanned.last)
			case (_, rest) => Right(rest.head)
		}
	}

	property("foldWithIndex") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.foldWithIndex(start)((a :Int, b :Int, i :Int) => (a + b) * i) ?=
			numbers.zip(numbers.toSeq.indices).fold((start, 0)) { (acc, pair) => ((acc._1 + pair._1) * pair._2, 0) }._1
	}
	property("foldLeftWithIndex") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.foldLeftWithIndex(start)((a :Int, b :Int, i :Int) => (a + b) * i) ?=
			numbers.zip(numbers.toSeq.indices).foldLeft(start) { (acc, pair) => (acc + pair._1) * pair._2 }
	}

	property("foldUntil") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.foldUntil(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilResult(start, numbers)
	}
	property("foldUntilOption") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.foldUntilOption(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilOptResult(start, numbers)
	}
	property("foldUntilEither") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.foldUntilEither(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilEitherResult(start, numbers)
	}

	property("foldLeftUntil") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.foldLeftUntil(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilResult(start, numbers)
	}
	property("foldLeftUntilOption") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.foldLeftUntilOption(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilOptResult(start, numbers)
	}
	property("foldLeftUntilEither") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.foldLeftUntilEither(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilEitherResult(start, numbers)
	}

	property("foldRightUntil") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.foldRightUntil(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilResult(start, numbers.toSeq.reverse)
	}
	property("foldRightUntilOption") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.foldRightUntilOption(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilOptResult(start, numbers.toSeq.reverse)
	}
	property("foldRightUntilEither") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.foldRightUntilEither(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilEitherResult(start, numbers.toSeq.reverse)
	}



	private def expectedFoldWhileResult(start :Int, numbers :Iterable[Int]) =
		numbers.toSeq.scanLeft(start)(_ + _).takeWhile(_ < Sum).last
	private def expectedFoldWhileOptResult(start :Int, numbers :Iterable[Int]) =
		numbers.toSeq.scanLeft(start)(_ + _).takeWhile(_ < Sum).lastOption
	private def expectedFoldWhileEitherResult(start :Int, numbers :Iterable[Int]) =
		if (start >= Sum) Left(start)
		else Right(expectedFoldWhileResult(start, numbers))


	property("foldWhile") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		if (start > Sum)
			throws(classOf[IllegalArgumentException]) { numbers.foldWhile(start)(_ < Sum)(_ + _) } :| "empty"
		else
			numbers.foldWhile(start)(_ < Sum)(_ + _) ?= expectedFoldWhileResult(start, numbers)
	}
	property("foldWhileOption") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.foldWhileOption(start)(_ < Sum)(_ + _) ?= expectedFoldWhileOptResult(start, numbers)
	}
	property("foldWhileEither") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.foldWhileEither(start)(_ < Sum)(_ + _) ?= expectedFoldWhileEitherResult(start, numbers)
	}

	property("foldLeftWhile") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		if (start > Sum)
			throws(classOf[IllegalArgumentException]) { numbers.foldLeftWhile(start)(_ < Sum)(_ + _) } :| "empty"
		else
			numbers.foldLeftWhile(start)(_ < Sum)(_ + _) ?= expectedFoldWhileResult(start, numbers)
	}
	property("foldLeftWhileOption") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.foldLeftWhileOption(start)(_ < Sum)(_ + _) ?= expectedFoldWhileOptResult(start, numbers)
	}
	property("foldLeftWhileEither") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.foldLeftWhileEither(start)(_ < Sum)(_ + _) ?= expectedFoldWhileEitherResult(start, numbers)
	}

	property("foldRightWhile") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		if (start > Sum)
			throws(classOf[IllegalArgumentException]) { numbers.foldRightWhile(start)(_ < Sum)(_ + _) } :| "empty"
		else
			numbers.foldRightWhile(start)(_ < Sum)(_ + _) ?= expectedFoldWhileResult(start, numbers.toSeq.reverse)
	}
	property("foldRightWhileOption") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.foldRightWhileOption(start)(_ < Sum)(_ + _) ?= expectedFoldWhileOptResult(start, numbers.toSeq.reverse)
	}
	property("foldRightWhileEither") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.foldRightWhileEither(start)(_ < Sum)(_ + _) ?= expectedFoldWhileEitherResult(start, numbers.toSeq.reverse)
	}


	private def expectedFoldSomeResult(start :Int, numbers :Iterable[Int]) =
		numbers.toSeq.scanLeft(start)(_ + _).tail.takeWhile(_ < Sum) match {
			case Seq() => start
			case less => less.last
		}

	property("partialFold") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.partialFold(start) { case (sum, next) if sum + next < Sum => sum + next } ?=
			expectedFoldSomeResult(start, numbers)
	}
	property("partialFoldLeft") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.partialFoldLeft(start) { case (sum, next) if sum + next < Sum => sum + next } ?=
			expectedFoldSomeResult(start, numbers)
	}
	property("partialFoldRight") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.partialFoldRight(start) { case (sum, next) if sum + next < Sum => sum + next } ?=
			expectedFoldSomeResult(start, numbers.toSeq.reverse)
	}

	property("foldSome") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.foldSome(start)(_ + _ satisfying (_ < Sum )) ?= expectedFoldSomeResult(start, numbers)
	}
	property("foldLeftSome") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.foldLeftSome(start)(_ + _ satisfying (_ < Sum )) ?= expectedFoldSomeResult(start, numbers)
	}
	property("foldRightSome") = foldingProperty { (start :Int, numbers :Iterable[Int]) =>
		numbers.foldRightSome(start) { _ + _ satisfying (_ < Sum) } ?= expectedFoldSomeResult(start, numbers.toSeq.reverse)
	}

	private def reducingProperty(prop :Iterable[Int] => Prop) :Prop =
		forAll { items :List[Int] => prop(items) } &&
			forAll { items :ArraySeq[Int] => prop(items) } &&
			forAll { items :OrderedItems[Int] => prop(items) }

	private def reduceUntilProperty(numbers :Iterable[Int])(expr: => Long) =
		if (numbers.isEmpty)
			Prop(throws(classOf[UnsupportedOperationException])(expr)) :| "empty should throw"
		else
			numbers.scanLeft(0L)(_ + _).span(_ < Sum) match {
				case (_, fail) if fail.nonEmpty => expr ?= fail.head
				case (pass, _) => expr ?= pass.last
			}

	property("reduceUntil") = reducingProperty { (numbers :Iterable[Int]) =>
		reduceUntilProperty(numbers)(numbers.map(_.toLong).reduceUntil(_ >= Sum)(_ + _))
	}
	property("reduceUntilOption") = reducingProperty { (numbers :Iterable[Int]) =>
		numbers.reduceUntilOption(_ >= Sum)(_ + _) ?= numbers.scanLeft(0)(_ + _).dropWhile(_ < Sum).headOption
	}

	property("reduceLeftUntil") = reducingProperty { (numbers :Iterable[Int]) =>
		reduceUntilProperty(numbers)(numbers.map(_.toLong).reduceLeftUntil(_ >= Sum)(_ + _))
	}
	property("reduceLeftUntilOption") = reducingProperty { (numbers :Iterable[Int]) =>
		numbers.reduceLeftUntilOption(_ >= Sum)(_ + _) ?= numbers.scanLeft(0)(_ + _).dropWhile(_ < Sum).headOption
	}

	property("reduceRightUntil") = reducingProperty { (numbers :Iterable[Int]) =>
		reduceUntilProperty(numbers.toSeq.reverse)(numbers.map(_.toLong).reduceRightUntil(_ >= Sum)(_ + _))
	}
	property("reduceRightUntilOption") = reducingProperty { (numbers :Iterable[Int]) =>
		numbers.reduceRightUntilOption(_ >= Sum)(_ + _) ?=
			numbers.toSeq.reverse.scanLeft(0)(_ + _).dropWhile(_ < Sum).headOption
	}


	private def partialReduceSomeProperty(numbers :Iterable[Int])(expr: => Int) =
		numbers.toSeq.scanLeft(0)(_ + _).tail match {
			case Seq() => Prop(throws(classOf[UnsupportedOperationException])(expr))
			case Seq(first) => expr ?= first
			case Seq(first, second, _*) if second >= Sum => expr ?= first
			case Seq(_, rest @ _*) => expr ?= rest.takeWhile(_ < Sum).last
		}

	property("partialReduce") = reducingProperty { (numbers :Iterable[Int]) =>
		partialReduceSomeProperty(numbers)(
			numbers.partialReduce[Int] { case (sum :Int, e :Int) if sum + e < Sum => sum + e }
		)
	}
	property("partialReduceLeft") = reducingProperty { (numbers :Iterable[Int]) =>
		partialReduceSomeProperty(numbers)(
			numbers.partialReduceLeft[Int] { case (sum :Int, e :Int) if sum + e < Sum => sum + e }
		)
	}
	property("partialReduceRight") = reducingProperty { (numbers :Iterable[Int]) =>
		partialReduceSomeProperty(numbers.toSeq.reverse)(
			numbers.partialReduceRight[Int] { case (e :Int, sum :Int) if sum + e < Sum => sum + e }
		)
	}

	property("reduceSome") = reducingProperty { (numbers :Iterable[Int]) =>
		partialReduceSomeProperty(numbers)(numbers.reduceSome(_ + _ satisfying (_ < Sum)))
	}
	property("reduceLeftSome") = reducingProperty { (numbers :Iterable[Int]) =>
		partialReduceSomeProperty(numbers)(numbers.reduceLeftSome(_ + _ satisfying (_ < Sum)))
	}
	property("reduceRightSome") = reducingProperty { (numbers :Iterable[Int]) =>
		partialReduceSomeProperty(numbers.toSeq.reverse)(numbers.reduceRightSome(_ + _ satisfying (_ < Sum)))
	}



	property("foreachWithIndex") = mappingProperty { list :Iterable[Int] =>
		val res = new ArrayBuffer[Int]
		list.foreachWithIndex { (e, i) => if (i % 2 == 0) res += e else res += -e } ?=
			list.toSeq.zipWithIndex.map { case (e, i) => if (i % 2 == 0) e else -e }
	}
	property("forallWithIndex") = mappingProperty { list :Iterable[Int] =>
		list.forallWithIndex { (e, i) => e % 2 == 0 || i % 2 == 0 } ?=
			list.toSeq.zipWithIndex.forall { case (e, i) => e % 2 == 0 || i % 2 == 0 }
	}

	property("zipForeach") = zipMapProperty { (l :Iterable[Int], r :Iterable[Int]) =>
		var sum = 0
		l.zipForeach(r) { (a, b) => sum += a * b }
		sum ?= l.zip(r).foldLeft(0) { (sum, pair) => sum + pair._1 * pair._2 }
	}

	property("zipForall") = zipMapProperty { (l :Iterable[Int], r :Iterable[Int]) =>
		l.zipForall(r) { (a, b) => (a % 2 == 0) == (b % 2 == 0) } ?=
			l.zip(r).forall { case (a, b) => (a % 2 == 0) == (b % 2 == 0) }
	}

}
