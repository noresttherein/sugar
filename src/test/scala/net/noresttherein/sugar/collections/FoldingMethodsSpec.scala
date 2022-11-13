package net.noresttherein.sugar.collections


import scala.collection.immutable.{ArraySeq, ListSet}

import net.noresttherein.sugar.extensions.{foldingMethods, satisfyingMethods}
import org.scalacheck.Prop._
import org.scalacheck.{Prop, Properties}





object FoldingMethodsSpec extends Properties("FoldingMethods") {
	val Sum = 100

	private def expectedFoldUntilResult(start :Int, numbers :Seq[Int]) = {
		numbers.scanLeft(start)(_ + _).span(_ < Sum) match {
			case (all, Seq()) => all.last
			case (_, rest) => rest.head
		}
	}
	private def expectedFoldUntilOptResult(start :Int, numbers :Seq[Int]) =
		numbers.scanLeft(start)(_ + _).dropWhile(_ < Sum).headOption

	private def expectedFoldUntilEitherResult(start :Int, numbers :Seq[Int]) = {
		numbers.scanLeft(start)(_ + _).span(_ < Sum) match {
			case (scanned, Seq()) => Left(scanned.last)
			case (_, rest) => Right(rest.head)
		}
	}

	property("foldUntil") = forAll { (start :Int, numbers :List[Int]) =>
		numbers.foldUntil(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilResult(start, numbers)
	}
	property("foldUntilOption") = forAll { (start :Int, numbers :List[Int]) =>
		numbers.foldUntilOption(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilOptResult(start, numbers)
	}
	property("foldUntilEither") = forAll { (start :Int, numbers :List[Int]) =>
		numbers.foldUntilEither(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilEitherResult(start, numbers)
	}

	property("foldLeftUntil") = forAll { (start :Int, numbers :List[Int]) =>
		numbers.foldLeftUntil(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilResult(start, numbers)
	}
	property("foldLeftUntilOption") = forAll { (start :Int, numbers :List[Int]) =>
		numbers.foldLeftUntilOption(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilOptResult(start, numbers)
	}
	property("foldLeftUntilEither") = forAll { (start :Int, numbers :List[Int]) =>
		numbers.foldLeftUntilEither(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilEitherResult(start, numbers)
	}

	property("foldRightUntil") = forAll { (start :Int, numbers :List[Int]) =>
			numbers.foldRightUntil(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilResult(start, numbers.reverse)
		} && forAll { (start :Int, numbers :ArraySeq[Int]) =>
			numbers.foldRightUntil(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilResult(start, numbers.reverse)
		} && forAll { (start :Int, numbers :ListSet[Int]) =>
			numbers.foldRightUntil(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilResult(start, numbers.toSeq.reverse)
	}
	property("foldRightUntilOption") = forAll { (start :Int, numbers :List[Int]) =>
			numbers.foldRightUntilOption(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilOptResult(start, numbers.reverse)
		} && forAll { (start :Int, numbers :ArraySeq[Int]) =>
			numbers.foldRightUntilOption(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilOptResult(start, numbers.reverse)
		} && forAll { (start :Int, numbers :ListSet[Int]) =>
			numbers.foldRightUntilOption(start)(_ >= Sum)(_ + _) ?=
				expectedFoldUntilOptResult(start, numbers.toList.reverse)
	}
	property("foldRightUntilEither") = forAll { (start :Int, numbers :List[Int]) =>
			numbers.foldRightUntilEither(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilEitherResult(start, numbers.reverse)
		} &&  forAll { (start :Int, numbers :ArraySeq[Int]) =>
			numbers.foldRightUntilEither(start)(_ >= Sum)(_ + _) ?= expectedFoldUntilEitherResult(start, numbers.reverse)
		} && forAll { (start :Int, numbers :ListSet[Int]) =>
			numbers.foldRightUntilEither(start)(_ >= Sum)(_ + _) ?=
				expectedFoldUntilEitherResult(start, numbers.toList.reverse)
	}



	private def expectedFoldWhileResult(start :Int, numbers :Seq[Int]) =
		numbers.scanLeft(start)(_ + _).takeWhile(_ < Sum).last
	private def expectedFoldWhileOptResult(start :Int, numbers :Seq[Int]) =
		numbers.scanLeft(start)(_ + _).takeWhile(_ < Sum).lastOption
	private def expectedFoldWhileEitherResult(start :Int, numbers :Seq[Int]) =
		if (start >= Sum) Left(start)
		else Right(expectedFoldWhileResult(start, numbers))

	property("foldWhile") = forAll { (start :Int, numbers :List[Int]) =>
		if (start > Sum)
			throws(classOf[IllegalArgumentException]) { numbers.foldWhile(start)(_ < Sum)(_ + _) } :| "empty"
		else
			numbers.foldWhile(start)(_ < Sum)(_ + _) ?= expectedFoldWhileResult(start, numbers)
	}
	property("foldWhileOpt") = forAll { (start :Int, numbers :List[Int]) =>
		numbers.foldWhileOpt(start)(_ < Sum)(_ + _) ?= expectedFoldWhileOptResult(start, numbers)
	}
	property("foldWhileEither") = forAll { (start :Int, numbers :List[Int]) =>
		numbers.foldWhileEither(start)(_ < Sum)(_ + _) ?= expectedFoldWhileEitherResult(start, numbers)
	}

	property("foldLeftWhile") = forAll { (start :Int, numbers :List[Int]) =>
		if (start > Sum)
			throws(classOf[IllegalArgumentException]) { numbers.foldLeftWhile(start)(_ < Sum)(_ + _) } :| "empty"
		else
			numbers.foldLeftWhile(start)(_ < Sum)(_ + _) ?= expectedFoldWhileResult(start, numbers)
	}
	property("foldLeftWhileOption") = forAll { (start :Int, numbers :List[Int]) =>
		numbers.foldLeftWhileOption(start)(_ < Sum)(_ + _) ?= expectedFoldWhileOptResult(start, numbers)
	}
	property("foldLeftWhileEither") = forAll { (start :Int, numbers :List[Int]) =>
		numbers.foldLeftWhileEither(start)(_ < Sum)(_ + _) ?= expectedFoldWhileEitherResult(start, numbers)
	}

	private def foldRightWhileProperty(start :Int, numbers :Iterable[Int]) = {
		if (start > Sum)
			throws(classOf[IllegalArgumentException]) {numbers.foldRightWhile(start)(_ < Sum)(_ + _)} :| "empty"
		else
			numbers.foldRightWhile(start)(_ < Sum)(_ + _) ?= expectedFoldWhileResult(start, numbers.toSeq.reverse)
	}
	property("foldRightWhile") = forAll { (start :Int, numbers :List[Int]) =>
			foldRightWhileProperty(start, numbers)
		} && forAll { (start :Int, numbers :ArraySeq[Int]) =>
			foldRightWhileProperty(start, numbers)
		} && forAll { (start :Int, numbers :ListSet[Int]) =>
		foldRightWhileProperty(start, numbers)
		}

	property("foldRightWhileOption") = forAll { (start :Int, numbers :List[Int]) =>
			numbers.foldRightWhileOption(start)(_ < Sum)(_ + _) ?= expectedFoldWhileOptResult(start, numbers.reverse)
		} && forAll { (start :Int, numbers :ArraySeq[Int]) =>
			numbers.foldRightWhileOption(start)(_ < Sum)(_ + _) ?= expectedFoldWhileOptResult(start, numbers.reverse)
		} && forAll { (start :Int, numbers :ListSet[Int]) =>
			numbers.foldRightWhileOption(start)(_ < Sum)(_ + _) ?= expectedFoldWhileOptResult(start, numbers.toList.reverse)
		}
	property("foldRightWhileEither") = forAll { (start :Int, numbers :List[Int]) =>
			numbers.foldRightWhileEither(start)(_ < Sum)(_ + _) ?= expectedFoldWhileEitherResult(start, numbers.reverse)
		} && forAll { (start :Int, numbers :ArraySeq[Int]) =>
			numbers.foldRightWhileEither(start)(_ < Sum)(_ + _) ?= expectedFoldWhileEitherResult(start, numbers.reverse)
		} && forAll { (start :Int, numbers :ListSet[Int]) =>
			numbers.foldRightWhileEither(start)(_ < Sum)(_ + _) ?=
				expectedFoldWhileEitherResult(start, numbers.toList.reverse)
	}


	private def expectedFoldSomeResult(start :Int, numbers :Seq[Int]) =
		numbers.scanLeft(start)(_ + _).tail.takeWhile(_ < Sum) match {
			case Seq() => start
			case less => less.last
		}

	property("partialFold") = forAll { (start :Int, numbers :List[Int]) =>
		numbers.partialFold(start) { case (sum, next) if sum + next < Sum => sum + next } ?=
			expectedFoldSomeResult(start, numbers)
	}
	property("foldSome") = forAll { (start :Int, numbers :List[Int]) =>
		numbers.foldSome(start)(_ + _ satisfying (_ < Sum )) ?= expectedFoldSomeResult(start, numbers)
	}

	property("partialFoldLeft") = forAll { (start :Int, numbers :List[Int]) =>
		numbers.partialFoldLeft(start) { case (sum, next) if sum + next < Sum => sum + next } ?=
			expectedFoldSomeResult(start, numbers)
	}
	property("foldLeftSome") = forAll { (start :Int, numbers :List[Int]) =>
		numbers.foldLeftSome(start)(_ + _ satisfying (_ < Sum )) ?= expectedFoldSomeResult(start, numbers)
	}

	property("partialFoldRight") = forAll { (start :Int, numbers :List[Int]) =>
			numbers.partialFoldRight(start) { case (sum, next) if sum + next < Sum => sum + next } ?=
				expectedFoldSomeResult(start, numbers.reverse)
		} && forAll { (start :Int, numbers :ArraySeq[Int]) =>
			numbers.partialFoldRight(start) { case (sum, next) if sum + next < Sum => sum + next } ?=
				expectedFoldSomeResult(start, numbers.reverse)
		} && forAll { (start :Int, numbers :ListSet[Int]) =>
			numbers.partialFoldRight(start) { case (sum, next) if sum + next < Sum => sum + next } ?=
				expectedFoldSomeResult(start, numbers.toList.reverse)
	}
	property("foldRightSome") = forAll { (start :Int, numbers :List[Int]) =>
			numbers.foldRightSome(start) { _ + _ satisfying (_ < Sum) } ?= expectedFoldSomeResult(start, numbers.reverse)
		} && forAll { (start :Int, numbers :ArraySeq[Int]) =>
			numbers.foldRightSome(start) {_ + _ satisfying (_ < Sum)} ?= expectedFoldSomeResult(start, numbers.reverse)
		} && forAll { (start :Int, numbers :ListSet[Int]) =>
			numbers.foldRightSome(start) { _ + _ satisfying (_ < Sum) } ?=
				expectedFoldSomeResult(start, numbers.toList.reverse)
	}


	private def reduceUntilProperty(numbers :Iterable[Int])(expr: => Long) =
		if (numbers.isEmpty)
			Prop(throws(classOf[UnsupportedOperationException])(expr))
		else
			numbers.scanLeft(0L)(_ + _).dropWhile(_ < Sum).headOption match {
				case Some(found) => expr ?= found
				case _ => Prop(throws(classOf[RuntimeException])(expr))
			}

	property("reduceUntil") = forAll { (numbers :List[Int]) =>
		reduceUntilProperty(numbers)(numbers.map(_.toLong).reduceUntil(_ >= Sum)(_ + _))
	}
	property("reduceUntilOption") = forAll { (numbers :List[Int]) =>
		numbers.reduceUntilOption(_ >= Sum)(_ + _) ?= numbers.scanLeft(0)(_ + _).dropWhile(_ < Sum).headOption
	}

	property("reduceLeftUntil") = forAll { (numbers :List[Int]) =>
		reduceUntilProperty(numbers)(numbers.map(_.toLong).reduceLeftUntil(_ >= Sum)(_ + _))
	}
	property("reduceLeftUntilOption") = forAll { (numbers :List[Int]) =>
		numbers.reduceLeftUntilOption(_ >= Sum)(_ + _) ?= numbers.scanLeft(0)(_ + _).dropWhile(_ < Sum).headOption
	}

	property("reduceRightUntil") = forAll { (numbers :List[Int]) =>
			reduceUntilProperty(numbers.reverse)(numbers.map(_.toLong).reduceRightUntil(_ >= Sum)(_ + _))
		} && forAll { (numbers :ArraySeq[Int]) =>
			reduceUntilProperty(numbers.reverse)(numbers.map(_.toLong).reduceRightUntil(_ >= Sum)(_ + _))
		} && forAll { (numbers :ListSet[Int]) =>
			reduceUntilProperty(numbers.toList.reverse)(numbers.map(_.toLong).reduceRightUntil(_ >= Sum)(_ + _))
		}
	property("reduceRightUntilOption") = forAll { (numbers :List[Int]) =>
			numbers.reduceRightUntilOption(_ >= Sum)(_ + _) ?=
				numbers.reverse.scanLeft(0)(_ + _).dropWhile(_ < Sum).headOption
		} && forAll { (numbers :ArraySeq[Int]) =>
			numbers.reduceRightUntilOption(_ >= Sum)(_ + _) ?=
				numbers.reverse.scanLeft(0)(_ + _).dropWhile(_ < Sum).headOption
		} && forAll { (numbers :ListSet[Int]) =>
			numbers.reduceRightUntilOption(_ >= Sum)(_ + _) ?=
				numbers.toList.reverse.scanLeft(0)(_ + _).dropWhile(_ < Sum).headOption
		}


	private def partialReduceSomeProperty(numbers :Seq[Int])(expr: => Int) =
		numbers.scanLeft(0)(_ + _).tail match {
			case Seq() => Prop(throws(classOf[UnsupportedOperationException])(expr))
			case Seq(first) => expr ?= first
			case Seq(first, second, _*) if second >= Sum => expr ?= first
			case Seq(_, rest @ _*) => expr ?= rest.takeWhile(_ < Sum).last
		}

	property("partialReduce") = forAll { (numbers :List[Int]) =>
		partialReduceSomeProperty(numbers)(
			numbers.partialReduce[Int] { case (sum :Int, e :Int) if sum + e < Sum => sum + e }
		)
	}
	property("reduceSome") = forAll { (numbers :List[Int]) =>
		partialReduceSomeProperty(numbers)(numbers.reduceSome(_ + _ satisfying (_ < Sum)))
	}

	property("partialReduceLeft") = forAll { (numbers :List[Int]) =>
		partialReduceSomeProperty(numbers)(
			numbers.partialReduceLeft[Int] { case (sum :Int, e :Int) if sum + e < Sum => sum + e }
		)
	}
	property("reduceLeftSome") = forAll { (numbers :List[Int]) =>
		partialReduceSomeProperty(numbers)(numbers.reduceLeftSome(_ + _ satisfying (_ < Sum)))
	}

	property("partialReduceRight") = forAll { (numbers :List[Int]) =>
			partialReduceSomeProperty(numbers.reverse)(
				numbers.partialReduceRight[Int] { case (e :Int, sum :Int) if sum + e < Sum => sum + e }
			)
		} && forAll { (numbers :ArraySeq[Int]) =>
			partialReduceSomeProperty(numbers.reverse)(
				numbers.partialReduceRight[Int] { case (e :Int, sum :Int) if sum + e < Sum => sum + e }
			)
		} && forAll { (numbers :ListSet[Int]) =>
			partialReduceSomeProperty(numbers.toList.reverse)(
				numbers.partialReduceRight[Int] { case (e :Int, sum :Int) if sum + e < Sum => sum + e }
			)
		}
	property("reduceRightSome") = forAll { (numbers :List[Int]) =>
			partialReduceSomeProperty(numbers.reverse)(numbers.reduceRightSome(_ + _ satisfying (_ < Sum)))
		} && forAll { (numbers :ArraySeq[Int]) =>
			partialReduceSomeProperty(numbers.reverse)(numbers.reduceRightSome(_ + _ satisfying (_ < Sum)))
		} && forAll { (numbers :ListSet[Int]) =>
			partialReduceSomeProperty(numbers.toList.reverse)(numbers.reduceRightSome(_ + _ satisfying (_ < Sum)))
		}

}
