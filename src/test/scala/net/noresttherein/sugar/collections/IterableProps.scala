package net.noresttherein.sugar.collections

import scala.collection.{mutable, Factory, IterableFactory, Stepper}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.Builder
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.reflect.ClassTag
import scala.util.{Success, Try}

import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Test}
import org.scalacheck.Prop.{all, forAll, AnyOperators}
import org.scalacheck.commands.Commands
import org.scalacheck.util.{Buildable, ConsoleReporter}

import net.noresttherein.sugar.numeric
import net.noresttherein.sugar.collections.IterableProps.{filter, flatMap, fold, foldLeft, foldRight, foldZero, map, value, Dummy, Filter, FlatMap, Fold, FoldSide, Map}
import net.noresttherein.sugar.extensions.{castTypeParam, classNameMethods, factoryExtension, intObjectExtension, iterableExtension}
import net.noresttherein.sugar.testing.scalacheck.extensions.{BooleanExtension, LazyExtension, PropExtension}




//This class is flawed because it relies on a IterableFactory, and C may have only an EvidenceIterableFactory or other.
//We can't use it to test SortedSet, for example
abstract class GenericIterableProps[C[T] <: S[T], S[T] <: Iterable[T], E[_]](name :String) extends Properties(name) {
	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(3, 140)).withMinSuccessfulTests(500)

	protected def typeS :String = S[Int].source.localClassName

/*
	property("knownSize")         = Prop(!knowsSize) || forAll { col :C[Int] => col.knownSize =? col.size }
	property("size")              = test { (expect :S[Int], result :C[Int]) => expect.size =? result.size }
	property("compareSize")       = test { (expect :S[Int], result :C[Int]) =>
	                                    forAll {
		                                    i :Int => expect.sizeCompare(i).sign =? expect.to(C).sizeCompare(i).sign
	                                    }
	                                }
	property("isEmpty")           = Prop(C[Int].fromSpecific(Nil).isEmpty) :| "empty is empty" && compare((_ :S[Int]).isEmpty)
	property("iterator")          = test { (expect :S[Int], result :C[Int]) => checkIterator(expect, result.iterator) }
	property("stepper")           = test { (expect :S[Int], result :C[Int]) => checkStepper(expect, result.stepper) }
	property("empty")             = forAll { (col :C[Int]) => compare(S[Int].fromSpecific(Nil), col.empty) }
	property("filter")            = test((_ :S[Int]).filter(filter))
	property("filterNot")         = test((_ :S[Int]).filterNot(filter))
	property("partition")         = test { (expect :S[Int], result :C[Int]) =>
	                                    val (expect_1, expect_2) = expect.partition(filter)
	                                    val (result_1, result_2) = result.partition(filter)
	                                	("_1" |: validate(expect_1.to(S), result_1.to(C))) &&
			                                ("_2" |: validate(expect_2.to(S), result_2.to(C)))
	                                }
	property("map")               = test((_ :S[Int]).map(map))
	property("flatMap(Seq)")      = test((_ :S[Int]).flatMap(flatMap))
	property(s"flatMap($name)")   = test((_ :S[Int]).flatMap(i => C[Int].fromSpecific(flatMap(i))))
	property(s"flatten($typeS)")  = test((_ :S[C[Int]]).flatten) //todo: this is very slow
	property("collect")           = test((_ :S[Int]).collect { case i if filter(i) => i })
	property("foreach")           = test { (expect :S[Int], result :C[Int]) =>
	                                    var sum1 = 0; expect.foreach { sum1 += _ }
							      	    var sum2 = 0; result.foreach { sum2 += _ }
							      	    sum1 =? sum2
	                                }
	property("forall")            = compare((_ :S[Int]).forall(filter))
	property("exists")            = compare((_ :S[Int]).exists(filter))
	property("count")             = compare((_ :S[Int]).count(filter))
	property("fold")              = compare((_ :S[Int]).fold(value[Int])(fold))
	property("foldLeft")          = compare((_ :S[Int]).foldLeft(foldZero[Long, Int])(foldLeft))
	property("foldRight")         = compare((_ :S[Int]).foldRight(foldZero[Long, Int])(foldRight))
	property("reduce")            = test { (expect :S[Int], result :C[Int]) =>
	                                    if (expect.isEmpty)
		                                    result.reduce(fold[Int]).throws[UnsupportedOperationException]
	                                    else
		                                    expect.reduce(fold[Int]) =? result.reduce(fold[Int])
	                                }
	property("reduceLeft")        = test { (expect :S[Int], result :C[Int]) =>
		                                if (expect.isEmpty)
			                                result.reduceLeft(fold[Int]).throws[UnsupportedOperationException]
		                                else
			                                expect.reduceLeft(fold[Int]) =? result.reduceLeft(fold[Int])
	                                }
	property("reduceRight")       = test { (expect :S[Int], result :C[Int]) =>
		                                if (expect.isEmpty)
			                                result.reduceRight(fold[Int]).throws[UnsupportedOperationException]
	                                    else
			                                expect.reduceRight(fold[Int]) =? result.reduceRight(fold[Int])
	                                }
	property("reduceLeftOption")  = test { (expect :S[Int], result :C[Int]) =>
	                                    expect.reduceLeftOption(fold[Int]) =? result.reduceLeftOption(fold[Int])
	                                }
	property("reduceRightOption") = test { (expect :S[Int], result :C[Int]) =>
	                                    expect.reduceRightOption(fold[Int]) =? result.reduceRightOption(fold[Int])
	                                }
	property("copyToArray")       = test { (expect :S[Int], result :C[Int]) =>
	                                    if (hasOrder)
		                                    forAll { (i :Int, j :Int, len :Short) =>
			                                    val expectArray = new Array[Int](len.toInt.abs)
			                                    val resultArray = new Array[Int](len.toInt.abs)
			                                    if (i < 0)
				                                    result.copyToArray(resultArray, i, j).throws[IndexOutOfBoundsException] orElse
					                                    result.copyToArray(resultArray, i, j) =? 0 lbl "throws or copies zero"
			                                    else {
				                                    expect.copyToArray(expectArray, i, j) =?
				                                        result.copyToArray(resultArray, i, j) &&
					                                    ArraySeq.unsafeWrapArray(expectArray) =?
						                                    ArraySeq.unsafeWrapArray(resultArray)
			                                    }
		                                    }
	                                    else {
		                                    val resultArray = new Array[Int](expect.size)
		                                    expect.size =? result.copyToArray(resultArray) &&
			                                    compare(expect, ArraySeq.unsafeWrapArray(resultArray))
	                                    }
	                                }
	property("++")                = test { (expect :S[Int], result :C[Int]) =>
	                                    forAll { list :List[Int] => validate(expect ++ list to S, result ++ list to C) }
	                                }
*/


//	private val property = new PropertySpecifier
//
//	class PropertySpecifier() {
//		def update(propName :String, p : => Prop) =
//			(GenericIterableProps.this :Properties).property(propName) = p
//	}


	protected def test[T :Arbitrary :E](check :(S[T], C[T]) => Prop) :Prop =
		forAll { elems :S[T] => check(elems, elems to C) }

	protected def test[T, X, F, M, FM]
	                  (f :S[T] => Iterable[X])
	                  (implicit input :Arbitrary[T], output :Arbitrary[X], ev1 :E[T], ev2 :E[X], tag :ClassTag[X],
	                            filt :Filter[X], fldA :FoldSide[F, X], evf :E[F], fld :Fold[X],
	                            mp :Map[X, M], evm :E[M], fmap :FlatMap[X, FM], evfm :E[FM]) :Prop =
		try
			forAll { elems :S[T] => validate(f(elems) to S, f(elems to C) to C) }
		catch {
			case e :Exception =>
				System.err.println(e)
				e.printStackTrace(System.err)
				throw e
		}

	protected def compare[T :Arbitrary :E, X](f :S[T] => X) :Prop =
		forAll { elems :S[T] => f(elems) =? f(elems to C[T]) }

	protected def compare[T :E](expect :Iterable[T], result :Iterable[T]) :Prop =
		(if (hasOrder)
			Prop(expect.iterator sameElements result)
		else
			expect =? result.to(S)
		) lbl "Expected: " + expect + ";\n     got: " + result

	protected def validate[T, F, M, FM](label: => String, expect :S[T], result :S[T])
	                                   (implicit arbitrary :Arbitrary[T], ev :E[T], tag :ClassTag[T], filt :Filter[T],
	                                    fldA :FoldSide[F, T], evf :E[F], fld :Fold[T], mp :Map[T, M], evm :E[M],
	                                    fmap :FlatMap[T, FM], evfm :E[FM]) :Prop =
		try
			label @: (compare(expect, result) && props(expect, result))
		catch {
			case e :Exception =>
				System.err.println(e)
				e.printStackTrace(System.err)
				throw e
		}

	protected def validate[T, F, M, FM](expect :S[T], result :S[T])
	                                   (implicit arbitrary :Arbitrary[T], ev :E[T], tag :ClassTag[T], filt :Filter[T],
	                                    fldA :FoldSide[F, T], evf :E[F], fld :Fold[T], mp :Map[T, M], evm :E[M],
	                                    fmap :FlatMap[T, FM], evfm :E[FM]) :Prop =

		validate(s"Input:   $expect;\ntesting: $result :${result.localClassName}", expect, result)

	protected def props[T, F, M, FM](expect :S[T], result :S[T])
	                                (implicit arbitrary :Arbitrary[T], ev :E[T], tag :ClassTag[T], filt :Filter[T],
	                                 fldA :FoldSide[F, T], evf :E[F], fld :Fold[T], mp :Map[T, M], evm :E[M],
	                                 fmap :FlatMap[T, FM], evfm :E[FM]) :Prop =
		all(
			"knownSize"         lbl_: !knowsSize || expect.size =? result.knownSize,
			"size"              lbl_: expect.size =? result.size,
			"isEmpty"           lbl_: expect.isEmpty =? result.isEmpty,
			"sizeCompare"       lbl_: forAll { i :Int => expect.sizeCompare(i).sign =? result.sizeCompare(i).sign },
			"iterator"          lbl_: checkIterator(expect, result.iterator),
			"stepper"           lbl_: checkStepper(expect, result.stepper),
			"empty"             lbl_: compare(expect.empty, result.empty),
			"filter"            lbl_: compare(expect.filter(filter), result.filter(filter)),
			"filterNot"         lbl_: compare(expect.filterNot(filter), result.filterNot(filter)),
			"partition"         lbl_: {
			                          	val (expect_1, expect_2) = expect.partition(filter)
			                          	val (result_1, result_2) = result.partition(filter)
			                          	compare(expect_1, result_1) && compare(expect_2, result_2)
			                          },
			"map"               lbl_: compare(expect.map(map), result.map(map)),
			"flatMap"           lbl_: compare(expect.flatMap(flatMap), result.flatMap(flatMap)),
			"flatten"           lbl_: compare(expect.map(flatMap).flatten, result.map(flatMap).flatten),
			"collect"           lbl_: compare(expect.collect { case a if filter[T].apply(a) => a },
			                             result.collect { case a if filter[T].apply(a) => a }),
			"foreach"           lbl_: {
			                          	val b = S[T].newBuilder
			                          	result.foreach(b += _)
			                          	expect =? b.result()
			                          },
//			"tapEach"
			"forall"            lbl_: expect.forall(filter) =? result.forall(filter),
			"exists"            lbl_: expect.exists(filter) =? result.exists(filter),
			"count"             lbl_: expect.count(filter) =? result.count(filter),
			"fold"              lbl_: expect.fold(value[T])(fold) =? result.fold(value[T])(fold),
			"foldLeft"          lbl_: expect.foldLeft(foldZero[F, T])(foldLeft) =?
			                          result.foldLeft(foldZero[F, T])(foldLeft),
			"foldRight"         lbl_: expect.foldRight(foldZero[F, T])(foldRight) =?
			                          result.foldRight(foldZero[F, T])(foldRight),
			"reduce"            lbl_: (if (expect.isEmpty) result.reduce(fold[T]).throws[UnsupportedOperationException]
			                          else expect.reduce(fold[T]) =? result.reduce(fold[T])),
			"reduceLeft"        lbl_: (if (expect.isEmpty) result.reduceLeft(fold[T]).throws[UnsupportedOperationException]
			                          else expect.reduceLeft(fold[T]) =? result.reduceLeft(fold[T])),
			"reduceRight"       lbl_: (if (expect.isEmpty) result.reduceRight(fold[T]).throws[UnsupportedOperationException]
			                          else expect.reduceRight(fold[T]) =? result.reduceRight(fold[T])),
			"reduceOption"      lbl_: expect.reduceOption(fold[T]) =? result.reduceOption(fold[T]),
			"reduceLeftOption"  lbl_: expect.reduceLeftOption(fold[T]) =? result.reduceLeftOption(fold[T]),
			"reduceRightOption" lbl_: expect.reduceRightOption(fold[T]) =? result.reduceRightOption(fold[T]),
			"copyToArray"       lbl_: (if (hasOrder)
				                        forAll { (i :Int, n :Int, max :Short) =>
					                        val expectErasedArr = new Array[AnyRef](max.toInt.abs).asInstanceOf[Array[T]]
					                        val resultErasedArr = new Array[AnyRef](max.toInt.abs).asInstanceOf[Array[T]]
					                        val expectSpecificArr = new Array[T](max.toInt.abs)
					                        val resultSpecificArr = new Array[T](max.toInt.abs)
											val copied = n min (max.toInt.abs - (i max 0)) max 0 min expect.size
											if (i < 0) {
												(result.copyToArray(resultErasedArr, i, n).throws[IndexOutOfBoundsException]
													orElse result.copyToArray(resultErasedArr, i, n) =? 0) &&
												(result.copyToArray(resultSpecificArr, i, n).throws[IndexOutOfBoundsException]
												    orElse result.copyToArray(resultSpecificArr, i, n) =? 0) lbl "throws or 0"
											} else {
						                         expect.copyToArray(expectSpecificArr, i, n)
						                         expect.copyToArray(expectErasedArr, i, n)
					                             copied =? result.copyToArray(resultErasedArr, i, n) &&
					                             copied =? result.copyToArray(resultSpecificArr, i, n) && {
				                                     val expectErased = ArraySeq.unsafeWrapArray(
					                                     expectErasedArr.slice(i, i + copied)
				                                     ).to(S)
						                             val resultErased = ArraySeq.unsafeWrapArray(
							                             resultErasedArr.slice(i, i + copied)
						                             ).to(S)
						                             val expectSpecific = ArraySeq.unsafeWrapArray(
							                             expectSpecificArr.slice(i, i + copied)
						                             ).to(S)
						                             val resultSpecific = ArraySeq.unsafeWrapArray(
							                             resultSpecificArr.slice(i, i + copied)
						                             ).to(S)
						                             (expectErased =? resultErased lbl "erased") &&
							                             (expectSpecific =? resultSpecific lbl "specific")
					                             }
											}
					                    }
			                          else {
			                            val erasedArr   = new Array[AnyRef](expect.size).asInstanceOf[Array[T]]
			                            val specificArr = new Array[T](expect.size)
			                            expect.size =? result.copyToArray(erasedArr) &&
				                            expect.size =? result.copyToArray(specificArr) && {
				                                compare(expect, ArraySeq.unsafeWrapArray(erasedArr)) &&
				                                compare(expect, ArraySeq.unsafeWrapArray(specificArr))
			                                }
			                          }),
//			"grouped",
//			"sliding",
//          "inits",
//          "tails",
//			"groupBy",
//			"groupMap",
//			"groupMapReduce",
//			"partitionMap"

			"++"                lbl_: forAll { items :List[T] => compare(expect ++ items, result ++ items) }
		)


	protected def checkIterator[T :E](expect :S[T], iterator: => Iterator[T], prefix :String = "iterator") :Prop =
		expect =? iterator.to(S[T])

	protected def checkStepper[T :E](expect :S[T], stepper: => Stepper[T], prefix :String = "stepper") :Prop = {
		val s = stepper
		val b = S[T].newBuilder
		while (s.hasStep)
			b += s.nextStep()
		expect =? b.result() &&
			(prefix + ".iterator" lbl_: checkIterator(expect, stepper.iterator, prefix + ".iterator")) &&
			(prefix + ".javaIterator" lbl_: expect =? (stepper.javaIterator.castParam[T].asScala to S)) &&
			(prefix + ".spliterator.forEachRemaining" lbl_: {
				val b = S[T].newBuilder
				stepper.spliterator.castParam[T].forEachRemaining(b += (_:T))
				expect =? b.result()
			}) &&
			(prefix + ".spliterator.tryAdvance" lbl_: {
				val b = S[T].newBuilder
				val s = stepper.spliterator.castParam[T]
				while (s.tryAdvance(b += (_ :T)))
					{}
				expect =? b.result()
			}) &&
			(prefix + ".spliterator.trySplit" lbl_: {
				val s1 = stepper.spliterator.castParam[T]
				val s2 = s1.trySplit.castParam[T]
				val buf = S[T].newBuilder
				if (s2 != null)
					while (s2.tryAdvance(buf += (_ :T)))
						{}
				while (s1.tryAdvance(buf += (_ :T)))
					{}
				buf.result() ?= expect.castParam[T]
			})
	}

	protected def knowsSize = false
	protected def hasOrder  = false


	implicit def buildableChecked[T :E] :Buildable[T, C[T]] = new Buildable[T, C[T]] {
		override def builder :Builder[T, C[T]] = C[T].newBuilder
	}
	implicit def buildableReference[T :E] :Buildable[T, S[T]] = new Buildable[T, S[T]] {
		override def builder :Builder[T, S[T]] = S[T].newBuilder
	}

	protected def S[T :E] :Factory[T, S[T]] //= referenceFactory[T]
	protected def C[T :E] :Factory[T, C[T]] //= checkedFactory[T]

	implicit def intEvidence    :E[Int]
	implicit def longEvidence   :E[Long]
	implicit def stringEvidence :E[String]
	implicit def intSEvidence   :E[S[Int]]
	implicit def intCEvidence   :E[C[Int]]
	implicit def pairEvidence[A :E, B :E] :E[(A, B)]

	implicit val intFilter      :Filter[Int]             =
		new Filter[Int]((i :Int) => i % 10 >= 7)
	implicit val intMap         :Map[Int, Long]          =
		new Map[Int, Long](i => i.toLong << 32 | i & 0xffffffffL)
	implicit val intFlatMap     :FlatMap[Int, Int]       =
		new FlatMap[Int, Int]((i :Int) => Seq.iterate(i, 10)(_ + i))
	implicit val intFold        :Fold[Int]               =
		new Fold[Int](Int.MinValue, (i :Int, j :Int) => i max j)
	implicit val intFoldSide    :FoldSide[Long, Int]     =
		new FoldSide[Long, Int](0L, (acc :Long, i :Int) => acc + i)
	implicit val longFilter     :Filter[Long]            =
		new Filter[Long]((i :Long) => i % 10 >= 7)
	implicit val longMap        :Map[Long, String]       =
		new Map[Long, String](String.valueOf(_:Long))
	implicit val longFlatMap    :FlatMap[Long, Long]     =
		new FlatMap[Long, Long]((i :Long) => Seq.iterate(i, 10)(_ + i))
	implicit val longFold       :Fold[Long]              =
		new Fold[Long](Long.MinValue, (i :Long, j :Long) => i max j)
	implicit val longFoldSide   :FoldSide[Long, Long]    =
		new FoldSide[Long, Long](0L, (acc :Long, i :Long) => acc + i)
	implicit val stringFilter   :Filter[String]          =
		new Filter[String](_.toIntOption.exists(_ % 2 == 0))
	implicit val stringMap      :Map[String, String]     =
		new Map[String, String](_ + ":)")
	implicit val stringFlatMap  :FlatMap[String, String] =
		new FlatMap[String, String](_.split(""))
	implicit val stringFold     :Fold[String]            =
		new Fold[String]("", _ + _)
	implicit val stringFoldSide :FoldSide[Int, String]   =
		new FoldSide[Int, String](0, _ + _.length)

	implicit val byteFilter   :Filter[Byte] = new Filter[Byte]((i :Byte) => i % 10 >= 7)
	implicit val shortFilter  :Filter[Short] = new Filter[Short]((i :Short) => i % 10 >= 7)
	implicit val charFilter   :Filter[Char] = new Filter[Char]((i :Char) => i.isLetter)
//	implicit val longFilter     = new Filter[Long]((i :Long) => i % 10 >= 7)
	implicit val floatFilter  :Filter[Float] = new Filter[Float]((i :Float) => i <= 0)
	implicit val doubleFilter :Filter[Double] = new Filter[Double]((i :Double) => i <= 0)

}



abstract class IterableProps[C[T] <: S[T], S[T] <: Iterable[T]](name :String)
	extends GenericIterableProps[C, S, Dummy](name)
{
	implicit override def intEvidence    :Dummy[Int] = new Dummy
	implicit override def longEvidence   :Dummy[Long] = new Dummy
	implicit override def stringEvidence :Dummy[String] = new Dummy
	implicit override def intSEvidence   :Dummy[S[Int]] = new Dummy
	implicit override def intCEvidence   :Dummy[C[Int]] = new Dummy
	implicit override def pairEvidence[A :Dummy, B :Dummy] :Dummy[(A, B)] = new Dummy

	protected override def S[T :Dummy] = referenceFactory
	protected override def C[T :Dummy] = checkedFactory

	protected def referenceFactory :IterableFactory[S]
	protected def checkedFactory   :IterableFactory[C]
}



object IterableProps {
	class Filter[T](val f :T => Boolean)
	class FoldSide[A, T](val zero :A, val f :(A, T) => A)
	class Fold[T](val zero :T, val f :(T, T) => T)
	class Map[-T, +A](val f :T => A)
	class FlatMap[-T, +A](val f :T => IterableOnce[A])
	class Dummy[T]
//	type FlatMap[-T, +A] = Map[T, IterableOnce[A]]

	@inline def filter[T](implicit filter :Filter[T]) :T => Boolean = filter.f
	@inline def filter[T](x :T)(implicit filter :Filter[T]) :Boolean = filter.f(x)
	@inline def foldZero[A, T](implicit fold :FoldSide[A, T]) :A = fold.zero
//	@inline def foldZero[T](implicit fold :Fold[T]) :T = fold.zero
	@inline def value[T](implicit fold :Fold[T]) :T = fold.zero
	@inline def foldLeft[A, T](implicit fold :FoldSide[A, T]) :(A, T) => A = fold.f
	@inline def foldLeft[A, T](a :A, t :T)(implicit fold :FoldSide[A, T]) :A = fold.f(a, t)
	@inline def foldRight[A, T](implicit fold :FoldSide[A, T]) :(T, A) => A = (t, a) => fold.f(a, t)
	@inline def foldRight[A, T](t :T, a :A)(implicit fold :FoldSide[A, T]) :A = fold.f(a, t)
	@inline def fold[T](implicit fold :Fold[T]) :(T, T) => T = fold.f
	@inline def fold[T](a :T, b :T)(implicit fold :Fold[T]) :T = fold.f(a, b)
	@inline def map[T, A](implicit map :Map[T, A]) :T => A = map.f
	@inline def map[T, A](x :T)(implicit map :Map[T, A]) :A = map.f(x)
	@inline def flatMap[T, A](implicit map :FlatMap[T, A]) :T => IterableOnce[A] = map.f
	@inline def flatMap[T, A](x :T)(implicit map :FlatMap[T, A]) :IterableOnce[A] = map.f(x)
	@inline implicit def dummy[T] = new Dummy[T]
}






trait OrderedProps[C[T] <: S[T], S[T] <: Iterable[T], E[T]] extends GenericIterableProps[C, S, E] {
/*
	property("head")         = test { (expect :S[Int], result :C[Int]) =>
	                            if (expect.isEmpty) result.head.throws[NoSuchElementException]
	                            else expect.head =? result.head
	                           }
	property("last")         = test { (expect :S[Int], result :C[Int]) =>
	                           	if (expect.isEmpty) result.last.throws[NoSuchElementException]
	                           	else expect.last =? result.last
	                           }
	property("headOption")   = compare((_ :S[Int]).headOption)
	property("lastOption")   = compare((_ :S[Int]).lastOption)
	property("collectFirst") = compare((_ :S[Int]).collectFirst { case i if filter(i) => i })
	property("find")         = compare((_ :S[Int]).find(filter))
	property("init")         = test { (expect :S[Int], result :C[Int]) =>
	                           	if (expect.isEmpty) result.init.throws[UnsupportedOperationException]
	                           	else validate(expect.init to S, result.init to C)
	                           }
	property("tail")         = test { (expect :S[Int], result :C[Int]) =>
	                           	if (expect.isEmpty) result.tail.throws[UnsupportedOperationException]
	                           	else validate(expect.tail to S, result.tail to C)
	                           }
	property("take")         = test { (expect :S[Int], result :C[Int]) =>
	                           	forAll { i :Int => validate(expect.take(i) to S, result.take(i) to C) }
	                           }
	property("drop")         = test { (expect :S[Int], result :C[Int]) =>
	                        	forAll { i :Int => validate(expect.drop(i) to S, result.drop(i) to C) }
	                           }
	property("takeRight")    = test { (expect :S[Int], result :C[Int]) =>
	                        	forAll { i :Int => validate(expect.takeRight(i) to S, result.takeRight(i) to C) }
	                           }
	property("dropRight")    = test { (expect :S[Int], result :C[Int]) =>
	                           	forAll { i :Int => validate(expect.dropRight(i) to S, result.dropRight(i) to C) }
	                           }
*/

/*
	property("scan")         = test((_ :S[Int]).scan(value[Int])(fold))
	property("scanLeft")     = test((_ :S[Int]).scanLeft(foldZero[Long, Int])(foldLeft))
	property("scanRight")    = test((_ :S[Int]).scanRight(foldZero[Long, Int])(foldRight))
	property("zipWithIndex") = test { (expect :S[Int], result :C[Int]) =>
	                           	compare(expect.zipWithIndex, result.zipWithIndex)
	                           }
*/




	protected def orderedProps[T, F, M, FM](expect :S[T], result :S[T])
	                                       (implicit tag :ClassTag[T], arbitrary :Arbitrary[T], ev :E[T],
	                                                 filt :Filter[T], fldA :FoldSide[F, T], evf :E[F], fld :Fold[T],
	                                                 mp :Map[T, M], evm :E[M], fmap :FlatMap[T, FM], evfm :E[FM]) :Prop =
		all(
			"head"                 lbl_: (if (expect.isEmpty) result.head.throws[NoSuchElementException]
			                             else expect.head =? result.head),
			"last"                 lbl_: (if (expect.isEmpty) result.last.throws[NoSuchElementException]
			                             else expect.last =? result.last),
			"headOption"           lbl_: expect.headOption =? result.headOption,
			"lastOption"           lbl_: expect.lastOption =? result.lastOption,
			"collectFirst"         lbl_: expect.collectFirst { case a if filter(a) => a } =?
			                                result.collectFirst { case a if filter(a) => a },
			"find"                 lbl_: expect.find(filter) =? result.find(filter),
			"init"                 lbl_: (if (expect.isEmpty) result.init.throws[UnsupportedOperationException]
			                             else compare(expect.init, result.init)),
			"tail"                 lbl_: (if (expect.isEmpty) result.tail.throws[UnsupportedOperationException]
			                             else compare(expect.tail, result.tail)),
			"take"                 lbl_: forAll { n :Int => compare(expect.take(n), result.take(n)) },
			"drop"                 lbl_: forAll { n :Int => compare(expect.drop(n), result.drop(n)) },
			"takeRight"            lbl_: forAll { n :Int => compare(expect.takeRight(n), result.takeRight(n)) },
			"dropRight"            lbl_: forAll { n :Int => compare(expect.dropRight(n), result.dropRight(n)) },
			"slice"                lbl_: forAll { (i :Int, j :Int) => compare(expect.slice(i, j), result.slice(i, j)) },
			"splitAt"              lbl_: forAll { n :Int =>
			                             	val (expect_1, expect_2) = expect.splitAt(n)
			                             	val (result_1, result_2) = result.splitAt(n)
			                             	(compare(expect_1, result_1) lbl "_1") && compare(expect_2, result_2)
			                             },
			"takeWhile"            lbl_: forAll { n :Int => compare(expect.takeWhile(filter), result.takeWhile(filter)) } &&
			                                compare(expect :Iterable[T], result.takeWhile(_ => true)) :| "all",
			"dropWhile"            lbl_: forAll { n :Int => compare(expect.dropWhile(filter), result.dropWhile(filter)) },
			                                compare(expect.empty, result.dropWhile(_ => true)) :| "all",
			"span"                 lbl_: forAll { n :Int =>
			                             	val (expect_1, expect_2) = expect.span(filter)
			                             	val (result_1, result_2) = result.span(filter)
			                             	("_1" |: compare(expect_1, result_1)) && ("_2" |: compare(expect_2, result_2))
			                             },
			"scan"                 lbl_: compare(expect.scan(value[T])(fold), result.scan(value[T])(fold)),
			"scanLeft"             lbl_: compare(expect.scanLeft(foldZero[F, T])(foldLeft(_, _)),
			                                     result.scanLeft(foldZero[F, T])(foldLeft(_, _))),
			"scanRight"            lbl_: compare(expect.scanRight(foldZero[F, T])(foldRight(_, _)),
			                                     result.scanRight(foldZero[F, T])(foldRight(_, _))),
			"zipWithIndex"         lbl_: compare(expect.zipWithIndex, result.zipWithIndex)
			//"zip",
			//"zipAll",
		)


	protected override def props[T, F, M, FM](expect :S[T], result :S[T])
	                                         (implicit arbitrary :Arbitrary[T], ev :E[T], tag :ClassTag[T],
	                                          filt :Filter[T], fldA :FoldSide[F, T], evf :E[F], fld :Fold[T],
	                                          mp :Map[T, M], evm :E[M], fmap :FlatMap[T, FM], evfm :E[FM]) :Prop =
		super.props(expect, result) && orderedProps(expect, result)
}





abstract class SeqProps[C[+T] <: Seq[T]](name :String)
	extends IterableProps[C, Seq](name) with OrderedProps[C, Seq, Dummy]
{
	//	include("newBuilder", new SeqBuilderStateProperties[A])
/*
	property("length") = test { (expect :Seq[Int], subject :C[Int]) => expect.length =? subject.length }
	property("apply") = test { (expect :Seq[Int], subject :C[Int]) =>
		all(expect.indices.map(i => expect(i) =? subject(i)) :_*)
	}
	property("distinct") = test((_ :Seq[Int]).distinct)
	property("reverse") = test((_ :Seq[Int]).reverse)
	property("reverseIterator") = test { (expect :Seq[Int], subject :C[Int]) =>
		checkIterator(expect.reverse, subject.reverseIterator)
	}
	property("startsWith") = test { (expect :Seq[Int], subject :C[Int]) =>
		all(
			(for {
				from <- 0 to expect.length
				until <- from to expect.length
			} yield Prop(subject.startsWith(expect.slice(from, until), from)))
		:_*) && forAll { (seq :Seq[Int], offset :Int) =>
			if (offset < 0 || offset > expect.length) Prop(!subject.startsWith(seq, offset))
			else expect.startsWith(seq, offset) =? subject.startsWith(seq, offset)
		}
	}
	property("endsWith") = test { (expect :Seq[Int], subject :C[Int]) =>
		all((0 to expect.length).map(i => Prop(subject.endsWith(expect.drop(i)))) :_*)
	}
//	property("padTo") = forAll { (length :Short, elem :Int) => test((_ :Seq[Int]).padTo(length, elem)) }
	property("padTo") = test { (expect :Seq[Int], subject :C[Int]) =>
		forAll { (length :Short, elem :Int) => expect.padTo(length, elem) =? subject.padTo(length, elem) }
	}
	property("segmentLength") = test { (expect :Seq[Int], subject :C[Int]) =>
		all(expect.indices.map(i => expect.segmentLength(_ % 3 <= 1, i) =? subject.segmentLength(_ % 3 <= 1, i)) :_*)
	}


	property("indexOf") = test { (expect :Seq[Int], subject :C[Int]) =>
		forAll { (x :Int, i :Int) =>
			expect.indexOf(x, i) =? subject.indexOf(x, i)
		} && all(expect.mapWithIndex { (x, i) =>
			(expect.indexOf(x) =? subject.indexOf(x)) :| (x.toString + "@" + expect.indexOf(x)) &&
				(subject.indexOf(x, i) ?= i) :| s"#$i->$x"
		} :_*)
	}
	property("lastIndexOf") = test { (expect :Seq[Int], subject :C[Int]) =>
		forAll { (x :Int, i :Int) =>
			expect.lastIndexOf(x, i) =? subject.lastIndexOf(x, i)
		} && all(expect.mapWithIndex { (x, i) =>
			(expect.lastIndexOf(x) =? subject.lastIndexOf(x)) :| (x.toString + "@" + expect.lastIndexOf(x)) &&
				(subject.lastIndexOf(x, i) ?= i) :| s"#$i->$x"
		} :_*)
	}
	property("indexWhere") = test { (expect :Seq[Int], subject :C[Int]) =>
		forAll { (x :Int, i :Int) =>
			expect.indexOf(x, i) =? subject.indexWhere(_ == x, i)
		} && all(expect.mapWithIndex { (x, i) =>
			(expect.indexOf(x) =? subject.indexWhere(_ == x)) :| (x.toString + "@" + expect.indexOf(x)) &&
				(subject.indexWhere(_ == x, i) ?= i) :| s"#$i->$x"
		} :_*)
	}
	property("lastIndexWhere") = test { (expect :Seq[Int], subject :C[Int]) =>
		forAll { (x :Int, i :Int) =>
			expect.lastIndexOf(x, i) =? subject.lastIndexWhere(_ == x, i)
		} && all(expect.mapWithIndex { (x, i) =>
			(expect.lastIndexOf(x) =? subject.lastIndexWhere(_ == x)) :| (x.toString + "@" + expect.lastIndexOf(x)) &&
				(subject.lastIndexWhere(_ == x, i) ?= i) :| s"$x<-#$i"
		} :_*)
	}
	property("indexOfSlice") = test { (expect :Seq[Int], subject :C[Int]) =>
		val indexed = expect.toVector //List.indexOfSlice is buggy in 2.13.10
		all(
			(for {
				from <- expect.indices
				until <- from to expect.length
			} yield {
				val slice = expect.slice(from, until)
				val i = expect.indexOfSlice(slice)
				((subject.indexOfSlice(slice) ?= i) :| slice.toString + "@" + i) &&
					((subject.indexOfSlice(slice, from) ?= from) :| "[" + from + ", " + until + ")@" + from)
			})
		:_*) && forAll { (x :Seq[Int], i :Short) =>
			subject.indexOfSlice(x, i & 0xffff) ?= indexed.indexOfSlice(x, i & 0xffff)
		}
	}
	property("lastIndexOfSlice") = test { (expect :Seq[Int], subject :C[Int]) =>
			all(
			(for {
				from <- expect.indices
				until <- from to expect.length
			} yield {
				val slice = expect.slice(from, until)
				val i = expect.lastIndexOfSlice(slice)
				((subject.lastIndexOfSlice(slice) ?= i) :| slice.toString + "@" + i) &&
					((subject.lastIndexOfSlice(slice, from) ?= from) :| "[" + from + ", " + until + ")@" + from)
			})
		:_*) && forAll { (x :Seq[Int], i :Short) =>
			subject.lastIndexOfSlice(x, i & 0xffff) ?= expect.lastIndexOfSlice(x, i & 0xffff)
		}
	}

	property("updated") = test { (expect :Seq[Int], subject :C[Int]) =>
		import numeric.globalRandom
		all(expect.indices.map { i =>
			val elem = Int.random
			val reference = expect.updated(i, elem)
			val result = subject.updated(i, elem)
			result ?= reference
		} :_*)
	}
	property("patch") = forAll { (from :Int, patch :Seq[Int], replaced :Int) =>
		test((_:Seq[Int]).patch(from, patch, replaced))
	}

	property(":+(Int)") = test { (expect :Seq[Int], subject :C[Int]) =>
		forAll { x :Int => validate(expect :+ x, subject :+ x) }
	}
	property(":+(String)") = test { (expect :Seq[Int], subject :C[Int]) =>
		forAll { x :String => compare(expect :+ x, subject :+ x) }
	}

	property("(Int)+:") = test { (expect :Seq[Int], subject :C[Int]) =>
		forAll { x :Int => validate(x +: expect, x +: subject) }
	}
	property("(String)+:") = test { (expect :Seq[Int], subject :C[Int]) =>
		forAll { x :String => compare(x +: expect, x +: subject) }
	}
*/

	var prideCounter = 0
	property(":++(Seq[Int]))") = forAll { (prefix :C[Int], suffix :Seq[Int]) =>
		System.err.println(f"$prideCounter%10d: $prefix ++: $suffix")
		prideCounter += 1
		validate(List.from(prefix) ++ suffix, (prefix :++ suffix) to C)
	}
/*
	property(":++(Seq[String])") = forAll { (prefix :C[Int], suffix :Seq[String]) =>
		compare(List.from(prefix) ++ suffix :Seq[Any], (prefix :++ suffix) to C)
	}
	property(s":++(${C[Int].source.localClassName}[Int])") = forAll { (prefix :C[Int], suffix :C[Int]) =>
		validate(List.from(prefix) ::: List.from(suffix), (prefix :++ suffix) to C)
	}
	property(s":++(${C[String].source.localClassName}[String])") = forAll { (prefix :C[Int], suffix :C[String]) =>
		compare(List.from(prefix) ::: List.from(suffix) :Seq[Any], (prefix :++ suffix) to C)
	}
*/

	var shameCounter = 0
	property("(Seq[Int]))++:") = forAll { (prefix :Seq[Int], suffix :C[Int]) =>
		System.err.println(f"$shameCounter%10d: $prefix ++: $suffix")
		shameCounter += 1
		validate(prefix ++ List.from(suffix), (prefix ++: suffix) to C)
	}
/*
	property("(Seq[String])++:") = forAll { (prefix :Seq[String], suffix :C[Int]) =>
		compare(prefix ++ List.from(suffix) :Seq[Any], (prefix ++: suffix) to C)
	}
	property(s"(${C[Int].source.localClassName}[Int])++:") = forAll { (prefix :C[Int], suffix :C[Int]) =>
		validate(List.from(prefix) ::: List.from(suffix), (prefix ++: suffix) to C)
	}
	property(s"(${C[String].source.localClassName}[String])++:") =
		forAll { (prefix :C[Int], suffix :C[String]) =>
			compare(List.from(prefix) ::: List.from(suffix) :Seq[Any], (prefix ++: suffix) to C)
		}
*/


	protected override def orderedProps[T, F, M, FM](expect :Seq[T], result :Seq[T])
	                                                (implicit tag :ClassTag[T], arbitrary :Arbitrary[T], ev :Dummy[T],
	                                                 filt :Filter[T], fldA :FoldSide[F, T], evf :Dummy[F], fld :Fold[T],
	                                                 mp :Map[T, M], evm :Dummy[M],
	                                                 fmap :FlatMap[T, FM], evfm :Dummy[FM]) :Prop =
		super.orderedProps(expect, result) && all(
			"apply"           lbl_: all(expect.indices.map(i => (expect(i) =? result(i)) :| i.toString) :_*) &&
		                                result(-1).throws[IndexOutOfBoundsException] :| "-1" &&
			                            result(expect.length).throws[IndexOutOfBoundsException] :| expect.length.toString,
			"distinct"        lbl_: compare(expect.distinct, result.distinct),
			"reverseIterator" lbl_: checkIterator(expect.reverse, result.reverseIterator),
			"reverse"         lbl_: compare(expect.reverse, result.reverse),
			"updated"         lbl_: forAll { (i :Int, x :T) =>
			                            if (i < 0 || i >= expect.length)
				                            result.updated(i, x).throws[IndexOutOfBoundsException]
			                            else
				                            compare(expect.updated(i, x), result.updated(i, x))
			                        },
			"startsWith"      lbl_: forAll { (from :Int, until :Int) =>
			                            if (from < 0)
				                            Prop(!result.startsWith(expect.slice(from, until), from))
			                            else if (from > expect.length)
				                            Prop(!result.startsWith(Nil, from))
			                            else if (from <= until)
				                            Prop(result.startsWith(expect.slice(from, until), from))
			                            else forAll { subseq :Seq[T] =>
				                            expect.startsWith(subseq, from) =? result.startsWith(subseq, from)
			                            }
			                        },
			"endsWith "       lbl_: forAll { (from :Int) =>
			                            if (from < 0 || from > expect.length)
				                            forAll {
					                            subseq :Seq[T] => expect.endsWith(subseq) =? result.endsWith(subseq)
				                            }
			                            else
				                            Prop(result.endsWith(expect.drop(from)))
			                        },
			"segmentLength"   lbl_: forAll { (from :Int) =>
			                        	expect.segmentLength(filt.f, from) =? result.segmentLength(filt.f, from)
			                        },
			"indexOf"         lbl_: forAll { (x :T, from :Int) => expect.indexOf(x, from) =? result.indexOf(x, from) },
			"lastIndexOf"     lbl_: forAll { (x :T, from :Int) =>
			                            expect.lastIndexOf(x, from) =? result.lastIndexOf(x, from)
			                        },
			"indexWhere"      lbl_: forAll { (from :Int) =>
			                        	expect.indexWhere(filt.f, from) =? result.indexWhere(filt.f, from)
			                        },
			"lastIndexWhere"   lbl_: forAll { (from :Int) =>
			                        	expect.lastIndexWhere(filt.f, from) =? result.lastIndexWhere(filt.f, from)
			                        },
			"indexOfSlice"     lbl_: forAll { (x :Seq[T], from :Int) =>
			                            if (from == expect.length && x.isEmpty) result.indexOfSlice(x, from) ?= from
			                            else expect.indexOfSlice(x, from max 0) =? result.indexOfSlice(x, from)
			                         },
			"lastIndexOfSlice" lbl_: forAll { (x :Seq[T], from :Int) =>
			                            if (from == expect.length && x.isEmpty) result.indexOfSlice(x, from) ?= from
			                            else expect.lastIndexOfSlice(x, from) =? result.lastIndexOfSlice(x, from)
			                         },
		)


	override def referenceFactory :IterableFactory[Seq] = Seq
	override def hasOrder = true
}






//object SeqProperties extends Properties("immutable.Seq builder implementations") {
//
//	type A = Int
//
//	property("Seq builder stateful testing") = new SeqBuilderStateProperties(Seq.newBuilder[A]).property()
//	property("List builder stateful testing") = new SeqBuilderStateProperties(List.newBuilder[A]).property()
//	property("ArraySeq builder stateful testing") = new SeqBuilderStateProperties(ArraySeq.newBuilder[A]).property()
//	property("Queue builder stateful testing") = new SeqBuilderStateProperties(Queue.newBuilder[A]).property()
//	property("IndexedSeq builder stateful testing") = new SeqBuilderStateProperties(IndexedSeq.newBuilder[A]).property()
//	property("Stream builder stateful testing") = new SeqBuilderStateProperties(Stream.newBuilder[A]).property()
//	property("Vector builder stateful testing") = new SeqBuilderStateProperties(Vector.newBuilder[A]).property()
//	property("WrappedString builder stateful testing") = new SeqBuilderStateProperties(WrappedString.newBuilder).property()
//}




/** Generic stateful property testing for Seq builders
  *
  * Usage: {{{
  *   class MyCollectionProperties extends Properties("my.Collection") {
  *      property("MyCollection builder stateful testing") =
  *        new SeqBuilderStateProperties(MySeq.newBuilder[A]).property() &&
  *   }
  * }}}
  * @param arbA  gen for the elements of the Seq
  * @tparam To the type of Seq under test
  */
class SeqBuilderStateProperties[A, To <: Seq[A]](newBuilder : => Builder[A, To])(implicit arbA :Arbitrary[A])
	extends Commands
{
	override type State = Seq[A]
	override type Sut = mutable.Builder[A, To]

	import Gen._

	val commandGen = Gen.oneOf(
		const(Clear),
		const(Result),
		choose(0, 10000).map(SizeHint(_)),
		arbA.arbitrary.map(a => AddOne(a)),
		listOf(arbA.arbitrary).map(a => AddAll(a))
	)

	override def genInitialState :Gen[State] = newBuilder.result()

	override def canCreateNewSut(newState :State, initSuts :scala.Iterable[State], runningSuts :scala.Iterable[Sut]) = true
	override def newSut(state :State) :mutable.Builder[A, To] = newBuilder.addAll(state)
	override def destroySut(sut :Sut) :Unit = ()
	override def initialPreCondition(state :State) = state.isEmpty

	override def genCommand(state :State) :Gen[Command] = commandGen

	case object Clear extends UnitCommand {
		override def postCondition(state :State, success :Boolean) = success
		override def run(sut :Sut) :Unit = sut.clear()
		override def nextState(state :State) = Vector.empty
		override def preCondition(state :State) = true
	}

	case object Result extends Command {
		override type Result = State
		override def postCondition(state :State, result :Try[Result]) = result.map(_.toVector) == Success(state.toVector)
		override def run(sut :Sut) = sut.result().toVector
		override def nextState(state :State) = state
		override def preCondition(state :State) = true
	}

	case class SizeHint(size :Int) extends UnitCommand {
		override def postCondition(state :State, success :Boolean) = success
		override def run(sut :Sut) = sut.sizeHint(size)
		override def nextState(state :State) = state
		override def preCondition(state :State) = true
	}

	case class AddOne(elem :A) extends UnitCommand {
		override def postCondition(state :State, success :Boolean) = success
		override def run(sut :Sut) = sut.addOne(elem)
		override def nextState(state :State) = state.appended(elem)
		override def preCondition(state :State) = true
	}

	case class AddAll(elems :scala.collection.immutable.Seq[A]) extends UnitCommand {
		override def postCondition(state :State, success :Boolean) = success
		override def run(sut :Sut) = sut.addAll(elems)
		override def nextState(state :State) = state.appendedAll(elems)
		override def preCondition(state :State) = true
	}
}







/*
class SeqTest extends AllocationTest {

	@Test def `t9936 indexWhere`() :Unit = {
		assertEquals(2, "abcde".indexOf('c', -1))
		assertEquals(2, "abcde".indexOf('c', -2))
		assertEquals(2, "abcde".toVector.indexOf('c', -1))
		assertEquals(2, "abcde".toVector.indexOf('c', -2))
		assertEquals(2, "abcde".toVector.indexWhere(_ == 'c', -1))
		assertEquals(2, "abcde".toVector.indexWhere(_ == 'c', -2))
	}

	@Test def combinations() :Unit = {
		assertEquals(List(Nil), Nil.combinations(0).toList)
		assertEquals(Nil, Nil.combinations(1).toList)
		assertEquals(List(List(1, 2), List(1, 3), List(2, 3)), List(1, 2, 3).combinations(2).toList)
		assertEquals(List(List(1, 2, 3)), List(1, 2, 3).combinations(3).toList)
	}

	@Test
	def hasCorrectDistinct() :Unit = {
		assertEquals(Seq(1, 2, 3, 4, 5), Seq(1, 1, 2, 3, 3, 3, 4, 5, 5).distinct)
	}

	@Test
	def hasCorrectDistinctBy() :Unit = {
		val result = Seq("a", "aa", "aaa", "b", "bb", "bbb", "bbbb", "c").distinctBy(_.length)

		assertEquals(Seq("a", "aa", "aaa", "bbbb"), result)
	}

	@Test
	def hasCorrectIndexOfSlice() :Unit = {
		assertEquals(0, Vector(0, 1).indexOfSlice(List(0, 1)))
		assertEquals(0, Vector(0, 1).indexOfSlice(Vector(0, 1)))
		assertEquals(1, Vector(0, 1, 2, 0, 1, 2).indexOfSlice(Vector(1, 2)))
		assertEquals(4, Vector(0, 1, 2, 0, 1, 2).indexOfSlice(Vector(1, 2), from = 2))
		assertEquals(-1, List(0, 1).indexOfSlice(List(1, 2)))
	}

	@Test
	def hasCorrectLastIndexOfSlice() :Unit = {
		assertEquals(0, Vector(0, 1).lastIndexOfSlice(List(0, 1)))
		assertEquals(0, Vector(0, 1).lastIndexOfSlice(Vector(0, 1)))
		assertEquals(4, Vector(0, 1, 2, 0, 1, 2).lastIndexOfSlice(Vector(1, 2)))
		assertEquals(1, Vector(0, 1, 2, 0, 1, 2).lastIndexOfSlice(Vector(1, 2), end = 3))
		assertEquals(-1, List(0, 1).lastIndexOfSlice(List(1, 2)))
	}

	@Test
	def hasCorrectDiff() :Unit = {
		val s1 = Seq(1, 2, 3, 4, 5)
		val s2 = Seq(1, 3, 5, 7, 9)

		assertEquals(Seq(2, 4), s1.diff(s2))
	}

	@Test
	def hasCorrectIntersect() :Unit = {
		val s1 = Seq(1, 2, 3, 4, 5)
		val s2 = Seq(1, 3, 5, 7, 9)

		assertEquals(Seq(1, 3, 5), s1.intersect(s2))
	}

	@deprecated("Tests deprecated API", since = "2.13")
	@Test
	def unionAlias() :Unit = {
		val s1 = Seq(1, 2, 3)
		val s2 = Seq(4, 5, 6)
		assertEquals(s1.concat(s2), s1.union(s2))
	}

	@Test
	def testLengthIs() :Unit = {
		val s = Seq(1, 2, 3)
		assert(s.lengthIs <= 3)
		assert(s.lengthIs == 3)
		assert(s.lengthIs >= 3)
		assert(s.lengthIs <= 4)
		assert(s.lengthIs < 4)
		assert(s.lengthIs != 4)
		assert(s.lengthIs >= 2)
		assert(s.lengthIs > 2)
		assert(s.lengthIs != 2)
	}

	@Test def emptyNonAllocating() :Unit = {
		nonAllocating(Seq.empty)
		nonAllocating(Seq())
	}

	@Test def smallSeqAllocation() :Unit = {
		if (CompileTime.versionNumberString == "2.13.2") return
		exactAllocates(Sizes.list * 1, "collection seq  size 1")(Seq("0"))
		exactAllocates(Sizes.list * 2, "collection seq  size 2")(Seq("0", "1"))
		exactAllocates(Sizes.list * 3, "collection seq  size 3")(Seq("0", "1", ""))
		exactAllocates(Sizes.list * 4, "collection seq  size 4")(Seq("0", "1", "2", "3"))
		exactAllocates(Sizes.list * 5, "collection seq  size 5")(Seq("0", "1", "2", "3", "4"))
		exactAllocates(Sizes.list * 6, "collection seq  size 6")(Seq("0", "1", "2", "3", "4", "5"))
		exactAllocates(Sizes.list * 7, "collection seq  size 7")(Seq("0", "1", "2", "3", "4", "5", "6"))
	}

	@Test def largeSeqAllocation() :Unit = {
		def expected(n :Int) = Sizes.list * n + Sizes.wrappedRefArray(n) + Sizes.wrappedRefArrayIterator
		exactAllocates(expected(10), "collection seq size 10")(
			Seq("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
		exactAllocates(expected(20), "collection seq size 20")(
			Seq("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19"))
	}
}
*/
