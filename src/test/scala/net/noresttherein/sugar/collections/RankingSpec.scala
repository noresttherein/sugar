package net.noresttherein.sugar.collections

import scala.collection.{IterableFactory, mutable}
import scala.reflect.ClassTag

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Prop.{AnyOperators, all, forAll, propBoolean, throws}
import net.noresttherein.sugar.collections.IterableProps.{Dummy, Filter, FlatMap, Fold, FoldSide, Map}
import net.noresttherein.sugar.extensions.{SeqExtension, classNameMethods}
import net.noresttherein.sugar.numeric.globalRandom
import net.noresttherein.sugar.numeric.extensions.{BooleanCompanionExtension, IntCompanionExtension}
import net.noresttherein.sugar.testing.scalacheck.extensions.{BooleanAsPropExtension, LazyExtension, PropExtension}
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{Yes, No}




object RankingSpec
	extends IterableProps[Ranking, Iterable]("Ranking")(Ranking, Vector)
	   with OrderedProps[Ranking, Iterable, Dummy]
{
	import net.noresttherein.sugar.testing.scalacheck.noShrinking

//	private def dedup[T](expect :Iterable[T]) :Iterable[T] =
//		expect.flatMapWith(Set.empty[T]) { (t, dups) => (if (dups(t)) Nil else t::Nil, dups + t) }

	//todo: test the proper builder semantics
	private def aligned[T](expect :List[T], result :Ranking[T], i :Int) :Boolean = expect match {
		case h::t if i < result.size && h == result(i) => aligned(t, result, i + 1)
		case _::t if i < result.size                   => aligned(t, result, i)
		case h::t if result.contains(h)                => aligned(t, result, i)
		case Nil => true
		case _   => false
	}
	private def equalsFirstOccurrences[T](expect :Iterable[T], result :Iterable[T]) :Prop =
		result.isInstanceOf[Ranking[_]] :| s"${result.className}.isInstanceOf[Ranking]" &&
			expect.toSeq.distinct =? result.toSeq lbl "aligned with first occurrences"

	private def equalsLastOccurrences[T](expect :Iterable[T], result :Iterable[T]) :Prop =
		result.isInstanceOf[Ranking[_]] :| s"${result.className}.isInstanceOf[Ranking]" &&
			expect.toSeq.reverse.distinct.reverse =? result.toSeq lbl "aligned with last occurrences"

	protected override def compare[T :Dummy](expect :Iterable[T], result :Iterable[T]) :Prop =
		equalsFirstOccurrences(expect, result) lbl "Expected: " + expect + ";\n     got: " + result

	protected override def compare[T :Arbitrary :Dummy, X](f :Iterable[T] => X) :Prop =
		forAll { elems :Seq[T] => f(elems.toSeq.distinct) =? f(elems to C) }

	protected override def test[T :Arbitrary :Dummy](check :(Iterable[T], Ranking[T]) => Prop) :Prop =
		forAll { elems :Seq[T] =>
			val distinct = elems.toSeq.distinct
			check(distinct, Ranking.from(elems)) lbl distinct.mkString("Distinct(", ", ", ")")
		}

	protected override def test[T, X, F, M, FM]
	                           (f :Iterable[T] => Iterable[X])
	                           (implicit input :Arbitrary[T], output :Arbitrary[X], ev1 :Dummy[T], ev2 :Dummy[X],
	                            tag :ClassTag[X], filt :Filter[X], fldA :FoldSide[F, X], evf :Dummy[F], fld :Fold[X],
	                            mp :Map[X, M], evm :Dummy[M], fmap :FlatMap[X, FM], evfm :Dummy[FM]) :Prop =
		forAll { elems :Seq[T] => validate(f(elems.toSeq.distinct).toSeq.distinct, f(elems to C) to C) }

	protected override def validate[T, F, M, FM](label: => String, expect :Iterable[T], result :Iterable[T])
	                                            (implicit arbitrary :Arbitrary[T], ev :Dummy[T], tag :ClassTag[T],
	                                             filt :Filter[T], fldA :FoldSide[F, T], evf :Dummy[F], fld :Fold[T],
	                                             mp :Map[T, M], evm :Dummy[M], fmap :FlatMap[T, FM], evfm :Dummy[FM]) :Prop =
		equalsFirstOccurrences(expect, result) && all(props(result.toSeq, result) :_*) lbl label

	private def validateAppend[T, F, M, FM](expect :Iterable[T], result :Iterable[T])
	                                      (implicit arbitrary :Arbitrary[T], ev :Dummy[T], tag :ClassTag[T], filt :Filter[T],
	                                       fldA :FoldSide[F, T], evf :Dummy[F], fld :Fold[T], mp :Map[T, M], evm :Dummy[M],
	                                       fmap :FlatMap[T, FM], evfm :Dummy[FM]) :Prop =
		equalsLastOccurrences(expect, result) && all(props(result.toSeq, result) :_*) lbl
			s"Input:   $expect;\ntesting: $result :${result.localClassName}"

	private def validateAnyOrder[T, F, M, FM](expect :Iterable[T], result :Iterable[T])
	                                         (implicit arbitrary :Arbitrary[T], ev :Dummy[T], tag :ClassTag[T], filt :Filter[T],
	                                          fldA :FoldSide[F, T], evf :Dummy[F], fld :Fold[T], mp :Map[T, M], evm :Dummy[M],
	                                          fmap :FlatMap[T, FM], evfm :Dummy[FM]) :Prop =
		result.isInstanceOf[Ranking[_]] :| s"${result.className}.isInstanceOf[Ranking]" &&
			(aligned(expect.toList, result to Ranking, 0) lbl "aligned with any occurrences") &&
			all(props(result.toSeq, result) :_*) lbl
				s"Input:   $expect;\ntesting: $result :${result.localClassName}"

//
//	protected override def props[T, F, M, FM](expect :Iterable[T], result :Iterable[T])
//	                                         (implicit arbitrary :Arbitrary[T], ev :Dummy[T], tag :ClassTag[T],
//	                                          filt :Filter[T], fldA :FoldSide[F, T], evf :Dummy[F],
//	                                          fld :Fold[T], mp :IterableProps.Map[T, M], evm :Dummy[M],
//	                                          fmap :FlatMap[T, FM], evfm :Dummy[FM]) :Prop =
//		super.props(expect.toSeq.distinct, result)

	override def knowsSize = true
	override def hasOrder  = true

	property("iterableFactory.from") = forAll { list :List[Int] =>
		val distinct = list.distinct :Seq[Int]
		("List" |: distinct =? Ranking.from(list).toSeq) &&
			("Vector" |: distinct =? Ranking.from(list.toVector).toSeq) &&
			("Set" |: distinct =? Ranking.from(new SeqSet(distinct)).toSeq) &&
			("Iterator" |: distinct =? Ranking.from(list.iterator).toSeq) &&
			("VectorIterator" |: distinct =? Ranking.from(list.toVector.iterator).toSeq)
	}
	property("iterableFactory.lastOccurrences") = forAll { list :List[Int] =>
		val distinct = list.reverse.distinct.reverse :Seq[Int]
		("List" |: distinct =? Ranking.lastOccurrences(list).toSeq) &&
			("Vector" |: distinct =? Ranking.lastOccurrences(list.toVector).toSeq) &&
			("Set" |: distinct =? Ranking.lastOccurrences(new SeqSet(distinct)).toSeq) &&
			("Iterator" |: distinct =? Ranking.lastOccurrences(list.iterator).toSeq) &&
			("VectorIterator" |: distinct =? Ranking.lastOccurrences(list.toVector.iterator).toSeq)
	}
	//todo: stateful tests for newBuilder, appendingBuilder, unorderedBuilder

	property("length") = test { (expect :Iterable[Int], subject :Ranking[Int]) => expect.size =? subject.length }
	property("apply") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		val seq = expect.toSeq
		all(seq.indices.map(i => seq(i) =? subject(i)) :_*)
	}

	property("startsWith") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		val seq = expect.toVector
		all(
			(for {
				from <- 0 to seq.length
				until <- from to seq.length
				slice = seq.view.slice(from, until)
			} yield
				Prop(subject.startsWith(slice, from)) lbl slice.toString + "@" + from)
		:_*) && forAll { (subseq :Seq[Int], offset :Int) =>
			(offset >= 0 && offset <= seq.length && seq.startsWith(subseq, offset)) =? subject.startsWith(subseq, offset)
		}
	}
	property("endsWith") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		val seq = expect.toSeq
		all((0 to seq.length).map(i => Prop(subject.endsWith(seq.drop(i)))) :_*)
	}
	property("segmentLength") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		val seq = expect.toSeq
		all(seq.indices.map(i => seq.segmentLength(_ % 3 <= 1, i) =? subject.segmentLength(_ % 3 <= 1, i)) :_*)
	}

	property("indexWhere") = forAll { subject :Ranking[Int] =>
		all(subject.indices.map { i =>
			(s"$i =? indexWhere(_==${subject(i)})" lbl_: i =? subject.indexWhere(_ == subject(i)))  &&
				(s"$i =? indexWhere(_==${subject(i)}, ${i-1})" lbl_: i =? subject.indexWhere(_ == subject(i), i - 1)) &&
				(s"$i =? indexWhere(_==${subject(i)}, -1)" lbl_: i =? subject.indexWhere(_ == subject(i), -1)) &&
				(s"$i =? indexWhere(_==${subject(i)}, $i)" lbl_: i =? subject.indexWhere(_ == subject(i), i)) &&
				(s"-1 =? indexWhere(_==${subject(i)}, ${i + 1})" lbl_: -1 =? subject.indexWhere(_ == subject(i), i + 1))
		} : _*) &&
			(s"indexWhere(_=>false)" lbl_: -1 =? subject.indexWhere(_ => false)) &&
			(s"indexWhere(_=>false, -1)" lbl_: -1 =? subject.indexWhere(_ => false, -1)) &&
			(s"indexWhere(_=>false, ${subject.length})" lbl_: -1 =? subject.indexWhere(_ => false, subject.length))
	}
	property("getIndexWhere") = forAll { subject :Ranking[Int] =>
		all(subject.indices.map { i =>
			(s"$i =? getIndexWhere(_==${subject(i)})" lbl_: Maybe(i) =? subject.getIndexWhere(_ == subject(i)))  &&
				(s"getIndexWhere(_==${subject(i)}, ${i-1})" lbl_: Maybe(i) =? subject.getIndexWhere(_ == subject(i), i - 1)) &&
				(s"getIndexWhere(_==${subject(i)}, -1)" lbl_: Maybe(i) =? subject.getIndexWhere(_ == subject(i), -1)) &&
				(s"getIndexWhere(_==${subject(i)}, $i)" lbl_: Maybe(i) =? subject.getIndexWhere(_ == subject(i), i)) &&
				(s"getIndexWhere(_==${subject(i)}, ${i + 1})" lbl_: 
					(No :Maybe[Int]) =? subject.getIndexWhere(_ == subject(i), i + 1))
		} : _*) &&
			(s"getIndexWhere(_=>false)" lbl_: (No :Maybe[Int]) =? subject.getIndexWhere(_ => false)) &&
			(s"getIndexWhere(_=>false, -1)" lbl_: (No :Maybe[Int]) =? subject.getIndexWhere(_ => false, -1)) &&
			(s"indexWhere(_=>false, ${subject.length})" lbl_:
				(No :Maybe[Int]) =? subject.getIndexWhere(_ => false, subject.length))
	}
	property("sureIndexWhere") = forAll { subject :Ranking[Int] =>
		all(subject.indices.map { i =>
			(s"$i =? sureIndexWhere(_==${subject(i)})" lbl_: i =? subject.sureIndexWhere(_ == subject(i)))  &&
				(s"$i =? sureIndexWhere(_==${subject(i)}, ${i-1})" lbl_: i =? subject.sureIndexWhere(_ == subject(i), i - 1))  &&
				(s"$i =? sureIndexWhere(_==${subject(i)}, -1)" lbl_: i =? subject.sureIndexWhere(_ == subject(i), -1)) &&
				(s"$i =? sureIndexWhere(_==${subject(i)}, $i)" lbl_: i =? subject.sureIndexWhere(_ == subject(i), i)) &&
				(s"sureIndexWhere(_==${subject(i)}, ${i + 1}) throws NoSuchElementException" lbl_: 
					subject.sureIndexWhere(_ == subject(i), i + 1).throws[NoSuchElementException])
		} : _*) &&
			(s"sureIndexWhere(_=>false) throws NoSuchElementException" lbl_:
				subject.sureIndexWhere(_ => false).throws[NoSuchElementException]) &&
			(s"sureIndexWhere(_=>false, -1) throws NoSuchElementException" lbl_:
				subject.sureIndexWhere(_ => false, -1).throws[NoSuchElementException]) &&
			(s"sureIndexWhere(_=>false, ${subject.length}) throws NoSuchElementException" lbl_:
				subject.sureIndexWhere(_ => false, subject.length).throws[NoSuchElementException])
	}

	property("lastIndexWhere") = forAll { subject :Ranking[Int] =>
		all(subject.indices.map { i =>
			(s"$i =? lastIndexWhere(_==${subject(i)})" lbl_: i =? subject.lastIndexWhere(_ == subject(i))) &&
				(s"$i =? lastIndexWhere(_==${subject(i)}, $i)" lbl_:
					i =? subject.lastIndexWhere(_ == subject(i), i)) &&
				(s"$i =? lastIndexWhere(_==${subject(i)}, ${i + 1})" lbl_: 
					i =? subject.lastIndexWhere(_ == subject(i), i + 1)) &&
				(s"$i =? lastIndexWhere(_==${subject(i)}, ${subject.length + 1})" lbl_:
					i =? subject.lastIndexWhere(_ == subject(i), subject.length + 1)) &&
				(s"-1 =? lastIndexWhere(_==${subject(i)}, ${i - 1})" lbl_:
					-1 =? subject.lastIndexWhere(_ == subject(i), i - 1))
		} : _*) &&
			(s"lastIndexWhere(_=>false)" lbl_: -1 =? subject.lastIndexWhere(_ => false)) &&
			(s"lastIndexWhere(_=>false, Int.MaxValue)" lbl_: -1 =? subject.lastIndexWhere(_ => false, Int.MaxValue)) &&
			(s"lastIndexWhere(_=>false, -1)" lbl_: -1 =? subject.lastIndexWhere(_ => false, -1))
	}
	property("getLastIndexWhere") = forAll { subject :Ranking[Int] =>
		all(subject.indices.map { i =>
			(s"$i =? getLastIndexWhere(_==${subject(i)})" lbl_:
				Maybe(i) =? subject.getLastIndexWhere(_ == subject(i))) &&
				(s"$i =? getLastIndexWhere(_==${subject(i)}, $i)" lbl_:
					Maybe(i) =? subject.getLastIndexWhere(_ == subject(i), i)) &&
				(s"$i =? getLastIndexWhere(_==${subject(i)}, ${i + 1})" lbl_: 
					Maybe(i) =? subject.getLastIndexWhere(_ == subject(i), i + 1)) &&
				(s"$i =? getLastIndexWhere(_==${subject(i)}, ${subject.length + 1})" lbl_:
					Maybe(i) =? subject.getLastIndexWhere(_ == subject(i), subject.length + 1)) &&
				(s"-1 =? getLastIndexWhere(_==${subject(i)}, ${i - 1})" lbl_:
					(No :Maybe[Int]) =? subject.getLastIndexWhere(_ == subject(i), i - 1))
		} : _*) &&
			(s"getLastIndexWhere(_=>false)" lbl_:
				(No :Maybe[Int]) =? subject.getLastIndexWhere(_ => false)) &&
			(s"getLastIndexWhere(_=>false, Int.MaxValue)" lbl_: 
				(No :Maybe[Int]) =? subject.getLastIndexWhere(_ => false, Int.MaxValue)) &&
			(s"getLastIndexWhere(_=>false, -1)" lbl_:
				(No :Maybe[Int]) =? subject.getLastIndexWhere(_ => false, -1))
	}
	property("sureLastIndexWhere") = forAll { subject :Ranking[Int] =>
		all(subject.indices.map { i =>
			(s"$i =? sureLastIndexWhere(_==${subject(i)})" lbl_: i =? subject.sureLastIndexWhere(_ == subject(i))) &&
				(s"$i =? sureLastIndexWhere(_==${subject(i)}, $i)" lbl_:
					i =? subject.sureLastIndexWhere(_ == subject(i), i)) &&
				(s"$i =? sureLastIndexWhere(_==${subject(i)}, ${i + 1})" lbl_:
					i =? subject.sureLastIndexWhere(_ == subject(i), i + 1)) &&
				(s"$i =? sureLastIndexWhere(_==${subject(i)}, ${subject.length + 1})" lbl_:
					i =? subject.sureLastIndexWhere(_ == subject(i), subject.length + 1)) &&
				(s"-1 =? sureLastIndexWhere(_==${subject(i)}, ${i - 1}) throws NoSuchElementException" lbl_:
					subject.sureLastIndexWhere(_ == subject(i), i - 1).throws[NoSuchElementException])
		} : _*) &&
			(s"sureLastIndexWhere(_=>false) throws NoSuchElementException" lbl_:
				subject.sureLastIndexWhere(_ => false).throws[NoSuchElementException]) &&
			(s"sureLastIndexWhere(_=>false, Int.MaxValue) throws NoSuchElementException" lbl_:
				subject.sureLastIndexWhere(_ => false, Int.MaxValue).throws[NoSuchElementException]) &&
			(s"sureLastIndexWhere(_=>false, -1) throws NoSuchElementException" lbl_:
				subject.sureLastIndexWhere(_ => false, -1).throws[NoSuchElementException])
	}
	
	property("indexOf") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		val seq = expect.toSeq
		forAll { (x :Int) =>
			seq.indexOf(x) =? subject.indexOf(x) lbl
				seq.toString + ".indexOf(" + x + ")" lbl subject.toString + " :" + subject.className
		}
	}
	property("getIndexOf") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		val seq = expect.toSeq
		forAll { (x :Int) =>
			Maybe(seq.indexOf(x)).filter(_ >= 0) =? subject.getIndexOf(x) lbl
				seq.toString + ".getIndexOf(" + x + ")" lbl subject.toString + " :" + subject.className
		}
	}
	property("sureIndexOf") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		val seq = expect.toSeq
		forAll { (x :Int) =>
			seq.indexOf(x) match {
				case -1 => Prop(throws(classOf[NoSuchElementException])(subject.sureIndexOf(x)))
				case  n => subject.sureIndexOf(x) ?= n
			}
		}
	}

	property("indexOfSlice") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		val indexed = expect.toVector //Seq.indexOfSlice is buggy in 2.13.10
		all(
			(for {
				from <- indexed.indices
				until <- from to indexed.length
			} yield {
				val slice = indexed.slice(from, until)
				val i = indexed.indexOfSlice(slice) //may be 0 if slice.isEmpty
				((subject.indexOfSlice(slice) ?= i) :| slice.toString + "@" + i) &&
					((subject.indexOfSlice(slice, from) ?= from) :| "[" + from + ", " + until + ")@" + from)
			})
		:_*) && forAll { (x :Seq[Int], i :Short) =>
			subject.indexOfSlice(x, i & 0xffff) ?= indexed.indexOfSlice(x, i & 0xffff)
		}
	}
	property("getIndexOfSlice") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		val indexed = expect.toVector //Seq.indexOfSlice is buggy in 2.13.10
		all(
			(for {
				from <- indexed.indices
				until <- from to indexed.length
			} yield {
				val slice = indexed.slice(from, until)
				val i = indexed.indexOfSlice(slice)
				((subject.getIndexOfSlice(slice) ?= Yes(i)) :| slice.toString + "@" + i) &&
					((subject.getIndexOfSlice(slice, from) ?= Yes(from)) :| "[" + from + ", " + until + ")@" + from)
			})
		:_*) && forAll { (x :Seq[Int], i :Short) =>
			subject.getIndexOfSlice(x, i & 0xffff) ?= Maybe(indexed.indexOfSlice(x, i & 0xffff)).filter(_ >= 0)
		}
	}
	property("sureIndexOfSlice") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		val indexed = expect.toVector //List.indexOfSlice is buggy in 2.13.10
		def prop(expect :Int, from :Int = 0)(result: => Int) =
			if (expect < from) Prop(throws(classOf[NoSuchElementException])(result))
			else result ?= expect
		all(
			(for {
				from <- indexed.indices
				until <- from to indexed.length
			} yield {
				val slice = indexed.slice(from, until)
				val i = indexed.indexOfSlice(slice)
				(prop(i)(subject.sureIndexOfSlice(slice)) :| slice.toString + "@" + i) &&
					(prop(from)(subject.sureIndexOfSlice(slice, from)) :| "[" + from + ", " + until + ")@" + from)
			})
		:_*) && forAll { (x :Seq[Int], i :Int) =>
			prop(indexed.indexOfSlice(x, i & 0xffff))(subject.sureIndexOfSlice(x, i & 0xffff))
		}
	}

	property("contains") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		val seq = expect.toSeq
		forAll { x :Int => seq.contains(x) =? subject.contains(x) }
	}
	property("containsAll") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		val set = expect.toSet
		val sub = expect.toSeq.filter(_ => Boolean.random).shuffled
		Prop(subject.containsAll(sub)) && forAll { items :Seq[Int] =>
			items.forall(set) =? subject.containsAll(items)
		}
	}


	property("swapped(x, y)") = forAll { (subject :Ranking[Int], i :Int, j :Int) =>
		if (i < 0 | j < 0 || i >= subject.length || j >= subject.length)
			subject.swapped(i, j).throws[IndexOutOfBoundsException]
		else
			subject.toIndexedSeq.updated(i, subject(j)).updated(j, subject(i)) =? subject.swapped(i, j).toIndexedSeq
	}
	property("swapped(x, y, len)") = forAll { (subject :Ranking[Int], i :Int, j :Int, len :Int) =>
		val length = len max 0
		if (i < 0 | j < 0 || i > subject.length - length || j > subject.length - length)
			subject.swapped(i, j, len).throws[IndexOutOfBoundsException]
		else if (i == j || length == 0)
			subject =? subject.swapped(i, j, len)
		else if (if (i <= j) i + length > j else j + length > i)
			subject.swapped(i, j, len).throws[IllegalArgumentException]
		else {
			val lo = i min j
			val hi = i max j
			val expect = subject.iterator.take(lo) ++
				subject.iterator.slice(hi, hi + len) ++
				subject.iterator.slice(lo + len, hi) ++
				subject.iterator.slice(lo, lo + len) ++
				subject.iterator.drop(hi + len) to Seq
			validate(expect, subject.swapped(i, j, len))
		}
	}

	property("moved(x, y)") = forAll { (subject :Ranking[Int], i :Int, j :Int) =>
		if (i < 0 | j < 0 || i >= subject.length || j >= subject.length)
			subject.moved(i, j).throws[IndexOutOfBoundsException]
		else {
			val removed = (subject.iterator.take(i) ++ subject.iterator.drop(i + 1)).toSeq
			val expect = removed.take(j) :+ subject(i) :++ removed.iterator.drop(j)
			validate(expect, subject.moved(i, j))
		}
	}
	property("moved(x, y, len)") = forAll { (subject :Ranking[Int], i :Int, j :Int, len :Int) =>
		val length = len max 0
		if (i < 0 | j < 0 || i > subject.length - length || j > subject.length - length)
			subject.moved(i, j, len).throws[IndexOutOfBoundsException]
		else if (i == j | length == 0)
			subject =? subject.moved(i, j, len)
		else {
			val removed = subject.iterator.take(i) ++ subject.iterator.drop(i + length) to Seq
			val expect = removed.iterator.take(j) ++ subject.iterator.slice(i, i + length) ++ removed.iterator.drop(j)
			validate(expect.toSeq, subject.moved(i, j, len))
		}
	}

	property("rotatedLeft") = forAll { (subject :Ranking[Int], shift :Int) =>
		val seq = subject.toSeq
		implicit val arbitraryInt = Arbitrary(Gen.choose(-2, subject.length + 2))
		forAll { (from :Int, until :Int) =>
			val from0 = from max 0 min subject.length
			val until0 = until max from0 min subject.length
			val expect =
				if (from0 == until0)
					seq
				else {
					val n = shift % (until0 - from0)
					val rotated =
						if (shift >= 0)
							seq.slice(from0 + n, until0) :++ seq.slice(from0, from0 + n)
						else
							seq.slice(until0 + n, until0) :++ seq.slice(from0, until0 + n)
					seq.take(from0) :++ rotated :++ seq.drop(until0)
				}
			validate(expect, subject.rotatedLeft(from, until)(shift))
		}
	}
	property("rotatedRight") = forAll { (subject :Ranking[Int], shift :Int) =>
		val seq = subject.toSeq
		implicit val arbitraryInt = Arbitrary(Gen.choose(-2, subject.length + 2))
		forAll { (from :Int, until :Int) =>
			val from0 = from max 0 min subject.length
			val until0 = until max from0 min subject.length
			val expect =
				if (from0 == until0)
					seq
				else {
					val n = shift % (until0 - from0)
					val rotated =
						if (shift >= 0)
							seq.slice(until0 - n, until0) :++ seq.slice(from0, until0 - n)
						else
							seq.slice(from0 - n, until0) :++ seq.slice(from0, from0 - n)
					seq.take(from0) :++ rotated :++ seq.drop(until0)
				}
			validate(expect, subject.rotatedRight(from, until)(shift))
		}
	}
	//todo: test patch

	property("updated") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		all((0 until expect.size).map { i =>
			val elem = Int.random
			val reference = expect.toSeq.updated(i, elem)
			val result = subject.updated(i, elem)
			validate(reference, result)
		} :_*)
	}
	property("updatedAll") = test { (input :Iterable[Int], subject :Ranking[Int]) =>
		forAll { (index :Int, patch :List[Int]) =>
			val set    = new mutable.HashSet[Int]
			val unique = patch.toSeq.collect { case x if set.add(x) => x }
			val diff   = input.toSeq.filterNot(set.contains)
			val expect = diff.take(index) :++ unique :++ diff.takeRight(input.size - index - unique.size)

			def update(elems :IterableOnce[Int]) =
				if (index < 0 || index > input.size || expect.size > input.size)
					subject.updatedAll(index, elems).throws[IndexOutOfBoundsException]
				else
					validate(expect, subject.updatedAll(index, elems))
			(s"updatedAll($index, List)" lbl_: update(patch)) &&
				(s"updatedAll($index, Vector)" lbl_: update(patch.toVector)) &&
				(s"updatedAll($index, Set)" lbl_: update(new SeqSet(set.toSet, unique))) &&
				(s"updatedAll($index, Ranking)" lbl_: update(Ranking.from(unique))) &&
				(s"updatedAll($index, Iterator)" lbl_: update(patch.iterator)) &&
				(s"updatedAll($index, VectorIterator)" lbl_: update(patch.toVector.iterator))
		}
	}

	property("replace") = forAll { (subject :Ranking[Int], i :Int, elem :Int) =>
		if (i < 0 || i >= subject.length)
			subject.replace(i, elem).throws[IndexOutOfBoundsException]
		else {
			val prefix = subject.toSeq.take(i).filterNot(_ == elem)
			val suffix = subject.toSeq.drop(i + 1).filterNot(_ == elem)
			validate(prefix :+ elem :++ suffix, subject.replace(i, elem)) lbl
				subject.mkString(subject.getClass.getName + "(", ", ", ")")
		}
	}
	property("replaceAll") = test { (input :Iterable[Int], subject :Ranking[Int]) =>
		forAll { (index :Int, elems :List[Int]) =>
			val set      = new mutable.HashSet[Int]
			val patch    = elems.toSeq.collect { case x if set.add(x) => x }
			val distinct = input.toSeq.distinct
//			val diff   = input.toSeq.filterNot(set.contains)
			val prefix = distinct.take(index).filterNot(set)
			val suffix = distinct.drop(index + set.size).filterNot(set)
			val expect = prefix :++ patch :++ suffix
			def replace(elems :IterableOnce[Int]) =
				if (index < 0 || index > input.size || expect.size > input.size)
					subject.replaceAll(index, elems).throws[IndexOutOfBoundsException]
				else
					validate(expect, subject.replaceAll(index, elems))
			(s"replaceAll($index, List)" lbl_: replace(elems)) &&
				(s"replaceAll($index, Vector)" lbl_: replace(elems.toVector)) &&
				(s"replaceAll($index, Set)" lbl_: replace(new SeqSet(set.toSet, patch))) &&
				(s"replaceAll($index, Ranking)" lbl_: replace(Ranking.from(elems))) &&
				(s"replaceAll($index, Iterator)" lbl_: replace(elems.iterator)) &&
				(s"replaceAll($index, VectorIterator)" lbl_: replace(elems.toVector.iterator))
		}
	}

	property("inserted") = forAll { (subject :Ranking[Int], i :Int, elem :Int) =>
		val filtered = subject.toSeq.filterNot(_ == elem)
		validate(filtered.take(i) :+ elem :++ filtered.drop(i), subject.inserted(i, elem))
	}
	property("insertedAll") = forAll { (subject :Ranking[Int], i :Int, elems :List[Int]) =>
		val set = new SeqSet(elems)
		val diff = subject.toSeq.filterNot(set.contains)
		val expect = diff.take(i) :++ set :++ diff.drop(i)
		(s"insertAll($i, List)" lbl_: validate(expect, subject.insertedAll(i, elems))) &&
			(s"insertedAll($i, Vector)" lbl_: validate(expect, subject.insertedAll(i, elems.toVector))) &&
			(s"insertedAll($i, $set)" lbl_: validate(expect, subject.insertedAll(i, set))) &&
			(s"insertedAll($i, Ranking)" lbl_: validate(expect, subject.insertedAll(i, Ranking.from(set)))) &&
			(s"insertedAll($i, Iterator)" lbl_: validate(expect, subject.insertedAll(i, elems.iterator))) &&
			(s"insertedAll($i, VectorIterator)" lbl_: validate(expect, subject.insertedAll(i, elems.toVector.iterator)))
	}
	//todo: test patch


	property("+|") = forAll { (subject :Ranking[Int], x :Int) =>
		validateAnyOrder(subject.toSeq :+ x, subject +| x)
	}
	property("+") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		forAll { x :Int => expect.toSet + x =? (subject + x).toSet }
	}
	property(":+") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		forAll { x :Int => validateAppend(expect.toSeq.distinct.filterNot(_ == x) :+ x, subject :+ x) }
	}
	property("+:") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		forAll { x :Int => validate(x +: expect.toSeq.distinct.filterNot(_ == x), x +: subject) }
	}

	property(":++(List))") = forAll { (prefix :Ranking[Int], suffix :List[Int]) =>
		validateAppend(prefix.toSeq :++ suffix, prefix :++ suffix)
	}
	property(":++(Vector))") = forAll { (prefix :Ranking[Int], suffix :Vector[Int]) =>
		validateAppend(prefix.toSeq :++ suffix, prefix :++ suffix)
	}
	property(":++(Set))") = forAll { (prefix :Ranking[Int], suffix :Set[Int]) =>
		validateAppend(prefix.toSeq :++ suffix, prefix :++ suffix)
	}
	property(":++(Ranking)") = forAll { (prefix :Ranking[Int], suffix :Ranking[Int]) =>
		validateAppend(List.from(prefix).filterNot(suffix.contains) ++ suffix, prefix :++ suffix)
	}

	property("++:(List))") = forAll { (prefix :List[Int], suffix :Ranking[Int]) =>
		validate(prefix.distinct ++ suffix.toSeq.diff(prefix), prefix ++: suffix)
	}
	property("++:(Vector))") = forAll { (prefix :Vector[Int], suffix :Ranking[Int]) =>
		validate(prefix.distinct ++ suffix.toSeq.diff(prefix), prefix ++: suffix)
	}
	property("++:(Seq))") = forAll { (prefix :Set[Int], suffix :Ranking[Int]) =>
		validate(prefix ++: suffix.toSeq.filterNot(prefix), prefix ++: suffix)
	}
	property("++:(Ranking)") = forAll { (prefix :Ranking[String], suffix :Ranking[Int]) =>
		compare(prefix ++: suffix.toSeq.filterNot(prefix.contains), prefix ++: suffix)
	}

	property("++(List))") = forAll { (prefix :Ranking[Int], suffix :List[Int]) =>
		validate(prefix.toSeq :++ suffix, prefix ++ suffix)
	}
	property("++(Vector))") = forAll { (prefix :Ranking[Int], suffix :Vector[Int]) =>
		validate(prefix.toSeq :++ suffix, prefix ++ suffix)
	}
	property("++(Set))") = forAll { (prefix :Ranking[Int], suffix :Set[Int]) =>
		validate(prefix.toSeq :++ suffix, prefix ++ suffix)
	}
	property("++(Ranking)") = forAll { (prefix :Ranking[Int], suffix :Ranking[Int]) =>
		validate(prefix.toSeq :++ suffix, prefix ++ suffix)
//		Set.from(prefix) ++ Set.from(suffix) =? (prefix ++ suffix).toSet
	}

	property("+|+(List))") = forAll { (prefix :Ranking[Int], suffix :List[Int]) =>
		validateAnyOrder(prefix.toSeq :++ suffix, prefix +|+ suffix)
	}
	property("+|+(Vector))") = forAll { (prefix :Ranking[Int], suffix :Vector[Int]) =>
		validateAnyOrder(prefix.toSeq :++ suffix, prefix +|+ suffix)
	}
	property("+|+(Set))") = forAll { (prefix :Ranking[Int], suffix :Set[Int]) =>
		validateAnyOrder(prefix.toSeq :++ suffix, prefix +|+ suffix)
	}
	property("+|+(Ranking)") = forAll { (prefix :Ranking[Int], suffix :Ranking[Int]) =>
		validateAnyOrder(prefix.toSeq :++ suffix, prefix +|+ suffix)
	}


	property("-") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		forAll { x :Int => compare(expect.filterNot(_ == x), subject - x) }
	}
	property("--(List)") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		forAll { xs :Seq[Int] => validate(expect.toList diff xs, subject -- xs) }
	}
	property("--(Vector)") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		forAll { xs :Seq[Int] => validate(expect.toVector diff xs, subject -- xs) }
	}
	property("--(Set)") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		forAll { xs :Set[Int] => validate(expect.toSeq.filterNot(xs), subject -- xs) }
	}
	property("--(Ranking)") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		forAll { xs :Ranking[Int] => validate(expect.toSeq.filterNot(xs.contains), subject -- xs) }
	}

	property("&(Set)") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		forAll { xs :collection.Set[Int] =>
			val result = subject & xs
			expect.toSeq.filter(xs.contains).toSet =? result.toSet && all(props(result.toSeq, result) :_*)
		}
	}
	property("&(Ranking)") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		forAll { xs :Ranking[Int] =>
			val result = subject & xs
			expect.toSeq.filter(xs.contains).toSet =? result.toSet && all(props(result.toSeq, result) :_*)
		}
	}

	property(":&(Set)") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		forAll { xs :collection.Set[Int] => validate(expect.toSeq.filter(xs.contains), subject :& xs) }
	}
	property(":&(Ranking)") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		forAll { xs :Ranking[Int] => validate(expect.toSeq.filter(xs.contains), subject :& xs) }
	}

	property("intersectionSize(Ranking)") = forAll { (rank1 :Ranking[Int], rank2 :Ranking[Int]) =>
		(rank1.toSet & rank2.toSet).size =? rank1.intersectionSize(rank2)
	}
	property("intersectionSize(Set)") = forAll { (rank :Ranking[Int], set :Set[Int]) =>
		(rank.toSet & set).size =? rank.intersectionSize(set)
	}
	property("disjoint(Ranking)") = forAll { (rank1 :Ranking[Int], rank2 :Ranking[Int]) =>
		(rank1.toSet & rank2.toSet).isEmpty =? rank1.disjoint(rank2)
	}
	property("disjoint(Set)") = forAll { (rank :Ranking[Int], set :Set[Int]) =>
		(rank.toSet & set).isEmpty =? rank.disjoint(set)
	}

	property("reorder") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		val permutation = Permutation.random(subject.size)
		compare(permutation(expect.toSeq), subject.reorder(permutation))
	}
	property("reverse") = test {
		(expect :Iterable[Int], subject :Ranking[Int]) => subject.reverse ?= expect.toList.reverse.to(Ranking)
	}
	property("reverseIterator") = test {
		(expect :Iterable[Int], subject :Ranking[Int]) => checkIterator(expect.toList.reverse, subject.reverseIterator)
	}
	property("iterator") = test {
		(expect :Iterable[Int], subject :Ranking[Int]) => checkIterator(expect.toList, subject.iterator)
	}

	//todo: test copyRangeToArray, cyclicCopyRangeToArray
	//todo: test unorderedBuilder and appendingBuilder
}
