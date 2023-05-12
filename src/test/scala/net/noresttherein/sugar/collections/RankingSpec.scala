package net.noresttherein.sugar.collections

import scala.collection.IterableFactory
import scala.reflect.ClassTag

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.{all, forAll, throws, AnyOperators}
import net.noresttherein.sugar.collections.IterableProps.{Dummy, Filter, FlatMap, Fold, FoldSide, Map}
import net.noresttherein.sugar.extensions.{classNameMethods, SeqExtension}
import net.noresttherein.sugar.numeric.globalRandom
import net.noresttherein.sugar.numeric.extensions.{BooleanObjectExtension, IntObjectExtension}
import net.noresttherein.sugar.testing.scalacheck.extensions.{BooleanExtension, PropExtension}
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.Got


object RankingSpec
	extends IterableProps[Ranking, Iterable]("Ranking") with OrderedProps[Ranking, Iterable, Dummy]
{
	//todo: appending/prepending/adding other Rankings and Sets

	protected override def referenceFactory :IterableFactory[Iterable] = Seq
	protected override def checkedFactory :IterableFactory[Ranking] = Ranking

//	private def dedup[T](expect :Iterable[T]) :Iterable[T] =
//		expect.flatMapWith(Set.empty[T]) { (t, dups) => (if (dups(t)) Nil else t::Nil, dups + t) }

	private def aligned[T](expect :List[T], result :Ranking[T], i :Int) :Boolean = expect match {
		case h::t if i < result.size && h == result(i) => aligned(t, result, i + 1)
		case _::t if i < result.size                   => aligned(t, result, i)
		case h::t if result.contains(h)                => aligned(t, result, i)
		case Nil => true
		case _   => false
	}
	protected override def compare[T :Dummy](expect :Iterable[T], result :Iterable[T]) :Prop =
		aligned(expect.toList, result to Ranking, 0) lbl "Expected: " + expect + ";\n     got: " + result

	protected override def compare[T :Arbitrary :Dummy, X](f :Iterable[T] => X) :Prop =
		forAll { elems :Seq[T] => f(elems.toSeq.distinct) =? f(elems to C) }

	protected override def test[T :Arbitrary :Dummy](check :(Iterable[T], Ranking[T]) => Prop) :Prop =
		forAll { elems :Seq[T] => check(elems.toSeq.distinct, Ranking.from(elems)) }

	protected override def test[T, X, F, M, FM]
	                           (f :Iterable[T] => Iterable[X])
	                           (implicit input :Arbitrary[T], output :Arbitrary[X], ev1 :Dummy[T], ev2 :Dummy[X],
	                            tag :ClassTag[X], filt :Filter[X], fldA :FoldSide[F, X], evf :Dummy[F], fld :Fold[X],
	                            mp :Map[X, M], evm :Dummy[M], fmap :FlatMap[X, FM], evfm :Dummy[FM]) :Prop =
		forAll { elems :Seq[T] => validate(f(elems.toSeq.distinct), f(elems to C) to C) }

	protected override def validate[T, F, M, FM](label: => String, expect :Iterable[T], result :Iterable[T])
	                                            (implicit arbitrary :Arbitrary[T], ev :Dummy[T], tag :ClassTag[T],
	                                             filt :Filter[T], fldA :FoldSide[F, T], evf :Dummy[F], fld :Fold[T],
	                                             mp :Map[T, M], evm :Dummy[M], fmap :FlatMap[T, FM], evfm :Dummy[FM]) :Prop =
		label @: aligned(expect.toList, result to Ranking, 0) && props(result.toSeq, result)

//
//	protected override def props[T, F, M, FM](expect :Iterable[T], result :Iterable[T])
//	                                         (implicit arbitrary :Arbitrary[T], ev :Dummy[T], tag :ClassTag[T],
//	                                          filt :Filter[T], fldA :FoldSide[F, T], evf :Dummy[F],
//	                                          fld :Fold[T], mp :IterableProps.Map[T, M], evm :Dummy[M],
//	                                          fmap :FlatMap[T, FM], evfm :Dummy[FM]) :Prop =
//		super.props(expect.toSeq.distinct, result)

	override def knowsSize = true
	override def hasOrder  = true

	property("length") = test { (expect :Iterable[Int], subject :Ranking[Int]) => expect.size =? subject.length }
	property("apply") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		val seq = expect.toSeq
		all(seq.indices.map(i => seq(i) =? subject(i)) :_*)
	}
	property("reverse") = test {
		(expect :Iterable[Int], subject :Ranking[Int]) => subject.reverse ?= expect.toList.reverse.to(Ranking)
	}
	property("reverseIterator") = test {
		(expect :Iterable[Int], subject :Ranking[Int]) => checkIterator(expect.toList.reverse, subject.reverseIterator)
	}
	property("startsWith") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		val seq = expect.toSeq
		all(
			(for {
				from <- 0 to seq.length
				until <- from to seq.length
				slice = seq.slice(from, until)
			} yield
				Prop(subject.startsWith(slice, from)) lbl slice.toString + "@" + from)
		:_*) && forAll { (subseq :Seq[Int], offset :Int) =>
			(offset <= seq.length && seq.startsWith(subseq, offset)) =? subject.startsWith(subseq, offset)
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
			Opt(seq.indexOf(x)).filter(_ >= 0) =? subject.getIndexOf(x) lbl
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
	property("indexWhere") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		val seq = expect.toSeq
		forAll { (x :Int, i :Int) =>
			seq.indexOf(x, i) =? subject.indexWhere(_ == x, i)
		}
	}
	property("getIndexWhere") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		val seq = expect.toSeq
		forAll { (x :Int, i :Int) =>
			Opt(seq.indexOf(x, i)).filter(_ >= 0) =? subject.getIndexWhere(_ == x, i)
		}
	}
	property("sureIndexWhere") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		val seq = expect.toSeq
		forAll { (x :Int, i :Int) =>
			seq.indexOf(x, i) match {
				case -1 => Prop(throws(classOf[NoSuchElementException])(subject.sureIndexWhere(_ == x, i)))
				case  n => subject.sureIndexWhere(_ == x, i) ?= n
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
				((subject.getIndexOfSlice(slice) ?= Got(i)) :| slice.toString + "@" + i) &&
					((subject.getIndexOfSlice(slice, from) ?= Got(from)) :| "[" + from + ", " + until + ")@" + from)
			})
		:_*) && forAll { (x :Seq[Int], i :Short) =>
			subject.getIndexOfSlice(x, i & 0xffff) ?= Opt(indexed.indexOfSlice(x, i & 0xffff)).filter(_ >= 0)
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
		val sub = expect.toSeq.filter(_ => Boolean.random).shuffle
		Prop(subject.containsAll(sub)) && forAll { items :Seq[Int] =>
			items.forall(set) =? subject.containsAll(items)
		}
	}


	property("updated") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		all((0 until expect.size).map { i =>
			val elem = Int.random
			val reference = expect.toSeq.updated(i, elem)
			val result = subject.updated(i, elem)
			result sameElements reference lbl reference.toString + " =? " + result
		} :_*)
	}
	property("reorder") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		val permutation = Permutation.random(subject.size)
		compare(permutation(expect.toSeq), subject.reorder(permutation))
	}

	property("+") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		forAll { x :Int => expect.toSet + x =? (subject + x).toSet }
	}
	property(":+") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		forAll { x :Int => validate(expect.toSeq.filterNot(_ == x) :+ x, subject :+ x) }
	}
	property("+:") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		forAll { x :Int => validate(x +: expect.toSeq.filterNot(_ == x), x +: subject) }
	}

	property(":++(Seq))") = forAll { (prefix :Ranking[Int], suffix :Seq[Int]) =>
		validate(List.from(prefix).diff(suffix) ++ suffix.reverse.distinct.reverse, prefix :++ suffix)
	}
	property(":++(Set))") = forAll { (prefix :Ranking[Int], suffix :Set[Int]) =>
		validate(prefix.view.filterNot(suffix) ++ suffix, prefix :++ suffix)
	}
	property(":++(Ranking)") = forAll { (prefix :Ranking[Int], suffix :Ranking[Int]) =>
		compare(List.from(prefix).filterNot(suffix.contains) ++ suffix, prefix :++ suffix)
	}

	property("++:(Seq))") = forAll { (prefix :Seq[Int], suffix :Ranking[Int]) =>
		validate(prefix.distinct ++ suffix.toSeq.diff(prefix), prefix ++: suffix)
	}
	property("++:(Seq))") = forAll { (prefix :Set[Int], suffix :Ranking[Int]) =>
		validate(prefix ++: suffix.toSeq.filterNot(prefix), prefix ++: suffix)
	}
	property(":++(Ranking)") = forAll { (prefix :Ranking[String], suffix :Ranking[Int]) =>
		compare(prefix ++: suffix.toSeq.filterNot(prefix.contains), prefix ++: suffix)
	}

	property("++(Seq))") = forAll { (prefix :Ranking[Int], suffix :Seq[Int]) =>
		val set = suffix.toSet
		Set.from(prefix) ++ set =? (prefix ++ suffix).toSet
	}
	property("++(Set))") = forAll { (prefix :Ranking[Int], suffix :Set[Int]) =>
		suffix ++ prefix =? (prefix ++ suffix).toSet
	}
	property("++(Ranking)") = forAll { (prefix :Ranking[Int], suffix :Ranking[Int]) =>
		Set.from(prefix) ++ Set.from(suffix) =? (prefix ++ suffix).toSet
	}

	property("-") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		forAll { x :Int => compare(expect.filterNot(_ == x), subject - x) }
	}
	property("--(Seq)") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		forAll { xs :Seq[Int] => validate(expect.toSeq diff xs, subject -- xs) }
	}
	property("--(Set)") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		forAll { xs :Set[Int] => validate(expect.toSeq.filterNot(xs), subject -- xs) }
	}
	property("--(Ranking)") = test { (expect :Iterable[Int], subject :Ranking[Int]) =>
		forAll { xs :Ranking[Int] => validate(expect.toSeq.filterNot(xs.contains), subject -- xs) }
	}

}
