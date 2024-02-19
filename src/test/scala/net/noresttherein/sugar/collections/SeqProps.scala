package net.noresttherein.sugar.collections

import scala.collection.{IterableFactory, View, immutable}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.Builder
import scala.reflect.ClassTag

import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Shrink, Test}
import org.scalacheck.Prop.{AnyOperators, all, forAll}
import org.scalacheck.commands.Commands
import org.scalacheck.util.{Buildable, ConsoleReporter, Pretty}
import net.noresttherein.sugar.collections.IterableProps.{Dummy, Filter, FlatMap, Fold, FoldSide, Map}
import net.noresttherein.sugar.collections.extensions.IterableExtension
import net.noresttherein.sugar.extensions.{FactoryExtension, classNameMethods}
import net.noresttherein.sugar.numeric
import net.noresttherein.sugar.numeric.extensions.IntCompanionExtension
import net.noresttherein.sugar.testing.scalacheck.extensions.{LazyExtension, PropExtension}




trait SeqProps[C[T] <: collection.Seq[T], E[_]] extends OrderedProps[C, collection.Seq, E] {
	import scala.collection.Seq

	protected implicit def anyEvidence :E[Any]

	implicit def buildableRelayArray[T] :Buildable[T, RelayArray[T]] = new Buildable[T, RelayArray[T]] {
		override def builder :Builder[T, RelayArray[T]] = RelayArray.newBuilder[T]
	}

	property("length") = forAllChecked { (expect :Seq[Int], subject :C[Int]) => expect.length =? subject.length }
	property("apply") = forAllChecked { (expect :Seq[Int], subject :C[Int]) =>
		all(expect.indices.map(i => expect(i) =? subject(i)) :_*)
	}
	property("distinct") = test((_ :Seq[Int]).distinct)
	property("reverse") = test((_ :Seq[Int]).reverse)
	property("reverseIterator") = forAllChecked { (expect :Seq[Int], subject :C[Int]) =>
		checkIterator(expect.reverse, subject.reverseIterator)
	}
	property("startsWith") = forAllChecked { (expect :Seq[Int], subject :C[Int]) =>
		all(
			(for {
				from <- 0 to expect.length
				until <- from to expect.length
			} yield Prop(subject.startsWith(expect.slice(from, until), from)) lbl s"startsWith(slice($from, $until), $from)")
		:_*) && forAll { (seq :Seq[Int], offset :Int) =>
			if (offset < 0 || offset > expect.length) Prop(!subject.startsWith(seq, offset))
			else expect.startsWith(seq, offset) =? subject.startsWith(seq, offset)
		}
	}
	property("endsWith") = forAllChecked { (expect :Seq[Int], subject :C[Int]) =>
		all((0 to expect.length).map(i => Prop(subject.endsWith(expect.drop(i))) lbl s"endsWith(${expect.drop(i)})") :_*)
	}
//	property("padTo") = forAll { (length :Short, elem :Int) => test((_ :Seq[Int]).padTo(length, elem)) }
	property("padTo") = forAllChecked { (expect :Seq[Int], subject :C[Int]) =>
		forAll { (length :Short, elem :Int) => expect.padTo(length, elem) =? subject.padTo(length, elem) }
	}
	property("segmentLength") = forAllChecked { (expect :Seq[Int], subject :C[Int]) =>
		all(expect.indices.map { i =>
			(expect.segmentLength(_ % 3 <= 1, i) =? subject.segmentLength(_ % 3 <= 1, i)) label "from " + i
		} :_*)
	}


	property("indexOf") = forAllChecked { (expect :Seq[Int], subject :C[Int]) =>
		forAll { (x :Int, i :Int) =>
			expect.indexOf(x, i) =? subject.indexOf(x, i)
		} && all(expect.view.mapWithIndex { (x, i) =>
			(expect.indexOf(x) =? subject.indexOf(x)) :| (x.toString + "@" + expect.indexOf(x)) &&
				(subject.indexOf(x, i) ?= i) :| s"#$i->$x"
		}.toSeq :_*)
	}
	property("lastIndexOf") = forAllChecked { (expect :Seq[Int], subject :C[Int]) =>
		forAll { (x :Int, i :Int) =>
			expect.lastIndexOf(x, i) =? subject.lastIndexOf(x, i)
		} && all(expect.view.mapWithIndex { (x, i) =>
			(expect.lastIndexOf(x) =? subject.lastIndexOf(x)) :| (x.toString + "@" + expect.lastIndexOf(x)) &&
				(subject.lastIndexOf(x, i) ?= i) :| s"#$i->$x"
		}.toSeq :_*)
	}
	property("indexWhere") = forAllChecked { (expect :Seq[Int], subject :C[Int]) =>
		forAll { (x :Int, i :Int) =>
			expect.indexOf(x, i) =? subject.indexWhere(_ == x, i)
		} && all(expect.view.mapWithIndex { (x, i) =>
			(expect.indexOf(x) =? subject.indexWhere(_ == x)) :| (x.toString + "@" + expect.indexOf(x)) &&
				(subject.indexWhere(_ == x, i) ?= i) :| s"#$i->$x"
		}.toSeq :_*)
	}
	property("lastIndexWhere") = forAllChecked { (expect :Seq[Int], subject :C[Int]) =>
		forAll { (x :Int, i :Int) =>
			expect.lastIndexOf(x, i) =? subject.lastIndexWhere(_ == x, i)
		} && all(expect.view.mapWithIndex { (x, i) =>
			(expect.lastIndexOf(x) =? subject.lastIndexWhere(_ == x)) :| (x.toString + "@" + expect.lastIndexOf(x)) &&
				(subject.lastIndexWhere(_ == x, i) ?= i) :| s"$x<-#$i"
		}.toSeq :_*)
	}
	property("indexOfSlice") = forAllChecked { (expect :Seq[Int], subject :C[Int]) =>
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
	property("lastIndexOfSlice") = forAllChecked { (expect :Seq[Int], subject :C[Int]) =>
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

	property("updated") = forAllChecked { (expect :Seq[Int], subject :C[Int]) =>
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

	property("appended(Int)") = forAllChecked { (expect :Seq[Int], subject :C[Int]) =>
		forAll { x :Int => test(expect :+ x, subject :+ x) }
	}
	property("appended(String)") = forAllChecked { (expect :Seq[Int], subject :C[Int]) =>
		forAll { x :String => compare(expect :+ x, subject :+ x) }
	}

	property("prepended(Int)") = forAllChecked { (expect :Seq[Int], subject :C[Int]) =>
		forAll { x :Int => test(x +: expect, x +: subject) }
	}
	property("prepended(String)") = forAllChecked { (expect :Seq[Int], subject :C[Int]) =>
		forAll { x :String => compare(x +: expect, x +: subject) }
	}

	property("appendedAll(List[Int])") = forAll { (prefix :C[Int], suffix :List[Int]) =>
		test(Vector.from(prefix) ++ suffix, (prefix :++ suffix) to C)
	}
	property("appendedAll(List[String])") = forAll { (prefix :C[Int], suffix :List[String]) =>
		compare(Vector.from(prefix) ++ suffix :Seq[Any], (prefix :++ suffix) to C)
	}
	property("appendedAll(ArraySeq[Int])") = forAll { (prefix :C[Int], suffix :ArraySeq[Int]) =>
		test(Vector.from(prefix) ++ suffix, (prefix :++ suffix) to C)
	}
	property("appendedAll(ArraySeq[String])") = forAll { (prefix :C[Int], suffix :ArraySeq[String]) =>
		compare(Vector.from(prefix) ++ suffix :Seq[Any], (prefix :++ suffix) to C)
	}
	property("appendedAll(Vector[Int]))") = forAll { (prefix :C[Int], suffix :Vector[Int]) =>
		test(Vector.from(prefix) ++ suffix, (prefix :++ suffix) to C)
	}
	property("appendedAll(RelayArray[Int].slice)") = forAll { (prefix :C[Int], suffix :RelayArray[Int]) =>
		test(
			Vector.from(prefix) ++ suffix.slice(1, suffix.length - 1),
			(prefix :++ suffix.slice(1, suffix.length - 1)) to C
		) lbl passedArrayLabel(suffix)
	}
	property("appendedAll(RelayArray[String].slice)") = forAll { (prefix :C[Int], suffix :RelayArray[String]) =>
		compare(
			Vector.from(prefix) ++ suffix.slice(1, suffix.length - 1),
			(prefix :++ suffix.slice(1, suffix.length - 1)) to C
		) lbl passedArrayLabel(suffix)
	}
	property(s"appendedAll(${C[Int].source.localClassName}[Int])") = forAll { (prefix :C[Int], suffix :C[Int]) =>
		test(Vector.from(prefix) :++ Vector.from(suffix), (prefix :++ suffix) to C)
	}
	property(s"appendedAll(${C[String].source.localClassName}[String])") = forAll {
		(prefix :C[Int], suffix :C[String]) =>
			compare(Vector.from(prefix) :++ Vector.from(suffix), (prefix :++ suffix) to C)
	}
	property("prependedAll(List[Int])") = forAll { (prefix :List[Int], suffix :C[Int]) =>
		test(Vector.from(prefix) ++ suffix, (prefix ++: suffix) to C)
	}
	property("prependedAll(List[String])") = forAll { (prefix :List[String], suffix :C[Int]) =>
		compare(Vector.from(prefix) ++ suffix, (prefix ++: suffix) to C)
	}
	property("prependedAll(ArraySeq[Int])") = forAll { (prefix :ArraySeq[Int], suffix :C[Int]) =>
		test(prefix ++ suffix, (prefix ++: suffix) to C)
	}
	property("prependedAll(ArraySeq[String])") = forAll { (prefix :ArraySeq[String], suffix :C[Int]) =>
		compare(prefix ++ suffix, (prefix ++: suffix) to C)
	}
	property("prependedAll(Vector[Int])") = forAll { (prefix :Vector[Int], suffix :C[Int]) =>
		test(prefix ++ suffix, (prefix ++: suffix) to C)
	}
	property("prependedAll(RelayArray[Int].slice)") = forAll { (prefix :RelayArray[Int], suffix :C[Int]) =>
		test(
			prefix.slice(1, prefix.length - 1) ++ suffix,
			(prefix.slice(1, prefix.length - 1) ++: suffix) to C
		) lbl passedArrayLabel(prefix)
	}
	property("prependedAll(RelayArray[String].slice)") = forAll { (prefix :RelayArray[String], suffix :C[Int]) =>
		compare(
			prefix.slice(1, prefix.length - 1) ++ suffix,
			(prefix.slice(1, prefix.length - 1) ++: suffix) to C
		) lbl passedArrayLabel(prefix)
	}
	property(s"prependedAll(${C[Int].source.localClassName}[Int])") = forAll { (prefix :C[Int], suffix :C[Int]) =>
		test(Vector.from(prefix) ++ Vector.from(suffix), (prefix ++: suffix) to C)
	}
	property(s"prependedAll(${C[String].source.localClassName}[String])") =
		forAll { (prefix :C[Int], suffix :C[String]) =>
			compare(Vector.from(prefix) ++ Vector.from(suffix) :Seq[Any], (prefix ++: suffix) to C)
		}



	private def passedArrayLabel[E](seq :RelayArray[E]) = seq match {
		case slice :ProperRelayArray[E] => slice.dumpString
		case _                           => seq.mkString(seq.localClassName + "(", ", ", ")")
	}

	import net.noresttherein.sugar.testing.scalacheck.noShrinking
	//todo: copy&paste to RankingSpec
	protected override def orderedProps[T, F, M, FM](expect :Seq[T], result :Seq[T])
	                                                (implicit tag :ClassTag[T], arbitrary :Arbitrary[T], ev :E[T],
	                                                 filt :Filter[T], fldA :FoldSide[F, T], evf :E[F], fld :Fold[T],
	                                                 mp :Map[T, M], evm :E[M],
	                                                 fmap :FlatMap[T, FM], evfm :E[FM]) :immutable.Seq[Prop] =
		super.orderedProps(expect, result) ++: List(
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
			                            else if (from <= until) {
				                            Prop(result.startsWith(expect.slice(from, until), from))
			                            } else forAll { subseq :Seq[T] =>
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
			                            val clipped = math.min(from, expect.length)
			                            expect.lastIndexWhere(filt.f, clipped) =? result.lastIndexWhere(filt.f, from)
			                        },
			"indexOfSlice"     lbl_: forAll { (x :Seq[T], from :Int) =>
			                            val len = expect.length
			                            if (from == len && x.isEmpty) result.indexOfSlice(x, from) ?= len
			                            else expect.indexOfSlice(x, from max 0) =? result.indexOfSlice(x, from)
			                         },
			"lastIndexOfSlice" lbl_: forAll { (x :Seq[T], from :Int) =>
			                            val len = expect.length
			                            if (from >= len && x.isEmpty) result.lastIndexOfSlice(x, from) ?= len
			                            else expect.lastIndexOfSlice(x, from) =? result.lastIndexOfSlice(x, from)
			                         },
		)

	override def hasOrder = true
}




abstract class UntaggedSeqProps[C[T] <: collection.Seq[T]](name :String, factory :IterableFactory[C])
	extends IterableProps[C, collection.Seq](name)(factory, Vector) with SeqProps[C, Dummy]
{
//	override val referenceFactory :IterableFactory[collection.Seq] = if (knowsSize) Vector else List
	protected override implicit def anyEvidence :Dummy[Any] = new Dummy
}






trait SugaredSeqProps[C[T] <: collection.Seq[T] with SugaredSeqOps[T, collection.Seq, collection.Seq[T]], E[_]]
	extends SeqProps[C, E] with SugaredIterableProps[C, collection.Seq, E]
{
	import net.noresttherein.sugar.testing.scalacheck.noShrinking
	def updateAllProperty[I[X] <: Iterable[X], A, B >: A](method :String)(prop :(C[A], Int, I[B]) => Prop)
	                    (implicit ev :E[A], aElem :Arbitrary[A], aElems :Arbitrary[I[B]],
	                     shrinkElem :Shrink[A], shrinkElems :Shrink[I[B]],
	                     prettyElem :A => Pretty, prettyElems :I[B] => Pretty) :Prop =
		forAllIndices[A] { (subject :C[A], index :Int) =>
			forAll { (elems :I[B]) => prop(subject, index, elems) lbl s"$method($index, $elems)" }
		}

	def explodedUpdateProperty[A, B >: A](method :String)(prop :(C[A], Int, B, B, Seq[B]) => Prop)
	                                     (implicit ev :E[A], aElem :Arbitrary[A], aElems :Arbitrary[B],
	                                      shrinkElem :Shrink[A], shrinkElems :Shrink[B],
	                                      prettyElem :A => Pretty, prettyElems :B => Pretty) :Prop =
		forAll { (subject :C[A], first :B, second :B,  rest :Seq[B]) =>
			forAll { (index :Short) =>
				(prop(subject, index, first, second, rest) lbl s"$method($index, $first, $second, $rest)") &&
				(prop(subject, Int.MaxValue, first, second, rest) lbl s"$method(Int.MaxValue, $first, $second, $rest)") &&
				(prop(subject, Int.MinValue, first, second, rest) lbl s"$method(Int.MinValue, $first, $second, $rest)")
			}
		}


	private def updatedAll[A, B](subject :C[A], index :Int, items :Iterable[A])
	                            (implicit ev :E[A], evB :E[B], tag :ClassTag[A], arbitrary :Arbitrary[A],
	                             filt :Filter[A], fldA :FoldSide[B, A], fld :Fold[A], mp :Map[A, B], fmap :FlatMap[A, B])
			:Prop =
		if (index < 0 || index > subject.size - items.size)
			subject.updatedAll(index, items).throws[IndexOutOfBoundsException]
		else {
			val vec    = Vector.from(subject)
			val expect = vec.take(index) :++ items :++ vec.drop(index + items.size)
			test(expect, subject.updatedAll(index, items) to C)
		}

	private def explodedUpdatedAll[A, B](subject :C[A], index :Int, first :A, second :A, rest :Seq[A])
	                                    (implicit ev :E[A], evB :E[B], tag :ClassTag[A],
	                                     arbitrary :Arbitrary[A], filt :Filter[A], fldA :FoldSide[B, A], fld :Fold[A],
	                                     mp :Map[A, B], fmap :FlatMap[A, B]) :Prop =
		if (index < 0 || index > subject.size - rest.size - 2)
			subject.updatedAll(index, first, second, rest :_*).throws[IndexOutOfBoundsException]
		else {
			val vec    = Vector.from(subject)
			val expect = vec.take(index) :+ first :+ second :++ rest :++ vec.drop(index + 2 + rest.size)
			test(expect, subject.updatedAll(index, first, second, rest :_*) to C)
		}

	private def overwritten[A, B](subject :C[A], index :Int, items :Iterable[A])
	                             (implicit ev :E[A], evB :E[B], tag :ClassTag[A], arbitrary :Arbitrary[A],
	                              filt :Filter[A], fldA :FoldSide[B, A], fld :Fold[A], mp :Map[A, B], fmap :FlatMap[A, B])
			:Prop =
	{
		val expect = expectOverwritten(subject, index, items)
		val result = subject.overwritten(index, items)
		test(expect, result to C)
	}

	private def explodedOverwritten[A, B](subject :C[A], index :Int, first :A, second :A, rest :Seq[A])
	                                     (implicit ev :E[A], evB :E[B], tag :ClassTag[A],
	                                      arbitrary :Arbitrary[A], filt :Filter[A], fldA :FoldSide[B, A], fld :Fold[A],
	                                      mp :Map[A, B], fmap :FlatMap[A, B]) :Prop =
	{
		val expect = expectOverwritten(subject, index, Prepended2Seq(first, second, rest))
		val result = subject.overwritten(index, first, second, rest :_*)
		test(expect, result)
	}

	private def expectOverwritten[A](subject :C[A], index :Int, items :Iterable[A]) :Seq[A] = {
		val drop = math.max(0, -math.max(index, -Int.MaxValue))
		val from = math.min(subject.length, math.max(index, 0))
		val max  = subject.length - from
		val end  = from + math.max(0, math.min(max, items.size - drop))
		val vec  = Vector.from(subject)
		vec.take(index) :++ items.toSeq.slice(drop, drop + max) :++ vec.drop(end)
	}


	private def insertedAll[A, B](subject :C[A], index :Int, items :Iterable[A])
	                             (implicit ev :E[A], evB :E[B], tag :ClassTag[A], arbitrary :Arbitrary[A],
	                              filt :Filter[A], fldA :FoldSide[B, A], fld :Fold[A], mp :Map[A, B], fmap :FlatMap[A, B])
			:Prop =
		if (index < 0 || index > subject.size)
			subject.updatedAll(index, items).throws[IndexOutOfBoundsException]
		else {
			val vec    = Vector.from(subject)
			val expect = vec.take(index) :++ items :++ vec.drop(index)
			test(expect, subject.insertedAll(index, items))
		}

	private def explodedInsertedAll[A, B](subject :C[A], index :Int, first :A, second :A, rest :Seq[A])
	                                     (implicit ev :E[A], evB :E[B], tag :ClassTag[A],
	                                      arbitrary :Arbitrary[A], filt :Filter[A], fldA :FoldSide[B, A], fld :Fold[A],
	                                      mp :Map[A, B], fmap :FlatMap[A, B]) :Prop =
		if (index < 0 || index > subject.size)
			subject.insertedAll(index, first, second, rest :_*).throws[IndexOutOfBoundsException]
		else {
			val vec    = Vector.from(subject)
			val expect = vec.take(index) :+ first :+ second :++ rest :++ vec.drop(index)
			test(expect, subject.insertedAll(index, first, second, rest :_*))
		}


	private def inserted[A, Z](subject :C[A], index :Int, elem :A)
	                          (implicit evA :E[A], evZ :E[Z], tagA :ClassTag[A], arbA :Arbitrary[A], arbZ :Arbitrary[Z],
	                           filt :Filter[A], fldA :FoldSide[Z, A], fld :Fold[A], mp :Map[A, Z], fmap :FlatMap[A, Z])
			:Prop =
		if (index < 0 || index > subject.size)
			subject.inserted(index, elem).throws[IndexOutOfBoundsException]
		else {
			val vec = Vector.from(subject)
			val expect = vec.take(index) :+ elem :++ vec.drop(index)
			val result = subject.inserted(index, elem)
			test(expect, result)
		}

	property("updatedAll(Int, Iterable[Int])") = updateAllProperty[Iterable, Int, Int]("updatedAll")(
		updatedAll[Int, Long](_, _, _)
	)
//	property("updatedAll(Int, Iterable[String])") = updatedAll[Iterable, String, Long]
	property(s"updatedAll(Int, ${C[Int].source.localClassName}[Int])") = updateAllProperty[C, Int, Int]("updatedAll")(
		updatedAll[Int, Long](_, _, _)
	)
	property("updatedAll(Int, Int, Int, Int*)") = explodedUpdateProperty[Int, Int]("updatedAll")(
		explodedUpdatedAll[Int, Long](_, _, _, _, _)
	)

	//todo: overwritten takes two orders of magnitude more time than updatedAll, why???
	property("overwritten(Int, Iterable[Int])") = updateAllProperty[Iterable, Int, Int]("overwritten")(
		overwritten[Int, Long](_, _, _)
	)
	property(s"overwritten(Int, ${C[Int].source.localClassName}[Int]") = updateAllProperty[C, Int, Int]("overwritten")(
		overwritten[Int, Long](_, _, _)
	)
	property("overwritten(Int, Int, Int, Int*)") = explodedUpdateProperty[Int, Int]("overwritten")(
		explodedOverwritten[Int, Long](_, _, _, _, _)
	)


	property("insertedAll(Int, Iterable[Int])") = updateAllProperty[Iterable, Int, Int]("insertedAll")(
		insertedAll[Int, Long](_, _, _)
	)
	property(s"insertedAll(Int, ${C[Int].source.localClassName}[Int]") = updateAllProperty[C, Int, Int]("insertedAll")(
		insertedAll[Int, Long](_, _, _)
	)
	property("insertedAll(Int, Int, Int, Int*)") = explodedUpdateProperty[Int, Int]("insertedAll")(
		explodedInsertedAll[Int, Long](_, _, _, _, _)
	)

	property("inserted(Int, Int)") = forAllIndices[Int] { (subject :C[Int], index :Int) =>
		forAll { elem :Int => inserted[Int, Long](subject, index, elem) lbl s"inserted($index, $elem)" }
	}

}
