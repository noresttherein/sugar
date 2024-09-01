package net.noresttherein.sugar.collections

import scala.collection.{IterableFactory, IterableOps, immutable}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.Builder
import scala.reflect.ClassTag

import org.scalacheck.{Arbitrary, Gen, Prop, Shrink}
import org.scalacheck.Prop.{AnyOperators, all, forAll}
import org.scalacheck.util.{Buildable, Pretty}
import net.noresttherein.sugar.collections.IterableProps.{Dummy, Filter, FlatMap, Fold, FoldSide, Map}
import net.noresttherein.sugar.collections.extensions.{FactoryExtension, IterableExtension, IterableOnceExtension}
import net.noresttherein.sugar.funny.generic.Any1
import net.noresttherein.sugar.numeric
import net.noresttherein.sugar.numeric.extensions.IntCompanionExtension
import net.noresttherein.sugar.reflect.extensions.classNameMethods
import net.noresttherein.sugar.testing.scalacheck.extensions.{LazyExtension, PropExtension}




//trait SeqProps[C[T] <: collection.SeqOps[T, C, C[T]], E[_]]
//	extends SpecificIterableProps[C, ({ type S[X] = collection.SeqOps[X, collection.Seq, collection.Seq[X]] })#S, E]
trait SeqProps[C[T] <: collection.SeqOps[T, C, C[T]], S[T] >: C[T] <: collection.SeqOps[T, S, S[T]], E[_]]
	extends SpecificIterableProps[C, S, E]
{
	import net.noresttherein.sugar.testing.scalacheck.noShrinking

//	type S[X] = collection.SeqOps[X, collection.Seq, collection.Seq[X]]
	protected implicit def anyEvidence :E[Any]

	implicit def buildableRelayArray[T] :Buildable[T, RelayArray[T]] = new Buildable[T, RelayArray[T]] {
		override def builder :Builder[T, RelayArray[T]] = RelayArray.newBuilder[T]
	}

//	property("length") = forAllChecked { (expect :S[Int], subject :C[Int]) => expect.length =? subject.length }
	property("length") = shouldEqual((_:S[Int]).length)
	property("apply") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		all(expect.indices.map(i => expect(i) =? subject(i)) :_*)
	}
	property("distinct") = testSpecific((_ :S[Int]).distinct)
	property("reverse") = testSpecific((_ :S[Int]).reverse)
	property("reverseIterator") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		checkIterator(expect.reverse, subject.reverseIterator)
	}
	property("startsWith") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		all(
			(for {
				from <- 0 to expect.length
				until <- from to expect.length
			} yield Prop(subject.startsWith(expect.slice(from, until), from)) lbl s"startsWith(slice($from, $until), $from)")
		:_*) && forAll { (seq :S[Int], offset :Int) =>
			if (offset < 0 || offset > expect.length) Prop(!subject.startsWith(seq, offset))
			else expect.startsWith(seq, offset) =? subject.startsWith(seq, offset)
		}
	}
	property("endsWith") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		all((0 to expect.length).map(i => Prop(subject.endsWith(expect.drop(i))) lbl s"endsWith(${expect.drop(i)})") :_*)
	}
//	property("padTo") = forAll { (length :Short, elem :Int) => test((_ :Seq[Int]).padTo(length, elem)) }
	property("padTo") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		forAll { (length :Short, elem :Int) => expect.padTo(length, elem) =? subject.padTo(length, elem) }
	}
	property("segmentLength") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		all(expect.indices.map { i =>
			(expect.segmentLength(_ % 3 <= 1, i) =? subject.segmentLength(_ % 3 <= 1, i)) label "from " + i
		} :_*)
	}


	property("indexOf") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		forAll { (x :Int, i :Int) =>
			expect.indexOf(x, i) =? subject.indexOf(x, i)
		} && all(expect.view.mapWithIndex { (x, i) =>
			(expect.indexOf(x) =? subject.indexOf(x)) :| (x.toString + "@" + expect.indexOf(x)) &&
				(subject.indexOf(x, i) ?= i) :| s"#$i->$x"
		}.toSeq :_*)
	}
	property("lastIndexOf") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		forAll { (x :Int, i :Int) =>
			expect.lastIndexOf(x, i) =? subject.lastIndexOf(x, i)
		} && all(expect.view.mapWithIndex { (x, i) =>
			(expect.lastIndexOf(x) =? subject.lastIndexOf(x)) :| (x.toString + "@" + expect.lastIndexOf(x)) &&
				(subject.lastIndexOf(x, i) ?= i) :| s"#$i->$x"
		}.toSeq :_*)
	}
	property("indexWhere") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		forAll { (x :Int, i :Int) =>
			expect.indexOf(x, i) =? subject.indexWhere(_ == x, i)
		} && all(expect.view.mapWithIndex { (x, i) =>
			(expect.indexOf(x) =? subject.indexWhere(_ == x)) :| (x.toString + "@" + expect.indexOf(x)) &&
				(subject.indexWhere(_ == x, i) ?= i) :| s"#$i->$x"
		}.toSeq :_*)
	}
	property("lastIndexWhere") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		forAll { (x :Int, i :Int) =>
			expect.lastIndexOf(x, i) =? subject.lastIndexWhere(_ == x, i)
		} && all(expect.view.mapWithIndex { (x, i) =>
			(expect.lastIndexOf(x) =? subject.lastIndexWhere(_ == x)) :| (x.toString + "@" + expect.lastIndexOf(x)) &&
				(subject.lastIndexWhere(_ == x, i) ?= i) :| s"$x<-#$i"
		}.toSeq :_*)
	}
	property("indexOfSlice") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		val indexed = expect.toVector //List.indexOfSlice is buggy in 2.13.10
		forAll(Gen.choose(0, expect.length), Gen.choose(0, expect.length)) { (i :Int, j :Int) =>
			val from  = math.min(i, j)
			val until = math.max(i, j)
			val slice = indexed.slice(from, until)
			val idx = indexed.indexOfSlice(slice)
			((subject.indexOfSlice(slice) ?= idx) lbl s"$slice@$idx") &&
				((subject.indexOfSlice(slice, from) ?= from) lbl s"[$from, $until)@$from")
		} && forAll { (pattern :collection.Seq[Int], i :Short) =>
			subject.indexOfSlice(pattern, i & 0xfff) ?= indexed.indexOfSlice(pattern, i & 0xfff)
		}
	}
	property("lastIndexOfSlice") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		val indexed = expect.toVector
		forAll(Gen.choose(0, expect.length), Gen.choose(0, expect.length)) { (i :Int, j :Int) =>
			val from  = math.min(i, j)
			val until = math.max(i, j)
			val slice = indexed.slice(from, until)
			val idx   = indexed.lastIndexOfSlice(slice)
			((subject.lastIndexOfSlice(slice) ?= idx) lbl s"$slice@$idx") &&
				((subject.lastIndexOfSlice(slice, from) ?= from) lbl s"[$from, $until)@$from")
		} && forAll { (pattern :collection.Seq[Int], i :Short) =>
			subject.lastIndexOfSlice(pattern, i & 0xfff) ?= indexed.lastIndexOfSlice(pattern, i & 0xfff)
		}
	}

	property("updated") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		import numeric.globalRandom
		all(expect.indices.map { i =>
			val elem = Int.random
			val reference = expect.updated(i, elem)
			val result = subject.updated(i, elem)
//			test(reference, result)    //This takes ages!
			compare(reference, result)
		} :_*)
	}
	property("patch") = forAll { (from :Int, patch :collection.Seq[Int], replaced :Int) =>
		testSpecific((_:S[Int]).patch(from, patch, replaced))
	}

	property("appended(Int)") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		forAll { x :Int => test(expect :+ x, subject :+ x) }
	}
	property("appended(String)") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		forAll { x :String => compare(expect :+ x, subject :+ x) }
	}

	property("prepended(Int)") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		forAll { x :Int => test(x +: expect, x +: subject) }
	}
	property("prepended(String)") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		forAll { x :String => compare(x +: expect, x +: subject) }
	}

	property("appendedAll(List[Int])") = forAll { (prefix :C[Int], suffix :List[Int]) =>
		test(Vector.from(prefix) ++ suffix to S, prefix :++ suffix)
	}
	property("appendedAll(List[String])") = forAll { (prefix :C[Int], suffix :List[String]) =>
		compare(Vector.from(prefix) ++ suffix to S, prefix :++ suffix)
	}
	property("appendedAll(ArraySeq[Int])") = forAll { (prefix :C[Int], suffix :ArraySeq[Int]) =>
		test(Vector.from(prefix) ++ suffix to S, prefix :++ suffix)
	}
	property("appendedAll(ArraySeq[String])") = forAll { (prefix :C[Int], suffix :ArraySeq[String]) =>
		compare(Vector.from(prefix) ++ suffix to S, prefix :++ suffix)
	}
	property("appendedAll(Vector[Int]))") = forAll { (prefix :C[Int], suffix :Vector[Int]) =>
		test(Vector.from(prefix) ++ suffix to S, prefix :++ suffix)
	}
	property("appendedAll(RelayArray[Int].slice)") = forAll { (prefix :C[Int], suffix :RelayArray[Int]) =>
		test(
			Vector.from(prefix) ++ suffix.slice(1, suffix.length - 1) to S,
			prefix :++ suffix.slice(1, suffix.length - 1)
		) lbl passedArrayLabel(suffix)
	}
	property("appendedAll(RelayArray[String].slice)") = forAll { (prefix :C[Int], suffix :RelayArray[String]) =>
		compare(
			Vector.from(prefix) ++ suffix.slice(1, suffix.length - 1) to S,
			prefix :++ suffix.slice(1, suffix.length - 1)
		) lbl passedArrayLabel(suffix)
	}
	property(s"appendedAll(${C[Int].source.localClassName}[Int])") = forAll { (prefix :C[Int], suffix :C[Int]) =>
		test(Vector.from(prefix) :++ Vector.from(suffix) to S, prefix :++ suffix)
	}
	property(s"appendedAll(${C[String].source.localClassName}[String])") = forAll {
		(prefix :C[Int], suffix :C[String]) =>
			compare(Vector.from(prefix) :++ Vector.from(suffix) to S, prefix :++ suffix)
	}
	property("prependedAll(List[Int])") = forAll { (prefix :List[Int], suffix :C[Int]) =>
		test(Vector.from(prefix) ++ suffix to S, prefix ++: suffix)
	}
	property("prependedAll(List[String])") = forAll { (prefix :List[String], suffix :C[Int]) =>
		compare(Vector.from(prefix) ++ suffix to S, prefix ++: suffix)
	}
	property("prependedAll(ArraySeq[Int])") = forAll { (prefix :ArraySeq[Int], suffix :C[Int]) =>
		test(prefix ++ suffix to S, prefix ++: suffix)
	}
	property("prependedAll(ArraySeq[String])") = forAll { (prefix :ArraySeq[String], suffix :C[Int]) =>
		compare(prefix ++ suffix to S, prefix ++: suffix)
	}
	property("prependedAll(Vector[Int])") = forAll { (prefix :Vector[Int], suffix :C[Int]) =>
		test(prefix ++ suffix to S, prefix ++: suffix)
	}
	property("prependedAll(RelayArray[Int].slice)") = forAll { (prefix :RelayArray[Int], suffix :C[Int]) =>
		test(
			prefix.slice(1, prefix.length - 1) ++ suffix to S,
			prefix.slice(1, prefix.length - 1) ++: suffix
		) lbl passedArrayLabel(prefix)
	}
	property("prependedAll(RelayArray[String].slice)") = forAll { (prefix :RelayArray[String], suffix :C[Int]) =>
		compare(
			prefix.slice(1, prefix.length - 1) ++ suffix to S,
			prefix.slice(1, prefix.length - 1) ++: suffix
		) lbl passedArrayLabel(prefix)
	}
	property(s"prependedAll(${C[Int].source.localClassName}[Int])") = forAll { (prefix :C[Int], suffix :C[Int]) =>
		test(Vector.from(prefix) ++ Vector.from(suffix) to S, prefix ++: suffix)
	}
	property(s"prependedAll(${C[String].source.localClassName}[String])") =
		forAll { (prefix :C[Int], suffix :C[String]) =>
			compare(Vector.from(prefix) ++ Vector.from(suffix) to S, prefix ++: suffix)
		}



	private def passedArrayLabel[E](seq :RelayArray[E]) = seq match {
		case slice :ProperRelayArray[E] => slice.dumpString
		case _                           => seq.mkString(seq.localClassName + "(", ", ", ")")
	}

	//todo: copy&paste to RankingSpec
	protected override def props[T, F, M, FM](expect :S[T], result :C[T])
	                                         (implicit arbitrary :Arbitrary[T], ev :E[T], tag :ClassTag[T],
	                                                   filt :Filter[T], fldA :FoldSide[F, T], evf :E[F], fld :Fold[T],
	                                                   mp :Map[T, M], evm :E[M],
	                                                   fmap :FlatMap[T, FM], evfm :E[FM]) :immutable.Seq[Prop] =
		super.props(expect, result) ++ List(
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
				                            Prop(result.endsWith(expect.drop(from).asIterable))
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




abstract class UntaggedSeqProps[C[T] <: collection.Seq[T] with collection.SeqOps[T, C, C[T]]]
                               (name :String, factory :IterableFactory[C])
	extends IterableProps[C, collection.Seq](name)(factory, Vector) with SeqProps[C, collection.Seq, Dummy]
{
	def this(factory :IterableFactory[C]) = this(factory.toString, factory)
//	override val referenceFactory :IterableFactory[collection.Seq] = if (knowsSize) Vector else List
	protected override implicit def anyEvidence :Dummy[Any] = new Dummy
}




//trait PatchingProps[C[T] <: PatchingOps[T, C], C2[T] >: C[T] <: IterableOps[T, Any1, Any],
//	S[T] >: C2[T] <: IterableOps[T, Any1, Any], E[_]]
trait PatchingProps[C[T] <: PatchingOps[T, C], S[T] >: C[T] <: IterableOps[T, Any1, Any], E[_]]
	extends GenericIterableProps[C, S, E]
{
	import net.noresttherein.sugar.testing.scalacheck.noShrinking

	def updateAllProperty[I[X] <: IterableOps[X, Any1, Any], A, B >: A]
	                     (method :String)(prop :(C[A], Int, I[B]) => Prop)
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
			val idx    = math.min(vec.size, math.max(index, 0))
			val expect = vec.take(index) :++ items :++ vec.drop(idx + items.size) to S
			test(expect, subject.updatedAll(index, items))
		}

	private def explodedUpdatedAll[A, B](subject :C[A], index :Int, first :A, second :A, rest :Seq[A])
	                                    (implicit ev :E[A], evB :E[B], tag :ClassTag[A],
	                                     arbitrary :Arbitrary[A], filt :Filter[A], fldA :FoldSide[B, A], fld :Fold[A],
	                                     mp :Map[A, B], fmap :FlatMap[A, B]) :Prop =
		if (index < 0 || index > subject.size - rest.size - 2)
			subject.updatedAll(index, first, second, rest :_*).throws[IndexOutOfBoundsException]
		else {
			val vec    = Vector.from(subject)
			val idx    = math.min(vec.size, math.max(index, 0))
			val expect = vec.take(index) :+ first :+ second :++ rest :++ vec.drop(idx + 2 + rest.size) to S
			test(expect, subject.updatedAll(index, first, second, rest :_*))
		}

	private def overwritten[A, B](subject :C[A], index :Int, items :Iterable[A])
	                             (implicit ev :E[A], evB :E[B], tag :ClassTag[A], arbitrary :Arbitrary[A],
	                              filt :Filter[A], fldA :FoldSide[B, A], fld :Fold[A], mp :Map[A, B], fmap :FlatMap[A, B])
			:Prop =
	{
		val expect = expectOverwritten(subject, index, items)
		val result = subject.overwritten(index, items)
		test(expect, result)
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

	private def expectOverwritten[A :E](subject :C[A], index :Int, items :Iterable[A]) :S[A] = {
		val drop = math.max(0, -math.max(index, -Int.MaxValue))
		val from = math.min(subject.size, math.max(index, 0))
		val max  = subject.size - from
		val end  = from + math.max(0, math.min(max, items.size - drop))
		val vec  = Vector.from(subject)
		vec.take(index) :++ items.toSeq.slice(drop, drop + max) :++ vec.drop(end) to S[A]
	}


	private def insertedAll[A, B](subject :C[A], index :Int, items :Iterable[A])
	                             (implicit ev :E[A], evB :E[B], tag :ClassTag[A], arbitrary :Arbitrary[A],
	                              filt :Filter[A], fldA :FoldSide[B, A], fld :Fold[A], mp :Map[A, B], fmap :FlatMap[A, B])
			:Prop =
		if (index < 0 || index > subject.size)
			subject.insertedAll(index, items).throws[IndexOutOfBoundsException]
		else {
			val vec    = Vector.from(subject)
			val expect = vec.take(index) :++ items :++ vec.drop(index) to S
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
			val expect = vec.take(index) :+ first :+ second :++ rest :++ vec.drop(index) to S
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
			val expect = vec.take(index) :+ elem :++ vec.drop(index) to S
			val result = subject.inserted(index, elem)
			test(expect, result)
		}

	property("updatedAll(Int, Iterable[Int])") = updateAllProperty[Iterable, Int, Int]("updatedAll")(
		updatedAll[Int, Long](_, _, _)
	)
	property(s"updatedAll(Int, ${C[Int].source.localClassName}[Int])") = updateAllProperty[C, Int, Int]("updatedAll")(
		(subject, index, items) => updatedAll[Int, Long](subject, index, items.asIterable)
	)
	property("updatedAll(Int, Int, Int, Int*)") = explodedUpdateProperty[Int, Int]("updatedAll")(
		explodedUpdatedAll[Int, Long](_, _, _, _, _)
	)

	property("overwritten(Int, Iterable[Int])") = updateAllProperty[Iterable, Int, Int]("overwritten")(
		overwritten[Int, Long](_, _, _)
	)
	property(s"overwritten(Int, ${C[Int].source.localClassName}[Int]") = updateAllProperty[C, Int, Int]("overwritten")(
		(subject, index, items) => overwritten[Int, Long](subject, index, items.asIterable)
	)
	property("overwritten(Int, Int, Int, Int*)") = explodedUpdateProperty[Int, Int]("overwritten")(
		explodedOverwritten[Int, Long](_, _, _, _, _)
	)


	property("insertedAll(Int, Iterable[Int])") = updateAllProperty[Iterable, Int, Int]("insertedAll")(
		insertedAll[Int, Long](_, _, _)
	)
	property(s"insertedAll(Int, ${C[Int].source.localClassName}[Int]") = updateAllProperty[C, Int, Int]("insertedAll")(
		(subject, index, items) => insertedAll[Int, Long](subject, index, items.asIterable)
	)
	property("insertedAll(Int, Int, Int, Int*)") = explodedUpdateProperty[Int, Int]("insertedAll")(
		explodedInsertedAll[Int, Long](_, _, _, _, _)
	)

	property("inserted(Int, Int)") = forAllIndices[Int] { (subject :C[Int], index :Int) =>
		forAll { elem :Int => inserted[Int, Long](subject, index, elem) lbl s"inserted($index, $elem)" }
	}

}


//todo: SugaredSeqProps for reverse_++: