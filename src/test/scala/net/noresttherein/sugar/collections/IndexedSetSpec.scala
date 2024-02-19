package net.noresttherein.sugar.collections

import scala.Ordering.Implicits.sortedSetOrdering
import scala.collection.immutable.SortedSet
import scala.collection.{Factory, View}
import scala.reflect.ClassTag

import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Test}
import org.scalacheck.Prop.{AnyOperators, all, forAll, propBoolean}
import net.noresttherein.sugar.testing.scalacheck.extensions.{BooleanAsPropExtension, PropExtension}




trait SetProps[C[A] <: S[A], S[A] <: Set[A], E[_]] extends GenericIterableProps[C, S, E] {
	import net.noresttherein.sugar.testing.scalacheck.noShrinking

	property("contains") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		all(expect.toSeq.map { i => subject.contains(i) :| i.toString } :_*) &&
			forAll { i :Int =>
				expect.contains(i) =? subject.contains(i)
			}
	}

	property("subsetOf") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		forAll { xs :List[Int] =>
			val superset = expect ++ xs
			val same     = subject ++ xs
			(subject subsetOf superset lbl "superset: " + superset.toString) &&
				(subject subsetOf same lbl "superset: " + same.toString) &&
				(!expect.isEmpty ==> {
					val subset = expect - expect.head
					!(subject subsetOf subset) lbl "subset: " + subset
				})
		}
	}

	protected def testSmall[T :Arbitrary :E](check :(S[T], C[T]) => Prop) :Prop = {
		implicit val smallLists :Arbitrary[List[T]] =
			Arbitrary(Gen.choose(0, 10).flatMap { Gen.listOfN(_, Arbitrary.arbitrary[T]) })
		forAll { elems :List[T] => check(elems to S, elems to C) }
	}
	property("subsets") = testSmall { (expect :S[Int], subject :C[Int]) =>
		expect.subsets().toSeq =? subject.subsets().toSeq
	}
	property("subsets(size)") = testSmall { (expect :S[Int], subject :C[Int]) =>
		forAll { size :Int => expect.subsets(size).toSeq =? subject.subsets(size).toSeq }
	}

	property("incl") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		forAll { x :Int => test(expect + x to S, subject + x to C) }
	}
	property("excl") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		forAll { x :Int => test(expect - x to S, subject - x to C) }
	}
	property("union") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		forAll { xs :Set[Int] => test(expect | xs to S, subject | xs to C) } &&
			forAll { xs :S[Int] => test(expect | xs to S, subject | xs.to(C) to C) }
	}
	property("intersect") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		forAll { xs :Set[Int] => test(expect & xs to S, subject & xs to C) } &&
			forAll { xs :S[Int] => test(expect & xs to S, subject & xs.to(C) to C) }
	}
	property("diff") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		forAll { xs :Set[Int] => test(expect &~ xs to S, subject &~ xs to C) } &&
			forAll { xs :S[Int] => test(expect &~ xs to C, subject &~ xs.to(C) to C) }
	}
	property("removedAll") = forAllChecked { (expect :S[Int], subject :C[Int]) =>
		def forColl(xs :Iterable[Int]) = test(expect -- xs to S, subject -- xs to C)
		forAll { xs :List[Int] => forColl(xs) } &&
			forAll { xs :Vector[Int] => forColl(xs) } &&
			forAll { xs :Set[Int] => forColl(xs) } &&
			forAll { xs :S[Int] => test(expect -- xs to S, subject -- xs.to(C) to C) } &&
			forAll { xs :View[Int] => forColl(xs) }
	}

	override def hasOrder = false
}




object IndexedSetSpec
	extends EvidenceIterableProps[IndexedSet, SortedSet, Ordering]("IndexedSet")(IndexedSet, SortedSet)
	   with SetProps[IndexedSet, SortedSet, Ordering]
	   with OrderedProps[IndexedSet, SortedSet, Ordering]
{
	//todo: SetProps
	implicit override def pairEvidence[A :Ordering, B :Ordering] :Ordering[(A, B)] = Ordering.Tuple2

	override def hasOrder = true
	override def knowsSize = true

}