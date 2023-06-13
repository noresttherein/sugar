package net.noresttherein.sugar.collections

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

import net.noresttherein.sugar.extensions.IndexedSeqExtension
import net.noresttherein.sugar.testing.scalacheck.extensions.PropExtension
import net.noresttherein.sugar.typist.ConvertibleTo
import org.scalacheck.{Arbitrary, Prop, Properties, Shrink, Test}
import org.scalacheck.Prop._
import org.scalacheck.util.{ConsoleReporter, Pretty}




object IndexedSeqExtensionSpec extends Properties("IndexedSeqExtension") {
	import typeClasses._

	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140)).withMinSuccessfulTests(500)

	def seqProperty[X :ClassTag :Arbitrary :Shrink :ConvertibleTo[Pretty]#T]
		           (prop :collection.IndexedSeq[X] => Prop) :Prop =
		forAll { seq :ArraySeq[X] => prop(seq) :| "ArraySeq" } &&
			forAll { vec :Vector[X] => prop(vec) :| "Vector" } &&
			forAll { slice :IRefArraySlice[X] => prop(slice) :| "IArraySlice" } &&
			forAll { slice :RefArraySlice[X] => prop(slice) :| "ArraySlice" }


	property("takeRightWhile") = seqProperty { seq :collection.IndexedSeq[Int] =>
		var i = seq.length - 1
		var expect :List[Int] = Nil
		while (i >= 0 && seq(i) % 4 > 0) {
			expect = seq(i)::expect
			i -= 1
		}
		(seq.takeRightWhile(_ % 4 > 0) :collection.Seq[Int]) ?= expect
	}
	property("dropRightWhile") = seqProperty { seq :collection.IndexedSeq[Int] =>
		var i = seq.length
		while (i > 0 && seq(i - 1) % 4 > 0)
			i -= 1
		seq.dropRightWhile(_ % 4 > 0) ?= seq.slice(0, i)
	}

	property("binarySearch") = seqProperty { seq :collection.IndexedSeq[Int] =>
		val sorted = seq.sorted
		all(seq.indices.map {
			i => (sorted.binarySearch(seq(i)) ?= sorted.indexOf(seq(i))) :| "@" + i + "==" + seq(i)
		} :_*) && forAll { x :Int =>
			val i = sorted.indexOf(x)
			if (i >= 0)
				(sorted.binarySearch(x) ?= i) :| "binarySearch(" + x + ") ?= " + i
			else if (seq.length == 0)
				(sorted.binarySearch(x) ?= -1) :| "empty.binarySearch"
			else {
				val i = -sorted.binarySearch(x) - 1
				((i >= 0) :| "binarySearch <= -1" &&
					(i == 0 || sorted(i - 1) < x) &&
					(i == sorted.length || sorted(i) >= x)
				) :| "result=" + (-i - 1) :|
					"predecessor: " + (if (i > 0) sorted(i - 1) else "none") :|
					"successor: " + (if (i < sorted.length) sorted(i) else "none")
			}
		} lbl "sorted: " + sorted
	}
}
