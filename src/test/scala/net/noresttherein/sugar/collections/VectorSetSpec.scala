package net.noresttherein.sugar.collections

import net.noresttherein.sugar.collections.IterableProps.Dummy
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll



object VectorSetSpec
	extends IterableProps[VectorSet, Set](VectorSet, OrderedSet)
	   with SetProps[VectorSet, Set, Dummy]
	   with SugaredIterableProps[VectorSet, Set, Dummy]
{
	property("appended") = forAllChecked { (expect :Set[Int], subject :VectorSet[Int]) =>
		forAll { x :Int => test(expect - x + x, subject :+ x) }
	}
	property("appendedAll") = forAllChecked { (expect :Set[Int], subject :VectorSet[Int]) =>
		forAll { xs :Seq[Int] => test(expect -- xs ++ xs.reverse.distinct.reverse, subject :++ xs) }
	}
	property("appendingBuilder") = forAll { seq :Seq[Int] =>
		val expect = seq.reverse.distinct.reverse
		val result = (VectorSet.appendingBuilder[Int] ++= seq).result()
		test(OrderedSet from expect, result)
	}


	override def hasOrder = true
	override def knowsSize = true
}
