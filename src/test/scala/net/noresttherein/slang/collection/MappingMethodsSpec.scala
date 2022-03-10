package net.noresttherein.slang.collection

import scala.collection.immutable.ListSet
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

import org.scalacheck.Prop._
import org.scalacheck.Properties





object MappingMethodsSpec extends Properties("M1appingMethods") {

	property("mapWithIndex") = forAll { list :List[String] =>
		list.mapWithIndex { (e, i) => (e, i) } ?= list.zipWithIndex
	}
	property("flatMapWithIndex") = forAll { list :List[List[String]] =>
		list.flatMapWithIndex { (e, i) => e.map { (_, i) } } ?=
			list.zipWithIndex.flatMap { case (e, i) => e.map((_, i)) }
	}
	property("mapWith") = forAll { list :List[Int] =>
		list.mapWith(0) { (e, sum) => (e + sum, e + sum) } ?= list.scanLeft(0)(_ + _).tail
	}
	property("flatMapWith") = forAll { list :List[List[Int]] =>
		val flatMapped = list.flatMapWith(0) { (es, sum) =>
			val res = es.scanLeft(sum)(_ + _)
			(res.tail, res.last)
		}
		flatMapped ?= list.flatten.scanLeft(0)(_ + _).tail
	}
	property("mapReverse") = forAll { seq :collection.Seq[String] =>
		val expect = seq.map(s => s + s).reverse
		(expect =? seq.to(List).mapReverse(s => s + s)) :| "List" &&
		(expect =? seq.to(ListBuffer).mapReverse(s => s + s)) :| "ListBuffer" &&
		(expect =? seq.to(Vector).mapReverse(s => s + s)) :| "Vector" &&
		(expect =? seq.to(ArrayBuffer).mapReverse(s => s + s)) :| "ArrayBuffer" &&
		(expect.to(ListSet) =? seq.to(ListSet).mapReverse(s => s + s)) :| "ListSet"
	}
}
